# example code preparation
library(dplyr)
library(tidyr)
library(purrr) 
# Load packages for association rule mining
library(arules)
# Load packages for abundance classification with unsupervised learning
# also includes nice data
library(ulrb)
# Load packages for regular expressions and string wrangling
library(stringr)

# load example data
data("nice")
data("nice_env")

## Load and prepare data

# Transform to long format (samples are in columns, taxa in rows)
nice_long <- prepare_tidy_data(nice, sample_names = colnames(nice)[1:9])

# classify taxa by abundance
nice_long <- define_rb(nice_long)

# Make column for full taxonomy and select relevant columns
nice_long <- nice_long %>% 
  mutate(taxa = paste(Phylum, Class, Order, Family, Genus)) %>% 
  select(Sample, Classification, taxa)

# select metadata to use
nice_cat <- nice_env %>% 
  select(Sample = ENA_ID, Month, Depth, Region, Water.mass)

# Transform depth from numeric to factor variable
nice_cat <- nice_cat %>% mutate(Depth = as.factor(Depth))

# check if all features are factor (or coercible to factor)
str(nice_cat)

# Combine community data with metadata
nice_df <- nice_long %>% left_join(nice_cat, by = "Sample")

# sample ID will not be necessary for rule mining
nice_df$Sample <- NULL

## Apply association rule mining

# Create transactions object
# (it is safe to ignore warning message)
nice_transactions <- transactions(nice_df)

# check transactions
summary(nice_transactions)

# get rhs items (set the consequent)
classifications_rhs <- grep("Classification=", 
                            itemLabels(transactions_full), 
                            value = TRUE)

# mine rules
nice_rules <- apriori(nice_transactions,
                      parameter = list(support = 0.001,
                                       minlen = 2, 
                                       maxlen = 7),
                      appearance = list(rhs = classifications_rhs))

# check rule set
summary(nice_rules)

# transform transactions object in data frame
nice_rules_df <- DATAFRAME(nice_rules)

## AIRItaxa algorithm in R

# Identify dependent rules
# Chi-square test
nice_rules_df$chi_p_adj <- 
  nice_rules %>% 
  interestMeasure(measure = "chiSquared", 
                  significance = TRUE) %>% 
  p.adjust(method = "bonferroni") ## adjust p-values

# Calculate conviction
nice_rules_df$Conviction <- nice_rules %>% 
  interestMeasure(measure = "Conviction")

# update dataframe
nice_rules_df_dependent <- 
  nice_rules_df %>% 
  mutate(Dependence = ifelse(chi_p_adj <0.05, "Dependent", "Independent")) %>% # add test result 
  filter(Dependence == "Dependent")# select dependent rules

# Add new metrics to rules objects
quality(nice_rules)$conviction <- nice_rules_df$Conviction
quality(nice_rules)$chi_p_adj <- nice_rules_df$chi_p_adj

# filter dependent rules
nice_rules_dependent <- nice_rules[quality(nice_rules)$chi_p_adj<0.05,]

# Calculate Mutual information and Improvement
quality(nice_rules_dependent)$mutualInfo <- interestMeasure(nice_rules_dependent,
                                                            measure = "mutualInformation")
quality(nice_rules_dependent)$improvement <- interestMeasure(nice_rules_dependent,
                                                             measure = "improvement")
# Dependent rules - nonredundant
# key items are in taxonomy var
# Remove redundancy by Improvement
nice_dependent_rules_non_redundant <- 
  nice_rules_dependent %>% 
  DATAFRAME() %>% 
  filter(str_detect(LHS, "taxa")) %>%  # remove rules without key items
  separate_wider_delim(LHS, ",", names = "taxon", too_many = "debug") %>% 
  select(-LHS_ok, -LHS_pieces) %>% 
  mutate(taxon = factor(str_remove(taxon, "\\{taxon=")),
         taxon = str_remove(taxon, "\\}"),
         taxon = str_remove(taxon, ","),
         taxon = factor(taxon)) %>%  
  group_by(taxon) %>% 
  arrange(desc(improvement)) %>% ## change redundancy removal metric here
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) 

# View AIRItaxa result, using Improvement for redundancy removal
nice_dependent_rules_non_redundant %>% View()
