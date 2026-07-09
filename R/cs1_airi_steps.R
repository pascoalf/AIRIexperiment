# AIRI steps
source("R/prepare_session.R")
source("R/plot_helper.R")

if(!exists("full_rules") || !exists("full_rules_df")){
  source("R/cs1_arm.R")
}

## Step 1 - identify dependent rules
# Chi-square test
full_rules_df$chi_p_adj <- 
  full_rules %>% 
  interestMeasure(measure = "chiSquared", 
                  significance = TRUE) %>% 
  p.adjust(method = "bonferroni") ## adjust p-values

# Calculate conviction
full_rules_df$Conviction <- full_rules %>% 
  interestMeasure(measure = "Conviction")

# update dataframe
full_rules_df_dependent <- 
  full_rules_df %>% 
  mutate(Dependence = ifelse(chi_p_adj <0.05, "Dependent", "Independent")) %>% # add test result 
  filter(Dependence == "Dependent")# select dependent rules

# Add new metrics to rules objects
quality(full_rules)$conviction <- full_rules_df$Conviction
quality(full_rules)$chi_p_adj <- full_rules_df$chi_p_adj

# filter dependent rules
full_rules_dependent <- full_rules[quality(full_rules)$chi_p_adj<0.05,]

# statistics
full_rules_df %>% 
  mutate(Dependence = ifelse(chi_p_adj <0.05, "Dependent", "Independent")) %>%
  pull(Dependence) %>% table()

#
## Step 2 - remove redundant rules
# Calculate Mutual information and Improvement
quality(full_rules_dependent)$mutualInfo <- interestMeasure(full_rules_dependent,
                                                            measure = "mutualInformation")
quality(full_rules_dependent)$improvement <- interestMeasure(full_rules_dependent,
                                                             measure = "improvement")

plot_dependent_rule_scores(quality(full_rules_dependent))
#
plot_dependent_rule_metric_distribution(full_rules_dependent)


## dependent rules - nonredundant
# key items are in taxonomy var
extract_lhs_taxon <- function(rules){
  lhs_items <- LIST(lhs(rules), decode = TRUE)

  map_chr(lhs_items, function(items){
    taxon_items <- items[str_starts(items, "taxon=")]

    if(length(taxon_items) == 0){
      return(NA_character_)
    }

    str_remove(taxon_items[1], "^taxon=")
  })
}

full_rules_dependent_df <- full_rules_dependent %>%
  DATAFRAME() %>%
  mutate(taxon = factor(extract_lhs_taxon(full_rules_dependent)),
         LHSsize = size(lhs(full_rules_dependent)))

# Remove redundancy by Improvement
dependent_rules_non_redundant <- 
  full_rules_dependent_df %>% 
  filter(!is.na(taxon)) %>%  # remove rules without key items
  group_by(taxon) %>% 
  arrange(desc(improvement)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) 

if(interactive()){
  dependent_rules_non_redundant %>% View()
}

## remove redundancy by Complexity
non_redundant_by_complexity <- 
  full_rules_dependent_df %>% 
  filter(!is.na(taxon)) %>%  # remove rules without key items
  group_by(taxon) %>%
  arrange(desc(LHSsize)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) %>% 
  arrange(desc(conviction))

#
## remove redundancy by mutual information
non_redundant_by_mutualInfo <- 
  full_rules_dependent_df %>% 
  filter(!is.na(taxon)) %>%  # remove rules without key items
  group_by(taxon) %>%
  arrange(desc(mutualInfo)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) %>% 
  arrange(desc(conviction))


## idea: summarise metrics by method to solve non-redundant rules --> what method obtained highest conviction?
non_redundant_by_mutualInfo$Metric <- "Mutual\ninformation"
non_redundant_by_complexity$Metric <- "Complexity"
dependent_rules_non_redundant$Metric <-"Improvement"   

all_non_redundant <- 
  rbind(non_redundant_by_mutualInfo,
      non_redundant_by_complexity,
      dependent_rules_non_redundant) %>% 
  mutate(Classification = case_when(str_detect(RHS, "Abundant") ~ "Abundant",
                                    str_detect(RHS, "Undetermined") ~ "Undetermined",
                                    str_detect(RHS, "Rare") ~ "Rare")) 


plot_airi_selection_metric_distribution(all_non_redundant)


# percent shared rules
shared_rules <- all_non_redundant %>% 
  mutate(rule = paste(LHS, "->",RHS)) %>% 
  select(rule, Metric) %>% 
  distinct()

# venn diagram
ggVennDiagram(x = list(non_redundant_by_mutualInfo$LHS,
                       non_redundant_by_complexity$LHS,
                       dependent_rules_non_redundant$LHS),
              category.names = c("Mutual\nInformation",
                                 "Complexity",
                                 "Improvement"),
              label_size = 4) + 
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
  guides(fill = "none") +
  xlim(-6,9)+
  ylim(-9,6)



## save non redundant rules for inspection
#non_redundant_by_mutualInfo %>% write.csv("rule-sets/mosj_airi_by_mutual_information.csv")
#non_redundant_by_complexity %>% write.csv("rule-sets/mosj_airi_by_complexity.csv")
#dependent_rules_non_redundant %>% write.csv("rule-sets/mosj_airi_by_improvement.csv")
