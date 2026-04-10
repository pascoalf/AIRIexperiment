## Breast cancer - new case study

breast_cancer_df <- read.csv("data/breast_cancer.csv")

# ID not necessary for transactions database

# discretize based on quartiles
# use mean values
test <- breast_cancer_df
quantile(breast_cancer_df$radius_sd)

test$radius_sd

into_quartile <- function(x){
  case_when(is.na(x) ~ "Unknown",
            x == 0 ~ "Undetected",
            x <= quantile(x, na.rm = TRUE)[2] ~ "very low",
            x <= median(x, na.rm = TRUE) ~ "low",
            x <= quantile(x, na.rm = TRUE)[4] ~ "high",
            x > quantile(x, na.rm = TRUE)[4] ~ "very high")  
}

# all numerical to categorical
breast_cancer_cat <- sapply(breast_cancer_df[,3:32], into_quartile)
breast_cancer_cat <- breast_cancer_cat %>% 
  cbind(Diagnosis = breast_cancer_df$Diagnosis) %>% 
  as.data.frame() %>% 
  mutate(Diagnosis = ifelse(Diagnosis == "M", "Malginant", "Benign")) %>% 
  mutate_all(as.factor)


# view distribution of vars -- sup material
breast_cancer_transactions <- breast_cancer_cat %>% transactions()

#
View(DATAFRAME(breast_cancer_transactions))
#
summary(breast_cancer_transactions)

# get rhs items (set the consequent)
diagnosis_rhs <- grep("Diagnosis=", 
                            itemLabels(breast_cancer_transactions), 
                            value = TRUE)

#
diagnosis_rules <- apriori(breast_cancer_transactions,
                      parameter = list(support = 0.05,minlen = 2, maxlen = 30),
                      appearance = list(rhs = diagnosis_rhs))
#
diagnosis_rules %>% DATAFRAME() %>% View()
diagnosis_rules_df <- diagnosis_rules %>% DATAFRAME()

# AIRI steps

## Step 1 - identify dependent rules
# Chi-square test
diagnosis_rules_df$chi_p_adj <- 
  diagnosis_rules %>% 
  interestMeasure(measure = "chiSquared", 
                  significance = TRUE) %>% 
  p.adjust(method = "bonferroni") ## adjust p-values

# Calculate conviction
diagnosis_rules_df$Conviction <- diagnosis_rules %>% 
  interestMeasure(measure = "Conviction")

# update dataframe
diagnosis_rules_df_dependent <- 
  diagnosis_rules_df %>% 
  mutate(Dependence = ifelse(chi_p_adj <0.05, "Dependent", "Independent")) %>% # add test result 
  filter(Dependence == "Dependent")# select dependent rules

# Add new metrics to rules objects
quality(diagnosis_rules)$conviction <- diagnosis_rules_df$Conviction
quality(diagnosis_rules)$chi_p_adj <- diagnosis_rules_df$chi_p_adj

# filter dependent rules
diagnosis_rules_dependent <- diagnosis_rules[quality(diagnosis_rules)$chi_p_adj<0.05,]

# statistics
diagnosis_rules_df %>% 
  mutate(Dependence = ifelse(chi_p_adj <0.05, "Dependent", "Independent")) %>%
  pull(Dependence) %>% table()

#
## Step 2 - remove redundant rules
# Calculate Mutual information and Improvement
quality(diagnosis_rules_dependent)$mutualInfo <- interestMeasure(diagnosis_rules_dependent,
                                                            measure = "mutualInformation")
quality(diagnosis_rules_dependent)$improvement <- interestMeasure(diagnosis_rules_dependent,
                                                             measure = "improvement")
#
diagnosis_rules_dependent %>% DATAFRAME() %>% View()

## dependent rules - nonredundant
# key item is worst measurement of any var
# Remove redundancy by Improvement
#
#diagnosis_rules_non_redundant_improv <- 
diagnosis_rules_dependent %>% 
  DATAFRAME() %>%
  mutate(Key = case_when(str_detect(LHS, "fractal_dimension_worst") ~ "fractal_dimension_worst",
                         str_detect(LHS, "area_worst") ~ "area_worst",
                         str_detect(LHS, "smoothness_worst") ~ "smoothness_worst",
                         str_detect(LHS, "compactness_worst") ~ "compactness_worst",
                         str_detect(LHS, "concavity_worst") ~ "concavity_worst",
                         str_detect(LHS, "concave_points_worst") ~ "concave_points_worst",
                         str_detect(LHS, "symmetry_worst") ~ "symmetry_worst",
                         str_detect(LHS, "perimeter_worst") ~ "perimeter_worst",
                         str_detect(LHS, "radius_worst") ~ "radius_worst")) %>% 
  filter(!is.na(Key)) %>% 
  group_by(Key) %>% 
  arrange(desc(improvement)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(Key, LHS, RHS, support,
         confidence, coverage,	
         lift,	count,	conviction,	
         mutualInfo,	improvement) %>% View()

diagnosis_rules_non_redundant_improv %>% View()

##
diagnosis_rules_non_redundant_mutualInfo <- diagnosis_rules_dependent %>% 
  DATAFRAME() %>%
  mutate(Key = case_when(str_detect(LHS, "fractal_dimension_worst") ~ "fractal_dimension_worst",
                         str_detect(LHS, "area_worst") ~ "area_worst",
                         str_detect(LHS, "smoothness_worst") ~ "smoothness_worst",
                         str_detect(LHS, "compactness_worst") ~ "compactness_worst",
                         str_detect(LHS, "concavity_worst") ~ "concavity_worst",
                         str_detect(LHS, "concave_points_worst") ~ "concave_points_worst",
                         str_detect(LHS, "symmetry_worst") ~ "symmetry_worst",
                         str_detect(LHS, "perimeter_worst") ~ "perimeter_worst",
                         str_detect(LHS, "radius_worst") ~ "radius_worst")) %>% 
  distinct() %>% 
  #filter(!is.na(Key)) %>% 
  group_by(Key) %>% 
  arrange(desc(mutualInfo)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(Key, LHS, RHS, support,
         confidence, coverage,	
         lift,	count,	conviction,	
         mutualInfo,	improvement) 

diagnosis_rules_non_redundant_mutualInfo %>% View()


#
diagnosis_rules_non_redundant_complex <- diagnosis_rules_dependent %>% 
  DATAFRAME() %>%
  mutate(LHSsize = str_count(LHS, ",")) %>% 
  mutate(Key = case_when(str_detect(LHS, "fractal_dimension_worst") ~ "fractal_dimension_worst",
                         str_detect(LHS, "area_worst") ~ "area_worst",
                         str_detect(LHS, "smoothness_worst") ~ "smoothness_worst",
                         str_detect(LHS, "compactness_worst") ~ "compactness_worst",
                         str_detect(LHS, "concavity_worst") ~ "concavity_worst",
                         str_detect(LHS, "concave_points_worst") ~ "concave_points_worst",
                         str_detect(LHS, "symmetry_worst") ~ "symmetry_worst",
                         str_detect(LHS, "perimeter_worst") ~ "perimeter_worst",
                         str_detect(LHS, "radius_worst") ~ "radius_worst")) %>% 
  distinct() %>% 
  #filter(!is.na(Key)) %>% 
  group_by(Key) %>% 
  arrange(desc(LHSsize)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(Key, LHS, RHS, support,
         confidence, coverage,	
         lift,	count,	conviction,	
         mutualInfo,	improvement) 

diagnosis_rules_non_redundant_complex %>% View()

##==> not working as expected, because the key is just equal to each feature, not each feature value
## test dynamic key
diagnosis_rules_dependent_df <- diagnosis_rules_dependent %>% DATAFRAME()
#
names(breast_cancer_cat)
#
diagnosis_rules_dependent_df %>% 
  filter(str_detect(RHS, "Benign")) %>% 
  summarise(area = sum(str_count(LHS, "area")))

count_items <- function(LHS, x){
  sum(str_count(LHS, x))
}

diagnosis_rules_dependent_df$LHS %>% count_items(x = "area_mean")

# nest
diagnosis_rules_dependent_df %>% 
  group_by(RHS) %>% 
  nest() %>% 
  summarise() 
  

