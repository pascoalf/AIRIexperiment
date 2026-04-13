# breast cancer 2 -- as multi key - testing

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
            x <= quantile(x, na.rm = TRUE)[2] ~ "vlow",
            x <= median(x, na.rm = TRUE) ~ "low",
            x <= quantile(x, na.rm = TRUE)[4] ~ "high",
            x > quantile(x, na.rm = TRUE)[4] ~ "vhigh")  
}

# all numerical to categorical
breast_cancer_cat <- sapply(breast_cancer_df[,3:32], into_quartile)
breast_cancer_cat <- breast_cancer_cat %>% 
  cbind(Diagnosis = breast_cancer_df$Diagnosis) %>% 
  as.data.frame() %>% 
  mutate(Diagnosis = ifelse(Diagnosis == "M", "Malginant", "Benign")) %>% 
  mutate_all(as.factor)

#
breast_cancer_cat %>% View()

# Key by area
key_area <- breast_cancer_cat %>% 
  select(area_mean, area_sd, area_worst) %>% 
  distinct() %>% 
  mutate(Key = paste0("Area_mean: ", area_mean, 
                      " (sd: ", area_sd, "), ",
                      "worst: ", area_worst)) 
# Apped key_area and remove redundant features
breast_cancer_cat2 <- breast_cancer_cat %>% 
  left_join(key_area) %>% 
  select(-area_mean, -area_sd, -area_worst)

# view distribution of vars -- sup material
breast_cancer_transactions2 <- breast_cancer_cat2 %>% transactions()

View(DATAFRAME(breast_cancer_transactions2))

#
# get rhs items (set the consequent)
diagnosis_rhs <- grep("Diagnosis=", 
                      itemLabels(breast_cancer_transactions2), 
                      value = TRUE)

#
diagnosis_rules2 <- apriori(breast_cancer_transactions2,
                           parameter = list(support = 0.05,minlen = 2, maxlen = 30),
                           appearance = list(rhs = diagnosis_rhs))
#
diagnosis_rules2 %>% DATAFRAME %>% View()
diagnosis_rules_df2 <- diagnosis_rules2 %>% DATAFRAME()

#
## Step 1 - identify dependent rules
# Chi-square test
diagnosis_rules_df2$chi_p_adj <- 
  diagnosis_rules2 %>% 
  interestMeasure(measure = "chiSquared", 
                  significance = TRUE) %>% 
  p.adjust(method = "bonferroni") ## adjust p-values

# Calculate conviction
diagnosis_rules_df2$Conviction <- diagnosis_rules2 %>% 
  interestMeasure(measure = "Conviction")

# update dataframe
diagnosis_rules_df_dependent2 <- 
  diagnosis_rules_df2 %>% 
  mutate(Dependence = ifelse(chi_p_adj <0.05, "Dependent", "Independent")) %>% # add test result 
  filter(Dependence == "Dependent")# select dependent rules

# Add new metrics to rules objects
quality(diagnosis_rules2)$conviction <- diagnosis_rules_df2$Conviction
quality(diagnosis_rules2)$chi_p_adj <- diagnosis_rules_df2$chi_p_adj

# filter dependent rules
diagnosis_rules_dependent2 <- diagnosis_rules2[quality(diagnosis_rules2)$chi_p_adj<0.05,]

# statistics
diagnosis_rules_df2 %>% 
  mutate(Dependence = ifelse(chi_p_adj <0.05, "Dependent", "Independent")) %>%
  pull(Dependence) %>% table()

#
## Step 2 - remove redundant rules
# Calculate Mutual information and Improvement
quality(diagnosis_rules_dependent2)$mutualInfo <- interestMeasure(diagnosis_rules_dependent2,
                                                                 measure = "mutualInformation")
quality(diagnosis_rules_dependent2)$improvement <- interestMeasure(diagnosis_rules_dependent2,
                                                                  measure = "improvement")
#
diagnosis_rules_dependent2 %>% DATAFRAME() %>% View()

### dependent rules - nonredundant
# Remove redundancy by Improvement
#
#diagnosis_rules_non_redundant_improv <- 
diagnosis_rules_dependent2 %>% 
  DATAFRAME() %>%
  separate_wider_delim(LHS, "Key", names = "LHS", too_many = "debug") %>% 
  select(-LHS_pieces, -LHS_ok) %>% 
  mutate(LHS_remainder = ifelse(!str_detect(LHS_remainder, "Key"), 
                                "Out of key", LHS_remainder)) %>% 
  pull(LHS_remainder) %>% 
  unique()
  group_by(LHS_remainder) %>% # works as key 
  arrange(desc(improvement)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(Key = LHS_remainder, LHS, RHS, support,
         confidence, coverage,	
         lift,	count,	conviction,	
         mutualInfo,	improvement) %>% View()

