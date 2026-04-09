## Breast cancer - new case study

breast_cancer_df <- read.csv("data/breast_cancer.csv")

# ID not necessary for transactions database

# discretize based on quartiles
# use mean values

quantile(breast_cancer_df$radius_sd)

breast_cancer_transactions <- breast_cancer_df %>% select(-ID) %>% transactions()
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
                      parameter = list(support = 0.1,minlen = 2, maxlen = 30),
                      appearance = list(rhs = diagnosis_rhs))

diagnosis_rules %>% DATAFRAME() %>% View()
diagnosis_rules_df <- diagnosis_rules %>% DATAFRAME()

#
diagnosis_rules_df %>% 
  ggplot(aes(support, confidence, fill = lift)) + 
  geom_jitter(shape = 21, col = "grey", size = 2) + 
  scale_fill_gradient(low = reds[1], high = reds[9]) + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) + 
  labs(x = "Conviction",
       y = "Confidence",
       fill = "Lift: ")

# alternative
diagnosis_rules_df %>% 
  ggplot(aes(support, confidence, col = RHS)) + 
  geom_jitter(size = 2) + 
  #scale_color_gradient(low = reds[1], high = reds[9]) + 
  # labs(title = paste(length(full_rules_df[,1]), "rules")) + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.background = element_rect(fill = "grey80")) + 
  labs(x = "Conviction",
       y = "Confidence",
       col = "Diagnosis: ")

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
  group_by(LHS) %>% 
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





