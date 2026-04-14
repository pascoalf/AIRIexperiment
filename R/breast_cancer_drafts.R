##DRAFTS


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
# feature names
featues_bc <- names(breast_cancer_df)[3:32]

#
all_sums_test <- map(featues_bc, 
                     .f = ~count_items(diagnosis_rules_dependent_df$LHS, x = .x)) 
#
dk_features <- data.frame(total = unlist(all_sums_test)) %>% 
  cbind(featues_bc) %>% 
  arrange(desc(total)) %>% 
  mutate(Sample = "single") %>% 
  define_rb(abundance_col = "total") %>% ## ulrb step
  filter(Classification == "Abundant") %>% 
  pull(featues_bc)

# expand features
breast_cancer_cat %>% 
  select(all_of(dk_features)) %>% 
  distinct()

#
diagnosis_rules_dependent_df %>% head()







