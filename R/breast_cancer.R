## Breast cancer - new case study
breast_cancer_df <- read.csv("data/breast_cancer.csv")

# ID not necessary for transactions database

# discretize based on quartiles
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
diagnosis_rules_non_redundant_improv <- diagnosis_rules_dependent %>% 
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
         mutualInfo,	improvement)

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

## plots
ggVennDiagram(x = list(diagnosis_rules_non_redundant_mutualInfo$LHS,
                       diagnosis_rules_non_redundant_complex$LHS,
                       diagnosis_rules_non_redundant_improv$LHS),
              category.names = c("Mutual\nInformation",
                                 "Complexity",
                                 "Improvement"),
              label_size = 4) + 
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
  guides(fill = "none") +
  labs(title = "Dataset: Breast Cancer Diagnosis",
       subtitle = "Key: worst values") + 
  xlim(-6,9)+
  ylim(-9,6)

# summary
diagnosis_rules_non_redundant_mutualInfo$Metric <- "Mutual\ninformation"
diagnosis_rules_non_redundant_complex$Metric <- "Complexity"
diagnosis_rules_non_redundant_improv$Metric <-"Improvement"   

diagnosis_all_non_redundant <- 
  rbind(diagnosis_rules_non_redundant_mutualInfo,
        diagnosis_rules_non_redundant_complex,
        diagnosis_rules_non_redundant_improv) 

#
gridExtra::grid.arrange(
  diagnosis_all_non_redundant %>% 
    ggplot(aes(Metric, confidence)) + 
    geom_boxplot(outliers = FALSE) + 
    geom_jitter(height = 0, width = 0.1, col = "grey7") +
    theme_classic() + 
    theme(axis.title.x = element_blank())+
    labs(y = "Confidence"),
  diagnosis_all_non_redundant %>% 
    ggplot(aes(Metric, lift)) + 
    geom_boxplot(outliers = FALSE) +
    geom_jitter(height = 0, width = 0.1, col = "grey7") +
    theme_classic() + 
    theme(axis.title.x = element_blank())+
    labs(y = "Lift"),
  diagnosis_all_non_redundant %>% 
    ggplot(aes(Metric, conviction)) + 
    geom_boxplot(outliers = FALSE) + 
    geom_jitter(height = 0, width = 0.1, col = "grey7") +
    theme_classic() +
    theme(axis.title.x = element_blank())+
    labs(y = "Conviction"), ncol = 3)


##
gridExtra::grid.arrange(
  diagnosis_rules_df %>% 
    ggplot(aes(support, confidence, col = lift)) + 
    geom_jitter(size = 2) + 
    scale_color_gradient(low = reds[1], high = reds[9]) + 
    theme_classic() + 
    theme(legend.position = "top",
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          panel.background = element_rect(fill = "grey80")) + 
    labs(x = "Conviction",
         y = "Confidence"),
  diagnosis_rules_dependent_df %>% 
    ggplot(aes(support, confidence, col = lift)) + 
    geom_jitter(size = 2) + 
    scale_color_gradient(low = reds[1], high = reds[9]) + 
    theme_classic() + 
    theme(legend.position = "top",
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          panel.background = element_rect(fill = "grey80")) + 
    labs(x = "Conviction",
         y = "Confidence")
)


all_items <- names(breast_cancer_cat)[-31]

## jaccard index --> proxy to redundancy between rule sets
#
jaccard_all <- function(ruleset){
  
  # to get possible comparisons between rules
  set_comparisons <- function(ruleset = ruleset){
    #  
    comparisons <- expand.grid(seq_along(ruleset$LHS), seq_along(ruleset$LHS))
    #
    comparisons <- data.frame(t(apply(comparisons, 1, sort))) %>% 
      distinct()
    #
    comparisons <- comparisons %>% 
      mutate(check = X1 - X2) %>% 
      filter(check != 0) %>% 
      select(-check)
    #
    return(comparisons)
  }
  #
  comparisons <- set_comparisons(ruleset = ruleset)
  
  # to calculate jaccard between two rules
  ruleJaccard <- function(ruleset, p1, p2){
    #
    rule_items <- ruleset %>% 
      mutate(item_sets = str_extract_all(LHS, paste(all_items, collapse='|'))) %>% 
      pull(item_sets)
    #
    a <- rule_items[[p1]]
    b <- rule_items[[p2]]
    #
    jaccard_ab <- jaccard(a,b)
    #
    return(jaccard_ab)
  }
  
  # obtain ruleJaccard for all possible two rules
  jaccard_for_all <- map2(.x = comparisons$X1,
                          .y = comparisons$X2,
                          .f = ~ruleJaccard(ruleset, 
                                            p1 = .x, 
                                            p2 = .y)) %>% 
    unlist()
  
  #
  return(jaccard_for_all)
}

# apply to all
jaccard_scores_df <- diagnosis_all_non_redundant %>% 
  group_by(RHS, Metric) %>% 
  nest() %>% 
  mutate(jaccard = map(.x = data, .f = ~jaccard_all(.x)))
#
jaccard_scores_df %>%
  unnest(jaccard) %>% 
  ggplot(aes(Metric, jaccard, col = RHS)) + 
  geom_jitter(height = 0, width = 0.1, alpha = 0.5) + 
  stat_summary() + 
  labs(title = "Jaccard score for pairwise rules",
       subtitle = "Breast Cancer Diagnostic dataset") + 
  theme_classic()

# summary
jaccard_scores_df %>%
  mutate(mean_jaccard = map(.x = jaccard, ~mean(.x)),
         sd_jaccard = map(.x = jaccard, ~sd(.x)),
         min_jaccard = map(.x = jaccard, ~min(.x)),
         max_jaccard = map(.x = jaccard, ~max(.x))) %>% 
  unnest(c(mean_jaccard, sd_jaccard, min_jaccard, max_jaccard)) 


## overall (for general compar)
diagnosis_all_non_redundant %>% 
  group_by(Metric) %>% 
  nest() %>% 
  mutate(jaccard = map(.x = data, .f = ~jaccard_all(.x))) %>% 
  mutate(mean_jaccard = map(.x = jaccard, ~mean(.x)),
         sd_jaccard = map(.x = jaccard, ~sd(.x))) %>% 
  unnest(c(mean_jaccard, sd_jaccard))




