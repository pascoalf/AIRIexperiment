## AIRI - emose 

## Step 1 - identify dependent rules
# Chi-square test
emose_rules_df$chi_p_adj <- 
  emose_rules %>% 
  interestMeasure(measure = "chiSquared", 
                  significance = TRUE) %>% 
  p.adjust(method = "bonferroni") ## adjust p-values

# Calculate conviction
emose_rules_df$Conviction <- emose_rules %>% 
  interestMeasure(measure = "Conviction")

# update dataframe
emose_rules_df_dependent <- 
  emose_rules_df %>% 
  mutate(Dependence = ifelse(chi_p_adj <0.05, "Dependent", "Independent")) %>% # add test result 
  filter(Dependence == "Dependent")# select dependent rules

# Add new metrics to rules objects
quality(emose_rules)$conviction <- emose_rules_df$Conviction
quality(emose_rules)$chi_p_adj <- emose_rules_df$chi_p_adj

# filter dependent rules
emose_rules_dependent <- emose_rules[quality(emose_rules)$chi_p_adj<0.05,]

# statistics
emose_rules_df %>% 
  mutate(Dependence = ifelse(chi_p_adj <0.05, "Dependent", "Independent")) %>%
  pull(Dependence) %>% table()

#
## Step 2 - remove redundant rules
# Calculate Mutual information and Improvement
quality(emose_rules_dependent)$mutualInfo <- interestMeasure(emose_rules_dependent,
                                                            measure = "mutualInformation")
quality(emose_rules_dependent)$improvement <- interestMeasure(emose_rules_dependent,
                                                             measure = "improvement")

quality(emose_rules_dependent) %>% 
  filter(!is.infinite(conviction)) %>% 
  pivot_longer(cols = c("mutualInfo", "improvement"),
               values_to = "Score",
               names_to = "Metric") %>% 
  mutate(Metric = ifelse(Metric == "mutualInfo", "Mutual Information", "Improvement")) %>% 
  ggplot(aes(conviction, 
             confidence, 
             col = lift,
             size = Score)) + 
  geom_point() + 
  scale_color_gradient(low = reds[1], high = reds[9]) + 
  theme_classic() + 
  facet_grid(~Metric) + 
  theme(legend.position = "top",
        #axis.text = element_text(size = 14),
        #axis.title = element_text(size = 16),
        #legend.text = element_text(size = 14),
        #legend.title = element_text(size = 14),
        #strip.text = element_text(size = 14),
        #strip.background = element_blank(),
        panel.background = element_rect(fill = "grey89")) + 
  labs(x = "Conviction",
       y = "Confidence",
       col = "Lift: ",
       size = "Score: ")
#
emose_rules_dependent %>% 
  DATAFRAME() %>% 
  pivot_longer(cols = c("lift", "conviction", "confidence"),
               names_to = "Metric",
               values_to = "Score") %>%
  mutate(RHS = str_remove(RHS, "\\{Classification="),
         RHS = str_remove(RHS, "\\}")) %>% 
  mutate(RHS = factor(RHS, levels = c("Rare", "Undetermined", "Abundant"))) %>% 
  ggplot(aes(RHS, Score)) + 
  geom_boxplot() + 
  theme_classic() +
  facet_wrap(~Metric, scales = "free")+
  labs(x = "Consequent") 


## dependent rules - nonredundant
# key items are in taxonomy var
# Remove redundancy by Improvement
dependent_rules_non_redundant <- 
  emose_rules_dependent %>% 
  DATAFRAME() %>% 
  filter(str_detect(LHS, "taxa")) %>%  # remove rules without key items
  separate_wider_delim(LHS, ",", names = "taxa", too_many = "debug") %>% 
  select(-LHS_ok, -LHS_pieces) %>% 
  mutate(taxa = factor(str_remove(taxa, "\\{taxa="))) %>%  
  group_by(taxa) %>% 
  arrange(desc(improvement)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) 

dependent_rules_non_redundant %>% View()

## remove redundancy by Complexity
non_redundant_by_complexity <- 
  emose_rules_dependent %>%
  DATAFRAME() %>% 
  mutate(LHSsize = str_count(LHS, ",")) %>% 
  filter(str_detect(LHS, "taxa")) %>%  # remove rules without key items
  separate_wider_delim(LHS, ",", names = "taxa", too_many = "debug") %>% 
  select(-LHS_ok, -LHS_pieces) %>% 
  mutate(taxa = str_remove(taxa, "\\{taxa="),
         taxa = str_remove(taxa, "\\}"),
         taxa = str_remove(taxa, ","),
         taxa = factor(taxa)) %>%
  group_by(taxa) %>%
  arrange(desc(LHSsize)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) %>% 
  filter(str_detect(LHS, "taxa")) %>% # to focus only on LHS with taxonomy
  arrange(desc(conviction))

non_redundant_by_complexity %>% View()

## remove redundancy by mutual information
non_redundant_by_mutualInfo <- 
  emose_rules_dependent %>%
  DATAFRAME() %>% 
  filter(str_detect(LHS, "taxa")) %>%  # remove rules without key items
  separate_wider_delim(LHS, ",", names = "taxa", too_many = "debug") %>% 
  select(-LHS_ok, -LHS_pieces) %>% 
  mutate(taxa = str_remove(taxa, "\\{taxa="),
         taxa = str_remove(taxa, "\\}"),
         taxa = str_remove(taxa, ","),
         taxa = factor(taxa)) %>%
  group_by(taxa) %>%
  arrange(desc(mutualInfo)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) %>% 
  filter(str_detect(LHS, "taxa")) %>% # to focus only on LHS with taxaomy
  arrange(desc(conviction))

non_redundant_by_mutualInfo %>% View()


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


gridExtra::grid.arrange(
  all_non_redundant %>% 
    ggplot(aes(Metric, confidence)) + 
    geom_boxplot(outlier.shape = "cross", outlier.color = "red") + 
    geom_jitter(height = 0, width = 0.1, col = "grey") +
    theme_classic() + 
    theme(axis.title.x = element_blank())+
    labs(y = "Confidence"),
  all_non_redundant %>% 
    ggplot(aes(Metric, lift)) + 
    geom_boxplot(outlier.shape = "cross", outlier.color = "red") +
    geom_jitter(height = 0, width = 0.1, col = "grey") +
    theme_classic() + 
    theme(axis.title.x = element_blank())+
    labs(y = "Lift"),
  all_non_redundant %>% 
    ggplot(aes(Metric, conviction)) + 
    geom_boxplot(outlier.shape = "cross", outlier.color = "red") + 
    geom_jitter(height = 0, width = 0.1, col = "grey") +
    theme_classic() +
    theme(axis.title.x = element_blank())+
    labs(y = "Conviction"), ncol = 3)


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
non_redundant_by_mutualInfo %>% write.csv("output/Rd_mutual_info_case2.csv")
non_redundant_by_complexity %>% write.csv("output/Rd_complex_case2.csv")
dependent_rules_non_redundant %>% write.csv("output/Rd_improv_case2.csv")




