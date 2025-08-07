# AIRI steps

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
full_rules_dependent %>% 
  DATAFRAME() %>% 
  filter(!is.infinite(conviction)) %>% 
  ggplot(aes(conviction, confidence, col = lift)) + 
  geom_point(size = 4) + 
  scale_color_gradient(low = reds[1], high = reds[9]) + 
  labs(x = "Conviction",
       y = "Confidence",
       col = "Lift: ") + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.background = element_rect(fill = "grey90")) + 
  lims(y = c(0.80, 1.0))

## Step 2 - remove redundant rules
# Calculate Mutual information and Improvement
quality(full_rules_dependent)$mutualInfo <- interestMeasure(full_rules_dependent,
                                                            measure = "mutualInformation")
quality(full_rules_dependent)$improvement <- interestMeasure(full_rules_dependent,
                                                             measure = "improvement")
# Inspect scores for dependent rules
gridExtra::grid.arrange(
  # improvement
  quality(full_rules_dependent) %>% 
    ggplot(aes(conviction, confidence))+
    geom_point(aes(size = improvement, col = lift))+
    scale_color_gradient(low = reds[1], high = reds[9]) + 
    theme_classic() + 
    theme(legend.position = "top",
          panel.background = element_rect(fill = "grey90")) +
    labs(x = "Conviction",
         y = "Confidence",
         col = "Lift: ",
         size = "Improvement: ")# + 
  #guides(size  = FALSE)
  ,
  # mutual information
  quality(full_rules_dependent) %>% 
    ggplot(aes(conviction, confidence))+
    geom_point(aes(size = mutualInfo, col = lift))+
    scale_color_gradient(low = reds[1], high = reds[9]) + 
    theme_classic() + 
    theme(legend.position = "top",
          panel.background = element_rect(fill = "grey90"))+
    labs(x = "Conviction",
         y = "Confidence",
         col = "Lift: ",
         size = "Mutual\nInformation: "), 
  nrow=2
)


quality(full_rules_dependent) %>% 
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
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "grey89")) + 
  labs(x = "Conviction",
       y = "Confidence",
       col = "Lift: ",
       size = "Score: ")
#


## dependent rules - nonredundant
# key items are in taxonomy var
# Remove redundancy by Improvement
dependent_rules_non_redundant <- 
  full_rules_dependent %>% 
  DATAFRAME() %>% 
  filter(str_detect(LHS, "taxon")) %>%  # remove rules without key items
  separate_wider_delim(LHS, ",", names = "taxon", too_many = "debug") %>% 
  select(-LHS_ok, -LHS_pieces) %>% 
  mutate(taxon = factor(str_remove(taxon, "\\{taxon="))) %>%  
  group_by(taxon) %>% 
  arrange(desc(improvement)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) 

dependent_rules_non_redundant %>% View()

## remove redundancy by Complexity
non_redundant_by_complexity <- 
  full_rules_dependent %>%
  DATAFRAME() %>% 
  mutate(LHSsize = str_count(LHS, ",")) %>% 
  filter(str_detect(LHS, "taxon")) %>%  # remove rules without key items
  separate_wider_delim(LHS, ",", names = "taxon", too_many = "debug") %>% 
  select(-LHS_ok, -LHS_pieces) %>% 
  mutate(taxon = str_remove(taxon, "\\{taxon="),
         taxon = str_remove(taxon, "\\}"),
         taxon = str_remove(taxon, ","),
         taxon = factor(taxon)) %>%
  group_by(taxon) %>%
  arrange(desc(LHSsize)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) %>% 
  filter(str_detect(LHS, "taxon")) %>% # to focus only on LHS with taxonomy
  arrange(desc(conviction))

#
## remove redundancy by mutual information
non_redundant_by_mutualInfo <- 
  full_rules_dependent %>%
  DATAFRAME() %>% 
  filter(str_detect(LHS, "taxon")) %>%  # remove rules without key items
  separate_wider_delim(LHS, ",", names = "taxon", too_many = "debug") %>% 
  select(-LHS_ok, -LHS_pieces) %>% 
  mutate(taxon = str_remove(taxon, "\\{taxon="),
         taxon = str_remove(taxon, "\\}"),
         taxon = str_remove(taxon, ","),
         taxon = factor(taxon)) %>%
  group_by(taxon) %>%
  arrange(desc(mutualInfo)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) %>% 
  filter(str_detect(LHS, "taxon")) %>% # to focus only on LHS with taxonomy
  arrange(desc(conviction))


## idea: summarise metrics by method to solve non-redundant rules --> what method obtained highest conviction?
  
  
  
  
