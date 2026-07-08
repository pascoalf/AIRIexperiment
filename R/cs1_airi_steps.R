# AIRI steps
source("R/prepare_session.R")

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

quality(full_rules_dependent) %>% 
  filter(!is.infinite(conviction)) %>% 
  pivot_longer(cols = c("mutualInfo", "improvement"),
               values_to = "Score",
               names_to = "Metric") %>% 
  mutate(Metric = ifelse(Metric == "mutualInfo", "Mutual Information", "Improvement")) %>% 
  ggplot(aes(conviction, 
             confidence, 
             col = lift)) + 
  geom_point(size = 2) + 
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
full_rules_dependent %>% 
  DATAFRAME() %>% 
  pivot_longer(cols = c("lift", "conviction", "confidence"),
               names_to = "Metric",
               values_to = "Score") %>%
  mutate(RHS = str_remove(RHS, "\\{Classification="),
         RHS = str_remove(RHS, "\\}")) %>% 
  mutate(RHS = factor(RHS, levels = c("Rare", "Undetermined", "Abundant"))) %>% 
  mutate(Metric = str_to_title(Metric)) %>% 
  ggplot(aes(RHS, Score)) + 
  geom_boxplot() + 
  theme_classic() +
  theme(strip.text = element_text(size = 12),
        axis.title = element_text(size = 12)) + 
  facet_wrap(~Metric, scales = "free")+
  labs(y = "Score",
       x = "Consequent") 


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


gridExtra::grid.arrange(
all_non_redundant %>% 
  ggplot(aes(Metric, confidence)) + 
  geom_boxplot(outliers = FALSE) + 
  geom_jitter(height = 0, width = 0.1, col = "grey") +
  theme_classic() + 
  theme(axis.title.x = element_blank())+
  labs(y = "Confidence"),
all_non_redundant %>% 
  ggplot(aes(Metric, lift)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(height = 0, width = 0.1, col = "grey") +
  theme_classic() + 
  theme(axis.title.x = element_blank())+
  labs(y = "Lift"),
all_non_redundant %>% 
  ggplot(aes(Metric, conviction)) + 
  geom_boxplot(outliers = FALSE) + 
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
#non_redundant_by_mutualInfo %>% write.csv("rule-sets/mosj_airi_by_mutual_information.csv")
#non_redundant_by_complexity %>% write.csv("rule-sets/mosj_airi_by_complexity.csv")
#dependent_rules_non_redundant %>% write.csv("rule-sets/mosj_airi_by_improvement.csv")
