## AIRI - emose 
source("R/prepare_session.R")
source("R/plot_helper.R")

if(!exists("emose_rules") || !exists("emose_rules_df")){
  source("R/cs2_arm_emose.R")
}

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

cs2_dependent_rule_scores_plot <- plot_dependent_rule_scores(quality(emose_rules_dependent))

save_airi_plot(cs2_dependent_rule_scores_plot,
               filename = "cs2_dependent_rule_scores.png",
               width = 7,
               height = 4.6)

cs2_dependent_rule_scores_plot
#
cs2_dependent_rule_metric_distribution_plot <- plot_dependent_rule_metric_distribution(emose_rules_dependent)

save_airi_plot(cs2_dependent_rule_metric_distribution_plot,
               filename = "cs2_dependent_rule_metric_distribution.png",
               width = 7,
               height = 4.6)

cs2_dependent_rule_metric_distribution_plot


## dependent rules - nonredundant
# key items are in taxonomy var
extract_lhs_taxa <- function(rules){
  lhs_items <- LIST(lhs(rules), decode = TRUE)

  map_chr(lhs_items, function(items){
    taxa_items <- items[str_starts(items, "taxa=")]

    if(length(taxa_items) == 0){
      return(NA_character_)
    }

    str_remove(taxa_items[1], "^taxa=")
  })
}

emose_rules_dependent_df <- emose_rules_dependent %>%
  DATAFRAME() %>%
  mutate(taxa = factor(extract_lhs_taxa(emose_rules_dependent)),
         LHSsize = size(lhs(emose_rules_dependent)))

# Remove redundancy by Improvement
emose_dependent_rules_non_redundant <- emose_rules_dependent_df %>% 
  filter(!is.na(taxa)) %>%  # remove rules without key items
  group_by(taxa) %>% 
  arrange(desc(improvement)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) 

#emose_dependent_rules_non_redundant %>% View()

## remove redundancy by Complexity
emose_non_redundant_by_complexity <- 
  emose_rules_dependent_df %>% 
  filter(!is.na(taxa)) %>%  # remove rules without key items
  group_by(taxa) %>%
  arrange(desc(LHSsize)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) %>% 
  arrange(desc(conviction))

#emose_non_redundant_by_complexity %>% View()

## remove redundancy by mutual information
emose_non_redundant_by_mutualInfo <- 
  emose_rules_dependent_df %>% 
  filter(!is.na(taxa)) %>%  # remove rules without key items
  group_by(taxa) %>%
  arrange(desc(mutualInfo)) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(LHS, RHS, support,	confidence, coverage,	lift,	count,	conviction,	mutualInfo,	improvement) %>% 
  arrange(desc(conviction))

#emose_non_redundant_by_mutualInfo %>% View()


## idea: summarise metrics by method to solve non-redundant rules --> what method obtained highest conviction?
emose_non_redundant_by_mutualInfo$Metric <- "Mutual\ninformation"
emose_non_redundant_by_complexity$Metric <- "Complexity"
emose_dependent_rules_non_redundant$Metric <-"Improvement"   

emose_all_non_redundant <- 
  rbind(emose_non_redundant_by_mutualInfo,
        emose_non_redundant_by_complexity,
        emose_dependent_rules_non_redundant) %>% 
  mutate(Classification = case_when(str_detect(RHS, "Abundant") ~ "Abundant",
                                    str_detect(RHS, "Undetermined") ~ "Undetermined",
                                    str_detect(RHS, "Rare") ~ "Rare")) 


cs2_airi_selection_metric_distribution_plot <- plot_airi_selection_metric_distribution(emose_all_non_redundant)

save_airi_plot(cs2_airi_selection_metric_distribution_plot,
               filename = "cs2_airi_selection_metric_distribution.png",
               width = 7,
               height = 4.2)

cs2_airi_selection_metric_distribution_plot


# percent shared rules
emose_shared_rules <- emose_all_non_redundant %>% 
  mutate(rule = paste(LHS, "->",RHS)) %>% 
  select(rule, Metric) %>% 
  distinct()

# venn diagram
cs2_airi_shared_rules_venn_plot <- ggVennDiagram(x = list(emose_non_redundant_by_mutualInfo$LHS,
                                                          emose_non_redundant_by_complexity$LHS,
                                                          emose_dependent_rules_non_redundant$LHS),
                                                 category.names = c("Mutual\nInformation",
                                                                    "Complexity",
                                                                    "Improvement"),
                                                 label_size = 4) + 
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
  guides(fill = "none") +
  xlim(-6,9)+
  ylim(-9,6)

save_airi_plot(cs2_airi_shared_rules_venn_plot,
               filename = "cs2_airi_shared_rules_venn.png",
               width = 5.5,
               height = 5)

cs2_airi_shared_rules_venn_plot

## save non redundant rules for inspection
emose_non_redundant_by_mutualInfo %>% write.csv("rule-sets/emose_airi_by_mutual_information.csv")
emose_non_redundant_by_complexity %>% write.csv("rule-sets/emose_airi_by_complexity.csv")
emose_dependent_rules_non_redundant %>% write.csv("rule-sets/emose_airi_by_improvement.csv")

