# Compare methods - case study 2
source("R/prepare_session.R")
source("R/plot_helper.R")

# CS1 compare alternatives

if(!exists("emose_rules") ||
   !exists("emose_rules_dependent") ||
   !exists("emose_dependent_rules_non_redundant") ||
   !exists("emose_non_redundant_by_mutualInfo") ||
   !exists("emose_non_redundant_by_complexity")){
  source("R/cs2_airi_emose.R")
}

source("R/functions/get_rules_by.R")

# Flat threshold approach
cs2.conf.90_all  <- get_rules_by(emose_rules, metric = "confidence", score = 0.9)
cs2.conf.90_dep <- get_rules_by(emose_rules_dependent, metric = "confidence", score = 0.9)
cs2.conf.100_all <- get_rules_by(emose_rules, metric = "confidence", score = 1)
cs2.conf.100_dep <- get_rules_by(emose_rules_dependent, metric = "confidence", score = 1)
cs2.lift.1_all <- get_rules_by(emose_rules, metric = "lift", score = 1)
cs2.lift.1_dep <- get_rules_by(emose_rules_dependent, metric = "lift", score = 1)
cs2.lift.10_all <- get_rules_by(emose_rules, metric = "lift", score = 10)
cs2.lift.10_dep <- get_rules_by(emose_rules_dependent, metric = "lift", score = 10)
cs2.conv.1_all <- get_rules_by(emose_rules, metric = "conviction", score = 1)
cs2.conv.1_dep <- get_rules_by(emose_rules_dependent, metric = "conviction", score = 5)
cs2.conv.5_all <- get_rules_by(emose_rules, metric = "conviction", score = 5) 
cs2.conv.5_dep <- get_rules_by(emose_rules_dependent, metric = "conviction", score = 5)

# After threshold, if they are a lot, arrange by some other metric
# Merge as a single data frame
cs2.conf.90_all.df <- cs2.conf.90_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Conf. \U2265 90%",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
cs2.conf.90_dep.df <- cs2.conf.90_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Conf. \U2265 90%",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
cs2.conf.100_all.df <- cs2.conf.100_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Conf. = 100%",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
cs2.conf.100_dep.df <- cs2.conf.100_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Conf. = 100%",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
cs2.lift.1_all.df <- cs2.lift.1_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Lift \U2265 1",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
cs2.lift.1_dep.df <- cs2.lift.1_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Lift \U2265 1",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
cs2.lift.10_all.df <- cs2.lift.10_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Lift \U2265 10",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
cs2.lift.10_dep.df <- cs2.lift.10_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Lift \U2265 10",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
cs2.conv.1_all.df <- cs2.conv.1_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Conv. \U2265 1",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
cs2.conv.1_dep.df <- cs2.conv.1_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Conv. \U2265 1",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
cs2.conv.5_all.df <- cs2.conv.5_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Conv. \U2265 5",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
cs2.conv.5_dep.df <- cs2.conv.5_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Conv. \U2265 5",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)

# airi - by improv
cs2.airi_emose_dep_imp <- emose_dependent_rules_non_redundant %>% 
  select(!Metric) %>% mutate(method = "AIRI \n by improvement", subset = "from dependent rules")

# airi - by mutual information
cs2.airi_emose_dep_mi <- emose_non_redundant_by_mutualInfo %>% 
  select(!Metric) %>% mutate(method = "AIRI \n by mutualInfo", subset = "from dependent rules")

# airi - by complexity
cs2.airi_emose_dep_comp <- emose_non_redundant_by_complexity %>% 
  select(!Metric) %>% mutate(method = "AIRI \n by complexity", subset = "from dependent rules")

# Add original rules
quality(emose_rules)$mutualInfo = interestMeasure(emose_rules, "mutualInfo")
quality(emose_rules)$improvement = interestMeasure(emose_rules, "improvement")
cs2.emose_rules_df.temp <- DATAFRAME(emose_rules)
cs2.emose_rules_df.temp <- cs2.emose_rules_df.temp %>% 
  select(!chi_p_adj) %>% 
  mutate(method = "None", subset = "from all rules")
# Add dependent rules
cs2.emose_rules_dependent_df.temp <- DATAFRAME(emose_rules_dependent)
cs2.emose_rules_dependent_df.temp <- cs2.emose_rules_dependent_df.temp %>% 
  select(!chi_p_adj) %>% 
  mutate(method = "None", subset = "from dependent rules")

#
multi_options_cs2 <- cs2.airi_emose_dep_comp %>% 
  rbind(cs2.airi_emose_dep_imp) %>%
  rbind(cs2.airi_emose_dep_mi) %>% 
  rbind(cs2.conf.90_all.df) %>% 
  rbind(cs2.conf.90_dep.df) %>% 
  rbind(cs2.conf.100_all.df) %>% 
  rbind(cs2.conf.100_dep.df) %>% 
  rbind(cs2.lift.1_all.df) %>% 
  rbind(cs2.lift.1_dep.df) %>% 
  rbind(cs2.emose_rules_df.temp) %>% 
  rbind(cs2.emose_rules_dependent_df.temp) %>% 
  rbind(cs2.lift.10_all.df) %>% 
  rbind(cs2.lift.10_dep.df) %>% 
  rbind(cs2.conv.5_dep.df) %>% 
  rbind(cs2.conv.1_dep.df) %>% 
  rbind(cs2.conv.5_all.df) %>% 
  rbind(cs2.conv.1_all.df)%>% 
  mutate(method = factor(method, 
                         levels = c("None", 
                                    "AIRI \n by improvement",
                                    "AIRI \n by mutualInfo",
                                    "AIRI \n by complexity",
                                    "Conf. \U2265 90%", "Conf. = 100%", 
                                    "Lift \U2265 1", "Lift \U2265 10",
                                    "Conv. \U2265 5", "Conv. \U2265 1")))

#
cs2.count_emose_rules <- multi_options_cs2 %>% 
  group_by(method, subset) %>% 
  count()
#
cs2_rule_count_comparison_plot <- plot_rule_count_comparison(cs2.count_emose_rules,
                                                             title = "Case study 2 (EMOSE)")

save_airi_plot(cs2_rule_count_comparison_plot,
               filename = "cs2_rule_count_comparison.png",
               width = 7,
               height = 5)

cs2_rule_count_comparison_plot

#
cs2_mutual_information_comparison_plot <- multi_options_cs2 %>% 
  ggplot(aes(method, mutualInfo, fill = subset)) + 
  geom_boxplot(outlier.alpha = 0.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  airi_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin = margin(5.5, 5.5, 12, 5.5)) +
  labs(y = "Mutual information",
       x = "Method",
       fill = "Subset")

save_airi_plot(cs2_mutual_information_comparison_plot,
               filename = "cs2_mutual_information_comparison.png",
               width = 7,
               height = 5)

cs2_mutual_information_comparison_plot
