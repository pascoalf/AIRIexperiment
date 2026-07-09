# CS1 compare alternatives
source("R/prepare_session.R")
source("R/plot_helper.R")

if(!exists("full_rules") ||
   !exists("full_rules_dependent") ||
   !exists("dependent_rules_non_redundant") ||
   !exists("non_redundant_by_mutualInfo") ||
   !exists("non_redundant_by_complexity")){
  source("R/cs1_airi_steps.R")
}

source("R/functions/get_rules_by.R")

# Flat threshold approach
conf.90_all  <- get_rules_by(full_rules, metric = "confidence", score = 0.9)
conf.90_dep <- get_rules_by(full_rules_dependent, metric = "confidence", score = 0.9)
conf.100_all <- get_rules_by(full_rules, metric = "confidence", score = 1)
conf.100_dep <- get_rules_by(full_rules_dependent, metric = "confidence", score = 1)
lift.1_all <- get_rules_by(full_rules, metric = "lift", score = 1)
lift.1_dep <- get_rules_by(full_rules_dependent, metric = "lift", score = 1)
lift.10_all <- get_rules_by(full_rules, metric = "lift", score = 10)
lift.10_dep <- get_rules_by(full_rules_dependent, metric = "lift", score = 10)
conv.5_all <- get_rules_by(full_rules, metric = "conviction", score = 5)
conv.5_dep <- get_rules_by(full_rules_dependent, metric = "conviction", score = 5)
conv.10_all <- get_rules_by(full_rules, metric = "conviction", score = 10) 
conv.10_dep <- get_rules_by(full_rules_dependent, metric = "conviction", score = 10)

# After threshold, if they are a lot, arrange by some other metric
# Merge as a single data frame
conf.90_all.df <- conf.90_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Conf. \U2265 90%",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
conf.90_dep.df <- conf.90_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Conf. \U2265 90%",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
conf.100_all.df <- conf.100_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Conf. = 100%",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
conf.100_dep.df <- conf.100_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Conf. = 100%",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
lift.1_all.df <- lift.1_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Lift \U2265 1",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
lift.1_dep.df <- lift.1_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Lift \U2265 1",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
lift.10_all.df <- lift.10_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Lift \U2265 10",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
lift.10_dep.df <- lift.10_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Lift \U2265 10",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
conv.5_all.df <- conv.5_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Conv. \U2265 5",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
conv.5_dep.df <- conv.5_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Conv. \U2265 5",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)
conv.10_all.df <- conv.10_all %>% 
  DATAFRAME() %>% 
  mutate(method = "Conv. \U2265 10",
         subset = "from all rules") %>% 
  select(!chi_p_adj)
conv.10_dep.df <- conv.10_dep %>% 
  DATAFRAME() %>% 
  mutate(method = "Conv. \U2265 10",
         subset = "from dependent rules") %>% 
  select(!chi_p_adj)

# airi - by improv
airi_mosj_dep_imp <- dependent_rules_non_redundant %>% 
  select(!Metric) %>% mutate(method = "AIRI \n by improvement", subset = "from dependent rules")

# airi - by mutual information
airi_mosj_dep_mi <- non_redundant_by_mutualInfo %>% 
  select(!Metric) %>% mutate(method = "AIRI \n by mutualInfo", subset = "from dependent rules")

# airi - by complexity
airi_mosj_dep_comp <- non_redundant_by_complexity %>% 
  select(!Metric) %>% mutate(method = "AIRI \n by complexity", subset = "from dependent rules")

# Add original rules
quality(full_rules)$mutualInfo = interestMeasure(full_rules, "mutualInfo")
quality(full_rules)$improvement = interestMeasure(full_rules, "improvement")
full_rules_df.temp <- DATAFRAME(full_rules)
full_rules_df.temp <- full_rules_df.temp %>% 
  select(!chi_p_adj) %>% 
  mutate(method = "None", subset = "from all rules")
# Add dependent rules
full_rules_dependent_df.temp <- DATAFRAME(full_rules_dependent)
full_rules_dependent_df.temp <- full_rules_dependent_df.temp %>% 
  select(!chi_p_adj) %>% 
  mutate(method = "None", subset = "from dependent rules")

#
multi_options_cs1 <- airi_mosj_dep_comp %>% 
  rbind(airi_mosj_dep_imp) %>%
  rbind(airi_mosj_dep_mi) %>% 
  rbind(conf.90_all.df) %>% 
  rbind(conf.90_dep.df) %>% 
  rbind(conf.100_all.df) %>% 
  rbind(conf.100_dep.df) %>% 
  rbind(lift.1_all.df) %>% 
  rbind(lift.1_dep.df) %>% 
  rbind(full_rules_df.temp) %>% 
  rbind(full_rules_dependent_df.temp) %>% 
  rbind(lift.10_all.df) %>% 
  rbind(lift.10_dep.df) %>% 
  rbind(conv.10_dep.df) %>% 
  rbind(conv.5_dep.df) %>% 
  rbind(conv.10_all.df) %>% 
  rbind(conv.5_all.df)%>% 
  mutate(method = factor(method, 
                         levels = c("None", 
                                    "AIRI \n by improvement",
                                    "AIRI \n by mutualInfo",
                                    "AIRI \n by complexity",
                                    "Conf. \U2265 90%", "Conf. = 100%", 
                                    "Lift \U2265 1", "Lift \U2265 10",
                                    "Conv. \U2265 10", "Conv. \U2265 5")))

#
count_mosj_rules <- multi_options_cs1 %>% 
  group_by(method, subset) %>% 
  count()

cs1_rule_count_comparison_plot <- plot_rule_count_comparison(count_mosj_rules,
                                                             title = "Case study 1 (MOSJ)")

save_airi_plot(cs1_rule_count_comparison_plot,
               filename = "cs1_rule_count_comparison.png",
               width = 7,
               height = 5)

cs1_rule_count_comparison_plot
