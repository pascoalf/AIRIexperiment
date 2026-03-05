# CS1 compare alternatives

## function to get rules by approach
get_rules_by <- function(rules, by = NULL, metric = "confidence", score = NULL){
  if(metric == "confidence"){
    temp_rules <- rules[quality(rules)$confidence >= score]    
  } else if(metric == "lift"){
    temp_rules <- rules[quality(rules)$lift >= score]
  } else if(metric == "conviction"){
    temp_rules <- rules[quality(rules)$conviction >= score]
  } else {stop("select valid metric")}
  quality(temp_rules)$mutualInfo <- interestMeasure(temp_rules, measure = "mutualInformation")
  quality(temp_rules)$improvement <- interestMeasure(temp_rules, measure = "improvement")
  return(temp_rules)
}

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
#
airi_mosj_dep <- non_redundant_by_mutualInfo %>% 
  select(!Metric) %>% mutate(method = "AIRI", subset = "from dependent rules")

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
multi_options_cs1 <- airi_mosj_dep %>% 
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
                         levels = c("None", "AIRI",
                                    "Conf. = 90%", "Conf. = 100%", 
                                    "Lift > 1", "Lift > 10",
                                    "Conv. ≥ 10", "Conv. ≥ 5")))

#
count_mosj_rules <- multi_options_cs1 %>% 
  group_by(method, subset) %>% 
  count()
count_mosj_rules %>% 
  ggplot(aes(method, n , fill = subset)) + 
  geom_col(position = "dodge") + 
  scale_y_log10() + 
  theme_classic() + 
  theme(legend.position = "top") +
  labs(y = "Number of rules (Log10 scale)",
       x = "Method",
       fill = "Subset")

# mutual information 
multi_options_cs1 %>% 
  ggplot(aes(method, improvement, col = subset)) + 
  stat_summary()+ 
  theme_classic() + 
  theme(legend.position = "top") +
  labs(y = "Improvement (mean \U2213 sd)",
       x = "Method",
       col = "Subset")
#
multi_options_cs1 %>% 
  ggplot(aes(method, mutualInfo, col = subset)) + 
  stat_summary()+ 
  theme_classic() + 
  theme(legend.position = "top") +
  labs(y = "Mutual Information (mean \U2213 sd)",
       x = "Method",
       col = "Subset")

## From previous, look deeper at best methods
## best methods: Conf100; Conv10; Conv5; Lift10 


# Summarise




# could we also add a network???



