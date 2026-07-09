# systematic comparisons
source("R/prepare_session.R")
source("R/plot_helper.R")

if(!exists("full_rules") || !exists("full_rules_dependent")){
  source("R/cs1_airi_steps.R")
}

# load functions
source("R/functions/get_rules_by.R")
source("R/functions/count_rules_by.R")
# Range of values
# for confidence
conf_values_cs1 <- quality(full_rules)$confidence %>% round(3) %>% unique() %>% sort()
conv_values_cs1 <- quality(full_rules)$conviction %>% round(3) %>% unique() %>% sort()
lift_values_cs1 <- quality(full_rules)$lift %>% round(3) %>% unique() %>% sort()


# make df with options
options_cs1 <- data.frame(metric = "confidence", score = conf_values_cs1) %>% 
  rbind(data.frame(metric = "conviction", score = conv_values_cs1)) %>% 
  rbind(data.frame(metric = "lift", score = lift_values_cs1))


## calculate number of rules for all options
## for all rules
cs1_nrules_all  <- map2(.x = options_cs1$metric, 
                        .y = options_cs1$score,
                        .f = ~count_rules_by(full_rules, 
                                         metric = .x, score = .y)) %>% unlist()
cs1_nrules_all <- options_cs1 %>% cbind(cs1_nrules_all)
cs1_nrules_all <- cs1_nrules_all %>% mutate(nrules = cs1_nrules_all)
cs1_nrules_all <- cs1_nrules_all %>% mutate(subset = "from all rules")
#
cs1_nrules_all <- cs1_nrules_all[,c(1,2,4,5)] %>% rbind(cs1_nrules_all[,c(1,2,4,5)])

## for dependent rules
cs1_nrules_dep  <- map2(.x = options_cs1$metric, 
                        .y = options_cs1$score,
                        .f = ~count_rules_by(full_rules_dependent, 
                                             metric = .x, score = .y)) %>% unlist()
cs1_nrules_dep <- options_cs1 %>% cbind(cs1_nrules_dep)
cs1_nrules_dep <- cs1_nrules_dep %>% mutate(nrules = cs1_nrules_dep)
cs1_nrules_dep <- cs1_nrules_dep %>% mutate(subset = "from dependent rules")
#
cs1_nrules_dep <- cs1_nrules_dep[,c(1,2,4,5)] %>% rbind(cs1_nrules_dep[,c(1,2,4,5)])
cs1_nrules<- cs1_nrules_all %>%
  rbind(cs1_nrules_dep)

## plot
cs1_systematic_rule_count_plot_a <- cs1_nrules %>% 
  mutate(metric = str_to_title(metric)) %>% 
  ggplot(aes(score, nrules, col = subset)) + 
  geom_point(alpha = 0.55) + 
  geom_line(aes(group = subset), col = "grey20") +
  geom_hline(yintercept = 1, lty = "dashed", col = "grey") +
  facet_wrap(~metric, scale = "free") +
  scale_x_continuous(n.breaks = 4) +
  scale_y_log10(labels = scales::label_number()) +
  airi_plot_theme() +
  theme(axis.text.x = element_text(size = 8)) +
  labs(y = "Number of rules (log10 scale)",
       x = "Score",
       col = "Subset",
       tag = "a")

save_airi_plot(cs1_systematic_rule_count_plot_a,
               filename = "cs1_systematic_rule_count_a.png",
               width = 7,
               height = 4.8)

cs1_systematic_rule_count_plot_a

#
cs1_systematic_rule_count_plot_b <- cs1_nrules %>% 
  mutate(metric = str_to_title(metric)) %>% 
  ggplot(aes(score, nrules, col = subset)) + 
  geom_point(alpha = 0.55) + 
  geom_line(aes(group = subset), col = "grey20") +
  geom_hline(yintercept = 1, lty = "dashed", col = "grey") +
  facet_wrap(~metric, scale = "free") +
  scale_x_continuous(n.breaks = 4) +
  scale_y_log10(labels = scales::label_number()) +
  airi_plot_theme() +
  theme(axis.text.x = element_text(size = 8)) +
  labs(y = "Number of rules (log10 scale)",
       x = "Score",
       col = "Subset",
       tag = "b")

save_airi_plot(cs1_systematic_rule_count_plot_b,
               filename = "cs1_systematic_rule_count_b.png",
               width = 7,
               height = 4.8)

cs1_systematic_rule_count_plot_b
