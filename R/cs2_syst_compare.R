source("R/prepare_session.R")
source("R/plot_helper.R")

if(!exists("emose_rules") || !exists("emose_rules_dependent")){
  source("R/cs2_airi_emose.R")
}

# load functions
source("R/functions/get_rules_by.R")
source("R/functions/count_rules_by.R")
# Range of values
# for confidence
conf_values_cs2 <- quality(emose_rules)$confidence %>% round(3) %>% unique() %>% sort()
conv_values_cs2 <- quality(emose_rules)$conviction %>% round(3) %>% unique() %>% sort()
lift_values_cs2 <- quality(emose_rules)$lift %>% round(3) %>% unique() %>% sort()

# make df with options
options_cs2 <- data.frame(metric = "confidence", score = conf_values_cs2) %>% 
  rbind(data.frame(metric = "conviction", score = conv_values_cs2)) %>% 
  rbind(data.frame(metric = "lift", score = lift_values_cs2))

## calculate number of rules for all options
## for all rules
cs2_nrules_all  <- map2(.x = options_cs2$metric, 
                        .y = options_cs2$score,
                        .f = ~count_rules_by(emose_rules, 
                                             metric = .x, score = .y)) %>% unlist()
cs2_nrules_all <- options_cs2 %>% cbind(cs2_nrules_all)
cs2_nrules_all <- cs2_nrules_all %>% mutate(nrules = cs2_nrules_all)
cs2_nrules_all <- cs2_nrules_all %>% mutate(subset = "from all rules")
#
cs2_nrules_all <- cs2_nrules_all[,c(1,2,4,5)] %>% rbind(cs2_nrules_all[,c(1,2,4,5)])

## for dependent rules
cs2_nrules_dep  <- map2(.x = options_cs2$metric, 
                        .y = options_cs2$score,
                        .f = ~count_rules_by(emose_rules_dependent, 
                                             metric = .x, score = .y)) %>% unlist()
cs2_nrules_dep <- options_cs2 %>% cbind(cs2_nrules_dep)
cs2_nrules_dep <- cs2_nrules_dep %>% mutate(nrules = cs2_nrules_dep)
cs2_nrules_dep <- cs2_nrules_dep %>% mutate(subset = "from dependent rules")
#
cs2_nrules_dep <- cs2_nrules_dep[,c(1,2,4,5)] %>% rbind(cs2_nrules_dep[,c(1,2,4,5)])
cs2_nrules<- cs2_nrules_all %>%
  rbind(cs2_nrules_dep)

## plot
cs2_systematic_rule_count_log_plot <- cs2_nrules %>% 
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

save_airi_plot(cs2_systematic_rule_count_log_plot,
               filename = "cs2_systematic_rule_count_log.png",
               width = 7,
               height = 4.8)

cs2_systematic_rule_count_log_plot

# normal scale
cs2_systematic_rule_count_linear_plot <- cs2_nrules %>% 
  mutate(metric = str_to_title(metric)) %>% 
  ggplot(aes(score, nrules, col = subset)) + 
  geom_point(alpha = 0.55) + 
  geom_line(aes(group = subset), col = "grey20") +
  geom_hline(yintercept = 1, lty = "dashed", col = "grey") +
  facet_wrap(~metric, scale = "free") +
  scale_x_continuous(n.breaks = 4) +
  airi_plot_theme() +
  theme(axis.text.x = element_text(size = 8)) +
  labs(y = "Number of rules",
       x = "Score",
       col = "Subset",
       tag = "b")

save_airi_plot(cs2_systematic_rule_count_linear_plot,
               filename = "cs2_systematic_rule_count_linear.png",
               width = 7,
               height = 4.8)

cs2_systematic_rule_count_linear_plot

if(exists("cs1_systematic_rule_count_plot_a")){
  systematic_rule_count_log_grid_plot <- ggarrange(
    cs1_systematic_rule_count_plot_a + labs(tag = NULL),
    cs2_systematic_rule_count_log_plot + labs(tag = NULL),
    ncol = 2,
    labels = c("a", "b"),
    common.legend = TRUE,
    legend = "top"
  )

  save_airi_plot(systematic_rule_count_log_grid_plot,
                 filename = "cs1_cs2_systematic_rule_count_log.png",
                 width = 12,
                 height = 5.4)
}
