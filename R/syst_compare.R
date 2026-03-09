# systematic comparisons

# load functions
source("R/functions/redundancy.R")
source("R/functions/get_rules_by.R")
source("R/functions/count_rules_by.R")
# Range of values
# for confidence
conf_values_cs1 <- quality(full_rules)$confidence %>% round(3) %>% unique() %>% sort()
conv_values_cs1 <- quality(full_rules)$conviction %>% round(3) %>% unique() %>% sort()
lift_values_cs1 <- quality(full_rules)$lift %>% round(3) %>% unique() %>% sort()

# wrap redundancy and get rules
redund_get <- function(rules, metric, score){
  redundancy(get_rules_by(rules = rules, metric = metric, score = score))  
}

# make df with options
options_cs1 <- data.frame(metric = "confidence", score = conf_values_cs1) %>% 
  rbind(data.frame(metric = "conviction", score = conv_values_cs1)) %>% 
  rbind(data.frame(metric = "lift", score = lift_values_cs1))

# calculate redundancy for all metrics and all scores
cs1_redund_all  <- map2(.x = options_cs1$metric, 
     .y = options_cs1$score,
     .f = ~redund_get(full_rules, metric = .x, score = .y)) %>% unlist()
#
cs1_redund_all <- options_cs1 %>% cbind(cs1_redund_all)
cs1_redund_all <- cs1_redund_all %>% mutate(redundancy = cs1_redund_all)
cs1_redund_all <- cs1_redund_all %>% mutate(subset = "from all rules")
# For dependent subset
cs1_redund_dep  <- map2(.x = options_cs1$metric, 
                        .y = options_cs1$score,
                        .f = ~redund_get(full_rules_dependent, 
                                         metric = .x, score = .y)) %>% unlist()
#
cs1_redund_dep <- options_cs1 %>% cbind(cs1_redund_dep)
cs1_redund_dep <- cs1_redund_dep %>% mutate(redundancy = cs1_redund_dep)
cs1_redund_dep <- cs1_redund_dep %>% mutate(subset = "from dependent rules")
#
cs1_redund <- cs1_redund_all[,c(1,2,4,5)] %>% rbind(cs1_redund_dep[,c(1,2,4,5)])

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
grid.arrange(
  #
  cs1_nrules %>% 
    mutate(metric = str_to_title(metric)) %>% 
    ggplot(aes(score, nrules, col = subset)) + 
    geom_point(alpha = 0.55) + 
    geom_line(aes(group = subset), col = "grey20") +
    geom_hline(yintercept = 1, lty = "dashed", col = "grey") +
    facet_wrap(~metric, scale = "free") +
    scale_y_log10() +
    labs(y = "Number of rules (Log10)",
         x = "Score",
         col = "Subset") + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 12)),
#
  cs1_redund %>% 
  mutate(redundancy = redundancy*100) %>% 
  mutate(metric = str_to_title(metric)) %>% 
  ggplot(aes(score, redundancy, col = subset)) + 
  geom_point(alpha = 0.55) + 
  geom_line(aes(group = subset), col = "grey20") +
  #scale_y_log10() + 
  facet_wrap(~metric, scale = "free") +
  labs(y = "Redundancy",
       x = "Score",
       col = "Subset") + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(size = 12)))

grid.arrange(
  #
  cs1_nrules %>% 
    mutate(metric = str_to_title(metric)) %>% 
    ggplot(aes(score, nrules, col = subset)) + 
    geom_point(alpha = 0.55) + 
    geom_line(aes(group = subset), col = "grey20") +
    geom_hline(yintercept = 1, lty = "dashed", col = "grey") +
    facet_wrap(~metric, scale = "free") +
    scale_y_log10() +
    labs(y = "Number of rules (Log10)",
         x = "Score",
         col = "Subset") + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 12)),
  #
  cs1_redund %>% 
    mutate(redundancy = redundancy*100) %>% 
    mutate(metric = str_to_title(metric)) %>% 
    ggplot(aes(score, redundancy, col = subset)) + 
    geom_point(alpha = 0.55) + 
    geom_line(aes(group = subset), col = "grey20") +
    scale_y_log10() + 
    facet_wrap(~metric, scale = "free") +
    labs(y = "Redundancy (Log10)",
         x = "Score",
         col = "Subset") + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 12)))
