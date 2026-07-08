source("R/prepare_session.R")

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
cs2_nrules %>% 
    mutate(metric = str_to_title(metric)) %>% 
    ggplot(aes(score, nrules, col = subset)) + 
    geom_point(alpha = 0.55) + 
    geom_line(aes(group = subset), col = "grey20") +
    geom_hline(yintercept = 1, lty = "dashed", col = "grey") +
    facet_wrap(~metric, scale = "free") +
    scale_y_log10() +
    labs(y = "Number of rules (Log10)",
         x = "Score",
         col = "Subset", tag = "a") + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12))

# normal scale
cs2_nrules %>% 
    mutate(metric = str_to_title(metric)) %>% 
    ggplot(aes(score, nrules, col = subset)) + 
    geom_point(alpha = 0.55) + 
    geom_line(aes(group = subset), col = "grey20") +
    geom_hline(yintercept = 1, lty = "dashed", col = "grey") +
    facet_wrap(~metric, scale = "free") +
    #scale_y_log10() +
    labs(y = "Number of rules",
         x = "Score",
         col = "Subset", tag = "a") + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12))
