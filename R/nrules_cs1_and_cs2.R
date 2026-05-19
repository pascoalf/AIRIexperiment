# number of rules plot - both case studies
grid.arrange(
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
         col = "Subset", tag = "a") + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12)),
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
         col = "Subset", tag = "b") + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12))
)
