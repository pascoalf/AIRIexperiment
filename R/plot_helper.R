# Shared figure style helpers

airi_plot_theme <- function(base_size = 12){
  theme_classic(base_size = base_size) +
    theme(legend.position = "top",
          legend.title = element_text(size = base_size),
          legend.text = element_text(size = base_size - 2),
          axis.title = element_text(size = base_size),
          axis.text = element_text(size = base_size - 2),
          strip.background = element_blank(),
          strip.text = element_text(size = base_size, face = "bold"),
          panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3))
}

airi_rule_set_colors <- c("All rules" = "#0072B2",
                          "Dependent rules" = "#D55E00",
                          "AIRI" = "#009E73")

airi_case_study_colors <- c("Case study 1 (MOSJ)" = "#0072B2",
                            "Case study 2 (EMOSE)" = "#D55E00")

airi_log_breaks <- c(0, 1, 10, 100, 1000, 10000, 100000, 1000000)

airi_pseudo_log_y <- function(){
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                     breaks = airi_log_breaks,
                     labels = scales::label_number())
}

save_airi_plot <- function(plot,
                           filename,
                           width = 7,
                           height = 5,
                           dpi = 600,
                           units = "in"){
  dir.create("figures", showWarnings = FALSE, recursive = TRUE)

  ggplot2::ggsave(filename = file.path("figures", filename),
                  plot = plot,
                  width = width,
                  height = height,
                  dpi = dpi,
                  units = units,
                  bg = "white")
}

plot_rule_count_comparison <- function(rule_counts,
                                       title = NULL){
  rule_counts %>%
    ggplot(aes(method, n, fill = subset)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_y_log10(labels = scales::label_number()) +
    airi_plot_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(y = "Number of rules (log10 scale)",
         x = "Method",
         fill = "Subset",
         title = title)
}

plot_combined_rule_count_comparison <- function(count_mosj_rules,
                                                cs2_count_emose_rules){
  gridExtra::grid.arrange(
    plot_rule_count_comparison(count_mosj_rules,
                               title = "Case study 1 (MOSJ)"),
    plot_rule_count_comparison(cs2_count_emose_rules,
                               title = "Case study 2 (EMOSE)"),
    ncol = 2
  )
}

plot_dependent_rule_scores <- function(rule_quality){
  rule_quality %>%
    filter(!is.infinite(conviction)) %>%
    pivot_longer(cols = c("mutualInfo", "improvement"),
                 values_to = "Score",
                 names_to = "Metric") %>%
    mutate(Metric = dplyr::recode(Metric,
                                  mutualInfo = "Mutual information",
                                  improvement = "Improvement")) %>%
    ggplot(aes(conviction, confidence, col = lift)) +
    geom_point(size = 1.8, alpha = 0.75) +
    scale_color_gradient(low = reds[1], high = reds[9]) +
    facet_wrap(~Metric) +
    airi_plot_theme() +
    labs(x = "Conviction",
         y = "Confidence",
         col = "Lift")
}

plot_dependent_rule_metric_distribution <- function(rules){
  rules %>%
    DATAFRAME() %>%
    pivot_longer(cols = c("lift", "conviction", "confidence"),
                 names_to = "Metric",
                 values_to = "Score") %>%
    mutate(RHS = str_remove(RHS, "\\{Classification="),
           RHS = str_remove(RHS, "\\}"),
           RHS = factor(RHS, levels = c("Rare", "Undetermined", "Abundant")),
           Metric = dplyr::recode(Metric,
                                  lift = "Lift",
                                  conviction = "Conviction",
                                  confidence = "Confidence")) %>%
    ggplot(aes(RHS, Score)) +
    geom_boxplot(outlier.alpha = 0.35, width = 0.65) +
    facet_wrap(~Metric, scales = "free_y") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
    airi_plot_theme() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
          plot.margin = margin(5.5, 5.5, 12, 5.5)) +
    labs(x = "Consequent",
         y = "Score")
}

plot_airi_selection_metric_distribution <- function(non_redundant_rules){
  non_redundant_rules %>%
    pivot_longer(cols = c("confidence", "lift", "conviction"),
                 names_to = "Rule_metric",
                 values_to = "Score") %>%
    mutate(Rule_metric = dplyr::recode(Rule_metric,
                                       confidence = "Confidence",
                                       lift = "Lift",
                                       conviction = "Conviction")) %>%
    ggplot(aes(Metric, Score)) +
    geom_boxplot(outlier.shape = NA, width = 0.65) +
    geom_jitter(width = 0.12, height = 0, alpha = 0.35, size = 1, col = "grey35") +
    facet_wrap(~Rule_metric, scales = "free_y", nrow = 1) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
    airi_plot_theme() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
          plot.margin = margin(5.5, 5.5, 12, 5.5)) +
    labs(y = "Score")
}
