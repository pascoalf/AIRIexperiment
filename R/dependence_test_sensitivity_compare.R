# Compare dependence tests for ARM + AIRItaxa in both case studies
source("R/prepare_session.R")
source("R/plot_helper.R")
source("R/functions/airitaxa_pipeline.R")

support_values <- c(0.0005, 0.001, 0.002, 0.005, 0.01)
dependence_methods <- c("chisq", "fisher")

dependence_labels <- c(
  chisq = "Chi-squared",
  fisher = "Fisher exact"
)

## Case study 1 - MOSJ
source("R/cs1_prepare_metadata.R")

ASVs_df <- readRDS("data/mosj_ASV_df.rds")

ASVs_cat_df <- ASVs_df %>%
  select(Sample, Classification, taxon) %>%
  mutate(Sample = factor(Sample),
         taxon = factor(taxon)) %>%
  left_join(env_cat) %>%
  ungroup() %>%
  select(-Sample, -Station)

## Case study 2 - EMOSE
if(!exists("emose_df")){
  if(file.exists("data/emose_df")){
    load("data/emose_df")
  } else {
    source("R/cs2_prepare_emose_data.R")
  }
}

emose_df_selected <- emose_df %>%
  ungroup() %>%
  select(-Abundance) %>%
  mutate(volume = as.character(effected_volume)) %>%
  select(-effected_volume, -Sample) %>%
  mutate(across(where(is.character), as.factor))

run_dependence_test_sensitivity <- function(data,
                                            key_prefix,
                                            case_study,
                                            dependence_method){
  dependence_test_label <- dependence_labels[[dependence_method]]

  run_support_sensitivity(data = data,
                          support_values = support_values,
                          key_prefix = key_prefix,
                          case_study = case_study,
                          dependence_method = dependence_method) %>%
    mutate(dependence_method = dependence_method,
           dependence_test = dependence_test_label,
           .before = support)
}

dependence_test_sensitivity <- bind_rows(
  map_dfr(dependence_methods,
          ~run_dependence_test_sensitivity(data = ASVs_cat_df,
                                           key_prefix = "taxon=",
                                           case_study = "Case study 1 (MOSJ)",
                                           dependence_method = .x)),
  map_dfr(dependence_methods,
          ~run_dependence_test_sensitivity(data = emose_df_selected,
                                           key_prefix = "taxa=",
                                           case_study = "Case study 2 (EMOSE)",
                                           dependence_method = .x))
)

dependence_test_sensitivity_long <- dependence_test_sensitivity %>%
  select(case_study, dependence_test, support, n_all_rules, n_dependent_rules, n_airi_improvement) %>%
  rename(n_airi = n_airi_improvement) %>%
  pivot_longer(cols = starts_with("n_"),
               names_to = "rule_set",
               values_to = "n_rules") %>%
  mutate(rule_set = dplyr::recode(rule_set,
                                  n_all_rules = "All rules",
                                  n_dependent_rules = "Dependent rules",
                                  n_airi = "AIRI"))

dependence_test_sensitivity_plot <- dependence_test_sensitivity_long %>%
  ggplot(aes(support, n_rules, col = rule_set, linetype = dependence_test)) +
  geom_point(aes(shape = dependence_test)) +
  geom_line() +
  geom_text(data = filter(dependence_test_sensitivity_long, rule_set == "AIRI"),
            aes(label = n_rules),
            position = position_dodge(width = 0.12),
            vjust = -0.7,
            show.legend = FALSE, col = "black") +
  facet_wrap(~case_study) +
  scale_color_manual(values = airi_rule_set_colors) +
  scale_x_log10(labels = scales::label_percent(accuracy = 0.01)) +
  airi_pseudo_log_y() +
  airi_plot_theme() +
  labs(x = "Minimum support (%)",
       y = "Number of rules",
       col = "Rule set",
       linetype = "Dependence test",
       shape = "Dependence test")

save_airi_plot(dependence_test_sensitivity_plot,
               filename = "dependence_test_sensitivity.png",
               width = 7.5,
               height = 4.8)

dependence_test_sensitivity_plot
