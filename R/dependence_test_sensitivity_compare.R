# Compare dependence tests for ARM + AIRItaxa in both case studies
source("R/prepare_session.R")
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

dependence_test_sensitivity_long %>%
  ggplot(aes(support, n_rules, col = rule_set, linetype = dependence_test)) +
  geom_point(aes(shape = dependence_test)) +
  geom_line() +
  geom_text(data = filter(dependence_test_sensitivity_long, rule_set == "AIRI"),
            aes(label = n_rules),
            position = position_dodge(width = 0.12),
            vjust = -0.7,
            show.legend = FALSE, col = "black") +
  facet_wrap(~case_study, scales = "free_y") +
  scale_x_log10() +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                     breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000),
                     labels = scales::label_number()) +
  theme_classic() +
  theme(legend.position = "top",
        panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3)) +
  labs(x = "Minimum support",
       y = "Number of rules",
       col = "Rule set",
       linetype = "Dependence test",
       shape = "Dependence test")
