# Compare support values for ARM + AIRItaxa in both case studies
source("R/prepare_session.R")
source("R/functions/airitaxa_pipeline.R")

support_values <- c(0.0005, 0.001, 0.002, 0.005, 0.01)

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

cs1_support_sensitivity <- run_support_sensitivity(data = ASVs_cat_df,
                                                   support_values = support_values,
                                                   key_prefix = "taxon=",
                                                   case_study = "CS1")

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

cs2_support_sensitivity <- run_support_sensitivity(data = emose_df_selected,
                                                   support_values = support_values,
                                                   key_prefix = "taxa=",
                                                   case_study = "CS2")

support_sensitivity <- bind_rows(cs1_support_sensitivity,
                                 cs2_support_sensitivity)

support_sensitivity_long <- support_sensitivity %>%
  pivot_longer(cols = starts_with("n_"),
               names_to = "rule_set",
               values_to = "n_rules") %>%
  mutate(rule_set = dplyr::recode(rule_set,
                                  n_all_rules = "All rules",
                                  n_dependent_rules = "Dependent rules",
                                  n_airi_improvement = "AIRI by improvement",
                                  n_airi_mutualInfo = "AIRI by mutual information",
                                  n_airi_complexity = "AIRI by complexity"))

support_sensitivity_long %>%
  ggplot(aes(support, n_rules, col = rule_set)) +
  geom_point() +
  geom_line() +
  facet_wrap(~case_study, scales = "free_y") +
  scale_x_log10() +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10)) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(x = "Minimum support",
       y = "Number of rules",
       col = "Rule set")
