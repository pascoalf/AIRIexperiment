# Compare metadata discretization methods for case study 1
source("R/prepare_session.R")
source("R/plot_helper.R")
source("R/functions/airitaxa_pipeline.R")

set.seed(123)

support_values <- c(0.0005, 0.001, 0.002, 0.005, 0.01)

discretization_options <- list(
  quartile = list(method = "frequency", breaks = 4),
  tertile = list(method = "frequency", breaks = 3),
  equal_interval_4 = list(method = "interval", breaks = 4),
  kmeans_4 = list(method = "cluster", breaks = 4)
)

discretization_labels <- c(
  quartile = "Equal frequency (4 bins)",
  tertile = "Equal frequency (3 bins)",
  equal_interval_4 = "Equal interval (4 bins)",
  kmeans_4 = "K-means (4 bins)"
)

metadata_discretization_vars <- c("PO4", "NO2", "NH4", "Si", "Chl", "Temperature")
metadata_fixed_vars <- c("Salinity")
zero_as_undetected_vars <- c("PO4", "NO2", "NH4", "Si", "Chl")

make_bin_labels <- function(n_bins){
  if(n_bins == 3){
    return(c("low", "medium", "high"))
  }

  if(n_bins == 4){
    return(c("very low", "low", "high", "very high"))
  }

  paste0("bin ", seq_len(n_bins))
}

discretize_numeric_metadata <- function(x, option, zero_as_undetected = FALSE){
  output <- rep(NA_character_, length(x))
  output[is.na(x)] <- "Unknown"

  values_to_discretize <- !is.na(x)

  if(zero_as_undetected){
    output[x == 0 & !is.na(x)] <- "Undetected"
    values_to_discretize <- values_to_discretize & x != 0
  }

  if(!any(values_to_discretize)){
    return(factor(output))
  }

  n_unique <- length(unique(x[values_to_discretize]))
  n_bins <- min(option$breaks, n_unique)

  if(n_bins < 2){
    output[values_to_discretize] <- "constant"
    return(factor(output))
  }

  output[values_to_discretize] <- as.character(
    discretize(x[values_to_discretize],
               method = option$method,
               breaks = n_bins,
               labels = make_bin_labels(n_bins),
               include.lowest = TRUE,
               ordered_result = FALSE)
  )

  factor(output)
}

make_env_cat_by_discretization <- function(env_data, option){
  env_data %>%
    mutate(Salinity = discretize_numeric_metadata(Salinity,
                                                  option = discretization_options$quartile,
                                                  zero_as_undetected = FALSE)) %>%
    mutate(across(all_of(metadata_discretization_vars),
                  ~discretize_numeric_metadata(.x,
                                                option = option,
                                                zero_as_undetected = cur_column() %in% zero_as_undetected_vars))) %>%
    select(Sample, Station, PelagicLayer, all_of(metadata_discretization_vars), all_of(metadata_fixed_vars), WaterMass) %>%
    mutate(across(!where(is.factor), as.factor))
}

make_cs1_arm_data <- function(env_cat){
  ASVs_df <- readRDS("data/mosj_ASV_df.rds")

  ASVs_df %>%
    select(Sample, Classification, taxon) %>%
    mutate(Sample = factor(Sample),
           taxon = factor(taxon)) %>%
    left_join(env_cat) %>%
    ungroup() %>%
    select(-Sample, -Station)
}

format_airi_rules <- function(rules_df,
                              airi_metric,
                              support_value,
                              discretization_name){
  rules_df %>%
    mutate(discretization = discretization_name,
           discretization_method = discretization_labels[[discretization_name]],
           min_support = support_value,
           airi_metric = airi_metric,
           .before = 1) %>%
    rename(rule_support = support)
}

collect_airi_rules <- function(airitaxa_result,
                               support_value,
                               discretization_name){
  bind_rows(
    format_airi_rules(airitaxa_result$by_improvement,
                      airi_metric = "improvement",
                      support_value = support_value,
                      discretization_name = discretization_name),
    format_airi_rules(airitaxa_result$by_mutualInfo,
                      airi_metric = "mutual_information",
                      support_value = support_value,
                      discretization_name = discretization_name),
    format_airi_rules(airitaxa_result$by_complexity,
                      airi_metric = "complexity",
                      support_value = support_value,
                      discretization_name = discretization_name)
  )
}

run_discretization_sensitivity <- function(option, discretization_name){
  env_cat <- make_env_cat_by_discretization(env_data = env_data,
                                            option = option)

  ASVs_cat_df <- make_cs1_arm_data(env_cat)

  support_runs <- map(support_values, function(support_value){
    rules <- mine_classification_rules(data = ASVs_cat_df,
                                       support = support_value)

    airitaxa_result <- run_airitaxa(rules = rules,
                                    key_prefix = "taxon=")

    list(summary = summarise_airitaxa_result(airitaxa_result = airitaxa_result,
                                             support = support_value,
                                             case_study = discretization_labels[[discretization_name]]) %>%
           mutate(discretization = discretization_name,
                  discretization_method = discretization_labels[[discretization_name]],
                  .before = support),
         airi_rules = collect_airi_rules(airitaxa_result = airitaxa_result,
                                         support_value = support_value,
                                         discretization_name = discretization_name))
  })

  list(summary = map_dfr(support_runs, "summary"),
       airi_rules = map_dfr(support_runs, "airi_rules"))
}

env_data <- readRDS("./data/mosj_env_data.rds")

discretization_runs <- imap(discretization_options,
                            run_discretization_sensitivity)

discretization_sensitivity <- map_dfr(discretization_runs, "summary")

discretization_airi_rules <- map_dfr(discretization_runs, "airi_rules")

dir.create("rule-sets/discretization_sensitivity",
           showWarnings = FALSE,
           recursive = TRUE)

write.csv(discretization_airi_rules,
          "rule-sets/discretization_sensitivity/cs1_airi_rules_by_discretization.csv",
          row.names = FALSE)

discretization_sensitivity_long <- discretization_sensitivity %>%
  select(discretization_method, support, n_all_rules, n_dependent_rules, n_airi_improvement) %>%
  rename(n_airi = n_airi_improvement) %>%
  pivot_longer(cols = starts_with("n_"),
               names_to = "rule_set",
               values_to = "n_rules") %>%
  mutate(rule_set = dplyr::recode(rule_set,
                                  n_all_rules = "All rules",
                                  n_dependent_rules = "Dependent rules",
                                  n_airi = "AIRI"))

discretization_sensitivity_long %>%
  ggplot(aes(support, n_rules, col = rule_set)) +
  geom_point() +
  geom_line() +
  geom_text(data = filter(discretization_sensitivity_long, rule_set == "AIRI"),
            aes(label = n_rules),
            vjust = -0.7,
            show.legend = FALSE) +
  facet_wrap(~discretization_method, scales = "free_y") +
  scale_color_manual(values = airi_rule_set_colors) +
  scale_x_log10(labels = scales::label_percent(accuracy = 0.01)) +
  airi_pseudo_log_y() +
  airi_plot_theme() +
  labs(x = "Minimum support (%)",
       y = "Number of rules",
       col = "Rule set")
