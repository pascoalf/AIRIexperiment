# Runtime and RAM benchmark for ARM + AIRItaxa
source("R/prepare_session.R")
source("R/functions/airitaxa_pipeline.R")

set.seed(123)

support_value <- 0.001
sample_fractions <- c(0.25, 0.50, 0.75, 1.00)
n_repeats <- 5

prepare_cs1_data <- function(){
  source("R/cs1_prepare_metadata.R")

  ASVs_df <- readRDS("data/mosj_ASV_df.rds")

  ASVs_df %>%
    select(Sample, Classification, taxon) %>%
    mutate(Sample = factor(Sample),
           taxon = factor(taxon)) %>%
    left_join(env_cat) %>%
    ungroup() %>%
    select(-Sample, -Station)
}

prepare_cs2_data <- function(){
  if(!exists("emose_df")){
    if(file.exists("data/emose_df")){
      load("data/emose_df")
    } else {
      source("R/cs2_prepare_emose_data.R")
    }
  }

  emose_df %>%
    ungroup() %>%
    select(-Abundance) %>%
    mutate(volume = as.character(effected_volume)) %>%
    select(-effected_volume, -Sample) %>%
    mutate(across(where(is.character), as.factor))
}

sample_benchmark_data <- function(data, sample_fraction){
  if(sample_fraction == 1){
    return(data)
  }

  data %>%
    slice_sample(prop = sample_fraction)
}

run_airitaxa_benchmark_core <- function(data,
                                        case_study,
                                        key_prefix,
                                        sample_fraction,
                                        repeat_id,
                                        support = support_value){
  sampled_data <- sample_benchmark_data(data = data,
                                        sample_fraction = sample_fraction)

  sampled_transactions <- sampled_data %>% transactions()

  rules <- mine_classification_rules(data = sampled_data,
                                     support = support)

  airitaxa_result <- run_airitaxa(rules = rules,
                                  key_prefix = key_prefix)

  tibble(case_study = case_study,
         sample_fraction = sample_fraction,
         repeat_id = repeat_id,
         n_rows = nrow(sampled_data),
         n_items = length(itemLabels(sampled_transactions)),
         support = support,
         n_all_rules = length(airitaxa_result$all_rules),
         n_dependent_rules = length(airitaxa_result$dependent_rules),
         n_airi_rules = nrow(airitaxa_result$by_improvement))
}

benchmark_airitaxa_run <- function(data,
                                   case_study,
                                   key_prefix,
                                   sample_fraction,
                                   repeat_id,
                                   support = support_value){
  benchmark_result <- NULL

  gc()

  memory_profile <- peakRAM({
    benchmark_result <- run_airitaxa_benchmark_core(data = data,
                                                    case_study = case_study,
                                                    key_prefix = key_prefix,
                                                    sample_fraction = sample_fraction,
                                                    repeat_id = repeat_id,
                                                    support = support)
  })

  benchmark_result %>%
    mutate(runtime_seconds = memory_profile$Elapsed_Time_sec,
           total_ram_used_mib = memory_profile$Total_RAM_Used_MiB,
           peak_ram_used_mib = memory_profile$Peak_RAM_Used_MiB,
           .after = support)
}

run_case_study_benchmark <- function(data, case_study, key_prefix){
  benchmark_grid <- expand_grid(sample_fraction = sample_fractions,
                                repeat_id = seq_len(n_repeats))

  pmap_dfr(benchmark_grid,
           function(sample_fraction, repeat_id){
             benchmark_airitaxa_run(data = data,
                                    case_study = case_study,
                                    key_prefix = key_prefix,
                                    sample_fraction = sample_fraction,
                                    repeat_id = repeat_id)
           })
}

cs1_benchmark_data <- prepare_cs1_data()
cs2_benchmark_data <- prepare_cs2_data()

airitaxa_runtime_memory <- bind_rows(
  run_case_study_benchmark(data = cs1_benchmark_data,
                           case_study = "Case study 1 (MOSJ)",
                           key_prefix = "taxon="),
  run_case_study_benchmark(data = cs2_benchmark_data,
                           case_study = "Case study 2 (EMOSE)",
                           key_prefix = "taxa=")
)

dir.create("rule-sets/benchmark",
           showWarnings = FALSE,
           recursive = TRUE)

write.csv(airitaxa_runtime_memory,
          "rule-sets/benchmark/airitaxa_runtime_memory.csv",
          row.names = FALSE)

airitaxa_runtime_memory_long <- airitaxa_runtime_memory %>%
  select(case_study, sample_fraction, repeat_id,
         runtime_seconds, peak_ram_used_mib) %>%
  pivot_longer(cols = c(runtime_seconds, peak_ram_used_mib),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = dplyr::recode(metric,
                                runtime_seconds = "Runtime (seconds)",
                                peak_ram_used_mib = "Peak RAM used (MiB)"))

airitaxa_runtime_memory_long %>% 
  filter(metric %in% c("Runtime (seconds)","Peak RAM used (MiB)")) %>%
  ggplot(aes(sample_fraction, value, 
             col = case_study, linetype = case_study)) +
  geom_point(alpha = 0.55) +
  stat_summary(fun = median, geom = "line", linewidth = 0.6) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(legend.position = "top",
        panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3)) +
  labs(x = "Sample fraction",
       y = "Value",
       col = "Case study",
       linetype = "Case study")
