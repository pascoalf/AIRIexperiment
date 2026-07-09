# Reproduce full AIRItaxa analyses and figures

if(!file.exists("R/prepare_session.R")){
  stop("Run this script from the project root, e.g. source('R/run_full_analyses.R').")
}

source("R/prepare_session.R")
source("R/plot_helper.R")

run_review_sensitivity <- TRUE
run_runtime_benchmark <- TRUE

run_analysis_script <- function(script){
  message("\n--- Running ", script, " ---")
  source(script)
  message("--- Finished ", script, " ---")
}

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("rule-sets", showWarnings = FALSE, recursive = TRUE)

## Case study 1 - MOSJ
run_analysis_script("R/cs1_prepare_metadata.R")
run_analysis_script("R/cs1_arm.R")
run_analysis_script("R/cs1_airi_steps.R")
run_analysis_script("R/cs1_compare.R")
run_analysis_script("R/cs1_syst_compare.R")

## Case study 2 - EMOSE
run_analysis_script("R/cs2_prepare_emose_data.R")
run_analysis_script("R/cs2_arm_emose.R")
run_analysis_script("R/cs2_airi_emose.R")
run_analysis_script("R/cs2_compare.R")
run_analysis_script("R/cs2_syst_compare.R")

## Reviewer sensitivity analyses
if(run_review_sensitivity){
  run_analysis_script("R/support_sensitivity_compare.R")
  run_analysis_script("R/discretization_sensitivity_compare.R")
  run_analysis_script("R/dependence_test_sensitivity_compare.R")
}

## Runtime/RAM benchmark
if(run_runtime_benchmark){
  run_analysis_script("R/runtime_memory_benchmark.R")
}

message("\nFull analysis run complete. Figures were saved to figures/.")
