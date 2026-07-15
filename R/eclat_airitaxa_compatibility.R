# AIRItaxa compatibility check with Eclat-derived rules
source("R/prepare_session.R")
source("R/functions/airitaxa_pipeline.R")

compatibility_support <- 0.001
compatibility_confidence <- 0.8
compatibility_minlen <- 2
compatibility_maxlen <- 14
compatibility_induction_method <- "ptree"

mine_eclat_classification_rules <- function(data,
                                            rhs_prefix = "Classification=",
                                            support = compatibility_support,
                                            confidence = compatibility_confidence,
                                            minlen = compatibility_minlen,
                                            maxlen = compatibility_maxlen,
                                            induction_method = compatibility_induction_method,
                                            verbose = FALSE){
  if(support <= 0 || support > 1){
    stop("support must be > 0 and <= 1")
  }

  if(confidence <= 0 || confidence > 1){
    stop("confidence must be > 0 and <= 1")
  }

  transactions_data <- data %>% transactions()
  rhs_items <- grep(rhs_prefix, itemLabels(transactions_data), value = TRUE)

  if(length(rhs_items) == 0){
    stop("No RHS items found with prefix: ", rhs_prefix)
  }

  frequent_itemsets <- eclat(transactions_data,
                             parameter = list(support = support,
                                              minlen = minlen,
                                              maxlen = maxlen),
                             control = list(verbose = verbose))

  induced_rules <- ruleInduction(frequent_itemsets,
                                 transactions = transactions_data,
                                 confidence = confidence,
                                 method = induction_method,
                                 verbose = verbose)

  rhs_labels <- LIST(rhs(induced_rules), decode = TRUE)
  lhs_labels <- LIST(lhs(induced_rules), decode = TRUE)

  keep_classification_rules <- map2_lgl(rhs_labels, lhs_labels, function(rhs_items_rule, lhs_items_rule){
    length(rhs_items_rule) == 1 &&
      str_starts(rhs_items_rule[1], rhs_prefix) &&
      !any(str_starts(lhs_items_rule, rhs_prefix))
  })

  induced_rules[keep_classification_rules,]
}

prepare_cs1_arm_data <- function(){
  if(!exists("env_cat")){
    source("R/cs1_prepare_metadata.R")
  }

  ASVs_df <- readRDS("data/mosj_ASV_df.rds")

  ASVs_df %>%
    select(Sample, Classification, taxon) %>%
    mutate(Sample = factor(Sample),
           taxon = factor(taxon)) %>%
    left_join(env_cat) %>%
    ungroup() %>%
    select(-Sample, -Station)
}

prepare_cs2_arm_data <- function(){
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

load_or_mine_apriori_rules <- function(data,
                                       rules_path,
                                       rhs_prefix = "Classification=",
                                       support = compatibility_support,
                                       minlen = compatibility_minlen,
                                       maxlen = compatibility_maxlen){
  if(file.exists(rules_path)){
    return(readRDS(rules_path))
  }

  mine_classification_rules(data = data,
                            rhs_prefix = rhs_prefix,
                            support = support,
                            minlen = minlen,
                            maxlen = maxlen,
                            verbose = FALSE)
}

run_arm_airitaxa_compatibility <- function(data,
                                           case_study,
                                           key_prefix,
                                           apriori_rules_path,
                                           output_prefix){
  apriori_rules <- load_or_mine_apriori_rules(data = data,
                                              rules_path = apriori_rules_path)

  apriori_airitaxa <- run_airitaxa(rules = apriori_rules,
                                   key_prefix = key_prefix,
                                   dependence_method = "chisq",
                                   p_adjust = "bonferroni",
                                   alpha = 0.05)

  eclat_rules <- mine_eclat_classification_rules(data = data,
                                                 rhs_prefix = "Classification=",
                                                 support = compatibility_support,
                                                 confidence = compatibility_confidence,
                                                 minlen = compatibility_minlen,
                                                 maxlen = compatibility_maxlen,
                                                 induction_method = compatibility_induction_method,
                                                 verbose = FALSE)

  eclat_airitaxa <- run_airitaxa(rules = eclat_rules,
                                 key_prefix = key_prefix,
                                 dependence_method = "chisq",
                                 p_adjust = "bonferroni",
                                 alpha = 0.05)

  dir.create("rule-sets/eclat_airitaxa", showWarnings = FALSE, recursive = TRUE)

  write.csv(DATAFRAME(eclat_rules),
            file.path("rule-sets/eclat_airitaxa",
                      paste0(output_prefix, "_eclat_classification_rules.csv")),
            row.names = FALSE)

  write.csv(eclat_airitaxa$by_improvement %>%
              mutate(Metric = "Improvement"),
            file.path("rule-sets/eclat_airitaxa",
                      paste0(output_prefix, "_eclat_airitaxa_by_improvement.csv")),
            row.names = FALSE)

  write.csv(eclat_airitaxa$by_complexity %>%
              mutate(Metric = "Complexity"),
            file.path("rule-sets/eclat_airitaxa",
                      paste0(output_prefix, "_eclat_airitaxa_by_complexity.csv")),
            row.names = FALSE)

  write.csv(eclat_airitaxa$by_mutualInfo %>%
              mutate(Metric = "Mutual information"),
            file.path("rule-sets/eclat_airitaxa",
                      paste0(output_prefix, "_eclat_airitaxa_by_mutual_information.csv")),
            row.names = FALSE)

  bind_rows(
    summarise_airitaxa_result(airitaxa_result = apriori_airitaxa,
                              support = compatibility_support,
                              case_study = case_study) %>%
      mutate(arm_algorithm = "Apriori",
             confidence = compatibility_confidence,
             induction_method = NA_character_),
    summarise_airitaxa_result(airitaxa_result = eclat_airitaxa,
                              support = compatibility_support,
                              case_study = case_study) %>%
      mutate(arm_algorithm = "Eclat + ruleInduction",
             confidence = compatibility_confidence,
             induction_method = compatibility_induction_method)
  ) %>%
    select(case_study,
           arm_algorithm,
           support,
           confidence,
           induction_method,
           n_all_rules,
           n_dependent_rules,
           n_airi_improvement,
           n_airi_complexity,
           n_airi_mutualInfo)
}

set.seed(123)

cs1_arm_data <- prepare_cs1_arm_data()
cs2_arm_data <- prepare_cs2_arm_data()

airitaxa_arm_compatibility_summary <- bind_rows(
  run_arm_airitaxa_compatibility(data = cs1_arm_data,
                                 case_study = "Case study 1 (MOSJ)",
                                 key_prefix = "taxon=",
                                 apriori_rules_path = "rule-sets/mosj_full_rules_set.rds",
                                 output_prefix = "cs1"),
  run_arm_airitaxa_compatibility(data = cs2_arm_data,
                                 case_study = "Case study 2 (EMOSE)",
                                 key_prefix = "taxa=",
                                 apriori_rules_path = "rule-sets/emose_rules.rds",
                                 output_prefix = "cs2")
)

write.csv(airitaxa_arm_compatibility_summary,
          "rule-sets/eclat_airitaxa/airitaxa_arm_compatibility_summary.csv",
          row.names = FALSE)
