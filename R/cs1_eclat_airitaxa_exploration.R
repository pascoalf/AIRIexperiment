# Explore AIRItaxa after Eclat-based ARM for CS1
source("R/prepare_session.R")
source("R/functions/airitaxa_pipeline.R")

if(!exists("env_cat")){
  source("R/cs1_prepare_metadata.R")
}

mine_eclat_classification_rules <- function(data,
                                            rhs_prefix = "Classification=",
                                            support = 0.001,
                                            confidence = 0.8,
                                            minlen = 2,
                                            maxlen = 14,
                                            induction_method = "ptree",
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

set.seed(123)

ASVs_df <- readRDS("data/mosj_ASV_df.rds")

ASVs_cat_df <- ASVs_df %>%
  select(Sample, Classification, taxon) %>%
  mutate(Sample = factor(Sample),
         taxon = factor(taxon)) %>%
  left_join(env_cat) %>%
  ungroup() %>%
  select(-Sample, -Station)

cs1_eclat_rules <- mine_eclat_classification_rules(data = ASVs_cat_df,
                                                   rhs_prefix = "Classification=",
                                                   support = 0.001,
                                                   confidence = 0.8,
                                                   minlen = 2,
                                                   maxlen = 14,
                                                   induction_method = "ptree",
                                                   verbose = FALSE)

cs1_eclat_airitaxa <- run_airitaxa(rules = cs1_eclat_rules,
                                   key_prefix = "taxon=",
                                   dependence_method = "chisq",
                                   p_adjust = "bonferroni",
                                   alpha = 0.05)

cs1_eclat_airitaxa_summary <- summarise_airitaxa_result(airitaxa_result = cs1_eclat_airitaxa,
                                                        support = 0.001,
                                                        case_study = "Case study 1 (MOSJ)") %>%
  mutate(arm_algorithm = "Eclat + ruleInduction",
         confidence = 0.8,
         induction_method = "ptree")

dir.create("rule-sets/eclat_airitaxa", showWarnings = FALSE, recursive = TRUE)

write.csv(DATAFRAME(cs1_eclat_rules),
          "rule-sets/eclat_airitaxa/cs1_eclat_classification_rules.csv",
          row.names = FALSE)

write.csv(cs1_eclat_airitaxa_summary,
          "rule-sets/eclat_airitaxa/cs1_eclat_airitaxa_summary.csv",
          row.names = FALSE)

write.csv(cs1_eclat_airitaxa$by_improvement %>%
            mutate(Metric = "Improvement"),
          "rule-sets/eclat_airitaxa/cs1_eclat_airitaxa_by_improvement.csv",
          row.names = FALSE)

write.csv(cs1_eclat_airitaxa$by_complexity %>%
            mutate(Metric = "Complexity"),
          "rule-sets/eclat_airitaxa/cs1_eclat_airitaxa_by_complexity.csv",
          row.names = FALSE)

write.csv(cs1_eclat_airitaxa$by_mutualInfo %>%
            mutate(Metric = "Mutual information"),
          "rule-sets/eclat_airitaxa/cs1_eclat_airitaxa_by_mutual_information.csv",
          row.names = FALSE)

