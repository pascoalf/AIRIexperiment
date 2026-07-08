# Reusable ARM + AIRItaxa helpers

mine_classification_rules <- function(data,
                                      rhs_prefix = "Classification=",
                                      support = 0.001,
                                      minlen = 2,
                                      maxlen = 14,
                                      verbose = FALSE){
  if(support <= 0 || support > 1){
    stop("support must be > 0 and <= 1")
  }

  transactions_data <- data %>% transactions()
  rhs_items <- grep(rhs_prefix, itemLabels(transactions_data), value = TRUE)

  if(length(rhs_items) == 0){
    stop("No RHS items found with prefix: ", rhs_prefix)
  }

  apriori(transactions_data,
          parameter = list(support = support, minlen = minlen, maxlen = maxlen),
          appearance = list(rhs = rhs_items),
          control = list(verbose = verbose))
}

extract_lhs_item <- function(rules, item_prefix){
  lhs_items <- LIST(lhs(rules), decode = TRUE)

  map_chr(lhs_items, function(items){
    matching_items <- items[str_starts(items, item_prefix)]

    if(length(matching_items) == 0){
      return(NA_character_)
    }

    str_remove(matching_items[1], paste0("^", item_prefix))
  })
}

empty_airitaxa_df <- function(){
  data.frame(LHS = character(),
             RHS = character(),
             support = numeric(),
             confidence = numeric(),
             coverage = numeric(),
             lift = numeric(),
             count = integer(),
             chi_p_adj = numeric(),
             conviction = numeric(),
             mutualInfo = numeric(),
             improvement = numeric(),
             key_item = factor(),
             LHSsize = integer())
}

select_non_redundant_rules <- function(rules_df, metric){
  if(nrow(rules_df) == 0){
    return(rules_df)
  }

  rules_df %>%
    filter(!is.na(key_item)) %>%
    group_by(key_item) %>%
    arrange(desc(.data[[metric]])) %>%
    slice_head(n = 1) %>%
    ungroup()
}

run_airitaxa <- function(rules,
                         key_prefix,
                         p_adjust = "bonferroni",
                         alpha = 0.05){
  if(length(rules) == 0){
    rules_df <- empty_airitaxa_df()

    return(list(all_rules = rules,
                all_rules_df = rules_df,
                dependent_rules = rules,
                dependent_rules_df = rules_df,
                by_improvement = rules_df,
                by_mutualInfo = rules_df,
                by_complexity = rules_df))
  }

  rules_df <- DATAFRAME(rules)

  rules_df$chi_p_adj <- rules %>%
    interestMeasure(measure = "chiSquared", significance = TRUE) %>%
    p.adjust(method = p_adjust)

  rules_df$conviction <- rules %>%
    interestMeasure(measure = "Conviction")

  quality(rules)$conviction <- rules_df$conviction
  quality(rules)$chi_p_adj <- rules_df$chi_p_adj

  dependent_rules <- rules[quality(rules)$chi_p_adj < alpha,]

  if(length(dependent_rules) == 0){
    dependent_rules_df <- empty_airitaxa_df()

    return(list(all_rules = rules,
                all_rules_df = rules_df,
                dependent_rules = dependent_rules,
                dependent_rules_df = dependent_rules_df,
                by_improvement = dependent_rules_df,
                by_mutualInfo = dependent_rules_df,
                by_complexity = dependent_rules_df))
  }

  quality(dependent_rules)$mutualInfo <- interestMeasure(dependent_rules,
                                                         measure = "mutualInformation")
  quality(dependent_rules)$improvement <- interestMeasure(dependent_rules,
                                                          measure = "improvement")

  dependent_rules_df <- DATAFRAME(dependent_rules) %>%
    mutate(key_item = factor(extract_lhs_item(dependent_rules, key_prefix)),
           LHSsize = size(lhs(dependent_rules)))

  list(all_rules = rules,
       all_rules_df = rules_df,
       dependent_rules = dependent_rules,
       dependent_rules_df = dependent_rules_df,
       by_improvement = select_non_redundant_rules(dependent_rules_df, "improvement"),
       by_mutualInfo = select_non_redundant_rules(dependent_rules_df, "mutualInfo"),
       by_complexity = select_non_redundant_rules(dependent_rules_df, "LHSsize"))
}

summarise_airitaxa_result <- function(airitaxa_result, support, case_study){
  tibble(case_study = case_study,
         support = support,
         n_all_rules = length(airitaxa_result$all_rules),
         n_dependent_rules = length(airitaxa_result$dependent_rules),
         n_airi_improvement = nrow(airitaxa_result$by_improvement),
         n_airi_mutualInfo = nrow(airitaxa_result$by_mutualInfo),
         n_airi_complexity = nrow(airitaxa_result$by_complexity))
}

run_support_sensitivity <- function(data,
                                    support_values,
                                    key_prefix,
                                    case_study,
                                    rhs_prefix = "Classification=",
                                    minlen = 2,
                                    maxlen = 14,
                                    verbose = FALSE,
                                    p_adjust = "bonferroni",
                                    alpha = 0.05){
  map_dfr(support_values, function(support){
    rules <- mine_classification_rules(data = data,
                                       rhs_prefix = rhs_prefix,
                                       support = support,
                                       minlen = minlen,
                                       maxlen = maxlen,
                                       verbose = verbose)

    airitaxa_result <- run_airitaxa(rules = rules,
                                    key_prefix = key_prefix,
                                    p_adjust = p_adjust,
                                    alpha = alpha)

    summarise_airitaxa_result(airitaxa_result = airitaxa_result,
                              support = support,
                              case_study = case_study)
  })
}
