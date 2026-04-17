jaccard_all <- function(ruleset){
  # jaccard formula for sets a and b
  jaccard <- function(a, b){
    intersection = length(intersect(a, b))
    union = length(a) + length(b) - intersection
    return(intersection/union)
  }
  # to get possible comparisons between rules
  set_comparisons <- function(ruleset = ruleset){
    #  
    comparisons <- expand.grid(seq_along(ruleset$LHS), seq_along(ruleset$LHS))
    #
    comparisons <- data.frame(t(apply(comparisons, 1, sort))) %>% 
      distinct()
    #
    comparisons <- comparisons %>% 
      mutate(check = X1 - X2) %>% 
      filter(check != 0) %>% 
      select(-check)
    #
    return(comparisons)
  }
  #
  comparisons <- set_comparisons(ruleset = ruleset)
  
  # to calculate jaccard between two rules
  ruleJaccard <- function(ruleset, p1, p2){
    #
    rule_items <- ruleset %>% 
      mutate(item_sets = str_extract_all(LHS, paste(bm_items, collapse='|'))) %>% 
      pull(item_sets)
    #
    a <- rule_items[[p1]]
    b <- rule_items[[p2]]
    #
    jaccard_ab <- jaccard(a,b)
    #
    return(jaccard_ab)
  }
  
  # obtain ruleJaccard for all possible two rules
  jaccard_for_all <- map2(.x = comparisons$X1,
                          .y = comparisons$X2,
                          .f = ~ruleJaccard(ruleset, 
                                            p1 = .x, 
                                            p2 = .y)) %>% 
    unlist()
  
  #
  return(jaccard_for_all)
}