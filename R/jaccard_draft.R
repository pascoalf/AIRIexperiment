## drafts - jaccard
# jaccard = A int B / A U B
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}
#

#jaccard_rules <- function(rule_a, rule_b){

all_items_nr_comp <- diagnosis_rules_non_redundant_complex %>% 
  mutate(item_sets = str_extract_all(LHS, paste(all_items, collapse='|'))) %>% 
  select(LHS, RHS, item_sets) %>% 
  pull(item_sets)

a <- all_items_nr_comp[[1]]
b <- all_items_nr_comp[[2]]

jaccard(a,b)

ruleJaccard <- function(ruleset, p1, p2){
  #
  rule_items <- ruleset %>% 
    mutate(item_sets = str_extract_all(LHS, paste(all_items, collapse='|'))) %>% 
    select(LHS, RHS, item_sets) %>% 
    pull(item_sets)
  #
  a <- rule_items[[p1]]
  b <- rule_items[[p2]]
  #
  jaccard_ab <- jaccard(a,b)
  #
  return(jaccard_ab)
}

#
ruleJaccard(diagnosis_rules_non_redundant_improv, 
            p1 = 1, p2 = 2)

#
comparisons <- expand.grid(1:9, 1:9)
#
comparisons <- data.frame(t(apply(comparisons, 1, sort))) %>% 
  distinct()
#
comparisons <- comparisons %>% mutate(check = X1 - X2) %>% filter(check != 0) %>% select(-check)

#
jaccard_complex <- map2(.x = comparisons$X1,
                        .y = comparisons$X2,
                        .f = ~ruleJaccard(diagnosis_rules_non_redundant_complex,
                                          p1 = .x,
                                          p2 = .y)) %>% 
  unlist()