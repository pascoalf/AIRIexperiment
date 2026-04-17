# Jaccard draft 
## jaccard index --> proxy to redundancy between rule sets
bm_items <- names(bank_marketing_cat)[-17]

#
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

# apply to all
jaccard_scores_df <- subscription_all_non_redundant %>% 
  group_by(RHS, Metric) %>% 
  nest() %>% 
  mutate(jaccard = map(.x = data, .f = ~jaccard_all(.x)))

# points
jaccard_scores_df %>%
  unnest(jaccard) %>% 
  ggplot(aes(Metric, jaccard, col = RHS)) + 
  geom_jitter(height = 0, width = 0.1, alpha = 0.25) +
  stat_summary() + 
  labs(title = "Jaccard score for pairwise rules",
       subtitle = "Bank Marketing dataset \npairwise rule comparisons: 2129") + 
  theme_classic()

# violin
jaccard_scores_df %>%
  unnest(jaccard) %>% 
  ggplot(aes(Metric, jaccard, col = RHS)) + 
  #geom_jitter(height = 0, width = 0.1, alpha = 0.5) + 
  geom_violin(aes(fill = RHS)) + 
  stat_summary() + 
  labs(title = "Jaccard score for pairwise rules",
       subtitle = "Bank Marketing dataset \npairwise rule comparisons: 2129") + 
  theme_classic()

# summary
jaccard_scores_df %>%
  mutate(mean_jaccard = map(.x = jaccard, ~mean(.x)),
         sd_jaccard = map(.x = jaccard, ~sd(.x)),
         min_jaccard = map(.x = jaccard, ~min(.x)),
         max_jaccard = map(.x = jaccard, ~max(.x))) %>% 
  unnest(c(mean_jaccard, sd_jaccard, min_jaccard, max_jaccard)) 


## overall (for general compar)
subscription_all_non_redundant %>% 
  group_by(Metric) %>% 
  nest() %>% 
  mutate(jaccard = map(.x = data, .f = ~jaccard_all(.x))) %>% 
  mutate(mean_jaccard = map(.x = jaccard, ~mean(.x)),
         sd_jaccard = map(.x = jaccard, ~sd(.x))) %>% 
  unnest(c(mean_jaccard, sd_jaccard))