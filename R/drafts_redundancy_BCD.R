## drafts -- redundancy 
## redundancy
all_items <- names(breast_cancer_cat)[-31]

# count total items
diagnosis_rules_non_redundant_improv %>% 
  mutate(item_count = str_count(LHS,  paste(all_items, collapse='|'))) %>% 
  select(LHS, item_count)

## redundancy = redundant rules / total rules


item_count_vector <- map(all_items, 
                         .f = ~dim(
                           filter(
                             diagnosis_rules_non_redundant_mutualInfo, 
                             str_detect(LHS, .x)))[1]) %>% 
  unlist()

summary_redund_items <- data.frame(item = all_items, 
                                   item_count = item_count_vector) %>% 
  filter(item_count > 0) %>% 
  mutate(redundant = ifelse(item_count == 1, "no", "yes")) %>% 
  group_by(redundant) %>% 
  summarise(item_count = sum(item_count))
#
(redundnacy_BCD <- summary_redund_items[2,2]*100/sum(summary_redund_items[,2]))

##