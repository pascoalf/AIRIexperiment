#removed redundancty metric

redundancy <- function(rules){
  # count redundant rules
  # key item set is taxon
  redundant_rules <- rules %>% 
    separate_wider_delim(LHS, ",", names = "taxon", too_many = "debug") %>% 
    select(-LHS_ok, -LHS_pieces) %>% 
    mutate(taxon = str_remove(taxon, "\\{taxon="),
           taxon = str_remove(taxon, "\\}"),
           taxon = str_remove(taxon, ","),
           taxon = factor(taxon)) %>% 
    group_by(taxon) %>% 
    count() %>% 
    filter(n > 1)
  
  redundancy <- (sum(redundant_rules$n)/dim(rules)[1])
  #
  return(redundancy)
}

# Calc redundancy for all
redund_df <- multi_options_cs1 %>% 
  group_by(method, subset) %>% 
  nest() %>% 
  mutate(redundancy = map(.x = data, .f = ~redundancy(.x))) %>% 
  unnest(redundancy)

redund_df %>% 
  mutate(redundancy = 100*redundancy) %>% 
  ggplot(aes(method, redundancy, fill = subset)) +
  geom_col(position = "dodge") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(y = "Redundancy (%)", 
       x = "Method",
       fill = "Subset")
#

