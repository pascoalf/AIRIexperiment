redundancy <- function(rules){
  if(is_empty(rules)){
    return(NA)
  }
  if(!is.data.frame(rules)){
    rules <- DATAFRAME(rules)
  }
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