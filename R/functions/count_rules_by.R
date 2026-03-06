# count rules
count_rules_by <- function(rules, metric = "confidence", score = "0.9"){
  tmp <- get_rules_by(rules, metric = metric, score = score)
  #
  if(is_empty(tmp)){
    nrules <- 0
  } else{
    tmp <- DATAFRAME(tmp)
    nrules <- dim(tmp)[1]
  }
  return(nrules)
}
