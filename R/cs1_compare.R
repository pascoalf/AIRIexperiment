# CS1 compare alternatives

## function to get rules by approach
get_rules_by <- function(rules, by = NULL, metric = "confidence", score = NULL){
  if(metric == "confidence"){
    temp_rules <- rules[quality(rules)$confidence >= score]    
  } else if(metric == "lift"){
    temp_rules <- rules[quality(rules)$lift >= score]
  } else if(metric == "conviction"){
    temp_rules <- rules[quality(rules)$conviction >= score]
  } else {stop("select valid metric")}
  quality(temp_rules)$mutualInfo <- interestMeasure(temp_rules, measure = "mutualInformation")
  quality(temp_rules)$improvement <- interestMeasure(temp_rules, measure = "improvement")
  return(temp_rules)
}

# Flat threshold approach
conf.90_all  <- get_rules_by(full_rules, metric = "confidence", score = 0.9)
conf.90_dep <- get_rules_by(full_rules_dependent, metric = "confidence", score = 0.9)
conf.100_all <- get_rules_by(full_rules, metric = "confidence", score = 1)
conf.100_dep <- get_rules_by(full_rules_dependent, metric = "confidence", score = 1)
lift.1_all <- get_rules_by(full_rules, metric = "lift", score = 1)
lift.1_dep <- get_rules_by(full_rules_dependent, metric = "lift", score = 1)

# After threshold, if they are a lot, arrange by some other metric

## Summarise 





## could we also add a network???



