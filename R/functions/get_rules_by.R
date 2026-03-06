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