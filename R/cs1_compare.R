# CS1 compare alternatives

## REDO --> use transactions object ---> on top of it calculate mutual info and improvement


# Just Confidence 90%
conf.90_all <- full_rules[quality(full_rules)$confidence >= 0.9]
quality(conf.90_all)$mutualInfo <- interestMeasure(conf.90_all, measure = "mutualInformation")
quality(conf.90_all)$improvement <- interestMeasure(conf.90_all, measure = "improvement")

conf.90_dep <- full_rules_dependent[quality(full_rules_dependent)$confidence >= 0.9]
quality(conf.90_dep)$mutualInfo <- interestMeasure(conf.90_dep, measure = "mutualInformation")
quality(conf.90_dep)$improvement <- interestMeasure(conf.90_dep, measure = "improvement")


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
test <- get_rules_by(full_rules_dependent, metric = "lift", score = 10)

## how to implement this for arranged rules??


# Just Confidence 100%
conf.100_all <- full_rules_df %>% filter(confidence == 1) %>% mutate(Strategy = "Conf.100%", Set = "All")
conf.100_dep <- full_rules_df_dependent %>% filter(confidence == 1) %>% mutate(Strategy = "Conf.100%", Set = "Dependent")%>% 
  select(!Dependence)

# Just Lift > 1
lift.1_all <- full_rules_df %>% filter(lift > 1) %>% mutate(Strategy = "Lift.1", Set = "All")
lift.1_dep <- full_rules_df_dependent %>% filter(lift > 1) %>% mutate(Strategy = "Lift.1", Set = "Dependent")%>% 
  select(!Dependence)

# Top 100 by Conf
top100_conf_all <- full_rules_df %>% arrange(desc(confidence)) %>% head(100) %>% mutate(Strategy = "top100.Conf", Set = "All")
top100_conf_dep <- full_rules_df_dependent %>% arrange(desc(confidence)) %>% head(100) %>% mutate(Strategy = "top100.Conf", Set = "Dependent")%>% 
  select(!Dependence)

# Top 10 by Conf
top10_conf_all <- full_rules_df %>% arrange(desc(confidence)) %>% head(10) %>% mutate(Strategy = "top10.Conf", Set = "All")
top10_conf_dep <- full_rules_df_dependent %>% arrange(desc(confidence)) %>% head(10) %>% mutate(Strategy = "top10.Conf", Set = "Dependent")%>% 
  select(!Dependence)

# Top 100 by Conv
top100_conv_all <- full_rules_df %>% arrange(desc(Conviction)) %>% head(100) %>% mutate(Strategy = "top100.Conv", Set = "All")
top100_conv_dep <- full_rules_df_dependent %>% arrange(desc(Conviction)) %>% head(100) %>% mutate(Strategy = "top100.Conv", Set = "Dependent")%>% 
  select(!Dependence)

# Top 10 by Conv
top10_conv_all <- full_rules_df %>% arrange(desc(Conviction)) %>% head(10) %>% mutate(Strategy = "top10.Conv", Set = "All")
top10_conv_dep <- full_rules_df_dependent %>% arrange(desc(Conviction)) %>% head(10) %>% mutate(Strategy = "top10.Conv", Set = "Dependent")%>% 
  select(!Dependence)

# Top 100 by Lift
top100_lift_all <- full_rules_df %>% arrange(desc(lift)) %>% head(100) %>% mutate(Strategy = "top100.Lift", Set = "All")
top100_lift_dep <- full_rules_df_dependent %>% arrange(desc(lift)) %>% head(100) %>% mutate(Strategy = "top100.Lift", Set = "Dependent")%>% 
  select(!Dependence)

# Top 10 by Lift
top10_lift_all <- full_rules_df %>% arrange(desc(lift)) %>% head(10) %>% mutate(Strategy = "top10.Lift", Set = "All")
top10_lift_dep <- full_rules_df_dependent %>% 
  arrange(desc(lift)) %>% 
  head(10) %>% 
  mutate(Strategy = "top10.Lift", Set = "Dependent") %>% 
  select(!Dependence)

##
all_options_df <-
conf.90_all %>% 
  rbind(conf.90_dep) %>% 
  rbind(conf.100_all) %>% 
  rbind(conf.100_dep) %>% 
  rbind(lift.1_all) %>% 
  rbind(lift.1_dep) %>% 
  rbind(top100_conf_all) %>% 
  rbind(top100_conf_dep) %>% 
  rbind(top10_conf_all) %>% 
  rbind(top10_conf_dep) %>% 
  rbind(top100_conv_all) %>% 
  rbind(top100_conv_dep) %>% 
  rbind(top10_conv_all) %>% 
  rbind(top10_conv_dep) %>% 
  rbind(top100_lift_all) %>% 
  rbind(top100_lift_dep) %>% 
  rbind(top10_lift_all) %>% 
  rbind(top10_lift_dep)

## Summarise 





## could we also add a network???



