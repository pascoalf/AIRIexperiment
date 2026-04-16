## load diabetes dataset
diabetes <- read.csv("data/diabetes.csv")

# bmi calc
bmi_class <- function(bmi){
  if(bmi < 16){
    return("Severe thiness")
  } else if(bmi < 17){
    return("Moderate thiness")
  } else if(bmi < 18.5){
    return("Mild thiness")
  } else if(bmi < 25){
    return("Normal")
  } else if(bmi <30){
    return("Overweight")
  } else if(bmi < 35){
    return("Obese Class I")
  } else if(bmi < 40){
    return("Obese Class II")
  } else if(bmi >= 40){
    return("Obese Class III")
  } else {
    return(NA)
  }
}
general_health <- function(GenHlth){
  if(GenHlth == 1){
    return("Excellent")
  } else if(GenHlth == 2){
    return("Very good")
  } else if(GenHlth == 3){
    return("Good")
  } else if(GenHlth == 4){
    return("Fair")
  } else if(GenHlth == 5){
    return("Poor")
  } else {
    return(NA)
  }
}
education_code <- function(Education){
  if(Education == 1){
    return("Never attended school or only kindergarden")
  } else if(Education == 2){
    return("Elementary school")
  } else if(Education == 3){
    return("Some high school")
  } else if(Education == 4){
    return("Finished high school")
  } else if(Education == 5){
    return("Some college or technical school")
  } else if(Education ==6){
    return("4 college years or graduate")
  } else {
    return(NA)
  }
}

# to informative categories
diabetes_cat <- diabetes %>% 
  mutate(Diabetes_012 = ifelse(Diabetes_012 == 0, "No diabetes", "Diabetes"),
         HighBP = ifelse(HighBP == 0, "no high BP", "high BP"),
         HighChol = ifelse(HighChol == 0, "no high cholesterol", "high colesterol"),
         CholCheck = ifelse(CholCheck == 0, "no in 5 years", "yes in 5 years"),
         Smoker = ifelse(Smoker == 0, "Not frequent smoker", "Frequent smoker"),
         Stroke = ifelse(Stroke == 0, "Never", "Yes"),
         HeartDiseaseorAttack = ifelse(HeartDiseaseorAttack == 0, "Never", "Yes"),
         PhysActivity = ifelse(PhysActivity == 0, "Not in last month", "Yes in last month"),
         Fruits = ifelse(Fruits == 0, "Not daily", "Daily"),
         Veggies = ifelse(Veggies == 0, "Not daily", "Daily"),
         HvyAlcoholConsump = ifelse(HvyAlcoholConsump == 0, "No", "Yes"),
         AnyHealthcare = ifelse(AnyHealthcare == 0, "No", "Yes"),
         NoDocbcCost = ifelse(NoDocbcCost == 0, "Not in last year", "Yes in last year"),
         MentHlth = paste(MentHlth, " bad mental health days"),
         PhysHlth = paste(PhysHlth, " bad physical health days"),
         DiffWalk = ifelse(DiffWalk == 0, "No", "Yes"),
         Sex = ifelse(Sex == 0, "Female", "Male"),
         Age = as.factor(Age),
         Income = case_when(Income == 1 ~ "lowest",
                            Income == 2 ~ "vvlow",
                            Income == 3 ~ "vlow",
                            Income == 4 ~ "average",
                            Income == 5 ~ "high",
                            Income == 6 ~ "vhigh",
                            Income == 7 ~ "vvhigh",
                            Income == 8 ~ "highest"))
## apply functions 
diabetes_cat$BMI <- sapply(diabetes_cat$BMI, bmi_class)
diabetes_cat$GenHlth <- sapply(diabetes_cat$GenHlth, general_health)
diabetes_cat$Education <- sapply(diabetes_cat$Education, education_code)

## Create transactions 
diabetes_transactions <- transactions(diabetes_cat)

##
diabetes_rhs <- grep("Diabetes_012=",
                        itemLabels(diabetes_transactions), 
                        value = TRUE)
## make rule set
diabetes_rules <- apriori(diabetes_transactions,
                           parameter = list(support = 0.01,minlen = 3, maxlen = 20),
                           appearance = list(rhs = diabetes_rhs),maxtime = 200)
#
diabetes_rules %>% DATAFRAME() %>% View()

diabetes_rules %>% DATAFRAME() %>% filter(str_detect(RHS, "Diabetes_012=Diabetes"))


