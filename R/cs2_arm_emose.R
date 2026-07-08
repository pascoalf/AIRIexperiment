## ARM - EMOSE
source("R/prepare_session.R")

set.seed(123)
# load env and ASVs data
if(!exists("emose_df")){
  if(file.exists("data/emose_df")){
    load("data/emose_df")
  } else {
    source("R/cs2_prepare_emose_data.R")
  }
}

# all features must be categorical 
emose_df_selected <- emose_df %>% ungroup() %>% 
  select(-Abundance) %>% # categorized based on abundance classification
  mutate(volume = as.character(effected_volume)) %>% 
  select(-effected_volume, -Sample) %>% 
  mutate(across(where(is.character), as.factor))

# make transactions database
emose_transactions <- emose_df_selected %>% transactions()

#
summary(emose_transactions)

# get rhs items (set the consequent)
classifications_rhs <- grep("Classification=", 
                            itemLabels(emose_transactions), 
                            value = TRUE)

# mine rules
emose_rules <- apriori(emose_transactions,
                      parameter = list(support = 0.001,minlen = 2, maxlen = 14),
                      appearance = list(rhs = classifications_rhs))

summary(emose_rules)
#
emose_rules_df <- DATAFRAME(emose_rules)


# alternative
emose_rules_df %>% 
  ggplot(aes(support, confidence, col = lift)) + 
  geom_jitter(size = 2) + 
  scale_color_gradient(low = reds[1], high = reds[9]) + 
  # labs(title = paste(length(emose_rules_df[,1]), "rules")) + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.background = element_rect(fill = "grey80")) + 
  labs(x = "Conviction",
       y = "Confidence",
       fill = "Lift: ")

emose_rules %>% summary()




