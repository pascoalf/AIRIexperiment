# Association rule mining
# association rule mining 
set.seed(123)
# load env and ASVs data

# merge relevant data
ASVs_cat_df <- ASVs_df %>% 
  select(Sample, Classification, taxon) %>% 
  mutate(Sample = factor(Sample),
         taxon = factor(taxon)) %>% 
  left_join(env_cat) %>% ## env_cat includes the discretized numerical variables (made in "env_data_discretization.R")
  ungroup() %>% 
  select(-Sample, -Station) # discuss this (features that are redundant)

# make transactions
transactions_full <- ASVs_cat_df %>% transactions()

#
summary(transactions_full)

# get rhs items (set the consequent)
classifications_rhs <- grep("Classification=", 
                            itemLabels(transactions_full), 
                            value = TRUE)

# mine rules
full_rules <- apriori(transactions_full,
                      parameter = list(support = 0.001,minlen = 2, maxlen = 14),
                      appearance = list(rhs = classifications_rhs))

#plot(full_rules)
reds <- RColorBrewer::brewer.pal(9, "Reds")

#
full_rules_df <- DATAFRAME(full_rules)

#
full_rules_df %>% 
  ggplot(aes(support, confidence, fill = lift)) + 
  geom_jitter(shape = 21, col = "grey", size = 2) + 
  scale_fill_gradient(low = reds[1], high = reds[9]) + 
  # labs(title = paste(length(full_rules_df[,1]), "rules")) + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) + 
  labs(x = "Conviction",
       y = "Confidence",
       fill = "Lift: ")

# alternative
full_rules_df %>% 
  ggplot(aes(support, confidence, col = lift)) + 
  geom_jitter(size = 2) + 
  scale_color_gradient(low = reds[1], high = reds[9]) + 
  # labs(title = paste(length(full_rules_df[,1]), "rules")) + 
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

full_rules %>% summary()


# check top10 by lift
top100_rules_by_lift <- full_rules_df %>% 
  arrange(desc(lift)) %>% 
  head(100) 



