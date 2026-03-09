# plot helper script 
grid.arrange(
count_mosj_rules %>% 
  ggplot(aes(method, n , fill = subset)) + 
  geom_col(position = "dodge") + 
  scale_y_log10() + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.text.x = element_text(size = 12, angle = 45, vjust = 0.7),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size =12)) +
  labs(y = "Number of rules \n(Log10 scale)",
       x = "Method",
       fill = "Subset",
       title = "Case study 1"),

cs2.count_emose_rules %>% 
  ggplot(aes(method, n , fill = subset)) + 
  geom_col(position = "dodge") + 
  scale_y_log10() + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.text.x = element_text(size = 12, angle = 45, vjust = 0.7),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size =12)) +
  labs(y = "Number of rules \n(Log10 scale)",
       x = "Method",
       fill = "Subset",
       title = "Case study 2"))

## repeat for redundancy metric
grid.arrange(
redund_df %>% 
  mutate(redundancy = 100*redundancy) %>% 
  ggplot(aes(method, redundancy, fill = subset)) +
  geom_col(position = "dodge") + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.text.x = element_text(size = 12, angle = 45, vjust = 0.7),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size =12)) +
  labs(y = "Redundancy (%)",
       x = "Method",
       fill = "Subset",
       title = "Case study 1"),
redund_df_cs2 %>% 
  mutate(redundancy = 100*redundancy) %>% 
  ggplot(aes(method, redundancy, fill = subset)) +
  geom_col(position = "dodge") + 
  theme_classic() + 
  theme(legend.position = "top",
        axis.text.x = element_text(size = 12, angle = 40, vjust = 0.7),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size =12)) +
  labs(y = "Redundancy (%)",
       x = "Method",
       fill = "Subset",
       title = "Case study 2")
)

