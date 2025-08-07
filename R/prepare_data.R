
# Load data from Pascoal et al., 2025
ASVs_env <- read.csv("data/AO_ASVs_env_clean.csv", row.names = 1)

# Order Station levels by longitude
Stations_by_long <- ASVs_env %>% 
  select(Station, Longitude, Latitude) %>% 
  distinct() %>% 
  arrange(desc(Longitude)) %>% 
  pull(Station)

# correct format
ASVs_env_clean <- 
  ASVs_env %>% 
  as_tibble() %>% 
  mutate(PelagicLayer = case_when(Depth < 200 ~ "Epipelagic",
                                  Depth < 1000 ~ "Mesopelagic",
                                  Depth >=1000 ~ "Bathypelagic")) %>% # re-define pelagic layers, because depth was corrected
  mutate(Kingdom = factor(Kingdom),
         Phylum = factor(Phylum),
         Class = factor(Class),
         Order = factor(Order),
         Family = factor(Family),
         Genus = factor(Genus),
         Station = factor(Station, levels = Stations_by_long),
         PelagicLayer = factor(PelagicLayer, levels = c("Epipelagic", "Mesopelagic", "Bathypelagic")),
         WaterMass = factor(WaterMAss))

# Group ASVs at genus level
taxa_table <- ASVs_env_clean %>% 
  filter(!is.na(Kingdom)) %>% 
  mutate(taxon = 
           str_remove_all(
             paste(Kingdom, Phylum, Class, Order, Family, Genus, sep = "_"), c("NA"))) %>%
  mutate(taxon = str_remove(taxon, "__")) %>% 
  group_by(Sample, taxon) %>% 
  summarise(Abundance = sum(Abundance)) %>% 
  filter(Abundance > 0) # remove zeros

# get just env data in a separate table
env_data <- ASVs_env_clean %>% 
  select(Sample, year, Station, Depth, 
         PelagicLayer, Longitude, 
         Latitude,PO4, NO2, NO3, 
         NH4, Si, Chl, Temperature, 
         Salinity, WaterMass) %>% 
  mutate(WaterMass = case_when(is.na(WaterMass) ~ "Unknown",
                               WaterMass == "[]" ~ "Unknown",
                               TRUE ~ WaterMass)) %>% 
  mutate(
    across(
      where(is.double), ~round(.x, digits = 3))) %>% 
  distinct()

saveRDS(env_data, "data/env_data.rds")

# join collapsed taxa table with env data
full_table <- taxa_table %>% left_join(env_data)


# check taxa that appear in only one sample
taxa_singles <- full_table %>% 
  group_by(taxon) %>% 
  count() %>% 
  summarise(category = ifelse(n == 1, "single", "good")) 

# by grouping, only 28% appear in a single sample
taxa_singles %>% 
  pull(category) %>% 
  table()

# verify relation with abundance
full_table %>% 
  left_join(taxa_singles) %>% 
  mutate(category = factor(category, levels = c("single", "good"))) %>% 
  group_by(Sample) %>% 
  mutate(RelativeAbundance = Abundance * 100/sum(Abundance)) %>%
  mutate(isSingleton = ifelse(Abundance == 1, "yes", "no")) %>% 
  ggplot(aes(Sample, RelativeAbundance, col = isSingleton)) + 
  geom_point() + 
  scale_y_log10() +
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank())+ 
  facet_grid(~category) +
  geom_hline(yintercept = 0.01, col = "red") +
  labs(y = "Relative abundance (%)",
       title = "note that this table includes singletons") +
  scale_color_manual(values = c("grey", "red"))

# pre-processing steps:
# remove taxa with fewer than 0.01% reads, keeping singles above 0.01% reads
# remove samples with less than 10000 total reads
# remove NAs at Phylum level

#
total_reads_df <- full_table %>% 
  group_by(Sample, year) %>% 
  summarise(Total_reads = sum(Abundance))

# sample with less than 10 000 reads (to be removed)
samples_to_remove <- total_reads_df %>% filter(Total_reads <10000) %>% pull(Sample)

ASVs_df <- full_table %>% 
  group_by(Sample) %>% 
  mutate(RelativeAbundance = Abundance * 100/sum(Abundance)) %>% 
  filter(RelativeAbundance > 0.01,
         !Sample %in% samples_to_remove,
         !taxon %in% c("Bacteria___", "Archaea___"),
         !str_detect(taxon, "Chloroplast"),
         !str_detect(taxon, "Eukaryota___"))


## define rare, undetermined and abundant taxa
ASVs_df <- ASVs_df %>% define_rb() ## the clustering result is now better

ASVs_df %>% saveRDS("./data/ASV_clean_full_df.rds")
