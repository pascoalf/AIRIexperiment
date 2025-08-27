# prepare emose data

## load abundance tables curated by Pascoal et al., 2023
load("./data/emose_curated_taxonomy")
## load metadata
load("data/metadata_emose");emose_metadata <- metadata

## Standardize taxonomy columns
# Protists by 18S 
prot_18S <- curated_taxonomy_protist_18S %>% 
  rename(Domain = `Super Kingdom`, 
         Group = FAKE_rank) %>% 
  select(-Species, -total)
# Protists by metagenomics
prot_meta <- curated_taxonomy_protist_metagenome %>% 
  rename(Domain = `Super Kingdom`, 
         Group = FAKE_rank) %>% 
  select(-Species)
# Prokaryotes by 16S 
prok_16S <- curated_taxonomy_prokaryotes_16S %>% 
  rename(Domain = `Super Kingdom`) %>% 
  select(-Species, -FAKE_rank) %>% 
  mutate(Group = NA)
# Prokaryotes by metagenomes 
prok_meta <- curated_taxonomy_prokaryotes_metagenome %>% 
  rename(Domain = `Super Kingdom`) %>% 
  select(-Species, -FAKE_rank) %>% 
  mutate(Group = NA) 

## change all to long format
# quick function to catch sample names
catch_sample <- function(x){
  names(x)[str_detect(names(x), "ERR")]
}
# store all abundance tables in a list
taxa_abundance_list <- list(prot_meta, prok_meta, 
                            prot_18S, prok_16S)

# tidy all abundance tables 
tidy_abundance_list <- map(taxa_abundance_list, 
    .f = ~prepare_tidy_data(.x, sample_names = catch_sample(.x)))

# unlist into a long data.frame
emose_abundance <- tidy_abundance_list %>% 
  bind_rows()

# remove zeros and singletons, and clean emose data frame
emose_abundance_clean <- emose_abundance %>% 
  filter(Abundance > 1) %>% 
  filter(Domain != "NA") %>% 
  mutate(Domain = str_remove(Domain, "sk__"),
         Kingdom = str_remove(Kingdom, "k__"),
         Phylum = str_remove(Phylum, "p__"),
         Class = str_remove(Class, "c__"),
         Order = str_remove(Order, "o__"),
         Family = str_remove(Family, "f__"),
         Genus = str_remove(Genus, "g__")) %>% 
  #mutate(across(where(is.character), as.factor)) %>% 
  select(Domain, Group, Phylum, Class, Order, Family, Genus, Sample, Abundance) %>% ## reorder cols logically
  mutate(Phylum = ifelse(Phylum == "", NA, Phylum),
         Class = ifelse(Class == "", NA, Class),
         Order = ifelse(Order == "", NA, Order),
         Family = ifelse(Family == "", NA, Family),
         Genus = ifelse(Genus == "", NA, Genus),
         Genus = ifelse(Genus == "NA", NA, Genus),
         Group = ifelse(Group == "Others", "Other", Group),
         Group = ifelse(Group == "NA", NA, Group)) %>% 
  filter(!is.na(Phylum)) ## remove NAs at Phylum level

# make taxonomic group
emose_tg <- emose_abundance_clean %>% 
  mutate(taxa = paste(Domain, Group, Phylum, Class, Order, Family, Genus)) %>% 
  select(-Domain, -Group, -Phylum, -Class, -Order, -Family, -Genus)

## join metadata
# clean metadata
metadata_clean <- metadata %>% 
  filter(!is.na(run_accession))

# inspect suspicious samples
suspicious_samples <- metadata_clean$run_accession %>% table() %>% as.data.frame() %>% filter(Freq > 1) %>% pull(".")
metadata_clean %>% filter(run_accession %in% suspicious_samples)
#
metadata_clean <- metadata_clean %>% 
  filter(!is.na(Sequencing_strategy))

# ready to use
emose_tg_clean <- emose_tg %>% 
  left_join(metadata_clean, c("Sample" = "run_accession")) %>% 
  select(-sample_alias, -protocol, -planned_volume, -replica, -Valid_sequences) %>% # remove unnecessary cols
  filter(!is.na(method)) ## remove samples without relevant metadata

# Get abundance classification 
emose_classified <- emose_tg_clean %>% 
  define_rb(samples_col = "Sample")

# make final data frame, ready to use for AIRI
emose_df <- emose_classified %>% 
  select(-Cluster_median_abundance, -median_Silhouette, -Evaluation, -Silhouette_scores, -Level, -pam_object)

#
#save(emose_df, file = "data/emose_df")



