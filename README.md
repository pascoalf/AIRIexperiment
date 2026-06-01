# Source code and data to reproduce paper: "AIRItaxa: Automatic Interesting Rule Mining of Taxa in complex communities"

This repository serves to share the original R code used to produce the results presented in the paper "AIRI: automatic interesting rules identification", which is currently undergoing peer-review.

This repository is actively managged and will be updated until the paper is accepted for publication.

# Source data files description (data/)

- mosj_ASV_df.rds - ASV table of MOSJ dataset 
- mosj_env_data.rds - metadata for ASV table of MOSJ dataset
- mosj_environmental_data_category.rds - discretized metadata values for MOSJ dataset

- emose_df - ASV table with metadata from EMOSE dataset
- emose_curated_taxonomy - taxonomic data on ASVs from EMOSE dataset

# Rule sets obtained (rule-sets/)

**Case study 1:**
- mosj_full_rules_set_as_data_frame.rds - Association rule mining of MOSJ results as data.frame object
- mosj_full_rules_set.rds - Association rule mining os MOSJ results as transactions object 
- mosj_airi_by_complexity.csv - AIRI results by complexity for MOSJ data
- mosj_airi_by_improvement.csv - AIRI results by improvement for MOSJ data
- mosj_airi_by_mutual_information.csv - AIRI results by mutual information for MOSJ data

**Case study 2:**
- emose_rules_df.rds - Association rule mining of EMOSE results as data.frame object
- emose_rules.rds - Association rule mining of EMOSE results as transactions object
- emose_airi_by_complexity.csv - AIRI results by complexity for EMOSE data
- emose_airi_by_improvement.csv - AIRI results by improvement for EMOSE data
- emose_airi_by_mutual_information.csv - AIRI results by mutual information for EMOSE data



# Instructions to reproduce results (R/)

Assuming a dedicated environment with all the files in R/ and data/ directories inside:

1. prepare_data.R - loades R packages needed.

2. Case study 1 - MOSJ dataset

  2.1.  cs1_prepare_data.R - loads MOSJ dataset and pre-processing
  
  2.2.  cs1_prepare_metadata.R - loads contextual environmental data from MOSJ and preprocessing
  
  2.3.  cs1_arm.R - asociation rule mining on MOSJ dataset
  
  2.4.  cs1_airi_steps.R - AIRI implementation on MOSJ dataset

3. Case study 2 - EMOSE dataset

  3.1. cs2_prepare_emose_data.R - load EMOSE dataset and metadata, preprocessing
  
  3.2. cs2_arm_emose.R - association rule mining for EMOSE dataset
  
  3.3. cs2_airi_emose.R - AIRI for EMOSE dataset

4. Comparison: AIRI vs alternatives

  4.1. cs1_compare.R - AIRI vs alternatives for MOSJ dataset
  
  4.2. cs2_compare.R - AIRI vs alternatives for EMOSE dataset
  
  4.3. cs1_syst_compare.R - systematic comparison for all metric values - MOSJ dataset 
  
  4.4. cs2_syst_compare.R - systematic comparison for all metric values - EMOSE dataset

# Citation
## If you use AIRItaxa, please cite: 
Pascoal F., Baptista, S.M., Costa R., Magalhães C., Branco P., AIRItaxa: Automatic Interesting Rule Mining of Taxa in complex communities. Under peer-review.

## Additional citation for source data
If you use the source data available at this repository, please cite the original papers describing and presenting the datasets:

- **MOSJ**: Pascoal, F., Branco, P., Torgo, L. et al. Definition of the microbial rare biosphere through unsupervised machine learning. Commun Biol 8, 544 (2025). https://doi.org/10.1038/s42003-025-07912-4
- **EMOSE**: Pascoal, F., Tomasino, M. P., Piredda, R., Quero, G. M., Torgo, L., Poulain, J., Galand, P. E., Fuhrman, J. A., Mitchell, A., Tinta, T., Turk Dermastia, T., Fernandez-Guerra, A., Vezzi, A., Logares, R., Malfatti, F., Endo, H., Dąbrowska, A. M., De Pascale, F., Sánchez, P., Henry, N., Fosso, B., Wilson, B., Toshchakov, S., Ferrant, G. K., Grigorov, I., Vieira, F. R. J., Costa, R., Pesant, S., Magalhães, C. (2023). Inter-comparison of marine microbiome sampling protocols. ISME Communications, 3(1), 84. https://doi.org/10.1038/s43705-023-00278-w
