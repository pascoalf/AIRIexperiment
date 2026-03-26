# Source code and data to reproduce paper: "AIRI: automatic interesting rules identification"

This repository serves to share the original R code used to produce the results presented in the paper "AIRI: automatic interesting rules identification", which is currently undergoing peer-review.

This repository is actively managged and will be updated until the paper is accepted for publication.

# Instructions to reproduce results

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
## Source code citation
If you use this code for your own research, please cite our paper:
1. Pascoal F., Costa R., Magalhães C., Baptista S.M., Branco P., AIRI: automatic interesting rules identification. Under peer-review.

## Additional citation for source data
If you use the source data available at this repository, please cite the original papers describing and presenting the datasets:

- **MOSJ**: Pascoal, F., Branco, P., Torgo, L. et al. Definition of the microbial rare biosphere through unsupervised machine learning. Commun Biol 8, 544 (2025). https://doi.org/10.1038/s42003-025-07912-4
- **EMOSE**: Francisco Pascoal, Maria Paola Tomasino, Roberta Piredda, Grazia Marina Quero, Luís Torgo, Julie Poulain, Pierre E Galand, Jed A Fuhrman, Alex Mitchell, Tinkara Tinta, Timotej Turk Dermastia, Antonio Fernandez-Guerra, Alessandro Vezzi, Ramiro Logares, Francesca Malfatti, Hisashi Endo, Anna Maria Dąbrowska, Fabio De Pascale, Pablo Sánchez, Nicolas Henry, Bruno Fosso, Bryan Wilson, Stephan Toshchakov, Gregory Kevin Ferrant, Ivo Grigorov, Fabio Rocha Jimenez Vieira, Rodrigo Costa, Stéphane Pesant, Catarina Magalhães, Inter-comparison of marine microbiome sampling protocols, ISME Communications, Volume 3, Issue 1, December 2023, 84, https://doi.org/10.1038/s43705-023-00278-w
