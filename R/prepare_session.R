# Prepare session
# Packages
library(dplyr)
library(arules)
library(arulesViz)
library(ggplot2)
library(ulrb)
library(stringr)
library(tidyr)
library(ggpubr)
library(tidyr)
library(ggVennDiagram)
# vector colors
qualitative_colors <- 
  c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
reds <- RColorBrewer::brewer.pal(9, "Reds")