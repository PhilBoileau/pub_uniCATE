################################################################################
# 02c. Gene Set Enrichment Analysis of uniCATE Biomarkers
################################################################################

# load the required libraries and functions
library(here)
library(dplyr)
library(readr)
library(xtable)

# load the biomarker data
unicate_results <- readRDS(here("results/uniCATE-results.Rds"))
unicate_biomarkers <- unicate_results %>%
  filter(p_value_bh <= 0.05) %>%
  select(biomarker)

# print out the biomarkers
write_csv(
  unicate_biomarkers,
  file = here("results/uniCATE-biomarkers.csv")
)

# Run the analysis on MSigDB with GO:BP using the webtool

# format the results file in latex
msigdb_res <- read_tsv(here("results/top-go-terms.tsv")) %>%
  xtable(
    label = "tab:GOBP",
    caption = "Gene set enrichment analysis of GO biological processes terms for uniCATE's selected predictive biomarkers using IMmotion 150 data."
  )

