################################################################################
# 00. Prepare the data
################################################################################

# In this script, the IMmotion150 anf IMmotion151 studies data' are prepared for
# downstream analyses.

# laod the required libraries
library(here)
library(dplyr)
library(tidyr)
library(matrixStats)

# load the CIT data
cit_tbl <- readRDS(here("data/cit_processed.rds"))

# IMmotion150 Preparation ######################################################

# extract the IMmotion150 data
imm150_tbl <- cit_tbl %>% filter(STUDYID == "WO29074")

# extract the RNA-seq data
start_rnaseq_vars <- which(colnames(imm150_tbl) == "AVALU_PFS") + 1
imm150_rnaseq_tbl <- imm150_tbl %>%
  select(start_rnaseq_vars:(length(imm150_tbl) - 2))

# extract non-biomarker covariates
imm150_tbl <- imm150_tbl %>%
  select(
    USUBJID, AGE, SEX, RACE, ETHNIC, ARMCD, ARM, COUNTRY, rsp, AVAL_OS,
    AVALU_OS, AVAL_PFS, AVALU_PFS
  )

# recombine the imm150 study data
imm150_tbl <- bind_cols(imm150_tbl, imm150_rnaseq_tbl)

# remove all rows with missing data
imm150_tbl <- imm150_tbl %>% drop_na() 

# only consider the "MPDL3280A AND BEVACIZUMAB" and "SUNITINIB" arms
imm150_tbl <- imm150_tbl %>% filter(ARM != "MPDL3280A ALONE")

# extract a small random sample: use it to identify the most informative genes
set.seed(723423)
imm150_var_subset <- imm150_tbl %>% sample_n(20)
imm150_gene_vars <- imm150_var_subset %>%
  select(
    -c(USUBJID, AGE, SEX, RACE, ETHNIC, ARMCD, ARM, COUNTRY, rsp, AVAL_OS,
       AVALU_OS, AVAL_PFS, AVALU_PFS)
  ) %>%
  log1p %>%
  as.matrix %>%
  colVars
names(imm150_gene_vars) <- colnames(imm150_rnaseq_tbl)
imm150_inf_genes <- names(which(rank(-imm150_gene_vars) <= 500))

# subset and save the remaining data for analysis
imm150_cov_tbl <- imm150_tbl %>%
  filter(!(USUBJID %in% imm150_var_subset$USUBJID)) %>%
  select(
    USUBJID, AGE, SEX, RACE, ETHNIC, ARMCD, ARM, COUNTRY, rsp, AVAL_OS,
    AVALU_OS, AVAL_PFS, AVALU_PFS
  )
imm150_rnaseq_tbl <-imm150_tbl %>%
  filter(!(USUBJID %in% imm150_var_subset$USUBJID)) %>%
  select(all_of(imm150_inf_genes)) %>%
  log1p
imm150_tbl <- bind_cols(imm150_cov_tbl, imm150_rnaseq_tbl)
saveRDS(
  imm150_tbl,
  file = "data/processed-IMmotion150.Rds"
)


# IMmotion151 Preparation ######################################################

# extract the IMmotion151 data
imm151_tbl <- cit_tbl %>% filter(STUDYID == "WO29637")

# extract the RNA-seq data
imm151_rnaseq_tbl <- imm151_tbl %>%
  select(all_of(imm150_inf_genes)) %>%
  log1p

# extract non-biomarker covariates
imm151_tbl <- imm151_tbl %>%
  select(
    USUBJID, AGE, SEX, RACE, ETHNIC, ARMCD, ARM, COUNTRY, rsp, AVAL_OS,
    AVALU_OS, AVAL_PFS, AVALU_PFS
  )

# recombine the imm150 study data, and remove missing data 
imm151_tbl <- bind_cols(imm151_tbl, imm151_rnaseq_tbl) %>% drop_na()

# save the data for further analysis
saveRDS(
  imm151_tbl,
  file = "data/processed-IMmotion151.Rds"
)
