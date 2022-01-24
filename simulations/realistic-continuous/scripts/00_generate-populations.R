################################################################################
# 00. Generate Population Datasets
################################################################################

# load the required libraries and functions
library(here)
library(future.apply)
library(Biobase)
library(curatedCRCData)
library(matrixStats)
library(cvCovEst)
library(Matrix)
source(here("realistic-continuous", "R", "data-generating-processes.R"))
source(here("realistic-continuous", "R", "generate-populations.R"))

# set the future plan
plan(multisession, workers = 20)

# set the seed
set.seed(7134513)

# generate the covariance matrix
# load the data, and extract the expression data of the 500 most variable genes
data("GSE14095_eset")
expr <- exprs(GSE14095_eset)
mvr_genes <- expr[which(rank(-rowVars(expr)) <= 500), ]
mvr_genes <- as.data.frame(t(mvr_genes))

# cluster them using hierarchical clustering with complete linkage
gene_clusters <- hclust(dist(t(mvr_genes)))
mvr_genes <- mvr_genes[, gene_clusters$order]

# extract small correlated gene set identified through visual inspection
# and reorder most variable gene expression list
gene_set <- c(
  "IGF2///INS-IGF2", "MAGEA2B///MAGEA2", "MAGEA12", "MAGEA6///MAGEA3"
)
gene_set_expr <- mvr_genes[, gene_set]
mvr_genes <- mvr_genes[, !(colnames(mvr_genes) %in% gene_set)]
mvr_genes <- bind_cols(gene_set_expr, mvr_genes)

# estimate the correlation matrix
cv_cov_est_out <- cvCovEst(
  dat = mvr_genes,
  center = TRUE, scale = TRUE,
  estimators = c(
    bandingEst, taperingEst
  ),
  estimator_params = list(
    bandingEst = list(k = as.integer(seq_len(10))),
    taperingEst = list(k = as.integer(2*seq_len(10)))
  ),
  cv_loss = cvMatrixFrobeniusLoss,
  cv_scheme = "v_fold",
  v_folds = 5,
  parallel = TRUE
)
cor_mat <- cv_cov_est_out$estimate

# make the corre4lation matrix positive definition using the linear
# shrinkage covariance matrix estimator technique
cor_mat <- Matrix::nearPD(
  cor_mat, corr = TRUE, base.matrix = TRUE, ensureSymmetry = TRUE
)$mat
colnames(cor_mat) <- colnames(mvr_genes)
rownames(cor_mat) <- colnames(mvr_genes)

# generate the populations
generate_populations(n = 100000, cov_mat = cor_mat)
