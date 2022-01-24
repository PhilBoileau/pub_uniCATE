################################################################################
# Compare Gene Expression of Selected Biomarkers
################################################################################

# Input:
#   data: A patient x covariates tibble object containing study information
#     and gene expression data.
#   biomarker_vect: A character vector of gene names. These genes' expression
#     will make up the columns of the heatmap.
#   title: A character defining the title of the heatmap.
#   filename: A character defining the name of the resulting file.
#   path: A character determining the path towards the destination of the
#     resulting file.
#   scale_row: A logical indicating whether to scale each patients' biomarkers.
# Output:
#   A heatmap plot saved in the directory defined by the path argument under the
#   name defined by the filename argument.
plot_heatmap <- function(
  data, biomarker_vect, title, filename = NULL, path = NULL, scale_row = FALSE
) {

  # create the full data.frame
  heatmap_df <- data %>%
    dplyr::select(ARM, rsp, all_of(str_remove_all(biomarker_vect, "`"))) %>%
    as.data.frame

  rownames(heatmap_df) <- 1:nrow(heatmap_df)

  # extract the annotation data
  row_anno_df <- heatmap_df %>%
    dplyr::select(ARM, rsp)
  colnames(row_anno_df) <- c("Arm", "Response")

  # remove anno data from heatmap df and coerce to matrix
  heatmap_mat <- heatmap_df %>%
    dplyr::select(-ARM, -rsp) %>%
    as.matrix

  # plot the heatmap
  if (scale_row)
    scale_row <- "row"
  else
    scale_row <- "none"

  if (!is.null(filename) & !is.null(path)) {

    pheatmap(
      mat = heatmap_mat,
      scale = scale_row,
      clustering_method = "complete",
      annotation_row = row_anno_df,
      show_rownames = FALSE,
      annotation_legend = FALSE,
      main = title,
      filename = here(path, filename),
      width = 8,
      height = 12
    )

  } else {
    pheatmap(
      mat = heatmap_mat,
      scale = scale_row,
      clustering_method = "complete",
      show_rownames = FALSE,
      show_colnames = FALSE,
      annotation_legend = FALSE,
      main = title,
      width = 8,
      height = 12,
      breaks = seq(from = 0, to = 11, by = 11/100),
      legend_breaks = seq(from = 0, to = 11, by = 2),
      legend_labels = seq(from = 0, to = 11, by = 2)
    )
  }
}
