################################################################################
# Summarize Differences in Subgroups Identified with Biomakers and hclust
################################################################################

# load required libraries
library(dplyr)
library(stringr)

# Cluster the patients based on the identified biomarkers using hclust, complete
# linkage.
# Input:
#   data: A patient x covariates tibble object containing study information
#     and gene expression data.
#   biomarkers_vect: A character vector of gene names. These genes' expression
#     will make up the columns of the heatmap.
#   k: An interger indicating the number of cluster to extract from the hclust
#     object.
# Output:
#   An updated version of the data argument containing a column indicating
#   each patients' cluster assignment, cluster_assignment.
cluster_participants <- function(data, biomarkers_vect, k = 2) {

  # prepare the data
  data_mat <- data %>%
    select(all_of(str_remove_all(biomarkers_vect, "`"))) %>%
    as.matrix
  rownames(data_mat) <- data$USUBJID

  # perform the hierarchical clustering using complete linkage
  hclust_results <- hclust(d = dist(data_mat), method = "complete")

  # extract two clusters
  clusters_assignments <- hclust_results %>% cutree(k = k)

  # add the cluster assignments back to the data
  data$cluster_assignment <- clusters_assignments

  return(data)

}


# Create a table showing differences in response rate across subgroups
cluster_response_table <- function(clustered_data) {

  clustered_data %>%
    mutate(rsp = if_else(rsp == "Response", 1, 0)) %>%
    group_by(cluster_assignment, ARM) %>%
    summarize(
      mean_responders = mean(rsp),
      num_patients = n(),
      .groups = "drop"
    )
}
