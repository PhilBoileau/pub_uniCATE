###############################################################################
# Functions for Computing Treatment Scores
################################################################################

# Create a table showing differences in response rate across subgroups
group_response_table <- function(data) {

  data %>%
    mutate(rsp = if_else(rsp == "Response", 1, 0)) %>%
    group_by(group_assignment, ARM) %>%
    summarize(
      mean_responders = mean(rsp),
      num_patients = n(),
      .groups = "drop"
    )

}
