#' @title directionalcausaltest
#' @description Summarise a lagged CCM result.
#' @details
#'    CCM predictability for a given lag is only considered if the analysis is
#'    convergent.
#'
#'    Convergence is determined by Wilcoxon rank-sum test on the smallest and largest library sizes. If CCM
#'    skill for the largest library sizes is higher (p < 0.01) than for the
#'    lowest library sizes, then the analysis is potentially convergent.
#'    Additionally, the median CCM curve over increasing library sizes must be
#'    satisfactorily fitted by a convergent exponential function. If both these
#'    criteria are met, then the analysis is labelled convergent.
#'
#'    The only grouping variable in this summary function is the lag, so all
#'    further groupings must be handled outside this function.
#' @param dt A data frame containing the result of a tstools::ccm_lagged() call.
#' @return The difference between summed negative median CCM skills and
#'     summed positive median CCM skills. If the sum > 0, then the analysis
#'     passes the lag test.
#' @examples
#' # res is the result of a tstools::ccm_lagged() call
#'library(dplyr)
#'res %>% directionalcausaltest
#'
#'# res is a data frame containing vertically concatenated results of
#'# tstools::ccm_lagged() calls for different causal variables.
#'res %>%
#'  group_by(causal.direction) %>%
#'  do(directionalcausaltest(.))
directionalcausaltest <- function(res) {

  dt = res %>%
    dplyr::group_by(lag) %>%
    dplyr::filter(convergent == 1, p.value < 0.01, k > 0) %>%
    dplyr::do(as.data.frame(as.list(summary_stats(.$rho))))

  negatives = dt %>% filter(lag < 0)
  positives = dt %>% filter(lag > 0)

  # If the following sum > 0, then the analysis is causal.
  if (nrow(negatives) == 0 & nrow(positives) == 0) {
    causaldifference = NA
  } else if (nrow(negatives) > 0 & nrow(positives) > 0) {
    causaldifference = sum(negatives$median) - sum(positives$median)
  } else if (nrow(negatives) > 0 & nrow(positives) == 0) {
    causaldifference = sum(negatives$median)
  } else if (nrow(negatives) == 0 & nrow(positives) > 0) {
    causaldifference = sum(positives$median)
  }

  # Add the causal difference sum to the data as repeated values in a column.
  dt$causaldifference = rep(causaldifference)

  return(dt)
}
