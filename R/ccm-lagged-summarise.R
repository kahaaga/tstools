#' Summarise a lagged CCM result.
#'
#' @details CCM predictability for a given lag is only considered if the analysis is
#'    convergent. Convergence is determined by Wilcoxon rank-sum test on the smallest and largest library sizes. If CCM
#'    skill for the largest library sizes is higher (p < 0.01) than for the
#'    lowest library sizes, then the analysis is potentially convergent.
#'    Additionally, the median CCM curve over increasing library sizes must be
#'    satisfactorily fitted by a convergent exponential function. If both these
#'    criteria are met, then the analysis is labelled convergent. The only grouping variable in this summary function is the lag, so all
#'    further groupings must be handled outside this function.
#' @param res A data frame containing the result of a tstools::ccm_lagged() call.
#' @param library.size The library size to summarise over. Defaults to the
#'     largest available library size.
#' @return The difference between summed negative median CCM skills and
#'     summed positive median CCM skills. If the sum > 0, then the analysis
#'     passes the lag test.
#' @examples
#' # res is the result of a tstools::ccm_lagged() call
#' library(dplyr)
#' res %>% directionalcausaltest
#'
#' # res is a data frame containing vertically concatenated results of
#' # tstools::ccm_lagged() calls for different causal variables.
#' res %>%
#'   group_by(causal.direction) %>%
#'   do(directionalcausaltest(.))
#'
#' @importFrom magrittr "%>%"
#' @export
directionalcausaltest <- function(res, library.size = max(res$library.size)) {
  res = res[res$library.size == library.size, ]
  results = list()

  # A little trickery to deal with analyses that are not convergent
  # for a specific lag. Returns a row of NA/Inf statistics, but with
  # the same column names as usual if that is the case.
  for (lag in unique(res$lag)) {
    results_this_lag = res[res$lag == lag &
                             res$convergent == 1 &
                             res$p.value < 0.01 &
                             res$k > 0, ]

    # If there are no convergent analyses, run dummy summary statistics on
    # NA that yields a row of NAs of Inf/-Infs for the statistics.
    if (nrow(results_this_lag) == 0) {
      statsummary = tstools::summary_stats(c(NA), as.df = T)
    } else {# Do regular stats.
      statsummary = tstools::summary_stats(results_this_lag$rho, as.df = T)
    }
    # Add information about lag again.
    results[[toString(lag)]] = cbind(data.frame(lag = lag), statsummary)
  }
  # Combine results for all the lags.
  res = results %>% dplyr::bind_rows()

  # Separate negative and positive lags.
  negatives = res %>% filter(lag < 0) %>% dplyr::select(median) %>% sum
  positives = res %>% filter(lag > 0) %>% dplyr::select(median) %>% sum

  # Take the difference between causal (negative lags) and non-causal (positive
  # lags) skills.
  if (is.na(negatives) & is.na(positives)) {
    causaldifference = NA
  } else if (is.finite(negatives) & is.finite(positives)) {
    causaldifference = negatives - positives
  } else if (is.na(negatives) & is.finite(positives)) {
    causaldifference = positives
  } else if (is.finite(negatives) & is.na(positives)) {
    causaldifference = negatives
  }

  # Add the causal difference sum to the data as repeated values in a column.
  res$causaldifference = rep(causaldifference)
  return(res)
}
