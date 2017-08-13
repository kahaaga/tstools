
average_causal_ccm <- function(ccm.result, print.to.console = F, return.vector = F) {
  negative.lags = ccm.result %>% subset(lag <= 0)
  ccm.result[, "CCMmedian.average.for.causal.lags"] = rep(mean(negative.lags$CCM.median))

  if (return.vector) {
    return(ccm.result$CCMmedian.average.for.causal.lags)
  }

  return(ccm.result)
}

max_causal_ccm <- function(ccm.result, print.to.console = F, return.vector = F) {
  negative.lags = ccm.result %>% subset(lag <= 0)
  ccm.result[, "CCMmedian.max.for.causal.lags"] = rep(max(negative.lags$CCM.median))

  if (return.vector) {
    return(ccm.result$CCMmedian.max.for.causal.lags)
  }

  return(ccm.result)
}

causal_sum_without_surrogates <- function(ccm.result, print.to.console = F) {
  n_lags = length(unique(ccm.result$lag))
  negative.lags = ccm.result[ccm.result$lag <= 0, ]
  positive.lags = ccm.result[ccm.result$lag > 0, ]
  neg = sum(negative.lags$CCM.median[!is.na(negative.lags$CCM.median)] / n_lags)
  pos = sum(positive.lags$CCM.median[!is.na(positive.lags$CCM.median)] / n_lags)

  # Causal sum is ill-defined in some cases, so take care of NAs.
  if (is.finite(neg) & !is.finite(pos)) {
    causal_sum = neg - pos
  } else if(!is.finite(neg) & !is.finite(pos)){
    causal_sum = NA
  } else if (!is.finite(neg)) {
    causal_sum = - pos
  } else if (neg < pos) {
    causal_sum = neg - pos
  } else {
    causal_sum = neg - pos
  }

  ccm.result[, "causal.sum.nosurr"] = rep(causal_sum)
  return(ccm.result)
}

CausalityCheck <- function(ccm.result, significance.level = 0.9545, print.to.console = F) {

  # Some string manipulation to get the columns and printing right.
  siglevel = str_replace(toString(significance.level), "0.", "")
  significance.column = paste("significant.at.p", siglevel, sep="")

  if (significance.level == 0.6827 & print.to.console) {
    cat("\nCausality test at the 68.27% confidence level (~1 sigma assuming normality)")
  } else if (significance.level == 0.9545 & print.to.console) {
    cat("\nCausality test at the 95.45% confidence level (~2 sigma assuming normality)")
  } else if (significance.level == 0.9973 & print.to.console) {
    cat("\nCausality test at the 99.73% confidence level (~3 sigma assuming normality)\n")
  }

  #ccm.result = CheckSignificance(ccm.result, significance.level)
  causal.sum.colname = paste("causal.sum.p", siglevel, sep="")

  # If there are significant rows, we normalise them against the original number of lags.
  n_lags = length(unique(ccm.result$lag))

  # ------------------------------------------------------
  # Nonsignificant rows:  add NAs as the causal sum column
  # ------------------------------------------------------
  # When all rows are nonsignificant, adjust the original data frame
  n.significant.rows = nrow(ccm.result[!is.na(ccm.result[, c(significance.column)]), ])
  n.rows = nrow(ccm.result)

  if (n.significant.rows == 0) {
    ccm.result[, causal.sum.colname] = rep(NA)
    return(ccm.result)

  } else if (n.significant.rows < n.rows) {# If there are some nonsignificant rows {
    # If some rows are significant and some are not, split the cases
    # and combine afterwards.
    nonsignificant.rows = ccm.result[is.na(ccm.result[, c(significance.column)]), ]
    nonsignificant.rows[, causal.sum.colname] = rep(NA)
  }

  ############################################################
  # When all rows are significant, do the usual thing
  ############################################################

  # -------------------------------------------------
  # Significant rows:     calculate the causal sum
  # -------------------------------------------------
  significant.rows = ccm.result[!is.na(ccm.result[, c(significance.column)]), ]

  # Filter rows for negative and positive lags, and normalise separately
  negative.lags = significant.rows[significant.rows$lag <= 0]
  positive.lags = significant.rows[significant.rows$lag > 0]
  neg = sum(negative.lags$CCM.median[!is.na(negative.lags$CCM.median)] / n_lags)
  pos = sum(positive.lags$CCM.median[!is.na(positive.lags$CCM.median)] / n_lags)

  # Causal sum is ill-defined in some cases, so take care of NAs.
  if (is.finite(neg) & !is.finite(pos)) causal_sum = neg - pos
  else if(!is.finite(neg) & !is.finite(pos)) causal_sum = NA
  else if (!is.finite(neg)) causal_sum = - pos
  else if (neg < pos) causal_sum = neg - pos
  else causal_sum = neg - pos

  significant.rows[, causal.sum.colname] = rep(causal_sum)

  # -------------------------------------------------
  # Combine results and sort them according to lag
  # -------------------------------------------------
  if (n.significant.rows == n.rows) return(significant.rows)
  else combined = arrange(rbind(significant.rows, nonsignificant.rows), lag)
  return(combined)
}
