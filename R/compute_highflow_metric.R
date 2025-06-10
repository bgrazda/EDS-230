#' highflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @param high_flow_months which to use default (May = 5)
#' @return annual_max_err, annual_max_corr, high_month_cor, high_month_err

compute_highflowmetrics <- function(m, o, month, day, year, wy, high_flow_months = 5) {
  flow <- cbind.data.frame(m, o, month, day, year, wy)
  # first lets get max yearly values
  
  tmp <- flow %>%
    group_by(wy) %>%
    summarize(maxo = max(o), maxm = max(m))
  
  # Fix variable names for consistency
  annual_max_err <- mean((tmp$maxm - tmp$maxo) / tmp$maxo) # percent error instead of raw error
  annual_max_cor <- cor(tmp$maxm, tmp$maxo, use = "complete.obs")
  
  
  # now lets get monthly values
  tmp <- flow %>%
    group_by(month, year) %>%
    summarize(model = sum(m), obs = sum(o))
  # now extract may
  high <- subset(tmp, month %in% high_flow_months)
  high_month_err <- mean((high$model - high$obs) / high$obs) # Percent error
  high_month_cor <- cor(high$model, high$obs, use = "complete.obs")
  
  return(list(
    annual_max_err = annual_max_err, annual_max_cor = annual_max_cor, high_month_err = high_month_err,
    high_month_cor = high_month_cor
  ))
}
