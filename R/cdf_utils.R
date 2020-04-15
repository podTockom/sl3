#' Generate quantiles estimates from the estimated CDF
#'
#' @param task A \code{\link{sl3_Task}} where the outcome is cdf estimate.
#' @param hazard_fit A \code{\link{sl3}} fit for the cdf.
#' @param qProb A list of quantiles
#'
#'
#' @export
#
estQuantile <- function(task, hazard_fit, qProb = list(0.05, 0.95)) {
  rootFun <- function(preds, q) {
    return(abs(preds - q))
  }

  # Get the outcome and predictions
  Y_node <- task$nodes$outcome
  data <- data.frame(task$data)

  preds <- hazard_fit$predict(task)
  Y <- data[, Y_node]

  qY <- lapply(qProb, function(q) {
    Y[which.min(rootFun(preds, q))]
  })

  return(qY)
}
