#' Quantile Regression Forests
#'
#' Quantile Regression Forests infer conditional quantile functions from data
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom popbio mean.list
#'
#' @export
#'
#' @keywords data
#'
#' @return Learner object with methods for training and prediction. See
#'  \code{\link{Lrnr_base}} for documentation on learners.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @family Learners
#'
#' @section Parameters:
#' \describe{
#'   \item{\code{nthreads}}{The number of threads to use (for parallel computation inside the algorithm).
#'   }
#'   \item{\code{keep.inbag}}{Keep information which observations are in and out-of-bag?
#'   For out-of-bag predictions, this argument needs to be set to TRUE.
#'   }
#' }
#
Lrnr_quantregForest <- R6Class(
  classname = "Lrnr_quantregForest", inherit = Lrnr_base,
  portable = TRUE, class = TRUE,
  public = list(
    initialize = function(nthreads = 1,
                          keep.inbag = FALSE,
                          ...) {
      params <- args_to_list()
      super$initialize(params = params, ...)
    }
  ),
  private = list(
    .properties = c("continuous", "cdf"),

    .train = function(task) {
      args <- self$params

      outcome_type <- self$get_outcome_type(task)

      args$x <- as.matrix(task$X)
      args$y <- outcome_type$format(task$Y)

      fit_object <- call_with_args(quantregForest::quantregForest, args)
      return(fit_object)
    },
    .predict = function(task = NULL) {
      n <- nrow(task$data)

      # TO DO: This solution requires that we have Y info...
      data <- data.frame(task$data)
      Y_node <- task$nodes$outcome
      Y <- data[, Y_node]

      pred_val_cdf_full <- predict(self$fit_object, as.matrix(task$X), what = ecdf)

      # Average over all forests:
      predictions <- list()
      for (i in 1:n) {
        predictions[[i]] <- matrix(unlist(lapply(seq(n), function(t) {
          pred_val_cdf_full[[i]](Y[t])
        })),
        nrow = n, ncol = 1
        )
      }
      predictions <- mean.list(predictions)

      return(predictions)
    },
    .required_packages = c("quantregForest")
  )
)
