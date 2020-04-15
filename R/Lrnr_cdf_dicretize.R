#' cdf from Classification
#'
#' This learner discretizes variable and then fits a categorical learner
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that is.count is.flag
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
#'   \item{\code{categorical_learner}}{The learner to wrap.}
#' }
#'
#' @template common_parameters
#
Lrnr_cdf_discretize <- R6Class(
  classname = "Lrnr_cdf_discretize",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(categorical_learner = NULL, type = "equal_mass",
                          n_bins = 20, ...) {
      if (is.null(categorical_learner)) {
        categorical_learner <- make_learner(Lrnr_glmnet)
      }
      params <- list(
        type = type, n_bins = n_bins,
        categorical_learner = categorical_learner, ...
      )
      super$initialize(params = params, ...)
    },
    rescale = function(obs_pred) {
      min_pred <- min(obs_pred)
      max_pred <- max(obs_pred)

      return((obs_pred - min_pred) / (max_pred - min_pred))
    }
  ),
  active = list(
    return_breaks = function() {
      return(private$.breaks)
    }
  ),
  private = list(
    .properties = c("density"),
    .breaks = NULL,

    .train = function(task) {
      if (is.factor(task$Y)) {
        Y <- as.numeric(levels(task$Y))[task$Y]

        discretized <- discretize_variable(
          x = Y,
          type = self$params$type,
          n_bins = self$params$n_bins,
          breaks = self$params_breaks
        )
      } else {
        discretized <- discretize_variable(task$Y,
          type = self$params$type,
          n_bins = self$params$n_bins,
          breaks = self$params_breaks
        )
      }

      private$.breaks <- discretized$breaks

      # make discretized task
      new_columns <-
        task$add_columns(data.table(
          Y_discretize = factor(discretized$x_discrete_int)
        ))
      discrete_task <- task$next_in_chain(
        outcome = "Y_discretize",
        column_names = new_columns
      )
      # fit categorical learner to discretized task
      categorical_fit <- self$params$categorical_learner$train(discrete_task)

      # fit_object <- list(
      #  categorical_fit = categorical_fit,
      #  breaks = discretized$breaks
      # )
      fit_object <- categorical_fit

      return(fit_object)
    },

    .predict = function(task) {
      # make discretized task
      if (is.factor(task$Y)) {
        Y <- as.numeric(levels(task$Y))[task$Y]

        discretized <- discretize_variable(Y,
          breaks = self$return_breaks
        )
      } else {
        discretized <- discretize_variable(task$Y,
          breaks = self$return_breaks
        )
      }

      new_columns <-
        task$add_columns(data.table(
          Y_discretize =
            factor(discretized$x_discrete_int)
        ))
      discrete_task <- task$next_in_chain(
        outcome = "Y_discretize",
        column_names = new_columns
      )

      # predict categorical learner on discretized task
      raw_preds <- self$fit_object$predict(discrete_task)

      # bin_lengths <- diff(self$return_breaks)
      # scale_mat <- matrix(rep(1 / bin_lengths, each = task$nrow),
      #                    nrow = task$nrow
      # )
      # predmat <- raw_preds * scale_mat

      # subset predictions to only those bins relevant
      # obs_pred <- predmat[cbind(seq_len(task$nrow), discretized$x_discrete)]

      # Rescale:
      # obs_pred <- self$rescale(obs_pred)
      obs_pred <- raw_preds

      return(obs_pred)
    },
    .required_packages = c()
  )
)
