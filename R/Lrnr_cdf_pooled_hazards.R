#' CDF estimation via pooled hazards
#'
#' This learner converts a binomial learner into a multinomial learner
#' using a pooled hazards model.
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
#'   \item{\code{binomial_learner}}{The learner to wrap.}
#' }
#'
#' @template common_parameters
#
Lrnr_cdf_pooled_hazards <- R6Class(
  classname = "Lrnr_cdf_pooled_hazards",
  inherit = Lrnr_base, portable = TRUE,
  class = TRUE,
  public = list(
    initialize = function(binomial_learner = NULL, ...) {
      if (is.null(binomial_learner)) {
        binomial_learner <- make_learner(Lrnr_glm_fast)
      }
      params <- list(binomial_learner = binomial_learner, ...)
      super$initialize(params = params, ...)
    },

    normalize_rows = function(x) {
      return(sweep(x, 1, rowSums(x), "/"))
    },

    rescale = function(obs_pred) {
      min_pred <- min(obs_pred)
      max_pred <- max(obs_pred)

      return((obs_pred - min_pred) / (max_pred - min_pred))
    }
  ),

  private = list(
    .properties = c("categorical"),

    .train = function(task) {
      outcome_type <- self$get_outcome_type(task)

      if (outcome_type$type != "categorical") {
        stop("Lrnr_pooled_hazards only works for categorical outcomes")
      }

      # trim=TRUE creates issues with CV learners?
      hazards_task <- pooled_hazard_task(task)
      hazards_task$folds <- id_folds_to_folds(task$folds, hazards_task$data$id)

      outcome_levels <- task$outcome_type$levels
      binomial_learner <- self$params$binomial_learner
      hazards_fit <- binomial_learner$train(hazards_task)

      # NOTE: drop hazards training_task to save memory
      hazards_fit$set_train(hazards_fit$fit_object, NULL)
      fit_object <- list(
        hazards_fit = hazards_fit,
        outcome_levels = outcome_levels
      )
      return(fit_object)
    },
    
    .predict = function(task) {
      # Get original values used to build the cdf estimate
      levels_n <- length(self$fit_object$outcome_levels)
      levels <- self$fit_object$outcome_levels

      # "id":         level of cdf,
      # "bin_number": ordered level,
      # "in_bin":     when we reach "bin_number"
      # Ex: Level "-1.625" is the lowest level, hence bin_number=1,
      #    but id=55 since it comes number 55 in the observed data
      pred_hazards_task <- pooled_hazard_task(task, trim = FALSE)
      pred_hazards_task$folds <- id_folds_to_folds(task$folds, pred_hazards_task$data$id)

      raw_preds <- self$fit_object$hazards_fit$predict(pred_hazards_task)

      outcome_level <- match(task$data$Y, levels)
      
      prediction <- apply(raw_preds, 2, function(raw_pred){
        # Rows are outcomes sorted as observed, columns are "times"/level of cdf
        predmat <- matrix(raw_pred, nrow = task$nrow, byrow = FALSE)
        t <- ncol(predmat)
        
        # probability of surviving until time t
        psurv <- t(apply(1 - predmat, 1, cumprod))
        psurv <- cbind(1, psurv)[, seq_len(ncol(predmat))]
        predictions <- 1 - psurv
        
        # predictions <- psurv * predmat
        # predictions <- self$normalize_rows(predictions)
        # predictions <-  1-predictions
        
        # Find the closest value if NAs
        if (anyNA(outcome_level)) {
          outcome_level <- findInterval(
            as.numeric(levels(task$data$Y))[task$data$Y],
            as.numeric(levels)
          )
        }
        
        # Scale if necessary
        if (levels_n > t) {
          outcome_level <- round(rescale(outcome_level,
                                         to = c(1, t)
          ))
        } else if (levels_n < t) {
          outcome_level <- round(rescale(outcome_level,
                                         to = c(1, levels_n)
          ))
        }
        
        est <- matrix(NA, nrow = t, ncol = 1)
        for (i in 1:t) {
          est[i] <- predictions[i, outcome_level[i]]
        }
        
        # Potentially rescale
        if (max(est) < 0.6) {
          est <- self$rescale(est)
        }
        
        est
      })
      return(prediction)
    },
    .required_packages = c()
  )
)
