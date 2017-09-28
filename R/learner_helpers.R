# these functions exist for two reasons:
# 1) a delayed(learner) is not a learner and therefore doesn't have the same members as a learner
# 2) delayed_fun does not respect member function's environment (containing self and private)

#' Learner helpers
#' @param learner a learner object to fit to the task
#' @param task the task to fit on
#' @param pretrain any data obtained from a pretrain step
#' @rdname learner_helpers
#' @export
learner_train <- function(learner, task, pretrain){
  learner$base_train(task, pretrain)
}

#' @rdname learner_helpers
#' @export
delayed_learner_train <- function(learner, task){
  pretrain <- learner$pretrain(task)
  train_delayed <- delayed_fun(learner_train)(learner, task, pretrain)
  train_delayed$name <- learner$name
  if(!is.null(pretrain)){
    #if a learner is sequential assume the train step is minimal and don't paralellize
    train_delayed$sequential <- TRUE
  }
  return(train_delayed)
}


#' @param learner_fit a learner object that has already been fit
#' @rdname learner_helpers
#' @export
learner_fit_predict <- function(learner_fit, task = NULL){
  learner_fit$base_predict(task)
}

#' @rdname learner_helpers
#' @export
delayed_learner_fit_predict <- function(learner_fit, task=NULL){
  pred_delayed <- delayed_fun(learner_fit_predict, sequential = TRUE)(learner_fit, task)
  pred_delayed$name <- "predict"
  return(pred_delayed)
}

#' @rdname learner_helpers
#' @export
learner_fit_chain <- function(learner_fit, task = NULL){
  learner_fit$base_chain(task)
}

#' @rdname learner_helpers
#' @export
delayed_learner_fit_chain <- function(learner_fit, task = NULL){
  chain_delayed <- delayed_fun(learner_fit_chain, sequential = TRUE)(learner_fit, task)
  chain_delayed$name <- "chain"
  return(chain_delayed)
}