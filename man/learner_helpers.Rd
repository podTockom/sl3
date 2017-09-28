% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/learner_helpers.R
\name{learner_train}
\alias{delayed_learner_fit_chain}
\alias{delayed_learner_fit_predict}
\alias{delayed_learner_train}
\alias{learner_fit_chain}
\alias{learner_fit_predict}
\alias{learner_train}
\title{Learner helpers}
\usage{
learner_train(learner, task, pretrain)

delayed_learner_train(learner, task)

learner_fit_predict(learner_fit, task = NULL)

delayed_learner_fit_predict(learner_fit, task = NULL)

learner_fit_chain(learner_fit, task = NULL)

delayed_learner_fit_chain(learner_fit, task = NULL)
}
\arguments{
\item{learner}{a learner object to fit to the task}

\item{task}{the task to fit on}

\item{pretrain}{any data obtained from a pretrain step}

\item{learner_fit}{a learner object that has already been fit}
}
\description{
Learner helpers
}
