% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lrnr_earth.R
\docType{class}
\name{Lrnr_earth}
\alias{Lrnr_earth}
\title{Earth - multivariate adaptive regression splines}
\format{\code{\link{R6Class}} object.}
\usage{
Lrnr_earth
}
\value{
Learner object with methods for training and prediction. See
\code{\link{Lrnr_base}} for documentation on learners.
}
\description{
This learner provides fitting procedures for building regression models using
the techniques in Friedman’s papers "Multivariate Adaptive Regres-sion
Splines" and "Fast MARS", using the function \code{\link[earth]{earth}} from
the \code{earth} package.
}
\section{Parameters}{

\describe{
\item{\code{degree}}{A \code{numeric} specifying the maximum degree of
interactions to be used in the model. This defaults to 2, specifying up
through one-way interaction terms. Note that this differs from the
default of \code{earth::earth}.
}
\item{\code{penalty}}{Generalized Cross Validation (GCV) penalty per knot.
Defaults to 3 as recommended for \code{degree} > 1 in the documentation
of \code{earth::earth}. Special values (for use by knowledgeable users):
The value 0 penalizes only terms, not knots. The value -1 means no
penalty.
}
\item{\code{pmethod}}{Pruning method, defaulting to \code{"backward"}.
Other options include \code{"none"}, \code{"exhaustive"},
}
\item{\code{nfold}}{Number of cross-validation folds. Default is0, no
cross validation.
}
\item{\code{ncross}}{Only applies if \code{nfold} > 1. Number of
cross-validations. Each cross-validation has \code{nfold} folds.
Defaults to 1.
}
\item{\code{minspan}}{Minimum number of observations between knots.
}
\item{\code{endspan}}{Minimum number of observations before the first and
after the final knot.
}
\item{\code{...}}{Other parameters passed to \code{\link[earth]{earth}}.
See its documentation for details.
}
}
}

\seealso{
Other Learners: \code{\link{Custom_chain}},
  \code{\link{Lrnr_HarmonicReg}}, \code{\link{Lrnr_arima}},
  \code{\link{Lrnr_bartMachine}}, \code{\link{Lrnr_base}},
  \code{\link{Lrnr_bilstm}}, \code{\link{Lrnr_caret}},
  \code{\link{Lrnr_condensier}}, \code{\link{Lrnr_cv}},
  \code{\link{Lrnr_dbarts}},
  \code{\link{Lrnr_define_interactions}},
  \code{\link{Lrnr_density_discretize}},
  \code{\link{Lrnr_density_hse}},
  \code{\link{Lrnr_density_semiparametric}},
  \code{\link{Lrnr_expSmooth}}, \code{\link{Lrnr_gam}},
  \code{\link{Lrnr_gbm}}, \code{\link{Lrnr_glm_fast}},
  \code{\link{Lrnr_glmnet}}, \code{\link{Lrnr_glm}},
  \code{\link{Lrnr_grf}}, \code{\link{Lrnr_h2o_grid}},
  \code{\link{Lrnr_hal9001}},
  \code{\link{Lrnr_haldensify}},
  \code{\link{Lrnr_independent_binomial}},
  \code{\link{Lrnr_lstm}}, \code{\link{Lrnr_mean}},
  \code{\link{Lrnr_multivariate}}, \code{\link{Lrnr_nnls}},
  \code{\link{Lrnr_optim}}, \code{\link{Lrnr_pca}},
  \code{\link{Lrnr_pkg_SuperLearner}},
  \code{\link{Lrnr_polspline}},
  \code{\link{Lrnr_pooled_hazards}},
  \code{\link{Lrnr_randomForest}},
  \code{\link{Lrnr_ranger}},
  \code{\link{Lrnr_revere_task}}, \code{\link{Lrnr_rfcde}},
  \code{\link{Lrnr_rpart}}, \code{\link{Lrnr_rugarch}},
  \code{\link{Lrnr_screener_corP}},
  \code{\link{Lrnr_screener_corRank}},
  \code{\link{Lrnr_screener_randomForest}},
  \code{\link{Lrnr_sl}}, \code{\link{Lrnr_solnp_density}},
  \code{\link{Lrnr_solnp}}, \code{\link{Lrnr_stratified}},
  \code{\link{Lrnr_subset_covariates}},
  \code{\link{Lrnr_svm}}, \code{\link{Lrnr_tsDyn}},
  \code{\link{Lrnr_xgboost}}, \code{\link{Pipeline}},
  \code{\link{Stack}}, \code{\link{define_h2o_X}},
  \code{\link{undocumented_learner}}
}
\concept{Learners}
\keyword{data}
