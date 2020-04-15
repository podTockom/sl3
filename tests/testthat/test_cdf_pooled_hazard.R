context("test-cdf_pooled_hazard: Lrnr_cdf_pooled_hazards")

library(sl3)
library(origami)
library(R6)
library(uuid)
library(devtools)
# load_all()

set.seed(11)
n <- 100

# Generate some simple data (normal, dependent on uniform)
U <- runif(n = n)
Y <- rnorm(n = n, mean = 2 * U)

# True cdfs
true_ordered_cdfs <- pnorm(Y[order(Y)])
true_cdfs <- pnorm(Y)

# Create a full dataset:
# df <- cbind.data.frame(ID=rep(1,n), U=U, Y=as.factor(Y))
df <- cbind.data.frame(ID = seq(n), U = U, Y = as.factor(Y))

# Create a task:
task <- sl3_Task$new(df,
  covariates = c("U"), outcome = "Y", id = "ID",
  folds = make_folds(n = n)
)

# Create a library:
grid_params <- list(
  max_depth = c(2, 5, 8),
  eta = c(0.005, 0.1, 0.25)
)
grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
params_default <- list(nthread = getOption("sl.cores.learners", 1))
xgb_learners <- apply(grid, MARGIN = 1, function(params_tune) {
  do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))
})
lrnr_glm <- make_learner(Lrnr_glm_fast)
learners <- unlist(list(xgb_learners, lrnr_glm))
sl <- make_learner(Lrnr_sl, learners)

hazard_learner <- Lrnr_cdf_pooled_hazards$new(sl)
hazard_fit <- hazard_learner$train(task)
pred <- hazard_fit$predict(task)

mse <- sum((pred - true_cdfs)^2)

expect_equal(mse, 7.4, tol = 2)

# Discretize the outcome:
# Try to discretize the learner:
discretize_cdf_learner <- Lrnr_cdf_discretize$new(hazard_learner,
  type = "equal_mass",
  n_bins = 20
)
fit_discretize_cdf <- discretize_cdf_learner$train(task)
pred_discretize_cdf <- fit_discretize_cdf$predict()

mse_2 <- sum((pred_discretize_cdf - true_cdfs)^2)

expect_equal(mse_2, 7.2, tol = 2)
