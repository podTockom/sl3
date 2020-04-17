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
  folds = make_folds(n = n, V=5)
)

# Create a library:
lrnr_xgboost <- make_learner(Lrnr_xgboost)
lrnr_glm <- make_learner(Lrnr_glm_fast)
cdf_xgboost <- Lrnr_cdf_pooled_hazards$new(lrnr_xgboost)
cdf_glm <- Lrnr_cdf_pooled_hazards$new(lrnr_glm)

### Test learner:
hazard_learner <-  Lrnr_cdf_pooled_hazards$new(lrnr_xgboost)
xgboost_fit <- hazard_learner$train(task)
pred_xgboost <- xgboost_fit$predict(task)
quants <- estQuantile(task=task, hazard_fit = xgboost_fit)

### Test Stack:
stack <- make_learner(Stack, cdf_glm, cdf_xgboost)
stack_fit <- stack$train(task)
pred_stack <- stack_fit$predict(task)
expect_equal(dim(pred_stack)[2], 2, tol=1)

### Test CV preds:
cv_stack <- Lrnr_cv$new(stack, full_fit = TRUE)
cv_fit <- cv_stack$train(task)
pred_cv <- cv_fit$predict(task)
pred_cv_f1 <- cv_fit$predict_fold(task=task, fold_number = 1)
expect_equal(dim(pred_cv)[2], 2, tol=1)

### Test sl preds:
metalearner <- make_learner(Lrnr_nnls)
sl <- Lrnr_sl$new(learners = stack,
                  metalearner = metalearner)
sl_fit <- sl$train(task)
preds_sl <- sl_fit$predict()/100

mse <- sum((preds_sl - true_cdfs)^2)
expect_equal(mse, 7.53, tol = 2)

### Discretize the outcome:
# Try to discretize the learner:
hazard_learner <-  Lrnr_cdf_pooled_hazards$new(lrnr_xgboost)
discretize_cdf_learner <- Lrnr_cdf_discretize$new(hazard_learner,
  type = "equal_mass",
  n_bins = 20
)
fit_discretize_cdf <- discretize_cdf_learner$train(task)
pred_discretize_cdf <- fit_discretize_cdf$predict()

mse_2 <- sum((pred_discretize_cdf - true_cdfs)^2)

expect_equal(mse_2, 9.49, tol = 2)
