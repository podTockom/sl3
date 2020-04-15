context("test-quantile-regression-Forests: Lrnr_quantregForest")

library(sl3)
library(origami)
library(R6)
library(uuid)
library(devtools)
# load_all()

set.seed(11)
n <- 1000

# Generate some simple data (normal, dependent on uniform)
U <- runif(n = n)
Y <- rnorm(n = n, mean = 2 * U)

# True cdfs
true_ordered_cdfs <- pnorm(Y[order(Y)])
true_cdfs <- pnorm(Y)

# Create a full dataset:
df <- cbind.data.frame(ID = seq(n), U = U, Y = as.factor(Y))

# Create a task:
task <- sl3_Task$new(df,
  covariates = c("U"), outcome = "Y", id = "ID",
  folds = make_folds(n = n)
)

lrnr_quantregForest <- Lrnr_quantregForest$new()
fit <- lrnr_quantregForest$train(task)
preds <- fit$predict()

expect_equal(median(preds), 0.5, tol = 2)
