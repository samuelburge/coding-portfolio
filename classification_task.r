# ================================================================
#  STAT 639 FINAL PROJECT (GROUP 18)
#  AUTHORS: Samuel Burge, Chad Austgen, Skylar Liu
#  DATE: April 19, 2022
# ================================================================

# Set seed for reproducibility
set.seed(639)

# Load the necessary packages for the classification task
require(tidyverse)
require(MASS)
require(class)
require(glmnet)
require(ROCR)
require(e1071)
require(naivebayes)
require(tree)
require(randomForest)
require(xgboost)
require(gbm)

# ====================================================================================
#                                CLASSIFICATION TASK
# ====================================================================================

# Set working directory and import the data file
load('class_data.RData')

# ====================================================================================
# Set-up the k-fold cross validation
# ====================================================================================

# Needs to be a matrix for glmnet function to work
x <- as.matrix(x)

# Standardize the features in the data matrix
scaled_x <- scale(x, center = TRUE, scale = TRUE)

# Combine the label and features into a single data set for some algorithms (SVMs)
dataset <- data.frame(y = as.factor(y), scaled_x)

# Coerce data vector to matrix, calculate training sample size, and create folds
n <- nrow(x)
k <- 10
folds <- sample(1:k, n, replace=TRUE)


# Create search grid of SVM tuning parameters for cross-validation testing
cost_grid <- seq(0.01, 100, length.out = 15)  # cost for all kernels
gamma_grid <- seq(0, 1, length.out = 15)      # gamma for all kernels
degree_grid <- seq(1, 5)                      # degrees for polynomial kernel

# Grid search for the best parameters for the boosted trees
grid_search <- expand.grid(subsample = seq(from = 0.5, to = 1, by = 0.1),
                           max_depth = c(1, 2, 3, 4, 5),
                           eta = seq(0.001, 0.01, 0.005),
                           best_iteration = 0,
                           test_error_mean = 0)

# Storage for the best parameters of each boosted tree in each fold
boost.tuning.params <- cbind(fold = seq(1:k),
                             subsample = rep(0, 10),
                             max_depth = rep(0,10),
                             eta = rep(0, 10),
                             nrounds = rep(0, 10))

# ====================================================================================
#           Estimate the test error for each fitted model using 10-fold CV
# ====================================================================================

# Initialize vectors to store the training error for each fold
lasso.train.errors <- rep(0,k)
net.train.errors <- rep(0,k)
ridge.train.errors <- rep(0,k)
naive.train.errors <- rep(0,k)
radialSVM.train.errors <- rep(0,k)
polySVM.train.errors <- rep(0,k)
sigmoidSVM.train.errors <- rep(0,k)
boost.train.errors <- rep(0,k)


# Initialize vectors to store the CV error for each fold
lasso.fold.errors <- rep(0,k)
net.fold.errors <- rep(0,k)
ridge.fold.errors <- rep(0,k)
naive.fold.errors <- rep(0,k)
radialSVM.fold.errors <- rep(0,k)
polySVM.fold.errors <- rep(0,k)
sigmoidSVM.fold.errors <- rep(0,k)
boost.fold.errors <- rep(0,k)

# ===================================================================================
#                           K-fold cross validation loop
# ===================================================================================

for (j in 1:k) {

  # =================================================================================
  # Fit regularized logistic regression models using L1, elastic net, and L2
  # using the tuning parameters (alpha and lambda) that min. k-fold CV error rates
  # =================================================================================
    
  # ====================================
  #      Lasso logistic regression
  # ====================================
  lasso <- cv.glmnet(x[folds != j, ], y[folds != j],
                     family = "binomial", alpha = 1, nfolds = 10,
                     type.measure="class")
  
  # Error on the training set   
  lasso.tr.pred <- predict(lasso, newx = x[folds != j, ],
                           s = lasso$lambda.min, type = 'class')
  
  lasso.train.errors[j] <- mean(lasso.tr.pred != y[folds != j])
  
  # Error on the validation set 
  lasso.pred <- predict(lasso, newx = x[folds == j, ],
                        s = lasso$lambda.min, type = 'class')
  
  lasso.fold.errors[j] <- mean(lasso.pred != y[folds == j])
  

  # ====================================
  #   Elastic net logistic regression
  # ====================================
  
  # Initialize vector to store misclassification rates and lambdas for varying alpha levels 
  
  alphas <- seq(from = 0.05, to = 0.95, by = 0.05)  # weighting parameter for L1 and L2 penalties
  lambdas <- rep(0, length(alphas))                 # shrinkage parameter
  net_error_rates <- length(alphas)                 # Vector to store results
  
  # Iterate over the alpha values, finding the lambda that minimizes error rate
  for (i in 1:length(alphas)) {
    cv.net <- cv.glmnet(x, y,
                        alpha = alphas[i],
                        family = "binomial",
                        nfolds = 10,
                        type.measure = 'class')
    
    lambdas[i] <- cv.net$lambda.min
    net_error_rates[i] <- mean((predict(cv.net, x, type = 'class',
                                        s = cv.net$lambda.min) != y))
  }
  
  best.alpha <- alphas[which.min(net_error_rates)]
  best.lambda <- lambdas[which.min(net_error_rates)]
  
  net <- glmnet(x[folds != j, ], y[folds != j],
                family = "binomial", alpha = best.alpha, lambda = best.lambda,
                type.measure="class")
  
  # Error on the training set   
  net.tr.pred <- predict(net, newx = x[folds != j, ],
                         s = best.lambda, type = 'class')
  
  net.train.errors[j] <- mean(lasso.tr.pred != y[folds != j])
  
  # Error on the validation set
  net.pred <- predict(net, newx = x[folds == j, ],
                      s = best.lambda, type = 'class')
  
  net.fold.errors[j] <- mean(net.pred != y[folds==j])
  
  # ====================================
  #      Ridge logistic regression
  # ====================================
  
  ridge <- cv.glmnet(x[folds != j, ], y[folds != j],
                     family = "binomial", alpha = 0, nfolds = 10,
                     type.measure="class")
  
  # Error on the training set   
  ridge.tr.pred <- predict(ridge, newx = x[folds != j, ],
                           s = ridge$lambda.min, type = 'class')
  
  ridge.train.errors[j] <- mean(ridge.tr.pred != y[folds != j]) 
  
  # Error on the validation set
  ridge.pred <- predict(ridge, newx = x[folds == j, ],
                        s = ridge$lambda.min, type = 'class')
  
  ridge.fold.errors[j] <- mean(ridge.pred != y[folds==j])
  
  # ====================================
  #            Naive Bayes
  # ====================================
  
  fit <- naive_bayes(y ~ .,
                     data = data.frame(x[folds != j, ], y = as.factor(y[folds != j])),
                     usekernel = TRUE)
  
  train.pred <- predict(fit, x[folds != j, ])
  naive.train.errors[j] <- mean(train.pred != y[folds != j])
  
  cv.pred <- predict(fit, x[folds == j , ])
  naive.fold.errors[j] <- mean(cv.pred != y[folds == j])
  
  # ====================================
  #      SVM with radial kernel
  # ====================================

  # This will be used for the remaining SVM algorithms as well
  cv_train_set <- dataset[(folds != j), ]
  cv_test_set <- dataset[(folds == j), ]

  # Inner cross-validation for tuning parameters
  # (The tune function will do inner cross-validation)
  tune.out <- tune( svm, y ~ ., data = cv_train_set,

                    kernel = "radial",

                    ranges = list(cost = cost_grid,
                                  gamma = gamma_grid),

                    tunecontrol = tune.control(sampling = "cross",
                                               cross = k,
                                               best.model = TRUE) )

  # Output the best model from the hyper-parameter tuning inner CV
  params <- tune.out$best.parameters

  radialSVM.train.errors[j] <- tune.out$best.performance

  # Retrain the SVM on the full fold using the best cost value
  fit <- svm(y ~ ., data = cv_train_set,

             kernel = 'radial',

             cost = params$cost,

             gamma = params$gamma)

  # predict on the test set
  pred <- predict(fit, newdata = cv_test_set)

  # Calculate the test error
  radialSVM.fold.errors[j] <- sum(pred != cv_test_set[ ,1]) / nrow(cv_test_set)

  # ====================================
  #      SVM with poly. kernel
  # ====================================

  # Inner cross-validation for tuning parameters
  # (I believe the tune function will do inner cross-validation)
  tune.out <- tune( svm, y ~ ., data = cv_train_set,

                    kernel = "polynomial",

                    ranges = list(cost = cost_grid,
                                  gamma = gamma_grid,
                                  degree = degree_grid),

                    tunecontrol = tune.control(sampling = "cross",
                                               cross = k,
                                               best.model = TRUE) )

  # Output the best model from the hyper-parameter tuning inner CV
  params <- tune.out$best.parameters

  polySVM.train.errors[j] <- tune.out$best.performance

  # Retrain the SVM on the full fold using the best cost value
  fit <- svm(y ~ ., data = cv_train_set,

             kernel = 'polynomial',

             cost = params$cost,

             gamma = params$gamma,

             degree = params$degree )

  # predict on the test set
  pred <- predict(fit, newdata = cv_test_set)

  # Calculate the test error
  polySVM.fold.errors[j] <- sum(pred != cv_test_set[ ,1]) / nrow(cv_test_set)
  
  # ====================================
  #      Boosted Trees (XGBoost)
  # ====================================

    # Convert the training and test sets of each fold into a data matrix (DMatrix)
    train <- xgb.DMatrix(data = as.matrix(x[folds != j, ]), label = y[folds != j])
    test <- xgb.DMatrix(data = as.matrix(x[folds == j, ]), label = y[folds == j])
    best_iteration <- rep(0, nrow(grid_search))
    
    for (i in 1:nrow(grid_search)) {
      inner_fit <- xgb.cv(data = train,
                          objective = "binary:logistic",  # "binary:logistic" for logistic regression
                          metrics = c('error'),
                          
                          params = list(booster = 'gbtree',
                                        subsample = grid_search[i, 'subsample'],
                                        eta = grid_search[i, 'eta'],
                                        max_depth = grid_search[i, 'max_depth']),
                          
                          nrounds = 10000,              # max number of boosting iterations
                          early_stopping_rounds = 150,  # stop boosting after x iterations with no improvement
                          nfold = k)
      
      # Store the results for each combination of tuning parameters and the associated performance
      grid_search[i, 'best_iteration'] = inner_fit$best_iteration
      grid_search[i, 'test_error_mean'] = inner_fit$evaluation_log[inner_fit$best_iteration, 'test_error_mean']
    }
    
    boost.tuning.params[j, 'subsample'] <- grid_search[which.min(grid_search$test_error_mean), 'subsample']
    boost.tuning.params[j, 'eta'] <- grid_search[which.min(grid_search$test_error_mean), 'eta']
    boost.tuning.params[j, 'max_depth'] <- grid_search[which.min(grid_search$test_error_mean), 'max_depth']
    boost.tuning.params[j, 'nrounds'] <- grid_search[which.min(grid_search$test_error_mean), 'best_iteration']
    
    fit <- xgboost(data = train,
                   objective = "binary:logistic",  # "binary:logistic" for logistic regression
                   metrics = c('error'),
                   
                   params = list(booster = 'gbtree',
                                 subsample = boost.tuning.params[j, 'subsample'],
                                 eta = boost.tuning.params[j, 'eta'],
                                 max_depth = boost.tuning.params[j, 'max_depth']),
                   
                   nrounds = boost.tuning.params[j, 'nrounds']) 
    
    y_fit <- predict(fit, data.matrix(x[folds != j, ]))
    boost.train.errors[j] <- sum((y_fit > 0.5) != y[folds != j]) / length(y[folds != j])
    
    y_pred <- predict(fit, data.matrix(x[folds == j, ]))
    boost.fold.errors[j] <- sum((y_pred > 0.5) != y[folds == j]) / length(y[folds == j])
  
    paste('Fold ',j,' complete.', sep = '')
} # END OUTER CV LOOP

# ===================================================================================
#      Random forest doesn't need to be cross-validated (will use OOB error)
# ===================================================================================
     
# Fit a random forest to the data
fit <- randomForest(y = dataset[, 1],
                    x = dataset[,-1],
                    ntree = 10000)

# Calculate the training error rate
randforest_error_rate <- fit$err.rate[10000]

# ===================================================================================
#        Compute the CV error for each algorithm and compare the results
# ===================================================================================

# Compute the average training error
     lasso.train.error <- mean(lasso.train.errors)
       net.train.error <- mean(net.train.errors)
     ridge.train.error <- mean(ridge.train.errors)
     naive.train.error <- mean(naive.train.errors)
 radialSVM.train.error <- mean(radialSVM.train.errors)
   polySVM.train.error <- mean(polySVM.train.errors)
randforest.train.error <- NA
     boost.train.error <- mean(boost.train.errors)

# Compute the average validation error
     lasso.cv.error <- mean(lasso.fold.errors)
       net.cv.error <- mean(net.fold.errors)
     ridge.cv.error <- mean(ridge.fold.errors)
     naive.cv.error <- mean(naive.fold.errors)
 radialSVM.cv.error <- mean(radialSVM.fold.errors)
   polySVM.cv.error <- mean(polySVM.fold.errors)
randforest.cv.error <- randforest_error_rate
     boost.cv.error <- mean(boost.fold.errors)

# Combine the estimated train and test errors into vectors
    train.errors <- c(lasso.train.error, net.train.error, ridge.train.error, naive.train.error,
                      radialSVM.train.error, polySVM.train.error, randforest.train.error, boost.train.error)
    
names(train.errors) <- c('Lasso','Net','Ridge', 'Naive Bayes', 'Radial SVM',
                         'Poly. SVM', 'Random Forest', 'Boosted Trees')

       cv.errors <- c(lasso.cv.error, net.cv.error, ridge.cv.error, naive.cv.error,
                      radialSVM.cv.error, polySVM.cv.error, randforest.cv.error, boost.cv.error)
       
names(cv.errors) <- c('Lasso','Net','Ridge', 'Naive Bayes', 'Radial SVM',
                      'Poly. SVM', 'Random Forest', 'Boosted Trees')

# Combine the training and test errors together for comparison
          errors_matrix <- cbind(train.errors, cv.errors)
colnames(errors_matrix) <- c("Avg. Training Error","Est. Test Error")

# Print the results
errors_matrix


# Create a nice box plot to visualize the variation between folds
foldErrors <- data.frame(boost.fold.errors,lasso.fold.errors,net.fold.errors,
                         ridge.fold.errors,naive.fold.errors,radialSVM.fold.errors,polySVM.fold.errors)

foldErrors$RForest <- randforest_error_rate
colnames(foldErrors) <- c('Boosted Trees','Lasso','E.Net','Ridge',
                          'N.Bayes','Radial SVM','Poly SVM','R. Forest')

boxplot(foldErrors, xlab='Model',
        ylab='CV Fold Error Rate', main='Model Comparison')

# ===================================================================================
#  Compute test error for the best model, refit the model, and compute predictions
# ===================================================================================

# Calculate the estimated test error
test_error <- mean(boost.fold.errors)

# Re-tune the model using all the training data
final_grid_search <- expand.grid(subsample = seq(from = 0.5, to = 1, by = 0.1),
                                 max_depth = c(1, 2, 3, 4, 5),
                                 eta = seq(0.001, 0.01, 0.005),
                                 best_iteration = 0,
                                 test_error_mean = 0)

data <- xgb.DMatrix(data = as.matrix(x), label = y)

for (i in 1:nrow(grid_search)) {
  inner_fit <- xgb.cv(data = data,
                      objective = "binary:logistic",  # "binary:logistic" for logistic regression
                      metrics = c('error'),
                      
                      params = list(booster = 'gbtree',
                                    subsample = grid_search[i, 'subsample'],
                                    eta = grid_search[i, 'eta'],
                                    max_depth = grid_search[i, 'max_depth']),
                      
                      nrounds = 10000,              # max number of boosting iterations
                      early_stopping_rounds = 150,  # stop boosting after x iterations with no improvement
                      nfold = 5)
  
  # Store the results for each combination of tuning parameters and the associated performance
  grid_search[i, 'best_iteration'] = inner_fit$best_iteration
  grid_search[i, 'test_error_mean'] = inner_fit$evaluation_log[inner_fit$best_iteration, 'test_error_mean']
}

# Identify the best tuning parameters
boost.tuning.params[j, 'subsample'] <- grid_search[which.min(grid_search$test_error_mean), 'subsample']
boost.tuning.params[j, 'eta'] <- grid_search[which.min(grid_search$test_error_mean), 'eta']
boost.tuning.params[j, 'max_depth'] <- grid_search[which.min(grid_search$test_error_mean), 'max_depth']
boost.tuning.params[j, 'nrounds'] <- grid_search[which.min(grid_search$test_error_mean), 'best_iteration']

# Fit the data with the best tuning parameters 
best_fit <- xgboost(data = data,
                    objective = "binary:logistic",  # "binary:logistic" for logistic regression
                    metrics = c('error'),
                    
                    params = list(booster = 'gbtree',
                                  subsample = boost.tuning.params[j, 'subsample'],
                                  eta = boost.tuning.params[j, 'eta'],
                                  max_depth = boost.tuning.params[j, 'max_depth']),
                    
                    nrounds = boost.tuning.params[j, 'nrounds'])

importance_matrix <- xgb.importance(colnames(data), model = best_fit)
xgb.ggplot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")

# Generate predictions on the test set
ynew <- predict(best_fit, data.matrix(xnew))

# Converts the prediction to 0, 1 labels like the training y labels
ynew <- as.numeric(ynew >= 0.5)

# Save the test predictions and the estimated test error as specified
save(ynew, test_error, file = "18.RData")

# Save it so when we finally it all together we don't have to do it again.
save.image('cv_final_results.RData')


