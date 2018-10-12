library(forestry)
library(ranger)
library(glmnet)
library(ggplot2)

# Define all estimators:

estimator_grid <- list(
  "ranger_1" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),
  "ranger_2" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),
  "ranger_3" = function(Xobs, Yobs)
    ranger(Yobs ~., data = cbind(Xobs, Yobs)),

  "glmnet_1" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = 1),
  "glmnet_2" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = 0),
  "glmnet_3" = function(Xobs, Yobs)
    glmnet(x = data.matrix(Xobs), y = Yobs, alpha = .5),

  "xgboost_1" = function(Xobs, Yobs)
    xgboost(data = data.matrix(Xobs), label= Yobs, nrounds=10),
  "gbm_1" = function(Xobs, Yobs)
    gbm.fit(Xobs, Yobs, distribution="gaussian"),
  "gradient_boosting_1" = function(Xobs, Yobs)
    gradient_boosting(Xobs, Yobs, n_iterations = 10)
)



predictor_grid <- list(
  "ranger_1" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_2" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  "ranger_3" = function(estimator, feat) {
    return(predict(estimator, feat)$predictions)
  },
  
  "glmnet_1" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },
  "glmnet_2" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },
  "glmnet_3" = function(estimator, feat) {
    feat <- data.matrix(feat)
    l <- estimator$lambda[estimator$lambda == min(estimator$lambda)]
    return(predict(estimator, s = l, newx = feat))
  },

  "xgboost_1" = function(estimator, feat) {
    feat <- data.matrix(feat)
    return(predict(estimator, feat))
  },
  "gbm_1" = function(estimator, feat) {
    return(predict(estimator, feat, n.trees=100))
  },
  "gradient_boosting_1" = function(estimator, feat) {
    return(predict(estimator, feat))
  }
)
