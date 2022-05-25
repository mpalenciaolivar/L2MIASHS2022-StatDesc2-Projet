#CRÉATION XGBOOST----------------#

library(xgboost)
library(dplyr)


#La création du modèle XGBOOST est un peu particulière car nous devons enlever 
#toutes les variables non-numériques (ou convertir en numérique si possible)
#et l'entrée doit être sous forme matricielle et non pas de data frame

load('data/processed/train.csv')

train <- select(train, -1, -3, -5, -8, -12)


train$amount <- as.integer(train$amount)
train$oldbalanceOrg <- as.integer(train$oldbalanceOrg) 
train$newbalanceOrig <- as.integer(train$newbalanceOrig)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$isFraud <- as.integer(train$isFraud)

X_train = data.matrix(train[,-7])
y_train = as.numeric(train$isFraud)

xgboost_train = xgb.DMatrix(data=X_train, label=y_train)

#------------------Construction du modele------------------#

xgb <- xgboost(data = xgboost_train, 
               eta = 1,
               max_depth = 10, 
               nrounds = 30, 
               objective = "binary:logistic",
               eval_metric = "auc",
               verbose = 1,
               nthread = 7,
               set.seed(777))

summary(xgb)

#Les 3 variables plus importantes selon xgb

xgb.plot.shap(data = X_train,
              model = xgb,
              top_n = 3)