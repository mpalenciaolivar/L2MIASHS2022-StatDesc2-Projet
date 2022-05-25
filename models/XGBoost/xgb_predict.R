#Évaluation du modèle XGBoost------#

#pas fonctionnel


load('data/processed/test.csv')
load('models/XGBoost/xgb')

test <- select(test, -1, -3, -5, -8, -12)

test$amount <- as.integer(test$amount)
test$oldbalanceOrg <- as.integer(test$oldbalanceOrg) 
test$newbalanceOrig <- as.integer(test$newbalanceOrig)
test$oldbalanceDest <- as.integer(test$oldbalanceDest)
test$newbalanceDest <- as.integer(test$newbalanceDest)


X_test = data.matrix(test[,-7])
y_test = as.numeric(test$isFraud)


xgb_pred <- predict(xgb, X_test)


xgb_precision <- Precision(y_true, xgb_pred, positive = 1)
xgb_recall <- Recall(y_true, xgb_pred, positive = 1)
xgb_f1 <- F1_Score(y_true, xgb_pred, positive = 1)


roc.curve(xgb_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)
