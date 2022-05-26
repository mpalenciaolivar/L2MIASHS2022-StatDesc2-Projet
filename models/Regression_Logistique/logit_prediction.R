#ÉVALUATION REGRESSION LOGISTIQUE---------------#
library(MLmetrics)
library(ROSE)


load('data/processed/test.csv')
#load('models/Regression_Logistique/Regression_Logistique')

y_true <- test$isFraud


logit_y_pred <- predict(glm_fit,test, type = 'response')
logit_y_pred <- as.factor(ifelse(y_pred > 0.5, 1, 0))

logit_precision <- Precision(y_true, logit_y_pred, positive = 1)
logit_recall <- Recall(y_true, logit_y_pred, positive = 1)
logit_f1 <- F1_Score(y_true, logit_y_pred, positive = 1)

roc.curve(logit_y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)
