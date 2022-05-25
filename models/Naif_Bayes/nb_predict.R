#PIPELINE EVALUATION NAIVE BAYES------------
library(MLmetrics)
library(ROSE)


load('data/processed/test.csv')
load('models/Naif_Bayes/BNaif_FINAL')

nb_y_pred <- predict(BNaif, test)
y_true <- test$isFraud

BNaif_precision <- Precision(y_true, nb_y_pred, positive = 1)
BNaif_recall <- Recall(y_true, nb_y_pred, positive = 1)
BNaif_f1 <- F1_Score(y_true, nb_y_pred, positive = 1)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)