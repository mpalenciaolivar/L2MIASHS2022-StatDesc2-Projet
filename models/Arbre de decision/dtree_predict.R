
#PIPELINE ARBRE DE DECISION--------------

library(MLmetrics)
library(ROSE)
library(rpart)
library(rpart.plot)


#Chargement du modèle

load('models/Arbre de decision/decision_tree_FINAL')
summary(dtree)


#Prédiction sur test

load('data/processed/test.csv')
load('models/decision_tree_FINAL')

dtree_y_pred <- predict(dtree, test, type = 'class')
y_true <- test$isFraud

dtree_precision <- Precision(y_true, dtree_y_pred, positive = 1)
dtree_recall <- Recall(y_true, dtree_y_pred, positive = 1)
dtree_f1 <- F1_Score(y_true, dtree_y_pred, positive = 1)
dtree_err <- mean(y_true != dtree_y_pred)

rpart.plot(dtree, extra = 106)

roc.curve(dtree_y_pred, dtree_y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)

