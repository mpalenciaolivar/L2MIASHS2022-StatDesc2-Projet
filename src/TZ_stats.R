
#Executez le script "requirements.R"

library(disk.frame)
library(parallel)
library(dplyr)
library(ggplot2)
library(xgboost)


#Configuration
nCores <- detectCores()    
setup_disk.frame(workers = nCores)
options(future.globals.maxSize = Inf)


#Morceler les données
df <- csv_to_disk.frame(
  file.path("data", "train.csv"), 
  outdir = file.path("data", "train.df"),
  inmapfn = base::I,
  recommend_nchunks(sum(file.size(file.path("data", "train.csv")))),
  backend = "data.table")
df[1,]

set.seed(777)

#Mettre 60% du jeu de donnée dans train

library(splitTools)
library(ranger)

temp <- partition(is.atomic(temp), p = c(train = 0.6, valid = 0.2, test = 0.2))

temp <- collect(sample_frac(df, 1))



          #Exploration des données--------


head(df)
df$amount <- as.integer(df$amount)
df$oldbalanceOrg <- as.integer(df$oldbalanceOrg)
df$oldbalanceDest <- as.integer(df$oldbalanceDest)
df$newbalanceDest <- as.integer(df$newbalanceDest)
df$isFraud <- as.factor(df$isFraud)
df$isFlaggedFraud <- as.factor(df$isFlaggedFraud)
df$type<-as.factor(df$type)
df$newbalanceDest <- as.integer(df$newbalanceDest)
df$newbalanceOrig <- as.integer(df$newbalanceOrig)
df$oldbalanceDest <- as.integer(df$oldbalanceDest)



object.size(train)
train <- select(train,-1,-12)
load("train.csv")

#Afficher les modalités et leurs effectifs



View(train)
str(train)

#Transtypage de colonnes, afin de réduire l'espace du df et mettre un type correspondant

train$amount <- as.integer(train$amount)
train$oldbalanceOrg <- as.integer(train$oldbalanceOrg)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$isFraud <- as.factor(train$isFraud)
train$isFlaggedFraud <- as.factor(train$isFlaggedFraud)
train$type<-as.factor(train$type)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$newbalanceOrig <- as.integer(train$newbalanceOrig)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)



#Nous allons ensuite effectuer une exploration des données plus precise pour chaque colonne
#afin de savoir si celle-ci peut nous apporter de l'information ou pas. 
#Vu que nous travaillons avec un jeu de données assez massif, il est important d'essayer d'enlever 
#les informations qui ne sont pas nécéssaires pour entrainer nos modèles.


#Vérification sur les noms (nameOrig et nameDest)

table(train$nameDest)
table(train$nameOrig)

#On s'aperçoit que la colonne nameDest possède plusieurs doublons, ont pourrait donc pense qu'il serait
#intéressant de gardes les colonnes des noms, mais quand on execute la même commande sur nameOrig
#donc le nom du destinataire de la transaction, ne semble pas avoir de doublons, ce que nous indique
#qu'il y aurait pas de corrélation entre les destinataires et les fraudes. 
#De ce fait nous allons enlever la colonne des noms de notre jeu de donné d'entrainement.

#Nous allons aussi enlever les colonnes id et isFlaggedFraud, car comme nous souhaitons construire
#notre modèle du 0 nous voulons pas que celui-ci se base sur un autre modèle précédant.


#-----------------------------------------#


#Nous allons continuer notre exploration des données

sum(is.na(train))
#Le jeu de données n'as pas de valeurs manquantes

#Histogramme Fraudes et Non-Fraudes

plot(train$isFraud, 
     main = "Taux de fraudes et non-fraudes", 
     sub = "0=Non-Fraude   1=Fraude")
  
#Le jeu de données est déséquilibré, il faudra donc utilisé des modèles adaptés (ex: arbres de décision, xgboost...)

#--------------------------------------#

#On va voir comment sont reparties les données en fonction si c'est des fraudes ou pas
#pour cela, nous allons créer des sous jeux de données

fraude_f = train[train$isFraud == 0,] 
fraude_t = train[train$isFraud == 1,] 


#--------------------------------------#

#Temps des transactions

ggplot()+geom_density(data = fraude_t,aes(x = fraude_t$step),color="red",
                      fill="red",alpha=0.5)+
  geom_density(data = fraude_f,aes(x = fraude_f$step),color="blue",fill="blue",
               alpha=0.55)+
  ggtitle("Comparaison temps des transactiosn frauduleuses ou pas")+
  xlab("Step")+
  ylab("Densité")

#1ère hypothèse : les transactions avec plus de 400 step (heures) ont plus de chance d'être des fraudes

#-----------------------------#

#Montant des transactions

#Comparaison des 2
ggplot()+geom_density(data = fraude_t,aes(x = fraude_t$amount),color="red",
                      fill="red",alpha=0.2)+
  geom_density(data = fraude_f,aes(x = fraude_f$amount),color="blue",fill="blue",
               alpha=0.5)+
  ggtitle("Comparaison du montant des transactions frauduleuses ou pas")+
  xlab("Amount")+
  ylab("Densité") 

#Montant des transactions frauduleuses
ggplot()+geom_density(data = fraude_t,aes(x = fraude_t$amount),color="red",
                      fill="red",alpha=0.2)

#Montant des transactions normales
ggplot()+geom_density(data = fraude_f,aes(x = fraude_f$amount),color="blue",fill="blue",
                      alpha=0.5)

#------------------------------#

#Matrice de corrélation entre les variables

ggcorr(train,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "gray50")

#------------------------------#
#Les types de transaction

table(train$type) 

#Types de transactions 


ggplot(train, aes(x = isFraud, fill = type)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count',
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_classic()


plot(typ_fraude_f, col = "blue",
     main = "Types des transactions normales")

plot(typ_fraude_t, col = "red",
     main = "Types des transactions frauduleuses")

table(typ_fraude_t)

#Toutes les transactions frauduleuses sont de type "CASH OUT" ou "TRANSER"

#-------------------------------#


#Conséquence de notre exploration des données, nous venons d'observer que toute les transactions frauduleuse
#sont de type "CASH_OUT" ou "TRANSFER" sur plus de 2 millions de transactions. Nous pouvons donc tracer 
#la l'hypothèse basé sur ces +2 millions de transaction que toutes les fraudes sont de ces 2 types.
#Donc aussi dans le but de réduire notre jeu de donnée nous allons garder pour notre modèle d'entraiment
#que les transactions de ces 2 types correspondants


object.size(train)
train = subset(train, type == "CASH_OUT" | type == "TRANSFER" )

#Nous pouvons vérifier que notre jeu de données continue toujours déséquilibré

#passons à la création des modèles
#------------------------------------------------#


#----------------------Les modèles-------------------

library(MLmetrics)

#------------------------------------ARBRE DE DÉCISION-----------

View(train)


library(rpart)
library(rpart.plot)

#------Préparation des donnees-----#

load('data/processed/train.csv')

train <- select(train, -1, -5, -8, -12)

train$amount <- as.integer(train$amount)
train$type <- as.factor(train$type)
train$oldbalanceOrg <- as.integer(train$oldbalanceOrg) 
train$newbalanceOrig <- as.integer(train$newbalanceOrig)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$isFraud <- as.factor(train$isFraud)

#-------------------Construction du modèle-------------#

dtree <- rpart(isFraud ~ ., data = train, method = 'class')
summary(dtree)

#--------------Evaluation du modèle-----------#

load('data/processed/test.csv')
load('models/decision_tree_FINAL')

dtree_y_pred <- predict(dtree, test, type = 'class')
y_true <- test$isFraud

dtree_precision <- Precision(y_true, dtree_y_pred, positive = 1)
dtree_recall <- Recall(y_true, dtree_y_pred, positive = 1)
dtree_f1 <- F1_Score(y_true, dtree_y_pred, positive = 1)
dtree_err <- mean(y_true != dtree_y_pred)

#Nous avons un f1_score de 0,81, une precision de 0,96 et un taux de recall de 0,71
#et aussi taux d'erreur de 0,002, ce modèle est donc à retenir  

rpart.plot(dtree, extra = 106)

roc.curve(dtree_y_pred, dtree_y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)


#---------------------Fine tuning arbre de decision


control <- rpart.control(minsplit = 5,
                         minbucket = 2,
                         maxdepth = 8,
                         cp = 0)
dtree_tuned_fit <- rpart(isFraud ~ ., data = train, method = 'class', control = control)

y_pred <- predict(dtree_tuned_fit, validation, type = 'class')

dtree_tuned_fit_precision <- Precision(y_true, y_pred, positive = 1)
dtree_tuned_fit_recall <- Recall(y_true, y_pred, positive = 1)
dtree_tuned_fit_f1 <- F1_Score(y_true, y_pred, positive = 1)



rpart.plot(dtree_tuned_fit, extra = 106)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)


y_pred <- predict(dtree_tuned_fit, validation, type = 'class')


dtree_tuned_fit_precision <- Precision(y_true, y_pred, positive = 1)
dtree_tuned_fit_recall <- Recall(y_true, y_pred, positive = 1)
dtree_tuned_fit_f1 <- F1_Score(y_true, y_pred, positive = 1)



#---------------------------------------------------#

#-------------------------------------REGRESSION LOGISTIQUE---------------------


rm(list = ls())

#------Préparation des donnees-----#

load('data/processed/train.csv')

train <- select(train, -1, -5, -8, -12)

train$amount <- as.integer(train$amount)
train$type <- as.factor(train$type)
train$oldbalanceOrg <- as.integer(train$oldbalanceOrg) 
train$newbalanceOrig <- as.integer(train$newbalanceOrig)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$isFraud <- as.factor(train$isFraud)


#-------------------Construction du modèle-------------#


glm_fit <- glm(isFraud ~ ., data = train, family = binomial(logit))
summary(glm_fit)

#--------------Evaluation du modèle-----------#

load('data/processed/test.csv')


y_true <- test$isFraud

logit_y_pred <- predict(glm_fit,test, type = 'response')
logit_y_pred <- as.factor(ifelse(y_pred > 0.5, 1, 0))

logit_precision <- Precision(y_true, y_pred, positive = 1)
logit_recall <- Recall(y_true, y_pred, positive = 1)
logit_f1 <- F1_Score(y_true, y_pred, positive = 1)

#---------------------------------------------#



#------Classification naïve bayésienne---------------------#

rm(list = ls())

#------------Preparation des données---------------#

load('data/processed/train.csv')

train <- select(train, -1, -5, -8, -12)

train$amount <- as.integer(train$amount)
train$type <- as.factor(train$type)
train$oldbalanceOrg <- as.integer(train$oldbalanceOrg) 
train$newbalanceOrig <- as.integer(train$newbalanceOrig)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$isFraud <- as.factor(train$isFraud)

#------------------Construction du modele------------------#

nbClassifier <- naiveBayes(isFraud ~., data = train)
BNaif <- nbClassifier

summary(BNaif)

#--------------Evaluation du modèle-----------#

load('data/processed/test.csv')

nb_y_pred <- predict(BNaif, test)
y_true <- test$isFraud

BNaif_precision <- Precision(y_true, nb_y_pred, positive = 1)
BNaif_recall <- Recall(y_true, nb_y_pred, positive = 1)
BNaif_f1 <- F1_Score(y_true, nb_y_pred, positive = 1)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)

#---------------------------------------XGBOOST------------------

library(xgboost)

#------------Preparation des données---------------#


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

xgb <- xgboost(data = X_train, 
               label = y_train,
               eta = 0.1,
               gamma = 0.1,
               max_depth = 10, 
               nrounds = 100, 
               objective = "binary:logistic",
               colsample_bytree = 0.6,
               verbose = 1,
               nthread = 7,
               set.seed(777)
)



xgb <- xgboost(data = X_train, label = y_train,
        max.depth = 6, eta = 1, nthread = 2, nrounds = 20,
        eval_metric = "f1_score",
        objective = "binary:logistic")

summary(xgb)


xgb.plot.shap(data = X_train,
              model = xgb,
              top_n = 3)

#--------------Evaluation du modèle-----------#

load('data/processed/test.csv')

test <- select(test, -1, -3, -5, -8, -12)


test$amount <- as.integer(test$amount)
test$oldbalanceOrg <- as.integer(test$oldbalanceOrg) 
test$newbalanceOrig <- as.integer(test$newbalanceOrig)
test$oldbalanceDest <- as.integer(test$oldbalanceDest)
test$newbalanceDest <- as.integer(test$newbalanceDest)



X_test = data.matrix(test[,-7])
y_test = as.numeric(test$isFraud)


xgboost_test = xgb.DMatrix(data=X_test, label=y_test)



xgb_pred <- predict(xgb, xgboost_test)

y_true <- test$isFraud


xgb_precision <- Precision(y_true, xgb_pred, positive = 1)
xgb_recall <- Recall(y_true, xgb_pred, positive = 1)
xgb_f1 <- F1_Score(y_true, xgb_pred, positive = 1)


roc.curve(xgb_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)

#ARBRE DE DECISION

library(rpart)
library(MLmetrics)

set.seed(777)

#LE MODELE
dtree <- rpart(isFraud ~ ., data = train, method = 'class')

#EVALUATION DU MODELE
y_pred <- predict(dtree, train, type = 'class')
y_true <- train$isFraud

dtree_precision <- Precision(y_true, y_pred, positive = 1)
dtree_recall <- Recall(y_true, y_pred, positive = 1)
dtree_f1 <- F1_Score(y_true, y_pred, positive = 1)

paste0("Precision: ", dtree_precision)
paste0("Recall: ", dtree_recall)
paste0("F1 Score: ", dtree_f1)


#COURBE ROC "arbre de decision"
library(ROSE)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)

 