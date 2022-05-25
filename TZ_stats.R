
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

train_id <- train$id
save(train_id, file = 'train_id.csv')
rm(train_id)

train <- select(train, -1, -5, -8, -12)

#v2

train <- select(train, -1,, -3, -5, -8, -12)

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

table(train$type)
save(paulo, file = "paulo.csv")
object.size(train)

#passons à la création des modèles
#------------------------------------------------#


#----------------------Les modèles----

library(MLmetrics)

#------------------------------------ARBRE DE DÉCISION

View(train)


library(rpart)
library(rpart.plot)


dtree <- rpart(isFraud ~ ., data = train, method = 'class')
save(dtree, file = 'HapinezTree')

load(file = 'data/processed/test.csv')
load(file = 'models/decision_tree_FINAL')
load(file = 'da')

str(test)




y_pred <- predict(dtree, test, type = 'class')
y_true <- test$isFraud

dtree_precision <- Precision(y_true, y_pred, positive = 1)
dtree_recall <- Recall(y_true, y_pred, positive = 1)
dtree_f1 <- F1_Score(y_true, y_pred, positive = 1)

err <- mean(train$isFraud != y_pred)


#Taux d'erreur de plus de 35%, et un F1 score incroyablement bas, notre modèle n'est donc pas à retenir

#On obtient un f1 score plus eleve et un taux d'erreur plus faible quand on enlève la colonne 'type'


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

load('data/processed/validation.csv')
load('models/HapinezTree')


y_pred <- predict(dtree_tuned_fit, validation, type = 'class')


dtree_tuned_fit_precision <- Precision(y_true, y_pred, positive = 1)
dtree_tuned_fit_recall <- Recall(y_true, y_pred, positive = 1)
dtree_tuned_fit_f1 <- F1_Score(y_true, y_pred, positive = 1)



#---------------------------------------------------#

#-------------------------------------REGRESSION LOGISTIQUE


glm_fit <- glm(isFraud ~ ., data = train[,-2], family = 'binomial')


glm_fit <- glm(isFraud ~ type, data = train, family = binomial(logit))

y_pred <- predict(glm_fit,test, type = 'response')
y_pred <- as.factor(ifelse(y_pred > 0.5, 1, 0))

logit_precision <- Precision(y_true, y_pred, positive = 1)
logit_recall <- Recall(y_true, y_pred, positive = 1)
logit_f1 <- F1_Score(y_true, y_pred, positive = 1)

#---------------------------------------------#

#---------------------------------------XGBOOST

install.packages("xgboost")
library(xgboost)

params <- list(eval_metric = "auc",
               objective = "binary:logistic")


xgb <- xgboost(data = as.matrix(train[,-7]),
               label = train$isFraud,
               nrounds = 20,
               verbose = 1)

View(test[-c(1,3,5,8,12)])
test_xgb <- select(test, -1,-3,-5,-8,-12)

View(test_xgb)
str(test_xgb)

test_xgb$amount <- as.integer(test_xgb$amount)
test_xgb$oldbalanceOrg <- as.integer(test_xgb$oldbalanceOrg) 
test_xgb$newbalanceOrig <- as.integer(test_xgb$newbalanceOrig)
test_xgb$oldbalanceDest <- as.integer(test_xgb$oldbalanceDest)
test_xgb$newbalanceDest <- as.integer(test_xgb$newbalanceDest)
test_xgb$isFraud <- as.factor(test_xgb$isFraud)



y_pred <- predict(xgb,as.matrix(test_xgb[,-7]))


xgb_precision <- Precision(y_true, y_pred, positive = 1)
xgb_recall <- Recall(y_true, y_pred, positive = 1)
xgb_f1 <- F1_Score(y_true, y_pred, positive = 1)

save(xgb, file='xgb')



str(train)

summary(glm_fit)

oito <- lm(amount ~ ., data = train3)


dtree <- rpart(isFraud ~., data = train3, method = "class")
rpart.plot(dtree, extra = 100)

save(nbClassifier,file="NaifBayes")
save(dtree,file="Arb_Deci")
save(train,file = "train_version0")




labels <- train$isFraud
y <- recode(labels, '0' = 0, "1" = 1)
xgb <- xgboost(data = data.matrix(credit_card.train[,-31]), 
               label = y,
               eta = 0.1,
               gamma = 0.1,
               max_depth = 10, 
               nrounds = 300, 
               objective = "binary:logistic",
               colsample_bytree = 0.6,
               verbose = 0,
               nthread = 7,
               set.seed(777)
)
xgb_pred <- predict(xgb, data.matrix(credit_card.test))


load('data/processed/train.csv')

id_train <- as.data.frame(train$id)
id_test <- as.data.frame(test$id)
id_valid <- as.data.frame(validation$id)

sum(duplicated(c(id_train, id_test, id_valid)))

save(train, file = 'train.csv')
load('')

train <- select(train, -1, -3, -5, -8, -12)


X_train = data.matrix(train[,-7])
y_train = as.data.frame(train[,7])

str(X_train)
dim(X_train)

View(y_train)

xgboost_train = xgb.DMatrix(data=X_train, label=y_train)

length(y_train)

model <- xgboost(data = as.matrix(train),max.depth=3,nrounds=50) 

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

 