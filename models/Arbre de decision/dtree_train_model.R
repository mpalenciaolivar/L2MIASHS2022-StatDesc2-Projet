
#CREATION ARBRE DE DECISION----------------


load('data/processed/train.csv')

#Séléction de colonnes:

train <- select(train, -1, -5, -8, -12)

#Transtypage des données

train$amount <- as.integer(train$amount)
train$type <- as.factor(train$type)
train$oldbalanceOrg <- as.integer(train$oldbalanceOrg) 
train$newbalanceOrig <- as.integer(train$newbalanceOrig)
train$oldbalanceDest <- as.integer(train$oldbalanceDest)
train$newbalanceDest <- as.integer(train$newbalanceDest)
train$isFraud <- as.factor(train$isFraud)

#-------Construction du modèle-------------#

dtree <- rpart(isFraud ~ ., data = train, method = 'class')
summary(dtree)