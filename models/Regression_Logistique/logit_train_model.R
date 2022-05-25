#CREATION REGRESSION LOGISTIQUE--------------------#

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

#----------Construction du modèle-------------#


glm_fit <- glm(isFraud ~ ., data = train, family = binomial(logit))
summary(glm_fit)

