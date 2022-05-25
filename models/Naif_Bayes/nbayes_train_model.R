
#CRÉATION CLASSIFIEUR BAYESIEN NAIF-----

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