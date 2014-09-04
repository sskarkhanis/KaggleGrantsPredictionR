##########################################################################################################
#FNN -> fast nearest neighbours
#the knnx function requires all indicators to be numeric so we first convert our data
trainKNN <- data.frame(sapply(training_grants, function(x) as.numeric(as.character(x))))
testKNN <- data.frame(sapply(test_grants, function(x) as.numeric(as.character(x))))
#example for 10 nearest neighbors:

k=10
#retrieve the indicators of the k nearest neighbors of the query data 
indicatorsKNN <- as.integer(knnx.index(data=trainKNN, query=test_grants, k=k))
#retrieve the actual y from the tarining set
predKNN <- as.integer(as.character(yTRAIN[indicatorsKNN]))
#if k > 1 than we take the proportion of 1s
predKNN <- rowMeans(data.frame(matrix(data=predKNN,ncol=k,nrow=nrow(testKNN))))

#if you want to tune
#tuning comes down to evaluating which value for k is best
auc <- numeric()
for (k in 1:nrow(trainKNN)) {
  #retrieve the indicators of the k nearest neighbors of the query data 
  indicatorsKNN <- as.integer(knnx.index(data=trainKNN, query=testKNN, k=k)) #get all neighbours
  #retrieve the actual y from the tarining set
  predKNN <- as.integer(as.character(yTRAIN[indicatorsKNN])) #get characteristics of neighbours
  #if k > 1 than we take the proportion of 1s
  predKNN <- rowMeans(data.frame(matrix(data=predKNN,ncol=k,nrow=nrow(testKNN)))) #take mean of the char
  
  #COMPUTE AUC 
  auc[k] <- AUC::auc(roc(predKNN,yTEST))
  print(k)
}

plot(1:nrow(trainKNN),auc,type="l")


k=which.max(auc)
# k#retrieve the indicators of the k nearest neighbors of the query data 
# indicatorsKNN <- as.integer(knnx.index(data=trainKNN, query=testKNN, k=k))
# #retrieve the actual y from the tarining set
# predKNNoptimal <- as.integer(as.character(yTRAIN[indicatorsKNN]))
# #if k > 1 than we take the proportion of 1s
# predKNNoptimal <- rowMeans(data.frame(matrix(data=predKNNoptimal,ncol=k,nrow=nrow(testKNN))))

##########################################################################################################
#SVM
library(e1071)
source("tuneMember.R")

#tuning grid
SV.cost <-  2^(-5:-3) # 2^(-5:13)
SV.gamma <-  2^(-15:-14) #2^(-15:3)
SV.degree <-  c(1,2,3)
SV.kernel <- c('radial','polynomial')

result <- list()

for (i in SV.kernel) {
  call <- call("svm", formula = as.factor(yTRAIN) ~ ., data=training_grants, type = "C-classification",probability=TRUE)
  #the probability model for classification fits a logistic distribution using maximum likelihood to the decision values of the binary classifier
  
  if (i=='radial'  ) tuning <- list(gamma = SV.gamma, cost = SV.cost, kernel='radial')
  if (i=='polynomial') tuning <- list(gamma = SV.gamma, cost = SV.cost, degree=SV.degree, kernel='polynomial')
  
  
  #tune svm
  result[[i]] <- tuneMember(call=call,
                            tuning=tuning,
                            xtest=test_grants,
                            ytest=yTEST,
                            probability=TRUE)
  
}
auc <- numeric()
for (i in 1:length(result)) auc[i] <- result[[i]]$auc

result <- result[[which.max(auc)]]

SV <- svm(as.factor(yTRAIN) ~ ., data = training_grants,
          type = "C-classification", kernel = as.character(result$kernel), degree= if (is.null(result$degree)) 3 else result$degree, 
          cost = result$cost, gamma = if (is.null(result$gamma)) 1 / ncol(xTRAIN) else result$gamma , probability=TRUE)

#Compute the predictions for the test set
predSV <- as.numeric(attr(predict(SV,test_grants, probability=TRUE),"probabilities")[,2])

##########################################################################################################
#GLM: Generalized Linear Models = Logistic Regression
(LR <- glm(factor(yTRAIN) ~ ., data=training_grants, family=binomial("logit")))

#stepwise variable selection
(LRstep <- step(LR, direction="forward", trace = FALSE))

#Use the model to make a prediction on test data.
predLRstep <- predict(LRstep, newdata=test_grants, type="response")

# REGULARIZED LOGISTIC REGRESSION

#Regularization refers to the introduction of a penalty for complexity in order to avoid overfitting
#This boils down to keeping all variables in the equation but shrinking their coefficients towards 0. 
#regularization is often called shrinkage or lasso (least absolute shrinkage and selection operator)
#More concretely, we set a bound on the sum of the absolute values of the coefficients

#logistic regression
(LR <-  glmnet(x=data.matrix(training_grants), y=yTRAIN, family="binomial"))


coef(LR) #for every lambda there is a set of coefficients. The number of active variables can also be found by looking at LR

#?plot.glmnet
#plot(LR) #L1 norm= sum of the absolute values of the coefficients
#plot(LR,xvar="lambda") #Bigger lambda results in more coefficients shrunk to zero. min(LR$lambda) max(LR$lambda)

#cross- validate lambda
#aucstore <- numeric()

# for (i in 1:length(LR$lambda) ) {
#   predglmnet <- predict(LR,newx=data.matrix(test_grants),type="response",s=LR$lambda[i])
#   aucstore[i] <- AUC::auc(roc(as.numeric(predglmnet),yTEST))
# }
# plot(1:length(LR$lambda),aucstore,type="l")       
# LR.lambda <- LR$lambda[which.max(aucstore)]

predLRlas <- as.numeric(predict(LR,newx=data.matrix(test_grants),type="response"))
#plot(LR,aucstore,type="l") 

###########################################################################################################
##########################################################################################################
#BOOSTING

#?ada
ABmodel <- ada(yTRAIN ~ . ,training_grants,iter=500)
predAB <- as.numeric(predict(ABmodel,test_grants,type="probs")[,2])

#plotting learning curve
#ABmodel <- ada(x=BasetableTRAIN[,names(BasetableTRAIN)!= "Grant.Status"],y=BasetableTRAIN$Grant.Status,test.x=BasetableTEST[,names(BasetableTEST)!= "Grant.Status"],test.y=BasetableTEST$Grant.Status,iter=50)
ABmodel <- ada(x=training_grants ,y=yTRAIN,test.x=test_grants ,test.y=yTEST,iter=500)

plot(ABmodel,test=TRUE)

#variable importances
#Same as in single decision tree but now aggregated across all trees: sum of the decrease in impurity for each of the surrogate variables at each node
varplot(ABmodel)
#getting them in a character string
varplot(ABmodel,type="scores")
summary(ABmodel)


##########################################################################################################
#RANDOM FOREST

rFmodel <- randomForest(x=training_grants ,y=factor(yTRAIN),  ntree=500, importance=TRUE)

#look at the importance of the variables
importance(rFmodel)
varImpPlot(rFmodel)
#For more imformation about the measures: ?importance

#prediction
#?predict.randomForest
predrF <- predict(rFmodel,test_grants,type="prob")[,2]

#plotting learing curve
head(plot(rFmodel))



##########################################################################################################
#PERFORMANCE EVALUATION

#AUC and ROC curve of AdaBoost model

(AUC_adaboost <- performance(prediction (predAB ,yTEST),"auc")@y.values[[1]])

plot(performance(prediction(predAB ,yTEST), "tpr", "fpr"))


#AUC and ROC curve of Random Forest model
(AUC_RF <- performance(prediction(predrF ,yTEST),"auc")@y.values[[1]])

plot(performance(prediction(predrF ,yTEST), "tpr", "fpr"))

plot(performance(prediction(predAB ,yTEST), "tpr", "fpr"),add=TRUE,col="red")
plot(performance(prediction(predrF ,yTEST), "tpr", "fpr"),add=TRUE,col="green")

#?legend
legend("bottomright",legend=c("AdaBoost","Random Forest"),col=c( "red","green"),lwd=1)

auc(roc(predAB,yTEST))
plot(roc(predAB,yTEST))
plot(roc(predrF,BYTEST),add=TRUE,col="blue")
legend("bottomright",legend=c("AdaBoost","Random Forest"),col=c("black","blue"),lwd=1)

AUC::auc(roc(predKNN,yTEST))
AUC::auc(roc(predKNNoptimal,yTEST))
AUC::auc(roc(predLRstep,yTEST))
AUC::auc(roc(predLRlas,yTEST))
AUC::auc(roc(predSV,yTEST))
AUC::auc(roc(predNN,yTEST))
AUC::auc(roc(predKF,yTEST))

plot(roc(predKNN,yTEST))
plot(roc(predKNNoptimal,yTEST),add=TRUE,col="blue")
plot(roc(predLRstep,yTEST),add=TRUE,col="red")
plot(roc(predLRlas,yTEST),add=TRUE,col="green")
plot(roc(predSV,yTEST),add=TRUE,col="yellow")
plot(roc(predNN,yTEST),add=TRUE,col="orange")
plot(roc(predKF,yTEST),add=TRUE,col="brown")

legend("bottomright",legend=c("KNN","KNNopt","LRstep","LRlasso","SVM","NNet"),col=c("black","blue","red","green","yellow","orange"),lwd=1)
