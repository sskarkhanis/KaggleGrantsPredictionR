
#Before starting the analyses, clean up by removing all objects except the Basetable
#rm(list=setdiff(ls(),"Basetable")) #setdiff returns which elements of the first argument are not in the second argument
ls()

##########################################################################################################
#LOADING REQUIRED PACKAGES
set.seed(108)

pckg2 = c('ada','randomForest','e1071','ROCR','pROC')

install.packages(pckg,dependencies=T)

## Load required libraries
require(ada)
require(randomForest)
require(e1071)
require(ROCR) 
require(pROC)


if(require('AUC')==FALSE)  install.packages('AUC',repos="http://www.freestatistics.org/cran/"); require('AUC')
if(require('FNN')==FALSE)  install.packages('FNN',repos="http://www.freestatistics.org/cran/"); require('FNN')
if(require('glmnet')==FALSE)  install.packages('glmnet',repos="http://www.freestatistics.org/cran/"); require('glmnet')
if(require("nnet")==FALSE) install.packages("nnet") ; library(nnet)


##########################################################################################################
#Data Splitting
Basetable <- grants
#class(Basetable) <- 'data.frame'
#randomize order of indicators
Basetable$Start.date<-as.Date(Basetable$Start.date,format="%d/%m/%y")

#splitting basetable into training & test based on start date
#data with start date < 2008 goes to training_grants & rest to test_grants
training_grants<-Basetable[year(Start.date)<2008,]
test_grants<-Basetable[year(Start.date)==2008,]

# training_inv<-invT[Grant.Application.ID %in%training_grants$Grant.Application.ID,]

dim(training_grants)
dim(test_grants)

allind <- sample(1:nrow(training_grants),nrow(training_grants))

#split in three parts 
trainind <- allind[1:round(length(allind)/3)]
valind   <- allind[(round(length(allind)/3)+1):round(length(allind)*(2/3))]
testind  <- allind[round(length(allind)*(2/3)+1):length(allind)]

#testind  <- allind[-trainind]

BasetableTRAIN <- training_grants[trainind,] ; class(BasetableTRAIN) <- 'data.frame'
BasetableVAL   <- training_grants[valind,] ; class(BasetableVAL) <- 'data.frame'
BasetableTEST  <- training_grants[testind,] ; class(BasetableTEST) <- 'data.frame'

#isolate the response variable
yTRAIN <- BasetableTRAIN$Grant.Status
#BasetableTRAIN[,Grant.Status:=NULL] 
BasetableTRAIN$Grant.Status <- NULL

yVAL <- BasetableVAL$Grant.Status
#BasetableVAL[,Grant.Status:=NULL] 
BasetableVAL$Grant.Status <- NULL

yTEST <- BasetableTEST$Grant.Status
#BasetableTEST[,Grant.Status:=NULL] 
BasetableTEST$Grant.Status <- NULL

table(yTRAIN);table(yVAL);table(yTEST)

#if no tuning is required than we will use TRAIN + VAL as training set
BasetableTRAINbig <- rbind(BasetableTRAIN,BasetableVAL)
yTRAINbig <- factor(c(as.integer(as.character(yTRAIN)),as.integer(as.character(yVAL))))

#check whether we didn't make a mistake
dim(BasetableTRAIN)
dim(BasetableVAL)
dim(BasetableTEST)

# ?intersect
# intersect(rownames(BasetableTRAIN),rownames(BasetableTEST))
# intersect(rownames(BasetableTRAIN),rownames(BasetableVAL))
# intersect(rownames(BasetableTEST) ,rownames(BasetableVAL))
#intersection is empty so we can proceed


##########################################################################################################
#BOOSTING

#?ada
ABmodel <- ada(yTRAIN ~ . ,BasetableTRAIN,iter=500)
predAB <- as.numeric(predict(ABmodel,BasetableTEST,type="probs")[,2])

#plotting learning curve
#ABmodel <- ada(x=BasetableTRAIN[,names(BasetableTRAIN)!= "Grant.Status"],y=BasetableTRAIN$Grant.Status,test.x=BasetableTEST[,names(BasetableTEST)!= "Grant.Status"],test.y=BasetableTEST$Grant.Status,iter=50)
ABmodel <- ada(x=BasetableTRAIN ,y=yTRAIN,test.x=BasetableTEST ,test.y=yTEST,iter=500)

plot(ABmodel,test=TRUE)

#variable importances
#Same as in single decision tree but now aggregated across all trees: sum of the decrease in impurity for each of the surrogate variables at each node
varplot(ABmodel)
#getting them in a character string
varplot(ABmodel,type="scores")
summary(ABmodel)


##########################################################################################################
#RANDOM FOREST

rFmodel <- randomForest(x=BasetableTRAIN ,y=factor(yTRAIN),  ntree=10, importance=TRUE)

#look at the importance of the variables
importance(rFmodel)
varImpPlot(rFmodel)
#For more imformation about the measures: ?importance

#prediction
?predict.randomForest
predrF <- predict(rFmodel,BasetableTEST[,names(BasetableTEST)!= "Grant.Status"],type="prob")[,2]

#plotting learing curve
head(plot(rFmodel))

##########################################################################################################
#kNN: K-Nearest Neighbors

#FNN -> fast nearest neighbours
#the knnx function requires all indicators to be numeric so we first convert our data
trainKNN <- data.frame(sapply(BasetableTRAIN, function(x) as.numeric(as.character(x))))
#trainKNNbig <- data.frame(sapply(BasetableTRAINbig, function(x) as.numeric(as.character(x))))   
valKNN <- data.frame(sapply(BasetableVAL, function(x) as.numeric(as.character(x))))
testKNN <- data.frame(sapply(BasetableTEST, function(x) as.numeric(as.character(x))))

#note that all computations take place in the prediction phase

#example for 10 nearest neighbors:
k=10
#retrieve the indicators of the k nearest neighbors of the query data 
indicatorsKNN <- as.integer(knnx.index(data=BasetableTRAINbig, query=BasetableTEST, k=k))
#retrieve the actual y from the tarining set
predKNN <- as.integer(as.character(yTRAINbig[indicatorsKNN]))
#if k > 1 than we take the proportion of 1s
predKNN <- rowMeans(data.frame(matrix(data=predKNN,ncol=k,nrow=nrow(testKNN))))


#if you want to tune
#tuning comes down to evaluating which value for k is best
auc <- numeric()
for (k in 1:nrow(trainKNN)) {
  #retrieve the indicators of the k nearest neighbors of the query data 
  indicatorsKNN <- as.integer(knnx.index(data=trainKNN, query=valKNN, k=k)) #get all neighbours
  #retrieve the actual y from the tarining set
  predKNN <- as.integer(as.character(yTRAIN[indicatorsKNN])) #get characteristics of neighbours
  #if k > 1 than we take the proportion of 1s
  predKNN <- rowMeans(data.frame(matrix(data=predKNN,ncol=k,nrow=nrow(valKNN)))) #take mean of the char
  
  #COMPUTE AUC 
  auc[k] <- AUC::auc(roc(predKNN,yVAL))
  print(k)
}

plot(1:nrow(trainKNN),auc,type="l")
#very low values of k result in a very flexible classifier: low bias, high variance
#very high values of k result a very inflexbile classifier: high bias, low variance
#when k equals the the number of training instances than all response values are selected once per new (i.e., validation) data point. 
#Then all values of predKNN will have mean(as.integer(as.character(yTRAIN))).

#the next step would be to train again on trainKNNbig using the best value of k and predict on testKNN


k=which.max(auc)
k#retrieve the indicators of the k nearest neighbors of the query data 
indicatorsKNN <- as.integer(knnx.index(data=trainKNNbig, query=testKNN, k=k))
#retrieve the actual y from the tarining set
predKNNoptimal <- as.integer(as.character(yTRAINbig[indicatorsKNN]))
#if k > 1 than we take the proportion of 1s
predKNNoptimal <- rowMeans(data.frame(matrix(data=predKNNoptimal,ncol=k,nrow=nrow(testKNN))))

##########################################################################################################
#GLM: Generalized Linear Models = Logistic Regression

#Method 1: LOGISTIC REGRESSION WITH STEPWISE VARIABLE SELECTION.


(LR <- glm(yTRAINbig ~ ., data=BasetableTRAINbig, family=binomial("logit")))

#stepwise variable selection
(LRstep <- step(LR, direction="both", trace = FALSE))

#Use the model to make a prediction on test data.
predLRstep <- predict(LRstep, newdata=BasetableTEST, type="response")

#Method 2: REGULARIZED LOGISTIC REGRESSION

#Regularization refers to the introduction of a penalty for complexity in order to avoid overfitting
#This boils down to keeping all variables in the equation but shrinking their coefficients towards 0. 
#regularization is often called shrinkage or lasso (least absolute shrinkage and selection operator)
#More concretely, we set a bound on the sum of the absolute values of the coefficients

#logistic regression
(LR <-  glmnet(x=data.matrix(BasetableTRAIN), y=yTRAIN, family="binomial"))


coef(LR) #for every lambda there is a set of coefficients. The number of active variables can also be found by looking at LR

#?plot.glmnet
plot(LR) #L1 norm= sum of the absolute values of the coefficients
plot(LR,xvar="lambda") #Bigger lambda results in more coefficients shrunk to zero. min(LR$lambda) max(LR$lambda)

#cross- validate lambda
aucstore <- numeric()

##########################################################################################################
#SVM: Support Vector Machines
source("tuneMember.R")

#tuning grid
SV.cost <-  2^(-5:-3) # 2^(-5:13)
SV.gamma <-  2^(-15:-14) #2^(-15:3)
SV.degree <-  c(1,2,3)
SV.kernel <- c('radial','polynomial')

result <- list()

for (i in SV.kernel) {
  call <- call("svm", formula = as.factor(yTRAIN) ~ ., data=BasetableTRAIN, type = "C-classification",probability=TRUE)
  #the probability model for classification fits a logistic distribution using maximum likelihood to the decision values of the binary classifier
  
  if (i=='radial'  ) tuning <- list(gamma = SV.gamma, cost = SV.cost, kernel='radial')
  if (i=='polynomial') tuning <- list(gamma = SV.gamma, cost = SV.cost, degree=SV.degree, kernel='polynomial')
  
  
  #tune svm
  result[[i]] <- tuneMember(call=call,
                            tuning=tuning,
                            xtest=BasetableVAL,
                            ytest=yVAL,
                            probability=TRUE)
  
}
auc <- numeric()
for (i in 1:length(result)) auc[i] <- result[[i]]$auc

result <- result[[which.max(auc)]]

SV <- svm(as.factor(yTRAINbig) ~ ., data = BasetableTRAINbig,
          type = "C-classification", kernel = as.character(result$kernel), degree= if (is.null(result$degree)) 3 else result$degree, 
          cost = result$cost, gamma = if (is.null(result$gamma)) 1 / ncol(xTRAIN) else result$gamma , probability=TRUE)

#Compute the predictions for the test set
predSV <- as.numeric(attr(predict(SV,BasetableTEST, probability=TRUE),"probabilities")[,2])

for (i in 1:length(LR$lambda) ) {
  predglmnet <- predict(LR,newx=data.matrix(BasetableVAL),type="response",s=LR$lambda[i])
  aucstore[i] <- AUC::auc(roc(as.numeric(predglmnet),yVAL))
}
plot(1:length(LR$lambda),aucstore,type="l")       
LR.lambda <- LR$lambda[which.max(aucstore)]


#Create final model
LR <-  glmnet(x=data.matrix(BasetableTRAINbig), y=yTRAINbig, family="binomial")


predLRlas <- as.numeric(predict(LR,newx=data.matrix(BasetableTEST),type="response",s=LR.lambda))

###########################################################################################################
#NN: Neural Networks


#first we need to scale the data to range [0,1] avoid numerical problems
BasetableTRAINnumID <- sapply(BasetableTRAIN, is.numeric)
BasetableTRAINnum <- BasetableTRAIN[, BasetableTRAINnumID]

minima <- sapply(BasetableTRAINnum,min)
scaling <- sapply(BasetableTRAINnum,max)-minima
#?scale
#center is subtracted from each column. Because we use the minima this sets the minimum to zero.
#scale: each column is divided by scale. Because we use the range this sets the maximum to one.
BasetableTRAINscaled <- data.frame(base::scale(BasetableTRAINnum,center=minima,scale=scaling), BasetableTRAIN[,!BasetableTRAINnumID])
colnames(BasetableTRAINscaled) <-  c(colnames(BasetableTRAIN)[BasetableTRAINnumID], colnames(BasetableTRAIN)[!BasetableTRAINnumID])
#check
sapply(BasetableTRAINscaled,range)

NN.rang <- 0.5 #the range of the initial random weights parameter
NN.maxit <- 10000 #set high in order not to run into early stopping
NN.size <- c(5,10,20) #number of units in the hidden layer
NN.decay <- c(0,0.001,0.01,0.1) #weight decay. Same as lambda in regularized LR. Controls overfitting

call <- call("nnet", formula = yTRAIN ~ ., data=BasetableTRAINscaled, rang=NN.rang, maxit=NN.maxit, trace=FALSE, MaxNWts= Inf)
tuning <- list(size=NN.size, decay=NN.decay)

#tune nnet
#scale validation data              
BasetableVALIDATEnum <- BasetableVAL[, BasetableTRAINnumID]
BasetableVALIDATEscaled <- data.frame(base::scale(BasetableVALIDATEnum,center=minima,scale=scaling), BasetableVAL[,!BasetableTRAINnumID])
colnames(BasetableVALIDATEscaled) <- colnames(BasetableTRAINscaled)

result <- tuneMember(call=call,
                     tuning=tuning,
                     xtest=BasetableVALIDATEscaled,
                     ytest=yVAL,
                     predicttype="raw")



#Create final model

BasetableTRAINbignum <- BasetableTRAINbig[, BasetableTRAINnumID]
BasetableTRAINbigscaled <- data.frame(base::scale(BasetableTRAINbignum,center=minima,scale=scaling), BasetableTRAINbig[,!BasetableTRAINnumID])
colnames(BasetableTRAINbigscaled) <-  c(colnames(BasetableTRAINbig)[BasetableTRAINnumID], colnames(BasetableTRAINbig)[!BasetableTRAINnumID])

NN <- nnet(yTRAINbig ~ ., BasetableTRAINbigscaled, size = result$size, rang = NN.rang, decay = result$decay, maxit = NN.maxit, trace=FALSE, MaxNWts= Inf)

#predict on test
BasetableTESTbignum <- BasetableTEST[, BasetableTRAINnumID]
BasetableTESTscaled <- data.frame(base::scale(BasetableTESTbignum,center=minima,scale=scaling), BasetableTESTbignum[,!BasetableTRAINnumID])
colnames(BasetableTESTscaled) <-  c(colnames(BasetableTESTbignum)[BasetableTRAINnumID], colnames(BasetableTESTbignum)[!BasetableTRAINnumID])

predNN <- as.numeric(predict(NN,BasetableTESTscaled,type="raw"))

##########################################################################################################
#PERFORMANCE EVALUATION

#AUC and ROC curve of AdaBoost model

(AUC_adaboost_CV1 <- performance(prediction (predAB ,yTEST),"auc")@y.values[[1]])

plot(performance(prediction(predAB ,yTEST), "tpr", "fpr"))


#AUC and ROC curve of Random Forest model
(AUC_RF <- performance(prediction(predrF ,BasetableTEST$Grant.Status),"auc")@y.values[[1]])

plot(performance(prediction(predrF ,BasetableTEST$Grant.Status), "tpr", "fpr"))

plot(performance(prediction(predAB ,BasetableTEST$Grant.Status), "tpr", "fpr"),add=TRUE,col="red")
plot(performance(prediction(predrF ,BasetableTEST$Grant.Status), "tpr", "fpr"),add=TRUE,col="green")

?legend
legend("bottomright",legend=c("AdaBoost","Random Forest"),col=c( "red","green"),lwd=1)

auc(roc(predAB,BasetableTEST$Grant.Status))
plot(roc(predAB,BasetableTEST$Grant.Status))
plot(roc(predrF,BasetableTEST$Grant.Status),add=TRUE,col="blue")
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
