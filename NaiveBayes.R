library(e1071)
library(pROC)
N<-5 # 5-fold
CV<-generateCV(training_grants,N) # generate the CV-data


results<-list()
auc<-rep(0,N)
# run the crossvalidation
for(cv in seq(N))
{
  #train the model
  bayes <- naiveBayes(Grant.Status ~ Contract.Value.Group+Age, data = CV[[cv]]$training)
  #predict on unseen data
  p<-predict(bayes, CV[[cv]]$test[,list(Contract.Value.Group,Age)], type = "raw")
  #generate roc-data
  r<-roc(CV[[cv]]$test$Grant.Status, p[,2],levels=c("0", "1"))
  #store the results of the run in a list
  results[[cv]]<-list(model=bayes,prediction=p,roc=r)
  auc[cv]<-r$auc
}
