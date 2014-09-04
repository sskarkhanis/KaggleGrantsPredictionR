generateCV <-function(data,num_folds)
{
  years<-c(2005,2006,2007)
  N<-sapply(years,function(y) sum(year(training_grants$Start.date)==y))
  # shuffle the items in each year
  shuffle<-lapply(N,function(n)sample(n,n))
  CV<-num_folds
  T<-floor(N/CV)
  CV_data<-list()
  indices<-rep(1,3)
  # get training and test sets for each cv-run and each year
  for(cv in seq(CV))
  {
    test_sets<-list()
    train_sets<-list()
    for(year in seq(3))
    {
      test<-shuffle[[year]][indices[year]:(indices[year]+T[year]-1)]
      train<-shuffle[[year]][-test]
      test_sets[[year]]<-test
      train_sets[[year]]<-train
      indices[year]<-indices[year]+T[year]
    }
    CV_data[[cv]]<-list(test_sets=test_sets,train_sets=train_sets)
  }
  # join the sets from the three years into one training and test set for each cv-run
  CV_final<-list()
  for(cv in seq(CV))
  {
    training<-CV_data[[cv]]$train_sets[[1]]
    training<-c(training,(CV_data[[cv]]$train_sets[[2]]+N[1]))
    training<-c(training,CV_data[[cv]]$train_sets[[3]]+N[1]+N[2])
    test<-CV_data[[cv]]$test_sets[[1]]
    test<-c(test,(CV_data[[cv]]$test_sets[[2]]+N[1]))
    test<-c(test,CV_data[[cv]]$test_sets[[3]]+N[1]+N[2])
    CV_final[[cv]]<-list(training=data[training,],test=data[test,])
  }
  return(CV_final)
}
