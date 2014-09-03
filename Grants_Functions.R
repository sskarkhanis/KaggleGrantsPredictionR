calculate_percentages <-function(training_grants,vars)
{
  only_vars<-training_grants[,vars,with=FALSE]
  N<-dim(only_vars)[1]
  m<-matrix(nrow = dim(only_vars)[1],ncol = dim(only_vars)[2]+1)
  for(g in 1:N)
  {
    ids<-only_vars[g,seq(1,9,2),with=FALSE]
    percentages<-only_vars[g,seq(2,10,2),with=FALSE]
    if(!any(is.na(percentages)))
    {
      p<-as.numeric(percentages)
      i<-as.numeric(ids)
      o<-order(p,decreasing = TRUE)
      reordered_p<-p[o]
      reordered_i<-i[o]
      m[g,1:5]<-reordered_p
      m[g,6:10]<-reordered_i
      m[g,11]<-sum(p>0)
      if(sum(reordered_p)>100.0)
      {
        print(paste('error at line',g,': sums to more than 100'))
      }
      check_ids<-sum(i>0)
      if(m[g,11]<1)
      {
        print(paste('error at line',g,': all % equal to 0'))
      }
      if(m[g,11]!=check_ids)
      {
        print(g)
        print(p)
        print(i)
        print(paste('error at line',g,': different percentages and ids different from 0'))
      }
    }
  }
  return(m)
}

update_grants <-function(grants,m,name)
{
  p<-paste0(name,'.Percentage.',1:5,':=m[,',1:5,']')
  l<-lapply(p,function(x)parse(text=x))
  lapply(l,function(x)grants[,eval(x)])
  p<-paste0(name,'.Code.',1:5,':=m[,',6:10,']')
  l<-lapply(p,function(x)parse(text=x))
  lapply(l,function(x)grants[,eval(x)])
  size_name<-paste0(name,'.Size:=m[,11]')
  grants[,eval(parse(text=size_name))]
}