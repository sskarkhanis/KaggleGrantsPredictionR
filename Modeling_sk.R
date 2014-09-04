
#xx <- grants[,c('SEO.DESC.1'),with=F]
#install.packages('dummies',depen=T) ; 
#require(dummies)

tmp_seo  <- vector(length=5,mode='numeric')
tmp_rfcd <- vector(length=5,mode='numeric')
tmp_rfcd.code <- vector(length=5,mode='numeric')
tmp_seo.code <- vector(length=5,mode='numeric')
Basetable <- grants
tmp_Grant.Application.ID <- Basetable$Grant.Application.ID
Basetable[,Grant.Application.ID:=NULL]
tmp_Sponsor.Code <- Basetable$Sponsor.Code
Basetable[,Sponsor.Code:=NULL]
# tmp_Start.date <- Basetable$Start.date
# Basetable[,Start.date:=NULL]

for (i in 1:5)
    {
    tmp_seo[i]  <- grep(paste0('SEO.DESC.',i),names(Basetable),value=F)
     
    }
Basetable <- Basetable[,-(tmp_seo),with=F]

for (i in 1:5)
{
  tmp_rfcd[i] <- grep(paste0('RFCD.DESC.',i),names(Basetable),value=F)
}

Basetable <- Basetable[,-(tmp_rfcd),with=F]

for (i in 1:5)
{
  tmp_rfcd.code[i]  <- grep(paste0('RFCD.Code.',i),names(Basetable),value=F)
  
}
Basetable <- Basetable[,-(tmp_rfcd.code),with=F]

for (i in 1:5)
{
  tmp_seo.code[i]  <- grep(paste0('SEO.Code.',i),names(Basetable),value=F)
  
}
Basetable <- Basetable[,-(tmp_seo.code),with=F]


Basetable$RFCD.Size<-as.numeric(Basetable$RFCD.Size)
Basetable$SEO.Size<-as.numeric(Basetable$SEO.Size)
Basetable$Grant.Category.Code<-as.numeric(Basetable$Grant.Category.Code)
Basetable$Contract.Value.Group<-as.numeric(Basetable$Contract.Value.Group)
Basetable$RFCD.DESC2.1 <- as.numeric(as.character(Basetable$RFCD.DESC2.1))
Basetable$RFCD.DESC2.2 <- as.numeric(as.character(Basetable$RFCD.DESC2.2))
Basetable$RFCD.DESC2.3 <- as.numeric(as.character(Basetable$RFCD.DESC2.3))
Basetable$RFCD.DESC2.4 <- as.numeric(as.character(Basetable$RFCD.DESC2.4))
Basetable$RFCD.DESC2.5 <- as.numeric(as.character(Basetable$RFCD.DESC2.5))
Basetable$SEO.DESC2.1 <- as.numeric(as.character(Basetable$SEO.DESC2.1))
Basetable$SEO.DESC2.2 <- as.numeric(as.character(Basetable$SEO.DESC2.2))
Basetable$SEO.DESC2.3 <- as.numeric(as.character(Basetable$SEO.DESC2.3))
Basetable$SEO.DESC2.4 <- as.numeric(as.character(Basetable$SEO.DESC2.4))
Basetable$SEO.DESC2.5 <- as.numeric(as.character(Basetable$SEO.DESC2.5))
 
#f2n <- function(x) {as.numeric(levels(x)[as.numeric(x)])}
#my function
#   other.fact <- grep(paste0('RFCD.DESC2.'),names(Basetable),value=T)
#   
#   class(Basetable) <- 'data.frame'
#   for (i in 1:length(other.fact))
#               {  
#               c <- paste('Basetable$RFCD.DESC2.',i, '<- as.numeric(as.character(Basetable$RFCD.DESC2.',i,'))',sep='')
#               eval(c)  
#               }

# > for ( i in 1:length(other.fact)) {sapply(other.fact[i],f2c)}
# Warning messages:
#   1: In FUN("g2$RFCD.DESC2.1"[[1L]], ...) : NAs introduced by coercion
# 2: In FUN("g2$RFCD.DESC2.2"[[1L]], ...) : NAs introduced by coercion
# 3: In FUN("g2$RFCD.DESC2.3"[[1L]], ...) : NAs introduced by coercion
# 4: In FUN("g2$RFCD.DESC2.4"[[1L]], ...) : NAs introduced by coercion
# 5: In FUN("g2$RFCD.DESC2.5"[[1L]], ...) : NAs introduced by coercion


Basetable <- dummy.data.frame(Basetable,names=c('RFCD.Size','SEO.Size','Grant.Category.Code','Contract.Value.Group')) 
#class(Basetable) <- 'data.table'
training_grants<-Basetable[year(Basetable$Start.date)<2008,]
test_grants<-Basetable[year(Basetable$Start.date)==2008,]

tmp_Start.date <- training_grants$Start.date
training_grants$Start.date <- NULL
tmp_Start.date.test <- test_grants$Start.date
test_grants$Start.date <- NULL

#install.packages('caret',depen=T)
#require(caret)

# yTRAIN <- factor(training_grants$Grant.Status)
# training_grants$Grant.Status <- NULL
# yTEST <- factor(test_grants$Grant.Status)
# test_grants$Grant.Status <- NULL

# ctrl <- trainControl(method = "repeatedcv", savePred=T, classProbs=T,repeats=3)
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)



# #logistic regression
# mod.logreg <- train(as.factor(Grant.Status) ~., data=training_grants, method = "LMT", trControl = ctrl)
# 
# #C5.0
# mod.c5.0 <- train(as.factor(Grant.Status) ~., data=training_grants, method = "C5.0", trControl = ctrl)
# predict(mod.c5.0, newdata = test_grants,type = 'prob')
# 
# #RandomForest
# mod.rf <- train(as.factor(Grant.Status) ~., data=training_grants, method = "rf", trControl = ctrl)
# 
# #NaiveBayes
# mod.nb <- train(as.factor(Grant.Status) ~., data=training_grants, method = "nb", trControl = ctrl)
# trellis.par.set(caretTheme())
# plot(mod.nb, metric = "ROC")
# 
# #adaboost
# mod.ada <- train(factor(Grant.Status) ~., data=BasetableTRAINbig, method = "ada", trControl = ctrl)
# trellis.par.set(caretTheme())
# plot(mod.ada, metric = "ROC")
# 
# #Gradient Boosting Method
# gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), n.trees = (1:30)*50, shrinkage = 0.1)
# nrow(gbmGrid)
# mod.gbm <- train(factor(Grant.Status) ~., data=training_grants, method = "gbm", trControl = ctrl,tuneGrid = gbmGrid)
# trellis.par.set(caretTheme())
# plot(mod.gbm, metric = "ROC")
# 
# #regularized discriminant analysis 
# mod.rdaFit <- train(factor(Grant.Status) ~ ., data = training_grants, method = "rda", trControl = fitControl,tuneLength = 4,metric = "ROC")
# 
#  