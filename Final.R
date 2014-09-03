### R code from Applied Predictive Modeling (2013) by Kuhn and Johnson.
### Copyright 2013 Kuhn and Johnson
###
### R code to process the Kaggle grant application data.
###
### Required packages: plyr, caret, lubridate
################################################################################

## The plyr, caret and libridate packages are used in this script. The
## code can also be run using multiple cores using the ddply()
## function. See ?ddply to get more information.
##
## These computations will take a fair amount of time and may consume
## a non-trivial amount of memory in the process.
##
set.seed(108)

pckg = c('caret','MASS','data.table','lubridate','plyr','sqldf','stringi','reshape2')

install.packages(pckg,dependencies=T)

## Load required libraries
require(caret)
require(MASS)
require(data.table)
require(lubridate) 
require(plyr)
require(sqldf)
require(stringi)

#help(package=data.table)

#rm(list=ls())

## How many cores on the machine should be used for the data
## processing. Making cores > 1 will speed things up (depending on your
## machine) but will consume more memory.
cores <- 3
if(cores > 1){
  library(doParallel)
  library(foreach)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
}

# Note: when using parallel=T, there are always a couple of warnings: 
#   Warning messages:
#   1: <anonymous>: ... may be used in an incorrect context: '.fun(piece, ...)'
# 2: <anonymous>: ... may be used in an incorrect context: '.fun(piece, ...)'
# The issue is open on github: https://github.com/hadley/plyr/issues/203
# 
# It seems to be safe to ignore them
# 
# there are many other parallel backends, (eg doParallel), it might be worth trying them



## Read in the data in it's raw form. Some of the column headings do
## not convert to proper R variable names, so many will contain dots,
## such as "Dept.No" instead of "Dept No"

#setwd("~/SkyDrive/DSR-docs/dsrteach/grants/data")
setwd('/Users/skarkhanis/Desktop/DSR 2014/KaggleGrants')
raw <- read.csv("unimelb_training.csv")

## In many cases, missing values in categorical data will be converted
## to a value of "Unk"
raw$Sponsor.Code <- as.character(raw$Sponsor.Code)
raw$Sponsor.Code[raw$Sponsor.Code == ""] <- "Unk"
raw$Sponsor.Code <- factor(paste("Sponsor", raw$Sponsor.Code, sep = ""))

raw$Grant.Category.Code <- as.character(raw$Grant.Category.Code)
raw$Grant.Category.Code[raw$Grant.Category.Code == ""] <- "Unk"
raw$Grant.Category.Code <- factor(paste("GrantCat", raw$Grant.Category.Code, sep = ""))

raw$Contract.Value.Band...see.note.A <- as.character(raw$Contract.Value.Band...see.note.A)
raw$Contract.Value.Band...see.note.A[raw$Contract.Value.Band...see.note.A == ""] <- "Unk"
raw$Contract.Value.Band...see.note.A <- factor(paste("ContractValueBand", raw$Contract.Value.Band...see.note.A, sep = ""))
## Change missing Role.1 information to Unk
#raw$Role.1 <- as.character(raw$Role.1)
#raw$Role.1[raw$Role.1 == ""] <- "Unk"


invT<-data.table()
columns<-colnames(raw)
#get the columns that are named Person.X
id_columns<-which(!is.na(stri_match_first(columns,regex='Person.ID')))
id_columns[16]<-252

# stack the investigators
for(id in seq(15))
{
  subdata<-raw[,id_columns[id]:(id_columns[id+1]-1)]
  if(id<10)
    colnames(subdata)<-stri_sub(colnames(subdata),1,-3)
  else
    colnames(subdata)<-stri_sub(colnames(subdata),1,-4)
  not_na<-!all(is.na(subdata))
  grants<-raw[not_na,'Grant.Application.ID']
  subdata<-cbind(Grant.Application.ID=grants,subdata[not_na,])
  invT<-rbind(invT,subdata)
}

# reorder by Application ID
invT<-invT[order(invT$Grant.Application.ID),]

# remove subjects who have ID but no role
#invT[Role=='',Role:='Unk']
invT<-invT[!(is.na(Person.ID)&Role==''),]
invT$Role<-factor(invT$Role)

# set person ids to zero for people with role but no id
external_inv<-c('EXT_CHIEF_INVESTIGATOR','EXT_CHIEF_INVESTIGATOR','STUD_CHIEF_INVESTIGATOR','STUDRES','EXTERNAL_ADVISOR')
invT[Role%in% external_inv&is.na(Person.ID),Person.ID:=0]



#Splitting into training and test

grants <- data.table(raw[,1:26])
grants$Start.date<-as.Date(grants$Start.date,format="%d/%m/%y")
training_grants<-grants[year(Start.date)<2008,]
training_inv<-invT[Grant.Application.ID %in%training_grants$Grant.Application.ID,]

dim(training_grants)
dim(training_inv)


########################################SK Part########################################
View(training_grants)
 
#recoding RFCD Code to create RFCD Desc
for (i in 1:5)
    {
      tmp1 <- paste0('t_rfcdcode',i)    
      tmp2 <- paste0('RFCD.Code.',i)
      training_grants[,tmp1] <- substr(training_grants[,eval( (tmp2))],1,2)
      training_grants <- merge(rfcdlookup,training_grants,by.x='RFCD.Code',by.y=(tmp1),sort=T,all.y=T,incomparables=NA,NA.last=T)
      training_grants['RFCD.Code'] <- NULL
      training_grants[tmp1] <- NULL
      names(training_grants)[names(training_grants) == 'RFCD.DESC'] <- paste0('RFCD.DESC.',i)
    }

#recoding SEO Code to create SEO Desc
for (i in 1:5)
{
  tmp1 <- paste0('t_seocode',i)    
  tmp2 <- paste0('SEO.Code.',i)
  training_grants[,tmp1] <- substr(training_grants[,eval( (tmp2))],1,2)
  training_grants <- merge(seolookup,training_grants,by.x='SEO.Code',by.y=(tmp1),sort=T,all.y=T,incomparables=NA,NA.last=T)
  training_grants['SEO.Code'] <- NULL
  training_grants[tmp1] <- NULL
  names(training_grants)[names(training_grants) == 'SEO.DESC'] <- paste0('SEO.DESC.',i)
}

########################################SK Part########################################

View(training_inv)

tmp_sk <- subset(training_inv,select=c('Grant.Application.ID','Role','Year.of.Birth','Country.of.Birth',"A.","A","B","C"))
setkey(tmp_sk,Grant.Application.ID)


#tmp_sk <- tmp_sk[order(tmp_sk$Grant.Application.ID),]



#setting missing levels of Country.of.Birth to unknown
levels(tmp_sk$Country.of.Birth)[1] <- 'Unknown'
# alt method to investigate : 
  #levels(tmp_sk$Country.of.Birth) == ''
  #tmp_sk$Country.of.Birth[tmp_sk$Country.of.Birth == '']

#calculating age
tmp_sk$Age <- (year(today()) - tmp_sk[,Year.of.Birth]) 

#creating a dummy indicator where Age is NA
tmp_sk$d_age_ind <- ifelse((is.na(tmp_sk$Age) == T),1,0)

#imputing 0 where Age == NA
tmp_sk$Age[is.na(tmp_sk$Age)==T] <- 0

#setting missing levels of Role to unknown
levels(tmp_sk$Role)[1] <- 'UNKNOWN'

dim(tmp_sk)
#data table containing aggregate Application ID by Role
tmp_sk_Role <- dcast(tmp_sk,Grant.Application.ID ~ Role,value.var="Role",fun=length)

#data table containing aggregate Application ID by Age
tmp_sk2 <- subset(tmp_sk,select=c('Grant.Application.ID','Age'))
class(tmp_sk2) <- "data.frame"
tmp_sk_Age <- aggregate(tmp_sk2["Age"],tmp_sk2["Grant.Application.ID"],mean)
#tmp_sk_Age  <- dcast.data.table(tmp_sk,Grant.Application.ID ~ Age,fun=mean)

#data table containing aggregate Application ID by Country of Birth
tmp_sk_CountryofBirth <- dcast(tmp_sk,Grant.Application.ID ~ Country.of.Birth,fun=length,value.var="Country.of.Birth")

#aggregating by A.,A,B,C
tmp_sk4 <- subset(tmp_sk,select=c('Grant.Application.ID','A.',"A","B","C"))
class(tmp_sk4) <- "data.frame"
#check part on removing na.rm = T
tmp_sk_4_A. <- aggregate(tmp_sk4["A."],tmp_sk2["Grant.Application.ID"],sum,na.rm=T)
tmp_sk_4_A  <- aggregate(tmp_sk4["A"],tmp_sk2["Grant.Application.ID"],sum,na.rm=T)
tmp_sk_4_B  <- aggregate(tmp_sk4["B"],tmp_sk2["Grant.Application.ID"],sum,na.rm=T)
tmp_sk_4_C  <- aggregate(tmp_sk4["C"],tmp_sk2["Grant.Application.ID"],sum,na.rm=T)

tmp_sk_4_A.A <- merge(tmp_sk_4_A.,tmp_sk_4_A,by.x="Grant.Application.ID",sort=T,all.y=T,incomparable=NA)
tmp_sk_4_B.C <- merge(tmp_sk_4_B,tmp_sk_4_C,by.x="Grant.Application.ID",sort=T,all.y=T,incomparable=NA)
  #final merge of various sum of publications by Grant.Application.ID
tmp_skAABC <- merge(tmp_sk_4_A.A,tmp_sk_4_B.C,by.x="Grant.Application.ID",sort=T,all.y=T,incomparable=NA)
rm(tmp_sk_4_A.A,tmp_sk_4_B.C,tmp_sk_4_A.,tmp_sk_4_A,tmp_sk_4_B,tmp_sk_4_C)

#finalmerging of various tmp_sk's
n_training_inv <- cbind(tmp_sk_Role,tmp_sk_Age,tmp_sk_CountryofBirth,tmp_skAABC)
