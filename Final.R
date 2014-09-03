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

 
pckg = c('caret','MASS','data.table','lubridate','plyr','sqldf','stringi','reshape2')
 

rm(list=ls())


## Load required libraries
library(plyr)
library(caret)
library(lubridate)
library(data.table)
library(stringi)
library(reshape2)
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
# Warning messages:
# 1: <anonymous>: ... may be used in an incorrect context: '.fun(piece, ...)'
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
#setwd('/Users/skarkhanis/Dropbox/gitrepo/KaggleGrants')
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

investigators<-data.table()
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
  investigators<-rbind(investigators,subdata)
}

setkey(investigators,'Grant.Application.ID')

#remove all investigators with no role
investigators<-investigators[Role!='',]
investigators$Role<-factor(investigators$Role)

# set person ids to zero for people with role but no id
external_inv<-c('EXT_CHIEF_INVESTIGATOR','EXT_CHIEF_INVESTIGATOR','STUD_CHIEF_INVESTIGATOR','STUDRES','EXTERNAL_ADVISOR')
investigators[Role%in% external_inv&is.na(Person.ID),Person.ID:=0]

grants<-as.data.table(raw[1:26])
setkey(grants,'Grant.Application.ID')

#find out grants without investigators
investigator_grants<-unique(investigators$Grant.Application.ID)
grants_without_investigators<-grants[!(Grant.Application.ID %in%investigator_grants)]
#remove them from the grants for now
grants<-grants[!Grant.Application.ID %in% grants_without_investigators$Grant.Application.ID,]

source('Grants_Functions.R')

vars<-seq(7,7+1.9*5)
m<-calculate_percentages(grants,vars)
update_grants(grants,m,'RFCD')
vars<-seq(17,17+1.9*5)
m<-calculate_percentages(grants,vars)
update_grants(grants,m,'SEO')

########################################SK Part########################################
 
source('RFCD.Code.Lookup.R')
source('SEO.Code.Lookup.R')

#recoding RFCD Code to create RFCD Desc
class(grants)<-'data.frame'

grants2<-copy(grants)

for (i in 1:5)
    {
      tmp1 <- paste0('t_rfcdcode',i)
      tmp2 <- paste0('RFCD.Code.',i)
      #grants[,tmp1] <- substr(unlist(grants[,tmp2,with=FALSE]),1,2)
      grants[,tmp1] <- substr(unlist(grants[tmp2]),1,2)
      grants <- merge(rfcdlookup,grants,by.x='RFCD.Code',by.y=(tmp1),sort=T,all.y=T,incomparables=NA,NA.last=T)
      grants['RFCD.Code'] <- NULL
      grants[tmp1] <- NULL
      names(grants)[names(grants) == 'RFCD.DESC'] <- paste0('RFCD.DESC.',i)
    }

#recoding SEO Code to create SEO Desc
for (i in 1:5)
{
  tmp1 <- paste0('t_seocode',i)    
  tmp2 <- paste0('SEO.Code.',i)
  grants[,tmp1] <- substr(unlist(grants[tmp2]),1,2)
  #grants[,tmp1] <- substr(grants[,eval( (tmp2))],1,2)
  grants <- merge(seolookup,grants,by.x='SEO.Code',by.y=(tmp1),sort=T,all.y=T,incomparables=NA,NA.last=T)
  grants['SEO.Code'] <- NULL
  grants[tmp1] <- NULL
  names(grants)[names(grants) == 'SEO.DESC'] <- paste0('SEO.DESC.',i)
}

grants<-as.data.table(grants)
setkey(grants,'Grant.Application.ID')
grants$Start.date<-as.Date(grants$Start.date,format="%d/%m/%y")

# impute nr of years in university
investigators[Person.ID==0,No..of.Years.in.Uni.at.Time.of.Grant:='Not Applicable']
investigators[No..of.Years.in.Uni.at.Time.of.Grant==''&!is.na(Dept.No.),No..of.Years.in.Uni.at.Time.of.Grant:='Unknown']
investigators[No..of.Years.in.Uni.at.Time.of.Grant==''&is.na(Dept.No.),No..of.Years.in.Uni.at.Time.of.Grant:='Not Applicable']
investigators$No..of.Years.in.Uni.at.Time.of.Grant<-factor(investigators$No..of.Years.in.Uni.at.Time.of.Grant)

# rename contract value type (contract value N is missing)
levels(grants$Contract.Value.Band...see.note.A)<-c(LETTERS[1:13],LETTERS[15:17],'U')

#combine levels of contract value into groups
grants[Contract.Value.Band...see.note.A %in% list('A'),Contract.Value.Group:=1]
grants[Contract.Value.Band...see.note.A %in% LETTERS[2:7],Contract.Value.Group:=2]
grants[Contract.Value.Band...see.note.A %in% LETTERS[8:17],Contract.Value.Group:=3]
grants[Contract.Value.Band...see.note.A %in% list('U'),Contract.Value.Group:=4]
grants$Contract.Value.Group<-factor(grants$Contract.Value.Group)

# get all poeple with phds in a grant
levels(investigators$With.PHD)<-c(0,1)
with_phd<-investigators[,list(Sum.PHD=sum(With.PHD=='1',na.rm=TRUE)),by='Grant.Application.ID']
grants[,Sum.PHD:=with_phd[,Sum.PHD]]
# get total number of successful or unsuccessful grant applications
sum_grants<-investigators[,list(Sum.of.Successful.Grant=sum(Number.of.Successful.Grant,na.rm=TRUE),Sum.of.Unsuccessful.Grant=sum(Number.of.Unsuccessful.Grant,na.rm=TRUE)),by='Grant.Application.ID']
grants$Sum.of.Successful.Grant<-sum_grants[,Sum.of.Successful.Grant]
grants$Sum.of.Unsuccessful.Grant<-sum_grants[,Sum.of.Unsuccessful.Grant]
########################################SK Part########################################


tmp_sk <- subset(investigators,select=c('Grant.Application.ID','Role','Year.of.Birth','Country.of.Birth',"A.","A","B","C"))
#setkey(tmp_sk,Grant.Application.ID)


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
#tmp_sk$Age[is.na(tmp_sk$Age)==T] <- NA
#hist(tmp_sk$Age[!tmp_sk$d_age_ind])
#setting missing levels of Role to unknown
#levels(tmp_sk$Role)[1] <- 'UNKNOWN'
#levels(tmp_sk$Role)
#dim(tmp_sk)
#data table containing aggregate Application ID by Role
tmp_sk_Role <- dcast(tmp_sk,Grant.Application.ID ~ Role,value.var="Role",fun=length)

#data table containing aggregate Application ID by Age
tmp_sk2 <- subset(tmp_sk,select=c('Grant.Application.ID','Age'))
class(tmp_sk2) <- "data.frame"
tmp_sk_Age <- aggregate(tmp_sk2["Age"],tmp_sk2["Grant.Application.ID"],mean,na.rm=TRUE)

# There are grants where all people have NA age
tmp_sk_Age$Age[is.nan(tmp_sk_Age$Age)]<-NA
hist(tmp_sk_Age$Age)

#data table containing aggregate Application ID by Country of Birth
tmp_sk_CountryofBirth <- dcast(tmp_sk,Grant.Application.ID ~ Country.of.Birth,fun=length,value.var="Country.of.Birth")

#aggregating by A.,A,B,C
tmp_sk4 <- subset(tmp_sk,select=c('Grant.Application.ID','A.',"A","B","C"))

tmp_Journals<-tmp_sk4[,list(A.=sum(A.,na.rm=TRUE),A=sum(A,na.rm=TRUE),B=sum(B,na.rm=TRUE),C=sum(C,na.rm=TRUE)),by='Grant.Application.ID']
#result<-tmp_sk4[,list(M=mean(A.)),by='Grant.Application.ID']

#class(tmp_sk4) <- "data.frame"
# value<-dcast(tmp_sk4,'Grant.Application.ID~A.',value.var='A.',fun=sum)
# View(tmp_sk4)
# #check part on removing na.rm = T
# tmp_sk_4_A. <- aggregate(tmp_sk4["A."],tmp_sk2["Grant.Application.ID"],sum,na.rm=T)
# tmp_sk_4_A  <- aggregate(tmp_sk4["A"],tmp_sk2["Grant.Application.ID"],sum,na.rm=T)
# tmp_sk_4_B  <- aggregate(tmp_sk4["B"],tmp_sk2["Grant.Application.ID"],sum,na.rm=T)
# tmp_sk_4_C  <- aggregate(tmp_sk4["C"],tmp_sk2["Grant.Application.ID"],sum,na.rm=T)
# 
# tmp_sk_4_A.A <- merge(tmp_sk_4_A.,tmp_sk_4_A,by.x="Grant.Application.ID",sort=T,all.y=T,incomparable=NA)
# tmp_sk_4_B.C <- merge(tmp_sk_4_B,tmp_sk_4_C,by.x="Grant.Application.ID",sort=T,all.y=T,incomparable=NA)
#   #final merge of various sum of publications by Grant.Application.ID
# tmp_skAABC <- merge(tmp_sk_4_A.A,tmp_sk_4_B.C,by.x="Grant.Application.ID",sort=T,all.y=T,incomparable=NA)
# rm(tmp_sk_4_A.A,tmp_sk_4_B.C,tmp_sk_4_A.,tmp_sk_4_A,tmp_sk_4_B,tmp_sk_4_C)
# View(tmp_skAABC)
#finalmerging of various tmp_sk's
#str(tmp_sk_Role)
#?merge
grants <- cbind(grants,tmp_sk_Role,tmp_sk_Age,tmp_sk_CountryofBirth,tmp_Journals)

#removing the extra Grant.Application.IDs
cc <- which(names(grants)=='Grant.Application.ID')
cc <- cc[2:length(cc)]
length(cc)

grants <- grants[,-(cc),with=F]
tmp.Contract.Value.Band...see.note.A <- grants$Contract.Value.Band...see.note.A
grants$Contract.Value.Band...see.note.A <- NULL
View(grants)
