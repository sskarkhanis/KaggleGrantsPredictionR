#kaggle grants data set competition
set.seed(108)

pckg = c('caret','MASS','data.table','lubridate','plyr','sqldf')

install.packages(pckg,dependencies=T)

require(caret)
require(MASS)
require(data.table)
require(lubridate) 
require(plyr)
require(sqldf)
help(package=data.table)


setwd('/Users/skarkhanis/Desktop/DSR 2014/KaggleGrants')
raw = read.csv('./unimelb_training.csv',header=T)

#raw <- read.csv("unimelb_training.csv")

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
raw$Role.1 <- as.character(raw$Role.1)
raw$Role.1[raw$Role.1 == ""] <- "Unk"

#finding missing values per column
NA.find <- function(x) length(which(is.na(x))) #function for finding missing values
NaN.find <- function(x) length(which(is.nan(x))) #function for finding non-numbers

#function call to check missing values
NA.grants <- data.frame(sapply(raw, NA.find))
colnames(NA.grants) <- c('nr.missing')

tmp <- data.frame((NA.grants))
tmp$pct.missing <- round((tmp$nr.missing / nrow(raw)),3)*100

rmv.col1 = (grep('RFCD*', colnames(raw),value=T))
rmv.col2 = (grep('SEO*', colnames(raw),value=T))

inicols = c("Grant.Status", "Sponsor.Code","Grant.Category.Code" , "Contract.Value.Band...see.note.A","Start.date")
raw1 <- cbind(raw[,c("Grant.Application.ID",inicols)],raw[,c(grep('RFCD*', colnames(raw),value=F), grep('SEO*', colnames(raw),value=F) )])
raw2 <- raw[,!(names(raw) %in% c(inicols,rmv.col1,rmv.col2))]

#split column names based on '.'
sn = strsplit((names(raw2)),"\\.")

#creating empty vector
v <- vector(length=length(sn)-1)

#removing grant.application.id
for (i in 2:length(v))
  {
    v[i-1] = paste(sn[[i]],collapse='.')
    
  }

#creating empty list
rd <- vector(mode = "list", length = 15)
for (i in 1:15){
                  xx = grep(paste('\\.',i,'$',sep=''), v,value=T)
                  rd[[i]] = raw2[,c('Grant.Application.ID',xx)]
                  
              }

#storing column names
cnames <- gsub('\\.1',replacement='',names(rd[[1]]))

for (i in 1:15){
names(rd[[i]]) <- cnames
}

#vertical <- do.call("rbind", rd)
#vertical <- subset(vertical, Role != "")

mergedGrants <- (rd[[1]])
for (i in 2:15)
{
  mergedGrants = rbind(mergedGrants, rd[[i]])
  
}

mergedGrants <- (mergedGrants[order(mergedGrants$Grant.Application.ID),])
mergedGrants <- data.table(mergedGrants)

#removing NULL personIDs and role = ''
mergedGrants <- mergedGrants[!(is.na(Person.ID) & Role == ''),]













#Exercise questions from 20140901

v2 <- data.table(vertical)
#question1
xx <- v2[,.N,by=Grant.Application.ID]

#question2
#names(v2) <- shortNames(names(v2),'Num')
yy <- v2[,.N,by=list(Grant.Application.ID,Role)]

#question3
#people per age group
#vertical$age <- year(today()) - as.numeric(as.character(vertical$Year.of.Birth)) #
v2$age <- as.numeric(as.character(v2$Year.of.Birth))
zz <- v2[,.N,by=list(Grant.Application.ID,Role,age)]

#question4



#question5



#question6



#question7
