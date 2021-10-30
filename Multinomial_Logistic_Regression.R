# Remove all variables
rm(list = ls())

# Set working directory
setwd('E:/PGDBA/IIMC/Categorical Data Analysis/Project')

###################################################################
# Load the Libraries
###################################################################
library('nnet')
library('ggplot2')
library('plotly')

###################################################################
# Load the Dataset
###################################################################
dos_data<-read.csv('dos_original.csv')

# Change Factor Variables
dos_data$Gender<-as.factor(dos_data$Gender)
dos_data$marital<-as.factor(dos_data$marital)
dos_data$dep<-as.factor(dos_data$dep)

# Remove race column
dos_data<-dos_data[-c(3)]

nrow(dos_data)   # 745 observations


###################################################################
# Clean the Dataset
###################################################################

# Create New Dataset removing marital and education columns
dos_data_2<-dos_data[,c(1,2,4,5,7)]

# Remove NA rows
dos_data_1<-dos_data[complete.cases(dos_data), ]

# Remove NA rows
dos_data_2<-dos_data_2[complete.cases(dos_data_2), ]

nrow(dos_data_1)   # 731 observations
nrow(dos_data_2)   # 743 observations

# Create MS3Level variable
dos_data_1$MS3Level<-dos_data_1$marital
dos_data_1[dos_data_1$marital==2,]$MS3Level<-1
dos_data_1[dos_data_1$marital==1 | dos_data_1$marital==4 | dos_data_1$marital==5 | dos_data_1$marital==3,]$MS3Level<-3 
dos_data_1[dos_data_1$marital==6,]$MS3Level<-2

dos_data_1$MS3Level<-as.factor(dos_data_1$MS3Level)

# Change Factor Variables
dos_data_1$Gender<-as.character(dos_data_1$Gender)
dos_data_1$MS3Level<-as.character(dos_data_1$MS3Level)
dos_data_1[dos_data_1$Gender==2,]$Gender<-0
dos_data_1[dos_data_1$MS3Level==3,]$MS3Level<-0
dos_data_1$Gender<-as.factor(dos_data_1$Gender)
dos_data_1$MS3Level<-as.factor(dos_data_1$MS3Level)

dos_data_1<-dos_data_1[,c(2,4,5,3,1,8,7)]
summary(dos_data_1)



# Change Factor Variables
dos_data_2$Gender<-as.character(dos_data_2$Gender)
dos_data_2[dos_data_2$Gender==2,]$Gender<-0
dos_data_2$Gender<-as.factor(dos_data_2$Gender)

###################################################################
# Exploratory Data Analysis
###################################################################


# Create Box Plots of the data
head(dos_data_1)
head(dos_data_2)


###################################################################
# Multinomial Logistic Regression Model
###################################################################

# Model 1
dos_mlr <- multinom(dep ~ ., data = dos_data_1)
summary(dos_mlr)

# Create Classification Table
classification_table<-data.frame(predict(dos_mlr,dos_data_1))
colnames(classification_table)<-c('Predicted')
classification_table$Actual<-dos_data_1$dep

table(classification_table$Actual, classification_table$Predicted)


# Model 2
dos_mlr_2 <- multinom(dep ~ ., data = dos_data_2)
summary(dos_mlr_2)

# Create Classification Table
classification_table<-data.frame(predict(dos_mlr_2,dos_data_2))
colnames(classification_table)<-c('Predicted')
classification_table$Actual<-dos_data_2$dep

table(classification_table$Actual, classification_table$Predicted)















###################################################################
# Load the Dataset
###################################################################
cog_data<-read.csv('data.csv')
colnames(cog_data)<-c('BAI','BDI','DAS1','DAS2','Age','Gender',
                      'ResidentialArea','FamilyType','MothersOccupation','FatherOccupation')

# Gender 1==Male 2==Female
# ResidentialArea 1==Urban 2==Rural
# FamilyType 1==Nuclear 2==Joint
# Mother 0==Diseased 1==Housewife 2==WorkingWoman
# Father 0==Diseased 1==GazettedOfficer 2==NonGazettedOfficer 3==GovtEmployee
# 4==Defence 5==BankEmployee 6==LargeScaleBusinessman 7==SmallScaleBusinessMan 8==WorkingClass

cog_data<-cog_data[complete.cases(cog_data), ]

cog_data$Gender<-as.factor(cog_data$Gender)
cog_data$ResidentialArea<-as.factor(cog_data$ResidentialArea)
cog_data$FamilyType<-as.factor(cog_data$FamilyType)
cog_data$MothersOccupation<-as.factor(cog_data$MothersOccupation)


cog_data[cog_data$FatherOccupation==1 | cog_data$FatherOccupation==2 |
         cog_data$FatherOccupation==3 | cog_data$FatherOccupation==4 |
         cog_data$FatherOccupation==5,]$FatherOccupation<-1

cog_data[cog_data$FatherOccupation==6 | cog_data$FatherOccupation==7,]$FatherOccupation<-2
cog_data[cog_data$FatherOccupation==8,]$FatherOccupation<-3

cog_data$FatherOccupation<-as.factor(cog_data$FatherOccupation)

cog_data$CD<-cog_data$DAS1+cog_data$DAS2
head(cog_data)

cog_data$CognitiveDisorder = -1
cog_data[cog_data$CD<=35,]$CognitiveDisorder<-0
cog_data[cog_data$CD>35 & cog_data$CD<=43,]$CognitiveDisorder<-1
cog_data[cog_data$CD>43 & cog_data$CD<=50,]$CognitiveDisorder<-2
cog_data[cog_data$CD>50,]$CognitiveDisorder<-3

table(cog_data$CognitiveDisorder)

# Model 1
cog_mlr <- multinom(CognitiveDisorder ~ .-CD-BAI-BDI-DAS1-DAS2, data = cog_data)
summary(cog_mlr)

library(brant)
cog_data$CognitiveDisorder=factor(cog_data$CognitiveDisorder,levels=c(0,1,2,3),ordered = TRUE)
cog_pol=polr(CognitiveDisorder ~ Age+Gender+ResidentialArea+FamilyType+MothersOccupation+FatherOccupation, data = cog_data,Hess = TRUE)
summary(cog_pol)
brant(cog_pol)



  

