library(ggplot2)
library(ggrepel)
library(plyr)
library(dplyr)
library(scales)
library(corrplot)
library(Metrics)
library(dummies)
library(caret)
library(glmnet)
library(caretEnsemble)
library(moments)
library(MASS)

setwd("/Users/archana/Downloads/input/")
# Step 1:     Preprocessing the data set 
#Load the train and test data set.
trainingSet <- read.csv("train.csv", stringsAsFactors = F)
testingSet <- read.csv("test.csv", stringsAsFactors = F)

#Divided 50 percent of data in training set and 50 percent in testing set.
#check the records as below. reset testing sets sale price.

dim(trainingSet)
testSalePrice <- testingSet$SalePrice[1:1460]
testingSet$SalePrice <-  as.numeric(0)

#row bind the sets results in total set to a data frame all
totalSet <- rbind(trainingSet, testingSet)
dim(totalSet)

#check the duplicates.
anyDuplicated(totalSet)

#check mean median and details of sale price of houses.
ggplot(data=totalSet[!is.na(totalSet$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="black", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
summary(totalSet$SalePrice)

#totalColumns
numericVars <- which(sapply(totalSet, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(totalSet, is.factor)) #index vector factor variables
charVars <- which(sapply(totalSet, is.character)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables', length(charVars) , 'char varraibles')

all_numVar <- totalSet[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables


#sort on decreasing correlations with SalePrice

cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorAll <- names(which(apply(cor_sorted, 1,function(x) abs(x)>0.5)))
cor_numVarAll <- cor_numVar[CorAll, CorAll]
corrplot.mixed(cor_numVarAll, tl.col="blue", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

# As per the visualization graph, overallquality and above grade living area have highest correlation.
ggplot(data=totalSet[!is.na(totalSet$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(totalSet$GrLivArea[!is.na(totalSet$SalePrice)]>4500, rownames(totalSet), '')))

ggplot(data=totalSet[!is.na(totalSet$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(totalSet$OverallQual[!is.na(totalSet$SalePrice)]>10000, rownames(totalSet), '')))


# check all the missing columns in the csv file (test n train)
missing_cols <- sapply(totalSet,function(x)sum(is.na(x)))
summary_missing <- data.frame(index = names(totalSet),missing_values=missing_cols)

null_col <- which(colSums(is.na(totalSet)) > 0)
cat('There are', length(null_col), 'columns with missing values')
#print missing cols 
summary_missing[summary_missing$missing_values > 0,]

#Print all missing columns
null_col #null columns are available
cat(null_col) #column number list is available.

#MSZoning ,LotFrontage ,Alley ,Utilities ,Exterior1st ,Exterior2nd, MasVnrType ,MasVnrArea 
#BsmtQual ,BsmtCond ,BsmtExposure ,BsmtFinType1 ,BsmtFinSF1 ,BsmtFinType2 ,BsmtFinSF2 ,BsmtUnfSF 
#TotalBsmtSF ,Electrical ,BsmtFullBath ,BsmtHalfBath ,KitchenQual ,Functional ,FireplaceQu, GarageType 
#GarageYrBlt ,GarageFinish , GarageCars  ,GarageArea ,GarageQual ,GarageCond ,PoolQC ,Fence 
#MiscFeature ,SaleType ,SalePrice 


#-1-----------------There is a missing value in MSZoning column -----
#The factor() command is used to create and modify factors in R:
#RL- residential low density,C - commercial,RH - residential high density ,FV -floating vilage residential,
#RM - residential medium density
#RL, C, RM, FV, RH - converts to factors
#using the mode value for assigning to the null values. This is a categorical variable which needs to
#be converted
table(totalSet$MSZoning)
totalSet$MSZoning[is.na(totalSet$MSZoning)] <- names(sort(-table(totalSet$MSZoning)))[1]
table(totalSet$MSZoning)

#-2---------------Missing values in lot frontage- replace with median values of the class
#linear feet of street connected to property
#Each area has a defined space based on the locality 
qplot(totalSet$LotFrontage, geom="histogram")  
#Histogram shows that it is normal distribution (denser in centres and less dense in tails)
#we cam impute with mean when data is normally distributed
sum(is.na(totalSet$LotFrontage))
for (i in 1:nrow(totalSet)){
  if(is.na(totalSet$LotFrontage[i])){
    totalSet$LotFrontage[i] <- mean(totalSet$LotFrontage,na.rm = TRUE)
    #totalSet$LotFrontage[i] <- as.integer(median(totalSet$LotFrontage[totalSet$Neighborhood==totalSet$Neighborhood[i]], na.rm=TRUE)) 
  }
}
sum(is.na(totalSet$LotFrontage))

#-3----------Missing values in alley - replace nulls with None and convert to factors
#a narrow passage way between or behind buildings Gravl, Pave , NA (No alley access)
sum(is.na(totalSet$Alley))
#total no of null values are 2721 which is 93 percent of the total data.(we can also drop this after checking p values)
ggplot(totalSet[!is.na(totalSet$SalePrice),], aes(x=Alley, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma)
totalSet$Alley[is.na(totalSet$Alley)] <- 'None'

table(totalSet$Alley)
#Grvl, None, Pave

#-4---------- Missing utilities values - 2 NAs- expect three values all public utilities are available in houses.
#we can also ignore.. lets consider for timebeing
#All public utilities, No sewr (electricity,gas, water) , NoSewa (elec n gas) , ELO (Elec Only)
#categorical feature
sum(is.na(totalSet$Utilities))
ggplot(totalSet[!is.na(totalSet$Utilities),], aes(x=Utilities, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma)
totalSet$Utilities[is.na(totalSet$Utilities)] <- 'None'


#-5----------Missing exterior first - copies mode value to NA (1 NA)-----------
#most used exterior is vinyl siding , so replace the missing value with that.
sum(is.na(totalSet$Exterior1st))
totalSet$Exterior1st[is.na(totalSet$Exterior1st)] <- names(sort(-table(totalSet$Exterior1st)))[1]
table(totalSet$Exterior1st)

#-6------------Missing exterior second - copies mode to NA (1 NA)-------------
sum(is.na(totalSet$Exterior2st))
totalSet$Exterior2nd[is.na(totalSet$Exterior2nd)] <- names(sort(-table(totalSet$Exterior2nd)))[1]
table(totalSet$Exterior2nd)

#-7-8-----------Missing MasVnrtype & MasVnrArea are related---
#if area is null type should be null and Vice-versa
#MasVnrType record which is null
#MasVnrtype  -- vineer area covered with None ,Brick Common, Brickface, stone,
#MasvnrArea -- veneer area
sum(is.na(totalSet$MasVnrType))
sum(is.na(totalSet$MasVnrArea))
totalSet[is.na(totalSet$MasVnrType) & !is.na(totalSet$MasVnrArea), c('MasVnrType', 'MasVnrArea')]
#2611 type is null, fill it with brickface #copy value to type from area
totalSet$MasVnrType[2611] <- names(sort(-table(totalSet$MasVnrType)))[2] 
totalSet[2611, c('MasVnrType', 'MasVnrArea')]

table(totalSet$MasVnrType)
table(totalSet$Area)

qplot(totalSet$MasVnrArea, geom="histogram")  
#above histogram shows, most of the data is at zero
#so it is appropiate to fill the null values with zero, which means there is
#no masonry stone work veeneer area in square feet.
totalSet$MasVnrType[is.na(totalSet$MasVnrType)] <- 'None'
totalSet$MasVnrArea[is.na(totalSet$MasVnrArea)] <-0

Masonry_type <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
totalSet$MasVnrType<-as.integer(revalue(totalSet$MasVnrType, Masonry_type))

#-9-----------Missing basement Quality-----
#BsmtQual: Evaluates the height of the basementEx   Excellent (100+ inches) ,Gd   Good (90-99 inches)
#TA   Typical (80-89 inches) ,Fa   Fair (70-79 inches) ,Po   Poor (<70 inches, NA   No Basement
#update null to none and convert from ordinal to continuous
sum(is.na(totalSet$BsmtQual))
ggplot(totalSet,aes(x=factor(totalSet$BsmtQual))) + geom_bar(stat="count")
totalSet$BsmtQual[is.na(totalSet$BsmtQual)] <- 'None'
totalSet$BsmtQual<- recode(totalSet$BsmtQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
table(totalSet$BsmtQual)


# Revalue the ordinal values to continuous values
qual_types <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

#-10---------Missing basement condition ---- ----
totalSet$BsmtCond[is.na(totalSet$BsmtCond)] <- 'None'
totalSet$BsmtCond<-as.integer(revalue(totalSet$BsmtCond, c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4)))
table(totalSet$BsmtCond)

#-11--------missing basement exposure --------
totalSet$BsmtExposure[is.na(totalSet$BsmtExposure)] <- 'None'
basemt_exp <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)

totalSet$BsmtExposure<-as.integer(revalue(totalSet$BsmtExposure, basemt_exp))
table(totalSet$BsmtExposure)

#-12----------missing basement fintype 1---------
totalSet$BsmtFinType1[is.na(totalSet$BsmtFinType1)] <- 'None'
fin_Type <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

totalSet$BsmtFinType1<-as.integer(revalue(totalSet$BsmtFinType1, fin_Type))
table(totalSet$BsmtFinType1)

#-13-------------missing basement fintype 2---------
totalSet$BsmtFinType2[is.na(totalSet$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

totalSet$BsmtFinType2<-as.integer(revalue(totalSet$BsmtFinType2, FinType))
table(totalSet$BsmtFinType2)

#-14 15 16 17 18 19---------------
totalSet$BsmtFinSF1[is.na(totalSet$BsmtFinSF1)] <- 0
totalSet$BsmtFinSF2[is.na(totalSet$BsmtFinSF2)] <- 0
totalSet$BsmtUnfSF[is.na(totalSet$BsmtUnfSF)] <- 0
totalSet$TotalBsmtSF[is.na(totalSet$TotalBsmtSF)] <- 0
totalSet$BsmtFullBath[is.na(totalSet$BsmtFullBath)] <- 0
totalSet$BsmtHalfBath[is.na(totalSet$BsmtHalfBath)] <- 0

#-20 --- missing electrical values----
#it is a categorical variable
sum(is.na(totalSet$Electrical))
ggplot(totalSet,aes(x=factor(totalSet$Electrical))) + geom_bar(stat="count")
#Sbrkr - replaced with mode value
totalSet$Electrical[is.na(totalSet$Electrical)] <- names(sort(-table(totalSet$Electrical)))[1]
table(totalSet$Electrical)

#-21-----Check missing kitchen quality values--
sum(is.na(totalSet$KitchenQual))
ggplot(totalSet,aes(x=factor(totalSet$KitchenQual))) + geom_bar(stat="count")
totalSet$KitchenQual[is.na(totalSet$KitchenQual)] <- 'TA' #replace with most used value
totalSet$KitchenQual<-as.integer(revalue(totalSet$KitchenQual, qual_types))
table(totalSet$KitchenQual)

#-22----check missing functional values--------
sum(is.na(totalSet$Functional))
ggplot(totalSet,aes(x=factor(totalSet$Functional))) + geom_bar(stat="count")
#replace with typ
totalSet$Functional[is.na(totalSet$Functional)] <- names(sort(-table(totalSet$Functional)))[1]
totalSet$Functional <- as.integer(revalue(totalSet$Functional, c('None'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(totalSet$Functional)


#-23------check missing fireplace qulity--------
sum(is.na(totalSet$FireplaceQu))
#majority are null..almost 95 plus percent..so jus replace with none
totalSet$FireplaceQu[is.na(totalSet$FireplaceQu)] <- 'None'
totalSet$FireplaceQu<- recode(totalSet$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
table(totalSet$FireplaceQu)

#-24--------check missing garage type----------
sum(is.na(totalSet$GarageType))
ggplot(totalSet,aes(x=factor(totalSet$GarageType))) + geom_bar(stat="count")
totalSet$GarageType[is.na(totalSet$GarageType)] <- 'No Garage'
table(totalSet$GarageType)

#-25 , 26, 27, 28 , 29, 30--------check missing garage year built , garage cars, --garage area , garage finish, qualiyu n condition---
totalSet$GarageYrBlt[is.na(totalSet$GarageYrBlt)] <- 0
totalSet$GarageCars[is.na(totalSet$GarageCars)] <- 0
totalSet$GarageArea[is.na(totalSet$GarageArea)] <- 0

totalSet$GarageFinish[is.na(totalSet$GarageFinish)] <- 'None'
finishing <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
totalSet$GarageFinish<-as.integer(revalue(totalSet$GarageFinish, finishing))
table(totalSet$GarageFinish)

totalSet$GarageQual[is.na(totalSet$GarageQual)] <- 'None'
totalSet$GarageQual<-as.integer(revalue(totalSet$GarageQual, qual_types))
table(totalSet$GarageQual)

totalSet$GarageCond[is.na(totalSet$GarageCond)] <- 'None'
totalSet$GarageCond<-as.integer(revalue(totalSet$GarageCond, qual_types))
table(totalSet$GarageCond)

#-31---------PoolQC ----------------------------------
totalSet$PoolQC[is.na(totalSet$PoolQC)] <- 'None'
totalSet$PoolQC<- recode(totalSet$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)

#-32-------------Fence-------------------------------
totalSet$Fence[is.na(totalSet$Fence)] <- 'None'
totalSet$Fence<- recode(totalSet$Fence,"None"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)

#-33-------MiscFeature----------------------------
totalSet$MiscFeature[is.na(totalSet$MiscFeature)] <- 'None'
table(totalSet$MiscFeature)

#-34------------SaleType----------------------------- 
totalSet$SaleType[is.na(totalSet$SaleType)] <- names(sort(-table(totalSet$SaleType)))[1]
table(totalSet$SaleType)

#-35--Remove the missing value of sale price.as.integer() it will convert your "NULL" to NA automatically.
totalSet$SalePrice[is.na(totalSet$SalePrice)] <- names(sort(-table(totalSet$SalePrice)))[1]
totalSet$SalePrice <- as.integer(totalSet$SalePrice)
table(totalSet$SalePrice)


#----------------------------------------------------------
null_col <- which(colSums(is.na(totalSet)) > 0)
cat('There are', length(null_col), 'columns with missing values')
#--------------------done with data cleaning-----------------------------

# if columns are ordinal,recode ordered factors as pseudo-continuous numerical variables -
totalSet$ExterQual<- recode(totalSet$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
totalSet$ExterCond<- recode(totalSet$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
totalSet$HeatingQC<- recode(totalSet$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)

totalSet$TotalSF = totalSet$TotalBsmtSF + totalSet$X1stFlrSF + totalSet$X2ndFlrSF
totalSet$TotBathrooms <- totalSet$FullBath + (totalSet$HalfBath*0.5) + totalSet$BsmtFullBath + (totalSet$BsmtHalfBath*0.5)
totalSet$TotalPorchSF <- totalSet$OpenPorchSF + totalSet$EnclosedPorch + totalSet$X3SsnPorch + totalSet$ScreenPorch
sapply(totalSet, class)

#------check the correlation of all the above fields with our target variable
numericVars <- which(sapply(totalSet, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(totalSet, is.factor)) #index vector factor variables
charVars <- which(sapply(totalSet, is.character)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables', length(charVars) , 'char varraibles')

#cat('There are', length(which(sapply(totalSet, is.numeric))), 'numeric variables, and', length(which(sapply(totalSet, is.factor))), 'categoric variables', length(which(sapply(totalSet, is.character))) , 'char varraibles')


#take high skewed numeric variables and categorical variables for regression analysis
totalSet_Corr <-dummy.data.frame(totalSet, dummy.classes = "character")

#BoxCoxTransform- Pls check what it does.looks like normalization of columns
feature_classes <- sapply(names(totalSet_Corr), function(x) {class(totalSet_Corr[[x]])})
# find numeric classes
numeric_cols <- names(feature_classes[feature_classes != "character"])
#find correlated columns
skewed_cols <- sapply(numeric_cols, function(x) {skewness(totalSet_Corr[[x]], na.rm = TRUE)})
#find columsn with corr > .75
skewed_feats <- skewed_cols[abs(skewed_cols) > 0.75]
for (x in names(skewed_feats)) {
  bc = BoxCoxTrans(totalSet_Corr[[x]], lambda = 0.15)
  totalSet_Corr[[x]] = predict(bc, totalSet_Corr[[x]])
}

#(g)Split combined data back into test,train and validation dataframes
trainSet <- totalSet_Corr[1:1460,]
testSet <- totalSet_Corr[1461:2919,]
#set.seed(123)
in_train <- createDataPartition(trainSet$SalePrice,p=0.7,list=F)
new_train <- trainSet[in_train,]
validation <- trainSet[-in_train,]

#-----------Multiple linear regression----------
#Linear regression is used to predict the value of an outcome variable Y based on one or more input predictor variables X. The aim is to establish a linear relationship (a mathematical formula) between the predictor variable(s) and the response variable, so that, we can use this formula to estimate the value of the response Y, when only the predictors (Xs) values are known.
mlr <-lm(formula = SalePrice ~., data = new_train) 
getOption("max.print")
options(max.print = 2000)
prediction<- predict(mlr,validation, type="response")
summary(mlr)
rmse(validation$SalePrice,prediction)
rss_mlr= (validation$SalePrice-prediction)^2
tss_mlr= (validation$SalePrice-mean(validation$SalePrice))^2
rSquare_mlr = 1 - sum(rss_mlr)/sum(tss_mlr)
rSquare_mlr

#-----write to a file----------------
prediction<- predict(mlr,testSet, type="response")
sub_avg <- data.frame(Id = testSet$Id, SalePrice = prediction)
head(sub_avg)
write.csv(sub_avg, file = 'archana_mlr.csv', row.names = F)

#-----------------------LASSO----------------------------------------
set.seed(123)
lasso <-cv.glmnet(as.matrix(new_train[, -233]), new_train[, 233])
pred_on_train <- predict(lasso, newx = as.matrix(validation[, -233]), s = "lambda.min")
dim(pred_on_train)
rmse= sqrt(apply((validation$SalePrice-pred_on_train)^2,2,mean))
rss= (validation$SalePrice-pred_on_train)^2
tss= (validation$SalePrice-mean(validation$SalePrice))^2
rSquare = 1 - sum(rss)/sum(tss)
rmse(validation$SalePrice,pred_on_train)
rmse
rSquare

#-----write to a file----------------
pred_on_test <- predict(lasso, newx = as.matrix(testSet[, -233]), s = "lambda.min")
sub_avg <- data.frame(Id = testSet$Id, SalePrice = pred_on_test)
head(sub_avg)
write.csv(sub_avg, file = 'archana.csv', row.names = F)
#----------------------------------------------------------------
