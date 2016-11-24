#########################################################################################################################################
# Author              : Mihir Sanghvi                             
# Email Address       : mihir.sanghvi@uconn.edu                   
# Date                : 11/23/2016                                
# Dataset Source      : http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_pums_csv_2015&prodType=document
# Note                : Downloaded Connecticut Housing Unit Records Dataset from the above mentioned  link
#Problem Definition   : Predicting Rental Property Prices based on the Rental Property Characteristics using US Census Housing Data
#########################################################################################################################################

# Remove comment from following code to install packages 

# install.packages("dplyr")
# install.packages("car")
# install.packages("MASS")
# install.packages("corrgram")
# install.packages("Ckmeans.1d.dp")
# install.packages("xgboost")
# install.packages("leaps")

# Load Packages 
library(dplyr)
library(car)
library(MASS)
library(corrgram)
library(Ckmeans.1d.dp)
library(xgboost)
library(leaps)

##################################################################################################################
# Created following functions
##################################################################################################################

#1. Created following function to identify missing values in any dataframe 
Missing = function(dataFrame){
  
  # Check if the provided argument is a dataframe or not 
  if(! is.data.frame(dataFrame)){
    stop("Please provide data frame in the argument")
  }
  
  # compute total number of columns in the dataframe 
  totalCols = ncol(dataFrame)
  
  # Create empty vector to store total number of missing values 
  totalMissingValues <- numeric()
  
  # iterate from 1 to number of cols   
  for(i in 1:totalCols){
    # compute number of missing values for each column and save it into vector
    totalMissingValues <-  c(totalMissingValues,sum(is.na(dataFrame[,i])))  
  }
  
  # create new dataframe as required
  newFrame = data.frame(colnames(dataFrame),totalMissingValues)
  
  # provide column names 
  colnames(newFrame) <- c("Column Name","No. of Missing Values")
  return(newFrame)  
}

#2. Created following function to evaulaute model performance 

NumMetrics = function(a,m)  #here a is target value matrix and m is model value matrix
{
  metrics = c(MAD=0, MSE = 0, MAPE = 0, MPSE = 0, tMAD = 0, p90=0, R2 =0)
  metrics["MAD"] = mean(abs(a-m))
  metrics["MSE"] = mean((a-m)^2)
  metrics["MAPE"] = mean(abs(a-m)*100/a)
  metrics["MPSE"] = mean((((a-m)/a)^2)*100)
  metrics["tMAD"] = mean(abs(a-m),trim = 0.05)
  metrics["p90"] = quantile(abs(a-m),probs = 0.9)
  
  SST = sum((a-mean(a))^2)
  SSE = sum((a-m)^2)
  metrics["R2"] = 1- (SSE/SST)
  return(metrics)
}


# Load dataset
CTHousingData <- read.csv("F:/Atomized-Assignment/ss15hct.csv")

##########################################################################################################################
# Target Variable - RNTP - Rent Amount                                                                        
##########################################################################################################################

# Following Columns will be same for a given State, to use this column we need to download Dataset from multiple states 
# REGION, DIVISION 

###############################################################################################################

# The following variables are only applicable for Puerto Rico 
# PLMPRP,RWATPR,HOTWAT -  N/A Means GQ 

###############################################################################################################

###############################################################################################################
# DO NOT USE FOLLOWING COLS IN MODEL BUILDING                                                                 #
###############################################################################################################
# INSP,TAXP -  N/A means GQ/vacant/not owned or being bought - (NA for Rent Housing Data Can't Use in model)

# CONP,VALP - N/A Means GQ/vacant units, except "for sale-only" 
##          and "sold, not occupied"/not owned or being bought - (NA for Rent Housing Data Can't Use in model)

###############################################################################################################



###############################################################################################################
# Filter on following Columns to get rental properties data                                                   #
###############################################################################################################
# NP - Vacent Unit (0) [Remove All 0s]
# WGTP - Group Quaters -GQ (0) [Remove All 0s]
# TYPE - Housing unit(1) [Keep only 1s]
# TEN - Rented(3) [Keep Only 3s]
###############################################################################################################

# Total 235 Columns are present in the datasat 
nrow(CTHousingData)

CTHousingRentalData <- filter(CTHousingData,NP>0,WGTP>0,TYPE==1,TEN==3)


CTHousingRentalFeatures <- CTHousingRentalData[,c("WGTP","NP","ACR","BATH","BDSP","BLD","BUS","ELEP","FULP","GASP",
                                                  "HFL","REFR","RMSP","RNTM","RNTP","RWAT","STOV","TOIL","WATP","PLM")]


###############################################################################################################
# Data Cleaning - Variable Value Recoding 
###############################################################################################################

# Need to recode BATH Variable values (Currently 1=Yes, 2 = NO, Converted it to 0 = No, 1= Yes)
CTHousingRentalFeatures$BATH <- ifelse(CTHousingRentalFeatures$BATH ==2,0,1)

# BLD is an ordered column, need to convert it from int to ordered data type 
CTHousingRentalFeatures$BLD <- ordered(CTHousingRentalFeatures$BLD)

# HFL Column is a nominal variable, need to convert from int to factor data type 
CTHousingRentalFeatures$HFL <- factor(CTHousingRentalFeatures$HFL)

# Need to recode REFR Variable values (Currently 1=Yes, 2 = NO, Converted it to 0 = No, 1= Yes)
CTHousingRentalFeatures$REFR <- ifelse(CTHousingRentalFeatures$REFR ==2,0,1)

# Need to recode RNTM Variable values (Currently 1=Yes, 2 = NO, Converted it to 0 = No, 1= Yes)
CTHousingRentalFeatures$RNTM <- ifelse(CTHousingRentalFeatures$RNTM ==2,0,1)

# Need to recode RWAT Variable values (Currently 1=Yes, 2 = NO, Converted it to 0 = No, 1= Yes)
CTHousingRentalFeatures$RWAT <- ifelse(CTHousingRentalFeatures$RWAT ==2,0,CTHousingRentalFeatures$RWAT)

# Need to recode STOV Variable values (Currently 1=Yes, 2 = NO, Converted it to 0 = No, 1= Yes)
CTHousingRentalFeatures$STOV <- ifelse(CTHousingRentalFeatures$STOV ==2,0,1)

# Need to recode TOIL Variable values (Currently 1=Yes, 2 = NO, Converted it to 0 = No, 1= Yes)
CTHousingRentalFeatures$TOIL <- ifelse(CTHousingRentalFeatures$TOIL ==2,0,1)

# Need to recode PLM Variable values (Currently 1=Yes, 2 = NO, Converted it to 0 = No, 1= Yes)
CTHousingRentalFeatures$PLM <- ifelse(CTHousingRentalFeatures$PLM ==2,0,CTHousingRentalFeatures$PLM)


###############################################################################################################
# Missing Value Check 
###############################################################################################################
# Check Missing Values 
Missing(CTHousingRentalFeatures)


# Following variables have same pattern in missing data 
# BUS, ACR - N/A Means GQ/not a one-family house or mobile home
# Since I have flitered GQ, the missing value is a new feature (Not one Family House or Mobile Home)

CTHousingRentalFeatures$ACR <- ifelse(is.na(CTHousingRentalFeatures$ACR),4,CTHousingRentalFeatures$ACR)
# ACR Column is a nominal variable, need to convert from int to factor data type 
CTHousingRentalFeatures$ACR <- factor(CTHousingRentalFeatures$ACR)


CTHousingRentalFeatures$BUS <- ifelse(is.na(CTHousingRentalFeatures$BUS),3,CTHousingRentalFeatures$BUS)
# BUS Column is a nominal variable, need to convert from int to factor data type 
CTHousingRentalFeatures$BUS <- factor(CTHousingRentalFeatures$BUS)

###############################################################################################################
# Performed Feature Engineering on following columns
# ELEP,FULP,GASP,WATP
###############################################################################################################

###################################################
# ELEP - Electricity (monthly cost)
#   bbb .N/A (GQ/vacant)
#   001 .Included in rent or in condo fee
#   002 .No charge or electricity not used
#   003..999 .$3 to $999 (Rounded and top-coded)

# As we can see above, 
# 1 refers to the case when electricity cost is added into monthly rent or condo fee 
# 2 refers to the case when no electricity cost is taken from tenants/ not used 
# We dont need to worry about N/A cases because we have already filtered out all GQ (Group Quaters) 
# and Vacent household data 
###################################################

# Created new Dummy variables for above mentioned cases 
CTHousingRentalFeatures$ELEPIR <- ifelse(CTHousingRentalFeatures$ELEP==1,1,0)
CTHousingRentalFeatures$ELEPNC <- ifelse(CTHousingRentalFeatures$ELEP==2,1,0)

# After creating dummy variables, replaced 1 & 2 with 0s in the ELEP column 
CTHousingRentalFeatures$ELEP <- ifelse(CTHousingRentalFeatures$ELEP>2,CTHousingRentalFeatures$ELEP,0)


###################################################
# Created features similarly for FULP,GASP,WATP
###################################################

# Created new Dummy variables for FULP
CTHousingRentalFeatures$FULPIR <- ifelse(CTHousingRentalFeatures$FULP==1,1,0)
CTHousingRentalFeatures$FULPNC <- ifelse(CTHousingRentalFeatures$FULP==2,1,0)

# After creating dummy variables, replaced 1 & 2 with 0s in the FULP column 
CTHousingRentalFeatures$FULP <- ifelse(CTHousingRentalFeatures$FULP>2,CTHousingRentalFeatures$FULP,0)

###################################################

# Created new Dummy variables for GASP

CTHousingRentalFeatures$GASPIR <- ifelse(CTHousingRentalFeatures$GASP==1,1,0)
CTHousingRentalFeatures$GASPIE <- ifelse(CTHousingRentalFeatures$GASP==2,1,0)
CTHousingRentalFeatures$GASPNR <- ifelse(CTHousingRentalFeatures$GASP==3,1,0)

# After creating dummy variables, replaced 1 & 2 with 0s in the GASP column 
CTHousingRentalFeatures$GASP <- ifelse(CTHousingRentalFeatures$GASP>3,CTHousingRentalFeatures$GASP,0)

###################################################

# Created new Dummy variables for WATP
CTHousingRentalFeatures$WATPIR <- ifelse(CTHousingRentalFeatures$WATP == 1,1,0)
CTHousingRentalFeatures$WATPNC <- ifelse(CTHousingRentalFeatures$WATP == 2,1,0)

# After creating dummy variables, replaced 1 & 2 with 0s in the WATP column 
CTHousingRentalFeatures$WATP <- ifelse(CTHousingRentalFeatures$WATP>2,CTHousingRentalFeatures$WATP,0)
###############################################################################################################


###############################################################################################################
# Categorical Variables 
###############################################################################################################

# BLD - Units in structure                     (Ordinal)
# ACR - Lot size - Factor                      (Nominal)
# BUS - Business or medical office on property (Nominal)
# HFL - House heating fuel                     (Nominal)

###############################################################################################################
# Continous Variables 
###############################################################################################################

# RNTP - Monthly rent  - TARGET VARIABLE


# BDSP - Number of bedrooms 
# RMSP - Number of Rooms 
# ELEP - Electricity (monthly cost) 
# FULP - Fuel cost(yearly cost for fuels other than gas and electricity)
# GASP - Gas (monthly cost)
# WATP - Water (yearly cost) 
# WGTP - Housing Weight 
# NP   - Number of person records following this housing record 

###############################################################################################################
# Binary Dummy Variables 
###############################################################################################################

# BATH - Bathtub or shower 
# REFR - Refrigerator 
# RNTM - Meals included in rent
# RWAT - Hot and cold running water 
# STOV - Stove or range 
# TOIL - Flush toilet 
# PLM  - Complete plumbing facilities
# ELEPIR - Electricity included in rent or in condo fee 
# ELEPNC - No charge or electricity not used


# FULPIR - Fuel included in rent or in condo fee
# FULPNC - No charge or these fuels not used

# GASPIR - Gas included in rent or in condo fee
# GASPIE - Included in electricity payment
# GASPNR - No charge or gas not used

# WATPIR - Water included in rent or in condo fee
# WATPNC - No charge 

###############################################################################################################
# Data Visualization / Exploratory Data Analysis 
###############################################################################################################



# Attach Dataframe to the R work environment 
attach(CTHousingRentalFeatures)

# Visualization-1: Categorical variables with Rent Amount - Box Plot
opar = par()
par(mfrow=c(2,2))
boxplot(RNTP~BLD, data = CTHousingRentalFeatures, col = "green", notch = T)
title(main = "Rent Vs. Units in structure")
boxplot(RNTP~ACR, data = CTHousingRentalFeatures, col = "green", notch = T)
title(main = "Rent Vs. Lot size")
boxplot(RNTP~BUS, data = CTHousingRentalFeatures, col = "green", notch = T)
title(main = "Rent Vs. Business or Medical Office on Property")
boxplot(RNTP~HFL, data = CTHousingRentalFeatures, col = "green", notch = T)
title(main = "Rent Vs. House Heating Fuel Type")
par(opar)

# Visualization-2: Continuous variables with Rent Amount - Scatter Plot

plot(RMSP,RNTP, main="Number of Rooms Vs. Rent", 
     xlab="Number of Rooms", ylab="Rent", pch=19)

plot(NP,RNTP, main="Number of People Living in the Property Vs. Rent", 
     xlab="Number of People Living", ylab="Rent", pch=19, col="blue")


# Visualization-3: Continuous variables with Rent Amount - Corrgram (Correlation Analysis)

corrgram(CTHousingRentalFeatures,order = T, upper.panel = panel.pie)


###############################################################################################################
# Variable Transformation 
###############################################################################################################

# Finding potential transformation for Target Variable RNTP
reg1 = lm(RNTP~.,data = CTHousingRentalFeatures)
boxCox(reg1,family="yjPower",plotit = T)

# Square root transformation on target variable (RNTP) will give better results

# Created new variable that is squared root of RNTP 
CTHousingRentalFeatures$SQRT_RNTP = sqrt(CTHousingRentalFeatures$RNTP)

# Tried to findout possible transformations on predictor variables using box tidwell,
# However it was not shoing meaningful transformations, thus not performaing any transformation on predictor variable 

###############################################################################################################
# Make 75-25 Split for training and test dataset 
###############################################################################################################

sampleSize <- floor(0.75 * nrow(CTHousingRentalFeatures))

# Set the seed so that partition is reproductible
set.seed(123)

train_ind <- sample(seq_len(nrow(CTHousingRentalFeatures)), size = sampleSize)

trainRentalData <- CTHousingRentalFeatures[train_ind, ]
testRentalData <- CTHousingRentalFeatures[-train_ind, ]



###############################################################################################################
#                                        MODELING                                                             #
###############################################################################################################

#############################################################################
#  XGBoost Model
#############################################################################

trainRentalData[] <- lapply(trainRentalData, as.numeric)
testRentalData[] <- lapply(testRentalData, as.numeric)


cnt <- rep(0, nrow(testRentalData))
control <- 50


for (i in 1:control){
  
  bst1 <- xgboost(data = as.matrix(trainRentalData[,-c(15,30)]),
                  label = as.matrix(trainRentalData$RNTP),
                  eta = 0.3,
                  max_depth = 8,
                  subsample = 0.5,
                  colsample_bytree = 1,
                  nrounds = 50,
                  objective = "reg:linear",
                  eval_metric = "rmse",
                  early.stop.round = 3)

  pred = predict(bst1,as.matrix(testRentalData[,-c(15,30)]))
  cnt <- cnt + pred
}
cnt <- cnt/control

# Save Predictions in a new column in test dataset
testRentalData$predXgboost = cnt

###########################################################################################
# XGBOOST Model Performance & Identifying Top 10 Features 
###########################################################################################
NumMetrics(testRentalData$RNTP,testRentalData$predXgboost)

# Copmute Importance Matrix 
importance_matrix <- xgb.importance(names(CTHousingRentalFeatures[,-c(15,30)]), model = bst1)
xgb.plot.importance(importance_matrix)


# Top 10 influential features 
importance_matrix[order(importance_matrix[,c(1:10)]),]$Feature


###############################################################################################################
# Exterme Value Analysis (Influential Observation Analysis using Cooks Distance)
###############################################################################################################

#Checking for Influential Observations  
cutoff=4/nrow(CTHousingRentalFeatures)
plot(reg1,which=4,cook.levels = cutoff)
abline(h=cutoff,col="red")

# Calculate % Influential Observations in  the data 
cooksDistance <- ifelse(cooks.distance(model = reg1)>cutoff,1,0)
total_Outliers <- table(cooksDistance)[2]
Percentage_Outliers <- (total_Outliers/nrow(CTHousingRentalFeatures))*100
Percentage_Outliers


##################################################################################################
# Best Predictor variables based on regsubsets function from leaps package 
# (Model selection by exhaustive search, forward or backward stepwise, or sequential replacement)
##################################################################################################


regfit = regsubsets(SQRT_RNTP~.-RNTP,data=CTHousingRentalFeatures,nvmax = 10)
summary(regfit)

# From the summary we can see that following variables are best predictor variables 
# RMSP, WATPIR, WATPNC, BLD^4, RNTM, ELEPIR, NP, GASPNR

###############################################################################################
#                             Regression Modeling 
###############################################################################################

###########################################################
#  Linear Regression 
###########################################################
linearReg <- glm(SQRT_RNTP~RMSP+WATPIR+WATPNC+BLD+RNTM+ELEPIR+NP+GASPNR, data=trainRentalData)
summary(linearReg)

# From the Summary we can see that BLD is not a significant variable, thus we can remove that variable 
linearRegUpdated <- glm(SQRT_RNTP~RMSP+WATPIR+WATPNC+RNTM+ELEPIR+NP+GASPNR, data=trainRentalData)

# Save predictions in test dataset 
testRentalData$linearRegPred <- predict(linearRegUpdated,testRentalData)^2

###############################################################
# Identifying 2 way interactions for regression model 
###############################################################

# Checks all 2 way interactions
res = step(linearRegUpdated,~.^2) 

res$anova
# There are no such significant interactions which we can add in our model to improve accuracy 

###############################################################
# Robust  Linear Regression 
# Allocates weights to the observations based on Cooks Distance 
###############################################################

# Since we have seen many outliers in the dataset, we will use robust regression model

robustReg <- rlm(SQRT_RNTP~RMSP+WATPIR+WATPNC+RNTM+ELEPIR+NP+GASPNR, data=trainRentalData)
summary(robustReg)

# Save predictions in test dataset 
testRentalData$robustRegPred <- predict(robustReg,testRentalData)^2

###########################################################################################
# Regression Model Performance Evaluation
###########################################################################################

anova(robustReg,linearRegUpdated)
# Anova results shows that both the model are identical, and there is not much improvement 
# in the robust regression model

AIC(robustReg,linearRegUpdated)
# AIC results also shows that both the models are identical 

  NumMetrics(testRentalData$RNTP,testRentalData$robustRegPred)
  NumMetrics(testRentalData$RNTP,testRentalData$linearRegPred)

# Model evaluation matrix also shows that both the models are explaining almost same amount of variance in 
# target variable 
# We will go with linear Regression Model

###########################################################################################
# Regression Diagnostics
###########################################################################################

summary(linearRegUpdated)


opar = par() 
par(mfrow=c(2,2)) 
plot(linearRegUpdated) 
par(opar)


shapiro.test(x = linearRegUpdated$residuals)
# Shapiro test results suggests that the residuals are not normally distributed 
# (p value is less than 0.05 thus we can reject the null hypothesis that model residuals are normally distrubuted )

vif(linearRegUpdated)
# There is no multicollinearity present in the data 


# Model Estimates 
round(coef(linearRegUpdated),2)^2

# Since we have extereme values in predictor variables, model is not able to satisfy assumption of normality 
# However since model is satisfying assumptions of homoscadisticity and there is no  multicollinearity
# present in the data, model parameter estimates are reliable  

linearReg <- glm(RNTP~RMSP+WATPIR+WATPNC+BLD+RNTM+ELEPIR+NP+GASPNR, data=trainRentalData)
summary(linearReg)

NumMetrics(trainRentalData$RNTP,  predict(linearReg,testRentalData))
