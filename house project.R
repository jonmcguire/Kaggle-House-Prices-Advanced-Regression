#load libaries
library(ggplot2)
library(dplyr)
library(forcats)
library(lattice)

#load data
train=read.csv("train.csv")
test=read.csv("Test.csv")

#cleaning

#change years to categorical
train$YearBuilt<-as.factor(train$YearBuilt)
train$YearRemodAdd<-as.factor(train$YearRemodAdd)
train$GarageYrBlt<-as.factor(train$GarageYrBlt)
train$YrSold<-as.factor(train$YrSold)

#change NA to none
train$Alley<-fct_explicit_na(train$Alley,"None")
train$MasVnrType<-fct_explicit_na(train$MasVnrType,"None")
train$BsmtQual<-fct_explicit_na(train$BsmtQual,"None")
train$BsmtCond<-fct_explicit_na(train$BsmtCond,"None")
train$BsmtExposure<-fct_explicit_na(train$BsmtExposure,"None")
train$BsmtFinType1<-fct_explicit_na(train$BsmtFinType1,"None")
train$BsmtFinType2<-fct_explicit_na(train$BsmtFinType2,"None")
train$Electrical<-fct_explicit_na(train$Electrical,"None")
train$FireplaceQu<-fct_explicit_na(train$FireplaceQu,"None")
train$GarageType<-fct_explicit_na(train$GarageType,"None")
train$GarageFinish<-fct_explicit_na(train$GarageFinish,"None")
train$GarageQual<-fct_explicit_na(train$GarageQual,"None")
train$GarageCond<-fct_explicit_na(train$GarageCond,"None")
train$PoolQC<-fct_explicit_na(train$PoolQC,"None")
train$Fence<-fct_explicit_na(train$Fence,"None")
train$MiscFeature<-fct_explicit_na(train$MiscFeature,"None")

test$Alley<-fct_explicit_na(test$Alley,"None")
test$MasVnrType<-fct_explicit_na(test$MasVnrType,"None")
test$BsmtQual<-fct_explicit_na(test$BsmtQual,"None")
test$BsmtCond<-fct_explicit_na(test$BsmtCond,"None")
test$BsmtExposure<-fct_explicit_na(test$BsmtExposure,"None")
test$BsmtFinType1<-fct_explicit_na(test$BsmtFinType1,"None")
test$BsmtFinType2<-fct_explicit_na(test$BsmtFinType2,"None")
test$Electrical<-fct_explicit_na(test$Electrical,"None")
test$FireplaceQu<-fct_explicit_na(test$FireplaceQu,"None")
test$GarageType<-fct_explicit_na(test$GarageType,"None")
test$GarageFinish<-fct_explicit_na(test$GarageFinish,"None")
test$GarageQual<-fct_explicit_na(test$GarageQual,"None")
test$GarageCond<-fct_explicit_na(test$GarageCond,"None")
test$PoolQC<-fct_explicit_na(test$PoolQC,"None")
test$Fence<-fct_explicit_na(test$Fence,"None")
test$MiscFeature<-fct_explicit_na(test$MiscFeature,"None")


#take out variables
train$MSZoning <- NULL
train$LotFrontage <- NULL
train$OverallCond <- NULL
train$BsmtFinSF2 <- NULL
train$LowQualFinSF <- NULL
train$BsmtHalfBath <- NULL
train$KitchenAbvGr <- NULL
train$EnclosedPorch <- NULL
train$X3SsnPorch <- NULL
train$MiscVal <- NULL
train$MasVnrArea <- NULL
train$Alley <- NULL
train$Fence <- NULL
train$MiscFeature <- NULL
train$FireplaceQu <- NULL
train$Utilities <- NULL
train$MasVnrArea <- NULL
train$LotFrontage <- NULL
train$Street <- NULL
train$LandContour <- NULL
train$LandSlope <- NULL
train$Condition1 <- NULL
train$Condition2 <- NULL
train$BldgType <- NULL
train$RoofMatl <- NULL
train$ExterCond <- NULL
train$BsmtCond <- NULL
train$BsmtFinType2 <- NULL
train$Heating <- NULL
train$CentralAir <- NULL
train$Electrical <- NULL
train$PavedDrive <- NULL
train$Functional <- NULL
train$GarageQual <- NULL
train$GarageCond <- NULL
train$SaleCondition <- NULL
train$SaleType <- NULL
train$PoolQC<-NULL
train$ScreenPorch<-NULL
train$Neighborhood<-NULL
train$Exterior1st<-NULL
train$Exterior2nd<-NULL
train$PoolArea<-NULL
train$OpenPorchSF<-NULL
train$Id<-NULL
train$BsmtFinSF1<-NULL
train$BsmtFullBath<-NULL
train$BsmtUnfSF<-NULL
train$ExterQual<-NULL
train$Foundation<-NULL
train$GarageArea<-NULL
train$GarageCars<-NULL
train$GarageFinish<-NULL
train$GarageType<-NULL
train$GarageYrBlt<-NULL
train$LotArea<-NULL
train$LotShape<-NULL
train$MasVnrType<-NULL
train$MoSold<-NULL
train$RoofStyle<-NULL
train$TotalBsmtSF<-NULL
train$TotRmsAbvGrd<-NULL
train$X1stFlrSF<-NULL
train$X2ndFlrSF<-NULL
train$YearBuilt<-NULL
train$YearRemodAdd<-NULL
train$YrSold<-NULL
train$Fireplaces<-NULL
train$WoodDeckSF<-NULL
train$BsmtExposure<-NULL
train$HalfBath<-NULL
train$LotConfig<-NULL

test$MSZoning <- NULL
test$LotFrontage <- NULL
test$OverallCond <- NULL
test$BsmtFinSF2 <- NULL
test$LowQualFinSF <- NULL
test$BsmtHalfBath <- NULL
test$KitchenAbvGr <- NULL
test$EnclosedPorch <- NULL
test$X3SsnPorch <- NULL
test$MiscVal <- NULL
test$MasVnrArea <- NULL
test$Alley <- NULL
test$Fence <- NULL
test$MiscFeature <- NULL
test$FireplaceQu <- NULL
test$Utilities <- NULL
test$MasVnrArea <- NULL
test$LotFrontage <- NULL
test$Street <- NULL
test$LandContour <- NULL
test$LandSlope <- NULL
test$Condition1 <- NULL
test$Condition2 <- NULL
test$BldgType <- NULL
test$RoofMatl <- NULL
test$ExterCond <- NULL
test$BsmtCond <- NULL
test$BsmtFinType2 <- NULL
test$Heating <- NULL
test$CentralAir <- NULL
test$Electrical <- NULL
test$PavedDrive <- NULL
test$Functional <- NULL
test$GarageQual <- NULL
test$GarageCond <- NULL
test$SaleCondition <- NULL
test$SaleType <- NULL
test$PoolQC<-NULL
test$ScreenPorch<-NULL
test$Neighborhood<-NULL
test$Exterior1st<-NULL
test$Exterior2nd<-NULL
test$PoolArea<-NULL
test$OpenPorchSF<-NULL
test$BsmtFinSF1<-NULL
test$BsmtFullBath<-NULL
test$BsmtUnfSF<-NULL
test$ExterQual<-NULL
test$Foundation<-NULL
test$GarageArea<-NULL
test$GarageCars<-NULL
test$GarageFinish<-NULL
test$GarageType<-NULL
test$GarageYrBlt<-NULL
test$LotArea<-NULL
test$LotShape<-NULL
test$MasVnrType<-NULL
test$MoSold<-NULL
test$RoofStyle<-NULL
test$TotalBsmtSF<-NULL
test$TotRmsAbvGrd<-NULL
test$X1stFlrSF<-NULL
test$X2ndFlrSF<-NULL
test$YearBuilt<-NULL
test$YearRemodAdd<-NULL
test$YrSold<-NULL
test$Fireplaces<-NULL
test$WoodDeckSF<-NULL
test$BsmtExposure<-NULL
test$HalfBath<-NULL
test$LotConfig<-NULL


#get rid of outlier
train<-train[-c(1299,524),]

#normalize sales price
train$SalePrice <- log(train$SalePrice)

summary(train)
str(train)

null=lm(train$SalePrice~1,data=train)
full=lm(train$SalePrice~.,data=train)
#step(null,scope=list(lower=null,upper=full),direction="forward")

#step(full, data=Housing, direction="backward")
step(null, scope = list(upper=full), data=Housing, direction="both")

ggplot(data=train,aes(x=train$LotConfig,y=train$SalePrice))+geom_point()



#model<-lm(formula = train$SalePrice ~ OverallQual + log(GrLivArea) + BsmtQual + 
#            HouseStyle + KitchenQual + MSSubClass + BsmtFinType1 + 
#            FullBath + 
#            HeatingQC + BedroomAbvGr, data = train)
model<-lm(formula = train$SalePrice ~ OverallQual + GrLivArea + HouseStyle + 
            BsmtFinType1 + BsmtQual + KitchenQual + MSSubClass + HeatingQC, 
          data = train)

summary(model)
confint(model, conf.level=0.95)

plot(model)
par(mfrow=c(2,2))

pairs(train)

str(train)

num<-predict(model,test)

num<- exp(num)
num
write.csv(num,"results.csv")
actual<-data.frame(num,test$Id)
test[96,"KitchenQual"]<-"TA"

actual
