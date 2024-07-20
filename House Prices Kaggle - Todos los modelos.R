# Funciones

## Función para tratar outliers
tratar_outliers <- function(x) {
  cuartiles <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- cuartiles[2] - cuartiles[1]
  limite_inferior <- cuartiles[1] - 1.5 * iqr
  limite_superior <- cuartiles[2] + 1.5 * iqr
  #x <- ifelse(x < limite_inferior, limite_inferior, x)
  x <- ifelse(x > limite_superior, limite_superior, x)
  return(x)
}

# Función para encontrar la moda
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#Carga de data

Data_Test=read.csv("test.csv")
Data_Test_O=read.csv("test.csv")
Data_Train=read.csv("train.csv")
#str(Data_Train)
#str(Data_Test)

#Se cargan las librerias
#install.packages("gbm")
#install.packages("xgboost")
#install.packages("mice")
#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("randomForest")
#install.packages("MASS")
#install.packages("rpart")
#install.packages("rpart.plot")
library(MASS)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(xgboost)
library(randomForest)
library(gbm)
library(rpart)
library(rpart.plot)
#library(mice)



# Analizaremos datos faltantes

missing_values <- colSums(is.na(Data_Train))
missing_values <- sort(missing_values, decreasing = TRUE)
missing_values[missing_values > 0]
missing_values_perct = round(missing_values/nrow(Data_Train),2)
missing_values_perct[missing_values > 0]

missing_values <- colSums(is.na(Data_Test))
missing_values <- sort(missing_values, decreasing = TRUE)
missing_values[missing_values > 0]
missing_values_perct = round(missing_values/nrow(Data_Train),2)
missing_values_perct[missing_values > 0]


#Tratamiento de outliers

Data_Train <- subset(Data_Train, GrLivArea <= 4000)

#Data_Train <- Data_Train %>%mutate(across(where(is.numeric) & !c("SalePrice","GarageYrBlt","MSSubClass","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MoSold","YrSold"), tratar_outliers))
#Data_Test <- Data_Test %>%mutate(across(where(is.numeric) & !c("MSSubClass","GarageYrBlt","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MoSold","YrSold"), tratar_outliers))


#Conversion de data categorica a numerica

Data_Train <- Data_Train %>% mutate(GarageFinish = recode(GarageFinish, Unf = 1, RFn = 2, Fin = 3,.default = 0)) %>% mutate(GarageFinish = as.integer(GarageFinish))
Data_Train <- Data_Train %>% mutate(Fence = recode(Fence, MnWw = 1, GdWo = 2, MnPrv = 3, GdPrv = 4, .default = 0)) %>% mutate(Fence = as.integer(Fence))
Data_Train <- Data_Train %>% mutate(Functional = recode(Functional, Sal = 1, Sev = 2, Maj2 = 3, Maj1 = 4, Mod = 5, Min2 = 6, Min1 = 7, Typ = 8, .default = 0)) %>% mutate(Functional = as.integer(Functional))
Data_Train <- Data_Train %>% mutate(BsmtFinType1 = recode(BsmtFinType1, Unf = 1, LwQ = 2, Rec = 3, BLQ = 4, ALQ = 5, GLQ = 6, .default = 0)) %>% mutate(BsmtFinType1 = as.integer(BsmtFinType1))
Data_Train <- Data_Train %>% mutate(BsmtFinType2 = recode(BsmtFinType2, Unf = 1, LwQ = 2, Rec = 3, BLQ = 4, ALQ = 5, GLQ = 6, .default = 0)) %>% mutate(BsmtFinType2 = as.integer(BsmtFinType2))
Data_Train <- Data_Train %>% mutate(BsmtExposure = recode(BsmtExposure, No = 1, Mn = 2, Av = 3, Gd = 4, .default = 0)) %>% mutate(BsmtExposure = as.integer(BsmtExposure))
Data_Train <- Data_Train %>% mutate(ExterQual = recode(ExterQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(ExterQual = as.integer(ExterQual))
Data_Train <- Data_Train %>% mutate(ExterCond = recode(ExterCond, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(ExterCond = as.integer(ExterCond))
Data_Train <- Data_Train %>% mutate(BsmtQual = recode(BsmtQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(BsmtQual = as.integer(BsmtQual))
Data_Train <- Data_Train %>% mutate(BsmtCond = recode(BsmtCond, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(BsmtCond = as.integer(BsmtCond))
Data_Train <- Data_Train %>% mutate(HeatingQC = recode(HeatingQC, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(HeatingQC = as.integer(HeatingQC))
Data_Train <- Data_Train %>% mutate(KitchenQual = recode(KitchenQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(KitchenQual = as.integer(KitchenQual))
Data_Train <- Data_Train %>% mutate(FireplaceQu = recode(FireplaceQu, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(FireplaceQu = as.integer(FireplaceQu))
Data_Train <- Data_Train %>% mutate(GarageQual = recode(GarageQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(GarageQual = as.integer(GarageQual))
Data_Train <- Data_Train %>% mutate(GarageCond = recode(GarageCond, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(GarageCond = as.integer(GarageCond))
Data_Train <- Data_Train %>% mutate(PoolQC = recode(PoolQC, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(PoolQC = as.integer(PoolQC))
Data_Train <- Data_Train %>% mutate(CentralAir = recode(CentralAir, Y = 1, N = 0, .default = 0)) %>% mutate(CentralAir = as.integer(CentralAir))
Data_Train <- Data_Train %>% mutate(PavedDrive = recode(PavedDrive, Y = 2, P=1, N = 0, .default = 0)) %>% mutate(PavedDrive = as.integer(PavedDrive))

Data_Test <- Data_Test %>% mutate(GarageFinish = recode(GarageFinish, Unf = 1, RFn = 2, Fin = 3,.default = 0)) %>% mutate(GarageFinish = as.integer(GarageFinish))
Data_Test <- Data_Test %>% mutate(Fence = recode(Fence, MnWw = 1, GdWo = 2, MnPrv = 3, GdPrv = 4, .default = 0)) %>% mutate(Fence = as.integer(Fence))
Data_Test <- Data_Test %>% mutate(Functional = recode(Functional, Sal = 1, Sev = 2, Maj2 = 3, Maj1 = 4, Mod = 5, Min2 = 6, Min1 = 7, Typ = 8, .default = 0)) %>% mutate(Functional = as.integer(Functional))
Data_Test <- Data_Test %>% mutate(BsmtFinType1 = recode(BsmtFinType1, Unf = 1, LwQ = 2, Rec = 3, BLQ = 4, ALQ = 5, GLQ = 6, .default = 0)) %>% mutate(BsmtFinType1 = as.integer(BsmtFinType1))
Data_Test <- Data_Test %>% mutate(BsmtFinType2 = recode(BsmtFinType2, Unf = 1, LwQ = 2, Rec = 3, BLQ = 4, ALQ = 5, GLQ = 6, .default = 0)) %>% mutate(BsmtFinType2 = as.integer(BsmtFinType2))
Data_Test <- Data_Test %>% mutate(BsmtExposure = recode(BsmtExposure, No = 1, Mn = 2, Av = 3, Gd = 4, .default = 0)) %>% mutate(BsmtExposure = as.integer(BsmtExposure))
Data_Test <- Data_Test %>% mutate(ExterQual = recode(ExterQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(ExterQual = as.integer(ExterQual))
Data_Test <- Data_Test %>% mutate(ExterCond = recode(ExterCond, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(ExterCond = as.integer(ExterCond))
Data_Test <- Data_Test %>% mutate(BsmtQual = recode(BsmtQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(BsmtQual = as.integer(BsmtQual))
Data_Test <- Data_Test %>% mutate(BsmtCond = recode(BsmtCond, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(BsmtCond = as.integer(BsmtCond))
Data_Test <- Data_Test %>% mutate(HeatingQC = recode(HeatingQC, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(HeatingQC = as.integer(HeatingQC))
Data_Test <- Data_Test %>% mutate(KitchenQual = recode(KitchenQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(KitchenQual = as.integer(KitchenQual))
Data_Test <- Data_Test %>% mutate(FireplaceQu = recode(FireplaceQu, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(FireplaceQu = as.integer(FireplaceQu))
Data_Test <- Data_Test %>% mutate(GarageQual = recode(GarageQual, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(GarageQual = as.integer(GarageQual))
Data_Test <- Data_Test %>% mutate(GarageCond = recode(GarageCond, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(GarageCond = as.integer(GarageCond))
Data_Test <- Data_Test %>% mutate(PoolQC = recode(PoolQC, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5, .default = 0)) %>% mutate(PoolQC = as.integer(PoolQC))
Data_Test <- Data_Test %>% mutate(CentralAir = recode(CentralAir, Y = 1, N = 0, .default = 0)) %>% mutate(CentralAir = as.integer(CentralAir))
Data_Test <- Data_Test %>% mutate(PavedDrive = recode(PavedDrive, Y = 2, P=1, N = 0, .default = 0)) %>% mutate(PavedDrive = as.integer(PavedDrive))
Data_Test <- Data_Test %>%mutate(MSSubClass = ifelse(MSSubClass == 150, 160, MSSubClass))


#Tratando datos faltantes

Data_Test <- Data_Test %>% mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt))
Data_Train <- Data_Train %>% mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt))

Data_Train <- Data_Train %>% mutate(Alley = ifelse(is.na(Alley), "ND", Alley))
Data_Test <- Data_Test %>% mutate(Alley = ifelse(is.na(Alley), "ND", Alley))

Data_Train <- Data_Train %>% mutate_if(~ !is.numeric(.), ~ replace_na(., get_mode(.)))
Data_Train <- Data_Train %>% mutate_if(is.numeric, ~ replace_na(., 0))
Data_Test <- Data_Test %>% mutate_if(~ !is.numeric(.), ~ replace_na(., get_mode(.)))
Data_Test <- Data_Test %>% mutate_if(is.numeric, ~ replace_na(., 0))

#MSSubClass, existe un valor unico en el Data_Test (150), se reemplaza por 


#Eliminando variables con alta cantidad de variables faltantes e Id

Data_Train <- subset(Data_Train,select = -Id)
#Data_Train <- subset(Data_Train,select = -PoolQC)
#Data_Train <- subset(Data_Train,select = -Fence)
Data_Train <- subset(Data_Train,select = -MiscFeature)
#Data_Train <- subset(Data_Train,select = -FireplaceQu)
#Data_Train <- subset(Data_Train,select = -Alley)

Data_Test <- subset(Data_Test,select = -Id)
#Data_Test <- subset(Data_Test,select = -PoolQC)
#Data_Test <- subset(Data_Test,select = -Fence)
Data_Test <- subset(Data_Test,select = -MiscFeature)
#Data_Test <- subset(Data_Test,select = -FireplaceQu)
#Data_Test <- subset(Data_Test,select = -Alley)



#Se convierte las variables categorias a factor

Data_Test$MSSubClass <-as.factor(Data_Test$MSSubClass)
#Data_Test$OverallQual <-as.factor(Data_Test$POverallQual)
#Data_Test$OverallCond <-as.factor(Data_Test$OverallCond)
#Data_Test$YearBuilt <-as.factor(Data_Test$YearBuilt)
#Data_Test$YearRemodAdd <-as.factor(Data_Test$YearRemodAdd)
Data_Test$MoSold <-as.factor(Data_Test$MoSold)
#Data_Test$YrSold <-as.factor(Data_Test$YrSold)
#Data_Test$PoolQC <-as.factor(Data_Test$PoolQC)
#Data_Test$MiscFeature <-as.factor(Data_Test$MiscFeature)
Data_Test$Alley <-as.factor(Data_Test$Alley)
#Data_Test$Fence <-as.factor(Data_Test$Fence)
#Data_Test$FireplaceQu <-as.factor(Data_Test$FireplaceQu)
Data_Test$MSZoning <-as.factor(Data_Test$MSZoning)
Data_Test$Street <- as.factor(Data_Test$Street)
Data_Test$LotShape <- as.factor(Data_Test$LotShape)
Data_Test$LandContour <- as.factor(Data_Test$LandContour)
Data_Test$Utilities<- as.factor(Data_Test$Utilities)
Data_Test$LotConfig <- as.factor(Data_Test$LotConfig)
Data_Test$LotConfig<- as.factor(Data_Test$LotConfig)
Data_Test$LandSlope<- as.factor(Data_Test$LandSlope)
Data_Test$Neighborhood<- as.factor(Data_Test$Neighborhood)
Data_Test$Condition1 <- as.factor(Data_Test$Condition1)
Data_Test$Condition2 <- as.factor(Data_Test$Condition2)
Data_Test$BldgType <- as.factor(Data_Test$BldgType)
Data_Test$HouseStyle <- as.factor(Data_Test$HouseStyle)
Data_Test$RoofStyle <- as.factor(Data_Test$RoofStyle)
Data_Test$RoofMatl <- as.factor(Data_Test$RoofMatl)
Data_Test$Exterior1st<- as.factor(Data_Test$Exterior1st)
Data_Test$Exterior2nd<- as.factor(Data_Test$Exterior2nd)
Data_Test$MasVnrType <- as.factor(Data_Test$MasVnrType)
#Data_Test$ExterQual <- as.factor(Data_Test$ExterQual)
#Data_Test$ExterCond <- as.factor(Data_Test$ExterCond)
Data_Test$Foundation <- as.factor(Data_Test$Foundation)
#Data_Test$BsmtQual <- as.factor(Data_Test$BsmtQual)
#Data_Test$BsmtCond <- as.factor(Data_Test$BsmtCond)
#Data_Test$BsmtFinType1 <- as.factor(Data_Test$BsmtFinType1)
#Data_Test$BsmtFinType2 <- as.factor(Data_Test$BsmtFinType2)
Data_Test$Heating<- as.factor(Data_Test$Heating)
#Data_Test$HeatingQC <- as.factor(Data_Test$HeatingQC)
Data_Test$CentralAir<- as.factor(Data_Test$CentralAir)
Data_Test$Electrical <- as.factor(Data_Test$Electrical)
#Data_Test$KitchenQual <- as.factor(Data_Test$KitchenQual)
#Data_Test$Functional<- as.factor(Data_Test$Functional)
Data_Test$GarageType <- as.factor(Data_Test$GarageType)
#Data_Test$GarageFinish<- as.factor(Data_Test$GarageFinish)
#Data_Test$GarageQual <- as.factor(Data_Test$GarageQual)
#Data_Test$GarageCond<- as.factor(Data_Test$GarageCond)
Data_Test$PavedDrive<- as.factor(Data_Test$PavedDrive)
Data_Test$SaleType <- as.factor(Data_Test$SaleType)
Data_Test$SaleCondition <- as.factor(Data_Test$SaleCondition)
#Data_Test$BsmtExposure<- as.factor(Data_Test$BsmtExposure)

Data_Train$MSSubClass <-as.factor(Data_Train$MSSubClass)
#Data_Train$OverallQual <-as.factor(Data_Train$POverallQual)
#Data_Train$OverallCond <-as.factor(Data_Train$OverallCond)
#Data_Train$YearBuilt <-as.factor(Data_Train$YearBuilt)
#Data_Train$YearRemodAdd <-as.factor(Data_Train$YearRemodAdd)
Data_Train$MoSold <-as.factor(Data_Train$MoSold)
#Data_Train$YrSold <-as.factor(Data_Train$YrSold)
#Data_Train$PoolQC <-as.factor(Data_Train$PoolQC)
#Data_Train$MiscFeature <-as.factor(Data_Train$MiscFeature)
Data_Train$Alley <-as.factor(Data_Train$Alley)
#Data_Train$Fence <-as.factor(Data_Train$Fence)
#Data_Train$FireplaceQu <-as.factor(Data_Train$FireplaceQu)
Data_Train$MSZoning <-as.factor(Data_Train$MSZoning)
Data_Train$Street <- as.factor(Data_Train$Street)
Data_Train$LotShape <- as.factor(Data_Train$LotShape)
Data_Train$LandContour <- as.factor(Data_Train$LandContour)
Data_Train$Utilities<- as.factor(Data_Train$Utilities)
Data_Train$LotConfig <- as.factor(Data_Train$LotConfig)
Data_Train$LotConfig<- as.factor(Data_Train$LotConfig)
Data_Train$LandSlope<- as.factor(Data_Train$LandSlope)
Data_Train$Neighborhood<- as.factor(Data_Train$Neighborhood)
Data_Train$Condition1 <- as.factor(Data_Train$Condition1)
Data_Train$Condition2 <- as.factor(Data_Train$Condition2)
Data_Train$BldgType <- as.factor(Data_Train$BldgType)
Data_Train$HouseStyle <- as.factor(Data_Train$HouseStyle)
Data_Train$RoofStyle <- as.factor(Data_Train$RoofStyle)
Data_Train$RoofMatl <- as.factor(Data_Train$RoofMatl)
Data_Train$Exterior1st<- as.factor(Data_Train$Exterior1st)
Data_Train$Exterior2nd<- as.factor(Data_Train$Exterior2nd)
Data_Train$MasVnrType <- as.factor(Data_Train$MasVnrType)
#Data_Train$ExterQual <- as.factor(Data_Train$ExterQual)
#Data_Train$ExterCond <- as.factor(Data_Train$ExterCond)
Data_Train$Foundation <- as.factor(Data_Train$Foundation)
#Data_Train$BsmtQual <- as.factor(Data_Train$BsmtQual)
#Data_Train$BsmtCond <- as.factor(Data_Train$BsmtCond)
#Data_Train$BsmtFinType1 <- as.factor(Data_Train$BsmtFinType1)
#Data_Train$BsmtFinType2 <- as.factor(Data_Train$BsmtFinType2)
Data_Train$Heating<- as.factor(Data_Train$Heating)
#Data_Train$HeatingQC <- as.factor(Data_Train$HeatingQC)
Data_Train$CentralAir<- as.factor(Data_Train$CentralAir)
Data_Train$Electrical <- as.factor(Data_Train$Electrical)
#Data_Train$KitchenQual <- as.factor(Data_Train$KitchenQual)
#Data_Train$Functional<- as.factor(Data_Train$Functional)
Data_Train$GarageType <- as.factor(Data_Train$GarageType)
#Data_Train$GarageFinish<- as.factor(Data_Train$GarageFinish)
#Data_Train$GarageQual <- as.factor(Data_Train$GarageQual)
#Data_Train$GarageCond<- as.factor(Data_Train$GarageCond)
Data_Train$PavedDrive<- as.factor(Data_Train$PavedDrive)
Data_Train$SaleType <- as.factor(Data_Train$SaleType)
Data_Train$SaleCondition <- as.factor(Data_Train$SaleCondition)
#Data_Train$BsmtExposure<- as.factor(Data_Train$BsmtExposure)



#Ajustando niveles(MSSubClass,OverallQual,OverallCond,YearBuilt,YearRemodAdd,MoSold,YrSold)

Data_Test$MSSubClass <-factor(Data_Test$MSSubClass, levels = levels(Data_Train$MSSubClass))
Data_Test$MSZoning <- factor(Data_Test$MSZoning, levels = levels(Data_Train$MSZoning))
Data_Test$Street <- factor(Data_Test$Street, levels = levels(Data_Train$Street))
Data_Test$Alley <- factor(Data_Test$Alley, levels = levels(Data_Train$Alley))
Data_Test$LotShape <- factor(Data_Test$LotShape, levels = levels(Data_Train$LotShape))
Data_Test$LandContour <- factor(Data_Test$LandContour, levels = levels(Data_Train$LandContour))
Data_Test$Utilities<- factor(Data_Test$Utilities, levels = levels(Data_Train$Utilities))
Data_Test$LotConfig <- factor(Data_Test$LotConfig, levels = levels(Data_Train$LotConfig))
Data_Test$LandSlope <- factor(Data_Test$LandSlope, levels = levels(Data_Train$LandSlope))
Data_Test$Neighborhood <- factor(Data_Test$Neighborhood, levels = levels(Data_Train$Neighborhood))
Data_Test$Condition1 <- factor(Data_Test$Condition1, levels = levels(Data_Train$Condition1))
Data_Test$Condition2 <- factor(Data_Test$Condition2, levels = levels(Data_Train$Condition2))
Data_Test$BldgType <- factor(Data_Test$BldgType, levels = levels(Data_Train$BldgType))
Data_Test$HouseStyle <- factor(Data_Test$HouseStyle, levels = levels(Data_Train$HouseStyle))
Data_Test$RoofStyle <- factor(Data_Test$RoofStyle, levels = levels(Data_Train$RoofStyle))
Data_Test$RoofMatl <- factor(Data_Test$RoofMatl, levels = levels(Data_Train$RoofMatl))
Data_Test$Exterior1st<- factor(Data_Test$Exterior1st, levels = levels(Data_Train$Exterior1st))
Data_Test$Exterior2nd<- factor(Data_Test$Exterior2nd, levels = levels(Data_Train$Exterior2nd))
Data_Test$MasVnrType <- factor(Data_Test$MasVnrType, levels = levels(Data_Train$MasVnrType))
Data_Test$Foundation <- factor(Data_Test$Foundation, levels = levels(Data_Train$Foundation))
Data_Test$Heating<- factor(Data_Test$Heating, levels = levels(Data_Train$Heating))
Data_Test$CentralAir<- factor(Data_Test$CentralAir, levels = levels(Data_Train$CentralAir))
Data_Test$Electrical <- factor(Data_Test$Electrical, levels = levels(Data_Train$Electrical))
Data_Test$GarageType <- factor(Data_Test$GarageType, levels = levels(Data_Train$GarageType))
Data_Test$PavedDrive<- factor(Data_Test$PavedDrive, levels = levels(Data_Train$PavedDrive))
Data_Test$MoSold <-factor(Data_Test$MoSold, levels = levels(Data_Train$MoSold))
Data_Test$SaleType <- factor(Data_Test$SaleType, levels = levels(Data_Train$SaleType))
Data_Test$SaleCondition <- factor(Data_Test$SaleCondition, levels = levels(Data_Train$SaleCondition))

#Data_Test$OverallQual <-factor(Data_Test$OverallQual, levels = levels(Data_Train$OverallQual))
#Data_Test$OverallCond <-factor(Data_Test$OverallCond, levels = levels(Data_Train$OverallCond))
#Data_Test$YearBuilt <-factor(Data_Test$YearBuilt, levels = levels(Data_Train$YearBuilt))
#Data_Test$YearRemodAdd <-factor(Data_Test$YearRemodAdd, levels = levels(Data_Train$YearRemodAdd))
#Data_Test$YrSold <-factor(Data_Test$YrSold, levels = levels(Data_Train$YrSold))
#Data_Test$PoolQC <-factor(Data_Test$PoolQC, levels = levels(Data_Train$ PoolQC))
#Data_Test$MiscFeature <-factor(Data_Test$MiscFeature, levels = levels(Data_Train$ MiscFeature))
#Data_Test$Fence <-factor(Data_Test$Fence, levels = levels(Data_Train$ Fence))
#Data_Test$FireplaceQu <-factor(Data_Test$FireplaceQu, levels = levels(Data_Train$ FireplaceQu))
#Data_Test$LotConfig<- factor(Data_Test$LotConfig, levels = levels(Data_Train$ LotConfig))
#Data_Test$ExterQual <- factor(Data_Test$ExterQual, levels = levels(Data_Train$ ExterQual))
#Data_Test$ExterCond <- factor(Data_Test$ExterCond, levels = levels(Data_Train$ ExterCond))
#Data_Test$BsmtQual <- factor(Data_Test$BsmtQual, levels = levels(Data_Train$ BsmtQual))
#Data_Test$BsmtCond <- factor(Data_Test$BsmtCond, levels = levels(Data_Train$ BsmtCond))
#Data_Test$BsmtFinType1 <- factor(Data_Test$BsmtFinType1, levels = levels(Data_Train$ BsmtFinType1))
#Data_Test$BsmtFinType2 <- factor(Data_Test$BsmtFinType2, levels = levels(Data_Train$ BsmtFinType2))
#Data_Test$HeatingQC <- factor(Data_Test$HeatingQC, levels = levels(Data_Train$ HeatingQC))
#Data_Test$KitchenQual <- factor(Data_Test$KitchenQual, levels = levels(Data_Train$itchenQual))
#Data_Test$Functional<- factor(Data_Test$Functional, levels = levels(Data_Train$ Functional))
#Data_Test$GarageFinish<- factor(Data_Test$GarageFinish, levels = levels(Data_Train$ GarageFinish))
#Data_Test$GarageQual <- factor(Data_Test$GarageQual, levels = levels(Data_Train$ GarageQual))
#Data_Test$GarageCond<- factor(Data_Test$GarageCond, levels = levels(Data_Train$ GarageCond))
#Data_Test$BsmtExposure<- factor(Data_Test$BsmtExposure, levels = levels(Data_Train$BsmtExposure))


#Regresion Lineal
LinearRegresionModel=lm(SalePrice ~.,data=Data_Train)
summary(LinearRegresionModel)
LinearRegresionModel=lm(SalePrice ~ LotArea+Neighborhood+Condition1+BldgType+OverallQual+OverallCond+YearBuilt+RoofMatl+MasVnrType+MasVnrArea+BsmtExposure+TotalBsmtSF+X1stFlrSF+GrLivArea+BedroomAbvGr+TotRmsAbvGrd+Functional+Fireplaces+GarageYrBlt+OpenPorchSF,data=Data_Train)
summary(LinearRegresionModel)
LinearPredict = predict(LinearRegresionModel,newdata=Data_Test)


#Random Forest
Data_Train_RD=Data_Train
RFHousePrices = randomForest(SalePrice~.,data=Data_Train_RD,ntree=1000, mtry = sqrt(ncol(Data_Train) - 1))
RFPredTest = predict(RFHousePrices,newdata=Data_Test)


#Árbol de decisión 
tree_model <- 
  rpart(SalePrice~.,
        method = "anova", 
        data=Data_Train,
        control=rpart.control(cp = 0.01)
)
prp(tree_model)

tree_predictions <- predict(tree_model, newdata = Data_Test)



#GBM
gbm_model <- gbm(
  formula = SalePrice ~ .,
  distribution = "gaussian",
  data = Data_Train,
  n.trees = 1000,
  interaction.depth = 4,
  shrinkage = 0.1,
  cv.folds = 5,
  n.minobsinnode = 10, 
  verbose = TRUE
)

gbm_predictions <- predict(gbm_model, newdata = Data_Test)


#XG Boost
data = Data_Train
data_features = data[, which(names(data) != "SalePrice")]
data_matrix = as.matrix(data_features)
data_matrix = apply(data_matrix, 2, as.numeric)
Data_Train_M = xgb.DMatrix(data_matrix, label = data$SalePrice)

datat=Data_Test
Data_Test_Matrix = as.matrix(datat)
Data_Test_Matrix = apply(Data_Test_Matrix, 2, as.numeric)
Data_Test_M = xgb.DMatrix(data = Data_Test_Matrix)

params <- list(
  colsample_bytree = 0.4,
  gamma = 0,
  eta = 0.1,
  max_depth = 4,
  min_child_weight = 1.5,
  alpha = 0.75,
  lambda = 0.45,
  subsample = 0.6,
  objective = "reg:squarederror"
)

bst_model <- xgb.train(
  params = params,
  data = Data_Train_M,
  nrounds = 100,
  watchlist = list(train = Data_Train_M),
  nthread = 0,
  verbose = 0
)

XGBPredTest = predict(bst_model,newdata=Data_Test_M)



#Creando los archivos a subir
#Model = glm(y ~ x1+x2+x3, data=Train, family=binomial)
# And then make predictions on the test set:
#PredTest = predict(Model, newdata=Test, type="response")

MySubmissionLM = data.frame(Id = Data_Test_O$Id, SalePrice = LinearPredict)
write.csv(MySubmissionLM, "SubmissionLM.csv", row.names=FALSE)
#0.30666

MySubmissionRF = data.frame(Id = Data_Test_O$Id, SalePrice = RFPredTest)
write.csv(MySubmissionRF, "SubmissionRF.csv", row.names=FALSE)
#0.15089

MySubmissionTM = data.frame(Id = Data_Test_O$Id, SalePrice = tree_predictions)
write.csv(MySubmissionTM, "SubmissionTM.csv", row.names=FALSE)
#0.23917

MySubmissionGBM = data.frame(Id = Data_Test_O$Id, SalePrice = gbm_predictions)
write.csv(MySubmissionGBM, "SubmissionGBM.csv", row.names=FALSE)
#0.13411

MySubmissionXGB = data.frame(Id = Data_Test_O$Id, SalePrice = XGBPredTest)
write.csv(MySubmissionXGB, "SubmissionXGB.csv", row.names=FALSE)
#0.13465

# You should upload the submission "Submission.csv" on the Kaggle website 
# to use this as a submission to the competition
