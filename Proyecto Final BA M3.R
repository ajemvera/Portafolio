#Carga de data

Data_Test=read.csv("test.csv")
Data_Train=read.csv("train.csv")
Data_Train_I=read.csv("train.csv")

summary(Data_Train)
str(Data_Train)


#Analizando variables

boxplot(Data_Train$X1stFlrSF)
table(Data_Train$X1stFlrSF)


# Iniciaremos con la etapa de limpieza de data, eliminaremos la columna ID y analizaremos datos faltantes

Data_Train <- subset(Data_Train,select = -Id)

missing_values <- colSums(is.na(Data_Train))
missing_values <- sort(missing_values, decreasing = TRUE)
missing_values[missing_values > 0]
missing_values_perct = round(missing_values/nrow(Data_Train),2)
missing_values_perct[missing_values > 0]


## Eliminar columnas con más del 40% de valores faltantes

threshold <- 0.4 * nrow(Data_Train)
cols_to_drop <- names(missing_values[missing_values > threshold])
Data_Train <- subset(Data_Train,select = -c(PoolQC,MiscFeature,Alley,Fence,FireplaceQu))
str(Data_Train)


# Función para tratar outliers

tratar_outliers <- function(x) {
  cuartiles <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- cuartiles[2] - cuartiles[1]
  limite_inferior <- cuartiles[1] - 1.5 * iqr
  limite_superior <- cuartiles[2] + 1.5 * iqr
  x <- ifelse(x < limite_inferior, limite_inferior, x)
  x <- ifelse(x > limite_superior, limite_superior, x)
  return(x)
}


#Tratamiento de datos faltantes

for (col in colnames(Data_Train)) {
  if (is.numeric(Data_Train[[col]])) {
    mediana <- median(Data_Train[[col]], na.rm = TRUE)
    Data_Train[[col]][is.na(Data_Train[[col]])] <- mediana
  }
}

for (col in colnames(Data_Test)) {
  if (is.numeric(Data_Test[[col]])) {
    mediana <- median(Data_Test[[col]], na.rm = TRUE)
    Data_Test[[col]][is.na(Data_Test[[col]])] <- mediana
  }
}


#Tratamiento de outliers

#install.packages("magrittr")
#library(magrittr)
#install.packages("dplyr")
Data_Train = Data_Train%>%mutate(across(where(is.numeric), tratar_outliers))

Data_Test = Data_Test%>%mutate(across(where(is.numeric), tratar_outliers))

#Regresion Lineal

lmHousePrices=lm(SalePrice~YearBuilt+YearRemodAdd+YrSold+MSSubClass+LotFrontage+LotArea+OverallQual+OverallCond+MasVnrArea+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+Fireplaces+GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold ,data=Data_Train)
summary(lrHousePrices)
lmHousePrices=lm(SalePrice~YearBuilt+MSSubClass+LotArea+OverallQual+OverallCond+MasVnrArea+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+GarageCars,data=Data_Train)
summary(lmHousePrices)

subset_lmHousePrices = subset(Data_Test,select=c(MSSubClass,LotArea,OverallQual,OverallCond,YearBuilt,MasVnrArea,BsmtUnfSF,TotalBsmtSF,X1stFlrSF,X2ndFlrSF,BsmtFullBath,BedroomAbvGr,KitchenAbvGr,TotRmsAbvGrd,GarageCars,WoodDeckSF,ScreenPorch))
cor(subset_lmHousePrices, use = "complete.obs")
#Se quitan del modelo las variables TotRmsAbvGrd y TotalBsmtSF al tener una alta correlacion con BedroomAbvGr y X1stFlrSF respectivamente


#Realizando el Test
lmPredTest = predict(lmHousePrices,newdata=Data_Test)
summary(lmPredTest)

#Creando el archivo a subir
#Model = glm(y ~ x1+x2+x3, data=Train, family=binomial)
# And then make predictions on the test set:
#PredTest = predict(Model, newdata=Test, type="response")
MySubmission = data.frame(Id = Data_Test$Id, SalePrice = lmPredTest)
write.csv(MySubmission, "Submission.csv", row.names=FALSE)
# You should upload the submission "Submission.csv" on the Kaggle website 
# to use this as a submission to the competition

#Score: 0.18175