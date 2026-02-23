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
library(SHAPforxgboost)
library(data.table)


## PREDICCION DE VENTAS

#Carga del dataset

Data_Test=read.csv("test.csv",sep = ";",fileEncoding = "latin1",stringsAsFactors = FALSE)
Data_Train=read.csv("train.csv",sep = ";",fileEncoding = "latin1",stringsAsFactors = FALSE)


#Preparacion de la data

Data_Train$Año <- as.numeric(Data_Train$Año)
Data_Train$Mes <- as.factor(Data_Train$Mes)
Data_Train$Cant.Bol <- as.numeric(Data_Train$Cant.Bol)
Data_Train$Cant.Fact <- as.numeric(Data_Train$Cant.Fact)
Data_Train$Cant.Otros <- as.numeric(Data_Train$Cant.Otros)
Data_Train$Vendedores.Call <- as.numeric(Data_Train$Vendedores.Call)
Data_Train$Vendores.General <- as.numeric(Data_Train$Vendores.General)
Data_Train$Vendedores.ST <- as.numeric(Data_Train$Vendedores.ST)
Data_Train$Clientes.B2B <- as.numeric(Data_Train$Clientes.B2B)
Data_Train$CLientes.B2C <- as.numeric(Data_Train$CLientes.B2C)

Data_Test$Año  <- as.numeric(Data_Test$Año)
Data_Test$Mes <- as.factor(Data_Test$Mes)
Data_Test$Cant.Bol  <- as.numeric(Data_Test$Cant.Bol)
Data_Test$Cant.Fact  <- as.numeric(Data_Test$Cant.Fact)
Data_Test$Cant.Otros  <- as.numeric(Data_Test$Cant.Otros)
Data_Test$Vendedores.Call <- as.numeric(Data_Test$Vendedores.Call)
Data_Test$Vendedores.ST <- as.numeric(Data_Test$Vendedores.ST)
Data_Test$Vendores.General <- as.numeric(Data_Test$Vendores.General)
Data_Test$Clientes.B2B <- as.numeric(Data_Test$Clientes.B2B)
Data_Test$CLientes.B2C <- as.numeric(Data_Test$CLientes.B2C)

y_test <- Data_Test$Monto.Vendido
Data_Test <- subset(Data_Test,select = -Monto.Vendido)
y_train <- Data_Train$Monto.Vendido


#XG Boost

data = Data_Train
data_matrix <- model.matrix(Monto.Vendido ~ . -1, data = Data_Train)
Data_Train_M = xgb.DMatrix(data_matrix, label = data$Monto.Vendido)

datat=Data_Test
Data_Test_Matrix <- model.matrix(~ . -1, data = Data_Test)
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
  evals = list(train = Data_Train_M),
  nthread = 0,
  verbose = 0
)

XGBPredTest = predict(bst_model,newdata=Data_Test_M)


#Probando eficiencia

pred <- predict(bst_model, Data_Test_M)
rmse <- sqrt(mean((pred - y_test)^2))
rmse / mean(y_test)
mean(abs((pred - y_test) / y_test)) * 100


#variables relevantes

importance_matrix <- xgb.importance(
  feature_names = colnames(data_matrix),
  model = bst_model)

print(importance_matrix)



#comparacion de real y proyectado

comparacion <- data.frame(Real = y_test,Prediccion = pred)
head(comparacion, 20)
1 - sum((y_test - pred)^2) / sum((y_test - mean(y_test))^2)



## RIESGO DE ABANDONO DE CLIENTE

df <- read.csv("C:/Users/User/OneDrive/Soldamundo Peru/R Studio/Compras a 6 M.csv", fileEncoding = "Latin1", sep = ";")
setDT(df)

features <- c(
  "Monto.Total.6M",
  "Unidades.de.Negocio.6M",
  "Marcas.6M",
  "Productos.6M",
  "Compras.6M",
  "Meses.desde.la.ultima.Compra",
  "Monto.Total.3M",
  "Unidades.de.Negocio.3M",
  "Marcas.3M",
  "Productos.3M",
  "Compras.3M"
)

# Verificar que churn sea numérico 0/1
df[, Churn := as.numeric(Churn)]

#Separar train y test
set.seed(123)
train_index <- createDataPartition(df$Churn, p = 0.8, list = FALSE)

train <- df[train_index]
test  <- df[-train_index]


#convertir a matriz
dtrain <- xgb.DMatrix(
  data = as.matrix(train[, ..features]),
  label = train$Churn
)

dtest <- xgb.DMatrix(
  data = as.matrix(test[, ..features]),
  label = test$Churn
)

#Modelo
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

modelo <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 20,
  print_every_n = 10
)

pred_prob <- predict(modelo, dtest)


# Convertir a clase (threshold 0.5)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

confusionMatrix(
  as.factor(pred_class),
  as.factor(test$Churn)
)

df[, Prob_Churn := predict(
  modelo,
  xgb.DMatrix(as.matrix(df[, ..features]))
)]

df[, Segmento_Riesgo := fifelse(
  Prob_Churn >= 0.7, "Alto Riesgo",
  fifelse(Prob_Churn >= 0.4, "Riesgo Medio", "Bajo Riesgo")
)]

importance <- xgb.importance(model = modelo)
xgb.plot.importance(importance)


#Guardar y cargar modelo
xgb.save(modelo, "modelo_churn.model")
modelo <- xgb.load("modelo_churn.model")


#Evaluar clientes recientes y crear un csv
nuevo <- read.csv("C:/Users/User/OneDrive/Soldamundo Peru/R Studio/Ultimos Clientes.csv", fileEncoding = "Latin1", sep = ";")
setnames(nuevo, make.names(names(nuevo)))
setDT(nuevo)

X_nuevo <- as.matrix(nuevo[, features, with = FALSE])
nuevo[, Prob_Churn := predict(modelo, xgb.DMatrix(X_nuevo))]
fwrite(nuevo, "clientes_con_score.csv")
