install.packages("arules")
install.packages("openxlsx")
library(arules)
library(openxlsx)

ventas <- read.csv(
  "C:/Users/User/OneDrive/Soldamundo Peru/R Studio/ProductosxFactura.csv",
  fileEncoding = "Latin1",
  sep = ";",
  stringsAsFactors = FALSE
)

ventas$Producto <- as.character(ventas$Producto)
ventas$Factura  <- as.character(ventas$Factura)

transacciones <- as(
  split(ventas$Producto, ventas$Factura),
  "transactions"
)

summary(transacciones)
inspect(transacciones[1:5])

reglas <- apriori(
  transacciones,
  parameter = list(
    supp = 0.01,    # aparece en al menos 1% de facturas
    conf = 0.3,     # 30% de confianza mínima
    minlen = 2
  )
)

reglas_ordenadas <- sort(reglas, by = "lift", decreasing = TRUE)
inspect(head(reglas_ordenadas, 10))

reglas_fuertes <- subset(reglas, lift > 1.5 & confidence > 0.4)
inspect(reglas_fuertes)


#Buscar Reglas en especifico
#reglas_producto <- subset(reglas, lhs %in% "Electrodo 6011")
#inspect(sort(reglas_producto, by = "lift"))

#Imprimir reglas
reglas_df <- as(reglas, "data.frame")






# Obtener LHS y RHS directamente desde las reglas
lhs_list <- as(lhs(reglas), "list")
rhs_list <- as(rhs(reglas), "list")

# Obtener métricas
metricas <- quality(reglas)

# Lista para guardar las combinaciones
pares <- list()

for (i in seq_along(reglas)) {
  
  # Productos del antecedente
  lhs_productos <- lhs_list[[i]]
  
  # Productos del consecuente
  rhs_productos <- rhs_list[[i]]
  
  # Crear todas las combinaciones LHS x RHS
  combinaciones <- expand.grid(
    SKU_Origen = lhs_productos,
    SKU_CrossSelling = rhs_productos,
    stringsAsFactors = FALSE
  )
  
  # Agregar métricas
  combinaciones$support <- metricas$support[i]
  combinaciones$confidence <- metricas$confidence[i]
  combinaciones$lift <- metricas$lift[i]
  
  pares[[i]] <- combinaciones
}

# Unir todas las combinaciones
cross_selling_df <- do.call(rbind, pares)

# Reiniciar índices
rownames(cross_selling_df) <- NULL

# Ordenar por Lift
cross_selling_df <- cross_selling_df[
  order(-cross_selling_df$lift),
]

# Exportar
write.xlsx(
  cross_selling_df,
  file = "C:/Users/User/OneDrive/Soldamundo Peru/R Studio/reglas_cross_selling.xlsx",
  overwrite = TRUE
)