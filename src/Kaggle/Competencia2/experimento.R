# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("/Users/alejandrrodr/Documents/DMUBA/DM-EyF") # establezco la carpeta donde voy a trabajar
# cargo el dataset
dataset <- fread("./datasets/competencia2_2022.csv.gz")

marzoMayo = dataset[foto_mes%in%c(202103, 202105),]
marzoMayo[, clase_mes := ifelse(foto_mes == 202103, "MARZO", "MAYO")]
dtrain <- marzoMayo # defino donde voy a entrenar

# genero el modelo,  aqui se construye el arbol
modelo <- rpart(
        formula = "clase_mes ~ . -clase_ternaria -numero_de_cliente -foto_mes", # quiero predecir clase_ternaria a partir de el resto de las variables
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 800, # minima cantidad de registros para que se haga el split
        minbucket = 1, # tamaño minimo de una hoja
        maxdepth = 10
) # profundidad maxima del arbol

modelo$variable.importance

marzoMayo[  , .N,  list( foto_mes,  Visa_fultimo_cierre ) ]
marzoMayo[  , .N,  list( foto_mes,  Master_fultimo_cierre ) ]
marzoMayo[  , .N,  list( foto_mes,  ctarjeta_visa ) ]

dataset[foto_mes == 202103 & Visa_fultimo_cierre == 1, Visa_fultimo_cierre := 5]
dataset[foto_mes == 202103 & Visa_fultimo_cierre == 7, Visa_fultimo_cierre := 12]
dataset[foto_mes == 202103 & Visa_fultimo_cierre == 21, Visa_fultimo_cierre := 26]

dataset[foto_mes == 202103 & Master_fultimo_cierre == 1, Master_fultimo_cierre := 5]
dataset[foto_mes == 202103 & Master_fultimo_cierre == 14, Master_fultimo_cierre := 19]
dataset[foto_mes == 202103 & Master_fultimo_cierre == 7, Master_fultimo_cierre := 12]
dataset[foto_mes == 202103 & Master_fultimo_cierre == 21, Master_fultimo_cierre := 26]




# uso esta semilla para los canaritos
set.seed(539573)


# agrego 30 canaritos
for (i in 1:30) dataset[, paste0("canarito", i) := runif(nrow(dataset))]

# Feature engeneering section
dataset[, campo1 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales < 2)]
dataset[, campo2 := as.integer(ctrx_quarter < 14 & mcuentas_saldo < -1256.1 & cprestamos_personales >= 2)]

dataset[, campo3 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro < 2601.1)]
dataset[, campo4 := as.integer(ctrx_quarter < 14 & mcuentas_saldo >= -1256.1 & mcaja_ahorro >= 2601.1)]

dataset[, campo5 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status >= 8 | is.na(Master_status)))]
dataset[, campo6 := as.integer(ctrx_quarter >= 14 & (Visa_status >= 8 | is.na(Visa_status)) & (Master_status < 8 & !is.na(Master_status)))]

dataset[, campo7 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter < 38)]
dataset[, campo8 := as.integer(ctrx_quarter >= 14 & Visa_status < 8 & !is.na(Visa_status) & ctrx_quarter >= 38)]

# creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[foto_mes == 202101, clase_binaria := ifelse(clase_ternaria == "CONTINUA", "NO", "SI")]

# Armo dataset de train y test
dtrain <- dataset[foto_mes == 202101] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202103] # defino donde voy a aplicar el modelo

# Modelo con canaritos
modelo <- rpart(
    formula = "clase_binaria ~ . -clase_ternaria -mcomisiones_mantenimiento -Visa_mpagado",
    data = dtrain, # los datos donde voy a entrenar
    xval = 0,
    cp = -0.7657821164, #  -0.89
    minsplit = 895, # 621
    minbucket = 446, # 309
    maxdepth = 11
) #  12

# modelo_original <- rpart(
#     formula = "clase_binaria ~ . -clase_ternaria -mcomisiones_mantenimiento -Visa_mpagado",
#     data = dtrain,
#     model = TRUE,
#     xval = 0,
#     cp = -1,
#     minsplit = 2, # dejo que crezca y corte todo lo que quiera
#     minbucket = 1,
#     maxdepth = 30
# )

# modelo_original$frame[modelo_original$frame$var %like% "canarito", "complexity"] <- -666
# modelo_pruned <- prune(modelo_original, -666)

# aplico el modelo a los datos de testing
prediccion <- predict(
    object = modelo,
    newdata = dapply,
    type = "prob"
)

# prediccion <- predict(modelo_pruned, dapply, type = "prob")


# Valido
# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal <- copy(dapply[, list(numero_de_cliente)])
dfinal[, prob_SI := prediccion[, "SI"]]

set.seed(481721)
dfinal[, azar := runif(nrow(dapply))]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder(dfinal, -prob_SI, azar)

# Armo el dataset para Kaggle
dir.create("./exp/")
dir.create("./exp/KAGGLE1")

for (corte in c(7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000))
{
    # le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
    dfinal[, Predicted := 0L]
    dfinal[1:corte, Predicted := 1L]

    fwrite(dfinal[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
        file = paste0("./exp/KAGGLE1/PRUEBA_", corte, ".csv"),
        sep = ","
    )
}
