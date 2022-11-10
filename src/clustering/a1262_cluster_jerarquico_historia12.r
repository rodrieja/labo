#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")

#Parametros del script
PARAM <- list()
PARAM$experimento  <- "CLU1262"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset original
# pero podria leer cualquiera que tenga Feature Engineering
dataset_original  <- fread( "./datasets/competencia3_2022.csv.gz", stringsAsFactors= TRUE)
dataset <- copy(dataset_original)

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#me quedo SOLO con los BAJA+2
dataset  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202006  & foto_mes<=202105, ] 

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
dataset12  <- copy( dataset_original[  numero_de_cliente %in%  dataset[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
setorderv( dataset12, c("numero_de_cliente", "foto_mes"), c(1,-1) )
dataset12[  , pos := seq(.N) , numero_de_cliente ]

dataset12[ , c("numero_de_cliente", "foto_mes", "pos") ]

#me quedo solo con los 12 meses antes de morir
dataset12  <- dataset12[  pos <= 12 , ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )


#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar

campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[  , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )


#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


#genero 7 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)
  
  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]
  
  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset,
        file= "cluster_de_bajas.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset[  , mean(ctrx_quarter),  c("foto_mes,cluster2") ]  #media de la variable  ctrx_quarter


barplot(table(dataset$foto_mes))

dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(chomebanking_transacciones),  cluster2 ]

dataset[  , mean(cliente_edad),  cluster2 ]
dataset[  , min(cliente_edad),  cluster2 ]
dataset[  , max(cliente_edad),  cluster2 ]
boxplot(dataset[  , cliente_edad~cluster2])

dataset[  , mean(cliente_antiguedad),  cluster2 ]
boxplot(dataset[  , cliente_antiguedad~cluster2])


barplot(table(dataset$foto_mes))



--------------------------------------------------------------------

# valores medios por mes para las bajas
# transacciones
dataset[ , mean(ctrx_quarter) , cluster2 ]
# sueldo
dataset[ , mean(mpayroll) , cluster2 ]
# ganancias del banco en ese mes
dataset[ , mean(mrentabilidad) , cluster2 ]
# comisiones que abona el cliente
dataset[ , mean(mcomisiones_mantenimiento) , cluster2 ]
dataset[ , mean(mcomisiones_otras) , cluster2 ]
# ganancias del banco
dataset[ , mean(mcomisiones) , cluster2 ]
# transferencias del cliente
dataset[ , mean(mtransferencias_recibidas) , cluster2 ]
dataset[ , mean(mtransferencias_emitidas) , cluster2 ]
# pago de servicios
dataset[ , mean(mpagomiscuentas) , cluster2 ]
dataset[ , mean(mcuenta_debitos_automaticos) , cluster2 ]
# pasivos
# todas las cuentas
dataset[ , mean(mcuentas_saldo) , cluster2 ]
dataset[ , mean(mcaja_ahorro) , cluster2 ]
dataset[ , mean(mcuenta_corriente) , cluster2 ]
dataset[ , mean(mcaja_ahorro_dolares) , cluster2 ]
# activos
dataset[ , mean(mprestamos_personales) , cluster2 ]
dataset[ , mean(mtarjeta_visa_consumo) , cluster2 ]
dataset[ , mean(mtarjeta_master_consumo) , cluster2 ]


--------------------------------------------------------------------

#Finalmente grabo el archivo para  Juan Pablo Cadaveira
#agrego a dataset12 el cluster2  y lo grabo
dataset12[ dataset,
           on= "numero_de_cliente",
           cluster2 := i.cluster2 ]

dataset12[ , c("numero_de_cliente", "foto_mes", "cluster2")]


dataset_original[ clase_ternaria == "BAJA+2" , mean(ctrx_quarter) , foto_mes ]
# sueldo
dataset_original[ clase_ternaria == "BAJA+2" , mean(mpayroll) , foto_mes ]
# ganancias del banco en ese mes
dataset_original[ clase_ternaria == "BAJA+2" , mean(mrentabilidad) , foto_mes ]
# comisiones que abona el cliente
dataset_original[ clase_ternaria == "BAJA+2" , mean(mcomisiones_mantenimiento) , foto_mes ]
dataset_original[ clase_ternaria == "BAJA+2" , mean(mcomisiones_otras) , foto_mes ]
# ganancias del banco
dataset_original[ clase_ternaria == "BAJA+2" , mean(mcomisiones) , foto_mes ]
# transferencias del cliente
dataset_original[ clase_ternaria == "BAJA+2" , mean(mtransferencias_recibidas) , foto_mes ]
dataset_original[ clase_ternaria == "BAJA+2" , mean(mtransferencias_emitidas) , foto_mes ]
# pago de servicios
dataset_original[ clase_ternaria == "BAJA+2" , mean(mpagomiscuentas) , foto_mes ]
dataset_original[ clase_ternaria == "BAJA+2" , mean(mcuenta_debitos_automaticos) , foto_mes ]
# pasivos
# todas las cuentas
dataset_original[ clase_ternaria == "BAJA+2" , mean(mcuentas_saldo) , foto_mes ]
dataset_original[ clase_ternaria == "BAJA+2" , mean(mcaja_ahorro) , foto_mes ]
dataset_original[ clase_ternaria == "BAJA+2" , mean(mcuenta_corriente) , foto_mes ]
dataset_original[ clase_ternaria == "BAJA+2" , mean(mcaja_ahorro_dolares) , foto_mes ]
# activos
dataset_original[ clase_ternaria == "BAJA+2" , mean(mprestamos_personales) , foto_mes ]
dataset_original[ clase_ternaria == "BAJA+2" , mean(mtarjeta_visa_consumo) , foto_mes ]
dataset_original[ clase_ternaria == "BAJA+2" , mean(mtarjeta_master_consumo) , foto_mes ]


--------------------------------------------------------------------

plot_12_meses <- function(data, title, y_name) {
  p<-ggplot(data=data, aes(x=foto_mes, y=V1, group=cluster)) +
    geom_line(aes(color=cluster))+
    geom_point(aes(color=cluster))+
    labs(title=title, y = y_name)
  p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

dataset12$foto_mes = as.factor(dataset12$foto_mes)
dataset12$cluster = as.character(dataset12$cluster2)

# transacciones
mean_ctrx_quarter = dataset12[ , mean(ctrx_quarter) , by = .(foto_mes, cluster) ]
# sueldo
mean_mpayroll = dataset12[ , mean(mpayroll) , by = .(foto_mes, cluster) ]
# ganancias del banco en ese mes
mean_mrentabilidad = dataset12[ , mean(mrentabilidad) , by = .(foto_mes, cluster) ]
# comisiones que abona el cliente
mean_mcomisiones_mantenimiento = dataset12[ , mean(mcomisiones_mantenimiento) , by = .(foto_mes, cluster) ]
mean_mcomisiones_otras = dataset12[ , mean(mcomisiones_otras) , by = .(foto_mes, cluster) ]
# ganancias del banco
mean_mcomisiones = dataset12[ , mean(mcomisiones) , by = .(foto_mes, cluster) ]
# transferencias del cliente
mean_mtransferencias_recibidas = dataset12[ , mean(mtransferencias_recibidas) , by = .(foto_mes, cluster) ]
mean_mtransferencias_emitidas = dataset12[ , mean(mtransferencias_emitidas) , by = .(foto_mes, cluster) ]
# pago de servicios
mean_mpagomiscuentas = dataset12[ , mean(mpagomiscuentas) , by = .(foto_mes, cluster) ]
mean_mcuenta_debitos_automaticos = dataset12[ , mean(mcuenta_debitos_automaticos) , by = .(foto_mes, cluster) ]
# pasivos
# todas las cuentas
mean_mcuentas_saldo = dataset12[ , mean(mcuentas_saldo) , by = .(foto_mes, cluster) ]
mean_mcaja_ahorro = dataset12[ , mean(mcaja_ahorro) , by = .(foto_mes, cluster) ]
mean_mcuenta_corriente = dataset12[ , mean(mcuenta_corriente) , by = .(foto_mes, cluster) ]
mean_mcaja_ahorro_dolares = dataset12[ , mean(mcaja_ahorro_dolares) , by = .(foto_mes, cluster) ]
# activos
mean_mprestamos_personales = dataset12[ , mean(mprestamos_personales) , by = .(foto_mes, cluster) ]
mean_mtarjeta_visa_consumo = dataset12[ , mean(mtarjeta_visa_consumo) , by = .(foto_mes, cluster) ]
mean_mtarjeta_master_consumo = dataset12[ , mean(mtarjeta_master_consumo) , by = .(foto_mes, cluster) ]

users_by_cluster = dataset12[ , .N , by = .(foto_mes, cluster) ]

pdf( "graficos_12_meses_cluster.pdf" )
bajas_12_meses = dataset_original[ clase_ternaria == "BAJA+2", .N , by=.(foto_mes) ]
bajas_12_meses$foto_mes = as.factor(bajas_12_meses$foto_mes)
p<-ggplot(data=bajas_12_meses, aes(x=foto_mes, y=N)) +
  geom_line(aes(group = 1))+
  geom_point(aes(group = 1))+
  labs(title="Baja de usuarios", y = "Usuarios")
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Usuarios
p<-ggplot(data=users_by_cluster, aes(x=foto_mes, y=N, group=cluster)) +
  geom_line(aes(color=cluster))+
  geom_point(aes(color=cluster))+
  labs(title="Baja de usuarios", y = "Usuarios")
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_12_meses(mean_ctrx_quarter, "Promedio de transacciones", "Transacciones")
plot_12_meses(mean_mpayroll, "Promedio de sueldos", "Monto")
plot_12_meses(mean_mrentabilidad, "Rentabilidad promedio", "Monto")
plot_12_meses(mean_mcomisiones_mantenimiento, "Promedio Comisiones mantenimiento", "Monto")
plot_12_meses(mean_mcomisiones_otras, "Promedio Otras Comisiones", "Monto")
plot_12_meses(mean_mcomisiones, "Promedio Comisiones cobradas", "Monto")
plot_12_meses(mean_mtransferencias_recibidas, "Promedio tranferencias recibidas", "Monto")
plot_12_meses(mean_mtransferencias_emitidas, "Promedio transferencias emitidas", "Monto")
plot_12_meses(mean_mpagomiscuentas, "Promedio pago servicios", "Monto")
plot_12_meses(mean_mcuenta_debitos_automaticos, "Promedio débitos automáticos", "Monto")
plot_12_meses(mean_mcuentas_saldo, "Promedio saldo en cuenta", "Monto")
plot_12_meses(mean_mcaja_ahorro, "Promedio caja de ahorros", "Monto")
plot_12_meses(mean_mcuenta_corriente, "Promedio cuenta corriente", "Monto")
plot_12_meses(mean_mcaja_ahorro_dolares, "Promedio caja ahorro dólares", "Monto")
plot_12_meses(mean_mprestamos_personales, "Promedio prestamos personales", "Monto")
plot_12_meses(mean_mtarjeta_visa_consumo, "Promedio Visa", "Monto")
plot_12_meses(mean_mtarjeta_master_consumo, "Promedio Master", "Monto")
dev.off()





fwrite( dataset12, 
        file= "cluster_de_bajas_12meses.txt",
        sep= "\t" )
