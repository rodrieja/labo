#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("primes")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "HM_semillerio"

PARAM$modelos  <- c("ZZ_HT_BO_02", "ZZ_RF_TRAINING_201901-202105", "ZZ_SS_TRAINING_201901-202105", "AD_ZZ9430_TRAINING_201901-202105", "AD_SS9430_TRAINING_201901-202105")

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

dataset_prediccion <- data.table()
lista_pred <- c()

for( modelo in  PARAM$modelos )
{
  nom_prob  <- paste0( base_dir, 
                        "exp/", 
                        modelo,
                        "/prob_semillerio.csv"
                        )
  
  tb_prediccion <- fread( file=nom_prob, sep="," )
  setorder( tb_prediccion, numero_de_cliente )

  if (nrow(dataset_prediccion) == 0) {
    dataset_prediccion <- data.table(numero_de_cliente=tb_prediccion$numero_de_cliente, 
                                      foto_mes=tb_prediccion$foto_mes)
  }
  
  lista_pred = c(lista_pred, paste0( "modelo_", modelo ))
  
  dataset_prediccion[ , paste0( "modelo_", modelo ) := tb_prediccion$prob_semillerio]
}
  
dataset_prediccion[ , prob_semillerio := apply(dataset_prediccion[ , lista_pred, with = FALSE ], 1, mean) ]

#genero los archivos para Kaggle
cortes  <- seq( from=  8500,
                to=   18000,
                by=     500 )

setorder( dataset_prediccion, -prob_semillerio )

for( corte in cortes )
{
  dataset_prediccion[  , Predicted := 0L ]
  dataset_prediccion[ 1:corte, Predicted := 1L ]
  
  nom_submit  <- paste0( base_dir, 
                         "exp/", 
                         PARAM$experimento, 
                         "/prob_",
                         sprintf( "%05d", corte ),
                         "_hibridacion",
                         ".csv" )
  
  fwrite(  dataset_prediccion[ , list( numero_de_cliente, Predicted ) ],
            file= nom_submit,
            sep= "," )
}
  
nom_hibridacion <- paste0( base_dir,
                           "exp/",
                           PARAM$experimento, "/",
                           PARAM$experimento,
                           "_",
                           "prob_modelos.csv" )

fwrite(  dataset_prediccion,
          file= nom_hibridacion,
          sep= "," )

