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
PARAM$experimento  <- "ZZ_RF_TRAINING_201901-202105"
PARAM$exp_input  <- "RF_HT9420_TRAINING_201901-202105"

PARAM$modelos  <- 1
PARAM$semilla_primos = 539573
PARAM$semillerio  <- 50

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

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

# Semillerio
#genero un vector de una cantidad de ksemillerio  de semillas,  buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
set.seed( PARAM$semilla_primos ) #seteo la semilla que controla al sample de los primos
sample_semillas  <- sample(primos)[ 1:PARAM$semillerio ]   #me quedo con  PARAM$semillerio primos al azar
sample_semillas

for( i in  1:PARAM$modelos )
{
  parametros  <- as.list( copy( tb_log[ i ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana
  
  dataset_prediccion <- data.table()
  lista_pred <- c()
  
  # Recupero las predicciones generadas con las semillas del semillerio
  for (ksemilla in sample_semillas) {
    nom_pred  <- paste0( base_dir, 
                         "exp/", PARAM$experimento,
                         "/pred_",
                         sprintf( "%02d", i ),
                         "_",
                         sprintf( "%03d", iteracion_bayesiana),
                         "_",
                         sprintf( "%07d", ksemilla ),
                         ".csv"  )
    
    tb_prediccion <- fread( file=nom_pred, sep= "\t" )
    
    if (nrow(dataset_prediccion) == 0) {
      dataset_prediccion <- data.table(numero_de_cliente=tb_prediccion$numero_de_cliente, 
                                       foto_mes=tb_prediccion$foto_mes)
    }
    
    lista_pred = c(lista_pred, paste0( "pred_", sprintf( "%07d", ksemilla ) ))
    
    dataset_prediccion[ , paste0( "pred_", sprintf( "%07d", ksemilla ) ) := tb_prediccion$prob]
  }
  
  dataset_prediccion[ , prob_semillerio := apply(dataset_prediccion[ , lista_pred, with = FALSE ], 1, mean) ]
  
  #genero los archivos para Kaggle
  cortes  <- seq( from=  7000,
                  to=   18000,
                  by=     500 )
  
  setorder( dataset_prediccion, -prob_semillerio )
  
  for( corte in cortes )
  {
    dataset_prediccion[  , Predicted := 0L ]
    dataset_prediccion[ 1:corte, Predicted := 1L ]
    
    nom_submit  <- paste0( base_dir, "exp/", PARAM$experimento, "/",
                           PARAM$experimento, 
                           "_",
                           sprintf( "%02d", i ),
                           "_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           "_",
                           sprintf( "%05d", corte ),
                           "_semillerio",
                           ".csv" )
    
    fwrite(  dataset_prediccion[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )
  }
  
  nom_prob    <- paste0( base_dir, "exp/", PARAM$experimento, "/",
                         PARAM$experimento, 
                         "_",
                         sprintf( "%02d", i ),
                         "_",
                         sprintf( "%03d", iteracion_bayesiana ),
                         "_",
                         "prob",
                         "_semillerio",
                         ".csv" )
  
  fwrite(  dataset_prediccion,
           file= nom_prob,
           sep= "," )
}

