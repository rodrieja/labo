# Este script esta pensado para corren en Google Cloud
# si se lo desea correr en Windows debera
#  * cambiar el setwd()  y las rutas
#  * cuando llame a la funcion mcmapply  poner  mc.cores=1
#  * armarse de mucha paciencia porque va a demorar muchas horas en Windows

#Optimizacion Bayesiana de hiperparametros de  rpart
# Hace  1-Repeated  5-Fold Cross Validation


# NO utiliza Feature Engineering  ( el Fiscal General se enoja ... )


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

require("smoof")


#aqui deben ir SUS semillas, se usan para  1-Repeated  (5-Fold Cross Validation)
ksemilla_azar  <- c(481721, 483757, 503609, 519427, 539573)


#Defino la  Optimizacion Bayesiana

kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
          makeNumericParam("cp"       , lower=  -1.0, upper=    0.1),
          makeNumericParam("minsplit" , lower=   1,   upper= 5000 ),
          makeNumericParam("minbucket", lower=   1,   upper= 1000 ),
          makeIntegerParam("maxdepth" , lower=   3L,  upper=   20L),  #la letra L al final significa ENTERO
          forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la mitad de minsplit


#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol

ArbolSimple  <- function( fold_test, data, param )
{
  param2 <- param
  #param2$minsplit   <- as.integer( round( 2^param$minsplit ) )
  #param2$minbucket  <- as.integer( round( 2^param$minbucket ) )
  
  #genero el modelo
  modelo  <- rpart("clase_binaria ~ .  -Visa_mpagado -mcomisiones_mantenimiento -clase_ternaria",
                   data= data[ fold != fold_test, ],  #entreno en todo MENOS el fold_test que uso para testing
                   xval= 0,
                   control= param2 )

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ],  #aplico el modelo sobre los datos de testing
                          type= "prob")   #quiero que me devuelva probabilidades

  #En el 1er cuatrimestre del Tercer Año de la Maestria se explicaran las siguientes 12 lineas
  dtest <- copy( data[ fold==fold_test , list( clase_ternaria )] )
  dtest[ , pred := prediccion[ ,"SI"] ]
  dtest[ , azar := runif( nrow( dtest ) ) ]
  setorder(  dtest, -pred, azar )

  dtest[ , gan :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
  dtest[ , gan_acum := cumsum( gan ) ]

  #calculo la ganancia
  dtest2   <- dtest[ (1:100)*100,  ]
  idx_max  <- which.max( dtest2$gan_acum ) 
  ganancia_testing  <- dtest2[ (idx_max-1):(idx_max+1),  mean(gan_acum) ]


  rm( dtest )
  rm( dtest2 )

  return( ganancia_testing )  #esta es la ganancia sobre el fold de testing, NO esta normalizada
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( semilla, data, param, qfolds, pagrupa )
{
  divi  <- rep( 1, qfolds )  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos

  particionar( data, divi, seed=semilla, agrupa=pagrupa )  #particiono en dataset en folds

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 2 )   #debe ir 1 si es Windows

  data[ , fold := NULL ]

  #devuelvo la primer ganancia y el promedio
  ganancia_promedio  <- mean( unlist( ganancias ) )   #promedio las ganancias
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds  #aqui normalizo la ganancia

  gc()

  return( ganancia_promedio_normalizada )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x )
{
   GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1

   xval_folds  <- 5
   vganancias <- mcmapply( ArbolesCrossValidation,
                           ksemilla_azar,
                           MoreArgs= list ( dtrain, param=x, qfolds= xval_folds, pagrupa= "clase_ternaria" ),
                           SIMPLIFY= FALSE,
                           mc.cores = 2 )  #debe ir 1 si es Windows


   ganancia_promedio  <- mean( unlist( vganancias ) )
   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- ganancia_promedio
   xx$iteracion <- GLOBAL_iteracion
   loguear( xx,  arch= archivo_log )

   return( xx$ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd("/Users/alejandrrodr/Documents/DMUBA/DM-EyF") 

#cargo el dataset, aqui debe poner  SU RUTA
dataset  <- fread("./datasets/competencia1_2022.csv")   #donde entreno

#creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[ foto_mes==202101, clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]
# agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[, campo000 := ctrx_quarter / mactivos_margen]
dataset[, campo001 := ctrx_quarter / mcomisiones]
dataset[, campo002 := ctrx_quarter / mprestamos_personales]
dataset[, campo003 := ctrx_quarter / cprestamos_personales]
dataset[, campo004 := ctrx_quarter / mcuentas_saldo]
dataset[, campo005 := ctrx_quarter / mcuenta_corriente]
dataset[, campo006 := ctrx_quarter / ccomisiones_otras]
dataset[, campo007 := ctrx_quarter / mrentabilidad]
dataset[, campo008 := ctrx_quarter / mpasivos_margen]
dataset[, campo009 := ctrx_quarter / mcomisiones_otras]
dataset[, campo010 := ctrx_quarter / cdescubierto_preacordado]
dataset[, campo011 := ctrx_quarter / active_quarter]
dataset[, campo012 := mactivos_margen / ctrx_quarter]
dataset[, campo013 := mactivos_margen / mcomisiones]
dataset[, campo014 := mactivos_margen / mprestamos_personales]
dataset[, campo015 := mactivos_margen / cprestamos_personales]
dataset[, campo016 := mactivos_margen / mcuentas_saldo]
dataset[, campo017 := mactivos_margen / mcuenta_corriente]
dataset[, campo018 := mactivos_margen / ccomisiones_otras]
dataset[, campo019 := mactivos_margen / mrentabilidad]
dataset[, campo020 := mactivos_margen / mpasivos_margen]
dataset[, campo021 := mactivos_margen / mcomisiones_otras]
dataset[, campo022 := mactivos_margen / cdescubierto_preacordado]
dataset[, campo023 := mactivos_margen / active_quarter]
dataset[, campo024 := mprestamos_personales / ctrx_quarter]
dataset[, campo025 := mprestamos_personales / mcomisiones]
dataset[, campo026 := mprestamos_personales / mactivos_margen]
dataset[, campo027 := mprestamos_personales / cprestamos_personales]
dataset[, campo028 := mprestamos_personales / mcuentas_saldo]
dataset[, campo029 := mprestamos_personales / mcuenta_corriente]
dataset[, campo030 := mprestamos_personales / ccomisiones_otras]
dataset[, campo031 := mprestamos_personales / mrentabilidad]
dataset[, campo032 := mprestamos_personales / mpasivos_margen]
dataset[, campo033 := mprestamos_personales / mcomisiones_otras]
dataset[, campo034 := mprestamos_personales / cdescubierto_preacordado]
dataset[, campo035 := mprestamos_personales / active_quarter]
dataset[, campo036 := mcomisiones / ctrx_quarter]
dataset[, campo037 := mcomisiones / mcomisiones]
dataset[, campo038 := mcomisiones / mactivos_margen]
dataset[, campo039 := mcomisiones / cprestamos_personales]
dataset[, campo040 := mcomisiones / mcuentas_saldo]
dataset[, campo041 := mcomisiones / mcuenta_corriente]
dataset[, campo042 := mcomisiones / ccomisiones_otras]
dataset[, campo043 := mcomisiones / mrentabilidad]
dataset[, campo044 := mcomisiones / mpasivos_margen]
dataset[, campo045 := mcomisiones / mcomisiones_otras]
dataset[, campo046 := mcomisiones / cdescubierto_preacordado]
dataset[, campo047 := mcomisiones / active_quarter]
dataset[, campo048 := cprestamos_personales / ctrx_quarter]
dataset[, campo049 := cprestamos_personales / mcomisiones]
dataset[, campo050 := cprestamos_personales / mactivos_margen]
dataset[, campo051 := cprestamos_personales / mprestamos_personales]
dataset[, campo052 := cprestamos_personales / mcuentas_saldo]
dataset[, campo053 := cprestamos_personales / mcuenta_corriente]
dataset[, campo054 := cprestamos_personales / ccomisiones_otras]
dataset[, campo055 := cprestamos_personales / mrentabilidad]
dataset[, campo056 := cprestamos_personales / mpasivos_margen]
dataset[, campo057 := cprestamos_personales / mcomisiones_otras]
dataset[, campo058 := cprestamos_personales / cdescubierto_preacordado]
dataset[, campo059 := cprestamos_personales / active_quarter]
dataset[, campo060 := mcuentas_saldo / ctrx_quarter]
dataset[, campo061 := mcuentas_saldo / mcomisiones]
dataset[, campo062 := mcuentas_saldo / mactivos_margen]
dataset[, campo063 := mcuentas_saldo / mprestamos_personales]
dataset[, campo064 := mcuentas_saldo / cprestamos_personales]
dataset[, campo065 := mcuentas_saldo / mcuenta_corriente]
dataset[, campo066 := mcuentas_saldo / ccomisiones_otras]
dataset[, campo067 := mcuentas_saldo / mrentabilidad]
dataset[, campo068 := mcuentas_saldo / mpasivos_margen]
dataset[, campo069 := mcuentas_saldo / mcomisiones_otras]
dataset[, campo070 := mcuentas_saldo / cdescubierto_preacordado]
dataset[, campo071 := mcuentas_saldo / active_quarter]
dataset[, campo072 := mcuenta_corriente / ctrx_quarter]
dataset[, campo073 := mcuenta_corriente / mcomisiones]
dataset[, campo074 := mcuenta_corriente / mactivos_margen]
dataset[, campo075 := mcuenta_corriente / mprestamos_personales]
dataset[, campo076 := mcuenta_corriente / cprestamos_personales]
dataset[, campo077 := mcuenta_corriente / mcuentas_saldo]
dataset[, campo078 := mcuenta_corriente / ccomisiones_otras]
dataset[, campo079 := mcuenta_corriente / mrentabilidad]
dataset[, campo080 := mcuenta_corriente / mpasivos_margen]
dataset[, campo081 := mcuenta_corriente / mcomisiones_otras]
dataset[, campo082 := mcuenta_corriente / cdescubierto_preacordado]
dataset[, campo083 := mcuenta_corriente / active_quarter]
dataset[, campo084 := ccomisiones_otras / ctrx_quarter]
dataset[, campo085 := ccomisiones_otras / mcomisiones]
dataset[, campo086 := ccomisiones_otras / mactivos_margen]
dataset[, campo087 := ccomisiones_otras / mprestamos_personales]
dataset[, campo088 := ccomisiones_otras / cprestamos_personales]
dataset[, campo089 := ccomisiones_otras / mcuentas_saldo]
dataset[, campo090 := ccomisiones_otras / mcuenta_corriente]
dataset[, campo091 := ccomisiones_otras / mrentabilidad]
dataset[, campo092 := ccomisiones_otras / mpasivos_margen]
dataset[, campo093 := ccomisiones_otras / mcomisiones_otras]
dataset[, campo094 := ccomisiones_otras / cdescubierto_preacordado]
dataset[, campo095 := ccomisiones_otras / active_quarter]
dataset[, campo096 := mrentabilidad / ctrx_quarter]
dataset[, campo097 := mrentabilidad / mcomisiones]
dataset[, campo098 := mrentabilidad / mactivos_margen]
dataset[, campo099 := mrentabilidad / mprestamos_personales]
dataset[, campo100 := mrentabilidad / cprestamos_personales]
dataset[, campo101 := mrentabilidad / mcuentas_saldo]
dataset[, campo102 := mrentabilidad / mcuenta_corriente]
dataset[, campo103 := mrentabilidad / ccomisiones_otras]
dataset[, campo104 := mrentabilidad / mpasivos_margen]
dataset[, campo105 := mrentabilidad / mcomisiones_otras]
dataset[, campo106 := mrentabilidad / cdescubierto_preacordado]
dataset[, campo107 := mrentabilidad / active_quarter]
dataset[, campo108 := mpasivos_margen / ctrx_quarter]
dataset[, campo109 := mpasivos_margen / mcomisiones]
dataset[, campo110 := mpasivos_margen / mactivos_margen]
dataset[, campo111 := mpasivos_margen / mprestamos_personales]
dataset[, campo112 := mpasivos_margen / cprestamos_personales]
dataset[, campo113 := mpasivos_margen / mcuentas_saldo]
dataset[, campo114 := mpasivos_margen / mcuenta_corriente]
dataset[, campo115 := mpasivos_margen / ccomisiones_otras]
dataset[, campo116 := mpasivos_margen / mrentabilidad]
dataset[, campo117 := mpasivos_margen / mcomisiones_otras]
dataset[, campo118 := mpasivos_margen / cdescubierto_preacordado]
dataset[, campo119 := mpasivos_margen / active_quarter]
dataset[, campo120 := mcomisiones_otras / ctrx_quarter]
dataset[, campo121 := mcomisiones_otras / mcomisiones]
dataset[, campo122 := mcomisiones_otras / mactivos_margen]
dataset[, campo123 := mcomisiones_otras / mprestamos_personales]
dataset[, campo124 := mcomisiones_otras / cprestamos_personales]
dataset[, campo125 := mcomisiones_otras / mcuentas_saldo]
dataset[, campo126 := mcomisiones_otras / mcuenta_corriente]
dataset[, campo127 := mcomisiones_otras / ccomisiones_otras]
dataset[, campo128 := mcomisiones_otras / mrentabilidad]
dataset[, campo129 := mcomisiones_otras / mpasivos_margen]
dataset[, campo130 := mcomisiones_otras / cdescubierto_preacordado]
dataset[, campo131 := mcomisiones_otras / active_quarter]
dataset[, campo132 := cdescubierto_preacordado / ctrx_quarter]
dataset[, campo133 := cdescubierto_preacordado / mcomisiones]
dataset[, campo134 := cdescubierto_preacordado / mactivos_margen]
dataset[, campo135 := cdescubierto_preacordado / mprestamos_personales]
dataset[, campo136 := cdescubierto_preacordado / cprestamos_personales]
dataset[, campo137 := cdescubierto_preacordado / mcuentas_saldo]
dataset[, campo138 := cdescubierto_preacordado / mcuenta_corriente]
dataset[, campo139 := cdescubierto_preacordado / ccomisiones_otras]
dataset[, campo140 := cdescubierto_preacordado / mrentabilidad]
dataset[, campo141 := cdescubierto_preacordado / mpasivos_margen]
dataset[, campo142 := cdescubierto_preacordado / mcomisiones_otras]
dataset[, campo143 := cdescubierto_preacordado / active_quarter]
dataset[, campo144 := active_quarter / ctrx_quarter]
dataset[, campo145 := active_quarter / mcomisiones]
dataset[, campo146 := active_quarter / mactivos_margen]
dataset[, campo147 := active_quarter / mprestamos_personales]
dataset[, campo148 := active_quarter / cprestamos_personales]
dataset[, campo149 := active_quarter / mcuentas_saldo]
dataset[, campo150 := active_quarter / mcuenta_corriente]
dataset[, campo151 := active_quarter / ccomisiones_otras]
dataset[, campo152 := active_quarter / mrentabilidad]
dataset[, campo153 := active_quarter / mpasivos_margen]
dataset[, campo154 := active_quarter / cdescubierto_preacordado]
dataset[, campo155 := active_quarter / cdescubierto_preacordado]

#defino los datos donde entreno
dtrain  <- dataset[ foto_mes==202101, ]


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT4110.1/", showWarnings = FALSE )
setwd("./exp/HT4110.1/")   #Establezco el Working Directory DEL EXPERIMENTO

#defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log  <- "HT4110.1.txt"
archivo_BO   <- "HT4110.1.RDATA"

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0

if( file.exists(archivo_log) )
{
 tabla_log  <- fread( archivo_log )
 GLOBAL_iteracion  <- nrow( tabla_log )
}



#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,
              has.simple.signature = FALSE   #espia Tomas Delvechio, dejar este parametro asi
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( archivo_BO ) ) {

  run  <- mbo( fun=     obj.fun, 
               learner= surr.km,
               control= ctrl)

} else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista

