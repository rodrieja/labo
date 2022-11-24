# Competencia FINAL
## Replicabilidad

### Generación de OB para los modelos
Es necesario generar el FE base y la OB con el TS de todos los meses

/competencia_FINAL/FE_competencia_final_base.r
/competencia_FINAL/OB/Training_Strategy_competencia_final_base.r
/competencia_FINAL/OB/HT_lightgbm_under_modificado_20192021.r
/competencia_FINAL/OB/ZZ_lightgbm_under_modificado_20192021.r

### Semillerio
/competencia_FINAL/semillerio/semillero_under_modificado_20192021.r
/competencia_FINAL/semillerio/semillero_under_results_20192021.r

### Semillerio naive
/competencia_FINAL/semillerio/semillero_under_first_20_20192021.r
/competencia_FINAL/semillerio/semillero_under_first_20_results_20192021.r

### FE Random Forest
/competencia_FINAL/RF/RF_FE_cuarta_competencia.r
/competencia_FINAL/RF/RF_Training_Strategy_20192021.r
/competencia_FINAL/RF/RF_HT_lightgbm_under_modificado_20192021.r
/competencia_FINAL/RF/RF_semillero_under_20192021.r
/competencia_FINAL/RF/RF_semillero_under_results_20192021.r

### Generación del modelo con la mejor segunda OB
```cut --output-separator="," -f1,2,3 ~/buckets/b1/exp/AD_ZZ9430_TRAINING_201901-202105/pred_02_074.csv > ~/buckets/b1/exp/ZZ_HT_BO_02/prob_semillerio.csv```

renombrar cabecera prob prob_promedio
lo realice desde vi, pero automatizado, no se me ocurre como hacerlo

### Hibridación de los modelos
/competencia_FINAL/hibridacion/HM_semillero_under.r






