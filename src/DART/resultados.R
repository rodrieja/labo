require("data.table")
library(ggplot2)

setwd( "/Users/alejandrrodr/Documents/DMUBA/DM-EyF/labo/src/DART" )  #cambiar por la carpeta local

res_kaggle  <- fread( "./resultados.csv", stringsAsFactors= TRUE)
res_kaggle

p<-ggplot(data=res_kaggle, aes(x=corte, y=ganancia, group=tipo)) +
  geom_line(aes(color=tipo))+
  geom_point(aes(color=tipo))+
  labs(title="Ganancias por modelo", y = "Millones $")
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

