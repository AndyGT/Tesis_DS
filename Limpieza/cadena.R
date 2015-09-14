#CADENA (millones de pesos)
library(tidyr)
library(dplyr)
library(Hmisc)

CADENA_CC_historica <- read.csv("~/Desktop/Tesis_DS/Datos/CADENA/CADENA_CC_historica.csv", stringsAsFactors=FALSE)
INPC_99_15 <- read.csv("~/Dropbox/Tesis_CD/Datos/INPC_99_15.csv", stringsAsFactors=FALSE)


# deflactar, agrpar y quedarnos con el periodo de interes


names(INPC_99_15)<-c("a_noDOF","deflactor")
CADENA <- merge(CADENA_CC_historica,INPC_99_15, by = intersect(names(CADENA_CC_historica), names(INPC_99_15)))

# deflactamos y pasamos a pesos ( estaba en millones)
CADENA.1 <- CADENA %>%
  mutate(Monto_CADENA_d = (federal / deflactor)*1000000, 
Monto_Estatal_d = (estatal/deflactor)*1000000,
Total_d = (total / deflactor)*1000000 

#Agrupamos
CADENA.2 <- CADENA.1%>%
  filter(a_noDOF>2002, fenomeno!="SEQ",fenomeno!="SAC" )%>%
  group_by(a_noDOF, Clave_estado, fenomeno)%>%
  summarise(sum(Monto_CADENA_d, na.rm = TRUE), sum(Monto_Estatal_d, na.rm = TRUE), sum(Total_d,na.rm = TRUE))
describe(CADENA.2)

write.csv(CADENA.2, file = "~/Desktop/Tesis_DS/Datos/procesados/cadena_3_13.csv", row.names=FALSE)
