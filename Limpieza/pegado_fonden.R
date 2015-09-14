#FONDEN pegar historica deflactada a precios de 2015 con la de 2013
library(tidyr)
library(dplyr)
library(Hmisc)

F_13<- read.csv("~/Desktop/Tesis_DS/Datos/procesados/Fonden_13_limpia.csv", stringsAsFactors=FALSE)
F_99_12 <-read.csv("~/Desktop/Tesis_DS/Datos/procesados/FONDEN_99_13m.csv", stringsAsFactors=FALSE)

names (F_13)
names(F_99_12)
names(F_13) <- c("No.","Estatus","NR","Estado", "Clave_estado","Evento","Fenomeno","Clasificacion","TipoDeDeclaratoria", "Municipios","Solicitud.de.Declaratoria","FechaDOF","AcuerdoComiteTecnico_Original","Sector","Rubro","Apoyos.Parciales.Inmdiatos.APIN","Gastos.de.Evaluacion", "Anticipos","Anticipos.1","Acciones.de.Restauracion", "Monto_FONDEN_d","Monto_Estatal_d","Aportacion.Dependencia.Federal..Pesos.", "Total_d", "dif")

F_13_1 <- F_13%>%mutate(a_noDOF=substr(FechaDOF,1,4))



intersect(names(F_99_12), names(F_13_1))
F_99_12_1<- subset(F_99_12, select= c("Estatus", "NR", "Estado" ,"Clave_estado" ,"Evento" ,"Fenomeno"  ,"Clasificacion","TipoDeDeclaratoria" ,"Municipios"  ,"FechaDOF"  ,"a_noDOF","Sector" , "Rubro" ,"Monto_FONDEN_d" , "Monto_Estatal_d" ,"Total_d" ) )

F_13_2 <- subset(F_13_1,select= c("Estatus", "NR", "Estado" ,"Clave_estado" ,"Evento" ,"Fenomeno"  ,"Clasificacion","TipoDeDeclaratoria" ,"Municipios"  ,"FechaDOF"  ,"a_noDOF","Sector" , "Rubro" ,"Monto_FONDEN_d" , "Monto_Estatal_d" ,"Total_d" ) )

#Base Pegada 
FONDEN_99_13<- rbind(F_99_12_1,F_13_2)

#write.csv(FONDEN_99_13, file = "~/Desktop/Tesis_DS/Datos/FONDEN/Fonden_99_13_deflactada.csv", row.names=FALSE)



#Ahora tenemos que quitar las sequias, Geo y recortar por año
describe(FONDEN_99_13)

#hay 115 sequias , 190 GEO, 11 QUIM de 2003-3013
FONDEN_3_13<- FONDEN_99_13 %>%
  filter(Clasificacion == "HIDRO") %>%
  filter(Fenomeno!="SEQ")%>%
  filter(a_noDOF > 2002)
  subset(FONDEN_99_13, Clasificacion == "HIDRO")

describe(FONDEN_3_13)

write.csv(FONDEN_3_13, file = "~/Desktop/Tesis_DS/Datos/FONDEN/Fonden_3_13_completa.csv", row.names=FALSE)
names(FONDEN_3_13)

#Ahora la agrupamos por clave estado, 

FONDEN_3_13_montos <-FONDEN_3_13 %>%
  group_by(Estado,Clave_estado, Fenomeno, Clasificacion, TipoDeDeclaratoria, FechaDOF, a_noDOF, Municipios)%>%
  summarise(sum(Monto_FONDEN_d,na.rm = TRUE), sum(Monto_Estatal_d,na.rm = TRUE), sum(Total_d,na.rm = TRUE))
  
write.csv(FONDEN_3_13_montos, file = "~/Desktop/Tesis_DS/Datos/procesados/fonden_3_13_montos.csv", row.names=FALSE)

