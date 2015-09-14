#Deflactar FONDEN 2013 
FONDEN_2013 <- read.csv("~/Desktop/Tesis_DS/Datos/FONDEN/FONDEN_2013.csv", stringsAsFactors=FALSE)
names(FONDEN_2013)

#Deflactor 
#deflactamos 2013 a precios constantes de 2015
FONDEN_2013_def <- data.frame( FONDEN_2013[1:15], (FONDEN_2013[16:24]*0.94))

# declaramos cuales son fechas
FONDEN_2013_def <- FONDEN_2013_def %>%
         mutate(Solicitud.de.Declaratoria = as.Date(Solicitud.de.Declaratoria, format="%m/%d/%Y"),
               FechaDOF = as.Date(FechaDOF, format="%m/%d/%Y"))

#diferencia de dias entre la solicitud y la declaratoria
dif <- FONDEN_2013_def %>%
  mutate (dif=difftime(FechaDOF, Solicitud.de.Declaratoria, units = c("days") ))

# veamos cuanto tardaron en atender desastres  desastres que sucedieron en 2013
dif.1<-subset (dif, dif!= "-20")
mean(dif.1$dif)
max(dif.1$dif)
min (dif.1$dif)
describe(dif.1$dif)
df<- as.data.frame(dif$FechaDOF, dif$Solicitud.de.Declaratoria, dif$dif )
View(dif[(11:10, 25),])
names(dif)

write.csv(FIM, file = "~/Desktop/Tesis_DS/Datos/procesados/Fonden_13_limpia.csv", row.names=FALSE)
