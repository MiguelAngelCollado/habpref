
#Tomamos los datos nos vienen dados, este script es para limpiarlos
d <- read.table("~/Desktop/Tesis/R/habpref full data/Combines Taxonomy File to Genus Augmented Flat File.txt", sep = "$", header = TRUE, quote = "", 
                comment.char = "")
#Establecemos working directory
setwd("~/Desktop/Tesis/R/habpref full data/")
getwd()
#Creamos unos datos de base
write.csv(d,"Datosraw.csv")
sd.1 <- d
####Corregimos sexo----
#En la variable sex, cambiamos todo por male, female, worker, queen, unknown
str(sd.1)
summary(sd.1$sex)
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="", "unknown")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="Destroyed", "unknown")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="femal", "Female")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="Female", "female")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="female?", "unknown")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="group", "unknown")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="Halictus ligatus/poeyi", "unknown")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="Male", "male")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="Male", "male")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="Queen", "queen")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="undetermined", "unknown")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="USGS_DRO10530", "unknown")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="USGS_DRO10536", "unknown")
sd.1$sex <- replace(sd.1$sex, sd.1$sex=="W", "unknown")
summary(sd.1$sex)
#Corregimos estado----
#Vamos a por la variable state
sd.2 <- sd.1
str(sd.2)
summary(sd.2$state)
sd.2$state <- replace(sd.2$state, sd.2$state=="", "NA")
sd.2$state <- replace(sd.2$state, sd.2$state=="D of Columbia", "District of Columbia")
sd.2$state <- replace(sd.2$state, sd.2$state=="D.C.", "District of Columbia")
sd.2$state <- replace(sd.2$state, sd.2$state=="D.Columbia", "District of Columbia")
sd.2$state <- replace(sd.2$state, sd.2$state=="D. Columbia", "District of Columbia")
sd.2$state <- replace(sd.2$state, sd.2$state=="DC", "District of Columbia")
sd.2$state <- replace(sd.2$state, sd.2$state=="Jane Whitaker", "NA")
sd.2$state <- replace(sd.2$state, sd.2$state=="SC", "South Carolina")
sd.2$state <- replace(sd.2$state, sd.2$state=="USA", "xddddd")
summary(sd.2$state)
#Corregimos fecha y hora----
sd.3 <- sd.2
summary(sd.3$time1, maxsum = 500000)
str(sd.3)
# fix times where there is an "E" present (eg. 2.00309E+13 = 200309)
# remove x's
sd.3$time1 <- sub("x", "", sd.3$time1, fixed = TRUE)
sd.3$time2 <- sub("x", "", sd.3$time2, fixed = TRUE)
# get years
sd.3$year1 <- as.numeric(substr(sd.3$time1, 1, 4))
sd.3$year2 <- as.numeric(substr(sd.3$time2, 1, 4))
# get months
sd.3$month1 <- as.numeric(substr(sd.3$time1, 5, 6))
sd.3$month2 <- as.numeric(substr(sd.3$time2, 5, 6))
# get days
sd.3$day1 <- as.numeric(substr(sd.3$time1, 7, 8))
sd.3$day2 <- as.numeric(substr(sd.3$time2, 7, 8))
# get hours
sd.3$hours1 <- as.numeric(substr(sd.3$time1, 9, 10))
sd.3$hours2 <- as.numeric(substr(sd.3$time2, 9, 10))
# get minutes
sd.3$min1 <- as.numeric(substr(sd.3$time1, 11, 12))
sd.3$min2 <- as.numeric(substr(sd.3$time2, 11, 12))
#Renombramos columnas----
sd.4 <- sd.3
names(sd.4)
names(sd.4)[which(names(sd.4)=="how0")]="method"
names(sd.4)[which(names(sd.4)=="how1")]="bowl.num"
names(sd.4)[which(names(sd.4)=="how2")]="bowl.size"
names(sd.4)[which(names(sd.4)=="how3")]="bowl.col"
names(sd.4)[which(names(sd.4)=="how4")]="bowl.fill"
names(sd.4)[which(names(sd.4)=="X100.Order")]="Order"
names(sd.4)[which(names(sd.4)=="X90.Superorder")]="Superorder"
names(sd.4)[which(names(sd.4)=="X110.Suborder")]="Suborder"
names(sd.4)[which(names(sd.4)=="X130.Superfamily")]="Superfamily"
names(sd.4)[which(names(sd.4)=="X140.Family")]="Family"
names(sd.4)[which(names(sd.4)=="X150.Subfamily")]="Subfamily"
names(sd.4)[which(names(sd.4)=="X160.Tribe")]="Tribe"
names(sd.4)[which(names(sd.4)=="X170.Subtribe")]="Subtribe"
names(sd.4)[which(names(sd.4)=="X170.Subtribe")]="Subtribe"
names(sd.4)[which(names(sd.4)=="ID1")]="IDs1"
names(sd.4)[which(names(sd.4)=="ID")]="IDs"
names(sd.4)[which(names(sd.4)=="COLLECTIONdb")]="id"
names(sd.4)
# species cleaning (column `name`)
# remove all lines where `name` has no information
str(sd.4)
names(sd.4)
#filtramos nombres que no estén claros
nrow(sd.4)
nrow(sd.3)
#Retiramos lo que no sea hymenoptera
sd.4<-subset(sd.4, subset=(sd.4$Order=="Hymenoptera"))
summary(sd.4$name, maxsum = 1400)
#Retiramos o reagrupamos las especies con nombres raros, confusos y mal escritos-----
#Estas especies tienen más de 100 casos por lo que son relevantes el hecho de
summary(sd.4$name)
#Renombramos----
#Evaluamos, si están agrupadas e individualmente no aportan mucho, las agrupamos o las
#dejamos como individuales
levels(sd.4$name)[levels(sd.4$name)=="Andrena  carlini"]<-"Andrena carlini"
levels(sd.4$name)[levels(sd.4$name)=="Andrena  nasonii"]<-"Andrena nasonii"
levels(sd.4$name)[levels(sd.4$name)=="Apis  mellifera"]<-"Apis mellifera"
levels(sd.4$name)[levels(sd.4$name)=="Andrena  nasonii"]<-"Andrena nasonii"
a<-summary(sd.4$name, maxsum=5000)
#
a[["Agapostemon angelicus/texanus"]]
a[["Agapostemon angelicus"]]
a[["Agapostemon texanus"]]
levels(sd.4$name)[levels(sd.4$name)=="Agapostemon angelicus"]<-"Agapostemon angelicus/texanus"
levels(sd.4$name)[levels(sd.4$name)=="Agapostemon texanus"]<-"Agapostemon angelicus/texanus"
#
a[["Ceratina dupla/mikmaqi"]]
a[["Ceratina dupla"]]
a[["Ceratina mikmaqi"]]
levels(sd.4$name)[levels(sd.4$name)=="Ceratina dupla"]<-"Ceratina dupla/mikmaqi"
levels(sd.4$name)[levels(sd.4$name)=="Ceratina mikmaqi"]<-"Ceratina dupla/mikmaqi"
#Halictus ligatus/poeyi
a[["Halictus ligatus/poeyi"]]
a[["Halictus ligatus"]]
a[["Halictus poeyi"]]
a[["Nomada poeyi/ligatus"]]
levels(sd.4$name)[levels(sd.4$name)=="Halictus ligatus"]<-"Halictus ligatus/poeyi"
levels(sd.4$name)[levels(sd.4$name)=="Halictus poeyi"]<-"Halictus ligatus/poeyi"
levels(sd.4$name)[levels(sd.4$name)=="Nomada poeyi/ligatus"]<-"Halictus ligatus/poeyi"
#
a[["Lasioglossum near_admirandum"]]
a[["Lasioglossum admirandum"]]
levels(sd.4$name)[levels(sd.4$name)=="Lasioglossum admirandum"]<-"Lasioglossum admirandum"
#Lasioglossum viridatum_group
a[["Lasioglossum viridatum_group"]]
a[["Lasioglossum viridatum"]]
levels(sd.4$name)[levels(sd.4$name)=="Lasioglossum viridatum"]<-"Lasioglossum viridatum_group"
#"Hylaeus affinis/modestus"
a[["Hylaeus affinis/modestus"]]
a[["Hylaeus affinis"]]
a[["Hylaeus modestus"]]
levels(sd.4$name)[levels(sd.4$name)=="Hylaeus affinis"]<-"Hylaeus affinis/modestus"
levels(sd.4$name)[levels(sd.4$name)=="Hylaeus modestus"]<-"Hylaeus affinis/modestus"
#"Ceratina calcarata/dupla/mikmaqi"
a[["Ceratina calcarata/dupla/mikmaqi"]]
a[["Ceratina calcarata"]]
a[["Ceratina dupla/mikmaqi"]]
a[["Ceratina dupla"]]
a[["Ceratina mikmaqi"]]
a[["Ceratina calcarata/mikmaqi"]]
a[["Ceratina dupla/calcarata"]]
a[["Ceratica calcarata/dupla"]]
a[["Ceratina mikmaqi/calcarata"]]
a[["Ceratina dupla/mikmaqi/calcarata"]]
levels(sd.4$name)[levels(sd.4$name)=="Ceratina calcarata"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.4$name)[levels(sd.4$name)=="Ceratina dupla/mikmaqi"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.4$name)[levels(sd.4$name)=="Ceratina calcarata/mikmaqi"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.4$name)[levels(sd.4$name)=="Ceratina dupla/calcarata"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.4$name)[levels(sd.4$name)=="Ceratica calcarata/dupla"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.4$name)[levels(sd.4$name)=="Ceratina mikmaqi/calcarata"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.4$name)[levels(sd.4$name)=="Ceratina dupla/mikmaqi/calcarata"]<-"Ceratina calcarata/dupla/mikmaqi"
#"Nomada bidentate_species_group"
a[["Nomada bidentate_species_group"]]
a[["Nomada bidentate_group"]]
levels(sd.4$name)[levels(sd.4$name)=="Nomada bidentate_species_group"]<-"Nomada bidentate_group"
#"Lasioglossum lepidii/puteulanum"
a[["Lasioglossum lepidii/puteulanum"]]
a[["Lasioglossum lepidii"]]
a[["Lasioglossum puteulanum"]]
a[["Lasioglossum puteulanum/lepidii"]]
levels(sd.4$name)[levels(sd.4$name)=="Lasioglossum lepidii"]<-"Lasioglossum lepidii/puteulanum"
levels(sd.4$name)[levels(sd.4$name)=="Lasioglossum puteulanum"]<-"Lasioglossum lepidii/puteulanum"
levels(sd.4$name)[levels(sd.4$name)=="Lasioglossum puteulanum/lepidii"]<-"Lasioglossum lepidii/puteulanum"
#"Halictus poeyi/ligatus"
a[["Halictus poeyi/ligatus"]]
a[["Halictus poeyi"]]
a[["Halictus ligatus"]]
a[["Halictus ligatus/poeyi"]]
levels(sd.4$name)[levels(sd.4$name)=="Halictus poeyi/ligatus"]<-"Halictus ligatus/poeyi"
levels(sd.4$name)[levels(sd.4$name)=="Halictus poeyi"]<-"Halictus ligatus/poeyi"
#"Lasioglossum affiliated_with_punctatoventre"
a[["Lasioglossum affiliated_with_punctatoventre"]]
#"Osmia pumila/atriventris" son pocas las quito
a[["Osmia pumila/atriventris"]]
a[["Osmia pumila"]]
a[["Osmia atriventris"]]
#"Lasioglossum admirandum/rohweri"
a[["Lasioglossum admirandum/rohweri"]]
a[["Lasioglossum admirandum"]]
a[["Lasioglossum rohweri/lineatulum"]]
a[["Lasioglossum rohweri/admirandum"]]
levels(sd.4$name)[levels(sd.4$name)=="Lasioglossum admirandum"]<-"Lasioglossum admirandum/rohweri"
levels(sd.4$name)[levels(sd.4$name)=="Lasioglossum rohweri/lineatulum"]<-"Lasioglossum admirandum/rohweri"
levels(sd.4$name)[levels(sd.4$name)=="Lasioglossum rohweri/admirandum"]<-"Lasioglossum admirandum/rohweri"
#"Nomada sayi/illinoensis"
a[["Nomada sayi/illinoensis"]]
a[["Nomada sayi"]]
a[["Nomada illinoensis"]]
a[["Nomada illinoensi/sayi"]]
levels(sd.4$name)[levels(sd.4$name)=="Nomada sayi"]<-"Nomada sayi/illinoensis"
levels(sd.4$name)[levels(sd.4$name)=="Nomada illinoensis"]<-"Nomada sayi/illinoensis"
levels(sd.4$name)[levels(sd.4$name)=="Nomada illinoensi/sayi"]<-"Nomada sayi/illinoensis"
#"Andrena imitatrix/morrisonella"
a[["Andrena imitatrix/morrisonella"]]
a[["Andrena imitatrix"]]
a[["Andrena morrisonella"]]
a[["Lasioglossum imitatrix/morrisonella"]]
levels(sd.4$name)[levels(sd.4$name)=="Andrena imitatrix/morrisonella"]<-"Lasioglossum imitatrix/morrisonella"
#"Lasioglossum near_puteulanum"
a[["Lasioglossum near_puteulanum"]]
a[["Lasioglossum puteulanum"]]
levels(sd.4$name)[levels(sd.4$name)=="Lasioglossum near_puteulanum"]<-"Lasioglossum lepidii/puteulanum"
#"Osmia near_collinsiae"
a[["Osmia near_collinsiae"]]
a[["Osmia collinsiae"]]
#"Lasioglossum near_ruidosense"
a[["Lasioglossum near_ruidosense"]]
a[["Lasioglossum ruidosense"]]
a<-summary(sd.4$name, maxsum=5000)
summary(sd.4$name, maxsum=1000)
#Filtramos individuos mal etiquetados
sd.4<-subset(sd.4, subset=(!sd.4$name == "" & !sd.4$name == "??" & 
                             !sd.4$name == "Andrena species"&
                             !sd.4$name == "Osmia species"& 
                             !sd.4$name == "Ceratina species"& 
                             !sd.4$name == "Lasioglossum"& 
                             !sd.4$name == "Osmia pumila/atriventris"& 
                             !sd.4$name == "wasp"& 
                             !sd.4$name == "Wasps"& 
                             !sd.4$name == "wasp"& 
                             !sd.4$name == "Wasps"& 
                             !sd.4$name == "Andrena (trachandrena)"& 
                             !sd.4$name == "Nomada species"&
                             !sd.4$name == "Hylaeus species"
                             ))
#Filtramos ahora por coordenadas
sd.5<-sd.4 
summary(sd.5$latitude)
summary(sd.5$longitude)
sd.5<-subset(sd.5, subset=(sd.5$latitude>-40&sd.5$latitude<45))
sd.5<-subset(sd.5, subset=(sd.5$latitude>-40))

sd.5<-subset(sd.5, subset=(!sd.5$latitude>100))
sd.5<-subset(sd.5, subset=(sd.5$longitude>-165&sd.5$longitude<130))
######Representamos los puntos geograficamente de EEUU para seleccionar y vamos cortando----
sd.6<-sd.5
sd.6<-subset(sd.6, subset=(country=="USA"))
nrow(sd.3)
nrow(sd.6)

#cargamos las librerias (a lo mejor tienes que instalarlas)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(rgdal)
library(dismo)
library(jsonlite)

#Cargamos conjunto de datos
names(sd.4)
#Hacemos subset solo con las variables que nos interesan
locs <- subset(sd.6, select = c("country", "latitude", "longitude"))
#Comprobamos que tenemos los datos que queremos
head(locs) 
#Descartamos los datos con errores en las coordenadas
locs <- subset(locs, locs$latitude < 90)
#Comprobamos que no haya NA ni datos extra??os
View(locs)
#Tenemos un dataframe con coordinadas, podemos plasmarlas ahora espacialmente
coordinates(locs) <- c("longitude", "latitude")  
plot(locs)
#Definimos la proyecci??n geogr??fica, hay que ponerle un PROJ4 espec??fico a cada zona
#o algo as?? nuse nuse Xdxdxdzd
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)
#Cambiamos el color y tama??o de los puntos
plot(locs, pch = 4, col = "red")
#Cargamos library que contiene muchos tipos de mapas, por ejemplo estos
library(rworldmap)
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)

lemap <- gmap(locs, type = "satellite")
locs.merc <- Mercator(locs)
plot(lemap)
points(locs.merc, pch = 4, col = "red")

##Ahora filtramos por coordenadas a que elijamos-----
sd.7<-sd.5
head(sd.7)
sd.7<-subset(sd.7, subset=(sd.7$latitude>35.00&sd.7$latitude<43.00))
sd.7<-subset(sd.7, subset=(sd.7$longitude>(-87.55)&sd.7$longitude<(-69.85)))
summary(sd.7$latitude)
summary(sd.7$longitude)

#Hacemos subset solo con las variables que nos interesan
locs <- subset(sd.7, select = c("country", "latitude", "longitude"))
#Comprobamos que tenemos los datos que queremos
head(locs) 
#Descartamos los datos con errores en las coordenadas
locs <- subset(locs, locs$latitude < 90)
#Comprobamos que no haya NA ni datos extra??os
View(locs)
#Tenemos un dataframe con coordinadas, podemos plasmarlas ahora espacialmente
coordinates(locs) <- c("longitude", "latitude")  
plot(locs)
#Definimos la proyecci??n geogr??fica, hay que ponerle un PROJ4 espec??fico a cada zona
#o algo as?? nuse nuse Xdxdxdzd
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)


#Cambiamos el color y tamaño de los puntos
plot(locs, pch = 4, col = "red")
#Cargamos library que contiene muchos tipos de mapas, por ejemplo estos
library(rworldmap)
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)

lemap <- gmap(locs, type = "satellite")
locs.merc <- Mercator(locs)
plot(lemap)
points(locs.merc, pch = 4, col = "red")


#Ahora filtremos más nombres raros--------
sd.8<-sd.7
b<-summary(sd.8$name, maxsum=860)

#"Nomada near_parva"
b[["Andrena thaspii"]]
b[["Andrena carolina"]]
levels(sd.8$name)[levels(sd.8$name)=="Andrena thaspii"]<-"Andrena thaspii/carolina"
levels(sd.8$name)[levels(sd.8$name)=="Andrena carolina"]<-"Andrena thaspii/carolina"
b[["Hylaeus modestus/affinis"]]
b[["Chrysis nitidula"]]
b[["Chrysis nitidula/coerulans"]]
levels(sd.8$name)[levels(sd.8$name)=="Chrysis nitidula"]<-"Chrysis nitidula/coerulans"
b[["Sphecodes cressonii/atlantis"]]
b[["Sphecodes cressonii"]]
b[["Sphecodes atlantis"]]
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes cressonii"]<-"Sphecodes cressonii/atlantis"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes atlantis"]<-"Sphecodes cressonii/atlantis"
b[["Melissodes boltoniae/fumosa"]]
b[["Melissodes boltoniae"]]
levels(sd.8$name)[levels(sd.8$name)=="Melissodes boltoniae"]<-"Melissodes boltoniae/fumosa"
levels(sd.8$name)[levels(sd.8$name)=="Melissodes fumosa"]<-"Melissodes boltoniae/fumosa"
b[["Melissodes illata/subillata"]]
b[["Melissodes illata"]]
b[["Melissodes subillata"]]
levels(sd.8$name)[levels(sd.8$name)=="Melissodes subillata"]<-"Melissodes illata/subillata"
levels(sd.8$name)[levels(sd.8$name)=="Melissodes illata"]<-"Melissodes illata/subillata"
b[["Hylaeus  affinis/modestus"]]
b[["Hylaeus affinis/modestus"]]
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus  affinis/modestus"]<-"Hylaeus affinis/modestus"
b[["Lasioglossum mitchelli/weemsi"]]
b[["Lasioglossum mitchelli"]]
b[["Lasioglossum weemsi"]]
b[["Lasioglossum rohweri"]]
b[["Lasioglossum versatum/rohweri"]]
b[["Lasioglossum versatum"]]
b[["Lasioglossum rohweri"]]
levels(sd.8$name)[levels(sd.8$name)=="Lasioglossum mitchelli"]<-"Lasioglossum mitchelli/weemsi"
levels(sd.8$name)[levels(sd.8$name)=="Lasioglossum weemsi"]<-"Lasioglossum mitchelli/weemsi"
b[["Osmia cornifrons"]]
b[["Osmia taurus"]]
b[["Lasioglossum pilosum"]]
b[["Lasioglossum leucocomum"]]
b[["Lasioglossum rohweri/versatum"]]
b[["Lasioglossum versatum/rohweri"]]
levels(sd.8$name)[levels(sd.8$name)=="Lasioglossum versatum/rohweri"]<-"Lasioglossum rohweri/versatum"
b[["Lasgioglossum rohweri/lineatulum"]]
b[["Lasioglossum pilosum"]]
b[["Lasioglossum leucocomum"]]
b[["Nomada bidentate species"]]
summary(sd.8$name, maxsum=860)
#Filtramos nombres inconclusos o singletones
sd.8<-subset(sd.8, subset=(!sd.8$name == "" & !sd.8$name == "??" & 
                             !sd.8$name == "Nomada near_parva" & 
                             !sd.8$name == "Nomada near_cressonii" & 
                             !sd.8$name == "Nomada near_luteoloides" & 
                             !sd.8$name == "Nomada near_armatella" & 
                             !sd.8$name == "Melissodes near_agilis" & 
                             !sd.8$name == "Melissodes near_comptoides" & 
                             !sd.8$name == "Lasioglossum weemsi?" & 
                             !sd.8$name == "Lasioglossum planatum?" & 
                             !sd.8$name == "Lasioglossum near_zephyrum" & 
                             !sd.8$name == "Lasioglossum near_pilosum" & 
                             !sd.8$name == "lasioglossum heterognathum" & 
                             !sd.8$name == "Lasioglossum leucocomum?" & 
                             !sd.8$name == "Lasioglossum admirandum?" & 
                             !sd.8$name == "Hylaeus near_annulatus" & 
                             !sd.8$name == "Hylaeus nelumbonis/ornatus" & 
                             !sd.8$name == "Epeolus ilicis?" & 
                             !sd.8$name == "Andrena uvulariae" & 
                             !sd.8$name == "Andrena near_mariae" & 
                             !sd.8$name == "Andrena (Trachandrena)" & 
                             !sd.8$name == "Andrena (tranchandrena)" & 
                             !sd.8$name == "Nomada sulphurata/luteola" & 
                             !sd.8$name == "Nomada near_lehighensis" & 
                             !sd.8$name == "Nomada near_fragariae" & 
                             !sd.8$name == "Nomada near_Inepta/denticulata" & 
                             !sd.8$name == "Nomada near_fragariae" & 
                             !sd.8$name == "Nomada gracilis?" & 
                             !sd.8$name == "Melissodes near_subillata" & 
                             !sd.8$name == "Melissodes agilis/trinodis" & 
                             !sd.8$name == "Melissodes agilis/nivea" & 
                             !sd.8$name == "Megachile near_texana" & 
                             !sd.8$name == "Lasioglossum near_perpunctatum" & 
                             !sd.8$name == "Lasioglossum near_sagax"&
                             !sd.8$name == "Lasioglossum lineatulum/smilancinae" & 
                             !sd.8$name == "Heriades variolosa/leavitti" &
                             !sd.8$name == "Hoplitis producta/pilosifrons" &
                             !sd.8$name == "Heriades leavitti/variolosa" &
                             !sd.8$name == "Nomada near_inepta/denticulata" &
                             !sd.8$name == "Andrena near_barbara" &
                             !sd.8$name == "Nomada near_composita" &
                             !sd.8$name == "Lasioglossum Species" &
                             !sd.8$name == "Megachile mendica/brevis" &
                             !sd.8$name == "Lasioglossum near_planatum" &
                             !sd.8$name == "Hylaeus illinoisensis/species A" &
                             !sd.8$name == "Coelioxys octodentata/sayi" &
                             !sd.8$name == "Andrena near_imitatrix" &
                             !sd.8$name == "Sphecodes banksii?" &
                             !sd.8$name == "Pseudopanurgus near_rudbeckiae" &
                             !sd.8$name == "Lasioglossum near_laevissimum" &
                             !sd.8$name == "Melissodes comptoides?" &
                             !sd.8$name == "Lasioglossum abanci?" &
                             !sd.8$name == "Lasioglossum near_cressonii" &
                             !sd.8$name == "Sphecodes illinoensis?"& 
                             !sd.8$name == "Lasioglossum lineatulum/smilacinae"&
                             !sd.8$name == "Lasioglossum near_cattellae"&
                             !sd.8$name == "Andrena imitatrix/morrsionella"&
                             !sd.8$name == "Nomada near_pygmaea"&
                             !sd.8$name == "Andrena Trachandrena_species"&
                             !sd.8$name == "Lasioglossum near_versatum"&
                             !sd.8$name == "Melissodes illata/subillata"&
                             !sd.8$name == "Augochlorella near_gratiosa"&
                             !sd.8$name == "Lasioglossum JG_4"&
                             !sd.8$name == "Andrena (Melandrena)"&
                             !sd.8$name == "Lasioglossum near_floridanum"&
                             !sd.8$name == "Hoplitis species"&
                             !sd.8$name == "Andrena species"&
                             !sd.8$name == "Agapostemon species"&
                             !sd.8$name == "Lasioglossum apocyni/fattigi"&
                             !sd.8$name == "LAsioglossum near_floridanum"&
                             !sd.8$name == "LAsioglossum near_rohweri"&
                             !sd.8$name == "Hylaeus aff/ill/mod"&
                             !sd.8$name == "Lasioglossum rohweri/lineatulu m"&
                             !sd.8$name == "Lasioglossum JG3"&
                             !sd.8$name == "Bombus species"&
                             !sd.8$name == "Nomada bidenate_group"&
                             !sd.8$name == "Lasioglossum near_rohweri"&
                             !sd.8$name == "Osmia cornifrons/taurus"&
                             !sd.8$name == "Lasioglossum near_oblongum"&
                             !sd.8$name == "Lasioglossum pilosum/leucocomum"&
                             !sd.8$name == "Lasioglossum planatum?"&
                             !sd.8$name == "Megachile species"&
                             !sd.8$name == "Lasioglossum species"&
                             !sd.8$name == "Pseudopanurgus species"&
                             !sd.8$name == "Sphecodes species"&
                             !sd.8$name == "Melissodes species"&
                             !sd.8$name == "sphecodes species"&
                             !sd.8$name == "Augochlorella species"&
                             !sd.8$name == "Halictus species"&
                             !sd.8$name == "Perdita species"&
                             !sd.8$name == "Colletes species"&
                             !sd.8$name == "Heriades species"&
                             !sd.8$name == "Dufourea species"&
                             !sd.8$name == "Prionyx species"&
                             !sd.8$name == "lasioglossum species"&
                             !sd.8$name == "Coelioxys species"&
                             !sd.8$name == "Svastra species"&
                             !sd.8$name == "Panurginus species"&
                             !sd.8$name == "Nomada bidentate species"&
                             !sd.8$name == "Lasioglossum species_A"&
                             !sd.8$name == "Evylaeus 1EV"&
                             !sd.8$name == "Hylaeus_species_A"&
                             !sd.8$name == "Andrena uvulariae?"&
                             
                             !sd.8$name == ""))

summary(sd.8$name, maxsum=750)
#Renombramos mispellings-----
b[["Ceratina dupla"]]
levels(sd.8$name)[levels(sd.8$name)=="ceratina dupla"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.8$name)[levels(sd.8$name)=="ceratina calcarata"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.8$name)[levels(sd.8$name)=="Ceratina calcarata/dupla"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.8$name)[levels(sd.8$name)=="Andrena morrisonella/imitatrix"]<-"Lasioglossum imitatrix/morrisonella"
levels(sd.8$name)[levels(sd.8$name)=="Augochloropsis metallica_metallica"]<-"Augochloropsis metallica"
levels(sd.8$name)[levels(sd.8$name)=="megachile brevis"]<-"Megachile brevis"
levels(sd.8$name)[levels(sd.8$name)=="Lasioglossum Gotham"]<-"Lasioglossum gotham"
levels(sd.8$name)[levels(sd.8$name)=="Lasioglossum cornifrons"]<-"Osmia cornifrons"
levels(sd.8$name)[levels(sd.8$name)=="hylaeus ornatus"]<-"Hylaeus ornatus"
levels(sd.8$name)[levels(sd.8$name)=="hylaeus affinis/modestus"]<-"Hylaeus affinis/modestus"
levels(sd.8$name)[levels(sd.8$name)=="Halictus poeyi/liugatus"]<-"Halictus ligatus/poeyi"
levels(sd.8$name)[levels(sd.8$name)=="Halictus Ligatus/poeyi"]<-"Halictus ligatus/poeyi"
levels(sd.8$name)[levels(sd.8$name)=="Colletes  inaequalis"]<-"Colletes inaequalis"
levels(sd.8$name)[levels(sd.8$name)=="Ceratina Strenua"]<-"Ceratina strenua"
levels(sd.8$name)[levels(sd.8$name)=="Ceratina clacarata"]<-"Ceratina calcarata/dupla/mikmaqi"
levels(sd.8$name)[levels(sd.8$name)=="Augochloropsis mettalica"]<-"Augochloropsis metallica"
levels(sd.8$name)[levels(sd.8$name)=="Augochloropsis Metallica"]<-"Augochloropsis metallica"
levels(sd.8$name)[levels(sd.8$name)=="apis mellifera"]<-"Apis mellifera"
levels(sd.8$name)[levels(sd.8$name)=="Andrena miserablis"]<-"Andrena miserabilis"
b[["Osmia cornifrons"]]
levels(sd.8$name)[levels(sd.8$name)=="Osmia corniforns"]<-"Osmia cornifrons"
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus modestus/affinis"]<-"Hylaeus affinis/modestus"
b[["Sphecodes atlantis"]]
b[["Sphecodes cressonii"]]
b[["Sphecodes atlantis/cressonii"]]
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes atlantis"]<-"Sphecodes atlantis/cressonii"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes cressonii"]<-"Sphecodes atlantis/cressonii"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes cressonii/atlantis"]<-"Sphecodes atlantis/cressonii"
b[["Hylaeus ornatus_black"]]
b[["Hylaeus black-ornatus"]]
b[["Hylaeus black_ornatus"]]
b[["Hylaeus ornatus"]]
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus ornatus_black"]<-"Hylaeus ornatus"
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus black-ornatus"]<-"Hylaeus ornatus"
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus black_ornatus"]<-"Hylaeus ornatus"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes cressonii/atlantis"]<-"Sphecodes atlantis/cressonii"
b[["Augochloropsis metallica"]]
b[["Ceratina calcarata"]]
b[["Ceratina strenua"]]
b[["Andrena miserabilis"]]
b[["Halictus ligatus/poeyi"]]
b[["Colletes inaequalis"]]
b[["Hylaeus ornatus_black"]]
b[["Hylaeus black-ornatus"]]
b[["Hylaeus black_ornatus"]]
#Eliminamos singletones mal escritos
sd.9<-sd.8

sd.9<-subset(sd.9, subset=(!sd.9$name == "" & !sd.9$name == "??" & 
                             !sd.9$name == "Triepeolus species_101" & 
                             !sd.9$name == "Sphecodes stygius?" & 
                             !sd.9$name == "Sphecodes galerus?" &
                             !sd.9$name == "Sphecodes cressonii?" &
                             !sd.9$name == "Sphecodes banksi" &
                             !sd.9$name == "Sphecodes banksii" &
                             !sd.9$name == "Sphecodes antennariae?" &
                             !sd.9$name == "Sphecodes atlantis banksii" &
                             !sd.9$name == "Pseudopanurgus near_passiflorae" &
                             !sd.9$name == "Palmodes near_collinsiae" &
                             !sd.9$name == "Osmia near_composita" &
                             !sd.9$name == "Osmia near_distincta" &
                             !sd.9$name == "Osmia georgica?" &
                             !sd.9$name == "Nomada townesi?" &
                             !sd.9$name == "Nomada sayi/illinoense" &
                             !sd.9$name == "Nomada red-haired" &
                             !sd.9$name == "Nomada near_sayi" &
                             !sd.9$name == "Nomada near_valida" &
                             !sd.9$name == "Nomada near_depressa" &
                             !sd.9$name == "Nomada H" &
                             !sd.9$name == "Nomada lehighensis?" &
                             !sd.9$name == "Nomada detrita?" &
                             !sd.9$name == "Nomada composita?" &
                             !sd.9$name == "Nomada crudelis?" &
                             !sd.9$name == "Melissodes trinodis?" &
                             !sd.9$name == "Nomada banksi" &
                             !sd.9$name == "Melissodes near_communis" &
                             !sd.9$name == "Melissodes near_pilleata" &
                             !sd.9$name == "Melissodes near_bidentis" &
                             !sd.9$name == "Melissodes near_boltoniae" &
                             !sd.9$name == "Melissodes boltoniae?" &
                             !sd.9$name == "Megachile brevis/mendica" &
                             !sd.9$name == "Lasioglossum timothyi/smilacinae" &
                             !sd.9$name == "Bombus impatiens" &
                             !sd.9$name == "Lasioglossum Species" &
                             !sd.9$name == "Lasioglossum swenki" &
                             !sd.9$name == "Lasioglossum sagax" &
                             !sd.9$name == "Lasioglossum smilacinae?" &
                             !sd.9$name == "Lasioglossum puteulanum?" &
                             !sd.9$name == "Lasioglossum near_smilacinae" &
                             !sd.9$name == "Lasioglossum near_tegulare" &
                             !sd.9$name == "Lasioglossum near_paradmirandum" &
                             !sd.9$name == "Lasioglossum near_nymphale" &
                             !sd.9$name == "Lasioglossum near_katherinae" &
                             !sd.9$name == "Lasioglossum near_lineatuium" &
                             !sd.9$name == "Lasioglossum near_ephialtum" &
                             !sd.9$name == "Lasioglossum near_foxii" &
                             !sd.9$name == "Lasioglossum near puteulanum" &
                             !sd.9$name == "Lasioglossum near_callidum" &
                             !sd.9$name == "Lasioglossum mutant_cf_versatum" &
                             !sd.9$name == "Lasioglossum ligatus/poeyi" &
                             !sd.9$name == "Lasioglossum JG4" &
                             !sd.9$name == "Lasioglossum illinoense?" &
                             !sd.9$name == "Lasioglossum foxii?" &
                             !sd.9$name == "Lasioglossum ephialtum?" &
                             !sd.9$name == "Lasioglossum cf_geogeickworti" &
                             !sd.9$name == "Lasioglossum blue_rohweri" &
                             !sd.9$name == "Hylaeus ornatus/confluens" &
                             !sd.9$name == "Hylaeus punctatus/hyalinatus" &
                             !sd.9$name == "Hylaeus near_verticalis" &
                             !sd.9$name == "Hylaeus ornatus/confluens" &
                             !sd.9$name == "Hylaeus punctatus/hyalinatus" &
                             !sd.9$name == "Hylaeeus ornatus/confluens" &
                             !sd.9$name == "Hylaeus punctatus/hyalinatus" &
                             !sd.9$name == "Hylaeus near_verticalis" &
                             !sd.9$name == "Hylaeus near_mesillae" &
                             !sd.9$name == "Hylaeus near_nelumbonis" &
                             !sd.9$name == "Hylaeus near_confluens" &
                             !sd.9$name == "Hylaeus near_floridanus" &
                             !sd.9$name == "Heriades leavitti/variolosus" &
                             !sd.9$name == "Heriades levilti/variolosa" &
                             !sd.9$name == "Halictus poeyi/liugatus" &
                             !sd.9$name == "Heriades leavitti//variolosa" &
                             !sd.9$name == "Halictus affinis/modestus" &
                             !sd.9$name == "Eucera dubitata/rosae" &
                             !sd.9$name == "Epeolus near_pusillus" &
                             !sd.9$name == "Coelioxys moesta/porterae" &
                             !sd.9$name == "Coelioxys near_octodentata" &
                             !sd.9$name == "Ceratina near_mikmaqi" &
                             !sd.9$name == "Ceratina deformed_calcarata" &
                             !sd.9$name == "Coelioxys near_octodentata" &
                             !sd.9$name == "Coelioxys moesta/porterae" &
                             !sd.9$name == "Ceratina near_mikmaqi" &
                             !sd.9$name == "Ceratina deformed_calcarata" &
                             !sd.9$name == "Bombus vagans/perplexus" &
                             !sd.9$name == "Bombus vagans/sandersoni" &
                             !sd.9$name == "Bombus sandersoni/vagans" &
                             !sd.9$name == "Bombus vagans?" &
                             !sd.9$name == "Bombus magans" &
                             !sd.9$name == "Augochloropsis near metellica" &
                             !sd.9$name == "Andrena w-scripta" &
                             !sd.9$name == "Andrena w-scripta?" &
                             !sd.9$name == "Andrena sp_Tracandrena" &
                             !sd.9$name == "Andrena tridens/erythronii" &
                             !sd.9$name == "Andrena sigmundi?" &
                             !sd.9$name == "Andrena near_simplex" &
                             !sd.9$name == "Andrena personata?" &
                             !sd.9$name == "Andrena near_miserabilis" &
                             !sd.9$name == "Andrena near_morrisonella" &
                             !sd.9$name == "Andrena near_ilicis" &
                             !sd.9$name == "Andrena near_macoupinensis" &
                             !sd.9$name == "Andrena near_barbilabris" &
                             !sd.9$name == "Andrena near_cressonii" &
                             !sd.9$name == "Andrena near forbesii" &
                             !sd.9$name == "Andrena near quintilis" &
                             !sd.9$name == "Andrena naonii" &
                             !sd.9$name == "Andrena naosnii" &
                             !sd.9$name == "Andrena melandrena_sp" &
                             !sd.9$name == "Andrena macra?" &
                             !sd.9$name == "Andrena geranii/nigrae" &
                             !sd.9$name == "Andrena cornelli?" &
                             !sd.9$name == "Osmia speices" &
                             !sd.9$name == "Sphecodes atlantis/banksii" &
                             !sd.9$name == "Lasioglossum near_quebecense" &
                             !sd.9$name == "Lasioglossum near_nigroviride" &
                             !sd.9$name == "Lasioglossum Gotham" &
                             !sd.9$name == "Lasioglossum creberrimum?" &
                             !sd.9$name == "lasioglossum Species" &
                             !sd.9$name == "Lasioglossum taylorae?" &
                             !sd.9$name == "Nomada Superba" &
                             !sd.9$name == "Lasioglossum calcarata" &
                             !sd.9$name == "Lasioglossum cf_georgeickworti" &
                             !sd.9$name == "Ceratina weird" &
                             !sd.9$name == "augochlorella aurata" &
                             !sd.9$name == "Andrena morrisonella?" &
                             !sd.9$name == "Andrena banski" &
                             !sd.9$name == "Andrena banksii" &
                             !sd.9$name == "Andrena imitatrive/morrisonella" &
                             !sd.9$name == "Lasioglossum lepidii/puteulanum" &
                             !sd.9$name == "Hylaeus black-ornatus" &
                             !sd.9$name == ""))
summary(sd.9$name, maxsum=640)                             
#Podemos usar el comando droplevels() pero nos desordena al hacer summary()
#NORUN sd.x<-droplevels(sd.9)

# Creamos una columna con solo los géneros--------
library(reshape)
library(vegan)
library(Hmisc)


m <- length(sd.9$name)
sd.genus <- character(m)
for (i in 1:m) {sd.genus[i] <- first.word(sd.9$name[i])}

sd.9 <- cbind(sd.9, sd.genus)  # sd.6 with correct sd.genus added on
names(sd.9)
#Renombramos columnas una vez más
sd.10<-sd.9
names(sd.10)[which(names(sd.10)=="sd.genus")]="Genus"
names(sd.10)[which(names(sd.10)=="name")]="Species"
##


##
#Ya tenemos aislados los habitats, ahora retiramos las columnas que no nos interesan de 
#nuestro dataframe


names(sd.10)
sd.11 <- subset(sd.10, select = c("Genus", "Species","id","latitude","longitude","sex","Speciesnotes","IDs1","IDs","ip","accuracy","elevation","country","state","county","city","site","position","method","bowl.num","bowl.size","bowl.col","bowl.fill","habitat","field_note","year1","year2","month1","month2","day1","day2","hours1","hours2","min1","min2"))
head(sd.11)


names(sd.11)[which(names(sd.11)=="Species")]="gen_sp"
names(sd.11)[which(names(sd.11)=="cover2")]="habitat.extracted"





#Creamos un identificador individual para cada site####
sd.12<-unique(sd.11[,c("latitude","longitude")])
nrow(sd.11)
nrow(sd.12)
IDsite<-seq(1,nrow(sd.12))
IDsite<-paste(c("SAMD"),1:nrow(sd.12))
sd.12<-cbind(sd.12,IDsite)
sd.13<-merge(sd.12,sd.11)
summary(sd.13$IDsite)
View(sd.13)
nrow(sd.13)
#####

#Tras unas anotaciones de Sam tenemos que hacer estos cambios
names(sd.13)[which(names(sd.13)=="id")]="COLLECTIONdb"
#Tomamos la IDsite (la que hemos creado individual para cada sitio) como id para extraer habitat luego
names(sd.13)[which(names(sd.13)=="IDsite")]="id"
names(sd.13)
View(sd.13)
#Preparamos unos datos para extraer habitats
write.csv(sd.13,"Datos.csv")
View(sd.13)
#Extraemos información espacial con capas raster del landcover del NLCD----
###Extracción de datos espaciales para los datos hasta 2006, usando la capa de 2001
setwd("~/Desktop/Tesis/R/habpref full data/")
getwd()
Datos<-read.csv("~/Desktop/Tesis/R/habpref full data/Datos.csv")
View(Datos)
summary(Datos$id)
Datos<-subset(Datos, subset=(Datos$year2<2006))
Datos<-subset(Datos, subset=(duplicated(Datos$id)==FALSE))
nrow(Datos)
write.csv(Datos,"Datos.csv")

#Para 2001 esta función tienen un cambio específico, dentro de NLCD data atributes
# al dar los cover names para este año la línea es 
#cover.names <- NLCD@data@attributes[[1]]$Land.Cover.Class[num.codes + 1]
#para 2006 la línea es 
# cover.names <- NLCD@data@attributes[[1]]$NLCD.2006.Land.Cover.Class[num.codes + 1]
year = 2001
extract_cover <- function(year,
                          point_d = "Datos.csv",  
                          data_dir="NLCD_data",
                          write_dir="extracted"){
  require(raster)
  require(rgdal)
  require(stringr)
  
  # load cover data 
  filename <- paste(data_dir, "/nlcd_",
                    year,
                    "_landcover_2011_edition_2014_10_10.img",
                    sep="")
  NLCD <- raster(filename)
  
  # load site data
  Datos <- read.csv(point_d, header=T, sep=",") #Si falla, usar como sep "," o ";"
  #según tus datos
  head(Datos)
  coords <- Datos[, c("longitude", "latitude")]  
  #convert lat/lon to appropriate projection
  names(coords) <- c("x", "y")
  coordinates(coords) <- ~x + y
  proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  crs_args <- NLCD@crs@projargs
  Datos_transformed <- spTransform(coords, CRS(crs_args))
  
  #extract land cover data for each point, given buffer size
  Landcover <- extract(NLCD, Datos_transformed)
  
  # summarize each site's data by proportion of each cover type
  summ <- lapply(Landcover, function(x){
    prop.table(table(x))
  }
  )
  
  # generate land cover number to name conversions
  num.codes <- unique(unlist(Landcover))
  cover.names <- NLCD@data@attributes[[1]]$Land.Cover.Class[num.codes + 1]
  levels(cover.names)[1] <- NA # first level is ""
  conversions <- data.frame(num.codes, cover.names)
  conversions <- na.omit(conversions)
  conversions <- conversions[order(conversions$num.codes),]
  
  # convert to data frame
  mydf <- data.frame(id = rep(Datos$id, lapply(summ, length)),
                     cover = names(unlist(summ)),
                     percent = unlist(summ)
  )
  
  # create cover name column
  mydf$cover2 <- mydf$cover
  levels(mydf$cover2) <- conversions$cover.names
  
  # write output
  out_name <- paste(write_dir, "/",
                    year, "_cover_", 
                    "_m_buffer.csv", sep="")
  write.csv(mydf, out_name)
}

#Aquí ejecutamos la función que hemos creado, esto crea en el directorio un archivo

extract_cover(year = 2001, point_d = "Datos.csv", write_dir = "extracted")
#Con buffer----

year = 2001
buffer = 1000
extract_cover <- function(year, buffer,
                          point_d = "Datos.csv",  
                          data_dir="NLCD_data",
                          write_dir="extracted"){
  require(raster)
  require(rgdal)
  require(stringr)
  
  # load cover data 
  filename <- paste(data_dir, "/nlcd_",
                    year,
                    "_landcover_2011_edition_2014_10_10.img",
                    sep="")
  NLCD <- raster(filename)
  
  # load site data
  Datos <- read.csv(point_d, header=T, sep=",") #Si falla, Datos a cambiar el sep por ","
  head(Datos)
  coords <- Datos[, c("longitude", "latitude")]  
  #convert lat/lon to appropriate projection
  names(coords) <- c("x", "y")
  coordinates(coords) <- ~x + y
  proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  crs_args <- NLCD@crs@projargs
  Datos_transformed <- spTransform(coords, CRS(crs_args))
  
  #extract land cover data for each point, given buffer size
  Landcover <- extract(NLCD, Datos_transformed, buffer=buffer)
  
  # summarize each site's data by proportion of each cover type
  summ <- lapply(Landcover, function(x){
    prop.table(table(x))
  }
  )
  
  # generate land cover number to name conversions
  num.codes <- unique(unlist(Landcover))
  cover.names <- NLCD@data@attributes[[1]]$Land.Cover.Class[num.codes + 1]
  levels(cover.names)[1] <- NA # first level is ""
  conversions <- data.frame(num.codes, cover.names)
  conversions <- na.omit(conversions)
  conversions <- conversions[order(conversions$num.codes),]
  
  # convert to data frame
  mydf <- data.frame(id = rep(Datos$id, lapply(summ, length)),
                     cover = names(unlist(summ)),
                     percent = unlist(summ)
  )
  
  # create cover name column
  mydf$cover2 <- mydf$cover
  levels(mydf$cover2) <- conversions$cover.names
  
  # write output
  out_name <- paste(write_dir, "/",
                    year, "_cover_", buffer,
                    "_m_buffer.csv", sep="")
  write.csv(mydf, out_name)
}

#Aquí ejecutamos la función que hemos creado
extract_cover(year = 2001, buffer= 1000, point_d = "Datos.csv", write_dir = "extracted")

#
sd.13
sd.2001<-subset(sd.13, subset=(sd.13$year2<2006))
str(sd.2001$year2)
View(sd.2001)
##Le importamos los datos de cobertura a los datos de <2006#########

#Importamos los datos de cobertura------
cover <- read.csv("~/Desktop/Tesis/R/habpref full data/extracted/2001cover.csv")
nrow(cover)
id<-cover$id
id<-as.data.frame(id)
cover2<-cover$cover2
cover2<-as.data.frame(cover2)
coverr<-cbind(id,cover2)
coverr<-subset(coverr, subset = (duplicated(coverr$id)==FALSE))
head(coverr)
summary(coverr, maxsum = 15) 

##Que coincida la columna ID
sd.m<-merge(sd.2001,coverr)
View(sd.m)
names(sd.m)[which(names(sd.m)=="cover2")]="habitat.extracted"
sd.uno<-sd.m
####
###Extracción de datos espaciales para 2006 a 2010 usamos el NLCD de 2006----
setwd("~/Desktop/Tesis/R/habpref full data/")
getwd()
sd.13
write.csv(sd.13,"Datos.csv")
Datos<-read.csv("~/Desktop/Tesis/R/habpref full data/Datos.csv")

View(Datos$id)
summary(Datos$id)
Datos<-subset(Datos, subset=(duplicated(Datos$id)==FALSE))
Datos<-subset(Datos, subset=(year2>2005 & year2<2011))
summary(Datos$id)

write.csv(Datos,"Datos.csv")
View(Datos)

year = 2006
extract_cover <- function(year,
                          point_d = "Datos.csv",  
                          data_dir="NLCD_data",
                          write_dir="extracted"){
  require(raster)
  require(rgdal)
  require(stringr)
  
  # load cover data 
  filename <- paste(data_dir, "/nlcd_",
                    year,
                    "_landcover_2011_edition_2014_10_10.img",
                    sep="")
  NLCD <- raster(filename)
  
  # load site data
  Datos <- read.csv(point_d, header=T, sep=",") #Si falla, usar como sep "," o ";"
  #según tus datos
  head(Datos)
  View(Datos)
  coords <- Datos[, c("longitude", "latitude")]  
  #convert lat/lon to appropriate projection
  names(coords) <- c("x", "y")
  coordinates(coords) <- ~x + y
  proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  crs_args <- NLCD@crs@projargs
  Datos_transformed <- spTransform(coords, CRS(crs_args))
  
  #extract land cover data for each point, given buffer size
  Landcover <- extract(NLCD, Datos_transformed)
  
  # summarize each site's data by proportion of each cover type
  summ <- lapply(Landcover, function(x){
    prop.table(table(x))
  }
  )
  
  # generate land cover number to name conversions
  num.codes <- unique(unlist(Landcover))
  cover.names <- NLCD@data@attributes[[1]]$NLCD.2006.Land.Cover.Class[num.codes + 1]
  levels(cover.names)[1] <- NA # first level is ""
  conversions <- data.frame(num.codes, cover.names)
  conversions <- na.omit(conversions)
  conversions <- conversions[order(conversions$num.codes),]
  
  # convert to data frame
  mydf <- data.frame(id = rep(Datos$id, lapply(summ, length)),
                     cover = names(unlist(summ)),
                     percent = unlist(summ)
  )
  
  # create cover name column
  mydf$cover2 <- mydf$cover
  levels(mydf$cover2) <- conversions$cover.names
  
  # write output
  out_name <- paste(write_dir, "/",
                    year, "_cover_", 
                    "_m_buffer.csv", sep="")
  write.csv(mydf, out_name)
}

#Aquí ejecutamos la función que hemos creado, esto crea en el directorio un archivo

extract_cover(year = 2006, point_d = "Datos.csv", write_dir = "extracted")

##Con buffer-----
year = 2006
buffer = 1000
extract_cover <- function(year, buffer,
                          point_d = "Datos.csv",  
                          data_dir="NLCD_data",
                          write_dir="extracted"){
  require(raster)
  require(rgdal)
  require(stringr)
  
  # load cover data 
  filename <- paste(data_dir, "/nlcd_",
                    year,
                    "_landcover_2011_edition_2014_10_10.img",
                    sep="")
  NLCD <- raster(filename)
  
  # load site data
  Datos <- read.csv(point_d, header=T, sep=",") #Si falla, Datos a cambiar el sep por ","
  head(Datos)
  coords <- Datos[, c("longitude", "latitude")]  
  #convert lat/lon to appropriate projection
  names(coords) <- c("x", "y")
  coordinates(coords) <- ~x + y
  proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  crs_args <- NLCD@crs@projargs
  Datos_transformed <- spTransform(coords, CRS(crs_args))
  
  #extract land cover data for each point, given buffer size
  Landcover <- extract(NLCD, Datos_transformed, buffer=buffer)
  
  # summarize each site's data by proportion of each cover type
  summ <- lapply(Landcover, function(x){
    prop.table(table(x))
  }
  )
  
  # generate land cover number to name conversions
  num.codes <- unique(unlist(Landcover))
  cover.names <- NLCD@data@attributes[[1]]$NLCD.2006.Land.Cover.Class[num.codes + 1]
  levels(cover.names)[1] <- NA # first level is ""
  conversions <- data.frame(num.codes, cover.names)
  conversions <- na.omit(conversions)
  conversions <- conversions[order(conversions$num.codes),]
  
  # convert to data frame
  mydf <- data.frame(id = rep(Datos$id, lapply(summ, length)),
                     cover = names(unlist(summ)),
                     percent = unlist(summ)
  )
  
  # create cover name column
  mydf$cover2 <- mydf$cover
  levels(mydf$cover2) <- conversions$cover.names
  
  # write output
  out_name <- paste(write_dir, "/",
                    year, "_cover_", buffer,
                    "_m_buffer.csv", sep="")
  write.csv(mydf, out_name)
}

#Aquí ejecutamos la función que hemos creado
extract_cover(year = 2006, buffer= 1000, point_d = "Datos.csv", write_dir = "extracted")

#Añadimos el landcover extraido a las muestras de 2006-2010
sd.13
sd.2006<-subset(sd.13, subset=(year2>2005 & year2<2011))
cover <- read.csv("~/Desktop/Tesis/R/habpref full data/extracted/2006cover.csv")
nrow(cover)
id<-cover$id
id<-as.data.frame(id)
cover2<-cover$cover2
cover2<-as.data.frame(cover2)
coverr<-cbind(id,cover2)
coverr<-subset(coverr, subset = (duplicated(coverr$id)==FALSE))
head(coverr)
summary(coverr, maxsum = 15) 

##Que coincida la columna ID
sd.n<-merge(sd.2006,coverr)
View(sd.n)
names(sd.n)[which(names(sd.n)=="cover2")]="habitat.extracted"
sd.uno
sd.dos<-sd.n

#Extraemos el landcover para las muestras entre 2011 - 2015-----
setwd("~/Desktop/Tesis/R/habpref full data/")
getwd()
sd.13
write.csv(sd.13,"Datos.csv")
Datos<-read.csv("~/Desktop/Tesis/R/habpref full data/Datos.csv")


summary(Datos$id)
Datos<-subset(Datos, subset=(duplicated(Datos$id)==FALSE))
Datos<-subset(Datos, subset=(year2>2011))
summary(Datos$id)
nrow(Datos)
write.csv(Datos,"Datos.csv")
View(Datos)

year = 2011
extract_cover <- function(year,
                          point_d = "Datos.csv",  
                          data_dir="NLCD_data",
                          write_dir="extracted"){
  require(raster)
  require(rgdal)
  require(stringr)
  
  # load cover data 
  filename <- paste(data_dir, "/nlcd_",
                    year,
                    "_landcover_2011_edition_2014_10_10.img",
                    sep="")
  NLCD <- raster(filename)
  
  # load site data
  Datos <- read.csv(point_d, header=T, sep=",") #Si falla, usar como sep "," o ";"
  #según tus datos
  head(Datos)
  coords <- Datos[, c("longitude", "latitude")]  
  #convert lat/lon to appropriate projection
  names(coords) <- c("x", "y")
  coordinates(coords) <- ~x + y
  proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  crs_args <- NLCD@crs@projargs
  Datos_transformed <- spTransform(coords, CRS(crs_args))
  
  #extract land cover data for each point, given buffer size
  Landcover <- extract(NLCD, Datos_transformed)
  
  # summarize each site's data by proportion of each cover type
  summ <- lapply(Landcover, function(x){
    prop.table(table(x))
  }
  )
  
  # generate land cover number to name conversions
  num.codes <- unique(unlist(Landcover))
  cover.names <- NLCD@data@attributes[[1]]$NLCD.2011.Land.Cover.Class[num.codes + 1]
  levels(cover.names)[1] <- NA # first level is ""
  conversions <- data.frame(num.codes, cover.names)
  conversions <- na.omit(conversions)
  conversions <- conversions[order(conversions$num.codes),]
  
  # convert to data frame
  mydf <- data.frame(id = rep(Datos$id, lapply(summ, length)),
                     cover = names(unlist(summ)),
                     percent = unlist(summ)
  )
  
  # create cover name column
  mydf$cover2 <- mydf$cover
  levels(mydf$cover2) <- conversions$cover.names
  
  # write output
  out_name <- paste(write_dir, "/",
                    year, "_cover_", 
                    "_m_buffer.csv", sep="")
  write.csv(mydf, out_name)
}

#Aquí ejecutamos la función que hemos creado, esto crea en el directorio un archivo

extract_cover(year = 2011, point_d = "Datos.csv", write_dir = "extracted")

##Con buffer-----
year = 2006
buffer = 1000
extract_cover <- function(year, buffer,
                          point_d = "Datos.csv",  
                          data_dir="NLCD_data",
                          write_dir="extracted"){
  require(raster)
  require(rgdal)
  require(stringr)
  
  # load cover data 
  filename <- paste(data_dir, "/nlcd_",
                    year,
                    "_landcover_2011_edition_2014_10_10.img",
                    sep="")
  NLCD <- raster(filename)
  
  # load site data
  Datos <- read.csv(point_d, header=T, sep=",") #Si falla, Datos a cambiar el sep por ","
  head(Datos)
  coords <- Datos[, c("longitude", "latitude")]  
  #convert lat/lon to appropriate projection
  names(coords) <- c("x", "y")
  coordinates(coords) <- ~x + y
  proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  crs_args <- NLCD@crs@projargs
  Datos_transformed <- spTransform(coords, CRS(crs_args))
  
  #extract land cover data for each point, given buffer size
  Landcover <- extract(NLCD, Datos_transformed, buffer=buffer)
  
  # summarize each site's data by proportion of each cover type
  summ <- lapply(Landcover, function(x){
    prop.table(table(x))
  }
  )
  
  # generate land cover number to name conversions
  num.codes <- unique(unlist(Landcover))
  cover.names <- NLCD@data@attributes[[1]]$NLCD.2011.Land.Cover.Class[num.codes + 1]
  levels(cover.names)[1] <- NA # first level is ""
  conversions <- data.frame(num.codes, cover.names)
  conversions <- na.omit(conversions)
  conversions <- conversions[order(conversions$num.codes),]
  
  # convert to data frame
  mydf <- data.frame(id = rep(Datos$id, lapply(summ, length)),
                     cover = names(unlist(summ)),
                     percent = unlist(summ)
  )
  
  # create cover name column
  mydf$cover2 <- mydf$cover
  levels(mydf$cover2) <- conversions$cover.names
  
  # write output
  out_name <- paste(write_dir, "/",
                    year, "_cover_", buffer,
                    "_m_buffer.csv", sep="")
  write.csv(mydf, out_name)
}

#Aquí ejecutamos la función que hemos creado
extract_cover(year = 2011, buffer= 1000, point_d = "Datos.csv", write_dir = "extracted")

#Añadimos el landcover extraido a las muestras de 20
sd.13
sd.2011<-subset(sd.13, subset=(year2>2011))
cover <- read.csv("~/Desktop/Tesis/R/habpref full data/extracted/2011cover.csv")
nrow(cover)
id<-cover$id
id<-as.data.frame(id)
cover2<-cover$cover2
cover2<-as.data.frame(cover2)
coverr<-cbind(id,cover2)
coverr<-subset(coverr, subset = (duplicated(coverr$id)==FALSE))
head(coverr)
summary(coverr, maxsum = 15) 

##Que coincida la columna ID
sd.z<-merge(sd.2011,coverr)
View(sd.n)
names(sd.z)[which(names(sd.z)=="cover2")]="habitat.extracted"
sd.uno
sd.tres<-sd.z
sd.14<-rbind(sd.uno,sd.dos,sd.tres)
####
View(sd.14)
str(sd.14)
summary(sd.14$habitat.extracted)

lol<- unique(sd.14[,c("latitude", "longitude")])
nrow(lol)

sd.14
#Tenemos un problema con Open Water, que incluye tanto Polinizadores de costa como
#de interior
sd.15<-sd.14

sd.15<-subset(sd.15, subset = (sd.15$habitat.extracted== "Open Water"))
View(sd.15)
sd.16<-sd.15
sd.16<-subset(sd.16, subset= (sd.16$longitude<(-76.61)))
#Vemos en un mapa dónde se encuentran estas observaciones
locs <- subset(sd.15, select = c("country", "latitude", "longitude"))
#Comprobamos que tenemos los datos que queremos
head(locs) 
#Descartamos los datos con errores en las coordenadas
locs <- subset(locs, locs$latitude < 90)
#Comprobamos que no haya NA ni datos extra??os
View(locs)
#Tenemos un dataframe con coordinadas, podemos plasmarlas ahora espacialmente
coordinates(locs) <- c("longitude", "latitude")  
plot(locs)
#Definimos la proyecci??n geogr??fica, hay que ponerle un PROJ4 espec??fico a cada zona
#o algo as?? nuse nuse Xdxdxdzd
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)


#Cambiamos el color y tamaño de los puntos
plot(locs, pch = 4, col = "red")
#Cargamos library que contiene muchos tipos de mapas, por ejemplo estos
library(rworldmap)
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)

lemap <- gmap(locs, type = "satellite")
locs.merc <- Mercator(locs)
plot(lemap)
points(locs.merc, pch = 4, col = "red")
#Estos los removemos, están etiquetados como Open Water y están en interior
sd.15<-subset(sd.15, subset= (!sd.15$longitude<(-76.61)))
#Cambiamos el nombre de "Open Water" por "Coastal"
sd.14<-subset(sd.14, subset = (!sd.14$habitat.extracted== "Open Water"))
summary(sd.14$habitat.extracted)
summary(sd.17$habitat.extracted)
sd.17<-rbind(sd.14,sd.15)
levels(sd.17$habitat.extracted)[levels(sd.17$habitat.extracted)=="Open Water"]<-"Coastal"

#Con estos datos ya podemos trabajar
getwd()
write.csv(sd.17,"Datos1.csv")

