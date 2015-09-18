   #NO RUN----- 
#Cargamos los datos originales y les adjuntamos los habitats extraidos
DatosRaw<-read.csv("~/Desktop/Tesis/R/habpref/DatosRaw.csv")
habitats <- read.csv("~/Desktop/Tesis/R/habpref/extracted/habitats sin buffer.csv")
names(habitats)<-c("X","ids","cover","percent","cover2")
habitatss<-habitats$ids
habitatss<-as.data.frame(habitatss)
habitat.extracted<-habitats$cover2
hab<-cbind(habitatss,habitat.extracted)
names(hab)<-c("ids","habitat.extracted")
habs<-subset(hab,subset=(duplicated(hab$ids)==FALSE))
Datos2<-merge(DatosRaw,habs)
#Filtramos los datos para que solo nos queden las especies bien definidas como tal
Datos2 <- read.csv("~/Desktop/Tesis/R/habpref/Datos2.csv")
Datos3<-<-subset(Datos2, subset=(!Datos2$gen_sp == "Agapostemon species" & !Datos2$gen_sp == "Andrena species" & !Datos2$gen_sp == "Anthidiellum species" & !Datos2$gen_sp == "Augochlorella species" & !Datos2$gen_sp == "Bombus species" & !Datos2$gen_sp == "Calliopsis species" & !Datos2$gen_sp == "Ceratina species" & !Datos2$gen_sp == "Ceratina species" & !Datos2$gen_sp == "Coelioxys species"  & !Datos2$gen_sp == "Epeolus species" & !Datos2$gen_sp == "Halictus species" & !Datos2$gen_sp == "Heriades species" & !Datos2$gen_sp == "Eucera species" & !Datos2$gen_sp == "Hylaeus species" & !Datos2$gen_sp == "Lasioglossum species" & !Datos2$gen_sp == "Megachile species" & !Datos2$gen_sp == "Melissodes species" & !Datos2$gen_sp == "Melissodes species" & !Datos2$gen_sp == "Nomada species" & !Datos2$gen_sp == "Osmia species" & !Datos2$gen_sp == "Perdita species" & !Datos2$gen_sp == "Pseudopanurgus species" & !Datos2$gen_sp == "Sphecodes species"))
pff<-subset(Datos3, subset=(!Datos3$gen_sp == "Ceratina calcarata/dupla" & !Datos3$gen_sp == "Halictus ligatus/poeyi" & !Datos3$gen_sp == "Hylaeus affinis/modestus" & !Datos3$gen_sp == "Lasioglossum admirandum/rohweri" & !Datos3$gen_sp == "Lasioglossum viridatum group"))
Ceratina calcarata/dupla
write.csv(pff, "Datos4.csv")
# RUN beyond this point----
Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
Datos4

#Extraemos Datos 4, que son los datos ya filtrados para que solo aparezcan especies
#bien definidas como tal
Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")



####Extracción de strength y Richness sin singletones----
#Queremos extraer el vector de fuerza y de riqueza de nuestros datos, para ello 
#remuestrearemos 400 individuos de cada uno de nuestros 15 habitats, mediante un loop
#de cien repeticiones, que al final representaremos con un boxplot

#Por rapidez identifiqué los singletones y los extraigo nombrándolos uno a uno
####Strength sin singletones----
summary(Datos4$gen_sp, maxsum=Inf)
Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena accepta" & !Datos4$gen_sp == "Andrena braccata" & !Datos4$gen_sp == "Andrena cragini" & !Datos4$gen_sp == "Andrena erythrogaster" & !Datos4$gen_sp == "Andrena integra" & !Datos4$gen_sp == "Andrena phaceliae" & !Datos4$gen_sp == "Andrena polemonii" & !Datos4$gen_sp == "Andrena salictaria" & !Datos4$gen_sp == "Andrena sigmundi"  & !Datos4$gen_sp == "Andrena w-scripta" & !Datos4$gen_sp == "Andrena zizeaformis" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))
#Comprobamos que se hayan ido los singletones
summary(Datos6$gen_sp, maxsum=Inf)
summary(Datos6$habitat.extracted)
#Ahora hacemos un loop, para extraer 400 individuos para cada habitat y calcular
#la strength 100 veces
bl100s <- NULL
library(bipartite)
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  View(jaja)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  bl100s[n] = c1[1] 
}
bl100s
#Y esto lo repetimos con todos los habitats

cc100s <- NULL
library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  cc100s[n] = c1[2] 
}
cc100s

df100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  df100s[n] = c1[3] 
}
df100s


dh100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  dh100s[n] = c1[4] 
}
dh100s

dl100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  dl100s[n] = c1[5] 
}
dl100s

dm100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  dm100s[n] = c1[6] 
}
dm100s

do100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  do100s[n] = c1[7] 
}
dl100s

eh100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  c1[1]  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  eh100s[n] = c1[8] 
}
eh100s

ef100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  ef100s[n] = c1[9] 
}
efl100s

hp100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  hp100s[n] = c1[10] 
}
hp100s


hb100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  hb100s[n] = c1[11] 
}
hb100s


mf100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  mf100s[n] = c1[12] 
}
mf100s


ow100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  ow100s[n] = c1[13] 
}
ow100s

ss100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  ss100s[n] = c1[14] 
}
ss100s


ww100s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 400), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 400), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 400), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 400), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 400), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 400), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 400), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 400), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 400), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 400), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 400), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 400), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 400), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 400), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 400), ]
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri10,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  jeje<-jeje[,-2]
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  ww100s[n] = c1[15] 
}
ww100s

#Hacemos un boxplot con los valores de strength resampleados 100 veces para cada habitat
nombres<-c("BL", "CC","DF","DH","DL","DM","DO", "EH","EF","HP","HB","MF","OW","SS","WW")
boxplot(bl100s,cc100s,df100s,dh100s,dl100s,dm100s,do100s,eh100s,ef100s,hp100s,hb100s,mf100s,ow100s,ss100s,ww100s, names = nombres)



###Richness sin singletones#####
#Es el mismo proceso que para extraer la strength, retiramos los singletones de los datos
#y hacemos otro loop para remuestrear 100 veces 400 individuos de cada habitat
Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena accepta" & !Datos4$gen_sp == "Andrena braccata" & !Datos4$gen_sp == "Andrena cragini" & !Datos4$gen_sp == "Andrena erythrogaster" & !Datos4$gen_sp == "Andrena integra" & !Datos4$gen_sp == "Andrena phaceliae" & !Datos4$gen_sp == "Andrena polemonii" & !Datos4$gen_sp == "Andrena salictaria" & !Datos4$gen_sp == "Andrena sigmundi"  & !Datos4$gen_sp == "Andrena w-scripta" & !Datos4$gen_sp == "Andrena zizeaformis" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))
summary(Datos6$habitat.extracted)

bl100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  bl100ds[n]=c1[1]
}
bl100ds
#Sacamos diversidad
bl100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  bl100ds[n]=c1[1]
}
bl100ds
cc100ds
df100ds
dh100ds
dl100ds
dm100ds
do100ds
eh100ds
ef100ds
hp100ds
hb100ds
mf100ds
ow100ds
ss100ds
ww100ds

#Sacamos diversidad
cc100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  cc100ds[n]=c1[2]
}
cc100ds

#Sacamos diversidad
df100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  df100ds[n]=c1[3]
}
df100ds

#Sacamos diversidad
dh100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  dh100ds[n]=c1[4]
}
dh100ds

#Sacamos diversidad
dl100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  dl100ds[n]=c1[5]
}
dl100ds

#Sacamos diversidad
dm100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  dm100ds[n]=c1[6]
}
dm100ds

#Sacamos diversidad
do100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  do100ds[n]=c1[7]
}
do100ds

#Sacamos diversidad
eh100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  eh100ds[n]=c1[8]
}
eh100ds

#Sacamos diversidad
ef100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  ef100ds[n]=c1[9]
}
ef100ds

#Sacamos diversidad
hp100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  hp100ds[n]=c1[10]
}
hp100ds

#Sacamos diversidad
hb100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  hb100ds[n]=c1[11]
}
hb100ds

#Sacamos diversidad
mf100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  mf100ds[n]=c1[12]
}
mf100ds


#Sacamos diversidad
ow100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  ow100ds[n]=c1[13]
}
ow100ds

#Sacamos diversidad
ss100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  ss100ds[n]=c1[14]
}
ss100ds

#Sacamos diversidad
ww100ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  
  ww100ds[n]=c1[15]
}
ww100ds

nombres<-c("BL", "CC","DF","DH","DL","DM","DO", "EH","EF","HP","HB","MF","OW","SS","WW")
boxplot(bl100ds,cc100ds,df100ds,dh100ds,dl100ds,dm100ds,do100ds,eh100ds,ef100ds,hp100ds,hb100ds,mf100ds,ow100ds,ss100ds,ww100ds, names = nombres)


####Strength de grupos kmeans, sin singletones----
#Explicar la extracción de kmeans
merger <- read.csv("~/Desktop/Tesis/R/habpref/merger.csv")
str(merger)
group<-as.factor(merger$group)
merger<-merger[,-9]
merger<-cbind(merger,group)

merger<-subset(merger, subset=(!merger$gen_sp == "Andrena accepta" & !merger$gen_sp == "Andrena braccata" & !merger$gen_sp == "Andrena cragini" & !merger$gen_sp == "Andrena erythrogaster" & !merger$gen_sp == "Andrena integra" & !merger$gen_sp == "Andrena phaceliae" & !merger$gen_sp == "Andrena polemonii" & !merger$gen_sp == "Andrena salictaria" & !merger$gen_sp == "Andrena sigmundi"  & !merger$gen_sp == "Andrena w-scripta" & !merger$gen_sp == "Andrena zizeaformis" & !merger$gen_sp == "Anthophorula micheneri" & !merger$gen_sp == "Bombus fernaldae" & !merger$gen_sp == "Bombus imitator" & !merger$gen_sp == "Cemolobus ipomoeae" & !merger$gen_sp == "Coelioxys alternata" & !merger$gen_sp == "Coelioxys dolichos" & !merger$gen_sp == "Coelioxys immaculata" & !merger$gen_sp == "Colletes brevicornis" & !merger$gen_sp == "Epeolus bifasciatus" & !merger$gen_sp == "Lasioglossum anomalum" & !merger$gen_sp == "Lasioglossum apopkense" & !merger$gen_sp == "Lasioglossum asteris" & !merger$gen_sp == "Lasioglossum curtulum" & !merger$gen_sp == "Lasioglossum floridanum" & !merger$gen_sp == "Lasioglossum michiganense" & !merger$gen_sp == "Lasioglossum pectinatum" & !merger$gen_sp == "Lasioglossum rufitarse" & !merger$gen_sp == "Lasioglossum simplex" & !merger$gen_sp == "Nomada affabilis" & !merger$gen_sp == "Nomada annulata" & !merger$gen_sp == "Nomada bethunei" & !merger$gen_sp == "Nomada composita" & !merger$gen_sp == "Nomada ovata" & !merger$gen_sp == "Nomada placida" & !merger$gen_sp == "Nomada rubicunda" & !merger$gen_sp == "Nomada valida" & !merger$gen_sp == "Osmia caerulescens" & !merger$gen_sp == "Osmia inspergens" & !merger$gen_sp == "Osmia proxima" & !merger$gen_sp == "Osmia simillima" & !merger$gen_sp == "Paranthidium jugatorium" & !merger$gen_sp == "Perdita bequaerti" & !merger$gen_sp == "Perdita gerardiae" & !merger$gen_sp == "Pseudopanurgus andrenoides" & !merger$gen_sp == "Pseudopanurgus nebrascensis" & !merger$gen_sp == "Pseudopanurgus rugosus" & !merger$gen_sp == "Sphecodes antennariae" & !merger$gen_sp == "Sphecodes aroniae" & !merger$gen_sp == "Sphecodes clematidis" & !merger$gen_sp == "Sphecodes cressonii" & !merger$gen_sp == "Sphecodes galerus" & !merger$gen_sp == "Sphecodes heraclei" & !merger$gen_sp == "Sphecodes johnsonii" & !merger$gen_sp == "Stelis nitida" & !merger$gen_sp == "Triepeolus helianthi" & !merger$gen_sp == "Triepeolus pectoralis" & !merger$gen_sp == "Triepeolus simplex"))

unos <- NULL

library(bipartite)
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  View(jejem)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  unos[n] = c1[1] 
}
unos
doss
doss <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  doss[n] = c1[2] 
}
doss
unos
tress <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  tress[n] = c1[3] 
}

cuatros <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  cuatros[n] = c1[4] 
}

cincos <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  cincos[n] = c1[5] 
}
seiss <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  seiss[n] = c1[6] 
}

sietes <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  sietes[n] = c1[7] 
}

ochos <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  ochos[n] = c1[8] 
}
nueves <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  nueves[n] = c1[9] 
}

diezs <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  diezs[n] = c1[10] 
}

onces <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  onces[n] = c1[11] 
}

doces <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  doces[n] = c1[12] 
}

treces <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  treces[n] = c1[13] 
}

catorces <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  catorces[n] = c1[14] 
}

quinces <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  quinces[n] = c1[15] 
}

dieciseiss <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  dieciseiss[n] = c1[16] 
}

diecisietes <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  diecisietes[n] = c1[17] 
}

dieciochos <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  dieciochos[n] = c1[18] 
}

diecinueves <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  diecinueves[n] = c1[19] 
}

veintes <- NULL
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  row.names(jeje)<- jiji[,1]
  jeje<-jeje[,-1]
  jejem<-apply(jeje, MARGIN=2, FUN=as.numeric)
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  c1
  
  
  c1[1]
  
  
  
  barr<-c1[1]
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Cultivated.Crops              
  #[3] Deciduous.Forest             
  #[4] Developed..High.Intensity             
  #[5] Developed..Low.Intensity    
  #[6] Developed..Medium.Intensity     
  #[7] Developed..Open.Space  
  #[8] Emergent.Herbaceuous.Wetlands        
  #[9] Evergreen.Forest
  #[10] Hay.Pasture             
  #[11] Herbaceuous                  
  #[12] Mixed.Forest                  
  #[13] Open.Water                 
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  
  veintes[n] = c1[20] 
}

####Richness de grupos kmeans, sin singletones----
merger <- read.csv("~/Desktop/Tesis/R/habpref/merger.csv")
group<-as.factor(merger$group)
str(group)
str(merger)
View(merger)
merger<-merger[,-9]
merger<-cbind(merger,group)
str(merger)


Datos6<-subset(merger, subset=(!merger$gen_sp == "Andrena accepta" & !merger$gen_sp == "Andrena braccata" & !merger$gen_sp == "Andrena cragini" & !merger$gen_sp == "Andrena erythrogaster" & !merger$gen_sp == "Andrena integra" & !merger$gen_sp == "Andrena phaceliae" & !merger$gen_sp == "Andrena polemonii" & !merger$gen_sp == "Andrena salictaria" & !merger$gen_sp == "Andrena sigmundi"  & !merger$gen_sp == "Andrena w-scripta" & !merger$gen_sp == "Andrena zizeaformis" & !merger$gen_sp == "Anthophorula micheneri" & !merger$gen_sp == "Bombus fernaldae" & !merger$gen_sp == "Bombus imitator" & !merger$gen_sp == "Cemolobus ipomoeae" & !merger$gen_sp == "Coelioxys alternata" & !merger$gen_sp == "Coelioxys dolichos" & !merger$gen_sp == "Coelioxys immaculata" & !merger$gen_sp == "Colletes brevicornis" & !merger$gen_sp == "Epeolus bifasciatus" & !merger$gen_sp == "Lasioglossum anomalum" & !merger$gen_sp == "Lasioglossum apopkense" & !merger$gen_sp == "Lasioglossum asteris" & !merger$gen_sp == "Lasioglossum curtulum" & !merger$gen_sp == "Lasioglossum floridanum" & !merger$gen_sp == "Lasioglossum michiganense" & !merger$gen_sp == "Lasioglossum pectinatum" & !merger$gen_sp == "Lasioglossum rufitarse" & !merger$gen_sp == "Lasioglossum simplex" & !merger$gen_sp == "Nomada affabilis" & !merger$gen_sp == "Nomada annulata" & !merger$gen_sp == "Nomada bethunei" & !merger$gen_sp == "Nomada composita" & !merger$gen_sp == "Nomada ovata" & !merger$gen_sp == "Nomada placida" & !merger$gen_sp == "Nomada rubicunda" & !merger$gen_sp == "Nomada valida" & !merger$gen_sp == "Osmia caerulescens" & !merger$gen_sp == "Osmia inspergens" & !merger$gen_sp == "Osmia proxima" & !merger$gen_sp == "Osmia simillima" & !merger$gen_sp == "Paranthidium jugatorium" & !merger$gen_sp == "Perdita bequaerti" & !merger$gen_sp == "Perdita gerardiae" & !merger$gen_sp == "Pseudopanurgus andrenoides" & !merger$gen_sp == "Pseudopanurgus nebrascensis" & !merger$gen_sp == "Pseudopanurgus rugosus" & !merger$gen_sp == "Sphecodes antennariae" & !merger$gen_sp == "Sphecodes aroniae" & !merger$gen_sp == "Sphecodes clematidis" & !merger$gen_sp == "Sphecodes cressonii" & !merger$gen_sp == "Sphecodes galerus" & !merger$gen_sp == "Sphecodes heraclei" & !merger$gen_sp == "Sphecodes johnsonii" & !merger$gen_sp == "Stelis nitida" & !merger$gen_sp == "Triepeolus helianthi" & !merger$gen_sp == "Triepeolus pectoralis" & !merger$gen_sp == "Triepeolus simplex"))

uno=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  View(Datos5)
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  View(meji)
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[1]
  
  uno[n]=c1[1]
}
uno

dos=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  View(Datos5)
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  View(meji)
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  dos[n]=c1[2]
}
dos

tres=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  tres[n]=c1[3]
}
tres

cuatro=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  cuatro[n]=c1[4]
}
cuatro

cinco=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  cinco[n]=c1[5]
}
cinco

seis=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  seis[n]=c1[6]
}

siete=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  siete[n]=c1[7]
}


ocho=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  ocho[n]=c1[8]
}

nueve=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  nueve[n]=c1[9]
}

diez=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  diez[n]=c1[10]
}

once=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  once[n]=c1[11]
}

doce=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  doce[n]=c1[12]
}

trece=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  trece[n]=c1[13]
}

catorce=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  catorce[n]=c1[14]
}

quince=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  quince[n]=c1[15]
}
dieciseis=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  dieciseis[n]=c1[16]
}

diecisiete=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  diecisiete[n]=c1[17]
}

dieciocho=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  dieciocho[n]=c1[18]
}
diecinueve
diecinueve=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  diecinueve[n]=c1[19]
}

veinte=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos6, subset=(Datos6$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos6, subset=(Datos6$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos6, subset=(Datos6$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos6, subset=(Datos6$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos6, subset=(Datos6$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos6, subset=(Datos6$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos6, subset=(Datos6$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos6, subset=(Datos6$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos6, subset=(Datos6$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos6, subset=(Datos6$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos6, subset=(Datos6$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos6, subset=(Datos6$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos6, subset=(Datos6$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos6, subset=(Datos6$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  tri16<-subset(Datos6, subset=(Datos6$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 400), ]
  tri17<-subset(Datos6, subset=(Datos6$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 400), ]
  tri18<-subset(Datos6, subset=(Datos6$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 400), ]
  tri19<-subset(Datos6, subset=(Datos6$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 400), ]
  tri20<-subset(Datos6, subset=(Datos6$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$group ~ Datos5$gen_sp, FUN = summary, Datos5)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  mejib<-replace(meji, meji>1,1)
  row.names(mejib)<- meji[,1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat/nº total)
  c1<-colSums(momo)
  c1<-c1[-1]
  c1[2]
  
  veinte[n]=c1[20]
}
Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")



#Boxplots----
par(mfrow=c(2,2))
nombres<-c("BL", "CC","DF","DH","DL","DM","DO", "EH","EF","HP","HB","MF","OW","SS","WW")
#strength
boxplot(bl100s,cc100s,df100s,dh100s,dl100s,dm100s,do100s,eh100s,ef100s,hp100s,hb100s,mf100s,ow100s,ss100s,ww100s, names = nombres, main="Strength without singletones", xlab="Habitats", ylab = "Strength")
#diversidad 
boxplot(bl100ds,cc100ds,df100ds,dh100ds,dl100ds,dm100ds,do100ds,eh100ds,ef100ds,hp100ds,hb100ds,mf100ds,ow100ds,ss100ds,ww100ds, names = nombres, main="Richness without singletones", xlab="Habitats", ylab = "Richness")
#strength kmeans
boxplot(unos,doss,tress,cuatros,cincos,seiss,sietes,ochos,nueves,diezs,onces,doces,treces,catorces,quinces,dieciseiss,diecisietes,dieciochos,diecinueves,veintes, main="Grouped Strength without singletones", xlab="Habitats", ylab = "Strength")
#diversidad kmeans
boxplot(uno,dos,tres,cuatro,cinco,seis,siete,ocho,nueve,diez,once,doce,trece,catorce,quince,dieciseis,diecisiete,dieciocho,diecinueve,veinte, main="Grouped Richness without singletones", xlab="Habitats", ylab = "Richness")





