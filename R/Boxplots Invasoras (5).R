Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
#Filtramos para que solo quedarnos con las especies invasoras
Datos6<-subset(Datos4, subset=(Datos4$gen_sp == "Andrena wilkella" | Datos4$gen_sp == "Anthidium manicatum" | Datos4$gen_sp == "Anthidium oblongatum" | Datos4$gen_sp == "Hylaeus leptocephalus" | Datos4$gen_sp == "Hylaeus punctatus" | Datos4$gen_sp == "Lasioglossum leucozonium" | Datos4$gen_sp == "Megachile apicalis" | Datos4$gen_sp == "Megachile concinna" | Datos4$gen_sp == "Megachile rotundata"  | Datos4$gen_sp == "Megachile sculpturalis" | Datos4$gen_sp == "Andrena" | Datos4$gen_sp == "Anthophorula micheneri" | Datos4$gen_sp == "Bombus fernaldae" | Datos4$gen_sp == "Bombus imitator" | Datos4$gen_sp == "Cemolobus ipomoeae" | Datos4$gen_sp == "Coelioxys alternata" | Datos4$gen_sp == "Coelioxys dolichos" | Datos4$gen_sp == "Coelioxys immaculata" | Datos4$gen_sp == "Colletes brevicornis" | Datos4$gen_sp == "Epeolus bifasciatus" | Datos4$gen_sp == "Lasioglossum anomalum" | Datos4$gen_sp == "Lasioglossum apopkense" | Datos4$gen_sp == "Lasioglossum asteris" | Datos4$gen_sp == "Lasioglossum curtulum" | Datos4$gen_sp == "Lasioglossum floridanum" | Datos4$gen_sp == "Lasioglossum michiganense" | Datos4$gen_sp == "Lasioglossum pectinatum" | Datos4$gen_sp == "Lasioglossum rufitarse" | Datos4$gen_sp == "Lasioglossum simplex" | Datos4$gen_sp == "Nomada affabilis" | Datos4$gen_sp == "Nomada annulata" | Datos4$gen_sp == "Nomada bethunei" | Datos4$gen_sp == "Nomada composita" | Datos4$gen_sp == "Nomada ovata" | Datos4$gen_sp == "Nomada placida" | Datos4$gen_sp == "Nomada rubicunda" | Datos4$gen_sp == "Nomada valida" | Datos4$gen_sp == "Osmia caerulescens" | Datos4$gen_sp == "Osmia inspergens" | Datos4$gen_sp == "Osmia proxima" | Datos4$gen_sp == "Osmia simillima" | Datos4$gen_sp == "Paranthidium jugatorium" | Datos4$gen_sp == "Perdita bequaerti" | Datos4$gen_sp == "Perdita gerardiae" | Datos4$gen_sp == "Pseudopanurgus andrenoides" | Datos4$gen_sp == "Pseudopanurgus nebrascensis" | Datos4$gen_sp == "Pseudopanurgus rugosus" | Datos4$gen_sp == "Sphecodes antennariae" | Datos4$gen_sp == "Sphecodes aroniae" | Datos4$gen_sp == "Sphecodes clematidis" | Datos4$gen_sp == "Sphecodes cressonii" | Datos4$gen_sp == "Sphecodes galerus" | Datos4$gen_sp == "Sphecodes heraclei" | Datos4$gen_sp == "Sphecodes johnsonii" | Datos4$gen_sp == "Stelis nitida" | Datos4$gen_sp == "Triepeolus helianthi" | Datos4$gen_sp == "Triepeolus pectoralis" | Datos4$gen_sp == "Triepeolus simplex"))
View(Datos13)

summary(Datos6$habitat.extracted)
library(bipartite)
###Strength----
#Queremos extraer el vector de fuerza y de riqueza de nuestros datos, para ello 
#remuestrearemos 10 individuos de cada uno de nuestros 15 habitats, mediante un loop
#de cien repeticiones, que al final representaremos con un boxplot


bl10i <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
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
  
  bl10i[n] = c1[1] 
}



cc10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  cc10i[n] = c1[2] 
}
bl10i
cc10i

df10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  df10i[n] = c1[3] 
}

dh10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  dh10i[n] = c1[4] 
}

dl10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  dl10i[n] = c1[5] 
}


dh10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  dh10i[n] = c1[4] 
}

dm10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  dm10i[n] = c1[6] 
}

do10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  do10i[n] = c1[7] 
}

eh10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  eh10i[n] = c1[8] 
}

ef10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  ef10i[n] = c1[9] 
}

hp10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  hp10i[n] = c1[10] 
}

hb10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  hb10i[n] = c1[11] 
}

mf10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  mf10i[n] = c1[12] 
}

ow10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  ow10i[n] = c1[13] 
}

ss10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  ss10i[n] = c1[14] 
}

ww10i <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  ww10i[n] = c1[15] 
}

##Richness-----
bl10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  bl10di[n]=c1[1]
}
bl10di
cc10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  cc10di[n]=c1[2]
}
df10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  df10di[n]=c1[3]
}
dh10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  dh10di[n]=c1[4]
}

dl10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  dl10di[n]=c1[5]
}
dm10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  dm10di[n]=c1[6]
}
do10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  do10di[n]=c1[7]
}
eh10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  eh10di[n]=c1[8]
}

ef10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  ef10di[n]=c1[9]
}
hp10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  hp10di[n]=c1[10]
}
hb10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  hb10di[n]=c1[11]
}
mf10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  mf10di[n]=c1[12]
}
ow10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  ow10di[n]=c1[13]
}
ss10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  ss10di[n]=c1[14]
}
ww10di=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  ww10di[n]=c1[15]
}
#Retirando habitats-----
#Strength----

summary(Datos6$habitat.extracted)

bl10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  bl10if[n] = c1[1] 
}
bl10if

cc10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  cc10if[n] = c1[2] 
}
cc10if
df10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  df10if[n] = c1[3] 
}
dfl10if

dh10if <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  dh10if[n] = c1[4] 
}
bl10if

dl10if <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  dl10if[n] = c1[5] 
}
dl10if
dm10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  dm10if[n] = c1[6] 
}
bl10if

do10if <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  do10if[n] = c1[7] 
}
bl10if
eh10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  eh10if[n] = c1[8] 
}
bl10if
ef10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  ef10if[n] = c1[9] 
}
ef10if

hp10if <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  hp10if[n] = c1[10] 
}
hp10if

hb10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  hb10if[n] = c1[11] 
}
hb10if
mf10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  mf10if[n] = c1[12] 
}
bl10if

ow10if <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  ow10if[n] = c1[13] 
}
bl10if
ss10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  ss10if[n] = c1[14] 
}
bl10if
ww10if <- NULL

for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  ww10if[n] = c1[15] 
}
bl10if
nombrs<-c("CC","DF","DH","DL","DM","DO","HP","SS")
#Richness-----
summary(Datos6$habitat.extracted)
bl10dif=NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  bl10dif[n]=c1[1]
}
cc10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  cc10dif[n]=c1[2]
}
df10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  df10dif[n]=c1[3]
}
dh10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  dh10dif[n]=c1[4]
}
dl10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  dl10dif[n]=c1[5]
}
dm10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  dm10dif[n]=c1[6]
}
do10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  do10dif[n]=c1[7]
}
eh10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  eh10dif[n]=c1[8]
}
ef10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  ef10dif[n]=c1[9]
}

hp10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  hp10dif[n]=c1[10]
}
hb10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  hb10dif[n]=c1[11]
}
mf10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  mf10dif[n]=c1[12]
}
ow10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  ow10dif[n]=c1[13]
}
ss10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  ss10dif[n]=c1[14]
}
ww10dif = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  ww10dif[n]=c1[15]
}



#No invasoras----
#Strength----
Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena wilkella" & !Datos4$gen_sp == "Anthidium manicatum" & !Datos4$gen_sp == "Anthidium oblongatum" & !Datos4$gen_sp == "Hylaeus leptocephalus" & !Datos4$gen_sp == "Hylaeus punctatus" & !Datos4$gen_sp == "Lasioglossum leucozonium" & !Datos4$gen_sp == "Megachile apicalis" & !Datos4$gen_sp == "Megachile concinna" & !Datos4$gen_sp == "Megachile rotundata"  & !Datos4$gen_sp == "Megachile sculpturalis" & !Datos4$gen_sp == "Andrena" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))

bl100s <- NULL

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
  
  bl100s[n] = c1[1] 
}
bl100s


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

#Richness-----
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

#Strength No invasoras Con n = 10 -----

bl10s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  
  
  
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  bl10s[n] = c1[1] 
}
bl100s
cc10s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  cc10s[n] = c1[2] 
}
df10s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  df10s[n] = c1[3] 
}
dh10s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  dh10s[n] = c1[4] 
}
dl10s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  dl10s[n] = c1[5] 
}
dm10s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  dm10s[n] = c1[6] 
}

do10s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  do10s[n] = c1[7] 
}

eh10s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  eh10s[n] = c1[8] 
}

ef10s <- NULL

library(bipartite)
for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  ef10s[n] = c1[9] 
}
hp10s <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  hp10s[n] = c1[10] 
}
hb10s <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  hb10s[n] = c1[11] 
}

mf10s <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  mf10s[n] = c1[12] 
}
ow10s <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  ow10s[n] = c1[13] 
}
ss10s <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  ss10s[n] = c1[14] 
}
ww10s <- NULL

for (n in 1:100){
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 10, replace = TRUE), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 10, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 10, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 10, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 10, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 10, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 10, replace = TRUE), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 10, replace = TRUE), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 10, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 10, replace = TRUE), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 10, replace = TRUE), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 10, replace = TRUE), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  bri13<-bri13[sample((nrow(bri13)), 10, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 10, replace = TRUE), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 10, replace = TRUE), ]
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
  
  ww10s[n] = c1[15] 
}
###Richness no invasoras con n= 10----
bl10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  bl10ds[n]=c1[1]
}
cc10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  cc10ds[n]=c1[2]
}
df10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  df10ds[n]=c1[3]
}
dh10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  dh10ds[n]=c1[4]
}

dl10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  dl10ds[n]=c1[5]
}

dm10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  dm10ds[n]=c1[6]
}
do10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  do10ds[n]=c1[7]
}

eh10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  eh10ds[n]=c1[1]
}

ef10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  ef10ds[n]=c1[9]
}

hp10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  hp10ds[n]=c1[10]
}

hb10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  hb10ds[n]=c1[11]
}

mf10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  mf10ds[n]=c1[12]
}

ow10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  ow10ds[n]=c1[13]
}

ss10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  ss10ds[n]=c1[14]
}

ww10ds=NULL
for (n in 1:100){
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 10, replace = TRUE), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 10, replace = TRUE), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 10, replace = TRUE), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 10, replace = TRUE), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 10, replace = TRUE), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri6)), 10, replace = TRUE), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 10, replace = TRUE), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 10, replace = TRUE), ]
  tri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 10, replace = TRUE), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 10, replace = TRUE), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 10, replace = TRUE), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 10, replace = TRUE), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 10, replace = TRUE), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 10, replace = TRUE), ]
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
  
  
  ww10ds[n]=c1[15]
}
#Retirando habitats------
#Strength no invasoras n=40----
Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena wilkella" & !Datos4$gen_sp == "Anthidium manicatum" & !Datos4$gen_sp == "Anthidium oblongatum" & !Datos4$gen_sp == "Hylaeus leptocephalus" & !Datos4$gen_sp == "Hylaeus punctatus" & !Datos4$gen_sp == "Lasioglossum leucozonium" & !Datos4$gen_sp == "Megachile apicalis" & !Datos4$gen_sp == "Megachile concinna" & !Datos4$gen_sp == "Megachile rotundata"  & !Datos4$gen_sp == "Megachile sculpturalis" & !Datos4$gen_sp == "Andrena" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))
library(bipartite)
bl10ifs <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  bl10ifs[n] = c1[1] 
}
bl10ifs
cc10ifs <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  cc10ifs[n] = c1[2] 
}
cc10ifs
df10ifs <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  df10ifs[n] = c1[3] 
}
df10ifs
dh10ifs <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  dh10ifs[n] = c1[4] 
}
dh10ifs
dl10ifs <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  dl10ifs[n] = c1[5] 
}
dl10ifs
dm10ifs <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  dm10ifs[n] = c1[6] 
}
dl10ifs
do10ifs <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  do10ifs[n] = c1[7] 
}
do10ifs
hp10ifs <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  hp10ifs[n] = c1[10] 
}
hp10ifs
ss10ifs <- NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  ss10ifs[n] = c1[14] 
}
ss10ifs
#Richness no invasoras n=40----
bl10difs=NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  bl10difs[n]=c1[1]
}
cc10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  cc10difs[n]=c1[2]
}
df10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  df10difs[n]=c1[3]
}
dh10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  dh10difs[n]=c1[4]
}
dl10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  dl10difs[n]=c1[5]
}
dm10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  dm10difs[n]=c1[6]
}
do10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  do10difs[n]=c1[7]
}
eh10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  eh10difs[n]=c1[8]
}
ef10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  ef10difs[n]=c1[9]
}

hp10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  hp10difs[n]=c1[10]
}
hb10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  hb10difs[n]=c1[11]
}
mf10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  mf10difs[n]=c1[12]
}
ow10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  ow10difs[n]=c1[13]
}
ss10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  ss10difs[n]=c1[14]
}
ww10difs = NULL
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 40, replace = TRUE), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 40, replace = TRUE), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 40, replace = TRUE), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 40, replace = TRUE), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 40, replace = TRUE), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 40, replace = TRUE), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 40, replace = TRUE), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 40, replace = TRUE), ]
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri2,bri3,bri4,bri5,bri6,bri7,bri10,bri14))
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
  
  
  ww10difs[n]=c1[15]
}










#Boxplots----

nombres<-c("BL", "CC","DF","DH","DL","DM","DO", "EH","EF","HP","HB","MF","OW","SS","WW")

boxplot(bl10i,cc10i,df10i,dh10i,dl10i,dm10i,do10i,eh10i,ef10i,hp10i,hb10i,mf10i,ow10i,ss10i,ww10i, main="Strength Invasive Bees n=10", ylab="Strength", xlab="Habitat", names = nombres)
boxplot(bl10di,cc10di,df10di,dh10di,dl10di,dm10di,do10di,eh10di,ef10di,hp10di,hb10di,mf10di,ow10di,ss10di,ww10di, main="Richness Invasive Bees n=10", ylab="Richness", xlab="Habitat", names = nombres)
boxplot(cc10if,df10if,dh10if,dl10if,dm10if,do10if,hp10if,ss10if, main="Strength Invasive Bees n=40", ylab="Strength", xlab="Habitat", names = nombrs)
boxplot(cc10dif,df10dif,dh10dif,dl10dif,dm10dif,do10dif,hp10dif,ss10dif, main="Richness Invasive Bees n=40", ylab="Richness", xlab="Habitat", names = nombrs)


boxplot(bl100s,cc100s,df100s,dh100s,dl100s,dm100s,do100s,eh100s,ef100s,hp100s,hb100s,mf100s,ow100s,ss100s,ww100s, main="Strength Without Invasive Bees n=400", ylab="Strength", xlab="Habitat", names = nombres)
boxplot(bl100ds,cc100ds,df100ds,dh100ds,dl100ds,dm100ds,do100ds,eh100ds,ef100ds,hp100ds,hb100ds,mf100ds,ow100ds,ss100ds,ww100ds, main="Richness Without Invasive Bees n=400", ylab="Richness", xlab="Habitat", names = nombres)
boxplot(bl10s,cc10s,df10s,dh10s,dl10s,dm10s,do10s,eh10s,ef10s,hp10s,hb10s,mf10s,ow10s,ss10s,ww10s, main="Strength Without Invasive Bees n=10", ylab="Strength", xlab="Habitat", names = nombres)
boxplot(bl10ds,cc10ds,df10ds,dh10ds,dl10ds,dm10ds,do10ds,eh10ds,ef10ds,hp10ds,hb10ds,mf10ds,ow10ds,ss10ds,ww10ds, main="Richness Without Invasive Bees n=10", ylab="Strength", xlab="Habitat", names = nombres)
boxplot(cc10ifs,df10ifs,dh10ifs,dl10ifs,dm10ifs,do10ifs,hp10ifs,ss10ifs, main="Strength Without Invasive Bees n=40", ylab="Strength", xlab="Habitat", names = nombrs)
boxplot(cc10difs,df10difs,dh10difs,dl10difs,dm10difs,do10difs,hp10difs,ss10difs, main="Richness Without Invasive Bees n=40", ylab="Richness", xlab="Habitat", names = nombrs)

###Chi-residuals de invasoras----
Datos6<-subset(Datos4, subset=(Datos4$gen_sp == "Andrena wilkella" | Datos4$gen_sp == "Anthidium manicatum" | Datos4$gen_sp == "Anthidium oblongatum" | Datos4$gen_sp == "Hylaeus leptocephalus" | Datos4$gen_sp == "Hylaeus punctatus" | Datos4$gen_sp == "Lasioglossum leucozonium" | Datos4$gen_sp == "Megachile apicalis" | Datos4$gen_sp == "Megachile concinna" | Datos4$gen_sp == "Megachile rotundata"  | Datos4$gen_sp == "Megachile sculpturalis" | Datos4$gen_sp == "Andrena" | Datos4$gen_sp == "Anthophorula micheneri" | Datos4$gen_sp == "Bombus fernaldae" | Datos4$gen_sp == "Bombus imitator" | Datos4$gen_sp == "Cemolobus ipomoeae" | Datos4$gen_sp == "Coelioxys alternata" | Datos4$gen_sp == "Coelioxys dolichos" | Datos4$gen_sp == "Coelioxys immaculata" | Datos4$gen_sp == "Colletes brevicornis" | Datos4$gen_sp == "Epeolus bifasciatus" | Datos4$gen_sp == "Lasioglossum anomalum" | Datos4$gen_sp == "Lasioglossum apopkense" | Datos4$gen_sp == "Lasioglossum asteris" | Datos4$gen_sp == "Lasioglossum curtulum" | Datos4$gen_sp == "Lasioglossum floridanum" | Datos4$gen_sp == "Lasioglossum michiganense" | Datos4$gen_sp == "Lasioglossum pectinatum" | Datos4$gen_sp == "Lasioglossum rufitarse" | Datos4$gen_sp == "Lasioglossum simplex" | Datos4$gen_sp == "Nomada affabilis" | Datos4$gen_sp == "Nomada annulata" | Datos4$gen_sp == "Nomada bethunei" | Datos4$gen_sp == "Nomada composita" | Datos4$gen_sp == "Nomada ovata" | Datos4$gen_sp == "Nomada placida" | Datos4$gen_sp == "Nomada rubicunda" | Datos4$gen_sp == "Nomada valida" | Datos4$gen_sp == "Osmia caerulescens" | Datos4$gen_sp == "Osmia inspergens" | Datos4$gen_sp == "Osmia proxima" | Datos4$gen_sp == "Osmia simillima" | Datos4$gen_sp == "Paranthidium jugatorium" | Datos4$gen_sp == "Perdita bequaerti" | Datos4$gen_sp == "Perdita gerardiae" | Datos4$gen_sp == "Pseudopanurgus andrenoides" | Datos4$gen_sp == "Pseudopanurgus nebrascensis" | Datos4$gen_sp == "Pseudopanurgus rugosus" | Datos4$gen_sp == "Sphecodes antennariae" | Datos4$gen_sp == "Sphecodes aroniae" | Datos4$gen_sp == "Sphecodes clematidis" | Datos4$gen_sp == "Sphecodes cressonii" | Datos4$gen_sp == "Sphecodes galerus" | Datos4$gen_sp == "Sphecodes heraclei" | Datos4$gen_sp == "Sphecodes johnsonii" | Datos4$gen_sp == "Stelis nitida" | Datos4$gen_sp == "Triepeolus helianthi" | Datos4$gen_sp == "Triepeolus pectoralis" | Datos4$gen_sp == "Triepeolus simplex"))


jiji <-aggregate (Datos6$habitat.extracted ~ Datos6$gen_sp, FUN = summary, Datos6)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(habpref)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
#Filtramos para que no aparezcan las especies que solo tienen una
#aparición en nuestros datos
momomos<-replace(momomo, momomo==1,0)
summmm<-rowSums(momomos)
momomosb<-cbind(momomos,summmm)
momomost<-subset(momomosb, subset=momomosb$summmm>0)
momomost<-momomost[,1:15]
observeds<-momomost[,1:15]

#Ahora las preferencias de habitat
rip<-rowSums(momomost)
reee<-colSums(momomost)
sum(reee)
multiplicador<-colSums(momomost)/sum(reee)
a<-multiplicador
matriz1<-cbind(rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip)
matriz1<-as.data.frame(matriz1)
nombres<-names(momomost)
names(matriz1)<-c(nombres)
nrow(momomost)
matriz2<-rbind(a,a,a,a,a,a,a,a,a)
nrow(matriz2)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:9,]
expecteds<-(matriz1*matriz2)
View(observeds)
chis<-chisq.test(observeds, p=expecteds, rescale.p = TRUE)
nombres<-c("BL", "CC","DF","DH","DL","DM","DO", "EH","EF","HP","HB","MF","OW","SS","WW")
boxplot(chis$residuals, names=nombres, main="Habitat preference invasive bees", xlab="Habitats", ylab="Chi-residuals")
colSums(observeds)
dev.off()
#Chi-residuals no invasoras-----
Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena wilkella" & !Datos4$gen_sp == "Anthidium manicatum" & !Datos4$gen_sp == "Anthidium oblongatum" & !Datos4$gen_sp == "Hylaeus leptocephalus" & !Datos4$gen_sp == "Hylaeus punctatus" & !Datos4$gen_sp == "Lasioglossum leucozonium" & !Datos4$gen_sp == "Megachile apicalis" & !Datos4$gen_sp == "Megachile concinna" & !Datos4$gen_sp == "Megachile rotundata"  & !Datos4$gen_sp == "Megachile sculpturalis" & !Datos4$gen_sp == "Andrena" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))



View(Datos6)
jiji <-aggregate (Datos6$habitat.extracted ~ Datos6$gen_sp, FUN = summary, Datos6)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
#Filtramos para que no aparezcan las especies que solo tienen una
#aparición en nuestros datos
momomos<-replace(momomo, momomo==1,0)
summmm<-rowSums(momomos)
momomosb<-cbind(momomos,summmm)
momomost<-subset(momomosb, subset=momomosb$summmm>0)
momomost<-momomost[,1:15]
observeds<-momomost[,1:15]

#Ahora las preferencias de habitat
rip<-rowSums(momomost)
reee<-colSums(momomost)
sum(reee)
multiplicador<-colSums(momomost)/sum(reee)
a<-multiplicador
matriz1<-cbind(rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip)
matriz1<-as.data.frame(matriz1)
nombres<-names(momomost)
names(matriz1)<-c(nombres)
nrow(momomost)
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
nrow(matriz2)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:304,]
expecteds<-(matriz1*matriz2)

chisn<-chisq.test(observeds, p=expecteds, rescale.p = TRUE)
nombres<-c("BL", "CC","DF","DH","DL","DM","DO", "EH","EF","HP","HB","MF","OW","SS","WW")
boxplot(chisn$residuals, main="Habitat preference without invasive bees", names= nombres ,xlab="Habitats", ylab="Chi-residuals")

#Chi-residuals no invasoras para n>100 casos-----
Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena wilkella" & !Datos4$gen_sp == "Anthidium manicatum" & !Datos4$gen_sp == "Anthidium oblongatum" & !Datos4$gen_sp == "Hylaeus leptocephalus" & !Datos4$gen_sp == "Hylaeus punctatus" & !Datos4$gen_sp == "Lasioglossum leucozonium" & !Datos4$gen_sp == "Megachile apicalis" & !Datos4$gen_sp == "Megachile concinna" & !Datos4$gen_sp == "Megachile rotundata"  & !Datos4$gen_sp == "Megachile sculpturalis" & !Datos4$gen_sp == "Andrena" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))


#Vamos a crear la matriz de datos de abundancia esperada para cada habitat, para ello
#primero sacamos la matriz de interacciones de nuestros datos
jiji <-aggregate (Datos6$habitat.extracted ~ Datos6$gen_sp, FUN = summary, Datos6)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
View(momomo)
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
filtra<-rowSums(momomo)
mememe<-cbind(momomo,filtra)
mo<-subset(mememe, subset=(mememe$filtra)>100)
momomo<-mo[,-16]
observed<-momomo
View(observed)
#Ahora vamos a crear la matriz de abundancia esperada para cada habitat, para ello hacemos
#un vector que contenga la cantidad total de individuos de cada especie, y convertirmos este
#vector en una matriz, luego hacemos otro con la proporción de la cantidad total de
#individuos que le corresponde estar, sacado de la abundancia en cada habitat, y también
#hacemos una matriz, multiplicamos las dos matrices y obtenemos la matriz de abundancia
#esperada
rip<-rowSums(momomo)
reee<-colSums(momomo)
sum(reee)
multiplicador<-colSums(momomo)/sum(reee)
matriz1<-cbind(rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip)
matriz1<-as.data.frame(matriz1)
nombres<-names(momomo)
names(matriz1)<-c(nombres)
nrow(momomo)
a<-multiplicador
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
nrow(matriz2)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:89,]
expected<-(matriz1*matriz2)
View(observed)
View(expected)

chisn100<-chisq.test(observed, p=expected, rescale.p = TRUE)
nombres<-c("BL", "CC","DF","DH","DL","DM","DO", "EH","EF","HP","HB","MF","OW","SS","WW")
boxplot(chisn100$residuals, main="Habitat preference without invasive bees n>100", names= nombres ,xlab="Habitats", ylab="Chi-residuals")

