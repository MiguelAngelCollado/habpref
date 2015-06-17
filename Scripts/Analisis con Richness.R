#Vamos a repetir los análisis de diversidad, extrayendo richness, osea, el nº de
#especies presente

bl100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  bl100d[n]=c1[1]
}
bl100d
cc100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  cc100d[n]=c1[2]
}

df100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  df100d[n]=c1[3]
}

dh100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  dh100d[n]=c1[4]
}







ww100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  ww100d[n]=c1[15]
}

dl100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  dl100d[n]=c1[5]
}

dm100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  dm100d[n]=c1[6]
}

do100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  do100d[n]=c1[7]
}

eh100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  eh100d[n]=c1[8]
}

ef100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  ef100d[n]=c1[9]
}

hp100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  hp100d[n]=c1[10]
}

hb100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  hb100d[n]=c1[11]
}

mf100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  mf100d[n]=c1[12]
}

ow100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  ow100d[n]=c1[13]
}

ss100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  ss100d[n]=c1[14]
}

ww100d=NULL
for (n in 1:100){
  tri1<-subset(Datos4, subset=(Datos4$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 400), ]
  tri2<-subset(Datos4, subset=(Datos4$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 400), ]
  tri3<-subset(Datos4, subset=(Datos4$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 400), ]
  tri4<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 400), ]
  tri5<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 400), ]
  tri6<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 400), ]
  tri7<-subset(Datos4, subset=(Datos4$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 400), ]
  tri8<-subset(Datos4, subset=(Datos4$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 400), ]
  tri9<-subset(Datos4, subset=(Datos4$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 400), ]
  tri10<-subset(Datos4, subset=(Datos4$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 400), ]
  tri11<-subset(Datos4, subset=(Datos4$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 400), ]
  tri12<-subset(Datos4, subset=(Datos4$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 400), ]
  tri13<-subset(Datos4, subset=(Datos4$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 400), ]
  tri14<-subset(Datos4, subset=(Datos4$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 400), ]
  tri15<-subset(Datos4, subset=(Datos4$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 400), ]
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
  
  
  ww100d[n]=c1[15]
}



bl100d
cc100d
df100d
dh100d
dl100d
dm100d
do100d
eh100d
ef100d
hp100d
hb100d
mf100d
ow100d
ss100d
ww100d


# y creamos un vector con las medias de la diversidad de cada habitat
div<-c(mean(bl100d),mean(cc100d),mean(df100d),mean(dh100d),mean(dl100d)
       ,mean(dm100d), mean(do100d), mean(eh100d), mean(ef100d), 
       mean(hp100d), mean(hb100d), mean(mf100d), mean(ow100d), 
       mean(ss100d), mean(ww100d))
divsd<-c(sd(bl100d),sd(cc100d),sd(df100d),sd(dh100d),sd(dl100d)
         ,sd(dm100d), sd(do100d), sd(eh100d), sd(ef100d), 
         sd(hp100d), sd(hb100d), sd(mf100d), sd(ow100d), 
         sd(ss100d), sd(ww100d))

div
divsd

#######################Sin singletones###############################################

Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena accepta" & !Datos4$gen_sp == "Andrena braccata" & !Datos4$gen_sp == "Andrena cragini" & !Datos4$gen_sp == "Andrena erythrogaster" & !Datos4$gen_sp == "Andrena integra" & !Datos4$gen_sp == "Andrena phaceliae" & !Datos4$gen_sp == "Andrena polemonii" & !Datos4$gen_sp == "Andrena salictaria" & !Datos4$gen_sp == "Andrena sigmundi"  & !Datos4$gen_sp == "Andrena w-scripta" & !Datos4$gen_sp == "Andrena zizeaformis" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))
View(Datos6)
View(Datos4)
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
# y creamos un vector con las medias de la diversidad de cada habitat
divs<-c(mean(bl100ds),mean(cc100ds),mean(df100ds),mean(dh100ds),mean(dl100ds)
        ,mean(dm100ds), mean(do100ds), mean(eh100ds), mean(ef100ds), 
        mean(hp100ds), mean(hb100ds), mean(mf100ds), mean(ow100ds), 
        mean(ss100ds), mean(ww100ds))
divs
divsd<-c(sd(bl100d),sd(cc100d),sd(df100d),sd(dh100d),sd(dl100d)
         ,sd(dm100d), sd(do100d), sd(eh100d), sd(ef100d), 
         sd(hp100d), sd(hb100d), sd(mf100d), sd(ow100d), 
         sd(ss100d), sd(ww100d))
divs
divsd




######################Sin repeticiones en el mismo punto de muestreo xd##############
#Ahora con mis datos
Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
View(Datos4)
#Creo un vector único de identificación del sitio y lo adjunto a mis datos
sumcord<-(Datos4$latitude + Datos4$longitude)
Datos9<-(cbind(Datos4,sumcord))
View(Datos9)

#Aquí voy a filtrar mis datos, dejándome solo la cita de cada primera especie que me
#encuentro en cada punto de muestreo


psssss<-aggregate(Datos9, by=list(Datos9$sumcord, Datos9$gen_sp, Datos9$habitat.extracted),
                  FUN=duplicated, na.rm=TRUE)

Datos11<-psssss[3:14780,]
Datos11<-as.data.frame(Datos11)


#con summary, vemos que el grupo que tiene menos especies es barren land, por eso
#ahora los muestreos aleatorios los haremos para 100 casos
summary(Datos11$Group.3)

#Sacamos diversidad
bl100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  bl100dr[n]=c1[1]
}
bl100dr
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

cc100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  cc100dr[n]=c1[2]
}
bl100dr
cc100dr

df100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  df100dr[n]=c1[3]
}
df100dr
mean(df100dr)
dh100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  dh100dr[n]=c1[4]
}
dh100dr
dl100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  dl100dr[n]=c1[5]
}
dl100dr
dm100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  dm100dr[n]=c1[6]
}
dm100dr
do100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  do100dr[n]=c1[7]
}
do100dr
mean(do100dr)
eh100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  eh100dr[n]=c1[8]
}
eh100dr

ef100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  ef100dr[n]=c1[9]
}
ef100dr


ef100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  ef100dr[n]=c1[9]
}
ef100dr

hp100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  hp100dr[n]=c1[10]
}
hp100dr

hb100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  hb100dr[n]=c1[11]
}
hb100dr

mf100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  mf100dr[n]=c1[12]
}
mf100dr

ow100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  ow100dr[n]=c1[13]
}
ow100dr

ss100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  ss100dr[n]=c1[14]
}
ss100dr

ww100dr=NULL
for (n in 1:100){
  tri1<-subset(Datos11, subset=(Datos11$Group.3 == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(Datos11, subset=(Datos11$Group.3 == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(Datos11, subset=(Datos11$Group.3 == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(Datos11, subset=(Datos11$Group.3 == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(Datos11, subset=(Datos11$Group.3 == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(Datos11, subset=(Datos11$Group.3 == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(Datos11, subset=(Datos11$Group.3 == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(Datos11, subset=(Datos11$Group.3 == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(Datos11, subset=(Datos11$Group.3 == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(Datos11, subset=(Datos11$Group.3 == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(Datos11, subset=(Datos11$Group.3 == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(Datos11, subset=(Datos11$Group.3 == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (Datos7$Group.3 ~ Datos7$Group.2, FUN = summary, Datos7)
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
  
  
  ww100dr[n]=c1[15]
}
ww100dr






# y creamos un vector con las medias de la diversidad de cada habitat
divr<-c(mean(bl100dr),mean(cc100dr),mean(df100dr),mean(dh100dr),mean(dl100dr)
        ,mean(dm100dr), mean(do100dr), mean(eh100dr), mean(ef100dr), 
        mean(hp100dr), mean(hb100dr), mean(mf100dr), mean(ow100dr), 
        mean(ss100dr), mean(ww100dr))

divr
divrsd<-c(sd(bl100dr),sd(cc100dr),sd(df100dr),sd(dh100dr),sd(dl100dr)
          ,sd(dm100dr), sd(do100dr), sd(eh100dr), sd(ef100dr), 
          sd(hp100dr), sd(hb100dr), sd(mf100dr), sd(ow100dr), 
          sd(ss100dr), sd(ww100dr))

divrsd


#######################################Sin invasoras################################
Datos13
Datos13<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena wilkella" & !Datos4$gen_sp == "Anthidium manicatum" & !Datos4$gen_sp == "Anthidium oblongatum" & !Datos4$gen_sp == "Hylaeus leptocephalus" & !Datos4$gen_sp == "Hylaeus punctatus" & !Datos4$gen_sp == "Lasioglossum leucozonium" & !Datos4$gen_sp == "Megachile apicalis" & !Datos4$gen_sp == "Megachile concinna" & !Datos4$gen_sp == "Megachile rotundata"  & !Datos4$gen_sp == "Megachile sculpturalis" & !Datos4$gen_sp == "Andrena" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))
View(Datos6)
View(Datos4)
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


# y creamos un vector con las medias de la diversidad de cada habitat
divs<-c(mean(bl100ds),mean(cc100ds),mean(df100ds),mean(dh100ds),mean(dl100ds)
        ,mean(dm100ds), mean(do100ds), mean(eh100ds), mean(ef100ds), 
        mean(hp100ds), mean(hb100ds), mean(mf100ds), mean(ow100ds), 
        mean(ss100ds), mean(ww100ds))
divs
divsd<-c(sd(bl100d),sd(cp100d),sd(df100d),sd(dh100d),sd(dl100d)
         ,sd(dm100d), sd(do100d), sd(eh100d), sd(ef100d), 
         sd(hp100d), sd(hb100d), sd(mf100d), sd(ow100d), 
         sd(ss100d), sd(ww100d))
names(div)<-nombres
divsd














################INVIERNO VERANO#################################

invierno<-subset(Datos4, subset=(!(jday_end>150 & jday_end<305)))
verano<-subset(Datos4, subset=(jday_end>150 & jday_end<305))
View(invierno)
summary(invierno$habitat.extracted)
summary(verano$habitat.extracted)

bl100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  bl100dve[n]=c1[1]
}
bl100dve

cc100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  cc100dve[n]=c1[2]
}
cc100dve

df100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  df100dve[n]=c1[3]
}
df100dve

dh100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  dh100dve[n]=c1[4]
}
dh100dve

dl100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  dl100dve[n]=c1[5]
}
dl100dve



dm100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  dm100dve[n]=c1[6]
}
dm100dve

do100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  do100dve[n]=c1[7]
}
do100dve

eh100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  eh100dve[n]=c1[8]
}
eh100dve

ef100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  ef100dve[n]=c1[9]
}
ef100dve

hp100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  hp100dve[n]=c1[10]
}
hp100dve

hb100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  hb100dve[n]=c1[11]
}
hb100dve

mf100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  mf100dve[n]=c1[12]
}
mf100dve

ow100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  ow100dve[n]=c1[13]
}
ow100dve

ss100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  ss100dve[n]=c1[14]
}
ss100dve

ww100dve=NULL
for (n in 1:100){
  tri1<-subset(verano, subset=(verano$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 100), ]
  tri2<-subset(verano, subset=(verano$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 100), ]
  tri3<-subset(verano, subset=(verano$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 100), ]
  tri4<-subset(verano, subset=(verano$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 100), ]
  tri5<-subset(verano, subset=(verano$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 100), ]
  tri6<-subset(verano, subset=(verano$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 100), ]
  tri7<-subset(verano, subset=(verano$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 100), ]
  tri8<-subset(verano, subset=(verano$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 100), ]
  tri9<-subset(verano, subset=(verano$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 100), ]
  tri10<-subset(verano, subset=(verano$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 100), ]
  tri11<-subset(verano, subset=(verano$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 100), ]
  tri12<-subset(verano, subset=(verano$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 100), ]
  tri13<-subset(verano, subset=(verano$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 100), ]
  tri14<-subset(verano, subset=(verano$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 100), ]
  tri15<-subset(verano, subset=(verano$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 100), ]
  veranete<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (veranete$habitat.extracted ~ veranete$gen_sp, FUN = summary, Datos7)
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
  
  
  ww100dve[n]=c1[15]
}
ww100dve

divve<-c(mean(bl100ve),mean(cc100ve),mean(df100ve),mean(dh100ve),mean(dl100ve)
         ,mean(dm100ve), mean(do100ve), mean(eh100ve), mean(ef100ve), 
         mean(hp100ve), mean(hb100ve), mean(mf100ve), mean(ow100ve), 
         mean(ss100ve), mean(ww100ve))

divve
divrsd<-c(sd(bl100ve),sd(cc100ve),sd(df100ve),sd(dh100ve),sd(dl100ve)
          ,sd(dm100ve), sd(do100ve), sd(eh100ve), sd(ef100ve), 
          sd(hp100ve), sd(hb100ve), sd(mf100ve), sd(ow100ve), 
          sd(ss100ve), sd(ww100ve))
names(div)<-nombres
divrsd


#INVIERNO

#Diversidad
bl100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  bl100dinv[n]=c1[1]
}
bl100dinv


#Diversidad
cc100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  cc100dinv[n]=c1[2]
}
cc100dinv
mean(cc100dinv)
df100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  df100dinv[n]=c1[3]
}
df100dinv

dh100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  dh100dinv[n]=c1[4]
}
dh100dinv
mean(dh100dinv)
dl100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  dl100dinv[n]=c1[5]
}
dl100dinv
mean(dl100dinv)

dm100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  dm100dinv[n]=c1[6]
}
dm100dinv

do100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  do100dinv[n]=c1[7]
}
do100dinv

eh100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  eh100dinv[n]=c1[8]
}
eh100dinv


ef100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  ef100dinv[n]=c1[9]
}
ef100dinv

hp100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  hp100dinv[n]=c1[10]
}
hp100dinv


hb100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  hb100dinv[n]=c1[11]
}
hb100dinv

mf100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  mf100dinv[n]=c1[12]
}
mf100dinv

ow100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  ow100dinv[n]=c1[13]
}
ow100dinv

ss100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  ss100dinv[n]=c1[14]
}
ss100dinv

ww100dinv=NULL
for (n in 1:100){
  tri2<-subset(invierno, subset=(invierno$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 50), ]
  tri3<-subset(invierno, subset=(invierno$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 50), ]
  tri4<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 50), ]
  tri5<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 50), ]
  tri6<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 50), ]
  tri7<-subset(invierno, subset=(invierno$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 50), ]
  tri8<-subset(invierno, subset=(invierno$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 50), ]
  tri9<-subset(invierno, subset=(invierno$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 50), ]
  tri10<-subset(invierno, subset=(invierno$habitat.extracted == "Hay/Pasture"))
  tri10<-tri10[sample((nrow(tri10)), 50), ]
  tri11<-subset(invierno, subset=(invierno$habitat.extracted == "Herbaceuous"))
  tri11<-tri11[sample((nrow(tri11)), 50), ]
  tri12<-subset(invierno, subset=(invierno$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 50), ]
  tri13<-subset(invierno, subset=(invierno$habitat.extracted == "Open Water"))
  tri13<-tri13[sample((nrow(tri13)), 50), ]
  tri14<-subset(invierno, subset=(invierno$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 50), ]
  tri15<-subset(invierno, subset=(invierno$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 50), ]
  inviernete<-(rbind(tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15))
  #Creamos matriz de interacciones
  jiji <-aggregate (inviernete$habitat.extracted ~ inviernete$gen_sp, FUN = summary, inviernete)
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
  
  
  ww100dinv[n]=c1[15]
}
ww100dinv

divinvr<-c(mean(bl100dinv),mean(cc100dinv),mean(df100dinv),mean(dh100dinv),mean(dl100dinv)
           ,mean(dm100dinv), mean(do100dinv), mean(eh100dinv), mean(ef100dinv), 
           mean(hp100dinv), mean(hb100dinv), mean(mf100dinv), mean(ow100dinv), 
           mean(ss100dinv), mean(ww100dinv))
divinvr
divr
divrinvsd<-c(sd(bl100dinv),sd(cc100dinv),sd(df100dinv),sd(dh100dinv),sd(dl100dinv)
             ,sd(dm100dinv), sd(do100dinv), sd(eh100dinv), sd(ef100dinv), 
             sd(hp100dinv), sd(hb100dinv), sd(mf100dinv), sd(ow100dinv), 
             sd(ss100dinv), sd(ww100dinv))
divrinvsd


