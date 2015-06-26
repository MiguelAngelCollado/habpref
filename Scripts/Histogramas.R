Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
View(Datos4)

#Histogramas
#Primero ponemos los datos en forma de matriz de interacciones
jiji <-aggregate (Datos4$habitat.extracted ~ Datos4$gen_sp, FUN = summary, Datos4)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)

#Y ahora sacamos los histogramas, filtrando las especies que no aparecen en cada habitat
pdf(file = "histogram.pdf")
hist(subset(momomo$Datos4.habitat.extracted.Barren.Land,
            subset = (momomo$Datos4.habitat.extracted.Barren.Land > 0))
          , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Barren Land")
hist(subset(momomo$Datos4.habitat.extracted.Cultivated.Crops,
            subset = (momomo$Datos4.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Cultivated Crops")
hist(subset(momomo$Datos4.habitat.extracted.Deciduous.Forest,
            subset = (momomo$Datos4.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Deciduous Forest")
hist(subset(momomo$Datos4.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed High Intensity")
hist(subset(momomo$Datos4.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos4.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Medium Intensity")
hist(subset(momomo$Datos4.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos4.habitat.extracted.Developed..Open.Space,
            subset = (momomo$Datos4.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Open Space")
hist(subset(momomo$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Emergent Herbaceuous Wetlands")
hist(subset(momomo$Datos4.habitat.extracted.Evergreen.Forest,
            subset = (momomo$Datos4.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Evergreen Forest")
hist(subset(momomo$Datos4.habitat.extracted.Hay.Pasture,
            subset = (momomo$Datos4.habitat.extracted.Hay.Pasture > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Hay Pasture")
hist(subset(momomo$Datos4.habitat.extracted.Herbaceuous,
            subset = (momomo$Datos4.habitat.extracted.Herbaceuous > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Herbaceuous")
hist(subset(momomo$Datos4.habitat.extracted.Open.Water,
            subset = (momomo$Datos4.habitat.extracted.Open.Water > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Open Water")
hist(subset(momomo$Datos4.habitat.extracted.Shrub.Scrub,
            subset = (momomo$Datos4.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Shrub Scrub")
hist(subset(momomo$Datos4.habitat.extracted.Woody.Wetlands,
            subset = (momomo$Datos4.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Woody Wetlands")
dev.off()

#Histogramas de los habitats sin repeticiones en la aprición de la misma especie en el
#mismo punto de muestreo
sumcord<-(Datos4$latitude + Datos4$longitude)
Datos9<-(cbind(Datos4,sumcord))
View(Datos9)



attach(Datos9)
psssss<-aggregate(Datos9, by=list(sumcord, gen_sp, habitat.extracted),
                  FUN=duplicated, na.rm=TRUE)

Datos11<-psssss[3:14780,]
Datos11<-as.data.frame(Datos11)
detach(Datos9)
View(Datos11)


jiji <-aggregate (Datos11$Group.3 ~ Datos11$Group.2, FUN = summary, Datos11)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
View(momomo)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
View(momomo)
#Y ahora sacamos los histogramas, filtrando las especies que no aparecen en cada habitat
pdf("histogramsr.pdf")
hist(subset(momomo$Datos11.Group.3.Barren.Land,
            subset = (momomo$Datos11.Group.3.Barren.Land > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Barren Land")
hist(subset(momomo$Datos11.Group.3.Cultivated.Crops,
            subset = (momomo$Datos11.Group.3.Cultivated.Crops > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Cultivated Crops")
hist(subset(momomo$Datos11.Group.3.Deciduous.Forest,
            subset = (momomo$Datos11.Group.3.Deciduous.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Deciduous Forest")
hist(subset(momomo$Datos11.Group.3.Developed..High.Intensity,
            subset = (momomo$Datos11.Group.3.Developed..High.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed High Intensity")
hist(subset(momomo$Datos11.Group.3.Developed..Low.Intensity,
            subset = (momomo$Datos11.Group.3.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos11.Group.3.Developed..Medium.Intensity,
            subset = (momomo$Datos11.Group.3.Developed..Medium.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Medium Intensity")
hist(subset(momomo$Datos11.Group.3.Developed..Low.Intensity,
            subset = (momomo$Datos11.Group.3.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos11.Group.3.Developed..Open.Space,
            subset = (momomo$Datos11.Group.3.Developed..Open.Space > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Open Space")
hist(subset(momomo$Datos11.Group.3.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$Datos11.Group.3.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Emergent Herbaceuous Wetlands")
hist(subset(momomo$Datos11.Group.3.Evergreen.Forest,
            subset = (momomo$Datos11.Group.3.Evergreen.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Evergreen Forest")
hist(subset(momomo$Datos11.Group.3.Hay.Pasture,
            subset = (momomo$Datos11.Group.3.Hay.Pasture > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Hay Pasture")
hist(subset(momomo$Datos11.Group.3.Herbaceuous,
            subset = (momomo$Datos11.Group.3.Herbaceuous > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Herbaceuous")
hist(subset(momomo$Datos11.Group.3.Open.Water,
            subset = (momomo$Datos11.Group.3.Open.Water > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Open Water")
hist(subset(momomo$Datos11.Group.3.Shrub.Scrub,
            subset = (momomo$Datos11.Group.3.Shrub.Scrub > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Shrub Scrub")
hist(subset(momomo$Datos11.Group.3.Woody.Wetlands,
            subset = (momomo$Datos11.Group.3.Woody.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Woody Wetlands")
dev.off()

#Con el subset de invierno / verano

#invierno
invierno<-subset(Datos4, subset=(!(jday_end>150 & jday_end<305)))
verano<-subset(Datos4, subset=(jday_end>150 & jday_end<305))
jiji <-aggregate (invierno$habitat.extracted ~ invierno$gen_sp, FUN = summary, invierno)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
View(momomo)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
View(momomo)

pdf(file = "histogrami.pdf")
hist(subset(momomo$invierno.habitat.extracted.Barren.Land,
            subset = (momomo$invierno.habitat.extracted.Barren.Land > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Barren Land")
hist(subset(momomo$invierno.habitat.extracted.Cultivated.Crops,
            subset = (momomo$invierno.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Cultivated Crops")
hist(subset(momomo$invierno.habitat.extracted.Deciduous.Forest,
            subset = (momomo$invierno.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Deciduous Forest")
hist(subset(momomo$invierno.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$invierno.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed High Intensity")
hist(subset(momomo$invierno.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$invierno.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$invierno.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$invierno.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Medium Intensity")
hist(subset(momomo$invierno.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$invierno.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$invierno.habitat.extracted.Developed..Open.Space,
            subset = (momomo$invierno.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Open Space")
hist(subset(momomo$invierno.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$invierno.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Emergent Herbaceuous Wetlands")
hist(subset(momomo$invierno.habitat.extracted.Evergreen.Forest,
            subset = (momomo$invierno.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Evergreen Forest")
hist(subset(momomo$invierno.habitat.extracted.Hay.Pasture,
            subset = (momomo$invierno.habitat.extracted.Hay.Pasture > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Hay Pasture")
hist(subset(momomo$invierno.habitat.extracted.Herbaceuous,
            subset = (momomo$invierno.habitat.extracted.Herbaceuous > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Herbaceuous")
hist(subset(momomo$invierno.habitat.extracted.Open.Water,
            subset = (momomo$invierno.habitat.extracted.Open.Water > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Open Water")
hist(subset(momomo$invierno.habitat.extracted.Shrub.Scrub,
            subset = (momomo$invierno.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Shrub Scrub")
hist(subset(momomo$invierno.habitat.extracted.Woody.Wetlands,
            subset = (momomo$invierno.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Woody Wetlands")
dev.off()

#verano
jiji <-aggregate (verano$habitat.extracted ~ verano$gen_sp, FUN = summary, verano)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
View(momomo)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
View(momomo)



pdf(file = "histogramv.pdf")
hist(subset(momomo$verano.habitat.extracted.Barren.Land,
            subset = (momomo$verano.habitat.extracted.Barren.Land > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Barren Land")
hist(subset(momomo$verano.habitat.extracted.Cultivated.Crops,
            subset = (momomo$verano.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Cultivated Crops")
hist(subset(momomo$verano.habitat.extracted.Deciduous.Forest,
            subset = (momomo$verano.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Deciduous Forest")
hist(subset(momomo$verano.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$verano.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed High Intensity")
hist(subset(momomo$verano.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$verano.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$verano.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$verano.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Medium Intensity")
hist(subset(momomo$verano.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$verano.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$verano.habitat.extracted.Developed..Open.Space,
            subset = (momomo$verano.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Open Space")
hist(subset(momomo$verano.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$verano.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Emergent Herbaceuous Wetlands")
hist(subset(momomo$verano.habitat.extracted.Evergreen.Forest,
            subset = (momomo$verano.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Evergreen Forest")
hist(subset(momomo$verano.habitat.extracted.Hay.Pasture,
            subset = (momomo$verano.habitat.extracted.Hay.Pasture > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Hay Pasture")
hist(subset(momomo$verano.habitat.extracted.Herbaceuous,
            subset = (momomo$verano.habitat.extracted.Herbaceuous > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Herbaceuous")
hist(subset(momomo$verano.habitat.extracted.Open.Water,
            subset = (momomo$verano.habitat.extracted.Open.Water > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Open Water")
hist(subset(momomo$verano.habitat.extracted.Shrub.Scrub,
            subset = (momomo$verano.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Shrub Scrub")
hist(subset(momomo$verano.habitat.extracted.Woody.Wetlands,
            subset = (momomo$verano.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Woody Wetlands")
dev.off()

#histogramas sin singletones 

Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena accepta" & !Datos4$gen_sp == "Andrena braccata" & !Datos4$gen_sp == "Andrena cragini" & !Datos4$gen_sp == "Andrena erythrogaster" & !Datos4$gen_sp == "Andrena integra" & !Datos4$gen_sp == "Andrena phaceliae" & !Datos4$gen_sp == "Andrena polemonii" & !Datos4$gen_sp == "Andrena salictaria" & !Datos4$gen_sp == "Andrena sigmundi"  & !Datos4$gen_sp == "Andrena w-scripta" & !Datos4$gen_sp == "Andrena zizeaformis" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))
View(Datos6)

jiji <-aggregate (Datos6$habitat.extracted ~ Datos6$gen_sp, FUN = summary, Datos6)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
View(momomo)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)

#Y ahora sacamos los histogramas, filtrando las especies que no aparecen en cada habitat
pdf(file = "histograms.pdf")
hist(subset(momomo$Datos6.habitat.extracted.Barren.Land,
            subset = (momomo$Datos6.habitat.extracted.Barren.Land > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Barren Land")
hist(subset(momomo$Datos6.habitat.extracted.Cultivated.Crops,
            subset = (momomo$Datos6.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Cultivated Crops")
hist(subset(momomo$Datos6.habitat.extracted.Deciduous.Forest,
            subset = (momomo$Datos6.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Deciduous Forest")
hist(subset(momomo$Datos6.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$Datos6.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed High Intensity")
hist(subset(momomo$Datos6.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos6.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos6.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$Datos6.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Medium Intensity")
hist(subset(momomo$Datos6.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos6.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos6.habitat.extracted.Developed..Open.Space,
            subset = (momomo$Datos6.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Open Space")
hist(subset(momomo$Datos6.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$Datos6.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Emergent Herbaceuous Wetlands")
hist(subset(momomo$Datos6.habitat.extracted.Evergreen.Forest,
            subset = (momomo$Datos6.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Evergreen Forest")
hist(subset(momomo$Datos6.habitat.extracted.Hay.Pasture,
            subset = (momomo$Datos6.habitat.extracted.Hay.Pasture > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Hay Pasture")
hist(subset(momomo$Datos6.habitat.extracted.Herbaceuous,
            subset = (momomo$Datos6.habitat.extracted.Herbaceuous > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Herbaceuous")
hist(subset(momomo$Datos6.habitat.extracted.Open.Water,
            subset = (momomo$Datos6.habitat.extracted.Open.Water > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Open Water")
hist(subset(momomo$Datos6.habitat.extracted.Shrub.Scrub,
            subset = (momomo$Datos6.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Shrub Scrub")
hist(subset(momomo$Datos6.habitat.extracted.Woody.Wetlands,
            subset = (momomo$Datos6.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Woody Wetlands")
dev.off()

#Sin especies invasoras
Datos13
Datos13<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena wilkella" & !Datos4$gen_sp == "Anthidium manicatum" & !Datos4$gen_sp == "Anthidium oblongatum" & !Datos4$gen_sp == "Hylaeus leptocephalus" & !Datos4$gen_sp == "Hylaeus punctatus" & !Datos4$gen_sp == "Lasioglossum leucozonium" & !Datos4$gen_sp == "Megachile apicalis" & !Datos4$gen_sp == "Megachile concinna" & !Datos4$gen_sp == "Megachile rotundata"  & !Datos4$gen_sp == "Megachile sculpturalis" & !Datos4$gen_sp == "Andrena" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))

jiji <-aggregate (Datos13$habitat.extracted ~ Datos13$gen_sp, FUN = summary, Datos13)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
View(momomo)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)

#Y ahora sacamos los histogramas, filtrando las especies que no aparecen en cada habitat
pdf(file = "histograminv.pdf")
hist(subset(momomo$Datos13.habitat.extracted.Barren.Land,
            subset = (momomo$Datos13.habitat.extracted.Barren.Land > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Barren Land")
hist(subset(momomo$Datos13.habitat.extracted.Cultivated.Crops,
            subset = (momomo$Datos13.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Cultivated Crops")
hist(subset(momomo$Datos13.habitat.extracted.Deciduous.Forest,
            subset = (momomo$Datos13.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Deciduous Forest")
hist(subset(momomo$Datos13.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$Datos13.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed High Intensity")
hist(subset(momomo$Datos13.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos13.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos13.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$Datos13.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Medium Intensity")
hist(subset(momomo$Datos13.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos13.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos13.habitat.extracted.Developed..Open.Space,
            subset = (momomo$Datos13.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Open Space")
hist(subset(momomo$Datos13.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$Datos13.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Emergent Herbaceuous Wetlands")
hist(subset(momomo$Datos13.habitat.extracted.Evergreen.Forest,
            subset = (momomo$Datos13.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Evergreen Forest")
hist(subset(momomo$Datos13.habitat.extracted.Hay.Pasture,
            subset = (momomo$Datos13.habitat.extracted.Hay.Pasture > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Hay Pasture")
hist(subset(momomo$Datos13.habitat.extracted.Herbaceuous,
            subset = (momomo$Datos13.habitat.extracted.Herbaceuous > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Herbaceuous")
hist(subset(momomo$Datos13.habitat.extracted.Open.Water,
            subset = (momomo$Datos13.habitat.extracted.Open.Water > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Open Water")
hist(subset(momomo$Datos13.habitat.extracted.Shrub.Scrub,
            subset = (momomo$Datos13.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Shrub Scrub")
hist(subset(momomo$Datos13.habitat.extracted.Woody.Wetlands,
            subset = (momomo$Datos13.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 50, xlab= "Abundancia", ylab= "Frecuencia", main = "Woody Wetlands")
dev.off()
#Hacemos uno extra en el que quitamos los valores "1" de cada habitat
jiji <-aggregate (Datos4$habitat.extracted ~ Datos4$gen_sp, FUN = summary, Datos4)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)

#Y ahora sacamos los histogramas, filtrando las especies que no aparecen en cada habitat
pdf(file = "histogramwo1.pdf")
hist(subset(momomo$Datos4.habitat.extracted.Barren.Land,
            subset = (momomo$Datos4.habitat.extracted.Barren.Land > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Barren Land")
hist(subset(momomo$Datos4.habitat.extracted.Cultivated.Crops,
            subset = (momomo$Datos4.habitat.extracted.Cultivated.Crops > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Cultivated Crops")
hist(subset(momomo$Datos4.habitat.extracted.Deciduous.Forest,
            subset = (momomo$Datos4.habitat.extracted.Deciduous.Forest > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Deciduous Forest")
hist(subset(momomo$Datos4.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..High.Intensity > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed High Intensity")
hist(subset(momomo$Datos4.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..Low.Intensity > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos4.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..Medium.Intensity > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Medium Intensity")
hist(subset(momomo$Datos4.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..Low.Intensity > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Low Intensity")
hist(subset(momomo$Datos4.habitat.extracted.Developed..Open.Space,
            subset = (momomo$Datos4.habitat.extracted.Developed..Open.Space > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Developed Open Space")
hist(subset(momomo$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Emergent Herbaceuous Wetlands")
hist(subset(momomo$Datos4.habitat.extracted.Evergreen.Forest,
            subset = (momomo$Datos4.habitat.extracted.Evergreen.Forest > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Evergreen Forest")
hist(subset(momomo$Datos4.habitat.extracted.Hay.Pasture,
            subset = (momomo$Datos4.habitat.extracted.Hay.Pasture > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Hay Pasture")
hist(subset(momomo$Datos4.habitat.extracted.Herbaceuous,
            subset = (momomo$Datos4.habitat.extracted.Herbaceuous > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Herbaceuous")
hist(subset(momomo$Datos4.habitat.extracted.Open.Water,
            subset = (momomo$Datos4.habitat.extracted.Open.Water > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Open Water")
hist(subset(momomo$Datos4.habitat.extracted.Shrub.Scrub,
            subset = (momomo$Datos4.habitat.extracted.Shrub.Scrub > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Shrub Scrub")
hist(subset(momomo$Datos4.habitat.extracted.Woody.Wetlands,
            subset = (momomo$Datos4.habitat.extracted.Woody.Wetlands > 1))
     , breaks = 100, xlab= "Abundancia", ylab= "Frecuencia", main = "Woody Wetlands")
dev.off()


#Aquí los valores de cada gráfica


#Valores de histogramas Datos4
jiji <-aggregate (Datos4$habitat.extracted ~ Datos4$gen_sp, FUN = summary, Datos4)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
sink("histogramnumbers.txt")
subset(momomo$Datos4.habitat.extracted.Barren.Land,
            subset = (momomo$Datos4.habitat.extracted.Barren.Land > 0))
subset(momomo$Datos4.habitat.extracted.Cultivated.Crops,
            subset = (momomo$Datos4.habitat.extracted.Cultivated.Crops > 0))
subset(momomo$Datos4.habitat.extracted.Deciduous.Forest,
            subset = (momomo$Datos4.habitat.extracted.Deciduous.Forest > 0))
subset(momomo$Datos4.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..High.Intensity > 0))
subset(momomo$Datos4.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$Datos4.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..Medium.Intensity > 0))
subset(momomo$Datos4.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos4.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$Datos4.habitat.extracted.Developed..Open.Space,
            subset = (momomo$Datos4.habitat.extracted.Developed..Open.Space > 0))
subset(momomo$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
subset(momomo$Datos4.habitat.extracted.Evergreen.Forest,
            subset = (momomo$Datos4.habitat.extracted.Evergreen.Forest > 0))
subset(momomo$Datos4.habitat.extracted.Hay.Pasture,
            subset = (momomo$Datos4.habitat.extracted.Hay.Pasture > 0))
subset(momomo$Datos4.habitat.extracted.Herbaceuous,
            subset = (momomo$Datos4.habitat.extracted.Herbaceuous > 0))
subset(momomo$Datos4.habitat.extracted.Open.Water,
            subset = (momomo$Datos4.habitat.extracted.Open.Water > 0))
subset(momomo$Datos4.habitat.extracted.Shrub.Scrub,
            subset = (momomo$Datos4.habitat.extracted.Shrub.Scrub > 0))
subset(momomo$Datos4.habitat.extracted.Woody.Wetlands,
            subset = (momomo$Datos4.habitat.extracted.Woody.Wetlands > 0))
sink()

#Valores de histogramas de datos sin repeticiones

jiji <-aggregate (Datos11$Group.3 ~ Datos11$Group.2, FUN = summary, Datos11)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)

meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)

row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)


sink("histogramrnumbers.txt")
subset(momomo$Datos11.Group.3.Barren.Land,
            subset = (momomo$Datos11.Group.3.Barren.Land > 0))
subset(momomo$Datos11.Group.3.Cultivated.Crops,
            subset = (momomo$Datos11.Group.3.Cultivated.Crops > 0))
subset(momomo$Datos11.Group.3.Deciduous.Forest,
            subset = (momomo$Datos11.Group.3.Deciduous.Forest > 0))
subset(momomo$Datos11.Group.3.Developed..High.Intensity,
            subset = (momomo$Datos11.Group.3.Developed..High.Intensity > 0))
subset(momomo$Datos11.Group.3.Developed..Low.Intensity,
            subset = (momomo$Datos11.Group.3.Developed..Low.Intensity > 0))
subset(momomo$Datos11.Group.3.Developed..Medium.Intensity,
            subset = (momomo$Datos11.Group.3.Developed..Medium.Intensity > 0))
subset(momomo$Datos11.Group.3.Developed..Low.Intensity,
            subset = (momomo$Datos11.Group.3.Developed..Low.Intensity > 0))
subset(momomo$Datos11.Group.3.Developed..Open.Space,
            subset = (momomo$Datos11.Group.3.Developed..Open.Space > 0))
subset(momomo$Datos11.Group.3.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$Datos11.Group.3.Emergent.Herbaceuous.Wetlands > 0))
subset(momomo$Datos11.Group.3.Evergreen.Forest,
            subset = (momomo$Datos11.Group.3.Evergreen.Forest > 0))
subset(momomo$Datos11.Group.3.Hay.Pasture,
            subset = (momomo$Datos11.Group.3.Hay.Pasture > 0))
subset(momomo$Datos11.Group.3.Herbaceuous,
            subset = (momomo$Datos11.Group.3.Herbaceuous > 0))
subset(momomo$Datos11.Group.3.Open.Water,
            subset = (momomo$Datos11.Group.3.Open.Water > 0))
subset(momomo$Datos11.Group.3.Shrub.Scrub,
            subset = (momomo$Datos11.Group.3.Shrub.Scrub > 0))
subset(momomo$Datos11.Group.3.Woody.Wetlands,
            subset = (momomo$Datos11.Group.3.Woody.Wetlands > 0))
sink()     

#invierno

#invierno
invierno<-subset(Datos4, subset=(!(jday_end>150 & jday_end<305)))
verano<-subset(Datos4, subset=(jday_end>150 & jday_end<305))
jiji <-aggregate (invierno$habitat.extracted ~ invierno$gen_sp, FUN = summary, invierno)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
View(momomo)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
View(momomo)

sink("histograminumbers.txt")
subset(momomo$invierno.habitat.extracted.Barren.Land,
            subset = (momomo$invierno.habitat.extracted.Barren.Land > 0))
subset(momomo$invierno.habitat.extracted.Cultivated.Crops,
            subset = (momomo$invierno.habitat.extracted.Cultivated.Crops > 0))
subset(momomo$invierno.habitat.extracted.Deciduous.Forest,
            subset = (momomo$invierno.habitat.extracted.Deciduous.Forest > 0))
subset(momomo$invierno.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$invierno.habitat.extracted.Developed..High.Intensity > 0))
subset(momomo$invierno.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$invierno.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$invierno.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$invierno.habitat.extracted.Developed..Medium.Intensity > 0))
subset(momomo$invierno.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$invierno.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$invierno.habitat.extracted.Developed..Open.Space,
            subset = (momomo$invierno.habitat.extracted.Developed..Open.Space > 0))
subset(momomo$invierno.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$invierno.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
subset(momomo$invierno.habitat.extracted.Evergreen.Forest,
            subset = (momomo$invierno.habitat.extracted.Evergreen.Forest > 0))
subset(momomo$invierno.habitat.extracted.Hay.Pasture,
            subset = (momomo$invierno.habitat.extracted.Hay.Pasture > 0))
subset(momomo$invierno.habitat.extracted.Herbaceuous,
            subset = (momomo$invierno.habitat.extracted.Herbaceuous > 0))
subset(momomo$invierno.habitat.extracted.Open.Water,
            subset = (momomo$invierno.habitat.extracted.Open.Water > 0))
subset(momomo$invierno.habitat.extracted.Shrub.Scrub,
            subset = (momomo$invierno.habitat.extracted.Shrub.Scrub > 0))
subset(momomo$invierno.habitat.extracted.Woody.Wetlands,
            subset = (momomo$invierno.habitat.extracted.Woody.Wetlands > 0))
sink()

jiji <-aggregate (verano$habitat.extracted ~ verano$gen_sp, FUN = summary, verano)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
View(momomo)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
View(momomo)



sink("histogramvnumbers.txt")
subset(momomo$verano.habitat.extracted.Barren.Land,
            subset = (momomo$verano.habitat.extracted.Barren.Land > 0))
subset(momomo$verano.habitat.extracted.Cultivated.Crops,
            subset = (momomo$verano.habitat.extracted.Cultivated.Crops > 0))
subset(momomo$verano.habitat.extracted.Deciduous.Forest,
            subset = (momomo$verano.habitat.extracted.Deciduous.Forest > 0))
subset(momomo$verano.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$verano.habitat.extracted.Developed..High.Intensity > 0))
subset(momomo$verano.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$verano.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$verano.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$verano.habitat.extracted.Developed..Medium.Intensity > 0))
subset(momomo$verano.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$verano.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$verano.habitat.extracted.Developed..Open.Space,
            subset = (momomo$verano.habitat.extracted.Developed..Open.Space > 0))
subset(momomo$verano.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$verano.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
subset(momomo$verano.habitat.extracted.Evergreen.Forest,
            subset = (momomo$verano.habitat.extracted.Evergreen.Forest > 0))
subset(momomo$verano.habitat.extracted.Hay.Pasture,
            subset = (momomo$verano.habitat.extracted.Hay.Pasture > 0))
subset(momomo$verano.habitat.extracted.Herbaceuous,
            subset = (momomo$verano.habitat.extracted.Herbaceuous > 0))
subset(momomo$verano.habitat.extracted.Open.Water,
            subset = (momomo$verano.habitat.extracted.Open.Water > 0))
subset(momomo$verano.habitat.extracted.Shrub.Scrub,
            subset = (momomo$verano.habitat.extracted.Shrub.Scrub > 0))
subset(momomo$verano.habitat.extracted.Woody.Wetlands,
            subset = (momomo$verano.habitat.extracted.Woody.Wetlands > 0))
sink()

#Sin singletones
jiji <-aggregate (Datos6$habitat.extracted ~ Datos6$gen_sp, FUN = summary, Datos6)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
View(momomo)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)

#Y ahora sacamos los histogramas, filtrando las especies que no aparecen en cada habitat
sink("histogramsnumbers.txt")
subset(momomo$Datos6.habitat.extracted.Barren.Land,
            subset = (momomo$Datos6.habitat.extracted.Barren.Land > 0))
subset(momomo$Datos6.habitat.extracted.Cultivated.Crops,
            subset = (momomo$Datos6.habitat.extracted.Cultivated.Crops > 0))
subset(momomo$Datos6.habitat.extracted.Deciduous.Forest,
            subset = (momomo$Datos6.habitat.extracted.Deciduous.Forest > 0))
subset(momomo$Datos6.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$Datos6.habitat.extracted.Developed..High.Intensity > 0))
subset(momomo$Datos6.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos6.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$Datos6.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$Datos6.habitat.extracted.Developed..Medium.Intensity > 0))
subset(momomo$Datos6.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos6.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$Datos6.habitat.extracted.Developed..Open.Space,
            subset = (momomo$Datos6.habitat.extracted.Developed..Open.Space > 0))
subset(momomo$Datos6.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$Datos6.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
subset(momomo$Datos6.habitat.extracted.Evergreen.Forest,
            subset = (momomo$Datos6.habitat.extracted.Evergreen.Forest > 0))
subset(momomo$Datos6.habitat.extracted.Hay.Pasture,
            subset = (momomo$Datos6.habitat.extracted.Hay.Pasture > 0))
subset(momomo$Datos6.habitat.extracted.Herbaceuous,
            subset = (momomo$Datos6.habitat.extracted.Herbaceuous > 0))
subset(momomo$Datos6.habitat.extracted.Open.Water,
            subset = (momomo$Datos6.habitat.extracted.Open.Water > 0))
subset(momomo$Datos6.habitat.extracted.Shrub.Scrub,
            subset = (momomo$Datos6.habitat.extracted.Shrub.Scrub > 0))
subset(momomo$Datos6.habitat.extracted.Woody.Wetlands,
            subset = (momomo$Datos6.habitat.extracted.Woody.Wetlands > 0))
sink()
#sin invasoras
jiji <-aggregate (Datos13$habitat.extracted ~ Datos13$gen_sp, FUN = summary, Datos13)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
View(juju)
meji<-habpref
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
View(momomo)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)

#Y ahora sacamos los histogramas, filtrando las especies que no aparecen en cada habitat
sink("histograminvnumbers.txt")
subset(momomo$Datos13.habitat.extracted.Barren.Land,
            subset = (momomo$Datos13.habitat.extracted.Barren.Land > 0))
subset(momomo$Datos13.habitat.extracted.Cultivated.Crops,
            subset = (momomo$Datos13.habitat.extracted.Cultivated.Crops > 0))
subset(momomo$Datos13.habitat.extracted.Deciduous.Forest,
            subset = (momomo$Datos13.habitat.extracted.Deciduous.Forest > 0))
subset(momomo$Datos13.habitat.extracted.Developed..High.Intensity,
            subset = (momomo$Datos13.habitat.extracted.Developed..High.Intensity > 0))
subset(momomo$Datos13.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos13.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$Datos13.habitat.extracted.Developed..Medium.Intensity,
            subset = (momomo$Datos13.habitat.extracted.Developed..Medium.Intensity > 0))
subset(momomo$Datos13.habitat.extracted.Developed..Low.Intensity,
            subset = (momomo$Datos13.habitat.extracted.Developed..Low.Intensity > 0))
subset(momomo$Datos13.habitat.extracted.Developed..Open.Space,
            subset = (momomo$Datos13.habitat.extracted.Developed..Open.Space > 0))
subset(momomo$Datos13.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (momomo$Datos13.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
subset(momomo$Datos13.habitat.extracted.Evergreen.Forest,
            subset = (momomo$Datos13.habitat.extracted.Evergreen.Forest > 0))
subset(momomo$Datos13.habitat.extracted.Hay.Pasture,
            subset = (momomo$Datos13.habitat.extracted.Hay.Pasture > 0))
subset(momomo$Datos13.habitat.extracted.Herbaceuous,
            subset = (momomo$Datos13.habitat.extracted.Herbaceuous > 0))
subset(momomo$Datos13.habitat.extracted.Open.Water,
            subset = (momomo$Datos13.habitat.extracted.Open.Water > 0))
subset(momomo$Datos13.habitat.extracted.Shrub.Scrub,
            subset = (momomo$Datos13.habitat.extracted.Shrub.Scrub > 0))
subset(momomo$Datos13.habitat.extracted.Woody.Wetlands,
            subset = (momomo$Datos13.habitat.extracted.Woody.Wetlands > 0))
sink()


momomo<10
cuantos<-c(momomo<10)
sum(replace(cuantos, cuantos == TRUE, 1))
