#Sacamos dependencia (vector de fuerza para especies)
#en cada uno de nuestros tratamientos,
#lo hacemos para los datos en bruto


Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
Datos4
View(Datos4)


#tomamos una muestra para que tengan el mismo tamaño muestral todos
#Sacamos matriz de interacción
jaja <-aggregate (Datos4$habitat.extracted ~ Datos4$gen_sp, FUN = summary, Datos4)
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
jajam<-t(jejem)
colnames(jajam)<- jiji[,1]
c1<-strength(jajam, type="Bascompte")
sink("strengthspecies.txt")
c1
sink()


#tomamos una muestra para que tengan el mismo tamaño muestral todos
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
#Sacamos matriz de interacción
jaja <-aggregate (Datos5$habitat.extracted ~ Datos5$gen_sp, FUN = summary, Datos5)
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
jajam<-t(jejem)
colnames(jajam)<- jiji[,1]
c1<-strength(jajam, type="Bascompte")
sink("strengthspeciessampled.txt")
c1

View(jajam)
View(jejem)
View(jiji)


#sin invasoras





#En la librería bipartite, con el comando linklevel podemos extraer la dependencia

#aquí un ejemplo

library(bipartite)
data(Safariland)
#Si queremos la dependencia sin evaluar la abundancia de especies en dicho habitat
#podemos filtrar
Sofariland<-replace(Safariland, Safariland>1,1)
View(Sofariland)
#Contando el número de especies
View(linklevel(Safariland))
#Con presencia y ausencia solo
View(linklevel(Sofariland))

##Y ahora con mis datos

jaja <-aggregate (Datos4$habitat.extracted ~ Datos4$gen_sp, FUN = summary, Datos4)
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
row.names(jejem)<- jiji[,1]
jejem<-jejem[-1,]

jajam<-t(jejem)
presen<-replace(jajam, jajam>1,1)
#Uso el comando linklevel para extraer las dependencias
dependencias<-linklevel(jajam)
dependenciasf<-linklevel(presen)
dependencias<-as.data.frame(dependencias)
dependenciast<-as.data.frame(t(dependencias))
dependenciast<-(t(dependencias))
View(dependenciast)
#El comando linklevel crea muchas columnas locas, las retiramos
dependenciast<-dependenciast[1:398,]
dependenciast<-as.data.frame(dependenciast)



#Hacemos los histogramas, para los datos en bruto y retirando las ausencias

pdf("Dependences.pdf")
hist(dependenciast$Datos4.habitat.extracted.Barren.Land, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Barren.Land,
            subset = (dependenciast$Datos4.habitat.extracted.Barren.Land > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Barren Land only presence", xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Cultivated.Crops, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Cultivated.Crops,
            subset = (dependenciast$Datos4.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops only presence", xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Deciduous.Forest, breaks=10, 
     xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Deciduous.Forest,
            subset = (dependenciast$Datos4.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest only presence", xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Developed..High.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Developed..High.Intensity,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity only presence", xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Developed..Low.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Developed..Low.Intensity,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Developed..Medium.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Developed..Medium.Intensity,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Developed..Open.Space, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Developed..Open.Space,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (dependenciast$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Evergreen.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Evergreen.Forest,
            subset = (dependenciast$Datos4.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Hay.Pasture, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Hay.Pasture,
            subset = (dependenciast$Datos4.habitat.extracted.Hay.Pasture > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Herbaceuous, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Herbaceuous,
            subset = (dependenciast$Datos4.habitat.extracted.Herbaceuous > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Mixed.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Mixed.Forest,
            subset = (dependenciast$Datos4.habitat.extracted.Mixed.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Open.Water, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Open Water", xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Open.Water,
            subset = (dependenciast$Datos4.habitat.extracted.Open.Water > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Open Water only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Shrub.Scrub, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Shrub.Scrub,
            subset = (dependenciast$Datos4.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub only presence" , xlim=c(0,1))
hist(dependenciast$Datos4.habitat.extracted.Woody.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$Datos4.habitat.extracted.Woody.Wetlands,
            subset = (dependenciast$Datos4.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands only presence" , xlim=c(0,1))
dev.off()

#####################################################Histogramas retirando singletones
View(Datos6)
Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena accepta" & !Datos4$gen_sp == "Andrena braccata" & !Datos4$gen_sp == "Andrena cragini" & !Datos4$gen_sp == "Andrena erythrogaster" & !Datos4$gen_sp == "Andrena integra" & !Datos4$gen_sp == "Andrena phaceliae" & !Datos4$gen_sp == "Andrena polemonii" & !Datos4$gen_sp == "Andrena salictaria" & !Datos4$gen_sp == "Andrena sigmundi"  & !Datos4$gen_sp == "Andrena w-scripta" & !Datos4$gen_sp == "Andrena zizeaformis" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))


jaja <-aggregate (Datos6$habitat.extracted ~ Datos6$gen_sp, FUN = summary, Datos6)
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
row.names(jejem)<- jiji[,1]
jejem<-jejem[-1,]

jajam<-t(jejem)
presen<-replace(jajam, jajam>1,1)
#Uso el comando linklevel para extraer las dependencias
dependencias<-linklevel(jajam)
dependenciasf<-linklevel(presen)
dependencias<-as.data.frame(dependencias)
dependenciast<-as.data.frame(t(dependencias))
dependenciast<-(t(dependencias))
View(dependenciast)
#El comando linklevel crea muchas columnas locas, las retiramos
dependenciast<-dependenciast[1:340,]
dependenciast<-as.data.frame(dependenciast)


#Hacemos los histogramas, para los datos en bruto y retirando las ausencias

pdf("Dependencess.pdf")
hist(dependenciast$Datos6.habitat.extracted.Barren.Land, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Barren.Land,
            subset = (dependenciast$Datos6.habitat.extracted.Barren.Land > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Barren Land only presence", xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Cultivated.Crops, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Cultivated.Crops,
            subset = (dependenciast$Datos6.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops only presence", xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Deciduous.Forest, breaks=10, 
     xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Deciduous.Forest,
            subset = (dependenciast$Datos6.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest only presence", xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Developed..High.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Developed..High.Intensity,
            subset = (dependenciast$Datos6.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity only presence", xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Developed..Low.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Developed..Low.Intensity,
            subset = (dependenciast$Datos6.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Developed..Medium.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Developed..Medium.Intensity,
            subset = (dependenciast$Datos6.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Developed..Open.Space, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Developed..Open.Space,
            subset = (dependenciast$Datos6.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Emergent.Herbaceuous.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (dependenciast$Datos6.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands only presence" , xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Evergreen.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Evergreen.Forest,
            subset = (dependenciast$Datos6.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest only presence" , xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Hay.Pasture, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Hay.Pasture,
            subset = (dependenciast$Datos6.habitat.extracted.Hay.Pasture > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture only presence" , xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Herbaceuous, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Herbaceuous,
            subset = (dependenciast$Datos6.habitat.extracted.Herbaceuous > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous only presence" , xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Mixed.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Mixed.Forest,
            subset = (dependenciast$Datos6.habitat.extracted.Mixed.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest only presence" , xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Open.Water, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Open Water", xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Open.Water,
            subset = (dependenciast$Datos6.habitat.extracted.Open.Water > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Open Water only presence" , xlim=c(0,1))
hist(dependenciast$Datos6.habitat.extracted.Shrub.Scrub, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Shrub.Scrub,
            subset = (dependenciast$Datos6.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub only presence" , xlim=c(0,1))


hist(dependenciast$Datos6.habitat.extracted.Woody.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$Datos6.habitat.extracted.Woody.Wetlands,
            subset = (dependenciast$Datos6.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands only presence" , xlim=c(0,1))
dev.off()

########################################Sin repeticiones en el mismo punto de muestreo

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
jaja <-aggregate (Datos11$Group.3 ~ Datos11$Group.2, FUN = summary, Datos11)
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
row.names(jejem)<- jiji[,1]
jejem<-jejem[-1,]

jajam<-t(jejem)
presen<-replace(jajam, jajam>1,1)
#Uso el comando linklevel para extraer las dependencias
dependencias<-linklevel(jajam)
dependenciasf<-linklevel(presen)
dependencias<-as.data.frame(dependencias)
dependenciast<-as.data.frame(t(dependencias))
dependenciast<-(t(dependencias))
View(dependenciast)
#El comando linklevel crea muchas columnas locas, las retiramos
dependenciast<-dependenciast[1:397,]
dependenciast<-as.data.frame(dependenciast)


#Hacemos los histogramas, para los datos en bruto y retirando las ausencias

pdf("Dependencesr.pdf")
hist(dependenciast$Datos11.Group.3.Barren.Land, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Barren.Land,
            subset = (dependenciast$Datos11.Group.3.Barren.Land > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Barren Land only presence", xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Cultivated.Crops, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Cultivated.Crops,
            subset = (dependenciast$Datos11.Group.3.Cultivated.Crops > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops only presence", xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Deciduous.Forest, breaks=10, 
     xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Deciduous.Forest,
            subset = (dependenciast$Datos11.Group.3.Deciduous.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest only presence", xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Developed..High.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Developed..High.Intensity,
            subset = (dependenciast$Datos11.Group.3.Developed..High.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity only presence", xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Developed..Low.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Developed..Low.Intensity,
            subset = (dependenciast$Datos11.Group.3.Developed..Low.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Developed..Medium.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Developed..Medium.Intensity,
            subset = (dependenciast$Datos11.Group.3.Developed..Medium.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Developed..Open.Space, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Developed..Open.Space,
            subset = (dependenciast$Datos11.Group.3.Developed..Open.Space > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Emergent.Herbaceuous.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Emergent.Herbaceuous.Wetlands,
            subset = (dependenciast$Datos11.Group.3.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Evergreen.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Evergreen.Forest,
            subset = (dependenciast$Datos11.Group.3.Evergreen.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Hay.Pasture, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Hay.Pasture,
            subset = (dependenciast$Datos11.Group.3.Hay.Pasture > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Herbaceuous, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Herbaceuous,
            subset = (dependenciast$Datos11.Group.3.Herbaceuous > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Mixed.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Mixed.Forest,
            subset = (dependenciast$Datos11.Group.3.Mixed.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Open.Water, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Open Water", xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Open.Water,
            subset = (dependenciast$Datos11.Group.3.Open.Water > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Open Water only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Shrub.Scrub, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Shrub.Scrub,
            subset = (dependenciast$Datos11.Group.3.Shrub.Scrub > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub only presence" , xlim=c(0,1))
hist(dependenciast$Datos11.Group.3.Woody.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$Datos11.Group.3.Woody.Wetlands,
            subset = (dependenciast$Datos11.Group.3.Woody.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands only presence" , xlim=c(0,1))
dev.off()

########################################################################Sin invasoras

Datos13<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena wilkella" & !Datos4$gen_sp == "Anthidium manicatum" & !Datos4$gen_sp == "Anthidium oblongatum" & !Datos4$gen_sp == "Hylaeus leptocephalus" & !Datos4$gen_sp == "Hylaeus punctatus" & !Datos4$gen_sp == "Lasioglossum leucozonium" & !Datos4$gen_sp == "Megachile apicalis" & !Datos4$gen_sp == "Megachile concinna" & !Datos4$gen_sp == "Megachile rotundata"  & !Datos4$gen_sp == "Megachile sculpturalis" & !Datos4$gen_sp == "Andrena" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))

jaja <-aggregate (Datos13$habitat.extracted ~ Datos13$gen_sp, FUN = summary, Datos13)
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
row.names(jejem)<- jiji[,1]
jejem<-jejem[-1,]

jajam<-t(jejem)
presen<-replace(jajam, jajam>1,1)
#Uso el comando linklevel para extraer las dependencias
dependencias<-linklevel(jajam)
dependenciasf<-linklevel(presen)
dependencias<-as.data.frame(dependencias)
dependenciast<-as.data.frame(t(dependencias))
dependenciast<-(t(dependencias))
View(dependenciast)
#El comando linklevel crea muchas columnas locas, las retiramos
dependenciast<-dependenciast[1:341,]
dependenciast<-as.data.frame(dependenciast)


#Hacemos los histogramas, para los datos en bruto y retirando las ausencias

pdf("Dependencesi.pdf")
hist(dependenciast$Datos13.habitat.extracted.Barren.Land, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Barren.Land,
            subset = (dependenciast$Datos13.habitat.extracted.Barren.Land > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Barren Land only presence", xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Cultivated.Crops, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Cultivated.Crops,
            subset = (dependenciast$Datos13.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops only presence", xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Deciduous.Forest, breaks=10, 
     xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Deciduous.Forest,
            subset = (dependenciast$Datos13.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest only presence", xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Developed..High.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Developed..High.Intensity,
            subset = (dependenciast$Datos13.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity only presence", xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Developed..Low.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Developed..Low.Intensity,
            subset = (dependenciast$Datos13.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Developed..Medium.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Developed..Medium.Intensity,
            subset = (dependenciast$Datos13.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Developed..Open.Space, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Developed..Open.Space,
            subset = (dependenciast$Datos13.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Emergent.Herbaceuous.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (dependenciast$Datos13.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Evergreen.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Evergreen.Forest,
            subset = (dependenciast$Datos13.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Hay.Pasture, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Hay.Pasture,
            subset = (dependenciast$Datos13.habitat.extracted.Hay.Pasture > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Herbaceuous, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Herbaceuous,
            subset = (dependenciast$Datos13.habitat.extracted.Herbaceuous > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Mixed.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Mixed.Forest,
            subset = (dependenciast$Datos13.habitat.extracted.Mixed.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Open.Water, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Open Water", xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Open.Water,
            subset = (dependenciast$Datos13.habitat.extracted.Open.Water > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Open Water only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Shrub.Scrub, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Shrub.Scrub,
            subset = (dependenciast$Datos13.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub only presence" , xlim=c(0,1))
hist(dependenciast$Datos13.habitat.extracted.Woody.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$Datos13.habitat.extracted.Woody.Wetlands,
            subset = (dependenciast$Datos13.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands only presence" , xlim=c(0,1))
dev.off()

##################################################INVIERNO / VERANO##################
invierno<-subset(Datos4, subset=(!(jday_end>150 & jday_end<305)))
verano<-subset(Datos4, subset=(jday_end>150 & jday_end<305))

jaja <-aggregate (invierno$habitat.extracted ~ invierno$gen_sp, FUN = summary, invierno)
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
row.names(jejem)<- jiji[,1]
jejem<-jejem[-1,]

jajam<-t(jejem)
presen<-replace(jajam, jajam>1,1)
#Uso el comando linklevel para extraer las dependencias
dependencias<-linklevel(jajam)
dependenciasf<-linklevel(presen)
dependencias<-as.data.frame(dependencias)
dependenciast<-as.data.frame(t(dependencias))
dependenciast<-(t(dependencias))
View(dependenciast)
#El comando linklevel crea muchas columnas locas, las retiramos
dependenciast<-dependenciast[1:254,]
dependenciast<-as.data.frame(dependenciast)


#Hacemos los histogramas, para los datos en bruto y retirando las ausencias

pdf("Dependencesinv.pdf")
hist(dependenciast$invierno.habitat.extracted.Barren.Land, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Barren.Land,
            subset = (dependenciast$invierno.habitat.extracted.Barren.Land > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Barren Land only presence", xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Cultivated.Crops, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Cultivated.Crops,
            subset = (dependenciast$invierno.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops only presence", xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Deciduous.Forest, breaks=10, 
     xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Deciduous.Forest,
            subset = (dependenciast$invierno.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest only presence", xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Developed..High.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Developed..High.Intensity,
            subset = (dependenciast$invierno.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity only presence", xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Developed..Low.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Developed..Low.Intensity,
            subset = (dependenciast$invierno.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Developed..Medium.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Developed..Medium.Intensity,
            subset = (dependenciast$invierno.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Developed..Open.Space, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Developed..Open.Space,
            subset = (dependenciast$invierno.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Emergent.Herbaceuous.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (dependenciast$invierno.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Evergreen.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Evergreen.Forest,
            subset = (dependenciast$invierno.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Hay.Pasture, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Hay.Pasture,
            subset = (dependenciast$invierno.habitat.extracted.Hay.Pasture > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Herbaceuous, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Herbaceuous,
            subset = (dependenciast$invierno.habitat.extracted.Herbaceuous > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Mixed.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Mixed.Forest,
            subset = (dependenciast$invierno.habitat.extracted.Mixed.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Open.Water, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Open Water", xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Open.Water,
            subset = (dependenciast$invierno.habitat.extracted.Open.Water > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Open Water only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Shrub.Scrub, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Shrub.Scrub,
            subset = (dependenciast$invierno.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub only presence" , xlim=c(0,1))
hist(dependenciast$invierno.habitat.extracted.Woody.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$invierno.habitat.extracted.Woody.Wetlands,
            subset = (dependenciast$invierno.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands only presence" , xlim=c(0,1))
dev.off()

#Verano

jaja <-aggregate (verano$habitat.extracted ~ verano$gen_sp, FUN = summary, verano)
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
row.names(jejem)<- jiji[,1]
jejem<-jejem[-1,]

jajam<-t(jejem)
presen<-replace(jajam, jajam>1,1)
#Uso el comando linklevel para extraer las dependencias
dependencias<-linklevel(jajam)
dependenciasf<-linklevel(presen)
dependencias<-as.data.frame(dependencias)
dependenciast<-as.data.frame(t(dependencias))
dependenciast<-(t(dependencias))
View(dependenciast)
#El comando linklevel crea muchas columnas locas, las retiramos
dependenciast<-dependenciast[1:328,]
dependenciast<-as.data.frame(dependenciast)


#Hacemos los histogramas, para los datos en bruto y retirando las ausencias

pdf("Dependencesver.pdf")
hist(dependenciast$verano.habitat.extracted.Barren.Land, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Barren.Land,
            subset = (dependenciast$verano.habitat.extracted.Barren.Land > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Barren Land only presence", xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Cultivated.Crops, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Cultivated.Crops,
            subset = (dependenciast$verano.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops only presence", xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Deciduous.Forest, breaks=10, 
     xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Deciduous.Forest,
            subset = (dependenciast$verano.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest only presence", xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Developed..High.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Developed..High.Intensity,
            subset = (dependenciast$verano.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity only presence", xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Developed..Low.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Developed..Low.Intensity,
            subset = (dependenciast$verano.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Developed..Medium.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Developed..Medium.Intensity,
            subset = (dependenciast$verano.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Developed..Open.Space, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Developed..Open.Space,
            subset = (dependenciast$verano.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Emergent.Herbaceuous.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (dependenciast$verano.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Evergreen.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Evergreen.Forest,
            subset = (dependenciast$verano.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Hay.Pasture, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Hay.Pasture,
            subset = (dependenciast$verano.habitat.extracted.Hay.Pasture > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Herbaceuous, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Herbaceuous,
            subset = (dependenciast$verano.habitat.extracted.Herbaceuous > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Mixed.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Mixed.Forest,
            subset = (dependenciast$verano.habitat.extracted.Mixed.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Open.Water, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Open Water", xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Open.Water,
            subset = (dependenciast$verano.habitat.extracted.Open.Water > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Open Water only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Shrub.Scrub, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Shrub.Scrub,
            subset = (dependenciast$verano.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub only presence" , xlim=c(0,1))
hist(dependenciast$verano.habitat.extracted.Woody.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1))
hist(subset(dependenciast$verano.habitat.extracted.Woody.Wetlands,
            subset = (dependenciast$verano.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands only presence" , xlim=c(0,1))
dev.off()









