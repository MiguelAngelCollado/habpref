Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
Datos4
#En la librería bipartite, con el comando linklevel podemos extraer la dependencia

#aquí un ejemplo----

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

##Dependencias----
#Trabajamos ahora con nuestros datos, queremos la abundancia por cada habitat
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

##Dependencias con abundancia----

dependencias<-linklevel(jajam)
dependencias<-as.data.frame(dependencias)
dependenciast<-as.data.frame(t(dependencias))
dependenciast<-(t(dependencias))
View(dependenciast)
#El comando linklevel crea muchas columnas locas, las retiramos
dependenciast<-dependenciast[1:398,]
dependenciast<-as.data.frame(dependenciast)
write.csv(dependenciast, "dependenciast.csv")


#Hacemos los histogramas, para los datos en bruto y retirando las ausencias-----

par(mfrow=c(3,5))
hist(dependenciast$Datos4.habitat.extracted.Barren.Land, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Cultivated.Crops, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Deciduous.Forest, breaks=10, 
     xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Developed..High.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Developed..Low.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Developed..Medium.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Developed..Open.Space, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Open Space" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Evergreen.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Hay.Pasture, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Herbaceuous, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Mixed.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Open.Water, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Open Water", xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Shrub.Scrub, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciast$Datos4.habitat.extracted.Woody.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1), ylim=c(0,400))
dev.off()

#Dependencia solo presencia----
hist(subset(dependenciast$Datos4.habitat.extracted.Barren.Land,
            subset = (dependenciast$Datos4.habitat.extracted.Barren.Land > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Cultivated.Crops,
            subset = (dependenciast$Datos4.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Deciduous.Forest,
            subset = (dependenciast$Datos4.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Developed..High.Intensity,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Developed..Low.Intensity,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Developed..Medium.Intensity,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Developed..Open.Space,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (dependenciast$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Evergreen.Forest,
            subset = (dependenciast$Datos4.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Hay.Pasture,
            subset = (dependenciast$Datos4.habitat.extracted.Hay.Pasture > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Herbaceuous,
            subset = (dependenciast$Datos4.habitat.extracted.Herbaceuous > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Mixed.Forest,
            subset = (dependenciast$Datos4.habitat.extracted.Mixed.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Open.Water,
            subset = (dependenciast$Datos4.habitat.extracted.Open.Water > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Open Water" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Shrub.Scrub,
            subset = (dependenciast$Datos4.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciast$Datos4.habitat.extracted.Woody.Wetlands,
            subset = (dependenciast$Datos4.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1), ylim=c(0,130))

















dev.off()


  
     
####filtrando para no tener abundancia----
dependenciasp<-linklevel(presen)
dependenciasp<-as.data.frame(dependenciasp)
dependenciasp<-t(dependenciasp)
#El comando linklevel crea muchas columnas locas, las retiramos
dependenciasp<-dependenciasp[1:398,]
dependenciasp<-as.data.frame(dependenciasp)


par(mfrow=c(3,5))
hist(dependenciasp$Datos4.habitat.extracted.Barren.Land, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Cultivated.Crops, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Deciduous.Forest, breaks=10, 
     xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Developed..High.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Developed..Low.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Developed..Medium.Intensity, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Developed..Open.Space, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Evergreen.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Hay.Pasture, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Herbaceuous, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Mixed.Forest, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Open.Water, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Open Water", xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Shrub.Scrub, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1), ylim=c(0,400))
hist(dependenciasp$Datos4.habitat.extracted.Woody.Wetlands, breaks=10,
     xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1), ylim=c(0,400))


##Dependencia solo presencia----
hist(subset(dependenciasp$Datos4.habitat.extracted.Barren.Land,
            subset = (dependenciast$Datos4.habitat.extracted.Barren.Land > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Barren Land", xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Cultivated.Crops,
            subset = (dependenciast$Datos4.habitat.extracted.Cultivated.Crops > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Cultivated Crops", xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Deciduous.Forest,
            subset = (dependenciast$Datos4.habitat.extracted.Deciduous.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Deciduous Forest", xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Developed..High.Intensity,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..High.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed High Intensity", xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Developed..Low.Intensity,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..Low.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Low Intensity" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Developed..Medium.Intensity,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..Medium.Intensity > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Developed..Open.Space,
            subset = (dependenciast$Datos4.habitat.extracted.Developed..Open.Space > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Developed Medium Intensity" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands,
            subset = (dependenciast$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Emergent Herbaceuous Wetlands" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Evergreen.Forest,
            subset = (dependenciast$Datos4.habitat.extracted.Evergreen.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Evergreen Forest" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Hay.Pasture,
            subset = (dependenciast$Datos4.habitat.extracted.Hay.Pasture > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Hay Pasture" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Herbaceuous,
            subset = (dependenciast$Datos4.habitat.extracted.Herbaceuous > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Herbaceuous" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Mixed.Forest,
            subset = (dependenciast$Datos4.habitat.extracted.Mixed.Forest > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Mixed Forest" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Open.Water,
            subset = (dependenciast$Datos4.habitat.extracted.Open.Water > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Open Water" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Shrub.Scrub,
            subset = (dependenciast$Datos4.habitat.extracted.Shrub.Scrub > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Shrub Scrub" , xlim=c(0,1), ylim=c(0,130))
hist(subset(dependenciasp$Datos4.habitat.extracted.Woody.Wetlands,
            subset = (dependenciast$Datos4.habitat.extracted.Woody.Wetlands > 0))
     , breaks = 10, xlab= "Dependence", ylab= "Frecuency", main = "Woody Wetlands" , xlim=c(0,1), ylim=c(0,130))


