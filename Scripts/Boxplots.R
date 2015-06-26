#Boxplots, sacamos la electicity de nuestros datos generales

Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
Datos4

#Vamos a crear la matriz de datos de abundancia esperada para cada habitat, para ello
#primero sacamos la matriz de interacciones de nuestros datos
jiji <-aggregate (Datos4$habitat.extracted ~ Datos4$gen_sp, FUN = summary, Datos4)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)

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
matriz2<-rbind(multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador,multiplicador)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:398,]
expected<-(matriz1*matriz2)
observed<-momomo
#Sacamos electicity una vez tenemos observados y esperados
electicity <- function(obs, exp){
  p_obs <- obs/sum(obs)
  p_exp <- exp/sum(exp)
  out <- (p_obs-p_exp)/(p_obs+p_exp)
}
Electicity<-electicity(observed,expected)
View(Electicity)

boxplot(Electicity)
names(Electicity)
pdf("boxplotssin-1.pdf")
boxplot(subset(Electicity$Datos4.habitat.extracted.Barren.Land, 
       subset =(!Electicity$Datos4.habitat.extracted.Barren.Land==-1)), ylim=c(-1,1), main= "Barren Land")
boxplot(subset(Electicity$Datos4.habitat.extracted.Cultivated.Crops, 
             subset =(!Electicity$Datos4.habitat.extracted.Cultivated.Crops==-1)), ylim=c(-1,1), main= "Cultivated Crops")
boxplot(subset(Electicity$Datos4.habitat.extracted.Deciduous.Forest, 
               subset =(!Electicity$Datos4.habitat.extracted.Deciduous.Forest==-1)), ylim=c(-1,1), main= "Decoduous Forest")
boxplot(subset(Electicity$Datos4.habitat.extracted.Developed..High.Intensity, 
               subset =(!Electicity$Datos4.habitat.extracted.Developed..High.Intensity==-1)), ylim=c(-1,1), main= "Developed High Intensity")
boxplot(subset(Electicity$Datos4.habitat.extracted.Developed..Low.Intensity, 
               subset =(!Electicity$Datos4.habitat.extracted.Developed..Low.Intensity==-1)), ylim=c(-1,1), main="Developed Low Intensity")
boxplot(subset(Electicity$Datos4.habitat.extracted.Developed..Medium.Intensity, 
               subset =(!Electicity$Datos4.habitat.extracted.Developed..Medium.Intensity==-1)), ylim=c(-1,1), main= "Developed Medium Intensity")
boxplot(subset(Electicity$Datos4.habitat.extracted.Developed..Open.Space, 
               subset =(!Electicity$Datos4.habitat.extracted.Developed..Open.Space==-1)), ylim=c(-1,1), main="Developed Open Space")
boxplot(subset(Electicity$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands, 
               subset =(!Electicity$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands==-1)), ylim=c(-1,1), main="Emergent Herbaceuous Wetlands")
boxplot(subset(Electicity$Datos4.habitat.extracted.Evergreen.Forest, 
               subset =(!Electicity$Datos4.habitat.extracted.Evergreen.Forest==-1)), ylim=c(-1,1), main="Evergreen Forest")
boxplot(subset(Electicity$Datos4.habitat.extracted.Hay.Pasture, 
               subset =(!Electicity$Datos4.habitat.extracted.Hay.Pasture==-1)), ylim=c(-1,1), main="Hay Pasture")
boxplot(subset(Electicity$Datos4.habitat.extracted.Herbaceuous, 
               subset =(!Electicity$Datos4.habitat.extracted.Herbaceuous==-1)), ylim=c(-1,1), main="Herbaceuous")
boxplot(subset(Electicity$Datos4.habitat.extracted.Mixed.Forest, 
               subset =(!Electicity$Datos4.habitat.extracted.Mixed.Forest==-1)), ylim=c(-1,1), main="Mixed Forest")
boxplot(subset(Electicity$Datos4.habitat.extracted.Open.Water, 
               subset =(!Electicity$Datos4.habitat.extracted.Open.Water==-1)), ylim=c(-1,1), main="Open Water")
boxplot(subset(Electicity$Datos4.habitat.extracted.Shrub.Scrub, 
               subset =(!Electicity$Datos4.habitat.extracted.Shrub.Scrub==-1)), ylim=c(-1,1), main="Shrub Scrub")
boxplot(subset(Electicity$Datos4.habitat.extracted.Woody.Wetlands, 
               subset =(!Electicity$Datos4.habitat.extracted.Woody.Wetlands==-1)), ylim=c(-1,1), main="Woody Wetlands")
dev.off()

#Tomamos ahora los residuals del chi.test debido a que con Electicity nos aparecen
#mucha cantidad de valores "-1"

obs<-observed
exp<-expected
chi <- chisq.test(observed, p = expected, rescale.p = TRUE)
res <- chi$residuals
colnames(res)<-c("BL","CC","DF","DH","DL","DM","DO","EH","EF","HP","HB","MF","OW","SS","WW")
colnames(res)
pdf("BoxplotResiduals.pdf")
boxplot(res, main= "Residuals Chi Boxplot", names= (colnames(res)))
dev.off()
View(res)
res<-as.data.frame(res)
write.csv(res,"chiresiduals.csv")
names(res)
min(res)
max(res)
pdf("boxplotresiduals.pdf")
boxplot(res$Datos4.habitat.extracted.Barren.Land, main="Barren Land", ylab="Chi Residuals", ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Cultivated.Crops, main="Cultivated Crops", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Deciduous.Forest, main="Deciduous Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Developed..High.Intensity, main="Developed High Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Developed..Low.Intensity, main="Developed Low Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Developed..Medium.Intensity, main="Developed Medium Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Developed..Open.Space, main="Developed Open Space", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Emergent.Herbaceuous.Wetlands, main="Emergent Herbaceuous Wetlands", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Evergreen.Forest, main="Evergreen Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Hay.Pasture, main="Hay Pasture", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Herbaceuous, main="Herbaceuous", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Mixed.Forest, main="Mixed Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Open.Water, main="Open Water", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Shrub.Scrub, main="Shrub Scrub", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(res$Datos4.habitat.extracted.Woody.Wetlands, main="Woody Wetlands", ylab="Chi Residuals",ylim=c(-27,67))
dev.off()














################Comparamos los boxplots de las especies invasoras con respecto al resto

########################################################Hacemos un subset sin invasoras
Datos13<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena wilkella" & !Datos4$gen_sp == "Anthidium manicatum" & !Datos4$gen_sp == "Anthidium oblongatum" & !Datos4$gen_sp == "Hylaeus leptocephalus" & !Datos4$gen_sp == "Hylaeus punctatus" & !Datos4$gen_sp == "Lasioglossum leucozonium" & !Datos4$gen_sp == "Megachile apicalis" & !Datos4$gen_sp == "Megachile concinna" & !Datos4$gen_sp == "Megachile rotundata"  & !Datos4$gen_sp == "Megachile sculpturalis" & !Datos4$gen_sp == "Andrena" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))
jiji <-aggregate (Datos13$habitat.extracted ~ Datos13$gen_sp, FUN = summary, Datos13)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)

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
a<-multiplicador
matriz1<-cbind(rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip)
matriz1<-as.data.frame(matriz1)
nombres<-names(momomo)
names(matriz1)<-c(nombres)
nrow(momomo)
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:341,]
expected<-(matriz1*matriz2)
observed<-momomo
chinoinv <- chisq.test(observed, p = expected, rescale.p = TRUE)
resnoinv <- chinoinv$residuals
colnames(resnoinv)<-c("BL","CC","DF","DH","DL","DM","DO","EH","EF","HP","HB","MF","OW","SS","WW")
colnames(resnoinv)
boxplot(resnoinv, main= "Residuals Chi Boxplot Without Invasive Bees", names= (colnames(res)))
resnoinv<-as.data.frame(resnoinv)
write.csv(resnoinv,"chiresidualsnoinv.csv")
View(resnoinv)
pdf("boxplotresidualsnoinv.pdf")
boxplot(resnoinv$BL, main="Barren Land", ylab="Chi Residuals", ylim=c(-27,67))
boxplot(resnoinv$CC, main="Cultivated Crops", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$DF, main="Deciduous Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$DH, main="Developed High Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$DL, main="Developed Low Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$DM, main="Developed Medium Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$DO, main="Developed Open Space", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$EH, main="Emergent Herbaceuous Wetlands", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$EF, main="Evergreen Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$HP, main="Hay Pasture", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$HB, main="Herbaceuous", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$MF, main="Mixed Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$OW, main="Open Water", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$SS, main="Shrub Scrub", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resnoinv$WW, main="Woody Wetlands", ylab="Chi Residuals",ylim=c(-27,67))
dev.off()



##########################################################Y ahora uno solo de invasoras

Datosinv<-subset(Datos4, subset=(Datos4$gen_sp == "Andrena wilkella" | Datos4$gen_sp == "Anthidium manicatum" | Datos4$gen_sp == "Anthidium oblongatum" | Datos4$gen_sp == "Hylaeus leptocephalus" | Datos4$gen_sp == "Hylaeus punctatus" | Datos4$gen_sp == "Lasioglossum leucozonium" | Datos4$gen_sp == "Megachile apicalis" | Datos4$gen_sp == "Megachile concinna" | Datos4$gen_sp == "Megachile rotundata"  | Datos4$gen_sp == "Megachile sculpturalis" | Datos4$gen_sp == "Andrena" | Datos4$gen_sp == "Anthophorula micheneri" | Datos4$gen_sp == "Bombus fernaldae" | Datos4$gen_sp == "Bombus imitator" | Datos4$gen_sp == "Cemolobus ipomoeae" | Datos4$gen_sp == "Coelioxys alternata" | Datos4$gen_sp == "Coelioxys dolichos" | Datos4$gen_sp == "Coelioxys immaculata" | Datos4$gen_sp == "Colletes brevicornis" | Datos4$gen_sp == "Epeolus bifasciatus" | Datos4$gen_sp == "Lasioglossum anomalum" | Datos4$gen_sp == "Lasioglossum apopkense" | Datos4$gen_sp == "Lasioglossum asteris" | Datos4$gen_sp == "Lasioglossum curtulum" | Datos4$gen_sp == "Lasioglossum floridanum" | Datos4$gen_sp == "Lasioglossum michiganense" | Datos4$gen_sp == "Lasioglossum pectinatum" | Datos4$gen_sp == "Lasioglossum rufitarse" | Datos4$gen_sp == "Lasioglossum simplex" | Datos4$gen_sp == "Nomada affabilis" | Datos4$gen_sp == "Nomada annulata" | Datos4$gen_sp == "Nomada bethunei" | Datos4$gen_sp == "Nomada composita" | Datos4$gen_sp == "Nomada ovata" | Datos4$gen_sp == "Nomada placida" | Datos4$gen_sp == "Nomada rubicunda" | Datos4$gen_sp == "Nomada valida" | Datos4$gen_sp == "Osmia caerulescens" | Datos4$gen_sp == "Osmia inspergens" | Datos4$gen_sp == "Osmia proxima" | Datos4$gen_sp == "Osmia simillima" | Datos4$gen_sp == "Paranthidium jugatorium" | Datos4$gen_sp == "Perdita bequaerti" | Datos4$gen_sp == "Perdita gerardiae" | Datos4$gen_sp == "Pseudopanurgus andrenoides" | Datos4$gen_sp == "Pseudopanurgus nebrascensis" | Datos4$gen_sp == "Pseudopanurgus rugosus" | Datos4$gen_sp == "Sphecodes antennariae" | Datos4$gen_sp == "Sphecodes aroniae" | Datos4$gen_sp == "Sphecodes clematidis" | Datos4$gen_sp == "Sphecodes cressonii" | Datos4$gen_sp == "Sphecodes galerus" | Datos4$gen_sp == "Sphecodes heraclei" | Datos4$gen_sp == "Sphecodes johnsonii" | Datos4$gen_sp == "Stelis nitida" | Datos4$gen_sp == "Triepeolus helianthi" | Datos4$gen_sp == "Triepeolus pectoralis" | Datos4$gen_sp == "Triepeolus simplex"))
jiji <-aggregate (Datosinv$habitat.extracted ~ Datosinv$gen_sp, FUN = summary, Datosinv)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
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
a<-multiplicador
matriz1<-cbind(rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip)
matriz1<-as.data.frame(matriz1)
nombres<-names(momomo)
names(matriz1)<-c(nombres)
nrow(momomo)
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,,a,a,a,a,a,a)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:56,]
expected<-(matriz1*matriz2)
observed<-momomo
chiinv <- chisq.test(observed, p = expected, rescale.p = TRUE)
resinv <- chiinv$residuals
colnames(resinv)<-c("BL","CC","DF","DH","DL","DM","DO","EH","EF","HP","HB","MF","OW","SS","WW")
colnames(resinv)
boxplot(resinv, main= "Residuals Chi Boxplot Invasive Bees", names= (colnames(res)), ylim= c(-20,60))
resinv<-as.data.frame(resinv)
write.csv(resinv,"chiresidualsinv.csv")
pdf("boxplotresidualsinv.pdf")
boxplot(resinv$BL, main="Barren Land", ylab="Chi Residuals", ylim=c(-27,67))
boxplot(resinv$CC, main="Cultivated Crops", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$DF, main="Deciduous Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$DH, main="Developed High Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$DL, main="Developed Low Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$DM, main="Developed Medium Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$DO, main="Developed Open Space", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$EH, main="Emergent Herbaceuous Wetlands", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$EF, main="Evergreen Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$HP, main="Hay Pasture", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$HB, main="Herbaceuous", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$MF, main="Mixed Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$OW, main="Open Water", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$SS, main="Shrub Scrub", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(resinv$WW, main="Woody Wetlands", ylab="Chi Residuals",ylim=c(-27,67))
dev.off()




View(resinv)
View(resnoinv)

###########Me interesa ver si las repeticiones o los singletones cambian la preferencia
########################################################################sin singletones
Datos6<-subset(Datos4, subset=(!Datos4$gen_sp == "Andrena accepta" & !Datos4$gen_sp == "Andrena braccata" & !Datos4$gen_sp == "Andrena cragini" & !Datos4$gen_sp == "Andrena erythrogaster" & !Datos4$gen_sp == "Andrena integra" & !Datos4$gen_sp == "Andrena phaceliae" & !Datos4$gen_sp == "Andrena polemonii" & !Datos4$gen_sp == "Andrena salictaria" & !Datos4$gen_sp == "Andrena sigmundi"  & !Datos4$gen_sp == "Andrena w-scripta" & !Datos4$gen_sp == "Andrena zizeaformis" & !Datos4$gen_sp == "Anthophorula micheneri" & !Datos4$gen_sp == "Bombus fernaldae" & !Datos4$gen_sp == "Bombus imitator" & !Datos4$gen_sp == "Cemolobus ipomoeae" & !Datos4$gen_sp == "Coelioxys alternata" & !Datos4$gen_sp == "Coelioxys dolichos" & !Datos4$gen_sp == "Coelioxys immaculata" & !Datos4$gen_sp == "Colletes brevicornis" & !Datos4$gen_sp == "Epeolus bifasciatus" & !Datos4$gen_sp == "Lasioglossum anomalum" & !Datos4$gen_sp == "Lasioglossum apopkense" & !Datos4$gen_sp == "Lasioglossum asteris" & !Datos4$gen_sp == "Lasioglossum curtulum" & !Datos4$gen_sp == "Lasioglossum floridanum" & !Datos4$gen_sp == "Lasioglossum michiganense" & !Datos4$gen_sp == "Lasioglossum pectinatum" & !Datos4$gen_sp == "Lasioglossum rufitarse" & !Datos4$gen_sp == "Lasioglossum simplex" & !Datos4$gen_sp == "Nomada affabilis" & !Datos4$gen_sp == "Nomada annulata" & !Datos4$gen_sp == "Nomada bethunei" & !Datos4$gen_sp == "Nomada composita" & !Datos4$gen_sp == "Nomada ovata" & !Datos4$gen_sp == "Nomada placida" & !Datos4$gen_sp == "Nomada rubicunda" & !Datos4$gen_sp == "Nomada valida" & !Datos4$gen_sp == "Osmia caerulescens" & !Datos4$gen_sp == "Osmia inspergens" & !Datos4$gen_sp == "Osmia proxima" & !Datos4$gen_sp == "Osmia simillima" & !Datos4$gen_sp == "Paranthidium jugatorium" & !Datos4$gen_sp == "Perdita bequaerti" & !Datos4$gen_sp == "Perdita gerardiae" & !Datos4$gen_sp == "Pseudopanurgus andrenoides" & !Datos4$gen_sp == "Pseudopanurgus nebrascensis" & !Datos4$gen_sp == "Pseudopanurgus rugosus" & !Datos4$gen_sp == "Sphecodes antennariae" & !Datos4$gen_sp == "Sphecodes aroniae" & !Datos4$gen_sp == "Sphecodes clematidis" & !Datos4$gen_sp == "Sphecodes cressonii" & !Datos4$gen_sp == "Sphecodes galerus" & !Datos4$gen_sp == "Sphecodes heraclei" & !Datos4$gen_sp == "Sphecodes johnsonii" & !Datos4$gen_sp == "Stelis nitida" & !Datos4$gen_sp == "Triepeolus helianthi" & !Datos4$gen_sp == "Triepeolus pectoralis" & !Datos4$gen_sp == "Triepeolus simplex"))
jiji <-aggregate (Datos6$habitat.extracted ~ Datos6$gen_sp, FUN = summary, Datos6)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
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
a<-multiplicador
matriz1<-cbind(rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip)
matriz1<-as.data.frame(matriz1)
nombres<-names(momomo)
names(matriz1)<-c(nombres)
nrow(momomo)
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:340,]
expected<-(matriz1*matriz2)
observed<-momomo
chis <- chisq.test(observed, p = expected, rescale.p = TRUE)
ress <- chis$residuals
colnames(ress)<-c("BL","CC","DF","DH","DL","DM","DO","EH","EF","HP","HB","MF","OW","SS","WW")
colnames(resinv)
boxplot(ress, main= "Residuals Chi Boxplot Without Singletones", names= (colnames(res)), ylim= c(-20,60))
ress<-as.data.frame(ress)
pdf("Boxplotchiresidualssingletones.pdf")
boxplot(ress$BL, main="Barren Land", ylab="Chi Residuals", ylim=c(-27,67))
boxplot(ress$CC, main="Cultivated Crops", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$DF, main="Deciduous Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$DH, main="Developed High Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$DL, main="Developed Low Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$DM, main="Developed Medium Intensity", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$DO, main="Developed Open Space", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$EH, main="Emergent Herbaceuous Wetlands", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$EF, main="Evergreen Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$HP, main="Hay Pasture", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$HB, main="Herbaceuous", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$MF, main="Mixed Forest", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$OW, main="Open Water", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$SS, main="Shrub Scrub", ylab="Chi Residuals",ylim=c(-27,67))
boxplot(ress$WW, main="Woody Wetlands", ylab="Chi Residuals",ylim=c(-27,67))
dev.off()
write.csv(res,"chiresidualss.csv")


################################################Sin repeticioes en el punto de muestreo

sumcord<-(Datos4$latitude + Datos4$longitude)
Datos9<-(cbind(Datos4,sumcord))


#Aquí voy a filtrar mis datos, dejándome solo la cita de cada primera especie que me
#encuentro en cada punto de muestreo
psssss<-aggregate(Datos9, by=list(Datos9$sumcord, Datos9$gen_sp, Datos9$habitat.extracted),
                  FUN=duplicated, na.rm=TRUE)
Datos11<-psssss[3:14780,]
Datos11<-as.data.frame(Datos11)
View(Datos11)
jiji <-aggregate (Datos11$Group.3 ~ Datos11$Group.2, FUN = summary, Datos11)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
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
a<-multiplicador
matriz1<-cbind(rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip)
matriz1<-as.data.frame(matriz1)
nombres<-names(momomo)
names(matriz1)<-c(nombres)
nrow(momomo)
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:397,]
expected<-(matriz1*matriz2)
observed<-momomo
chisr <- chisq.test(observed, p = expected, rescale.p = TRUE)
ressr <- chisr$residuals
colnames(ressr)<-c("BL","CC","DF","DH","DL","DM","DO","EH","EF","HP","HB","MF","OW","SS","WW")
boxplot(ressr, main= "Residuals Chi Boxplot Without Repetitions", names= (colnames(res)), ylim= c(-20,60))
ressr<-as.data.frame(ressr)
pdf("Boxplotchiresidualsrepetitions.pdf")
boxplot(ressr$BL, main="Barren Land", ylab="Chi Residuals", ylim=c(-5,16))
boxplot(ressr$CC, main="Cultivated Crops", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$DF, main="Deciduous Forest", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$DH, main="Developed High Intensity", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$DL, main="Developed Low Intensity", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$DM, main="Developed Medium Intensity", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$DO, main="Developed Open Space", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$EH, main="Emergent Herbaceuous Wetlands", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$EF, main="Evergreen Forest", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$HP, main="Hay Pasture", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$HB, main="Herbaceuous", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$MF, main="Mixed Forest", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$OW, main="Open Water", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$SS, main="Shrub Scrub", ylab="Chi Residuals",ylim=c(-5,16))
boxplot(ressr$WW, main="Woody Wetlands", ylab="Chi Residuals",ylim=c(-5,16))
dev.off()
write.csv(res,"chiresidualssr.csv")


########################################################Boxplots para invierno / verano
######################################################################Invierno
invierno<-subset(Datos4, subset=(!(jday_end>150 & jday_end<305)))
verano<-subset(Datos4, subset=(jday_end>150 & jday_end<305))

jiji <-aggregate (invierno$habitat.extracted ~ invierno$gen_sp, FUN = summary, invierno)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
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
a<-multiplicador
matriz1<-cbind(rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip)
matriz1<-as.data.frame(matriz1)
nombres<-names(momomo)
names(matriz1)<-c(nombres)
nrow(momomo)
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:254,]
expected<-(matriz1*matriz2)
observed<-momomo
chisinv <- chisq.test(observed, p = expected, rescale.p = TRUE)
ressinv <- chisinv$residuals
colnames(ressinv)<-c("BL","CC","DF","DH","DL","DM","DO","EH","EF","HP","HB","MF","OW","SS","WW")
colnames(resinv)
boxplot(ressinv, main= "Residuals Chi Boxplot Winter", names= (colnames(res)), ylim= c(-20,60))
ressinv<-as.data.frame(ressinv)
pdf("Boxplotchiresidualswinter.pdf")
boxplot(ressinv$BL, main="Barren Land", ylab="Chi Residuals", ylim=c(-15,60))
boxplot(ressinv$CC, main="Cultivated Crops", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$DF, main="Deciduous Forest", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$DH, main="Developed High Intensity", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$DL, main="Developed Low Intensity", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$DM, main="Developed Medium Intensity", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$DO, main="Developed Open Space", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$EH, main="Emergent Herbaceuous Wetlands", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$EF, main="Evergreen Forest", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$HP, main="Hay Pasture", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$HB, main="Herbaceuous", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$MF, main="Mixed Forest", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$OW, main="Open Water", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$SS, main="Shrub Scrub", ylab="Chi Residuals",ylim=c(-15,60))
boxplot(ressinv$WW, main="Woody Wetlands", ylab="Chi Residuals",ylim=c(-15,60))
dev.off()
write.csv(res,"chiresidualsinvierno.csv")


########################################################################Verano
jiji <-aggregate (verano$habitat.extracted ~ verano$gen_sp, FUN = summary, verano)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-(1:2)]
momomo<-as.data.frame(momomo)
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
a<-multiplicador
matriz1<-cbind(rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip,rip)
matriz1<-as.data.frame(matriz1)
nombres<-names(momomo)
names(matriz1)<-c(nombres)
nrow(momomo)
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:328,]
expected<-(matriz1*matriz2)
observed<-momomo
chisver <- chisq.test(observed, p = expected, rescale.p = TRUE)
ressver <- chisver$residuals
colnames(ressver)<-c("BL","CC","DF","DH","DL","DM","DO","EH","EF","HP","HB","MF","OW","SS","WW")
boxplot(ressver, main= "Residuals Chi Boxplot Summer", names= (colnames(res)), ylim= c(-20,60))
ressver<-as.data.frame(ressver)
pdf("Boxplotchiresidualsverano.pdf")
boxplot(ressver$BL, main="Barren Land", ylab="Chi Residuals", ylim=c(-20,67))
boxplot(ressver$CC, main="Cultivated Crops", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$DF, main="Deciduous Forest", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$DH, main="Developed High Intensity", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$DL, main="Developed Low Intensity", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$DM, main="Developed Medium Intensity", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$DO, main="Developed Open Space", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$EH, main="Emergent Herbaceuous Wetlands", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$EF, main="Evergreen Forest", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$HP, main="Hay Pasture", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$HB, main="Herbaceuous", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$MF, main="Mixed Forest", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$OW, main="Open Water", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$SS, main="Shrub Scrub", ylab="Chi Residuals",ylim=c(-20,67))
boxplot(ressver$WW, main="Woody Wetlands", ylab="Chi Residuals",ylim=c(-20,67))
dev.off()

write.csv(res,"chiresidualsverano.csv")




















###############################################Ve soltando los boxplots por aquí chaval
pdf("BOXPLOTSRESIDUALS.pdf")
colnames(res)<-c("BL","CC","DF","DH","DL","DM","DO","EH","EF","HP","HB","MF","OW","SS","WW")
boxplot(res, main= "Residuals Chi Boxplot", names= (colnames(res)), ylim= c(-20,60))
boxplot(resnoinv, main= "Residuals Chi Boxplot Without Invasive Bees", names= (colnames(res)), ylim= c(-20,60))
boxplot(resinv, main= "Residuals Chi Boxplot Invasive Bees", names= (colnames(res)), ylim= c(-20,60))
boxplot(ress, main= "Residuals Chi Boxplot Without Singletones", names= (colnames(res)), ylim= c(-20,60))
boxplot(ressr, main= "Residuals Chi Boxplot Without Repetitions", names= (colnames(res)), ylim= c(-20,60))
boxplot(ressinv, main= "Residuals Chi Boxplot Winter", names= (colnames(res)), ylim= c(-20,60))
boxplot(ressver, main= "Residuals Chi Boxplot Summer", names= (colnames(res)), ylim= c(-20,60))
dev.off()



