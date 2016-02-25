#Habitat preference without repetitions in the sampling point
#Strength y richness full data sin repeticiones en el punto de muestreo
Datos1 <- read.csv("~/Desktop/Tesis/R/habpref full data/Datos1.csv")
getwd()

#Agruamos habitats herbaceos con pastos
Datos1$habitat.extracted <- replace(Datos1$habitat.extracted, Datos1$habitat.extracted=="Hay/Pasture", "Herbaceuous")
#Agrupamos habitats como en el paper de Koh et al 2015
levels(Datos1$habitat.extracted)[levels(Datos1$habitat.extracted)=="Herbaceuous"]<-"Herbaceuous/Hay/Pasture"


#Creo un vector único de identificación del sitio y lo adjunto a mis datos
summary(Datos1$habitat.extracted)

sumcord<-(Datos1$latitude + Datos1$longitude)
Datos9<-(cbind(Datos1,sumcord))


tonto<- unique(Datos9[,c("sumcord", "gen_sp")])
identifi<-rep("SI",nrow(tonto))

Datosr<-cbind(identifi,tonto)

row<-rownames(Datosr)
Datosre<-cbind(Datosr,row)
row<-rownames(Datos1)
Datos1<-cbind(Datos1,row)
row<-Datosre$row
identifi<-Datosre$identifi
identifi<-as.data.frame(identifi)
vamos<-cbind(row,identifi)
head(Datos1)
nrow(vamos)
Datos1<-merge(Datos1,vamos)

#Retiramos singletones
Datos6<-subset(Datos1, subset=(!Datos1$gen_sp == "Triepeolus obliteratus" & 
                                 !Datos1$gen_sp == "Stelis nitida" &
                                 !Datos1$gen_sp == "Sphecodes oripimpinello" & 
                                 !Datos1$gen_sp == "Stelis nitida" & 
                                 !Datos1$gen_sp == "Sphecodes heraclei" & 
                                 !Datos1$gen_sp == "Sphecodes johnsoni" & 
                                 !Datos1$gen_sp == "Sphecodes confertus" &
                                 !Datos1$gen_sp == "Sphecodes fattigi" & 
                                 !Datos1$gen_sp == "Pseudopanurgus rugosus" & 
                                 !Datos1$gen_sp == "Pseudopanurgus andrenoides" & 
                                 !Datos1$gen_sp == "Perdita gerardiae" & 
                                 !Datos1$gen_sp == "Philanthus gibbosus" & 
                                 !Datos1$gen_sp == "Pemphredon rugifer" &
                                 !Datos1$gen_sp == "Perdita consobrina" & 
                                 !Datos1$gen_sp == "Palmodes dimidiatus" & 
                                 !Datos1$gen_sp == "Pemphredon inornata" & 
                                 !Datos1$gen_sp == "Osmia proxima" & 
                                 !Datos1$gen_sp == "osmia sandhouseae" &
                                 !Datos1$gen_sp == "Osmia gerorgica" &
                                 !Datos1$gen_sp == "Osmia michiganensis" &
                                 !Datos1$gen_sp == "Osmia collinsae" &
                                 !Datos1$gen_sp == "Osmia cordata" &
                                 !Datos1$gen_sp == "Nomada tiftonensis" &
                                 !Datos1$gen_sp == "Osmia callinsia" &
                                 !Datos1$gen_sp == "Nomada personata" &
                                 !Datos1$gen_sp == "Nomada placida" &
                                 !Datos1$gen_sp == "Nomada nasonii" &
                                 !Datos1$gen_sp == "Nomada ovata" &
                                 !Datos1$gen_sp == "Nomada leucozonium" &
                                 !Datos1$gen_sp == "Nomada luteoides" &
                                 !Datos1$gen_sp == "Megachile parallela" &
                                 !Datos1$gen_sp == "Nomada despressa" &
                                 !Datos1$gen_sp == "Lasioglossum teuglare" &
                                 !Datos1$gen_sp == "Megachile melanophaea" &
                                 !Datos1$gen_sp == "Lasioglossum llinoense" &
                                 !Datos1$gen_sp == "Lasioglossum petulans" &
                                 !Datos1$gen_sp == "Lasioglossum incompletum" &
                                 !Datos1$gen_sp == "Lasioglossum kevensi" &
                                 !Datos1$gen_sp == "Lasioglossum foveolatum" &
                                 !Datos1$gen_sp == "Lasioglossum htichensi" &
                                 !Datos1$gen_sp == "Lasioglossum curculum" &
                                 !Datos1$gen_sp == "Lasioglossum floridana" &
                                 !Datos1$gen_sp == "Lasioglossum comagenense" &
                                 !Datos1$gen_sp == "Lasioglossum coreopsis" &
                                 !Datos1$gen_sp == "Lasioglossum atwoodi" &
                                 !Datos1$gen_sp == "Lasioglossum coeruleun" &
                                 !Datos1$gen_sp == "Hylaeus verticalis" &
                                 !Datos1$gen_sp == "Lasioglossum agilis" &
                                 !Datos1$gen_sp == "Hylaeus georgicus" &
                                 !Datos1$gen_sp == "Hylaeus sparsus" &
                                 !Datos1$gen_sp == "Hylaeus georgicus" &
                                 !Datos1$gen_sp == "Hylaeus annulata" &
                                 !Datos1$gen_sp == "Hylaeus floridanus" &
                                 !Datos1$gen_sp == "Halictus mikmaqi" &
                                 !Datos1$gen_sp == "Holcopasites heliopsis" &
                                 !Datos1$gen_sp == "Eumenes crucifera" &
                                 !Datos1$gen_sp == "Eumenes reticalis" &
                                 !Datos1$gen_sp == "Crossocerus similis" &
                                 !Datos1$gen_sp == "Crossocerus nitidiventris" & 
                                 !Datos1$gen_sp == "Crossocerus elongatulus" &
                                 !Datos1$gen_sp == "Conura coxalis" &
                                 !Datos1$gen_sp == "Coelioxys sodalis" &
                                 !Datos1$gen_sp == "Coelioxys moesta" &
                                 !Datos1$gen_sp == "Coelioxys immaculata" &
                                 !Datos1$gen_sp == "Chrysis tripartita" &
                                 !Datos1$gen_sp == "Chrysis smaragdala" &
                                 !Datos1$gen_sp == "Chrysis pellucidula" &
                                 !Datos1$gen_sp == "Chrysis inaequidens" &
                                 !Datos1$gen_sp == "Chrysis dugesi" &
                                 !Datos1$gen_sp == "Chrysis conica" &
                                 !Datos1$gen_sp == "Chrysis antennalis" &
                                 !Datos1$gen_sp == "Ceratina inaequialis" &
                                 !Datos1$gen_sp == "Ceratina floridana" &
                                 !Datos1$gen_sp == "Bombus imaptiens" &
                                 !Datos1$gen_sp == "Bombus fraternus" &
                                 !Datos1$gen_sp == "bombus bimaculatus" &
                                 !Datos1$gen_sp == "Augochlorella pura" &
                                 !Datos1$gen_sp == "Anthidium oblongum" &
                                 !Datos1$gen_sp == "Anthidium notatum" &
                                 !Datos1$gen_sp == "Anthidium bombiformis" &
                                 !Datos1$gen_sp == "Andrena ziziaformis" &
                                 !Datos1$gen_sp == "Andrena sigmundi" &
                                 !Datos1$gen_sp == "Andrena salictaria" &
                                 !Datos1$gen_sp == "Andrena polemonii" &
                                 !Datos1$gen_sp == "Andrena luteoloides" &
                                 !Datos1$gen_sp == "Andrena integra" &
                                 !Datos1$gen_sp == "Andrena hitchensi" &
                                 !Datos1$gen_sp == "Andrena erythrogaster" &
                                 !Datos1$gen_sp == "Andrena craegei" &
                                 !Datos1$gen_sp == "Andrena cornifrons" &
                                 !Datos1$gen_sp == "Andrena coreopsis" &
                                 !Datos1$gen_sp == "Andrena campestris" &
                                 !Datos1$gen_sp == "Andrena calini" &
                                 !Datos1$gen_sp == "Andrena braccata" &
                                 !Datos1$gen_sp == "Andrena biscalicis" &
                                 !Datos1$gen_sp == "Andrena andrenoides" &
                                 !Datos1$gen_sp == "Andrena accepta" &
                                 !Datos1$gen_sp == "Ammophila procera" &
                                 !Datos1$gen_sp == "Triepeolus obliteratus" &
                                 !Datos1$gen_sp == "Stelis nitida" &
                                 !Datos1$gen_sp == "Sphecodes heraclei" &
                                 !Datos1$gen_sp == "Sphecodes johnsoni" &
                                 !Datos1$gen_sp == "Sphecodes confertus" &
                                 !Datos1$gen_sp == "Sphecodes fattigi" &
                                 !Datos1$gen_sp == "Pseudopanurgus rugosus" &
                                 !Datos1$gen_sp == "Sphecodes clematidis" &
                                 !Datos1$gen_sp == "Podalonia violaceipennis" &
                                 !Datos1$gen_sp == "Pseunopanurgus andrenoides" &
                                 !Datos1$gen_sp == "Xylocopa micans" &
                                 !Datos1$gen_sp == "Vespula acadica" &
                                 !Datos1$gen_sp == "Lasioglossum coreopsis" &
                                 !Datos1$gen_sp == "Dieunomia nevadensis" &
                                 !Datos1$gen_sp == "lasioglossum coreopsis" &
                                 !Datos1$gen_sp == "Ceratina inaequalis" &
                                 !Datos1$gen_sp == "Sphecodes galerus"& 
                                 !Datos1$gen_sp == "Perdita bequaerti"&
                                 !Datos1$gen_sp == "Nomada vegana"&
                                 !Datos1$gen_sp == "Nomada valida"&
                                 !Datos1$gen_sp == "Nomada rubicunda"&
                                 !Datos1$gen_sp == "Melissodes nivea"&
                                 !Datos1$gen_sp == "Lasioglossum tenax"&
                                 !Datos1$gen_sp == "Lasioglossum birkmani"&
                                 !Datos1$gen_sp == "Lasioglossum oenotherae"&
                                 !Datos1$gen_sp == "Epeolus bifasciatus"&
                                 !Datos1$gen_sp == "Lasioglossum anomalum"&
                                 !Datos1$gen_sp == "Chrysis equidens"&
                                 !Datos1$gen_sp == "Chrysis scitula"&
                                 !Datos1$gen_sp == "Bombus fernaldae"&
                                 !Datos1$gen_sp == "Anthophorula micheneri"&
                                 !Datos1$gen_sp == "Andrena clarkella"&
                                 !Datos1$gen_sp == "Chrysis angolensis"&
                                 !Datos1$gen_sp == ""))

#Extracción de matriz de interacciones y filtrado para solo usar especies con más
#de 100 individuos en nuestro muestreo

jiji <-aggregate (Datos6$habitat.extracted ~ Datos6$gen_sp, FUN = summary, Datos6)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)

meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-1]
momomo<-as.data.frame(momomo)
#Filtramos para que solo aparezcan las especies que solo tienen >100 observaciones

summmm<-rowSums(momomo)
momomosb<-cbind(momomo,summmm)
momomost<-subset(momomosb, subset=momomosb$summmm>100)
momomost<-momomost[,1:15]
observeds<-momomost
rownames(observeds)


library(bipartite)
#Con este comando extraemos 1000 matrices de interacción de esperados
nmodels<-nullmodel(observeds, N=1000, method="r2dtable")
head(observeds)

#sacamos para barren land solamente pero para cada especie el percentil 0.05 y el
#0.95
##Vamos a crear una matriz con los los cuartiles .05 y otra con los cuartiles .95
#Empezamos por Barren Land, y seguimos para el resto de habitats
menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,1]
    AsericeusBL[n]=(nmodels[[n]])[k,1]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
BLmenor<-menor
BLmayor<-mayor
BLmed<-med
BLmed<-as.data.frame(BLmed)
BLmenor<-as.data.frame(BLmenor)
BLmayor<-as.data.frame(BLmayor)
rownames(BLmenor)<-rownames(observeds)
rownames(BLmayor)<-rownames(observeds)
rownames(BLmed)<-rownames(observeds)
BLmenor
BLmed
BLmayor


#Las sacamos para Coastal y para el resto de habitats
#
for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,2]
    AsericeusBL[n]=(nmodels[[n]])[k,2]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
COmenor<-menor
COmayor<-mayor
COmed<-med
COmed<-as.data.frame(COmed)
COmenor<-as.data.frame(COmenor)
COmayor<-as.data.frame(COmayor)
rownames(COmenor)<-rownames(observeds)
rownames(COmayor)<-rownames(observeds)
rownames(COmed)<-rownames(observeds)
COmenor
COmed
COmayor

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,3]
    AsericeusBL[n]=(nmodels[[n]])[k,3]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
CCmenor<-menor
CCmayor<-mayor
CCmed<-med
CCmed<-as.data.frame(CCmed)
CCmenor<-as.data.frame(CCmenor)
CCmayor<-as.data.frame(CCmayor)
rownames(CCmenor)<-rownames(observeds)
rownames(CCmayor)<-rownames(observeds)
rownames(CCmed)<-rownames(observeds)
CCmenor
CCmayor
CCmed


menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,4]
    AsericeusBL[n]=(nmodels[[n]])[k,4]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
DFmenor<-menor
DFmayor<-mayor
DFmed<-med
DFmed<-as.data.frame(DFmed)
DFmenor<-as.data.frame(DFmenor)
DFmayor<-as.data.frame(DFmayor)
rownames(DFmenor)<-rownames(observeds)
rownames(DFmayor)<-rownames(observeds)
rownames(DFmed)<-rownames(observeds)
DFmenor
DFmayor
DFmed

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,5]
    AsericeusBL[n]=(nmodels[[n]])[k,5]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
DHmenor<-menor
DHmayor<-mayor
DHmed<-med
DHmed<-as.data.frame(DHmed)
DHmenor<-as.data.frame(DHmenor)
DHmayor<-as.data.frame(DHmayor)
rownames(DHmenor)<-rownames(observeds)
rownames(DHmayor)<-rownames(observeds)
rownames(DHmed)<-rownames(observeds)
DHmenor
DHmayor
DHmed

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,6]
    AsericeusBL[n]=(nmodels[[n]])[k,6]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
DLmenor<-menor
DLmayor<-mayor
DLmed<-med
DLmed<-as.data.frame(DLmed)
DLmenor<-as.data.frame(DLmenor)
DLmayor<-as.data.frame(DLmayor)
rownames(DLmenor)<-rownames(observeds)
rownames(DLmayor)<-rownames(observeds)
rownames(DLmed)<-rownames(observeds)
DLmenor
DLmed
DLmayor



menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,7]
    AsericeusBL[n]=(nmodels[[n]])[k,7]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
DMmenor<-menor
DMmayor<-mayor
DMmed<-med
DMmed<-as.data.frame(DMmed)
DMmenor<-as.data.frame(DMmenor)
DMmayor<-as.data.frame(DMmayor)
rownames(DMmenor)<-rownames(observeds)
rownames(DMmayor)<-rownames(observeds)
rownames(DMmed)<-rownames(observeds)
DMmenor
DMmed
DMmayor


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,8]
    AsericeusBL[n]=(nmodels[[n]])[k,8]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
DOmenor<-menor
DOmayor<-mayor
DOmed<-med
DOmed<-as.data.frame(DOmed)
DOmenor<-as.data.frame(DOmenor)
DOmayor<-as.data.frame(DOmayor)
rownames(DOmenor)<-rownames(observeds)
rownames(DOmayor)<-rownames(observeds)
rownames(DOmed)<-rownames(observeds)
DOmenor
DOmed
DOmayor


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,9]
    AsericeusBL[n]=(nmodels[[n]])[k,9]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
EHmenor<-menor
EHmayor<-mayor
EHmed<-med
EHmed<-as.data.frame(EHmed)
EHmenor<-as.data.frame(EHmenor)
EHmayor<-as.data.frame(EHmayor)
rownames(EHmenor)<-rownames(observeds)
rownames(EHmayor)<-rownames(observeds)
rownames(EHmed)<-rownames(observeds)
EHmenor
EHmed
EHmayor


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,10]
    AsericeusBL[n]=(nmodels[[n]])[k,10]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
EFmenor<-menor
EFmayor<-mayor
EFmed<-med
EFmed<-as.data.frame(EFmed)
EFmenor<-as.data.frame(EFmenor)
EFmayor<-as.data.frame(EFmayor)
rownames(EFmenor)<-rownames(observeds)
rownames(EFmayor)<-rownames(observeds)
rownames(EFmed)<-rownames(observeds)
EFmenor
EFmed
EFmayor


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,12]
    AsericeusBL[n]=(nmodels[[n]])[k,12]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
HBmenor<-menor
HBmayor<-mayor
HBmed<-med
HBmed<-as.data.frame(HBmed)
HBmenor<-as.data.frame(HBmenor)
HBmayor<-as.data.frame(HBmayor)
rownames(HBmenor)<-rownames(observeds)
rownames(HBmayor)<-rownames(observeds)
rownames(HBmed)<-rownames(observeds)
HBmenor
HBmed
HBmayor


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,13]
    AsericeusBL[n]=(nmodels[[n]])[k,13]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
MFmenor<-menor
MFmayor<-mayor
MFmed<-med
MFmed<-as.data.frame(MFmed)
MFmenor<-as.data.frame(MFmenor)
MFmayor<-as.data.frame(MFmayor)
rownames(MFmenor)<-rownames(observeds)
rownames(MFmayor)<-rownames(observeds)
rownames(MFmed)<-rownames(observeds)
MFmenor
MFmed
MFmayor


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,14]
    AsericeusBL[n]=(nmodels[[n]])[k,14]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
SSmenor<-menor
SSmayor<-mayor
SSmed<-med
SSmed<-as.data.frame(SSmed)
SSmenor<-as.data.frame(SSmenor)
SSmayor<-as.data.frame(SSmayor)
rownames(SSmenor)<-rownames(observeds)
rownames(SSmayor)<-rownames(observeds)
rownames(SSmed)<-rownames(observeds)
SSmenor
SSmed
SSmayor

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,15]
    AsericeusBL[n]=(nmodels[[n]])[k,15]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
WWmenor<-menor
WWmayor<-mayor
WWmed<-med
WWmed<-as.data.frame(WWmed)
WWmenor<-as.data.frame(WWmenor)
WWmayor<-as.data.frame(WWmayor)
rownames(WWmenor)<-rownames(observeds)
rownames(WWmayor)<-rownames(observeds)
rownames(WWmed)<-rownames(observeds)
WWmenor
WWmed
WWmayor
menor<-cbind(BLmenor,COmenor,CCmenor,DFmenor,DHmenor,DLmenor,DMmenor,DOmenor,EHmenor,EFmenor,HBmenor,MFmenor,SSmenor,WWmenor)
menor<-as.data.frame(menor)
mayor<-cbind(BLmayor,COmayor,CCmayor,DFmayor,DHmayor,DLmayor,DMmayor,DOmayor,EHmayor,EFmayor,HBmayor,MFmayor,SSmayor,WWmayor)
mayor<-as.data.frame(mayor)
med<-cbind(BLmed,COmed,CCmed,DFmed,DHmed,DLmed,DMmed,DOmed,EHmed,EFmed,HBmed,MFmed,SSmed,WWmed)
med<-as.data.frame(med)
#Los que tengan valores negativos o el 0 serán avoiders, porque significa que tendrán
#una abundancia menor al valor del cuantil .05 de la distribución de valores esperados
colnames(observeds)
observeds<-observeds[,-11]
avoiders<-(observeds-menor)
colnames(avoiders)
names(avoiders)<-c("BarrenLand", "Coastal", "Cultivated Crops" ,"DeciduousForest", "DevelopedHighIntensity", "DevelopedLowIntensity", "DevelopedMediumIntensity","DevelopedOpenSpace","EmergentHerbaceuousWetlands", "EvergreenForest","Herbaceuous/Hay/Pasture","MixedForest","ShrubScrub","WoodyWetlands")
names(observeds)<-c("BarrenLand", "Coastal", "Cultivated Crops", "DeciduousForest", "DevelopedHighIntensity", "DevelopedLowIntensity", "DevelopedMediumIntensity","DevelopedOpenSpace","EmergentHerbaceuousWetlands", "EvergreenForest","Herbaceuous/Hay/Pasture","MixedForest","ShrubScrub","WoodyWetlands")



#Los que tengan valores negativos o el 0 serán exploiters
exploiters<-(mayor-observeds)
names(exploiters)<-c("BarrenLand", "Coastal", "Cultivated Crops" ,"DeciduousForest", "DevelopedHighIntensity", "DevelopedLowIntensity", "DevelopedMediumIntensity","DevelopedOpenSpace","EmergentHerbaceuousWetlands", "EvergreenForest","Herbaceuous/Hay/Pasture","MixedForest","ShrubScrub","WoodyWetlands")

#Para evaluar la preferencia, necesitamos que las especies se distribuyan de forma amplia
#no solo en nuestro subset, por eso vamos a comprobar qué especies se distribuyen de forma
#amplia
full<-subset(sd.4, subset=(country=="USA"))

sd.8<-full
#
b<-summary(sd.8$name, maxsum=860)
#es necesario renombrar algunos casos
#"Nomada near_parva"
levels(sd.8$name)[levels(sd.8$name)=="Andrena thaspii"]<-"Andrena thaspii/carolina"
levels(sd.8$name)[levels(sd.8$name)=="Andrena carolina"]<-"Andrena thaspii/carolina"
levels(sd.8$name)[levels(sd.8$name)=="Chrysis nitidula"]<-"Chrysis nitidula/coerulans"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes cressonii"]<-"Sphecodes cressonii/atlantis"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes atlantis"]<-"Sphecodes cressonii/atlantis"
levels(sd.8$name)[levels(sd.8$name)=="Melissodes boltoniae"]<-"Melissodes boltoniae/fumosa"
levels(sd.8$name)[levels(sd.8$name)=="Melissodes fumosa"]<-"Melissodes boltoniae/fumosa"
levels(sd.8$name)[levels(sd.8$name)=="Melissodes subillata"]<-"Melissodes illata/subillata"
levels(sd.8$name)[levels(sd.8$name)=="Melissodes illata"]<-"Melissodes illata/subillata"
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus  affinis/modestus"]<-"Hylaeus affinis/modestus"
levels(sd.8$name)[levels(sd.8$name)=="Lasioglossum mitchelli"]<-"Lasioglossum mitchelli/weemsi"
levels(sd.8$name)[levels(sd.8$name)=="Lasioglossum weemsi"]<-"Lasioglossum mitchelli/weemsi"
levels(sd.8$name)[levels(sd.8$name)=="Lasioglossum versatum/rohweri"]<-"Lasioglossum rohweri/versatum"
summary(sd.8$name, maxsum=860)
#Filtramos nombres inconclusos o singletones

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
levels(sd.8$name)[levels(sd.8$name)=="Osmia corniforns"]<-"Osmia cornifrons"
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus modestus/affinis"]<-"Hylaeus affinis/modestus"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes atlantis"]<-"Sphecodes atlantis/cressonii"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes cressonii"]<-"Sphecodes atlantis/cressonii"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes cressonii/atlantis"]<-"Sphecodes atlantis/cressonii"
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus ornatus_black"]<-"Hylaeus ornatus"
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus black-ornatus"]<-"Hylaeus ornatus"
levels(sd.8$name)[levels(sd.8$name)=="Hylaeus black_ornatus"]<-"Hylaeus ornatus"
levels(sd.8$name)[levels(sd.8$name)=="Sphecodes cressonii/atlantis"]<-"Sphecodes atlantis/cressonii"

full<-sd.8
summary(full$name)
summary(Datos6$gen_sp)
# Ahora comprobamos que nuestras especies están también fuera de nuestra distribución
rownames(observeds)
#Creamos una función para ver los mapas de distribución rápidamente, en esta función necesitas----
#un data.frame con latitude, longitude y country

satellite.map<-function(aver){
  library(sp)  # classes for spatial data
  library(raster)  # grids, rasters
  library(rasterVis)  # raster visualisation
  library(maptools)
  library(rgeos)
  library(rgdal)
  library(dismo)
  library(jsonlite)
  
  locs <- subset(aver, select = c("country", "latitude", "longitude"))
  locs
  locs<-na.omit(locs)
  coordinates(locs) <- c("longitude", "latitude")  
  plot(locs)
  #Definimos la proyecci??n geogr??fica, hay que ponerle un PROJ4 espec??fico a cada zona
  #o algo as?? nuse nuse Xdxdxdzd
  crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
  proj4string(locs) <- crs.geo  # define projection system of our data
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
}

rownames(avoiders)
aver<-subset(full, subset=(name=="Xylocopa virginica"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
#Con esto vemos la distribución de la especie
satellite.map(aver)
#y Aquí vemos el número de casos que se localizan fuera de nuestra zona de estudio
nrow(averr)

aver<-subset(full, subset=(name=="Xylocopa virginica"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Ptilothrix bombiformis"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Osmia taurus"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Osmia pumila"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Osmia pumila"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)



aver<-subset(full, subset=(name=="Osmia georgica"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Osmia bucephala"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Osmia atriventris"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Nomada pygmaea"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Nomada bidentate_group"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)




aver<-subset(full, subset=(name=="Melissodes bimaculata"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)



aver<-subset(full, subset=(name=="Megachile mendica"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Megachile brevis"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)



aver<-subset(full, subset=(name=="Lasioglossum versatum"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)



aver<-subset(full, subset=(name=="Lasioglossum tegulare"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Lasioglossum pilosum"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Lasioglossum pectorale"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Lasioglossum oblongum"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Lasioglossum near_admirandum"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)



aver<-subset(full, subset=(name=="Lasioglossum imitatum"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Lasioglossum illinoense"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Lasioglossum hitchensi"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Lasioglossum cressonii"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Lasioglossum coriaceum"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Lasioglossum callidum"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Lasioglossum bruneri"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


rownames(observeds)
aver<-subset(full, subset=(name=="Hylaeus affinis/modestus"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)



aver<-subset(full, subset=(name=="Halictus rubicundus"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)
rownames(observeds)

aver<-subset(full, subset=(name=="Halictus ligatus/poeyi"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Halictus confusus"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Ceratina strenua"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Ceratina calcarata/dupla/mikmaqi"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)

nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Calliopsis andreniformis"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Bombus griseocollis"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Bombus fervidus"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Bombus bimaculatus"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Augochlorella aurata"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Augochlora pura"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Apis mellifera"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)



aver<-subset(full, subset=(name=="Andrena violae"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Andrena perplexa"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Andrena nasonii"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Andrena erigeniae"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Andrena cressonii"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)

aver<-subset(full, subset=(name=="Andrena carlini"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


aver<-subset(full, subset=(name=="Agapostemon virescens"))
averr<-subset(aver, subset=(   ( (aver$latitude)>43.0)==TRUE | 
                                 ( (aver$latitude)<35.0)==TRUE) |
                ( ((aver$longitude)>(-69.85))==TRUE) | 
                ( (aver$longitude)<(-87.55))==TRUE)
nrow(averr)
satellite.map(aver)


#Número de exploiters por habitat (42 especies de referencia)----
nrow(exploiters)
nexploiters<-NULL
for(n in 1:(ncol(exploiters))){
  nrow(subset(exploiters, subset=(exploiters[,n]<0.1)))
  nexploiters[n]<-nrow(subset(exploiters, subset=(exploiters[,n]<0.1)))}
nexploiters<-as.data.frame(nexploiters)
rownames(nexploiters)<-c("BarrenLand", "Coastal", "Cultivated Crops" ,"DeciduousForest", "DevelopedHighIntensity", "DevelopedLowIntensity", "DevelopedMediumIntensity","DevelopedOpenSpace","EmergentHerbaceuousWetlands", "EvergreenForest","HerbaceuousHayPasture","MixedForest","ShrubScrub","WoodyWetlands")
#explotadores por habitat
nexploiters
#en porcentaje
(nexploiters*100)/(nrow(exploiters))

#Número de avoiders por habitat----
navoiders<-NULL
for(n in 1:(ncol(avoiders))){
  nrow(subset(avoiders, subset=(avoiders[,n]<0.1)))
  navoiders[n]<-nrow(subset(avoiders, subset=(avoiders[,n]<0.1)))}
navoiders<-as.data.frame(navoiders)
rownames(navoiders)<-c("BarrenLand", "Coastal", "Cultivated Crops" ,"DeciduousForest", "DevelopedHighIntensity", "DevelopedLowIntensity", "DevelopedMediumIntensity","DevelopedOpenSpace","EmergentHerbaceuousWetlands", "EvergreenForest","HerbaceuousHayPasture","MixedForest","ShrubScrub","WoodyWetlands")
#explotadores por habitat
navoiders
#en porcentaje
(navoiders*100)/(nrow(avoiders))

#Así vemos mejor los exploiters y avoiders------
indifferent<-rep(nrow(exploiters),14)
nindifferent<-indifferent-navoiders-nexploiters
colnames(nindifferent)<-c("nindifferent")

PREFERENCE<-cbind(nexploiters,navoiders,nindifferent)

mean(nexploiters$nexploiters)
mean(navoiders$navoiders)
names(exploiters)

names(exploiters)<-c("BarrenLand", "Coastal", "Cultivated Crops" ,"DeciduousForest", "DevelopedHighIntensity", "DevelopedLowIntensity", "DevelopedMediumIntensity","DevelopedOpenSpace","EmergentHerbaceuousWetlands", "EvergreenForest","HerbaceuousHayPasture","MixedForest","ShrubScrub","WoodyWetlands")
names(avoiders)<-c("BarrenLand", "Coastal", "Cultivated Crops" ,"DeciduousForest", "DevelopedHighIntensity", "DevelopedLowIntensity", "DevelopedMediumIntensity","DevelopedOpenSpace","EmergentHerbaceuousWetlands", "EvergreenForest","HerbaceuousHayPasture","MixedForest","ShrubScrub","WoodyWetlands")

developedH<-subset(exploiters, subset=((exploiters$DevelopedHighIntensity<0.1)==TRUE))
developedM<-subset(exploiters, subset=((exploiters$DevelopedMediumIntensity<0.1)==TRUE))
developedL<-subset(exploiters, subset=((exploiters$DevelopedLowIntensity<0.1)==TRUE))
developedO<-subset(exploiters, subset=((exploiters$DevelopedOpenSpace<0.1)==TRUE))
CultivatedC<-subset(exploiters, subset=((exploiters$CultivatedCrops<0.1)==TRUE))
HerbaceousH<-subset(exploiters, subset=((exploiters$Her<0.1)==TRUE))

developeds<-subset(exploiters, subset=(
  (exploiters$DevelopedHighIntensity<0.1)==TRUE  | 
    (exploiters$DevelopedMediumIntensity<0.1)==TRUE|
    (exploiters$DevelopedLowIntensity<0.1)==TRUE  |
    (exploiters$DevelopedOpenSpace<0.1)==TRUE|
    #Cultivated crops no tiene
    #(exploiters$CultivatedCrops<0.1)==TRUE |
    (exploiters$HerbaceuousHay<0.1)==TRUE) ) 



D1.developed<-subset(Datos1, subset=((Datos1$habitat.extracted=="Developed, High Intensity")|(Datos1$habitat.extracted=="Developed, Medium Intensity")|(Datos1$habitat.extracted=="Developed, Low Intensity")|(Datos1$habitat.extracted=="Developed, Open Space")|(Datos1$habitat.extracted=="Cultivated Crops")|(Datos1$habitat.extracted=="Herbaceuous/Hay/Pasture")))
D1.nodeveloped<-subset(Datos1, subset=((!Datos1$habitat.extracted=="Developed, High Intensity")&(!Datos1$habitat.extracted=="Developed, Medium Intensity")&(!Datos1$habitat.extracted=="Developed, Low Intensity")&(!Datos1$habitat.extracted=="Developed, Open Space")&(!Datos1$habitat.extracted=="Cultivated Crops")&(!Datos1$habitat.extracted=="Herbaceuous/Hay/Pasture")))

summary(D1.developed$habitat.extracted)
summary(D1.nodeveloped$habitat.extracted)

a<-summary(D1.developed$gen_sp, maxsum=400)
b<-summary(D1.nodeveloped$gen_sp, maxsum=400)
#La lista de invasoras
a[["Andrena wilkella"]]
a[["Anthidium manicatum"]]
a[["Anthidium oblongatum"]]
a[["Hylaeus leptocephalus"]]
a[["Hylaeus punctatus"]]
a[["Lasioglossum leucozonium"]]
a[["Megachile apicalis"]]
a[["Megachile concinna"]]
a[["Megachile rotundata"]]
a[["Megachile sculpturalis"]]
#a[["Anthophorula micheneri"]]
#a[["Bombus fernaldae"]]
#a[["Bombus imitator"]]
#a[["Cemolobus ipomoeae"]]
#a[["Coelioxys alternata"]]
a[["Coelioxys dolichos"]]
a[["Coelioxys immaculata"]]
#a[["Colletes brevicornis"]]
#a[["Epeolus bifasciatus"]]
#a[["Lasioglossum anomalum"]]
a[["Lasioglossum apopkense"]]
#a[["Lasioglossum asteris"]]
#a[["Lasioglossum curtulum"]]
a[["Lasioglossum floridanum"]]
a[["Lasioglossum michiganense"]]
#a[["Lasioglossum pectinatum"]]
#a[["Lasioglossum rufitarse"]]
a[["Lasioglossum simplex"]]
#a[["Nomada affabilis"]]
#a[["Nomada annulata"]] 
a[["Nomada bethunei"]]
a[["Nomada composita"]]
a[["Nomada ovata"]]
#a[["Nomada placida"]]
a[["Nomada rubicunda"]]
#a[["Nomada valida"]]
#a[["Osmia caerulescens"]]
a[["Osmia inspergens"]]
#a[["Osmia proxima"]]
a[["Osmia simillima"]]
#a[["Paranthidium jugatorium"]]
a[["Perdita bequaerti"]]
#a[["Perdita gerardiae"]]
a[["Pseudopanurgus andrenoides"]]
a[["Pseudopanurgus nebrascensis"]]
#a[["Pseudopanurgus rugosus"]]
a[["Sphecodes antennariae"]]
#a[["Sphecodes aroniae"]]
#a[["Sphecodes clematidis"]]
#a[["Sphecodes cressonii"]]
#a[["Sphecodes galerus"]] 
#a[["Sphecodes heraclei"]]
#a[["Sphecodes johnsonii"]] 
#a[["Stelis nitida"]] 
#a[["Triepeolus helianthi"]]
#a[["Triepeolus pectoralis"]] 
a[["Triepeolus simplex"]]


b[["Andrena wilkella"]]
b[["Anthidium manicatum"]]
b[["Anthidium oblongatum"]]
b[["Hylaeus leptocephalus"]]
b[["Hylaeus punctatus"]]
b[["Lasioglossum leucozonium"]]
b[["Megachile apicalis"]]
b[["Megachile concinna"]]
b[["Megachile rotundata"]]
b[["Megachile sculpturalis"]]
#b[["Anthophorula micheneri"]]
#b[["Bombus fernaldae"]]
#b[["Bombus imitator"]]
#b[["Cemolobus ipomoeae"]]
#b[["Coelioxys alternata"]]
#b[["Coelioxys dolichos"]]
#b[["Coelioxys immaculata"]]
#b[["Colletes brevicornis"]]
#b[["Epeolus bifasciatus"]]
#b[["Lasioglossum anomalum"]]
b[["Lasioglossum apopkense"]]
b[["Lasioglossum asteris"]]
#b[["Lasioglossum curtulum"]]
b[["Lasioglossum floridanum"]]
b[["Lasioglossum michiganense"]]
#b[["Lasioglossum pectinatum"]]
b[["Lasioglossum rufitarse"]]
b[["Lasioglossum simplex"]]
b[["Nomada affabilis"]]
b[["Nomada annulata"]] 
#b[["Nomada bethunei"]]
b[["Nomada composita"]]
#b[["Nomada ovata"]]
#b[["Nomada placida"]]
#b[["Nomada rubicunda"]]
#b[["Nomada valida"]]
#b[["Osmia caerulescens"]]
b[["Osmia inspergens"]]
#b[["Osmia proxima"]]
#b[["Osmia simillima"]]
#b[["Paranthidium jugatorium"]]
#b[["Perdita bequaerti"]]
#b[["Perdita gerardiae"]]
#b[["Pseudopanurgus andrenoides"]]
#b[["Pseudopanurgus nebrascensis"]]
#b[["Pseudopanurgus rugosus"]]
#b[["Sphecodes antennariae"]]
b[["Sphecodes aroniae"]]
#b[["Sphecodes clematidis"]]
#b[["Sphecodes cressonii"]]
#b[["Sphecodes galerus"]] 
#b[["Sphecodes heraclei"]]
#b[["Sphecodes johnsonii"]] 
#b[["Stelis nitida"]] 
b[["Triepeolus helianthi"]]
b[["Triepeolus pectoralis"]] 
#b[["Triepeolus simplex"]]

onlydevelopds<-c(a[["Andrena wilkella"]],
                 a[["Anthidium manicatum"]],
                 a[["Anthidium oblongatum"]],
                 a[["Hylaeus leptocephalus"]],
                 a[["Hylaeus punctatus"]],
                 a[["Lasioglossum leucozonium"]],
                 a[["Megachile apicalis"]],
                 a[["Megachile concinna"]],
                 a[["Megachile rotundata"]],
                 a[["Megachile sculpturalis"]],
                 a[["Coelioxys dolichos"]],
                 a[["Coelioxys immaculata"]],
                 a[["Lasioglossum apopkense"]],
                 a[["Lasioglossum floridanum"]],
                 a[["Lasioglossum michiganense"]],
                 a[["Lasioglossum simplex"]],
                 a[["Nomada bethunei"]],
                 a[["Nomada composita"]],
                 a[["Nomada ovata"]],
                 a[["Nomada rubicunda"]],
                 a[["Sphecodes antennariae"]]
                 
)

onlynodevelopds<-c(b[["Andrena wilkella"]],
                   b[["Anthidium manicatum"]],
                   b[["Anthidium oblongatum"]],
                   b[["Hylaeus leptocephalus"]],
                   b[["Hylaeus punctatus"]],
                   b[["Lasioglossum leucozonium"]],
                   b[["Megachile apicalis"]],
                   b[["Megachile concinna"]],
                   b[["Megachile rotundata"]],
                   b[["Megachile sculpturalis"]],
                   b[["Lasioglossum apopkense"]],
                   b[["Lasioglossum asteris"]],
                   b[["Lasioglossum floridanum"]],
                   b[["Lasioglossum michiganense"]],
                   b[["Lasioglossum rufitarse"]],
                   b[["Lasioglossum simplex"]],
                   b[["Nomada affabilis"]],
                   b[["Nomada annulata"]],
                   b[["Nomada composita"]],
                   b[["Osmia inspergens"]],
                   b[["Sphecodes aroniae"]],
                   b[["Triepeolus helianthi"]],
                   b[["Triepeolus pectoralis"]] 
                   
)
onlynodevelopds<-as.data.frame(onlynodevelopds)
rownames(onlynodevelopds)<-c("Andrena wilkella",
                             "Anthidium manicatum",
                             "Anthidium oblongatum",
                             "Hylaeus leptocephalus",
                             "Hylaeus punctatus",
                             "Lasioglossum leucozonium",
                             "Megachile apicalis",
                             "Megachile concinna",
                             "Megachile rotundata",
                             "Megachile sculpturalis",
                             "Lasioglossum apopkense",
                             "Lasioglossum asteris",
                             "Lasioglossum floridanum",
                             "Lasioglossum michiganense",
                             "Lasioglossum rufitarse",
                             "Lasioglossum simplex",
                             "Nomada affabilis",
                             "Nomada annulata",
                             "Nomada composita",
                             "Osmia inspergens",
                             "Sphecodes aroniae",
                             "Triepeolus helianthi",
                             "Triepeolus pectoralis")
onlydevelopds<-as.data.frame(onlydevelopds)
nrow(onlydevelopds)
rownames(onlydevelopds)<-c("Andrena wilkella",
                           "Anthidium manicatum",
                           "Anthidium oblongatum",
                           "Hylaeus leptocephalus",
                           "Hylaeus punctatus",
                           "Lasioglossum leucozonium",
                           "Megachile apicalis",
                           "Megachile concinna",
                           "Megachile rotundata",
                           "Megachile sculpturalis",
                           "Coelioxys dolichos",
                           "Coelioxys immaculata",
                           "Lasioglossum apopkense",
                           "Lasioglossum floridanum",
                           "Lasioglossum michiganense",
                           "Lasioglossum simplex",
                           "Nomada bethunei",
                           "Nomada composita",
                           "Nomada ovata",
                           "Nomada rubicunda",
                           "Sphecodes antennariae"
                           )

#Aparecer en developed habitats o en el resto-----
nrow(Datos1)
onlydevelopds
onlynodevelopds
cbind(onlydevelopds,onlynodevelopds)
q<-summary(Datos1$habitat.extracted)
head(Datos1)
sitesonly<-subset(Datos1, subset=(duplicated(Datos1$id)==FALSE))

q<-summary(sitesonly$habitat.extracted)
q[["Cultivated Crops"]]+q[["Developed, High Intensity"]]+q[["Developed, Low Intensity"]]+q[["Developed, Open Space"]]+q[["Developed, Medium Intensity"]]+q[["Herbaceuous/Hay/Pasture"]]
q[["Barren Land"]]+q[["Coastal"]]+q[["Deciduous Forest"]]+q[["Emergent Herbaceuous Wetlands"]]+q[["Evergreen Forest"]]+q[["Mixed Forest"]]+q[["Shrub/Scrub"]]+q[["Woody Wetlands"]]  

(561*232)/848

#Habitat preference k-means-----
#Calculamos las preferencia de habitat por los grupos creados con el k-means


merger <- read.csv("~/Desktop/Tesis/R/habpref full data/merger.csv")
#Convertimos group en factor
merger$group<-as.factor(as.integer(merger$group))
str(merger)
summary(merger$gen_sp, maxsum= 465)



#retiramos singletones

merger<-subset(merger, subset=(!merger$gen_sp == "Triepeolus obliteratus" & 
                                 !merger$gen_sp == "Stelis nitida" &
                                 !merger$gen_sp == "Sphecodes oripimpinello" & 
                                 !merger$gen_sp == "Stelis nitida" & 
                                 !merger$gen_sp == "Sphecodes heraclei" & 
                                 !merger$gen_sp == "Sphecodes johnsoni" & 
                                 !merger$gen_sp == "Sphecodes confertus" &
                                 !merger$gen_sp == "Sphecodes fattigi" & 
                                 !merger$gen_sp == "Pseudopanurgus rugosus" & 
                                 !merger$gen_sp == "Pseudopanurgus andrenoides" & 
                                 !merger$gen_sp == "Perdita gerardiae" & 
                                 !merger$gen_sp == "Philanthus gibbosus" & 
                                 !merger$gen_sp == "Pemphredon rugifer" &
                                 !merger$gen_sp == "Perdita consobrina" & 
                                 !merger$gen_sp == "Palmodes dimidiatus" & 
                                 !merger$gen_sp == "Pemphredon inornata" & 
                                 !merger$gen_sp == "Osmia proxima" & 
                                 !merger$gen_sp == "osmia sandhouseae" &
                                 !merger$gen_sp == "Osmia gerorgica" &
                                 !merger$gen_sp == "Osmia michiganensis" &
                                 !merger$gen_sp == "Osmia collinsae" &
                                 !merger$gen_sp == "Osmia cordata" &
                                 !merger$gen_sp == "Nomada tiftonensis" &
                                 !merger$gen_sp == "Osmia callinsia" &
                                 !merger$gen_sp == "Nomada personata" &
                                 !merger$gen_sp == "Nomada placida" &
                                 !merger$gen_sp == "Nomada nasonii" &
                                 !merger$gen_sp == "Nomada ovata" &
                                 !merger$gen_sp == "Nomada leucozonium" &
                                 !merger$gen_sp == "Nomada luteoides" &
                                 !merger$gen_sp == "Megachile parallela" &
                                 !merger$gen_sp == "Nomada despressa" &
                                 !merger$gen_sp == "Lasioglossum teuglare" &
                                 !merger$gen_sp == "Megachile melanophaea" &
                                 !merger$gen_sp == "Lasioglossum llinoense" &
                                 !merger$gen_sp == "Lasioglossum petulans" &
                                 !merger$gen_sp == "Lasioglossum incompletum" &
                                 !merger$gen_sp == "Lasioglossum kevensi" &
                                 !merger$gen_sp == "Lasioglossum foveolatum" &
                                 !merger$gen_sp == "Lasioglossum htichensi" &
                                 !merger$gen_sp == "Lasioglossum curculum" &
                                 !merger$gen_sp == "Lasioglossum floridana" &
                                 !merger$gen_sp == "Lasioglossum comagenense" &
                                 !merger$gen_sp == "Lasioglossum coreopsis" &
                                 !merger$gen_sp == "Lasioglossum atwoodi" &
                                 !merger$gen_sp == "Lasioglossum coeruleun" &
                                 !merger$gen_sp == "Hylaeus verticalis" &
                                 !merger$gen_sp == "Lasioglossum agilis" &
                                 !merger$gen_sp == "Hylaeus georgicus" &
                                 !merger$gen_sp == "Hylaeus sparsus" &
                                 !merger$gen_sp == "Hylaeus georgicus" &
                                 !merger$gen_sp == "Hylaeus annulata" &
                                 !merger$gen_sp == "Hylaeus floridanus" &
                                 !merger$gen_sp == "Halictus mikmaqi" &
                                 !merger$gen_sp == "Holcopasites heliopsis" &
                                 !merger$gen_sp == "Eumenes crucifera" &
                                 !merger$gen_sp == "Eumenes reticalis" &
                                 !merger$gen_sp == "Crossocerus similis" &
                                 !merger$gen_sp == "Crossocerus nitidiventris" & 
                                 !merger$gen_sp == "Crossocerus elongatulus" &
                                 !merger$gen_sp == "Conura coxalis" &
                                 !merger$gen_sp == "Coelioxys sodalis" &
                                 !merger$gen_sp == "Coelioxys moesta" &
                                 !merger$gen_sp == "Coelioxys immaculata" &
                                 !merger$gen_sp == "Chrysis tripartita" &
                                 !merger$gen_sp == "Chrysis smaragdala" &
                                 !merger$gen_sp == "Chrysis pellucidula" &
                                 !merger$gen_sp == "Chrysis inaequidens" &
                                 !merger$gen_sp == "Chrysis dugesi" &
                                 !merger$gen_sp == "Chrysis conica" &
                                 !merger$gen_sp == "Chrysis antennalis" &
                                 !merger$gen_sp == "Ceratina inaequialis" &
                                 !merger$gen_sp == "Ceratina floridana" &
                                 !merger$gen_sp == "Bombus imaptiens" &
                                 !merger$gen_sp == "Bombus fraternus" &
                                 !merger$gen_sp == "bombus bimaculatus" &
                                 !merger$gen_sp == "Augochlorella pura" &
                                 !merger$gen_sp == "Anthidium oblongum" &
                                 !merger$gen_sp == "Anthidium notatum" &
                                 !merger$gen_sp == "Anthidium bombiformis" &
                                 !merger$gen_sp == "Andrena ziziaformis" &
                                 !merger$gen_sp == "Andrena sigmundi" &
                                 !merger$gen_sp == "Andrena salictaria" &
                                 !merger$gen_sp == "Andrena polemonii" &
                                 !merger$gen_sp == "Andrena luteoloides" &
                                 !merger$gen_sp == "Andrena integra" &
                                 !merger$gen_sp == "Andrena hitchensi" &
                                 !merger$gen_sp == "Andrena erythrogaster" &
                                 !merger$gen_sp == "Andrena craegei" &
                                 !merger$gen_sp == "Andrena cornifrons" &
                                 !merger$gen_sp == "Andrena coreopsis" &
                                 !merger$gen_sp == "Andrena campestris" &
                                 !merger$gen_sp == "Andrena calini" &
                                 !merger$gen_sp == "Andrena braccata" &
                                 !merger$gen_sp == "Andrena biscalicis" &
                                 !merger$gen_sp == "Andrena andrenoides" &
                                 !merger$gen_sp == "Andrena accepta" &
                                 !merger$gen_sp == "Ammophila procera" &
                                 !merger$gen_sp == "Triepeolus obliteratus" &
                                 !merger$gen_sp == "Stelis nitida" &
                                 !merger$gen_sp == "Sphecodes heraclei" &
                                 !merger$gen_sp == "Sphecodes johnsoni" &
                                 !merger$gen_sp == "Sphecodes confertus" &
                                 !merger$gen_sp == "Sphecodes fattigi" &
                                 !merger$gen_sp == "Pseudopanurgus rugosus" &
                                 !merger$gen_sp == "Sphecodes clematidis" &
                                 !merger$gen_sp == "Podalonia violaceipennis" &
                                 !merger$gen_sp == "Pseunopanurgus andrenoides" &
                                 !merger$gen_sp == "Xylocopa micans" &
                                 !merger$gen_sp == "Vespula acadica" &
                                 !merger$gen_sp == "Lasioglossum coreopsis" &
                                 !merger$gen_sp == "Dieunomia nevadensis" &
                                 !merger$gen_sp == "lasioglossum coreopsis" &
                                 !merger$gen_sp == "Ceratina inaequalis" &
                                 !merger$gen_sp == "Sphecodes galerus"& 
                                 !merger$gen_sp == "Perdita bequaerti"&
                                 !merger$gen_sp == "Nomada vegana"&
                                 !merger$gen_sp == "Nomada valida"&
                                 !merger$gen_sp == "Nomada rubicunda"&
                                 !merger$gen_sp == "Melissodes nivea"&
                                 !merger$gen_sp == "Lasioglossum tenax"&
                                 !merger$gen_sp == "Lasioglossum birkmani"&
                                 !merger$gen_sp == "Lasioglossum oenotherae"&
                                 !merger$gen_sp == "Epeolus bifasciatus"&
                                 !merger$gen_sp == "Lasioglossum anomalum"&
                                 !merger$gen_sp == "Chrysis equidens"&
                                 !merger$gen_sp == "Chrysis scitula"&
                                 !merger$gen_sp == "Bombus fernaldae"&
                                 !merger$gen_sp == "Anthophorula micheneri"&
                                 !merger$gen_sp == "Andrena clarkella"&
                                 !merger$gen_sp == "Chrysis angolensis"&
                                 !merger$gen_sp == ""))


#Ahora retiramos las repeticiones en el punto de muestreo
names(merger)

sumcord<-(merger$latitude + merger$longitude)
merger<-(cbind(merger,sumcord))

tonto<- unique(merger[,c("sumcord", "gen_sp")])
identifi<-rep("SI",nrow(tonto))

Datosr<-cbind(identifi,tonto)

row<-rownames(Datosr)
Datosre<-cbind(Datosr,row)
row<-rownames(merger)
merger<-cbind(merger,row)
row<-Datosre$row
identifi<-Datosre$identifi
identifi<-as.data.frame(identifi)
vamos<-cbind(row,identifi)
head(merger)
nrow(vamos)
merger<-merge(merger,vamos)

#Comprobamos que no queden singletones
summary(mergers$gen_sp, maxsum=463)


nrow(merger)
nrow(mergers)
#Extracción de matriz de interacciones y filtrado para solo usar especies con más
#de 100 individuos en nuestro muestreo

jiji <-aggregate (merger$group ~ merger$gen_sp, FUN = summary, merger)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-1]
momomo<-as.data.frame(momomo)
#Filtramos para que solo aparezcan las especies que solo tienen >100 observaciones

summmm<-rowSums(momomo)
momomosb<-cbind(momomo,summmm)
momomost<-subset(momomosb, subset=momomosb$summmm>100)
momomost<-momomost[,1:20]
observeds<-momomost
rownames(observeds)
nrow(observeds)

library(bipartite)
#Con este comando extraemos 1000 matrices de interacción de esperados
nmodels<-nullmodel(observeds, N=1000, method="r2dtable")
head(observeds)







menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,1]
    AsericeusBL[n]=(nmodels[[n]])[k,1]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor1<- menor
mayor1<-mayor
med1<-med
med1<-as.data.frame(med1)
menor1<-as.data.frame(menor1)
mayor1<-as.data.frame(mayor1)
rownames(menor1)<-rownames(observeds)
rownames(mayor1)<-rownames(observeds)
rownames(med1)<-rownames(observeds)
menor1
med1
mayor1


#Las sacamos para el resto de grupos
#
menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,2]
    AsericeusBL[n]=(nmodels[[n]])[k,2]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor2<-menor
mayor2<-mayor
med2<-med
med2<-as.data.frame(med2)
menor2<-as.data.frame(menor2)
mayor2<-as.data.frame(mayor2)
rownames(menor2)<-rownames(observeds)
rownames(mayor2)<-rownames(observeds)
rownames(med2)<-rownames(observeds)
menor2
med2
mayor2

#

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,3]
    AsericeusBL[n]=(nmodels[[n]])[k,3]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor3<-menor
mayor3<-mayor
med3<-med
med3<-as.data.frame(med3)
menor3<-as.data.frame(menor3)
mayor3<-as.data.frame(mayor3)
rownames(menor3)<-rownames(observeds)
rownames(mayor3)<-rownames(observeds)
rownames(med3)<-rownames(observeds)
menor3
mayor3
med3


menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,4]
    AsericeusBL[n]=(nmodels[[n]])[k,4]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor4<-menor
mayor4<-mayor
med4<-med
med4<-as.data.frame(med4)
menor4<-as.data.frame(menor4)
mayor4<-as.data.frame(mayor4)
rownames(menor4)<-rownames(observeds)
rownames(mayor4)<-rownames(observeds)
rownames(med4)<-rownames(observeds)
menor4
mayor4
med4

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,5]
    AsericeusBL[n]=(nmodels[[n]])[k,5]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor5<-menor
mayor5<-mayor
med5<-med
med5<-as.data.frame(med5)
menor5<-as.data.frame(menor5)
mayor5<-as.data.frame(mayor5)
rownames(menor5)<-rownames(observeds)
rownames(mayor5)<-rownames(observeds)
rownames(med5)<-rownames(observeds)
menor5
mayor5
med5

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,6]
    AsericeusBL[n]=(nmodels[[n]])[k,6]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor6<-menor
mayor6<-mayor
med6<-med
med6<-as.data.frame(med6)
menor6<-as.data.frame(menor6)
mayor6<-as.data.frame(mayor6)
rownames(menor6)<-rownames(observeds)
rownames(mayor6)<-rownames(observeds)
rownames(med6)<-rownames(observeds)
menor6
med6
mayor6



menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,7]
    AsericeusBL[n]=(nmodels[[n]])[k,7]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor7<-menor
mayor7<-mayor
med7<-med
med7<-as.data.frame(med7)
menor7<-as.data.frame(menor7)
mayor7<-as.data.frame(mayor7)
rownames(menor7)<-rownames(observeds)
rownames(mayor7)<-rownames(observeds)
rownames(med7)<-rownames(observeds)
menor7
med7
mayor7

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,8]
    AsericeusBL[n]=(nmodels[[n]])[k,8]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor8<-menor
mayor8<-mayor
med8<-med
med8<-as.data.frame(med8)
menor8<-as.data.frame(menor8)
mayor8<-as.data.frame(mayor8)
rownames(menor8)<-rownames(observeds)
rownames(mayor8)<-rownames(observeds)
rownames(med8)<-rownames(observeds)
menor8
med8
mayor8

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,9]
    AsericeusBL[n]=(nmodels[[n]])[k,9]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor9<-menor
mayor9<-mayor
med9<-med
med9<-as.data.frame(med9)
menor9<-as.data.frame(menor9)
mayor9<-as.data.frame(mayor9)
rownames(menor9)<-rownames(observeds)
rownames(mayor9)<-rownames(observeds)
rownames(med9)<-rownames(observeds)
menor9
med9
mayor9


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,10]
    AsericeusBL[n]=(nmodels[[n]])[k,10]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor10<-menor
mayor10<-mayor
med10<-med
med10<-as.data.frame(med10)
menor10<-as.data.frame(menor10)
mayor10<-as.data.frame(mayor10)
rownames(menor10)<-rownames(observeds)
rownames(mayor10)<-rownames(observeds)
rownames(med10)<-rownames(observeds)
menor10
med10
mayor10

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,11]
    AsericeusBL[n]=(nmodels[[n]])[k,11]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor11<-menor
mayor11<-mayor
med11<-med
med11<-as.data.frame(med11)
menor11<-as.data.frame(menor11)
mayor11<-as.data.frame(mayor11)
rownames(menor11)<-rownames(observeds)
rownames(mayor11)<-rownames(observeds)
rownames(med11)<-rownames(observeds)
menor11
med11
mayor11

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,12]
    AsericeusBL[n]=(nmodels[[n]])[k,12]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor12<-menor
mayor12<-mayor
med12<-med
med12<-as.data.frame(med12)
menor12<-as.data.frame(menor12)
mayor12<-as.data.frame(mayor12)
rownames(menor12)<-rownames(observeds)
rownames(mayor12)<-rownames(observeds)
rownames(med12)<-rownames(observeds)
menor12
med12
mayor12

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,13]
    AsericeusBL[n]=(nmodels[[n]])[k,13]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor13<-menor
mayor13<-mayor
med13<-med
med13<-as.data.frame(med13)
menor13<-as.data.frame(menor13)
mayor13<-as.data.frame(mayor13)
rownames(menor13)<-rownames(observeds)
rownames(mayor13)<-rownames(observeds)
rownames(med13)<-rownames(observeds)
menor13
med13
mayor13

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL


for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,14]
    AsericeusBL[n]=(nmodels[[n]])[k,14]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor14<-menor
mayor14<-mayor
med14<-med
med14<-as.data.frame(med14)
menor14<-as.data.frame(menor14)
mayor14<-as.data.frame(mayor14)
rownames(menor14)<-rownames(observeds)
rownames(mayor14)<-rownames(observeds)
rownames(med14)<-rownames(observeds)
menor14
med14
mayor14

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,15]
    AsericeusBL[n]=(nmodels[[n]])[k,15]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor15<-menor
mayor15<-mayor
med15<-med
med15<-as.data.frame(med15)
menor15<-as.data.frame(menor15)
mayor15<-as.data.frame(mayor15)
rownames(menor15)<-rownames(observeds)
rownames(mayor15)<-rownames(observeds)
rownames(med15)<-rownames(observeds)
menor15
med15
mayor15

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,16]
    AsericeusBL[n]=(nmodels[[n]])[k,16]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor16<-menor
mayor16<-mayor
med16<-med
med16<-as.data.frame(med16)
menor16<-as.data.frame(menor16)
mayor16<-as.data.frame(mayor16)
rownames(menor16)<-rownames(observeds)
rownames(mayor16)<-rownames(observeds)
rownames(med16)<-rownames(observeds)
menor16
med16
mayor16

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL



for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,17]
    AsericeusBL[n]=(nmodels[[n]])[k,17]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor17<-menor
mayor17<-mayor
med17<-med
med17<-as.data.frame(med17)
menor17<-as.data.frame(menor17)
mayor17<-as.data.frame(mayor17)
rownames(menor17)<-rownames(observeds)
rownames(mayor17)<-rownames(observeds)
rownames(med17)<-rownames(observeds)
menor17
med17
mayor17

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,18]
    AsericeusBL[n]=(nmodels[[n]])[k,18]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor18<-menor
mayor18<-mayor
med18<-med
med18<-as.data.frame(med18)
menor18<-as.data.frame(menor18)
mayor18<-as.data.frame(mayor18)
rownames(menor18)<-rownames(observeds)
rownames(mayor18)<-rownames(observeds)
rownames(med18)<-rownames(observeds)
menor18
med18
mayor18

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,19]
    AsericeusBL[n]=(nmodels[[n]])[k,19]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor19<-menor
mayor19<-mayor
med19<-med
med19<-as.data.frame(med19)
menor19<-as.data.frame(menor19)
mayor19<-as.data.frame(mayor19)
rownames(menor19)<-rownames(observeds)
rownames(mayor19)<-rownames(observeds)
rownames(med19)<-rownames(observeds)
menor19
med19
mayor19

menor=NULL
mayor=NULL
med=NULL
AsericeusBL=NULL

for(k in 1:(nrow(observeds))){
  for(n in 1:1000){
    (nmodels[[n]])[k,20]
    AsericeusBL[n]=(nmodels[[n]])[k,20]
  }
  AsericeusBL
  menor[k]=quantile(AsericeusBL, c(.05))
  mayor[k]=quantile(AsericeusBL, c(.95))
  med[k]=quantile(AsericeusBL, c(.50))
}
menor20<-menor
mayor20<-mayor
med20<-med
med20<-as.data.frame(med20)
menor20<-as.data.frame(menor20)
mayor20<-as.data.frame(mayor20)
rownames(menor20)<-rownames(observeds)
rownames(mayor20)<-rownames(observeds)
rownames(med20)<-rownames(observeds)
menor20
med20
mayor20

menork<-cbind(menor1,menor2,menor3,menor4,menor5,menor6,menor7,menor8,menor9,menor10,menor11,menor12,menor13,menor14,menor15,menor16,menor17,menor18,menor19,menor20)
menork<-as.data.frame(menork)
nrow(menork)
mayork<-cbind(mayor1,mayor2,mayor3,mayor4,mayor5,mayor6,mayor7,mayor8,mayor9,mayor10,mayor11,mayor12,mayor13,mayor14,mayor15,mayor16,mayor17,mayor18,mayor19,mayor20)
mayork<-as.data.frame(mayork)
medk<-cbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,med12,med13,med14,med15,med16,med17,med18,med19,med20)
medk<-as.data.frame(medk)

#Los que tengan valores negativos o el 0 serán avoiders, porque significa que tendrán
#una abundancia menor al valor del cuantil .05 de la distribución de valores esperados
nrow(observeds)
avoidersk<-(observeds-menork)
nrow(avoidersk)
names(avoidersk)<-c("G1", "G2", "G3", "G4", "G5", "G6","G7","G8", "G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20")
names(observeds)<-c("G1", "G2", "G3", "G4", "G5", "G6","G7","G8", "G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20")


#Los que tengan valores negativos o el 0 serán exploiters
exploitersk<-(mayork-observeds)
names(exploitersk)<-c("G1", "G2", "G3", "G4", "G5", "G6","G7","G8", "G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20")





#Número de exploiters por habitat----
avoidersk
exploitersk
nrow(exploitersk)

nexploitersk<-NULL
for(n in 1:(ncol(exploitersk))){
  nrow(subset(exploitersk, subset=(exploitersk[,n]<0.1)))
  nexploitersk[n]<-nrow(subset(exploitersk, subset=(exploitersk[,n]<0.1)))}
nexploitersk<-as.data.frame(nexploitersk)
rownames(nexploitersk)<-c("G1", "G2", "G3", "G4", "G5", "G6","G7","G8", "G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20")

#explotadores por habitat
nexploitersk
#en porcentaje
(nexploitersk*100)/(nrow(exploitersk))

#Número de avoiders por habitat----
navoidersk<-NULL
for(n in 1:(ncol(avoidersk))){
  nrow(subset(avoidersk, subset=(avoidersk[,n]<0.1)))
  navoidersk[n]<-nrow(subset(avoidersk, subset=(avoidersk[,n]<0.1)))}
navoidersk<-as.data.frame(navoidersk)
rownames(navoidersk)<-c("G1", "G2", "G3", "G4", "G5", "G6","G7","G8", "G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20")

#explotadores por habitat
navoidersk
#en porcentaje
(navoidersk*100)/(nrow(avoidersk))

#Así vemos mejor los exploiters y avoiders------
indifferentk<-rep(42,20)
nindifferentk<-indifferentk-navoidersk-nexploitersk
colnames(nindifferentk)<-c("nindifferent")

PREFERENCE.k<-cbind(nexploitersk,navoidersk,nindifferentk)




mean(nexploitersk$nexploitersk)
mean(navoidersk$navoidersk)
nmbrs<-c("G4","G9","G20","G10","G18","G12","G2","G5","G11","G14","G13","G19","G16","G8","G17","G1","G6","G7","G15","G3")
boxplot(nexploitersk[4,],nexploitersk[9,],
        nexploitersk[20,],nexploitersk[10,],
        nexploitersk[18,],nexploitersk[12,],
        nexploitersk[2,],nexploitersk[5,],
        nexploitersk[11,],nexploitersk[14,],
        nexploitersk[13,],nexploitersk[19,],
        nexploitersk[16,],nexploitersk[8,],
        nexploitersk[17,],nexploitersk[1,],
        nexploitersk[6,],nexploitersk[7,],
        nexploitersk[15,],nexploitersk[3,], names=nmbrs)

PREFERENCE
PREFERENCE.k

#Sacamos porcentaje de invasoras de habitats developed y no developed
int.mat<-as.data.frame(int.mat)
colnames(int.mat)
int.mat.d<-cbind(int.mat$Datos6.habitat.extracted.Developed..High.Intensity,int.mat$Datos6.habitat.extracted.Developed..Medium.Intensity,int.mat$Datos6.habitat.extracted.Developed..Low.Intensity,int.mat$Datos6.habitat.extracted.Cultivated.Crops,int.mat$Datos6.habitat.extracted.Herbaceuous.Hay.Pasture,int.mat$Datos6.habitat.extracted.Developed..Open.Space)
rownames(int.mat.d)<-rownames(int.mat)
colnames(int.mat.d)<-c("DevelopedHigh","DevelopedMedium","DevelopedLow","CultivatedCrops","HerbaceuousHayPasture","DevelopedOpen")
int.mat.d
d<-rowSums(int.mat.d)



int.mat.n<-cbind(int.mat$Datos6.habitat.extracted.Barren.Land,int.mat$Datos6.habitat.extracted.Coastal,int.mat$Datos6.habitat.extracted.Deciduous.Forest,int.mat$Datos6.habitat.extracted.Emergent.Herbaceuous.Wetlands,int.mat$Datos6.habitat.extracted.Evergreen.Forest,int.mat$Datos6.habitat.extracted.Mixed.Forest,int.mat$Datos6.habitat.extracted.Shrub.Scrub,int.mat$Datos6.habitat.extracted.Woody.Wetlands)
rownames(int.mat.n)<-rownames(int.mat)
colnames(int.mat.n)<-c("BarrenLand","Coastal","DeciduousForest","EmergentHerbaceuousWetlands","EvergreenForest","MixedForest","Shrub/Scrub","WoodyWetlands")
n<-rowSums(int.mat.n)
nrow(int.mat)

d[["Andrena wilkella"]]
d[["Anthidium manicatum"]]
d[["Anthidium oblongatum"]]
d[["Hylaeus leptocephalus"]]
d[["Hylaeus punctatus"]]
d[["Lasioglossum leucozonium"]]
d[["Megachile apicalis"]]
d[["Megachile concinna"]]
d[["Megachile rotundata"]]
d[["Megachile sculpturalis"]]
d[["Coelioxys dolichos"]]
d[["Colletes brevicornis"]]
d[["Lasioglossum apopkense"]]
d[["Lasioglossum floridanum"]]
d[["Lasioglossum rufitarse"]]
d[["Lasioglossum simplex"]]
d[["Nomada bethunei"]]
d[["Nomada composita"]]
d[["Osmia inspergens"]]
d[["Lasioglossum michiganense"]]
d[["Osmia simillima"]]
d[["Pseudopanurgus nebrascensis"]]
d[["Sphecodes aroniae"]]
d[["Triepeolus simplex"]]
#24 species en developed



n[["Andrena wilkella"]]
n[["Anthidium manicatum"]]
n[["Anthidium oblongatum"]]
n[["Hylaeus leptocephalus"]]
n[["Hylaeus punctatus"]]
n[["Lasioglossum leucozonium"]]
n[["Megachile apicalis"]]
n[["Megachile concinna"]]
n[["Megachile rotundata"]]
n[["Megachile sculpturalis"]]
n[["Colletes brevicornis"]]
n[["Lasioglossum apopkense"]]
n[["Lasioglossum floridanum"]]
n[["Lasioglossum rufitarse"]]
n[["Lasioglossum simplex"]]
n[["Nomada bethunei"]]
n[["Nomada composita"]]
n[["Osmia inspergens"]]
n[["Lasioglossum michiganense"]]
n[["Osmia simillima"]]
n[["Pseudopanurgus nebrascensis"]]
n[["Sphecodes aroniae"]]
n[["Triepeolus simplex"]]
n[["Triepeolus helianthi"]]
n[["Triepeolus pectoralis"]]
n[["Paranthidium jugatorium"]]
n[["Osmia caerulescens"]]
n[["Nomada annulata"]]
n[["Nomada affabilis"]]
n[["Lasioglossum pectinatum"]]
n[["Coelioxys alternata"]]
n[["Cemolobus ipomoeae"]]
#32 especies invasoras en non-developed

#Porcentaje de especies invasoras en habitats desarrollados y no desarrollados
d<-as.data.frame(d)
nrow(d)
d<-subset(d, subset=(d>0))
#Porcentaje de especies invasoras en habitats desarrollados
(24/nrow(d))*100

n<-as.data.frame(n)
nrow(n)
n<-subset(n, subset=(n>0))
#Porcentaje de especies invasoras en habitats no desarrollados
(32/nrow(n))*100




#número de individuos invasores en habitats desarrollados
dd<-d[["Andrena wilkella"]]+d[["Anthidium manicatum"]]+d[["Anthidium oblongatum"]]+d[["Hylaeus leptocephalus"]]+d[["Hylaeus punctatus"]]+d[["Lasioglossum leucozonium"]]+d[["Megachile apicalis"]]+d[["Megachile concinna"]]+d[["Megachile rotundata"]]+d[["Megachile sculpturalis"]]+d[["Coelioxys dolichos"]]+d[["Colletes brevicornis"]]+d[["Lasioglossum apopkense"]]+d[["Lasioglossum floridanum"]]+d[["Lasioglossum rufitarse"]]+d[["Lasioglossum simplex"]]+d[["Nomada bethunei"]]+d[["Nomada composita"]]+d[["Osmia inspergens"]]+d[["Lasioglossum michiganense"]]+d[["Osmia simillima"]]+d[["Pseudopanurgus nebrascensis"]]+d[["Sphecodes aroniae"]]+d[["Triepeolus simplex"]]
(dd/sum(colSums(int.mat.d)))*100 

#número de individuos invasores en habitats no desarrollados

nn<-n[["Andrena wilkella"]]+n[["Anthidium manicatum"]]+n[["Anthidium oblongatum"]]+n[["Hylaeus leptocephalus"]]+n[["Hylaeus punctatus"]]+n[["Lasioglossum leucozonium"]]+n[["Megachile apicalis"]]+n[["Megachile concinna"]]+n[["Megachile rotundata"]]+n[["Megachile sculpturalis"]]+n[["Colletes brevicornis"]]+n[["Lasioglossum apopkense"]]+n[["Lasioglossum floridanum"]]+n[["Lasioglossum rufitarse"]]+n[["Lasioglossum simplex"]]+n[["Nomada bethunei"]]+n[["Nomada composita"]]+n[["Osmia inspergens"]]+n[["Lasioglossum michiganense"]]+n[["Osmia simillima"]]+n[["Pseudopanurgus nebrascensis"]]+n[["Sphecodes aroniae"]]+n[["Triepeolus simplex"]]+n[["Triepeolus helianthi"]]+n[["Triepeolus pectoralis"]]+n[["Paranthidium jugatorium"]]+n[["Osmia caerulescens"]]+n[["Nomada annulata"]]+n[["Nomada affabilis"]]+n[["Lasioglossum pectinatum"]]+n[["Coelioxys alternata"]]+n[["Cemolobus ipomoeae"]]
(nn/sum(colSums(int.mat.n)))*100
####

PREFERENCE
PREFERENCE.k


#Comprobamos qué especies prefieren cada habitat----
exploiters
avoiders
names(exploiters)

BLexploiters<-exploiters[,1]
BLexploiters<-as.data.frame(BLexploiters)
rownames(BLexploiters)<-rownames(exploiters)
BLexploiters<-subset(BLexploiters, subset=(BLexploiters<1))
BLexploiters

COexploiters<-exploiters[,2]
COexploiters<-as.data.frame(COexploiters)
rownames(COexploiters)<-rownames(exploiters)
COexploiters<-subset(COexploiters, subset=(COexploiters<1))
COexploiters

CCexploiters<-exploiters[,3]
CCexploiters<-as.data.frame(CCexploiters)
rownames(CCexploiters)<-rownames(exploiters)
CCexploiters<-subset(CCexploiters, subset=(CCexploiters<1))
CCexploiters

DFexploiters<-exploiters[,4]
DFexploiters<-as.data.frame(DFexploiters)
rownames(DFexploiters)<-rownames(exploiters)
DFexploiters<-subset(DFexploiters, subset=(DFexploiters<1))
DFexploiters

DHexploiters<-exploiters[,5]
DHexploiters<-as.data.frame(DHexploiters)
rownames(DHexploiters)<-rownames(exploiters)
DHexploiters<-subset(DHexploiters, subset=(DHexploiters<1))
DHexploiters

DLexploiters<-exploiters[,6]
DLexploiters<-as.data.frame(DLexploiters)
rownames(DLexploiters)<-rownames(exploiters)
DLexploiters<-subset(DLexploiters, subset=(DLexploiters<1))
DLexploiters

DMexploiters<-exploiters[,7]
DMexploiters<-as.data.frame(DMexploiters)
rownames(DMexploiters)<-rownames(exploiters)
DMexploiters<-subset(DMexploiters, subset=(DMexploiters<1))
DMexploiters

DOexploiters<-exploiters[,8]
DOexploiters<-as.data.frame(DOexploiters)
rownames(DOexploiters)<-rownames(exploiters)
DOexploiters<-subset(DOexploiters, subset=(DOexploiters<1))
DOexploiters

EHexploiters<-exploiters[,9]
EHexploiters<-as.data.frame(EHexploiters)
rownames(EHexploiters)<-rownames(exploiters)
EHexploiters<-subset(EHexploiters, subset=(EHexploiters<1))
EHexploiters

EFexploiters<-exploiters[,10]
EFexploiters<-as.data.frame(EFexploiters)
rownames(EFexploiters)<-rownames(exploiters)
EFexploiters<-subset(EFexploiters, subset=(EFexploiters<1))
EFexploiters

HBexploiters<-exploiters[,11]
HBexploiters<-as.data.frame(HBexploiters)
rownames(HBexploiters)<-rownames(exploiters)
HBexploiters<-subset(HBexploiters, subset=(HBexploiters<1))
HBexploiters

MFexploiters<-exploiters[,12]
MFexploiters<-as.data.frame(MFexploiters)
rownames(MFexploiters)<-rownames(exploiters)
MFexploiters<-subset(MFexploiters, subset=(MFexploiters<1))
MFexploiters

SSexploiters<-exploiters[,13]
SSexploiters<-as.data.frame(SSexploiters)
rownames(SSexploiters)<-rownames(exploiters)
SSexploiters<-subset(SSexploiters, subset=(SSexploiters<1))
SSexploiters

WWexploiters<-exploiters[,14]
WWexploiters<-as.data.frame(WWexploiters)
rownames(WWexploiters)<-rownames(exploiters)
WWexploiters<-subset(WWexploiters, subset=(WWexploiters<1))
WWexploiters


BLavoiders<-avoiders[,1]
BLavoiders<-as.data.frame(BLavoiders)
rownames(BLavoiders)<-rownames(avoiders)
BLavoiders<-subset(BLavoiders, subset=(BLavoiders<1))
BLavoiders

COavoiders<-avoiders[,2]
COavoiders<-as.data.frame(COavoiders)
rownames(COavoiders)<-rownames(avoiders)
COavoiders<-subset(COavoiders, subset=(COavoiders<1))
COavoiders

CCavoiders<-avoiders[,3]
CCavoiders<-as.data.frame(CCavoiders)
rownames(CCavoiders)<-rownames(avoiders)
CCavoiders<-subset(CCavoiders, subset=(CCavoiders<1))
CCavoiders

DFavoiders<-avoiders[,4]
DFavoiders<-as.data.frame(DFavoiders)
rownames(DFavoiders)<-rownames(avoiders)
DFavoiders<-subset(DFavoiders, subset=(DFavoiders<1))
DFavoiders

DHavoiders<-avoiders[,5]
DHavoiders<-as.data.frame(DHavoiders)
rownames(DHavoiders)<-rownames(avoiders)
DHavoiders<-subset(DHavoiders, subset=(DHavoiders<1))
DHavoiders

DLavoiders<-avoiders[,6]
DLavoiders<-as.data.frame(DLavoiders)
rownames(DLavoiders)<-rownames(avoiders)
DLavoiders<-subset(DLavoiders, subset=(DLavoiders<1))
DLavoiders

DMavoiders<-avoiders[,7]
DMavoiders<-as.data.frame(DMavoiders)
rownames(DMavoiders)<-rownames(avoiders)
DMavoiders<-subset(DMavoiders, subset=(DMavoiders<1))
DMavoiders

DOavoiders<-avoiders[,8]
DOavoiders<-as.data.frame(DOavoiders)
rownames(DOavoiders)<-rownames(avoiders)
DOavoiders<-subset(DOavoiders, subset=(DOavoiders<1))
DOavoiders

EHavoiders<-avoiders[,9]
EHavoiders<-as.data.frame(EHavoiders)
rownames(EHavoiders)<-rownames(avoiders)
EHavoiders<-subset(EHavoiders, subset=(EHavoiders<1))
EHavoiders

EFavoiders<-avoiders[,10]
EFavoiders<-as.data.frame(EFavoiders)
rownames(EFavoiders)<-rownames(avoiders)
EFavoiders<-subset(EFavoiders, subset=(EFavoiders<1))
EFavoiders

HBavoiders<-avoiders[,11]
HBavoiders<-as.data.frame(HBavoiders)
rownames(HBavoiders)<-rownames(avoiders)
HBavoiders<-subset(HBavoiders, subset=(HBavoiders<1))
HBavoiders

MFavoiders<-avoiders[,12]
MFavoiders<-as.data.frame(MFavoiders)
rownames(MFavoiders)<-rownames(avoiders)
MFavoiders<-subset(MFavoiders, subset=(MFavoiders<1))
MFavoiders

SSavoiders<-avoiders[,13]
SSavoiders<-as.data.frame(SSavoiders)
rownames(SSavoiders)<-rownames(avoiders)
SSavoiders<-subset(SSavoiders, subset=(SSavoiders<1))
SSavoiders

WWavoiders<-avoiders[,14]
WWavoiders<-as.data.frame(WWavoiders)
rownames(WWavoiders)<-rownames(avoiders)
WWavoiders<-subset(WWavoiders, subset=(WWavoiders<1))
WWavoiders





namesexploiters<-rownames(exploiters)
namesexploiters<-as.data.frame(namesexploiters)
getwd()
write.csv(namesexploiters, "exploitersyavoiders.csv")




#Ahora quiero ver qué sucede con los diferentes géneros
avoiders
#Sospecho que las andrenas no quieren los habitats developed high

names(Datos1)
Datos1$Genus
Datos.andrena<-subset(Datos1, subset=(Datos1$Genus=="Andrena"))
summary(Datos.andrena$habitat.extracted)

#Veamos que sucede con lasioglossum
Datos.lasioglossum<-subset(Datos1, subset=(Datos1$Genus=="Lasioglossum"))
nrow(Datos.lasioglossum)
summary(Datos.lasioglossum$habitat.extracted)


Datos.bombus<-subset(Datos1, subset=(Datos1$Genus=="Bombus"))
nrow(Datos.bombus)
summary(Datos.bombus$habitat.extracted)

Datos.nomada<-subset(Datos1, subset=(Datos1$Genus=="Nomada"))
nrow(Datos.nomada)
summary(Datos.nomada$habitat.extracted)

Datos.osmia<-subset(Datos1, subset=(Datos1$Genus=="Osmia"))
nrow(Datos.osmia)
summary(Datos.osmia$habitat.extracted)

Datos.augochlora<-subset(Datos1, subset=(Datos1$Genus=="Augochlora"))
nrow(Datos.augochlora)
summary(Datos.augochlora$habitat.extracted)

Datos.apis<-subset(Datos1, subset=(Datos1$Genus=="Apis"))
nrow(Datos.apis)
summary(Datos.apis$gen_sp)

Datos.calliopsis<-subset(Datos1, subset=(Datos1$Genus=="Calliopsis"))
nrow(Datos.calliopsis)
summary(Datos.calliopsis$habitat.extracted)

Datos.halictus<-subset(Datos1, subset=(Datos1$Genus=="Halictus"))
nrow(Datos.halictus)
summary(Datos.halictus$habitat.extracted)

Datos.hylaeus<-subset(Datos1, subset=(Datos1$Genus=="Hylaeus"))
nrow(Datos.hylaeus)
summary(Datos.hylaeus$habitat.extracted)

Datos.megachile<-subset(Datos1, subset=(Datos1$Genus=="Megachile"))
nrow(Datos.megachile)
summary(Datos.megachile$habitat.extracted)

Datos.melissodes<-subset(Datos1, subset=(Datos1$Genus=="Melissodes"))
nrow(Datos.melissodes)
summary(Datos.melissodes$habitat.extracted)

Datos.ptilothrix<-subset(Datos1, subset=(Datos1$Genus=="Ptilothrix"))
nrow(Datos.ptilothrix)
summary(Datos.ptilothrix$habitat.extracted)

Datos.xylocopa<-subset(Datos1, subset=(Datos1$Genus=="Xylocopa"))
nrow(Datos.xylocopa)
summary(Datos.xylocopa$habitat.extracted)
