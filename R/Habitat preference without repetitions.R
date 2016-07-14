#Habitat preference without repetitions in the sampling point
#Strength y richness full data sin repeticiones en el punto de muestreo
Datos1 <- read.csv("~/Desktop/Tesis/R/habpref full data/Datos1.csv")
getwd()
observeds
#Agruamos habitats herbaceos con pastos
Datos1$habitat.extracted <- replace(Datos1$habitat.extracted, Datos1$habitat.extracted=="Hay/Pasture", "Herbaceuous")
#Agrupamos habitats como en el paper de Koh et al 2015
levels(Datos1$habitat.extracted)[levels(Datos1$habitat.extracted)=="Herbaceuous"]<-"Herbaceuous/Hay/Pasture"


#Creo un vector único de identificación del sitio y lo adjunto a mis datos
summary(Datos1$habitat.extracted)

sumcord<-(Datos1$latitude + Datos1$longitude)
Datos9<-(cbind(Datos1,sumcord))
USO

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
                                 !Datos1$gen_sp == "Andrena aurata"&
                                 !Datos1$gen_sp == "Andrena bruneri"&
                                 !Datos1$gen_sp == "Andrena gardineri"&
                                 !Datos1$gen_sp == "Andrena maculata"&
                                 !Datos1$gen_sp == "Andrena pura"&
                                 !Datos1$gen_sp == "Andrena vidae"&
                                 !Datos1$gen_sp == "Augochlora aurata"&
                                 !Datos1$gen_sp == "Augochlorella persimillus"&
                                 !Datos1$gen_sp == "Chrysis krombeini"&
                                 !Datos1$gen_sp == "Dolichovespula albida"&
                                 !Datos1$gen_sp == "Epeolus howardi"&
                                 !Datos1$gen_sp == "Heriades truncorum"&
                                 !Datos1$gen_sp == "hoplitis spoliata"&
                                 !Datos1$gen_sp == "Hylaeus basalis"&
                                 !Datos1$gen_sp == "Hylaeus leptocephala"&
                                 !Datos1$gen_sp == "Hylaeus messilae"&
                                 !Datos1$gen_sp == "Hylaeus messillae"&
                                 !Datos1$gen_sp == "Lasioglossum paraforbesii"&
                                 !Datos1$gen_sp == "lasioglossum vierecki"&
                                 !Datos1$gen_sp == "Megachile umbripennis"&
                                 !Datos1$gen_sp == "Melissodes apicata"&
                                 !Datos1$gen_sp == "Nomada lutuoloides"&
                                 !Datos1$gen_sp == "Nomada obliterata"&
                                 !Datos1$gen_sp == "Nomada seneciophila"&
                                 !Datos1$gen_sp == "Nomada texana"&
                                 !Datos1$gen_sp == "Pseudopanurgus solidaginis"&
                                 !Datos1$gen_sp == "Pseudopanurgus virginicus"&
                                 !Datos1$gen_sp == "Scolia dubia"&
                                 !Datos1$gen_sp == "Sphex nudus"&
                                 !Datos1$gen_sp == "Triepeolus concavus"&
                                 !Datos1$gen_sp == "Vespula vulgaris"&
                                 !Datos1$gen_sp == ""))

#Extracción de matriz de interacciones y filtrado para solo usar especies con más
#de 100 individuos en nuestro muestreo
summary(Datos6$gen_sp, maxsum=500)
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
View(observeds)
nmodels
rownames(observeds)
a

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
#amplia, usamos sd.4 del código de limpieza de datos, que contiene las especies antes de ser
#filtradas solo para nuestra zona
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


#Número de exploiters por habitat (45 especies de referencia)----
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


#Lista de especies que prefieren cada habitat----
names(exploiters)
row.names(subset(exploiters, subset=(exploiters[,1]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,2]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,3]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,4]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,5]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,6]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,7]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,8]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,9]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,10]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,11]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,12]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,13]<0.1)))
row.names(subset(exploiters, subset=(exploiters[,14]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,1]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,2]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,3]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,4]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,5]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,6]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,7]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,8]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,9]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,10]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,11]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,12]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,13]<0.1)))
row.names(subset(avoiders, subset=(avoiders[,14]<0.1)))



#

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

#Traits-----
#Cargo base de datos de traits para ver si encuentro algo
traits <- read.csv("~/Desktop/Tesis/Habitat preferences/Data/Traits de abejas.csv")
names(traits)
View(traits)
#Hago un objeto con la IT
IT.mean<-aggregate(traits$ITfam ~ traits$Genus, FUN=mean)
summary(traits$ITfam)
boxplot(traits$ITfam ~ traits$Genus) 
plot(IT.mean) 
IT.mean
#Nidos
nests<-aggregate(traits$Nest_location ~ traits$Genus, FUN=summary)
nests<-c(nests)
nests<-as.data.frame(nests)
View(nests)
rownames(nests) <- nests$traits.Genus
nests
#Dieta
diet<-aggregate(traits$Dietary_specialization ~ traits$Genus, FUN=summary)
diet<-c(diet)
diet<-as.data.frame(diet)
rownames(diet)<-nests$traits.Genus
diet

#Sociality
social<-aggregate(traits$Sociality ~ traits$Genus, FUN=summary)
social<-c(social)
social<-as.data.frame(social)
rownames(social)<-nests$traits.Genus
social
#Voltinism
volt<-aggregate(traits$Voltinism ~ traits$Genus, FUN=summary)
volt<-c(volt)
volt<-as.data.frame(volt)
rownames(volt)<-volt$traits.Genus
volt
#Phenology
boxplot(traits$pheno_q10, traits$Pheno_mean, traits$pheno_q90)
mean(traits$Pheno_mean)
median(traits$Pheno_mean)
phen<-aggregate(traits$pheno_q10 ~ traits$Genus, FUN=summary)
phen<-as.data.frame(phen)
rownames(phen)<-phen$traits.Genus
phen

#Comprobamos si los que tienen preferencia por deciduous forest tienen temprana fenología
rownames(subset(exploiters, subset=(exploiters$DeciduousForest < 0.01)))
rownames(subset(avoiders, subset=(avoiders$DeciduousForest < 0.01)))

View(traits)
#los exploiters
traits[23,18]
traits[39,18]
traits[70,18]
traits[109,18]
traits[107,18]
traits[115,18]
traits[116,18]
traits[122,18]
traits[174,18]
d.mean.exp<-c(traits[23,18],traits[39,18],traits[70,18],traits[109,18],traits[107,18],traits[115,18],traits[116,18],traits[122,18],traits[174,18])
#los avoiders
traits[4,18]
traits[73,18]
traits[85,18]
traits[112,18]
traits[129,18]
traits[134,18]
traits[138,18]
traits[148,18]
traits[161,18]
#Parece que los avoiders tienen una fenología tardía
d.mean.avo<-c(traits[4,18],traits[73,18],traits[85,18],traits[112,18],traits[129,18],traits[134,18],traits[138,18],traits[148,18],traits[161,18])
boxplot(traits$Pheno_mean, d.mean.exp,d.mean.avo, names=nomnom, ylab="Mean Phenology")
nomnom<-c("Reference","Deciduous exploiters","Deciduous avoiders")
mean(d.mean.avo)
mean(d.mean.exp)
#

#Diversidad de habitats----
#Aquí vamos a calcular la diversidad de habitats preferidos por las diferentes especies
#Para ello, convertirmos en NA todo lo que no indique preferencia, y lo que indique preferencia
#lo transformamos en 1, para sumarlo y que nos diga el número de preferencias de habitat
exploiters
d.exploiters<-replace(exploiters, exploiters>0, NA)
d.exploiters<-replace(d.exploiters, is.na(d.exploiters)==FALSE, 1)
diversity.exploiters<-rowSums(d.exploiters, na.rm = TRUE)
diversity.exploiters

#Uso de habitats-----
#queremos rareficar los datos y sacar el uso de habitats, para eso necesitaremos la 
#presencia/ausencia de cada especie en cada uno de los habitats
Datos6
summary(Datos6$habitat.extracted)


#Creamos una matriz de interacciones con nuestros datos
int.mat<-aggregate(Datos6$habitat.extracted~Datos6$gen_sp, FUN = summary, Datos6)
jeje<-c(int.mat)
jiji<-c(jaja)
#lo convertimos en dataframe
jeje<-as.data.frame(jeje)
jiji<-as.data.frame(jeje)
jejem<-jeje[,-1]
jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
row.names(jejem)<- jeje[,1]
#Esta es nuestra matriz
int.mat<-jejem
nrow(int.mat)
View(int.mat)
#Cálculos de coverage :DDDDD
#vemos a qué tamaño muestral cubrimos qué porcentaje de riqueza de habitat con la función
#iNEXT, en cada habitat
library(iNEXT)
#Con este loop vemos el coverage de cada habitat
for( n in 1:15){
  a<-iNEXT(int.mat[,n],datatype = "abundance", endpoint = 805)
  print(a$iNextEst)
}
#Vemos cuanto coverage tiene el habitat que menos individuos tiene, el cual es "herbaceuous"
iNEXT(int.mat[,1],datatype = "abundance", endpoint = 176)
iNEXT(int.mat[,2],datatype = "abundance", endpoint = 843)
iNEXT(int.mat[,3],datatype = "abundance", endpoint = 1680)
iNEXT(int.mat[,4],datatype = "abundance", endpoint = 5453)
iNEXT(int.mat[,5],datatype = "abundance", endpoint = 360)
iNEXT(int.mat[,6],datatype = "abundance", endpoint = 805)
iNEXT(int.mat[,7],datatype = "abundance", endpoint = 545)
iNEXT(int.mat[,8],datatype = "abundance", endpoint = 1499)
iNEXT(int.mat[,9],datatype = "abundance", endpoint = 456)
iNEXT(int.mat[,10],datatype = "abundance", endpoint = 405)
iNEXT(int.mat[,11],datatype = "abundance", endpoint = 0)
iNEXT(int.mat[,12],datatype = "abundance", endpoint = 1577)
iNEXT(int.mat[,13],datatype = "abundance", endpoint = 591)
iNEXT(int.mat[,14],datatype = "abundance", endpoint = 159)
iNEXT(int.mat[,15],datatype = "abundance", endpoint = 1213)

#Shrub/Scrub que es el que menos individuos tiene tiene un coverage de 0.617
#Ahora tenemos que buscar cuantos individuos tenemos que tomar de cada habitat para tener el
#mismo coverage
int.mat<-int.mat[,-11]
colnames(int.mat)
samplesize<-NULL
for (n in 1:14){
  c<-estimateD(int.mat[,n], datatype = "abundance", base= "coverage", level = 0.60)
  samplesize[n]<-c
}
colnames(int.mat)
samplesize
samplesize<-as.data.frame(samplesize)
nombres<-c("BL","CO","CC","DF","DH","DL","DM","DO", "EH","EF","HB","MF","SS","WW")
colnames(samplesize)<-nombres
#Tenemos el tamaño que muestral que tenemos que coger de cada habitat para que el coverage
#sea el mismo
samplesize
#restamos para comprobar que tenemos suficiente muestra en nuestro dataframe
#al ser todo valores positivos indican que si tenemos suficiente muestra
summary(Datos6$habitat.extracted) - samplesize 
summary(Datos6$habitat.extracted)
#Ahora hacemos un loop, para extraer los individuos que correspondan para cada habitat 
#para igualar la misma coverage 


#Sacamos matriz de interacción


#Hacemos una lista, donde en cada especie se nos dirá cuanto aparece en cada habitat
intd6<-tapply(Datos6$habitat.extracted, Datos6$gen_sp, FUN = summary)
#Convertimos la lista en un gran vector, y luego en dataframe
intd6<-unlist(intd6)
length(intd6)
intd6 <- as.data.frame(intd6)
rownames(intd6)
#Queremos crear un dataframe de referencia al que le iremos pegando (merge) los 100 vectores
#resultantes de la rarefacción
a<- rep(x = NA,nrow(intd6))
d <- data.frame(a)
nombrecitos<-d
rownames(d)<-rownames(intd6)
rownames(d)
#"d" es el dataframe que hemos preparado, con el mismo rownames, para ir guardando
#vectores con la abundancia de especies en cada habitat

samplesize
###Hacemos un loop para rareficar, tomando muestras igualando por coverage
for (n in 1:100) {
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 134), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 114), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 146), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 91), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 131), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 105), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 124), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 154), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 171), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous/Hay/Pasture"))
  bri11<-bri11[sample((nrow(bri11)), 122), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 145), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Coastal"))
  bri13<-bri13[sample((nrow(bri13)), 164), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 143), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 146), ]
  
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción, en forma de lista, con todas las abundancias de cada
  #especie en cada habitat
  int<-tapply(Datos7$habitat.extracted, Datos7$gen_sp, FUN = summary)
  #Deslistamos en un vector más fácil para operar
  int.u<- unlist(int)
  length(int.u)
  #Sustituimos abundancia por presencia/ausencia
  int.u<-replace(int.u, int.u>1,1)
  
  #Unimos el vector de presencia/ausencia al data frame, al unir por row.names
  #nos crea en la unión una colunma nueva, que eliminamos
  d <- merge(d,int.u, by="row.names",all=TRUE)
  row.names(d)<-row.names(intd6)
  d<-d[,-1]
  }
#Cambiamos los NA por 0
d[is.na(d)] <-  0
rowSums(d)
#Hacemos una columna con la suma de las 100 repeticiones, si el resultado es mayor a uno
summary(Datos6$habitat.extracted) 
#hay USO
d$suma <- rowSums(d)
d$uso <- rowSums(d)
d$uso <- replace(d$uso, d$uso>0, "YES")
d$uso <- replace(d$uso, d$uso==0, "NO")
d$uso.n <- d$suma
d$uso.n <- replace(d$uso.n, d$uso.n >1,1)
d$rnames <- row.names(d) 
#Creamos data frame con los resultados del uso
uso<- data.frame(d$rnames,d$suma,d$uso.n,d$uso)
row.names(uso) <- row.names(intd6)
espacio <- regexpr(pattern = " ", uso$d.rnames)
uso$genus<-substr(uso$d.rnames, start = 0 , stop = espacio - 1) 
uso$genus <- as.factor(uso$genus)
summary(uso$genus)


#Creamos una columna con habitats a partir de los rownames##
summary(uso$genus)
h1 <- regexpr(pattern = "Barren Land", uso$d.rnames)
habitat.extracted <- substr(uso$d.rnames, start = h1, stop = 100)
h2 <- regexpr(pattern = "Coastal", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h2, stop = 100)
h3 <- regexpr(pattern = "Cultivated Crops", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h3, stop = 100)
h4 <- regexpr(pattern = "Deciduous Forest", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h4, stop = 100)
h5 <- regexpr(pattern = "Developed, H", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h5, stop = 100)
h6 <- regexpr(pattern = "Developed, Lo", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h6, stop = 100)
h7 <- regexpr(pattern = "Developed, Med", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h7, stop = 100)
h8 <- regexpr(pattern = "Developed, Op", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h8, stop = 100)
h9 <- regexpr(pattern = "Emerg", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h9, stop = 100)
h10 <- regexpr(pattern = "Evergr", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h10, stop = 100)
h11 <- regexpr(pattern = "Herbaceuous/Hay/Pasture", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h11, stop = 100)
h12 <- regexpr(pattern = "Mixed", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h12, stop = 100)
h13 <- regexpr(pattern = "Shrub", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h13, stop = 100)
h14 <- regexpr(pattern = "Woody", uso$d.rnames)
habitat.extracted <- substr(habitat.extracted, start = h14, stop = 100)

uso
#Unimos la nueva columna
uso.1<-cbind(uso,habitat.extracted)
View(uso.1)
#Retiramos el Hay/Pasture que siempre va a ser 0
uso.2<-subset(uso.1, subset=(uso.1$habitat.extracted=="Barren Land" | uso.1$habitat.extracted=="Coastal" | uso.1$habitat.extracted== "Cultivated Crops" | uso.1$habitat.extracted=="Deciduous Forest" | uso.1$habitat.extracted=="Developed, High Intensity" | uso.1$habitat.extracted=="Developed, Low Intensity" | uso.1$habitat.extracted=="Developed, Medium Intensity" | uso.1$habitat.extracted=="Developed, Open Space" | uso.1$habitat.extracted=="Emergent Herbaceuous Wetlands" | uso.1$habitat.extracted=="Evergreen Forest" | uso.1$habitat.extracted=="Herbaceuous/Hay/Pasture" | uso.1$habitat.extracted=="Mixed Forest" | uso.1$habitat.extracted=="Shrub/Scrub" | uso.1$habitat.extracted=="Woody Wetlands"))
View(uso.2)
#Uso general----
#Veamos el uso de cada habitat, para eso construimos un data.frame con el uso
uso.BL <- subset(uso.2, subset=(uso.2$habitat.extracted=="Barren Land"))
ny.BL <-  as.vector(summary(uso.BL$d.uso))
uso.CO <- subset(uso.2, subset=(uso.2$habitat.extracted=="Coastal"))
ny.CO <-  as.vector(summary(uso.CO$d.uso))
uso.CC <- subset(uso.2, subset=(uso.2$habitat.extracted=="Cultivated Crops"))
ny.CC <-  as.vector(summary(uso.CC$d.uso))
uso.DF <- subset(uso.2, subset=(uso.2$habitat.extracted=="Deciduous Forest"))
ny.DF <-  as.vector(summary(uso.DF$d.uso))
uso.DH <- subset(uso.2, subset=(uso.2$habitat.extracted=="Developed, High Intensity"))
ny.DH <-  as.vector(summary(uso.DH$d.uso))
uso.DL <- subset(uso.2, subset=(uso.2$habitat.extracted=="Developed, Low Intensity"))
ny.DL <-  as.vector(summary(uso.DL$d.uso))
uso.DM <- subset(uso.2, subset=(uso.2$habitat.extracted=="Developed, Medium Intensity"))
ny.DM <-  as.vector(summary(uso.DM$d.uso))
uso.DO <- subset(uso.2, subset=(uso.2$habitat.extracted=="Developed, Open Space"))
ny.DO <-  as.vector(summary(uso.DO$d.uso))
uso.EH <- subset(uso.2, subset=(uso.2$habitat.extracted=="Emergent Herbaceuous Wetlands"))
ny.EH <-  as.vector(summary(uso.EH$d.uso))
uso.EF <- subset(uso.2, subset=(uso.2$habitat.extracted=="Evergreen Forest"))
ny.EF <-  as.vector(summary(uso.EF$d.uso))
uso.HH <- subset(uso.2, subset=(uso.2$habitat.extracted=="Herbaceuous/Hay/Pasture"))
ny.HH <-  as.vector(summary(uso.HH$d.uso))
uso.MF <- subset(uso.2, subset=(uso.2$habitat.extracted=="Mixed Forest"))
ny.MF <-  as.vector(summary(uso.MF$d.uso))
uso.SS <- subset(uso.2, subset=(uso.2$habitat.extracted=="Shrub/Scrub"))
ny.SS <-  as.vector(summary(uso.SS$d.uso))
uso.WW <- subset(uso.2, subset=(uso.2$habitat.extracted=="Woody Wetlands"))
ny.WW <-  as.vector(summary(uso.WW$d.uso))

ny.uso <- rbind(ny.BL,ny.CO,ny.CC,ny.DF,ny.DH,ny.DL,ny.DM,ny.DO,ny.EH,ny.EF,ny.HH,ny.MF,ny.SS,ny.WW)
row.names(ny.uso) <-c("BarrenLand", "Coastal", "Cultivated Crops" ,"DeciduousForest", "DevelopedHighIntensity", "DevelopedLowIntensity", "DevelopedMediumIntensity","DevelopedOpenSpace","EmergentHerbaceuousWetlands", "EvergreenForest","Herbaceuous/Hay/Pasture","MixedForest","ShrubScrub","WoodyWetlands")
colnames(ny.uso) <- c("NO","YES")
USO <- ny.uso
rowSums(USO)
nrow(Datos6)
#Exploramos por género----
View(uso.2)
#Veamos si las Andrenas evitan habitats antropizados
Andrena.uso <- subset(uso.2, subset=(uso.2$genus == "Andrena"))
View(Andrena.uso)
#Número de especies
nrow(Andrena.uso)/14

Andrena.uso.d <- subset(Andrena.uso, subset=(Andrena.uso$habitat.extracted == "Developed, High Intensity" | Andrena.uso$habitat.extracted == "Developed, Medium Intensity" | Andrena.uso$habitat.extracted == "Developed, Low Intensity" | Andrena.uso$habitat.extracted == "Developed, Open Space" | Andrena.uso$habitat.extracted == "Cultivated Crops"))
Andrena.uso.n <- subset(Andrena.uso, subset=(!Andrena.uso$habitat.extracted == "Developed, High Intensity" & !Andrena.uso$habitat.extracted == "Developed, Medium Intensity" & !Andrena.uso$habitat.extracted == "Developed, Low Intensity" & !Andrena.uso$habitat.extracted == "Developed, Open Space" & !Andrena.uso$habitat.extracted == "Cultivated Crops"))
head(Andrena.uso.d)
head(Andrena.uso.n)
#El uso de andrena de habitats antropizados
summary(Andrena.uso$d.uso)
View(Andrena.uso.d)
summary(Andrena.uso.d$d.uso)

#El uso en cada habitat
Andrena.uso.dh <- subset(Andrena.uso, Andrena.uso$habitat.extracted=="Developed, High Intensity")
#Quiero ver las especies de andrena que usan estos habitats desarrollados
which(Andrena.uso.dh$d.uso == "YES" )
row.names(Andrena.uso.dh[c(which(Andrena.uso.dh$d.uso == "YES" )),])
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Developed, High Intensity"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Developed, Medium Intensity"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Developed, Low Intensity"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Developed, Open Space"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Barren Land"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Coastal"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Deciduous Forest"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Emergent Herbaceuous Wetlands"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Evergreen Forest"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Herbaceuous/Hay/Pasture"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Mixed Forest"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Shrub/Scrub"))
summary(subset(Andrena.uso$d.uso, Andrena.uso$habitat.extracted=="Woody Wetlands"))


summary(Andrena.uso.d$d.uso) + summary(Andrena.uso.n$d.uso)
View(Andrena.uso.d)


#Vemos los Lasiolgossum
Lasioglossum.uso <- subset(uso.2, subset=(uso.2$genus == "Lasioglossum"))
#Cuantas especies?
nrow(Lasioglossum.uso)/14
Lasioglossum.uso.dh <- subset(Lasioglossum.uso, Lasioglossum.uso$habitat.extracted=="Developed, High Intensity")
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Developed, High Intensity"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Developed, Medium Intensity"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Developed, Low Intensity"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Developed, Open Space"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Cultivated Crops"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Barren Land"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Coastal"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Deciduous Forest"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Emergent Herbaceuous Wetlands"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Evergreen Forest"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Herbaceuous/Hay/Pasture"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Mixed Forest"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Shrub/Scrub"))
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Woody Wetlands"))


66+124

#
summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Developed, High Intensity"))+summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Developed, Medium Intensity"))+summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Developed, Low Intensity"))+summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Developed, Open Space"))+summary(subset(Lasioglossum.uso$d.uso, Lasioglossum.uso$habitat.extracted=="Cultivated Crops"))
#

Lasioglossum.uso.d <- subset(Lasioglossum.uso, subset=(Lasioglossum.uso$habitat.extracted == "Developed, High Intensity" | Lasioglossum.uso$habitat.extracted == "Developed, Medium Intensity" | Lasioglossum.uso$habitat.extracted == "Developed, Low Intensity" | Lasioglossum.uso$habitat.extracted == "Developed, Open Space" | Lasioglossum.uso$habitat.extracted == "Cultivated Crops"))
Lasioglossum.uso.n <- subset(Lasioglossum.uso, subset=(!Lasioglossum.uso$habitat.extracted == "Developed, High Intensity" & !Lasioglossum.uso$habitat.extracted == "Developed, Medium Intensity" & !Lasioglossum.uso$habitat.extracted == "Developed, Low Intensity" & !Lasioglossum.uso$habitat.extracted == "Developed, Open Space" & !Lasioglossum.uso$habitat.extracted == "Cultivated Crops"))

summary(Lasioglossum.uso.d$d.uso)

summary(uso.2$genus)
#Genero nomada
Nomada.uso <- subset(uso.2, subset=(uso.2$genus == "Nomada"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Developed, High Intensity"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Developed, Medium Intensity"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Developed, Low Intensity"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Developed, Open Space"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Cultivated Crops"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Developed, High Intensity"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Developed, Medium Intensity"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Developed, Low Intensity"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Developed, Open Space"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Cultivated Crops"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Barren Land"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Coastal"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Deciduous Forest"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Emergent Herbaceuous Wetlands"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Evergreen Forest"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Herbaceuous/Hay/Pasture"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Mixed Forest"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Shrub/Scrub"))
summary(subset(Nomada.uso$d.uso, Nomada.uso$habitat.extracted=="Woody Wetlands"))


Osmia.uso <- subset(uso.2, subset=(uso.2$genus == "Osmia"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Developed, High Intensity"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Developed, Medium Intensity"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Developed, Low Intensity"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Developed, Open Space"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Cultivated Crops"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Developed, High Intensity"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Developed, Medium Intensity"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Developed, Low Intensity"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Developed, Open Space"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Cultivated Crops"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Barren Land"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Coastal"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Deciduous Forest"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Emergent Herbaceuous Wetlands"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Evergreen Forest"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Herbaceuous/Hay/Pasture"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Mixed Forest"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Shrub/Scrub"))
summary(subset(Osmia.uso$d.uso, Osmia.uso$habitat.extracted=="Woody Wetlands"))

#Relación lineal de USO con porcentaje de cobertura
uso.habitat <- USO[,2]
uso.habitat <- as.data.frame(uso.habitat)
cobertura<-c(0.34,6.98,14.46,29.25,0.52,2.91,1.27,5.97,1.00,8.49,14.23,3.34,3.82,6.41)
uso.habitat <- cbind(uso.habitat,cobertura)
plot(uso.habitat$uso.habitat,uso.habitat$cobertura)
text(uso.habitat$uso.habitat,uso.habitat$cobertura, labels = row.names(uso.habitat), cex=0.7, pos=3)

#Lista de USO por especie

#
matriz.uso <- data.frame()
jiji <-aggregate (Datos6$habitat.extracted ~ Datos6$gen_sp, FUN = summary, Datos6)
matriz.uso<-as.data.frame(jiji[[1]])
row.names(matriz.uso)<-matriz.uso$`jiji[[1]]`
matriz.uso.2 <- matriz.uso
#aa<- rep(x = NA,nrow(matriz.uso))
#matriz.uso<-cbind(matriz.uso,aa)

#











for (n in 1:100){
  
  tri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  tri1<-tri1[sample((nrow(tri1)), 134), ]
  tri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  tri2<-tri2[sample((nrow(tri2)), 114), ]
  tri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  tri3<-tri3[sample((nrow(tri3)), 146), ]
  tri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  tri4<-tri4[sample((nrow(tri4)), 91), ]
  tri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  tri5<-tri5[sample((nrow(tri5)), 131), ]
  tri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  tri6<-tri6[sample((nrow(tri6)), 105), ]
  tri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  tri7<-tri7[sample((nrow(tri7)), 124), ]
  tri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  tri8<-tri8[sample((nrow(tri8)), 154), ]
  tri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  tri9<-tri9[sample((nrow(tri9)), 171), ]
  tri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous/Hay/Pasture"))
  tri11<-tri11[sample((nrow(tri11)), 122), ]
  tri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  tri12<-tri12[sample((nrow(tri12)), 145), ]
  tri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Coastal"))
  tri13<-tri13[sample((nrow(tri13)), 164), ]
  tri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  tri14<-tri14[sample((nrow(tri14)), 143), ]
  tri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  tri15<-tri15[sample((nrow(tri15)), 146), ]
  Datos7<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri11,tri12,tri13,tri14,tri15))
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
  row.names(meji)<- meji[,1]
  nombremes<-meji[,1]
  View(meji)
  mejib<-mejib[,-1]
  meji<-meji[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  mimi<-apply(meji, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- nombremes
  row.names(mimi)<- nombremes
  View(momo)
  momo<-momo[,-11]
  mimi <- mimi[,-11]
  rowSums(momo)
  rowSums(mimi)
  
  a1<-rowSums(momo)
  a1<-as.data.frame(a1)
  a2 <- rowSums(mimi)
  a2 <- as.data.frame(a2)
  rownames(a1)
  matriz.uso <- merge(matriz.uso,a1, by="row.names",all=TRUE)
  rownames(matriz.uso)<-matriz.uso$Row.names
  matriz.uso<-matriz.uso[,-1]
  
  matriz.uso.2 <- merge(matriz.uso.2,a2, by="row.names",all=TRUE)
  rownames(matriz.uso.2)<-matriz.uso.2$Row.names
  matriz.uso.2 <- matriz.uso.2[,-1]
  
  }
matriz.uso <- matriz.uso[,-1]
matriz.uso.2 <- matriz.uso.2[,-1]

View(matriz.uso)
View(matriz.uso.2)

names(matriz.uso)
names(matriz.uso.2)


matriz.uso[is.na(matriz.uso)] <-  0
matriz.uso.2[is.na(matriz.uso.2)] <-  0


USO.ESPECIES<-rowSums(matriz.uso)/100
USO.ESPECIES<-as.data.frame(USO.ESPECIES)
USO.ESPECIES<-cbind(USO.ESPECIES,(rowSums(matriz.uso.2)/100))
names(USO.ESPECIES) <- c("n de habitats","Abundancia")
View(USO.ESPECIES)
nrow(USO.ESPECIES)

View(dependencias)

q<-summary(Datos6$gen_sp, maxsum= 500)
q[["Panurginus atramontensis"]]
q[["Nomada subrutila"]]
q[["Nomada gracilis"]]
q[["Nomada dentariae"]]
q[["Melissodes boltoniae/fumosa"]]
q[["Megachile latimanus"]]
q[["Megachile inermis"]]
q[["Lasioglossum forbesii"]]
q[["Lasioglossum acuminatum"]]
q[["Bombus citrinus"]]
q[["Andrena simplex"]]
q[["Andrena robertsonii"]]
q[["Andrena bradleyi"]]

#Agrupamos para repetir preferencias----
nrow(Datos6)
habitat.grouped <- Datos6$habitat.extracted
Datos.g <- cbind(Datos6,habitat.grouped)
View(Datos.g)
#Nombramos los nuevos grupos
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Developed, High Intensity"]<-"Urban"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Developed, Medium Intensity"]<-"Urban"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Cultivated Crops"]<-"Semiurban"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Herbaceuous/Hay/Pasture"]<-"Semiurban"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Developed, Low Intensity"]<-"Semiurban"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Developed, Open Space"]<-"Semiurban"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Deciduous Forest"]<-"Forest"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Evergreen Forest"]<-"Forest"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Mixed Forest"]<-"Forest"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Coastal"]<-"Other/Natural"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Barren Land"]<-"Other/Natural"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Emergent Herbaceuous Wetlands"]<-"Other/Natural"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Woody Wetlands"]<-"Other/Natural"
levels(Datos.g$habitat.grouped)[levels(Datos.g$habitat.grouped)=="Shrub/Scrub"]<-"Other/Natural"
summary(Datos.g$habitat.grouped)
#Extracción de matriz de interacciones y filtrado para solo usar especies con más
#de 100 individuos en nuestro muestreo
summary(Datos.g$gen_sp, maxsum=500)
jiji <-aggregate (Datos.g$habitat.grouped ~ Datos.g$gen_sp, FUN = summary, Datos.g)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
row.names(habpref)<-habpref$Datos.g.gen_sp
meji<-habpref[,-1]
meji <- meji[,-5]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- habpref[,1]
momomo<-as.data.frame(momo)
View(momomo)
#Filtramos para que solo aparezcan las especies que solo tienen >100 observaciones

summmm<-rowSums(momomo)
momomosb<-cbind(momomo,summmm)
momomost<-subset(momomosb, subset=momomosb$summmm>100)
names(momomost)
observeds<-momomost[,1:4]
rownames(observeds)
names(observeds)

library(bipartite)
#Con este comando extraemos 1000 matrices de interacción de esperados
nmodels<-nullmodel(observeds, N=1000, method="r2dtable")

#sacamos para Other.Natural solamente pero para cada especie el percentil 0.05 y el
#0.95
##Vamos a crear una matriz con los los cuartiles .05 y otra con los cuartiles .95
#Empezamos por Other.Natural, y seguimos para el resto de habitats
names(observeds)
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
ONmenor<-menor
ONmayor<-mayor
ONmed<-med
ONmed<-as.data.frame(ONmed)
ONmenor<-as.data.frame(ONmenor)
ONmayor<-as.data.frame(ONmayor)
rownames(ONmenor)<-rownames(observeds)
rownames(ONmayor)<-rownames(observeds)
rownames(ONmed)<-rownames(observeds)
ONmenor
ONmed
ONmayor


#Las sacamos para Coastal y para el resto de habitats
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
PCmenor<-menor
PCmayor<-mayor
PCmed<-med
PCmed<-as.data.frame(PCmed)
PCmenor<-as.data.frame(PCmenor)
PCmayor<-as.data.frame(PCmayor)
rownames(PCmenor)<-rownames(observeds)
rownames(PCmayor)<-rownames(observeds)
rownames(PCmed)<-rownames(observeds)
PCmenor
PCmed
PCmayor
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
FOmenor<-menor
FOmayor<-mayor
FOmed<-med
FOmed<-as.data.frame(FOmed)
FOmenor<-as.data.frame(FOmenor)
FOmayor<-as.data.frame(FOmayor)
rownames(FOmenor)<-rownames(observeds)
rownames(FOmayor)<-rownames(observeds)
rownames(FOmed)<-rownames(observeds)
FOmenor
FOmayor
FOmed

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
URmenor<-menor
URmayor<-mayor
URmed<-med
URmed<-as.data.frame(URmed)
URmenor<-as.data.frame(URmenor)
URmayor<-as.data.frame(URmayor)
rownames(URmenor)<-rownames(observeds)
rownames(URmayor)<-rownames(observeds)
rownames(URmed)<-rownames(observeds)
URmenor
URmayor
URmed



menor<-cbind(ONmenor,PCmenor,FOmenor,URmenor)
menor<-as.data.frame(menor)
mayor<-cbind(ONmayor,PCmayor,FOmayor,URmayor)
mayor<-as.data.frame(mayor)
med<-cbind(ONmed,PCmed,FOmed,URmed)
med<-as.data.frame(med)
#Los que tengan valores negativos o el 0 serán avoiders, porque significa que tendrán
#una abundancia menor al valor del cuantil .05 de la distribución de valores esperados
colnames(observeds)
avoiders<-(observeds-menor)
colnames(avoiders)
names(avoiders)<-c("Other.Natural", "Pasture.Crops", "Forest" ,"Urban")
names(observeds)<-c("Other.Natural", "Pasture.Crops", "Forest" ,"Urban")
avoiders.g2 <- avoiders
#Los que tengan valores negativos o el 0 serán exploiters
exploiters<-(mayor-observeds)
names(exploiters) <- c("Other.Natural", "Pasture.Crops", "Forest" ,"Urban")
exploiters.g2 <- exploiters



nrow(subset(exploiters.g2, subset = (exploiters.g2[,1]<0.1)))
nrow(subset(exploiters.g2, subset = (exploiters.g2[,2]<0.1)))
nrow(subset(exploiters.g2, subset = (exploiters.g2[,3]<0.1)))
nrow(subset(exploiters.g2, subset = (exploiters.g2[,4]<0.1)))

#Creamos data frame con el número de exploiters y avoiders de cada grupo de habitats

PREFERENCE.g2 <- c(nrow(subset(exploiters.g2, subset = (exploiters.g2[,1]<0.1))),nrow(subset(exploiters.g2, subset = (exploiters.g2[,2]<0.1))),nrow(subset(exploiters.g2, subset = (exploiters.g2[,3]<0.1))),nrow(subset(exploiters.g2, subset = (exploiters.g2[,4]<0.1))))
PREFERENCE.g2 <- as.data.frame(PREFERENCE.g2)
row.names(PREFERENCE.g2) <-c("Other.Natural", "Pasture.Crops", "Forest" ,"Urban") 
avoidance.g2 <- c(nrow(subset(avoiders.g2, subset = (avoiders.g2[,1]<0.1))),nrow(subset(avoiders.g2, subset = (avoiders.g2[,2]<0.1))),nrow(subset(avoiders.g2, subset = (avoiders.g2[,3]<0.1))),nrow(subset(avoiders.g2, subset = (avoiders.g2[,4]<0.1))))
PREFERENCE.g2 <- cbind(PREFERENCE.g2,avoidance.g2)
nrow(exploiters.g2)
names(PREFERENCE.g2)<-c("Exploiters","Avoiders")

indiferencia.g2 <- nrow(exploiters.g2)-(PREFERENCE.g2$Exploiters+PREFERENCE.g2$Avoiders)
PREFERENCE.g2 <- cbind(PREFERENCE.g2,indiferencia.g2)
names(PREFERENCE.g2)<-c("Exploiters","Avoiders","Indifferent")
PREFERENCE.g2

#Lista de usuarios y evitadores de los habitat grupo
row.names(subset(exploiters.g2, subset = (exploiters.g2[,1]<0.1)))
row.names(subset(exploiters.g2, subset = (exploiters.g2[,2]<0.1)))
row.names(subset(exploiters.g2, subset = (exploiters.g2[,3]<0.1)))
row.names(subset(exploiters.g2, subset = (exploiters.g2[,4]<0.1)))

row.names(subset(avoiders.g2, subset = (avoiders.g2[,1]<0.1)))
row.names(subset(avoiders.g2, subset = (avoiders.g2[,2]<0.1)))
row.names(subset(avoiders.g2, subset = (avoiders.g2[,3]<0.1)))
row.names(subset(avoiders.g2, subset = (avoiders.g2[,4]<0.1)))


USO
PREFERENCE
PREFERENCE.g

#Agrupamos.2----- 
#de una forma diferente, separando los pastures y crops en un solo grupo
#incluimos developed low y open como urban
nrow(Datos6)
habitat.grouped.2 <- Datos6$habitat.extracted
Datos.g2 <- cbind(Datos6,habitat.grouped)
Datos.g2 <- replace(Datos.g2, Datos.g$habitat.grouped == "Developed, High Intensity", "Urban")
#Nombramos los nuevos grupos
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Developed, High Intensity"]<-"Urban"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Developed, Medium Intensity"]<-"Urban"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Cultivated Crops"]<-"Pasture and Crops"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Herbaceuous/Hay/Pasture"]<-"Pasture and Crops"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Developed, Low Intensity"]<-"Urban"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Developed, Open Space"]<-"Urban"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Deciduous Forest"]<-"Forest"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Evergreen Forest"]<-"Forest"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Mixed Forest"]<-"Forest"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Coastal"]<-"Other/Natural"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Barren Land"]<-"Other/Natural"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Emergent Herbaceuous Wetlands"]<-"Other/Natural"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Woody Wetlands"]<-"Other/Natural"
levels(Datos.g2$habitat.grouped)[levels(Datos.g2$habitat.grouped)=="Shrub/Scrub"]<-"Other/Natural"
summary(Datos.g2$habitat.grouped)
a

#USO tomando 100 muestras de cada especies----
##
a
u1<-subset(Datos6, subset=(Datos6$gen_sp == names(a[1])))
u1<-u1[sample((nrow(u1)), 100), ]
summary(u1$habitat.extracted)
u2<-subset(Datos6, subset=(Datos6$gen_sp == names(a[2])))
u2<-u2[sample((nrow(u2)), 100), ]
u3<-subset(Datos6, subset=(Datos6$gen_sp == names(a[3])))
u3<-u3[sample((nrow(u3)), 100), ]
u4<-subset(Datos6, subset=(Datos6$gen_sp == names(a[4])))
u4<-u4[sample((nrow(u4)), 100), ]
u5<-subset(Datos6, subset=(Datos6$gen_sp == names(a[5])))
u5<-u5[sample((nrow(u5)), 100), ]
u6<-subset(Datos6, subset=(Datos6$gen_sp == names(a[6])))
u6<-u6[sample((nrow(u6)), 100), ]
u7<-subset(Datos6, subset=(Datos6$gen_sp == names(a[7])))
u7<-u7[sample((nrow(u7)), 100), ]
u8<-subset(Datos6, subset=(Datos6$gen_sp == names(a[8])))
u8<-u8[sample((nrow(u8)), 100), ]
u9<-subset(Datos6, subset=(Datos6$gen_sp == names(a[9])))
u9<-u9[sample((nrow(u9)), 100), ]
u10<-subset(Datos6, subset=(Datos6$gen_sp == names(a[10])))
u10<-u10[sample((nrow(u10)), 100), ]
u11<-subset(Datos6, subset=(Datos6$gen_sp == names(a[11])))
u11<-u11[sample((nrow(u11)), 100), ]
u12<-subset(Datos6, subset=(Datos6$gen_sp == names(a[12])))
u12<-u12[sample((nrow(u12)), 100), ]
u13<-subset(Datos6, subset=(Datos6$gen_sp == names(a[13])))
u13<-u13[sample((nrow(u13)), 100), ]
u14<-subset(Datos6, subset=(Datos6$gen_sp == names(a[14])))
u14<-u14[sample((nrow(u14)), 100), ]
u15<-subset(Datos6, subset=(Datos6$gen_sp == names(a[15])))
u15<-u15[sample((nrow(u15)), 100), ]
u16<-subset(Datos6, subset=(Datos6$gen_sp == names(a[16])))
u16<-u16[sample((nrow(u16)), 100), ]
u17<-subset(Datos6, subset=(Datos6$gen_sp == names(a[17])))
u17<-u17[sample((nrow(u17)), 100), ]
u18<-subset(Datos6, subset=(Datos6$gen_sp == names(a[18])))
u18<-u18[sample((nrow(u18)), 100), ]
u19<-subset(Datos6, subset=(Datos6$gen_sp == names(a[19])))
u19<-u19[sample((nrow(u19)), 100), ]
u20<-subset(Datos6, subset=(Datos6$gen_sp == names(a[20])))
u20<-u20[sample((nrow(u20)), 100), ]
u21<-subset(Datos6, subset=(Datos6$gen_sp == names(a[21])))
u21<-u21[sample((nrow(u21)), 100), ]
u22<-subset(Datos6, subset=(Datos6$gen_sp == names(a[22])))
u22<-u22[sample((nrow(u22)), 100), ]
u23<-subset(Datos6, subset=(Datos6$gen_sp == names(a[23])))
u23<-u23[sample((nrow(u23)), 100), ]
u24<-subset(Datos6, subset=(Datos6$gen_sp == names(a[24])))
u24<-u24[sample((nrow(u24)), 100), ]
u25<-subset(Datos6, subset=(Datos6$gen_sp == names(a[25])))
u25<-u25[sample((nrow(u25)), 100), ]
u26<-subset(Datos6, subset=(Datos6$gen_sp == names(a[26])))
u26<-u26[sample((nrow(u26)), 100), ]
u27<-subset(Datos6, subset=(Datos6$gen_sp == names(a[27])))
u27<-u27[sample((nrow(u27)), 100), ]
u28<-subset(Datos6, subset=(Datos6$gen_sp == names(a[28])))
u28<-u28[sample((nrow(u28)), 100), ]
u29<-subset(Datos6, subset=(Datos6$gen_sp == names(a[29])))
u29<-u29[sample((nrow(u29)), 100), ]
u30<-subset(Datos6, subset=(Datos6$gen_sp == names(a[30])))
u30<-u30[sample((nrow(u30)), 100), ]
u31<-subset(Datos6, subset=(Datos6$gen_sp == names(a[31])))
u31<-u31[sample((nrow(u31)), 100), ]
u32<-subset(Datos6, subset=(Datos6$gen_sp == names(a[32])))
u32<-u32[sample((nrow(u32)), 100), ]
u33<-subset(Datos6, subset=(Datos6$gen_sp == names(a[33])))
u33<-u33[sample((nrow(u33)), 100), ]
u34<-subset(Datos6, subset=(Datos6$gen_sp == names(a[34])))
u34<-u34[sample((nrow(u34)), 100), ]
u35<-subset(Datos6, subset=(Datos6$gen_sp == names(a[35])))
u35<-u35[sample((nrow(u35)), 100), ]
u36<-subset(Datos6, subset=(Datos6$gen_sp == names(a[36])))
u36<-u36[sample((nrow(u36)), 100), ]
u37<-subset(Datos6, subset=(Datos6$gen_sp == names(a[37])))
u37<-u37[sample((nrow(u37)), 100), ]
u38<-subset(Datos6, subset=(Datos6$gen_sp == names(a[38])))
u38<-u38[sample((nrow(u38)), 100), ]
u39<-subset(Datos6, subset=(Datos6$gen_sp == names(a[39])))
u39<-u39[sample((nrow(u39)), 100), ]
u40<-subset(Datos6, subset=(Datos6$gen_sp == names(a[40])))
u40<-u40[sample((nrow(u40)), 100), ]
u41<-subset(Datos6, subset=(Datos6$gen_sp == names(a[41])))
u41<-u41[sample((nrow(u41)), 100), ]
u42<-subset(Datos6, subset=(Datos6$gen_sp == names(a[42])))
u42<-u42[sample((nrow(u42)), 100), ]
u43<-subset(Datos6, subset=(Datos6$gen_sp == names(a[43])))
u43<-u43[sample((nrow(u43)), 100), ]
u45<-subset(Datos6, subset=(Datos6$gen_sp == names(a[45])))
u45<-u45[sample((nrow(u45)), 100), ]
u46<-subset(Datos6, subset=(Datos6$gen_sp == names(a[46])))
u46<-u46[sample((nrow(u46)), 100), ]

Datos.uso <- rbind(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21,u22,u23,u24,u25,u26,u27,u28,u29,u30,u31,u32,u33,u34,u35,u36,u37,u38,u39,u40,u41,u42,u43,u45,u46)

#Sacamos matriz de interacción, en forma de lista, con todas las abundancias de cada
#especie en cada habitat
int<-tapply(Datos.uso$habitat.extracted, Datos.uso$gen_sp, FUN = summary)
#Deslistamos en un vector más fácil para operar
int.u<- unlist(int)
int.u
length(int.u)
length(d)
#Sustituimos abundancia por presencia/ausencia
int.u<-replace(int.u, int.u>1,1)
row.names(d)
int.u <- as.data.frame(int.u)
row.names(int.u)
View(int.u)
de<-int.u
##
###Hacemos un loop para rareficar, tomando muestras igualando el muestreo de cada especie

a <- (summary(Datos6$gen_sp))
for (n in 1:99) {

  u1<-subset(Datos6, subset=(Datos6$gen_sp == names(a[1])))
  u1<-u1[sample((nrow(u1)), 100), ]
  summary(u1$habitat.extracted)
  u2<-subset(Datos6, subset=(Datos6$gen_sp == names(a[2])))
  u2<-u2[sample((nrow(u2)), 100), ]
  u3<-subset(Datos6, subset=(Datos6$gen_sp == names(a[3])))
  u3<-u3[sample((nrow(u3)), 100), ]
  u4<-subset(Datos6, subset=(Datos6$gen_sp == names(a[4])))
  u4<-u4[sample((nrow(u4)), 100), ]
  u5<-subset(Datos6, subset=(Datos6$gen_sp == names(a[5])))
  u5<-u5[sample((nrow(u5)), 100), ]
  u6<-subset(Datos6, subset=(Datos6$gen_sp == names(a[6])))
  u6<-u6[sample((nrow(u6)), 100), ]
  u7<-subset(Datos6, subset=(Datos6$gen_sp == names(a[7])))
  u7<-u7[sample((nrow(u7)), 100), ]
  u8<-subset(Datos6, subset=(Datos6$gen_sp == names(a[8])))
  u8<-u8[sample((nrow(u8)), 100), ]
  u9<-subset(Datos6, subset=(Datos6$gen_sp == names(a[9])))
  u9<-u9[sample((nrow(u9)), 100), ]
  u10<-subset(Datos6, subset=(Datos6$gen_sp == names(a[10])))
  u10<-u10[sample((nrow(u10)), 100), ]
  u11<-subset(Datos6, subset=(Datos6$gen_sp == names(a[11])))
  u11<-u11[sample((nrow(u11)), 100), ]
  u12<-subset(Datos6, subset=(Datos6$gen_sp == names(a[12])))
  u12<-u12[sample((nrow(u12)), 100), ]
  u13<-subset(Datos6, subset=(Datos6$gen_sp == names(a[13])))
  u13<-u13[sample((nrow(u13)), 100), ]
  u14<-subset(Datos6, subset=(Datos6$gen_sp == names(a[14])))
  u14<-u14[sample((nrow(u14)), 100), ]
  u15<-subset(Datos6, subset=(Datos6$gen_sp == names(a[15])))
  u15<-u15[sample((nrow(u15)), 100), ]
  u16<-subset(Datos6, subset=(Datos6$gen_sp == names(a[16])))
  u16<-u16[sample((nrow(u16)), 100), ]
  u17<-subset(Datos6, subset=(Datos6$gen_sp == names(a[17])))
  u17<-u17[sample((nrow(u17)), 100), ]
  u18<-subset(Datos6, subset=(Datos6$gen_sp == names(a[18])))
  u18<-u18[sample((nrow(u18)), 100), ]
  u19<-subset(Datos6, subset=(Datos6$gen_sp == names(a[19])))
  u19<-u19[sample((nrow(u19)), 100), ]
  u20<-subset(Datos6, subset=(Datos6$gen_sp == names(a[20])))
  u20<-u20[sample((nrow(u20)), 100), ]
  u21<-subset(Datos6, subset=(Datos6$gen_sp == names(a[21])))
  u21<-u21[sample((nrow(u21)), 100), ]
  u22<-subset(Datos6, subset=(Datos6$gen_sp == names(a[22])))
  u22<-u22[sample((nrow(u22)), 100), ]
  u23<-subset(Datos6, subset=(Datos6$gen_sp == names(a[23])))
  u23<-u23[sample((nrow(u23)), 100), ]
  u24<-subset(Datos6, subset=(Datos6$gen_sp == names(a[24])))
  u24<-u24[sample((nrow(u24)), 100), ]
  u25<-subset(Datos6, subset=(Datos6$gen_sp == names(a[25])))
  u25<-u25[sample((nrow(u25)), 100), ]
  u26<-subset(Datos6, subset=(Datos6$gen_sp == names(a[26])))
  u26<-u26[sample((nrow(u26)), 100), ]
  u27<-subset(Datos6, subset=(Datos6$gen_sp == names(a[27])))
  u27<-u27[sample((nrow(u27)), 100), ]
  u28<-subset(Datos6, subset=(Datos6$gen_sp == names(a[28])))
  u28<-u28[sample((nrow(u28)), 100), ]
  u29<-subset(Datos6, subset=(Datos6$gen_sp == names(a[29])))
  u29<-u29[sample((nrow(u29)), 100), ]
  u30<-subset(Datos6, subset=(Datos6$gen_sp == names(a[30])))
  u30<-u30[sample((nrow(u30)), 100), ]
  u31<-subset(Datos6, subset=(Datos6$gen_sp == names(a[31])))
  u31<-u31[sample((nrow(u31)), 100), ]
  u32<-subset(Datos6, subset=(Datos6$gen_sp == names(a[32])))
  u32<-u32[sample((nrow(u32)), 100), ]
  u33<-subset(Datos6, subset=(Datos6$gen_sp == names(a[33])))
  u33<-u33[sample((nrow(u33)), 100), ]
  u34<-subset(Datos6, subset=(Datos6$gen_sp == names(a[34])))
  u34<-u34[sample((nrow(u34)), 100), ]
  u35<-subset(Datos6, subset=(Datos6$gen_sp == names(a[35])))
  u35<-u35[sample((nrow(u35)), 100), ]
  u36<-subset(Datos6, subset=(Datos6$gen_sp == names(a[36])))
  u36<-u36[sample((nrow(u36)), 100), ]
  u37<-subset(Datos6, subset=(Datos6$gen_sp == names(a[37])))
  u37<-u37[sample((nrow(u37)), 100), ]
  u38<-subset(Datos6, subset=(Datos6$gen_sp == names(a[38])))
  u38<-u38[sample((nrow(u38)), 100), ]
  u39<-subset(Datos6, subset=(Datos6$gen_sp == names(a[39])))
  u39<-u39[sample((nrow(u39)), 100), ]
  u40<-subset(Datos6, subset=(Datos6$gen_sp == names(a[40])))
  u40<-u40[sample((nrow(u40)), 100), ]
  u41<-subset(Datos6, subset=(Datos6$gen_sp == names(a[41])))
  u41<-u41[sample((nrow(u41)), 100), ]
  u42<-subset(Datos6, subset=(Datos6$gen_sp == names(a[42])))
  u42<-u42[sample((nrow(u42)), 100), ]
  u43<-subset(Datos6, subset=(Datos6$gen_sp == names(a[43])))
  u43<-u43[sample((nrow(u43)), 100), ]
  u45<-subset(Datos6, subset=(Datos6$gen_sp == names(a[45])))
  u45<-u45[sample((nrow(u45)), 100), ]
  u46<-subset(Datos6, subset=(Datos6$gen_sp == names(a[46])))
  u46<-u46[sample((nrow(u46)), 100), ]
  Datos.uso <- rbind(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21,u22,u23,u24,u25,u26,u27,u28,u29,u30,u31,u32,u33,u34,u35,u36,u37,u38,u39,u40,u41,u42,u43,u45,u46)
  summary(Datos.uso$habitat.extracted)  
  #Sacamos matriz de interacción, en forma de lista, con todas las abundancias de cada
  #especie en cada habitat
  int<-tapply(Datos.uso$habitat.extracted, Datos.uso$gen_sp, FUN = summary)
  int
  #Deslistamos en un vector más fácil para operar
  int.u<- unlist(int)
  int.u
  length(int.u)
  length(d)
  #Sustituimos abundancia por presencia/ausencia
  int.u<-replace(int.u, int.u>1,1)
  row.names(d)
  int.u <- as.data.frame(int.u)
  row.names(int.u)
  #Unimos el vector de presencia/ausencia al data frame, al unir por row.names
  #nos crea en la unión una colunma nueva, que eliminamos
  de <- merge(de,int.u, by="row.names",all=TRUE)
  
  row.names(de)<-row.names(int.u)
  de<-de[,-1]
  }
View(de)
#Hacemos una columna con la suma de las 100 repeticiones, si el resultado es mayor a uno
#hay USO
de$suma <- rowSums(de) 
de$uso <- rowSums(de) 
de$uso <- replace(de$uso, de$uso>0, "YES")
de$uso <- replace(de$uso, de$uso==0, "NO")
de$uso.n <- de$suma
de$uso.n <- replace(de$uso.n, de$uso.n >1,1)
de$rnames <- row.names(de) 
#Creamos data frame con los resultados del uso
uso.p<- data.frame(de$rnames,de$suma,de$uso.n,de$uso)
row.names(uso.p) <- uso.p[,1]
espacio <- regexpr(pattern = " ", uso.p$d.rnames)

View(uso.p)

summary(Datos6$gen_sp, maxsum=46)
View(uso.p)
uso.p.yesno <- uso.p[,c(1,4)]
View(uso.p.yesno)

#Rareficando el número de habitats usados-----

#Creamos data frame en blanco con los nombres de las especies
a<-summary(Datos6$gen_sp)

blanknames <- c("Agapostemon virescens","Andrena carlini","Andrena cressonii","Andrena erigeniae","Andrena nasonii","Andrena perplexa","Andrena violae","Apis mellifera","Augochlora pura","Augochlorella aurata","Bombus bimaculatus","Bombus fervidus","Bombus griseocollis","Calliopsis andreniformis","Ceratina calcarata/dupla/mikmaqi","Ceratina strenua","Halictus confusus","Halictus ligatus/poeyi","Halictus rubicundus","Hylaeus affinis/modestus","Lasioglossum bruneri","Lasioglossum callidum","Lasioglossum coriaceum","Lasioglossum cressonii","Lasioglossum hitchensi","Lasioglossum illinoense","Lasioglossum imitatum","Lasioglossum near_admirandum","Lasioglossum oblongum","Lasioglossum pectorale","Lasioglossum pilosum","Lasioglossum tegulare","Lasioglossum versatum","Megachile brevis","Megachile mendica","Melissodes bimaculata","Nomada bidentate_group","Nomada pygmaea","Osmia atriventris","Osmia bucephala","Osmia georgica","Osmia pumila","Osmia taurus","Ptilothrix bombiformis","Xylocopa virginica")
blank<-data.frame(row.names = blanknames)
for (n in 1:100) { 
#Tomamos una muestra de 100 individuos de cada una de las especies más abundantes  
u1<-subset(Datos6, subset=(Datos6$gen_sp == names(a[1])))
u1<-u1[sample((nrow(u1)), 100), ]
u2<-subset(Datos6, subset=(Datos6$gen_sp == names(a[2])))
u2<-u2[sample((nrow(u2)), 100), ]
u3<-subset(Datos6, subset=(Datos6$gen_sp == names(a[3])))
u3<-u3[sample((nrow(u3)), 100), ]
u4<-subset(Datos6, subset=(Datos6$gen_sp == names(a[4])))
u4<-u4[sample((nrow(u4)), 100), ]
u5<-subset(Datos6, subset=(Datos6$gen_sp == names(a[5])))
u5<-u5[sample((nrow(u5)), 100), ]
u6<-subset(Datos6, subset=(Datos6$gen_sp == names(a[6])))
u6<-u6[sample((nrow(u6)), 100), ]
u7<-subset(Datos6, subset=(Datos6$gen_sp == names(a[7])))
u7<-u7[sample((nrow(u7)), 100), ]
u8<-subset(Datos6, subset=(Datos6$gen_sp == names(a[8])))
u8<-u8[sample((nrow(u8)), 100), ]
u9<-subset(Datos6, subset=(Datos6$gen_sp == names(a[9])))
u9<-u9[sample((nrow(u9)), 100), ]
u10<-subset(Datos6, subset=(Datos6$gen_sp == names(a[10])))
u10<-u10[sample((nrow(u10)), 100), ]
u11<-subset(Datos6, subset=(Datos6$gen_sp == names(a[11])))
u11<-u11[sample((nrow(u11)), 100), ]
u12<-subset(Datos6, subset=(Datos6$gen_sp == names(a[12])))
u12<-u12[sample((nrow(u12)), 100), ]
u13<-subset(Datos6, subset=(Datos6$gen_sp == names(a[13])))
u13<-u13[sample((nrow(u13)), 100), ]
u14<-subset(Datos6, subset=(Datos6$gen_sp == names(a[14])))
u14<-u14[sample((nrow(u14)), 100), ]
u15<-subset(Datos6, subset=(Datos6$gen_sp == names(a[15])))
u15<-u15[sample((nrow(u15)), 100), ]
u16<-subset(Datos6, subset=(Datos6$gen_sp == names(a[16])))
u16<-u16[sample((nrow(u16)), 100), ]
u17<-subset(Datos6, subset=(Datos6$gen_sp == names(a[17])))
u17<-u17[sample((nrow(u17)), 100), ]
u18<-subset(Datos6, subset=(Datos6$gen_sp == names(a[18])))
u18<-u18[sample((nrow(u18)), 100), ]
u19<-subset(Datos6, subset=(Datos6$gen_sp == names(a[19])))
u19<-u19[sample((nrow(u19)), 100), ]
u20<-subset(Datos6, subset=(Datos6$gen_sp == names(a[20])))
u20<-u20[sample((nrow(u20)), 100), ]
u21<-subset(Datos6, subset=(Datos6$gen_sp == names(a[21])))
u21<-u21[sample((nrow(u21)), 100), ]
u22<-subset(Datos6, subset=(Datos6$gen_sp == names(a[22])))
u22<-u22[sample((nrow(u22)), 100), ]
u23<-subset(Datos6, subset=(Datos6$gen_sp == names(a[23])))
u23<-u23[sample((nrow(u23)), 100), ]
u24<-subset(Datos6, subset=(Datos6$gen_sp == names(a[24])))
u24<-u24[sample((nrow(u24)), 100), ]
u25<-subset(Datos6, subset=(Datos6$gen_sp == names(a[25])))
u25<-u25[sample((nrow(u25)), 100), ]
u26<-subset(Datos6, subset=(Datos6$gen_sp == names(a[26])))
u26<-u26[sample((nrow(u26)), 100), ]
u27<-subset(Datos6, subset=(Datos6$gen_sp == names(a[27])))
u27<-u27[sample((nrow(u27)), 100), ]
u28<-subset(Datos6, subset=(Datos6$gen_sp == names(a[28])))
u28<-u28[sample((nrow(u28)), 100), ]
u29<-subset(Datos6, subset=(Datos6$gen_sp == names(a[29])))
u29<-u29[sample((nrow(u29)), 100), ]
u30<-subset(Datos6, subset=(Datos6$gen_sp == names(a[30])))
u30<-u30[sample((nrow(u30)), 100), ]
u31<-subset(Datos6, subset=(Datos6$gen_sp == names(a[31])))
u31<-u31[sample((nrow(u31)), 100), ]
u32<-subset(Datos6, subset=(Datos6$gen_sp == names(a[32])))
u32<-u32[sample((nrow(u32)), 100), ]
u33<-subset(Datos6, subset=(Datos6$gen_sp == names(a[33])))
u33<-u33[sample((nrow(u33)), 100), ]
u34<-subset(Datos6, subset=(Datos6$gen_sp == names(a[34])))
u34<-u34[sample((nrow(u34)), 100), ]
u35<-subset(Datos6, subset=(Datos6$gen_sp == names(a[35])))
u35<-u35[sample((nrow(u35)), 100), ]
u36<-subset(Datos6, subset=(Datos6$gen_sp == names(a[36])))
u36<-u36[sample((nrow(u36)), 100), ]
u37<-subset(Datos6, subset=(Datos6$gen_sp == names(a[37])))
u37<-u37[sample((nrow(u37)), 100), ]
u38<-subset(Datos6, subset=(Datos6$gen_sp == names(a[38])))
u38<-u38[sample((nrow(u38)), 100), ]
u39<-subset(Datos6, subset=(Datos6$gen_sp == names(a[39])))
u39<-u39[sample((nrow(u39)), 100), ]
u40<-subset(Datos6, subset=(Datos6$gen_sp == names(a[40])))
u40<-u40[sample((nrow(u40)), 100), ]
u41<-subset(Datos6, subset=(Datos6$gen_sp == names(a[41])))
u41<-u41[sample((nrow(u41)), 100), ]
u42<-subset(Datos6, subset=(Datos6$gen_sp == names(a[42])))
u42<-u42[sample((nrow(u42)), 100), ]
u43<-subset(Datos6, subset=(Datos6$gen_sp == names(a[43])))
u43<-u43[sample((nrow(u43)), 100), ]
u45<-subset(Datos6, subset=(Datos6$gen_sp == names(a[45])))
u45<-u45[sample((nrow(u45)), 100), ]
u46<-subset(Datos6, subset=(Datos6$gen_sp == names(a[46])))
u46<-u46[sample((nrow(u46)), 100), ]
#Hacemos dataframe con los muestreos
Datos.uso.2 <- rbind(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21,u22,u23,u24,u25,u26,u27,u28,u29,u30,u31,u32,u33,u34,u35,u36,u37,u38,u39,u40,u41,u42,u43,u45,u46)
View(Datos.uso.2)
jiji <-aggregate (Datos.uso.2$habitat.extracted ~ Datos.uso.2$gen_sp, FUN = summary, Datos.uso.2)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref
meji
#Transformamos la abundancia en presencia o ausencia, 
row.names(meji)<- meji[,1]
mejib<-meji[,-c(1,11)]
mejib<-replace(mejib, mejib>1,1)
momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momo<-momo[,-10]
blank[,n]<-rowSums(momo)
}
View(blank)
#Vector con el número de habitats usado rareficado
use.rarefied<-rowSums(blank)/100
use.rarefied

#Cuantiles de las preferencias de habitats------
summary(Datos6$gen_sp, maxsum=500)
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
View(observeds)

nmodels

lista.bl<-list()
lista.bl.c<-list()
lista.co<-list()
lista.co.c<-list()
lista.cc<-list()
lista.cc.c<-list()
lista.df<-list()
lista.df.c<-list()
lista.dh<-list()
lista.dh.c<-list()
lista.dl<-list()
lista.dl.c<-list()
lista.dm<-list()
lista.dm.c<-list()
lista.do<-list()
lista.do.c<-list()
lista.eh<-list()
lista.eh.c<-list()
lista.ef<-list()
lista.ef.c<-list()
lista.hp<-list()
lista.hp.c<-list()
lista.hb<-list()
lista.hb.c<-list()
lista.mf<-list()
lista.mf.c<-list()
lista.ss<-list()
lista.ss.c<-list()
lista.ww<-list()
lista.ww.c<-list()
uno.uno<-NULL
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,1]
  }
  lista.bl.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.bl[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,1])
  
}

for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,2]
  }
  lista.co.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.co[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,2])
  
}

for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,3]
  }
  lista.cc.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.cc[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,3])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,4]
  }
  lista.df.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.df[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,4])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,5]
  }
  lista.dh.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.dh[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,5])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,6]
  }
  lista.dl.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.dl[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,6])
  
}

for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,7]
  }
  lista.dm.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.dm[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,7])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,8]
  }
  lista.do.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.do[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,8])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,9]
  }
  lista.eh.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.eh[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,9])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,10]
  }
  lista.ef.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.ef[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,10])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,11]
  }
  lista.hp.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.hp[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,11])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,12]
  }
  lista.hb.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.hb[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,12])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,13]
  }
  lista.mf.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.mf[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,13])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,14]
  }
  lista.ss.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.ss[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,14])
  
}
for (k in 1:nrow(observeds)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels[[n]][k,15]
  }
  lista.ww.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.ww[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds[k,15])
  
}
View(observeds)
lista.bl
lista.bl.c
lista.co
lista.co.c
lista.cc
lista.cc.c
lista.df
lista.df.c
lista.dh
lista.dl
lista.dm
lista.dm.c
lista.do
lista.do.c
lista.eh
lista.eh.c
lista.ef.c
lista.ef
#lista.hp
lista.hb
lista.mf
lista.ss
lista.ww
lista.ww.c

##Uso para los habitats agrupados tomando 100 muestras de los individuos más abundantes----
summary(Datos.g$habitat.grouped)


b
d

f<-summary(Datos.g$gen_sp)
View(Datos.g)
blanknames <- c("Agapostemon virescens","Andrena carlini","Andrena cressonii","Andrena erigeniae","Andrena nasonii","Andrena perplexa","Andrena violae","Apis mellifera","Augochlora pura","Augochlorella aurata","Bombus bimaculatus","Bombus fervidus","Bombus griseocollis","Calliopsis andreniformis","Ceratina calcarata/dupla/mikmaqi","Ceratina strenua","Halictus confusus","Halictus ligatus/poeyi","Halictus rubicundus","Hylaeus affinis/modestus","Lasioglossum bruneri","Lasioglossum callidum","Lasioglossum coriaceum","Lasioglossum cressonii","Lasioglossum hitchensi","Lasioglossum illinoense","Lasioglossum imitatum","Lasioglossum near_admirandum","Lasioglossum oblongum","Lasioglossum pectorale","Lasioglossum pilosum","Lasioglossum tegulare","Lasioglossum versatum","Megachile brevis","Megachile mendica","Melissodes bimaculata","Nomada bidentate_group","Nomada pygmaea","Osmia atriventris","Osmia bucephala","Osmia georgica","Osmia pumila","Osmia taurus","Ptilothrix bombiformis","Xylocopa virginica")
blank.g<-data.frame(row.names = blanknames)
for (n in 1:100) { 
  #Tomamos una muestra de 100 individuos de cada una de las especies más abundantes  
  u1<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[1])))
  u1<-u1[sample((nrow(u1)), 100), ]
  u2<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[2])))
  u2<-u2[sample((nrow(u2)), 100), ]
  u3<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[3])))
  u3<-u3[sample((nrow(u3)), 100), ]
  u4<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[4])))
  u4<-u4[sample((nrow(u4)), 100), ]
  u5<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[5])))
  u5<-u5[sample((nrow(u5)), 100), ]
  u6<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[6])))
  u6<-u6[sample((nrow(u6)), 100), ]
  u7<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[7])))
  u7<-u7[sample((nrow(u7)), 100), ]
  u8<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[8])))
  u8<-u8[sample((nrow(u8)), 100), ]
  u9<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[9])))
  u9<-u9[sample((nrow(u9)), 100), ]
  u10<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[10])))
  u10<-u10[sample((nrow(u10)), 100), ]
  u11<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[11])))
  u11<-u11[sample((nrow(u11)), 100), ]
  u12<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[12])))
  u12<-u12[sample((nrow(u12)), 100), ]
  u13<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[13])))
  u13<-u13[sample((nrow(u13)), 100), ]
  u14<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[14])))
  u14<-u14[sample((nrow(u14)), 100), ]
  u15<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[15])))
  u15<-u15[sample((nrow(u15)), 100), ]
  u16<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[16])))
  u16<-u16[sample((nrow(u16)), 100), ]
  u17<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[17])))
  u17<-u17[sample((nrow(u17)), 100), ]
  u18<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[18])))
  u18<-u18[sample((nrow(u18)), 100), ]
  u19<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[19])))
  u19<-u19[sample((nrow(u19)), 100), ]
  u20<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[20])))
  u20<-u20[sample((nrow(u20)), 100), ]
  u21<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[21])))
  u21<-u21[sample((nrow(u21)), 100), ]
  u22<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[22])))
  u22<-u22[sample((nrow(u22)), 100), ]
  u23<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[23])))
  u23<-u23[sample((nrow(u23)), 100), ]
  u24<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[24])))
  u24<-u24[sample((nrow(u24)), 100), ]
  u25<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[25])))
  u25<-u25[sample((nrow(u25)), 100), ]
  u26<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[26])))
  u26<-u26[sample((nrow(u26)), 100), ]
  u27<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[27])))
  u27<-u27[sample((nrow(u27)), 100), ]
  u28<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[28])))
  u28<-u28[sample((nrow(u28)), 100), ]
  u29<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[29])))
  u29<-u29[sample((nrow(u29)), 100), ]
  u30<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[30])))
  u30<-u30[sample((nrow(u30)), 100), ]
  u31<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[31])))
  u31<-u31[sample((nrow(u31)), 100), ]
  u32<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[32])))
  u32<-u32[sample((nrow(u32)), 100), ]
  u33<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[33])))
  u33<-u33[sample((nrow(u33)), 100), ]
  u34<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[34])))
  u34<-u34[sample((nrow(u34)), 100), ]
  u35<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[35])))
  u35<-u35[sample((nrow(u35)), 100), ]
  u36<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[36])))
  u36<-u36[sample((nrow(u36)), 100), ]
  u37<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[37])))
  u37<-u37[sample((nrow(u37)), 100), ]
  u38<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[38])))
  u38<-u38[sample((nrow(u38)), 100), ]
  u39<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[39])))
  u39<-u39[sample((nrow(u39)), 100), ]
  u40<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[40])))
  u40<-u40[sample((nrow(u40)), 100), ]
  u41<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[41])))
  u41<-u41[sample((nrow(u41)), 100), ]
  u42<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[42])))
  u42<-u42[sample((nrow(u42)), 100), ]
  u43<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[43])))
  u43<-u43[sample((nrow(u43)), 100), ]
  u45<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[45])))
  u45<-u45[sample((nrow(u45)), 100), ]
  u46<-subset(Datos.g, subset=(Datos.g$gen_sp == names(f[46])))
  u46<-u46[sample((nrow(u46)), 100), ]
  #Hacemos dataframe con los muestreos
  Datos.uso.g <- rbind(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21,u22,u23,u24,u25,u26,u27,u28,u29,u30,u31,u32,u33,u34,u35,u36,u37,u38,u39,u40,u41,u42,u43,u45,u46)
  View(Datos.uso.g)
  jiji <-aggregate (Datos.uso.g$habitat.grouped ~ Datos.uso.g$gen_sp, FUN = summary, Datos.uso.g)
  juju<-c(jiji)
  juju
  habpref<-as.data.frame(juju)
  meji<-habpref
  meji
  #Transformamos la abundancia en presencia o ausencia, 
  row.names(meji)<- meji[,1]
  View(mejib)
  mejib<-meji[,-c(1,6)]
  mejib<-replace(mejib, mejib>1,1)
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  
  blank.g[,n]<-rowSums(momo)
}
View(blank.g)
#Vector con el número de habitats usado rareficado
use.rarefied.g<-rowSums(blank.g)/100
use.rarefied.g
observeds.g[,4]
#Habitats agrupados, ver los cuantiles de las preferencias de habitats------
#Can you feel it?

#Sacamos la matriz de observados
summary(Datos.g$gen_sp, maxsum=500)
jiji <-aggregate (Datos.g$habitat.grouped ~ Datos.g$gen_sp, FUN = summary, Datos.g)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)

meji<-habpref[-1,]
momo<-apply(meji, MARGIN=2, FUN=as.numeric)
row.names(momo)<- meji[,1]
momomo<-momo[,-c(1,6)]
momomo<-as.data.frame(momomo)
View(momomo)
#Filtramos para que solo aparezcan las especies que solo tienen >100 observaciones

summmm<-rowSums(momomo)
momomosb<-cbind(momomo,summmm)
momomost<-subset(momomosb, subset=momomosb$summmm>100)

View(momomost)
momomost<-momomost[,1:4]
observeds.g<-momomost
View(observeds.g)
nmodels
rownames(observeds.g)
a

library(bipartite)
#Con este comando extraemos 1000 matrices de interacción de esperados
nmodels.g<-nullmodel(observeds.g, N=1000, method="r2dtable")


View(Datos.g2)
View(Datos.g)


names(observeds.g)
#Hacemos unas listas para ver los cuantiles en los cuatro diferentes grupos

lista.ong<-list()
lista.ong.c<-list()
lista.pcg<-list()
lista.pcg.c<-list()
lista.fog<-list()
lista.fog.c<-list()
lista.urg<-list()
lista.urg.c<-list()
uno.uno<-NULL
for (k in 1:nrow(observeds.g)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels.g[[n]][k,1]
  }
  lista.ong.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.ong[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds.g[k,1])
  
}

for (k in 1:nrow(observeds.g)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels.g[[n]][k,2]
  }
  lista.pcg.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.pcg[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds.g[k,2])
  
}
nmodels.g[[1000]][45,3]
for (k in 1:nrow(observeds.g)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels.g[[n]][k,3]
  }
  lista.fog.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.fog[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds.g[k,3])
  
}
for (k in 1:nrow(observeds.g)) {
  
  for (n in 1:1000) {
    uno.uno[n]<-nmodels.g[[n]][k,4]
  }
  lista.urg.c[[k]]<-quantile(uno.uno,probs = seq(0,1,0.01))
  lista.urg[[k]]<-which(quantile(uno.uno,probs = seq(0,1,0.01))==observeds.g[k,4])
  
}
observeds.g
lista.ong
lista.ong.c
lista.pcg
lista.pcg.c
lista.fog
lista.fog.c
lista.urg
lista.urg.c


View(observeds.g)
getwd()
write.csv(observeds.g,"observeds.csv")
Datos6

seq(0,1,0.05)
quantiles.grouped <- read.csv("~/Desktop/Tesis/R/habpref full data/Results/Cuantiles habitat agrupados.csv", row.names = 1)
quantiles.habitat <- read.csv("~/Desktop/Tesis/R/habpref full data/Results/Cuantiles habitats.csv", row.names=1)
View(quantiles.habitat)

??histogram
histogram(quantiles.grouped$Urban, breaks = seq(0,1,0.05), col = "grey", xlab="Quantiles", ylim = c(0,33), main= "Urban Preference Quantiles")
histogram(quantiles.grouped$Forests, breaks = seq(0,1,0.05), col = "grey", xlab="Quantiles", ylim = c(0,33), main= "Forest Preference Quantiles")
histogram(quantiles.grouped$Pasture.and.Crops, breaks = seq(0,1,0.05), col = "grey", xlab="Quantiles", ylim = c(0,33), main= "Crops and Pasture Preference Quantiles")

#Queremos saber el número de especies que tienen preferencia
#Preferencia general
quantiles.habitat<-quantiles.habitat[,-1]
quantiles.grouped<-quantiles.grouped[,-1]
prefs<-subset(quantiles.habitat, subset=(quantiles.habitat$Barren.Land>0.949|quantiles.habitat$Coastal>0.949|quantiles.habitat$Cultivated.Crops>0.949|quantiles.habitat$Deciduous.Forest>0.949|quantiles.habitat$Developed..High.Intensity>0.949|quantiles.habitat$Developed..Low.Intensity>0.949|quantiles.habitat$Developed..Medium.Intensity>0.949|quantiles.habitat$Developed..Open.Space>0.949|quantiles.habitat$Emergent.Herbaceuous.Wetlands>0.949|quantiles.habitat$Evergreen.Forest>0.949|quantiles.habitat$Herbaceuous.Hay.Pasture>0.949|quantiles.habitat$Mixed.Forest>0.949|quantiles.habitat$Shrub.Scrub>0.949|quantiles.habitat$Woody.Wetlands>0.949))
nrow(prefs)
prefs.g<-subset(quantiles.grouped, subset=(quantiles.grouped$Forests>0.949|quantiles.grouped$Urban>0.949|quantiles.grouped$Pasture.and.Crops>0.949|quantiles.grouped$Other.Natural>0.949))
nrow(quantiles.grouped)
nrow(prefs.g)
nrow(prefs.f<-subset(quantiles.grouped, subset=(quantiles.grouped$Forests>0.949)))
nrow(prefs.u<-subset(quantiles.grouped, subset=(quantiles.grouped$Urban>0.949)))
prefs.a<-subset(quantiles.grouped, subset=(quantiles.grouped$Pasture.and.Crops>0.949))

#Avoidance
avoid.u<-subset(quantiles.grouped, subset=(quantiles.grouped$Urban<0.0501))
avoid.a<-subset(quantiles.grouped, subset=(quantiles.grouped$Pasture.and.Crops<0.0501))
avoid.f<-subset(quantiles.grouped, subset=(quantiles.grouped$Forests<0.0501))

#Construimos una lista para ver fácilmente exploiters y avoiders, tanto para los habitats
#agrupados como para todos
PREFERENCIAS.g<-list()
PREFERENCIAS.g$Preference.forest<-row.names(prefs.f)
PREFERENCIAS.g$Preference.urban<-row.names(prefs.u)
PREFERENCIAS.g$Preference.crops<-row.names(prefs.a)
PREFERENCIAS.g$Avoidance.forest<-row.names(avoid.f)
PREFERENCIAS.g$Avoidance.urban<-row.names(avoid.u)
PREFERENCIAS.g$Avoidance.crops<-row.names(avoid.a)
PREFERENCIAS.g

PREFERENCIAS<-list()
PREFERENCIAS$Preference.barrenland<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Barren.Land>0.949)))
PREFERENCIAS$Preference.coastal<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Coastal>0.949)))
PREFERENCIAS$Preference.crops<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Cultivated.Crops>0.949)))
PREFERENCIAS$Preference.deciduousforests<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Deciduous.Forest>0.949)))
PREFERENCIAS$Preference.developedhigh<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Developed..High.Intensity>0.949)))
PREFERENCIAS$Preference.developedlow<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Developed..Low.Intensity>0.949)))
PREFERENCIAS$Preference.developedmedium<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Developed..Medium.Intensity>0.949)))
PREFERENCIAS$Preference.developedopen<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Developed..Open.Space>0.949)))
PREFERENCIAS$Preference.emergentherbaceouswetlands<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Emergent.Herbaceuous.Wetlands>0.949)))
PREFERENCIAS$Preference.evergreenforest<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Evergreen.Forest>0.949)))
PREFERENCIAS$Preference.herbaceoushaypasture<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Herbaceuous.Hay.Pasture>0.949)))
PREFERENCIAS$Preference.mixedforest<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Mixed.Forest>0.949)))
PREFERENCIAS$Preference.shrubscrub<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Shrub.Scrub>0.949)))
PREFERENCIAS$Preference.woodywetlands<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Woody.Wetlands>0.949)))

PREFERENCIAS$Avoidance.barrenland<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Barren.Land<0.0501)))
PREFERENCIAS$Avoidance.coastal<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Coastal<0.0501)))
PREFERENCIAS$Avoidance.crops<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Cultivated.Crops<0.0501)))
PREFERENCIAS$Avoidance.deciduousforests<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Deciduous.Forest<0.0501)))
PREFERENCIAS$Avoidance.developedhigh<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Developed..High.Intensity<0.0501)))
PREFERENCIAS$Avoidance.developedlow<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Developed..Low.Intensity<0.0501)))
PREFERENCIAS$Avoidance.developedmedium<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Developed..Medium.Intensity<0.0501)))
PREFERENCIAS$Avoidance.developedopen<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Developed..Open.Space<0.0501)))
PREFERENCIAS$Avoidance.emergentherbaceouswetlands<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Emergent.Herbaceuous.Wetlands<0.0501)))
PREFERENCIAS$Avoidance.evergreenforest<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Evergreen.Forest<0.0501)))
PREFERENCIAS$Avoidance.herbaceoushaypasture<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Herbaceuous.Hay.Pasture<0.0501)))
PREFERENCIAS$Avoidance.mixedforest<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Mixed.Forest<0.0501)))
PREFERENCIAS$Avoidance.shrubscrub<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Shrub.Scrub<0.0501)))
PREFERENCIAS$Avoidance.woodywetlands<-row.names(subset(quantiles.habitat, subset=(quantiles.habitat$Woody.Wetlands<0.0501)))

PREFERENCIAS
PREFERENCIAS.g

PREFERENCIAS.g$Preference.forest
PREFERENCIAS.g$Avoidance.urban

PREFERENCIAS.g$Preference.urban
PREFERENCIAS.g$Avoidance.forest
plot(quantiles.grouped$Forests,quantiles.grouped$Urban)


Datos6
