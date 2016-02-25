#Strength y richness full data sin repeticiones en el punto de muestreo
getwd()
Datos1 <- read.csv("~/Desktop/Tesis/R/habpref full data/Datos1.csv")

#Agruamos habitats herbaceos con pastos
Datos1$habitat.extracted <- replace(Datos1$habitat.extracted, Datos1$habitat.extracted=="Hay/Pasture", "Herbaceuous")
#Agrupamos habitats como en el paper de Koh et al 2015
levels(Datos1$habitat.extracted)[levels(Datos1$habitat.extracted)=="Herbaceuous"]<-"Herbaceuous/Hay/Pasture"

#Creo un vector único de identificación del sitio y lo adjunto a mis datos
summary(Datos1$habitat.extracted)

sumcord<-(Datos1$latitude + Datos1$longitude)
Datos9<-(cbind(Datos1,sumcord))

tonto<- unique(Datos9[,c("sumcord", "gen_sp")])
View(tonto)
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
#Por rapidez identifiqué los singletones y los extraigo nombrándolos uno a uno
####Strength sin singletones----
summary(Datos6$gen_sp, maxsum= 465)
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

satellite.map(Datos6)
nrow(Datos6)
#Comprobamos que se hayan ido los singletones
summary(Datos6$gen_sp)
summary(Datos6$habitat.extracted)
#Calculamos la strenght----
#Creamos una matriz de interacciones con nuestros datos
int.mat<-aggregate(Datos6$habitat.extracted~Datos6$gen_sp, FUN = summary, Datos6)
jeje<-c(int.mat)
jiji<-c(jaja)
#lo convertimos en dataframe
jeje<-as.data.frame(jeje)
jiji<-as.data.frame(jeje)
row.names(jejem)<- jeje[,1]
jejem<-jeje[,-1]
jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
row.names(jejem)<- jeje[,1]
#Esta es nuestra matriz
int.mat<-jejem
nrow(int.mat)
View(int.mat)
#Cálculos de coverage----
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

bl100s <- NULL
co100s <- NULL
cc100s <- NULL
df100s <- NULL
dh100s <- NULL
dl100s <- NULL
dm100s <- NULL
do100s <- NULL
eh100s <- NULL
ef100s <- NULL
hb100s <- NULL
mf100s <- NULL
ss100s <- NULL
ww100s <- NULL
library(bipartite)
samplesize
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
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
  #Agrupamos las muestras en un solo dataframe nuevo
  Datos7<-(rbind(bri1,bri2,bri3,bri4,bri5,bri6,bri7,bri8,bri9,bri11,bri12,bri13,bri14,bri15))
  #Sacamos matriz de interacción
  jaja <-aggregate (Datos7$habitat.extracted ~ Datos7$gen_sp, FUN = summary, Datos7)
  #lo convertimos en objeto
  jeje<-c(jaja)
  jiji<-c(jaja)
  #lo convertimos en dataframe
  jeje<-as.data.frame(jeje)
  jiji<-as.data.frame(jeje)
  
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  jejem<-jejem[,-11]
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Coastal
  #[3] Cultivated.Crops              
  #[4] Deciduous.Forest             
  #[5] Developed..High.Intensity             
  #[6] Developed..Low.Intensity    
  #[7] Developed..Medium.Intensity     
  #[8] Developed..Open.Space  
  #[9] Emergent.Herbaceuous.Wetlands        
  #[10] Evergreen.Forest
  #[11] Herbaceuous/Hay/Pasture                  
  #[12] Mixed.Forest                  
  #[13] Shrub.Scrub                   
  #[14] Woody.Wetlands                  
  bl100s[n] = c1[1] 
  co100s[n] = c1[2]
  cc100s[n] = c1[3]
  df100s[n] = c1[4]
  dh100s[n] = c1[5]
  dl100s[n] = c1[6]
  dm100s[n] = c1[7]
  do100s[n] = c1[8]
  eh100s[n] = c1[9]
  ef100s[n] = c1[10]
  hb100s[n] = c1[11]
  mf100s[n] = c1[12]
  ss100s[n] = c1[13]
  ww100s[n] = c1[14]
  
}
bl100s
co100s
cc100s
df100s
dh100s
dl100s
dm100s
do100s
eh100s
ef100s
hp100s
hb100s
mf100s
ss100s
ww100s

nombres<-c("EF","CO","DF","EH","WW","MF","SS","BL", "DL","HB","DO","CC","DM","DH")
namus<-c("BL","CO","CC","DF","DH","DL","DM","DO","EH","EF","HB","MF","SS","WW")
boxplot(ef100s,co100s,df100s,eh100s,ww100s,mf100s,ss100s,bl100s,dl100s,hb100s,do100s,cc100s,dm100s,dh100s, names = nombres, main="Habitat Strength", xlab="Habitat", ylab="Strength")
strengthmeans<-c(mean(bl100s),mean(co100s),mean(cc100s),mean(df100s),mean(dh100s),mean(dl100s),
                 mean(dm100s),mean(do100s),mean(eh100s),mean(ef100s),mean(hb100s),
                 mean(mf100s),mean(ss100s),mean(ww100s))
strengthmeans<-as.data.frame(strengthmeans)
rownames(strengthmeans)<-namus
strengthmeans
#Extraemos la richness-----
#Usamos los mismos datos sin singletones
bl100ds <- NULL
cc100ds <- NULL
df100ds <- NULL
dh100ds <- NULL
dl100ds <- NULL
dm100ds <- NULL
do100ds <- NULL
eh100ds <- NULL
ef100ds <- NULL
hb100ds <- NULL
mf100ds <- NULL
co100ds <- NULL
ss100ds <- NULL
ww100ds <- NULL
samplesize
summary(Datos6$habitat.extracted)/samplesize
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
  mejib<-mejib[,-1]
  momo<-apply(mejib, MARGIN=2, FUN=as.numeric)
  row.names(momo)<- meji[,1]
  momo<-momo[,-11]
  colSums(momo)
  nrow(momo)
  #creamos un objeto con la diversidad (el número de especies en un habitat)
  c1<-colSums(momo)
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
  co100ds[n]=c1[2]
  cc100ds[n]=c1[3]
  df100ds[n]=c1[4]
  dh100ds[n]=c1[5]
  dl100ds[n]=c1[6]
  dm100ds[n]=c1[7]
  do100ds[n]=c1[8]
  eh100ds[n]=c1[9]
  ef100ds[n]=c1[10]
  hb100ds[n]=c1[11]
  mf100ds[n]=c1[12]
  ss100ds[n]=c1[13]
  ww100ds[n]=c1[14]
  
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

nombresd<-c("EF", "CO","EH","WW","MF","DF","SS","BL","DL","DO","HB","CC","DM","DH")
boxplot(ef100ds,co100ds,eh100ds,ww100ds,mf100ds,df100ds,ss100ds,bl100ds,dl100ds,do100ds,hb100ds,cc100ds,dm100ds,dh100ds, names = nombresd,main="Richness",xlab="Habitat",ylab="Richness")
namus
richnessmeans<-c(mean(bl100ds),mean(co100ds),mean(cc100ds),mean(df100ds),mean(dh100ds),mean(dl100ds),
                 mean(dm100ds),mean(do100ds),mean(eh100ds),mean(ef100ds),
                 mean(hb100ds),mean(mf100ds),mean(ss100ds),mean(ww100ds))
richnessmeans<-as.data.frame(richnessmeans)
rownames(richnessmeans)<-namus
richnessmeans
strengthmeans
cat<-c("Other","Other","Crops","Forest","Developed","Developed","Developed","Developed","Shrubs","Forest","Crops","Forest","Shrubs","Forest")
cat<-as.factor(cat)
MINs<-c(min(bl100s),min(co100s),min(cc100s),min(df100s),min(dh100s),min(dl100s),min(dm100s),min(do100s)
      ,min(eh100s),min(ef100s),min(hb100s),min(mf100s),min(ss100s),min(ww100s))
MAXs<-c(max(bl100s),max(co100s),max(cc100s),max(df100s),max(dh100s),max(dl100s),max(dm100s),max(do100s)
        ,max(eh100s),max(ef100s),max(hb100s),max(mf100s),max(ss100s),max(ww100s))
MINr<-c(min(bl100ds),min(co100ds),min(cc100ds),min(df100ds),min(dh100ds),min(dl100ds),min(dm100ds),min(do100ds)
        ,min(eh100ds),min(ef100ds),min(hb100ds),min(mf100ds),min(ss100ds),min(ww100ds))
MAXr<-c(max(bl100ds),max(co100ds),max(cc100ds),max(df100ds),max(dh100ds),max(dl100ds),max(dm100ds),max(do100ds)
        ,max(eh100ds),max(ef100ds),max(hb100ds),max(mf100ds),max(ss100ds),max(ww100ds))
SDs<-c(sd(bl100ds),sd(co100ds),sd(cc100ds),sd(df100ds),sd(dh100ds),sd(dl100ds),sd(dm100ds),sd(do100ds)
       ,sd(eh100ds),sd(ef100ds),sd(hb100ds),sd(mf100ds),sd(ss100ds),sd(ww100ds))
SDr<-c(sd(bl100ds),sd(co100ds),sd(cc100ds),sd(df100ds),sd(dh100ds),sd(dl100ds),sd(dm100ds),sd(do100ds)
       ,sd(eh100ds),sd(ef100ds),sd(hb100ds),sd(mf100ds),sd(ss100ds),sd(ww100ds))


strm<-cbind(richnessmeans,strengthmeans,cat,MINs,MAXs,MINr,MAXr,SDs,SDr)
colnames(strm)<-c("RarefiedRichness","RarefiedStrength","cat","MINs","MAXs","MINr","MAXr","SDs","SDr")

noun<-(c("Barren Land","Coastal","Crops","Deciduous","Developed High","Developed Low","Developed Medium","Developed Open","Emergent Herbaceuous","Evergreen","Pasture","Mixed","Shrub/Scrub","Woody Wetlands"))

#Ploteamos Richness vs Strength, de varias formas

plot(c(mean(bl100s),mean(co100s),mean(cc100s),mean(df100s),mean(dh100s),mean(dl100s),mean(dm100s),mean(do100s),mean(eh100s),mean(ef100s),mean(hb100s),mean(mf100s),mean(ss100s),mean(ww100s)),c(mean(bl100ds),mean(co100ds),mean(cc100ds),mean(df100ds),mean(dh100ds),mean(dl100ds),mean(dm100ds),mean(do100ds),mean(eh100ds),mean(ef100ds),mean(hb100ds),mean(mf100ds),mean(ss100ds),mean(ww100ds)), main="Rarefied Mean Strength vs Rarefied Mean Richness", xlab="Strength",ylab="Richness", ylim=c(40,110))
text(c(mean(bl100s),mean(co100s),mean(cc100s),mean(df100s),mean(dh100s),mean(dl100s),mean(dm100s),mean(do100s),mean(eh100s),mean(ef100s),mean(hb100s),mean(mf100s),mean(ss100s),mean(ww100s)),c(mean(bl100ds),mean(co100ds),mean(cc100ds),mean(df100ds),mean(dh100ds),mean(dl100ds),mean(dm100ds),mean(do100ds),mean(eh100ds),mean(ef100ds),mean(hb100ds),mean(mf100ds),mean(ss100ds),mean(ww100ds)), labels=noun, cex=0.8, pos=2)   

library(ggplot2)

#Solo los puntos
ggplot(strm,aes(x=Richness, y=Strength,color=factor(cat)))+
  geom_point(size=4)+
  geom_text(aes(label=noun), hjust=1.5, vjust=1.5)+
  theme_bw()+
  geom_text(data = strm, x = 69, y = 30, label = "Strength vs Richness", size=10)

#Añadimos barras de máximos y mínimos
ggplot(strm,aes(x=Richness, y=Strength,color=factor(cat)))+
  geom_point(size=4)+
  geom_text(aes(label=noun), hjust=1.5, vjust=1.5)+
  theme_bw()+
  geom_text(data = strm, x = 69, y = 35, label = "Strength vs Richness", size=10)+
  geom_errorbar(aes(ymin=MINs,ymax=MAXs),color="antiquewhite2")+
  geom_errorbarh(aes(xmin=MINr,xmax=MAXr), color="antiquewhite2")+
  theme(text=element_text(size=16),axis.text=element_text(size=rel(1.2)))

#Usamos barras de error estándar (sd)
ggplot(strm,aes(x=RarefiedRichness, y=RarefiedStrength,color=factor(cat)))+
  geom_point(size=8)+
  geom_text(aes(label=noun), hjust=-0.15, vjust=1)+
  theme_bw()+
  geom_text(data = strm, x = 69, y = 35, label = "Habitat Importance", size=10)+
  geom_errorbar(aes(ymin=RarefiedStrength-SDs,ymax=RarefiedStrength+SDs),color="antiquewhite2")+
  geom_errorbarh(aes(xmin=RarefiedRichness-SDr,xmax=RarefiedRichness+SDr), color="antiquewhite2")+
  theme(text=element_text(size=18),axis.text=element_text(size=rel(1.2)))
  


#Strength y richness kmeans----

#Esto no debería correrse, al menos no guardar el data.frame para no sobre-escribirlo
#pues cada vez que se corre la función de k-means varían los grupos
#tenemos que unir los dataframes de los diferentes grupos hechos con buffers
#Están comprobadas que no se pisan lugaresde muestreo
library(reshape)
buff1 <- read.csv("~/Desktop/Tesis/R/habpref full data/extracted/2001cover1000mbuffer.csv")
buff2 <- read.csv("~/Desktop/Tesis/R/habpref full data/extracted/2006cover1000mbuffer.csv")
buff3 <- read.csv("~/Desktop/Tesis/R/habpref full data/extracted/2011cover1000mbuffer.csv")
buff<- rbind(buff1,buff2,buff3)
buff<-as.data.frame(buff)
head(buff)
View(buff)
#melt(buff)
names(buff)
summary(buff$cover2)
levels(buff$cover2)[levels(buff$cover2)=="Open Water"]<-"Coastal"
satellite.map(subset(buff,subset=(buff$cover2=="Coastal")))
coasty<-subset(buff,subset=(buff$cover2=="Coastal"))
satellite.map(coasty)


#Transformamos los datos tal cual nos lo da el script de extracción de de buffers de
#un punto con la información espacial
View(buff)
buff<-cast(buff, formula=(id ~ cover), value="percent")
#cambiamos los NA´s por 0
buff[is.na(buff)]<- 0
nombres<-c("id","Coastal","Developed, Open Space","Developed, Low Intensity","Developed, Medium Intensity","Developed, High Intensity","Barren Land","Deciduous Forest","Evergreen Forest","Mixed Forest","Shrub/Scrub","Herbaceuous","Hay/Pasture","Cultivated Crops","Woody Wetlands","Emergent Herbaceuous Wetlands")
colnames(buff)<-nombres
#cargamos las librerias que necesitamos
library(vegan)
library(FD)
#hacemos ahora el kmeans
wss <- (nrow(buff)-1)*sum(apply(buff,2,var))
#para el for metemos un número de grupos menor al que tenemos
for (i in 2:100) wss[i] <- sum(kmeans(buff, 
                                      centers=i)$withinss)
plot(1:100, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# corremos el K-Means Cluster Analysis
fit <- kmeans(buff, 20) # Seleccionamos 20 grupos
# obtenemos los centroides 
centroides<-aggregate(buff ,by=list(fit$cluster),FUN=mean)
warnings()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/habpref full data/Results")
getwd()
write.csv(centroides,"centroidesdelos20grupos.csv")

# agregamos los grupos como variable nueva
buff <- data.frame(buff, fit$cluster)
View(buff)
copiar<-cbind(buff$id,buff$fit.cluster)
copiar<-cbind(copiar,buff$fit.cluster)
lel<-buff$id
lel<-as.data.frame(lel)
lal<-buff$fit.cluster
lal<-as.factor(lal)
pegar<-cbind(lel,lal)
str(pegar)
View(pegar)
View(merger)
is.numeric(lal)
names(pegar)<-c("id","group")
merger<-merge(pegar,Datos1, all=TRUE)
getwd()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/habpref full data/")
#Lo grabamos como data.frame para cargarlo, porque cada vez que corramos el análisis de 
#k-means, los grupos serán similares, pero estarán en diferente posición y nos servirán

# write.csv(merger,"merger.csv")

#Strength de grupos hechos con k-means-----
summary(merger$habitat.extracted)
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

#Ahora retiramos las repeticiones
names(merger)

sumcord<-(merger$latitude + merger$longitude)
merger<-(cbind(merger,sumcord))

tonto<- unique(merger[,c("sumcord", "gen_sp")])
View(tonto)
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
View(merger)

#Creamos una matriz de interacciones con nuestros datos
summary(merger$group)
#El mínimo es el group 3 con 707 individuos
min(summary(merger$group))
int.mat.k<-aggregate(merger$group~merger$gen_sp, FUN = summary, merger)
jeje<-c(int.mat.k)
#jiji<-c(jaja)
#lo convertimos en dataframe
jeje<-as.data.frame(jeje)
jiji<-as.data.frame(jeje)
row.names(jeje)<- jeje[,1]
jejem<-jeje[,-1]
jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
row.names(jejem)<- jeje[,1]
str(jejem)
#Esta es nuestra matriz
int.mat.k<-jejem

#Cálculos de coverage----
#vemos a qué tamaño muestral cubrimos qué porcentaje de riqueza de habitat con la función
#iNEXT, en cada grupo creado con el k-means
library(iNEXT)
summary(merger$group)
min(summary(merger$group))
#vemos el coverage de cada group con el número de individuos del group que tiene menos
for( n in 1:20){
  a<-iNEXT(int.mat.k[,n],datatype = "abundance", endpoint = 183)
  print(a$iNextEst)
}
#Vemos cuanto coverage tiene el habitat que menos individuos tiene, el cual es "herbaceuous"
iNEXT(int.mat.k[,15],datatype = "abundance", endpoint = 119)

iNEXT(int.mat.k[,1],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,2],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,3],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,4],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,5],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,6],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,7],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,8],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,9],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,10],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,11],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,12],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,13],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,14],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,15],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,16],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,17],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,18],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,19],datatype = "abundance", endpoint = 183)
iNEXT(int.mat.k[,20],datatype = "abundance", endpoint = 183)

#el group 15 que es el que menos individuos tiene tiene un coverage de 0.741

#Ahora tenemos que buscar cuantos individuos tenemos que tomar de cada habitat para tener el
#mismo coverage que en herbaceuous
samplesize.k<-NULL
for (n in 1:20){
  cc<-estimateD(int.mat.k[,n], datatype = "abundance", base= "coverage", level = 0.60)
  samplesize.k[n]<-cc
}
#Es por la columna no numérica este warning
warnings()
iNEXT(int.mat.k[,15],datatype = "abundance", endpoint = 119)
estimateD(int.mat.k[,10], datatype = "abundance", base= "coverage", level = 0.92)
samplesize.k
samplesize.k<-as.data.frame(samplesize.k)
nombres.k<-c("G1","G2","G3","G4","G5","G6","G7", "G8","G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20")
colnames(samplesize.k)<-nombres.k
#Tenemos el tamaño que muestral que tenemos que coger de cada habitat para que el coverage
#sea el mismo
samplesize.k
#restamos para comprobar que tenemos suficiente muestra en nuestro dataframe
#al ser todo valores positivos indican que si tenemos suficiente muestra
summary(merger$group) - samplesize.k 




un <- NULL
do <- NULL
tr <- NULL
cu <- NULL
ci <- NULL
se <- NULL
si <- NULL
oc <- NULL
nu <- NULL
di <- NULL
on <- NULL
doc <- NULL
tre <- NULL
ca <- NULL
qu <- NULL
di6 <- NULL
di7 <- NULL
di8 <- NULL
di9 <- NULL
ve <- NULL
samplesize.k
library(bipartite)
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 102), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 147), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 79), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 135), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 143), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 149), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 120), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 96), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 129), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 142), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 123), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 139), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 144), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 172), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 57), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 121), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 132), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 104), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 117), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 117), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  View(Datos5)
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
  row.names(jejem)<- jiji[,1]
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  un[n] = c1[1] 
  do[n] = c1[2]
  tr[n] = c1[3]
  cu[n] = c1[4]
  ci[n] = c1[5]
  se[n] = c1[6]
  si[n] = c1[7]
  oc[n] = c1[8]
  nu[n] = c1[9]
  di[n] = c1[10]
  on[n] = c1[11]
  doc[n] = c1[12]
  tre[n] = c1[13]
  ca[n] = c1[14]
  qu[n] = c1[15]
  di6[n] = c1[16]
  di7[n] = c1[17]
  di8[n] = c1[18]
  di9[n] = c1[19]
  ve[n] = c1[20]
  
}
boxplot(un,do,tr,cu,ci,se,si,oc,nu,di,on,doc,tre,ca,qu,di6,di7,di8,di9,ve)
nom.ks<-c("G14","G6","G5","G4","G2","G10","G9","G13","G12","G11","G7","G17","G19","G20","G16","G18","G1","G3","G8","G15")

boxplot(ca,se,ci,cu,do,di,nu,tre,doc,on,si,di7,di9,di6,ve,di8,un,tr,oc,qu, main= "Grouped Strength",names=nom.ks, xlab="Groups", ylab="Strength")

#Richness para grupos hechos con k-means-----
#con el data.frame merger y las cifras de coverage que necesitamos para cada group
#podemos hacer el loop para tomar muestras y calcular la richness de los groups
unr <- NULL
dor <- NULL
trr <- NULL
cur <- NULL
cir <- NULL
ser <- NULL
sir <- NULL
ocr <- NULL
nur <- NULL
dir <- NULL
onr <- NULL
docr <- NULL
trer <- NULL
car <- NULL
qur <- NULL
di6r <- NULL
di7r <- NULL
di8r <- NULL
di9r <- NULL
ver <- NULL
samplesize.k
library(bipartite)
for (n in 1:100){
  tri1<-subset(merger, subset=(merger$group == "1"))
  tri1<-tri1[sample((nrow(tri1)), 102), ]
  tri2<-subset(merger, subset=(merger$group == "2"))
  tri2<-tri2[sample((nrow(tri2)), 147), ]
  tri3<-subset(merger, subset=(merger$group == "3"))
  tri3<-tri3[sample((nrow(tri3)), 79), ]
  tri4<-subset(merger, subset=(merger$group == "4"))
  tri4<-tri4[sample((nrow(tri4)), 135), ]
  tri5<-subset(merger, subset=(merger$group == "5"))
  tri5<-tri5[sample((nrow(tri5)), 143), ]
  tri6<-subset(merger, subset=(merger$group == "6"))
  tri6<-tri6[sample((nrow(tri6)), 149), ]
  tri7<-subset(merger, subset=(merger$group == "7"))
  tri7<-tri7[sample((nrow(tri7)), 120), ]
  tri8<-subset(merger, subset=(merger$group == "8"))
  tri8<-tri8[sample((nrow(tri8)), 96), ]
  tri9<-subset(merger, subset=(merger$group == "9"))
  tri9<-tri9[sample((nrow(tri9)), 129), ]
  tri10<-subset(merger, subset=(merger$group == "10"))
  tri10<-tri10[sample((nrow(tri10)), 142), ]
  tri11<-subset(merger, subset=(merger$group == "11"))
  tri11<-tri11[sample((nrow(tri11)), 123), ]
  tri12<-subset(merger, subset=(merger$group == "12"))
  tri12<-tri12[sample((nrow(tri12)), 139), ]
  tri13<-subset(merger, subset=(merger$group == "13"))
  tri13<-tri13[sample((nrow(tri13)), 144), ]
  tri14<-subset(merger, subset=(merger$group == "14"))
  tri14<-tri14[sample((nrow(tri14)), 172), ]
  tri15<-subset(merger, subset=(merger$group == "15"))
  tri15<-tri15[sample((nrow(tri15)), 57), ]
  tri16<-subset(merger, subset=(merger$group == "16"))
  tri16<-tri16[sample((nrow(tri16)), 121), ]
  tri17<-subset(merger, subset=(merger$group == "17"))
  tri17<-tri17[sample((nrow(tri17)), 132), ]
  tri18<-subset(merger, subset=(merger$group == "18"))
  tri18<-tri18[sample((nrow(tri18)), 104), ]
  tri19<-subset(merger, subset=(merger$group == "19"))
  tri19<-tri19[sample((nrow(tri19)), 117), ]
  tri20<-subset(merger, subset=(merger$group == "20"))
  tri20<-tri20[sample((nrow(tri20)), 117), ]
  
  Datos5<-(rbind(tri1,tri2,tri3,tri4,tri5,tri6,tri7,tri8,tri9,tri10,tri11,tri12,tri13,tri14,tri15,tri16,tri17,tri18,tri19,tri20))
  
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
  
  unr[n] = c1[1] 
  dor[n] = c1[2]
  trr[n] = c1[3]
  cur[n] = c1[4]
  cir[n] = c1[5]
  ser[n] = c1[6]
  sir[n] = c1[7]
  ocr[n] = c1[8]
  nur[n] = c1[9]
  dir[n] = c1[10]
  onr[n] = c1[11]
  docr[n] = c1[12]
  trer[n] = c1[13]
  car[n] = c1[14]
  qur[n] = c1[15]
  di6r[n] = c1[16]
  di7r[n] = c1[17]
  di8r[n] = c1[18]
  di9r[n] = c1[19]
  ver[n] = c1[20]
  
}

boxplot(unr,dor,trr,cur,cir,ser,sir,ocr,nur,dir,onr,docr,trer,car,qur,di6r,di7r,di8r,di9r,ver)
nom.ksr<-c("G14","G2","G13","G5","G6","G10","G12","G17","G4","G9","G16","G11","G19","G20","G7","G18","G1","G8","G3","G15")

boxplot(car,dor,trer,cir,ser,dir,docr,di7r,cur,nur,di6r,onr,di9r,ver,sir,di8r,unr,ocr,trr,qur,names=nom.ksr, xlab="Groups", ylab="Strength", main= "Grouped Richness")

mean(tr) - mean(doc)
nom.ks<-as.data.frame(nom.ks)
boxplot(doc)

#Ploteamos Richness vs Strength
plot(c(mean(bl100s),mean(cc100s),mean(df100s),mean(dh100s),mean(dl100s),mean(dm100s),mean(do100s),mean(eh100s),mean(ef100s),mean(hp100s),mean(hb100s),mean(mf100s),mean(ow100s),mean(ss100s),mean(ww100s)),c(mean(bl100ds),mean(cc100ds),mean(df100ds),mean(dh100ds),mean(dl100ds),mean(dm100ds),mean(do100ds),mean(eh100ds),mean(ef100ds),mean(hp100ds),mean(hb100ds),mean(mf100ds),mean(ow100ds),mean(ss100ds),mean(ww100ds)), main="Rarefied Mean Strength vs Rarefied Mean Richness", xlab="Strength",ylab="Richness", ylim=c(40,100))
text(c(mean(bl100s),mean(cc100s),mean(df100s),mean(dh100s),mean(dl100s),mean(dm100s),mean(do100s),mean(eh100s),mean(ef100s),mean(hp100s),mean(hb100s),mean(mf100s),mean(ow100s),mean(ss100s),mean(ww100s)),c(mean(bl100ds),mean(cc100ds),mean(df100ds),mean(dh100ds),mean(dl100ds),mean(dm100ds),mean(do100ds),mean(eh100ds),mean(ef100ds),mean(hp100ds),mean(hb100ds),mean(mf100ds),mean(ow100ds),mean(ss100ds),mean(ww100ds)), labels=noun, cex=0.8, pos=2)   

library(ggplot2)


strengthmeansk<-c(mean(un),mean(do),mean(tr),mean(cu),mean(ci),mean(se),
                  mean(si),mean(oc),mean(nu),mean(di),
                  mean(on),mean(doc),mean(tre),mean(ca),mean(qu),mean(di6)
                  ,mean(di7),mean(di8),mean(di9),mean(ve))
strengthmeansk<- as.data.frame(strengthmeansk)
richnessmeansk<-c(mean(unr),mean(dor),mean(trr),mean(cur),mean(cir),mean(ser),
                 mean(sir),mean(ocr),mean(nur),mean(dir),
                 mean(onr),mean(docr),mean(trer),mean(car),mean(qur),mean(di6r)
                 ,mean(di7r),mean(di8r),mean(di9r),mean(ver))
richnessmeansk<-as.data.frame(richnessmeansk)
nemus<-c("G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","G13","G14","G15","G16","G17","G18","G19","G20")
rownames(richnessmeansk)<-nemus
rownames(strengthmeansk)<-nemus

richnessmeansk
strengthmeansk
cat.k<-c("Coastal Crops","Coastal Forest","Herbaceuous Coastal","Forests","Forests Crops","Coastal","Herbaceuous Coastal","Crops Forest","Forests Crops","Forests","Developeds","Forests","Forests","Forests","Herbaceuous Forest Crops","Forests Developed","Forests Crops","Forests Developed","Developeds","Forests Developed")
cat.k<-as.factor(cat.k)
MINs.k<-c(min(un),min(do),min(tr),min(cu),min(ci),min(se),min(si),min(oc)
        ,min(nu),min(di),min(on),min(doc),min(tre),min(ca),min(qu),min(di6),min(di7),min(di8),min(di9),min(ve))
duplicated(MAXs.k)
MAXs.k<-c(max(un),max(do),max(tr),max(cu),max(ci),max(se),max(si),max(oc)
        ,max(nu),max(di),max(on),max(doc),max(tre),max(ca),max(qu),max(di6),max(di7),max(di8),max(di9),max(ve))
MINr.k<-c(min(unr),min(dor),min(trr),min(cur),min(cir),min(ser),min(sir),min(ocr)
          ,min(nur),min(dir),min(onr),min(docr),min(trer),min(car),min(qur),min(di6r),min(di7r),min(di8r),min(di9r),min(ver))
MAXr.k<-c(max(unr),max(dor),max(trr),max(cur),max(cir),max(ser),max(sir),max(ocr)
          ,max(nur),max(dir),max(onr),max(docr),max(trer),max(car),max(qur),max(di6r),max(di7r),max(di8r),max(di9r),max(ver))
SDs.k<-c(sd(un),sd(do),sd(tr),sd(cu),sd(ci),sd(se),sd(si),sd(oc)
         ,sd(nu),sd(di),sd(on),sd(doc),sd(tre),sd(ca),sd(qu),sd(di6),sd(di7),sd(di8),sd(di9),sd(ve))
SDr.k<-c(sd(unr),sd(dor),sd(trr),sd(cur),sd(cir),sd(ser),sd(sir),sd(ocr)
         ,sd(nur),sd(dir),sd(onr),sd(docr),sd(trer),sd(car),sd(qur),sd(di6r),sd(di7r),sd(di8r),sd(di9r),sd(ver))


strm.k<-cbind(richnessmeansk,strengthmeansk,cat.k,MINs.k,MAXs.k,MINr.k,MAXr.k,SDs.k,SDr.k)
colnames(strm.k)<-c("RarefiedRichness","RarefiedStrength","cat","MINs","MAXs","MINr","MAXr","SDs","SDr")

noun.k<-(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))




names(strm.k)
#Solo los puntos
library(ggplot2)
ggplot(strm.k,aes(x=RarefiedRichness, y=RarefiedStrength,color=factor(cat.k)))+
  geom_point(size=4)+
  geom_text(aes(label=noun.k), hjust=1.5, vjust=1.5)+
  theme_bw()+
  geom_text(data = strm.k, x = 69, y = 30, label = "Strength vs Richness", size=10)

#Añadimos barras de máximos y mínimos
ggplot(strm.k,aes(x=RarefiedRichness, y=RarefiedStrength,color=factor(cat.k)))+
  geom_point(size=4)+
  geom_text(aes(label=noun.k), hjust=1.5, vjust=1.5)+
  theme_bw()+
  geom_text(data = strm.k, x = 40, y = 30, label = "Strength vs Richness", size=10)+
  geom_errorbar(aes(ymin=MINs.k,ymax=MAXs.k),color="antiquewhite2")+
  geom_errorbarh(aes(xmin=MINr.k,xmax=MAXr.k), color="antiquewhite2")+
  theme(text=element_text(size=16),axis.text=element_text(size=rel(1.2)))

#Usamos barras de error estándar (sd)
ggplot(strm.k,aes(x=RarefiedRichness, y=RarefiedStrength,color=factor(cat.k)))+
  geom_point(size=8)+
  geom_text(aes(label=noun.k), hjust=1.5, vjust=-1)+
  theme_bw()+
  geom_text(data = strm.k, x = 50, y = 25, label = "Habitat Mosaic Importance", size=10)+
  geom_errorbar(aes(ymin=RarefiedStrength-SDs.k,ymax=RarefiedStrength+SDs.k),color="antiquewhite2")+
  geom_errorbarh(aes(xmin=RarefiedRichness-SDr.k,xmax=RarefiedRichness+SDr.k), color="antiquewhite2")+
  theme(text=element_text(size=18),axis.text=element_text(size=rel(1.2)))


####Otros cálculos irrelevantes-----



#Strength para especies con >10 individuos----


bl10s <- NULL
co10s <- NULL
cc10s <- NULL
df10s <- NULL
dh10s <- NULL
dl10s <- NULL
dm10s <- NULL
do10s <- NULL
eh10s <- NULL
ef10s <- NULL
hp10s <- NULL
hb10s <- NULL
mf10s <- NULL
ss10s <- NULL
ww10s <- NULL
library(bipartite)
samplesize
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 238), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 455), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 723), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 351), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 427), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 332), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 417), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 387), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 506), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 509), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 304), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 422), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Coastal"))
  bri13<-bri13[sample((nrow(bri13)), 476), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 313), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 493), ]
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
  
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  View(jejem)
  nrow(jejem)
  suma<-rowSums(jejem)
  suma<-as.data.frame(suma)
  jejem<-cbind(jejem,suma)
  View(jejem)
  jejem<-subset(jejem, subset=(jejem$suma>10))
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Coastal
  #[3] Cultivated.Crops              
  #[4] Deciduous.Forest             
  #[5] Developed..High.Intensity             
  #[6] Developed..Low.Intensity    
  #[7] Developed..Medium.Intensity     
  #[8] Developed..Open.Space  
  #[9] Emergent.Herbaceuous.Wetlands        
  #[10] Evergreen.Forest
  #[11] Hay.Pasture             
  #[12] Herbaceuous                  
  #[13] Mixed.Forest                  
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  bl10s[n] = c1[1] 
  co10s[n] = c1[2]
  cc10s[n] = c1[3]
  df10s[n] = c1[4]
  dh10s[n] = c1[5]
  dl10s[n] = c1[6]
  dm10s[n] = c1[7]
  do10s[n] = c1[8]
  eh10s[n] = c1[9]
  ef10s[n] = c1[10]
  hp10s[n] = c1[11]
  hb10s[n] = c1[12]
  mf10s[n] = c1[13]
  ss10s[n] = c1[14]
  ww10s[n] = c1[15]
  
}
nombres<-c("DF","EF","CO","HP","WW","DO","CC","MF", "DL","EH","DM","HB","DH","SS","BL")
boxplot(df10s,ef10s,co10s,hp10s,ww10s,do10s,cc10s,mf10s,dl10s,eh10s,dm10s,hb10s,dh10s,ss10s,bl10s, names = nombres, main="Habitat Strength >10 individuals", xlab="Habitat", ylab="Strength")

#Strength para especies con >1 individuo-----
bl1s <- NULL
co1s <- NULL
cc1s <- NULL
df1s <- NULL
dh1s <- NULL
dl1s <- NULL
dm1s <- NULL
do1s <- NULL
eh1s <- NULL
ef1s <- NULL
hp1s <- NULL
hb1s <- NULL
mf1s <- NULL
ss1s <- NULL
ww1s <- NULL
library(bipartite)
samplesize
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 238), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 455), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 723), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 351), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 427), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 332), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 417), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 387), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 506), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 509), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 304), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 422), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Coastal"))
  bri13<-bri13[sample((nrow(bri13)), 476), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 313), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 493), ]
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
  
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  View(jejem)
  nrow(jejem)
  suma<-rowSums(jejem)
  suma<-as.data.frame(suma)
  jejem<-cbind(jejem,suma)
  View(jejem)
  jejem<-subset(jejem, subset=(jejem$suma>1))
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Coastal
  #[3] Cultivated.Crops              
  #[4] Deciduous.Forest             
  #[5] Developed..High.Intensity             
  #[6] Developed..Low.Intensity    
  #[7] Developed..Medium.Intensity     
  #[8] Developed..Open.Space  
  #[9] Emergent.Herbaceuous.Wetlands        
  #[10] Evergreen.Forest
  #[11] Hay.Pasture             
  #[12] Herbaceuous                  
  #[13] Mixed.Forest                  
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  bl1s[n] = c1[1] 
  co1s[n] = c1[2]
  cc1s[n] = c1[3]
  df1s[n] = c1[4]
  dh1s[n] = c1[5]
  dl1s[n] = c1[6]
  dm1s[n] = c1[7]
  do1s[n] = c1[8]
  eh1s[n] = c1[9]
  ef1s[n] = c1[10]
  hp1s[n] = c1[11]
  hb1s[n] = c1[12]
  mf1s[n] = c1[13]
  ss1s[n] = c1[14]
  ww1s[n] = c1[15]
  
}
nombres.1<-c("DF","EF","CO","HP","WW","CC","DH","MF", "DL","EH","DO","HB","DM","SS","BL")
boxplot(df1s,ef1s,co1s,hp1s,ww1s,cc1s,dh1s,mf1s,dl1s,eh1s,do1s,hb1s,dm1s,ss1s,bl1s, names = nombres.1, main="Habitat Strength >1 individuals", xlab="Habitat", ylab="Strength")

#Sin invasoras----
Datos13<-subset(Datos6, subset=(!Datos6$gen_sp == "Andrena wilkella" & !Datos6$gen_sp == "Anthidium manicatum" & !Datos6$gen_sp == "Anthidium oblongatum" & !Datos6$gen_sp == "Hylaeus leptocephalus" & !Datos6$gen_sp == "Hylaeus punctatus" & !Datos6$gen_sp == "Lasioglossum leucozonium" & !Datos6$gen_sp == "Megachile apicalis" & !Datos6$gen_sp == "Megachile concinna" & !Datos6$gen_sp == "Megachile rotundata"  & !Datos6$gen_sp == "Megachile sculpturalis" & !Datos6$gen_sp == "Andrena" & !Datos6$gen_sp == "Anthophorula micheneri" & !Datos6$gen_sp == "Bombus fernaldae" & !Datos6$gen_sp == "Bombus imitator" & !Datos6$gen_sp == "Cemolobus ipomoeae" & !Datos6$gen_sp == "Coelioxys alternata" & !Datos6$gen_sp == "Coelioxys dolichos" & !Datos6$gen_sp == "Coelioxys immaculata" & !Datos6$gen_sp == "Colletes brevicornis" & !Datos6$gen_sp == "Epeolus bifasciatus" & !Datos6$gen_sp == "Lasioglossum anomalum" & !Datos6$gen_sp == "Lasioglossum apopkense" & !Datos6$gen_sp == "Lasioglossum asteris" & !Datos6$gen_sp == "Lasioglossum curtulum" & !Datos6$gen_sp == "Lasioglossum floridanum" & !Datos6$gen_sp == "Lasioglossum michiganense" & !Datos6$gen_sp == "Lasioglossum pectinatum" & !Datos6$gen_sp == "Lasioglossum rufitarse" & !Datos6$gen_sp == "Lasioglossum simplex" & !Datos6$gen_sp == "Nomada affabilis" & !Datos6$gen_sp == "Nomada annulata" & !Datos6$gen_sp == "Nomada bethunei" & !Datos6$gen_sp == "Nomada composita" & !Datos6$gen_sp == "Nomada ovata" & !Datos6$gen_sp == "Nomada placida" & !Datos6$gen_sp == "Nomada rubicunda" & !Datos6$gen_sp == "Nomada valida" & !Datos6$gen_sp == "Osmia caerulescens" & !Datos6$gen_sp == "Osmia inspergens" & !Datos6$gen_sp == "Osmia proxima" & !Datos6$gen_sp == "Osmia simillima" & !Datos6$gen_sp == "Paranthidium jugatorium" & !Datos6$gen_sp == "Perdita bequaerti" & !Datos6$gen_sp == "Perdita gerardiae" & !Datos6$gen_sp == "Pseudopanurgus andrenoides" & !Datos6$gen_sp == "Pseudopanurgus nebrascensis" & !Datos6$gen_sp == "Pseudopanurgus rugosus" & !Datos6$gen_sp == "Sphecodes antennariae" & !Datos6$gen_sp == "Sphecodes aroniae" & !Datos6$gen_sp == "Sphecodes clematidis" & !Datos6$gen_sp == "Sphecodes cressonii" & !Datos6$gen_sp == "Sphecodes galerus" & !Datos6$gen_sp == "Sphecodes heraclei" & !Datos6$gen_sp == "Sphecodes johnsonii" & !Datos6$gen_sp == "Stelis nitida" & !Datos6$gen_sp == "Triepeolus helianthi" & !Datos6$gen_sp == "Triepeolus pectoralis" & !Datos6$gen_sp == "Triepeolus simplex"))

summary(Datos6$gen_sp)
summary(Datos13$gen_sp, maxsum= 430)
#Calculamos la strenght
#Creamos una matriz de interacciones con nuestros datos
int.mat<-aggregate(Datos13$habitat.extracted~Datos13$gen_sp, FUN = summary, Datos13)
jeje<-c(int.mat)
jiji<-c(jaja)
#lo convertimos en dataframe
jeje<-as.data.frame(jeje)
jiji<-as.data.frame(jeje)
row.names(jejem)<- jeje[,1]
jejem<-jeje[,-1]
jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
row.names(jejem)<- jeje[,1]
#Esta es nuestra matriz
int.mat<-jejem
View(int.mat)
#Cálculos de coverage
#vemos a qué tamaño muestral cubrimos qué porcentaje de riqueza de habitat con la función
#iNEXT, en cada habitat
library(iNEXT)
summary(Datos13$habitat.extracted)
View(int.mat)
#Con este loop vemos el coverage de cada habitat
for( n in 1:15){
  a<-iNEXT(int.mat[,n],datatype = "abundance", endpoint = 798)
  print(a$iNextEst)
}
#Vemos cuanto coverage tiene el habitat que menos individuos tiene, el cual es "herbaceuous"
iNEXT(int.mat[,12],datatype = "abundance", endpoint = 798)
#Herbaceus que es el que menos individuos tiene tiene un coverage de 0.966

#Ahora tenemos que buscar cuantos individuos tenemos que tomar de cada habitat para tener el
#mismo coverage en todos
samplesize<-NULL
for (n in 1:15){
  c<-estimateD(int.mat[,n], datatype = "abundance", base= "coverage", level = 0.92)
  samplesize[n]<-c
}
colnames(int.mat)
samplesize
samplesize<-as.data.frame(samplesize)
nombres<-c("BL","CO","CC","DF","DH","DL","DM","DO", "EH","EF","HP","HB","MF","SS","WW")
colnames(samplesize)<-nombres
#Tenemos el tamaño que muestral que tenemos que coger de cada habitat para que el coverage
#sea el mismo
samplesize
#restamos para comprobar que tenemos suficiente muestra en nuestro dataframe
#al ser todo valores positivos indican que si tenemos suficiente muestra
summary(Datos6$habitat.extracted) - samplesize 

#Ahora hacemos un loop, para extraer los individuos que correspondan para cada habitat 
#para igualar la misma coverage 

bl100si <- NULL
co100si <- NULL
cc100si <- NULL
df100si <- NULL
dh100si <- NULL
dl100si <- NULL
dm100si <- NULL
do100si <- NULL
eh100si <- NULL
ef100si <- NULL
hp100si <- NULL
hb100si <- NULL
mf100si <- NULL
ss100si <- NULL
ww100si <- NULL
library(bipartite)
samplesize
for (n in 1:100){
  #Vamos haciendo subsets en los que solo nos quedamos con dicho habitat
  bri1<-subset(Datos6, subset=(Datos6$habitat.extracted == "Barren Land"))
  bri1<-bri1[sample((nrow(bri1)), 227), ]
  bri2<-subset(Datos6, subset=(Datos6$habitat.extracted == "Cultivated Crops"))
  bri2<-bri2[sample((nrow(bri2)), 431), ]
  bri3<-subset(Datos6, subset=(Datos6$habitat.extracted == "Deciduous Forest"))
  bri3<-bri3[sample((nrow(bri3)), 688), ]
  bri4<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, High Intensity"))
  bri4<-bri4[sample((nrow(bri4)), 317), ]
  bri5<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Low Intensity"))
  bri5<-bri5[sample((nrow(bri5)), 384), ]
  bri6<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Medium Intensity"))
  bri6<-bri6[sample((nrow(bri6)), 303), ]
  bri7<-subset(Datos6, subset=(Datos6$habitat.extracted == "Developed, Open Space"))
  bri7<-bri7[sample((nrow(bri7)), 395), ]
  bri8<-subset(Datos6, subset=(Datos6$habitat.extracted == "Emergent Herbaceuous Wetlands"))
  bri8<-bri8[sample((nrow(bri8)), 358), ]
  bri9<-subset(Datos6, subset=(Datos6$habitat.extracted == "Evergreen Forest"))
  bri9<-bri9[sample((nrow(bri9)), 480), ]
  bri10<-subset(Datos6, subset=(Datos6$habitat.extracted == "Hay/Pasture"))
  bri10<-bri10[sample((nrow(bri10)), 477), ]
  bri11<-subset(Datos6, subset=(Datos6$habitat.extracted == "Herbaceuous"))
  bri11<-bri11[sample((nrow(bri11)), 288), ]
  bri12<-subset(Datos6, subset=(Datos6$habitat.extracted == "Mixed Forest"))
  bri12<-bri12[sample((nrow(bri12)), 404), ]
  bri13<-subset(Datos6, subset=(Datos6$habitat.extracted == "Coastal"))
  bri13<-bri13[sample((nrow(bri13)), 440), ]
  bri14<-subset(Datos6, subset=(Datos6$habitat.extracted == "Shrub/Scrub"))
  bri14<-bri14[sample((nrow(bri14)), 304), ]
  bri15<-subset(Datos6, subset=(Datos6$habitat.extracted == "Woody Wetlands"))
  bri15<-bri15[sample((nrow(bri15)), 475), ]
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
  
  row.names(jeje)<- jiji[,1]
  jejem<-jeje[,-1]
  jejem<-apply(jejem, MARGIN=2, FUN=as.numeric)
  
  #Y extraemos el vector de fuerza  
  c1<-strength(jejem, type="Bascompte")
  #Aquí según el número que pongamos entre corchetes, nos extrae para
  #un habitat u otro 
  #[1] Barren.Land         
  #[2] Coastal
  #[3] Cultivated.Crops              
  #[4] Deciduous.Forest             
  #[5] Developed..High.Intensity             
  #[6] Developed..Low.Intensity    
  #[7] Developed..Medium.Intensity     
  #[8] Developed..Open.Space  
  #[9] Emergent.Herbaceuous.Wetlands        
  #[10] Evergreen.Forest
  #[11] Hay.Pasture             
  #[12] Herbaceuous                  
  #[13] Mixed.Forest                  
  #[14] Shrub.Scrub                   
  #[15] Woody.Wetlands                  
  bl100si[n] = c1[1] 
  co100si[n] = c1[2]
  cc100si[n] = c1[3]
  df100si[n] = c1[4]
  dh100si[n] = c1[5]
  dl100si[n] = c1[6]
  dm100si[n] = c1[7]
  do100si[n] = c1[8]
  eh100si[n] = c1[9]
  ef100si[n] = c1[10]
  hp100si[n] = c1[11]
  hb100si[n] = c1[12]
  mf100si[n] = c1[13]
  ss100si[n] = c1[14]
  ww100si[n] = c1[15]
  
}
nombres.i<-c("DF","EF","CO","WW","HP","CC","DH","MF", "DO","DL","EH","HB","DM","SS","BL")
boxplot(df100s,ef100s,co100s,ww100s,hp100s,cc100s,dh100s,mf100s,do100s,dl100s,eh100s,hb100s,dm100s,ss100s,bl100s, names = nombres.i, main="Habitat Strength", xlab="Habitat", ylab="Strength")

strengthmeans<-c(mean(bl100s),mean(co100s),mean(cc100s),mean(df100s),mean(dh100s),mean(dl100s),
                 mean(dm100s),mean(do100s),mean(eh100s),mean(ef100s),mean(ef100s),
                 mean(hp100s),mean(mf100s),mean(ss100s),mean(ww100s))
strengthmeans.i<-c(mean(bl100si),mean(co100si),mean(cc100si),mean(df100si),mean(dh100si),mean(dl100si),
                   mean(dm100si),mean(do100si),mean(eh100si),mean(ef100si),mean(ef100si),
                   mean(hp100si),mean(mf100si),mean(ss100si),mean(ww100si))

strengthmeans<-as.data.frame(strengthmeans)
rownames(strengthmeans)<-nombres
strengthmeans

#Mapeamos cada habitat según nuestro muestreo----
evergreen<-subset(Datos6, subset=(Datos6$habitat.extracted=="Evergreen Forest"))
deciduous<-subset(Datos6, subset=(Datos6$habitat.extracted=="Deciduous Forest"))
mixed<-subset(Datos6, subset=(Datos6$habitat.extracted=="Mixed Forest"))
woody<-subset(Datos6, subset=(Datos6$habitat.extracted=="Woody Wetlands"))

satellite.map(evergreen)
satellite.map(deciduous)
satellite.map(mixed)
satellite.map(woody)

nrow(evergreen)
nrow(deciduous)
nrow(mixed)
