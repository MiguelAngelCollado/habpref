#Cargamos los datos
Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
Datos4
#Con este código, generamos de nuestros datos subsets con n=400 para
#habitat, de manera que tendremos un nuevo conjunto de datos con 
#un muestreo igual para cada habitat, y con de este nuevo conjunto
#calcularemos la diversidad de cada habitat
#para cada habitat, el muestreo lo hacemos 100 veces y el resultado
#es un vector de fuerza para un habitat con 100 valores para promediar

#hacemos esto entero {lo de dentro de corchetes} una vez para cada habitat
#muestreamos al azar
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
tri7<-tri7[sample((nrow(tri6)), 400), ]
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
c1<-colSums(momo)/nrow(momo)
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
cp100d
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
div<-c(mean(bl100d),mean(cp100d),mean(df100d),mean(dh100d),mean(dl100d)
          ,mean(dm100d), mean(do100d), mean(eh100d), mean(ef100d), 
          mean(hp100d), mean(hb100d), mean(mf100d), mean(ow100d), 
          mean(ss100d), mean(ww100d))
divsd<-c(sd(bl100d),sd(cp100d),sd(df100d),sd(dh100d),sd(dl100d)
       ,sd(dm100d), sd(do100d), sd(eh100d), sd(ef100d), 
       sd(hp100d), sd(hb100d), sd(mf100d), sd(ow100d), 
       sd(ss100d), sd(ww100d))
