#Con este código, generamos de nuestros datos subsets con n=400 para
#habitat, de manera que tendremos un nuevo conjunto de datos con 
#un muestreo igual para cada habitat, y con de este nuevo conjunto
#crearemos la matriz de interacciones y extraeremos el vector de fuerzas
#para cada habitat, el muestreo lo hacemos 100 veces y el resultado
#es un vector de fuerza para un habitat con 100 valores para promediar

#hacemos esto entero {lo de dentro de corchetes} una vez para cada habitat
ww100<-NULL

library(bipartite)
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

ww100[n] = c1[15] 
}
#Las salidas las vamos guardando en 15 objetos diferentes
bl100
cc100
df100
dh100
dl100
dm100
do100
eh100
ef100
hp100
hb100
mf100
ow100
ss100
ww100
#Y creamos el vector de fuerza de nuestros habitats a partir de las
#medias de las 100 repeticiones de cada uno
stronk<-c(mean(bl100),mean(cc100),mean(df100),mean(dh100),mean(dl100)
          ,mean(dm100), mean(do100), mean(eh100), mean(ef100), 
          mean(hp100), mean(hb100), mean(mf100), mean(ow100), 
          mean(ss100), mean(ww100))

