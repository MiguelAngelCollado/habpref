Datos2 <- read.csv("~/Desktop/Tesis/R/habpref/Datos2.csv")

Datos3<-<-subset(Datos2, subset=(!Datos2$gen_sp == "Agapostemon species" & !Datos2$gen_sp == "Andrena species" & !Datos2$gen_sp == "Anthidiellum species" & !Datos2$gen_sp == "Augochlorella species" & !Datos2$gen_sp == "Bombus species" & !Datos2$gen_sp == "Calliopsis species" & !Datos2$gen_sp == "Ceratina species" & !Datos2$gen_sp == "Ceratina species" & !Datos2$gen_sp == "Coelioxys species"  & !Datos2$gen_sp == "Epeolus species" & !Datos2$gen_sp == "Halictus species" & !Datos2$gen_sp == "Heriades species" & !Datos2$gen_sp == "Eucera species" & !Datos2$gen_sp == "Hylaeus species" & !Datos2$gen_sp == "Lasioglossum species" & !Datos2$gen_sp == "Megachile species" & !Datos2$gen_sp == "Melissodes species" & !Datos2$gen_sp == "Melissodes species" & !Datos2$gen_sp == "Nomada species" & !Datos2$gen_sp == "Osmia species" & !Datos2$gen_sp == "Perdita species" & !Datos2$gen_sp == "Pseudopanurgus species" & !Datos2$gen_sp == "Sphecodes species"))
pff<-subset(Datos3, subset=(!Datos3$gen_sp == "Ceratina calcarata/dupla" & !Datos3$gen_sp == "Halictus ligatus/poeyi" & !Datos3$gen_sp == "Hylaeus affinis/modestus" & !Datos3$gen_sp == "Lasioglossum admirandum/rohweri" & !Datos3$gen_sp == "Lasioglossum viridatum group"))
Ceratina calcarata/dupla
write.csv(pff, "Datos4.csv")

Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
Datos4
#Convierto mis variables de habitat y especies en una tabla con la abundancia de 
#cada especie para cada habitat
jiji <-aggregate (Datos4$habitat.extracted ~ Datos4$gen_sp, FUN = summary, Datos4)
sink("jiji.txt")
print(jiji)
sink()
jiji
juju<-c(jiji)
juju
sink("juju.txt")
print(juju)
sink()
habpref<-as.data.frame(juju)
habpref
write.csv(habpref, "especiesyhabitats.csv")


jaja <-aggregate (Datos4$gen_sp ~ Datos4$habitat.extracted, FUN = summary, Datos4)
jaja
jeje<-c(jaja)
jeje
memes<-as.data.frame(jeje)
memes
write.csv(habpref, "habitatsyespecies.csv")


#Cargamos los dataframes que hemos creado
especiesyhabitats <- read.csv("~/Desktop/Tesis/R/habpref/especiesyhabitats.csv", header=TRUE)
habitatsyespecies <- read.csv("~/Desktop/Tesis/R/habpref/habitatsyespecies.csv", header=TRUE)

#Renombramos las variables y eliminamos la primera fila que no tiene
#información relevante

names(especiesyhabitats)[2]<-"gen_sp"
names(especiesyhabitats)[4]<-"Barren.Land"
names(especiesyhabitats)[5]<-"Cultivated.Crops"
names(especiesyhabitats)[6]<-"Deciduous.Forest"
names(especiesyhabitats)[7]<-"Developed.High.Intensity"
names(especiesyhabitats)[8]<-"Developed.Low.Intensity"
names(especiesyhabitats)[9]<-"Developed.Medium.Intensity"
names(especiesyhabitats)[10]<-"Developed.Open.Space"
names(especiesyhabitats)[11]<-"Emergent.Herbaceuous.Wetlands"
names(especiesyhabitats)[12]<-"Evergreen.Forest"
names(especiesyhabitats)[13]<-"Hay.Pasture"
names(especiesyhabitats)[14]<-"Herbaceuous"
names(especiesyhabitats)[15]<-"Mixed.Forest"
names(especiesyhabitats)[16]<-"Open.Water"
names(especiesyhabitats)[17]<-"Shrub.Scrub"
names(especiesyhabitats)[18]<-"Woody.Wetlands"
especiesyhabitats
especiesyhabitats<-especiesyhabitats[-1,]
write.csv(especiesyhabitats,"especiesyhabitats.csv")

#Calculamos las medias de abundancia de individuos en cada habitat, quitando las
#ausencias
especiesyhabitats
View(especiesyhabitats)
lapply(especiesyhabitats, mean, na.rm=TRUE)
bl<-subset(especiesyhabitats,!Barren.Land %in% c(0))
mean(bl$Barren.Land)
ds <- subset(especiesyhabitats,!Cultivated.Crops %in% c(0)) 
mean(ds$Cultivated.Crops)
df<-subset(especiesyhabitats,!Deciduous.Forest %in% c(0))
mean(df$Deciduous.Forest)
dh<-subset(especiesyhabitats,!Developed.High.Intensity %in% c(0))
mean(dh$Developed.High.Intensity)
dl<-subset(especiesyhabitats,!Developed.Low.Intensity %in% c(0))
mean(dl$Developed.Low.Intensity)
dm<-subset(especiesyhabitats,!Developed.Medium.Intensity %in% c(0))
mean(dm$Developed.Medium.Intensity)
do<-subset(especiesyhabitats,!Developed.Open.Space %in% c(0))
mean(do$Developed.Open.Space)
em<-subset(especiesyhabitats,!Emergent.Herbaceuous.Wetlands %in% c(0))
mean(em$Emergent.Herbaceuous.Wetlands)
ef<-subset(especiesyhabitats,!Evergreen.Forest %in% c(0))
mean(ef$Evergreen.Forest)
hp<-subset(especiesyhabitats,!Hay.Pasture %in% c(0))
mean(hp$Hay.Pasture)
hb<-subset(especiesyhabitats,!Herbaceuous %in% c(0))
mean(hb$Herbaceuous)
mf<-subset(especiesyhabitats,!Mixed.Forest %in% c(0))
mean(mf$Mixed.Forest)
ow<-subset(especiesyhabitats,!Mixed.Forest %in% c(0))
mean(ow$Open.Water)
ss<-subset(especiesyhabitats,!Shrub.Scrub %in% c(0))
mean(ss$Shrub.Scrub)
ww<-subset(especiesyhabitats,!Woody.Wetlands %in% c(0))
mean(ww$Woody.Wetlands)

#Quiero saber el nº de especies en cada habitat y qué especies hay
#Creamos un subset para cada habitat
barrenland<-subset(Datos4, subset =(Datos4$habitat.extracted== "Barren Land"))
barrenland
barrenlands<-subset(barrenland, subset = (duplicated(barrenland$gen_sp)==FALSE))
sink("barrenlandspecies.txt")
barrenlands$gen_sp
sink()

cultivatedcrops<-subset(Datos4, subset =(Datos4$habitat.extracted== "Cultivated Crops"))
cultivatedcrops
cultivatedcropss<-subset(cultivatedcrops, subset = (duplicated(cultivatedcrops$gen_sp)==FALSE))
sink("cultivatedcropsspecies.txt")
cultivatedcropss$gen_sp
sink()

deciduousforest<-subset(Datos4, subset =(Datos4$habitat.extracted== "Deciduous Forest"))
deciduousforest
deciduousforests<-subset(deciduousforest, subset = (duplicated(deciduousforest$gen_sp)==FALSE))
sink("deciduousforestspecies.txt")
deciduousforests$gen_sp
sink()

developedhighintensity<-subset(Datos4, subset =(Datos4$habitat.extracted== "Developed, High Intensity"))
developedhighintensity
developedhighintensitys<-subset(developedhighintensity, subset = (duplicated(developedhighintensity$gen_sp)==FALSE))
sink("developedhighintensityspecies.txt")
developedhighintensitys$gen_sp
sink()

developedlowintensity<-subset(Datos4, subset =(Datos4$habitat.extracted== "Developed, Low Intensity"))
developedlowintensity
developedlowintensitys<-subset(developedlowintensity, subset = (duplicated(developedlowintensity$gen_sp)==FALSE))
sink("developedlowintensityspecies.txt")
developedlowintensitys$gen_sp
sink()

developedmediumintensity<-subset(Datos4, subset =(Datos4$habitat.extracted== "Developed, Medium Intensity"))
developedmediumintensity
developedmediumintensitys<-subset(developedmediumintensity, subset = (duplicated(developedmediumintensity$gen_sp)==FALSE))
sink("developedmediumintensityspecies.txt")
developedmediumintensitys$gen_sp
sink()

developedopenspace<-subset(Datos4, subset =(Datos4$habitat.extracted== "Developed, Open Space"))
developedopenspace
developedopenspaces<-subset(developedopenspace, subset = (duplicated(developedopenspace$gen_sp)==FALSE))
sink("developedopenspacespecies.txt")
developedopenspaces$gen_sp
sink()

emergentherbaceuouswetlands<-subset(Datos4, subset =(Datos4$habitat.extracted== "Emergent Herbaceuous Wetlands"))
emergentherbaceuouswetlands
emergentherbaceuouswetlandss<-subset(emergentherbaceuouswetlands, subset = (duplicated(emergentherbaceuouswetlands$gen_sp)==FALSE))
sink("emergentherbaceuouswetlandsspecies.txt")
emergentherbaceuouswetlandss$gen_sp
sink()

evergreenforest<-subset(Datos4, subset =(Datos4$habitat.extracted== "Evergreen Forest"))
evergreenforest
evergreenforests<-subset(evergreenforest, subset = (duplicated(evergreenforest$gen_sp)==FALSE))
sink("evergreenforestspecies.txt")
evergreenforests$gen_sp
sink()

haypasture<-subset(Datos4, subset =(Datos4$habitat.extracted== "Hay/Pasture"))
haypasture
haypastures<-subset(haypasture, subset = (duplicated(haypasture$gen_sp)==FALSE))
sink("haypasturespecies.txt")
haypastures$gen_sp
sink()

herbaceus<-subset(Datos4, subset =(Datos4$habitat.extracted== "Herbaceuous"))
herbaceus
herbaceuss<-subset(herbaceus, subset = (duplicated(herbaceus$gen_sp)==FALSE))
sink("herbaceusspecies.txt")
herbaceuss$gen_sp
sink()

summary(Datos4$habitat.extracted)

mixedforest<-subset(Datos4, subset =(Datos4$habitat.extracted== "Mixed Forest"))
mixedforest
mixedforests<-subset(mixedforest, subset = (duplicated(mixedforest$gen_sp)==FALSE))
sink("mixedforestspecies.txt")
mixedforests$gen_sp
sink()

openwater<-subset(Datos4, subset =(Datos4$habitat.extracted== "Open Water"))
openwater
openwaters<-subset(openwater, subset = (duplicated(openwater$gen_sp)==FALSE))
sink("openwaterspecies.txt")
openwaters$gen_sp
sink()

shrubscrub<-subset(Datos4, subset =(Datos4$habitat.extracted== "Shrub/Scrub"))
shrubscrub
shrubscrubs<-subset(shrubscrub, subset = (duplicated(shrubscrub$gen_sp)==FALSE))
sink("shrubscrubspecies.txt")
shrubscrubs$gen_sp
sink()

woodywetlands<-subset(Datos4, subset =(Datos4$habitat.extracted== "Woody Wetlands"))
woodywetlands
woodywetlandss<-subset(woodywetlands, subset = (duplicated(woodywetlands$gen_sp)==FALSE))
sink("woodywetlandsspecies.txt")
woodywetlandss$gen_sp
sink()
3

summary(Datos2$gen_sp)
pfff<-<-subset(Datos2, subset=(!Datos2$gen_sp == "Agapostemon species" & !Datos2$gen_sp == "Andrena species" & !Datos2$gen_sp == "Anthidiellum species" & !Datos2$gen_sp == "Augochlorella species" & !Datos2$gen_sp == "Bombus species" & !Datos2$gen_sp == "Calliopsis species" & !Datos2$gen_sp == "Ceratina species" & !Datos2$gen_sp == "Ceratina species" & !Datos2$gen_sp == "Coelioxys species"  & !Datos2$gen_sp == "Epeolus species" & !Datos2$gen_sp == "Halictus species" & !Datos2$gen_sp == "Heriades species" & !Datos2$gen_sp == "Eucera species" & !Datos2$gen_sp == "Hylaeus species" & !Datos2$gen_sp == "Lasioglossum species" & !Datos2$gen_sp == "Megachile species" & !Datos2$gen_sp == "Melissodes species" & !Datos2$gen_sp == "Melissodes species" & !Datos2$gen_sp == "Nomada species" & !Datos2$gen_sp == "Osmia species" & !Datos2$gen_sp == "Perdita species" & !Datos2$gen_sp == "Pseudopanurgus species" & !Datos2$gen_sp == "Sphecodes species"))
pff<-subset(Datos3, subset=(!Datos3$gen_sp == "Ceratina calcarata/dupla" & !Datos3$gen_sp == "Halictus ligatus/poeyi" & !Datos3$gen_sp == "Hylaeus affinis/modestus" & !Datos3$gen_sp == "Lasioglossum admirandum/rohweri" & !Datos3$gen_sp == "Lasioglossum viridatum group"))
Ceratina calcarata/dupla
write.csv(Datos4, "Datos4.csv")


write.csv(especiesyhabitats,"vamospapi.csv")

#Presencia de especies en cada habitat
meji<-especiesyhabitats
meji
replace(meji, meji>1,1)
mejib<-t(mejib)
View(mejib)
write.csv(presencia,"presencia.csv")
presencia
colSums(presencia)
summary(presencia)
presencia<-t(presencia)
View(presencia)
write.csv(mejib, "importanciahabitats.csv")

xdxd

