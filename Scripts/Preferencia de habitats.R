Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
Datos4

#Vamos a crear la matriz de datos de abundancia esperada para cada habitat, para ello
#primero sacamos la matriz de interacciones de nuestros datos
jiji <-aggregate (Datos4$habitat.extracted ~ Datos4$gen_sp, FUN = summary, Datos4)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
View(momomo)
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
View(observed)
observed<-momomo
View(expected)
#así, expected es la matriz de abundancias esperadas, y observed la de observadas

#probamos ahora algunos indices de preferencias de habitats 

#"Forage ratio"
#obs: observed frequency of visits to item 1
#exp: expected frequency of visits to item 1 (i.e. resource abundance/availability)
fr <- function(obs, exp){
  p_obs <- obs/sum(obs)
  p_exp <- exp/sum(exp)
  out <- p_obs/p_exp
  out
}
sum(observed)
sum(expected)
Forageratio<-fr(observed,expected)
write.csv(Forageratio,"Forageratio.csv")

#Electicity
electicity <- function(obs, exp){
  p_obs <- obs/sum(obs)
  p_exp <- exp/sum(exp)
  out <- (p_obs-p_exp)/(p_obs+p_exp)
}
Electicity<-electicity(observed,expected)
write.csv(Electicity, "Electicity.csv")

#jacobs
jacobs <- function(obs, exp){
  p_obs <- obs/sum(obs)
  p_exp <- exp/sum(exp)
  out <- (p_obs-p_exp)/(p_obs+p_exp-2*p_obs*p_exp)
  out
}
jacobs(observed,expected)
View(jacobs(observed,expected))
Jacbos<-jacobs(observed,expected)
write.csv(Jacbos, "Jacobs.csv")

#Chi test, vamos a hacer un data frame con solo las significancias


chi_pref <- function(obs, exp, alpha = 0.05){
  chi <- chisq.test(obs, p = exp, rescale.p = TRUE)
  print(chi) #tells you if there is an overall preference. (sig = pref)
  res <- chi$residuals
  #res <- (obs-exp)/sqrt(exp) #hand calculation, same result.
  #calculate bonferoni Z-statistic for each plant.
  alpha <- alpha
  k <- length(obs)
  n <- sum(obs)
  p_obs <- obs/n
  ak <- alpha/(2*k)
  Zak <- abs(qnorm(ak))
  low_interval <- p_obs - (Zak*(sqrt(p_obs*(1-p_obs)/n)))
  upper_interval <- p_obs + (Zak*(sqrt(p_obs*(1-p_obs)/n)))
  p_exp <- exp/sum(exp)
  sig <- ifelse(p_exp >= low_interval & p_exp <= upper_interval, "ns", "sig")
  out <- data.frame(chi_test_p = rep(chi$p.value, length(res)), 
                    chi_residuals = res, sig = sig)
  out
}
n<-100
out <- data.frame(chi_test_p = NA, chi_residuals = NA,   ...)

for(n in 1:398){
  chi <- chi_pref(observedm[n,],expectedm[n,], alpha=0.05)
  chi
  chi$habitat <- rownames(chi)
  chi$sp <- rep(rownames(observedm)[n], 15)
  out <- rbind(out, chi)
  #camarero[n] = chi[3]
}
tobserved<-t(observed)
camarero<-as.data.frame(camarero)
rownames(camarero)<-rownames(tobserved)
names(camarero)<-rownames(observed)
is.data.frame(camarero)
camarero<-t(camarero)
View(camarero)
write.csv(camarero,"chi.test.significane.csv")
#######################################################################################
#Quiero calcular el % de especies que tienen una preferencia de habitat, para ello
#usamos el chi.test que nos dice si una especie tiene preferencia (si chi.test < 0.05)
######################################################################################
Datos4 <- read.csv("~/Desktop/Tesis/R/habpref/Datos4.csv")
Datos4

#Vamos a crear la matriz de datos de abundancia esperada para cada habitat, para ello
#primero sacamos la matriz de interacciones de nuestros datos
jiji <-aggregate (Datos4$habitat.extracted ~ Datos4$gen_sp, FUN = summary, Datos4)
juju<-c(jiji)
juju
habpref<-as.data.frame(juju)
meji<-habpref[-1,]
View(momomo)
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
View(observed)
observed<-momomo
View(expected)
obs
exp
#Ahora creamos la función que utiliza la chisquared
chi_pref <- function(obs, exp, alpha = 0.05){
  chi <- chisq.test(obs, p = exp, rescale.p = TRUE)
  print(chi) #tells you if there is an overall preference. (sig = pref)
  res <- chi$residuals
  #res <- (obs-exp)/sqrt(exp) #hand calculation, same result.
  #calculate bonferoni Z-statistic for each plant.
  alpha <- alpha
  k <- length(obs)
  n <- sum(obs)
  p_obs <- obs/n
  ak <- alpha/(2*k)
  Zak <- abs(qnorm(ak))
  low_interval <- p_obs - (Zak*(sqrt(p_obs*(1-p_obs)/n)))
  upper_interval <- p_obs + (Zak*(sqrt(p_obs*(1-p_obs)/n)))
  p_exp <- exp/sum(exp)
  sig <- ifelse(p_exp >= low_interval & p_exp <= upper_interval, "ns", "sig")
  out <- data.frame(chi_test_p = rep(chi$p.value, length(res)), 
                    chi_residuals = res, sig = sig)
  out
}

#Creamos una matriz vacía para rellenarla con nuestros datos, con las columnas que
#ya sabemos que vamos a querer
getout <- data.frame(chi_test_p = NA, chi_residuals = NA, sig = NA, habitat = NA, sp = NA)
#ahora hacemos un loop un número de veces igual al número de especies (filas)que tenemos
#al resultado le agregamos una fila con el nombre de cada habitat para los que testa
#y también otra columna con el nombre (repetido 15 veces porque son 15 habitats)
for(n in 1:398){
  chi <- chi_pref(observedm[n,],expectedm[n,], alpha=0.05)
  chi
  chi$habitat <- rownames(chi)
  chi$sp <- rep(rownames(observedm)[n], 15)
  getout <- rbind(getout, chi)
}
#getout es nuestra tabla de datos con las preferencias de habitat
nrow(getout)
nrow(obs)
names(getout)
#Filtramos solo aquellos valores <0.05 que son los que según chi.test hay preferencia
pref<-subset(getout, subset=(getout$chi_test_p < 0.05))
#Queremos que cada nombre de especie solo aparezca una vez
preff<-subset(pref, subset=((duplicated(pref$chi_test_p)== FALSE)))
#ahora dividimos el número de especies que tienen preferencia de habitat entre el número
#total de especies, para ver el %
(nrow(preff)/nrow(obs))*100
View(preff)

