##Porcentaje de especies con preferencia >100 individuos#####################################################################################
#Quiero calcular el % de especies que tienen una preferencia de habitat, para ello
#usamos el chi.test que nos dice si una especie tiene preferencia (si chi.test < 0.05)
#
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
filtra<-rowSums(momomo)
mememe<-cbind(momomo,filtra)
mo<-subset(mememe, subset=(mememe$filtra)>100)
View(mo)
momomo<-mo[,-16]
observed<-momomo
View(observed)
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
a<-multiplicador
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
nrow(matriz2)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:92,]
expected<-(matriz1*matriz2)
View(observed)
View(expected)
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
observed<-as.matrix(observed)
expected<-as.matrix(expected)
for(n in 1:92){
  chi <- chi_pref(observed[n,],expected[n,], alpha=0.05)
  chi
  chi$habitat <- rownames(chi)
  chi$sp <- rep(rownames(observed)[n], 15)
  getout <- rbind(getout, chi)
}
warnings()
View(getout)
#getout es nuestra tabla de datos con las preferencias de habitat
nrow(getout)
nrow(observed)
names(getout)

#Filtramos solo aquellos valores <0.05 que son los que según chi.test hay preferencia
pref<-subset(getout, subset=(getout$chi_test_p < 0.05))
View(pref)
#Queremos que cada nombre de especie solo aparezca una vez
preff<-subset(pref, subset=((duplicated(pref$chi_test_p)== FALSE)))
#ahora dividimos el número de especies que tienen preferencia de habitat entre el número
#total de especies, para ver el %
cien<-(nrow(preff)/nrow(observed))*100
View(preff)


##Porcentaje de especies con preferencia >30 individuos#####################################################################################
#Quiero calcular el % de especies que tienen una preferencia de habitat, para ello
#usamos el chi.test que nos dice si una especie tiene preferencia (si chi.test < 0.05)
#
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
filtra<-rowSums(momomo)
mememe<-cbind(momomo,filtra)
mo<-subset(mememe, subset=(mememe$filtra)>30)
View(mo)
momomo<-mo[,-16]
observed<-momomo
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
a<-multiplicador
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
nrow(matriz2)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:149,]
expected<-(matriz1*matriz2)
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
observed<-as.matrix(observed)
expected<-as.matrix(expected)
for(n in 1:149){
  chi <- chi_pref(observed[n,],expected[n,], alpha=0.05)
  chi
  chi$habitat <- rownames(chi)
  chi$sp <- rep(rownames(observed)[n], 15)
  getout <- rbind(getout, chi)
}
warnings()
View(getout)
#getout es nuestra tabla de datos con las preferencias de habitat
nrow(getout)
nrow(observed)
names(getout)

#Filtramos solo aquellos valores <0.05 que son los que según chi.test hay preferencia
pref<-subset(getout, subset=(getout$chi_test_p < 0.05))
View(pref)
#Queremos que cada nombre de especie solo aparezca una vez
preff<-subset(pref, subset=((duplicated(pref$chi_test_p)== FALSE)))
#ahora dividimos el número de especies que tienen preferencia de habitat entre el número
#total de especies, para ver el %
treinta<-(nrow(preff)/nrow(observed))*100
View(preff)

##Porcentaje de especies con preferencia >10 individuos#####################################################################################
#Quiero calcular el % de especies que tienen una preferencia de habitat, para ello
#usamos el chi.test que nos dice si una especie tiene preferencia (si chi.test < 0.05)
#
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
filtra<-rowSums(momomo)
mememe<-cbind(momomo,filtra)
View(mememe)
mo<-subset(mememe, subset=(mememe$filtra)>10)
View(mo)
momomo<-mo[,-16]
observed<-momomo
View(observed)
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
a<-multiplicador
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
nrow(matriz2)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:218,]
expected<-(matriz1*matriz2)
View(observed)
View(expected)
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
observed<-as.matrix(observed)
expected<-as.matrix(expected)
for(n in 1:218){
  chi <- chi_pref(observed[n,],expected[n,], alpha=0.05)
  chi
  chi$habitat <- rownames(chi)
  chi$sp <- rep(rownames(observed)[n], 15)
  getout <- rbind(getout, chi)
}
warnings()
View(getout)
#getout es nuestra tabla de datos con las preferencias de habitat
nrow(getout)
nrow(observed)
names(getout)

#Filtramos solo aquellos valores <0.05 que son los que según chi.test hay preferencia
pref<-subset(getout, subset=(getout$chi_test_p < 0.05))
View(preff)
#Queremos que cada nombre de especie solo aparezca una vez
preff<-subset(pref, subset=((duplicated(pref$chi_test_p)== FALSE)))
#ahora dividimos el número de especies que tienen preferencia de habitat entre el número
#total de especies, para ver el %
diez<-(nrow(preff)/nrow(observed))*100
View(preff)

##Porcentaje de especies con preferencia sin singletones########################
#Quiero calcular el % de especies que tienen una preferencia de habitat, para ello
#usamos el chi.test que nos dice si una especie tiene preferencia (si chi.test < 0.05)
#
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
filtra<-rowSums(momomo)
mememe<-cbind(momomo,filtra)
View(mememe)
mo<-subset(mememe, subset=(mememe$filtra)>1)
View(mo)
momomo<-mo[,-16]
observed<-momomo
View(observed)


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
a<-multiplicador
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
nrow(matriz2)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:340,]
expected<-(matriz1*matriz2)
View(observed)
View(expected)
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
observed<-as.matrix(observed)
expected<-as.matrix(expected)
for(n in 1:340){
  chi <- chi_pref(observed[n,],expected[n,], alpha=0.05)
  chi
  chi$habitat <- rownames(chi)
  chi$sp <- rep(rownames(observed)[n], 15)
  getout <- rbind(getout, chi)
}
warnings()
View(getout)
#getout es nuestra tabla de datos con las preferencias de habitat
nrow(getout)
nrow(observed)
names(getout)

#Filtramos solo aquellos valores <0.05 que son los que según chi.test hay preferencia
pref<-subset(getout, subset=(getout$chi_test_p < 0.05))
View(preff)
#Queremos que cada nombre de especie solo aparezca una vez
preff<-subset(pref, subset=((duplicated(pref$chi_test_p)== FALSE)))
#ahora dividimos el número de especies que tienen preferencia de habitat entre el número
#total de especies, para ver el %
sinsingletones<-(nrow(preff)/nrow(observed))*100






##Porcentaje de especies con preferencia sin filtrar#####################################################################################
#Quiero calcular el % de especies que tienen una preferencia de habitat, para ello
#usamos el chi.test que nos dice si una especie tiene preferencia (si chi.test < 0.05)
#
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
observed<-momomo
View(observed)
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
a<-multiplicador
matriz2<-rbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
nrow(matriz2)
matriz21<-rbind(matriz2,matriz2)
matriz2<-matriz21[1:398,]
expected<-(matriz1*matriz2)
View(observed)
View(expected)
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
observed<-as.matrix(observed)
expected<-as.matrix(expected)
for(n in 1:398){
  chi <- chi_pref(observed[n,],expected[n,], alpha=0.05)
  chi
  chi$habitat <- rownames(chi)
  chi$sp <- rep(rownames(observed)[n], 15)
  getout <- rbind(getout, chi)
}
warnings()
View(getout)
#getout es nuestra tabla de datos con las preferencias de habitat
nrow(getout)
nrow(observed)
names(getout)

#Filtramos solo aquellos valores <0.05 que son los que según chi.test hay preferencia
pref<-subset(getout, subset=(getout$chi_test_p < 0.05))
View(preff)
#Queremos que cada nombre de especie solo aparezca una vez
preff<-subset(pref, subset=((duplicated(pref$chi_test_p)== FALSE)))
#ahora dividimos el número de especies que tienen preferencia de habitat entre el número
#total de especies, para ver el %
alldata<-(nrow(preff)/nrow(observed))*100
alldata
sinsingletones
diez
treinta
cien
points(c(alldata,sinsingletones,diez,treinta,cien),ylim = c(0,100),main="Habitat Preference",names=nombrez,xlab="Treatment",ylab="Percentage")
nombrez<-c("All Data","Without Singletones","n>10","n>30","n>100")
