str(Datos$habitat.extracted)
head(Datos$habitat.extracted)
summary(Datos2$habitat.extracted)
hist(Datos$habitat.extracted)
plot(Datos$habitat.extracted)
summary(Datos2$gen_sp)


jiji <-aggregate (Datos2$habitat.extracted ~ Datos2$gen_sp, FUN = summary, Datos2)
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
