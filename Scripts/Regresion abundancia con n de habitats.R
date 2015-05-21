
#Regresión abundancia - presencia en habitats
banana <- read.csv("~/Desktop/Tesis/R/habpref/regresionabunhabit.csv")
banana
names(banana)
pairs(banana[,2:3])
cor(banana[,2:3])
regresion <- lm(habitat.cuantity ~ abundance, data = banana)
summary(regresion)
plot(banana$habitat.cuantity, banana$abundance, xlab = "Presence", ylab = "Abundance")
abline()

#Misma regresión con transformación en la abundancia
banana <- read.csv("~/Desktop/Tesis/R/habpref/regresionabunhabit.csv")
banana
names(banana)
pairs(banana[,2:3])
cor(banana[,2:3])
regresion <- lm(habitat.cuantity ~ log(abundance), data = banana)
summary(regresion)
plot(banana$habitat.cuantity, log(banana$abundance), xlab = "Presence", ylab = "LogAbundance")
abline()




