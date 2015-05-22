#usar dataframe "especiesyhabitats" creado en el script 
#"abundancia y habitats"
str(especiesyhabitats)
summary(especiesyhabitats)



library(bipartite)
#Para que las funciones del library "bipartite" funcionen bien, 
#tenemos que transformar los datos para que tenga la forma que necesita
#también necesitamos una matriz de interacción

#convertimos a matriz nuestros datos
especiesyhabitatsm<-as.matrix(especiesyhabitats)

#Convertimos nuestros datos a numéricos
especiesyhabitatsm<-apply(especiesyhabitatsm, MARGIN=2, FUN=as.numeric)


#nombramos las filas y eliminamos la columna de nombres que nos sobra
row.names(especiesyhabitatsm)<- especiesyhabitats[,1]
especiesyhabitatsm<-especiesyhabitatsm[,-1]
#nombramos las columnas
names(especiesyhabitatsm)<-especiesyhabitatsm[1,]


#Hacemos un diagrama de interacción simple (con muchas interacciones falla)
visweb(especiesyhabitatsm)

#Aquí uno de ejemplo
data(Safariland)
visweb(Safariland)

#Con esta función, podemos crear una red de interacción
plotweb(especiesyhabitatsm)
#podemos rotar el texto y reducir el tamaño de los cuadrados
plotweb(especiesyhabitatsm, text.rot=90, low.lablength=2, arrow="both", y.width.low=0.02, y.width.high = 0.02,)

#Ahora creamos un vector, cuyos valores son las "species strenght"
#pero no se aplica en species sino son valores para los habitats
c1<-strength(especiesyhabitatsm, type="Bascompte")
c1









