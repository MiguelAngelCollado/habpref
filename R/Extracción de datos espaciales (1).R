#Extracción de datos espaciales sin buffer----
getwd()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/habpref")
# Function to extract the data in R instead of Arc
# inputs: year, point data file, 
#   cover data directory, and output file directory
# output: csv file with site id, cover type, and % in buffer
#Year is only used to give the output a name
year = 2006
extract_cover <- function(year,
                          point_d = "Datos.csv",  
                          data_dir="NLCD_data",
                          write_dir="extracted"){
  require(raster)
  require(rgdal)
  require(stringr)
  
  # load cover data 
  filename <- paste(data_dir, "/nlcd_",
                    year,
                    "_landcover_2011_edition_2014_10_10.img",
                    sep="")
  NLCD <- raster(filename)
  
  # load site data
  Datos <- read.csv(point_d, header=T, sep=",") #Si falla, usar como sep "," o ";"
  #según tus datos
  head(Datos)
  View(Datos)
  coords <- Datos[, c("longitude", "latitude")]  
  #convert lat/lon to appropriate projection
  names(coords) <- c("x", "y")
  coordinates(coords) <- ~x + y
  proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  crs_args <- NLCD@crs@projargs
  Datos_transformed <- spTransform(coords, CRS(crs_args))
  
  #extract land cover data for each point, given buffer size
  Landcover <- extract(NLCD, Datos_transformed)
  
  # summarize each site's data by proportion of each cover type
  summ <- lapply(Landcover, function(x){
    prop.table(table(x))
  }
  )
  
  # generate land cover number to name conversions
  num.codes <- unique(unlist(Landcover))
  cover.names <- NLCD@data@attributes[[1]]$NLCD.2006.Land.Cover.Class[num.codes + 1]
  levels(cover.names)[1] <- NA # first level is ""
  conversions <- data.frame(num.codes, cover.names)
  conversions <- na.omit(conversions)
  conversions <- conversions[order(conversions$num.codes),]
  
  # convert to data frame
  mydf <- data.frame(id = rep(Datos$id, lapply(summ, length)),
                     cover = names(unlist(summ)),
                     percent = unlist(summ)
  )
  
  # create cover name column
  mydf$cover2 <- mydf$cover
  levels(mydf$cover2) <- conversions$cover.names
  
  # write output
  out_name <- paste(write_dir, "/",
                    year, "_cover_", 
                    "_m_buffer.csv", sep="")
  write.csv(mydf, out_name)
}

#Aquí ejecutamos la función que hemos creado, esto crea en el directorio un archivo
#indicando para cada punto de muestreo, qué habitats hay en un buffer de 1000m2 y 
#qué habitats aparecen y en qué proporción
extract_cover(year = 2006, point_d = "Datos.csv", write_dir = "extracted")

#Extracción de datos espaciales con buffer##(Ejecutar la función puede tardar >8 horas
getwd()
setwd("/Users/Bartomeus_lab/Desktop/Tesis/R/habpref")
# Function to extract the data in R instead of Arc
# inputs: year, buffer size (in meters), point data file, 
#   cover data directory, and output file directory
# output: csv file with site id, cover type, and % in buffer
year = 2006
buffer = 1000
extract_cover <- function(year, buffer,
                          point_d = "Datos.csv",  
                          data_dir="NLCD_data",
                          write_dir="extracted"){
  require(raster)
  require(rgdal)
  require(stringr)
  
  # load cover data 
  filename <- paste(data_dir, "/nlcd_",
                    year,
                    "_landcover_2011_edition_2014_10_10.img",
                    sep="")
  NLCD <- raster(filename)
  
  # load site data
  Datos <- read.csv(point_d, header=T, sep=",") #Si falla, Datos a cambiar el sep por ","
  head(Datos)
  coords <- Datos[, c("longitude", "latitude")]  
  #convert lat/lon to appropriate projection
  names(coords) <- c("x", "y")
  coordinates(coords) <- ~x + y
  proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  crs_args <- NLCD@crs@projargs
  Datos_transformed <- spTransform(coords, CRS(crs_args))
  
  #extract land cover data for each point, given buffer size
  Landcover <- extract(NLCD, Datos_transformed, buffer=buffer)
  
  # summarize each site's data by proportion of each cover type
  summ <- lapply(Landcover, function(x){
    prop.table(table(x))
  }
  )
  
  # generate land cover number to name conversions
  num.codes <- unique(unlist(Landcover))
  cover.names <- NLCD@data@attributes[[1]]$NLCD.2006.Land.Cover.Class[num.codes + 1]
  levels(cover.names)[1] <- NA # first level is ""
  conversions <- data.frame(num.codes, cover.names)
  conversions <- na.omit(conversions)
  conversions <- conversions[order(conversions$num.codes),]
  
  # convert to data frame
  mydf <- data.frame(id = rep(Datos$id, lapply(summ, length)),
                     cover = names(unlist(summ)),
                     percent = unlist(summ)
  )
  
  # create cover name column
  mydf$cover2 <- mydf$cover
  levels(mydf$cover2) <- conversions$cover.names
  
  # write output
  out_name <- paste(write_dir, "/",
                    year, "_cover_", buffer,
                    "_m_buffer.csv", sep="")
  write.csv(mydf, out_name)
}

#Aquí ejecutamos la función que hemos creado
extract_cover(year = 2006, buffer= 1000, point_d = "Datos.csv", write_dir = "extracted")

