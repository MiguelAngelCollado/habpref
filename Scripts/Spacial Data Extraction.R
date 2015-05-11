
# Function to extract the data in R instead of Arc
# inputs: year, buffer size (in meters), point data file, 
#   cover data directory, and output file directory
# output: csv file with site id, cover type, and % in buffer

extract_cover <- function(year, buffer,
                          point_d = "sites.csv",  
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
  sites <- read.csv(point_d, header=T, sep=";")
  head(sites)
  coords <- sites[, c("longitude", "latitude")]  
  #convert lat/lon to appropriate projection
  names(coords) <- c("x", "y")
  coordinates(coords) <- ~x + y
  proj4string(coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  crs_args <- NLCD@crs@projargs
  sites_transformed <- spTransform(coords, CRS(crs_args))
  
  #extract land cover data for each point, given buffer size
  Landcover <- extract(NLCD, sites_transformed, buffer=buffer)
  
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
  mydf <- data.frame(id = rep(sites$id, lapply(summ, length)),
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

?levels
levels(x = covers.names)
str(sites)
as.numeric(sites$id)
