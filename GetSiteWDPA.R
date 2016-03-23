###############   Set Working directory  ####################
#######################################################
setwd("~/econservation/eConservation_March2016")

###############   Needed Packages  ####################
#######################################################
library(maptools)
library(rgdal)
library(sp)
library(plyr)
#######################################################

# I. Load WB sites
wb_web_biodiv_sites <- read.csv(paste(getwd(), "/eConservation_WB_2010_2015/Main_tables//WB_2010_2015/wb_web_biodiv_sites.csv", sep=""), 
                                header=T, fileEncoding = "latin1", sep="|")

# II.  For sites that have been associated by hand to a WDPA, find the coordinates of the sites without coordinates, meaning the centroid of the WDPA they have been associated to.
## II.1. Load the WDPA polygons centroid shapefile  
wdpa_centroids <- readShapePoints(paste(getwd(), "/gis_econservation/WDPA_Jan2016_shapefile_polygons_CENTROIDS/wdpa_centroids.shp", sep="")
## II.2. Subset sites without coordinats and allocate them the centroid coordinates of the corresponding WDPA 
sites_wo_coord <- wb_web_biodiv_sites[is.na(wb_web_biodiv_sites$latitude),]
for (i in 1:nrow(sites_wo_coord)){
sites_wo_coord[i,c(2,1)] <- wdpa_centroids@coords[wdpa_centroids@data$WDPAID==sites_wo_coord$wdpa_id[i],]
}
## II.3. Combine all sites back together
sites_w_coord <- wb_web_biodiv_sites[!is.na(wb_web_biodiv_sites$latitude),]
wb_web_biodiv_sites <- rbind.fill(sites_w_coord, sites_wo_coord)
rm(wdpa_centroids)
gc()

# III. For all sites - when applicable associate sites to a WDPA
## III.1. Convert SITE table to a point shapefile (ALL SITES NEED TO HAVE COORDINATES)
wb_web_biodiv_sites$wdpa_id <- NA
wb_web_biodiv_sites_POINTS <- wb_web_biodiv_sites
coordinates(wb_web_biodiv_sites_POINTS)=~longitude+latitude
# ### Visual check
# map_countries <- readShapePoly("E:/bicheor/gis_econservation/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
# plot(map_countries)
# plot(wb_web_biodiv_sites_POINTS, add=T, col="red")
# rm(sites_w_coord, sites_wo_coord, wdpa_centroids, map_countries)
# gc()
# Save the site point shapefile
writeSpatialShape(wb_web_biodiv_sites_POINTS, "E:/bicheor/Working/New_data_structure/R_database/GIS/WB_2010_2015/wb_web_biodiv_sites_POINTS.shp")

# Overlay sites with WDPA polygons and associate the corresponding WDPA IDs to the sites
# wb_web_biodiv_sites_POINTS <- readShapePoints("E:/bicheor/Working/New_data_structure/R_database/GIS/WB_2010_2015/wb_web_biodiv_sites_POINTS.shp")
wdpashpfile <- list.files("E:/bicheor/gis_econservation/WDPA_poly_divided/", pattern=".shp")
for (i in 1:length(wdpashpfile)){
  wdpa_poly <- readShapePoly(paste("E:/bicheor/gis_econservation/WDPA_poly_divided/", wdpashpfile[i], sep=""))
  o = over(wb_web_biodiv_sites_POINTS, wdpa_poly, returnList = T)
  for (j in 1:length(wb_web_biodiv_sites_POINTS)){
    wb_web_biodiv_sites_POINTS$wdpa_id[j] <- ifelse(!is.na(o[[j]]$WDPAID[1]) & is.na(wb_web_biodiv_sites_POINTS$wdpa_id[j]), 
                                                    paste(o[[j]]$WDPAID, collapse=","), 
                                                    ifelse(!is.na(o[[j]]$WDPAID[1]) & !is.na(wb_web_biodiv_sites_POINTS$wdpa_id[j]),
                                                           paste(wb_web_biodiv_sites_POINTS$wdpa_id[j], paste(o[[j]]$WDPAID, collapse=","), sep=","),
                                                           wb_web_biodiv_sites_POINTS$wdpa_id[j]))
    }
}
gc()
# Save the site point shapefile
writeSpatialShape(wb_web_biodiv_sites_POINTS, "E:/bicheor/Working/New_data_structure/R_database/GIS/WB_2010_2015/wb_web_biodiv_sites_POINTS.shp")




##################################
############  For sites that could not be associated to a WDPA polygon
# The distance between each site and its closest WDPA neighbour points and polygon was calculated in QGIS 
df <- readShapePoints("E:/bicheor/Working/New_data_structure/R_database/GIS/WB_2010_2015/wb_web_biodiv_sites_POINTS_MinDistWDPA.shp")

# The WDPA ID of the closest WDPA point is associated to the site as long as the site falls within 5 km of the WDPA point (and the site is not IN a polygon)
df@data$WDPAID <- ifelse(!is.na(df@data$wdpa_id), as.character(df@data$wdpa_id), ## Sites that are within a WDPA polygon
                         ifelse(is.na(df@data$wdpa_id) & df@data$MinDisWDPA <= 5000, as.character(df@data$nearWDPA),
                                ifelse(is.na(df@data$wdpa_id) & df@data$MinDistWDPA > 5000, as.character(df@data$nearWDPA), 
                                       NA
                                )))

# Save the site point shapefile
writeSpatialShape(df, "E:/bicheor/Working/New_data_structure/R_database/GIS/WB_2010_2015/wb_web_biodiv_sites_POINTS_MinDistWDPA.shp")


















#####################  #####################  #####################  #####################  #####################  
## Associate sites with the wdpa id when applicable
#####################  SEE GetSiteWDPA.R

## Associate sites to the country they are located in
map_countries <- readShapePoly("E:/bicheor/econservation/eConservation_March2016/eConservation_GIS/Coutries_and_marine_territories/gaul_eez_labels.shp")
o = over(wb_web_biodiv_sites_POINTS, map_countries, returnList = T)
for (j in 1:length(wb_web_biodiv_sites_POINTS)){
  wb_web_biodiv_sites_POINTS@data$country[j] <- as.character(o[[j]]$countries_)
}
countries <- readShapePoly("E:/bicheor/econservation/eConservation_March2016/eConservation_GIS/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
wb_web_biodiv_sites_POINTS@data <- merge(wb_web_biodiv_sites_POINTS@data, 
                                         subset(countries@data, select=c(REGION_WB, ISO_A2, ECONOMY)),
                                         by.x="country", by.y="ISO_A2", all.x=T, all.y=F)
### Save the site point shapefile
writeSpatialShape(wb_web_biodiv_sites_POINTS, "E:/bicheor/econservation/eConservation_March2016/eConservation_GIS/WB_2010_2015_sites/wb_web_biodiv_sites_POINTS_MinDistWDPA.shp")



#####################  #####################  #####################  #####################  #####################  #####################  








