###############   Set Working directory  ####################
#######################################################
setwd("~/econservation/eConservation_March2016")

###############   Needed Packages  ####################
#######################################################
library(plyr)
library(spatstat)
library(rgdal)
library(maptools)
library(sp)

################  Needed Functions  ###################
#######################################################
## Function to count how the number of NA in one variavble
count_NA <- function(x) sum(is.na(x))

## Function to count how the number of NA in each column of a data frame 
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)), 
      n=length(x), 
      propmiss=sum(is.na(x))/length(x) 
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}

## Function to convert capital letter text in lower case except the first letter
r_ucfirst <- function (str) {
  paste(toupper(substring(str, 1, 1)), tolower(substring(str, 2)), sep = "")
}

## Function to convert capital letter text in lower case except the first letter of each word
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

## Funtion to remove leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Funtion to remove all extra leading and trailing whitespace
trim_blank  <-  function (x) gsub("^([ \t\n\r\f\v]+)|([ \t\n\r\f\v]+)$", "", x)

## Funtion to remove leading and trailing punctuation
trim_punct <- function (x) gsub("([,;])$", "", x)
#######################################################
#######################################################

#######################################################
# I. Access, filter and clean the World Bank data

# I.1. Load the data from the website
wb_web <- read.csv("http://search.worldbank.org/api/projects/all.csv",  header=T, sep=",", quote = "", na.strings = "NA", colClasses="character")

# I.2. Filter the data
## I.2.1. Subset only projects about Biodiversity
wb_web_biodiv <- wb_web[with(wb_web, grepl("Biodiversity",theme1)|grepl("Biodiversity", theme2)|grepl("Biodiversity", theme3)|
                               grepl("Biodiversity", theme4)|grepl("Biodiversity", theme5)|grepl("Biodiversity", theme)),]
## I.2.2. Remove projects that have been dropped or are still in the pipeline
wb_web_biodiv <- subset(wb_web_biodiv, wb_web_biodiv$status %in% c("Active", "Closed"))
## I.2.3. Extract only the projects starting in 2010-2015
# wb_web_biodiv_10_15 <- subset(wb_web_biodiv, wb_web_biodiv$project_start_date>="2010-01-01")
wb_web_biodiv_10_15 <- subset(wb_web_biodiv, wb_web_biodiv$project_start_date>="2010-01-01" & wb_web_biodiv$project_start_date<="2016-02-01")

# I.3. Clean the data
## I.3.1. Remove unnecessary variables
wb_web_biodiv_10_15 <- wb_web_biodiv_10_15[,-c(4:11, 14, 17:20, 24:30, 32:36, 44:50, 51,57)]
## I.3.2. Convert empty fields to NA
wb_web_biodiv_10_15[wb_web_biodiv_10_15==''] <- NA
## I.3.3. Aggregate the theme variables into one
wb_web_biodiv_10_15$theme <- paste(wb_web_biodiv_10_15$theme1, wb_web_biodiv_10_15$theme2, wb_web_biodiv_10_15$theme3, wb_web_biodiv_10_15$theme4, wb_web_biodiv_10_15$theme5, sep=";")
wb_web_biodiv_10_15$theme1 <- NULL
wb_web_biodiv_10_15$theme2 <- NULL 
wb_web_biodiv_10_15$theme3 <- NULL
wb_web_biodiv_10_15$theme4 <- NULL
wb_web_biodiv_10_15$theme5 <- NULL
## I.3.4. Rename variables
wb_web_biodiv_10_15 <- rename(wb_web_biodiv_10_15, replace=c("id"="id_proj_from_provider",
                                                 "project_name"="title",
                                                 "boardapprovaldate"="project_start_date",
                                                 "closingdate"="project_end_date",
                                                 "lendprojectcost"="budget",
                                                 "url"="proj_link"))
## I.3.5. Convert date variables to date format
wb_web_biodiv_10_15$project_start_date <- gsub("T00:00:00Z", "", wb_web_biodiv_10_15$project_start_date)
wb_web_biodiv_10_15$project_end_date <- gsub("T00:00:00Z", "", wb_web_biodiv_10_15$project_end_date)  
wb_web_biodiv_10_15$project_start_date <- as.Date(wb_web_biodiv_10_15$project_start_date, "%Y-%m-%d")
wb_web_biodiv_10_15$project_end_date <- as.Date(wb_web_biodiv_10_15$project_end_date, "%Y-%m-%d")  
## I.3.6. Remove semicolon in the budget variable
wb_web_biodiv_10_15$budget <- as.numeric(gsub(";", "", wb_web_biodiv_10_15$budget))
## I.3.7. Within some variables, the information if duplicated. Keep only one 
wb_web_biodiv_10_15$countryname <- as.character(wb_web_biodiv_10_15$countryname)
s <- (strsplit(wb_web_biodiv_10_15$countryname, split = ";"))
temp <- data.frame(id_proj_from_provider = rep(wb_web_biodiv_10_15$id_proj_from_provider, sapply(s, length)), countryname = unlist(s))
temp <- temp[!duplicated(temp),]
wb_web_biodiv_10_15$countryname <- NULL
wb_web_biodiv_10_15 <- merge(wb_web_biodiv_10_15, temp, by="id_proj_from_provider", all=T)
## I.3.8. Clean up the project titles
wb_web_biodiv_10_15$title <- gsub("Proejct", "Project", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("%th", "Fifth", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("PIAU&#205;", "PIAUÍ", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("CO Mainstreaming Sust. Cattle Ranching Project AF", "Colombia - Additional Financing for the Mainstreaming Sustainable Cattle Ranching Project", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title[c(41,59,86,113,115,122)] <- r_ucfirst(wb_web_biodiv_10_15$title[c(41,59,86,113,115,122)])
wb_web_biodiv_10_15$title <- gsub("piauí", "Piauí", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title[122] <- gsub("mali", "Mali", wb_web_biodiv_10_15$title[122])
wb_web_biodiv_10_15$title <- gsub("Devt", "Development", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("AF", "Additional Financing", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("  ", " ", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("Svcs", "Services", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub(";", ",", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- trim_blank(wb_web_biodiv_10_15$title) # has to be done before trim_punct
wb_web_biodiv_10_15$title <- trim_punct(wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("\\|", " - ", wb_web_biodiv_10_15$title) # replace all pipes in the titles so pipes can be used as a field separator
wb_web_biodiv_10_15$title <- gsub("[\r\n]", "", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("\"", "", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("[\t]", "", wb_web_biodiv_10_15$title)
wb_web_biodiv_10_15$title <- gsub("N/A", NA, wb_web_biodiv_10_15$title)

# I.4. Add the missing variable to resect the eConservation database structure
## I.4.1. Add the update date, the provider and the user name
wb_web_biodiv_10_15$provider <- "World Bank"
wb_web_biodiv_10_15$puser <- "BdB,OB"
wb_web_biodiv_10_15$currency <- "USD"
wb_web_biodiv_10_15$update_date <- "2016-02-02"
## I.4.2. Create a unique numerical ID for the projects
wb_web_biodiv_10_15$id_proj_from_postgres <- seq(1000000, 1000000-1+nrow(wb_web_biodiv_10_15)) 
## I.4.3. Convert date variables to date format
wb_web_biodiv_10_15$update_date <- as.Date(wb_web_biodiv_10_15$update_date, "%Y-%m-%d")  

# I.5. Check for missing values
missing_wb_web_biodiv_10_15 <- propmiss(wb_web_biodiv_10_15)

# I.6. Complete the missing data
## I.6.2. Complete the missing dates
dates <- subset(wb_web_biodiv_10_15, select=c("id_proj_from_provider" ,"project_start_date","project_end_date"))
propmiss(dates) # Check for missing values
# Date found in various parts of the online documents on the World Bank project page. 
# When the missing dates were project end dates for additional financing for an existing project, the initial project end date was used if not clearly stated otherwise in the additional financing documentation.
# When the information could not be found, the project start date was used
dates_missing <- dates[is.na(dates$project_end_date),]
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P112106"] <- as.Date("2014-10-18", "%Y-%m-%d")   
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P116734"] <- as.Date("2013-06-30", "%Y-%m-%d")   
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P119725"] <- as.Date("2012-05-01", "%Y-%m-%d")  
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P120039"] <- as.Date(wb_web_biodiv_10_15$project_start_date[wb_web_biodiv_10_15$id_proj_from_provider=="P120039"], "%Y-%m-%d") # No date nor documentation available. The project start date was put as the end date to avoid NA
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P126542"] <- as.Date("2015-03-31", "%Y-%m-%d")  
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P128392"] <- as.Date("2021-06-30", "%Y-%m-%d")  # The date in the documents was 2021-06-31 which does not exist
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P132100"] <- as.Date("2016-02-15", "%Y-%m-%d")
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P144183"] <- as.Date("2021-01-31", "%Y-%m-%d") 
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P144902"] <- as.Date("2015-02-28", "%Y-%m-%d")  
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P145732"] <- as.Date("2017-08-23", "%Y-%m-%d")  
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P152066"] <- as.Date("2021-06-30", "%Y-%m-%d")    
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P153721"] <- as.Date("2021-09-30", "%Y-%m-%d") # But documentation under P151102
wb_web_biodiv_10_15$project_end_date[wb_web_biodiv_10_15$id_proj_from_provider=="P153958"] <- as.Date("2015-06-30", "%Y-%m-%d")  

# I.7. Save the table as csv
write.table(wb_web_biodiv_10_15, paste(getwd(), "/eConservation_WB_2010_2015/Main_tables/data_all_projects_wb_10_15.csv",
            row.names=FALSE, sep="|", fileEncoding = "latin1", na = "")




#######################################################
# II. Create the PROJECT table
wb_web_biodiv_10_15_projects <- subset(wb_web_biodiv_10_15, select=c("id_proj_from_provider", "title", 
                                                                     "project_start_date","project_end_date",
                                                                     "proj_link", "budget", "currency",
                                                                     "update_date" , "puser","id_proj_from_postgres"))
wb_web_biodiv_10_15_projects$proj_summary <- NA
wb_web_biodiv_10_15_projects$description <- NA
wb_web_biodiv_10_15_projects$pcomments <- NA
## Link the projects to the existing ID in Bowy's database
codes_wb <- read.csv("E:/bicheor/econservation/Database_2014/proj_codes/proj_codes_worldbank.csv", header=T)
wb_web_biodiv_10_15_projects <- merge(wb_web_biodiv_10_15_projects, codes_wb, by="id_proj_from_provider", all.x=T, all.y=F)
wb_web_biodiv_10_15_projects$proj_title <- NULL
## Save the project table
write.table(wb_web_biodiv_10_15_projects, "E:/bicheor/Working/New_data_structure/R_database/Main_tables/WB_2010_2015/wb_web_biodiv_10_15_projects.csv", 
            row.names=FALSE, sep="|", fileEncoding = "latin1", na = "")










# Clean up of implementing agencies names 
df_impl_agency <- subset(wb_web_biodiv_10_15, select=c("id_proj_from_provider" ,"borrower","impl_agency"))
## More individual and in depth cleaning needs to be done in excel
write.csv(df_impl_agency, "E:/bicheor/Working/New_data_structure/Temp/df_impl_agency.csv", row.names = F)
df_impl_agency <- read.csv("E:/bicheor/Working/New_data_structure/Temp/df_impl_agency.csv", na.strings = NA,
                           header=T, sep=",", encoding="latin1") 
## Include it back to the data
wb_web_biodiv_10_15 <- merge(wb_web_biodiv_10_15, df_impl_agency, by="id_proj_from_provider", all.x=T)

## Decompose impl agencies as there is only one row per projects but it includes multiple sites --> lookup table
df_impl_agency$impl_agency <- as.character(df_impl_agency$impl_agency)
s <- (strsplit(as.character(df_impl_agency$impl_agency), split = ";"))
wb_web_biodiv_lt_proj_agency <- data.frame(id_proj_from_provider = rep(df_impl_agency$id_proj_from_provider, sapply(s, length)), impl_agency = unlist(s))
wb_web_biodiv_lt_proj_agency$impl_agency <- trim_blank(wb_web_biodiv_lt_proj_agency$impl_agency)
## Extract the unique implementing agencies from the lookup table
wb_web_biodiv_implementing_agency <- data.frame(impl_agency = unique(wb_web_biodiv_lt_proj_agency$impl_agency))
## Complete the implementing agencies info in excel
write.csv(wb_web_biodiv_implementing_agency, "E:/bicheor/Working/New_data_structure/Temp/wb_web_biodiv_implementing_agency.csv", row.names = F)
wb_web_biodiv_implementing_agency <- read.csv("E:/bicheor/Working/New_data_structure/Temp/wb_web_biodiv_implementing_agency.csv", na.strings = NA,
                                              header=T, sep=",", encoding="latin1")

## Add the new implementing agencies to the existing IMPLEMENTING AGENCIES table
wb_web_biodiv_implementing_agency$impl_agency <- trim_blank(wb_web_biodiv_implementing_agency$impl_agency)
same_implementing_agency <- merge(wb_web_biodiv_implementing_agency, implementing_agencies[,c(1:3,6)], by="impl_agency", all=F) # extract the WB agencies that are already in the IMPLEMENTING AGENCIES table
old_implementing_agency <- subset(implementing_agencies, !implementing_agencies$id_impl_agency %in% same_implementing_agency$id_impl_agency) # remove these WB agencies from the IMPLEMENTING AGENCIES table
new_implementing_agency <- merge(wb_web_biodiv_implementing_agency, implementing_agencies[,c(1:3,6)], by="impl_agency", all.x=T) # extract the WB agencies that are NOT already in the IMPLEMENTING AGENCIES table
new_implementing_agency <- subset(new_implementing_agency, !new_implementing_agency$id_impl_agency %in% same_implementing_agency$id_impl_agency)
new_implementing_agency$id_impl_agency <- seq(max(implementing_agencies$id_impl_agency)+1, max(implementing_agencies$id_impl_agency)+nrow(new_implementing_agency)) # extract the WB agencies
implementing_agencies <- rbind.fill(old_implementing_agency, same_implementing_agency)
implementing_agencies <- rbind.fill(implementing_agencies, new_implementing_agency)
implementing_agencies <- implementing_agencies[!duplicated(implementing_agencies),]

## Clean the implementing agencies
implementing_agencies <- rename(implementing_agencies, replace=c("comments"="acomments"))
implementing_agencies$impl_agency <- gsub("\\|", " - ", implementing_agencies$impl_agency)
implementing_agencies$acomments <- gsub("\\|", " - ", implementing_agencies$acomments)
implementing_agencies$ngo_link <- gsub("\\|", " - ", implementing_agencies$ngo_link)
implementing_agencies$impl_agency <- gsub("[\r\n]", "", implementing_agencies$impl_agency)
implementing_agencies$acomments <- gsub("[\r\n]", "", implementing_agencies$acomments)
implementing_agencies$ngo_link <- gsub("[\r\n]", " - ", implementing_agencies$ngo_link)
implementing_agencies$impl_agency <- gsub("\"", "", implementing_agencies$impl_agency)
implementing_agencies$acomments <- gsub("\"", "", implementing_agencies$acomments)
implementing_agencies$ngo_link <- gsub("\"", " - ", implementing_agencies$ngo_link)
implementing_agencies$impl_agency <- gsub("[\t]", "", implementing_agencies$impl_agency)
implementing_agencies$acomments <- gsub("[\t]", "", implementing_agencies$acomments)
implementing_agencies$ngo_link <- gsub("[\t]", " - ", implementing_agencies$ngo_link)
implementing_agencies$impl_agency <- gsub("N/A", NA, implementing_agencies$impl_agency)
implementing_agencies$acomments <- gsub("N/A", NA, implementing_agencies$acomments)
implementing_agencies$ngo_link <- gsub("N/A", NA, implementing_agencies$ngo_link)
implementing_agencies$impl_agency <- trim_blank(implementing_agencies$impl_agency)
write.table(implementing_agencies, "E:/bicheor/Working/New_data_structure/R_database/Main_tables/WB_2010_2015/implementing_agencies.csv", 
            row.names=FALSE, sep="|", fileEncoding = "latin1", na = "")

## Comple the lookup table with the new implementing agencies id 
wb_web_biodiv_lt_proj_agency <- merge(wb_web_biodiv_lt_proj_agency, implementing_agencies, by="impl_agency", all.x=T, all.y=F)
table(is.na(wb_web_biodiv_lt_proj_agency$id_impl_agency)) # All implementing agencies are associated to an id
wb_web_biodiv_lt_proj_agency <- subset(wb_web_biodiv_lt_proj_agency, select=c("id_proj_from_provider", "id_impl_agency"))
wb_web_biodiv_lt_proj_agency <- merge(wb_web_biodiv_lt_proj_agency, 
                                      subset(wb_web_biodiv_10_15_projects, select=c(id_proj_from_provider, id_proj_from_postgres)), 
                                      by="id_proj_from_provider", all.x=T, all.y=F)
table(is.na(wb_web_biodiv_lt_proj_agency))
write.table(wb_web_biodiv_lt_proj_agency, "E:/bicheor/Working/New_data_structure/R_database/Lookup_tables/WB_2010_2015/wb_web_biodiv_lt_proj_agency.csv", 
            row.names=FALSE, sep="|", fileEncoding = "UTF-8", na = "")














# Clean up of sites
wb_web_biodiv_sites <- subset(wb_web_biodiv_10_15, select=c("id_proj_from_provider", "id_proj_from_postgres","GeoLocID","GeoLocName","Latitude","Longitude"))
propmiss(wb_web_biodiv_sites) # Check for missing values
## Check if projects without site coordinates had been georeferenced in Bowy's database
sites_missing <- wb_web_biodiv_sites[is.na(wb_web_biodiv_sites$Latitude),]
is_site <- merge(sites_missing, lt_proj_sites, by="id_proj_from_provider", all.x=T, all.y=F)
propmiss(is_site) # Check for missing values # 1 project had been associated to sites. 37 NEED TO BE GEOREFERENCED
## The georeferencing was undertaken following the procedure described by Bowy. It was done in excel:
write.csv(sites_missing, "E:/bicheor/Working/New_data_structure/Temp/sites_missing.csv", row.names = F)
lt_proj_sites_missing_wb <- read.csv("E:/bicheor/Working/New_data_structure/Temp/sites_missing.csv", 
                                     na.strings = NA, header=T, sep=",", dec=".", encoding="latin1")
lt_proj_sites_missing_wb$site_name <- trim_blank(lt_proj_sites_missing_wb$site_name)
lt_proj_sites_missing_wb$geonames <- trim_blank(lt_proj_sites_missing_wb$geonames)

## Decompose sites as there is only one row per projects but it includes multiple sites --> lookup table
### The latitude, longitude are in equal number when decomposed. But GeoLocID and GeoLocName have differing length so could not be included.
### The information about the site, other that the coordinates, will need to be handled by hand
sites_non_missing <- subset(wb_web_biodiv_sites, !wb_web_biodiv_sites$id_proj_from_provider %in% sites_missing$id_proj_from_provider) # remove the projects without site
s3 <- (strsplit(as.character(sites_non_missing$Latitude), split = ";"))
s4 <- (strsplit(as.character(sites_non_missing$Longitude), split = ";"))
wb_web_biodiv_lt_proj_sites_coord <- data.frame(id_proj_from_provider = rep(sites_non_missing$id_proj_from_provider, sapply(s3, length)),
                                          latitude = unlist(s3),
                                          longitude = unlist(s4))
wb_web_biodiv_lt_proj_sites_coord <- wb_web_biodiv_lt_proj_sites_coord[!duplicated(wb_web_biodiv_lt_proj_sites_coord),]
wb_web_biodiv_lt_proj_sites_coord$latitude <- as.numeric(as.character(wb_web_biodiv_lt_proj_sites_coord$latitude))
wb_web_biodiv_lt_proj_sites_coord$longitude <- as.numeric(as.character(wb_web_biodiv_lt_proj_sites_coord$longitude))
### Remove sites with coordinate error
wb_web_biodiv_lt_proj_sites_coord <- subset(wb_web_biodiv_lt_proj_sites_coord, wb_web_biodiv_lt_proj_sites_coord$latitude < 90)
wb_web_biodiv_lt_proj_sites_coord <- subset(wb_web_biodiv_lt_proj_sites_coord, wb_web_biodiv_lt_proj_sites_coord$latitude > -90)
wb_web_biodiv_lt_proj_sites_coord <- subset(wb_web_biodiv_lt_proj_sites_coord, wb_web_biodiv_lt_proj_sites_coord$longitude < 180)
wb_web_biodiv_lt_proj_sites_coord <- subset(wb_web_biodiv_lt_proj_sites_coord, wb_web_biodiv_lt_proj_sites_coord$longitude > -180)

## Add the lookup table proj-sites extracted from the web database to the lookup table proj-sites georeferenced by hand 
wb_web_biodiv_lt_proj_sites <- rbind.fill(wb_web_biodiv_lt_proj_sites_coord, lt_proj_sites_missing_wb)
wb_web_biodiv_lt_proj_sites <- wb_web_biodiv_lt_proj_sites[!duplicated(wb_web_biodiv_lt_proj_sites),]

## Extract the unique sites from the lookup table
wb_web_biodiv_sites <- subset(wb_web_biodiv_lt_proj_sites, select=-id_proj_from_provider)
wb_web_biodiv_sites <- wb_web_biodiv_sites[!duplicated(wb_web_biodiv_sites),]
wb_web_biodiv_sites <- wb_web_biodiv_sites[order(wb_web_biodiv_sites$precision_id, decreasing=TRUE),]
wb_web_biodiv_sites_wNA <- wb_web_biodiv_sites[is.na(wb_web_biodiv_sites$latitude),]
wb_web_biodiv_sites_woNA <- wb_web_biodiv_sites[!is.na(wb_web_biodiv_sites$latitude),]
wb_web_biodiv_sites_woNA <- wb_web_biodiv_sites_woNA[!duplicated(wb_web_biodiv_sites_woNA[,c('latitude', 'longitude')]),]
wb_web_biodiv_sites <- rbind(wb_web_biodiv_sites_woNA, wb_web_biodiv_sites_wNA)
wb_web_biodiv_sites$id_site_from_postgres <- seq(max(sites$id_site_from_postgres)+1, max(sites$id_site_from_postgres)+nrow(wb_web_biodiv_sites)) # extract the WB agencies
write.table(wb_web_biodiv_sites, "E:/bicheor/Working/New_data_structure/R_database/Main_tables/WB_2010_2015/wb_web_biodiv_sites.csv", 
            row.names=FALSE, sep="|", fileEncoding = "latin1", na = "")

# ### Trial to merge the wb site to the existing SITES table by coordinates
# str(wb_web_biodiv_sites)
# str(sites)
# test <- merge(wb_web_biodiv_sites, sites, by=c("latitude", "longitude"), all.x=T, all.y=F)
# ## Merging the wb site to the existing SITES table by coordinates does not give any results (maybe the coordinates do not have the same number of decimals)

## Associate sites with the wdpa id when applicable
#####################  SEE GetSiteWDPA.R

## Add a precision code of "2" to sites for which coordinates were provided by the World Bank
wb_web_biodiv_sites_POINTS <- readShapePoints("E:/bicheor/Working/New_data_structure/R_database/GIS/WB_2010_2015/wb_web_biodiv_sites_POINTS_MinDistWDPA.shp")
wb_web_biodiv_sites <- wb_web_biodiv_sites_POINTS@data
wb_web_biodiv_sites <- rename(wb_web_biodiv_sites, replace=c("precision_"="precision_id", "wdpa_id"="inter_wdpaPOLY",
                                                             "link_to_si"="link_to_site", "id_site_fr"="id_site_from_postgres"))
wb_web_biodiv_sites$precision_id <- ifelse(is.na(wb_web_biodiv_sites$precision_id), 2, wb_web_biodiv_sites$precision_id)
wb_web_biodiv_sites$coords_x1 <- NULL
wb_web_biodiv_sites$coords_x2 <- NULL

write.table(wb_web_biodiv_sites, "E:/bicheor/Working/New_data_structure/R_database/Main_tables/WB_2010_2015/wb_web_biodiv_sites.csv", 
            row.names=FALSE, sep="|", fileEncoding = "UTF-8", na = "")


## Complete the lookup table with the new sites id 
wb_web_biodiv_lt_proj_sites <- wb_web_biodiv_lt_proj_sites[,1:3]
wb_web_biodiv_lt_proj_sites <- merge(wb_web_biodiv_lt_proj_sites, 
                                     subset(wb_web_biodiv_sites, select=c(latitude, longitude, id_site_from_postgres)), 
                                     by=c("latitude","longitude"), all.x=T, all.y=F)
wb_web_biodiv_lt_proj_sites <- wb_web_biodiv_lt_proj_sites[!is.na(wb_web_biodiv_lt_proj_sites$id_site_from_postgres),]
wb_web_biodiv_lt_proj_sites <- subset(wb_web_biodiv_lt_proj_sites, select=c("id_proj_from_provider", "id_site_from_postgres"))
wb_web_biodiv_lt_proj_sites <- wb_web_biodiv_lt_proj_sites[!duplicated(wb_web_biodiv_lt_proj_sites),]
codes_wb_sites <- subset(wb_web_biodiv_10_15, select=c("id_proj_from_provider", "id_proj_from_postgres"))
wb_web_biodiv_lt_proj_sites <- merge(wb_web_biodiv_lt_proj_sites, codes_wb_sites, by="id_proj_from_provider", all.x=T, all.y=F)
table(is.na(wb_web_biodiv_lt_proj_sites$id_proj_from_postgres))

## Link the projects to the existing ID in Bowy's database
codes_wb <- read.csv("E:/bicheor/econservation/Database_2014/proj_codes/proj_codes_worldbank.csv", header=T)
wb_web_biodiv_lt_proj_sites <- merge(wb_web_biodiv_lt_proj_sites, codes_wb[,c(1,3)], by="id_proj_from_provider", all.x=T, all.y=F)
## Save the proj-site lookup table
write.table(wb_web_biodiv_lt_proj_sites, "E:/bicheor/Working/New_data_structure/R_database/Lookup_tables/WB_2010_2015/wb_web_biodiv_lt_proj_sites.csv", row.names=FALSE, sep="|", fileEncoding = "UTF-8", na = "")


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





## Create the site-WDPA(polygons) lookup table 
wb_web_biodiv_lt_sites_wdpa <- subset(wb_web_biodiv_sites, select=c(id_site_from_postgres, WDPAID))

s5 <- (strsplit(as.character(wb_web_biodiv_lt_sites_wdpa$WDPAID), split = ","))
wb_web_biodiv_lt_sites_wdpa <- data.frame(id_site_from_postgres = rep(wb_web_biodiv_lt_sites_wdpa$id_site_from_postgres, sapply(s5, length)),
                                          WDPAID = unlist(s5))
wb_web_biodiv_lt_sites_wdpa <- wb_web_biodiv_lt_sites_wdpa[!duplicated(wb_web_biodiv_lt_sites_wdpa),]
wb_web_biodiv_lt_sites_wdpa$WDPAID <- as.numeric(as.character(wb_web_biodiv_lt_sites_wdpa$WDPAID))
write.table(wb_web_biodiv_lt_sites_wdpa, "E:/bicheor/Working/New_data_structure/R_database/Lookup_tables/WB_2010_2015/wb_web_biodiv_lt_sites_wdpa.csv", row.names=FALSE, sep="|", fileEncoding = "UTF-8", na = "")









# Find donors data (not specified in the web database)
## Check if projects were associated to a donor in Bowy's database.
is_donor <- merge(wb_web_biodiv_10_15, lt_proj_donors, by="id_proj_from_provider", all.x=T, all.y=F)
propmiss(is_donor) # Check for missing values
is_donor <- merge(is_donor, donors, by="id_donor", all.x=T, all.y=F) 
# Project that were associated with donor were all given the World Bank as donor which is not correct.
# The table was exported into excel and completed by hand by searching online documentation of projects on the WB website
write.csv(is_donor, "E:/bicheor/Working/New_data_structure/Temp/is_donor.csv", row.names = F) # Export the project table
wb_web_biodiv_donors <- read.csv("E:/bicheor/Working/New_data_structure/Temp/is_donor.csv",
                                 na.strings = NA, header=T, sep=",", dec=".", encoding="latin1") # Import the completed table
wb_web_biodiv_donors <- wb_web_biodiv_donors[,c(1,3)]

## Create the lookup table project-donors
s <- (strsplit(as.character(wb_web_biodiv_donors$donor_name), split = ";"))
wb_web_biodiv_lt_proj_donors <- data.frame(id_proj_from_provider = rep(wb_web_biodiv_donors$id_proj_from_provider, sapply(s, length)),
                                           donor_name = unlist(s))
### Link it to the DONORS table
wb_web_biodiv_lt_proj_donors <- merge(wb_web_biodiv_lt_proj_donors, donors, by="donor_name", all.x=T, all.y=F)
### Extract the new donors and add them to the DONOR table
new_donors <- wb_web_biodiv_lt_proj_donors[is.na(wb_web_biodiv_lt_proj_donors$id_donor),]
new_donors <- subset(new_donors, select=-id_proj_from_provider)
new_donors <- new_donors[!duplicated(new_donors),]
new_donors$id_donor <- seq(max(donors$id_donor)+1, max(donors$id_donor)+nrow(new_donors)) 
donors <- rbind.fill(donors, new_donors)

#### The table was then exported into excel and completed by hand by searching information online
write.csv(donors, "E:/bicheor/Working/New_data_structure/Temp/donors_to_complete.csv", row.names = F)
donors <- read.csv("E:/bicheor/Working/New_data_structure/Temp/donors_to_complete.csv",
                                 na.strings = NA, header=T, sep=",", dec=".", encoding="latin1") # Import the completed table

## Clean donors for postgres
donors$donor_name <- gsub("\\|", " - ", donors$donor_name)
donors$address <- gsub("\\|", " - ", donors$address)
donors$Description <- gsub("\\|", " - ", donors$Description)
donors$donor_name <- gsub("[\r\n]", "", donors$donor_name)
donors$address <- gsub("[\r\n]", "", donors$address)
donors$Description <- gsub("[\r\n]", "", donors$Description)
donors$donor_name <- gsub("\"", "", donors$donor_name)
donors$address <- gsub("\"", "", donors$address)
donors$Description <- gsub("\"", "", donors$Description)
donors$donor_name <- gsub("[\t]", "", donors$donor_name)
donors$address <- gsub("[\t]", "", donors$address)
donors$Description <- gsub("[\t]", "", donors$Description)
donors$donor_name <- gsub("N/A", NA, donors$donor_name)
donors$address <- gsub("N/A", NA, donors$address)
donors$Description <- gsub("N/A", NA, donors$Description)
donors$date_oldest_project <- NA
donors$date_latest_project <- NA

write.table(donors, "E:/bicheor/Working/New_data_structure/R_database/Main_tables/WB_2010_2015/donors.csv", 
            row.names=FALSE, sep="|", fileEncoding = "latin1", na = "")


## Comple the lookup table with the new donors id 
wb_web_biodiv_lt_proj_donors <- wb_web_biodiv_lt_proj_donors[,1:3]
wb_web_biodiv_lt_proj_donors <- merge(wb_web_biodiv_lt_proj_donors, donors, by="donor_name", all.x=T, all.y=F)
wb_web_biodiv_lt_proj_donors <- subset(wb_web_biodiv_lt_proj_donors, select=c("id_proj_from_provider", "id_donor.y"))
wb_web_biodiv_lt_proj_donors <- rename(wb_web_biodiv_lt_proj_donors, replace=c("id_donor.y"="id_donor"))
wb_web_biodiv_lt_proj_donors <- merge(wb_web_biodiv_lt_proj_donors, 
                                      subset(wb_web_biodiv_10_15_projects, select=c(id_proj_from_provider, id_proj_from_postgres)), 
                                      by="id_proj_from_provider", all.x=T, all.y=F)
table(is.na(wb_web_biodiv_lt_proj_donors))
write.table(wb_web_biodiv_lt_proj_donors, "E:/bicheor/Working/New_data_structure/R_database/Lookup_tables/WB_2010_2015/wb_web_biodiv_lt_proj_donors.csv", 
            row.names=FALSE, sep="|", fileEncoding = "UTF-8", na = "")










# Create the project - provider lookup table (id World Bank as a data provider is 10)
wb_web_biodiv_lt_proj_provider <- subset(wb_web_biodiv_10_15, select=c(id_proj_from_provider, id_proj_from_postgres))
wb_web_biodiv_lt_proj_provider$id_provider <- 10
wb_web_biodiv_lt_proj_provider <- wb_web_biodiv_lt_proj_provider[!duplicated(wb_web_biodiv_lt_proj_provider),]
write.table(wb_web_biodiv_lt_proj_provider, "E:/bicheor/Working/New_data_structure/R_database/Lookup_tables/WB_2010_2015/wb_web_biodiv_lt_proj_provider.csv", 
            row.names=FALSE, sep="|", fileEncoding = "UTF-8", na = "")


providers$Physical.address <- gsub("\\|", " - ", providers$Physical.address)
providers$Description <- gsub("\\|", " - ", providers$Description)
providers$Physical.address <- gsub("[\r\n]", "", providers$Physical.address)
providers$Description <- gsub("[\r\n]", "", providers$Description)
providers$Physical.address <- gsub("\"", "", providers$Physical.address)
providers$Description <- gsub("\"", "", providers$Description)
providers$Physical.address <- gsub("[\t]", "", providers$Physical.address)
providers$Description <- gsub("[\t]", "", providers$Description)
providers$Physical.address <- gsub("N/A", NA, providers$Physical.address)
providers$Description <- gsub("N/A", NA, providers$Description)
providers$Projects.recorded.by.the.data.provider <- NULL

providers <- rename(providers, replace=c(
  "provider" ="provider_name_en",
  "Link.to.data.provider.website" ="provider_link",
  "Data.provider.type" ="provider_type",
  "Country" ="country_origin",
  "Physical.address" ="address",
  "Phone.number" ="phone",
  "General.email.address" ="email",
  "Contact.person.1" ="Contact1",
  "email.address.of.Contact.person.1"="emailContact1",
  "Contact.person.2" ="Contact2",
  "email.address.of.Contact.person.2"="emailContact2",
  "Contact.person.3" ="Contact3",
  "email.address.of.Contact.person.3"="emailContact3",
  "Contact.person.4" ="Contact4",
  "email.address.of.Contact.person.4"="emailContact4",
  "Last.update" ="update_date",
  "Time.spend" ="time_span",
  "Quality.completeness.of.the.data" ="quality"
))

write.table(providers, "E:/bicheor/Working/New_data_structure/R_database/Main_tables/WB_2010_2015/providers.csv", 
            row.names=FALSE, sep="|", fileEncoding = "latin1", na = "")









  

# Find target species data (not specified in the web database)
## Create target species variables
wb_web_biodiv_10_15$species_name <- NA
wb_web_biodiv_10_15$iucn_species_id <- NA
wb_web_biodiv_10_15$scientific_name <- NA

## Complete with the species info.
## From a visual scanning of the projects titles, only one had information about a specific species
wb_web_biodiv_10_15$species_name[wb_web_biodiv_10_15$id_proj_from_provider=="P113860"] <- "Tiger"
wb_web_biodiv_10_15$scientific_name[wb_web_biodiv_10_15$id_proj_from_provider=="P113860"] <- "Panthera tigris"
wb_web_biodiv_10_15$iucn_species_id[wb_web_biodiv_10_15$id_proj_from_provider=="P113860"] <- 15955
write.table(wb_web_biodiv_10_15, "E:/bicheor/Working/New_data_structure/R_database/Main_tables/WB_2010_2015/data_all_projects_wb_10_15.csv", 
            row.names=FALSE, sep="|", fileEncoding = "latin1", na = "")

# Create the project - species lookup table 
wb_web_biodiv_lt_proj_species <- subset(wb_web_biodiv_10_15, select=c(id_proj_from_provider, id_proj_from_postgres, iucn_species_id))
wb_web_biodiv_lt_proj_species <- wb_web_biodiv_lt_proj_species[!duplicated(wb_web_biodiv_lt_proj_species),]
write.table(wb_web_biodiv_lt_proj_species, "E:/bicheor/Working/New_data_structure/R_database/Lookup_tables/WB_2010_2015/wb_web_biodiv_lt_proj_species.csv", 
            row.names=FALSE, sep="|", fileEncoding = "UTF-8", na = "")










# Find IUCN category data (not specified in the web database)
## The categorisation was made by scanning the variables susceptible to give information in the following order: title, sector, mjsector, theme
wb_web_biodiv_theme <- subset(wb_web_biodiv_10_15, select=c(id_proj_from_provider, id_proj_from_postgres, title, sector, mjsector, theme))
write.table(wb_web_biodiv_theme, "E:/bicheor/Working/New_data_structure/Temp/wb_web_biodiv_theme.csv", row.names=FALSE, sep="|", fileEncoding = "UTF-8", na = "NA")








