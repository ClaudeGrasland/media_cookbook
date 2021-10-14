
# load local dictionnaries

tabfreq_spatial_entities_annoted <- read.delim("data_mediacloud/FRA_lmonde/tabfreq_spatial_entities_annoted_V2.csv",fileEncoding = "UTF-8")

tabfreq_spatial_entities_annoted <- tabfreq_spatial_entities_annoted[tabfreq_spatial_entities_annoted$freq>2,]

#tabfreq_spatial_entities_annoted <- tabfreq_spatial_entities_annoted[!is.na(tabfreq_spatial_entities_annoted$spatial_type),]

#tabfreq_spatial_entities_annoted

head(tabfreq_spatial_entities_annoted)


# 1) According to the size (type B)


unique(tabfreq_spatial_entities_annoted$spatial_type_B)

tabfreq_spatial_entities_annoted$spatial_type_B <- as.character(tabfreq_spatial_entities_annoted$spatial_type_B)


tabfreq_spatial_entities_annoted <- tabfreq_spatial_entities_annoted[!tabfreq_spatial_entities_annoted$spatial_type_B %in% c("","astronomic object"),]



tabfreq_spatial_entities_annoted$spatial_type_B <- ifelse(tabfreq_spatial_entities_annoted$spatial_type_B %in%  c("river","strait","mountain"),
                                                          "river_strait_mountain",tabfreq_spatial_entities_annoted$spatial_type_B)


tabfreq_spatial_entities_annoted$spatial_type_B <- ifelse(tabfreq_spatial_entities_annoted$spatial_type_B %in%  c("city_state"),
                                                          "country",tabfreq_spatial_entities_annoted$spatial_type_B)



# 2) regarder les fréquences pour chaque catégori

total_type_freq <- tabfreq_spatial_entities_annoted %>% group_by(spatial_type_B) %>% 
  summarise(nb_total = sum(freq) )

total_type_freq <- total_type_freq[order(total_type_freq$nb_total,decreasing = T),]


# ordonné relativement ? type

total_type_freq$region <- c("#d8b365","#d8b365","#5ab4ac","#5ab4ac",
                            "#d8b365","#d8b365","#5ab4ac","#d8b365","#d8b365",
                            "#5ab4ac","#5ab4ac","#5ab4ac","#5ab4ac","#5ab4ac")

ggplot() + geom_bar(data= total_type_freq, aes(reorder(spatial_type_B,-nb_total),
                                               nb_total), stat = 'identity', 
                    fill=total_type_freq$region)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  scale_y_log10() +
  xlab("Spatial types")+
  ylab("total quotations")

# size related

total_type_freq <- total_type_freq[!total_type_freq$spatial_type_B %in% c("adj_inhabitant","other","cardinal points"),]


total_type_freq$spatial_type_B

myfactor.ordered.byobjectsize <- c("world","ocean","world_region","international_cooperation_organisation",
                                   "sea","transnational_region","country","infra_national_region","river_strait_mountain",
                                   "city","local_place")
myfactor.ordered.byobjectsize <- data.frame(myfactor.ordered.byobjectsize)

names(myfactor.ordered.byobjectsize) <-c("spatial_type_B")

tab_select <- merge(myfactor.ordered.byobjectsize,total_type_freq,by="spatial_type_B")


total_type_freq$spatial_type_B<- factor(total_type_freq$spatial_type_B,
                                              levels = myfactor.ordered.byobjectsize$spatial_type_B)




ggplot(total_type_freq, aes(spatial_type_B,nb_total)) +                                   
  geom_bar(stat = "identity", fill=total_type_freq$region)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  scale_y_log10()

# wikidata

library(WikidataR)


adams_items <- find_item("Douglas Adams")
peerage_props <- find_property("peerage")


Europe_items <- find_item("Paris",lang="fr")
Europa_props <- find_property("Europe")

print("Q46",get_property)
get_property("Q46")


geo_entity_Europe <- get_geo_entity("Europe")

get_property("Q46")

get_item("Q46")

#Retrieve a specific item
adams_metadata <- get_item("42")

#Retrieve a specific property
object_is_child <- get_property("P40")

sf_locations <- get_geo_entity("Q46")


my_properties <- get_property("Q46")

str(my_properties)

my_properties[[1]]$descriptions$en

my_properties[[1]]$aliases$en


my_properties <- get_property("Q46")
my.local.area<- (my_properties[[1]]$claims$P2046)


my_properties <- get_property("Q90")
my.local.area<- (my_properties[[1]]$claims$P2046)


# coordinate location (P625) 

# coordinates of easternmost point (P1334)

# coordinates of northernmost point (P1332)

# coordinates of southernmost point (P1333)

# coordinates of westernmost point (P1335)


# first we have to create a list of selected entities

my.local.easternpoint<- (my_properties[[1]]$claims$P1332)

my.entities <- unique(tabfreq_spatial_entities_annoted$spatial_lemma)

# we have to suppress the compounds because the wi
my.entities <- gsub("[_\"]", " ", my.entities)

# first we have to identify the words which are not detectec by wikimedia

tab.wiki.detected <- data.frame()
tab.wiki.detected$labelorig <- as.character()
tab.wiki.detected$labelen <- as.character()


for (i in 1:length(my.entities)) {
  
  my.entity <- my.entities[i]

  labelorig <- my.entity
  
  # find item?
  
  find.item <- find_item(my.entity,language="fr")[1]
  not_detected <- ifelse(is.null(names(find.item[[1]])),1,0)
  
  # creation of a local df with wiki data
  
  local.tab.wiki.detected <- data.frame (labelorig,not_detected)
  
  # cbind of df
  
  tab.wiki.detected <- rbind(tab.wiki.detected,local.tab.wiki.detected)
  
  # print
  
  print(my.entities[i])
  
  }


#write.csv(tab.wiki.detected, file = "data_mediacloud/FRA_lmonde/tab.wiki.detected.csv",
#          fileEncoding = "UTF-8")

# item which are not detected in Wikipedia

not_detected_entities <- tab.wiki.detected[tab.wiki.detected$not_detected == 1,]

tab_detected_entities <- tab.wiki.detected[tab.wiki.detected$not_detected == 0,]

my.entities <- tab_detected_entities$labelorig

#my.entities <- my.entities[!my.entities %in% c("hongkong","venezuela","marseille","lyon","londres")]

# ambigous?

tab.wiki.ambigous <- data.frame()
tab.wiki.ambigous$labelorig <- as.character()
tab.wiki.ambigous$wikidata_item <- as.character()
tab.wiki.ambigous$homonymie <- as.character()

for (i in 1:length(my.entities)) {

  my.entity <- my.entities[i]
  
  my.local.item <- find_item(my.entity,language="fr")
  labelorig <- my.entity
  wikidata_item <- my.local.item[[1]]$id
  
  
  my_properties <- get_property(wikidata_item)
  labelen <- my_properties[[1]]$labels$en$value
  description <- my_properties[[1]]$descriptions$fr$value
  
  is.homonymie <- ifelse(is.null(description),"null", ifelse(description == "page d'homonymie d'un projet Wikimédia",
                                                             "homonymie",0))
    
  # creation of a local df with wiki data
  
  local.tab.wiki.ambigous <- data.frame (labelorig,wikidata_item,is.homonymie)
  
  # cbind of df
  
  tab.wiki.ambigous <- rbind(tab.wiki.ambigous,local.tab.wiki.ambigous) 
  
  # print
  
  print(my.entities[i])
  
  }

tab.wiki.ambigous[tab.wiki.ambigous$is.homonymie == "homonymie",]
tab.wiki.ambigous[tab.wiki.ambigous$is.homonymie == "null",]






#write.csv(tab.wiki.ambigous, file = "data_mediacloud/FRA_lmonde/tab.wiki.ambigous.csv",
#          fileEncoding = "UTF-8")

  

# Suppres data not detected or ambigous

tab.wiki.ambigous <- read.csv("data_mediacloud/FRA_lmonde/tab.wiki.ambigous.csv",
                              fileEncoding = "UTF-8")



select.non.ambigous <- tab.wiki.ambigous[tab.wiki.ambigous$is.homonymie == 0,]


my.entities <- select.non.ambigous$labelorig


# is there spatial information in the data?

tab.wiki.geom.isna <- data.frame()
tab.wiki.geom.isna$wikidata_item <- as.character()
tab.wiki.geom.isna$labelorig <- as.character()
tab.wiki.geom.isna$labelen <- as.character()
tab.wiki.geom.isna$description <- as.character()
tab.wiki.geom.isna$nesw.most.point.isna <- as.numeric()
tab.wiki.geom.isna$centroid.isna<- as.numeric()
tab.wiki.geom.isna$area.isna <- as.numeric()



for (i in 1:length(my.entities)) {
  
  my.entity <- my.entities[i]

  my.local.item <- find_item(my.entity,language="fr")
  labelorig <- my.entity
  wikidata_item <- my.local.item[[1]]$id
  
  my_properties <- get_property(wikidata_item)
  labelen <- ifelse(is.null(my_properties[[1]]$labels$en$value),"NONE",my_properties[[1]]$labels$en$value)
  description <- my_properties[[1]]$descriptions$fr$value
  
  
  # area
  tab.area<- (my_properties[[1]]$claims$P2046)
  area <- tab.area$mainsnak$datavalue$value$amount
  area.unit <- tab.area$mainsnak$datavalue$value$unit
  
  #centroid
  tab.centroid <- (my_properties[[1]]$claims$P625)
  centroid.latitude <- tab.centroid$mainsnak$datavalue$value$latitude
  centroid.longitude <- tab.centroid$mainsnak$datavalue$value$longitude
  
  #easter most point : for more information about that: https://www.wikidata.org/wiki/Property:P1332
  tab.easternmostpoint<- (my_properties[[1]]$claims$P1334)
  easternmostpoint.latitude <- tab.easternmostpoint$mainsnak$datavalue$value$latitude
  easternmostpoint.longitude <- tab.easternmostpoint$mainsnak$datavalue$value$longitude
  
  #northernmost most point 
  tab.northernmostpoint<- (my_properties[[1]]$claims$P1332)
  northernmostpoint.latitude <- tab.northernmostpoint$mainsnak$datavalue$value$latitude
  northernmostpoint.longitude <- tab.northernmostpoint$mainsnak$datavalue$value$longitude
  
  #western most point 
  tab.westernmostpoint<- (my_properties[[1]]$claims$P1335)
  westernmostpoint.latitude <- tab.westernmostpoint$mainsnak$datavalue$value$latitude
  westernmostpoint.longitude <- tab.westernmostpoint$mainsnak$datavalue$value$longitude
  
  
  #southern most point 
  tab.southernmostpoint<- (my_properties[[1]]$claims$P1333)
  southernmostpoint.latitude <- tab.southernmostpoint$mainsnak$datavalue$value$latitude
  southernmostpoint.longitude <- tab.southernmostpoint$mainsnak$datavalue$value$longitude
  

  
  # creation of a local df with wiki data
  
  
  centroid <-c(centroid.latitude,centroid.longitude)
  nsew.most.point <-c(easternmostpoint.latitude,
        easternmostpoint.longitude,northernmostpoint.latitude,
        northernmostpoint.longitude,westernmostpoint.latitude,
        westernmostpoint.longitude,southernmostpoint.latitude,
        southernmostpoint.longitude)
  
  nesw.most.point.isna <- ifelse(is.null(nsew.most.point),"null",
                                 ifelse(length(nsew.most.point)<8,"incomplete",
                                        ifelse(length(nsew.most.point)>8,"more","ok")))
                                        
  
  
  
  centroid.isna <- ifelse(unique(is.null(centroid)),1,0)
  local.area.isna <- ifelse(is.null(area),1,0)
  
  
  # creation of a local df with wiki data
  
  local.tab.wiki.geom.isna <- data.frame(wikidata_item,labelorig,labelen,description,
                                    nesw.most.point.isna,centroid.isna,local.area.isna)
  
  # cbind of df
  
  tab.wiki.geom.isna <- rbind(tab.wiki.geom.isna, local.tab.wiki.geom.isna)
  
  # print
  
  print(my.entities[i])
  
}

head(tab.wiki.geom.isna)

#saving results
write.csv(tab.wiki.geom.isna, file = "data_mediacloud/FRA_lmonde/tab.wiki.geom.isna.csv",
          fileEncoding = "UTF-8")

#recoding as character
tab.wiki.geom.isna$centroid.isna <- as.factor(tab.wiki.geom.isna$centroid.isna)
tab.wiki.geom.isna$local.area.isna <- as.factor(tab.wiki.geom.isna$local.area.isna)
tab.wiki.geom.isna$nesw.most.point.isna <- as.factor(tab.wiki.geom.isna$nesw.most.point.isna)

# conting table
table(local.area = tab.wiki.geom.isna$local.area.isna,
      centroid.isna = tab.wiki.geom.isna$centroid.isna )

summary(tab.wiki.geom.isna$nesw.most.point.isna )
# EXTRACTING AREA

# selection of the entities with an area

tab.select.my.entities <- tab.wiki.geom.isna[tab.wiki.geom.isna$local.area.isna == 0,]
my.entities <- tab.select.my.entities$labelorig
wikidata_item <- tab.select.my.entities$wikidata_item

# creation of a df

tab.wiki.geom.area <- data.frame()
tab.wiki.geom.area$my.entity <- as.character()
tab.wiki.geom.area$wikidata_item <- as.character()
tab.wiki.geom.area$labelen <- as.character()
tab.wiki.geom.area$description <- as.character()
tab.wiki.geom.area$area <- as.numeric()
tab.wiki.geom.area$area.unit <- as.numeric()

for (i in 1:length(my.entities)) {
  
  my.entity <- my.entities[i]
  
  my_properties <- get_property(wikidata_item[i])
  labelen <- my_properties[[1]]$labels$en$value
  description <- my_properties[[1]]$descriptions$fr$value
  
  # extract area
  
  tab.area<- (my_properties[[1]]$claims$P2046)
  area <- tab.area$mainsnak$datavalue$value$amount
  area.unit <- tab.area$mainsnak$datavalue$value$unit
  
  # creation of a local df with wiki data
  
  local.tab.wiki.geom.area <- data.frame(my.entity,wikidata_item[i],labelen,description,
                                    area,area.unit)
  
  # cbind of df
  
  tab.wiki.geom.area <- rbind(tab.wiki.geom.area, local.tab.wiki.geom.area)
  
  # print
  
  print(my.entities[i])
  
}


# save df

#write.csv(tab.wiki.geom.area, file = "data_mediacloud/FRA_lmonde/tab.wiki.geom.area.csv",
#          fileEncoding = "UTF-8")

tab.wiki.geom.area$area <- gsub("[+\"]", "", tab.wiki.geom.area$area)
tab.wiki.geom.area$area <- as.numeric(tab.wiki.geom.area$area)

# convert all data in the same unit

library(datamart)

tab.wiki.geom.area$area<- ifelse(tab.wiki.geom.area$area.unit == "http://www.wikidata.org/entity/Q232291",
       uconv(tab.wiki.geom.area$area, from = "mi²",to= "m²", uset= "Area"),
       tab.wiki.geom.area$area)

#size of the represented objects

tab.wiki.geom.area.ordered <- tab.wiki.geom.area[order(tab.wiki.geom.area$area,decreasing=TRUE),]
plot(tab.wiki.geom.area.ordered$area)

# il y a visiblement des soucis dans le référencement de la taille des villes

tab.wiki.geom.area <- tab.wiki.geom.area[tab.wiki.geom.area$area != max(tab.wiki.geom.area$area), ]


# max

tab.wiki.geom.area[tab.wiki.geom.area$area == max(tab.wiki.geom.area$area), ]

max(tab.wiki.geom.area.ordered$area)


# EXTRACTING CENTROIDS


# selection of the entities with a centroid

tab.select.my.entities <- tab.wiki.geom.isna[tab.wiki.geom.isna$centroid.isna == 0,]
my.entities <- tab.select.my.entities$labelorig
wikidata_item <- tab.select.my.entities$wikidata_item


tab.wiki.geom.justloc <- data.frame()
tab.wiki.geom.justloc$my.entity <- as.character()
tab.wiki.geom.justloc$wikidata_item <- as.character()
tab.wiki.geom.justloc$labelen <- as.character()
tab.wiki.geom.justloc$description <- as.character()
tab.wiki.geom.justloc$centroid.latitude <- as.numeric()
tab.wiki.geom.justloc$centroid.longitude <- as.numeric()


for (i in 1:length(my.entities)) {
  
  
  my.entity <- my.entities[i]
  
  my_properties <- get_property(wikidata_item[i])
  labelen <- my_properties[[1]]$labels$en$value
  description <- my_properties[[1]]$descriptions$fr$value
  
  
  #centroid
  tab.centroid <- (my_properties[[1]]$claims$P625)
  centroid.latitude <- tab.centroid$mainsnak$datavalue$value$latitude
  centroid.longitude <- tab.centroid$mainsnak$datavalue$value$longitude
  
  # creation of a local df with wiki data
  
  local.tab.wiki.geom <- data.frame(my.entity,wikidata_item[i],labelen,description,
                                    centroid.latitude,centroid.longitude)
  
  
  # cbind of df
  
  tab.wiki.geom.justloc <- rbind(tab.wiki.geom.justloc, local.tab.wiki.geom)
  
  # print
  
  print(my.entities[i])
  
  }

# save df

write.csv(tab.wiki.geom.justloc, file = "data_mediacloud/FRA_lmonde/tab.wiki.geom.justloc.csv",
                    fileEncoding = "UTF-8")





# plot


library(sf)

#même principê que l'ancien package rgdal

tab.wiki.geom.justloc <- read.delim("data_mediacloud/FRA_lmonde/tab.wiki.geom.justloc.csv",sep=",",
                                    fileEncoding = "UTF-8")


monFondDeCarte <-st_read(dsn ="C:/Users/Etienne/Nextcloud/@Home/Perso/IMAGEUN/Media_analysis/geom",layer = "world_eckert_WGS84")


#je transforme ma couche de données (points ici) en couche géographique

my.points <- st_as_sf(tab.wiki.geom.justloc, 
                       coords = c("centroid.longitude", "centroid.latitude"), 
                       crs = 4326, 
                       agr = "constant")

# transform

#monFondDeCarte <- st_transform(monFondDeCarte, crs= "+init=epsg:4326 +proj=longlat+ellps=WGS84 +datum=WGS84 +no_defs+towgs84=0,0,0")
my.points <-st_transform(my.points, from =4326, to = 54012)
monFondDeCarte <- st_transform(monFondDeCarte, from =4326, to=54012)

# plot data
plot(st_geometry(monFondDeCarte))
plot(st_geometry(my.points),cex=0.01,pch=19,col="red",add=T)

#st_crs(CRS("+init=epsg:4326"))

# FUZZY LOCATION

# create a summary df

tab.wiki.geom.justloc.summary <- data.frame()

tab.wiki.geom.justloc.summary$my.entity <- as.character()
tab.wiki.geom.justloc.summary$wikidata_item.i.  <- as.character()
tab.wiki.geom.justloc.summary$labelen <- as.character()
tab.wiki.geom.justloc.summary$description  <- as.character()
tab.wiki.geom.justloc.summary$centroid.latitude   <- as.numeric()
tab.wiki.geom.justloc.summary$centroid.longitude <- as.numeric()

df.names <- names(tab.wiki.geom.justloc.summary)

# calcul median

my.entity <- "median"
wikidata_item.i. <- "none"
labele <- "median"
description <- "the median"
median.lat <- median(tab.wiki.geom.justloc$centroid.latitude)
median.long <- median(tab.wiki.geom.justloc$centroid.longitude)

median.df <- data.frame(my.entity,wikidata_item.i.,labele,description,median.lat,median.long)

names(median.df) <- df.names

# rbind

tab.wiki.geom.justloc.summary <- rbind(tab.wiki.geom.justloc.summary,median.df)



monFondDeCarte <-st_read(dsn ="C:/Users/Etienne/Nextcloud/@Home/Perso/IMAGEUN/Media_analysis/geom",layer = "world_eckert_WGS84")

plot(st_geometry(monFondDeCarte))
#je transforme ma couche de données (points ici) en couche géographique

my.points <- st_as_sf(tab.wiki.geom.justloc.summary, 
                      coords = c("centroid.longitude", "centroid.latitude"), 
                      crs = 4326, 
                      agr = "constant")

monFondDeCarte <- st_transform(monFondDeCarte,crs=54012)
my.points <-st_transform(my.points,crs=54012)

plot(st_geometry(monFondDeCarte))
plot(st_geometry(my.points),cex=0.01,pch=19,col="red",add=T)




# CREATING POLYGONS

#list of sovereign states and dependent territories in Africa (Q228201) 

# selection of the entities with a centroid

tab.select.my.entities <- tab.wiki.geom.isna[tab.wiki.geom.isna$nesw.most.point.isna == "ok",]
my.entities <- tab.select.my.entities$labelorig
wikidata_item <- tab.select.my.entities$wikidata_item


# creating df

tab.wiki.geom <- data.frame()
tab.wiki.geom$my.entity <- as.character()
tab.wiki.geom$wikidata_item <- as.character()
tab.wiki.geom$labelen <- as.character()
tab.wiki.geom$description <- as.character()
tab.wiki.geom$easternmostpoint.latitude <- as.numeric()
tab.wiki.geom$easternmostpoint.longitude <- as.numeric()
tab.wiki.geom$northernmostpoint.latitude <- as.numeric()
tab.wiki.geom$northernmostpoint.longitude <- as.numeric()
tab.wiki.geom$westernmostpoint.latitude <- as.numeric()
tab.wiki.geom$westernmostpoint.longitude <- as.numeric()
tab.wiki.geom$southernmostpoint.latitude <- as.numeric()
tab.wiki.geom$southernmostpoint.longitude <- as.numeric()


# ATTENTION PROBLEME DE DETECTION AVEC L'AFRIQUE!!

for (i in 1:length(my.entities)) {
  
  
  my.entity <- my.entities[i]
  #my.entity <-"japon"
  my_properties <- get_property(wikidata_item[i])
  #my_properties <- get_property("Q17")
  labelen <- my_properties[[1]]$labels$en$value
  description <- my_properties[[1]]$descriptions$fr$value


  
  #easter most point : for more information about that: https://www.wikidata.org/wiki/Property:P1332
  tab.easternmostpoint<- (my_properties[[1]]$claims$P1334)
  easternmostpoint.latitude <- tab.easternmostpoint$mainsnak$datavalue$value$latitude
  easternmostpoint.longitude <- tab.easternmostpoint$mainsnak$datavalue$value$longitude
  
  #northernmost most point 
  tab.northernmostpoint<- (my_properties[[1]]$claims$P1332)
  northernmostpoint.latitude <- tab.northernmostpoint$mainsnak$datavalue$value$latitude
  northernmostpoint.longitude <- tab.northernmostpoint$mainsnak$datavalue$value$longitude

  #western most point 
  tab.westernmostpoint<- (my_properties[[1]]$claims$P1335)
  westernmostpoint.latitude <- tab.westernmostpoint$mainsnak$datavalue$value$latitude
  westernmostpoint.longitude <- tab.westernmostpoint$mainsnak$datavalue$value$longitude


  #southern most point 
  tab.southernmostpoint<- (my_properties[[1]]$claims$P1333)
  southernmostpoint.latitude <- tab.southernmostpoint$mainsnak$datavalue$value$latitude
  southernmostpoint.longitude <- tab.southernmostpoint$mainsnak$datavalue$value$longitude
  
  # creation of a local df with wiki data
  
  local.tab.wiki.geom <- data.frame(my.entity,wikidata_item[i],labelen,description,
                                    easternmostpoint.latitude,
                                    easternmostpoint.longitude,northernmostpoint.latitude,
                                    northernmostpoint.longitude,westernmostpoint.latitude,
                                    westernmostpoint.longitude,southernmostpoint.latitude,
                                    southernmostpoint.longitude)
  
  
  
  
  
  # cbind of df
  
  tab.wiki.geom <- rbind(tab.wiki.geom, local.tab.wiki.geom)
  
  # print
  
  print(my.entities[i])
  
}


write.csv(tab.wiki.geom, file = "data_mediacloud/FRA_lmonde/tab.wiki.geom.csv",
          fileEncoding = "UTF-8")


# tableau final
#tab.wiki.geom.sel <- read.delim(,fileEncoding = "UTF-8")


