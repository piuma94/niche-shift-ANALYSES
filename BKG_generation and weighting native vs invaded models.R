setwd("G:\\New test with laure")

setwd("F:\\New test with laure")

## Setting the environment ###
## for windows
memory.limit(size=76000)
## uploading some libraries
library(dplyr)
#library(purrr)
#library(readr)  
#library(magrittr) # for %T>% pipe
#library(rgbif) # for occ_download
#library(taxize) # for get_gbifid_
library(biomod2)
#library(snowfall)
library(raster)
library(dismo)
library(ecospat)
library(sf)
#
###rm(list = ls(all.names = TRUE))
##
##
##
##
###### FROM HERE NEW TRY
##p2<-raster("D:\\Luigi\\Dataset e modelli DEF\\GBIF_sampl.int.tif")
##p1<-raster("D:\\Luigi\\Dataset e modelli DEF\\EVA_sampl.int.tif")
##EVA_sampl <- raster("D:\\Luigi\\Dataset e modelli DEF\\EVA_sampl.tif")  ## EVA Blank raster
##plot(EVA_sampl)
##load("D:\\Luigi\\Dataset e modelli DEF\\ly.names.def")
##GBIF_sampl <- raster("D:\\Luigi\\Dataset e modelli DEF\\Blank_raster.tif") ## GBIF Blank raster
##plot(GBIF_sampl)
#### # fisso casa
##EVA_buffer <- st_read("D:\\Luigi\\Dataset e modelli DEF\\EVA_BUFFER_100km.shp") ## this is the good one
##plot(EVA_buffer)
### Fisso casa
##GBIF_buffer<-st_read("D:\\Luigi\\Dataset e modelli DEF\\DATA_WIDE_GRILLED_BUFFER_ALL6.shp")   ### this is the good one
##plot(GBIF_buffer)
##
##myexpl.var30_Global_used <- stack("D:\\Luigi\\Dataset e modelli DEF\\myexpl.var30_Global_used.tif")
##names(myexpl.var30_Global_used)<- ly.names.def
##
#### GENERATING GLOBAL AND LOCAL BKG
##### LOCAL !!
### i don't need here the cell value
##EVA_BKG <- as.data.frame(sampleRandom(  mask(EVA_sampl,EVA_buffer), size=1200000,na.rm=T,xy=T))
###EVA_BKG <- as.data.frame(EVA_BKG)
##colnames(EVA_BKG) <-c("Longitude","Latitude","values")
##
#### GLOBAL!! #mask(GBIF_sampl,GBIF_buffer)
##GBIF_BKG <- as.data.frame(sampleRandom( mask(GBIF_sampl,GBIF_buffer), size=10000000,na.rm=T,xy=T))
### 10000000
###GBIF_BKG <- as.data.frame(GBIF_BKG)
#### renaming columns
##colnames(GBIF_BKG) <-c("Longitude","Latitude","values")
####### fINALLY COMPUTING PA WEIGHT 
##### THIS IS THE VALUE I NEED  FOR EVA SAMPL
##EVA_BKG_Weigth <- as.data.frame(cbind(EVA_BKG,raster::extract(p1, EVA_BKG[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
##GBIF_BKG_Weigth <- as.data.frame(cbind(GBIF_BKG,raster::extract(p1, GBIF_BKG[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
#### the variable values has no meaning here
##colnames(EVA_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots")
##colnames(GBIF_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots")
##
#### EXTRACTING GBIF INT
##EVA_BKG_Weigth <- as.data.frame(cbind(EVA_BKG_Weigth,raster::extract(p2, EVA_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
##GBIF_BKG_Weigth <- as.data.frame(cbind(GBIF_BKG_Weigth,raster::extract(p2, GBIF_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
#### the variable values has no meaning here
##colnames(EVA_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots","cell","GBIF_Nr.pres")
##colnames(GBIF_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots","cell","GBIF_Nr.pres")
##
##
##### correcting NA's
##EVA_BKG_Weigth[is.na(EVA_BKG_Weigth$EVA_Nr.plots),"EVA_Nr.plots"] <- 0
##GBIF_BKG_Weigth[is.na(GBIF_BKG_Weigth$EVA_Nr.plots),"EVA_Nr.plots"] <- 0
### Then i add 1 to all plots sampled
### TO GIVE A MINIMUM VALUE ALSO TO NOT SAMPLED BACKGROUND POINTS
##EVA_BKG_Weigth[,"EVA_Nr.plots"] <- EVA_BKG_Weigth[,"EVA_Nr.plots"]+1
##GBIF_BKG_Weigth[,"EVA_Nr.plots"] <- GBIF_BKG_Weigth[,"EVA_Nr.plots"]+1
##
##
##### correcting NA's
##EVA_BKG_Weigth[is.na(EVA_BKG_Weigth$GBIF_Nr.pres),"GBIF_Nr.pres"] <- 0
##GBIF_BKG_Weigth[is.na(GBIF_BKG_Weigth$GBIF_Nr.pres),"GBIF_Nr.pres"] <- 0
### Then i add 1 to all plots sampled
### TO GIVE A MINIMUM VALUE ALSO TO NOT SAMPLED BACKGROUND POINTS
##EVA_BKG_Weigth[,"GBIF_Nr.pres"] <- EVA_BKG_Weigth[,"GBIF_Nr.pres"]+1
##GBIF_BKG_Weigth[,"GBIF_Nr.pres"] <- GBIF_BKG_Weigth[,"GBIF_Nr.pres"]+1
##
###### extracting ENV.VARS
##EVA_BKG_Weigth <- as.data.frame(cbind(EVA_BKG_Weigth,raster::extract(myexpl.var30_Global_used, EVA_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
##GBIF_BKG_Weigth <- as.data.frame(cbind(GBIF_BKG_Weigth,raster::extract(myexpl.var30_Global_used, GBIF_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
##
#### checking them
##apply(EVA_BKG_Weigth,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
##apply(GBIF_BKG_Weigth,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
##
##### i need all columns except for the cell one
##EVA_BKG_Weigth<-EVA_BKG_Weigth[,-4]
##GBIF_BKG_Weigth<-GBIF_BKG_Weigth[,-4]
##### maybe here i am eliminating also 
##EVA_BKG_Weigth <-na.omit(EVA_BKG_Weigth )
##GBIF_BKG_Weigth<-na.omit(GBIF_BKG_Weigth)
##
##### saving it 
##save(EVA_BKG_Weigth, file="LOCAL_BKG.Rda")
##### 
##save(GBIF_BKG_Weigth, file="GLOBAL_BKG.Rda")
##
##load("GLOBAL_BKG.Rda")
##load("LOCAL_BKG.Rda")
##
##
##
###### check on the environmental variables: #############
##load("D:\\Luigi\\Dataset e modelli DEF\\EVAE.70.Rda")
##load(file="D:\\Luigi\\Dataset e modelli DEF\\GBIF_grilled.P2.Rda") ## this is the best, i will not have duplicates when reimporting
##
##
##
##EVA_ENV.val <- as.data.frame(raster::extract(myexpl.var30_Global_used, EVAE.70[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T))
##GBIF_ENV.val <- as.data.frame(raster::extract(myexpl.var30_Global_used, GBIF_grilled[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T))
##
#### checking
##summary(EVA_ENV.val)
##summary(EVAE.70[,98:104])
#
#
#
#### NEW NICHE SHIFT ANALYSES, GENERATING BKG IN STUDY AREA AND NATIVE RANGE ############
#
#### IN THE LOCAL INVADED BUFFER, I CAN USE THIS DATASET,THAT I NEED TO RESAMPLE IN THE AREA WE DECIDE TO INVESTIGATE. 
#load("LOCAL_BKG.Rda") ## this is a good dataset 
#### local area in which we want to project 
#load("ly.names.def")
#myexpl.var30_ST_DEF <- stack("myexpl.var30_ST_DEF.tif")  ## here i am saving the all dataset cropped
#myexpl.var30_ST_DEF [[6]]<-log(myexpl.var30_ST_DEF [[6]]+1)
#
#names(myexpl.var30_ST_DEF)<- ly.names.def
#
#
##### reducing  the dataset in only the straight invaded range 
#EVA_BKG_Weigth<- cbind(EVA_BKG_Weigth,raster::extract(myexpl.var30_ST_DEF[[1]], EVA_BKG_Weigth[,c("Longitude","Latitude")]))
#length(EVA_BKG_Weigth$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA_BKG_Weigth[, c("Longitude", `[!is.na(EVA_BKG_Weigth$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA_BKG_Weigth[, c("Longitude", `)]) ### I will remove half of the points remaining with 482720
#
#### Eliminating NA's cioè quelli fuori il med_europe# taglio parecchio stretto
#EVA_BKG_Weigth  <- EVA_BKG_Weigth[!is.na(EVA_BKG_Weigth$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA_BKG_Weigth[, c("Longitude", `),]
#
#apply(EVA_BKG_Weigth,2, function(x) sum(is.na(x)))   ## ok i have NO NA in all the rows
#
#
##### ELABORATING GBIF NOW ##########
#p1<-raster("F:\\Luigi\\Dataset e modelli DEF\\EVA_sampl.int.tif")## This dataset should be controlled, i think it's correct 
#p2<-raster("F:\\Luigi\\Dataset e modelli DEF\\GBIF_sampl.int.tif")  ### gbif sampling intensity.
#GBIF_sampl <- raster("F:\\Luigi\\Dataset e modelli DEF\\Blank_raster.tif") ## GBIF Blank raster
#plot(GBIF_sampl)
#plot(p1)
#plot(p2)
### GLOBAL!! #mask(GBIF_sampl,GBIF_buffer)
#GBIF_BKG <- as.data.frame(sampleRandom( (GBIF_sampl), size=10000000,na.rm=T,xy=T))
## 10000000
##GBIF_BKG <- as.data.frame(GBIF_BKG)
### renaming columns
#colnames(GBIF_BKG) <-c("Longitude","Latitude","values")
###### fINALLY COMPUTING PA WEIGHT 
#### THIS IS THE VALUE I NEED  FOR EVA SAMPL
#GBIF_BKG_Weigth <- as.data.frame(cbind(GBIF_BKG,raster::extract(p1, GBIF_BKG[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
### the variable values has no meaning here
#colnames(GBIF_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots")
#
### EXTRACTING GBIF INT
#GBIF_BKG_Weigth <- as.data.frame(cbind(GBIF_BKG_Weigth,raster::extract(p2, GBIF_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
### the variable values has no meaning here
#colnames(GBIF_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots","cell","GBIF_Nr.pres")
#
#
#### correcting NA's
#GBIF_BKG_Weigth[is.na(GBIF_BKG_Weigth$EVA_Nr.plots),"EVA_Nr.plots"] <- 0
## Then i add 1 to all plots sampled
## TO GIVE A MINIMUM VALUE ALSO TO NOT SAMPLED BACKGROUND POINTS
#GBIF_BKG_Weigth[,"EVA_Nr.plots"] <- GBIF_BKG_Weigth[,"EVA_Nr.plots"]+1
#
#
#### correcting NA's
#GBIF_BKG_Weigth[is.na(GBIF_BKG_Weigth$GBIF_Nr.pres),"GBIF_Nr.pres"] <- 0
## Then i add 1 to all plots sampled
## TO GIVE A MINIMUM VALUE ALSO TO NOT SAMPLED BACKGROUND POINTS
#GBIF_BKG_Weigth[,"GBIF_Nr.pres"] <- GBIF_BKG_Weigth[,"GBIF_Nr.pres"]+1
#
##### extracting ENV.VARS
#GBIF_BKG_Weigth <- as.data.frame(cbind(GBIF_BKG_Weigth,raster::extract(myexpl.var30_Global_used, GBIF_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
#
### checking them
#apply(GBIF_BKG_Weigth,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
#
#### i need all columns except for the cell one
#GBIF_BKG_Weigth<-GBIF_BKG_Weigth[,-4]
#### maybe here i am eliminating also 
#GBIF_BKG_Weigth<-na.omit(GBIF_BKG_Weigth)
#
#### saving it 
#### 
#save(GBIF_BKG_Weigth, file="GLOBAL_BKG_no_BUFF.Rda")

######### FINAL DEF METHOD I NEED TO RE DO BOTH EVA AND GBIF DATASETS #####################
### GENERATING GLOBAL AND LOCAL BKG
p1<-raster("F:\\Luigi\\Dataset e modelli DEF\\EVA_sampl.int.tif")## This dataset should be controlled, i think it's correct 
p2<-raster("F:\\Luigi\\Dataset e modelli DEF\\GBIF_sampl.int.tif")  ### gbif sampling intensity.
GBIF_sampl <- raster("F:\\Luigi\\Dataset e modelli DEF\\Blank_raster.tif") ## GBIF Blank raster
EVA_sampl <- raster("F:\\Luigi\\Dataset e modelli DEF\\EVA_sampl.tif")  ## EVA Blank raster
myexpl.var30_ST_DEF <- stack("myexpl.var30_ST_DEF.tif")  ## here i am saving the all dataset cropped

plot(GBIF_sampl)
plot(EVA_sampl)
plot(p1)
plot(p2)
## GLOBAL!! #mask(GBIF_sampl,GBIF_buffer)
GBIF_BKG <- as.data.frame(sampleRandom( (GBIF_sampl), size=20000000,na.rm=T,xy=T))
### LOCAL !!
# i don't need here the cell value
EVA_BKG <- as.data.frame(sampleRandom(  EVA_sampl , size=4000000,na.rm=T,xy=T))
#EVA_BKG <- as.data.frame(EVA_BKG)
colnames(EVA_BKG) <-c("Longitude","Latitude","values")


## renaming columns
colnames(GBIF_BKG) <-c("Longitude","Latitude","values")
##### fINALLY COMPUTING PA WEIGHT 
### THIS IS THE VALUE I NEED  FOR EVA SAMPL
EVA_BKG_Weigth <- as.data.frame(cbind(EVA_BKG,raster::extract(p1, EVA_BKG[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
GBIF_BKG_Weigth <- as.data.frame(cbind(GBIF_BKG,raster::extract(p1, GBIF_BKG[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
## the variable values has no meaning here
colnames(EVA_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots")
colnames(GBIF_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots")

## EXTRACTING GBIF INT
EVA_BKG_Weigth <- as.data.frame(cbind(EVA_BKG_Weigth,raster::extract(p2, EVA_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
GBIF_BKG_Weigth <- as.data.frame(cbind(GBIF_BKG_Weigth,raster::extract(p2, GBIF_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
## the variable values has no meaning here
colnames(EVA_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots","cell","GBIF_Nr.pres")
colnames(GBIF_BKG_Weigth) <-c("Longitude","Latitude","values","cell","EVA_Nr.plots","cell","GBIF_Nr.pres")


### correcting NA's
EVA_BKG_Weigth[is.na(EVA_BKG_Weigth$EVA_Nr.plots),"EVA_Nr.plots"] <- 0
GBIF_BKG_Weigth[is.na(GBIF_BKG_Weigth$EVA_Nr.plots),"EVA_Nr.plots"] <- 0
# Then i add 1 to all plots sampled
# TO GIVE A MINIMUM VALUE ALSO TO NOT SAMPLED BACKGROUND POINTS
EVA_BKG_Weigth[,"EVA_Nr.plots"] <- EVA_BKG_Weigth[,"EVA_Nr.plots"]+1
GBIF_BKG_Weigth[,"EVA_Nr.plots"] <- GBIF_BKG_Weigth[,"EVA_Nr.plots"]+1


### correcting NA's
EVA_BKG_Weigth[is.na(EVA_BKG_Weigth$GBIF_Nr.pres),"GBIF_Nr.pres"] <- 0
GBIF_BKG_Weigth[is.na(GBIF_BKG_Weigth$GBIF_Nr.pres),"GBIF_Nr.pres"] <- 0
# Then i add 1 to all plots sampled
# TO GIVE A MINIMUM VALUE ALSO TO NOT SAMPLED BACKGROUND POINTS
EVA_BKG_Weigth[,"GBIF_Nr.pres"] <- EVA_BKG_Weigth[,"GBIF_Nr.pres"]+1
GBIF_BKG_Weigth[,"GBIF_Nr.pres"] <- GBIF_BKG_Weigth[,"GBIF_Nr.pres"]+1

## global unused variables
myexpl.var30_Global_used<- stack("myexpl.var30_Global_used.tif")
load("ly.names.def")
names(myexpl.var30_Global_used)<- ly.names.def


#### extracting ENV.VARS
EVA_BKG_Weigth <- as.data.frame(cbind(EVA_BKG_Weigth,raster::extract(myexpl.var30_Global_used, EVA_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
GBIF_BKG_Weigth <- as.data.frame(cbind(GBIF_BKG_Weigth,raster::extract(myexpl.var30_Global_used, GBIF_BKG_Weigth[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))

## checking them
apply(EVA_BKG_Weigth,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
apply(GBIF_BKG_Weigth,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows

### i need all columns except for the cell one
EVA_BKG_Weigth<-EVA_BKG_Weigth[,-4]
GBIF_BKG_Weigth<-GBIF_BKG_Weigth[,-4]
### maybe here i am eliminating also 
EVA_BKG_Weigth <-na.omit(EVA_BKG_Weigth )
GBIF_BKG_Weigth<-na.omit(GBIF_BKG_Weigth)

### saving it 
save(EVA_BKG_Weigth, file="LOCAL_BKG_NICHE_AN.Rda")
### 
save(GBIF_BKG_Weigth, file="GLOBAL_BKG_NICHE_AN.Rda")


