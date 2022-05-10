

#setwd("/Volumes/Extreme SSD/New test with laure") ### mac Luigi extreme SSD

# setwd("G:\\New test with laure") ## this is for fisso UNI
## directory on MAC



### fisso casa
setwd("E:\\New test with laure")
#setwd("C:\\Users\\luigi\\OneDrive - Universita degli Studi Roma Tre\\New test with laure") ##one drive fisso casa 
### uni: 
setwd("H:\\New test with laure")
#setwd("C:\\Users\\labEcologia\\OneDrive - Universita degli Studi Roma Tre\\Seebens residence Time\\Per diletta") ##one drive fisso casa 

### ATTENZIONE DUE COSE SONO DA MODIFICARE: 
## 1) DEVO PRENDERE SOLO LE PRESENZE GRIGLIATE, QUINDI EVAT_30 E EVAE_70 # E IL RISPETTIVO GBIF
## working on it
## 2) CAPIRE COME è MEGLIO FARE LA FRAMEWORK PER TESTARE LE MIE VARIE COSE.
## IL PUNTO UNO E DUE POTREI GIà IMPLEMENTARLI IO


## Setting the environment ###
## for windows
#memory.limit(size=106000)
## uploading some libraries
#library(bigmemory)
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
library(doParallel)
library(foreach)

#rm(list = ls(all.names = TRUE))

#### rethinking the environmental varibles. Should be global and only for current
### Chelsae envirem and HF, log(pop.dens)
## subsetting each time native and invaded range 
# the invaded is always my med env. also variables make sense in the end... 
# should i check for correlation in the env. vars??
### defining the niche shift between native and invaded range. same method. 

### remember, my datasets are in the local and global buffer....

### 1) upload global env.vars and local dataset (perchè è sempre lo stesso)
### 1a) upload the diletta's dataset of native rangeallowing me to select the correct environmental conditions
#in the loop
### 2) cut points and env vars for global range,  (il local è sempre lo stesso, devo solo selezionare la specie)
### 3) assembling dataset: local env+ sp.presences/ global env + sp.presences


#### I NEED LOCAL AND GLOBAL DATASETS THAT SHOULD BE NOT GRILLED ####
### EVA
# # #load("EVA_notgrilled.Rda") ### carfully check how many should they be?? more than 105425 that are the full EVA30+EVA70
# # ### MI SERVE SPECIES ENV.VALS NOTGRILLED CHE è UGUALE A SPECIESNOTGRILLED ENV1
# # #### ho questi due dataset, uno dovrebbe avere le assenze l'altro no 
# # ### una vecchia prova, togliamolo nel file DEF
# # #load("/Volumes/Extreme SSD/New test with laure/species.notgrilled.env1.Rda")
# # ##### little check 
# # #apply(species.notgrilled.env1,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows
# # #
# # #### is it true one has absences and the other not??
# # #dim(species.notgrilled.env1[rowSums(species.notgrilled.env1[,7:99])>0,]) ## these are the only invaded plots I used
# # #
# # #setdiff(species.notgrilled.env1[rowSums(species.notgrilled.env1[,7:99])>0,"PlotObservationID"],EVA_notgrilled$PlotObservationID)
# # #setdiff(EVA_notgrilled$PlotObservationID,species.notgrilled.env1[rowSums(species.notgrilled.env1[,8:99])>0,"PlotObservationID"])
# # ### prefect!! the dataset are exactly the same!!
# # #### Penso si possano usare entrambi, le differenze sono di pochi valori.... 
# # #sum(duplicated(EVA_notgrilled$cell)) ### i have very few duplicated in EVA anyway!!
# # #sum(duplicated(species.notgrilled.env1$cell)) ### i have very few duplicated in EVA anyway!!
# # #### quindi utilizzo quello più facile da gestire, ossia le sole presenze di EVA non grigliato
# # #
# # #
# # #
# # ### eliminating GBIF sampling intensity because it has NA's
# # #EVA_notgrilled <- (EVA_notgrilled[-109])
# # #
# # #apply(EVA_notgrilled,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows
# # ##EVA_notgrilled <- (EVA_notgrilled[-109])
# # ### the species i need to evaluate
# # #sp.names <- colnames(EVA_notgrilled[,7:99])  ## check these ## 
# # #
# # #### GBIF (i can not use these datasets withouth checking, these are old datasets i am not sure of their validity, check on the old)
# # #load("GBIF_notgrilled.Rda")
# # #apply(GBIF_notgrilled,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows
# # ##GBIF_notgrilled <- na.omit(GBIF_notgrilled)
# # ##### CLEANING GBIF AND EVA, I NEED ONLY ABSENCES 
# # #colnames(GBIF_notgrilled[,4:96]) ## species are 4:96
# # #colnames(EVA_notgrilled[,7:99]) # # 7:99
# # ### i find out they have yet only presences
# # ### EVA_notgrilled_pres<- EVA_notgrilled[rowSums(EVA_notgrilled[,7:99])>0,]
# # ### GBIF_notgrilled_pres<- GBIF_notgrilled[rowSums(GBIF_notgrilled[,4:96])>0,]
# # #
# # #### i checked these datasets are yet grilled and of only presences. 
# # ### these dataset also has sampling intensity, because we had the records of absences that for now are not important for me
# # #
# # ###  ## GBIF european
# # ###  #GBIF_notgrilled_env_EUROPE<- read.csv("GBIF_notgrilled_env_EUROPE.csv")
# # ###  load("GBIF_notgrilled_env_EUROPE.Rda")
# # ###  apply(GBIF_notgrilled_env_EUROPE,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
# # ###  ## plotting it to check my values
# # ###  plot(crop(myexpl.var30_MEDEU[[1]],c(-15,42,33,52)))
# # ###  points(GBIF_notgrilled_env_EUROPE[,c("Longitude","Latitude")],cex=0.1, pch=22, col="black")
# # ###  points(EVA_notgrilled[,c("Longitude","Latitude")],cex=0.1, pch=22, col="red")
# # #



#### GRILLED DATASETS FOR EVA AND GBIF #####
### EVA 30 and 70
load("EVAE.70.Rda")
load("EVAT.30.Rda")
## first assembling a complete single dataset
EVA <- rbind(EVAE.70[,-106],EVAT.30[,-106])
## the species i need to evaluate
sp.names <- colnames(EVA[,5:97])  ## check these ## 

### GBIF_ grilled with ENV.vals
load(file="GBIF_grilled.P2.Rda") ## this is the best, i will not have duplicates when reimporting

## QUESTI DATASET SONO BLINDATI, super controllati ed esenti errori. Un unico plot in ogni cella. 

#### Global env. layers ####
load("ly.names.def")
## global buffer, that may not be enough to sample native range env.cond
##myexpl.var30_GBIF_buf <- stack("myexpl.var30_GBIF_buf.tif")
##names(myexpl.var30_GBIF_buf) <- ly.names.def
##plot(myexpl.var30_GBIF_buf)
#### need to do a global dataset with these variables
##ly.names.def
### global complete environmental variable: i don't need however to extract it for the global dataset
## global unused variables
global<- stack("myexpl.var30_Global_used.tif")
#names(global) <- ly.names.def
plot(global) ## this is the one, i visully checked it 
global [[6]]<-log(global [[6]]+1)
names(global) <- ly.names.def

### REMEMBER, THE EXTRACTED VALUES IN EVA AND GBIF ARE NOT LOGARITMIZED.


###### Local env.layers ######
load("ly.names.def")
##  ## ## local buffer 100 km around eva and GBIF
##  ## myexpl.var30_MEDEU <- stack("/Volumes/Extreme SSD/New test with laure/myexpl.var30_MEDEU.tif") ## this is not the right buffer
##  ## names(myexpl.var30_MEDEU) <- ly.names.def
##  ## plot(myexpl.var30_MEDEU)
##  ## ## this is the final buffer i used for my final analyses ## too big
##  ## new_lc.buff<- st_read("/Volumes/Extreme SSD/New test with laure/LOCAL_BUFF_EVAGBIF.shp")
##  ## plot(new_lc.buff)
##  ## ## MED_ENV WHERE I WANT TO PROJECT
myexpl.var30_ST_DEF <- stack("myexpl.var30_ST_DEF.tif")  ## here i am saving the all dataset cropped
myexpl.var30_ST_DEF [[6]]<-log(myexpl.var30_ST_DEF [[6]]+1)

names(myexpl.var30_ST_DEF)<- ly.names.def

### PARTE IMPORTANTE IN CUI SELEZIONO SOLO LE PRESENZE NELLA MIA AREA DI STUDIO ######
##### POTREBBE AVERE SENSO PRENDERE IL MED. BASIN. COME BIOME??? 
##
##### IN BASE A CHE ARIA VOGLIO PRENDERE, POSSO CARICARE UN FILE RASTER DIFFERENTE
#### intersezione tra med_env e EVA/GBIF ## solo le presenze però, perchè le assenze non  mi servono
##GBIF_notgrilled_study_area<- cbind(GBIF_notgrilled,raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_notgrilled[,c("Longitude","Latitude")]))
##length(GBIF_notgrilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_notgrilled[, c("Longitude", `[!is.na(GBIF_notgrilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_notgrilled[, c("Longitude", `)]) ### 3646 values are not NA
##
##EVA_notgrilled_study_area<- cbind(EVA_notgrilled,raster::extract(myexpl.var30_ST_DEF[[1]], EVA_notgrilled[,c("Longitude","Latitude")]))
##length(EVA_notgrilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA_notgrilled[, c("Longitude", `[!is.na(EVA_notgrilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA_notgrilled[, c("Longitude", `)]) ### 3646 values are not NA
##
##### Eliminating NA's cioè quelli fuori il med_europe# taglio parecchio stretto
##EVA_notgrilled_study_area  <- EVA_notgrilled_study_area[!is.na(EVA_notgrilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA_notgrilled[, c("Longitude", `),]
##GBIF_notgrilled_study_area <- GBIF_notgrilled_study_area[!is.na(GBIF_notgrilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_notgrilled[, c("Longitude", `),]
##
##
##apply(EVA_notgrilled_study_area,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows
##apply(GBIF_notgrilled_study_area,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows
##
#### ok i have 
#### GBIF: 207980 losing 2873569-207980=2665589
#### EVA: 6677 losing 29294-6677= 22617 plots
##### a visual chek on EVA numbers:
##plot(myexpl.var30_ST_DEF[[1]])
##points(GBIF_notgrilled_study_area[!is.na(GBIF_notgrilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_notgrilled[, c("Longitude", `), c("Longitude", "Latitude")], cex=0.1, pch=22, col="blue")
##
##points(EVA_notgrilled_study_area[!is.na(EVA_notgrilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA_notgrilled[, c("Longitude", `), c("Longitude", "Latitude")], cex=0.1, pch=22)
##### rispetto a quello che potevo fare 
###plot(myexpl.var30_ST_DEF[[1]])
###points(b[, c("Longitude", "Latitude")], cex=0.1, pch=22, col="blue")
###points(c[, c("Longitude", "Latitude")], cex=0.1, pch=22, col="black")
###  PER I DATI GRIGLIATI! PARTE IMPORTANTE IN CUI SELEZIONO SOLO LE PRESENZE NELLA MIA AREA DI STUDIO ######
### POTREBBE AVERE SENSO PRENDERE IL MED. BASIN. COME BIOME??? 

### IN BASE A CHE ARIA VOGLIO PRENDERE, POSSO CARICARE UN FILE RASTER DIFFERENTE
## intersezione tra med_env e EVA/GBIF ## solo le presenze però, perchè le assenze non  mi servono
GBIF_grilled_study_area<- cbind(GBIF_grilled,raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_grilled[,c("Longitude","Latitude")]))
length(GBIF_grilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_grilled[, c("Longitude", `[!is.na(GBIF_grilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_grilled[, c("Longitude", `)]) ### 53083 values are not NA

EVA_study_area<- cbind(EVA,raster::extract(myexpl.var30_ST_DEF[[1]], EVA[,c("Longitude","Latitude")]))
length(EVA_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA[, c("Longitude", `[!is.na(EVA_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA[, c("Longitude", `)]) ### 34033 values are not NA

### Eliminating NA's cioè quelli fuori il med_europe# taglio parecchio stretto
EVA_study_area  <- EVA_study_area[!is.na(EVA_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA[, c("Longitude", `),]
GBIF_grilled_study_area <- GBIF_grilled_study_area[!is.na(GBIF_grilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_grilled[, c("Longitude", `),]


apply(EVA_study_area,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows
apply(GBIF_grilled_study_area,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows

## ok i have 
## GBIF: 689561 losing 689561-53083=636478
## EVA: 105425 losing 105425-34033= 71392 plots
### a visual chek on EVA numbers:
plot(myexpl.var30_ST_DEF[[1]])
points(GBIF_grilled_study_area[, c("Longitude", "Latitude")], cex=0.1, pch=22, col="blue")

points(EVA_study_area[, c("Longitude", "Latitude")], cex=0.1, pch=22)
### rispetto a quello che potevo fare 
#plot(myexpl.var30_ST_DEF[[1]])
#points(b[, c("Longitude", "Latitude")], cex=0.1, pch=22, col="blue")
#points(c[, c("Longitude", "Latitude")], cex=0.1, pch=22, col="black")

###########


### TRANSFORMING MED ST AREA IN A DATAFRAME FOR THE ANALYSES ## this has log pop.dens #######
## done it yet
##  ## med_ENV <- rasterToPoints(myexpl.var30_ST_DEF, fun=NULL, spatial=FALSE) ## this suppose i have sampled the whole region in the same way
#write.csv(med_ENV,file="med_ENV.study_area_env.var.dataframe.csv")
med_ENV <- read.csv("med_ENV.study_area_env.var.dataframe.csv") ## ok, it is a DF
med_ENV <- na.omit(med_ENV)## ADDING A COLUMN THAT I CAN THEN RENAME EACH TIME TO HAVE A 0 VALUE IN IT DEFINING ENV.BACK
a <- rep(0,dim(med_ENV)[1])
## add the new column and then the name
 med_ENV <- cbind(med_ENV,New.col=a)

### POWO map species #####
 ### mac
POWO_regions<- shapefile("/Users/luigi/Desktop/Seebens residence Time-NON AGGIORNATO/Per diletta/TDWG level-3 regions/level3")

 ## fisso
 POWO_regions<- shapefile("H:\\Seebens residence Time\\Per diletta\\TDWG level-3 regions\\level3")
 
 #plot(POWO_regions)
# i can use this to preliminarily procede with scripting
POWO_regions@data[POWO_regions@data[["LEVEL3_NAM"]]=="Western Australia",]

### THIS PART IS A FOUNDATION OF FUTURE ANALYSES
##  ##  ### una prova per vedere se POWO e i miei layer si sovrappongono bene
##  ##  plot(crop(global,POWO_regions[POWO_regions@data[["LEVEL3_NAM"]]=="Western Australia",]))
##  ##  ## this allow me subsetting the data, considering the native range
##  ##  a<- cbind(GBIF_notgrilled,raster::extract(crop(global[[1]],POWO_regions[POWO_regions@data[["LEVEL3_NAM"]]=="Western Australia",]), GBIF_notgrilled[,c("Longitude","Latitude")]))
##  ##  length(a$`raster::extract(crop(global[[1]], POWO_regions[POWO_regions@data[["LEVEL3_NAM"]] == `[!is.na(a$`raster::extract(crop(global[[1]], POWO_regions[POWO_regions@data[["LEVEL3_NAM"]] == `)]) ### 3646 values are not NA
##  ##  ## checking results
##  ##  plot(crop(global[[1]],POWO_regions[POWO_regions@data[["LEVEL3_NAM"]]=="Western Australia",]))
##  ##  ## against the total points
##  ##  points(GBIF_notgrilled[,c("Longitude","Latitude")], cex=0.1,pch=22, col="red")
##  ##  ## add these after to see which i have selected (deve essere apposto perchè avevo eliminato in precedenza gli NA)
##  ##  points(a[!is.na(a$`raster::extract(crop(global[[1]], POWO_regions[POWO_regions@data[["LEVEL3_NAM"]] == `),c("Longitude","Latitude")], cex=0.1,pch=22)
#### OUT OF TOPIC iDENTIFICARE LA REGIONE BIOGEOGRAFICA D'ORIGINE ######
Dataset<-read.csv("H:\\New test with laure\\dataset 0103.csv", sep=";", header = T)
# myRespName<-"Lepidium.didymum"
## this is to plot the biogeo rewgion TDWG first level
  plot(POWO_regions[POWO_regions@data[["LEVEL1_COD"]]=="9",])
## 1= Europe
## 2= Africa
## 3= Asia
## 4= Indo-Malay
## 5= Australia
## 6= pacific       : non penso ci saranno specie qui
## 7= north america
## 8= south America
## 9= antartica
### i need to add to each line = species, the 1 values in all columns 
## create the dataframe: 
biogeo.reg<-as.data.frame(cbind(sp.names,rep(0,93),
                  rep(0,93),
                  rep(0,93),
                  rep(0,93),
                  rep(0,93),
                  rep(0,93),
                  rep(0,93),
                  rep(0,93),
                  rep(0,93)))
class(biogeo.reg) ## it should be a datarfame
colnames(biogeo.reg)<-c("sp.names","1","2","3","4","5","6","7","8","9")

for( myRespName in sp.names[]) {
  
native_range <- Dataset[ Dataset[,myRespName]==1,"POWO.REGIONS" ]
a<-unique(POWO_regions@data$LEVEL1_COD [POWO_regions@data[["LEVEL3_NAM"]] %in% native_range ])
#POWO_regions@data [POWO_regions@data[["LEVEL3_NAM"]] %in% native_range, ] ## to know which regions are the ones that define my specie's biogeo origin.
## if you are curious and whant to plot the native range
# plot(POWO_regions [POWO_regions@data[["LEVEL3_NAM"]] %in% native_range ,])

biogeo.reg[biogeo.reg[,1]==myRespName,colnames(biogeo.reg) %in% a]<-1

}
colnames(biogeo.reg)<-c("Species","Europe",
                        "Africa",
                        "Asia",
                        "Indo-Malay",
                        "Australia",
                        "Pacific" ,     
                        "north America",
                        "south America",
                        "antartica")
save(biogeo.reg,file="biogeo.reg.Rda") ## saving it 
# reuploading it 
load("biogeo.reg.Rda")


###### uploading Diletta's data #####
#  i will need a matrix of all my species (columns) and the regions (rows)
Dataset<-read.csv("dataset 0103.csv", sep=";", header = T)
#Dataset[ Dataset[,my.resp.name]==1,"POWO.REGIONS" ]

### performing the loop #######
##correcting pop.density values in the dataframe
# # GBIF_notgrilled$Population.density.2000 <- log(GBIF_notgrilled$Population.density.2000+1)
# # EVA_notgrilled_study_area$Population.density.2000 <- log(EVA_notgrilled_study_area$Population.density.2000+1)
# # GBIF_notgrilled_study_area$Population.density.2000 <- log(GBIF_notgrilled_study_area$Population.density.2000+1)
## correcting  grilled datasets pop.density
GBIF_grilled$Population.density.2000 <- log(GBIF_grilled$Population.density.2000+1)
EVA_study_area$Population.density.2000 <- log(EVA_study_area$Population.density.2000+1)
GBIF_grilled_study_area$Population.density.2000 <- log(GBIF_grilled_study_area$Population.density.2000+1)

### dataset i can remove 
rm(EVAE.70)
rm(EVAT.30)
rm(EVA)
rm(myexpl.var30_ST_DEF)
gc(reset=c("EVAE.70"))
gc(reset=c("EVAT.30"))
gc(reset=c("EVA"))
gc(reset=c("myexpl.var30_ST_DEF"))

#unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#gc(reset=T)
## ##system.time(
for( myRespName in sp.names[46:93]) {
  ### try parallel
  #create the cluster
#n.cores=2 ## per ora teniamoci bassi, pure perchè voglio fare solo due specie, già così mi inchioda parecchio
#  my.cluster <- parallel::makeCluster(
#    n.cores, 
#    type = "FORK") ## or more efficient FORK
#  
#  
#  #check cluster definition (optional)
#  print(my.cluster)
#  system.time(
#  x <- foreach(
#    myRespName = sp.names[1:2], 
#    .combine = 'c',
#    .packages = "ecospat",
#    .inorder=FALSE
#    
#  ) %dopar% {
 

  #myRespName <-  "Acacia.dealbata"#species
  
  
  cat("Start_" ,myRespName,'_elaboration')
  native_range <- Dataset[ Dataset[,myRespName]==1,"POWO.REGIONS" ]
 ## rownames(TDWG[TDWG[,myRespName]==1,]) ## così mi seleziono le regioni che mi servono: per ogni specie
  #points(GBIF_notgrilled[,c("longitude","Latitude")], pch=22, cex=0.1)
  #### IN THIS TEST, I ASSUMED THE ONLY NATIVE RANGE IS WESTERN AUSTRALIA AND THE NATIVE RANGE IS THE EUROPEAN BUFFER
  ## POTREI FARE UN BUFFER DI 0 METRI INTORNO ALLE ESOTICHE IN EVA ED UNO A 100 KM E UTILIZZARE QUEL BUFFER PER ANALIZZARE I DATI
  ## PERO' ALLORA DEVO FARE UN BUFFER PURE PER LE NATIVE. MEGLIO PER ORA NON FARE I BUFFER, PERò RIMANGO CON POCHE PRESENZE ALLORA.
  ## cut the global data (ENV and presences) according to the native's range
  ##    QUI FACCIO LA PROVA SENZA BUFFER, NATIVE AND INVADED SECCO CON LE VARIABILI CHE GIà HO, ASSUNTO NATIVE IS WESTERN AUSTRALIA
  #### HANDLING NATIVE ENV#####
  ## native environment
 native_env <- (mask(crop(global,POWO_regions[POWO_regions@data[["LEVEL3_NAM"]] %in% native_range ,]),POWO_regions[POWO_regions@data[["LEVEL3_NAM"]] %in% native_range ,])) ## rownames(TDWG[TDWG[,myRespName]==1,]) ## lo potrei pure aggiungere qui per avere una sola riga  di codice.
 plot(native_env[[1]])
 native_pres <- GBIF_grilled[GBIF_grilled[,myRespName]==1,c("Longitude","Latitude",ly.names.def, myRespName) ]
 native_pres<- cbind(native_pres[,],raster::extract(native_env[[1]], native_pres[,c("Longitude","Latitude")]))
 length(native_pres$`raster::extract(native_env[[1]], native_pres[, c("Longitude", `[!is.na(native_pres$`raster::extract(native_env[[1]], native_pres[, c("Longitude", `)]) ### 16744 values are not NA
 cat(myRespName,'Nr. presences in the native range=')
 print(sum(!is.na(native_pres$`raster::extract(native_env[[1]], native_pres[, c("Longitude", `)))
 cat("\n\n")
 
 ## eliminating presences outside of the native range
 native_pres<- native_pres[!is.na(native_pres$`raster::extract(native_env[[1]], native_pres[, c("Longitude", `),3:10]
 if(length(native_pres[,myRespName])>=5) {
   
 #cellStats(native_env[[1]], function(x,...) sum(!is.na(x)))
 #a<-xyFromCell(native_env[[1]], cell=ncell(native_env[[1]]))
  native_DF_env <- as.data.frame(rasterToPoints(native_env[[1]], fun=NULL, spatial=FALSE) )
 native_DF_env <- na.omit(native_DF_env)## eliminating the na's i have extracted
 
 #ifelse(dim(native_DF_env)[1]>15000000, print("A"),print("B"))
 if(dim(native_DF_env)[1]>6000000) {
   native_DF_env <- as.data.frame(sampleRandom( native_env, size=6000000,na.rm=T,xy=T))
   native_DF_env <- as.data.frame(native_DF_env)
   #colnames(native_DF_env) <-c("Longitude","Latitude","values")
 } else{
 ### SE SONO PI� DI 15M NE RISAMPLO SOLO 15 ALTRIMENTO LI PRENDO TUTTI 
 native_DF_env <- as.data.frame(rasterToPoints(native_env, fun=NULL, spatial=FALSE, na.rm=T) )
 native_DF_env <- na.omit(native_DF_env)## eliminating the na's i have extracted
 }
 
 
 
 ## adding the zero meaning it is a background env.value
 native_DF_env <- cbind(native_DF_env, rep(0,dim(native_DF_env)[1]))
 colnames(native_DF_env)[10] <- myRespName
  ## native presences (in teoria qui devi accingere da EVA e GBIF potenzialmente)
 ### L'appendice sulle differenze tra EVA e GBIF nell'area di studio ci vuole a sto punto
 ## posso usare il native env per selezionare le presenze di quella specie
  ## seleziono solo le presenze nel nativo
 ### seleziono GBIF presences in the native ranges
 
 ## eliminating presences outside of the native range
#### HOW MANY PRESENCES IN THE NATIVE RANGE??  
 ## little ceck old ceck
 #plot(native_env[[1]])
 #points(native_pres[!is.na(native_pres$`raster::extract(native_env[[1]], native_pres[, c("Longitude", `),c("Longitude","Latitude")], cex=0.1, pch=22)
  ## it seems ok
  #####HANDLING INVADED RANGE #####
  colnames(med_ENV)[11] <- myRespName ## adding the value of the species names in the black value of 0 
  
  #EVA_notgrilled_study_area <-  EVA_notgrilled_study_area [EVA_notgrilled_study_area[,myRespName]==1,c(ly.names.def, myRespName ) ]
  #GBIF_notgrilled_study_area<- GBIF_notgrilled_study_area[GBIF_notgrilled_study_area[,myRespName]==1,c(ly.names.def, myRespName ) ]
  ##### assembling dataset #####
  NATIVE <- rbind(native_pres, 
                  native_DF_env[,c(ly.names.def,myRespName)])
  INVADED <- rbind(EVA_study_area [EVA_study_area[,myRespName]==1,c(ly.names.def, myRespName ) ],
                   GBIF_grilled_study_area[GBIF_grilled_study_area[,myRespName]==1,c(ly.names.def, myRespName ) ],
                   med_ENV[,c(ly.names.def, myRespName) ])
  ### SE HO MENO DI 5 PRESENZE PER RANGE, IL LOOP SI INTERROMPE. NON PU� ESSERE CHE L'INVADED ABBIA MENO DI 5 PUNTI, MA PU� ESSERE CHE SUCCEDA PER L'INVADED

  
  ### cleaning a bit my dataset 
  rm(native_pres)
  rm(native_env)
  rm(native_DF_env)
  gc(reset=c("native_pres"))
  gc(reset=c("native_env"))
  gc(reset=c("native_DF_env"))
  
  #gc(reset=T)
  #rm()
  cat(myRespName,'Nr. presences in the native range=')
  print(sum(NATIVE[NATIVE[,myRespName]==1,]))
  cat("\n\n")
  
#  apply(NATIVE,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
#  apply(INVADED,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
  ### i may need to know the number of presences
  dim(NATIVE[NATIVE[,myRespName]==1,])[1] ## 25343
   cat(myRespName,'Nr. presences in the invaded range=')
  
 print( dim(INVADED[INVADED[,myRespName]==1,])[1] )## 6782
  cat("\n\n")
  
  #### STARTING THE NICHE ANALYSES
  cat('Completed_',myRespName,'dataset preparation')
  cat("\n\n")
  cat('Performing_PCA')
  cat("\n\n")
  
  
  pca.env<-dudi.pca(rbind(NATIVE[,1:7] , INVADED[,1:7]),scannf=FALSE,nf=2) 
  ### checking the PCA
  s.corcircle(pca.env$co[,1:2]) # 
  png(filename=file.path("../Seebens residence Time/Per diletta/Niche analyses results/",paste(myRespName,"PCA_axes.png")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  s.corcircle(pca.env$co[,1:2]) # 
  dev.off()
  
  #s.corcircle(pca.env$co[,c(1,3)]) 
  #ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
  # analisi delle componenti:
  # ricalcolo le percentuali delle pc:
  eigenvals <- as.data.frame(pca.env$eig)
  (eigenvals[1,]*100)/7 # PCA = 
  (eigenvals[2,]*100)/7       # 
  
  sum(c((eigenvals[1,]*100)/7,(eigenvals[2,]*100)/7  ))
  
  correl_pc_variables <- pca.env$co # aka coefficient of correlation between axes and variables.
  ## i may want to save it ??
  print(correl_pc_variables)
  write.csv(correl_pc_variables, file=file.path("../Seebens residence Time/Per diletta/Niche analyses results/",paste(myRespName,"Coefficient of correlation between axes.txt")))
  
  ### THIS WAS THE RBID OF THE GLOBAL AND LOCAL DATASET, BUT FOR MEMORY ISSUES I HAD TO DO THAT
  cat('Completed_',myRespName,'_PCA')
  cat("\n\n")
  
  scores.globclim<-pca.env$li #	PCA	scores	for	the	whole	study	area
  ###PCA scores for species presences
  scores.sp.nat<-suprow(pca.env,NATIVE[which(NATIVE[,myRespName]==1),1:7])$li #	PCA	scores	for	the	species	native	distribution
  scores.sp.inv<-suprow(pca.env,INVADED[which(INVADED[,myRespName]==1),1:7])$li #PCA	scores	for	the	species	invasive	distribution
  ## for the hole study area
  #### Global  ## 
  scores.clim.nat<-suprow(pca.env,NATIVE[,1:7])$li #	PCA	scores	for	the	whole	native	study	area
  # Europe
  scores.clim.inv<-suprow(pca.env,INVADED[,1:7])$li #	PCA	scores	for	the	whole	invaded	study area
  
  #	For	a	species	in	the	native	range	(global) ## GREEN
#  grid.clim.nat<-ecospat.grid.clim.dyn(glob=scores.globclim,glob1=scores.clim.nat, sp=scores.sp.nat, R=100, th.sp=0,th.env=0) ###  the value th.sp=0.01 was the same, i changed that to not have expantion etc
  ### Eliminated decimo quantile, perchè sono dati GBIF non ben definiti- sia dell'ambiente che delle presenze
  ## stima più conservativa
  #### QUELLO CHE HO CAPITO è CHE DEVO GIOCARE CON TH.SP
  
  #	For	a	species	in	the	invaded	range	(Europe) ## RED IN GENERAL
#  grid.clim.inv<-ecospat.grid.clim.dyn(glob=scores.globclim,glob1=scores.clim.inv, sp=scores.sp.inv, R=100, th.sp=0, th.env=0) ## these values should have the same settings
  
  #	For	a	species	in	the	native	range	(global) ## GREEN
  grid.clim.nat<-ecospat.grid.clim.dyn(glob=scores.globclim,glob1=scores.clim.nat, sp=scores.sp.nat, R=100, th.sp=0,th.env=0) ###  the value th.sp=0.01 was the same, i changed that to not have expantion etc
  ### Eliminated decimo quantile, perchè sono dati GBIF non ben definiti- sia dell'ambiente che delle presenze
  ## stima più conservativa
  #### QUELLO CHE HO CAPITO è CHE DEVO GIOCARE CON TH.SP
  
  #	For	a	species	in	the	invaded	range	(Europe) ## RED IN GENERAL
  grid.clim.inv<-ecospat.grid.clim.dyn(glob=scores.globclim,glob1=scores.clim.inv, sp=scores.sp.inv, R=100, th.sp=0, th.env=0) ## these values should have the same settings
  
  
  ecospat.niche.overlap (grid.clim.nat, grid.clim.inv, cor=TRUE)
  ## preforming some tests
  #	Perform	the	Niche	Equivalency	Test	with	ecospat.niche.equivalency.test()
##  #LONG TIME
##   eq.test<-ecospat.niche.equivalency.test(grid.clim.nat,
##                                           grid.clim.inv, rep=100, alternative = "greater", ncores = 1) ## attention at the core usage
##   eq.test$p.D 
##   
##  ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
##   
##  ##### capture.output(eq.test, file=file.path(myRespName, paste(myRespName,"GLOBAL_Niche Dynamic index.txt", sep="_")))
##   capture.output(eq.test, file=file.path("/Users/luigi/Desktop/Seebens residence Time/Per diletta/Niche analyses results",paste(myRespName,"Niche equivalency test.txt")))
  
  cat("\n\n")
   
   ### this script part that is up is looking if in the invaded areal i have a niche shift
  #  Similarity test
  #	shifting	randomly	the	invasive	niche	in	the	invaded	study	area
  
  cat('Performing',myRespName,'Similarity Test')
  
  sim.test<-ecospat.niche.similarity.test(grid.clim.nat,grid.clim.inv, rep=1000, alternative = "greater", rand.type = 2,ncores = 1) ## attention at the cores usage
  #	shifting	randomly	the	invasive	niche	in	the	invaded	study	area rand.type= 2: ho assumptions about a reference niche
  #ecospat.plot.overlap.test(sim.test, "D", "Similarity")
  cat("...")
  #	Plot	Similarity	test
#  png(filename=file.path(myRespName, paste(myRespName,"GLOBAL_Preliminary niche analyses overlap_Similarity.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  png(filename=file.path("../Seebens residence Time/Per diletta/Niche analyses results/",paste(myRespName,"Niche_SimilarityTest_result.png")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
   ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  dev.off()
  #	Delimiting	niche	categories	and	quantifying	niche	dynamics	in	analogue	climates	with
  a <- ecospat.niche.dyn.index (grid.clim.nat, grid.clim.inv, intersection=0) ## stability + expantion=100 plus unfilling  
  ## IN THE ECOSPAT SCRIPT IT WAS 0.1
  #intersection of 0.1 means that i have an overlap between the env.space excluded the 10 percentile
  a1 <- round(c(dim(NATIVE[NATIVE[,myRespName]==1,])[1],dim(INVADED[INVADED[,myRespName]==1,])[1],sum(c((eigenvals[1,]*100)/7,(eigenvals[2,]*100)/7  )),a$dynamic.index.w,sim.test$p.D,sim.test[["obs"]]$D),2) ## add here the equivalency test if i want to save it
  names(a1) <- c("Nr.presencesNATIVE","Nr.presencesINVADED","PCA-axes explained variability","expansion", "stability","unfilling" ,"sim.test_p-value", "Obs_D") ## add here the eq test i want to save
  
  write.csv(a1, file=file.path("../Seebens residence Time/Per diletta/Niche analyses results/",paste(myRespName,"Niche Dynamic index.txt")))

    cat("\n\n")
  
  ### the final plot i hadn't been able to interpret
  png(filename=file.path("../Seebens residence Time/Per diletta/Niche analyses results/",paste(myRespName,"Niche_Similarity.png")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #png(filename=file.path(myRespName, paste(myRespName,"GLOBAL_Preliminary niche analyses overlap.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  ecospat.plot.niche.dyn(grid.clim.nat, grid.clim.inv, quant=0,interest=2, title= "Niche Overlap", name.axis1="PC1",name.axis2="PC2")
  ### quantile of interest 
  ### IN BROENNIMANN FRAMEWORK IT WAS QUANT=0.1
  
  dev.off()    
  
  # save.image(paste(myRespName,"_Niche_comparison.Rdata")) i wasn't able to save image for these analyses, because it gave me an error. Probably it was to big
  cat('completed',myRespName,'Niche_comparison')
  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files ## i had to remove this, because it give me problems with the global raster
  #gc(reset=T)
  #rm(LOCAL)
  #rm(GLOBAL)
  
  ### THIS CAN BE A SEPARATE ANALYSES I DON'T THINK IT USES PCA RESULTS, NEITHER PRECEDENT GRILLING
  cat('Starting ',myRespName,'niche	dynamics	along	one	gradient')
  ##### TEMPERTAURE ### working on bio_10, in this example is the 1 in the script
  #	gridding	the	native	niche
   grid.clim.t.nat <- ecospat.grid.clim.dyn(glob=as.data.frame(rbind(NATIVE,INVADED)[,1]),
    glob1=as.data.frame(NATIVE[,1]),
    sp=as.data.frame(NATIVE[which(NATIVE[,myRespName]==1),1]), R=1000, th.sp=0)
  #	gridding	the	invaded	niche
   grid.clim.t.inv <- ecospat.grid.clim.dyn(glob=as.data.frame(rbind(NATIVE,INVADED)[,1]),
    glob1=as.data.frame(INVADED[,1]),
    sp=as.data.frame(INVADED[which(INVADED[,myRespName]==1),1]), R=1000, th.sp=0)
   
   t.dyn <- ecospat.niche.dyn.index (grid.clim.t.nat,grid.clim.t.inv, intersection=0)
   ecospat.plot.niche.dyn(grid.clim.t.nat, grid.clim.t.inv,
                           quant=0, interest=2, title= "Niche Overlap", name.axis1="Bio_07")
   
   ## For my self knowledge i will need to save these graphs! ##
   
   ##### Precipitations of the warmest quarter ### working on bio_17, in this example is the 1 in the script
   #	gridding	the	native	niche
 #  grid.clim.t.nat <- ecospat.grid.clim.dyn(glob=as.data.frame(rbind(NATIVE,INVADED)[,4]),
 #                                           glob1=as.data.frame(NATIVE[,4]),
 #                                           sp=as.data.frame(NATIVE[which(NATIVE[,myRespName]==1),4]), R=1000, th.sp=0)
 #  #	gridding	the	invaded	niche
 #  grid.clim.t.inv <- ecospat.grid.clim.dyn(glob=as.data.frame(rbind(NATIVE,INVADED)[,4]),
 #                                           glob1=as.data.frame(INVADED[,4]),
 #                                           sp=as.data.frame(INVADED[which(INVADED[,myRespName]==1),4]), R=1000, th.sp=0)
 #  
 #  t.dyn <- ecospat.niche.dyn.index (grid.clim.t.nat,grid.clim.t.inv, intersection=0)
 #  ecospat.plot.niche.dyn(grid.clim.t.nat, grid.clim.t.inv,
 #                         quant=0, interest=2, title= "Niche Overlap", name.axis1="BIO_17")
 #  
 #  
   ## For my self knowledge i will need to save these graphs! ##
   rm(NATIVE)
   rm(INVADED)
   rm(scores.clim.nat)
   rm(scores.clim.inv)
   rm(pca.env)
   
   gc(reset="NATIVE" )
   gc(reset="INVADED")
   gc(reset="scores.clim.nat")
   gc(reset="scores.clim.inv")
   gc(reset="pca.env")
  } else{
    ### SE AVEVO MENO DI 5 PUNTI NEL NATIVE SKYPPO TUTTO E ARRIVO DIRETTAMENTE QUI
    capture.output(length(native_pres[,myRespName]) ,file=file.path("../Seebens residence Time/Per diletta/Niche analyses results/",paste(myRespName,"A_no-enough nativr pres.txt")))
    
  } #  write.csv(a1, file=file.path("../Seebens residence Time/Per diletta/Niche analyses results/",paste(myRespName,"Niche Dynamic index.txt")))

}

 # stop clusters
 # parallel::stopCluster(cl = my.cluster)
  
## time for the first two not parallel: 844.145
  ## elaboration time for parallel: 719, ma comunque non ha fatto parallel
  ## se fatto parallel da 603.9
