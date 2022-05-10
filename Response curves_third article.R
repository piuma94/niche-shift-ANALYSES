
### fisso casa
setwd("E:\\New test with laure")
#setwd("C:\\Users\\luigi\\OneDrive - Universita degli Studi Roma Tre\\New test with laure") ##one drive fisso casa 
### uni: 
setwd("H:\\New test with laure")
#setwd("C:\\Users\\labEcologia\\OneDrive - Universita degli Studi Roma Tre\\Seebens residence Time\\Per diletta") ##one drive fisso casa 

## Setting the environment ###
## for windows
#memory.limit(size=76000)
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
#install.packages('SDMTools')
#library(SDMTools)
library(readxl)


### colors for the biomes
#col2hex(c("aquamarine4", "forestgreen","gold1","firebrick2",  "darkgoldenrod1","burlywood3","yellow3","darkolivegreen2", "steelblue4","lightsalmon2", "mediumaquamarine", "deeppink2", "darkseagreen2", "cyan3", "red"))
### better candidates: stellblue4, lightsalmon2/3, forestgreen



#rm(list = ls(all.names = TRUE))
#load eva to have teh performed species models
load("EVAT.30.Rda")

sp.names <- colnames(EVAT.30[,5:97])
rm(EVAT.30)

##### i need to: 
# 1) upload the Environmental variables
# 2) upload the model LOCAL or GLOBAL 
# 3) plot the responce curve only for one predictor, in the range of the variable and keep all other values to their mean
# I THINK IT'S BETTER TO PROJECT THE MODEL IN THE RANGE IN WHICH IT WAS TRAINED, SO MED BUFFER AND GLOBAL BUFFER, IN ORDER TO HAVE A GOOD RESPONCE
# THAT HOWEVER DIDN'T LET ME COMPARE THE CURVES, GLOBAL HAS A GREATER RANGE. 
# THE TWO MODELS WILL BE ANALYSED SEPARATELY

##### For a first try, that is however possibly becaming def. i used the BKG to define the area in which i want to project my model #######
#load("GLOBAL_BKG.Rda")
### this is for the global BKG to create the fake dataset. 
## global unused variables
### THIS HAS BEEN DONE, IT SHOULD BE DONE ONLY ONE
## ## load("ly.names.def")
## ## global<- stack("myexpl.var30_Global_used.tif")
## ## #names(global) <- ly.names.def
## ## plot(global[[1]]) ## this is the one, i visully checked it 
## ## # global [[6]]<-log(global [[6]]+1) ### i can both do or not this line, however, then i need to change
## ## names(global) <- ly.names.def
## ## ### sampling it randomly.
## ## GBIF_BKG<-sampleRandom(  global, size=1500000,na.rm=T,xy=T)
## ## ## renaming the first two columns
## ## colnames(GBIF_BKG)[1]<-"Longitude"
## ## colnames(GBIF_BKG)[2]<-"Latitude"

### done once, saving it 
#save(GBIF_BKG,file="GBIF_BKG_2Msampled_4resp.curv.Rda")

#GBIF_BKG_ENV <- as.data.frame(cbind(GBIF_BKG,raster::extract(global, GBIF_BKG[ ,c("Longitude","Latitude")],  buffer=NULL, cellnumbers=T)))
#save(GBIF_BKG_ENV,file="GBIF_BKG_ENV_2Msampled_4resp.curv.Rda")


## reloading it:
load("GBIF_BKG_2Msampled_4resp.curv.Rda")
#load("GBIF_BKG_ENV_2Msampled_4resp.curv.Rda")
### these are for the Local BKG. to create the fake data.frame. 
#load("LOCAL_BKG.Rda")
load("LOCAL_BKG_NICHE_AN.Rda") ## this is the newly evaluated

#load("LOCAL_EVAGBIFBKG.Rda")
## not usign local in the local buffer, but local BKG in the LOCAL BUFFER from EVA+GBIF
###Local_BKG<-EVA_BKG_Weigth ## this one has been done only in the buffer around EVA Presences
#Local_BKG <- EVA_BKG_Weigth ## this has been done using the EXTENT of EVA to cut GBIF and then do a buffer with both
#rm(EVA_BKG_Weigth)
MEDEU.DF <- EVA_BKG_Weigth[1:1000000,c(1,2,8:14)]
GLOBAL_buff<- as.data.frame(GBIF_BKG[1:1000000,])
rm(GBIF_BKG)
rm(EVA_BKG_Weigth)
########## PREPARING THE LOCAL DATAFRAMES ########
###
### DATAFRAME FOR BIO 07: RANGE OF BIO_07 ALL THE OTHER AT THEIR MEAN
#DF_bio_07
DF_bio_07<- cbind(MEDEU.DF[1:length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_07)),max(na.omit(MEDEU.DF$CHELSA_bio10_07)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
seq(min(na.omit(MEDEU.DF$CHELSA_bio10_07)),max(na.omit(MEDEU.DF$CHELSA_bio10_07)), 0.1), ## my range of bio_07 con un passo di 0.1
rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_10) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_07)),max(na.omit(MEDEU.DF$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_16) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_07)),max(na.omit(MEDEU.DF$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_17) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_07)),max(na.omit(MEDEU.DF$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
rep(mean(na.omit(MEDEU.DF$BLDFIE_M) )                ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_07)),max(na.omit(MEDEU.DF$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
rep(mean(log(na.omit(MEDEU.DF$Population.density.2000)+1) ) ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_07)),max(na.omit(MEDEU.DF$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
rep(mean(na.omit(MEDEU.DF$TWI) )                     ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_07)),max(na.omit(MEDEU.DF$CHELSA_bio10_07)), 0.1)))) # mean of the other variables 

#DF_bio_10
DF_bio_10<- cbind(MEDEU.DF[1:length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_10)),max(na.omit(MEDEU.DF$CHELSA_bio10_10)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_07) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_10)),max(na.omit(MEDEU.DF$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  seq(min(na.omit(MEDEU.DF$CHELSA_bio10_10)),max(na.omit(MEDEU.DF$CHELSA_bio10_10)), 0.1), ## my range of bio_07 con un passo di 0.1
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_16) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_10)),max(na.omit(MEDEU.DF$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_17) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_10)),max(na.omit(MEDEU.DF$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$BLDFIE_M) )                ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_10)),max(na.omit(MEDEU.DF$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  rep(mean(log(na.omit(MEDEU.DF$Population.density.2000)+1) ) ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_10)),max(na.omit(MEDEU.DF$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$TWI) )                     ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_10)),max(na.omit(MEDEU.DF$CHELSA_bio10_10)), 0.1)))) # mean of the other variables 
#DF_bio_16
DF_bio_16<- cbind(MEDEU.DF[1:length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_16)),max(na.omit(MEDEU.DF$CHELSA_bio10_16)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_07) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_16)),max(na.omit(MEDEU.DF$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_10) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_16)),max(na.omit(MEDEU.DF$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  seq(min(na.omit(MEDEU.DF$CHELSA_bio10_16)),max(na.omit(MEDEU.DF$CHELSA_bio10_16)), 0.1), ## my range of bio_07 con un passo di 0.1
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_17) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_16)),max(na.omit(MEDEU.DF$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$BLDFIE_M) )                ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_16)),max(na.omit(MEDEU.DF$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  rep(mean(log(na.omit(MEDEU.DF$Population.density.2000)+1) )  ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_16)),max(na.omit(MEDEU.DF$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$TWI) )                     ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_16)),max(na.omit(MEDEU.DF$CHELSA_bio10_16)), 0.1)))) # mean of the other variables 
#DF_bio_17
DF_bio_17<- cbind(MEDEU.DF[1:length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_17)),max(na.omit(MEDEU.DF$CHELSA_bio10_17)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_07) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_17)),max(na.omit(MEDEU.DF$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_10) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_17)),max(na.omit(MEDEU.DF$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_16) )         ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_17)),max(na.omit(MEDEU.DF$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  seq(min(na.omit(MEDEU.DF$CHELSA_bio10_17)),max(na.omit(MEDEU.DF$CHELSA_bio10_17)), 0.1), ## my range of bio_07 con un passo di 0.1
                  rep(mean(na.omit(MEDEU.DF$BLDFIE_M) )                ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_17)),max(na.omit(MEDEU.DF$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  rep(mean(log(na.omit(MEDEU.DF$Population.density.2000)+1) ) ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_17)),max(na.omit(MEDEU.DF$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$TWI) )                     ,length(seq(min(na.omit(MEDEU.DF$CHELSA_bio10_17)),max(na.omit(MEDEU.DF$CHELSA_bio10_17)), 0.1)))) # mean of the other variables 

#DF_BLDFIE_M
DF_BLDFIE_M<- cbind(MEDEU.DF[1:length(seq(min(na.omit(MEDEU.DF$BLDFIE_M)),max(na.omit(MEDEU.DF$BLDFIE_M)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_07) )         ,length(seq(min(na.omit(MEDEU.DF$BLDFIE_M)),max(na.omit(MEDEU.DF$BLDFIE_M)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_10) )         ,length(seq(min(na.omit(MEDEU.DF$BLDFIE_M)),max(na.omit(MEDEU.DF$BLDFIE_M)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_16) )         ,length(seq(min(na.omit(MEDEU.DF$BLDFIE_M)),max(na.omit(MEDEU.DF$BLDFIE_M)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_17) )                ,length(seq(min(na.omit(MEDEU.DF$BLDFIE_M)),max(na.omit(MEDEU.DF$BLDFIE_M)), 0.1))), # mean of the other variables 
                  seq(min(na.omit(MEDEU.DF$BLDFIE_M)),max(na.omit(MEDEU.DF$BLDFIE_M)), 0.1), ## my range of bio_07 con un passo di 0.1
                  rep(mean(log(na.omit(MEDEU.DF$Population.density.2000)+1) )  ,length(seq(min(na.omit(MEDEU.DF$BLDFIE_M)),max(na.omit(MEDEU.DF$BLDFIE_M)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(MEDEU.DF$TWI) )                     ,length(seq(min(na.omit(MEDEU.DF$BLDFIE_M)),max(na.omit(MEDEU.DF$BLDFIE_M)), 0.1)))) # mean of the other variables 
#DF_Population.density.2000
DF_Population.density.2000<- cbind(MEDEU.DF[1:length(seq(min(log(na.omit(MEDEU.DF$Population.density.2000)+1)),max(log(na.omit(MEDEU.DF$Population.density.2000)+1)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                    rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_07) )         ,length(seq(min(log(na.omit(MEDEU.DF$Population.density.2000)+1)),max(log(na.omit(MEDEU.DF$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                    rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_10) )         ,length(seq(min(log(na.omit(MEDEU.DF$Population.density.2000)+1)),max(log(na.omit(MEDEU.DF$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                    rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_16) )         ,length(seq(min(log(na.omit(MEDEU.DF$Population.density.2000)+1)),max(log(na.omit(MEDEU.DF$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                    rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_17) )                ,length(seq(min(log(na.omit(MEDEU.DF$Population.density.2000)+1)),max(log(na.omit(MEDEU.DF$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                    rep(mean(na.omit(MEDEU.DF$BLDFIE_M) )                ,length(seq(min(log(na.omit(MEDEU.DF$Population.density.2000)+1)),max(log(na.omit(MEDEU.DF$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                    seq(min(log(na.omit(MEDEU.DF$Population.density.2000)+1)),max(log(na.omit(MEDEU.DF$Population.density.2000)+1)), 0.1), ## my range of bio_07 con un passo di 0.1
                    rep(mean(na.omit(MEDEU.DF$TWI) )                     ,length(seq(min(log(na.omit(MEDEU.DF$Population.density.2000)+1)),max(log(na.omit(MEDEU.DF$Population.density.2000)+1)), 0.1)))) # mean of the other variables 

#DF_TWI
DF_TWI<- cbind(MEDEU.DF[1:length(seq(min(na.omit(MEDEU.DF$TWI)),max(na.omit(MEDEU.DF$TWI)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                                   rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_07) )         ,length(seq(min(na.omit(MEDEU.DF$TWI)),max(na.omit(MEDEU.DF$TWI)), 0.1))), # mean of the other variables 
                                   rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_10) )         ,length(seq(min(na.omit(MEDEU.DF$TWI)),max(na.omit(MEDEU.DF$TWI)), 0.1))), # mean of the other variables 
                                   rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_16) )         ,length(seq(min(na.omit(MEDEU.DF$TWI)),max(na.omit(MEDEU.DF$TWI)), 0.1))), # mean of the other variables 
                                   rep(mean(na.omit(MEDEU.DF$CHELSA_bio10_17) )                ,length(seq(min(na.omit(MEDEU.DF$TWI)),max(na.omit(MEDEU.DF$TWI)), 0.1))), # mean of the other variables 
                                   rep(mean(na.omit(MEDEU.DF$BLDFIE_M) ) ,length(seq(min(na.omit(MEDEU.DF$TWI)),max(na.omit(MEDEU.DF$TWI)), 0.1))), # mean of the other variables 
                                   rep(mean(log(na.omit(MEDEU.DF$Population.density.2000)+1) )                 ,length(seq(min(na.omit(MEDEU.DF$TWI)),max(na.omit(MEDEU.DF$TWI)), 0.1))), # mean of the other variables 
               
                                   seq(min(na.omit(MEDEU.DF$TWI)),max(na.omit(MEDEU.DF$TWI)), 0.1)) ## my range of bio_07 con un passo di 0.1



### changing colnames values
colnames(DF_bio_07                 ) <- colnames(MEDEU.DF) # change colnames
colnames(DF_bio_10                 ) <- colnames(MEDEU.DF) # change colnames
colnames(DF_bio_16                 ) <- colnames(MEDEU.DF) # change colnames
colnames(DF_bio_17                 ) <- colnames(MEDEU.DF) # change colnames
colnames(DF_BLDFIE_M               ) <- colnames(MEDEU.DF) # change colnames
colnames(DF_Population.density.2000) <- colnames(MEDEU.DF) # change colnames
colnames(DF_TWI                    ) <- colnames(MEDEU.DF) # change colnames



## the output 
### Uploading the inizializers that are also the results i will have in the end
load(file="LOCAL_ResponceCurve_bio_07.Rda")
load(file="LOCAL_ResponceCurve_bio_10.Rda")
load(file="LOCAL_ResponceCurve_bio_16.Rda")
load(file="LOCAL_ResponceCurve_bio_17.Rda")
load(file="LOCAL_ResponceCurve_BLDFIE_M.Rda")
load(file="LOCAL_ResponceCurve_pop.dens.Rda")
load(file="LOCAL_ResponceCurve_TWI.Rda"     )

LOCAL_ResponceCurve_bio_07  <-LOCAL_ResponceCurve_bio_07  [,1]
LOCAL_ResponceCurve_bio_10  <-LOCAL_ResponceCurve_bio_10  [,1]
LOCAL_ResponceCurve_bio_16  <-LOCAL_ResponceCurve_bio_16  [,1]
LOCAL_ResponceCurve_bio_17  <-LOCAL_ResponceCurve_bio_17  [,1]
LOCAL_ResponceCurve_BLDFIE_M<-LOCAL_ResponceCurve_BLDFIE_M[,1]
LOCAL_ResponceCurve_pop.dens<-LOCAL_ResponceCurve_pop.dens[,1]
LOCAL_ResponceCurve_TWI     <-LOCAL_ResponceCurve_TWI     [,1]

### if i will have to re_do it, i have to take only the first two columns

### What am i doing ?
# THE AIM IS TO PREPARE A DATASET OF RESPONCE CURVES FOR EACH SPECIES RESPONCE CURVE
# I PREPARED BEFORE IN THE SCRIPT THE DATASETS IN WHICH I HAVE THE MEAN OF ALL VARIABLES EXEPT FOR THE ONE I WANT TO PREDICT
# nella pratica, ho preparato i dataset e caricato gli inizializer, in modo da aggiungere colonne al mio dataset iniziale

####### PERFORMING  THE LOCAL LOOP   ########
### few species did not have the invaded range model, because they have not at least five presences
for (sp in sp.names[-7]) {
  #sp <- sp.names[c(7)]
  # load("Zea.mays _LOCAL.Rdata")
  ## loading the model for the species i am taking into account
  load(paste(sp, "_INVADEDRANGE.Rdata"))
  
  #grep('GLM', get_built_models(myBiomodEMLOCAL), value=TRUE)

## BIO_07
a_bio_07 <- BIOMOD_EnsembleForecasting( myBiomodEMLOCAL,
                            #projection.output = NULL,
                            new.env = as.matrix(DF_bio_07[,3:9]),
                            xy.new.env = as.matrix(DF_bio_07[,1:2]),
                            selected.models = 'all',
                            proj.name = "responce curve_bio_07",
                           # prob.cv = FALSE, ## questo non funziona per escludere il cv
                           #selected.models =  get_built_models(myBiomodEMLOCAL)[3],
                            #binary.meth = NULL,
                            #filtered.meth = NULL,
                            output.format = ".img"
                            )

## BIO_10
a_bio_10 <- BIOMOD_EnsembleForecasting( myBiomodEMLOCAL,
                                        #projection.output = NULL,
                                        new.env = as.matrix(DF_bio_10[,3:9]),
                                        xy.new.env = as.matrix(DF_bio_10[,1:2]),
                                        selected.models = 'all',
                                        proj.name = "responce curve_bio_10",
                                        #binary.meth = NULL,
                                        #filtered.meth = NULL,
                                        output.format = ".img"
)
## DF_bio_16
a_bio_16 <- BIOMOD_EnsembleForecasting( myBiomodEMLOCAL,
                                        #projection.output = NULL,
                                        new.env = as.matrix(DF_bio_16[,3:9]),
                                        xy.new.env = as.matrix(DF_bio_16[,1:2]),
                                        selected.models = 'all',
                                        proj.name = "responce curve_bio_16",
                                        #binary.meth = NULL,
                                        #filtered.meth = NULL,
                                        output.format = ".img"
)
## DF_bio_17
a_bio_17 <- BIOMOD_EnsembleForecasting( myBiomodEMLOCAL,
                                        #projection.output = NULL,
                                        new.env = as.matrix(DF_bio_17[,3:9]),
                                        xy.new.env = as.matrix(DF_bio_17[,1:2]),
                                        selected.models = 'all',
                                        proj.name = "responce curve_bio_17",
                                        #binary.meth = NULL,
                                        #filtered.meth = NULL,
                                        output.format = ".img"
)
## DF_BLDFIE_M
a_BLDFIE_M <- BIOMOD_EnsembleForecasting( myBiomodEMLOCAL,
                                        #projection.output = NULL,
                                        new.env = as.matrix(DF_BLDFIE_M[,3:9]),
                                        xy.new.env = as.matrix(DF_BLDFIE_M[,1:2]),
                                        selected.models = 'all',
                                        proj.name = "responce curve_BLDFIE",
                                        #binary.meth = NULL,
                                        #filtered.meth = NULL,
                                        output.format = ".img"
)
## DF_Population.density.2000
a_Population.density.2000 <- BIOMOD_EnsembleForecasting( myBiomodEMLOCAL,
                                        #projection.output = NULL,
                                        new.env = as.matrix(DF_Population.density.2000[,3:9]),
                                        xy.new.env = as.matrix(DF_Population.density.2000[,1:2]),
                                        selected.models = 'all',
                                        proj.name = "responce curve_popDens",
                                        #binary.meth = NULL,
                                        #filtered.meth = NULL,
                                        output.format = ".img"
)
# DF_TWI
a_TWI <- BIOMOD_EnsembleForecasting( myBiomodEMLOCAL,
                                                         #projection.output = NULL,
                                                         new.env = as.matrix(DF_TWI[,3:9]),
                                                         xy.new.env = as.matrix(DF_TWI[,1:2]),
                                                         selected.models = 'all',
                                                         proj.name = "responce curve_TWI",
                                                         #binary.meth = NULL,
                                                         #filtered.meth = NULL,
                                                         output.format = ".img"
)
## Extracting from the elaborated model my predictions
aa_bio_07                  <- as.data.frame(a_bio_07@proj@val) 
aa_bio_10                  <- as.data.frame(a_bio_10@proj@val) 
aa_bio_16                  <- as.data.frame(a_bio_16@proj@val) 
aa_bio_17                  <- as.data.frame(a_bio_17@proj@val) 
aa_BLDFIE_M                <- as.data.frame(a_BLDFIE_M@proj@val) 
aa_Population.density.2000 <- as.data.frame(a_Population.density.2000@proj@val) 
aa_TWI                     <- as.data.frame(a_TWI@proj@val) 
 
#### INIZIALIZER PREPARATION done only one 
### preparing the dataset to elaborate the values in a loop
# inizialiser
## LOCAL_ResponceCurve_bio_07   <- aa_bio_07                 [,c(3)]
## LOCAL_ResponceCurve_bio_10   <- aa_bio_10                 [,c(3)]
## LOCAL_ResponceCurve_bio_16   <- aa_bio_16                 [,c(3)]
## LOCAL_ResponceCurve_bio_17   <- aa_bio_17                 [,c(3)]
## LOCAL_ResponceCurve_BLDFIE_M <- aa_BLDFIE_M               [,c(3)]
## LOCAL_ResponceCurve_pop.dens <- aa_Population.density.2000[,c(3)]
## LOCAL_ResponceCurve_TWI <-      aa_TWI                    [,c(3)]
## # that way, i make the columns neglected, so i will not have them back during the future analyses
## names(LOCAL_ResponceCurve_bio_07  ) <- c("a")              ## this is not necessary
## names(LOCAL_ResponceCurve_bio_10  ) <- c("a")
## names(LOCAL_ResponceCurve_bio_16  ) <- c("a")
## names(LOCAL_ResponceCurve_bio_17  ) <- c("a")
## names(LOCAL_ResponceCurve_BLDFIE_M) <- c("a")
## names(LOCAL_ResponceCurve_pop.dens) <- c("a")
## names(LOCAL_ResponceCurve_TWI      ) <- c("a")
##  #saving it 
##  save(LOCAL_ResponceCurve_bio_07 , file="LOCAL_ResponceCurve_bio_07.Rda")
##  save(LOCAL_ResponceCurve_bio_10 , file="LOCAL_ResponceCurve_bio_10.Rda")
##  save(LOCAL_ResponceCurve_bio_16 , file="LOCAL_ResponceCurve_bio_16.Rda")
##  save(LOCAL_ResponceCurve_bio_17 , file="LOCAL_ResponceCurve_bio_17.Rda")
##  save(LOCAL_ResponceCurve_BLDFIE_M,file="LOCAL_ResponceCurve_BLDFIE_M.Rda")
##  save(LOCAL_ResponceCurve_pop.dens,file="LOCAL_ResponceCurve_pop.dens.Rda")
##  save(LOCAL_ResponceCurve_TWI    , file="LOCAL_ResponceCurve_TWI.Rda"     )


LOCAL_ResponceCurve_bio_07  <- cbind(LOCAL_ResponceCurve_bio_07  ,aa_bio_07                 [,c(3)])
LOCAL_ResponceCurve_bio_10  <- cbind(LOCAL_ResponceCurve_bio_10  ,aa_bio_10                 [,c(3)])
LOCAL_ResponceCurve_bio_16  <- cbind(LOCAL_ResponceCurve_bio_16  ,aa_bio_16                 [,c(3)])
LOCAL_ResponceCurve_bio_17  <- cbind(LOCAL_ResponceCurve_bio_17  ,aa_bio_17                 [,c(3)])
LOCAL_ResponceCurve_BLDFIE_M<- cbind(LOCAL_ResponceCurve_BLDFIE_M,aa_BLDFIE_M               [,c(3)])
LOCAL_ResponceCurve_pop.dens<- cbind(LOCAL_ResponceCurve_pop.dens,aa_Population.density.2000[,c(3)])
LOCAL_ResponceCurve_TWI     <- cbind(LOCAL_ResponceCurve_TWI     ,aa_TWI                    [,c(3)])
ls(pattern="my")
ls(pattern=sp)
ls(pattern="EF")
rm(list=ls(pattern="GAM"))
rm(list=ls(pattern="GLM"))
rm(list=ls(pattern=sp))

gc(reset=T)


}

#### before saving i have to add the x_range values, that derive from the corresponding value of the dataset range of values
LOCAL_ResponceCurve_bio_07  <- cbind(LOCAL_ResponceCurve_bio_07  ,DF_bio_07$CHELSA_bio10_07  )              
LOCAL_ResponceCurve_bio_10  <- cbind(LOCAL_ResponceCurve_bio_10  ,DF_bio_10$CHELSA_bio10_10  )              
LOCAL_ResponceCurve_bio_16  <- cbind(LOCAL_ResponceCurve_bio_16  ,DF_bio_16$CHELSA_bio10_16  )               
LOCAL_ResponceCurve_bio_17  <- cbind(LOCAL_ResponceCurve_bio_17  ,DF_bio_17$CHELSA_bio10_17  )               
LOCAL_ResponceCurve_BLDFIE_M<- cbind(LOCAL_ResponceCurve_BLDFIE_M,DF_BLDFIE_M$BLDFIE_M )              
LOCAL_ResponceCurve_pop.dens<- cbind(LOCAL_ResponceCurve_pop.dens,DF_Population.density.2000$Population.density.2000)
LOCAL_ResponceCurve_TWI     <- cbind(LOCAL_ResponceCurve_TWI     ,DF_TWI$TWI )                   
                    

#### eliminating the inizializer
LOCAL_ResponceCurve_bio_07  <- as.data.frame(LOCAL_ResponceCurve_bio_07  )[,-1]
LOCAL_ResponceCurve_bio_10  <- as.data.frame(LOCAL_ResponceCurve_bio_10  )[,-1]
LOCAL_ResponceCurve_bio_16  <- as.data.frame(LOCAL_ResponceCurve_bio_16  )[,-1]
LOCAL_ResponceCurve_bio_17  <- as.data.frame(LOCAL_ResponceCurve_bio_17  )[,-1]
LOCAL_ResponceCurve_BLDFIE_M<- as.data.frame(LOCAL_ResponceCurve_BLDFIE_M)[,-1]
LOCAL_ResponceCurve_pop.dens<- as.data.frame(LOCAL_ResponceCurve_pop.dens)[,-1]
LOCAL_ResponceCurve_TWI     <- as.data.frame(LOCAL_ResponceCurve_TWI     )[,-1]

colnames(LOCAL_ResponceCurve_bio_07  ) <- c(sp.names[-7], "CHELSA_bio10_07")
colnames(LOCAL_ResponceCurve_bio_10  ) <- c(sp.names[-7], "CHELSA_bio10_10")
colnames(LOCAL_ResponceCurve_bio_16  ) <- c(sp.names[-7], "CHELSA_bio10_16")
colnames(LOCAL_ResponceCurve_bio_17  ) <- c(sp.names[-7], "CHELSA_bio10_17")
colnames(LOCAL_ResponceCurve_BLDFIE_M) <- c(sp.names[-7], "BLDFIE_M")
colnames(LOCAL_ResponceCurve_pop.dens) <- c(sp.names[-7], "Population.density.2000")
colnames(LOCAL_ResponceCurve_TWI     ) <- c(sp.names[-7], "TWI")

### after the loop i can finally save it
# saving it ## do it with carefull...... it takes long time to perform the analysis 
 save(LOCAL_ResponceCurve_bio_07  , file="LOCAL_ResponceCurve_bio_07.Rda")
 save(LOCAL_ResponceCurve_bio_10  , file="LOCAL_ResponceCurve_bio_10.Rda")
 save(LOCAL_ResponceCurve_bio_16  , file="LOCAL_ResponceCurve_bio_16.Rda")
 save(LOCAL_ResponceCurve_bio_17  , file="LOCAL_ResponceCurve_bio_17.Rda")
 save(LOCAL_ResponceCurve_BLDFIE_M,file="LOCAL_ResponceCurve_BLDFIE_M.Rda")
 save(LOCAL_ResponceCurve_pop.dens,file="LOCAL_ResponceCurve_pop.dens.Rda")
 save(LOCAL_ResponceCurve_TWI     , file="LOCAL_ResponceCurve_TWI.Rda"     )



#### breve parentesi, adding amaranthus.albus ######
 
 ### all dataset for each responce curve LOCAL MODEL
 load("LOCAL_ResponceCurve_bio_07.Rda")#LOCAL_ResponceCurve_bio_07 , 
 load("LOCAL_ResponceCurve_bio_10.Rda")#LOCAL_ResponceCurve_bio_10 , 
 load("LOCAL_ResponceCurve_bio_16.Rda")#LOCAL_ResponceCurve_bio_16 , 
 load("LOCAL_ResponceCurve_bio_17.Rda")#LOCAL_ResponceCurve_bio_17 , 
 load("LOCAL_ResponceCurve_BLDFIE_M.Rda")#LOCAL_ResponceCurve_BLDFIE_M,
 load("LOCAL_ResponceCurve_pop.dens.Rda")#LOCAL_ResponceCurve_pop.dens,
 load("LOCAL_ResponceCurve_TWI.Rda"     )#LOCAL_ResponceCurve_TWI    , 
 
 sp<-"Amaranthus.albus"
 load(paste(sp, "_INVADEDRANGE.Rdata"))
 
 #grep('GLM', get_built_models(myBiomodEMLOCAL), value=TRUE)
 
 ## BIO_07
 a_bio_07 <- BIOMOD_Projection( myBiomodModelOutLOCAL,
                                         #projection.output = NULL,
                                         new.env = as.matrix(DF_bio_07[,3:9]),
                                         xy.new.env = as.matrix(DF_bio_07[,1:2]),
                                         #selected.models = 'all',
                                         proj.name = "responce curve_bio_07",
                                         # prob.cv = FALSE, ## questo non funziona per escludere il cv
                                         selected.models =  get_built_models(myBiomodModelOutLOCAL)[17], ### here i know I only want this model. it can be difficult for the global
                                         #binary.meth = NULL,
                                         #filtered.meth = NULL,
                                         #output.format = ".img"
 )
 
 ## BIO_10
 a_bio_10 <- BIOMOD_Projection( myBiomodModelOutLOCAL,
                                         #projection.output = NULL,
                                         new.env = as.matrix(DF_bio_10[,3:9]),
                                         xy.new.env = as.matrix(DF_bio_10[,1:2]),
                                         selected.models =  get_built_models(myBiomodModelOutLOCAL)[17], ### here i know I only want this model. it can be difficult for the global
                                         proj.name = "responce curve_bio_10",
                                         #binary.meth = NULL,
                                         #filtered.meth = NULL,
                                         #output.format = ".img"
 )
 ## DF_bio_16
 a_bio_16 <- BIOMOD_Projection( myBiomodModelOutLOCAL,
                                         #projection.output = NULL,
                                         new.env = as.matrix(DF_bio_16[,3:9]),
                                         xy.new.env = as.matrix(DF_bio_16[,1:2]),
                                         selected.models =  get_built_models(myBiomodModelOutLOCAL)[17], ### here i know I only want this model. it can be difficult for the global
                                         proj.name = "responce curve_bio_16",
                                         #binary.meth = NULL,
                                         #filtered.meth = NULL,
                                         #output.format = ".img"
 )
 ## DF_bio_17
 a_bio_17 <- BIOMOD_Projection( myBiomodModelOutLOCAL,
                                         #projection.output = NULL,
                                         new.env = as.matrix(DF_bio_17[,3:9]),
                                         xy.new.env = as.matrix(DF_bio_17[,1:2]),
                                         selected.models =  get_built_models(myBiomodModelOutLOCAL)[17], ### here i know I only want this model. it can be difficult for the global
                                         proj.name = "responce curve_bio_17",
                                         #binary.meth = NULL,
                                         #filtered.meth = NULL,
                                         #output.format = ".img"
 )
 ## DF_BLDFIE_M
 a_BLDFIE_M <- BIOMOD_Projection( myBiomodModelOutLOCAL,
                                           #projection.output = NULL,
                                           new.env = as.matrix(DF_BLDFIE_M[,3:9]),
                                           xy.new.env = as.matrix(DF_BLDFIE_M[,1:2]),
                                           selected.models =  get_built_models(myBiomodModelOutLOCAL)[17], ### here i know I only want this model. it can be difficult for the global
                                           proj.name = "responce curve_BLDFIE",
                                           #binary.meth = NULL,
                                           #filtered.meth = NULL,
                                           #output.format = ".img"
 )
 ## DF_Population.density.2000
 a_Population.density.2000 <- BIOMOD_Projection( myBiomodModelOutLOCAL,
                                                          #projection.output = NULL,
                                                          new.env = as.matrix(DF_Population.density.2000[,3:9]),
                                                          xy.new.env = as.matrix(DF_Population.density.2000[,1:2]),
                                                          selected.models =  get_built_models(myBiomodModelOutLOCAL)[17], ### here i know I only want this model. it can be difficult for the global
                                                          proj.name = "responce curve_popDens",
                                                          #binary.meth = NULL,
                                                          #filtered.meth = NULL,
                                                          #output.format = ".img"
 )
 # DF_TWI
 a_TWI <- BIOMOD_Projection( myBiomodModelOutLOCAL,
                                      #projection.output = NULL,
                                      new.env = as.matrix(DF_TWI[,3:9]),
                                      xy.new.env = as.matrix(DF_TWI[,1:2]),
                                      selected.models =  get_built_models(myBiomodModelOutLOCAL)[17], ### here i know I only want this model. it can be difficult for the global
                                      proj.name = "responce curve_TWI",
                                      #binary.meth = NULL,
                                      #filtered.meth = NULL,
                                     #output.format = ".img"
 )
 ## Extracting from the elaborated model my predictions
 aa_bio_07                  <- as.data.frame(a_bio_07@proj@val) 
 aa_bio_10                  <- as.data.frame(a_bio_10@proj@val) 
 aa_bio_16                  <- as.data.frame(a_bio_16@proj@val) 
 aa_bio_17                  <- as.data.frame(a_bio_17@proj@val) 
 aa_BLDFIE_M                <- as.data.frame(a_BLDFIE_M@proj@val) 
 aa_Population.density.2000 <- as.data.frame(a_Population.density.2000@proj@val) 
 aa_TWI                     <- as.data.frame(a_TWI@proj@val) 

 #### ading the amaranthus to the dataframe 
 ### I THINK IT WILL WORK, BUT CHECK IT
 LOCAL_ResponceCurve_bio_07  <- cbind(LOCAL_ResponceCurve_bio_07  [,1:6],aa_bio_07                 ,LOCAL_ResponceCurve_bio_07  [,7:dim(LOCAL_ResponceCurve_bio_07  )[2]])
 LOCAL_ResponceCurve_bio_10  <- cbind(LOCAL_ResponceCurve_bio_10  [,1:6],aa_bio_10                 ,LOCAL_ResponceCurve_bio_10  [,7:dim(LOCAL_ResponceCurve_bio_10  )[2]])
 LOCAL_ResponceCurve_bio_16  <- cbind(LOCAL_ResponceCurve_bio_16  [,1:6],aa_bio_16                 ,LOCAL_ResponceCurve_bio_16  [,7:dim(LOCAL_ResponceCurve_bio_16  )[2]])
 LOCAL_ResponceCurve_bio_17  <- cbind(LOCAL_ResponceCurve_bio_17  [,1:6],aa_bio_17                 ,LOCAL_ResponceCurve_bio_17  [,7:dim(LOCAL_ResponceCurve_bio_17  )[2]])
 LOCAL_ResponceCurve_BLDFIE_M<- cbind(LOCAL_ResponceCurve_BLDFIE_M[,1:6],aa_BLDFIE_M               ,LOCAL_ResponceCurve_BLDFIE_M[,7:dim(LOCAL_ResponceCurve_BLDFIE_M)[2]])
 LOCAL_ResponceCurve_pop.dens<- cbind(LOCAL_ResponceCurve_pop.dens[,1:6],aa_Population.density.2000,LOCAL_ResponceCurve_pop.dens[,7:dim(LOCAL_ResponceCurve_pop.dens)[2]])
 LOCAL_ResponceCurve_TWI     <- cbind(LOCAL_ResponceCurve_TWI     [,1:6],aa_TWI                    ,LOCAL_ResponceCurve_TWI     [,7:dim(LOCAL_ResponceCurve_TWI     )[2]])
 ls(pattern="my")
 ls(pattern=sp)
 ls(pattern="EF")
 rm(list=ls(pattern="GAM"))
 rm(list=ls(pattern="GLM"))
 rm(list=ls(pattern=sp))
 
 gc(reset=T)
 ### renaming the columns 
 colnames(LOCAL_ResponceCurve_bio_07  ) <- c(sp.names[], "CHELSA_bio10_07")
 colnames(LOCAL_ResponceCurve_bio_10  ) <- c(sp.names[], "CHELSA_bio10_10")
 colnames(LOCAL_ResponceCurve_bio_16  ) <- c(sp.names[], "CHELSA_bio10_16")
 colnames(LOCAL_ResponceCurve_bio_17  ) <- c(sp.names[], "CHELSA_bio10_17")
 colnames(LOCAL_ResponceCurve_BLDFIE_M) <- c(sp.names[], "BLDFIE_M")
 colnames(LOCAL_ResponceCurve_pop.dens) <- c(sp.names[], "Population.density.2000")
 colnames(LOCAL_ResponceCurve_TWI     ) <- c(sp.names[], "TWI")
 
 ### saving the name 
 
 save(LOCAL_ResponceCurve_bio_07  , file="LOCAL_ResponceCurve_bio_07.Rda")
 save(LOCAL_ResponceCurve_bio_10  , file="LOCAL_ResponceCurve_bio_10.Rda")
 save(LOCAL_ResponceCurve_bio_16  , file="LOCAL_ResponceCurve_bio_16.Rda")
 save(LOCAL_ResponceCurve_bio_17  , file="LOCAL_ResponceCurve_bio_17.Rda")
 save(LOCAL_ResponceCurve_BLDFIE_M,file="LOCAL_ResponceCurve_BLDFIE_M.Rda")
 save(LOCAL_ResponceCurve_pop.dens,file="LOCAL_ResponceCurve_pop.dens.Rda")
 save(LOCAL_ResponceCurve_TWI     , file="LOCAL_ResponceCurve_TWI.Rda"     )
 
## reordering columns now
### IT IS NOT NECESSARY FOR HOW I WORKED THE SCRIPT TO ADD THE COLUMNS NOW. 
## i should have saved it

#plot((aa_bio_07$Zea.mays_EMwmeanByROC_mergedAlgo_mergedRun_mergedData)/1000~DF_bio_07$CHELSA_bio10_07)
#plot((LOCAL_ResponceCurve_bio_07$Zea.mays_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData)/1000~LOCAL_ResponceCurve_bio_07)

## this to upload the projection??
# load(paste("Zea.mays//","proj_responce curve_bio_07//","proj_responce curve_bio_07_Zea.mays_ensemble.RData",sep = ""))

### but now how can i access the data?

a <- LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)]
#  ss<-mean(as.numeric(w[w$sp==species & w$scen %in% unique(grep(x=w$scen,patter="2050", value=T)) & w$scen %in% unique(grep(x=w$scen,patter="DISP_LIM", value=T)) & w$scen %in% unique(grep(x=w$scen,patter="45", value=T)),"SpeciesRangeChange"])) ## subsetting the species and the scenario to make the mean

########## PREPARING THE GLOBAL DATAFRAMES ########
###
### DATAFRAME FOR BIO 07: RANGE OF BIO_07 ALL THE OTHER AT THEIR MEAN
#DF_bio_07
DF_bio_07<- cbind(GLOBAL_buff[1:length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_07)),max(na.omit(GLOBAL_buff$CHELSA_bio10_07)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                  seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_07)),max(na.omit(GLOBAL_buff$CHELSA_bio10_07)), 0.1), ## my range of bio_07 con un passo di 0.1
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_10) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_07)),max(na.omit(GLOBAL_buff$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_16) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_07)),max(na.omit(GLOBAL_buff$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_17) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_07)),max(na.omit(GLOBAL_buff$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$BLDFIE_M) )                ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_07)),max(na.omit(GLOBAL_buff$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
                  rep(mean(log(na.omit(GLOBAL_buff$Population.density.2000)+1) ) ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_07)),max(na.omit(GLOBAL_buff$CHELSA_bio10_07)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$TWI) )                     ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_07)),max(na.omit(GLOBAL_buff$CHELSA_bio10_07)), 0.1)))) # mean of the other variables 

#DF_bio_10
DF_bio_10<- cbind(GLOBAL_buff[1:length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_10)),max(na.omit(GLOBAL_buff$CHELSA_bio10_10)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_07) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_10)),max(na.omit(GLOBAL_buff$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_10)),max(na.omit(GLOBAL_buff$CHELSA_bio10_10)), 0.1), ## my range of bio_07 con un passo di 0.1
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_16) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_10)),max(na.omit(GLOBAL_buff$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_17) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_10)),max(na.omit(GLOBAL_buff$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$BLDFIE_M) )                ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_10)),max(na.omit(GLOBAL_buff$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  rep(mean(log(na.omit(GLOBAL_buff$Population.density.2000)+1) ) ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_10)),max(na.omit(GLOBAL_buff$CHELSA_bio10_10)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$TWI) )                     ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_10)),max(na.omit(GLOBAL_buff$CHELSA_bio10_10)), 0.1)))) # mean of the other variables 
#DF_bio_16
DF_bio_16<- cbind(GLOBAL_buff[1:length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_16)),max(na.omit(GLOBAL_buff$CHELSA_bio10_16)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_07) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_16)),max(na.omit(GLOBAL_buff$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_10) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_16)),max(na.omit(GLOBAL_buff$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_16)),max(na.omit(GLOBAL_buff$CHELSA_bio10_16)), 0.1), ## my range of bio_07 con un passo di 0.1
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_17) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_16)),max(na.omit(GLOBAL_buff$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$BLDFIE_M) )                ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_16)),max(na.omit(GLOBAL_buff$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  rep(mean(log(na.omit(GLOBAL_buff$Population.density.2000)+1) ) ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_16)),max(na.omit(GLOBAL_buff$CHELSA_bio10_16)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$TWI) )                     ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_16)),max(na.omit(GLOBAL_buff$CHELSA_bio10_16)), 0.1)))) # mean of the other variables 
#DF_bio_17
DF_bio_17<- cbind(GLOBAL_buff[1:length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_17)),max(na.omit(GLOBAL_buff$CHELSA_bio10_17)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_07) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_17)),max(na.omit(GLOBAL_buff$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_10) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_17)),max(na.omit(GLOBAL_buff$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_16) )         ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_17)),max(na.omit(GLOBAL_buff$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_17)),max(na.omit(GLOBAL_buff$CHELSA_bio10_17)), 0.1), ## my range of bio_07 con un passo di 0.1
                  rep(mean(na.omit(GLOBAL_buff$BLDFIE_M) )                ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_17)),max(na.omit(GLOBAL_buff$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  rep(mean(log(na.omit(GLOBAL_buff$Population.density.2000)+1) ) ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_17)),max(na.omit(GLOBAL_buff$CHELSA_bio10_17)), 0.1))), # mean of the other variables 
                  rep(mean(na.omit(GLOBAL_buff$TWI) )                     ,length(seq(min(na.omit(GLOBAL_buff$CHELSA_bio10_17)),max(na.omit(GLOBAL_buff$CHELSA_bio10_17)), 0.1)))) # mean of the other variables 

#DF_BLDFIE_M
DF_BLDFIE_M<- cbind(GLOBAL_buff[1:length(seq(min(na.omit(GLOBAL_buff$BLDFIE_M)),max(na.omit(GLOBAL_buff$BLDFIE_M)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                    rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_07) )         ,length(seq(min(na.omit(GLOBAL_buff$BLDFIE_M)),max(na.omit(GLOBAL_buff$BLDFIE_M)), 0.1))), # mean of the other variables 
                    rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_10) )         ,length(seq(min(na.omit(GLOBAL_buff$BLDFIE_M)),max(na.omit(GLOBAL_buff$BLDFIE_M)), 0.1))), # mean of the other variables 
                    rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_16) )         ,length(seq(min(na.omit(GLOBAL_buff$BLDFIE_M)),max(na.omit(GLOBAL_buff$BLDFIE_M)), 0.1))), # mean of the other variables 
                    rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_17) )                ,length(seq(min(na.omit(GLOBAL_buff$BLDFIE_M)),max(na.omit(GLOBAL_buff$BLDFIE_M)), 0.1))), # mean of the other variables 
                    seq(min(na.omit(GLOBAL_buff$BLDFIE_M)),max(na.omit(GLOBAL_buff$BLDFIE_M)), 0.1), ## my range of bio_07 con un passo di 0.1
                    rep(mean(log(na.omit(GLOBAL_buff$Population.density.2000)+1) ),length(seq(min(na.omit(GLOBAL_buff$BLDFIE_M)),max(na.omit(GLOBAL_buff$BLDFIE_M)), 0.1))), # mean of the other variables 
                    rep(mean(na.omit(GLOBAL_buff$TWI) )                     ,length(seq(min(na.omit(GLOBAL_buff$BLDFIE_M)),max(na.omit(GLOBAL_buff$BLDFIE_M)), 0.1)))) # mean of the other variables 
#DF_Population.density.2000
DF_Population.density.2000<- cbind(GLOBAL_buff[1:length(seq(min(log(na.omit(GLOBAL_buff$Population.density.2000)+1)),max(log(na.omit(GLOBAL_buff$Population.density.2000)+1)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
                                   rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_07) )         ,length(seq(min(log(na.omit(GLOBAL_buff$Population.density.2000)+1)),max(log(na.omit(GLOBAL_buff$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                                   rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_10) )         ,length(seq(min(log(na.omit(GLOBAL_buff$Population.density.2000)+1)),max(log(na.omit(GLOBAL_buff$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                                   rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_16) )         ,length(seq(min(log(na.omit(GLOBAL_buff$Population.density.2000)+1)),max(log(na.omit(GLOBAL_buff$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                                   rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_17) )                ,length(seq(min(log(na.omit(GLOBAL_buff$Population.density.2000)+1)),max(log(na.omit(GLOBAL_buff$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                                   rep(mean(na.omit(GLOBAL_buff$BLDFIE_M) )                ,length(seq(min(log(na.omit(GLOBAL_buff$Population.density.2000)+1)),max(log(na.omit(GLOBAL_buff$Population.density.2000)+1)), 0.1))), # mean of the other variables 
                                   seq(min(log(na.omit(GLOBAL_buff$Population.density.2000)+1)),max(log(na.omit(GLOBAL_buff$Population.density.2000)+1)), 0.1), ## my range of bio_07 con un passo di 0.1
                                   rep(mean(na.omit(GLOBAL_buff$TWI) )                     ,length(seq(min(log(na.omit(GLOBAL_buff$Population.density.2000)+1)),max(log(na.omit(GLOBAL_buff$Population.density.2000)+1)), 0.1)))) # mean of the other variables 

#DF_TWI
DF_TWI<- cbind(GLOBAL_buff[1:length(seq(min(na.omit(GLOBAL_buff$TWI)),max(na.omit(GLOBAL_buff$TWI)), 0.1)),c(1,2)], ## adding the coordinates for my new projections
               rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_07) )         ,length(seq(min(na.omit(GLOBAL_buff$TWI)),max(na.omit(GLOBAL_buff$TWI)), 0.1))), # mean of the other variables 
               rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_10) )         ,length(seq(min(na.omit(GLOBAL_buff$TWI)),max(na.omit(GLOBAL_buff$TWI)), 0.1))), # mean of the other variables 
               rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_16) )         ,length(seq(min(na.omit(GLOBAL_buff$TWI)),max(na.omit(GLOBAL_buff$TWI)), 0.1))), # mean of the other variables 
               rep(mean(na.omit(GLOBAL_buff$CHELSA_bio10_17) )                ,length(seq(min(na.omit(GLOBAL_buff$TWI)),max(na.omit(GLOBAL_buff$TWI)), 0.1))), # mean of the other variables 
               rep(mean(na.omit(GLOBAL_buff$BLDFIE_M) ) ,                   length(seq(min(na.omit(GLOBAL_buff$TWI)),max(na.omit(GLOBAL_buff$TWI)), 0.1))), # mean of the other variables 
               rep(mean(log(na.omit(GLOBAL_buff$Population.density.2000)+1) )                ,length(seq(min(na.omit(GLOBAL_buff$TWI)),max(na.omit(GLOBAL_buff$TWI)), 0.1))), # mean of the other variables 
               
               seq(min(na.omit(GLOBAL_buff$TWI)),max(na.omit(GLOBAL_buff$TWI)), 0.1)) ## my range of bio_07 con un passo di 0.1



### changing colnames values
colnames(DF_bio_07                 ) <- colnames(GLOBAL_buff) # change colnames
colnames(DF_bio_10                 ) <- colnames(GLOBAL_buff) # change colnames
colnames(DF_bio_16                 ) <- colnames(GLOBAL_buff) # change colnames
colnames(DF_bio_17                 ) <- colnames(GLOBAL_buff) # change colnames
colnames(DF_BLDFIE_M               ) <- colnames(GLOBAL_buff) # change colnames
colnames(DF_Population.density.2000) <- colnames(GLOBAL_buff) # change colnames
colnames(DF_TWI                    ) <- colnames(GLOBAL_buff) # change colnames

### removing the dataset as it is too big
rm(GLOBAL_buff)

## the output 
### Uploading the inizializers that are also the results i will have in the end
load(file="GLOBAL_ResponceCurve_bio_07.Rda")
load(file="GLOBAL_ResponceCurve_bio_10.Rda")
load(file="GLOBAL_ResponceCurve_bio_16.Rda")
load(file="GLOBAL_ResponceCurve_bio_17.Rda")
load(file="GLOBAL_ResponceCurve_BLDFIE_M.Rda")
load(file="GLOBAL_ResponceCurve_pop.dens.Rda")
load(file="GLOBAL_ResponceCurve_TWI.Rda"     )

### if i will have to re_do it, i have to take only the first two columns

### What am i doing ?
# THE AIM IS TO PREPARE A DATASET OF RESPONCE CURVES FOR EACH SPECIES RESPONCE CURVE
# I PREPARED BEFORE IN THE SCRIPT THE DATASETS IN WHICH I HAVE THE MEAN OF ALL VARIABLES EXEPT FOR THE ONE I WANT TO PREDICT
# nella pratica, ho preparato i dataset e caricato gli inizializer, in modo da aggiungere colonne al mio dataset iniziale

####### PERFORMING  THE GLOBAL LOOP   ########
## queste dovrebbero essere quelle per cui non ho il global...c(-11,-27,-30,-49,-55,-69)
##ok, i keep e. indica, because it was ok in the end. but i eliminated instead orzya sativa, that has not enough presences and 
for (sp in sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)]) { ## plus i eliminated eleusine indic 36a because it stopped the loop ## it seems however that the problem is orzya sativa 
  #sp <- sp.names[c(36)]
  ### Eleusine indica la facciamo un'altra votla, oppure il problema è di memoria
  # load("Zea.mays _GLOB.Rdata")
  ## loading the model for the species i am taking into account
  load(paste(sp, "_NATIVE.Rdata"))
  
  
  
  
  ## BIO_07
  a_bio_07 <- BIOMOD_EnsembleForecasting( myBiomodEMGLOB,
                                          #projection.output = NULL,
                                          new.env = as.matrix(DF_bio_07[,3:9]),
                                          xy.new.env = as.matrix(DF_bio_07[,1:2]),
                                          selected.models = 'all',
                                          proj.name = "responce curve_bio_07",
                                          #binary.meth = NULL,
                                          #filtered.meth = NULL,
                                          output.format = ".img"
  )
  
  ## BIO_10
  a_bio_10 <- BIOMOD_EnsembleForecasting( myBiomodEMGLOB,
                                          #projection.output = NULL,
                                          new.env = as.matrix(DF_bio_10[,3:9]),
                                          xy.new.env = as.matrix(DF_bio_10[,1:2]),
                                          selected.models = 'all',
                                          proj.name = "responce curve_bio_10",
                                          #binary.meth = NULL,
                                          #filtered.meth = NULL,
                                          output.format = ".img"
  )
  ## DF_bio_16
  a_bio_16 <- BIOMOD_EnsembleForecasting( myBiomodEMGLOB,
                                          #projection.output = NULL,
                                          new.env = as.matrix(DF_bio_16[,3:9]),
                                          xy.new.env = as.matrix(DF_bio_16[,1:2]),
                                          selected.models = 'all',
                                          proj.name = "responce curve_bio_16",
                                          #binary.meth = NULL,
                                          #filtered.meth = NULL,
                                          output.format = ".img"
  )
  ## DF_bio_17
  a_bio_17 <- BIOMOD_EnsembleForecasting( myBiomodEMGLOB,
                                          #projection.output = NULL,
                                          new.env = as.matrix(DF_bio_17[,3:9]),
                                          xy.new.env = as.matrix(DF_bio_17[,1:2]),
                                          selected.models = 'all',
                                          proj.name = "responce curve_bio_17",
                                          #binary.meth = NULL,
                                          #filtered.meth = NULL,
                                          output.format = ".img"
  )
  ## DF_BLDFIE_M
  a_BLDFIE_M <- BIOMOD_EnsembleForecasting( myBiomodEMGLOB,
                                            #projection.output = NULL,
                                            new.env = as.matrix(DF_BLDFIE_M[,3:9]),
                                            xy.new.env = as.matrix(DF_BLDFIE_M[,1:2]),
                                            selected.models = 'all',
                                            proj.name = "responce curve_BLDFIE",
                                            #binary.meth = NULL,
                                            #filtered.meth = NULL,
                                            output.format = ".img"
  )
  ## DF_Population.density.2000
  a_Population.density.2000 <- BIOMOD_EnsembleForecasting( myBiomodEMGLOB,
                                                           #projection.output = NULL,
                                                           new.env = as.matrix(DF_Population.density.2000[,3:9]),
                                                           xy.new.env = as.matrix(DF_Population.density.2000[,1:2]),
                                                           selected.models = 'all',
                                                           proj.name = "responce curve_popDens",
                                                           #binary.meth = NULL,
                                                           #filtered.meth = NULL,
                                                           output.format = ".img"
  )
  # DF_TWI
  a_TWI <- BIOMOD_EnsembleForecasting( myBiomodEMGLOB,
                                       #projection.output = NULL,
                                       new.env = as.matrix(DF_TWI[,3:9]),
                                       xy.new.env = as.matrix(DF_TWI[,1:2]),
                                       selected.models = 'all',
                                       proj.name = "responce curve_TWI",
                                       #binary.meth = NULL,
                                       #filtered.meth = NULL,
                                       output.format = ".img"
  )
  ## Extracting from the elaborated model my predictions
  aa_bio_07                  <- as.data.frame(a_bio_07@proj@val) 
  aa_bio_10                  <- as.data.frame(a_bio_10@proj@val) 
  aa_bio_16                  <- as.data.frame(a_bio_16@proj@val) 
  aa_bio_17                  <- as.data.frame(a_bio_17@proj@val) 
  aa_BLDFIE_M                <- as.data.frame(a_BLDFIE_M@proj@val) 
  aa_Population.density.2000 <- as.data.frame(a_Population.density.2000@proj@val) 
  aa_TWI                     <- as.data.frame(a_TWI@proj@val) 
  
  #### INIZIALIZER PREPARATION done only one 
  ### preparing the dataset to elaborate the values in a loop
  # inizialiser
##   GLOBAL_ResponceCurve_bio_07   <- aa_bio_07                 [,c(3)]
##   GLOBAL_ResponceCurve_bio_10   <- aa_bio_10                 [,c(3)]
##   GLOBAL_ResponceCurve_bio_16   <- aa_bio_16                 [,c(3)]
##   GLOBAL_ResponceCurve_bio_17   <- aa_bio_17                 [,c(3)]
##   GLOBAL_ResponceCurve_BLDFIE_M <- aa_BLDFIE_M               [,c(3)]
##   GLOBAL_ResponceCurve_pop.dens <- aa_Population.density.2000[,c(3)]
##   GLOBAL_ResponceCurve_TWI <-      aa_TWI                    [,c(3)]
##   # that way, i make the columns neglected, so i will not have them back during the future analyses
##   
##   # saving it 
##    save(GLOBAL_ResponceCurve_bio_07 , file="GLOBAL_ResponceCurve_bio_07.Rda")
##    save(GLOBAL_ResponceCurve_bio_10 , file="GLOBAL_ResponceCurve_bio_10.Rda")#GLOBAL_ResponceCurve_bio_10.Rda
##    save(GLOBAL_ResponceCurve_bio_16 , file="GLOBAL_ResponceCurve_bio_16.Rda")
##    save(GLOBAL_ResponceCurve_bio_17 , file="GLOBAL_ResponceCurve_bio_17.Rda")
##    save(GLOBAL_ResponceCurve_BLDFIE_M,file="GLOBAL_ResponceCurve_BLDFIE_M.Rda")
##    save(GLOBAL_ResponceCurve_pop.dens,file="GLOBAL_ResponceCurve_pop.dens.Rda")
##    save(GLOBAL_ResponceCurve_TWI    , file="GLOBAL_ResponceCurve_TWI.Rda"     )
  
  ## adding the new projections to the current values
  GLOBAL_ResponceCurve_bio_07  <- cbind(GLOBAL_ResponceCurve_bio_07  ,aa_bio_07                 [,c(3)])
  GLOBAL_ResponceCurve_bio_10  <- cbind(GLOBAL_ResponceCurve_bio_10  ,aa_bio_10                 [,c(3)])
  GLOBAL_ResponceCurve_bio_16  <- cbind(GLOBAL_ResponceCurve_bio_16  ,aa_bio_16                 [,c(3)])
  GLOBAL_ResponceCurve_bio_17  <- cbind(GLOBAL_ResponceCurve_bio_17  ,aa_bio_17                 [,c(3)])
  GLOBAL_ResponceCurve_BLDFIE_M<- cbind(GLOBAL_ResponceCurve_BLDFIE_M,aa_BLDFIE_M               [,c(3)])
  GLOBAL_ResponceCurve_pop.dens<- cbind(GLOBAL_ResponceCurve_pop.dens,aa_Population.density.2000[,c(3)])
  GLOBAL_ResponceCurve_TWI     <- cbind(GLOBAL_ResponceCurve_TWI     ,aa_TWI                    [,c(3)])
  rm(list=ls(pattern="GAM"))
  rm(list=ls(pattern="GLM"))
  rm(list=ls(pattern=sp))
  for(sp in sp.names[]){rm(list=ls(pattern=sp))} ### eliminating all other variables not necessary
  
  gc(reset=T)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
  
  
}

#### before saving i have to add the x_range values, that derive from the corresponding value of the dataset range of values
GLOBAL_ResponceCurve_bio_07  <- cbind(GLOBAL_ResponceCurve_bio_07  ,DF_bio_07$CHELSA_bio10_07  )              
GLOBAL_ResponceCurve_bio_10  <- cbind(GLOBAL_ResponceCurve_bio_10  ,DF_bio_10$CHELSA_bio10_10  )              
GLOBAL_ResponceCurve_bio_16  <- cbind(GLOBAL_ResponceCurve_bio_16  ,DF_bio_16$CHELSA_bio10_16  )               
GLOBAL_ResponceCurve_bio_17  <- cbind(GLOBAL_ResponceCurve_bio_17  ,DF_bio_17$CHELSA_bio10_17  )               
GLOBAL_ResponceCurve_BLDFIE_M<- cbind(GLOBAL_ResponceCurve_BLDFIE_M,DF_BLDFIE_M$BLDFIE_M )              
GLOBAL_ResponceCurve_pop.dens<- cbind(GLOBAL_ResponceCurve_pop.dens,DF_Population.density.2000$Population.density.2000)
GLOBAL_ResponceCurve_TWI     <- cbind(GLOBAL_ResponceCurve_TWI    ,DF_TWI$TWI )                   

#### eliminating the inizializer
GLOBAL_ResponceCurve_bio_07  <- as.data.frame(GLOBAL_ResponceCurve_bio_07  )[,-1]
GLOBAL_ResponceCurve_bio_10  <- as.data.frame(GLOBAL_ResponceCurve_bio_10  )[,-1]
GLOBAL_ResponceCurve_bio_16  <- as.data.frame(GLOBAL_ResponceCurve_bio_16  )[,-1]
GLOBAL_ResponceCurve_bio_17  <- as.data.frame(GLOBAL_ResponceCurve_bio_17  )[,-1]
GLOBAL_ResponceCurve_BLDFIE_M<- as.data.frame(GLOBAL_ResponceCurve_BLDFIE_M)[,-1]
GLOBAL_ResponceCurve_pop.dens<- as.data.frame(GLOBAL_ResponceCurve_pop.dens)[,-1]
GLOBAL_ResponceCurve_TWI     <- as.data.frame(GLOBAL_ResponceCurve_TWI     )[,-1]
## renaming them 
colnames(GLOBAL_ResponceCurve_bio_07  ) <- c(sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)], "CHELSA_bio10_07")
colnames(GLOBAL_ResponceCurve_bio_10  ) <- c(sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)], "CHELSA_bio10_10")
colnames(GLOBAL_ResponceCurve_bio_16  ) <- c(sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)], "CHELSA_bio10_16")
colnames(GLOBAL_ResponceCurve_bio_17  ) <- c(sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)], "CHELSA_bio10_17")
colnames(GLOBAL_ResponceCurve_BLDFIE_M) <- c(sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)], "BLDFIE_M")
colnames(GLOBAL_ResponceCurve_pop.dens) <- c(sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)], "Population.density.2000")
colnames(GLOBAL_ResponceCurve_TWI     ) <- c(sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)], "TWI")



### after the loop i can finally save it
# saving it 
save(GLOBAL_ResponceCurve_bio_07  , file="GLOBAL_ResponceCurve_bio_07.Rda")
save(GLOBAL_ResponceCurve_bio_10   , file="GLOBAL_ResponceCurve_bio_10.Rda")
save(GLOBAL_ResponceCurve_bio_16  , file="GLOBAL_ResponceCurve_bio_16.Rda")
save(GLOBAL_ResponceCurve_bio_17  , file="GLOBAL_ResponceCurve_bio_17.Rda")
save(GLOBAL_ResponceCurve_BLDFIE_M,file="GLOBAL_ResponceCurve_BLDFIE_M.Rda")
save(GLOBAL_ResponceCurve_pop.dens,file="GLOBAL_ResponceCurve_pop.dens.Rda")
save(GLOBAL_ResponceCurve_TWI    , file="GLOBAL_ResponceCurve_TWI.Rda"     )








plot((aa_bio_07$Zea.mays_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData)/1000~DF_bio_07$CHELSA_bio10_07)
plot((aa_bio_10$Zea.mays_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData)/1000~DF_bio_10$CHELSA_bio10_10)

## this to upload the projection??
# load(paste("Zea.mays//","proj_responce curve_bio_07//","proj_responce curve_bio_07_Zea.mays_ensemble.RData",sep = ""))

### now, i need to plot the values of the y responce, evaluated by the model, and the x is for each model and range, 
## MY X IS THE RANGE VALUES OF EACH DATAFRAME MAYBE I CAN ADD IT WITH A CBIND



##### PLOTTING RESULTS ########################
### all dataset for each responce curve GLOBAL MODEL
## load("GLOBAL_ResponceCurve_bio_07.Rda")#GLOBAL_ResponceCurve_bio_07  
## load("GLOBAL_ResponceCurve_bio_10.Rda")#GLOBAL_ResponceCurve_bio_10  
## load("GLOBAL_ResponceCurve_bio_16.Rda")#GLOBAL_ResponceCurve_bio_16  
## load("GLOBAL_ResponceCurve_bio_17.Rda")#GLOBAL_ResponceCurve_bio_17  
## load("GLOBAL_ResponceCurve_BLDFIE_M.Rda")#GLOBAL_ResponceCurve_BLDFIE_M
## load("GLOBAL_ResponceCurve_pop.dens.Rda")#GLOBAL_ResponceCurve_pop.dens
## load("GLOBAL_ResponceCurve_TWI.Rda"     )#GLOBAL_ResponceCurve_TWI    ,
## 
## 
## ### all dataset for each responce curve LOCAL MODEL
## load("LOCAL_ResponceCurve_bio_07.Rda")#LOCAL_ResponceCurve_bio_07 , 
## load("LOCAL_ResponceCurve_bio_10.Rda")#LOCAL_ResponceCurve_bio_10 , 
## load("LOCAL_ResponceCurve_bio_16.Rda")#LOCAL_ResponceCurve_bio_16 , 
## load("LOCAL_ResponceCurve_bio_17.Rda")#LOCAL_ResponceCurve_bio_17 , 
## load("LOCAL_ResponceCurve_BLDFIE_M.Rda")#LOCAL_ResponceCurve_BLDFIE_M,
## load("LOCAL_ResponceCurve_pop.dens.Rda")#LOCAL_ResponceCurve_pop.dens,
## load("LOCAL_ResponceCurve_TWI.Rda"     )#LOCAL_ResponceCurve_TWI    , 


## FIRST TRY TO PLOT LOCAL MODEL #####
### an example che deve essere affinato, non abbiamo ancora l'asse y ben definita
#### starting with LOCAL model 
#### LOCAL_ResponceCurve_bio_07
#
##### plotting all species responce curves
#range(as.vector(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)]))
#plot(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)][[1]]~LOCAL_ResponceCurve_bio_07$`DF_bio_07$CHELSA_bio10_07`, pch=20, cex=0.005,ylim=range(as.vector(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)])))
#
#for (i in 2:93) {
#  points(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)][[i]]~LOCAL_ResponceCurve_bio_07$`DF_bio_07$CHELSA_bio10_07`, pch=20, cex=0.005)
#  
#}
#
#### LOCAL_ResponceCurve_bio_10
#
##### plotting all species responce curves
#range(as.vector(LOCAL_ResponceCurve_bio_10[grep(x=colnames(LOCAL_ResponceCurve_bio_10),patter="TSS", value=T)]))
#
#plot(LOCAL_ResponceCurve_bio_10[grep(x=colnames(LOCAL_ResponceCurve_bio_10),patter="TSS", value=T)][[1]]~LOCAL_ResponceCurve_bio_10$`DF_bio_10$CHELSA_bio10_10`, pch=20, cex=0.005, ylim=range(as.vector(LOCAL_ResponceCurve_bio_10[grep(x=colnames(LOCAL_ResponceCurve_bio_10),patter="TSS", value=T)])))
#
#for (i in 2:93) {
#  points(LOCAL_ResponceCurve_bio_10[grep(x=colnames(LOCAL_ResponceCurve_bio_10),patter="TSS", value=T)][[i]]~LOCAL_ResponceCurve_bio_10$`DF_bio_10$CHELSA_bio10_10`, pch=20, cex=0.005)
#  
#}
#
#### LOCAL_ResponceCurve_bio_16
#
##### plotting all species responce curves
#range(as.vector(LOCAL_ResponceCurve_bio_16[grep(x=colnames(LOCAL_ResponceCurve_bio_16),patter="TSS", value=T)]))
#
#plot(LOCAL_ResponceCurve_bio_16[grep(x=colnames(LOCAL_ResponceCurve_bio_16),patter="TSS", value=T)][[1]]~LOCAL_ResponceCurve_bio_16$`DF_bio_16$CHELSA_bio10_16` , pch=20, cex=0.005, ylim=range(as.vector(LOCAL_ResponceCurve_bio_16[grep(x=colnames(LOCAL_ResponceCurve_bio_16),patter="TSS", value=T)])))
#
#for (i in 2:93) {
#  points(LOCAL_ResponceCurve_bio_16[grep(x=colnames(LOCAL_ResponceCurve_bio_16),patter="TSS", value=T)][[i]]~LOCAL_ResponceCurve_bio_16$`DF_bio_16$CHELSA_bio10_16`, pch=20, cex=0.005)
#  
#}
#
#
#
#### LOCAL_ResponceCurve_bio_17
#
##### plotting all species responce curves
#range(as.vector(LOCAL_ResponceCurve_bio_17[grep(x=colnames(LOCAL_ResponceCurve_bio_17),patter="TSS", value=T)]))
#
#plot(LOCAL_ResponceCurve_bio_17[grep(x=colnames(LOCAL_ResponceCurve_bio_17),patter="TSS", value=T)][[1]]~LOCAL_ResponceCurve_bio_17$`DF_bio_17$CHELSA_bio10_17` , pch=20, cex=0.005, ylim=range(as.vector(LOCAL_ResponceCurve_bio_17[grep(x=colnames(LOCAL_ResponceCurve_bio_17),patter="TSS", value=T)])))
#
#for (i in 2:93) {
#  points(LOCAL_ResponceCurve_bio_17[grep(x=colnames(LOCAL_ResponceCurve_bio_17),patter="TSS", value=T)][[i]]~LOCAL_ResponceCurve_bio_17$`DF_bio_17$CHELSA_bio10_17` , pch=20, cex=0.005)
#  
#}
#
#### LOCAL_ResponceCurve_BLDFIE_M
#
##### plotting all species responce curves
#range(as.vector(LOCAL_ResponceCurve_BLDFIE_M[grep(x=colnames(LOCAL_ResponceCurve_BLDFIE_M),patter="TSS", value=T)]))
#
#plot(LOCAL_ResponceCurve_BLDFIE_M[grep(x=colnames(LOCAL_ResponceCurve_BLDFIE_M),patter="TSS", value=T)][[1]]~LOCAL_ResponceCurve_BLDFIE_M$`DF_BLDFIE_M$BLDFIE_M` , pch=20, cex=0.005, ylim=range(as.vector(LOCAL_ResponceCurve_BLDFIE_M[grep(x=colnames(LOCAL_ResponceCurve_BLDFIE_M),patter="TSS", value=T)])))
#
#for (i in 2:93) {
#  points(LOCAL_ResponceCurve_BLDFIE_M[grep(x=colnames(LOCAL_ResponceCurve_BLDFIE_M),patter="TSS", value=T)][[i]]~LOCAL_ResponceCurve_BLDFIE_M$`DF_BLDFIE_M$BLDFIE_M` , pch=20, cex=0.005)
#  
#}
#
#### LOCAL_ResponceCurve_pop.dens
#
##### plotting all species responce curves
#range(as.vector(LOCAL_ResponceCurve_pop.dens[grep(x=colnames(LOCAL_ResponceCurve_pop.dens),patter="TSS", value=T)]))
#
#plot(LOCAL_ResponceCurve_pop.dens[grep(x=colnames(LOCAL_ResponceCurve_pop.dens),patter="TSS", value=T)][[1]]~LOCAL_ResponceCurve_pop.dens$`DF_Population.density.2000$Population.density.2000` , pch=20, cex=0.005, ylim=range(as.vector(LOCAL_ResponceCurve_pop.dens[grep(x=colnames(LOCAL_ResponceCurve_pop.dens),patter="TSS", value=T)])))
#
#for (i in 2:5) {
#  points(LOCAL_ResponceCurve_pop.dens[grep(x=colnames(LOCAL_ResponceCurve_pop.dens),patter="TSS", value=T)][[i]]~LOCAL_ResponceCurve_pop.dens$`DF_Population.density.2000$Population.density.2000` , pch=20, cex=0.005)
#  
#}
#
#
#### LOCAL_ResponceCurve_pop.dens
#
##### plotting all species responce curves
#range(as.vector(LOCAL_ResponceCurve_TWI[grep(x=colnames(LOCAL_ResponceCurve_TWI),patter="TSS", value=T)]))
#
#plot(LOCAL_ResponceCurve_TWI[grep(x=colnames(LOCAL_ResponceCurve_TWI),patter="TSS", value=T)][[1]]~LOCAL_ResponceCurve_TWI$`DF_TWI$TWI` , pch=20, cex=0.005, ylim=range(as.vector(LOCAL_ResponceCurve_TWI[grep(x=colnames(LOCAL_ResponceCurve_TWI),patter="TSS", value=T)])))
#
#for (i in 2:93) {
#  points(LOCAL_ResponceCurve_TWI[grep(x=colnames(LOCAL_ResponceCurve_TWI),patter="TSS", value=T)][[i]]~LOCAL_ResponceCurve_TWI$`DF_TWI$TWI` , pch=20, cex=0.005)
#  
#}
#
#
#



###### LOCAL MODEL RESPONCE CURVE IN FUNZIONE DEI BIOMI ######
## come faccio a plottarle??
#load("RangeChange_Traits.Rda")
#
#
###### ADDING MAINE BIOME AGGR COLUMN
#### adding a column for main biome aggregated
#RangeChange_Traits$Main.Biome_AGGR <- rep("temp",93)
#RangeChange_Traits[grep("Temperate",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"]### these are temperate yet
### xeric
#RangeChange_Traits[grep("Deserts",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
#RangeChange_Traits[grep("Mediterranean",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
### tropical
#RangeChange_Traits[grep("Tropical",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "trop"
#### trying with aggregated main biomes
###MED
#MED_sp<-RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med", "SpName"]
#a<-paste(MED_sp[1],
#         MED_sp[2],
#         MED_sp[3],
#         MED_sp[4],
#         MED_sp[5],
#         MED_sp[6],
#         MED_sp[7],
#         MED_sp[8],
#         MED_sp[9],
#         MED_sp[10],
#         MED_sp[11],
#         MED_sp[12],
#         MED_sp[13],
#         sep="|")
#### LOCAL_ResponceCurve_bio_07
#
##### plotting all species responce curves
##range(as.vector(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter=a, value=T)]))
#
###eliminating ROC from the columns
#x<-LOCAL_ResponceCurve_bio_07$`DF_bio_07$CHELSA_bio10_07` ## metto da parte le x
### seleziono solo le colonne con tss
#LOCAL_ResponceCurve_bio_07<-LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)]
### riaggiungo le x
#LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, x)
#plot(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter=a, value=T)][[1]]~LOCAL_ResponceCurve_bio_07$x, pch=20, cex=0.005,ylim=range(as.vector(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter=a, value=T)])))
#
#for (i in 2:13) {
#  points(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter=a, value=T)][[i]]~LOCAL_ResponceCurve_bio_07$x, pch=20, cex=0.005)
#  
#}
#
#
#
###### trop
###trop
###TROP
#load("LOCAL_ResponceCurve_bio_07.Rda")#LOCAL_ResponceCurve_bio_07 , 
#
#trop_sp<-RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop", "SpName"]
#
#a<-paste(trop_sp[1],
#         trop_sp[2],
#         trop_sp[3],
#         trop_sp[4],
#         trop_sp[5],
#         trop_sp[6],
#         trop_sp[7],
#         trop_sp[8],
#         trop_sp[9],
#         trop_sp[10],
#         trop_sp[11],
#         trop_sp[12],
#         trop_sp[13],
#         trop_sp[14],
#         trop_sp[15],
#         trop_sp[16],
#         trop_sp[17],
#         trop_sp[18],
#         trop_sp[19],
#         trop_sp[20],
#         trop_sp[21],
#         trop_sp[22],
#         trop_sp[23],
#         trop_sp[24],
#         trop_sp[25],
#         trop_sp[26],
#         trop_sp[27],
#         trop_sp[27],
#         trop_sp[28],
#         trop_sp[29],
#         trop_sp[30],
#         trop_sp[31],
#         sep="|")
#### LOCAL_ResponceCurve_bio_07
#
##### plotting all species responce curves
##range(as.vector(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter=a, value=T)]))
#
###eliminating ROC from the columns
#x<-LOCAL_ResponceCurve_bio_07$`DF_bio_07$CHELSA_bio10_07` ## metto da parte le x
### seleziono solo le colonne con tss
#LOCAL_ResponceCurve_bio_07<-LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)]
### riaggiungo le x
#LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, x)
#plot(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter=a, value=T)][[1]]~LOCAL_ResponceCurve_bio_07$x, pch=20, cex=0.005,ylim=range(as.vector(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter=a, value=T)])))
#
#for (i in 2:31) {
#  points(LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter=a, value=T)][[i]]~LOCAL_ResponceCurve_bio_07$x, pch=20, cex=0.005)
#  
#}
#
#
#
#
#
#
#
####### METODO PIU' INTELLIGENTE PER CREARE IL DATASET ######
#
### metto da parte le x
#x<-LOCAL_ResponceCurve_bio_07$`DF_bio_07$CHELSA_bio10_07` ## metto da parte le x
#x1<-GLOBAL_ResponceCurve_bio_07$`DF_bio_07$CHELSA_bio10_07` ## metto da parte le x
#
### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_bio_07<-LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_bio_07<-GLOBAL_ResponceCurve_bio_07[grep(x=colnames(GLOBAL_ResponceCurve_bio_07),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_bio_07<-rbind(sp.names,LOCAL_ResponceCurve_bio_07 )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_bio_07<-rbind(sp.names,GLOBAL_ResponceCurve_bio_07 )
#
###### ADDING MAINE BIOME AGGR COLUMN
#load("RangeChange_Traits.Rda")
#### adding a column for main biome aggregated
#RangeChange_Traits$Main.Biome_AGGR <- rep("temp",93)
#RangeChange_Traits[grep("Temperate",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"]### these are temperate yet
### xeric
#RangeChange_Traits[grep("Deserts",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
#RangeChange_Traits[grep("Mediterranean",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
### tropical
#RangeChange_Traits[grep("Tropical",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "trop"
#### trying with aggregated main biomes
#
#### PLOTTING LOCAL MODEL RESPONCE BIO_07 FOR MED SPECIES 
###MED ## LOCAL 
#MD<-LOCAL_ResponceCurve_bio_07[,LOCAL_ResponceCurve_bio_07[1,] %in% RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med","SpName" ]]
## re adding the x
#MD<-cbind(MD, c("x",x))
#colnames(MD)<- c(colnames(MD[,-14]),"x")
#par(mfrow=c(2,1))
#  
##sapply(MD[-1,1:13],max)
#plot(as.numeric(MD[-1,1])~as.numeric(MD$x[-1]), pch=20, cex=0.005,ylim=c(0,max(sapply(MD[-1,-14],as.numeric))))
#
#for (i in 2:dim(MD)[2]) {
#  points(as.numeric(MD[-1,i])~as.numeric(MD$x[-1]), pch=20, cex=0.005)
#  
#}
#
### MED GLOBAL
#### PLOTTING LOCAL MODEL RESPONCE BIO_07 FOR MED SPECIES 
###MED
#MD_GLOB<-GLOBAL_ResponceCurve_bio_07[,GLOBAL_ResponceCurve_bio_07[1,] %in% RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med","SpName" ]]
## re adding the x
#MD_GLOB<-cbind(MD_GLOB, c("x",x1))
#colnames(MD_GLOB)<- c(colnames(MD_GLOB[,-14]),"x1")
#
##sapply(MD_GLOB[-1,1:13],max)
#plot(as.numeric(MD_GLOB[-1,1])~as.numeric(MD_GLOB$x1[-1]), pch=20, cex=0.005,ylim=c(0,max(sapply(MD_GLOB[-1,-14],as.numeric))))
#
#for (i in 2:dim(MD_GLOB)[2]) {
#  points(as.numeric(MD_GLOB[-1,i])~as.numeric(MD_GLOB$x1[-1]), pch=20, cex=0.005)
#  
#}
#
#
#### questo accenno mi fa gi? capire che nel modello globale abbiamo meglio caturato la nicchia della specie,
### nel caso del modello locale invece i trend non appaiono cos? evidenti direi
#
#### PLOTTING LOCAL MODEL RESPONCE BIO_07 FOR temp SPECIES 
###temp ## LOCAL 
#MD<-LOCAL_ResponceCurve_bio_07[,LOCAL_ResponceCurve_bio_07[1,] %in% RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp","SpName" ]]
## re adding the x
#MD<-cbind(MD, c("x",x))
#colnames(MD)<- c(colnames(MD[,-50]),"x")
#par(mfrow=c(2,1))
#
##sapply(MD[-1,1:13],max)
#plot(as.numeric(MD[-1,1])~as.numeric(MD$x[-1]), pch=20, cex=0.005,ylim=c(0,max(sapply(MD[-1,-50],as.numeric))))
#
#for (i in 2:dim(MD)[2]) {
#  points(as.numeric(MD[-1,i])~as.numeric(MD$x[-1]), pch=20, cex=0.005)
#  
#}
#
### temp GLOBAL
#### PLOTTING LOCAL MODEL RESPONCE BIO_07 FOR temp SPECIES 
###temp
#MD_GLOB<-GLOBAL_ResponceCurve_bio_07[,GLOBAL_ResponceCurve_bio_07[1,] %in% RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp","SpName" ]]
## re adding the x
#MD_GLOB<-cbind(MD_GLOB, c("x",x1))
#colnames(MD_GLOB)<- c(colnames(MD_GLOB[,-50]),"x1")
#
##sapply(MD_GLOB[-1,1:13],max)
#plot(as.numeric(MD_GLOB[-1,1])~as.numeric(MD_GLOB$x1[-1]), pch=20, cex=0.005,ylim=c(0,max(sapply(MD_GLOB[-1,-50],as.numeric))))
#
#for (i in 2:dim(MD_GLOB)[2]) {
#  points(as.numeric(MD_GLOB[-1,i])~as.numeric(MD_GLOB$x1[-1]), pch=20, cex=0.005)
#  
#}
#
#### PLOTTING LOCAL MODEL RESPONCE BIO_07 FOR trop SPECIES 
### trop ## LOCAL 
#MD<-LOCAL_ResponceCurve_bio_07[,LOCAL_ResponceCurve_bio_07[1,] %in% RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop","SpName" ]]
## re adding the x
#MD<-cbind(MD, c("x",x))
#colnames(MD)<- c(colnames(MD[,-32]),"x")
#par(mfrow=c(2,1))
#
##sapply(MD[-1,1:13],max)
#plot(as.numeric(MD[-1,1])~as.numeric(MD$x[-1]), pch=20, cex=0.005,ylim=c(0,max(sapply(MD[-1,-32],as.numeric))))
#
#for (i in 2:dim(MD)[2]) {
#  points(as.numeric(MD[-1,i])~as.numeric(MD$x[-1]), pch=20, cex=0.005)
#  
#}
#
### trop GLOBAL
#### PLOTTING LOCAL MODEL RESPONCE BIO_07 FOR trop SPECIES 
###trop
#MD_GLOB<-GLOBAL_ResponceCurve_bio_07[,GLOBAL_ResponceCurve_bio_07[1,] %in% RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop","SpName" ]]
## re adding the x
#MD_GLOB<-cbind(MD_GLOB, c("x",x1))
#colnames(MD_GLOB)<- c(colnames(MD_GLOB[,-32]),"x1")
#
##sapply(MD_GLOB[-1,1:13],max)
#plot(as.numeric(MD_GLOB[-1,1])~as.numeric(MD_GLOB$x1[-1]), pch=20, cex=0.005,ylim=c(0,max(sapply(MD_GLOB[-1,-32],as.numeric))))
#
#for (i in 2:dim(MD_GLOB)[2]) {
#  points(as.numeric(MD_GLOB[-1,i])~as.numeric(MD_GLOB$x1[-1]), pch=20, cex=0.005)
#  
#}
#
#
#
#

##### off topic responce curve for my selected species to check LAURE NICHE DOUBTS #########################
#### all dataset for each responce curve GLOBAL MODEL
#
## i want to plot the species i selected for laure: "Acacia.dealbata","Erigeron.canadensis" , "Euphorbia.maculata" , "Solidago.canadensis"
##for (sp in c("Acacia.dealbata","Erigeron.canadensis" , "Euphorbia.maculata" , "Solidago.canadensis")) {
#  sp<-"Solidago.canadensis"
#  # the loop doesn't work 
#load("GLOBAL_ResponceCurve_bio_07.Rda")#GLOBAL_ResponceCurve_bio_07  
#load("GLOBAL_ResponceCurve_bio_10.Rda")#GLOBAL_ResponceCurve_bio_10  
#load("GLOBAL_ResponceCurve_bio_16.Rda")#GLOBAL_ResponceCurve_bio_16  
#load("GLOBAL_ResponceCurve_bio_17.Rda")#GLOBAL_ResponceCurve_bio_17  
#load("GLOBAL_ResponceCurve_BLDFIE_M.Rda")#GLOBAL_ResponceCurve_BLDFIE_M
#load("GLOBAL_ResponceCurve_pop.dens.Rda")#GLOBAL_ResponceCurve_pop.dens
#load("GLOBAL_ResponceCurve_TWI.Rda"     )#GLOBAL_ResponceCurve_TWI    ,
#
#
#### all dataset for each responce curve LOCAL MODEL
#load("LOCAL_ResponceCurve_bio_07.Rda")#LOCAL_ResponceCurve_bio_07 , 
#load("LOCAL_ResponceCurve_bio_10.Rda")#LOCAL_ResponceCurve_bio_10 , 
#load("LOCAL_ResponceCurve_bio_16.Rda")#LOCAL_ResponceCurve_bio_16 , 
#load("LOCAL_ResponceCurve_bio_17.Rda")#LOCAL_ResponceCurve_bio_17 , 
#load("LOCAL_ResponceCurve_BLDFIE_M.Rda")#LOCAL_ResponceCurve_BLDFIE_M,
#load("LOCAL_ResponceCurve_pop.dens.Rda")#LOCAL_ResponceCurve_pop.dens,
#load("LOCAL_ResponceCurve_TWI.Rda"     )#LOCAL_ResponceCurve_TWI    , 
#
##png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
#par(mfrow=c(3,2))
#
#### BIO_07
### selecting the x
#x_bio_07<-LOCAL_ResponceCurve_bio_07$`DF_bio_07$CHELSA_bio10_07` ## metto da parte le x
#x1_bio_07<-GLOBAL_ResponceCurve_bio_07$`DF_bio_07$CHELSA_bio10_07` ## metto da parte le x
#
### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_bio_07<-LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_bio_07<-GLOBAL_ResponceCurve_bio_07[grep(x=colnames(GLOBAL_ResponceCurve_bio_07),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_bio_07<-rbind(sp.names,LOCAL_ResponceCurve_bio_07 )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_bio_07<-rbind(sp.names,GLOBAL_ResponceCurve_bio_07 )
#
### plotting responces for Acacia.dealbata
### bio_07 Local
#plot(as.numeric(LOCAL_ResponceCurve_bio_07[-1,LOCAL_ResponceCurve_bio_07[1,]==sp])~x_bio_07 , pch=20, cex=0.005, main="LOCAL_RC.Bio_07")
#plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
#
#
#
#
#
#
#
#
#
#
#
#
#
#### BIO_10
### selecting the x
#x_bio_10<-LOCAL_ResponceCurve_bio_10$`DF_bio_10$CHELSA_bio10_10` ## metto da parte le x
#x1_bio_10<-GLOBAL_ResponceCurve_bio_10$`DF_bio_10$CHELSA_bio10_10` ## metto da parte le x
#
### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_bio_10<-LOCAL_ResponceCurve_bio_10[grep(x=colnames(LOCAL_ResponceCurve_bio_10),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_bio_10<-GLOBAL_ResponceCurve_bio_10[grep(x=colnames(GLOBAL_ResponceCurve_bio_10),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_bio_10<-rbind(sp.names,LOCAL_ResponceCurve_bio_10 )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_bio_10<-rbind(sp.names,GLOBAL_ResponceCurve_bio_10 )
#
### plotting responces for Acacia.dealbata
### bio_10 Local
#plot(as.numeric(LOCAL_ResponceCurve_bio_10[-1,LOCAL_ResponceCurve_bio_10[1,]==sp])~x_bio_10 , pch=20, cex=0.005, main="LOCAL_RC.Bio_10")
#plot(as.numeric(GLOBAL_ResponceCurve_bio_10[-1,GLOBAL_ResponceCurve_bio_10[1,]==sp])~x1_bio_10 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_10")
#
#
#
#
#
#### BIO_16
### selecting the x
#x_bio_16<-LOCAL_ResponceCurve_bio_16$`DF_bio_16$CHELSA_bio10_16` ## metto da parte le x
#x1_bio_16<-GLOBAL_ResponceCurve_bio_16$`DF_bio_16$CHELSA_bio10_16` ## metto da parte le x
#
### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_bio_16<-LOCAL_ResponceCurve_bio_16[grep(x=colnames(LOCAL_ResponceCurve_bio_16),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_bio_16<-GLOBAL_ResponceCurve_bio_16[grep(x=colnames(GLOBAL_ResponceCurve_bio_16),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_bio_16<-rbind(sp.names,LOCAL_ResponceCurve_bio_16 )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_bio_16<-rbind(sp.names,GLOBAL_ResponceCurve_bio_16 )
#
### plotting responces for Acacia.dealbata
### bio_16 Local
#plot(as.numeric(LOCAL_ResponceCurve_bio_16[-1,LOCAL_ResponceCurve_bio_16[1,]==sp])~x_bio_16 , pch=20, cex=0.005, main="LOCAL_RC.Bio_16")
#plot(as.numeric(GLOBAL_ResponceCurve_bio_16[-1,GLOBAL_ResponceCurve_bio_16[1,]==sp])~x1_bio_16 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_16")
#
#
##dev.off()
#
#par(mfrow=c(4,2))
#
#
#
#### BIO_17
### selecting the x
# x_bio_17 <-LOCAL_ResponceCurve_bio_17$`DF_bio_17$CHELSA_bio10_17` ## metto da parte le x
#x1_bio_17<-GLOBAL_ResponceCurve_bio_17$`DF_bio_17$CHELSA_bio10_17` ## metto da parte le x
#
### seleziono solo le colonne con tss
#
# LOCAL_ResponceCurve_bio_17<- LOCAL_ResponceCurve_bio_17[grep( x=colnames(LOCAL_ResponceCurve_bio_17),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_bio_17<-GLOBAL_ResponceCurve_bio_17[grep(x=colnames(GLOBAL_ResponceCurve_bio_17),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_bio_17<-rbind(sp.names,LOCAL_ResponceCurve_bio_17 )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_bio_17<-rbind(sp.names,GLOBAL_ResponceCurve_bio_17 )
#
### plotting responces for Acacia.dealbata
### bio_16 Local
#plot(as.numeric( LOCAL_ResponceCurve_bio_17[-1, LOCAL_ResponceCurve_bio_17[1,]==sp])~ x_bio_17 , pch=20, cex=0.005, main= "LOCAL_RC.Bio_17")
#plot(as.numeric(GLOBAL_ResponceCurve_bio_17[-1,GLOBAL_ResponceCurve_bio_17[1,]==sp])~x1_bio_17 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_17")
#
#
#
#
#
#
#
#
#
#### LOCAL_ResponceCurve_BLDFIE_M
### selecting the x
#x_BLDFIE_M <-LOCAL_ResponceCurve_BLDFIE_M$`DF_BLDFIE_M$BLDFIE_M` ## metto da parte le x
#x1_BLDFIE_M<-GLOBAL_ResponceCurve_BLDFIE_M$`DF_BLDFIE_M$BLDFIE_M`  ## metto da parte le x
#
### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_BLDFIE_M<- LOCAL_ResponceCurve_BLDFIE_M[grep( x=colnames(LOCAL_ResponceCurve_BLDFIE_M),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_BLDFIE_M<-GLOBAL_ResponceCurve_BLDFIE_M[grep(x=colnames(GLOBAL_ResponceCurve_BLDFIE_M),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_BLDFIE_M<-rbind(sp.names,LOCAL_ResponceCurve_BLDFIE_M )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_BLDFIE_M<-rbind(sp.names,GLOBAL_ResponceCurve_BLDFIE_M )
#
### plotting responces for Acacia.dealbata
### bio_16 Local
#plot(as.numeric( LOCAL_ResponceCurve_BLDFIE_M[-1, LOCAL_ResponceCurve_BLDFIE_M[1,]==sp])~ x_BLDFIE_M , pch=20, cex=0.005, main= "LOCAL_RC.BLDFIE_M")
#plot(as.numeric(GLOBAL_ResponceCurve_BLDFIE_M[-1,GLOBAL_ResponceCurve_BLDFIE_M[1,]==sp])~x1_BLDFIE_M , pch=20, cex=0.005, main="GLOBAL_RC.BLDFIE_M")
#
#
#
#
#### pop.dens
### selecting the x
#x_pop_dens <-LOCAL_ResponceCurve_pop.dens$`DF_Population.density.2000$Population.density.2000` ## metto da parte le x
#x1_pop_dens<-GLOBAL_ResponceCurve_pop.dens$`DF_Population.density.2000$Population.density.2000`  ## metto da parte le x
#
### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_pop.dens<- LOCAL_ResponceCurve_pop.dens[grep( x=colnames(LOCAL_ResponceCurve_pop.dens),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_pop.dens<-GLOBAL_ResponceCurve_pop.dens[grep(x=colnames(GLOBAL_ResponceCurve_pop.dens),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_pop.dens<-rbind(sp.names,LOCAL_ResponceCurve_pop.dens )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_pop.dens<-rbind(sp.names,GLOBAL_ResponceCurve_pop.dens )
#
### plotting responces for Acacia.dealbata
### bio_16 Local
#plot(as.numeric( LOCAL_ResponceCurve_pop.dens[-1, LOCAL_ResponceCurve_pop.dens[1,]==sp])~x_pop_dens , pch=20, cex=0.005, main= "LOCAL_RC.POP_DENS")
#plot(as.numeric(GLOBAL_ResponceCurve_pop.dens[-1,GLOBAL_ResponceCurve_pop.dens[1,]==sp])~x1_pop_dens , pch=20, cex=0.005, main="GLOBAL_RC.POP_DENS")
#
#
#
#
#
#
#
#### TWI
### selecting the x
#x_TWI <-LOCAL_ResponceCurve_TWI$`DF_TWI$TWI` ## metto da parte le x
#x1_TWI<-GLOBAL_ResponceCurve_TWI$`DF_TWI$TWI`  ## metto da parte le x
#
### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_TWI<- LOCAL_ResponceCurve_TWI[grep( x=colnames(LOCAL_ResponceCurve_TWI),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_TWI<-GLOBAL_ResponceCurve_TWI[grep(x=colnames(GLOBAL_ResponceCurve_TWI),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_TWI<-rbind(sp.names,LOCAL_ResponceCurve_TWI )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_TWI<-rbind(sp.names,GLOBAL_ResponceCurve_TWI )
#
### plotting responces for Acacia.dealbata
### bio_16 Local
#plot(as.numeric( LOCAL_ResponceCurve_TWI[-1, LOCAL_ResponceCurve_TWI[1,]==sp])~x_TWI , pch=20, cex=0.005, main= "LOCAL_RC.TWI")
#plot(as.numeric(GLOBAL_ResponceCurve_TWI[-1,GLOBAL_ResponceCurve_TWI[1,]==sp])~x1_TWI , pch=20, cex=0.005, main="GLOBAL_RC.TWI")
#
#

####### PERFORMING RESPONCE CURVES IN A LOOP_DEF #### 
### PREPARING BIOMES, MI SAR? UTILE IN SEGUITO
### thinking of how plotting mean from biomes lines
#### AGGREGATING BIOMES
load("RangeChange_Traits.Rda")
load("RangeChange_Traits_new.Rda")
## fro now
RangeChange_Traits <- RangeChange_Traits_new


RangeChange_Traits$Main.Biome_AGGR <- rep("temp",93)
RangeChange_Traits[grep("Temperate",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"]### these are temperate yet
## xeric
RangeChange_Traits[grep("Deserts",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
RangeChange_Traits[grep("Mediterranean",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
## tropical
RangeChange_Traits[grep("Tropical",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "trop"
### trying with aggregated main biomes

# the loop doesn't work 
load("GLOBAL_ResponceCurve_bio_07.Rda")#GLOBAL_ResponceCurve_bio_07  
load("GLOBAL_ResponceCurve_bio_10.Rda")#GLOBAL_ResponceCurve_bio_10  
load("GLOBAL_ResponceCurve_bio_16.Rda")#GLOBAL_ResponceCurve_bio_16  
load("GLOBAL_ResponceCurve_bio_17.Rda")#GLOBAL_ResponceCurve_bio_17  
load("GLOBAL_ResponceCurve_BLDFIE_M.Rda")#GLOBAL_ResponceCurve_BLDFIE_M
load("GLOBAL_ResponceCurve_pop.dens.Rda")#GLOBAL_ResponceCurve_pop.dens
load("GLOBAL_ResponceCurve_TWI.Rda"     )#GLOBAL_ResponceCurve_TWI    ,


### all dataset for each responce curve LOCAL MODEL
load("LOCAL_ResponceCurve_bio_07.Rda")#LOCAL_ResponceCurve_bio_07 , 
load("LOCAL_ResponceCurve_bio_10.Rda")#LOCAL_ResponceCurve_bio_10 , 
load("LOCAL_ResponceCurve_bio_16.Rda")#LOCAL_ResponceCurve_bio_16 , 
load("LOCAL_ResponceCurve_bio_17.Rda")#LOCAL_ResponceCurve_bio_17 , 
load("LOCAL_ResponceCurve_BLDFIE_M.Rda")#LOCAL_ResponceCurve_BLDFIE_M,
load("LOCAL_ResponceCurve_pop.dens.Rda")#LOCAL_ResponceCurve_pop.dens,
load("LOCAL_ResponceCurve_TWI.Rda"     )#LOCAL_ResponceCurve_TWI    , 



### BIO_07
## selecting the x
x_bio_07<-LOCAL_ResponceCurve_bio_07$CHELSA_bio10_07 ## metto da parte le x
x1_bio_07<-GLOBAL_ResponceCurve_bio_07$CHELSA_bio10_07 ## metto da parte le x

### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_bio_07<-LOCAL_ResponceCurve_bio_07[grep(x=colnames(LOCAL_ResponceCurve_bio_07),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_bio_07<-GLOBAL_ResponceCurve_bio_07[grep(x=colnames(GLOBAL_ResponceCurve_bio_07),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_bio_07<-rbind(sp.names,LOCAL_ResponceCurve_bio_07 )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_bio_07<-rbind(sp.names,GLOBAL_ResponceCurve_bio_07 )


### BIO_10
## selecting the x
x_bio_10<-LOCAL_ResponceCurve_bio_10$CHELSA_bio10_10 ## metto da parte le x
x1_bio_10<-GLOBAL_ResponceCurve_bio_10$CHELSA_bio10_10 ## metto da parte le x

### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_bio_10<-LOCAL_ResponceCurve_bio_10[grep(x=colnames(LOCAL_ResponceCurve_bio_10),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_bio_10<-GLOBAL_ResponceCurve_bio_10[grep(x=colnames(GLOBAL_ResponceCurve_bio_10),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_bio_10<-rbind(sp.names,LOCAL_ResponceCurve_bio_10 )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_bio_10<-rbind(sp.names,GLOBAL_ResponceCurve_bio_10 )

### BIO_16
## selecting the x
x_bio_16<-LOCAL_ResponceCurve_bio_16$CHELSA_bio10_16  ## metto da parte le x
x1_bio_16<-GLOBAL_ResponceCurve_bio_16$CHELSA_bio10_16 ## metto da parte le x

### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_bio_16<-LOCAL_ResponceCurve_bio_16[grep(x=colnames(LOCAL_ResponceCurve_bio_16),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_bio_16<-GLOBAL_ResponceCurve_bio_16[grep(x=colnames(GLOBAL_ResponceCurve_bio_16),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_bio_16<-rbind(sp.names,LOCAL_ResponceCurve_bio_16 )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_bio_16<-rbind(sp.names,GLOBAL_ResponceCurve_bio_16 )
#


#for (sp in sp.names[c(89 ,92  ,12, 59 ,63 ,42 ,36 ,62,  1 ,83, 60 ,52 ,38 ,64 ,13 ,17 ,53 ,79 ,19 ,29, 61, 18 ,91, 51 ,22, 24 ,71 ,78)]) {
  #sp<-"Acacia.dealbata"

#png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)

    #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
#  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
#      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
#  par(mfrow=c(3,2))
#  
### plotting responces for Acacia.dealbata
### bio_07 Local
#plot(as.numeric(LOCAL_ResponceCurve_bio_07[-1,LOCAL_ResponceCurve_bio_07[1,]==sp])~x_bio_07 , pch=20, cex=0.005, main="LOCAL_RC.Bio_07")
#plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
#
#
#
#
#
#
#
#
#
### plotting responces for Acacia.dealbata
### bio_10 Local
#plot(as.numeric(LOCAL_ResponceCurve_bio_10[-1,LOCAL_ResponceCurve_bio_10[1,]==sp])~x_bio_10 , pch=20, cex=0.005, main="LOCAL_RC.Bio_10")
#plot(as.numeric(GLOBAL_ResponceCurve_bio_10[-1,GLOBAL_ResponceCurve_bio_10[1,]==sp])~x1_bio_10 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_10")






## plotting responces for Acacia.dealbata
## bio_16 Local
#plot(as.numeric(LOCAL_ResponceCurve_bio_16[-1,LOCAL_ResponceCurve_bio_16[1,]==sp])~x_bio_16 , pch=20, cex=0.005, main="LOCAL_RC.Bio_16")
#plot(as.numeric(GLOBAL_ResponceCurve_bio_16[-1,GLOBAL_ResponceCurve_bio_16[1,]==sp])~x1_bio_16 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_16")
#
#
#dev.off()
#gc(reset=T)
#unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#}


### preparing the second tranches of values 
# clear memory
gc(reset=T)



### BIO_17
## selecting the x
x_bio_17 <-LOCAL_ResponceCurve_bio_17$CHELSA_bio10_17 ## metto da parte le x
x1_bio_17<-GLOBAL_ResponceCurve_bio_17$CHELSA_bio10_17 ## metto da parte le x

### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_bio_17<- LOCAL_ResponceCurve_bio_17[grep( x=colnames(LOCAL_ResponceCurve_bio_17),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_bio_17<-GLOBAL_ResponceCurve_bio_17[grep(x=colnames(GLOBAL_ResponceCurve_bio_17),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_bio_17<-rbind(sp.names,LOCAL_ResponceCurve_bio_17 )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_bio_17<-rbind(sp.names,GLOBAL_ResponceCurve_bio_17 )


### LOCAL_ResponceCurve_BLDFIE_M
## selecting the x
x_BLDFIE_M <-LOCAL_ResponceCurve_BLDFIE_M$BLDFIE_M ## metto da parte le x
x1_BLDFIE_M<-GLOBAL_ResponceCurve_BLDFIE_M$BLDFIE_M ## metto da parte le x

### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_BLDFIE_M<- LOCAL_ResponceCurve_BLDFIE_M[grep( x=colnames(LOCAL_ResponceCurve_BLDFIE_M),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_BLDFIE_M<-GLOBAL_ResponceCurve_BLDFIE_M[grep(x=colnames(GLOBAL_ResponceCurve_BLDFIE_M),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_BLDFIE_M<-rbind(sp.names,LOCAL_ResponceCurve_BLDFIE_M )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_BLDFIE_M<-rbind(sp.names,GLOBAL_ResponceCurve_BLDFIE_M )



### pop.dens
## selecting the x
x_pop_dens <-LOCAL_ResponceCurve_pop.dens$Population.density.2000 ## metto da parte le x
x1_pop_dens<-GLOBAL_ResponceCurve_pop.dens$Population.density.2000  ## metto da parte le x

### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_pop.dens<- LOCAL_ResponceCurve_pop.dens[grep( x=colnames(LOCAL_ResponceCurve_pop.dens),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_pop.dens<-GLOBAL_ResponceCurve_pop.dens[grep(x=colnames(GLOBAL_ResponceCurve_pop.dens),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_pop.dens<-rbind(sp.names,LOCAL_ResponceCurve_pop.dens )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_pop.dens<-rbind(sp.names,GLOBAL_ResponceCurve_pop.dens )






### TWI
## selecting the x
x_TWI <-LOCAL_ResponceCurve_TWI$TWI ## metto da parte le x
x1_TWI<-GLOBAL_ResponceCurve_TWI$TWI   ## metto da parte le x

### seleziono solo le colonne con tss
#
#LOCAL_ResponceCurve_TWI<- LOCAL_ResponceCurve_TWI[grep( x=colnames(LOCAL_ResponceCurve_TWI),patter="TSS", value=T)]
#GLOBAL_ResponceCurve_TWI<-GLOBAL_ResponceCurve_TWI[grep(x=colnames(GLOBAL_ResponceCurve_TWI),patter="TSS", value=T)]
#
#
### adding sp.names ina simple format
#LOCAL_ResponceCurve_TWI<-rbind(sp.names,LOCAL_ResponceCurve_TWI )
### riaggiungo le x con un valore sopra
##LOCAL_ResponceCurve_bio_07<-cbind(LOCAL_ResponceCurve_bio_07, c("x",x))
#GLOBAL_ResponceCurve_TWI<-rbind(sp.names,GLOBAL_ResponceCurve_TWI )
#




##for (sp in sp.names) {
#  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_last_four_RC.png", sep="")), 
#      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
#  par(mfrow=c(4,2))
#  
#
### plotting responces for Acacia.dealbata
### bio_17 Local
#plot(as.numeric( LOCAL_ResponceCurve_bio_17[-1, LOCAL_ResponceCurve_bio_17[1,]==sp])~ x_bio_17 , pch=20, cex=0.005, main= "LOCAL_RC.Bio_17")
#plot(as.numeric(GLOBAL_ResponceCurve_bio_17[-1,GLOBAL_ResponceCurve_bio_17[1,]==sp])~x1_bio_17 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_17")
#
#
#
#
#
#
#
#
#
#
### plotting responces for Acacia.dealbata
### bio_16 Local
#plot(as.numeric( LOCAL_ResponceCurve_BLDFIE_M[-1, LOCAL_ResponceCurve_BLDFIE_M[1,]==sp])~ x_BLDFIE_M , pch=20, cex=0.005, main= "LOCAL_RC.BLDFIE_M")
#plot(as.numeric(GLOBAL_ResponceCurve_BLDFIE_M[-1,GLOBAL_ResponceCurve_BLDFIE_M[1,]==sp])~x1_BLDFIE_M , pch=20, cex=0.005, main="GLOBAL_RC.BLDFIE_M")
#
#
#
#
### plotting responces for Acacia.dealbata
### bio_16 Local
#plot(as.numeric( LOCAL_ResponceCurve_pop.dens[-1, LOCAL_ResponceCurve_pop.dens[1,]==sp])~x_pop_dens , pch=20, cex=0.005, main= "LOCAL_RC.POP_DENS")
#plot(as.numeric(GLOBAL_ResponceCurve_pop.dens[-1,GLOBAL_ResponceCurve_pop.dens[1,]==sp])~x1_pop_dens , pch=20, cex=0.005, main="GLOBAL_RC.POP_DENS")
#
#
#
#
### plotting responces for Acacia.dealbata
### bio_16 Local
#plot(as.numeric( LOCAL_ResponceCurve_TWI[-1, LOCAL_ResponceCurve_TWI[1,]==sp])~x_TWI , pch=20, cex=0.005, main= "LOCAL_RC.TWI")
#plot(as.numeric(GLOBAL_ResponceCurve_TWI[-1,GLOBAL_ResponceCurve_TWI[1,]==sp])~x1_TWI , pch=20, cex=0.005, main="GLOBAL_RC.TWI")
#
#dev.off()
#gc(reset=T)
#unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#
#}






#### PLOTTING ALL SPECIES RC ####
makeTransparent = function(..., alpha=0.5) {
  
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  
  return(newColor)
  
} ## defining the function transparent
# define the function
std <- function(x) sd(x)/sqrt(length(x))
# FIRST PLEASE RUN LINES: 1240-1307
## AND THEN 1368-1442 
## mi permettono di preparare i dataset con x e y corrette e tutte le specie
## Mettiamo da parte le Specie
# MED Species
med.sp <- RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med", "SpName"]
## TROP species
trop.sp <- RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop", "SpName"]
## TEMP species
temp.sp <- RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp", "SpName"]


## BIO_07 ####
#par(mfrow=c(1,2))
## i sample to all other curves an exact number of points
## 2751
#### plotting bio_07 _LOCAL 
png(filename=file.path( paste("Resp.curves_bio07.png", sep="_")), width = 1200, height = 550, pointsize = 12, bg = "white", res = NA)
par(mfrow=c(1,2))

plot(as.numeric(LOCAL_ResponceCurve_bio_07[,"Acacia.saligna"])~x_bio_07 , col="white" ,lwd=0.0000001, pch=20, cex=0.000000000000005, main="LOCAL Respoince Curve Bio_07", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")

for (sp in sp.names) {
  #sp<-"Acacia.dealbata"
  print(sp)
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
#  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
#      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
#  par(mfrow=c(3,2))
#  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(LOCAL_ResponceCurve_bio_07[,sp])~x_bio_07 ,lty=1,lwd=0.5, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}

## to
LOCAL_ResponceCurve_bio_07<-LOCAL_ResponceCurve_bio_07[,]

LOCAL_ResponceCurve_bio_07[,]<-apply((LOCAL_ResponceCurve_bio_07[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((LOCAL_ResponceCurve_bio_07))))
mean(as.numeric(LOCAL_ResponceCurve_bio_07[1,]))

lines(rowMeans(LOCAL_ResponceCurve_bio_07)~x_bio_07 , pch=20, cex=6,lwd=7.5, col="black")
#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   

### i need to eliminate the rows i don't  have 
RangeChange_Traits<-RangeChange_Traits[,]
rownames(RangeChange_Traits)<-1:92 ## ATTENTION, THE ORDER may  NOT THE SAME HERE

# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])~x_bio_07 ,lwd=7.5, col="forestgreen")
## TEMPERATE
lines(rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x_bio_07 , pch=20, cex=6,lwd=7.5, col="lightseagreen")
## MEDITERRANEAN 
lines(rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])~x_bio_07 , pch=20, cex=6,lwd=7.5, col="darkred")
#dev.off()
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")

#### FAMILY
#lines(rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x_bio_07 , pch=20, cex=6,lwd=7.5, col="forestgreen")

#lines(rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x_bio_07 , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x_bio_07 , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x_bio_07 , pch=20, cex=6,lwd=7.5, col="orange")


#### plotting bio_07 Global 

### correcting the global DATASET

load("RangeChange_Traits.Rda")
load("RangeChange_Traits_new.Rda")
## fro now
RangeChange_Traits <- RangeChange_Traits_new


RangeChange_Traits$Main.Biome_AGGR <- rep("temp",93)
RangeChange_Traits[grep("Temperate",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"]### these are temperate yet
## xeric
RangeChange_Traits[grep("Deserts",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
RangeChange_Traits[grep("Mediterranean",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
## tropical
RangeChange_Traits[grep("Tropical",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "trop"

### crrecting it 

### i need to eliminate the rows i don't  have 
RangeChange_Traits_glob<-RangeChange_Traits[c(-11,-20,-27,-30,-49,-55,-62,-69),]
rownames(RangeChange_Traits_glob)<-1:85 ## i have only to be sure the order is the same

## vado a fare una sorta di rarefazione dei punti tra il range 
a <- sort(sample(2:length(x1_bio_07),2751))
#plot(,main="GLOBAL Respoince Curve Bio_07", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
plot(as.numeric(GLOBAL_ResponceCurve_bio_07[,"Acacia.dealbata"])~x1_bio_07 , col="white" ,pch=20,lwd=0.1,cex=0.00000000001, main="GLOBAL Respoince Curve Bio_07", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")

for (sp in sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)]) {
  #sp<-"Acacia.dealbata"
  print(sp)
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(GLOBAL_ResponceCurve_bio_07[a,sp])~x1_bio_07[a] , lty=1,lwd=0.0000001, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}
### Plotting a single mean value
GLOBAL_ResponceCurve_bio_07<-GLOBAL_ResponceCurve_bio_07[,]

GLOBAL_ResponceCurve_bio_07[,]<-apply((GLOBAL_ResponceCurve_bio_07[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((GLOBAL_ResponceCurve_bio_07))))
mean(as.numeric(GLOBAL_ResponceCurve_bio_07[1,]))
## one line for alla species or go down to add three lines one from each biome
lines(rowMeans(GLOBAL_ResponceCurve_bio_07[a,])~x1_bio_07[a] , pch=20, cex=6,lwd=7.5, col="black")

#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE
     
     ## TROPICAL LINE ## ? SULLE COLONNE   
   # as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
    ### the columns correnspond to the species for which i do the mean in the other dataset
                        lines(rowMeans(GLOBAL_ResponceCurve_bio_07[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",]))])~x1_bio_07[a] , pch=20, cex=6,lwd=7.5, col="forestgreen")
                        
                        lines(rowMeans(GLOBAL_ResponceCurve_bio_07[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_bio_07[a] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
                        lines(rowMeans(GLOBAL_ResponceCurve_bio_07[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="med",]))])~x1_bio_07[a] , pch=20, cex=6,lwd=7.5, col="darkred")
                        #lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")
               dev.off()
                        
                        #### TRY with families
                        #lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Family_Agg =="Poaceae",]))])~x1_bio_07 , pch=20, cex=6,lwd=7.5, col="forestgreen")
                        
                        #lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Family_Agg=="Compositae",]))])~x1_bio_07 , pch=20, cex=6,lwd=7.5, col="lightseagreen")
                        #lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x1_bio_07 , pch=20, cex=6,lwd=7.5, col="darkred")
                        #lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x1_bio_07 , pch=20, cex=6,lwd=7.5, col="orange")
                        

## BIO_10####
png(filename=file.path( paste("Resp.curves_bio10.png", sep="_")), width = 1200, height = 550, pointsize = 12, bg = "white", res = NA)
               
par(mfrow=c(1,2))
 
                                               
## vado a fare una sorta di rarefazione dei punti tra il range 
a <- sort(sample(2:length(x_bio_10),2751))                        

#### plotting bio_10 _LOCAL 
plot(as.numeric(LOCAL_ResponceCurve_bio_10[,"Acacia.dealbata"])~x_bio_10  , lwd=0.00001, pch=20, cex=0.001,col="white", main="LOCAL Respoince Curve Bio_10", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")

for (sp in sp.names) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(LOCAL_ResponceCurve_bio_10[a,sp])~x_bio_10[a] , lty=1,lwd=0.2, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


LOCAL_ResponceCurve_bio_10<-LOCAL_ResponceCurve_bio_10[,]

LOCAL_ResponceCurve_bio_10[,]<-apply((LOCAL_ResponceCurve_bio_10[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((LOCAL_ResponceCurve_bio_10))))
mean(as.numeric(LOCAL_ResponceCurve_bio_10[1,]))

lines(rowMeans(LOCAL_ResponceCurve_bio_10[a,])~x_bio_10[a] , pch=20, cex=6,lwd=7.5, col="black")

#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(LOCAL_ResponceCurve_bio_10[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])~x_bio_10[a] , pch=20, cex=6,lwd=7.5, col="forestgreen")
## TEMPERATE
lines(rowMeans(LOCAL_ResponceCurve_bio_10[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x_bio_10[a] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
## MEDITERRANEAN 
lines(rowMeans(LOCAL_ResponceCurve_bio_10[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])~x_bio_10[a] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")

#### FAMILY
#lines(rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x_bio_10 , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x_bio_10 , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x_bio_10 , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x_bio_10 , pch=20, cex=6,lwd=7.5, col="orange")



#### plotting bio_10 Global 
plot(as.numeric(GLOBAL_ResponceCurve_bio_10[,"Acacia.dealbata"])~x1_bio_10 ,col="white",lwd=0.1, pch=20, cex=0.000000000000005, main="GLOBAL Responce Curve Bio_10", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
a <- sort(sample(2:length(x1_bio_10),2751))                        

for (sp in  sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)]) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(GLOBAL_ResponceCurve_bio_10[a,sp])~x1_bio_10[a] , lty=1,lwd=0.2, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


GLOBAL_ResponceCurve_bio_10<-GLOBAL_ResponceCurve_bio_10[,]

GLOBAL_ResponceCurve_bio_10[,]<-apply((GLOBAL_ResponceCurve_bio_10[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((GLOBAL_ResponceCurve_bio_10))))
mean(as.numeric(GLOBAL_ResponceCurve_bio_10[1,]))

lines(rowMeans(GLOBAL_ResponceCurve_bio_10[a,])~x1_bio_10[a] , pch=20, cex=6,lwd=7.5, col="black")


#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(GLOBAL_ResponceCurve_bio_10[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",]))])~x1_bio_10[a] , pch=20, cex=6,lwd=7.5, col="forestgreen")

lines(rowMeans(GLOBAL_ResponceCurve_bio_10[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_bio_10[a] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
lines(rowMeans(GLOBAL_ResponceCurve_bio_10[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="med",]))])~x1_bio_10[a] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")

#### FAMILY
#lines(rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x1_bio_10 , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x1_bio_10 , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x1_bio_10 , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x1_bio_10 , pch=20, cex=6,lwd=7.5, col="orange")
dev.off()



## BIO_16####
png(filename=file.path( paste("Resp.curves_bio16.png", sep="_")), width = 1200, height = 550, pointsize = 12, bg = "white", res = NA)

par(mfrow=c(1,2))
#sample()
#### plotting bio_16 _LOCAL 
plot(as.numeric(LOCAL_ResponceCurve_bio_16[,"Acacia.dealbata"])~x_bio_16 ,col="white" , lwd=0.1,pch=20, cex=0.000000000000005, main="LOCAL Respoince Curve bio_16", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
a <- sort(sample(2:length(x_bio_16),2751))                        

for (sp in sp.names[]) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(LOCAL_ResponceCurve_bio_16[a,sp])~x_bio_16[a] , lty=1,lwd=0.2,col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


LOCAL_ResponceCurve_bio_16<-LOCAL_ResponceCurve_bio_16[,]

LOCAL_ResponceCurve_bio_16[,]<-apply((LOCAL_ResponceCurve_bio_16[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((LOCAL_ResponceCurve_bio_16))))
mean(as.numeric(LOCAL_ResponceCurve_bio_16[1,]))

lines(rowMeans(LOCAL_ResponceCurve_bio_16[a,])~x_bio_16[a] , pch=20, cex=6,lwd=7.5, col="black")
#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(LOCAL_ResponceCurve_bio_16[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])~x_bio_16[a] , pch=20, cex=6,lwd=7.5, col="forestgreen")
## TEMPERATE
lines(rowMeans(LOCAL_ResponceCurve_bio_16[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x_bio_16[a] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
## MEDITERRANEAN 
lines(rowMeans(LOCAL_ResponceCurve_bio_16[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])~x_bio_16[a] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")


#### FAMILY
#lines(rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x_bio_16 , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x_bio_16 , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x_bio_16 , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x_bio_16 , pch=20, cex=6,lwd=7.5, col="orange")




#### plotting bio_16 Global 
plot(as.numeric(GLOBAL_ResponceCurve_bio_16[,"Acacia.dealbata"])~x1_bio_16 ,col="white" , lwd=0.1,pch=20, cex=0.000000000000005, main="GLOBAL Respoince Curve bio_16", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
a <- sort(sample(2:length(x1_bio_16),2751))                        

for (sp in  sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)]) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(GLOBAL_ResponceCurve_bio_16[a,sp])~x1_bio_16[a] , lty=1,lwd=0.2,col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


GLOBAL_ResponceCurve_bio_16<-GLOBAL_ResponceCurve_bio_16[,]

GLOBAL_ResponceCurve_bio_16[,]<-apply((GLOBAL_ResponceCurve_bio_16[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((GLOBAL_ResponceCurve_bio_16))))
mean(as.numeric(GLOBAL_ResponceCurve_bio_16[89,]))

lines(rowMeans(GLOBAL_ResponceCurve_bio_16[a,])~x1_bio_16[a] , pch=20, cex=6,lwd=7.5, col="black")




#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(GLOBAL_ResponceCurve_bio_16[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",]))])~x1_bio_16[a] , pch=20, cex=6,lwd=7.5, col="forestgreen")

lines(rowMeans(GLOBAL_ResponceCurve_bio_16[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_bio_16[a] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
lines(rowMeans(GLOBAL_ResponceCurve_bio_16[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="med",]))])~x1_bio_16[a] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")
dev.off()

#### FAMILY
#lines(rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x1_bio_16 , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x1_bio_16 , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x1_bio_16 , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x1_bio_16 , pch=20, cex=6,lwd=7.5, col="orange")


## BIO_17####
png(filename=file.path( paste("Resp.curves_bio17.png", sep="_")), width = 1200, height = 550, pointsize = 12, bg = "white", res = NA)

par(mfrow=c(1,2))
#### plotting bio_17 _LOCAL 
plot(as.numeric(LOCAL_ResponceCurve_bio_17[,"Acacia.dealbata"])~x_bio_17 ,lwd=0.1,col="white" , pch=20, cex=0.000000000000005, main="LOCAL Respoince Curve bio_17", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
a <- sort(sample(2:length(x_bio_17),2751))                        

for (sp in sp.names) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(LOCAL_ResponceCurve_bio_17[a,sp])~x_bio_17[a] , lty=1,lwd=0.2, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


LOCAL_ResponceCurve_bio_17<-LOCAL_ResponceCurve_bio_17[,]

LOCAL_ResponceCurve_bio_17[,]<-apply((LOCAL_ResponceCurve_bio_17[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((LOCAL_ResponceCurve_bio_17))))
mean(as.numeric(LOCAL_ResponceCurve_bio_17[93,]))

lines(rowMeans(LOCAL_ResponceCurve_bio_17[a,])~x_bio_17[a] , pch=20, cex=6,lwd=7.5, col="black")

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(LOCAL_ResponceCurve_bio_17[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])~x_bio_17[a] , pch=20, cex=6,lwd=7.5, col="forestgreen")
## TEMPERATE
lines(rowMeans(LOCAL_ResponceCurve_bio_17[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x_bio_17[a] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
## MEDITERRANEAN 
lines(rowMeans(LOCAL_ResponceCurve_bio_17[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])~x_bio_17[a] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")



#### FAMILY
#lines(rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x_bio_17 , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x_bio_17 , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x_bio_17 , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x_bio_17 , pch=20, cex=6,lwd=7.5, col="orange")



#### plotting bio_17 Global 
plot(as.numeric(GLOBAL_ResponceCurve_bio_17[,"Acacia.dealbata"])~x1_bio_17 ,col="white" , lwd=0.1,pch=20, cex=0.000000000000005, main="GLOBAL Respoince Curve bio_17", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
a <- sort(sample(2:length(x1_bio_17),2751))  ## rarefaccio le x per limitare il range di valori estratti                      

for (sp in  sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)]) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(GLOBAL_ResponceCurve_bio_17[a,sp])~x1_bio_17[a] , lty=1,lwd=0.2,col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


GLOBAL_ResponceCurve_bio_17<-GLOBAL_ResponceCurve_bio_17[,]

GLOBAL_ResponceCurve_bio_17[,]<-apply((GLOBAL_ResponceCurve_bio_17[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((GLOBAL_ResponceCurve_bio_17))))
mean(as.numeric(GLOBAL_ResponceCurve_bio_17[1,]))

lines(rowMeans(GLOBAL_ResponceCurve_bio_17[a,])~x1_bio_17[a] , pch=20, cex=6,lwd=7.5, col="black")
#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(GLOBAL_ResponceCurve_bio_17[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",]))])~x1_bio_17[a] , pch=20, cex=6,lwd=7.5, col="forestgreen")

lines(rowMeans(GLOBAL_ResponceCurve_bio_17[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_bio_17[a] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
lines(rowMeans(GLOBAL_ResponceCurve_bio_17[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="med",]))])~x1_bio_17[a] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")
dev.off()
#### FAMILY
#lines(rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x1_bio_17 , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x1_bio_17 , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x1_bio_17 , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x1_bio_17 , pch=20, cex=6,lwd=7.5, col="orange")

## BLDFIE_M####
png(filename=file.path( paste("Resp.curves_BULK.png", sep="_")), width = 1200, height = 550, pointsize = 12, bg = "white", res = NA)

par(mfrow=c(1,2))
#### plotting BLDFIE_M _LOCAL 
plot(as.numeric(LOCAL_ResponceCurve_BLDFIE_M[,"Acacia.dealbata"])~x_BLDFIE_M ,col="white" ,lwd=0.1, pch=20, cex=0.000000000000005, main="LOCAL Respoince Curve BLDFIE_M", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
a <- sort(sample(2:length(x_BLDFIE_M),2751))                        

for (sp in sp.names) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(LOCAL_ResponceCurve_BLDFIE_M[a,sp])~x_BLDFIE_M[a] , lty=1,lwd=0.2, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


LOCAL_ResponceCurve_BLDFIE_M<-LOCAL_ResponceCurve_BLDFIE_M[,]

LOCAL_ResponceCurve_BLDFIE_M[,]<-apply((LOCAL_ResponceCurve_BLDFIE_M[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((LOCAL_ResponceCurve_BLDFIE_M))))
mean(as.numeric(LOCAL_ResponceCurve_BLDFIE_M[93,]))

lines(rowMeans(LOCAL_ResponceCurve_BLDFIE_M[a,])~x_BLDFIE_M[a] , pch=20, cex=6,lwd=7.5, col="black")

#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(LOCAL_ResponceCurve_BLDFIE_M[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])~x_BLDFIE_M[a] , pch=20, cex=6,lwd=7.5, col="forestgreen")
## TEMPERATE
lines(rowMeans(LOCAL_ResponceCurve_BLDFIE_M[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x_BLDFIE_M[a] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
## MEDITERRANEAN 
lines(rowMeans(LOCAL_ResponceCurve_BLDFIE_M[a,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])~x_BLDFIE_M[a] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")

#### FAMILY
#lines(rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x_BLDFIE_M , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x_BLDFIE_M , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x_BLDFIE_M , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x_BLDFIE_M , pch=20, cex=6,lwd=7.5, col="orange")



#### plotting BLDFIE_M Global 
plot(as.numeric(GLOBAL_ResponceCurve_BLDFIE_M[,"Acacia.dealbata"])~x1_BLDFIE_M ,col="white" ,lwd=0.1, pch=20, cex=0.000000000000005, main="GLOBAL Respoince Curve BLDFIE_M", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
a <- sort(sample(2:length(x1_BLDFIE_M),2751))  ## this is to reduce the x and predicted range of values plotted                      

for (sp in  sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)]) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(GLOBAL_ResponceCurve_BLDFIE_M[a,sp])~x1_BLDFIE_M[a] , lty=1,lwd=0.2, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


GLOBAL_ResponceCurve_BLDFIE_M<-GLOBAL_ResponceCurve_BLDFIE_M[,]

GLOBAL_ResponceCurve_BLDFIE_M[,]<-apply((GLOBAL_ResponceCurve_BLDFIE_M[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((GLOBAL_ResponceCurve_BLDFIE_M))))
mean(as.numeric(GLOBAL_ResponceCurve_BLDFIE_M[1,]))

lines(rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[a,])~x1_BLDFIE_M[a] , pch=20, cex=6,lwd=7.5, col="black")


#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",]))])~x1_BLDFIE_M[a] , pch=20, cex=6,lwd=7.5, col="forestgreen")

lines(rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_BLDFIE_M[a] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
lines(rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[a,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="med",]))])~x1_BLDFIE_M[a] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")
dev.off()

#### FAMILY
#lines(rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x1_BLDFIE_M , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x1_BLDFIE_M , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x1_BLDFIE_M , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x1_BLDFIE_M , pch=20, cex=6,lwd=7.5, col="orange")


## TWI####
png(filename=file.path( paste("Resp.curves_TWI.png", sep="_")), width = 1200, height = 550, pointsize = 12, bg = "white", res = NA)

par(mfrow=c(1,2))
#### plotting TWI _LOCAL 
plot(as.numeric(LOCAL_ResponceCurve_TWI[,"Acacia.dealbata"])~x_TWI ,lwd=0.1,col="white" , pch=20, cex=0.000000000000005, main="LOCAL Respoince Curve TWI", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
#a <- sort(sample(2:length(x_TWI),2751))  ## this is to reduce the x and predicted range of values plotted                      

for (sp in sp.names) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(LOCAL_ResponceCurve_TWI[,sp])~x_TWI , lty=1,lwd=0.2, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


LOCAL_ResponceCurve_TWI<-LOCAL_ResponceCurve_TWI[,]

LOCAL_ResponceCurve_TWI[,]<-apply((LOCAL_ResponceCurve_TWI[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((LOCAL_ResponceCurve_TWI))))
mean(as.numeric(LOCAL_ResponceCurve_TWI[93,]))

lines(rowMeans(LOCAL_ResponceCurve_TWI)~x_TWI , pch=20, cex=6,lwd=7.5, col="black")

#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])~x_TWI , pch=20, cex=6,lwd=7.5, col="forestgreen")
## TEMPERATE
lines(rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x_TWI , pch=20, cex=6,lwd=7.5, col="lightseagreen")
## MEDITERRANEAN 
lines(rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])~x_TWI , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")

#### FAMILY
#lines(rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x_TWI , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x_TWI , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x_TWI , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x_TWI , pch=20, cex=6,lwd=7.5, col="orange")



#### plotting TWI Global 
plot(as.numeric(GLOBAL_ResponceCurve_TWI[,"Acacia.dealbata"])~x1_TWI ,col="white" , pch=20, lwd=0.1,cex=0.000000000000005, main="GLOBAL Respoince Curve TWI", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
#a <- sort(sample(2:length(x1_TWI),2751))  ## this is to reduce the x and predicted range of values plotted                      

for (sp in  sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)]) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(GLOBAL_ResponceCurve_TWI[,sp])~x1_TWI , lty=1,lwd=0.2, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


GLOBAL_ResponceCurve_TWI<-GLOBAL_ResponceCurve_TWI[,]

GLOBAL_ResponceCurve_TWI[,]<-apply((GLOBAL_ResponceCurve_TWI[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((GLOBAL_ResponceCurve_TWI))))
mean(as.numeric(GLOBAL_ResponceCurve_TWI[1,]))

lines(rowMeans(GLOBAL_ResponceCurve_TWI)~x1_TWI , pch=20, cex=6,lwd=7.5, col="black")


#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",]))])~x1_TWI , pch=20, cex=6,lwd=7.5, col="forestgreen")

lines(rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_TWI , pch=20, cex=6,lwd=7.5, col="lightseagreen")
lines(rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="med",]))])~x1_TWI , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")
dev.off()
#### FAMILY
#lines(rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg =="Poaceae",]))])~x1_TWI , pch=20, cex=6,lwd=7.5, col="forestgreen")
#
#lines(rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])~x1_TWI , pch=20, cex=6,lwd=7.5, col="lightseagreen")
#lines(rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])~x1_TWI , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])~x1_TWI , pch=20, cex=6,lwd=7.5, col="orange")




## Population density####
png(filename=file.path( paste("Resp.curves_pop.dens.png", sep="_")), width = 1200, height = 550, pointsize = 12, bg = "white", res = NA)

par(mfrow=c(1,2))
#### plotting pop.dens _LOCAL 
plot(as.numeric(LOCAL_ResponceCurve_pop.dens[,"Acacia.dealbata"])~x_pop_dens ,col="white" , lwd=0.1,pch=20, cex=0.000000000000005, main="LOCAL Respoince Curve pop.dens", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
#a <- sort(sample(2:length(x_pop_dens),2751))  ## this is to reduce the x and predicted range of values plotted                      

for (sp in sp.names) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(LOCAL_ResponceCurve_pop.dens[,sp])~x_pop_dens[] , lty=1,lwd=0.2, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


LOCAL_ResponceCurve_pop.dens<-LOCAL_ResponceCurve_pop.dens[,]

LOCAL_ResponceCurve_pop.dens[,]<-apply((LOCAL_ResponceCurve_pop.dens[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((LOCAL_ResponceCurve_pop.dens))))
mean(as.numeric(LOCAL_ResponceCurve_pop.dens[93,]))

lines(rowMeans(LOCAL_ResponceCurve_pop.dens[,])~x_pop_dens[] , pch=20, cex=6,lwd=7.5, col="black")

#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])~x_pop_dens[] , pch=20, cex=6,lwd=7.5, col="forestgreen")
## TEMPERATE
lines(rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x_pop_dens[] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
## MEDITERRANEAN 
lines(rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])~x_pop_dens[] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")




#### plotting pop.dens Global 
plot(as.numeric(GLOBAL_ResponceCurve_pop.dens[,"Acacia.dealbata"])~x1_pop_dens , col="white" ,lwd=0.1,pch=20, cex=0.000000000000005, main="GLOBAL Respoince Curve pop.dens", ylim=c(0,1000),ylab="Probability of occurrence", xlab="Environmental values")
#a <- sort(sample(2:length(x1_pop_dens),2751))  ## this is to reduce the x and predicted range of values plotted                      

for (sp in  sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)]) {
  #sp<-"Acacia.dealbata"
  ifelse(sp %in% med.sp, color <- "darkred",
         ifelse(sp %in% trop.sp, color <- "forestgreen",color <- "lightseagreen"))
  
  
  #png(filename=file.path("Desktop\\", paste(sp,"first_three_RC.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  #ecospat.plot.overlap.test(sim.test, "D", paste(myRespName,"Similarity"))
  
  
  #  png(filename=file.path(paste("D:\\Luigi\\Dataset e modelli DEF\\Consensus maps\\",sp,"_first_three_RC.png", sep="")), 
  #      width = 862, height = 1136, pointsize = 12, bg = "white", res = NA)
  #  par(mfrow=c(3,2))
  #  
  ## plotting responces for Acacia.dealbata
  ## bio_07 Local
  lines(as.numeric(GLOBAL_ResponceCurve_pop.dens[,sp])~x1_pop_dens[] , lty=1,lwd=0.2, col=makeTransparent(color, alpha=0.4))
  #plot(as.numeric(GLOBAL_ResponceCurve_bio_07[-1,GLOBAL_ResponceCurve_bio_07[1,]==sp])~x1_bio_07 , pch=20, cex=0.005, main="GLOBAL_RC.Bio_07")
}


GLOBAL_ResponceCurve_pop.dens<-GLOBAL_ResponceCurve_pop.dens[,]

GLOBAL_ResponceCurve_pop.dens[,]<-apply((GLOBAL_ResponceCurve_pop.dens[,]),2,as.numeric)
#LOCAL_ResponceCurve_bio_07<-as.matrix(LOCAL_ResponceCurve_bio_07)


#sapply(LOCAL_ResponceCurve_bio_07, as.numeric)

#apply((LOCAL_ResponceCurve_bio_07[,]),1,mean)
sum(is.na(rowMeans((GLOBAL_ResponceCurve_pop.dens))))
mean(as.numeric(GLOBAL_ResponceCurve_pop.dens[1,]))

lines(rowMeans(GLOBAL_ResponceCurve_pop.dens[,])~x1_pop_dens[] , pch=20, cex=6,lwd=7.5, col="black")

#### SKIP THIS PART IF YOU ONLY WANT A SINGLE MEAN VALUE

## TROPICAL LINE ## ? SULLE COLONNE   
# as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",])) ## this tell me which columns i want
### the columns correnspond to the species for which i do the mean in the other dataset
lines(rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="trop",]))])~x1_pop_dens[] , pch=20, cex=6,lwd=7.5, col="forestgreen")

lines(rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_pop_dens[] , pch=20, cex=6,lwd=7.5, col="lightseagreen")
lines(rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="med",]))])~x1_pop_dens[] , pch=20, cex=6,lwd=7.5, col="darkred")
#lines(rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits_glob[RangeChange_Traits_glob$Main.Biome_AGGR=="temp",]))])~x1_bio_07 , pch=20, cex=6,lwd=5, col="lightseagreen")
dev.off()










##### CURVES WITH A GG_PLOT GAM ###########
#### THIS IS FOR THE BIOME OF ORIGIN
# BIO_07######
## local
library(ggplot2)
sp<-ggplot(data=LOCAL_ResponceCurve_bio_07) +
 # geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,-94])-apply(LOCAL_ResponceCurve_bio_07[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
 # geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,-94])+apply(LOCAL_ResponceCurve_bio_07[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Local_Bio_07.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_07) +
 # geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,-94])-apply(GLOBAL_ResponceCurve_bio_07[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
 # geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,-94])+apply(GLOBAL_ResponceCurve_bio_07[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
 #geom_rect(aes(xmin =min(x1_bio_07), xmax =  min(x_bio_07), ymin = -Inf, ymax = Inf),fill="grey" ,alpha = .90) +
  annotate("rect",xmin = min(x1_bio_07), xmax =  min(x_bio_07), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_07), xmax =  max(x1_bio_07), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  #geom_rect(aes(xmin =max(x_bio_07), xmax =  max(x1_bio_07), ymin = -Inf, ymax = Inf),fill="grey" , alpha = .10)+

  #scale_fill_manual(values = c("a" = "grey70"))+
  #annotate("rect", min(x1_bio_07), xmax =  min(x_bio_07), ymin = -Inf, ymax = Inf, alpha=0.2, fill="grey") +
                               #,"Democratic" = "blue3"))
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000)) 
  
ggsave("Global_Bio_07.png", width = 10, height = 4)

### BIO_10 ####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_bio_10) +
  #geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,-94])-apply(LOCAL_ResponceCurve_bio_10[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,-94])+apply(LOCAL_ResponceCurve_bio_10[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Local_Bio_10.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_10) +
  #geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,-94])-apply(GLOBAL_ResponceCurve_bio_10[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,-94])+apply(GLOBAL_ResponceCurve_bio_10[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_bio_10), xmax =  min(x_bio_10), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_10), xmax =  max(x1_bio_10), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Global_Bio_10.png", width = 10, height = 4)

### BIO_16#####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_bio_16) +
  #geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,-94])-apply(LOCAL_ResponceCurve_bio_16[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,-94])+apply(LOCAL_ResponceCurve_bio_16[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
 
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Local_bio_16.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_16) +
  #geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,-94])-apply(GLOBAL_ResponceCurve_bio_16[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,-94])+apply(GLOBAL_ResponceCurve_bio_16[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_bio_16), xmax =  min(x_bio_16), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_16), xmax =  max(x1_bio_16), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Global_bio_16.png", width = 10, height = 4)

#### BIO_17####

## local

sp<-ggplot(data=LOCAL_ResponceCurve_bio_17) +
 #geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,-94])-apply(LOCAL_ResponceCurve_bio_17[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
 #geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,-94])+apply(LOCAL_ResponceCurve_bio_17[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
   #geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Local_bio_17.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_17) +
  #geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,-94])-apply(GLOBAL_ResponceCurve_bio_17[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,-94])+apply(GLOBAL_ResponceCurve_bio_17[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_bio_17), xmax =  min(x_bio_17), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_17), xmax =  max(x1_bio_17), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Global_bio_17.png", width = 10, height = 4)

####BULK#####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_BLDFIE_M) +
  #geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,-94])-apply(LOCAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,-94])+apply(LOCAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
 # geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Local_BLDFIE_M.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_BLDFIE_M) +
  #geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,-94])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,-94])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_BLDFIE_M), xmax =  min(x_BLDFIE_M), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_BLDFIE_M), xmax =  max(x1_BLDFIE_M), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Global_BLDFIE_M.png", width = 10, height = 4)
####TWI####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_TWI) +
  #geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,-94])-apply(LOCAL_ResponceCurve_TWI[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,-94])+apply(LOCAL_ResponceCurve_TWI[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Local_TWI.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_TWI) +
  #geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,-94])-apply(GLOBAL_ResponceCurve_TWI[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,-94])+apply(GLOBAL_ResponceCurve_TWI[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_TWI), xmax =  min(x_TWI), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_TWI), xmax =  max(x1_TWI), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Global_TWI.png", width = 10, height = 4)
####POP.DENS####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_pop.dens) +
  #geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,-94])-apply(LOCAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,-94])+apply(LOCAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Local_pop.dens.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_pop.dens) +
  #geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,-94])-apply(GLOBAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,-94])+apply(GLOBAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
 # annotate("rect",xmin = min(x1_pop_dens), xmax =  min(x_pop_dens), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
#  annotate("rect",xmin =max(x_pop_dens), xmax =  max(x1_pop_dens), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  ## the two have almost the same range of predictions 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))],1,std) ), color = "darkred"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))],1,std) ), color = "lightseagreen",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="trop",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="temp",]))]) ), color = "lightseagreen",   size=2.3)  + 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Main.Biome_AGGR=="med",]))]) ), color = "darkred" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("Global_pop.dens.png", width = 10, height = 4)






##### CURVES WITH A GG_PLOT GAM ###########
#### THIS IS FOR THE fAMILY OF ORIGIn
## the colour scheme is the same of the initial boxplot of the first version on the ppt
# BIO_07######
## local
library(ggplot2)
sp<-ggplot(data=LOCAL_ResponceCurve_bio_07) +
 # geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,-94])-apply(LOCAL_ResponceCurve_bio_07[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
 # geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,-94])+apply(LOCAL_ResponceCurve_bio_07[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color =         "#90d743" ,    size=2.3)  +
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color =       "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color =    "#31688e",   size=2.3)  + 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Local_Bio_07.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_07) +
  # geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,-94])-apply(GLOBAL_ResponceCurve_bio_07[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  # geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,-94])+apply(GLOBAL_ResponceCurve_bio_07[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  #geom_rect(aes(xmin =min(x1_bio_07), xmax =  min(x_bio_07), ymin = -Inf, ymax = Inf),fill="grey" ,alpha = .90) +
  annotate("rect",xmin = min(x1_bio_07), xmax =  min(x_bio_07), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_07), xmax =  max(x1_bio_07), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  #geom_rect(aes(xmin =max(x_bio_07), xmax =  max(x1_bio_07), ymin = -Inf, ymax = Inf),fill="grey" , alpha = .10)+
  
  #scale_fill_manual(values = c("a" = "grey70"))+
  #annotate("rect", min(x1_bio_07), xmax =  min(x_bio_07), ymin = -Inf, ymax = Inf, alpha=0.2, fill="grey") +
  #,"Democratic" = "blue3"))
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000)) 

ggsave("FAM_Global_Bio_07.png", width = 10, height = 4)

### BIO_10 ####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_bio_10) +
  #geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,-94])-apply(LOCAL_ResponceCurve_bio_10[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,-94])+apply(LOCAL_ResponceCurve_bio_10[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Local_Bio_10.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_10) +
  #geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,-94])-apply(GLOBAL_ResponceCurve_bio_10[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,-94])+apply(GLOBAL_ResponceCurve_bio_10[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_bio_10), xmax =  min(x_bio_10), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_10), xmax =  max(x1_bio_10), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Global_Bio_10.png", width = 10, height = 4)

### BIO_16#####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_bio_16) +
  #geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,-94])-apply(LOCAL_ResponceCurve_bio_16[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,-94])+apply(LOCAL_ResponceCurve_bio_16[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Local_bio_16.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_16) +
  #geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,-94])-apply(GLOBAL_ResponceCurve_bio_16[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,-94])+apply(GLOBAL_ResponceCurve_bio_16[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_bio_16), xmax =  min(x_bio_16), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_16), xmax =  max(x1_bio_16), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Global_bio_16.png", width = 10, height = 4)

#### BIO_17####

## local

sp<-ggplot(data=LOCAL_ResponceCurve_bio_17) +
  #geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,-94])-apply(LOCAL_ResponceCurve_bio_17[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,-94])+apply(LOCAL_ResponceCurve_bio_17[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Local_bio_17.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_17) +
  #geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,-94])-apply(GLOBAL_ResponceCurve_bio_17[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,-94])+apply(GLOBAL_ResponceCurve_bio_17[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_bio_17), xmax =  min(x_bio_17), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_17), xmax =  max(x1_bio_17), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Global_bio_17.png", width = 10, height = 4)

####BULK#####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_BLDFIE_M) +
  #geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,-94])-apply(LOCAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,-94])+apply(LOCAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  # geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Local_BLDFIE_M.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_BLDFIE_M) +
  #geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,-94])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,-94])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_BLDFIE_M), xmax =  min(x_BLDFIE_M), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_BLDFIE_M), xmax =  max(x1_BLDFIE_M), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Global_BLDFIE_M.png", width = 10, height = 4)
####TWI####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_TWI) +
  #geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,-94])-apply(LOCAL_ResponceCurve_TWI[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,-94])+apply(LOCAL_ResponceCurve_TWI[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Local_TWI.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_TWI) +
  #geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,-94])-apply(GLOBAL_ResponceCurve_TWI[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,-94])+apply(GLOBAL_ResponceCurve_TWI[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_TWI), xmax =  min(x_TWI), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_TWI), xmax =  max(x1_TWI), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Global_TWI.png", width = 10, height = 4)
####POP.DENS####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_pop.dens) +
  #geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,-94])-apply(LOCAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,-94])+apply(LOCAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Local_pop.dens.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_pop.dens) +
  #geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,-94])-apply(GLOBAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,-94])+apply(GLOBAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "#90d743"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  # annotate("rect",xmin = min(x1_pop_dens), xmax =  min(x_pop_dens), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  #  annotate("rect",xmin =max(x_pop_dens), xmax =  max(x1_pop_dens), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  ## the two have almost the same range of predictions 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))],1,std) ), color = "#440154"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))],1,std) ), color = "#31688e",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))],1,std) ),size=1, color = "#35b779" ,linetype = "longdash", alpha=0.5)  +
  
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))],1,std) ),size=1, color = "#90d743" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="other",]))]) ), color = "#90d743" ,    size=2.3)  +
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Poaceae",]))]) ), color = "#35b779" ,    size=2.3)  +
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Compositae",]))]) ), color = "#31688e",   size=2.3)  + 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$Family_Agg=="Amaranthaceae",]))]) ), color = "#440154" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,-94]) ), color = "#90d743",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("FAM_Global_pop.dens.png", width = 10, height = 4)









##### CURVES WITH A GG_PLOT GAM ###########
#### THIS IS FOR THE GF
# BIO_07######
## local
library(ggplot2)
sp<-ggplot(data=LOCAL_ResponceCurve_bio_07) +
  # geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,-94])-apply(LOCAL_ResponceCurve_bio_07[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  # geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,-94])+apply(LOCAL_ResponceCurve_bio_07[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_07,y = rowMeans(LOCAL_ResponceCurve_bio_07[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Local_Bio_07.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_07) +
  # geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,-94])-apply(GLOBAL_ResponceCurve_bio_07[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  # geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,-94])+apply(GLOBAL_ResponceCurve_bio_07[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  #geom_rect(aes(xmin =min(x1_bio_07), xmax =  min(x_bio_07), ymin = -Inf, ymax = Inf),fill="grey" ,alpha = .90) +
  annotate("rect",xmin = min(x1_bio_07), xmax =  min(x_bio_07), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_07), xmax =  max(x1_bio_07), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  #geom_rect(aes(xmin =max(x_bio_07), xmax =  max(x1_bio_07), ymin = -Inf, ymax = Inf),fill="grey" , alpha = .10)+
  
  #scale_fill_manual(values = c("a" = "grey70"))+
  #annotate("rect", min(x1_bio_07), xmax =  min(x_bio_07), ymin = -Inf, ymax = Inf, alpha=0.2, fill="grey") +
  #,"Democratic" = "blue3"))
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color =  "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color =  "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_07,y = rowMeans(GLOBAL_ResponceCurve_bio_07[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000)) 

ggsave("GF_Global_Bio_07.png", width = 10, height = 4)

### BIO_10 ####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_bio_10) +
  #geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,-94])-apply(LOCAL_ResponceCurve_bio_10[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,-94])+apply(LOCAL_ResponceCurve_bio_10[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_10,y = rowMeans(LOCAL_ResponceCurve_bio_10[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Local_Bio_10.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_10) +
  #geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,-94])-apply(GLOBAL_ResponceCurve_bio_10[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,-94])+apply(GLOBAL_ResponceCurve_bio_10[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_bio_10), xmax =  min(x_bio_10), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_10), xmax =  max(x1_bio_10), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_10,y = rowMeans(GLOBAL_ResponceCurve_bio_10[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Global_Bio_10.png", width = 10, height = 4)

### BIO_16#####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_bio_16) +
  #geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,-94])-apply(LOCAL_ResponceCurve_bio_16[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,-94])+apply(LOCAL_ResponceCurve_bio_16[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_16,y = rowMeans(LOCAL_ResponceCurve_bio_16[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Local_bio_16.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_16) +
  #geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,-94])-apply(GLOBAL_ResponceCurve_bio_16[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,-94])+apply(GLOBAL_ResponceCurve_bio_16[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_bio_16), xmax =  min(x_bio_16), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_16), xmax =  max(x1_bio_16), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_16,y = rowMeans(GLOBAL_ResponceCurve_bio_16[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Global_bio_16.png", width = 10, height = 4)

#### BIO_17####

## local

sp<-ggplot(data=LOCAL_ResponceCurve_bio_17) +
  #geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,-94])-apply(LOCAL_ResponceCurve_bio_17[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,-94])+apply(LOCAL_ResponceCurve_bio_17[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x_bio_17,y = rowMeans(LOCAL_ResponceCurve_bio_17[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Local_bio_17.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_bio_17) +
  #geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,-94])-apply(GLOBAL_ResponceCurve_bio_17[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,-94])+apply(GLOBAL_ResponceCurve_bio_17[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_bio_17), xmax =  min(x_bio_17), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_bio_17), xmax =  max(x1_bio_17), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_bio_17,y = rowMeans(GLOBAL_ResponceCurve_bio_17[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Global_bio_17.png", width = 10, height = 4)

####BULK#####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_BLDFIE_M) +
  #geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,-94])-apply(LOCAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,-94])+apply(LOCAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  # geom_line(aes( x=x_BLDFIE_M,y = rowMeans(LOCAL_ResponceCurve_BLDFIE_M[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Local_BLDFIE_M.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_BLDFIE_M) +
  #geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,-94])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,-94])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_BLDFIE_M), xmax =  min(x_BLDFIE_M), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_BLDFIE_M), xmax =  max(x1_BLDFIE_M), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_BLDFIE_M,y = rowMeans(GLOBAL_ResponceCurve_BLDFIE_M[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Global_BLDFIE_M.png", width = 10, height = 4)
####TWI####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_TWI) +
  #geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,-94])-apply(LOCAL_ResponceCurve_TWI[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,-94])+apply(LOCAL_ResponceCurve_TWI[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x_TWI,y = rowMeans(LOCAL_ResponceCurve_TWI[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Local_TWI.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_TWI) +
  #geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,-94])-apply(GLOBAL_ResponceCurve_TWI[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,-94])+apply(GLOBAL_ResponceCurve_TWI[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  annotate("rect",xmin = min(x1_TWI), xmax =  min(x_TWI), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  annotate("rect",xmin =max(x_TWI), xmax =  max(x1_TWI), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_TWI,y = rowMeans(GLOBAL_ResponceCurve_TWI[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Global_TWI.png", width = 10, height = 4)
####POP.DENS####
## local

sp<-ggplot(data=LOCAL_ResponceCurve_pop.dens) +
  #geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,-94])-apply(LOCAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,-94])+apply(LOCAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x_pop_dens,y = rowMeans(LOCAL_ResponceCurve_pop.dens[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Local_pop.dens.png", width = 10, height = 4)

### global 
sp<-ggplot(data=GLOBAL_ResponceCurve_pop.dens) +
  #geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,-94])-apply(GLOBAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "black"   ,linetype = "longdash",size=1 , alpha=0.5  )  +
  #geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,-94])+apply(GLOBAL_ResponceCurve_pop.dens[,-94],1,std) ), color = "black"   ,linetype = "longdash" ,size=1 , alpha=0.5  )  +
  # annotate("rect",xmin = min(x1_pop_dens), xmax =  min(x_pop_dens), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  #  annotate("rect",xmin =max(x_pop_dens), xmax =  max(x1_pop_dens), ymin = -Inf, ymax = Inf, alpha=0.7, fill="grey")  + 
  ## the two have almost the same range of predictions 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))],1,std) ), color = "olivedrab"  ,linetype = "longdash" ,size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))],1,std) ), color = "chartreuse3",linetype = "longdash",size=1, alpha=0.5)  + 
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])+apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))])-apply(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))],1,std) ),size=1, color = "forestgreen" ,linetype = "longdash", alpha=0.5)  +
  
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="tree",]))]) ), color = "forestgreen" ,    size=2.3)  +
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="herb",]))]) ), color = "chartreuse3",   size=2.3)  + 
  geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,as.numeric(rownames(RangeChange_Traits[RangeChange_Traits$GF=="shrub",]))]) ), color = "olivedrab" ,         size=2.3 )  + 
  #geom_line(aes( x=x1_pop_dens,y = rowMeans(GLOBAL_ResponceCurve_pop.dens[,-94]) ), color = "black",   size=2.3   )  +
  
  
  xlab("Env_var") + 
  ylab("Raw_prob")
sp+theme_gray(base_size = 20)+coord_cartesian(ylim=c(0,1000))
ggsave("GF_Global_pop.dens.png", width = 10, height = 4)






###### MEAN VARIABLE IMPORTANCE FOR MODELS GLM & GAM toghether######
global_var.imp<- matrix(data=rep(0,7), nrow = 1, ncol=7)
local_var.imp<- matrix(data=rep(0,7), nrow = 1, ncol=7)
### i did this for the same species in both the local and the global model
# 89 ,92 ,47 ,12, 59 ,63 ,42 ,36 ,62,  1 ,83, 60 ,52 ,38 ,64 ,13 ,17 ,53 ,79  ,9 ,19 ,29, 61, 18 ,91, 51 ,22, 24 ,71 ,78
for(sp in sp.names[c(-11,-20,-27,-30,-49,-55,-62,-69)]){ ## i will only use a value of the index to retreive all data
  #sp <- sp.names[13]
  print(paste(sp, "...  mean variable importance evaluations",sep="  ."))
  ### LOCAL
  load(paste(sp,"_INVADEDRANGE.Rdata"))
  ## clenaing the environment to clean ram
  rm(list=ls(pattern="EF"))
  rm(list=ls(pattern=sp))
  rm(list=ls(pattern="GAM"))
  rm(list=ls(pattern="GLM"))
  
  MyModels_var_importLOCAL <- get_variables_importance(myBiomodModelOutLOCAL)
  
  # make the mean of variable importance by algorithm
  MyModels_var_importLOCAL <- apply(MyModels_var_importLOCAL, c(1,2), median) 
  MyModels_var_importLOCAL <- apply(MyModels_var_importLOCAL, 2, function(x) x*(1/sum(x))) # standardize the data
  MyModels_var_importLOCAL <- apply(MyModels_var_importLOCAL,1, mean)
  MyModels_var_importLOCAL <- rbind(MyModels_var_importLOCAL)
  rownames(MyModels_var_importLOCAL)<- myRespName 
  local_var.imp <- rbind(local_var.imp, MyModels_var_importLOCAL)
  #rm(list = ls(all.names = TRUE))
  
  load(paste(sp, "_NATIVE.Rdata"))
  rm(list=ls(pattern="EF"))
  rm(list=ls(pattern=sp))
  rm(list=ls(pattern="GAM"))
  rm(list=ls(pattern="GLM"))
  
  
  MyModels_var_importGLOB <- get_variables_importance(myBiomodModelOutGLOB)
  
  # make the mean of variable importance by algorithm
  MyModels_var_importGLOB <- apply(MyModels_var_importGLOB, c(1,2), median) 
  MyModels_var_importGLOB <- apply(MyModels_var_importGLOB, 2, function(x) x*(1/sum(x))) # standardize the data
  MyModels_var_importGLOB <- apply(MyModels_var_importGLOB,1, mean)
  MyModels_var_importGLOB <- rbind(MyModels_var_importGLOB)
  rownames(MyModels_var_importGLOB)<- myRespName 
  global_var.imp <- rbind(global_var.imp, MyModels_var_importGLOB)
  
  rm(list=ls(pattern="my"))
  #rm(list=ls(pattern="loc"))
  #rm(list=ls(pattern="glob"))
  
  
  unlink(paste(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
  #gc(reset=T)
  gc(reset=T)
  
}
global_var.imp  <- as.data.frame(global_var.imp)[-1,]
local_var.imp <- as.data.frame(local_var.imp)[-1,]

## putting names in columns 
local_var.imp <- cbind(local_var.imp,rownames(local_var.imp))
global_var.imp <- cbind(global_var.imp,rownames(global_var.imp))
## renaming columns 
colnames(local_var.imp) <- c("loc_bio_07",         "loc_bio_10",         "loc_bio_16"     ,   
                             "loc_bio_17",         "loc_BLDFIE_M"       ,         "loc_Pop.dens.",
                             "loc_TWI"            ,         "species")
colnames(global_var.imp) <- c("glob_bio_07",         "glob_bio_10",         "glob_bio_16"     ,   
                              "glob_bio_17",         "glob_BLDFIE_M"       ,         "glob_Pop.dens.",
                              "glob_TWI"            ,         "species")


#save(global_var.imp,file="global_var.imp.rda")
#save(local_var.imp,file="local_var.imp.rda")

### A rapid elaboration of these values 
load("global_var.imp.rda")
load("local_var.imp.rda")
global_var.imp <- global_var.imp[-1,1:8]
local_var.imp <- local_var.imp[-1,1:8]
global_var.imp[,1:7]<-apply(global_var.imp[,1:7],2,as.numeric)
local_var.imp [,1:7]<-apply(local_var.imp [,1:7],2,as.numeric)
#
#boxplot(as.numeric(global_var.imp[,1]),
#          as.numeric(local_var.imp [,1]),
#          as.numeric(global_var.imp[,2]),
#          as.numeric(local_var.imp [,2]),
#          as.numeric(global_var.imp[,3]),
#          as.numeric(local_var.imp [,3]),
#          as.numeric(global_var.imp[,4]),
#          as.numeric(local_var.imp [,4]),
#          as.numeric(global_var.imp[,5]),
#          as.numeric(local_var.imp [,5]),
#          as.numeric(global_var.imp[,6]),
#          as.numeric(local_var.imp [,6]),
#          as.numeric(global_var.imp[,7]),
#          as.numeric(local_var.imp [,7]),col=rep( c("#1a71ab","#dd5d9f"),7))
### preparing for anova
global_var.imp <- cbind(global_var.imp,rep("glob",93))
colnames(global_var.imp)[9] <- "model"

local_var.imp <- cbind(local_var.imp,rep("loc",93))
colnames(local_var.imp)[9] <- "model"
#
#s <- aov(c(global_var.imp[,1],
#      local_var.imp [,1],
#      global_var.imp[,2],
#      local_var.imp [,2],
#      global_var.imp[,3],
#      local_var.imp [,3],
#      global_var.imp[,4],
#      local_var.imp [,4],
#      global_var.imp[,5],
#      local_var.imp [,5],
#      global_var.imp[,6],
#      local_var.imp [,6],
#      global_var.imp[,7],
#      local_var.imp [,7]))
#
#
#
#
## variable importance for biomes of origin
load("RangeChange_Traits_new.Rda")
RangeChange_Traits <- RangeChange_Traits_new
#### adding a column for main biome aggregated
#RangeChange_Traits$Main.Biome_AGGR <- rep("temp",93) ## 28 for my test
#RangeChange_Traits[grep("Temperate",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"]### these are temperate yet
### xeric
#RangeChange_Traits[grep("Deserts",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
#RangeChange_Traits[grep("Mediterranean",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
### tropical
#RangeChange_Traits[grep("Tropical",as.character(RangeChange_Traits$Main.Biome), value=F),"Main.Biome_AGGR"] <- "trop"
#
#
#setdiff(RangeChange_Traits$SpName, local_var.imp[,8])
#setdiff( local_var.imp[,8],RangeChange_Traits$SpName)
#
RangeChange_Traits_new_var.imp <- merge(RangeChange_Traits, local_var.imp,by.x="SpName", by.y="species")
RangeChange_Traits_new_var.imp <- merge(RangeChange_Traits_new_var.imp, global_var.imp,by.x="SpName", by.y="species")
#
#### are there any differences in var importances for BIOMES 
### local model var.imp
#apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="med",colnames(RangeChange_Traits_new_var.imp[, 76:81])],2, mean)
#apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="temp",colnames(RangeChange_Traits_new_var.imp[,76:81])],2, mean)
#apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="trop",colnames(RangeChange_Traits_new_var.imp[,76:81])],2, mean)
### boxplot
RangeChange_Traits_new_var.imp[,74:80]<-apply(RangeChange_Traits_new_var.imp[,74:80],2,as.numeric)

#par(mfrow=c(3,1))
#boxplot((RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="med",colnames(RangeChange_Traits_new_var.imp[, 76:81])]), main="MED sp Local var.imp", col="firebrick3")
#boxplot((RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="temp",colnames(RangeChange_Traits_new_var.imp[,76:81])]), main="TEMP sp Local var.imp", col="deepskyblue2")
#boxplot((RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="trop",colnames(RangeChange_Traits_new_var.imp[,76:81])]), main="Trop sp Local var.imp", col="chartreuse4")
#
##### The same on the Global model
### global model var.imp
#apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="med",colnames(RangeChange_Traits_new_var.imp[, 70:76])],2, mean)
#apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="temp",colnames(RangeChange_Traits_new_var.imp[,70:76])],2, mean)
#apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="trop",colnames(RangeChange_Traits_new_var.imp[,70:76])],2, mean)
### boxplot
RangeChange_Traits_new_var.imp[,82:88]<-apply(RangeChange_Traits_new_var.imp[,82:88],2,as.numeric)

#par(mfrow=c(3,1))
#boxplot(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="med",colnames(RangeChange_Traits_new_var.imp[, 82:88])], main="MED sp Local var.imp", col="firebrick3")
#boxplot(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="temp",colnames(RangeChange_Traits_new_var.imp[,82:88])], main="TEMP sp Local var.imp", col="deepskyblue2")
#boxplot(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="trop",colnames(RangeChange_Traits_new_var.imp[,82:88])], main="Trop sp Local var.imp", col="chartreuse4")
#
#### final graph for this thing
#loc_med  <-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="med",colnames(RangeChange_Traits_new_var.imp[,75:81])], 2, mean))
#glob_med <-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="med",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
# loc_temp<-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="temp",colnames(RangeChange_Traits_new_var.imp[,75:81])], 2, mean))
#glob_temp<-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="temp",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
# loc_trop<-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="trop",colnames(RangeChange_Traits_new_var.imp[,75:81])], 2, mean))
#glob_trop<-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="trop",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
#
#
#
#prov<-as.data.frame(rbind(loc_med,glob_med))
### first try
#rownames(prov)<- c("loc", "glob")
#barplot(as.matrix(prov[,]), beside=T, col=c("#dd5d9f","#1a71ab"))

## def affiancando i biomi
## LOCAL var.imp
#DEF_local<-rbind(loc_med  ,
#           #glob_med ,
#           loc_temp,
#           #glob_temp,
#           loc_trop)
#           #glob_trop)
#
#
#           par(mfrow=c(3,1))
#           
#           
#barplot(as.matrix(DEF_local[,]), beside=T, col=c("firebrick3","deepskyblue2","chartreuse4"), ylim=c(0,0.4))
#### DEF GLOBAL
#DEF_glob<-rbind(#loc_med  ,
#                 glob_med ,
#                 #loc_temp,
#                 glob_temp,
#                 #loc_trop)
#glob_trop)
#
#barplot(as.matrix(DEF_glob[,]), beside=T, col=c("firebrick3","deepskyblue2","chartreuse4"), ylim=c(0,0.4))
### COMPARING ONLY THE TWO NICHES
loc<-(apply(RangeChange_Traits_new_var.imp[,colnames(RangeChange_Traits_new_var.imp[,74:80])], 2, mean))
glob<-(apply(RangeChange_Traits_new_var.imp[,colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
DEF_both<-rbind(loc,glob)

png(filename=file.path( paste("Var.imp.png", sep="_")), width = 1200, height = 450, pointsize = 12, bg = "white", res = NA)

barplot(as.matrix(DEF_both[,]), beside=T, col=c("darkolivegreen3"
                                                ,"lightgoldenrod3"), ylim=c(0,0.3))
dev.off()


## PLOTTING WITH GGPLOT

#DEF_both1<-as.data.frame(cbind(c(DEF_both[1,],DEF_both[2,]), c(rep("loc",7),rep("glob",7) ),c(names(loc),names(glob)) ))
#colnames(DEF_both1)<-c("var.imp","model","var")
#ggplot(data=DEF_both1[7:14,], aes(x=dose, y=var.imp, fill=supp)) +
#  geom_bar(stat="identity")


### t. test to only compare pop.dens results
shapiro.test(RangeChange_Traits_new_var.imp$loc_Pop.dens.) ## non normali
shapiro.test(RangeChange_Traits_new_var.imp$glob_Pop.dens.) ## non normali

var.test(RangeChange_Traits_new_var.imp$loc_Pop.dens.,RangeChange_Traits_new_var.imp$glob_Pop.dens.)


wilcox.test(RangeChange_Traits_new_var.imp$loc_Pop.dens.,RangeChange_Traits_new_var.imp$glob_Pop.dens.)


## var.imp but for the main biome classes 
## Global and Local 
RangeChange_Traits_new_var.imp[,colnames(RangeChange_Traits_new_var.imp[,82:88])]
## AGGREGATING BIOMES

RangeChange_Traits_new_var.imp$Main.Biome_AGGR <- rep("temp",93) ## 28 for my test
RangeChange_Traits_new_var.imp[grep("Temperate",as.character(RangeChange_Traits_new_var.imp$Main.Biome), value=F),"Main.Biome_AGGR"]### these are temperate yet
## xeric
RangeChange_Traits_new_var.imp[grep("Deserts",as.character(RangeChange_Traits_new_var.imp$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
RangeChange_Traits_new_var.imp[grep("Mediterranean",as.character(RangeChange_Traits_new_var.imp$Main.Biome), value=F),"Main.Biome_AGGR"] <- "med"
## tropical
RangeChange_Traits_new_var.imp[grep("Tropical",as.character(RangeChange_Traits_new_var.imp$Main.Biome), value=F),"Main.Biome_AGGR"] <- "trop"
### GLOBAL MODEL
### PLOTTING VARIABLE IMPORTANCE FROM BIOMES
temp<-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="temp",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
trop<-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="trop",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
med <-(apply(RangeChange_Traits_new_var.imp[ RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="med",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
### doing the final dataset 
png(filename=file.path( paste("GLOBAL_Var.imp_x_biome.png", sep="_")), width = 1200, height = 450, pointsize = 12, bg = "white", res = NA)

barplot(rbind(med,temp,trop ),background="grey", beside=T, col=c("darkred","lightseagreen","forestgreen"), ylim=c(0,0.4), main="Global model, var.imp x biome")

dev.off()

#ggplotting it 
## PLOTTING WITH GGPLOT

DEF_both1<-as.data.frame(cbind(c(med,temp,trop ), c(rep("a_med",7),rep("b_temp",7),rep("c_trop",7) ),names(c(med,temp,trop )) ))
colnames(DEF_both1)<-c("var.imp","biome","var")
#DEF_both1$var<-as.factor(DEF_both1$var)
DEF_both1$biome<-as.factor(DEF_both1$biome)

# isolate the subset of data you want to order by
subset_to_order = DEF_both1[DEF_both1$biome=="a_med",]
subset_to_order = DEF_both1[DEF_both1$biome=="b_temp",]

subset_to_order$biome = with(subset_to_order, reorder(DEF_both1$biome, DEF_both1$var))

# use reorder to reorder the factor
subset_to_order = with(DEF_both1, reorder(DEF_both1$var.imp, DEF_both1$biome))


p<-ggplot(data=DEF_both1, aes(x=var, y=var.imp, fill=biome)) +
  geom_bar(stat="identity", position=position_dodge())

p + scale_fill_manual(values=c("darkred","lightseagreen","forestgreen"))


## checking the values are 1 
rowSums(rbind(med,temp,trop ))

### LOCAL MODEL
### PLOTTING VARIABLE IMPORTANCE FROM BIOMES
temp<-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="temp",colnames(RangeChange_Traits_new_var.imp[,74:80])], 2, mean))
trop<-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="trop",colnames(RangeChange_Traits_new_var.imp[,74:80])], 2, mean))
med <-(apply(RangeChange_Traits_new_var.imp[ RangeChange_Traits_new_var.imp$Main.Biome_AGGR=="med",colnames(RangeChange_Traits_new_var.imp[,74:80])], 2, mean))
### doing the final dataset 
png(filename=file.path( paste("LOCAL_Var.imp_x_biome.png", sep="_")), width = 1200, height = 450, pointsize = 12, bg = "white", res = NA)

barplot(rbind(med,temp,trop ), beside=T, col=c("darkred","lightseagreen","forestgreen"), ylim=c(0,0.4), main="Local model, var.imp x biome")
dev.off()

## checking the values are 1 
rowSums(rbind(med,temp,trop ))


### Growth forms mean variable importance. 

### PLOTTING VARIABLE IMPORTANCE FROM BIOMES
herb <-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$GF=="herb",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
shrub<-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$GF=="shrub",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
tree <-(apply(RangeChange_Traits_new_var.imp[ RangeChange_Traits_new_var.imp$GF=="tree",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
### doing the final dataset 
png(filename=file.path( paste("GLOBAL_Var.imp_x_GrowthForm.png", sep="_")), width = 1200, height = 450, pointsize = 12, bg = "white", res = NA)

barplot(rbind(herb,shrub,tree ),background="grey", beside=T, col=c(
  "chartreuse3",
  "olivedrab","forestgreen" ), ylim=c(0,0.4), main="Global model, var.imp x biome")

dev.off()
### alla fine non l'ho fatto con GGPLOT

### Growth forms mean variable importance. 

### PLOTTING VARIABLE IMPORTANCE FROM BIOMES
Amaranthaceae <-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Family_Agg=="Amaranthaceae",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
Compositae    <-(apply(RangeChange_Traits_new_var.imp[RangeChange_Traits_new_var.imp$Family_Agg=="Compositae",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
Poaceae       <-(apply(RangeChange_Traits_new_var.imp[ RangeChange_Traits_new_var.imp$Family_Agg=="Poaceae",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))
other         <-(apply(RangeChange_Traits_new_var.imp[ RangeChange_Traits_new_var.imp$Family_Agg=="other",colnames(RangeChange_Traits_new_var.imp[,82:88])], 2, mean))

### doing the final dataset 
png(filename=file.path( paste("GLOBAL_Var.imp_x_Fam.png", sep="_")), width = 1200, height = 450, pointsize = 12, bg = "white", res = NA)

barplot(rbind(Amaranthaceae,
              Compositae,   
              Poaceae  ,    
              other         ),background="grey", beside=T, col=c("#440154","grey","#35b779","#90d743" ), ylim=c(0,0.4), main="Global model, var.imp x biome")

dev.off()


## checking the values are 1 
rowSums(rbind(med,temp,trop ))




