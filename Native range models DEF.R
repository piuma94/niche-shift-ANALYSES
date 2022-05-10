


## setwd("D:\\Luigi\\Laure_suggestions\\MODELLI COMPLETATI")  ## hard disk 12 TB if i need to add something
###setwd("/Volumes/WD Elements/Luigi/Laure_suggestions")  ## MAC, WD

#setwd("/Volumes/Extreme SSD/Secondo Articolo/R Projects/Laure_suggestions")
#setwd("D:\\Secondo Articolo\\R Projects\\Dummy_models_Leungstyle")  ## fisso casa, extreme SSD
#setwd("E:\\Secondo Articolo\\R Projects\\Laure_suggestions") ## fisso casa

#setwd("D:\\Luigi\\Laure_suggestions")  ## hard disk 12 TB from fisso home
#setwd("/Volumes/Extreme SSD/Secondo Articolo/R Projects/Laure_suggestions/Dataset e modelli DEF") ### mac Luigi extreme SSD
#setwd("/Volumes/UNTITLED/Secondo Articolo/R Projects/Laure_suggestions/Dataset e modelli DEF")
#setwd("G:\\New test with laure")


### fisso UNI
setwd("E:\\New test with laure")
#setwd("C:\\Users\\luigi\\OneDrive - Universita degli Studi Roma Tre\\New test with laure")

## Setting the environment ###
## for windows
#memory.limit(size=76000)
## uploading some libraries
library(dplyr)

library(biomod2)
library(raster)
library(dismo)
library(ecospat)
library(sf)

#rm(list = ls(all.names = TRUE))

load("ly.names.def")

### MED_ENV WHERE I WANT TO PROJECT
#myexpl.var30_ST_DEF <- stack("myexpl.var30_ST_DEF.tif")  ## here i am saving the all dataset cropped
#myexpl.var30_ST_DEF[[6]]<-log(myexpl.var30_ST_DEF[[6]]+1)
#names(myexpl.var30_ST_DEF)<- ly.names.def

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




#### Defining only european dataset

### EVA 30 and 70
#### EVA 30 and 70
load("EVAE.70.Rda")
#load("EVAT.30.Rda")
### first assembling a complete single dataset
### the species i need to evaluate
sp.names <- colnames(EVAE.70[,5:97])  ## check these ## 
#
rm(EVAE.70)
### first assembling a complete single dataset
#EVA <- rbind(EVAE.70[,-106],EVAT.30[,-106])

### GBIF_ grilled with ENV.vals
load(file="GBIF_grilled.P2.Rda") ## this is the best, i will not have duplicates when reimporting
#load("GBIF_grilled_env_EUROPE_70.Rda"        )
### eliminating the strange presence 

## newly evaluated BGK points
load("GLOBAL_BKG_NICHE_AN.Rda")
#load("LOCAL_BKG.Rda")

#Local_BKG<-EVA_BKG_Weigth
Global_BKG<-GBIF_BKG_Weigth 
rm(GBIF_BKG_Weigth)
## checking them
#apply(Local_BKG,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
apply(Global_BKG,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
## eliminating NA's
#Local_BKG<-na.omit(Local_BKG)
Global_BKG<-na.omit(Global_BKG)

## duplicating rows considering sampling intensity





## UPLOADING THE EUROPEAN DATASET: 70 AND 30 %
#load("GBIF_grilled_env_EUROPE_70.Rda"        )
#load("GBIF_grilled_env_EUROPE_EVALUATION.Rda")
#### CLEANING THE GLOBAL_GBIF FROM THE EUROPEAN EVALUATIONS
#GBIF_grilled<-GBIF_grilled[!GBIF_grilled$cells %in% intersect(GBIF_grilled_env_EUROPE_EVALUATION$cells, GBIF_grilled$cells)             ,]
## I RENAMED IT IN THE SAME WAY, TO HAVE THE DATASET CLEAN ### ok there are the exact number of cells i expect 30% 
### I eliminated this really bad point that is outside all values. an outlier
#hist(GBIF_grilled$CHELSA_bio10_16)
GBIF_grilled <- GBIF_grilled[GBIF_grilled$CHELSA_bio10_16<=5000,] ## ok, this was only one plot only in the global buffer
#summary(GBIF_grilled[,ly.names.def])
 #GBIF_grilled[GBIF_grilled$BLDFIE_M<=600,] ## ok, this was only one plot only in the global buffer

### preparing the loop
#sp.names <- colnames(EVAT.30[,5:97])

## how many run evaluations i want?
n.run <- 3  ## Lascialo comunque ad un valore alto sennò non si riesce ad andare avanti nelle analisi

## REDUCING SAMPLING TO ONLY 1 MILLION POINTS
#Global_BKG<-Global_BKG[1:1000000,] ## subsetting to one million BKG
## re-doing it to give less value to BKG
 #Global_BKG[Global_BKG$EVA_Nr.plots>=300, "EVA_Nr.plots"] <- 300
# Global_BKG[Global_BKG$GBIF_Nr.pres>=300, "GBIF_Nr.pres"] <- 300
 ## rescaling from 0:100
# Global_BKG$EVA_Nr.plots <- round(Global_BKG$EVA_Nr.plots/3,0)+1
 #Global_BKG$GBIF_Nr.pres <- round(Global_BKG$GBIF_Nr.pres/3,0)+1
 
## I want not oto give the model too much absences, to not pull the curve down. So i am giving a subset of my BKG points but extracted considerign their weight considering the 
#PROB <- (Global_BKG$EVA_Nr.plots+Global_BKG$GBIF_Nr.pres)/max(Global_BKG$EVA_Nr.plots+Global_BKG$GBIF_Nr.pres)
#Global_BKG <- Global_BKG[sample(row.names(Global_BKG), size=100000, prob=PROB),]

###### uploading Diletta's data #####
#  i will need a matrix of all my species (columns) and the regions (rows)
Dataset<-read.csv("dataset 0103.csv", sep=";", header = T)
#Dataset[ Dataset[,my.resp.name]==1,"POWO.REGIONS" ]


### POWO RWGIONS
## fisso
POWO_regions<- shapefile("TDWG level-3 regions\\level3")


### talk with Laure: 
## we decided: if a plot has been sampled more than 300 times we gave it a value of 300. 
## for the others we used the sampling value. then we divided everything by 3, to rescale it among 1-100
## i can not divide by 3 because 1/3 is less than one ## doing that for both EVA and GBIF
# Global_BKG[Global_BKG$EVA_Nr.plots>=300, "EVA_Nr.plots"] <- 300
# Global_BKG[Global_BKG$GBIF_Nr.pres>=300, "GBIF_Nr.pres"] <- 300

# repeating it considering sampling intensity
#  Global_BKG <- as.data.frame(lapply(Global_BKG, rep, (Global_BKG$EVA_Nr.plots+Global_BKG$GBIF_Nr.pres ) )) ### is like i am extracting from a box in which i have more plots for more sampled areas



####################### OK ORA PARTO CON I MODELLI, SPERANDO BENE
# LA RIDUZIONE AMBIENTALE ALLA FINE NON ? ELEVATISSIMA, MA FORSE BASTA POCO.
## ALTRIMENTI IL GROSSO ? DATO DA RISOLUZIONE!, VARIABILI! O DATASET?
#unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#gc(reset=T)
## first set c(89 ,92 ,47 ,12, 59 ,63 ,42 ,36 ,62,  1 ,83, 60 ,52 ,38 ,64 ,13 ,17 ,53 ,79  ,9 ,19 ,29, 61, 18 ,91, 51 ,22, 24 ,71 ,78)
## second set c(4,20,45,75,81)
## 47 non fatto per il global
for( species in sp.names[45:93]) {
  myRespName <-  species
  #myRespName <-  "Acacia.dealbata"
  #myRespName <-  "Symphyotrichum.squamatum"
  
  #### subsetting presences in only native range 
  cat("Start_" ,myRespName,'_elaboration')
  native_range <- Dataset[ Dataset[,myRespName]==1,"POWO.REGIONS" ]
   ## native environment
  native_env <- (mask(crop(global,POWO_regions[POWO_regions@data[["LEVEL3_NAM"]] %in% native_range ,]),POWO_regions[POWO_regions@data[["LEVEL3_NAM"]] %in% native_range ,])) ## rownames(TDWG[TDWG[,myRespName]==1,]) ## lo potrei pure aggiungere qui per avere una sola riga  di codice.
 # plot(native_env[[1]])
  
  
  ### combining presences
  all.pres<-rbind(#EVA[!is.na(EVA[myRespName]) & EVA[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)],
                  GBIF_grilled[!is.na(GBIF_grilled[myRespName]) & GBIF_grilled[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)]
                  #EVAT.30[!is.na(EVAT.30[myRespName]) & EVAT.30[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)]
                  #GBIF_grilled_env_EUROPE_EVALUATION[!is.na(GBIF_grilled_env_EUROPE_EVALUATION[myRespName]) & GBIF_grilled_env_EUROPE_EVALUATION[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)]
  )
  #sum(duplicated(all.pres$cells))  ## 57 presences are repeated, because EVAmatch GBIF
  
  #apply(all.pres, 2, function (x){sum(is.na(x))} )  ## 20k PA only in the myRespname colmn
  all.pres$Population.density.2000<-log(all.pres$Population.density.2000+1)
  
  
  ### SUBSETTIN GPRESENCES IN THE NATIVE RANGE 
  #all.pres <- all.pres[all.pres[,myRespName]==1,c("Longitude","Latitude",ly.names.def, myRespName) ]
  all.pres<- cbind(all.pres[,],raster::extract(native_env[[1]], all.pres[,c("Longitude","Latitude")]))
  #length(all.pres$`raster::extract(native_env[[1]], all.pres[, c("Longitude", `[!is.na(all.pres$`raster::extract(native_env[[1]], all.pres[, c("Longitude", `)]) ### 16744 values are not NA
  ## eliminating presences outside of the native range
  #### HOW MANY PRESENCES IN THE NATIVE RANGE??  
  ## little ceck old ceck
  #plot(native_env[[1]])
  #points(native_pres[!is.na(native_pres$`raster::extract(native_env[[1]], native_pres[, c("Longitude", `),c("Longitude","Latitude")], cex=0.1, pch=22)
  cat(myRespName,'Nr. presences in the native range=')
  print(sum(!is.na(all.pres$`raster::extract(native_env[[1]], all.pres[, c("Longitude", `)))
  cat("\n\n")
  
  ## eliminating presences outside of the native range
  all.pres<- all.pres[!is.na(all.pres$`raster::extract(native_env[[1]], all.pres[, c("Longitude", `),]
  
  ## I NEED TO WEIGTH PA FOR EACH MODEL ISTEAD ALL PRESENCES SHOULD HAVESAME WEIGHT AND PREVALENCE 1
  ##  cells is the unified cellID, 
  ### global weighted PA NOT 
  ## eliminating PA that falls over presences of my species
  # as i will take the first 10k i am not having troubles, i always use ALMOST the same cells for each species
  #GBIF_BKG_Weigth<-GBIF_BKG_Weigth[,2:14]
  
  ## combining and selecting 10k global absences
  # loc.abs<- EVA_BKG_Weigth_env[ !EVA_BKG_Weigth_env$cells %in% intersect(EVA_BKG_Weigth_env[,"cells"], c(all.pres[ ,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1, "cells" ],EVAT.30[EVAT.30[,myRespName]==1, "cells" ] )   ) ,] ## this is a correction, based on the all presences, not only few of them
  
#  glob.abs <- GBIF_BKG_Weigth[ !GBIF_BKG_Weigth$cells %in% intersect(GBIF_BKG_Weigth[,"cells"], c(all.pres[ ,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1, "cells" ],EVAT.30[EVAT.30[,myRespName]==1, "cells" ] )    ),]
  ## IS THE CORRECT CELL?? it's CELLS ok!! 265 and more intersections for A.dealbata  
  
  #### TAKING ONLY NATIVE WEIGHTED BKG. 
  glob.abs <- Global_BKG[ !Global_BKG$cells %in% intersect(Global_BKG[,"cells"], c(all.pres[ ,"cells"] )    ),]
  ### ARE THESE MATCHING?? yes
  glob.abs<- cbind(glob.abs[,],raster::extract(native_env[[1]], glob.abs[,c("Longitude","Latitude")]))
  #length(glob.abs$`raster::extract(native_env[[1]], glob.abs[, c("Longitude", "Latitude")])`[!is.na(glob.abs$`raster::extract(native_env[[1]], glob.abs[, c("Longitude", "Latitude")])`)]) ### 16744 values are not NA
  glob.abs<- glob.abs[!is.na(glob.abs$`raster::extract(native_env[[1]], glob.abs[, c("Longitude", "Latitude")])`),]
  #`raster::extract(native_env[[1]], glob.abs[, c("Longitude", "Latitude")])`
  ## PROBABILITIES FOR MY STUDY AREA 
  #glob.abs$`raster::extract(native_env[[1]], glob.abs[, c("Longitude", "Latitude")])`
### SPECIFIC RESAMPLING DIFFERENT IF I HAVE LESS THAN 400000 OF MY bkg 
  if(dim(glob.abs)[1]>=400000) {
    glob.abs <-glob.abs[0:400000,]  ### I am taking only one 400 k perch� mi aspetto che anche nel native non ne avr� pi� di cos�
    PROB <- (glob.abs$EVA_Nr.plots+glob.abs$GBIF_Nr.pres)/max(glob.abs$EVA_Nr.plots+glob.abs$GBIF_Nr.pres)
    glob.abs <- glob.abs[sample(row.names(glob.abs), size=100000, prob=PROB),]
    
  } else{ if( dim(glob.abs)[1]>=100000 ){ ### PRENDO DA QUELLI CHE HO: SPERIAMO NON SIANO MAI MENO DI 100 K 
    PROB <- (glob.abs$EVA_Nr.plots+glob.abs$GBIF_Nr.pres)/max(glob.abs$EVA_Nr.plots+glob.abs$GBIF_Nr.pres)
    glob.abs <- glob.abs[sample(row.names(glob.abs), size=100000, prob=PROB),]
    ## Se sono meno di  100 mila crasha tipo c. acinaciformis, per cui ho messo replace =T
  

  } else{ PROB <- (glob.abs$EVA_Nr.plots+glob.abs$GBIF_Nr.pres)/max(glob.abs$EVA_Nr.plots+glob.abs$GBIF_Nr.pres)
    glob.abs <- glob.abs[sample(row.names(glob.abs), size=100000, prob=PROB, replace=T),]
    ## Se sono meno di  100 mila crasha tipo c. acinaciformis, per cui ho messo replace =T
  
}}
  ### I SHOULD NOW WEIGHT ABSENCES; AD HOC FOR MY STUDY AREA. CONSIDERIGN A SPECIES AND AREA SPECIFIC WEIGHTNING

 # glob.abs <- glob.abs[sample(row.names(glob.abs), size=100000, prob=PROB),]
  
  
  # glob.abs<-glob.abs[1:10000,] keep all global absences
  # glob.abs$prop <-  glob.abs[,"Nr.plots"]/sum(glob.abs[,"Nr.plots"]) do not weight them
  ## check
  #sum(glob.abs$prop) ## it's 1 ok
  #sum(glob.abs$Nr.plots) ### 37648
  glob.abs$Population.density.2000<-log(glob.abs$Population.density.2000+1)
  #plot(native_env[[1]])
  #points(glob.abs[,c("Longitude","Latitude")], pch=20, cex=0.01)
  #points(all.pres[,c("Longitude","Latitude")], pch=20, cex=0.01, col="red")
  ### Local weighted PA
  #EVA_BKG_Weigth<-EVA_BKG_Weigth[,2:14]
  
  ## combining and selecting 10k local absences
  # loc.abs<- EVA_BKG_Weigth_env[ !EVA_BKG_Weigth_env$cells %in% intersect(EVA_BKG_Weigth_env[,"cells"], c(all.pres[ ,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1, "cells" ],EVAT.30[EVAT.30[,myRespName]==1, "cells" ] )   ) ,] ## this is a correction, based on the all presences, not only few of them
  
#  loc.abs<- EVA_BKG_Weigth[ !EVA_BKG_Weigth$cells %in% intersect(EVA_BKG_Weigth[,"cells"],  c(all.pres[ ,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1, "cells" ],EVAT.30[EVAT.30[,myRespName]==1, "cells" ] )  ),] ## this is a correction, based on the all presences, not only few of them
#  # loc.abs<-loc.abs[1:10000,] kep them all 
#  # loc.abs$prop <-  loc.abs[,"Nr.plots"]/sum(loc.abs[,"Nr.plots"]) do not weight them
#  ## check
#  #sum(loc.abs$prop) ## it's 1 ok
#  sum(loc.abs$Nr.plots) ## 42382
#  #hist(loc.abs$Population.density.2000)
#  #hist(log(loc.abs$Population.density.2000))
#  loc.abs$Population.density.2000<-log(loc.abs$Population.density.2000+1)
#  ## eliminating PA over presences may have a little effect
#  ## Eliminating absences over presences for EVALUATION is foundamental
#  ### can we give a single dataset of all BKG??
  
  
  
  
  ### eliminating EVAT.30 pa that fall over 
  #EVAT.30<-EVAT.30[,2:145]
  ## sistemata to the evaluation set
  # EVALUATION <- EVAT.30[ !EVAT.30$cells %in% intersect(EVAT.30[EVAT.30[,myRespName]==0,"cells"],all.pres[all.pres[,myRespName]==1  ,"cells"]) ,colnames(all.pres)[1:11]]  ## here i don't have the props col
  #apply(EVALUATION, 2, function (x){sum(is.na(x))} )  ## 20k PA only in the myRespname colmn
  ## elimionating absences on which fall  presences
  #  EVALUATION<-EVALUATION[ !EVALUATION$cells %in% intersect(EVALUATION[EVALUATION[,myRespName]==0,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1  ,"cells"]),] ## ELIMINATING ABSENCES THAT FALL OVER GBIF_EVALUATIONS PRESENCES
  
  ## adding the GBIF presences to the dataset
  #  EVALUATION<-rbind(EVALUATION,      GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1  ,colnames(all.pres)[1:11] ]) ## ADDING ONLY MYRESPNAME PRESENCES
  #  EVALUATION$Population.density.2000<-log(EVALUATION$Population.density.2000+1)
  
  ## Subsetting EVALUATION with cellsd ID for where i want to project
  
  #  EVALUATION_MED_ENV <- EVALUATION[EVALUATION$cells %in% intersect(Proj.cells$cells, EVALUATION$cells),] ## here i subsect only really cells in which i am projecting
  #  EVALUATION_MED_ENV$Population.density.2000<-log(EVALUATION_MED_ENV$Population.density.2000+1)
  
  
  ## now i also need to weight Presences: same prevalence and all presences same weigth
  #   sum(sum(loc.abs$prop),sum(glob.abs$prop)) ## total weight of absences is 2
  ## global=37648 local=42382
  ### WEIGHTING PRESENCES RESPECTIVELY
  #all.pres$prop<- rep(2/length(all.pres$cells),length(all.pres$cells))
  
  ## performing the analyses 
  ## FIRST ASSEMBLING THE FINAL DATASET FOR THESE PSECIES 
  #colnames(all.pres)[1:11]
  # PA for loc.abs  glob.abs
 #loc.abs[,myRespName]<- rep(NA,dim(loc.abs)[1])
  glob.abs[,myRespName]<- rep(NA,dim(glob.abs)[1])
  #sum(loc.abs$Nr.plots) ## 42382
  #sum(glob.abs$EVA_Nr.plots) ## 37648
  #(42382+37648)/12689
  
  #all.pres$WGH<-rep((sum(loc.abs$Nr.plots)+sum(glob.abs$Nr.plots))/dim( all.pres)[1],dim( all.pres)[1])
  #sum(glob.abs$Nr.plots ) 
  #sum(loc.abs$Nr.plots)
  #sum(all.pres$WGH) ### le presenze pesano come le assenze
  #glob.abs$WGH<-glob.abs$Nr.plots
  #loc.abs$WGH<-loc.abs$Nr.plots
  
  
  
  # NOW ASSEMBLING 
  b <- as.data.frame(rbind(all.pres[ ,  colnames(all.pres)[1:11]],
                           #loc.abs[,colnames(all.pres)],
                           glob.abs[,colnames(all.pres)[1:11]]))
  if(sum(b[ !is.na(b[,myRespName]) ,myRespName])>=10) {
    
  #apply(b, 2, function (x){sum(is.na(x))} )  ## 20k PA only in the myRespname colmn
  #class(b)
  #sum(duplicated(b$cells))
  #b<- b[!duplicated(b$cells),]
  #apply(b, 2, class)
  ## FIRST TRY TO WEIGHT PA
  myBiomodDataGLOB <- BIOMOD_FormatingData(resp.var= as.data.frame(b[  ,myRespName]) ,#World_pr.ab.TA_ENV.vals[ !is.na(World_pr.ab.TA_ENV.vals[myRespName]) & World_pr.ab.TA_ENV.vals[myRespName]==0 ,myRespName] ,
                                           
                                           
                                           resp.xy = b[,c("Longitude","Latitude")], 
                                           resp.name = myRespName,
                                           expl.var=b[ ,ly.names.def ],
                                           
                                           #eval.resp.var = as.data.frame(EVALUATION[ ,myRespName]) , ## taking only few absences
                                           #eval.expl.var = EVALUATION[ ,ly.names.def], ## here i also have the dummy var
                                           #eval.resp.xy = EVALUATION [,c("Longitude", "Latitude")],
                                           
                                           PA.nb.rep =3,   #Posso pure generare 
                                           PA.nb.absences=10000, #(length(b[b[,myRespName]==1 & !is.na(b[,myRespName]),myRespName])*5), ## vedi in letteratura circa 10 mila assenze?
                                           #PA.strategy = "random",  ##  to define the distances for PA selection
                                           ##PA.dist.min = 20000, ###new insertion
                                           ##PA.dist.max = 1000000,
                                           na.rm=T) ### i  remove an NA, i am not sure non mi slitta l'ordine
  
  
  #myBiomodOption <- BIOMOD_ModelingOptions(GLM=list(type="polynomial",test="AIC"), GBM=list(n.trees=3000) ,GAM=list(k=-1) )### same options as marta's
  myBiomodOption <- BIOMOD_ModelingOptions(
    GLM = list( type = 'quadratic', interaction.level = 1 ),
    #GBM = list( n.trees = 1000 ),
    GAM = list( algo = 'GAM_mgcv' , k=1,select=T) )
  
  ## Modelling
  myBiomodModelOutGLOB <- BIOMOD_Modeling(
    myBiomodDataGLOB,
    models = c( "GLM","GAM"),### i usually do:  c("GLM", "GBM", "GAM",  "RF"),
    models.options = myBiomodOption, 
    NbRunEval = n.run,# better if it is 3 ### if i want to estimate the variability in local performance caused by data split
    DataSplit = 70,## MARS is particularly long to compute
    Prevalence=0.5, # this to 0.5 gave me an orrible TSS
    #Yweights=b$WGH*1000000000, ## SE NON MOLTIPLICO MI DA ERRORE E PROBABILMENTE NON MI PESA NIENTE
    VarImport=3, ## 
    models.eval.meth = c('TSS','ROC'),   ### roc doesn't give me results valid if i need a threshols
    SaveObj = TRUE,
    #rescal.all.models = TRUE, vignette: if true, all model prediction will be scaled with a binomial GLM
    do.full.models = F, ## this was on F it is not correct however if i want to estimate the variability given by 
    transpose=F,
    modeling.id = paste(myRespName,"NATIVE_Modelling_Test",sep="_"))
  
  capture.output(get_evaluations(myBiomodModelOutGLOB),
                 file=file.path(myRespName, paste(myRespName,"NATIVE_formal_models_evaluation.txt", sep="_")))
  capture.output(get_variables_importance(myBiomodModelOutGLOB), file=file.path(myRespName, paste(myRespName,"NATIVE_formal_models_variables_importance.txt", sep="_")))
  if(sum( na.omit(myBiomodModelOutGLOB@models.evaluation@val[2,1,,,])>=0.8)>=2) ## ## i add the = because if a value is of 0.6 it is considered in the threshold
  {
    ### Building ensemble-models 
   myBiomodEMGLOB <- BIOMOD_EnsembleModeling(
      modeling.output = myBiomodModelOutGLOB, 
      chosen.models = 'all', ## this way i define i need only one EM
      em.by= 'all', #'PA_dataset+repet',  
      eval.metric = "ROC", ### evaluating only wiht TSS i will have only TSS maps
      eval.metric.quality.threshold = 0.8, ## this was 0.8 but if it's too high i may havo not a model for some species, interrupting the loop
      models.eval.meth = c('TSS', "ROC"),
      prob.mean = F,                           ### maybe i should choose roc evaluation or both TSS and ROC
      prob.cv = T,                             ## posso mettere pure due metriche forse
      #prob.ci = T, ## Estimate the confidence interval around the prob.mean
      ## SEPPUR INTERESSANTE, FECENDO IL CI NON POSSO POI FARE LA BINARIZZAZIONE SULL'ENSEMBLE MODEL QUINDI NON RITENGO SIA UTILE FARLO, TANTO HO COMUNQUE IL CV
      #prob.ci.alpha = 0.05, # this is the default
      #prob.median = F,
      committee.averaging = T,
      prob.mean.weight = T, # with the decay option, the differenes between models are less important
      #VarImport=2, # a very long time
      prob.mean.weight.decay = 'proportional'  ## this is the default values
    )
    
    capture.output(get_evaluations(myBiomodEMGLOB),
                   file=file.path(myRespName, paste(myRespName,"NATIVE_EM_formal_models_evaluation.txt", sep="_")))
    
    ### from Laure script
    # On these graphs, the points represent the mean of evaluation score for a given condition 
##      # and lines represents associated standard deviations.
##          models_scores_graph(myBiomodModelOutGLOB, by = "models" , metrics = c("ROC","TSS"), xlim = c(0.5,1), ylim = c(0.5,1))
##          models_scores_graph(myBiomodModelOutGLOB, by = "cv_run" , metrics = c("ROC","TSS"), xlim = c(0.5,1), ylim = c(0.5,1))
##          models_scores_graph(myBiomodModelOutGLOB, by = "data_set" , metrics = c("ROC","TSS"), xlim = c(0.5,1), ylim = c(0.5,1))
##          
##          MyModels_var_import <- get_variables_importance(myBiomodModelOutGLOB)
##          MyModels_var_import
##          dimnames(MyModels_var_import)
##          
##          # make the mean of variable importance by algorithm
##          mVarImp <- apply(MyModels_var_import, c(1,2), median) 
##          mVarImp <- apply(mVarImp, 2, function(x) x*(1/sum(x))) # standardize the data
##          mVarImp 
##          barplot(mVarImp, legend.text=row.names(mVarImp), col=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f'))
##          
##          MySpc_glm <- BIOMOD_LoadModels(myBiomodModelOutGLOB, models='GLM')
##          MySpc_gam <- BIOMOD_LoadModels(myBiomodModelOutGLOB, models='GAM')
##          #MySpc_gbm <- BIOMOD_LoadModels(myBiomodModelOutGLOB, models='GBM')
##          
##    glm_eval_strip <- biomod2::response.plot2(
##      models  = MySpc_glm, Data = get_formal_data(myBiomodModelOutGLOB,'expl.var'), 
##      show.variables= get_formal_data(myBiomodModelOutGLOB,'expl.var.names'),
##      do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
##      display_title = FALSE, data_species = get_formal_data(myBiomodModelOutGLOB,'resp.var'))
##        
##          gam_eval_strip <- biomod2::response.plot2(
##            models  = MySpc_gam, Data = get_formal_data(myBiomodModelOutGLOB,'expl.var'), 
##            show.variables= get_formal_data(myBiomodModelOutGLOB,'expl.var.names'),
##            do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
##            display_title = FALSE, data_species = get_formal_data(myBiomodModelOutGLOB,'resp.var'))
##          
##      #    gbm_eval_strip <- biomod2::response.plot2(
##      #      models  = MySpc_gbm, Data = get_formal_data(myBiomodModelOutGLOB,'expl.var'), 
##      #      show.variables= get_formal_data(myBiomodModelOutGLOB,'expl.var.names'),
##      #      do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
##      #      display_title = FALSE, data_species = get_formal_data(myBiomodModelOutGLOB,'resp.var'))
##      #    
##      #    
    
       # ### BIOMOD PROJECTIONS
       # myBiomodP1 <- BIOMOD_Projection(modeling.output=myBiomodModelOutGLOB,
       #                                 new.env = myexpl.var30_ST_DEF,
       #                                 proj.name="_NATIVE_MODEL_CURRENT_project",
       #                                 #selected.models="all",
       #                                 build.clamping.mask=T,
       #                                 output.format = ".img",
       #                                 binary.meth="ROC")
       # 
       # #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
       # #gc(reset=T)
       # 
       # ## CURRENT 
       # myBiomodEF1 <- BIOMOD_EnsembleForecasting(
       #   projection.output = myBiomodP1,
       #   #new.env = myexpl.var30_ST_DEF,   ### questi due non servono se faccio tutti i modelli
       #   proj.name = "EM_NATIVE_MODEL_CURRENT_project",  ### 
       #   EM.output = myBiomodEMGLOB,
       #   do.stack = FALSE,
       #   output.format = ".img",
       #   total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
       #   binary.meth = "ROC") ## maybe it's better by ROC, as in the EM option it is by ROC
       # ## with ROC this doesn't work using myBiomodProj projections
        
        #### Projecting the maps in future conditions
        ###  #### Projecting the maps in future conditions
        
        ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
        #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
        #gc(reset=T)
        
#        
#        ### BIOMOD PROJECTIONS
#        myBiomodP2 <- BIOMOD_Projection(modeling.output=myBiomodModelOutGLOB,
#                                        new.env = myexpl.var30_2050_45_CESM1,
#                                        proj.name="_NATIVE_MODEL_2050_45_CESM1_project",
#                                        #selected.models="all",
#                                        build.clamping.mask=T,
#                                        output.format = ".img",
#                                        binary.meth="ROC")
#        
#        #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        #gc(reset=T)
#        
#        
#        ### myexpl.var30_2050_45_CESM1
#        myBiomodEF2 <- BIOMOD_EnsembleForecasting(
#          projection.output = myBiomodP2,
#          #new.env = myexpl.var30_2050_45_CESM1,   ### questi due non servono se faccio tutti i modelli
#          proj.name = "EM_GLOBAL_MODEL_2050_45_CESM1_project",  ### 
#          EM.output = myBiomodEMGLOB,
#          do.stack = FALSE,
#          output.format = ".img",
#          total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#          binary.meth = "ROC") 
#        
#        ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
#        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        gc(reset=T)
#        
#        ### BIOMOD PROJECTIONS
#        myBiomodP3 <- BIOMOD_Projection(modeling.output=myBiomodModelOutGLOB,
#                                        new.env = myexpl.var30_2050_85_CESM1,
#                                        proj.name="_GLOBAL_MODEL_2050_85_CESM1_project",
#                                        #selected.models="all",
#                                        build.clamping.mask=T,
#                                        output.format = ".img",
#                                        binary.meth="ROC")
#        #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        #gc(reset=T)
#        
#        
#        
#        
#        
#        #myexpl.var30_2050_85_CESM1
#        myBiomodEF3 <- BIOMOD_EnsembleForecasting(
#          projection.output = myBiomodP3,
#          #new.env = myexpl.var30_2050_85_CESM1,   ### questi due non servono se faccio tutti i modelli
#          proj.name = "EM_GLOBAL_MODEL_2050_85_CESM1_project",  ### 
#          EM.output = myBiomodEMGLOB,
#          do.stack = FALSE,
#          output.format = ".img",
#          total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#          binary.meth = "ROC") 
#        
#        ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
#        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        gc(reset=T)
#        
#        ### BIOMOD PROJECTIONS
#        myBiomodP4 <- BIOMOD_Projection(modeling.output=myBiomodModelOutGLOB,
#                                        new.env = myexpl.var30_2050_45_CMCC,
#                                        proj.name="_GLOBAL_MODEL_2050_45_CMCC_project",
#                                        #selected.models="all",
#                                        build.clamping.mask=T,
#                                        output.format = ".img",
#                                        binary.meth="ROC")
#        
#        #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        #gc(reset=T)
#        
#        
#        #myexpl.var30_2050_45_CMCC 
#        myBiomodEF4 <- BIOMOD_EnsembleForecasting(
#          projection.output = myBiomodP4,
#          #new.env = myexpl.var30_2050_45_CMCC,   ### questi due non servono se faccio tutti i modelli
#          proj.name = "EM_GLOBAL_MODEL_2050_45_CMCC_project",  ### 
#          EM.output = myBiomodEMGLOB,
#          do.stack = FALSE,
#          output.format = ".img",
#          total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#          binary.meth = "ROC")
#        
#        ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
#        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        gc(reset=T)
#        
#        ### BIOMOD PROJECTIONS
#        myBiomodP5 <- BIOMOD_Projection(modeling.output=myBiomodModelOutGLOB,
#                                        new.env = myexpl.var30_2050_85_CMCC,
#                                        proj.name="_GLOBAL_MODEL_2050_85_CMCC_project",
#                                        #selected.models="all",
#                                        build.clamping.mask=T,
#                                        output.format = ".img",
#                                        binary.meth="ROC")
#        
#        #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        #gc(reset=T)
#        
#        
#        #myexpl.var30_2050_85_CMCC 
#        myBiomodEF5 <- BIOMOD_EnsembleForecasting(
#          projection.output = myBiomodP5,
#          # new.env = myexpl.var30_2050_85_CMCC,   ### questi due non servono se faccio tutti i modelli
#          proj.name = "EM_GLOBAL_MODEL_2050_85_CMCC_project",  ### 
#          EM.output = myBiomodEMGLOB,
#          do.stack = FALSE,
#          output.format = ".img",
#          total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#          binary.meth = "ROC") 
#        
#        ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
#        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        gc(reset=T)
#        
#        
#        ## 2070 PROJECTIONS ### BIOMOD PROJECTIONS
#        myBiomodP6 <- BIOMOD_Projection(modeling.output=myBiomodModelOutGLOB,
#                                        new.env = myexpl.var30_2070_45_CESM1,
#                                        proj.name="_GLOBAL_MODEL_2070_45_CESM1_project",
#                                        #selected.models="all",
#                                        build.clamping.mask=T,
#                                        output.format = ".img",
#                                        binary.meth="ROC")
#        
#        
#        #myexpl.var30_2070_45_CESM1
#        myBiomodEF6 <- BIOMOD_EnsembleForecasting(
#          projection.output = myBiomodP6,
#          #new.env = myexpl.var30_2070_45_CESM1,   ### questi due non servono se faccio tutti i modelli
#          proj.name = "EM_GLOBAL_MODEL_2070_45_CESM1_project",  ### 
#          EM.output = myBiomodEMGLOB,
#          do.stack = FALSE,
#          output.format = ".img",
#          total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#          binary.meth = "ROC") 
#        
#        ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
#        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        gc(reset=T)
#        
#        ### BIOMOD PROJECTIONS
#        myBiomodP7 <- BIOMOD_Projection(modeling.output=myBiomodModelOutGLOB,
#                                        new.env = myexpl.var30_2070_85_CESM1,
#                                        proj.name="_GLOBAL_MODEL_2070_85_CESM1_project",
#                                        #selected.models="all",
#                                        build.clamping.mask=T,
#                                        output.format = ".img",
#                                        binary.meth="ROC")
#        
#        
#        #myexpl.var30_2070_85_CESM1
#        myBiomodEF7 <- BIOMOD_EnsembleForecasting(
#          projection.output = myBiomodP7,
#          #new.env = myexpl.var30_2070_85_CESM1,   ### questi due non servono se faccio tutti i modelli
#          proj.name = "EM_GLOBAL_MODEL_2070_85_CESM1_project",  ### 
#          EM.output = myBiomodEMGLOB,
#          do.stack = FALSE,
#          output.format = ".img",
#          total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#          binary.meth = "ROC") 
#        
#        ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
#        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        gc(reset=T)
#        
#        ### BIOMOD PROJECTIONS
#        myBiomodP8 <- BIOMOD_Projection(modeling.output=myBiomodModelOutGLOB,
#                                        new.env = myexpl.var30_2070_45_CMCC,
#                                        proj.name="_GLOBAL_MODEL_2070_45_CMCC_project",
#                                        #selected.models="all",
#                                        build.clamping.mask=T,
#                                        output.format = ".img",
#                                        binary.meth="ROC")
#        
#        
#        #myexpl.var30_2070_45_CMCC 
#        myBiomodEF8 <- BIOMOD_EnsembleForecasting(
#          projection.output = myBiomodP8,
#          #new.env = myexpl.var30_2070_45_CMCC,   ### questi due non servono se faccio tutti i modelli
#          proj.name = "EM_GLOBAL_MODEL_2070_45_CMCC_project",  ### 
#          EM.output = myBiomodEMGLOB,
#          do.stack = FALSE,
#          output.format = ".img",
#          total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#          binary.meth = "ROC") 
#        
#        ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
#        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        gc(reset=T)
#        
#        ### BIOMOD PROJECTIONS
#        myBiomodP9 <- BIOMOD_Projection(modeling.output=myBiomodModelOutGLOB,
#                                        new.env = myexpl.var30_2070_85_CMCC,
#                                        proj.name="_GLOBAL_MODEL_2070_85_CMCC_project",
#                                        #selected.models="all",
#                                        build.clamping.mask=T,
#                                        output.format = ".img",
#                                        binary.meth="ROC")
#        
#        
#        #myexpl.var30_2070_85_CMCC 
#        myBiomodEF9 <- BIOMOD_EnsembleForecasting(
#          projection.output = myBiomodP9,
#          #new.env = myexpl.var30_2070_85_CMCC,   ### questi due non servono se faccio tutti i modelli
#          proj.name = "EM_GLOBAL_MODEL_2070_85_CMCC_project",  ### 
#          EM.output = myBiomodEMGLOB,
#          do.stack = FALSE,
#          output.format = ".img",
#          total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#          binary.meth = "ROC") 
#        
#        ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
#        unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#        gc(reset=T)
#        
        
    
  } else{ capture.output(get_evaluations(myBiomodModelOutGLOB),file=file.path(myRespName, paste(myRespName,"No_NATIVE_Model_over_ROC_0.8.txt", sep="_")))
  }
  ### saving number of presences
  capture.output(sum(b[ !is.na(b[,myRespName]) ,myRespName]) ,file=file.path(myRespName, paste(myRespName,"NATIVE_Nr.Pres.txt", sep="_")))
  ### saving few images
  ### VAR. IMP
  MyModels_var_import <- get_variables_importance(myBiomodModelOutGLOB)
  MyModels_var_import
  dimnames(MyModels_var_import)
  
  # make the mean of variable importance by algorithm
  mVarImp <- apply(MyModels_var_import, c(1,2), median) 
  mVarImp <- apply(mVarImp, 2, function(x) x*(1/sum(x))) # standardize the data
  
  png(filename=file.path(myRespName, paste(myRespName,"NATIVE_var.imp.png", sep="_")), width = 780, height = 480, pointsize = 12, bg = "white", res = NA)
  #par(mfrow=c(3,3))
  barplot(mVarImp, legend.text=row.names(mVarImp), col=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f'))
  
  dev.off()  
  ### RESPONCE CURVES
  ## uploading the models
  MySpc_glm <- BIOMOD_LoadModels(myBiomodModelOutGLOB, models='GLM')
  MySpc_gam <- BIOMOD_LoadModels(myBiomodModelOutGLOB, models='GAM')
  
  ## GLM
  png(filename=file.path(myRespName, paste(myRespName,"NATIVE_GLM_resp.curv.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  glm_eval_strip <- biomod2::response.plot2(
    models  = MySpc_glm, Data = get_formal_data(myBiomodModelOutGLOB,'expl.var'),
    show.variables= get_formal_data(myBiomodModelOutGLOB,'expl.var.names'),
    do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
    display_title = FALSE, data_species = get_formal_data(myBiomodModelOutGLOB,'resp.var'))
  dev.off()  
  
  ##GAM
  png(filename=file.path(myRespName, paste(myRespName,"NATIVE_GAM_resp.curv.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  glm_eval_strip <- biomod2::response.plot2(
    models  = MySpc_gam, Data = get_formal_data(myBiomodModelOutGLOB,'expl.var'),
    show.variables= get_formal_data(myBiomodModelOutGLOB,'expl.var.names'),
    do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
    display_title = FALSE, data_species = get_formal_data(myBiomodModelOutGLOB,'resp.var'))
  dev.off()  
  
  
  ## presences and absences
  b[is.na(b)] = 0 # eliminating NA's
  png(filename=file.path(myRespName, paste(myRespName,"NATIVE_pres.abs.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  par(mfrow=c(3,3))
  for(i in ly.names.def) plot(b[,myRespName]~ b[,i], data=b, xlab=i, cex=.5)
  dev.off()  
  #capture.output(sum(b[ !is.na(b[,myRespName]) ,myRespName]),file=file.path(myRespName, paste(myRespName,"NATIVE_Nr.Pres.txt", sep="_")))
  
  
  
    
  
  save.image(paste(myRespName,"_NATIVE.Rdata"))
  cat('completed',myRespName,'_NATIVE_Model')
  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
  #gc(reset=T)
  rm(all.pres)
  rm(b)
  rm(glob.abs)
  rm(list=grep(myRespName,ls(), value=T))
  
  
  gc(reset="NATIVE" )
  gc(reset="b")
  gc(reset="glob.abs")
  } else{
    ### SE AVEVO MENO DI 5 PUNTI NEL NATIVE SKYPPO TUTTO E ARRIVO DIRETTAMENTE QUI
    capture.output(sum(b[ !is.na(b[,myRespName]) ,myRespName]) ,file=file.path(myRespName, paste(myRespName,"A_no-enough nativr pres.txt", sep="_")))
    
  }
  
}







