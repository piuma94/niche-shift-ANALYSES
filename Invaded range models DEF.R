

#setwd("/Volumes/Extreme SSD/Secondo Articolo/R Projects/Laure_suggestions")
#setwd("/Volumes/UNTITLED/Laure_suggestions ")  ## fisso casa, extreme SSD
##setwd("/Volumes/WD Elements/Luigi/Laure_suggestions")  ## MAC, WD

#setwd("E:\\Secondo Articolo\\R Projects\\Laure_suggestions") ## fisso casa

#setwd("D:\\Luigi\\Laure_suggestions")  ## hard disk 12 TB from fisso home
#setwd("/Volumes/Extreme SSD/Secondo Articolo/R Projects/Laure_suggestions/Dataset e modelli DEF") ### mac Luigi extreme SSD
#setwd("G:\\New test with laure")


### fisso casa
setwd("H:\\New test with laure")
#setwd("C:\\Users\\luigi\\OneDrive - Universita degli Studi Roma Tre\\New test with laure")

#devtools::install_github('biomodhub/biomod2')
## Setting the environment ###
## for windows
#memory.limit(size=56000)
## uploading some libraries
library(dplyr)

library(biomod2)
library(raster)
library(dismo)
library(ecospat)
library(sf)

#rm(list = ls(all.names = TRUE))



####### adding the same names to the  data, 
load("ly.names.def")


## MED_ENV WHERE I WANT TO PROJECT
myexpl.var30_ST_DEF <- stack("myexpl.var30_ST_DEF.tif")  ## here i am saving the all dataset cropped
myexpl.var30_ST_DEF [[6]]<-log(myexpl.var30_ST_DEF [[6]]+1)

names(myexpl.var30_ST_DEF)<- ly.names.def




###########  uploading the datasets

#### GRILLED DATASETS FOR EVA AND GBIF #####
### EVA 30 and 70
load("EVAE.70.Rda")
load("EVAT.30.Rda")
## first assembling a complete single dataset
## the species i need to evaluate
sp.names <- colnames(EVAE.70[,5:97])  ## check these ## 

## first assembling a complete single dataset
EVA <- rbind(EVAE.70[,-106],EVAT.30[,-106])

### GBIF_ grilled with ENV.vals
load(file="GBIF_grilled.P2.Rda") ## this is the best, i will not have duplicates when reimporting
GBIF_grilled <- GBIF_grilled[GBIF_grilled$CHELSA_bio10_16<=5000,] ## ok, this was only one plot only in the global buffer

## QUESTI DATASET SONO BLINDATI, super controllati ed esenti errori. Un unico plot in ogni cella. 


### EVALUATING NEW BKG points
### newly evaluated BGK points
#load("GLOBAL_BKG.Rda")
#load("LOCAL_BKG.Rda")
load("LOCAL_BKG_NICHE_AN.Rda") ## this is the newly evaluated

#load("LOCAL_EVAGBIFBKG.Rda")
## not usign local in the local buffer, but local BKG in the LOCAL BUFFER from EVA+GBIF
###Local_BKG<-EVA_BKG_Weigth ## this one has been done only in the buffer around EVA Presences
Local_BKG <- EVA_BKG_Weigth ## this has been done using the EXTENT of EVA to cut GBIF and then do a buffer with both
rm(EVA_BKG_Weigth)

#Global_BKG<-GBIF_BKG_Weigth
#rm(GBIF_BKG_Weigth)


## checking them
apply(Local_BKG,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
#apply(Global_BKG,2, function(x) sum(is.na(x)))   ## ok i have no NA in all the rows
## eliminating NA's
Local_BKG<-na.omit(Local_BKG)
#Global_BKG<-na.omit(Global_BKG)
# STANDARDIZZO LE BKG CIOE' DEVO PARTIRE DALLO STESSO NUMERO


#### TAKING ONLY PRESENCES IN THE STRAIGHT STUDY AREA
## intersezione tra med_env e EVA/GBIF ## solo le presenze però, perchè le assenze non  mi servono
GBIF_grilled_study_area<- cbind(GBIF_grilled,raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_grilled[,c("Longitude","Latitude")]))
length(GBIF_grilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_grilled[, c("Longitude", `[!is.na(GBIF_grilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_grilled[, c("Longitude", `)]) ### 53083 values are not NA

EVA_study_area<- cbind(EVA,raster::extract(myexpl.var30_ST_DEF[[1]], EVA[,c("Longitude","Latitude")]))
length(EVA_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA[, c("Longitude", `[!is.na(EVA_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA[, c("Longitude", `)]) ### 34033 values are not NA

Local_BKG<- cbind(Local_BKG,raster::extract(myexpl.var30_ST_DEF[[1]], Local_BKG[,c("Longitude","Latitude")]))
length(Local_BKG$`raster::extract(myexpl.var30_ST_DEF[[1]], Local_BKG[, c("Longitude", ` [!is.na(Local_BKG$`raster::extract(myexpl.var30_ST_DEF[[1]], Local_BKG[, c("Longitude", `)]) ### 333539 values are not NA

### Eliminating NA's cioè quelli fuori il med_europe# taglio parecchio stretto
EVA_study_area  <- EVA_study_area[!is.na(EVA_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], EVA[, c("Longitude", `),]
GBIF_grilled_study_area <- GBIF_grilled_study_area[!is.na(GBIF_grilled_study_area$`raster::extract(myexpl.var30_ST_DEF[[1]], GBIF_grilled[, c("Longitude", `),]
Local_BKG <- Local_BKG[!is.na(Local_BKG$`raster::extract(myexpl.var30_ST_DEF[[1]], Local_BKG[, c("Longitude", `),]


apply(EVA_study_area,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows
apply(GBIF_grilled_study_area,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows
apply(Local_BKG,2, function(x) sum(is.na(x)))   ## ok i have  NA in all the rows
plot(myexpl.var30_ST_DEF[[1]])
points(Local_BKG[,c("Longitude","Latitude")], cex=0.1, pch=22)



###### WEIGHTNENING PA/BKG
## subsetting global to subset also Local
Local_BKG <-Local_BKG[0:400000,]  ### I am taking only one 400 k perch� mi aspetto che anche nel native non ne avr� pi� di cos�

### rescaling from 0:100
#Local_BKG$EVA_Nr.plots <- round(Local_BKG$EVA_Nr.plots/3,0)+1
#Local_BKG$GBIF_Nr.pres <- round(Local_BKG$GBIF_Nr.pres/3,0)+1

## I want not oto give the model too much absences, to not pull the curve down. So i am giving a subset of my BKG points but extracted considerign their weight considering the sampling intensity
PROB <- (Local_BKG$EVA_Nr.plots+Local_BKG$GBIF_Nr.pres)/max(Local_BKG$EVA_Nr.plots+Local_BKG$GBIF_Nr.pres)
Local_BKG <- Local_BKG[sample(row.names(Local_BKG), size=100000, prob=PROB),]





### preparing the loop
sp.names <- colnames(EVA[,5:97])

n.run <- 3
rm(EVA)
rm(GBIF_grilled)

unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
gc(reset=T)

## first set c(89 ,92 ,47 ,12, 59 ,63 ,42 ,36 ,62,  1 ,83, 60 ,52 ,38 ,64 ,13 ,17 ,53 ,79  ,9 ,19 ,29, 61, 18 ,91, 51 ,22, 24 ,71 ,78)
## second set c(4,20,45,75,81)
for( species in sp.names[]) {
  myRespName <-  species
  #myRespName <-  "Acacia.dealbata"
  #myRespName <- sp.names[4]
  #load(paste(myRespName,"_LOCAL.Rdata"))
  
  ### combining presences
  all.pres<-rbind(EVA_study_area[!is.na(EVA_study_area[myRespName]) & EVA_study_area[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)],
                  GBIF_grilled_study_area[!is.na(GBIF_grilled_study_area[myRespName]) & GBIF_grilled_study_area[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)]
                 # EVAT.30[!is.na(EVAT.30[myRespName]) & EVAT.30[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)]
                  #GBIF_grilled_env_EUROPE_EVALUATION[!is.na(GBIF_grilled_env_EUROPE_EVALUATION[myRespName]) & GBIF_grilled_env_EUROPE_EVALUATION[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)]
  )
  # sum(duplicated(all.pres$cells))  ## 3 presences are repeated
  # apply(all.pres, 2, function (x){sum(is.na(x))} )  ## 
  all.pres$Population.density.2000<-log(all.pres$Population.density.2000+1)
  
  ## I NEED TO WEIGTH PA FOR EACH MODEL ISTEAD ALL PRESENCES SHOULD HAVESAME WEIGHT AND PREVALENCE 1
  ##  cells is the unified cellID, 
  ### global weighted PA NOT 
  ## eliminating PA that falls over presences of my species
  # as i will take the first 10k i am not having troubles, i always use ALMOST the same cells for each species
  #GBIF_BKG_Weigth<-GBIF_BKG_Weigth[,2:14]
  
  
  ## IN THIS SITUATIONS WE ONLY NEED LOCAL ABSENCES TO BE WEIGHTED CONSIDERING THE DATASETS
  ## combining and selecting 10k global absences
  #glob.abs <- GBIF_BKG_Weigth_env[ !GBIF_BKG_Weigth_env$cells %in% intersect(GBIF_BKG_Weigth_env[,"cells"],all.pres[  ,"cells"]) ,]
  #glob.abs<-glob.abs[1:10000,]
  #glob.abs$prop <-  glob.abs[,"Nr.plots"]/sum(glob.abs[,"Nr.plots"])
  ### check
  #sum(glob.abs$prop) ## it's 1 ok
  
  ### Local weighted PA
  #EVA_BKG_Weigth<-EVA_BKG_Weigth[,2:14]
  
  ## combining and selecting 10k local absences ### THESE SHOULD BE CORRECTED FOR EVAT30 E GBF EUROEPE EVALUATIONS
  ## THESE ARE BOTH BKG LESS THAN PRESENCES AND EVALUATIONS
#  loc.abs<- EVA_BKG_Weigth[ !EVA_BKG_Weigth$cells %in% intersect(EVA_BKG_Weigth[,"cells"], c(all.pres[ ,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1, "cells" ],EVAT.30[EVAT.30[,myRespName]==1, "cells" ] )   ) ,] ## this is a correction, based on the all presences, not only few of them
  #loc.abs<-loc.abs[1:10000,]
  
  ## IS THE CORRECT CELL??  
  
    loc.abs<- Local_BKG[ !Local_BKG$cells %in% intersect(Local_BKG[,"cells"], c(all.pres[ ,"cells"] )   ) ,] ## this is a correction, based on the all presences, not only few of them
  
  loc.abs$Population.density.2000<-log(loc.abs$Population.density.2000+1)
  
  
  # THE COL NR.PLOTS IS THE EVA SAMPLING INTENSITY
  #loc.abs$EVA.Sampl.prop <-  loc.abs[,"Nr.plots"]/sum(loc.abs[,"Nr.plots"])
  ## THE COLUMN GBIF_sampl.int DESCRIBE THE NUMBER OF GBIF PLOTS IN EACH ABSENCE CELL
  #loc.abs$GBIF.Sampl.prop <-  loc.abs[,"GBIF_sampl.int"]/sum(loc.abs[,"GBIF_sampl.int"]) ## Remeber, this is GBIF s.intensity but in the same local buffered area of EVA
  
  # IMPORTANT 
  ## now i need a single value that describe the sampling intensity of each dataset:EVA,GBIF on the same points. 
  # I CAN SUM THE WEIGHTED VALUE AND BALANCE ALL PRESENCES DI CONSEGUENZA
  #loc.abs$prop<-loc.abs$EVA.Sampl.prop+ loc.abs$GBIF.Sampl.prop
  
  ###### PICCOLO INCISO:
  # i miei due metodi di pesare le assenze, sono basati su una standardizzazione tra zero e uno per ogni dataset
  # così ogni dataset ha due valori che sono basati sul loro stesso massimo,
  #STANDARDIZZATI TRA ZERO E UNO PER OGNI DATASET
  ## check
 # sum(loc.abs$prop) ## it's 2 ok
  
  ## eliminating PA over presences may have a little effect
  ## Eliminating absences over presences for EVALUATION is foundamental
  
#  glob.abs <- GBIF_BKG_Weigth[ !GBIF_BKG_Weigth$cells %in% intersect(GBIF_BKG_Weigth[,"cells"], c(all.pres[ ,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1, "cells" ],EVAT.30[EVAT.30[,myRespName]==1, "cells" ] )    ),]
#  # glob.abs<-glob.abs[1:10000,] keep all global absences
#  # glob.abs$prop <-  glob.abs[,"Nr.plots"]/sum(glob.abs[,"Nr.plots"]) do not weight them
#  ## check
#  #sum(glob.abs$prop) ## it's 1 ok
#  sum(glob.abs$Nr.plots) ### 37648
#  glob.abs$Population.density.2000<-log(glob.abs$Population.density.2000+1)
  
  
  
  
  
  ### eliminating EVAT.30 pa that fall over 
  #EVAT.30<-EVAT.30[,2:145]
  ## sistemata to the evaluation set
#  EVALUATION <- EVAT.30[ !EVAT.30$cells %in% intersect(EVAT.30[EVAT.30[,myRespName]==0,"cells"],all.pres[all.pres[,myRespName]==1  ,"cells"]) ,colnames(all.pres)[1:11]]  ## here i don't have the props col
  #apply(EVALUATION, 2, function (x){sum(is.na(x))} )  ## 20k PA only in the myRespname colmn
  
  ### ADDING ALSO GBIF EVALUATIONS
  ## ADD HERE A FILTER OVER EVA ABSENCES ON GBIF
  ## IN GBIF HO ANCHE DEGLI ZERO, PERCHE' PER? LI E' PRESENTE UN'ALTRA ALIENA, MA NON LA MIA
  
  #### Prima tolgo le assenze, che mi cadono sopra le presenze del mio evaluation set
#  EVALUATION<-EVALUATION[ !EVALUATION$cells %in% intersect(EVALUATION[EVALUATION[,myRespName]==0,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1  ,"cells"]),] ## ELIMINATING ABSENCES THAT FALL OVER GBIF_EVALUATIONS PRESENCES
  
  
  
#  EVALUATION<-rbind(EVALUATION,      GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1  ,colnames(all.pres)[1:11] ]) ## ADDING ONLY MYRESPNAME PRESENCES
#  EVALUATION$Population.density.2000<-log(EVALUATION$Population.density.2000+1)
  
  ## attento, così vado ad eliminare sia la presenza che l'assenza in quella cella
  ## ATTENTO, DEVE ESSERE MODIFICATO!!
  #EVALUATION<-EVALUATION[ !EVALUATION$cells %in% intersect(EVALUATION[EVALUATION[,myRespName]==0,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1  ,"cells"]),] ## ELIMINATING ABSENCES THAT FALL OVER GBIF_EVALUATIONS PRESENCES
  
  ## Subsetting EVALUATION with cellsd ID for where i want to project
  
#  EVALUATION_MED_ENV <- EVALUATION[EVALUATION$cells %in% intersect(Proj.cells$cells, EVALUATION$cells),] ## here i subsect only really cells in which i am projecting
#  EVALUATION_MED_ENV$Population.density.2000<-log(EVALUATION_MED_ENV$Population.density.2000+1)
  
  
  
  
  ### WEIGHTING PRESENCES RESPECTIVELY
  #all.pres$prop<- rep(2/length(all.pres$cells),length(all.pres$cells)) ## all presence has the same weight
  ## performing the analyses 
  ## FIRST ASSEMBLING THE FINAL DATASET FOR THIS PSECIES 
  #colnames(all.pres)
  # PA for loc.abs  glob.abs
  loc.abs[,myRespName]<- rep(NA,dim(loc.abs)[1])
  
  
  
#  glob.abs[,myRespName]<- rep(NA,dim(glob.abs)[1])
  
  
  
  # NOW ASSEMBLING 
  b <- as.data.frame(rbind(all.pres[ ,  colnames(all.pres)],
                           loc.abs[,colnames(all.pres)]
                           #,glob.abs[,colnames(all.pres)]))
  ))
  #apply(b, 2, function (x){sum(is.na(x))} )  ## 20k PA only in the myRespname colmn
  #class(b)
  #sum(duplicated(b$cells))
  #b<- b[!duplicated(b$cells),]
  #apply(b, 2, class)
  ## FIRST TRY TO WEIGHT PA
  myBiomodDataLOCAL <- BIOMOD_FormatingData(resp.var= as.data.frame(b[  ,myRespName]) ,#World_pr.ab.TA_ENV.vals[ !is.na(World_pr.ab.TA_ENV.vals[myRespName]) & World_pr.ab.TA_ENV.vals[myRespName]==0 ,myRespName] ,
                                           
                                           
                                           resp.xy = b[,c("Longitude","Latitude")], 
                                           resp.name = myRespName,
                                           expl.var=b[ ,ly.names.def ],
                                           
                                           #eval.resp.var = as.data.frame(EVALUATION[ ,myRespName]) , ## taking only few absences
                                           #eval.expl.var = EVALUATION[ ,ly.names.def], ## here i also have the dummy var
                                           #eval.resp.xy = EVALUATION [,c("Longitude", "Latitude")],
                                           
                                           PA.nb.rep =3,   #Posso pure generare 
                                           PA.nb.absences=10000, #(length(World_pr.ab[World_pr.ab[,myRespName]==1,myRespName])*5), ## vedi in letteratura circa 10 mila assenze?
                                           #PA.strategy = "random",  ##  to define the distances for PA selection
                                           ##PA.dist.min = 20000, ###new insertion
                                           ##PA.dist.max = 1000000,
                                           na.rm=T) ### i  remove an NA, i am not sure non mi slitta l'ordine
  
  
  #myBiomodOption <- BIOMOD_ModelingOptions(GLM=list(type="polynomial",test="AIC"), GBM=list(n.trees=3000) ,GAM=list(k=-1) )### same options as marta's
  myBiomodOption <- BIOMOD_ModelingOptions(
    GLM = list( type = "quadratic", interaction.level = 1),
    #GBM = list( n.trees = 1000 ),
    GAM = list( algo = 'GAM_mgcv' , k=1,select=T))
  
  
  
  ## Modelling
  myBiomodModelOutLOCAL <- BIOMOD_Modeling(
    myBiomodDataLOCAL,
    models = c("GLM","GAM"),### i usually do:  c("GLM", "GBM", "GAM",  "RF"),
    models.options = myBiomodOption, 
    NbRunEval = n.run,# better if it is 3 ### if i want to estimate the variability in local performance caused by data split
    DataSplit = 70,## 
    Prevalence=0.5, # this to 0.5 gave me an orrible TSS
    #Yweights=b$prop*1000000000, ## SE NON MOLTIPLICO MI DA ERRORE E PROBABILMENTE NON MI PESA NIENTE
    VarImport=3, ## 
    models.eval.meth = c('TSS','ROC'),   ### roc doesn't give me results valid if i need a threshols
    SaveObj = TRUE,
    #rescal.all.models = TRUE, vignette: if true, all model prediction will be scaled with a binomial GLM
    do.full.models = F, ## this was on F it is not correct however if i want to estimate the variability given by 
    transpose=F,
    modeling.id = paste(myRespName,"INVADEDRANGE_Modelling_Test",sep="_"))
  
  capture.output(get_evaluations(myBiomodModelOutLOCAL),
                 file=file.path(myRespName, paste(myRespName,"INVADEDRANGE_formal_models_evaluation.txt", sep="_")))
  capture.output(get_variables_importance(myBiomodModelOutLOCAL), file=file.path(myRespName, paste(myRespName,"INVADEDRANGE_formal_models_variables_importance.txt", sep="_")))

  
  
  
  
  
  ### from Laure script
  # On these graphs, the points represent the mean of evaluation score for a given condition 
  # and lines represents associated standard deviations.
  #    models_scores_graph(myBiomodModelOutLOCAL, by = "models" , metrics = c("ROC","TSS"), xlim = c(0.5,1), ylim = c(0.5,1))
  #    models_scores_graph(myBiomodModelOutLOCAL, by = "cv_run" , metrics = c("ROC","TSS"), xlim = c(0.5,1), ylim = c(0.5,1))
  #    models_scores_graph(myBiomodModelOutLOCAL, by = "data_set" , metrics = c("ROC","TSS"), xlim = c(0.5,1), ylim = c(0.5,1))
  #    
  #    MyModels_var_import <- get_variables_importance(myBiomodModelOutLOCAL)
  #    MyModels_var_import
  #    dimnames(MyModels_var_import)
  #    
  #    # make the mean of variable importance by algorithm
  #    mVarImp <- apply(MyModels_var_import, c(1,2), median) 
  #    mVarImp <- apply(mVarImp, 2, function(x) x*(1/sum(x))) # standardize the data
  #    mVarImp 
  #    barplot(mVarImp, legend.text=row.names(mVarImp), col=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f'))
  #    
  #    MySpc_glm <- BIOMOD_LoadModels(myBiomodModelOutLOCAL, models='GLM')
  #    MySpc_gam <- BIOMOD_LoadModels(myBiomodModelOutLOCAL, models='GAM')
  #    #MySpc_gbm <- BIOMOD_LoadModels(myBiomodModelOutLOCAL, models='GBM')
  #    
  #    glm_eval_strip <- biomod2::response.plot2(
  #      models  = MySpc_glm, Data = get_formal_data(myBiomodModelOutLOCAL,'expl.var'), 
  #      show.variables= get_formal_data(myBiomodModelOutLOCAL,'expl.var.names'),
  #      do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
  #      display_title = FALSE, data_species = get_formal_data(myBiomodModelOutLOCAL,'resp.var'))
  #    
  #    gam_eval_strip <- biomod2::response.plot2(
  #      models  = MySpc_gam, Data = get_formal_data(myBiomodModelOutLOCAL,'expl.var'), 
  #      show.variables= get_formal_data(myBiomodModelOutLOCAL,'expl.var.names'),
  #      do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
  #      display_title = FALSE, data_species = get_formal_data(myBiomodModelOutLOCAL,'resp.var'))
  #    
  #    gbm_eval_strip <- biomod2::response.plot2(
  #      models  = MySpc_gbm, Data = get_formal_data(myBiomodModelOutLOCAL,'expl.var'), 
  #      show.variables= get_formal_data(myBiomodModelOutLOCAL,'expl.var.names'),
  #      do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
  #      display_title = FALSE, data_species = get_formal_data(myBiomodModelOutLOCAL,'resp.var'))
  #    
  #    
  
  
  
  
  
  
  
  
  
  
  
  
    ## controllo se ho almeno un valore maggiore di 0.7
  # Faccio l'if, le proiezioni e tutto, altrimenti vado in else
  ### AGGIUNGI IL CHECK SUGLI NA'S
  if(sum( na.omit(myBiomodModelOutLOCAL@models.evaluation@val[2,1,,,])>=0.8)>=2) ## i add the = because if a value is of 0.6 it is considered in the threshold
         {
  
    ### Building ensemble-models 
    myBiomodEMLOCAL <- BIOMOD_EnsembleModeling(
      modeling.output = myBiomodModelOutLOCAL, 
      chosen.models = 'all', ## this way i define i need only one EM
      em.by= 'all', #'PA_dataset+repet',  
      eval.metric = c("ROC"), ### evaluating only wiht TSS i will have only TSS maps
      eval.metric.quality.threshold = c(0.8), ## this was 0.8 but if it's too high i may have not a model for some species, interrupting the loop
      models.eval.meth = c('TSS',"ROC"),
      prob.mean = F,                           ### maybe i should choose roc evaluation or both TSS and ROC
      prob.cv = T,                             ## posso mettere pure due metriche forse
      #prob.ci = T, ## Estimate the confidence interval around the prob.mean
      ## INSERTING THIS DON'T ALLOW ME TO ESTIMATE 
      #prob.ci.alpha = 0.05, # this is the default
      #prob.median = F,
      committee.averaging = T,
      prob.mean.weight = T, # with the decay option, the differenes between models are less important
      #VarImport=2, # a very long time
      prob.mean.weight.decay = 'proportional'  ## this is the default values
    )
    ### save ensemble models evaluation scores and variables importance on hard drive 
    capture.output(get_evaluations(myBiomodEMLOCAL),
                   file=file.path(myRespName, paste(myRespName,"INVADEDRANGE_EM_formal_models_evaluation.txt", sep="_")))
    
    

    
    
    
#    
#    ### the three mean values to have a comparable result
#    testLOCAL <- biomod2::evaluate(myBiomodEMLOCAL,
#                                  EVALUATION[EVALUATION[,myRespName]==1 | EVALUATION[,"cells"] %in% sample(EVALUATION$cells, size=sum(EVALUATION[,myRespName]==1)) ,colnames(all.pres)[1:11]],stat = c("TSS", "ROC"))
#    
#    testLOCAL1 <- biomod2::evaluate(myBiomodEMLOCAL,
#                                   EVALUATION[EVALUATION[,myRespName]==1 | EVALUATION[,"cells"] %in% sample(EVALUATION$cells, size=sum(EVALUATION[,myRespName]==1)) ,colnames(all.pres)[1:11]],stat = c("TSS", "ROC"))
#    
#    testLOCAL3 <- biomod2::evaluate(myBiomodEMLOCAL,
#                                   EVALUATION[EVALUATION[,myRespName]==1 | EVALUATION[,"cells"] %in% sample(EVALUATION$cells, size=sum(EVALUATION[,myRespName]==1)) ,colnames(all.pres)[1:11]],stat = c("TSS", "ROC"))
#    
#    capture.output( (testLOCAL[[paste(myRespName,"_EMwmeanByROC_mergedAlgo_mergedRun_mergedData", sep="")]]+testLOCAL1[[paste(myRespName,"_EMwmeanByROC_mergedAlgo_mergedRun_mergedData", sep="")]]+testLOCAL3[[paste(myRespName,"_EMwmeanByROC_mergedAlgo_mergedRun_mergedData", sep="")]])/3,
#                    file=file.path(myRespName, paste(myRespName,"LOCAL_test.w.mean_onEVAT30_meanof_3_prova.txt", sep="_")))
#    
#    testLOCALall <- biomod2::evaluate(myBiomodEMLOCAL,
#                                     EVALUATION[,colnames(all.pres)[1:11]],          
#                                     stat = c("TSS", "ROC"))
#    
#    
#    capture.output(testLOCALall,
#                   file=file.path(myRespName, paste(myRespName,"LOCAL_test.w.mean_onEVAT30_all.abs_prova.txt", sep="_")))
#    
#    
#    ## More Precise test in the area i am projecting IN
#    ### the three mean values to have a comparable result
#    testLOCAL <- biomod2::evaluate(myBiomodEMLOCAL,
#                                   EVALUATION_MED_ENV[EVALUATION_MED_ENV[,myRespName]==1 | EVALUATION_MED_ENV[,"cells"] %in% sample(EVALUATION_MED_ENV$cells, size=sum(EVALUATION_MED_ENV[,myRespName]==1)) ,colnames(all.pres)[1:11]],stat = c("TSS", "ROC"))
#    
#    testLOCAL1 <- biomod2::evaluate(myBiomodEMLOCAL,
#                                    EVALUATION_MED_ENV[EVALUATION_MED_ENV[,myRespName]==1 | EVALUATION_MED_ENV[,"cells"] %in% sample(EVALUATION_MED_ENV$cells, size=sum(EVALUATION_MED_ENV[,myRespName]==1)) ,colnames(all.pres)[1:11]],stat = c("TSS", "ROC"))
#    
#    testLOCAL3 <- biomod2::evaluate(myBiomodEMLOCAL,
#                                    EVALUATION_MED_ENV[EVALUATION_MED_ENV[,myRespName]==1 | EVALUATION_MED_ENV[,"cells"] %in% sample(EVALUATION_MED_ENV$cells, size=sum(EVALUATION_MED_ENV[,myRespName]==1)) ,colnames(all.pres)[1:11]],stat = c("TSS", "ROC"))
#    
#    capture.output( (testLOCAL[[paste(myRespName,"_EMwmeanByROC_mergedAlgo_mergedRun_mergedData", sep="")]]+testLOCAL1[[paste(myRespName,"_EMwmeanByROC_mergedAlgo_mergedRun_mergedData", sep="")]]+testLOCAL3[[paste(myRespName,"_EMwmeanByROC_mergedAlgo_mergedRun_mergedData", sep="")]])/3,
#                    file=file.path(myRespName, paste(myRespName,"LOCAL_IN.ST.AREA.test.w.mean_onEVAT30_meanof_3_prova.txt", sep="_")))
#    
#    testLOCALall <- biomod2::evaluate(myBiomodEMLOCAL,
#                                      EVALUATION_MED_ENV[,colnames(all.pres)[1:11]],          
#                                      stat = c("TSS", "ROC"))
#    
#    
#    capture.output(testLOCALall,
#                   file=file.path(myRespName, paste(myRespName,"LOCAL_IN.ST.AREA.test.w.mean_onEVAT30_all.abs_prova.txt", sep="_")))
#    
#    ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
#    #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#    #gc(reset=T)
#    # BUT WILL THE MAPS BEING PERFORMED THE SAME
#
#  
#  ### Make ensemble-models projections on current variable
#  ## THIS METHOD IS JUST TO DO THE ENSEMBLE MODELING
  ### BIOMOD PROJECTIONS
 # myBiomodP1 <- BIOMOD_Projection(modeling.output=myBiomodModelOutLOCAL,
 #                                   new.env = myexpl.var30_ST_DEF,
 #                                   proj.name="INVADEDRANGE_MODEL_CURRENT_project",
 #                                   #selected.models="all",
 #                                   build.clamping.mask=T,
 #                                   #do.stack = FALSE, ## check it?
 #                                   output.format = ".img",
 #                                   binary.meth="ROC") ## here ROC WORKs
 #
 # ### NON POSSO FARLO, NON AVREI PIù LE PROIEZIONI
 #  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
 # #gc(reset=T)
 # 
 # ## CURRENT 
 # myBiomodEF1 <- BIOMOD_EnsembleForecasting(
 #   projection.output = myBiomodP1,
 #   #new.env = myexpl.var30_ST_DEF,   ### questi due non servono se faccio tutti i modelli
 #   proj.name = "EM_INVADEDRANGE_MODEL_CURRENT_project",  ### 
 #   EM.output = myBiomodEMLOCAL,
 #   do.stack = FALSE,
 #   output.format = ".img",
 #   total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
 #   binary.meth = "ROC"
 #  ) ## maybe it's better by ROC, as in the EM option it is by ROC
 # ## with ROC this doesn't work using myBiomodProj projections
 # 
  ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
  gc(reset=T)
  
 #### Projecting the maps in future conditions
 ### BIOMOD PROJECTIONS
# myBiomodP2 <- BIOMOD_Projection(modeling.output=myBiomodModelOutLOCAL,
#                                 new.env = myexpl.var30_2050_45_CESM1,
#                                 proj.name="_LOCAL_MODEL_2050_45_CESM1_project",
#                                 #selected.models="all",
#                                 build.clamping.mask=T,
#                                 output.format = ".img",
#                                 binary.meth="ROC") ## here ROC WORKs
# 
# #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
# #gc(reset=T)
# 
# ### myexpl.var30_2050_45_CESM1
# myBiomodEF2 <- BIOMOD_EnsembleForecasting(
#   projection.output = myBiomodP2,
#   #new.env = myexpl.var30_2050_45_CESM1,   ### questi due non servono se faccio tutti i modelli
#   proj.name = "EM_LOCAL_MODEL_2050_45_CESM1_project",  ### 
#   EM.output = myBiomodEMLOCAL,
#   do.stack = FALSE,
#   output.format = ".img",
#   total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#   binary.meth = "ROC") 
# 
# 
# ### cleaning the memory, it will be really usefull to clean the disk during several models tryals
# unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
# gc(reset=T)
# 
#  ### BIOMOD PROJECTIONS
#  myBiomodP3 <- BIOMOD_Projection(modeling.output=myBiomodModelOutLOCAL,
#                                  new.env = myexpl.var30_2050_85_CESM1,
#                                  proj.name="_LOCAL_MODEL_2050_85_CESM1_project",
#                                  #selected.models="all",
#                                  build.clamping.mask=T,
#                                  output.format = ".img",
#                                  binary.meth="ROC") ## here ROC WORKs
#  
#  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  #gc(reset=T)
#  
#  #myexpl.var30_2050_85_CESM1
#  myBiomodEF3 <- BIOMOD_EnsembleForecasting(
#    projection.output = myBiomodP3,
#    #new.env = myexpl.var30_2050_85_CESM1,   ### questi due non servono se faccio tutti i modelli
#    proj.name = "EM_LOCAL_MODEL_2050_85_CESM1_project",  ### 
#    EM.output = myBiomodEMLOCAL,
#    do.stack = FALSE,
#    output.format = ".img",
#    total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#    binary.meth = "ROC") 
#
#  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  gc(reset=T)
#  
#  ### BIOMOD PROJECTIONS
#  myBiomodP4 <- BIOMOD_Projection(modeling.output=myBiomodModelOutLOCAL,
#                                  new.env = myexpl.var30_2050_45_CMCC,
#                                  proj.name="_LOCAL_MODEL_2050_45_CMCC_project",
#                                  #selected.models="all",
#                                  build.clamping.mask=T,
#                                  output.format = ".img",
#                                  binary.meth="ROC") ## here ROC WORKs
#  
#  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  #gc(reset=T)
#  
#  
#  #myexpl.var30_2050_45_CMCC 
#  myBiomodEF4 <- BIOMOD_EnsembleForecasting(
#    projection.output = myBiomodP4,
#    #new.env = myexpl.var30_2050_45_CMCC,   ### questi due non servono se faccio tutti i modelli
#    proj.name = "EM_LOCAL_MODEL_2050_45_CMCC_project",  ### 
#    EM.output = myBiomodEMLOCAL,
#    do.stack = FALSE,
#    output.format = ".img",
#    total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#    binary.meth = "ROC") 
#
#  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  gc(reset=T)
#
#
#  
#  ### BIOMOD PROJECTIONS
#  myBiomodP5 <- BIOMOD_Projection(modeling.output=myBiomodModelOutLOCAL,
#                                  new.env = myexpl.var30_2050_85_CMCC,
#                                  proj.name="_LOCAL_MODEL_2050_85_CMCC_project",
#                                  #selected.models="all",
#                                  build.clamping.mask=T,
#                                  output.format = ".img",
#                                  binary.meth="ROC") ## here ROC WORKs
#  
#  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  #gc(reset=T)
#  
#  
#  #myexpl.var30_2050_85_CMCC 
#  myBiomodEF5 <- BIOMOD_EnsembleForecasting(
#    projection.output = myBiomodP5,
#    #new.env = myexpl.var30_2050_85_CMCC,   ### questi due non servono se faccio tutti i modelli
#    proj.name = "EM_LOCAL_MODEL_2050_85_CMCC_project",  ### 
#    EM.output = myBiomodEMLOCAL,
#    do.stack = FALSE,
#    output.format = ".img",
#    total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#    binary.meth = "ROC") 
#  
#  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  gc(reset=T)
##  
#  ### BIOMOD PROJECTIONS 2070
#  myBiomodP6 <- BIOMOD_Projection(modeling.output=myBiomodModelOutLOCAL,
#                                  new.env = myexpl.var30_2070_45_CESM1,
#                                  proj.name="_LOCAL_MODEL_2070_45_CESM1_project",
#                                  #selected.models="all",
#                                  build.clamping.mask=T,
#                                  output.format = ".img",
#                                  binary.meth="ROC") ## here ROC WORKs
#  
#  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  #gc(reset=T)
#  
#  #myexpl.var30_2070_45_CESM1
#  myBiomodEF6 <- BIOMOD_EnsembleForecasting(
#    projection.output = myBiomodP6,
#    #new.env = myexpl.var30_2070_45_CESM1,   ### questi due non servono se faccio tutti i modelli
#    proj.name = "EM_LOCAL_MODEL_2070_45_CESM1_project",  ### 
#    EM.output = myBiomodEMLOCAL,
#    do.stack = FALSE,
#    output.format = ".img",
#    total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#    binary.meth = "ROC") 
#  
#  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  gc(reset=T)
#  
#  
#  ### BIOMOD PROJECTIONS
#  myBiomodP7 <- BIOMOD_Projection(modeling.output=myBiomodModelOutLOCAL,
#                                  new.env = myexpl.var30_2070_85_CESM1,
#                                  proj.name="_LOCAL_MODEL_2070_85_CESM1_project",
#                                  #selected.models="all",
#                                  build.clamping.mask=T,
#                                  output.format = ".img",
#                                  binary.meth="ROC") ## here ROC WORKs
#  
#  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  #gc(reset=T)
#  
#  
#  #myexpl.var30_2070_85_CESM1
#  myBiomodEF7 <- BIOMOD_EnsembleForecasting(
#    projection.output = myBiomodP7,
#    #new.env = myexpl.var30_2070_85_CESM1,   ### questi due non servono se faccio tutti i modelli
#    proj.name = "EM_LOCAL_MODEL_2070_85_CESM1_project",  ### 
#    EM.output = myBiomodEMLOCAL,
#    do.stack = FALSE,
#    output.format = ".img",
#    total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#    binary.meth = "ROC") 
#  
#
#  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  gc(reset=T)
#  
#  ### BIOMOD PROJECTIONS
#  myBiomodP8 <- BIOMOD_Projection(modeling.output=myBiomodModelOutLOCAL,
#                                  new.env = myexpl.var30_2070_45_CMCC,
#                                  proj.name="_LOCAL_MODEL_2070_45_CMCC_project",
#                                  #selected.models="all",
#                                  build.clamping.mask=T,
#                                  output.format = ".img",
#                                  binary.meth="ROC") ## here ROC WORKs
#  
#  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  #gc(reset=T)
#  
#  
#  #myexpl.var30_2070_45_CMCC 
#  myBiomodEF8 <- BIOMOD_EnsembleForecasting(
#    projection.output = myBiomodP8,
#    #new.env = myexpl.var30_2070_45_CMCC,   ### questi due non servono se faccio tutti i modelli
#    proj.name = "EM_LOCAL_MODEL_2070_45_CMCC_project",  ### 
#    EM.output = myBiomodEMLOCAL,
#    do.stack = FALSE,
#    output.format = ".img",
#    total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#    binary.meth = "ROC") 
#
#
#  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  gc(reset=T)
#  
#  
#  ### BIOMOD PROJECTIONS
#  myBiomodP9 <- BIOMOD_Projection(modeling.output=myBiomodModelOutLOCAL,
#                                  new.env = myexpl.var30_2070_85_CMCC,
#                                  proj.name="_LOCAL_MODEL_2070_85_CMCC_project",
#                                  #selected.models="all",
#                                  build.clamping.mask=T,
#                                  output.format = ".img",
#                                  binary.meth="ROC") ## here ROC WORKs
#  
#  #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  #gc(reset=T)
#  
#  
#  #myexpl.var30_2070_85_CMCC 
#  myBiomodEF9 <- BIOMOD_EnsembleForecasting(
#    projection.output = myBiomodP9,
#    #new.env = myexpl.var30_2070_85_CMCC,   ### questi due non servono se faccio tutti i modelli
#    proj.name = "EM_LOCAL_MODEL_2070_85_CMCC_project",  ### 
#    EM.output = myBiomodEMLOCAL,
#    do.stack = FALSE,
#    output.format = ".img",
#    total.consensus=TRUE,#### può essere utile fare una prova con questo a T per vedere se fa il TOTAL_consensus
#    binary.meth = "ROC") 
#
#
#  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
#  gc(reset=T)
#  
#  
#  ####### PERFORMING THE MESS ANALYSES 
#  ## LA MESS, SEPPUR OTTIMA COME ANALISI ESPLORATIVA, NON MI DICE QUANTE VARIABILI AMBIENTALI ESTRAPOLANO, NE QUALI
#  # PUO' ESSERE CHE UNA VARIABILE MI ESTRAPOLI, MA IO NON SO QUALE E' O CHE L'ESTRAPOLAZIONE VENGA RIDOTTA CONSIDERANDO L'ANALISI MULTIVARIATA
#  ## PRESENT
#  #myexpl.var30_ST_DEF
#  #LOCAL_MESS.myexpl.var30_ST_DEF<- mess(myexpl.var30_ST_DEF , myBiomodDataLOCAL@data.env.var)
#  ### LOCAL_MESS.myexpl.var30_ST_DEF 
#  #a <- sampleRandom(LOCAL_MESS.myexpl.var30_ST_DEF, size = 1000000)
#  #LOCAL_MESS.myexpl.var30_ST_DEF_percentage <- length(a[a<0])/length(a)
#  #capture.output(LOCAL_MESS.myexpl.var30_ST_DEF_percentage, file=file.path(myRespName, paste(myRespName, "LOCAL_MESS-%_of.negative_values_LOCAL_MESS.CURRENT_conditions.txt", sep = "_")))
#  #writeRaster(LOCAL_MESS.myexpl.var30_ST_DEF, filename =file.path(myRespName,paste(myRespName,"LOCAL_MESS.CURRENT_conditions.tif", sep="_")), options="INTERLEAVE=BAND", overwrite=TRUE)  ## here i am saving the all dataset cropped
  
  
  ## FUTURE
  #LOCAL_MESS.myexpl.var30_2050_45_CESM1<- mess(myexpl.var30_2050_45_CESM1 , myBiomodDataLOCAL@data.env.var)
  #LOCAL_MESS.myexpl.var30_2050_85_CESM1<- mess(myexpl.var30_2050_85_CESM1 , myBiomodDataLOCAL@data.env.var)
  #LOCAL_MESS.myexpl.var30_2050_45_CMCC <- mess(myexpl.var30_2050_45_CMCC  , myBiomodDataLOCAL@data.env.var)
  #LOCAL_MESS.myexpl.var30_2050_85_CMCC <- mess(myexpl.var30_2050_85_CMCC  , myBiomodDataLOCAL@data.env.var)
  #LOCAL_MESS.myexpl.var30_2070_45_CESM1<- mess(myexpl.var30_2070_45_CESM1 , myBiomodDataLOCAL@data.env.var)
  #LOCAL_MESS.myexpl.var30_2070_85_CESM1<- mess(myexpl.var30_2070_85_CESM1 , myBiomodDataLOCAL@data.env.var)
  #LOCAL_MESS.myexpl.var30_2070_45_CMCC <- mess(myexpl.var30_2070_45_CMCC  , myBiomodDataLOCAL@data.env.var)
  #LOCAL_MESS.myexpl.var30_2070_85_CMCC <- mess(myexpl.var30_2070_85_CMCC  , myBiomodDataLOCAL@data.env.var)
  #
  ### SAVING THE DATA
  #### Evaluating MESS analyses i can save: a raster or the percentage of negative values
 ### LOCAL_MESS.myexpl.var30_2050_45_CESM1 
  #a <- sampleRandom(LOCAL_MESS.myexpl.var30_2050_45_CESM1, size = 1000000)
  #LOCAL_MESS.myexpl.var30_2050_45_CESM1_percentage <- length(a[a<0])/length(a)
  #capture.output(LOCAL_MESS.myexpl.var30_2050_45_CESM1_percentage, file=file.path(myRespName, paste(myRespName, "LOCAL_MESS-%_of.negative_values_LOCAL_MESS.myexpl.var30_2050_45_CESM1.txt", sep = "_")))
  #writeRaster(LOCAL_MESS.myexpl.var30_2050_45_CESM1, filename =file.path(myRespName,paste(myRespName,"LOCAL_MESS.myexpl.var30_2050_45_CESM1.tif", sep="_")), options="INTERLEAVE=BAND", overwrite=TRUE)  ## here i am saving the all dataset cropped
  #
#
  ## LOCAL_MESS.myexpl.var30_2050_85_CESM1
  #a <- sampleRandom(LOCAL_MESS.myexpl.var30_2050_85_CESM1, size = 1000000)
  #LOCAL_MESS.myexpl.var30_2050_85_CESM1_percentage <- length(a[a<0])/length(a)
  #capture.output(LOCAL_MESS.myexpl.var30_2050_85_CESM1_percentage, file=file.path(myRespName, paste(myRespName, "LOCAL_MESS-%_of.negative_values_LOCAL_MESS.myexpl.var30_2050_85_CESM1.txt", sep = "_")))
  #writeRaster(LOCAL_MESS.myexpl.var30_2050_85_CESM1, filename =file.path(myRespName,paste(myRespName,"LOCAL_MESS.myexpl.var30_2050_85_CESM1.tif", sep="_")), options="INTERLEAVE=BAND", overwrite=TRUE)  ## here i am saving the all dataset cropped
  #
  ## LOCAL_MESS.myexpl.var30_2050_45_CMCC
  #a <- sampleRandom(LOCAL_MESS.myexpl.var30_2050_45_CMCC, size = 1000000)
  #LOCAL_MESS.myexpl.var30_2050_45_CMCC_percentage <- length(a[a<0])/length(a)
  #capture.output(LOCAL_MESS.myexpl.var30_2050_45_CMCC_percentage, file=file.path(myRespName, paste(myRespName, "LOCAL_MESS-%_of.negative_values_LOCAL_MESS.myexpl.var30_2050_45_CMCC.txt", sep = "_")))
  #writeRaster(LOCAL_MESS.myexpl.var30_2050_45_CMCC, filename =file.path(myRespName,paste(myRespName,"LOCAL_MESS.myexpl.var30_2050_45_CMCC.tif", sep="_")), options="INTERLEAVE=BAND", overwrite=TRUE)  ## here i am saving the all dataset cropped
  #
  ## LOCAL_MESS.myexpl.var30_2050_85_CMCC 
  #a <- sampleRandom(LOCAL_MESS.myexpl.var30_2050_85_CMCC, size = 1000000)
  #LOCAL_MESS.myexpl.var30_2050_85_CMCC_percentage <- length(a[a<0])/length(a)
  #capture.output(LOCAL_MESS.myexpl.var30_2050_85_CMCC_percentage, file=file.path(myRespName, paste(myRespName, "LOCAL_MESS-%_of.negative_values_LOCAL_MESS.myexpl.var30_2050_85_CMCC.txt", sep = "_")))
  #writeRaster(LOCAL_MESS.myexpl.var30_2050_85_CMCC, filename =file.path(myRespName,paste(myRespName,"LOCAL_MESS.myexpl.var30_2050_85_CMCC.tif", sep="_")), options="INTERLEAVE=BAND", overwrite=TRUE)  ## here i am saving the all dataset cropped
  #
  ## LOCAL_MESS.myexpl.var30_2070_45_CESM1
  #a <- sampleRandom(LOCAL_MESS.myexpl.var30_2070_45_CESM1, size = 1000000)
  #LOCAL_MESS.myexpl.var30_2070_45_CESM1_percentage <- length(a[a<0])/length(a)
  #capture.output(LOCAL_MESS.myexpl.var30_2070_45_CESM1_percentage, file=file.path(myRespName, paste(myRespName, "LOCAL_MESS-%_of.negative_values_LOCAL_MESS.myexpl.var30_2070_45_CESM1.txt", sep = "_")))
  #writeRaster(LOCAL_MESS.myexpl.var30_2070_45_CESM1, filename =file.path(myRespName,paste(myRespName,"LOCAL_MESS.myexpl.var30_2070_45_CESM1.tif", sep="_")), options="INTERLEAVE=BAND", overwrite=TRUE)  ## here i am saving the all dataset cropped
  #
  ## LOCAL_MESS.myexpl.var30_2070_85_CESM1
  #a <- sampleRandom(LOCAL_MESS.myexpl.var30_2070_85_CESM1, size = 1000000)
  #LOCAL_MESS.myexpl.var30_2050_45_CESM1_percentage <- length(a[a<0])/length(a)
  #capture.output(LOCAL_MESS.myexpl.var30_2050_45_CESM1_percentage, file=file.path(myRespName, paste(myRespName, "LOCAL_MESS-%_of.negative_values_LOCAL_MESS.myexpl.var30_2070_85_CESM1.txt", sep = "_")))
  #writeRaster(LOCAL_MESS.myexpl.var30_2070_85_CESM1, filename =file.path(myRespName,paste(myRespName,"LOCAL_MESS.myexpl.var30_2070_85_CESM1.tif", sep="_")), options="INTERLEAVE=BAND", overwrite=TRUE)  ## here i am saving the all dataset cropped
  #
  ## LOCAL_MESS.myexpl.var30_2070_45_CMCC 
  #a <- sampleRandom(LOCAL_MESS.myexpl.var30_2070_45_CMCC, size = 1000000)
  #LOCAL_MESS.myexpl.var30_2070_45_CMCC_percentage <- length(a[a<0])/length(a)
  #capture.output(LOCAL_MESS.myexpl.var30_2070_45_CMCC_percentage, file=file.path(myRespName, paste(myRespName, "LOCAL_MESS-%_of.negative_values_LOCAL_MESS.myexpl.var30_2070_45_CMCC.txt", sep = "_")))
  #writeRaster(LOCAL_MESS.myexpl.var30_2070_45_CMCC, filename =file.path(myRespName,paste(myRespName,"LOCAL_MESS.myexpl.var30_2070_45_CMCC.tif", sep="_")), options="INTERLEAVE=BAND", overwrite=TRUE)  ## here i am saving the all dataset cropped
  #
  ## LOCAL_MESS.myexpl.var30_2070_85_CMCC 
  #a <- sampleRandom(LOCAL_MESS.myexpl.var30_2070_85_CMCC, size = 1000000)
  #LOCAL_MESS.myexpl.var30_2070_85_CMCC_percentage <- length(a[a<0])/length(a)
  #capture.output(LOCAL_MESS.myexpl.var30_2070_85_CMCC_percentage, file=file.path(myRespName, paste(myRespName, "LOCAL_MESS-%_of.negative_values_LOCAL_MESS.myexpl.var30_2070_85_CMCC.txt", sep = "_")))
  #writeRaster(LOCAL_MESS.myexpl.var30_2070_85_CMCC, filename =file.path(myRespName,paste(myRespName,"LOCAL_MESS.myexpl.var30_2070_85_CMCC.tif", sep="_")), options="INTERLEAVE=BAND", overwrite=TRUE)  ## here i am saving the all dataset cropped
  
  } else{ capture.output(get_evaluations(myBiomodModelOutLOCAL),file=file.path(myRespName, paste(myRespName,"No_INVADEDRANGE_Model_over_ROC_0.txt", sep="_")))
   }
     
  ### saving number of presences
  #capture.output(sum(b[ !is.na(b[,myRespName]) ,myRespName]) ,file=file.path(myRespName, paste(myRespName,"NATIVE_Nr.Pres.txt", sep="_")))
  
  ### saving few images
  ### VAR. IMP
  MyModels_var_import <- get_variables_importance(myBiomodModelOutLOCAL)
  MyModels_var_import
  dimnames(MyModels_var_import)
  
  # make the mean of variable importance by algorithm
  mVarImp <- apply(MyModels_var_import, c(1,2), median) 
  mVarImp <- apply(mVarImp, 2, function(x) x*(1/sum(x))) # standardize the data

  png(filename=file.path(myRespName, paste(myRespName,"INVADEDRANGE_var.imp.png", sep="_")), width = 780, height = 480, pointsize = 12, bg = "white", res = NA)
  #par(mfrow=c(3,3))
  barplot(mVarImp, legend.text=row.names(mVarImp), col=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f'))
  
  dev.off()  
  ### RESPONCE CURVES
  ## uploading the models
  MySpc_glm <- BIOMOD_LoadModels(myBiomodModelOutLOCAL, models='GLM')
  MySpc_gam <- BIOMOD_LoadModels(myBiomodModelOutLOCAL, models='GAM')
  
  ## GLM
  png(filename=file.path(myRespName, paste(myRespName,"INVADEDRANGE_GLM_resp.curv.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  glm_eval_strip <- biomod2::response.plot2(
    models  = MySpc_glm, Data = get_formal_data(myBiomodModelOutLOCAL,'expl.var'),
    show.variables= get_formal_data(myBiomodModelOutLOCAL,'expl.var.names'),
    do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
    display_title = FALSE, data_species = get_formal_data(myBiomodModelOutLOCAL,'resp.var'))
  dev.off()  
  
  ##GAM
  png(filename=file.path(myRespName, paste(myRespName,"INVADEDRANGE_GAM_resp.curv.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  glm_eval_strip <- biomod2::response.plot2(
    models  = MySpc_gam, Data = get_formal_data(myBiomodModelOutLOCAL,'expl.var'),
    show.variables= get_formal_data(myBiomodModelOutLOCAL,'expl.var.names'),
    do.bivariate = FALSE, fixed.var.metric = 'median', legend = FALSE,
    display_title = FALSE, data_species = get_formal_data(myBiomodModelOutLOCAL,'resp.var'))
  dev.off()  
  
  
  ## presences and absences
  b[is.na(b)] = 0 # eliminating NA's
  png(filename=file.path(myRespName, paste(myRespName,"INVADEDRANGE_pres.abs.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
  
  par(mfrow=c(3,3))
  for(i in ly.names.def) plot(b[,myRespName]~ b[,i], data=b, xlab=i, cex=.5)
  dev.off()  
  
  capture.output(sum(b[ !is.na(b[,myRespName]) ,myRespName]) ,file=file.path(myRespName, paste(myRespName,"INVADED_Nr.Pres.txt", sep="_")))
  
  
  cat('completed',myRespName,'_INVADEDRANGE_Model')
  save.image(paste(myRespName,"_INVADEDRANGE.Rdata"))
  
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files
  gc(reset=T)
  #rm(ls())
  rm(list=grep(myRespName,ls(), value=T))
  
  
}






## LOOP for interaction and ENV. vars
# ##for( species in sp.names[c(1,4,17,39,79,90)]) {
# ##  myRespName <-  species
# ##  #myRespName <-  "Ricinus.communis"
# ##  #myRespName <- sp.names[4]
# ##  #load(paste(myRespName,"_LOCAL.Rdata"))
# ##  
# ##  ### combining presences
# ##  all.pres<-rbind(EVAE.70[!is.na(EVAE.70[myRespName]) & EVAE.70[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)],
# ##                  GBIF_grilled_env_EUROPE_70[!is.na(GBIF_grilled_env_EUROPE_70[myRespName]) & GBIF_grilled_env_EUROPE_70[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)],
# ##                  EVAT.30[!is.na(EVAT.30[myRespName]) & EVAT.30[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)],
# ##                  GBIF_grilled_env_EUROPE_EVALUATION[!is.na(GBIF_grilled_env_EUROPE_EVALUATION[myRespName]) & GBIF_grilled_env_EUROPE_EVALUATION[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)]
# ##  )
# ##  # sum(duplicated(all.pres$cells))  ## 3 presences are repeated
# ##  # apply(all.pres, 2, function (x){sum(is.na(x))} )  ## 
# ##  all.pres$Population.density.2000<-log(all.pres$Population.density.2000+1)
# ##  
# ##  
# ##  loc.abs<- Local_BKG[ !Local_BKG$cells %in% intersect(Local_BKG[,"cells"], c(all.pres[ ,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1, "cells" ],EVAT.30[EVAT.30[,myRespName]==1, "cells" ] )   ) ,] ## this is a correction, based on the all presences, not only few of them
# ##  
# ##  loc.abs$Population.density.2000<-log(loc.abs$Population.density.2000+1)
# ##  loc.abs[,myRespName]<- rep(0,dim(loc.abs)[1])
# ##  
# ##   b <- as.data.frame(rbind(all.pres[ ,  colnames(all.pres)],
# ##                           loc.abs[,colnames(all.pres)]
# ##                           #,glob.abs[,colnames(all.pres)]))
# ##  ))
# ##  png(filename=file.path(myRespName, paste(myRespName,"LOCAL_EVA_Vs_GBIF_Preliminary niche analyses overlap.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
# ##  par(mfrow=c(3,3))
# ##  for(i in ly.names.def) {#plot(myRespName~ b[b[,myRespName],i], data=b, xlab=i, cex=.5)
# ##  hist(b[b[,myRespName]==0,i], main=paste(i))
# ##    hist((b[b[,myRespName]==1,i]),col="red", add=T)}
# ##  
# ##  dev.off()    
# ##
# ##}
# ##
# ##
# ##
# ##### another try with the MESS ####
# ##
# ###  ##library(dismo)
# ###  ##
# ###  ###MESS.values<-dismo::mess(glob.buff,LocalEnvEPres[,ly.names.def], full=F)
# ###  ##scenarios <- c(myexpl.var30_2050_45_CESM1,
# ###  ##               myexpl.var30_2050_85_CESM1,
# ###  ##               myexpl.var30_2050_45_CMCC,
# ###  ##               myexpl.var30_2050_85_CMCC,
# ###  ##               myexpl.var30_2070_45_CESM1,
# ###  ##               myexpl.var30_2070_85_CESM1,
#  ##               myexpl.var30_2070_45_CMCC,
#  ##               myexpl.var30_2070_85_CMCC)
#  ##for( species in sp.names[c(1,4,17,39,79,90)]) {
#  ##  myRespName <-  species
#  ##  #myRespName <-  "Ricinus.communis"
#  ##  #myRespName <- sp.names[4]
#  ##  #load(paste(myRespName,"_LOCAL.Rdata"))
#  ##  
#  ##  ### combining presences
#  ##  all.pres<-rbind(EVAE.70[!is.na(EVAE.70[myRespName]) & EVAE.70[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)],
#  ##                  GBIF_grilled_env_EUROPE_70[!is.na(GBIF_grilled_env_EUROPE_70[myRespName]) & GBIF_grilled_env_EUROPE_70[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)],
#  ##                  EVAT.30[!is.na(EVAT.30[myRespName]) & EVAT.30[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)],
#  ##                  GBIF_grilled_env_EUROPE_EVALUATION[!is.na(GBIF_grilled_env_EUROPE_EVALUATION[myRespName]) & GBIF_grilled_env_EUROPE_EVALUATION[myRespName]==1 ,c(myRespName,"Longitude","Latitude","cells",ly.names.def)]
#  ##  )
#  ##  # sum(duplicated(all.pres$cells))  ## 3 presences are repeated
#  ##  # apply(all.pres, 2, function (x){sum(is.na(x))} )  ## 
#  ##  all.pres$Population.density.2000<-log(all.pres$Population.density.2000+1)
#  ##  
#  ##  
#  ##  loc.abs<- Local_BKG[ !Local_BKG$cells %in% intersect(Local_BKG[,"cells"], c(all.pres[ ,"cells"],GBIF_grilled_env_EUROPE_EVALUATION[GBIF_grilled_env_EUROPE_EVALUATION[,myRespName]==1, "cells" ],EVAT.30[EVAT.30[,myRespName]==1, "cells" ] )   ) ,] ## this is a correction, based on the all presences, not only few of them
#  ##  
#  ##  loc.abs$Population.density.2000<-log(loc.abs$Population.density.2000+1)
#  ##  loc.abs[,myRespName]<- rep(0,dim(loc.abs)[1])
#  ##  
#  ##  b <- as.data.frame(rbind(all.pres[ ,  colnames(all.pres)],
#  ##                           loc.abs[,colnames(all.pres)]
#  ##                           #,glob.abs[,colnames(all.pres)]))
#  ##  ))
#  ##  for (scen in scenarios) {
#  ##    MESS.values_50CMCC_45<-dismo::mess(scenarios[[3]],b[,ly.names.def], full=F)
#  ##    plot(MESS.values_50CMCC_45, breaks=c(-10000,0,100000),col=c("red","blue"))
#  ##    
#  ##  }
#  ##  #MESS.values<-dismo::mess(scenarios[[1]],b[,ly.names.def], full=F)
#  ##  
#  ##  png(filename=file.path(myRespName, paste(myRespName,"LOCAL_EVA_Vs_GBIF_Preliminary niche analyses overlap.png", sep="_")), width = 480, height = 480, pointsize = 12, bg = "white", res = NA)
#  ##  #par(mfrow=c(3,3))
#  ##  
#  ##  dev.off()    
#  ##  
#  ##}
#  ##
#  ##
#  ##
#  ##