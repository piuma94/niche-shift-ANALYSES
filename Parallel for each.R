

setwd("/Volumes/Extreme SSD/DEF_models")


## Setting the environment ###
## for windows
memory.limit(size=76000)
## uploading some libraries
library(dplyr)

library(biomod2)
library(raster)
library(dismo)
library(ecospat)
library(sf)

#rm(list = ls(all.names = TRUE))
#automatic install of packages if they are not installed already
list.of.packages <- c(
  "foreach",
  "doParallel",
  "ranger",
  "palmerpenguins",
  "tidyverse",
  "kableExtra"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}

#loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(
    library(
      package.i, 
      character.only = TRUE
    )
  )
}

#loading example data
data("penguins")


## symple example the default is output as a list
x <- foreach(i = 1:10) %do% {
  sqrt(i)
}
x

### if you want a vector you have to 
x <- foreach(
  i = 1:10, 
  .combine = 'c'
) %do% {
  sqrt(i)
}
x

## i can also use multiple indexes

x <- foreach(
  i = 1:3, 
  j = 1:3, 
  k = 1:3, 
  .combine = 'c'
) %do% {
  i + j + k
}
x




#### but to really work in parallel we should ####
parallel::detectCores() ## how many cores do i have?
## in the mac i have 4 cores
n.cores <- parallel::detectCores() - 1
## less one that is the master

## prepare the clusters
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK" ## or more efficient FORK
)

#check cluster definition (optional)
print(my.cluster)

x <- foreach(
  i = 1:10, 
  .combine = 'c'
) %dopar% {
  sqrt(i)
}
x
## IT IS RECCOMENDED TO STOP THE CLUSTER WHEN DONE
parallel::stopCluster(cl = my.cluster)

### a complex example he said it was faster than doing the job with the imbeeded parallelization
#performing 1000 iterations in parallel
importance.scores <- foreach(
  i = 1:1000, 
  .combine = 'rbind', 
  .packages = "ranger"
) %dopar% {
  
  #fit model
  m.i <- ranger::ranger(
    data = penguins,
    dependent.variable.name = "species",
    importance = "permutation",
    mtry = best.hyperparameters$mtry,
    num.trees = best.hyperparameters$num.trees,
    min.node.size = best.hyperparameters$min.node.size
  )
  
  #format importance
  m.importance.i <- importance_to_df(model = m.i)
  
  #returning output
  return(m.importance.i)
  
}

)

