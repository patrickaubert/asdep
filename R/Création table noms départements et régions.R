# --------------------------------------------------------------------------------------------------------------
# Création base des noms de départements
# --------------------------------------------------------------------------------------------------------------

library(tidyverse)
#library(asdep)

dir <- getwd()
setwd( paste(dir,"/data-raw/",sep=""))

departements <- read.csv2("Liste des departements.csv",header=TRUE,sep=",",stringsAsFactors = FALSE,fileEncoding="utf8")
regions <- read.csv2("Liste des regions.csv",header=TRUE,sep=",",stringsAsFactors = FALSE,fileEncoding="utf8")

setwd( dir )

departementsFR <- departements %>% left_join(regions, by="NumReg")

# ===================================================================================
usethis::use_data(departementsFR,
                  overwrite = T)
