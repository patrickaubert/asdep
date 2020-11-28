# --------------------------------------------------------------------------------------------------------------
# Création de la base des indicateurs tirés de l'enquête OARSA de la DREES
# --------------------------------------------------------------------------------------------------------------


library(openxlsx)
library(reshape2)
library(plyr)

options(encoding = "utf8")

setwd(paste(getwd(),"/data-raw/",sep=""))

#nomfich <- "OARSA – Principaux indicateurs de 2015 à 2018.xlsx"
#nomsheet <- "Tableau B1"

# --------------------------------------------------------------------------------------------------------------
# Paramètres généraux et fonctions générales

departements <- read.csv2("Liste des departements.csv",header=TRUE,sep=",",stringsAsFactors = FALSE,fileEncoding="utf8")
syndep <- read.csv2("Synonymes noms départements.csv",header=TRUE,sep=",",stringsAsFactors = FALSE,fileEncoding="utf8")

listesyn <- as.list(setNames(syndep$Nom.departement, syndep$Synonyme.nom))

CorrigeNom <- function(nomdep){
  if (nomdep %in% names(listesyn)) { return( listesyn[[nomdep]]  )  }
  else { return( nomdep )}
}

CorrigeNumReg <- function(numreg){
  if (numreg<10) { return(100+numreg) } else { return(numreg) }
}

CorrigeNomTerritoire <- function(nom){
  #return( trimws(CorrigeNom(nom), which=c("both")) )
  return( CorrigeNom(gsub("^[[:space:]]*|[[:space:]]*$","",nom)) )
}

# --------------------------------------------------------------------------------------------------------------

# contenu du fichier Excel

lisheets <- getSheetNames(nomfich)
lisheets <- lisheets[5:NROW(lisheets)]
namesindic <- gsub("^Tableau ","OarsaTab",lisheets)

# boucle d'extraction des données

for (i in 1:NROW(lisheets)) {
  tabs <- LitOngletOarsa(Nom.var=namesindic[i], nomfich=nomfich, nomsheet=lisheets[i], corrigenom=CorrigeNomTerritoire)
  if (i == 1) {
    tabOarsa <- tabs$tab
    descrOarsa <- tabs$infovar
  } else {
    tabOarsa <- full_join(tabOarsa, tabs$tab, by = c("Territoire", "Annee","TypeTerritoire") )
    descrOarsa <- rbind(descrOarsa, tabs$infovar)
  }
}

# vérifications que tous les noms de départements sont corrects

verifnom <- unique(tabOarsa$Territoire)
verifnom[!(verifnom %in% departements$Departement)]

# -------------------------------------------------------------------------------------------------
# sauvegarde les tables constituées

OARSAsl <- tabOarsa
OARSAsl_description <- descrOarsa

# ===================================================================================
usethis::use_data(OARSAsl,
                  OARSAsl_description,
                  overwrite = T)
