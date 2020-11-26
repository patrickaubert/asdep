# --------------------------------------------------------------------------------------------------------------
# Création de la base des indicateurs tirés de l'enquête OARSA de la DREES
# --------------------------------------------------------------------------------------------------------------


library(openxlsx)
library(reshape2)
library(plyr)

options(encoding = "utf8")

setwd(paste(getwd(),"/data-raw/",sep=""))

nomfich <- "OARSA – Principaux indicateurs de 2015 à 2018.xlsx"
nomsheet <- "Tableau B1"

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
  return( trimws(CorrigeNom(nom), which=c("both")) )
}

# --------------------------------------------------------------------------------------------------------------


LitOngletOarsa <- function(Nom.var,
                           nomfich = FichierSource,
                           nomsheet) {

  # valeurs des indicateurs

  vals <- read.xlsx(nomfich, sheet = nomsheet, rows = c(5:109), cols= c(2:6),
                    colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = TRUE)
  colnames(vals)[1] <- "terr"
  valdep <- vals %>%
    mutate(Territoire = gsub("^[[:alnum:]]{2,3}\\-","",terr)) %>%
    select(-terr) %>%
    pivot_longer(cols=-c(Territoire), names_to="Annee",values_to = "Indic" ) %>%
    mutate(Indic = gsub("[[:alpha:]]*","",Indic),
           Indic = as.numeric(Indic),
           TypeTerritoire = "Département")
  names(valdep)[3] <- Nom.var
  valdep$Territoire <- unlist(lapply( valdep$Territoire, CorrigeNomTerritoire))

  # métadonnées

  Intitule.var <- read.xlsx(nomfich, sheet = nomsheet, rows = c(2), cols= c(2), colNames = FALSE)
  Intitule.var <- gsub("^Tableau [[:alnum:]]+ \\- ","",Intitule.var)
  Intitule.var <- gsub("au 31/12 de l'année","",Intitule.var)

  Unite.var <- read.xlsx(nomfich, sheet = nomsheet, rows = c(4), cols= c(2), colNames = FALSE)
  Unite.var <- gsub("^[Ee]n ","",Unite.var)

  infovar <- data.frame(Nom.var = c(Nom.var),
                        Intitule.var = c(Intitule.var),
                        Intitulecourt.var = c(""),
                        Source.var = c("DREES, enquête OARSA"),
                        Champ.var = c("France"),
                        Note.var = c(""),
                        Unite.var = c(Unite.var),
                        Thematique.var = c("Insertion"),
                        TexteDenom = c(""),
                        ListeDenom.var = c(""),
                        ListeComposante.var = c(""),
                        Type.var = c("Parts"),
                        Popref.var = c(""),
                        stringsAsFactors = FALSE
  )

  return( list(tab = valdep, infovar = infovar))

}


lisheets <- getSheetNames(nomfich)
lisheets <- lisheets[5:NROW(lisheets)]
namesindic <- gsub("^Tableau ","OarsaTab",lisheets)

truc <- LitOngletOarsa("Part1", nomfich, nomsheet)
