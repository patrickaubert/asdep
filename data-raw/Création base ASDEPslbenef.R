# --------------------------------------------------------------------------------------------------------------
# Création de la base des effectifs de bénéficiaires d'aide sociale, à partir du fichier Excel téléchargé sur data.drees
# --------------------------------------------------------------------------------------------------------------

library(openxlsx)
library(reshape2)
library(plyr)

options(encoding = "utf8")

setwd(paste(getwd(),"/data-raw/",sep=""))

#fichierloc <- "Les bénéficiaires de l’aide sociale départementale - séries longues (1996-2018).xlsx"
fichierloc <- "Les bénéficiaires de l aide sociale départementale - séries longues (1996-2018).xlsx"


# --------------------------------------------------------------------------------------------------------------
# Paramètres généraux et fonctions générales

departements <- read.csv2("Liste des departements.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
syndep <- read.csv2("Synonymes noms départements.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)

listesyn <- as.list(setNames(syndep$Nom.departement, syndep$Synonyme.nom))

CorrigeNom <- function(nomdep){
  if (nomdep %in% names(listesyn)) { return( listesyn[[nomdep]]  )  }
  else { return( nomdep )}
}

CorrigeNumReg <- function(numreg){
  if (numreg<10) { return(100+numreg) } else { return(numreg) }
}

CorrigeNomTerritoire <- function(nom){
  return( trimws(nom, which=c("both")) )
}

# --------------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------------------
# Fonction LitOnglet : lit un onglet du fichier Excel et restitue les données lues sous la forme de tables
# (onglet dans lequel les colonnes correspondent aux années d'observation)

# en input :
# Nom.var : nom que l'on donnera à la variable
# nomfich : nom du fichier (excel) lu
# nomsheet : nom de l'onglet

LitOnglet <- function(Nom.var,
                      nomfich,
                      nomsheet) {

  # pré-identifie la thématique d'après le nom de l'onglet

  if (grepl("pa",nomsheet)) { thematique <- "Perte d'autonomie"}
  else if (grepl("ph",nomsheet)) { thematique <- "Handicap"}
  else if (grepl("ase",nomsheet)) { thematique <- "Aide sociale à l'enfance"}
  else { thematique <- ""}


  # identifie les lignes correspondant à des départements, des régions, ou la France entière, à partir d'une lecture de la premiÃ¨re colonne

  col1 <- read.xlsx(nomfich, sheet = nomsheet, cols= c(1),  colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = TRUE)
  col1$numligne <- seq(1,nrow(col1),1)
  col1 <- col1[!is.na(col1$X1),]

  Source.var <- ""
  Intitule.var <- ""
  Champ.var <- ""
  Note.var <- ""

  i <- 1
  col1[i,c("X1")] <- sub("\n","",col1[i,c("X1")])
  col1[i,c("X1")] <- sub("\r","",col1[i,c("X1")])
  col1[i,c("X1")] <- trimws(col1[i,c("X1")],which="left")
  while ((col1[i,c("X1")] != "Code r\u00E9gion") & (i<nrow(col1))) {
    if (substr(col1[i,1],1,7) == "Tableau") {
      Intitule.var <- gsub("\\,"," ",col1[i,1])
      Intitule.var <- sub("Tableau [0-9a-z]+ .","",Intitule.var)
      Intitule.var <- sub(" de [0-9]+ à 2[0-9]+","",Intitule.var)
      Intitule.var <- sub("\\*","",Intitule.var)
      Intitule.var <- sub(", par d\u00E9partement","",Intitule.var)
      Intitule.var <- sub(" par d\u00E9partement","",Intitule.var)
      Intitule.var <- sub(",","",Intitule.var)
      Intitule.var <- sub("[dD]onn\u00E9es au 31 d\u00E9cembre","",Intitule.var)
      Intitule.var <- sub("[dD]onn\u00E9es en d\u00E9cembre","",Intitule.var)
      Intitule.var <- sub(" au 31 d\u00E9cembre [0-9][0-9][0-9][0-9]","",Intitule.var)
      Intitule.var <- sub(" au 31 d\u00E9cembre","",Intitule.var)
      Intitule.var <- trimws(Intitule.var,which="left")
    }

    if (substr(col1[i,1],1,6) == "Source") {   Source.var <- trimws(sub("[sS]ources? [:-] ","",col1[i,1]), which="left") }
    if (substr(col1[i,1],1,5) == "Champ") {   Champ.var <- trimws(sub("[cC]hamp [:-] ","",col1[i,1]), which="left") }
    if (substr(col1[i,1],1,4) == "Note") {   Note.var <- trimws(sub("[nN]otes? [:-]","",col1[i,1]), which="left") }
    if (grepl("[nN]otes? [:-]",Champ.var)) {
      ch <- strsplit(Champ.var,"[nN]otes? [:-]")
      Champ.var <- trimws(ch[[1]][1], which="both")
      Note.var <- paste(trimws( ch[[1]][2] , which="both"),
                        Note.var,
                        sep="")
    }

    i <- i+1
    col1[i,c("X1")] <- sub("\n","",col1[i,c("X1")])
    col1[i,c("X1")] <- sub("\r","",col1[i,c("X1")])
    col1[i,c("X1")] <- trimws(col1[i,c("X1")],which="left")
  }
  rowdep <- c(  )
  while ((tolower(substr(col1[i,1],1,5)) != "total") & (i<nrow(col1))) {
    rowdep <- c( rowdep, c( col1[i,c("numligne")] ) )
    i <- i+1
    col1[i,c("X1")] <- sub("\n","",col1[i,c("X1")])
    col1[i,c("X1")] <- sub("\r","",col1[i,c("X1")])
  }

  rowfrance <- c( )
  while ((col1[i,c("X1")] != "Code r\u00E9gion") & (i<nrow(col1))) {
    if (tolower(substr(col1[i,1],1,5)) == "total") {   rowfrance <- c( rowfrance, c( col1[i,c("numligne")] ) ) }
    i <- i+1
    col1[i,c("X1")] <- sub("\n","",col1[i,c("X1")])
    col1[i,c("X1")] <- sub("\r","",col1[i,c("X1")])
  }

  rowregion <- c( )
  while (i<nrow(col1)) {
    if (tolower(substr(col1[i,1],1,5)) != "total") {   rowregion <- c( rowregion, c( col1[i,c("numligne")] ) ) }
    i <- i+1
    col1[i,c("X1")] <- sub("\n","",col1[i,c("X1")])
    col1[i,c("X1")] <- sub("\r","",col1[i,c("X1")])
  }


  # extrait les données de l'onglet du fichier Excel

  tab.deb. <- read.xlsx(nomfich, sheet = nomsheet,
                        rows = rowdep, colNames = TRUE, rowNames = FALSE, na.strings = "NA"                 )
  if ("Départements" %in% names(tab.deb.)) { tab.deb. <- plyr::rename(tab.deb., c("D\u00E9partements"="D\u00E9partement") ) }
  tab.deb <- melt(tab.deb.,id=c("Code.r\u00E9gion" , "Code.d\u00E9partement","D\u00E9partement"))
  tab.deb <- tab.deb[!is.na(tab.deb$value),]
  tab.deb <- tab.deb[(tab.deb$value != "-"),]
  tab.deb <- merge(subset(tab.deb, select = -c(Département)),
                   plyr::rename(departements[,c("NumDept","Departement")], c("Departement"="Territoire")),
                   by.x="Code.d\u00E9partement", by.y="NumDept", all.x=TRUE, all.y=FALSE)
  tab.deb$TypeTerritoire <- rep("D\u00E9partement",nrow(tab.deb))
  #tab.deb$Territoire <- sapply( tab.deb$Département, CorrigeNom)
  tab.deb$Code.région <- sapply( as.numeric(as.character(tab.deb$Code.région)), CorrigeNumReg)
  tab.deb$Annee <- tab.deb$variable
  tab.deb[,c(Nom.var)] <- tab.deb$value

  tab.reg. <- read.xlsx(nomfich, sheet = nomsheet,
                        rows = rowregion, colNames = TRUE, rowNames = FALSE, na.strings = "NA" )
  if ("R\u00E9gions" %in% names(tab.reg.)) { tab.reg. <- plyr::rename(tab.reg., c("R\u00E9gions"="R\u00E9gion") ) }
  tab.reg <- melt(tab.reg.,id=c("Code.r\u00E9gion","R\u00E9gion" ))
  tab.reg$Code.région <- sapply( as.numeric(as.character(tab.reg$Code.région)), CorrigeNumReg)
  tab.reg$TypeTerritoire <- rep("R\u00E9gion",nrow(tab.reg))
  tab.reg$Code.département <- rep("",nrow(tab.reg))
  tab.reg$Territoire <- tab.reg$Région
  tab.reg$Annee <- tab.reg$variable
  tab.reg[,c(Nom.var)] <- tab.reg$value

  tab.france. <- read.xlsx(nomfich, sheet = nomsheet,
                           rows = rowfrance, colNames = FALSE, rowNames = FALSE, na.strings = "NA"   )
  #names(tab.france.) <- c("Territoire", c(  as.character(unique(tab.deb$Annee))  ) )
  names(tab.france.) <- c("Territoire", c(  names(tab.deb.)[4:NROW(names(tab.deb.))]  ) )
  tab.france <- melt(tab.france.,id=c("Territoire"))
  tab.france <- tab.france[!is.na(tab.france$value),]
  tab.france$variable <- as.character(tab.france$variable)
  tab.france <- tab.france[!is.na(tab.france$variable),]
  tab.france$TypeTerritoire <- rep("France",nrow(tab.france))
  tab.france$Code.département <- rep("",nrow(tab.france))
  tab.france$Code.région <- rep("",nrow(tab.france))
  tab.france$Annee <- tab.france$variable
  tab.france[,c(Nom.var)] <- tab.france$value

  # concatene et restitue les outputs

  tab <- rbind( tab.deb[,c("Annee","Code.r\u00E9gion","Code.d\u00E9partement","TypeTerritoire","Territoire", Nom.var)],
                tab.reg[,c("Annee","Code.r\u00E9gion","Code.d\u00E9partement","TypeTerritoire","Territoire", Nom.var)],
                tab.france[,c("Annee","Code.r\u00E9gion","Code.d\u00E9partement","TypeTerritoire","Territoire", Nom.var)]            )

  tab[,c(Nom.var)] <- as.numeric(tab[,c(Nom.var)])
  tab$Annee <- as.numeric(as.character(tab$Annee))
  tab$Territoire <- sapply( tab$Territoire, CorrigeNomTerritoire)

  infovar <- data.frame(Nom.var = c(Nom.var),
                        Intitule.var = c(Intitule.var),
                        Intitulecourt.var = c(""),
                        Source.var = c(Source.var),
                        Champ.var = c(Champ.var),
                        Note.var = c(Note.var),
                        Unite.var = c(""),
                        Thematique.var = thematique,
                        TexteDenom = c(""),
                        ListeDenom.var = c(""),
                        ListeComposante.var = c("")
  )
  infovar[] <- lapply(infovar, as.character)

  return( list(tab = tab, infovar = infovar))

}

# fin de la fonction LitOnglet
# --------------------------------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------------------------------
# fonction ImputeZeros : ajoute des observations avec des valeurs à 0 pour une variable donnée
# (pour éviter d'avoir des erreurs dues à des valeurs manquantes pour des variables qui n'existent pas ou plus certaines années)

ImputeZeros <- function(donneesloc, varloc, anmin, anmax) {

  tabterritoires <- merge( unique(donneesloc[,c("Territoire","TypeTerritoire","Code.région","Code.département")]), data.frame(Annee=c(anmin:anmax)) )

  tabout <- donneesloc[(!( (donneesloc$Annee >= anmin) & (donneesloc$Annee <= anmax) )),]
  tabin  <- merge( donneesloc[( (donneesloc$Annee >= anmin) & (donneesloc$Annee <= anmax) ),],
                   tabterritoires,
                   by = c("Territoire","TypeTerritoire","Code.région","Code.département","Annee"),
                   all=TRUE )
  tabin[is.na(tabin[,varloc]),varloc] <- 0

  return( rbind(tabout,
                tabin[,names(tabout)]))

}

# fin de la fonction ImputeZeros
# --------------------------------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------------------------------
# extraction des données

# lit en boucle tous les onglets du fichiers "séries longues"

infos.onglets <- read.xlsx("Contenu fichiers excel.xlsx",
                           sheet = "SL_benef_2018",
                           colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = TRUE)

#  --- lecture des onglets un par un

for (i in (1:nrow(infos.onglets))) {
  lit <- LitOnglet(Nom.var = infos.onglets[i,"Nom.var"],
                   nomfich = fichierloc,
                   nomsheet = infos.onglets[i,"NoOngletExcel"])
  if (i == 1) {
    BenefAidessociales <- lit$tab
  } else {
    BenefAidessociales <- merge(BenefAidessociales, lit$tab, by=c("Annee","Code.région","Code.département","TypeTerritoire","Territoire"), all.x=TRUE, all.y=TRUE)

  }
  infovar <- lit$infovar
  if (!is.na(infos.onglets[i,"Intitulecourt.var"])) { infovar$Intitulecourt.var <- infos.onglets[i,"Intitulecourt.var"] }
  if (!is.na(infos.onglets[i,"ListeDenom.var"])) { infovar$ListeDenom.var <- infos.onglets[i,"ListeDenom.var"] }
  if (!is.na(infos.onglets[i,"TexteDenom"])) { infovar$TexteDenom <- infos.onglets[i,"TexteDenom"] }
  if (!is.na(infos.onglets[i,"ListeComposante.var"])) { infovar$ListeComposante.var <- infos.onglets[i,"ListeComposante.var"] }
  if (i == 1) {
    varbenef <- infovar
  } else {
    varbenef <- rbind( varbenef, infovar)
  }
}


#  --- ajout information

varbenef$Type.var <- c( rep("Nombres de bénéficiaires", nrow(varbenef) ) )

varbenef$Unite.var <- c( rep("personnes",  nrow(varbenef)) )


#  --- ajout de variables

complete <- function(tabloc, nom, nomnewdenom) {
  # on repère n° de ligne avec nom recherché
  vartab <- names(tabloc)
  tabloc$no <- c(1:nrow(tabloc))
  nligne <- tabloc[(tabloc$Nom.var == nom),"no" ]
  # on complète le tableau
  if (tabloc[nligne,"ListeDenom.var" ] %in% c("",NA)) { tabloc[nligne,"ListeDenom.var" ] <- nomnewdenom}
  else { { tabloc[nligne,"ListeDenom.var" ] <- paste(tabloc[nligne,"ListeDenom.var" ],nomnewdenom,sep="_")}  }
  return( tabloc[,vartab] )
}

# bénéficiaire de l'APA ou de la PSD
BenefAidessociales$NbBenefAPAPSD <- rowSums(BenefAidessociales[,c("NbBenefAPA","NbBenefPSD")], na.rm=TRUE)

intituleAPAASD <- data.frame(Nom.var= "NbBenefAPAPSD",
                             Intitule.var = "Nombre de bénéficiaires de l'APA ou de la PSD",
                             Intitulecourt.var = "APA ou PSD",
                             Source.var="DREES, Enquêtes Aide sociale",
                             Champ.var="France métropolitaine et DROM (Hors Mayotte)",
                             Note.var="",
                             Thematique.var="Perte d'autonomie",
                             Type.var="Nombres de bénéficiaires",
                             Unite.var="personnes",
                             TexteDenom = "prestations d'APA ou de PSD",
                             ListeDenom.var = c(""),
                             ListeComposante.var = c("NbBenefAPA_NbBenefPSD"))

varbenef <- rbind( varbenef,
                   intituleAPAASD[,colnames(varbenef)])

# bénéficiaires ACTP ou PCH
BenefAidessociales$TotBenefACTPPCH <- rowSums(BenefAidessociales[,c("NbBenefACTP","NbBenefPCH")], na.rm=TRUE)

intituleACTPPCH <- data.frame(Nom.var= "TotBenefACTPPCH",
                              Intitule.var = "Nombre de bénéficiaires de la PCH ou de l'ACTP",
                              Intitulecourt.var = "PCH ou ACTP",
                              Source.var="DREES, Enquêtes Aide sociale",
                              Champ.var="France métropolitaine et DROM (Hors Mayotte)",
                              Note.var="",
                              Thematique.var="Handicap",
                              Type.var="Nombres de bénéficiaires",
                              Unite.var="personnes",
                              TexteDenom = "droits ouverts d'ACTP ou de PCH",
                              ListeDenom.var = c(""),
                              ListeComposante.var = c("NbBenefACTP_NbBenefPCH"))
varbenef <- rbind( varbenef,
                   intituleACTPPCH[,colnames(varbenef)])
#varbenef <- complete(varbenef,"NbBenefACTP","TotBenefACTPPCH")
#varbenef <- complete(varbenef,"NbBenefPCH","TotBenefACTPPCH")


#  --- complétude des variables ListeDenom.var à partir des variables ListeComposante.var
rownames(varbenef) <- varbenef$Nom.var
for (i in (1:nrow(varbenef))) {
  if (!(varbenef[i,"ListeComposante.var"]) %in% c("", NA)) {
    composantes <- as.vector(unlist(strsplit(varbenef[i,"ListeComposante.var"],split="_|\\s")))
    nomdenom <- as.character(varbenef[i,"Nom.var"])
    for (j in (1:NROW(composantes))) {
      if (!(grepl(nomdenom,varbenef[composantes[j],"ListeDenom.var"]))) {
        if (varbenef[composantes[j],"ListeDenom.var"] %in% c(NA,"")) { varbenef[composantes[j],"ListeDenom.var"] <- nomdenom }
        else { varbenef[composantes[j],"ListeDenom.var"] <- paste(varbenef[composantes[j],"ListeDenom.var"],nomdenom,sep="_") }
      }
    }
  }
}

#  --- corrections sur certaines variables

# pour les prestations qui n'existent pas certaines années, on remplace les valeurs manquantes par des 0 :

# APA avant 2002
BenefAidessociales[(BenefAidessociales$Annee<2002),c("NbBenefAPA","NbBenefAPADomicile","NbBenefAPAEtab")][is.na(BenefAidessociales[(BenefAidessociales$Annee<2002),c("NbBenefAPA","NbBenefAPADomicile","NbBenefAPAEtab")])] <- 0
# PSD après 2002
BenefAidessociales[(BenefAidessociales$Annee>=2002),c("NbBenefPSD","NbBenefPSDDomicile","NbBenefPSDEtab")][is.na(BenefAidessociales[(BenefAidessociales$Annee>=2002),c("NbBenefPSD","NbBenefPSDDomicile","NbBenefPSDEtab")])] <- 0
# PCH avant 2007
BenefAidessociales[(BenefAidessociales$Annee<2007),c("NbBenefPCH")][is.na(BenefAidessociales[(BenefAidessociales$Annee<2007),c("NbBenefPCH")])] <- 0

# --- ajout des populations de référence (pour la ratio par habitant) pour chaque variable, quand elles ne sont pas dans les données)

fPopref <- function(them){
  if (them == "Perte d'autonomie") {return("60-99")}
  else if (them == "Handicap") {return("20-64")}
  else if (them == "Aide sociale à l'enfance") {return("00-20")}
  else if (them == "Insertion") {return("20-64")}
  else(return("popTOT"))
}
varbenef$Popref.var <- sapply(varbenef$Thematique.var, fPopref)


#  --- reste à faire

# ajouter automatiquement les dénominateurs de niveau 2, en lisant les dénominateurs des dénominateurs



#  --- suppression des caractères qui posent problèmes

BenefAidessociales <- plyr::rename(BenefAidessociales, c("Code.région"="Code.region", "Code.département"="Code.departement"))

BenefAidessociales$Annee <- as.character(BenefAidessociales$Annee)
#BenefAidessociales <- BenefAidessociales[order(-BenefAidessociales$Annee),]

#  --- sauvegarde les tables constituées

ASDEPslbenef <- BenefAidessociales
ASDEPslbenef_description <- varbenef

# ===================================================================================
usethis::use_data(ASDEPslbenef,
                  ASDEPslbenef_description,
                  overwrite = T)
