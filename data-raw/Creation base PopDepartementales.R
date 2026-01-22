# --------------------------------------------------------------------------------------------------------------
# Création de la base des populations départementales, à partir du fichier Excel téléchargé sur le site de l'Insee
# --------------------------------------------------------------------------------------------------------------


library(openxlsx)
library(reshape2)
library(tidyverse)

#options(encoding = "utf8")
#setwd(paste(getwd(),"/data-raw/",sep=""))

# extraction des populations departementales du fichier Excel telecharge sur le site de l'Insee
# paru le 18/01/2022
# téléchargé le 24/06/2022
# source : https://www.insee.fr/fr/statistiques/1893198

nomfich <- "data-raw/estim-pop-dep-sexe-aq-1975-2022.xlsx"
#nomsheet <- "2022"

# fonction d'extraction onglet pour une annee
extronglet <- function(nomfich = nomfich, sheet) {
  val <- read.xlsx(nomfich, sheet = sheet,
                   cols= c(1:23), rows = c(c(6:101),c(103:107)),
                   colNames = FALSE, rowNames = FALSE, na.strings = "NA"   )
  names(val) <- c("Code.departement","Territoire",as.character(seq(0,95,5)),"popTOT")
  val <- val %>%
    filter(!is.na(Code.departement),grepl("^[[:digit:]]",Code.departement)) %>%
    mutate_at(vars(-c("Code.departement","Territoire")),as.numeric)
  val$popASE <- rowSums(val[,c(as.character(seq(0,15,5)))],na.rm=TRUE)
  val$popPH <- rowSums(val[,c(as.character(seq(20,95,5)))],na.rm=TRUE)
  val$popPA <- rowSums(val[,c(as.character(seq(60,95,5)))],na.rm=TRUE)
  val$Annee <- rep((as.numeric(sheet)-1),nrow(val))
  return(val)
}

popdepartementales <- do.call("bind_rows", lapply(1990:2022,function(an){extronglet(nomfich,as.character(an))}))

popdepartementales <- popdepartementales %>%
  mutate(TypeTerritoire = "Département") %>%
  rename_at(vars(c(as.character(seq(0,95,5)))) , function(x){paste("pop",x,as.character(as.numeric(x)+4),sep=".")})

# verification et correction des noms de département

popdepartementales <- popdepartementales %>%
  mutate(Territoire = corrigeNom(Territoire))

verif <- unique(popdepartementales$Territoire)
verif[!(verif %in% asdep::nomscorrectsterritoires$TerritoireCorrect)]

# correction des pop de 95 ans et + manquantes pour les DROM dans les années 1990 (elles ont été agrégées avec les 90-94)

popdepartementales <- popdepartementales %>%
  mutate(pop.95.99 = ifelse(is.na(pop.95.99),0,pop.95.99))

# ajout des régions

departementsFR <- departementsFR %>%
  mutate(NumReg = as.character(NumReg))

popdepartementales <- popdepartementales %>%
  left_join(departementsFR %>%
              select(NumDept,NumReg) %>%
              rename(Code.departement = NumDept,
                     Code.region = NumReg),
            by = "Code.departement")

popregionales <- popdepartementales %>%
  select(-Code.departement,-TypeTerritoire,-Territoire) %>%
  group_by(Code.region,Annee) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  mutate(TypeTerritoire = "Région",
         Code.Departement = NA) %>%
  left_join(departementsFR %>%
              select(NumReg,Region) %>%
              distinct() %>%
              rename(Territoire = Region,
                     Code.region = NumReg),
            by = "Code.region")

# calcul des populations nationales

metropole <- c("01", "02",  "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "2A", "2B",
               "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41" ,
               "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62" ,
               "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83" ,
               "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95")
champ.national <- list(
  "TOTAL estim\u00E9 France m\u00E9tropolitaine" = c(metropole),
  "France m\u00E9tropolitaine" = c(metropole),
  "TOTAL estim\u00E9 DROM (hors Mayotte)" = c( "971", "972", "973", "974"),
  "TOTAL estim\u00E9 France enti\u00E8re (hors Mayotte)" = c(metropole, "971", "972", "973", "974"),
  "TOTAL estim\u00E9 DROM" = c( "971", "972", "973", "974", "976"),
  "TOTAL estim\u00E9 France enti\u00E8re" = c(metropole, "971", "972", "973", "974",  "976"),
  "France" = c(metropole, "971", "972", "973", "974",  "976")
)

popagr <- function(i) {
  popdepartementales %>%
    filter(Code.departement %in% champ.national[[i]]) %>%
    select(-Code.departement,-Code.region,-TypeTerritoire,-Territoire) %>%
    group_by(Annee) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    mutate(Territoire = names(champ.national)[i],
           TypeTerritoire = "France",
           Code.region = NA,
           Code.Departement = NA)
}

popnationales <- do.call("bind_rows", lapply( 1:NROW(champ.national) , popagr))

# agrégation des trois niveaux géographiques

popdepartementales <- bind_rows(
  popdepartementales,
  popregionales,
  popnationales
)

# calcul des agrégats de population pertinents (d'après les variables contenues dans le fichier de données)
# (les populations de références de chaque variable sont sous le format, par exemple, "20-64" pour désigner les 20-64 ans => cette section crée la variable "pop.20.64")

nomspop <- names(popdepartementales)
nomspop <- nomspop[grepl("pop\\.",nomspop)]
varpop <- do.call( c , lapply( nomspop, function(x){ return(as.vector(unlist(strsplit(x, split=".")))) } ))
varpop <- as.data.frame( do.call(rbind  , lapply( nomspop, function(x){ return(as.vector(unlist(strsplit(x, split="\\.")))) } ) ) )
names(varpop) <- c("pop","agemin","agemax")
varpop$agemin <- as.numeric(as.character(varpop$agemin))
varpop$agemax <- as.numeric(as.character(varpop$agemax))
varpop <- cbind(varpop,nomspop)
varpop$nomspop <- as.character(varpop$nomspop)
AgrPop <- function(tab,tranche) {
  if (grepl("[0-9][0-9]\\-[0-9][0-9]",tranche[[1]]))  {
    tr <- as.numeric(as.vector(unlist(strsplit(tranche[[1]],split="-") )))
    if (!(paste("pop",tr[1],tr[2],sep=".") %in% names(tab))) {
      tab[,paste("pop",tr[1],tr[2],sep=".")] <- rowSums(popdepartementales[,c(varpop[( (varpop$agemin>=tr[1]) & (varpop$agemax<=tr[2]) ),"nomspop"])])
    }
  }
  if (NROW(tranche)>1) { return( AgrPop(tab, tranche[2:NROW(tranche)]) )}
  else { return(tab) }
}
tranches.utilisees <- unique( c(ASDEPslbenef_description$Popref.var, ASDEPsldepenses_description$Popref.var ) )
tranches.utilisees <- tranches.utilisees[!is.na(tranches.utilisees)]
popdepartementales <- AgrPop(popdepartementales, tranches.utilisees )

NommeVarpop <- function(x){
  if (grepl("[0-9][0-9]\\-[0-9][0-9]",x))  { return(paste(c("pop",as.numeric(as.vector(unlist(strsplit(x,split="-") )))),collapse=".")) }
  else {  return(x) }
}
noms.pop <- sapply( tranches.utilisees , NommeVarpop)
noms.varpop <- unique(c("popTOT","popASE","popPH","popPA", noms.pop    ))
popdepartementales <- popdepartementales[,c("Code.departement","TypeTerritoire","Territoire","Code.region",noms.varpop,"Annee")]

#Intitulepop <- function(nom){
#  if (grepl("[0-9]\\.[0-9]",nom))  {
#    tr <- as.vector(unlist(strsplit(nom,split="\\.") ))
#    amin <- as.numeric(tr[2])
#    amax <- as.numeric(tr[3])
#    if (amin == 0) { return(paste(" de moins de ",(amax+1)," ans",sep="")) }
#    else if (amax >= 99) { return(paste(" de ",amin," ans et plus",sep="")) }
#    else { return(paste(" de ",amin," à ",amax," ans",sep="")) }
#  }
#  else if (nom == "popTOT") { return("") }
#  else if (nom == "popPH") { return(Intitulepop("pop.20.64")) }
#  else if (nom == "popASE") { return(Intitulepop("pop.00.19")) }
#  else if (nom == "popPA") { return(Intitulepop("pop.60.99")) }
#  else { return("")}
#}

PopDepartementales_description <- data.frame(
  Nom.var=noms.varpop,
  Intitule.var=paste("Population",sapply(noms.varpop, Intitulepop),sep=""),
  Intitulecourt.var=paste("Population",sapply(noms.varpop, Intitulepop),sep=""),
  Source.var=rep("Insee",NROW(noms.varpop)),
  Champ.var=rep("",NROW(noms.varpop)),
  Note.var=rep("",NROW(noms.varpop)),
  TexteDenom=rep("",NROW(noms.varpop)),
  ListeDenom.var=rep("",NROW(noms.varpop)),
  ListeComposante.var=rep("",NROW(noms.varpop)),
  Thematique.var= rep("Descripteur socio-économique",NROW(noms.varpop)),
  Type.var=rep("Nombre de personnes",NROW(noms.varpop)),
  Unite.var=rep("personnes",NROW(noms.varpop)),
  Popref.var=rep("popTOT",NROW(noms.varpop)) )


popdepartementales$Territoire <- trimws(popdepartementales$Territoire, "both")

#  --- encodage en UTF-8 des noms de territoire

#popdepartementales$Territoire <- enc2utf8(popdepartementales$Territoire)
#popdepartementales$TypeTerritoire <- enc2utf8(popdepartementales$TypeTerritoire)


# -------------------------------------------------------------------------------------------------
# sauvegarde les tables constituées

PopDepartementales <- popdepartementales

# ===================================================================================
# Dernière actualisation de la base réalisée le : 24/06/2022

usethis::use_data(PopDepartementales,
                  PopDepartementales_description,
                  overwrite = T)

