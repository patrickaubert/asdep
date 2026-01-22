# --------------------------------------------------------------------------------------------------------------
# Création de la base des minima sociaux, à partir du fichier Excel téléchargé sur data.drees
# --------------------------------------------------------------------------------------------------------------

library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

#options(encoding = "utf8")

setwd(paste(getwd(),"/data-raw/",sep=""))

# Extraction des données Excel

getSheetNames("Minima sociaux - donnees departementales par dispositif.xlsx")

tabsms1 <- readExcelDrees(fich="Minima sociaux - donnees departementales par dispositif.xlsx",
                          sheetexclude = c("Sommaire","Tableau 11","Tableau 14","Tableau 15","Tableau 16","Tableau 17","Tableau 18","Tableau 19"),
                          nlignetitre = 1,
                          options = "minsocsl")
tabsms2 <- readExcelDrees(fich="Minima sociaux - donnees departementales par dispositif.xlsx",
                          sheetinclude = c("Tableau 11","Tableau 14","Tableau 15"),
                          sheetexclude = c("Sommaire","Tableau 16","Tableau 17","Tableau 18","Tableau 19"),
                          nlignetitre = 2,
                          options = "minsocsl")

MINSOCsl_description <- bind_rows(
  tabsms1$metadonnees,
  tabsms2$metadonnees
  ) %>%
  select(ongletsource,intitule,info,source,note) %>%
  mutate(Intitulecourt.var = gsub("par département","",intitule),
         Intitulecourt.var = gsub("selon le département de résidence","",Intitulecourt.var),
         Intitulecourt.var = gsub("de chaque année","",Intitulecourt.var),
         Intitulecourt.var = gsub("depuis [[:digit:]]+","",Intitulecourt.var),
         Intitulecourt.var = gsub("de [[:digit:]]+ à [[:digit:]]+","",Intitulecourt.var),
         Intitulecourt.var = gsub("Répartition des allocataires (de l'|du |des )","",Intitulecourt.var),
         Intitulecourt.var = gsub("Répartition (de l'|du |des |de la )","",Intitulecourt.var),
         Intitulecourt.var = gsub("au 31 décembre","",Intitulecourt.var),
         Intitulecourt.var = trimws(gsub("[[:space:]]+"," ",Intitulecourt.var)),
         ongletsource = gsub("[[:space:][:punct:]]","",ongletsource),
         note = paste(info,note,sep="\n"),
  ) %>%
  select(-info) %>%
  rename(Nom.var = ongletsource,
         Intitule.var = intitule,
         Note.var = note,
         Source.var = source) %>%
  mutate(Thematique.var = "Minima sociaux",
         TexteDenom = "personnes",
         Unite.var = "personnes",
         Type.var = "Nombres de bénéficiaires",
         Champ.var = "France",
         Popref.var = "popTOT")

# nom de variables : on utilise le nom de la prestation, récupéré d'après l'intitulé de chaque tableau
notab <- MINSOCsl_description$Nom.var
nompresta <- gsub("[^[:alpha:]]","",MINSOCsl_description$Intitulecourt.var)
nompresta[c(7,8,9,12,13,14,15)] <- c("MinV","ASI","AV","TotMinSoc","RSAalloc","RSAbenef","RSApopcouv")
correcnom <- setNames(nompresta, notab)
MINSOCsl_description <- MINSOCsl_description %>%
  mutate(Nom.var = recode(Nom.var, !!!correcnom))
rownames(MINSOCsl_description) <- MINSOCsl_description$Nom.var

MINSOCsl <- bind_rows(
  tabsms1$tablong,
  tabsms2$tablong
  ) %>%
  rename(Annee = annee) %>%
  mutate(sheet = gsub("[[:space:][:punct:]]","",sheet),
         Territoire = trimws(gsub("\\*","",Territoire))) %>%
  mutate(sheet = recode(sheet, !!!correcnom)) %>%
  # pour les prestations CNAF : on retient l'année 2016 définitive, et on supprime la semi-définitive
  filter(!((Annee == 2016) & !(grepl("\\(",info.annee)) & (sheet %in% c("AAH","RSO","TotMinSoc","RSAalloc","RSAbenef","RSApopcouv")))) %>%
  select(-info.annee) %>%
  pivot_wider(id_cols=c("Code.departement","Territoire","TypeTerritoire","Annee"),
              names_from="sheet",
              values_from="valeur") %>%
  mutate(Territoire = asdep::corrigeNom(Territoire)) %>%
  mutate(RMIRSA = ifelse(is.na(RMI),0,RMI)+ifelse(is.na(RSAalloc),0,RSAalloc) )

# vérification
verif <- unique(MINSOCsl$Territoire)
verif[!(verif %in% asdep::nomscorrectsterritoires$TerritoireCorrect)]



# ===================================================================================
usethis::use_data(MINSOCsl,
                  MINSOCsl_description,
                  overwrite = T)
