# --------------------------------------------------------------------------------------------------------------
# Création de la base des dépenses annuelles d'aide sociale, à partir du fichier Excel téléchargé sur data.drees
# --------------------------------------------------------------------------------------------------------------
# (nouvelle version : on utilise maintenant les fonctions du package)

library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(devtools)

devtools::load_all()

# ===================================
# Extraction des données Excel

tabsdepenses <- readExcelDrees(fich="data-raw/Les dépenses d'aide sociale départementale - séries longues (1999 - 2019).xlsx",
                           options = "ASDEPsldepenses")

# ===================================
# Extraction des métadonnées enregistrées par ailleurs

infos.onglets <- read.xlsx("data-raw/Contenu fichiers excel.xlsx",
                           sheet = "SL_depenses_2019",
                           colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = TRUE)

# ===================================
# traitement des bases : 1) métadonnées

# on récupère d'abord les source, champ, note, intitulé dans les informations lues dans le fichier Excel récupéré sous data.drees

ASDEPsldepenses_description <- tabsdepenses$metadonnees %>%
  select(ongletsource, intitule, source, champ,note) %>%
  mutate(intitule = gsub("^[[:punct:][:space:]–]+|[[:punct:][:space:]–]+$","",intitule)) %>%
  rename(Intitule.var = intitule,
         Source.var = source,
         Champ.var = champ,
         Note.var = note)

# d'autres métadonnées ont été enregistrées (à la main) dans un fichier Excel auxiliaire

ASDEPsldepenses_description <- ASDEPsldepenses_description %>%
  left_join(infos.onglets, by= c("ongletsource" = "NoOngletExcel")) %>%
  mutate(Type.var = "Montants",
         Unite.var = "€",
         Thematique.var = tolower(gsub("[[:punct:][:space:]]+|tab[[:digit:]]+$","",ongletsource)),
         Popref.var = recode(Thematique.var,
                             "pa" = "60-99",
                             "ph" = "20-64",
                             "ase" = "00-20",
                             "rsarmi" = "20-64",
                             "tot" = "popTOT",
                             "autres" = "Aide sociale générale"),
         Thematique.var = recode(Thematique.var,
                                 "pa" = "Perte d'autonomie",
                                 "ph" = "Handicap",
                                 "ase" = "Aide sociale à l'enfance",
                                 "rsarmi" = "insertion",
                                 "tot" = "Aide sociale générale",
                                 "autres" = "Aide sociale générale")
         ) %>%
  select(-ongletsource)

# RQ : quelques derniers traitements des métadonnées sont réalisées à la fin du programme

# ===================================
# traitement des bases : 2) indicateurs

ASDEPsldepenses <- tabsdepenses$tablong %>%
  rename(Annee = annee) %>%
  left_join(infos.onglets %>%
              select(NoOngletExcel,Nom.var),
            by = c("sheet" = "NoOngletExcel")) %>%
  select(-c(info.annee,sheet)) %>%
  # RQ : la manip suivante est réalisée car, pour certaines variables, le total France est en double avec un petit écart d'arrondi (ne disparaît pas avec distinct ())
  group_by(Code.region,Code.departement,Territoire,TypeTerritoire,Annee,Nom.var) %>%
  summarise_all(mean) %>%
  ungroup()

# on ajoute certaines variables d'agrégat

#ASDEPsldepenses <- bind_rows(
#  ASDEPsldepenses,
#  ASDEPsldepenses %>%
#    filter(Nom.var %in% c("NbBenefAPA","NbBenefPSD")) %>%
#    select(-Nom.var) %>%
#    group_by(Code.region,Code.departement,Territoire,TypeTerritoire,Annee) %>%
#    summarise_all(sum) %>%
#    ungroup() %>%
#    mutate(Nom.var = "NbBenefAPAPSD"),
#  ASDEPsldepenses %>%
#    filter(Nom.var %in% c("NbBenefACTP","NbBenefPCH")) %>%
#    select(-Nom.var) %>%
#    group_by(Code.region,Code.departement,Territoire,TypeTerritoire,Annee) %>%
#    summarise_all(sum) %>%
#    ungroup() %>%
#    mutate(Nom.var = "TotBenefACTPPCH"),
#)

# on remet les variables en colonnes

ASDEPsldepenses <- ASDEPsldepenses %>%
  pivot_wider(id_cols=c("Code.region","Code.departement","Territoire","TypeTerritoire","Annee"),
              names_from="Nom.var",
              values_from="valeur") %>%
  mutate(Territoire = asdep::corrigeNom(Territoire)) %>%
  data.frame()


# vérification
verif <- unique(ASDEPsldepenses$Territoire)
verif[!(verif %in% asdep::nomscorrectsterritoires$TerritoireCorrect)]

# ===================================
# derniers traitements sur les métadonnées

# ajout des métadonnées sur les agrégats ajoutés

#ASDEPsldepenses_description <- bind_rows(
#  ASDEPsldepenses_description,
#  data.frame(Nom.var= "NbBenefAPAPSD",
#             Intitule.var = "Nombre de bénéficiaires de l'APA ou de la PSD",
#             Intitulecourt.var = "APA ou PSD",
#             Source.var="DREES, Enquêtes Aide sociale",
#             Champ.var="France métropolitaine et DROM (Hors Mayotte)",
#             Note.var="",
#             Thematique.var="Perte d'autonomie",
#             Type.var="Nombres de bénéficiaires",
#             Unite.var="personnes",
#             TexteDenom = "prestations d'APA ou de PSD",
#             ListeDenom.var = c(""),
#             ListeComposante.var = c("NbBenefAPA_NbBenefPSD")),
#  data.frame(Nom.var= "TotBenefACTPPCH",
#             Intitule.var = "Nombre de bénéficiaires de la PCH ou de l'ACTP",
#             Intitulecourt.var = "PCH ou ACTP",
#             Source.var="DREES, Enquêtes Aide sociale",
#             Champ.var="France métropolitaine et DROM (Hors Mayotte)",
#             Note.var="",
#             Thematique.var="Handicap",
#             Type.var="Nombres de bénéficiaires",
#             Unite.var="personnes",
#             TexteDenom = "droits ouverts d'ACTP ou de PCH",
#             ListeDenom.var = c(""),
#             ListeComposante.var = c("NbBenefACTP_NbBenefPCH"))
#)

#  complétude des variables ListeDenom.var à partir des variables ListeComposante.var
rownames(ASDEPsldepenses_description) <- ASDEPsldepenses_description$Nom.var
for (i in (1:nrow(ASDEPsldepenses_description))) {
  if (!(ASDEPsldepenses_description[i,"ListeComposante.var"]) %in% c("", NA)) {
    composantes <- as.vector(unlist(strsplit(ASDEPsldepenses_description[i,"ListeComposante.var"],split="_|\\s")))
    nomdenom <- as.character(ASDEPsldepenses_description[i,"Nom.var"])
    for (j in (1:NROW(composantes))) {
      if (!(grepl(nomdenom,ASDEPsldepenses_description[composantes[j],"ListeDenom.var"]))) {
        if (ASDEPsldepenses_description[composantes[j],"ListeDenom.var"] %in% c(NA,"")) { ASDEPsldepenses_description[composantes[j],"ListeDenom.var"] <- nomdenom }
        else { ASDEPsldepenses_description[composantes[j],"ListeDenom.var"] <- paste(ASDEPsldepenses_description[composantes[j],"ListeDenom.var"],nomdenom,sep="_") }
      }
    }
  }
}

#rownames(ASDEPsldepenses_description) <- ASDEPsldepenses_description$Nom.var

# ===================================================================================
# Dernière actualisation de la base réalisée le : 19/06/2021

usethis::use_data(ASDEPsldepenses,
                  ASDEPsldepenses_description,
                  overwrite = T)
