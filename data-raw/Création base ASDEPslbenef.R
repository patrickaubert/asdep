# --------------------------------------------------------------------------------------------------------------
# Création de la base des effectifs de bénéficiaires d'aide sociale, à partir du fichier Excel téléchargé sur data.drees
# --------------------------------------------------------------------------------------------------------------
# (nouvelle version : on utilise maintenant les fonctions du package)

library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# ===================================
# Extraction des données Excel

tabsbenef <- readExcelDrees(fich="data-raw/Les bénéficiaires de laide sociale départementale - séries longues (1996-2019).xlsx",
                           options = "ASDEPslbenef")

# ===================================
# Extraction des métadonnées enregistrées par ailleurs

infos.onglets <- read.xlsx("data-raw/Contenu fichiers excel.xlsx",
                           sheet = "SL_benef_2019",
                           colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = TRUE)

# ===================================
# traitement des bases : 1) métadonnées

pasteNA <- function(a,b) ifelse(is.na(a),b,ifelse(is.na(b),a,paste(a,b)))

# on récupère d'abord les source, champ, note, intitulé dans les informations lues dans le fichier Excel récupéré sous data.drees

ASDEPslbenef_description <- tabsbenef$metadonnees %>%
  select(ongletsource, intitule, source, champ,note,info) %>%
  mutate(note = pasteNA(note,info)) %>%
  select(-info) %>%
  rename(Intitule.var = intitule,
         Source.var = source,
         Champ.var = champ,
         Note.var = note)

# d'autres métadonnées ont été enregistrées (à la main) dans un fichier Excel auxiliaire

ASDEPslbenef_description <- ASDEPslbenef_description %>%
  left_join(infos.onglets, by= c("ongletsource" = "NoOngletExcel")) %>%
  mutate(Type.var = "Nombres de bénéficiaires",
         Unite.var = "personnes",
         Thematique.var = gsub("^[^\\-]+\\-","",ongletsource),
         Popref.var = recode(Thematique.var,
                             "pa" = "60-99",
                             "ph" = "20-64",
                             "ase" = "00-20"),
         Thematique.var = recode(Thematique.var,
                                 "pa" = "Perte d'autonomie",
                                 "ph" = "Handicap",
                                 "ase" = "Aide sociale à l'enfance")
         ) %>%
  select(-ongletsource)

# quelques derniers traitements des métadonnées sont réalisées à la fin du programme

# ===================================
# traitement des bases : 2) indicateurs

ASDEPslbenef <- tabsbenef$tablong %>%
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

ASDEPslbenef <- bind_rows(
  ASDEPslbenef,
  ASDEPslbenef %>%
    filter(Nom.var %in% c("NbBenefAPA","NbBenefPSD")) %>%
    select(-Nom.var) %>%
    group_by(Code.region,Code.departement,Territoire,TypeTerritoire,Annee) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    mutate(Nom.var = "NbBenefAPAPSD"),
  ASDEPslbenef %>%
    filter(Nom.var %in% c("NbBenefACTP","NbBenefPCH")) %>%
    select(-Nom.var) %>%
    group_by(Code.region,Code.departement,Territoire,TypeTerritoire,Annee) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    mutate(Nom.var = "TotBenefACTPPCH"),
)

# on remet les variables en colonnes

ASDEPslbenef <- ASDEPslbenef %>%
  pivot_wider(id_cols=c("Code.region","Code.departement","Territoire","TypeTerritoire","Annee"),
              names_from="Nom.var",
              values_from="valeur") %>%
  mutate(Territoire = asdep::corrigeNom(Territoire)) %>%
  data.frame()


# vérification
verif <- unique(ASDEPslbenef$Territoire)
verif[!(verif %in% asdep::nomscorrectsterritoires$TerritoireCorrect)]

# ===================================
# derniers traitements sur les métadonnées

# ajout des métadonnées sur les agrégats ajoutés

ASDEPslbenef_description <- bind_rows(
  ASDEPslbenef_description,
  data.frame(Nom.var= "NbBenefAPAPSD",
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
             ListeComposante.var = c("NbBenefAPA_NbBenefPSD")),
  data.frame(Nom.var= "TotBenefACTPPCH",
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
)

#  complétude des variables ListeDenom.var à partir des variables ListeComposante.var
rownames(ASDEPslbenef_description) <- ASDEPslbenef_description$Nom.var
for (i in (1:nrow(ASDEPslbenef_description))) {
  if (!(ASDEPslbenef_description[i,"ListeComposante.var"]) %in% c("", NA)) {
    composantes <- as.vector(unlist(strsplit(ASDEPslbenef_description[i,"ListeComposante.var"],split="_|\\s")))
    nomdenom <- as.character(ASDEPslbenef_description[i,"Nom.var"])
    for (j in (1:NROW(composantes))) {
      if (!(grepl(nomdenom,ASDEPslbenef_description[composantes[j],"ListeDenom.var"]))) {
        if (ASDEPslbenef_description[composantes[j],"ListeDenom.var"] %in% c(NA,"")) { ASDEPslbenef_description[composantes[j],"ListeDenom.var"] <- nomdenom }
        else { ASDEPslbenef_description[composantes[j],"ListeDenom.var"] <- paste(ASDEPslbenef_description[composantes[j],"ListeDenom.var"],nomdenom,sep="_") }
      }
    }
  }
}

rownames(ASDEPslbenef_description) <- ASDEPslbenef_description$Nom.var

# ===================================================================================
usethis::use_data(ASDEPslbenef,
                  ASDEPslbenef_description,
                  overwrite = T)
