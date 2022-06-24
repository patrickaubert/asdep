# --------------------------------------------------------------------------------------------------------------
# Création de la base des personnesl de l'aide sociale départementale, à partir du fichier Excel téléchargé sur data.drees
# --------------------------------------------------------------------------------------------------------------

library(openxlsx)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

#options(encoding = "utf8")

#setwd(paste(getwd(),"/data-raw/",sep=""))

# == Extraction des données Excel

tabspers <- readExcelDrees(fich="data-raw/Le personnel départemental de l'action sociale et médico-sociale de 2014 à 2020.xlsx",
                           options = "ASDEPslperso")

# == mise en forme des métadonnées

ASDEPslperso_description <- tabspers$metadonnees %>%
  select(ongletsource,info,champ,source,note) %>%
  mutate(ongletsource = gsub("[[:space:]]*\\-[[:space:]]*","_",ongletsource),
         ongletsource = gsub("[[:space:][:punct:]]","",ongletsource),
         note = paste(str_extract(info,"\\\n.*$"),note,sep="\n"),
         info = info %>% str_replace("\\\n.*$","") %>% trimws(),
         Intitulecourt.var = gsub("^[^\\-]*[[:space:]]*\\-[[:space:]]*","",info),
         # -- deux lignes suivantes désactivées le 24/06/2022
         #Intitulecourt.var = gsub("^[^\\-]*[[:space:]]*\\-[[:space:]]*","",info),
         #info = paste(gsub("^[^\\-]*[[:space:]]*\\-[[:space:]]*","",info),str_extract(info,"^[^\\-]*(?=[[:space:]]*\\-)"),sep=" - ")#,
         # -- deux lignes suivantes étaient déjà désactivées
         #note = paste(str_extract(info,"^[^\\-]*(?=[[:space:]]*\\-)"),note,sep="\n"),
         #info = gsub("^[^\\-]*[[:space:]]*\\-[[:space:]]*","",info)
         ) %>%
  dplyr::rename(Nom.var = ongletsource,
         Intitule.var = info,
         Note.var = note,
         Champ.var = champ,
         Source.var = source) %>%
  mutate(Thematique.var = "Personnels",
         TexteDenom = "personnes",
         Unite.var = "personnes",
         Type.var = "Nombres de personnels",
         Popref.var = "popTOT")
rownames(ASDEPslperso_description) <- ASDEPslperso_description$Nom.var

# == mise en forme des données

ASDEPslperso <- tabspers$tablong %>%
  dplyr::rename(Annee = annee) %>%
  select(-info.annee) %>%
  mutate(sheet = gsub("[[:space:]]*\\-[[:space:]]*","_",sheet),
         sheet = gsub("[[:space:][:punct:]]","",sheet)) %>%
  pivot_wider(id_cols=c("Code.departement","Territoire","TypeTerritoire","Annee"),
              names_from="sheet",
              values_from="valeur") %>%
  mutate(Territoire = asdep::corrigeNom(Territoire))

# vérification
verif <- unique(ASDEPslperso$Territoire)
verif[!(verif %in% asdep::nomscorrectsterritoires$TerritoireCorrect)]



# ===================================================================================
# dernière actualisation : 24/06/2022

usethis::use_data(ASDEPslperso,
                  ASDEPslperso_description,
                  overwrite = T)
