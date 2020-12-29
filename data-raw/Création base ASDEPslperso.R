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

setwd(paste(getwd(),"/data-raw/",sep=""))

# Extraction des données Excel

tabspers <- readExcelDrees(fich="Le personnel départemental de l'action sociale et médico-sociale de 2014 à 2018.xlsx",
                           options = "ASDEPslperso")

ASDEPslperso_description <- tabspers$metadonnees %>%
  select(ongletsource,info,champ,source,note) %>%
  mutate(ongletsource = gsub("[[:space:]]*\\-[[:space:]]*","_",ongletsource),
         ongletsource = gsub("[[:space:][:punct:]]","",ongletsource),
         info = paste(gsub("^[^\\-]*[[:space:]]*\\-[[:space:]]*","",info),str_extract(info,"^[^\\-]*(?=[[:space:]]*\\-)"),sep=" - ")#,
         #note = paste(str_extract(info,"^[^\\-]*(?=[[:space:]]*\\-)"),note,sep="\n"),
         #info = gsub("^[^\\-]*[[:space:]]*\\-[[:space:]]*","",info)
         ) %>%
  rename(Nom.var = ongletsource,
         Intitule.var = info,
         Note.var = note,
         Champ.var = champ,
         Source.var = source) %>%
  mutate(Thematique.var = "Personnels",
         TexteDenom = "personnes",
         Type.var = "Nombres de personnes",
         Popref.var = "popTOT")

ASDEPslperso <- tabspers$tablong %>%
  rename(Annee = annee) %>%
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
usethis::use_data(ASDEPslperso,
                  ASDEPslperso_description,
                  overwrite = T)
