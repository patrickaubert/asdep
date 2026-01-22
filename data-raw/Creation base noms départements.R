# --------------------------------------------------------------------------------------------------------------
# Création des bases auxiliaires avec les noms de régions et de départements
# --------------------------------------------------------------------------------------------------------------

setwd(paste(getwd(),"/data-raw/",sep=""))

# Lectures des données de base
departements <- read.csv2("Liste des departements.csv",header=TRUE,sep=",",stringsAsFactors = FALSE,fileEncoding="utf8")
regions <- read.csv2("Liste des regions.csv",header=TRUE,sep=",",stringsAsFactors = FALSE,fileEncoding="utf8")
syndep <- read.csv2("Synonymes noms départements.csv",header=TRUE,sep=",",stringsAsFactors = FALSE,fileEncoding="utf8")

# Table des codes
codeterritoires <- bind_rows(
  departements %>%
    dplyr::rename(Territoire = Departement,
           Code.departement = NumDept,
           Code.region = NumReg) %>%
    mutate(TypeTerritoire = "Département"),
  regions %>%
    dplyr::rename(Territoire = Region,
           Code.region = NumReg) %>%
    mutate(TypeTerritoire = "Région")
)

# Table des "synonymes" de noms de territoire (ie noms mal orthographies)
nomscorrectsterritoires <- syndep %>%
  dplyr::rename(TerritoireCorrect = Nom.departement,
         TerritoireMalortho = Synonyme.nom)

# NROW(unique(nomscorrectsterritoires$TerritoireMalortho)) == NROW(nomscorrectsterritoires$TerritoireMalortho)


# ===================================================================================
usethis::use_data(codeterritoires,
                  nomscorrectsterritoires,
                  overwrite = T)
