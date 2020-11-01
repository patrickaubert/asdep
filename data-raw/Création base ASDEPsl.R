# --------------------------------------------------------------------------------------------------------------
# Création de la base des données d'aide sociale, à partir des bases déjà extraites et incluses dans le package
# --------------------------------------------------------------------------------------------------------------

library(tidyverse)
#library(asdep)

varcom <- intersect( names(ASDEPslbenef), names(ASDEPsldepenses) )

ASDEPsl <- ASDEPslbenef %>%
  left_join(ASDEPsldepenses, by = varcom)%>%
  left_join(PopDepartementales, by = varcom)

ASDEPsl_description <- rbind(
  ASDEPslbenef_description,
  ASDEPsldepenses_description,
  PopDepartementales_description
)

# ===================================================================================
usethis::use_data(ASDEPsl,
                  ASDEPsl_description,
                  overwrite = T)
