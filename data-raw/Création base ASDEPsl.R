# --------------------------------------------------------------------------------------------------------------
# Création de la base des données d'aide sociale, à partir des bases déjà extraites et incluses dans le package
# --------------------------------------------------------------------------------------------------------------

library(tidyverse)
#library(asdep)

#varcom <- intersect( names(ASDEPslbenef), names(ASDEPsldepenses) )
#
#ASDEPsl <- ASDEPslbenef %>%
#  left_join(ASDEPsldepenses, by = varcom) %>%
#  left_join(PopDepartementales, by = varcom)

mergecom <- function(tab1,tab2) {full_join(tab1,tab2,by = intersect( names(tab1), names(tab2) ))}

ASDEPsl <- ASDEPslbenef %>%
  mergecom(ASDEPsldepenses) %>%
  mergecom(OARSAsl) %>%
  mergecom(PopDepartementales)

ASDEPsl_description <- rbind(
  ASDEPslbenef_description,
  ASDEPsldepenses_description,
  OARSAsl_description,
  PopDepartementales_description
)


# ===================================================================================
usethis::use_data(ASDEPsl,
                  ASDEPsl_description,
                  overwrite = T)
