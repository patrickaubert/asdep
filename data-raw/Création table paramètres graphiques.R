# Création d'une table avec les paramètres graphiques du package (pour toutes les fonctions produisant des graphiques)
# --------------------------------------------------------------------------------------------------------------


ParamGraphiquesAsdep <- data.frame(
  intitules = c("Territoire de référence","Groupe de comparaison","Autres territoires","Zone interdécile","Zone interquartile","Zone interdécile (pondérée)","Zone interquartile (pondérée)","Médiane +/- 20 %","Médiane +/- 10 %"),
  noms = c("dept","comp","autres","interdeciles","interquartiles","interdecilespond","interquartilespond","medianePM20","medianePM10"),
  ymin = c("","","","p10","p25","p10pond","p25pond","p50.m20","p50.m10"),
  ymax = c("","","","p90","p75","p90pond","p75pond","p50.p20","p50.p10"),
  alpha = c(0.7,0.5,0.3,0.1, 0.2, 0.1, 0.2, 0.1, 0.2),
  #couleur = c("red","blue","grey","blue","blue","green","green","red","red"),
  couleur = c("#1B9E77","#D95F02","#7570B3","#E7298A","#E7298A","#66A61E","#66A61E","#E6AB02","#E6AB02"), # "#A6761D"
  stringsAsFactors = FALSE
)
rownames(ParamGraphiquesAsdep) <- ParamGraphiquesAsdep$noms

# rq : la palette de couleur est inspirée de la palette "Dark2" de RColorBrewer


# ===================================================================================
usethis::use_data(ParamGraphiquesAsdep,
                  overwrite = T)
