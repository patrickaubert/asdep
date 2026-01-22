# --------------------------------------------------------------------------------------------------------------
# Création fond de carte
# --------------------------------------------------------------------------------------------------------------

# la création de ces fonds de carte s'est inspiré des codes diffusés par iresmi :
# http://r.iresmi.net/2020/05/26/polygons-to-hexagons/
# http://r.iresmi.net/2020/06/15/using-the-geofacet-package-to-spatially-arrange-plots/


# packages ----------------------------------------------------------------
library(tidyverse)
library(sf)
library(janitor)
library(geogrid)
library(geofacet)
library(cartogram)
library(rmapshaper)
library(plotly)


#library(httr)
#library(fs)
#library(tmap)
#library(grid)
#library(classInt)
#library(magick)

# sources -----------------------------------------------------------------
pathdep <- "C:/Users/PA/Documents/R/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00235/GEOFLA_2-2_SHP_LAMB93_FR-ED161/DEPARTEMENT"

# données -----------------------------------------------------------------
dep <- read_sf(paste0(pathdep, "/DEPARTEMENT.shp")) %>%
  clean_names() %>%
  left_join(asdep::departementsFR, by= c("code_dept" = "NumDept")) %>%
  rename(NumDept = code_dept) %>%
  select(NumDept)

# ===== fonds de carte simplifié
depsimpl <- dep %>% sf::st_transform(3857)
depsimpl <- depsimpl %>%
  rmapshaper::ms_simplify(input = as(depsimpl, 'Spatial'), keep=0.005) %>%
  st_as_sf()

# ===== carte avec grille hexagonale
dep_cells_hex <- calculate_grid(shape = dep, grid_type = "hexagonal", seed = 2)
dep_hex <- assign_polygons(dep, dep_cells_hex) #%>%
#st_set_crs(2154)
# pour la communauté unique de corse, on reprend la place du 2A
d20 <- dep_hex[dep_hex$NumDept == "2A", ]
d20$NumDept <- "20"
# Pour les DOM on duplique et déplace un département existant
d971 <- dep_hex[dep_hex$NumDept == "29", ]
d971$geometry[[1]] <- d971$geometry[[1]] + st_point(c(0, -350000))
d971$NumDept <- "971"
d972 <- dep_hex[dep_hex$NumDept == "29", ]
d972$geometry[[1]] <- d972$geometry[[1]] + st_point(c(0, -450000))
d972$NumDept <- "972"
d973 <- dep_hex[dep_hex$NumDept == "29", ]
d973$geometry[[1]] <- d973$geometry[[1]] + st_point(c(0, -550000))
d973$NumDept <- "973"
d974 <- dep_hex[dep_hex$NumDept == "29", ]
d974$geometry[[1]] <- d974$geometry[[1]] + st_point(c(0, -650000))
d974$NumDept <- "974"
d976 <- dep_hex[dep_hex$NumDept == "29", ]
d976$geometry[[1]] <- d976$geometry[[1]] + st_point(c(0, -750000))
d976$NumDept <- "976"
dep_hex <- rbind(dep_hex, d971, d972, d973, d974, d20) #, d976)

# ===== cartes en anamorphose
dep_pour_ana <- depsimpl %>%
  left_join(PopDepartementales %>%
              filter(TypeTerritoire == "Département", Annee == 2019) %>%
              select(Code.departement,popTOT),
            by = c("NumDept" = "Code.departement")) %>%
  filter(popTOT>0)

fondcarte_anapop <- cartogram_cont(dep_pour_ana[,c("geometry","NumDept","popTOT")],
                                weight="popTOT")


# ==== carte avec grille pour graphiques par département
grid_fr <- dep %>%
  left_join(asdep::departementsFR, by= c("code_dept" = "NumDept")) %>%
  rename(NumDept = code_dept) %>%
  select(Departement,NumDept) %>%
  grid_auto(names = "Departement", codes = "NumDept", seed = 4) %>%
  add_row(row = 8,col = 1,
          name_Departement = "Guadeloupe",  code_NumDept = "971") %>%
  add_row(row = 9, col = 1,
          name_Departement = "Martinique", code_NumDept = "972") %>%
  add_row(row = 10, col = 1,
          name_Departement = "Guyane", code_NumDept = "973") %>%
  add_row(row = 11, col = 1,
          name_Departement = "La Réunion", code_NumDept = "974") %>%
  add_row(row = 12, col = 1,
          name_Departement = "Mayotte", code_NumDept = "976")
grid_fr[grid_fr$code_NumDept %in% c("2A", "2B"), "col"] <- 13
grid_fr[grid_fr$code_NumDept %in% c("2A", "2B"), "row"] <- grid_fr[grid_fr$code_NumDept %in% c("2A", "2B"), "row"] - 1



# essai de graphique -----------------------------------------------------
if (FALSE) {
  tabgraph0 <- ASDEPsl %>%
    filter(TypeTerritoire == "Département",Annee==2019) %>%
    mutate(txpch = round(1000*TotBenefACTPPCH / popTOT, 1) ) %>%
    filter(!is.na(txpch)) %>%
    select(Annee, txpch,Territoire,Code.departement) %>%
    mutate(label = paste(Territoire,", ",Annee,"<br>",txpch," pour 1000 hab.",sep=""))

  # graphique départements hexagonaux
  tabgraph <- dep_hex %>%
    right_join(tabgraph0 , by = c("NumDept" = "Code.departement"))

  # graphique : carte départementale "simple"
  tabgraph <- depsimpl %>%
    right_join(tabgraph0 , by = c("NumDept" = "Code.departement"))

  # graphique : carte départementale en anamorphose (proportionnelle à la population)
  tabgraph <- fondcarte_anapop %>%
    right_join(tabgraph0 , by = c("NumDept" = "Code.departement"))


  # affichage du graphique
  g <-  ggplotAsdep(tabgraph, aes(fill=txpch,label=NumDept)) + #paste(NumDept,txpch,sep="\n"))) +
    geom_sf() +
    geom_sf_text() +
    coord_sf(datum = NA) +
    theme(legend.position = "none") +
    labs(title = "Taux d'ACTP ou PCH")

  ggplotlyAsdep(g)



  # graphique : grille de petits graphiques départementaux
  g <-  ggplotAsdep(tabgraph, aes(y=txpch,x=Annee)) +
    geom_line() +
    facet_geo(~ Territoire, grid = grid_fr) +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    labs(title = "Taux d'ACTP ou PCH")

  ggplotlyAsdep(g)
}


# ===================================================================================
# sauvegarde dans le package
fondcarte_dep <- depsimpl
fondcarte_hex <- dep_hex
fondcarte_grilledep <- grid_fr

usethis::use_data(fondcarte_dep,
                  fondcarte_hex,
                  fondcarte_grilledep,
                  overwrite = T)
