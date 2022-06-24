# ==================================================================================================
# Prévalence dans les prestations d'aide sociale pour personnes âgées : construction des fichiers
# ==================================================================================================



# ==================================================================================================
# 0) Initialisation


library(readxl)
library(tidyverse)

# remotes::install_github("patrickaubert/healthexpectancies",ref='main')
library(healthexpectancies)

reprawdata <- "C:/Users/PA/Documents/R/Projets/divers_stat_privé/Evolution APA/"


# ==================================================================================================
# 1) extraction des données : parts par âge pour les principales prestations

# fonctions pour l'extraction

extrAPA2016 <- function(an,rangegir="A3:E5",rangeage ="A12:I17",rangegirage="A5:I12") {
  fich <- paste(reprawdata,"/data-raw/APA - Données détaillées par GIR, âge et sexe en ",
                as.character(an),".xlsx",sep="")
  gir <- read_excel(fich,sheet = "nat-APA par GIR", range = rangegir)
  names(gir) <- c("prestation", names(gir)[2:5])
  gir <- gir %>% pivot_longer(cols=-prestation,names_to="gir",values_to="part")
  age <- read_excel(fich,sheet = "nat-APA par sexe et par âge", range = rangeage) %>% filter(!is.na(...1))
  names(age) <- c("prestation", names(age)[2:NROW(names(age))])
  age <- age %>% pivot_longer(cols=-prestation,names_to="trAge",values_to="part")
  girage <- read_excel(fich,sheet = "nat-APA par GIR et âge", col_names=FALSE, range = rangegirage)
  names(girage) <- c("trAge",paste0("domicile_GIR",1:4),paste0("étab_GIR",1:4))
  girage <- girage %>% pivot_longer(cols=-trAge,names_to="trGirAge",values_to="partGir") %>%
    mutate(prestation = gsub("_.*$","",trGirAge),
           gir = gsub("^.*_","",trGirAge)) %>%
    select(-trGirAge)
  return(list(
    gir = gir %>% mutate(annee = an),
    age = age %>% mutate(annee = an),
    girage = girage %>% mutate(annee = an)
  ))
}

extrAPA2014 <- function(an,rangegir="A3:D4",rangeage ="A3:G5",rangegirage="A5:I10") {
  fich <- paste(reprawdata,"/data-raw/APA - Données détaillées par GIR, âge et sexe en ",
                as.character(an),".xlsx",sep="")
  girdom <- read_excel(fich,sheet = "nat-APA domicile par GIR", range = rangegir)
  giretab <- read_excel(fich,sheet = "nat-APA étab par GIR", range = rangegir)
  gir <- rbind(girdom %>% mutate(prestation = "APAdom"),
               giretab %>% mutate(prestation = "APAetab")) %>%
    pivot_longer(cols=-prestation,names_to="gir",values_to="part")
  agedom <- read_excel(fich,sheet = "nat-APA domicile par âge", range = rangeage)
  ageetab <- read_excel(fich,sheet = "nat-APA étab par âge", range = rangeage)
  age <-  rbind(agedom,ageetab ) %>%
    rename( "prestation" = "...1") %>%
    pivot_longer(cols=-prestation,names_to="trAge",values_to="part")
  girage <- read_excel(fich,sheet = "nat-APA par GIR et âge", col_names=FALSE, range = rangegirage)
  names(girage) <- c("trAge",paste0("domicile_GIR",1:4),paste0("étab_GIR",1:4))
  girage <- girage %>% pivot_longer(cols=-trAge,names_to="trGirAge",values_to="partGir") %>%
    mutate(prestation = gsub("_.*$","",trGirAge),
           gir = gsub("^.*_","",trGirAge)) %>%
    select(-trGirAge)
  return(list(
    gir = gir %>% mutate(annee = an),
    age = age %>% mutate(annee = an),
    girage = girage %>% mutate(annee = an)
  ))
}

extrAPA2010 <- function(an,rangegir="A3:D5",rangeagedom ="B9:G12",
                        rangeageetab ="A4:F11",rangegirage="A3:E9") {
  fich <- paste(reprawdata,"/data-raw/Les bénéficiaires de l'aide sociale départementale en ",
                as.character(an),".xlsx",sep="")
  graph <- function(nb) ifelse(an==2010,paste0("graph",nb),paste0("Graph",nb))
  tab <- function(nb) ifelse(an==2010,paste0("tab",nb),paste0("Tab",nb))
  girdom <- read_excel(fich,sheet = graph(4), range = rangegir)
  # attention : pour les fichiers 2012 et 2013, on a supprimé  à la main la 3e ligne de l'onglet Graph3
  giretab <- read_excel(fich,sheet = graph(3), range = rangegir)
  gir <- rbind(girdom[2,] %>% mutate(prestation = "APAdom"),
               giretab[2,] %>% mutate(prestation = "APAetab")) %>%
    pivot_longer(cols=-prestation,names_to="gir",values_to="part")
  if (an == 2010) {
    agedom <- read_excel(fich,sheet = graph(6), range = rangeagedom)[3,] %>%
      mutate(prestation = "APAdom")
    ageetab <- read_excel(fich,sheet = graph(7), range = rangeageetab)[c(2,7),]  %>%
      mutate(prestation = c("ASH","APAetab"))
  } else if (an<=2011) {
    agedom <- read_excel(fich,sheet = graph(6), range = "B3:G8")[5,] %>%
      mutate(prestation = "APAdom")
    ageetab <- read_excel(fich,sheet = graph(7), range = "B3:G7")[c(2,4),]  %>%
      mutate(prestation = c("ASH","APAetab"))
  } else  {
    agedom <- read_excel(fich,sheet = graph(6), range = "B3:G9")[6,] %>%
      mutate(prestation = "APAdom")
    ageetab <- read_excel(fich,sheet = graph(7), range = "B3:G9")[c(5,6),]  %>%
      mutate(prestation = c("ASH","APAetab"))
  }
  age <-  rbind(
    agedom %>%  pivot_longer(cols=-prestation,names_to="trAge",values_to="part"),
    ageetab %>%  pivot_longer(cols=-prestation,names_to="trAge",values_to="part")
  )
  if (an == 2013) {
    giragedom <- read_excel(fich,sheet = "Graph 8 ", range = rangegirage)
    girageetab <- read_excel(fich,sheet = "Graph9", range = rangegirage)
  } else {
    giragedom <- read_excel(fich,sheet = tab(1), range = rangegirage)
    girageetab <- read_excel(fich,sheet = tab(2), range = rangegirage)
  }
  names(giragedom) <- c("trAge",names(giragedom)[2:5])
  names(girageetab) <- c("trAge",names(girageetab)[2:5])
  girage <- rbind(giragedom %>% mutate(prestation = "APAdom"),
                  girageetab %>% mutate(prestation = "APAetab")) %>%
    pivot_longer(cols=-c(trAge,prestation),names_to="gir",values_to="partGir")
  girage <- girage[,c("trAge","partGir","prestation","gir")]
  return(list(
    gir = gir %>% mutate(annee = an),
    age = age %>% mutate(annee = an),
    girage = girage %>% mutate(annee = an)
  ))
}

# extraction des données fichier par fichier

dataApa <- list()

for (an in 2016:2019) {
  dataApa[[as.character(an)]] <- extrAPA2016(an)
}
for (an in 2014:2015) {
  dataApa[[as.character(an)]] <- extrAPA2014(an)
}
for (an in 2010:2013) {
  dataApa[[as.character(an)]] <- extrAPA2010(an)
}

# agrégation des tableaux

rangeAn <- c(2010:2019)

gir <- do.call("rbind", lapply(rangeAn, function(a){dataApa[[as.character(a)]]$gir}))

age <- do.call("rbind", lapply(rangeAn, function(a){dataApa[[as.character(a)]]$age}))

girage <- do.call("rbind", lapply(rangeAn, function(a){dataApa[[as.character(a)]]$girage}))

# récupération des ventilations par âge antérieures à 2010

ageavt <- read_csv2(
  paste0(reprawdata,"/data-raw/part par âge et prestations_synthèse DT bénéficiaires aides sociales.csv") ) %>%
  filter(type=="pct") %>%
  select(-type,-total) %>%
  pivot_longer(cols=-c("annee","prestation"),names_to="trAge",values_to="part") %>%
  filter(!is.na(part)) %>%
  group_by(annee,prestation) %>% mutate(part= part/sum(part)) %>% ungroup()
attr(ageavt, 'spec') <- NULL

age <- bind_rows(ageavt,
                 age %>% mutate(part = as.numeric(part)) )

# homogénéisation des données

presta <- list("APA à domicile" = "APAdom",
               "Aide sociale à l'hébergement (ASH)" = "ASH",
               "APA en établissement" = "APAetab",
               "Aide sociale à l’hébergement (ASH)" = "ASH",
               "APA en établissement (hors dotation globale)" = "APAetab",
               "Aide ménagère" = "AidesMenag",
               "Aide ménagères" = "AidesMenag",
               "aide ménagère" = "AidesMenag",
               "Domicile" = "APAdom",
               "Etablissement" = "APAetab",
               "APA HDG+EDG" = "APAetab",
               "APA en établissements"= "APAetab",
               "domicile" = "APAdom",
               "étab" = "APAetab",
               "aides à domicile" = "EnsAidesDom",
               "aides à l’hébergement" = "EnsAidesEtab",
               "PSD à domicile" = "PSDDom",
               "PSD en établissement" = "PSDEtab",
               "Ensemble des aides à domicile" = "EnsAidesDom",
               "Ensemble des aides en établissement" = "EnsAidesEtab")

trancheage <- list(
  "Moins de 65 ans" = "[60,65)",
  "moins de 65 ans" = "[60,65)",
  "de 60 à 65 ans" = "[60,65)",
  "de 65 à 69 ans" = "[65,70)",
  "de 70 à 74 ans" = "[70,75)",
  "de 75 à 79 ans" = "[75,80)",
  "de 80 à 84 ans" = "[80,85)",
  "85 ans ou plus" = "[85,Inf]",
  "85 ans et plus" = "[85,Inf]",
  "de 85 à 89 ans" = "[85,90)",
  "de 90 à 94 ans" = "[90,95)",
  "95 ans et plus" = "[95,Inf]",
  "95 ans ou plus" = "[95,Inf]"
)

age <- age %>%
  mutate(part = as.numeric(part),
         age = recode(tolower(trAge), !!!trancheage),
         prestation = recode(prestation, !!!presta)) %>%
  filter(prestation %in% unique(unlist(presta)))

gir <- gir %>%
  mutate(prestation = recode(prestation, !!!presta)) %>%
  filter(prestation %in% unique(unlist(presta)))

girage <- girage %>%
  mutate(age = recode(tolower(trAge), !!!trancheage),
         gir = gsub(" ","",gir),
         prestation = recode(prestation, !!!presta)) %>%
  filter(prestation %in% unique(unlist(presta)))


# ==================================================================================================
# 2) Effectifs par tranches d'âge
# (rq : dans cette section, on crée aussi des agrégats intermédiaires qui n'existe pas dans les données de base)

# récupération des nombres de bénéficiaires des prestations à partir de la table ASDEPslbenef du package 'asdep'

eff <- asdep::ASDEPslbenef[
  ASDEPslbenef$Territoire == "TOTAL estimé France entière (hors Mayotte)",
  c("Annee","NbBenefAPADomicile","NbBenefAPAEtab","NbBenefASH","NbBenefAideMenagerePA",
    "NbBenefPSD", "NbBenefPSDDomicile", "NbBenefPSDEtab",
    "TotBenefPADomicile", "TotBenefPAEtab" )] %>%
  filter(Annee %in% c(min(age$annee):max(age$annee))) %>%
  rename(annee = Annee ,
         APAdom = NbBenefAPADomicile,
         APAetab = NbBenefAPAEtab ,
         ASH = NbBenefASH,
         AidesMenag = NbBenefAideMenagerePA,
         PSDDom = NbBenefPSDDomicile,
         PSDEtab = NbBenefPSDEtab,
         EnsAidesDom = TotBenefPADomicile,
         EnsAidesEtab = TotBenefPAEtab) %>%
  pivot_longer(cols=-annee,names_to="prestation",values_to="nb")

# calcul des effectifs par age et GIR
# RQ : on calcule ici les agrégats "APA" et "GIR12" et "GIR34"
# RQ2 : Attention, les ventilations par âges pour l'APA tous GIR confondus et l'APA par GIR ne sont pas
#       forcément cohérentes entre elles, du fait de la non-réponse partielle et des modalités de réponse
#       "inconnu" dans l'enquête => on intègre deux modes de calcul, avec et son recalage

gir <- gir %>%
  left_join(eff, by=c("annee","prestation")) %>%
  mutate(nb = part*nb)

age <- age %>%
  left_join(eff, by=c("annee","prestation")) %>%
  mutate(nb = part*nb)

girage <- girage %>%
  left_join(gir[,c("annee","prestation","gir","nb")], by=c("annee","prestation","gir")) %>%
  mutate(nb = partGir*nb)

# -- vérification de la cohérence par âge vs. par âge * gir
#calegir <- girage %>%
#  select(prestation,annee,age,nb) %>%
#  group_by(prestation,annee,age) %>% summarise(nbtsgir = sum(nb)) %>% ungroup() %>%
#  left_join(age %>% select(annee,prestation,age,nb) %>% rename(nbcale=nb),
#            by=c("annee","age","prestation") ) %>%
#  mutate(ecart=round(nbtsgir/nbcale-1,3))

# ajout des totaux intermédiaires APA tous lieux, GIR12, GIR34

age2 <- bind_rows(
  age %>% mutate(recale_gir = FALSE),
  girage %>%
    select(prestation,annee,age,nb) %>%
    group_by(prestation,annee,age) %>% summarise_all(sum) %>% ungroup() %>%
    mutate(recale_gir = TRUE) %>%
    group_by(prestation,annee) %>% mutate(part=nb/sum(nb)) %>% ungroup()
)

age3 <- rbind(
  age2 %>% select(prestation,annee,age,nb,recale_gir),
  age2 %>%
    filter(prestation %in% c("APAdom","APAetab")) %>%
    select(annee,age,nb,recale_gir) %>%
    group_by(annee,age,recale_gir) %>% summarise_all(sum) %>% ungroup() %>%
    mutate(prestation = "APA") %>%
    select(prestation,annee,age,nb,recale_gir)
)

# -- vérification de la cohérence ensemble vs. par lieu
#verifage <- age2 %>%
#  filter(grepl("^APA",prestation)) %>%
#  pivot_wider(id_cols=c("annee","age"),names_from="prestation",values_from="nb") %>%
#  mutate(ecart=APA-APAdom-APAetab)
#min(verifage$ecart)
#max(verifage$ecart)

girage2 <- rbind(
  girage %>% mutate(prestation = paste0(prestation,"_",gir)) %>% select(prestation,annee,age,nb),
  girage %>%
    filter(prestation %in% c("APAdom","APAetab")) %>%
    select(annee,age,gir,nb) %>%
    group_by(annee,age,gir) %>% summarise_all(sum) %>% ungroup() %>%
    mutate(prestation = "APA",
           prestation = paste0(prestation,"_",gir)) %>%
    select(prestation,annee,age,nb)
)

girage3 <- bind_rows(
  girage2,
  girage2 %>%
    filter(grepl("^APA.*_GIR[12]$",prestation)) %>%
    mutate(prestation = gsub("GIR[12]$","GIR12",prestation)) %>%
    select(annee,age,prestation,nb) %>%
    group_by(annee,age,prestation) %>% summarise_all(sum) %>% ungroup() %>%
    select(prestation,annee,age,nb),
  girage2 %>%
    filter(grepl("^APA.*_GIR[34]$",prestation)) %>%
    mutate(prestation = gsub("GIR[34]$","GIR34",prestation)) %>%
    select(annee,age,prestation,nb) %>%
    group_by(annee,age,prestation) %>% summarise_all(sum) %>% ungroup() %>%
    select(prestation,annee,age,nb)
)

preval <- rbind( age3, girage3 %>% mutate(recale_gir=FALSE) )

# table complémentaire regroupant toutes les catégories de 85 ans et plus à partir de 2016, pour faire calcul de façon homogène avec les données antérieures à 2016

prevalagecompl <- bind_rows(age3, girage3 %>% mutate(recale_gir=FALSE)) %>%
  filter(annee>=2016, age %in% c("[85,90)","[90,95)","[95,Inf]")) %>%
  select(prestation,annee,nb,recale_gir) %>%
  group_by(prestation,annee,recale_gir) %>% summarise_all(sum) %>% ungroup() %>%
  mutate(age ="[85,Inf]" ) %>%
  select(prestation,annee,age,nb,recale_gir)

# ==================================================================================================
# 3) Calcul des prévalences par tranches d'âge

# récupération des populations par tranche d'âge

popnew <- healthexpectancies::FRInseePopulationForecast2021

pop <- popnew %>%
  filter(sex == "all",type=="observed") %>%
  rename(agefin = age0101,
         annee = year,
         sexe = sex,
         pop = popx0101) %>%
  mutate(annee = annee-1) %>% #pour passer de la population au 01/01/N à celle au 31/12/N-1
  select(-sexe,-geo,-type) %>%
  filter(agefin>=60,annee>=2000) %>%
  group_by(annee,agefin) %>% summarise_all(sum) %>% ungroup()

popagr <- pop  %>%
  mutate(travt = cut(agefin, breaks=c(60,65,70,75,80,85,Inf),include.lowest = TRUE, right = FALSE ),
         trapr = cut(agefin, breaks=c(60,65,70,75,80,85,90,95,Inf),
                     include.lowest = TRUE, right = FALSE ),
         age = ifelse(annee<=2015,as.character(travt),as.character(trapr))) %>%
  select(annee,age,pop) %>%
  group_by(annee,age) %>% summarise_all(sum) %>% ungroup() %>%
  select(annee,age,pop)

#ggplot(popagr %>% filter(age %in% c("[85,90)",  "[90,95)",  "[95,Inf]")),
#ggplot(popagr %>% filter(age %in% c("[95,Inf]")),
#       aes(x=annee,y=pop,colour=age,group=age)) +
#  geom_line() +
#  geom_point()

#ggplot(age %>% filter(age %in% c("[95,Inf]"),grepl("^APA",prestation)),
#       aes(x=annee,y=part,colour=prestation,group=prestation)) +
#  geom_line() +
#  geom_point()

popagrcompl <- pop %>%
  filter(annee>=2016, agefin>=85) %>%
  select(annee,pop) %>%
  group_by(annee) %>% summarise_all(sum) %>% ungroup() %>%
  mutate(age = "[85,Inf]")

# calcul des prévalences

# regroupement en 6 classes d'âge jusqu'en 2015, 8 à partir de 2016
preval <- left_join(preval, popagr, by=c("annee","age")) %>%
  mutate(prevalence = nb/pop)

# regroupement des 85 ans et plus à partir de 2016
prevalcompl <- left_join(prevalagecompl,  popagrcompl, by=c("annee","age")) %>%
  mutate(prevalence = nb/pop)

# effectifs totaux par prestation
efftot <- preval %>%
  select(prestation,annee,recale_gir,nb) %>%
  filter(!is.na(nb)) %>%
  group_by(prestation,recale_gir,annee) %>% summarise_all(sum) %>% ungroup() %>%
  rename(nbtot = nb)

# ensemble des données

agesavant <-  c("[60,65)","[65,70)","[70,75)","[75,80)","[80,85)")

prevalences <- bind_rows(
  preval %>%
    mutate(decomp6age = ((annee<=2015)  | (age %in% agesavant)),
           decomp8age = (annee>=2016)),
  prevalcompl %>%
    mutate(decomp6age = TRUE,
           decomp8age = FALSE)
  ) %>%
  filter(!is.na(nb)) %>%
  left_join(efftot, by = c("annee","prestation","recale_gir")) %>%
  mutate(part = nb / nbtot) %>%
  rename(catprestation = prestation) %>%
  mutate(gir = str_extract(catprestation,"(?<=_)GIR.*$"),
         gir = ifelse(is.na(gir),"Ensemble",gir),
         prestation = gsub("_.*$","",catprestation),
         lieu = case_when(
           grepl("[Dd]om$",prestation) ~ "domicile",
           grepl("[Eeé]tab$",prestation) ~ "établissement",
           (prestation == "ASH") ~ "établissement",
           (prestation == "AidesMenag") ~ "domicile",
           TRUE ~ "ensemble"
         )) %>%
  select(-nbtot,-catprestation)

prevalences <- prevalences[,c("prestation", "gir","lieu",
                              "annee","age",
                              "nb", "pop", "prevalence","part" ,
                              "recale_gir",
                              "decomp6age","decomp8age"
                              )]


ASDEPprevalaidessoc <- prevalences

# ===================================================================================
# Dernière actualisation de la base réalisée le : 24/06/2022

# == historique des versions :
# 10/10/2021 : ajout PSD et ensemble des aides sociales PA
# 17/07/2021 : ajout de la variable "recale_gir" et du calcul de l'APA tous GIR comme sommes de l'APA par GIR
# 20/06/2021 : correction de l'oubli de décalage d'une année de la pop (du 01/01/N+1 au 31/12/N)

usethis::use_data(ASDEPprevalaidessoc,     overwrite = T)
