library(tidyverse)
library(dplyr)
library(magrittr)
library(stats)
library(shiny)
library(shinyWidgets)
library(Hmisc)
library(ggplot2)
library(shinydashboard)
library(plotly)
# devtools::install_github("patrickaubert/asdep",ref='main')
library(asdep)


# === sources et variables

#source(paste(getwd(),"inst/shiny-examples/aidesocialedep/textesappli.R", sep="/"))
source("textesappli.R",encoding="UTF8")

listedepartements <- unique(ASDEPsl[ASDEPsl$TypeTerritoire == "Département", "Territoire"])
listedepartements <- listedepartements[order(listedepartements)]


# === fonction de mise en forme des boîtes (box et tabBox dans le dashboard)

texteintroductif <- function(var,varnum,vardenum) {
  txt <- ""
  #if (!is.null(var)) txt <- paste(txt,ASDEPsl_description[ASDEPsl_description$Nom.var==var,"Note.var"],"<br>",sep="")
  #if (!is.null(varnum)) txt <- paste(txt,"Numérateur : ",ASDEPsl_description[ASDEPsl_description$Nom.var==varnum,"Note.var"],"<br>",sep="")
  #if (!is.null(vardenum)) txt <- paste(txt,"Dénominateur : ",ASDEPsl_description[ASDEPsl_description$Nom.var==vardenum,"Note.var"],"<br>",sep="")
  txtvar <- if (!is.null(var)){ASDEPsl_description[ASDEPsl_description$Nom.var==var,"Note.var"]}else{""}
  txtvarnum <- if (!is.null(varnum)){ASDEPsl_description[ASDEPsl_description$Nom.var==varnum,"Note.var"]}else{""}
  txtvardenum <- if (!is.null(vardenum)){ASDEPsl_description[ASDEPsl_description$Nom.var==vardenum,"Note.var"]}else{""}
  if (nchar(txtvar)>1) txt <- paste(txt,txtvar,"<br>",sep="")
  if (nchar(txtvarnum)>1) txt <- paste(txt,"Numérateur : ",txtvarnum,"<br>",sep="")
  if (nchar(txtvardenum)>1) txt <- paste(txt,"Dénominateur : ",txtvardenum,"<br>",sep="")
  return(txt)
}

boite <- function(titre="",var=NULL,varnum=NULL,vardenum=NULL,graph,collapsed=FALSE) {
  fluidRow(
    box(
      title = titre,
      width=12, collapsible = TRUE, solidHeader = TRUE,  collapsed = collapsed,
      HTML( texteintroductif(var,varnum,vardenum) ),
      column(6, plotlyOutput(paste(graph,"Evol",sep="")) ),
      column(6, plotlyOutput(graph) )
    )
  )
}

multiboite <- function(titre="",id="",var=NULL,varnum=NULL,vardenum=NULL,graph,intitules,collapsed=FALSE) {
  nb <- case_when(
    is.null(var) ~ min(NROW(varnum),NROW(vardenum),NROW(graph),NROW(intitules)),
    is.null(varnum) | is.null(vardenum) ~ min(NROW(var),NROW(graph),NROW(intitules)),
    TRUE ~ min(NROW(var),NROW(varnum),NROW(vardenum),NROW(graph),NROW(intitules))
  )
  if (nb==0) {return()
  } else if (nb == 1) {
    return( boite(titre=titre,var=var[1],varnum=varnum[1],vardenum=vardenum[1],graph=graph[1],collapsed=collapsed) )
  } else if (nb > 1) {
    #tabBoxLoc <- function(...) {  }
    return(
    fluidRow(
      do.call(
        tabBox,
        c(
          title = titre,
          id = id,
          width=12,
          lapply(c(1:nb),
                 function(i) {
                   tabPanel(
                     intitules[i],
                     HTML( texteintroductif(var[i],varnum[i],vardenum[i]) ),
                     fluidRow(
                       column(6, plotlyOutput(paste(graph[i],"Evol",sep="")) ),
                       column(6, plotlyOutput(graph[i]) )
                     )
                   )
                 } # fin function
                 )  # fin lapply
          ) # fin c()
        ) # fin do.call
    ) # fin Fluidrow
  ) # fin du return
  }
}

  # === partie UI de l'application
ui <- dashboardPage(

  skin = "green",

  dashboardHeader(
    title = "Aide sociale des départements",
    titleWidth = 350
  ),

  # ====================================================================
  # options de visualisation

  dashboardSidebar(
    sidebarMenu(
      selectInput("dep", label = NULL, choices = listedepartements, selected = listedepartements[108]),
      fluidRow(
        column(6, align="left", actionButton("dep.moins", label = NULL, icon = icon("caret-left")) ),
        column(6, align="right", actionButton("dep.plus", label = NULL, icon = icon("caret-right")) )
      ),
      menuItem("Perte d'autonomie", tabName = "dependance", icon = icon("bed")),
      menuItem("Handicap", tabName = "handicap", icon = icon("wheelchair")),
      menuItem("ASE", tabName = "ase", icon = icon("child")),
      menuItem("Insertion", tabName = "insertion", icon = icon("hands-helping")),
      menuItem("Minima sociaux", tabName = "minsoc", icon = icon("money-bill-wave")),
      menuItem("Personnels", tabName = "perso", icon = icon("users")),
      menuItem("Paramétrer", tabName = "param", icon = icon("cog")),
      menuItem("Documentation", tabName = "doc", icon = icon("book")),
      menuItem("A propos", tabName = "info", icon = icon("th"))
    ) # fin sidebarMenu
  ), # fin sidebar

  # ====================================================================
  # affichage des résultats

  dashboardBody(
    tabItems(

      # === graphiques et indicateurs

      # perte d'autonomie
      tabItem(
        tabName = "dependance",
        fluidRow(
          tabBox(
            title = "Nb de bénéficiaires de l'APA, en % de la population de 60 ans et plus",
            id = "tabpartAPA", width=12, # collapsible = TRUE, solidHeader = TRUE,
            tabPanel("Ensemble",
                     ASDEPsl_description[ASDEPsl_description$Nom.var=="NbBenefAPA","Note.var"],
                     fluidRow(
                       column(6, plotlyOutput("partAPApopEvol")),
                       column(6, plotlyOutput("partAPApop"))
                     )
            ),
            tabPanel("Domicile",
                     ASDEPsl_description[ASDEPsl_description$Nom.var=="NbBenefAPADomicile","Note.var"],
                     fluidRow(
                       column(6, plotlyOutput("partAPAdompopEvol")),
                       column(6, plotlyOutput("partAPAdompop"))
                     )
            ),
            tabPanel("Etablissement",
                     ASDEPsl_description[ASDEPsl_description$Nom.var=="NbBenefAPAEtab","Note.var"],
                     fluidRow(
                       column(6, plotlyOutput("partAPAetabpopEvol")),
                       column(6, plotlyOutput("partAPAetabpop"))
                     )
            )
          )
        ), # fin fluidRow part APA dans population

        boite(titre = "Proportion de bénéficiaires de l'APA à domicile, en % de l'ensemble des bénéficiaires de l'APA",
              varnum = "NbBenefAPADomicile",
              vardenum = "NbBenefAPA",
              graph = "partAPAdom",
              collapsed = TRUE),
        #fluidRow(
        #  box(
        #    title = "Proportion de bénéficiaires de l'APA à domicile, en % de l'ensemble des bénéficiaires de l'APA",
        #    width=12, collapsible = TRUE, solidHeader = TRUE,  collapsed = TRUE,
        #    ASDEPsl_description[ASDEPsl_description$Nom.var=="NbBenefAPADomicile","Note.var"],
        #    column(6, plotlyOutput("partAPAdomEvol") ),
        #    column(6, plotlyOutput("partAPAdom") )
        #  )
        #), # fin fluidRow ratio ASH / APA
        #fluidRow(
        #  tabBox(
        #    title = "APA",
        #    id = "tabpartAPA", height = "300px",
        #    tabPanel("Par année", HTML("Nb de bénéficiaires, en % de la population totale de 60 ans et plus"), plotlyOutput("partAPApop2")),
        #    tabPanel("Dernière année", HTML("Nb de bénéficiaires, en % de la population totale de 60 ans et plus"))
        #  ),
        #  tabBox(
        #    title = "APA à domicile",
        #    id = "tabpartAPA", height = "300px",
        #    tabPanel("Par année", HTML("Part parmi l'ensemble des bénéficiaires de l'APA (en %)"), plotlyOutput("partAPAdom2")),
        #    tabPanel("Dernière année", HTML("Part parmi l'ensemble des bénéficiaires de l'APA (en %)"))
        #  )
        #), # fin fluidRow expérimental
        fluidRow(
          tabBox(
            title = "Montant moyen d'APA par bénéficiaire, en € par mois",
            id = "tabmontAPA", width=12, # collapsible = TRUE, solidHeader = TRUE,
            tabPanel("Ensemble",
                     ASDEPsl_description[ASDEPsl_description$Nom.var=="DepBruteAPA","Note.var"],
                     fluidRow(
                       column(6, plotlyOutput("montAPAEvol")),
                       column(6, plotlyOutput("montAPA"))
                     )
                     ),
            tabPanel("Domicile",
                     ASDEPsl_description[ASDEPsl_description$Nom.var=="DepBruteAPAdom","Note.var"],
                     fluidRow(
                       column(6, plotlyOutput("montAPAdomEvol")),
                       column(6, plotlyOutput("montAPAdom"))
                      )
            ),
            tabPanel("Etablissement",
                     ASDEPsl_description[ASDEPsl_description$Nom.var=="DepBruteAPAetab","Note.var"],
                     fluidRow(
                       column(6, plotlyOutput("montAPAetabEvol")),
                       column(6, plotlyOutput("montAPAetab"))
                     )
            )
          )
        ), # fin fluidRow montant APA
        fluidRow(
          box(
            title = "Ratio du nb de bénéficiaires de l'ASH sur le nb de bénéficiaires de l'APA en établissement",
            width=12,  collapsible = TRUE, solidHeader = TRUE, collapsed = TRUE,
            ASDEPsl_description[ASDEPsl_description$Nom.var=="NbBenefASH","Note.var"],
            column(6, plotlyOutput("ratioASHAPAEvol") ),
            column(6, plotlyOutput("ratioASHAPA") )
          )
        ), # fin fluidRow ratio ASH / APA
        fluidRow(
          box(
            title = "Montant moyen d'ASH, en € par bénéficiaire",
            width=12,  collapsible = TRUE, solidHeader = TRUE,
            ASDEPsl_description[ASDEPsl_description$Nom.var=="DepNetteASH","Note.var"],
            column(6, plotlyOutput("montASHEvol") ),
            column(6, plotlyOutput("montASH") )
          )
        ), # fin fluidRow montant ASH
        fluidRow(
          box(
            title = "Nb de bénéficiaires d'aides ménagères aux personnes âgées, en % de la population de 60 ans et plus",
            width=12,  collapsible = TRUE, solidHeader = TRUE,
            ASDEPsl_description[ASDEPsl_description$Nom.var=="NbBenefAideMenagerePA","Note.var"],
            column(6, plotlyOutput("partAidesMenPAEvol") ),
            column(6, plotlyOutput("partAidesMenPA") )
          )
        ) # fin fluidRow part Aides ménagères
      ),

      # handicap
      tabItem(
        tabName = "handicap",

        multiboite(
          titre = "Bénéficiaires de la PCH ou de l'ACTP, en % de la population de 60 ans et plus",
          intitules = c("PCH et ACTP","PCH","ACTP"),
          graph = c("partPCHACTPpop","partPCHpop","partACTPpop"),
          var = c("TotBenefACTPPCH","NbBenefPCH","NbBenefACTP"),
          collapsed = TRUE),

        boite(titre = "Proportion de bénéficiaires de l'ACTP, en % de l'ensemble des bénéficiaires de la PCH ou l'ACTP",
              varnum = "NbBenefACTP",
              vardenum = "TotBenefACTPPCH",
              graph = "partACTPpresta",
              collapsed = TRUE),

        multiboite(
          titre = "Dépenses moyennes de PCH et d'ACTP par bénéficiaire, en €",
          intitules = c("PCH","ACTP"),
          graph = c("depmoyPCH","depmoyACTP"),
          varnum = c("DepBrutePCH","DepBruteACTP"),
          vardenum = c("NbBenefPCH","NbBenefACTP"),
          collapsed = TRUE),

        boite(titre = "Bénéficiaires d'aides à l'accueil, en % de la population",
              var = "TotBenefPHEtab.horsACTP",
              graph = "partAcchorsactppop"),

        boite(titre = "Dépenses moyennes d'aide à l'accueil par bénéficiaire, en €",
              varnum = "DepBruteAidesAccueiletabPH",
              vardenum = "NbBenefAideHebergementPH",
              graph = "depmoyAccueiletabPH")

      ), # fin tabItem sur le handicap

      tabItem(
        tabName = "ase",
        fluidRow(
          tabBox(
            title = "Part d'enfants placés dans la population",
            id = "tabplac", width=12, # collapsible = TRUE, solidHeader = TRUE,
            tabPanel(
              "Ensemble",
              ASDEPsl_description[ASDEPsl_description$Nom.var=="TotEnfAccueillisASE","Note.var"],
              fluidRow(column(6, plotlyOutput("partAccueilASEpopEvol")), column(6, plotlyOutput("partAccueilASEpop"))       )
            ),
            tabPanel(
              "Hors placements directs par le juge",
              ASDEPsl_description[ASDEPsl_description$Nom.var=="TotEnfConfiesASE","Note.var"],
              fluidRow(column(6, plotlyOutput("partConfiesASEpopEvol")), column(6, plotlyOutput("partConfiesASEpop")) )
            )
          )
        ), # fin fluidRow part enfants accueillis et confiés
        fluidRow(
          tabBox(
            title = "Proportion des enfants confiés à l'ASE par mode de placement",
            id = "tabplac", width=12, # collapsible = TRUE, solidHeader = TRUE,
            tabPanel(
              "Familles d'accueil",
              ASDEPsl_description[ASDEPsl_description$Nom.var=="TotEnfASEPlacesFamillesAccueil","Note.var"],
              fluidRow(column(6, plotlyOutput("partAssFamEvol")), column(6, plotlyOutput("partAssFam"))       )
            ),
            tabPanel(
              "Etablissements",
              ASDEPsl_description[ASDEPsl_description$Nom.var=="TotEnfASEPlacesEtab","Note.var"],
              fluidRow(column(6, plotlyOutput("partEtabEvol")), column(6, plotlyOutput("partEtab")) )
            )
          )
        ), # fin fluidRow part mode de placement
        fluidRow(
          tabBox(
            title = "Nb de bénéficiaires d'actions éducatives, en % de la population moins de 20 ans",
            id = "tabpartAE", width=12, # collapsible = TRUE, solidHeader = TRUE,
            tabPanel(
              "Ensemble",
              ASDEPsl_description[ASDEPsl_description$Nom.var=="TotBenefAE","Note.var"],
              fluidRow(column(6, plotlyOutput("partAEpopEvol")), column(6, plotlyOutput("partAEpop"))       )
            ),
            tabPanel(
              "AED",
              ASDEPsl_description[ASDEPsl_description$Nom.var=="TotBenefAED","Note.var"],
              fluidRow(column(6, plotlyOutput("partAEDpopEvol")), column(6, plotlyOutput("partAEDpop")) )
            ),
            tabPanel(
              "AEMO",
              ASDEPsl_description[ASDEPsl_description$Nom.var=="TotBenefAEMO","Note.var"],
              fluidRow(column(6, plotlyOutput("partAEMOpopEvol")), column(6, plotlyOutput("partAEMOpop")) )
            )
          )
        ), # fin fluidRow part actions éducatives
        fluidRow(
          tabBox(
            title = "Dépenses d'actions éducatives, en €",
            id = "tabdepAE", width=12, # collapsible = TRUE, solidHeader = TRUE,
            tabPanel(
              "Par bénéficiaire",
              ASDEPsl_description[ASDEPsl_description$Nom.var=="DepBruteAEDAEMO","Note.var"],
              fluidRow(column(6, plotlyOutput("depAEEvol")), column(6, plotlyOutput("depAE"))       )
            ),
            tabPanel(
              "Par enfant du département",
              ASDEPsl_description[ASDEPsl_description$Nom.var=="DepBruteAEDAEMO","Note.var"],
              fluidRow(column(6, plotlyOutput("depAEpopEvol")), column(6, plotlyOutput("depAEpop")) )
            )
          )
        ), # fin fluidRow dépenses actions éducatives
        fluidRow(
          box(
            title = "Dépenses de prévention spécialisée (en €), par enfants dans le département",
            width=12,  collapsible = TRUE, solidHeader = TRUE, collapsed = TRUE,
            ASDEPsl_description[ASDEPsl_description$Nom.var=="DepBrutePrevSpe","Note.var"],
            column(6, plotlyOutput("depprevspeEvol") ),
            column(6, plotlyOutput("depprevspe") )
          )
        ), # fin fluidRow dep prév spé
        fluidRow(
          box(
            title = "Dépenses d'allocations (en €), par enfants dans le département",
            width=12,  collapsible = TRUE, solidHeader = TRUE, collapsed = TRUE,
            ASDEPsl_description[ASDEPsl_description$Nom.var=="DepBrutePrevSpe","Note.var"],
            column(6, plotlyOutput("depallocASEEvol") ),
            column(6, plotlyOutput("depallocASE") )
          )
        ) # fin fluidRow dep alloc ASE
      ),

      tabItem(
        tabName = "insertion",

        boite(titre = "Dépense nette d'allocation et d'insertion par habitant, en €",
              var = "DepNetteInsertion",
              graph = "depmoyAllocIns",
              collapsed = FALSE),

        boite(titre = "Part des dépenses d'allocation dans le total des dépenses d'insertion, en %",
              varnum = "DepBruteRSA",  vardenum = "DepBruteInsertion",
              graph = "partdepAlloc",
              collapsed = FALSE),

        boite(titre = "Part des personnes orientées parmi les personnes soumises aux droits et devoirs, en %",
              var = "OarsaTabB1",
              graph = "partBRSAorientes"),

        boite(titre = "Part des personnes orientées vers Pôle Emploi parmi les bénéficiaires du RSA orientés, en %",
              var = "OarsaTabB5",
              graph = "partBRSAorientesPE"),

        boite(titre = "Part des personnes orientées vers le conseil départemental ou territorial parmi les bénéficiaires du RSA orientés, en %",
              var = "OarsaTabB7",
              graph = "partBRSAorientesCD"),

        boite(titre = "Part des personnes ayant un CER parmi les personnes orientées vers un autre organisme que Pôle Emploi, en %",
              var = "OarsaTabB9",
              graph = "partBRSACER")
      ),

      # === personnels de l'aide sociale départementale
      tabItem(
        tabName = "perso",

        boite(titre = "Total des personnels du département y compris assistants familiaux (effectifs physiques)",
              var = "efftotalgeneralycAF",
              graph = "totalgeneralycAF",
              collapsed = FALSE),
        boite(titre = "Assistants familiaux (effectifs physiques)",
              var = "effpersassfam",
              graph = "persassfam",
              collapsed = FALSE),
        boite(titre = "Total des personnels du département (en ETP), hors assistants familiaux",
              var = "etptotalgeneralhorsAF",
              graph = "totalgeneralhorsAF",
              collapsed = FALSE),
        boite(titre = "Personnel médical (en ETP)",
              var = "etppersmedical",
              graph = "persmedical",
              collapsed = FALSE),
        boite(titre = "Psychologues(en ETP)",
              var = "etppsychologues",
              graph = "psychologues",
              collapsed = FALSE),
        boite(titre = "Personnel paramédical (en ETP)",
              var = "etppersparamed",
              graph = "persparamed",
              collapsed = FALSE),
        boite(titre = "Personnel social et éducatif (en ETP)",
              var = "etpperssocialeteducatif",
              graph = "perssocialeteducatif",
              collapsed = FALSE),
        boite(titre = "Personnel médico-technique (en ETP)",
              var = "etppersmedicotech",
              graph = "persmedicotech",
              collapsed = FALSE),
        boite(titre = "Personnel administratif et technique (en ETP)",
              var = "etppersadminettech",
              graph = "persadminettech",
              collapsed = FALSE)
      ), # fin partie personnels

      # === minima sociaux (à compléter)
      tabItem(
        tabName = "minsoc",

        boite(titre = "Nombre d'allocataires de l'AAH",
              var = "AAH",
              graph = "aah",
              collapsed = FALSE),
        boite(titre = "Nombre d'allocataires de l'ASS",
              var = "ASS",
              graph = "ass",
              collapsed = FALSE)
      ), # fin partie minima sociaux

      # === paramétrage des graphiques affichés
      tabItem(
        tabName = "param",

        fluidRow(
          box(
            title = "Choisir l'année de référence", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
            width = 12,
            sliderInput(
              "anneeref", label = "Année de référence",
              min = min(ASDEPsl$Annee, na.rm = TRUE), max = max(ASDEPsl$Annee, na.rm = TRUE),
              value = max(ASDEPsl$Annee, na.rm = TRUE),
              step=1, animate = FALSE    )
          ) # fin box paramétrage année réf
        ), # fin fluidrow paramétrage année ref

        fluidRow(
          box(
            title = "Choisir le territoire auquel se comparer", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
            width = 12,
            HTML("Vous pouvez choisir le territoire auquel vous souhaitez comparer les données relatives
            au département sélectionné : France entière, région, groupe de département défini de façon <i>ad-hoc</i>.
            Ce territoire de comparaison sera représenté sur tous les graphiques, si les données sont disponibles."),
            br(),br(),
            HTML("Les territoires qui constituent le groupe de comparaison sont les suivants : "),
            htmlOutput("gpecomparaison"),
            br(),br(),
            column(
              4,
              selectInput(
                inputId = "terrcomp", label = "Groupe de comparaison",
                choices = list("France" = "france",
                               "Région" = "region",
                               "Groupe de comparaison ad-hoc" = "choix",
                               "Départements similaires" = "proche"),
                selected = "france",
                width = "100%")
              ),
            column(
              8,
              conditionalPanel(
                condition = "input.terrcomp == 'choix'",
                pickerInput(
                  inputId = "listedepcomp",
                  label = "Départements pour la comparaison",
                  choices = listedepartements,
                  selected = listedepartements[[1]],
                  options = list(    `actions-box` = TRUE,
                                     size = 10,
                                     `selected-text-format` = "count > 4" ,
                                     `count-selected-text` = "{0} départements sélectionnés",
                                     `deselect-all-text` = "Tout désélectionner",
                                     `select-all-text` = "Tout sélectionner"        ),
                  multiple = TRUE   ,
                  width = "100%"   )
                ),
              conditionalPanel(
                condition = "input.terrcomp == 'proche'",
                selectInput("varcomp",
                            label = "... similaires du point de vue de :",
                            choices = setNames(ASDEPsl_description$Nom.var, ASDEPsl_description$Intitule.var),
                            width = "100%"),
                selectInput("denomvarcomp",
                            label = "... exprimé :",
                            choices = list("En niveau" = "nb", "Par habitant" = "popTOT"),
                            width = "100%"),
                sliderInput("nbcomp",
                            label = "Nb de départements similaires retenus :",
                            min = 1, max = 15, value = 5, animate=FALSE, step=1)
                )
            )
          ) # fin box paramétrage territoire comp
        ), # fin fluidrow paramétrage territoire comp

        fluidRow(
          box(
            title = "Afficher des éléments de dispersion", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
            width = 12,
            "Vous pouvez ici définir des zones à afficher systématiquement sur les graphiques : zone interquartile, zone interdécile, etc. Par exemple, la zone interquartile
            correspond à la zone dans laquelle se situent 50 % des départements, en excluant le quart des départements ayant les valeurs les plus faibles
            pour l'indicateur (premier quartile) et le quart ayant les valeurs les plus élevées (dernier quartile).",
            br(),br(),
            checkboxGroupButtons(
              inputId = "affichedispers",
              label = "Afficher des éléments de distribution",
              choices = list("Zone interquartile [Q1; Q3]" = "interquartiles",
                             "Zone interdécile [D1; D9]" = "interdeciles",
                             "Zone interquartile (pondérée par la population départementale)" = "interquartilespond",
                             "Zone interdécile (pondérée par la population départementale)" = "interdecilespond",
                             "Zone médiane +/- 10 %" = "medianePM10",
                             "Zone médiane +/- 20 %" = "medianePM20"),
              selected = c("interdeciles","interquartiles"),
              direction = "vertical",
              justified = TRUE,
              checkIcon = list(yes = icon("check")),
              width = '70%'
              )
          ) # fin box paramétrage éléments dispersion affichés
        ), # fin fluidRow  paramétrage éléments dispersion affichés

        fluidRow(
          box(
            title = "Afficher les graphiques en base 100", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
            width = 12
          ) # fin box paramétrage graphiques en base 100
        ), # fin fluidRow  paramétrage graphiques en base 100

      ), # fin tabItem paramétrage


      # === documentation de l'appli
      tabItem(
        tabName = "doc",
        fluidRow(
          box(
            title = "Fonctionnement de l'application", solidHeader = TRUE,  collapsible = TRUE, collapsed = FALSE,
            width = 12,
            HTML(txt.outil)
          ) # fin box texte outil
        ), # fin fluidRow  texte outil
        fluidRow(
          box(
            title = "Références bibliographiques", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
            width = 12,
            HTML(txt.biblio)
          ) # fin box texte réf
        ), # fin fluidRow  texte réf
        fluidRow(
          box(
            title = "Sources statistiques", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
            width = 12,
            HTML(txt.sourcesstat)
          ) # fin box sources stats
        ), # fin fluidRow sources stats
        fluidRow(
          box(
            title = "L'aide sociale des départements", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
            width = 12,
            HTML(txt.themaidesoc)
          ) # fin box texte aide sociel
        ), # fin fluidRow  texte aide sociale
        fluidRow(
          box(
            title = "Les minima sociaux", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
            width = 12,
            HTML(txt.themMS)
          ) # fin box texte minima sociaux
        ) # fin fluidRow minima sociaux
      ),# fin tabItem  documentation

      tabItem(
        tabName = "info",
        HTML("Cette application fait partie du package R <i>asdep</i>, dont elle illustre l'utilisation.
             Le package peut être téléchargé sous <a href='https://github.com/patrickaubert/asdep'>https://github.com/patrickaubert/asdep</a>,
             et l'application utilisée en local par la commande <i>asdep::runExample()</i>.
             <br><br>
             Toutes les données présentées dans l'application sont des données publiques diffusées
             par la <a href='https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/'>DREES</a> ou par l'<a href='https://www.insee.fr/fr/accueil'>Insee</a>.
             Le code source de l'application et du package <i>asdep</i> ont
             été développés par <a href='https://sites.google.com/site/patrickauber/'>Patrick Aubert</a>
             et sont diffusés sous la license <a href='https://eur-lex.europa.eu/legal-content/FR/TXT/?uri=uriserv:OJ.L_.2017.128.01.0059.01.FRA&toc=OJ:L:2017:128:TOC'>EUPL</a>.
             Ces développements sont encore en cours et des erreurs peuvent subsister.")
      )
    ) # fin tabItems
  ) #fin body
) # fin page
