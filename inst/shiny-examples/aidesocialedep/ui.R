library(tidyverse)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(Hmisc)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(asdep)

#source(paste(getwd(),"inst/shiny-examples/aidesocialedep/textesappli.R", sep="/"))
source("textesappli.R",encoding="UTF8")


listedepartements <- unique(ASDEPsl[ASDEPsl$TypeTerritoire == "Département", "Territoire"])
listedepartements <- listedepartements[order(listedepartements)]



  # === partie UI de l'application
ui <- dashboardPage(

  dashboardHeader(
    title = "Aide sociale des départements",
    titleWidth = 350
  ),

  # ====================================================================
  # options de visualisation

  dashboardSidebar(
    sidebarMenu(
      selectInput("dep", label = NULL, choices = listedepartements, selected = listedepartements[101]),
      fluidRow(
        column(6, align="left", actionButton("dep.moins", label = NULL, icon = icon("caret-left")) ),
        column(6, align="right", actionButton("dep.plus", label = NULL, icon = icon("caret-right")) )
      ),
      menuItem("Perte d'autonomie", tabName = "dependance", icon = icon("bed")),
      menuItem("Handicap", tabName = "handicap", icon = icon("wheelchair")),
      menuItem("ASE", tabName = "ase", icon = icon("child")),
      menuItem("Insertion", tabName = "insertion", icon = icon("hands-helping")),
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
      tabItem(
        tabName = "dependance",
        fluidRow(
          box(
            title = "Nb de bénéficiaires de l'APA, en % de la population de 60 ans et plus",
            width=12, collapsible = TRUE, solidHeader = TRUE,
            column(6, plotlyOutput("partAPApop")),
            column(6, plotlyOutput("partAPAdom"))
          )
        ), # fin fluidRow part APA dans population
        fluidRow(
          tabBox(
            title = "APA",
            id = "tabpartAPA", height = "300px",
            tabPanel("Par année", HTML("Nb de bénéficiaires, en % de la population totale de 60 ans et plus"), plotlyOutput("partAPApop2")),
            tabPanel("Dernière année", HTML("Nb de bénéficiaires, en % de la population totale de 60 ans et plus"))
          ),
          tabBox(
            title = "APA à domicile",
            id = "tabpartAPA", height = "300px",
            tabPanel("Par année", HTML("Part parmi l'ensemble des bénéficiaires de l'APA (en %)"), plotlyOutput("partAPAdom2")),
            tabPanel("Dernière année", HTML("Part parmi l'ensemble des bénéficiaires de l'APA (en %)"))
          )

        )
      ),
      tabItem(
        tabName = "handicap",
      ),
      tabItem(
        tabName = "ase",
      ),
      tabItem(
        tabName = "insertion",
      ),

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
            au département sélectionn : France entière, région, groupe de département défini de façon <i>ad-hoc</i>.
            Ce territoire de comparaison sera représenté sur tous les graphiques, si les données sont disponibles."),
            br(),br(),
            column(
              4,
              selectInput(
                inputId = "terrcomp", label = "Groupe de comparaison",
                choices = list("France" = "france",
                               "Région" = "region",
                               "Groupe de comparaison ad-hoc" = "choix"),
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
                )#,
              #conditionalPanel(
              #  condition = "input.comp == 'proche'",
              #  selectInput("varcomp",
              #              #label = input$var,
              #              label = "... similaires du point de vue de :",
              #              choices = listenomsvariablescontexte,
              #              selected = listenomsvariablescontexte[[1]],
              #              width = "100%")        )
#
              #)
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
              selected = c("interdeciles"),
              direction = "vertical",
              justified = TRUE,
              checkIcon = list(yes = icon("check")),
              width = '70%'
              )
          ) # fin box paramétrage éléments dispersion affichés
        ) # fin fluidRow  paramétrage éléments dispersion affichés
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
      )
    ) # fin tabItems
  ) #fin body
) # fin page
