library(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(plotly)
library(asdep)

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
          tabBox(
            title = "APA",
            id = "tabpartAPA", height = "300px",
            tabPanel("Par année", HTML("Nb de bénéficiaires, en % de la population totale de 60 ans et plus"), plotlyOutput("partAPApop")),
            tabPanel("Dernière année", HTML("Nb de bénéficiaires, en % de la population totale de 60 ans et plus"))
          ),
          tabBox(
            title = "APA à domicile",
            id = "tabpartAPA", height = "300px",
            tabPanel("Par année", HTML("Part parmi l'ensemble des bénéficiaires de l'APA (en %)"), plotlyOutput("partAPAdom")),
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
      tabItem(
        tabName = "param",
      ),
      tabItem(
        tabName = "doc",
      ),
      tabItem(
        tabName = "info",
      )
    ) # fin tabItems
  ) #fin body
) # fin page
