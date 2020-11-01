server <- function(input, output, session) {

  # ========================================================
  # mise en forme des graphiques dans l'appli

  ggplotlocal <- function(...) {
    ggplot(...)
    # to be done: ajout mis en forme
  }

  # ========================================================
  # actualisation des widgets

  observeEvent(input$dep.plus, {
    newval <- max(1, min(NROW(listedepartements), (match(input$dep,listedepartements)+1) ))
    updateSelectInput(session, "dep",
                      label = NULL, #Département",
                      choices = listedepartements,
                      selected = listedepartements[newval] )
  })

  observeEvent(input$dep.moins, {
    newval <- max(1, min(NROW(listedepartements), (match(input$dep,listedepartements)-1) ))
    updateSelectInput(session, "dep",
                      label = NULL, #Département",
                      choices = listedepartements,
                      selected = listedepartements[newval] )
  })


  # ========================================================
  # Graphiques

  # === Perte d'autonomie

  # -- part des bénéficiaires de l'APA dans la population de 60 ans et plus, en série temporelle
  output$partAPApop <- renderPlotly({
    tab <- selectIndic(
      nomvariable="NbBenefAPA",denom="pop.60.99",
      gpeDpt = c(), options="")$var %>%
      filter(Territoire %in% c(input$dep,"TOTAL estimé France entière (hors Mayotte)"))
    g <- ggplotlocal(tab , aes(x=Annee,y=NbBenefAPA,colour=Territoire) ) +
      geom_line()
    ggplotly(g)
  })
  # -- part des bénéficiaires de l'APA à domicile dans l'ensemble des bénéficiaires de l'APA, en série temporelle
  output$partAPAdom <- renderPlotly({
    tab <- selectIndic(
      nomvariable="NbBenefAPADomicile",denom="NbBenefAPA",
      gpeDpt = c(), options="")$var %>%
      filter(Territoire %in% c(input$dep,"TOTAL estimé France entière (hors Mayotte)"))
    g <- ggplotlocal(tab , aes(x=Annee,y=NbBenefAPADomicile,colour=Territoire) ) +
      geom_line()
    ggplotly(g)
  })



}
