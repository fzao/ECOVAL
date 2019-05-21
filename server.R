#
#   ______ _____ ______      __     _      
#  |  ____/ ____/ __ \ \    / /\   | |     
#  | |__ | |   | |  | \ \  / /  \  | |     
#  |  __|| |   | |  | |\ \/ / /\ \ | |     
#  | |___| |___| |__| | \  / ____ \| |____ 
#  |______\_____\____/   \/_/    \_\______|
#
# Cadre methodologique pour le calcul de l'equivalence ecologique dans le contexte de la sequence ERC en France
#
# # Copyright (c) EDF-IRSTEA 2019
#
# Auteurs : Fabrice Zaoui - Lucie Bezombes
#
# Licence CeCILL v2.1
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  source('site.R', local =TRUE)
  source('species.R', local = TRUE)
  source('habitat.R', local = TRUE)
  source('init.R', local = TRUE)
  
  observeEvent(input$redir1, {
    updateTabsetPanel(session, "tabs", selected = "projet")
  })
  
  observeEvent(input$redir2, {
    updateTabsetPanel(session, "tabs", selected = "impact")
  })
  
  observeEvent(input$redir3, {
    updateTabsetPanel(session, "tabs", selected = "impact")
  })
  
  observeEvent(input$redir4, {
    updateTabsetPanel(session, "tabs", selected = "compens")
  })
  
  observeEvent(input$redir5, {
    updateTabsetPanel(session, "tabs", selected = "equival")
  })
  
  observeEvent(input$redir6, {
    updateTabsetPanel(session, "tabs", selected = "synth")
  })
  
  observeEvent(input$redir7, {
    updateTabsetPanel(session, "tabs", selected = "propos")
  })
  
  output$projectmap <- renderLeaflet({
    if(input$latitude != 0 | input$longitude != 0){
        m <- leaflet() %>%
          addTiles() %>%
          addMarkers(lat=input$latitude , lng=input$longitude, popup=input$sitename)
        m
    }else{
      pk <- sample(1:dim(park)[1], 1) 
      m <- leaflet() %>%
        addTiles() %>%
        addMarkers(lat=park[pk, 2] , lng=park[pk,3], popup=park[pk,1])
    }
  })

  # observe({
  #   if(input$descrimpact == "impactindicnh") updateSelectInput(session, "selecthabitatSI", selected = "0")
  #   if(input$descrimpact == "impactindicnsp") updateSelectInput(session, "selectspeciesSI", selected = "0")
  # })
})
