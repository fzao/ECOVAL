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
# Copyright (c) EDF-INRAE 2019-2021
#
# Auteurs : Fabrice Zaoui - Lucie Bezombes
#
# Licence CeCILL v2.1
#

# Define server logic
shinyServer(function(input, output, session) {
  
  source('init.R', local = TRUE)
  source('site.R', local =TRUE)
  source('species.R', local = TRUE)
  source('habitat.R', local = TRUE)
  source('synthese.R', local = TRUE)
  
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
    updateTabsetPanel(session, "tabs", selected = "compens")
  })
  
  observeEvent(input$redir6, {
    updateTabsetPanel(session, "tabs", selected = "calculs")
  })
  
  observeEvent(input$redir7, {
    updateTabsetPanel(session, "tabs", selected = "propos")
  })
  
  output$projectmap <- renderLeaflet({
    if(!is.numeric(input$latitude)) lat = 0
    else lat = input$latitude
    if(!is.numeric(input$longitude)) long = 0
    else long = input$longitude
    if(lat != 0 | long != 0){
        m <- leaflet() %>%
          addTiles() %>%
          addMarkers(lat = lat, lng =long, popup = paste("Surface", toString(input$surface), "ha"),  label = paste("Site", input$sitename))
        m
    }else{
      pk <- sample(1:dim(park)[1], 1) 
      m <- leaflet() %>%
        addTiles() %>%
        addMarkers(lat=park[pk, 2] , lng = park[pk,3], popup = park[pk,1])
    }
  })
  
  observe({  click = input$projectmap_click
    if(is.null(click))
      return()
    else{
      click= data.frame(click[1:2])
      updateNumericInput(session, "latitude", value = click[[1]])
      updateNumericInput(session, "longitude", value = click[[2]])
    }
  })

})
