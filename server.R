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
  model_info_general <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 1, header = FALSE, stringsAsFactors = FALSE)
  model_site <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 2, header = FALSE, stringsAsFactors = FALSE)
  model_species <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 3, header = FALSE, stringsAsFactors = FALSE)
  model_habitat <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 4, header = FALSE, stringsAsFactors = FALSE)
  ecoval <- list()
  ecoval[["General"]] <- model_info_general
  numsite <- 0
  numspecies <- 0
  numhabitat <- 0
  listsite <- data.frame("site" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
  listspecies <- data.frame("species" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
  listhabitat <- data.frame("habitat" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
  
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
  
  # output$projectmap <- renderLeaflet({
  #   if(is.numeric(input$latitude) & is.numeric(input$longitude)){
  #     if(input$latitude != 0. | input$longitude !=0.){
  #       m <- leaflet() %>%
  #         addTiles() %>%
  #         addMarkers(lat=input$latitude , lng=input$longitude, popup=input$projectcontext)
  #       m
  #     }  
  #   } 
  # })

})
