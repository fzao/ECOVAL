#
#   ______ _____ ______      __     _      
#  |  ____/ ____/ __ \ \    / /\   | |     
#  | |__ | |   | |  | \ \  / /  \  | |     
#  |  __|| |   | |  | |\ \/ / /\ \ | |     
#  | |___| |___| |__| | \  / ____ \| |____ 
#  |______\_____\____/   \/_/    \_\______|
#
# Cadre méthodologique pour le calcul de l\'équivalence écologique dans le contexte de la séquence ERC en France
#
# # Copyright (c) EDF-IRSTEA 2019
#
# Auteurs : Fabrice Zaoui - Lucie Bezombes
#
# Licence CeCILL v2.1
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  fileloaded <- ""
  projectmodel1 <- read.xlsx2('model/projet.xlsx', sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE)

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
  
  output$viewsiteno <- renderText({ paste("<font color=\"#000000\"; size=\"+1\"><b>", "SITE NUMERO", "</b></font>", "<font color=\"#000000\"; size=\"+1\"><b>", input$selectsite, "</b></font>") })
  
  output$btn_telecharger <- downloadHandler(
      filename = function() {
        if(input$projectname == ""){
          paste('Ecoval-', Sys.Date(), '.xlsx', sep='')
        }else{
          paste(input$projectname, '.xlsx', sep='')
        }
      },
      content = function(con) {
        write.xlsx2(projectmodel1, con, sheetName = 'Caractéristiques projet', row.names = FALSE)
        #write.xlsx2(projectmodel2A, con, sheetName = 'Identification enjeux A', row.names = FALSE, append = TRUE)
      }
  )
  
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
