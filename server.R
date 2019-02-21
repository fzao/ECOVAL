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

library(shiny)
library(plotly)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  projectmodel <- read.xlsx2('model/projet.xlsx', sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE)
  prj <- projectmodel[4:10,]
  
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
  
  output$projecttab <- DT::renderDataTable({
    projectmodel[1,2] <<- input$projectname
    projectmodel[2,2] <<- input$projectcontext
    projectmodel[3,2] <<- input$sitenumber
    # append new fiels if necessary
    if(!is.na(input$sitenumber)){
      nsite <- input$sitenumber
      nprj <- dim(prj)[1]
      nproj <- dim(projectmodel)[1] - 3
      niter <- ((nsite * nprj) - nproj) / nprj
      if(niter > 0){
        for(i in 1:niter){
          projectmodel <<- rbind(projectmodel, prj)
        }
      }
      DT::datatable(projectmodel[4:dim(projectmodel)[1],], options = list(pageLength = 7, autoWidth = TRUE), rownames = FALSE)
    }
  })
  
  output$btn_telecharger <- downloadHandler(
      filename = function() {
        if(input$projectname == ""){
          paste('Ecoval-', Sys.Date(), '.xlsx', sep='')
        }else{
          paste(input$projectname, '.xlsx', sep='')
        }
      },
      content = function(con) {
        write.xlsx2(projectmodel, con, sheetName = 'Caractéristiques projet', row.names = FALSE)
      }
  )
  
})
