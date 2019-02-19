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
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(df)
  })
})
