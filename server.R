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

  fileloaded <- ""
  projectmodel1 <- read.xlsx2('model/projet.xlsx', sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE)
  projectmodel2A <- read.xlsx2('model/projet.xlsx', sheetIndex = 2, header = TRUE, stringsAsFactors = FALSE)
  projectmodel4 <- read.xlsx2('model/projet.xlsx', sheetIndex = 7, header = TRUE, stringsAsFactors = FALSE)
  prj <- projectmodel1[4:10,]
  prjenjeuA <- projectmodel2A[2:6,]

  
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
    if(!is.null(input$userfile)){
      if(fileloaded != input$userfile$name){
        projectmodel1 <<- read.xlsx2(input$userfile$datapath, sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE)
        projectmodel4 <<- read.xlsx2(input$userfile$datapath, sheetIndex = 7, header = TRUE, stringsAsFactors = FALSE) #!!!! CORRIGER INDEX
        updateTextInput(session, "projectname", value=projectmodel1[1,2])
        updateTextInput(session, "projectcontext", value=projectmodel1[2,2])
        updateNumericInput(session, "sitenumber", value=projectmodel1[3,2])
        fileloaded <<- input$userfile$name
      }
    }
    projectmodel1[1,2] <<- input$projectname
    projectmodel1[2,2] <<- input$projectcontext
    projectmodel1[3,2] <<- input$sitenumber
    # append new fields or remove if necessary
    if(!is.na(input$sitenumber)){
      nsite <- input$sitenumber
      nprj <- dim(prj)[1]
      nproj <- dim(projectmodel1)[1] - 3
      niter <- ((nsite * nprj) - nproj) / nprj
      if(niter > 0){
        for(i in 1:niter){
          projectmodel1 <<- rbind(projectmodel1, prj)
        }
      }else if(niter < 0){
        for(i in -1:niter){
          projectmodel1 <<- projectmodel1[1:(dim(projectmodel1)[1]-nprj),]
        }
      }
      DT::datatable(projectmodel1[4:dim(projectmodel1)[1],], options = list(pageLength = 7, autoWidth = TRUE, server = F), rownames = FALSE, selection = 'none', editable = T )
    }
  })
  
  
  output$enjeuxtab <- DT::renderDataTable({   #projectmodel2, rownames = FALSE, selection = 'none', editable = T)
    if(!is.null(input$userfile)){
      if(fileloaded != ""){
        projectmodel2A <<- read.xlsx2(input$userfile$datapath, sheetIndex = 2, header = TRUE, stringsAsFactors = FALSE)
        updateNumericInput(session, "nbhabimp", value=projectmodel2A[2,2])
      }
    }
    projectmodel2A[1,2] <<- input$nbhabimp
    # append new fields or remove if necessary
    if(!is.na(input$nbhabimp)){
      nhab <- input$nbhabimp
      nprj <- dim(prjenjeuA)[1]
      nproj <- dim(projectmodel2A)[1] - 1
      niter <- ((nhab * nprj) - nproj) / nprj
      if(niter > 0){
        for(i in 1:niter){
          projectmodel2A <<- rbind(projectmodel2A, prjenjeuA)
        }
      }else if(niter < 0){
        for(i in -1:niter){
          projectmodel2A <<- projectmodel2A[1:(dim(projectmodel2A)[1]-nprj),]
        }
      }
      DT::datatable(projectmodel2A[2:dim(projectmodel2A)[1],], options = list(pageLength = 5, autoWidth = TRUE, server = F), rownames = FALSE, selection = 'none', editable = T )
    }
  })
                                          
  output$ssitab <- DT::renderDataTable(projectmodel4, rownames = FALSE, selection = 'none', editable = T)
  
  
  
  proxy1 = dataTableProxy('projecttab')
  proxy2A = dataTableProxy('enjeuxtab')
  proxy4 = dataTableProxy('ssitab')

  observeEvent(input$projecttab_cell_edit, {
    info = input$projecttab_cell_edit
    i = info$row + 3
    j = info$col + 1
    v = info$value
    projectmodel1[i, j] <<- DT::coerceValue(v, projectmodel1[i, j])
    replaceData(proxy1, projectmodel1[4:dim(projectmodel1)[1],], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  observeEvent(input$enjeuxtab_cell_edit, {
    info = input$enjeuxtab_cell_edit
    i = info$row + 1
    j = info$col + 1
    v = info$value
    projectmodel2A[i, j] <<- DT::coerceValue(v, projectmodel2A[i, j])
    replaceData(proxy2A, projectmodel2A[2:dim(projectmodel2A)[1],], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  observeEvent(input$ssitab_cell_edit, {
    info = input$ssitab_cell_edit
    i = info$row
    j = info$col + 1
    v = info$value
    projectmodel4[i, j] <<- DT::coerceValue(v, projectmodel4[i, j])
    replaceData(proxy4, projectmodel4, resetPaging = FALSE, rownames = FALSE)  # important
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
        write.xlsx2(projectmodel1, con, sheetName = 'Caractéristiques projet', row.names = FALSE)
        write.xlsx2(projectmodel2A, con, sheetName = 'Identification enjeux A', row.names = FALSE, append = TRUE)
        write.xlsx2(projectmodel4, con, sheetName = 'SSI', row.names = FALSE, append = TRUE)
      }
  )
  
})
