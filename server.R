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
  projectmodel2B <- read.xlsx2('model/projet.xlsx', sheetIndex = 3, header = TRUE, stringsAsFactors = FALSE)
  projectmodel2C <- read.xlsx2('model/projet.xlsx', sheetIndex = 4, header = TRUE, stringsAsFactors = FALSE)
  projectmodel2D <- read.xlsx2('model/projet.xlsx', sheetIndex = 5, header = TRUE, stringsAsFactors = FALSE)
  projectmodel3 <- read.xlsx2('model/projet.xlsx', sheetIndex = 6, header = TRUE, stringsAsFactors = FALSE)
  projectmodel4 <- read.xlsx2('model/projet.xlsx', sheetIndex = 7, header = TRUE, stringsAsFactors = FALSE)
  prj <- projectmodel1[4:10,]
  prjenjeuA <- projectmodel2A[2:6,]
  prjenjeuB <- projectmodel2B[2:5,]
  prjenjeuC <- projectmodel2C[2:7,]
  prjenjeuD <- projectmodel2D[2:6,]

  
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
        projectmodel4 <<- read.xlsx2(input$userfile$datapath, sheetIndex = 7, header = TRUE, stringsAsFactors = FALSE)
        updateTextInput(session, "projectname", value=projectmodel1[1,2])
        updateTextInput(session, "projectcontext", value=projectmodel1[2,2])
        if(is.numeric(projectmodel1[3,2])){
          updateNumericInput(session, "sitenumber", value=projectmodel1[3,2])
        }
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
  
  
  output$enjeuxtabA <- DT::renderDataTable({
    if(!is.null(input$userfile)){
      if(fileloaded != ""){
        projectmodel2A <<- read.xlsx2(input$userfile$datapath, sheetIndex = 2, header = TRUE, stringsAsFactors = FALSE)
        if(is.numeric(projectmodel2A[1,2])){
          updateNumericInput(session, "nbhabimp", value=projectmodel2A[1,2])
        }
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
  
  output$enjeuxtabB <- DT::renderDataTable({
    if(!is.null(input$userfile)){
      if(fileloaded != ""){
        projectmodel2B <<- read.xlsx2(input$userfile$datapath, sheetIndex = 3, header = TRUE, stringsAsFactors = FALSE)
        if(is.numeric(projectmodel2B[1,2])){
          updateNumericInput(session, "nbespimp", value=projectmodel2B[1,2])
        }
      }
    }
    projectmodel2B[1,2] <<- input$nbespimp
    # append new fields or remove if necessary
    if(!is.na(input$nbespimp)){
      nesp <- input$nbespimp
      nprj <- dim(prjenjeuB)[1]
      nproj <- dim(projectmodel2B)[1] - 1
      niter <- ((nesp * nprj) - nproj) / nprj
      if(niter > 0){
        for(i in 1:niter){
          projectmodel2B <<- rbind(projectmodel2B, prjenjeuB)
        }
      }else if(niter < 0){
        for(i in -1:niter){
          projectmodel2B <<- projectmodel2B[1:(dim(projectmodel2B)[1]-nprj),]
        }
      }
      DT::datatable(projectmodel2B[2:dim(projectmodel2B)[1],], options = list(pageLength = 4, autoWidth = TRUE, server = F), rownames = FALSE, selection = 'none', editable = T )
    }
  })
        
  output$enjeuxtabC <- DT::renderDataTable({
    if(!is.null(input$userfile)){
      if(fileloaded != ""){
        projectmodel2C <<- read.xlsx2(input$userfile$datapath, sheetIndex = 4, header = TRUE, stringsAsFactors = FALSE)
        if(is.numeric(projectmodel2C[1,2])){
          updateNumericInput(session, "nbhabcomp", value=projectmodel2C[1,2])
        }
      }
    }
    projectmodel2C[1,2] <<- input$nbhabcomp
    # append new fields or remove if necessary
    if(!is.na(input$nbhabcomp)){
      nhab <- input$nbhabcomp
      nprj <- dim(prjenjeuC)[1]
      nproj <- dim(projectmodel2C)[1] - 1
      niter <- ((nhab * nprj) - nproj) / nprj
      if(niter > 0){
        for(i in 1:niter){
          projectmodel2C <<- rbind(projectmodel2C, prjenjeuC)
        }
      }else if(niter < 0){
        for(i in -1:niter){
          projectmodel2C <<- projectmodel2C[1:(dim(projectmodel2C)[1]-nprj),]
        }
      }
      DT::datatable(projectmodel2C[2:dim(projectmodel2C)[1],], options = list(pageLength = 6, autoWidth = TRUE, server = F), rownames = FALSE, selection = 'none', editable = T )
    }
  })
  
  output$enjeuxtabD <- DT::renderDataTable({
    if(!is.null(input$userfile)){
      if(fileloaded != ""){
        projectmodel2D <<- read.xlsx2(input$userfile$datapath, sheetIndex = 5, header = TRUE, stringsAsFactors = FALSE)
        if(is.numeric(projectmodel2D[1,2])){
          updateNumericInput(session, "nbespcomp", value=projectmodel2D[1,2])
        }
      }
    }
    projectmodel2D[1,2] <<- input$nbespcomp
    # append new fields or remove if necessary
    if(!is.na(input$nbespcomp)){
      nesp <- input$nbespcomp
      nprj <- dim(prjenjeuD)[1]
      nproj <- dim(projectmodel2D)[1] - 1
      niter <- ((nesp * nprj) - nproj) / nprj
      if(niter > 0){
        for(i in 1:niter){
          projectmodel2D <<- rbind(projectmodel2D, prjenjeuD)
        }
      }else if(niter < 0){
        for(i in -1:niter){
          projectmodel2D <<- projectmodel2D[1:(dim(projectmodel2D)[1]-nprj),]
        }
      }
      DT::datatable(projectmodel2D[2:dim(projectmodel2D)[1],], options = list(pageLength = 5, autoWidth = TRUE, server = F), rownames = FALSE, selection = 'none', editable = T )
    }
  })
                                    
  output$synthtab <- DT::renderDataTable(projectmodel3, rownames = FALSE, selection = 'none', editable = T, options=list(pageLength = 20))
  
  output$ssitab <- DT::renderDataTable(projectmodel4, rownames = FALSE, selection = 'none', editable = T)

  proxy1 = dataTableProxy('projecttab')
  proxy2A = dataTableProxy('enjeuxtabA')
  proxy2B = dataTableProxy('enjeuxtabB')
  proxy2C = dataTableProxy('enjeuxtabC')
  proxy2D = dataTableProxy('enjeuxtabD')
  proxy4 = dataTableProxy('ssitab')

  observeEvent(input$projecttab_cell_edit, {
    info = input$projecttab_cell_edit
    i = info$row + 3
    j = info$col + 1
    v = info$value
    projectmodel1[i, j] <<- DT::coerceValue(v, projectmodel1[i, j])
    replaceData(proxy1, projectmodel1[4:dim(projectmodel1)[1],], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  observeEvent(input$enjeuxtabA_cell_edit, {
    info = input$enjeuxtabA_cell_edit
    i = info$row + 1
    j = info$col + 1
    v = info$value
    projectmodel2A[i, j] <<- DT::coerceValue(v, projectmodel2A[i, j])
    replaceData(proxy2A, projectmodel2A[2:dim(projectmodel2A)[1],], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  observeEvent(input$enjeuxtabB_cell_edit, {
    info = input$enjeuxtabB_cell_edit
    i = info$row + 1
    j = info$col + 1
    v = info$value
    projectmodel2B[i, j] <<- DT::coerceValue(v, projectmodel2B[i, j])
    replaceData(proxy2B, projectmodel2B[2:dim(projectmodel2B)[1],], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  observeEvent(input$enjeuxtabC_cell_edit, {
    info = input$enjeuxtabC_cell_edit
    i = info$row + 1
    j = info$col + 1
    v = info$value
    projectmodel2C[i, j] <<- DT::coerceValue(v, projectmodel2C[i, j])
    replaceData(proxy2C, projectmodel2C[2:dim(projectmodel2C)[1],], resetPaging = FALSE, rownames = FALSE)  # important
  })
  
  observeEvent(input$enjeuxtabD_cell_edit, {
    info = input$enjeuxtabD_cell_edit
    i = info$row + 1
    j = info$col + 1
    v = info$value
    projectmodel2D[i, j] <<- DT::coerceValue(v, projectmodel2D[i, j])
    replaceData(proxy2D, projectmodel2D[2:dim(projectmodel2D)[1],], resetPaging = FALSE, rownames = FALSE)  # important
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
        write.xlsx2(projectmodel2B, con, sheetName = 'Identification enjeux B', row.names = FALSE, append = TRUE)
        write.xlsx2(projectmodel2C, con, sheetName = 'Identification enjeux C', row.names = FALSE, append = TRUE)
        write.xlsx2(projectmodel2D, con, sheetName = 'Identification enjeux D', row.names = FALSE, append = TRUE)
        write.xlsx2(projectmodel3, con, sheetName = 'Tableau synthétique', row.names = FALSE, append = TRUE)
        write.xlsx2(projectmodel4, con, sheetName = 'SSI', row.names = FALSE, append = TRUE)
      }
  )
  
})
