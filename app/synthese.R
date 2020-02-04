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
# # Copyright (c) EDF-INRAE 2019-2020
#
# Auteurs : Fabrice Zaoui - Lucie Bezombes
#
# Licence CeCILL v2.1
#

output$genere <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', 'html')
  },
  
  content = function(file) {
    showModal(modalDialog(h5("GENERATION DU RAPPORT EN COURS"), hr(), "Veuillez patienter svp ceci peut prendre du temps !", footer = NULL))
    src <- normalizePath('md/rapport.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'rapport.Rmd', overwrite = TRUE)
    #shinyjs::disable("genere")
    out <- rmarkdown::render('rapport.Rmd', html_document())
    #shinyjs::enable("genere")
    file.rename(out, file)
    removeModal()
  }
)

observeEvent(input$selectsiteimpact4, {
  if(input$selectsitecompens4 == '0'){
    shinyjs::hide("genere")
  }else{
    if(input$selectsiteimpact4 == '0'){
      shinyjs::hide("genere")
    }else{
      shinyjs::show("genere")
    }
  }
})

observeEvent(input$selectsitecompens4, {
  if(input$selectsiteimpact4 == '0'){
    shinyjs::hide("genere")
  }else{
    if(input$selectsitecompens4 == '0'){
      shinyjs::hide("genere")
    }else{
      shinyjs::show("genere")
    }
  }
})

