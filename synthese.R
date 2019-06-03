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

output$genere <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  
  content = function(file) {
    src <- normalizePath('md/rapport.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'rapport.Rmd', overwrite = TRUE)
    
    out <- rmarkdown::render('rapport.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)

observeEvent(input$selectsitecompens4, {
  if(input$selectsitecompens4 == '0'){
    shinyjs::hide("format")
    shinyjs::hide("genere")
  }else{
    if(input$selectsiteimpact4 == '0'){
      shinyjs::hide("format")
      shinyjs::hide("genere")
    }else{
      shinyjs::show("format")
      shinyjs::show("genere")
    }
  }
})

observeEvent(input$selectsiteimpact4, {
  if(input$selectsiteimpact4 == '0'){
    shinyjs::hide("format")
    shinyjs::hide("genere")
  }else{
    if(input$selectsitecompens4 == '0'){
      shinyjs::hide("format")
      shinyjs::hide("genere")
    }else{
      shinyjs::show("format")
      shinyjs::show("genere")
    }
  }
})

