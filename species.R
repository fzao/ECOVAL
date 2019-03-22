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

cleanSpecies <- function(){
  updateTextInput(session, "latinnamespecies", value = "")
  updateTextInput(session, "frenchnamespecies", value = "")
  updateSelectInput(session, "typespecies", selected = 1)
  updateTextAreaInput(session, "justifyspecies", value = "")
  updateSelectInput(session, "presencespecies", selected = 1)
}

updateListSpecies <- function(name){
  partialistspecies <- list("-" = 0)
  if(numspecies > 0){
    for(i in 1:numspecies){
      spname <- paste("Espece", as.character(i))
      if(exists(spname, where = ecoval)){
        if(ecoval[[spname]][6,2] == ecoval[[name]][12,2]){
          partialistspecies[[spname]] <- i
        }
      }
    }
    updateSelectInput(session, "selectspecies", choices = partialistspecies, selected = partialistspecies[[length(partialistspecies)]])  
  }
}

observeEvent(input$newspecies, {
  numspecies <<- numspecies + 1
  newname <- paste("Espece", as.character(numspecies))
  listspecies[[newname]] <<- numspecies
  updateSelectInput(session, "selectspecies", choices = listspecies, selected = numspecies)
  # create new DF
  ecoval[[newname]] <<- model_species
  # add site info
  ecoval[[newname]][6,2] <<- ecoval[[paste("Site no.", input$selectsite)]][12,2]
  # clean species
  cleanSpecies()
  # update list
  updateListSpecies(paste("Site no.", input$selectsite)) 
})

observeEvent(input$deletespecies, {
  numero <- as.integer(input$deletespecies)
  if(numero > 0) cleanSpecies()
})

observeEvent(input$destroyspecies, {
  numero <- as.integer(input$selectspecies)
  if(numero > 0){
    name <- paste("Espece", as.character(numero))
    listspecies[[name]] <<- NULL
    ecoval[[name]] <<- NULL
    updateListSpecies(paste("Site no.", input$selectsite)) 
  }
})

observeEvent(input$selectspecies, {
  numero <- as.integer(input$selectspecies)
  if(numero == 0){
    shinyjs::hide("latinnamespecies")
    shinyjs::hide("frenchnamespecies")
    shinyjs::hide("typespecies")
    shinyjs::hide("justifyspecies")
    shinyjs::hide("presencespecies")
  }else{
    shinyjs::show("latinnamespecies")
    shinyjs::show("frenchnamespecies")
    shinyjs::show("typespecies")
    shinyjs::show("justifyspecies")
    shinyjs::show("presencespecies")
    name  <- paste("Espece", as.character(numero))
    if(exists(name, where = ecoval)){
      cleanSpecies()
      if(ecoval[[name]][1,2] != "NA") updateTextInput(session, "latinnamespecies", value = ecoval[[name]][1,2])
      if(ecoval[[name]][2,2] != "NA") updateTextInput(session, "frenchnamespecies", value = ecoval[[name]][2,2])
      if(ecoval[[name]][3,2] != "NA") updateSelectInput(session, "typespecies", selected = as.integer(ecoval[[name]][3,2]))
      if(ecoval[[name]][4,2] != "NA") updateTextAreaInput(session, "justifyspecies", value = ecoval[[name]][4,2])
      if(ecoval[[name]][5,2] != "NA") updateSelectInput(session, "presencespecies", selected = as.integer(ecoval[[name]][5,2]))
    }
  }
})

observeEvent(input$latinnamespecies, {saveSpecies(as.integer(input$selectspecies))})
observeEvent(input$frenchnamespecies, {saveSpecies(as.integer(input$selectspecies))})
observeEvent(input$typespecies, {saveSpecies(as.integer(input$selectspecies))})
observeEvent(input$justifyspecies, {saveSpecies(as.integer(input$selectspecies))})
observeEvent(input$presencespecies, {saveSpecies(as.integer(input$selectspecies))})

saveSpecies <- function(numero){
  if(numero > 0){
    name <- paste("Espece", as.character(numero))
    ecoval[[name]][1,2] <<- input$latinnamespecies
    ecoval[[name]][2,2] <<- input$frenchnamespecies
    ecoval[[name]][3,2] <<- input$typespecies
    ecoval[[name]][4,2] <<- input$justifyspecies
    ecoval[[name]][5,2] <<- input$presencespecies
  }
}
