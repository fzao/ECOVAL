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

cleanHabitat <- function(){
  updateTextInput(session, "namehabitat", value = "")
  updateTextInput(session, "codecorinehabitat", value = "")
  updateTextInput(session, "codeeunishabitat", value = "")
  updateSelectInput(session, "typehabitat", selected = 1)
  updateTextAreaInput(session, "justifyhabitat", value = "")
  updateSelectInput(session, "presencehabitat", selected = 1)
}

updateListHabitat <- function(name){
  partialisthabitat <- list("-" = 0)
  if(numhabitat > 0){
    for(i in 1:numhabitat){
      hname <- paste("Habitat", as.character(i))
      if(exists(hname, where = ecoval)){
        if(ecoval[[hname]][7,2] == ecoval[[name]][12,2]){
          partialisthabitat[[hname]] <- i
        }
      }
    }
    updateSelectInput(session, "selecthabitat", choices = partialisthabitat, selected = partialisthabitat[[length(partialisthabitat)]])  
  }
}

observeEvent(input$newhabitat, {
  numhabitat <<- numhabitat + 1
  newname <- paste("Habitat", as.character(numhabitat))
  listhabitat[[newname]] <<- numhabitat
  updateSelectInput(session, "selecthabitat", choices = listhabitat, selected = numhabitat)
  # create new DF
  ecoval[[newname]] <<- model_habitat
  # add site info
  ecoval[[newname]][7,2] <<- ecoval[[paste("Site no.", input$selectsite)]][12,2]
  # clean habitat
  cleanHabitat()
  # update list
  updateListHabitat(paste("Site no.", input$selectsite))
})

observeEvent(input$deletehabitat, {
  numero <- as.integer(input$deletehabitat)
  if(numero > 0) cleanHabitat()
})

observeEvent(input$destroyhabitat, {
  numero <- as.integer(input$selecthabitat)
  if(numero > 0){
    name <- paste("Habitat", as.character(numero))
    listhabitat[[name]] <<- NULL
    ecoval[[name]] <<- NULL
    updateListHabitat(paste("Site no.", input$selectsite)) 
  }
})

observeEvent(input$selecthabitat, {
  numero <- as.integer(input$selecthabitat)
  if(numero == 0){
    shinyjs::hide("namehabitat")
    shinyjs::hide("codecorinehabitat")
    shinyjs::hide("codeeunishabitat")
    shinyjs::hide("typehabitat")
    shinyjs::hide("justifyhabitat")
    shinyjs::hide("presencehabitat")
  }else{
    shinyjs::show("namehabitat")
    shinyjs::show("codecorinehabitat")
    shinyjs::show("codeeunishabitat")
    shinyjs::show("typehabitat")
    shinyjs::show("justifyhabitat")
    if(input$sitetype == 2) shinyjs::show("presencehabitat")
    name  <- paste("Habitat", as.character(numero))
    if(exists(name, where = ecoval)){
      cleanHabitat()
      if(ecoval[[name]][1,2] != "NA") updateTextInput(session, "namehabitat", value = ecoval[[name]][1,2])
      if(ecoval[[name]][2,2] != "NA") updateTextInput(session, "codecorinehabitat", value = ecoval[[name]][2,2])
      if(ecoval[[name]][3,2] != "NA") updateTextInput(session, "codeeunishabitat", value = ecoval[[name]][3,2])
      if(ecoval[[name]][4,2] != "NA") updateSelectInput(session, "typehabitat", selected = as.integer(ecoval[[name]][4,2]))
      if(ecoval[[name]][5,2] != "NA") updateTextAreaInput(session, "justifyhabitat", value = ecoval[[name]][5,2])
      if(ecoval[[name]][6,2] != "NA") updateSelectInput(session, "presencehabitat", selected = as.integer(ecoval[[name]][6,2]))
    }
  }
})

observeEvent(input$namehabitat, {saveHabitat(as.integer(input$selecthabitat))})
observeEvent(input$codecorinehabitat, {saveHabitat(as.integer(input$selecthabitat))})
observeEvent(input$codeeunishabitat, {saveHabitat(as.integer(input$selecthabitat))})
observeEvent(input$typehabitat, {saveHabitat(as.integer(input$selecthabitat))})
observeEvent(input$justifyhabitat, {saveHabitat(as.integer(input$selecthabitat))})
observeEvent(input$presencehabitat, {saveHabitat(as.integer(input$selecthabitat))})

saveHabitat <- function(numero){
  if(numero > 0){
    name <- paste("Habitat", as.character(numero))
    ecoval[[name]][1,2] <<- input$namehabitat
    ecoval[[name]][2,2] <<- input$codecorinehabitat
    ecoval[[name]][3,2] <<- input$codeeunishabitat
    ecoval[[name]][4,2] <<- input$typehabitat
    ecoval[[name]][5,2] <<- input$justifyhabitat
    ecoval[[name]][6,2] <<- input$presencehabitat
  }
}
