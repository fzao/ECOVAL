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

updateListHabitat <- function(name, inplace=FALSE){
  showlist <- list()
  for(i in 1:dim(listhabitat)[1]){
    if(i == 1) showlist[[listhabitat[i,1]]] <- listhabitat[i,2]
    else{
      hname <- listhabitat[i,1]
      if(exists(hname, where = ecoval)){
        if(ecoval[[hname]][7,2] == ecoval[[name]][12,2]){
          if(listhabitat[i,3] != "NA" & listhabitat[i,3] != "") showlist[[listhabitat[i,3]]] <- listhabitat[i,2]
          else showlist[[listhabitat[i,1]]] <- listhabitat[i,2]
        }
      }
    }
  }
  if(inplace) updateSelectInput(session, "selecthabitat", choices = showlist, selected = input$selecthabitat)
  else updateSelectInput(session, "selecthabitat", choices = showlist, selected = showlist[[length(showlist)]])
}

observeEvent(input$newhabitat, {
  numhabitat <<- numhabitat + 1
  newname <- paste("Habitat", as.character(numhabitat))
  
  newhabitat <- data.frame("habitat" = newname, "index" = numhabitat, "name" = newname)
  listhabitat <<- rbind(listhabitat, newhabitat)
  # create new DF
  ecoval[[newname]] <<- model_habitat
  # add site info
  ecoval[[newname]][7,2] <<- ecoval[[paste("Site no.", input$selectsite)]][12,2] # ID
  ecoval[[newname]][8,2] <<- ecoval[[paste("Site no.", input$selectsite)]][2,2] # type
  ecoval[[newname]][1,2] <<- newname #default name
  # create new DF
  if(ecoval[[newname]][8,2] == '1' | ecoval[[newname]][8,2] == '3'){
    newname <- paste("CI no.", as.character(numhabitat))
    ecoval[[newname]] <<- model_C
  }
  if(ecoval[[newname]][8,2] == '2' | ecoval[[newname]][8,2] == '3'){
    newname <- paste("CC no.", as.character(numhabitat))
    ecoval[[newname]] <<- model_C
  }
  # clean habitat
  cleanHabitat()
  # update list
  updateListHabitat(paste("Site no.", input$selectsite))
  # force default choice to NULL (auto update for "selecthabitatSI")
  updateSelectInput(session, "selectsiteimpact", selected = "0")
  updateSelectInput(session, "selectsitecompens", selected = "0")
  updateSelectInput(session, "selectsiteimpact2", selected = "0")
  updateSelectInput(session, "selectsitecompens2", selected = "0")
})

observeEvent(input$deletehabitat, {
  numero <- as.integer(input$deletehabitat)
  if(numero > 0) cleanHabitat()
})

observeEvent(input$destroyhabitat, {
  numero <- as.integer(input$selecthabitat)
  if(numero > 0){
    name <- paste("Habitat", as.character(numero))
    listhabitat <<- listhabitat[-c(which(listhabitat$habitat == name)),]
    type <- ecoval[[name]][8,2]
    ecoval[[name]] <<- NULL
    updateListHabitat(paste("Site no.", input$selectsite))
    if(type == '1' | type == '3'){
      newname <- paste("CI no.", as.character(numero))
      ecoval[[newname]] <<- NULL
    }
    if(type == '2' | type == '3'){
      newname <- paste("CC no.", as.character(numero))
      ecoval[[newname]] <<- NULL
    }
    # force default choice to NULL (auto update for "selecthabitatSI")
    updateSelectInput(session, "selectsiteimpact", selected = "0")
    updateSelectInput(session, "selectsitecompens", selected = "0")
    updateSelectInput(session, "selectsiteimpact2", selected = "0")
    updateSelectInput(session, "selectsitecompens2", selected = "0")
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

observeEvent(input$namehabitat, {
  numero = as.integer(input$selecthabitat)
  if(numero > 0){
    if(input$namehabitat != "NA" & input$namehabitat != ""){
      name <- paste("Habitat", as.character(numero))
      listhabitat[c(which(listhabitat$habitat == name)),3] <<- input$namehabitat
    }
  }
  updateListHabitat(paste("Site no.", input$selectsite), TRUE)
  saveHabitat(as.integer(input$selecthabitat))
})
observeEvent(input$codecorinehabitat, {saveHabitat(as.integer(input$selecthabitat))})
observeEvent(input$codeeunishabitat, {saveHabitat(as.integer(input$selecthabitat))})
observeEvent(input$typehabitat, {saveHabitat(as.integer(input$selecthabitat))})
observeEvent(input$justifyhabitat, {saveHabitat(as.integer(input$selecthabitat))})
observeEvent(input$presencehabitat, {saveHabitat(as.integer(input$selecthabitat))})

saveHabitat <- function(numero){
  if(numero > 0){
    name <- paste("Habitat", as.character(numero))
    if(input$namehabitat != "NA" & input$namehabitat != "") ecoval[[name]][1,2] <<- input$namehabitat
    ecoval[[name]][2,2] <<- input$codecorinehabitat
    ecoval[[name]][3,2] <<- input$codeeunishabitat
    ecoval[[name]][4,2] <<- input$typehabitat
    ecoval[[name]][5,2] <<- input$justifyhabitat
    ecoval[[name]][6,2] <<- input$presencehabitat
  }
}
