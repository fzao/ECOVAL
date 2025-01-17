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
# Copyright (c) EDF-INRAE 2019-2021
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
  updateNumericInput(session, "perimelargsp", value = 0)
  updateSelectInput(session, "commfaun", selected = 1)
}

updateListSpecies <- function(name, inplace=FALSE){
  showlist <- list()
  for(i in 1:dim(listspecies)[1]){
    if(i == 1) showlist[[listspecies[i,1]]] <- listspecies[i,2]
    else{
      spname <- listspecies[i,1]
      if(exists(spname, where = ecoval)){
        if(ecoval[[spname]][6,2] == ecoval[[name]][12,2]){
          if(listspecies[i,3] != "NA" & listspecies[i,3] != "") showlist[[listspecies[i,3]]] <- listspecies[i,2]
          else showlist[[listspecies[i,1]]] <- listspecies[i,2]
        }
      }
    }
  }
  if(inplace) updateSelectInput(session, "selectspecies", choices = showlist, selected = input$selectspecies)
  else updateSelectInput(session, "selectspecies", choices = showlist, selected = showlist[[length(showlist)]])
  # equivalence
  updateListSpeciesSiteEquivalence()
}

observeEvent(input$newspecies, {
  numspecies <<- numspecies + 1
  newname <- paste("Espece", as.character(numspecies))
  newspecies <- data.frame("species" = newname, "index" = numspecies, "name" = newname)
  listspecies <<- rbind(listspecies, newspecies)
  # create new DF
  ecoval[[newname]] <<- model_species
  # add site info
  ecoval[[newname]][6,2] <<- ecoval[[paste("Site no.", input$selectsite)]][12,2] # ID
  ecoval[[newname]][7,2] <<- ecoval[[paste("Site no.", input$selectsite)]][2,2] # type
  ecoval[[newname]][2,2] <<- newname #default french name
  # create new DF
  if(ecoval[[newname]][7,2] == '1' | ecoval[[newname]][7,2] == '3'){
    name <- paste("DI no.", as.character(numspecies))
    ecoval[[name]] <<- model_D
  }
  if(ecoval[[newname]][7,2] == '2' | ecoval[[newname]][7,2] == '3'){
    name <- paste("DC no.", as.character(numspecies))
    ecoval[[name]] <<- model_D
  }
  # clean species
  cleanSpecies()
  # update list
  updateListSpecies(paste("Site no.", input$selectsite)) 
  # force default choice to NULL (auto update for "selecthabitatSI")
  updateSelectInput(session, "selectsiteimpact", selected = "0")
  updateSelectInput(session, "selectsitecompens", selected = "0")
  updateSelectInput(session, "selectsiteimpact2", selected = "0")
  updateSelectInput(session, "selectsitecompens2", selected = "0")
  updateSelectInput(session, "selectsiteimpact3", selected = "0")
  updateSelectInput(session, "selectsitecompens3", selected = "0")
  updateSelectInput(session, "selectsiteimpact4", selected = "0")
  updateSelectInput(session, "selectsitecompens4", selected = "0")
})

observeEvent(input$deletespecies, {
  numero <- as.integer(input$deletespecies)
  if(numero > 0) cleanSpecies()
})

observeEvent(input$destroyspecies, {
  numero <- as.integer(input$selectspecies)
  if(numero > 0){
    name <- paste("Espece", as.character(numero))
    listspecies <<- listspecies[-c(which(listspecies$species == name)),]
    type <- ecoval[[name]][7,2]
    ecoval[[name]] <<- NULL
    updateListSpecies(paste("Site no.", input$selectsite))
    if(type == '1' | type == '3'){
      newname <- paste("DI no.", as.character(numero))
      ecoval[[newname]] <<- NULL
    }
    if(type == '2' | type == '3'){
      newname <- paste("DC no.", as.character(numero))
      ecoval[[newname]] <<- NULL
    }
    # force default choice to NULL (auto update for "selecthabitatSI")
    updateSelectInput(session, "selectsiteimpact", selected = "0")
    updateSelectInput(session, "selectsitecompens", selected = "0")
    updateSelectInput(session, "selectsiteimpact2", selected = "0")
    updateSelectInput(session, "selectsitecompens2", selected = "0")
    updateSelectInput(session, "selectsiteimpact3", selected = "0")
    updateSelectInput(session, "selectsitecompens3", selected = "0")
    updateSelectInput(session, "selectsiteimpact4", selected = "0")
    updateSelectInput(session, "selectsitecompens4", selected = "0")
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
    shinyjs::hide("perimelargsp")
    shinyjs::hide("commfaun")
  }else{
    shinyjs::show("latinnamespecies")
    shinyjs::show("frenchnamespecies")
    shinyjs::show("typespecies")
    shinyjs::show("justifyspecies")
    shinyjs::show("perimelargsp")
    shinyjs::show("commfaun")
    if(input$sitetype == 2) shinyjs::show("presencespecies")
    name  <- paste("Espece", as.character(numero))
    if(exists(name, where = ecoval)){
      cleanSpecies()
      if(ecoval[[name]][1,2] != "NA") updateTextInput(session, "latinnamespecies", value = ecoval[[name]][1,2])
      if(ecoval[[name]][2,2] != "NA") updateTextInput(session, "frenchnamespecies", value = ecoval[[name]][2,2])
      if(ecoval[[name]][3,2] != "NA") updateSelectInput(session, "typespecies", selected = as.integer(ecoval[[name]][3,2]))
      if(ecoval[[name]][4,2] != "NA") updateTextAreaInput(session, "justifyspecies", value = ecoval[[name]][4,2])
      if(ecoval[[name]][5,2] != "NA") updateSelectInput(session, "presencespecies", selected = as.integer(ecoval[[name]][5,2]))
      if(ecoval[[name]][8,2] != "NA") updateNumericInput(session, "perimelargsp", value = as.numeric(ecoval[[name]][8,2]))
      if(ecoval[[name]][9,2] != "NA") updateSelectInput(session, "commfaun", selected = as.integer(ecoval[[name]][9,2]))
    }
  }
})

observeEvent(input$latinnamespecies, {saveSpecies(as.integer(input$selectspecies))})
observeEvent(input$frenchnamespecies, {
  numero = as.integer(input$selectspecies)
  if(numero > 0){
    if(input$frenchnamespecies != "NA" & input$frenchnamespecies != ""){
      name <- paste("Espece", as.character(numero))
      listspecies[c(which(listspecies$species == name)),3] <<- input$frenchnamespecies
    }
  }
  updateListSpecies(paste("Site no.", input$selectsite), TRUE)
  saveSpecies(as.integer(input$selectspecies))
})
observeEvent(input$typespecies, {saveSpecies(as.integer(input$selectspecies))})
observeEvent(input$justifyspecies, {saveSpecies(as.integer(input$selectspecies))})
observeEvent(input$presencespecies, {saveSpecies(as.integer(input$selectspecies))})
observeEvent(input$perimelargsp, {saveSpecies(as.integer(input$selectspecies))})
observeEvent(input$commfaun, {saveSpecies(as.integer(input$selectspecies))})

saveSpecies <- function(numero){
  if(numero > 0){
    name <- paste("Espece", as.character(numero))
    ecoval[[name]][1,2] <<- input$latinnamespecies
    if(input$frenchnamespecies != "NA" & input$frenchnamespecies != "") ecoval[[name]][2,2] <<- input$frenchnamespecies
    ecoval[[name]][3,2] <<- input$typespecies
    ecoval[[name]][4,2] <<- input$justifyspecies
    ecoval[[name]][5,2] <<- input$presencespecies
    ecoval[[name]][8,2] <<- input$perimelargsp
    ecoval[[name]][9,2] <<- input$commfaun
  }
}
