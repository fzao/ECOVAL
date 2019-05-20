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
}

observeEvent(input$newspecies, {
  numspecies <<- numspecies + 1
  newname <- paste("Espece", as.character(numspecies))
  newspecies <- data.frame("species" = newname, "index" = numspecies, "name" = newname)
  listspecies <<- rbind(listspecies, newspecies)
  # create new DF
  ecoval[[newname]] <<- model_species
  # add site info
  ecoval[[newname]][6,2] <<- ecoval[[paste("Site no.", input$selectsite)]][12,2]
  ecoval[[newname]][2,2] <<- newname #default french name
  # create new DF
  newname <- paste("SID no.", as.character(numspecies))
  ecoval[[newname]] <<- model_D
  # clean species
  cleanSpecies()
  # update list
  updateListSpecies(paste("Site no.", input$selectsite)) 
  # force default choice to NULL (auto update for "selecthabitatSI")
  updateSelectInput(session, "selectsiteimpact", selected = "0")
  updateSelectInput(session, "selectsitecompens", selected = "0")
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
    ecoval[[name]] <<- NULL
    updateListSpecies(paste("Site no.", input$selectsite))
    newname <- paste("SID no.", as.character(numero))
    ecoval[[newname]] <<- NULL
    # force default choice to NULL (auto update for "selecthabitatSI")
    updateSelectInput(session, "selectsiteimpact", selected = "0")
    updateSelectInput(session, "selectsitecompens", selected = "0")
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
    if(input$sitetype == 2) shinyjs::show("presencespecies")
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

saveSpecies <- function(numero){
  if(numero > 0){
    name <- paste("Espece", as.character(numero))
    ecoval[[name]][1,2] <<- input$latinnamespecies
    if(input$frenchnamespecies != "NA" & input$frenchnamespecies != "") ecoval[[name]][2,2] <<- input$frenchnamespecies
    ecoval[[name]][3,2] <<- input$typespecies
    ecoval[[name]][4,2] <<- input$justifyspecies
    ecoval[[name]][5,2] <<- input$presencespecies
  }
}
