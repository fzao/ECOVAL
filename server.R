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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  fileloaded <- ""
  model_info_general <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 1, header = FALSE, stringsAsFactors = FALSE)
  model_site <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 2, header = FALSE, stringsAsFactors = FALSE)
  model_species <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 3, header = FALSE, stringsAsFactors = FALSE)
  model_habitat <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 4, header = FALSE, stringsAsFactors = FALSE)
  ecoval <- list()
  ecoval[["General"]] <- model_info_general
  numsite <- 0
  numspecies <- 0
  numhabitat <- 0
  listsite <- list("-" = 0)
  listspecies <- list("-" = 0)
  listhabitat <- list("-" = 0)
  
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
  
  observeEvent(input$projectname, {
    ecoval$General[1,2] <<- input$projectname
  })
  
  observeEvent(input$projectcontext, {
    ecoval$General[2,2] <<- input$projectcontext
  })
  
  observeEvent(input$date, {
    ecoval$General[3,2] <<- format(input$date)
  })
  
  output$viewsiteno <- renderText({ paste("<font color=\"#000000\"; size=\"+1\"><b>", "SITE NUMERO", "</b></font>", "<font color=\"#000000\"; size=\"+1\"><b>", input$selectsite, "</b></font>") })
  output$enjeusiteno <- renderText({ paste("<font color=\"#000000\"; size=\"+1\"><b>", "ENJEUX DU SITE NUMERO", "</b></font>", "<font color=\"#000000\"; size=\"+1\"><b>", input$selectsite, "</b></font>") })
  
  output$btn_telecharger <- downloadHandler(
      filename = function() {
        if(input$projectname == ""){
          paste('Ecoval-', Sys.Date(), '.xlsx', sep='')
        }else{
          paste(input$projectname, '.xlsx', sep='')
        }
      },
      content = function(con) {
        nbsite <- length(listsite) - 1
        nbspecies <- length(listspecies) - 1
        ecoval$General[4,2] <<- nbsite
        ecoval$General[5,2] <<- nbspecies
        write.xlsx2(ecoval$General, con, sheetName = 'Général', row.names = FALSE, col.names = FALSE)
        if(nbsite > 0){
          for(i in 1:nbsite) write.xlsx2(ecoval[[i+1]], con, sheetName = names(ecoval)[i+1], row.names = FALSE, col.names = FALSE, append = TRUE)
          if(nbspecies > 0){
            for(i in 1:nbspecies) write.xlsx2(ecoval[[nbsite+i+1]], con, sheetName = names(ecoval)[nbsite+i+1], row.names = FALSE, col.names = FALSE, append = TRUE)
          }
        }
      }
  )
  
  observeEvent(input$userfile, {
    inFile <- input$userfile
    if (is.null(inFile)) return(NULL)
    ecoval[["General"]] <<- read.xlsx2(inFile$datapath, sheetIndex = 1, header = FALSE, stringsAsFactors = FALSE)
    updateTextInput(session, "projectname", value = ecoval$General[1,2])
    updateTextAreaInput(session, "projectcontext", value = ecoval$General[2,2])
    updateDateInput(session, "date", value = ecoval$General[3,2])
    numsite <<- as.integer(ecoval$General[4,2])
    if(numsite > 0) for(i in 1:numsite){
      name <- paste("Site no.", as.character(i))
      listsite[[name]] <<- i
      ecoval[[name]] <<- read.xlsx2(inFile$datapath, sheetIndex = i+1, header = FALSE, stringsAsFactors = FALSE)
    }
    updateSelectInput(session, "selectsite", choices = listsite, selected = listsite[[length(listsite)]])
  })
  
  observeEvent(input$link1, {
    if(input$duree == '1'){
      text1 <- h5(strong("-- Temporaire de courte durée --\n"))
      text2 <- h5("La perturbation a des effets qui durent jusqu'à quelques années sans intervention supplémentaire")
      if(input$sitetype == '1'){
        text3 <- h5(em("Ex.: chantier, stockage d'engins"))
      }else if(input$sitetype == '2'){
        text3 <- h5(em("Ex.: fauche, pâturage"))
      }else if(input$sitetype == '3'){
        text3 <- h5(em("Ex. Impacts: chantier, stockage d'engins / Ex. MC: fauche, pâturage")) 
      }
    }else if(input$duree == '2'){
      text1 <- h5(strong("-- Temporaire de longue durée --\n"))
      text2 <- h5("La perturbation a des effets qui durent de quelques années à quelques dizaines d'années sans intervention supplémentaire")
      if(input$sitetype == '1'){
        text3 <- h5(em("Ex.: carrière, stockage de déchets"))
      }else if(input$sitetype == '2'){
        text3 <- h5(em("Ex.: semi, gîtes pour reptiles"))
      }else if(input$sitetype == '3'){
        text3 <- h5(em("Ex. Impacts: carrière, stockage de déchets / Ex. MC: semi, gîtes pour reptiles")) 
      }
    }else if(input$duree == '3'){
      text1 <- h5(strong("-- Permanent --\n"))
      text2 <- h5("La perturbation a des effets qui durent au-delà de quelques dizaines d'années")
      if(input$sitetype == '1'){
        text3 <- h5(em("Ex.: bâtiment, retenue d'eau"))
      }else if(input$sitetype == '2'){
        text3 <- h5(em("Ex.: arrêt d'une activité agricole"))
      }else if(input$sitetype == '3'){
        text3 <- h5(em("Ex. Impacts: bâtiment, retenue d'eau / Ex. MC: arrêt d'une activité agricole")) 
      }
    }
    showModal(modalDialog(
      h5("DUREE DES PERTURBATIONS"), hr(), text1, text2, text3, easyClose = TRUE, footer = NULL))
  })
  
  observeEvent(input$link2, {
    if(input$intensite == '1'){
      text1 <- h5(strong("-- Modification peu intense de l'écosystème --\n"))
      text2 <- h5("Action sur quelques compartiments de l'écosystème (sol, flore, faune, hydrologie...)")
      if(input$sitetype == '1'){
        text3 <- h5(em("Ex.: passage d'engins"))
      }else if(input$sitetype == '2'){
        text3 <- h5(em("Ex.: semi, plantation"))
      }else if(input$sitetype == '3'){
        text3 <- h5(em("Ex. Impacts: passage d'engins / Ex. MC: semi, plantation")) 
      }
    }else if(input$intensite == '2'){
      text1 <- h5(strong("-- Modification intense de l'écosystème --\n"))
      text2 <- h5("Action sur plusieurs compartiments de l'écosystème (sol, flore, faune, hydrologie...)")
      if(input$sitetype == '1'){
        text3 <- h5(em("Ex.: défrichement"))
      }else if(input$sitetype == '2'){
        text3 <- h5(em("Ex.: décomposition du sol, amélioration de l'hydrologie"))
      }else if(input$sitetype == '3'){
        text3 <- h5(em("Ex. Impacts: défrichement / Ex. MC: décomposition du sol, amélioration de l'hydrologie")) 
      }
    }else if(input$intensite == '3'){
      text1 <- h5(strong("-- Modification très intense de l'écosystème --\n"))
      text2 <- h5("Action sur tous les compartiements de l'écosystème (sol, flore, faune, hydrologie...)")
      if(input$sitetype == '1'){
        text3 <- h5(em("Ex.: destruction totale par imperméabilisation"))
      }else if(input$sitetype == '2'){
        text3 <- h5(em("Ex.: création d'une mare"))
      }else if(input$sitetype == '3'){
        text3 <- h5(em("Ex. Impacts: destruction totale par imperméabilisation / Ex. MC: création d'une mare")) 
      }
    }
    showModal(modalDialog(
      h5("INTENSITE DES MODIFICATIONS"), hr(), text1, text2, text3, easyClose = TRUE, footer = NULL))
  })
  
  observeEvent(input$link3, {
    if(input$portee == '1'){
      text1 <- h5(strong("-- Ponctuelle de faible surface --\n"))
      text2 <- h5("La pertubation concerne une emprise au sol ponctuelle sur une surface allant jusqu'à quelques hectares")
      if(input$sitetype == '1'){
        text3 <- h5(em("Ex.: bâtiments, parking, centrale hydroélectrique"))
      }else if(input$sitetype == '2'){
        text3 <- h5(em("Ex.: création d'une mare"))
      }else if(input$sitetype == '3'){
        text3 <- h5(em("Ex. Impacts: bâtiments, parking, centrale hydroélectrique / Ex. MC: création d'une mare")) 
      }
    }else if(input$portee == '2'){
      text1 <- h5(strong("-- Ponctuelle de surface importante --\n"))
      text2 <- h5("La pertubation concerne une emprise au sol ponctuelle sur une surface allant au-delà de quelques hectares")
      if(input$sitetype == '1'){
        text3 <- h5(em("Ex.: aéroport, retenue d'eau"))
      }else if(input$sitetype == '2'){
        text3 <- h5(em("Ex.: ouverture de milieux sur une dizaine d'ha"))
      }else if(input$sitetype == '3'){
        text3 <- h5(em("Ex. Impacts: aéroport, retenue d'eau / Ex. MC: ouverture de milieux sur une dizaine d'ha")) 
      }
    }else if(input$portee == '3'){
      text1 <- h5(strong("-- Linéaire --\n"))
      text2 <- h5("Perturbation indépendante de la surface. La perturbation concerne une emprise au sol linéaire influençant les connectivités")
      if(input$sitetype == '1'){
        text3 <- h5(em("Ex.: ligne grande vitesse, autoroute"))
      }else if(input$sitetype == '2'){
        text3 <- h5(em("Ex.: plantation de haie, reméandrage d'un tronçon de rivière"))
      }else if(input$sitetype == '3'){
        text3 <- h5(em("Ex. Impacts: ligne grande vitesse, autoroute / Ex. MC: plantation de haie, reméandrage d'un tronçon de rivière")) 
      }
    }
    showModal(modalDialog(
      h5("PORTEE DES PERTURBATIONS"), hr(), text1, text2, text3, easyClose = TRUE, footer = NULL))
  })
  
  observeEvent(input$new, {
    numsite <<- numsite + 1
    newname <- paste("Site no.", as.character(numsite))
    listsite[[newname]] <<- numsite
    updateSelectInput(session, "selectsite", choices = listsite, selected = numsite)
    # create new DF
    ecoval[[newname]] <<- model_site
    # clean window
    cleanWindow()
  })

  observeEvent(input$destroy, {
    numero <- as.integer(input$selectsite)
    if(numero > 0){
      name <- paste("Site no.", as.character(numero))
      listsite[[name]] <<- NULL
      ecoval[[name]] <<- NULL
      updateSelectInput(session, "selectsite", choices = listsite, selected = listsite[[length(listsite)]])
    }
  })

  observeEvent(input$selectsite, {
    numero <- as.integer(input$selectsite)
    if(numero == 0){
      hideTab(inputId = "prjtabs", target = "description")
      hideTab(inputId = "prjtabs", target = "identification")
    }else{
      showTab(inputId = "prjtabs", target = "description")
      showTab(inputId = "prjtabs", target = "identification")
      name  <- paste("Site no.", as.character(numero))
      if(exists(name, where = ecoval)){
        if(ecoval[[name]][1,2] != "NA"){
          updateTextInput(session, "sitename", value = ecoval[[name]][1,2])
          updateSelectInput(session, "sitetype", selected = as.integer(ecoval[[name]][2,2]))
          updateNumericInput(session, "surface", value = as.numeric(ecoval[[name]][3,2]))
          updateNumericInput(session, "latitude", value = as.numeric(ecoval[[name]][4,2]))
          updateNumericInput(session, "longitude", value = as.numeric(ecoval[[name]][5,2]))
          updateTextAreaInput(session, "sitecontext", value = ecoval[[name]][6,2])
          updateTextAreaInput(session, "descqual", value = ecoval[[name]][7,2])
          updateTextAreaInput(session, "tempo", value = ecoval[[name]][8,2])
          updateSelectInput(session, "duree", selected = as.integer(ecoval[[name]][9,2]))
          updateSelectInput(session, "intensite", selected = as.integer(ecoval[[name]][10,2]))
          updateSelectInput(session, "portee", selected = as.integer(ecoval[[name]][11,2]))
        }
      }
    }
  })
  
  observeEvent(input$sitename, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$sitetype, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$surface, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$latitude, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$longitude, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$sitecontext, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$descqual, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$tempo, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$duree, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$intensite, {saveSite(as.integer(input$selectsite))})
  observeEvent(input$portee, {saveSite(as.integer(input$selectsite))})

  saveSite <- function(numero){
    if(numero > 0){
      name <- paste("Site no.", as.character(numero))
      ecoval[[name]][1,2] <<- input$sitename
      ecoval[[name]][2,2] <<- input$sitetype
      ecoval[[name]][3,2] <<- input$surface
      ecoval[[name]][4,2] <<- input$latitude
      ecoval[[name]][5,2] <<- input$longitude
      ecoval[[name]][6,2] <<- input$sitecontext
      ecoval[[name]][7,2] <<- input$descqual
      ecoval[[name]][8,2] <<- input$tempo
      ecoval[[name]][9,2] <<- input$duree
      ecoval[[name]][10,2] <<- input$intensite
      ecoval[[name]][11,2] <<- input$portee
    }
  }

  cleanWindow <- function(){
    updateTextInput(session, "sitename", value = "", placeholder = "Nom du site...")
    updateSelectInput(session, "sitetype", selected = 1)
    updateNumericInput(session, "surface", value = 0.)
    updateNumericInput(session, "latitude", value = 0.)
    updateNumericInput(session, "longitude", value = 0.)
    updateTextAreaInput(session, "sitecontext", value = "", placeholder = "Décrire le contexte du site ici...")
    updateTextAreaInput(session, "descqual", value = "", placeholder = "Nature, emprise, effets indirects...")
    updateTextAreaInput(session, "tempo", value = "", placeholder = "Plusieurs phases? Court/long terme...")
    updateSelectInput(session, "duree", selected = 1)
    updateSelectInput(session, "intensite", selected = 1)
    updateSelectInput(session, "portee", selected = 1)
  }
  
  cleanSpecies <- function(){
    updateTextInput(session, "latinnamespecies", value = "")
    updateTextInput(session, "frenchnamespecies", value = "")
    updateSelectInput(session, "typespecies", selected = 1)
    updateTextAreaInput(session, "justifyspecies", value = "")
    updateSelectInput(session, "presencespecies", selected = 1)
  }
  
  cleanHabitat <- function(){
    updateTextInput(session, "namehabitat", value = "")
    updateTextInput(session, "codecorinehabitat", value = "")
    updateTextInput(session, "codeeunishabitat", value = "")
    updateSelectInput(session, "typehabitat", selected = 1)
    updateTextAreaInput(session, "justifyhabitat", value = "")
    updateSelectInput(session, "presencehabitat", selected = 1)
  }
  
  observeEvent(input$delete, {
    numero <- as.integer(input$selectsite)
    if(numero > 0) cleanWindow()
  })
  
  observeEvent(input$newspecies, {
    numspecies <<- numspecies + 1
    newname <- paste("Espece", as.character(numspecies))
    listspecies[[newname]] <<- numspecies
    updateSelectInput(session, "selectspecies", choices = listspecies, selected = numspecies)
    # create new DF
    ecoval[[newname]] <<- model_species
    # clean species
    cleanSpecies()
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
      updateSelectInput(session, "selectspecies", choices = listspecies, selected = listspecies[[length(listspecies)]])
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
        if(ecoval[[name]][1,2] != "NA"){
          updateTextInput(session, "latinnamespecies", value = ecoval[[name]][1,2])
          updateTextInput(session, "frenchnamespecies", value = ecoval[[name]][2,2])
          updateSelectInput(session, "typespecies", selected = as.integer(ecoval[[name]][3,2]))
          updateTextAreaInput(session, "justifyspecies", value = ecoval[[name]][4,2])
          updateSelectInput(session, "presencespecies", selected = as.integer(ecoval[[name]][5,2]))
        }
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

  observeEvent(input$newhabitat, {
    numhabitat <<- numhabitat + 1
    newname <- paste("Habitat", as.character(numhabitat))
    listhabitat[[newname]] <<- numhabitat
    updateSelectInput(session, "selecthabitat", choices = listhabitat, selected = numhabitat)
    # create new DF
    ##cfz ecoval[[newname]] <<- model_site
    # clean window
    ##cfz cleanWindow()
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
      ##cfz ecoval[[name]] <<- NULL
      updateSelectInput(session, "selecthabitat", choices = listhabitat, selected = listhabitat[[length(listhabitat)]])
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
      shinyjs::show("presencehabitat")
      #cfz name  <- paste("Site no.", as.character(numero))
      # if(exists(name, where = ecoval)){
      #   if(ecoval[[name]][1,2] != "NA"){
      #     updateTextInput(session, "sitename", value = ecoval[[name]][1,2])
      #     updateSelectInput(session, "sitetype", selected = as.integer(ecoval[[name]][2,2]))
      #     updateNumericInput(session, "surface", value = as.numeric(ecoval[[name]][3,2]))
      #     updateNumericInput(session, "latitude", value = as.numeric(ecoval[[name]][4,2]))
      #     updateNumericInput(session, "longitude", value = as.numeric(ecoval[[name]][5,2]))
      #     updateTextAreaInput(session, "sitecontext", value = ecoval[[name]][6,2])
      #     updateTextAreaInput(session, "descqual", value = ecoval[[name]][7,2])
      #     updateTextAreaInput(session, "tempo", value = ecoval[[name]][8,2])
      #     updateSelectInput(session, "duree", selected = as.integer(ecoval[[name]][9,2]))
      #     updateSelectInput(session, "intensite", selected = as.integer(ecoval[[name]][10,2]))
      #     updateSelectInput(session, "portee", selected = as.integer(ecoval[[name]][11,2]))
      #   }
      # }
    }
  })
  
  # output$projectmap <- renderLeaflet({
  #   if(is.numeric(input$latitude) & is.numeric(input$longitude)){
  #     if(input$latitude != 0. | input$longitude !=0.){
  #       m <- leaflet() %>%
  #         addTiles() %>%
  #         addMarkers(lat=input$latitude , lng=input$longitude, popup=input$projectcontext)
  #       m
  #     }  
  #   } 
  # })
  
})
