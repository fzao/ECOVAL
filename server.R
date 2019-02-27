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
  projectmodel1 <- read.xlsx2('model/projet.xlsx', sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE)
  numsite <- 0

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
  
  output$viewsiteno <- renderText({ paste("<font color=\"#000000\"; size=\"+1\"><b>", "SITE NUMERO", "</b></font>", "<font color=\"#000000\"; size=\"+1\"><b>", input$selectsite, "</b></font>") })
  
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
        #write.xlsx2(projectmodel2A, con, sheetName = 'Identification enjeux A', row.names = FALSE, append = TRUE)
      }
  )
  
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
    newlist <- list("-" = 0)
    numsite <<- numsite + 1
    for(i in 1:numsite){
      newlist[[paste("Site no.", as.character(i))]] <- i
    }
    updateSelectInput(session, "selectsite", choices = newlist, selected = (length(newlist) - 1))
  })

  observeEvent(input$delete, {
    newlist <- list("-" = 0)
    numsite <<- numsite - 1
    numsite <<- max(numsite, 0)
    if(numsite > 0){
      for(i in 1:numsite){
        newlist[[paste("Site no.", as.character(i))]] <- i
      }
    }
    updateSelectInput(session, "selectsite", choices = newlist, selected = (length(newlist) - 1))
  })
  
  observeEvent(input$selectsite, {
    if(as.integer(input$selectsite) == 0){
      hideTab(inputId = "prjtabs", target = "description")
      hideTab(inputId = "prjtabs", target = "enjeux")
    }else{
      showTab(inputId = "prjtabs", target = "description")
      showTab(inputId = "prjtabs", target = "enjeux")
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
