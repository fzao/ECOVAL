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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  #source('site.R', local =TRUE)
  source('species.R', local = TRUE)
  source('habitat.R', local = TRUE)
  source('init.R', local = TRUE)
  
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
  
  output$projectmap <- renderLeaflet({
    if(input$latitude != 0 | input$longitude != 0){
        m <- leaflet() %>%
          addTiles() %>%
          addMarkers(lat=input$latitude , lng=input$longitude, popup=input$sitename)
        m
    }else{
      pk <- sample(1:dim(park)[1], 1) 
      m <- leaflet() %>%
        addTiles() %>%
        addMarkers(lat=park[pk, 2] , lng=park[pk,3], popup=park[pk,1])
    }
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
  
  output$btn_telecharger <- downloadHandler(
    filename = function() {
      if(input$projectname == ""){
        paste('Ecoval-', Sys.Date(), '.xlsx', sep='')
      }else{
        paste(input$projectname, '.xlsx', sep='')
      }
    },
    content = function(con) {
      nbsite <- dim(listsite)[1] - 1
      nbspecies <- dim(listspecies)[1] - 1
      nbhabitats <- dim(listhabitat)[1] - 1
      ecoval$General[4,2] <<- nbsite
      ecoval$General[5,2] <<- nbspecies
      ecoval$General[6,2] <<- nbhabitats
      write.xlsx2(ecoval$General, con, sheetName = 'Général', row.names = FALSE, col.names = FALSE)
      s <- 1
      e <- 1
      h <- 1
      si <- 0
      nbsite <- dim(listsite)[1] - 1
      if(nbsite > 0){
        for(i in 1:nbsite){
          index <- listsite[1+i,2]
          name <- paste("Site no.", as.character(s))
          write.xlsx2(ecoval[[listsite[1+i,1]]], con, sheetName = name, row.names = FALSE, col.names = FALSE, append = TRUE)
          if(ecoval[[listsite[1+i,1]]][2,2] == "1" | ecoval[[listsite[1+i,1]]][2,2] == "3"){ # info specifique site impacte
            name <- paste("SIA1 no.", as.character(s))
            siname <- paste("SIA1 no.", as.character(index))
            write.xlsx2(ecoval[[siname]], con, sheetName = name, row.names = FALSE, col.names = TRUE, append = TRUE)
            name <- paste("SIA2 no.", as.character(s))
            siname <- paste("SIA2 no.", as.character(index))
            write.xlsx2(ecoval[[siname]], con, sheetName = name, row.names = FALSE, col.names = TRUE, append = TRUE)
            name <- paste("SIA3 no.", as.character(s))
            siname <- paste("SIA3 no.", as.character(index))
            write.xlsx2(ecoval[[siname]], con, sheetName = name, row.names = FALSE, col.names = TRUE, append = TRUE)
          }
          s <- s + 1
        }
      }
      nbspecies <- dim(listspecies)[1] - 1
      if(nbspecies > 0){
        for(i in 1:nbspecies){
          name <- paste("Espece", as.character(e))
          write.xlsx2(ecoval[[listspecies[1+i,1]]], con, sheetName = name, row.names = FALSE, col.names = FALSE, append = TRUE)
          e <- e + 1
        }
      }
      nbhabitat <- dim(listhabitat)[1] - 1
      if(nbhabitat > 0){
        for(i in 1:nbhabitat){
          name <- paste("Habitat", as.character(h))
          write.xlsx2(ecoval[[listhabitat[1+i,1]]], con, sheetName = paste("Habitat", as.character(h)), row.names = FALSE, col.names = FALSE, append = TRUE)
          h <- h + 1
        }
      }

    }
  )
  
  observeEvent(input$userfile, {
    inFile <- input$userfile
    if (is.null(inFile)) return(NULL)
    
    ecoval <<- list()
    ecoval[["General"]] <<- model_info_general
    numsite <<- 0
    numspecies <<- 0
    numhabitat <<- 0
    listsite <<- data.frame("site" = '-', "index" = 0, "name" = '-', "type" = 0, stringsAsFactors=FALSE)
    listspecies <<- data.frame("species" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
    listhabitat <<- data.frame("habitat" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
    
    ecoval[["General"]] <<- read.xlsx2(inFile$datapath, sheetIndex = 1, header = FALSE, stringsAsFactors = FALSE)
    updateTextInput(session, "projectname", value = ecoval$General[1,2])
    updateTextAreaInput(session, "projectcontext", value = ecoval$General[2,2])
    updateDateInput(session, "date", value = ecoval$General[3,2])
    numsite <<- as.integer(ecoval$General[4,2])
    if(numsite > 0) for(i in 1:numsite){
      name <- paste("Site no.", as.character(i))
      ecoval[[name]] <<- read.xlsx2(inFile$datapath, sheetName = name, header = FALSE, stringsAsFactors = FALSE)
      newsite <- data.frame("site" = name, "index" = i, "name" = ecoval[[name]][1,2], "type" = as.integer(ecoval[[name]][2,2]))
      listsite <<- rbind(listsite, newsite)
      if(ecoval[[name]][2,2] == "1" | ecoval[[name]][2,2] == "3"){ # site impacte
        siname <- paste("SIA1 no.", as.character(i))
        ecoval[[siname]] <<- read.xlsx2(inFile$datapath, sheetName = siname, header = TRUE, stringsAsFactors = FALSE)
        siname <- paste("SIA2 no.", as.character(i))
        ecoval[[siname]] <<- read.xlsx2(inFile$datapath, sheetName = siname, header = TRUE, stringsAsFactors = FALSE)
        siname <- paste("SIA3 no.", as.character(i))
        ecoval[[siname]] <<- read.xlsx2(inFile$datapath, sheetName = siname, header = TRUE, stringsAsFactors = FALSE)
      }
      
    }
    numspecies <<- as.integer(ecoval$General[5,2])
    if(numspecies > 0) for(i in 1:numspecies){
      name <- paste("Espece", as.character(i))
      ecoval[[name]] <<- read.xlsx2(inFile$datapath, sheetName = name, header = FALSE, stringsAsFactors = FALSE)
      newspecies <- data.frame("species" = name, "index" = i, "name" = ecoval[[name]][2,2])
      listspecies <<- rbind(listspecies, newspecies)
    }
    numhabitat <<- as.integer(ecoval$General[6,2])
    if(numhabitat > 0) for(i in 1:numhabitat){
      name <- paste("Habitat", as.character(i))
      ecoval[[name]] <<- read.xlsx2(inFile$datapath, sheetName = name, header = FALSE, stringsAsFactors = FALSE)
      newhabitat <- data.frame("habitat" = name, "index" = i, "name" = ecoval[[name]][1,2])
      listhabitat <<- rbind(listhabitat, newhabitat)
    }
    updateListSite()
    updateListSiteImpactCompens()
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
    newsite <- data.frame("site" = newname, "index" = numsite, "name" = newname, "type" = as.integer(input$sitetype))
    listsite <<- rbind(listsite, newsite)
    updateListSite()
    updateListSiteImpactCompens()
    updateSelectInput(session, "selectspecies", choices = list("-" = 0), selected = 0)
    updateSelectInput(session, "selecthabitat", choices = list("-" = 0), selected = 0)
    # create new DF
    ecoval[[newname]] <<- model_site
    # unique ID
    tmpf <- tempfile()
    n <- 8
    code <- substr(tmpf, (nchar(tmpf)+1)-n, nchar(tmpf))
    ecoval[[newname]][12,2] <<- code
    # back to description (comment: bug with tabset title?...)
    #cfz updateTabsetPanel(session, "prjtabs", selected = "description")
    # clean window
    cleanWindow()
    # SIC
    newSICX(numsite)
  })
  
  observeEvent(input$delete, {
    numero <- as.integer(input$selectsite)
    if(numero > 0) cleanWindow()
  })
  
  observeEvent(input$destroy, {
    numero <- input$selectsite
    if(numero != "0"){
      name <- paste("Site no.", numero)
      # destroy species
      nbspecies <- dim(listspecies)[1] - 1
      mylist <- listspecies
      if(nbspecies > 0){
        for(i in 1:nbspecies){
          spname <- paste("Espece", as.character(listspecies$index[i+1]))
          if(exists(spname, where = ecoval)){
            if(ecoval[[spname]][6,2] == ecoval[[name]][12,2]){
              mylist <- mylist[-c(which(mylist$species == spname)),]
              ecoval[[spname]] <<- NULL
            }
          }
        }
      }
      listspecies <<- mylist
      # destroy habitat
      nbhabitat <- dim(listhabitat)[1] - 1
      mylist <- listhabitat
      if(nbhabitat > 0){
        for(i in 1:nbhabitat){
          hname <- paste("Habitat", as.character(listhabitat$index[i+1]))
          if(exists(hname, where = ecoval)){
            if(ecoval[[hname]][7,2] == ecoval[[name]][12,2]){
              mylist <- mylist[-c(which(mylist$habitat == hname)),]
              ecoval[[hname]] <<- NULL
            }
          }
        }
      }
      listhabitat <<- mylist
      # destroy site
      listsite <<- listsite[-c(which(listsite$site == name)),]
      ecoval[[name]] <<- NULL
      updateListSite()
      updateListSiteImpactCompens()
      # destroy tabs sicx
      delSICX(numero)
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
        cleanWindow()
        if(ecoval[[name]][1,2] != "NA") updateTextInput(session, "sitename", value = ecoval[[name]][1,2])
        if(ecoval[[name]][2,2] != "NA") updateSelectInput(session, "sitetype", selected = ecoval[[name]][2,2])
        if(ecoval[[name]][3,2] != "NA") updateNumericInput(session, "surface", value = as.numeric(ecoval[[name]][3,2]))
        if(ecoval[[name]][4,2] != "NA") updateNumericInput(session, "latitude", value = as.numeric(ecoval[[name]][4,2]))
        if(ecoval[[name]][5,2] != "NA") updateNumericInput(session, "longitude", value = as.numeric(ecoval[[name]][5,2]))
        if(ecoval[[name]][6,2] != "NA") updateTextAreaInput(session, "sitecontext", value = ecoval[[name]][6,2])
        if(ecoval[[name]][7,2] != "NA") updateTextAreaInput(session, "descqual", value = ecoval[[name]][7,2])
        if(ecoval[[name]][8,2] != "NA") updateTextAreaInput(session, "tempo", value = ecoval[[name]][8,2])
        if(ecoval[[name]][9,2] != "NA") updateSelectInput(session, "duree", selected = ecoval[[name]][9,2])
        if(ecoval[[name]][10,2] != "NA") updateSelectInput(session, "intensite", selected = ecoval[[name]][10,2])
        if(ecoval[[name]][11,2] != "NA") updateSelectInput(session, "portee", selected = ecoval[[name]][11,2])
        updateListSpecies(name)
        updateListHabitat(name)
      }
    }
  })
  
  observeEvent(input$sitename, {
    saveSite(input$selectsite)
    updateSiteName(input$selectsite, input$sitename)})
  observeEvent(input$sitetype, {
    saveSite(input$selectsite)
    updateDescTemp(input$sitetype)})
  observeEvent(input$surface, {saveSite(input$selectsite)})
  observeEvent(input$latitude, {saveSite(input$selectsite)})
  observeEvent(input$longitude, {saveSite(input$selectsite)})
  observeEvent(input$sitecontext, {saveSite(input$selectsite)})
  observeEvent(input$descqual, {saveSite(input$selectsite)})
  observeEvent(input$tempo, {saveSite(input$selectsite)})
  observeEvent(input$duree, {saveSite(input$selectsite)})
  observeEvent(input$intensite, {saveSite(input$selectsite)})
  observeEvent(input$portee, {saveSite(input$selectsite)})
  
  saveSite <- function(numero){
    if(numero > 0){
      name <- paste("Site no.", numero)
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
    updateSelectInput(session, "sitetype", selected = "1")
    updateNumericInput(session, "surface", value = 0.)
    updateNumericInput(session, "latitude", value = 0.)
    updateNumericInput(session, "longitude", value = 0.)
    updateTextAreaInput(session, "sitecontext", value = "", placeholder = "Décrire le contexte du site ici...")
    updateTextAreaInput(session, "descqual", value = "", placeholder = "Nature, emprise, effets indirects...")
    updateTextAreaInput(session, "tempo", value = "", placeholder = "Plusieurs phases? Court/long terme...")
    updateSelectInput(session, "duree", selected = "1")
    updateSelectInput(session, "intensite", selected = "1")
    updateSelectInput(session, "portee", selected = "1")
  }
  
  updateSiteName <- function(numero, sitename){
    if(sitename != ""){
      name <- paste("Site no.", numero)
      index <- which(listsite$site == name)
      listsite[index, 3] <<- input$sitename
      updateListSite(TRUE)
      updateListSiteImpactCompens()
      output$viewsiteno <- renderText({ paste("<font color=\"#005BBB\"; size=\"+2\"><b>", sitename, "</b></font>")})
      output$enjeusiteno <- renderText({ paste("<font color=\"#005BBB\"; size=\"+2\"><b>", sitename, "</b></font>")})
    }else{
      output$viewsiteno <- renderText({ paste("<font color=\"#005BBB\"; size=\"+2\"><b>", "SITE NUMERO", "</b></font>", "<font color=\"#005BBB\"; size=\"+2\"><b>", input$selectsite, "</b></font>") })
      output$enjeusiteno <- renderText({ paste("<font color=\"#005BBB\"; size=\"+2\"><b>", "ENJEUX DU SITE NUMERO", "</b></font>", "<font color=\"#005BBB\"; size=\"+2\"><b>", input$selectsite, "</b></font>") })
    }
  }
  
  updateDescTemp <- function(sitetype){
    name <- paste("Site no.", input$selectsite)
    index <- which(listsite$site == name)
    listsite[index, 4] <<- as.integer(input$sitetype)
    if(sitetype == 1){
      updateTextAreaInput(session, "descqual", "DESCRIPTION DES IMPACTS", placeholder = "Nature, emprise, effets indirects...")
      updateTextAreaInput(session, "tempo", "TEMPORALITE DES IMPACTS", placeholder = "Plusieurs phases? Court/long terme...")
      shinyjs::hide("presencespecies")
      shinyjs::hide("presencehabitat")
    }else if(sitetype == 2){
      updateTextAreaInput(session, "descqual", "DESCRIPTION DES MESURES COMPENSATOIRES", placeholder = "Nature, emprise, effets indirects...")
      updateTextAreaInput(session, "tempo", "TEMPORALITE DES MESURES COMPENSATOIRES", placeholder = "Plusieurs phases? Court/long terme...")
      if(as.integer(input$selectspecies) > 0) shinyjs::show("presencespecies")
      if(as.integer(input$selecthabitat) > 0) shinyjs::show("presencehabitat")
    }else if(sitetype == 3){
      updateTextAreaInput(session, "descqual", "DESCRIPTION DES IMPACTS ET DES MESURES COMPENSATOIRES", placeholder = "Nature, emprise, effets indirects...")
      updateTextAreaInput(session, "tempo", "TEMPORALITE DES IMPACTS ET DES MESURES COMPENSATOIRES", placeholder = "Plusieurs phases? Court/long terme...")
      shinyjs::hide("presencespecies")
      shinyjs::hide("presencehabitat")
    }
    updateListSiteImpactCompens()
  }
  
  updateListSite <- function(inplace=FALSE){
    showlist <- list()
    for(i in 1:dim(listsite)[1]){
      if(listsite[i,3] != "NA" & listsite[i,3] != "") showlist[[listsite[i,3]]] <- listsite[i,2]
      else showlist[[listsite[i,1]]] <- listsite[i,2]
    }
    if(inplace) updateSelectInput(session, "selectsite", choices = showlist, selected = input$selectsite)
    else updateSelectInput(session, "selectsite", choices = showlist, selected = showlist[[length(showlist)]])
  }
  
  
  ## SI
  updateListSiteImpactCompens <- function(){
    showlistimpact <- list()
    showlistcompens <- list()
    showlistimpact[['-']] <- 0
    showlistcompens[['-']] <- 0
    for(i in 1:dim(listsite)[1]){
      if(listsite[i,3] != "NA" & listsite[i,3] != ""){
        if(listsite[i,4] == 1 | listsite[i,4] == 3) showlistimpact[[listsite[i,3]]] <- listsite[i,2]
        if (listsite[i,4] == 2 | listsite[i,4] == 3) showlistcompens[[listsite[i,3]]] <- listsite[i,2]
      }else{
        if(listsite[i,4] == 1 | listsite[i,4] == 3) showlistimpact[[listsite[i,1]]] <- listsite[i,2]
        if (listsite[i,4] == 2 | listsite[i,4] == 3) showlistcompens[[listsite[i,1]]] <- listsite[i,2]
      }
    }
    updateSelectInput(session, "selectsiteimpact", choices = showlistimpact, selected = showlistimpact[[length(showlistimpact)]])
    updateSelectInput(session, "selectsitecompens", choices = showlistcompens, selected = showlistcompens[[length(showlistcompens)]])
  }
  
  observeEvent(input$selectsiteimpact, {
    # restore ecoval
    name <- paste("SIA1 no.", as.character(input$selectsiteimpact))
    tableau$A1 <- ecoval[[name]]
    name <- paste("SIA2 no.", as.character(input$selectsiteimpact))
    tableau$A2 <- ecoval[[name]]
    name <- paste("SIA3 no.", as.character(input$selectsiteimpact))
    tableau$A3 <- ecoval[[name]]
  })
  
  newSICX <- function(numsite){
    type <- input$sitetype
    if(type == "1" | type == "3"){  # impacte
      # A1
      newname <- paste("SIA1 no.", numsite)
      ecoval[[newname]] <<- model_A1  # create new DF
      # A2
      newname <- paste("SIA2 no.", numsite)
      ecoval[[newname]] <<- model_A2  # create new DF
      # A3
      newname <- paste("SIA3 no.", numsite)
      ecoval[[newname]] <<- model_A3  # create new DF
      
    }
    if(type == "2" | type == "3"){  # compensatoire
      
    }
  }
  
  delSICX <- function(numsite){
    type <- input$sitetype
    if(type == "1" | type == "3"){  # impacte
      # A1
      newname <- paste("SIA1 no.", numsite)
      ecoval[[newname]] <<- NULL  # delete DF
      # A2
      newname <- paste("SIA2 no.", numsite)
      ecoval[[newname]] <<- NULL  # delete DF
      # A3
      newname <- paste("SIA3 no.", numsite)
      ecoval[[newname]] <<- NULL  # delete DF
    }
    if(type == "2" | type == "3"){  # compensatoire
      
    }
  }
  
  ## SI A1
  observeEvent(input$addlisthab,{
    # data
    newDF <- data.frame(
      "Nom.habitat"=input$SInamehabitat,
      "Code.Corine"=input$SIcodecorine,
      "Code.Eunis"=input$SIcodeeunis,
      "Surface"=input$SIsurface,
      "Type"=as.character(A1listtype[input$SItype]),
      "Etat.conservation"=as.character(A1listetat[input$SIetat]),
      "Intérêt.communautaire"=as.character(A1listinter[input$SIinteret]),
      "En.danger.ou.menacé.localement"=as.character(A1listinter[input$SImenace]),
      "Surface.dégradée"=input$SIsurfacedeg)
    # array visu
    tableau$A1 <- rbind(tableau$A1, newDF)
    # save ecoval
    name <- paste("SIA1 no.", as.character(input$selectsiteimpact))
    ecoval[[name]] <<- tableau$A1
  })
  
  observeEvent(input$dellisthab,{
    rs <- as.numeric(input$SItable1_rows_selected)
    if(length(rs) > 0){
      # update array visu
      tableau$A1 <- tableau$A1[-rs,]
      # save ecoval
      name <- paste("SIA1 no.", as.character(input$selectsiteimpact))
      ecoval[[name]] <<- tableau$A1
    }
  })
  
  output$SItable1 <- renderDataTable({
     tableau$A1
  })
  
  ## SI A2
  observeEvent(input$addlistesp,{
    # data
    newDF <- data.frame(
      "Nom.Latin"=input$SIlatinnamespecies,
      "Nom.français"=input$SIfrenchnamespecies,
      "Type.1"=as.character(A2listtype1[input$SItype1]),
      "Type.2"=as.character(A2listtype2[input$SItype2]),
      "Protection.nationale.ou.régionale"=as.character(A2listprot[input$SIprotect]),
      "Liste.rouge..CR.VU.EN..France"=as.character(A2listprot[input$SIrougeF]),
      "Liste.rouge..CR.VU.EN..Régional"=as.character(A2listprot[input$SIrougeR]),
      "Directives.Européennes"=as.character(A2listdir[input$SIdirect]),
      "Reproduction"=as.character(A2listrepro[input$SIreprod]),
      "Indice.spécialisation"=SSI[as.integer(input$SIindssi)],
      "TVB"=as.character(A2listprot[input$SItvb]),
      "Déterminant.Znieff.dans.le.PE"=as.character(A2listprot[input$SIdet]),
      "Espèce.Exotique.Envahissante"=as.character(A2listprot[input$SIexo]))
    # array visu
    tableau$A2 <- rbind(tableau$A2, newDF)
    # save ecoval
    name <- paste("SIA2 no.", as.character(input$selectsiteimpact))
    ecoval[[name]] <<- tableau$A2
  })
  
  observeEvent(input$dellistesp,{
    rs <- as.numeric(input$SItable2_rows_selected)
    if(length(rs) > 0){
      # update array visu
      tableau$A2 <- tableau$A2[-rs,]
      # save ecoval
      name <- paste("SIA2 no.", as.character(input$selectsiteimpact))
      ecoval[[name]] <<- tableau$A2
    }
  })

   output$SItable2 <- renderDataTable({
     tableau$A2
   })
  
  observeEvent(input$SItype1,{
    if(input$SItype1 == "1") ltype2 <- list("Cortège forestier" = 1, "Cortège agricole" = 2, "Cortège du bâti" = 3, "Cortège généraliste" = 4)
    else if(input$SItype1 == "6") ltype2 <- list("Odonate" = 5, "Lépidoptère" = 6, "Orthoptère" = 7, "Coléoptère" = 8)
    else ltype2 <- list("-" = 0)
    updateSelectInput(session, "SItype2", choices = ltype2, selected = ltype2[[1]])
  })
  
  ## SI A3
  observeEvent(input$addlistper,{
    # data
    newDF <- data.frame(
      "Type"=as.character(A3listtype[input$SIpertype]),
      "Couche.SIG.EUNIS"=input$SIpercouche,
      "Code.SIG.OSO"=input$SIpercode,
      "Surface"=input$SIpersurf
      )
    # array visu
    tableau$A3 <- rbind(tableau$A3, newDF)
    # save ecoval
    name <- paste("SIA3 no.", as.character(input$selectsiteimpact))
    ecoval[[name]] <<- tableau$A3
  })
  
  observeEvent(input$dellistper,{
    rs <- as.numeric(input$SItable3_rows_selected)
    if(length(rs) > 0){
      # update array visu
      tableau$A3 <- tableau$A3[-rs,]
      # save ecoval
      name <- paste("SIA3 no.", as.character(input$selectsiteimpact))
      ecoval[[name]] <<- tableau$A3
    }
  })

  output$SItable3 <- renderDataTable({
    tableau$A3
  })
  
})
