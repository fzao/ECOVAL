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

  #cfz source('site.R', local =TRUE)
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

  #####################
  # site.R
  #####################
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
            name <- paste("SIB no.", as.character(s))
            siname <- paste("SIB no.", as.character(index))
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
    
    # re-init
    ecoval <<- list()
    ecoval[["General"]] <<- model_info_general
    numsite <<- 0
    numspecies <<- 0
    numhabitat <<- 0
    listsite <<- data.frame("site" = '-', "index" = 0, "name" = '-', "type" = 0, stringsAsFactors=FALSE)
    listspecies <<- data.frame("species" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
    listhabitat <<- data.frame("habitat" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
    #tableau <<- reactiveValues(A1=NULL, A2=NULL, A3=NULL, B=NULL)
    tableau$A1 <- model_A1
    tableau$A2 <- model_A2
    tableau$A3 <- model_A3
    tableau$B <- model_B
    
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
        tableau$A1 <- ecoval[[siname]]
        siname <- paste("SIA2 no.", as.character(i))
        ecoval[[siname]] <<- read.xlsx2(inFile$datapath, sheetName = siname, header = TRUE, stringsAsFactors = FALSE)
        tableau$A2 <- ecoval[[siname]]
        siname <- paste("SIA3 no.", as.character(i))
        ecoval[[siname]] <<- read.xlsx2(inFile$datapath, sheetName = siname, header = TRUE, stringsAsFactors = FALSE)
        tableau$A3 <- ecoval[[siname]]
        siname <- paste("SIB no.", as.character(i))
        ecoval[[siname]] <<- read.xlsx2(inFile$datapath, sheetName = siname, header = TRUE, stringsAsFactors = FALSE)
        tableau$B <- ecoval[[siname]]
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
    updateListSite(0)
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
    updateListSite(2)
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
      updateListSite(2)
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
      updateListSite(1)
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
  
  updateListSite <- function(place){
    showlist <- list()
    for(i in 1:dim(listsite)[1]){
      if(listsite[i,3] != "NA" & listsite[i,3] != "") showlist[[listsite[i,3]]] <- listsite[i,2]
      else showlist[[listsite[i,1]]] <- listsite[i,2]
    }
    if(place==0) updateSelectInput(session, "selectsite", choices = showlist, selected = "0")
    else if(place==1) updateSelectInput(session, "selectsite", choices = showlist, selected = input$selectsite)
    else if(place==2) updateSelectInput(session, "selectsite", choices = showlist, selected = showlist[[length(showlist)]])
  }
  
  #
  # site_impact.R
  #
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
  
  updateListHabitatSiteImpact <- function(){
    showlisthabitatimpact <- list()
    showlisthabitatimpact[['-']] <- 0
    name <- paste("Site no.", input$selectsiteimpact)
    nbhabitat <- dim(listhabitat)[1] - 1
    if(nbhabitat > 0){
      for(i in 1:nbhabitat){
        hname <- listhabitat$habitat[i+1]
        if(exists(hname, where = ecoval)){
          if(ecoval[[hname]][7,2] == ecoval[[name]][12,2]){
            showlisthabitatimpact[[listhabitat$name[i+1]]] <- listhabitat$index[i+1]
          }
        }
      }
    }
    updateSelectInput(session, "selecthabitatSI", choices = showlisthabitatimpact, selected = showlisthabitatimpact[[length(showlisthabitatimpact)]])
  }
  
  updateListSpeciesSiteImpact <- function(){
    showlistspeciesimpact <- list()
    showlistspeciesimpact[['-']] <- 0
    name <- paste("Site no.", input$selectsiteimpact)
    nbspecies <- dim(listspecies)[1] - 1
    if(nbspecies > 0){
      for(i in 1:nbspecies){
        spname <- listspecies$species[i+1]
        if(exists(spname, where = ecoval)){
          if(ecoval[[spname]][6,2] == ecoval[[name]][12,2]){
            showlistspeciesimpact[[listspecies$name[i+1]]] <- listspecies$index[i+1]
          }
        }
      }
    }
    updateSelectInput(session, "selectspeciesSI", choices = showlistspeciesimpact, selected = showlistspeciesimpact[[length(showlistspeciesimpact)]])
  }
  
  observeEvent(input$selectsiteimpact, {
    if(input$selectsiteimpact != '0'){
      # habitat
      updateListHabitatSiteImpact()
      # species
      updateListSpeciesSiteImpact()
      # restore ecoval
      name <- paste("SIA1 no.", input$selectsiteimpact)
      tableau$A1 <- ecoval[[name]]
      name <- paste("SIA2 no.", input$selectsiteimpact)
      tableau$A2 <- ecoval[[name]]
      name <- paste("SIA3 no.", input$selectsiteimpact)
      tableau$A3 <- ecoval[[name]]
      name <- paste("SIB no.", input$selectsiteimpact)
      tableau$B <- ecoval[[name]]
    }
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
      # B
      newname <- paste("SIB no.", numsite)
      ecoval[[newname]] <<- model_B  # create new DF
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
      # B
      newname <- paste("SIB no.", numsite)
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
      "Surface"=as.character(input$SIsurface),
      "Type"=as.character(A1listtype[input$SItype]),
      "Etat.conservation"=as.character(A1listetat[input$SIetat]),
      "Intérêt.communautaire"=as.character(A1listinter[input$SIinteret]),
      "En.danger.ou.menacé.localement"=as.character(A1listinter[input$SImenace]),
      "Surface.dégradée"=as.character(input$SIsurfacedeg), stringsAsFactors=FALSE)
    # array visu
    tableau$A1 <- rbind(tableau$A1, newDF)
    # save ecoval
    name <- paste("SIA1 no.", input$selectsiteimpact)
    ecoval[[name]] <<- tableau$A1
    updateTabB()
    # clean widgets
    updateTextInput(session, "SInamehabitat", value = "")
    updateTextInput(session, "SIcodecorine", value = "")
    updateTextInput(session, "SIcodeeunis", value = "")
    updateNumericInput(session, "SIsurface", value = 0.)
    updateSelectInput(session, "SItype", selected = "1")
    updateSelectInput(session, "SIetat", selected = "1")
    updateSelectInput(session, "SIinteret", selected = "1")
    updateSelectInput(session, "SImenace", selected = "1")
    updateNumericInput(session, "SIsurfacedeg", value = 0.)
  })
  
  observeEvent(input$dellisthab,{
    rs <- as.numeric(input$SItable1_rows_selected)
    if(length(rs) > 0){
      # update array visu
      tableau$A1 <- tableau$A1[-rs,]
      # save ecoval
      name <- paste("SIA1 no.", input$selectsiteimpact)
      ecoval[[name]] <<- tableau$A1
      updateTabB()
    }
  })
  
  output$SItable1 <- DT::renderDataTable({
    dat <- datatable(tableau$A1, rownames = TRUE,
                     colnames = c("Nom habitat" = 2, "Code Corine" = 3, "Code Eunis" = 4, "Etat conservation" = 7, "Intérêt communautaire" = 8, "En danger ou menacé localement" = 9, "Surface dégradée" = 10),
                     options = list(pageLength = dim.data.frame(tableau$A1)[1], searching = FALSE, dom = 'ft', ordering = FALSE))
    return(dat)
  })
  
  ## SI A2
  observeEvent(input$addlistesp,{
    # data
    if(input$SItype1 == "1") ssival = SSI[as.integer(input$SIindssi)]
    else ssival = NA
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
      "Indice.spécialisation"=ssival,
      "TVB"=as.character(A2listprot[input$SItvb]),
      "Déterminant.Znieff.dans.le.PE"=as.character(A2listprot[input$SIdet]),
      "Espèce.Exotique.Envahissante"=as.character(A2listprot[input$SIexo]),stringsAsFactors=FALSE)
    # array visu
    tableau$A2 <- rbind(tableau$A2, newDF)
    # save ecoval
    name <- paste("SIA2 no.", input$selectsiteimpact)
    ecoval[[name]] <<- tableau$A2
    updateTabB()
    # clean widgets
    updateTextInput(session, "SIlatinnamespecies", value = "")
    updateTextInput(session, "SIfrenchnamespecies", value = "")
    updateSelectInput(session, "SItype1", selected = "1")
    updateSelectInput(session, "SItype2", selected = "1")
    updateSelectInput(session, "SIprotect", selected = "1")
    updateSelectInput(session, "SIrougeF", selected = "1")
    updateSelectInput(session, "SIrougeR", selected = "1")
    updateSelectInput(session, "SIdirect", selected = "0")
    updateSelectInput(session, "SIreprod", selected = "0")
    updateSelectInput(session, "SIexo", selected = "1")
    updateSelectInput(session, "SItvb", selected = "1")
    updateSelectInput(session, "SIdet", selected = "1")
    updateSelectInput(session, "SIindssi", selected = "0")
  })
  
  observeEvent(input$dellistesp,{
    rs <- as.numeric(input$SItable2_rows_selected)
    if(length(rs) > 0){
      # update array visu
      tableau$A2 <- tableau$A2[-rs,]
      # save ecoval
      name <- paste("SIA2 no.", input$selectsiteimpact)
      ecoval[[name]] <<- tableau$A2
      updateTabB()
    }
  })
  
  output$SItable2 <- DT::renderDataTable({
    dat <- datatable(tableau$A2, rownames = TRUE,
                     colnames = c("Nom Latin" = 2, "Nom français" = 3, "Type 1" = 4, "Type 2" = 5, "Protection nationale ou régionale" = 6, "Liste rouge (CR,VU,EN) France" = 7, "Liste rouge (CR,VU,EN) Régional" = 8, "Directives Européennes" = 9, "Indice spécialisation" = 11, "Déterminant Znieff dans le PE" = 13, "Espèce Exotique Envahissante" = 14),
                     options = list(pageLength = dim.data.frame(tableau$A2)[1], searching = FALSE, dom = 'ft', ordering = FALSE))
    return(dat)
  })
  
  observeEvent(input$SItype1,{
    shinyjs::hide("SIindssi")
    if(input$SItype1 == "1"){
      ltype2 <- list("Cortège forestier" = 1, "Cortège agricole" = 2, "Cortège du bâti" = 3, "Cortège généraliste" = 4)
      shinyjs::show("SIindssi")
    }
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
      "Surface"=as.character(input$SIpersurf),stringsAsFactors=FALSE
    )
    # array visu
    tableau$A3 <- rbind(tableau$A3, newDF)
    # save ecoval
    name <- paste("SIA3 no.", input$selectsiteimpact)
    ecoval[[name]] <<- tableau$A3
    updateTabB()
    # clean widgets
    updateSelectInput(session, "SIpertype", selected = "1")
    updateTextInput(session, "SIpercouche", value = "")
    updateTextInput(session, "SIpercode", value = "")
    updateNumericInput(session, "SIpersurf", value = 0.)
  })
  
  observeEvent(input$dellistper,{
    rs <- as.numeric(input$SItable3_rows_selected)
    if(length(rs) > 0){
      # update array visu
      tableau$A3 <- tableau$A3[-rs,]
      # save ecoval
      name <- paste("SIA3 no.", input$selectsiteimpact)
      ecoval[[name]] <<- tableau$A3
      updateTabB()
    }
  })
  
  output$SItable3 <- renderDataTable(tableau$A3, rownames=FALSE)
  output$SItable3 <- DT::renderDataTable({
    dat <- datatable(tableau$A3, rownames = TRUE,
                     colnames = c("Couche SIG EUNIS" = 3, "Couche SIG OSO" = 4),
                     options = list(pageLength = dim.data.frame(tableau$A3)[1], searching = FALSE, dom = 'ft', ordering = FALSE))
    return(dat)
  })
  
  
  ## SI B
  observeEvent(input$renseigner,{
    rs <- as.numeric(input$SItable4_rows_selected)
    if(length(rs) == 1){
      # initial state
      if(rs %in% c(13,39,44,45,46,47,48,56,61)){
        tableau$B[rs,4] <- input$Manuel
      }
      # update array visu CT
      tableau$B[rs,5] <- input$SIjustifCT
      if(is.null(input$SIdegincCT)) tableau$B[rs,6] <- ""
      else{
        dimselect <- length(input$SIdegincCT)
        if(dimselect == 1) tableau$B[rs,6] <- "*"
        else if(dimselect == 2) tableau$B[rs,6] <- "**"
        else if(dimselect == 3) tableau$B[rs,6] <- "***"
      }
      tableau$B[rs,7] <- input$SIvalCT
      # update array visu LT
      tableau$B[rs,8] <- input$SIjustifLT
      if(is.null(input$SIdegincLT)) tableau$B[rs,9] <- ""
      else{
        dimselect <- length(input$SIdegincLT)
        if(dimselect == 1) tableau$B[rs,9] <- "*"
        else if(dimselect == 2) tableau$B[rs,9] <- "**"
        else if(dimselect == 3) tableau$B[rs,9] <- "***"
      }
      tableau$B[rs,10] <- input$SIvalLT
      # save ecoval
      name <- paste("SIB no.", input$selectsiteimpact)
      ecoval[[name]] <<- tableau$B
    }
  })
  
  updateTabB <- function(){
    # from A1
    n <- dim(tableau$A1)[1]
    if(n > 0){
      val1 <- 0 # Nombre d'habitat forestier
      val2 <- 0. # Surface (ha) d'habitat forestier
      val3 <- 0 # Nombre d'habitat ouvert
      val4 <- 0. # Surface (ha) d'habitat ouvert
      val5 <- 0 # Nombre d'habitat buissonnant
      val6 <- 0. # Surface (ha) d'habitat buissonnant
      val7 <- 0 # Nombre d'habitat rocheux
      val8 <- 0. # Surface (ha) d'habitat rocheux
      val9 <- 0 # Nombre de zone humide
      val10 <- 0. # Surface (ha) de zone humide
      val11 <- 0 # Nombre d'habitat aquatique
      val12 <- 0. # Surface (ha) d'habitat aquatique
      val25 <- 0. # Proportion surfacique des habitat menacés/en danger localement
      valsurf <- 0. # Surface global
      val26 <- 0.
      val40num <- 0.
      val40den <- 0.
      val41num <- 0.
      val41den <- 0.
      val42num <- 0.
      val42den <- 0.
      val50num <- 0.
      val51num <- 0.
      val52num <- 0.
      val53num <- 0.
      val54num <- 0.
      val55num <- 0.
      for(i in 1:n){
        if(tableau$A1[i,5] == "Fermé"){
          val1 <-val1 + 1
          val2 <- val2 + as.numeric(tableau$A1[i,4])
        }else if(tableau$A1[i,5] == "Ouvert"){
          val3 <-val3 + 1
          val4 <- val4 + as.numeric(tableau$A1[i,4])
        }else if(tableau$A1[i,5] == "Buissonnant"){
          val5 <-val5 + 1
          val6 <- val6 + as.numeric(tableau$A1[i,4])
        }else if(tableau$A1[i,5] == "Rocheux"){
          val7 <-val7 + 1
          val8 <- val8 + as.numeric(tableau$A1[i,4])
        }else if(tableau$A1[i,5] == "Zone humide"){
          val9 <-val9 + 1
          val10 <- val10 + as.numeric(tableau$A1[i,4])
        }else if(tableau$A1[i,5] == "Aquatique"){
          val11 <-val11 + 1
          val12 <- val12 + as.numeric(tableau$A1[i,4])
        }
        valsurf <- valsurf + as.numeric(tableau$A1[i,4]) # surface totale
        if(tableau$A1[i,8] == "Oui") val25 <- val25 + as.numeric(tableau$A1[i,4])
        if(tableau$A1[i,7] == "Oui") val26 <- val26 + as.numeric(tableau$A1[i,4])
        val40den <- val40den + as.numeric(tableau$A1[i,4])
        if(tableau$A1[i,6] == "Bon") val40num <- val40num + as.numeric(tableau$A1[i,4])
        val41den <- val40den
        if(tableau$A1[i,5] != "Cultivé") val41num <- val41num + as.numeric(tableau$A1[i,4])
        val42den <- val40den
        if(tableau$A1[i,5] != "Imperméabilisé") val42num <- val42num + as.numeric(tableau$A1[i,4])
        if(tableau$A1[i,5] == "Fermé") val50num <- val50num + as.numeric(tableau$A1[i,4])
        if(tableau$A1[i,5] == "Ouvert") val51num <- val51num + as.numeric(tableau$A1[i,4])
        if(tableau$A1[i,5] == "Buissonnant") val52num <- val52num + as.numeric(tableau$A1[i,4])
        if(tableau$A1[i,5] == "Rocheux") val53num <- val53num + as.numeric(tableau$A1[i,4])
        if(tableau$A1[i,5] == "Zone humide") val54num <- val54num + as.numeric(tableau$A1[i,4])
        if(tableau$A1[i,5] == "Aquatique") val55num <- val55num + as.numeric(tableau$A1[i,4])
      }
      tableau$B[1,4] <- as.character(round(val1,2))
      tableau$B[2,4] <- as.character(round(val2,2))
      tableau$B[3,4] <- as.character(round(val3,2))
      tableau$B[4,4] <- as.character(round(val4,2))
      tableau$B[5,4] <- as.character(round(val5,2))
      tableau$B[6,4] <- as.character(round(val6,2))
      tableau$B[7,4] <- as.character(round(val7,2))
      tableau$B[8,4] <- as.character(round(val8,2))
      tableau$B[9,4] <- as.character(round(val9,2))
      tableau$B[10,4] <- as.character(round(val10,2))
      tableau$B[11,4] <- as.character(round(val11,2))
      tableau$B[12,4] <- as.character(round(val12,2))
      tableau$B[25,4] <- as.character(round(val25 * 100. / valsurf,2))
      tableau$B[26,4] <- as.character(round(val26 * 100. / valsurf,2))
      tableau$B[40,4] <- as.character(round(val40num * 100. / val40den,2))
      tableau$B[41,4] <- as.character(round(val41num * 100. / val41den,2))
      tableau$B[42,4] <- as.character(round(val42num * 100. / val42den,2))
    }
    # from A2
    n <- dim(tableau$A2)[1]
    if(n > 0){
      val14 <- 0 # Diversité avifaune
      val15 <- 0 # Diversité chiroptères 
      val16 <- 0 # Diversité reptiles
      val17 <- 0 # Diversité amphibiens
      val18 <- 0 # Diversité mammifères
      val19 <- 0 # Diversité insectes
      val20 <- 0 # Diversité lépidoptères
      val21 <- 0 # Diversité odonates
      val22 <- 0 # Diversité orthoptères
      val23 <- 0 # Diversité coléoptères
      val24 <- 0 # Diversité flore totale
      val27num <- 0.
      val27den <- 0.
      val28num <- 0.
      val28den <- 0.
      val29num <- 0.
      val29den <- 0.
      val30num <- 0.
      val30den <- 0.
      val31num <- 0.
      val31den <- 0.
      val32num <- 0.
      val32den <- 0.
      val33num <- 0.
      val33den <- 0.
      val34num <- 0.
      val34den <- 0.
      val35num <- 0.
      val35den <- 0.
      val36num <- 0.
      val36den <- 0.
      val37num <- 0.
      val37den <- 0.
      val38num <- 0.
      val38den <- 0.
      val43 <- 0
      val49 <- 0
      val57 <- 0
      val58 <- 0
      for(i in 1:n){
        if(tableau$A2[i,3] == "Avifaune") val14 <- val14 + 1
        else if(tableau$A2[i,3] == "Chiroptère") val15 <- val15 + 1
        else if(tableau$A2[i,3] == "Reptile") val16 <- val16 + 1
        else if(tableau$A2[i,3] == "Amphibien") val17 <- val17 + 1
        else if(tableau$A2[i,3] == "Mammifère") val18 <- val18 + 1
        else if(tableau$A2[i,3] == "Insecte") val19 <- val19 + 1
        else if(tableau$A2[i,4] == "Lépidoptère") val20 <- val20 + 1
        else if(tableau$A2[i,4] == "Odonate") val21 <- val21 + 1
        else if(tableau$A2[i,4] == "Orthoptère") val22 <- val22 + 1
        else if(tableau$A2[i,4] == "Coléoptère") val23 <- val23 + 1
        else if(tableau$A2[i,3] == "Flore") val24 <- val24 + 1
        if(tableau$A2[i,3] != "Flore"){
          if(tableau$A2[i,5] == "Oui") val27num <- val27num + 1
          val27den <- val27den + 1
        }
        if(tableau$A2[i,3] == "Flore"){
          if(tableau$A2[i,5] == "Oui") val28num <- val28num + 1
          val28den <- val28den + 1
        }
        if(tableau$A2[i,3] != "Flore"){
          if(tableau$A2[i,6] == "Oui") val29num <- val29num + 1
          val29den <- val29den + 1
        }
        if(tableau$A2[i,3] == "Flore"){
          if(tableau$A2[i,6] == "Oui") val30num <- val30num + 1
          val30den <- val30den + 1
        }
        if(tableau$A2[i,3] != "Flore"){
          if(tableau$A2[i,7] == "Oui") val31num <- val31num + 1
          val31den <- val31den + 1
        }
        if(tableau$A2[i,3] == "Flore"){
          if(tableau$A2[i,7] == "Oui") val32num <- val32num + 1
          val32den <- val32den + 1
        }
        if(tableau$A2[i,3] != "Flore"){
          if(tableau$A2[i,8] == "Annexe II DFFH") val33num <- val33num + 1
          val33den <- val33den + 1
        }
        if(tableau$A2[i,3] == "Flore"){
          if(tableau$A2[i,8] == "Annexe II DFFH") val34num <- val34num + 1
          val34den <- val34den + 1
        }
        if(tableau$A2[i,8] == "Annexe I DO") val35num <- val35num + 1
        if(tableau$A2[i,3] == "Avifaune") val35den <- val35den + 1
        if(tableau$A2[i,9] != "-") val36num <- val36num + 1
        val36den <- val35den
        val37num <- val36num
        if(tableau$A2[i,3] != "Avifaune" & tableau$A2[i,3] != "Flore") val37den <- val37den + 1
        if(tableau$A2[i,3] == "Avifaune"){
          val38num <- val38num + as.numeric(tableau$A2[i,10])
          val38den <- val38den + 1
        }
        if(tableau$A2[i,13] == "Oui") val43 <- val43 + 1
        if(tableau$A2[i,11] == "Oui") val49 <- val49 + 1
        if(tableau$A2[i,12] == "Oui" & tableau$A2[i,3] != "Flore") val57 <- val57 + 1
        if(tableau$A2[i,12] == "Oui" & tableau$A2[i,3] == "Flore") val58 <- val58 + 1
      }
      tableau$B[14,4] <- as.character(round(val14,2))
      tableau$B[15,4] <- as.character(round(val15,2))
      tableau$B[16,4] <- as.character(round(val16,2))
      tableau$B[17,4] <- as.character(round(val17,2))
      tableau$B[18,4] <- as.character(round(val18,2))
      tableau$B[19,4] <- as.character(round(val19,2))
      tableau$B[20,4] <- as.character(round(val20,2))
      tableau$B[21,4] <- as.character(round(val21,2))
      tableau$B[22,4] <- as.character(round(val22,2))
      tableau$B[23,4] <- as.character(round(val23,2))
      tableau$B[24,4] <- as.character(round(val24,2))
      tableau$B[27,4] <- as.character(round(val27num * 100 / val27den,2))
      tableau$B[28,4] <- as.character(round(val28num * 100 / val28den,2))
      tableau$B[29,4] <- as.character(round(val29num * 100 / val29den,2))
      tableau$B[30,4] <- as.character(round(val30num * 100 / val30den,2))
      tableau$B[31,4] <- as.character(round(val31num * 100 / val31den,2))
      tableau$B[32,4] <- as.character(round(val32num * 100 / val32den,2))
      tableau$B[33,4] <- as.character(round(val33num * 100 / val33den,2))
      tableau$B[34,4] <- as.character(round(val34num * 100 / val34den,2))
      tableau$B[35,4] <- as.character(round(val35num * 100 / val35den,2))
      tableau$B[36,4] <- as.character(round(val36num * 100 / val36den,2))
      tableau$B[37,4] <- as.character(round(val37num * 100 / val37den,2))
      tableau$B[38,4] <- as.character(round(val38num / val38den,2))
      tableau$B[43,4] <- as.character(round(val43,2))
      tableau$B[49,4] <- as.character(round(val49,2))
      tableau$B[57,4] <- as.character(round(val57,2))
      tableau$B[58,4] <- as.character(round(val58,2))
    }
    # from A3
    n <- dim(tableau$A3)[1]
    if(n > 0){
      val59 <- 0.
      val60 <- 0.
      val50den <- 0.
      val51den <- 0.
      val52den <- 0.
      val53den <- 0.
      val54den <- 0.
      val55den <- 0.
      for(i in 1:n){
        if(tableau$A3[i,1] == "Cultivé") val59 <- val59 + as.numeric(tableau$A3[i,4])
        if(tableau$A3[i,1] == "Imperméabilisé") val60 <- val60 + as.numeric(tableau$A3[i,4])
        if(tableau$A3[i,1] == "Fermé") val50den <- val50den + as.numeric(tableau$A3[i,4])
        if(tableau$A3[i,1] == "Ouvert") val51den <- val51den + as.numeric(tableau$A3[i,4])
        if(tableau$A3[i,1] == "Buissonnant") val52den <- val52den + as.numeric(tableau$A3[i,4])
        if(tableau$A3[i,1] == "Rocheux") val53den <- val53den + as.numeric(tableau$A3[i,4])
        if(tableau$A3[i,1] == "Zone humide") val54den <- val54den + as.numeric(tableau$A3[i,4])
        if(tableau$A3[i,1] == "Aquatique") val55den <- val55den + as.numeric(tableau$A3[i,4])
      }
      tableau$B[59,4] <- as.character(round(val59,2))
      tableau$B[60,4] <- as.character(round(val60,2))
      tableau$B[50,4] <- as.character(round(val50num * 100. / val50den,2))
      tableau$B[51,4] <- as.character(round(val51num * 100. / val51den,2))
      tableau$B[52,4] <- as.character(round(val52num * 100. / val52den,2))
      tableau$B[53,4] <- as.character(round(val53num * 100. / val53den,2))
      tableau$B[54,4] <- as.character(round(val54num * 100. / val54den,2))
      tableau$B[55,4] <- as.character(round(val55num * 100. / val55den,2))
    }
    # save ecoval
    name <- paste("SIB no.", input$selectsiteimpact)
    ecoval[[name]] <<- tableau$B
  }
  
  output$SItable4<- DT::renderDataTable({
    dat <- datatable(tableau$B, rownames = TRUE,
                     colnames = c("Valeur à l'état initial" = 5, "Justification de l'estimation CT" = 6, "Degré d'incertitude CT" = 7, "Valeur après impact CT" = 8, "Justification de l'estimation LT" = 9, "Degré d'incertitude LT" = 10, "Valeur après impact LT" = 11),
                     selection = 'single', options = list(pageLength = dim.data.frame(tableau$B)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top") %>%
      formatStyle(4, 3, backgroundColor = styleEqual(c('Longueur de lisière (Km) / surface de milieu forestier (Ha)',
                                                       'Proportion des chiroptères spécialistes',
                                                       'Nombre de patchs d\\\'EEE',
                                                       '% recouvrement des EEE',
                                                       'Longueur de linéaire de transport (Km)',
                                                       'Longueur de linéaire de haie (PS et PE)',
                                                       'Surface (Ha) de corridor traversant le site',
                                                       'Nombre d\\\'espaces protégé ou à enjeu (au moins 1/3 de la surface dans le PE)',
                                                       'Surface d\\\'EEE à proximité immédiate du PS'),
                                                     c(rep('#FFA02F', 9))))
    return(dat)
  })
  
  
})
