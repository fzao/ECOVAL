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

## SC
updateListHabitatSiteCompens <- function(){
  showlisthabitatcompens <- list()
  showlisthabitatcompens[['-']] <- 0
  name <- paste("Site no.", input$selectsitecompens)
  nbhabitat <- dim(listhabitat)[1] - 1
  if(nbhabitat > 0){
    for(i in 1:nbhabitat){
      hname <- listhabitat$habitat[i+1]
      if(exists(hname, where = ecoval)){
        if(ecoval[[hname]][7,2] == ecoval[[name]][12,2]){
          showlisthabitatcompens[[listhabitat$name[i+1]]] <- listhabitat$index[i+1]
        }
      }
    }
  }
  updateSelectInput(session, "selecthabitatSC", choices = showlisthabitatcompens, selected = showlisthabitatcompens[[length(showlisthabitatcompens)]])
}

updateListSpeciesSiteCompens <- function(){
  showlistspeciescompens <- list()
  showlistspeciescompens[['-']] <- 0
  name <- paste("Site no.", input$selectsitecompens)
  nbspecies <- dim(listspecies)[1] - 1
  if(nbspecies > 0){
    for(i in 1:nbspecies){
      spname <- listspecies$species[i+1]
      if(exists(spname, where = ecoval)){
        if(ecoval[[spname]][6,2] == ecoval[[name]][12,2]){
          showlistspeciescompens[[listspecies$name[i+1]]] <- listspecies$index[i+1]
        }
      }
    }
  }
  updateSelectInput(session, "selectspeciesSC", choices = showlistspeciescompens, selected = showlistspeciescompens[[length(showlistspeciescompens)]])
}

observeEvent(input$selectsitecompens, {
  if(input$selectsitecompens != '0'){
    # habitat
    updateListHabitatSiteCompens()
    # species
    updateListSpeciesSiteCompens()
    # restore ecoval
    name <- paste("SCA1 no.", input$selectsitecompens)
    tableau$A1 <- ecoval[[name]]
    name <- paste("SCA2 no.", input$selectsitecompens)
    tableau$A2 <- ecoval[[name]]
    name <- paste("SCA3 no.", input$selectsitecompens)
    tableau$A3 <- ecoval[[name]]
    name <- paste("SCB no.", input$selectsitecompens)
    tableau$B <- ecoval[[name]]
  }
  if(input$tabs == "compens") updateSelectInput(session, "selectsiteimpact", selected = "0")
})

## SC A1
observeEvent(input$addlisthab2,{
  # test validation 0
  if(is.na(input$SIsurface) | is.na(input$SIsurfacedeg)){
    showModal(modalDialog(h5("ERREUR SUR LES SURFACES"), hr(), "Une valeur numérique n'est pas correcte", easyClose = TRUE, footer = NULL))
    return(-1)
  }
  # test validation 1
  if(input$SCsurfacedeg > input$SCsurface){
    showModal(modalDialog(h5("ERREUR SUR LES SURFACES"), hr(), "La surface dégradée ne peut pas être plus grande que la surface initiale", easyClose = TRUE, footer = NULL))
    return(-1)
  }
  # test validation 2
  name <- paste("Site no.", input$selectsitecompens)
  surfacesite <- as.numeric(ecoval[[name]][3,2])
  if(is.na(surfacesite)) surfacesite <- 0.
  surfsomme <- 0.
  name <- paste("SCA1 no.", input$selectsitecompens)
  dimrow <- dim(ecoval[[name]])[1]
  if(dimrow > 0){
    for(i in 1:dimrow){
      surfsomme <- surfsomme + as.numeric(ecoval[[name]][i,4])
    }
  }
  surfsomme <- surfsomme + input$SCsurface
  if(surfsomme > surfacesite){
    showModal(modalDialog(h5("ERREUR SUR LES SURFACES"), hr(), "La surface totale du site ne doit pas être inférieure à la somme des surfaces d'habitats", easyClose = TRUE, footer = NULL))
    return(-1)
  }
  # data
  newDF <- data.frame(
    "Nom.habitat"=input$SCnamehabitat,
    "Code.Corine"=input$SCcodecorine,
    "Code.Eunis"=input$SCcodeeunis,
    "Surface"=as.character(input$SCsurface),
    "Type"=as.character(A1listtype[input$SCtype]),
    "Etat.conservation"=as.character(A1listetat[input$SCetat]),
    "Intérêt.communautaire"=as.character(A1listinter[input$SCinteret]),
    "En.danger.ou.menacé.localement"=as.character(A1listinter[input$SCmenace]),
    "Surface.dégradée"=as.character(input$SCsurfacedeg), stringsAsFactors=FALSE)
  # array visu
  tableau$A1 <- rbind(tableau$A1, newDF)
  # save ecoval
  ecoval[[name]] <<- tableau$A1
  updateTabB2()
  # clean widgets
  updateTextInput(session, "SCnamehabitat", value = "")
  updateTextInput(session, "SCcodecorine", value = "")
  updateTextInput(session, "SCcodeeunis", value = "")
  updateNumericInput(session, "SCsurface", value = 0.)
  updateSelectInput(session, "SCtype", selected = "1")
  updateSelectInput(session, "SCetat", selected = "1")
  updateSelectInput(session, "SCinteret", selected = "1")
  updateSelectInput(session, "SCmenace", selected = "1")
  updateNumericInput(session, "SCsurfacedeg", value = 0.)
})

observeEvent(input$dellisthab2,{
  rs <- as.numeric(input$SCtable1_rows_selected)
  if(length(rs) > 0){
    # update array visu
    tableau$A1 <- tableau$A1[-rs,]
    # save ecoval
    name <- paste("SCA1 no.", input$selectsitecompens)
    ecoval[[name]] <<- tableau$A1
    updateTabB2()
  }
})

output$SCtable1 <- DT::renderDataTable({
  dat <- datatable(tableau$A1, rownames = TRUE,
                   colnames = c("Nom habitat" = 2, "Code Corine" = 3, "Code Eunis" = 4, "Etat conservation" = 7, "Intérêt communautaire" = 8, "En danger ou menacé localement" = 9, "Surface dégradée" = 10),
                   options = list(pageLength = dim.data.frame(tableau$A1)[1], searching = FALSE, dom = 'ft', ordering = FALSE))
  return(dat)
})

## SC A2
observeEvent(input$addlistesp2,{
  # data
  if(input$SCtype1 == "1") ssival = SSI[as.integer(input$SCindssi)]
  else ssival = NA
  newDF <- data.frame(
    "Nom.Latin"=input$SClatinnamespecies,
    "Nom.français"=input$SCfrenchnamespecies,
    "Type.1"=as.character(A2listtype1[input$SCtype1]),
    "Type.2"=as.character(A2listtype2[input$SCtype2]),
    "Protection.nationale.ou.régionale"=as.character(A2listprot[input$SCprotect]),
    "Liste.rouge..CR.VU.EN..France"=as.character(A2listprot[input$SCrougeF]),
    "Liste.rouge..CR.VU.EN..Régional"=as.character(A2listprot[input$SCrougeR]),
    "Directives.Européennes"=as.character(A2listdir[input$SCdirect]),
    "Reproduction"=as.character(A2listrepro[input$SCreprod]),
    "Indice.spécialisation"=ssival,
    "TVB"=as.character(A2listprot[input$SCtvb]),
    "Déterminant.Znieff.dans.le.PE"=as.character(A2listprot[input$SCdet]),
    "Espèce.Exotique.Envahissante"=as.character(A2listprot[input$SCexo]),stringsAsFactors=FALSE)
  # array visu
  tableau$A2 <- rbind(tableau$A2, newDF)
  # save ecoval
  name <- paste("SCA2 no.", input$selectsitecompens)
  ecoval[[name]] <<- tableau$A2
  updateTabB2()
  # clean widgets
  updateTextInput(session, "SClatinnamespecies", value = "")
  updateTextInput(session, "SCfrenchnamespecies", value = "")
  updateSelectInput(session, "SCtype1", selected = "1")
  updateSelectInput(session, "SCtype2", selected = "1")
  updateSelectInput(session, "SCprotect", selected = "1")
  updateSelectInput(session, "SCrougeF", selected = "1")
  updateSelectInput(session, "SCrougeR", selected = "1")
  updateSelectInput(session, "SCdirect", selected = "0")
  updateSelectInput(session, "SCreprod", selected = "0")
  updateSelectInput(session, "SCexo", selected = "1")
  updateSelectInput(session, "SCtvb", selected = "1")
  updateSelectInput(session, "SCdet", selected = "1")
  updateSelectInput(session, "SCindssi", selected = "0")
})

observeEvent(input$dellistesp2,{
  rs <- as.numeric(input$SCtable2_rows_selected)
  if(length(rs) > 0){
    # update array visu
    tableau$A2 <- tableau$A2[-rs,]
    # save ecoval
    name <- paste("SCA2 no.", input$selectsitecompens)
    ecoval[[name]] <<- tableau$A2
    updateTabB2()
  }
})

output$SCtable2 <- DT::renderDataTable({
  dat <- datatable(tableau$A2, rownames = TRUE,
                   colnames = c("Nom Latin" = 2, "Nom français" = 3, "Type 1" = 4, "Type 2" = 5, "Protection nationale ou régionale" = 6, "Liste rouge (CR,VU,EN) France" = 7, "Liste rouge (CR,VU,EN) Régional" = 8, "Directives Européennes" = 9, "Indice spécialisation" = 11, "Déterminant Znieff dans le PE" = 13, "Espèce Exotique Envahissante" = 14),
                   options = list(pageLength = dim.data.frame(tableau$A2)[1], searching = FALSE, dom = 'ft', ordering = FALSE))
  return(dat)
})

observeEvent(input$SCtype1,{
  shinyjs::hide("SCindssi")
  if(input$SCtype1 == "1"){
    ltype2 <- list("Cortège forestier" = 1, "Cortège agricole" = 2, "Cortège du bâti" = 3, "Cortège généraliste" = 4)
    shinyjs::show("SCindssi")
  }
  else if(input$SCtype1 == "6") ltype2 <- list("Odonate" = 5, "Lépidoptère" = 6, "Orthoptère" = 7, "Coléoptère" = 8)
  else ltype2 <- list("-" = 0)
  updateSelectInput(session, "SCtype2", choices = ltype2, selected = ltype2[[1]])
})

## SC A3
observeEvent(input$addlistper2,{
  # data
  newDF <- data.frame(
    "Type"=as.character(A3listtype[input$SCpertype]),
    "Couche.SIG.EUNIS"=input$SCpercouche,
    "Code.SIG.OSO"=input$SCpercode,
    "Surface"=as.character(input$SCpersurf),stringsAsFactors=FALSE
  )
  # array visu
  tableau$A3 <- rbind(tableau$A3, newDF)
  # save ecoval
  name <- paste("SCA3 no.", input$selectsitecompens)
  ecoval[[name]] <<- tableau$A3
  updateTabB2()
  # clean widgets
  updateSelectInput(session, "SCpertype", selected = "1")
  updateTextInput(session, "SCpercouche", value = "")
  updateTextInput(session, "SCpercode", value = "")
  updateNumericInput(session, "SCpersurf", value = 0.)
})

observeEvent(input$dellistper2,{
  rs <- as.numeric(input$SCtable3_rows_selected)
  if(length(rs) > 0){
    # update array visu
    tableau$A3 <- tableau$A3[-rs,]
    # save ecoval
    name <- paste("SCA3 no.", input$selectsitecompens)
    ecoval[[name]] <<- tableau$A3
    updateTabB2()
  }
})

output$SCtable3 <- DT::renderDataTable({
  dat <- datatable(tableau$A3, rownames = TRUE,
                   colnames = c("Couche SIG EUNIS" = 3, "Couche SIG OSO" = 4),
                   options = list(pageLength = dim.data.frame(tableau$A3)[1], searching = FALSE, dom = 'ft', ordering = FALSE))
  return(dat)
})


## SC B
observeEvent(input$renseigner2,{
  rs <- as.numeric(input$SCtable4_rows_selected)
  if(length(rs) == 1){
    # initial state
    if(rs %in% c(13,39,44,45,46,47,48,56,61)){
      tableau$B[rs,4] <- input$Manuel2
    }
    # update array visu CT
    tableau$B[rs,5] <- input$SCjustifCT
    if(is.null(input$SCdegincCT)) tableau$B[rs,6] <- ""
    else{
      dimselect <- length(input$SCdegincCT)
      if(dimselect == 1) tableau$B[rs,6] <- "*"
      else if(dimselect == 2) tableau$B[rs,6] <- "**"
      else if(dimselect == 3) tableau$B[rs,6] <- "***"
    }
    tableau$B[rs,7] <- input$SCvalCT
    # update array visu LT
    tableau$B[rs,8] <- input$SCjustifLT
    if(is.null(input$SCdegincLT)) tableau$B[rs,9] <- ""
    else{
      dimselect <- length(input$SCdegincLT)
      if(dimselect == 1) tableau$B[rs,9] <- "*"
      else if(dimselect == 2) tableau$B[rs,9] <- "**"
      else if(dimselect == 3) tableau$B[rs,9] <- "***"
    }
    tableau$B[rs,10] <- input$SCvalLT
    # save ecoval
    name <- paste("SCB no.", input$selectsitecompens)
    ecoval[[name]] <<- tableau$B
    # clean widgets
    updateTextAreaInput(session, "SCjustifCT", value ="")
    updateCheckboxGroupInput(session, "SCdegincCT", selected = character(0))
    updateNumericInput(session, "SCvalCT", value = 0.)
    updateTextAreaInput(session, "SCjustifLT", value ="")
    updateCheckboxGroupInput(session, "SCdegincLT", selected = character(0))
    updateNumericInput(session, "SCvalLT", value = 0.)
    updateNumericInput(session, "Manuel2", value = 0.)
  }
})

updateTabB2 <- function(){
  # from A1
  n1 <- dim(tableau$A1)[1]
  if(n1 > 0){
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
    for(i in 1:n1){
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
  }
  # from A2
  n2 <- dim(tableau$A2)[1]
  if(n2 > 0){
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
    for(i in 1:n2){
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
  }
  # from A3
  n3 <- dim(tableau$A3)[1]
  if(n3 > 0){
    val59 <- 0.
    val60 <- 0.
    val50den <- 0.
    val51den <- 0.
    val52den <- 0.
    val53den <- 0.
    val54den <- 0.
    val55den <- 0.
    valsurf <- 0. # Surface global
    for(i in 1:n3){
      valsurf <- valsurf + as.numeric(tableau$A3[i,4]) # surface totale
      if(tableau$A3[i,1] == "Cultivé") val59 <- val59 + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Imperméabilisé") val60 <- val60 + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Fermé") val50den <- val50den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Ouvert") val51den <- val51den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Buissonnant") val52den <- val52den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Rocheux") val53den <- val53den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Zone humide") val54den <- val54den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Aquatique") val55den <- val55den + as.numeric(tableau$A3[i,4])
    }
  }
  if(n1 > 0 & n2 > 0 & n3 > 0){
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
    tableau$B[59,4] <- as.character(round(val59 * 100. / valsurf,2))
    tableau$B[60,4] <- as.character(round(val60 * 100. / valsurf,2))
    tableau$B[50,4] <- as.character(round(val50num * 100. / val50den,2))
    tableau$B[51,4] <- as.character(round(val51num * 100. / val51den,2))
    tableau$B[52,4] <- as.character(round(val52num * 100. / val52den,2))
    tableau$B[53,4] <- as.character(round(val53num * 100. / val53den,2))
    tableau$B[54,4] <- as.character(round(val54num * 100. / val54den,2))
    tableau$B[55,4] <- as.character(round(val55num * 100. / val55den,2))
    # save ecoval
    name <- paste("SCB no.", input$selectsitecompens)
    ecoval[[name]] <<- tableau$B
  }else{
    tableau$B[1:60,4] <- "0"
  }
}

output$SCtable4<- DT::renderDataTable({
  dat <- datatable(tableau$B, rownames = TRUE,
                   colnames = c("Valeur à l'état initial" = 5, "Justification de l'estimation CT" = 6, "Degré d'incertitude CT" = 7, "Valeur après compensation CT" = 8, "Justification de l'estimation LT" = 9, "Degré d'incertitude LT" = 10, "Valeur après compensation LT" = 11),
                   selection = 'single', options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$B)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top") %>%
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

## SC C
observeEvent(input$selecthabitatSC, {
  if(input$selecthabitatSC != '0'){
    name <- paste("CC no.", input$selecthabitatSC)
    tableau$C <- ecoval[[name]]
    shinyjs::show("SCjustifCTNH")
    shinyjs::show("SCdegincCTNH")
    shinyjs::show("SCvalCTNH")
    shinyjs::show("SCjustifLTNH")
    shinyjs::show("SCdegincLTNH")
    shinyjs::show("SCvalLTNH")
    shinyjs::show("renseignerNH2")
    shinyjs::show("ManuelNH2")
    shinyjs::show("justifySCNH")
    shinyjs::show("SCtable5")
    shinyjs::show("linkC3")
    shinyjs::show("linkC4")
  }else{
    shinyjs::hide("SCjustifCTNH")
    shinyjs::hide("SCdegincCTNH")
    shinyjs::hide("SCvalCTNH")
    shinyjs::hide("SCjustifLTNH")
    shinyjs::hide("SCdegincLTNH")
    shinyjs::hide("SCvalLTNH")
    shinyjs::hide("renseignerNH2")
    shinyjs::hide("ManuelNH2")
    shinyjs::hide("justifySCNH")
    shinyjs::hide("SCtable5")
    shinyjs::hide("linkC3")
    shinyjs::hide("linkC4")
  }
  if(input$descrcompens == "compensindicnh") updateSelectInput(session, "selecthabitatSI", selected = "0")
})

observeEvent(input$renseignerNH2,{
  rs <- as.numeric(input$SCtable5_rows_selected)
  if(length(rs) == 1){
    partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
    name  <- paste("Habitat", input$selecthabitatSC)
    if(ecoval[[name]][4,2] == "1"){ # Fermé
      partial_select <- c(partial_select, c(10,11,12,13,14,21))
    }else if(ecoval[[name]][4,2] == "2"){ # Ouvert
      partial_select <- c(partial_select, c(15, 22))
    }else if(ecoval[[name]][4,2] == "4"){ # Zone humide
      partial_select <- c(partial_select, c(16, 23))
    }
    pointer2row <- list()
    for(i in 1:length(partial_select)) pointer2row[i] <- partial_select[i]
    pointer2row <- as.integer(pointer2row)
    tableau$C[pointer2row[rs],4] <- input$ManuelNH2
    tableau$C[pointer2row[rs],5] <- input$justifySCNH
    # update array visu CT
    tableau$C[pointer2row[rs],6] <- input$SCjustifCTNH
    if(is.null(input$SCdegincCTNH)) tableau$C[pointer2row[rs],7] <- ""
    else{
      dimselect <- length(input$SCdegincCTNH)
      if(dimselect == 1) tableau$C[pointer2row[rs],7] <- "*"
      else if(dimselect == 2) tableau$C[pointer2row[rs],7] <- "**"
      else if(dimselect == 3) tableau$C[pointer2row[rs],7] <- "***"
    }
    tableau$C[pointer2row[rs],8] <- input$SCvalCTNH
    # update array visu LT
    tableau$C[pointer2row[rs],9] <- input$SCjustifLTNH
    if(is.null(input$SCdegincLTNH)) tableau$C[pointer2row[rs],10] <- ""
    else{
      dimselect <- length(input$SCdegincLTNH)
      if(dimselect == 1) tableau$C[pointer2row[rs],10] <- "*"
      else if(dimselect == 2) tableau$C[pointer2row[rs],10] <- "**"
      else if(dimselect == 3) tableau$C[pointer2row[rs],10] <- "***"
    }
    tableau$C[pointer2row[rs],11] <- input$SCvalLTNH
    # save ecoval
    name <- paste("CC no.", input$selecthabitatSC)
    ecoval[[name]] <<- tableau$C
    # clean widgets
    updateTextAreaInput(session, "SCjustifCTNH", value ="")
    updateCheckboxGroupInput(session, "SCdegincCTNH", selected = character(0))
    updateNumericInput(session, "SCvalCTNH", value = 0.)
    updateTextAreaInput(session, "SCjustifLTNH", value ="")
    updateCheckboxGroupInput(session, "SCdegincLTNH", selected = character(0))
    updateNumericInput(session, "SCvalLTNH", value = 0.)
    updateNumericInput(session, "ManuelNH2", value = 0.)
  }
})

output$SCtable5<- DT::renderDataTable({
  dat <- NULL
  if(input$selecthabitatSC != "0"){
    partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
    name  <- paste("Habitat", input$selecthabitatSC)
    if(ecoval[[name]][4,2] == "1"){ # Fermé
      partial_select <- c(partial_select, c(10,11,12,13,14,21))
    }else if(ecoval[[name]][4,2] == "2"){ # Ouvert
      partial_select <- c(partial_select, c(15, 22))
    }else if(ecoval[[name]][4,2] == "4"){ # Zone humide
      partial_select <- c(partial_select, c(16, 23))
    }
    viewTabC <- tableau$C[partial_select,]
    dat <- datatable(viewTabC, rownames = TRUE,
                     colnames = c("Valeur à l'état initial" = 5, "Justification" = 6, "Justification prédiction CT" = 7, "Incertitudes CT" = 8, "Valeur après compensation CT" = 9, "Justification prédiction LT" = 10, "Incertitudes LT" = 11, "Valeur après compensation LT" = 12),
                     selection = 'single', options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$C)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")%>%
      formatStyle(4, 3, backgroundColor = '#FFA02F')
  }
  return(dat)
})

## SC D
observeEvent(input$selectspeciesSC, {
  if(input$selectspeciesSC != '0'){
    name <- paste("DC no.", input$selectspeciesSC)
    tableau$D <- ecoval[[name]]
    shinyjs::show("SCjustifCTNSP")
    shinyjs::show("SCdegincCTNSP")
    shinyjs::show("SCvalCTNSP")
    shinyjs::show("SCjustifLTNSP")
    shinyjs::show("SCdegincLTNSP")
    shinyjs::show("SCvalLTNSP")
    shinyjs::show("renseignerNSP2")
    shinyjs::show("ManuelNSP2")
    shinyjs::show("justifySCNSP")
    shinyjs::show("SCtable6")
    shinyjs::show("linkC5")
    shinyjs::show("linkC6")
  }else{
    shinyjs::hide("SCjustifCTNSP")
    shinyjs::hide("SCdegincCTNSP")
    shinyjs::hide("SCvalCTNSP")
    shinyjs::hide("SCjustifLTNSP")
    shinyjs::hide("SCdegincLTNSP")
    shinyjs::hide("SCvalLTNSP")
    shinyjs::hide("renseignerNSP2")
    shinyjs::hide("ManuelNSP2")
    shinyjs::hide("justifySCNSP")
    shinyjs::hide("SCtable6")
    shinyjs::hide("linkC5")
    shinyjs::hide("linkC6")
  }
  if(input$descrcompens == "compensindicnsp") updateSelectInput(session, "selectspeciesSI", selected = "0")
})

observeEvent(input$renseignerNSP2,{
  rs <- as.numeric(input$SCtable6_rows_selected)
  if(length(rs) == 1){
    partial_select <- c(1,2,15,16,17,18,19)
    name  <- paste("Espece", input$selectspeciesSC)
    if(ecoval[[name]][3,2] != "7"){ # Faune
      partial_select <- c(partial_select, c(3))
    }
    if(ecoval[[name]][3,2] == "1"){ # Avifaune
      partial_select <- c(partial_select, c(4,5,6))
    }else if(ecoval[[name]][3,2] == "2"){ # Chiroptere
      partial_select <- c(partial_select, c(7,8))
    }else if(ecoval[[name]][3,2] == "4"){ # Amphibien
      partial_select <- c(partial_select, c(9,10))
    }else if(ecoval[[name]][3,2] == "6"){ # Insecte
      partial_select <- c(partial_select, c(11))
    }else if(ecoval[[name]][3,2] == "7"){ # Flore
      partial_select <- c(partial_select, c(12))
    }else if(ecoval[[name]][3,2] == "10"){ # Communaute faunistique
      partial_select <- c(partial_select, c(13,14))
    }
    pointer2row <- list()
    for(i in 1:length(partial_select)) pointer2row[i] <- partial_select[i]
    pointer2row <- as.integer(pointer2row)
    tableau$D[pointer2row[rs],4] <- input$ManuelNSP2
    tableau$D[pointer2row[rs],5] <- input$justifySCNSP
    # update array visu CT
    tableau$D[pointer2row[rs],6] <- input$SCjustifCTNSP
    if(is.null(input$SCdegincCTNSP)) tableau$D[pointer2row[rs],7] <- ""
    else{
      dimselect <- length(input$SCdegincCTNSP)
      if(dimselect == 1) tableau$D[pointer2row[rs],7] <- "*"
      else if(dimselect == 2) tableau$D[pointer2row[rs],7] <- "**"
      else if(dimselect == 3) tableau$D[pointer2row[rs],7] <- "***"
    }
    tableau$D[pointer2row[rs],8] <- input$SCvalCTNSP
    # update array visu LT
    tableau$D[pointer2row[rs],9] <- input$SCjustifLTNSP
    if(is.null(input$SCdegincLTNSP)) tableau$D[pointer2row[rs],10] <- ""
    else{
      dimselect <- length(input$SCdegincLTNSP)
      if(dimselect == 1) tableau$D[pointer2row[rs],10] <- "*"
      else if(dimselect == 2) tableau$D[pointer2row[rs],10] <- "**"
      else if(dimselect == 3) tableau$D[pointer2row[rs],10] <- "***"
    }
    tableau$D[pointer2row[rs],11] <- input$SCvalLTNSP
    # save ecoval
    name <- paste("DC no.", input$selectspeciesSC)
    ecoval[[name]] <<- tableau$D
    # clean widgets
    updateTextAreaInput(session, "SCjustifCTNSP", value ="")
    updateCheckboxGroupInput(session, "SCdegincCTNSP", selected = character(0))
    updateNumericInput(session, "SCvalCTNSP", value = 0.)
    updateTextAreaInput(session, "SCjustifLTNSP", value ="")
    updateCheckboxGroupInput(session, "SCdegincLTNSP", selected = character(0))
    updateNumericInput(session, "SCvalLTNSP", value = 0.)
    updateNumericInput(session, "ManuelNSP2", value = 0.)
  }
})

output$SCtable6<- DT::renderDataTable({
  dat <- NULL
  if(input$selectspeciesSC != "0"){
    partial_select <- c(1,2,15,16,17,18,19)
    name  <- paste("Espece", input$selectspeciesSC)
    if(ecoval[[name]][3,2] != "7"){ # Faune
      partial_select <- c(partial_select, c(3))
    }
    if(ecoval[[name]][3,2] == "1"){ # Avifaune
      partial_select <- c(partial_select, c(4,5,6))
    }else if(ecoval[[name]][3,2] == "2"){ # Chiroptere
      partial_select <- c(partial_select, c(7,8))
    }else if(ecoval[[name]][3,2] == "4"){ # Amphibien
      partial_select <- c(partial_select, c(9,10))
    }else if(ecoval[[name]][3,2] == "6"){ # Insecte
      partial_select <- c(partial_select, c(11))
    }else if(ecoval[[name]][3,2] == "7"){ # Flore
      partial_select <- c(partial_select, c(12))
    }else if(ecoval[[name]][3,2] == "10"){ # Communaute faunistique
      partial_select <- c(partial_select, c(13,14))
    }
    viewTabD <- tableau$D[partial_select,]
    dat <- datatable(viewTabD, rownames = TRUE,
                     colnames = c("Valeur à l'état initial" = 5, "Justification" = 6, "Justification prédiction CT" = 7, "Incertitudes CT" = 8, "Valeur après compensation CT" = 9, "Justification prédiction LT" = 10, "Incertitudes LT" = 11, "Valeur après compensation LT" = 12),
                     selection = 'single', options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$D)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")%>%
      formatStyle(4, 3, backgroundColor = '#FFA02F')
  }
  return(dat)
})
