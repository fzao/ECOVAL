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

## SI
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
  if(input$tabs == "siteimpact") updateSelectInput(session, "selectsitecompens", selected = "0")
})

## SI A1
cleanwidgetsA1 <- function(){
  updateTextInput(session, "SInamehabitat", value = "")
  updateTextInput(session, "SIcodecorine", value = "")
  updateTextInput(session, "SIcodeeunis", value = "")
  updateNumericInput(session, "SIsurface", value = 0.)
  updateSelectInput(session, "SItype", selected = "1")
  updateSelectInput(session, "SIetat", selected = "0")
  updateSelectInput(session, "SIinteret", selected = "1")
  updateSelectInput(session, "SImenace", selected = "1")
  updateNumericInput(session, "SIsurfacedeg", value = 0.)
}

myA1 <- function(rs){
  # test validation 0
  if(is.na(input$SIsurface) | is.na(input$SIsurfacedeg)){
    showModal(modalDialog(h5("ERREUR SUR LES SURFACES"), hr(), "Une valeur numérique n'est pas correcte", easyClose = TRUE, footer = NULL))
    return(-1)
  }
  # test validation 1
  if(input$SIsurfacedeg > input$SIsurface){
    showModal(modalDialog(h5("ERREUR SUR LES SURFACES"), hr(), "La surface dégradée ne peut pas être plus grande que la surface initiale", easyClose = TRUE, footer = NULL))
    return(-1)
  }
  # test validation 2
  name <- paste("Site no.", input$selectsiteimpact)
  surfacesite <- 0.
  if(ecoval[[name]][3,2] != "NA") surfacesite <- as.numeric(ecoval[[name]][3,2])
  surfsomme <- 0.
  name <- paste("SIA1 no.", input$selectsiteimpact)
  dimrow <- dim(ecoval[[name]])[1]
  if(dimrow > 0){
    for(i in 1:dimrow){
      if(i != rs) surfsomme <- surfsomme + as.numeric(ecoval[[name]][i,4])
    }
  }
  surfsomme <- surfsomme + input$SIsurface
  if(surfsomme > surfacesite){
    showModal(modalDialog(h5("ERREUR SUR LES SURFACES"), hr(), "La surface totale du site ne doit pas être inférieure à la somme des surfaces d'habitats", easyClose = TRUE, footer = NULL))
    return(-1)
  }
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
  if(rs == 0) tableau$A1 <- rbind(tableau$A1, newDF)
  else tableau$A1[rs,] <- newDF
  # save ecoval
  ecoval[[name]] <<- tableau$A1
  updateTabB()
  # clean widgets
  cleanwidgetsA1()
}

observeEvent(input$addlisthab,{
  myA1(0)
})

observeEvent(input$chglisthab,{
  rs <- as.numeric(input$SItable1_rows_selected)
  if(length(rs) > 0) myA1(rs)
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
                   selection = 'single',
                   options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$A1)[1], searching = FALSE, dom = 'ft', ordering = FALSE))
  return(dat)
})

output$SItable1rowselected <- DT::renderDataTable({
  rs <- as.numeric(input$SItable1_rows_selected)
  if(length(rs) > 0){ # update contents of widgets
    updateTextInput(session, "SInamehabitat", value = tableau$A1[rs, 1])
    updateTextInput(session, "SIcodecorine", value = tableau$A1[rs, 2])
    updateTextInput(session, "SIcodeeunis", value = tableau$A1[rs, 3])
    updateNumericInput(session, "SIsurface", value = as.numeric(tableau$A1[rs, 4]))
    updateSelectInput(session, "SItype", selected = names(A1listtype)[match(tableau$A1[rs, 5], A1listtype)])
    updateSelectInput(session, "SIetat", selected = names(A1listetat)[match(tableau$A1[rs, 6], A1listetat)])
    updateSelectInput(session, "SIinteret", selected = names(A1listinter)[match(tableau$A1[rs, 7], A1listinter)])
    updateSelectInput(session, "SImenace", selected = names(A1listinter)[match(tableau$A1[rs, 8], A1listinter)])
    updateNumericInput(session, "SIsurfacedeg", value = as.numeric(tableau$A1[rs, 9]))
  }else cleanwidgetsA1()
  return(NULL)
})

## SI A2
cleanwidgetsA2 <- function(){
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
}

myA2 <- function(rs){
  # data
  if(input$SItype1 == "1"){
    if(rs == 0){
      ssival <- SSI[as.integer(input$SIindssi)]
    }else{
      if(input$SIindssi != as.character(length(Species))) ssival <- SSI[as.integer(input$SIindssi)]
      else ssival <- as.numeric(tableau$A2[rs, 10])
    }
  }else ssival = NA
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
  if(rs == 0) tableau$A2 <- rbind(tableau$A2, newDF)
  else tableau$A2[rs,] <- newDF
  # save ecoval
  name <- paste("SIA2 no.", input$selectsiteimpact)
  ecoval[[name]] <<- tableau$A2
  updateTabB()
  # clean widgets
  cleanwidgetsA2()
}

observeEvent(input$addlistesp,{
  myA2(0)
})

observeEvent(input$chglistesp,{
  rs <- as.numeric(input$SItable2_rows_selected)
  if(length(rs) > 0) myA2(rs)
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
                   selection = 'single',
                   options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$A2)[1], searching = FALSE, dom = 'ft', ordering = FALSE))
  return(dat)
})

output$SItable2rowselected <- DT::renderDataTable({
  rs <- as.numeric(input$SItable2_rows_selected)
  if(length(rs) > 0){ # update contents of widgets
    updateTextInput(session, "SIlatinnamespecies", value = tableau$A2[rs, 1])
    updateTextInput(session, "SIfrenchnamespecies", value = tableau$A2[rs, 2])
    updateSelectInput(session, "SItype1", selected = names(A2listtype1)[match(tableau$A2[rs, 3], A2listtype1)])
    updateSelectInput(session, "SItype2", selected = names(A2listtype2)[match(tableau$A2[rs, 4], A2listtype2)])
    updateSelectInput(session, "SIprotect", selected = names(A2listprot)[match(tableau$A2[rs, 5], A2listprot)])
    updateSelectInput(session, "SIrougeF", selected = names(A2listprot)[match(tableau$A2[rs, 6], A2listprot)])
    updateSelectInput(session, "SIrougeR", selected = names(A2listprot)[match(tableau$A2[rs, 7], A2listprot)])
    updateSelectInput(session, "SIdirect", selected = names(A2listdir)[match(tableau$A2[rs, 8], A2listdir)])
    updateSelectInput(session, "SIreprod", selected = names(A2listrepro)[match(tableau$A2[rs, 9], A2listrepro)])
    updateSelectInput(session, "SIexo", selected = names(A2listprot)[match(tableau$A2[rs, 13], A2listprot)])
    updateSelectInput(session, "SItvb", selected = names(A2listprot)[match(tableau$A2[rs, 11], A2listprot)])
    updateSelectInput(session, "SIdet", selected = names(A2listprot)[match(tableau$A2[rs, 12], A2listprot)])
    updateSelectInput(session, "SIindssi", selected = as.character(length(Species))) # unable to recover the species
  }else cleanwidgetsA2()
  return(NULL)
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
cleanwidgetsA3 <- function(){
  updateSelectInput(session, "SIpertype", selected = "1")
  updateTextInput(session, "SIpercouche", value = "")
  updateTextInput(session, "SIpercode", value = "")
  updateNumericInput(session, "SIpersurf", value = 0.)
}

myA3 <- function(rs){
  # data
  newDF <- data.frame(
    "Type"=as.character(A3listtype[input$SIpertype]),
    "Couche.SIG.EUNIS"=input$SIpercouche,
    "Code.SIG.OSO"=input$SIpercode,
    "Surface"=as.character(input$SIpersurf),stringsAsFactors=FALSE
  )
  # array visu
  if(rs == 0) tableau$A3 <- rbind(tableau$A3, newDF)
  else tableau$A3[rs,] <- newDF
  # save ecoval
  name <- paste("SIA3 no.", input$selectsiteimpact)
  ecoval[[name]] <<- tableau$A3
  updateTabB()
  # clean widgets
  cleanwidgetsA3()
}

observeEvent(input$addlistper,{
  myA3(0)
})

observeEvent(input$chglistper,{
  rs <- as.numeric(input$SItable3_rows_selected)
  if(length(rs) > 0) myA3(rs)
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

output$SItable3 <- DT::renderDataTable({
  dat <- datatable(tableau$A3, rownames = TRUE,
                   colnames = c("Couche SIG EUNIS" = 3, "Couche SIG OSO" = 4),
                   selection = 'single',
                   options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$A3)[1], searching = FALSE, dom = 'ft', ordering = FALSE))
  return(dat)
})

output$SItable3rowselected <- DT::renderDataTable({
  rs <- as.numeric(input$SItable3_rows_selected)
  if(length(rs) > 0){ # update contents of widgets
    updateSelectInput(session, "SIpertype", selected = names(A1listtype)[match(tableau$A3[rs, 1], A1listtype)])
    updateTextInput(session, "SIpercouche", value = tableau$A3[rs, 2])
    updateTextInput(session, "SIpercode", value = tableau$A3[rs, 3])
    updateNumericInput(session, "SIpersurf", value = as.numeric(tableau$A3[rs, 4]))
  }else cleanwidgetsA3()
  return(NULL)
})

## SI B
cleanwidgetsB <- function(){
  updateTextAreaInput(session, "SIjustifCT", value ="")
  updateCheckboxGroupInput(session, "SIdegincCT", selected = character(0))
  updateNumericInput(session, "SIvalCT", value = 0.)
  updateTextAreaInput(session, "SIjustifLT", value ="")
  updateCheckboxGroupInput(session, "SIdegincLT", selected = character(0))
  updateNumericInput(session, "SIvalLT", value = 0.)
  updateNumericInput(session, "Manuel", value = 0.)
}

observeEvent(input$renseigner,{
  rs <- as.numeric(input$SItable4_rows_selected)
  if(length(rs) == 1){
    # initial state
    if(!(rs %in% c(13,39,44,45,46,47,48,56,61))){
      if(input$Manuel != tableau$B[rs,4]) showModal(modalDialog(h5("La valeur de l'état initial a été modifiée !"), easyClose = TRUE, footer = NULL))
    }
    tableau$B[rs,4] <- input$Manuel
    # update array visu CT
    tableau$B[rs,5] <- input$SIjustifCT
    if(is.null(input$SIdegincCT)) tableau$B[rs,6] <- ""
    else{
      tableau$B[rs,6] <- paste(input$SIdegincCT, collapse = "")
    }
    tableau$B[rs,7] <- input$SIvalCT
    # update array visu LT
    tableau$B[rs,8] <- input$SIjustifLT
    if(is.null(input$SIdegincLT)) tableau$B[rs,9] <- ""
    else{
      tableau$B[rs,9] <- paste(input$SIdegincLT, collapse = "")
    }
    tableau$B[rs,10] <- input$SIvalLT
    # save ecoval
    name <- paste("SIB no.", input$selectsiteimpact)
    ecoval[[name]] <<- tableau$B
    # clean widgets
    cleanwidgetsB()
  }
})

updateTabB <- function(){
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
      if(tableau$A2[i,9] == "Certaine") val36num <- val36num + 1
      val36den <- val35den
      if(tableau$A2[i,3] == "Avifaune"){
        val38num <- val38num + as.numeric(tableau$A2[i,10])
        val38den <- val38den + 1
      }else{
        if(tableau$A2[i,9] == "Certaine") val37num <- val37num + 1
        val37den <- val37den + 1
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
    name <- paste("SIB no.", input$selectsiteimpact)
    ecoval[[name]] <<- tableau$B
  }else{
    tableau$B[1:60,4] <- "0"
  }
}

output$SItable4<- DT::renderDataTable({
  dat1 <- tableau$B
  dat1[, 1] <- as.factor(dat1[, 1])
  dat1[, 2] <- as.factor(dat1[, 2])
  dat1[, 3] <- as.factor(dat1[, 3])
  dat <- datatable(dat1, rownames = TRUE,
                   colnames = c("Valeur à l'état initial" = 5, "Justification de l'estimation CT" = 6, "Degré d'incertitude CT" = 7, "Valeur après impact CT" = 8, "Justification de l'estimation LT" = 9, "Degré d'incertitude LT" = 10, "Valeur après impact LT" = 11),
                   selection = 'single', options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$B)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top") %>%
    formatStyle(4, 3, backgroundColor = styleEqual(
                                                   c('Nombre d\\\'habitat forestier',
                                                     'Surface (ha) d\\\'habitat forestier',
                                                     'Nombre d\\\'habitat ouvert',
                                                     'Surface (ha) d\\\'habitat ouvert',
                                                     'Nombre d\\\'habitat buissonnant',
                                                     'Surface (ha) d\\\'habitat buissonnant',
                                                     'Nombre d\\\'habitat rocheux',
                                                     'Surface (ha) d\\\'habitat rocheux',
                                                     'Nombre de zone humide',
                                                     'Surface (ha) de zone humide',
                                                     'Nombre d\\\'habitat aquatique',
                                                     'Surface (ha) d\\\'habitat aquatique',
                                                     'Diversité avifaune',
                                                     'Diversité chiroptères',
                                                     'Diversité reptiles',
                                                     'Diversité amphibiens',
                                                     'Diversité mammifères',
                                                     'Diversité insectes',
                                                     'Diversité lépidoptères',
                                                     'Diversité odonates',
                                                     'Diversité orthoptères',
                                                     'Diversité coléoptères',
                                                     'Diversité flore totale',
                                                     'Proportion surfacique des habitat menacés/en danger localement',
                                                     'Proportion surfacique des habitat d\\\'intérêt communautaires (et prioritaires)',
                                                     'Proportion d\\\'espèces protégées faune au niveau national et regional',
                                                     'Proportion d\\\'espèces protégées flore au niveau national et regional',
                                                     'Proportion d\\\'espèces menacées faune au niveau national',
                                                     'Proportion d\\\'espèces menacées flore au niveau national',
                                                     'Proportion d\\\'espèces menacées faune au niveau regional',
                                                     'Proportion d\\\'espèces menacées flore au niveau regional',
                                                     'Proportion d\\\'espèces faune sur l\\\'annexe II de la DFFH',
                                                     'Proportion d\\\'espèces flore sur l\\\'annexe II de la DFFH',
                                                     'Proportion d\\\'avifaune sur la DO',
                                                     'Proportion d\\\'oiseaux nicheurs',
                                                     'Proportion d\\\'espèces (hors oiseaux) se reproduisant sur le site',
                                                     'Indice de spécialisation de l\\\'avifaune',
                                                     'Proportion surfacique des habitats en bon état de conservation',
                                                     '% de milieux NON cultivées',
                                                     '% de zones NON imperméabilisées',
                                                     'Nombre d\\\'espèces d\\\'EEE',
                                                     'Nombre d\\\'espèces de cohérence régional TVB dans PS',
                                                     '% Habitat forestier PS /PE',
                                                     '% Habitat ouvert PS /PE',
                                                     '% Habitat buissonnant PS /PE',
                                                     '% Habitat rocheux PS /PE',
                                                     '% Habitat humide PS /PE',
                                                     '% Habitat aquatique PS /PE',
                                                     'Nb espèces faune déterminante des Znieffs du PE',
                                                     'Nb espèces flore déterminante des Znieffs du PE',
                                                     '% Milieux cultivés',
                                                     '% Zones urbanisées'),
                                                   c(rep('#EBE491', 52))))
  return(dat)
})

output$SItable4rowselected <- DT::renderDataTable({
  rs <- as.numeric(input$SItable4_rows_selected)
  if(length(rs) > 0){ # update contents of widgets
    updateTextAreaInput(session, "SIjustifCT", value = tableau$B[rs, 5])
    updateCheckboxGroupInput(session, "SIdegincCT", selected = strsplit(tableau$B[rs, 6], "")[[1]])
    updateNumericInput(session, "SIvalCT", value = as.numeric(tableau$B[rs, 7]))
    updateTextAreaInput(session, "SIjustifLT", value = tableau$B[rs, 8])
    updateCheckboxGroupInput(session, "SIdegincLT", selected = strsplit(tableau$B[rs, 9], "")[[1]])
    updateNumericInput(session, "SIvalLT", value = as.numeric(tableau$B[rs, 10]))
    updateNumericInput(session, "Manuel", value = as.numeric(tableau$B[rs, 4]))
  }else cleanwidgetsB()
  return(NULL)
})


## SI C
cleanwidgetsC <- function(){
  updateTextAreaInput(session, "SIjustifCTNH", value ="")
  updateCheckboxGroupInput(session, "SIdegincCTNH", selected = character(0))
  updateNumericInput(session, "SIvalCTNH", value = 0.)
  updateTextAreaInput(session, "SIjustifLTNH", value ="")
  updateCheckboxGroupInput(session, "SIdegincLTNH", selected = character(0))
  updateNumericInput(session, "SIvalLTNH", value = 0.)
  updateNumericInput(session, "ManuelNH", value = 0.)
  updateTextInput(session, "justifySINH", value ="")
}

observeEvent(input$selecthabitatSI, {
  if(input$selecthabitatSI != '0'){
    name <- paste("CI no.", input$selecthabitatSI)
    tableau$C <- ecoval[[name]]
    shinyjs::show("SIjustifCTNH")
    shinyjs::show("SIdegincCTNH")
    shinyjs::show("SIvalCTNH")
    shinyjs::show("SIjustifLTNH")
    shinyjs::show("SIdegincLTNH")
    shinyjs::show("SIvalLTNH")
    shinyjs::show("renseignerNH")
    shinyjs::show("ManuelNH")
    shinyjs::show("justifySINH")
    shinyjs::show("SItable5")
    shinyjs::show("linkI3")
    shinyjs::show("linkI4")
    shinyjs::show("linkJ3")
    shinyjs::show("linkJ4")
  }else{
    shinyjs::hide("SIjustifCTNH")
    shinyjs::hide("SIdegincCTNH")
    shinyjs::hide("SIvalCTNH")
    shinyjs::hide("SIjustifLTNH")
    shinyjs::hide("SIdegincLTNH")
    shinyjs::hide("SIvalLTNH")
    shinyjs::hide("renseignerNH")
    shinyjs::hide("ManuelNH")
    shinyjs::hide("justifySINH")
    shinyjs::hide("SItable5")
    shinyjs::hide("linkI3")
    shinyjs::hide("linkI4")
    shinyjs::hide("linkJ3")
    shinyjs::hide("linkJ4")
  }
  if(input$descrimpact == "impactindicnh") updateSelectInput(session, "selecthabitatSC", selected = "0")
})

observeEvent(input$renseignerNH,{
  rs <- as.numeric(input$SItable5_rows_selected)
  if(length(rs) == 1){
    partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
    name  <- paste("Habitat", input$selecthabitatSI)
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
    tableau$C[pointer2row[rs],4] <- input$ManuelNH
    tableau$C[pointer2row[rs],5] <- input$justifySINH
    # update array visu CT
    tableau$C[pointer2row[rs],6] <- input$SIjustifCTNH
    if(is.null(input$SIdegincCTNH)) tableau$C[pointer2row[rs],7] <- ""
    else{
      tableau$C[pointer2row[rs],7] <- paste(input$SIdegincCTNH, collapse = "")
    }
    tableau$C[pointer2row[rs],8] <- input$SIvalCTNH
    # update array visu LT
    tableau$C[pointer2row[rs],9] <- input$SIjustifLTNH
    if(is.null(input$SIdegincLTNH)) tableau$C[pointer2row[rs],10] <- ""
    else{
      tableau$C[pointer2row[rs],10] <- paste(input$SIdegincLTNH, collapse = "")
    }
    tableau$C[pointer2row[rs],11] <- input$SIvalLTNH
    # save ecoval
    name <- paste("CI no.", input$selecthabitatSI)
    ecoval[[name]] <<- tableau$C
    # clean widgets
    cleanwidgetsC()
  }
})

output$SItable5<- DT::renderDataTable({
  dat <- NULL
  if(input$selecthabitatSI != "0"){
    partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
    name  <- paste("Habitat", input$selecthabitatSI)
    if(ecoval[[name]][4,2] == "1"){ # Fermé
      partial_select <- c(partial_select, c(10,11,12,13,14,21))
    }else if(ecoval[[name]][4,2] == "2"){ # Ouvert
      partial_select <- c(partial_select, c(15, 22))
    }else if(ecoval[[name]][4,2] == "4"){ # Zone humide
      partial_select <- c(partial_select, c(16, 23))
    }
    viewTabC <- tableau$C[partial_select,]
    viewTabC[, 1] <- as.factor(viewTabC[, 1])
    viewTabC[, 2] <- as.factor(viewTabC[, 2])
    viewTabC[, 3] <- as.factor(viewTabC[, 3])
    dat <- datatable(viewTabC, rownames = TRUE,
                     colnames = c("Valeur à l'état initial" = 5, "Détail" = 6, "Justification prédiction CT" = 7, "Incertitudes CT" = 8, "Valeur après impact CT" = 9, "Justification prédiction LT" = 10, "Incertitudes LT" = 11, "Valeur après impact LT" = 12),
                     selection = 'single', options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$C)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")
  }
  return(dat)
})

output$SItable5rowselected <- DT::renderDataTable({
  rs <- as.numeric(input$SItable5_rows_selected)
  if(length(rs) > 0){ # update contents of widgets
    partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
    name  <- paste("Habitat", input$selecthabitatSI)
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
    updateTextAreaInput(session, "SIjustifCTNH", value = tableau$C[pointer2row[rs], 6])
    updateCheckboxGroupInput(session, "SIdegincCTNH", selected = strsplit(tableau$C[pointer2row[rs], 7], "")[[1]])
    updateNumericInput(session, "SIvalCTNH", value = as.numeric(tableau$C[pointer2row[rs], 8]))
    updateTextAreaInput(session, "SIjustifLTNH", value = tableau$C[pointer2row[rs], 9])
    updateCheckboxGroupInput(session, "SIdegincLTNH", selected = strsplit(tableau$C[pointer2row[rs], 10], "")[[1]])
    updateNumericInput(session, "SIvalLTNH", value = as.numeric(tableau$C[pointer2row[rs], 11]))
    updateNumericInput(session, "ManuelNH", value = as.numeric(tableau$C[pointer2row[rs], 4]))
    updateTextInput(session, "justifySINH", value = tableau$C[pointer2row[rs], 5])
  }else cleanwidgetsC()
  return(NULL)
})


## SI D
cleanwidgetsD <- function(){
  updateTextAreaInput(session, "SIjustifCTNSP", value ="")
  updateCheckboxGroupInput(session, "SIdegincCTNSP", selected = character(0))
  updateNumericInput(session, "SIvalCTNSP", value = 0.)
  updateTextAreaInput(session, "SIjustifLTNSP", value ="")
  updateCheckboxGroupInput(session, "SIdegincLTNSP", selected = character(0))
  updateNumericInput(session, "SIvalLTNSP", value = 0.)
  updateNumericInput(session, "ManuelNSP", value = 0.)
  updateTextInput(session, "justifySINSP", value ="")
}

observeEvent(input$selectspeciesSI, {
  if(input$selectspeciesSI != '0'){
    name <- paste("DI no.", input$selectspeciesSI)
    tableau$D <- ecoval[[name]]
    shinyjs::show("SIjustifCTNSP")
    shinyjs::show("SIdegincCTNSP")
    shinyjs::show("SIvalCTNSP")
    shinyjs::show("SIjustifLTNSP")
    shinyjs::show("SIdegincLTNSP")
    shinyjs::show("SIvalLTNSP")
    shinyjs::show("renseignerNSP")
    shinyjs::show("ManuelNSP")
    shinyjs::show("justifySINSP")
    shinyjs::show("SItable6")
    shinyjs::show("linkI5")
    shinyjs::show("linkI6")
    shinyjs::show("linkJ5")
    shinyjs::show("linkJ6")
  }else{
    shinyjs::hide("SIjustifCTNSP")
    shinyjs::hide("SIdegincCTNSP")
    shinyjs::hide("SIvalCTNSP")
    shinyjs::hide("SIjustifLTNSP")
    shinyjs::hide("SIdegincLTNSP")
    shinyjs::hide("SIvalLTNSP")
    shinyjs::hide("renseignerNSP")
    shinyjs::hide("ManuelNSP")
    shinyjs::hide("justifySINSP")
    shinyjs::hide("SItable6")
    shinyjs::hide("linkI5")
    shinyjs::hide("linkI6")
    shinyjs::hide("linkJ5")
    shinyjs::hide("linkJ6")
  }
  if(input$descrimpact == "impactindicnsp") updateSelectInput(session, "selectspeciesSC", selected = "0")
})

observeEvent(input$renseignerNSP,{
  rs <- as.numeric(input$SItable6_rows_selected)
  if(length(rs) == 1){
    partial_select <- c(1,2,15,16,17,18,19)
    name  <- paste("Espece", input$selectspeciesSI)
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
    tableau$D[pointer2row[rs],4] <- input$ManuelNSP
    tableau$D[pointer2row[rs],5] <- input$justifySINSP
    # update array visu CT
    tableau$D[pointer2row[rs],6] <- input$SIjustifCTNSP
    if(is.null(input$SIdegincCTNSP)) tableau$D[pointer2row[rs],7] <- ""
    else{
      tableau$D[pointer2row[rs],7] <- paste(input$SIdegincCTNSP, collapse = "")
    }
    tableau$D[pointer2row[rs],8] <- input$SIvalCTNSP
    # update array visu LT
    tableau$D[pointer2row[rs],9] <- input$SIjustifLTNSP
    if(is.null(input$SIdegincLTNSP)) tableau$D[pointer2row[rs],10] <- ""
    else{
      tableau$D[pointer2row[rs],10] <- paste(input$SIdegincLTNSP, collapse = "")
    }
    tableau$D[pointer2row[rs],11] <- input$SIvalLTNSP
    # save ecoval
    name <- paste("DI no.", input$selectspeciesSI)
    ecoval[[name]] <<- tableau$D
    # clean widgets
    cleanwidgetsD()
  }
})

output$SItable6<- DT::renderDataTable({
  dat <- NULL
  if(input$selectspeciesSI != "0"){
    partial_select <- c(1,2,15,16,17,18,19)
    name  <- paste("Espece", input$selectspeciesSI)
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
    viewTabD[, 1] <- as.factor(viewTabD[, 1])
    viewTabD[, 2] <- as.factor(viewTabD[, 2])
    viewTabD[, 3] <- as.factor(viewTabD[, 3])
    dat <- datatable(viewTabD, rownames = TRUE,
                     colnames = c("Valeur à l'état initial" = 5, "Détail" = 6, "Justification prédiction CT" = 7, "Incertitudes CT" = 8, "Valeur après impact CT" = 9, "Justification prédiction LT" = 10, "Incertitudes LT" = 11, "Valeur après impact LT" = 12),
                     selection = 'single', options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$D)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")
  }
  return(dat)
})

output$SItable6rowselected <- DT::renderDataTable({
  rs <- as.numeric(input$SItable6_rows_selected)
  if(length(rs) > 0){ # update contents of widgets
    partial_select <- c(1,2,15,16,17,18,19)
    name  <- paste("Espece", input$selectspeciesSI)
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
    updateTextAreaInput(session, "SIjustifCTNSP", value = tableau$D[pointer2row[rs], 6])
    updateCheckboxGroupInput(session, "SIdegincCTNSP", selected = strsplit(tableau$D[pointer2row[rs], 7], "")[[1]])
    updateNumericInput(session, "SIvalCTNSP", value = as.numeric(tableau$D[pointer2row[rs], 8]))
    updateTextAreaInput(session, "SIjustifLTNSP", value = tableau$D[pointer2row[rs], 9])
    updateCheckboxGroupInput(session, "SIdegincLTNSP", selected = strsplit(tableau$D[pointer2row[rs], 10], "")[[1]])
    updateNumericInput(session, "SIvalLTNSP", value = as.numeric(tableau$D[pointer2row[rs], 11]))
    updateNumericInput(session, "ManuelNSP", value = as.numeric(tableau$D[pointer2row[rs], 4]))
    updateTextInput(session, "justifySINSP", value = tableau$D[pointer2row[rs], 5])
  }else cleanwidgetsD()
  return(NULL)
})
