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
# # Copyright (c) EDF-INRAE 2019-2020
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

#cfzxx
observeEvent(input$tabs, { 
  if(input$tabs == "impact") updateSelectInput(session, "selectsitecompens", selected = "0")
  if(input$tabs == "compens") updateSelectInput(session, "selectsiteimpact", selected = "0")
})

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
}

myA1 <- function(rs){
  # test validation 0
  if(is.na(input$SIsurface)){
    showModal(modalDialog(h5("ERREUR SUR LES SURFACES"), hr(), "Une valeur numérique n'est pas correcte", easyClose = TRUE, footer = NULL))
    return(-1)
  }

  # test validation 1
  name <- paste("Site no.", input$selectsiteimpact)
  surfacesite <- 0.
  if(!is.na(ecoval[[name]][3,2])) surfacesite <- as.numeric(ecoval[[name]][3,2])
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
    "En.danger.ou.menacé.localement"=as.character(A1listinter[input$SImenace]), stringsAsFactors=FALSE)
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
  }else cleanwidgetsA1()
  return(NULL)
})

## SI A2
cleanwidgetsA2 <- function(){
  updateTextInput(session, "SIlatinnamespecies", value = "")
  updateTextInput(session, "SIfrenchnamespecies", value = "")
  updateSelectInput(session, "SItype1", selected = "1")
  type2selection <<- "0"
  updateSelectInput(session, "SItype2", selected = type2selection)
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
    from_row_selection <<- TRUE
    updateTextInput(session, "SIlatinnamespecies", value = tableau$A2[rs, 1])
    updateTextInput(session, "SIfrenchnamespecies", value = tableau$A2[rs, 2])
    updateSelectInput(session, "SItype1", selected = names(A2listtype1)[match(tableau$A2[rs, 3], A2listtype1)])
    type2selection <<- names(A2listtype2)[match(tableau$A2[rs, 4], A2listtype2)]
    updateSelectInput(session, "SItype2", selected = type2selection)
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
  shinyjs::hide("linkinfoIS1")
  if(input$SItype1 == "1"){
    ltype2 <- list("-"=0, "Cortège généraliste"=1, "Cortège forestier"=2, "Cortège agricole ou ouvert"=3, "Cortège zone humide"=4, "Cortège du bâti"=5)
    shinyjs::show("SIindssi")
    shinyjs::show("linkinfoIS1")
  }
  else if(input$SItype1 == "6") ltype2 <- list("-"=0, "Odonate"=6,"Lépidoptère"=7,"Orthoptère"=8,"Coléoptère"=9)
  else ltype2 <- list("-" = 0)
  updateSelectInput(session, "SItype2", choices = ltype2, selected = type2selection)
})

observeEvent(input$linkinfoIS1, {
  showModal(modalDialog(
    h5("CLIQUEZ SUR L'ESP\u00C8CE CONCERN\u00C9E"), h5("l'indice de spécialisation apparaîtra automatiquement dans le tableau"), hr(), easyClose = TRUE, footer = NULL))
})

## SI A3
cleanwidgetsA3 <- function(){
  updateSelectInput(session, "SIpertype", selected = "1")
  updateTextInput(session, "SIpercouche", value = "")
  updateTextInput(session, "SIpercode", value = "")
  updateNumericInput(session, "SIpersurf", value = 0.)
}

myA3 <- function(rs){
  # test validation 0
  if(is.na(input$SIpersurf)){
    showModal(modalDialog(h5("ERREUR SUR LA SURFACE"), hr(), "Une valeur numérique n'est pas correcte", easyClose = TRUE, footer = NULL))
    return(-1)
  }
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
  val1 <- NA # Nombre d'habitat forestier
  val2 <- NA # Surface (ha) d'habitat forestier
  val3 <- NA # Nombre d'habitat ouvert
  val4 <- NA # Surface (ha) d'habitat ouvert
  val5 <- NA # Nombre d'habitat buissonnant
  val6 <- NA # Surface (ha) d'habitat buissonnant
  val7 <- NA # Nombre d'habitat rocheux
  val8 <- NA # Surface (ha) d'habitat rocheux
  val9 <- NA # Nombre de zone humide
  val10 <- NA # Surface (ha) de zone humide
  val11 <- NA # Nombre d'habitat aquatique
  val12 <- NA # Surface (ha) d'habitat aquatique
  val25 <- NA # Proportion surfacique des habitat menacés/en danger localement
  val26 <- NA # Proportion surfacique des habitats d'interet communautaire
  valsurf1 <- NA # Surface global
  val40num <- NA
  val40den <- NA
  val41num <- NA
  val41den <- NA
  val42num <- NA
  val42den <- NA
  val50num <- NA
  val51num <- NA
  val52num <- NA
  val53num <- NA
  val54num <- NA
  val55num <- NA
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
    val26 <- 0. # Proportion surfacique des habitats d'interet communautaire
    valsurf1 <- 0. # Surface global
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
      if(tableau$A1[i,5] == "Forestier"){
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
      valsurf1 <- valsurf1 + as.numeric(tableau$A1[i,4]) # surface totale
      if(tableau$A1[i,8] == "Oui") val25 <- val25 + as.numeric(tableau$A1[i,4])
      if(tableau$A1[i,7] == "Oui") val26 <- val26 + as.numeric(tableau$A1[i,4])
      val40den <- val40den + as.numeric(tableau$A1[i,4])
      if(tableau$A1[i,6] == "Bon") val40num <- val40num + as.numeric(tableau$A1[i,4])
      val41den <- val40den
      if(tableau$A1[i,5] != "Cultivé") val41num <- val41num + as.numeric(tableau$A1[i,4])
      val42den <- val40den
      if(tableau$A1[i,5] != "Imperméabilisé") val42num <- val42num + as.numeric(tableau$A1[i,4])
      if(tableau$A1[i,5] == "Forestier") val50num <- val50num + as.numeric(tableau$A1[i,4])
      if(tableau$A1[i,5] == "Ouvert") val51num <- val51num + as.numeric(tableau$A1[i,4])
      if(tableau$A1[i,5] == "Buissonnant") val52num <- val52num + as.numeric(tableau$A1[i,4])
      if(tableau$A1[i,5] == "Rocheux") val53num <- val53num + as.numeric(tableau$A1[i,4])
      if(tableau$A1[i,5] == "Zone humide") val54num <- val54num + as.numeric(tableau$A1[i,4])
      if(tableau$A1[i,5] == "Aquatique") val55num <- val55num + as.numeric(tableau$A1[i,4])
    }
  }
  # from A2
  n2 <- dim(tableau$A2)[1]
  val14 <- NA # Diversité avifaune
  val15 <- NA # Diversité chiroptères 
  val16 <- NA # Diversité reptiles
  val17 <- NA # Diversité amphibiens
  val18 <- NA # Diversité mammifères
  val19 <- NA # Diversité insectes
  val20 <- NA # Diversité lépidoptères
  val21 <- NA # Diversité odonates
  val22 <- NA # Diversité orthoptères
  val23 <- NA # Diversité coléoptères
  val24 <- NA # Diversité flore totale
  val27num <- NA
  val27den <- NA
  val28num <- NA
  val28den <- NA
  val29num <- NA
  val29den <- NA
  val30num <- NA
  val30den <- NA
  val31num <- NA
  val31den <- NA
  val32num <- NA
  val32den <- NA
  val33num <- NA
  val33den <- NA
  val34num <- NA
  val34den <- NA
  val35num <- NA
  val35den <- NA
  val36num <- NA
  val36den <- NA
  val37num <- NA
  val37den <- NA
  val38num <- NA
  val38den <- NA
  val43 <- NA
  val49 <- NA
  val57 <- NA
  val58 <- NA
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
      else if(tableau$A2[i,3] == "Flore") val24 <- val24 + 1
      if(tableau$A2[i,4] == "Lépidoptère") val20 <- val20 + 1
      if(tableau$A2[i,4] == "Odonate") val21 <- val21 + 1
      if(tableau$A2[i,4] == "Orthoptère") val22 <- val22 + 1
      if(tableau$A2[i,4] == "Coléoptère") val23 <- val23 + 1
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
      if(tableau$A2[i,3] == "Avifaune"){
        val35den <- val35den + 1
        if(tableau$A2[i,9] == "Certaine") val36num <- val36num + 1
      }
      val36den <- val35den
      if(tableau$A2[i,3] == "Avifaune"){
        if(!is.na(tableau$A2[i,10]) & tableau$A2[i,10] != ""){
          val38num <- val38num + as.numeric(tableau$A2[i,10])
          val38den <- val38den + 1
        }
      }else{
        if(tableau$A2[i,3] != "Flore"){
          val37den <- val37den + 1
          if(tableau$A2[i,9] == "Certaine") val37num <- val37num + 1
        }
      }
      if(tableau$A2[i,13] == "Oui") val43 <- val43 + 1
      if(tableau$A2[i,11] == "Oui") val49 <- val49 + 1
      if(tableau$A2[i,12] == "Oui" & tableau$A2[i,3] != "Flore") val57 <- val57 + 1
      if(tableau$A2[i,12] == "Oui" & tableau$A2[i,3] == "Flore") val58 <- val58 + 1
    }
  }
  # from A3
  n3 <- dim(tableau$A3)[1]
  val59 <- NA
  val60 <- NA
  val50den <- NA
  val51den <- NA
  val52den <- NA
  val53den <- NA
  val54den <- NA
  val55den <- NA
  valsurf3 <- NA # Surface global
  if(n3 > 0){
    val59 <- 0.
    val60 <- 0.
    val50den <- 0.
    val51den <- 0.
    val52den <- 0.
    val53den <- 0.
    val54den <- 0.
    val55den <- 0.
    valsurf3 <- 0. # Surface global
    for(i in 1:n3){
      valsurf3 <- valsurf3 + as.numeric(tableau$A3[i,4]) # surface totale
      if(tableau$A3[i,1] == "Cultivé") val59 <- val59 + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Imperméabilisé") val60 <- val60 + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Forestier") val50den <- val50den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Ouvert") val51den <- val51den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Buissonnant") val52den <- val52den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Rocheux") val53den <- val53den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Zone humide") val54den <- val54den + as.numeric(tableau$A3[i,4])
      if(tableau$A3[i,1] == "Aquatique") val55den <- val55den + as.numeric(tableau$A3[i,4])
    }
  }
    tableau$B[1:60,4] <- NA
    if(is.numeric(val1)) tableau$B[1,4] <- as.character(round(val1,1))
    if(is.numeric(val2)) tableau$B[2,4] <- as.character(round(val2,1))
    if(is.numeric(val3)) tableau$B[3,4] <- as.character(round(val3,1))
    if(is.numeric(val4)) tableau$B[4,4] <- as.character(round(val4,1))
    if(is.numeric(val5)) tableau$B[5,4] <- as.character(round(val5,1))
    if(is.numeric(val6)) tableau$B[6,4] <- as.character(round(val6,1))
    if(is.numeric(val7)) tableau$B[7,4] <- as.character(round(val7,1))
    if(is.numeric(val8)) tableau$B[8,4] <- as.character(round(val8,1))
    if(is.numeric(val9)) tableau$B[9,4] <- as.character(round(val9,1))
    if(is.numeric(val10)) tableau$B[10,4] <- as.character(round(val10,1))
    if(is.numeric(val11)) tableau$B[11,4] <- as.character(round(val11,1))
    if(is.numeric(val12)) tableau$B[12,4] <- as.character(round(val12,1))
    if(is.numeric(val25) & is.numeric(valsurf1)) tableau$B[25,4] <- as.character(round(val25 * 100. / valsurf1,1))
    if(is.numeric(val26) & is.numeric(valsurf1)) tableau$B[26,4] <- as.character(round(val26 * 100. / valsurf1,1))
    if(is.numeric(val40num) & is.numeric(val40den)) tableau$B[40,4] <- as.character(round(val40num * 100. / val40den,1))
    if(is.numeric(val41num) & is.numeric(val41den)) tableau$B[41,4] <- as.character(round(val41num * 100. / val41den,1))
    if(is.numeric(val42num) & is.numeric(val42den)) tableau$B[42,4] <- as.character(round(val42num * 100. / val42den,1))
    if(is.numeric(val14)) tableau$B[14,4] <- as.character(round(val14,1))
    if(is.numeric(val15)) tableau$B[15,4] <- as.character(round(val15,1))
    if(is.numeric(val16)) tableau$B[16,4] <- as.character(round(val16,1))
    if(is.numeric(val17)) tableau$B[17,4] <- as.character(round(val17,1))
    if(is.numeric(val18)) tableau$B[18,4] <- as.character(round(val18,1))
    if(is.numeric(val19)) tableau$B[19,4] <- as.character(round(val19,1))
    if(is.numeric(val20)) tableau$B[20,4] <- as.character(round(val20,1))
    if(is.numeric(val21)) tableau$B[21,4] <- as.character(round(val21,1))
    if(is.numeric(val22)) tableau$B[22,4] <- as.character(round(val22,1))
    if(is.numeric(val23)) tableau$B[23,4] <- as.character(round(val23,1))
    if(is.numeric(val24)) tableau$B[24,4] <- as.character(round(val24,1))
    if(is.numeric(val27num) & is.numeric(val27den)) tableau$B[27,4] <- as.character(round(val27num * 100 / val27den,1))
    if(is.numeric(val28num) & is.numeric(val28den)) tableau$B[28,4] <- as.character(round(val28num * 100 / val28den,1))
    if(is.numeric(val29num) & is.numeric(val29den)) tableau$B[29,4] <- as.character(round(val29num * 100 / val29den,1))
    if(is.numeric(val30num) & is.numeric(val30den)) tableau$B[30,4] <- as.character(round(val30num * 100 / val30den,1))
    if(is.numeric(val31num) & is.numeric(val31den)) tableau$B[31,4] <- as.character(round(val31num * 100 / val31den,1))
    if(is.numeric(val32num) & is.numeric(val32den)) tableau$B[32,4] <- as.character(round(val32num * 100 / val32den,1))
    if(is.numeric(val33num) & is.numeric(val33den)) tableau$B[33,4] <- as.character(round(val33num * 100 / val33den,1))
    if(is.numeric(val34num) & is.numeric(val34den)) tableau$B[34,4] <- as.character(round(val34num * 100 / val34den,1))
    if(is.numeric(val35num) & is.numeric(val35den)) tableau$B[35,4] <- as.character(round(val35num * 100 / val35den,1))
    if(is.numeric(val36num) & is.numeric(val36den)) tableau$B[36,4] <- as.character(round(val36num * 100 / val36den,1))
    if(is.numeric(val37num) & is.numeric(val37den)) tableau$B[37,4] <- as.character(round(val37num * 100 / val37den,1))
    if(is.numeric(val38num) & is.numeric(val38den)) tableau$B[38,4] <- as.character(round(val38num / val38den,1))
    if(is.numeric(val43)) tableau$B[43,4] <- as.character(round(val43,1))
    if(is.numeric(val49)) tableau$B[49,4] <- as.character(round(val49,1))
    if(is.numeric(val57)) tableau$B[57,4] <- as.character(round(val57,1))
    if(is.numeric(val58)) tableau$B[58,4] <- as.character(round(val58,1))
    if(is.numeric(val59) & is.numeric(valsurf3)) tableau$B[59,4] <- as.character(round(val59 * 100. / valsurf3,1))
    if(is.numeric(val60) & is.numeric(valsurf3)) tableau$B[60,4] <- as.character(round(val60 * 100. / valsurf3,1))
    if(is.numeric(val50num) & is.numeric(val50den)) tableau$B[50,4] <- as.character(round(val50num * 100. / val50den,1))
    if(is.numeric(val51num) & is.numeric(val51den)) tableau$B[51,4] <- as.character(round(val51num * 100. / val51den,1))
    if(is.numeric(val52num) & is.numeric(val52den)) tableau$B[52,4] <- as.character(round(val52num * 100. / val52den,1))
    if(is.numeric(val53num) & is.numeric(val53den)) tableau$B[53,4] <- as.character(round(val53num * 100. / val53den,1))
    if(is.numeric(val54num) & is.numeric(val54den))  tableau$B[54,4] <- as.character(round(val54num * 100. / val54den,1))
    if(is.numeric(val55num) & is.numeric(val55den)) tableau$B[55,4] <- as.character(round(val55num * 100. / val55den,1))
    # save ecoval
    tableau$B[which(tableau$B[1:60,4]=="NaN"),4] <- NA
    tableau$B[which(tableau$B[1:60,4]=="Inf"),4] <- NA
    tableau$B[which(tableau$B[1:60,4]=="-Inf"),4] <- NA
    name <- paste("SIB no.", input$selectsiteimpact)
    ecoval[[name]] <<- tableau$B
}

output$SItable4<- DT::renderDataTable({
  dat1 <- tableau$B
  dat1[, 1] <- as.factor(dat1[, 1])
  dat1[, 2] <- as.factor(dat1[, 2])
  dat1[, 3] <- as.factor(dat1[, 3])
  dat <- datatable(dat1, rownames = TRUE,
                   colnames = c("(\u03A3) Valeur à l'état initial" = 5, "Justification de l'estimation CT" = 6, "(\u03A3) Degré d'incertitude CT" = 7, "(\u03A3) Valeur après impact CT" = 8, "Justification de l'estimation LT" = 9, "(\u03A3) Degré d'incertitude LT" = 10, "(\u03A3) Valeur après impact LT" = 11),
                   selection = 'single', options = list(scrollY='300px', scrollCollapse=TRUE, pageLength = dim.data.frame(tableau$B)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top") %>%
                   formatStyle(4, 3, backgroundColor = styleEqual(colistcol, c(rep('#EBE491', 52))))

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
    shinyjs::show("linkET2")
    shinyjs::show("linkCT2")
    shinyjs::show("linkLT2")
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
    shinyjs::hide("linkET2")
    shinyjs::hide("linkCT2")
    shinyjs::hide("linkLT2")
  }
  if(input$descrimpact == "impactindicnh") updateSelectInput(session, "selecthabitatSC", selected = "0")
})

observeEvent(input$renseignerNH,{
  rs <- as.numeric(input$SItable5_rows_selected)
  if(length(rs) == 1){
    partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
    name  <- paste("Habitat", input$selecthabitatSI)
    if(ecoval[[name]][4,2] == "1"){ # Forestier
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
    if(ecoval[[name]][4,2] == "1"){ # Forestier
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
    if(ecoval[[name]][4,2] == "1"){ # Forestier
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
    shinyjs::show("linkCT3")
    shinyjs::show("linkLT3")
    shinyjs::show("linkET3")
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
    shinyjs::hide("linkCT3")
    shinyjs::hide("linkLT3")
    shinyjs::hide("linkET3")
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
