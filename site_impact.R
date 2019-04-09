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
  name <- paste("SIB no.", as.character(input$selectsiteimpact))
  tableau$B <- ecoval[[name]]
  
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
    "Surface.dégradée"=as.character(input$SIsurfacedeg))
  # array visu
  tableau$A1 <- rbind(tableau$A1, newDF)
  # save ecoval
  name <- paste("SIA1 no.", as.character(input$selectsiteimpact))
  ecoval[[name]] <<- tableau$A1
  updateTabB()
})

observeEvent(input$dellisthab,{
  rs <- as.numeric(input$SItable1_rows_selected)
  if(length(rs) > 0){
    # update array visu
    tableau$A1 <- tableau$A1[-rs,]
    # save ecoval
    name <- paste("SIA1 no.", as.character(input$selectsiteimpact))
    ecoval[[name]] <<- tableau$A1
    updateTabB()
  }
})

output$SItable1 <- renderDataTable(tableau$A1, rownames=FALSE)

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
    "Espèce.Exotique.Envahissante"=as.character(A2listprot[input$SIexo]))
  # array visu
  tableau$A2 <- rbind(tableau$A2, newDF)
  # save ecoval
  name <- paste("SIA2 no.", as.character(input$selectsiteimpact))
  ecoval[[name]] <<- tableau$A2
  updateTabB()
})

observeEvent(input$dellistesp,{
  rs <- as.numeric(input$SItable2_rows_selected)
  if(length(rs) > 0){
    # update array visu
    tableau$A2 <- tableau$A2[-rs,]
    # save ecoval
    name <- paste("SIA2 no.", as.character(input$selectsiteimpact))
    ecoval[[name]] <<- tableau$A2
    updateTabB()
  }
})

output$SItable2 <- renderDataTable(tableau$A2, rownames=FALSE)

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
    "Surface"=as.character(input$SIpersurf)
  )
  # array visu
  tableau$A3 <- rbind(tableau$A3, newDF)
  # save ecoval
  name <- paste("SIA3 no.", as.character(input$selectsiteimpact))
  ecoval[[name]] <<- tableau$A3
  updateTabB()
})

observeEvent(input$dellistper,{
  rs <- as.numeric(input$SItable3_rows_selected)
  if(length(rs) > 0){
    # update array visu
    tableau$A3 <- tableau$A3[-rs,]
    # save ecoval
    name <- paste("SIA3 no.", as.character(input$selectsiteimpact))
    ecoval[[name]] <<- tableau$A3
    updateTabB()
  }
})

output$SItable3 <- renderDataTable(tableau$A3, rownames=FALSE)

## SI B
observeEvent(input$renseigner,{
  rs <- as.numeric(input$SItable4_rows_selected)
  if(length(rs) == 1){
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
    name <- paste("SIB no.", as.character(input$selectsiteimpact))
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
    tableau$B[1,4] <- as.character(val1)
    tableau$B[2,4] <- as.character(val2)
    tableau$B[3,4] <- as.character(val3)
    tableau$B[4,4] <- as.character(val4)
    tableau$B[5,4] <- as.character(val5)
    tableau$B[6,4] <- as.character(val6)
    tableau$B[7,4] <- as.character(val7)
    tableau$B[8,4] <- as.character(val8)
    tableau$B[9,4] <- as.character(val9)
    tableau$B[10,4] <- as.character(val10)
    tableau$B[11,4] <- as.character(val11)
    tableau$B[12,4] <- as.character(val12)
    tableau$B[25,4] <- as.character(val25 * 100. / valsurf)
    tableau$B[26,4] <- as.character(val26 * 100. / valsurf)
    tableau$B[40,4] <- as.character(val40num * 100. / val40den)
    tableau$B[41,4] <- as.character(val41num * 100. / val41den)
    tableau$B[42,4] <- as.character(val42num * 100. / val42den)
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
    tableau$B[14,4] <- as.character(val14)
    tableau$B[15,4] <- as.character(val15)
    tableau$B[16,4] <- as.character(val16)
    tableau$B[17,4] <- as.character(val17)
    tableau$B[18,4] <- as.character(val18)
    tableau$B[19,4] <- as.character(val19)
    tableau$B[20,4] <- as.character(val20)
    tableau$B[21,4] <- as.character(val21)
    tableau$B[22,4] <- as.character(val22)
    tableau$B[23,4] <- as.character(val23)
    tableau$B[24,4] <- as.character(val24)
    tableau$B[27,4] <- as.character(val27num * 100 / val27den)
    tableau$B[28,4] <- as.character(val28num * 100 / val28den)
    tableau$B[29,4] <- as.character(val29num * 100 / val29den)
    tableau$B[30,4] <- as.character(val30num * 100 / val30den)
    tableau$B[31,4] <- as.character(val31num * 100 / val31den)
    tableau$B[32,4] <- as.character(val32num * 100 / val32den)
    tableau$B[33,4] <- as.character(val33num * 100 / val33den)
    tableau$B[34,4] <- as.character(val34num * 100 / val34den)
    tableau$B[35,4] <- as.character(val35num * 100 / val35den)
    tableau$B[36,4] <- as.character(val36num * 100 / val36den)
    tableau$B[37,4] <- as.character(val37num * 100 / val37den)
    tableau$B[38,4] <- as.character(val38num / val38den)
    tableau$B[43,4] <- as.character(val43)
    tableau$B[49,4] <- as.character(val49)
    tableau$B[57,4] <- as.character(val57)
    tableau$B[58,4] <- as.character(val58)
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
    tableau$B[59,4] <- as.character(val59)
    tableau$B[60,4] <- as.character(val60)
    tableau$B[50,4] <- as.character(val50num * 100. / val50den)
    tableau$B[51,4] <- as.character(val51num * 100. / val51den)
    tableau$B[52,4] <- as.character(val52num * 100. / val52den)
    tableau$B[53,4] <- as.character(val53num * 100. / val53den)
    tableau$B[54,4] <- as.character(val54num * 100. / val54den)
    tableau$B[55,4] <- as.character(val55num * 100. / val55den)
  }
  # save ecoval
  name <- paste("SIB no.", as.character(input$selectsiteimpact))
  ecoval[[name]] <<- tableau$B
}

output$SItable4 <- DT::renderDataTable(tableau$B, rownames=FALSE, selection='single')
