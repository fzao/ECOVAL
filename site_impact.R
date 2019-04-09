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

output$SItable4 <- DT::renderDataTable(tableau$B, rownames=FALSE, selection='single')
