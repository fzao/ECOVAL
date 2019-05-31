#
#   ______ _____ ______      __     _      
#  |  ____/ ____/ __ \ \    / /\   | |     
#  | |__ | |   | |  | \ \  / /  \  | |     
#  |  __|| |   | |  | |\ \/ / /\ \ | |     
#  | |___| |___| |__| | \  / ____ \| |____ 
#  |______\_____\____/   \/_/    \_\______|
#
# Cadre methodologique pour le calcul de l\'equivalence ecologique dans le contexte de la sequence ERC en France
#
# # Copyright (c) EDF-IRSTEA 2019
#
# Auteurs : Fabrice Zaoui - Lucie Bezombes
#
# Licence CeCILL v2.1
#

updateListHabitatSiteEquivalence <- function(){
  showlisthabitatequivalence <- list()
  showlisthabitatequivalence[['-']] <- 0
  if(input$selectsiteimpact3 != '0'){
    name <- paste("Site no.", input$selectsiteimpact3)
    nbhabitat <- dim(listhabitat)[1] - 1
    if(nbhabitat > 0){
      for(i in 1:nbhabitat){
        hname <- listhabitat$habitat[i+1]
        if(exists(hname, where = ecoval)){
          if(ecoval[[hname]][7,2] == ecoval[[name]][12,2]){
            showlisthabitatequivalence[[listhabitat$name[i+1]]] <- listhabitat$index[i+1]
          }
        }
      }
    }
  }
  if(input$selectsitecompens3 != '0'){
    name <- paste("Site no.", input$selectsitecompens3)
    nbhabitat <- dim(listhabitat)[1] - 1
    if(nbhabitat > 0){
      for(i in 1:nbhabitat){
        hname <- listhabitat$habitat[i+1]
        if(exists(hname, where = ecoval)){
          if(ecoval[[hname]][7,2] == ecoval[[name]][12,2]){
            showlisthabitatequivalence[[listhabitat$name[i+1]]] <- listhabitat$index[i+1]
          }
        }
      }
    }
  }
  updateSelectInput(session, "selecthabitatSE", choices = showlisthabitatequivalence, selected = showlisthabitatequivalence[[length(showlisthabitatequivalence)]])
}

updateListSpeciesSiteEquivalence <- function(){
  showlistspeciesequivalence <- list()
  showlistspeciesequivalence[['-']] <- 0
  if(input$selectsiteimpact3 != '0'){
    name <- paste("Site no.", input$selectsiteimpact3)
    nbspecies <- dim(listspecies)[1] - 1
    if(nbspecies > 0){
      for(i in 1:nbspecies){
        spname <- listspecies$species[i+1]
        if(exists(spname, where = ecoval)){
          if(ecoval[[spname]][6,2] == ecoval[[name]][12,2]){
            showlistspeciesequivalence[[listspecies$name[i+1]]] <- listspecies$index[i+1]
          }
        }
      }
    }
  }
  if(input$selectsitecompens3 != '0'){
    name <- paste("Site no.", input$selectsitecompens3)
    nbspecies <- dim(listspecies)[1] - 1
    if(nbspecies > 0){
      for(i in 1:nbspecies){
        spname <- listspecies$species[i+1]
        if(exists(spname, where = ecoval)){
          if(ecoval[[spname]][6,2] == ecoval[[name]][12,2]){
            showlistspeciesequivalence[[listspecies$name[i+1]]] <- listspecies$index[i+1]
          }
        }
      }
    }
  }
  updateSelectInput(session, "selectspeciesSE", choices = showlistspeciesequivalence, selected = showlistspeciesequivalence[[length(showlistspeciesequivalence)]])
}

observeEvent(input$selectsiteimpact3, {
  updateListHabitatSiteEquivalence()
  updateListSpeciesSiteEquivalence()
  updateTabsetPanel(session, "resultequivalence", selected = "graphe")
  if(input$selectsiteimpact3 == '0'){
    shinyjs::hide("plot_equivalence")
    shinyjs::hide("SEcalcul")
    shinyjs::hide("dwnlequivalence")
    shinyjs::hide("selecttypegraphequivalence")
    shinyjs::hide("selectniveauequivalence")
  }else{
    if(input$selectsitecompens3 == '0'){
      shinyjs::hide("plot_equivalence")
      shinyjs::hide("SEcalcul")
      shinyjs::hide("dwnlequivalence")
      shinyjs::hide("selecttypegraphequivalence")
      shinyjs::hide("selectniveauequivalence")
    }else{
      shinyjs::show("plot_equivalence")
      shinyjs::show("SEcalcul")
      shinyjs::show("dwnlequivalence")
      shinyjs::show("selecttypegraphequivalence")
      shinyjs::show("selectniveauequivalence")
    }
  }
})

observeEvent(input$selectsitecompens3, {
  updateListHabitatSiteEquivalence()
  updateListSpeciesSiteEquivalence()
  updateTabsetPanel(session, "resultequivalence", selected = "graphe")
  if(input$selectsitecompens3 == '0'){
    shinyjs::hide("plot_equivalence")
    shinyjs::hide("SEcalcul")
    shinyjs::hide("dwnlequivalence")
    shinyjs::hide("selecttypegraphequivalence")
    shinyjs::hide("selectniveauequivalence")
  }else{
    if(input$selectsiteimpact3 == '0'){
      shinyjs::hide("plot_equivalence")
      shinyjs::hide("SEcalcul")
      shinyjs::hide("dwnlequivalence")
      shinyjs::hide("selecttypegraphequivalence")
      shinyjs::hide("selectniveauequivalence")
    }else{
      shinyjs::show("plot_equivalence")
      shinyjs::show("SEcalcul")
      shinyjs::show("dwnlequivalence")
      shinyjs::show("selecttypegraphequivalence")
      shinyjs::show("selectniveauequivalence")
    }
  }
})

observeEvent(input$selectniveauequivalence, {
  updateTabsetPanel(session, "resultequivalence", selected = "graphe")
  if(input$selectniveauequivalence == '1'){
    shinyjs::hide("selecthabitatSE")
    shinyjs::hide("selectspeciesSE")
  }else if(input$selectniveauequivalence == '2'){
    shinyjs::show("selecthabitatSE")
    shinyjs::hide("selectspeciesSE")
  }else if(input$selectniveauequivalence == '3'){
    shinyjs::hide("selecthabitatSE")
    shinyjs::show("selectspeciesSE")
  }
})

observeEvent(input$selecttypegraphequivalence, updateTabsetPanel(session, "resultequivalence", selected = "graphe"))
observeEvent(input$selecthabitatSE, updateTabsetPanel(session, "resultequivalence", selected = "graphe"))
observeEvent(input$selectspeciesSE, updateTabsetPanel(session, "resultequivalence", selected = "graphe"))

output$plot_equivalence <- renderPlotly({
  if(input$selectsiteimpact3 != '0' & input$selectsitecompens3 != '0'){
    # Niveau General
    if(input$selectniveauequivalence == '1'){
      nameImp <- paste("SIB no.", input$selectsiteimpact3)
      nameComp <- paste("SCB no.", input$selectsitecompens3)
      if(input$selecttypegraphequivalence == '1'){
      # Equivalence CT
        moins <- as.numeric(ecoval[[nameImp]][[7]]) - as.numeric(ecoval[[nameImp]][[4]])
        plus <- as.numeric(ecoval[[nameComp]][[7]]) - as.numeric(ecoval[[nameComp]][[4]])
        equivalCT <- moins + plus
        natzero <- (equivalCT == 0) & (moins != 0)
        natzero[natzero==TRUE] <- '*'
        natzero[natzero==FALSE] <- ''
        dat1 <- data.frame(
          perimetres = ecoval[[nameImp]][[1]],
          indicateurs = ecoval[[nameImp]][[3]],
          criteres = factor(ecoval[[nameImp]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          equivalence = equivalCT)
        p <- ggplot(data=dat1, aes(x=criteres, y=equivalence, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
        dat1["100% compensé"] <- natzero
        dat1["pertes brutes"] <- moins
        dat1["gains brutes"] <- plus
      }else if(input$selecttypegraphequivalence == '2'){
      # Equivalence LT
        moins <- as.numeric(ecoval[[nameImp]][[10]]) - as.numeric(ecoval[[nameImp]][[4]])
        plus <- as.numeric(ecoval[[nameComp]][[10]]) - as.numeric(ecoval[[nameComp]][[4]])
        equivalLT <- moins + plus
        natzero <- (equivalLT == 0) & (moins != 0)
        natzero[natzero==TRUE] <- '*'
        natzero[natzero==FALSE] <- ''
        dat1 <- data.frame(
          perimetres = ecoval[[nameImp]][[1]],
          indicateurs = ecoval[[nameImp]][[3]],
          criteres = factor(ecoval[[nameImp]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          equivalence = equivalLT)
        p <- ggplot(data=dat1, aes(x=criteres, y=equivalence, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
        dat1["100% compensé"] <- natzero
        dat1["pertes brutes"] <- moins
        dat1["gains brutes"] <- plus
      }
      p <- ggplotly(p, width=500, height=1000)
    }else if(input$selectniveauequivalence == '2'){
      # Niveau Habitat
      if(input$selecthabitatSE != '0'){
        shinyjs::show("dwnlequivalence")
        numHabitat <- as.numeric(input$selecthabitatSE)
        nameHabitat <- listhabitat$name[numHabitat+1]
        indicehabitat <- which(listhabitat$name == nameHabitat, arr.ind = TRUE)
        nameI <- paste("Site no.", input$selectsiteimpact3) # Impact
        nameC <- paste("Site no.", input$selectsitecompens3) # Compens
        moins <- c(rep(0., dim(model_C)[1]))
        plus <- c(rep(0., dim(model_C)[1]))
        if(input$selecttypegraphequivalence == '1'){
        # Equivalence CT
          for(i in indicehabitat){
            hname <- listhabitat$habitat[i]
            if(exists(hname, where = ecoval)){
              if(ecoval[[hname]][7,2] == ecoval[[nameI]][12,2]){ # Pertes
                nameImp <- paste("CI no.", as.character(listhabitat$index[i]))
                nameIC <- nameImp
                moins <- as.numeric(ecoval[[nameImp]][[7]]) - as.numeric(ecoval[[nameImp]][[4]])
              }
              if(ecoval[[hname]][7,2] == ecoval[[nameC]][12,2]){ # Gains
                nameComp <- paste("CC no.", as.character(listhabitat$index[i]))
                nameIC <- nameComp
                plus <- as.numeric(ecoval[[nameComp]][[7]]) - as.numeric(ecoval[[nameComp]][[4]])
              }
            }
          }
        }else if(input$selecttypegraphequivalence == '2'){
        # Equivalence LT
          for(i in indicehabitat){
            hname <- listhabitat$habitat[i]
            if(exists(hname, where = ecoval)){
              if(ecoval[[hname]][7,2] == ecoval[[nameI]][12,2]){ # Pertes
                nameImp <- paste("CI no.", as.character(listhabitat$index[i]))
                nameIC <- nameImp
                moins <- as.numeric(ecoval[[nameImp]][[10]]) - as.numeric(ecoval[[nameImp]][[4]])
              }
              if(ecoval[[hname]][7,2] == ecoval[[nameC]][12,2]){ # Gains
                nameComp <- paste("CC no.", as.character(listhabitat$index[i]))
                nameIC <- nameComp
                plus <- as.numeric(ecoval[[nameComp]][[10]]) - as.numeric(ecoval[[nameComp]][[4]])
              }
            }
          }
        }
        equival <- moins + plus
        natzero <- (equival == 0) & (moins != 0)
        natzero[natzero==TRUE] <- '*'
        natzero[natzero==FALSE] <- ''
        dat1 <- data.frame(
          perimetres = ecoval[[nameIC]][[1]],
          indicateurs = ecoval[[nameIC]][[3]],
          criteres = factor(ecoval[[nameIC]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
          equivalence = equival)
        p <- ggplot(data=dat1, aes(x=criteres, y=equivalence, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
        dat1["100% compensé"] <- natzero
        dat1["pertes brutes"] <- moins
        dat1["gains brutes"] <- plus
      }else{
        shinyjs::hide("dwnlequivalence")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
      p <- ggplotly(p, width=500, height=1000)
    }else if(input$selectniveauequivalence == '3'){
      # Niveau Espece
      if(input$selectspeciesSE != '0'){
        shinyjs::show("dwnlequivalence")
        numSpecies <- as.numeric(input$selectspeciesSE)
        nameSpecies <- listspecies$name[numSpecies+1]
        indicespecies <- which(listspecies$name == nameSpecies, arr.ind = TRUE)
        nameI <- paste("Site no.", input$selectsiteimpact3) # Impact
        nameC <- paste("Site no.", input$selectsitecompens3) # Compens
        moins <- c(rep(0., dim(model_D)[1]))
        plus <- c(rep(0., dim(model_D)[1]))
        if(input$selecttypegraphequivalence == '1'){
        # Equivalence CT
          for(i in indicespecies){
            sname <- listspecies$species[i]
            if(exists(sname, where = ecoval)){
              if(ecoval[[sname]][6,2] == ecoval[[nameI]][12,2]){ # Pertes
                nameImp <- paste("DI no.", as.character(listspecies$index[i]))
                nameIC <- nameImp
                moins <- as.numeric(ecoval[[nameImp]][[7]]) - as.numeric(ecoval[[nameImp]][[4]])
              }
              if(ecoval[[sname]][6,2] == ecoval[[nameC]][12,2]){ # Gains
                nameComp <- paste("DC no.", as.character(listspecies$index[i]))
                nameIC <- nameComp
                plus <- as.numeric(ecoval[[nameComp]][[7]]) - as.numeric(ecoval[[nameComp]][[4]])
              }
            }
          }
        }else if(input$selecttypegraphequivalence == '2'){
        # Equivalence LT
          for(i in indicespecies){
            sname <- listspecies$species[i]
            if(exists(sname, where = ecoval)){
              if(ecoval[[sname]][6,2] == ecoval[[nameI]][12,2]){ # Pertes
                nameImp <- paste("DI no.", as.character(listspecies$index[i]))
                nameIC <- nameImp
                moins <- as.numeric(ecoval[[nameImp]][[10]]) - as.numeric(ecoval[[nameImp]][[4]])
              }
              if(ecoval[[sname]][6,2] == ecoval[[nameC]][12,2]){ # Gains
                nameComp <- paste("DC no.", as.character(listspecies$index[i]))
                nameIC <- nameComp
                plus <- as.numeric(ecoval[[nameComp]][[10]]) - as.numeric(ecoval[[nameComp]][[4]])
              }
            }
          }
        }
        equival <- moins + plus
        natzero <- (equival == 0) & (moins != 0)
        natzero[natzero==TRUE] <- '*'
        natzero[natzero==FALSE] <- ''
        dat1 <- data.frame(
          perimetres = ecoval[[nameIC]][[1]],
          indicateurs = ecoval[[nameIC]][[3]],
          criteres = factor(ecoval[[nameIC]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
          equivalence = equival)
        p <- ggplot(data=dat1, aes(x=criteres, y=equivalence, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
        dat1["100% compensé"] <- natzero
        dat1["pertes brutes"] <- moins
        dat1["gains brutes"] <- plus
      }else{
        shinyjs::hide("dwnlequivalence")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
      p <- ggplotly(p, width=500, height=1000)
    }
    equivalence$tableau <- dat1
  }else{
    p <- plotly_empty(type = "scatter", mode = "markers")
    p <- ggplotly(p)
    equivalence$tableau <- NULL
  }
  p
})

output$SEcalcul <- DT::renderDataTable({
  dat <- datatable(equivalence$tableau, rownames = FALSE, options = list(pageLength = dim.data.frame(equivalence$tableau)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")
  return(dat)
})

output$dwnlequivalence  <- downloadHandler(
  filename = function() {
    radix <- paste(listsite[as.numeric(input$selectsiteimpact3)+1, 3], '_', listsite[as.numeric(input$selectsitecompens3)+1, 3], '_', sep = "")
    if(input$selecttypegraphequivalence == '1') type <- "Equivalence CT"
    else if(input$selecttypegraphequivalence == '2') type <- "Equivalence LT"
    if(input$selectniveauequivalence == '1'){
      niveau <- "Général"
      habitat <- ""
      species <- ""
    }
    else if(input$selectniveauequivalence == '2'){
      niveau <- "Habitat_"
      species <- ""
      habitat <- listhabitat[as.numeric(input$selecthabitatSE)+1, 3]
    }
    else if(input$selectniveauequivalence == '3'){
      niveau <- "Espèce_"
      habitat <- ""
      species <- listspecies[as.numeric(input$selectspeciesSE)+1, 3]
    }
    paste(radix, type, '_', niveau, habitat, species, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(equivalence$tableau, file, row.names = FALSE)
  }
)
