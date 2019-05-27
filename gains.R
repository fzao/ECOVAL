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

updateListHabitatSiteCompens2 <- function(){
  showlisthabitatcompens <- list()
  showlisthabitatcompens[['-']] <- 0
  if(input$selectsitecompens2 != '0'){
    name <- paste("Site no.", input$selectsitecompens2)
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
  }
  updateSelectInput(session, "selecthabitatSC2", choices = showlisthabitatcompens, selected = showlisthabitatcompens[[length(showlisthabitatcompens)]])
}

updateListSpeciesSiteCompens2 <- function(){
  showlistspeciescompens <- list()
  showlistspeciescompens[['-']] <- 0
  if(input$selectsitecompens2 != '0'){
    name <- paste("Site no.", input$selectsitecompens2)
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
  }
  updateSelectInput(session, "selectspeciesSC2", choices = showlistspeciescompens, selected = showlistspeciescompens[[length(showlistspeciescompens)]])
}

observeEvent(input$selectsitecompens2, {
  updateListHabitatSiteCompens2()
  updateListSpeciesSiteCompens2()
  if(input$selectsitecompens2 == '0'){
    shinyjs::hide("plot_gains")
    shinyjs::hide("SCcalcul")
    shinyjs::hide("dwnlgains")
  }else{
    shinyjs::show("plot_gains")
    shinyjs::show("SCcalcul")
    shinyjs::show("dwnlgains")
  }
})

observeEvent(input$selectniveaugain, {
  if(input$selectniveaugain == '1'){
    shinyjs::hide("selecthabitatSC2")
    shinyjs::hide("selectspeciesSC2")
  }else if(input$selectniveaugain == '2'){
    shinyjs::show("selecthabitatSC2")
    shinyjs::hide("selectspeciesSC2")
  }else if(input$selectniveaugain == '3'){
    shinyjs::hide("selecthabitatSC2")
    shinyjs::show("selectspeciesSC2")
  }
})

output$plot_gains <- renderPlotly({
  if(input$selectsitecompens2 != '0'){
    # Niveau General
    if(input$selectniveaugain == '1'){
      name <- paste("SCB no.", input$selectsitecompens2)
      # Etat initial
      if(input$selecttypegraphgain == '1'){
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          valeurs = as.numeric(ecoval[[name]][[4]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
      }else if(input$selecttypegraphgain == '2'){
        # Gains CT
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          valeurs = as.numeric(ecoval[[name]][[7]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
        dat1$incertitudes <- ecoval[[name]][[6]]
        dat1["gains brutes"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])
        dat1["gains relatifs"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
      }else if(input$selecttypegraphgain == '3'){
        # Gains LT
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          valeurs = as.numeric(ecoval[[name]][[10]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
        dat1$incertitudes <- ecoval[[name]][[9]]
        dat1["gains brutes"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])
        dat1["gains relatifs"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
      }
      p <- ggplotly(p)
    }else if(input$selectniveaugain == '2'){
      # Niveau Habitat  
      if(input$selecthabitatSC2 != '0'){
        shinyjs::show("dwnlgains")
        # Etat initial
        name <- paste("CC no.", input$selecthabitatSC2)
        if(input$selecttypegraphgain == '1'){
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[4]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
        }else if(input$selecttypegraphgain == '2'){
          # Gains CT
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[7]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
          dat1$incertitudes <- ecoval[[name]][[6]]
          dat1["gains brutes"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])
          dat1["gains relatifs"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
        }else if(input$selecttypegraphgain == '3'){
          # Gains LT
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[10]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
          dat1$incertitudes <- ecoval[[name]][[9]]
          dat1["gains brutes"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])
          dat1["gains relatifs"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
        }
      }else{
        shinyjs::hide("dwnlgains")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
      p <- ggplotly(p)
    }else if(input$selectniveaugain == '3'){
      # Niveau Espece
      if(input$selectspeciesSC2 != '0'){
        shinyjs::show("dwnlgains")
        # Etat initial
        name <- paste("DC no.", input$selectspeciesSC2)
        if(input$selecttypegraphgain == '1'){
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[4]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
        }else if(input$selecttypegraphgain == '2'){
          # Gains CT
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[7]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
          dat1$incertitudes <- ecoval[[name]][[6]]
          dat1["gains brutes"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])
          dat1["gains relatifs"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
        }else if(input$selecttypegraphgain == '3'){
          # Gains LT
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[10]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
          dat1$incertitudes <- ecoval[[name]][[9]]
          dat1["gains brutes"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])
          dat1["gains relatifs"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
        }
      }else{
        shinyjs::hide("dwnlgains")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
      p <- ggplotly(p)
    }
    gains$tableau <- dat1
  }else{
    p <- plotly_empty(type = "scatter", mode = "markers")
    p <- ggplotly(p)
    gains$tableau <- NULL
  }
  p
})

output$SCcalcul <- DT::renderDataTable({
  dat <- datatable(gains$tableau, rownames = FALSE, options = list(pageLength = dim.data.frame(gains$tableau)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")
  return(dat)
})


output$dwnlgains  <- downloadHandler(
  filename = function() {
    radix <- paste(listsite[as.numeric(input$selectsitecompens2)+1, 1], '_', sep = "")
    if(input$selecttypegraphgain == '1') type <- "Etat initial"
    else if(input$selecttypegraphgain == '2') type <- "Gains CT"
    else if(input$selecttypegraphgain == '3') type <- "Gains LT"
    if(input$selectniveaugain == '1'){
      niveau <- "Général"
      habitat <- ""
      species <- ""
    }
    else if(input$selectniveaugain == '2'){
      niveau <- "Habitat_"
      species <- ""
      habitat <- listhabitat[as.numeric(input$selecthabitatSC2)+1, 1]
    }
    else if(input$selectniveaugain == '3'){
      niveau <- "Espèce_"
      habitat <- ""
      species <- listspecies[as.numeric(input$selectspeciesSC2)+1, 1]
    }
    paste(radix, type, '_', niveau, habitat, species, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(pertes$tableau, file, row.names = FALSE)
  }
)