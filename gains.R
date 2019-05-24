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
  }else{
    shinyjs::show("plot_gains")
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
      # Pertes CT
      dat1 <- data.frame(
        indicateurs = ecoval[[name]][[3]],
        criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
        valeurs = as.numeric(ecoval[[name]][[7]]))
      p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
        geom_bar(stat="identity", position=position_dodge(), colour="black")
    }else if(input$selecttypegraphgain == '3'){
      # Pertes LT
      dat1 <- data.frame(
        indicateurs = ecoval[[name]][[3]],
        criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
        valeurs = as.numeric(ecoval[[name]][[10]]))
      p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
        geom_bar(stat="identity", position=position_dodge(), colour="black")
    }
    p <- ggplotly(p)
  }else if(input$selectniveaugain == '2'){
    # Niveau Habitat  
    if(input$selecthabitatSC2 != '0'){
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
        # Pertes CT
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
          valeurs = as.numeric(ecoval[[name]][[7]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
      }else if(input$selecttypegraphgain == '3'){
        # Pertes LT
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
          valeurs = as.numeric(ecoval[[name]][[10]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
      }
    }else p <- plotly_empty(type = "scatter", mode = "markers")
    p <- ggplotly(p)
  }else if(input$selectniveaugain == '3'){
    # Niveau Espece
    if(input$selectspeciesSC2 != '0'){
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
        # Pertes CT
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
          valeurs = as.numeric(ecoval[[name]][[7]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
      }else if(input$selecttypegraphgain == '3'){
        # Pertes LT
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
          valeurs = as.numeric(ecoval[[name]][[10]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
      }
    }else p <- plotly_empty(type = "scatter", mode = "markers")
    p <- ggplotly(p)
  }
  p
})
