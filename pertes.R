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

updateListHabitatSiteImpact2 <- function(){
  showlisthabitatimpact <- list()
  showlisthabitatimpact[['-']] <- 0
  if(input$selectsiteimpact2 != '0'){
    name <- paste("Site no.", input$selectsiteimpact2)
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
  }
  updateSelectInput(session, "selecthabitatSI2", choices = showlisthabitatimpact, selected = showlisthabitatimpact[[length(showlisthabitatimpact)]])
}

updateListSpeciesSiteImpact2 <- function(){
  showlistspeciesimpact <- list()
  showlistspeciesimpact[['-']] <- 0
  if(input$selectsiteimpact2 != '0'){
    name <- paste("Site no.", input$selectsiteimpact2)
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
  }
  updateSelectInput(session, "selectspeciesSI2", choices = showlistspeciesimpact, selected = showlistspeciesimpact[[length(showlistspeciesimpact)]])
}

observeEvent(input$selectsiteimpact2, {
  updateListHabitatSiteImpact2()
  updateListSpeciesSiteImpact2()
  if(input$selectsiteimpact2 == '0'){
    shinyjs::hide("plot_pertes")
    shinyjs::hide("SIcalcul")
    shinyjs::hide("dwnlpertes")
  }else{
    shinyjs::show("plot_pertes")
    shinyjs::show("SIcalcul")
    shinyjs::show("dwnlpertes")
  }
})

observeEvent(input$selectniveauperte, {
  if(input$selectniveauperte == '1'){
    shinyjs::hide("selecthabitatSI2")
    shinyjs::hide("selectspeciesSI2")
  }else if(input$selectniveauperte == '2'){
    shinyjs::show("selecthabitatSI2")
    shinyjs::hide("selectspeciesSI2")
  }else if(input$selectniveauperte == '3'){
    shinyjs::hide("selecthabitatSI2")
    shinyjs::show("selectspeciesSI2")
  }
})

output$plot_pertes <- renderPlotly({
  if(input$selectsiteimpact2 != '0'){
    # Niveau General
    if(input$selectniveauperte == '1'){
      name <- paste("SIB no.", input$selectsiteimpact2)
      # Etat initial
      if(input$selecttypegraphperte == '1'){
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          valeurs = as.numeric(ecoval[[name]][[4]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
      }else if(input$selecttypegraphperte == '2'){
        # Pertes CT
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          valeurs = as.numeric(ecoval[[name]][[7]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
        dat1$incertitudes <- ecoval[[name]][[6]]
        dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])
        dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
      }else if(input$selecttypegraphperte == '3'){
        # Pertes LT
        dat1 <- data.frame(
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          valeurs = as.numeric(ecoval[[name]][[10]]))
        p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
          geom_bar(stat="identity", position=position_dodge(), colour="black")
        dat1$incertitudes <- ecoval[[name]][[9]]
        dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])
        dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
      }
      p <- ggplotly(p)
    }else if(input$selectniveauperte == '2'){
      # Niveau Habitat  
      if(input$selecthabitatSI2 != '0'){
        shinyjs::show("dwnlpertes")
      # Etat initial
        name <- paste("CI no.", input$selecthabitatSI2)
        if(input$selecttypegraphperte == '1'){
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[4]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
        }else if(input$selecttypegraphperte == '2'){
        # Pertes CT
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[7]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
          dat1$incertitudes <- ecoval[[name]][[6]]
          dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])
          dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
        }else if(input$selecttypegraphperte == '3'){
        # Pertes LT
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[10]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
          dat1$incertitudes <- ecoval[[name]][[9]]
          dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])
          dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
        }
      }else{
        shinyjs::hide("dwnlpertes")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
      p <- ggplotly(p)
    }else if(input$selectniveauperte == '3'){
      # Niveau Espece
      if(input$selectspeciesSI2 != '0'){
        shinyjs::show("dwnlpertes")
        # Etat initial
        name <- paste("DI no.", input$selectspeciesSI2)
        if(input$selecttypegraphperte == '1'){
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[4]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
        }else if(input$selecttypegraphperte == '2'){
          # Pertes CT
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[7]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
          dat1$incertitudes <- ecoval[[name]][[6]]
          dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])
          dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
        }else if(input$selecttypegraphperte == '3'){
          # Pertes LT
          dat1 <- data.frame(
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(ecoval[[name]][[10]]))
          p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
            geom_bar(stat="identity", position=position_dodge(), colour="black")
          dat1$incertitudes <- ecoval[[name]][[9]]
          dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])
          dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
        }
      }else{
        shinyjs::hide("dwnlpertes")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
      p <- ggplotly(p)
    }
    pertes$tableau <- dat1
  }else{
    p <- plotly_empty(type = "scatter", mode = "markers")
    p <- ggplotly(p)
    pertes$tableau <- NULL
  }
  p
})

output$SIcalcul <- DT::renderDataTable({
  dat <- datatable(pertes$tableau, rownames = FALSE, options = list(pageLength = dim.data.frame(pertes$tableau)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")
  return(dat)
})

output$dwnlpertes  <- downloadHandler(
  filename = function() {
    radix <- paste(listsite[as.numeric(input$selectsiteimpact2)+1, 1], '_', sep = "")
    if(input$selecttypegraphperte == '1') type <- "Etat initial"
    else if(input$selecttypegraphperte == '2') type <- "Pertes CT"
    else if(input$selecttypegraphperte == '3') type <- "Pertes LT"
    if(input$selectniveauperte == '1'){
      niveau <- "Général"
      habitat <- ""
      species <- ""
    }
    else if(input$selectniveauperte == '2'){
      niveau <- "Habitat_"
      species <- ""
      habitat <- listhabitat[as.numeric(input$selecthabitatSI2)+1, 1]
    }
    else if(input$selectniveauperte == '3'){
      niveau <- "Espèce_"
      habitat <- ""
      species <- listspecies[as.numeric(input$selectspeciesSI2)+1, 1]
    }
    paste(radix, type, '_', niveau, habitat, species, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(pertes$tableau, file, row.names = FALSE)
  }
)
