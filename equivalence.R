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

updateListHabitatEquivalence <- function(){
  showlisthabitatequivalence <- list()
  showlisthabitatequivalence[['-']] <- 0
  nbhabitat <- dim(listhabitat)[1] - 1
  if(nbhabitat > 0){
    for(i in 1:nbhabitat){
      hname <- listhabitat$habitat[i+1]
      if(exists(hname, where = ecoval)) showlisthabitatequivalence[[listhabitat$name[i+1]]] <- listhabitat$index[i+1]
    }
  }
  updateSelectInput(session, "selecthabitatSE", choices = showlisthabitatequivalence, selected = showlisthabitatequivalence[[length(showlisthabitatequivalence)]])
}

updateListSpeciesEquivalence <- function(){
  showlistspeciesequivalence <- list()
  showlistspeciesequivalence[['-']] <- 0
  nbspecies <- dim(listspecies)[1] - 1
  if(nbspecies > 0){
    for(i in 1:nbspecies){
      spname <- listspecies$species[i+1]
      if(exists(spname, where = ecoval)) showlistspeciesequivalence[[listspecies$name[i+1]]] <- listspecies$index[i+1]
    }
  }
  updateSelectInput(session, "selectspeciesSE", choices = showlistspeciesequivalence, selected = showlistspeciesequivalence[[length(showlistspeciesequivalence)]])
}

showhideSelectSpeciesHabitat <- function(){
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
}

observeEvent(input$selecttypegraphequivalence, {
  if(input$selecttypegraphequivalence == '0'){
    shinyjs::hide("selectniveauequivalence")
    shinyjs::hide("selecthabitatSE")
    shinyjs::hide("selectspeciesSE")
    shinyjs::hide("plot_equivalence")
    shinyjs::hide("SEcalcul")
    shinyjs::hide("dwnlequivalence")
  }else{
    shinyjs::show("selectniveauequivalence")
    shinyjs::show("plot_equivalence")
    shinyjs::show("SEcalcul")
    shinyjs::show("dwnlequivalence")
    showhideSelectSpeciesHabitat()
    
  }
})

observeEvent(input$selectniveauequivalence, {
  showhideSelectSpeciesHabitat()
})
# 
# output$plot_pertes <- renderPlotly({
#   if(input$selectsiteimpact2 != '0'){
#     # Niveau General
#     if(input$selectniveauperte == '1'){
#       name <- paste("SIB no.", input$selectsiteimpact2)
#       # Etat initial
#       if(input$selecttypegraphperte == '1'){
#         dat1 <- data.frame(
#           indicateurs = ecoval[[name]][[3]],
#           criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
#           valeurs = as.numeric(ecoval[[name]][[4]]))
#         p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") +
#           geom_bar(stat="identity", position=position_dodge(), colour="black")
#       }else if(input$selecttypegraphperte == '2'){
#         # Pertes CT
#         dat1 <- data.frame(
#           indicateurs = ecoval[[name]][[3]],
#           criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
#           valeurs = as.numeric(ecoval[[name]][[7]]))
#         p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
#           geom_bar(stat="identity", position=position_dodge(), colour="black")
#         dat1$incertitudes <- ecoval[[name]][[6]]
#         dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])
#         dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
#       }else if(input$selecttypegraphperte == '3'){
#         # Pertes LT
#         dat1 <- data.frame(
#           indicateurs = ecoval[[name]][[3]],
#           criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
#           valeurs = as.numeric(ecoval[[name]][[10]]))
#         p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
#           geom_bar(stat="identity", position=position_dodge(), colour="black")
#         dat1$incertitudes <- ecoval[[name]][[9]]
#         dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])
#         dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
#       }
#       p <- ggplotly(p)
#     }else if(input$selectniveauperte == '2'){
#       # Niveau Habitat  
#       if(input$selecthabitatSI2 != '0'){
#         shinyjs::show("dwnlpertes")
#         # Etat initial
#         name <- paste("CI no.", input$selecthabitatSI2)
#         if(input$selecttypegraphperte == '1'){
#           dat1 <- data.frame(
#             indicateurs = ecoval[[name]][[3]],
#             criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
#             valeurs = as.numeric(ecoval[[name]][[4]]))
#           p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") +
#             geom_bar(stat="identity", position=position_dodge(), colour="black")
#         }else if(input$selecttypegraphperte == '2'){
#           # Pertes CT
#           dat1 <- data.frame(
#             indicateurs = ecoval[[name]][[3]],
#             criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
#             valeurs = as.numeric(ecoval[[name]][[7]]))
#           p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
#             geom_bar(stat="identity", position=position_dodge(), colour="black")
#           dat1$incertitudes <- ecoval[[name]][[6]]
#           dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])
#           dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
#         }else if(input$selecttypegraphperte == '3'){
#           # Pertes LT
#           dat1 <- data.frame(
#             indicateurs = ecoval[[name]][[3]],
#             criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
#             valeurs = as.numeric(ecoval[[name]][[10]]))
#           p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
#             geom_bar(stat="identity", position=position_dodge(), colour="black")
#           dat1$incertitudes <- ecoval[[name]][[9]]
#           dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])
#           dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
#         }
#       }else{
#         shinyjs::hide("dwnlpertes")
#         dat1 <- NULL
#         p <- plotly_empty(type = "scatter", mode = "markers")
#       }
#       p <- ggplotly(p)
#     }else if(input$selectniveauperte == '3'){
#       # Niveau Espece
#       if(input$selectspeciesSI2 != '0'){
#         shinyjs::show("dwnlpertes")
#         # Etat initial
#         name <- paste("DI no.", input$selectspeciesSI2)
#         if(input$selecttypegraphperte == '1'){
#           dat1 <- data.frame(
#             indicateurs = ecoval[[name]][[3]],
#             criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
#             valeurs = as.numeric(ecoval[[name]][[4]]))
#           p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") +
#             geom_bar(stat="identity", position=position_dodge(), colour="black")
#         }else if(input$selecttypegraphperte == '2'){
#           # Pertes CT
#           dat1 <- data.frame(
#             indicateurs = ecoval[[name]][[3]],
#             criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
#             valeurs = as.numeric(ecoval[[name]][[7]]))
#           p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
#             geom_bar(stat="identity", position=position_dodge(), colour="black")
#           dat1$incertitudes <- ecoval[[name]][[6]]
#           dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])
#           dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
#         }else if(input$selecttypegraphperte == '3'){
#           # Pertes LT
#           dat1 <- data.frame(
#             indicateurs = ecoval[[name]][[3]],
#             criteres = factor(ecoval[[name]][[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
#             valeurs = as.numeric(ecoval[[name]][[10]]))
#           p <- ggplot(data=dat1, aes(x=criteres, y=valeurs, fill=indicateurs)) + theme(legend.position="none") + coord_flip() +
#             geom_bar(stat="identity", position=position_dodge(), colour="black")
#           dat1$incertitudes <- ecoval[[name]][[9]]
#           dat1["pertes brutes"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])
#           dat1["pertes relatives"] <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]])
#         }
#       }else{
#         shinyjs::hide("dwnlpertes")
#         dat1 <- NULL
#         p <- plotly_empty(type = "scatter", mode = "markers")
#       }
#       p <- ggplotly(p)
#     }
#     pertes$tableau <- dat1
#   }else{
#     p <- plotly_empty(type = "scatter", mode = "markers")
#     p <- ggplotly(p)
#     pertes$tableau <- NULL
#   }
#   p
# })
# 
# output$SIcalcul <- DT::renderDataTable({
#   dat <- datatable(pertes$tableau, rownames = FALSE, options = list(pageLength = dim.data.frame(pertes$tableau)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")
#   return(dat)
# })
# 
# output$dwnlpertes  <- downloadHandler(
#   filename = function() {
#     radix <- paste(listsite[as.numeric(input$selectsiteimpact2)+1, 1], '_', sep = "")
#     if(input$selecttypegraphperte == '1') type <- "Etat initial"
#     else if(input$selecttypegraphperte == '2') type <- "Pertes CT"
#     else if(input$selecttypegraphperte == '3') type <- "Pertes LT"
#     if(input$selectniveauperte == '1'){
#       niveau <- "Général"
#       habitat <- ""
#       species <- ""
#     }
#     else if(input$selectniveauperte == '2'){
#       niveau <- "Habitat_"
#       species <- ""
#       habitat <- listhabitat[as.numeric(input$selecthabitatSI2)+1, 1]
#     }
#     else if(input$selectniveauperte == '3'){
#       niveau <- "Espèce_"
#       habitat <- ""
#       species <- listspecies[as.numeric(input$selectspeciesSI2)+1, 1]
#     }
#     paste(radix, type, '_', niveau, habitat, species, ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(pertes$tableau, file, row.names = FALSE)
#   }
# )
