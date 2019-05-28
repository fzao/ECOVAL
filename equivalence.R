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

output$plot_equivalence <- renderPlotly({
  if(input$selecttypegraphequivalence != '0'){
    name <- "SIB no. 1"
    dat1 <- data.frame(
      perimetres = ecoval[[name]][[1]],
      indicateurs = ecoval[[name]][[3]],
      criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")))
      # valeurs = as.numeric(ecoval[[name]][[4]]))
    # Niveau General
    if(input$selectniveauequivalence == '1'){
      # Equivalence CT
      if(input$selecttypegraphequivalence == '1'){
        pertesCT <- 0.
        #p <- plotly_empty(type = "scatter", mode = "markers")
      }else if(input$selecttypegraphequivalence == '2'){
      # Equivalence LT
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
    }else if(input$selectniveauequivalence == '2'){
    # Niveau Habitat
      if(input$selecthabitatSE != '0'){
        shinyjs::show("dwnlequivalence")
        # Equivalence CT
        if(input$selecttypegraphequivalence == '1'){
          p <- plotly_empty(type = "scatter", mode = "markers")
        }else if(input$selecttypegraphequivalence == '2'){
        # Equivalence LT
          p <- plotly_empty(type = "scatter", mode = "markers")
        }
      }else{
        shinyjs::hide("dwnlequivalence")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
      p <- ggplotly(p)
    }else if(input$selectniveauequivalence == '3'){
    # Niveau Espece
    if(input$selectspeciesSE != '0'){
        shinyjs::show("dwnlequivalence")
        # Equivalence CT
        if(input$selecttypegraphequivalence == '1'){
          p <- plotly_empty(type = "scatter", mode = "markers")
        }else if(input$selecttypegraphequivalence == '2'){
        # Equivalence LT
          p <- plotly_empty(type = "scatter", mode = "markers")
        }
      }else{
        shinyjs::hide("dwnlequivalence")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
      p <- ggplotly(p)
    }
    equivalence$tableau <- dat1
  }else{
    p <- plotly_empty(type = "scatter", mode = "markers")
    p <- ggplotly(p)
    equivalence$tableau <- NULL
  }
  p
})
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
