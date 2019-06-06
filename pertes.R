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
  updateTabsetPanel(session, "resultpertes", selected = "graphe")
  if(input$selectsiteimpact2 == '0'){
    shinyjs::hide("plot_pertes")
    shinyjs::hide("SIcalcul")
    shinyjs::hide("dwnlpertes")
    shinyjs::hide("selecttypegraphperte")
    shinyjs::hide("selectniveauperte")
  }else{
    shinyjs::show("plot_pertes")
    shinyjs::show("SIcalcul")
    shinyjs::show("dwnlpertes")
    shinyjs::show("selecttypegraphperte")
    shinyjs::show("selectniveauperte")
  }
})

observeEvent(input$selectniveauperte, {
  updateTabsetPanel(session, "resultpertes", selected = "graphe")
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

observeEvent(input$selecttypegraphperte, updateTabsetPanel(session, "resultpertes", selected = "graphe"))
observeEvent(input$selecthabitatSI2, updateTabsetPanel(session, "resultpertes", selected = "graphe"))
observeEvent(input$selectspeciesSI2, updateTabsetPanel(session, "resultpertes", selected = "graphe"))

output$plot_pertes <- renderPlotly({
  if(input$selectsiteimpact2 != '0'){
    # Niveau General
    if(input$selectniveauperte == '1'){
      name <- paste("SIB no.", input$selectsiteimpact2)
      # Etat initial
      if(input$selecttypegraphperte == '1'){
        dat1 <- data.frame(
          perimetres = ecoval[[name]][[1]],
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
          valeurs = as.numeric(ecoval[[name]][[4]]))
        couleurs <- c("Diversité habitat" = "#83D072",
                      "Diversité Espèce" ="#1E6218",
                      "Patrimonialité_PS" = "#9D403E",
                      "Fonctionnalité" = "#4894DC",
                      "Pression_PS" = "#E6A936",
                      "Connectivité" = "#AF76C4",
                      "Représentativité" = "#68DDEA",
                      "Patrimonialite_PE" = "#842D2A",
                      "Pression_PE" = "#DE9830",
                      "Structure" = "grey")
        
        p <- ggplot(data=dat1, aes(x=indicateurs, y=valeurs)) +
          geom_bar(stat="identity", width=0.5, aes(fill=criteres))+
          theme_bw()+
          scale_fill_manual(values=couleurs)+
          geom_text(aes(label=valeurs,  hjust="center",vjust="bottom", y=valeurs+2))+
          theme(legend.position='none')+
          facet_grid(.~criteres, scales = "free", space ="free")+
          theme (axis.text.x = element_text(colour="black", angle = 45, size = 8))
        p <- ggplotly(p, width=1300, height=800)
      }else if(input$selecttypegraphperte == '2'){
        # Pertes CT
        dat1 <- data.frame(
          perimetres = ecoval[[name]][[1]],
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
          valeurs = as.numeric(ecoval[[name]][[7]],
          incertitudes <- ecoval[[name]][[6]],
          pertes_brutes <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]),
          pertes_relatives <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]]))
          )
        p <- ggplot(data=dat1, aes(x=indicateurs, y=pertes_relatives)) +
          geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(pertes_relatives))))+
          coord_flip()+
          labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
          scale_fill_manual(values=c("#B82010", "#7FDD4C", "#7FDD4C"))+
          theme(legend.position="none")+
          geom_text(aes(label=pertes_brutes, hjust="left", vjust="center", y=2), size=2.5)+
          theme(axis.text.y=element_text(colour="black", size = 8))+
          geom_text(aes(label=incertitudes, hjust="top", vjust="center", y= -50))+
          #geom_text(aes(label=valeurs, hjust="top", vjust="center", y= - 100), size=2.5)+
          theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
          theme_bw()+
          facet_grid(criteres ~ ., scales = "free", space = "free")
        p <- ggplotly(p, width=1000, height=1200)
      }else if(input$selecttypegraphperte == '3'){
        # Pertes LT
        dat1 <- data.frame(
          perimetres = ecoval[[name]][[1]],
          indicateurs = ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
          valeurs = as.numeric(ecoval[[name]][[7]],
          incertitudes <- ecoval[[name]][[6]],
          pertes_brutes <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]),
          pertes_relatives <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]]))
        )
        p <- ggplot(data=dat1, aes(x=indicateurs, y=pertes_relatives)) +
          geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(pertes_relatives))))+
          coord_flip()+
          labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
          scale_fill_manual(values=c("#B82010", "#7FDD4C", "#7FDD4C"))+
          theme(legend.position="none")+
          geom_text(aes(label=pertes_brutes, hjust="left", vjust="center", y=2), size=2.5)+
          theme(axis.text.y=element_text(colour="black", size = 8))+
          geom_text(aes(label=incertitudes, hjust="top", vjust="center", y= -50))+
          #geom_text(aes(label=valeurs, hjust="top", vjust="center", y= - 100), size=2.5)+
          theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
          theme_bw()+
          facet_grid(criteres ~ ., scales = "free", space = "free")
        p <- ggplotly(p, width=1000, height=1200)
      }
    }else if(input$selectniveauperte == '2'){
      # Niveau Habitat  
      if(input$selecthabitatSI2 != '0'){
        shinyjs::show("dwnlpertes")
      # Etat initial
        name <- paste("CI no.", input$selecthabitatSI2)
        if(input$selecttypegraphperte == '1'){
          dat1 <- data.frame(
            perimetres = ecoval[[name]][[1]],
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            valeurs = as.numeric(ecoval[[name]][[4]]))
          couleurs <- c("Diversité habitat" = "#83D072",
                        "Diversité Espèce" ="#1E6218",
                        "Patrimonialité_PS" = "#9D403E",
                        "Fonctionnalité" = "#4894DC",
                        "Pression_PS" = "#E6A936",
                        "Connectivité" = "#AF76C4",
                        "Représentativité" = "#68DDEA",
                        "Patrimonialite_PE" = "#842D2A",
                        "Pression_PE" = "#DE9830",
                        "Structure" = "grey")
          
          p <- ggplot(data=dat1, aes(x=indicateurs, y=valeurs)) +
            geom_bar(stat="identity", width=0.5, aes(fill=criteres))+
            theme_bw()+
            scale_fill_manual(values=couleurs)+
            geom_text(aes(label=valeurs,  hjust="center",vjust="bottom", y=valeurs+2))+
            theme(legend.position='none')+
            facet_grid(.~criteres, scales = "free", space ="free")+
            theme (axis.text.x = element_text(colour="black", angle = 45, size = 8))
          p <- ggplotly(p, width=1000, height=800)
        }else if(input$selecttypegraphperte == '2'){
        # Pertes CT
          dat1 <- data.frame(
            perimetres = ecoval[[name]][[1]],
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            valeurs = as.numeric(ecoval[[name]][[7]],
            incertitudes <- ecoval[[name]][[6]],
            pertes_brutes <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]),
            pertes_relatives <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]]))
          )
          p <- ggplot(data=dat1, aes(x=indicateurs, y=pertes_relatives)) +
            geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(pertes_relatives))))+
            coord_flip()+
            labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
            scale_fill_manual(values=c("#B82010", "#7FDD4C", "#7FDD4C"))+
            theme(legend.position="none")+
            geom_text(aes(label=pertes_brutes, hjust="left", vjust="center", y=2), size=2.5)+
            theme(axis.text.y=element_text(colour="black", size = 8))+
            geom_text(aes(label=incertitudes, hjust="top", vjust="center", y= -50))+
            #geom_text(aes(label=valeurs, hjust="top", vjust="center", y= - 100), size=2.5)+
            theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
            theme_bw()+
            facet_grid(criteres ~ ., scales = "free", space = "free")
          p <- ggplotly(p, width=1100, height=800)
        }else if(input$selecttypegraphperte == '3'){
        # Pertes LT
          dat1 <- data.frame(
            perimetres = ecoval[[name]][[1]],
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            valeurs = as.numeric(ecoval[[name]][[7]],
            incertitudes <- ecoval[[name]][[6]],
            pertes_brutes <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]),
            pertes_relatives <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]]))
          )
          p <- ggplot(data=dat1, aes(x=indicateurs, y=pertes_relatives)) +
            geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(pertes_relatives))))+
            coord_flip()+
            labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
            scale_fill_manual(values=c("#B82010", "#7FDD4C", "#7FDD4C"))+
            theme(legend.position="none")+
            geom_text(aes(label=pertes_brutes, hjust="left", vjust="center", y=2), size=2.5)+
            theme(axis.text.y=element_text(colour="black", size = 8))+
            geom_text(aes(label=incertitudes, hjust="top", vjust="center", y= -50))+
            #geom_text(aes(label=valeurs, hjust="top", vjust="center", y= - 100), size=2.5)+
            theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
            theme_bw()+
            facet_grid(criteres ~ ., scales = "free", space = "free")
          p <- ggplotly(p, width=1100, height=800)
        }
      }else{
        shinyjs::hide("dwnlpertes")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
    }else if(input$selectniveauperte == '3'){
      # Niveau Espece
      if(input$selectspeciesSI2 != '0'){
        shinyjs::show("dwnlpertes")
        # Etat initial
        name <- paste("DI no.", input$selectspeciesSI2)
        if(input$selecttypegraphperte == '1'){
          dat1 <- data.frame(
            perimetres = ecoval[[name]][[1]],
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            valeurs = as.numeric(ecoval[[name]][[4]]))
          couleurs <- c("Diversité habitat" = "#83D072",
                        "Diversité Espèce" ="#1E6218",
                        "Patrimonialité_PS" = "#9D403E",
                        "Fonctionnalité" = "#4894DC",
                        "Pression_PS" = "#E6A936",
                        "Connectivité" = "#AF76C4",
                        "Représentativité" = "#68DDEA",
                        "Patrimonialite_PE" = "#842D2A",
                        "Pression_PE" = "#DE9830",
                        "Structure" = "grey")
          
          p <- ggplot(data=dat1, aes(x=indicateurs, y=valeurs)) +
            geom_bar(stat="identity", width=0.5, aes(fill=criteres))+
            theme_bw()+
            scale_fill_manual(values=couleurs)+
            geom_text(aes(label=valeurs,  hjust="center",vjust="bottom", y=valeurs+2))+
            theme(legend.position='none')+
            facet_grid(.~criteres, scales = "free", space ="free")+
            theme (axis.text.x = element_text(colour="black", angle = 45, size = 8))
          p <- ggplotly(p, width=800, height=800)
        }else if(input$selecttypegraphperte == '2'){
          # Pertes CT
          dat1 <- data.frame(
            perimetres = ecoval[[name]][[1]],
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            valeurs = as.numeric(ecoval[[name]][[7]],
            incertitudes <- ecoval[[name]][[6]],
            pertes_brutes <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]),
            pertes_relatives <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]]))
          )
          p <- ggplot(data=dat1, aes(x=indicateurs, y=pertes_relatives)) +
            geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(pertes_relatives))))+
            coord_flip()+
            labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
            scale_fill_manual(values=c("#B82010", "#7FDD4C", "#7FDD4C"))+
            theme(legend.position="none")+
            geom_text(aes(label=pertes_brutes, hjust="left", vjust="center", y=2), size=2.5)+
            theme(axis.text.y=element_text(colour="black", size = 8))+
            geom_text(aes(label=incertitudes, hjust="top", vjust="center", y= -50))+
            #geom_text(aes(label=valeurs, hjust="top", vjust="center", y= - 100), size=2.5)+
            theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
            theme_bw()+
            facet_grid(criteres ~ ., scales = "free", space = "free")
          p <- ggplotly(p, width=1000, height=800)
        }else if(input$selecttypegraphperte == '3'){
          # Pertes LT
          dat1 <- data.frame(
            perimetres = ecoval[[name]][[1]],
            indicateurs = ecoval[[name]][[3]],
            criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            valeurs = as.numeric(ecoval[[name]][[7]],
            incertitudes <- ecoval[[name]][[6]],
            pertes_brutes <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]),
            pertes_relatives <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]) * 100 / as.numeric(ecoval[[name]][[4]]))
          )
          p <- ggplot(data=dat1, aes(x=indicateurs, y=pertes_relatives)) +
            geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(pertes_relatives))))+
            coord_flip()+
            labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
            scale_fill_manual(values=c("#B82010", "#7FDD4C", "#7FDD4C"))+
            theme(legend.position="none")+
            geom_text(aes(label=pertes_brutes, hjust="left", vjust="center", y=2), size=2.5)+
            theme(axis.text.y=element_text(colour="black", size = 8))+
            geom_text(aes(label=incertitudes, hjust="top", vjust="center", y= -50))+
            #geom_text(aes(label=valeurs, hjust="top", vjust="center", y= - 100), size=2.5)+
            theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
            theme_bw()+
            facet_grid(criteres ~ ., scales = "free", space = "free")
          p <- ggplotly(p, width=1000, height=800)
        }
      }else{
        shinyjs::hide("dwnlpertes")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
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
    radix <- paste(listsite[as.numeric(input$selectsiteimpact2)+1, 3], '_', sep = "")
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
      habitat <- listhabitat[as.numeric(input$selecthabitatSI2)+1, 3]
    }
    else if(input$selectniveauperte == '3'){
      niveau <- "Espèce_"
      habitat <- ""
      species <- listspecies[as.numeric(input$selectspeciesSI2)+1, 3]
    }
    paste(radix, type, '_', niveau, habitat, species, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(pertes$tableau, file, row.names = FALSE)
  }
)
