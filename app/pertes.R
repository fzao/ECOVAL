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
# # Copyright (c) EDF-INRAE 2019-2020
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
    shinyjs::hide("selecthabitatSI2")
    shinyjs::hide("selectspeciesSI2")
  }else{
    shinyjs::show("plot_pertes")
    shinyjs::show("SIcalcul")
    shinyjs::show("dwnlpertes")
    shinyjs::show("selecttypegraphperte")
    shinyjs::show("selectniveauperte")
    updateSelectInput(session, "selecttypegraphperte", selected = '1')
    updateSelectInput(session, "selectniveauperte", selected = '1')
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

output$plot_pertes_ui <- renderUI({
  if(input$selectniveauperte == '1'){ # Niveau General
    if(input$selecttypegraphperte == '1'){ # Etat initial
      plot.width = "100%"
      plot.height = "100%"
    }else{ # Autres
      plot.width = "100%"
      plot.height = "100%"
    }
  }else if(input$selectniveauperte == '2'){ # Niveau Habitat
    if(input$selecttypegraphperte == '1'){ # Etat initial
      plot.width = "100%"
      plot.height = "100%"
    }else{ # Autres
      plot.width = "100%"
      plot.height = "100%"
    } 
  }else if(input$selectniveauperte == '3'){ # Niveau Espece
    if(input$selecttypegraphperte == '1'){ # Etat initial
      plot.width = "100%"
      plot.height = "100%"
    }else{ # Autres
      plot.width = "100%"
      plot.height = "100%"
    } 
  }
  plotlyOutput('plot_pertes', width = plot.width, height = plot.height)
})

output$plot_pertes <- renderPlotly({
  if(input$selectsiteimpact2 != '0'){
    
    ### Niveau General
    
    if(input$selectniveauperte == '1'){
      name <- paste("SIB no.", input$selectsiteimpact2)
      shortindicnames <- c("Nb hab forestier",
                           "Surf hab forestier",
                           "Nb hab ouvert",
                           "Surf hab ouvert",
                           "Nb hab buiss",
                           "Surf hab buiss",
                           "Nb hab rocheux",
                           "Surf hab rocheux",
                           "Nb hab humide",
                           "Surf hab humide",
                           "Nb hab aqua",
                           "Surf hab aqua",
                           "Lg lisière _ ha forêt",
                           "Diversité avifaune", 
                           "Diversité chiroptères", 
                           "Diversité reptiles", 
                           "Diversité amphibiens", 
                           "Diversité mammifères",
                           "Diversité insectes", 
                           "Diversité lépidoptères", 
                           "Diversité odonates", 
                           "Diversité orthoptères", 
                           "Diversité coléoptères", 
                           "Diversité flore totale",
                           "% habitat en danger localement",
                           "% habitat d'intérêt comm +prio",
                           "% sp protégées faune national et regional",
                           "% sp protégées flore national et regional",
                           "% sp menacées faune national",
                           "% sp menacées flore national",
                           "% sp menacées faune regional",
                           "% sp menacées flore regional",
                           "% sp faune DFFH",
                           "% sp flore DFFH",
                           "% avifaune DO",
                           "% oiseaux nicheurs",
                           "% sp repro site",
                           "Indice de spé avifaune",
                           "% chiroptères spécialistes",
                           "% hab bon état conservation",
                           "Surf milieux NON cultivés",
                           "Surf zones NON urbanisées",
                           "Nb sp EEE",
                           "Nb patchs EEE",
                           "% recouvrement EEE",
                           "Lg linéaire transpt",
                           "Lg linéaire haie", 
                           "Surf corridor",
                           "Nb Sp TVB",
                           "% hab forestier _ PE",
                           "% hab ouvert _ PE",
                           "% hab rocheux _ PE",
                           "% hab aqua _ PE",
                           "% hab humide _ PE",
                           "% hab buiss _ PE",
                           "Nb zonage",
                           "Nb Sp faune ZNIEFF",
                           "Nb Sp flore ZNIEFF",
                           "% milieu cultivés_PE",
                           "% zones arti_PE",
                           "Surf EEE prox")
      # Etat initial
      if(input$selecttypegraphperte == '1'){
        dat1 <- data.frame(
          perimetres = ecoval[[name]][[1]],
          indicateurs = shortindicnames,
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
          valeurs = as.numeric(ecoval[[name]][[4]]))

        couleurs <- c("Diversité habitat" = "#83D072",
                      "Diversité espèce" ="#1E6218",
                      "Patrimonialité_PS" = "#9D403E",
                      "Fonctionnalité" = "#4894DC",
                      "Pression_PS" = "#E6A936",
                      "Connectivité" = "#AF76C4",
                      "Représentativité" = "#68DDEA",
                      "Patrimonialité_PE" = "#842D2A",
                      "Pression_PE" = "#DE9830",
                      "Structure" = "grey")
        dat1$valeurs[is.na(dat1$valeurs)] <- 0
        dat1$valeurs[is.nan(dat1$valeurs)] <- 0
        data1 <- dat1[1:13,]
        data2 <- dat1[14:24,]
        data3 <- dat1[25:35,]
        data4 <- dat1[36:40,]
        data5 <- dat1[41:45,]
        data6 <- dat1[46:49,]
        data7 <- dat1[50:55,]
        data8 <- dat1[56:58,]
        data9 <- dat1[59:61,]
        
        p1 <- plot_ly(data1,
                      x = as.character(data1$indicateurs), y = data1$valeurs,
                      type = 'bar', 
                      marker = list(color = couleurs["Diversité habitat"])) %>%
          layout(xaxis = list(title = "Diversité habitat", showticklabels=FALSE))
        
        p2 <- plot_ly(data2,
                      x = as.character(data2$indicateurs), y = data2$valeurs,
                      type = 'bar',
                      marker = list(color =couleurs["Diversité espèce"])) %>%
          layout(xaxis = list(title = "Diversité espèce", showticklabels=FALSE))
        
        p3 <- plot_ly(data3,
                      x = as.character(data3$indicateurs), y = data3$valeurs,
                      type = 'bar',
                      marker = list(color = couleurs["Patrimonialité_PS"])) %>%
          layout(xaxis = list(title = "Patrimonialité_PS", showticklabels=FALSE))
        
        p4 <- plot_ly(data4,
                      x = as.character(data4$indicateurs), y = data4$valeurs,
                      type = 'bar',
                      marker = list(color = couleurs["Fonctionnalité"])) %>%
          layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                 yaxis = list(title = "Valeurs à l'état initial"))
        
        p5 <- plot_ly(data5,
                      x = as.character(data5$indicateurs), y = data5$valeurs,
                      type = 'bar',
                      marker = list(color = couleurs["Pression_PS"])) %>%
          layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE))
        
        p6 <- plot_ly(data6,
                      x = as.character(data6$indicateurs), y = data6$valeurs,
                      type = 'bar',
                      marker = list(color = couleurs["Connectivité"])) %>%
          layout(xaxis = list(title = "Connectivité", showticklabels=FALSE))
        
        p7 <- plot_ly(data7,
                      x = as.character(data7$indicateurs), y = data7$valeurs,
                      type = 'bar',
                      marker = list(color = couleurs["Représentativité"])) %>%
          layout(xaxis = list(title = "Représentativité", showticklabels=FALSE))
        
        p8 <- plot_ly(data8,
                      x = as.character(data8$indicateurs), y = data8$valeurs,
                      type = 'bar',
                      marker = list(color = couleurs["Patrimonialité_PE"])) %>%
          layout(xaxis = list(title = "Patrimonialité_PE", showticklabels=FALSE))
        
        p9 <- plot_ly(data9,
                      x = as.character(data9$indicateurs), y = data9$valeurs,
                      type = 'bar',
                      marker = list(color = couleurs["Pression_PE"])) %>%
          layout(xaxis = list(title = "Pression_PE", showticklabels=FALSE))
        
        blankplot <- plotly_empty(type = "scatter", mode = "markers")
        
        p <- subplot(p1,p2,p3,blankplot,blankplot,blankplot,p4,p5,p6,blankplot,blankplot,blankplot,p7,p8,p9, nrows = 5, titleX = TRUE, titleY = FALSE) %>%
                layout(title = "INDICATEURS", showlegend=FALSE)
        
      }else if(input$selecttypegraphperte == '2'){
        
        # Pertes CT
        dat1 <- data.frame(
          perimetres = ecoval[[name]][[1]],
          indicateurs = shortindicnames,
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
          incertitudes <- gsub("[ABC]", "*", ecoval[[name]][[6]]),
          pertes_brutes <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]),
          pertes_relatives <- (as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])) * 100 / as.numeric(ecoval[[name]][[4]])
        )
        
        dat1[[5]][is.na(dat1[[5]])] <- 0
        dat1[[5]][is.nan(dat1[[5]])] <- 0
        dat1[[6]][is.na(dat1[[6]])] <- 0
        dat1[[6]][is.nan(dat1[[6]])] <- 0
        dat1[[6]][is.infinite(dat1[[6]])] <- 0
        
        data1 <- dat1[1:13,]
        data2 <- dat1[14:24,]
        data3 <- dat1[25:35,]
        data4 <- dat1[36:40,]
        data5 <- dat1[41:45,]
        data6 <- dat1[46:49,]
        data7 <- dat1[50:55,]
        data8 <- dat1[56:58,]
        data9 <- dat1[59:61,]
        
        couleurs <- c("negative" = "#C67677",
                      "positive" ="#7FDD4C")
        
        p1 <- plot_ly(data1,
                      y = as.character(data1$indicateurs), x = data1[[6]],
                      type = 'bar', text = paste(data1[[5]], gsub("[ABC]", "*", data1[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data1[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Diversité habitat", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p2 <- plot_ly(data2,
                      y = as.character(data2$indicateurs), x = data2[[6]],
                      type = 'bar', text = paste(data2[[5]], gsub("[ABC]", "*", data2[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data2[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Diversité espèce", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p3 <- plot_ly(data3,
                      y = as.character(data3$indicateurs), x = data3[[6]],
                      type = 'bar', text = paste(data3[[5]], gsub("[ABC]", "*", data3[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data3[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Patrimonialité_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p4 <- plot_ly(data4,
                      y = as.character(data4$indicateurs), x = data4[[6]],
                      type = 'bar', text = paste(data4[[5]], gsub("[ABC]", "*", data4[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data4[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p5 <- plot_ly(data5,
                      y = as.character(data5$indicateurs), x = data5[[6]],
                      type = 'bar', text = paste(data5[[5]], gsub("[ABC]", "*", data5[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data5[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p6 <- plot_ly(data6,
                      y = as.character(data6$indicateurs), x = data6[[6]],
                      type = 'bar', text = paste(data6[[5]], gsub("[ABC]", "*", data6[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data6[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p7 <- plot_ly(data7,
                      y = as.character(data7$indicateurs), x = data7[[6]],
                      type = 'bar', text = paste(data7[[5]], gsub("[ABC]", "*", data7[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data7[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p8 <- plot_ly(data8,
                      y = as.character(data8$indicateurs), x = data8[[6]],
                      type = 'bar', text = paste(data8[[5]], gsub("[ABC]", "*", data8[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data8[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Patrimonialité_PE", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p9 <- plot_ly(data9,
                      y = as.character(data9$indicateurs), x = data9[[6]],
                      type = 'bar', text = paste(data9[[5]], gsub("[ABC]", "*", data9[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data9[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PE", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p <- subplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, titleX = TRUE, titleY = FALSE) %>%
          layout(title = "PERTES RELATIVES (barre) ET BRUTES (nombre)", showlegend=FALSE)
        
      }else if(input$selecttypegraphperte == '3'){
        
        # Pertes LT
        dat1 <- data.frame(
          perimetres = ecoval[[name]][[1]],
          indicateurs = shortindicnames,
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
          incertitudes <- gsub("[ABC]", "*", ecoval[[name]][[9]]),
          pertes_brutes <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]),
          pertes_relatives <- (as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])) * 100 / as.numeric(ecoval[[name]][[4]])
        )
        
        dat1[[5]][is.na(dat1[[5]])] <- 0
        dat1[[5]][is.nan(dat1[[5]])] <- 0
        dat1[[6]][is.na(dat1[[6]])] <- 0
        dat1[[6]][is.nan(dat1[[6]])] <- 0
        dat1[[6]][is.infinite(dat1[[6]])] <- 0
        
        data1 <- dat1[1:13,]
        data2 <- dat1[14:24,]
        data3 <- dat1[25:35,]
        data4 <- dat1[36:40,]
        data5 <- dat1[41:45,]
        data6 <- dat1[46:49,]
        data7 <- dat1[50:55,]
        data8 <- dat1[56:58,]
        data9 <- dat1[59:61,]
        
        couleurs <- c("negative" = "#C67677",
                      "positive" ="#7FDD4C")
        
        p1 <- plot_ly(data1,
                      y = as.character(data1$indicateurs), x = data1[[6]],
                      type = 'bar', text = paste(data1[[5]], gsub("[ABC]", "*", data1[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data1[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Diversité habitat", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p2 <- plot_ly(data2,
                      y = as.character(data2$indicateurs), x = data2[[6]],
                      type = 'bar', text = paste(data2[[5]], gsub("[ABC]", "*", data2[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data2[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Diversité espèce", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p3 <- plot_ly(data3,
                      y = as.character(data3$indicateurs), x = data3[[6]],
                      type = 'bar', text = paste(data3[[5]], gsub("[ABC]", "*", data3[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data3[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Patrimonialité_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p4 <- plot_ly(data4,
                      y = as.character(data4$indicateurs), x = data4[[6]],
                      type = 'bar', text = paste(data4[[5]], gsub("[ABC]", "*", data4[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data4[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p5 <- plot_ly(data5,
                      y = as.character(data5$indicateurs), x = data5[[6]],
                      type = 'bar', text = paste(data5[[5]], gsub("[ABC]", "*", data5[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data5[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p6 <- plot_ly(data6,
                      y = as.character(data6$indicateurs), x = data6[[6]],
                      type = 'bar', text = paste(data6[[5]], gsub("[ABC]", "*", data6[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data6[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p7 <- plot_ly(data7,
                      y = as.character(data7$indicateurs), x = data7[[6]],
                      type = 'bar', text = paste(data7[[5]], gsub("[ABC]", "*", data7[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data7[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p8 <- plot_ly(data8,
                      y = as.character(data8$indicateurs), x = data8[[6]],
                      type = 'bar', text = paste(data8[[5]], gsub("[ABC]", "*", data8[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data8[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Patrimonialité_PE", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p9 <- plot_ly(data9,
                      y = as.character(data9$indicateurs), x = data9[[6]],
                      type = 'bar', text = paste(data9[[5]], gsub("[ABC]", "*", data9[[4]])), textposition='outside', orientation = 'h',
                      marker = list(color = ifelse(data9[[6]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PE", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p <- subplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, titleX = TRUE, titleY = FALSE) %>%
          layout(title = "PERTES RELATIVES (barre) ET BRUTES (nombre)", showlegend=FALSE)
        
      }
    }else if(input$selectniveauperte == '2'){
      
      ### Niveau Habitat  
      
      if(input$selecthabitatSI2 != '0'){
        shinyjs::show("dwnlpertes")
      # Etat initial
        name <- paste("CI no.", input$selecthabitatSI2)
        shortindicnames <- c("Nb sp faune dep hab",
                             "Nb sp flore",
                             "Surface totale habitat",
                             "Nombre de patches d'habitat",
                             "Nombre de micro-habitats",
                             "Nb horizons sol",
                             "Epaisseur d'horizons sol",
                             "Abondance relative de faune détritivore",
                             "Nb sp faune dep hab reproduction",
                             "Nb TGB",
                             "% bois mort",
                             "Nb sp bio-indicatrices",
                             "Densité de lichen",
                             "Ancienneté de la forêt",
                             "Nb sp pollinisatrices",
                             "% flore dominante",
                             "Nb strates végétation",
                             "Hauteur strates",
                             "% sol dégradé",
                             "Nb sp indicatrices de pression",
                             "Tps depuis dernière coupe",
                             "Tx recouvrement ligneux",
                             "Tx couvert algues eutrophisation",
                             "Indice frag type hab",
                             "Surface d'habitat dans le PE",
                             "% surf hab _ PE")
        if(input$selecttypegraphperte == '1'){
          namehab <- paste("Habitat", input$selecthabitatSI2)
          partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
          if(ecoval[[namehab]][4,2] == "1"){ # Forestier
            partial_select <- c(partial_select, c(10,11,12,13,14,21))
          }else if(ecoval[[namehab]][4,2] == "2"){ # Ouvert
            partial_select <- c(partial_select, c(15, 22))
          }else if(ecoval[[namehab]][4,2] == "4"){ # Zone humide
            partial_select <- c(partial_select, c(16, 23))
          }
          tabhab <- ecoval[[name]][partial_select,]
          tabhab[[3]] <- shortindicnames[partial_select]
          dat1 <- data.frame(
            perimetres = tabhab[[1]],
            indicateurs = tabhab[[3]],
            criteres = factor(tabhab[[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            valeurs = as.numeric(tabhab[[4]])
          )
          couleurs <- c("Diversité habitat" = "#83D072",
                        "Diversité espèce" ="#1E6218",
                        "Patrimonialité_PS" = "#9D403E",
                        "Fonctionnalité" = "#4894DC",
                        "Pression_PS" = "#E6A936",
                        "Connectivité" = "#AF76C4",
                        "Représentativité" = "#68DDEA",
                        "Patrimonialité_PE" = "#842D2A",
                        "Pression_PE" = "#DE9830",
                        "Structure" = "grey")
          dat1$valeurs[is.na(dat1$valeurs)] <- 0
          dat1$valeurs[is.nan(dat1$valeurs)] <- 0
          
          data1 <- dat1[which(dat1$criteres == "Diversité espèce"),]
          data2 <- dat1[which(dat1$criteres == "Fonctionnalité"),]
          data3 <- dat1[which(dat1$criteres == "Pression_PS"),]
          data4 <- dat1[which(dat1$criteres == "Connectivité"),]
          data5 <- dat1[which(dat1$criteres == "Représentativité"),]
          data6 <- dat1[which(dat1$criteres == "Structure"),]
          
          p1 <- plot_ly(data1,
                        x = as.character(data1$indicateurs), y = data1$valeurs,
                        type = 'bar', 
                        marker = list(color = couleurs["Diversité espèce"])) %>%
            layout(xaxis = list(title = "Diversité espèce", showticklabels=FALSE))
          
          p2 <- plot_ly(data2,
                        x = as.character(data2$indicateurs), y = data2$valeurs,
                        type = 'bar',
                        marker = list(color = couleurs["Fonctionnalité"])) %>%
            layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE))
          
          p3 <- plot_ly(data3,
                        x = as.character(data3$indicateurs), y = data3$valeurs,
                        type = 'bar',
                        marker = list(color = couleurs["Pression_PS"])) %>%
            layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE))
          
          p4 <- plot_ly(data4,
                        x = as.character(data4$indicateurs), y = data4$valeurs,
                        type = 'bar',
                        marker = list(color = couleurs["Connectivité"])) %>%
            layout(xaxis = list(title = "Connectivité", showticklabels=FALSE))
          
          p5 <- plot_ly(data5,
                        x = as.character(data5$indicateurs), y = data5$valeurs,
                        type = 'bar',
                        marker = list(color = couleurs["Représentativité"])) %>%
            layout(xaxis = list(title = "Représentativité", showticklabels=FALSE))
          
          p6 <- plot_ly(data6,
                        x = as.character(data6$indicateurs), y = data6$valeurs,
                        type = 'bar',
                        marker = list(color = couleurs["Structure"])) %>%
            layout(xaxis = list(title = "Structure", showticklabels=FALSE))
          
          blankplot0 <- plotly_empty(type = "scatter", mode = "markers")  %>% layout(yaxis = list(title = "Valeurs à l'état initial"))
          blankplot <- plotly_empty(type = "scatter", mode = "markers")
          
          p <- subplot(p1,p2,p3,blankplot0,blankplot,blankplot,p4,p5,p6, nrows = 3, titleX = TRUE, titleY = TRUE) %>%
            layout(title = "INDICATEURS", showlegend=FALSE)

          
        }else if(input$selecttypegraphperte == '2'){
          
        # Pertes CT
          namehab <- paste("Habitat", input$selecthabitatSI2)
          partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
          if(ecoval[[namehab]][4,2] == "1"){ # Forestier
            partial_select <- c(partial_select, c(10,11,12,13,14,21))
          }else if(ecoval[[namehab]][4,2] == "2"){ # Ouvert
            partial_select <- c(partial_select, c(15, 22))
          }else if(ecoval[[namehab]][4,2] == "4"){ # Zone humide
            partial_select <- c(partial_select, c(16, 23))
          }
          tabhab <- ecoval[[name]][partial_select,]
          tabhab[[3]] <- shortindicnames[partial_select]
          dat1 <- data.frame(
            perimetres = tabhab[[1]],
            indicateurs = tabhab[[3]],
            criteres = factor(tabhab[[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            incertitudes <- gsub("[ABC]", "*", tabhab[[7]]),
            pertes_brutes <- as.numeric(tabhab[[8]]) - as.numeric(tabhab[[4]]),
            pertes_relatives <- (as.numeric(tabhab[[8]]) - as.numeric(tabhab[[4]])) * 100 / as.numeric(tabhab[[4]])
          )
          dat1[[5]][is.na(dat1[[5]])] <- 0
          dat1[[5]][is.nan(dat1[[5]])] <- 0
          dat1[[6]][is.na(dat1[[6]])] <- 0
          dat1[[6]][is.nan(dat1[[6]])] <- 0
          dat1[[6]][is.infinite(dat1[[6]])] <- 0
          
          couleurs <- c("negative" = "#C67677",
                        "positive" ="#7FDD4C")
          
          data1 <- dat1[which(dat1$criteres == "Diversité espèce"),]
          data2 <- dat1[which(dat1$criteres == "Fonctionnalité"),]
          data3 <- dat1[which(dat1$criteres == "Pression_PS"),]
          data4 <- dat1[which(dat1$criteres == "Connectivité"),]
          data5 <- dat1[which(dat1$criteres == "Représentativité"),]
          data6 <- dat1[which(dat1$criteres == "Structure"),]

          p1 <- plot_ly(data1,
                        y = as.character(data1$indicateurs), x = data1[[6]],
                        type = 'bar', text = paste(data1[[5]], gsub("[ABC]", "*", data1[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data1[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Diversité espèce", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p2 <- plot_ly(data2,
                        y = as.character(data2$indicateurs), x = data2[[6]],
                        type = 'bar', text = paste(data2[[5]], gsub("[ABC]", "*", data2[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data2[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p3 <- plot_ly(data3,
                        y = as.character(data3$indicateurs), x = data3[[6]],
                        type = 'bar', text = paste(data3[[5]], gsub("[ABC]", "*", data3[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data3[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p4 <- plot_ly(data4,
                        y = as.character(data4$indicateurs), x = data4[[6]],
                        type = 'bar', text = paste(data4[[5]], gsub("[ABC]", "*", data4[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data4[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p5 <- plot_ly(data5,
                        y = as.character(data5$indicateurs), x = data5[[6]],
                        type = 'bar', text = paste(data5[[5]], gsub("[ABC]", "*", data5[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data5[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p6 <- plot_ly(data6,
                        y = as.character(data6$indicateurs), x = data6[[6]],
                        type = 'bar', text = paste(data6[[5]], gsub("[ABC]", "*", data6[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data6[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Structure", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p <- subplot(p1, p2, p3, p4, p5, p6, titleX = TRUE, titleY = FALSE) %>%
            layout(title = "PERTES RELATIVES (barre) ET BRUTES (nombre)", showlegend=FALSE)
          
        }else if(input$selecttypegraphperte == '3'){
          
        # Pertes LT
          namehab <- paste("Habitat", input$selecthabitatSI2)
          partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
          if(ecoval[[namehab]][4,2] == "1"){ # Forestier
            partial_select <- c(partial_select, c(10,11,12,13,14,21))
          }else if(ecoval[[namehab]][4,2] == "2"){ # Ouvert
            partial_select <- c(partial_select, c(15, 22))
          }else if(ecoval[[namehab]][4,2] == "4"){ # Zone humide
            partial_select <- c(partial_select, c(16, 23))
          }
          tabhab <- ecoval[[name]][partial_select,]
          tabhab[[3]] <- shortindicnames[partial_select]
          dat1 <- data.frame(
            perimetres = tabhab[[1]],
            indicateurs = tabhab[[3]],
            criteres = factor(tabhab[[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            #valeurs = as.numeric(tabhab[[4]],
            incertitudes <- gsub("[ABC]", "*", tabhab[[10]]),
            pertes_brutes <- as.numeric(tabhab[[11]]) - as.numeric(tabhab[[4]]),
            pertes_relatives <- (as.numeric(tabhab[[11]]) - as.numeric(tabhab[[4]])) * 100 / as.numeric(tabhab[[4]])
          )
          dat1[[5]][is.na(dat1[[5]])] <- 0
          dat1[[5]][is.nan(dat1[[5]])] <- 0
          dat1[[6]][is.na(dat1[[6]])] <- 0
          dat1[[6]][is.nan(dat1[[6]])] <- 0
          dat1[[6]][is.infinite(dat1[[6]])] <- 0
          
          couleurs <- c("negative" = "#C67677",
                        "positive" ="#7FDD4C")
          
          data1 <- dat1[which(dat1$criteres == "Diversité espèce"),]
          data2 <- dat1[which(dat1$criteres == "Fonctionnalité"),]
          data3 <- dat1[which(dat1$criteres == "Pression_PS"),]
          data4 <- dat1[which(dat1$criteres == "Connectivité"),]
          data5 <- dat1[which(dat1$criteres == "Représentativité"),]
          data6 <- dat1[which(dat1$criteres == "Structure"),]
          
          p1 <- plot_ly(data1,
                        y = as.character(data1$indicateurs), x = data1[[6]],
                        type = 'bar', text = paste(data1[[5]], gsub("[ABC]", "*", data1[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data1[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Diversité espèce", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p2 <- plot_ly(data2,
                        y = as.character(data2$indicateurs), x = data2[[6]],
                        type = 'bar', text = paste(data2[[5]], gsub("[ABC]", "*", data2[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data2[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p3 <- plot_ly(data3,
                        y = as.character(data3$indicateurs), x = data3[[6]],
                        type = 'bar', text = paste(data3[[5]], gsub("[ABC]", "*", data3[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data3[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p4 <- plot_ly(data4,
                        y = as.character(data4$indicateurs), x = data4[[6]],
                        type = 'bar', text = paste(data4[[5]], gsub("[ABC]", "*", data4[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data4[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p5 <- plot_ly(data5,
                        y = as.character(data5$indicateurs), x = data5[[6]],
                        type = 'bar', text = paste(data5[[5]], gsub("[ABC]", "*", data5[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data5[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p6 <- plot_ly(data6,
                        y = as.character(data6$indicateurs), x = data6[[6]],
                        type = 'bar', text = paste(data6[[5]], gsub("[ABC]", "*", data6[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data6[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Structure", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p <- subplot(p1, p2, p3, p4, p5, p6, titleX = TRUE, titleY = FALSE) %>%
            layout(title = "PERTES RELATIVES (barre) ET BRUTES (nombre)", showlegend=FALSE)
        }
      }else{
        shinyjs::hide("dwnlpertes")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
    }else if(input$selectniveauperte == '3'){
      
      ### Niveau Espece
      
      if(input$selectspeciesSI2 != '0'){
        shinyjs::show("dwnlpertes")
        
        # Etat initial
        name <- paste("DI no.", input$selectspeciesSI2)
        shortindicnames <- c("Surf tot hab favorable",
                             "Nb patchs hab favorable",
                             "Estimation nb indiv",
                             "Surf nourrissage favorable",
                             "Surf reproduction favorable",
                             "Estimation nb couples",
                             "Surf chasse favorable",
                             "Nb gîtes favorables",
                             "Nb mâles chanteurs",
                             "Nb pontes",
                             "% plante(s) hôte(s)",
                             "Nb station / pieds",
                             "Nombre d'sp",
                             "Nombre de familles",
                             "% Surf SANS perturbation",
                             "Surf hab favorable _ PE",
                             "Nb osb sp",
                             "Surf hab favorable connecté _ PE",
                             "Nb zones connectées entre elles")
        
        if(input$selecttypegraphperte == '1'){
          partial_select <- c(1,2,15,16,17,18,19)
          namesp  <- paste("Espece", input$selectspeciesSI2)
          if(ecoval[[namesp]][3,2] != "7"){ # Faune
            partial_select <- c(partial_select, c(3))
          }
          if(ecoval[[namesp]][3,2] == "1"){ # Avifaune
            partial_select <- c(partial_select, c(4,5,6))
          }else if(ecoval[[namesp]][3,2] == "2"){ # Chiroptere
            partial_select <- c(partial_select, c(7,8))
          }else if(ecoval[[namesp]][3,2] == "4"){ # Amphibien
            partial_select <- c(partial_select, c(9,10))
          }else if(ecoval[[namesp]][3,2] == "6"){ # Insecte
            partial_select <- c(partial_select, c(11))
          }else if(ecoval[[namesp]][3,2] == "7"){ # Flore
            partial_select <- c(partial_select, c(12))
          }else if(ecoval[[namesp]][3,2] == "10"){ # Communaute faunistique
            partial_select <- c(partial_select, c(13,14))
          }
          tabsp <- ecoval[[name]][partial_select,]
          tabsp[[3]] <- shortindicnames[partial_select]
          dat1 <- data.frame(
            perimetres = tabsp[[1]],
            indicateurs = tabsp[[3]],
            criteres = factor(tabsp[[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            valeurs = as.numeric(tabsp[[4]])
          )
          dat1$valeurs[is.na(dat1$valeurs)] <- 0
          dat1$valeurs[is.nan(dat1$valeurs)] <- 0
          couleurs <- c("Diversité habitat" = "#83D072",
                        "Diversité espèce" ="#1E6218",
                        "Patrimonialité_PS" = "#9D403E",
                        "Fonctionnalité" = "#4894DC",
                        "Pression_PS" = "#E6A936",
                        "Connectivité" = "#AF76C4",
                        "Représentativité" = "#68DDEA",
                        "Patrimonialité_PE" = "#842D2A",
                        "Pression_PE" = "#DE9830",
                        "Structure" = "grey")
          
          data1 <- dat1[which(dat1$criteres == "Fonctionnalité"),]
          data2 <- dat1[which(dat1$criteres == "Pression_PS"),]
          data3 <- dat1[which(dat1$criteres == "Connectivité"),]
          data4 <- dat1[which(dat1$criteres == "Représentativité"),]
          
          p1 <- plot_ly(data1,
                        x = as.character(data1$indicateurs), y = data1$valeurs,
                        type = 'bar', 
                        marker = list(color = couleurs["Fonctionnalité"])) %>%
            layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                   yaxis = list(title = "Valeurs à l'état initial"))
          
          p2 <- plot_ly(data2,
                        x = as.character(data2$indicateurs), y = data2$valeurs,
                        type = 'bar',
                        marker = list(color = couleurs["Pression_PS"])) %>%
            layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE))
          
          p3 <- plot_ly(data3,
                        x = as.character(data3$indicateurs), y = data3$valeurs,
                        type = 'bar',
                        marker = list(color = couleurs["Connectivité"])) %>%
            layout(xaxis = list(title = "Connectivité", showticklabels=FALSE))
          
          p4 <- plot_ly(data4,
                        x = as.character(data4$indicateurs), y = data4$valeurs,
                        type = 'bar',
                        marker = list(color = couleurs["Représentativité"])) %>%
            layout(xaxis = list(title = "Représentativité", showticklabels=FALSE))
          
          p <- subplot(p1,p2,p3,p4,nrows = 1, titleX = TRUE, titleY = TRUE) %>%
            layout(title = "INDICATEURS", showlegend=FALSE)
          
        }else if(input$selecttypegraphperte == '2'){
          
          # Pertes CT
          partial_select <- c(1,2,15,16,17,18,19)
          namesp  <- paste("Espece", input$selectspeciesSI2)
          if(ecoval[[namesp]][3,2] != "7"){ # Faune
            partial_select <- c(partial_select, c(3))
          }
          if(ecoval[[namesp]][3,2] == "1"){ # Avifaune
            partial_select <- c(partial_select, c(4,5,6))
          }else if(ecoval[[namesp]][3,2] == "2"){ # Chiroptere
            partial_select <- c(partial_select, c(7,8))
          }else if(ecoval[[namesp]][3,2] == "4"){ # Amphibien
            partial_select <- c(partial_select, c(9,10))
          }else if(ecoval[[namesp]][3,2] == "6"){ # Insecte
            partial_select <- c(partial_select, c(11))
          }else if(ecoval[[namesp]][3,2] == "7"){ # Flore
            partial_select <- c(partial_select, c(12))
          }else if(ecoval[[namesp]][3,2] == "10"){ # Communaute faunistique
            partial_select <- c(partial_select, c(13,14))
          }
          tabsp <- ecoval[[name]][partial_select,]
          tabsp[[3]] <- shortindicnames[partial_select]
          dat1 <- data.frame(
            perimetres = tabsp[[1]],
            indicateurs = tabsp[[3]],
            criteres = factor(tabsp[[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            incertitudes <- gsub("[ABC]", "*", tabsp[[7]]),
            pertes_brutes <- as.numeric(tabsp[[8]]) - as.numeric(tabsp[[4]]),
            pertes_relatives <- (as.numeric(tabsp[[8]]) - as.numeric(tabsp[[4]])) * 100 / as.numeric(tabsp[[4]])
          )
          dat1[[5]][is.na(dat1[[5]])] <- 0
          dat1[[5]][is.nan(dat1[[5]])] <- 0
          dat1[[6]][is.na(dat1[[6]])] <- 0
          dat1[[6]][is.nan(dat1[[6]])] <- 0
          dat1[[6]][is.infinite(dat1[[6]])] <- 0
          
          data1 <- dat1[which(dat1$criteres == "Fonctionnalité"),]
          data2 <- dat1[which(dat1$criteres == "Pression_PS"),]
          data3 <- dat1[which(dat1$criteres == "Connectivité"),]
          data4 <- dat1[which(dat1$criteres == "Représentativité"),]
          
          couleurs <- c("negative" = "#C67677",
                        "positive" ="#7FDD4C")
          p1 <- plot_ly(data1,
                        y = as.character(data1$indicateurs), x = data1[[6]],
                        type = 'bar', text = paste(data1[[5]], gsub("[ABC]", "*", data1[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data1[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p2 <- plot_ly(data2,
                        y = as.character(data2$indicateurs), x = data2[[6]],
                        type = 'bar', text = paste(data2[[5]], gsub("[ABC]", "*", data2[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data2[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p3 <- plot_ly(data3,
                        y = as.character(data3$indicateurs), x = data3[[6]],
                        type = 'bar', text = paste(data3[[5]], gsub("[ABC]", "*", data3[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data3[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p4 <- plot_ly(data4,
                        y = as.character(data4$indicateurs), x = data4[[6]],
                        type = 'bar', text = paste(data4[[5]], gsub("[ABC]", "*", data4[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data4[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p <- subplot(p1, p2, p3, p4, titleX = TRUE, titleY = FALSE) %>%
            layout(title = "PERTES RELATIVES (barre) ET BRUTES (nombre)", showlegend=FALSE)
          
        }else if(input$selecttypegraphperte == '3'){
          
          # Pertes LT
          partial_select <- c(1,2,15,16,17,18,19)
          namesp  <- paste("Espece", input$selectspeciesSI2)
          if(ecoval[[namesp]][3,2] != "7"){ # Faune
            partial_select <- c(partial_select, c(3))
          }
          if(ecoval[[namesp]][3,2] == "1"){ # Avifaune
            partial_select <- c(partial_select, c(4,5,6))
          }else if(ecoval[[namesp]][3,2] == "2"){ # Chiroptere
            partial_select <- c(partial_select, c(7,8))
          }else if(ecoval[[namesp]][3,2] == "4"){ # Amphibien
            partial_select <- c(partial_select, c(9,10))
          }else if(ecoval[[namesp]][3,2] == "6"){ # Insecte
            partial_select <- c(partial_select, c(11))
          }else if(ecoval[[namesp]][3,2] == "7"){ # Flore
            partial_select <- c(partial_select, c(12))
          }else if(ecoval[[namesp]][3,2] == "10"){ # Communaute faunistique
            partial_select <- c(partial_select, c(13,14))
          }
          tabsp <- ecoval[[name]][partial_select,]
          tabsp[[3]] <- shortindicnames[partial_select]
          dat1 <- data.frame(
            perimetres = tabsp[[1]],
            indicateurs = tabsp[[3]],
            criteres = factor(tabsp[[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            incertitudes <- gsub("[ABC]", "*", tabsp[[10]]),
            pertes_brutes <- as.numeric(tabsp[[11]]) - as.numeric(tabsp[[4]]),
            pertes_relatives <- (as.numeric(tabsp[[11]]) - as.numeric(tabsp[[4]])) * 100 / as.numeric(tabsp[[4]])
          )
          dat1[[5]][is.na(dat1[[5]])] <- 0
          dat1[[5]][is.nan(dat1[[5]])] <- 0
          dat1[[6]][is.na(dat1[[6]])] <- 0
          dat1[[6]][is.nan(dat1[[6]])] <- 0
          dat1[[6]][is.infinite(dat1[[6]])] <- 0
          
          data1 <- dat1[which(dat1$criteres == "Fonctionnalité"),]
          data2 <- dat1[which(dat1$criteres == "Pression_PS"),]
          data3 <- dat1[which(dat1$criteres == "Connectivité"),]
          data4 <- dat1[which(dat1$criteres == "Représentativité"),]
          
          couleurs <- c("negative" = "#C67677",
                        "positive" ="#7FDD4C")
          p1 <- plot_ly(data1,
                        y = as.character(data1$indicateurs), x = data1[[6]],
                        type = 'bar', text = paste(data1[[5]], gsub("[ABC]", "*", data1[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data1[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p2 <- plot_ly(data2,
                        y = as.character(data2$indicateurs), x = data2[[6]],
                        type = 'bar', text = paste(data2[[5]], gsub("[ABC]", "*", data2[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data2[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p3 <- plot_ly(data3,
                        y = as.character(data3$indicateurs), x = data3[[6]],
                        type = 'bar', text = paste(data3[[5]], gsub("[ABC]", "*", data3[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data3[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p4 <- plot_ly(data4,
                        y = as.character(data4$indicateurs), x = data4[[6]],
                        type = 'bar', text = paste(data4[[5]], gsub("[ABC]", "*", data4[[4]])), textposition='outside', orientation = 'h',
                        marker = list(color = ifelse(data4[[6]] < 0, couleurs[1], couleurs[2]))) %>%
            layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                   yaxis = list(showticklabels=FALSE))
          
          p <- subplot(p1, p2, p3, p4, titleX = TRUE, titleY = FALSE) %>%
            layout(title = "PERTES RELATIVES (barre) ET BRUTES (nombre)", showlegend=FALSE)
        }
      }else{
        shinyjs::hide("dwnlpertes")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
    }
    dat1[['colour']] <- NULL
    pertes$tableau <- dat1
  }else{
    p <- plotly_empty(type = "scatter", mode = "markers")
    pertes$tableau <- NULL
  }
  p
})

output$SIcalcul <- DT::renderDataTable({
  if(dim(pertes$tableau)[2] < 5){
    mydata <- pertes$tableau
    mycolnames <- c("perimetres" = 1, "indicateurs" = 2, "criteres" = 3, "valeurs"= 4)
  }else{
      mydata <- pertes$tableau[, c(1,2,3,4,5,6)]
      mycolnames <- c("perimetres" = 1, "indicateurs" = 2, "criteres" = 3, "incertitudes"= 4, "pertes brutes" = 5, "pertes relatives" = 6)
    }
  dat <- datatable(mydata, rownames = FALSE, colnames = mycolnames, options = list(pageLength = dim.data.frame(pertes$tableau)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")
  pertes$tableau <- mydata
  colnames(pertes$tableau) <- names(mycolnames)
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
    write.csv2(pertes$tableau, file, row.names = FALSE, fileEncoding = "latin1")
  }
)
