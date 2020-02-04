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
    shinyjs::hide("selecthabitatSE")
    shinyjs::hide("selectspeciesSE")
  }else{
    if(input$selectsitecompens3 == '0'){
      shinyjs::hide("plot_equivalence")
      shinyjs::hide("SEcalcul")
      shinyjs::hide("dwnlequivalence")
      shinyjs::hide("selecttypegraphequivalence")
      shinyjs::hide("selectniveauequivalence")
      shinyjs::hide("selecthabitatSE")
      shinyjs::hide("selectspeciesSE")
    }else{
      shinyjs::show("plot_equivalence")
      shinyjs::show("SEcalcul")
      shinyjs::show("dwnlequivalence")
      shinyjs::show("selecttypegraphequivalence")
      shinyjs::show("selectniveauequivalence")
      updateSelectInput(session, "selecttypegraphequivalence", selected = '1')
      updateSelectInput(session, "selectniveauequivalence", selected = '1')
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
    shinyjs::hide("selecthabitatSE")
    shinyjs::hide("selectspeciesSE")
  }else{
    if(input$selectsiteimpact3 == '0'){
      shinyjs::hide("plot_equivalence")
      shinyjs::hide("SEcalcul")
      shinyjs::hide("dwnlequivalence")
      shinyjs::hide("selecttypegraphequivalence")
      shinyjs::hide("selectniveauequivalence")
      shinyjs::hide("selecthabitatSE")
      shinyjs::hide("selectspeciesSE")
    }else{
      shinyjs::show("plot_equivalence")
      shinyjs::show("SEcalcul")
      shinyjs::show("dwnlequivalence")
      shinyjs::show("selecttypegraphequivalence")
      shinyjs::show("selectniveauequivalence")
      updateSelectInput(session, "selecttypegraphequivalence", selected = '1')
      updateSelectInput(session, "selectniveauequivalence", selected = '1')
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

output$plot_equivalence_ui <- renderUI({
  if(input$selectniveauequivalence == '1'){ # Niveau General
      plot.width = "100%"
      plot.height = "100%"
  }else if(input$selectniveauequivalence == '2'){ # Niveau Habitat
      plot.width = "100%"
      plot.height = "100%"
  }else if(input$selectniveauequivalence == '3'){ # Niveau Espece
      plot.width = "100%"
      plot.height = "100%"
  }
  plotlyOutput('plot_equivalence', width = plot.width, height = plot.height)
})

output$plot_equivalence <- renderPlotly({
  if(input$selectsiteimpact3 != '0' & input$selectsitecompens3 != '0'){
    
    ### Niveau General
    
    if(input$selectniveauequivalence == '1'){
      nameImp <- paste("SIB no.", input$selectsiteimpact3)
      nameComp <- paste("SCB no.", input$selectsitecompens3)
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
          indicateurs = shortindicnames,
          criteres = factor(ecoval[[nameImp]][[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          equivalence = equivalCT)
        
        dat1[[4]][is.na(dat1[[4]])] <- 0
        
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
                      y = as.character(data1$indicateurs), x = data1[[4]],
                      type = 'bar', text = data1[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data1[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Diversité habitat", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p2 <- plot_ly(data2,
                      y = as.character(data2$indicateurs), x = data2[[4]],
                      type = 'bar', text = data2[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data2[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Diversité espèce", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p3 <- plot_ly(data3,
                      y = as.character(data3$indicateurs), x = data3[[4]],
                      type = 'bar', text = data3[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data3[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Patrimonialité_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p4 <- plot_ly(data4,
                      y = as.character(data4$indicateurs), x = data4[[4]],
                      type = 'bar', text = data4[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data4[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p5 <- plot_ly(data5,
                      y = as.character(data5$indicateurs), x = data5[[4]],
                      type = 'bar', text = data5[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data5[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p6 <- plot_ly(data6,
                      y = as.character(data6$indicateurs), x = data6[[4]],
                      type = 'bar', text = data6[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data6[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p7 <- plot_ly(data7,
                      y = as.character(data7$indicateurs), x = data7[[4]],
                      type = 'bar', text = data7[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data7[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p8 <- plot_ly(data8,
                      y = as.character(data8$indicateurs), x = data8[[4]],
                      type = 'bar', text = data8[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data8[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Patrimonialité_PE", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p9 <- plot_ly(data9,
                      y = as.character(data9$indicateurs), x = data9[[4]],
                      type = 'bar', text = data9[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data9[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PE", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p <- subplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, titleX = TRUE, titleY = FALSE) %>%
          layout(title = "PERTES / GAINS NETS", showlegend=FALSE)

        dat1["absence perte nette"] <- natzero
        dat1["pertes brutes"] <- moins
        dat1["gains bruts"] <- plus
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
          indicateurs = shortindicnames, #ecoval[[nameImp]][[3]],
          criteres = factor(ecoval[[nameImp]][[2]], levels=c("Diversité habitat","Diversité espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          equivalence = equivalLT)

        dat1[[4]][is.na(dat1[[4]])] <- 0
        
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
                      y = as.character(data1$indicateurs), x = data1[[4]],
                      type = 'bar', text = data1[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data1[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Diversité habitat", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p2 <- plot_ly(data2,
                      y = as.character(data2$indicateurs), x = data2[[4]],
                      type = 'bar', text = data2[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data2[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Diversité espèce", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p3 <- plot_ly(data3,
                      y = as.character(data3$indicateurs), x = data3[[4]],
                      type = 'bar', text = data3[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data3[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Patrimonialité_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p4 <- plot_ly(data4,
                      y = as.character(data4$indicateurs), x = data4[[4]],
                      type = 'bar', text = data4[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data4[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p5 <- plot_ly(data5,
                      y = as.character(data5$indicateurs), x = data5[[4]],
                      type = 'bar', text = data5[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data5[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p6 <- plot_ly(data6,
                      y = as.character(data6$indicateurs), x = data6[[4]],
                      type = 'bar', text = data6[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data6[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p7 <- plot_ly(data7,
                      y = as.character(data7$indicateurs), x = data7[[4]],
                      type = 'bar', text = data7[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data7[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p8 <- plot_ly(data8,
                      y = as.character(data8$indicateurs), x = data8[[4]],
                      type = 'bar', text = data8[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data8[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Patrimonialité_PE", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p9 <- plot_ly(data9,
                      y = as.character(data9$indicateurs), x = data9[[4]],
                      type = 'bar', text = data9[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data9[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PE", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p <- subplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, titleX = TRUE, titleY = FALSE) %>%
          layout(title = "PERTES / GAINS NETS", showlegend=FALSE)

        dat1["absence perte nette"] <- natzero
        dat1["pertes brutes"] <- moins
        dat1["gains bruts"] <- plus
      }
      
    }else if(input$selectniveauequivalence == '2'){
      
      ### Niveau Habitat
      
      if(input$selecthabitatSE != '0'){
        shinyjs::show("dwnlequivalence")
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
                moins <- as.numeric(ecoval[[nameImp]][[8]]) - as.numeric(ecoval[[nameImp]][[4]])
              }
              if(ecoval[[hname]][7,2] == ecoval[[nameC]][12,2]){ # Gains
                nameComp <- paste("CC no.", as.character(listhabitat$index[i]))
                nameIC <- nameComp
                plus <- as.numeric(ecoval[[nameComp]][[8]]) - as.numeric(ecoval[[nameComp]][[4]])
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
                moins <- as.numeric(ecoval[[nameImp]][[11]]) - as.numeric(ecoval[[nameImp]][[4]])
              }
              if(ecoval[[hname]][7,2] == ecoval[[nameC]][12,2]){ # Gains
                nameComp <- paste("CC no.", as.character(listhabitat$index[i]))
                nameIC <- nameComp
                plus <- as.numeric(ecoval[[nameComp]][[11]]) - as.numeric(ecoval[[nameComp]][[4]])
              }
            }
          }
        }
        equival <- moins + plus
        natzero <- (equival == 0) & (moins != 0)
        natzero[natzero==TRUE] <- '*'
        natzero[natzero==FALSE] <- ''
        namehab <- paste("Habitat", input$selecthabitatSE)
        partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
        if(ecoval[[namehab]][4,2] == "1"){ # Fermé
          partial_select <- c(partial_select, c(10,11,12,13,14,21))
        }else if(ecoval[[namehab]][4,2] == "2"){ # Ouvert
          partial_select <- c(partial_select, c(15, 22))
        }else if(ecoval[[namehab]][4,2] == "4"){ # Zone humide
          partial_select <- c(partial_select, c(16, 23))
        }
        tabhab <- ecoval[[nameIC]][partial_select,]
        tabhab[[3]] <- shortindicnames[partial_select]
        equival <- equival[partial_select]
        natzero <- natzero[partial_select]
        plus <- plus[partial_select]
        moins <- moins[partial_select]

        dat1 <- data.frame(
          perimetres = tabhab[[1]],
          indicateurs = tabhab[[3]],
          criteres = factor(tabhab[[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression_PS", "Connectivité", "Représentativité")),
          equivalence = equival)
        
        dat1[[4]][is.na(dat1[[4]])] <- 0
        
        couleurs <- c("negative" = "#C67677",
                      "positive" ="#7FDD4C")
        
        data1 <- dat1[which(dat1$criteres == "Diversité espèce"),]
        data2 <- dat1[which(dat1$criteres == "Fonctionnalité"),]
        data3 <- dat1[which(dat1$criteres == "Pression_PS"),]
        data4 <- dat1[which(dat1$criteres == "Connectivité"),]
        data5 <- dat1[which(dat1$criteres == "Représentativité"),]
        data6 <- dat1[which(dat1$criteres == "Structure"),]
        
        p1 <- plot_ly(data1,
                      y = as.character(data1$indicateurs), x = data1[[4]],
                      type = 'bar', text = data1[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data1[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Diversité espèce", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p2 <- plot_ly(data2,
                      y = as.character(data2$indicateurs), x = data2[[4]],
                      type = 'bar', text = data2[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data2[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p3 <- plot_ly(data3,
                      y = as.character(data3$indicateurs), x = data3[[4]],
                      type = 'bar', text = data3[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data3[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p4 <- plot_ly(data4,
                      y = as.character(data4$indicateurs), x = data4[[4]],
                      type = 'bar', text = data4[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data4[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p5 <- plot_ly(data5,
                      y = as.character(data5$indicateurs), x = data5[[4]],
                      type = 'bar', text = data5[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data5[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p6 <- plot_ly(data6,
                      y = as.character(data6$indicateurs), x = data6[[4]],
                      type = 'bar', text = data6[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data6[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Structure", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p <- subplot(p1, p2, p3, p4, p5, p6, titleX = TRUE, titleY = FALSE) %>%
          layout(title = "PERTES / GAINS NETS", showlegend=FALSE)
        
        dat1["absence perte nette"] <- natzero
        dat1["pertes brutes"] <- moins
        dat1["gains bruts"] <- plus
      }else{
        shinyjs::hide("dwnlequivalence")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
      
    }else if(input$selectniveauequivalence == '3'){
      
      ### Niveau Espece
      
      if(input$selectspeciesSE != '0'){
        shinyjs::show("dwnlequivalence")
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
                moins <- as.numeric(ecoval[[nameImp]][[8]]) - as.numeric(ecoval[[nameImp]][[4]])
              }
              if(ecoval[[sname]][6,2] == ecoval[[nameC]][12,2]){ # Gains
                nameComp <- paste("DC no.", as.character(listspecies$index[i]))
                nameIC <- nameComp
                plus <- as.numeric(ecoval[[nameComp]][[8]]) - as.numeric(ecoval[[nameComp]][[4]])
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
                moins <- as.numeric(ecoval[[nameImp]][[11]]) - as.numeric(ecoval[[nameImp]][[4]])
              }
              if(ecoval[[sname]][6,2] == ecoval[[nameC]][12,2]){ # Gains
                nameComp <- paste("DC no.", as.character(listspecies$index[i]))
                nameIC <- nameComp
                plus <- as.numeric(ecoval[[nameComp]][[11]]) - as.numeric(ecoval[[nameComp]][[4]])
              }
            }
          }
        }
        equival <- moins + plus
        natzero <- (equival == 0) & (moins != 0)
        natzero[natzero==TRUE] <- '*'
        natzero[natzero==FALSE] <- ''
        
        partial_select <- c(1,2,15,16,17,18,19)
        namesp  <- paste("Espece", input$selectspeciesSE)
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
        tabsp <- ecoval[[nameIC]][partial_select,]
        tabsp[[3]] <- shortindicnames[partial_select]
        equival <- equival[partial_select]
        natzero <- natzero[partial_select]
        plus <- plus[partial_select]
        moins <- moins[partial_select]
        
        dat1 <- data.frame(
          perimetres = tabsp[[1]],
          indicateurs = tabsp[[3]],
          criteres = factor(tabsp[[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression_PS", "Connectivité", "Représentativité")),
          equivalence = equival)
        
        dat1[[4]][is.na(dat1[[4]])] <- 0
        
        data1 <- dat1[which(dat1$criteres == "Fonctionnalité"),]
        data2 <- dat1[which(dat1$criteres == "Pression_PS"),]
        data3 <- dat1[which(dat1$criteres == "Connectivité"),]
        data4 <- dat1[which(dat1$criteres == "Représentativité"),]
        
        couleurs <- c("negative" = "#C67677",
                      "positive" ="#7FDD4C")
        p1 <- plot_ly(data1,
                      y = as.character(data1$indicateurs), x = data1[[4]],
                      type = 'bar', text = data1[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data1[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Fonctionnalité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p2 <- plot_ly(data2,
                      y = as.character(data2$indicateurs), x = data2[[4]],
                      type = 'bar', text = data2[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data2[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Pression_PS", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p3 <- plot_ly(data3,
                      y = as.character(data3$indicateurs), x = data3[[4]],
                      type = 'bar', text = data3[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data3[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Connectivité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p4 <- plot_ly(data4,
                      y = as.character(data4$indicateurs), x = data4[[4]],
                      type = 'bar', text = data4[[4]], textposition='auto', orientation = 'h',
                      marker = list(color = ifelse(data4[[4]] < 0, couleurs[1], couleurs[2]))) %>%
          layout(xaxis = list(title = "Représentativité", showticklabels=FALSE),
                 yaxis = list(showticklabels=FALSE))
        
        p <- subplot(p1, p2, p3, p4, titleX = TRUE, titleY = FALSE) %>%
          layout(title = "PERTES / GAINS NETS", showlegend=FALSE)
        
        
        # dat1$colour <- ifelse(dat1$equivalence < 0, "negative","positive")
        # p <- ggplot(data=dat1, aes(x=indicateurs, y= equivalence)) +
        # geom_bar(stat="identity",  width=0.5, aes(fill=colour))+
        #   coord_flip()+
        #   labs(x="Indicateurs", y="Pertes / Gains NETS")+
        #   scale_fill_manual(values=c(positive="#7FDD4C",negative="#C67677")) +
        #   theme_bw()+
        #   theme(legend.position="none")+
        #   theme(axis.text.x=element_text(colour="black", size = 11))+
        #   geom_text(aes(label=equivalence, hjust="center", vjust="center", y= equivalence))+
        #   theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
        #   facet_grid(criteres ~ ., scales = "free", space = "free")
      
        
        dat1["absence perte nette"] <- natzero
        dat1["pertes brutes"] <- moins
        dat1["gains bruts"] <- plus
      }else{
        shinyjs::hide("dwnlequivalence")
        dat1 <- NULL
        p <- plotly_empty(type = "scatter", mode = "markers")
      }
    }
    dat1[['colour']] <- NULL
    equivalence$tableau <- dat1
  }else{
    p <- plotly_empty(type = "scatter", mode = "markers")
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
    write.csv2(equivalence$tableau, file, row.names = FALSE, fileEncoding = "latin1")
  }
)
