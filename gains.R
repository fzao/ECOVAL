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
  updateTabsetPanel(session, "resultgains", selected = "graphe")
  if(input$selectsitecompens2 == '0'){
    shinyjs::hide("plot_gains")
    shinyjs::hide("SCcalcul")
    shinyjs::hide("dwnlgains")
    shinyjs::hide("selecttypegraphgain")
    shinyjs::hide("selectniveaugain")
  }else{
    shinyjs::show("plot_gains")
    shinyjs::show("SCcalcul")
    shinyjs::show("dwnlgains")
    shinyjs::show("selecttypegraphgain")
    shinyjs::show("selectniveaugain")
  }
})

observeEvent(input$selectniveaugain, {
  updateTabsetPanel(session, "resultgains", selected = "graphe")
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

observeEvent(input$selecttypegraphgain, updateTabsetPanel(session, "resultgains", selected = "graphe"))
observeEvent(input$selecthabitatSC2, updateTabsetPanel(session, "resultgains", selected = "graphe"))
observeEvent(input$selectspeciesSC2, updateTabsetPanel(session, "resultgains", selected = "graphe"))

output$plot_gains <- renderPlot({
  if(input$selectsitecompens2 != '0'){
    
    ### Niveau General
    
    if(input$selectniveaugain == '1'){
      name <- paste("SCB no.", input$selectsitecompens2)
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
      if(input$selecttypegraphgain == '1'){
        dat1 <- data.frame(
          perimetres = ecoval[[name]][[1]],
          indicateurs = shortindicnames, #ecoval[[name]][[3]]
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE")),
          valeurs = as.numeric(ecoval[[name]][[4]])
        )
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
          labs(x="Indicateurs", y="Valeur à l'état initial")+
          facet_grid(.~criteres, scales = "free", space ="free")+
          theme (axis.text.x = element_text(colour="black", angle = 45, size = 10, hjust = 1))
        # renderPlot(p, height = 800, width = 800)
        
      }else if(input$selecttypegraphgain == '2'){
        
        # Gains CT
        dat1 <- data.frame(
          perimetres = ecoval[[name]][[1]],
          indicateurs = shortindicnames, # ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
          # valeurs = as.numeric(ecoval[[name]][[7]]),
          incertitudes <- ecoval[[name]][[6]],
          gains_bruts <- as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]]),
          gains_relatifs <- (as.numeric(ecoval[[name]][[7]]) - as.numeric(ecoval[[name]][[4]])) * 100 / as.numeric(ecoval[[name]][[4]])
        )
        
        p <- ggplot(data=dat1, aes(x=indicateurs, y=gains_relatifs)) +
          geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(gains_relatifs))))+
          coord_flip()+
          labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
          scale_fill_manual(values=c("#C67677", "#7FDD4C", "#7FDD4C"))+
          theme_bw()+
          theme(legend.position="none")+
          geom_text(aes(label=gains_bruts, hjust="center", vjust="center", y=gains_relatifs*0.5), size=3)+
          theme(axis.text.x=element_text(colour="black", size = 11))+
          geom_text(aes(label=incertitudes, hjust="center", vjust="bottom", y= gains_relatifs))+
          theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
          facet_grid(criteres ~ ., scales = "free", space = "free")
        # plotOutput(p, width = "80%", height = "100%")
        
      }else if(input$selecttypegraphgain == '3'){
        
        # Gains LT
        dat1 <- data.frame(
          perimetres = ecoval[[name]][[1]],
          indicateurs = shortindicnames, # ecoval[[name]][[3]],
          criteres = factor(ecoval[[name]][[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
          # valeurs = as.numeric(ecoval[[name]][[7]]),
          incertitudes <- ecoval[[name]][[6]],
          gains_bruts <- as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]]),
          gains_relatifs <- (as.numeric(ecoval[[name]][[10]]) - as.numeric(ecoval[[name]][[4]])) * 100 / as.numeric(ecoval[[name]][[4]])
        )
        
        p <- ggplot(data=dat1, aes(x=indicateurs, y=gains_relatifs)) +
          geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(gains_relatifs))))+
          coord_flip()+
          labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
          scale_fill_manual(values=c("#C67677", "#7FDD4C", "#7FDD4C"))+
          theme_bw()+
          theme(legend.position="none")+
          geom_text(aes(label=gains_bruts, hjust="center", vjust="center", y=gains_relatifs*0.5), size=3)+
          theme(axis.text.x=element_text(colour="black", size = 11))+
          geom_text(aes(label=incertitudes, hjust="center", vjust="bottom", y= gains_relatifs))+
          theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
          facet_grid(criteres ~ ., scales = "free", space = "free")
        
      }
    }else if(input$selectniveaugain == '2'){
      
      ### Niveau Habitat  
      
      if(input$selecthabitatSC2 != '0'){
        shinyjs::show("dwnlgains")
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
        
        # Etat initial
        name <- paste("CC no.", input$selecthabitatSC2)
        if(input$selecttypegraphgain == '1'){
          # cfz
          namehab <- paste("Habitat", input$selecthabitatSC2)
          partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
          if(ecoval[[namehab]][4,2] == "1"){ # Fermé
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
            criteres = factor(tabhab[[2]], levels=c("Diversité espèce", "Fonctionnalité", "Structure", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(tabhab[[4]])
          )
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
            labs(x="Indicateurs", y="Valeur à l'état initial")+
            facet_grid(.~criteres, scales = "free", space ="free")+
            theme (axis.text.x = element_text(colour="black", angle = 45, size = 10, hjust = 1))
          # p <- ggplotly(p, width=1000, height=800)
          
        }else if(input$selecttypegraphgain == '2'){
          
          # Gains CT
          namehab <- paste("Habitat", input$selecthabitatSC2)
          partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
          if(ecoval[[namehab]][4,2] == "1"){ # Fermé
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
            criteres = factor(tabhab[[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            # valeurs = as.numeric(tabhab[[7]]),
            incertitudes <- tabhab[[6]],
            gains_bruts <- as.numeric(tabhab[[7]]) - as.numeric(tabhab[[4]]),
            gains_relatifs <- (as.numeric(tabhab[[7]]) - as.numeric(tabhab[[4]])) * 100 / as.numeric(tabhab[[4]])
          )
          
          p <- ggplot(data=dat1, aes(x=indicateurs, y=gains_relatifs)) +
            geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(gains_relatifs))))+
            coord_flip()+
            labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
            scale_fill_manual(values=c("#C67677", "#7FDD4C", "#7FDD4C"))+
            theme_bw()+
            theme(legend.position="none")+
            geom_text(aes(label=gains_bruts, hjust="center", vjust="center", y=gains_relatifs*0.5), size=3)+
            theme(axis.text.x=element_text(colour="black", size = 11))+
            geom_text(aes(label=incertitudes, hjust="center", vjust="bottom", y= gains_relatifs))+
            theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
            facet_grid(criteres ~ ., scales = "free", space = "free")
          # p <- ggplotly(p, width=1100, height=800)
          
        }else if(input$selecttypegraphgain == '3'){
          
          # Gains LT
          namehab <- paste("Habitat", input$selecthabitatSC2)
          partial_select <- c(1,2,3,4,5,6,7,8,9,16,17,18,19,20,24,25,26)
          if(ecoval[[namehab]][4,2] == "1"){ # Fermé
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
            criteres = factor(tabhab[[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            #valeurs = as.numeric(tabhab[[7]]),
            incertitudes <- tabhab[[6]],
            gains_bruts <- as.numeric(tabhab[[10]]) - as.numeric(tabhab[[4]]),
            gains_relatifs <- (as.numeric(tabhab[[10]]) - as.numeric(tabhab[[4]])) * 100 / as.numeric(tabhab[[4]])
          )
          
          p <- ggplot(data=dat1, aes(x=indicateurs, y=gains_relatifs)) +
            geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(gains_relatifs))))+
            coord_flip()+
            labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
            scale_fill_manual(values=c("#C67677", "#7FDD4C", "#7FDD4C"))+
            theme_bw()+
            theme(legend.position="none")+
            geom_text(aes(label=gains_bruts, hjust="center", vjust="center", y=gains_relatifs*0.5), size=3)+
            theme(axis.text.x=element_text(colour="black", size = 11))+
            geom_text(aes(label=incertitudes, hjust="center", vjust="bottom", y= gains_relatifs))+
            theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
            facet_grid(criteres ~ ., scales = "free", space = "free")
          # p <- ggplotly(p, width=1100, height=800)
          
        }
      }else{
        shinyjs::hide("dwnlgains")
        dat1 <- NULL
        p <- ggplot() + theme_void()
      }
    }else if(input$selectniveaugain == '3'){
      
      ### Niveau Espece
      
      if(input$selectspeciesSC2 != '0'){
        shinyjs::show("dwnlgains")
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
        
        # Etat initial
        name <- paste("DC no.", input$selectspeciesSC2)
        if(input$selecttypegraphgain == '1'){
          partial_select <- c(1,2,15,16,17,18,19)
          namesp  <- paste("Espece", input$selectspeciesSC2)
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
            criteres = factor(tabsp[[2]], levels=c("Diversité espèce", "Fonctionnalité", "Pression", "Connectivité", "Représentativité")),
            valeurs = as.numeric(tabsp[[4]]))
          
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
              labs(x="Indicateurs", y="Valeur à l'état initial")+
              facet_grid(.~criteres, scales = "free", space ="free")+
              theme (axis.text.x = element_text(colour="black", angle = 45, size = 10, hjust = 1))
          
        }else if(input$selecttypegraphgain == '2'){
          
          # Gains CT
          partial_select <- c(1,2,15,16,17,18,19)
          namesp  <- paste("Espece", input$selectspeciesSC2)
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
            criteres = factor(tabsp[[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            # valeurs = as.numeric(tabsp[[7]]),
            incertitudes <- tabsp[[6]],
            gains_bruts <- as.numeric(tabsp[[7]]) - as.numeric(tabsp[[4]]),
            gains_relatifs <- (as.numeric(tabsp[[7]]) - as.numeric(tabsp[[4]])) * 100 / as.numeric(tabsp[[4]])
          )
          
          p <- ggplot(data=dat1, aes(x=indicateurs, y=gains_relatifs)) +
            geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(gains_relatifs))))+
            coord_flip()+
            labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
            scale_fill_manual(values=c("#C67677", "#7FDD4C", "#7FDD4C"))+
            theme_bw()+
            theme(legend.position="none")+
            geom_text(aes(label=gains_bruts, hjust="center", vjust="center", y=gains_relatifs*0.5), size=3)+
            theme(axis.text.x=element_text(colour="black", size = 11))+
            geom_text(aes(label=incertitudes, hjust="center", vjust="bottom", y= gains_relatifs))+
            theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
            facet_grid(criteres ~ ., scales = "free", space = "free")
          # p <- ggplotly(p, width=1000, height=800)
          
        }else if(input$selecttypegraphgain == '3'){
          
          # Gains LT
          partial_select <- c(1,2,15,16,17,18,19)
          namesp  <- paste("Espece", input$selectspeciesSC2)
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
            criteres = factor(tabsp[[2]], levels=c("Diversité habitat","Diversité Espèce","Patrimonialité_PS","Fonctionnalité","Pression_PS","Connectivité","Représentativité","Patrimonialité_PE","Pression_PE", "Structure")),
            # valeurs = as.numeric(tabsp[[7]]),
            incertitudes <- tabsp[[6]],
            gains_bruts <- as.numeric(tabsp[[10]]) - as.numeric(tabsp[[4]]),
            gains_relatifs <- (as.numeric(tabsp[[10]]) - as.numeric(tabsp[[4]])) * 100 / as.numeric(tabsp[[4]])
          )
          
          p <- ggplot(data=dat1, aes(x=indicateurs, y=gains_relatifs)) +
            geom_bar(stat="identity",  width=0.5, aes(fill=as.factor(sign(gains_relatifs))))+
            coord_flip()+
            labs(x="Indicateurs", y="Pertes relatives (barre) et brutes (nombre)")+
            scale_fill_manual(values=c("#C67677", "#7FDD4C", "#7FDD4C"))+
            theme_bw()+
            theme(legend.position="none")+
            geom_text(aes(label=gains_bruts, hjust="center", vjust="center", y=gains_relatifs*0.5), size=3)+
            theme(axis.text.x=element_text(colour="black", size = 11))+
            geom_text(aes(label=incertitudes, hjust="center", vjust="bottom", y= gains_relatifs))+
            theme(panel.grid.major = element_line(size = 0.5, colour = "light grey"))+
            facet_grid(criteres ~ ., scales = "free", space = "free")
          # p <- ggplotly(p, width=1000, height=800)
          
        }
      }else{
        shinyjs::hide("dwnlgains")
        dat1 <- NULL
        p <- ggplot() + theme_void()
      }
    }
    gains$tableau <- dat1
  }else{
    p <- ggplot() + theme_void()
    gains$tableau <- NULL
  }
  p
})

output$SCcalcul <- DT::renderDataTable({
  if(dim(gains$tableau)[2] < 5){
    mydata <- gains$tableau
    mycolnames <- c("perimetres" = 1, "indicateurs" = 2, "criteres" = 3, "valeurs"= 4)
  }else{
    mydata <- gains$tableau[, c(1,2,3,4,5)]
    mycolnames <- c("perimetres" = 1, "indicateurs" = 2, "criteres" = 3, "incertitudes"= 4, "gains bruts" = 5)
  }
  dat <- datatable(mydata, rownames = FALSE, colnames = mycolnames, options = list(pageLength = dim.data.frame(gains$tableau)[1], searching = TRUE, dom = 'ft', ordering = FALSE), filter = "top")
  return(dat)
})

output$dwnlgains  <- downloadHandler(
  filename = function() {
    radix <- paste(listsite[as.numeric(input$selectsitecompens2)+1, 3], '_', sep = "")
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
      habitat <- listhabitat[as.numeric(input$selecthabitatSC2)+1, 3]
    }
    else if(input$selectniveaugain == '3'){
      niveau <- "Espèce_"
      habitat <- ""
      species <- listspecies[as.numeric(input$selectspeciesSC2)+1, 3]
    }
    paste(radix, type, '_', niveau, habitat, species, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(pertes$tableau, file, row.names = FALSE)
  }
)