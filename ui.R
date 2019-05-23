#
#   ______ _____ ______      __     _      
#  |  ____/ ____/ __ \ \    / /\   | |     
#  | |__ | |   | |  | \ \  / /  \  | |     
#  |  __|| |   | |  | |\ \/ / /\ \ | |     
#  | |___| |___| |__| | \  / ____ \| |____ 
#  |______\_____\____/   \/_/    \_\______|
#
# Cadre methodologique pour le calcul de l'equivalence ecologique dans le contexte de la sequence ERC en France
#
# # Copyright (c) EDF-IRSTEA 2019
#
# Auteurs : Fabrice Zaoui - Lucie Bezombes
#
# Licence CeCILL v2.1
#

library(shiny)
library(shinyjs)
library(plotly)
library(xlsx)
library(DT)
library(leaflet)

Species <- readRDS("model/Species_names.rds")

# Define UI for application that draws a histogram
shinyUI(fluidPage(HTML("<!DOCTYPE html>
                    <html>
                    <head>
                    <link type=\"text/css\" rel=\"stylesheet\" href=\"carousel.css\"/>
                    </head>
                    <body>
                    </body>
                    </html>"),
                  title="ECOVAL",
                  useShinyjs(),
                  fluidRow(
                    column(10, align="left",
                           HTML('<h1 style="color: #FFA02F; background-color: #FFFFFF;"><b>ECOVAL</b></h1>'),
                           HTML('<h4 style="color: #A5C226; background-color: #FFFFFF;"><b>Cadre méthodologique pour le calcul de l\'équivalence écologique dans le contexte de la séquence ERC en France</b></h5>')),
                    column(2, align="right",
                           img(height=40, width=80, src="edf.jpg"),
                           img(height=80, width=77, src="Irstea.png"))
                  ),
                  tabsetPanel(id="tabs",
                              # ACCUEIL ---------------------------------------------------
                              tabPanel(value="accueil", HTML('<h4 style="color: #005BBB; "><b>Accueil</b></h4>'), br(), br(),
                                       includeMarkdown("md/accueil_1.md"),
                                       actionLink(inputId = "redir1", label=HTML('<h4 style="color: #005BBB;" >Pour commencer à utiliser ECOVAL vous devrez créer un projet.</h4>')),
                                       includeMarkdown("md/accueil_2.md"),
                                       actionLink(inputId = "redir2", label=HTML('<h4 style="color: #005BBB;" >1. Evaluation de l\'état initial du site impacté</h4>')),
                                       actionLink(inputId = "redir3", label=HTML('<h4 style="color: #005BBB;" >2. Prédiction de la valeur des indicateurs sur le site impacté après impacts (à court et long terme) et calcul du delta entre état initial et après impact («pertes»)</h4>')),
                                       actionLink(inputId = "redir4", label=HTML('<h4 style="color: #005BBB;" >3. Evaluation de l’état initial du site compensatoire</h4>')),
                                       actionLink(inputId = "redir5", label=HTML('<h4 style="color: #005BBB;" >4. Prédiction de la valeur des indicateurs sur le site compensatoire après compensation (à court et long terme) et calcul du delta entre état initial et après compensation («gains»)</h4>')),
                                       actionLink(inputId = "redir6", label=HTML('<h4 style="color: #005BBB;" >5. Calcul de l’équilibre entre pertes et gains pour chaque indicateur et évaluation de l’équivalence écologique globale</h4>')), br(),
                                       includeMarkdown("md/accueil_3.md"),
                                       actionLink(inputId = "redir7", label=HTML('<h4 style="color: #005BBB;" >Pour la phase d’interprétation des résultats, veuillez vous référer à la notice explicative disponible dans l’onglet «à propos»</h4>'))),
                              # PROJET ----------------------------------------------------
                              tabPanel(value="projet", HTML('<h4 style="color: #005BBB; "><b>Projet</b></h4>'), br(),
                                       sidebarLayout(
                                         sidebarPanel(
                                           titlePanel(HTML('<h5><b>FICHIER DU PROJET</b></h5>')),
                                           fluidRow(column(9, align="left", tags$div(title="Importe un fichier ECOVAL .xlsx", fileInput("userfile", NULL, multiple = FALSE, accept = ".xlsx", buttonLabel = 'Charger', placeholder = '...'))),
                                                    column(3, align="left", tags$div(title="Télécharge le projet ECOVAL en cours",downloadButton("btn_telecharger", "Sauver")))),
                                           fluidRow(column(7, align="left", textInput("projectname", "TITRE", placeholder = "Titre du projet ECOVAL...")),
                                                    column(5, align="left", tags$div(title="Date de réalisation du projet", dateInput("date", label = "DATE", format = "dd-mm-yyyy", value = Sys.Date())))),
                                           textAreaInput("projectcontext", "CONTEXTE", height='300px', placeholder = "Décrire le contexte du projet ici..."), br(),
                                           selectInput("selectsite", label = "SITE ECOLOGIQUE", 
                                                       choices = list("-" = 0), 
                                                       selected = 0),
                                           fluidRow(column(4, align="center", tags$div(title="Créer un nouveau site", actionButton("new", "AJOUTER"))),
                                                    column(4, align="center", tags$div(title="Effacer la description du site en cours", actionButton("delete", "EFFACER"))),
                                                    column(4, align="center", tags$div(title="Supprime définitivement le site en cours", actionButton("destroy", "ENLEVER")))), br(),
                                           leafletOutput("projectmap"),
                                           width = 3
                                         ),
                                         mainPanel(
                                           tabsetPanel(id="prjtabs",
                                             tabPanel(HTML('<h4 style="color: #005BBB; ">Description</h4>'), value="description", br(),
                                                      fluidRow(column(12, align="center", htmlOutput("viewsiteno", inline = TRUE))), br(), br(),
                                                      fluidRow(column(4, align="left", textInput("sitename", "NOM", placeholder = "Nom du site...")),
                                                               column(4, align="left", selectInput("sitetype", label = "TYPE", choices = list("Impacté" = 1, "Compensatoire" = 2, "Impacté et Compensatoire" = 3))),
                                                               column(4, align="left", numericInput("surface", "SURFACE (ha)", value = 0., min = 0., max = 1e6, step = 1.))),
                                                      fluidRow(column(12, align="left", textAreaInput("sitecontext", "CONTEXTE", height='170px', width='1250px', placeholder = "Décrire le contexte du site ici..."))),
                                                      fluidRow(column(4, align="left", numericInput("latitude", "LATITUDE", 0.)),
                                                               column(4, align="left", numericInput("longitude", "LONGITUDE", 0.))),
                                                      fluidRow(column(12, align = "left", textAreaInput("descqual", "DESCRIPTION QUALITATIVE", height='90px', width='1250px', placeholder = "Nature, emprise, effets indirects..."))),
                                                      fluidRow(column(12, align = "left",textAreaInput("tempo", "TEMPORALITE", height='90px', width='1250px', placeholder = "Plusieurs phases? Court/long terme..."))),
                                                      fluidRow(column(4, align="left", actionLink(inputId = "link1", label=HTML('<h5><b>DUREE</b></h5>'))),
                                                               column(4, align="left", actionLink(inputId = "link2", label=HTML('<h5><b>INTENSITE</b></h5>'))),
                                                               column(4, align="left", actionLink(inputId = "link3", label=HTML('<h5><b>PORTEE SPATIALE</b></h5>')))
                                                               ),
                                                      fluidRow(column(4, align = "left", selectInput("duree", label = NA, choices = list("Temporaire Courte Durée" = 1, "Temporaire Longue Durée" = 2, "Permanent" = 3))),
                                                               column(4, align = "left", selectInput("intensite", label = NA, choices = list("Peu Intense" = 1, "Intense" = 2, "Très Intense" = 3))),
                                                               column(4, align = "left",    selectInput("portee", label = NA, choices = list("Ponctuelle Faible Surface" = 1, "Ponctuelle Surface Importante" = 2, "Linéaire" = 3))))
                                             ),
                                             tabPanel(HTML('<h4 style="color: #005BBB; ">Identification Des Enjeux</h4>'), value="identification", br(),
                                                      fluidRow(column(12, align="center", htmlOutput("enjeusiteno", inline = TRUE))),
                                                      fluidRow(column(6, align="center", HTML('<h4 style="color: #878F99; "><b>ESPECES</b></h4>')), column(6, align="center", HTML('<h4 style="color: #878F99; "><b>HABITATS</b></h4>'))),
                                                      fluidPage(
                                                        column(6, align="center",
                                                               selectInput("selectspecies", label = NULL, 
                                                                           choices = list("-" = 0), 
                                                                           selected = 0),
                                                               fluidRow(column(4, align="right", tags$div(title="Créer une nouvelle espèce", actionButton("newspecies", "AJOUTER"))),
                                                                        column(4, align="center", tags$div(title="Effacer le contenu de l'espèce en cours", actionButton("deletespecies", "EFFACER"))),
                                                                        column(4, align="left", tags$div(title="Supprime définitivement l'espèce en cours", actionButton("destroyspecies", "ENLEVER")))), br(), br(),
                                                               fluidRow(column(4, align="left", textInput("latinnamespecies", "Nom Latin")), 
                                                                        column(4, align="left", textInput("frenchnamespecies", "Nom Français")),
                                                                        column(4, align="left", selectInput("typespecies", label = "Type", choices = list("Avifaune" = 1, "Chiroptère" = 2, "Mammifère" = 3, "Amphibien" = 4, "Reptile" = 5, "Insecte" = 6, "Flore" = 7, "Poisson" = 8, "Crustacé/Mollusque" = 9, "Communauté faunistique" = 10)))), br(),
                                                               fluidRow(column(8, align="left", textAreaInput("justifyspecies", label = "Justification de l'enjeu", height='200px', width='390px')),
                                                                        column(4, align="left", selectInput("presencespecies", label = "Présence sur site impacté", choices = list("Oui" = 1, "Non" = 2))))
                                                               ),
                                                        column(6, align="center",
                                                               selectInput("selecthabitat", label = NULL, 
                                                                           choices = list("-" = 0), 
                                                                           selected = 0),
                                                               fluidRow(column(4, align="right", tags$div(title="Créer un nouvel habitat", actionButton("newhabitat", "AJOUTER"))),
                                                                        column(4, align="center", tags$div(title="Effacer le contenu de l'habitat en cours", actionButton("deletehabitat", "EFFACER"))),
                                                                        column(4, align="left", tags$div(title="Supprime définitivement l'habitat en cours", actionButton("destroyhabitat", "ENLEVER")))), br(), br(),
                                                               fluidRow(column(3, align="left", textInput("namehabitat", "Nom")),
                                                                        column(3, align="left", textInput("codecorinehabitat", "Code Corine")),
                                                                        column(3, align="left", textInput("codeeunishabitat", "Code Eunis")),
                                                                        column(3, align="left", selectInput("typehabitat", label = "Type", choices = list("Fermé" = 1, "Ouvert" = 2, "Buissonnant" = 3, "Zone humide" = 4, "Aquatique" = 5, "Rocheux" = 6, "Cultivé" = 7, "Imperméabilisé" = 8)))), br(),
                                                               fluidRow(column(8, align="left", textAreaInput("justifyhabitat", label = "Justification de l'enjeu", height='200px', width='390px')),
                                                                        column(4, align="left", selectInput("presencehabitat", label = "Présence sur site impacté", choices = list("Oui" = 1, "Non" = 2))))
                                                               )
                                                      )
                                             )
                                           ),
                                           width = 9)
                                       )
                                       # leafletOutput("projectmap")
                                      ),
                              # SITE IMPACTE ----------------------------------------------
                              tabPanel(value="siteimpact", HTML('<h4 style="color: #005BBB; "><b>Site impacté</b></h4>'), br(),
                                       selectInput("selectsiteimpact", label = "SELECTIONNER LE SITE", choices = list("-" = 0), selected = 0), hr(),
                                       conditionalPanel(condition = "input.selectsiteimpact != '0'",
                                         tabsetPanel(id="descrimpact",
                                                     tabPanel(HTML('<h4 style="color: #005BBB; ">Entrée des données</h4>'), value="impactindata", br(),
                                                              fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">LISTE DES HABITATS</h4>'))), br(),
                                                              fluidRow(column(3, align="left", textInput("SInamehabitat", "Nom habitat")),
                                                                       column(3, align="left", textInput("SIcodecorine", "Code Corine", placeholder = "ex. : 41.12")),
                                                                       column(3, align="left", textInput("SIcodeeunis", "Code Eunis", placeholder = "ex. : G1.21")),
                                                                       column(3, align="left", numericInput("SIsurface", "Surface (ha)", value = 0., min = 0., step = 0.01))),
                                                              fluidRow(column(3, align="left", selectInput("SItype", label = "Type", choices = list("Fermé" = 1, "Ouvert" = 2, "Buissonnant" = 3, "Zone humide" = 4, "Aquatique" = 5, "Rocheux" = 6, "Cultivé" = 7, "Imperméabilisé" = 8))),
                                                                       column(3, align="left", selectInput("SIetat", label = "Etat conservation", choices = list("Bon" = 1, "Mauvais" = 2, "Moyen" = 3))),
                                                                       column(3, align="left", selectInput("SIinteret", label = "Intérêt communautaire", choices = list("Oui" = 1, "Non" = 2))),
                                                                       column(3, align="left", selectInput("SImenace", label = "En danger ou menacé localement", choices = list("Oui" = 1, "Non" = 2)))),
                                                              fluidRow(column(3, align="left", numericInput("SIsurfacedeg", "Surface dégradée (ha)", value = 0., min = 0., step = 0.01))),
                                                              fluidRow(column(6, align="right", actionButton("addlisthab", "AJOUTER")), column(6, align="left", actionButton("dellisthab", "SUPRRIMER"))), br(),
                                                              DT::dataTableOutput("SItable1"), br(), br(), br(), hr(), br(), br(), br(), 
                                                              fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">LISTE DES ESPECES</h4>'))), br(),
                                                              fluidRow(column(2, align="left", textInput("SIlatinnamespecies", "Nom latin")),
                                                                       column(2, align="left", textInput("SIfrenchnamespecies", "Nom français")),
                                                                       column(2, align="left", selectInput("SItype1", label = "Type 1", choices = list("Avifaune" = 1, "Chiroptère" = 2, "Mammifère" = 3, "Amphibien" = 4, "Reptile" = 5, "Insecte" = 6, "Flore" = 7, "Poisson" = 8, "Crustacé/Mollusque" = 9))),
                                                                       column(2, align="left", selectInput("SItype2", label = "Type 2", choices = list("Cortège forestier" = 1, "Cortège agricole" = 2, "Cortège du bâti" = 3, "Cortège généraliste" = 4, "Odonate" = 5, "Lépidoptère" = 6, "Orthoptère" = 7, "Coléoptère" = 8))),
                                                                       column(2, align="left", selectInput("SIprotect", label = "Protection nationale ou régionale", choices = list("Oui" = 1, "Non" = 2))),
                                                                       column(2, align="left", selectInput("SIrougeF", label = "Liste rouge (CR,VU,EN) France", choices = list("Oui" = 1, "Non" = 2)))),
                                                              fluidRow(column(2, align="left", selectInput("SIrougeR", label = "Liste rouge (CR,VU,EN) Régional", choices = list("Oui" = 1, "Non" = 2))),
                                                                       column(2, align="left", selectInput("SIdirect", label = "Directives Européennes", choices = list("-" = 0, "Annexe II DFFH" = 1, "Annexe I DO" = 2))),
                                                                       column(2, align="left", selectInput("SIreprod", label = "Reproduction", choices = list("-" = 0, "Certaine" = 1, "Possible" = 2))),
                                                                       column(2, align="left", selectInput("SIexo", label = "Espèce Exotique Envahissante", choices = list("Oui" = 1, "Non" = 2))),
                                                                       column(2, align="left", selectInput("SItvb", label = "TVB", choices = list("Oui" = 1, "Non" = 2))),
                                                                       column(2, align="left", selectInput("SIdet", label = "Déterminant Znieff dans le PE", choices = list("Oui" = 1, "Non" = 2)))),
                                                              fluidRow(column(2, align="left", selectInput("SIindssi", label = "Indice spécialisation", choices = split(seq_along(Species), Species)))),
                                                              fluidRow(column(6, align="right", actionButton("addlistesp", "AJOUTER")), column(6, align="left", actionButton("dellistesp", "SUPRRIMER"))), br(),
                                                              DT::dataTableOutput("SItable2"), br(), br(), br(), hr(), br(), br(), br(),
                                                              fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">PERIMETRE ELARGI</h4>'))), br(),
                                                              fluidRow(column(3, align="left", selectInput("SIpertype", label = "Type", choices = list("Fermé" = 1, "Ouvert" = 2, "Buissonnant" = 3, "Zone Humide" = 4, "Aquatique" = 5, "Rocheux" = 6, "Cultivé" = 7, "Imperméabilisé" = 8))),
                                                                       column(3, align="left", textInput("SIpercouche", "Couche SIG EUNIS", placeholder = "ex. : 41 (forêt décidue)")),
                                                                       column(3, align="left", textInput("SIpercode", "Code SIG OSO", placeholder = "ex. : 34 (pelouse)")),
                                                                       column(3, align="left", numericInput("SIpersurf", "Surface (ha)", value = 0., min = 0., step = 0.01))),
                                                              fluidRow(column(6, align="right", actionButton("addlistper", "AJOUTER")), column(6, align="left", actionButton("dellistper", "SUPRRIMER"))), br(),
                                                              DT::dataTableOutput("SItable3")
                                                     ),
                                                     tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Général</h4>'), value="impactindicng", br(),
                                                              fluidRow(column(6, align="center", HTML('<h4 style="color: #A5C226; ">COURT TERME</h4>')), column(6, align="center", HTML('<h4 style="color: #A5C226; ">LONG TERME</h4>'))), br(),
                                                              fluidRow(column(2, align="left", textAreaInput("SIjustifCT", "Justification de l'estimation", height='250px', placeholder = "justification de la prédiction faite")),
                                                                       column(2, align="left", checkboxGroupInput("SIdegincCT", "Degré d'incertitude",
                                                                                          c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                            "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                            "Définition de l'emprise de l'impact : l'emprise impactée est-elle en lien direct avec la valeur prédite de l'indicateur ?" = "3"))),
                                                                       column(2, align="left", numericInput("SIvalCT", "Valeur après impact", value = 0.)),
                                                                       column(2, align="left", textAreaInput("SIjustifLT", "Justification de l'estimation", height='250px', placeholder = "justification de la prédiction faite")),
                                                                       column(2, align="left", checkboxGroupInput("SIdegincLT", "Degré d'incertitude",
                                                                                                                   c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                     "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                     "Définition de l'emprise de l'impact : l'emprise impactée est-elle en lien direct avec la valeur prédite de l'indicateur ?" = "3"))),
                                                                       column(2, align="left", numericInput("SIvalLT", "Valeur après impact", value = 0.))), br(),
                                                              fluidRow(column(6, align="right", actionButton("renseigner", "RENSEIGNER")),
                                                                       column(6, align="left", tags$style("#Manuel {background-color:#FFA02F;}"), numericInput("Manuel", NA, value = 0.))), br(),
                                                              DT::dataTableOutput("SItable4")
                                                     ),
                                                     tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Habitat</h4>'), value="impactindicnh", br(),
                                                              selectInput("selecthabitatSI", label = "SELECTIONNER L'HABITAT", choices = list("-" = 0), selected = 0), br(),
                                                              fluidRow(column(6, align="center", HTML('<h4 style="color: #A5C226; ">COURT TERME</h4>')), column(6, align="center", HTML('<h4 style="color: #A5C226; ">LONG TERME</h4>'))), br(),
                                                              fluidRow(column(2, align="left", textAreaInput("SIjustifCTNH", "Justification de prédiction", height='250px', placeholder = "justification de la prédiction court terme")),
                                                                       column(2, align="left", checkboxGroupInput("SIdegincCTNH", "Incertitudes",
                                                                                                                  c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                    "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                    "Définition de l'emprise de l'impact : l'emprise impactée est-elle en lien direct avec la valeur prédite de l'indicateur ?" = "3"))),
                                                                       column(2, align="left", numericInput("SIvalCTNH", "Valeur après impact CT", value = 0.)),
                                                                       column(2, align="left", textAreaInput("SIjustifLTNH", "Justification de prédiction", height='250px', placeholder = "justification de la prédiction long terme")),
                                                                       column(2, align="left", checkboxGroupInput("SIdegincLTNH", "Incertitudes",
                                                                                                                  c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                    "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                    "Définition de l'emprise de l'impact : l'emprise impactée est-elle en lien direct avec la valeur prédite de l'indicateur ?" = "3"))),
                                                                       column(2, align="left", numericInput("SIvalLTNH", "Valeur après impact LT", value = 0.))), br(),
                                                              fluidRow(column(6, align="right", actionButton("renseignerNH", "RENSEIGNER")),
                                                                       column(6, align="left", tags$style("#ManuelNH {background-color:#FFA02F;}"), numericInput("ManuelNH", NA, value = 0.))), br(),
                                                              DT::dataTableOutput("SItable5")
                                                     ),
                                                     tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Espèce</h4>'), value="impactindicnsp", br(),
                                                              selectInput("selectspeciesSI", label = "SELECTIONNER L'ESPECE", choices = list("-" = 0), selected = 0), br(),
                                                              fluidRow(column(6, align="center", HTML('<h4 style="color: #A5C226; ">COURT TERME</h4>')), column(6, align="center", HTML('<h4 style="color: #A5C226; ">LONG TERME</h4>'))), br(),
                                                              fluidRow(column(2, align="left", textAreaInput("SIjustifCTNSP", "Justification de prédiction", height='250px', placeholder = "justification de la prédiction court terme")),
                                                                       column(2, align="left", checkboxGroupInput("SIdegincCTNSP", "Incertitudes",
                                                                                                                  c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                    "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                    "Définition de l'emprise de l'impact : l'emprise impactée est-elle en lien direct avec la valeur prédite de l'indicateur ?" = "3"))),
                                                                       column(2, align="left", numericInput("SIvalCTNSP", "Valeur après impact CT", value = 0.)),
                                                                       column(2, align="left", textAreaInput("SIjustifLTNSP", "Justification de prédiction", height='250px', placeholder = "justification de la prédiction long terme")),
                                                                       column(2, align="left", checkboxGroupInput("SIdegincLTNSP", "Incertitudes",
                                                                                                                  c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                    "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                    "Définition de l'emprise de l'impact : l'emprise impactée est-elle en lien direct avec la valeur prédite de l'indicateur ?" = "3"))),
                                                                       column(2, align="left", numericInput("SIvalLTNSP", "Valeur après impact LT", value = 0.))), br(),
                                                              fluidRow(column(6, align="right", actionButton("renseignerNSP", "RENSEIGNER")),
                                                                       column(6, align="left", tags$style("#ManuelNSP {background-color:#FFA02F;}"), numericInput("ManuelNSP", NA, value = 0.))), br(),
                                                              DT::dataTableOutput("SItable6")
                                                              
                                                     )
                                         )
                                       )
                              ),
                              # SITE COMPENSATOIRE ----------------------------------------------
                              tabPanel(value="compens", HTML('<h4 style="color: #005BBB; "><b>Site compensatoire</b></h4>'), br(),
                                      selectInput("selectsitecompens", label = "SELECTIONNER LE SITE", choices = list("-" = 0), selected = 0), hr(),
                                      conditionalPanel(condition = "input.selectsitecompens != '0'",
                                                       tabsetPanel(id="descrcompens",
                                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Entrée des données</h4>'), value="compensindata", br(),
                                                                            fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">LISTE DES HABITATS</h4>'))), br(),
                                                                            fluidRow(column(3, align="left", textInput("SCnamehabitat", "Nom habitat")),
                                                                                     column(3, align="left", textInput("SCcodecorine", "Code Corine", placeholder = "ex. : 41.12")),
                                                                                     column(3, align="left", textInput("SCcodeeunis", "Code Eunis", placeholder = "ex. : G1.21")),
                                                                                     column(3, align="left", numericInput("SCsurface", "Surface (ha)", value = 0., min = 0., step = 0.01))),
                                                                            fluidRow(column(3, align="left", selectInput("SCtype", label = "Type", choices = list("Fermé" = 1, "Ouvert" = 2, "Buissonnant" = 3, "Zone humide" = 4, "Aquatique" = 5, "Rocheux" = 6, "Cultivé" = 7, "Imperméabilisé" = 8))),
                                                                                     column(3, align="left", selectInput("SCetat", label = "Etat conservation", choices = list("Bon" = 1, "Mauvais" = 2, "Moyen" = 3))),
                                                                                     column(3, align="left", selectInput("SCinteret", label = "Intérêt communautaire", choices = list("Oui" = 1, "Non" = 2))),
                                                                                     column(3, align="left", selectInput("SCmenace", label = "En danger ou menacé localement", choices = list("Oui" = 1, "Non" = 2)))),
                                                                            fluidRow(column(3, align="left", numericInput("SCsurfacedeg", "Surface dégradée (ha)", value = 0., min = 0., step = 0.01))),
                                                                            fluidRow(column(6, align="right", actionButton("addlisthab2", "AJOUTER")), column(6, align="left", actionButton("dellisthab2", "SUPRRIMER"))), br(),
                                                                            DT::dataTableOutput("SCtable1"), br(), br(), br(), hr(), br(), br(), br(), 
                                                                            fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">LISTE DES ESPECES</h4>'))), br(),
                                                                            fluidRow(column(2, align="left", textInput("SClatinnamespecies", "Nom latin")),
                                                                                     column(2, align="left", textInput("SCfrenchnamespecies", "Nom français")),
                                                                                     column(2, align="left", selectInput("SCtype1", label = "Type 1", choices = list("Avifaune" = 1, "Chiroptère" = 2, "Mammifère" = 3, "Amphibien" = 4, "Reptile" = 5, "Insecte" = 6, "Flore" = 7, "Poisson" = 8, "Crustacé/Mollusque" = 9))),
                                                                                     column(2, align="left", selectInput("SCtype2", label = "Type 2", choices = list("Cortège forestier" = 1, "Cortège agricole" = 2, "Cortège du bâti" = 3, "Cortège généraliste" = 4, "Odonate" = 5, "Lépidoptère" = 6, "Orthoptère" = 7, "Coléoptère" = 8))),
                                                                                     column(2, align="left", selectInput("SCprotect", label = "Protection nationale ou régionale", choices = list("Oui" = 1, "Non" = 2))),
                                                                                     column(2, align="left", selectInput("SCrougeF", label = "Liste rouge (CR,VU,EN) France", choices = list("Oui" = 1, "Non" = 2)))),
                                                                            fluidRow(column(2, align="left", selectInput("SCrougeR", label = "Liste rouge (CR,VU,EN) Régional", choices = list("Oui" = 1, "Non" = 2))),
                                                                                     column(2, align="left", selectInput("SCdirect", label = "Directives Européennes", choices = list("-" = 0, "Annexe II DFFH" = 1, "Annexe I DO" = 2))),
                                                                                     column(2, align="left", selectInput("SCreprod", label = "Reproduction", choices = list("-" = 0, "Certaine" = 1, "Possible" = 2))),
                                                                                     column(2, align="left", selectInput("SCexo", label = "Espèce Exotique Envahissante", choices = list("Oui" = 1, "Non" = 2))),
                                                                                     column(2, align="left", selectInput("SCtvb", label = "TVB", choices = list("Oui" = 1, "Non" = 2))),
                                                                                     column(2, align="left", selectInput("SCdet", label = "Déterminant Znieff dans le PE", choices = list("Oui" = 1, "Non" = 2)))),
                                                                            fluidRow(column(2, align="left", selectInput("SCindssi", label = "Indice spécialisation", choices = split(seq_along(Species), Species)))),
                                                                            fluidRow(column(6, align="right", actionButton("addlistesp2", "AJOUTER")), column(6, align="left", actionButton("dellistesp2", "SUPRRIMER"))), br(),
                                                                            DT::dataTableOutput("SCtable2"), br(), br(), br(), hr(), br(), br(), br(),
                                                                            fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">PERIMETRE ELARGI</h4>'))), br(),
                                                                            fluidRow(column(3, align="left", selectInput("SCpertype", label = "Type", choices = list("Fermé" = 1, "Ouvert" = 2, "Buissonnant" = 3, "Zone Humide" = 4, "Aquatique" = 5, "Rocheux" = 6, "Cultivé" = 7, "Imperméabilisé" = 8))),
                                                                                     column(3, align="left", textInput("SCpercouche", "Couche SIG EUNIS", placeholder = "ex. : 41 (forêt décidue)")),
                                                                                     column(3, align="left", textInput("SCpercode", "Code SIG OSO", placeholder = "ex. : 34 (pelouse)")),
                                                                                     column(3, align="left", numericInput("SCpersurf", "Surface (ha)", value = 0., min = 0., step = 0.01))),
                                                                            fluidRow(column(6, align="right", actionButton("addlistper2", "AJOUTER")), column(6, align="left", actionButton("dellistper2", "SUPRRIMER"))), br(),
                                                                            DT::dataTableOutput("SCtable3")
                                                                   ),
                                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Général</h4>'), value="compensindicng", br(),
                                                                            fluidRow(column(6, align="center", HTML('<h4 style="color: #A5C226; ">COURT TERME</h4>')), column(6, align="center", HTML('<h4 style="color: #A5C226; ">LONG TERME</h4>'))), br(),
                                                                            fluidRow(column(2, align="left", textAreaInput("SCjustifCT", "Justification de l'estimation", height='250px', placeholder = "justification de la prédiction faite")),
                                                                                     column(2, align="left", checkboxGroupInput("SCdegincCT", "Degré d'incertitude",
                                                                                                                                c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                                  "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                                  "Risque d'échec associé à la mesure compensatoire : les résultats de l'action de compensation sont-ils bien maîtrisés?" = "3"))),
                                                                                     column(2, align="left", numericInput("SCvalCT", "Valeur après compensation", value = 0.)),
                                                                                     column(2, align="left", textAreaInput("SCjustifLT", "Justification de l'estimation", height='250px', placeholder = "justification de la prédiction faite")),
                                                                                     column(2, align="left", checkboxGroupInput("SCdegincLT", "Degré d'incertitude",
                                                                                                                                c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                                  "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                                  "Risque d'échec associé à la mesure compensatoire : les résultats de l'action de compensation sont-ils bien maîtrisés?" = "3"))),
                                                                                     column(2, align="left", numericInput("SCvalLT", "Valeur après compensation", value = 0.))), br(),
                                                                            fluidRow(column(6, align="right", actionButton("renseigner2", "RENSEIGNER")),
                                                                                     column(6, align="left", tags$style("#Manuel2 {background-color:#FFA02F;}"), numericInput("Manuel2", NA, value = 0.))), br(),
                                                                            DT::dataTableOutput("SCtable4")
                                                                   ),
                                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Habitat</h4>'), value="compensindicnh", br(),
                                                                            selectInput("selecthabitatSC", label = "SELECTIONNER L'HABITAT", choices = list("-" = 0), selected = 0), br(),
                                                                            fluidRow(column(6, align="center", HTML('<h4 style="color: #A5C226; ">COURT TERME</h4>')), column(6, align="center", HTML('<h4 style="color: #A5C226; ">LONG TERME</h4>'))), br(),
                                                                            fluidRow(column(2, align="left", textAreaInput("SCjustifCTNH", "Justification de prédiction", height='250px', placeholder = "justification de la prédiction court terme")),
                                                                                     column(2, align="left", checkboxGroupInput("SCdegincCTNH", "Incertitudes",
                                                                                                                                c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                                  "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                                  "Risque d'échec associé à la mesure compensatoire : les résultats de l'action de compensation sont-ils bien maîtrisés?" = "3"))),
                                                                                     column(2, align="left", numericInput("SCvalCTNH", "Valeur après compensation CT", value = 0.)),
                                                                                     column(2, align="left", textAreaInput("SCjustifLTNH", "Justification de prédiction", height='250px', placeholder = "justification de la prédiction long terme")),
                                                                                     column(2, align="left", checkboxGroupInput("SCdegincLTNH", "Incertitudes",
                                                                                                                                c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                                  "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                                  "Risque d'échec associé à la mesure compensatoire : les résultats de l'action de compensation sont-ils bien maîtrisés?" = "3"))),
                                                                                     column(2, align="left", numericInput("SCvalLTNH", "Valeur après compensation LT", value = 0.))), br(),
                                                                            fluidRow(column(6, align="right", actionButton("renseignerNH2", "RENSEIGNER")),
                                                                                     column(6, align="left", tags$style("#ManuelNH2 {background-color:#FFA02F;}"), numericInput("ManuelNH2", NA, value = 0.))), br(),
                                                                            DT::dataTableOutput("SCtable5")
                                                                   ),
                                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Espèce</h4>'), value="compensindicnsp", br(),
                                                                            selectInput("selectspeciesSC", label = "SELECTIONNER L'ESPECE", choices = list("-" = 0), selected = 0), br(),
                                                                            fluidRow(column(6, align="center", HTML('<h4 style="color: #A5C226; ">COURT TERME</h4>')), column(6, align="center", HTML('<h4 style="color: #A5C226; ">LONG TERME</h4>'))), br(),
                                                                            fluidRow(column(2, align="left", textAreaInput("SCjustifCTNSP", "Justification de prédiction", height='250px', placeholder = "justification de la prédiction court terme")),
                                                                                     column(2, align="left", checkboxGroupInput("SCdegincCTNSP", "Incertitudes",
                                                                                                                                c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                                  "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                                  "Risque d'échec associé à la mesure compensatoire : les résultats de l'action de compensation sont-ils bien maîtrisés?" = "3"))),
                                                                                     column(2, align="left", numericInput("SCvalCTNSP", "Valeur après compensation CT", value = 0.)),
                                                                                     column(2, align="left", textAreaInput("SCjustifLTNSP", "Justification de prédiction", height='250px', placeholder = "justification de la prédiction long terme")),
                                                                                     column(2, align="left", checkboxGroupInput("SCdegincLTNSP", "Incertitudes",
                                                                                                                                c("Influence du projet : la prédiction de la valeur de l'indicateur dépend-elle essentiellement des actions réalisées par le maître d'ouvrage ? Y a-t-il une part qu'il ne peut pas maîtriser ?" = "1",
                                                                                                                                  "Comportement de l'espèce faune ou flore : le comportement (capacité de recolonisation) des espèces sur lesquelles porte l'indicateur est-il bien connu ?" = "2",
                                                                                                                                  "Risque d'échec associé à la mesure compensatoire : les résultats de l'action de compensation sont-ils bien maîtrisés?" = "3"))),
                                                                                     column(2, align="left", numericInput("SCvalLTNSP", "Valeur après compensation LT", value = 0.))), br(),
                                                                            fluidRow(column(6, align="right", actionButton("renseignerNSP2", "RENSEIGNER")),
                                                                                     column(6, align="left", tags$style("#ManuelNSP2 {background-color:#FFA02F;}"), numericInput("ManuelNSP2", NA, value = 0.))), br(),
                                                                            DT::dataTableOutput("SCtable6")
                                                                            
                                                                   )
                                                       )
                                      )
                              ),
                              # EQUIVALENCE ----------------------------------------------
                              tabPanel(value="calculs", HTML('<h4 style="color: #005BBB; "><b>Calculs</b></h4>'),
                                       tabsetPanel(id="descrcompens",
                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Pertes</h4>'), br(),
                                                            fluidRow(column(2, align="left", selectInput("selectsiteimpact2", label = "SELECTIONNER LE SITE IMPACT\u00C9", choices = list("-" = 0), selected = 0)),
                                                                     column(2, align="left", selectInput("selecttypegraphperte", label = "TYPE DE GRAPHE", choices = list("Etat initial" = 1, "Pertes CT" = 2, "Pertes LT" = 3), selected = 1)),
                                                                     column(2, align="left", selectInput("selectniveauperte", label = "NIVEAU", choices = list("Général" = 1, "Habitat" = 2, "Espèce" = 3), selected = 1)),
                                                                     column(2, align="left", selectInput("selecthabitatSI2", label = "SELECTIONNER L'HABITAT", choices = list("-" = 0), selected = 0)),
                                                                     column(2, align="left", selectInput("selectspeciesSI2", label = "SELECTIONNER L'ESPECE", choices = list("-" = 0), selected = 0))), hr(),
                                                                     plotlyOutput('plot_pertes')
                                                   ),
                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Gains</h4>'), br(),
                                                            selectInput("selectsitecompens2", label = "SELECTIONNER LE SITE COMPENSATOIRE", choices = list("-" = 0), selected = 0), hr()
                                                   ),
                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Equivalence</h4>'))
                                       )
                              ),
                              # FICHE DE SYNTHESE ----------------------------------------------
                              tabPanel(value="synth", HTML('<h4 style="color: #005BBB; "><b>Synthèse</b></h4>'), br(),
                                       DT::dataTableOutput('synthtab')
                              ),
                              # A PROPOS ----------------------------------------------
                              tabPanel(value="propos", HTML('<h4 style="color: #005BBB; "><b>A propos</b></h4>'))
                  )
        )
)
