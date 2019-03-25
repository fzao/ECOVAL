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
#library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(title="ECOVAL",
                  useShinyjs(),
                  fluidRow(
                    column(10, align="left",
                           HTML('<h1 style="color: #FFA02F; background-color: #FFFFFF;">ECOVAL</h1>'),
                           HTML('<h4 style="color: #A5C226; background-color: #FFFFFF;">Cadre méthodologique pour le calcul de l\'équivalence écologique dans le contexte de la séquence ERC en France</h5>')),
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
                                           titlePanel(HTML('<h5><b>FICHIERS</b></h5>')),
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
                                                    column(4, align="center", tags$div(title="Supprime définitivement le site en cours", actionButton("destroy", "ENLEVER")))),
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
                                                      fluidRow(column(4, align = "left", selectInput("duree", label = "DUREE", choices = list("Temporaire Courte Durée" = 1, "Temporaire Longue Durée" = 2, "Permanent" = 3))),
                                                                  #actionLink(inputId = "link1", label=h5("?")), br(), hr(),
                                                               column(4, align = "left", selectInput("intensite", label = "INTENSITE", choices = list("Peu Intense" = 1, "Intense" = 2, "Très Intense" = 3))),
                                                                  #actionLink(inputId = "link2", label=h5("?")), br(), hr(),
                                                               column(4, align = "left",    selectInput("portee", label = "PORTEE SPATIALE", choices = list("Ponctuelle Faible Surface" = 1, "Ponctuelle Surface Importante" = 2, "Linéaire" = 3))))
                                                                  #actionLink(inputId = "link3", label=h5("?"))
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
                              tabPanel(value="impact", HTML('<h4 style="color: #005BBB; "><b>Site impacté</b></h4>')),
                              # SITE COMPENSATOIRE ----------------------------------------------
                              tabPanel(value="compens", HTML('<h4 style="color: #005BBB; "><b>Site compensatoire</b></h4>')),
                              # EQUIVALENCE ----------------------------------------------
                              tabPanel(value="equival", HTML('<h4 style="color: #005BBB; "><b>Equivalence</b></h4>')),
                              # FICHE DE SYNTHESE ----------------------------------------------
                              tabPanel(value="synth", HTML('<h4 style="color: #005BBB; "><b>Synthèse</b></h4>'), br(),
                                       DT::dataTableOutput('synthtab')
                              ),
                              # A PROPOS ----------------------------------------------
                              tabPanel(value="propos", HTML('<h4 style="color: #005BBB; "><b>A propos</b></h4>'))
                  )
        )
)
