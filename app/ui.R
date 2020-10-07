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
# # Copyright (c) EDF-INRAE 2019-2020
#
# Auteurs : Fabrice Zaoui - Lucie Bezombes
#
# Licence CeCILL v2.1
#

library(shiny)
library(shinyjs)
library(xlsx)
library(DT)
library(plotly)
library(leaflet)
library(rmarkdown)
library(knitr)
library(kableExtra)

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
                           HTML('<h4 style="color: #A5C226; background-color: #FFFFFF;"><b>Cadre méthodologique pour le calcul de l\'équivalence écologique dans le contexte de la séquence ERC en France</b></h5>'),
                           HTML('<h5 style="color: #FF0000; background-color: #FFFFFF;"><b>Version beta</b></h1>')),
                    column(2, align="right",
                           img(height=40, width=80, src="edf.jpg"), HTML('<h4> </h4>'),
                           img(height=30, width=114, src="Inrae.jpg"))
                  ),
                  tabsetPanel(id="tabs",
                              # ACCUEIL ---------------------------------------------------
                              tabPanel(value="accueil", HTML('<h4 style="color: #005BBB; "><b>Accueil</b></h4>'), br(),
                                       includeMarkdown("md/accueil.md"), br(),
                                       fluidRow(column(6, align="center", HTML('<h4 style="color: #005BBB; "><b>Tuto #1</b>   (version beta - juillet 2019)</h4>'), HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/sktibtPlT9o" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                                                column(6, align="center", HTML('<h4 style="color: #005BBB; "><b>Tuto #2</b>   (version beta - juillet 2019)</h4>'), HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/DmwFXCOCAsA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')))),
                              # PROJET ----------------------------------------------------
                              tabPanel(value="projet", HTML('<h4 style="color: #005BBB; "><b>Projet</b></h4>'), br(),
                                       sidebarLayout(
                                         sidebarPanel(
                                           titlePanel(HTML('<h5><b>FICHIER DU PROJET</b></h5>')),
                                           fluidRow(column(12, align="left", tags$div(title="Importe un fichier ECOVAL .xlsx", fileInput("userfile", NULL, multiple = FALSE, accept = ".xlsx", buttonLabel = 'Charger', placeholder = '...')))),
                                           fluidRow(column(4, align="left", tags$div(title="Télécharge le projet ECOVAL en cours",downloadButton("btn_telecharger", "Sauver"))), column(8, align="left", HTML('<a href="Correspondance.pdf" target="_blank"><img src="aide.png" alt="excel"></a>'))), br(),
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
                                             tabPanel(HTML('<h4 style="color: #005BBB; ">Description du site</h4>'), value="description", br(),
                                                      fluidRow(column(12, align="center", htmlOutput("viewsiteno", inline = TRUE))), br(), br(),
                                                      fluidRow(column(4, align="left", textInput("sitename", "NOM", placeholder = "Nom du site...")),
                                                               column(4, align="left", selectInput("sitetype", label = "TYPE", choices = list("Impacté" = 1, "Compensatoire" = 2, "Impacté et Compensatoire" = 3))),
                                                               column(4, align="left", numericInput("surface", "SURFACE (ha)", value = 0., min = 0., max = 1e6, step = 1.))),
                                                      fluidRow(column(12, align="left", textAreaInput("sitecontext", "CONTEXTE", height='170px', width='1250px', placeholder = "Décrire le contexte du site ici..."))),
                                                      fluidRow(column(4, align="left", numericInput("latitude", "LATITUDE (degrés décimaux)", 0.)),
                                                               column(4, align="left", numericInput("longitude", "LONGITUDE (degrés décimaux)", 0.))),
                                                      fluidRow(column(12, align = "left", textAreaInput("descqual", "DESCRIPTION QUALITATIVE", height='90px', width='1250px', placeholder = "Nature, emprise, effets indirects..."))),
                                                      fluidRow(column(12, align = "left",textAreaInput("tempo", "TEMPORALITE", height='90px', width='1250px', placeholder = "Plusieurs phases? Court/long terme..."))),
                                                      fluidRow(column(4, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "link1", label=HTML('<h5><b>DUREE</b></h5>')))),
                                                               column(4, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "link2", label=HTML('<h5><b>INTENSITE</b></h5>')))),
                                                               column(4, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "link3", label=HTML('<h5><b>PORTEE SPATIALE</b></h5>'))))
                                                               ),
                                                      fluidRow(column(4, align = "left", selectInput("duree", label = NULL, choices = list("-" = 0, "Temporaire Courte Durée" = 1, "Temporaire Longue Durée" = 2, "Permanent" = 3))),
                                                               column(4, align = "left", selectInput("intensite", label = NULL, choices = list("-" = 0, "Peu Intense" = 1, "Intense" = 2, "Très Intense" = 3))),
                                                               column(4, align = "left", selectInput("portee", label = NULL, choices = list("-" = 0, "Ponctuelle Faible Surface" = 1, "Ponctuelle Surface Importante" = 2, "Linéaire" = 3))))
                                             ),
                                             tabPanel(HTML('<h4 style="color: #005BBB; ">Identification des enjeux</h4>'), value="identification", br(),
                                                      fluidRow(column(12, align="center", htmlOutput("enjeusiteno", inline = TRUE))), br(),
                                                      fluidRow(column(12, align="center", HTML('<h4 style="color: #005BBB; ">Les espèces et habitats considérés "à enjeux" dans ECOVAL sont ceux qui seront évalués plus précisément au Niveau Espèce ou Niveau Habitat car ils représentent un intérêt réglementaire, patrimonial ou fonctionnel, et pour lesquels un focus est nécessaire. Généralement, le nombre d\'espèces ou d\'habitats à enjeu va de 0 à 6 (voire une dizaine au maximum), n\'hésitez pas à regrouper les espèces à enjeu en communauté de mêmes besoins écologiques.</h4>'))), br(),
                                                      fluidRow(column(6, align="center", HTML('<h4 style="color: #878F99; "><b>HABITAT(S) \u00C0 ENJEUX</b></h4>')), column(6, align="center", HTML('<h4 style="color: #878F99; "><b>ESP\u00C8CE(S) \u00C0 ENJEUX</b></h4>'))), br(),
                                                      fluidPage(
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
                                                               fluidRow(column(4, align="left", textAreaInput("justifyhabitat", label = "Justification de l'enjeu", height='200px')),
                                                                        column(4, align="left", selectInput("presencehabitat", label = "Présence sur site impacté", choices = list("Oui" = 1, "Non" = 2))),
                                                                        column(4, align="left", numericInput("perimelarghab", "Périmètre élargi (km)", value = 0., min = 0., max = 1e6, step = 1.)))
                                                        ),
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
                                                               fluidRow(column(4, align="left", textAreaInput("justifyspecies", label = "Justification de l'enjeu", height='200px')),
                                                                        column(4, align="left", selectInput("presencespecies", label = "Présence sur site impacté", choices = list("Oui" = 1, "Non" = 2))),
                                                                        column(4, align="left", numericInput("perimelargsp", "Périmètre élargi (km)", value = 0., min = 0., max = 1e6, step = 1.)))
                                                               )
                                                      )
                                             )
                                           ),
                                           width = 9)
                                       )
                                       # leafletOutput("projectmap")
                                      ),
                              # SITE IMPACTE ----------------------------------------------
                              tabPanel(value="impact", HTML('<h4 style="color: #005BBB; "><b>Site impacté</b></h4>'), br(),
                                       fluidRow(column(6, align="left", selectInput("selectsiteimpact", label = "SITE", choices = list("-" = 0), selected = 0)), column(6, align="right", downloadButton("btn_telecharger2", "Sauver"))), hr(),
                                       conditionalPanel(condition = "input.selectsiteimpact != '0'",
                                         tabsetPanel(id="descrimpact",
                                                     tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Général</h4>'), value="impactindicng", br(),
                                                          tabsetPanel(id="descrNGSI",
                                                              tabPanel(HTML('<h4 style="color: #005BBB; ">Saisie des données brutes</h4>'), value="impactrawdata", br(),
                                                                       fluidRow(column(7, align="right", HTML('<h4 style="color: #A5C226; ">LISTE DES HABITATS - P\u00C9RIM\u00C8TRE SITE (PS)</h4>')), column(5, align="left", HTML('<a href="Aide_donnees_brutes.pdf" target="_blank"><img src="aide.png" alt="donnees brutes"></a>'))), br(),
                                                                       fluidRow(column(3, align="left", textInput("SInamehabitat", "(\u03A3) Nom habitat")),
                                                                                column(3, align="left", textInput("SIcodecorine", "Code Corine", placeholder = "ex. : 41.12")),
                                                                                column(3, align="left", textInput("SIcodeeunis", "Code Eunis", placeholder = "ex. : G1.21")),
                                                                                column(3, align="left", numericInput("SIsurface", "(\u03A3) Surface (ha)", value = 0., min = 0., step = 0.01))),
                                                                       fluidRow(column(3, align="left", selectInput("SItype", label = "(\u03A3) Type", choices = list("Fermé" = 1, "Ouvert" = 2, "Buissonnant" = 3, "Zone humide" = 4, "Aquatique" = 5, "Rocheux" = 6, "Cultivé" = 7, "Imperméabilisé" = 8))),
                                                                                column(3, align="left", selectInput("SIetat", label = "(\u03A3) Etat conservation", choices = list("-" = 0, "Bon" = 1, "Mauvais" = 2, "Moyen" = 3))),
                                                                                column(3, align="left", selectInput("SIinteret", label = "(\u03A3) Intérêt communautaire", choices = list("Non" = 1, "Oui" = 2))),
                                                                                column(3, align="left", selectInput("SImenace", label = "(\u03A3) En danger ou menacé localement", choices = list("Non" = 1, "Oui" = 2)))),
                                                                       fluidRow(column(3, align="left", numericInput("SIsurfacedeg", "Surface dégradée (ha)", value = 0., min = 0., step = 0.01))),
                                                                       fluidRow(column(4, align="right", actionButton("addlisthab", "AJOUTER")), column(4, align="center", actionButton("chglisthab", "MODIFIER")), column(4, align="left", actionButton("dellisthab", "SUPPRIMER"))), br(),
                                                                       DT::dataTableOutput("SItable1"), DT::dataTableOutput("SItable1rowselected"), br(), br(), br(), hr(), br(), br(), br(),
                                                                       fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">LISTE DES ESP\u00C8CES - P\u00C9RIM\u00C8TRE SITE (PS)</h4>'))), br(),
                                                                       fluidRow(column(2, align="left", textInput("SIlatinnamespecies", "(\u03A3) Nom latin")),
                                                                                column(2, align="left", textInput("SIfrenchnamespecies", "Nom français")),
                                                                                column(2, align="left", selectInput("SItype1", label = "(\u03A3) Type 1", choices = list("Avifaune" = 1, "Chiroptère" = 2, "Mammifère" = 3, "Amphibien" = 4, "Reptile" = 5, "Insecte" = 6, "Flore" = 7, "Poisson" = 8, "Crustacé/Mollusque" = 9))),
                                                                                column(2, align="left", selectInput("SItype2", label = "(\u03A3) Type 2", choices = list("-"=0, "Cortège généraliste"=1, "Cortège forestier"=2, "Cortège agricole ou ouvert"=3, "Cortège zone humide"=4, "Cortège du bâti"=5, "Odonate"=6,"Lépidoptère"=7,"Orthoptère"=8,"Coléoptère"=9))),
                                                                                column(2, align="left", selectInput("SIprotect", label = "(\u03A3) Protection nationale ou régionale", choices = list("Non" = 1, "Oui" = 2))),
                                                                                column(2, align="left", selectInput("SIrougeF", label = "(\u03A3) Liste rouge (CR,VU,EN) France", choices = list("Non" = 1, "Oui" = 2)))),
                                                                       fluidRow(column(2, align="left", selectInput("SIrougeR", label = "(\u03A3) Liste rouge (CR,VU,EN) Régional", choices = list("Non" = 1, "Oui" = 2))),
                                                                                column(2, align="left", selectInput("SIdirect", label = "Directives Européennes", choices = list("-" = 0, "Annexe II DFFH" = 1, "Annexe I DO" = 2))),
                                                                                column(2, align="left", selectInput("SIreprod", label = "(\u03A3) Reproduction", choices = list("-" = 0, "Certaine" = 1, "Possible" = 2))),
                                                                                column(2, align="left", selectInput("SIexo", label = "(\u03A3) Espèce Exotique Envahissante", choices = list("Non" = 1, "Oui" = 2))),
                                                                                column(2, align="left", selectInput("SItvb", label = HTML('(\u03A3) TVB <a href="TVB.pdf" target="_blank"><img src="aide_small.png" alt="TVB"></a>'), choices = list("Non" = 1, "Oui" = 2))),
                                                                                column(2, align="left", selectInput("SIdet", label = "(\u03A3) Déterminant Znieff dans le PE", choices = list("Non" = 1, "Oui" = 2)))),
                                                                       fluidRow(column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkinfoIS1", label=HTML('<h5><b>(\u03A3) Indice spécialisation</b></h5>'))))),
                                                                       fluidRow(column(2, align="left", selectInput("SIindssi", label = NULL, choices = split(seq_along(Species), Species)))),
                                                                       fluidRow(column(4, align="right", actionButton("addlistesp", "AJOUTER")), column(4, align="center", actionButton("chglistesp", "MODIFIER")), column(4, align="left", actionButton("dellistesp", "SUPPRIMER"))), br(),
                                                                       DT::dataTableOutput("SItable2"), DT::dataTableOutput("SItable2rowselected"), br(), br(), br(), hr(), br(), br(), br(),
                                                                       fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">P\u00C9RIM\u00C8TRE \u00C9LARGI (PE)</h4>'))), br(),
                                                                       fluidRow(column(3, align="left", selectInput("SIpertype", label = "(\u03A3) Type", choices = list("Fermé" = 1, "Ouvert" = 2, "Buissonnant" = 3, "Zone Humide" = 4, "Aquatique" = 5, "Rocheux" = 6, "Cultivé" = 7, "Imperméabilisé" = 8))),
                                                                                column(3, align="left", textInput("SIpercouche", "Couche SIG EUNIS", placeholder = "ex. : 41 (forêt décidue)")),
                                                                                column(3, align="left", textInput("SIpercode", "Code SIG OSO", placeholder = "ex. : 34 (pelouse)")),
                                                                                column(3, align="left", numericInput("SIpersurf", "(\u03A3) Surface (ha)", value = 0., min = 0., step = 0.01))), br(),
                                                                       fluidRow(column(4, align="right", actionButton("addlistper", "AJOUTER")), column(4, align="center", actionButton("chglistper", "MODIFIER")), column(4, align="left", actionButton("dellistper", "SUPPRIMER"))), br(),
                                                                       DT::dataTableOutput("SItable3"), DT::dataTableOutput("SItable3rowselected")),
                                                              tabPanel(HTML('<h4 style="color: #005BBB; ">Tableau des indicateurs</h4>'), value="impactindic", br(),
                                                                  fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">ETAT INITIAL</h4>'))), br(),
                                                                  fluidRow(column(12, align="center", numericInput("Manuel", NULL, value = 0.))),
                                                                  fluidRow(column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkCT1", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION COURT TERME</h4>')))), column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkLT1", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION LONG TERME</h4>'))))), br(),
                                                                  fluidRow(column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJ1", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SIjustifCT", NULL, height='125px', placeholder = "justification de l'estimation faite"))),
                                                                           column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkI1", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SIdegincCT", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                           column(2, align="left", numericInput("SIvalCT", "Valeur après impact", value = 0.)),
                                                                           column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJ2", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SIjustifLT", NULL, height='125px', placeholder = "justification de l'estimation faite"))),
                                                                           column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible",actionLink(inputId = "linkI2", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SIdegincLT", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                           column(2, align="left", numericInput("SIvalLT", "Valeur après impact", value = 0.))), br(),
                                                                  fluidRow(column(12, align="center", actionButton("renseigner", "RENSEIGNER"))),
                                                                  fluidRow(column(12, align="left", HTML('<a href="Lot_indicateurs_NG.pdf" target="_blank"><img src="aide.png" alt="TVB"></a>'))),
                                                                  DT::dataTableOutput("SItable4"), DT::dataTableOutput("SItable4rowselected"), br()))
                                                     ),
                                                     tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Habitat</h4>'), value="impactindicnh", br(),
                                                              selectInput("selecthabitatSI", label = "HABITAT", choices = list("-" = 0), selected = 0), br(),
                                                              fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">ETAT INITIAL</h4>'))), br(),
                                                              fluidRow(column(6, align="right", numericInput("ManuelNH", NULL, value = 0.)),
                                                                       column(6, align="left", textInput("justifySINH", NULL, placeholder = "détail"))), br(),
                                                              fluidRow(column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkCT2", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION COURT TERME</h4>')))), column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkLT2", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION LONG TERME</h4>'))))), br(),
                                                              fluidRow(column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJ3", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SIjustifCTNH", NULL, height='125px', placeholder = "justification de l'estimation court terme"))),
                                                                       column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkI3", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SIdegincCTNH", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                       column(2, align="left", numericInput("SIvalCTNH", "Valeur après impact CT", value = 0.)),
                                                                       column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJ4", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SIjustifLTNH", NULL, height='125px', placeholder = "justification de l'estimation long terme"))),
                                                                       column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkI4", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SIdegincLTNH", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                       column(2, align="left", numericInput("SIvalLTNH", "Valeur après impact LT", value = 0.))), br(),
                                                              fluidRow(column(12, align="center", actionButton("renseignerNH", "RENSEIGNER"))),
                                                              fluidRow(column(12, align="left", HTML('<a href="Lot_indicateurs_NH.pdf" target="_blank"><img src="aide.png" alt="TVB"></a>'))),
                                                              DT::dataTableOutput("SItable5"), DT::dataTableOutput("SItable5rowselected"), br()
                                                     ),
                                                     tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Espèce</h4>'), value="impactindicnsp", br(),
                                                              selectInput("selectspeciesSI", label = "ESPECE", choices = list("-" = 0), selected = 0), br(),
                                                              fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">ETAT INITIAL</h4>'))), br(),
                                                              fluidRow(column(6, align="right", numericInput("ManuelNSP", NULL, value = 0.)),
                                                                       column(6, align="left", textInput("justifySINSP", NULL, placeholder = "détail"))), br(),
                                                              fluidRow(column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkCT3", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION COURT TERME</h4>')))), column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkLT3", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION LONG TERME</h4>'))))), br(),
                                                              fluidRow(column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJ5", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SIjustifCTNSP", NULL, height='125px', placeholder = "justification de l'estimation court terme"))),
                                                                       column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkI5", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SIdegincCTNSP", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                       column(2, align="left", numericInput("SIvalCTNSP", "Valeur après impact CT", value = 0.)),
                                                                       column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJ6", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SIjustifLTNSP", NULL, height='125px', placeholder = "justification de l'estimation long terme"))),
                                                                       column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkI6", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SIdegincLTNSP", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                       column(2, align="left", numericInput("SIvalLTNSP", "Valeur après impact LT", value = 0.))), br(),
                                                              fluidRow(column(12, align="center", actionButton("renseignerNSP", "RENSEIGNER"))),
                                                              fluidRow(column(12, align="left", HTML('<a href="Lot_indicateurs_NSp.pdf" target="_blank"><img src="aide.png" alt="TVB"></a>'))),
                                                              DT::dataTableOutput("SItable6"), DT::dataTableOutput("SItable6rowselected"), br()

                                                     )
                                         )
                                       )
                              ),
                              # SITE COMPENSATOIRE ----------------------------------------------
                              tabPanel(value="compens", HTML('<h4 style="color: #005BBB; "><b>Site compensatoire</b></h4>'), br(),
                                      fluidRow(column(6, align="left", selectInput("selectsitecompens", label = "SITE", choices = list("-" = 0), selected = 0)), column(6, align="right", downloadButton("btn_telecharger3", "Sauver"))), hr(),
                                      conditionalPanel(condition = "input.selectsitecompens != '0'",
                                                       tabsetPanel(id="descrcompens",
                                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Général</h4>'), value="compensindicng", br(),
                                                                        tabsetPanel(id="descrNGSC",
                                                                            tabPanel(HTML('<h4 style="color: #005BBB; ">Saisie des données brutes</h4>'), value="compensrawdata", br(),
                                                                                     fluidRow(column(7, align="right", HTML('<h4 style="color: #A5C226; ">LISTE DES HABITATS - P\u00C9RIM\u00C8TRE SITE (PS)</h4>')), column(5, align="left", HTML('<a href="Aide_donnees_brutes.pdf" target="_blank"><img src="aide.png" alt="donnees brutes"></a>'))), br(),
                                                                                     fluidRow(column(3, align="left", textInput("SCnamehabitat", "(\u03A3) Nom habitat")),
                                                                                              column(3, align="left", textInput("SCcodecorine", "Code Corine", placeholder = "ex. : 41.12")),
                                                                                              column(3, align="left", textInput("SCcodeeunis", "Code Eunis", placeholder = "ex. : G1.21")),
                                                                                              column(3, align="left", numericInput("SCsurface", "(\u03A3) Surface (ha)", value = 0., min = 0., step = 0.01))),
                                                                                     fluidRow(column(3, align="left", selectInput("SCtype", label = "(\u03A3) Type", choices = list("Fermé" = 1, "Ouvert" = 2, "Buissonnant" = 3, "Zone humide" = 4, "Aquatique" = 5, "Rocheux" = 6, "Cultivé" = 7, "Imperméabilisé" = 8))),
                                                                                              column(3, align="left", selectInput("SCetat", label = "(\u03A3) Etat conservation", choices = list("-" = 0, "Bon" = 1, "Mauvais" = 2, "Moyen" = 3))),
                                                                                              column(3, align="left", selectInput("SCinteret", label = "(\u03A3) Intérêt communautaire", choices = list("Non" = 1, "Oui" = 2))),
                                                                                              column(3, align="left", selectInput("SCmenace", label = "(\u03A3) En danger ou menacé localement", choices = list("Non" = 1, "Oui" = 2)))),
                                                                                     fluidRow(column(3, align="left", numericInput("SCsurfacedeg", "Surface dégradée (ha)", value = 0., min = 0., step = 0.01))),
                                                                                     fluidRow(column(4, align="right", actionButton("addlisthab2", "AJOUTER")), column(4, align="center", actionButton("chglisthab2", "MODIFIER")), column(4, align="left", actionButton("dellisthab2", "SUPPRIMER"))), br(),
                                                                                     DT::dataTableOutput("SCtable1"), DT::dataTableOutput("SCtable1rowselected"), br(), br(), br(), hr(), br(), br(), br(),
                                                                                     fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">LISTE DES ESP\u00C8CES - P\u00C9RIM\u00C8TRE SITE (PS)</h4>'))), br(),
                                                                                     fluidRow(column(2, align="left", textInput("SClatinnamespecies", "(\u03A3) Nom latin")),
                                                                                              column(2, align="left", textInput("SCfrenchnamespecies", "Nom français")),
                                                                                              column(2, align="left", selectInput("SCtype1", label = "(\u03A3) Type 1", choices = list("Avifaune" = 1, "Chiroptère" = 2, "Mammifère" = 3, "Amphibien" = 4, "Reptile" = 5, "Insecte" = 6, "Flore" = 7, "Poisson" = 8, "Crustacé/Mollusque" = 9))),
                                                                                              column(2, align="left", selectInput("SCtype2", label = "(\u03A3) Type 2", choices = list("-"=0, "Cortège généraliste"=1, "Cortège forestier"=2, "Cortège agricole ou ouvert"=3, "Cortège zone humide"=4, "Cortège du bâti"=5, "Odonate"=6,"Lépidoptère"=7,"Orthoptère"=8,"Coléoptère"=9))),
                                                                                              column(2, align="left", selectInput("SCprotect", label = "(\u03A3) Protection nationale ou régionale", choices = list("Non" = 1, "Oui" = 2))),
                                                                                              column(2, align="left", selectInput("SCrougeF", label = "(\u03A3) Liste rouge (CR,VU,EN) France", choices = list("Non" = 1, "Oui" = 2)))),
                                                                                     fluidRow(column(2, align="left", selectInput("SCrougeR", label = "(\u03A3) Liste rouge (CR,VU,EN) Régional", choices = list("Non" = 1, "Oui" = 2))),
                                                                                              column(2, align="left", selectInput("SCdirect", label = "Directives Européennes", choices = list("-" = 0, "Annexe II DFFH" = 1, "Annexe I DO" = 2))),
                                                                                              column(2, align="left", selectInput("SCreprod", label = "(\u03A3) Reproduction", choices = list("-" = 0, "Certaine" = 1, "Possible" = 2))),
                                                                                              column(2, align="left", selectInput("SCexo", label = "(\u03A3) Espèce Exotique Envahissante", choices = list("Non" = 1, "Oui" = 2))),
                                                                                              column(2, align="left", selectInput("SCtvb", label = HTML('(\u03A3) TVB <a href="TVB.pdf" target="_blank"><img src="aide_small.png" alt="TVB"></a>'), choices = list("Non" = 1, "Oui" = 2))),
                                                                                              column(2, align="left", selectInput("SCdet", label = "(\u03A3) Déterminant Znieff dans le PE", choices = list("Non" = 1, "Oui" = 2)))),
                                                                                     fluidRow(column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkinfoIS2", label=HTML('<h5><b>(\u03A3) Indice spécialisation</b></h5>'))))),
                                                                                     fluidRow(column(2, align="left", selectInput("SCindssi", label = NULL, choices = split(seq_along(Species), Species)))),
                                                                                     fluidRow(column(4, align="right", actionButton("addlistesp2", "AJOUTER")), column(4, align="center", actionButton("chglistesp2", "MODIFIER")), column(4, align="left", actionButton("dellistesp2", "SUPPRIMER"))), br(),
                                                                                     DT::dataTableOutput("SCtable2"), DT::dataTableOutput("SCtable2rowselected"), br(), br(), br(), hr(), br(), br(), br(),
                                                                                     fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">P\u00C9RIM\u00C8TRE \u00C9LARGI (PE)</h4>'))), br(),
                                                                                     fluidRow(column(3, align="left", selectInput("SCpertype", label = "(\u03A3) Type", choices = list("Fermé" = 1, "Ouvert" = 2, "Buissonnant" = 3, "Zone Humide" = 4, "Aquatique" = 5, "Rocheux" = 6, "Cultivé" = 7, "Imperméabilisé" = 8))),
                                                                                              column(3, align="left", textInput("SCpercouche", "Couche SIG EUNIS", placeholder = "ex. : 41 (forêt décidue)")),
                                                                                              column(3, align="left", textInput("SCpercode", "Code SIG OSO", placeholder = "ex. : 34 (pelouse)")),
                                                                                              column(3, align="left", numericInput("SCpersurf", "(\u03A3) Surface (ha)", value = 0., min = 0., step = 0.01))), br(),
                                                                                     fluidRow(column(4, align="right", actionButton("addlistper2", "AJOUTER")), column(4, align="center", actionButton("chglistper2", "MODIFIER")), column(4, align="left", actionButton("dellistper2", "SUPPRIMER"))), br(),
                                                                                     DT::dataTableOutput("SCtable3"), DT::dataTableOutput("SCtable3rowselected")),
                                                                            tabPanel(HTML('<h4 style="color: #005BBB; ">Tableau des indicateurs</h4>'), value="compensindic", br(),
                                                                                fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">ETAT INITIAL</h4>'))), br(),
                                                                                fluidRow(column(12, align="center", numericInput("Manuel2", NULL, value = 0.))),
                                                                                fluidRow(column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkCT4", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION COURT TERME</h4>')))), column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkLT4", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION LONG TERME</h4>'))))), br(),
                                                                                fluidRow(column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJC1", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SCjustifCT", NULL, height='125px', placeholder = "justification de l'estimation faite"))),
                                                                                         column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkC1", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SCdegincCT", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                                         column(2, align="left", numericInput("SCvalCT", "Valeur après compensation", value = 0.)),
                                                                                         column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJC2", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SCjustifLT", NULL, height='125px', placeholder = "justification de l'estimation faite"))),
                                                                                         column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkC2", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SCdegincLT", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                                         column(2, align="left", numericInput("SCvalLT", "Valeur après compensation", value = 0.))), br(),
                                                                                fluidRow(column(12, align="center", actionButton("renseigner2", "RENSEIGNER"))),
                                                                                fluidRow(column(12, align="left", HTML('<a href="Lot_indicateurs_NG.pdf" target="_blank"><img src="aide.png" alt="TVB"></a>'))),
                                                                                DT::dataTableOutput("SCtable4"), DT::dataTableOutput("SCtable4rowselected"), br()))
                                                                   ),
                                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Habitat</h4>'), value="compensindicnh", br(),
                                                                            selectInput("selecthabitatSC", label = "HABITAT", choices = list("-" = 0), selected = 0), br(),
                                                                            fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">ETAT INITIAL</h4>'))), br(),
                                                                            fluidRow(column(6, align="right", numericInput("ManuelNH2", NULL, value = 0.)),
                                                                                     column(6, align="left", textInput("justifySCNH", NULL, placeholder = "détail"))), br(),
                                                                            fluidRow(column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkCT5", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION COURT TERME</h4>')))), column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkLT5", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION LONG TERME</h4>'))))), br(),
                                                                            fluidRow(column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJC3", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SCjustifCTNH", NULL, height='125px', placeholder = "justification de l'estimation court terme"))),
                                                                                     column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkC3", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SCdegincCTNH", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                                     column(2, align="left", numericInput("SCvalCTNH", "Valeur après compensation CT", value = 0.)),
                                                                                     column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJC4", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SCjustifLTNH", NULL, height='125px', placeholder = "justification de l'estimation long terme"))),
                                                                                     column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkC4", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SCdegincLTNH", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                                     column(2, align="left", numericInput("SCvalLTNH", "Valeur après compensation LT", value = 0.))), br(),
                                                                            fluidRow(column(12, align="center", actionButton("renseignerNH2", "RENSEIGNER"))),
                                                                            fluidRow(column(12, align="left", HTML('<a href="Lot_indicateurs_NH.pdf" target="_blank"><img src="aide.png" alt="TVB"></a>'))),
                                                                            DT::dataTableOutput("SCtable5"), DT::dataTableOutput("SCtable5rowselected"), br()
                                                                   ),
                                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Niveau Espèce</h4>'), value="compensindicnsp", br(),
                                                                            selectInput("selectspeciesSC", label = "ESPECE", choices = list("-" = 0), selected = 0), br(),
                                                                            fluidRow(column(12, align="center", HTML('<h4 style="color: #A5C226; ">ETAT INITIAL</h4>'))), br(),
                                                                            fluidRow(column(6, align="right", numericInput("ManuelNSP2", NULL, value = 0.)),
                                                                                     column(6, align="left", textInput("justifySCNSP", NULL, placeholder = "détail"))), br(),
                                                                            fluidRow(column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkCT6", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION COURT TERME</h4>')))), column(6, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkLT6", label=HTML('<h4 style="color: #A5C226; ">ESTIMATION LONG TERME</h4>'))))), br(),
                                                                            fluidRow(column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJC5", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SCjustifCTNSP", NULL, height='125px', placeholder = "justification de l'estimation court terme"))),
                                                                                     column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkC5", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SCdegincCTNSP", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                                     column(2, align="left", numericInput("SCvalCTNSP", "Valeur après compensation CT", value = 0.)),
                                                                                     column(2, align="left", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkJC6", label=HTML('<h5><b>Justification de l\'estimation</b></h5>')), textAreaInput("SCjustifLTNSP", NULL, height='125px', placeholder = "justification de l'estimation long terme"))),
                                                                                     column(2, align="center", tags$div(style="display:inline-block",title="Aide disponible", actionLink(inputId = "linkC6", label=HTML('<h5><b>Sources d\'incertitude</b></h5>'))), checkboxGroupInput("SCdegincLTNSP", NULL, c("A", "B", "C"), inline = TRUE)),
                                                                                     column(2, align="left", numericInput("SCvalLTNSP", "Valeur après compensation LT", value = 0.))), br(),
                                                                            fluidRow(column(12, align="center", actionButton("renseignerNSP2", "RENSEIGNER"))),
                                                                            fluidRow(column(12, align="left", HTML('<a href="Lot_indicateurs_NSp.pdf" target="_blank"><img src="aide.png" alt="TVB"></a>'))),
                                                                            DT::dataTableOutput("SCtable6"), DT::dataTableOutput("SCtable6rowselected"), br()

                                                                   )
                                                       )
                                      )
                              ),
                              # EQUIVALENCE ----------------------------------------------
                              tabPanel(value="calculs", HTML('<h4 style="color: #005BBB; "><b>Calculs</b></h4>'),
                                       tabsetPanel(id="descrcalculs",
                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Site impacté</h4>'), br(),
                                                            fluidRow(column(2, align="left", selectInput("selectsiteimpact2", label = "SITE", choices = list("-" = 0), selected = 0)),
                                                                     column(2, align="left", selectInput("selecttypegraphperte", label = "TYPE DE CALCUL", choices = list("Etat initial" = 1, "Pertes CT" = 2, "Pertes LT" = 3), selected = 1)),
                                                                     column(2, align="left", selectInput("selectniveauperte", label = "NIVEAU", choices = list("Général" = 1, "Habitat" = 2, "Espèce" = 3), selected = 1)),
                                                                     column(2, align="left", selectInput("selecthabitatSI2", label = "HABITAT", choices = list("-" = 0), selected = 0)),
                                                                     column(2, align="left", selectInput("selectspeciesSI2", label = "ESPECE", choices = list("-" = 0), selected = 0))),
                                                            tabsetPanel(id="resultpertes",
                                                                       tabPanel(value="graphe"," Graphe", fluidRow(column(12, align="center", br(), uiOutput('plot_pertes_ui'), br(),br(),br(),br(),br()))),
                                                                       tabPanel(value="tableau", "Tableau", fluidRow(column(8, align="left", br(), DT::dataTableOutput("SIcalcul")), column(4, align="left", br(), downloadButton("dwnlpertes", "TELECHARGER LA TABLE"))))
                                                            )
                                                   ),
                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Site compensatoire</h4>'), br(),
                                                            fluidRow(column(2, align="left", selectInput("selectsitecompens2", label = "SITE", choices = list("-" = 0), selected = 0)),
                                                                     column(2, align="left", selectInput("selecttypegraphgain", label = "TYPE DE CALCUL", choices = list("Etat initial" = 1, "Gains CT" = 2, "Gains LT" = 3), selected = 1)),
                                                                     column(2, align="left", selectInput("selectniveaugain", label = "NIVEAU", choices = list("Général" = 1, "Habitat" = 2, "Espèce" = 3), selected = 1)),
                                                                     column(2, align="left", selectInput("selecthabitatSC2", label = "HABITAT", choices = list("-" = 0), selected = 0)),
                                                                     column(2, align="left", selectInput("selectspeciesSC2", label = "ESPECE", choices = list("-" = 0), selected = 0))),
                                                            tabsetPanel(id="resultgains",
                                                                        tabPanel(value="graphe", "Graphe", fluidRow(column(12, align="center", br(), uiOutput('plot_gains_ui'),br(),br(),br(),br(),br()))),
                                                                        tabPanel(value="tableau", "Tableau", fluidRow(column(8, align="left", br(), DT::dataTableOutput("SCcalcul")), column(4, align="left", br(), downloadButton("dwnlgains", "TELECHARGER LA TABLE"))))
                                                            )
                                                   ),
                                                   tabPanel(HTML('<h4 style="color: #005BBB; ">Equivalence</h4>'), br(),
                                                            fluidRow(column(2, align="left", selectInput("selectsiteimpact3", label = "SITE IMPACT\u00C9", choices = list("-" = 0), selected = 0)),
                                                                     column(2, align="left", selectInput("selectsitecompens3", label = "SITE COMPENSATOIRE", choices = list("-" = 0), selected = 0)),
                                                                     column(2, align="left", selectInput("selecttypegraphequivalence", label = "TYPE DE GRAPHE", choices = list("Equivalence CT" = 1, "Equivalence LT" = 2), selected = 0)),
                                                                     column(2, align="left", selectInput("selectniveauequivalence", label = "NIVEAU", choices = list("Général" = 1, "Habitat" = 2, "Espèce" = 3), selected = 1)),
                                                                     column(2, align="left", selectInput("selecthabitatSE", label = "HABITAT", choices = list("-" = 0), selected = 0)),
                                                                     column(2, align="left", selectInput("selectspeciesSE", label = "ESPECE", choices = list("-" = 0), selected = 0))),
                                                            tabsetPanel(id="resultequivalence",
                                                                        tabPanel(value="graphe", "Graphe", fluidRow(column(12, align="center", br(), uiOutput('plot_equivalence_ui'),br(),br(),br(),br(),br()))),
                                                                        tabPanel(value="tableau", "Tableau", fluidRow(column(8, align="left", br(), DT::dataTableOutput("SEcalcul")), column(4, align="left", br(), downloadButton("dwnlequivalence", "TELECHARGER LA TABLE"))))
                                                            )
                                                   )
                                       )
                              ),
                              # FICHE DE SYNTHESE ----------------------------------------------
                              tabPanel(value="synth", HTML('<h4 style="color: #005BBB; "><b>Synthèse</b></h4>'), br(),
                                       fluidRow(column(2, selectInput("selectsiteimpact4", label = "SITE IMPACT\u00C9", choices = list("-" = 0), selected = 0)),
                                                column(2, align="left", selectInput("selectsitecompens4", label = "SITE COMPENSATOIRE", choices = list("-" = 0), selected = 0))), hr(),
                                       fluidRow(column(2, align="left", downloadButton("genere", "Générer un rapport")))
                              ),
                              # A PROPOS ----------------------------------------------
                              tabPanel(value="propos", HTML('<h4 style="color: #005BBB; "><b>A propos</b></h4>'),
                                       tabsetPanel(
                                         tabPanel(HTML('<h4 style="color: #005BBB; ">Ecoval</h4>'), br(),
                                                  includeMarkdown("md/a_propos_0.md"),
                                                  actionLink(inputId = "redir2", label=HTML('<h4 style="color: #005BBB;" >1. Evaluation de l\'état initial du site impacté</h4>')),
                                                  actionLink(inputId = "redir3", label=HTML('<h4 style="color: #005BBB;" >2. Prédiction de la valeur des indicateurs sur le site impacté après impacts (à court et long terme) et calcul du delta entre état initial et après impact («pertes»)</h4>')),
                                                  actionLink(inputId = "redir4", label=HTML('<h4 style="color: #005BBB;" >3. Evaluation de l’état initial du site compensatoire</h4>')),
                                                  actionLink(inputId = "redir5", label=HTML('<h4 style="color: #005BBB;" >4. Prédiction de la valeur des indicateurs sur le site compensatoire après compensation (à court et long terme) et calcul du delta entre état initial et après compensation («gains»)</h4>')),
                                                  actionLink(inputId = "redir6", label=HTML('<h4 style="color: #005BBB;" >5. Calcul de l’équilibre entre pertes et gains pour chaque indicateur et évaluation de l’équivalence écologique globale</h4>')), br(),
                                                  includeMarkdown("md/a_propos_1.md")
                                                  # actionLink(inputId = "redir7", label=HTML('<h4 style="color: #005BBB;" >Pour la phase d’interprétation des résultats, veuillez vous référer à la notice explicative disponible dans l’onglet «à propos»</h4>'))),
                                          ),
                                         tabPanel(HTML('<h4 style="color: #005BBB; ">Logiciel</h4>'), br(),
                                                  includeMarkdown("md/a_propos_2.md")
                                          )
                                      )
                              )
                  )
        )
)
