#
#   ______ _____ ______      __     _      
#  |  ____/ ____/ __ \ \    / /\   | |     
#  | |__ | |   | |  | \ \  / /  \  | |     
#  |  __|| |   | |  | |\ \/ / /\ \ | |     
#  | |___| |___| |__| | \  / ____ \| |____ 
#  |______\_____\____/   \/_/    \_\______|
#
# Cadre méthodologique pour le calcul de l\'équivalence écologique dans le contexte de la séquence ERC en France
#
# # Copyright (c) EDF-IRSTEA 2019
#
# Auteurs : Fabrice Zaoui - Lucie Bezombes
#
# Licence CeCILL v2.1
#

library(shiny)
library(plotly)
library(xlsx)
library(DT)
#library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(title="ECOVAL",
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
                                           downloadButton("btn_telecharger", "Exporter"), br(), br(),
                                           fileInput("userfile", NULL, multiple = FALSE, accept = ".xlsx", buttonLabel = 'Importer...', placeholder = 'Fichier'),
                                           textInput("projectname", "NOM"), br(),
                                           textInput("projectcontext", "CONTEXTE", width = '100%'), br(),
                                           numericInput("sitenumber", "NOMBRE DE SITE(S) :", 1, min = 1, max = 100), br(),
                                           fluidRow(
                                             column(6,
                                              numericInput("latitude", "LAT. :", 0.)),
                                             column(6,
                                              numericInput("longitude", "LONG. :", 0.))),
                                           width = 3
                                         ),
                                          mainPanel(
                                           tabsetPanel(
                                             tabPanel(HTML('<h4 style="color: #808080; "><b>Description</b></h4>'), br(),
                                                      DT::dataTableOutput('projecttab')
                                             ),
                                             tabPanel(HTML('<h4 style="color: #808080; "><b>Enjeux</b></h4>'), br(),
                                                      tabsetPanel(
                                                        tabPanel(HTML('<h4 style="color: #808080; ">Habitats Site Impacté</h4>'), br(),
                                                            numericInput("nbhabimp", "Nombre :", 1, min = 1, max = 10), br(),
                                                            DT::dataTableOutput('enjeuxtabA')
                                                        ),
                                                        tabPanel(HTML('<h4 style="color: #808080; ">Espèces Site Impacté</h4>'), br(),
                                                                 numericInput("nbespimp", "Nombre :", 1, min = 1, max = 10), br(),
                                                                 DT::dataTableOutput('enjeuxtabB')
                                                        ),
                                                        tabPanel(HTML('<h4 style="color: #808080; ">Habitats Site Compensatoire</h4>'), br(),
                                                                 numericInput("nbhabcomp", "Nombre :", 1, min = 1, max = 10), br(),
                                                                 DT::dataTableOutput('enjeuxtabC')
                                                        ),
                                                        tabPanel(HTML('<h4 style="color: #808080; ">Espèces Site Compensatoire</h4>'), br(),
                                                               numericInput("nbespcomp", "Nombre :", 1, min = 1, max = 10), br(),
                                                               DT::dataTableOutput('enjeuxtabD')
                                                        )
                                                      )
                                             ),
                                             tabPanel(HTML('<h4 style="color: #808080; "><b>Valeurs</b></h4>'), br(),
                                                      DT::dataTableOutput('synthtab')
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
                              tabPanel(value="synth", HTML('<h4 style="color: #005BBB; "><b>Synthèse</b></h4>')),
                              # A PROPOS ----------------------------------------------
                              tabPanel(value="propos", HTML('<h4 style="color: #005BBB; "><b>A propos</b></h4>'))
                  )
        )
)
