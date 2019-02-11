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
# library(xlsx)

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
                              tabPanel(value="accueil", HTML('<h4 style="color: #005BBB; "><b>Accueil</b></h4>')),
                              # PROJET ----------------------------------------------------
                              tabPanel(value="projet", HTML('<h4 style="color: #005BBB; "><b>Projet</b></h4>'),
                                # Sidebar layout with input and output definitions ----
                                sidebarLayout(
                                # Sidebar panel for inputs
                                  sidebarPanel(
                                    # Input: Select a file ----
                                    fileInput("file1", "Choose CSV File",
                                              multiple = FALSE,
                                              accept = c(".csv")),
                                      # Horizontal line
                                      tags$hr()
                                      # Main panel for displaying outputs
                                  ),
                                  mainPanel(
                                        # Output: Data file
                                        tableOutput("contents")
                                  )
                                )
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
