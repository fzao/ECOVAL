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

model_info_general <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 1, header = FALSE, stringsAsFactors = FALSE)
model_site <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 2, header = FALSE, stringsAsFactors = FALSE)
model_A1 <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 3, header = TRUE, stringsAsFactors = FALSE)
model_species <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 4, header = FALSE, stringsAsFactors = FALSE)
model_habitat <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 5, header = FALSE, stringsAsFactors = FALSE)
ecoval <- list()
ecoval[["General"]] <- model_info_general
numsite <- 0
numspecies <- 0
numhabitat <- 0
listsite <- data.frame("site" = '-', "index" = 0, "name" = '-', "type" = 0, stringsAsFactors=FALSE)
listspecies <- data.frame("species" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
listhabitat <- data.frame("habitat" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
tableau <- reactiveValues(A1=NULL)
tableau$A1 <- model_A1
A1listtype <- list("1"="Fermé" , "2"="Ouvert", "3"="Buissonnant", "4"="Zone humide", "5"="Aquatique", "6"="Rocheux", "7"="Cultivé", "8"="Imperméabilisé")
A1listetat <- list("1"="Bon", "2"="Mauvais", "3"="Moyen")
A1listinter <- list("1"="Oui", "2"="Non")