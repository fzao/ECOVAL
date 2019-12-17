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
model_species <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 3, header = FALSE, stringsAsFactors = FALSE)
model_habitat <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 4, header = FALSE, stringsAsFactors = FALSE)
model_A1 <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 5, header = TRUE, stringsAsFactors = FALSE)
model_A2 <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 6, header = TRUE, stringsAsFactors = FALSE)
model_A3 <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 7, header = TRUE, stringsAsFactors = FALSE)
model_B <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 8, header = TRUE, stringsAsFactors = FALSE)
model_C <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 9, header = TRUE, stringsAsFactors = FALSE)
model_D <- read.xlsx2('model/ECOVAL.xlsx', sheetIndex = 10, header = TRUE, stringsAsFactors = FALSE)
SSI <- readRDS('data/Species_SSI.rds')
Species <- readRDS("model/Species_names.rds")
park <- readRDS('data/Park.rds')
ecoval <- list()
ecoval[["General"]] <- model_info_general
numsite <- 0
numspecies <- 0
numhabitat <- 0
listsite <- data.frame("site" = '-', "index" = 0, "name" = '-', "type" = 0, stringsAsFactors=FALSE)
listspecies <- data.frame("species" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
listhabitat <- data.frame("habitat" = '-', "index" = 0, "name" = '-', stringsAsFactors=FALSE)
tableau <- reactiveValues(A1=NULL, A2=NULL, A3=NULL, B=NULL, C=NULL, D=NULL)
tableau$A1 <- model_A1
A1listtype <- list("1"="Fermé" , "2"="Ouvert", "3"="Buissonnant", "4"="Zone humide", "5"="Aquatique", "6"="Rocheux", "7"="Cultivé", "8"="Imperméabilisé")
A1listetat <- list("0"="", "1"="Bon", "2"="Mauvais", "3"="Moyen")
A1listinter <- list("1"="Non", "2"="Oui")
tableau$A2 <- model_A2
A2listtype1 <- list("1"="Avifaune","2"="Chiroptère","3"="Mammifère","4"="Amphibien","5"="Reptile","6"="Insecte","7"="Flore","8"="Poisson","9"="Crustacé/Mollusque")
A2listtype2 <- list("0"="-", "1"="Cortège généraliste", "2"="Cortège forestier", "3"="Cortège agricole ou ouvert", "4"="Cortège zone humide", "5"="Cortège du bâti", "6"="Odonate", "7"="Lépidoptère", "8"="Orthoptère", "9"="Coléoptère")
type2selection <- "0"
A2listprot <- list("1"="Non", "2"="Oui")
A2listdir <- list("0"="-","1"="Annexe II DFFH","2"="Annexe I DO")
A2listrepro <- list("0"="-","1"="Certaine","2"="Possible")
tableau$A3 <- model_A3
A3listtype <- list("1"="Fermé", "2"="Ouvert", "3"="Buissonnant", "4"="Zone humide", "5"="Aquatique", "6"="Rocheux", "7"="Cultivé", "8"="Imperméabilisé")
tableau$B <- model_B
tableau$C <- model_C
tableau$D <- model_D
pertes <- reactiveValues(tableau=NULL)
gains <- reactiveValues(tableau=NULL)
equivalence <- reactiveValues(tableau=NULL)
duree <- list("1"="Temporaire Courte Durée", "2"="Temporaire Longue Durée", "3"="Permanent")
intensite <- list("1"="Peu Intense", "2"="Intense", "3"="Très Intense")
portee <- list("1"="Ponctuelle Faible Surface", "2"="Ponctuelle Surface Importante", "3"="Linéaire")
