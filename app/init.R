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
Species <- readRDS("model/Species_names.rds")
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
A1listtype <- list("1"="Forestier" , "2"="Ouvert", "3"="Buissonnant", "4"="Zone humide", "5"="Aquatique", "6"="Rocheux", "7"="Cultivé", "8"="Imperméabilisé")
A1listetat <- list("0"="", "1"="Bon", "2"="Mauvais", "3"="Moyen")
A1listinter <- list("1"="Non", "2"="Oui", "-" = 3)
tableau$A2 <- model_A2
A2listtype1 <- list("1"="Avifaune","3"="Mammifère","4"="Amphibien","5"="Reptile","6"="Insecte","7"="Flore","8"="Poisson","9"="Crustacé/Mollusque")
A2listtype2 <- list("0"="-", "1"="Cortège généraliste", "2"="Cortège forestier", "3"="Cortège agricole ou ouvert", "4"="Cortège zone humide", "5"="Cortège du bâti", "6"="Odonate", "7"="Lépidoptère", "8"="Orthoptère", "9"="Coléoptère", "10"="Chiroptère", "11"="Autre")
type2selection <- "0"
A2listprot <- list("1"="Non", "2"="Oui")
A2listdir <- list("0"="Non","1"="Annexe II DFFH","2"="Annexe I DO")
A2listrepro <- list("0"="Non","1"="Certaine","2"="Possible", "3"="-")
tableau$A3 <- model_A3
A3listtype <- list("1"="Forestier", "2"="Ouvert", "3"="Buissonnant", "4"="Zone humide", "5"="Aquatique", "6"="Rocheux", "7"="Cultivé", "8"="Imperméabilisé")
tableau$B <- model_B
tableau$C <- model_C
tableau$D <- model_D
pertes <- reactiveValues(tableau=NULL)
gains <- reactiveValues(tableau=NULL)
equivalence <- reactiveValues(tableau=NULL)
duree <- list("1"="Temporaire Courte Durée", "2"="Temporaire Longue Durée", "3"="Permanent")
intensite <- list("1"="Peu Intense", "2"="Intense", "3"="Très Intense")
portee <- list("1"="Ponctuelle Faible Surface", "2"="Ponctuelle Surface Importante", "3"="Linéaire")
# OS specific (web server = GNU-Linux)
if(Sys.info()['sysname'] == "Darwin"){
  SSI <- readRDS('data/Species_SSI.rds')
  park <- readRDS('data/Park.rds')
}else{
  SSI <- readRDS('/home/ecoval/data/Species_SSI.rds')
  park <- readRDS('/home/ecoval/data/Park.rds')
}
colistcol <- c('Nombre d\'habitat forestier',
               'Surface (ha) d\'habitat forestier',
               'Nombre d\'habitat ouvert',
               'Surface (ha) d\'habitat ouvert',
               'Nombre d\'habitat buissonnant',
               'Surface (ha) d\'habitat buissonnant',
               'Nombre d\'habitat rocheux',
               'Surface (ha) d\'habitat rocheux',
               'Nombre de zone humide',
               'Surface (ha) de zone humide',
               'Nombre d\'habitat aquatique',
               'Surface (ha) d\'habitat aquatique',
               'Diversité avifaune',
               'Diversité chiroptères',
               'Diversité reptiles',
               'Diversité amphibiens',
               'Diversité mammifères',
               'Diversité insectes',
               'Diversité lépidoptères',
               'Diversité odonates',
               'Diversité orthoptères',
               'Diversité coléoptères',
               'Diversité flore totale',
               'Proportion surfacique des habitat menacés/en danger localement',
               'Proportion surfacique des habitat d\'intérêt communautaires (et prioritaires)',
               'Proportion d\'espèces protégées faune au niveau national et regional',
               'Proportion d\'espèces protégées flore au niveau national et regional',
               'Proportion d\'espèces menacées faune au niveau national',
               'Proportion d\'espèces menacées flore au niveau national',
               'Proportion d\'espèces menacées faune au niveau regional',
               'Proportion d\'espèces menacées flore au niveau regional',
               'Proportion d\'espèces faune sur l\'annexe II de la DFFH',
               'Proportion d\'espèces flore sur l\'annexe II de la DFFH',
               'Proportion d\'avifaune sur la DO',
               'Proportion d\'oiseaux nichant sur le site',
               'Proportion d\'espèces (hors oiseaux) se reproduisant sur le site',
               'Indice de spécialisation de l\'avifaune',
               'Proportion surfacique des habitats en bon état de conservation',
               '% de milieux NON cultivées',
               '% de zones NON imperméabilisées',
               'Nombre d\'espèces d\'EEE',
               'Nombre d\'espèces de cohérence régional TVB dans PS',
               '% Habitat forestier PS /PE',
               '% Habitat ouvert PS /PE',
               '% Habitat buissonnant PS /PE',
               '% Habitat rocheux PS /PE',
               '% Habitat humide PS /PE',
               '% Habitat aquatique PS /PE',
               'Nb espèces faune déterminante des Znieffs du PE',
               'Nb espèces flore déterminante des Znieffs du PE',
               '% Milieux cultivés',
               '% Zones urbanisées')




