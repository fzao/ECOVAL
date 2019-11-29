---
title: "ECOVAL"
author: "Lucie Bezombes & Fabrice Zaoui"
output: html_document
---

![Etapes](Image2.png)

* * *

## Comprendre chaque étape

<img style="float: right; max-width: 30%" src="Image3.png">

### **L'état initial (site impacté ou compensatoire)**

<font size="4">
Dans un premier temps, ECOVAL permet d’évaluer l’état initial des sites impactés
et compensatoires grâce au lot d’indicateurs disponible.
Vous aurez à renseigner de 2 à 4 étapes selon les cas :
   - **Niveau Général** (dans tous les cas) : permet de réaliser un diagnostic de la biodiversité présente sur le site évalué. La majorité des indicateurs est calculé automatiquement à partir de la liste des espèces et habitats importée (voir détails) ou saisie directement via l’interface. Quelques indicateurs doivent être renseignés manuellement.
   - **Identification des habitats et/ou espèces « à enjeu »**, c’est-à-dire faisant l’objet d’une règlementation spécifique, étant menacé ou ayant un rôle fonctionnel majeur (dans tous les cas).
   - **Niveau Habitat** (dans les cas où il y a un ou plusieurs habitat(s) à enjeu) : se focalise sur le(s) habitat(s) identifié(s). Les indicateurs doivent être renseignés manuellement pour chaque habitat à enjeu.
   - **Niveau Espèce** (dans les cas où il y a une ou plusieurs espèce(s) à enjeu) : se focalise sur le(s) espèce(s) identifiée(s). Les indicateurs doivent être renseignés manuellement pour chaque espèce à enjeu.

</font>

<font size="4">
Pour chaque niveau les indicateurs sont classés en deux échelles :
**le site** (PS), qui correspond soit au projet d’aménagement, soit aux mesures compensatoires ;
et **le périmètre élargi** (PE), qui permet d’évaluer la manière dont le site s’insère dans son contexte paysager.
Son rayon est défini au cas par cas grâce à différents paramètres (capacité de dispersion pour les espèces à enjeu par exemple).
</font>

### **L'estimation des valeurs des indicateurs** (après impact sur le site impacté et après compensation sur le site compensatoire)

<font size="4">
Dans un second temps, vous pouvez prédire la valeur des indicateurs renseignés
pour l’état initial, après impacts ou mesures compensatoires,
à court et long terme. Nous conseillons pour cette étape de réunir les experts impliqués
dans le projet afin que les prédictions soient les plus réalistes possibles.
L’interface vous permet de garder une trace de vos réflexions et également d’attribuer un degré d’incertitude à chaque prédiction.
</font>

### **Le calcul des «pertes»** (après impact sur le site impacté) **et des «gains»** (après compensation sur le site compensatoire)

<font size="4">
Les pertes et les gains sont calculés automatiquement en valeur «brutes»
(ex : gain de 3 ha de prairie) et «relatives» (ex : perte de 70% de la surface en prairie)
et sont visualisables via un graphique synthétique (voir plus bas).
Par abus de langage les deltas observés sur le site impacté sont nommés «pertes» et
ceux sur le site compensatoire «gains» mais il peut y avoir à la fois des diminutions (deltas négatifs)
et augmentations (deltas positifs) des valeurs des indicateurs pour les sites impactés et compensatoires.
</font>

<font size="4">
Exemple de graphe côté impact (pertes) <a href="Image4.png" target="_blank"><img src="Image4_small.png" alt="im4"></a> et côté compensation (gains) <a href="Image4bis.png" target="_blank"><img src="Image4bis_small.png" alt="im4bis"></a>

 **Attention**, les échelles varient d’un graphe à l’autre. Les pertes et gains calculés en relatif par rapport à l’état initial (barres rouges si le delta est négatif, et verte s’il est positif) et en valeur brutes selon les unités des indicateurs (hectare, nombre d’espèce…). La valeurs brutes sont les nombres qui aparaissent au milieu des barres. Le degré d’incertitude associé aux pertes ou gains est noté sous forme d’étoile au bout de chaque barre.
</font>

### **L’évaluation de l’équivalence** (entre pertes sur le site impacté et gains sur le site compensatoire)

<font size="4">
L’évaluation de l’équivalence écologique est réalisée en deux temps :
indicateur par indicateur (équilibre atteint lorsque l’effet des mesures compensatoires
permet tout juste de compenser l’effet des impacts) et de manière transversale
à tous les indicateurs (vérification que l’équilibre est atteint
pour les enjeux de biodiversité prioritaire déterminés en concertation
avec tous les acteurs lors de la phase de construction du projet).
</font>

<font size="4">Exemple d’un graphe montrant le résultat du calcul « pertes + gains ». Les barres et les nombres représentent les pertes ou les gains « nets ». Une perte nette signifie que les pertes dues aux impacts sont plus importantes que les gains apportées par les mesures compensatoires, et inversement :</font>

<a href="Image5.png" target="_blank"><img src="Image5_small.png" alt="im5"></a>

* * *

## Pour aller plus loin

### Analyse de méthodes d’évaluation de l’équivalence existantes comme base au développement d’ECOVAL
<a href="https://link.springer.com/article/10.1007/s00267-017-0877-5" target="_blank">Bezombes, L., Gaucherand, S., Kerbiriou, C., Reinert, M. E., & Spiegelberger, T. (2017). Ecological equivalence assessment methods: what trade-offs between operationality, scientific basis and comprehensiveness?. Environmental management, 60(2), 216-230.</a>

### Choix des indicateurs de biodiversité utilisés dans ECOVAL
<a href="https://www.sciencedirect.com/science/article/pii/S1470160X18304679" target="_blank">Bezombes, L., Gaucherand, S., Spiegelberger, T., Gouraud, V., & Kerbiriou, C. (2018). A set of organized indicators to conciliate scientific knowledge, offset policies requirements and operational constraints in the context of biodiversity offsets. Ecological indicators, 93, 1244-1252.</a>

### Présentation d’ECOVAL
<a href="http://www.set-revue.fr/un-cadre-methodologique-pour-evaluer-lequivalence-entre-pertes-et-gains-de-biodiversite-induits-par" target="_blank">Bezombes L., Kerbiriou C., Spiegelberger T., Gouraud V., Gaucherand S. (2018) Un cadre méthodologique pour évaluer l’équivalence entre pertes et gains de biodiversité induits par les projets d’aménagement et leurs mesures compensatoires, Revue Science Eaux & Territoires, article hors-série, 9 p.</a>

### Thèse de Lucie Bezombes
<a href="BEZOMBES_2017_diffusion.pdf" target="_blank"><img src="these_small.png" alt="these"></a>&emsp;&emsp;&emsp;&emsp;&emsp;<a href="BEZOMBES_2017_presentation.pdf" target="_blank"><img src="presentation_small.png" alt="these_presentation"></a>
