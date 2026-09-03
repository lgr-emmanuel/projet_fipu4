###############################################
## Installation des paquets nécessaires, chargement des librairies

packages = c("dplyr", "tidyr", "ggplot2", "gridExtra", "xtable", "knitr")

for(pack in packages){
  if(!(pack %in% rownames(installed.packages()))){
    install.packages(pack)
  }
}


library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(xtable)
library(knitr)


################################################

# Détermination de l'année en cours
annee_finale = as.Date(Sys.Date(), "%Y-%m-%d")
annee_choisie = as.integer(format(annee_finale, '%Y')) - 1

# annee_finale = 2022  #si l'utilisateur veut les résultats pour 2022 plutôt que pour la dernière année donnée par Eurostat

# Récupération et mise en forme des données dans < df_final >

url = paste("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/gov_10dd_edpt1/1.0/*.*.*.*.*?c[freq]=A&c[unit]=PC_GDP&c[sector]=S13&c[na_item]=GD&c[geo]=EU27_2020,EA21,EA20,EA19,BE,BG,CZ,DK,DE,EE,IE,EL,ES,FR,HR,IT,CY,LV,LT,LU,HU,MT,NL,AT,PL,PT,RO,SI,SK,FI,SE&c[TIME_PERIOD]=", 
            annee_choisie, ",", annee_choisie - 1, ",", annee_choisie - 2, ",", annee_choisie - 3, 
            "&compress=false&format=csvdata&formatVersion=2.0&lang=fr&labels=name", sep="")

destfile = paste(getwd(), "/donnees.csv", sep="") # Nom du fichier .csv qui contient les données d'Eurostat (avec chemin absolu)

download.file(url, destfile)

df = read.csv("donnees.csv")

df_final = df |> pivot_wider(names_from = TIME_PERIOD, names_prefix = "annee", values_from = OBS_VALUE)


# Pour que la structure de l'eurozone soit correcte à date
if(annee_choisie <= 2022){zone_euro = "EA19"
}else{
  if(2023 <= annee_choisie && annee_choisie <=2025){
    zone_euro = "EA20"
  }else{zone_euro = "EA21"}
}


#################################################
## Calculs et données qui nous intéressent 

# Récupération des colonnes d'intérêt
nb_col = ncol(df_final)
res = df_final |> 
  select(geo, all_of(nb_col - 3), all_of(nb_col - 1), all_of(nb_col))

# Détermination des pays aux dettes extrémales à l'année choisie   
pays_dette_max = res[which.max(unlist(res[, 4])), "geo"]
pays_dette_min = res[which.min(unlist(res[, 4])), "geo"]

# Récupération des dettes publiques, calcul des variations

res = res |>
  filter(geo %in% c("FR", zone_euro, "EU27_2020", pays_dette_max, pays_dette_min)) |> # si autre pays désiré, remplacer FR par GE (pour l'Allemagne), etc.
  mutate(diff_0_moins1 = select(cur_data_all(), 4) - select(cur_data_all(), 3)) |>
  mutate(diff_0_moins3 = select(cur_data_all(), 4) - select(cur_data_all(), 2))

###################################################
## Mise en page du tableau des résultats

ordre_lignes = c("FR", zone_euro, "EU27_2020", pays_dette_max, pays_dette_min)
res = res |>
  slice(match(ordre_lignes, geo))

dette_forte = paste("État à plus forte dette en ", annee_choisie, " (", res[4, "geo"], ")", sep="")
dette_faible = paste("État à plus faible dette en ", annee_choisie, " (", res[5, "geo"], ")", sep="")
res$geo = c("France", "Zone euro", "Union européenne", dette_forte, dette_faible)


names(res)[1] = "*"
names(res)[2] = annee_choisie - 3
names(res)[3] = annee_choisie - 1
names(res)[4] = annee_choisie
names(res)[5] = paste("Evolution", annee_choisie - 1, "/", annee_choisie, sep=" ")         
names(res)[6] = paste("Evolution", annee_choisie - 3, "/", annee_choisie, sep=" ") 

View(res)

## Exportation du tableau des résultats en .png, .html et en Markdown

#Export en .png
png(paste("resultats_", annee_choisie, ".png", sep=""), height = 200, width = 800)
p = tableGrob(res)
grid.arrange(p)
dev.off()

# Export en html
print(xtable(res), type = "html", file = paste("resultats_", annee_choisie, ".html", sep=""))

# Export en markdown
knitr::kable(res, format = "markdown", file = paste("resultats_", annee_choisie, ".md", sep=""))
