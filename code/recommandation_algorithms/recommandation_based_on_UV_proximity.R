library(readr)
library(data.table)
library(dplyr)
library(mltools) # Pour le One-Hot encoding
library(data.table)
library(proxy) # Pour le calcul des proximites
library(stringr)

# Liste de toutes les insciptions aux UVs depuis A18
Inscriptions_A18_P22 <- read_delim("../../data/Inscriptions_UVs_A2018-P2022.csv",
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
Inscriptions_A18_P22 = rename(Inscriptions_A18_P22, UV = "Code UV")

# Pour l'instant l'algorithme ne recommande que des UVs de branche
UVs_Branche <- setDT(read_delim("../../data/data_for_graphes/branche_nodes_incriptions.csv", 
                                delim = ",", escape_double = FALSE, trim_ws = TRUE))[type == "UV", Id]

# Les UVs suiavntes sont obligatoires dans tous les cursus, il n'est donc par pertinent de les recommander
UVs_a_exclures <- c("TN09", "TN10", "MA09", "TX00", "PR00", "LA93", "LA92") 

# Proximite entre deux UVs
proxi_UV_UV <- function(x, y){ 
  # proxi_UV_UV(i, j) = nb_etudiants_ayant_suivi_i_et_j / nb_etudiants_ayant_suivi_au_moins_une_des_deux
  sum(x & y) / (sum(x) + sum(y) - sum(x & y))
}

etu_uv = Inscriptions_A18_P22[, c("Etudiant","UV")] # Toutes les inscriptions aux UVs depuis A18
etu_uv$UV <- as.factor(etu_uv$UV)
etu_uv = distinct(etu_uv) # Si un etudiant a realise plusieurs fois la meme UV, on considere qu'il ne l'a faite qu'une fois

etu_uv <- one_hot(as.data.table(etu_uv)) # One-Hot encoding du tableau
etu_uv = etu_uv[, lapply(.SD, sum), .(Etudiant)]
setDF(etu_uv) # Tableau Etudiants - UVs
etu_uv$Etudiant = NULL

uv_etu = t(etu_uv)  # Tableau UVs - Etudiants
rm(etu_uv)
df_proximite_uv_uv = as.matrix(dist(uv_etu, method = proxi_UV_UV)) # Calcul la matrice de proximite

liste_UV_etu = c("AS01", "LA13", "MQ01", "MQ03", "TN12", "GE25", "NF04", "TF01", "TN20") # Les UVs que l'utilisateur met en entree

# Pour chaque UV que l'utilisateur a saisi, on prend les 30 UVs les plus proches et on les met dans un dataframe (UV, proximite)
UVs_proches = data.frame(UV = character(), Proxi = double())

for(uv in paste0("UV_", liste_UV_etu)){
  UVs_proches = rbind(UVs_proches, data.frame(UV = row.names(df_proximite_uv_uv[order(-df_proximite_uv_uv[, uv]),])[c(1:30)], Proxi = df_proximite_uv_uv[order(-df_proximite_uv_uv[, uv]),][c(1:30), uv]))
}
rm(uv)



setDT(UVs_proches)
UVs_proches$UV = str_sub(UVs_proches$UV, start = 4)

# On exclu les UVs non branche et les UVs non recommandables, puis on aggrege par UV en faisait la somme des proximites
UVs_proches = UVs_proches[!(UV %in% liste_UV_etu) & (UV %in% UVs_Branche) & !(UV %in% UVs_a_exclures), sum(Proxi), .(UV)]

# On ajoute la catégorie de l'UV (CS, TM, TSH)
UVs_proches = merge(UVs_proches, distinct(Inscriptions_A18_P22[, c("UV", "Catégorie UV")], UV, .keep_all = TRUE), by = "UV", how = "left")

# On ordonne de manière décroissante de façon a avoir les UVs les plus proches en haut du tableau
UVs_proches = UVs_proches[order(-UVs_proches$V1), ]
UVs_proches = rename(UVs_proches, type_UV = "Catégorie UV")
UVs_proches = rename(UVs_proches, Proximite = V1)

