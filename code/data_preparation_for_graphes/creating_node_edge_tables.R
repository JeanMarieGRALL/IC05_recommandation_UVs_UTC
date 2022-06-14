library(readr)
library(data.table)
library(dplyr)

Inscriptions_UVs_A2018_P2022 <- read_delim("../../data/Inscriptions_UVs_A2018-P2022.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

setDT(Inscriptions_UVs_A2018_P2022)

etudiants = Inscriptions_UVs_A2018_P2022[, .(Etudiant, `Dernière inscription`, `Diplôme`, Branche, Niveau, Filière, `Ancienneté`, `Statut`)]
etudiants = distinct(etudiants)
etudiants$type = "etudiants"
etudiants = rename(etudiants, Id = Etudiant)

UVs = Inscriptions_UVs_A2018_P2022[, .(`Code UV`, `Catégorie UV`, `Crédits ECTS`)]
UVs = UVs[order(`Crédits ECTS`)]
UVs = distinct(UVs, `Code UV`, .keep_all = TRUE)
UVs$type = "UV"
UVs = rename(UVs, Id = `Code UV`) # Ici on peut ajouter les infos scrappées concernant les UVs

nodes_incriptions = rbind(etudiants, UVs, fill = TRUE)

write_csv(nodes_incriptions, "../../data/data_for_graphes/nodes_incriptions.csv")

edges_inscriptions = Inscriptions_UVs_A2018_P2022[, .(Etudiant, `Code UV`, `Diplôme UV`, `Spécialité UV`, `Filière UV`, `Niveau UV`, `Période UV`, `Réussite UV`)]
edges_inscriptions[`Période UV` == "Printemps 2022",]$`Réussite UV` = "en cours"
edges_inscriptions = rename(edges_inscriptions, Source = Etudiant, Target = `Code UV`)

write_csv(edges_inscriptions, "../../data/data_for_graphes/edges_inscriptions.csv")

# UVS inconnues Master: TIS02, SCI25, TIS05, SCI22, SCI31
# UV inconnue HuTech : FR00 (une seule inscription, 0 crédits)

##################
# Création des tables Nodes - Edge pour les apprentis
##################

etudiants_apprentis = Inscriptions_UVs_A2018_P2022[Statut == "Apprentissage (Formation initiale)" & `Diplôme UV` == "Branche", .(Etudiant, `Dernière inscription`, `Diplôme`, Branche, Niveau, Filière, `Ancienneté`, `Statut`)]
etudiants_apprentis = distinct(etudiants_apprentis)
etudiants_apprentis$type = "etudiants"
etudiants_apprentis = rename(etudiants_apprentis, Id = Etudiant)

UVs_apprentis = Inscriptions_UVs_A2018_P2022[Statut == "Apprentissage (Formation initiale)" & `Diplôme UV` == "Branche", .(`Code UV`, `Catégorie UV`, `Crédits ECTS`)]
UVs_apprentis = UVs_apprentis[order(`Crédits ECTS`)]
UVs_apprentis = distinct(UVs_apprentis, `Code UV`, .keep_all = TRUE)
UVs_apprentis$type = "UV"
UVs_apprentis = rename(UVs_apprentis, Id = `Code UV`) # Ici on peut ajouter les infos scrappées concernant les UVs

apprentis_nodes_incriptions = rbind(etudiants_apprentis, UVs_apprentis, fill = TRUE)

write_csv(apprentis_nodes_incriptions, "../../data/data_for_graphes/apprentis_nodes_incriptions.csv")

apprentis_edges_inscriptions = Inscriptions_UVs_A2018_P2022[Statut == "Apprentissage (Formation initiale)" & `Diplôme UV` == "Branche", .(Etudiant, `Code UV`, `Diplôme UV`, `Spécialité UV`, `Filière UV`, `Niveau UV`, `Période UV`, `Réussite UV`)]
apprentis_edges_inscriptions[`Période UV` == "Printemps 2022",]$`Réussite UV` = "en cours"
apprentis_edges_inscriptions = rename(apprentis_edges_inscriptions, Source = Etudiant, Target = `Code UV`)

write_csv(apprentis_edges_inscriptions, "../../data/data_for_graphes/apprentis_edges_inscriptions.csv")

##################
# Création des tables Nodes - Edge pour les TC
##################

etudiants_TC = Inscriptions_UVs_A2018_P2022[`Diplôme UV` == "TC", .(Etudiant, `Dernière inscription`, `Diplôme`, Branche, Niveau, Filière, `Ancienneté`, `Statut`)]
etudiants_TC = distinct(etudiants_TC)
etudiants_TC$type = "etudiants"
etudiants_TC = rename(etudiants_TC, Id = Etudiant)

UVs_TC = Inscriptions_UVs_A2018_P2022[`Diplôme UV` == "TC", .(`Code UV`, `Catégorie UV`, `Crédits ECTS`)]
UVs_TC = UVs_TC[order(`Crédits ECTS`)]
UVs_TC = distinct(UVs_TC, `Code UV`, .keep_all = TRUE)
UVs_TC$type = "UV"
UVs_TC = rename(UVs_TC, Id = `Code UV`) # Ici on peut ajouter les infos scrappées concernant les UVs

TC_nodes_incriptions = rbind(etudiants_TC, UVs_TC, fill = TRUE)

write_csv(TC_nodes_incriptions, "../../data/data_for_graphes/TC_nodes_incriptions.csv")

TC_edges_inscriptions = Inscriptions_UVs_A2018_P2022[`Diplôme UV` == "TC", .(Etudiant, `Code UV`, `Diplôme UV`, `Spécialité UV`, `Filière UV`, `Niveau UV`, `Période UV`, `Réussite UV`)]
TC_edges_inscriptions[`Période UV` == "Printemps 2022",]$`Réussite UV` = "en cours"
TC_edges_inscriptions = rename(TC_edges_inscriptions, Source = Etudiant, Target = `Code UV`)

write_csv(TC_edges_inscriptions, "../../data/data_for_graphes/TC_edges_inscriptions.csv")

##################
# Création des tables Nodes - Edge pour les branches
##################

etudiants_branche = Inscriptions_UVs_A2018_P2022[`Diplôme UV` == "Branche" & Statut == "Formation initiale", .(Etudiant, `Dernière inscription`, `Diplôme`, Branche, Niveau, Filière, `Ancienneté`, `Statut`)]
etudiants_branche = distinct(etudiants_branche)
etudiants_branche$type = "etudiants"
etudiants_branche = rename(etudiants_branche, Id = Etudiant)

UVs_branche = Inscriptions_UVs_A2018_P2022[`Diplôme UV` == "Branche" & Statut == "Formation initiale", .(`Code UV`, `Catégorie UV`, `Crédits ECTS`)]
UVs_branche = UVs_branche[order(`Crédits ECTS`)]
UVs_branche = distinct(UVs_branche, `Code UV`, .keep_all = TRUE)
UVs_branche$type = "UV"
UVs_branche = rename(UVs_branche, Id = `Code UV`) # Ici on peut ajouter les infos scrappées concernant les UVs
branche_nodes_incriptions = rbind(etudiants_branche, UVs_branche, fill = TRUE)

write_csv(branche_nodes_incriptions, "../../data/data_for_graphes/branche_nodes_incriptions.csv")

branche_edges_inscriptions = Inscriptions_UVs_A2018_P2022[`Diplôme UV` == "Branche" & Statut == "Formation initiale", .(Etudiant, `Code UV`, `Diplôme UV`, `Spécialité UV`, `Filière UV`, `Niveau UV`, `Période UV`, `Réussite UV`)]
branche_edges_inscriptions[`Période UV` == "Printemps 2022",]$`Réussite UV` = "en cours"
branche_edges_inscriptions = rename(branche_edges_inscriptions, Source = Etudiant, Target = `Code UV`)

write_csv(branche_edges_inscriptions, "../../data/data_for_graphes/branche_edges_inscriptions.csv")
