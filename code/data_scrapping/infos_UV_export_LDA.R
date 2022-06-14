library(stringr)
library(readr)
library(dplyr)

UVs <- read_csv("../../data/data_scrapped_from_UV_catalogue/UVs.csv")


description = UVs[,c("Code",
                     "Mots_cles",
                     "Description",
                     "Intitule",
                     "Domaine",
                     "Theme",
                     "Objectif_formation",
                     "Objectifs_pedagogiques_specifiques",
                     "Objectifs_pedagogiques_transverses",
                     "Programme", "Resulat"
)]

description$Description = str_replace_all(description$Description,"Ã©","e")
description$Description = str_replace_all(description$Description,"�","c")
description$Description = str_replace_all(description$Description,"Ãš","e")
description$Description = str_replace_all(description$Description,"A Ã"," a")

description = description[description$Code != "SC23",]

description$infos = paste(description$Intitule,
                          description$Mots_cles, 
                          description$Description, 
                          description$Domaine,
                          description$Theme,
                          description$Objectif_formation,
                          description$Objectifs_pedagogiques_specifiques,
                          description$Objectifs_pedagogiques_transverses,
                          description$Programme,
                          description$Resulat,
                          sep = " / ")

description = distinct(description, Code, .keep_all = T )

write.csv(description[,c("Code","infos")], "../../data/data_scrapped_from_UV_catalogue/descriptions.csv")

