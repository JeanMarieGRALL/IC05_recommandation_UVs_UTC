library(RCurl)# Recuperer des pages URL
library(XML) 
library(tidyr) # Pour la manipulation de donnees
library(stringr) # Travail sur les chaines de caracteres
library(questionr) # Manipulation de donnees
library(dplyr)

dep2$noms<-str_trim(dep2$noms)
dep2<-separate(dep2, noms, c("Nom","Pr?nom"),sep=",")
dep2$Pr?nom<-str_trim(dep2$Pr?nom)


uvs <- readLines("uvs.txt")
uvs <- htmlParse(uvs)

XmlPathCodeUv <- "//h4/text()"
Code <- xpathApply(uvs,XmlPathCodeUv,xmlValue)
Credits <- "//div[@id='accueil']/table//tr[1]/td[2]/text()"
Credits <- xpathApply(uvs,Credits,xmlValue)
Domaine <- "//div[@id='accueil']/table//tr[2]/td[2]/text()"
Domaine <- xpathApply(uvs,Domaine,xmlValue)
Theme <- "//div[@id='accueil']/table//tr[3]/td[2]/text()"
Theme <- xpathApply(uvs,Theme,xmlValue)
Categorie <- "//div[@id='accueil']/table//tr[4]/td[2]/text()"
Categorie <- xpathApply(uvs,Categorie,xmlValue)
Enseignee_en <- "//div[@id='accueil']/table//tr[5]/td[2]/text()"
Enseignee_en <- xpathApply(uvs,Enseignee_en,xmlValue)
Responsable_Automne <- "//div[@id='accueil']/table//tr[6]/td[2]/text()"
Responsable_Automne <- xpathApply(uvs,Responsable_Automne,xmlValue)
Responsable_Printemps <- "//div[@id='accueil']/table//tr[7]/td[2]/text()"
Responsable_Printemps <- xpathApply(uvs,Responsable_Printemps,xmlValue)
VolumeHoraireCours <- "//div[@id='accueil']/table//tr[8]/td[2]/span[1]"
VolumeHoraireCours <- xpathApply(uvs,VolumeHoraireCours,xmlValue)
VolumeHoraireTD <- "//div[@id='accueil']/table//tr[8]/td[2]/span[2]"
VolumeHoraireTD <- xpathApply(uvs,VolumeHoraireTD,xmlValue)
VolumeHoraireHors <- "//div[@id='accueil']/table//tr[8]/td[2]/span[3]"
VolumeHoraireHors <- xpathApply(uvs,VolumeHoraireHors,xmlValue)


Description <- "//div[@id='menu1']/table//tr[1]/td[2]"
Description <- xpathApply(uvs,Description,xmlValue)
Niveau_conseille <- "//div[@id='menu1']/table//tr[3]/td[2]"
Niveau_conseille <- xpathApply(uvs,Niveau_conseille,xmlValue)
Prerequis <- "//div[@id='menu1']/table//tr[4]/td[2]"
Prerequis <- xpathApply(uvs,Prerequis,xmlValue)
Final <- "//div[@id='menu1']/table//tr[5]/td[2]"
Final <- xpathApply(uvs,Final,xmlValue)
Conditions_evaluation <- "//div[@id='menu1']/table//tr[6]/td[2]"
Conditions_evaluation <- xpathApply(uvs,Conditions_evaluation,xmlValue)
Conditions_attribution <- "//div[@id='menu1']/table//tr[7]/td[2]"
Conditions_attribution <- xpathApply(uvs,Conditions_attribution,xmlValue)
Mots_cles <- "//div[@id='menu1']/table//tr[9]/td[2]"
Mots_cles <- xpathApply(uvs,Mots_cles,xmlValue)

Objectif_formation <- "//div[@id='menu3']/table//tr[1]/td[2]"
Objectif_formation <- xpathApply(uvs,Objectif_formation,xmlValue)
Objectifs_pedagogiques_specifiques <- "//div[@id='menu3']/table//tr[2]/td[2]"
Objectifs_pedagogiques_specifiques <- xpathApply(uvs,Objectifs_pedagogiques_specifiques,xmlValue)
Objectifs_pedagogiques_transverses <- "//div[@id='menu3']/table//tr[3]/td[2]"
Objectifs_pedagogiques_transverses <-xpathApply(uvs,Objectifs_pedagogiques_transverses,xmlValue)
Programme <- "//div[@id='menu4']/table//tr[1]/td[2]"
Programme <- xpathApply(uvs,Programme,xmlValue)
Resulat <- "//div[@id='menu4']/table//tr[2]/td[2]"
Resulat <- xpathApply(uvs,Resulat,xmlValue)

rm(VolumeHoraireCours)
rm(VolumeHoraireHors)
rm(VolumeHoraireTD)
rm(dataBranche)

UVs <- cbind(Code, Credits, Domaine, Theme, Categorie, Enseignee_en, Responsable_Automne, Responsable_Printemps, Description, Niveau_conseille, Prerequis, Final, Conditions_evaluation, Conditions_attribution, Mots_cles, Objectif_formation, Objectifs_pedagogiques_specifiques, Objectifs_pedagogiques_transverses, Programme, Resulat)
UVs <- as.data.frame(UVs)

#Nettoyage
UVs$Code <- str_trim(UVs$Code)
UVs<-separate(UVs, Code, c("Code","Description"),sep=":")
UVs$Description<-str_trim(UVs$Description)

dep2$amendements<-as.numeric(str_extract(dep2$amendements,"[0-9]+"))
UVs$Credits <- as.numeric(str_extract(UVs$Credits,"[0-9]"))

UVs$Domaine <- str_replace_all(UVs$Domaine,"é","e")
UVs$Domaine <- str_replace_all(UVs$Domaine,"????","E")
UVs$Domaine <- str_replace_all(UVs$Domaine,"????","E")
UVs$Domaine <- str_replace_all(UVs$Domaine,"?^","E")
UVs$Domaine <- str_replace_all(UVs$Domaine,"?,","A")
UVs$Domaine <- str_replace_all(UVs$Domaine,"â","a")
UVs$Domaine <- str_replace_all(UVs$Domaine,"ç","?")
UVs$Domaine <- str_replace_all(UVs$Domaine,"è","e")
UVs$Domaine <- str_replace_all(UVs$Domaine,"?","oe")
UVs$Domaine <- str_replace_all(UVs$Domaine,"a?","e")




UVs$Description <- str_replace_all(UVs$Description,"é","e")
UVs$Description <- str_replace_all(UVs$Description,"????","E")
UVs$Description <- str_replace_all(UVs$Description,"????","E")
UVs$Description <- str_replace_all(UVs$Description,"?^","E")
UVs$Description <- str_replace_all(UVs$Description,"?,","A")
UVs$Description <- str_replace_all(UVs$Description,"â","a")
UVs$Description <- str_replace_all(UVs$Description,"ç","?")
UVs$Description <- str_replace_all(UVs$Description,"è","e")
UVs$Description <- str_replace_all(UVs$Description,"?","oe")
UVs$Description <- str_replace_all(UVs$Description,"a?","e")

UVs$Theme <- str_replace_all(UVs$Theme,"é","e")
UVs$Theme <- str_replace_all(UVs$Theme,"????","E")
UVs$Theme <- str_replace_all(UVs$Theme,"????","E")
UVs$Theme <- str_replace_all(UVs$Theme,"?^","E")
UVs$Theme <- str_replace_all(UVs$Theme,"?,","A")
UVs$Theme <- str_replace_all(UVs$Theme,"â","a")
UVs$Theme <- str_replace_all(UVs$Theme,"ç","?")
UVs$Theme <- str_replace_all(UVs$Theme,"è","e")
UVs$Theme <- str_replace_all(UVs$Theme,"?","oe")
UVs$Theme <- str_replace_all(UVs$Theme,"a?","e")

UVs$Categorie <- str_replace_all(UVs$Categorie,"é","e")
UVs$Categorie <- str_replace_all(UVs$Categorie,"????","E")
UVs$Categorie <- str_replace_all(UVs$Categorie,"????","E")
UVs$Categorie <- str_replace_all(UVs$Categorie,"?^","E")
UVs$Categorie <- str_replace_all(UVs$Categorie,"?,","A")
UVs$Categorie <- str_replace_all(UVs$Categorie,"â","a")
UVs$Categorie <- str_replace_all(UVs$Categorie,"ç","?")
UVs$Categorie <- str_replace_all(UVs$Categorie,"è","e")
UVs$Categorie <- str_replace_all(UVs$Categorie,"?","oe")
UVs$Categorie <- str_replace_all(UVs$Categorie,"a?","e")

UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"é","e")
UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"????","E")
UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"????","E")
UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"?^","E")
UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"?,","A")
UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"â","a")
UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"ç","?")
UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"è","e")
UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"?","oe")
UVs$Enseignee_en <- str_replace_all(UVs$Enseignee_en,"a?","e")

UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"é","e")
UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"????","E")
UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"????","E")
UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"?^","E")
UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"?,","A")
UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"â","a")
UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"ç","?")
UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"è","e")
UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"?","oe")
UVs$Responsable_Automne <- str_replace_all(UVs$Responsable_Automne,"a?","e")

UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"é","e")
UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"????","E")
UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"????","E")
UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"?^","E")
UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"?,","A")
UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"â","a")
UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"ç","?")
UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"è","e")
UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"?","oe")
UVs$Responsable_Printemps <- str_replace_all(UVs$Responsable_Printemps,"a?","e")

UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"é","e")
UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"????","E")
UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"????","E")
UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"?^","E")
UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"?,","A")
UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"â","a")
UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"ç","?")
UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"è","e")
UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"?","oe")
UVs$Niveau_conseille <- str_replace_all(UVs$Niveau_conseille,"a?","e")

UVs$Prerequis <- str_replace_all(UVs$Prerequis,"é","e")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"????","E")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"????","E")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"?^","E")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"?,","A")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"â","a")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"ç","?")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"è","e")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"?","oe")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"a?","e")
UVs$Prerequis <- str_replace_all(UVs$Prerequis,"¿","'")


UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"é","e")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"????","E")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"????","E")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"?^","E")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"?,","A")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"â","a")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"ç","?")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"?","a")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"af©","e")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"è","e")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"?","oe")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"a?","e")
UVs$Conditions_evaluation <- str_replace_all(UVs$Conditions_evaluation,"¿","'")


UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"é","e")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"????","E")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"????","E")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"?^","E")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"?,","A")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"â","a")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"ç","?")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"?","a")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"af©","e")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"è","e")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"?","oe")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"a?","e")
UVs$Conditions_attribution <- str_replace_all(UVs$Conditions_attribution,"¿","'")


UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"é","e")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"????","E")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"????","E")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"?^","E")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"?,","A")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"â","a")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"ç","?")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"?","a")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"af©","e")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"è","e")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"?","oe")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"a?","e")
UVs$Mots_cles <- str_replace_all(UVs$Mots_cles,"¿","'")


UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"é","e")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"????","E")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"????","E")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"?^","E")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"?,","A")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"â","a")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"ç","?")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"?","a")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"af©","e")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"è","e")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"?","oe")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"a?","e")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"a?","e")
UVs$Objectif_formation <- str_replace_all(UVs$Objectif_formation,"¿","'")

UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"é","e")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"????","E")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"????","E")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"?^","E")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"?,","A")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"â","a")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"ç","?")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"?","a")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"af©","e")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"è","e")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"?","oe")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"a?","e")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"a?","e")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"¿","'")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"aa?","'ai")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"a?","e")
UVs$Objectifs_pedagogiques_specifiques <- str_replace_all(UVs$Objectifs_pedagogiques_specifiques,"a?","o")

UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"é","e")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"????","E")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"????","E")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"?^","E")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"?,","A")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"â","a")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"ç","?")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"?","a")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"af©","e")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"è","e")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"?","oe")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"a?","e")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"a?","e")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"¿","'")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"aa?","'ai")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"a?","e")
UVs$Objectifs_pedagogiques_transverses <- str_replace_all(UVs$Objectifs_pedagogiques_transverses,"a?","o")

UVs$Programme <- str_replace_all(UVs$Programme,"é","e")
UVs$Programme <- str_replace_all(UVs$Programme,"????","E")
UVs$Programme <- str_replace_all(UVs$Programme,"????","E")
UVs$Programme <- str_replace_all(UVs$Programme,"?^","E")
UVs$Programme <- str_replace_all(UVs$Programme,"?,","A")
UVs$Programme <- str_replace_all(UVs$Programme,"â","a")
UVs$Programme <- str_replace_all(UVs$Programme,"ç","?")
UVs$Programme <- str_replace_all(UVs$Programme,"?","a")
UVs$Programme <- str_replace_all(UVs$Programme,"af©","e")
UVs$Programme <- str_replace_all(UVs$Programme,"è","e")
UVs$Programme <- str_replace_all(UVs$Programme,"?","oe")
UVs$Programme <- str_replace_all(UVs$Programme,"a?","e")
UVs$Programme <- str_replace_all(UVs$Programme,"a?","e")
UVs$Programme <- str_replace_all(UVs$Programme,"af?fA©","outen")
UVs$Programme <- str_replace_all(UVs$Programme,"¿","'")
UVs$Programme <- str_replace_all(UVs$Programme,"aa?","'ai")
UVs$Programme <- str_replace_all(UVs$Programme,"a?","e")
UVs$Programme <- str_replace_all(UVs$Programme,"a?","o")

UVs$Resulat <- str_replace_all(UVs$Resulat,"é","e")
UVs$Resulat <- str_replace_all(UVs$Resulat,"????","E")
UVs$Resulat <- str_replace_all(UVs$Resulat,"????","E")
UVs$Resulat <- str_replace_all(UVs$Resulat,"?^","E")
UVs$Resulat <- str_replace_all(UVs$Resulat,"?,","A")
UVs$Resulat <- str_replace_all(UVs$Resulat,"â","a")
UVs$Resulat <- str_replace_all(UVs$Resulat,"ç","?")
UVs$Resulat <- str_replace_all(UVs$Resulat,"?","a")
UVs$Resulat <- str_replace_all(UVs$Resulat,"af©","e")
UVs$Resulat <- str_replace_all(UVs$Resulat,"è","e")
UVs$Resulat <- str_replace_all(UVs$Resulat,"?","oe")
UVs$Resulat <- str_replace_all(UVs$Resulat,"a?","e")
UVs$Resulat <- str_replace_all(UVs$Resulat,"a?","e")
UVs$Resulat <- str_replace_all(UVs$Resulat,"¿","'")
UVs$Resulat <- str_replace_all(UVs$Resulat,"aa?","'ai")
UVs$Resulat <- str_replace_all(UVs$Resulat,"a?","e")
UVs$Resulat <- str_replace_all(UVs$Resulat,"a?","o")
