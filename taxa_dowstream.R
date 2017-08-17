# Script para ver os generos em ter steege et al 2016 e quantas espécies em cada genero

library(taxize)
library(plyr)
library(data.table)
library(dplyr)
library(magrittr)

setwd("C:/Users/Bruno Garcia Luiz/Documents/DB_Trees_Species_Inventories_Amazon_Wetlands/MS_Tree_species_pool/SpeciesPool_amazonian_floodplains_sec/Result_intermediario")

checklist <- fread("srep29549-s2_all_spp.csv")#11683 linhas
checklist <- checklist[-c(11677:11683),]# remove ultimas linhas informa diversidade e nao especie
dim(checklist)#11676 species names X 6 collumns: ID_ALLTGT	Species	Author	Family	Source Name	url
dimnames(checklist)

checklist<-gbif_parse(checklist$Species) 

checklist_generos <- as.data.frame(unique(checklist$genusorabove))
checklist_generos

#a<- downstream(checklist_generos[1,1], downto = "species", db="col")
?downstream
test<- downstream(checklist_generos[1,1], downto = "species", db="col",rows = 1)


p<- downstream(checklist_generos[1,1], downto = "species", db="col",rows = 1)
for(i in 2:nrow(checklist_generos)){
  p<- rbind(p, downstream(checklist_generos[i,1], downto = "species", db="col",rows = 1))
}

class(p)
dim(p)
p[2]

p_df <- do.call("rbind",p)

write.csv(p_df, "checklist_spp_gen_all_COL.csv")

?ipni_search

spp_gen <- ipni_search(genus="Abarema", output = "short")

nrow(checklist_generos)

spp_pouteria <- ipni_search(genus="Pouteria", output = "short")

spp_gen_b <- ipni_search(genus="Abarema", output = "short")
for(i in 2:nrow(checklist_generos)){
  spp_gen <- rbind(spp_gen,ipni_search(genus=as.character(checklist_generos[i,1]), output = "short", rankToReturn ="spec" ))
  }

warnings()

unique(spp_gen$rank)
spp_gen_F <- filter(spp_gen, spp_gen$rank == "spec.")

write.csv(spp_gen_F, "checklist_spp_gen_all_IPNI.csv")

todas_spp_gen<- unique(spp_gen_F$full_name_without_family_and_authors)

write.csv(todas_spp_gen, "checklist_unique_spp_gen_all_IPNI.csv")

checklist_ipni_tnrs <- fread("checklist_unique_spp_gen_all_IPNI_TNRS.txt")

length(unique(checklist_ipni_tnrs$Name_submitted))
length(unique(checklist_ipni_tnrs$Genus_matched))

checklist_ipni_tnrs %>% filter(Selected=="true")%>%
                     filter(Taxonomic_status=="Accepted")-> acc.checklist_ipni_tnrs

acc.checklist_ipni_tnrs # tabela onde as especies aceitas para cada genero são listadas - geral ALL
unique(acc.checklist_ipni_tnrs$Genus_matched)

#quantas especies por genero
generos_conta_all<- count(acc.checklist_ipni_tnrs$Genus_matched)

generos_conta_amz <- count(checklist$genusorabove)

generos_conta_amz$match <-  generos_conta_amz$x %in% generos_conta_all$x

generos_conta_amz %>% filter(match=="TRUE")-> generos_conta_amz_F
 
generos_conta_all$match <-  generos_conta_all$x %in% generos_conta_amz_F$x

generos_conta_all %>% filter(match=="TRUE")-> generos_conta_all_F

generos_conta_amz_F$prop <- round((generos_conta_amz_F$freq/generos_conta_all_F$freq)*100,1)

