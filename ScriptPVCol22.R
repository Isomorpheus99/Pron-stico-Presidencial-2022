#Pronóstico Elecciones Presidenciales Colombia 2022
#Mayo 2022
#Load libraries
# R
rm(list=ls(all=TRUE))
# We recommend running this is a fresh R session or restarting your current session
library(magrittr)
library(readxl)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(ggplot2)
#Load data
setwd("D:/Uni_PUJ/Semestre 4")
df<-read_xlsx("DataColPV2022.xlsx", sheet="Polls")
dfweights<-read_xlsx("DataColPV2022.xlsx", sheet="Rating")
clean_df<-df %>%
  select(-c(Source, Link)) %>%
  mutate(John_Rodriguez=as.numeric(John_Rodriguez), Enrique_Gomez=as.numeric(Enrique_Gomez),
         Luis_Perez=as.numeric(Luis_Perez), None=as.numeric(None), Uncertain=as.numeric(Uncertain))%>%
  replace_na(list(John_Rodriguez=0, Enrique_Gomez=0, Luis_Perez=0, None=0, Uncertain=0)) %>%
  mutate(Undefined=None+Uncertain+Blanco, 
         En_Blanco=0.0246,
         Vote=1-Undefined+En_Blanco,
         voteava=1-Vote,
         Vote=1-Undefined+En_Blanco,
         voteava=1-Vote
  )
SimulationElect<-(S){
  dffinal=list()
  for(i in 1:S){
  x<-rnrom(8)
  x<-x/2
  dfadjust<-clean_df%>%
    mutate(Gustavo_Petro=Gustavo_Petro+x[1]*MoE, Fico_Gutierrez=Fico_Gutierrez+x[2]*MoE,
           Sergio_Fajardo=Sergio_Fajardo+x[3]*MoE, Rodolfo_Hernandez=Rodolfo_Hernandez+x[4]*MoE,
           Ingrid_Betancourt=Ingrid_Betancourt+x[5]*MoE, John_Rodriguez=John_Rodriguez+x[6]*MoE,
           Enrique_Gomez=Enrique_Gomez+x[7]*MoE, En_Blanco=En_Blanco+x[8]*MoE)
  random=runif(8)
  for(i in 1:8){
    random[[i]]=random[[i]]/sum(random)
  }
  dfadjust<-dfadjust %>%
    mutate(Gustavo_Petro=Gustavo_Petro+random[1]*voteava, 
           Fico_Gutierrez=Fico_Gutierrez+random[2]*voteava, Sergio_Fajardo=Sergio_Fajardo+random[3]*voteava, 
           Rodolfo_Hernandez=Rodolfo_Hernandez+random[4]*voteava, Ingrid_Betancourt=Ingrid_Betancourt+random[5]*voteava, 
           Enrique_Gomez=Enrique_Gomez+random[6]*voteava,
           John_Rodriguez=John_Rodriguez+random[7]*voteava, Luis_Perez=Luis_Perez, En_Blanco=En_Blanco+random[8]*voteava)%>%
    left_join(dfweights)
  
  currentdate=Sys.Date()
  dfweighting<-dfadjust%>%
    select(-(Rating))%>%
    #filter(Pollster!="MassiveCaller")%>%
    mutate(Age=as.numeric(currentdate-as.Date(Date)),
           Decay=1/(MoE*Accuracy))
  dfweighting<-dfweighting%>%
    mutate(Time=((Decay*0.5)/mean(dfweighting$Decay))^(Age/30))
  dfweighting<-dfweighting%>%
    mutate(Weight=Time/sum(dfweighting$Time))
  dffinal[[i]]<-dfweighting %>%
    pivot_longer(cols=contains("_"), 
                 names_to="nombre", values_to="Int_ajus_voto") %>%
    mutate(Candidato=case_when(nombre=="Gustavo_Petro"~"Gustavo Petro", 
                               nombre=="Fico_Gutierrez"~"Federico Gutiérrez", 
                               nombre=="Sergio_Fajardo"~"Sergio Fajardo",
                               nombre=="Rodolfo_Hernandez"~"Rodolfo Hernández",
                               nombre=="John_Rodriguez"~"John M. Rodríguez",
                               nombre=="Luis_Perez"~"Luis Pérez",
                               nombre=="Enrique_Gomez"~"Enrique Gómez",
                               nombre=="Ingrid_Betancourt"~"Ingrid Betancourt",
                               nombre=="En_Blanco"~"Voto en Blanco")%>%
             factor())%>%
    group_by(Candidato)%>%
    summarize(signif(Predicción=weighted.mean(Int_ajus_voto, Weight)*100, digits=4))%>%
    arrange(desc(Candidato))
  dffinal2<- dffinal%>%
    select(-c(Candidato))
  for(j in ncol(dffinal[i])){
    dffinal[i][j, 2]=dffinal2[i][j]/as.numeric(colSums(dffinal2))
  }
  }
  Result=bind_rows(dffinal, .id = "Candidato")
}