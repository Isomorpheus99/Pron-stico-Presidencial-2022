#Pronóstico Elecciones Presidenciales Colombia 2022
#Mayo 2022
#Load libraries
# R
rm(list=ls(all=TRUE))
# We recommend running this is a fresh R session or restarting your current session
library(magrittr)
library(readxl)
library(plyr)
library(Rmisc)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(ggplot2)
#Load data
setwd("C:/Users/sergi/OneDrive/Desktop/42/Work/Research/Election Forecasting/Colombia/Presidential")
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
SimulationElect<-function(S){
  dfs=list()
  for(i in 1:S){
    x<-rnorm(8)
    x<-x/2
    dfadjust<-clean_df%>%
      mutate(Gustavo_Petro=Gustavo_Petro+x[1]*MoE, Fico_Gutierrez=Fico_Gutierrez+x[2]*MoE,
             Sergio_Fajardo=Sergio_Fajardo+x[3]*MoE, Rodolfo_Hernandez=Rodolfo_Hernandez+x[4]*MoE,
             Ingrid_Betancourt=Ingrid_Betancourt+x[5]*MoE, John_Rodriguez=John_Rodriguez+x[6]*MoE,
             Enrique_Gomez=Enrique_Gomez+x[7]*MoE, Luis_Perez=Luis_Perez, En_Blanco=En_Blanco+x[8]*MoE)
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
    dfadjust<-dfadjust %>%
      mutate(Gustavo_Petro=if_else(Gustavo_Petro<0, 0, Gustavo_Petro), 
             Fico_Gutierrez=if_else(Fico_Gutierrez<0, 0, Fico_Gutierrez),
             Sergio_Fajardo=if_else(Sergio_Fajardo<0, 0, Sergio_Fajardo),
             Rodolfo_Hernandez=if_else(Rodolfo_Hernandez<0, 0, Rodolfo_Hernandez),
             Ingrid_Betancourt=if_else(Ingrid_Betancourt<0, 0, Ingrid_Betancourt),
             John_Rodriguez=if_else(John_Rodriguez<0, 0, John_Rodriguez),
             Enrique_Gomez=if_else(Enrique_Gomez<0, 0, Enrique_Gomez),
             Luis_Perez=if_else(Luis_Perez<0, 0, Luis_Perez),
             En_Blanco=if_else(En_Blanco<0, 0, En_Blanco))
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
    dffinal<-dfweighting %>%
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
      summarize(Predicción=signif(weighted.mean(Int_ajus_voto, Weight)*100, digits=4))%>%
      arrange(desc(Predicción))
    dfs=append(dfs,dffinal)
  }
  Result<-join_all(dfs, by="Candidato",type = "left", match = "all")
  scale<-function(x) x/sum(x)
  Result<-Result%>%
    mutate(across(!Candidato, scale))
  Dist<-Result
  Result<-Result%>%
    mutate(Promedio=rowMeans(Result[,2:ncol(Result)]))
  Result<-Result%>%
    select(c(Candidato, Promedio))
  Return<-list("Dist"=Dist, "Result"=Result)
  return(Return)
}

SimulationElect(2)
install.packages("gmodels")
apply(as.matrix(dffinal3), 1, function(x) ci(x))

