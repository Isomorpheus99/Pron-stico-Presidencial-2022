#Modelo pronóstico 
#Elecciones Presidenciales Colombia 2022
#Segunda Vuelta
#Autor: Sergio Eduardo Calvo Mazuera
rm(list=ls(all=TRUE))
library(magrittr)
library(plyr)
library(Rmisc)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(prophet)
library(ggplot2)
library(matrixStats)
setwd("C:/Users/sergi/OneDrive/Desktop/42/Work/Research/Election Forecasting/Colombia/Presidential")
df<-read.table("SerieTemporalSV2022.txt", T)
clean_df<-df%>%
  mutate(Gustavo_Petro=Gustavo_Petro/(1-None),
         Rodolfo_Hernandez=Rodolfo_Hernandez/(1-None),
         En_Blanco=En_Blanco/(1-None))
SVSimulation<-function(S){
  dflist=list()
  dflowlist=list()
  dfupplist=list()
  for(i in 1:S){
    TS<-clean_df
    n_polls=nrow(TS)
    TS$GPrand<-rnorm(n_polls)/2
    TS$RHrand<-rnorm(n_polls)/2
    TS$EBrand<-rnorm(n_polls)/2

    
    TS<-TS%>%
      mutate(GPMoE=qnorm(0.975)*sqrt((Gustavo_Petro*(1-Gustavo_Petro))/SS),
             RHMoE=qnorm(0.975)*sqrt((Rodolfo_Hernandez*(1-Rodolfo_Hernandez))/SS),
             EBMoE=qnorm(0.975)*sqrt((En_Blanco*(1-En_Blanco))/SS))%>%
      mutate(Gustavo_Petro=Gustavo_Petro+GPrand*GPMoE,
             Rodolfo_Hernandez=Rodolfo_Hernandez+RHrand*RHMoE,
             En_Blanco=En_Blanco+EBrand*EBMoE)
    TS<-TS %>%
      mutate(Gustavo_Petro=if_else(Gustavo_Petro<0, 0, Gustavo_Petro), 
             Rodolfo_Hernandez=if_else(Rodolfo_Hernandez<0, 0, Rodolfo_Hernandez),
             En_Blanco=if_else(En_Blanco<0, 0, En_Blanco))
  
    TS<-TS%>%
      mutate(Total=Gustavo_Petro+
               Rodolfo_Hernandez+
               En_Blanco)%>%
      mutate(across(Gustavo_Petro:En_Blanco, ~ ./Total))%>%
      mutate(Gustavo_Petro=Gustavo_Petro/Total,
             Rodolfo_Hernandez=Rodolfo_Hernandez/Total,
             En_Blanco=En_Blanco/Total)
    
    Petro<-TS%>%
      select(Date, Gustavo_Petro)
    colnames(Petro)=c('ds','y')
    head(Petro)
    ModelPetro<-prophet(Petro)
    FuturePetro<-make_future_dataframe(ModelPetro, periods=9)
    tail(FuturePetro)
    ForecastPetro<-predict(ModelPetro, FuturePetro)
    #prophet_plot_components(ModelPetro, ForecastPetro)
    #View(ForecastPetro)
    dyplot.prophet(ModelPetro, ForecastPetro)
    
    Hernandez<-TS%>%
      select(Date, Rodolfo_Hernandez)
    Hernandez
    colnames(Hernandez)=c('ds','y')
    head(Hernandez)
    ModelHernandez<-prophet(Hernandez)
    FutureHernandez<-make_future_dataframe(ModelHernandez, periods=9)
    tail(FutureHernandez)
    ForecastHernandez<-predict(ModelHernandez, FutureHernandez)
    #View(ForecastHernandez)
    #dyplot.prophet(ModelHernandez, ForecastHernandez)
    Blanco<-TS%>%
      select(Date, En_Blanco)
    Blanco
    colnames(Blanco)=c('ds','y')
    head(Blanco)
    ModelBlanco<-prophet(Blanco)
    FutureBlanco<-make_future_dataframe(ModelBlanco, periods=9)
    tail(FutureBlanco)
    ForecastBlanco<-predict(ModelBlanco, FutureBlanco)
    #View(ForecastBlanco)
    #dyplot.prophet(ModelBlanco, ForecastBlanco)
    DF<-TS%>%
      filter(!row_number() %in% c(1:nrow(TS)))%>%
      add_row(Gustavo_Petro=ForecastPetro$yhat[nrow(ForecastPetro)], 
              Rodolfo_Hernandez=ForecastHernandez$yhat[nrow(ForecastHernandez)],
              En_Blanco=ForecastBlanco$yhat[nrow(ForecastBlanco)])%>%
      pivot_longer(cols=contains("_"), names_to="nombre", values_to="Int_voto")%>%
      mutate(Candidato=case_when(nombre=="Gustavo_Petro"~"Gustavo Petro", 
                                 nombre=="Rodolfo_Hernandez"~"Rodolfo Hernández",
                                 nombre=="En_Blanco"~"Voto en Blanco")%>%
               factor())%>%
      group_by(Candidato)%>%
      summarize(Predicción=Int_voto*100)%>%
      arrange(desc(Predicción))
    DF$Predicción[DF$Predicción<0]<-0
    scale<-function(x) (x/sum(x))*100
    DF<-DF%>%
      mutate(across(!Candidato, scale))
    DFCIupp<-TS%>%
      filter(!row_number() %in% c(1:nrow(TS)))%>%
      add_row(Gustavo_Petro=ForecastPetro$yhat_upper[nrow(ForecastPetro)], 
              Rodolfo_Hernandez=ForecastHernandez$yhat_upper[nrow(ForecastHernandez)],
              En_Blanco=ForecastBlanco$yhat_upper[nrow(ForecastBlanco)])%>%
      pivot_longer(cols=contains("_"), names_to="nombre", values_to="Int_voto")%>%
      mutate(Candidato=case_when(nombre=="Gustavo_Petro"~"Gustavo Petro", 
                                 nombre=="Rodolfo_Hernandez"~"Rodolfo Hernández",
                                 nombre=="En_Blanco"~"Voto en Blanco")%>%
               factor())%>%
      group_by(Candidato)%>%
      summarize(CISup=Int_voto*100)%>%
      arrange(desc(CISup))
    DFCIupp$CISup[DFCIupp$CISup<0]<-0
   # DFCIupp<-DFCIupp%>%
  #    mutate(across(!Candidato, scale))
    DFCIlow<-TS%>%
      filter(!row_number() %in% c(1:nrow(TS)))%>%
      add_row(Gustavo_Petro=ForecastPetro$yhat_lower[nrow(ForecastPetro)], 
              Rodolfo_Hernandez=ForecastHernandez$yhat_lower[nrow(ForecastHernandez)],
              En_Blanco=ForecastBlanco$yhat_lower[nrow(ForecastBlanco)])%>%
      pivot_longer(cols=contains("_"), names_to="nombre", values_to="Int_voto")%>%
      mutate(Candidato=case_when(nombre=="Gustavo_Petro"~"Gustavo Petro", 
                                 nombre=="Rodolfo_Hernandez"~"Rodolfo Hernández",
                                 nombre=="En_Blanco"~"Voto en Blanco")%>%
               factor())%>%
      group_by(Candidato)%>%
      summarize(CIInf=Int_voto*100)%>%
      arrange(desc(CIInf))
    DFCIlow$CIInf[DFCIlow$CIInf<0]<-0
 #   DFCIlow<-DFCIlow%>%
#      mutate(across(!Candidato, scale))
    dflist[[i]]=DF
    dflowlist[[i]]=DFCIlow
    dfupplist[[i]]=DFCIupp
  }
  Result<-join_all(dflist, by="Candidato",type = "left", match = "all")
  namesdf=c("Candidato")
  for(i in 1:S){
    namesdf[[i+1]] <- paste("Simulation", i, sep = "")}
  colnames(Result)<-namesdf
  
  
  Result<-Result%>%
    mutate(Promedio=rowMedians(as.matrix(Result[,2:ncol(Result)])))%>%
    arrange(desc(Promedio))%>%
    mutate(Pronostico=signif(Promedio, digits=4))
  
  Result<-Result%>%
    select(c(Candidato, Pronostico))
  
  Resultinf<-join_all(dflowlist, by="Candidato",type = "left", match = "all")
  namesdf=c("Candidato")
  for(i in 1:S){
    namesdf[[i+1]] <- paste("Simulation", i, sep = "")}
  colnames(Resultinf)<-namesdf
  
  
  Resultinf<-Resultinf%>%
    mutate(CIInf=rowMedians(as.matrix(Resultinf[,2:ncol(Resultinf)])))%>%
    arrange(desc(CIInf))%>%
    mutate(CIInf=signif(CIInf, digits=4))
  
  Resultinf<-Resultinf%>%
    select(c(Candidato, CIInf))
  
  Resultsup<-join_all(dfupplist, by="Candidato",type = "left", match = "all")
  namesdf=c("Candidato")
  for(i in 1:S){
    namesdf[[i+1]] <- paste("Simulation", i, sep = "")}
  colnames(Resultsup)<-namesdf
  
  
  Resultsup<-Resultsup%>%
    mutate(CISup=rowMedians(as.matrix(Resultsup[,2:ncol(Resultsup)])))%>%
    arrange(desc(CISup))%>%
    mutate(CISup=signif(CISup, digits=4))
  
  Resultsup<-Resultsup%>%
    select(c(Candidato, CISup))
  Result<-Result%>%
    left_join(Resultinf)%>%
    left_join(Resultsup)
  
  return(Result)
}
Simresult=SVSimulation(5000)

Simresulttablets<-Simresult%>%
  mutate(Votos=formatC(2173157*Pronostico/100, digits = 0, format = "f"))%>%
  mutate(VotosInf=formatC(2173157*CIInf/100, digits = 0, format = "f"))%>%
  mutate(VotosSup=formatC(2173157*CISup/100, digits = 0, format = "f"))
Simresulttablets<-Simresulttablets%>%
  kable("html", 
        digits=2,
        caption = "Predicción: % y votos por candidato") %>% 
  kable_styling(full_width = F) %>% 
  footnote(number = c("Autor: Sergio Calvo","Twitter: @Scalvo25","Fecha pronóstico: 2022-06-14"))%>%
  kable_paper() %>%
  save_kable(file = "tableaverageSV.html", self_contained = T)


dataplot <- data.frame(Candidato <-Simresult$Candidato,
                       F <- Simresult$Pronostico,
                       L <- Simresult$CIInf,
                       U <- Simresult$CISup)

ggplot(dataplot, aes(x = reorder(Candidato, -F), y = F, colour=Candidato)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = U, ymin = L))+scale_color_manual(values=c("purple", "orange", "grey"))+
  ggtitle("Pronóstico Segunda Vuelta Presidencial Colombia 2022 (Prophet)")+
  ylab("Porcentaje")+xlab("Candidato")+labs(fill = "Candidato")+
  theme(axis.text=element_text(size=6))
ggsave("SegVuelta.png",width = 20, height = 20, units = "cm")



dfgraph<-clean_df %>%
  pivot_longer(cols=contains("_"), 
               names_to="Candidato", values_to="Voto")%>%
  mutate(Candidato=case_when(Candidato=="Gustavo_Petro"~"Gustavo Petro", 
                             Candidato=="Rodolfo_Hernandez"~"Rodolfo Hernández",
                             Candidato=="En_Blanco"~"Voto en Blanco")%>%
           factor())%>%
  mutate(Voto=Voto*100)
#View(dfgraph)
plot<-ggplot(dfgraph, aes(as.Date(Date), Voto, colour = Candidato)) +
  geom_point() +
  geom_smooth(level=0.95)
plot+scale_color_manual(values=c("purple", "orange", "grey"))+
  ggtitle("Encuestas Segunda Vuelta 2022")+
  ylab("Porcentaje")+xlab("Fecha")+labs(fill = "Candidato")
ggsave("GraphPollsSV.png",width = 20, height = 20, units = "cm")


