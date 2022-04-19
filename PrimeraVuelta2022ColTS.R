#Pronóstico Elecciones Presidenciales Colombia 2022
#Time Series
#Mayo 2022
#Load libraries
# R
rm(list=ls(all=TRUE))
# We recommend running this is a fresh R session or restarting your current session
library(prophet)
library(magrittr)
library(readxl)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(ggplot2)
#Load data
##FBProphet
setwd("C:/Users/sergi/OneDrive/Desktop/42/Work/Research/Election Forecasting/Colombia/Presidential")
TS<-read_xlsx("DataCol22.xlsx", sheet="TimeSeries")
TS<-TS%>%
  mutate(En_Blanco=0.0246,
         Undefined=Blanco+None+Uncertain,
         Vote=1-Undefined+En_Blanco,
         Voteava=1-Vote)

random<-runif(8)
random<-random/sum(random)
TS<-TS %>%
  mutate(Gustavo_Petro=Gustavo_Petro+random[1]*Voteava, 
         Fico_Gutierrez=Fico_Gutierrez+random[2]*Voteava, Sergio_Fajardo=Sergio_Fajardo+random[3]*Voteava, 
         Rodolfo_Hernandez=Rodolfo_Hernandez+random[4]*Voteava, Ingrid_Betancourt=Ingrid_Betancourt+random[5]*Voteava, 
         Enrique_Gomez=Enrique_Gomez+random[6]*Voteava,
         John_Rodriguez=John_Rodriguez+random[7]*Voteava, Luis_Perez=Luis_Perez+random[8]*Voteava)
view(TS)
TSgraph<-TS %>%
  pivot_longer(cols=contains("_"), 
               names_to="Candidato", values_to="Voto")%>%
  mutate(Candidato=case_when(Candidato=="Gustavo_Petro"~"Gustavo Petro", 
                             Candidato=="Fico_Gutierrez"~"Federico Gutiérrez", 
                             Candidato=="Sergio_Fajardo"~"Sergio Fajardo",
                             Candidato=="Rodolfo_Hernandez"~"Rodolfo Hernández",
                             Candidato=="John_Rodriguez"~"John M. Rodríguez",
                             Candidato=="Luis_Perez"~"Luis Pérez",
                             Candidato=="Enrique_Gomez"~"Enrique Gómez",
                             Candidato=="Ingrid_Betancourt"~"Ingrid Betancourt",
                             Candidato=="En_Blanco"~"Voto en Blanco")%>%
           factor())%>%
  mutate(Voto=Voto*100)
view(TSgraph)
plot<-ggplot(TSgraph, aes(Date, Voto, colour = Candidato)) +
  geom_point() +
  geom_smooth()
plot+scale_color_manual(values=c("blue", "darkblue", "purple", "darkgreen", "brown", "yellow", "orange", "green", "grey"))+
  ggtitle("Encuestas Post-consultas Primera Vuelta 2022")+
  ylab("Porcentaje")+xlab("Fecha")+labs(fill = "Candidato")
ggsave("GraphPollsTS.png",width = 20, height = 20, units = "cm")

####fbprophet
currentdate=Sys.Date()
electiondate=as.Date("2022-05-29")
daystoelect=as.numeric(electiondate-currentdate)
TS<-read_xlsx("DataCol22.xlsx", sheet="TimeSeries")
TS<-TS%>%
  mutate(En_Blanco=0.0246,
         Undefined=Blanco+None+Uncertain,
         Vote=1-Undefined+En_Blanco,
         Voteava=1-Vote)

random<-runif(8)
random<-random/sum(random)
TS<-TS %>%
  mutate(Gustavo_Petro=Gustavo_Petro+random[1]*Voteava, 
         Fico_Gutierrez=Fico_Gutierrez+random[2]*Voteava, Sergio_Fajardo=Sergio_Fajardo+random[3]*Voteava, 
         Rodolfo_Hernandez=Rodolfo_Hernandez+random[4]*Voteava, Ingrid_Betancourt=Ingrid_Betancourt+random[5]*Voteava, 
         Enrique_Gomez=Enrique_Gomez+random[6]*Voteava,
         John_Rodriguez=John_Rodriguez+random[7]*Voteava, Luis_Perez=Luis_Perez+random[8]*Voteava)
Petro<-TS%>%
  select(Date, Gustavo_Petro)
colnames(Petro)=c('ds','y')
head(Petro)
ModelPetro<-prophet(Petro)
FuturePetro<-make_future_dataframe(ModelPetro, periods=daystoelect)
tail(FuturePetro)
ForecastPetro<-predict(ModelPetro, FuturePetro)
View(ForecastPetro)
dyplot.prophet(ModelPetro, ForecastPetro)

Fico<-TS%>%
  select(Date, Fico_Gutierrez)
colnames(Fico)=c('ds','y')
head(Fico)
ModelFico<-prophet(Fico)
FutureFico<-make_future_dataframe(ModelFico, periods=daystoelect)
tail(FutureFico)

ForecastFico<-predict(ModelFico, FutureFico)
View(ForecastFico)
dyplot.prophet(ModelFico, ForecastFico)
Fajardo<-TS%>%
  select(Date, Sergio_Fajardo)
Fajardo
colnames(Fajardo)=c('ds','y')
head(Fajardo)
ModelFajardo<-prophet(Fajardo)
FutureFajardo<-make_future_dataframe(ModelFajardo, periods=daystoelect)
tail(FutureFajardo)
ForecastFajardo<-predict(ModelFajardo, FutureFajardo)
View(ForecastFajardo)
dyplot.prophet(ModelFajardo, ForecastFajardo)

Hernandez<-TS%>%
  select(Date, Rodolfo_Hernandez)
Hernandez
colnames(Hernandez)=c('ds','y')
head(Hernandez)
ModelHernandez<-prophet(Hernandez)
FutureHernandez<-make_future_dataframe(ModelHernandez, periods=daystoelect)
tail(FutureHernandez)
ForecastHernandez<-predict(ModelHernandez, FutureHernandez)
View(ForecastHernandez)
dyplot.prophet(ModelHernandez, ForecastHernandez)
Betancourt<-TS%>%
  select(Date, Ingrid_Betancourt)
Betancourt
colnames(Betancourt)=c('ds','y')
head(Betancourt)
ModelBetancourt<-prophet(Betancourt)
FutureBetancourt<-make_future_dataframe(ModelBetancourt, periods=daystoelect)
tail(FutureBetancourt)
ForecastBetancourt<-predict(ModelBetancourt, FutureBetancourt)
View(ForecastBetancourt)
dyplot.prophet(ModelBetancourt, ForecastBetancourt)
Hurtado<-TS%>%
  select(Date, Enrique_Gomez)
Hurtado
colnames(Hurtado)=c('ds','y')
head(Hurtado)
ModelHurtado<-prophet(Hurtado)
FutureHurtado<-make_future_dataframe(ModelHurtado, periods=daystoelect)
tail(FutureHurtado)
ForecastHurtado<-predict(ModelHurtado, FutureHurtado)
View(ForecastHurtado)
dyplot.prophet(ModelHurtado, ForecastHurtado)
Perez<-TS%>%
  select(Date, Luis_Perez)
Perez
colnames(Perez)=c('ds','y')
head(Perez)
ModelPerez<-prophet(Perez)
FuturePerez<-make_future_dataframe(ModelPerez, periods=daystoelect)
tail(FuturePerez)
ForecastPerez<-predict(ModelPerez, FuturePerez)
View(ForecastPerez)
dyplot.prophet(ModelPerez, ForecastPerez)
Rodriguez<-TS%>%
  select(Date, John_Rodriguez)
Rodriguez
colnames(Rodriguez)=c('ds','y')
head(Rodriguez)
ModelRodriguez<-prophet(Rodriguez)
FutureRodriguez<-make_future_dataframe(ModelRodriguez, periods=daystoelect)
tail(FutureRodriguez)
ForecastRodriguez<-predict(ModelRodriguez, FutureRodriguez)
View(ForecastRodriguez)
dyplot.prophet(ModelRodriguez, ForecastRodriguez)
Blanco<-TS%>%
  select(Date, En_Blanco)
Blanco
colnames(Blanco)=c('ds','y')
head(Blanco)
ModelBlanco<-prophet(Blanco)
FutureBlanco<-make_future_dataframe(ModelBlanco, periods=daystoelect)
tail(FutureBlanco)
ForecastBlanco<-predict(ModelBlanco, FutureBlanco)
View(ForecastBlanco)
dyplot.prophet(ModelBlanco, ForecastBlanco)

DF<-TS%>%
  filter(!row_number() %in% c(1:nrow(TS)))%>%
  add_row(Gustavo_Petro=ForecastPetro$yhat[nrow(ForecastPetro)], 
          Fico_Gutierrez=ForecastFico$yhat[nrow(ForecastFico)],
          Sergio_Fajardo=ForecastFajardo$yhat[nrow(ForecastFajardo)],
          Rodolfo_Hernandez=ForecastHernandez$yhat[nrow(ForecastHernandez)],
          Ingrid_Betancourt=ForecastBetancourt$yhat[nrow(ForecastBetancourt)],
          John_Rodriguez=ForecastRodriguez$yhat[nrow(ForecastRodriguez)],
          Enrique_Gomez=ForecastHurtado$yhat[nrow(ForecastHurtado)],
          Luis_Perez=ForecastPerez$yhat[nrow(ForecastPerez)],
          En_Blanco=ForecastBlanco$yhat[nrow(ForecastBlanco)])%>%
  pivot_longer(cols=contains("_"), names_to="nombre", values_to="Int_voto")%>%
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
  summarize(Predicción=Int_voto*100)%>%
  arrange(desc(Predicción))%>%
  kable("html", 
        digits=1,
        caption = "Predicción: % votos por candidato") %>% 
  kable_styling(full_width = F) %>% 
  footnote(number = c("Cocinero: Sergio Calvo","Twitter: @Scalvo25","Fecha pronóstico: 2022-11-04"))%>%
  kable_paper() %>%
  save_kable(file = "table2.html", self_contained = T)
