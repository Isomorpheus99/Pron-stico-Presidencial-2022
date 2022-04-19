#Pronóstico Elecciones Presidenciales Colombia 2022
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
setwd("C:/Users/sergi/OneDrive/Desktop/42/Work/Research/Election Forecasting/Colombia/Presidential")
df<-read_xlsx("DataCol22.xlsx", sheet="Polls")
clean_df<-df %>%
  select(-c(Source, Link, Total)) %>%
  mutate(John_Rodriguez=as.numeric(John_Rodriguez), Enrique_Gomez=as.numeric(Enrique_Gomez),
         Luis_Perez=as.numeric(Luis_Perez), None=as.numeric(None), Uncertain=as.numeric(Uncertain))%>%
    replace_na(list(John_Rodriguez=0, Enrique_Gomez=0, Luis_Perez=0, None=0, Uncertain=0)) %>%
  mutate(Undefined=None+Uncertain+Blanco, 
         En_Blanco=0.0246,
         Null=0.0175,
         Vote=1-Undefined+En_Blanco,
         voteava=1-Vote)
view(clean_df)
set.seed(10)
random<-runif(8)
random<-random/sum(random)
dfadjust<-clean_df %>%
  mutate(Gustavo_Petro=Gustavo_Petro+random[1]*voteava, 
         Fico_Gutierrez=Fico_Gutierrez+random[2]*voteava, Sergio_Fajardo=Sergio_Fajardo+random[3]*voteava, 
         Rodolfo_Hernandez=Rodolfo_Hernandez+random[4]*voteava, Ingrid_Betancourt=Ingrid_Betancourt+random[5]*voteava, 
         Enrique_Gomez=Enrique_Gomez+random[6]*voteava,
         John_Rodriguez=John_Rodriguez+random[7]*voteava, Luis_Perez=Luis_Perez+random[8]*voteava)
view(dfadjust)

dffinal<-dfadjust %>%
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
  summarize(Predicción=weighted.mean(Int_ajus_voto, Weight)*100)%>%
  arrange(desc(Predicción))%>%
  kable("html", 
        digits=1,
        caption = "Predicción: % votos por candidato") %>% 
  kable_styling(full_width = F) %>% 
  footnote(number = c("Cocinero: Sergio Calvo","Twitter: @Scalvo25","Fecha pronóstico: 2022-11-04"))%>%
  kable_paper() %>%
  save_kable(file = "table1.html", self_contained = T)
dffinal

##Gráfico pronóstico
dffinal1<-dfadjust %>%
  pivot_longer(cols=contains("_"), 
               names_to="nombre", values_to="Int_ajus_voto") %>%
  mutate(Candidato=case_when(nombre=="Gustavo_Petro"~"Petro", 
                             nombre=="Fico_Gutierrez"~"Gutiérrez", 
                             nombre=="Sergio_Fajardo"~"Fajardo",
                             nombre=="Rodolfo_Hernandez"~"Hernández",
                             nombre=="John_Rodriguez"~"Rodríguez",
                             nombre=="Luis_Perez"~"Pérez",
                             nombre=="Enrique_Gomez"~"Gómez",
                             nombre=="Ingrid_Betancourt"~"Betancourt",
                             nombre=="En_Blanco"~"V.B")%>%
           factor())%>%
  group_by(Candidato)%>%
  summarize(Predicción=signif((weighted.mean(Int_ajus_voto, Weight)*100), digits=4))%>%
  arrange(desc(Predicción))
ggplot(data=dffinal1, aes(x=reorder(Candidato, -Predicción), y=Predicción, fill=Candidato)) +
  geom_bar(stat="identity")+scale_fill_manual(values=c("darkgreen", "green", "darkblue", "blue", "orange", "yellow", "purple", "brown", "grey"))+
  geom_text(aes(label=Predicción), vjust=-0.3, size=3.5)+
  theme_minimal()+ggtitle("Predicción Primera Vuelta Elecciones 2022")+xlab("Candidato")+
  ylab("Porcentaje")+theme(legend.position="none")
ggsave("Forecast.png",width = 20, height = 20, units = "cm")

##Gráfico de encuestas
dfgraph<-dfadjust %>%
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
view(dfgraph)
plot<-ggplot(dfgraph, aes(Date, Voto, colour = Candidato)) +
  geom_point() +
  geom_smooth(level=0.8)
plot+scale_color_manual(values=c("blue", "darkblue", "purple", "darkgreen", "brown", "yellow", "orange", "green", "grey"))+
  ggtitle("Encuestas Post-consultas Primera Vuelta 2022")+
  ylab("Porcentaje")+xlab("Fecha")+labs(fill = "Candidato")
ggsave("GraphPolls.png",width = 20, height = 20, units = "cm")

