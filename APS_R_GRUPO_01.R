library(plotly)
library(tidyverse)
library(readxl)
library(stringr)
library(ggthemes)
require(gridExtra)
 
#Importando as bases de dados

dados <- read.csv("ocorrencias_registradas.csv",
                  fileEncoding = "utf-8") %>% 
  filter(ano<2021)
pop <- read_excel("total_populacao_sao_paulo.xlsx")

View(dados)
View(pop)

#Definindo a coluna id_municipio como numeric

pop <- pop %>% 
  mutate(id_municipio=as.integer(id_municipio))

#Selecionando apenas as colunas de interesse da base pop

pop <- pop %>% 
  select(id_municipio,nome_municipio,pop_2010)

#Agregando as duas bases

dados <- dados %>% 
  left_join(pop,by="id_municipio")

#Selecionando as colunas de interesse da base final
dados <- dados %>% 
  select(-c(regiao_ssp,
            numero_de_vitimas_em_homicidio_doloso,
            numero_de_vitimas_em_homicidio_doloso_por_acidente_de_transito,
            numero_de_vitimas_em_latrocinio)) %>% 
  mutate(ocorrencias_tot = select(.,homicidio_doloso:furto_de_veiculo) %>% 
           rowSums(na.rm = TRUE))

view(dados)

##############################################################################
#Evoluçao dos crimes totais no Estado de Sao Paulo

grafico1 <- dados %>% 
  group_by(ano) %>%
  summarise(ocorrencias_tot_ano = sum(ocorrencias_tot)/1000000) %>% 
  ggplot()+
  geom_line(aes(ano,ocorrencias_tot_ano),color = "darkred", size = 1) +
    labs(x = "Ano",
         y = "Ocorrencias totais (em milhares)",
         title = "Evoluçao das Ocorrencias") +
    scale_y_continuous(n.breaks = 10) +
    scale_x_continuous(n.breaks = 12) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

ggplotly(grafico1,tooltip="ocorrencias_tot_ano")

#Vendo a relação entre roubos de veiculos e ocorrencias totais

grafico2 <- dados %>% 
  group_by(ano) %>%
  summarise(roubos_veic_tot = sum(roubo_de_veiculo)/1000) %>% 
  ggplot()+
  geom_line(aes(ano,roubos_veic_tot),color = "darkblue", size = 1) +
  labs(x = "Ano",
       y = "Roubos de veículos (em milhares)",
       title = "Evoluçao dos roubos de veiculo") +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 12) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

ggplotly(grafico2,tooltip="roubos_veic_tot")

#Comparando 

grid.arrange(grafico1,grafico2,ncol=2,nrow=1)

###############################################################################
#Vendo as cidades mais perigosas em termos per capita do Estado de SP com bases
# em ocorrencias totais

cid_mais_perig <- dados %>% 
  group_by(ano,nome_municipio) %>% 
  summarise(ocorrencias_tot_pop = sum(ocorrencias_tot)/pop_2010*1000) %>% 
  ggplot()+
  geom_line(aes(ano,ocorrencias_tot_pop, 
                group = nome_municipio, color = nome_municipio)) +
  labs(x = "Ano",
       y = "Ocorrencias totais por mil habitantes",
       title = "Evolução das Ocorrencias por mil habitantes") +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 15) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

ggplotly(cid_mais_perig,tooltip = c("nome_municipio","ocorrencias_tot_pop"))

#Analise comparativa da ocorrencia dos crimes entre a cidade mais violenta 
#e a menos violenta

# Descobrindo quais são as cidades mais violentas e menos violentas
dados %>% 
  group_by(nome_municipio) %>% 
  summarize(ocorr_hist = sum(ocorrencias_tot)) %>% 
  left_join(pop,by="nome_municipio") %>% 
  select(.,-id_municipio) %>% 
  mutate(violencia = ocorr_hist/pop_2010) %>% 
  arrange(violencia)

#Ilha Comprida
ilhacomprida <- dados %>% 
  select(-c(mes,id_municipio,pop_2010,ocorrencias_tot)) %>% 
  pivot_longer(-c(ano,nome_municipio),
               names_to="Crime",
               values_to="Ocorrencias") %>% 
  group_by(nome_municipio,ano,Crime) %>% 
  summarize(Ocorrencias = sum(Ocorrencias)) %>% 
  filter(nome_municipio == 'Ilha Comprida') 


ilhacomprida$Crime <- ilhacomprida$Crime %>% 
  str_replace_all("_"," ") %>% 
  str_to_title()

ilhacomprida$Crime[which(ilhacomprida$Ocorrencias <20)] <- "Outros"

#Dobrada
dobrada <- dados %>% 
  select(-c(mes,id_municipio,pop_2010,ocorrencias_tot)) %>% 
  pivot_longer(-c(ano,nome_municipio),
               names_to="Crime",
               values_to="Ocorrencias") %>% 
  group_by(nome_municipio,ano,Crime) %>% 
  summarize(Ocorrencias = sum(Ocorrencias)) %>% 
  filter(nome_municipio == 'Dobrada') 


dobrada$Crime <- dobrada$Crime %>% 
  str_replace_all("_"," ") %>% 
  str_to_title()

dobrada$Crime[which(dobrada$Ocorrencias < 5)] <- "Outros"

#Grafico

fig <- plot_ly()

fig<- fig %>% add_pie(data = ilhacomprida %>% 
                        mutate(Crime = fct_lump(Crime,5)),
                      labels=~Crime,
                      values=~Ocorrencias,
                      name = "Ilha Comprida",
                      title = 'Ilha Comprida',
                      domain = list(row = 0, column = 0),
                      legendgroup=~Crime)

fig <- fig %>% add_pie(data = dobrada %>% 
                         mutate(Crime = fct_lump(Crime,5)),
                       labels=~Crime,
                       values=~Ocorrencias,
                       name = "Dobrada",
                       title = "Dobrada",
                       domain = list(row = 0, column = 1))

fig <- fig %>% layout(title = "Ilha Comprida x Dobrada",
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                   showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                   showticklabels = FALSE))

fig

###############################################################################
##Estado de SP

#2010
estado2010 <- dados %>% 
  select(-c(mes,nome_municipio,id_municipio,pop_2010,ocorrencias_tot)) %>% 
  pivot_longer(-c(ano),
               names_to="Crime",
               values_to="Ocorrencias") %>% 
  group_by(ano,Crime) %>% 
  summarize(Ocorrencias = sum(Ocorrencias)) %>% 
  filter(ano == 2010) 

estado2010$Crime <- estado2010$Crime %>% 
  str_replace_all("_"," ") %>% 
  str_to_title()

estado2019$Crime[which(estado2010$Ocorrencias < 5000)] <- "Outros"

#2020
estado2020 <- dados %>% 
  select(-c(mes,nome_municipio,id_municipio,pop_2010,ocorrencias_tot)) %>% 
  pivot_longer(-c(ano),
               names_to="Crime",
               values_to="Ocorrencias") %>% 
  group_by(ano,Crime) %>% 
  summarize(Ocorrencias = sum(Ocorrencias)) %>% 
  filter(ano == 2020) 

estado2020$Crime <- estado2020$Crime %>% 
  str_replace_all("_"," ") %>% 
  str_to_title()

estado2020$Crime[which(estado2020$Ocorrencias < 5000)] <- "Outros"

#Grafiico
fig1 <- plot_ly()

fig1<- fig1 %>% add_pie(data = estado2010,
                      labels=~Crime,
                      values=~Ocorrencias,
                      name = "2010",
                      title = '2010',
                      domain = list(row = 0, column = 0),
                      legendgroup=~Crime)

fig1 <- fig1 %>% add_pie(data = estado2020,
                       labels=~Crime,
                       values=~Ocorrencias,
                       name = "2020",
                       title = "2020",
                       domain = list(row = 0, column = 1))

fig1 <- fig1 %>% layout
(title = "Distribuiçao dos Crimes no Estado de Sao Paulo 2010 x 2020",
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                   showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                   showticklabels = FALSE))

fig1

##############################################################################
#Pandemia: Analise dos crimes ao longo dos meses -> 2019 x 2020
mes2019<-dados %>% 
  filter(ano==2019) %>% 
  select(-c(ano,id_municipio,ocorrencias_tot,pop_2010)) %>% 
  pivot_longer(-c(mes,nome_municipio),
               names_to="Crime",
               values_to="Ocorrencias") %>% 
  group_by(mes) %>% 
  summarize(Ocorrencias = sum(Ocorrencias))

mes2020<-dados %>% 
  filter(ano==2020) %>% 
  select(-c(ano,id_municipio,ocorrencias_tot,pop_2010)) %>% 
  pivot_longer(-c(mes,nome_municipio),
               names_to="Crime",
               values_to="Ocorrencias") %>% 
  group_by(mes) %>% 
  summarize(Ocorrencias = sum(Ocorrencias))


graficomes2019 <- mes2019 %>%
  ggplot()+
        geom_bar(aes(mes,Ocorrencias), fill = "darkblue", stat="identity")+
        theme_set(theme_bw())+
        scale_x_continuous(n.breaks=11)+
        labs(x = "Mes",
             y = "Ocorrencias",
             title='2019')+
  ylim(c(0,125000))+
  scale_y_continuous(n.breaks = 8) +
  scale_x_continuous(n.breaks = 13) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

graficomes2020 <- mes2020 %>%
  ggplot()+
  geom_bar(aes(mes,Ocorrencias),fill = "black", stat="identity")+
  theme_set(theme_bw())+
  scale_x_continuous(n.breaks=11)+
  labs(x = "Mes",
       y = "",
       title='2020')+
  ylim(c(0,125000))+
  scale_y_continuous(n.breaks = 8) +
  scale_x_continuous(n.breaks = 13) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

grid.arrange(graficomes2019, graficomes2020, nrow = 1, 
             top="Distribuiçao da quantidade de crimes ao longo dos meses")


###############################################################################
#Regressao

#Analise para entender de que maneira o numero de roubos de carro influencia
#o numero de homicidios culposos no transito 
#(devido a acidentes com morte durante a fulga dos crimes)

cor(dados$homicidio_culposo_por_acidente_de_transito,
    dados$roubo_de_veiculo)
model <- lm(homicidio_culposo_por_acidente_de_transito ~ roubo_de_veiculo,
            data=dados) 
summary(model)




