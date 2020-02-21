# Carregar pacotes
library(magrittr)
library(tidyverse)
library(survey)
library(srvyr)
library(DBI)


library(readr)
# Carregar a coleta inicial do caged por cbo

cbos_caged <- read_delim("C:/Users/faabr/Downloads/cbos_caged.txt", 
                         ";", escape_double = FALSE, 
                         trim_ws = TRUE)

library(readr)
cbos_rais <- read_delim("C:/Users/faabr/Downloads/cbos_rais.txt", 
                        ";", escape_double = FALSE, col_types = cols(cbo2002ocupacao = col_number(), 
                                                                     cnae20subclas = col_number()), trim_ws = TRUE)
View(cbos_rais)

cbos_rais<-cbos_rais %>% 
rename(cbo =cbo2002ocupacao)

cbos_rais<-as.numeric(cbos_rais,"cnae_subclasse")

dplyr::left_join(pmad_dom_2018 %>% 
                   dplyr::select(-c(A01npessoas,referencia,municipio,A01setor,A01nficha_ant)),
                 by=c("A01nficha"="A01nficha")) %>% 


raiscaged<-left_join(cbos_rais,cbos_caged, by=c("cnae_subclasse","cbo"))

raisproj2017<-cbos_rais %>%
 dplyr::filter(referencia==2017)

raisproj2008<-raisproj2008 %>% 
  dplyr::mutate(case_when(competencia== 2008 ~'2008-12-31'))

raisproj2008<-raisproj2008 %>% 
  rename(competencia = `case_when(competencia == 2008 ~ "2008-12-31")`)

caged2008= caged2008[,-5]

raisproj2008$`case_when(competencia == 2008 ~ "2008-12-31")`


raisproj2007<-raisproj2007 %>% 
group_by(competencia,cbo,cnae_subclasse) %>% 
  summarise(vinculos = sum(vinculoativo3112, na.rm = FALSE))

cbos_rais<-cbos_rais %>% 
  group_by(referencia,cbo,cnae_subclasse) %>% 
  summarise(vinculos = sum(vinculoativo3112, na.rm = FALSE))

caged<-caged %>% 
  mutate(competencia=as.Date(`Competência Declarada`))

caged2008<-caged2008 %>% 
  rename(vinculos = saldo)

raisalt<-rais %>% gather("referencia","vinculo",2:13)
raisdes<-raisalt %>% spread(cnae,vinculo)

rais<-rais %>%
  rename('2009-12-31' ='2009') %>% 
  rename('2010-12-31'= '2010') %>%
  rename('2011-12-31' ='2011') %>%
  rename('2012-12-31'= '2012') %>%
  rename('2013-12-31' ='2013') %>%
  rename('2014-12-31'= '2014') %>%
  rename('2015-12-31' ='2015') %>%
  rename('2016-12-31'= '2016') %>%
  rename('2017-12-31' ='2017') %>%
  rename('2018-12-31'= '2018')

raisdes<-raisdes %>% 
  rename(competencia=referencia)

caged<-caged %>% 
  rename(competencia=`Competência Declarada`)
  
caged = as.data.frame(caged)

dados<-merge(raisdes,caged)

total <- rbind(raisdes, caged)

total<-total %>% 
   mutate(ompetencia=as.Date(competencia))


cbos_caged<-cbos_caged %>% 
  group_by(competencia,cbo,cnae_subclasse) %>% 
  summarise(saldo = sum(saldo_movimentacao, na.rm = FALSE))

caged2008<-cbos_caged %>%
  dplyr::filter(ano==2018)

raisproj2007<-as.data.frame(raisproj2007)
cbos_caged$ano=substr(cbos_caged$competencia,1,4)

caged2008<-as.data.frame.character(caged2008)

lapply(raiscaged, function(x)  ggseasonplot(ts(x, freq=12, start=c(2007,12), end=c(2019,12)))+
                                            theme(plot.title = element_text(hjust = 0.5)) +  # to center the plot title
                                              ylab("projeção de vínculos ativos") +
                                              ggtitle(names(raiscaged)))
                                            

                                  



proj2008 = merge(raisproj2007,caged2008, by=c("competencia","cbo","cnae_subclasse"))

filter()cbo<-caged_cbo %>%
  filter(competencia>'2006-01-01')

# tranforma a competencia em data
cbo<-caged_cbo %>% 
  mutate(competencia=as.Date(competencia))

#Soma os saldo dentro e fora do prazo apresentados
cbo <-cbo %>%
  group_by(competencia,cbo) %>% 
  summarise(saldo_movimentaçao = sum(saldo_movimentacao, na.rm = FALSE))

#transforma as cbos em colunas
frame_cbo<- cbo %>% spread(competencia,saldo_movimentaçao)

#Substitui os valores "NA" por 0
frame_cbo[is.na(frame_cbo)] <- 0

####Saldo Acumulado###

#criando um vetor com as referencias
a<-c(frame_cbo$competencia)
#transformado em data
frame_cbo<-as.data.frame.character(frame_cbo)

frame_cbo$cbo2002ocupacao = (frame_cbo$cbo)


#loop de saldo de acumulado em 12 meses para todas as colunas, incluido-as no vetor a
for (i in names(frame_cbo)){
  
  a[[paste(i)]] = zoo::rollsum(frame_cbo[[i]], 12, fill = NA, align = "right")
}

#retirando a coluna que acumula as competências (kkk)
a<-a[-2]

B<-c(frame_cbo$competencia)
B<-as.data.frame(B)

for (i in names(frame_cb)){
  
  B[[paste(i)]] = cumsum(frame_cbo[[i]])
}

cbo <- rais_cbo[,3]

#retorna a tabela para o formato inicial(cbos como variaveis)#
data<-a %>%
  gather("cbo", "saldo", 3:2412)
teste$media=forecast::ma(Cbo_serie, order=12, centre=TRUE)

teste$as = (teste$saldo_movimentaçao- teste$media)

teste$mes = (substr(teste$competencia,6,7))


sazonalidade<-teste %>% 
  select(as, mes) %>%
  group_by(mes) %>% 
  summarise(coef = mean(as, na.rm = TRUE))

sazonalidade<-as.data.frame(sazonalidade)

summarise(saldo_movimentaçao = sum(saldo_movimentacao, na.rm = FALSE))

sazonalidade<- spread(sazonalidade,mes,as)

qw<-left_join(teste,sazonalidade, by= "mes")

td<-right_join(rais_cbo,frame_cbo, by= "cbo2002ocupacao")



asd<-teste$competencia
teste<-as.data.frame(teste)
#Teste para uma CBO
#Filtra dados para apenas uma cbo(vendedor), e soma os saldo dentro e fora do prazo apresentados
teste<-caged_cbo %>%
  srvyr::filter(cbo==521110) %>% 
  group_by(competencia,cbo) %>% 
  summarise(saldo_movimentaçao = sum(saldo_movimentacao, na.rm = FALSE))

#pacotes séries históricas
#install.packages("forecast")
#install.packages("fpp2")
#install.packages("readxl")

library(forecast)
library(fpp2)
library(readxl)


#Filtra dados para apenas uma cbo(vendedor), e soma os saldo dentro e fora do prazo apresentados
teste<-data %>%
  srvyr::filter(cbo== 521110) %>% 
  group_by(a) %>% 
  summarise(saldo_movimentaçao = sum(saldo, na.rm = FALSE))


#Transformando um Data Frame em uma Série Temporal

Cbo_serie = ts(data = teste$saldo_movimentaçao, start = c(2006,1),frequency = 12)
Cbo_serie
plot.ts(Cbo_serie)
autoplot(Cbo_serie)
d = decompose(Cbo_serie, type = c("additive"), filter = NULL)

plot(d)
tenden<-Cbo_serie- c
is.na(c)<-0
dia<-Cbo_serie-d$seasonal
plot.ts(dia)
c$c
tempo=1:168
tempo
modelo=lm(Cbo_serie~tempo)
summary(modelo)

plot(Cbo_serie)
plot(modelo)





qwe<-d$trend
plot(qwe)
art<-decompose(dia)
plot(art)
-----
  #Agora que temos uma série temporal, vamos plotar:
  forecast::autoplot(Cbo_serie)
#Observando um gráfico sazonal:
ggseasonplot(Cbo_serie)
#Observando um gráfico sazonal "polar":
ggseasonplot(Cbo_serie, polar = T)

------
  ggsubseriesplot(Cbo_serie)
previsao = ses(Cbo_serie, h = 12) # Previsao para os próximos 12 meses
summary(previsao)
autoplot(previsao)
------
  modelo_arima = forecast::auto.arima(Cbo_serie)

autoplot(forecast(modelo_arima))

library("seasonal")
ajuste<-seasonal::seas(Cbo_serie)

plot(ajuste)
z
checkX13()
#xts, zoo, TTR, forecast, quantmod and tidyquant
#https://www.pedronl.com/post/previsao-de-series-temporais-com-o-r/


d$trend
write.table(c,
            "/u01/u104409/c.csv",
            sep = ";",
            dec = ",",
            fileEncoding = "latin1",
            row.names = F,
            na="")


write.table(total,"total.csv",
            row.names = F, sep = ";", dec = '.')

