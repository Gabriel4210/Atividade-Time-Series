#Atividade 2 - Séries Temporáis
#############################################################
#Lendo a base
base=read.csv("https://raw.githubusercontent.com/Gabriel4210/Atividade-Time-Series/main/atv2.csv", sep = ";")

#Carregando pacotes
library(forecast)
library(ggplot2)

#Transformando em ts e visualizando
tsbase=ts(base$refrigerantes,frequency=12, start=c(1970,1))
plot(tsbase)

#Dividindo em treino e teste
tstreino=ts(tsbase[c(1:36)],frequency=12, start=c(1970,1))
tsteste=ts(tsbase[c(37:48)],frequency=12, start=c(1973,1))

#Ajustando Modelo
fit=ets(tstreino, model = ("MNM"))
summary(fit)
plot(fit)

#Comparar predito e real
predito = fit %>% forecast(h=12)

predito$mean
tsteste

#Fazer o plot da comparação
time=rep(0,12)
for(i in 1:11){
time[1]=1973
time[i+1] = 1973+1/12*(i)
}

fit %>% forecast(h=12) %>%
  autoplot() +
  geom_line(aes(x=(time),y=tsteste), colour = "red")+
  xlab("Ano")+
  ylab("Vendas de refrigerantes")+
  labs(caption = "Vermelho = Valores reais
       Azul = Valores Preditos")

#Medidas de comparação
RMSE = sum((as.numeric(predito$mean-tsteste))^2)/12

