#'Trabalho final 
#'Fernanda Cunha 

#1 carregando pacotes


library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)
library(scatterplot3d)
library(ggfortify)



#2 Importando dados 

dados <- read.csv("dados/dados.csv")

#3 Inspecionando o conjunto de dados

str(dados)

#4criando conjunto de dados 

dadosLSC <- dados %>%
  select(LSC,ospan,simon)%>%
  droplevels()


#5 visualizacao grafica dos dados por meio de graficos de dispersao

ggplot(dados, aes(x = ospan, y = LSC)) +
  geom_point(alpha = 0.5, outlier.size = 2) +
  labs(x= "Ospan task", y= "LSC (conectividade)") +
  theme_bw()

ggplot(dados, aes(x = simon, y = LSC)) +
  geom_point(alpha = 0.5, outlier.size = 2) +
  labs(x= "Simon task", y= "LSC (conectividade)") +
  theme_bw()

scatterplot3d(x=dados$ospan,      ## achei bonito
              y=dados$LSC, 
              z=dados$simon,
              pch = 20,
              angle = 50, 
              type = 'h')
      
    
        
#6 estatistica descritiva da variavel resposta 


mean(dados$LSC)
sd(dados$LSC)
range(dados$LSC)
median(dados$LSC)


#'7 ## modelo estatistico
#'
#' pergunta: como memoria de trabalho e controle inibitorio predizem LSC?

##ajuste de modelo 

modelo <- lm(LSC ~ ospan*simon, dados)

##coeficientes do modelo

summary(modelo) 

################################################################################

#8 ajuste de modelo sem interacao >> 

modelo2 <- lm(LSC ~ ospan+simon, dados)

#9 comparacao entre os modelos

anova(modelo2, modelo)


# diagnosticos do modelo

autoplot(modelo) ## homocedasticidade 
vif(modelo)      ## VIF > 10, ha colinearidade  
shapiro.test(modelo$residuals)

################################################################################

#' Escrita dos resultados 
#' 
#' jdsfhgsdjhgfjhsdgfjsdhfd
#' fjhsdgjshdfjdhfadnbnmsdnfbmnd
#' asjdhfbjshbfjahssdfbsdnmfbnsm



