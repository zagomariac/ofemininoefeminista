###### regressão logistica binaria da pesquisa de zago, 2026 - o feminino e feminista ######
##### carregando pacotes e preparando o banco #####
library(readxl)
library(ResourceSelection)
library(pscl)
library(car)
library(ggplot2)
library(dplyr)
library(patchwork)
library(marginaleffects)

dados <- read_excel("C:\\Users\\DRT904284\\Documents\\arquivos locais R\\BANCO 4 - FINAL.xlsx")

#mudando o tipo de variavel
glimpse(dados)

dados$gen <- factor(dados$gen)
dados$espec <- factor(dados$espec)
dados$posic <- factor(dados$posic)
dados$relig <- factor(dados$relig)
dados$raca <- factor(dados$raca)

#categorias de referencia (obs.: serao alteradas abaixo)
levels(dados$gen) #1 (mulher) 
levels(dados$espec) #centro-direita 
levels(dados$posic) #0 (desfav)
levels(dados$relig) #cristao 
levels(dados$raca) #amarela 

#mudando categoria de referência
dados$gen <- relevel(dados$gen, ref = "4")
dados$posic <- relevel(dados$posic, ref = "1")
dados$relig <- relevel(dados$relig, ref = "FPE, FPC, FPMA")
dados$raca <- relevel(dados$raca, ref = "BRANCA")

##### estimando modelos #####

mod1 <- glm(posic ~ gen + espec + relig,
            data = dados,
            family = binomial(link = "logit")
            ) # Obs.: raça foi tirada por apresentar 254 NAs

mod11 <- glm(posic ~ gen + espec + relig + raca,
                   data = dados,
                   family = binomial(link = "logit")
)

##### checagem dos pressupostos #####

#multicolinearidade pelo teste de VIF - Variance Inflation Factor

vif(mod1) 
vif(mod11) #todos abaixo de 2 - não há multicolinearidade em nenhum modelo

#presença de outliers
#resíduos padronizados
rstandard(mod1)
which(abs(rstandard(mod1)) > 3) #nenhuma observação abaixo de 3
which(abs(rstandard(mod1)) > 2) #algumas observações abaixo de 2 que sugerem alguma discrepância no modelo

rstandard(mod11)
which(abs(rstandard(mod11)) > 3) #duas observaçoes abaixo de 3
which(abs(rstandard(mod11)) > 2) #algumas observações abaixo de 2, mas menos que no modelo 1

#leverage
lev_mod1 <- hatvalues(mod1)
k_lev_mod1 <- length(coef(mod1))   #parâmetros
n_lev_mod1 <- nrow(dados)          #observações
(2*k_lev_mod1)/n_lev_mod1 #0.02432432
max(lev_mod1) #0.08993184 - há observações influentes
summary(lev_mod1)
limite_lev_mod1 <- (2 * k_lev_mod1) / n_lev_mod1
limite_lev_mod1
obs_lev_alto <- which(lev_mod1 > limite_lev_mod1)
obs_lev_alto
length(obs_lev_alto)

lev_mod11 <- hatvalues(mod11)
k_lev_mod11 <- length(coef(mod11))   #parâmetros
n_lev_mod11 <- nrow(dados)          #observações
(2*k_lev_mod11)/n_lev_mod11 #0.03243243
max(lev_mod11) #1 - há observações extremamente influentes
which(lev_mod11 == 1) #0
print(max(lev_mod11), digits = 15) #0.999999996203546 valor da observação perto de 1
which.max(lev_mod11)
dados[which.max(lev_mod11), ]

#distância de Cook
cook1 <- cooks.distance(mod1)
which(cook1 > (4/nrow(dados)))
summary(cook1)
max(cook1) #o valor máximo (0.06246015) é muito menor do que 0.5, portanto não há observações excessivamente influentes

cook11 <- cooks.distance(mod11)
which(cook11 > (4/nrow(dados)))
max(cook11) #124402866 - há observações excessivamente influentes

##### resultados #####
summary(mod1)

#odds ratio
exp(coef(mod1))
exp(confint(mod1)) #variáveis significativas (cujo intervalo não passou por 1): gen 1, espec extrema-direita

#Tabela com coeficiente, erro padrão, p-valor, odds ratio
resultados_mod1 <- data.frame(
  coeficiente = coef(mod1),
  erro_padrao = summary(mod1)$coefficients[, "Std. Error"],
  p_valor = summary(mod1)$coefficients[, "Pr(>|z|)"],
  odds_ratio = exp(coef(mod1)))

##### probab predita de cada perfil #####
novo <- data.frame(
  gen = factor("4", levels = levels(dados$gen)),
  espec = factor("centro-direita", levels = levels(dados$espec)),
  relig = factor("cristao", levels = levels(dados$relig))
)

predict(mod1, newdata = novo, type = "response")

#visualização:
ggplot(dados_mod1, aes(x = gen, y = pred)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_minimal()