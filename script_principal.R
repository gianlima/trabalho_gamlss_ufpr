
# INSERÇÃO DOS DADOS ------------------------------------------
library(readxl)
library(gamlss)
library(car)
# Inserção de dados
dados <- read_excel("dados_gamlss.xlsx")

# LIMPEZA DOS DADOS -------------------------------------------
# Retirar código e nome das cidades
dados <- dados[,-c(1,2)]
# Troca de nome das variáveis, inserção de letras
# Variável resposta do modelo: g
names(dados) <- letters[1:9]
# Conferir se tem NA
anyNA(dados)

# ANÁLISE RÁPIDA ----------------------------------------------
# Conferir a correlação, retirado a variável i do modelo
require(corrplot)
corrplot.mixed(cor(dados), upper = "ellipse")

# Gráficos de dispersão de todas as variáveis
scatterplotMatrix(dados) 

# CRIAÇÃO DO MODELO -----------------------------------------------
# Seleção de covariáveis, retirando a variável i
# por ter correlação acima de 0.8
ajuste <- step(gamlss(g ~ . -i, data = dados, 
                      family = NO), direction = 'backward') 

# Resumo do modelo
summary(ajuste)

# Gráfico wormplot
wp(ajuste, ylim.all = 3) # o que significa esse ylim.all, testar outros
# VIF
vif(ajuste)

