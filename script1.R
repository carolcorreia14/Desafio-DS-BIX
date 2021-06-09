# Instalando os pacotes necessários
install.packages("glmnet")
install.packages("ranger")
install.packages("tidymodels")
install.packages("vip")
install.packages("forcats")
install.packages("ISwR")
install.packages("MASS")

# Carregando as bibliotecas
library(glmnet)
library(tidymodels)
library(ranger)
library(vip)
library(forcats)
library(ISwR)
library(MASS)

######## Importando os datasets ########
base2020 <- read.csv("~/Desafio - BIX Tecnologia/PS datasets/base_vigente_2020.csv")
anos_ant <- read.csv("~/Desafio - BIX Tecnologia/PS datasets/base_vigente_anos_anteriores.csv")

-----------------------------------------------------------------------------------------------------
  
# Visualizar quantas linhas e quantas colunas têm cada dataset
dim(base2020)
dim(anos_ant)

# Ver quais são os tipos das variáveis
glimpse(base2020)
glimpse(anos_ant)

-----------------------------------------------------------------------------------------------------
  
############ Transformando a variável "class" em fator e recodificando ############

### base de teste ####
# Opção 1 substituindo pos "pos" por 1 e "neg" por 0
#base2020$class <- ifelse(base2020$class=="pos",1,0)
#base2020$class <- as.factor(base2020$class)

#Opção 2 que eu irei utilizar
levels(base2020$class) <- c("neg","pos")
base2020$class <- relevel(base2020$class,"neg")
View(base2020)

### base de treino ###
# Opção 1 substituindo pos "pos" por 1 e "neg" por 0
#anos_ant$class <- ifelse(anos_ant$class=="pos",1,0)
#anos_ant$class <- as.factor(anos_ant$class)

#Opção 2 que eu irei utilizar
levels(anos_ant$class) <- c("neg","pos")
anos_ant$class <- relevel(anos_ant$class,"neg")
View(anos_ant)

#### Transformando "na" em missing values ####
base2020[base2020 == "na"] <- NA 
glimpse(base2020)

anos_ant[anos_ant == "na"] <- NA
glimpse(anos_ant)

########### Transformando as variáveis character em numérica ###########

base2020 <- base2020 %>%
  mutate_if(is.character,as.numeric)

anos_ant <- anos_ant %>%
  mutate_if(is.character,as.numeric)

--------------------------------------------------------------------------------
############# Tratando missing values ################

######### base de teste ########## 
is.na(base2020)

bs2020 <- na.omit(base2020) # vai apagar todos os NAs para ver com quantas linhas ficaria
nrow(base2020)
nrow(bs2020) 

View(base2020)

#Percentual de missing values
NAs <- round(colSums(is.na(base2020))*100/nrow(base2020), 2)
NAs[NAs >0]

colSums(is.na(base2020)) # quantos missing values por coluna

#Excluindo colunas que possuem mais de 50% dos valores sendo missing
NAs[NAs > 50]

base2020$ab_000 <- NULL
base2020$bm_000 <- NULL
base2020$bn_000 <- NULL
base2020$bo_000 <- NULL
base2020$bp_000 <- NULL
base2020$bq_000 <- NULL
base2020$br_000 <- NULL
base2020$cr_000 <- NULL

#Substituir os NAs pela mediana(pois os dados variam muito) para colunas cuja proporção de NAs foi menor que 50% 
summary(base2020)
NAs[NAs < 50]

for(i in 2:ncol(base2020)){
  base2020[,i][is.na(base2020[,i])] <- median(base2020[,i], na.rm = TRUE)
}

View(base2020)

#Verificar se tem NAs no banco
any(is.na(base2020))

############## base de treino ##################

is.na(anos_ant)

ans_ant <- na.omit(anos_ant) # vai apagar todos os NAs para ver com quantas linhas ficaria
nrow(anos_ant)
nrow(ans_ant)

View(anos_ant)

#Percentual de missing values
NAs2 <- round(colSums(is.na(anos_ant))*100/nrow(anos_ant), 2)
NAs2[NAs2 >0]

colSums(is.na(anos_ant)) # quantos missing values por coluna

# Excluindo as colunas que possuem mais de 50% de missing values
NAs2[NAs2 > 50]

anos_ant$ab_000 <- NULL
anos_ant$bm_000 <- NULL
anos_ant$bn_000 <- NULL
anos_ant$bo_000 <- NULL
anos_ant$bp_000 <- NULL
anos_ant$bq_000 <- NULL
anos_ant$br_000 <- NULL
anos_ant$cr_000 <- NULL

#Substituir os NAs pela mediana(pois os dados variam muito) para colunas cuja proporção de NAs foi menor que 50% 
summary(anos_ant)
NAs2[NAs2 < 50]

for(i in 2:ncol(anos_ant)){
  anos_ant[,i][is.na(anos_ant[,i])] <- median(anos_ant[,i], na.rm = TRUE)
}

View(anos_ant)

#Verificar se tem NAs no banco
any(is.na(anos_ant))

--------------------------------------------------------------------------
# Contar quantos pos e neg temos em cada dataset
base2020 %>% 
  count(class)

anos_ant %>% 
  count(class)

## Explorando os dados
names(base2020)
names(anos_ant)

summary(base2020)
summary(anos_ant)















