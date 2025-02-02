####### Construindo o modelo ########

# Vou fazer um modelo de Regressão Logística
# A variável resposta segue uma distribuição binomial onde 1 = pos e 0 = neg
# Y = caminhões que apresentam problemas no sistema de ar
--------------------------------------------------------------------------------
  
# Dataset de treino
treino <- anos_ant
  
# Dataset de teste
teste <- base2020
  
semproblemas <- treino[treino$class == "neg", ]
View(semproblemas)
# Verificar a quantidade de caminhões "sem" problemas no sistema de ar
semproblemas %>% 
  count(class)
  
problemas_ar <- treino[treino$class == "pos", ]
View(problemas_ar)
# Verificar a quantidade de caminhões "com" problemas no sistema de ar
problemas_ar %>% 
  count(class)
  
--------------------------------------------------------------------------------
#Y = 1 (Possui problemas no sistema de ar) (binomial)
#Y = 0 (Possui problemas em outro lugar)
    
## Stepwise 
    
## O modelo é melhor quando o AIC e BIC são menores
    
#Plotar todos os dados
    
#Opção 1 que eu usei
modelo1 <- glm(class ~ .,data = treino, family = binomial) 
stepAIC(modelo1)
summary(modelo1)
##Se beta1(estimate) é positivo, contribui de forma positiva na probabilidade
##Se for negativo, o aumento dessa variavel, diminui a probabilidade de apresentar problemas de ar
  
#Opção 2
modelo <- glm(class~. , data = treino, family = binomial) %>%
  stepAIC(trace = FALSE)
summary(modelo)
  
# Fazendo predições
probabilidades <- modelo1 %>% predict(teste, type = "response")
predicao_class <- ifelse(probabilidades > 0.5, "pos", "neg")
  
# Acurácia do modelo
mean(predicao_class == teste$class)
  
# Modelo de regressão logística completa
modelo_completo <- glm(class ~ ., data = treino, family = binomial) # Creio que por conta da quantidade de variáveis muito altas, travou meu R, mesmo assim desenvolvi o código
coef(modelo_completo)
  
#### Selecionar as variáveis que mais contribuem no modelo ####
  
library(MASS)
modelo_step <- modelo_completo %>% stepAIC(trace = FALSE)
coef(modelo_step)
  
####### Calculando a precisão do modelo de regressão logistica completo ##########
  
## Fazendo predições
probabilidades <- modelo_completo %>% predict(teste, type = "response")
predicao_class <- ifelse(probabilidades > 0.5, "pos", "neg")
  
## Acurácia da predição
class_observado <- teste$class
mean(predicao_class == class_observado)
  
####### Calculando a precisão do modelo de regressão logistica stepwise ##########
  
## Fazendo predições
probabilidades2 <- predict(modelo_step,teste, type = "response")
predicao_class2 <- ifelse(probabilidades > 0.5, "pos", "neg")
  
## Acurácia da predição
class_observado2 <- teste$class
mean(predicao_class == class_observado)
  

#### Usando amostra de teste para ver se o modelo é bom mesmo
  
modelo1_teste <- glm(class ~ . ,data = treino, family = binomial()) # todas as variaveis(exceto cd_000) são siginificativas pelo p-valor < 0.05
summary(modelo1_teste)
  
pred.Teste <- predict(modelo1, teste, type = "response") #Vai dar a probabilidade de cada caminhão apresentar defeitos no sistema de ar
teste$Prob_defeito <- pred.Teste
  
