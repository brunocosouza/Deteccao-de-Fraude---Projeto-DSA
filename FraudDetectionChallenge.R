#Mini Projeto de Detecçao de Fraudes - DSA

#Diretorio do Projeto
setwd("C:/FCD/1-BigDataRAzure/ProjetoFeedBack/Projeto1")
getwd()
options(warn=-1)

#Pacotes utilizados
library(readr)
library(dplyr)
library(tidyr)
library("corrgram")
library(normalize)
library(lubridate)
library(BBmisc)

#Carregando os dados
# dataset_submission <- read_csv("C:/FCD/1-BigDataRAzure/ProjetoFeedBack/Projeto1/data/sample_submission.csv")
# View(dataset_submission)
dataset_train <- read_csv("C:/FCD/1-BigDataRAzure/ProjetoFeedBack/Projeto1/data/train_sample.csv")

#Visualizar os dados
head(dataset_train)
str(dataset_train)

#----------------------------------------Data fields-----------------------------------------------------
#Each row of the training data contains a click record, with the following features.

# ip: ip address of click.
# app: app id for marketing.
# device: device type id of user mobile phone (e.g., iphone 6 plus, iphone 7, huawei mate 7, etc.)
# os: os version id of user mobile phone
# channel: channel id of mobile ad publisher
# click_time: timestamp of click (UTC)
# attributed_time: if user download the app for after clicking an ad, this is the time of the app download
# is_attributed: the target that is to be predicted, indicating the app was downloaded
#Note that ip, app, device, os, and channel are encoded.

#The test data is similar, with the following differences:
# click_id: reference for making predictions
# is_attributed: not included

#Checando NA
apply(dataset_train, 2, function(x) any(is.na(x)))

#Quantidades de IP, devices, app, channel e ip.
apply(dataset_train,2,function(x) length(unique(x)))


#Criando variaveis diarias
#dataset_train$Dia_Semana <- weekdays(dataset_train$Data)
dataset_train <- separate(dataset_train, col = 'click_time', into = c('data','horario'), sep = ' ')
dataset_train$Dia_Semana <- wday(dataset_train$data)
dataset_train$horario <- hour(as.POSIXct(dataset_train$horario
                                           , format = c("%H:%M:%S")))
head(dataset_train)

#Deletando a variavel attributed time visto que a maioria de seus valores sao NA.
dataset_train$attributed_time <- NULL
dataset_train$data <- NULL
head(dataset_train)

#Analisando a distribuição de dados
table(as.factor(dataset_train$is_attributed))
print("Dados Desbalanceados")

#Graficos de analise
hist(dataset_train$horario, xlab = "Horarios" , main =  "Histograma dos horarios")
hist(dataset_train$Dia_Semana, xlab = "Horarios" , main =  "Histograma dos Dias das Semanas")
hist(x = dataset_train$ip, xlab = "IP" , main =  "Histograma dos IPs")

ggplot(dataset_train, aes(x = as.factor(horario), y = as.factor(is_attributed) ,fill=factor(is_attributed))) +
  geom_bar(stat="identity",besides= T)
ggplot(dataset_train, aes(x = as.factor(Dia_Semana), y = as.factor(is_attributed) ,fill=factor(is_attributed))) +
  geom_bar(stat="identity",besides= T)




                                                  #VERIFICACAO DAS VARIAVEIS

#Grafico de correlação
var <- c("ip", "app", "device", "os", "channel", "Dia_Semana", "is_attributed", "horario" )
corrgram(dataset_train[,var])

#Verificaçao
dim(dataset_train)
apply(dataset_train, 2, function(x) any(is.na(x)))

#Colocando a variavel target como fator
dataset_train$is_attributed <- as.factor(dataset_train$is_attributed)
str(dataset_train)

# Criando um modelo para identificar os atributos com maior importância para o modelo preditivo
require(randomForest)


# Avalidando a importância de todas as variaveis
modelo <- randomForest(is_attributed ~ .,
                        data = dataset_train, 
                        ntree = 100, nodesize = 10, importance = T)


# Plotando as variáveis por grau de importância
varImpPlot(modelo)
modelo$importance


#Separando dados de treino e de teste
# Funcao para gerar dados de treino e dados de teste
splitData <- function(dataframe) {
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset = trainset, testset = testset)
}

# Gerando dados de treino e de teste
splits <- splitData(dataset_train)

# Separando os dados
dados_treino <- splits$trainset
dados_teste <- splits$testset

#Criando Modelo
# Construindo o modelo
library(randomForest)
modelo <- randomForest(is_attributed ~ .,
                       data = dados_treino, 
                       ntree = 150, nodesize = 10)
print(modelo)


# Previsoes e analise
previsoes <- data.frame(observado = dados_teste$is_attributed,
                        previsto = predict(modelo, newdata = dados_teste))

table(previsoes)
prop.table(table(previsoes),2)

# Optimizacao do projeto

# Criando uma Cost Function
# Colocando um custo mais pesado caso de um falso positivo
Cost_func <- matrix(c(0, 0.5, 1, 0), nrow = 2, dimnames = list(c("1", "2"), c("1", "2")))



modelo_v2 <- randomForest(is_attributed ~ . -device -Dia_Semana,
                       data = dados_treino,
                       cost = Cost_func,
                       ntree = 150, nodesize = 10)

print(modelo_v2)

# Previsoes e analise
# Dataframes com valores observados e previstos Modelo V2
previsoes2 <- data.frame(observado = dados_teste$is_attributed,
                         previsto = predict(modelo_v2, newdata = dados_teste))
table(previsoes2)
prop.table(table(previsoes2),2)
# Observamos que os TP aumentaram consideravelmente dado a quantidade de positivos nos dados.

#C50 Modelo para tentar optimizacao
require(C50)
modelo_C50 <- C5.0(is_attributed ~ .,
                   data = dados_treino,
                   trials = 100,
                   cost = Cost_func)
plot(modelo_C50)

# Previsoes e analise
# Dataframes com valores observados e previstos Modelo V2
previsoes_c50 <- data.frame(observado = dados_teste$is_attributed,
                         previsto = predict(modelo_C50, newdata = dados_teste))
table(previsoes_c50)
prop.table(table(previsoes_c50),2)



library(pROC)
pROC_obj <- roc(as.numeric(previsoes$observado),as.numeric(previsoes$previsto),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)
print(pROC_obj)

pROC_obj2 <- roc(as.numeric(previsoes2$observado),as.numeric(previsoes2$previsto),
                 smoothed = TRUE,
                 # arguments for ci
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                 # arguments for plot
                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                 print.auc=TRUE, show.thres=TRUE)
print(pROC_obj2)

pROC_objc50 <- roc(as.numeric(previsoes_c50$observado),as.numeric(previsoes_c50$previsto),
                 smoothed = TRUE,
                 # arguments for ci
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                 # arguments for plot
                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                 print.auc=TRUE, show.thres=TRUE)
print(pROC_objc50)

  # Bibliografias Usadas
  # https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
  # https://www.rdocumentation.org/
  # DSA cursos e scripts
  # https://rviews.rstudio.com/2019/01/17/roc-curves/
  # https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
  # https://cran.r-project.org/web/packages/C50/vignettes/C5.0.html


#                                                               ANEXO
# PROGRAMACOES CRIADAS AO LONGO DO EXERCICIO PARA ANALISE
# TENTATIVA DE CRIAR UMA VARIAVEL PARA AUMENTAR A PRECISAO DA PREVISAO
#========================================================================================================================
          #Verificamos que o dataset é muito mal distribuido, portanto poderemos ou balancear os dados ou tentar uma função de custo
          
          #Recuperando os valores fraudulentos para analisar se ha um padrão
          # Download_app <- dataset_train %>% filter(is_attributed==1) %>%select( ip,app, device, os, channel, Data, Horario,is_attributed)
          # str(Download_app)
          # View(Download_app)
          
          # NotDownload_app <- dataset_train %>% filter(is_attributed==0) %>%select( ip,app, device, os, channel, Data, Horario,is_attributed)
          # str(NotDownload_app)
          # View(NotDownload_app)
          
          #Avaliação da quantidade de download e não Downloads a partir do IP, APP, Channel
          #dataset_train %>% select(-click_time, -attributed_time, -device, -os)%>% group_by_all() %>% summarise(COUNT = n())  %>% as.data.frame() %>% View
          
          #tabela com agrupada apenas pelo ip, app e channel
          #Juncao <-  dataset_train  %>%  group_by(ip, app, channel) %>% count() %>% data.frame()
          
          
          # Criaremos uma variavel que contabiliza a uniao das variaveis IP, APP, e Channel e a Target
          # Decisao foi tomada apos analise das junções da etapa anterior
          #dataset_train$Var_Juncao <- rep(1,nrow(dataset_train))
          
          # Unindo as variaveis
          #dataset_train <- dataset_train %>% unite("unite", c('ip','app','channel'), sep='', remove = FALSE)
          #Juncao <- Juncao %>% unite("unite", c('ip','app','channel'), sep='', remove = FALSE)
          #View(Juncao)
          
          
          #Verificamos que alguns Unite sao identicos para quem fez download e quem nao fez, logo temos
          #valores nao distintos, que darao problema na etapa seguinte
          #Recuperando os Unites dos valores duplicados
          #duplicador <- Juncao %>% group_by(unite) %>% count() %>% filter(n>1) %>% select(unite) 
          
          
          # Somando a contagem dos valores duplicados e recuperando,apos, apenas o distintos
          #for (i in duplicador$unite){
          #  Juncao[Juncao$unite == i,'n'] = Juncao[Juncao$unite == i,'n'][1]+ Juncao[Juncao$unite == i,'n'][2]
          #  print(Juncao[Juncao$unite == i,'n'])
          #}
          
          # Juncao <- Juncao %>% distinct()
          
          # Adicionando a quantiade de vezes que a Unite se repete e adicionar no dataframe
          # OBS : Se tiver alguma funçao tipo um "map" que verifica cada valor de uma dataframe 
          # a todos os valores do outro dataframe e case de match, retornar outro valor, voces poderiam me falar? Eu nao consegui encontrar
          # Obrigado
          #for (i in Juncao$unite){
          #  dataset_train[i == dataset_train$unite,'Var_Juncao'] = rep(Juncao[i == Juncao$unite,'n'],1)
          #}
          
          #dataset_train$unite <- NULL
          
          #Normalizando as variaveis
          #var2 <- c("ip", "app", "device", "os", "channel")
          #dataset_train[,var2] <- normalize(dataset_train[,var2], method = "standardize", range = c(0, 1), margin = 2)

#========================================================================================================================
