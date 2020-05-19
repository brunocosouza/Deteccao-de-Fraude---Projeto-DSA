# Deteccao-de-Fraude---Projeto-DSA
Projeto de detecção de Fraude em R do Curso de R com Azure da DSA. O projeto aborda todas as etapas de analise de dados, 
desde o pré processamento até a criação e validação de um modelo preditivo.

O risco de fraude está em toda parte, mas para as empresas que anunciam
online, a fraude de cliques pode acontecer em um volume avassalador,
resultando em dados de cliques enganosos e dinheiro desperdiçado. Os canais de
anúncios podem aumentar os custos simplesmente quando pessoas ou bots
clicam nos anúncios em grande escala, o que na prática não gera o resultado
esperado. Com mais de 1 bilhão de dispositivos móveis em uso todos os meses, a
China é o maior mercado móvel do mundo e, portanto, sofre com grandes
volumes de tráfego fraudulento.

A TalkingData (https://www.talkingdata.com), a maior plataforma de Big Data independente da China,
cobre mais de 70% dos dispositivos móveis ativos
em todo o país. Eles lidam com 3 bilhões de cliques por dia, dos quais 90% são
potencialmente fraudulentos. Sua abordagem atual para impedir fraudes de
cliques para desenvolvedores de aplicativos é medir a jornada do clique de um
usuário em todo o portfólio e sinalizar endereços IP que produzem muitos cliques,
mas nunca acabam instalando aplicativos. Com essas informações, eles criaram
uma lista negra de IPs e uma lista negra de dispositivos.

O projeto visa a criação de um modelo preditivo para analise detecção 
de fraudes. O dataset esta disponivel no Kaggle - Talking Data Adtracking Fraud
Detection. 

O projeto foi realizado todo em linguagem R. O dataset contém 8 variaveis (incluindo a target) 
e 100000 observações. 
O dataset foi divido em dados de treino e de teste e foi realizado um modelo preditivo supervisionado. 
Durante esse projeto foi utilizado o algoritmo Random Florest, do pacote randomflorest do CRAN, para a Feature Selection.
Os dados então foram treinando, também, com random florest e avaliados com os dados de teste. A otimização
ocorreu introduzindo uma função de custo ao modelo. E a avalição final ocorreu com a criação dos graficos ROC
do pacote pROC do CRAN.
