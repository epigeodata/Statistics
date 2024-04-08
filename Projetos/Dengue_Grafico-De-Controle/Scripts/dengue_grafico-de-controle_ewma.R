#função que instala se necessário e carrega os dados
install_library <- function(package){
  if(!require(package, character.only = TRUE)) install.packages(package); require(package, character.only = TRUE)
}

#pacote do gráfico de controle
install_library("qcc")

#---- dados de dengue
library(readr)
#diretorio
setwd("C:/Users/belsd/Desktop/Epigeodata/DENGUE")

#lendo
dados <- read_delim("casos_dengue_geral.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(GEOCODIGO = col_character()), 
                     trim_ws = TRUE)

#transfomando em tipo data
dengue$DATA = as.Date(paste0(dengue$DATA, "-01"), format = "%Y-%b-%d")

#classe do objeto "dados"
class(dados)

#colocando um indice pra agrupar mes-ano e transformando em dataframe
dados <- dados %>% 
  mutate(indice = rep(seq_len(ceiling(n() / 184)), each = 184, length.out = n())) %>%
  as.data.frame()

#desctiva
summary(dados$N_CASOS)
boxplot(dados$N_CASOS)

#vendo a observação com o maior número de casos
dados %>% filter(N_CASOS==max(N_CASOS))

#vendo a observação com o maior taxa
dados %>% filter(TX_DENGUE==max(TX_DENGUE))
#classe do objeto "dados"
class(dados) 

#--------- analise de controle
attach(dados)

dengue_table <- qcc.groups(N_CASOS, indice)
q <- ewma(dengue_table, lambda=0.5, nsigmas=2.7)
summary(q)

detach(dados)

#---- taxa de dengue por mes e ano
dengueAgrupado =  dados %>% as.data.frame()%>% 
  group_by(DATA)

#agrupando e tirando a media da taxa
dengue_mes_ano = dengueAgrupado %>% 
  summarise(cum_casos = sum(N_CASOS),
            tx_dengue_media = mean(TX_DENGUE),
            .groups = 'drop') %>% 
  distinct(select = DATA, .keep_all = TRUE)
#verificando se tem NA
anyNA(dengue_mes_ano)
dengue_mes_ano
dim(dengue_mes_ano)

#---------- GRAFICO DE CONTROLE EWMA
#Create an object of class 'ewma.qcc' to compute and 
#draw an Exponential Weighted Moving Average (EWMA) chart 
#for statistical quality control.

#fazendo o grafico de controle dos casos acumulados e da taxa media.
#sigma = 2.7 (faixa de 95%)
#lambda = 0.8 #parametro de suavizacao
q <-  ewma(dengue_mes_ano$cum_casos, lambda=0.8, nsigmas=2.7)
q <-  ewma(dengue_mes_ano$tx_dengue_media, lambda=0.8, nsigmas=2.7)

