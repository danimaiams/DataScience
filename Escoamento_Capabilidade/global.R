source("C:/Users/Administrator/Desktop/R DanielMaia/Escoamento_Capabilidade/dados.R")

library(plyr)

#Funções de uso geral
#Função para gerar resumo estatístico com total de pontos e desvio padrão
SummaryComSD <- function(vector, na.rm = FALSE, dp_round = 2)
{ results <- c(summary(vector), 
               'Std.Dev' = ifelse(is.numeric(vector)=='TRUE',
                                  round(sd(vector, na.rm), dp_round),
                                  0),
               'Count'   = NROW(vector))

return(results)
}
