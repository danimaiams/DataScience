source("C:/Users/Administrator/Desktop/R DanielMaia/Escoamento_Capabilidade/db.R")

#Busca dos fitros no Banco de Dados
# Diametro, parede, grau, AQA -> pla_ordem_producao
# hf1, tf1, tf2 -> qt_qts.res_tubo_sofia_aust
# Data -> dth_criacao_reg do qt_qts.res_tubo_austenitizacao

# componentes químicos -> dwa_res_lims_corrida


query <- 
  "select 
cod_ordem_producao as Ordem,
dim_ext_tubo as Diametro,
esp_par_tubo as Parede,
cod_aqa as AQA,
tmo_ciclo_plan as Ciclo,
dth_criacao_reg as Data,
dsc_aco as Grau,

val_lim_escoamento as LE,
val_tensao_residual as TR
from
QT_QTS.PLA_ORDEM_PRODUCAO

where DIM_EXT_TUBO = 244.48
and esp_par_tubo = 11.99
and VAL_LIM_ESCOAMENTO != 0

order by DTH_CRIACAO_REG desc"

df <- dbGetQuery(
  connection_reportUser,
  query
)
df

tabela_saida = df

valor_le = df$LE

valor_tr = df$TR

relacao_ts = (valor_le/valor_tr)
#------------------------------------Calculo pra Capabilidade---------------------
capabilidade_LE <- valor_le
capabilidade_LE = matrix(valor_le, nrow = 1, ncol = length(valor_le), byrow = TRUE)
normalidade_LE = matrix(valor_le, nrow = length(valor_le), ncol = 1, byrow = TRUE)


capabilidade2 <- relacao_ts
capabilidade2 = matrix(relacao_ts, nrow = 1, ncol = length(relacao_ts), byrow = TRUE)
normalidade2 = matrix(relacao_ts, nrow = length(relacao_ts), ncol = 1, byrow = TRUE)


