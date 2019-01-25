#Configuração para conexão com o servidor de histórico
#Jeceaba
library("ROracle")

Sys.setenv(TZ = "GMT-3")
Sys.setenv(ORA_SDTZ = "GMT-3")

driver <- dbDriver("Oracle")
host <- "10.211.4.200"
port <- 1521
sid <- "hist01"

connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))",
  sep = ""
)

connection <- dbConnect(
  driver, username = "DWA_ODS",
  password = "DWA_ODS",
  dbname=connect.string
)

connection_reportUser <- dbConnect(
  driver, username = "report_user",
  password = "oracle",
  dbname=connect.string
)

