#ConfiguraÃ§Ã£o para conexÃ£o com o servidor de histÃ³rico
#Jeceaba
library("ROracle")

Sys.setenv(TZ = "GMT-3")
Sys.setenv(ORA_SDTZ = "GMT-3")

driver <- dbDriver("Oracle")
host <- "
port <- 
sid <- 

connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))",
  sep = ""
)

connection <- dbConnect(
  driver, username = ,
  password = ,
  dbname=connect.string
)

connection_reportUser <- dbConnect(
  driver, username = ,
  password = ,
  dbname=connect.string
)

