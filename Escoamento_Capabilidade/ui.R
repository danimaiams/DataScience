library(shiny)
library(shinydashboard)
library(plotly)
library(qcc)
library(ggplot2)
library(SixSigma)
library(sqldf)
library(ROracle)
library(rmarkdown)
library(odbc)
library(readxl)
library(DT)
library(glue)
library(dplyr)




#-----------Declaracao da Matriz Transporta para o QCC---------------
histograma <- c(600, 615, 620, 622, 603, 607, 627, 605, 641, 613, 613, 625, 635, 646, 636, 631, 620, 621, 635, 608, 629, 607, 608, 615, 632, 643)
histograma = matrix(histograma, nrow = 1, ncol = 26, byrow = TRUE)
a = histograma


normalidade <- c(600, 615, 620, 622, 603, 607, 627, 605, 641, 613, 613, 625, 635, 646, 636, 631, 620, 621, 635, 608, 629, 607, 608, 615, 632, 643)
normalidade = matrix(normalidade, nrow = 26, ncol = 1, byrow = TRUE)

#normalidadeYs = matrix(histogramaYs, nrow = 8, ncol = 1, byrow = TRUE)
#cap = matrix(histogramaYs, nrow = 1, ncol = 2, byrow = TRUE)

#--------------------TESTE--------------------------------------------
#Ys<-read_excel(path = "C:/Users/Administrator/Desktop/Ys.xlsx", sheet = 1)
#str(Ys)
#summary(Ys)
#head(Ys)

#histogramaYs <- sqldf('
#      select Ys, TS, round(Ys/TS,2) as Tracao
#                      from Ys
#                      where Ordem = "LQ01134205"
#                      
#                      ')
#eixoX = histogramaYs$YS
#
#hist(x = eixoX, col = 'yellow')
#----------------------------------------------------------------------



#Histograma do Ys que deve ser criado
#hist(Ys, nclass=)

dashboardPage(
  
  dashboardHeader (title= "Limite de Escoamento e Capabilidade"),
  
  
  dashboardSidebar(width = 300,
                   
                   # Range de Selecao de Data
                   dateRangeInput("dates", 
                                  label = ("Periodo"),
                                  start = Sys.Date() - 90, end = Sys.Date() - 1,
                                  format = "dd/mm/yyyy",
                                  language = "pt-BR",
                                  separator = "ate"
                                  
                   ),
                   hr(),
                   
                   
                   #Entrada de Descricao do Aco. VERIFICAR QUE NEM TODAS SAO APENAS NUMERICAS
                   sidebarMenu(
                     menuItem( text = "Descricao do Material",
                               tabName = "descricao",
                               
                               
                               
                               numericInput(inputId= "diametro", label= "Diametro do Aco:", value= 0),
                               
                               numericInput(inputId= "parede", label= "Parede:", value= 0),
                               
                               textInput(inputId= "grau", label= "Grau:", value= 0),
                               
                               textInput(inputId= "aqa", label= "AQA:", value= 0),
                               
                               #AQA Checkbox sera um CHeckbox com os valores retornados da pesquisa do banco
                               #textInput(inputId= "aqacheck", label= "AQA:", value= 0),
                               
                               
                               #Tempo de Ciclo sera um CHeckbox com os valores retornados da pesquisa do banco
                               numericInput(inputId= "ciclo", label = "Ciclo:", value = 0),
                               
                               actionButton("plot", "Plotar!"),
                               
                               hr()
                               
                     ) #fecha o menu DESCRICAO DO MATERIAL
                   ),
                   
                   sidebarMenu(
                     menuItem( text = "Tempos Excessivos",
                               tabName = "temp exce",
                               
                               
                               
                               #Paradas
                               numericInput(inputId= "hf1", label= "Tempo Excessivo: (HF1)", value= 0),
                               
                               numericInput(inputId= "tf1", label= "Tempo Excessivo: (TF1)", value= 0),
                               
                               numericInput(inputId= "tf2", label= "Tempo Excessivo: (TF2)", value= 0),
                               
                               hr()
                               
                     ) #fecha o menu TEMPOS EXCESSIVOS
                   ),
                   
                   sidebarMenu(
                     menuItem( text = "Limites",
                               tabName = "lim",
                               
                               #Entrada Manual dos Limites Inferior e Superior
                               numericInput(inputId= "lininf", label= "Limite Inferior (YS):", value= 0),
                               
                               numericInput(inputId= "linsup", label= "Limite Superior (YS):", value= 0),
                               
                               hr()
                               
                     ) #fecha o menu LIMITES
                   ),
                   
                   #Escolha do Componente Quimico por selecao. OS VALORES MINIMO E MAXIMOS
                   #SERAO BUSCADOS PELO NUMERO DE CADA COMPONENTE NO BANCO. OU SEJA, ELE VAI VARIAR
                   sidebarMenu(
                     menuItem( text = "Componentes Quimicos",
                               tabName = "quimicos",
                               
                               
                               sliderInput("valmolibidenio", "Valor de Molibidenio:",
                                           min = 1, max = 1000,
                                           value = c(200,500)),
                               
                               sliderInput("valtitanio", "Valor de Titanio:",
                                           min = 1, max = 1000,
                                           value = c(200,500)),
                               
                               sliderInput("valnitrogenio", "Valor de Nitrogenio:",
                                           min = 1, max = 1000,
                                           value = c(200,500)),
                               
                               sliderInput("valcromo", "Valor de Cromo:",
                                           min = 1, max = 1000,
                                           value = c(200,500)),
                               
                               sliderInput("valcarbonoequivalente", "Valor de Carbono Equivalente:",
                                           min = 1, max = 1000,
                                           value = c(200,500)),
                               
                               hr()
                               
                     ) #fecha o menu de COMPONENTES QUIMICOS
                   ),
                   
                   
                   
                   #Upar planilha em Excel
                   hr(),
                   
                   sidebarMenu(
                     fileInput(inputId = "file1",placeholder = "Selecione a planilha", buttonLabel = "Upar", label = "Ou selecione a planilha do Ys", multiple = FALSE, accept = ".xlsx")
                     
                   ), #fecha input de planilha
                   
                   
                   
                   #Texto que ajuda e especifica os codigos
                   helpText("Os campos nao precisam necessariamente estar todos preenchidos para",
                            "realizar a busca.")
                   
                   
                   
  ),
  
  dashboardBody( 
    tabsetPanel(type= "tabs",
                
                tabPanel("Grafico",
                         h2(textOutput ("tempoatual"), align= "center" ),
                         
                         #GRAFICO DE CAPABILIDADE
                         box( title = "Relatorio de Capabilidade - Diametro x Parede - AQA - Grau",
                              width = "8",
                              footer = "Aqui pode vir qualquer texto",
                              plotOutput("histograma")
                         ),
                         
                         box( title = "Valores de Entrada",
                              width = "3",
                              footer = NULL,
                              tableOutput("entradas")
                         ),
                         
                         textOutput("diametro"),
                         textOutput("parede"),
                         textOutput("grau"),
                         textOutput("aqa"),
                         
                         #GRAFICO DE NORMALIDADE
                         box( title = NULL,
                              width = "12",
                              footer = "Qualquer texto ou valores",
                              plotOutput("normalidade")
                              
                         )
                ), #fecha o tabpanel de GRAFICO. L152
                
                tabPanel("Grafico Ys/Ts",
                         box( title = "Relatorio de Capabilidade - Diametro x Parede - AQA - Grau",
                              width = "8",
                              footer = "Aqui pode vir qualquer texto",
                              plotOutput("histograma2")
                         ),
                         
                         box( title = "Valores de Entrada",
                              width = "3",
                              footer = NULL,
                              tableOutput("entradas2")
                         ),
                         
                         box( title = NULL,
                              width = "12",
                              footer = "Qualquer texto ou valores",
                              plotOutput("normalidade2")
                         )
                         
                ),
                
                
                tabPanel("Dados do Banco",
                         
                         #img(src = "vsbimg.png", height = 150, width = 150),
                         
                         h4("Dados de Saida", align = "center"),
                         
                         # tableOutput("contents"),
                         DT::dataTableOutput("contents2"),
                         
                         datatable(
                           data = tabela_saida, extensions = 'Buttons', options = list(
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                           )
                         )
                         
                ),
                
                #Nessa Tab ficara o texto para acrescentar no botao de exportar para PDF o relatorio.
                #Sera criado um metodo que o usuario digita o texto e ele se posiciona em algum lugar no relatorio
                tabPanel("Texto",
                         
                         helpText("Texto padrao para exportacao via PDF",
                                  "Caso tenha um relatorio ja existente, reproduzir o modelo aqui!"),
                         br(),
                         br(),
                         
                         downloadButton("downloadData", "Baixar Planilha de Dados"),
                         
                         br(),
                         br(),
                         
                         downloadButton("pdfexp", "Exportar PDF"),
                         
                         br(),
                         br(),
                         
                         downloadButton("excelexp", "Exportar Excel")
                         
                         
                ) #Fecha ultimo tabpanel. L193
                
    ) #fecha o tabsetpanel que inicia a funcao das tabpanel. L150
  ) #fecha a dashboard body. L149
) #fecha a dashboardpage que inicia todo o padrao de dash. L31

