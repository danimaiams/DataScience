library(shiny)
library(shinydashboard)
library(plotly)
library(qcc)
library(ggplot2)
library(SixSigma)
library(sqldf)
library(ROracle)
library(rmarkdown)
library(readxl)
library(DT)
library(glue)
library(dplyr)
library(sqldf)


dashboardPage(
  
  dashboardHeader (title= "Limite de Escoamento e Capabilidade"),
  
  
  dashboardSidebar(width = 300,
                   
                   
                   
                   # Range de Selecao de Data
                   dateRangeInput("dates", 
                                  label = ("Periodo"),
                                  start = Sys.Date() - 365, end = Sys.Date() - 1,
                                  format = "dd/mm/yyyy",
                                  language = "pt-BR",
                                  separator = "ate"
                                  
                   ),
                   hr(),
                   
                   
                   
                   
                   
                   
                   #Entrada de Descricao do Aco. VERIFICAR QUE NEM TODAS SAO APENAS NUMERICAS
                   sidebarMenu(
                     menuItem( text = "Descricao do Material",
                               tabName = "descricao",
                               
                               
                               
                               numericInput(inputId= "diametro", label= "Diametro do Aco:", value = 0),
                               
                               numericInput(inputId= "parede", label= "Parede:", value = 0),
                               
                               numericInput(inputId= "ciclo", label = "Tempo de Ciclo:", value = 0),
                               
                               selectInput(inputId= "grau", label = "Grau:",
                                           choices = list('DNV SMLS 415 SF','DNV SMLS 450','DNV SMLS 450 S','DNV SMLS 450 SF','DNV 450 FPD','DNV 450 SFDP','DNV 450 SFPD','K55','L80 TIPO 1','L80 TIPO 1 HCE','L80 1%Cr','L80EC','L80SS','N80 TIPO Q','P110','P110EC','P29HBV-QT','Q125 TIPO 1','R95','T95 TIPO 1','X42','X42QO','X52Q','X52QO','X52QS','X60QOS','X60QO/X60QS','X60QS/X60QO','X65QO','X65QOS','X65QS','X70QO')
                               ),
                               
                               selectInput("aqa", "Tipo de AQA:",
                                           choices=list('P24M','P25B','S43M','T989','VD00','VD04','VD06','VD07','VD08','VD26','VD28','VD30','VD31','VD32','VD33','VD34','VD43','VD48','VD50','VD51','VD56','VD57','VD58','VD59','VD60','VD61','VD62','VD63','VD68','VS01','VS04','VS06','VS09','VS13','VS19','VS24','VS33','VS35','VS49','VS53','VS55','9')
                               ),
                               
                               selectInput("ordemlista", "Tipo de Ordem:",
                                           choices=list('LQ','LQR', 'LQX')
                               ),
                               
                               
                               
                               hr()
                               
                     ) #fecha o menu DESCRICAO DO MATERIAL
                   ),
                   
                   sidebarMenu(
                     menuItem( text = "Tempos Excessivos",
                               tabName = "temp exce",
                               
                               
                               
                               #Paradas
                               numericInput(inputId= "hf1.0", label= " Menor Tempo Excessivo: (HF1)", value= 0),
                               
                               numericInput(inputId= "hf1.1", label= "Maior Tempo Excessivo: (HF1)", value= 100),
                               
                               numericInput(inputId= "tf1.0", label= "Menor Tempo Excessivo: (TF1)", value= 0),
                               
                               numericInput(inputId= "tf1.1", label= "Maior Tempo Excessivo: (TF1)", value= 100),
                               
                               numericInput(inputId= "tf2.0", label= "Menor Tempo Excessivo: (TF2)", value= 0),
                               
                               numericInput(inputId= "tf2.1", label= "Maior Tempo Excessivo: (TF2)", value= 100),
                               
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
                   
                   
                   
                   #------------------------------------Upar planilha em Excel-------------------------------
                   #         hr(),
                   
                   #         sidebarMenu(
                   #           fileInput(inputId = "file1",placeholder = "Selecione a planilha", buttonLabel = "Upar", label = "Ou selecione a planilha do Ys", multiple = FALSE, accept = ".xlsx")
                   
                   #         ), #fecha input de planilha
                   
                   #-------------------------------------------------------------------------------------------
                   
                   #Texto que ajuda e especifica os codigos
                   helpText("Os campos precisam estar todos preenchidos para",
                            "realizar a busca."),
                   
                   hr(),
                   
                   actionButton("plot", "Plotar!")
                   
                   
                   
                   
                   
  ),
  
  dashboardBody( 
    tabsetPanel(type= "tabs",
                
                tabPanel("Grafico",
                         h2(textOutput ("tempoatual"), align= "center" ),
                         
                         #GRAFICO DE CAPABILIDADE
                         box( title = "Relatorio de Capabilidade - Diametro x Parede - AQA - Grau",
                              width = "8",
                              footer = "Capabilidade",
                              plotOutput("histograma")
                         ),
                         
                         box( title = "Valores de Entrada",
                              width = "4",
                              footer = NULL,
                              tableOutput("entradas")
                         ),
                         
                         textOutput("diametro"),
                         textOutput("parede"),
                         textOutput("grau"),
                         textOutput("aqa"),
                         
                         #GRAFICO DE NORMALIDADE
                         box( title = "Grafico de Normalidade",
                              width = "12",
                              footer = "Normalidade",
                              plotOutput("normalidade")
                              
                         )
                ), #fecha o tabpanel de GRAFICO. L152
                
                #----------------------------------------GRAFICO DO BISPO------------------------------------               
                
                #            tabPanel("Grafico Ys/Ts",
                #                    box( title = "Relatorio de Capabilidade - Diametro x Parede - AQA - Grau",
                #                        width = "8",
                #                       footer = "Aqui pode vir qualquer texto",
                #                      plotOutput("histograma2")
                #                ),
                
                #               box( title = "Valores de Entrada",
                #                   width = "3",
                #                  footer = NULL,
                #                 tableOutput("entradas2")
                #           ),
                
                #          box( title = NULL,
                #              width = "12",
                #             footer = "Qualquer texto ou valores",
                #            plotOutput("normalidade2")
                #      )
                
                #  ),
                
                #--------------------------------------------------------------------------------------------
                
                tabPanel("Dados do Banco",
                         
                         
                         h4("Dados de Saida", align = "center"),
                         
                         #tableOutput("contents"),
                         DT::dataTableOutput("contents2")
                         
                         
                         #-------------------------------------DATATABLE COM BOTOES-----------------------------------                        
                         # datatable(
                         #  data = tabela_saida, extensions = 'Buttons', options = list
                         # (
                         #  dom = 'Bfrtip',
                         # buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                         # )
                         # )
                         
                         #--------------------------------------------------------------------------------------------                     
                         
                         
                ),
                
                #Nessa Tab ficara o texto para acrescentar no botao de exportar para PDF o relatorio.
                #Sera criado um metodo que o usuario digita o texto e ele se posiciona em algum lugar no relatorio
                tabPanel("Texto",
                         
                         helpText("Texto padrao para exportacao via PDF.",
                                  "Clique um 'Baixar Planilha de Dados!' para baixar a tabela em Excel"),
                         br(),
                         br(),
                         
                         downloadButton("downloadData", "Baixar Planilha de Dados"),
                         
                         br(),
                         br(),
                         
                         downloadButton("pdfexp", "Exportar PDF")
                         
                ) #Fecha ultimo tabpanel. L193
                
    ) #fecha o tabsetpanel que inicia a funcao das tabpanel. 
  ) #fecha a dashboard body. 
)#fecha a dashboardpage que inicia todo o padrao de dash. 

