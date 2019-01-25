source("C:/Users/Administrator/Desktop/R DanielMaia/Escoamento_Capabilidade/dados.R")
source("C:/Users/Administrator/Desktop/R DanielMaia/Escoamento_Capabilidade/global.R")

function(input, output) {
  
  #-------------------Campo Data------------------------------------------------------
  
  output$dates <- renderText({
    paste("Data selecionada:",input$dates)
  })
  
  #-----------------------------------------------------------------------------------
  
  
  
  #------------------Campo Descricao do Aco-------------------------------------------
  
  #                     output$diametro <- renderText({ 
  #                 paste("Diametro:", input$diametro, "mm") 
  #                     })
  
  #                       output$parede <- renderText({ 
  #                       paste("Parede:", input$parede) 
  #                             })
  
  #                           output$grau <- renderText({ 
  #                         paste("Grau:", input$grau) 
  #                           })
  
  #                         output$aqa <- renderText({ 
  #                         paste("AQA:", input$aqa) 
  #                             })
  
  #-------------------------------------------------------------------------
  
  
  
  #----------------------Gerar tabela de Entradas---------------------------
  entradasmanuais <- reactive({ 
    
    data.frame(
      Nome = c("Diametro",
               "Parede",
               "Grau",
               "AQA",
               "Ciclo",
               "HF1",
               "TF1",
               "TF2",
               "Limite Inferior",
               "Limite Superior"
      ),
      Valores = (c(input$diametro,
                   input$parede,
                   input$grau,
                   input$aqa,
                   input$ciclo,
                   input$hf1,
                   input$tf1,
                   input$tf2,
                   input$lininf,
                   input$linsup
      )),
      stringsAsFactors = FALSE)
    
  })
  
  output$entradas <- renderTable({
    entradasmanuais()
  })
  
  #-------------------------------------------------------------------------
  entradasmanuais2 <- reactive({ 
    
    data.frame(
      Nome = c("Diametro",
               "Parede",
               "Grau",
               "AQA",
               "Ciclo",
               "HF1",
               "TF1",
               "TF2",
               "Limite Inferior",
               "Limite Superior"
      ),
      Valores = (c(input$diametro,
                   input$parede,
                   input$grau,
                   input$aqa,
                   input$ciclo,
                   input$hf1,
                   input$tf1,
                   input$tf2,
                   input$lininf,
                   input$linsup
      )),
      stringsAsFactors = FALSE)
    
  })
  
  output$entradas2 <- renderTable({
    entradasmanuais2()
  })
  

  #-------------------------Campo gerar histograma--------------------------
  
  output$histograma <-renderPlot({
    capabilidade_LE = qcc(data = capabilidade_LE, type="xbar", plot = TRUE)
    process.capability(object = capabilidade_LE, spec.limits = c(input$lininf,input$linsup))
  })
  
  #-------------------------------------------------------------------------
  
  output$histograma2 <-renderPlot({
    capabilidade2 = qcc(data = capabilidade2, type="xbar", plot = TRUE, lines(x = "red", y = "yellow"))
    process.capability(object = capabilidade2, spec.limits = c(input$lininf,input$linsup))
  })
  
  #------------------------Grafico de normalidade---------------------------
  
  output$normalidade <-renderPlot({
    ss.study.ca(normalidade_LE, LSL = input$lininf, USL = input$linsup, Target = ((input$lininf+input$linsup)/2),alpha = 0.05, f.main = "Teste de Normalidade")
    
  })
  
  #-------------------------------------------------------------------------
  output$normalidade2 <-renderPlot({
    ss.study.ca(normalidade2, LSL = input$lininf, USL = input$linsup, Target = ((input$lininf+input$linsup)/2),alpha = 0.05, f.main = "Teste de Normalidade")
    
  })
  
  
  #-------------mOSTRAR HORaRIO E DATA PARA EXPORTAR JUNTO NO RELATORIO--------------
  
  output$tempoatual <- renderText({
    invalidateLater (1000)
    paste("Tempo Atual:", Sys.time())
  })
  
  #-------------------------------------------------------------------------
  
  
  
  
  
  
  #------------------Criar plot de dados da Planilha de entrada manual------------------------
  
  #output$contents <- renderTable({ 
  
  #df<-read_excel(input$file1$datapath, sheet = 1)
  # return(histogramaYs)  
  # }
  # )
  
  
  #-------------------------------------------------------------------------
  #------------------Criar plot de dados do Banco---------------------------
  
  ###############Descomentar essa parte caso queira a tabela maior###############################
  
  #  output$contents2 = DT::renderDataTable({
  #    tabela_saida
  # })
  
  #output$downloadData <- downloadHandler(
  
  # filename = "LimiteEscoamento.csv",
  #content = function(file) write.csv2(tabela_saida, file, row.names = F),  # replace "tabela_saida" with your data.frame to export'
  #contentType = "text/csv"
  #  )
  
  
  
  
  #--------------------DOwnload dos Arquivos--------------------------------
  
  
  #-------------------------------------------------------------------------
  
  
  
  
  
  #--------------Download dos relatorios em PDF ou EXCEL--------------------
  
  
  #-------------------------------------------------------------------------
  
  
}

