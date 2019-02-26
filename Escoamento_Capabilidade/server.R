source("db.R")

function(input, output, session) {
  
  #-------------------Campo Data------------------------------------------------------
  
  output$dates <- renderText({
    paste("Data selecionada:",input$dates)
  })
  
  #-----------------------------------------------------------------------------------
  
  
  
  
  
  #----------------------Gerar tabela de Entradas---------------------------
  entradasmanuais <- reactive({ 
    
    data.frame(
      Nome = c("Diametro",
               "Parede",
               "Grau",
               "AQA",
               "Ciclo",
               "HF1 <",
               "HF1 >",
               "TF1 <",
               "TF1 >",
               "TF2 <",
               "TF2 >",
               "Limite Inferior",
               "Limite Superior"
      ),
      Valores = (c(input$diametro,
                   input$parede,
                   input$grau,
                   input$aqa,
                   input$ciclo,
                   input$hf1.0,
                   input$hf1.1,
                   input$tf1.0,
                   input$tf1.1,
                   input$tf2.0,
                   input$tf2.1,
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
               "HF1 <",
               "HF1 >",
               "TF1 <",
               "TF1 >",
               "TF2 <",
               "TF2 >",
               "Limite Inferior",
               "Limite Superior"
      ),
      Valores = (c(input$diametro,
                   input$parede,
                   input$grau,
                   input$aqa,
                   input$ciclo,
                   input$hf1.0,
                   input$hf1.1,
                   input$tf1.0,
                   input$tf1.1,
                   input$tf2.0,
                   input$tf2.1,
                   input$lininf,
                   input$linsup
      )),
      stringsAsFactors = FALSE)
    
  })
  
  output$entradas2 <- renderTable({
    entradasmanuais2()
  })
  
  
  #-------------------------ESPERA A ENTRADA DOS PARAMETROS DIGITADOS------  
  observeEvent(input$plot,{
    
    df <- dbGetQuery(
      connection_reportUser,
      query <- glue(
        "Select
        pla.DTH_CRIACAO_REG as DATA,
        pla.COD_ORDEM_PRODUCAO as Ordem,
        aust.IDC_TUBO as IPPN,
        pla.DIM_EXT_TUBO as DimTubo,
        pla.ESP_PAR_TUBO as ParTubo,
        pla.dsc_aco as GRAU,
        pla.cod_aqa as AQA,
        pla.tmo_ciclo_plan as CICLO,
        round ((aust.DTH_DESENFORNAMENTO - aust.dth_enfornamento),7)*1440 as HF,
        round ((res_rev1.DTH_DESENFORNAMENTO - res_rev1.dth_enfornamento),7)*1440 as TF1,
        round ((res_rev2.DTH_DESENFORNAMENTO - res_rev2.dth_enfornamento),7)*1440 as TF2,
        r.value as LE
        
        FROM
        limsprod.SAMPLE sp, 
        QT_QTS.PLA_ORDEM_PRODUCAO pla,
        qt_qts.res_tubo_austenitizacao aust,
        qt_qts.res_tubo_revenimento1 res_rev1,
        qt_qts.res_tubo_revenimento2 res_rev2,
        limsprod.test t,
        limsprod.result r
        
        where
        pla.cod_ordem_producao = sp.ordem_de_processo
        and aust.COD_ORDEM_PRODUCAO = pla.COD_ORDEM_PRODUCAO
        and res_rev1.COD_ORDEM_PRODUCAO = res_rev2.COD_ORDEM_PRODUCAO
        and aust.IDC_TUBO = res_rev1.IDC_TUBO
        and res_rev1.IDC_TUBO = res_rev2.IDC_TUBO
        and t.sample = sp.id_numeric
        and r.test_number = t.test_number
        
        and r.name = 'YIELD STRENGTH (LIESCLTA)'
        and pla.DIM_EXT_TUBO = {as.numeric(input$diametro)}
        and pla.esp_par_tubo = {as.numeric(input$parede)}
        and tmo_ciclo_plan = {as.numeric(input$ciclo)}
        and pla.cod_aqa = '{input$aqa}'
        and dsc_aco = '{input$grau}'
        and pla.cod_ordem_producao like '{input$ordemlista}%'
        and ROUND((aust.DTH_DESENFORNAMENTO - aust.dth_enfornamento),7)*1440 between '{input$hf1.0}' and '{input$hf1.1}'
        and round((res_rev1.DTH_DESENFORNAMENTO - res_rev1.dth_enfornamento),7)*1440 between '{input$tf1.0}' and '{input$tf1.1}'
        and round((res_rev2.DTH_DESENFORNAMENTO - res_rev2.dth_enfornamento),7)*1440 between '{input$tf2.0}' and '{input$tf2.1}'
        and pla.dth_criacao_reg between DATE '{as.character(input$dates[1])}' and DATE '{as.character(input$dates[2])}'
        and r.value != 0
        
        order by pla.DTH_CRIACAO_REG desc")
      )
    
    
    #----------------------------------CALCULO PARA COMECAR A PLOTAR-------------------
    #----------------------------------ESSES DADOS VEM DO DB.R-------------------------
    
    
    df
    tabela_saida = df
    valor_le = df$LE
    #    valor_tr = df$TR
    #    relacao_ts = (valor_le/valor_tr)
    
    
    
    observeEvent(input$plot, {
      
      
      #-------------------------Campo gerar histograma--------------------------
      
      output$histograma <-renderPlot({
        capabilidade_LE = qcc(data = capabilidade_LE, type="xbar", plot = TRUE)
        process.capability(object = capabilidade_LE, spec.limits = c(input$lininf,input$linsup))
      })
      
      #-------------------------------------------------------------------------
      
      #    output$histograma2 <-renderPlot({
      #      capabilidade2 = qcc(data = capabilidade2, type="xbar", plot = TRUE, lines(x = "red", y = "yellow"))
      #      process.capability(object = capabilidade2, spec.limits = c(input$lininf,input$linsup))
      #    })
      
      #------------------------Grafico de normalidade---------------------------
      
      output$normalidade <-renderPlot({
        ss.study.ca(normalidade_LE, LSL = input$lininf, USL = input$linsup, Target = ((input$lininf+input$linsup)/2),alpha = 0.05, f.main = "Teste de Normalidade")
        
      })
      
      #-------------------------------------------------------------------------
      #    output$normalidade2 <-renderPlot({
      #      ss.study.ca(normalidade2, LSL = input$lininf, USL = input$linsup, Target = ((input$lininf+input$linsup)/2),alpha = 0.05, f.main = "Teste de Normalidade")
      #      
      #    })
      
      
      #-------------MOSTRAR HORaRIO E DATA PARA EXPORTAR JUNTO NO RELATORIO--------------
      
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
      
      
      
      
      #------------------------------------Calculo pra Capabilidade---------------------
      capabilidade_LE <- valor_le
      capabilidade_LE = matrix(valor_le, nrow = 1, ncol = length(valor_le), byrow = TRUE)
      normalidade_LE = matrix(valor_le, nrow = length(valor_le), ncol = 1, byrow = TRUE)
      
      
      capabilidade2 <- relacao_ts
      capabilidade2 = matrix(relacao_ts, nrow = 1, ncol = length(relacao_ts), byrow = TRUE)
      normalidade2 = matrix(relacao_ts, nrow = length(relacao_ts), ncol = 1, byrow = TRUE)
      
      #-----------------------------------------------------------------------------------
      
      
      
      
      
      
      #------------------Criar plot de dados do Banco------------------------------
      
      ###############Descomentar essa parte caso queira a tabela maior###############################
      
      
      
      output$contents2 = DT::renderDataTable({
        tabela_saida
      })
      
      output$downloadData <- downloadHandler(
        
        filename = "LimiteEscoamento.csv",
        content = function(file) write.csv2(tabela_saida, file, row.names = F),  # replace "tabela_saida" with your data.frame to export'
        contentType = "text/csv"
      )
      #------------------------------------------------------------------------------
      
      
      
    })
    
    
    
    
  })   
  
  
}

