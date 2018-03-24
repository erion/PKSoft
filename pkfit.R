ajustar_parametros <- function() {
  #alteração235
  
  #coletando as concentrações medidas
  #abriu_conexao <- abrir_conexao()
  query_concentracoes_medidas <- paste("SELECT cod_historico, valor_historico, data_hora_historico FROM historico WHERE atributo_historico LIKE '%Concentra%' AND cod_tratamento = ", parametros$tratamento_selecionado)
  abriu_conexao <- abre_conexao()
  rs_cm = dbSendQuery(abriu_conexao,query_concentracoes_medidas)
  data_historico = fetch(rs_cm,n=50)
  df_cm <- data.frame(data_historico)
  dbDisconnect(abriu_conexao)
  
  #vetor com as concentraçoes medidas
  cm <- df_cm[2]
  
  #vetor com os horários em que as concentrações foram medidas
  horarios_concentracoes_medidas <- df_cm[3]
  if(length(horarios_concentracoes_medidas[[1]])>0){
    #vetor contendo a diferença entre o horário medido e o horário inicial
    horarios_relativos <- vector()
    for(i in 1 : length(horarios_concentracoes_medidas)){
      temporario <-  as.period(ymd_hms(horarios_concentracoes_medidas[[i]]) - ymd_hms(retorna_data_hora()))
      temporario <- as.numeric(temporario, "hours")
      horarios_relativos <- c(horarios_relativos, temporario)
    }
    
    
    #data frame da curva de concentrações
    curva_y <- parametros$simulacao_curva_final
    curva_x <- seq(0.1,120,0.1)
    curva <- data.frame(curva_x, curva_y)
    
    horarios_concentracoes_medidas <- horarios_relativos
    
    parametros$cm <- cm
    parametros$hcm <- horarios_concentracoes_medidas
    
    horarios_concentracoes_previstas <- vector()
    cp <- vector()
    
    for(i in 1:length(horarios_relativos)){
      concentracao_prevista <- (input$dose/parametros$simulacao_valor_cl)*((1-exp(-(parametros$simulacao_valor_cl/parametros$simulacao_valor_vdb)*input$tinf))/(1-exp(-(parametros$simulacao_valor_cl/parametros$simulacao_valor_vdb)*input$intervalo)))*exp(-(parametros$simulacao_valor_cl/parametros$simulacao_valor_vdb)*horarios_relativos[i])
      horarios_concentracoes_previstas <- c(horarios_concentracoes_previstas,horarios_relativos[i])
      cp <- c(cp,concentracao_prevista)
    }
    
    if(length(horarios_concentracoes_previstas)==length(horarios_concentracoes_medidas[[1]])){
      desvio_padrao_concentracao <- 0.30751 + (0.024864 * cp) + (0.00027637 * cp^2)
      
      parametros_iniciais <- c(parametros$simulacao_valor_cl, parametros$simulacao_valor_vdb)
      d1 <- (parametros_iniciais[1]*28)/100
      d2 <- (parametros_iniciais[2]*37)/100
      desvios <- c(d1, d2)
      parametros_iniciais <- c(parametros$simulacao_valor_cl + 1, parametros$simulacao_valor_vdb + 1)
      
      concentra <- function(cl,vdb){
        CPt <- sum((input$dose/cl)*((1-exp(-(cl/vdb)*input$tinf))/(1-exp(-(cl/vdb)*input$intervalo)))*exp(-(cl/vdb)*horarios_relativos))
        #CPt <- sum((input$dose/cl)*(1-exp(-(cl/vdb)*input$tinf))*exp(-(cl/vdb)*(horarios_relativos-input$tinf)))
        return(CPt)
      }
      
      min <- function(p){
        sum((((cm-concentra(p[1],p[2]))^2)/desvio_padrao_concentracao^2)+(((parametros$simulacao_valor_cl-p[1])^2)/desvios[1]^2)+(((parametros$simulacao_valor_vdb-p[2])^2)/desvios[2]^2))
      }
      
      result <- optim(par = c(parametros_iniciais[1],parametros_iniciais[2]), fn=min, method="BFGS")
      return(result)
    } else{
      r1 <- c(parametros$simulacao_valor_cl, parametros$simulacao_valor_vdb)
      r2 <- c(0,0)
      result <- data.frame(r1,r2)
      colnames(result) <- c("par","ns")
      return(result)
    }
  } else{
    r1 <- c(parametros$simulacao_valor_cl, parametros$simulacao_valor_vdb)
    r2 <- c(0,0)
    result <- data.frame(r1,r2)
    colnames(result) <- c("par","ns")
    return(result)
  }
}