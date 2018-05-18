# server.R

source("./Functions/ALFA.R")
source("./Functions/ASC.R")
source("./Functions/ASC2.R")
source("./Functions/AUIC.R")
source("./Functions/BETA.R")
source("./Functions/calculaAnos.R")
source("./Functions/calculaMeses.R")
source("./Functions/CL.R")
source("./Functions/CL_Alt.R")
source("./Functions/concentracao_pico_prevista.R")
source("./Functions/concentracao_vale_prevista.R")
source("./Functions/CPT.R")
source("./Functions/CPT_Alt.R")
source("./Functions/CRCL.R")
source("./Functions/CrCl_Alt.R")
source("./Functions/IMC.R")
source("./Functions/intervalo_calculado.R")
source("./Functions/K0.R")
source("./Functions/K10.R")
source("./Functions/PA.R")
source("./Functions/PCI.R")
source("./Functions/PCM.R")
source("./Functions/PD.R")
source("./Functions/sobreposicao.R")
source("./Functions/T.R")
source("./Functions/T12.R")
source("./Functions/VC.R")
source("./Functions/VDB.R")
source("./Functions/VDB_Alt.R")

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @post /novoPaciente
novoPaciente <- function(paciente){
  df_paciente <- json2df(paciente)
  conexao <- abre_conexao()
  query <- paste("INSERT INTO paciente(nome_paciente,cpf_paciente,nascimento_paciente,genero_paciente,peso_paciente,altura_paciente,cr_paciente,unid_int_paciente,observacao_paciente,rg_paciente,telefone_paciente,agente_saude) VALUES('",df_paciente[1],"',",df_paciente[2],",'",df_paciente[3],"',",df_paciente[4],",",df_paciente[5],",",df_paciente[6],",",df_paciente[7],",'",df_paciente[8],"','",df_paciente[9],"',",df_paciente[10],",",df_paciente[11],",'",df_paciente[12],"')")
  dbSendQuery(conexao,query)
  dbDisconnect(conexao)
  return("Paciente inserido com sucesso!")
}

#* @post /novoTratamento
novoTratamento <- function(tratamento){
  df_tratamento <- json2df(tratamento)
  conexao <- abre_conexao()
  query <- paste("INSERT INTO tratamento(cod_paciente, cod_farmaco) VALUES(",df_tratamento[1],",",df_tratamento[2],")")
  dbSendQuery(conexao,query)
  dbDisconnect(conexao)
  return("Tratamento inserido com sucesso!")
}

#* @post /novoEvento
novoEvento <- function(evento){
  df_evento <- json2df(evento)
  conexao <- abre_conexao()
  query <- paste("INSERT INTO historico(atributo_historico, valor_historico, data_hora_historico, cod_tratamento) VALUES('",df_evento[1],"',",df_evento[2],",'",df_evento[3],"',",df_evento[4],")")
  dbSendQuery(conexao,query)
  disconnect(conexao)
}

#* @get /tabela_pacientes
get_tabela_pacientes <- function(){
  jsondf <- df2json(tabela_pacientes())
  return(jsondf)
}

#* @get /tabela_tratamentos
get_tabela_tratamentos <- function(){
  jsondf <- df2json(tabela_tratamentos())
  return(jsondf)
}

#* @get /pesquisa_tratamentos
#* @cod_paciente
get_pesquisa_tratamentos <- function(cod_paciente){
  jsondf <- df2json(pesquisa_tratamentos(cod_paciente))
  return(jsondf)
}

#* @get /pesquisa_tratamentos_paciente
#* @cod_paciente
get_pesquisa_tratamentos <- function(cod_paciente){
  jsondf <- df2json(dados_paciente(cod_paciente))
  return(jsondf)
}

#* @get /dados_historico
#* @cod_paciente
get_dados_historico <- function(cod_tratamento){
  jsondf <- df2json(dados_historico(cod_tratamento))
  return(jsondf)
}

#* @get /get_farmacos
get_farmacos <- function(){
  return(df2json(tabela_farmacos()))
}

#* @get /previsaoParametros
get_previsao_parametros <- function(codPaciente,concValeDes,dose,intervaloInformado,tempoInfusao){
  dfPaciente <- dados_paciente(codPaciente)
  return(df2json(previsao_parametros(dfPaciente[3],dfPaciente[4],dfPaciente[5],dfPaciente[11],dfPaciente[6],concValeDes,dose,intervaloInformado,tempoInfusao)))
}

#* @get /simulacaoInicial
get_simulacao_inicial <- function(codPaciente,concValeDes,dose,intervaloInformado,tempoInfusao,pacienteQtd){
  dfPaciente <- dados_paciente(codPaciente)
  dfParametros <- previsao_parametros(dfPaciente[3],dfPaciente[4],dfPaciente[5],dfPaciente[11],dfPaciente[6],concValeDes,dose,intervaloInformado,tempoInfusao)
  dfSim <- simulacao(dose,tempoInfusao,dfParametros[15],dfParametros[13],pacienteQtd)
  
  return(df2json(dfSim))
}

abre_conexao <- function(){
  conexao <- dbConnect(MySQL(),user="0130841",password="zforzachariah",dbname="farmacocinetica", host="ceted.feevale.br", port=3306)
  return(conexao)
}

#todas as funções com acesso a banco de dados para retorno do GET

tabela_farmacos <- function(){
  query_farmaco <- paste("SELECT cod_farmaco, nome_farmaco FROM farmaco")
  rsFarmaco <- dbSendQuery(conexao,query_farmaco)
  data_farmaco <- fetch(rsFarmaco, n=10)
  df_farmaco <- data.frame(data_farmaco)
  return(df_farmaco)
}

tabela_pacientes <- function(){
  abriu_conexao <- abre_conexao()
  rs_paciente = dbSendQuery(abriu_conexao,"SELECT p.cod_paciente, p.nome_paciente, p.cpf_paciente, p.nascimento_paciente, p.unid_int_paciente FROM paciente p")
  data_paciente = fetch(rs_paciente,n = 50)
  df_paciente <- data.frame(data_paciente)
  #colnames(df_paciente) <- c("Cod. Paciente", "Nome", "CPF", "Data de Nascimento", "Unidade de Internação")
  dbDisconnect(abriu_conexao)
  return(df_paciente)
}

tabela_tratamentos <- function(){
  abriu_conexao <- abre_conexao()
  rs_tratamento = dbSendQuery(abriu_conexao,"SELECT p.nome_paciente, f.nome_farmaco, t.cod_tratamento FROM paciente p INNER JOIN tratamento t ON t.cod_paciente = p.cod_paciente INNER JOIN farmaco f ON f.cod_farmaco = t.cod_farmaco")
  data_tratamento = fetch(rs_tratamento,n=50)
  df_tratamento = data.frame(data_tratamento)
  colnames(df_tratamento) <- c("Paciente", "FÃ¡rmaco", "Cod. Tratamento")
  dbDisconnect(abriu_conexao)
  return(df_tratamento)
}

pesquisa_tratamentos <- function(cod_paciente){
  abriu_conexao <- abre_conexao()
  query = paste("SELECT p.nome_paciente, f.nome_farmaco, t.cod_tratamento FROM paciente p INNER JOIN tratamento t ON t.cod_paciente = p.cod_paciente INNER JOIN farmaco f ON f.cod_farmaco = t.cod_farmaco WHERE p.cod_paciente = ",cod_paciente)
  rs_tratamento = dbSendQuery(abriu_conexao,query)
  data_tratamento = fetch(rs_tratamento,n=50)
  df_tratamento = data.frame(data_tratamento)
  colnames(df_tratamento) <- c("Paciente", "Fármaco", "Cod. Tratamento")
  dbDisconnect(abriu_conexao)
  return(df_tratamento)
}

dados_paciente <- function(cod_paciente){
  abriu_conexao <- abre_conexao()
  query <- paste("select nome_paciente, cpf_paciente, nascimento_paciente, peso_paciente, altura_paciente, cr_paciente, unid_int_paciente, observacao_paciente, rg_paciente, telefone_paciente, genero_paciente, agente_saude from paciente where cod_paciente = ",cod_paciente)
  rs <- dbSendQuery(abriu_conexao,query)
  data <- fetch(rs, n=1)
  df <- data.frame(data)
  dbDisconnect(abriu_conexao)
  
  return(df)
}

dados_historico <- function(cod_tratamento){
  query <- paste("select h.cod_historico,h.atributo_historico, h.valor_historico, h.data_hora_historico,h.cod_tratamento FROM historico h WHERE cod_tratamento = ",cod_tratamento)
  abriu_conexao <- abre_conexao()
  rs = dbSendQuery(abriu_conexao,query)
  data = fetch(rs,n=50)
  df <- data.frame(data)
  
  return(df)
}

previsao_parametros <- function(nascimento,peso,altura,genero,cr,concValeDes,dose,intervaloInformado,tempoInfusao){
  pacienteIdade <- calculaAnos(nascimento)
  pacientePeso <- peso
  pacienteAltura <- altura
  pacienteGenero <- genero
  pacienteCr <- cr
  
  pacienteIMC <- IMC(pacientePeso,pacienteAltura)
  pacientePCI <- PCI(calculaAnos(nascimento),pacienteAltura,pacienteGenero)
  pacientePCM <- PCM(pacienteGenero,pacientePeso,pacienteIMC)
  pacientePD <- PD(pacientePCI,pacienteGenero)
  pacientePA <- PA(pacientePeso,pacientePCI,pacientePCM)
  pacienteASC <- ASC(pacientePeso,pacienteAltura)
  pacienteCrCl <- CrCl_Alt(pacientePeso,pacientePCI,pacienteGenero,calculaAnos(nascimento),pacienteCr,pacienteASC,pacienteAltura, pacientePA)
  pacienteCl <- CL_Alt(pacienteCrCl,pacientePeso,pacienteCr,calculaMeses(nascimento),calculaAnos(nascimento),calculaAnos(nascimento))
  pacienteVc <- VC(pacientePeso,calculaMeses(nascimento),pacientePD,calculaAnos(nascimento),pacienteCrCl)
  #pacienteVdb <- VDB(calculaMeses(nascimento),pacientePD,pacienteCl,pacienteVc,pacientePeso,pacienteCrCl,calculaAnos(nascimento))
  pacienteVdb <- VDB_Alt(pacientePeso)
  pacienteK10 <- K10(pacienteCl,pacienteVc)
  pacienteBeta <- BETA(pacienteCl,pacienteVdb)
  pacienteAlfa <- ALFA(pacienteK10,pacienteBeta)
  pacienteT12 <- T12(pacienteBeta)
  pacienteT <- T(pacienteT12)
  pacienteIntervalo <- intervalo_calculado(concValeDes,pacienteCl)
  pacienteASC2 <- ASC2(dose,pacienteCl)
  pacienteAUIC <- AUIC(pacienteASC2,pacienteIntervalo)
  pacienteK0 <- K0(concValeDes,pacienteAlfa,pacienteBeta,pacienteVc,tempoInfusao,intervaloInformado)
  pacienteCPP <- concentracao_pico_prevista(pacienteK0,tempoInfusao,pacienteAlfa,pacienteBeta,pacienteVc,intervaloInformado)
  #pacienteCVP <- concentracao_vale_prevista(pacienteK0,tempoInfusao,pacienteAlfa,pacienteBeta,intervaloInformado,pacienteVc,tempoInfusao)
  pacienteCVP <- concentracao_vale_prevista(dose,tempoInfusao,pacienteAlfa,pacienteBeta,intervaloInformado,pacienteVc,tempoInfusao)
  
  dfPaciente <- data.frame(pacienteIdade,pacientePeso,pacienteAltura,pacienteGenero,pacienteCr,pacienteIMC,
             pacientePCI,pacientePCM,pacientePD,pacientePA,pacienteASC,pacienteCrCl,pacienteCl,
             pacienteVc,pacienteVdb,pacienteK10,pacienteBeta,pacienteAlfa,pacienteT12,pacienteT,
             pacienteIntervalo,pacienteASC2,pacienteAUIC,pacienteK0,pacienteCPP,pacienteCVP)
  
  return(dfPaciente)
}

simulacao <- function(dose,tempoInfusao,pacienteVdb,pacienteCl,pacienteQtd){
  eixo_y <- seq(0.1,120,0.1)
  pos_curva <- 1
  #parametros$simulacao_curva <- mapply(CPT,input$dose,parametros$simulacao_valor_alfa,input$tinf,parametros$eixo_y,parametros$simulacao_valor_beta,parametros$simulacao_valor_vc,parametros$simulacao_valor_vdb,input$dose, parametros$simulacao_valor_cl)
  simulacao_curva <- mapply(CPT,dose,tempoInfusao,eixo_y,pacienteVdb,pacienteCl)
  simulacao_curva_final <- sobreposicao(simulacao_curva,pacienteIntervalo, pacienteQtd)
  
  return(data.frame(simulacao_curva_final),eixo_y)
}