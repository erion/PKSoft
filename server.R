# server.R

root_path <- './R/'
source(paste(root_path,"Functions/ALFA.R", sep=""))
source(paste(root_path,"Functions/ASC.R", sep=""))
source(paste(root_path,"Functions/ASC2.R", sep=""))
source(paste(root_path,"Functions/AUIC.R", sep=""))
source(paste(root_path,"Functions/BETA.R", sep=""))
source(paste(root_path,"Functions/calculaAnos.R", sep=""))
source(paste(root_path,"Functions/calculaMeses.R", sep=""))
source(paste(root_path,"Functions/CL.R", sep=""))
source(paste(root_path,"Functions/CL_Alt.R", sep=""))
source(paste(root_path,"Functions/concentracao_pico_prevista.R", sep=""))
source(paste(root_path,"Functions/concentracao_vale_prevista.R", sep=""))
source(paste(root_path,"Functions/CPT.R", sep=""))
source(paste(root_path,"Functions/CPT_Alt.R", sep=""))
source(paste(root_path,"Functions/CRCL.R", sep=""))
source(paste(root_path,"Functions/CrCl_Alt.R", sep=""))
source(paste(root_path,"Functions/IMC.R", sep=""))
source(paste(root_path,"Functions/intervalo_calculado.R", sep=""))
source(paste(root_path,"Functions/K0.R", sep=""))
source(paste(root_path,"Functions/K10.R", sep=""))
source(paste(root_path,"Functions/PA.R", sep=""))
source(paste(root_path,"Functions/PCI.R", sep=""))
source(paste(root_path,"Functions/PCM.R", sep=""))
source(paste(root_path,"Functions/PD.R", sep=""))
source(paste(root_path,"Functions/sobreposicao.R", sep=""))
source(paste(root_path,"Functions/T.R", sep=""))
source(paste(root_path,"Functions/T12.R", sep=""))
source(paste(root_path,"Functions/VC.R", sep=""))
source(paste(root_path,"Functions/VDB.R", sep=""))
source(paste(root_path,"Functions/VDB_Alt.R", sep=""))

#* @assets /home/oem/erion/pksoft-fe/build /
list()

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @json
#* @post /login
login <- function(usuario, res){
  conexao <- abre_conexao()
  query <- paste("SELECT * FROM usuario WHERE login ='",usuario$login,"'",
    "AND senha ='",usuario$senha,"'")

  rs_usuario = dbSendQuery(conexao,query)
  data_usuario = fetch(rs_usuario,n = 1)
  df_usuario <- data.frame(data_usuario)
  dbDisconnect(conexao)

  res$setHeader("Content-Type", "application/json")
  res$status <- 200
  res$body <- jsonlite::toJSON(data_usuario,auto_unbox=TRUE)

  res
}

#* @post /novo_paciente
novo_paciente <- function(paciente){
  conexao <- abre_conexao()
  query <- paste("INSERT INTO paciente(nome_paciente,cpf_paciente,nascimento_paciente,genero_paciente,",
    "peso_paciente,altura_paciente,cr_paciente,unid_int_paciente,observacao_paciente,",
    "rg_paciente,telefone_paciente,agente_saude) VALUES('",
    paciente$nome_paciente,"',",paciente$cpf_paciente,",'",paciente$nascimento_paciente,
    "','",paciente$genero_paciente,"',",paciente$peso_paciente,",",paciente$altura_paciente,
    ",",paciente$cr_paciente,",'",paciente$unid_int_paciente,"','",paciente$observacao_paciente,
    "',",paciente$rg_paciente,",",paciente$telefone_paciente,",'",paciente$agente_saude,"')", sep="")

  dbSendQuery(conexao,query)
  dbDisconnect(conexao)
  return("Paciente inserido com sucesso!")
}

#* @post /alterar_paciente/<id>
alterar_paciente <- function(id, paciente){
  conexao <- abre_conexao()
  query <- paste("UPDATE paciente SET ",
    "nome_paciente='",paciente$nome_paciente,"',",
    "cpf_paciente='",paciente$cpf_paciente,"',",
    "nascimento_paciente='",paciente$nascimento_paciente,"',",
    "genero_paciente='",paciente$genero_paciente,"',",
    "peso_paciente=",paciente$peso_paciente,",",
    "altura_paciente=",paciente$altura_paciente,",",
    "cr_paciente=",paciente$cr_paciente,",",
    "unid_int_paciente='",paciente$unid_int_paciente,"',",
    "observacao_paciente='",paciente$observacao_paciente,"',",
    "rg_paciente='",paciente$rg_paciente,"',",
    "telefone_paciente='",paciente$telefone_paciente,"',",
    "agente_saude= '",paciente$agente_saude,"'",
    "WHERE cod_paciente=",id, sep="")

  dbSendQuery(conexao,query)
  dbDisconnect(conexao)
  return("Paciente alterado com sucesso!")
}

#* @post /novo_tratamento
novo_tratamento <- function(tratamento){
  conexao <- abre_conexao()
  query <- paste("INSERT INTO tratamento(cod_paciente, cod_farmaco) VALUES(",tratamento$cod_paciente,",",tratamento$cod_farmaco,")")
  dbSendQuery(conexao,query)
  dbDisconnect(conexao)
  return("Tratamento inserido com sucesso!")
}

#* @post /alterar_tratamento/<id>
alterar_tratamento <- function(id, tratamento){
  conexao <- abre_conexao()
  query <- paste("UPDATE tratamento SET ",
    "cod_paciente=",tratamento$cod_paciente,",",
    "cod_farmaco=",tratamento$cod_farmaco,
    "WHERE cod_tratamento=",id)
  dbSendQuery(conexao,query)
  dbDisconnect(conexao)
  return("Tratamento alterado com sucesso!")
}

#* @post /novo_historico
novo_historico <- function(historico){
  conexao <- abre_conexao()
  query <- paste("INSERT INTO historico(atributo_historico, valor_historico, data_hora_historico, cod_tratamento)",
                 "VALUES('",historico$atributo_historico,"',",historico$valor_historico,
                 ",'",historico$data_hora_historico,"',",historico$cod_tratamento,")", sep="")
  dbSendQuery(conexao,query)
  dbDisconnect(conexao)
}

# não está sendo utilizado, foi removido após implementação, como RN, não faz sentido editar o histórico
#* @post /alterar_historico/<id>
alterar_historico <- function(id, historico){
  df_evento <- json2df(evento)
  conexao <- abre_conexao()
  query <- paste("UPDATE historico SET ",
                "atributo_historico = '",historico$atributo_historico,"'",
                ",valor_historico = ",historico$valor_historico,
                ",data_hora_historico = '",historico$data_hora_historico,"'",
                ",cod_tratamento = ",historico$cod_tratamento,
                "WHERE cod_historico = ",id, sep="")
  dbSendQuery(conexao,query)
  dbDisconnect(conexao)
}

#* @preempt cors
#* @post /novo_farmaco
novo_farmaco <- function(farmaco, res){
  conexao <- abre_conexao()
  query <- paste("INSERT INTO farmaco(nome_farmaco) VALUES('",farmaco$nome_farmaco,"')")
  dbSendQuery(conexao,query)

  res$status <- 200
  dbDisconnect(conexao)
  res
  res$toResponse()
  return(res)
}

#* @preempt cors
#* @post /alterar_farmaco/<id>
alterar_farmaco <- function(id, farmaco, res){
  conexao <- abre_conexao()
  query <- paste("UPDATE farmaco SET nome_farmaco= '",farmaco$nome_farmaco,"' WHERE cod_farmaco= '",id,"'")
  dbSendQuery(conexao,query)
  dbDisconnect(conexao)

  res
}

#* @json
#* @get /tabela_pacientes
get_tabela_pacientes <- function(){
  return(tabela_pacientes())
}

#* @json
#* @get /tabela_tratamentos
get_tabela_tratamentos <- function(){
  return(tabela_tratamentos())
}

#* @json
#* @get /pesquisa_tratamentos
#* @cod_paciente
get_pesquisa_tratamentos <- function(cod_paciente){
  return(pesquisa_tratamentos(cod_paciente))
}

#* @json
#* @get /dados_historico
#* @cod_paciente
get_dados_historico <- function(cod_paciente){
  return(dados_historico(cod_paciente))
}

#* @json
#* @get /get_farmacos
get_farmacos <- function(){
  return(tabela_farmacos())
}

#* @get /previsaoParametros
get_previsao_parametros <- function(codPaciente,concValeDes,dose,intervaloInformado,tempoInfusao){
  dfPaciente <- dados_paciente(codPaciente)
  return(df2json(previsao_parametros(dfPaciente[3],dfPaciente[4],dfPaciente[5],dfPaciente[11],dfPaciente[6],concValeDes,dose,intervaloInformado,tempoInfusao)))
}

#* @json
#* @post /simulacao
get_simulacao_inicial <- function(simulacao){
  dfPaciente <- dados_paciente(simulacao$cod_paciente)
  dfParametros <- previsao_parametros(dfPaciente[3],dfPaciente[4],dfPaciente[5],dfPaciente[11],dfPaciente[6],simulacao$concentracao_desejada,simulacao$dose,simulacao$intervalo,simulacao$duracao_infusao)
  dfSim <- simulacao(simulacao$dose,simulacao$duracao_infusao,dfParametros[15],dfParametros[13],simulacao$quantidade_doses,dfParametros[21])
  return(dfSim)
}

abre_conexao <- function(){
  conexao <- dbConnect(MySQL(),user="0051085",password="1234",dbname="pksoft", host="ceted.feevale.br", port=3306)
  return(conexao)
}

#todas as funções com acesso a banco de dados para retorno do GET

tabela_farmacos <- function(){
  conexao <- abre_conexao()
  query_farmaco <- paste("SELECT * FROM farmaco")
  rsFarmaco <- dbSendQuery(conexao,query_farmaco)
  data_farmaco <- fetch(rsFarmaco, n=10)
  df_farmaco <- data.frame(data_farmaco)
  dbDisconnect(conexao)
  return(df_farmaco)
}

tabela_pacientes <- function(){
  abriu_conexao <- abre_conexao()
  rs_paciente = dbSendQuery(abriu_conexao,"SELECT * FROM paciente")
  data_paciente = fetch(rs_paciente,n = 50)
  df_paciente <- data.frame(data_paciente)
  dbDisconnect(abriu_conexao)
  return(df_paciente)
}

tabela_tratamentos <- function(){
  abriu_conexao <- abre_conexao()
  rs_tratamento = dbSendQuery(abriu_conexao,"SELECT p.nome_paciente, f.nome_farmaco, t.cod_tratamento FROM paciente p INNER JOIN tratamento t ON t.cod_paciente = p.cod_paciente INNER JOIN farmaco f ON f.cod_farmaco = t.cod_farmaco")
  data_tratamento = fetch(rs_tratamento,n=50)
  df_tratamento = data.frame(data_tratamento)
  dbDisconnect(abriu_conexao)
  return(df_tratamento)
}

pesquisa_tratamentos <- function(cod_paciente){
  abriu_conexao <- abre_conexao()
  query = paste("SELECT p.nome_paciente, f.nome_farmaco, t.cod_tratamento, t.cod_farmaco, t.cod_paciente FROM paciente p INNER JOIN tratamento t ON t.cod_paciente = p.cod_paciente INNER JOIN farmaco f ON f.cod_farmaco = t.cod_farmaco WHERE p.cod_paciente = ",cod_paciente)
  rs_tratamento = dbSendQuery(abriu_conexao,query)
  data_tratamento = fetch(rs_tratamento,n=50)
  df_tratamento = data.frame(data_tratamento)
  dbDisconnect(abriu_conexao)
  return(df_tratamento)
}

dados_historico <- function(cod_paciente){
  query <- paste("select h.*, f.nome_farmaco FROM historico h, tratamento t, paciente p, farmaco f",
    "WHERE h.cod_tratamento = t.cod_tratamento",
    "AND t.cod_paciente = p.cod_paciente",
    "AND t.cod_farmaco = f.cod_farmaco",
    "AND p.cod_paciente = ", cod_paciente)
  abriu_conexao <- abre_conexao()
  rs = dbSendQuery(abriu_conexao,query)
  data = fetch(rs,n=50)
  df <- data.frame(data)
  dbDisconnect(abriu_conexao)

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
  pacientePD <- PD(pacientePCI,pacientePeso)
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

simulacao <- function(dose,tempoInfusao,pacienteVdb,pacienteCl,pacienteQtd,pacienteIntervalo){
  eixo_y <- seq(0.1,120,0.1)
  pos_curva <- 1
  #parametros$simulacao_curva <- mapply(CPT,input$dose,parametros$simulacao_valor_alfa,input$tinf,parametros$eixo_y,parametros$simulacao_valor_beta,parametros$simulacao_valor_vc,parametros$simulacao_valor_vdb,input$dose, parametros$simulacao_valor_cl)
  simulacao_curva <- mapply(CPT,dose,tempoInfusao,eixo_y,pacienteVdb,pacienteCl)
  simulacao_curva_final <- sobreposicao(simulacao_curva,pacienteIntervalo, pacienteQtd)

  return(data.frame(simulacao_curva_final,eixo_y))
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