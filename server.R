# server.R

#library(plumber)
#library(jsonlite)
#library(df2json)
#library(RMySQL)
#r <- plumb("server.R")
#r$run(port=8000)

#GET:
#tabela pacientes, tabela tratamentos (por paciente), tabela histórico (por tratamento), tabela fármacos
#cada função do get tem uma função de acesso ao banco relacionada
#pesquisa_pacientes, pesquisa_tratamentos, pesquisa_historico
#dados_paciente para retornar as infos daquele paciente
#avançado: simulação

#POST:
#novo paciente
#update paciente
#remover paciente
#novo tratamento
#update tratamento
#remover tratamento
#novo dado histórico
#remover dado histórico

#agora no início eu insiro todas as funções do GET

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @post /testePost
testePost <- function(teste){
  fileConn<-file("output.txt")
  writeLines(teste, fileConn)
  close(fileConn)
  return("Ma oe")
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

abre_conexao <- function(){
  conexao <- dbConnect(MySQL(),user="0130841",password="zforzachariah",dbname="farmacocinetica", host="ceted.feevale.br", port=3306)
  return(conexao)
}

#todas as funções com acesso a banco de dados para retorno do GET

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