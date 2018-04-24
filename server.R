# server.R

#library(plumber)
#library(jsonlite)
#library(df2json)
#library(RMySQL)
#setwd("C:/Temp/drive-download-20180419T204420Z-001")
#r <- plumb("server.R")
#r$run(port=8000)


#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @post /testePost
testePost <- function(a){
  df <- json2df(a)
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

abre_conexao <- function(){
  conexao <- dbConnect(MySQL(),user="0130841",password="zforzachariah",dbname="farmacocinetica", host="ceted.feevale.br", port=3306)
  return(conexao)
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



