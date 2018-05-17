# server.R

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @post /login
login <- function(usuario){
  print(usuario)
  login <- json2df(usuario)
  if(login[1] == 'admin' & login[2] == 'admin')
    return(TRUE)
  else
    return(FALSE)
}

#* @post /novo_paciente
novo_paciente <- function(paciente){
  conexao <- abre_conexao()
  query <- paste("INSERT INTO paciente(nome_paciente,cpf_paciente,nascimento_paciente,genero_paciente,",
    "peso_paciente,altura_paciente,cr_paciente,unid_int_paciente,observacao_paciente,",
    "rg_paciente,telefone_paciente,agente_saude) VALUES('",
    paciente$nome_paciente,"',",paciente$cpf_paciente,",'",paciente$nascimento_paciente,
    "',",paciente$genero_paciente,",",paciente$peso_paciente,",",paciente$altura_paciente,
    ",",paciente$cr_paciente,",'",paciente$unid_int_paciente,"','",paciente$observacao_paciente,
    "',",paciente$rg_paciente,",",paciente$telefone_paciente,",'",paciente$agente_saude,"')")

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
    "unid_int_paciente=",paciente$unid_int_paciente,",",
    "observacao_paciente='",paciente$observacao_paciente,"',",
    "rg_paciente='",paciente$rg_paciente,"',",
    "telefone_paciente='",paciente$telefone_paciente,"',",
    "agente_saude= '",paciente$agente_saude,"'",
    "WHERE cod_paciente=",id)

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
                 ",'",historico$data_hora_historico,"',",historico$cod_tratamento,")")
  dbSendQuery(conexao,query)
  disconnect(conexao)
}

#* @post /alterar_historico/<id>
alterar_historico <- function(id, historico){
  df_evento <- json2df(evento)
  conexao <- abre_conexao()
  query <- paste("UPDATE historico SET ",
                "atributo_historico = '",historico$atributo_historico,"'",
                ",valor_historico = ",historico$valor_historico,
                ",data_hora_historico = '",historico$data_hora_historico,"'",
                ",cod_tratamento = ",historico$cod_tratamento,
                "WHERE cod_historico = ",id)
  dbSendQuery(conexao,query)
  disconnect(conexao)
}

#* @preempt cors
#* @post /novo_farmaco
novo_farmaco <- function(farmaco, res){
  conexao <- abre_conexao()
  query <- paste("INSERT INTO farmaco(nome_farmaco) VALUES('",farmaco$nome_farmaco,"')")
  dbSendQuery(conexao,query)

  res$status = 200
  res$body = 'Fármaco inserido com sucesso'

  dbDisconnect(conexao)

  return(res)
}

#* @preempt cors
#* @put /alterar_farmaco/<id>
alterar_farmaco <- function(id, farmaco, res){
  conexao <- abre_conexao()
  query <- paste("UPDATE farmaco SET nome_farmaco= '",farmaco$nome_farmaco,"' WHERE cod_farmaco= '",id,"'")
  dbSendQuery(conexao,query)

  res$status = 200
  res$body = 'Fármaco alterado com sucesso'

  dbDisconnect(conexao)

  return(res)
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

abre_conexao <- function(){
  conexao <- dbConnect(MySQL(),user="0130841",password="zforzachariah",dbname="farmacocinetica", host="ceted.feevale.br", port=3306)
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

  return(df)
}