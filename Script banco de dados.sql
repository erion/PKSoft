CREATE TABLE paciente (
  cod_paciente int(10) NOT NULL AUTO_INCREMENT,
  nome_paciente varchar(50),
  cpf_paciente varchar(14),
  nascimento_paciente date,
  peso_paciente float,
  altura_paciente float,
  cr_paciente float,
  unid_int_paciente varchar(100),
  observacao_paciente varchar(500),
  rg_paciente int(12),
  telefone_paciente int(14),
  genero_paciente char(1),
  agente_saude varchar(100),
  PRIMARY KEY (cod_paciente)
)

CREATE TABLE farmaco (
  cod_farmaco int(10) NOT NULL AUTO_INCREMENT,
  nome_farmaco varchar(100),
  PRIMARY KEY (cod_farmaco)
)

CREATE TABLE tratamento (
  cod_tratamento int(11) NOT NULL AUTO_INCREMENT,
  cod_farmaco int(10),
  cod_paciente int(10),
  PRIMARY KEY (cod_tratamento),
  FOREIGN KEY (cod_farmaco) REFERENCES farmaco (cod_farmaco),
  FOREIGN KEY (cod_paciente) REFERENCES paciente (cod_paciente)
)

CREATE TABLE historico (
  cod_historico int(11) NOT NULL AUTO_INCREMENT,
  atributo_historico char(1),
  valor_historico float,
  data_hora_historico datetime,
  cod_tratamento int(11),
  PRIMARY KEY (cod_historico),
  FOREIGN KEY (cod_tratamento) REFERENCES tratamento (cod_tratamento)
)

CREATE TABLE simulacao (
  cod_simulacao int(10) NOT NULL AUTO_INCREMENT,
  dose decimal(10,0),
  cvd decimal(10,0),
  duracao_infusao int(10),
  intervalo int(10),
  quantidade_doses int(10),
  data_inicial datetime,
  cod_tratamento int(10),
  PRIMARY KEY (cod_simulacao),
  FOREIGN KEY (cod_tratamento) REFERENCES tratamento (cod_tratamento)
)

CREATE TABLE usuario (
  codigo int(10) NOT NULL AUTO_INCREMENT,
  nome varchar(50),
  login varchar(14),
  senha varchar(255),
  PRIMARY KEY (codigo)
)

