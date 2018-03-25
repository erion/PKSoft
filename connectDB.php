<?php
	define('DB_SERVER','localhost:3306');
	define('DB_USERNAME','pksoft');
	define('DB_PASSWORD','pksoft');
	define('DB_DATABASE','pksoft');
	$conexao = mysqli_connect(DB_SERVER,DB_USERNAME,DB_PASSWORD,DB_DATABASE);
	$banco = mysqli_select_db($conexao,'pksoft');
?>