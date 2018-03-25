<?php
	include("connectDB.php");
	
	if($_SERVER["REQUEST_METHOD"] == "POST"){

		$myusername = $_POST['username'];
		$mypassword = $_POST['password'];

		$sql = mysqli_query($conexao,"SELECT nameUser, passwordUser FROM users WHERE nameUser = '$myusername' AND passwordUser = '$mypassword'") or die("Erro na consulta ao banco de dados!");
		$dados = mysqli_fetch_assoc($sql);

		if($dados['nameUser'] == $myusername && $dados['passwordUser'] == $mypassword){
			session_start();
			$_SESSION['sess_user'] = $myusername;
			header("Location: ../PKSoft/Index.html");
		} else{
			header("Location: ../PKSoft/Login.html");
		}
	}
?>