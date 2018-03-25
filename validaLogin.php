<?php
	include("connectDB.php");
	

	if($_SERVER["REQUEST_METHOD"] == "POST"){

		$myusername = $_POST['username'];
		$mypassword = $_POST['password'];

		$sql = mysqli_query($conexao,"SELECT nameUser, passwordUser FROM users WHERE nameUser = '$myusername' AND passwordUser = '$mypassword'") or die("Erro");
		$dados = mysqli_fetch_assoc($sql);

		if($dados['nameUser'] == $myusername && $dados['passwordUser'] == $mypassword){
			session_start();
			$_SESSION['sess_user'] = $myusername;
			header("Location: ../PKSoft/Index.html");
		}

		
		#$result = mysqli_execute($sql);
		#$numrows = mysqli_num_rows($result);

		#if($numrows==1){
		#	while($row=mysql_fetch_assoc($sql)){
		#		$dbusername=$row['nameUser'];  
    	#		$dbpassword=$row['passwordUser'];
    	#		
    	#		session_register($myusername);
		#		$_SESSION['login_user'] = $myusername;
#
		#		header("location: Index.html");  
		#	}
		#} else {
		#	echo "Usuario ou senha invalidos!";
		#}
	}
?>