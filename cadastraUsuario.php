<?php
	include("connectDB.php");
	if($_SERVER["REQUEST_METHOD"] == "POST"){
		$myusername = $_POST['username'];
		$mypassword = $_POST['password'];
	}
?>