<?php
$username = 'admin';
$password = 'admin';

$pdo = 0;

try{
	$pdo = new PDO('pgsql:host=labdb; dbname=bd;', $username, $password );
	$pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch(PDOException $e) {
	die('Połączenie z BD nie mogło zostać utworzone.<br />');
}

?>
