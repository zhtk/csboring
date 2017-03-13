<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel BOK </h2> </center> 

<a href="bok_add_k.php">Dodaj klienta do bazy</a> <br> <br>
<strong> Lista klientów: </strong> <br>

<table border=3px>
<tr>
<td> <b>ID</b> </td>
<td> <b>Imię</b> </td>
<td> <b>Nazwisko</b> </td>
<td> <b>Dane dodatkowe</b> </td>
<td> Możliwe akcje </td>
</tr>

<?php

$s = $pdo->query('SELECT * FROM klient');

foreach($s as $row) {
	echo('<tr>');

	echo('<td>'.$row['id'].'</td>');
	echo('<td>'.$row['imie'].'</td>');
	echo('<td>'.$row['nazwisko'].'</td>');
	echo('<td>'.$row['dane_dodatkowe'].'</td>');
	echo('<td><a href=bok_add_z.php?id='.$row['id'].'>dodaj zamówienie</a></td>');
	echo('</tr>');
}

?>

</table>
