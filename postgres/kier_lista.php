<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel Kierownika - prace do wykonania </h2> </center> 

<a href="kier.php"> Potem się tym zajmę </a><br><br>
<strong> Lista prac: </strong> <br>

<table border=3px>
<tr>
<td> <b>Opis</b> </td>
<td> <b>Koszt</b> </td>
<td> <b>Czy akceptowano</b> </td>
</tr>

<?php

$s = $pdo->prepare('SELECT id, koszt, nazwa, czy_akceptowany FROM prace WHERE kosztorys = :kid ORDER BY id');
$s->bindValue(':kid', $_GET["id"], PDO::PARAM_INT);
$s->execute();

foreach($s as $row) {
	echo('<tr>');
	echo('<td>'.$row['nazwa'].'</td>');
	echo('<td>'.floor($row['koszt']/100).','.($row['koszt']%100).'</td>');
	echo('<td>'.($row['czy_akceptowany'] ? "Tak" : "Nie").'</td>');
	echo('</tr>');
}
?>

</table>


