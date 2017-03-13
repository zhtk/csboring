<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel inspektora </h2> </center> 
<strong> Lista oczekujących zleceń: </strong> <br>

<table border=3px>
<tr>
<td> <b>ID</b> </td>
<td> <b>Opis</b> </td>
<td> Możliwe akcje </td>
</tr>

<?php

$s = $pdo->query('SELECT id, opis FROM zamowienie WHERE kosztorys IS NULL');

foreach($s as $row) {
	echo('<tr>');

	echo('<td>'.$row['id'].'</td>');
	echo('<td>'.$row['opis'].'</td>');
	echo('<td><a href=insp_rec.php?id='.$row['id'].'>Przydziel rzeczoznawcę</a><br><a href=insp_src.php?id='.$row['id'].'>Znajdź kosztorys</a></td>');
	echo('</tr>');
}

?>

</table>

<br><br>

<strong> Lista niewycenionych zleceń w kolejności od najstarszego: </strong> <br>

<table border=3px>
<tr>
<td> <b>Opis</b> </td>
<td> <b>Dane rzeczoznawcy</b> </td>
</tr>

<?php

$s = $pdo->query('SELECT Z.opis, p.imie, p.nazwisko FROM zamowienie Z INNER JOIN kosztorys K ON K.id = Z.kosztorys INNER JOIN pracownik p ON p.id = k.rzeczoznawca WHERE Z.kosztorys IS NOT NULL AND K.wyceniono IS NULL AND K.deadline <= CURRENT_TIMESTAMP ORDER BY Z.id ASC');

foreach($s as $row) {
	echo('<tr>');
	echo('<td>'.$row['opis'].'</td>');
	echo('<td>'.$row['imie'].' '.$row['nazwisko'].'</td>');
	echo('</tr>');
}

?>

</table>

