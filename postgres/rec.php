<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel rzeczoznawcy </h2> </center> 
<?php
if(!isset($_GET['recid'])) {
?>
<strong> Zaloguj się jako: </strong> <br>

<table border=3px>
<tr>
<td> <b>ID</b> </td>
<td> <b>Imię i nazwisko</b> </td>
<td> Możliwe akcje </td>
</tr>

<?php

$s = $pdo->query('SELECT id, imie, nazwisko FROM pracownik WHERE rola = 1');

foreach($s as $row) {
	echo('<tr>');

	echo('<td>'.$row['id'].'</td>');
	echo('<td>'.$row['imie'].' '.$row['nazwisko'].'</td>');
	echo('<td><a href=rec.php?recid='.$row['id'].'> Zaloguj </a></td>');
	echo('</tr>');
}

?>

</table>

<br><br>

<?php } else { ?>

<strong> Lista niewycenionych zleceń w kolejności od najstarszego: </strong> <br>

<table border=3px>
<tr>
<td> <b>Opis</b> </td>
<td> <b>Akcje</b> </td>
</tr>

<?php

$s = $pdo->prepare('SELECT K.id, Z.opis FROM zamowienie Z INNER JOIN kosztorys K ON K.id = Z.kosztorys WHERE Z.kosztorys IS NOT NULL AND K.wyceniono IS NULL AND K.rzeczoznawca = :recid ORDER BY Z.id ASC');
$s->bindValue(':recid', $_GET['recid'], PDO::PARAM_STR);
$s->execute();

foreach($s as $row) {
	echo('<tr>');
	echo('<td>'.$row['opis'].'</td>');
	echo('<td><a href=rec_wy.php?recid='.$_GET['recid'].'&kid='.$row['id'].'> Wyceń </a>'.'</td>');
	echo('</tr>');
}

?>

</table>

<?php } ?>
