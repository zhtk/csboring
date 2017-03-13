<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel klienta </h2> </center> 
<?php
if(!isset($_GET['kliid'])) {
?>
<strong> Zaloguj się jako: </strong> <br>

<table border=3px>
<tr>
<td> <b>ID</b> </td>
<td> <b>Imię i nazwisko</b> </td>
<td> Możliwe akcje </td>
</tr>

<?php

$s = $pdo->query('SELECT id, imie, nazwisko FROM klient');

foreach($s as $row) {
	echo('<tr>');

	echo('<td>'.$row['id'].'</td>');
	echo('<td>'.$row['imie'].' '.$row['nazwisko'].'</td>');
	echo('<td><a href=kli.php?kliid='.$row['id'].'> Zaloguj </a></td>');
	echo('</tr>');
}

?>

</table>

<br><br>

<?php } else { ?>

<strong> Lista zleceń do zaakceptowania: </strong> <br>

<table border=3px>
<tr>
<td> <b>Opis</b> </td>
<td> <b>Akcje</b> </td>
</tr>

<?php

$s = $pdo->prepare('SELECT Z.opis, Z.id, K.id AS kid FROM zamowienie Z INNER JOIN kosztorys K ON K.id = Z.kosztorys WHERE Z.zlecajacy = :kliid AND Z.kosztorys IS NOT NULL AND K.wyceniono IS NOT NULL AND K.przyjeto IS NULL ORDER BY Z.id ASC');
$s->bindValue(':kliid', $_GET['kliid'], PDO::PARAM_STR);
$s->execute();

foreach($s as $row) {
	echo('<tr>');
	echo('<td>'.$row['opis'].'</td>');
	echo('<td><a href=kli_wy.php?kliid='.$_GET['kliid'].'&kid='.$row['kid'].'> Zobacz wycenę </a>'.'</td>');
	echo('</tr>');
}

?>

</table>

<?php } ?>
