<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel kierownika </h2> </center> 
<strong> Lista oczekujących zleceń: </strong> <br>

<table border=3px>
<tr>
<td> <b>ID</b> </td>
<td> <b>Nr. kosztorysu</b> </td>
<td> <b>Zlecający</b> </td>
<td> Możliwe akcje </td>
</tr>

<?php

if(isset($_GET['done'])) {
	try {
		$s = $pdo->prepare('DELETE FROM zlecenie WHERE id = :done'); 
		$s->bindValue(':done', $_GET['done'], PDO::PARAM_INT);
		$s->execute();
	} catch(PDOException $e) {
		die("Nie udało się usunąć zlecenia");
	}

}

$s = $pdo->query('SELECT Z.id, Z.kosztorys, K.imie, K.nazwisko FROM zlecenie Z INNER JOIN klient K ON Z.klient = K.id ORDER BY id');

foreach($s as $row) {
	echo('<tr>');

	echo('<td>'.$row['id'].'</td>');
	echo('<td>'.$row['kosztorys'].'</td>');
	echo('<td>'.$row['imie'].' '.$row['nazwisko'].'</td>');
	echo('<td><a href=kier_lista.php?id='.$row['kosztorys'].'>Zobacz listę prac</a><br><a href=kier.php?done='.$row['id'].'>Wykonane</a></td>');
	echo('</tr>');
}
?>
</table>

