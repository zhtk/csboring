<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel Klienta - wybierz prace do wykonania </h2> </center> 

<?php
// Dokonaj zamiany stanu
if(isset($_GET['switch'])) {
	try {
		$s = $pdo->prepare('UPDATE prace SET czy_akceptowany = NOT czy_akceptowany WHERE id = :id'); 
		$s->bindValue(':id', $_GET['switch'], PDO::PARAM_INT);
		$s->execute();
	} catch(PDOException $e) {
		die("Nie udało się ustawić stanu");
	}
}

// Reszta kodu
if(isset($_GET["gotowe"])) {
	try {
	$s = $pdo->prepare('UPDATE kosztorys SET przyjeto = current_timestamp WHERE id = :kid'); 
	$s->bindValue(':kid', $_GET["kid"], PDO::PARAM_INT);
	$s->execute();
	} catch(PDOException $e) {
	die("Nie udało się zaklepać daty");
	}
?>
	Przyjęto do wykonania. <a href="kli.php?kliid=<?php echo($_GET['kliid']);?>"> Powrót do panelu </a>
<?php } else { ?>
<a href="kli_wy.php?kliid=<?php echo($_GET['kliid']);?>&kid=<?php echo($_GET['kid']);?>&gotowe"> Skończ pracę i zatwierdź kosztorys </a><br>
<a href="kli.php?kliid=<?php echo($_GET['kliid']);?>"> Potem się tym zajmę </a><br><br>
<strong> Lista prac: </strong> <br>

<table border=3px>
<tr>
<td> <b>Opis</b> </td>
<td> <b>Koszt</b> </td>
<td> <b>Czy akceptowano</b> </td>
<td> <b>Akcje</b> </td>
</tr>

<?php

$s = $pdo->prepare('SELECT id, koszt, nazwa, czy_akceptowany FROM prace WHERE kosztorys = :kid ORDER BY id');
$s->bindValue(':kid', $_GET["kid"], PDO::PARAM_INT);
$s->execute();

foreach($s as $row) {
	echo('<tr>');
	echo('<td>'.$row['nazwa'].'</td>');
	echo('<td>'.floor($row['koszt']/100).','.($row['koszt']%100).'</td>');
	echo('<td>'.($row['czy_akceptowany'] ? "Tak" : "Nie").'</td>');

	echo("<td><a href=\"kli_wy.php?kliid=".$_GET['kliid']."&kid=".$_GET['kid']."&switch=".$row['id']."\"> Zamień stan </a> </td>");
	echo('</tr>');
}
?>

</table>

<?php
}
?>

