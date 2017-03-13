<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel rzeczoznawcy - wyceń zlecenie </h2> </center> 

<?php
if(isset($_GET["gotowe"])) {
	try {
	$s = $pdo->prepare('UPDATE kosztorys SET wyceniono = current_timestamp WHERE id = :kid'); 
	$s->bindValue(':kid', $_GET["kid"], PDO::PARAM_INT);
	$s->execute();
	} catch(PDOException $e) {
	die("Nie udało się zaklepać daty");
	}
?>
	Wyceniono. <a href="rec.php?recid=<?php echo($_GET['recid']);?>"> Powrót do panelu </a>
<?php
} else {
// Jeśli jest taka potrzeba to dodajemy rzeczoznawcę
	if(isset($_POST['nazwa']) && isset($_POST['koszt'])) {
	try {
	$s = $pdo->prepare('INSERT INTO prace (nazwa, koszt, kosztorys) VALUES (:naz, :koszt, :kid)'); 
	$s->bindValue(':naz', $_POST["nazwa"], PDO::PARAM_STR);
	$s->bindValue(':koszt', floor($_POST["koszt"]*100), PDO::PARAM_INT);
	$s->bindValue(':kid', $_GET["kid"], PDO::PARAM_INT);

	$s->execute();
	} catch(PDOException $e) {
	die("Błąd w dodawaniu zlecenia");
	}

	}
?>

<a href="rec_wy.php?recid=<?php echo($_GET['recid']);?>&kid=<?php echo($_GET['kid']);?>&gotowe"> Skończ pracę i zatwierdź kosztorys </a><br><br>
<strong> Lista prac: </strong> <br>
<a href="#add">Dodaj pracę do listy</a><br><br>

<table border=3px>
<tr>
<td> <b>Opis</b> </td>
<td> <b>Koszt</b> </td>
</tr>

<?php

$s = $pdo->prepare('SELECT koszt, nazwa FROM prace WHERE kosztorys = :kid');
$s->bindValue(':kid', $_GET["kid"], PDO::PARAM_INT);
$s->execute();

foreach($s as $row) {
	echo('<tr>');
	echo('<td>'.$row['nazwa'].'</td>');
	echo('<td>'.floor($row['koszt']/100).','.($row['koszt']%100).'</td>');
	echo('</tr>');
}
?>

</table>
<br><br>

<a name="add"></a>
<form action="<?php echo("http://$_SERVER[HTTP_HOST]$_SERVER[REQUEST_URI]"); ?>" method="post">
<b> Dodawanie pracy </b>
<table>
	<tr> <td>Nazwa:</td> <td><input type="text" name="nazwa" /></td> </tr>
	<tr> <td>Koszt:</td> <td><input type="text" name="koszt" /></td> </tr> 
</table>
	<input type="submit" value="Dodaj pracę" />
</form>

<?php
}
?>

</table>
