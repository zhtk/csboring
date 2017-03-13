<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel inspektora - szukaj rzeczoznawcy </h2> </center> 

<?php
if(isset($_GET["przydziel"])) {
	try {
	$s = $pdo->prepare('INSERT INTO kosztorys (rzeczoznawca, deadline) VALUES (:rec, current_timestamp + interval \'90 seconds\') RETURNING id'); 
	$s->bindValue(':rec', $_GET["przydziel"], PDO::PARAM_INT);
	$s->execute();
	$nowy = $s->fetch();

	$s = $pdo->prepare('UPDATE zamowienie SET kosztorys = :nowy WHERE id = :id'); 
	$s->bindValue(':id', $_GET["id"], PDO::PARAM_INT);
	$s->bindValue(':nowy', $nowy['id'], PDO::PARAM_INT);
	$s->execute();
	
	} catch(PDOException $e) {
	die("Nie udało się ustawić rzeczoznawcy");
	}
?>
Ustawiono rzeczoznawcę. Ma 90 sekund na zrobienie wyceny. <a href="insp.php"> Powrót do panelu </a>
<?php
} else {
// Jeśli jest taka potrzeba to dodajemy rzeczoznawcę
	if(isset($_POST['imie'])) {
	$s = $pdo->prepare('INSERT INTO pracownik (imie, nazwisko, dane_dodatkowe, rola) VALUES (:im, :naz, :dd, 1);'); 
	$s->bindValue(':im', $_POST["imie"], PDO::PARAM_STR);
	$s->bindValue(':naz', $_POST["nazw"], PDO::PARAM_STR);
	$s->bindValue(':dd', $_POST["dd"], PDO::PARAM_STR);

	$s->execute();
	}

// Lista rzeczoznawców	 
?>

<strong> Lista rzeczoznawców: </strong> <br>
<a href="#add">Dodaj rzeczoznawcę</a><br><br>

<table border=3px>
<tr>
<td> <b>ID</b> </td>
<td> <b>Imie</b> </td>
<td> <b>Nazwisko</b> </td>
<td> <b>Dane dodatkowe</b> </td>
<td> Możliwe akcje </td>
</tr>

<?php

$s = $pdo->query('SELECT * FROM pracownik WHERE rola = 1');

foreach($s as $row) {
	echo('<tr>');

	echo('<td>'.$row['id'].'</td>');
	echo('<td>'.$row['imie'].'</td>');
	echo('<td>'.$row['nazwisko'].'</td>');
	echo('<td>'.$row['dane_dodatkowe'].'</td>');
	echo('<td><a href=insp_rec.php?id='.$_GET["id"].'&przydziel='.$row['id'].'>Wybierz</a></td>');
	echo('</tr>');
}
?>

</table>
<br><br>

<a name="add"></a>
<form action="<?php echo("http://$_SERVER[HTTP_HOST]$_SERVER[REQUEST_URI]"); ?>" method="post">
<b> Dodawanie rzeczoznawcy </b>
<table>
	<tr> <td>Imię:</td> <td><input type="text" name="imie" /></td> </tr>
	<tr> <td>Nazwisko:</td><td> <input type="text" name="nazw" /></td> </tr> 
	<tr> <td>Dane dodatkowe:</td><td> <textarea name="dd" cols="60" rows="5"></textarea></td></tr>
</table>
	<input type="submit" value="Dodaj pracownika" />
</form>


<?php
}
?>

</table>
