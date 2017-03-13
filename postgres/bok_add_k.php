<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel BOK - dodawanie klienta </h2> </center> 

<?php
if(isset($_POST["nazw"])) {
	try {
	$s = $pdo->prepare('INSERT INTO klient (imie, nazwisko, dane_dodatkowe) VALUES (:im, :naz, :dd);'); 
	$s->bindValue(':im', $_POST["imie"], PDO::PARAM_STR);
	$s->bindValue(':naz', $_POST["nazw"], PDO::PARAM_STR);
	$s->bindValue(':dd', $_POST["dd"], PDO::PARAM_STR);

	$s->execute();
	} catch(PDOException $e) {
	die("Nie udało się dodać klienta do bazy");
	}
?>
Dodano klienta do bazy. <a href="bok.php"> Powrót do panelu </a>
<?php
} else {
?>
<form action="bok_add_k.php" method="post">
<table>
	<tr> <td>Imię:</td> <td><input type="text" name="imie" /></td> </tr>
	<tr> <td>Nazwisko:</td><td> <input type="text" name="nazw" /></td> </tr> 
	<tr> <td>Dane dodatkowe:</td><td> <textarea name="dd" cols="60" rows="5"></textarea></td></tr>
</table>
	<input type="submit" value="Dodaj klienta" />
</form>
<?php
}
?>

</table>
