<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel BOK - dodawanie zamówienia </h2> </center> 

<?php
if(isset($_POST["opis"])) {
	try {
	$s = $pdo->prepare('INSERT INTO zamowienie (zlecajacy, opis, kosztorys) VALUES (:id, :opis, NULL);'); 
	$s->bindValue(':opis', $_POST["opis"], PDO::PARAM_STR);
	$s->bindValue(':id', $_POST["id"], PDO::PARAM_STR);
	$s->execute();
	} catch(PDOException $e) {
	die("Nie udało się dodać zamówienia do bazy");
	}
?>
Dodano zamówienie do bazy. <a href="bok.php"> Powrót do panelu </a>
<?php
} else {
?>
<form action="bok_add_z.php" method="post">
<table>
	<tr> <td>Opis zamówienia:</td><td> <textarea name="opis" cols="80" rows="10"></textarea></td></tr>
</table>
	<input name="id" type="hidden" value="<?php echo($_GET["id"]); ?>"/>
	<input type="submit" value="Dodaj zamówienie" />
</form>
<?php
}
?>

</table>
