<?php
require('config.php');
?>

<meta charset="UTF-8" /> <center> 
<h2> Panel inspektora - przeglądarka kosztorysów </h2> </center> 

<?php
if(!isset($_GET["kid"])) {
	$s = $pdo->query('SELECT id FROM kosztorys WHERE wyceniono IS NOT NULL ORDER BY id LIMIT 1');

	foreach($s as $row)
		$_GET["kid"] = $row[id];
	
	if(!isset($_GET["kid"]))
		die("Brak kosztorysów w bazie");
}

if(isset($_GET["select"]) && isset($_GET["id"]) && isset($_GET["kid"])) {
	try { 	
	$s = $pdo->prepare('SELECT clone_kosztorys(:kid) AS newid'); 
	$s->bindValue(':kid', $_GET["kid"], PDO::PARAM_STR);
	$s->execute();
	$res = $s->fetch(); 

	$s = $pdo->prepare('UPDATE zamowienie SET kosztorys = :newid WHERE id = :id'); 
	$s->bindValue(':id', $_GET["id"], PDO::PARAM_STR);
	$s->bindValue(':newid', $res['newid'], PDO::PARAM_INT);
	$s->execute();
	 
	} catch(PDOException $e) {
	die("Nie udało się dodać kosztorysu do bazy");
	}
?>
Wybrano kosztorys. <a href="insp.php"> Powrót do panelu </a>
<?php
} else {
	$prev = $_GET["kid"];
	$next = $_GET["kid"];

	try {
		// Wartość prev
		$s = $pdo->prepare('SELECT id FROM kosztorys K WHERE K.id < :kid AND K.wyceniono IS NOT NULL ORDER BY K.id DESC LIMIT 1'); 
		$s->bindValue(':kid', $_GET["kid"], PDO::PARAM_STR);
		$s->execute();

		if($row = $s->fetch())
			$prev = $row['id'];

		// Wartość next
		$s = $pdo->prepare('SELECT id FROM kosztorys K WHERE K.id > :kid AND K.wyceniono IS NOT NULL ORDER BY K.id ASC LIMIT 1'); 
		$s->bindValue(':kid', $_GET["kid"], PDO::PARAM_STR);
		$s->execute();

		if($row = $s->fetch())
			$next = $row['id'];
	} catch(PDOException $e) {
		die("Błąd bazy");
	}
?>

<?php
	if($prev != $_GET["kid"]) {
?>
<a href="insp_src.php?id=<?php echo($_GET["id"]); ?>&kid=<?php echo($prev);?>">Poprzedni kosztorys</a>
<?php } ?>
<a href="insp_src.php?id=<?php echo($_GET["id"]); ?>&kid=<?php echo($_GET["kid"]);?>&select">Wybieram ten kosztorys</a>
<?php
	if($next != $_GET["kid"]) {
?>
<a href="insp_src.php?id=<?php echo($_GET["id"]); ?>&kid=<?php echo($next);?>">Następny kosztorys</a>
<?php } ?>

<br> <br>

<table border=3px>
<tr>
	<td> Nazwa </td>
	<td> Koszt </td>
</tr>
<?php
	try {	
	$s = $pdo->prepare('SELECT * FROM prace WHERE kosztorys = :kid'); 
	$s->bindValue(':kid', $_GET["kid"], PDO::PARAM_STR);
	$s->execute();

	while($row = $s->fetch()) {
?>
<tr>
	<td> <?php echo($row['nazwa']); ?> </td>
	<td> <?php echo(floor($row['koszt']/100).'.'.$row['koszt']%100); ?> </td>
</tr>
<?php
	}

	} catch(PDOException $e) {
	die("Błąd bazy");
	}
}
?>
</table>
