-- Usunięcie starych tabel
DROP TABLE klient CASCADE;
DROP TABLE pracownik CASCADE;
DROP TABLE zamowienie CASCADE;
DROP TABLE zlecenie CASCADE;
DROP TABLE prace CASCADE;
DROP TABLE kosztorys CASCADE;

-- Utworzenie potrzebnych tabel
CREATE TABLE klient (
	id SERIAL PRIMARY KEY,
	imie varchar(100) NOT NULL,
	nazwisko varchar(100) NOT NULL,
	dane_dodatkowe varchar(5000)
);

CREATE TABLE pracownik (
	id SERIAL PRIMARY KEY,
	imie varchar(100) NOT NULL,
	nazwisko varchar(100) NOT NULL,
	dane_dodatkowe varchar(5000),
	-- rola pracownika przyjmuje wartości:
	-- 1 -> rzeczoznawca
	-- 2 -> kierownik robót
	-- .......
	rola int
);

CREATE TABLE kosztorys (
	id SERIAL PRIMARY KEY,
	rzeczoznawca int references pracownik(id),
	wyceniono TIMESTAMP, -- Uzupełnia rzeczoznawca
	przyjeto TIMESTAMP, -- Uzupełnia klient
	deadline TIMESTAMP -- Ustawia inpektor
);

CREATE TABLE prace (
	id SERIAL PRIMARY KEY,
	nazwa varchar(200) NOT NULL,
	koszt int NOT NULL,
	czy_akceptowany boolean NOT NULL DEFAULT FALSE,
	kosztorys int NOT NULL references kosztorys(id)
);

CREATE TABLE zamowienie (
	id SERIAL PRIMARY KEY,
	zlecajacy int NOT NULL references klient(id),
	opis varchar(5000) NOT NULL,
	kosztorys int references kosztorys(id)
);

CREATE TABLE zlecenie (
	id SERIAL PRIMARY KEY,
	klient int NOT NULL references klient(id),
	-- kierownik int references pracownik(id),
	kosztorys int NOT NULL references kosztorys(id)
);

-- Wyzwalacz który po zatwierdzeniu projektu robót (zamówienia) przez klienta 
-- utworzy zlecenie
DROP TRIGGER potwierdzono ON kosztorys;
DROP FUNCTION IF EXISTS zatwierdzaj();
DROP FUNCTION IF EXISTS copy_order(int);

CREATE FUNCTION copy_order(int) RETURNS void AS $$
-- Argumentem jest id zlecenia do skopiowania
DECLARE
	klient int;
	kosztorys int;
BEGIN
	SELECT Z.zlecajacy, Z.kosztorys INTO klient, kosztorys
       	FROM zamowienie Z WHERE Z.id = $1;

	IF NOT FOUND THEN
		RAISE EXCEPTION 'Nie ma zamowienia nr %', $1;
	END IF;
	
	INSERT INTO zlecenie (klient, kosztorys) VALUES (klient, kosztorys);
END;
$$ LANGUAGE 'plpgsql';

CREATE FUNCTION zatwierdzaj() RETURNS TRIGGER AS $$
DECLARE 
	zid int;
BEGIN
	-- Sprawdź czy pojawił się nowy timestamp
	IF OLD.przyjeto IS NOT NULL OR NEW.przyjeto IS NULL THEN
		RETURN NEW;
	END IF;

	-- Znajdź zamówienie powiązane z kosztorysem
	SELECT Z.id INTO zid
	FROM zamowienie Z WHERE Z.kosztorys = NEW.id;

	IF NOT FOUND THEN
		RAISE EXCEPTION 'Nie ma zamowienia nr %', $1;
	END IF;
	
	-- Przenieś je do tabeli zlecenie
	PERFORM copy_order(zid);

	RETURN NEW;
END;
$$ LANGUAGE 'plpgsql';

CREATE TRIGGER potwierdzono 
AFTER UPDATE ON kosztorys
FOR EACH ROW EXECUTE PROCEDURE zatwierdzaj();

-- Funkcja składowana która bierze id kosztorysu i kopiuje go 
-- na potrzeby pracy wykonywanej przez inspektora

DROP FUNCTION IF EXISTS clone_kosztorys(int);

CREATE FUNCTION clone_kosztorys(int) RETURNS int AS $$
-- Argumentem jest id kosztorysu do skopiowania
-- Funkcja zwraca id nowego kosztorysu
DECLARE
	nowyid int;
	kur CURSOR FOR SELECT p.nazwa, p.koszt 
	FROM prace p where p.kosztorys = $1;
	
	nazwa prace.nazwa%TYPE;
	koszt prace.koszt%TYPE;
BEGIN
	INSERT INTO kosztorys (rzeczoznawca, wyceniono) 
	VALUES (NULL, current_timestamp)
	RETURNING id INTO nowyid;

	OPEN kur;
	LOOP
		FETCH kur INTO nazwa, koszt;
		
		EXIT WHEN NOT FOUND;
	
		INSERT INTO prace (nazwa, koszt, kosztorys) 
		VALUES (nazwa, koszt, nowyid);
	END LOOP;
	CLOSE kur;

	RETURN nowyid;
END;
$$ LANGUAGE 'plpgsql';
