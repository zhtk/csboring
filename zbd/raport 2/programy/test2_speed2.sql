DECLARE
tbl INTEGER;
ixb INTEGER;
ix1k INTEGER;
ix2k INTEGER;
ix3k INTEGER;
ixz INTEGER;

par1 INTEGER;
par2 INTEGER;

CURSOR testquery (p1 NUMBER, p2 NUMBER) IS
  SELECT /*+ INDEX (testy test_ix)*/ a, b, c
  FROM testy
  WHERE p1 <= a AND a <= p2;

BEGIN
  par1 := dbms_random.value(0, 1000000);
  par2 := dbms_random.value(par1, 1000000);

  tbl := dbms_utility.get_time;
  FOR Cnt IN 1..100000 LOOP
  OPEN testquery(par1, par2);
  CLOSE testquery;
  END LOOP;
  tbl := dbms_utility.get_time - tbl;
  
  -- Bez kompresji
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c)';
  ixb := dbms_utility.get_time;
  FOR Cnt IN 1..100000 LOOP
  OPEN testquery(par1, par2);
  CLOSE testquery;
  END LOOP;
  ixb := dbms_utility.get_time - ixb;
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  -- Kolumna 1
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c) COMPRESS 1';
  ix1k := dbms_utility.get_time;
  FOR Cnt IN 1..100000 LOOP
  OPEN testquery(par1, par2);
  CLOSE testquery;
  END LOOP;
  ix1k := dbms_utility.get_time - ix1k;
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  -- Kolumna 2
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c) COMPRESS 2';
  ix2k := dbms_utility.get_time;
  FOR Cnt IN 1..100000 LOOP
  OPEN testquery(par1, par2);
  CLOSE testquery;
  END LOOP;
  ix2k := dbms_utility.get_time - ix2k;
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  -- Kolumna 3
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c) COMPRESS 3';
  ix3k := dbms_utility.get_time;
  FOR Cnt IN 1..100000 LOOP
  OPEN testquery(par1, par2);
  CLOSE testquery;
  END LOOP;
  ix3k := dbms_utility.get_time - ix3k;
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  -- Zaawansowana kompresja
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c) COMPRESS ADVANCED LOW';
  ixz := dbms_utility.get_time;
  FOR Cnt IN 1..100000 LOOP
  OPEN testquery(par1, par2);
  CLOSE testquery;
  END LOOP;
  ixz := dbms_utility.get_time - ixz;
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';

  INSERT INTO wyniki2b VALUES (tbl, ixb, ix1k, ix2k, ix3k, ixz);
END;
/
