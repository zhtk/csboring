DECLARE
tbl INTEGER;
ixb INTEGER;
ix1k INTEGER;
ix2k INTEGER;
ix3k INTEGER;
ixz INTEGER;
BEGIN
  dbms_stats.set_global_prefs('CONCURRENT', 'FALSE');
  -- dbms_stats.gather_schema_stats('system');
  
  SELECT SUM(bytes) INTO tbl
  FROM dba_segments
  WHERE segment_type IN ('TABLE', 'TABLE PARTITION', 'TABLE SUBPARTITION')
  AND segment_name = 'TESTY';
  
  -- Bez kompresji
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c)';
  SELECT SUM(s.bytes) INTO ixb
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  -- Kolumna 1
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c) COMPRESS 1';
  SELECT SUM(s.bytes) INTO ix1k
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  -- Kolumna 2
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c) COMPRESS 2';
  SELECT SUM(s.bytes) INTO ix2k
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  -- Kolumna 3
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c) COMPRESS 3';
  SELECT SUM(s.bytes) INTO ix3k
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  -- Zaawansowana kompresja
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b, c) COMPRESS ADVANCED LOW';
  SELECT SUM(s.bytes) INTO ixz
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';

  INSERT INTO wyniki1 VALUES (tbl, ixb, ix1k, ix2k, ix3k, ixz);
END;
/
