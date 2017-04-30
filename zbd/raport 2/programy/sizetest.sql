DECLARE
tbl INTEGER;
ix1b INTEGER;
ix1k INTEGER;
ix1z INTEGER;
ix2b INTEGER;
ix2k INTEGER;
ix2z INTEGER;
BEGIN
  dbms_stats.set_global_prefs('CONCURRENT', 'FALSE');
  -- dbms_stats.gather_schema_stats('system');
  
  SELECT SUM(bytes) INTO tbl
  FROM dba_segments
  WHERE segment_type IN ('TABLE', 'TABLE PARTITION', 'TABLE SUBPARTITION')
  AND segment_name = 'TESTY';
  
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a)';
  SELECT SUM(s.bytes) INTO ix1b
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a) COMPRESS';
  SELECT SUM(s.bytes) INTO ix1k
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a) COMPRESS ADVANCED LOW';
  SELECT SUM(s.bytes) INTO ix1z
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';

  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b)';
  SELECT SUM(s.bytes) INTO ix2b
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b) COMPRESS';
  SELECT SUM(s.bytes) INTO ix2k
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';
  
  EXECUTE IMMEDIATE 'CREATE INDEX test_ix ON testy (a, b) COMPRESS ADVANCED LOW';
  SELECT SUM(s.bytes) INTO ix2z
  FROM dba_segments s
  WHERE s.segment_name = 'TEST_IX'
  AND   s.segment_type IN ('INDEX', 'INDEX PARTITION', 'INDEX SUBPARTITION');
  EXECUTE IMMEDIATE 'DROP INDEX test_ix';

  INSERT INTO wyniki VALUES (tbl, ix1b, ix1k, ix1z, ix2b, ix2k, ix2z);
END;
/
