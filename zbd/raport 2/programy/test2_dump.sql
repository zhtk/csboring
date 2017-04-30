set pagesize 0
set trimspool off
-- set linesize 2000

spool test2_wyniki1.csv

SELECT tbl||','||ixb||','||ix1k||','||ix2k||','||ix3k||','||ixz
FROM wyniki1;

spool off

spool test2_wyniki2a.csv

SELECT tbl||','||ixb||','||ix1k||','||ix2k||','||ix3k||','||ixz
FROM wyniki2a;

spool off

spool test2_wyniki2b.csv

SELECT tbl||','||ixb||','||ix1k||','||ix2k||','||ix3k||','||ixz
FROM wyniki2b;

spool off

spool test2_wyniki2c.csv

SELECT tbl||','||ixb||','||ix1k||','||ix2k||','||ix3k||','||ixz
FROM wyniki2c;

spool off
