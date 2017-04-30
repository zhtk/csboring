WHENEVER SQLERROR CONTINUE NONE
DROP TABLE wyniki1 PURGE;

WHENEVER SQLERROR CONTINUE NONE
DROP TABLE wyniki2a PURGE;

WHENEVER SQLERROR CONTINUE NONE
DROP TABLE wyniki2b PURGE;

WHENEVER SQLERROR CONTINUE NONE
DROP TABLE wyniki2c PURGE;

WHENEVER SQLERROR CONTINUE NONE
DROP TABLE testy PURGE;

WHENEVER SQLERROR CONTINUE NONE
DROP index test_ix;

CREATE TABLE testy (a NUMBER, b NUMBER, c NUMBER);

-- Zajętość miejsca
CREATE TABLE wyniki1 (tbl NUMBER, ixb NUMBER, 
   ix1k NUMBER,
   ix2k NUMBER,
   ix3k NUMBER, 
   ixz NUMBER);

-- Tabelka na czasy testów wydajnościowych - zapytanie 1
CREATE TABLE wyniki2a (tbl NUMBER, ixb NUMBER,
   ix1k NUMBER, 
   ix2k NUMBER,
   ix3k NUMBER,
   ixz NUMBER);

-- Tabelka na czasy testów wydajnościowych - zapytanie 2
CREATE TABLE wyniki2b (tbl NUMBER, ixb NUMBER,
   ix1k NUMBER, 
   ix2k NUMBER,
   ix3k NUMBER,
   ixz NUMBER);

-- Tabelka na czasy testów wydajnościowych - zapytanie 3
CREATE TABLE wyniki2c (tbl NUMBER, ixb NUMBER,
   ix1k NUMBER, 
   ix2k NUMBER,
   ix3k NUMBER,
   ixz NUMBER);
