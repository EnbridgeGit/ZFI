FUNCTION ZTR_GET_T028Q_INSTANCE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      TBL_T028Q STRUCTURE  T028Q
*"----------------------------------------------------------------------



**** Get the data.
SELECT *
FROM T028Q
INTO TABLE TBL_T028Q.


ENDFUNCTION.
