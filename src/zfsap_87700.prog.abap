*&---------------------------------------------------------------------*
*& Title: Transfer flag prel.posting is set in table RFDT              *
*&---------------------------------------------------------------------*
REPORT ZFSAP .
TABLES: RFDT.
DATA: XFLAG LIKE BOOLE-BOOLE VALUE 'X'.
  EXPORT XFLAG TO DATABASE RFDT(PP) ID 'LF040FWF'.
IF SY-SUBRC NE 0. MESSAGE A016(PN) WITH 'RFDT'. ENDIF.
