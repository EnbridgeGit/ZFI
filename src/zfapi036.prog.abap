REPORT ZFAPI036 MESSAGE-ID ZS.

************************************************************************
*  Project:     Mercator replacement                                   *
*  Interface:   Banner-AP customer rebate (IFAP024)                    *
*  Author:      Mohammad T. Khan                                       *
*  Date:        August, 2005                                           *
*  Description:                                                        *
*     - The purpose of this program is to map the data for Banner-AP   *
*       customer rebate interface. It will replace the mapping through *
*       Mercator.                                                      *
*                                                                      *
*  Modifications:                                                      *
*  2006/04/05 gymana 4.7upg Modified initialization of BGR00, BBKPF, & *
*                           ZBSEG table structures so no changes have  *
*                           be made to this ABAP when changes are made *
*                           to any of the tables.                      *
************************************************************************
TABLES:  DD03L.

DATA: MSG(100) TYPE C.

*-----------------------------------------------------------------------
FIELD-SYMBOLS: <F1>.
DATA:    CHAR(21)    TYPE c,
         NODATA(1)   VALUE '/'.
*-----------------------------------------------------------------------

*Input file record
DATA: BEGIN OF INREC,
        CHRQ_NUM(15)    TYPE C,
        TRAN_TYPE(2)    TYPE C,
        CUST_CODE(9)    TYPE C,
        PREM_CODE(7)    TYPE C,
        CUST_NAME(90)   TYPE C,
        ADDRESS01(60)   TYPE C,
        ADDRESS02(60)   TYPE C,
        ADDRESS03(60)   TYPE C,
        CITY_NAME(20)   TYPE C,
        STAT_CODE(3)    TYPE C,
        ZIP_CODE(10)    TYPE C,
        NATION(28)      TYPE C,
        CHECK_DATE(11)  TYPE C,
        NET_AMOUNT(11)  TYPE C,
        USER_ID(30)     TYPE C,
        ACTV_DATE(11)   TYPE C,
        SEQ_NUM(10)     TYPE C,
        STREET_NAME(30) TYPE C,
        STREET_NUM(12)  TYPE C,
        PDIR_CODE(2)    TYPE C,
        SSFX_CODE(6)    TYPE C,
        PDIR_POST(2)    TYPE C,
        PREM_ZIPC(10)   TYPE C,
        PREM_CITY(20)   TYPE C,
        PREM_STATE(3)   TYPE C,
        PREM_SDSCR(30)  TYPE C,
        PREM_UTYP(6)    TYPE C,
        PREM_UNIT(6)    TYPE C,
        LAND_CODE(4)    TYPE C,
        PREM_UTDESC(35) TYPE C,
*        ATTN_LINE(30)   TYPE C,

      END OF INREC.

*Output file Session Header
DATA: BEGIN OF Z_BGR00.
        INCLUDE STRUCTURE BGR00.
DATA: END OF Z_BGR00.

*Output file Document header
DATA: BEGIN OF Z_BBKPF.
        INCLUDE STRUCTURE BBKPF.
DATA: END OF Z_BBKPF.

*Output file Document line item
DATA: BEGIN OF Z_ZBSEG.
        INCLUDE STRUCTURE ZBSEG.
DATA: END OF Z_ZBSEG.


DATA: WRK_SYMBOLIC(4) TYPE C VALUE '$sys',
      WRK_AMOUNT      TYPE P DECIMALS 2,
      KOUNT           TYPE I,
      REC3(73)        TYPE C.


SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP 1.
PARAMETER:  INFILE LIKE FILENAME-FILEEXTERN
              DEFAULT '/usr/sap/interfaces/$sys/BANNER/zbis105.chk'.
SELECTION-SCREEN SKIP 1.
PARAMETER: OUTFILE LIKE FILENAME-FILEEXTERN
              DEFAULT '/usr/sap/interfaces/$sys/BANNER/zbis105.sap'.
SELECTION-SCREEN SKIP 1.
PARAMETER:
 P_GROUP  LIKE  BGR00-GROUP DEFAULT 'ZAP-BAN-REBA',
 P_TCODE(4)                 DEFAULT 'FB01',
 P_BLART  LIKE  BBKPF-BLART DEFAULT 'K7',
 P_BUKRS  LIKE  BBKPF-BUKRS DEFAULT 'UGL',
 P_WAERS  LIKE  BBKPF-WAERS DEFAULT 'CAD',
 P_BKTXT  LIKE  BBKPF-BKTXT DEFAULT 'BANNER CUSTOMER REBATE',
 P_HKONTC LIKE  ZBSEG-HKONT DEFAULT 'OTREBA', "GL Acct-Credit Entry (31)
 P_HKONTD LIKE  ZBSEG-HKONT DEFAULT '179000'. "GL Acct-Debit Entry 40
SELECTION-SCREEN END OF BLOCK BOX1.

AT SELECTION-SCREEN OUTPUT.
  REPLACE WRK_SYMBOLIC WITH SY-SYSID INTO: INFILE, OUTFILE.
  CONDENSE: INFILE, OUTFILE NO-GAPS.


START-OF-SELECTION.
*Replace system id in the file paths
  REPLACE WRK_SYMBOLIC WITH SY-SYSID INTO: INFILE, OUTFILE.
  CONDENSE INFILE  NO-GAPS.
  CONDENSE OUTFILE NO-GAPS.

*Open Input File
  OPEN DATASET INFILE FOR INPUT IN TEXT MODE MESSAGE MSG.
  IF ( SY-SUBRC <> 0 ).
    MESSAGE E002 WITH INFILE MSG.
  ENDIF.

*Open Output File
  OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE MESSAGE MSG.
  IF ( SY-SUBRC <> 0 ).
    MESSAGE E002 WITH OUTFILE MSG.
  ENDIF.

*Output file session header record type 0.
  PERFORM INIT_STRUCTURES USING 'BGR00'.
  MOVE '0'           TO Z_BGR00-STYPE.
  MOVE P_GROUP       TO Z_BGR00-GROUP.
  MOVE SY-MANDT      TO Z_BGR00-MANDT.    "Client
  MOVE 'BATCH'       TO Z_BGR00-USNAM.
  MOVE '////////'    TO Z_BGR00-START.    "Session date
  TRANSFER Z_BGR00 TO OUTFILE.

* Do until the end of file.
  DO.
    READ DATASET INFILE INTO INREC.

*   If file is empty, stop run.
    IF ( SY-INDEX = 1 ).
      IF ( SY-SUBRC = 4 ).
        MESSAGE I028.
        STOP.
      ENDIF.
    ENDIF.

*   When end of file, Exit.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.

*Output file document header records type 1.
    DATA: WRK_MONTH(3) TYPE C,
          WRK_MNR LIKE T247-MNR.

    PERFORM INIT_STRUCTURES USING 'BBKPF'.
    MOVE '1'        TO  Z_BBKPF-STYPE.
    MOVE:  P_TCODE  TO  Z_BBKPF-TCODE,
           P_BLART  TO  Z_BBKPF-BLART,
           P_BUKRS  TO  Z_BBKPF-BUKRS,
           P_WAERS  TO  Z_BBKPF-WAERS,
           P_BKTXT  TO  Z_BBKPF-BKTXT,
           SY-DATUM TO  Z_BBKPF-BLDAT,
           INREC-CHECK_DATE+3(3) TO WRK_MONTH.

    SELECT SINGLE MNR INTO WRK_MNR FROM T247 WHERE KTX = WRK_MONTH.
    CONCATENATE INREC-CHECK_DATE+7(4) WRK_MNR INREC-CHECK_DATE+0(2)
                INTO Z_BBKPF-BUDAT.
    Z_BBKPF-MONAT  =  INREC-CHECK_DATE+3(3).
    CONCATENATE INREC-CUST_CODE INREC-PREM_CODE INTO Z_BBKPF-XBLNR.
    TRANSFER Z_BBKPF TO OUTFILE.

*Output file document line item records type 2.
*Line item with key 31
    PERFORM INIT_STRUCTURES USING 'ZBSEG'.
    MOVE '2'               TO Z_ZBSEG-STYPE.
    MOVE 'ZBSEG'           TO Z_ZBSEG-TBNAM.
    MOVE '31'              TO Z_ZBSEG-NEWBS.
    MOVE 'N00'             TO Z_ZBSEG-ZTERM.
    MOVE 'E'               TO Z_ZBSEG-SPRAS.
    WRITE INREC-NET_AMOUNT TO Z_ZBSEG-WRBTR RIGHT-JUSTIFIED.
    Z_ZBSEG-NAME1 = INREC-CUST_NAME.
    Z_ZBSEG-ZFBDT = SY-DATUM.
    Z_ZBSEG-PSTLZ = INREC-ZIP_CODE.
    Z_ZBSEG-REGIO = INREC-STAT_CODE.
    Z_ZBSEG-HKONT = P_HKONTC.

    IF INREC-CITY_NAME <> SPACE.
      Z_ZBSEG-ORT01 = INREC-CITY_NAME.
    ELSE.
      Z_ZBSEG-ORT01 = '/'.
    ENDIF.

    IF INREC-NATION  = 'CANADA'.
      Z_ZBSEG-LAND1 = 'CA'.
    ELSEIF INREC-NATION = 'U.S.A.'.
      Z_ZBSEG-LAND1 = 'US'.
    ELSE.
      Z_ZBSEG-LAND1 = '/'.
    ENDIF.

*    IF INREC-ATTN_LINE <> SPACE.
*        Z_ZBSEG-NAME2 = INREC-ATTN_LINE.
*        Z_ZBSEG-NAME3 = INREC-ADDRESS01.
*    ELSE.
    Z_ZBSEG-NAME2 = INREC-ADDRESS01.
    Z_ZBSEG-NAME3 = '/'.
*    ENDIF.

    IF INREC-ADDRESS02 <> SPACE.
      CONCATENATE INREC-ADDRESS02 INREC-ADDRESS03 INTO
                      Z_ZBSEG-STRAS SEPARATED BY SPACE.
    ENDIF.

    TRANSFER Z_ZBSEG TO OUTFILE.

*Output file document line item records type 2.
*Line item with key 40
    PERFORM INIT_STRUCTURES USING 'ZBSEG'.
    MOVE '2'               TO Z_ZBSEG-STYPE.
    MOVE 'ZBSEG'           TO Z_ZBSEG-TBNAM.
    MOVE '40'              TO Z_ZBSEG-NEWBS.
    WRITE INREC-NET_AMOUNT TO Z_ZBSEG-WRBTR RIGHT-JUSTIFIED.
    WRITE P_HKONTD TO  Z_ZBSEG-HKONT LEFT-JUSTIFIED.
    TRANSFER Z_ZBSEG TO OUTFILE.

*Output file document line item records type 3.
CONCATENATE '3REBATE' SPACE SPACE SPACE INREC-STREET_NUM INREC-PDIR_CODE
   INREC-STREET_NAME INREC-SSFX_CODE INREC-PDIR_POST INREC-PREM_UTYP
   INREC-PREM_UNIT SPACE SPACE SPACE INREC-PREM_CITY INREC-PREM_STATE
   INREC-PREM_ZIPC INTO REC3 SEPARATED BY SPACE.

    TRANSFER REC3 TO OUTFILE.

  ENDDO.
  CLOSE DATASET: INFILE, OUTFILE.

  CLEAR MSG.
  MOVE '**** OUTPUT FILE CREATED *****' TO MSG.
  MESSAGE I019 WITH MSG.

*======================  INIT_STRUCTURES  =============================
*  Used to initialize the record to '/'
*======================================================================
FORM INIT_STRUCTURES USING TABLENAM.
  SELECT * FROM DD03L WHERE TABNAME = TABLENAM.
    CLEAR CHAR.
    CHAR(2) = 'Z_'.
    CHAR+2(5) = TABLENAM.
    CHAR+7(1) = '-'.
    CHAR+8    = DD03L-FIELDNAME.
    ASSIGN (CHAR) to <F1>.
    <F1> = NODATA.
  ENDSELECT.
ENDFORM.                    "INIT_STRUCTURES

********************* END OF PROGRAM ********************
