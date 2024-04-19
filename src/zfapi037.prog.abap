REPORT ZFAPI037 MESSAGE-ID ZS.

************************************************************************
*  Project:     Mercator replacement                                   *
*  Interface:   Liberty Remittance (IFAP023)                           *
*  Author:      Mohammad T. Khan                                       *
*  Date:        September, 2005                                        *
*  Description:                                                        *
*     - The purpose of this program is to map the data for Liberty     *
*       Remittance interface. It will replace the mapping through      *
*       Mercator.                                                      *
*                                                                      *
*  Modifications:                                                      *
*  2010/08/17 gymana TR804  Change default account number on line item *
*                           '31' records to use party ID. Update       *
*                           posting key from '40' to '02'              *
*  2006/04/05 gymana 4.7upg Modified initialization of BGR00, BBKPF, & *
*                           ZBSEG table structures so no changes have  *
*                           be made to this ABAP when changes are made *
*                           to any of the tables.                      *
************************************************************************
TABLES:  DD03L.

*-----------------------------------------------------------------------
FIELD-SYMBOLS: <F1>.
DATA:    CHAR(21)    TYPE c,
         NODATA(1)   VALUE '/'.
*-----------------------------------------------------------------------

DATA: MSG(100) TYPE C,
      INREC(140) TYPE C.

*Input file record
DATA: BEGIN OF INDATA,
        CONT_SUB(04)    TYPE C,
        CONT_ID(08)     TYPE C,
        VEND_ID(12)     TYPE C,
        INV_NUM(08)     TYPE C,
        AMOUNT(17)      TYPE C,
        PAY_MTHD(08)    TYPE C,
        TRAN_DTE(11)    TYPE C,
        PTY_ID(08)      TYPE C,
      END OF INDATA.

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
      REC3(02)        TYPE C,
      WRK_REC1         TYPE STRING,
      WRK_REC2         TYPE STRING.


CONSTANTS:  H_DMTR  TYPE X VALUE '09'.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP 1.
PARAMETER:  INFILE LIKE FILENAME-FILEEXTERN
              DEFAULT '/usr/sap/interfaces/$sys/IFAP023/libremitt.chk'.
SELECTION-SCREEN SKIP 1.
PARAMETER: OUTFILE LIKE FILENAME-FILEEXTERN
              DEFAULT '/usr/sap/interfaces/$sys/IFAP023/lib_remitt.sap'.
SELECTION-SCREEN SKIP 1.
PARAMETER:
 P_GROUP  LIKE  BGR00-GROUP DEFAULT 'ZAP-LIBERTY',
 P_TCODE(4)                 DEFAULT 'FB01',
 P_BLART  LIKE  BBKPF-BLART DEFAULT 'K3',
 P_BUKRS  LIKE  BBKPF-BUKRS DEFAULT 'UGL',
 P_WAERS  LIKE  BBKPF-WAERS DEFAULT 'CAD',
 P_XBLNR  LIKE  BBKPF-XBLNR DEFAULT 'LIBERTY REMITT',
* P_HKONTD LIKE  ZBSEG-HKONT DEFAULT '140140'. "GL Acct-Debit Entry 40
 P_HKONTD LIKE  ZBSEG-HKONT.                                   "TR804
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
    SPLIT INREC AT H_DMTR INTO INDATA-CONT_SUB INDATA-CONT_ID
                               INDATA-VEND_ID  INDATA-INV_NUM
                               INDATA-AMOUNT   INDATA-PAY_MTHD
                               INDATA-TRAN_DTE INDATA-PTY_ID.

    PERFORM INIT_STRUCTURES USING 'BBKPF'.
    MOVE '1'        TO  Z_BBKPF-STYPE.
    MOVE:  P_TCODE  TO  Z_BBKPF-TCODE,
           P_BLART  TO  Z_BBKPF-BLART,
           P_BUKRS  TO  Z_BBKPF-BUKRS,
           P_WAERS  TO  Z_BBKPF-WAERS,
           P_XBLNR  TO  Z_BBKPF-XBLNR,
           SY-DATUM TO  Z_BBKPF-BLDAT.


    CONCATENATE INDATA-CONT_SUB INDATA-CONT_ID INTO WRK_REC1.
    CONDENSE WRK_REC1 NO-GAPS.
    CONCATENATE TEXT-100 WRK_REC1 INTO Z_BBKPF-BKTXT SEPARATED BY SPACE.
    TRANSFER Z_BBKPF TO OUTFILE.

*Output file document line item records type 2.
*Line item with key 31
    PERFORM INIT_STRUCTURES USING 'ZBSEG'.
    MOVE '2'               TO Z_ZBSEG-STYPE.
    MOVE 'ZBSEG'           TO Z_ZBSEG-TBNAM.
    MOVE '31'              TO Z_ZBSEG-NEWBS.
    MOVE 'N00'             TO Z_ZBSEG-ZTERM.
    MOVE 'E'               TO Z_ZBSEG-SPRAS.
    WRITE INDATA-AMOUNT TO Z_ZBSEG-WRBTR RIGHT-JUSTIFIED.
    Z_ZBSEG-ZFBDT = SY-DATUM.
    MOVE INDATA-VEND_ID TO WRK_REC1.
    CONDENSE WRK_REC1 NO-GAPS.
    CONCATENATE INDATA-CONT_SUB+0(2) INDATA-CONT_ID INTO WRK_REC2.
    CONDENSE WRK_REC2 NO-GAPS.
    CONCATENATE TEXT-101 WRK_REC1 TEXT-102 WRK_REC2
                INTO Z_ZBSEG-SGTXT SEPARATED BY SPACE.
    Z_ZBSEG-HKONT = WRK_REC1.

    TRANSFER Z_ZBSEG TO OUTFILE.

*Output file document line item records type 2.
*Line item with key 40
    PERFORM INIT_STRUCTURES USING 'ZBSEG'.
    MOVE '2'               TO Z_ZBSEG-STYPE.
    MOVE 'ZBSEG'           TO Z_ZBSEG-TBNAM.
*    MOVE '40'              TO Z_ZBSEG-NEWBS.
    MOVE '02'              TO Z_ZBSEG-NEWBS.                   "TR804
    WRITE INDATA-AMOUNT TO Z_ZBSEG-WRBTR RIGHT-JUSTIFIED.
*    WRITE P_HKONTD TO  Z_ZBSEG-HKONT LEFT-JUSTIFIED.          "TR804
    MOVE INDATA-VEND_ID TO WRK_REC1.
    CONDENSE WRK_REC1 NO-GAPS.
    CONCATENATE INDATA-CONT_SUB+0(2) INDATA-CONT_ID INTO WRK_REC2.
    CONDENSE WRK_REC2 NO-GAPS.
    CONCATENATE TEXT-101 WRK_REC1 TEXT-102 WRK_REC2
                INTO Z_ZBSEG-SGTXT SEPARATED BY SPACE.
    CONCATENATE 'CX' INDATA-PTY_ID INTO WRK_REC1.             "TR804
    CONDENSE WRK_REC1 NO-GAPS.                                 "TR804
    Z_ZBSEG-HKONT = WRK_REC1.                                  "TR804
    TRANSFER Z_ZBSEG TO OUTFILE.

*Output file document line item records type 3.
    MOVE '3/' TO REC3.
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
