REPORT ZFFII025E MESSAGE-ID ZS.

************************************************************************
*  Project:     Mercator replacement                                   *
*  Interface:   Contrax Payment Interface (IFFI055)                    *
*  Author:      Mohammad T. Khan                                       *
*  Date:        October, 2005.                                         *
*  Description:                                                        *
*     - The purpose of this program is to map the data for Contrax     *
*       Payment Interface.    It will replace the mapping through      *
*       Mercator.                                                      *
*----------------------------------------------------------------------*
*  MODIFICATIONS:                                                      *
*----------------------------------------------------------------------*
*  2006/06/13 TRxxx Adel Mendoza                                       *
*  1. Added a routine to divide the input file into 2 parts:           *
*       a) records with exchange rate.                                 *
*       b) records without exchange rate.                              *
*  2. Added a routine to summarise the line items.                     *
*       a) this is only for the records without exchange rate.         *
*  3. Added a routine to create the following line items:              *
*       a) with Exchange Rate. ( from input file )                     *
*       b) with Applied Exchange Rate.                                 *
*       c) for Clearings.                                              *
*  4. Placed several codes within a FORM.                              *
*                                                                      *
*  2006/06/07 TR114 Mohammad Khan                                      *
*                      Program changes are made for:                   *
*                        1- Input file format changes.                 *
*                        2- ZFC07 table changes.                       *
*                        3- Apply exchange rate when exchange rate is  *
*                           there in the input file.                   *
*                                                                      *
*  2006/04/05 gymana 4.7upg Modified initialization of BGR00, BBKPF, & *
*                           ZBSEG table structures so no changes have  *
*                           be made to this ABAP when changes are made *
*                           to any of the tables.                      *
*                                                                      *
*  2008/10/28 GYMANA TR629 - Added new SOURCE_CODE field to input file.*
*                            and modified processing to account for new*
*                            source code field in table ZFC07.         *
************************************************************************
TABLES: ZFC07,     "Contrax Payment Account Lookup
        DD03L.     "Structures table

DATA: MSG(100) TYPE C.
DATA: INREC(69) TYPE C.

*-----------------------------------------------------------------------
FIELD-SYMBOLS: <F1>.
DATA:    CHAR(21)    TYPE C,
         NODATA(1)   VALUE '/'.
*-----------------------------------------------------------------------

*Input file record
*DATA: BEGIN OF CONTRAX,                               "TR114
*        PAYMENT_TYPE(8)    TYPE C,
*        FILLER1            TYPE C,
*        PAYMENT_AMT(14)    TYPE C,
*        FILLER2            TYPE C,
*        PAYMENT_DATE(8)    TYPE C,
*        FILLER3            TYPE C,
*      END OF CONTRAX.

*DATA: BEGIN OF EDITREC,                               "TR114
*        PAYMENT_TYPE(8)    TYPE C,
*        FILLER1            TYPE C,
*        PAYMENT_AMT(16)    TYPE C,
*        FILLER2            TYPE C,
*        CURRENCY_CODE(8)   TYPE C,
*        FILLER3            TYPE C,
*        PAYMENT_DATE(8)    TYPE C,
*        FILLER4(5)         TYPE C,
*        EXCHANGE_RATE(16)  TYPE C,
*      END OF EDITREC.

DATA: BEGIN OF EDITREC OCCURS 0,                            "TR114
        PAYMENT_TYPE(8)    TYPE C,
        FILLER1            TYPE C,
        PAYMENT_AMT(16)    TYPE C,
        FILLER2            TYPE C,
        CURRENCY_CODE(8)   TYPE C,
        FILLER3            TYPE C,
        PAYMENT_DATE(8)    TYPE C,
        FILLER4(5)         TYPE C,
        EXCHANGE_RATE(16)  TYPE C,
        SOURCE_CODE(5)     TYPE C,                          "TR629
      END OF EDITREC.

DATA: BEGIN OF LINE,                                        "TR114
        PAYMENT_TYPE(8)    TYPE C,
        NEWBS   LIKE  ZBSEG-NEWBS,     "Posting Key
        HKONT   LIKE  ZBSEG-HKONT,     "General ledger Account
        SGTXT   LIKE  ZBSEG-SGTXT,     "Item Text
        WRBTR   TYPE  P DECIMALS 2,    "Payment Amount
      END OF LINE.

*DATA: DATATAB LIKE SORTED TABLE OF LINE.              "TR114
*       WITH UNIQUE KEY PAYMENT_TYPE NEWBS HKONT SGTXT.

DATA: DATATAB LIKE TABLE OF LINE.                           "TR114

*- Added by ADM : 13-6-2006
DATA: BEGIN OF ISUM OCCURS 0,
        CURKY(8) TYPE  C,               "Currency Code
        NEWBS    LIKE  ZBSEG-NEWBS,     "Posting Key
        HKONT    LIKE  ZBSEG-HKONT,     "General ledger Account
        SGTXT    LIKE  ZBSEG-SGTXT,     "Item Text
        WRBTR    TYPE  P DECIMALS 2,    "Payment Amount
        BUDAT    LIKE  BBKPF-BUDAT.
DATA: END OF ISUM.

DATA: BEGIN OF ITAB OCCURS 0,
        INKEY(10) TYPE  C,
        CURKY(8)  TYPE  C,               "Currency Code
        NEWBS     LIKE  ZBSEG-NEWBS,     "Posting Key
        HKONT     LIKE  ZBSEG-HKONT,     "General ledger Account
        SGTXT     LIKE  ZBSEG-SGTXT,     "Item Text
        WRBTR     TYPE  P DECIMALS 2,    "Payment Amount
        BUDAT     LIKE  BBKPF-BUDAT,     "Posting Date
        EXRAT(16) TYPE C.                "Exchange Rate
DATA: END OF ITAB.
*- ADM END HERE

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

DATA: BEGIN OF CONTRAX OCCURS 0,
        CURRENCY_CODE      LIKE BBKPF-WAERS,
        PAYMENT_TYPE       LIKE ZFC07-PYMTTYP,
        PAYMENT_AMT        LIKE Z_ZBSEG-WRBTR,
        PAYMENT_DATE       LIKE Z_BBKPF-BUDAT,
        EXCHANGE_RATE(16)  TYPE C,
        SOURCE_CODE(5)     TYPE C,
      END OF CONTRAX.

*- Added by ADM : 13-6-2006
DATA: BEGIN OF CONTRAX2 OCCURS 0.
DATA:   INKEY(10) TYPE C.
INCLUDE   STRUCTURE CONTRAX.
DATA:   NEWBS     LIKE  ZBSEG-NEWBS,     "Posting Key
        HKONT     LIKE  ZBSEG-HKONT,     "General ledger Account
        SGTXT     LIKE  ZBSEG-SGTXT,     "Item Text
        WRBTR     TYPE  P DECIMALS 2.    "Payment Amount
DATA: END OF CONTRAX2.
*- ADM END HERE

DATA: WRK_SYMBOLIC(4) TYPE C VALUE '$sys',
      W_NEWBS         LIKE ZBSEG-NEWBS,
      W_HKONT         LIKE ZBSEG-HKONT,
      W_WRBTR(16)     TYPE C,
      FIRST_TIME      TYPE C VALUE 'X',
      PREV_CURR_CODE  LIKE BBKPF-WAERS.

DATA: WA_HKONT1  LIKE ZBSEG-HKONT,     "General ledger Account
      WA_HKONT2  LIKE ZBSEG-HKONT,     "General ledger Account
      WA_NEWBS1  LIKE ZBSEG-NEWBS,     "Posting Key
      WA_NEWBS2  LIKE ZBSEG-NEWBS,     "Posting Key
      WA_SGTXT1  LIKE ZBSEG-SGTXT.     "Item Text

DATA: SWITCH     TYPE I VALUE 0.

CONSTANTS:  H_DMTR     TYPE X VALUE '09',
            C_OACT(10) TYPE C VALUE '0000140145'.

*----------------------------------------------------------------------
* SELECTION SCREEN.
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP 1.

PARAMETER:  INFILE LIKE FILENAME-FILEEXTERN
              DEFAULT '/usr/sap/interfaces/$sys/IFFI055/zbis255.chk'.

*PARAMETER:  INFILE LIKE RLGRAP-FILENAME
*              DEFAULT 'C:\MERCATOR\STPayment1.txt'.

SELECTION-SCREEN SKIP 1.
PARAMETER: OUTFILE LIKE FILENAME-FILEEXTERN
              DEFAULT '/usr/sap/interfaces/$sys/IFFI055/zbis255.sap'.

SELECTION-SCREEN SKIP 1.
PARAMETER:
 P_GROUP  LIKE  BGR00-GROUP DEFAULT 'ZFI-CTRX-PYT',
 P_TCODE(4)                 DEFAULT 'FB01',
 P_BLART  LIKE  BBKPF-BLART DEFAULT 'S6',
 P_BUKRS  LIKE  BBKPF-BUKRS DEFAULT 'UGL',
* P_WAERS  LIKE  BBKPF-WAERS DEFAULT 'CAD',                "TR114
 P_XBLNR  LIKE  BBKPF-XBLNR DEFAULT 'CONTRAX',
 P_BKTXT  LIKE  BBKPF-BKTXT DEFAULT 'Contrax Payments'.
SELECTION-SCREEN END OF BLOCK BOX1.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  REPLACE WRK_SYMBOLIC WITH SY-SYSID INTO: INFILE, OUTFILE.
  CONDENSE: INFILE NO-GAPS, OUTFILE NO-GAPS.

*----------------------------------------------------------------------
* START OF SELECTION
*----------------------------------------------------------------------
START-OF-SELECTION.
  REFRESH: CONTRAX, CONTRAX2.

**--- for testing only
*  call FUNCTION 'WS_UPLOAD'
*    exporting
*      filename  = infile
*      filetype  = 'ASC'
*    tables
*      data_tab  = editrec.
*
*  delete editrec where PAYMENT_TYPE is initial.
**--- end here

  PERFORM OPEN_DATASETS.
  PERFORM TRANSFER_INPUT_FILE.

* Output file session header record type 0.
  PERFORM INIT_STRUCTURES USING 'BGR00'.
  MOVE '0'           TO Z_BGR00-STYPE.
  MOVE P_GROUP       TO Z_BGR00-GROUP.
  MOVE SY-MANDT      TO Z_BGR00-MANDT.    "Client
  MOVE 'BATCH'       TO Z_BGR00-USNAM.
  MOVE '////////'    TO Z_BGR00-START.    "Session date
  TRANSFER Z_BGR00 TO OUTFILE.

  PERFORM DIVIDE_FILES.
  PERFORM PROCESS_CTRX_WO_EXRATE.
  PERFORM PROCESS_CTRX_WT_EXRATE.

*- CREATE OUTPUT FILE
*  -------------------------------
*- Added by ADM : 13-6-2006
*- (for ITEMS w/o exchange rate)
  SORT ISUM BY CURKY.
  LOOP AT ISUM.
    AT NEW CURKY.
      SWITCH = 1.
    ENDAT.
    IF SWITCH = 1.
      SWITCH = 0.
      PERFORM BUILD_DOCUMENT_HEADER_RECORD USING 'ISUM'.
    ENDIF.

    PERFORM INIT_STRUCTURES USING 'ZBSEG'.
    MOVE '2'          TO Z_ZBSEG-STYPE.
    MOVE 'ZBSEG'      TO Z_ZBSEG-TBNAM.
    MOVE  ISUM-WRBTR  TO  W_WRBTR.
    WRITE W_WRBTR     TO  Z_ZBSEG-WRBTR RIGHT-JUSTIFIED.
    MOVE  ISUM-NEWBS  TO  Z_ZBSEG-NEWBS.
    MOVE  ISUM-SGTXT  TO  Z_ZBSEG-SGTXT.
    MOVE  ISUM-HKONT  TO  Z_ZBSEG-HKONT.
    TRANSFER Z_ZBSEG TO OUTFILE.
  ENDLOOP.

*- (for ITEMS with exchange rate)
  MOVE 0 TO SWITCH.
  SORT ITAB BY INKEY.
  LOOP AT ITAB.
    AT NEW INKEY.
      SWITCH = 1.
    ENDAT.
    IF SWITCH = 1.
      SWITCH = 0.
      PERFORM BUILD_DOCUMENT_HEADER_RECORD USING 'ITAB'.
    ENDIF.

    PERFORM INIT_STRUCTURES USING 'ZBSEG'.
    MOVE '2'          TO Z_ZBSEG-STYPE.
    MOVE 'ZBSEG'      TO Z_ZBSEG-TBNAM.
    MOVE  ITAB-WRBTR  TO  W_WRBTR.
    WRITE W_WRBTR     TO  Z_ZBSEG-WRBTR RIGHT-JUSTIFIED.
    MOVE  ITAB-NEWBS  TO  Z_ZBSEG-NEWBS.
    MOVE  ITAB-SGTXT  TO  Z_ZBSEG-SGTXT.
    MOVE  ITAB-HKONT  TO  Z_ZBSEG-HKONT.
    TRANSFER Z_ZBSEG TO OUTFILE.
  ENDLOOP.
*- ADM END HERE

  CLOSE DATASET: INFILE, OUTFILE.

  CLEAR MSG.
  MOVE '**** OUTPUT FILE CREATED *****' TO MSG.
  MESSAGE I019 WITH MSG.

************************************************************************
*             BUILD_DOCUMENT_HEADER_RECORD                             *
************************************************************************
FORM BUILD_DOCUMENT_HEADER_RECORD USING TABNAM.

  PERFORM INIT_STRUCTURES USING 'BBKPF'.
  MOVE '1'        TO  Z_BBKPF-STYPE.
  MOVE:  P_TCODE  TO  Z_BBKPF-TCODE,
         P_BLART  TO  Z_BBKPF-BLART,
         P_BUKRS  TO  Z_BBKPF-BUKRS,
         P_XBLNR  TO  Z_BBKPF-XBLNR,
         P_BKTXT  TO  Z_BBKPF-BKTXT,
         SY-DATUM TO  Z_BBKPF-BLDAT.

*- commented out by ADM : 13-6-2006
*  CONTRAX-PAYMENT_DATE  TO Z_BBKPF-BUDAT,
*  CONTRAX-CURRENCY_CODE TO Z_BBKPF-WAERS.
*- ADM END HERE

*- Added by ADM : 13-6-2006
  IF TABNAM = 'ISUM'.
    MOVE: ISUM-BUDAT TO Z_BBKPF-BUDAT,
          ISUM-CURKY TO Z_BBKPF-WAERS.
  ELSE.
    IF ITAB-CURKY = 'USD'.
      SHIFT ITAB-EXRAT LEFT DELETING LEADING SPACE.
      MOVE ITAB-EXRAT TO Z_BBKPF-KURSF.
      CONCATENATE '@' ITAB-EXRAT INTO Z_BBKPF-BKTXT.
      CONCATENATE P_BKTXT Z_BBKPF-BKTXT INTO Z_BBKPF-BKTXT
        SEPARATED BY SPACE.
    ENDIF.
    MOVE: ITAB-BUDAT TO Z_BBKPF-BUDAT,
          ITAB-CURKY TO Z_BBKPF-WAERS.
  ENDIF.
*- ADM END HERE

  TRANSFER Z_BBKPF TO OUTFILE.

ENDFORM.                    "BUILD_DOCUMENT_HEADER_RECORD

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
    ASSIGN (CHAR) TO <F1>.
    <F1> = NODATA.
  ENDSELECT.
ENDFORM.                    "INIT_STRUCTURES

*======================================================================
* GET ACCOUNT
*======================================================================
FORM GET_ACCOUNT USING ISOURCE_CODE
                       IPAYMENT_TYPE
                       ICURR
                       IEXRATE

              CHANGING EHKONT1
                       EHKONT2
                       ENEWBS1
                       ENEWBS2
                       ESGTEXT.

  CLEAR ZFC07.
  SELECT SINGLE * FROM ZFC07
   WHERE SRCCODE = ISOURCE_CODE AND
         PYMTTYP = IPAYMENT_TYPE.

  IF SY-SUBRC = 0.
    IF ICURR = 'CAD'.
      MOVE ZFC07-OACCT     TO EHKONT1.
      MOVE '40' TO ENEWBS1.
      MOVE ZFC07-USD_SAKNR TO EHKONT2.
      MOVE '50' TO ENEWBS2.
    ELSE.
      MOVE ZFC07-USD_OACCT TO EHKONT1.
      MOVE '40' TO ENEWBS1.
      MOVE ZFC07-SAKNR     TO EHKONT2.
      MOVE '50' TO ENEWBS2.
    ENDIF.
    CLEAR: ESGTEXT.
  ELSE.
    SELECT SINGLE * FROM ZFC07
     WHERE PYMTTYP = 'ERROR'.
    IF ICURR = 'CAD'.
      MOVE ZFC07-OACCT     TO EHKONT1.
      MOVE '40' TO ENEWBS1.
      MOVE ZFC07-USD_SAKNR TO EHKONT2.
      MOVE '50' TO ENEWBS2.
    ELSE.
      MOVE ZFC07-USD_OACCT TO EHKONT1.
      MOVE '40' TO ENEWBS1.
      MOVE ZFC07-SAKNR     TO EHKONT2.
      MOVE '50' TO ENEWBS2.
    ENDIF.
    CLEAR: ESGTEXT, INREC.
    MOVE IPAYMENT_TYPE TO INREC.
    SHIFT INREC LEFT DELETING LEADING SPACE.
    CONCATENATE TEXT-102 INREC INTO ESGTEXT
                SEPARATED BY SPACE.
  ENDIF.

ENDFORM.                    "GET_ACCOUNT

*======================================================================
* PROCESS CONTRAX WITH EXCHANGE RATE
*======================================================================
FORM PROCESS_CTRX_WT_EXRATE.
  REFRESH: ITAB.
  SORT CONTRAX2 BY INKEY.
  LOOP AT CONTRAX2.
    MOVE CONTRAX2-INKEY         TO ITAB-INKEY.
    MOVE CONTRAX2-CURRENCY_CODE TO ITAB-CURKY.
    MOVE CONTRAX2-NEWBS         TO ITAB-NEWBS.
    MOVE CONTRAX2-HKONT         TO ITAB-HKONT.
    MOVE CONTRAX2-SGTXT         TO ITAB-SGTXT.
    MOVE CONTRAX2-WRBTR         TO ITAB-WRBTR.
    MOVE CONTRAX2-PAYMENT_DATE  TO ITAB-BUDAT.
    MOVE CONTRAX2-EXCHANGE_RATE TO ITAB-EXRAT.
    APPEND ITAB. CLEAR ITAB.
  ENDLOOP.
ENDFORM.                    "PROCESS_CTRX_WT_EXRATE

*======================================================================
* PROCESS CONTRAX WITHOUT EXCHANGE RATE
*======================================================================
FORM PROCESS_CTRX_WO_EXRATE.
* &&&LOOP AT CONTRAX.
  REFRESH: ISUM.
  SORT CONTRAX BY CURRENCY_CODE.
  LOOP AT CONTRAX.
*-  --------------------------------
*-  commented out by ADM : 13-6-2006
*-  --------------------------------
*    CLEAR DATATAB.
*    REFRESH DATATAB.
*-  ADM END HERE

    LINE-WRBTR = CONTRAX-PAYMENT_AMT.

*   A separate header record for each currency code
    IF CONTRAX-CURRENCY_CODE <> PREV_CURR_CODE.
      MOVE CONTRAX-CURRENCY_CODE TO PREV_CURR_CODE.

*-       commented out by ADM  13-6-2006
*        PERFORM BUILD_DOCUMENT_HEADER_RECORD.
*-       end here
    ENDIF.

    CLEAR W_NEWBS.
    IF LINE-WRBTR < 0.
      MOVE  '40'     TO  LINE-NEWBS.                "Posting Key
      MOVE  '50'     TO  W_NEWBS.
      LINE-WRBTR  =  LINE-WRBTR * -1.               "Payment Amount
    ELSE.
      MOVE  '50'     TO  LINE-NEWBS.
      MOVE  '40'     TO  W_NEWBS.
    ENDIF.
*
    SELECT SINGLE SAKNR OACCT USD_SAKNR USD_OACCT
      INTO (ZFC07-SAKNR, ZFC07-OACCT, ZFC07-USD_SAKNR, ZFC07-USD_OACCT)
           FROM ZFC07
           WHERE SRCCODE = CONTRAX-SOURCE_CODE AND
                 PYMTTYP = CONTRAX-PAYMENT_TYPE.
    IF SY-SUBRC = 0.
      IF CONTRAX-CURRENCY_CODE = 'CAD'.
        MOVE ZFC07-SAKNR TO LINE-HKONT.
        MOVE ZFC07-OACCT TO W_HKONT.
      ELSE.
        MOVE ZFC07-USD_SAKNR TO LINE-HKONT.
        MOVE ZFC07-USD_OACCT TO W_HKONT.
      ENDIF.
      CLEAR LINE-SGTXT.
      IF CONTRAX-EXCHANGE_RATE <> 0.
        CONCATENATE TEXT-103 CONTRAX-EXCHANGE_RATE INTO LINE-SGTXT
                             SEPARATED BY SPACE.
      ELSE.
        MOVE TEXT-101    TO LINE-SGTXT.
      ENDIF.
    ELSE.
      SELECT SINGLE SAKNR OACCT USD_SAKNR USD_OACCT
        INTO (ZFC07-SAKNR, ZFC07-OACCT, ZFC07-USD_SAKNR, ZFC07-USD_OACCT
        )
          FROM ZFC07 WHERE PYMTTYP = 'ERROR'.
      IF CONTRAX-CURRENCY_CODE = 'CAD'.
        MOVE ZFC07-SAKNR TO LINE-HKONT.
        MOVE ZFC07-OACCT TO W_HKONT.
      ELSE.
        MOVE ZFC07-USD_SAKNR TO LINE-HKONT.
        MOVE ZFC07-USD_OACCT TO W_HKONT.
      ENDIF.
      CONCATENATE TEXT-102 CONTRAX-PAYMENT_TYPE INTO
                  LINE-SGTXT SEPARATED BY SPACE.
    ENDIF.
    CLEAR LINE-PAYMENT_TYPE.
*    COLLECT LINE INTO DATATAB.                        "TR114

*-   commented out by ADM  13-6-2006
*    APPEND LINE TO DATATAB.
*-   ADM END HERE

*-  -------------------------------------------------------------------
*-  Added by ADM : 13-6-2006               (Line Items From Input File)
*-  (create & sum line items based on Currency, Posting Key & Accounts)
    READ TABLE ISUM WITH KEY CURKY = CONTRAX-CURRENCY_CODE
                             NEWBS = LINE-NEWBS
                             HKONT = LINE-HKONT.
    IF SY-SUBRC = 0.
      ADD LINE-WRBTR TO ISUM-WRBTR.
      MODIFY ISUM INDEX SY-TABIX TRANSPORTING WRBTR.
    ELSE.
      MOVE-CORRESPONDING LINE TO ISUM.
      MOVE CONTRAX-CURRENCY_CODE TO ISUM-CURKY.
      MOVE CONTRAX-PAYMENT_DATE  TO ISUM-BUDAT.
      APPEND ISUM. CLEAR ISUM.
    ENDIF.
*-  ADM END HERE
*-  -------------------------------------------------------------------

*   Output file document line item records 2 (Offest Account).
    MOVE W_NEWBS   TO  LINE-NEWBS.
    MOVE W_HKONT   TO  LINE-HKONT.
    MOVE CONTRAX-PAYMENT_TYPE TO  LINE-PAYMENT_TYPE.
*    COLLECT LINE INTO DATATAB.                        "TR114

*-   commented out by ADM  13-6-2006
*    APPEND LINE TO DATATAB.
*-   end here

*-  -------------------------------------------------------------------
*-  Added by ADM : 13-6-2006                 (Offset Line Items)
*-  (create & sum line items based on Currency, Posting Key & Accounts)
    READ TABLE ISUM WITH KEY CURKY = CONTRAX-CURRENCY_CODE
                             NEWBS = LINE-NEWBS
                             HKONT = LINE-HKONT.
    IF SY-SUBRC = 0.
      ADD LINE-WRBTR TO ISUM-WRBTR.
      MODIFY ISUM INDEX SY-TABIX TRANSPORTING WRBTR.
    ELSE.
      MOVE-CORRESPONDING LINE TO ISUM.
      MOVE CONTRAX-CURRENCY_CODE TO ISUM-CURKY.
      MOVE CONTRAX-PAYMENT_DATE  TO ISUM-BUDAT.
      APPEND ISUM. CLEAR ISUM.
    ENDIF.
*-  ADM END HERE
*-  -------------------------------------------------------------------

    CLEAR   LINE.
*-  commented out by ADM : 13-6-2006
*-  --------------------------------
*    LOOP AT DATATAB INTO LINE.
*      PERFORM INIT_STRUCTURES USING 'ZBSEG'.
*      MOVE '2'          TO Z_ZBSEG-STYPE.
*      MOVE 'ZBSEG'      TO Z_ZBSEG-TBNAM.
*      MOVE  LINE-WRBTR  TO  W_WRBTR.
*      WRITE W_WRBTR     TO  Z_ZBSEG-WRBTR RIGHT-JUSTIFIED.
*      MOVE  LINE-NEWBS  TO  Z_ZBSEG-NEWBS.
*      MOVE  LINE-SGTXT  TO  Z_ZBSEG-SGTXT.
*      MOVE  LINE-HKONT  TO  Z_ZBSEG-HKONT.
*      TRANSFER Z_ZBSEG TO OUTFILE.
*    ENDLOOP.
*-  --------------------------------
*-  ADM END HERE

  ENDLOOP.
ENDFORM.                    "PROCESS_CTRX_WO_EXRATE

*======================================================================
* DIVIDE FILES
*======================================================================
FORM DIVIDE_FILES.
  DATA: SY_TABIX(10) TYPE C.
  DATA: WA_SGTXT     LIKE ZBSEG-SGTXT.
  DATA: WA_EXRAT(8)  TYPE P DECIMALS 5.
  DATA: WA_PYMNT     TYPE P DECIMALS 2.

  LOOP AT EDITREC.
*-  Output file document line item records 1.
    MOVE-CORRESPONDING EDITREC TO CONTRAX.                  "TR114

    IF EDITREC-EXCHANGE_RATE <> 0.
*-    ----------------------------------------------------
*-    LINE ITEMS WITH EXCHANGE RATE
*-    ----------------------------------------------------
*-    Added by ADM : 13-6-2006
*-    (create line item - original amount from input file)
*-    ----------------------------------------------------
      MOVE SY-TABIX TO SY_TABIX.

      MOVE-CORRESPONDING CONTRAX TO CONTRAX2.
      CLEAR CONTRAX2-INKEY.
      MOVE SY_TABIX TO CONTRAX2-INKEY.
      CONCATENATE CONTRAX2-INKEY 'A' INTO CONTRAX2-INKEY.

      IF EDITREC-CURRENCY_CODE = '$CDN'.
        CONTRAX2-CURRENCY_CODE = 'CAD'.
        CLEAR: WA_SGTXT.
        MOVE TEXT-101 TO WA_SGTXT.
      ELSE.
        CONTRAX2-CURRENCY_CODE = 'USD'.
        CLEAR: WA_SGTXT.
        MOVE EDITREC-EXCHANGE_RATE TO WA_SGTXT.
        SHIFT WA_SGTXT LEFT DELETING LEADING SPACE.
        CONCATENATE TEXT-103 WA_SGTXT INTO WA_SGTXT
                    SEPARATED BY SPACE.
        MOVE EDITREC-EXCHANGE_RATE TO WA_EXRAT.
        MOVE WA_EXRAT TO CONTRAX2-EXCHANGE_RATE.
      ENDIF.

      CLEAR: WA_HKONT1, WA_HKONT2, WA_NEWBS1, WA_NEWBS2, WA_SGTXT1.
      PERFORM GET_ACCOUNT USING CONTRAX-SOURCE_CODE
                                CONTRAX-PAYMENT_TYPE
                                CONTRAX2-CURRENCY_CODE
                                EDITREC-EXCHANGE_RATE

                       CHANGING WA_HKONT1
                                WA_HKONT2
                                WA_NEWBS1
                                WA_NEWBS2
                                WA_SGTXT1.

      MOVE WA_HKONT1 TO CONTRAX2-HKONT.
      MOVE WA_NEWBS1 TO CONTRAX2-NEWBS.

      IF WA_SGTXT1 IS INITIAL.
        MOVE WA_SGTXT TO CONTRAX2-SGTXT.
      ELSE.
        MOVE WA_SGTXT1 TO CONTRAX2-SGTXT.
      ENDIF.

      IF EDITREC-PAYMENT_AMT < 0.
        CONTRAX2-WRBTR  =  EDITREC-PAYMENT_AMT * -1.  "Payment Amount
      ELSE.
        CONTRAX2-WRBTR  =  CONTRAX2-PAYMENT_AMT.      "Payment Amount
      ENDIF.
      APPEND CONTRAX2.

*-    ----------------------------------------------------
*-    (create clearing line item for original amount)
*-    ----------------------------------------------------
      MOVE C_OACT TO CONTRAX2-HKONT.
      IF CONTRAX2-NEWBS = '50'.
        CONTRAX2-NEWBS = '40'.
      ELSE.
        CONTRAX2-NEWBS = '50'.
      ENDIF.
      APPEND CONTRAX2.
*-    ADM END HERE ----
*-    ----------------------------------------------------

      CONTRAX-PAYMENT_AMT = EDITREC-PAYMENT_AMT * EDITREC-EXCHANGE_RATE.

      IF EDITREC-CURRENCY_CODE = '$CDN'.
        CONTRAX-CURRENCY_CODE = 'USD'.
*-      --------------------------------------------
*-      Added by ADM 13-6-2006
*-      (get the inverted rate of the exchange rate)
        CLEAR WA_EXRAT.
        WA_EXRAT = ( 1 / EDITREC-EXCHANGE_RATE ).
        MOVE WA_EXRAT TO CONTRAX2-EXCHANGE_RATE.

**-      apply the exchange rate
*        CLEAR WA_PYMNT.
*        WA_PYMNT = EDITREC-PAYMENT_AMT / WA_EXRAT.
*        MOVE WA_PYMNT TO CONTRAX-PAYMENT_AMT.

*-      line item text
        CLEAR: WA_SGTXT.
        MOVE WA_EXRAT TO WA_SGTXT.
        SHIFT WA_SGTXT LEFT DELETING LEADING SPACE.
        CONCATENATE TEXT-103 WA_SGTXT INTO WA_SGTXT
                    SEPARATED BY SPACE.
*-      ADM END HERE
*-      --------------------------------------------
      ELSE.
        CONTRAX-CURRENCY_CODE = 'CAD'.
*-      --------------------------------------------
*-      Added by ADM 13-6-2006
        CLEAR: WA_SGTXT.
        MOVE TEXT-101 TO WA_SGTXT.
*-      ADM END HERE
*-      --------------------------------------------
      ENDIF.

*-    ----------------------------------------------------
*-    Added by ADM : 13-6-2006             - EXCHANGE RATE
*-    (create line item - converted amount from orig amt.
*-    ----------------------------------------------------
      CLEAR CONTRAX2-INKEY.
      MOVE SY_TABIX TO CONTRAX2-INKEY.
      CONCATENATE CONTRAX2-INKEY 'B' INTO CONTRAX2-INKEY.

      MOVE CONTRAX-PAYMENT_AMT   TO CONTRAX2-PAYMENT_AMT.
      MOVE CONTRAX-CURRENCY_CODE TO CONTRAX2-CURRENCY_CODE.

      MOVE WA_HKONT2             TO CONTRAX2-HKONT.
      MOVE WA_NEWBS2             TO CONTRAX2-NEWBS.

      IF WA_SGTXT1 IS INITIAL.
        MOVE WA_SGTXT TO CONTRAX2-SGTXT.
      ELSE.
        MOVE WA_SGTXT1 TO CONTRAX2-SGTXT.
      ENDIF.

      IF CONTRAX2-PAYMENT_AMT < 0.
        CONTRAX2-WRBTR  =  CONTRAX2-PAYMENT_AMT * -1.  "Payment Amount
      ELSE.
        CONTRAX2-WRBTR  =  CONTRAX2-PAYMENT_AMT.       "Payment Amount
      ENDIF.
      APPEND CONTRAX2.

*-    ----------------------------------------------------
*-    (create clearing line item for converted amount)
*-    ----------------------------------------------------
      MOVE C_OACT TO CONTRAX2-HKONT.
      IF CONTRAX2-NEWBS = '50'.
        CONTRAX2-NEWBS = '40'.
      ELSE.
        CONTRAX2-NEWBS = '50'.
      ENDIF.
      APPEND CONTRAX2.  CLEAR CONTRAX2.
*-    ADM END HERE ----
*-    ----------------------------------------------------

    ELSE.

*-    ----------------------------------------------------
*-    LINE ITEMS WITHOUT EXCHANGE RATE
*-    ----------------------------------------------------
      IF EDITREC-CURRENCY_CODE = '$CDN'.
        MOVE TEXT-104 TO CONTRAX-CURRENCY_CODE.
      ELSE.
        MOVE TEXT-105 TO CONTRAX-CURRENCY_CODE.
      ENDIF.
*-    ----------------------------------------------------
*-    Added by ADM : 13-6-2006
      APPEND CONTRAX.
      CLEAR  CONTRAX.
*-    ADM END HERE
*-    ----------------------------------------------------

    ENDIF.

*-  commented out by ADM 13-6-2006
*   APPEND CONTRAX.
*   CLEAR  CONTRAX.
*-  end here

  ENDLOOP.
ENDFORM.                    "DIVIDE_FILES

*======================================================================
* TRANSFER INPUT FILE
*======================================================================
FORM TRANSFER_INPUT_FILE.
  REFRESH EDITREC.
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
    MOVE INREC to EDITREC.
    APPEND EDITREC. CLEAR EDITREC.
  ENDDO.
ENDFORM.                    "TRANSFER_INPUT_FILE

*======================================================================
* OPEN FILES
*======================================================================
FORM OPEN_DATASETS.
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
ENDFORM.                    "OPEN_DATASETS

****************************** END OF PROGRAM *************************
