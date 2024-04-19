REPORT ZFFII018C NO STANDARD PAGE HEADING LINE-COUNT 65
                                         LINE-SIZE 120 MESSAGE-ID ZS.
************************************************************************
*  Client:     Duke Energy.                                            *
*  Date:       July 2003.                                              *
*  Author:     Mohammad Khan                                           *
*  Program Description:                                                *
*  This program will download G/L Inter Company Balance into a file for*
*  upload into PeopleSoft system for consolidation purpose.            *
*  File Header and Journal headers will be part of each detail record. *
*  A Journal Headere will be created for each company code.            *
*                                                                      *
*  - If SAP company code doesn't exist in ZJHDR table, all accounts    *
*    under this company code will not be processed. Similarly, only    *
*    those accounts will be processed that exist in the inter company  *
*    table ZINTC.                                                      *
*                                                                      *
*  - Hard coded delimeter Hex '07' is used to separate the fields.     *
*                                                                      *
*  - To keep the program consistent between EAST and WEST systems, the *
*    program uses SY-HOST field to determine the system and in the     *
*    INITIALIZATION step. HOST dependent fields are populated here,    *
*    for example report headings, file path, file number.              *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
*                                                                      *
*Changes:                                                              *
*                                                                      *
*IssueLog: Date:   By:            Description:                         *
*                                                                      *
************************************************************************

TABLES: BSAK,   "Accounting:Secondry index for Vendors (Cleared items)
        BSIK,   "Accounting:Secon. index for Vendors (Not Cleared items)
        BSAS,   "Accounting:Secondry index for G/L Accts (Cleared items)
        BSIS,   "Accounting:Sec. index for G/L Accts (Not Cleared items)
        T001,   "Company Codes
        ZJHDR,  "Journal Header for Consolidation
        ZACCT,  "Account Detail for Consolidation
        ZINTC.  "Inter Company  for Consolidation

*** File Header
DATA: BEGIN OF FILEHDR,
       CONTINUE_PROCESS_FLAG(01)   TYPE C,    "Upload - Yes/No
       RECORD_TYPE(02)             TYPE C,    "Record Type
       FILE_RECORD_COUNT(06)       TYPE C,    "Total Records in file
       FILE_TOTAL_DEBITS(17)       TYPE C,    "Sum of Debit  $ in file
       FILE_TOTAL_CREDITS(17)      TYPE C,    "Sum of Credit $ in file
       DSN_CONTROL_CODE(02)        TYPE C,    "Transmission Track code
       FILE_APPL_ID(03)            TYPE C,    "Application ID
       FILE_NUMBER(04)             TYPE N,    "File Number
      END   OF FILEHDR.

*** Journal Header
DATA: BEGIN OF JOURNAL OCCURS 0,
       JRNL_BATCH_NBR(10)          TYPE C,    "Journal Number 1,2,3...
       JRNL_HDR_BUSINESS_UNIT(05)  TYPE C,    "Business Unit
       JOURNAL_ID_MASK(06)         TYPE C,    "Journal Mask (Category)
       JOURNAL_ID_SEQ_NUM(04)      TYPE C,    "Segment Number
       JOURNAL_DATE(10)            TYPE C,    "Journal Date
       LEDGER_GROUP(10)            TYPE C,    "Ledger Group
       LEDGER(10)                  TYPE C,    "Ledger
       REVERSAL_CODE(01)           TYPE C,    "Journal Reversal Code
       REVERSAL_DATE(10)           TYPE C,    "Date of Journal Reversl
       JRNL_HDR_TOTAL_LINES(06)    TYPE N,    "Total Records in Journal
       JRNL_HDR_TOTAL_DEBITS(19)   TYPE C,    "Sum of Debit $ in Journal
       JRNL_HDR_TOTAL_CREDITS(19)  TYPE C,    "Sum of Credit$ in Journal
       SOURCE(03)                  TYPE C,    "Source - Entry came from
       OPERATOR_ID(08)             TYPE C,    "TSO ID-person entering Jr
       SYSTEM_SOURCE(03)           TYPE C,    "Jrnl file source -ext/int
       JRNL_HDR_LONG_DESCR(250)    TYPE C,    "Describes purpose of jrnl
      END   OF JOURNAL.

*** Detail Record
DATA: BEGIN OF DETAIL OCCURS 0,
       BUSINESS_UNIT_LN(05)        TYPE C,    "Owner of book/fin.record
       MONETARY_AMOUNT(19)         TYPE C,    "$ amount of journal line
       CF_ACCOUNT(07)              TYPE C,    "PS Account Number
       CF_RESOURCE_TYPE(05)        TYPE C,    "Source of charge
       CF_RESP_CENTER_TO(04)       TYPE C,    "Resp cntr receivingChrgs
       CF_RESP_CENTER_FROM(04)     TYPE C,    "Resp cntr performing work
       CF_PROCESS(07)              TYPE C,    "Grouping of a partic.type
       CF_AFFILIATE(05)            TYPE C,    "Counter party bus. unit
       CURRENCY_CD_LN(03)          TYPE C,    "Currency e.g. CAD
       EXCHANGE_RATE_TYPE_LN(05)   TYPE C,    "Exchange rate-US toother
       LINE_DESCR(30)              TYPE C,    "Line description
       FMIS_SEQ_NBR(06)            TYPE C,    "Sequential Journal line#
      END OF DETAIL.


DATA: BEGIN OF ITABDATA OCCURS 0,
            BUKRS  LIKE BSAK-BUKRS,          "Company code
            RACCT  LIKE BSAK-SAKNR,          "SAP account #
            RTCUR  LIKE T001-WAERS,          "Currency code
            AMOUNT LIKE BSAK-DMBTR,          "Dollars
            DRCRK  LIKE BSAK-SHKZG,          "Debit/Credit Indicator
            BLART  LIKE BSAK-BLART,          "Document Type
            LIFNR  LIKE BSAK-LIFNR,          "Vendor Number
            PSACT  LIKE ZACCT-PSACT,         "PS  account #
            JHDBU  LIKE ZJHDR-BUNIT,         "Journal Header Bus. Unit
            BUNIT  LIKE ZACCT-BUNIT,         "Business Unit
            BUNAM  LIKE ZJHDR-BUNAM,         "Business Unit Name
            RTYPE  LIKE ZACCT-RTYPE,         "Resource Type
            RCTO   LIKE ZACCT-RCTO,          "Rspnsblty Cntr Rec. Chrgs
            RCFRM  LIKE ZACCT-RCFRM,         "Rspnsblty Cntr Perf. Work
            AFFIL  LIKE ZACCT-AFFIL,         "Affiliate-Countr Party BU
            IC$    LIKE BSAK-DMBTR,          "I/C Amount
            PROCS  LIKE DETAIL-CF_PROCESS,   "Process
            STATUS(30) TYPE C,               "Current status
      END   OF ITABDATA.

DATA: BEGIN OF NOTFOUND OCCURS 0,    "Company code not found:table ZJHDR
        BUKRS LIKE GLT0-BUKRS,
      END   OF NOTFOUND.

DATA: W_DOLLARS      LIKE BSAK-DMBTR,
      W_WAERS        LIKE T001-WAERS,
      W_BUKRS        LIKE BSAK-BUKRS,
      NONCC$         LIKE BSAK-DMBTR,
      W_MONTH        LIKE T247-LTX,
      W_BLART        LIKE ZINTC-BLART,
      TEXT_COMN(36)  TYPE C,
      DEFAULT_VALUES VALUE 'N',
      LINE_COUNT     TYPE I,
      JHEAD_COUNT    TYPE I,
      FILE_TOT_RECS  TYPE I,
      NEXT_PERIOD(2) TYPE N,
      NEXT_YEAR(4)   TYPE N,
      FIRST_DAY      LIKE SY-DATUM,
      LAST_DAY       LIKE SY-DATUM,
      START_REC      TYPE I VALUE 1,
      END_REC        TYPE I VALUE 0,
      SAVE_JHDBU     LIKE ZJHDR-BUNIT,
      SAVE_BUNAM     LIKE ZJHDR-BUNAM,
      SAVE_RTCUR     LIKE T001-WAERS,
      FIRST_ROW(10)  TYPE C VALUE 'Heade8ftp',
      SECOND_ROW(7)  TYPE C VALUE 'Header',
      THIRD_ROW(7)   TYPE C VALUE 'Header',
      COMPANY_MAP(3) TYPE C.

DATA: DETL_REC TYPE STRING,
      FILE_REC TYPE STRING,
      JRNL_REC TYPE STRING,
      FULL_REC TYPE STRING.

CONSTANTS:  H_DMTR  TYPE X VALUE '09',
            TRN_FIELD1(2) TYPE C VALUE ' #',
            TRN_FIELD2(2) TYPE C VALUE '# '.

RANGES: IN_BLART FOR BSAK-BLART,
        IN_LIFNR FOR BSAK-LIFNR.

************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  P_YEAR LIKE BSAK-GJAHR DEFAULT SY-DATUM+0(4) OBLIGATORY, "Fiscal year
  P_PERD(2) TYPE N DEFAULT SY-DATUM+4(2) OBLIGATORY.       "Period

SELECT-OPTIONS:
  S_BUKRS  FOR BSAK-BUKRS NO INTERVALS.                   "Company Code
PARAMETERS:
  P_FILE        LIKE RFPDO-RFBIFILE,                      "File name
  DEF_CFLG(01)  TYPE C  DEFAULT 'Y'.               "Upload/Validate Flag

SELECTION-SCREEN SKIP 1.
*                                                     "Constant Data
SELECTION-SCREEN BEGIN OF BLOCK A3 WITH FRAME TITLE TEXT-003.
PARAMETERS:
   CON_RECT(02)  TYPE C  DEFAULT 'JE',       "Record Type
   CON_TTCD(02)  TYPE C  DEFAULT '04',       "Transmission Traking Code
   CON_FILE(04)  TYPE C,                     "File Number
   CON_RVRS(01)  TYPE C  DEFAULT 'N',        "Revere Journal Code
   CON_SRCE(03)  TYPE C  DEFAULT 'SAP',      "Source
   CON_OPID(08)  TYPE C  DEFAULT 'SAPFIA',   "Operator ID
   CON_SSRC(03)  TYPE C  DEFAULT 'EXT',      "System Source
   CON_PRCS(07)  TYPE C  DEFAULT 'ICACCT',   "Process
   CON_LGRP(10)  TYPE C  DEFAULT 'ACTUALS',  "Ledger Group
   CON_JIDM(06)  TYPE C  DEFAULT 'DEGTIC',   "Journal mask identifier
   CON_EXCH(05)  TYPE C  DEFAULT 'CRRNT'.    "Exchange Rate %

SELECTION-SCREEN END OF BLOCK A3.

SELECTION-SCREEN BEGIN OF BLOCK A4 WITH FRAME TITLE TEXT-004.
PARAMETERS:
   DEF_REST(05)  TYPE C  DEFAULT '99810'.    "Resource Type
SELECTION-SCREEN END OF BLOCK A4.

SELECTION-SCREEN BEGIN OF BLOCK A5 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT  1(62) TEXT-123.
         PARAMETERS: ABENDFLG   AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK A5.
SELECTION-SCREEN END OF BLOCK A1.

************************************************************************
INITIALIZATION.
*Set Report Heading, file number, file path.
 IF SY-HOST+0(3) = 'WEI'.
    MOVE 'Canada West - Inter Company Data For' TO TEXT_COMN.
    MOVE '0005'                           TO CON_FILE.
    CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
                '/IFFI060/icomwest.txt' INTO P_FILE.
*    CASE SY-HOST.
*      WHEN 'WEIrs2'.
*         MOVE '/usr/sap/interfaces/P11/IFFI060/icomwest.txt' TO P_FILE.
*      WHEN 'WEIrs1'.
*       IF SY-SYSID = 'C11'.
*         MOVE '/usr/sap/interfaces/C11/IFFI060/icomwest.txt' TO P_FILE.
*       ELSE.
*         MOVE '/usr/sap/interfaces/Q11/IFFI060/icomwest.txt' TO P_FILE.
*       ENDIF.
*      WHEN 'WEIrst'.
*         MOVE '/usr/sap/interfaces/SBX/IFFI060/icomwest.txt' TO P_FILE.
*    ENDCASE.
 ELSE.
    MOVE 'Canada East - Inter Company Data For' TO TEXT_COMN.
    MOVE '0002'                           TO CON_FILE.
    CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
                '/IFFI060/icomeast.txt' INTO P_FILE.
*    CASE SY-HOST.
*      WHEN 'chobis01'.
*         MOVE '/usr/sap/interfaces/P01/IFFI060/icomeast.txt' TO P_FILE.
*      WHEN 'chobisdv'.
*       IF SY-SYSID = 'D30'.
*         MOVE '/usr/sap/interfaces/D30/IFFI060/icomeast.txt' TO P_FILE.
*       ELSEIF SY-SYSID = 'D20'.
*         MOVE '/usr/sap/interfaces/D20/IFFI060/icomeast.txt' TO P_FILE.
*       ELSE.
*         MOVE '/usr/sap/interfaces/D22/IFFI060/icomeast.txt' TO P_FILE.
*       ENDIF.
*      WHEN 'chobissb'.
*         MOVE '/usr/sap/interfaces/D30/IFFI060/icomeast.txt' TO P_FILE.
*    ENDCASE.
 ENDIF.
************************************************************************
AT SELECTION-SCREEN.
   PERFORM VALID_PERIOD_CHECK.           "check if valid period entered
*   IF S_BUKRS[] IS INITIAL.             "CHANGE FOR VARIANTS
*      PERFORM FILL_COMPANY_CODES.
*   ENDIF.

************************************************************************
START-OF-SELECTION.
  IF S_BUKRS[] IS INITIAL.                   "CAHANGE FOR VARIANTS
     PERFORM FILL_COMPANY_CODES.
  ENDIF.
  PERFORM SETUP_DATES.
  PERFORM GET_GL_DATA.                   "get data from BSAK
  PERFORM PRINT_REPORT.
  PERFORM CREATE_OUTPUT_FILE.
END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM SETUP_DATES                                              *
*                                                                     *
*  1. Determine last day of period for which data is being extracted. *
*  2. Determine the first day of next period.                         *
*  3. Period could be from 01 to 13, consider 13 as 12.               *
*---------------------------------------------------------------------*
FORM SETUP_DATES.

 IF P_PERD > 12.
    CONCATENATE P_YEAR '1201' INTO FIRST_DAY.        "For period 13
 ELSE.
    CONCATENATE P_YEAR  P_PERD '01' INTO FIRST_DAY.
 ENDIF.

 CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
           DAY_IN            = FIRST_DAY
      IMPORTING
           LAST_DAY_OF_MONTH = LAST_DAY
      EXCEPTIONS
           DAY_IN_NO_DATE    = 1
           OTHERS            = 2
           .
 IF SY-SUBRC <> 0.
    WRITE: /5 'INVALID DATA FOR LAST_DAY'.
    STOP.
 ELSE.
    IF P_PERD > 11.
       NEXT_PERIOD = 01.
       NEXT_YEAR   = P_YEAR + 1.
       MOVE NEXT_PERIOD TO FIRST_DAY+4(2).
       MOVE NEXT_YEAR   TO FIRST_DAY+0(4).
    ELSE.
       NEXT_PERIOD = P_PERD + 1.
       MOVE NEXT_PERIOD    TO    FIRST_DAY+4(2).
    ENDIF.
 ENDIF.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHK_P_PERIOD                                             *
*                                                                     *
*    Validation for input period                                      *
*                                                                     *
*---------------------------------------------------------------------*
FORM VALID_PERIOD_CHECK.

  IF P_PERD GT 13  OR  P_PERD < 01.
     CALL FUNCTION 'POPUP_FOR_INTERACTION'
          EXPORTING
          HEADLINE    = '!! ERROR !!'
          TEXT1       = 'Invalid Period Selected'
          BUTTON_1    = 'OK'.
     STOP.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FILL_COMPANY_CODES.                                      *
*                                                                     *
*    Insert all company codes in ZJHDR table into variant "S_BUKRS"   *
*                                                                     *
*---------------------------------------------------------------------*
FORM FILL_COMPANY_CODES.

 SELECT * FROM ZJHDR.
      S_BUKRS-SIGN   = 'I'.
      S_BUKRS-OPTION = 'EQ'.
      S_BUKRS-LOW    = ZJHDR-BUKRS.
      APPEND S_BUKRS.
      CLEAR S_BUKRS.
 ENDSELECT.

ENDFORM.
*----------------------------------------------------------------------*
*       FORM GET_GL_DATA                                               *
*                                                                      *
*  This form reads data from posting summary table BSAK.               *
*                                                                      *
*----------------------------------------------------------------------*
FORM GET_GL_DATA.

 SELECT * FROM ZINTC.

  CLEAR IN_BLART.
  REFRESH IN_BLART.
  IF ZINTC-BLART <> SPACE.
     IN_BLART-SIGN   = 'I'.
     IN_BLART-OPTION = 'EQ'.
     IN_BLART-LOW    = ZINTC-BLART.
     APPEND IN_BLART.
  ENDIF.

  MOVE ZINTC-BLART TO W_BLART.
  IF ZINTC-LIFNR = SPACE.
     PERFORM NO_VENDOR_RECORD.
  ELSE.
     PERFORM YES_VENDOR_RECORD.
  ENDIF.

 ENDSELECT.

*SORT ITABDATA BY BUKRS LIFNR.
LOOP AT ITABDATA.
     IF ITABDATA-AMOUNT < 0.                                     "Credit
        ADD ITABDATA-AMOUNT TO: FILEHDR-FILE_TOTAL_CREDITS,
                               JOURNAL-JRNL_HDR_TOTAL_CREDITS.
     ELSE.
        ADD ITABDATA-AMOUNT TO: FILEHDR-FILE_TOTAL_DEBITS,       "Debit
                               JOURNAL-JRNL_HDR_TOTAL_DEBITS.
     ENDIF.
     IF ITABDATA-AMOUNT = 0.
        DELETE ITABDATA.
     ELSE.
        PERFORM BUILD_DETAIL_RECORD.
     ENDIF.
     AT END OF BUKRS.                                   "End of Journal
        PERFORM BUILD_JOURNAL_HEADER.
     ENDAT.
ENDLOOP.

  PERFORM BUILD_FILE_HEADER.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM     NO_VENDOR_RECORD.                                     *
*                                                                      *
*----------------------------------------------------------------------*
FORM  NO_VENDOR_RECORD.
    SELECT  BUKRS HKONT DMBTR SHKZG BLART
      INTO (BSAS-BUKRS, BSAS-HKONT, BSAS-DMBTR, BSAS-SHKZG, BSAK-BLART)
      FROM  BSAS
     WHERE  BUKRS IN S_BUKRS
       AND  HKONT = ZINTC-RACCT
       AND  GJAHR =  P_YEAR
       AND  BLART IN IN_BLART
       AND  MONAT =  P_PERD
     ORDER  BY BUKRS HKONT.
       ON CHANGE OF BSAS-BUKRS.
          MOVE BSAS-BUKRS TO W_BUKRS.
          PERFORM GET_COMPANY_CURRENCY.
       ENDON.
    IF BSAS-DMBTR <> 0.
       MOVE BSAS-HKONT TO ITABDATA-RACCT.
       MOVE BSAS-BUKRS TO ITABDATA-BUKRS.
       MOVE BSAS-SHKZG TO ITABDATA-DRCRK.
       MOVE BSAS-DMBTR TO ITABDATA-AMOUNT.
       MOVE BSAS-BLART TO ITABDATA-BLART.
       PERFORM ADD_THIS_RECORD.
    ENDIF.
    ENDSELECT.

    SELECT  BUKRS HKONT DMBTR SHKZG BLART
      INTO (BSIS-BUKRS, BSIS-HKONT, BSIS-DMBTR, BSIS-SHKZG, BSIS-BLART)
      FROM  BSIS
     WHERE  BUKRS IN S_BUKRS
       AND  HKONT =  ZINTC-RACCT
       AND  GJAHR =  P_YEAR
       AND  BLART IN IN_BLART
       AND  MONAT =  P_PERD
     ORDER  BY BUKRS HKONT.
       ON CHANGE OF BSIS-BUKRS.
          MOVE BSIS-BUKRS TO W_BUKRS.
          PERFORM GET_COMPANY_CURRENCY.
       ENDON.
    IF BSIS-DMBTR <> 0.
       MOVE BSIS-HKONT TO ITABDATA-RACCT.
       MOVE BSIS-BUKRS TO ITABDATA-BUKRS.
       MOVE BSIS-SHKZG TO ITABDATA-DRCRK.
       MOVE BSIS-DMBTR TO ITABDATA-AMOUNT.
       MOVE BSIS-BLART TO ITABDATA-BLART.
       PERFORM ADD_THIS_RECORD.
    ENDIF.
    ENDSELECT.

ENDFORM.
*----------------------------------------------------------------------*
*       FORM     YES_VENDOR_RECORD.
*
*                                                                      *
*----------------------------------------------------------------------*
FORM  YES_VENDOR_RECORD.
    SELECT  BUKRS SAKNR DMBTR LIFNR SHKZG BLART
      INTO (BSAK-BUKRS, BSAK-SAKNR, BSAK-DMBTR, BSAK-LIFNR,
            BSAK-SHKZG, BSAK-BLART)
      FROM  BSAK
     WHERE  BUKRS IN S_BUKRS
       AND  LIFNR = ZINTC-LIFNR
       AND  GJAHR =  P_YEAR
       AND  BLART IN IN_BLART
       AND  MONAT =  P_PERD
       AND  SAKNR = ZINTC-RACCT
     ORDER  BY BUKRS SAKNR.
       ON CHANGE OF BSAK-BUKRS.
          MOVE BSAK-BUKRS TO W_BUKRS.
          PERFORM GET_COMPANY_CURRENCY.
       ENDON.
    IF BSAK-DMBTR <> 0.
       MOVE BSAK-SAKNR TO ITABDATA-RACCT.
       MOVE BSAK-BUKRS TO ITABDATA-BUKRS.
       MOVE BSAK-SHKZG TO ITABDATA-DRCRK.
       MOVE BSAK-DMBTR TO ITABDATA-AMOUNT.
       MOVE BSAK-BLART TO ITABDATA-BLART.
       MOVE BSAK-LIFNR TO ITABDATA-LIFNR.
       PERFORM ADD_THIS_RECORD.
    ENDIF.
    ENDSELECT.

    SELECT  BUKRS SAKNR DMBTR LIFNR SHKZG
      INTO (BSIK-BUKRS, BSIK-SAKNR, BSIK-DMBTR, BSIK-LIFNR, BSIK-SHKZG)
      FROM  BSIK
     WHERE  BUKRS IN S_BUKRS
       AND  LIFNR =  ZINTC-LIFNR
       AND  GJAHR =  P_YEAR
       AND  BLART IN IN_BLART
       AND  MONAT =  P_PERD
       AND  SAKNR =  ZINTC-RACCT
     ORDER  BY BUKRS SAKNR.
       ON CHANGE OF BSIK-BUKRS.
          MOVE BSIK-BUKRS TO W_BUKRS.
          PERFORM GET_COMPANY_CURRENCY.
       ENDON.
    IF BSIK-DMBTR <> 0.
       MOVE BSIK-SAKNR TO ITABDATA-RACCT.
       MOVE BSIK-BUKRS TO ITABDATA-BUKRS.
       MOVE BSIK-SHKZG TO ITABDATA-DRCRK.
       MOVE BSIK-DMBTR TO ITABDATA-AMOUNT.
       MOVE BSIK-BLART TO ITABDATA-BLART.
       MOVE BSIK-LIFNR TO ITABDATA-LIFNR.
       PERFORM ADD_THIS_RECORD.
    ENDIF.
    ENDSELECT.

ENDFORM.

*----------------------------------------------------------------------*
*                GET_COMPANY_CURRENCY.                                 *
*                                                                      *
*----------------------------------------------------------------------*
FORM GET_COMPANY_CURRENCY.

  SELECT SINGLE * FROM ZJHDR
   WHERE BUKRS =  W_BUKRS.

   SELECT SINGLE WAERS INTO W_WAERS FROM T001   "Get comp.currency code
    WHERE BUKRS =  W_BUKRS.

ENDFORM.
*----------------------------------------------------------------------*
*               ADD_THIS_REOCRD.                                       *
*                                                                      *
*----------------------------------------------------------------------*
FORM ADD_THIS_RECORD.
DATA: KOUNT TYPE I,
      WORK_AMOUNT LIKE ITABDATA-AMOUNT.
DATA: COMP_IND, NORML_IND, VENDOR_IND, MULT_IND.

CLEAR ZACCT.
MOVE W_WAERS     TO ITABDATA-RTCUR.

*SELECT COUNT(*) INTO KOUNT FROM ZACCT
* WHERE RACCT = ZINTC-RACCT.
* IF KOUNT = 1.
*    SELECT SINGLE * FROM ZACCT
*     WHERE   RACCT = ZINTC-RACCT.
* ELSE.
*    SELECT SINGLE * FROM ZACCT
*     WHERE  RACCT = ZINTC-RACCT
*       AND  LIFNR = ZINTC-LIFNR.
* ENDIF.

    SELECT * FROM ZACCT
     WHERE RACCT = ZINTC-RACCT.
     IF ZACCT-LIFNR = ZINTC-LIFNR  AND
        ZACCT-LIFNR <> SPACE       AND
        ZACCT-BUKRS = ITABDATA-BUKRS.
        MOVE 'Y'  TO MULT_IND.
     ELSEIF ZACCT-BUKRS = ITABDATA-BUKRS.
        MOVE 'Y'  TO COMP_IND.
     ELSEIF ZACCT-LIFNR = ZINTC-LIFNR  AND
            ZACCT-LIFNR <> SPACE.
        MOVE 'Y'  TO VENDOR_IND.
     ELSE. MOVE 'Y' TO NORML_IND.
     ENDIF.

    ENDSELECT.

    IF MULT_IND = 'Y'.
       SELECT SINGLE * FROM ZACCT
        WHERE RACCT = ZINTC-RACCT
          AND LIFNR = ZINTC-LIFNR
          AND BUKRS = ITABDATA-BUKRS
          AND PSACT = ZINTC-PSACT1.
    ELSEIF COMP_IND = 'Y'.
       SELECT SINGLE * FROM ZACCT
        WHERE RACCT = ZINTC-RACCT
          AND LIFNR = SPACE
          AND BUKRS = ITABDATA-BUKRS
          AND PSACT = ZINTC-PSACT1.
    ELSEIF VENDOR_IND = 'Y'.
       SELECT SINGLE * FROM ZACCT
        WHERE RACCT = ZINTC-RACCT
          AND LIFNR = ZINTC-LIFNR
          AND BUKRS = SPACE
          AND PSACT = ZINTC-PSACT1.
    ELSE. SELECT SINGLE * FROM ZACCT
           WHERE RACCT = ZINTC-RACCT
             AND LIFNR = SPACE
             AND BUKRS = SPACE
             AND PSACT = ZINTC-PSACT1.
    ENDIF.

IF SY-SUBRC = 0.
   IF ZACCT-BUNIT <> SPACE.
      MOVE ZACCT-BUNIT TO ITABDATA-BUNIT.
   ELSE.
      MOVE ZJHDR-BUNIT TO ITABDATA-BUNIT.
   ENDIF.

   IF ZACCT-RCFRM <> SPACE.
      MOVE ZACCT-RCFRM TO ITABDATA-RCFRM.
   ELSE.
      MOVE ZJHDR-RCFRM TO ITABDATA-RCFRM.
   ENDIF.

   MOVE ZACCT-AFFIL TO ITABDATA-AFFIL.
   MOVE ZACCT-RTYPE TO ITABDATA-RTYPE.
   MOVE ZJHDR-RCTO  TO ITABDATA-RCTO.
   MOVE ZJHDR-BUNAM TO ITABDATA-BUNAM.
   MOVE ZJHDR-BUNIT TO ITABDATA-JHDBU.
   MOVE ITABDATA-AMOUNT TO WORK_AMOUNT.

   IF ITABDATA-DRCRK = 'S'.                            "Debit +
      ITABDATA-AMOUNT = WORK_AMOUNT * -1.
      ITABDATA-IC$    = WORK_AMOUNT.
      MOVE ZINTC-PSACT1 TO ITABDATA-PSACT.
      APPEND ITABDATA.
      CLEAR  ITABDATA-IC$.

      ITABDATA-AMOUNT = WORK_AMOUNT.
      MOVE ZINTC-PSACT2 TO ITABDATA-PSACT.
      MOVE CON_PRCS     TO ITABDATA-PROCS.
      APPEND ITABDATA.

      ITABDATA-AMOUNT = WORK_AMOUNT * -1.
      MOVE ZINTC-PSACT3 TO ITABDATA-PSACT.
      APPEND ITABDATA.
      CLEAR  ITABDATA-PROCS.

      ITABDATA-AMOUNT = WORK_AMOUNT.
      MOVE ZINTC-PSACT4 TO ITABDATA-PSACT.
      APPEND ITABDATA.
   ELSE.                                                 "Credit -
      ITABDATA-AMOUNT = WORK_AMOUNT.
      ITABDATA-IC$    = WORK_AMOUNT * -1.
      MOVE ZINTC-PSACT1 TO ITABDATA-PSACT.
      APPEND ITABDATA.
      CLEAR  ITABDATA-IC$.

      ITABDATA-AMOUNT = WORK_AMOUNT * -1.
      MOVE ZINTC-PSACT2 TO ITABDATA-PSACT.
      MOVE CON_PRCS     TO ITABDATA-PROCS.
      APPEND ITABDATA.

      ITABDATA-AMOUNT = WORK_AMOUNT.
      MOVE ZINTC-PSACT3 TO ITABDATA-PSACT.
      APPEND ITABDATA.
      CLEAR  ITABDATA-PROCS.

      ITABDATA-AMOUNT = WORK_AMOUNT * -1.
      MOVE ZINTC-PSACT4 TO ITABDATA-PSACT.
      APPEND ITABDATA.
   ENDIF.
ELSE.
 MOVE 'NO ZACCT MATCH' TO ITABDATA-STATUS.
 WRITE: / ITABDATA-BUKRS UNDER TEXT-102, ITABDATA-RACCT UNDER TEXT-103,
          ITABDATA-LIFNR UNDER TEXT-104, 27(16) ITABDATA-IC$ NO-ZERO,
          45(16) ITABDATA-AMOUNT,
          ITABDATA-PSACT UNDER TEXT-106, ITABDATA-AFFIL UNDER TEXT-107,
          ITABDATA-PROCS UNDER TEXT-108, ITABDATA-STATUS UNDER TEXT-115.
ENDIF.
   CLEAR ITABDATA.
ENDFORM.
*----------------------------------------------------------------------*
*       FORM     BUILD_DETAIL_RECORD.                                  *
*                                                                      *
*  This form builds a detail record.                                   *
*                                                                      *
*----------------------------------------------------------------------*
FORM  BUILD_DETAIL_RECORD.

       MOVE ITABDATA-BUNIT   TO DETAIL-BUSINESS_UNIT_LN.
       MOVE ITABDATA-AMOUNT  TO DETAIL-MONETARY_AMOUNT.
       MOVE ITABDATA-PSACT   TO DETAIL-CF_ACCOUNT.
       MOVE ITABDATA-RTYPE   TO DETAIL-CF_RESOURCE_TYPE.
       MOVE ITABDATA-RCTO    TO DETAIL-CF_RESP_CENTER_TO.
       MOVE ITABDATA-RCFRM   TO DETAIL-CF_RESP_CENTER_FROM.
       MOVE ITABDATA-PROCS   TO DETAIL-CF_PROCESS.
       MOVE ITABDATA-AFFIL   TO DETAIL-CF_AFFILIATE.
       MOVE ITABDATA-RTCUR   TO DETAIL-CURRENCY_CD_LN.
       MOVE CON_EXCH         TO DETAIL-EXCHANGE_RATE_TYPE_LN.
       CONCATENATE 'SAPfeed#for#A/C:' ITABDATA-RACCT+4(6) '#' P_PERD '/'
                    P_YEAR INTO DETAIL-LINE_DESCR.
       ADD 1                 TO LINE_COUNT.
       MOVE LINE_COUNT       TO DETAIL-FMIS_SEQ_NBR.
       APPEND: DETAIL.
       CLEAR:  DETAIL.
       MOVE ITABDATA-JHDBU   TO SAVE_JHDBU.
       MOVE ITABDATA-BUNAM   TO SAVE_BUNAM.
       MOVE ITABDATA-RTCUR   TO SAVE_RTCUR.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM     BUILD_JOURNAL_HEADER.                                 *
*                                                                      *
*  This form builds a journal header record.                           *
*                                                                      *
*----------------------------------------------------------------------*
*  Called by: Start-of-Selection                                       *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM  BUILD_JOURNAL_HEADER.

      ADD   1              TO JHEAD_COUNT.
      MOVE  JHEAD_COUNT    TO JOURNAL-JRNL_BATCH_NBR.
      MOVE  SAVE_JHDBU     TO JOURNAL-JRNL_HDR_BUSINESS_UNIT.
      MOVE  CON_JIDM       TO JOURNAL-JOURNAL_ID_MASK.
      MOVE  JHEAD_COUNT    TO JOURNAL-JOURNAL_ID_SEQ_NUM.

      CONCATENATE LAST_DAY+0(4) LAST_DAY+4(2) LAST_DAY+6(2)
                           INTO JOURNAL-JOURNAL_DATE.

      IF SAVE_RTCUR = 'USD'.
         MOVE  'ACTUALS'  TO: JOURNAL-LEDGER.
      ELSE.
         MOVE  'LOCAL'    TO: JOURNAL-LEDGER.
      ENDIF.
      MOVE  CON_RVRS       TO JOURNAL-REVERSAL_CODE.
      MOVE  LINE_COUNT     TO JOURNAL-JRNL_HDR_TOTAL_LINES.
      MOVE  CON_SRCE       TO JOURNAL-SOURCE.
      MOVE  CON_OPID       TO JOURNAL-OPERATOR_ID.
      MOVE  CON_SSRC       TO JOURNAL-SYSTEM_SOURCE.
      MOVE  CON_LGRP       TO JOURNAL-LEDGER_GROUP.

      SELECT SINGLE * FROM T001
       WHERE BUKRS =  ITABDATA-BUKRS.

      CONCATENATE P_PERD '/' P_YEAR '#Monthend Feed From SAP#'
                  ITABDATA-BUKRS '-' T001-BUTXT '#TO PeopleSoft#'
           SAVE_JHDBU '-' SAVE_BUNAM
                  INTO JOURNAL-JRNL_HDR_LONG_DESCR.
      ADD   LINE_COUNT     TO FILE_TOT_RECS.
    APPEND JOURNAL.
    CLEAR: LINE_COUNT, JOURNAL.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM     BUILD_FILE_HEADER.                                    *
*                                                                      *
*  This form builds a file header record.                              *
*                                                                      *
*----------------------------------------------------------------------*
*  Called by: Start-of-Selection                                       *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM BUILD_FILE_HEADER.

DATA: FINAL_TOTAL LIKE FILEHDR-FILE_TOTAL_CREDITS.

IF ABENDFLG = 'X'.
   FINAL_TOTAL = FILEHDR-FILE_TOTAL_CREDITS + FILEHDR-FILE_TOTAL_DEBITS.
   IF FINAL_TOTAL <> 0.
      MESSAGE E019 WITH TEXT-121.
   ENDIF.
ENDIF.
       FILE_TOT_RECS = FILE_TOT_RECS + JHEAD_COUNT + 1.
       MOVE  DEF_CFLG         TO  FILEHDR-CONTINUE_PROCESS_FLAG.
       MOVE  CON_RECT         TO  FILEHDR-RECORD_TYPE.
       MOVE  FILE_TOT_RECS    TO  FILEHDR-FILE_RECORD_COUNT.
       MOVE  CON_TTCD         TO  FILEHDR-DSN_CONTROL_CODE.
       MOVE  CON_SRCE         TO  FILEHDR-FILE_APPL_ID.
       MOVE  CON_FILE         TO  FILEHDR-FILE_NUMBER.

ENDFORM.

*----------------------------------------------------------------------*
*                 CREATE_OUTPUT_FILE                                   *
*                                                                      *
*Data available in Detail, Journal Header and File Header is transfered*
*to Output file on the server.                                         *
*----------------------------------------------------------------------*
FORM CREATE_OUTPUT_FILE.
 DATA: MSG(100).                           "open file - system message
 DATA: F_LENGTH TYPE I,
       LONG_DESCR TYPE STRING.
IF NOT DETAIL[] IS INITIAL.

 OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE MESSAGE MSG.
    IF SY-SUBRC NE '0'.
      MESSAGE E002 WITH P_FILE MSG.
      STOP.
    ELSE.
      CONCATENATE FIRST_ROW  H_DMTR INTO FIRST_ROW.
      CONCATENATE SECOND_ROW H_DMTR INTO SECOND_ROW.
      CONCATENATE THIRD_ROW  H_DMTR INTO THIRD_ROW.
      TRANSFER FIRST_ROW  TO P_FILE.
      TRANSFER SECOND_ROW TO P_FILE.
      TRANSFER THIRD_ROW  TO P_FILE.
      PERFORM PUT_SIGN_IN_FRONT CHANGING FILEHDR-FILE_TOTAL_DEBITS.
      PERFORM PUT_SIGN_IN_FRONT CHANGING FILEHDR-FILE_TOTAL_CREDITS.
      CONCATENATE FILEHDR-CONTINUE_PROCESS_FLAG FILEHDR-RECORD_TYPE
                  FILEHDR-FILE_RECORD_COUNT    FILEHDR-FILE_TOTAL_DEBITS
                  FILEHDR-FILE_TOTAL_CREDITS   FILEHDR-DSN_CONTROL_CODE
                  FILEHDR-FILE_APPL_ID         FILEHDR-FILE_NUMBER
                  INTO FILE_REC SEPARATED BY H_DMTR.
      CONDENSE FILE_REC NO-GAPS.

      LOOP AT JOURNAL.
           F_LENGTH = STRLEN( JOURNAL-JRNL_HDR_LONG_DESCR ).
           MOVE JOURNAL-JRNL_HDR_LONG_DESCR+0(F_LENGTH) TO LONG_DESCR.

           ADD JOURNAL-JRNL_HDR_TOTAL_LINES TO END_REC.
           TRANSLATE JOURNAL-JRNL_HDR_LONG_DESCR USING TRN_FIELD1.
      PERFORM PUT_SIGN_IN_FRONT CHANGING JOURNAL-JRNL_HDR_TOTAL_DEBITS.
      PERFORM PUT_SIGN_IN_FRONT CHANGING JOURNAL-JRNL_HDR_TOTAL_CREDITS.
      CONCATENATE JOURNAL-JRNL_BATCH_NBR  JOURNAL-JRNL_HDR_BUSINESS_UNIT
                  JOURNAL-JOURNAL_ID_MASK JOURNAL-JOURNAL_ID_SEQ_NUM
                  JOURNAL-JOURNAL_DATE    JOURNAL-LEDGER_GROUP
                  JOURNAL-LEDGER          JOURNAL-REVERSAL_CODE
                  JOURNAL-REVERSAL_DATE   JOURNAL-JRNL_HDR_TOTAL_LINES
            JOURNAL-JRNL_HDR_TOTAL_DEBITS JOURNAL-JRNL_HDR_TOTAL_CREDITS
                  JOURNAL-SOURCE          JOURNAL-OPERATOR_ID
                  JOURNAL-SYSTEM_SOURCE
                  INTO JRNL_REC SEPARATED BY H_DMTR.
      CONDENSE JRNL_REC NO-GAPS.
      CONCATENATE JRNL_REC LONG_DESCR INTO JRNL_REC
                           SEPARATED BY H_DMTR.
           TRANSLATE JRNL_REC USING TRN_FIELD2.
           PERFORM  MOVE_DETAIL_LINE_DATA.
           ADD JOURNAL-JRNL_HDR_TOTAL_LINES TO START_REC.
      ENDLOOP.

   CLOSE DATASET P_FILE.
   ENDIF.
 ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       MOVE_DETAIL_LINE_DATA                                          *
*                                                                      *
*  Called by: CREATE_OUTPUT_FILE                                       *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM MOVE_DETAIL_LINE_DATA.
     LOOP AT DETAIL FROM START_REC TO END_REC.
     PERFORM PUT_SIGN_IN_FRONT CHANGING DETAIL-MONETARY_AMOUNT.
     CONCATENATE  DETAIL-BUSINESS_UNIT_LN   DETAIL-MONETARY_AMOUNT
                  DETAIL-CF_ACCOUNT         DETAIL-CF_RESOURCE_TYPE
                  DETAIL-CF_RESP_CENTER_TO  DETAIL-CF_RESP_CENTER_FROM
                  SPACE SPACE DETAIL-CF_PROCESS SPACE SPACE SPACE SPACE
                  DETAIL-CF_AFFILIATE
                  SPACE SPACE                DETAIL-CURRENCY_CD_LN
                  DETAIL-EXCHANGE_RATE_TYPE_LN
                  SPACE SPACE SPACE SPACE SPACE
                  DETAIL-LINE_DESCR         DETAIL-FMIS_SEQ_NBR
                  INTO DETL_REC SEPARATED BY H_DMTR.
      CONDENSE DETL_REC NO-GAPS.
      TRANSLATE DETL_REC USING TRN_FIELD2.

      CONCATENATE FILE_REC JRNL_REC DETL_REC
      SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
      SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
      SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
      INTO FULL_REC SEPARATED BY H_DMTR.

      TRANSFER FULL_REC TO P_FILE.
     ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM PUT_SIGN_IN_FRONT.                                        *
*----------------------------------------------------------------------*

FORM PUT_SIGN_IN_FRONT CHANGING VALUE.
  DATA: TEXT1(1) TYPE C.

  SEARCH VALUE FOR '-'.
  IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
    SPLIT VALUE AT '-' INTO VALUE TEXT1.
    CONDENSE VALUE.
    CONCATENATE '-' VALUE INTO VALUE.
  ELSE.
    CONDENSE VALUE.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM PRINT_REPORT                                              *
*----------------------------------------------------------------------*
FORM PRINT_REPORT.
 DATA: TOT_AMOUNT LIKE ITABDATA-AMOUNT.

 LOOP AT ITABDATA.
 WRITE: / ITABDATA-BUKRS UNDER TEXT-102, ITABDATA-RACCT UNDER TEXT-103,
          ITABDATA-LIFNR UNDER TEXT-104, 27(16) ITABDATA-IC$ NO-ZERO,
          45(16) ITABDATA-AMOUNT,
          ITABDATA-PSACT UNDER TEXT-106, ITABDATA-AFFIL UNDER TEXT-107,
          ITABDATA-PROCS UNDER TEXT-108, ITABDATA-STATUS UNDER TEXT-115.
*           TOT_AMOUNT = TOT_AMOUNT + ITABDATA-IC$.

 AT END OF RACCT.
* AT END OF BUKRS.
    ULINE.
*    WRITE: /1 TEXT-120, 25(17) TOT_AMOUNT.
 ENDAT.
 ENDLOOP.
* ULINE.
ENDFORM.
*----------------------------------------------------------------------*
*                      TOP_OF_PAGE                                     *
*                                                                      *
*--------------------- TOP-OF-PAGE  -----------------------------------*

TOP-OF-PAGE.

  WRITE: / TEXT-RPT, SY-REPID, 42 TEXT-100,                 "Title
           90 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
           35 TEXT_COMN, 72 P_PERD, 74 '/', 75 P_YEAR,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.

  ULINE.

  WRITE: /1 TEXT-102, 10  TEXT-103, 19 TEXT-104, 27 TEXT-105,
         45 TEXT-109, 68 TEXT-106, 77 TEXT-107, 88 TEXT-108,
        106 TEXT-115.

  ULINE.
