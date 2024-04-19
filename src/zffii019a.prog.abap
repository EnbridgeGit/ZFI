REPORT ZFFII019A NO STANDARD PAGE HEADING LINE-COUNT 65
                                         LINE-SIZE 131 MESSAGE-ID ZS.
************************************************************************
* Date:  November 5, 2003.                                             *
* This program is a copy of ZFFII019. It's usage is one time only.     *
* Changes: 1- Multiply amount by -1.                                   *
*          2- Change reversal code vallue to N                         *
*          3- Make Journal_date as Reversal_date                       *
************************************************************************
*  Client:     Duke Energy.                                            *
*  Date:       Septeber 2003.                                          *
*  Author:     Mohammad Khan                                           *
*  Program Description:                                                *
*  This program will download O & M Data into a file for               *
*  upload into PeopleSoft system for consolidation purpose.            *
*  File Header Record and Journal Header records are part of detail    *
*  record that means file header and journal header are repeated with  *
*  each detail record.                                                 *
*                                                                      *
*  - If SAP company code doesn't exist in ZJHDR table, don't process   *
*    any account under this company and create exception record for    *
*    report printing.                                                  *
*                                                                      *
*  - Cost center group will be the journal for this program. Each cost *
*    center within a CC group will create a separate line in  the  file*
*    and report. For each record created, their will be an alternate   *
*    record with default values fed through variants, but it's amount  *
*    will have the opposite sign. For example, if original record has  *
*    debit amount, then default record will be credit and vice versa.  *
*                                                                      *
*  - Cost center groups in table ZOMGP must belong to one company.     *
*                                                                      *
*  - Hard coded delimeter Hex '07' is used to separate the fields.     *
*                                                                      *
*  - To keep the program consistent between EAST and WEST systems, the *
*    program uses SY-HOST field to determine the system and in the     *
*    INITIALIZATION step  HOST dependent fields are populated,         *
*    for example report headings, file path, file number.              *
*                                                                      *
************************************************************************
*                                                                      *
*Changes:                                                              *
*                                                                      *
*IssueLog: Date:   By:            Description:                         *
*                                                                      *
************************************************************************

TABLES: CSKS,      "Cost Center Master Data
        COSP,      "CO Object: Cost Totals for External Postings
        COSS,      "CO Object: Cost Totals for External Postings
        T001,      "Company Codes
        ZOMGP,     "O & M Group For Consolidation
        ZJHDR.     "Journal Header For Consolidation

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
       JOURNAL_DATE(08)            TYPE C,    "Journal Date
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
       CF_AFFILIATE(05)            TYPE C,    "Counter party bus. unit
       CURRENCY_CD_LN(03)          TYPE C,    "Currency e.g. CAD
       EXCHANGE_RATE_TYPE_LN(05)   TYPE C,    "Exchange rate-US toother
       LINE_DESCR(30)              TYPE C,    "Line description
       FMIS_SEQ_NBR(06)            TYPE C,    "Sequential Journal line#
      END OF DETAIL.


DATA: BEGIN OF ITABONM OCCURS 0,
            CCGRP  LIKE ZOMGP-CCGRP,       "Cost Center Group
            CEGRP  LIKE ZOMGP-CEGRP,       "Cost Element Group
            BUKRS  LIKE CSKS-BUKRS,          "Company code
            KOSTL  LIKE CSKS-KOSTL,        "Cost Center
            HBUNT  LIKE ZOMGP-HBUNT,       "Journal Header Bus. Unit
            DBUNT  LIKE ZOMGP-DBUNT,       "Business Unit
            RTCUR  LIKE T001-WAERS,        "Company Currency code
            AMOUNT LIKE COSS-WKG001,       "Dollars
            PSACT  LIKE ZOMGP-PSACT,       "PS  account #
            RTYPE  LIKE ZOMGP-RTYPE,       "Resource Type
            RCTO   LIKE ZOMGP-RCTO,        "Rspnsblty Cntr Rec. Chrgs
            RCFRM  LIKE ZOMGP-RCFRM,       "Rspnsblty Cntr Perf. Work
            RECTP  TYPE C,                 "Rec.Type,A-Actual, D-Default
            STATUS(35) TYPE C,             "Current status
      END   OF ITABONM.

DATA:   BEGIN OF VALTAB OCCURS 10.           "Orders for a group
        INCLUDE STRUCTURE RGSB4.
DATA:   END OF VALTAB.

DATA: W_DOLLARS      LIKE COSS-WKG001,
      CC$            LIKE COSS-WKG001,
      W_WAERS        LIKE T001-WAERS,
      W_KOKRS        LIKE CSKS-KOKRS,       "Controlling Area
      W_KTOPL        LIKE T004-KTOPL,       "Chart of Accounts
      W_MONTH        LIKE T247-LTX,
      TEXT_COMN(30)  TYPE C,
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
      SAVE_RTCUR     LIKE T001-WAERS,
      SAVE_HBUNT     LIKE ITABONM-HBUNT,
      SAVE_BUKRS     LIKE CSKS-BUKRS,
      SAVE_KOSTL     LIKE CSKS-KOSTL,
      SAVE_CCGRP     LIKE ZOMGP-CCGRP,
      FIRST_ROW(10)  TYPE C VALUE 'Heade8ftp',
      SECOND_ROW(7)  TYPE C VALUE 'Header',
      THIRD_ROW(7)   TYPE C VALUE 'Header'.

 DATA: DETL_REC TYPE STRING,
       FILE_REC TYPE STRING,
       JRNL_REC TYPE STRING,
       FULL_REC TYPE STRING.

 CONSTANTS:  H_DMTR  TYPE X VALUE '09',
             TRN_FIELD1(2) TYPE C VALUE ' #',
             TRN_FIELD2(2) TYPE C VALUE '# '.

RANGES: R_KOSTL FOR CSKS-KOSTL,         "Cost Centers
        R_KSTAR FOR COSS-KSTAR.          "Cost Elements

************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-101.
PARAMETERS:
  P_YEAR LIKE COSS-GJAHR DEFAULT SY-DATUM+0(4) OBLIGATORY, "Fiscal year
  P_PERD(2) TYPE N DEFAULT SY-DATUM+4(2) OBLIGATORY.       "Period
PARAMETERS:
  P_FILE        LIKE RFPDO-RFBIFILE,                       "File name
  DEF_CFLG(01)  TYPE C  DEFAULT 'Y'.               "Upload/Validate Flag

SELECTION-SCREEN SKIP 1.
*                                                     "Constant Data
SELECTION-SCREEN BEGIN OF BLOCK A3 WITH FRAME TITLE TEXT-103.
PARAMETERS:
   CON_RECT(02)  TYPE C  DEFAULT 'JE',       "Record Type
   CON_TTCD(02)  TYPE C  DEFAULT '04',       "Transmission Traking Code
   CON_FILE(04)  TYPE C,                     "File Number
   CON_RVRS(01)  TYPE C  DEFAULT 'N',        "Revere Journal Code
   CON_SRCE(03)  TYPE C  DEFAULT 'SAP',      "Source
   CON_OPID(08)  TYPE C  DEFAULT 'SAPFIA',  "Operator ID
   CON_SSRC(03)  TYPE C  DEFAULT 'EXT',      "System Source
   CON_LGRP(10)  TYPE C  DEFAULT 'ACTUALS',  "Ledger Group
   CON_JIDM(06)  TYPE C  DEFAULT 'DEGTOM',   "Journal mask identifier
   CON_EXCH(05)  TYPE C  DEFAULT 'CRRNT'.    "Exchange Rate %

SELECTION-SCREEN END OF BLOCK A3.
*                                                    "Default Data
SELECTION-SCREEN BEGIN OF BLOCK A4 WITH FRAME TITLE TEXT-105.
PARAMETERS:
   DEF_ACCT(07)  TYPE C  DEFAULT '0850001',  "PS Account #
   DEF_REST(05)  TYPE C  DEFAULT '99990'.    "Resource Type
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
    MOVE 'Canada West - O & M Data For'   TO TEXT_COMN.
    MOVE '0006'                           TO CON_FILE.
    MOVE '01'                             TO W_KOKRS.
    MOVE 'WECA'                           TO W_KTOPL.
    CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
                '/IFFI060/onmwest.txt' INTO P_FILE.
*    CASE SY-HOST.
*      WHEN 'WEIrs2'.
*          MOVE '/usr/sap/interfaces/P11/IFFI060/onmwest.txt' TO P_FILE.
*      WHEN 'WEIrs1'.
*       IF SY-SYSID = 'C11'.
*          MOVE '/usr/sap/interfaces/C11/IFFI060/onmwest.txt' TO P_FILE.
*       ELSE.
*          MOVE '/usr/sap/interfaces/Q11/IFFI060/onmwest.txt' TO P_FILE.
*       ENDIF.
*      WHEN 'WEIrst'.
*          MOVE '/usr/sap/interfaces/SBX/IFFI060/onmwest.txt' TO P_FILE.
*    ENDCASE.
 ELSE.
    MOVE 'Canada East - O & M Data For'   TO TEXT_COMN.
    MOVE '0003'                           TO CON_FILE.
    MOVE '10'                             TO W_KOKRS.
    MOVE 'COAT'                           TO W_KTOPL.
    CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
                '/IFFI060/onmeast.txt' INTO P_FILE.
*    CASE SY-HOST.
*      WHEN 'chobis01'.
*          MOVE '/usr/sap/interfaces/P01/IFFI060/onmeast.txt' TO P_FILE.
*      WHEN 'chobisdv'.
*       IF SY-SYSID = 'D30'.
*          MOVE '/usr/sap/interfaces/D30/IFFI060/onmeast.txt' TO P_FILE.
*       ELSEIF SY-SYSID = 'D20'.
*          MOVE '/usr/sap/interfaces/D20/IFFI060/onmeast.txt' TO P_FILE.
*       ELSE.
*          MOVE '/usr/sap/interfaces/D22/IFFI060/onmeast.txt' TO P_FILE.
*       ENDIF.
*      WHEN 'chobissb'.
*          MOVE '/usr/sap/interfaces/D30/IFFI060/onmeast.txt' TO P_FILE.
*    ENDCASE.
 ENDIF.

************************************************************************
AT SELECTION-SCREEN.
   PERFORM VALID_PERIOD_CHECK.           "check if valid period entered

************************************************************************
START-OF-SELECTION.
  PERFORM SETUP_DATES.
  PERFORM GET_ONM_DATA.                   "Get O & M Data
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

*----------------------------------------------------------------------*
*       FORM GET_ONM_DATA                                              *
*                                                                      *
*----------------------------------------------------------------------*
FORM GET_ONM_DATA.

CLEAR: CC$, W_DOLLARS.

SELECT * FROM ZOMGP.
 IF ZOMGP-CCGRP <> SAVE_CCGRP.
    MOVE ZOMGP-CCGRP  TO  SAVE_CCGRP.
    PERFORM GET_COST_CENTERS.
 ENDIF.
 IF ZOMGP-CEGRP <> SPACE.
    PERFORM GET_COST_ELEMENTS.
 ELSE.
    REFRESH R_KSTAR.
 ENDIF.

 PERFORM GET_DOLLARS.
ENDSELECT.
 PERFORM BUILD_FILE_RECORDS.
ENDFORM.
*----------------------------------------------------------------------*
*       FORM GET_COST_CENTERS                                          *
* Routine takes the CC Groups and determines the cost center ranges.   *
*                                                                      *
*----------------------------------------------------------------------*
FORM GET_COST_CENTERS.

  DATA: P_SET LIKE SETHIER-SHORTNAME.
  DATA: P_SETID    LIKE SETHIER-SETID,  "Internal ID of set to be read
        FIND_SETID LIKE SETHIER-SETID,  "Internal ID of set to be found
        MY_INT    TYPE I,
        MY_TABIX  LIKE SY-TABIX.

* Internal tables to hold set hierarchy, values, pointers and sorts
  DATA: SET_HIERARCHY    LIKE SETHIER     OCCURS 0 WITH HEADER LINE,
        SET_VALUES       LIKE SETVALUES   OCCURS 0 WITH HEADER LINE,
        SET_HIER_PTR     LIKE SETHIERPTR  OCCURS 0 WITH HEADER LINE,
        SET_VAL_PTR      LIKE SETVALPTR   OCCURS 0 WITH HEADER LINE,
        SET_HIER_SORT    LIKE SETHIERSRT  OCCURS 0 WITH HEADER LINE,
        SET_VAL_SORT     LIKE SETVALSRT   OCCURS 0 WITH HEADER LINE,
        FOUND_POINTERS   LIKE SETVALSRT   OCCURS 0 WITH HEADER LINE.

* Include to draw lines in lists
  INCLUDE <LINE>.

  DATA: SETNAME LIKE RKMAH-HNAM2.
  REFRESH: R_KOSTL.

    MOVE ZOMGP-CCGRP TO P_SET.
    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
         EXPORTING
              SHORTNAME = P_SET
              OLD_SETID = P_SETID
         IMPORTING
              NEW_SETID = P_SETID.

    CALL FUNCTION 'G_SET_TREE_IMPORT'
         EXPORTING
              SETID              = P_SETID  "Set-ID, not the Setname!!!
              NO_TABLE_BUFFERING = ' '
         TABLES
              SET_HIERARCHY      = SET_HIERARCHY
              SET_VALUES         = SET_VALUES.


    CALL FUNCTION 'G_SET_TREE_ADD_POINTERS'
         TABLES
              SET_HIERARCHY = SET_HIERARCHY
              SET_VALUES    = SET_VALUES
              SET_HIER_PTR  = SET_HIER_PTR
              SET_VAL_PTR   = SET_VAL_PTR
              SET_HIER_SORT = SET_HIER_SORT
              SET_VAL_SORT  = SET_VAL_SORT
         EXCEPTIONS
              OTHERS        = 1.

    CHECK SY-SUBRC = 0.


    LOOP AT SET_VAL_PTR.
      IF SET_VAL_PTR-FROM =  SET_VAL_PTR-TO.
         R_KOSTL-SIGN     =  'I'.
         R_KOSTL-OPTION   =  'EQ'.
         R_KOSTL-LOW      =  SET_VAL_PTR-FROM.
      ELSE.
         R_KOSTL-SIGN     =  'I'.
         R_KOSTL-OPTION   =  'BT'.
         R_KOSTL-LOW      =  SET_VAL_PTR-FROM.
         R_KOSTL-HIGH     =  SET_VAL_PTR-TO.
      ENDIF.
      APPEND R_KOSTL.
      CLEAR  R_KOSTL.
    ENDLOOP.                             "End of SET_VAL_PTR loop

ENDFORM.
*----------------------------------------------------------------------*
*       FORM GET_COST_ELEMENTS                                         *
* Routine takes the CE Groups and determines the cost element ranges.  *
*----------------------------------------------------------------------*
FORM GET_COST_ELEMENTS.
  DATA: SETNAME(30)  TYPE C.
  REFRESH: R_KSTAR.

  CONCATENATE W_KTOPL ZOMGP-CEGRP INTO SETNAME.
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
       EXPORTING
           CLASS         = '0102'
           SETNR         = SETNAME
           TABLE         = 'CCSS'
       TABLES
           SET_VALUES    = VALTAB
       EXCEPTIONS
           SET_NOT_FOUND = 1.

  IF SY-SUBRC = 0.
    LOOP AT VALTAB.
         IF VALTAB-FROM = VALTAB-TO.
            R_KSTAR-SIGN   = 'I'.
            R_KSTAR-OPTION = 'EQ'.
            R_KSTAR-LOW    = VALTAB-FROM.
         ELSE.
            R_KSTAR-SIGN   = 'I'.
            R_KSTAR-OPTION = 'BT'.
            R_KSTAR-LOW    = VALTAB-FROM.
            R_KSTAR-HIGH   = VALTAB-TO.
         ENDIF.
         APPEND R_KSTAR.
         CLEAR  R_KSTAR.
    ENDLOOP.                                     "End of VALTAB loop
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*       FORM GET_DOLLARS                                               *
*                                                                      *
*----------------------------------------------------------------------*
FORM GET_DOLLARS.

 CLEAR CC$.
 SELECT  KOSTL DATBI BUKRS OBJNR            "Get Cost Center Data
   INTO (CSKS-KOSTL, CSKS-DATBI, CSKS-BUKRS, CSKS-OBJNR)
   FROM  CSKS
  WHERE  KOKRS = W_KOKRS
    AND  KOSTL IN R_KOSTL
  ORDER BY KOSTL ASCENDING DATBI DESCENDING.
  IF CSKS-KOSTL <> SAVE_KOSTL.
     MOVE CSKS-KOSTL TO SAVE_KOSTL.

      SELECT * FROM COSS
       WHERE   OBJNR =  CSKS-OBJNR
         AND   GJAHR =  P_YEAR
         AND   WRTTP =  '04'
         AND   KSTAR IN R_KSTAR.
         ADD COSS-WKG001 FROM 1 TO P_PERD GIVING W_DOLLARS.
         CC$ = CC$ + W_DOLLARS.
      ENDSELECT.

      SELECT * FROM COSP
       WHERE   OBJNR = CSKS-OBJNR
         AND   GJAHR = P_YEAR
         AND   WRTTP = '04'
         AND   KSTAR IN R_KSTAR.
         ADD COSP-WKG001 FROM 1 TO P_PERD GIVING W_DOLLARS.
         CC$ = CC$ + W_DOLLARS.
      ENDSELECT.

  IF CC$ <> 0.                             "Eliminate zero amount rows
     PERFORM BUILD_ITABONM_TABLE.
     CLEAR CC$.
  ENDIF.
  ENDIF.
 ENDSELECT.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM BUILD_ITABONM_TABLE.                                      *
*                                                                      *
*----------------------------------------------------------------------*
FORM BUILD_ITABONM_TABLE.

* First record with table ZOMGP Values.
     CC$ = CC$ * -1.                       "One time change
     MOVE ZOMGP-HBUNT   TO  ITABONM-HBUNT.
     MOVE ZOMGP-CCGRP   TO  ITABONM-CCGRP.
     MOVE ZOMGP-CEGRP   TO  ITABONM-CEGRP.
     MOVE ZOMGP-DBUNT   TO  ITABONM-DBUNT.
     MOVE ZOMGP-PSACT   TO  ITABONM-PSACT.
     MOVE ZOMGP-RTYPE   TO  ITABONM-RTYPE.
     MOVE ZOMGP-RCTO    TO  ITABONM-RCTO.
     MOVE ZOMGP-RCFRM   TO  ITABONM-RCFRM.
     MOVE CSKS-KOSTL    TO  ITABONM-KOSTL.
     MOVE CSKS-BUKRS    TO  ITABONM-BUKRS.
     MOVE CC$           TO  ITABONM-AMOUNT.
     MOVE 'A'           TO  ITABONM-RECTP.      "Actual Record
     PERFORM GET_COMPANY_CURRENCY_CODE.
     APPEND ITABONM.

* Second record with default Values.
    SELECT SINGLE * FROM ZJHDR
     WHERE BUKRS = CSKS-BUKRS.
     IF SY-SUBRC = 0.
        MOVE ZJHDR-RCTO  TO  ITABONM-RCTO.
        MOVE ZJHDR-RCFRM TO  ITABONM-RCFRM.
     ELSE.
        MOVE ZOMGP-RCTO  TO  ITABONM-RCTO.
        MOVE ZOMGP-RCFRM TO  ITABONM-RCFRM.
        CONCATENATE 'COMP.CODE' CSKS-BUKRS 'NOTFOUND IN ZJHDR'
                     INTO ITABONM-STATUS SEPARATED BY SPACE.
*       This message means investigate for data validity and
*       RCTO and RCFRM should be corrected from the downloaded data
*       or from the PeopleSoft System.
     ENDIF.
      MOVE ZOMGP-HBUNT   TO  ITABONM-DBUNT.
      MOVE DEF_ACCT      TO  ITABONM-PSACT.
      MOVE DEF_REST      TO  ITABONM-RTYPE.
      ITABONM-AMOUNT = ITABONM-AMOUNT * -1.
      MOVE 'D'           TO  ITABONM-RECTP.     "Default Record
      APPEND ITABONM.
      CLEAR: ITABONM, CC$.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM BUILD_FILE_RECORDS.                                       *
*                                                                      *
*----------------------------------------------------------------------*
FORM BUILD_FILE_RECORDS.

LOOP AT ITABONM.
     IF ITABONM-AMOUNT < 0.
        ADD ITABONM-AMOUNT TO: FILEHDR-FILE_TOTAL_CREDITS,
                               JOURNAL-JRNL_HDR_TOTAL_CREDITS.
     ELSE.
        ADD ITABONM-AMOUNT TO: FILEHDR-FILE_TOTAL_DEBITS,       "Debit
                               JOURNAL-JRNL_HDR_TOTAL_DEBITS.
     ENDIF.
     PERFORM BUILD_DETAIL_RECORD.
     AT END OF CCGRP.                                  "End of Journal
        PERFORM BUILD_JOURNAL_HEADER.
     ENDAT.
ENDLOOP.

  PERFORM BUILD_FILE_HEADER.

ENDFORM.

*----------------------------------------------------------------------*
*       GET_COMPANY_CURRENCY_CODE.                                     *
*                                                                      *
*----------------------------------------------------------------------*
FORM GET_COMPANY_CURRENCY_CODE.

 SELECT SINGLE * FROM T001
  WHERE BUKRS =  CSKS-BUKRS.
  IF SY-SUBRC = 0.
     MOVE T001-WAERS  TO  ITABONM-RTCUR.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*                   BUILD_DETAIL_RECORD.                               *
*             This form builds a detail record.                        *
*                                                                      *
*----------------------------------------------------------------------*
FORM  BUILD_DETAIL_RECORD.

       MOVE ITABONM-DBUNT   TO DETAIL-BUSINESS_UNIT_LN.
       MOVE ITABONM-AMOUNT  TO DETAIL-MONETARY_AMOUNT.
       MOVE ITABONM-PSACT   TO DETAIL-CF_ACCOUNT.
       MOVE ITABONM-RTYPE   TO DETAIL-CF_RESOURCE_TYPE.
       MOVE ITABONM-RCTO    TO DETAIL-CF_RESP_CENTER_TO.
       MOVE ITABONM-RCFRM   TO DETAIL-CF_RESP_CENTER_FROM.
       MOVE SPACE           TO DETAIL-CF_AFFILIATE.
       MOVE ITABONM-RTCUR   TO DETAIL-CURRENCY_CD_LN.
       MOVE CON_EXCH        TO DETAIL-EXCHANGE_RATE_TYPE_LN.

       CONCATENATE 'O&M#for#Cost#Centre' ITABONM-KOSTL
                    INTO DETAIL-LINE_DESCR.
       ADD 1                TO LINE_COUNT.
       MOVE LINE_COUNT      TO DETAIL-FMIS_SEQ_NBR.
       APPEND: DETAIL.
       CLEAR:  DETAIL.
       MOVE ITABONM-HBUNT   TO SAVE_HBUNT.
       MOVE ITABONM-RTCUR   TO SAVE_RTCUR.
       MOVE ITABONM-BUKRS   TO SAVE_BUKRS.
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
      MOVE  SAVE_HBUNT     TO JOURNAL-JRNL_HDR_BUSINESS_UNIT.
      MOVE  JHEAD_COUNT    TO JOURNAL-JOURNAL_ID_SEQ_NUM.

*      CONCATENATE LAST_DAY+0(4) LAST_DAY+4(2) LAST_DAY+6(2)
*                           INTO JOURNAL-JOURNAL_DATE.  "One time change
      CONCATENATE FIRST_DAY+0(4) FIRST_DAY+4(2) FIRST_DAY+6(2)
                  INTO: JOURNAL-JOURNAL_DATE.

      IF SAVE_RTCUR = 'USD'.
         MOVE  'ACTUALS'  TO: JOURNAL-LEDGER.
      ELSE.
         MOVE  'LOCAL'    TO: JOURNAL-LEDGER.
      ENDIF.
      MOVE  CON_RVRS       TO JOURNAL-REVERSAL_CODE.
      MOVE  LINE_COUNT     TO JOURNAL-JRNL_HDR_TOTAL_LINES.
      MOVE  CON_SRCE       TO JOURNAL-SOURCE.
      MOVE  CON_OPID       TO JOURNAL-OPERATOR_ID.
      MOVE  CON_JIDM       TO JOURNAL-JOURNAL_ID_MASK.
      MOVE  CON_SSRC       TO JOURNAL-SYSTEM_SOURCE.
      MOVE  CON_LGRP       TO JOURNAL-LEDGER_GROUP.

    CONCATENATE 'O&M Report Grouping for SAP Company Code#' SAVE_BUKRS
                 INTO JOURNAL-JRNL_HDR_LONG_DESCR.
    ADD   LINE_COUNT     TO FILE_TOT_RECS.
    APPEND JOURNAL.
    CLEAR: LINE_COUNT, JOURNAL, SAVE_BUKRS.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM     BUILD_FILE_HEADER.                                    *
*                                                                      *
*  This form builds a file header record.                              *
*  It will abend the program if file total credit is not equal to      *
*  filoe total debit.                                                  *
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
           CLEAR JOURNAL-REVERSAL_DATE.
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
*                                                                      *
*       MOVE_DETAIL_LINE_DATA                                          *
*                                                                      *
*----------------------------------------------------------------------*
FORM MOVE_DETAIL_LINE_DATA.
     LOOP AT DETAIL FROM START_REC TO END_REC.
     PERFORM PUT_SIGN_IN_FRONT CHANGING DETAIL-MONETARY_AMOUNT.
     CONCATENATE  DETAIL-BUSINESS_UNIT_LN   DETAIL-MONETARY_AMOUNT
                  DETAIL-CF_ACCOUNT         DETAIL-CF_RESOURCE_TYPE
                  DETAIL-CF_RESP_CENTER_TO  DETAIL-CF_RESP_CENTER_FROM
                  SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE
                  SPACE SPACE                DETAIL-CURRENCY_CD_LN
                  DETAIL-EXCHANGE_RATE_TYPE_LN
                  SPACE SPACE SPACE SPACE SPACE
                  DETAIL-LINE_DESCR         DETAIL-FMIS_SEQ_NBR SPACE
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
*                                                                      *
*       FORM PRINT_REPORT                                              *
*                                                                      *
*----------------------------------------------------------------------*
FORM PRINT_REPORT.
 DATA: TOT_AMOUNT LIKE ITABONM-AMOUNT.
 SORT ITABONM BY CCGRP CEGRP KOSTL.

 LOOP AT ITABONM.

 WRITE: /  ITABONM-CCGRP UNDER TEXT-001,  ITABONM-CEGRP UNDER TEXT-003,
           ITABONM-KOSTL UNDER TEXT-005,  48(16) ITABONM-AMOUNT,
           ITABONM-DBUNT UNDER TEXT-008,  ITABONM-RCFRM UNDER TEXT-009,
           ITABONM-PSACT UNDER TEXT-011,  ITABONM-RTYPE UNDER TEXT-013,
           ITABONM-STATUS UNDER TEXT-015.
           IF ITABONM-RECTP = 'A'.       "Actual record, not default one
              TOT_AMOUNT = TOT_AMOUNT + ITABONM-AMOUNT.
           ENDIF.
 AT END OF CCGRP.
    ULINE.
    WRITE: /1 TEXT-120, 48(16) TOT_AMOUNT.
    CLEAR TOT_AMOUNT.
    ULINE.
 ENDAT.
 ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*                                                                      *
*                      TOP_OF_PAGE                                     *
*                                                                      *
*----------------------------------------------------------------------*

TOP-OF-PAGE.

  WRITE: / TEXT-RPT, SY-REPID, 47 TEXT-100,                 "Title
           103 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
           44 TEXT_COMN, 75 P_PERD, 77 '/', 78 P_YEAR,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  ULINE.
  WRITE: /1 TEXT-001, 20 TEXT-003, 40 TEXT-005, 48 TEXT-007,
         66 TEXT-008, 74 TEXT-009, 83 TEXT-011, 92 TEXT-013,
         99 TEXT-015.
  ULINE.

