REPORT ZFFII017 NO STANDARD PAGE HEADING LINE-COUNT 65
                                         LINE-SIZE 120 MESSAGE-ID ZS.
************************************************************************
*  Client:     Duke Energy.                                            *
*  Date:       July 2003.                                              *
*  Author:     Mohammad Khan                                           *
*  Program Description:                                                *
*  This program will download G/L YTD Trial Balances into a file for   *
*  upload into PeopleSoft system for consolidation purpose.            *
*  File Header and Journal headers will be part of each detail record. *
*  A Journal Headere will be created for each company code.            *
*                                                                      *
*  - All accounts from table GLT0 that match the selection criteria    *
*    will be downloaded to file even if no info exist in ZACCT table.  *
*    When no account info is found in ZACCT table, the record is       *
*    created with default values.                                      *
*                                                                      *
*  - If SAP company code doesn't exist in ZJHDR table, don't process   *
*    any account under this company and create exception record for    *
*    report printing.                                                  *
*                                                                      *
*  - If SAP account Number is not found in ZACCT table then use the    *
*    default data fed through variants and create a record for the file*
*    and create an exception record as well for printing in the report.*
*                                                                      *
*  - Hard coded delimeter Hex '07' is used to separate the fields.     *
*                                                                      *
*  - This program will abend if file header total debit amount is not  *
*    equal to file header total credit amount.                         *
*                                                                      *
*  - To keep the program consistent between EAST and WEST systems, the *
*    program uses SY-HOST field to determine the system and in the     *
*    INITIALIZATION step  HOST dependent fields are populated here,    *
*    for example report headings, file path, file number.              *
*                                                                      *
************************************************************************
*                                                                      *
*Changes:                                                              *
*                                                                      *
*IssueLog: Date:     By:    Description:                               *
*  794    14/10/2004 M Khan Changes for an extra run at begin of year. *
*                           1- Add reversal date in the variants and if*
*                           reversal date is entered:                  *
*                           a)use this date as reversal date for output*
*                             file.                                    *
*                           b)Take only balance forward and multiply by*
*                             -1 and use it as amount for an account.  *
*                             Don't take amount for the periods.       *
*                                                                      *
* 1091    18/04/2005 M Khan Apply the BOY changes to vandor record as  *
*                           as well, i.e, take balance carried forward *
*                           only.                                      *
* TR105   29/08/2005 M Khan Select Actual amount only as PLAN amount   *
*                           is going to be introduced in FI.           *
************************************************************************

TABLES: GLT0,    "G/L Account Master record transaction figures
        BSIS,    "Accounting: Secondary Index for G/L Accounts
        LFC1,    "Vendor Master (Transaction Figures)
        T001,    "Company Codes
        ZJHDR,   "Journal Header for Consolidation
        ZACCT.   "Account Detail for Consolidation

************************************************************************
*            note NOTE note Note nOte                                  *
*                                                                      *
*         use LFB1 for gl account / lifner cross reference            *
*                                                                      *
*                                                                      *
************************************************************************

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
       CF_PROCESS(07)              TYPE C,    "PROCESS
       CF_LOCATION(09)             TYPE C,    "Location
       CF_PRODUCT(06)              TYPE C,    "Product
       CF_AFFILIATE(05)            TYPE C,    "Counter party bus. unit
       CURRENCY_CD_LN(03)          TYPE C,    "Currency e.g. CAD
       EXCHANGE_RATE_TYPE_LN(05)   TYPE C,    "Exchange rate-US toother
       LINE_DESCR(30)              TYPE C,    "Line description
       FMIS_SEQ_NBR(06)            TYPE C,    "Sequential Journal line#
      END OF DETAIL.


DATA: BEGIN OF SUBGLT0 OCCURS 0,
            BUKRS  LIKE GLT0-BUKRS,          "Company code
            RACCT  LIKE GLT0-RACCT,          "SAP account #
            RTCUR  LIKE GLT0-RTCUR,          "Currency code
            AMOUNT LIKE GLT0-HSL01,          "Dollars
*            DRCRK  LIKE GLT0-DRCRK,          "Debit/Credit Indicator
            PSACT  LIKE ZACCT-PSACT,         "PS  account #
            JHDBU  LIKE ZJHDR-BUNIT,         "Journal Header Bus. Unit
            BUNIT  LIKE ZACCT-BUNIT,         "Business Unit
            BUNAM  LIKE ZJHDR-BUNAM,         "Business Unit Name
            RTYPE  LIKE ZACCT-RTYPE,         "Resource Type
            RCTO   LIKE ZACCT-RCTO,          "Rspnsblty Cntr Rec. Chrgs
            RCFRM  LIKE ZACCT-RCFRM,         "Rspnsblty Cntr Perf. Work
            AFFIL  LIKE ZACCT-AFFIL,         "Affiliate-Countr Party BU
            PROCS  LIKE ZACCT-PROCS,         "Process
            LOCTN  LIKE ZACCT-LOCTN,         "Location
            PRDCT  LIKE ZACCT-PRDCT,         "Product
            STATUS(34) TYPE C,               "Current status
            ERROR      TYPE C,               "Error Record - Print Only
      END   OF SUBGLT0.

DATA: BEGIN OF ITAB OCCURS 0,         "Used for accounts by cost centers
        KOSTL LIKE BSIS-KOSTL,
        DMBTR LIKE BSIS-DMBTR,
      END   OF ITAB.

DATA: BEGIN OF NOTFOUND OCCURS 0,    "Company code not found:table ZJHDR
        BUKRS LIKE GLT0-BUKRS,
      END   OF NOTFOUND.

DATA: W_DOLLARS      LIKE GLT0-HSL01,
      W_WAERS        LIKE T001-WAERS,
      NONCC$         LIKE BSIS-DMBTR,
      W_MONTH        LIKE T247-LTX,
      TEXT_COMN(30)  TYPE C,
      POSTED_IND     TYPE C,   "A/C posted/unposted as vendor or CC A/C
      VENDOR_DEFAULT TYPE C,
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
      SAVE_RTCUR     LIKE GLT0-RTCUR,
      FIRST_ROW(10)  TYPE C VALUE 'Heade8ftp',
      SECOND_ROW(7)  TYPE C VALUE 'Header',
      THIRD_ROW(7)   TYPE C VALUE 'Header',
      SAVE_BUKRS     LIKE GLT0-BUKRS,
      SAVE_RACCT     LIKE ZACCT-RACCT,
      SAVE_LIFNR     LIKE ZACCT-LIFNR,
      COMPANY_MAP(3) TYPE C.

 DATA: DETL_REC TYPE STRING,
       FILE_REC TYPE STRING,
       JRNL_REC TYPE STRING,
       FULL_REC TYPE STRING.

DATA: BEGIN OF STRUC1,
       UM01H LIKE LFC1-UM01H,
       UM02H LIKE LFC1-UM01H,
       UM03H LIKE LFC1-UM01H,
       UM04H LIKE LFC1-UM01H,
       UM05H LIKE LFC1-UM01H,
       UM06H LIKE LFC1-UM01H,
       UM07H LIKE LFC1-UM01H,
       UM08H LIKE LFC1-UM01H,
       UM09H LIKE LFC1-UM01H,
       UM10H LIKE LFC1-UM01H,
       UM11H LIKE LFC1-UM01H,
       UM12H LIKE LFC1-UM01H,
       UM13H LIKE LFC1-UM01H,
       UM14H LIKE LFC1-UM01H,
      END OF STRUC1.

DATA: BEGIN OF STRUC2,
       UM01S LIKE LFC1-UM01S,
       UM02S LIKE LFC1-UM01S,
       UM03S LIKE LFC1-UM01S,
       UM04S LIKE LFC1-UM01S,
       UM05S LIKE LFC1-UM01S,
       UM06S LIKE LFC1-UM01S,
       UM07S LIKE LFC1-UM01S,
       UM08S LIKE LFC1-UM01S,
       UM09S LIKE LFC1-UM01S,
       UM10S LIKE LFC1-UM01S,
       UM11S LIKE LFC1-UM01S,
       UM12S LIKE LFC1-UM01S,
       UM13S LIKE LFC1-UM01S,
       UM14S LIKE LFC1-UM01S,
      END OF STRUC2.

 CONSTANTS:  H_DMTR  TYPE X VALUE '09',
             TRN_FIELD1(2) TYPE C VALUE ' #',
             TRN_FIELD2(2) TYPE C VALUE '# '.

************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  P_YEAR LIKE GLT0-RYEAR DEFAULT SY-DATUM+0(4) OBLIGATORY, "Fiscal year
  P_PERD(2) TYPE N DEFAULT SY-DATUM+4(2) OBLIGATORY.       "Period
SELECT-OPTIONS:
  S_BUKRS  FOR GLT0-BUKRS NO INTERVALS,                    "CompanyCode
  S_RACCT  FOR GLT0-RACCT.                                 "Acct #
PARAMETERS:
  P_FILE        LIKE RFPDO-RFBIFILE,                       "File name
  DEF_CFLG(01)  TYPE C  DEFAULT 'Y'.               "Upload/Validate Flag

SELECTION-SCREEN SKIP 1.
*                                                     "Constant Data
SELECTION-SCREEN BEGIN OF BLOCK A3 WITH FRAME TITLE TEXT-003.
PARAMETERS:
   CON_RECT(02)  TYPE C  DEFAULT 'JE',       "Record Type
   CON_TTCD(02)  TYPE C  DEFAULT '04',       "Transmission Traking Code
   CON_FILE(04)  TYPE C,                     "File Number
   CON_RVRS(01)  TYPE C  DEFAULT 'D',        "Revere Journal Code
   CON_SRCE(03)  TYPE C  DEFAULT 'SAP',      "Source
   CON_OPID(08)  TYPE C  DEFAULT 'SAPFIA',   "Operator ID
   CON_SSRC(03)  TYPE C  DEFAULT 'EXT',      "System Source
   CON_LGRP(10)  TYPE C  DEFAULT 'ACTUALS',  "Ledger Group
   CON_JIDM(06)  TYPE C  DEFAULT 'DEGTWT',   "Journal mask identifier
   CON_EXCH(05)  TYPE C  DEFAULT 'CRRNT'.    "Exchange Rate %

SELECTION-SCREEN END OF BLOCK A3.
*                                                    "Default Data
SELECTION-SCREEN BEGIN OF BLOCK A4 WITH FRAME TITLE TEXT-004.
PARAMETERS:
   DEF_ACCT(07)  TYPE C  DEFAULT '0184007',  "PS Account #
   DEF_REST(05)  TYPE C  DEFAULT '99810'.    "Resource Type
SELECTION-SCREEN END OF BLOCK A4.

SELECTION-SCREEN BEGIN OF BLOCK A41 WITH FRAME TITLE TEXT-005.  "I794
PARAMETERS: BOY_DATE TYPE SY-DATUM.                             "I794
SELECTION-SCREEN END OF BLOCK A41.                              "I794

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
   MOVE 'Canada West -Trial Balance For' TO TEXT_COMN.
   MOVE '0004'                           TO CON_FILE.
   CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
               '/IFFI060/tbalwest.txt' INTO P_FILE.
*    CASE SY-HOST.
*      WHEN 'WEIrs2'.
*         MOVE '/usr/sap/interfaces/P11/IFFI060/tbalwest.txt' TO P_FILE.
*      WHEN 'WEIrs1'.
*       IF SY-SYSID = 'C11'.
*         MOVE '/usr/sap/interfaces/C11/IFFI060/tbalwest.txt' TO P_FILE.
*       ELSE.
*         MOVE '/usr/sap/interfaces/Q11/IFFI060/tbalwest.txt' TO P_FILE.
*       ENDIF.
*      WHEN 'WEIrst'.
*         MOVE '/usr/sap/interfaces/SBX/IFFI060/tbalwest.txt' TO P_FILE.
*    ENDCASE.
ELSE.
   MOVE 'Canada East -Trial Balance For' TO TEXT_COMN.
   MOVE '0001'                           TO CON_FILE.
   CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
               '/IFFI060/tbaleast.txt' INTO P_FILE.
*    CASE SY-HOST.
*      WHEN 'chobis01'.
*         MOVE '/usr/sap/interfaces/P01/IFFI060/tbaleast.txt' TO P_FILE.
*      WHEN 'chobisdv'.
*       IF SY-SYSID = 'D30'.
*         MOVE '/usr/sap/interfaces/D30/IFFI060/tbaleast.txt' TO P_FILE.
*       ELSEIF SY-SYSID = 'D20'.
*         MOVE '/usr/sap/interfaces/D20/IFFI060/tbaleast.txt' TO P_FILE.
*       ELSE.
*         MOVE '/usr/sap/interfaces/D22/IFFI060/tbaleast.txt' TO P_FILE.
*       ENDIF.
*      WHEN 'chobissb'.
*         MOVE '/usr/sap/interfaces/D30/IFFI060/tbaleast.txt' TO P_FILE.
*    ENDCASE.
 ENDIF.

************************************************************************
AT SELECTION-SCREEN.
   PERFORM VALID_PERIOD_CHECK.           "check if valid period entered

************************************************************************
START-OF-SELECTION.
  PERFORM SETUP_DATES.
  PERFORM GET_GL_DATA.                   "get data from GLT0
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
*       FORM GET_GL_DATA                                               *
*                                                                      *
*  This form reads data from G/L posting summary table GLT0. Use 'On   *
*  Change Of GLT0-Racct' from ordered GLT0 select to process per       *
*  SAP Company/G/L account.                                            *
*                                                                      *
*----------------------------------------------------------------------*
FORM GET_GL_DATA.

SELECT * FROM GLT0
 WHERE   RLDNR = '00'                  "Ledger Only
   AND   RRCTY = '0'                   "Actual                 TR105
   AND   BUKRS IN S_BUKRS              "Company Code
   AND   RYEAR = P_YEAR                "Year
   AND   RACCT IN S_RACCT              "G/L Account #
         ORDER BY BUKRS RACCT.

  IF SY-SUBRC = 0.
     ON CHANGE OF GLT0-BUKRS.
        COMPANY_MAP = 'YES'.
        PERFORM CHECK_COMPANY_CODE.
     ENDON.
     IF COMPANY_MAP = 'YES'.
        IF SAVE_BUKRS = GLT0-BUKRS AND  "To accomodate Vendor & CC A/C
           SAVE_RACCT = GLT0-RACCT AND
           POSTED_IND = 'Y'.
        ELSE.
           POSTED_IND = 'N'.
           SAVE_BUKRS = GLT0-BUKRS.
           SAVE_RACCT = GLT0-RACCT.
           PERFORM BUILD_SUBGLT0_TABLE.
        ENDIF.
     ENDIF.
  ENDIF.
ENDSELECT.

LOOP AT SUBGLT0.
     IF SUBGLT0-AMOUNT = 0.
        DELETE SUBGLT0.
     ENDIF.
ENDLOOP.

LOOP AT SUBGLT0.
  IF SUBGLT0-ERROR <> 'Y'.
*     IF SUBGLT0-DRCRK = 'H'.                                   "Credit
     IF SUBGLT0-AMOUNT < 0.
        ADD SUBGLT0-AMOUNT TO: FILEHDR-FILE_TOTAL_CREDITS,
                               JOURNAL-JRNL_HDR_TOTAL_CREDITS.
     ELSE.
        ADD SUBGLT0-AMOUNT TO: FILEHDR-FILE_TOTAL_DEBITS,       "Debit
                               JOURNAL-JRNL_HDR_TOTAL_DEBITS.
     ENDIF.
        PERFORM BUILD_DETAIL_RECORD.
     AT END OF BUKRS.                                  "End of Journal
        PERFORM BUILD_JOURNAL_HEADER.
     ENDAT.
  ENDIF.
ENDLOOP.

  PERFORM BUILD_FILE_HEADER.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM     BUILD_SUBGLT0_TABLE.                                  *
*                                                                      *
*  This form builds a Subset of GLT0 with one record for each account. *
*                                                                      *
*----------------------------------------------------------------------*
FORM  BUILD_SUBGLT0_TABLE.

      MOVE GLT0-RACCT TO SUBGLT0-RACCT.
      MOVE W_WAERS    TO SUBGLT0-RTCUR.
      MOVE GLT0-BUKRS TO SUBGLT0-BUKRS.
*      MOVE GLT0-DRCRK TO SUBGLT0-DRCRK.
      CLEAR: W_DOLLARS.
      IF BOY_DATE = '00000000'.                                  "I794
         ADD GLT0-HSL01 FROM 1 TO P_PERD GIVING W_DOLLARS.
         W_DOLLARS = W_DOLLARS + GLT0-HSLVT.        "Add Bal.Carried fwd
      ELSE.                                                      "I794
         W_DOLLARS = W_DOLLARS + GLT0-HSLVT * -1.                "I794
      ENDIF.                                                     "I794
      MOVE W_DOLLARS        TO SUBGLT0-AMOUNT.
      PERFORM CHECK_ACCOUNT_MAPPING.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM     CHEK_COMPANY_CODE.                                    *
*                                                                      *
*  This form checks for the existence of company code in               *
*  table ZJHDR.                                                        *
*                                                                      *
*----------------------------------------------------------------------*
FORM CHECK_COMPANY_CODE.

  SELECT SINGLE * FROM ZJHDR
   WHERE BUKRS = GLT0-BUKRS.
   IF SY-SUBRC <> 0.
      MOVE 'NO' TO COMPANY_MAP.
      PERFORM COMPANY_NOT_FOUND.
   ELSE.
   SELECT SINGLE WAERS INTO W_WAERS FROM T001   "Get comp.currency code
    WHERE BUKRS =  GLT0-BUKRS.
   ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*                COMPANY_NOT_FOUND.                                    *
*  Creates an internal table of company codes not found on table ZJHDR.*
*----------------------------------------------------------------------*
FORM COMPANY_NOT_FOUND.

 MOVE GLT0-BUKRS TO NOTFOUND-BUKRS.
 APPEND NOTFOUND.
 CLEAR  NOTFOUND.

ENDFORM.

*----------------------------------------------------------------------*
*                PERFORM CHEK_ACCOUNT_MAPPING.                         *
* Determines how each account is to be handled depending upon the data *
* in ZACCT table.                                                      *
*----------------------------------------------------------------------*
FORM  CHECK_ACCOUNT_MAPPING.
DATA: DFALT_IND,  DBDC_IND, CCNTR_IND, COMP_IND, NORML_IND,
      VENDOR_IND, MULT_IND, ERROR_IND.

SELECT * FROM ZACCT
 WHERE   RACCT = GLT0-RACCT.
    IF ZACCT-LIFNR <> SPACE AND
       ZACCT-BUKRS = SPACE  AND
       ZACCT-KOSTL = SPACE  AND
       ZACCT-DBDC  = SPACE.
       MOVE 'Y' TO VENDOR_IND.
    ELSEIF ZACCT-DBDC  = GLT0-DRCRK AND
           ZACCT-BUKRS = GLT0-BUKRS AND
           ZACCT-LIFNR = SPACE AND
           ZACCT-KOSTL = SPACE.
           MOVE 'Y' TO MULT_IND.
    ELSEIF ZACCT-BUKRS =  GLT0-BUKRS AND
           ZACCT-KOSTL =  SPACE AND
           ZACCT-LIFNR = SPACE  AND
           ZACCT-DBDC  =  SPACE.
           MOVE 'Y' TO COMP_IND.
    ELSEIF ZACCT-DBDC  = GLT0-DRCRK AND
           ZACCT-BUKRS = SPACE AND
           ZACCT-LIFNR = SPACE AND
           ZACCT-KOSTL = SPACE.
           MOVE 'Y' TO DBDC_IND.
    ELSEIF ZACCT-BUKRS =  GLT0-BUKRS AND
           ZACCT-KOSTL <> SPACE AND
           ZACCT-LIFNR =  SPACE AND
           ZACCT-DBDC  =  SPACE.
           MOVE 'Y' TO CCNTR_IND.
    ELSEIF ZACCT-BUKRS =  SPACE  AND
           ZACCT-KOSTL =  SPACE  AND
           ZACCT-LIFNR =  SPACE  AND
           ZACCT-DBDC  =  SPACE.
           MOVE 'Y' TO NORML_IND.
    ELSE.
           MOVE 'Y' TO ERROR_IND.
    ENDIF.

ENDSELECT.
 IF SY-SUBRC <> 0.
        MOVE 'Y' TO DFALT_IND.
        MOVE 'NO MAPPING FOUND-DEFAULTS USED' TO  SUBGLT0-STATUS.
 ENDIF.

         IF VENDOR_IND = 'Y'.
            PERFORM ACCOUNT_BY_VENDOR.
     ELSEIF MULT_IND = 'Y'.
            PERFORM ACCOUNT_BY_MULTI.
     ELSEIF COMP_IND = 'Y'.
            PERFORM ACCOUNT_BY_COMPANY.
     ELSEIF DBDC_IND = 'Y'.
            PERFORM ACCOUNT_BY_DBDC.
     ELSEIF CCNTR_IND = 'Y'.
            PERFORM ACCOUNT_BY_COST_CENTER.
     ELSEIF NORML_IND = 'Y'.
            PERFORM ACCOUNT_NORMAL.
     ELSEIF DFALT_IND = 'Y'.
            PERFORM ACCOUNT_BY_DEFAULT.
            CLEAR SUBGLT0.
      ELSE. PERFORM ACCOUNT_WITH_ERROR.   "No Valid Combo in ZACCT Table
     ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*                      ACCOUNT_BY_VENDOR                               *
*Following are the important points for this routine.                  *
* 1- There could be multiple records in GLT0 belonging to same vendor  *
*    & account but we need only one record to know that this account   *
*    should be processed as vendor account using table LFC1. Due to    *
*    this reason, the rest of the GLT0 enties and blocked from         *
*    processing using POSTED_IND field.                                *
*                                                                      *
* 2- If the total amount coming through GLT0 is not equal to the amount*
*    collected from the LFC1 table, then the difference is posted to a *
*    record with default values.                                       *
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_VENDOR.

DATA: TOT_UMH       LIKE W_DOLLARS,
      TOT_UMS       LIKE W_DOLLARS,
      VENDOR_GLT0_$ LIKE W_DOLLARS,
      VENDOR_LFC1_$ LIKE W_DOLLARS.

*Get all entries total for this account from table GLT0.
 SELECT * FROM GLT0
  WHERE   RRCTY = '0'                   "Actual                 TR105
    AND   BUKRS = SAVE_BUKRS
    AND   RYEAR = P_YEAR
    AND   RACCT = SAVE_RACCT.
     CLEAR: W_DOLLARS.
     IF BOY_DATE = '00000000'.                                     "I794
      ADD GLT0-HSL01 FROM 1 TO P_PERD GIVING W_DOLLARS.
      VENDOR_GLT0_$ = VENDOR_GLT0_$ + W_DOLLARS + GLT0-HSLVT.
     ELSE.                                                         "I794
       VENDOR_GLT0_$ = VENDOR_GLT0_$ + ( GLT0-HSLVT * -1 ).        "I794
     ENDIF.                                                        "I794
 ENDSELECT.

  SELECT * FROM ZACCT
   WHERE   RACCT = GLT0-RACCT
     AND   BUKRS = SPACE
     AND   KOSTL = SPACE
     AND   DBDC  = SPACE.
  IF  SY-SUBRC = 0.
      SELECT  * FROM  LFC1
       WHERE  LIFNR = ZACCT-LIFNR
         AND  BUKRS = GLT0-BUKRS
         AND  GJAHR = P_YEAR.

      CLEAR: SUBGLT0-AMOUNT, STRUC1, STRUC2.
   IF BOY_DATE = '00000000'.                                  "I1091
      MOVE-CORRESPONDING LFC1 TO STRUC1.
      MOVE-CORRESPONDING LFC1 TO STRUC2.
      ADD STRUC1-UM01H FROM 1 TO P_PERD GIVING TOT_UMH.
      ADD STRUC2-UM01S FROM 1 TO P_PERD GIVING TOT_UMS.
      SUBGLT0-AMOUNT = TOT_UMS + LFC1-UMSAV - TOT_UMH.
   ELSE.
      SUBGLT0-AMOUNT = LFC1-UMSAV * -1.                       "I1091
*      IF BOY_DATE <> '00000000'.                              "I794
*         SUBGLT0-AMOUNT = SUBGLT0-AMOUNT * -1.                "I794
   ENDIF.                                                      "I794
      IF SUBGLT0-AMOUNT <> 0.
         VENDOR_LFC1_$ = VENDOR_LFC1_$ + SUBGLT0-AMOUNT.
         PERFORM ADD_THIS_RECORD.
         POSTED_IND = 'Y'.
         CLEAR: SUBGLT0-AMOUNT.
      ENDIF.
     ENDSELECT.
   ENDIF.
  ENDSELECT.
*  IF BOY_DATE = '00000000'.                                   "I794
  IF VENDOR_GLT0_$ <> VENDOR_LFC1_$.
     SUBGLT0-AMOUNT = VENDOR_GLT0_$ - VENDOR_LFC1_$.
     MOVE 'DEFAULT REC. WITH REMAINING $' TO SUBGLT0-STATUS.
     POSTED_IND = 'Y'.
     PERFORM ACCOUNT_BY_DEFAULT.
  ENDIF.
*  ENDIF.                                                      "I794
  CLEAR: SUBGLT0, VENDOR_LFC1_$.
ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_BY_MULTI                                               *
*                                                                      *
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_MULTI.

* SELECT SINGLE * FROM ZACCT
*  WHERE   RACCT = GLT0-RACCT
*    AND   BUKRS = GLT0-BUKRS
*    AND   KOSTL = SPACE
*    AND   DBDC  = GLT0-DRCRK.     "H-Credit, S-Debit
* IF  SY-SUBRC = 0.
*     PERFORM ADD_THIS_RECORD.
*     CLEAR SUBGLT0.
* ELSE.
*     MOVE 'INVALID A/C, DB/DC COMBO-DEF.USED' TO  SUBGLT0-STATUS.
*     PERFORM ACCOUNT_BY_DEFAULT.
*     CLEAR SUBGLT0.
* ENDIF.

*Get all entries total for this account from table GLT0.
DATA:  MULTI_GLT0_$  LIKE W_DOLLARS,
       SAVE_DRCRK    LIKE GLT0-DRCRK.

 SELECT * FROM GLT0
  WHERE   RRCTY = '0'                   "Actual                 TR105
    AND   BUKRS = SAVE_BUKRS
    AND   RYEAR = P_YEAR
    AND   RACCT = SAVE_RACCT.
     CLEAR: W_DOLLARS.
     IF BOY_DATE = '00000000'.                                     "I794
      ADD GLT0-HSL01 FROM 1 TO P_PERD GIVING W_DOLLARS.
      MULTI_GLT0_$ = MULTI_GLT0_$ + W_DOLLARS + GLT0-HSLVT.
     ELSE.                                                         "I794
      MULTI_GLT0_$ = MULTI_GLT0_$ + W_DOLLARS + GLT0-HSLVT * -1.   "I794
     ENDIF.                                                        "I794
 ENDSELECT.
 IF MULTI_GLT0_$ <> 0.
    MOVE MULTI_GLT0_$  TO SUBGLT0-AMOUNT.
    IF   MULTI_GLT0_$ < 0.
         MOVE 'H'  TO  SAVE_DRCRK.
    ELSE.
         MOVE 'S'  TO  SAVE_DRCRK.
    ENDIF.

    SELECT SINGLE * FROM ZACCT
     WHERE RACCT = GLT0-RACCT
     AND   BUKRS = GLT0-BUKRS
     AND   KOSTL = SPACE
     AND   DBDC  = SAVE_DRCRK.     "H-Credit, S-Debit
     IF  SY-SUBRC = 0.
         PERFORM ADD_THIS_RECORD.
         POSTED_IND = 'Y'.
     ELSE.
     MOVE 'INVALID A/C, DB/DC COMBO-DEF.USED' TO  SUBGLT0-STATUS.
           PERFORM ACCOUNT_BY_DEFAULT.
           POSTED_IND = 'Y'.
     ENDIF.
 ENDIF.

  CLEAR: SUBGLT0.
ENDFORM.
*----------------------------------------------------------------------*
*       ACCOUNT_BY_DBDC.                                               *
*                                                                      *
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_DBDC.
*Get all entries total for this account from table GLT0.
DATA:  DBDC_GLT0_$   LIKE W_DOLLARS,
       SAVE_DRCRK    LIKE GLT0-DRCRK.

 SELECT * FROM GLT0
  WHERE   RRCTY = '0'                               "Actual     TR105
    AND   BUKRS = SAVE_BUKRS
    AND   RYEAR = P_YEAR
    AND   RACCT = SAVE_RACCT.
     CLEAR: W_DOLLARS.
     IF BOY_DATE = '00000000'.                                  "I794
      ADD GLT0-HSL01 FROM 1 TO P_PERD GIVING W_DOLLARS.
      DBDC_GLT0_$ = DBDC_GLT0_$ + W_DOLLARS + GLT0-HSLVT.
     ELSE.                                                      "I794
      DBDC_GLT0_$ = DBDC_GLT0_$ + W_DOLLARS + GLT0-HSLVT * -1.  "I794
     ENDIF.                                                     "I794
 ENDSELECT.

 IF DBDC_GLT0_$ <> 0.
    MOVE DBDC_GLT0_$  TO SUBGLT0-AMOUNT.
    IF   DBDC_GLT0_$ < 0.
         MOVE 'H'  TO  SAVE_DRCRK.
    ELSE.
         MOVE 'S'  TO  SAVE_DRCRK.
    ENDIF.

 SELECT SINGLE * FROM ZACCT
  WHERE   RACCT = GLT0-RACCT
    AND   BUKRS = SPACE
    AND   KOSTL = SPACE
    AND   DBDC  = SAVE_DRCRK.     "H-Credit, S-Debit
    IF  SY-SUBRC = 0.
        PERFORM ADD_THIS_RECORD.
        POSTED_IND = 'Y'.
        CLEAR SUBGLT0.
    ELSE.
        MOVE 'INVALID A/C, DB/DC COMBO-DEF.USED' TO  SUBGLT0-STATUS.
        PERFORM ACCOUNT_BY_DEFAULT.
        POSTED_IND = 'Y'.
        CLEAR SUBGLT0.
    ENDIF.
 ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_BY_COMPANY.                                            *
*                                                                      *
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_COMPANY.

 SELECT SINGLE * FROM ZACCT
  WHERE   RACCT = GLT0-RACCT
    AND   BUKRS = GLT0-BUKRS
    AND   KOSTL = SPACE
    AND   DBDC  = SPACE.
 IF  SY-SUBRC = 0.
     PERFORM ADD_THIS_RECORD.
     CLEAR SUBGLT0.
 ELSE.
     MOVE 'INVALID A/C, COMPNY COMBO-DEF.USED' TO  SUBGLT0-STATUS.
     PERFORM ACCOUNT_BY_DEFAULT.
     CLEAR SUBGLT0.
 ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*                 ACCOUNT_BY_COST_CENTER.                              *
*                                                                      *
*    There could be multiple records in GLT0 belonging to same vendor  *
*    & account but we need only one record to know that this account   *
*    should be processed as CC account using table BSIS. Due to this   *
*    reason, the rest of the GLT0 enties and blocked from processing   *
*    using POSTED_IND field.                                           *
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_COST_CENTER.

     SELECT * FROM BSIS
      WHERE   BUKRS = GLT0-BUKRS
        AND   HKONT = GLT0-RACCT
        AND   GJAHR = P_YEAR
        AND   MONAT <= P_PERD.
     IF SY-SUBRC = 0.
        MOVE BSIS-KOSTL TO ITAB-KOSTL.
        MOVE BSIS-DMBTR TO ITAB-DMBTR.
        COLLECT ITAB.
        CLEAR   ITAB.
     ENDIF.
     ENDSELECT.

     CLEAR NONCC$.
     LOOP AT ITAB.
          SELECT SINGLE * FROM ZACCT
           WHERE   RACCT = GLT0-RACCT
             AND   BUKRS = GLT0-BUKRS
             AND   KOSTL = ITAB-KOSTL.

          IF SY-SUBRC <> 0.
             ADD ITAB-DMBTR TO NONCC$.
          ELSE.
             PERFORM ADD_THIS_RECORD.
             POSTED_IND = 'Y'.
          ENDIF.
     ENDLOOP.
     IF NONCC$ <> 0.
        SELECT SINGLE * FROM ZACCT
         WHERE   RACCT = GLT0-RACCT
           AND   BUKRS = SPACE
           AND   KOSTL = SPACE
           AND   DBDC  = SPACE.
        IF SY-SUBRC = 0.
           PERFORM ADD_THIS_RECORD.
        ELSE.
           PERFORM ACCOUNT_BY_DEFAULT.
        ENDIF.
        POSTED_IND = 'Y'.
     ENDIF.
     CLEAR: ITAB, SUBGLT0.

ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_NORMAL.                                                *
*                                                                      *
*----------------------------------------------------------------------*
FORM ACCOUNT_NORMAL.

 SELECT SINGLE * FROM ZACCT
  WHERE   RACCT = GLT0-RACCT
    AND   BUKRS = SPACE
    AND   KOSTL = SPACE
    AND   DBDC  = SPACE.
 IF  SY-SUBRC = 0.
     PERFORM ADD_THIS_RECORD.
     CLEAR SUBGLT0.
 ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_BY_DEFAULT.                                            *
*This routie is performed when account no. is not found in ZACCT table *
*Or when no valid COMBO is found in ZACCT table.
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_DEFAULT.
     MOVE ZJHDR-BUNIT TO SUBGLT0-BUNIT.
     MOVE ZJHDR-RCTO  TO SUBGLT0-RCTO.
     MOVE ZJHDR-RCFRM TO SUBGLT0-RCFRM.
     MOVE DEF_ACCT    TO SUBGLT0-PSACT.
     MOVE DEF_REST    TO SUBGLT0-RTYPE.
     COLLECT SUBGLT0.
*     CLEAR  SUBGLT0.
ENDFORM.
*----------------------------------------------------------------------*
*       ACCOUNT_WITH_ERROR.                                            *
*                                                                      *
*----------------------------------------------------------------------*
FORM ACCOUNT_WITH_ERROR.
     MOVE 'NO VALID A/C COMBO-DEFAULTS USED' TO  SUBGLT0-STATUS.
     PERFORM ACCOUNT_BY_DEFAULT.
     CLEAR SUBGLT0.
ENDFORM.
*----------------------------------------------------------------------*
*               ADD_THIS_REOCRD.                                       *
*                                                                      *
*----------------------------------------------------------------------*
FORM ADD_THIS_RECORD.

     IF ZACCT-BUNIT <> SPACE.
        MOVE ZACCT-BUNIT TO SUBGLT0-BUNIT.   "To be used by DetailRecord
     ELSE.
        MOVE ZJHDR-BUNIT TO SUBGLT0-BUNIT.
     ENDIF.

     IF ZACCT-RCTO <> SPACE.
        MOVE ZACCT-RCTO TO SUBGLT0-RCTO.
     ELSE.
        MOVE ZJHDR-RCTO TO SUBGLT0-RCTO.
     ENDIF.

     IF ZACCT-RCFRM <> SPACE.
        MOVE ZACCT-RCFRM TO SUBGLT0-RCFRM.
     ELSE.
        MOVE ZJHDR-RCFRM TO SUBGLT0-RCFRM.
     ENDIF.

     MOVE ZACCT-PSACT TO SUBGLT0-PSACT.
     MOVE ZJHDR-BUNAM TO SUBGLT0-BUNAM.
     MOVE ZJHDR-BUNIT TO SUBGLT0-JHDBU.      "To be used for JHDR Record
     MOVE ZACCT-RTYPE TO SUBGLT0-RTYPE.
     MOVE ZACCT-AFFIL TO SUBGLT0-AFFIL.
     MOVE ZACCT-PROCS TO SUBGLT0-PROCS.
     MOVE ZACCT-LOCTN TO SUBGLT0-LOCTN.
     MOVE ZACCT-PRDCT TO SUBGLT0-PRDCT.

     COLLECT SUBGLT0.
*     CLEAR  SUBGLT0.
ENDFORM.
*----------------------------------------------------------------------*
*                   BUILD_DETAIL_RECORD.                               *
*             This form builds a detail record.                        *
*                                                                      *
*----------------------------------------------------------------------*
FORM  BUILD_DETAIL_RECORD.

       MOVE SUBGLT0-BUNIT   TO DETAIL-BUSINESS_UNIT_LN.
       MOVE SUBGLT0-AMOUNT  TO DETAIL-MONETARY_AMOUNT.
       MOVE SUBGLT0-PSACT   TO DETAIL-CF_ACCOUNT.
       MOVE SUBGLT0-RTYPE   TO DETAIL-CF_RESOURCE_TYPE.
       MOVE SUBGLT0-RCTO    TO DETAIL-CF_RESP_CENTER_TO.
       MOVE SUBGLT0-RCFRM   TO DETAIL-CF_RESP_CENTER_FROM.
       MOVE SUBGLT0-PROCS   TO DETAIL-CF_PROCESS.
       MOVE SUBGLT0-LOCTN   TO DETAIL-CF_LOCATION.
       MOVE SUBGLT0-PRDCT   TO DETAIL-CF_PRODUCT.
       MOVE SUBGLT0-AFFIL   TO DETAIL-CF_AFFILIATE.
       MOVE SUBGLT0-RTCUR   TO DETAIL-CURRENCY_CD_LN.
       MOVE CON_EXCH        TO DETAIL-EXCHANGE_RATE_TYPE_LN.
       CONCATENATE 'SAPfeed#for#A/C:' SUBGLT0-RACCT+4(6) '#' P_PERD '/'
                    P_YEAR INTO DETAIL-LINE_DESCR.
       ADD 1                TO LINE_COUNT.
       MOVE LINE_COUNT      TO DETAIL-FMIS_SEQ_NBR.
       APPEND: DETAIL.
       CLEAR:  DETAIL.
       MOVE SUBGLT0-JHDBU   TO SAVE_JHDBU.
       MOVE SUBGLT0-BUNAM   TO SAVE_BUNAM.
       MOVE SUBGLT0-RTCUR   TO SAVE_RTCUR.
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
      MOVE  JHEAD_COUNT    TO JOURNAL-JOURNAL_ID_SEQ_NUM.

   IF BOY_DATE = '00000000'.                                     "I794
      CONCATENATE LAST_DAY+0(4) LAST_DAY+4(2) LAST_DAY+6(2)
                           INTO JOURNAL-JOURNAL_DATE.
   ELSE.                                                         "I794
      MOVE BOY_DATE          TO JOURNAL-JOURNAL_DATE.            "I794
   ENDIF.                                                        "I794
   IF BOY_DATE = '00000000' AND CON_RVRS = 'D'.                  "I794
      CONCATENATE FIRST_DAY+0(4) FIRST_DAY+4(2) FIRST_DAY+6(2)
                           INTO JOURNAL-REVERSAL_DATE.
   ENDIF.                                                        "I794

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

      SELECT SINGLE * FROM T001
       WHERE BUKRS =  SUBGLT0-BUKRS.

   IF BOY_DATE = '00000000'.                                      "I794
      CONCATENATE P_PERD '/' P_YEAR '#Monthend Feed From SAP#'
                  SUBGLT0-BUKRS '-' T001-BUTXT '#TO PeopleSoft#'
                  SAVE_JHDBU '-' SAVE_BUNAM
                  INTO JOURNAL-JRNL_HDR_LONG_DESCR.
   ELSE.                                                           "I794
      CONCATENATE P_PERD '/' P_YEAR '#Begin of year Feed From SAP#' "794
                  SUBGLT0-BUKRS '-' T001-BUTXT '#TO PeopleSoft#'   "I794
                  SAVE_JHDBU '-' SAVE_BUNAM                        "I794
                  INTO JOURNAL-JRNL_HDR_LONG_DESCR.                "I794

   ENDIF.                                                          "I794

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
*      TRANSFER FILE_REC TO P_FILE.

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
*                  JOURNAL-SYSTEM_SOURCE   JOURNAL-JRNL_HDR_LONG_DESCR
                  INTO JRNL_REC SEPARATED BY H_DMTR.
      CONDENSE JRNL_REC NO-GAPS.
      CONCATENATE JRNL_REC LONG_DESCR INTO JRNL_REC
                           SEPARATED BY H_DMTR.
           TRANSLATE JRNL_REC USING TRN_FIELD2.
*           TRANSFER JRNL_REC TO P_FILE.
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
                  SPACE SPACE DETAIL-CF_PROCESS
                  DETAIL-CF_LOCATION        DETAIL-CF_PRODUCT
                  SPACE SPACE               DETAIL-CF_AFFILIATE
                  SPACE SPACE               DETAIL-CURRENCY_CD_LN
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
*          TRANSFER DETL_REC TO P_FILE.
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
 DATA: TOT_AMOUNT LIKE SUBGLT0-AMOUNT.
 SORT SUBGLT0 BY BUKRS RACCT RTCUR PSACT.

 LOOP AT SUBGLT0.
 AT NEW BUKRS.
    SELECT SINGLE * FROM ZJHDR
     WHERE BUKRS = SUBGLT0-BUKRS.
     IF SY-SUBRC = 0.
        WRITE: /1 TEXT-116, ZJHDR-BUNIT.
        WRITE: /1 TEXT-DSH.
     ENDIF.
 ENDAT.
 WRITE: /  SUBGLT0-BUKRS UNDER TEXT-104,  SUBGLT0-RACCT UNDER TEXT-105,
           8(16) SUBGLT0-AMOUNT,          SUBGLT0-PSACT UNDER TEXT-106,
           SUBGLT0-RTYPE UNDER TEXT-107,  SUBGLT0-AFFIL UNDER TEXT-109,
           SUBGLT0-RCTO  UNDER TEXT-110,  SUBGLT0-RCFRM UNDER TEXT-111,
           SUBGLT0-STATUS UNDER TEXT-115.
           TOT_AMOUNT = TOT_AMOUNT + SUBGLT0-AMOUNT.
 IF SUBGLT0-ERROR = 'Y'.
    DELETE SUBGLT0.
 ENDIF.
 AT END OF BUKRS.
    ULINE.
    WRITE: /1 TEXT-120, 7(17) TOT_AMOUNT.
    CLEAR TOT_AMOUNT.
    ULINE.
 ENDAT.
 ENDLOOP.

 SKIP 2.
 LOOP AT NOTFOUND.
      WRITE: /2 NOTFOUND-BUKRS, TEXT-118.
 ENDLOOP.
 ULINE.

ENDFORM.

*----------------------------------------------------------------------*
*                                                                      *
*                      TOP_OF_PAGE                                     *
*                                                                      *
*----------------------------------------------------------------------*

TOP-OF-PAGE.

  WRITE: / TEXT-RPT, SY-REPID, 42 TEXT-100,                 "Title
           90 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
           39 TEXT_COMN, 70 P_PERD, 72 '/', 73 P_YEAR,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  ULINE.
  WRITE: /1 TEXT-104, 8  TEXT-108, 28 TEXT-105, 38 TEXT-106,
         48 TEXT-107, 56 TEXT-109, 68 TEXT-110, 76 TEXT-111,
         86 TEXT-115.
  ULINE.
