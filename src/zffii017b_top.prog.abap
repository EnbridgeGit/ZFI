*&---------------------------------------------------------------------*
*&  Include           ZFFII017B_TOP                                    *
*&---------------------------------------------------------------------*
TABLES: GLT0,    "G/L Account Master record transaction figures
        BSIS,    "Accounting: Secondary Index for G/L Accounts
        LFC1,    "Vendor Master (Transaction Figures)
        T001,    "Company Codes
        ZJHDR,   "Journal Header for Consolidation
        ZPSACCT, "<<<Ins. TR495 (D30K913809)
        FAGLFLEXT,    "G/L Account Master table replacement TR582 (IFRS)
        ZACCTNEW.     "G/L to HFM Account mapping table     TR582 (IFRS)

TYPES: BEGIN OF I_FAGL,
        RLDNR  LIKE FAGLFLEXT-RLDNR,
        RBUKRS LIKE FAGLFLEXT-RBUKRS,
        RACCT  LIKE FAGLFLEXT-RACCT,
        DRCRK  LIKE FAGLFLEXT-DRCRK,
        RTCUR  LIKE FAGLFLEXT-RTCUR,
        HSL01  LIKE FAGLFLEXT-HSL01,
        HSL02  LIKE FAGLFLEXT-HSL02,
        HSL03  LIKE FAGLFLEXT-HSL03,
        HSL04  LIKE FAGLFLEXT-HSL04,
        HSL05  LIKE FAGLFLEXT-HSL05,
        HSL06  LIKE FAGLFLEXT-HSL06,
        HSL07  LIKE FAGLFLEXT-HSL07,
        HSL08  LIKE FAGLFLEXT-HSL08,
        HSL09  LIKE FAGLFLEXT-HSL09,
        HSL10  LIKE FAGLFLEXT-HSL10,
        HSL11  LIKE FAGLFLEXT-HSL11,
        HSL12  LIKE FAGLFLEXT-HSL12,
        HSLVT  LIKE FAGLFLEXT-HSLVT,
       END OF I_FAGL.

*- Global Interal Table for FAGLFLEXT records to remove nested SELECTs
DATA: GTFAGL TYPE STANDARD TABLE OF I_FAGL WITH HEADER LINE,
      GT_NO_PSACCT TYPE STANDARD TABLE OF Z_PSACT WITH HEADER LINE.

*- Detail Record
DATA: BEGIN OF DETAIL OCCURS 0,
        BUSINESS_UNIT_LN(5)         TYPE C,  "Bus. Unit in Peoplesoft
        ACCOUNT(7)                  TYPE C,  "Account
        SUBACCOUNT(5)               TYPE C,  "SubAccount / Affiliate
        MONETARY_AMOUNT(18)         TYPE C,  "YTD Balance
      END OF DETAIL.

DATA: BEGIN OF SUBFAGL OCCURS 0,
            RLDNR   LIKE FAGLFLEXT-RLDNR,     "Ledger Type
            RBUKRS  LIKE FAGLFLEXT-RBUKRS,    "Company code
            RACCT   LIKE FAGLFLEXT-RACCT,     "SAP account #
            HFMACCT LIKE ZACCTNEW-HFMLLACCT,  "HFM account #
            RTCUR   LIKE FAGLFLEXT-RTCUR,     "Currency code
            AMOUNT  LIKE FAGLFLEXT-HSL01,     "Dollars
            BUNIT   LIKE ZJHDR-BUNIT,         "Business Unit
            BUNAM   LIKE ZJHDR-BUNAM,         "Business Unit Name
            AFFIL   LIKE ZACCTNEW-AFFIL,      "Affiliate-Countr Party BU
            STATUS(34) TYPE C,                "Current status
            ERROR      TYPE C,                "Error Record - Print Only
      END   OF SUBFAGL.

DATA: BEGIN OF ITAB OCCURS 0,        "Used for accounts by cost centers
        KOSTL LIKE BSIS-KOSTL,
        DMBTR LIKE BSIS-DMBTR,
      END   OF ITAB.

DATA: BEGIN OF NOTFOUND OCCURS 0,   "Company code not found:table ZJHDR
        BUKRS LIKE FAGLFLEXT-RBUKRS,
      END   OF NOTFOUND.

DATA: W_DOLLARS        LIKE FAGLFLEXT-HSL01,
      W_WAERS          LIKE T001-WAERS,
      NONCC$           LIKE BSIS-DMBTR,
      W_MONTH          LIKE T247-LTX,
*      TEXT_COMN(30)  TYPE C,          "TR654
      TEXT_COMN(48)    TYPE C,           "TR654
      POSTED_IND       TYPE C,  "A/C posted/unposted as vendor or CC A/C
      VENDOR_DEFAULT   TYPE C,
      DEFAULT_VALUES   VALUE 'N',
      LINE_COUNT       TYPE I,
      JHEAD_COUNT      TYPE I,
      FILE_TOT_RECS    TYPE I,
      NEXT_PERIOD(2)   TYPE N,
      NEXT_YEAR(4)     TYPE N,
      FIRST_DAY        LIKE SY-DATUM,
      LAST_DAY         LIKE SY-DATUM,
      START_REC        TYPE I VALUE 1,
      END_REC          TYPE I VALUE 0,
      SAVE_BUNAM       LIKE ZJHDR-BUNAM,
      SAVE_RTCUR       LIKE FAGLFLEXT-RTCUR,
      FIRST_ROW(10)    TYPE C VALUE 'Heade8ftp',
      SECOND_ROW(7)    TYPE C VALUE 'Header',
      THIRD_ROW(7)     TYPE C VALUE 'Header',
      SAVE_BUKRS       LIKE FAGLFLEXT-RBUKRS,
      SAVE_RACCT       LIKE FAGLFLEXT-RACCT,
      COMPANY_MAP(3)   TYPE C,
      W_HFMACCT        LIKE ZACCTNEW-HFMLLACCT,
      W_LEDGER(15)     TYPE C.
*      W_ZACCTNEW_FOUND TYPE C.

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
            TRN_FIELD2(2) TYPE C VALUE '# ',
            GC_PS_DEFAULT LIKE ZPSACCT-PSACT VALUE '0184007',
            GC_ACTUAL(6)  TYPE C VALUE 'ACTUAL',
            GC_IFRS(4)    TYPE C VALUE 'IFRS',           "TR831
            GC_ZEROS(18)  TYPE C VALUE ' 00000000000000000'.

*---------------------------------------------------------------------*
*   S E L E C T I O N - S C R E E N
*---------------------------------------------------------------------*
*- General Data Selection
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001."StrtTR654
 SELECTION-SCREEN BEGIN OF LINE.
   PARAMETERS:     B_OPBAL RADIOBUTTON GROUP REPT.
   SELECTION-SCREEN COMMENT 3(33) TEXT-125.
 SELECTION-SCREEN END OF LINE.

 SELECTION-SCREEN BEGIN OF LINE.
   PARAMETERS:   B_TRBAL RADIOBUTTON GROUP REPT.
   SELECTION-SCREEN COMMENT 3(27) TEXT-126.
 SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.                              "End of TR654
PARAMETERS:
  P_LEDGER LIKE FAGLFLEXT-RLDNR
                DEFAULT '0L' OBLIGATORY,                  "Ledger TR582
  P_YEAR LIKE FAGLFLEXT-RYEAR
                DEFAULT SY-DATUM+0(4) OBLIGATORY,         "Fiscal year
  P_PERD(2) TYPE N DEFAULT SY-DATUM+4(2) OBLIGATORY.      "Period
SELECT-OPTIONS:
  S_BUKRS  FOR FAGLFLEXT-RBUKRS NO INTERVALS,             "CompanyCode
  S_RACCT  FOR FAGLFLEXT-RACCT.                                "Acct #
PARAMETERS:
  P_FILE        LIKE RFPDO-RFBIFILE,                      "File name
  DEF_CFLG(01)  TYPE C  DEFAULT 'Y'.             "Upload/Validate Flag
SELECTION-SCREEN END OF BLOCK B1.                         "TR654

*-      Start of Del. -> (Iss# TR582)
*SELECTION-SCREEN SKIP 1.

*- Constant Data
*SELECTION-SCREEN BEGIN OF BLOCK A3 WITH FRAME TITLE TEXT-003.
*PARAMETERS:
*   CON_RECT(02)  TYPE C  DEFAULT 'JE',      "Record Type
*   CON_TTCD(02)  TYPE C  DEFAULT '04',      "Transmission Traking Code
*   CON_FILE(04)  TYPE C,                    "File Number
*   CON_RVRS(01)  TYPE C  DEFAULT 'D',       "Revere Journal Code
*   CON_SRCE(03)  TYPE C  DEFAULT 'SAP',     "Source
*   CON_OPID(08)  TYPE C  DEFAULT 'SAPFIA',  "Operator ID
*   CON_SSRC(03)  TYPE C  DEFAULT 'EXT',     "System Source
*   CON_LGRP(10)  TYPE C  DEFAULT 'ACTUALS', "Ledger Group
*   CON_JIDM(06)  TYPE C  DEFAULT 'DEGTWT',  "Journal mask identifier
*   CON_EXCH(05)  TYPE C  DEFAULT 'CRRNT'.   "Exchange Rate %
*SELECTION-SCREEN END OF BLOCK A3.
*-      End of Del. -> (Iss# TR582)

*- Default Data
SELECTION-SCREEN BEGIN OF BLOCK A4 WITH FRAME TITLE TEXT-004.
PARAMETERS:
   DEF_ACCT(07)  TYPE C  DEFAULT '0184007'.  "PS Account #
*   DEF_REST(05)  TYPE C  DEFAULT '99810'.   "Resource Type - Del TR582
SELECTION-SCREEN END OF BLOCK A4.

*- Begin of Year
*SELECTION-SCREEN BEGIN OF BLOCK A41 WITH FRAME TITLE TEXT-005. "TR654
PARAMETERS: BOY_DATE TYPE SY-DATUM NO-DISPLAY.                  "TR654
*SELECTION-SCREEN END OF BLOCK A41.                             "TR654

SELECTION-SCREEN BEGIN OF BLOCK A5 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT  1(62) TEXT-123.
         PARAMETERS: ABENDFLG   AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK A5.
SELECTION-SCREEN END OF BLOCK A1.
