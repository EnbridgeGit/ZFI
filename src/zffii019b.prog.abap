REPORT ZFFII019 NO STANDARD PAGE HEADING LINE-COUNT 65
                                         LINE-SIZE 131 MESSAGE-ID ZS.
************************************************************************
* Report         : ZFFII019B
* Author         : Glenn Ymana
* Create Date    : 2008/01/23
* Description    : FI/GL: Hyperion Interface - O & M Extract
* Category       : Extract & Report
* Modification#  : Issue TR495
* Supporting Doc : ZFFII019B Development Specification Document.doc
* Spec. Author   : Andy Tattersall
*----------------------------------------------------------------------*
* Change History
*----------------------------------------------------------------------*
* Issue Log  |Date       |Developer       |Description
*----------------------------------------------------------------------
**  1332       3/11/2004   Mohammad Khan    Make reversal date blank if
*                                          reversal code is not "D".
*
*   495       2008/01/23  Glenn Ymana      Changed for Hyperion
*----------------------------------------------------------------------*
*  Description
*  -----------
*  NOTE: This is a copy of ZFFII019 that has been modified for the
*        Hyperion/SAP Interface....see specification for details
*        - Additional Code commented with 'TR495'
*        - Code has been modulized and nested SELECT statements
*          removed, specifically nested GLT0 statements
*
*  Details from ZFFII019
*  ---------------------
*  This program will download O & M Data into a file for
*  upload into PeopleSoft system for consolidation purposes.

*  File Header and Journal Headers will be part of each detail record.
*  A Journal Header will be created for each company code.
*
*  - If the SAP company code doesn't exist in ZJHDR table, don't
*    process any account under this company and create exception record
*    for report printing.
*
*  - Cost center group will be the journal for this program. Each cost
*    center within a CC group will create a separate line in the file
*    and report. For each record created, there will be an alternate
*    record with default values fed through variants, but its' amount
*    will have the opposite sign. For example, if original record has
*    debit amount, then default record will be credit and vice versa.
*
*  - Cost center groups in table ZOMGP must belong to one company.
*
*  - Hard coded delimiter Hex '07' is used to separate the fields.
*
*  - To keep the program consistent between EAST and WEST systems, the
*    program uses SY-HOST field to determine the system. In the
*    INITIALIZATION step, HOST dependent fields are populated.
*    For example: report headings, file path, file number.
*
*  - West system has activity type-related data which is not in use
*    in the east system. Therefore, WEST system will use CSSL table.
*
* Additions for ZFFI017B
* ----------------------
* 1) New Table ZPSACCT - mapping table to assign an Account Validation/
*                        Sign value to the Peoplesoft Account# in ZACCT
*                      - the field D_IC_ACCT will determine what
*                        sign-flipping or calculation rule should be
*                        applied to YTD Account Balance
*
*   ZPSACCT-D_IC_ACCT Values & Operation:
*----------------------------------------------------------------------
*   'A' - Not IC Acct - Sign Flipped - Acct Bal * -1
*
*   'B' - Not IC Acct - Sign Not Flipped - no change
*
*   'C' - Not IC Acct - Affiliate needed Sign Flipped - Acct Bal * -1
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'D' - Not IC Acct - Affiliate needed Sign not Flipped - no change
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'E' - IC Acct     - Not Balancing Sign Flipped - Acct Bal * -1
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'F' - IC Acct     - Not Balancing Sign not Flipped - no change
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'G' - IC Acct     - Sign Flipped - Acct Bal * -1
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'H' - IC Acct     - Sign Not Flipped - no change
*                       Attach Affiliate from ZJHDR/ZACCT tables
*
*   'X' - Exclude from Hyperion Extract - Exclude Amounts from file
*----------------------------------------------------------------------
* 2) Based on the Peoplesoft Account# in table ZPSACCT the code
*    performs the appropriate operation based on the rule assigned
*
* 3) File extract will be changed for Hyperion requirements
*
*    Header Line1 (Chars 1-7 = ACTUALS)
*    Header Line2 (Chars 1-2 = <Beginning Month>)
*    Header Line3 (Chars 1-2 = <Ending Month>)
*
*    Detail Line Format
*    Field 1 = Peoplesoft Business Unit (ZACCT-PSACT)
*    Field 2 = Account . Sub-Account (sub account is affiliate)
*    Field 3 = YTD Account Balance (w/leading sign if negative)
*
*    Note: field delimiters = comma
*
*----------------------------------------------------------------------

TABLES: CSKS,      "Cost Center Master Data
        COSP,      "CO Object: Cost Totals for External Postings
        COSS,      "CO Object: Cost Totals for External Postings
        T001,      "Company Codes
        ZOMGP,     "O & M Group For Consolidation
        ZJHDR,     "Journal Header For Consolidation
        ZPSACCT.   "Acct Validation/Sign Value mapping table

*** Detail Record
DATA: BEGIN OF DETAIL OCCURS 0,
       BUSINESS_UNIT_LN(5)        TYPE C,   "Business Unit in Peoplesoft
       ACCOUNT(7)                 TYPE C,    "Account
       SUBACCOUNT(5)              TYPE C,    "SubAccount
       MONETARY_AMOUNT(18)        TYPE C,    "YTD Balance
      END OF DETAIL.

DATA: GT_NO_PSACCT TYPE STANDARD TABLE OF Z_PSACT WITH HEADER LINE.

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
*            BUNAM  LIKE ZJHDR-BUNAM,         "Business Unit Name
*            DRCRK  LIKE GLT0-DRCRK,          "Debit/Credit Indicator
*            AFFIL  LIKE ZOMGP-AFFIL,         "Affiliate-Countr Party BU
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
             TRN_FIELD2(2) TYPE C VALUE '# ',
             GC_PS_DEFAULT LIKE ZPSACCT-PSACT VALUE '0184007',
             GC_ZEROES(18) TYPE C VALUE ' 00000000000000000'.

RANGES: R_KOSTL  FOR CSKS-KOSTL,          "Cost Centers
        R_KSTAR  FOR COSS-KSTAR.           "Cost Elements

************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-101.
PARAMETERS:
  P_YEAR LIKE COSS-GJAHR DEFAULT SY-DATUM+0(4) OBLIGATORY, "Fiscal year
  P_PERD(2) TYPE N DEFAULT SY-DATUM+4(2) OBLIGATORY.       "Period
*SELECT-OPTIONS:
*  S_BUKRS  FOR CSKS-BUKRS NO INTERVALS.                    "CompanyCode
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
   CON_RVRS(01)  TYPE C  DEFAULT 'D',        "Revere Journal Code
   CON_SRCE(03)  TYPE C  DEFAULT 'SAP',      "Source
   CON_OPID(08)  TYPE C  DEFAULT 'SAPFIA',   "Operator ID
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


*----------------------------------------------------------------------*
*   I N I T I A L I Z A T I O N
*----------------------------------------------------------------------*
INITIALIZATION.

*- Filename Format: SAPEAST.dat
*-                  SAPWEST.dat

*Set Report Heading, file number, file path.
 IF SY-HOST+0(3) = 'WEI'.
    MOVE 'Canada West - O & M Data For'   TO TEXT_COMN.
    MOVE '0006'                           TO CON_FILE.
    MOVE '01'                             TO W_KOKRS.
    MOVE 'WECA'                           TO W_KTOPL.
    CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
                '/IFFI060/SAPWEST.dat' INTO P_FILE.
 ELSE.
    MOVE 'Canada East - O & M Data For'   TO TEXT_COMN.
    MOVE '0003'                           TO CON_FILE.
    MOVE '10'                             TO W_KOKRS.
    MOVE 'COAT'                           TO W_KTOPL.
    CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
                '/IFFI060/SAPEAST.dat' INTO P_FILE.
 ENDIF.

*----------------------------------------------------------------------*
*   A T    S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
   PERFORM VALID_PERIOD_CHECK.

*----------------------------------------------------------------------*
*   S T A R T - O F - S E L E C T I O N
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM SETUP_DATES.
  PERFORM GET_ONM_DATA.
  PERFORM BUILD_EXTRACT.

*----------------------------------------------------------------------*
*   E N D - O F - S E L E C T I O N
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM PRINT_REPORT.
  PERFORM CREATE_OUTPUT_FILE.

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

 clear save_kostl.
 PERFORM GET_DOLLARS.
ENDSELECT.

* Apply PS Account Flipping & Affiliate Rules (ZPSACCT)
PERFORM APPLY_PS_ACCT_RULES.

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
*      Form  BUILD_EXTRACT
*----------------------------------------------------------------------*
FORM BUILD_EXTRACT.

*- Build Extract Records
   LOOP AT ITABONM.
     PERFORM BUILD_DETAIL_RECORD.
   ENDLOOP.

ENDFORM.                    " BUILD_EXTRACT

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
       MOVE ITABONM-PSACT   TO DETAIL-ACCOUNT.
       APPEND: DETAIL.
       CLEAR:  DETAIL.
       MOVE ITABONM-HBUNT   TO SAVE_HBUNT.
       MOVE ITABONM-RTCUR   TO SAVE_RTCUR.
       MOVE ITABONM-BUKRS   TO SAVE_BUKRS.
ENDFORM.

*----------------------------------------------------------------------*
*                 CREATE_OUTPUT_FILE                                   *
*                                                                      *
*----------------------------------------------------------------------*
FORM CREATE_OUTPUT_FILE.
   DATA: MSG(100).            "open file - system message
   DATA: F_LENGTH TYPE I,
         LONG_DESCR TYPE STRING.
   IF NOT DETAIL[] IS INITIAL.
     OPEN DATASET P_FILE FOR APPENDING IN TEXT MODE MESSAGE MSG.
     IF SY-SUBRC NE '0'.
       MESSAGE E002 WITH P_FILE MSG.
       STOP.
     ELSE.
       PERFORM WRITE_DETAIL_LINES.
       CLOSE DATASET P_FILE.
     ENDIF.
   ENDIF.
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

*&---------------------------------------------------------------------*
*&      Form  GET_ZPSACCT_REC
*&---------------------------------------------------------------------*
FORM GET_ZPSACCT_REC USING P_ACCT.

   CLEAR ZPSACCT.
   SELECT SINGLE * FROM ZPSACCT INTO ZPSACCT
                      WHERE PSACT EQ P_ACCT.

   CHECK SY-SUBRC NE 0.

*- Report PeopleSoft Accounts with no mapping in ZPSACCT
   GT_NO_PSACCT = P_ACCT.
   COLLECT GT_NO_PSACCT.

*- When no ZPSACCT record found use PS Acct '0184007'
   SELECT SINGLE * FROM ZPSACCT INTO ZPSACCT
                      WHERE PSACT EQ GC_PS_DEFAULT.

   CHECK SY-SUBRC NE 0.
   GT_NO_PSACCT = GC_PS_DEFAULT.
   COLLECT GT_NO_PSACCT.

ENDFORM.                    " GET_ZPSACCT_REC

*&---------------------------------------------------------------------*
*&      Form  APPLY_YTD_BALANCE_RULE
*&---------------------------------------------------------------------*
FORM APPLY_YTD_BALANCE_RULE CHANGING P_DOLLARS.

*- Based on the entry in ZPSACCT for Peoplesoft Account#, perform
*- calculation on YTD Balance based on D_IC_ACCT to Flip the sign or not

   CHECK NOT ( P_DOLLARS IS INITIAL ).

   CASE ZPSACCT-D_IC_ACCT.

*---FLIP YTD Balace
*   'A' - Not IC Acct - Sign Flipped - Acct Bal * -1
*   'C' - Not IC Acct - Affiliate needed Sign Flipped - Acct Bal * -1
*   'E' - IC Acct     - Not Balancing - Sign Flipped - Acct Bal * -1
*   'G' - IC Acct     - Sign Flipped - Acct Bal * -1
    WHEN 'A' OR 'C' OR 'E' OR 'G'.
     P_DOLLARS = P_DOLLARS * -1.

*---LEAVE YTD Balace "as is"
*   'B' - Not IC Acct - Sign Not Flipped - no change
*   'D' - Not IC Acct - Affiliate needed Sign not Flipped - no change
*   'F' - IC Acct     - Not Balancing Sign not Flipped - no change
*   'H' - IC Acct     - Sign Not Flipped - no change
    WHEN 'B' OR 'D' OR 'F' OR 'H'.
*-     no action

*---SET YTD Balance to Zero as all records with zero balance are not
*---extracted
*   'X' - Exclude from Hyperion Extract - Exclude Amounts from file
    WHEN 'X'.
       P_DOLLARS = 0.

   ENDCASE.

ENDFORM.                    " APPLY_YTD_BALANCE_RULE

*&---------------------------------------------------------------------*
*&      Form  WRITE_DETAIL_LINES
*&---------------------------------------------------------------------*
FORM WRITE_DETAIL_LINES.

DATA: L_ACCT_OUT(13) TYPE C,
      L_REC(100) TYPE C,
      L_AMT(18) TYPE C.

   LOOP AT DETAIL.

*-    Move YTD Balance to sign
      PERFORM PUT_SIGN_IN_FRONT CHANGING DETAIL-MONETARY_AMOUNT.

*-    Format the YTD Balance -00000000000000.00
      PERFORM FORMAT_AMOUNT USING DETAIL-MONETARY_AMOUNT
                         CHANGING L_AMT.

*-    Format Account.SubAccount field
      IF ( DETAIL-SUBACCOUNT IS INITIAL ).
         L_ACCT_OUT = DETAIL-ACCOUNT.
      ELSE.
         CONCATENATE DETAIL-ACCOUNT '.' DETAIL-SUBACCOUNT INTO
L_ACCT_OUT.
      ENDIF.

      CLEAR L_REC.
      L_REC+0(5) = DETAIL-BUSINESS_UNIT_LN.
      L_REC+5(1) = ','.
      L_REC+6(13) = L_ACCT_OUT.
      L_REC+19(1) = ','.
      L_REC+20(19) = L_AMT.

*-    Write Detail record to file
      TRANSFER L_REC TO P_FILE.
   ENDLOOP.

ENDFORM.                    " WRITE_DETAIL_LINES

*&---------------------------------------------------------------------*
*&      Form  FORMAT_AMOUNT
*&---------------------------------------------------------------------*
FORM FORMAT_AMOUNT USING    P_AMT
                   CHANGING P_AMT_FORMATTED.

DATA: L_AMT(18) TYPE C.

    L_AMT = P_AMT.

    IF L_AMT CS '-'.
       REPLACE '-' WITH SPACE INTO L_AMT.
    ENDIF.

    SHIFT L_AMT RIGHT DELETING TRAILING SPACE.
    OVERLAY L_AMT WITH GC_ZEROES.

    IF P_AMT CS '-'.
       L_AMT+0(1) = '-'.
    ENDIF.

    P_AMT_FORMATTED = L_AMT.

ENDFORM.                    " FORMAT_AMOUNT

*&---------------------------------------------------------------------*
*&      Form  APPLY_PS_ACCT_RULES
*&---------------------------------------------------------------------*
FORM APPLY_PS_ACCT_RULES.

*    Testing/debugging only - write out ITABONM & ZPSACCT
*  OPEN DATASET P_OUT FOR OUTPUT IN TEXT MODE.

  LOOP AT ITABONM.

*-  Get the ZPSACCT record to determine PS rule based on PS Account
    PERFORM GET_ZPSACCT_REC USING ITABONM-PSACT.
*    TRANSFER ITABONM TO P_OUT.
*    TRANSFER ZPSACCT TO P_OUT.
*-  Perform rule calculation on YTD Balance
    PERFORM APPLY_YTD_BALANCE_RULE CHANGING ITABONM-AMOUNT.

    IF ITABONM-AMOUNT EQ 0.
       DELETE ITABONM.
    ENDIF.

  ENDLOOP.

*  CLOSE DATASET P_OUT.
ENDFORM.                    " APPLY_PS_ACCT_RULES
