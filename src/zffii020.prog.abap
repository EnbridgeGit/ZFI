REPORT ZFFII020 NO STANDARD PAGE HEADING LINE-COUNT 65
                                         LINE-SIZE 120 MESSAGE-ID ZS.
************************************************************************
*  Client:     Duke Energy.                                            *
*  Date:       February 2005.                                          *
*  Author:     Mohammad Khan                                           *
*  Program Description:                                                *
*  This program will update the field GLACCT of table ZACCTNEW from    *
*  the field ACTDB or ACTCR of table ZCGRPLDG depending upon the       *
*  balance of a group of accounts.                                     *
*  Only those accounts with a company code from ZACCTNEW will be       *
*  updated. Also, accounts with no transactions will not have          *
*  any change, but zero balance accounts will be updated.              *
*  Accounts with no transactions means no data has been selected from  *
*  table GLT0 for the specified criteria.                              *
*  When calculating balance of an account for period 12,               *
*  the balance will be calculated for period 16, not for period 12.    *
*                                                                      *
************************************************************************
*Changes:                                                              *
*                                                                      *
* 2005/08/29 mkhan TR105 Select Actual amount only as PLAN amount      *
*                     is going to be introduced in FI.                 *
* 2009/10/20 gymana  TR582 IFRS Project                                *
*                    Replace ZACCT with ZACCTNEW                       *
* 2010/10/06 gymana  Corrected summarization code to summarize amts    *
*                    after a change in company code, ledger, & acct grp*
*                                                                      *
************************************************************************

TABLES: FAGLFLEXT,  "G/L Totals                                    TR582
        ZACCTNEW,   "Account Detail for Consolidation              TR582
        ZCGRPLDG.   "Account Grouping for Cons by ledger           TR582

DATA: BEGIN OF SUBFAGL OCCURS 0,
            RBUKRS    LIKE FAGLFLEXT-RBUKRS,   "Company code
            ACCTGRP   LIKE ZCGRPLDG-ACCTGRP,   "Account Group      TR582
            LEDGER    LIKE ZCGRPLDG-LEDGER,    "Ledger             TR582
            RACCT     LIKE FAGLFLEXT-RACCT,    "SAP account #
            HFMACCTDB LIKE ZCGRPLDG-HFMACCTDB, "Debit Account      TR582
            HFMACCTCR LIKE ZCGRPLDG-HFMACCTCR, "Credit Account     TR582
            AMOUNT    LIKE FAGLFLEXT-HSL01,    "Dollars            TR582
            DBCRF(2),                          "Debit/Credit Flag
            GRPAMT LIKE FAGLFLEXT-HSL01,       "Group Amount       TR582
      END   OF SUBFAGL.

DATA: BEGIN OF GRPTAB OCCURS 0,
            RBUKRS  LIKE FAGLFLEXT-RBUKRS,   "Company code         TR582
            ACCTGRP LIKE ZCGRPLDG-ACCTGRP,   "Account Group        TR582
            LEDGER  LIKE ZCGRPLDG-LEDGER,    "Ledger               TR582
            AMOUNT  LIKE FAGLFLEXT-HSL01,    "Dollars              TR582
            DBCRF(2),                        "Debit/Credit Flag
      END   OF GRPTAB.

DATA: W_DOLLARS      LIKE GLT0-HSL01.

************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  S_BUKRS  FOR FAGLFLEXT-RBUKRS NO INTERVALS.       "CompanyCode   TR582

PARAMETERS:
*  P_LEDGER LIKE FAGLFLEXT-RLDNR OBLIGATORY,         "Ledger      TR582
  P_YEAR  LIKE FAGLFLEXT-RYEAR DEFAULT SY-DATUM+0(4) OBLIGATORY,
  P_PERD(2) TYPE N DEFAULT SY-DATUM+4(2) OBLIGATORY. "Period
SELECTION-SCREEN END OF BLOCK A1.

************************************************************************
AT SELECTION-SCREEN.
   PERFORM VALID_PERIOD_CHECK.           "check if valid period entered

************************************************************************
START-OF-SELECTION.
  PERFORM GET_GL_DATA.                   "get data from FAGLFLEXT  TR582
  PERFORM UPDATE_ZACCTNEW_TABLE.                                  "TR582
END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM CHK_P_PERIOD                                             *
*                                                                     *
*    Validation for input period                                      *
*                                                                     *
*---------------------------------------------------------------------*
FORM VALID_PERIOD_CHECK.

  IF P_PERD GT 12  OR  P_PERD < 01.
       MESSAGE E019 WITH 'Invalid Period Selected' P_PERD.
       STOP.
  ENDIF.

  IF P_PERD = 12.
     MOVE 16 TO P_PERD.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM GET_GL_DATA                                               *
*                                                                      *
*  This form reads data from FAGLFLEXT depending upon data from        *
*  ZCGRPLDG and does the further processing.                           *
*----------------------------------------------------------------------*
FORM GET_GL_DATA.

SELECT * FROM ZCGRPLDG
 WHERE COCODE IN S_BUKRS.

  SELECT * FROM FAGLFLEXT
   WHERE RRCTY = '0'                   "Actual                     TR105
   AND   RBUKRS = ZCGRPLDG-COCODE      "Company Code               TR582
   AND   RYEAR = P_YEAR                "Year
   AND   RLDNR = ZCGRPLDG-LEDGER       "Ledger                     TR582
   AND   RACCT = ZCGRPLDG-GLACCT.      "G/L Account #              TR582

      MOVE FAGLFLEXT-RBUKRS   TO SUBFAGL-RBUKRS.                  "TR582
      MOVE FAGLFLEXT-RLDNR    TO SUBFAGL-LEDGER.                  "TR582
      MOVE ZCGRPLDG-ACCTGRP   TO SUBFAGL-ACCTGRP.                 "TR582
      MOVE FAGLFLEXT-RACCT    TO SUBFAGL-RACCT.                   "TR582
      MOVE ZCGRPLDG-HFMACCTDB TO SUBFAGL-HFMACCTDB.               "TR582
      MOVE ZCGRPLDG-HFMACCTCR TO SUBFAGL-HFMACCTCR.               "TR582

      CLEAR: W_DOLLARS.
      ADD FAGLFLEXT-HSL01 FROM 1 TO P_PERD GIVING W_DOLLARS.
      W_DOLLARS = W_DOLLARS + FAGLFLEXT-HSLVT.   "Add Bal.Carried fwd
      MOVE W_DOLLARS        TO SUBFAGL-AMOUNT.
      COLLECT SUBFAGL.
      CLEAR   SUBFAGL.
  ENDSELECT.
ENDSELECT.

*DELETE SUBFAGL WHERE AMOUNT = 0.

SORT SUBFAGL BY RBUKRS ACCTGRP LEDGER RACCT.                      "TR582
LOOP AT SUBFAGL.
     MOVE-CORRESPONDING SUBFAGL TO GRPTAB.
     AT END OF LEDGER.                   "End of Acct.Group      "TR582
        SUM.
        MOVE SUBFAGL-AMOUNT TO GRPTAB-AMOUNT.
        IF SUBFAGL-AMOUNT < 0.
           MOVE 'CR' TO GRPTAB-DBCRF.
        ELSE.
           MOVE 'DB' TO GRPTAB-DBCRF.
        ENDIF.
        APPEND GRPTAB.
        CLEAR  GRPTAB.
     ENDAT.
ENDLOOP.

SORT GRPTAB BY RBUKRS ACCTGRP.                                    "TR582
LOOP AT SUBFAGL.
     READ TABLE GRPTAB WITH KEY RBUKRS  = SUBFAGL-RBUKRS
                                ACCTGRP = SUBFAGL-ACCTGRP         "TR582
                                LEDGER  = SUBFAGL-LEDGER.
     IF SY-SUBRC = 0.
        MOVE GRPTAB-DBCRF  TO SUBFAGL-DBCRF.
        MOVE GRPTAB-AMOUNT TO SUBFAGL-GRPAMT.
        MODIFY SUBFAGL.
     ELSE.
      MESSAGE E019 WITH 'ACT GROUP NOT FOUND IN GRPTAB' SUBFAGL-ACCTGRP.
     ENDIF.
ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       UPDATE_ZACCTNEW_TABLE
*
*---------------------------------------------------------------------*

FORM UPDATE_ZACCTNEW_TABLE.                                       "TR582

DATA: W_ACCOUNT LIKE FAGLFLEXT-RACCT.                             "TR582
 LOOP AT SUBFAGL WHERE GRPAMT <> 0.
      IF SUBFAGL-DBCRF = 'DB'.
         MOVE SUBFAGL-HFMACCTDB TO W_ACCOUNT.                     "TR582
      ELSE.
         MOVE SUBFAGL-HFMACCTCR TO W_ACCOUNT.                     "TR582
      ENDIF.

      IF SUBFAGL-LEDGER = '0L'.                                   "TR582
         UPDATE ZACCTNEW SET HFMLLACCT = W_ACCOUNT                "TR582
                WHERE GLACCT  = SUBFAGL-RACCT                     "TR582
                  AND COCODE  = SUBFAGL-RBUKRS.                   "TR582
      ELSE.
         UPDATE ZACCTNEW SET HFMNLACCT = W_ACCOUNT                "TR582
                WHERE GLACCT  = SUBFAGL-RACCT                     "TR582
                  AND COCODE  = SUBFAGL-RBUKRS.                   "TR582
      ENDIF.
 ENDLOOP.
  MESSAGE i019 with TEXT-002.
*  WRITE: /10 TEXT-002.
ENDFORM.
