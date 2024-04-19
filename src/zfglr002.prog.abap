REPORT ZFGLR002 LINE-SIZE 110 NO STANDARD PAGE HEADING.
* FORMATTED HEADER IN TOP-OF-PAGE SECTION     - NGILLIGAN 98/10
********************************************************************
*      Owner: Centra/Union                                         *
* Programmer: Yaxin Veliz - OmniLogic System Group (Toronto)       *
*       Date: June 20, 1996                                        *
* Request ID: DRFI0030                                             *
*                                                                  *
*  This program will produce a large corporation tax report that   *
*  will display a range of specified accounts (Taxable Capital).   *
*  A sub total will be generated for all the specified accounts    *
*  and a small calculation will display the capital tax for a      *
*  specified period.                                               *
*                                                                  *
*  *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
* 2005/08/26 mkhan TR105 Select Actual amount only as PLAN amount  *
*                     is going to be introduced in FI.             *
*                                                                  *
*   MODIFIED BY NANCY GILLIGAN - OMNILOGIC        D30K906111       *
*   - STANDARDIZED HEADERS, ELIMINATED HARD CODED HEADERS,         *
*     BASED DEFAULT COMPANY CODE ON CLIENT, CLEANED UP TOTALS      *
********************************************************************

TABLES: GLT0,                      "G/L Account Monthly Debits & Credits
        SKB1,                      "G/L Account Master (Company Codes)
        T001.                      "COMPANY CODE       - NGILLIGAN 98/10
DATA:
  TOTAL    TYPE P DECIMALS 2,          "Current/Comparative Month Total
  P_TOTAL  LIKE TOTAL,                 "Previous Month Total
  GC_TOTAL LIKE TOTAL,                 "Grand Total Current/Comparative
  GP_TOTAL LIKE TOTAL,                 "Grand Total Previous
  WC_TOTAL LIKE TOTAL,                 "Westcoast Current Total
  WP_TOTAL LIKE TOTAL,                 "Westcoast Previous Total
  FC_TOTAL LIKE TOTAL,                 "Final Current/Comparative Total
  FP_TOTAL LIKE TOTAL,                 "Final Previous Total
  PREVTAX  LIKE TOTAL,                 "Previous Tax
  COMTAX   LIKE TOTAL,                 "Comparative Tax
  LN_CNTR  TYPE I.                     "Line Counter

* This first block produces a border like frame around the following: *
SELECTION-SCREEN BEGIN OF BLOCK
          INTRO WITH FRAME TITLE TEXT-022.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 15(19) TEXT-021 MODIF ID ABC.
PARAMETERS:
  P_PMONTH(2) DEFAULT 12 MODIF ID ABC,              "Previous Month
  P_PYEAR(4)  MODIF ID ABC.                         "Previous Year
SELECTION-SCREEN: COMMENT 47(12) TEXT-020 MODIF ID ABC.
PARAMETERS:
  P_CMONTH(2) DEFAULT SY-DATUM+4(2) MODIF ID ABC,   "Comparative Month
  P_CYEAR(4)  DEFAULT SY-DATUM(4)   MODIF ID ABC.   "Comparative Year
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS:
  S_SALRAT FOR SKB1-SAKNR,                "S & T Sales Rate Provision
  S_OTLIAB FOR SKB1-SAKNR,                "Other Liabilities
  S_CHEQUE FOR SKB1-SAKNR,                "Unclaimed Cheques
  S_COUPON FOR SKB1-SAKNR,                "Accrued Bond Coupon
  S_ERNRES FOR SKB1-SAKNR,                "Earnings Reserve
  S_CAPSTK FOR SKB1-SAKNR,                "Capital Stock
  S_RTNERN FOR SKB1-SAKNR,                "Retained Earnings
  S_MERCAP FOR SKB1-SAKNR,                "Investment Mercap Insurance
  S_DTAXLG FOR SKB1-SAKNR,                "Deferred Income Taxes - Long
  S_DTAXCU FOR SKB1-SAKNR,                "Deferred Income Taxes - Cur
  S_LGDBT  FOR SKB1-SAKNR,                "Long Term Debt
  S_CURPOR FOR SKB1-SAKNR,                "Current Portion Long Term Dbt
  S_BNKIND FOR SKB1-SAKNR,                "Bank Indebtedness
  S_NPYBLE FOR SKB1-SAKNR,                "Commercial Notes Payable
  S_CUSDEP FOR SKB1-SAKNR,                "Customer Deposits
  S_DPYBLE FOR SKB1-SAKNR,                "Dividends Payable
  S_WCOAST FOR SKB1-SAKNR OBLIGATORY.     "Investment Westcoast Energy

* First block ends after this statement. *
SELECTION-SCREEN END OF BLOCK INTRO.

* This second block produces a border like frame around the following: *
SELECTION-SCREEN BEGIN OF BLOCK
          INPUT WITH FRAME TITLE TEXT-037.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(28) TEXT-027.
PARAMETERS: P_PINVMR(7) TYPE P DECIMALS 2 OBLIGATORY.
SELECTION-SCREEN: COMMENT 48(6) TEXT-031.
PARAMETERS: P_CINVMR LIKE P_PINVMR OBLIGATORY.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(28) TEXT-028.
PARAMETERS: P_PMRGRC(7) TYPE P DECIMALS 2 OBLIGATORY.
SELECTION-SCREEN: COMMENT 48(6) TEXT-031.
PARAMETERS: P_CMRGRC LIKE P_PMRGRC OBLIGATORY.
SELECTION-SCREEN END OF LINE.

* Second block ends after this statement. *

SELECTION-SCREEN END OF BLOCK INPUT.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(75) TEXT-019.
PARAMETERS: P_CAPTAX(5) DEFAULT '.0225' MODIF ID ABC. "Capital Tax Rate
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(75) TEXT-047.
PARAMETERS: P_BUKRS LIKE T001-BUKRS MODIF ID ABC
                                    obligatory.         "NGILLIGAN 98/10
SELECTION-SCREEN END OF LINE.


INITIALIZATION.
  P_PYEAR = SY-DATUM(4) - 1.                  "Sets year-date to minus 1
  MOVE '60' TO LN_CNTR.

* GET COMPANY CODE BASED ON CLIENT                       NGILLIGAN 98/10
IF SY-MANDT+2(1) = '1'.                                 "NGILLIGAN 98/10
   P_BUKRS = 'UEC'.                                     "NGILLIGAN 98/10
ELSEIF SY-MANDT+2(1) = '0'.                             "NGILLIGAN 98/10
   P_BUKRS = 'UGL'.                                     "NGILLIGAN 98/10
ENDIF.                                                  "NGILLIGAN 98/10

* The following will highlight the screen's output for certain texts. *

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.

TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         37 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
         80 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-HED UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 2.                                          "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10
  WRITE: 5 TEXT-040, 28 TEXT-041, 71 TEXT-042,             "ngilligan 98
         96 TEXT-043.                                   "ngilligan 98/10
  WRITE: / TEXT-044 UNDER TEXT-040,                     "ngilligan 98/10
           TEXT-045 UNDER TEXT-041,                     "ngilligan 98/10
           TEXT-046 UNDER TEXT-042,                     "ngilligan 98/10
           TEXT-046 UNDER TEXT-043.                     "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10


***********************BEGINNING OF MAIN PROGRAM************************

START-OF-SELECTION.
* GET COMPANY CODE TEXT
 select single butxt from t001 into t001-butxt where bukrs = P_BUKRS.

 PERFORM SALRAT.
 PERFORM OTLIAB.
 PERFORM CHEQUE.
 PERFORM COUPON.
 PERFORM ERNRES.
 PERFORM CAPSTK.
 PERFORM RTNERN.
 PERFORM MERCAP.
 PERFORM DTAXLG.
 PERFORM DTAXCU.
 PERFORM LGDBT.
 PERFORM CURPOR.
 PERFORM BNKIND.
 PERFORM NPYBLE.
 PERFORM CUSDEP.
 PERFORM DPYBLE.
 PERFORM WRT_SUB.
 PERFORM WCOAST.
 PERFORM CALCULATE.
 PERFORM INVESTLN.
 PERFORM GRP_TOTAL.
 PERFORM WRT_TAX.
END-OF-SELECTION.
***********************BEGINNING OF FORMS*******************************

* Search the database for S & T Rate Provisions info. *
FORM SALRAT.
  IF S_SALRAT-LOW <> ' '.
     perform current  tables s_salrat using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_salrat using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_SALRAT USING TEXT-001.
  ENDIF.
ENDFORM.

* Search the database for Other Liabilities info.  *
FORM OTLIAB.
  IF S_OTLIAB-LOW <> ' '.
     perform current  tables s_otliab using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_otliab using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_OTLIAB USING TEXT-002.
  ENDIF.
ENDFORM.

* Search the database for Unclaimed Cheque info. *
FORM CHEQUE.
  IF S_CHEQUE-LOW <> ' '.
     perform current  tables s_cheque using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_cheque using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_CHEQUE USING TEXT-003.
  ENDIF.
ENDFORM.

* Seach the database for Accrued Bond Coupon info. *
FORM COUPON.
  IF S_COUPON-LOW <> ' '.
     perform current  tables s_coupon using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_coupon using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_COUPON USING TEXT-004.
  ENDIF.
ENDFORM.

* Search the database for Earnings Reserve info. *
FORM ERNRES.
  IF S_ERNRES-LOW <> ' '.
     perform current  tables s_ernres using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_ernres using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_ERNRES USING TEXT-005.
  ENDIF.
ENDFORM.

* Search the database for Capital Stock info. *
FORM CAPSTK.
  IF S_CAPSTK-LOW <> ' '.
     perform current  tables s_capstk using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_capstk using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_CAPSTK USING TEXT-006.
  ENDIF.
ENDFORM.

* Search the database for Retained Earnings info. *
FORM RTNERN.
  IF S_RTNERN-LOW <> ' '.
     perform current  tables s_rtnern using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_rtnern using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_RTNERN USING TEXT-007.
  ENDIF.
ENDFORM.

* Search the databse for Investment Mercap Insurance Ltd. info. *
FORM MERCAP.
  IF S_MERCAP-LOW <> ' '.
     perform current  tables s_mercap using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_mercap using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_MERCAP USING TEXT-008.
  ENDIF.
ENDFORM.

* Search the database for Deferred Income Taxes - Long Term info. *
FORM DTAXLG.
  IF S_DTAXLG-LOW <> ' '.
     perform current  tables s_dtaxlg using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_dtaxlg using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_DTAXLG USING TEXT-009.
  ENDIF.
ENDFORM.

* Serach the database for Deferred Income Taxes - Current info. *
FORM DTAXCU.
  IF S_DTAXCU-LOW <> ' '.
     perform current  tables s_dtaxcu using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_dtaxcu using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_DTAXCU USING TEXT-010.
  ENDIF.
ENDFORM.

* Search the database for Long Term Debt info. *
FORM LGDBT.
  IF S_LGDBT-LOW <> ' '.
     perform current  tables s_lgdbt using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_lgdbt using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_LGDBT USING TEXT-011.
  ENDIF.
ENDFORM.

* Search the database for Current Portion Long Term Debt info. *
FORM CURPOR.
  IF S_CURPOR-LOW <> ' '.
     perform current  tables s_curpor using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_curpor using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_CURPOR USING TEXT-012.
  ENDIF.
ENDFORM.

* Search the database for Bank Indebtedness info. *
FORM BNKIND.
  IF S_BNKIND-LOW <> ' '.
     perform current  tables s_bnkind using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_bnkind using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_BNKIND USING TEXT-013.
  ENDIF.
ENDFORM.

* Search the database for Commercial Notes Payable info. *
FORM NPYBLE.
  IF S_NPYBLE-LOW <> ' '.
     perform current  tables s_npyble using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_npyble using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_NPYBLE USING TEXT-014.
  ENDIF.
ENDFORM.

* Search the database for Customer Deposits info. *
FORM CUSDEP.
  IF S_CUSDEP-LOW <> ' '.
     perform current  tables s_cusdep using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_cusdep using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_CUSDEP USING TEXT-015.
  ENDIF.
ENDFORM.

* Search the database for Dividends Payable info. *
FORM DPYBLE.
  IF S_DPYBLE-LOW <> ' '.
     perform current  tables s_dpyble using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_dpyble using P_BUKRS p_pyear p_pmonth.
     PERFORM WRIT_DET TABLES S_DPYBLE USING TEXT-016.
  ENDIF.
ENDFORM.

* Searches the database for Investment Westcoast Energy info. *
FORM WCOAST.
  PERFORM ZERO_TOT.
  IF S_WCOAST-LOW <> ' '.
     perform current  tables s_wcoast using P_BUKRS p_cyear p_cmonth.
     perform previous tables s_wcoast using P_BUKRS p_pyear p_pmonth.
     PERFORM WCOAST_O TABLES S_WCOAST USING TEXT-018.
  ENDIF.
ENDFORM.

* Routine where specific values are passing to be processed and selected
form current tables in_acct using P_BUKRS p_cyear p_cmonth.
  SELECT * FROM GLT0
   WHERE   RRCTY = '0'                          "TR105
     AND   BUKRS = P_BUKRS
     AND   RYEAR = P_CYEAR
     AND   RACCT IN IN_ACCT.
  PERFORM CUR_BAL USING P_CMONTH CHANGING TOTAL.
  ENDSELECT.
ENDFORM.

* Routine where specific values are passing to be processed and selected
form previous tables in_acct using P_BUKRS p_pyear p_pmonth.
  SELECT * FROM GLT0
   WHERE   RRCTY = '0'                          "TR105
     AND   BUKRS = P_BUKRS
     AND   RYEAR = P_PYEAR
     AND   RACCT IN IN_ACCT.
  PERFORM CUR_BAL USING P_PMONTH CHANGING P_TOTAL.
  ENDSELECT.
ENDFORM.

* This routine writes the specifics of every selected account. *
FORM WRIT_DET TABLES TEMPTAB STRUCTURE S_SALRAT USING TEXT.
SY-TABIX = 1.                               "Counter for LOOP
LOOP AT TEMPTAB.
  IF SY-TABIX = 1.
    PERFORM CAL_TOT.
    IF LN_CNTR >= 59.
      PERFORM CHKLINE.
    ENDIF.
    WRITE: /5 TEMPTAB-LOW, 13 ' ', TEMPTAB-HIGH, 28 TEXT,
           68 P_TOTAL, 92 TOTAL.
    LN_CNTR = LN_CNTR + 1.
    PERFORM ZERO_TOT.
    SY-TABIX = SY-TABIX + 1.
  ELSE.
    PERFORM CAL_TOT.
    IF LN_CNTR >= 59.
      PERFORM CHKLINE.
    ENDIF.
    WRITE: /5 TEMPTAB-LOW, 13 ' ', TEMPTAB-HIGH, 28 TEXT-036.
    LN_CNTR = LN_CNTR + 1.
    SY-TABIX = SY-TABIX + 1.
    PERFORM ZERO_TOT.
  ENDIF.
  SKIP.
  LN_CNTR = LN_CNTR + 1.
ENDLOOP.
ENDFORM.

* This routine accumulates the appropriate month totals for the   *
* selected month.                                                 *
FORM CUR_BAL USING P_CMONTH CHANGING TOTAL.
  IF P_CMONTH = '1' OR P_CMONTH = '01'.
    TOTAL = TOTAL + GLT0-TSL01.
  ELSEIF P_CMONTH = '2' OR P_CMONTH = '02'.
    TOTAL = TOTAL + GLT0-TSL01 + GLT0-TSL02.
  ELSEIF P_CMONTH = '3' OR P_CMONTH = '03'.
    ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL03 TO TOTAL. "NGILLIGAN
   ELSEIF P_CMONTH = '4' OR P_CMONTH = '04'.
     ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL04 TO TOTAL. "NGILLIGA
   ELSEIF P_CMONTH = '5' OR P_CMONTH = '05'.
     ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL05 TO TOTAL. "NGILLIGA
   ELSEIF P_CMONTH = '6' OR P_CMONTH = '06'.
     ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL06 TO TOTAL. "NGILLIGA
   ELSEIF P_CMONTH = '7' OR P_CMONTH = '07'.
     ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL07 TO TOTAL. "NGILLIGA
   ELSEIF P_CMONTH = '8' OR P_CMONTH = '08'.
     ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL08 TO TOTAL. "NGILLIGA
   ELSEIF P_CMONTH = '9' OR P_CMONTH = '09'.
     ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL09 TO TOTAL. "NGILLIGA
   ELSEIF P_CMONTH = '10'.
     ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL10 TO TOTAL. "NGILLIGA
  ELSEIF P_CMONTH = '11'.
    ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL11 TO TOTAL. "NGILLIGAN
  ELSE.
    ADD GLT0-TSL01 THEN GLT0-TSL02 UNTIL GLT0-TSL12 TO TOTAL. "NGILLIGAN
  ENDIF.
ENDFORM.

* This routine accumulates the totals to the grand totals. *
FORM CAL_TOT.
  GP_TOTAL = GP_TOTAL + P_TOTAL.
  GC_TOTAL = GC_TOTAL + TOTAL.
ENDFORM.

* This routine is used to print all the headings at a specific counter *
FORM CHKLINE.
  FORMAT INTENSIFIED ON.
  NEW-PAGE.   " WITH-TITLE. NGILLIGAN 98/10
  PERFORM SUB_HDGS.
  FORMAT INTENSIFIED OFF.
ENDFORM.

* This routine writes a sub-heading which displays two date variables. *
FORM SUB_HDGS.
  MOVE '0' TO LN_CNTR.
  WRITE: /68 '(AS OF', 75 P_PMONTH, 77 '/', 78 P_PYEAR, 82 ')',
          93 '(AS OF', 100 P_CMONTH, 102 '/', 103 P_CYEAR, 107 ')'.
  SKIP 2.
  LN_CNTR = LN_CNTR + 7.
ENDFORM.

* This routine zeros out the totals. *
FORM ZERO_TOT.
  MOVE '0' TO TOTAL.
  MOVE '0' TO P_TOTAL.
ENDFORM.

* This routine writes the sub total with its specific details. *
FORM WRT_SUB.
    IF LN_CNTR >= 45.
      PERFORM CHKLINE.
    ENDIF.
    WRITE: /15 TEXT-017, 68 GP_TOTAL, 92 GC_TOTAL.
    LN_CNTR = LN_CNTR + 2.
ENDFORM.

* This routine writes the specifics of Investment Westcoast Energy. *
FORM WCOAST_O TABLES SUBTAB STRUCTURE S_WCOAST USING TEXT.
  SY-TABIX = 1.
  LOOP AT SUBTAB.
    IF SY-TABIX = 1.
      PERFORM WEST_TOT.
      SKIP.
      WRITE: /5 S_WCOAST-LOW, 13 ' ', S_WCOAST-HIGH, 28 TEXT,
             68 P_TOTAL, 92 TOTAL.
      PERFORM ZERO_TOT.
      LN_CNTR = LN_CNTR + 3.
      SY-TABIX = SY-TABIX + 1.
    ELSE.
      PERFORM WEST_TOT.
      SKIP.
      WRITE: /5 S_WCOAST-LOW, 13 ' ', S_WCOAST-HIGH, 28 TEXT-036.
      PERFORM ZERO_TOT.
      SY-TABIX = SY-TABIX + 1.
    ENDIF.
  ENDLOOP.
ENDFORM.

* This routine accumulates the Totals to the Westcoast Totals. *
FORM WEST_TOT.
  WP_TOTAL = WP_TOTAL + P_TOTAL.
  WC_TOTAL = WC_TOTAL + TOTAL.
ENDFORM.

*This routine is for the calculation of the Investments and Capital Tax.
FORM CALCULATE.
COMPUTE:
  FP_TOTAL = ( GP_TOTAL - WP_TOTAL - P_PINVMR - P_PMRGRC ),
  FC_TOTAL = ( GC_TOTAL - WC_TOTAL - P_CINVMR - P_CMRGRC ),
  PREVTAX  =  P_CAPTAX * FP_TOTAL,
  COMTAX   =  P_CAPTAX * FC_TOTAL.
ENDFORM.

* This routine writes the specifics of Mercap Investment and Mortgage *
* Receivables.                                                        *
FORM INVESTLN.
  SKIP.
  WRITE: /28 TEXT-029, 70 P_PINVMR, 94 P_CINVMR.
  SKIP.
  WRITE: /28 TEXT-030, 70 P_PMRGRC, 94 P_CMRGRC.
  LN_CNTR = LN_CNTR + 4.
  PERFORM UNDERLINE.
ENDFORM.

* This routine writes the Investment Group Totals. *
FORM GRP_TOTAL.
  SKIP.
  WRITE: /15 TEXT-035, 68 FP_TOTAL, 92 FC_TOTAL.
  LN_CNTR = LN_CNTR + 2.
  PERFORM UNDERLINE.
ENDFORM.

* This routine writes the Capital Tax totals. *
FORM WRT_TAX.
  SKIP.
  WRITE: /5 TEXT-019, 20 P_CAPTAX, 64 '$', 68 PREVTAX,
         88 '$', 92 COMTAX.
  LN_CNTR = LN_CNTR + 2.
  PERFORM UNDERLINE.
ENDFORM.

* This routine is self explanatory - it underlines. *
FORM UNDERLINE.
  ULINE /65(19).
  ULINE AT 89(19).
  LN_CNTR = LN_CNTR + 1.
ENDFORM.
