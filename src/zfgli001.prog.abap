*REPORT ZFMICR01 MESSAGE-ID ZA                                   "OMNISR
REPORT ZFMICR01 MESSAGE-ID ZF                                    "OMNISR
                NO STANDARD PAGE HEADING
                LINE-COUNT 65 LINE-SIZE 132.
************************************************************************
*                                                                      *
*  Westcoast Energy Inc.          Creation Date: 1993/11/16            *
*                                                                      *
*  Program Name: ZFMICR01         Author: B. Thorp                     *
*                                                                      *
*  Program Description:                                                *
*  This program will download the G/L YTD balances into an ASCII file  *
*  for load into Corporate Accounting's IMRS Micro Control application.*
*                                                                      *
*expand documentation - c11k902243                                     *
*  SAP table ZZF01 is used as a 'translation table' to map Micro       *
*  Control Components/Accounts to SAP Companies/G/L accounts. The      *
*  rows are unique per SAP Company/Account. The data in ZZF01 is       *
*  loaded from Micro Control using program ZFMICR02. The data is       *
*  deleted and the re-inserted for each run of ZFMICR02. If any errors *
*  exist, ZZF01 is not updated.                                        *
*  ZZF01 data is used in ZFMICR01 to build an internal table with a    *
*  single row per Micro Control Component/Account. SAP G/L account     *
*  summary table GLT0 is then used to accumulate YTD totals for each   *
*  internal table row. The resultant data is reported, scaled to       *
*  thousands and then loaded into an ascii file for subsequent         *
*  download to WS/upload to Micro Control.                             *
*                                                                      *
*  Called by: ZXXXXXXX - X-------------------------------------------X *
*             ZXXXXXXX - X-------------------------------------------X *
*                                                                      *
*  Calls:     ZXXXXXXX - X-------------------------------------------X *
*             ZXXXXXXX - X-------------------------------------------X *
*                                                                      *
*  Functions: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                           *
*             XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                           *
*                                                                      *
*  Includes:  ZXXXXXXX - X-------------------------------------------X *
*             ZXXXXXXX - X-------------------------------------------X *
*                                                                      *
************************************************************************

************************************************************************
*                                                                      *
*  Modification History                                                *
*                                                                      *
*  Date        Name                                                    *
*                                                                      *
*  1996/04/11    Al Leonard         ISM-BC                             *
*                SIR/CR #                                              *
*                Back out changes made on Z11K900103 regarding '-' #'s *
*                                                                      *
*  1996/03/12    Steven Falconer    Sierra System Consulting Inc.      *
*                SIR/CR #     Z11K900112                               *
*                Report Format Modifications.                          *
*                                                                      *
*  1996/01/31    Steven Falconer      Sierra Systems Consulting Inc.   *
*                SIR/CR#  Z11K900103                                   *
*                - Removed block checking for Negative values from     *
*                  Form Load Output and put in form 220 WR ASCII DATA  *
*                  block was irregularly changing the values.          *
*                - Added Company totals single and double lines to     *
*                  FORM DO_DETAIL_REPORT.                              *
*                                                                      *
*  1996/01/31    Steven Falconer      Sierra Systems                   *
*                                   CR# - Z11K900037                   *
*              - CHANGE 'scaling to 1000'S IN FORM  300_PRINT_ACCT_TTL *
*                                                                      *
*  1996/01/18  P. Roberts           SIR# F-174 CR# - C11K902848        *
*              - change 'scaling to 1000s' logic                       *
*                                                                      *
*  1995/12/20  P. Roberts           SIR# F-174 CR# - C11K902772        *
*              - add new functionality:                                *
*                - report on G/L accounts (from SKB1) that are not     *
*                  in Microcontrol (ZZF01); only accounts in range     *
*                  '100000' - '399999'                                 *
*                                                                      *
*  1995/11/26  P. Roberts           SIR# F-174 CR# - C11K902685        *
*              - redesign program; originally written as an extract,   *
*                the users now require considerably more functionality *
*              - use extract dataset to allow processing in summary or *
*                detail                                                *
*              - scale report output to 1000s by account               *
*              - scale output file to 1000s and multiply by '% of      *
*                ownership'                                            *
*              - add code to process summary or detail                 *
*                                                                      *
*  1995/10/04  P. Roberts           SIR# F-174 CR# - C11K902282        *
*              - remove (original) code to print 'T_Value' rows.       *
*              - fix scaled output; no decimal places                  *
*                                                                      *
*  1995/08/09  P. Roberts           SIR# N/A   CR# - C11K902283        *
*              - remove (original) code to print ZZF01 rows            *
*                                                                      *
*  1995/08/09  P. Roberts           SIR# F-174 CR# - C11K902282        *
*              - rename ZFMICR01 to ZFMICRSV; rename ZFMICRO1 (old     *
*                version) to ZFMICR01; make changes to ZFMICR01;       *
*                move to production                                    *
*                                                                      *
*  1995/08/02  P. Roberts           SIR# F-174 CR# - C11K902243        *
*            - Add 'Control totals'                                    *
*            - Scale output to thousands; remove as optional           *
*            - Add comparison column - scaled data                     *
*            - Add Grand totals in $ & scaled data                     *
*                                                                      *
*  1995/08/02  P. Roberts           No Sir #   CR# - C11K902243        *
*            - 'Rewrite' ZFMICR01:                                     *
*            - ZFMICR02 was redesigned with improved edits and         *
*              data integrity routines. ZFMICR01 and 02 contained      *
*              duplicate edits, superfluous processing and             *
*              redundant code. ZFMICR01 to be brought 'in line'        *
*              with ZFMICR02 changes.                                  *
*            - Redesign to remove redundant processing, multiple       *
*              internal tables and repetitive internal table reads     *
*            - Replace multiple 'Ifs' with 'Do Varying' to process     *
*              GLT0 $ fields                                           *
*            - Make input Parameters mandatory                         *
*            - Remove hard-coded conditional (If > 2500..) for number  *
*              of records to output                                    *
*            - Improve documentation and general standards             *
*            - Reorganize source code                                  *
*                                                                      *
*  CHANGES MADE IN UNION/CENTRA BIS PROJECT                            *
*  1996/11/20  Selwyn Rodricks - Omnilogic Systems Group (OMNISR)      *
*            - Create table ZF014. Same structure as ZZF01 but to keep *
*              in accordance with BIS naming standards for tables &    *
*              data elements                                           *
*              All occurences of ZZF01 is replaced with ZF014.         *
************************************************************************

TABLES: GLT0,    "G/L Account Master record - monthly debits and credits
        SKB1,    "SAP G/L account Master - by Company (C11K902772)
        T001,    "SAP Company code Master             (C11K902772)
        ZF014.   "Micro Control - SAP G/L account translation table

FIELD-GROUPS: HEADER, DATA.

************************************************************************
* Table T_FILE-BFR will contain the ASCII Micro Control file that is to
* be downloaded to the Novell network.  Note that the first 3 elements
* are the header for the file and thus the table must never be < 4
* entries.
************************************************************************
DATA T_FILE-BFFR-NXT-AVAIL             LIKE SY-TABIX.
DATA: BEGIN OF T_FILE-BFFR             OCCURS 2000,
            TFB-FLD(33)                TYPE C,
      END   OF T_FILE-BFFR.

*table for accumulating MC company data
DATA: BEGIN OF T_VALUE$                OCCURS 2000,
            ACCT                       LIKE ZF014-SAKNR,
            SAP-CO                     LIKE GLT0-BUKRS,
            MC-ACCT-VAL                LIKE GLT0-HSL01,
            MC-ACCT-VAL1000(9)         TYPE I,              "C11K902848
      END   OF T_VALUE$.

*table for accumulating MC account data
DATA: BEGIN OF T_ACCT$                 OCCURS 100,
            ACCT                       LIKE ZF014-ZZMCNO,
            MC-ACCT-$                  LIKE GLT0-HSL01,
            MC-ACCT-$1000(9)           TYPE I,              "C11K902848
      END   OF T_ACCT$.

*Output record - detail
DATA: BEGIN OF W_REC,
      COMPONENT(6),
      COMMA1(1)                        VALUE ',',
      ACCOUNT(6),
      COMMA2(1)                        VALUE ',',
      SIGN(1)                          TYPE C,
      DOL-AMT(15)                      TYPE C,
      DOT(1) ,
END OF W_REC.

*constant value for SAP Chart of Accounts
DATA: C_KTOPL LIKE ZF014-KTOPL
*                  VALUE 'WECA'.                                 "OMNISR
                   VALUE 'ZCON'.                                 "OMNISR

*totalling fields for reconciliation/balancing reports
DATA:
*fields for Micro Control companies
  BEGIN OF TTL,
  GL-ASSET-VAL                  LIKE GLT0-HSL01
                                    VALUE 0,
  GL-BAL-VAL                    LIKE GLT0-HSL01
                                    VALUE 0,
  GL-CLRG-VAL                   LIKE GLT0-HSL01
                                    VALUE 0,
  GL-INC-VAL                    LIKE GLT0-HSL01
                                    VALUE 0,
  GL-LIAB-VAL                   LIKE GLT0-HSL01
                                    VALUE 0,
  GL-ASSET-VAL1000(9)           TYPE I
                                    VALUE 0,
  GL-BAL-VAL1000(9)             TYPE I
                                    VALUE 0,
  GL-CLRG-VAL1000(9)            TYPE I
                                    VALUE 0,
  GL-INC-VAL1000(9)             TYPE I
                                    VALUE 0,
  GL-LIAB-VAL1000(9)            TYPE I
                                    VALUE 0,
  END OF TTL.

*fields for entire report
DATA: BEGIN OF RPT,
  GL-ASSET-VAL                  LIKE GLT0-HSL01
                                    VALUE 0,
  GL-BAL-VAL                    LIKE GLT0-HSL01
                                    VALUE 0,
  GL-CLRG-VAL                   LIKE GLT0-HSL01
                                    VALUE 0,
  GL-INC-VAL                    LIKE GLT0-HSL01
                                    VALUE 0,
  GL-LIAB-VAL                   LIKE GLT0-HSL01
                                    VALUE 0,
  GL-ASSET-VAL1000(9)           TYPE I
                                    VALUE 0,
  GL-BAL-VAL1000(9)             TYPE I
                                    VALUE 0,
  GL-CLRG-VAL1000(9)            TYPE I
                                    VALUE 0,
  GL-INC-VAL1000(9)             TYPE I
                                    VALUE 0,
  GL-LIAB-VAL1000(9)            TYPE I
                                    VALUE 0,
  END OF RPT.

DATA:
  TTL-MC-CO LIKE ZF014-ZZMCCO       VALUE ' ',
  TTL-SAP-CO LIKE GLT0-BUKRS        VALUE ' '.

*global variables
DATA:
  $_FLAG                            TYPE C,
  DS_ACCT-TYP                       TYPE C,
  F-MODE                            TYPE C        "Blank = 'Open'
                                    VALUE ' ',    "A = 'Append'
  FILELENGTH(7)                     TYPE P VALUE 0,
  W_ACCOUNT-VALUE                   LIKE GLT0-HSL01,
  W_SCALED$-VALUE(15)               TYPE I,
  W_CONV-FIELD(30)                  TYPE N,
  W_CTR                             LIKE SY-TABIX,
  W_DATUM1                          LIKE SY-DATUM,"sy-datum - 30 days
  W_DATUM2(8)                       TYPE N,        "yyyymmdd
  W_DOL-AMT(15)                     TYPE N,
  W_INDX                            LIKE SY-TABIX,
  W_TITLES                          TYPE C
                                    VALUE 'E'.

*control totals - c11k902243
DATA:
  W_NMBR-RCDS-IN                       TYPE I VALUE 0,
  W_NMBR-IGN-RCDS                      TYPE I VALUE 0,
  W_NMBR-EDIT-ERRS                     TYPE I VALUE 0,
  W_NMBR-RCDS-LOADED                   TYPE I VALUE 0.

*c11k902243
DATA: BEGIN OF PREV_GLT0,
  BUKRS    LIKE GLT0-BUKRS,
  RACCT    LIKE GLT0-RACCT,
END OF PREV_GLT0.

************************************************************************
PARAMETERS:
************************************************************************
  P_YEAR        LIKE GLT0-RYEAR        DEFAULT   "Fiscal year
     '1993' OBLIGATORY,
  P_PERIOD      LIKE GLT0-RPMAX        DEFAULT   "Period
     '11' OBLIGATORY,
  P_SMMRY                              DEFAULT   "Summary or detail
     'Y'  OBLIGATORY,
*c11k902243  remove as option
* P_SCALE                              DEFAULT   "Scale to thousands?
*    'Y',
  P_SCALE                              DEFAULT   "Scale to thousands?
     'Y'        NO-DISPLAY,
  P_FILE        LIKE RFPDO-RFBIFILE    DEFAULT   "File name to d/l to
*    'S:\MC\WCE\DUMPDC2.???' OBLIGATORY.                         "OMNISR
     'C:\MCA\UNIONGAS.TXT' OBLIGATORY.                           "OMNISR

*c11k902243 to replace multiple 'Betweens' in GLT0 Select
************************************************************************
SELECT-OPTIONS:
************************************************************************
     S_RACCT  FOR GLT0-RACCT NO-DISPLAY.

************************************************************************
INITIALIZATION.
************************************************************************

  REFRESH: T_FILE-BFFR,
           T_VALUE$.

  PERFORM 100_GUESS_PERIOD_PARAM.
*c11k902243 load 'internal' Sel table
  PERFORM 101_LOAD_SEL_TABLE.

************************************************************************
AT SELECTION-SCREEN.
************************************************************************
  PERFORM CHK_P_PERIOD.
  PERFORM CHK_P_SMMRY.

************************************************************************
START-OF-SELECTION.
************************************************************************
  PERFORM SET_SORT_KEYS.
  PERFORM 110_LOAD_MC_DATA.            "get data from ZF014
*c11k902772
  PERFORM 113_CHEK_GL/MCCO.            "check if G/L not in Microcontrol
  PERFORM 115_LOAD_GL_DATA.            "get data from GLT0

************************************************************************
END-OF-SELECTION.
************************************************************************

  IF P_SMMRY = 'Y'.
    PERFORM DO_SUMMARY_REPORT.         "process in 'summary'
  ELSE.
    PERFORM DO_DETAIL_REPORT.          "process in 'detail'
  ENDIF.

  PERFORM 210_INIT_OUTPUT.             "initialize output file
  PERFORM LOAD_OUTFILE.                "create ascii file
  PERFORM 290_PRINT_CTRL_TTL.          "print control totals

*---------------------------------------------------------------------*
*       FORM CHK_P_PERIOD                                             *
*                                                                     *
*    Validation for input period                                      *
*                                                                     *
*---------------------------------------------------------------------*
FORM CHK_P_PERIOD.

* IF P_PERIOD GT 16.                                             "OMNISR
  IF P_PERIOD GT 13.                                             "OMNISR
*   MESSAGE E001 WITH 'Period cannot be greater than 16'.        "OMNISR
    MESSAGE E006 WITH 'Period cannot be greater than 13'.        "OMNISR
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHK_P_SMMRY                                              *
*                                                                     *
*    Validation for input report version parameter                    *
*                                                                     *
*---------------------------------------------------------------------*
FORM CHK_P_SMMRY.

  IF NOT P_SMMRY CO 'YN'.
*   MESSAGE E001 WITH 'Only Y or N may be entered'.              "OMNISR
    MESSAGE E006 WITH 'Only Y or N may be entered'.              "OMNISR
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM SET_SORT_KEYS                                            *
*                                                                      *
*  This form sets the sort keys for the extract dataset.               *
*                                                                      *
*  Called by: Start-of-Selection                                       *
*  Calls:     None                                                     *
*---------------------------------------------------------------------*
FORM SET_SORT_KEYS.

  INSERT:
        ZF014-ZZMCCO
        ZF014-BUKRS
        ZF014-SAKNR
              INTO HEADER,
        ZF014-ZZMCCO
        ZF014-ZZMCNO
        ZF014-BUKRS
        ZF014-SAKNR
        DS_ACCT-TYP
        ZF014-ZZMCSIGN
        ZF014-ZZMCPCT
              INTO DATA.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM 100_GUESS_PERIOD_PARAM                                    *
*----------------------------------------------------------------------*
* Set up the default download date; typically, the first download is   *
*   done on the 6th working day of the month following the month to be *
*   processed; thus subtract (30+6) days from the current date to get  *
*   the period to download.                                            *
*----------------------------------------------------------------------*
*  Called by: Initialization                                           *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM 100_GUESS_PERIOD_PARAM.

  COMPUTE W_DATUM1 = SY-DATUM - 36.
  MOVE: W_DATUM1 TO W_DATUM2,
        W_DATUM2(4)   TO P_YEAR,
        W_DATUM2+4(2) TO P_PERIOD.

ENDFORM.

*c11k902243 load 'internal' Sel table
************************************************************************
*  FORM 101_Load_Sel_Table                                             *
*                                                                      *
*  This form loads Select-Options table S_Racct (for G/L accounts)     *
*  with values for use in Select from GLT0.                            *
*                                                                      *
*  Called by: Initialization                                           *
*  Calls:     None                                                     *
************************************************************************
FORM 101_LOAD_SEL_TABLE.

  MOVE: 'I'          TO S_RACCT-SIGN,
        'BT'         TO S_RACCT-OPTION.

  MOVE: '0000100000' TO S_RACCT-LOW,
        '0000499999' TO S_RACCT-HIGH.
  APPEND S_RACCT.
  MOVE: '0000582015' TO S_RACCT-LOW,
        '0000582016' TO S_RACCT-HIGH.
  APPEND S_RACCT.
  MOVE: '0000590010' TO S_RACCT-LOW,
        '0000590039' TO S_RACCT-HIGH.
  APPEND S_RACCT.
  MOVE: '0000900000' TO S_RACCT-LOW,
        '0000999999' TO S_RACCT-HIGH.
  APPEND S_RACCT.

ENDFORM.

*c11k902243
*----------------------------------------------------------------------*
*       FORM 110_LOAD_MC_DATA                                          *
*                                                                      *
*  This form loads data from Micro Control translation table ZF014     *
*  into an extract dataset.                                            *
*                                                                      *
*----------------------------------------------------------------------*
*  Called by: Start-of-Selection                                       *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM 110_LOAD_MC_DATA.

  SELECT * FROM ZF014
           ORDER BY BUKRS SAKNR.

    ADD 1 TO W_NMBR-RCDS-IN.           "control total

    IF ZF014-SAKNR BETWEEN '000010000' AND '0000200000'.
      DS_ACCT-TYP = 'A'.               "Assets
    ELSEIF ZF014-SAKNR BETWEEN '000020000' AND '0000300000'.
      DS_ACCT-TYP = 'B'.               "Liabilities
    ELSEIF ZF014-SAKNR BETWEEN '000030000' AND '0000500000'.
      DS_ACCT-TYP = 'C'.               "Income
    ELSEIF ZF014-SAKNR BETWEEN '000050000' AND '0000600000'.
      DS_ACCT-TYP = 'C'.               "Income
    ELSEIF ZF014-SAKNR BETWEEN '000090000' AND '0000999999'.
      DS_ACCT-TYP = 'D'.               "Clearing
    ELSE.
      PERFORM 160_WR_UNKNOWN_GL_ACCT_TYPE.
    ENDIF.

    EXTRACT DATA.

  ENDSELECT.

  CHECK W_NMBR-EDIT-ERRS > 0.
  SKIP 2.

ENDFORM.

*c11k902772
*----------------------------------------------------------------------*
*       FORM 113_CHEK_GL/MCCO                                          *
*                                                                      *
*  This form checks for the existence of G/L accounts in SAP that do   *
*  not exist in MicroControl. All exceptions are reported.             *
*                                                                      *
*----------------------------------------------------------------------*
*  Called by: Start-of-Selection                                       *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM 113_CHEK_GL/MCCO.

*counter field
  DATA:
      X_CTR TYPE I.

*get valid company codes
  SELECT * FROM T001
            ORDER BY BUKRS.
*get 'active' G/L accounts for each company
    SELECT * FROM SKB1
            WHERE BUKRS EQ T001-BUKRS
              AND SAKNR BETWEEN '0000100000' AND '0000399999'
              AND XLOEB EQ SPACE       "deletion indicator
             ORDER BY BUKRS SAKNR.
*get corresponding row in MicroControl 'translation' table
      SELECT SINGLE * FROM ZF014 WHERE
                    BUKRS EQ SKB1-BUKRS
                AND KTOPL EQ C_KTOPL   "az ders only vun COA
                AND SAKNR EQ SKB1-SAKNR.
      CHECK SY-SUBRC <> 0.
*if not in Microcontrol.......
      ADD 1 TO X_CTR.
      WRITE: / 'G/L Company ',  SKB1-BUKRS,
              ' account ',      SKB1-SAKNR,
              ' not found in translation table ZF014'.
      ADD 1 TO W_NMBR-EDIT-ERRS.

    ENDSELECT.
  ENDSELECT.

  CHECK X_CTR > 0.
  SKIP 2.

ENDFORM.

*c11k902243
*----------------------------------------------------------------------*
*       FORM 115_LOAD_GL_DATA                                          *
*                                                                      *
*  This form reads data from G/L posting summary table GLT0. Use 'On   *
*  Change Of GLT0-Racct' from ordered GLT0 select to process per       *
*  SAP Company/G/L account.                                            *
*                                                                      *
*----------------------------------------------------------------------*
*  Called by: Start-of-Selection                                       *
*  Calls:     120_Process_Gl_Mstr_Rcd                                  *
*             130_CALC_Gross_Ytd_Amt                                   *
*----------------------------------------------------------------------*
FORM 115_LOAD_GL_DATA.

  SELECT * FROM       GLT0
           WHERE      RLDNR = '00'     " LEDGER 00 ONLY
                 AND  RYEAR = P_YEAR
                 AND  RACCT IN S_RACCT
*                AND  (   RACCT BETWEEN '0000100000' AND '0000499999'
*                      OR RACCT BETWEEN '0000582015' AND '0000582016'
*                      OR RACCT BETWEEN '0000590010' AND '0000590039'
*                      OR RACCT BETWEEN '0000900000' AND '0000999999')
           ORDER BY   BUKRS RACCT.
* 'On Change of' provides the 'break level', but we want to process
* the row prior to the 'change of'.
* set up PREV_GLT0 fields when processing first record.
    IF PREV_GLT0 IS INITIAL.
      PREV_GLT0-BUKRS = GLT0-BUKRS.
      PREV_GLT0-RACCT = GLT0-RACCT.

    ENDIF.
* if record in header has different key than previous record;
* process the previous GLT0 record, reset account total,
* then reset the PREV_GLT0 fields.
    ON CHANGE OF GLT0-RACCT.
      PERFORM 120_PROCESS_GL_MSTR_RCD.
      W_ACCOUNT-VALUE = 0.
      PREV_GLT0-BUKRS = GLT0-BUKRS.
      PREV_GLT0-RACCT = GLT0-RACCT.
    ENDON.

* accumulate the dollar values for the current company code/account
* combination.
    PERFORM 130_CALC_GROSS_YTD_AMT.

  ENDSELECT.

* process the last (if any) records accumulated
  IF SY-SUBRC = 0.
    PERFORM 120_PROCESS_GL_MSTR_RCD.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DO_SUMMARY_REPORT                                         *
*                                                                      *
*  This form processes the extract dataset to produce a summary        *
*  report by MicroControl company.                                     *
*                                                                      *
*----------------------------------------------------------------------*
*  Called by: End-of-Selection                                       *
*  Calls:     Get_Totals                                               *
*             260_Print_Co_Ttl                                         *
*             265_Print_Rp_Ttl                                         *
*---------------------------------------------------------------------*
FORM DO_SUMMARY_REPORT.

  W_TITLES = 'S'.
  NEW-PAGE.
  SORT.
  LOOP.

    CLEAR T_VALUE$.
    MOVE: ZF014-BUKRS TO T_VALUE$-SAP-CO,
          ZF014-SAKNR TO T_VALUE$-ACCT.
    READ TABLE T_VALUE$.

    PERFORM GET_TOTALS.

    AT END OF ZF014-BUKRS.
      PERFORM 260_PRINT_CO_TTL.
      CLEAR TTL.
    ENDAT.

  ENDLOOP.

  PERFORM 265_PRINT_RP_TTL.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DO_DETAIL_REPORT                                         *
*                                                                      *
*  This form processes the extract dataset to produce a detail         *
*  report by MicroControl company/SAP G/L account                      *
*                                                                      *
*----------------------------------------------------------------------*
*  Called by: End-of-Selection                                       *
*  Calls:     Get_Totals                                               *
*             300_Print_Acct_Total                                     *
*             265_Print_Rp_Ttl                                         *
*---------------------------------------------------------------------*
FORM DO_DETAIL_REPORT.

  W_TITLES = 'D'.
  NEW-PAGE.
  SORT.
  LOOP.

    CLEAR T_VALUE$.
    MOVE: ZF014-BUKRS TO T_VALUE$-SAP-CO,
          ZF014-SAKNR TO T_VALUE$-ACCT.
    READ TABLE T_VALUE$.

    PERFORM GET_TOTALS.

    AT END OF ZF014-SAKNR.
      PERFORM 300_PRINT_ACCT_TTL.
    ENDAT.

    AT END OF ZF014-ZZMCCO.
      TTL-GL-BAL-VAL =  ( TTL-GL-ASSET-VAL
                        + TTL-GL-LIAB-VAL
                        + TTL-GL-INC-VAL
                        + TTL-GL-CLRG-VAL ).
      TTL-GL-BAL-VAL1000 =  ( TTL-GL-ASSET-VAL1000
                            + TTL-GL-LIAB-VAL1000
                            + TTL-GL-INC-VAL1000
                            + TTL-GL-CLRG-VAL1000 ).
      SKIP.

      IF $_FLAG = 'X'.
*      WRITE: /46 '---------------------',  Z11K900112
        WRITE: /48 '-------------------',
                71 '----------'.                    "Z11K900103
      ENDIF.
      WRITE: /   'Company', ZF014-ZZMCCO,
*               21 'Balance               ',
              46 TTL-GL-BAL-VAL,
              71 TTL-GL-BAL-VAL1000,
*  /46 '=====================',   "Z11K900103
            /48 '===================',            "Z11K900103
             71 '=========='.                       "Z11K900103
      SKIP.
      CLEAR: TTL,
             $_FLAG.
    ENDAT.

  ENDLOOP.

  PERFORM 265_PRINT_RP_TTL.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM LOAD_OUTFILE                                             *
*                                                                      *
*  This form processes the extract dataset to load an ASCII file       *
*  for download to the workstation.                                    *
*                                                                      *
*----------------------------------------------------------------------*
*  Called by: End-of-Selection                                       *
*  Calls:     220_Wr_Ascii_Data                                        *
*             250_Put_Bffr                                             *
*---------------------------------------------------------------------*
FORM LOAD_OUTFILE.

  SORT.
  LOOP.

    CLEAR T_VALUE$.
    MOVE: ZF014-BUKRS TO T_VALUE$-SAP-CO,
          ZF014-SAKNR TO T_VALUE$-ACCT.
    READ TABLE T_VALUE$.

    IF SY-SUBRC EQ 0.
      MOVE: ZF014-ZZMCNO         TO T_ACCT$-ACCT,
*            T_VALUE$-MC-ACCT-VAL TO T_ACCT$-MC-ACCT-$,
            T_VALUE$-MC-ACCT-VAL1000
                                TO T_ACCT$-MC-ACCT-$1000."c11k902848

      IF ZF014-ZZMCPCT <> 1.
*        MULTIPLY T_ACCT$-MC-ACCT-$ BY ZF014-ZZMCPCT.
        MULTIPLY T_ACCT$-MC-ACCT-$1000 BY ZF014-ZZMCPCT.
      ENDIF.
*Al uncommented 3 of these next 4 lines to back out Z11K900103
     IF ZF014-ZZMCSIGN = '-'.                       " Z11K900103
*        MULTIPLY T_ACCT$-MC-ACCT-$ BY -1.          " Z11K900103
       MULTIPLY T_ACCT$-MC-ACCT-$1000 BY -1.        " Z11K900103
     ENDIF.                                         " Z11K900103
*end of als change
      COLLECT T_ACCT$.

    ENDIF.

    AT END OF ZF014-ZZMCCO.
      SORT T_ACCT$.
      LOOP AT T_ACCT$.
*        IF T_ACCT$-MC-ACCT-$ <> 0.                   "c11k902848
         IF T_ACCT$-MC-ACCT-$1000 <> 0.               "c11k902848
          PERFORM 220_WR_ASCII_DATA.   "create output file
        ENDIF.
      ENDLOOP.
      REFRESH T_ACCT$.
    ENDAT.

  ENDLOOP.

  IF T_FILE-BFFR-NXT-AVAIL > 1.
    PERFORM 250_PUT_BFFR.              "send output file
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM GET_TOTALS                                               *
*                                                                      *
*  This form accumulates totals for both the summary and detail        *
*  reports.                                                            *
*                                                                      *
*----------------------------------------------------------------------*
*  Called by: Do_Summary_Report                                        *
*             Do_Detail_Report                                         *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM GET_TOTALS.

* W_SCALED$-VALUE = T_VALUE$-MC-ACCT-VAL.             "c11k902848
* W_SCALED$-VALUE = ( W_SCALED$-VALUE / 1000 ).       "c11k902848

  CASE DS_ACCT-TYP.
    WHEN 'A'.
      ADD: T_VALUE$-MC-ACCT-VAL TO TTL-GL-ASSET-VAL,
           T_VALUE$-MC-ACCT-VAL TO RPT-GL-ASSET-VAL,
           T_VALUE$-MC-ACCT-VAL1000
                            TO TTL-GL-ASSET-VAL1000,  "c11k902848
           T_VALUE$-MC-ACCT-VAL1000
                            TO RPT-GL-ASSET-VAL1000.  "c11k902848
*          W_SCALED$-VALUE      TO TTL-GL-ASSET-VAL1000,  "c11k902848
*          W_SCALED$-VALUE      TO RPT-GL-ASSET-VAL1000.  "c11k902848
    WHEN 'B'.
      ADD: T_VALUE$-MC-ACCT-VAL TO TTL-GL-LIAB-VAL,
           T_VALUE$-MC-ACCT-VAL TO RPT-GL-LIAB-VAL,
           T_VALUE$-MC-ACCT-VAL1000
                            TO TTL-GL-LIAB-VAL1000,  "c11k902848
           T_VALUE$-MC-ACCT-VAL1000
                            TO RPT-GL-LIAB-VAL1000.  "c11k902848
*          W_SCALED$-VALUE      TO TTL-GL-LIAB-VAL1000,"c11k902848
*          W_SCALED$-VALUE      TO RPT-GL-LIAB-VAL1000."c11k902848
    WHEN 'C'.
      ADD: T_VALUE$-MC-ACCT-VAL TO TTL-GL-INC-VAL,
           T_VALUE$-MC-ACCT-VAL TO RPT-GL-INC-VAL,
           T_VALUE$-MC-ACCT-VAL1000
                            TO TTL-GL-INC-VAL1000,     "c11k902848
           T_VALUE$-MC-ACCT-VAL1000
                            TO RPT-GL-INC-VAL1000.     "c11k902848
*          W_SCALED$-VALUE      TO TTL-GL-INC-VAL1000, "c11k902848
*          W_SCALED$-VALUE      TO RPT-GL-INC-VAL1000. "c11k902848
    WHEN 'D'.
      ADD: T_VALUE$-MC-ACCT-VAL TO TTL-GL-CLRG-VAL,
           T_VALUE$-MC-ACCT-VAL TO RPT-GL-CLRG-VAL,
           T_VALUE$-MC-ACCT-VAL1000
                            TO TTL-GL-CLRG-VAL1000,     "c11k902848
           T_VALUE$-MC-ACCT-VAL1000
                            TO RPT-GL-CLRG-VAL1000.     "c11k902848
*          W_SCALED$-VALUE      TO TTL-GL-CLRG-VAL1000, "c11k902848
*          W_SCALED$-VALUE      TO RPT-GL-CLRG-VAL1000. "c11k902848
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM 120_PROCESS_GL_MSTR_RCD                                   *
*----------------------------------------------------------------------*
* This routine will process all the GLT0 records returned from the     *
*   SELECT.                                                            *
*----------------------------------------------------------------------*
*  Called by: 115_Load_GL_Data                                         *
*  Calls:     140_Chk_Account                                          *
*----------------------------------------------------------------------*
FORM 120_PROCESS_GL_MSTR_RCD.
*
* if the gross amount is $0.00, don't bother processing the record.
*   the first step is to check whether or not this SAP account is in
*   the translation table.
*

  CASE W_ACCOUNT-VALUE.
    WHEN 0.
    WHEN OTHERS.
*c11k902243
      SELECT SINGLE * FROM ZF014
                WHERE KTOPL EQ C_KTOPL
                  AND BUKRS EQ PREV_GLT0-BUKRS
                  AND SAKNR EQ PREV_GLT0-RACCT.

      CASE SY-SUBRC.
        WHEN 0.
          IF ZF014-ZZMCPCT = 0.
            WRITE: / 'G/L company ',   PREV_GLT0-BUKRS,
                    ' account ',      PREV_GLT0-RACCT,
                    ' gross amount ', W_ACCOUNT-VALUE,
                    ' zero percent ownership account'.
          ENDIF.
          PERFORM SCALE_TO_1000S.                           "c11k902848
*c11k902243
          MOVE: PREV_GLT0-BUKRS TO T_VALUE$-SAP-CO,
                PREV_GLT0-RACCT TO T_VALUE$-ACCT,
                W_ACCOUNT-VALUE TO T_VALUE$-MC-ACCT-VAL,
                W_SCALED$-VALUE TO T_VALUE$-MC-ACCT-VAL1000."c11k902848
          APPEND T_VALUE$.
        WHEN OTHERS.
          WRITE: / 'G/L company ',   PREV_GLT0-BUKRS,
                  ' account ',      PREV_GLT0-RACCT,
                  ' gross amount ', W_ACCOUNT-VALUE,
                  ' not found in translation table ZF014'.
          ADD 1 TO W_NMBR-EDIT-ERRS.
      ENDCASE.
  ENDCASE.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM 130_CALC_GROSS_YTD_AMT                                    *
*----------------------------------------------------------------------*
* This routine will get the appropriate period's gross dollar value:   *
*   1. 1st, pick up the carry forward amount                           *
*   2. acummulate monthly values to get YTD value                      *
* At this point, we do NOT know if the account is in the XLT table!    *
*----------------------------------------------------------------------*
*  Called by: 115_Load_MC_Data                                         *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM 130_CALC_GROSS_YTD_AMT.

*C11K902243
  DATA: W_DIFF    TYPE I,
        W_FIELD   LIKE GLT0-HSL01.

  W_DIFF = P_PERIOD.

  ADD  GLT0-HSLVT   TO W_ACCOUNT-VALUE.

  DO W_DIFF TIMES VARYING
     W_FIELD FROM GLT0-HSL01 NEXT GLT0-HSL02.
    ADD W_FIELD TO W_ACCOUNT-VALUE.
  ENDDO.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM 160_WR_UNKNOWN_GL_ACCT_TYPE                               *
*----------------------------------------------------------------------*
* This routine prints an "Unknown account" error message.              *
*                                                                      *
*  Called by: 110_Load_MC_Data                                         *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM 160_WR_UNKNOWN_GL_ACCT_TYPE.

  WRITE: / 'G/L co/acct ', ZF014-BUKRS, ZF014-SAKNR,
           ' not identified as asset, liabibilty, income account or',
           ' clearing account'.

  ADD 1 TO W_NMBR-EDIT-ERRS.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM 210_INIT_OUTPUT                                           *
*----------------------------------------------------------------------*
* This routine will initialize the ouptut buffer (a table) that is     *
*   used to hold the ASCII Micro Control file.                         *
*----------------------------------------------------------------------*
*  Called by: End-of-Selection                                         *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM 210_INIT_OUTPUT.
*
* Place the ASCII file header into the file buffer
*
  IF P_SCALE = 'Y'.
    MOVE 'ACT-TH' TO T_FILE-BFFR-TFB-FLD.
  ELSE.
    MOVE 'ACT-DC' TO T_FILE-BFFR-TFB-FLD.
  ENDIF.
  INSERT T_FILE-BFFR INDEX 1.
  WRITE P_PERIOD TO T_FILE-BFFR-TFB-FLD NO-ZERO.
  INSERT T_FILE-BFFR INDEX 2.
  INSERT T_FILE-BFFR INDEX 3.
  T_FILE-BFFR-NXT-AVAIL = 4.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM 220_WR_ASCII_DATA                                         *
*----------------------------------------------------------------------*
* This routine writes an ASCII file Micro Control detail line.         *
*----------------------------------------------------------------------*
*  Called by: End-of-Selection                                         *
*  Calls:     250_Put_Bffr                                             *
*             260_Print_Co_Ttls                                        *
*----------------------------------------------------------------------*
FORM 220_WR_ASCII_DATA.

* IF T_ACCT$-MC-ACCT-$ <> 0.
     IF T_ACCT$-MC-ACCT-$1000 <> 0.                  "c11k902848
    MOVE: ZF014-ZZMCCO TO W_REC-COMPONENT,
          T_ACCT$-ACCT TO W_REC-ACCOUNT.

*convert type p to type n
    W_CONV-FIELD = T_ACCT$-MC-ACCT-$.
*scale to 1000s
    W_CONV-FIELD = W_CONV-FIELD / 1000.
*convert to type I (for '-' sign)
    W_SCALED$-VALUE = W_CONV-FIELD.
*add sign
*    IF T_ACCT$-MC-ACCT-$ < 0.                      "c11k902848
     IF T_ACCT$-MC-ACCT-$1000 < 0.                  "c11k902848
      W_SCALED$-VALUE = W_SCALED$-VALUE * -1.
    ENDIF.

*this may not be required
*   IF W_SCALED$-VALUE < 0.                         "c11k902848

   IF T_ACCT$-MC-ACCT-$1000 < 0.
     MOVE '-' TO W_REC-SIGN.
    T_ACCT$-MC-ACCT-$1000 = T_ACCT$-MC-ACCT-$1000 * -1.   "Z11K900103
   ELSE.
     MOVE ' ' TO W_REC-SIGN.
   ENDIF.

*   W_REC-DOL-AMT = W_SCALED$-VALUE.                 "c11k902848
    W_REC-DOL-AMT = T_ACCT$-MC-ACCT-$1000.           "c11k902848
    T_FILE-BFFR-TFB-FLD = W_REC.
    INSERT T_FILE-BFFR INDEX T_FILE-BFFR-NXT-AVAIL.
    ADD 1 TO W_NMBR-RCDS-LOADED.
    ADD 1 TO T_FILE-BFFR-NXT-AVAIL.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM 250_PUT_BFFR                                              *
*----------------------------------------------------------------------*
* This routine downloads the ASCII Micro Control file to Novell server *
*----------------------------------------------------------------------*
*  Called by: End-of-Selection                                         *
*             220_Write_Ascii_Data                                     *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM 250_PUT_BFFR.

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME         = P_FILE
            FILETYPE         = 'ASC'
            MODE             = F-MODE
       IMPORTING
            FILELENGTH       = FILELENGTH
       TABLES
            DATA_TAB         = T_FILE-BFFR
       EXCEPTIONS
            FILE_OPEN_ERROR  = 1
            FILE_WRITE_ERROR = 2
            INVALID_TYPE     = 3.

  CASE SY-SUBRC.
    WHEN 0.
    WHEN 1.
      WRITE: / 'Unable to open file', P_FILE.
    WHEN 2.
      WRITE: / 'Unable to write to file', P_FILE.
    WHEN OTHERS.
      WRITE: / 'ABEND IN FILE WS_DOWNLOAD'.
  ENDCASE.

  REFRESH T_FILE-BFFR.
  T_FILE-BFFR-NXT-AVAIL = 1.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM 260_PRINT_CO_TTL                                          *
*----------------------------------------------------------------------*
* This routine prints the control totals by company.                   *
*----------------------------------------------------------------------*
*  Called by: End-of-Selection                                         *
*             220_Write_Ascii_Data                                     *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM 260_PRINT_CO_TTL.

* IF ZF014-ZZMCCO NE ' '.
  TTL-GL-BAL-VAL =  ( TTL-GL-ASSET-VAL + TTL-GL-LIAB-VAL
                   + TTL-GL-INC-VAL   + TTL-GL-CLRG-VAL ).
*test
  TTL-GL-BAL-VAL1000 =  ( TTL-GL-ASSET-VAL1000 + TTL-GL-LIAB-VAL1000
                   + TTL-GL-INC-VAL1000   + TTL-GL-CLRG-VAL1000 ).

  SKIP 2.
  WRITE:
      /   'Company', ZF014-ZZMCCO,
          '/',       ZF014-BUKRS,
      /21 'Assets                ', 46 TTL-GL-ASSET-VAL,
                                    71 TTL-GL-ASSET-VAL1000,
      /21 'Liabilities and Equity', 46 TTL-GL-LIAB-VAL,
                                    71 TTL-GL-LIAB-VAL1000,
      /21 'Net income            ', 46 TTL-GL-INC-VAL,
                                    71 TTL-GL-INC-VAL1000,
      /21 'Clearing accounts     ', 46 TTL-GL-CLRG-VAL,
                                    71 TTL-GL-CLRG-VAL1000,
* /46 '---------------------',      Z11K900112
                                   /48 '-------------------',
                                    71 '----------',
      /21 'Balance               ', 46 TTL-GL-BAL-VAL,
                                    71 TTL-GL-BAL-VAL1000,
* /46 '=====================',      Z11K900112
                                   /48 '===================',
                                    71 '=========='.
* ENDIF.

ENDFORM.

*c11k902243
*----------------------------------------------------------------------*
*       FORM 265_PRINT_RP_TTL                                          *
*----------------------------------------------------------------------*
* This routine prints the control totals for report.                   *
*----------------------------------------------------------------------*
*  Called by: End-of-Selection                                         *
*  Calls:     None                                                     *
*----------------------------------------------------------------------*
FORM 265_PRINT_RP_TTL.

  RPT-GL-BAL-VAL =  ( RPT-GL-ASSET-VAL + RPT-GL-LIAB-VAL
                   + RPT-GL-INC-VAL   + RPT-GL-CLRG-VAL ).
  RPT-GL-BAL-VAL1000 =  ( RPT-GL-ASSET-VAL1000 + RPT-GL-LIAB-VAL1000
                   + RPT-GL-INC-VAL1000   + RPT-GL-CLRG-VAL1000 ).

  NEW-PAGE.
  SKIP 2.
  WRITE:
      /   'Grand total',
      /21 'Assets                ', 46 RPT-GL-ASSET-VAL,
                                    71 RPT-GL-ASSET-VAL1000,
      /21 'Liabilities and Equity', 46 RPT-GL-LIAB-VAL,
                                    71 RPT-GL-LIAB-VAL1000,
      /21 'Net income            ', 46 RPT-GL-INC-VAL,
                                    71 RPT-GL-INC-VAL1000,
      /21 'Clearing accounts     ', 46 RPT-GL-CLRG-VAL,
                                    71 RPT-GL-CLRG-VAL1000,
* /46 '---------------------',         Z11K900112
                                   /48 '-------------------',
*       71 '--------------------',     Z11K900112
                                    71 '----------',
      /21 'Balance               ', 46 RPT-GL-BAL-VAL,
                                    71 RPT-GL-BAL-VAL1000,
* /46 '=====================',         Z11K900112
                                   /48 '===================',
* 71 '====================='.          Z11K900112
                                    71 '=========='.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM 300_PRINT_ACCT_TTL                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM 300_PRINT_ACCT_TTL.

  CHECK T_VALUE$-MC-ACCT-VAL <> 0.
  $_FLAG = 'X'.
  SKIP.
  WRITE:
         /   ZF014-BUKRS+0(2),
             '/',
             ZF014-SAKNR+4(6),
             '/',
             ZF014-ZZMCNO,
          46 T_VALUE$-MC-ACCT-VAL,
          71 T_VALUE$-MC-ACCT-VAL1000.       "z11k900037

ENDFORM.

************************************************************************
*  FORM 290_Print_Ctrl_Ttl                                             *
*                                                                      *
*    This form prints control totals.                                  *
*                                                                      *
*  Called by: End-of-Selection                                         *
*  Calls:     None                                                     *
************************************************************************
FORM 290_PRINT_CTRL_TTL.

  W_TITLES = 'C'.

  NEW-PAGE.
  WRITE:   'Options chosen:',
         / ,
         / 'Fiscal year...........', P_YEAR,
         / 'Period................', P_PERIOD,
         / 'File name to d/l to...', P_FILE.
  SKIP 2.
  WRITE: / 'Control totals:'.
  SKIP.
  WRITE: / '# records read from ZF014........ ', W_NMBR-RCDS-IN,
         / '# records in error............... ', W_NMBR-EDIT-ERRS,
         / '# records loaded to ASCII file... ', W_NMBR-RCDS-LOADED.


ENDFORM.

*c11k902848
*---------------------------------------------------------------------*
*       FORM SCALE_TO_1000S                                           *
*                                                                      *
*    This form scales the G/L data to 1000s.                           *
*                                                                      *
*  Called by: ?????????????????                                        *
*  Calls:     None                                                     *
*---------------------------------------------------------------------*
FORM SCALE_TO_1000S.

  W_SCALED$-VALUE = W_ACCOUNT-VALUE.
  W_SCALED$-VALUE = ( W_SCALED$-VALUE / 1000 ).

ENDFORM.

************************************************************************
TOP-OF-PAGE.
************************************************************************

  NEW-PAGE.
* WRITE: / SY-REPID, 53  'Westcoast Energy Inc.',                "OMNISR
  WRITE: / SY-REPID, 53  'U N I O N / C E N T R A',              "OMNISR
                     110 'Page ', SY-PAGNO,
         / SY-DATUM, SY-UZEIT,
                     42 'Download G/L YTD Balances for Micro Control'.
  SKIP 2.

*C11K902685
  IF W_TITLES = 'S'.

*    WRITE: /21 'Actual G/L balances',          Z11K900112
    WRITE: /48 'Actual G/L balances',
*           70 'Percentage of Ownership Balances', *C11K902685
*          /70 'Scaled to 1000s'.                  *C11K902685
*  70 'G/L balances scaled to 1000s'.           Z11K900112
            71 'G/L balances scaled to 1000s'.
  ELSEIF W_TITLES = 'D'.
* WRITE: /46 'Actual G/L Balances',             Z11K900112
    WRITE: /48 'Actual G/L Balances',
*     70 'G/L balances scaled to 1000s'.        Z11K900112
            71 'G/L balances scaled to 1000s'.
  ELSEIF W_TITLES = 'E'.
    WRITE: /54 'Exceptions'.
  ELSEIF W_TITLES = 'C'.
    WRITE: /54 'Control totals'.
  ENDIF.                               "W_TITLES = ''
  ULINE.
  SKIP.
