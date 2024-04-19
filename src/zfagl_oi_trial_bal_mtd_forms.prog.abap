*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_MTD_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DATA  text
*----------------------------------------------------------------------*
FORM BUILD_HEADER  USING  PS_DATA TYPE ANY
                          PT_DATA TYPE STANDARD TABLE.

*  CONCATENATE   'LEDGER_NAME'
*                'TRANSACTION_NUMBER'
*                'REPORTING_PERIOD'
*                'EFFECTIVE_DATE'
*                'CURRENCY'
*                'COMPANY_CODE'
*                'ACCOUNT'
*                'TRADING_PARTNER'
*                'COST_CENTER'
*                'CASH_FLOW_ATTRIBUTE'
*                'STATISTICAL_INTERNAL_ORDER'
*                'SETTLEMENT_PERIOD'
*                'WORK_BREAKDOWN_STRUCTURE'
*                'LOCATION'
*                'PROFIT_CENTER'
*                'AMOUNT'     "16
*                'H_VARIABLE_1'
*                'H_VARIABLE_2'
*                'H_VARIABLE_3'
*                'L_VARIABLE_1'
*                'L_VARIABLE_2'
*                'L_VARIABLE_3'  "22
*                INTO ps_data SEPARATED BY ','.

  CONCATENATE   'LEDGER_NAME'
              'TRANSACTION_NUMBER'
              'REPORTING_PERIOD'
              'EFFECTIVE_DATE'
              'CURRENCY'
              'COMPANY_CODE'
              'ACCOUNT'
              'COST_CENTER'
              'CO_ORDER'
              'FINANCIAL_ASSET'
              'WORK_BREAKDOWN_STRUCTURE'
              'AMOUNT'        "12
              'H_VARIABLE_1'
              'H_VARIABLE_2'
              'H_VARIABLE_3'
              'L_VARIABLE_1'
              'L_VARIABLE_2'
              'L_VARIABLE_3'  "18
              INTO PS_DATA SEPARATED BY ','.

  INSERT PS_DATA INTO PT_DATA INDEX 1.

ENDFORM.                    " BUILD_HEADER
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN .

  LOOP AT SCREEN.
    IF RD_PC EQ 'X'.
      IF SCREEN-GROUP1 EQ 'RD4' OR
         SCREEN-GROUP1 EQ 'RD6'.
        SCREEN-INPUT     = 1.       "Visible
        SCREEN-INVISIBLE = 0.
        MODIFY SCREEN.
      ENDIF.
      IF SCREEN-GROUP1 EQ 'RD7'. "Server File Path
        SCREEN-INPUT     = 0.       "Hidden
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
      CLEAR P_MAN.
    ELSE. "Server File
      IF SCREEN-GROUP1 EQ 'RD5' OR
         SCREEN-GROUP1 EQ 'RD7'.
        SCREEN-INPUT     = 1.       "Visible
        SCREEN-INVISIBLE = 0.
        MODIFY SCREEN.
      ENDIF.
      IF SCREEN-GROUP1 EQ 'RD6'. "PC File Path
        SCREEN-INPUT     = 0.       "Hidden
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FILENAME .

  DATA: LV_FILENAME TYPE LOCALFILE,
        LV_PARAM_1  TYPE TEXT15,
        LV_PARAM_3  TYPE TEXT15.

  IF SY-SYSID EQ 'RDC'.
    LV_PARAM_1 = 'DEV'.
    CONCATENATE 'US_' SY-SYSID INTO LV_PARAM_3.
  ELSEIF SY-SYSID EQ 'RQC'.
    LV_PARAM_1 = 'QA'.
    CONCATENATE 'US_PR_QA_' SY-SYSID INTO LV_PARAM_3.
  ELSEIF SY-SYSID EQ 'PEC'.
    LV_PARAM_1 = 'PRD'.
    CONCATENATE 'US_PR_PRD_' SY-SYSID INTO LV_PARAM_3.
  ELSEIF SY-SYSID EQ 'D30'.
    LV_PARAM_1 = 'DEV'.
    CONCATENATE 'US_' SY-SYSID INTO LV_PARAM_3.
  ELSEIF SY-SYSID EQ 'D22'.
    LV_PARAM_1 = 'QA'.
    CONCATENATE 'US_PR_QA_' SY-SYSID INTO LV_PARAM_3.
  ELSEIF SY-SYSID EQ 'S02'.
    LV_PARAM_1 = 'DEV'.
    CONCATENATE 'UG_' SY-SYSID INTO LV_PARAM_3.
  ELSEIF SY-SYSID EQ 'Q02'.
    LV_PARAM_1 = 'QA'.
    CONCATENATE 'UG_PR_QA_' SY-SYSID INTO LV_PARAM_3.
  ELSEIF SY-SYSID EQ 'P01'.
    LV_PARAM_1 = 'PRD'.
    CONCATENATE 'UG_PR_PRD_' SY-SYSID INTO LV_PARAM_3.
  ENDIF.

* Lookup logical file path
  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      LOGICAL_FILENAME = 'ZFI_READ_GL_FOR_IS'
      PARAMETER_1      = LV_PARAM_1
      PARAMETER_2      = LV_PARAM_1
      PARAMETER_3      = LV_PARAM_3
    IMPORTING
      FILE_NAME        = P_APPF
    EXCEPTIONS
      FILE_NOT_FOUND   = 1
      OTHERS           = 2.

ENDFORM.                    " GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_FILE .

  DATA: LV_FILE       TYPE STRING,
        LV_APPFILE    TYPE LOCALFILE,
        LV_ERROR(75)  TYPE C.

  FIELD-SYMBOLS: <FILE> LIKE LINE OF GT_FILE.

  IF RD_PC EQ 'X'. " PC file
    IF GT_FILE[] IS NOT INITIAL.
      LV_FILE = P_FILE.
      REPLACE 'WAERS' IN LV_FILE WITH 'CAD'.
      PERFORM WRITE_PC_FILE USING GT_FILE[] LV_FILE.
    ENDIF.

    IF GT_USFILE[] IS NOT INITIAL.
      LV_FILE = P_FILE.
      REPLACE 'WAERS' IN LV_FILE WITH 'USD'.
      PERFORM WRITE_PC_FILE USING GT_USFILE[] LV_FILE.
    ENDIF.
  ELSE.

    IF GT_FILE[] IS NOT INITIAL.
      LV_APPFILE = P_APPF.
      REPLACE 'WAERS' IN LV_APPFILE WITH 'CAD'.
      OPEN DATASET LV_APPFILE FOR OUTPUT MESSAGE LV_ERROR IN TEXT MODE ENCODING DEFAULT.
      IF SY-SUBRC EQ 0.
        LOOP AT GT_FILE ASSIGNING <FILE>.
          TRANSFER <FILE> TO LV_APPFILE.
        ENDLOOP.
      ELSE.
        WRITE: / LV_ERROR , ' - ',  LV_APPFILE.
        EXIT.
      ENDIF.
    ENDIF.

    IF GT_USFILE[] IS NOT INITIAL.
      LV_APPFILE = P_APPF.
      REPLACE 'WAERS' IN LV_APPFILE WITH 'USD'.
      OPEN DATASET LV_APPFILE FOR OUTPUT MESSAGE LV_ERROR IN TEXT MODE ENCODING DEFAULT.
      IF SY-SUBRC EQ 0.
        LOOP AT GT_USFILE ASSIGNING <FILE>.
          TRANSFER <FILE> TO LV_APPFILE.
        ENDLOOP.
      ELSE.
        WRITE: / LV_ERROR , ' - ',  LV_APPFILE.
        EXIT.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " WRITE_FILE
*&---------------------------------------------------------------------*
*&      Form  WRITE_PC_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FILE[]  text
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM WRITE_PC_FILE  USING PT_DATA TYPE STANDARD TABLE
                             P_FILE TYPE ANY.

  DATA: LV_FILE TYPE STRING.

  LV_FILE = P_FILE.

* Write file data
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = LV_FILE
      FILETYPE                = 'ASC'
    TABLES
      DATA_TAB                = PT_DATA
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.

ENDFORM.                    " WRITE_PC_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LAST_DAYS .

  DATA: LV_DATE     TYPE DATUM,
        LV_LAST_DAY TYPE DATUM,
        LS_LASTDAY  LIKE LINE OF GT_LAST_DAYS.

  REFRESH: GT_LAST_DAYS.

  DO 12 TIMES.
    IF LV_DATE IS INITIAL.
      CONCATENATE P_RYEAR '0101' INTO LV_DATE.
    ENDIF.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = LV_DATE
      IMPORTING
        LAST_DAY_OF_MONTH = LV_LAST_DAY
      EXCEPTIONS
        DAY_IN_NO_DATE    = 1
        OTHERS            = 2.

    IF SY-SUBRC EQ 0.
      LS_LASTDAY-YEAR  = LV_LAST_DAY(4).
      LS_LASTDAY-MONTH = LV_LAST_DAY+4(2).
      LS_LASTDAY-DAY   = LV_LAST_DAY+6(2).
      APPEND LS_LASTDAY TO GT_LAST_DAYS.
    ENDIF.

    LV_DATE = LV_DATE + 31.
  ENDDO.

  SORT GT_LAST_DAYS.

ENDFORM.                    " GET_LAST_DAYS

*&---------------------------------------------------------------------*
*&      Form  OVERLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FAGLFLEXT>_RASSC  text
*      -->P_0229   text
*      <--P_LV_STRING  text
*----------------------------------------------------------------------*
FORM OVERLAY  USING    P_VAL TYPE ANY
                       P_OVERLAY TYPE ANY
              CHANGING P_DATA TYPE LINE OF TRUXS_T_TEXT_DATA .
  DATA: LV_VAL TYPE STRING.

  LV_VAL = P_VAL.
  OVERLAY LV_VAL WITH P_OVERLAY.
  CONCATENATE P_DATA LV_VAL INTO P_DATA SEPARATED BY ','.

ENDFORM.                    " OVERLAY
*&---------------------------------------------------------------------*
*&      Form  PROCESS_US_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FAGLFLEXT>  text
*      <--P_LS_DATA  text
*----------------------------------------------------------------------*
FORM PROCESS_US_FILE  USING    PS_FAGLFLEXT TYPE FAGLFLEXT
                      CHANGING PS_DATA      TYPE ANY.

ENDFORM.                    " PROCESS_US_FILE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_UG_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FAGLFLEXT>  text
*      -->P_LV_PERIOD  text
*      -->P_LV_AMT  text
*      <--P_LS_DATA  text
*----------------------------------------------------------------------*
FORM PROCESS_UG_FILE  USING    PS_FAGLFLEXT TYPE TY_MTD
                                P_PERIOD     TYPE ANY
                                P_AMT        TYPE ANY
                       CHANGING PS_DATA      TYPE ANY.

  IF PS_FAGLFLEXT-RCNTR IS NOT INITIAL AND PS_FAGLFLEXT-RCNTR NE ''.       " Cost center
    SHIFT PS_FAGLFLEXT-RCNTR LEFT DELETING LEADING '0'.
    CONCATENATE PS_DATA PS_FAGLFLEXT-RCNTR INTO PS_DATA SEPARATED BY ','.
  ELSE.
    CONCATENATE PS_DATA '0000000000' INTO PS_DATA SEPARATED BY ','.
  ENDIF.

  CONCATENATE PS_DATA '000000'          INTO PS_DATA SEPARATED BY ','.      " CO_ORDER
  CONCATENATE PS_DATA '000000000000000' INTO PS_DATA SEPARATED BY ','.      " Financial Asset
  CONCATENATE PS_DATA '000000000000000' INTO PS_DATA SEPARATED BY ','.      " WBS
  CONCATENATE PS_DATA
              P_AMT                                                         " Amount
              SPACE
              SPACE
              SPACE
              SPACE
              SPACE
              SPACE
              INTO PS_DATA SEPARATED BY ','.         " Profit Center
ENDFORM.                    " PROCESS_UG_FILE
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFAULT_VALUES .

  S_USCO-LOW    = 'DGLC'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'EMP'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'ESPC'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'EUSA'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'MLG'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'MPI'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'SCPC'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'TGC'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'TGS'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'TUSA'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  S_USCO-LOW    = 'UGSL'.
  S_USCO-OPTION = 'EQ'.
  S_USCO-SIGN   = 'I'.
  APPEND S_USCO.

  CONCATENATE 'H:\MDM_Data_Profiling_Samples\SAP_UG_WAERS_TB_GL_'
              SY-DATUM '_' SY-UZEIT '.csv' INTO P_FILE.

ENDFORM.                    " DEFAULT_VALUES
*&---------------------------------------------------------------------*
*&      Form  PROCESS_MTD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_MTD .

  DATA: LV_HSL(5)    TYPE C,
        FIRST_REC    TYPE C,
        LV_TABIX(2)  TYPE N,
        LT_MONTHS    TYPE STANDARD TABLE OF T247,
        LV_PERIOD(8) TYPE C,
        LV_MONTH     TYPE TEXT20,
        LV_EFFDATE   TYPE CHAR10,
        TOT_VAL      TYPE HSLVT12,
        LS_FILE_DATA TYPE LINE OF TRUXS_T_TEXT_DATA,
        LV_AMT       TYPE TEXT30,
        LV_TRANSAC   TYPE TEXT40,
        LV_STR       TYPE STRING,
        LV_PRCTR     TYPE STRING,
        LV_CURRENCY  TYPE WAERS,
        LV_SKIP      TYPE C,
        LV_ACCOUNT   TYPE RACCT,
        LT_T001      TYPE STANDARD TABLE OF T001.

  FIELD-SYMBOLS: <FAGLFLEXT> LIKE LINE OF GT_FAGLFLEXT,
                 <LAST_DAY>  LIKE LINE OF GT_LAST_DAYS,
                 <DATA>      LIKE LINE OF GT_DATA,
                 <T001>      LIKE LINE OF GT_T001,
                 <VAL>       TYPE ANY,
                 <MONTH>     LIKE LINE OF LT_MONTHS.

  REFRESH: GT_FAGLFLEXT.

  PERFORM CALC_DRCR_DIFFERENCE.

  LOOP AT GT_FAGLFLEXT ASSIGNING <FAGLFLEXT>.
    CONCATENATE 'HSL' P_PERIOD INTO LV_HSL.
    ASSIGN COMPONENT LV_HSL OF STRUCTURE <FAGLFLEXT> TO <VAL>.
    IF <VAL> IS ASSIGNED.
      CHECK <VAL> IS NOT INITIAL.
      IF FIRST_REC EQ ''. "Only execute once for the first record
        FIRST_REC = 'X'.
        CONCATENATE P_PERIOD '.' P_RYEAR INTO LV_PERIOD.
        SELECT BUKRS WAERS INTO TABLE GT_T001 FROM T001.
        SORT GT_T001.
        PERFORM GET_LAST_DAYS.
      ENDIF.
    ENDIF.

    CLEAR: LS_FILE_DATA, LV_AMT.
    READ TABLE GT_LAST_DAYS ASSIGNING <LAST_DAY>
                            WITH KEY MONTH = P_PERIOD BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      CONCATENATE P_RYEAR '-' <LAST_DAY>-MONTH '-' <LAST_DAY>-DAY INTO LV_EFFDATE.
    ELSE.
      CONCATENATE P_RYEAR '-12-31' INTO LV_EFFDATE.
    ENDIF.

    READ TABLE GT_T001 ASSIGNING <T001>
                       WITH KEY BUKRS = <FAGLFLEXT>-RBUKRS
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LV_CURRENCY = <T001>-WAERS.
    ENDIF.

    IF <VAL> > 0.
      LV_AMT = <VAL>.
      SHIFT LV_AMT LEFT DELETING LEADING SPACE.
    ELSE.
      <VAL>  = <VAL> * -1.
      LV_AMT = <VAL>.
      SHIFT LV_AMT LEFT DELETING LEADING SPACE.
      CONCATENATE '-' LV_AMT INTO LV_AMT.
    ENDIF.

* Delimit record by comma
*    CONCATENATE LS_CUR_DATA-LEDGER_NAME
*                LS_CUR_DATA-TRANSACTION_NUMBER
*                LS_CUR_DATA-REPORTING_PERIOD
*                LV_EFFDATE
*                LS_CUR_DATA-CURRENCY             " Currency
*                LS_CUR_DATA-COMPANY_CODE         " Company code
*                LS_CUR_DATA-ACCOUNT  " Cloumn 7  " Account number
*                INTO LS_FILE_DATA SEPARATED BY ','.

    LV_ACCOUNT = <FAGLFLEXT>-RACCT.
    SHIFT LV_ACCOUNT LEFT DELETING LEADING '0'.
    CONCATENATE 'SAP UG GAAP Ledger' ',' INTO LS_FILE_DATA.
    CONCATENATE LS_FILE_DATA 'SAP_UG_AB_' <FAGLFLEXT>-RBUKRS '_' SY-DATUM '_'
                SY-UZEIT INTO LS_FILE_DATA.
    CONCATENATE LS_FILE_DATA LV_PERIOD LV_EFFDATE LV_CURRENCY
                <FAGLFLEXT>-RBUKRS LV_ACCOUNT
                INTO LS_FILE_DATA SEPARATED BY ','.

    PERFORM PROCESS_UG_FILE USING <FAGLFLEXT> LV_PERIOD LV_AMT CHANGING LS_FILE_DATA.

    IF <FAGLFLEXT>-RBUKRS IN S_USCO.
      APPEND LS_FILE_DATA TO GT_USFILE.
    ELSE.
      APPEND LS_FILE_DATA TO GT_FILE.
    ENDIF.
  ENDLOOP.

  IF GT_FILE[] IS NOT INITIAL.
    PERFORM BUILD_HEADER USING LS_FILE_DATA GT_FILE[].
  ENDIF.

  IF GT_USFILE[] IS NOT INITIAL.
    PERFORM BUILD_HEADER USING LS_FILE_DATA GT_USFILE[].
  ENDIF.

ENDFORM.                    " PROCESS_MTD
*&---------------------------------------------------------------------*
*&      Form  CALC_DRCR_DIFFERENCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_DRCR_DIFFERENCE .

  DATA: LT_FAGLFLEXT TYPE STANDARD TABLE OF TY_MTD,
       LV_BUKRS     TYPE BUKRS,
       LV_ACCT      TYPE RACCT,
       LV_RASSC     TYPE RASSC,
       LV_HSL(5)    TYPE C,
       TOT_VAL      TYPE HSLVT12,
       LV_TABIX(2)  TYPE N.

  FIELD-SYMBOLS: <FAGLFLEXT> LIKE LINE OF LT_FAGLFLEXT,
                 <FAGL>      LIKE LINE OF GT_FAGLFLEXT,
                 <PREV_VAL>  TYPE ANY,
                 <VAL>         TYPE ANY,
                 <PREV_HSLVT>  TYPE ANY,
                 <VAL_HSLVT>   TYPE ANY.

  REFRESH: GT_FAGLFLEXT.

  SELECT RYEAR RACCT RBUKRS RCNTR PRCTR RASSC HSLVT
         HSL01 HSL02 HSL03 HSL04 HSL05 HSL06 HSL07 HSL08
         HSL09 HSL10 HSL11 HSL12 HSL13 HSL14 HSL15 HSL16 TIMESTAMP
         INTO TABLE LT_FAGLFLEXT
         FROM FAGLFLEXT
         WHERE RYEAR  EQ P_RYEAR
         AND   RLDNR  IN S_RLDNR
         AND   RACCT  IN S_RACCT
         AND   RBUKRS IN S_RBUKRS.

  IF SY-SUBRC EQ 0.
    IF P_PERIOD EQ '01'.
      DELETE LT_FAGLFLEXT WHERE HSL01 IS INITIAL .
    ELSEIF P_PERIOD EQ '02'.
      DELETE LT_FAGLFLEXT WHERE HSL02 IS INITIAL .
    ELSEIF P_PERIOD EQ '03'.
      DELETE LT_FAGLFLEXT WHERE HSL03 IS INITIAL .
    ELSEIF P_PERIOD EQ '04'.
      DELETE LT_FAGLFLEXT WHERE HSL04 IS INITIAL .
    ELSEIF P_PERIOD EQ '05'.
      DELETE LT_FAGLFLEXT WHERE HSL05 IS INITIAL .
    ELSEIF P_PERIOD EQ '06'.
      DELETE LT_FAGLFLEXT WHERE HSL06 IS INITIAL .
    ELSEIF P_PERIOD EQ '07'.
      DELETE LT_FAGLFLEXT WHERE HSL07 IS INITIAL .
    ELSEIF P_PERIOD EQ '08'.
      DELETE LT_FAGLFLEXT WHERE HSL08 IS INITIAL .
    ELSEIF P_PERIOD EQ '09'.
      DELETE LT_FAGLFLEXT WHERE HSL09 IS INITIAL .
    ELSEIF P_PERIOD EQ '10'.
      DELETE LT_FAGLFLEXT WHERE HSL10 IS INITIAL .
    ELSEIF P_PERIOD EQ '11'.
      DELETE LT_FAGLFLEXT WHERE HSL11 IS INITIAL .
    ELSEIF P_PERIOD EQ '12'.
      DELETE LT_FAGLFLEXT WHERE HSL12 IS INITIAL .
    ENDIF.
    SORT LT_FAGLFLEXT BY RACCT RBUKRS ASCENDING TIMESTAMP DESCENDING.
    LOOP AT LT_FAGLFLEXT ASSIGNING <FAGLFLEXT>.
      IF LV_BUKRS = <FAGLFLEXT>-RBUKRS AND
         LV_ACCT  = <FAGLFLEXT>-RACCT.

        CLEAR: LV_TABIX.
        DO 16 TIMES.
          LV_TABIX = LV_TABIX + 1.
          CONCATENATE 'HSL' LV_TABIX INTO LV_HSL.
          ASSIGN COMPONENT LV_HSL OF STRUCTURE <FAGL>      TO <PREV_VAL>.
          ASSIGN COMPONENT LV_HSL OF STRUCTURE <FAGLFLEXT> TO <VAL>.
          IF <VAL> IS NOT INITIAL.
            <PREV_VAL> = <PREV_VAL> + <VAL>.
          ENDIF.
        ENDDO.
        ASSIGN COMPONENT 'HSLVT' OF STRUCTURE <FAGL>      TO <PREV_HSLVT>.
        ASSIGN COMPONENT 'HSLVT' OF STRUCTURE <FAGLFLEXT> TO <VAL_HSLVT>.
        IF <VAL_HSLVT> IS NOT INITIAL.
          <PREV_HSLVT> = <PREV_HSLVT> + <VAL_HSLVT>.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO GT_FAGLFLEXT ASSIGNING <FAGL>.
        <FAGL> = <FAGLFLEXT>.
      ENDIF.
      LV_BUKRS = <FAGLFLEXT>-RBUKRS.
      LV_ACCT  = <FAGLFLEXT>-RACCT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " CALC_DRCR_DIFFERENCE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_HIST_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FAGLFLEXT>  text
*      -->P_LV_PERIOD  text
*      -->P_LV_AMT  text
*      <--P_LS_CUR_DATA  text
*----------------------------------------------------------------------*
FORM PROCESS_HIST_RECORD  USING PS_FAGLFLEXT TYPE TY_MTD
                                P_PERIOD     TYPE ANY
                                P_AMT        TYPE ANY
                                P_CURRENCY   TYPE ANY
                       CHANGING PS_DATA      TYPE LINE OF TRUXS_T_TEXT_DATA.

*  PS_DATA-LEDGER_NAME      = 'SAP UG GAAP Ledger'.
*  CONCATENATE 'SAP_UG_AB_' PS_FAGLFLEXT-RBUKRS '_' SY-DATUM '_'
*              SY-UZEIT INTO PS_DATA-TRANSACTION_NUMBER.
*  PS_DATA-REPORTING_PERIOD  = P_PERIOD.
*  PS_DATA-CURRENCY          = P_CURRENCY.    " Currency
*  PS_DATA-COMPANY_CODE      = PS_FAGLFLEXT-RBUKRS.   " Company Code
*  PS_DATA-ACCOUNT           = PS_FAGLFLEXT-RACCT.    " Account Number
*  PS_DATA-COST_CENTER       = PS_FAGLFLEXT-RCNTR.  " Cost center
**  OVERLAY ps_data-cost_center WITH '0000000000'.   " Cost center
**  ps_data-wbs               = '00000000'.          " WBS
**  ps_data-co_order          = '0000000000'.        " CO Order
**  ps_data-pm_order          = '0000000000'.        " PM Order
**  ps_data-statistical_order = '0000000000'.        " Statistical Order
**  ps_data-network_activity  = '0000000000'.        " Network Activity
*  PS_DATA-H_VARIABLE_1      = ''.
*  PS_DATA-H_VARIABLE_2      = ''.
*  PS_DATA-H_VARIABLE_3      = ''.
*  PS_DATA-L_VARIABLE_1      = ''.
*  PS_DATA-L_VARIABLE_2      = ''.
*  PS_DATA-L_VARIABLE_3      = ''.
*  PS_DATA-RUN_DATE          = SY-DATUM.
*  PS_DATA-TIMESTAMP         = SY-UZEIT.
*  PS_DATA-ZUSER             = SY-UNAME.



ENDFORM.                    " PROCESS_HIST_RECORD
*&---------------------------------------------------------------------*
*&      Form  COMPARE_RECORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FAGLFLEXT>  text
*      -->P_LS_CUR_DATA  text
*      -->P_LV_PERIOD  text
*      -->P_<VAL>  text
*      <--P_LV_SKIP  text
*----------------------------------------------------------------------*
*FORM COMPARE_RECORDS  USING    PS_FAGLFLEXT TYPE TY_MTD
*                               LS_CURRENT   TYPE ZFI_TRIAL_BAL
*                               P_PERIOD     TYPE ANY
*                               P_AMT        TYPE ANY
*                      CHANGING P_SKIP       TYPE ANY.

*  FIELD-SYMBOLS: <HIST_TRIAL_BAL> LIKE LINE OF GT_HIST_TRIAL_BAL.
*
*  CLEAR: P_SKIP.
*  READ TABLE GT_HIST_TRIAL_BAL ASSIGNING <HIST_TRIAL_BAL>
*                               WITH KEY ACCOUNT          = PS_FAGLFLEXT-RACCT
*                                        REPORTING_PERIOD = P_PERIOD
*                                        COMPANY_CODE     = PS_FAGLFLEXT-RBUKRS
*                               BINARY SEARCH.
*  IF SY-SUBRC EQ 0.
*    IF <HIST_TRIAL_BAL>-AMOUNT EQ P_AMT.
*      P_SKIP = 'X'.
*    ELSE.
*      LS_CURRENT-AMOUNT = P_AMT - <HIST_TRIAL_BAL>-AMOUNT.
*    ENDIF.
*  ELSE.
*    LS_CURRENT-AMOUNT = P_AMT.
*  ENDIF.

*ENDFORM.                    " COMPARE_RECORDS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CUR_TRIAL_BAL[]  text
*      -->P_0031   text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_REPORT  USING PT_DATA   TYPE TRUXS_T_TEXT_DATA
                               P_TABNAME TYPE ANY.

  PERFORM BUILD_FIELDCATALOG USING P_TABNAME.
  PERFORM BUILD_LAYOUT.
  PERFORM BUILD_EVENTS.
  PERFORM EXECUTE_ALV_FUCNTION USING PT_DATA[].

ENDFORM.                    " DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TABNAME  text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG  USING P_TABNAME TYPE ANY.

  REFRESH: GT_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = P_TABNAME
    CHANGING
      CT_FIELDCAT            = GT_FCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .

  GS_LAYO-ZEBRA      = 'X'.
  GS_LAYO-GRID_TITLE = 'Trial Balance'.

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_EVENTS .

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = GT_EVENTS[].

ENDFORM.                    " BUILD_EVENTS
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_ALV_FUCNTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_DATA[]  text
*----------------------------------------------------------------------*
FORM EXECUTE_ALV_FUCNTION  USING PT_DATA TYPE STANDARD TABLE.

  DATA: WA_SORT TYPE SLIS_SORTINFO_ALV,
        LR_GRID TYPE REF TO CL_GUI_ALV_GRID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME   = SY-REPID
      I_STRUCTURE_NAME = 'ZFI_TRIAL_BAL'
      I_INCLNAME       = SY-REPID
    CHANGING
      CT_FIELDCAT      = GT_FIELDCATALOG.

  GD_REPID = SY-REPID.
*  SORT gt_alv_data.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = GD_REPID
      I_CALLBACK_TOP_OF_PAGE = 'TOP-OF-PAGE' "see FORM
      IS_LAYOUT              = GD_LAYOUT
      IT_FIELDCAT            = GT_FIELDCATALOG[]
      IT_EVENTS              = GT_EVENTS
      IS_PRINT               = GD_PRNTPARAMS
      I_SAVE                 = 'X'
    TABLES
      T_OUTTAB               = PT_DATA[]
    EXCEPTIONS
      PROGRAM_ERROR          = 1
      OTHERS                 = 2.

  IF SY-SUBRC <> 0.
    WRITE: /'Alv report could not be generated'.
  ENDIF.

ENDFORM.                    " EXECUTE_ALV_FUCNTION
