*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_DELTA_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_process .

*  DATA: lv_hsl(5)    TYPE c,
*        lv_tabix(2)  TYPE n,
*        lt_months    TYPE STANDARD TABLE OF t247,
*        lv_period(8) TYPE c,
*        lv_month     TYPE text20,
*        lv_effdate   TYPE char10,
*        tot_val      TYPE hslvt12,
*        lv_currency  TYPE waers,
*        ls_data      TYPE LINE OF truxs_t_text_data,
*        lv_amt       TYPE text30,
*        lv_transac   TYPE text40,
*        lv_str       TYPE string,
*        lv_prctr     TYPE string.
*
*  FIELD-SYMBOLS: <faglflext> LIKE LINE OF gt_faglflext,
*                 <last_day>  LIKE LINE OF gt_last_days,
*                 <data>      LIKE LINE OF gt_data,
*                 <t001>      LIKE LINE OF gt_t001,
*                 <val>       TYPE any,
**                 <ccm>        LIKE LINE OF gt_ccm,
*                 <month>     LIKE LINE OF lt_months.
*
*  SELECT * INTO TABLE gt_faglflext FROM faglflext
*                                 WHERE ryear  EQ p_ryear
*                                 AND   rpmax  IN s_rpmax
*                                 AND   rldnr  IN s_rldnr
*                                 AND   racct  IN s_racct
*                                 AND   rbukrs IN s_rbukrs.
*  IF sy-subrc EQ 0.
*    SELECT bukrs waers INTO TABLE gt_t001 FROM t001.
*    SORT gt_t001.
*    PERFORM get_last_days.
**    LOOP AT s_ccm.
**      APPEND INITIAL LINE TO gt_ccm ASSIGNING <ccm>.
**      SPLIT s_ccm-low AT '-' INTO <ccm>-gl_no <ccm>-kostl.
**    ENDLOOP.
**    SORT gt_ccm.
*    LOOP AT gt_faglflext ASSIGNING <faglflext>.
*      CLEAR: tot_val, lv_tabix, lv_currency.
*      DO 16 TIMES.
*        lv_tabix = lv_tabix + 1.
*        CONCATENATE 'HSL' lv_tabix INTO lv_hsl.
*        ASSIGN COMPONENT lv_hsl OF STRUCTURE <faglflext> TO <val>.
*        IF <val> IS NOT INITIAL.
*          tot_val = tot_val + <val>.
*        ENDIF.
*      ENDDO.
*
*      tot_val = tot_val + <faglflext>-hslvt.
*
*      IF tot_val IS NOT INITIAL.
*        IF tot_val > 0.
*          lv_amt  = tot_val.
*          SHIFT lv_amt LEFT DELETING LEADING space.
*        ELSE.
*          tot_val = tot_val * -1.
*          lv_amt  = tot_val.
*          SHIFT lv_amt LEFT DELETING LEADING space.
*          CONCATENATE '-' lv_amt INTO lv_amt.
*        ENDIF.
*        CONCATENATE '' lv_amt '' INTO lv_amt.
*        READ TABLE gt_last_days ASSIGNING <last_day>
*                            WITH KEY month = <faglflext>-rpmax+1(2) BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          CONCATENATE p_ryear '-' <last_day>-month '-' <last_day>-day INTO lv_effdate.
*        ELSE.
*          CONCATENATE p_ryear '-12-31' INTO lv_effdate.
*        ENDIF.
*
*        CONCATENATE 'SAPUGAB_' <faglflext>-rbukrs '_' sy-datum+2(6) sy-uzeit INTO lv_transac.
*        CONCATENATE <faglflext>-rpmax '.' p_ryear INTO lv_period.
*        SHIFT lv_period LEFT DELETING LEADING '0'.
*        SHIFT <faglflext>-racct LEFT DELETING LEADING '0'.
*
*        READ TABLE gt_t001 ASSIGNING <t001>
*                           WITH KEY bukrs = <faglflext>-rbukrs
*                           BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          lv_currency = <t001>-waers.
*        ENDIF.
*
*        CONCATENATE 'SAP UG GAAP Ledger'
*                     lv_transac
*                     lv_period
*                     lv_effdate
*                     lv_currency
*                     <faglflext>-rbukrs INTO ls_data SEPARATED BY ','.  " Company code.
*
*        SHIFT <faglflext>-racct LEFT DELETING LEADING '0'.              " Account number
*        CONCATENATE ls_data <faglflext>-racct INTO ls_data SEPARATED BY ','.
*        PERFORM process_ug_file USING <faglflext> lv_period lv_amt CHANGING ls_data.
*        IF <faglflext>-rbukrs IN s_usco.
*          APPEND ls_data TO  gt_usfile.
*        ELSE.
*          APPEND ls_data TO gt_file.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

*  IF gt_file[] IS NOT INITIAL.
*    PERFORM build_header USING ls_data gt_file[].
*  ENDIF.
*
*  IF gt_usfile[] IS NOT INITIAL.
*    PERFORM build_header USING ls_data gt_usfile[].
*  ENDIF.

ENDFORM.                    " READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DATA  text
*----------------------------------------------------------------------*
FORM build_header  USING  ps_data TYPE any
                          pt_data TYPE STANDARD TABLE.

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
              INTO ps_data SEPARATED BY ','.

  INSERT ps_data INTO pt_data INDEX 1.

ENDFORM.                    " BUILD_HEADER
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen .

  LOOP AT SCREEN.
    IF rd_pc EQ 'X'.
      IF screen-group1 EQ 'RD4' OR
         screen-group1 EQ 'RD6'.
        screen-input     = 1.       "Visible
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'RD7'. "Server File Path
        screen-input     = 0.       "Hidden
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      CLEAR p_man.
    ELSE. "Server File
      IF screen-group1 EQ 'RD5' OR
         screen-group1 EQ 'RD7'.
        screen-input     = 1.       "Visible
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'RD6'. "PC File Path
        screen-input     = 0.       "Hidden
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF cb_clrd EQ 'X'.
      IF screen-group1 EQ 'CRD'.
        screen-input     = 1.       "Visible
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 EQ 'CRD'.
        screen-input     = 0.       "Hidden
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-name = 'P_ERRORF' OR screen-name = 'P_LOGF'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'P_APPF'.
      IF p_man EQ 'X'.
        screen-input = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-input = '0'.
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
FORM get_filename .

  DATA: lv_filename TYPE localfile,
        lv_param_1  TYPE text15,
        lv_param_3  TYPE text15.

  lv_param_1 = sy-sysid.

* Lookup logical file path
  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = 'ZFI_READ_GL_FOR_IS'
      parameter_1      = lv_param_1
      parameter_2      = lv_param_1
      parameter_3      = lv_param_3
    IMPORTING
      file_name        = p_appf
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  IF cb_bclr EQ 'X' OR cb_clrd EQ 'X'.
    CONCATENATE p_appf 'I_' INTO p_appf.
  ELSE.
    CONCATENATE p_appf 'D_' INTO p_appf.
  ENDIF.

  CONCATENATE p_appf run_date '_' run_time '.csv' INTO p_appf.

* lookup logical file path for error file location
  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = 'ZFI_READ_GL_FOR_IS_ERROR'
      parameter_1      = lv_param_1
      parameter_2      = lv_param_1
      parameter_3      = lv_param_3
    IMPORTING
      file_name        = p_errorf
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  IF cb_bclr EQ 'X' OR cb_clrd EQ 'X'.
    CONCATENATE p_errorf 'INIT_' INTO p_errorf.
  ELSE.
    CONCATENATE p_errorf 'DELTA_' INTO p_errorf.
  ENDIF.

  CONCATENATE p_errorf run_date '_' run_time '.EXT' INTO p_errorf.

* Lookup logical file path for error file location
  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = 'ZFI_READ_GL_FOR_IS_LOG'
      parameter_1      = lv_param_1
      parameter_2      = lv_param_1
      parameter_3      = lv_param_3
    IMPORTING
      file_name        = p_logf
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  IF cb_bclr EQ 'X' OR cb_clrd EQ 'X'.
    CONCATENATE p_logf 'INIT_' INTO p_logf.
  ELSE.
    CONCATENATE p_logf 'DELTA_' INTO p_logf.
  ENDIF.

  CONCATENATE p_logf run_date '_' run_time '.EXT' INTO p_logf.

ENDFORM.                    " GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_file .

  DATA: lv_file       TYPE string,
        lv_appfile    TYPE localfile,
        lv_error(75)  TYPE c.

  FIELD-SYMBOLS: <file> LIKE LINE OF gt_file.

  CLEAR: gv_file_ok.

  IF rd_pc EQ 'X'. " PC file
    IF gt_file[] IS NOT INITIAL.
      lv_file = p_file.
      REPLACE 'WAERS' IN lv_file WITH 'CAD'.
      PERFORM write_pc_file USING gt_file[] lv_file.
    ENDIF.

    IF gt_usfile[] IS NOT INITIAL.
      lv_file = p_file.
      REPLACE 'WAERS' IN lv_file WITH 'USD'.
      PERFORM write_pc_file USING gt_usfile[] lv_file.
    ENDIF.
  ELSE.

    IF gt_file[] IS NOT INITIAL.
      lv_appfile = p_appf.
      REPLACE 'WAERS' IN lv_appfile WITH 'CAD'.
      OPEN DATASET lv_appfile FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc EQ 0.
        LOOP AT gt_file ASSIGNING <file>.
          TRANSFER <file> TO lv_appfile.
        ENDLOOP.
        gv_file_ok = 'X'.
      ELSE.
        WRITE: / lv_error , ' - ',  lv_appfile.
        EXIT.
      ENDIF.
    ENDIF.

    IF gt_usfile[] IS NOT INITIAL.
      lv_appfile = p_appf.
      REPLACE 'WAERS' IN lv_appfile WITH 'USD'.
      OPEN DATASET lv_appfile FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc EQ 0.
        LOOP AT gt_usfile ASSIGNING <file>.
          TRANSFER <file> TO lv_appfile.
        ENDLOOP.
        gv_file_ok = 'X'.
      ELSE.
        WRITE: / lv_error , ' - ',  lv_appfile.
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
FORM write_pc_file  USING pt_data TYPE STANDARD TABLE
                             p_file TYPE any.

  DATA: lv_file TYPE string.

  lv_file = p_file.

* Write file data
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_file
      filetype                = 'ASC'
    TABLES
      data_tab                = pt_data
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc EQ 0.
    gv_file_ok = 'X'.
  ENDIF.

ENDFORM.                    " WRITE_PC_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_last_days .

  DATA: lv_date     TYPE datum,
        lv_last_day TYPE datum,
        ls_lastday  LIKE LINE OF gt_last_days.

  REFRESH: gt_last_days.

  DO 12 TIMES.
    IF lv_date IS INITIAL.
      CONCATENATE p_ryear '0101' INTO lv_date.
    ENDIF.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_date
      IMPORTING
        last_day_of_month = lv_last_day
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    IF sy-subrc EQ 0.
      ls_lastday-year  = lv_last_day(4).
      ls_lastday-month = lv_last_day+4(2).
      ls_lastday-day   = lv_last_day+6(2).
      APPEND ls_lastday TO gt_last_days.
    ENDIF.

    lv_date = lv_date + 31.
  ENDDO.

  SORT gt_last_days.

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
FORM overlay  USING    p_val TYPE any
                       p_overlay TYPE any
              CHANGING p_data TYPE LINE OF truxs_t_text_data .
  DATA: lv_val TYPE string.

  lv_val = p_val.
  OVERLAY lv_val WITH p_overlay.
  CONCATENATE p_data lv_val INTO p_data SEPARATED BY ','.

ENDFORM.                    " OVERLAY
*&---------------------------------------------------------------------*
*&      Form  PROCESS_US_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FAGLFLEXT>  text
*      <--P_LS_DATA  text
*----------------------------------------------------------------------*
FORM process_us_file  USING    ps_faglflext TYPE faglflext
                      CHANGING ps_data      TYPE any.

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
*FORM process_ug_file  USING    ps_faglflext  TYPE ty_mtd
*                                p_period     TYPE any
*                                p_amt        TYPE any
*                       CHANGING ps_data      TYPE any
*                                ps_cur_data  TYPE LINE OF ty_t_zfi_trial_bal.

*  IF ps_faglflext-rcntr IS NOT INITIAL AND ps_faglflext-rcntr NE ''.       " Cost center
*    SHIFT ps_faglflext-rcntr LEFT DELETING LEADING '0'.
*    CONCATENATE ps_data ps_faglflext-rcntr INTO ps_data SEPARATED BY ','.
*    ps_cur_data-cost_center = ps_faglflext-rcntr.
*  ELSE.
*    CONCATENATE ps_data '0000000000' INTO ps_data SEPARATED BY ','.
*    ps_cur_data-cost_center = '0000000000'.
*  ENDIF.
*
*  CONCATENATE ps_data '000000'          INTO ps_data SEPARATED BY ','.      " CO_ORDER
*  ps_cur_data-co_order = '000000'.
*  CONCATENATE ps_data '000000000000000' INTO ps_data SEPARATED BY ','.      " Financial Asset
*  ps_cur_data-financial_asset = '000000000000000'.
*  CONCATENATE ps_data '000000000000000' INTO ps_data SEPARATED BY ','.      " WBS
*  ps_cur_data-wbs = '000000000000000'.
*  CONCATENATE ps_data
*              p_amt                                                         " Amount
*              space
*              space
*              space
*              space
*              space
*              space
*              INTO ps_data SEPARATED BY ','.         " Profit Center
*ENDFORM.                    " PROCESS_UG_FILE
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_values .

  s_usco-low    = 'DGLC'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'EMP'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'ESPC'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'EUSA'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'MLG'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'MPI'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'SCPC'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'TGC'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'TGS'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'TUSA'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  s_usco-low    = 'UGSL'.
  s_usco-option = 'EQ'.
  s_usco-sign   = 'I'.
  APPEND s_usco.

  CONCATENATE 'H:\MDM_Data_Profiling_Samples\SAP_UG_WAERS_TB_GL_'
              sy-datum '_' sy-uzeit '.csv' INTO p_file.

ENDFORM.                    " DEFAULT_VALUES
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DELTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_delta .

*  DATA: lv_hsl(5)    TYPE c,
*        first_rec    TYPE c,
*        lv_tabix(2)  TYPE n,
*        lt_months    TYPE STANDARD TABLE OF t247,
*        lv_period(8) TYPE c,
*        lv_month     TYPE text20,
*        lv_effdate   TYPE char10,
*        tot_val      TYPE hslvt12,
*        ls_file_data TYPE LINE OF truxs_t_text_data,
*        lv_amt       TYPE text30,
*        lv_amt_delta TYPE hslvt12,
*        lv_transac   TYPE text40,
*        lv_str       TYPE string,
*        lv_prctr     TYPE string,
*        lv_skip      TYPE c,
*        ls_cur_data  TYPE LINE OF ty_t_zfi_trial_bal,
*        lv_account   TYPE racct,
*        lt_t001      TYPE STANDARD TABLE OF t001.
*
*  FIELD-SYMBOLS: <faglflext> LIKE LINE OF gt_faglflext,
*                 <last_day>  LIKE LINE OF gt_last_days,
*                 <data>      LIKE LINE OF gt_data,
*                 <t001>      LIKE LINE OF gt_t001,
*                 <val>       TYPE any,
*                 <month>     LIKE LINE OF lt_months.
*
*  REFRESH: gt_hist_trial_bal, gt_faglflext.
*
*  PERFORM calc_drcr_difference.
*
*  LOOP AT gt_faglflext ASSIGNING <faglflext>.
*    CONCATENATE 'HSL' p_period INTO lv_hsl.
*    ASSIGN COMPONENT lv_hsl OF STRUCTURE <faglflext> TO <val>.
*    IF <val> IS ASSIGNED.
*      CHECK <val> IS NOT INITIAL.
*      IF first_rec EQ ''. "Only execute once for the first record
*        first_rec = 'X'.
*        CONCATENATE p_period '.' p_ryear INTO lv_period.
*        SELECT * INTO TABLE gt_hist_trial_bal
*                            FROM zfi_trial_bal
*                            WHERE reporting_period EQ lv_period.
*        IF sy-subrc EQ 0.
*          SORT gt_hist_trial_bal
*          BY account reporting_period company_code ASCENDING
*             run_date timestamp DESCENDING.
*        ENDIF.
*
*        SELECT bukrs waers INTO TABLE gt_t001 FROM t001.
*        SORT gt_t001.
*        PERFORM get_last_days.
*      ENDIF.
*    ENDIF.
*
*    CLEAR: ls_cur_data, ls_file_data, lv_amt,lv_amt_delta.
*    READ TABLE gt_last_days ASSIGNING <last_day>
*                            WITH KEY month = p_period BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      CONCATENATE p_ryear '-' <last_day>-month '-' <last_day>-day INTO lv_effdate.
*    ELSE.
*      CONCATENATE p_ryear '-12-31' INTO lv_effdate.
*    ENDIF.
*
*    ls_cur_data-effective_date = lv_effdate.
*    READ TABLE gt_t001 ASSIGNING <t001>
*                       WITH KEY bukrs = <faglflext>-rbukrs
*                       BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      ls_cur_data-currency = <t001>-waers.
*    ENDIF.
*
** Populate history table work area
*    PERFORM process_hist_record USING <faglflext> lv_period <val> ls_cur_data-currency
*                                CHANGING ls_cur_data.
*
*    PERFORM compare_records USING <faglflext> ls_cur_data lv_period <val> lv_amt_delta
*                            CHANGING lv_skip.
*    CHECK lv_skip EQ ''.
*
*    IF lv_amt_delta > 0.
*      lv_amt = lv_amt_delta.
*      SHIFT lv_amt LEFT DELETING LEADING space.
*    ELSE.
*      lv_amt_delta = lv_amt_delta * -1.
*      lv_amt       = lv_amt_delta.
*      SHIFT lv_amt LEFT DELETING LEADING space.
*      CONCATENATE '-' lv_amt INTO lv_amt.
*    ENDIF.
*
** Delimit record by comma
*    lv_account = <faglflext>-racct.
*    SHIFT lv_account LEFT DELETING LEADING '0'.
*    CONCATENATE ls_cur_data-ledger_name
*                ls_cur_data-transaction_number
*                ls_cur_data-reporting_period
*                ls_cur_data-effective_date
*                ls_cur_data-currency             " Currency
*                ls_cur_data-company_code         " Company code
*                lv_account           " Cloumn 7  " Account number
*                INTO ls_file_data SEPARATED BY ','.
*
*    PERFORM process_ug_file USING <faglflext> lv_period lv_amt
*                            CHANGING ls_file_data ls_cur_data.
*
** Build internal table in order to populate histroy table
*    APPEND ls_cur_data TO gt_cur_trial_bal.
*
*    IF <faglflext>-rbukrs IN s_usco.
*      APPEND ls_file_data TO gt_usfile.
*    ELSE.
*      APPEND ls_file_data TO gt_file.
*    ENDIF.
*  ENDLOOP.
*
*  IF gt_file[] IS NOT INITIAL.
*    PERFORM build_header USING ls_file_data gt_file[].
*  ENDIF.
*
*  IF gt_usfile[] IS NOT INITIAL.
*    PERFORM build_header USING ls_file_data gt_usfile[].
*  ENDIF.

ENDFORM.                    " PROCESS_DELTA
*&---------------------------------------------------------------------*
*&      Form  merge_records
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM merge_records .

  DATA: lt_faglflext  TYPE STANDARD TABLE OF ty_delta,
        lt_t001       TYPE STANDARD TABLE OF ty_t001,
        lv_bukrs      TYPE bukrs,
        lv_acct       TYPE racct,
        lv_hsl(5)     TYPE c,
        lv_waers      TYPE waers,
        lv_kostl      TYPE kostl,
        lv_period(2)  TYPE n.

  FIELD-SYMBOLS: <faglflext>   LIKE LINE OF lt_faglflext,
                 <fagl>        LIKE LINE OF gt_faglflext,
                 <prev_val>    TYPE any,
                 <val>         TYPE any,
                 <prev_hslvt>  TYPE any,
                 <t001>        LIKE LINE OF lt_t001,
                 <val_hslvt>   TYPE any.

  REFRESH: gt_faglflext,gt_raw_faglflext.

  SELECT ryear racct rbukrs rcntr rtcur prctr hslvt
         hsl01 hsl02 hsl03 hsl04 hsl05 hsl06 hsl07 hsl08
         hsl09 hsl10 hsl11 hsl12 hsl13 hsl14 hsl15 hsl16 timestamp
         INTO TABLE lt_faglflext
         FROM faglflext
         WHERE ryear  EQ p_ryear
         AND   rldnr  IN s_rldnr
         AND   racct  IN s_racct
         AND   rbukrs IN s_rbukrs.

  CHECK sy-subrc EQ 0.
  SELECT bukrs waers INTO TABLE lt_t001 FROM t001.
  SORT lt_t001.
  SORT lt_faglflext BY rbukrs.
  LOOP AT lt_faglflext ASSIGNING <faglflext>.
    IF lv_bukrs NE <faglflext>-rbukrs.
      READ TABLE lt_t001 ASSIGNING <t001>
                         WITH KEY bukrs = <faglflext>-rbukrs
                         BINARY SEARCH.
      IF sy-subrc EQ 0.
        <faglflext>-rtcur = <t001>-waers.
      ENDIF.
    ELSE.
      IF <t001>-waers IS ASSIGNED.
        <faglflext>-rtcur = <t001>-waers.
      ENDIF.
    ENDIF.
    lv_bukrs = <faglflext>-rbukrs.
  ENDLOOP.
  CLEAR: lv_bukrs.

*Fill GT_RAW_FAGLFLEXT to write this internal table into a file so
*could be used for debugging in case of any error
  gt_raw_faglflext[] = lt_faglflext[].

  SORT lt_faglflext BY racct rbukrs rcntr rtcur ASCENDING
                                      timestamp DESCENDING.
  lv_period = p_period.
  WRITE lv_period USING EDIT MASK '==ALPHA'. "Zero padding
  CONCATENATE 'HSL' lv_period INTO lv_hsl.
  LOOP AT lt_faglflext ASSIGNING <faglflext>.
    IF lv_bukrs = <faglflext>-rbukrs AND
       lv_acct  = <faglflext>-racct  AND
       lv_kostl = <faglflext>-rcntr  AND
       lv_waers = <faglflext>-rtcur.

      ASSIGN COMPONENT lv_hsl OF STRUCTURE <fagl>      TO <prev_val>.
      ASSIGN COMPONENT lv_hsl OF STRUCTURE <faglflext> TO <val>.
      IF <val> IS NOT INITIAL.
        <prev_val> = <prev_val> + <val>.
      ENDIF.

      ASSIGN COMPONENT 'HSLVT' OF STRUCTURE <fagl>      TO <prev_hslvt>.
      ASSIGN COMPONENT 'HSLVT' OF STRUCTURE <faglflext> TO <val_hslvt>.
      IF <val_hslvt> IS NOT INITIAL.
        <prev_hslvt> = <prev_hslvt> + <val_hslvt>.
      ENDIF.
    ELSE.
      APPEND INITIAL LINE TO gt_faglflext ASSIGNING <fagl>.
      <fagl> = <faglflext>.
    ENDIF.
    lv_bukrs = <faglflext>-rbukrs.
    lv_acct  = <faglflext>-racct.
    lv_waers = <faglflext>-rtcur.
    lv_kostl = <faglflext>-rcntr.
  ENDLOOP.

ENDFORM.                    " merge_records
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
FORM process_hist_record  USING ps_faglflext TYPE ty_delta
                                p_period     TYPE any
                                p_amt        TYPE any
                                p_currency   TYPE any
                       CHANGING ps_data      TYPE LINE OF ty_t_zfi_trial_bal.

  ps_data-ledger_name      = 'SAP UG GAAP Ledger'.
  CONCATENATE 'SAP_UG_AB_' ps_faglflext-rbukrs '_' sy-datum '_'
              sy-uzeit INTO ps_data-transaction_number.
  ps_data-reporting_period  = p_period.
  ps_data-currency          = p_currency.    " Currency
  ps_data-company_code      = ps_faglflext-rbukrs.   " Company Code
  ps_data-account           = ps_faglflext-racct.    " Account Number
  ps_data-cost_center       = ps_faglflext-rcntr.  " Cost center
*  OVERLAY ps_data-cost_center WITH '0000000000'.   " Cost center
*  ps_data-wbs               = '00000000'.          " WBS
*  ps_data-co_order          = '0000000000'.        " CO Order
*  ps_data-pm_order          = '0000000000'.        " PM Order
*  ps_data-statistical_order = '0000000000'.        " Statistical Order
*  ps_data-network_activity  = '0000000000'.        " Network Activity
  ps_data-amount            = p_amt.
  ps_data-h_variable_1      = ''.
  ps_data-h_variable_2      = ''.
  ps_data-h_variable_3      = ''.
  ps_data-l_variable_1      = ''.
  ps_data-l_variable_2      = ''.
  ps_data-l_variable_3      = ''.
  ps_data-run_date          = sy-datum.
  ps_data-timestamp         = sy-uzeit.
  ps_data-zuser             = sy-uname.

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
FORM compare_records  USING    ps_faglflext TYPE ty_delta
                               ls_current   TYPE zfi_trial_bal
                               p_period     TYPE any
                               p_amt        TYPE any
                               p_amt_delta  TYPE any
                      CHANGING p_skip       TYPE any.

  FIELD-SYMBOLS: <hist_trial_bal> LIKE LINE OF gt_hist_trial_bal.

  CLEAR: p_skip.
  READ TABLE gt_hist_trial_bal ASSIGNING <hist_trial_bal>
                               WITH KEY account          = ps_faglflext-racct
                                        reporting_period = p_period
                                        company_code     = ps_faglflext-rbukrs
                               BINARY SEARCH.
  IF sy-subrc EQ 0.
    IF <hist_trial_bal>-amount EQ p_amt.
      p_skip = 'X'.
    ELSE.
      p_amt_delta = p_amt - <hist_trial_bal>-amount.
    ENDIF.
  ELSE.
    p_amt_delta = p_amt.
  ENDIF.

ENDFORM.                    " COMPARE_RECORDS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CUR_TRIAL_BAL[]  text
*      -->P_0031   text
*----------------------------------------------------------------------*
FORM display_alv_report  USING pt_data   TYPE ty_t_zfi_trial_bal
                               p_tabname TYPE any.

  PERFORM build_fieldcatalog USING p_tabname.
  PERFORM build_layout.
  PERFORM build_events.
  PERFORM execute_alv_fucntion USING pt_data[].

ENDFORM.                    " DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TABNAME  text
*----------------------------------------------------------------------*
FORM build_fieldcatalog  USING p_tabname TYPE any.

  REFRESH: gt_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_tabname
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
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
FORM build_layout .

  gs_layo-zebra      = 'X'.
  gs_layo-grid_title = 'Trial Balance'.

ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_events .

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events[].

ENDFORM.                    " BUILD_EVENTS
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_ALV_FUCNTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_DATA[]  text
*----------------------------------------------------------------------*
FORM execute_alv_fucntion  USING pt_data TYPE STANDARD TABLE.

  DATA: wa_sort TYPE slis_sortinfo_alv,
        lr_grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name   = sy-repid
      i_structure_name = 'ZFI_TRIAL_BAL'
      i_inclname       = sy-repid
    CHANGING
      ct_fieldcat      = gt_fieldcatalog.

  gd_repid = sy-repid.
*  SORT gt_alv_data.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = gd_repid
      i_callback_top_of_page = 'TOP-OF-PAGE' "see FORM
      is_layout              = gd_layout
      it_fieldcat            = gt_fieldcatalog[]
      it_events              = gt_events
      is_print               = gd_prntparams
      i_save                 = 'X'
    TABLES
      t_outtab               = pt_data[]
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

  IF sy-subrc <> 0.
    WRITE: /'Alv report could not be generated'.
  ENDIF.

ENDFORM.                    " EXECUTE_ALV_FUCNTION
*&---------------------------------------------------------------------*
*&      Form  DELTA_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delta_processing .

  DATA: lv_hsl(5)    TYPE c,
        first_rec    TYPE c,
        lv_tabix(2)  TYPE n,
        lt_months    TYPE STANDARD TABLE OF t247,
        lv_period(8) TYPE c,
        lv_month     TYPE text20,
        lv_effdate   TYPE char10,
        ls_file_data TYPE LINE OF truxs_t_text_data,
        lv_prctr     TYPE string,
        ls_cur_data  TYPE LINE OF ty_t_zfi_trial_bal,
        lv_account   TYPE racct.

  FIELD-SYMBOLS: <faglflext> LIKE LINE OF gt_faglflext,
                 <last_day>  LIKE LINE OF gt_last_days,
                 <data>      LIKE LINE OF gt_data,
                 <val>       TYPE any,
                 <month>     LIKE LINE OF lt_months.

  REFRESH: gt_hist_trial_bal, gt_faglflext, gt_hist_bal_del,gt_current_bal.

  PERFORM merge_records.

  LOOP AT gt_faglflext ASSIGNING <faglflext>.
    CONCATENATE 'HSL' p_period INTO lv_hsl.
    ASSIGN COMPONENT lv_hsl OF STRUCTURE <faglflext> TO <val>.
    CHECK <val> IS ASSIGNED.
    IF first_rec EQ ''. "Only execute once for the first record
      first_rec = 'X'.
      CONCATENATE p_period '.' p_ryear INTO lv_period.
      PERFORM get_last_days.
    ENDIF.

    CLEAR: ls_cur_data.
    READ TABLE gt_last_days ASSIGNING <last_day>
                            WITH KEY month = p_period
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      CONCATENATE p_ryear '-' <last_day>-month '-'
                  <last_day>-day INTO lv_effdate.
    ELSE.
      CONCATENATE p_ryear '-12-31' INTO lv_effdate.
    ENDIF.

* Populate history table work area
    ls_cur_data-effective_date    = lv_effdate.
    ls_cur_data-reporting_period  = lv_period.
    ls_cur_data-amount            = <val>.
    PERFORM populate_history_rec USING <faglflext> CHANGING ls_cur_data.

* Build internal table in order to populate histroy table
    APPEND ls_cur_data TO gt_cur_trial_bal.
  ENDLOOP.

  PERFORM find_deltas.

ENDFORM.                    " DELTA_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  FIND_DELTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_deltas .

  DATA: lv_period(8) TYPE c,
       lv_amt       TYPE text30,
       lv_amt_delta TYPE hslvt12,
       lv_skip      TYPE c,
       lv_del       TYPE c,
       lv_tabix     TYPE sytabix,
       record_found TYPE c,
       ls_file_data TYPE LINE OF truxs_t_text_data,
       ls_hist_del  LIKE LINE OF gt_hist_bal_del,
       ls_upd_rec   LIKE LINE OF gt_upd_recs,
       tot_amt      TYPE hslvt12,
       lv_bal       TYPE  hslvt12,
       hist_amt     TYPE hslvt12,
       cur_amt      TYPE hslvt12,
       lv_account   TYPE racct,
       lv_bukrs     TYPE bukrs,
       lt_indx_del  TYPE re_t_tabix.

  FIELD-SYMBOLS: <hist_trial_bal> LIKE LINE OF gt_hist_trial_bal,
                 <indx_del>       LIKE LINE OF lt_indx_del,
                 <cur_balance>    LIKE LINE OF gt_cur_trial_bal.

  REFRESH: gt_upd_recs.

  CONCATENATE p_period '.' p_ryear INTO lv_period.

  SELECT * INTO TABLE gt_hist_trial_bal
                            FROM zfi_trial_bal
                            WHERE reporting_period EQ lv_period.
  IF sy-subrc EQ 0.
    SORT gt_hist_trial_bal
          BY account reporting_period company_code
             cost_center currency co_order pm_order wbs ASCENDING
             run_date timestamp DESCENDING.
  ENDIF.

  SORT gt_cur_trial_bal
       BY account reporting_period company_code
          cost_center currency co_order pm_order wbs ASCENDING
          run_date timestamp DESCENDING.

  LOOP AT gt_cur_trial_bal ASSIGNING <cur_balance>.
    lv_tabix = sy-tabix.
    CLEAR: lv_skip,  record_found, lv_del.
    SHIFT <cur_balance>-account LEFT DELETING LEADING '0'.
    lv_account = <cur_balance>-account.
    lv_bukrs   = <cur_balance>-company_code.
    cur_amt    = <cur_balance>-amount.
    LOOP AT gt_hist_trial_bal ASSIGNING <hist_trial_bal>
                              WHERE account           = <cur_balance>-account AND
                                    reporting_period  = <cur_balance>-reporting_period AND
                                    company_code      = <cur_balance>-company_code AND
                                    cost_center       = <cur_balance>-cost_center AND
                                    currency          = <cur_balance>-currency AND
                                    co_order          = <cur_balance>-co_order AND
                                    pm_order          = <cur_balance>-pm_order AND
                                    wbs               = <cur_balance>-wbs.
      hist_amt = <hist_trial_bal>-amount.
      IF hist_amt IS NOT INITIAL AND cur_amt IS INITIAL.
        lv_del      = 'X'.
        ls_hist_del = <hist_trial_bal>.
        APPEND ls_hist_del TO gt_hist_bal_del.
        APPEND lv_tabix TO lt_indx_del.
      ELSEIF hist_amt EQ cur_amt.
        lv_skip = 'X'.
      ELSE.
        lv_amt_delta = cur_amt - hist_amt.
      ENDIF.
      record_found = 'X'.
      EXIT.
    ENDLOOP.

    CHECK lv_del EQ 'X' OR lv_skip EQ ''.
    IF lv_del EQ 'X'.
      IF hist_amt > 0.
        lv_amt = hist_amt.
        SHIFT lv_amt LEFT DELETING LEADING space.
        CONCATENATE '-' lv_amt INTO lv_amt.
      ELSE.
        lv_amt_delta = hist_amt * -1.
        lv_amt = lv_amt_delta.
        SHIFT lv_amt LEFT DELETING LEADING space.
      ENDIF.
* If current amount and previous amount is the same then skip the record
    ELSEIF lv_skip EQ ''.
* if matching record not found in history table then assign current amount
      IF record_found EQ ''.
* Record NOT found and amount is zero so skip the record
        CHECK cur_amt IS NOT INITIAL.
        lv_amt_delta = cur_amt.
      ELSE. "Record found but amount is zero so skip the record
        CHECK cur_amt IS NOT INITIAL.
      ENDIF.
      IF lv_amt_delta > 0.
        lv_amt = lv_amt_delta.
        SHIFT lv_amt LEFT DELETING LEADING space.
      ELSE.
        lv_amt_delta = lv_amt_delta * -1.
        lv_amt       = lv_amt_delta.
        SHIFT lv_amt LEFT DELETING LEADING space.
        CONCATENATE '-' lv_amt INTO lv_amt.
      ENDIF.
    ENDIF.

* Delimit record by comma
    CONCATENATE <cur_balance>-ledger_name
                <cur_balance>-transaction_number
                <cur_balance>-reporting_period
                <cur_balance>-effective_date
                <cur_balance>-currency             " Currency
                <cur_balance>-company_code         " Company code
                <cur_balance>-account              " Cloumn 7  " Account number
                <cur_balance>-cost_center
                <cur_balance>-co_order
                <cur_balance>-financial_asset
                <cur_balance>-wbs
                lv_amt
                space
                space
                space
                space
                space
                space
                INTO ls_file_data SEPARATED BY ','.
    IF <cur_balance>-company_code IN s_usco.
      APPEND ls_file_data TO gt_usfile.
      APPEND <cur_balance> TO  gt_current_bal.
    ELSE.
      APPEND ls_file_data TO gt_file.
      APPEND <cur_balance> TO  gt_current_bal.
    ENDIF.

    ls_upd_rec          = <cur_balance>.
    ls_upd_rec-amount   = cur_amt.
    lv_bal              = lv_amt.
    tot_amt             = tot_amt + lv_bal.
    IF lv_del = ''.
      APPEND ls_upd_rec TO gt_upd_recs.
    ENDIF.
  ENDLOOP.

*If amount is not balanced, abort further processing and start error handling process
  IF tot_amt NE 0.
    PERFORM error_handling USING tot_amt p_errorf.
  ELSE.
* If amount is balanced then write log files
    PERFORM write_log_files USING p_logf.
  ENDIF.

ENDFORM.                    " FIND_DELTAS
*&---------------------------------------------------------------------*
*&      Form  POPULATE_HISTORY_REC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FAGLFLEXT>  text
*      -->P_LV_PERIOD  text
*      -->P_<VAL>  text
*      -->P_LS_CUR_DATA_CURRENCY  text
*      <--P_LS_CUR_DATA  text
*----------------------------------------------------------------------*
FORM populate_history_rec  USING ps_faglflext TYPE ty_delta
                           CHANGING ps_data   TYPE LINE OF ty_t_zfi_trial_bal.


  FIELD-SYMBOLS: <t001> LIKE LINE OF gt_t001.

  ps_data-ledger_name      = 'SAP UG GAAP Ledger'.
  CONCATENATE 'SAP_UG_AB_' ps_faglflext-rbukrs '_' sy-datum '_'
              sy-uzeit INTO ps_data-transaction_number.

  ps_data-company_code      = ps_faglflext-rbukrs.   " Company Code
  ps_data-account           = ps_faglflext-racct.    " Account Number
  ps_data-h_variable_1      = ''.
  ps_data-h_variable_2      = ''.
  ps_data-h_variable_3      = ''.
  ps_data-l_variable_1      = ''.
  ps_data-l_variable_2      = ''.
  ps_data-l_variable_3      = ''.
  ps_data-run_date          = sy-datum.
  ps_data-timestamp         = sy-uzeit.
  ps_data-zuser             = sy-uname.
  ps_data-currency          = ps_faglflext-rtcur.


  IF ps_faglflext-rcntr IS NOT INITIAL AND ps_faglflext-rcntr NE ''.       " Cost center
    SHIFT ps_faglflext-rcntr LEFT DELETING LEADING '0'.
    ps_data-cost_center = ps_faglflext-rcntr.
  ELSE.
    ps_data-cost_center = '0000000000'.
  ENDIF.

  ps_data-co_order        = '000000'.
  ps_data-financial_asset = '000000000000000'.
  ps_data-wbs             = '000000000000000'.

ENDFORM.                    " POPULATE_HISTORY_REC
*&---------------------------------------------------------------------*
*&      Form  SCRN_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scrn_validation .

  DATA: lv_answer TYPE c.

  IF cb_bclr EQ 'X' AND cb_clrd EQ 'X'..
    MESSAGE 'Select either "Delete Tracking Table for Desired Period" OR "Delete Balance Tracking History Completely"' TYPE 'E'.
  ENDIF.

* When saved variant is pulled up and 'SAVE' button is pushed, pop-up window appears
* so to stop the pop-up window "sy-ucomm NE 'SPOS'" is checked
  IF cb_bclr EQ 'X' AND sy-ucomm NE 'SPOS'.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = text-011 "Confirm Deletion of History Table Completely'
        text_question         = text-012 "'Are you sure......'
        text_button_1         = text-013
        text_button_2         = text-014
        display_cancel_button = ''
      IMPORTING
        answer                = lv_answer.
    IF lv_answer EQ '1'. "Yes

    ELSE. "No
      CLEAR cb_bclr.
    ENDIF.
  ENDIF.

ENDFORM.                    " SCRN_VALIDATION
*&---------------------------------------------------------------------*
*&      Form  FIND_DUPLICATE_RECS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<CUR_BAL>  text
*----------------------------------------------------------------------*
FORM find_duplicate_recs  USING cur_bal TYPE zfi_trial_bal.

  IF account           = cur_bal-account           AND
     reporting_period  = cur_bal-reporting_period  AND
     company_code      = cur_bal-company_code      AND
     cost_center       = cur_bal-cost_center       AND
     currency          = cur_bal-currency          AND
     co_order          = cur_bal-co_order          AND
     pm_order          = cur_bal-pm_order          AND
     wbs               = cur_bal-wbs.

    WRITE: / cur_bal-account,
             cur_bal-reporting_period,
             cur_bal-company_code,
             cur_bal-cost_center,
             cur_bal-currency,
             cur_bal-co_order,
             cur_bal-pm_order,
             cur_bal-wbs,
             cur_bal-amount.
  ENDIF.

  account           = cur_bal-account.
  reporting_period  = cur_bal-reporting_period.
  company_code      = cur_bal-company_code.
  cost_center       = cur_bal-cost_center.
  currency          = cur_bal-currency.
  co_order          = cur_bal-co_order.
  pm_order          = cur_bal-pm_order.
  wbs               = cur_bal-wbs.

ENDFORM.                    " FIND_DUPLICATE_RECS
*&---------------------------------------------------------------------*
*&      Form  ERROR_HANDLING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TOT_AMT  text
*      -->P_P_ERRORF  text
*----------------------------------------------------------------------*
FORM error_handling  USING p_totamt TYPE hslvt12 p_path TYPE any.

  DATA: lv_file         TYPE string,
        lv_amt          TYPE text25,
        lv_timestamp    TYPE text30,
        lv_hsl(5)       TYPE c,
        lv_hsl_count(2) TYPE n,
        lt_mergred_recs TYPE truxs_t_text_data,
        lt_hist_tab     TYPE truxs_t_text_data,
        lt_raw_flext    TYPE truxs_t_text_data,
        ls_data         TYPE LINE OF truxs_t_text_data,
        ls_file_data    TYPE LINE OF truxs_t_text_data,
        htab            TYPE abap_char1 VALUE cl_abap_char_utilities=>horizontal_tab.


  FIELD-SYMBOLS:  <faglflext>   LIKE LINE OF gt_raw_faglflext,
                  <val>         TYPE any,
                  <cur_balance> LIKE LINE OF gt_cur_trial_bal,
                  <hist_tab>    LIKE LINE OF gt_hist_trial_bal.

  WRITE: / 'Sum of debits and credits is not balanced - ', p_totamt.


*********************Write error file
  IF gt_usfile[] IS NOT INITIAL.
    PERFORM build_header USING ls_file_data gt_usfile[].
    PERFORM write_error_file USING gt_usfile lv_file '' '.csv' 'USD' p_path.
    WRITE:/ 'Error US File Location:'.
    WRITE:/ lv_file.
  ENDIF.

  IF gt_file[] IS NOT INITIAL.
    PERFORM build_header USING ls_file_data gt_file[].
    PERFORM write_error_file USING gt_file lv_file '' '.csv' 'CAD' p_path.
    WRITE:/ 'Error Canada File Location:'.
    WRITE:/ lv_file.
  ENDIF.

******************** Write merged FAGLFLEXT Table
  IF gt_cur_trial_bal[] IS NOT  INITIAL.
    LOOP AT gt_cur_trial_bal ASSIGNING <cur_balance>.
      lv_amt = <cur_balance>-amount.
      SHIFT lv_amt LEFT DELETING LEADING space.
      CONCATENATE <cur_balance>-mandt
                  <cur_balance>-account
                 <cur_balance>-reporting_period
                 <cur_balance>-company_code
                 <cur_balance>-cost_center
                 <cur_balance>-currency
                 <cur_balance>-co_order
                 <cur_balance>-pm_order
                 <cur_balance>-wbs
                 <cur_balance>-run_date
                 <cur_balance>-timestamp
                 <cur_balance>-ledger_name
                 <cur_balance>-transaction_number
                 <cur_balance>-effective_date
                 <cur_balance>-statistical_order
                 <cur_balance>-network_activity
                 <cur_balance>-financial_asset
                 lv_amt
                 <cur_balance>-h_variable_1
                 <cur_balance>-h_variable_2
                 <cur_balance>-h_variable_3
                 <cur_balance>-l_variable_1
                 <cur_balance>-l_variable_2
                 <cur_balance>-l_variable_3
                 <cur_balance>-zuser
                 INTO ls_data SEPARATED BY htab.
      APPEND ls_data TO lt_mergred_recs.
    ENDLOOP.

    PERFORM build_header_error USING ls_file_data lt_mergred_recs[] 'M'.
    PERFORM write_error_file USING lt_mergred_recs lv_file '_GT_CUR_TRIAL_BAL' '.txt' '' p_path.
    WRITE:/ 'Current Trial Balance Records:'.
    WRITE:/ lv_file.
  ENDIF.

***************************** Write RAW FAGLFLEXT Table
  IF gt_raw_faglflext[] IS NOT INITIAL.
    LOOP AT gt_raw_faglflext ASSIGNING <faglflext>.
      CLEAR: ls_data.
      CONCATENATE  <faglflext>-ryear
                   <faglflext>-racct
                   <faglflext>-rbukrs
                   <faglflext>-rcntr
                   <faglflext>-rtcur
                   <faglflext>-prctr INTO  ls_data SEPARATED BY htab.

      ASSIGN COMPONENT 'HSLVT' OF STRUCTURE <faglflext>  TO <val>.
      lv_amt = <val>.
      SHIFT lv_amt LEFT DELETING LEADING space.
      CONCATENATE ls_data lv_amt INTO  ls_data SEPARATED BY htab.
      CLEAR: lv_amt, lv_hsl_count.
      UNASSIGN <val>.
      DO 16 TIMES.
        lv_hsl_count = lv_hsl_count + 1.
        CLEAR: lv_amt.
        UNASSIGN <val>.
        CONCATENATE 'HSL' lv_hsl_count INTO lv_hsl.
        ASSIGN COMPONENT lv_hsl OF STRUCTURE <faglflext> TO <val>.
        CHECK <val> IS ASSIGNED.
        lv_amt = <val>.
        SHIFT lv_amt LEFT DELETING LEADING space.
        CONCATENATE ls_data lv_amt INTO ls_data SEPARATED BY htab.
      ENDDO.
      lv_timestamp = <faglflext>-timestamp.
      SHIFT  lv_timestamp LEFT DELETING LEADING space.
      CONCATENATE ls_data lv_timestamp INTO ls_data SEPARATED BY htab.
      APPEND ls_data TO lt_raw_flext.
    ENDLOOP.

    PERFORM build_header_error USING ls_file_data lt_raw_flext[] 'R'.
    PERFORM write_error_file USING lt_raw_flext lv_file '_LT_FAGLFLEXT' '.txt' '' p_path.
    WRITE:/ 'File Path - Records selection from FAGLFLEXT'.
    WRITE:/ lv_file.
  ENDIF.
***************************** Write History table
  IF gt_hist_trial_bal[] IS NOT INITIAL.
    LOOP AT gt_hist_trial_bal ASSIGNING <hist_tab>.
      lv_amt = <hist_tab>-amount.
      SHIFT lv_amt LEFT DELETING LEADING space.
      CONCATENATE <hist_tab>-mandt
                  <hist_tab>-account
                  <hist_tab>-reporting_period
                  <hist_tab>-company_code
                  <hist_tab>-cost_center
                  <hist_tab>-currency
                  <hist_tab>-co_order
                  <hist_tab>-pm_order
                  <hist_tab>-wbs
                  <hist_tab>-run_date
                  <hist_tab>-timestamp
                  <hist_tab>-ledger_name
                  <hist_tab>-transaction_number
                  <hist_tab>-effective_date
                  <cur_balance>-statistical_order
                  <hist_tab>-network_activity
                  <hist_tab>-financial_asset
                   lv_amt
                  <hist_tab>-h_variable_1
                  <hist_tab>-h_variable_2
                  <hist_tab>-h_variable_3
                  <hist_tab>-l_variable_1
                  <hist_tab>-l_variable_2
                  <hist_tab>-l_variable_3
                  <hist_tab>-zuser INTO ls_data SEPARATED BY htab.
      APPEND ls_data TO lt_hist_tab.
    ENDLOOP.
    PERFORM build_header_error USING ls_file_data lt_hist_tab[] 'M'.
    PERFORM write_error_file USING lt_hist_tab lv_file '_ZFI_TRIAL_BAL' '.txt' '' p_path.
    WRITE:/ 'File Path - Records Sent on previous run'.
    WRITE:/ lv_file.
  ENDIF.

  MESSAGE a001(00) WITH 'Sum of debits and credits is not balanced'.

ENDFORM.                    " ERROR_HANDLING
*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_FILE_DATA  text
*      -->P_LT_MERGRED_RECS[]  text
*      -->P_2549   text
*----------------------------------------------------------------------*
FORM build_header_error  USING    ps_data TYPE any
                                pt_data TYPE STANDARD TABLE
                                p_flag  TYPE any .

  DATA: htab TYPE abap_char1 VALUE cl_abap_char_utilities=>horizontal_tab.

  CLEAR: ps_data.

  IF p_flag EQ 'M'.

    CONCATENATE 'Client'
'Account Number'
'Reporting Period'
'Company Code'
'Cost Center'
'Currency Key'
'CO Order'
'PM Order'
'WBS'
'Run Date'
'Run Time'
'Ledger Name'
'Transaction Number'
'Effective Date'
'Statistical Order'
'Network/Activity'
'Financial Asset'
'Amount'
'H VARIABLE 1'
'H VARIABLE 2'
'H VARIABLE 3'
'L VARIABLE 1'
'L VARIABLE 2'
'L VARIABLE 3'
'User Name' INTO ps_data SEPARATED BY htab.

    INSERT ps_data INTO pt_data INDEX 1.

  ELSEIF p_flag EQ 'R'.

    CONCATENATE 'Fiscal Year'
    'Account Number'
    'Company Code'
    'Cost Center'
    'Currency Key'
    'Profit Center'
    'Balance carried forward'
    'Period Amount 1'
    'Period Amount 2'
    'Period Amount 3'
    'Period Amount 4'
    'Period Amount 5'
    'Period Amount 6'
    'Period Amount 7'
    'Period Amount 8'
    'Period Amount 9'
    'Period Amount 10'
    'Period Amount 11'
    'Period Amount 12'
    'Period Amount 13'
    'Period Amount 14'
    'Period Amount 15'
    'Period Amount 16'
    'Timestamp' INTO ps_data SEPARATED BY htab.

    INSERT ps_data INTO pt_data INDEX 1.

  ENDIF.

ENDFORM.                    " BUILD_HEADER_ERROR
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MERGRED_RECS  text
*      -->P_LV_FILE  text
*      -->P_2555   text
*      -->P_2556   text
*      -->P_P_PATH  text
*----------------------------------------------------------------------*
FORM write_error_file  USING    pt_data  TYPE truxs_t_text_data
                            p_fname     TYPE any
                            p_name      TYPE any
                            p_extension TYPE any
                            p_usdcad    TYPE any
                            p_path      TYPE any.

  DATA: lv_appfile         TYPE string,
        lv_file             TYPE string,
        lv_error(75)    TYPE c,
        lv_filename     TYPE localfile,
        lv_param_1      TYPE text15,
        lv_param_3      TYPE text15,
        lv_len          TYPE i,
        lv_offset       TYPE i,
        lv_extension(5) TYPE c,
        lv_name         TYPE text30.

  FIELD-SYMBOLS: <file> LIKE LINE OF gt_file.

  CLEAR p_fname.
  IF rd_pc EQ 'X'. " PC file
    IF pt_data[] IS NOT INITIAL.
      lv_file = p_file.
      lv_len = strlen( lv_file ).
      lv_offset = lv_len - 4.

      lv_extension = lv_file+lv_offset(4).
      CONCATENATE p_name p_extension INTO lv_name.
      REPLACE ALL OCCURRENCES OF lv_extension IN lv_file WITH lv_name.
      IF p_usdcad EQ 'USD'.
        REPLACE 'WAERS' IN lv_file WITH 'USD'.
      ELSEIF p_usdcad EQ 'CAD'.
        REPLACE 'WAERS' IN lv_file WITH 'CAD'.
      ENDIF.
      p_fname = lv_file.
      PERFORM write_pc_file USING pt_data[] lv_file.
    ENDIF.
  ELSE.
    IF pt_data[] IS NOT INITIAL.
      lv_file = p_path.
      CONCATENATE p_name p_extension INTO lv_name.
      REPLACE ALL OCCURRENCES OF '.EXT' IN lv_file WITH lv_name.
      IF p_usdcad EQ 'USD'.
        REPLACE 'WAERS' IN lv_file WITH 'USD'.
      ELSEIF p_usdcad EQ 'CAD'.
        REPLACE 'WAERS' IN lv_file WITH 'CAD'.
      ENDIF.
      lv_appfile = lv_file.
      p_fname    = lv_file.
      OPEN DATASET lv_appfile FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc EQ 0.
        LOOP AT pt_data ASSIGNING <file>.
          TRANSFER <file> TO lv_appfile.
        ENDLOOP.
      ELSE.
        WRITE: / lv_error , ' - ',  lv_appfile.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " WRITE_ERROR_FILE
*&---------------------------------------------------------------------*
*&      Form  WRITE_LOG_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LOGF  text
*----------------------------------------------------------------------*
FORM write_log_files  USING   p_path TYPE any.

  DATA: lv_file         TYPE string,
        lv_amt          TYPE text25,
        lv_timestamp    TYPE text30,
        lv_hsl(5)       TYPE c,
        lv_hsl_count(2) TYPE n,
        lt_mergred_recs TYPE truxs_t_text_data,
        lt_hist_tab     TYPE truxs_t_text_data,
        lt_raw_flext    TYPE truxs_t_text_data,
        ls_data         TYPE LINE OF truxs_t_text_data,
        ls_file_data    TYPE LINE OF truxs_t_text_data,
        htab            TYPE abap_char1 VALUE cl_abap_char_utilities=>horizontal_tab.


  FIELD-SYMBOLS:  <faglflext>   LIKE LINE OF gt_raw_faglflext,
                  <val>         TYPE any,
                  <cur_balance> LIKE LINE OF gt_cur_trial_bal,
                  <hist_tab>    LIKE LINE OF gt_hist_trial_bal.


*********************Write error file
  IF gt_usfile[] IS NOT INITIAL.
    PERFORM build_header USING ls_file_data gt_usfile[].
    PERFORM write_error_file USING gt_usfile lv_file '' '.csv' 'USD' p_path.
    WRITE:/ 'US File Location:'.
    WRITE:/ lv_file.
  ENDIF.

  IF gt_file[] IS NOT INITIAL.
    PERFORM build_header USING ls_file_data gt_file[].
    PERFORM write_error_file USING gt_file lv_file '' '.csv' 'CAD' p_path.
    WRITE:/ 'Canada File Location:'.
    WRITE:/ lv_file.
  ENDIF.

******************** Write merged FAGLFLEXT Table
  IF gt_cur_trial_bal[] IS NOT INITIAL.
    LOOP AT gt_cur_trial_bal ASSIGNING <cur_balance>.
      lv_amt = <cur_balance>-amount.
      SHIFT lv_amt LEFT DELETING LEADING space.
      CONCATENATE <cur_balance>-mandt
                  <cur_balance>-account
                 <cur_balance>-reporting_period
                 <cur_balance>-company_code
                 <cur_balance>-cost_center
                 <cur_balance>-currency
                 <cur_balance>-co_order
                 <cur_balance>-pm_order
                 <cur_balance>-wbs
                 <cur_balance>-run_date
                 <cur_balance>-timestamp
                 <cur_balance>-ledger_name
                 <cur_balance>-transaction_number
                 <cur_balance>-effective_date
                 <cur_balance>-statistical_order
                 <cur_balance>-network_activity
                 <cur_balance>-financial_asset
                 lv_amt
                 <cur_balance>-h_variable_1
                 <cur_balance>-h_variable_2
                 <cur_balance>-h_variable_3
                 <cur_balance>-l_variable_1
                 <cur_balance>-l_variable_2
                 <cur_balance>-l_variable_3
                 <cur_balance>-zuser
                 INTO ls_data SEPARATED BY htab.
      APPEND ls_data TO lt_mergred_recs.
    ENDLOOP.

    PERFORM build_header_error USING ls_file_data lt_mergred_recs[] 'M'.
    PERFORM write_error_file USING lt_mergred_recs lv_file '_GT_CUR_TRIAL_BAL' '.txt' '' p_path.
    WRITE:/ 'Current Trial Balance Records:'.
    WRITE:/ lv_file.
  ENDIF.

***************************** Write RAW FAGLFLEXT Table
  IF gt_raw_faglflext[] IS NOT INITIAL.
    LOOP AT gt_raw_faglflext ASSIGNING <faglflext>.
      CLEAR: ls_data.
      CONCATENATE  <faglflext>-ryear
                   <faglflext>-racct
                   <faglflext>-rbukrs
                   <faglflext>-rcntr
                   <faglflext>-rtcur
                   <faglflext>-prctr INTO  ls_data SEPARATED BY htab.

      ASSIGN COMPONENT 'HSLVT' OF STRUCTURE <faglflext>  TO <val>.
      lv_amt = <val>.
      SHIFT lv_amt LEFT DELETING LEADING space.
      CONCATENATE ls_data lv_amt INTO  ls_data SEPARATED BY htab.
      CLEAR: lv_amt, lv_hsl_count.
      UNASSIGN <val>.
      DO 16 TIMES.
        lv_hsl_count = lv_hsl_count + 1.
        CLEAR: lv_amt.
        UNASSIGN <val>.
        CONCATENATE 'HSL' lv_hsl_count INTO lv_hsl.
        ASSIGN COMPONENT lv_hsl OF STRUCTURE <faglflext> TO <val>.
        CHECK <val> IS ASSIGNED.
        lv_amt = <val>.
        SHIFT lv_amt LEFT DELETING LEADING space.
        CONCATENATE ls_data lv_amt INTO ls_data SEPARATED BY htab.
      ENDDO.
      lv_timestamp = <faglflext>-timestamp.
      SHIFT  lv_timestamp LEFT DELETING LEADING space.
      CONCATENATE ls_data lv_timestamp INTO ls_data SEPARATED BY htab.
      APPEND ls_data TO lt_raw_flext.
    ENDLOOP.

    PERFORM build_header_error USING ls_file_data lt_raw_flext[] 'R'.
    PERFORM write_error_file USING lt_raw_flext lv_file '_LT_FAGLFLEXT' '.txt' '' p_path.
    WRITE:/ 'File Path - Records selection from FAGLFLEXT'.
    WRITE:/ lv_file.
  ENDIF.

***************************** Write History table
  IF gt_hist_trial_bal[] IS NOT INITIAL.
    LOOP AT gt_hist_trial_bal ASSIGNING <hist_tab>.

      lv_amt = <hist_tab>-amount.
      SHIFT lv_amt LEFT DELETING LEADING space.

      CONCATENATE <hist_tab>-mandt
                  <hist_tab>-account
                  <hist_tab>-reporting_period
                  <hist_tab>-company_code
                  <hist_tab>-cost_center
                  <hist_tab>-currency
                  <hist_tab>-co_order
                  <hist_tab>-pm_order
                  <hist_tab>-wbs
                  <hist_tab>-run_date
                  <hist_tab>-timestamp
                  <hist_tab>-ledger_name
                  <hist_tab>-transaction_number
                  <hist_tab>-effective_date
                  <cur_balance>-statistical_order
                  <hist_tab>-network_activity
                  <hist_tab>-financial_asset
                   lv_amt
                  <hist_tab>-h_variable_1
                  <hist_tab>-h_variable_2
                  <hist_tab>-h_variable_3
                  <hist_tab>-l_variable_1
                  <hist_tab>-l_variable_2
                  <hist_tab>-l_variable_3
                  <hist_tab>-zuser INTO ls_data SEPARATED BY htab.
      APPEND ls_data TO lt_hist_tab.
    ENDLOOP.
    PERFORM build_header_error USING ls_file_data lt_hist_tab[] 'M'.
    PERFORM write_error_file USING lt_hist_tab lv_file '_ZFI_TRIAL_BAL' '.txt' '' p_path.
    WRITE:/ 'File Path - Records Sent on previous run'.
    WRITE:/ lv_file.
  ENDIF.

ENDFORM.                    " WRITE_LOG_FILES
