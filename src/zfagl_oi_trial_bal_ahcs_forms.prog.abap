*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_AHCS_FORMS
*&---------------------------------------------------------------------*

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

  IF sy-sysid EQ 'RDC'.
    lv_param_1 = 'DEV'.
    CONCATENATE 'US_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'RQC'.
    lv_param_1 = 'QA'.
    CONCATENATE 'US_PR_QA_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'PEC'.
    lv_param_1 = 'PRD'.
    CONCATENATE 'US_PR_PRD_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'S02'.
    lv_param_1 = 'DEV'.
    CONCATENATE 'UG_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'Q02'.
    lv_param_1 = 'QA'.
    CONCATENATE 'UG_PR_QA_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'P01'.
    lv_param_1 = 'PRD'.
    CONCATENATE 'UG_PR_PRD_' sy-sysid INTO lv_param_3.
  ENDIF.

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
FORM process_ug_file  USING     "ps_faglflext TYPE faglflext
                                ps_faglflext TYPE ty_ytd
                                p_period     TYPE any
                                p_amt        TYPE any
                       CHANGING ps_data      TYPE any.

*  FIELD-SYMBOLS: <ccm> LIKE LINE OF gt_ccm.

  IF ps_faglflext-rcntr IS NOT INITIAL AND ps_faglflext-rcntr NE ''.       " Cost center
    SHIFT ps_faglflext-rcntr LEFT DELETING LEADING '0'.
    CONCATENATE ps_data ps_faglflext-rcntr INTO ps_data SEPARATED BY ','.
  ELSE.
    CONCATENATE ps_data '0000000000' INTO ps_data SEPARATED BY ','.
  ENDIF.

*  READ TABLE gt_ccm ASSIGNING <ccm>
*                    WITH KEY gl_no = ps_faglflext-racct(1)
*                    BINARY SEARCH.
*  IF sy-subrc EQ 0.
*    CONCATENATE ps_data <ccm>-kostl INTO ps_data SEPARATED BY ','.
*  ELSE.
*    CONCATENATE ps_data '0000000000' INTO ps_data SEPARATED BY ','.
*  ENDIF.

  CONCATENATE ps_data '000000'          INTO ps_data SEPARATED BY ','.      " CO_ORDER
  CONCATENATE ps_data '000000000000000' INTO ps_data SEPARATED BY ','.      " Financial Asset
  CONCATENATE ps_data '000000000000000' INTO ps_data SEPARATED BY ','.      " WBS
  CONCATENATE ps_data
              p_amt                                                         " Amount
              space
              space
              space
              space
              space
              space
              INTO ps_data SEPARATED BY ','.         " Profit Center
ENDFORM.                    " PROCESS_UG_FILE
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
*&      Form  MERGE_RECORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM merge_records.

  DATA: lt_faglflext TYPE STANDARD TABLE OF ty_ytd,
        lv_bukrs     TYPE bukrs,
        lv_acct      TYPE racct,
        lv_hsl(5)    TYPE c,
        tot_val      TYPE hslvt12,
        lv_tabix(2)  TYPE n.

  FIELD-SYMBOLS: <faglflext> LIKE LINE OF lt_faglflext,
                 <fagl>      LIKE LINE OF gt_faglflext,
                 <prev_val>  TYPE any,
                 <val>         TYPE any,
                 <prev_hslvt>  TYPE any,
                 <val_hslvt>   TYPE any.

  REFRESH: gt_faglflext.

  SELECT ryear rpmax rtcur racct rbukrs rcntr prctr rassc hslvt
         hsl01 hsl02 hsl03 hsl04 hsl05 hsl06 hsl07 hsl08
         hsl09 hsl10 hsl11 hsl12 hsl13 hsl14 hsl15 hsl16 timestamp
         INTO TABLE lt_faglflext
         FROM faglflext
         WHERE ryear  EQ p_ryear
         AND   rpmax  IN s_rpmax
         AND   rldnr  IN s_rldnr
         AND   racct  IN s_racct
         AND   rbukrs IN s_rbukrs.

  IF sy-subrc EQ 0.
    SORT lt_faglflext BY racct rbukrs ASCENDING timestamp DESCENDING.
    LOOP AT lt_faglflext ASSIGNING <faglflext>.
      IF lv_bukrs = <faglflext>-rbukrs AND lv_acct  = <faglflext>-racct.
        CLEAR: lv_tabix.
        DO 16 TIMES.
          lv_tabix = lv_tabix + 1.
          CONCATENATE 'HSL' lv_tabix INTO lv_hsl.
          ASSIGN COMPONENT lv_hsl OF STRUCTURE <fagl>      TO <prev_val>.
          ASSIGN COMPONENT lv_hsl OF STRUCTURE <faglflext> TO <val>.
          IF <val> IS NOT INITIAL.
            <prev_val> = <prev_val> + <val>.
          ENDIF.
        ENDDO.
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
    ENDLOOP.
  ENDIF.

ENDFORM.                    " MERGE_RECORDS
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS_YTD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_process_ytd .

  DATA: lv_hsl(5)    TYPE c,
        lv_tabix(2)  TYPE n,
        lt_months    TYPE STANDARD TABLE OF t247,
        lv_period(8) TYPE c,
        lv_month     TYPE text20,
        lv_effdate   TYPE char10,
        tot_val      TYPE hslvt12,
        lv_currency  TYPE waers,
        ls_data      TYPE LINE OF truxs_t_text_data,
        lv_amt       TYPE text30,
        lv_transac   TYPE text40,
        lv_str       TYPE string,
        lv_prctr     TYPE string.

  FIELD-SYMBOLS: <faglflext> LIKE LINE OF gt_faglflext,
                 <last_day>  LIKE LINE OF gt_last_days,
                 <data>      LIKE LINE OF gt_data,
                 <t001>      LIKE LINE OF gt_t001,
                 <val>       TYPE any,
                 <month>     LIKE LINE OF lt_months.

  PERFORM merge_records.

  IF sy-subrc EQ 0.
    SELECT bukrs waers INTO TABLE gt_t001 FROM t001.
    SORT gt_t001.
    PERFORM get_last_days.
    LOOP AT gt_faglflext ASSIGNING <faglflext>.
      CLEAR: tot_val, lv_tabix, lv_currency.
      DO 16 TIMES.
        lv_tabix = lv_tabix + 1.
        CONCATENATE 'HSL' lv_tabix INTO lv_hsl.
        ASSIGN COMPONENT lv_hsl OF STRUCTURE <faglflext> TO <val>.
        IF <val> IS NOT INITIAL.
          tot_val = tot_val + <val>.
        ENDIF.
      ENDDO.

      tot_val = tot_val + <faglflext>-hslvt.

      IF tot_val IS NOT INITIAL.
        IF tot_val > 0.
          lv_amt  = tot_val.
          SHIFT lv_amt LEFT DELETING LEADING space.
        ELSE.
          tot_val = tot_val * -1.
          lv_amt  = tot_val.
          SHIFT lv_amt LEFT DELETING LEADING space.
          CONCATENATE '-' lv_amt INTO lv_amt.
        ENDIF.
        CONCATENATE '' lv_amt '' INTO lv_amt.
        READ TABLE gt_last_days ASSIGNING <last_day>
                                WITH KEY month = <faglflext>-rpmax+1(2)
                                BINARY SEARCH.
        IF sy-subrc EQ 0.
          CONCATENATE p_ryear '-' <last_day>-month '-' <last_day>-day INTO lv_effdate.
        ELSE.
          CONCATENATE p_ryear '-12-31' INTO lv_effdate.
        ENDIF.

        CONCATENATE 'SAPUGAB_' <faglflext>-rbukrs '_' sy-datum+2(6) sy-uzeit INTO lv_transac.
        CONCATENATE <faglflext>-rpmax '.' p_ryear INTO lv_period.
        SHIFT lv_period LEFT DELETING LEADING '0'.
        SHIFT <faglflext>-racct LEFT DELETING LEADING '0'.

        READ TABLE gt_t001 ASSIGNING <t001>
                           WITH KEY bukrs = <faglflext>-rbukrs
                           BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_currency = <t001>-waers.
        ENDIF.

        CONCATENATE 'SAP UG GAAP Ledger'
                     lv_transac
                     lv_period
                     lv_effdate
                     lv_currency
                     <faglflext>-rbukrs INTO ls_data SEPARATED BY ','.  " Company code.

        SHIFT <faglflext>-racct LEFT DELETING LEADING '0'.              " Account number
        CONCATENATE ls_data <faglflext>-racct INTO ls_data SEPARATED BY ','.
        PERFORM process_ug_file USING <faglflext> lv_period lv_amt CHANGING ls_data.
        IF <faglflext>-rbukrs IN s_usco.
          APPEND ls_data TO  gt_usfile.
        ELSE.
          APPEND ls_data TO gt_file.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF gt_file[] IS NOT INITIAL.
    PERFORM build_header USING ls_data gt_file[].
  ENDIF.

  IF gt_usfile[] IS NOT INITIAL.
    PERFORM build_header USING ls_data gt_usfile[].
  ENDIF.

ENDFORM.                    " READ_PROCESS_YTD
