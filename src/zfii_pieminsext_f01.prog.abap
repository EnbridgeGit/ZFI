*----------------------------------------------------------------------*
* Report Name: ZFII_PIEMINSEXT_F01
* Author:	     KBANERJEE-Kaushiki Banerjee
* Date:	       October 22nd,2018
*
* Logical Database: NA
* SAPScript name:   NA
* Application Area: FI
* Description:  This tool automates the process of sending information
*               like posted travel expenses, including all detailed
*               information available in the current posting run to
*               Premier Insights vendor.
*               This report has following execution modes
*               1.Display the report
*               2.Download the output in local file presentation server
*               3.Download the output in application server
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 22-OCT-2018  KBANERJEE   D30K929247  CHG0125087:Initial development  *
* 18-SEP-2019  SHAFFES     DECK920174  CHG0159701: Exclude negative    *
*                                      Amount to Premier Insights      *
*                                      vendor. GL Acc: 251040 related  *
*                                      data is ignored from extract    *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
*       CLASS lcl_alv_event DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*Get the global data of class from class constructor method
CLASS lcl_alv_event DEFINITION.
  PUBLIC SECTION .
    METHODS:check_sel_rows  IMPORTING iv_ucomm   TYPE sy-ucomm
                            EXPORTING et_rows    TYPE lvc_t_row
                                      ev_failure TYPE char1.
ENDCLASS.                    "lcl_alv_event DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_alv_event IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_event IMPLEMENTATION.
  METHOD check_sel_rows.
    DATA: lt_rows TYPE lvc_t_row.
*--- Get selected Rows
    o_alvgd->get_selected_rows(
      IMPORTING
        et_index_rows = lt_rows ).    " Indexes of Selected Rows

    IF lt_rows IS INITIAL.
      MESSAGE s000(zfi01) WITH text-e04 DISPLAY LIKE if_xo_const_message=>error.
      ev_failure = abap_true.
      RETURN.
    ELSE.
      et_rows = lt_rows.
    ENDIF.
  ENDMETHOD.                    "check_sel_rows
ENDCLASS.                    "lcl_alv_event IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_FCAT
*&---------------------------------------------------------------------*
*  Modifying field descriptions in Fieldcatalog
*----------------------------------------------------------------------*
*      -->PI_REPTEXT   Field text for display
*      -->PI_STEXT     Short text for field names
*      -->PI_MTEXT     Middle text for field names
*      -->PI_LTEXT     Long text for field names
*      -->PI_FNAME     Field Names
*      <-->PC_FIELDCAT Fieldctalog that is changed
*----------------------------------------------------------------------*
FORM f_modify_fcat  USING  pi_reptext  TYPE reptext
                           pi_stext    TYPE scrtext_s
                           pi_mtext    TYPE scrtext_m
                           pi_ltext    TYPE scrtext_l
                           pi_fname    TYPE lvc_fname
                  CHANGING pc_fieldcat TYPE lvc_s_fcat.
  CLEAR pc_fieldcat.
  pc_fieldcat-reptext   = pi_reptext.
  pc_fieldcat-scrtext_s = pi_stext.
  pc_fieldcat-scrtext_m = pi_mtext.
  pc_fieldcat-scrtext_l = pi_ltext.
  pc_fieldcat-fieldname = pi_fname.
ENDFORM.                    "f_modify_fcat
*&---------------------------------------------------------------------*
*&      Form  F_FLD_NODISP
*&---------------------------------------------------------------------*
*  Modifying field descriptions in Fieldcatalog
*----------------------------------------------------------------------*
*      -->PI_FNAME     Field Names
*      <-->PC_FIELDCAT Fieldctalog that is changed
*----------------------------------------------------------------------*
FORM f_fld_nodisp  USING  pi_fname    TYPE lvc_fname
                CHANGING pc_fieldcat  TYPE lvc_s_fcat.
  CLEAR pc_fieldcat.
  pc_fieldcat-fieldname = pi_fname.
  pc_fieldcat-no_out    = gc_set.
  pc_fieldcat-tech      = gc_set.
ENDFORM.                    "f_fld_nodisp
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FCAT
*&---------------------------------------------------------------------*
*   Build fieldcatalog for ALV display
*----------------------------------------------------------------------*
*      -->pi_lwa_dfies     Field characteristics
*      <--pc_lwa_fieldcat  Fieldcatalog
*----------------------------------------------------------------------*
FORM f_build_fcat  USING    pi_lwa_dfies    TYPE dfies
                   CHANGING pc_lwa_fieldcat TYPE lvc_s_fcat.
  CASE pi_lwa_dfies-fieldname.
    WHEN 'PERNR'."Personnel Number
      PERFORM f_modify_fcat USING text-h01 text-h02 text-h03 text-h03 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'RUNID'.
      PERFORM f_fld_nodisp USING pi_lwa_dfies-fieldname
                        CHANGING pc_lwa_fieldcat."Runid no display
    WHEN 'EXBEL'."Assignment
      PERFORM f_modify_fcat USING text-h42 text-h43 text-h43 text-h43 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'BUKRS'."Comp.Code
      PERFORM f_modify_fcat USING text-h04 text-h05 text-h05 text-h06 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'BLART'."Blg
      PERFORM f_modify_fcat USING text-h44 text-h44 text-h44 text-h44 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'BUDAT'."Posting Date
      PERFORM f_modify_fcat USING text-h08 text-h09 text-h45 text-h45 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'KTOSL'."Transactn
      PERFORM f_modify_fcat USING text-h10 text-h11 text-h11 text-h12 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'HKONT'."G/L Accnt
      PERFORM f_modify_fcat USING text-h13 text-h14 text-h14  text-h15 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'HKONTTXT'."Account Name
      PERFORM f_modify_fcat USING text-h17 text-h16 text-h16 text-h17 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'WAERS'."Currency
      PERFORM f_modify_fcat USING text-h18 text-h18 text-h18 text-h19 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'WRBTR'."Amount without formatting no display
      PERFORM f_fld_nodisp USING pi_lwa_dfies-fieldname
                        CHANGING pc_lwa_fieldcat.
    WHEN 'AMOUNT'."Amount with formatting in sign
      PERFORM f_modify_fcat USING text-h20 text-h20 text-h20 text-h20 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'MWSKZ'."Tax code
      PERFORM f_modify_fcat USING text-h27 text-h27 text-h27 text-h27 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'HKONTTAX'."Tax Acct
      PERFORM f_modify_fcat USING text-h24 text-h25 text-h25 text-h26 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'STEUR_SOLL'."Tax Debit without formatting no display
      PERFORM f_fld_nodisp USING pi_lwa_dfies-fieldname
                        CHANGING pc_lwa_fieldcat.
    WHEN 'TAX_DR'."Tax Debit with sign formatted
      PERFORM f_modify_fcat USING text-h21 text-h22 text-h23 text-h23 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'STEUR_HABEN'."Tax Credit without fomratting no displaty
      PERFORM f_fld_nodisp USING pi_lwa_dfies-fieldname
                        CHANGING pc_lwa_fieldcat.
    WHEN 'TAX_CR'."Tax credit with formatting display
      PERFORM f_modify_fcat USING text-h28 text-h29 text-h30 text-h30 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'SGTXT'."Text
      PERFORM f_modify_fcat USING text-h36 text-h36 text-h36 text-h36 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'KOSTL'."Cost Ctr
      PERFORM f_modify_fcat USING text-h31 text-h32 text-h32 text-h31 pi_lwa_dfies-fieldname
                          CHANGING pc_lwa_fieldcat.
    WHEN 'AUFNR'."Order
      PERFORM f_modify_fcat USING text-h33 text-h33 text-h33 text-h33 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'POSNR'."WBS Elemnt
      PERFORM f_modify_fcat USING text-h34 text-h35 text-h35 text-h35 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'REINR'."Reference
      PERFORM f_modify_fcat USING text-h37 text-h38 text-h38 text-h38 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'MSATZ'."Tax Rate
      PERFORM f_modify_fcat USING text-h39 text-h40 text-h40 text-h40 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'LIFNR'."Vendor
      PERFORM f_modify_fcat USING text-h41 text-h41 text-h41 text-h41 pi_lwa_dfies-fieldname
                         CHANGING pc_lwa_fieldcat.
    WHEN 'AWREF'.
      PERFORM f_fld_nodisp USING pi_lwa_dfies-fieldname
                        CHANGING pc_lwa_fieldcat."Reference Document Number no display
    WHEN 'AWORG'.
      PERFORM f_fld_nodisp USING pi_lwa_dfies-fieldname
                        CHANGING pc_lwa_fieldcat."Reference Organizational Units no display
    WHEN 'AWLIN'.
      PERFORM f_fld_nodisp USING pi_lwa_dfies-fieldname
                        CHANGING pc_lwa_fieldcat."10 digit number
    WHEN OTHERS.
*Do nothing
  ENDCASE.
ENDFORM.                    "f_build_fcat
*&---------------------------------------------------------------------*
*&      Form  f_build_field_cat
*&---------------------------------------------------------------------*
*  Fieldcatalog creation
*----------------------------------------------------------------------*
FORM f_build_field_cat.
  DATA:
    lo_tabdescr  TYPE REF TO cl_abap_structdescr,
    lo_data      TYPE REF TO data,
    lt_dfies     TYPE ddfields,
    lwa_dfies    TYPE dfies,
    lwa_fieldcat TYPE lvc_s_fcat.

  CREATE DATA lo_data LIKE LINE OF gt_output.
  lo_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lo_data ).

  lt_dfies = cl_salv_data_descr=>read_structdescr( lo_tabdescr ).

  CLEAR gt_fcat.

  LOOP AT lt_dfies INTO lwa_dfies.
*Modify Field Catalog Headings for Display
    MOVE-CORRESPONDING lwa_dfies TO lwa_fieldcat.
    lwa_fieldcat-col_pos = sy-tabix.
    PERFORM f_build_fcat USING  lwa_dfies CHANGING lwa_fieldcat.
    APPEND lwa_fieldcat TO gt_fcat.
  ENDLOOP.

ENDFORM.                    "f_build_field_cat
*&---------------------------------------------------------------------*
*&      Form  F_ENABLE_FPATH
*&---------------------------------------------------------------------*
*  Options to download outout either to presentation or to
*  application server
*----------------------------------------------------------------------*
FORM f_enable_fpath.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'EXR'.
        IF cb_extrc = gc_set.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'XLS'.
        IF rb_xls = gc_set.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'APP'.
        IF rb_app = gc_set.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN OTHERS.
*Do Nothing
    ENDCASE.
  ENDLOOP.
  IF rb_app = gc_set AND p_apppth IS INITIAL.
    PERFORM f_set_appfile.
  ENDIF.
ENDFORM.                    " F_ENABLE_FPATH
*&---------------------------------------------------------------------*
*&      Form  F_FILE_F4
*&---------------------------------------------------------------------*
*      Browse path to download output to local PC
*----------------------------------------------------------------------*
FORM f_file_f4.
  DATA : lv_folder TYPE string,
         lv_title  TYPE string.
  lv_title = 'Please select a destination folder.'(t09).
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = lv_title
      initial_folder  = 'C:'
    CHANGING
      selected_folder = lv_folder.

  p_xlspth = lv_folder .
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.                    " F_FILE_F4
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
*     Refresh global structures
*----------------------------------------------------------------------*
FORM f_refresh .
  REFRESH:gt_app_filepath,
          gt_pevsh,
          gt_output.
  CLEAR gv_report.
ENDFORM.                    " F_REFRESH
*&---------------------------------------------------------------------*
*&      Form  F_APP_FILEPATH
*&---------------------------------------------------------------------*
*    Get application server path to download outpit based on
*    client/system executing the program
*----------------------------------------------------------------------*
FORM f_app_filepath.
  SELECT paramtype subtype key1 key2 key3 value1
    FROM zfit_xparam
    INTO TABLE gt_app_filepath
    WHERE paramtype = gc_paramtyp
      AND subtype   = gc_subtype
      AND key1      = gc_varname
      AND key2      = gc_key1.
  IF sy-subrc IS INITIAL.
    SORT gt_app_filepath BY paramtype subtype key1 key2 key3 value1.
  ENDIF.

ENDFORM.                    " F_APP_FILEPATH
*&---------------------------------------------------------------------*
*&      Form  F_SET_APPFILE
*&---------------------------------------------------------------------*
*   Create filename dynamically
*----------------------------------------------------------------------*
FORM f_set_appfile .
  DATA:lv_sysid  TYPE sysysid.
  DATA:ls_app_filepath  TYPE gty_app_filepath.
  lv_sysid = sy-sysid.
  IF gt_app_filepath IS NOT INITIAL.
    READ TABLE gt_app_filepath INTO ls_app_filepath
                               WITH KEY paramtype = gc_paramtyp
                                        subtype   = gc_subtype
                                        key1      = gc_varname
                                        key2      = gc_key1
                                        key3      = lv_sysid
                               BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      p_apppth = ls_app_filepath-value1.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_SET_APPFILE
*&---------------------------------------------------------------------*
*&      Form  F_FETCH_DATA
*&---------------------------------------------------------------------*
*    Fetch all data from database tables based on date entered
*----------------------------------------------------------------------*
*  <--> PCT_PEVSH   Data from PEVSH
*  <--> PCT_AWKEY   Data from PTRV_ROT_AWKEY
*----------------------------------------------------------------------*
FORM f_fetch_data CHANGING pct_pevsh TYPE tt_pevsh
                           pct_awkey TYPE tt_awkey.
  CONSTANTS:lc_tr   TYPE p_evtyp    VALUE 'TR',"Posting Travel Expenses
            lc_50   TYPE p_evstatus VALUE '50'.
  DATA:lt_pevsh TYPE STANDARD TABLE OF gty_pevsh INITIAL SIZE 0.
*Fetch Runid from History of Payroll Posting Runs
  SELECT type runid status creadate
    FROM pevsh
    INTO TABLE pct_pevsh
    WHERE type = lc_tr
     AND status = lc_50
     AND creadate IN s_crtdt.
  IF sy-subrc IS INITIAL.
    SORT pct_pevsh BY runid.
    lt_pevsh = pct_pevsh.
    DELETE ADJACENT DUPLICATES FROM lt_pevsh COMPARING runid.
  ENDIF.
  IF lt_pevsh IS NOT INITIAL.
    SELECT pernr reinr runid awref aworg awlin
      FROM ptrv_rot_awkey
      INTO TABLE pct_awkey
      FOR ALL ENTRIES IN lt_pevsh
      WHERE runid = lt_pevsh-runid.
    IF  sy-subrc IS INITIAL.
      SORT pct_awkey BY pernr runid awref aworg awlin.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_FETCH_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATA
*&---------------------------------------------------------------------*
*  Fill data for report display
*----------------------------------------------------------------------*
*  --> PIT_PEVSH Data from PEVSH
*  --> PIT_PTRV  Data from PTRV_ROT_AWKEY
*  <-- PCT_OUTPUT Output for report display
*----------------------------------------------------------------------*
FORM f_fill_data USING     pit_pevsh   TYPE tt_pevsh
                           pit_awkey   TYPE tt_awkey
                CHANGING   pct_output  TYPE tt_output.

  DATA:lt_pevsh TYPE STANDARD TABLE OF gty_pevsh INITIAL SIZE 0,
       lt_params TYPE STANDARD TABLE OF rsparams  INITIAL SIZE 0.
  DATA:ls_params     TYPE rsparams,
       ls_pevsh      TYPE gty_pevsh,
       ls_awkey      TYPE gty_awkey,
       ls_output     TYPE gty_output.
  CONSTANTS:lc_dash    TYPE flag VALUE '-',
            lc_asterix TYPE flag VALUE '*',
            lc_zero    TYPE c    VALUE '0'.

*- Begin of change CHG0159701 by SHAFFES
  CONSTANTS: lc_gl_exclude TYPE hkont VALUE '0000251040'.
*- End of change CHG0159701 by SHAFFES

  FIELD-SYMBOLS :<lfs_pay_data> TYPE ANY TABLE,
                 <lfs_output>   TYPE gty_output,
                 <lfs_data>     TYPE any .
  DATA lo_pay_data TYPE REF TO data.

  lt_pevsh = pit_pevsh.
  DELETE ADJACENT DUPLICATES FROM lt_pevsh COMPARING runid.
  LOOP AT lt_pevsh INTO ls_pevsh.
    IF lt_pevsh IS NOT INITIAL.
      ls_params-selname = 'P_RUNID'.
      ls_params-kind = 'P'.
      ls_params-low = ls_pevsh-runid.
      APPEND ls_params TO lt_params.
    ENDIF.
  ENDLOOP.
  IF lt_params IS NOT INITIAL.
    cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                            metadata = abap_false
                                            data     = abap_true ).
    SUBMIT rprhrdoc WITH SELECTION-TABLE lt_params AND RETURN.
    TRY.
        cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lo_pay_data ).
        ASSIGN lo_pay_data->* TO <lfs_pay_data>.
      CATCH cx_salv_bs_sc_runtime_info.
        MESSAGE s000(zfi01) WITH text-t01 DISPLAY LIKE gc_error."'E'.
        LEAVE LIST-PROCESSING .
    ENDTRY.
    cl_salv_bs_runtime_info=>clear_all( ).
    LOOP AT <lfs_pay_data> ASSIGNING  <lfs_data>.
      MOVE-CORRESPONDING <lfs_data> TO ls_output.
      APPEND ls_output TO pct_output.
      CLEAR ls_output.
    ENDLOOP.
  ENDIF.

**- Begin of change CHG0159701 by SHAFFES
  DELETE pct_output WHERE hkont EQ lc_gl_exclude.
**- End of change CHG0159701 by SHAFFES

*Modify output data
  LOOP AT pct_output ASSIGNING <lfs_output>.
*Alter posting date format
    CONCATENATE <lfs_output>-budat(4)   lc_dash
                <lfs_output>-budat+4(2) lc_dash
                <lfs_output>-budat+6(2) lc_dash
           INTO <lfs_output>-budat."Posting Date
    CONDENSE <lfs_output>-budat.
*Alter the tax account field with space
    IF <lfs_output>-hkonttax = lc_asterix.
      CLEAR <lfs_output>-hkonttax.
    ENDIF.
*Fill Reference field
    READ TABLE pit_awkey INTO ls_awkey
                         WITH KEY pernr = <lfs_output>-pernr
                                  runid = <lfs_output>-runid
                                  awref = <lfs_output>-awref
                                  aworg = <lfs_output>-aworg
                                  awlin = <lfs_output>-awlin
                         BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <lfs_output>-reinr = ls_awkey-reinr.
    ENDIF.
*Convert Amount with sign in front and no comma in thousand
    CALL FUNCTION 'ZFI_CONVERT_AMT_NEGATIVE'
      EXPORTING
        i_waers  = <lfs_output>-waers
      IMPORTING
        e_amount = <lfs_output>-amount
      CHANGING
        c_wrbtr  = <lfs_output>-wrbtr.
    IF <lfs_output>-amount IS INITIAL.
      MOVE lc_zero TO <lfs_output>-amount.
    ENDIF.
*Convert Tax Debit with sign in front and no comma in thousand
    CALL FUNCTION 'ZFI_CONVERT_AMT_NEGATIVE'
      EXPORTING
        i_waers  = <lfs_output>-waers
      IMPORTING
        e_amount = <lfs_output>-tax_dr
      CHANGING
        c_wrbtr  = <lfs_output>-steur_soll.
    IF <lfs_output>-tax_dr IS INITIAL.
      MOVE lc_zero TO <lfs_output>-tax_dr.
    ENDIF.
*Convert Tax Credit with sign in front and no comma in thousand
    CALL FUNCTION 'ZFI_CONVERT_AMT_NEGATIVE'
      EXPORTING
        i_waers  = <lfs_output>-waers
      IMPORTING
        e_amount = <lfs_output>-tax_cr
      CHANGING
        c_wrbtr  = <lfs_output>-steur_haben.
    IF <lfs_output>-tax_cr IS INITIAL.
      MOVE lc_zero TO <lfs_output>-tax_cr.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " F_FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*    Display ALV report
*----------------------------------------------------------------------*
*  --> pit_output ALV data table
*----------------------------------------------------------------------*
FORM f_display_data USING pit_output TYPE tt_output.
  IF pit_output IS NOT INITIAL.
*Display ALV
    CALL SCREEN 1001.
  ENDIF.
ENDFORM.                    " F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*   PBO module for ALV container screen
*----------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  SET  PF-STATUS 'ZPREM'.
  SET TITLEBAR 'ZTITLE1'.
ENDMODULE.                 " STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*   PAI module for ALV container screen
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
*Do nothing
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*   Display ALV report
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  IF gt_output IS NOT INITIAL.
    SORT gt_output BY pernr.
    DATA: ls_stable     TYPE lvc_s_stbl,
          lt_sort       TYPE lvc_t_sort,
          ls_sort       TYPE lvc_s_sort.
* Check to see if we are runnng on online
    IF cl_gui_alv_grid=>offline( ) IS INITIAL.
*Create docking container
      CREATE OBJECT o_dockingcontainer
        EXPORTING
          ratio                       = '95'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.

      IF sy-subrc NE 0.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
*create object of alv grid
    CREATE OBJECT o_alvgd
      EXPORTING
        i_parent = o_dockingcontainer.
* create ALV event handler
    CREATE OBJECT o_alv_toolbar.
* Register event handler
    gs_layout-grid_title = text-t05.
    gs_layout-zebra      = abap_true.
    gs_layout-cwidth_opt = abap_true.
    gs_layout-sel_mode   = 'A'.

*  Build filed catalog
    PERFORM f_build_field_cat.
    CLEAR ls_sort.
    ls_sort-fieldname = 'PERNR'.
    ls_sort-up        = abap_true.
    ls_sort-spos      = 2.
    APPEND ls_sort TO lt_sort.
    IF gt_output IS NOT INITIAL.
      CALL METHOD o_alvgd->set_table_for_first_display
        EXPORTING
          i_save                        = 'A'
          is_layout                     = gs_layout    " Layout
        CHANGING
          it_outtab                     = gt_output    " Output Table
          it_fieldcatalog               = gt_fcat      " Field Catalog
          it_sort                       = lt_sort
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.
      MOVE abap_true TO: ls_stable-row, ls_stable-col.
      o_alvgd->refresh_table_display(
        EXPORTING is_stable = ls_stable
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF."endif sy-subrc <> 0.

    ENDIF."endif it_alvout IS NOT INITIAL.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_DWNLD_XLS
*&---------------------------------------------------------------------*
*    Download output to Excel file in presentation server
*----------------------------------------------------------------------*
*      -->PIT_OUTPUT  Data tabel for display
*----------------------------------------------------------------------*
FORM f_dwnld_xls  USING  pit_output TYPE tt_output.
*Local internal table
  DATA:lt_fieldnames  TYPE STANDARD TABLE OF gty_fieldnames INITIAL SIZE 0,
       lt_excel       TYPE STANDARD TABLE OF gty_output_xcl INITIAL SIZE 0.
  DATA:ls_output TYPE gty_output,
       ls_excel  TYPE gty_output_xcl.
  DATA:lv_filename TYPE string,
       lv_file     TYPE string,
       lv_taxrt    TYPE string.
*Fetch file names
  PERFORM f_get_filename CHANGING lv_filename.
  CONCATENATE p_xlspth lv_filename INTO lv_file SEPARATED BY '\'.
  CONDENSE lv_file.
*Fill column headings in file
  PERFORM f_fill_headings CHANGING lt_fieldnames.
*Fill file data
  IF pit_output IS NOT INITIAL.
    LOOP AT pit_output INTO ls_output.
      MOVE-CORRESPONDING ls_output TO ls_excel.
*Convert amount fields into string
      PERFORM f_conv_amnt  USING  ls_output-wrbtr       ls_output-steur_soll
                                  ls_output-steur_haben ls_output-msatz      ls_output-waers
                         CHANGING ls_excel-amount ls_excel-steur_soll ls_excel-steur_haben lv_taxrt.
      "lv_tax_dr lv_tax_cr lv_taxrt.
*Convert WBS to external format for file display
      PERFORM f_conv_wbs USING ls_output-posnr CHANGING ls_excel-wbs.
      IF ls_excel IS NOT INITIAL.
        APPEND ls_excel TO lt_excel.
      ENDIF.
    ENDLOOP.
  ENDIF.
*Download to excel
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                = lv_file
      write_field_separator   = gc_set
      fieldnames              = lt_fieldnames
    CHANGING
      data_tab                = lt_excel
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
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_DWNLD_XLS
*&---------------------------------------------------------------------*
*&      Form  F_FILL_HEADINGS
*&---------------------------------------------------------------------*
*     Fill Excel file header names
*----------------------------------------------------------------------*
*      <--> PCT_FIELDNAMES  Headings in Excel file
*----------------------------------------------------------------------*
FORM f_fill_headings  CHANGING pct_fieldnames TYPE tt_fieldnames.

  DATA:lwa_fieldnames TYPE gty_fieldnames.

  lwa_fieldnames-line = text-h03. "Personnel.Number.
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h43. "Assignment
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h06. "Co.Cd
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h44. "Blg
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h45."Posting Date
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h11."Transactn
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h14."G/L Accnt
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h17."Account Name
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h18."Currency
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h20."Amount
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h27."Tax
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h25."Tax Accnt
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h22."Tax Dbt
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h29."Tax Crdt
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h36."Text
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h32."Cost Cntr
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h33."Order
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h35."WBS Elemnt
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h38."Reference
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h40."Tax Rate
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.

  lwa_fieldnames-line = text-h41."Vendor
  APPEND lwa_fieldnames TO pct_fieldnames.
  CLEAR lwa_fieldnames.
ENDFORM.                    " F_FILL_HEADINGS
*&---------------------------------------------------------------------*
*&      Form  F_FILE_SYSTEM
*&---------------------------------------------------------------------*
*    Get application server paths from ZFIT_PARAM
*----------------------------------------------------------------------*
FORM f_file_system .
  SELECT paramtype subtype key1 key2 key3 value1
    FROM zfit_xparam
    INTO TABLE gt_file_system
    WHERE paramtype = gc_paramtyp
      AND subtype   = gc_subtype
      AND key1      = gc_varname1
      AND key2      = gc_key2.
  IF sy-subrc IS INITIAL.
    SORT gt_file_system BY paramtype subtype key1 key2 key3 value1.
  ENDIF.
ENDFORM.                    " F_FILE_SYSTEM
*&---------------------------------------------------------------------*
*&      Form  F_GET_FILENAME
*&---------------------------------------------------------------------*
*    Build filename dynamically
*----------------------------------------------------------------------*
*      <--PC_FILENAME  Filename created
*----------------------------------------------------------------------*
FORM f_get_filename  CHANGING pc_filename TYPE string.
  DATA:lv_sysid  TYPE sy-sysid.
  DATA:ls_file_system TYPE gty_app_filepath.
  CONSTANTS:lc_uscore TYPE flag VALUE '_'.
  lv_sysid = sy-sysid.
  IF gt_file_system IS NOT INITIAL.
    READ TABLE gt_file_system  INTO ls_file_system
                               WITH KEY paramtype = gc_paramtyp
                                        subtype   = gc_subtype
                                        key1      = gc_varname1
                                        key2      = gc_key2
                                        key3      = lv_sysid
                               BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF rb_xls IS NOT INITIAL.
        CONCATENATE text-t03 ls_file_system-value1 lc_uscore sy-datum lc_uscore sy-uzeit '.xls' INTO pc_filename.
      ENDIF.
      IF rb_app IS NOT INITIAL.
        CONCATENATE text-t03 ls_file_system-value1 lc_uscore sy-datum lc_uscore sy-uzeit INTO pc_filename.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  F_DWNLD_APP
*&---------------------------------------------------------------------*
*      Download output in application server file
*----------------------------------------------------------------------*
*      -->PIT_OUTPUT  Data extract
*----------------------------------------------------------------------*
FORM f_dwnld_app  USING pit_output TYPE tt_output.
  DATA:lt_output   TYPE STANDARD TABLE OF gty_output INITIAL SIZE 0,
       ls_output  TYPE gty_output.
  DATA:lv_filename TYPE localfile,
       lv_file     TYPE string,
       lv_data     TYPE string,
       lv_wrbtr    TYPE string,
       lv_taxrt    TYPE string,
       lv_tax_dr   TYPE string,
       lv_tax_cr   TYPE string,
       lv_wbs      TYPE char24,
       lv_sep      TYPE string.
  DATA:lc_xstring TYPE xstring VALUE '01'."ASCII Code for ^A(SOH)
*Fetch file names
  PERFORM f_get_filename CHANGING lv_file.
  IF lv_file IS NOT INITIAL.
    CONCATENATE p_apppth lv_file INTO lv_filename SEPARATED BY '/'.
    CONDENSE lv_filename.
  ENDIF.
*Get file data
  IF pit_output IS NOT INITIAL.
    lt_output = pit_output.
  ENDIF.
*Convert ASCII value of separator to string for writing to file
  CALL FUNCTION 'HR_KR_XSTRING_TO_STRING'
    EXPORTING
      in_xstring = lc_xstring
    IMPORTING
      out_string = lv_sep.

  OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc IS INITIAL.
*Fill file heading
    CONCATENATE text-h03 "Personnel.Numberr
                text-h43 "Assignment
                text-h06 "Co.Cd
                text-h44 "Blg
                text-h45 "Posting.Date
                text-h11 "Transactn
                text-h14 "G/L Accnt
                text-h17 "Account Name
                text-h18 "Currency
                text-h20 "Amount
                text-h27 "Tax
                text-h25 "Tax acct
                text-h22 "Tax Dbt
                text-h29 "Tax Crdt
                text-h36 "Text
                text-h32 "Cost Cntr
                text-h33 "Order
                text-h35 "WBS Elemnt
                text-h38 "Reference
                text-h40 "Tax Rate
                text-h41 "Vendor
           INTO lv_data
           SEPARATED BY lv_sep."cl_abap_char_utilities=>horizontal_tab."'\x01'."'\u0001'.
    TRANSFER lv_data TO lv_filename.
*Fill file data
    CLEAR lv_data.
    LOOP AT lt_output INTO ls_output.
*Convert amount fields into string
      PERFORM f_conv_amnt  USING  ls_output-wrbtr       ls_output-steur_soll
                                  ls_output-steur_haben ls_output-msatz      ls_output-waers
                         CHANGING lv_wrbtr lv_tax_dr lv_tax_cr lv_taxrt.
      PERFORM f_conv_wbs USING ls_output-posnr CHANGING lv_wbs.
      CONCATENATE ls_output-pernr
                  ls_output-exbel
                  ls_output-bukrs
                  ls_output-blart
                  ls_output-budat
                  ls_output-ktosl
                  ls_output-hkont
                  ls_output-hkonttxt
                  ls_output-waers
                  lv_wrbtr
                  ls_output-mwskz
                  ls_output-hkonttax
                  lv_tax_dr
                  lv_tax_cr
                  ls_output-sgtxt
                  ls_output-kostl
                  ls_output-aufnr
                  lv_wbs
                  ls_output-reinr
                  lv_taxrt
                  ls_output-lifnr
             INTO lv_data
             SEPARATED BY lv_sep."cl_abap_char_utilities=>horizontal_tab.."'\x01'."'\u0001'.
      TRANSFER lv_data TO lv_filename.
      CLEAR:lv_data,ls_output,lv_wbs.
    ENDLOOP.
    CLOSE DATASET lv_filename.
*Success handling
    PERFORM f_handle_success USING gt_output lv_file.
  ELSE.
*Error handling
    PERFORM f_handle_error USING gt_output lv_file.
  ENDIF.
ENDFORM.                    " F_DWNLD_APP
*&---------------------------------------------------------------------*
*&      Form  F_CONV_AMNT
*&---------------------------------------------------------------------*
*     Convert amount field into text format for file display
*----------------------------------------------------------------------*
*      -->PI_WRBTR   Amount
*      -->PI_TAX_DR  Tax Debit
*      -->PI_TAX_CR  Tax Credit
*      <--PC_WRBTR   Amount converted
*      <--PC_TAX_DR  Tax Debit converted
*      <--PC_TAX_CR  Tax Credit converted
*----------------------------------------------------------------------*
FORM f_conv_amnt  USING    pi_wrbtr  TYPE acbtr
                           pi_tax_dr TYPE hwste
                           pi_tax_cr TYPE hwste
                           pi_taxrt  TYPE fwste
                           pi_waers  TYPE waers
                  CHANGING pc_wrbtr  TYPE string
                           pc_tax_dr TYPE string
                           pc_tax_cr TYPE string
                           pc_taxrt  TYPE string.
  DATA:lv_wrbtr    TYPE  c LENGTH 15,
       lv_tax_dr   TYPE  c LENGTH 15,
       lv_tax_cr   TYPE  c LENGTH 15,
       lv_taxrt    TYPE  c LENGTH 15.

  CONSTANTS lc_neg_sign TYPE flag VALUE '-'.
  IF pi_wrbtr IS NOT INITIAL.
*This us used to get the thousand separator and decimal notation for amount
    WRITE : pi_wrbtr TO lv_wrbtr CURRENCY pi_waers NO-GROUPING.
    IF lv_wrbtr CS lc_neg_sign.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_wrbtr.
      CONDENSE lv_wrbtr NO-GAPS.
    ELSE.
      CONDENSE lv_wrbtr NO-GAPS.
    ENDIF.
    pc_wrbtr = lv_wrbtr.
  ENDIF.
  IF pi_tax_dr IS NOT INITIAL.
*This us used to get the thousand separator and decimal notation for tax debit
    WRITE : pi_tax_dr TO lv_tax_dr CURRENCY pi_waers NO-GROUPING.
    IF lv_tax_dr CS lc_neg_sign.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_tax_dr.
      CONDENSE lv_tax_dr NO-GAPS.
    ELSE.
      CONDENSE lv_tax_dr NO-GAPS.
    ENDIF.
    pc_tax_dr = lv_tax_dr.
  ENDIF.
  IF pi_tax_cr IS NOT INITIAL.
*This us used to get the thousand separator and decimal notation for tax credit
    WRITE : pi_tax_cr TO lv_tax_cr CURRENCY pi_waers NO-GROUPING.
    IF lv_tax_cr CS lc_neg_sign.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_tax_cr.
      CONDENSE lv_tax_cr NO-GAPS.
    ELSE.
      CONDENSE lv_tax_cr NO-GAPS.
    ENDIF.
    pc_tax_cr = lv_tax_cr.
  ENDIF.
  IF pi_taxrt IS NOT INITIAL.
*This us used to get the thousand separator and decimal notation for tax rate
    WRITE : pi_taxrt TO lv_taxrt CURRENCY pi_waers.
    pc_taxrt = lv_taxrt.
  ENDIF.
ENDFORM.                    " F_CONV_AMNT
*&---------------------------------------------------------------------*
*&      Form  F_HANDLE_ERROR
*&---------------------------------------------------------------------*
*     Error handling if fiel creation fails
*----------------------------------------------------------------------*
*  -->  PIT_OUTPUT        Data extract
*  -->  PI_FILENAME       Filename to be created
*----------------------------------------------------------------------*
FORM f_handle_error USING pit_output  TYPE tt_output
                          pi_filename TYPE string.
  DATA:lv_flag    TYPE flag.

  lv_flag    = gc_error.
*Create error log file
  PERFORM f_write_error USING pit_output.
*Send error mail
  PERFORM f_send_mail USING pit_output pi_filename lv_flag.

ENDFORM.                    " F_HANDLE_ERROR
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_ERROR
*&---------------------------------------------------------------------*
*   Write error details in spool
*----------------------------------------------------------------------*
*  -->  PIT_OUT       Data extract
*----------------------------------------------------------------------*
FORM f_write_error USING pit_out TYPE tt_output.
  DATA:lv_failure_recs TYPE sy-tabix,
       lv_recs         TYPE sy-tabix.
  DESCRIBE TABLE pit_out LINES lv_recs.
  lv_failure_recs = lv_recs - lv_recs.
  WRITE:/5'Total no of records processed:'(t10),lv_recs,
        /5'Total number of success records:'(t12),lv_failure_recs,
        /5'Execution Date:'(t13),sy-datum,
        /5'Execution Time:'(t14),sy-uzeit.
ENDFORM.                    " F_WRITE_ERROR
*&---------------------------------------------------------------------*
*&      Form  F_SEND_MAIL
*&---------------------------------------------------------------------*
*    Send error or success mail
*----------------------------------------------------------------------*
*  <--  PI_CONTENT Content for mail
*  -->  PI_FILE    File name to be crated/created
*  -->  PI_FLAG    Success/Error indicator flag
*----------------------------------------------------------------------*
FORM f_send_mail USING pi_content TYPE tt_output
                       pi_file    TYPE string
                       pi_flag    TYPE flag.
  DATA: ls_docdata    TYPE sodocchgi1,   "Mail subject
        lt_receiver   TYPE STANDARD TABLE OF somlrec90 INITIAL SIZE 0,  "Recepient
        ls_receiver   TYPE somlrec90,
        lt_content    TYPE STANDARD TABLE OF soli      INITIAL SIZE 0,"Main body
        ls_content    TYPE soli,
        lt_mail_recp  TYPE STANDARD TABLE OF gty_app_filepath INITIAL SIZE 0,
        ls_mail_recp  TYPE gty_app_filepath,
        lt_text       TYPE STANDARD TABLE OF tline INITIAL SIZE 0,
        ls_text       TYPE tline,
        ls_header     TYPE thead.

  DATA:lv_st_text  TYPE tdobname.
  CONSTANTS:lc_sucs_st TYPE tdobname    VALUE 'Z_PRIEMTXT_SUCSMAIL',
            lc_err_st  TYPE tdobname    VALUE 'Z_PRIEMTXT_ERRMAIL',
            lc_htm     TYPE soodk-objtp VALUE 'HTM'.

  REFRESH lt_receiver.
*Get the list of recipients
  SELECT paramtype subtype key1 key2 key3 value1
    FROM zfit_xparam
    INTO TABLE lt_mail_recp
    WHERE paramtype = gc_paramtyp
      AND subtype   = gc_subtype
      AND key1      = gc_varname2
      AND key2      = gc_key3.
  IF sy-subrc IS INITIAL.
    SORT lt_mail_recp BY paramtype subtype key1 key2 key3 value1.
  ENDIF.
  LOOP AT lt_mail_recp INTO ls_mail_recp.
    ls_receiver-receiver = ls_mail_recp-value1.
    ls_receiver-rec_type = 'U'.
    ls_receiver-express  = gc_set."'X'.
    APPEND ls_receiver TO lt_receiver.
  ENDLOOP.

*****Mail Body
*Get file name that has to be generated
  CLEAR:gv_filename,gv_no_recs.
  gv_filename = pi_file.
*Get total records in file
  IF pi_flag = gc_sucs.
    IF pi_content IS NOT INITIAL.
      DESCRIBE TABLE pi_content LINES gv_no_recs.
    ENDIF.
  ENDIF.
*Get error message
  IF pi_flag = gc_error.
    gv_err_msg = text-t08.
  ENDIF.
*Get standard text name based on success/error flag
  CASE pi_flag.
    WHEN gc_sucs.
      lv_st_text = lc_sucs_st.
    WHEN gc_error.
      lv_st_text = lc_err_st.
    WHEN OTHERS.
*Do nothing
  ENDCASE.

*Get mail body text from standard text
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'ST'
      language                = sy-langu
      name                    = lv_st_text
      object                  = 'TEXT'
    TABLES
      lines                   = lt_text
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc = 0.
*Replace dynamic texts in standard text
    ls_header-tdname   = lv_st_text.
    ls_header-tdid     = 'ST'.
    ls_header-tdspras  = sy-langu.
    ls_header-tdobject = 'TEXT'.
    CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
      EXPORTING
        header          = ls_header
        replace_program = gc_set
      TABLES
        lines           = lt_text.
  ENDIF.

*Fill contents
  LOOP AT lt_text INTO ls_text.
    IF sy-tabix = 1.
*****Mail Subject
      CLEAR ls_docdata.
      ls_docdata-obj_name  = ls_text-tdline.
      ls_docdata-obj_descr = ls_text-tdline.
      ls_docdata-obj_langu = sy-langu.
    ELSE.
      ls_content-line = ls_text-tdline.
      APPEND ls_content TO lt_content.
      CLEAR ls_content.
    ENDIF.
    CLEAR:ls_text.
  ENDLOOP.


  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_docdata
      document_type              = lc_htm
      put_in_outbox              = gc_set "'X'
      commit_work                = gc_set "'X'
    TABLES
      object_content             = lt_content
      receivers                  = lt_receiver
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to send error mail'(t15).
  ENDIF.
ENDFORM.                    " F_SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_SUCCESS
*&---------------------------------------------------------------------*
*    Write success message to spool
*----------------------------------------------------------------------*
*      -->PIT_OUTPUT  Data extract
*----------------------------------------------------------------------*
FORM f_write_success  USING  pit_out TYPE tt_output.
  DATA:lv_success_recs TYPE sy-tabix,
       lv_recs         TYPE sy-tabix.
  DESCRIBE TABLE pit_out LINES lv_success_recs.
  lv_recs = lv_success_recs.
  WRITE:/5'Total no of records processed:'(t10),lv_recs,
        /5'Total number of success records:'(t12),lv_success_recs,
        /5'Execution Date:'(t13),sy-datum,
        /5'Execution Time:'(t14),sy-uzeit.
ENDFORM.                    " F_WRITE_SUCCESS
*&---------------------------------------------------------------------*
*&      Form  F_HANDLE_SUCCESS
*&---------------------------------------------------------------------*
*     File successfully created
*----------------------------------------------------------------------*
*      -->PIT_OUTPUT       Data extract
*      -->PI_FILENAME      File name
*----------------------------------------------------------------------*
FORM f_handle_success  USING pit_output  TYPE tt_output
                             pi_filename TYPE string.
  DATA:lv_flag    TYPE flag.
  lv_flag    = gc_sucs.
*Create success log
  PERFORM f_write_success USING pit_output.
*Send success mail
  PERFORM f_send_mail USING pit_output pi_filename lv_flag.
ENDFORM.                    " F_HANDLE_SUCCESS
*&---------------------------------------------------------------------*
*&      Form  F_SET_DATE_INTERVAL
*&---------------------------------------------------------------------*
*   Set date interval
*----------------------------------------------------------------------*
*      -->PC_LAST_MNTH_BEGIN     Begin date of last month
*      -->PC_LAST_MNTH_END       End date of last month
*----------------------------------------------------------------------*
FORM f_set_date_interval CHANGING pc_last_mnth_begin TYPE d
                                  pc_last_mnth_end   TYPE d.
  DATA:lv_datum           TYPE sy-datum,
       lv_last_month      TYPE sy-datum.

  lv_datum = sy-datum.

*Get last month
  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING
      months  = -1
      olddate = lv_datum
    IMPORTING
      newdate = lv_last_month.
*Get first and last date of previous month
  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = lv_last_month
    IMPORTING
      ev_month_begin_date = pc_last_mnth_begin
      ev_month_end_date   = pc_last_mnth_end.

ENDFORM.                    " F_SET_DATE_INTERVAL
*&---------------------------------------------------------------------*
*&      Form  F_CONV_WBS
*&---------------------------------------------------------------------*
*  Convert WBS from internal to external format for file display option
*----------------------------------------------------------------------*
*      -->PI_POSNR  WBS in internal format
*      <--PC_WBS    WBS in external format
*----------------------------------------------------------------------*
FORM f_conv_wbs  USING pi_posnr TYPE ps_posnr
              CHANGING pc_wbs   TYPE char24.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
    EXPORTING
      input  = pi_posnr
    IMPORTING
      output = pc_wbs.

ENDFORM.                    " F_CONV_WBS
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_DATE
*&---------------------------------------------------------------------*
*   Change date
*----------------------------------------------------------------------*
FORM f_change_date .
  IF s_crtdt[] IS INITIAL.
    s_crtdt-low    = gv_last_mnth_begin.
    s_crtdt-high   = gv_last_mnth_end.
    s_crtdt-option = gc_eq.
    s_crtdt-sign   = gc_i.
    APPEND s_crtdt.
  ENDIF.
ENDFORM.                    " F_CHANGE_DATE
*&---------------------------------------------------------------------*
*&      Form  F_INIT_ACTIONS
*&---------------------------------------------------------------------*
*    Perform actions at initialization
*----------------------------------------------------------------------*
FORM f_init_actions.
  PERFORM:f_refresh,      "Refresh global structures
          f_app_filepath, "Get Application filepath
          f_file_system,  "Get current system ID
          f_set_date_interval CHANGING gv_last_mnth_begin gv_last_mnth_end.
ENDFORM.                    " F_INIT_ACTIONS
*&---------------------------------------------------------------------*
*&      Form  F_SCREEN_PBO
*&---------------------------------------------------------------------*
*   Actions before selection screen display PBO
*----------------------------------------------------------------------*
FORM f_screen_pbo .
  PERFORM :f_enable_fpath,
           f_change_date.
ENDFORM.                    " F_SCREEN_PBO
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS
*&---------------------------------------------------------------------*
*   Processing logic
*----------------------------------------------------------------------*
FORM f_process .
*Fetch Data
  PERFORM f_fetch_data CHANGING gt_pevsh gt_awkey.
*Display data
  IF gt_pevsh IS NOT INITIAL.
    PERFORM f_fill_data USING gt_pevsh gt_awkey CHANGING  gt_output.
  ENDIF.
ENDFORM.                    " F_PROCESS
*&---------------------------------------------------------------------*
*&      Form  F_OUTPUT
*&---------------------------------------------------------------------*
*    Output data
*----------------------------------------------------------------------*
FORM f_output .
*Display report option checked by user
  IF rb_alv = gc_set.
    IF gt_output IS NOT INITIAL.
      PERFORM f_display_data USING gt_output.
    ELSE.
      MESSAGE s000(zfi01) WITH text-t01 DISPLAY LIKE gc_error."'E'.
      LEAVE LIST-PROCESSING .
    ENDIF.
  ENDIF.
*Download to local file option checked by user
  IF rb_xls = gc_set.
    IF sy-batch IS INITIAL.
      IF gt_output IS NOT INITIAL.
        PERFORM f_dwnld_xls USING gt_output.
      ELSE.
        MESSAGE s000(zfi01) WITH text-t01 DISPLAY LIKE gc_error."'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ELSE.
      MESSAGE e000(zfi01) WITH text-t02.
      LEAVE LIST-PROCESSING .
    ENDIF.
  ENDIF.
*Download to application server option checked by user
  IF rb_app = gc_set.
    IF gt_output IS NOT INITIAL.
      PERFORM f_dwnld_app USING gt_output.
    ELSE.
      MESSAGE s000(zfi01) WITH text-t01 DISPLAY LIKE gc_error."'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_RBUT
*&---------------------------------------------------------------------*
*   Ensure only one radiobutton is set at a time
*----------------------------------------------------------------------*
FORM f_reset_rbut .
  IF rb_app IS NOT INITIAL.
    rb_xls = space.
    rb_alv = space.
  ELSEIF rb_xls IS NOT INITIAL.
    rb_app = space.
    rb_alv = space.
  ELSEIF rb_alv IS NOT INITIAL.
    rb_xls = space.
    rb_app = space.
  ENDIF.
ENDFORM.                    " F_CHECK_RBUT
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_FCAT
*&---------------------------------------------------------------------*
*  Modifying field descriptions in Fieldcatalog
*----------------------------------------------------------------------*
*      -->PI_REPTEXT   Field text for display
*      -->PI_STEXT     Short text for field names
*      -->PI_MTEXT     Middle text for field names
*      -->PI_LTEXT     Long text for field names
*      -->PI_FNAME     Field Names
*      <-->PC_FIELDCAT Fieldctalog that is changed
*----------------------------------------------------------------------*
FORM f_sign_fcat    USING  pi_reptext  TYPE reptext
                           pi_stext    TYPE scrtext_s
                           pi_mtext    TYPE scrtext_m
                           pi_ltext    TYPE scrtext_l
                           pi_fname    TYPE lvc_fname
                  CHANGING pc_fieldcat TYPE lvc_s_fcat.
  CLEAR pc_fieldcat.
  pc_fieldcat-reptext   = pi_reptext.
  pc_fieldcat-scrtext_s = pi_stext.
  pc_fieldcat-scrtext_m = pi_mtext.
  pc_fieldcat-scrtext_l = pi_ltext.
  pc_fieldcat-fieldname = pi_fname.
  pc_fieldcat-edit_mask = '==ZAMNT'.
ENDFORM.                    " F_SIGN_FCAT
