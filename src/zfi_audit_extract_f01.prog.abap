*----------------------------------------------------------------------*
* Report Name: ZFR_AUDIT_EXTRACT_F01
* Author:	     KBANERJEE-Kaushiki Banerjee
* Date:	       December 4th,2018
* Logical Database: NA
* SAPScript name:   NA
* Application Area: FI
* Description:  This tool will allow the user with the opportunity to
*               receive the extract for Audit related tables.The extract
*               can be obtained as onoine report,and can also be
*               downloaded to local PC as excel or other types of files.
*               This program also allows users to send mails to the
*               specific person and attaching the table extract in mail.
*               If no mail receipient is specifiedmail is sent to the
*               person who executes this program.
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 04-DEC-2018  KBANERJEE   D30K929436  CHG0137915 -Initial Development *
* 22-JAN-2019  KBANERJEE   D30K929560  UAT issues resolution           *
*                          D30K929545                                  *
*                          D30K929519                                  *
*                          D30K929542                                  *
*                          D30K929483                                  *
*                          D30K929459                                  *
*                          D30K929696                                  *
* 17-FEB-2020 KBANERJEE    D30K930437 CHG0174059_DFCT0018156-Code fixes*
*                                     to audit extract programs        *
* 30-APR-2021 KOTTAPAN     D30K930935 CHG0206319 - Write BSEG, PRPS    *
*                          D30K930972 files to Application Server      *
*                          D30K930978                                  *
*                          D30K931002                                  *
* 07-Jun-2021 KOTTAPAN     D30K931020 CHG0216879 Increase field length *
*                          D30K931022 for file headings &              *
*                                     write BKPF to Appl server        *
************************************************************************
CREATE OBJECT o_extract.
*&---------------------------------------------------------------------*
*&      Form  f_primary_index
*&---------------------------------------------------------------------*
*       text:Determine the Primary Index
*----------------------------------------------------------------------*
*      <-- PCT_DDFIELDS  text:Field values
*      --> PIT_FIELDNAME Fieldnames
*----------------------------------------------------------------------*
FORM f_primary_index   USING pit_fieldname TYPE tt_ddfields
                    CHANGING pct_fields    TYPE tt_wh_fields.
  CALL METHOD o_extract->check_primary_key
    EXPORTING
      it_fieldnames = pit_fieldname
      it_fieldlist  = pct_fields.
ENDFORM.                    " f_primary_index
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_XLS
*&---------------------------------------------------------------------*
*  Download extract to local PC
*----------------------------------------------------------------------*

FORM f_download_pc .
  DATA:lv_filename TYPE string,
       lv_file     TYPE string.
  DATA:lv_ext TYPE char4.

  lv_ext = p_fext1.
  TRANSLATE lv_ext TO UPPER CASE.
*Fetch file names
  CONCATENATE p_fpth1 p_fnam1  INTO lv_filename SEPARATED BY '\'.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  "CONCATENATE lv_filename '.' p_fext1 INTO lv_file.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  MOVE lv_filename TO lv_file.
  CONDENSE lv_file.
  IF lv_ext EQ 'XLS' .
    PERFORM f_dwnld_excel USING lv_file.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  ELSEIF lv_ext = 'XLSX'.
    PERFORM f_dwnld_xlsx  USING lv_file.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  ELSE.
    PERFORM f_dwnld_oth   USING lv_file.
  ENDIF.
ENDFORM.                    " F_DOWNLOAD_XLS
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_SEL_RANGE
*&---------------------------------------------------------------------*
*    Build dynamic range table for select optin values
*----------------------------------------------------------------------*
* -->PCT_RANGE1 Range for select option 1
* -->PCT_RANGE2 Range for select option 2
* -->PCT_RANGE3 Range for select option 3
* -->PCT_RANGE4 Range for select option 4
* -->PCT_RANGE5 Range for select option 5
* -->PCT_RANGE6 Range for select option 6
*----------------------------------------------------------------------*
FORM f_build_sel_range
             CHANGING pct_range1 TYPE /sapdii/twty_rsds_selopt
                      pct_range2 TYPE /sapdii/twty_rsds_selopt
                      pct_range3 TYPE /sapdii/twty_rsds_selopt
                      pct_range4 TYPE /sapdii/twty_rsds_selopt
                      pct_range5 TYPE /sapdii/twty_rsds_selopt
                      pct_range6 TYPE /sapdii/twty_rsds_selopt
                      pct_range7 TYPE /sapdii/twty_rsds_selopt.
  DATA:  lt_field    TYPE STANDARD TABLE OF rsdsselopt INITIAL SIZE 0,
         ls_field    TYPE rsdsselopt,
         ls_ddfields TYPE dfies.

* Build Range table for Company Code
  IF s_bukrs1[] IS NOT INITIAL.
    LOOP AT s_bukrs1.
      ls_field-sign   = s_bukrs1-sign.
      ls_field-option = s_bukrs1-option.
      ls_field-low    = s_bukrs1-low.
      ls_field-high   = s_bukrs1-high.
      APPEND ls_field TO lt_field.
      CLEAR ls_field.
    ENDLOOP.
    CALL METHOD o_extract->build_dyn_range
      EXPORTING
        et_value  = lt_field
      IMPORTING
        it_ranges = pct_range1.
  ENDIF.
  REFRESH lt_field.
* Build Range table for Fiscal Year
  IF s_gjahr1[] IS NOT INITIAL.
    LOOP AT s_gjahr1.
      ls_field-sign   = s_gjahr1-sign.
      ls_field-option = s_gjahr1-option.
      ls_field-low    = s_gjahr1-low.
      ls_field-high   = s_gjahr1-high.
      APPEND ls_field TO lt_field.
      CLEAR ls_field.
    ENDLOOP.
    CALL METHOD o_extract->build_dyn_range
      EXPORTING
        et_value  = lt_field
      IMPORTING
        it_ranges = pct_range2.
  ENDIF.
  REFRESH lt_field.
* Build Range table for Posting period
  IF s_monat1[] IS NOT INITIAL.
    LOOP AT s_monat1.
      ls_field-sign   = s_monat1-sign.
      ls_field-option = s_monat1-option.
      ls_field-low    = s_monat1-low.
      ls_field-high   = s_monat1-high.
      APPEND ls_field TO lt_field.
      CLEAR ls_field.
    ENDLOOP.
    CALL METHOD o_extract->build_dyn_range
      EXPORTING
        et_value  = lt_field
      IMPORTING
        it_ranges = pct_range3.
  ENDIF.
  REFRESH lt_field.
* Build Range table1
  IF s_field1[] IS NOT INITIAL.
    LOOP AT s_field1.
      ls_field-sign   = s_field1-sign.
      ls_field-option = s_field1-option.
      READ TABLE gt_ddfields INTO ls_ddfields
                             WITH KEY fieldtext = p_field1.
      IF sy-subrc IS INITIAL AND ls_ddfields-datatype = 'DATS'.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external            = s_field1-low
          IMPORTING
            date_internal            = ls_field-low
          EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          CLEAR ls_field-low.
        ENDIF.
      ELSE.
        ls_field-low    = s_field1-low.
      ENDIF.
      READ TABLE gt_ddfields INTO ls_ddfields
                            WITH KEY fieldtext = p_field1.
      IF sy-subrc IS INITIAL AND ls_ddfields-datatype = 'DATS'.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external            = s_field1-high
          IMPORTING
            date_internal            = ls_field-high
          EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          CLEAR ls_field-high.
        ENDIF.
      ELSE.
        ls_field-high   = s_field1-high.
      ENDIF.
      APPEND ls_field TO lt_field.
      CLEAR ls_field.
    ENDLOOP.
    CALL METHOD o_extract->build_dyn_range
      EXPORTING
        et_value  = lt_field
      IMPORTING
        it_ranges = pct_range4.
  ENDIF.
  REFRESH lt_field.
* Build Range table2
  IF s_field2[] IS NOT INITIAL.
    LOOP AT s_field2.
      ls_field-sign   = s_field2-sign.
      ls_field-option = s_field2-option.
      READ TABLE gt_ddfields INTO ls_ddfields
                             WITH KEY fieldtext = p_field2.
      IF sy-subrc IS INITIAL AND ls_ddfields-datatype = 'DATS'.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external            = s_field2-low
          IMPORTING
            date_internal            = ls_field-low
          EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          CLEAR ls_field-low.
        ENDIF.
      ELSE.
        ls_field-low    = s_field2-low.
      ENDIF.
      READ TABLE gt_ddfields INTO ls_ddfields
                            WITH KEY fieldtext = p_field2.
      IF sy-subrc IS INITIAL AND ls_ddfields-datatype = 'DATS'.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external            = s_field2-high
          IMPORTING
            date_internal            = ls_field-high
          EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          CLEAR ls_field-high.
        ENDIF.
      ELSE.
        ls_field-high   = s_field2-high.
      ENDIF.
      APPEND ls_field TO lt_field.
      CLEAR ls_field.
    ENDLOOP.
    CALL METHOD o_extract->build_dyn_range
      EXPORTING
        et_value  = lt_field
      IMPORTING
        it_ranges = pct_range5.
  ENDIF.
  REFRESH lt_field.
* Build Range table3
  IF s_field3[] IS NOT INITIAL.
    LOOP AT s_field3.
      ls_field-sign   = s_field3-sign.
      ls_field-option = s_field3-option.
      READ TABLE gt_ddfields INTO ls_ddfields
                            WITH KEY fieldtext = p_field3.
      IF sy-subrc IS INITIAL AND ls_ddfields-datatype = 'DATS'.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external            = s_field3-low
          IMPORTING
            date_internal            = ls_field-low
          EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          CLEAR ls_field-low.
        ENDIF.
      ELSE.
        ls_field-low    = s_field3-low.
      ENDIF.
      READ TABLE gt_ddfields INTO ls_ddfields
                            WITH KEY fieldtext = p_field3.
      IF sy-subrc IS INITIAL AND ls_ddfields-datatype = 'DATS'.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external            = s_field3-high
          IMPORTING
            date_internal            = ls_field-high
          EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          CLEAR ls_field-low.
        ENDIF.
      ELSE.
        ls_field-high   = s_field3-high.
      ENDIF.
      APPEND ls_field TO lt_field.
      CLEAR ls_field.
    ENDLOOP.
    CALL METHOD o_extract->build_dyn_range
      EXPORTING
        et_value  = lt_field
      IMPORTING
        it_ranges = pct_range6.
  ENDIF.
  REFRESH lt_field.
  READ TABLE gt_ddfields INTO ls_ddfields INDEX 1.
  IF ls_ddfields-fieldname EQ gc_spras.
    ls_field-sign   = 'I'.
    ls_field-option = 'EQ'.
    ls_field-low    = sy-langu.
    APPEND ls_field TO lt_field.
    CLEAR ls_field.

    CALL METHOD o_extract->build_dyn_range
      EXPORTING
        et_value  = lt_field
      IMPORTING
        it_ranges = pct_range7.
  ELSE.
    CLEAR ls_ddfields.
    READ TABLE gt_ddfields INTO ls_ddfields INDEX 2.
    IF ls_ddfields-fieldname EQ gc_spras.
      ls_field-sign   = 'I'.
      ls_field-option = 'EQ'.
      ls_field-low    = sy-langu.
      APPEND ls_field TO lt_field.
      CLEAR ls_field.

      CALL METHOD o_extract->build_dyn_range
        EXPORTING
          et_value  = lt_field
        IMPORTING
          it_ranges = pct_range7.
    ELSE.
      CLEAR ls_ddfields.
      READ TABLE gt_ddfields INTO ls_ddfields INDEX 3.
      IF ls_ddfields-fieldname EQ gc_spras.
        ls_field-sign   = 'I'.
        ls_field-option = 'EQ'.
        ls_field-low    = sy-langu.
        APPEND ls_field TO lt_field.
        CLEAR ls_field.

        CALL METHOD o_extract->build_dyn_range
          EXPORTING
            et_value  = lt_field
          IMPORTING
            it_ranges = pct_range7.
      ENDIF.
    ENDIF.
  ENDIF.
  REFRESH lt_field.
ENDFORM.                    " F_BUILD_SEL_RANGE
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FLDLIST
*&---------------------------------------------------------------------*
* Build fieldlist in where clause of select query
*----------------------------------------------------------------------*
*<-- PCT_FIELDLIST    Fieldlist from selection screen
*<-- PCT_DDFIELDS     Fieldlist from database table
*<-- PCT_WH_FIELDS    Fields in where clause of select query
*<-- PC_FIELD_SEQUENCE Field sequence in select query
*----------------------------------------------------------------------*
FORM f_build_fldlist CHANGING pct_fieldlist     TYPE tt_field_pos
                              pct_ddfields      TYPE tt_ddfields
                              pct_wh_fields     TYPE tt_wh_fields
                              pc_field_sequence TYPE char90.
  DATA:lt_wh_fields  TYPE STANDARD TABLE OF gty_wh_fields
                     INITIAL SIZE 0,
       ls_wh_fields  TYPE gty_wh_fields,
       ls_ddfields   TYPE dfies.
  FIELD-SYMBOLS:<lfs_ddfields> TYPE dfies.
  IF s_bukrs1 IS NOT INITIAL.
    READ TABLE pct_ddfields INTO ls_ddfields
                            WITH KEY fieldname = gc_bukrs.
    IF sy-subrc IS INITIAL.
      ls_wh_fields-fieldname = gc_bukrs.
    ELSE.
      READ TABLE pct_ddfields INTO ls_ddfields
                              WITH KEY domname = gc_bukrs.
      IF sy-subrc IS INITIAL.
        ls_wh_fields-fieldname = ls_ddfields-fieldname.
      ENDIF.
    ENDIF.
    IF s_bukrs1 IS NOT INITIAL.
      ls_wh_fields-range_num = 1.
    ENDIF.
    APPEND ls_wh_fields TO lt_wh_fields.
    CLEAR  ls_wh_fields.
  ENDIF.
  IF s_gjahr1 IS NOT INITIAL.
    READ TABLE pct_ddfields INTO ls_ddfields
                            WITH KEY fieldname = gc_gjahr.
    IF sy-subrc IS INITIAL.
      ls_wh_fields-fieldname = gc_gjahr.
    ELSE.
      READ TABLE pct_ddfields INTO ls_ddfields
                              WITH KEY domname = gc_gjahr.
      IF sy-subrc IS INITIAL.
        ls_wh_fields-fieldname = ls_ddfields-fieldname.
      ENDIF.
    ENDIF.
    IF s_gjahr1 IS NOT INITIAL.
      ls_wh_fields-range_num = 2.
    ENDIF.
    APPEND ls_wh_fields TO lt_wh_fields.
    CLEAR  ls_wh_fields.
  ENDIF.
  IF s_monat1 IS NOT INITIAL.
    READ TABLE pct_ddfields INTO ls_ddfields
                            WITH KEY fieldname = gc_monat.
    IF sy-subrc IS INITIAL.
      ls_wh_fields-fieldname = gc_monat.
    ELSE.
      READ TABLE pct_ddfields INTO ls_ddfields
                              WITH KEY domname = gc_monat.
      IF sy-subrc IS INITIAL.
        ls_wh_fields-fieldname = ls_ddfields-fieldname.
      ENDIF.
    ENDIF.
    IF s_monat1 IS NOT INITIAL.
      ls_wh_fields-range_num = 3.
    ENDIF.
    APPEND ls_wh_fields TO lt_wh_fields.
    CLEAR  ls_wh_fields.
  ENDIF.
  IF gv_field1 IS NOT INITIAL.
    ls_wh_fields-fieldname = gv_field1.
    IF s_field1 IS NOT INITIAL.
      ls_wh_fields-range_num = 4.
    ENDIF.
    APPEND ls_wh_fields TO lt_wh_fields.
    CLEAR  ls_wh_fields.
  ENDIF.
  IF gv_field2 IS NOT INITIAL.
    ls_wh_fields-fieldname = gv_field2.
    IF s_field2 IS NOT INITIAL.
      ls_wh_fields-range_num = 5.
    ENDIF.
    APPEND ls_wh_fields TO lt_wh_fields.
    CLEAR  ls_wh_fields.
  ENDIF.
  IF gv_field3 IS NOT INITIAL.
    ls_wh_fields-fieldname = gv_field3.
    IF s_field3 IS NOT INITIAL.
      ls_wh_fields-range_num = 6.
    ENDIF.
    APPEND ls_wh_fields TO lt_wh_fields.
    CLEAR  ls_wh_fields.
  ENDIF.
  READ TABLE pct_ddfields INTO ls_ddfields INDEX 1.
  IF ls_ddfields-fieldname = gc_spras.
    ls_wh_fields-fieldname = gc_spras.
    ls_wh_fields-range_num = 7.
    APPEND ls_wh_fields TO lt_wh_fields.
    CLEAR  ls_wh_fields.
  ELSE.
    READ TABLE pct_ddfields INTO ls_ddfields INDEX 2.
    IF ls_ddfields-fieldname = gc_spras.
      ls_wh_fields-fieldname = gc_spras.
      ls_wh_fields-range_num = 7.
      APPEND ls_wh_fields TO lt_wh_fields.
      CLEAR  ls_wh_fields.
    ELSE.
      READ TABLE pct_ddfields INTO ls_ddfields INDEX 3.
      IF ls_ddfields-fieldname = gc_spras.
        ls_wh_fields-fieldname = gc_spras.
        ls_wh_fields-range_num = 7.
        APPEND ls_wh_fields TO lt_wh_fields.
        CLEAR  ls_wh_fields.
      ENDIF.
    ENDIF.
  ENDIF.
  CALL METHOD o_extract->input_validations
    EXPORTING
      i_tabname        = gv_table
      it_fields        = lt_wh_fields
    IMPORTING
      et_fieldlist     = pct_fieldlist
      et_ddfields      = pct_ddfields
      e_field_sequence = pc_field_sequence
      e_pack_size      = gv_pack_size.
  IF lt_wh_fields IS NOT INITIAL.
    pct_wh_fields = lt_wh_fields.
  ENDIF.
  IF pct_ddfields IS NOT INITIAL.
    LOOP AT pct_ddfields ASSIGNING <lfs_ddfields>.
      TRANSLATE <lfs_ddfields>-fieldtext TO LOWER CASE.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_BUILD_FLDLIST
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV
*&---------------------------------------------------------------------*
* Display ALV report
*----------------------------------------------------------------------*
FORM f_display_alv .
*Display ALV
  CALL SCREEN 1001.
ENDFORM.                    " F_DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       PBO of ALV screen
*----------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  SET  PF-STATUS 'ZEXTRACT'.
  SET TITLEBAR   'ZTITLE1'.
ENDMODULE.                 " STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       PAI of ALV screen
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
*      Display ALV Report
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  DATA: ls_stable     TYPE lvc_s_stbl,
        ls_layout     TYPE lvc_s_layo.
  DATA:lv_alv_title   TYPE lvc_title.
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
*create object of alv grid
    CREATE OBJECT o_alvgd
      EXPORTING
        i_parent = o_dockingcontainer.
  ENDIF.

* Register event handler
  CONCATENATE gv_table text-t07 INTO lv_alv_title SEPARATED BY space.
  ls_layout-grid_title = lv_alv_title.
  ls_layout-zebra      = abap_true.
  ls_layout-cwidth_opt = abap_true.
  ls_layout-sel_mode   = 'A'.

*  Build filed catalog
  CALL METHOD o_extract->create_fieldcat
    EXPORTING
      i_dbtab     = gv_table
    IMPORTING
      et_fieldcat = gt_fieldcat.

  IF <fs_output> IS NOT INITIAL.
    CALL METHOD o_alvgd->set_table_for_first_display
      EXPORTING
        i_save                        = 'A'
        is_layout                     = ls_layout    " Layout
      CHANGING
        it_outtab                     = <fs_output>    " Output Table
        it_fieldcatalog               = gt_fieldcat    " Field Catalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
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
    ENDIF.
  ENDIF."endif it_alvout IS NOT INITIAL.
ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS
*&---------------------------------------------------------------------*
*    Select data dynamically from database
*----------------------------------------------------------------------*
FORM f_process .

  TYPES : BEGIN OF lty_bkpf,
          bukrs TYPE bukrs,
          belnr	TYPE belnr_d,
          gjahr	TYPE gjahr,
        END OF lty_bkpf.

  DATA:lt_dfies TYPE STANDARD TABLE OF dfies INITIAL SIZE 0,
       ls_dfies TYPE dfies,
       lt_bkpf TYPE TABLE OF lty_bkpf,
       lwa_bkpf TYPE lty_bkpf.
* Build Dynamic Data declaration
  CALL METHOD o_extract->build_dynamic_table
    EXPORTING
      i_tabname = gv_table
    RECEIVING
      et_inttab = gt_inttab.
* Build SELECT statement
* get nametab
  CALL FUNCTION 'DDIF_NAMETAB_GET'
    EXPORTING
      tabname   = gv_table
    TABLES
      dfies_tab = lt_dfies
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc = 0.
** Logic for calculating Package size
* To calculate the memory taken by one record
    LOOP AT lt_dfies INTO ls_dfies.
      gv_pack_size = gv_pack_size + ls_dfies-leng.
    ENDLOOP.
  ENDIF.

* Start of Add by KOTTAPAN for CHG0206319
*  IF gv_table <> gc_bseg AND gv_table <> gc_prps. " Commented by KOTTAPAN for CHG0217805
  IF gv_table <> gc_bseg. " Added by KOTTAPAN for CHG0217805
    PERFORM build_select_query USING gv_table
                                     gt_wh_fields
                                     gt_fieldlist
                                     gr_field1
                                     gr_field2
                                     gr_field3
                                     gr_field4
                                     gr_field5
                                     gr_field6
                                     gr_field7
                                     gv_pack_size
                            CHANGING gt_inttab.

* Start of Add by KOTTAPAN for CHG0217805
    IF ( gv_table = gc_prps OR gv_table = gc_bkpf
        OR gv_table = gc_bsis ) AND rb_xls IS NOT INITIAL.    "Added by DADIM for CHG0244956
      ASSIGN gt_inttab->* TO <fs_output>.
      PERFORM f_download_appl_server USING  <fs_output>.
    ENDIF.
* End of Add by KOTTAPAN for CHG0217805

  ELSEIF gv_table = gc_bseg.

    SELECT bukrs
           belnr
           gjahr  FROM bkpf INTO TABLE lt_bkpf
           WHERE bukrs IN s_bukrs1 AND
                 gjahr IN s_gjahr1 AND
                 budat IN s_budat.

    IF lt_bkpf IS NOT INITIAL.
      ASSIGN gt_inttab->* TO <fs_output>.
      SELECT * FROM bseg INTO TABLE <fs_output>
        FOR ALL ENTRIES IN lt_bkpf
        WHERE  bukrs = lt_bkpf-bukrs AND
               belnr = lt_bkpf-belnr AND
               gjahr = lt_bkpf-gjahr.

      PERFORM f_download_appl_server USING  <fs_output>.
    ENDIF.

    " Start of comment by KOTTAPAN for CHG0217805
*  ELSEIF gv_table = gc_prps.
*
*    ASSIGN gt_inttab->* TO <fs_output>.
*    PERFORM build_select_query USING gv_table
*                                   gt_wh_fields
*                                   gt_fieldlist
*                                   gr_field1
*                                   gr_field2
*                                   gr_field3
*                                   gr_field4
*                                   gr_field5
*                                   gr_field6
*                                   gr_field7
*                                   gv_pack_size
*                          CHANGING gt_inttab.
*    ASSIGN gt_inttab->* TO <fs_output>.
*    PERFORM f_download_appl_server USING  <fs_output>.
    " End of comment by KOTTAPAN for CHG0217805
  ENDIF.

* Commented by KOTTAPAN for CHG0206319
*  CALL METHOD o_extract->build_select_query
*    EXPORTING
*      i_dbtab           = gv_table
*      it_fields_where   = gt_wh_fields
*      it_field_where_db = gt_fieldlist
*      it_range1         = gr_field1
*      it_range2         = gr_field2
*      it_range3         = gr_field3
*      it_range4         = gr_field4
*      it_range5         = gr_field5
*      it_range6         = gr_field6
*      it_range7         = gr_field7
*      i_pack_size       = gv_pack_size
*    CHANGING
*      ct_inttab         = gt_inttab.
* End of Add by KOTTAPAN for CHG0206319
ENDFORM.                    " F_PROCESS
*&---------------------------------------------------------------------*
*&      Form  F_OUTPUT
*&---------------------------------------------------------------------*
*  Display ALV/Download to excel /Send mail based on mode of output
*  selected by user
*----------------------------------------------------------------------*
FORM f_output .
  DATA:    lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex,
           lv_blocksize                TYPE syindex,
           ls_ddfields                 TYPE dfies,
           lv_exit_flag                TYPE flag,
           lv_file_len                 TYPE i.
  CONSTANTS:lc_mandt         TYPE fieldname VALUE 'MANDT'.
  DATA:lt_ddfields TYPE STANDARD TABLE OF dfies INITIAL SIZE 0.
*Append client data
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = gv_table
      fieldname      = lc_mandt
      langu          = sy-langu
    TABLES
      dfies_tab      = lt_ddfields
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc = 0.
    READ TABLE lt_ddfields INTO ls_ddfields INDEX 1.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  ELSE.
*Append client data
    CLEAR ls_ddfields.
    ls_ddfields-domname = lc_mandt.
    APPEND ls_ddfields TO lt_ddfields.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = gv_table
        langu          = sy-langu
      TABLES
        dfies_tab      = lt_ddfields
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      CLEAR ls_ddfields.
      READ TABLE lt_ddfields INTO ls_ddfields INDEX 1.
    ENDIF."endif call on ddif_fieldinfo_get call 2
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  ENDIF.
  INSERT ls_ddfields INTO gt_ddfields INDEX 1.
  lv_file_len = strlen( p_fext2 ).
  IF lv_file_len LE 3.
    gv_ext2 = p_fext2.
  ELSE.
    gv_ext3 = p_fext2.
  ENDIF.
  TRANSLATE :gv_ext2 TO UPPER CASE,
             gv_ext3 TO UPPER CASE.
  IF gt_inttab IS NOT INITIAL.
    ASSIGN gt_inttab->* TO <fs_output>.
  ENDIF.
  IF <fs_output> IS ASSIGNED.
    IF <fs_output> IS NOT INITIAL.
      IF sy-batch IS INITIAL.
        IF  rb_alv EQ gc_x."gc_set'.
          PERFORM f_display_alv."Display ALV data
        ELSEIF rb_xls EQ gc_x.
*          IF gv_table <> gc_bseg AND gv_table <> gc_prps.      "Added by KOTTAPAN for CHG0216879
          IF gv_table <> gc_bseg AND gv_table <> gc_prps AND gv_table <> gc_bkpf "Added by KOTTAPAN for CHG0217805
             AND gv_table <> gc_bsis.                                            "Added by DADIM for CHG0244956
            PERFORM f_download_pc."Download to excel
          ENDIF.
        ELSEIF rb_mail EQ gc_x.
          IF gv_ext2 = 'XLS'  "Excel file
          OR gv_ext3 = 'XLSX'. "XLSX file .
            PERFORM f_create_xls_attach  CHANGING gt_output.
          ELSE.
            PERFORM f_create_oth_attach  CHANGING gt_output.
          ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
          IF p_dpbc1 IS NOT INITIAL.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
            lv_blocksize = p_dpbc1.
            DO.
              IF lv_exit_flag EQ gc_x.
                EXIT. "loop termination
              ENDIF.
* Calculate the low and high indices for the batch of WBS elements
              lv_index    =     sy-index.
              lv_index_lo = ( ( lv_index - 1 ) * lv_blocksize ) + 1.
              lv_index_hi = (   lv_index       * lv_blocksize ).
* Send mail with extract file attachment
              PERFORM f_send_mail
                USING gt_output  lv_index_lo lv_index_hi
             CHANGING lv_exit_flag.
            ENDDO.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
          ELSE.
* Calculate the low and high indices for the batch of WBS elements
            lv_index_lo =  1.
            DESCRIBE TABLE gt_output LINES lv_index_hi.
* Send mail with extract file attachment
            PERFORM f_send_mail_allfile
              USING gt_output  lv_index_lo lv_index_hi.
          ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
        ENDIF.
      ELSE.
        IF rb_mail IS NOT INITIAL.
          IF  gv_ext2 = 'XLS' "Excel file
           OR gv_ext3 = 'XLSX'. "XLSX file
            PERFORM f_create_xls_attach  CHANGING gt_output.
          ELSE.
            PERFORM f_create_oth_attach  CHANGING gt_output.
          ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
          IF p_dpbc1 IS NOT INITIAL.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
            lv_blocksize = p_dpbc1.
            DO.
              IF lv_exit_flag EQ gc_x.
                EXIT. "loop termination
              ENDIF.
* Calculate the low and high indices for the extract records
              lv_index    =     sy-index.
              lv_index_lo = ( ( lv_index - 1 ) * lv_blocksize ) + 1.
              lv_index_hi = (   lv_index       * lv_blocksize ).
* Send mail with extract file attachment
              PERFORM f_send_mail
                USING gt_output lv_index_lo lv_index_hi
              CHANGING lv_exit_flag.
            ENDDO.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
          ELSE.
* Calculate the low and high indices for the batch of WBS elements
            lv_index_lo = 1.
            DESCRIBE TABLE gt_output LINES lv_index_hi.
* Send mail with extract file attachment
            PERFORM f_send_mail_allfile
              USING gt_output  lv_index_lo lv_index_hi.
          ENDIF."selection screen package size check
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
        ENDIF."rb_mail initial check
        IF rb_alv IS NOT INITIAL.
          PERFORM f_display_spool.
        ENDIF.
      ENDIF."sy-batch initial check
    ELSE.
      MESSAGE s000(zfi01) WITH text-e01 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING .
    ENDIF."<fs_output> initial check
  ENDIF."<fs_output> assigned check
ENDFORM.                    " F_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATIONS
*&---------------------------------------------------------------------*
*       Validate user inputs
*----------------------------------------------------------------------*
FORM f_validations .
  DATA:lv_ext TYPE char3.
  IF p_fext1 IS NOT INITIAL.
    lv_ext = p_fext1.
  ELSEIF p_fext2 IS NOT INITIAL.
    lv_ext = p_fext2.
  ENDIF.
  TRANSLATE lv_ext TO UPPER CASE.
*Validate the table name entered
  CALL METHOD o_extract->validate_table
    EXPORTING
      i_table = gv_table.

*Validate file delimiter for excel file
  IF lv_ext CS 'XLS'.
    IF p_fdlm1 IS NOT INITIAL AND rb_xls IS NOT INITIAL.
      MESSAGE e000(zfi01) WITH text-e02.
      LEAVE LIST-PROCESSING .
    ELSEIF p_fdlm2 IS NOT INITIAL AND rb_mail IS NOT INITIAL.
      MESSAGE e000(zfi01) WITH text-e02.
      LEAVE LIST-PROCESSING .
    ENDIF.
  ENDIF.
*Validate execution mode
  IF sy-batch IS NOT INITIAL.
    IF rb_xls IS NOT INITIAL OR rb_alv IS NOT INITIAL.
      "MESSAGE e000(zfi01) WITH text-e03.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATIONS
*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZE
*&---------------------------------------------------------------------*
*    Get initial values for program execution
*----------------------------------------------------------------------*
FORM f_initialize .
*Instantiate object of class ZFI_TABLE_EXTRACT
  CREATE OBJECT o_extract.
  CLEAR:  gv_field1,gv_field2,gv_field3,gv_file_size,gv_file_size_brk,
          gv_ext2,gv_table.
  REFRESH:gr_field1  ,gr_field2  ,gr_field3   ,gr_field4   ,gr_field5,
          gr_field6  ,gt_fieldcat,gt_ddfields ,gt_wh_fields,
          gt_fieldlist,gt_tabf4  ,gt_output.
  CLEAR:  p_field1,p_field2,p_field3.
  CLEAR:  gv_field1,gv_field2,gv_field3.
ENDFORM.                    " F_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_FIELD
*&---------------------------------------------------------------------*
*   Validate if values for fields entered are fine
*----------------------------------------------------------------------*
* -->PI_FIELDNAME  Field entered by user in selection criteria
* -->PIT_WH_FIELDS Where condition foe select query with input fields
* -->PIT_DDFIELDS  Fields in database table
* -->PIT_RANGE     Dynamic ranges for selection options values
*----------------------------------------------------------------------*
FORM f_validate_field USING pi_fieldname  TYPE fieldname
                            pit_wh_fields TYPE tt_wh_fields
                            pit_ddfields  TYPE tt_ddfields
                            pit_range     TYPE /sapdii/twty_rsds_selopt.
  DATA:lt_ddfields    TYPE STANDARD TABLE OF dfies INITIAL SIZE 0.
  DATA:lv_check_table TYPE checktable.
  DATA:lv_fieldname   TYPE fieldname.
  DATA:ls_ddfields    TYPE dfies.
  IF pit_wh_fields IS NOT INITIAL.
    IF pit_ddfields IS NOT INITIAL.
      lt_ddfields = pit_ddfields.
      SORT lt_ddfields BY fieldname.
    ENDIF.
    READ TABLE lt_ddfields INTO ls_ddfields
                           WITH KEY fieldname = pi_fieldname.
    IF sy-subrc IS INITIAL.
      IF ls_ddfields-datatype NE 'DATS'.
        IF ls_ddfields-fieldname IS NOT INITIAL.
          IF ls_ddfields-checktable IS NOT INITIAL.
            lv_check_table = ls_ddfields-checktable.
            IF ls_ddfields-fieldname = ls_ddfields-domname.
              lv_fieldname   = ls_ddfields-fieldname.
            ELSE.
              lv_fieldname   = ls_ddfields-domname.
            ENDIF.
          ELSE.
            lv_check_table = gv_table.
            lv_fieldname   = ls_ddfields-fieldname.
          ENDIF.
          IF pit_range IS NOT INITIAL.
            CALL METHOD o_extract->validate_data
              EXPORTING
                i_field_name   = lv_fieldname
                it_range_valid = pit_range
                i_check_tab    = lv_check_table.
          ENDIF."endif pit_range is not initial
        ENDIF."endif ls_ddfields-fieldname IS NOT INITIAL
      ENDIF.
    ENDIF."endif lt_ddfields read check
  ENDIF."endif  pit_wh_fields
ENDFORM.                    " F_VALIDATE_FIELD
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_INPUT
*&---------------------------------------------------------------------*
*      Validate user input values for fields and tables
*----------------------------------------------------------------------*
FORM f_validate_input .
  DATA:ls_ddfields TYPE dfies.
  REFRESH:gr_field1, gr_field2, gr_field3, gr_field4, gr_field5,
          gr_field6, gr_field7.
  REFRESH:gt_selfld_value,gt_wh_fields,gt_fieldlist.
  IF p_field1 IS NOT INITIAL .
    READ TABLE gt_ddfields WITH KEY fieldtext = p_field1
                           TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      CLEAR:p_field1.
      REFRESH:s_field1.
    ENDIF.
    PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                       gr_field4 gr_field5 gr_field6
                                       gr_field7.
*Fill global field values
    PERFORM f_set_values         USING gt_selfld_value gt_ddfields.
  ENDIF.
  IF p_field2 IS NOT INITIAL .
    READ TABLE gt_ddfields WITH KEY fieldtext = p_field2
                           TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      CLEAR:p_field2.
      REFRESH:s_field2.
    ELSE.
      PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                         gr_field4 gr_field5 gr_field6
                                         gr_field7.
*Fill global field values
      PERFORM f_set_values         USING gt_selfld_value gt_ddfields.
    ENDIF.
  ENDIF.
  IF p_field3 IS NOT INITIAL .
    READ TABLE gt_ddfields WITH KEY fieldtext = p_field3
                           TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      CLEAR:p_field3.
      REFRESH:s_field3.
    ELSE.
      PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                         gr_field4 gr_field5 gr_field6
                                         gr_field7.
*Fill global field values
      PERFORM f_set_values         USING gt_selfld_value gt_ddfields.
    ENDIF.
  ENDIF.
  IF s_bukrs1 IS NOT INITIAL.
    READ TABLE gt_ddfields WITH KEY fieldname = gc_bukrs
                           TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE gt_ddfields WITH KEY domname = gc_bukrs
                             TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        REFRESH:s_bukrs1.
      ELSE.
        PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                           gr_field4 gr_field5 gr_field6
                                           gr_field7.
*Fill global field values
        PERFORM f_set_values        USING gt_selfld_value gt_ddfields.
      ENDIF.
    ELSE.
      PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                         gr_field4 gr_field5 gr_field6
                                         gr_field7.
*Fill global field values
      PERFORM f_set_values         USING gt_selfld_value gt_ddfields.
    ENDIF.
  ENDIF.
  IF s_monat1 IS NOT INITIAL.
    READ TABLE gt_ddfields WITH KEY fieldname = gc_monat
                           TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE gt_ddfields WITH KEY domname = gc_monat
                             TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        REFRESH:s_monat1.
      ELSE.
        PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                           gr_field4 gr_field5 gr_field6
                                           gr_field7.
*Fill global field values
        PERFORM f_set_values         USING gt_selfld_value gt_ddfields.

      ENDIF.
    ELSE.
      PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                         gr_field4 gr_field5 gr_field6
                                         gr_field7.
*Fill global field values
      PERFORM f_set_values         USING gt_selfld_value gt_ddfields.
    ENDIF.
  ENDIF.
  IF s_gjahr1 IS NOT INITIAL.
    READ TABLE gt_ddfields WITH KEY fieldname = gc_gjahr
                           TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE gt_ddfields WITH KEY domname = gc_gjahr
                             TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        REFRESH:s_gjahr1.
      ELSE.
        PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                           gr_field4 gr_field5 gr_field6
                                           gr_field7.
*Fill global field values
        PERFORM f_set_values        USING gt_selfld_value gt_ddfields.
      ENDIF.
    ELSE.
      PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                         gr_field4 gr_field5 gr_field6
                                         gr_field7.
*Fill global field values
      PERFORM f_set_values         USING gt_selfld_value gt_ddfields.
    ENDIF.
  ENDIF.
  READ TABLE gt_ddfields INTO ls_ddfields INDEX 1.
  IF ls_ddfields-fieldname = gc_spras.
    PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                       gr_field4 gr_field5 gr_field6
                                       gr_field7.
*Fill global field values
    PERFORM f_set_values         USING gt_selfld_value gt_ddfields.
  ELSE.
    READ TABLE gt_ddfields INTO ls_ddfields INDEX 2.
    IF ls_ddfields-fieldname = gc_spras.
      PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                         gr_field4 gr_field5 gr_field6
                                         gr_field7.
*Fill global field values
      PERFORM f_set_values         USING gt_selfld_value gt_ddfields.
    ELSE.
      READ TABLE gt_ddfields INTO ls_ddfields INDEX 3.
      IF ls_ddfields-fieldname = gc_spras.
        PERFORM f_build_sel_range CHANGING gr_field1 gr_field2 gr_field3
                                           gr_field4 gr_field5 gr_field6
                                           gr_field7.
*Fill global field values
        PERFORM f_set_values         USING gt_selfld_value gt_ddfields.
      ENDIF.
    ENDIF.
  ENDIF.
  IF p_field1 IS INITIAL.
    CLEAR gv_field1.
  ENDIF.
  IF p_field2 IS INITIAL.
    CLEAR gv_field2.
  ENDIF.
  IF p_field3 IS INITIAL.
    CLEAR gv_field3.
  ENDIF.
*Get the list of fields in the database table and their position in
*database table
  PERFORM f_build_fldlist   CHANGING gt_fieldlist gt_ddfields
                                     gt_wh_fields gv_field_sequence.
*Check and apply conversion exits for fields entered if required
  IF p_field1 IS NOT INITIAL.
    PERFORM f_apply_convexit USING gv_field1 CHANGING gr_field4.
  ENDIF.
  IF p_field2 IS NOT INITIAL.
    PERFORM f_apply_convexit USING gv_field2 CHANGING gr_field5.
  ENDIF.
  IF p_field3 IS NOT INITIAL.
    PERFORM f_apply_convexit USING gv_field3 CHANGING gr_field6.
  ENDIF.
*Validate the input fields from selection screen
  IF s_bukrs1 IS NOT INITIAL.
    PERFORM f_validate_field USING 'BUKRS' gt_wh_fields gt_ddfields
                                           gr_field1.
  ENDIF.
  IF s_gjahr1 IS NOT INITIAL.
    PERFORM f_validate_field USING 'GJAHR' gt_wh_fields gt_ddfields
                                           gr_field2.
  ENDIF.
  IF s_monat1 IS NOT INITIAL.
    PERFORM f_validate_field USING 'MONAT' gt_wh_fields gt_ddfields
                                           gr_field3.
  ENDIF.

  IF gv_field1 IS NOT INITIAL.
    PERFORM f_validate_field USING gv_field1 gt_wh_fields gt_ddfields
                                   gr_field4.
  ENDIF.
  IF gv_field2 IS NOT INITIAL.
    PERFORM f_validate_field USING gv_field2 gt_wh_fields gt_ddfields
                                   gr_field5.
  ENDIF.
  IF gv_field3 IS NOT INITIAL.
    PERFORM f_validate_field USING gv_field3 gt_wh_fields gt_ddfields
                                   gr_field6.
  ENDIF.
ENDFORM.                    " F_VALIDATE_INPUT
*&---------------------------------------------------------------------*
*&      Form  F_CUSTOM_F4
*&---------------------------------------------------------------------*
*       Custom F4 values for fields
*----------------------------------------------------------------------*
*-->PI_FIELDNAME      Fieldname for which F4 has to be bilut
*<-->PCT_TABF4         Table for F4 Help values
*<-->PCT_DDFIELDS      Fields in database table
*----------------------------------------------------------------------*
FORM f_custom_f4_fld  USING pi_fieldname      TYPE dynfnam
                   CHANGING pct_tabf4         TYPE tt_tabf4
                            pct_ddfields      TYPE tt_ddfields
                            pct_selfld_value  TYPE cms_tab_field_values.

  DATA:ls_ddfields  TYPE dfies.
  DATA ls_tabf4     TYPE gty_tabf4.
  DATA:lt_retvalues TYPE STANDARD TABLE OF ddshretval
                    INITIAL SIZE 0.
  REFRESH :pct_tabf4,pct_ddfields,pct_selfld_value.
  PERFORM f_get_table_val CHANGING pct_ddfields.
  IF pct_ddfields IS NOT INITIAL.
    LOOP AT pct_ddfields INTO ls_ddfields.
      ls_tabf4-fieldname   = ls_ddfields-fieldname.
      ls_tabf4-domname     = ls_ddfields-domname.
      ls_tabf4-description = ls_ddfields-fieldtext.
      TRANSLATE ls_tabf4-description TO LOWER CASE.
      APPEND ls_tabf4 TO pct_tabf4.
      CLEAR :ls_tabf4,ls_ddfields.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DESCRIPTION'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = pi_fieldname
      value_org       = 'S'
    TABLES
      value_tab       = pct_tabf4
      return_tab      = lt_retvalues
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc = 0.
* Implement suitable error handling here
    pct_selfld_value = lt_retvalues.
  ENDIF.

ENDFORM.                    " F_CUSTOM_F4
*&---------------------------------------------------------------------*
*&      Form  F_CUSTOM_SHLP
*&---------------------------------------------------------------------*
*       Create custom serach help
*----------------------------------------------------------------------*
*--> PI_FIELDNAME      Field name entered by user
*<-- PCT_TABF4        Table name for F4 help values
*<-- PCT_DDFIELDS     Database fields
*--> PIT_SELFLD_VAL   Selected field
*----------------------------------------------------------------------*
FORM f_custom_shlp USING pi_fieldname      TYPE dynfnam
                         pit_selfld_value  TYPE crmt_ic_ui_ddshretval
                CHANGING pct_tabf4         TYPE tt_tabf4
                         pct_ddfields      TYPE tt_ddfields.


  DATA:ls_selfld_value  TYPE ddshretval,
       ls_ddfields      TYPE dfies,
       ls_tabf4         TYPE gty_tabf4,
       lt_dynpfields    LIKE dynpread OCCURS 0 WITH HEADER LINE,
       ls_dynpfields    TYPE dynpread,
       lt_dd03p         TYPE STANDARD TABLE OF dd03p INITIAL SIZE 0,
       ls_dd03p         TYPE dd03p,
       lt_retvalues     TYPE STANDARD TABLE OF ddshretval
                        INITIAL SIZE 0,
       ls_retvalues     TYPE ddshretval,
       ls_selval        TYPE help_info-fldvalue.
  DATA:lv_field1        TYPE as4text,
       lv_field2        TYPE as4text,
       lv_field3        TYPE as4text,
       lv_seldate       TYPE workflds-gkday,
       lv_seldate_ext   TYPE char10,
       lv_seltime       TYPE sy-uzeit,
       lv_db_field      TYPE fieldname.
  DATA:lv_explicit_shlp   TYPE flag,"shlpname,
       lv_collective_shlp TYPE flag."shlpname.
  CONSTANTS:lc_x TYPE shlporigin VALUE 'X',
            lc_p TYPE shlporigin VALUE 'P',
            lc_d TYPE shlporigin VALUE 'D',
            lc_f TYPE shlporigin VALUE 'F',
            lc_t TYPE shlporigin VALUE 'T'.
  REFRESH:s_field1.
  IF pct_ddfields IS NOT INITIAL.
    LOOP AT pct_ddfields INTO ls_ddfields.
      ls_tabf4-fieldname   = ls_ddfields-fieldname.
      ls_tabf4-description = ls_ddfields-fieldtext.
      TRANSLATE ls_tabf4-description TO LOWER CASE.
      APPEND ls_tabf4 TO pct_tabf4.
      CLEAR :ls_tabf4,ls_ddfields.
    ENDLOOP.
  ENDIF.
  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name          = gv_table
    TABLES
      dd03p_tab     = lt_dd03p
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc = 0.
* Implement suitable error handling here
    SORT lt_dd03p BY fieldname.
  ENDIF.

*Get field values
  IF pit_selfld_value IS INITIAL.
    IF    pi_fieldname = 'S_FIELD1-LOW'
       OR pi_fieldname = 'S_FIELD1-HIGH'.
      MOVE 'P_FIELD1' TO lt_dynpfields-fieldname.
    ELSEIF pi_fieldname = 'S_FIELD2-LOW'
        OR pi_fieldname = 'S_FIELD2-HIGH'.
      MOVE 'P_FIELD2' TO lt_dynpfields-fieldname.
    ELSEIF pi_fieldname = 'S_FIELD3-LOW'
        OR pi_fieldname = 'S_FIELD3-HIGH'.
      MOVE 'P_FIELD3' TO lt_dynpfields-fieldname.
    ENDIF.
    APPEND lt_dynpfields.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = lt_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc = 0.
      READ TABLE lt_dynpfields INTO ls_dynpfields INDEX 1.
      IF sy-subrc IS INITIAL.
*Get selected table field name
        IF pi_fieldname = 'S_FIELD1-LOW'
        OR pi_fieldname = 'S_FIELD1-HIGH'.
          lv_field1 = ls_dynpfields-fieldvalue.
          READ TABLE pct_tabf4 INTO ls_tabf4
                               WITH KEY description = lv_field1.
          IF sy-subrc IS INITIAL.
            gv_field1 = ls_tabf4-fieldname.
          ENDIF.
        ELSEIF pi_fieldname = 'S_FIELD2-LOW'
            OR pi_fieldname = 'S_FIELD2-HIGH'.
          lv_field2 = ls_dynpfields-fieldvalue.
          READ TABLE pct_tabf4 INTO ls_tabf4
                               WITH KEY description = lv_field2.
          IF sy-subrc IS INITIAL.
            gv_field2 = ls_tabf4-fieldname.
          ENDIF.
        ELSEIF pi_fieldname = 'S_FIELD3-LOW'
            OR pi_fieldname = 'S_FIELD3-HIGH'.
          lv_field3 = ls_dynpfields-fieldvalue.
          READ TABLE pct_tabf4 INTO ls_tabf4
                               WITH KEY description = lv_field3.
          IF sy-subrc IS INITIAL.
            gv_field3 = ls_tabf4-fieldname.
          ENDIF."endif check on pct_tabf4 read
        ENDIF."endif pi_fieldname check
      ENDIF.
    ENDIF.
  ELSE.
* Implement suitable error handling here
    READ TABLE pit_selfld_value INTO ls_selfld_value INDEX 1.
    IF sy-subrc IS INITIAL.
*Get selected table field name
      IF pi_fieldname = 'S_FIELD1-LOW'
      OR pi_fieldname = 'S_FIELD1-HIGH'.
        lv_field1 = ls_selfld_value-fieldval.
        READ TABLE pct_tabf4 INTO ls_tabf4
                             WITH KEY description = lv_field1.
        IF sy-subrc IS INITIAL.
          gv_field1 = ls_tabf4-fieldname.
        ENDIF.
      ELSEIF pi_fieldname = 'S_FIELD2-LOW'
          OR pi_fieldname = 'S_FIELD2-HIGH'.
        lv_field2 = ls_selfld_value-fieldval.
        READ TABLE pct_tabf4 INTO ls_tabf4
                             WITH KEY description = lv_field2.
        IF sy-subrc IS INITIAL.
          gv_field2 = ls_tabf4-fieldname.
        ENDIF.
      ELSEIF pi_fieldname = 'S_FIELD3-LOW'
          OR pi_fieldname = 'S_FIELD3-HIGH'.
        lv_field3 = ls_selfld_value-fieldval.
        READ TABLE pct_tabf4 INTO ls_tabf4
                             WITH KEY description = lv_field3.
        IF sy-subrc IS INITIAL.
          gv_field3 = ls_tabf4-fieldname.
        ENDIF."endif check on pct_tabf4 read
      ENDIF."endif pi_fieldname check
    ENDIF."endif read on pit_selfld_value
  ENDIF.
  READ TABLE lt_dd03p INTO ls_dd03p
                      WITH KEY fieldname = ls_tabf4-fieldname
                      BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    CASE ls_dd03p-shlporigin.
      WHEN lc_p. "Checktable based
        REFRESH lt_retvalues.
        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = gv_table
            fieldname         = ls_dd03p-fieldname
            value             = ls_selval
            selection_screen  = 'X'
          TABLES
            return_tab        = lt_retvalues
          EXCEPTIONS
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            OTHERS            = 5.
        IF sy-subrc IS NOT INITIAL.
          REFRESH lt_retvalues.
        ENDIF.
      WHEN lc_f. "domain values
        REFRESH lt_retvalues.
        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname           = gv_table
            fieldname         = ls_dd03p-fieldname
            value             = ls_selval
            selection_screen  = 'X'
          TABLES
            return_tab        = lt_retvalues
          EXCEPTIONS
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            OTHERS            = 5.
        IF sy-subrc IS NOT INITIAL.
          REFRESH lt_retvalues.
        ENDIF.
      WHEN lc_x."Explicit search help to field
        lv_collective_shlp = 'X'.
      WHEN lc_d."Explicit search help to data element
        lv_explicit_shlp   = 'X'.
      WHEN lc_t."input help based on data type DATS and TIMS
        CALL FUNCTION 'F4_DATE'
          EXPORTING
            date_for_first_month         = sy-datum
          IMPORTING
            select_date                  = lv_seldate
          EXCEPTIONS
            calendar_buffer_not_loadable = 1
            date_after_range             = 2
            date_before_range            = 3
            date_invalid                 = 4
            factory_calendar_not_found   = 5
            holiday_calendar_not_found   = 6
            parameter_conflict           = 7
            OTHERS                       = 8.
        IF sy-subrc <> 0.
* Implement suitable error handling here
          CLEAR lv_seldate.
        ENDIF.
        IF lv_seldate IS INITIAL.
          CALL FUNCTION 'F4_CLOCK'
            EXPORTING
              start_time    = sy-uzeit
            IMPORTING
              selected_time = lv_seltime.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    IF lv_explicit_shlp IS NOT INITIAL.
      CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
        EXPORTING
          tabname           = gv_table
          fieldname         = ls_dd03p-fieldname
          value             = ls_selval
          selection_screen  = 'X'
        TABLES
          return_tab        = lt_retvalues
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          no_values_found   = 4
          OTHERS            = 5.
      IF sy-subrc = 0.
* Implement suitable error handling here
        SORT lt_retvalues.
      ENDIF.
    ELSEIF lv_collective_shlp IS NOT INITIAL.
      CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
        EXPORTING
          tabname           = gv_table
          fieldname         = ls_dd03p-fieldname
          value             = ls_selval
          selection_screen  = 'X'
        TABLES
          return_tab        = lt_retvalues
        EXCEPTIONS
          field_not_found   = 1
          no_help_for_field = 2
          inconsistent_help = 3
          no_values_found   = 4
          OTHERS            = 5.
      IF sy-subrc = 0.
* Implement suitable error handling here
        SORT lt_retvalues.
      ENDIF.
    ELSE.
      CASE pi_fieldname.
        WHEN 'S_FIELD1-LOW'.
          s_field1-sign   = 'I'.
          s_field1-option = 'EQ'.
          IF lv_seldate IS NOT INITIAL.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lv_seldate
              IMPORTING
                date_external            = lv_seldate_ext
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc = 0.
              s_field1-low = lv_seldate_ext.
            ENDIF.
            APPEND s_field1.
          ENDIF.
          IF lv_seltime  IS NOT INITIAL.
            s_field1-low = lv_seltime.
            APPEND s_field1.
          ENDIF.
        WHEN 'S_FIELD2-LOW'.
          s_field2-sign   = 'I'.
          s_field2-option = 'EQ'.
          IF lv_seldate IS NOT INITIAL.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lv_seldate
              IMPORTING
                date_external            = lv_seldate_ext
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc = 0.
              s_field2-low = lv_seldate_ext.
            ENDIF.
            APPEND s_field2.
          ENDIF.
          IF lv_seltime  IS NOT INITIAL.
            s_field2-low = lv_seltime.
            APPEND s_field2.
          ENDIF.
        WHEN 'S_FIELD3-LOW'.
          s_field3-sign   = 'I'.
          s_field3-option = 'EQ'.
          IF lv_seldate IS NOT INITIAL.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lv_seldate
              IMPORTING
                date_external            = lv_seldate_ext
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc = 0.
              s_field3-low = lv_seldate_ext.
            ENDIF.
            APPEND s_field3.
          ENDIF.
          IF lv_seltime  IS NOT INITIAL.
            s_field3-low = lv_seltime.
            APPEND s_field3.
          ENDIF.
        WHEN 'S_FIELD1-HIGH'.
          s_field1-sign   = 'I'.
          s_field1-option = 'EQ'.
          IF lv_seldate IS NOT INITIAL.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lv_seldate
              IMPORTING
                date_external            = lv_seldate_ext
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc = 0.
              s_field1-high = lv_seldate_ext.
            ENDIF.
            APPEND s_field1.
          ENDIF.
          IF lv_seltime  IS NOT INITIAL.
            s_field1-high = lv_seltime.
            APPEND s_field1.
          ENDIF.
        WHEN 'S_FIELD2-HIGH'.
          s_field2-sign   = 'I'.
          s_field2-option = 'EQ'.
          IF lv_seldate IS NOT INITIAL.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lv_seldate
              IMPORTING
                date_external            = lv_seldate_ext
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc = 0.
              s_field2-high = lv_seldate_ext.
            ENDIF.
            APPEND s_field2.
          ENDIF.
          IF lv_seltime  IS NOT INITIAL.
            s_field2-high = lv_seltime.
            APPEND s_field2.
          ENDIF.
        WHEN 'S_FIELD3-HIGH'.
          s_field3-sign   = 'I'.
          s_field3-option = 'EQ'.
          IF lv_seldate IS NOT INITIAL.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = lv_seldate
              IMPORTING
                date_external            = lv_seldate_ext
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc = 0.
              s_field3-high = lv_seldate_ext.
            ENDIF.
            APPEND s_field3.
          ENDIF.
          IF lv_seltime  IS NOT INITIAL.
            s_field3-high = lv_seltime.
            APPEND s_field3.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDIF."IF EXPLICIT SEARCH help exits
*for other search helps
    IF lv_seldate IS INITIAL AND lv_seltime IS INITIAL.
      CASE pi_fieldname.
        WHEN 'S_FIELD1-LOW'.
          CLEAR s_field1.
          s_field1-sign   = 'I'.
          s_field1-option = 'EQ'.
          LOOP AT lt_retvalues INTO ls_retvalues
                               WHERE fieldname = ls_dd03p-fieldname.
            s_field1-low = ls_retvalues-fieldval.
            APPEND s_field1.
            CLEAR ls_retvalues.
          ENDLOOP.
          IF sy-subrc <> 0.
            CONCATENATE gv_table '-' ls_dd03p-fieldname
                   INTO lv_db_field.
            CONDENSE lv_db_field NO-GAPS.
            LOOP AT lt_retvalues INTO ls_retvalues
                                 WHERE retfield = lv_db_field.
              s_field1-low = ls_retvalues-fieldval.
              APPEND s_field1.
              CLEAR ls_retvalues.
            ENDLOOP.
          ENDIF.
        WHEN 'S_FIELD1-HIGH'.
          CLEAR s_field1.
          s_field1-sign   = 'I'.
          s_field1-option = 'EQ'.
          LOOP AT lt_retvalues INTO ls_retvalues
                               WHERE fieldname = ls_dd03p-fieldname.
            s_field1-high = ls_retvalues-fieldval.
            APPEND s_field1.
            CLEAR ls_retvalues.
          ENDLOOP.
          IF sy-subrc <> 0.
            CONCATENATE gv_table '-' ls_dd03p-fieldname
                   INTO lv_db_field.
            CONDENSE lv_db_field NO-GAPS.
            LOOP AT lt_retvalues INTO ls_retvalues
                                 WHERE retfield = lv_db_field.
              s_field1-high = ls_retvalues-fieldval.
              APPEND s_field1.
              CLEAR ls_retvalues.
            ENDLOOP.
          ENDIF.
        WHEN 'S_FIELD2-LOW'.
          CLEAR s_field2.
          s_field2-sign   = 'I'.
          s_field2-option = 'EQ'.
          LOOP AT lt_retvalues INTO ls_retvalues
                               WHERE fieldname = ls_dd03p-fieldname.
            s_field2-low = ls_retvalues-fieldval.
            APPEND s_field2.
            CLEAR ls_retvalues.
          ENDLOOP.
          IF sy-subrc <> 0.
            CONCATENATE gv_table '-' ls_dd03p-fieldname
                   INTO lv_db_field.
            CONDENSE lv_db_field NO-GAPS.
            LOOP AT lt_retvalues INTO ls_retvalues
                                 WHERE retfield = lv_db_field.
              s_field2-low = ls_retvalues-fieldval.
              APPEND s_field2.
              CLEAR ls_retvalues.
            ENDLOOP.
          ENDIF.
        WHEN 'S_FIELD2-HIGH'.
          CLEAR s_field2.
          s_field2-sign   = 'I'.
          s_field2-option = 'EQ'.
          LOOP AT lt_retvalues INTO ls_retvalues
                               WHERE fieldname = ls_dd03p-fieldname.

            s_field2-high = ls_retvalues-fieldval.
            APPEND s_field2.
            CLEAR ls_retvalues.
          ENDLOOP.
          IF sy-subrc <> 0.
            CONCATENATE gv_table '-' ls_dd03p-fieldname
                   INTO lv_db_field.
            CONDENSE lv_db_field NO-GAPS.
            LOOP AT lt_retvalues INTO ls_retvalues
                                 WHERE retfield = lv_db_field.
              s_field2-high = ls_retvalues-fieldval.
              APPEND s_field2.
              CLEAR ls_retvalues.
            ENDLOOP.
          ENDIF.
        WHEN 'S_FIELD3-LOW'.
          CLEAR s_field3.
          s_field3-sign   = 'I'.
          s_field3-option = 'EQ'.
          LOOP AT lt_retvalues INTO ls_retvalues
                               WHERE fieldname = ls_dd03p-fieldname.
            s_field3-low = ls_retvalues-fieldval.
            APPEND s_field3.
            CLEAR ls_retvalues.
          ENDLOOP.
          IF sy-subrc <> 0.
            CONCATENATE gv_table '-' ls_dd03p-fieldname
                   INTO lv_db_field.
            CONDENSE lv_db_field NO-GAPS.
            LOOP AT lt_retvalues INTO ls_retvalues
                                 WHERE retfield = lv_db_field.
              s_field3-low = ls_retvalues-fieldval.
              APPEND s_field3.
              CLEAR ls_retvalues.
            ENDLOOP.
          ENDIF.
        WHEN 'S_FIELD3-HIGH'.
          CLEAR s_field3.
          s_field3-sign   = 'I'.
          s_field3-option = 'EQ'.
          LOOP AT lt_retvalues INTO ls_retvalues
                               WHERE fieldname = ls_dd03p-fieldname.
            s_field3-high  = ls_retvalues-fieldval.
            APPEND s_field3.
            CLEAR ls_retvalues.
          ENDLOOP.
          IF sy-subrc <> 0.
            CONCATENATE gv_table '-' ls_dd03p-fieldname
                   INTO lv_db_field.
            CONDENSE lv_db_field NO-GAPS.
            LOOP AT lt_retvalues INTO ls_retvalues
                                 WHERE retfield = lv_db_field.
              s_field3-high = ls_retvalues-fieldval.
              APPEND s_field3.
              CLEAR ls_retvalues.
            ENDLOOP.
          ENDIF.
        WHEN OTHERS.
          "Do nothing
      ENDCASE.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM:s_field1 COMPARING low,
                                    s_field2 COMPARING low,
                                    s_field3 COMPARING low.
  ENDIF."endif FM F4IF_INT_TABLE_VALUE_REQUEST sy-subrc check
ENDFORM.                    " F_CUSTOM_SHLP1
*&---------------------------------------------------------------------*
*&      Form  F_GET_TABLE_VAL
*&---------------------------------------------------------------------*
*    Read table values from seelction screen
*----------------------------------------------------------------------*
*      <--PCT_DDFIELDS  Database fields
*----------------------------------------------------------------------*
FORM f_get_table_val CHANGING pct_ddfields  TYPE tt_ddfields.
  "DATA:lt_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE.
  DATA:lt_fieldlist      TYPE tt_field_pos,
       lt_wh_fields      TYPE tt_wh_fields,
       lv_field_sequence TYPE char90.
  CLEAR:  gv_field1,gv_field2,gv_field3.
  IF gv_table IS NOT INITIAL.
    PERFORM f_build_fldlist CHANGING lt_fieldlist pct_ddfields
                                     lt_wh_fields lv_field_sequence.
  ENDIF.
ENDFORM.                    " F_GET_TABLE_VAL
*&---------------------------------------------------------------------*
*&      Form  F_SCREEN_PBO
*&---------------------------------------------------------------------*
*  PBO for selection screen
*----------------------------------------------------------------------*
FORM f_screen_pbo .
  PERFORM f_enable_fpath.
  SPLIT p_table AT '|' INTO gv_table gv_desc.
  PERFORM:f_get_table_val CHANGING gt_ddfields,
          f_toggle_sel_flds  USING gt_ddfields.
ENDFORM.                    " F_SCREEN_PBO
*&---------------------------------------------------------------------*
*&      Form  F_ENABLE_FPATH
*&---------------------------------------------------------------------*
*   Eneable disable fields based on mode of output selected
*----------------------------------------------------------------------*
FORM f_enable_fpath .
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'XLS'.
        IF rb_xls = gc_x."gc_set'.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'MAL'.
        IF rb_mail = gc_x."gc_set.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " F_ENABLE_FPATH
*&---------------------------------------------------------------------*
*&      Form  F_FILE_F4
*&---------------------------------------------------------------------*
*   Provide browsing option for file
*----------------------------------------------------------------------*
FORM f_file_f4 .
  DATA : lv_folder TYPE string,
         lv_title  TYPE string.
  lv_title = 'Please select a destination folder.'(t08).
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = lv_title
      initial_folder  = 'H:\'
    CHANGING
      selected_folder = lv_folder.

  p_fpth1 = lv_folder .
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.                    " F_FILE_F4
*&---------------------------------------------------------------------*
*&      Form  F_RESET_RBUT
*&---------------------------------------------------------------------*
*    Reset radio button values
*----------------------------------------------------------------------*
FORM f_reset_rbut .
  IF rb_xls IS NOT INITIAL.
    rb_mail = space.
    rb_alv = space.
    CLEAR p_fdlm2.
  ELSEIF rb_mail IS NOT INITIAL.
    rb_xls = space.
    rb_alv = space.
    CLEAR p_fdlm1.
  ELSEIF rb_alv IS NOT INITIAL.
    rb_xls = space.
    rb_mail = space.
  ENDIF.
ENDFORM.                    " F_RESET_RBUT
*&---------------------------------------------------------------------*
*&      Form  F_FILL_HEADINGS
*&---------------------------------------------------------------------*
*   Fill headings for output file
*----------------------------------------------------------------------*
*      -->PIT_FIELDS     Fields entered by user
*      <--PCT_FIELDNAMES Fieldnames from table
*----------------------------------------------------------------------*
FORM f_fill_headings    USING  pit_ddfields   TYPE tt_ddfields
                     CHANGING  pct_fieldnames TYPE tt_fieldnames.
  DATA:ls_fieldnames TYPE gty_fieldnames,
       ls_ddfields   TYPE dfies.

  LOOP AT pit_ddfields INTO ls_ddfields.
    ls_fieldnames-line = ls_ddfields-fieldtext.
    APPEND ls_fieldnames TO pct_fieldnames.
    CLEAR :ls_ddfields,ls_fieldnames.
  ENDLOOP.
ENDFORM.                    " F_FILL_HEADINGS
*&---------------------------------------------------------------------*
*&      Form  F_APPLY_CONVEXIT
*&---------------------------------------------------------------------*
*  Apply conversion exit dynamically for fields of database table
*----------------------------------------------------------------------*
*      -->PI_FIELD     Fieldname
*      <--PCT_VALUES   Converted value
*----------------------------------------------------------------------*
FORM f_apply_convexit  USING pi_field   TYPE fieldname
                    CHANGING pct_values TYPE /sapdii/twty_rsds_selopt.
  CALL METHOD o_extract->use_conversion_exit
    EXPORTING
      i_field   = pi_field
      i_table   = gv_table
    CHANGING
      ct_values = pct_values.

ENDFORM.                    " F_APPLY_CONVEXIT
*&---------------------------------------------------------------------*
*&      Form  F_SET_VALUES
*&---------------------------------------------------------------------*
*   Set global values for fields
*----------------------------------------------------------------------*
FORM f_set_values USING pit_selfld_value TYPE crmt_ic_ui_ddshretval
                        pit_ddfields     TYPE tt_ddfields.
  DATA:lt_tabf4    TYPE STANDARD TABLE OF gty_tabf4 INITIAL SIZE 0,
       lt_ddfields TYPE STANDARD TABLE OF dfies     INITIAL SIZE 0,
       ls_tabf4    TYPE gty_tabf4,
       ls_ddfields TYPE dfies,
       ls_selfld_value  TYPE ddshretval.
  DATA:lv_field1        TYPE as4text,
       lv_field2        TYPE as4text,
       lv_field3        TYPE as4text.
  CLEAR :gv_field1,gv_field2,gv_field3.
  lt_ddfields = pit_ddfields.
  LOOP AT lt_ddfields INTO ls_ddfields.
    ls_tabf4-fieldname   = ls_ddfields-fieldname.
    ls_tabf4-description = ls_ddfields-fieldtext.
    TRANSLATE ls_tabf4-description TO LOWER CASE.
    APPEND ls_tabf4 TO lt_tabf4.
    CLEAR :ls_tabf4,ls_ddfields.
  ENDLOOP.
  IF pit_selfld_value IS INITIAL.
    IF p_field1 IS NOT INITIAL.
      READ TABLE lt_tabf4 INTO ls_tabf4 WITH KEY description = p_field1.
      IF sy-subrc IS INITIAL.
        gv_field1 = ls_tabf4-fieldname.
      ENDIF.
    ENDIF.
    IF p_field2 IS NOT INITIAL.
      READ TABLE lt_tabf4 INTO ls_tabf4 WITH KEY description = p_field2.
      IF sy-subrc IS INITIAL.
        gv_field2 = ls_tabf4-fieldname.
      ENDIF.
    ENDIF.
    IF p_field3 IS NOT INITIAL.
      READ TABLE lt_tabf4 INTO ls_tabf4 WITH KEY description = p_field3.
      IF sy-subrc IS INITIAL.
        gv_field3 = ls_tabf4-fieldname.
      ENDIF.
    ENDIF.
  ELSE.
    READ TABLE pit_selfld_value INTO ls_selfld_value INDEX 1.
    IF sy-subrc IS INITIAL.
*Build dynamic range
      IF s_field1 IS NOT INITIAL.
        lv_field1 = ls_selfld_value-fieldval.
        READ TABLE lt_tabf4  INTO ls_tabf4
                             WITH KEY description = lv_field1.
        IF sy-subrc IS INITIAL.
          gv_field1 = ls_tabf4-fieldname.
        ENDIF.
      ELSEIF s_field2 IS NOT INITIAL.
        lv_field2 = ls_selfld_value-fieldval.
        READ TABLE lt_tabf4  INTO ls_tabf4
                             WITH KEY description = lv_field2.
        IF sy-subrc IS INITIAL.
          gv_field2 = ls_tabf4-fieldname.
        ENDIF.
      ELSEIF s_field3 IS NOT INITIAL.
        lv_field3 = ls_selfld_value-fieldval.
        READ TABLE lt_tabf4  INTO ls_tabf4
                             WITH KEY description = lv_field3.
        IF sy-subrc IS INITIAL.
          gv_field3 = ls_tabf4-fieldname.
        ENDIF."endif check on pct_tabf4 read
      ENDIF."endif pi_fieldname check
    ENDIF."endif read on pit_selfld_value
  ENDIF.
ENDFORM.                    " F_SET_VALUES
*&---------------------------------------------------------------------*
*&      Form  F_SEND_MAIL
*&---------------------------------------------------------------------*
*    Send Mail
*----------------------------------------------------------------------*
*-->PIT_OUTPUT Data table for display
*-->PI_INDEX_LO Low Index for data limit in attachment file for size
*   limit
*-->PI_INDEX_HI High Index for data limit in attachment file for size
*   limit
*----------------------------------------------------------------------*
FORM f_send_mail USING pit_output  TYPE tt_output
                       pi_index_lo TYPE syindex
                       pi_index_hi TYPE syindex
              CHANGING pc_exit.
  DATA:  lo_send_request     TYPE REF TO cl_bcs,
         lo_document         TYPE REF TO cl_document_bcs,
         lo_recipient        TYPE REF TO if_recipient_bcs,
         lo_send_exc         TYPE REF TO cx_send_req_bcs,
         lo_addr_exc         TYPE REF TO cx_address_bcs,
         lo_send_email_mess  TYPE REF TO cx_document_bcs,
         lo_document_bcs     TYPE REF TO cx_document_bcs.
  DATA:  lt_message_body     TYPE bcsy_text,
         ls_message_body     TYPE soli,
         lt_att_content_hex  TYPE solix_tab.
  DATA:  lv_sent_to_all      TYPE os_boolean.
  DATA:  lt_return           TYPE STANDARD TABLE OF bapiret2
                             INITIAL SIZE 0,
       ls_return             TYPE bapiret2,
       ls_address            TYPE bapiaddr3.
  DATA:lv_attach_sub         TYPE sood-objdes,
       lv_email              TYPE ad_smtpadr,
       lv_fileno             TYPE string,
       lv_ext                TYPE soodk-objtp,
       lv_message            TYPE string.
  DATA:ls_object_hd_chg      TYPE sood1.
  lv_ext = gv_ext2.
  IF gv_ext2 EQ 'XLS' OR gv_ext3 = 'XLSX'.
    PERFORM f_fill_attachment_xls   USING pit_output pi_index_lo
                                          pi_index_hi
                                 CHANGING lt_att_content_hex pc_exit.
  ELSE.
    PERFORM f_fill_attachment_oth USING pit_output pi_index_lo
                                        pi_index_hi
                               CHANGING lt_att_content_hex pc_exit.
  ENDIF.
  IF pc_exit NE gc_x.
    TRY.
        "create send request
        lo_send_request = cl_bcs=>create_persistent( ).
        "create message body and subject
        APPEND text-t03 TO lt_message_body.
        APPEND INITIAL LINE TO lt_message_body.
        CONCATENATE text-t04 p_table text-t05
               INTO ls_message_body
       SEPARATED BY space.
        APPEND ls_message_body TO lt_message_body.
        APPEND INITIAL LINE TO lt_message_body.
        APPEND text-t06 TO lt_message_body.

        "put your text into the document
        lo_document = cl_document_bcs=>create_document(
                         i_type = 'RAW'
                         i_text = lt_message_body
                         i_subject = text-t01 ).

        lv_fileno = sy-index.
        IF gv_ext2 IS NOT INITIAL.
          CONCATENATE gv_table '_' text-t02 lv_fileno '.' gv_ext2
                 INTO lv_attach_sub.
        ELSEIF gv_ext3 IS NOT INITIAL.
          gv_ext2 = gv_ext3.
          CONCATENATE gv_table '_' text-t02 lv_fileno '.' gv_ext3
                 INTO lv_attach_sub.
        ENDIF.
        CONDENSE lv_attach_sub."attachment title
        DATA:lv_attach TYPE so_obj_len.
        lv_attach               = gv_attach_length.
        ls_object_hd_chg-objlen = lv_attach.
        TRY.
            IF gv_ext2 IS NOT INITIAL.
              lo_document->add_attachment(
                 EXPORTING
                   i_attachment_type    = gv_ext2
                   i_attachment_subject = lv_attach_sub
                   i_attachment_size    = ls_object_hd_chg-objlen
                   i_att_content_hex    = lt_att_content_hex ).
            ENDIF.
          CATCH cx_document_bcs INTO lo_document_bcs.
        ENDTRY.

* Add attachment
* Pass the document to send request
        lo_send_request->set_document( lo_document ).
        "Create recipient
        IF p_email IS INITIAL.
          CALL FUNCTION 'BAPI_USER_GET_DETAIL'
            EXPORTING
              username = sy-uname
            IMPORTING
              address  = ls_address
            TABLES
              return   = lt_return.
          READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
          IF sy-subrc IS NOT INITIAL.
            lv_email     = ls_address-e_mail.
            lo_recipient =
            cl_cam_address_bcs=>create_internet_address( lv_email ).
          ENDIF.
        ELSE.
          lo_recipient   =
          cl_cam_address_bcs=>create_internet_address( p_email ).
        ENDIF.

*Set recipient
        lo_send_request->add_recipient(
             EXPORTING
               i_recipient = lo_recipient i_express = gc_x ).

        lo_send_request->add_recipient( lo_recipient ).

* Send email
        lo_send_request->set_send_immediately( gc_x ).
        lo_send_request->send(
          RECEIVING
            result = lv_sent_to_all ).
      CATCH cx_send_req_bcs INTO lo_send_exc.

      CATCH cx_address_bcs  INTO lo_addr_exc.

      CATCH cx_document_bcs INTO lo_send_email_mess.
    ENDTRY.
    IF lo_send_exc IS NOT INITIAL.
      lv_message = lo_send_exc->get_text( ).
    ELSEIF lo_addr_exc IS NOT INITIAL.
      lv_message = lo_addr_exc->get_text( ).
    ELSEIF lo_send_email_mess IS NOT INITIAL.
      lv_message = lo_send_email_mess->get_text( ).
    ENDIF.
    IF lv_message IS INITIAL.
      COMMIT WORK.
      MESSAGE s000(zfi01) WITH text-s01.
    ELSE.
      MESSAGE e000(zfi01) WITH lv_message.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  F_FILL_ATTACHMENT
*&---------------------------------------------------------------------*
* Fill attachment in other format
*----------------------------------------------------------------------*
*-->PIT_OUTPUT   Output file
*-->PI_INDEXLO   Lower limit for data in file
*-->PI_INDEXHI   higher Limit for data in file
*<--PCT_HEX_CONTENT Hexadecimal content
*<--PC_EXIT_FLAG Exit flag from do loop for sending mails
*----------------------------------------------------------------------*
FORM f_fill_attachment_oth  USING pit_output      TYPE tt_output
                                  pi_indexlo      TYPE syindex
                                  pi_indexhi      TYPE syindex
                         CHANGING pct_hex_content TYPE solix_tab
                                  pc_exit_flag    TYPE flag.

  DATA lv_xstring TYPE xstring .
  DATA lv_length  TYPE i VALUE 0.
  DATA:lt_binary_content TYPE solix_tab.
  DATA:lv_pdf_obj_len    TYPE so_obj_len.
  DATA: lv_size1 TYPE abap_msize,
        lv_size2 TYPE abap_msize,
        lv_size3 TYPE abap_msize,
        lv_size4 TYPE abap_msize,
        lv_flag1 TYPE char128,
        lv_size5 TYPE abap_msize,
        lv_size6 TYPE abap_msize,
        lv_flag2 TYPE char128,
        lv_flag3 TYPE char128.
  DATA:lt_output_part TYPE STANDARD TABLE OF gty_output
                      INITIAL SIZE 0,
       ls_output      TYPE gty_output,
       lt_output      TYPE tt_output.
**Convert string to xstring
  IF rb_mail IS NOT INITIAL.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    lt_output = pit_output.
    DELETE lt_output INDEX 1.
    APPEND LINES OF lt_output "pit_output
            FROM pi_indexlo
              TO pi_indexhi
              TO lt_output_part.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    IF   ( lt_output_part IS INITIAL ).
      pc_exit_flag = gc_x."Set the exit from do loop
      RETURN. "EXIT.
    ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    "IF sy-index NE 1.
    READ TABLE pit_output INTO ls_output INDEX 1.
    IF sy-subrc IS INITIAL.
      INSERT ls_output INTO lt_output_part INDEX 1.
    ENDIF.
    " ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    TRY.
      CALL METHOD cl_abap_memory_utilities=>get_memory_size_of_object
        EXPORTING
          object                     = lt_output_part
        IMPORTING
          bound_size_alloc           = lv_size1
          bound_size_used            = lv_size2
          referenced_size_alloc      = lv_size3
          referenced_size_used       = lv_size4
          is_part_of_non_trivial_szk = lv_flag1
          szk_size_alloc             = lv_size5
          szk_size_used              = lv_size6
          low_mem                    = lv_flag2
          is_in_shared_memory        = lv_flag3.
    ENDTRY.
    lv_length = lv_size4.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_length
      IMPORTING
        buffer       = lv_xstring
      TABLES
        binary_tab   = lt_output_part
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc = 0.
* Implement suitable error handling here
    ENDIF.
***Xstring to binary
    IF lv_xstring IS NOT INITIAL.
      lv_pdf_obj_len = xstrlen( lv_xstring ).
      lt_binary_content = cl_document_bcs=>xstring_to_solix(
                      ip_xstring = lv_xstring ).
      pct_hex_content = lt_binary_content.
    ENDIF.


  ENDIF.
ENDFORM.                    " F_FILL_ATTACHMENT
*&---------------------------------------------------------------------*
*&      Form  F_DWNLD_EXCEL
*&---------------------------------------------------------------------*
*    Download excel to local PC file
*----------------------------------------------------------------------*
* --> PI_FILE     Output file for download
*----------------------------------------------------------------------*
FORM f_dwnld_excel USING pi_file TYPE string.
*Local internal table
  DATA:lt_fieldnames  TYPE STANDARD TABLE OF gty_fieldnames
                      INITIAL SIZE 0.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  DATA:    lv_index     TYPE syindex,
           lv_fileno    TYPE char10,
           lv_file      TYPE string,
           lv_index_lo  TYPE syindex,
           lv_index_hi  TYPE syindex,
           lv_blocksize TYPE syindex,
           lv_exit_flag TYPE flag.

  DATA:    lo_tab1 TYPE REF TO data,
           lo_tab2 TYPE REF TO data.

  FIELD-SYMBOLS: <lt_output>  TYPE STANDARD TABLE,
                 <lfs_wa>     TYPE any.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156

  IF p_chdr1 IS NOT INITIAL.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    CREATE DATA lo_tab1 LIKE <fs_output>.
* This would now assign the stucture of <fs_output> to the
* field-symbols <lt_output>
* So now <lt_output> has a stucture and it is a table.
    ASSIGN lo_tab1->* TO <lt_output>.
* So now <lfs_wa> becomes a work area having a structure
* like <fs_output>
    CREATE DATA lo_tab2 LIKE LINE OF <fs_output>.
    ASSIGN lo_tab2->* TO <lfs_wa>.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
*Fill column headings in file
    IF gt_ddfields IS NOT INITIAL.
      PERFORM f_fill_headings USING gt_ddfields CHANGING lt_fieldnames.
    ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
*Split large files into small ones
    IF p_dpbc IS NOT INITIAL.
      lv_blocksize = p_dpbc.
      DO.
        IF lv_exit_flag EQ gc_x.
          EXIT. "loop termination
        ENDIF.
        lv_fileno = sy-index.
*Create filename dynamically
        CONCATENATE pi_file
                    '_' 'File'(t09)
                    lv_fileno
                    '.'
                    p_fext1 INTO lv_file.
        CONDENSE pi_file.
* Calculate the low and high indices for the batch of WBS elements
        lv_index    =     sy-index.
        lv_index_lo = ( ( lv_index - 1 ) * lv_blocksize ) + 1.
        lv_index_hi = (   lv_index       * lv_blocksize ).
        LOOP AT <fs_output> ASSIGNING <lfs_wa>
                                 FROM lv_index_lo
                                   TO lv_index_hi.
          APPEND <lfs_wa> TO <lt_output>.
        ENDLOOP.
        IF <lt_output> IS INITIAL.
          lv_exit_flag = gc_x.
          RETURN.
        ENDIF.
*download to excel with column header
        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = lv_file
            write_field_separator   = 'X'"gc_set'
            fieldnames              = lt_fieldnames
          CHANGING
            data_tab                = <lt_output>
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
        CLEAR :<lfs_wa>,lv_file.
        REFRESH <lt_output>.
      ENDDO.
    ELSE.
*Create filename dynamically
      CONCATENATE pi_file
                  '.'
                  p_fext1 INTO pi_file.
      CONDENSE pi_file.
* Download whole file at once in case of small tables
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = pi_file
          write_field_separator   = 'X'"gc_set'
          fieldnames              = lt_fieldnames
        CHANGING
          data_tab                = <fs_output>
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
      ENDIF."endif sy-subrc check for gui_download
    ENDIF."endif p_dpbc is  initial
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  ELSE.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
*Split large files into small ones
    IF p_dpbc IS NOT INITIAL.
      lv_blocksize = p_dpbc.
      DO.
        IF lv_exit_flag EQ gc_x.
          EXIT. "loop termination
        ENDIF.
        lv_fileno = sy-index.
*Create filename dynamically
        CONCATENATE pi_file
                    '_' 'File'(t09)
                    lv_fileno
                    '.'
                    p_fext1 INTO lv_file.
        CONDENSE pi_file.
* Calculate the low and high indices for the batch of WBS elements
        lv_index    =     sy-index.
        lv_index_lo = ( ( lv_index - 1 ) * lv_blocksize ) + 1.
        lv_index_hi = (   lv_index       * lv_blocksize ).
        LOOP AT <fs_output> INTO <lfs_wa>
                            FROM lv_index_lo
                              TO lv_index_hi.
          APPEND  <lfs_wa> TO <lt_output>.
        ENDLOOP.
        IF <lt_output> IS INITIAL.
          lv_exit_flag = gc_x.
          RETURN.
        ENDIF.
*download to excel without column header
        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = lv_file
            write_field_separator   = 'X'"gc_set'
          CHANGING
            data_tab                = <lt_output>
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
        CLEAR :<lfs_wa>,lv_file.
        REFRESH <lt_output>.
      ENDDO.
    ELSE.
*Create filename dynamically
      CONCATENATE pi_file
                  '.'
                  p_fext1 INTO pi_file.
      CONDENSE pi_file.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
*download to excel without column header
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = pi_file
          write_field_separator   = 'X'"gc_set
        CHANGING
          data_tab                = <fs_output>
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
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    ENDIF."endif p_dpbc check
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  ENDIF."endif p_chdr1 check
ENDFORM.                    " F_DWNLD_EXCEL
*&---------------------------------------------------------------------*
*&      Form  F_DWNLD_OTH
*&---------------------------------------------------------------------*
*      Download file to PC in other format
*----------------------------------------------------------------------*
* --> PI_FILE Input file for download
*----------------------------------------------------------------------*
FORM f_dwnld_oth USING pi_file TYPE string.
  DATA: lo_structdescr TYPE REF TO cl_abap_structdescr,
        lt_components  TYPE cl_abap_structdescr=>component_table.
  FIELD-SYMBOLS:<lfs_comp>  LIKE LINE OF lt_components.
  FIELD-SYMBOLS:<lfs_wa>    TYPE any,
                <lfs_field> TYPE any.
  DATA:lv_tabix  TYPE sy-tabix,
       lv_fldval TYPE string.
  DATA:ls_ddfields    TYPE dfies.
  DATA:lv_offset      TYPE i,
       lv_sep_offset  TYPE i,
       lv_length      TYPE i,
       lv_prev_length TYPE i.
*Internal table for file
  DATA:lt_output  TYPE STANDARD TABLE OF gty_output INITIAL SIZE 0,
       ls_output  TYPE gty_output,
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
       ls_output1 TYPE gty_output.
  DATA:    lv_index     TYPE syindex,
           lv_fileno    TYPE n,
           lv_file      TYPE string,
           lv_index_lo  TYPE syindex,
           lv_index_hi  TYPE syindex,
           lv_blocksize TYPE syindex,
           lv_exit_flag TYPE flag.

  DATA:    lo_tab1 TYPE REF TO data,
           lo_tab2 TYPE REF TO data.

  FIELD-SYMBOLS: <lt_output>  TYPE STANDARD TABLE.

  CREATE DATA lo_tab1 LIKE <fs_output>.
* This would now assign the stucture of <fs_output> to the
* field-symbols <lt_output>
* So now <lt_output> has a stucture and it is a table.
  ASSIGN lo_tab1->* TO <lt_output>.
* So now <lfs_wa> becomes a work area having a structure
* like <fs_output>
  CREATE DATA lo_tab2 LIKE LINE OF <fs_output>.
  ASSIGN lo_tab2->* TO <lfs_wa>.

*Get all fields in main table
  READ TABLE <fs_output> ASSIGNING <lfs_wa> INDEX 1.
  IF lt_components IS INITIAL.  "get columns' names only once.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_data( <lfs_wa> ).
    lt_components = lo_structdescr->get_components( ).
  ENDIF.

*Check if an include is present in database table
  READ TABLE lt_components ASSIGNING <lfs_comp>
                           WITH KEY as_include = 'X'.
  IF sy-subrc IS INITIAL.
    CALL METHOD o_extract->get_incl_components
      EXPORTING
        im_structdescr = lo_structdescr
      CHANGING
        ct_components  = lt_components.
  ENDIF.
*Split large files into small ones
  IF p_dpbc IS NOT INITIAL.
    lv_blocksize = p_dpbc.
    DO.
      IF lv_exit_flag EQ gc_x.
        EXIT. "loop termination
      ENDIF.
      lv_fileno = sy-index.
*Create filename dynamically
      CONCATENATE pi_file
                  '_' 'File'(t09)
                  lv_fileno
                  '.'
                  p_fext1 INTO lv_file.
      CONDENSE pi_file.
* Calculate the low and high indices for the batch of WBS elements
      lv_index    =     sy-index.
      lv_index_lo = ( ( lv_index - 1 ) * lv_blocksize ) + 1.
      lv_index_hi = (   lv_index       * lv_blocksize ).
      LOOP AT <fs_output> ASSIGNING <lfs_wa>
                               FROM lv_index_lo
                                 TO lv_index_hi.
        APPEND <lfs_wa> TO <lt_output>.
      ENDLOOP.
      IF <lt_output> IS INITIAL.
        lv_exit_flag = gc_x.
        RETURN.
      ENDIF.
      IF lt_components IS NOT INITIAL.
        LOOP AT <lt_output> ASSIGNING <lfs_wa>.
          lv_tabix = sy-tabix.
          DO. "iterate all columns in the row
            ASSIGN COMPONENT sy-index OF STRUCTURE <lfs_wa>
            TO <lfs_field>.
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
            READ TABLE lt_components
             ASSIGNING <lfs_comp>
                 INDEX sy-index.
            IF <lfs_comp> IS ASSIGNED.
              READ TABLE gt_ddfields
                    INTO ls_ddfields
                    WITH KEY fieldname = <lfs_comp>-name.
              IF sy-subrc IS INITIAL.
                IF lv_tabix = 1.
                  IF p_chdr1 IS NOT INITIAL.
                    IF p_fdlm1 IS INITIAL.
                      "Add field value
                      lv_length = ls_ddfields-leng.
                      lv_offset = lv_prev_length + 1.
                      ls_output-line+lv_offset(lv_length)
                      = ls_ddfields-scrtext_m.
                      "Add field separator
                      lv_sep_offset = strlen( ls_output-line ).
                      lv_sep_offset = lv_sep_offset + 1.
                      ls_output-line+lv_sep_offset(1)
                      = cl_abap_char_utilities=>horizontal_tab.
                    ELSE.
                      "Add field value
                      lv_length = ls_ddfields-leng.
                      lv_offset = lv_prev_length + 1.
                      ls_output-line+lv_offset(lv_length) =
                                         ls_ddfields-scrtext_m.
                      "Add field separator
                      lv_sep_offset = lv_offset + lv_length + 1.
                      ls_output-line+lv_sep_offset(1) = p_fdlm1.
                    ENDIF."endif p_fdlm1 is not initial
                  ENDIF."endif p_chdr1 is initial
                  lv_fldval = <lfs_field>.
*Formatting fields
                  SHIFT lv_fldval RIGHT DELETING TRAILING space.
                  TRANSLATE lv_fldval USING ' 0'.
                  IF p_fdlm1 IS INITIAL.
                    "Add field value
                    lv_length = ls_ddfields-leng.
                    lv_offset = lv_prev_length + 1.
                    ls_output1-line+lv_offset(lv_length) = lv_fldval.
                    "Add field separator
                    lv_sep_offset = strlen( ls_output1-line ).
                    lv_sep_offset = lv_sep_offset + 1.
                    ls_output1-line+lv_sep_offset(1)
                    = cl_abap_char_utilities=>horizontal_tab.
                  ELSE.
                    "Add field value
                    lv_length = ls_ddfields-leng.
                    lv_offset = lv_prev_length + 1.
                    ls_output1-line+lv_offset(lv_length) = lv_fldval.
                    "Add field separator
                    lv_sep_offset = lv_offset + lv_length + 1.
                    ls_output1-line+lv_sep_offset(1) = p_fdlm1.
                  ENDIF.
                ENDIF."endif lv_tabix = 1
                IF lv_tabix GT 1.
                  lv_fldval = <lfs_field>.
*Formatting fields
                  SHIFT lv_fldval RIGHT DELETING TRAILING space.
                  TRANSLATE lv_fldval USING ' 0'.
                  IF p_fdlm1 IS INITIAL.
                    "Add field value
                    lv_length = ls_ddfields-leng.
                    lv_offset = lv_prev_length + 1.
                    ls_output-line+lv_offset(lv_length) = lv_fldval.
                    "Add field separator
                    lv_sep_offset = strlen( ls_output-line ).
                    lv_sep_offset = lv_sep_offset + 1.
                    ls_output-line+lv_sep_offset(1)
                    = cl_abap_char_utilities=>horizontal_tab.
                  ELSE.
                    "Add field value
                    lv_length = ls_ddfields-leng.
                    lv_offset = lv_prev_length + 1.
                    ls_output-line+lv_offset(lv_length) = lv_fldval.
                    "Add field separator
                    lv_sep_offset = lv_offset + lv_length + 1.
                    ls_output-line+lv_sep_offset(1) = p_fdlm1.
                  ENDIF."endif p_fdlm1 is initial
                ENDIF."endif lv_tabix gt 1 check
                CLEAR lv_prev_length.
                lv_prev_length = lv_sep_offset.
                CLEAR :lv_length,lv_offset,lv_sep_offset.
              ENDIF."endif read on gt_ddfields
            ENDIF."endif <lfs_comp> is assigned
          ENDDO.
          APPEND ls_output TO lt_output.
          IF ls_output1 IS NOT INITIAL.
            APPEND ls_output1 TO lt_output.
          ENDIF.
          CLEAR:ls_output,lv_fldval,ls_ddfields,lv_prev_length,
                lv_sep_offset,lv_offset,ls_output1.
        ENDLOOP.
      ENDIF.
      IF lt_output IS NOT INITIAL.
*download to txt or others with column header
        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = lv_file
            write_field_separator   = 'X'
            trunc_trailing_blanks   = 'X'"TRUNC_TRAILING_BLANKS'
          CHANGING
            data_tab                = lt_output
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
      ENDIF.
      CLEAR :<lt_output>,lv_file.
      REFRESH: lt_output.
    ENDDO.
  ELSE.
*Create filename dynamically
    CONCATENATE pi_file
                '.'
                p_fext1 INTO pi_file.
    CONDENSE pi_file.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    IF lt_components IS NOT INITIAL.
      LOOP AT <fs_output> ASSIGNING <lfs_wa>.
        lv_tabix = sy-tabix.
        DO. "iterate all columns in the row
          ASSIGN COMPONENT sy-index
              OF STRUCTURE <lfs_wa> TO <lfs_field>.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          READ TABLE lt_components ASSIGNING <lfs_comp> INDEX sy-index.
          IF <lfs_comp> IS ASSIGNED.
            READ TABLE gt_ddfields INTO ls_ddfields
                                 WITH KEY fieldname = <lfs_comp>-name.
            IF sy-subrc IS INITIAL.
              IF lv_tabix = 1.
                IF p_chdr1 IS NOT INITIAL.
                  IF p_fdlm1 IS INITIAL.
                    "Add field value
                    lv_length = ls_ddfields-leng.
                    lv_offset = lv_prev_length + 1.
                    ls_output-line+lv_offset(lv_length)
                    = ls_ddfields-scrtext_m.
                    "Add field separator
                    lv_sep_offset = strlen( ls_output-line ).
                    lv_sep_offset = lv_sep_offset + 1.
                    ls_output-line+lv_sep_offset(1)
                    = cl_abap_char_utilities=>horizontal_tab.
                  ELSE.
                    "Add field value
                    lv_length = ls_ddfields-leng.
                    lv_offset = lv_prev_length + 1.
                    ls_output-line+lv_offset(lv_length) =
                                       ls_ddfields-scrtext_m.
                    "Add field separator
                    lv_sep_offset = lv_offset + lv_length + 1.
                    ls_output-line+lv_sep_offset(1) = p_fdlm1.
                  ENDIF.
                ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
                lv_fldval = <lfs_field>.
*Formatting fields
                SHIFT lv_fldval RIGHT DELETING TRAILING space.
                TRANSLATE lv_fldval USING ' 0'.
                IF p_fdlm1 IS INITIAL.
                  "Add field value
                  lv_length = ls_ddfields-leng.
                  lv_offset = lv_prev_length + 1.
                  ls_output1-line+lv_offset(lv_length) = lv_fldval.
                  "Add field separator
                  lv_sep_offset = strlen( ls_output1-line ).
                  lv_sep_offset = lv_sep_offset + 1.
                  ls_output1-line+lv_sep_offset(1)
                  = cl_abap_char_utilities=>horizontal_tab.
                ELSE.
                  "Add field value
                  lv_length = ls_ddfields-leng.
                  lv_offset = lv_prev_length + 1.
                  ls_output1-line+lv_offset(lv_length) = lv_fldval.
                  "Add field separator
                  lv_sep_offset = lv_offset + lv_length + 1.
                  ls_output1-line+lv_sep_offset(1) = p_fdlm1.
                ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
              ENDIF.
              IF lv_tabix GT 1.
                lv_fldval = <lfs_field>.
*Formatting fields
                SHIFT lv_fldval RIGHT DELETING TRAILING space.
                TRANSLATE lv_fldval USING ' 0'.
                IF p_fdlm1 IS INITIAL.
                  "Add field value
                  lv_length = ls_ddfields-leng.
                  lv_offset = lv_prev_length + 1.
                  ls_output-line+lv_offset(lv_length) = lv_fldval.
                  "Add field separator
                  lv_sep_offset = strlen( ls_output-line ).
                  lv_sep_offset = lv_sep_offset + 1.
                  ls_output-line+lv_sep_offset(1)
                  = cl_abap_char_utilities=>horizontal_tab.
                ELSE.
                  "Add field value
                  lv_length = ls_ddfields-leng.
                  lv_offset = lv_prev_length + 1.
                  ls_output-line+lv_offset(lv_length) = lv_fldval.
                  "Add field separator
                  lv_sep_offset = lv_offset + lv_length + 1.
                  ls_output-line+lv_sep_offset(1) = p_fdlm1.
                ENDIF.
              ENDIF.
              CLEAR lv_prev_length.
              lv_prev_length = lv_sep_offset.
              CLEAR :lv_length,lv_offset,lv_sep_offset.
            ENDIF.
          ENDIF.
          CLEAR :<lt_output>,lv_file.
          REFRESH: lt_output.
        ENDDO.
        APPEND ls_output TO lt_output.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
        IF ls_output1 IS NOT INITIAL.
          APPEND ls_output1 TO lt_output.
        ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
        CLEAR:ls_output,lv_fldval,ls_ddfields,lv_prev_length,
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
              lv_sep_offset,lv_offset,ls_output1.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
      ENDLOOP.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156

    IF lt_output IS NOT INITIAL.
*download to txt or others with column header
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = pi_file
          write_field_separator   = 'X'
          trunc_trailing_blanks   = 'X'"TRUNC_TRAILING_BLANKS'
        CHANGING
          data_tab                = lt_output
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
    ENDIF."endif lt_output initial
  ENDIF."endif p_dpbc initial check
ENDFORM.                    " F_DWNLD_OTH
*&---------------------------------------------------------------------*
*&      Form  F_FILL_ATTACHMENT_XLS
*&---------------------------------------------------------------------*
*    Create XLS attachment
*----------------------------------------------------------------------*
*-->PIT_OUTPUT   Output file
*-->PI_INDEXLO   Lower limit for data in file
*-->PI_INDEXHI   higher Limit for data in file
*<--PCT_HEX_CONTENT Hexadecimal content
*<--PC_EXIT_FLAG   Exit flag from do loop for sending mails
*----------------------------------------------------------------------*
FORM f_fill_attachment_xls  USING pit_output      TYPE tt_output
                                  pi_indexlo      TYPE syindex
                                  pi_indexhi      TYPE syindex
                         CHANGING pct_hex_content TYPE solix_tab
                                  pc_exit_flag    TYPE flag.
  DATA lv_string         TYPE string .
  DATA:lt_binary_content TYPE solix_tab.
  DATA:lt_output_part    TYPE tt_output.
  DATA lv_xstring        TYPE xstring .
  DATA:ls_output         TYPE gty_output.
  DATA: lt_output        TYPE tt_output.

*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  IF rb_mail IS NOT INITIAL.
    lt_output = pit_output.
    "IF sy-index = 1.
    DELETE lt_output INDEX 1.
    "ENDIF.
    APPEND LINES OF lt_output "pit_output
               FROM pi_indexlo
                 TO pi_indexhi
                 TO lt_output_part.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    IF lt_output_part IS INITIAL.
      pc_exit_flag = gc_x.
      RETURN.
    ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    "IF sy-index NE 1.
    READ TABLE pit_output INTO ls_output INDEX 1.
    IF sy-subrc IS INITIAL.
      INSERT ls_output INTO lt_output_part INDEX 1.
    ENDIF.
    "ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    LOOP AT lt_output_part INTO ls_output.
      CONCATENATE  lv_string ls_output-line
              INTO lv_string
      SEPARATED BY cl_abap_char_utilities=>newline.
    ENDLOOP.
**Convert string to xstring
    CALL FUNCTION 'HR_KR_STRING_TO_XSTRING'
      EXPORTING
        unicode_string   = lv_string
      IMPORTING
        xstring_stream   = lv_xstring
      EXCEPTIONS
        invalid_codepage = 1
        invalid_string   = 2
        OTHERS           = 3.
    IF sy-subrc = 0.
**Convert string to xstring
      CALL METHOD cl_bcs_convert=>xstring_to_solix
        EXPORTING
          iv_xstring = lv_xstring
        RECEIVING
          et_solix   = lt_binary_content.
    ENDIF.
    IF lt_binary_content IS NOT INITIAL.
      pct_hex_content  = lt_binary_content.
      gv_attach_length = xstrlen( lv_xstring ).
    ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
ENDFORM.                    " F_FILL_ATTACHMENT_XLS
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_XLS_ATTACH
*&---------------------------------------------------------------------*
*       Create XLS attachment file
*----------------------------------------------------------------------*
*      <--PCT_OUTPUT  XLS format output
*----------------------------------------------------------------------*
FORM f_create_xls_attach  CHANGING pct_output TYPE tt_output.
  DATA: lo_structdescr  TYPE REF TO cl_abap_structdescr,
        lt_components   TYPE cl_abap_structdescr=>component_table.
  FIELD-SYMBOLS:<lfs_comp>  LIKE LINE OF lt_components.
  FIELD-SYMBOLS:<lfs_wa>    TYPE any,
                <lfs_field> TYPE any.
  DATA:lv_tabix    TYPE sy-tabix,
       lv_fldval   TYPE string.
  DATA:ls_ddfields TYPE dfies.
  DATA:ls_output   TYPE gty_output,
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
       ls_output1  TYPE gty_output.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156


*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
*Get all fields in main table
  READ TABLE <fs_output> ASSIGNING <lfs_wa> INDEX 1.
  IF lt_components IS INITIAL.  "get columns' names only once.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_data( <lfs_wa> ).
    lt_components = lo_structdescr->get_components( ).
  ENDIF.

*Check if an include is present in database table
  READ TABLE lt_components ASSIGNING <lfs_comp>
                           WITH KEY as_include = 'X'.
  IF sy-subrc IS INITIAL.
    CALL METHOD o_extract->get_incl_components
      EXPORTING
        im_structdescr = lo_structdescr
      CHANGING
        ct_components  = lt_components.
  ENDIF.

  IF lt_components IS NOT INITIAL.
    LOOP AT <fs_output> ASSIGNING <lfs_wa>.
      lv_tabix = sy-tabix.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
      DO. "iterate all columns in the row
        ASSIGN COMPONENT sy-index OF STRUCTURE <lfs_wa> TO <lfs_field>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        READ TABLE lt_components ASSIGNING <lfs_comp> INDEX sy-index.
        "field name: <ls_comp>-name.
        IF lv_tabix = 1.
          IF p_chdr2 IS NOT INITIAL.
            READ TABLE gt_ddfields INTO ls_ddfields
             WITH KEY fieldname = <lfs_comp>-name.
            IF sy-subrc IS INITIAL.
              CONCATENATE ls_output-line ls_ddfields-scrtext_m
                     INTO ls_output-line
             SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
            ENDIF."endif read on gt_ddfields
          ENDIF."endif p_chdr2 initial check
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
          lv_fldval = <lfs_field>.
          CONCATENATE ls_output1-line lv_fldval
                 INTO ls_output1-line
         SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
        ENDIF."endif read on lv_tabix = 1
        IF lv_tabix GT 1.
          lv_fldval = <lfs_field>.
          CONCATENATE ls_output-line lv_fldval
                 INTO ls_output-line
         SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
        ENDIF.
      ENDDO.
      APPEND ls_output TO pct_output.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
      IF ls_output1 IS NOT INITIAL.
        APPEND ls_output1 TO pct_output.
      ENDIF.
      CLEAR:ls_output,lv_fldval,ls_output1.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    ENDLOOP.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
ENDFORM.                    " F_CREATE_XLS_ATTACH
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_OTH_ATTACH
*&---------------------------------------------------------------------*
*   Create attachment in other format
*----------------------------------------------------------------------*
*      <--PCT_OUTPUT Output file in other format
*----------------------------------------------------------------------*
FORM f_create_oth_attach  CHANGING pct_output TYPE tt_output.
  DATA: lo_structdescr  TYPE REF TO cl_abap_structdescr,
        lt_components   TYPE cl_abap_structdescr=>component_table.
  FIELD-SYMBOLS:<lfs_comp>  LIKE LINE OF lt_components.
  FIELD-SYMBOLS:<lfs_wa>    TYPE any,
                <lfs_field> TYPE any.
  DATA:lv_tabix    TYPE sy-tabix,
       lv_fldval   TYPE string.
  DATA ls_ddfields TYPE dfies.
  DATA:lt_output   TYPE STANDARD TABLE OF gty_output INITIAL SIZE 0,
       ls_output   TYPE gty_output,
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
       ls_output1  TYPE gty_output.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  DATA:lv_offset      TYPE i,
       lv_sep_offset  TYPE i,
       lv_length      TYPE i,
       lv_prev_length TYPE i.


*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
*Get all fields in main table
  READ TABLE <fs_output> ASSIGNING <lfs_wa> INDEX 1.
  IF lt_components IS INITIAL.  "get columns' names only once.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_data( <lfs_wa> ).
    lt_components = lo_structdescr->get_components( ).
  ENDIF.

*Check if an include is present in database table
  READ TABLE lt_components ASSIGNING <lfs_comp>
                           WITH KEY as_include = 'X'.
  IF sy-subrc IS INITIAL.
    CALL METHOD o_extract->get_incl_components
      EXPORTING
        im_structdescr = lo_structdescr
      CHANGING
        ct_components  = lt_components.
  ENDIF.

  IF lt_components IS NOT INITIAL.
    LOOP AT <fs_output> ASSIGNING <lfs_wa>.
      lv_tabix = sy-tabix.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
      DO. "iterate all columns in the row
        ASSIGN COMPONENT sy-index OF STRUCTURE <lfs_wa> TO <lfs_field>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        READ TABLE lt_components ASSIGNING <lfs_comp> INDEX sy-index.
        IF <lfs_comp> IS ASSIGNED.
          READ TABLE gt_ddfields
                INTO ls_ddfields
            WITH KEY fieldname = <lfs_comp>-name.
          IF sy-subrc IS INITIAL.
            IF lv_tabix = 1.
              IF p_chdr2 IS NOT INITIAL.
                IF p_fdlm2 IS INITIAL.
                  "Add field value
                  lv_length = ls_ddfields-leng.
                  lv_offset = lv_prev_length + 1.
                  ls_output-line+lv_offset(lv_length)
                  = ls_ddfields-scrtext_m.
                  "Add field separator
                  lv_sep_offset = strlen( ls_output-line ).
                  lv_sep_offset = lv_sep_offset + 1.
                  ls_output-line+lv_sep_offset(1)
                  = cl_abap_char_utilities=>horizontal_tab.
                ELSE.
                  "Add field value
                  lv_length = ls_ddfields-leng.
                  lv_offset = lv_prev_length + 1.
                  ls_output-line+lv_offset(lv_length)
                  = ls_ddfields-scrtext_m.
                  "Add field separator
                  lv_sep_offset = lv_offset + lv_length + 1.
                  ls_output-line+lv_sep_offset(1) = p_fdlm2.
                ENDIF.
              ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
              lv_fldval = <lfs_field>.
*Formatting fields
              SHIFT lv_fldval RIGHT DELETING TRAILING space.
              IF p_fdlm2 IS INITIAL.
                "Add field value
                lv_length = ls_ddfields-leng.
                lv_offset = lv_prev_length + 1.
                ls_output1-line+lv_offset(lv_length) = lv_fldval.
                "Add field separator
                lv_sep_offset = strlen( ls_output1-line ).
                lv_sep_offset = lv_sep_offset + 1.
                ls_output1-line+lv_sep_offset(1)
                = cl_abap_char_utilities=>horizontal_tab.
              ELSE.
                "Add field value
                lv_length = ls_ddfields-leng.
                lv_offset = lv_prev_length + 1.
                ls_output1-line+lv_offset(lv_length) = lv_fldval.
                "Add field separator
                lv_sep_offset = lv_offset + lv_length + 1.
                ls_output1-line+lv_sep_offset(1) = p_fdlm2.
              ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
            ENDIF.
            IF lv_tabix GT 1.
              lv_fldval = <lfs_field>.
*Formatting fields
              SHIFT lv_fldval RIGHT DELETING TRAILING space.
              TRANSLATE lv_fldval USING ' 0'.
              IF p_fdlm2 IS INITIAL.
                "Add field value
                lv_length = ls_ddfields-leng.
                lv_offset = lv_prev_length + 1.
                ls_output-line+lv_offset(lv_length) = lv_fldval.
                "Add field separator
                lv_sep_offset = strlen( ls_output-line ).
                lv_sep_offset = lv_sep_offset + 1.
                ls_output-line+lv_sep_offset(1)
                = cl_abap_char_utilities=>horizontal_tab.
              ELSE.
                "Add field value
                lv_length = ls_ddfields-leng.
                lv_offset = lv_prev_length + 1.
                ls_output-line+lv_offset(lv_length) = lv_fldval.
                "Add field separator
                lv_sep_offset = lv_offset + lv_length + 1.
                ls_output-line+lv_sep_offset(1) = p_fdlm2.
              ENDIF.
            ENDIF.
            CLEAR lv_prev_length.
            lv_prev_length = lv_sep_offset.
            CLEAR :lv_length,lv_offset,lv_sep_offset.
          ENDIF.
        ENDIF.
      ENDDO.
      CONCATENATE  cl_abap_char_utilities=>cr_lf ls_output-line
             INTO ls_output-line.
      APPEND ls_output TO lt_output.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
      IF ls_output1 IS NOT INITIAL.
        CONCATENATE  cl_abap_char_utilities=>cr_lf ls_output1-line
               INTO ls_output1-line.
        APPEND ls_output1 TO lt_output.
      ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
      CLEAR:ls_output,lv_fldval,ls_ddfields,lv_prev_length,lv_sep_offset,
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
            lv_offset,ls_output1.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
    ENDLOOP.
    IF lt_output IS NOT INITIAL.
      pct_output = lt_output.
    ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
  ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
ENDFORM.                    " F_CREATE_OTH_ATTACH
*&---------------------------------------------------------------------*
*&      Form  F_TOGGLE_SEL_FLDS
*&---------------------------------------------------------------------*
*      Toggle selection criteria based on the type of table fields
*----------------------------------------------------------------------*
*   <--PIT_DDFIELDS  Database fields
*----------------------------------------------------------------------*
FORM f_toggle_sel_flds  USING  pit_ddfields TYPE tt_ddfields.
  DATA:lv_bkr_flg   TYPE flag,
       lv_gjr_flg   TYPE flag,
       lv_mnt_flg   TYPE flag,
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
       lv_vbln_flg  TYPE flag,
       lv_posnr_flg TYPE flag,
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
       lv_budat_flg TYPE flag.

  DATA:lt_ddfields TYPE tt_ddfields.
  IF pit_ddfields IS NOT INITIAL.
    lt_ddfields = pit_ddfields.
    SORT lt_ddfields BY fieldname.
  ENDIF.
*Check if company code is there in the table
  READ TABLE lt_ddfields  WITH KEY fieldname = gc_bukrs
                          TRANSPORTING NO FIELDS
                          BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_bkr_flg = gc_x.
  ELSE.
    READ TABLE lt_ddfields  WITH KEY domname = gc_bukrs
                            TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      lv_bkr_flg = gc_x.
    ENDIF.
  ENDIF.
*Check if Fiscal Year is there in the table
  READ TABLE lt_ddfields  WITH KEY fieldname = gc_gjahr
                          TRANSPORTING NO FIELDS
                          BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_gjr_flg = gc_x.
  ELSE.
    READ TABLE lt_ddfields  WITH KEY domname = gc_gjahr
                            TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      lv_gjr_flg = gc_x.
    ENDIF.
  ENDIF.
*Check if posting period is there in the table
  READ TABLE lt_ddfields  WITH KEY fieldname = gc_monat
                          TRANSPORTING NO FIELDS
                          BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_mnt_flg = gc_x.
  ELSE.
    READ TABLE lt_ddfields  WITH KEY domname = gc_monat
                            TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      lv_mnt_flg = gc_x.
    ENDIF.
  ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
*Check if document number is there in the table
  READ TABLE lt_ddfields  WITH KEY fieldname = gc_vbeln
                                   keyflag   = gc_x
                          TRANSPORTING NO FIELDS
                          BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_vbln_flg = gc_x.
  ELSE.
    READ TABLE lt_ddfields  WITH KEY domname = gc_vbeln
                                     keyflag = gc_x
                            TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      lv_vbln_flg = gc_x.
    ENDIF.
  ENDIF.
*Check if item number is there in the table
  READ TABLE lt_ddfields  WITH KEY fieldname = gc_posnr
                                   keyflag   = gc_x
                          TRANSPORTING NO FIELDS
                          BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_posnr_flg = gc_x.
  ELSE.
    READ TABLE lt_ddfields  WITH KEY domname = gc_posnr
                                     keyflag = gc_x
                            TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      lv_posnr_flg = gc_x.
    ENDIF.
  ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156

* Start of add for KOTTAPAN for CHG0206319
  IF gv_table = gc_bseg.
    lv_budat_flg = gc_x.
  ENDIF.
* End of add for KOTTAPAN for CHG0206319

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'BKR'.
        IF lv_bkr_flg = gc_x."gc_set'.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'GJR'.
        IF lv_gjr_flg = gc_x."gc_set'.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'MNT'.
        IF lv_mnt_flg = gc_x."gc_set'.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
      WHEN 'VBN'.
        IF lv_vbln_flg = gc_x."gc_set'.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'PSN'.
        IF lv_posnr_flg = gc_x."gc_set'.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
* Start of add for KOTTAPAN for CHG0206319
      WHEN 'BDT'.
        IF lv_budat_flg = gc_x."gc_set'.
          screen-invisible  = 0.
          screen-active     = 1.
        ELSE.
          screen-invisible  = 1.
          screen-active     = 0.
        ENDIF.
        MODIFY SCREEN.
* End of add for KOTTAPAN for CHG0206319
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " F_TOGGLE_SEL_FLDS
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_SPOOL
*&---------------------------------------------------------------------*
*     Display spool ALV
*----------------------------------------------------------------------*
FORM f_display_spool .
  DATA:lt_fieldcat TYPE STANDARD TABLE OF slis_fieldcat_alv
                   INITIAL SIZE 0.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = gv_table
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc = 0.
* Implement suitable error handling here
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        it_fieldcat        = lt_fieldcat
      TABLES
        t_outtab           = <fs_output>
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.
ENDFORM.                    " F_DISPLAY_SPOOL
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
*&---------------------------------------------------------------------*
*&      Form  F_SEND_MAIL_ALLFILE
*&---------------------------------------------------------------------*
*    Send Mail and attach whole table extract ifattacment size not
*    specified by user in selection screen
*----------------------------------------------------------------------*
*-->PIT_OUTPUT Data table for display
*-->PI_INDEX_LO Low Index for data limit in attachment file for size
*   limit
*-->PI_INDEX_HI High Index for data limit in attachment file for size
*   limit
*----------------------------------------------------------------------*
FORM f_send_mail_allfile USING pit_output  TYPE tt_output
                               pi_index_lo TYPE syindex
                               pi_index_hi TYPE syindex.

  DATA:  lo_send_request     TYPE REF TO cl_bcs,
         lo_document         TYPE REF TO cl_document_bcs,
         lo_recipient        TYPE REF TO if_recipient_bcs,
         lo_send_exc         TYPE REF TO cx_send_req_bcs,
         lo_addr_exc         TYPE REF TO cx_address_bcs,
         lo_send_email_mess  TYPE REF TO cx_document_bcs,
         lo_document_bcs     TYPE REF TO cx_document_bcs.
  DATA:  lt_message_body     TYPE bcsy_text,
         ls_message_body     TYPE soli,
         lt_att_content_hex  TYPE solix_tab.
  DATA:  lv_sent_to_all      TYPE os_boolean.
  DATA:  lt_return           TYPE STANDARD TABLE OF bapiret2
                             INITIAL SIZE 0,
       ls_return             TYPE bapiret2,
       ls_address            TYPE bapiaddr3.
  DATA:lv_attach_sub         TYPE sood-objdes,
       lv_email              TYPE ad_smtpadr,
       lv_fileno             TYPE string,
       lv_ext                TYPE soodk-objtp,
       lv_message            TYPE string.
  DATA:ls_object_hd_chg      TYPE sood1.
  lv_ext = gv_ext2.
  IF gv_ext2 EQ 'XLS' OR gv_ext3 = 'XLSX'.
    PERFORM f_fill_attachment_xls_all USING pit_output
                                            pi_index_lo
                                            pi_index_hi
                                   CHANGING lt_att_content_hex.
  ELSE.
    PERFORM f_fill_attachment_oth_all USING pit_output
                                            pi_index_lo
                                            pi_index_hi
                               CHANGING lt_att_content_hex.
  ENDIF.

  TRY.
      "create send request
      lo_send_request = cl_bcs=>create_persistent( ).
      "create message body and subject
      APPEND text-t03 TO lt_message_body.
      APPEND INITIAL LINE TO lt_message_body.
      CONCATENATE text-t04 p_table text-t05
             INTO ls_message_body
     SEPARATED BY space.
      APPEND ls_message_body TO lt_message_body.
      APPEND INITIAL LINE TO lt_message_body.
      APPEND text-t06 TO lt_message_body.

      "put your text into the document
      lo_document = cl_document_bcs=>create_document(
                       i_type = 'RAW'
                       i_text = lt_message_body
                       i_subject = text-t01 ).

      lv_fileno = pi_index_hi.
      IF gv_ext2 IS NOT INITIAL.
        CONCATENATE gv_table '_' text-t02 lv_fileno '.' gv_ext2
               INTO lv_attach_sub.
      ELSEIF gv_ext3 IS NOT INITIAL.
        gv_ext2 = gv_ext3.
        CONCATENATE gv_table '_' text-t02 lv_fileno '.' gv_ext3
               INTO lv_attach_sub.
      ENDIF.
      CONDENSE lv_attach_sub."attachment title
      DATA:lv_attach TYPE so_obj_len.
      lv_attach               = gv_attach_length.
      ls_object_hd_chg-objlen = lv_attach.
      TRY.
          IF gv_ext2 IS NOT INITIAL.
            lo_document->add_attachment(
               EXPORTING
                 i_attachment_type    = gv_ext2
                 i_attachment_subject = lv_attach_sub
                 i_attachment_size    = ls_object_hd_chg-objlen
                 i_att_content_hex    = lt_att_content_hex ).
          ENDIF.
        CATCH cx_document_bcs INTO lo_document_bcs.
      ENDTRY.

* Add attachment
* Pass the document to send request
      lo_send_request->set_document( lo_document ).
      "Create recipient
      IF p_email IS INITIAL.
        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = sy-uname
          IMPORTING
            address  = ls_address
          TABLES
            return   = lt_return.
        READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
        IF sy-subrc IS NOT INITIAL.
          lv_email     = ls_address-e_mail.
          lo_recipient =
          cl_cam_address_bcs=>create_internet_address( lv_email ).
        ENDIF.
      ELSE.
        lo_recipient   =
        cl_cam_address_bcs=>create_internet_address( p_email ).
      ENDIF.

*Set recipient
      lo_send_request->add_recipient(
           EXPORTING
             i_recipient = lo_recipient i_express = gc_x ).

      lo_send_request->add_recipient( lo_recipient ).

* Send email
      lo_send_request->set_send_immediately( gc_x ).
      lo_send_request->send(
        RECEIVING
          result = lv_sent_to_all ).
    CATCH cx_send_req_bcs INTO lo_send_exc.

    CATCH cx_address_bcs  INTO lo_addr_exc.

    CATCH cx_document_bcs INTO lo_send_email_mess.
  ENDTRY.
  IF lo_send_exc IS NOT INITIAL.
    lv_message = lo_send_exc->get_text( ).
  ELSEIF lo_addr_exc IS NOT INITIAL.
    lv_message = lo_addr_exc->get_text( ).
  ELSEIF lo_send_email_mess IS NOT INITIAL.
    lv_message = lo_send_email_mess->get_text( ).
  ENDIF.
  IF lv_message IS INITIAL.
    COMMIT WORK.
    MESSAGE s000(zfi01) WITH text-s01.
  ELSE.
    MESSAGE e000(zfi01) WITH lv_message.
  ENDIF.
ENDFORM.                                     " F_SEND_MAIL_ALLFILE
*&---------------------------------------------------------------------*
*&      Form  F_FILL_ATTACHMENT_XLS_ALL
*&---------------------------------------------------------------------*
*    Create XLS attachment
*----------------------------------------------------------------------*
*-->PIT_OUTPUT   Output file
*-->PI_INDEXLO   Lower limit for data in file
*-->PI_INDEXHI   higher Limit for data in file
*<--PCT_HEX_CONTENT Hexadecimal content
*----------------------------------------------------------------------*
FORM f_fill_attachment_xls_all  USING pit_output      TYPE tt_output
                                      pi_indexlo      TYPE syindex
                                      pi_indexhi      TYPE syindex
                             CHANGING pct_hex_content TYPE solix_tab.

  DATA lv_string         TYPE string .
  DATA:lt_binary_content TYPE solix_tab.
  DATA:lt_output_part    TYPE tt_output.
  DATA lv_xstring        TYPE xstring .
  DATA:ls_output         TYPE gty_output.
  APPEND LINES OF pit_output
             FROM pi_indexlo
               TO pi_indexhi
               TO lt_output_part.
  LOOP AT lt_output_part INTO ls_output.
    CONCATENATE  lv_string ls_output-line
            INTO lv_string
    SEPARATED BY cl_abap_char_utilities=>newline.
  ENDLOOP.
**Convert string to xstring
  CALL FUNCTION 'HR_KR_STRING_TO_XSTRING'
    EXPORTING
      unicode_string   = lv_string
    IMPORTING
      xstring_stream   = lv_xstring
    EXCEPTIONS
      invalid_codepage = 1
      invalid_string   = 2
      OTHERS           = 3.
  IF sy-subrc = 0.
**Convert string to xstring
    CALL METHOD cl_bcs_convert=>xstring_to_solix
      EXPORTING
        iv_xstring = lv_xstring
      RECEIVING
        et_solix   = lt_binary_content.
  ENDIF.
  IF lt_binary_content IS NOT INITIAL.
    pct_hex_content  = lt_binary_content.
    gv_attach_length = xstrlen( lv_xstring ).
  ENDIF.

ENDFORM.                    " F_FILL_ATTACHMENT_XLS_ALL
*&---------------------------------------------------------------------*
*&      Form  F_FILL_ATTACHMENT_OTH_ALL
*&---------------------------------------------------------------------*
* Fill attachment in other format
*----------------------------------------------------------------------*
*-->PIT_OUTPUT   Output file
*-->PI_INDEXLO   Lower limit for data in file
*-->PI_INDEXHI   higher Limit for data in file
*<--PCT_HEX_CONTENT Hexadecimal content
*----------------------------------------------------------------------*
FORM f_fill_attachment_oth_all  USING pit_output      TYPE tt_output
                                      pi_indexlo      TYPE syindex
                                      pi_indexhi      TYPE syindex
                             CHANGING pct_hex_content TYPE solix_tab.

  DATA lv_xstring TYPE xstring .
  DATA lv_length  TYPE i VALUE 0.
  DATA:lt_binary_content TYPE solix_tab.
  DATA:lv_pdf_obj_len    TYPE so_obj_len.
  DATA: lv_size1 TYPE abap_msize,
        lv_size2 TYPE abap_msize,
        lv_size3 TYPE abap_msize,
        lv_size4 TYPE abap_msize,
        lv_flag1 TYPE char128,
        lv_size5 TYPE abap_msize,
        lv_size6 TYPE abap_msize,
        lv_flag2 TYPE char128,
        lv_flag3 TYPE char128.
  DATA:lt_output_part TYPE STANDARD TABLE OF gty_output
                      INITIAL SIZE 0,
       ls_output      TYPE gty_output.
**Convert string to xstring
  IF rb_mail IS NOT INITIAL.
    APPEND LINES OF pit_output
            FROM pi_indexlo
              TO pi_indexhi
              TO lt_output_part.
    TRY.
      CALL METHOD cl_abap_memory_utilities=>get_memory_size_of_object
        EXPORTING
          object                     = lt_output_part
        IMPORTING
          bound_size_alloc           = lv_size1
          bound_size_used            = lv_size2
          referenced_size_alloc      = lv_size3
          referenced_size_used       = lv_size4
          is_part_of_non_trivial_szk = lv_flag1
          szk_size_alloc             = lv_size5
          szk_size_used              = lv_size6
          low_mem                    = lv_flag2
          is_in_shared_memory        = lv_flag3.
    ENDTRY.
    lv_length = lv_size4.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_length
      IMPORTING
        buffer       = lv_xstring
      TABLES
        binary_tab   = lt_output_part
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc = 0.
* Implement suitable error handling here
    ENDIF.
***Xstring to binary
    IF lv_xstring IS NOT INITIAL.
      lv_pdf_obj_len = xstrlen( lv_xstring ).
      lt_binary_content = cl_document_bcs=>xstring_to_solix(
                      ip_xstring = lv_xstring ).
      pct_hex_content = lt_binary_content.
    ENDIF.


  ENDIF.
ENDFORM.                    " F_FILL_ATTACHMENT_OTH_ALL
*&---------------------------------------------------------------------*
*&      Form  F_DWNLD_XLSX
*&---------------------------------------------------------------------*
*      Download file to XLSX format
*----------------------------------------------------------------------*
*      -->PI_FILE  text
*----------------------------------------------------------------------*
FORM f_dwnld_xlsx  USING  pi_file TYPE string.
*Local internal table
  DATA:    lt_fieldnames  TYPE STANDARD TABLE OF gty_fieldnames
                          INITIAL SIZE 0.
  DATA:    lt_text        TYPE truxs_t_text_data.
  DATA:    lv_index       TYPE syindex,
           lv_fileno      TYPE n,
           lv_file        TYPE string,
           lv_index_lo    TYPE syindex,
           lv_index_hi    TYPE syindex,
           lv_blocksize   TYPE syindex,
           lv_exit_flag   TYPE flag.

  DATA:    lo_tab1 TYPE REF TO data,
           lo_tab2 TYPE REF TO data.

  FIELD-SYMBOLS: <lt_output>  TYPE STANDARD TABLE,
                 <lfs_wa>     TYPE any.


  IF p_chdr1 IS NOT INITIAL.
    CREATE DATA lo_tab1 LIKE <fs_output>.
* This would now assign the stucture of <fs_output> to the
* field-symbols <lt_output>
* So now <lt_output> has a stucture and it is a table.
    ASSIGN lo_tab1->* TO <lt_output>.
* So now <lfs_wa> becomes a work area having a structure
* like <fs_output>
    CREATE DATA lo_tab2 LIKE LINE OF <fs_output>.
    ASSIGN lo_tab2->* TO <lfs_wa>.
*Fill column headings in file
    IF gt_ddfields IS NOT INITIAL.
      PERFORM f_fill_headings USING gt_ddfields CHANGING lt_fieldnames.
    ENDIF.
*Split large files into small ones
    IF p_dpbc IS NOT INITIAL.
      lv_blocksize = p_dpbc.
      DO.
        IF lv_exit_flag EQ gc_x.
          EXIT. "loop termination
        ENDIF.
        lv_fileno = sy-index.
*Create filename dynamically
        CONCATENATE pi_file
                    '_' 'File'(t09)
                    lv_fileno
                    '.'
                    p_fext1 INTO lv_file.
        CONDENSE pi_file.
* Calculate the low and high indices for the batch of WBS elements
        lv_index    =     sy-index.
        lv_index_lo = ( ( lv_index - 1 ) * lv_blocksize ) + 1.
        lv_index_hi = (   lv_index       * lv_blocksize ).
        LOOP AT <fs_output> ASSIGNING <lfs_wa>
                                 FROM lv_index_lo
                                   TO lv_index_hi.
          APPEND <lfs_wa> TO <lt_output>.
        ENDLOOP.
        IF <lt_output> IS INITIAL.
          lv_exit_flag = gc_x.
          RETURN.
        ENDIF.
        CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
          EXPORTING
            i_field_seperator    = ','  " Comma seperator
          TABLES
            i_tab_sap_data       = <lt_output>
          CHANGING
            i_tab_converted_data = lt_text
          EXCEPTIONS
            conversion_failed    = 1
            OTHERS               = 2.
        IF sy-subrc = 0.
*download to excel with column header
          CALL METHOD cl_gui_frontend_services=>gui_download
            EXPORTING
              filename                = lv_file
            CHANGING
              data_tab                = lt_text
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
        ENDIF.
        CLEAR :<lfs_wa>,lv_file.
        REFRESH <lt_output>.
      ENDDO.
    ELSE.
*Create filename dynamically
      CONCATENATE pi_file
                  '.'
                  p_fext1 INTO pi_file.
      CONDENSE pi_file.

      CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
        EXPORTING
          i_field_seperator    = ','  " Comma seperator
        TABLES
          i_tab_sap_data       = <fs_output>
        CHANGING
          i_tab_converted_data = lt_text
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc = 0.
*download to excel with column header
        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = pi_file
          CHANGING
            data_tab                = lt_text
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
        ENDIF."endif gui_download
      ENDIF."endif sap_txt format convert
    ENDIF."endif p_dpbc check
  ELSE.
*Split large files into small ones
    IF p_dpbc IS NOT INITIAL.
      lv_blocksize = p_dpbc.
      DO.
        IF lv_exit_flag EQ gc_x.
          EXIT. "loop termination
        ENDIF.
        lv_fileno = sy-index.
*Create filename dynamically
        CONCATENATE pi_file
                    '_' 'File'(t09)
                    lv_fileno
                    '.'
                    p_fext1 INTO lv_file.
        CONDENSE pi_file.
* Calculate the low and high indices for the batch of WBS elements
        lv_index    =     sy-index.
        lv_index_lo = ( ( lv_index - 1 ) * lv_blocksize ) + 1.
        lv_index_hi = (   lv_index       * lv_blocksize ).
        LOOP AT <fs_output> ASSIGNING <lfs_wa>
                                 FROM lv_index_lo
                                   TO lv_index_hi.
          APPEND <lfs_wa> TO <lt_output>.
        ENDLOOP.
        IF <lt_output> IS INITIAL.
          lv_exit_flag = gc_x.
          RETURN.
        ENDIF.
        CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
          EXPORTING
            i_field_seperator    = ','  " Comma seperator
          TABLES
            i_tab_sap_data       = <lt_output>
          CHANGING
            i_tab_converted_data = lt_text
          EXCEPTIONS
            conversion_failed    = 1
            OTHERS               = 2.
        IF sy-subrc = 0.
*download to excel with column header
          CALL METHOD cl_gui_frontend_services=>gui_download
            EXPORTING
              filename                = lv_file
            CHANGING
              data_tab                = lt_text
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
        ENDIF.
        CLEAR :<lfs_wa>,lv_file.
        REFRESH <lt_output>.
      ENDDO.
    ELSE.
      CONCATENATE pi_file
                  '.'
                  p_fext1 INTO pi_file.
      CONDENSE pi_file.
*download to excel without column header
      CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
        EXPORTING
          i_field_seperator    = ','  " Comma seperator
        TABLES
          i_tab_sap_data       = <fs_output>
        CHANGING
          i_tab_converted_data = lt_text
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc = 0.
        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = pi_file
          CHANGING
            data_tab                = lt_text
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
        ENDIF."endif gui_download
      ENDIF."endif sap_txt conversion check
    ENDIF."endif p_dpbc check
  ENDIF."endif p_chdr1 initial
ENDFORM.                    " F_DWNLD_XLSX
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156

* Start of Add by KOTTAPAN for CHG0206319
*&---------------------------------------------------------------------*
*&      Form  BUILD_SELECT_QUERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_TABLE  text
*      -->P_GT_WH_FIELDS  text
*      -->P_GT_FIELDLIST  text
*      -->P_GR_FIELD1  text
*      -->P_GR_FIELD2  text
*      -->P_GR_FIELD3  text
*      -->P_GR_FIELD4  text
*      -->P_GR_FIELD5  text
*      -->P_GR_FIELD6  text
*      -->P_GR_FIELD7  text
*      -->P_GV_PACK_SIZE  text
*      <--P_GT_INTTAB  text
*----------------------------------------------------------------------*
FORM build_select_query  USING    p_gv_table      TYPE tabname
                                  p_gt_wh_fields  TYPE tt_wh_fields
                                  p_gt_fieldlist  TYPE tt_field_pos
                                  p_gr_field1     TYPE /sapdii/twty_rsds_selopt
                                  p_gr_field2     TYPE /sapdii/twty_rsds_selopt
                                  p_gr_field3     TYPE /sapdii/twty_rsds_selopt
                                  p_gr_field4     TYPE /sapdii/twty_rsds_selopt
                                  p_gr_field5     TYPE /sapdii/twty_rsds_selopt
                                  p_gr_field6     TYPE /sapdii/twty_rsds_selopt
                                  p_gr_field7     TYPE /sapdii/twty_rsds_selopt
                                  p_gv_pack_size  TYPE i
                         CHANGING p_gt_inttab     TYPE REF TO data.
* Copied code from class ZFI_AUDIT_EXTRACT, method BUILD_SELECT_QUERY and did changes
  FIELD-SYMBOLS: <lfs_tab_t> TYPE STANDARD TABLE.

  DATA dref TYPE REF TO data.
  CREATE DATA dref TYPE STANDARD TABLE OF (p_gv_table).
  ASSIGN dref->* TO <lfs_tab_t>.

  DATA:lv_and_flag       TYPE flag,
       lv_package_size   TYPE i,
       ls_where_clause   TYPE edpline,
       lt_where_clause   TYPE STANDARD TABLE OF edpline INITIAL SIZE 0.
  DATA:ls_fields_where   TYPE gty_wh_fields,
       ls_field_where_db TYPE gty_field_pos.
  DATA:db_cursor TYPE cursor.
  FIELD-SYMBOLS:<lfs_tab> TYPE STANDARD TABLE.
  CONSTANTS:lc_and    TYPE char3  VALUE 'AND',
            lc_in     TYPE char2  VALUE 'IN',
            lc_range1 TYPE char15 VALUE 'P_GR_FIELD1',
            lc_range2 TYPE char15 VALUE 'P_GR_FIELD2',
            lc_range3 TYPE char15 VALUE 'P_GR_FIELD3',
            lc_range4 TYPE char15 VALUE 'P_GR_FIELD4',
            lc_range5 TYPE char15 VALUE 'P_GR_FIELD5',
            lc_range6 TYPE char15 VALUE 'P_GR_FIELD6',
            lc_range7 TYPE char15 VALUE 'P_GR_FIELD7'.
  DATA:lo_root    TYPE REF TO cx_root,
       lv_message TYPE string.

  DATA:lv_filename TYPE string,
       lv_file     TYPE string.
  DATA:lv_ext TYPE char4.

  ASSIGN p_gt_inttab->* TO <lfs_tab>.
  IF <lfs_tab> IS ASSIGNED.
    lv_package_size = 2147483648 / p_gv_pack_size.
    IF  p_gr_field1 IS INITIAL
    AND p_gr_field2 IS INITIAL
    AND p_gr_field3 IS INITIAL
    AND p_gr_field4 IS INITIAL
    AND p_gr_field5 IS INITIAL
    AND p_gr_field6 IS INITIAL
    AND p_gr_field7 IS INITIAL
    AND p_gt_wh_fields IS INITIAL
    AND p_gt_fieldlist IS INITIAL.
***** To calculaten maximum no of records that can be accomodated in 2gb
      TRY.
          OPEN CURSOR WITH HOLD db_cursor FOR
          SELECT *
          FROM (p_gv_table)
          BYPASSING BUFFER.
          DO.
*** To Fetch data in chunks of 2gb
            FETCH NEXT CURSOR db_cursor
            INTO CORRESPONDING FIELDS OF TABLE <lfs_tab_t>
            PACKAGE SIZE lv_package_size.
            IF sy-subrc NE 0.
              CLOSE CURSOR db_cursor.
              EXIT.
* Start of Add on 05.04.2021
            ELSE.
              APPEND LINES OF  <lfs_tab_t> TO <lfs_tab>.
* End of Add on 05.04.2021
            ENDIF.
*** Here do the operation you want on internal table <xtab_buf>
          ENDDO.
        CATCH cx_sy_dynamic_osql_syntax    INTO lo_root.
          IF lo_root IS NOT INITIAL.
            lv_message = lo_root->get_text( ).
            MESSAGE s000(zfi01) WITH lv_message DISPLAY LIKE 'E'.
            "Dynamic SQL exceptions
            LEAVE LIST-PROCESSING.
          ENDIF.
        CATCH cx_sy_dynamic_osql_semantics INTO lo_root.
          IF lo_root IS NOT INITIAL.
            lv_message = lo_root->get_text( ).
            MESSAGE s000(zfi01) WITH lv_message DISPLAY LIKE 'E'.
            "Dynamic SQL exceptions
            LEAVE LIST-PROCESSING.
          ENDIF.
      ENDTRY.
      IF <lfs_tab> IS NOT INITIAL.
        SORT <lfs_tab>.
      ELSE.
        MESSAGE s000(zfi01) WITH text-e03 DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      LOOP AT  p_gt_fieldlist INTO ls_field_where_db.
        READ TABLE p_gt_wh_fields
              INTO ls_fields_where
          WITH KEY fieldname = ls_field_where_db-fieldname.
        IF sy-subrc IS INITIAL.
          CASE ls_fields_where-range_num.
            WHEN 1.
              IF p_gr_field1 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range1
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range1
                         INTO ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 2.
              IF p_gr_field2 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range2
                  INTO ls_where_clause SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range2
                         INTO ls_where_clause SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 3.
              IF p_gr_field3 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range3
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range3
                         INTO ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 4.
              IF p_gr_field4 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range4
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE  lc_and ls_field_where_db-fieldname lc_in
                               lc_range4
                         INTO  ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 5.
              IF p_gr_field5 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range5
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE  lc_and ls_field_where_db-fieldname lc_in
                               lc_range5
                          INTO ls_where_clause SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 6.
              IF p_gr_field6 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range6
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range6
                         INTO ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 7.
              IF p_gr_field7 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range7
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range7
                         INTO ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.
        CLEAR:ls_fields_where,ls_field_where_db,ls_where_clause.
      ENDLOOP.
      IF lt_where_clause IS NOT INITIAL.
        APPEND '.' TO lt_where_clause.
      ENDIF.
      IF lt_where_clause IS NOT INITIAL.
        TRY.
            OPEN CURSOR WITH HOLD db_cursor FOR
              SELECT *
                FROM (p_gv_table)
                BYPASSING BUFFER
                WHERE (lt_where_clause).
            DO.
*** To Fetch data in chunks of 2gb
              FETCH NEXT CURSOR db_cursor
              INTO CORRESPONDING FIELDS OF TABLE <lfs_tab_t>
              PACKAGE SIZE lv_package_size.
              IF sy-subrc NE 0.
                CLOSE CURSOR db_cursor.
                EXIT.
* Start of Add on 05.04.2021
              ELSE.

                APPEND LINES OF  <lfs_tab_t> TO <lfs_tab>.
* End of Add on 05.04.2021
              ENDIF.
*** Here do the operation you want on internal table <xtab_buf>
            ENDDO.
          CATCH cx_sy_dynamic_osql_syntax    INTO lo_root.
            IF lo_root IS NOT INITIAL.
              lv_message = lo_root->get_text( ).
              MESSAGE s000(zfi01) WITH lv_message DISPLAY LIKE 'E'.
              "Dynamic SQL exceptions
              LEAVE LIST-PROCESSING.
            ENDIF.
          CATCH cx_sy_dynamic_osql_semantics INTO lo_root.
            IF lo_root IS NOT INITIAL.
              lv_message = lo_root->get_text( ).
              MESSAGE s000(zfi01) WITH lv_message DISPLAY LIKE 'E'.
              "Dynamic SQL exceptions
              LEAVE LIST-PROCESSING.
            ENDIF.
        ENDTRY.
        IF <lfs_tab> IS NOT INITIAL.
          SORT <lfs_tab>.
        ELSE.
          MESSAGE s000(zfi01) WITH text-e03 DISPLAY LIKE 'E'.
        ENDIF."endif sy-surc chck on select
      ENDIF."endif lt_where_clause is initial
    ENDIF."endif range values check
  ENDIF.

ENDFORM.                    " BUILD_SELECT_QUERY
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_APPL_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_OUTPUT>  text
*----------------------------------------------------------------------*
FORM f_download_appl_server  USING  p_lfs_tab TYPE STANDARD TABLE.


  DATA: lv_file_path       TYPE string,
*        lv_fieldname(5000) TYPE c, " Commented by KOTTAPAN for CHG0216879
        lv_fieldname       TYPE string, " Added by KOTTAPAN for CHG0216879
        lv_field_type(10)  TYPE c,
        lv_field_text(10)  TYPE c,
        lv_line(5000)      TYPE c.

  DATA:lt_fieldnames  TYPE STANDARD TABLE OF gty_fieldnames
                      INITIAL SIZE 0,
                      lwa_fieldnames TYPE gty_fieldnames.
  FIELD-SYMBOLS : <l_fs>   TYPE any,
                  <fs_field>.

  CONSTANTS : lc_slash TYPE c VALUE '/',
              lc_dot   TYPE c VALUE '.',
              lc_p     TYPE c VALUE 'P',
              lc_i     TYPE c VALUE 'I',
              lc_b     TYPE c VALUE 'b',
              lc_x     TYPE c VALUE 'X'.

  CONCATENATE p_fpth1 lc_slash  p_fnam1 lc_dot p_fext1 INTO lv_file_path.

*To open the dataset
  OPEN DATASET lv_file_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

*Fill column headings in file
  IF gt_ddfields IS NOT INITIAL.
    PERFORM f_fill_headings USING gt_ddfields CHANGING lt_fieldnames.
  ENDIF.

  LOOP AT lt_fieldnames INTO lwa_fieldnames.
    REPLACE ALL OCCURRENCES OF ',' IN lwa_fieldnames-line WITH '_'. "Added by DADIM for CHG0217805
    IF sy-tabix = 1.
      lv_fieldname = lwa_fieldnames-line.
    ELSE.
*      CONCATENATE lv_fieldname p_fdlm1 lwa_fieldnames-line INTO lv_fieldname SEPARATED BY space. " Commented by KOTTAPAN for CHG0216879
      CONCATENATE lv_fieldname p_fdlm1 lwa_fieldnames-line INTO lv_fieldname." Added by KOTTAPAN for CHG0216879
    ENDIF.

  ENDLOOP.
  TRANSFER lv_fieldname TO lv_file_path.

*To write data to a file on your application sever
  LOOP AT p_lfs_tab ASSIGNING <l_fs>.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <l_fs> TO <fs_field>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      IF lv_line IS INITIAL AND sy-index <> 1.
        lv_line = <fs_field>.
      ELSEIF sy-index <> 1.
        DESCRIBE FIELD <fs_field> TYPE lv_field_type.
        IF lv_field_type = lc_p OR lv_field_type = lc_i OR
           lv_field_type = lc_b OR lv_field_type = lc_x.
          lv_field_text = <fs_field>.
          CONDENSE lv_field_text NO-GAPS.
          REPLACE ALL OCCURRENCES OF ',' IN lv_field_text WITH '_'. "Added by DADIM for CHG0217805
          CONCATENATE lv_line p_fdlm1 lv_field_text INTO lv_line.
        ELSE.
          REPLACE ALL OCCURRENCES OF ',' IN <fs_field> WITH '_'. "Added by DADIM for CHG0217805
          CONCATENATE lv_line p_fdlm1 <fs_field> INTO lv_line.
        ENDIF.
      ENDIF.
    ENDDO.
    TRANSFER lv_line TO lv_file_path.
    CLEAR: lv_line.
  ENDLOOP.

* Close the data set you have opened earlier
  CLOSE DATASET lv_file_path.

ENDFORM.                    " F_DOWNLOAD_APPL_SERVER

* End of Add by KOTTAPAN for CHG0206319
