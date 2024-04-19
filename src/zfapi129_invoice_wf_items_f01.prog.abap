*&---------------------------------------------------------------------*
*&  Include           ZFAPI129_INVOICE_WF_ITEMS_F01
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI129_INVOICE_WF_ITEMS                      *
* Include Program    :  ZFAPI129_INVOICE_WF_ITEMS_F01                  *
* Author             :  Vijay Rajaputra                                *
* Creation Date      :  05-Nov-2018                                    *
* Application Area   :  FICO                                           *
* Technical Contact  :  Vijay Rajaputra                                *
*                                                                      *
* Purpose            :  Subroutines Include Program                    *
*                        - General and Workflow Extract Coding         *
*                                                                      *
*----------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 05-Nov-2018  VRAJAPUTRA  D30K929291  CHG0130803  Initial development *
*                          D30K929374                                  *
* 22-Jan-2019  VRAJAPUTRA  D30K929505  CHG0135339  Duplicate checks    *
*                          D30K929531                                  *
* 31-Jan-2019  VRAJAPUTRA  D30K929554  CHG0136094  Duplicate checks    *
* 13-Feb-2019  VRAJAPUTRA  D30K929576  CHG0137162  Additions to Extract*
* 14-Mar-2019  AKMADASU    D30K929708  CHG0139790  Reworked Invoices   *
* 14-Mar-2019  KBANERJEE   D30K929710  CHG0139790  Reworked Invoices-FUT
* 19-Mar-2019  KBANERJEE   D30K929714  CHG0140727  Add documents with  *
*                                                  reference transaction
*                                                  "ZCARS".            *
* 22-Mar-2019  KBANERJEE   D30K929732  CHG0139790  Reworked Invoices   *
* 04 Apr 2019  KBANERJEE   D30K929748  DFCT0017064 The IAP 129
*                          D30K929750              interface is unable *
*                                                  to classify reworked*
*                                                  invoices appropriatly
*&---------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  f_default_selection_screen
*&---------------------------------------------------------------------*
*       Default the selection screen parameters
*----------------------------------------------------------------------*
FORM f_default_selection_screen.

  DATA:    ls_xparam                   TYPE gty_xparam.

  DATA:    lt_sscr_restrict            TYPE sscr_restrict,
           ls_sscr_opt_list            TYPE sscr_opt_list,
           ls_sscr_ass                 TYPE sscr_ass.

  CONSTANTS:
           lc_erpid     TYPE char05        VALUE 'SAPUG',
           lc_tzone     TYPE char05        VALUE ' ',
           lc_ptype     TYPE zparamtype    VALUE 'OUTBOUND_INTERFACE',
           lc_stype     TYPE zparamsubtype VALUE 'I_P2C_AP_129',
           lc_key1_1    TYPE zparamkey     VALUE 'WF_TASK',
           lc_key2_11   TYPE zparamkey     VALUE 'INVOICE_IMAGE',
           lc_key2_12   TYPE zparamkey     VALUE 'NPO_INVOICE_START',
           lc_key2_13   TYPE zparamkey     VALUE 'NPO_INVOICE_RCO',
           lc_key2_14   TYPE zparamkey     VALUE 'NPO_INVOICE_APPROVE',
           lc_key2_15   TYPE zparamkey     VALUE 'NPO_INVOICE_POST',
           lc_key2_16   TYPE zparamkey     VALUE 'PO_INVOICE_START',
           lc_key2_17   TYPE zparamkey     VALUE 'PO_INVOICE_APPROVE',
           lc_key2_18   TYPE zparamkey     VALUE 'PO_INVOICE_POST',
           lc_key1_2    TYPE zparamkey     VALUE 'IMAGE_DOC_TYPE',
           lc_configure TYPE zparamkey     VALUE 'CONFIGURE'.

  MOVE:    lc_erpid                      TO p_erpid,
           lc_tzone                      TO p_tzone.

* Select the program parameters
  CLEAR    gt_xparam[].

  SELECT   *
    INTO   TABLE gt_xparam
    FROM   zfit_xparam
   WHERE   paramtype = lc_ptype
     AND   subtype   = lc_stype.
  IF     ( sy-subrc EQ 0 ).

*eject
* Default the workflow task ids
    CLEAR                                   ls_xparam.
    LOOP AT  gt_xparam                 INTO ls_xparam
                                      WHERE key1 = lc_key1_1.

      CASE     ls_xparam-key2.

        WHEN     lc_key2_11. "INVOICE_IMAGE.
          MOVE     ls_xparam-value1      TO p_wfiimg.

        WHEN     lc_key2_12. "NPO_INVOICE_START.
          MOVE     ls_xparam-value1      TO p_wfnpis.

        WHEN     lc_key2_13. "NPO_INVOICE_RCO.
          MOVE     ls_xparam-value1      TO p_wfnpir.

        WHEN     lc_key2_14. "NPO_INVOICE_APPROVE.
          MOVE     ls_xparam-value1      TO p_wfnpia.

        WHEN     lc_key2_15. "NPO_INVOICE_POST.
          MOVE     ls_xparam-value1      TO p_wfnpip.

        WHEN     lc_key2_16. "PO_INVOICE_START.
          MOVE     ls_xparam-value1      TO p_wfpois.

        WHEN     lc_key2_17. "PO_INVOICE_APPROVE.
          MOVE     ls_xparam-value1      TO p_wfpoia.

        WHEN     lc_key2_18. "PO_INVOICE_POST.
          MOVE     ls_xparam-value1      TO p_wfpoip.

        WHEN     OTHERS.

      ENDCASE.

      CLEAR  ls_xparam.
    ENDLOOP.

  ENDIF.

*eject
* Default the image document types
  CLEAR                                     ls_xparam.
  LOOP AT  gt_xparam                   INTO ls_xparam
                                      WHERE key1 = lc_key1_2.
    CLEAR                                   s_doctyp.
    MOVE   'I'                           TO s_doctyp-sign.
    MOVE   'EQ'                          TO s_doctyp-option.
    MOVE   ls_xparam-value1              TO s_doctyp-low.
    APPEND                                  s_doctyp.
    CLEAR  ls_xparam.
  ENDLOOP.

  IF     ( s_doctyp[]                    IS INITIAL ).
    CLEAR                                   s_doctyp.
    MOVE   'I'                           TO s_doctyp-sign.
    MOVE   'EQ'                          TO s_doctyp-option.
    MOVE   lc_configure                  TO s_doctyp-low.
    APPEND                                  s_doctyp.
  ENDIF.

* Configure the image document type select option as a list of values
  CLEAR    lt_sscr_restrict.

  CLEAR                                ls_sscr_opt_list.
  MOVE     'OPT_LIST'               TO ls_sscr_opt_list-name.
  MOVE     'X'                      TO ls_sscr_opt_list-options-eq.
  APPEND                               ls_sscr_opt_list
                                    TO lt_sscr_restrict-opt_list_tab.

* Apply the option list to the selection-screen select-option lists
  CLEAR                                ls_sscr_ass.
  MOVE     'S'                      TO ls_sscr_ass-kind.
  MOVE     'S_DOCTYP'               TO ls_sscr_ass-name.
  MOVE     'I'                      TO ls_sscr_ass-sg_main.
  MOVE     'OPT_LIST'               TO ls_sscr_ass-op_main.
  APPEND                               ls_sscr_ass
                                    TO lt_sscr_restrict-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = lt_sscr_restrict.

ENDFORM.                    " f_default_selection_screen
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_filepath_filename
*&---------------------------------------------------------------------*
*       Get the application server filepath and filename
*----------------------------------------------------------------------*
FORM f_get_filepath_filename
  CHANGING cv_filepath  TYPE string
           cv_filename  TYPE string.

  CLEAR    cv_filepath.
  CLEAR    cv_filename.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = gc_lgcfilpth
    IMPORTING
      file_name        = cv_filepath
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  IF     ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* File Name to Selection Screen
  MOVE     text-051                      TO cv_filename.

ENDFORM.                    " f_get_filepath_filename
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_f4_help_file_path
*&---------------------------------------------------------------------*
*       Help dialog to retrieve the application server file path
*----------------------------------------------------------------------*
FORM f_get_f4_help_file_path.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = p_path2
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF     ( sy-subrc NE 0 ).
    MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_get_f4_help_file_path
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_f4_help_file_path1
*&---------------------------------------------------------------------*
*       Help dialog to retrieve the presentation server filename
*----------------------------------------------------------------------*
FORM f_get_f4_help_file_path1.

  DATA: lv_repid                       TYPE sy-repid,
        lv_dynnr                       TYPE sy-dynnr,
        lv_file                        TYPE rlgrap-filename.

  CLEAR                                     lv_repid.
  MOVE     sy-repid                      TO lv_repid.
  CLEAR                                     lv_dynnr.
  MOVE     sy-dynnr                      TO lv_dynnr.

  CLEAR    lv_file.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = lv_repid
      dynpro_number = lv_dynnr
    CHANGING
      file_name     = lv_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF     ( sy-subrc EQ 0 ).

    MOVE   lv_file TO p_path1.

  ELSE.

    MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.                    " f_get_f4_help_file_path1
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  DATA:    ls_xparam                   TYPE gty_xparam.

  DATA:    lv_filename                 TYPE text128,
           lv_date_est                 TYPE char10,
           lv_time_est                 TYPE char8,
           lv_date                     TYPE char8,
           lv_time                     TYPE char6,
           lv_dtsl_n(14)               TYPE n,
           lv_dtsh_n(14)               TYPE n,
           lv_crea_tmp_l               TYPE swfrcrets,
           lv_crea_tmp_h               TYPE swfrcrets.

  CONSTANTS: lc_region                 TYPE zparamkey VALUE 'REGION'.

  CLEAR    gt_object[].
  CLEAR    gt_top_wi_id[].
  CLEAR    gt_wi_top[].
  CLEAR    gt_wi2obj_key[].
  CLEAR    gt_output[].

  CLEAR    gv_fl_sys_us.
  CLEAR    gv_fl_sys_ca.
  CLEAR    gv_cn_recs_total.
  CLEAR    gv_cn_recs_max.
  CLEAR    gv_cn_recs_file.
  CLEAR    gv_cn_files.
  CLEAR    gv_filename.
  CLEAR    gv_filename_p.

* Set the US or Canadian system flags
  CLEAR                                     ls_xparam.
  READ     TABLE gt_xparam             INTO ls_xparam
                                   WITH KEY key1 = lc_region.
  IF     ( sy-subrc EQ 0 ).
    IF     ( ls_xparam-value1            EQ text-061 ).
      MOVE     abap_true                 TO gv_fl_sys_us.
    ELSEIF ( ls_xparam-value1            EQ text-062 ).
      MOVE    abap_true                  TO gv_fl_sys_ca.
    ELSEIF ( ls_xparam-value1            EQ text-063 ).
      MOVE    abap_true                  TO gv_fl_sys_ca.
    ENDIF.
  ENDIF.

*eject
* Set the maximum number of records in a file
  gv_cn_recs_max = p_maxrec.

* Set the filename
  IF     ( ( rb_appl                     IS NOT INITIAL ) AND
           ( p_file2                     IS NOT INITIAL )     ).

    CLEAR    lv_date_est.
    CLEAR    lv_time_est.
    CLEAR    lv_date.
    CLEAR    lv_time.

    PERFORM  f_format_date_and_time   USING sy-datum
                                            sy-uzeit
                                   CHANGING lv_date_est
                                            lv_time_est.

    CLEAR                                   lv_filename.
    MOVE       p_file2                   TO lv_filename.

    IF     ( ( lv_filename               CS 'SSSSS'     ) AND
             ( p_erpid                   IS NOT INITIAL )     ).
      REPLACE  'SSSSS'                   IN lv_filename
                                       WITH p_erpid.
    ENDIF.

    IF       ( lv_filename               CS 'MMDDYYYY' ).
      CLEAR                                 lv_date.
      MOVE     lv_date_est+5(2)          TO lv_date+0(2).
      MOVE     lv_date_est+8(2)          TO lv_date+2(2).
      MOVE     lv_date_est+0(4)          TO lv_date+4(4).
      REPLACE  'MMDDYYYY'                IN lv_filename
                                       WITH lv_date.
    ENDIF.

    IF       ( lv_filename               CS 'HHMMSS' ).
      CLEAR                                 lv_time.
      MOVE     lv_time_est+0(2)          TO lv_time+0(2).
      MOVE     lv_time_est+3(2)          TO lv_time+2(2).
      MOVE     lv_time_est+6(2)          TO lv_time+4(2).
      REPLACE  'HHMMSS'                  IN lv_filename
                                       WITH lv_time.
    ENDIF.

    MOVE       lv_filename               TO gv_filename_p.

  ENDIF.

*eject
* Create the time stamp range
  CLEAR    lv_dtsl_n.
  CLEAR    lv_dtsh_n.
  CLEAR    lv_crea_tmp_l.
  CLEAR    lv_crea_tmp_h.

  MOVE     s_cpudt-low                   TO lv_dtsl_n+0(8).
  MOVE     '000000'                      TO lv_dtsl_n+8(6).

  IF     ( s_cpudt-high                  IS NOT INITIAL ).
    MOVE   s_cpudt-high                  TO lv_dtsh_n+0(8).
  ELSE.
    MOVE   s_cpudt-low                   TO lv_dtsh_n+0(8).
  ENDIF.

  MOVE     '235999'                      TO lv_dtsh_n+8(6).

  MOVE     lv_dtsl_n                     TO lv_crea_tmp_l.
  MOVE     lv_dtsh_n                     TO lv_crea_tmp_h.

  CLEAR                                     gs_crea_tmp.
  MOVE     'I'                           TO gs_crea_tmp-sign.
  MOVE     'BT'                          TO gs_crea_tmp-option.
  MOVE     lv_crea_tmp_l                 TO gs_crea_tmp-low.
  MOVE     lv_crea_tmp_h                 TO gs_crea_tmp-high.
  APPEND   gs_crea_tmp                   TO gr_crea_tmp.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_main
*&---------------------------------------------------------------------*
*       Main process
*----------------------------------------------------------------------*
FORM f_process_main.

  DATA:    lv_cn_recs   TYPE char12.

* Open the application server dataset
  IF       ( rb_appl      IS NOT INITIAL ).
    PERFORM  f_open_dataset.
  ELSEIF   ( rb_pres      IS NOT INITIAL ).
    PERFORM  f_create_header_rec.
  ENDIF.

* Extract items via the accounting documents
  IF       ( cb_viaad     IS NOT INITIAL ).

* Process the parked invoices
    IF       ( cb_prkd    IS NOT INITIAL ).

      PERFORM  f_process_parked_invoices.

    ENDIF.

* Process the open invoices
    IF       ( cb_open    IS NOT INITIAL ).

      PERFORM  f_process_open_invoices.

    ENDIF.

* Process the cleared invoices
    IF       ( cb_clrd    IS NOT INITIAL ).

      PERFORM  f_process_cleared_invoices.

    ENDIF.

  ENDIF.

* Extract items via the workflow
  IF   ( cb_viawf         IS NOT INITIAL ).

    PERFORM  f_extract_via_workflow.

  ENDIF.

* Extract the unregistered images                           "D30K929554
  IF       ( cb_ximgs     IS NOT INITIAL ).                 "D30K929554

    PERFORM  f_extract_unregistered_images.                 "D30K929554

  ENDIF.                                                    "D30K929554

*eject
* Close the application server dataset
  IF       ( rb_appl      IS NOT INITIAL ).
    IF     ( gv_filename  IS NOT INITIAL ).
      CLOSE  DATASET gv_filename.
    ENDIF.
  ENDIF.

* Download the output data
  IF       ( rb_pres      IS NOT INITIAL ).
    PERFORM  f_download_output.
  ENDIF.

* Write the record counts to the spool
  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_total              TO lv_cn_recs.
  WRITE:  /001 text-081,                035 lv_cn_recs.

ENDFORM.                    " f_process_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_dataset
*&---------------------------------------------------------------------*
*       Open the application server dataset
*----------------------------------------------------------------------*
FORM f_open_dataset.

  DATA:    lv_filename                 TYPE text256,
           lv_numc3                    TYPE numc3,
           lv_numc4                    TYPE numc4,
           lv_msg                      TYPE text100.

  ADD        1                           TO gv_cn_files.

  CLEAR                                     lv_filename.
  MOVE       gv_filename_p               TO lv_filename.

  IF       ( lv_filename                 CS 'NNNN' ).
    CLEAR                                   lv_numc4.
    MOVE     gv_cn_files                 TO lv_numc4.
    REPLACE  'NNNN'                      IN lv_filename
                                       WITH lv_numc4.
  ELSEIF   ( lv_filename                 CS 'NNN'  ).
    CLEAR                                   lv_numc3.
    MOVE     gv_cn_files                 TO lv_numc3.
    REPLACE  'NNN'                       IN lv_filename
                                       WITH lv_numc3.
  ENDIF.

  CLEAR                                     gv_filename.
  CONCATENATE   p_path2   lv_filename  INTO gv_filename.

  OPEN     DATASET gv_filename FOR OUTPUT IN TEXT MODE
                               ENCODING DEFAULT MESSAGE lv_msg.
  IF     ( sy-subrc NE 0 ).
    WRITE:   /001                 text-093, lv_msg.
    MESSAGE  e000(zfi01)     WITH text-093  lv_msg.
  ENDIF.

  PERFORM  f_create_header_rec.

ENDFORM.                    " f_open_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_header_rec
*&---------------------------------------------------------------------*
*       Create the header record
*----------------------------------------------------------------------*
FORM f_create_header_rec.

  DATA:    ls_output                   TYPE gty_output.

  CLEAR                                     ls_output.
  CONCATENATE text-c01  text-c02  text-c03  text-c04
              text-c05  text-c06  text-c07  text-c08
              text-c09  text-c10  text-c11  text-c12
              text-c13  text-c14  text-c15  text-c16
              text-c17  text-c18  text-c19  text-c20
              text-c21  text-c22  text-c23  text-c24
              text-c25  text-c26  text-c27  text-c28
              text-c29  text-c30  text-c31  text-c32
              text-c33  text-c34  text-c35  text-c36
              text-c37  text-c38  text-c39  text-c40
              text-c41  text-c42  text-c43  text-c44
              text-c45  text-c46  text-c47  text-c48
              text-c49  text-c50  text-c51  text-c52
              text-c53  text-c54  text-c55  text-c56
              text-c57  text-c58  text-c59  text-c60
              text-c61  text-c62  text-c63  text-c64
              text-c65  text-c66  text-c67  text-c68
              text-c69  text-c70  text-c71  text-c72
              text-c73  text-c74
                                       INTO ls_output
                               SEPARATED BY gc_delim.

  IF       ( rb_appl                     IS NOT INITIAL ).
    TRANSFER ls_output                   TO gv_filename.
  ELSEIF   ( rb_pres                     IS NOT INITIAL ).
    APPEND   ls_output                   TO gt_output.
  ENDIF.

  ADD      1                             TO gv_cn_recs_file.
  ADD      1                             TO gv_cn_recs_total.

ENDFORM.                    " f_create_header_rec
*eject
*&---------------------------------------------------------------------*
*&      Form  f_download_output
*&---------------------------------------------------------------------*
*       Download the output
*----------------------------------------------------------------------*
FORM f_download_output.

  DATA:    lv_filename  TYPE string.

  IF     ( p_path1        IS INITIAL ).
    RETURN.
  ENDIF.

  IF     ( gt_output[]    IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                      lv_filename.
  MOVE     p_path1        TO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = gt_output
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

  IF     ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_download_output
*eject
*&---------------------------------------------------------------------*
*&      Form  f_extract_unregistered_images
*&---------------------------------------------------------------------*
*       Extract the unregistered images
*----------------------------------------------------------------------*
FORM f_extract_unregistered_images.

  DATA:    lt_wi_id               TYPE gtt_wi_id,
           lt_wi_id_p             TYPE gtt_wi_id,
           lt_swihead             TYPE gtt_swwwihead,
           lt_wi2obj              TYPE gtt_wi2obj,
           lt_wicont              TYPE gtt_wicont,
           ls_rec                 TYPE gty_rec.

  DATA:    lv_subrc               TYPE sysubrc,             "D30K929554
           lv_index               TYPE syindex,
           lv_index_lo            TYPE syindex,
           lv_index_hi            TYPE syindex,
           lv_instid              TYPE sibfboriid.          "D30K929554

  CONSTANTS:
           lc_blocksize           TYPE syindex VALUE 5000,
           lc_witype_wrkitm       TYPE sww_witype VALUE 'W',
           lc_wistat_ready        TYPE sww_wistat VALUE 'READY',
           lc_wistat_slctd        TYPE sww_wistat VALUE 'SELECTED',
           lc_wistat_strtd        TYPE sww_wistat VALUE 'STARTED',
                                                        "BOC D30K929576
           lc_wistat_watng        TYPE sww_wistat VALUE 'WAITING',
           lc_wistat_cmpld        TYPE sww_wistat VALUE 'COMPLETED',
                                                        "EOC D30K929576
           lc_typeid_image        TYPE sibftypeid VALUE 'IMAGE',
           lc_elemnt_doctyp       TYPE swc_elem   VALUE 'DOCUMENTTYPE',
           lc_doctyp_fiinv        TYPE saeanwdid  VALUE 'ZFIINVNP',
           lc_doctyp_mminv        TYPE saeanwdid  VALUE 'ZMMINV'.

  FIELD-SYMBOLS: <fs_swihead>     TYPE gty_swwwihead,
                 <fs_wi2obj>      TYPE gty_wi2obj,
                 <fs_wicont>      TYPE gty_wicont.

  IF     ( cb_ximgs IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_wi_id[].
  CLEAR    lt_wi_id_p[].
  CLEAR    lt_swihead[].
  CLEAR    lt_wi2obj[].
  CLEAR    lt_wicont[].

*eject
* Select the image work item id's
  SELECT   wi_id
    INTO   TABLE lt_wi_id
    FROM   swwwihead
   WHERE   wi_type     =  lc_witype_wrkitm
     AND   wi_stat    IN (lc_wistat_ready, lc_wistat_slctd,
                          lc_wistat_watng, lc_wistat_cmpld, "D30K929576
                          lc_wistat_strtd)
     AND   crea_tmp   IN  gr_crea_tmp
     AND   wi_rh_task  =  p_wfiimg.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SORT     lt_wi_id            ASCENDING BY wi_id.
  DELETE   ADJACENT DUPLICATES         FROM lt_wi_id
                                  COMPARING wi_id.

* Process the image work item id's in batches
  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch of work item ids
    CLEAR           lt_wi_id_p[].
    APPEND LINES OF lt_wi_id
               FROM lv_index_lo
                 TO lv_index_hi
                 TO lt_wi_id_p.

    IF     ( lt_wi_id_p[] IS INITIAL ).
      EXIT.
    ENDIF.

    CLEAR    lt_swihead[].
    CLEAR    lt_wi2obj[].
    CLEAR    lt_wicont[].

*eject
* Select the work items
    SELECT   *
      INTO   TABLE lt_swihead
      FROM   swwwihead FOR ALL ENTRIES IN lt_wi_id_p
     WHERE   wi_id = lt_wi_id_p-wi_id.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_swihead[].
    ENDIF.

    IF     ( lt_swihead[] IS INITIAL ).
      CONTINUE.
    ENDIF.

    SORT     lt_swihead        ASCENDING BY wi_id.
    DELETE   ADJACENT DUPLICATES       FROM lt_swihead
                                  COMPARING wi_id.

* Select the work item to object entries
    SELECT   *
      INTO   TABLE lt_wi2obj
      FROM   sww_wi2obj FOR ALL ENTRIES IN lt_wi_id_p
     WHERE   wi_id = lt_wi_id_p-wi_id.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_wi2obj[].
    ENDIF.

    DELETE   lt_wi2obj                WHERE typeid <> lc_typeid_image.
    DELETE   lt_wi2obj                WHERE removed > space. """K929505

    IF     ( lt_wi2obj[] IS INITIAL ).
      CONTINUE.
    ENDIF.

    SORT     lt_wi2obj         ASCENDING BY wi_id.
    DELETE   ADJACENT DUPLICATES       FROM lt_wi2obj
                                  COMPARING wi_id.

* Select the work item container values
    SELECT   *
      INTO   TABLE lt_wicont
      FROM   sww_cont FOR ALL ENTRIES IN lt_wi2obj
     WHERE   wi_id = lt_wi2obj-wi_id
       AND   element = lc_elemnt_doctyp.
    IF     ( sy-subrc NE 0 ).
      CLEAR    lt_wicont[].
    ENDIF.

    IF     ( lt_wicont[] IS INITIAL ).
      RETURN.
    ENDIF.

    SORT     lt_wicont         ASCENDING BY wi_id.
    DELETE   ADJACENT DUPLICATES       FROM lt_wicont
                                  COMPARING wi_id.

*eject
* Generate the results
    LOOP AT  lt_wicont            ASSIGNING <fs_wicont>.

      IF     ( <fs_wicont>-value         IN s_doctyp ).
      ELSE.
        CONTINUE.
      ENDIF.

      READ     TABLE lt_wi2obj    ASSIGNING <fs_wi2obj>
                                   WITH KEY wi_id = <fs_wicont>-wi_id
                              BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CONTINUE.
      ENDIF.

      READ     TABLE lt_swihead   ASSIGNING <fs_swihead>
                                   WITH KEY wi_id = <fs_wicont>-wi_id
                              BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CONTINUE.
      ENDIF.

      CLEAR                                 ls_rec.
      MOVE     p_erpid                   TO ls_rec-erpid.       "Out 01"
      MOVE     <fs_wi2obj>-instid+02(40) TO ls_rec-invc_ulid.   "Out 02"
      MOVE     <fs_swihead>-wi_text      TO ls_rec-title_mwfi.

      PERFORM  f_remove_special_chars
                                   CHANGING ls_rec-title_mwfi.  "Out 04"

      MOVE     <fs_swihead>-wi_cruser    TO ls_rec-crnt_agent.  "Out 06"
      MOVE     <fs_swihead>-wi_stat      TO ls_rec-wf_status.   "Out 07"

      PERFORM  f_format_date_and_time USING <fs_swihead>-wi_cd
                                            <fs_swihead>-wi_ct
                                   CHANGING ls_rec-image_sdt    "Out 08"
                                            ls_rec-image_stm.   "Out 09"

      MOVE     <fs_swihead>-wi_text      TO ls_rec-title_image.

      PERFORM  f_remove_special_chars
                                   CHANGING ls_rec-title_image. "Out 53"

*eject
      MOVE     <fs_swihead>-wi_stat      TO ls_rec-image_status."Out 54"
      MOVE     <fs_swihead>-wi_prio      TO ls_rec-image_priority. " 55"
      MOVE     <fs_swihead>-wi_cruser    TO ls_rec-image_agent. "Out 56"
      MOVE     <fs_swihead>-note_exist   TO ls_rec-image_atchm. "Out 57"
      MOVE     <fs_swihead>-wi_confirm   TO ls_rec-image_conf.  "Out 58"
      MOVE     <fs_swihead>-wi_id        TO ls_rec-image_wi_id. "Out 59"
      MOVE     <fs_swihead>-wi_rhtext    TO ls_rec-image_task_text." 60"
      MOVE     <fs_swihead>-wi_type      TO ls_rec-image_wi_type.  " 64"
      MOVE     <fs_swihead>-wi_rh_task   TO ls_rec-image_task.  "Out 65"
      MOVE     <fs_wi2obj>-instid+02(40) TO ls_rec-image_guid.  "Out 74"

* Check if the document is a duplicate                      "D30K929554
      CLEAR                                 lv_instid.      "D30K929554
      MOVE     <fs_wi2obj>-instid+02(40) TO lv_instid.      "D30K929554
      CLEAR                                 lv_subrc.       "D30K929554
      PERFORM  f_check_duplicate_doc  USING lv_instid       "D30K929554
                                   CHANGING lv_subrc.       "D30K929554
      IF       ( lv_subrc                IS INITIAL ).      "D30K929554
        PERFORM  f_output_rec         USING ls_rec.         "D30K929554
      ENDIF.                                                "D30K929554

    ENDLOOP.

  ENDDO.

ENDFORM.                    " f_extract_unregistered_images
*eject
*&---------------------------------------------------------------------*
*&      Form f_extract_via_workflow
*&---------------------------------------------------------------------*
*       Extract items via the workflow
*----------------------------------------------------------------------*
FORM f_extract_via_workflow.

  CONSTANTS:
           lc_bkpf                     TYPE sibftypeid VALUE 'BKPF',
           lc_fipp                     TYPE sibftypeid VALUE 'FIPP',
           lc_2081                     TYPE sibftypeid VALUE 'BUS2081',
           lc_rmrp                     TYPE awtyp      VALUE 'RMRP'.

  DATA:    ls_invc_data                TYPE gty_invc_data,
           ls_object                   TYPE gty_object.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_awtyp                    TYPE awtyp,
           lv_instid                   TYPE sibfboriid.

  FIELD-SYMBOLS: <fs_wi2obj_key>       TYPE gty_wi2obj_key.

  IF     ( cb_viawf IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     gt_object           ASCENDING BY object_id.
  DELETE   ADJACENT DUPLICATES         FROM gt_object
                                  COMPARING object_id.

* Select the top work item ids within the creation date range
  SELECT   top_wi_id
    INTO   TABLE gt_top_wi_id
    FROM   swwwihead
   WHERE   crea_tmp IN gr_crea_tmp.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_top_wi_id[].
  ENDIF.

  IF     ( gt_top_wi_id[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     gt_top_wi_id        ASCENDING BY top_wi_id.
  DELETE   ADJACENT DUPLICATES         FROM gt_top_wi_id
                                  COMPARING top_wi_id.

*eject
* Filter the top work items to specific workflow tasks
  SELECT   wi_id  wi_type  wi_rh_task
    INTO   TABLE gt_wi_top
    FROM   swwwihead FOR ALL ENTRIES IN gt_top_wi_id
   WHERE   wi_id = gt_top_wi_id-top_wi_id.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_wi_top[].
  ENDIF.

  CLEAR    gt_top_wi_id[].

  DELETE   gt_wi_top         WHERE ( ( wi_rh_task <> p_wfiimg ) AND
                                     ( wi_rh_task <> p_wfnpis ) AND
                                     ( wi_rh_task <> p_wfpois ) ).

  IF     ( gt_wi_top[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     gt_wi_top           ASCENDING BY wi_id.
  DELETE   ADJACENT DUPLICATES         FROM gt_wi_top
                                  COMPARING wi_id.

* Find the corresponding object id for each work item
  SELECT   wi_id  instid  typeid  removed                   "D30K929505
    INTO   TABLE gt_wi2obj_key
    FROM   sww_wi2obj FOR ALL ENTRIES IN gt_wi_top
   WHERE   wi_id = gt_wi_top-wi_id.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_wi2obj_key[].
  ENDIF.

  CLEAR    gt_wi_top[].

  DELETE   gt_wi2obj_key     WHERE ( ( typeid <> lc_bkpf ) AND
                                     ( typeid <> lc_fipp ) AND
                                     ( typeid <> lc_2081 ) ).

  DELETE   gt_wi2obj_key     WHERE   ( removed > space   ). "D30K929505

  IF     ( gt_wi2obj_key[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     gt_wi2obj_key       ASCENDING BY instid.
  DELETE   ADJACENT DUPLICATES         FROM gt_wi2obj_key
                                  COMPARING instid.

*eject
* Loop for each distinct object id
  LOOP AT  gt_wi2obj_key          ASSIGNING <fs_wi2obj_key>.

    CLEAR    lv_awtyp.
    CLEAR    lv_instid.

    IF   ( ( <fs_wi2obj_key>-typeid      EQ lc_bkpf ) OR
           ( <fs_wi2obj_key>-typeid      EQ lc_fipp )    ).
      MOVE   lc_bkpf                     TO lv_awtyp.
    ELSEIF ( <fs_wi2obj_key>-typeid      EQ lc_2081 ).
      MOVE   lc_rmrp                     TO lv_awtyp.
    ELSE.
      CONTINUE.
    ENDIF.

    MOVE     <fs_wi2obj_key>-instid      TO lv_instid.

* Get the corresponding invoice data
    CLEAR                                   ls_invc_data.
    PERFORM  f_get_invoice_data       USING lv_awtyp
                                            lv_instid
                                   CHANGING ls_invc_data.

    IF     ( ls_invc_data-awtyp          NE lv_awtyp ).     "D30K929505
      CONTINUE.                                             "D30K929505
    ENDIF.                                                  "D30K929505

* Process the invoice workflow
    PERFORM  f_process_invoice_workflow
                                      USING lv_awtyp
                                            lv_instid
                                            ls_invc_data.

  ENDLOOP.

ENDFORM.                    " f_extract_via_workflow
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_invoice_workflow
*&---------------------------------------------------------------------*
*       Process the invoice workflow and generate the output
*----------------------------------------------------------------------*
FORM f_process_invoice_workflow
  USING    iv_awtyp               TYPE awtyp
           iv_instid              TYPE sibfboriid
           is_invc_data           TYPE gty_invc_data.
**-- start of changes by akmadasu CHG0139790

  DATA :ls_rec_ref        TYPE REF TO data.
  FIELD-SYMBOLS:<lfs_rec> TYPE any.
**--end of changes by akmadasu CHG0139790
  DATA:    lt_wi2obj              TYPE gtt_wi2obj,
           ls_wi2obj              TYPE gty_wi2obj,
           lt_swihead             TYPE gtt_swwwihead,
           ls_swihead             TYPE gty_swwwihead,
           lt_cont                TYPE gtt_cont,
           ls_cont                TYPE gty_cont,
           lt_toa01               TYPE gtt_toa01,
           ls_toa01               TYPE gty_toa01,
           ls_rec                 TYPE gty_rec.

  DATA:    lv_subrc               TYPE sysubrc,             "D30K929554
           lv_instid              TYPE sibfboriid,          "D30K929554
           lv_object_id           TYPE saeobjid,
           lv_top_wi_id           TYPE sww_wiid,
           lv_fl_found_rco        TYPE c,
           lv_fl_found_post       TYPE c,
           lv_amount              TYPE char16.

  CONSTANTS:
           lc_bo                  TYPE sibfcatid  VALUE 'BO',
           lc_bkpf                TYPE sibftypeid VALUE 'BKPF',
           lc_fipp                TYPE sibftypeid VALUE 'FIPP',
           lc_2081                TYPE sibftypeid VALUE 'BUS2081',
           lc_rmrp                TYPE awtyp      VALUE 'RMRP',
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0140727
           lc_zcar                TYPE awtyp      VALUE 'ZCARS',
           lc_cars                TYPE char4      VALUE 'CARS',
*END OF CHANGES BY KBANERJEE FOR CHG0140727
           lc_decis               TYPE swc_elem   VALUE 'DECISIONTYPE',
           lc_rcode               TYPE swc_value  VALUE 'RCODE',
           lc_post                TYPE swc_value  VALUE 'POST',
           lc_wfbatch             TYPE sww_aagent VALUE 'WF-BATCH',
           lc_npo                 TYPE char3      VALUE 'NPO',
           lc_po                  TYPE char3      VALUE 'PO',
           lc_cad                 TYPE waers      VALUE 'CAD'.

  IF   ( ( iv_awtyp                      IS INITIAL ) OR
         ( iv_instid                     IS INITIAL ) OR
         ( is_invc_data-bukrs            IS INITIAL )    ).
    RETURN.
  ENDIF.

*eject
  CLEAR    lt_wi2obj[].
  CLEAR    ls_wi2obj.
  CLEAR    lt_swihead[].
  CLEAR    ls_swihead.
  CLEAR    lt_cont[].
  CLEAR    ls_cont.
  CLEAR    lt_toa01[].
  CLEAR    ls_toa01.
  CLEAR    ls_rec.

  MOVE          p_erpid                  TO ls_rec-erpid.       "Out 01"
  CONCATENATE   is_invc_data-belnr '_'
                is_invc_data-bukrs '_'
                is_invc_data-gjahr     INTO ls_rec-invc_ulid.   "Out 02"
  MOVE          is_invc_data-stage       TO ls_rec-invc_status. "Out 03"
  MOVE          is_invc_data-zterm       TO ls_rec-zterm.       "Out 10"

  WRITE         is_invc_data-disc_prcnt  TO ls_rec-disc_prcnt   "Out 11"
                                   DECIMALS 1.
  CONDENSE                                  ls_rec-disc_prcnt NO-GAPS.

  CLEAR                                     lv_amount.
  WRITE         is_invc_data-wskto       TO lv_amount
                                   CURRENCY lc_cad.
  TRANSLATE                                 lv_amount USING ', - '.
  CONDENSE                                  lv_amount NO-GAPS.
  IF          ( is_invc_data-wskto       LT 0 ).
    CONCATENATE '-' lv_amount          INTO lv_amount.
  ENDIF.
  MOVE          lv_amount                TO ls_rec-wskto.       "Out 12"

  PERFORM  f_payment_status           USING is_invc_data-stage
                                            is_invc_data-due_date
                                            is_invc_data-augdt
                                   CHANGING ls_rec-paym_status  "Out 13"
                                            ls_rec-days_paym_out. "  16"

  IF          ( iv_awtyp                 EQ lc_bkpf ).
    MOVE        lc_npo                   TO ls_rec-invc_type.   "Out 17"
  ELSEIF      ( iv_awtyp                 EQ lc_rmrp ).
    MOVE        lc_po                    TO ls_rec-invc_type.   "Out 17"
    MOVE        iv_instid+00(10)         TO ls_rec-mm_invc.     "Out 19"
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0140727
  ELSEIF     ( iv_awtyp                  EQ lc_zcar ).
    MOVE       lc_cars                   TO ls_rec-invc_type.   "Out17"
  ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0140727
  PERFORM  f_format_date              USING is_invc_data-due_date
                                 CHANGING ls_rec-paym_due_dt. "Out 18"

  MOVE          is_invc_data-bukrs       TO ls_rec-bukrs.       "Out 20"
  MOVE          is_invc_data-belnr       TO ls_rec-belnr.       "Out 21"
  MOVE          is_invc_data-gjahr       TO ls_rec-gjahr.       "Out 22"

*eject
  PERFORM  f_format_date              USING is_invc_data-bldat
                                 CHANGING ls_rec-bldat.       "Out 23"
  PERFORM  f_format_date              USING is_invc_data-budat
                                 CHANGING ls_rec-budat.       "Out 24"

  CLEAR                                     lv_amount.
  WRITE         is_invc_data-wrbtr       TO lv_amount
                                   CURRENCY is_invc_data-waers.
  TRANSLATE                                 lv_amount USING ', - '.
  CONDENSE                                  lv_amount NO-GAPS.
  IF          ( is_invc_data-wrbtr       LT 0 ).
    CONCATENATE '-' lv_amount          INTO lv_amount.
  ENDIF.
  MOVE          lv_amount                TO ls_rec-wrbtr.       "Out 25"

  MOVE          is_invc_data-waers       TO ls_rec-waers.       "Out 26"
  MOVE          is_invc_data-xref3       TO ls_rec-xref3.       "Out 27"

  PERFORM  f_format_date_and_time     USING is_invc_data-cpudt "K929505
                                            is_invc_data-cputm "K929505
                                   CHANGING ls_rec-entrd_dt """"K929505
                                            ls_rec-entrd_tm. """K929505

  MOVE          is_invc_data-usnam       TO ls_rec-entrd_by. """K929505

  MOVE          is_invc_data-lifnr       TO ls_rec-lifnr.       "Out 40"
  MOVE          is_invc_data-zuonr       TO ls_rec-zuonr.       "Out 41"
  MOVE          is_invc_data-monat       TO ls_rec-monat.       "Out 42"
  MOVE          is_invc_data-blart       TO ls_rec-blart.       "Out 43"

  CLEAR                                     lv_amount.
  WRITE         is_invc_data-dmbtr       TO lv_amount
                                   CURRENCY is_invc_data-hwaer.
  TRANSLATE                                 lv_amount USING ', - '.
  CONDENSE                                  lv_amount NO-GAPS.
  IF          ( is_invc_data-dmbtr       LT 0 ).
    CONCATENATE '-' lv_amount          INTO lv_amount.
  ENDIF.
  MOVE          lv_amount                TO ls_rec-dmbtr.       "Out 44"

  MOVE          is_invc_data-hwaer       TO ls_rec-hwaer.       "Out 45"
  MOVE          is_invc_data-sgtxt       TO ls_rec-sgtxt.

  PERFORM       f_remove_special_chars
                                 CHANGING ls_rec-sgtxt.       "Out 46"

  MOVE          is_invc_data-augbl       TO ls_rec-augbl.       "Out 47"

  PERFORM       f_format_date         USING is_invc_data-augdt
                                 CHANGING ls_rec-augdt.       "Out 48"

  MOVE          is_invc_data-zlsch       TO ls_rec-zlsch.       "Out 49"
  MOVE          is_invc_data-zlspr       TO ls_rec-zlspr.       "Out 50"
  MOVE          is_invc_data-xblnr       TO ls_rec-xblnr.

  PERFORM       f_remove_special_chars
                                 CHANGING ls_rec-xblnr.       "Out 51"

*eject
  MOVE          is_invc_data-mwskz       TO ls_rec-mwskz.       "Out 52"
  MOVE          lc_cad                   TO ls_rec-rprtg_curr.  "Out 71"

  CLEAR                                     lv_amount.
  WRITE         is_invc_data-rprtg_amt   TO lv_amount
                                   CURRENCY lc_cad.
  TRANSLATE                                 lv_amount USING ', - '.
  CONDENSE                                  lv_amount NO-GAPS.
  IF          ( is_invc_data-rprtg_amt   LT 0 ).
    CONCATENATE '-' lv_amount          INTO lv_amount.
  ENDIF.
  MOVE          lv_amount                TO ls_rec-rprtg_amt.   "Out 72"

  WRITE         is_invc_data-rprtg_fx    TO ls_rec-rprtg_fx     "Out 73"
                                   DECIMALS 5.

* Process the invoice workflow
  SELECT   *
    INTO   TABLE lt_wi2obj
    FROM   sww_wi2obj
   WHERE   catid   = lc_bo
     AND   typeid IN (lc_bkpf, lc_fipp, lc_2081)
     AND   instid  = iv_instid.
  IF     ( sy-subrc EQ 0 ).                                 "D30K929505
    DELETE lt_wi2obj         WHERE   ( removed > space   ). "D30K929505
  ELSE.                                                     "D30K929505
    CLEAR  lt_wi2obj[].
  ENDIF.

*eject
* Find the invoice image data p_wfiimg -- Task - TS30001128
  SORT     lt_wi2obj           ASCENDING BY crea_tmp.

  READ     TABLE lt_wi2obj             INTO ls_wi2obj
                                   WITH KEY wi_rh_task = p_wfiimg.
  IF     ( sy-subrc EQ 0 ).

    SELECT   SINGLE *
      INTO   ls_swihead
      FROM   swwwihead
     WHERE   wi_id = ls_wi2obj-wi_id.
    IF     ( sy-subrc EQ 0 ).

      PERFORM  f_format_date_and_time
                                      USING ls_swihead-wi_cd
                                            ls_swihead-wi_ct
                                 CHANGING ls_rec-image_sdt    "Out 08"
                                          ls_rec-image_stm.   "Out 09"

      MOVE   ls_rec-image_sdt            TO ls_rec-ap_sdt.      "Out 14"
      MOVE   ls_rec-image_stm            TO ls_rec-ap_stm.      "Out 62"

      MOVE   ls_swihead-wi_text          TO ls_rec-title_image.

      PERFORM  f_remove_special_chars
                                 CHANGING ls_rec-title_image. "Out 53"

      MOVE   ls_swihead-wi_stat          TO ls_rec-image_status."Out 54"
      MOVE   ls_swihead-wi_prio          TO ls_rec-image_priority. " 55"
      MOVE   ls_swihead-wi_cruser        TO ls_rec-image_agent. "Out 56"
      MOVE   ls_swihead-note_exist       TO ls_rec-image_atchm. "Out 57"
      MOVE   ls_swihead-wi_confirm       TO ls_rec-image_conf.  "Out 58"
      MOVE   ls_swihead-wi_id            TO ls_rec-image_wi_id. "Out 59"
      MOVE   ls_swihead-wi_rhtext        TO ls_rec-image_task_text." 60"
      MOVE   ls_swihead-wi_type          TO ls_rec-image_wi_type.  " 64"
      MOVE   ls_swihead-wi_rh_task       TO ls_rec-image_task.  "Out 65"

    ELSE.
      CLEAR  ls_swihead.
    ENDIF.

  ENDIF.

*eject
  CLEAR                                     lv_object_id.
  MOVE     iv_instid                     TO lv_object_id.

  SELECT   *
    INTO   TABLE lt_toa01
    FROM   toa01
   WHERE   object_id = lv_object_id.
  IF     ( sy-subrc EQ 0 ).

    SORT           lt_toa01    ASCENDING BY ar_date.
    READ     TABLE lt_toa01            INTO ls_toa01 INDEX 1.

    IF       ( ls_swihead-wi_id          IS INITIAL ).

      PERFORM  f_format_date          USING ls_toa01-ar_date
                                 CHANGING ls_rec-image_sdt.   "Out 08"

      MOVE     '00:00:00'                TO ls_rec-image_stm.   "Out 09"
      MOVE     ls_rec-image_sdt          TO ls_rec-ap_sdt.      "Out 14"
      MOVE     ls_rec-image_stm          TO ls_rec-ap_stm.      "Out 62"

    ENDIF.

    MOVE       ls_toa01-arc_doc_id       TO ls_rec-image_guid.  "Out 74"

  ENDIF.

*eject
* Find the last workflow that was created
  SORT     lt_wi2obj          DESCENDING BY crea_tmp.

  CLEAR                                     lv_top_wi_id.
  IF     ( iv_awtyp EQ lc_bkpf ). "Non-PO Invoice
    READ     TABLE lt_wi2obj           INTO ls_wi2obj
                                   WITH KEY wi_rh_task = p_wfnpis.
    IF   ( sy-subrc EQ 0 ).
      MOVE     ls_wi2obj-wi_id           TO lv_top_wi_id.
    ENDIF.
  ELSEIF ( iv_awtyp EQ lc_rmrp ). "PO Invoice
    READ     TABLE lt_wi2obj           INTO ls_wi2obj
                                   WITH KEY wi_rh_task = p_wfpois.
    IF   ( sy-subrc EQ 0 ).
      MOVE     ls_wi2obj-wi_id           TO lv_top_wi_id.
    ENDIF.
  ENDIF.

* Process the work items in the last workflow
  IF     ( lv_top_wi_id IS NOT INITIAL ).

    SELECT   *
      INTO   TABLE lt_swihead
      FROM   swwwihead
     WHERE   top_wi_id = lv_top_wi_id.
    IF     ( sy-subrc EQ 0 ).

      SORT   lt_swihead BY wi_id DESCENDING.

* Find the current responsible person - last agent assigned
      CLEAR                                 ls_swihead.
      LOOP AT  lt_swihead              INTO ls_swihead
                                      WHERE wi_cruser IS NOT INITIAL
                                        AND wi_cruser NE lc_wfbatch.
        MOVE   ls_swihead-wi_cruser      TO ls_rec-crnt_agent.  "Out 06"
        EXIT.
      ENDLOOP.

* Find the status of the last work item
      CLEAR                                 ls_swihead.
      READ     TABLE lt_swihead        INTO ls_swihead INDEX 1.
      IF     ( sy-subrc EQ 0 ).
        MOVE   ls_swihead-wi_stat        TO ls_rec-wf_status.   "Out 07"
      ENDIF.

*eject
* Evaluate the NPO invoice work items
      IF     ( iv_awtyp EQ lc_bkpf ).

*---> Start Task   -- p_wfnpis -- Task WS98300017
        CLEAR                               ls_swihead.
        READ     TABLE lt_swihead      INTO ls_swihead
                                   WITH KEY wi_rh_task = p_wfnpis.
        IF     ( sy-subrc EQ 0 ).
          MOVE     ls_swihead-wi_text    TO ls_rec-title_mwfi.  "Out 04"

*         PERFORM  f_format_date_and_time
*                                     USING ls_swihead-wi_cd
*                                           ls_swihead-wi_ct
*                                  CHANGING ls_rec-ap_sdt       "Out 14"
*                                           ls_rec-ap_stm.      "Out 62"

          PERFORM  f_format_date_and_time
                                      USING ls_swihead-wi_cd
                                            ls_swihead-wi_ct
                                 CHANGING ls_rec-prkd_date    "Out 28"
                                          ls_rec-prkd_time.   "Out 29"

          MOVE     ls_swihead-wi_cruser  TO ls_rec-prkd_by.     "Out 30"
          MOVE     ls_swihead-wi_text    TO ls_rec-title_ap_clerk. " 61"
          MOVE     ls_swihead-wi_id      TO ls_rec-wf_wi_id.       " 63"
          MOVE     ls_swihead-no_deadl   TO ls_rec-ap_ddln.     "Out 66"
          MOVE     ls_swihead-wi_forw_by TO ls_rec-ap_frwrd_by. "Out 67"
          MOVE     ls_swihead-wi_prio    TO ls_rec-ap_priority. "Out 68"
          MOVE     ls_swihead-wi_dh_stat TO ls_rec-ap_ddln_status. " 69"
          MOVE     ls_swihead-wi_text    TO ls_rec-ap_text.     "Out 70"
        ENDIF.

        IF     ( gv_fl_sys_us            IS NOT INITIAL ). "US System

*---> RCO Task     -- p_wfnpir -- Task WS98300052
          CLEAR                             ls_swihead.
          READ     TABLE lt_swihead    INTO ls_swihead
                                   WITH KEY wi_rh_task = p_wfnpir.
          IF     ( sy-subrc EQ 0 ).
            MOVE     ls_swihead-wi_text  TO ls_rec-title_rwfi.

            PERFORM  f_remove_special_chars
                                 CHANGING ls_rec-title_rwfi.  "Out 05"

            PERFORM  f_format_date    USING ls_swihead-wi_cd
                                 CHANGING ls_rec-ap_cdt.      "Out 15"

            PERFORM  f_format_date_and_time
                                      USING ls_swihead-wi_cd
                                            ls_swihead-wi_ct
                                 CHANGING ls_rec-rco_rdt      "Out 31"
                                          ls_rec-rco_rtm.     "Out 32"

          ENDIF.

*eject
*---> Approve Task -- p_wfnpia -- Task TS98300060
          CLEAR                             ls_swihead.
          READ     TABLE lt_swihead    INTO ls_swihead
                                   WITH KEY wi_rh_task = p_wfnpia.
          IF     ( sy-subrc EQ 0 ).

            PERFORM  f_format_date_and_time
                                      USING ls_swihead-wi_cd
                                            ls_swihead-wi_ct
                                 CHANGING ls_rec-rco_cdt      "Out 33"
                                          ls_rec-rco_ctm.     "Out 34"

            PERFORM  f_format_date_and_time
                                      USING ls_swihead-wi_cd
                                            ls_swihead-wi_ct
                                 CHANGING ls_rec-aprvl_sdt    "Out 35"
                                          ls_rec-aprvl_stm.   "Out 36"

          ENDIF.

*---> POST Task    -- p_wfnpip -- Task TS98300069
          CLEAR                             ls_swihead.
          READ     TABLE lt_swihead    INTO ls_swihead
                                   WITH KEY wi_rh_task = p_wfnpip.
          IF     ( sy-subrc EQ 0 ).

            IF     ( ls_swihead-wi_cd    IS NOT INITIAL ).  "D30K929505
              PERFORM  f_format_date_and_time
                                      USING ls_swihead-wi_cd
                                            ls_swihead-wi_ct
                                 CHANGING ls_rec-entrd_dt     "Out 37"
                                          ls_rec-entrd_tm.    "Out 38"
            ENDIF.                                          "D30K929505

            IF     ( ls_swihead-wi_cruser IS NOT INITIAL ). "D30K929505
              MOVE   ls_swihead-wi_cruser TO ls_rec-entrd_by.   "Out 39"
            ENDIF.                                          "D30K929505

          ENDIF.

*eject
        ELSEIF ( gv_fl_sys_ca            IS NOT INITIAL ). "Canadian Sys

          CLEAR                             lv_fl_found_rco.
          CLEAR                             lv_fl_found_post.

          CLEAR                             ls_swihead.
          LOOP AT lt_swihead           INTO ls_swihead
                                      WHERE wi_rh_task = p_wfnpip.

            CLEAR    lt_cont[].

            CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
              EXPORTING
                workitem_id      = ls_swihead-wi_id
              TABLES
                simple_container = lt_cont.

            CLEAR                                ls_cont.
            READ     TABLE lt_cont          INTO ls_cont
                                        WITH KEY element = lc_decis.
            IF     ( sy-subrc EQ 0 ).

              IF     ( ( lv_fl_found_rco      IS INITIAL  ) AND
                       ( ls_cont-value        EQ lc_rcode )     ).
                MOVE     abap_true            TO lv_fl_found_rco.

*---> RCO Task
                MOVE     ls_swihead-wi_text   TO ls_rec-title_rwfi.

                PERFORM  f_remove_special_chars
                                      CHANGING ls_rec-title_rwfi. "05"

                PERFORM  f_format_date     USING ls_swihead-wi_cd
                                      CHANGING ls_rec-ap_cdt.     "15"

                PERFORM  f_format_date_and_time
                                           USING ls_swihead-wi_cd
                                                 ls_swihead-wi_ct
                                      CHANGING ls_rec-rco_rdt     "31"
                                               ls_rec-rco_rtm.    "32"

              ELSEIF ( ( lv_fl_found_post     IS INITIAL ) AND
                       ( ls_cont-value        EQ lc_post )     ).
                MOVE     abap_true            TO lv_fl_found_post.

*eject
*---> Approve Task

                PERFORM  f_format_date_and_time
                                           USING ls_swihead-wi_cd
                                                 ls_swihead-wi_ct
                                      CHANGING ls_rec-rco_cdt     "33"
                                               ls_rec-rco_ctm.    "34"

                PERFORM  f_format_date_and_time
                                           USING ls_swihead-wi_cd
                                                 ls_swihead-wi_ct
                                      CHANGING ls_rec-aprvl_sdt   "35"
                                               ls_rec-aprvl_stm.  "36"

*---> POST Task

                IF ( ls_swihead-wi_cd    IS NOT INITIAL ).  "D30K929505
                  PERFORM  f_format_date_and_time
                                           USING ls_swihead-wi_cd
                                                 ls_swihead-wi_ct
                                      CHANGING ls_rec-entrd_dt    "37"
                                               ls_rec-entrd_tm.   "38"
                ENDIF.                                      "D30K929505

                IF ( ls_swihead-wi_cruser IS NOT INITIAL ). "D30K929505
                  MOVE   ls_swihead-wi_cruser TO ls_rec-entrd_by.   "39"
                ENDIF.                                      "D30K929505

              ELSEIF ( ( lv_fl_found_rco  IS NOT INITIAL ) AND
                       ( lv_fl_found_post IS NOT INITIAL )     ).
                EXIT. "Exit the loop
              ENDIF.

            ENDIF.

            CLEAR  ls_swihead.
          ENDLOOP.

          IF      ( ls_rec-entrd_by      IS INITIAL ).
            MOVE    is_invc_data-usnam   TO ls_rec-entrd_by.    "Out 39"
          ENDIF.

        ENDIF. " ERP/System check

*eject
* Evaluate the PO invoice work items
      ELSEIF ( iv_awtyp EQ lc_rmrp ).

*---> Start Task   -- p_wfpois -- Task WS98400012
        CLEAR                               ls_swihead.
        READ     TABLE lt_swihead      INTO ls_swihead
                                   WITH KEY wi_rh_task = p_wfpois.
        IF     ( sy-subrc EQ 0 ).
          MOVE     ls_swihead-wi_text    TO ls_rec-title_mwfi.  "Out 04"

*         PERFORM  f_format_date_and_time
*                                     USING ls_swihead-wi_cd
*                                           ls_swihead-wi_ct
*                                  CHANGING ls_rec-ap_sdt       "Out 14"
*                                           ls_rec-ap_stm.      "Out 62"

          PERFORM  f_format_date_and_time
                                      USING ls_swihead-wi_cd
                                            ls_swihead-wi_ct
                                 CHANGING ls_rec-prkd_date    "Out 28"
                                          ls_rec-prkd_time.   "Out 29"

          MOVE     ls_swihead-wi_cruser  TO ls_rec-prkd_by.     "Out 30"
          MOVE     ls_swihead-wi_text    TO ls_rec-title_ap_clerk. " 61"
          MOVE     ls_swihead-wi_id      TO ls_rec-wf_wi_id.       " 63"
          MOVE     ls_swihead-no_deadl   TO ls_rec-ap_ddln.     "Out 66"
          MOVE     ls_swihead-wi_forw_by TO ls_rec-ap_frwrd_by. "Out 67"
          MOVE     ls_swihead-wi_prio    TO ls_rec-ap_priority. "Out 68"
          MOVE     ls_swihead-wi_dh_stat TO ls_rec-ap_ddln_status. " 69"
          MOVE     ls_swihead-wi_text    TO ls_rec-ap_text.     "Out 70"
        ENDIF.

*---> Approve Task -- p_wfpoia -- Task TS98400039
        CLEAR                               ls_swihead.
        READ     TABLE lt_swihead      INTO ls_swihead
                                   WITH KEY wi_rh_task = p_wfpoia.
        IF     ( sy-subrc EQ 0 ).
          MOVE     ls_swihead-wi_text    TO ls_rec-title_rwfi.

          PERFORM  f_remove_special_chars
                                 CHANGING ls_rec-title_rwfi.  "Out 05"

          PERFORM  f_format_date      USING ls_swihead-wi_cd
                                 CHANGING ls_rec-ap_cdt.      "Out 15"

          PERFORM  f_format_date_and_time
                                      USING ls_swihead-wi_cd
                                            ls_swihead-wi_ct
                                 CHANGING ls_rec-aprvl_sdt    "Out 35"
                                          ls_rec-aprvl_stm.   "Out 36"

        ENDIF.

*eject
*---> POST Task    -- p_wfnpip -- Task TS98300070
        CLEAR                               ls_swihead.
        READ     TABLE lt_swihead      INTO ls_swihead
                                   WITH KEY wi_rh_task = p_wfpoip.
        IF     ( sy-subrc EQ 0 ).

          IF       ( ls_swihead-wi_cd    IS NOT INITIAL ).  "D30K929505
            PERFORM  f_format_date_and_time
                                      USING ls_swihead-wi_cd
                                            ls_swihead-wi_ct
                                 CHANGING ls_rec-entrd_dt     "Out 37"
                                          ls_rec-entrd_tm.    "Out 38"
          ENDIF.                                            "D30K929505

          IF     ( ls_swihead-wi_cruser  IS NOT INITIAL ).  "D30K929505
            MOVE   ls_swihead-wi_cruser  TO ls_rec-entrd_by.    "Out 39"
          ENDIF.                                            "D30K929505

        ENDIF.

      ENDIF. "invoice reference "BKPF" or "RMRP"
    ENDIF. "select from table swwwihead
  ENDIF. "top work item found

*eject
* Check if the document is a duplicate - invoice ulid       "D30K929554
  CLEAR                                     lv_instid.      "D30K929554
  MOVE     ls_rec-invc_ulid              TO lv_instid.      "D30K929554
  CLEAR                                     lv_subrc.       "D30K929554
  PERFORM  f_check_duplicate_doc      USING lv_instid       "D30K929554
                                   CHANGING lv_subrc.       "D30K929554
  IF     ( lv_subrc IS NOT INITIAL ).                       "D30K929554
    RETURN.                                                 "D30K929554
  ENDIF.                                                    "D30K929554

  IF       ( ls_rec-image_guid           IS NOT INITIAL ).  "D30K929531

    CLEAR                                   ls_rec-invc_ulid. ""K929531
    MOVE     ls_rec-image_guid           TO ls_rec-invc_ulid. ""K929531

    CLEAR                                   lv_instid.      "D30K929554
    MOVE     ls_rec-image_guid           TO lv_instid.      "D30K929554
    CLEAR                                   lv_subrc.       "D30K929554
    PERFORM  f_check_duplicate_doc    USING lv_instid       "D30K929554
                                   CHANGING lv_subrc.       "D30K929554
    IF     ( lv_subrc IS NOT INITIAL ).                     "D30K929554
      RETURN.                                               "D30K929554
    ENDIF.                                                  "D30K929554

  ENDIF.                                                    "D30K929531
**-- start of changes by akmadasu CHG0139790
  IF ls_rec-invc_status = 'P'.                              "D30K929708
*----->  LOGIC FOR Reworked Invoices
    CREATE DATA ls_rec_ref TYPE gty_rec.
    ASSIGN ls_rec_ref->* TO <lfs_rec>.
    IF <lfs_rec> IS ASSIGNED.
      <lfs_rec> = ls_rec.
      PERFORM f_process_rework_invoices USING <lfs_rec>.
      ls_rec   = <lfs_rec>.
    ENDIF.
  ENDIF.                                                    "D30K929708
**-- end of changes by akmadasu CHG0139790
  PERFORM  f_output_rec               USING ls_rec.

ENDFORM.                    " f_process_invoice_workflow
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_date_and_time
*&---------------------------------------------------------------------*
*       Format the date and time based on time zone
*----------------------------------------------------------------------*
FORM f_format_date_and_time
  USING    iv_date                     TYPE sydatum
           iv_time                     TYPE syuzeit
  CHANGING cv_date                     TYPE char10
           cv_time                     TYPE char8.

  DATA:    lv_date                     TYPE sydatum,
           lv_time                     TYPE syuzeit,
           lv_timestamp_d              TYPE tzntstmps,
           lv_timestamp_c              TYPE char15.

  CONSTANTS:
           lc_time_blank               TYPE syuzeit VALUE '      ',
           lc_time_null                TYPE syuzeit VALUE '000000',
           lc_date_null                TYPE char10  VALUE '0000-00-00',
           lc_null                     TYPE char4   VALUE 'NULL'.

  CLEAR    cv_date.
  CLEAR    cv_time.

  CLEAR    lv_date.
  CLEAR    lv_time.

  MOVE     iv_date                       TO lv_date.

  IF     ( iv_time                       EQ lc_time_blank ).
    MOVE   lc_time_null                  TO lv_time.
  ELSE.
    MOVE   iv_time                       TO lv_time.
  ENDIF.

  IF   ( ( lv_date IS     INITIAL ) AND ( lv_time IS INITIAL ) ).
    RETURN.
  ENDIF.

  IF   ( ( lv_date IS NOT INITIAL ) AND ( lv_time IS INITIAL ) ).
    CONCATENATE     lv_date+00(04) '-'
                    lv_date+04(02) '-'
                    lv_date+06(02)     INTO cv_date.
    RETURN.
  ENDIF.

*eject
  IF     ( p_tzone IS     INITIAL ).
    IF   ( lv_date IS NOT INITIAL ).
      CONCATENATE   lv_date+00(04) '-'
                    lv_date+04(02) '-'
                    lv_date+06(02)     INTO cv_date.
    ENDIF.
    IF   ( lv_time IS NOT INITIAL ).
      CONCATENATE   lv_time+00(02) ':'
                    lv_time+02(02) ':'
                    lv_time+04(02)     INTO cv_time.
    ENDIF.
    RETURN.
  ENDIF.

  CLEAR    lv_timestamp_d.
  CLEAR    lv_timestamp_c.

  IF     ( lv_date IS INITIAL ).
    CONVERT DATE sy-datum TIME lv_time
       INTO TIME STAMP lv_timestamp_d
            TIME ZONE  p_tzone.
    MOVE        lv_timestamp_d           TO lv_timestamp_c.
    CONCATENATE lv_timestamp_c+08(02) ':'
                lv_timestamp_c+10(02) ':'
                lv_timestamp_c+12(02)  INTO cv_time.
  ELSE.
    CONVERT DATE lv_date  TIME lv_time
       INTO TIME STAMP lv_timestamp_d
            TIME ZONE  p_tzone.
    MOVE        lv_timestamp_d           TO lv_timestamp_c.
    CONCATENATE lv_timestamp_c+00(04) '-'
                lv_timestamp_c+04(02) '-'
                lv_timestamp_c+06(02)  INTO cv_date.
    CONCATENATE lv_timestamp_c+08(02) ':'
                lv_timestamp_c+10(02) ':'
                lv_timestamp_c+12(02)  INTO cv_time.
  ENDIF.

  IF       ( cv_date                     CS lc_date_null ).
    REPLACE  lc_date_null                IN cv_date
                                       WITH lc_null.
  ENDIF.

ENDFORM.                    " f_format_date_and_time
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_date
*&---------------------------------------------------------------------*
*       Format date
*----------------------------------------------------------------------*
FORM f_format_date
  USING    iv_date_in                  TYPE sydatum
  CHANGING cv_date_out                 TYPE char10.

  CONSTANTS:
           lc_date_null                TYPE char10  VALUE '0000-00-00',
           lc_null                     TYPE char4   VALUE 'NULL'.

  CLEAR    cv_date_out.

  CONCATENATE                               iv_date_in+0(4) '-'
                                            iv_date_in+4(2) '-'
                                            iv_date_in+6(2)
                                       INTO cv_date_out.

  IF       ( cv_date_out                 CS lc_date_null ).
    REPLACE  lc_date_null                IN cv_date_out
                                       WITH lc_null.
  ENDIF.

ENDFORM.                    " f_format_date
*eject
*&---------------------------------------------------------------------*
*&      Form  f_payment_status
*&---------------------------------------------------------------------*
*       Paid On-Time or Paid Late
*----------------------------------------------------------------------*
FORM f_payment_status
  USING    iv_stage                    TYPE char1
           iv_due_date                 TYPE sydatum
           iv_augdt                    TYPE augdt
  CHANGING cv_paym_status              TYPE char12
           cv_days_paym_out            TYPE char5.

  DATA:    lv_days_paym_out            TYPE numc5.

  CLEAR      cv_paym_status.
  CLEAR      cv_days_paym_out.
  CLEAR      lv_days_paym_out.

  IF       ( iv_stage                    EQ 'C' ).

    IF     ( iv_augdt                    GT iv_due_date ).
      MOVE   text-071                    TO cv_paym_status.
      lv_days_paym_out = iv_augdt - iv_due_date.
      MOVE   lv_days_paym_out            TO cv_days_paym_out.
    ELSE.
      MOVE   text-072                    TO cv_paym_status.
      MOVE   '0'                         TO cv_days_paym_out.
    ENDIF.

  ELSE.

    IF     ( sy-datum                    GT iv_due_date ).
      MOVE   text-071                    TO cv_paym_status.
      lv_days_paym_out = sy-datum - iv_due_date.
      MOVE   lv_days_paym_out            TO cv_days_paym_out.
    ELSE.
      CLEAR                                 cv_paym_status.
      MOVE   '0'                         TO cv_days_paym_out.
    ENDIF.

  ENDIF.

ENDFORM.                    " f_payment_status
*eject
*&---------------------------------------------------------------------*
*&      Form  f_remove_special_chars
*&---------------------------------------------------------------------*
*       Remove the special characters from text
*----------------------------------------------------------------------*
FORM f_remove_special_chars
  CHANGING cv_text                     TYPE any.

  DATA:    lv_text                     TYPE text1000,
           lv_len                      TYPE syindex,
           lv_ptr                      TYPE syindex,
           lv_char                     TYPE char1.

  CONSTANTS:
           lc_sep1                     TYPE char2 VALUE '|~',
           lc_sep2                     TYPE char2 VALUE '~|',
           lc_sep3                     TYPE char2 VALUE '|-',
           lc_sep4                     TYPE char2 VALUE '-|'.

  IF     ( cv_text IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_text.
  MOVE     cv_text                       TO lv_text.

  REPLACE  ALL OCCURRENCES    OF lc_sep1 IN lv_text WITH lc_sep3.
  REPLACE  ALL OCCURRENCES    OF lc_sep2 IN lv_text WITH lc_sep4.

  lv_len = strlen( lv_text ).

  DO       lv_len TIMES.
    lv_ptr = sy-index - 1.

    CLEAR                                   lv_char.
    MOVE     lv_text+lv_ptr(1)           TO lv_char.

    IF     ( lv_char                     LT space ).
      MOVE   space                       TO lv_text+lv_ptr(1).
    ENDIF.

  ENDDO.

  CLEAR                                     cv_text.
  MOVE     lv_text                       TO cv_text.

ENDFORM.                    " f_remove_special_chars
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_duplicate_doc                         "D30K929554
*&---------------------------------------------------------------------*
*       Check for duplicate document
*----------------------------------------------------------------------*
FORM f_check_duplicate_doc                                  "D30K929554
  USING    iv_instid                   TYPE sibfboriid
  CHANGING cv_subrc                    TYPE sysubrc.

  DATA:    ls_object                   TYPE gty_object.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_object_id                TYPE char32.

  CLEAR    cv_subrc.

  IF     ( iv_instid IS INITIAL ).
    cv_subrc = 1.
    RETURN.
  ENDIF.

  CLEAR                                     lv_object_id.
  MOVE     iv_instid                     TO lv_object_id.

  READ     TABLE gt_object             INTO ls_object
                                   WITH KEY object_id = lv_object_id
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF     ( lv_subrc EQ 0 ).
    cv_subrc = 1.
    RETURN.
  ENDIF.

  CLEAR                                     ls_object.
  MOVE     iv_instid                     TO ls_object-object_id.

  INSERT   ls_object                   INTO gt_object INDEX lv_tabix.

ENDFORM.                    " f_check_duplicate_doc         "D30K929554
*eject
*&---------------------------------------------------------------------*
*&      Form  f_output_rec
*&---------------------------------------------------------------------*
*       Output the record
*----------------------------------------------------------------------*
FORM f_output_rec
  USING    is_rec                      TYPE gty_rec.

  DATA:    ls_output                   TYPE gty_output.

  DATA:    lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_rec>              TYPE gty_rec,
                 <fs_attr>             TYPE any.

* Format the output string
  ASSIGN   is_rec                        TO <fs_rec>.

  DESCRIBE FIELD <fs_rec> TYPE lv_dtype COMPONENTS lv_cn_comp.

  CLEAR    ls_output.

  DO lv_cn_comp TIMES.

    ASSIGN COMPONENT sy-index OF STRUCTURE <fs_rec> TO <fs_attr>.
    IF     ( sy-index EQ 1 ).
      ls_output = <fs_attr>.
    ELSE.
      CONCATENATE ls_output <fs_attr>
             INTO ls_output SEPARATED BY gc_delim.
    ENDIF.

  ENDDO.

  IF       ( rb_pres                     IS NOT INITIAL ).
    APPEND   ls_output                   TO gt_output.
    ADD      1                           TO gv_cn_recs_total.
    RETURN.
  ENDIF.

*eject
* Check if the maximum number of records per file is exceeded
  IF       ( gv_cn_recs_file             GE gv_cn_recs_max ).

* Close the file
    IF   ( ( p_maxrec                    IS NOT INITIAL ) AND
           ( gv_filename                 IS NOT INITIAL )     ).

      CLOSE    DATASET gv_filename.

      CLEAR    gv_filename.
      CLEAR    gv_cn_recs_file.

    ENDIF.

  ENDIF.

* Open the file
  IF       ( gv_filename                 IS INITIAL ).

    PERFORM  f_open_dataset.

  ENDIF.

  IF       ( gv_filename                 IS NOT INITIAL ).

    TRANSFER ls_output                   TO gv_filename.

    ADD      1                           TO gv_cn_recs_file.
    ADD      1                           TO gv_cn_recs_total.

  ENDIF.

ENDFORM.                    " f_output_rec
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_REWORK_INVOICES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LFS_REC>  text
*----------------------------------------------------------------------*
FORM f_process_rework_invoices CHANGING pi_rec TYPE gty_rec.
  TYPES : BEGIN OF lty_out,
           wf_wi_id  TYPE swfrtwiid,
          END OF lty_out,
          BEGIN OF lty_ihead,
           wi_id      TYPE sww_wiid,
           wi_text    TYPE sww_witext,
*BEGIN OF CHANGES BY KBANERJEE FOR DFCT0017064
           wi_rhtext  TYPE sww_rhtext,
*END OF CHANGES BY KBANERJEE FOR DFCT0017064

           top_wi_id  TYPE swfrtwiid,
          END OF   lty_ihead.
  DATA:lt_output   TYPE STANDARD TABLE OF lty_out   INITIAL SIZE 0,
       ls_output   TYPE lty_out,
       lt_ihead    TYPE STANDARD TABLE OF lty_ihead INITIAL SIZE 0,
       ls_ihead    TYPE lty_ihead,
       lt_swr_cont TYPE STANDARD TABLE OF swr_cont  INITIAL SIZE 0,
       ls_swr_cont TYPE swr_cont,
       lt_wis      TYPE STANDARD TABLE OF swwwihead INITIAL SIZE 0,
       ls_wis      TYPE swwwihead,
       lt_user     TYPE STANDARD TABLE OF swhactor  INITIAL SIZE 0,
       ls_user     TYPE swhactor,
       lt_iap_user TYPE STANDARD TABLE OF rgsb4     INITIAL SIZE 0,
       ls_iap_user TYPE rgsb4,
       lt_log      TYPE STANDARD TABLE OF swp_logtab INITIAL SIZE 0,
       ls_log      TYPE swp_logtab.
  DATA:lv_flag     TYPE flag,
       lv_wi_id    TYPE swwwihead-wi_id,
       lv_setid    TYPE setid,
       lv_recs     TYPE i,
       lv_lines    TYPE i.

  CONSTANTS: lc_rej    TYPE char50     VALUE 'Rejection Confirmation',
*BEGIN OF CHANGES BY KBANERJEE FOR DFCT0017064
             lc_cnfrej TYPE sww_rhtext VALUE 'Confirm Rejection',
*END OF CHANGES BY KBANERJEE FOR DFCT0017064
             lc_val    TYPE char20     VALUE 'REJECTED',
             lc_flag   TYPE char1      VALUE 'X',
             lc_p      TYPE char1      VALUE 'P',
             lc_owner  TYPE char30     VALUE 'RC_OWNER_DECISION'.
  IF pi_rec-invc_status NE lc_p.
    RETURN.
  ENDIF.
*step A - get the rejected workflow work item
  CLEAR:lv_flag,lv_wi_id,lt_ihead[].
*Get work item text for rejection at WF level
  SELECT wi_id     wi_text
*BEGIN OF CHANGES BY KBANERJEE FOR DFCT0017064
         "         top_wi_id
         wi_rhtext top_wi_id
*END OF CHANGES BY KBANERJEE FOR DFCT0017064
    FROM swwwihead
    INTO TABLE lt_ihead
    WHERE  wi_text   = lc_rej
      AND  top_wi_id = pi_rec-wf_wi_id.
  IF sy-subrc IS INITIAL.
    SORT lt_ihead BY top_wi_id.
*BEGIN OF CHANGES BY KBANERJEE FOR DFCT0017064
  ELSE.
*Get work item text for rejection at task level
    SELECT wi_id     wi_text
           wi_rhtext top_wi_id
      FROM swwwihead
      INTO TABLE lt_ihead
      WHERE wi_text   = lc_cnfrej
       AND  top_wi_id = pi_rec-wf_wi_id.
    IF sy-subrc IS INITIAL.
      SORT lt_ihead BY top_wi_id.
    ENDIF.
*END OF CHANGES BY KBANERJEE FOR DFCT0017064

  ENDIF.
  READ TABLE lt_ihead INTO ls_ihead
                      WITH KEY top_wi_id = pi_rec-wf_wi_id
                      BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_flag = lc_flag.
  ELSE.
    lv_wi_id = pi_rec-wf_wi_id.
*step B :recjected workflow step by step name or action code
    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id      = lv_wi_id
        language         = sy-langu
        user             = sy-uname
        buffered_access  = 'X'
      TABLES
        simple_container = lt_swr_cont.

    IF lt_swr_cont[] IS NOT INITIAL.
      READ TABLE lt_swr_cont INTO ls_swr_cont
                             WITH KEY element = lc_owner
                                      value   = lc_val.
      IF sy-subrc IS INITIAL.
        lv_flag = lc_flag.
      ELSE. "check for RCO rejection step in log
*BEGIN OF CHANGES BY KBANERJEE TR D30K929732
        CALL FUNCTION 'SWP_WORKFLOW_LOG_READ'
          EXPORTING
            top_level_wf            = lv_wi_id
          TABLES
            wf_log                  = lt_log
          EXCEPTIONS
            workflow_does_not_exist = 1
            OTHERS                  = 2.
        IF sy-subrc = 0.
* Implement suitable error handling here
          READ TABLE lt_log INTO ls_log
                            WITH KEY node_text = text-t01.
          IF sy-subrc IS INITIAL.
            lv_flag = lc_flag.
          ELSE."check for initiator rejection step in log
            READ TABLE lt_log INTO ls_log
                              WITH KEY node_text = text-t02.
            IF sy-subrc IS INITIAL.
              lv_flag = lc_flag.
            ENDIF."endif read for ap reject
          ENDIF."endif read for rco reject
        ENDIF."endif log read FM
*END OF CHANGES BY KBANERJEE TR D30K929732
      ENDIF."endif lt_swr_cont
    ENDIF."endif lt_swr_cont table initial
  ENDIF."endif scenario a checK
  CLEAR: lv_wi_id.
  IF lv_flag = lc_flag.
    CLEAR:lt_user[],ls_wis,lt_wis[].
*If either of A or B is satisfied get the work item possible agents
    lv_wi_id = pi_rec-wf_wi_id.
*BEGIN OF CHANGES BY KBANERJEE TR D30K929732
    REFRESH lt_log.
    CALL FUNCTION 'SWP_WORKFLOW_LOG_READ'
      EXPORTING
        top_level_wf            = lv_wi_id
      TABLES
        wf_log                  = lt_log
      EXCEPTIONS
        workflow_does_not_exist = 1
        OTHERS                  = 2.
    IF sy-subrc = 0.
* Implement suitable error handling here
      DESCRIBE TABLE lt_log LINES lv_recs.
      READ TABLE lt_log INTO ls_log INDEX lv_recs.
*END OF CHANGES BY KBANERJEE TR D30K929732
*Get the latest agent with whom work items lies
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'RH_USERS_OF_WI_READ'
          EXPORTING
            wi_id         = ls_log-wi_id" D30K929732
          TABLES
            user_tab      = lt_user
          EXCEPTIONS
            nothing_found = 1
            OTHERS        = 2.
*START OF CHANGES BY KBANERJEE TR D30K929748
        IF sy-subrc IS NOT INITIAL.
          CALL FUNCTION 'SWI_GET_RELATED_WORKITEMS'
            EXPORTING
              wi_id       = lv_wi_id
            TABLES
              related_wis = lt_wis.
          IF lt_wis IS NOT INITIAL.
            SORT lt_wis BY wi_id DESCENDING.
            READ TABLE lt_wis INTO ls_wis INDEX 1.
            IF sy-subrc IS INITIAL.
              CALL FUNCTION 'RH_USERS_OF_WI_READ'
                EXPORTING
                  wi_id         = ls_wis-wi_id
                TABLES
                  user_tab      = lt_user
                EXCEPTIONS
                  nothing_found = 1
                  OTHERS        = 2.
            ENDIF."endif read on lt_wis
          ENDIF."endif lt_wis is not initial
        ENDIF."endif sy-subrc for RH_USERS_OF_WI_READ
*END OF CHANGES BY KBANERJEE TR D30K929748
*Get values for IAP_TEAM users using set AP_TEAM
        CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
          EXPORTING
            shortname                = 'AP_TEAM'
          IMPORTING
            new_setid                = lv_setid
          EXCEPTIONS
            no_set_found             = 1
            no_set_picked_from_popup = 2
            wrong_class              = 3
            wrong_subclass           = 4
            table_field_not_found    = 5
            fields_dont_match        = 6
            set_is_empty             = 7
            formula_in_set           = 8
            set_is_dynamic           = 9
            OTHERS                   = 10.
        IF sy-subrc = 0.
*Get values from 'AP_TEAM' set id
          CALL FUNCTION 'G_SET_GET_ALL_VALUES'
            EXPORTING
              setnr         = lv_setid
            TABLES
              set_values    = lt_iap_user
            EXCEPTIONS
              set_not_found = 1
              OTHERS        = 2.
          IF sy-subrc = 0.
* Implement suitable error handling here
            SORT lt_iap_user BY from.
          ENDIF.
        ENDIF.
*Check if the user/users lies within the specified values in SET
        DESCRIBE TABLE lt_user LINES lv_lines.
        IF lv_lines > '1'.
          pi_rec-invc_status = 'R'.
        ELSE.
          LOOP AT lt_user INTO ls_user.
            READ TABLE lt_iap_user INTO ls_iap_user
                                   WITH KEY from = ls_user-objid
                                   BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              pi_rec-invc_status = 'R'.
            ENDIF.
            CLEAR:ls_user.
          ENDLOOP.
        ENDIF."endif lv_lines > '1'
      ENDIF."endif sy-subrc for SWI_GET_RELATED_WORKITEMS
    ENDIF."endif lt_wis IS NOT INITIAL
  ENDIF."endif lv_flag = 'X'
ENDFORM.                    " F_PROCESS_REWORK_INVOICES
