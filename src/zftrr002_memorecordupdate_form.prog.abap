*&---------------------------------------------------------------------*
*&  Include           ZFTRP002_MEMORECORDUPDATE_FORM
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Form  DISPLAY_FINAL_FORMAT
*&---------------------------------------------------------------------*
*      Display the formatted file in alv.
*----------------------------------------------------------------------*
FORM display_final_format .

*ALV Layout for display
  PERFORM alv_fieldcat_display.

*ALV display for output
  PERFORM alv_output.

ENDFORM.                    " DISPLAY_FINAL_FORMAT

*&---------------------------------------------------------------------*
*&      Form  ALV_OUTPUT
*&---------------------------------------------------------------------*
*      Display ALV
*----------------------------------------------------------------------*
FORM alv_output .

*alv layout
  gwa_layout-colwidth_optimize = 'X'.
  gwa_layout-zebra = 'X'.

  gwa_alv_print-no_coverpage = 'X'.
  gwa_alv_print-no_print_selinfos = 'X'.
  gwa_alv_print-no_print_listinfos = 'X'.
  gwa_alv_print-no_new_page = 'X'.

*Assigning report Name
  gv_rep = sy-repid .

  CLEAR gwa_extab.
  gwa_extab-fcode = '&VEXCEL'.
  APPEND  gwa_extab TO git_extab.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gv_rep
      is_layout          = gwa_layout
*     i_structure_name   = gc_fieldcat_name                    " gc_fieldcat_name = 'FDES'
      it_fieldcat        = git_fieldcat_display
      it_excluding       = git_extab
      is_print           = gwa_alv_print
    TABLES
      t_outtab           = git_final_output
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " ALV_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_fieldcat_display .

  REFRESH : git_fieldcat_display.

  PERFORM populate_field_catalog USING:
         'BUKRS'      '1'  '1'  'Company Code'(013)     'GIT_FINAL_OUTPUT'(004)  'C200' '',
         'BNKKO'      '1'  '2'  'G/L account'(005)      'GIT_FINAL_OUTPUT'(004)  'C100' '',
         'DATUM'      '1'  '3'  'Planning Date'(006)    'GIT_FINAL_OUTPUT'(004)  'C200'  '',
         'IDENR'      '1'  '4'  'ID number'(014)        'GIT_FINAL_OUTPUT'(004)  'C100' '',
         'AVDAT'      '1'  '5'  'Expiration Date'(007)  'GIT_FINAL_OUTPUT'(004)  'C200'  '',
         'DMSHB'      '1'  '6'  'Amount'(008)           'GIT_FINAL_OUTPUT'(004)  'C100'  ''.


ENDFORM.                    " ALV_FIELDCAT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  populate_field_catalog
*&---------------------------------------------------------------------*
* i_fieldname => Name of the field to be displayed
* i_colpos    => Position of the field where the field should be displayed
*                in the output
* i_seltextl  => Text for displayed field
*----------------------------------------------------------------------*
FORM populate_field_catalog  USING          i_fieldname
                                            i_rowpos
                                            i_colpos
                                            i_seltextl
                                            i_tabname
                                            i_emphasize
                                            i_fixed_col.


  gwa_fieldcat_display-fieldname   = i_fieldname.
  gwa_fieldcat_display-row_pos     = i_rowpos.
  gwa_fieldcat_display-col_pos     = i_colpos.
  gwa_fieldcat_display-seltext_l   = i_seltextl.
  gwa_fieldcat_display-tabname     = i_tabname.
  gwa_fieldcat_display-emphasize   = i_emphasize.
  gwa_fieldcat_display-fix_column  = i_fixed_col.

  APPEND  gwa_fieldcat_display TO git_fieldcat_display.
  CLEAR gwa_fieldcat_display .
ENDFORM.                    " populate_field_catalog
*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_data .

  SELECT *
         FROM fdes
         INTO TABLE git_fdes
         WHERE ebene IN s_ebene[] AND
               datum IN s_datum[].
  IF sy-subrc = 0.
    LOOP AT git_fdes INTO gwa_fdes.
      IF gwa_fdes-avdat < gwa_fdes-datum.

* Prepare table for ALV display
        gwa_final_output-bnkko  = gwa_fdes-bnkko.
        gwa_final_output-ebene 	=	gwa_fdes-ebene.
        gwa_final_output-datum 	=	gwa_fdes-datum.
        gwa_final_output-dmshb 	=	gwa_fdes-dmshb.
        gwa_final_output-wrshb 	=	gwa_fdes-wrshb.
        gwa_final_output-avdat 	=	gwa_fdes-avdat.
        gwa_final_output-gvalt  = gwa_fdes-gvalt.
        gwa_final_output-bukrs  = gwa_fdes-bukrs.
        gwa_final_output-idenr  = gwa_fdes-idenr.

        APPEND  gwa_final_output TO git_final_output.
        CLEAR : gwa_final_output.

* Change expiration date to planning date
        gwa_fdes-avdat   = gwa_fdes-datum.
        MOVE-CORRESPONDING gwa_fdes TO gwa_fdes_flag_table.
*       It is very IMP to set this flag.
*       As it tells us which records to update
        gwa_fdes_flag_table-flag_change = gc_x.
        APPEND gwa_fdes_flag_table TO git_fdes_flag_table.
        CLEAR : gwa_fdes_flag_table.

      ENDIF.
      CLEAR : gwa_fdes.
    ENDLOOP.
  ELSE.
    MESSAGE s000(zfi01) WITH text-003.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Delete records which were not changed.
  DELETE git_fdes_flag_table WHERE flag_change = ''.

  REFRESH : git_fdes.
  CLEAR   : gwa_fdes.

  LOOP AT git_fdes_flag_table INTO gwa_fdes_flag_table.
*   Move all the records details
    MOVE-CORRESPONDING gwa_fdes_flag_table TO gwa_fdes.

    APPEND  gwa_fdes TO git_fdes.
    CLEAR : gwa_fdes,
            gwa_fdes_flag_table.
  ENDLOOP.
ENDFORM.                    " FETCH_DATA
*&---------------------------------------------------------------------*
*&      Form  CLEAR
*&---------------------------------------------------------------------*
*      clear internal table and work area
*----------------------------------------------------------------------*
FORM clear .

  CLEAR :   gwa_fdes,
            gwa_final_output,
            gwa_fdes_flag_table,
            gwa_fieldcat_display,
            gv_ebene_validation,
            gwa_layout,
            gwa_alv_print,
            gwa_extab,
            gv_rep,
            subsystem,
            aktyp,
            gv_tabix,
            gc_error_text,
            gc_exp_date,
            gwa_fdesdist,
            gwa_t001,
            gwa_t037,
            gwa_t036,
            gwa_t039,
            gwa_bdcdata,
            gwa_bdc_msg_error.

  REFRESH : git_fdes,
            git_final_output,
            git_fdes_flag_table,
            git_extab,
            git_fieldcat_display,
            git_fdesdist,
            git_t001,
            git_t037,
            git_t036,
            git_t039,
            git_bdcdata,
            git_bdc_msg_error.

ENDFORM.                    " CLEAR

*&---------------------------------------------------------------------*
*&      Form  fill_bdc_data
*&---------------------------------------------------------------------*
*      format data for BDC
*----------------------------------------------------------------------*
FORM fill_bdc_data  USING    value(cv_program)
                             value(cv_dynpro)
                             value(cv_dynbegin)
                             value(cv_fnam)
                             value(cv_fval).
  CLEAR gwa_bdcdata .
  IF cv_dynbegin = 'X' .
    gwa_bdcdata-program   = cv_program .
    gwa_bdcdata-dynpro    = cv_dynpro .
    gwa_bdcdata-dynbegin  = cv_dynbegin .
    APPEND gwa_bdcdata TO git_bdcdata.
  ELSE.
    gwa_bdcdata-fnam = cv_fnam.
    gwa_bdcdata-fval = cv_fval.
    APPEND gwa_bdcdata TO git_bdcdata.
  ENDIF.

ENDFORM.                    " FILL_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_FF63_CHANGE
*&---------------------------------------------------------------------*
*  Update records using BDC for FF63
*----------------------------------------------------------------------*
FORM update_ff63_change .

  LOOP AT git_fdes INTO gwa_fdes.
*       SUBSYSTEM determines whether records are taken from FDESDIST( if subsystem has some value ) or
*       FDES ( if subsystem is empty / initial ).
    READ TABLE git_fdesdist INTO gwa_fdesdist
                            WITH KEY bukrs = gwa_fdes-bukrs
                                     idenr = gwa_fdes-idenr.
    IF sy-subrc = 0.
      CLEAR : subsystem.
      subsystem = gwa_fdesdist-herku.
      EXPORT subsystem TO MEMORY ID 'FF63-SUBSYSTEM'.
    ELSE.
      CLEAR : subsystem.
      EXPORT subsystem TO MEMORY ID 'FF63-SUBSYSTEM'.
    ENDIF.

* Passing value 'AZ' means changes can be carried out for cash management and forecast using FF63.
    aktyp = 'AZ'.
    EXPORT aktyp TO MEMORY ID 'FF63-AKTYP'.

* Calling tcode FF63 by passing the the records from fdes table using
* SET parameter ID 'BUK' and 'FFI'.
* IT'S VERY IMP TO PASS THE ABOVE SET PARAMETERS FOR THIS LOGIC TO WORK.SO, DON'T CHANGE......

    SET PARAMETER ID 'BUK' FIELD gwa_fdes-bukrs.
    SET PARAMETER ID 'FFI' FIELD gwa_fdes-idenr.

    READ TABLE git_t001 INTO gwa_t001
                        WITH KEY bukrs = gwa_fdes-bukrs.
    IF sy-subrc = 0.
      READ TABLE git_t037 INTO gwa_t037
                          WITH KEY dsart = gwa_fdes-dsart.
      IF sy-subrc = 0.
        READ TABLE git_t036 INTO gwa_t036
                            WITH KEY ebene = gwa_t037-ebene.
        IF sy-subrc = 0.
          READ TABLE git_t039 INTO gwa_t039
                              WITH KEY orign = gwa_t036-orign.
          IF sy-subrc = 0.
            IF gwa_t039-xtfst <> space.

              REFRESH : git_bdcdata,
                        git_bdc_msg_error.

              CLEAR   : gwa_bdcdata,
                        gwa_bdc_msg_error.

              PERFORM :    fill_bdc_data USING 'SAPMF40E' '0100' 'X'  ' '  ' ',
                           fill_bdc_data USING  ''  ''  ''   'BDC_CURSOR'  'FDES-BUKRS',
                           fill_bdc_data USING  ''  ''  ''   'FDES-BUKRS'  gwa_fdes-bukrs,
                           fill_bdc_data USING  ''  ''  ''   'FDES-DSART'  gwa_fdes-dsart,
                           fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE'  '/00'.                         " Enter.

              PERFORM :    fill_bdc_data USING 'SAPMF40E' '0101' 'X'  ' '  ' ',
                           fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE' '=AEN'.                        " Change

              CLEAR : gc_exp_date.
              IF gwa_fdes-avdat IS NOT INITIAL.
                WRITE gwa_fdes-avdat TO gc_exp_date(000010) MM/DD/YYYY.
              ENDIF.

              PERFORM :    fill_bdc_data USING 'SAPMF40E' '0101' 'X'  ' '  ' ',
                           fill_bdc_data USING  ''  ''  ''   'BDC_CURSOR'  'FDES-AVDAT',
                           fill_bdc_data USING  ''  ''  ''   'FDES-AVDAT'  gc_exp_date,
                           fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE'  '=UPD'.                         " Update.

* Calling bdc for FF63 and updating records
              PERFORM call_bdc_for_update.

            ELSE.
              REFRESH : git_bdcdata,
                        git_bdc_msg_error.

              CLEAR   : gwa_bdcdata,
                        gwa_bdc_msg_error.

              PERFORM :    fill_bdc_data USING 'SAPMF40E' '0100' 'X'  ' '  ' ',
                           fill_bdc_data USING  ''  ''  ''   'BDC_CURSOR'  'FDES-BUKRS',
                           fill_bdc_data USING  ''  ''  ''   'FDES-BUKRS'  gwa_fdes-bukrs,
                           fill_bdc_data USING  ''  ''  ''   'FDES-DSART'  gwa_fdes-dsart,
                           fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE'  '/00'.                         " Enter.

              PERFORM :    fill_bdc_data USING 'SAPMF40E' '0102' 'X'  ' '  ' ',
                           fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE' '=AEN'.                        " Change


              CLEAR : gc_exp_date.
              IF gwa_fdes-avdat IS NOT INITIAL.
                WRITE gwa_fdes-avdat TO gc_exp_date(000010) MM/DD/YYYY.
              ENDIF.

              PERFORM :    fill_bdc_data USING 'SAPMF40E' '0102' 'X'  ' '  ' ',
                           fill_bdc_data USING  ''  ''  ''   'BDC_CURSOR'  'FDES-AVDAT',
                           fill_bdc_data USING  ''  ''  ''   'FDES-AVDAT'   gc_exp_date,
                           fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE'  '=UPD'.                         " Update.

* Calling bdc for FF63 and updating records
              PERFORM call_bdc_for_update.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " UPDATE_FF63_CHANGE
*&---------------------------------------------------------------------*
*&      Form  FETCH_CHECK_TABLE_RECORDS
*&---------------------------------------------------------------------*
*   Fetch records from tables T001, T037, T036, T039.
*----------------------------------------------------------------------*
FORM fetch_check_table_records .

  REFRESH : git_fdesdist.
  CLEAR   : gwa_fdesdist.

  SELECT herku bukrs idenr
    FROM fdesdist
    INTO TABLE git_fdesdist
    FOR ALL ENTRIES IN git_fdes
    WHERE bukrs = git_fdes-bukrs AND
          idenr = git_fdes-idenr.

  SELECT bukrs
    FROM t001
    INTO TABLE git_t001
    FOR ALL ENTRIES IN git_fdes
    WHERE bukrs = git_fdes-bukrs.

  IF sy-subrc = 0.
    SELECT dsart ebene
       FROM t037
       INTO TABLE git_t037
       FOR ALL ENTRIES IN git_fdes
       WHERE dsart = git_fdes-dsart.

    IF sy-subrc = 0.
      SELECT ebene orign
         FROM t036
         INTO TABLE git_t036
         FOR ALL ENTRIES IN git_t037
         WHERE ebene = git_t037-ebene.

      IF sy-subrc = 0.
        SELECT orign xtfst
           FROM t039
           INTO TABLE git_t039
           FOR ALL ENTRIES IN git_t036
           WHERE orign = git_t036-orign.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " FETCH_CHECK_TABLE_RECORDS
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_FOR_UPDATE
*&---------------------------------------------------------------------*
*     Call bdc for update of records.
*----------------------------------------------------------------------*
FORM call_bdc_for_update .

  CALL TRANSACTION   gco_bdc_tcode
       USING         git_bdcdata
       MODE          gco_bdc_mode                               " Background mode
       UPDATE        gco_bdc_option                             " Synchronous update
       MESSAGES INTO git_bdc_msg_error.

  IF sy-subrc = 0.
    READ TABLE git_final_output INTO gwa_final_output
                                WITH KEY bukrs = gwa_fdes-bukrs
                                         idenr = gwa_fdes-idenr.
    IF sy-subrc = 0.
      CLEAR : gv_tabix.
      gv_tabix = sy-tabix.
      gwa_final_output-status = 'Update Successful'(012).
      MODIFY git_final_output FROM  gwa_final_output
                              INDEX gv_tabix
                              TRANSPORTING status.
    ENDIF.
  ELSE.
    READ TABLE git_final_output INTO gwa_final_output
                                WITH KEY bukrs = gwa_fdes-bukrs
                                         idenr = gwa_fdes-idenr.
    IF sy-subrc = 0.
      CLEAR : gv_tabix.
      gv_tabix = sy-tabix.

      LOOP AT git_bdc_msg_error INTO gwa_bdc_msg_error WHERE msgtyp EQ 'E'.
        CLEAR: gc_error_text.
*                  Format Message
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = gwa_bdc_msg_error-msgid
            msgnr               = gwa_bdc_msg_error-msgnr
            msgv1               = gwa_bdc_msg_error-msgv1
            msgv2               = gwa_bdc_msg_error-msgv2
            msgv3               = gwa_bdc_msg_error-msgv3
            msgv4               = gwa_bdc_msg_error-msgv4
          IMPORTING
            message_text_output = gc_error_text.
        CONCATENATE gwa_final_output-status gc_error_text INTO gwa_final_output-status SEPARATED BY space.
      ENDLOOP.
      MODIFY git_final_output FROM  gwa_final_output
                              INDEX gv_tabix
                              TRANSPORTING status.
    ENDIF.
  ENDIF.
ENDFORM.                    " CALL_BDC_FOR_UPDATE
