*&---------------------------------------------------------------------*
*&  Include           ZFIPS_MASTER_DATA_UPDATE_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFIPS_MASTER_DATA_UPDATE                      *
*& Author             :  KMB                                           *
*& Creation Date      :  19/11/2020                                    *
*& Object ID          :  CHG0203062                                    *
*& Application Area   :  FICO                                          *
*& Description        :  Optimization of PS master data update         *
*&-------------------------------------------------------------------- *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 19-03-2020   KMB         D30K930800  CHG020306  Initial              *
*                          D30K930890
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  F_FILE_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_file_upload .

  CLEAR : gt_record_sf[], gt_record_est[], gt_record_c55[].

  IF r_sf = gc_x.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = gc_x
        i_tab_raw_data       = gt_raw
        i_filename           = p_file
      TABLES
        i_tab_converted_data = gt_record_sf
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE text-000 TYPE gc_e.
    ENDIF..
  ELSEIF r_est = gc_x.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = gc_x
        i_tab_raw_data       = gt_raw
        i_filename           = p_file
      TABLES
        i_tab_converted_data = gt_record_est
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE text-000 TYPE gc_e.
    ENDIF..
  ELSEIF r_c55 = gc_x.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = gc_x
        i_tab_raw_data       = gt_raw
        i_filename           = p_file
      TABLES
        i_tab_converted_data = gt_record_c55
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE text-000 TYPE gc_e.
    ENDIF..
  ENDIF.

ENDFORM.                    " F_FILE_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  F_FILL_BDCDATA_SF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_bdcdata_sf .

  DATA: lv_status TYPE char4,
        lv_str_msg1(255) TYPE c.

  CLEAR: lv_str_msg1, lv_status.

  LOOP AT gt_record_sf INTO gs_record_sf.

*read dataset dataset into record.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    PERFORM f_check_status USING gs_record_sf-pspid CHANGING lv_status.

    IF lv_status <> 'TECO' AND lv_status <> 'CLSD'.
      CONCATENATE text-004 gs_record_sf-pspid text-005 INTO lv_str_msg1 SEPARATED BY space.
      WRITE lv_str_msg1 COLOR 6.
      CONTINUE.
    ENDIF.

    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0100'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  '*PROJ-PSPID'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=MDTB'.
    PERFORM f_bdc_field       USING '*PROJ-PSPID'
                                  gs_record_sf-pspid.
    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0998'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM f_bdc_field       USING 'PROJ-PSPID'
                                  gs_record_sf-pspid.
    PERFORM f_bdc_field       USING 'PROJ-POST1'
                                  gs_record_sf-post1.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'PROJ-PLSEZ'.
    IF gs_record_sf-plfaz IS NOT INITIAL.
      PERFORM f_bdc_field       USING 'PROJ-PLFAZ'
                                    gs_record_sf-plfaz.
    ENDIF.
    IF gs_record_sf-plsez IS NOT INITIAL.
      PERFORM f_bdc_field       USING 'PROJ-PLSEZ'
                                    gs_record_sf-plsez.
    ENDIF.
    IF gs_record_sf-eprog IS NOT INITIAL.
    PERFORM f_bdc_field       USING 'PROJ-EPROG'
                                    gs_record_sf-eprog.
    ENDIF.

    CLEAR gt_msgtab.
    gv_dismode = p_dismod.
*Call Transaction
    CALL TRANSACTION gc_cj07
                  USING gt_bdcdata
                  MODE  gv_dismode
                  MESSAGES INTO gt_msgtab.

    CALL FUNCTION 'ABAP4_COMMIT_WORK'.

    FREE : gt_bdcdata[], gv_dismode.

    CLEAR : gv_str_msg, gv_len,gs_msgtab.

*Error/Sucess Handling
    LOOP AT gt_msgtab INTO gs_msgtab
                        WHERE msgtyp = 'E'
                           OR msgtyp = 'S'.
*Selecting the Text from T100 Table (T100 stores all the messages while running any Transaction)
      SELECT SINGLE text
                      FROM t100
                      INTO gv_str_msg
                      WHERE sprsl EQ gs_msgtab-msgspra AND
                            arbgb EQ gs_msgtab-msgid AND
                            msgnr EQ gs_msgtab-msgnr.
*If entry found in T100 then the 4 fields MSGV1, MSGV2, MSGV3, MSGV4 are concatenated
      IF sy-subrc EQ 0.
        IF NOT gs_msgtab-msgv1 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv1 ).
          REPLACE '&1' WITH gs_msgtab-msgv1(gv_len) INTO gv_str_msg.
        ENDIF.
        IF NOT gs_msgtab-msgv2 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv2 ).
          REPLACE '&2' WITH gs_msgtab-msgv2(gv_len) INTO gv_str_msg.
        ENDIF.
        IF NOT gs_msgtab-msgv3 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv3 ).
          REPLACE '&3' WITH gs_msgtab-msgv3(gv_len) INTO gv_str_msg.
        ENDIF.
        IF NOT gs_msgtab-msgv4 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv4 ).
          REPLACE '&' WITH gs_msgtab-msgv4(gv_len) INTO gv_str_msg.
        ENDIF.
        TRANSLATE gv_str_msg USING '& '.
        CONDENSE gv_str_msg.
      ENDIF.
      CONCATENATE gv_str_msg 'for WBS' gs_record_sf-pspid INTO gv_str_msg SEPARATED BY space.
*Display with Colour
      IF gs_msgtab-msgtyp EQ 'E'.
        WRITE gv_str_msg COLOR 6.                                   "Red Colour
      ELSE.
        WRITE gv_str_msg COLOR 5.                                   "Green Colour
      ENDIF.
    ENDLOOP.

    CLEAR : gs_record_sf.

    CLEAR : gv_str_msg, gv_len,gs_msgtab.

  ENDLOOP.

ENDFORM.                    " F_FILL_BDCDATA_SF
*&---------------------------------------------------------------------*
*&      Form  F_FILL_BDCDATA_CUST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_bdcdata_est .

  DATA : lv_str_msg(255) TYPE  c,
         lv_str_msg1(255) TYPE  c,
         lv_status TYPE char4.

  CLEAR: lv_str_msg, lv_str_msg1, lv_status.
  LOOP AT gt_record_est INTO gs_record_est.

*read dataset dataset into record.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    PERFORM f_check_status USING gs_record_est-posid CHANGING lv_status.

    IF lv_status <> 'TECO' AND lv_status <> 'CLSD'.
      CONCATENATE text-004 gs_record_est-posid text-005 INTO lv_str_msg1 SEPARATED BY space.
      WRITE lv_str_msg1 COLOR 6.
      CONTINUE.
    ENDIF.

    CLEAR lv_str_msg.
    IF gs_record_est-usr08 IS INITIAL.
      CONCATENATE text-003 gs_record_est-posid INTO lv_str_msg SEPARATED BY space.
      WRITE lv_str_msg COLOR 6.
      CONTINUE.
    ENDIF.

    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0100'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  '*PRPS-POSID'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=LETB'.
    PERFORM f_bdc_field       USING '*PROJ-PSPID'
                                 gs_record_est-pspid.
    PERFORM f_bdc_field       USING '*PRPS-POSID'
                                 gs_record_est-posid.
    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0901'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=PICK'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'RCWBS-IDENT(01)'.
    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0999'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=USR1'.
*    PERFORM f_bdc_field       USING 'BDC_CURSOR'
*                                  'PRPS-POSID'.
*    PERFORM f_bdc_field       USING 'PRPS-POSID'
*                                 gs_record_est-posid.
    PERFORM f_bdc_field       USING 'PRPS-POST1'
                                 gs_record_est-post1.
    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0999'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '/EBCK'.
*    PERFORM f_bdc_field       USING 'PRPS-POSID'
*                                 gs_record_est-posid.
    PERFORM f_bdc_field       USING 'PRPS-POST1'
                                 gs_record_est-post1.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'PRPS-USR09'.
    IF gs_record_est-usr08 IS NOT INITIAL.
      PERFORM f_bdc_field       USING 'PRPS-USR08'
                                   gs_record_est-usr08.
    ENDIF.
    IF gs_record_est-usr09 IS NOT INITIAL AND gs_record_est-usr08 IS NOT INITIAL.
      PERFORM f_bdc_field       USING 'PRPS-USR09'
                                   gs_record_est-usr09.
    ENDIF.
    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0901'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'RCWBS-IDENT(01)'.

    CLEAR gt_msgtab.
    gv_dismode = p_dismod.
*Call Transaction
    CALL TRANSACTION gc_cj02
                  USING gt_bdcdata
                  MODE  gv_dismode
                  MESSAGES INTO gt_msgtab.

    CALL FUNCTION 'ABAP4_COMMIT_WORK'.

    FREE : gt_bdcdata, gv_dismode.

    CLEAR : gv_str_msg, gv_len,gs_msgtab.

*Error/Sucess Handling
    LOOP AT gt_msgtab INTO gs_msgtab
                        WHERE msgtyp = 'E'
                           OR msgtyp = 'S'.
*Selecting the Text from T100 Table (T100 stores all the messages while running any Transaction)
      SELECT SINGLE text
                      FROM t100
                      INTO gv_str_msg
                      WHERE sprsl EQ gs_msgtab-msgspra AND
                            arbgb EQ gs_msgtab-msgid AND
                            msgnr EQ gs_msgtab-msgnr.
*If entry found in T100 then the 4 fields MSGV1, MSGV2, MSGV3, MSGV4 are concatenated
      IF sy-subrc EQ 0.
        IF NOT gs_msgtab-msgv1 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv1 ).
          REPLACE '&1' WITH gs_msgtab-msgv1(gv_len) INTO gv_str_msg.
        ENDIF.
        IF NOT gs_msgtab-msgv2 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv2 ).
          REPLACE '&2' WITH gs_msgtab-msgv2(gv_len) INTO gv_str_msg.
        ENDIF.
        IF NOT gs_msgtab-msgv3 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv3 ).
          REPLACE '&3' WITH gs_msgtab-msgv3(gv_len) INTO gv_str_msg.
        ENDIF.
        IF NOT gs_msgtab-msgv4 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv4 ).
          REPLACE '&' WITH gs_msgtab-msgv4(gv_len) INTO gv_str_msg.
        ENDIF.
        TRANSLATE gv_str_msg USING '& '.
        CONDENSE gv_str_msg.
      ENDIF.
      CONCATENATE gv_str_msg 'for WBS' gs_record_est-posid INTO gv_str_msg SEPARATED BY space.

*Display with Colour
      IF gs_msgtab-msgtyp EQ 'E'.
        WRITE gv_str_msg COLOR 6.                                   "Red Colour
      ELSE.
        WRITE gv_str_msg COLOR 5.                                   "Green Colour
      ENDIF.
    ENDLOOP.

    CLEAR gs_record_est.

    CLEAR : gv_str_msg, gv_len,gs_msgtab.

  ENDLOOP.

ENDFORM.                    " F_FILL_BDCDATA_CUST
*&---------------------------------------------------------------------*
*&      Form  F_FILL_BDCDATA_C55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_bdcdata_c55 .
  LOOP AT gt_record_c55 INTO gs_record_c55.

*read dataset dataset into record.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0100'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  '*PRPS-POSID'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=LETB'.
    PERFORM f_bdc_field       USING '*PROJ-PSPID'
                                  gs_record_c55-pspid.
    PERFORM f_bdc_field       USING '*PRPS-POSID'
                                  gs_record_c55-posid.
    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0901'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=PICK'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'RCWBS-IDENT(01)'.
    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0999'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=USR1'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'PRPS-POST1'.
    PERFORM f_bdc_field       USING 'PRPS-POST1'
                                  gs_record_c55-post1.
    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0999'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '/EBCK'.
    PERFORM f_bdc_field       USING 'PRPS-POST1'
                                  gs_record_c55-post1.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'PRPS-USR00'.
    IF gs_record_c55-usr00 IS NOT INITIAL.
      PERFORM f_bdc_field       USING 'PRPS-USR00'
                                    gs_record_c55-usr00.
    ENDIF.
    PERFORM f_bdc_dynpro      USING 'SAPLCJWB' '0901'.
    PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                  'RCWBS-IDENT(01)'.

    CLEAR gt_msgtab.
    gv_dismode = p_dismod.
*Call Transaction
    CALL TRANSACTION gc_cj02
                  USING gt_bdcdata
                  MODE  gv_dismode
                  MESSAGES INTO gt_msgtab.


    CALL FUNCTION 'ABAP4_COMMIT_WORK'.

    FREE : gt_bdcdata, gv_dismode.

    CLEAR : gv_str_msg, gv_len,gs_msgtab.

*Error/Sucess Handling
    LOOP AT gt_msgtab INTO gs_msgtab
                        WHERE msgtyp = 'E'
                           OR msgtyp = 'S'.
*Selecting the Text from T100 Table (T100 stores all the messages while running any Transaction)
      SELECT SINGLE text
                      FROM t100
                      INTO gv_str_msg
                      WHERE sprsl EQ gs_msgtab-msgspra AND
                            arbgb EQ gs_msgtab-msgid AND
                            msgnr EQ gs_msgtab-msgnr.
*If entry found in T100 then the 4 fields MSGV1, MSGV2, MSGV3, MSGV4 are concatenated
      IF sy-subrc EQ 0.
        IF NOT gs_msgtab-msgv1 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv1 ).
          REPLACE '&1' WITH gs_msgtab-msgv1(gv_len) INTO gv_str_msg.
        ENDIF.
        IF NOT gs_msgtab-msgv2 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv2 ).
          REPLACE '&2' WITH gs_msgtab-msgv2(gv_len) INTO gv_str_msg.
        ENDIF.
        IF NOT gs_msgtab-msgv3 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv3 ).
          REPLACE '&3' WITH gs_msgtab-msgv3(gv_len) INTO gv_str_msg.
        ENDIF.
        IF NOT gs_msgtab-msgv4 IS INITIAL.
          gv_len = strlen( gs_msgtab-msgv4 ).
          REPLACE '&' WITH gs_msgtab-msgv4(gv_len) INTO gv_str_msg.
        ENDIF.
        TRANSLATE gv_str_msg USING '& '.
        CONDENSE gv_str_msg.
      ENDIF.
      CONCATENATE gv_str_msg 'for WBS' gs_record_c55-posid INTO gv_str_msg SEPARATED BY space.
*Display with Colour
      IF gs_msgtab-msgtyp EQ 'E'.
        WRITE gv_str_msg COLOR 6.                                   "Red Colour
      ELSE.
        WRITE gv_str_msg COLOR 5.                                   "Green Colour
      ENDIF.
    ENDLOOP.

    CLEAR gs_record_c55.

    CLEAR : gv_str_msg, gv_len,gs_msgtab.

  ENDLOOP.


ENDFORM.                    " F_FILL_BDCDATA_C55
*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0058   text
*      -->P_0059   text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING fnam fval TYPE any.
  CLEAR: gs_bdcdata.
  gs_bdcdata-fnam = fnam.
  gs_bdcdata-fval = fval.
  WRITE fval TO gs_bdcdata-fval LEFT-JUSTIFIED.
  APPEND gs_bdcdata TO gt_bdcdata.
ENDFORM.                    " F_BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0053   text
*      -->P_0054   text
*----------------------------------------------------------------------*
FORM f_bdc_dynpro  USING  program dynpro.
  CLEAR gs_bdcdata.
  gs_bdcdata-program = program.
  gs_bdcdata-dynpro = dynpro.
  gs_bdcdata-dynbegin = gc_x.
  APPEND gs_bdcdata TO gt_bdcdata.
ENDFORM.                    " F_BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_status USING gv_pspid TYPE ps_posid CHANGING gv_status TYPE char4.

  DATA: lt_wbs TYPE TABLE OF bapi_wbs_elements,
        lt_status TYPE TABLE OF bapi_wbs_system_status,
        ls_wbs TYPE bapi_wbs_elements,
        ls_status TYPE bapi_wbs_system_status.

  ls_wbs-wbs_element = gv_pspid.
  APPEND ls_wbs TO lt_wbs.

  CALL FUNCTION 'BAPI_BUS2054_GET_STATUS'
    TABLES
      i_wbs_elements  = lt_wbs
      e_system_status = lt_status.
  IF sy-subrc = 0.
    READ TABLE lt_status INTO ls_status INDEX 1.
    IF sy-subrc = 0.
      gv_status = ls_status-system_status.
    ENDIF.
  ENDIF.

  CLEAR: ls_status, ls_wbs, lt_wbs[], lt_status[].


ENDFORM.                    " F_CHECK_STATUS
