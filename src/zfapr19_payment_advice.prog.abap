*&---------------------------------------------------------------------*
*& Report  ZFAPR19_PAYMENT_ADVICE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapr19_payment_advice LINE-SIZE 132.

TABLES: reguh,
        bnk_batch_header,
        rfpdo.
DATA: gt_bnk_batch TYPE TABLE OF bnk_batch_header,
      gt_reguh TYPE TABLE OF reguh,
      gv_msg TYPE t100-text.

DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
DATA: BEGIN OF messtab OCCURS 10.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF messtab.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_laufd TYPE bnk_batch_header-laufd_f OBLIGATORY,
            p_laufi TYPE bnk_batch_header-laufi_f,
            p_print TYPE  rfpdo-fordpria OBLIGATORY.
*            p_rzawe TYPE  reguh-rzawe OBLIGATORY.
SELECT-OPTIONS: s_rzawe FOR reguh-rzawe NO-EXTENSION,
                s_lifnr FOR reguh-lifnr NO-DISPLAY.
PARAMETERS: p_sofa TYPE rfpdo1-fordsofa.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

*  PERFORM get_data.
*  IF gt_bnk_batch IS NOT INITIAL.
*    PERFORM trigger_rffoavis_fpaym.
*  ELSE.
*    WRITE: / 'No Data to process....'.
*  ENDIF.
  PERFORM get_data_new.
  IF gt_reguh IS NOT INITIAL.
    PERFORM trigger_rffoavis_fpaym_new.
  ELSE.
    WRITE: / 'No Data to process....'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Extract Payment Run Date and Run IDs
*----------------------------------------------------------------------*
FORM get_data .

  DATA:  ls_bnk_batch TYPE bnk_batch_header,
         lt_bnk_batch TYPE TABLE OF bnk_batch_header,
         lt_reguh TYPE TABLE OF reguh.

  CLEAR: gt_bnk_batch.
  IF p_laufi IS INITIAL.
    SELECT * FROM bnk_batch_header INTO TABLE gt_bnk_batch
         WHERE laufd_f = p_laufd
           AND xvorl = space
           AND laufd_f <> space
           AND laufi_f <> space .
  ELSE.
    SELECT * FROM bnk_batch_header INTO TABLE gt_bnk_batch
         WHERE laufd_f = p_laufd
           AND xvorl = space
           AND laufd_f <> space
           AND laufi_f = p_laufi .
  ENDIF.
  IF gt_bnk_batch IS NOT INITIAL.
    SELECT * FROM reguh INTO TABLE lt_reguh
           FOR ALL ENTRIES IN gt_bnk_batch
           WHERE laufd = gt_bnk_batch-laufd_f
             AND laufi = gt_bnk_batch-laufi_f
             AND zbukr = gt_bnk_batch-zbukr
             AND xvorl = space
             AND rzawe IN s_rzawe. "= p_rzawe.
    LOOP AT gt_bnk_batch INTO ls_bnk_batch.
      READ TABLE lt_reguh WITH KEY laufd = ls_bnk_batch-laufd_f
                                   laufi = ls_bnk_batch-laufi_f
                                   zbukr = ls_bnk_batch-zbukr
                                   TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND ls_bnk_batch TO lt_bnk_batch.
      ENDIF.
    ENDLOOP.
    gt_bnk_batch[] = lt_bnk_batch[].
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  TRIGGER_RFFOAVIS_FPAYM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM trigger_rffoavis_fpaym .

  DATA: ls_bnk_batch TYPE bnk_batch_header,
        listob TYPE TABLE OF abaplist,
        listas TYPE TABLE OF listzeile.
  DATA: xdate(10) TYPE c,
        gv_msg1 TYPE TABLE OF bdcmsgcoll,
        ls_msg1 TYPE bdcmsgcoll,
        lv_chck TYPE i.
  DATA: lt_abaplist TYPE TABLE OF abaplist,
        ls_abaplist TYPE abaplist,
        lt_list TYPE list_string_table,
        ls_list TYPE LINE OF list_string_table.

  CLEAR: bdcdata,
         messtab.
  IF gt_bnk_batch[] IS NOT INITIAL.
    WRITE: /
'***** Following Batches are passed to the Payment Advice for processing *****'.
  ENDIF.
  LOOP AT gt_bnk_batch INTO ls_bnk_batch.
    CLEAR: lt_abaplist,
           lt_list.

    SUBMIT rffoavis_fpaym EXPORTING LIST TO MEMORY
           AND RETURN
           WITH par_pria = p_print
           WITH sel_zawe IN s_rzawe
           WITH zw_laufi = ls_bnk_batch-laufi_f
           WITH zw_laufd = ls_bnk_batch-laufd_f
           WITH par_sofa = p_sofa.

    WRITE: / ls_bnk_batch-laufd_f,
             ls_bnk_batch-laufi_f.

    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = lt_abaplist
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
    ENDIF.
    CALL FUNCTION 'LIST_TO_ASCI'
      IMPORTING
        list_string_ascii  = lt_list
      TABLES
        listobject         = lt_abaplist
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT lt_list INTO ls_list.
      WRITE: / ls_list.
    ENDLOOP.
    CALL FUNCTION 'LIST_FREE_MEMORY'
      TABLES
        listobject = lt_abaplist.
  ENDLOOP.
  SKIP 2.
  IF sy-batch IS NOT INITIAL.
    WRITE: / 'For Detail message, Please check the Background Job Log...'.
  ENDIF.
  SKIP 2.
  WRITE: / '*****COMPLETED*****'.

ENDFORM.                    " TRIGGER_RFFOAVIS_FPAYM
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_new .
  DATA:  lv_tabix TYPE sy-tabix,
         ls_bnk_batch TYPE bnk_batch_header,
         lt_bnk_batch TYPE TABLE OF bnk_batch_header,
         lt_reguh TYPE TABLE OF reguh,
         ls_reguh TYPE reguh.

  CLEAR: gt_bnk_batch,
         gt_reguh.
  IF p_laufi IS INITIAL.
    SELECT * FROM bnk_batch_header INTO TABLE gt_bnk_batch
         WHERE laufd_f = p_laufd
           AND xvorl = space
           AND laufd_f <> space
           AND laufi_f <> space .
  ELSE.
    SELECT * FROM bnk_batch_header INTO TABLE gt_bnk_batch
         WHERE laufd_f = p_laufd
           AND xvorl = space
           AND laufd_f <> space
           AND laufi_f = p_laufi .
  ENDIF.
  IF gt_bnk_batch IS NOT INITIAL.
    SELECT * FROM reguh INTO TABLE lt_reguh
           FOR ALL ENTRIES IN gt_bnk_batch
           WHERE laufd = gt_bnk_batch-laufd_f
             AND laufi = gt_bnk_batch-laufi_f
             AND zbukr = gt_bnk_batch-zbukr
             AND xvorl = space
             AND rzawe IN s_rzawe. "= p_rzawe.
    SORT lt_reguh BY laufd laufi zbukr lifnr.
    LOOP AT gt_bnk_batch INTO ls_bnk_batch.
      CLEAR lv_tabix.
      READ TABLE lt_reguh WITH KEY laufd = ls_bnk_batch-laufd_f
                                   laufi = ls_bnk_batch-laufi_f
                                   zbukr = ls_bnk_batch-zbukr
                                   TRANSPORTING NO FIELDS.
      lv_tabix = sy-tabix.
      LOOP AT lt_reguh INTO ls_reguh FROM lv_tabix..
        IF ls_bnk_batch-laufd_f <> ls_reguh-laufd OR
           ls_bnk_batch-laufi_f <> ls_reguh-laufi OR
           ls_bnk_batch-zbukr   <> ls_reguh-zbukr.
          EXIT.
        ENDIF.
        APPEND ls_reguh TO gt_reguh.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
  SORT gt_reguh BY laufd laufi zbukr lifnr.
  DELETE ADJACENT DUPLICATES FROM gt_reguh COMPARING laufd laufi zbukr
                                                     lifnr.
ENDFORM.                    " GET_DATA_NEW
*&---------------------------------------------------------------------*
*&      Form  TRIGGER_RFFOAVIS_FPAYM_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trigger_rffoavis_fpaym_new .
  DATA: ls_reguh TYPE reguh,
          listob TYPE TABLE OF abaplist,
          listas TYPE TABLE OF listzeile.
  DATA: xdate(10) TYPE c,
        gv_msg1 TYPE TABLE OF bdcmsgcoll,
        ls_msg1 TYPE bdcmsgcoll,
        lv_chck TYPE i.
  DATA: lt_abaplist TYPE TABLE OF abaplist,
        ls_abaplist TYPE abaplist,
        lt_list TYPE list_string_table,
        ls_list TYPE LINE OF list_string_table.

  CLEAR: bdcdata,
         messtab.
  IF gt_bnk_batch[] IS NOT INITIAL.
    WRITE: /
'***** Following Batches are passed to the Payment Advice for processing *****'.
  ENDIF.
  LOOP AT gt_reguh INTO ls_reguh.
    CLEAR: lt_abaplist,
           lt_list,
           s_lifnr.
    REFRESH: s_lifnr.
    "move vendor
    s_lifnr-SIGN = 'I'.
    s_lifnr-OPTION = 'EQ'.
    s_lifnr-LOW = ls_reguh-lifnr.
*    s_lifnr-High = .
    APPEND s_lifnr.
    SUBMIT rffoavis_fpaym EXPORTING LIST TO MEMORY
           AND RETURN
           WITH par_pria = p_print
           WITH sel_zawe IN s_rzawe
           WITH zw_laufi = ls_reguh-laufi
           WITH zw_laufd = ls_reguh-laufd
           WITH par_sofa = p_sofa
           WITH sel_lifn IN s_lifnr.

    WRITE: / ls_reguh-laufd,
             ls_reguh-laufi.

    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = lt_abaplist
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
    ENDIF.
    CALL FUNCTION 'LIST_TO_ASCI'
      IMPORTING
        list_string_ascii  = lt_list
      TABLES
        listobject         = lt_abaplist
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
    ENDIF.
    LOOP AT lt_list INTO ls_list.
      WRITE: / ls_list.
    ENDLOOP.
    CALL FUNCTION 'LIST_FREE_MEMORY'
      TABLES
        listobject = lt_abaplist.
  ENDLOOP.
  SKIP 2.
  IF sy-batch IS NOT INITIAL.
    WRITE: / 'For Detail message, Please check the Background Job Log...'.
  ENDIF.
  SKIP 2.
  WRITE: / '*****COMPLETED*****'.

ENDFORM.                    " TRIGGER_RFFOAVIS_FPAYM_NEW
