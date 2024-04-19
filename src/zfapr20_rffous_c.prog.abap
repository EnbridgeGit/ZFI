*&---------------------------------------------------------------------*
*& Report  ZFAPR20_RFFOUS_C
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapr20_rffous_c.
TABLES: reguh,
        bnk_batch_header,
        rfpdo.
DATA: gv_msg TYPE t100-text,
      gt_reguhm TYPE TABLE OF reguhm.
TYPES: BEGIN OF ty_msg,
       text TYPE char200,
       END OF ty_msg.

DATA: gt_msg TYPE TABLE OF ty_msg,
      gs_msg TYPE ty_msg.

DATA: BEGIN OF messtab OCCURS 10.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF messtab.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_laufd FOR bnk_batch_header-laufd_f OBLIGATORY DEFAULT sy-datum,
                s_laufi FOR bnk_batch_header-laufi_f.
PARAMETERS:     p_stap  TYPE rfpdo-fordstap OBLIGATORY DEFAULT 1,
                p_merge AS CHECKBOX DEFAULT 'X'.
SELECT-OPTIONS: s_rzawe FOR reguh-rzawe NO-EXTENSION NO INTERVALS DEFAULT 'C' OBLIGATORY,
                s_hbki FOR reguh-hbkid OBLIGATORY DEFAULT 'CIBC',
                s_hkti FOR reguh-hktid OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:   p_zdru TYPE rfpdo-fordzdru DEFAULT 'X' NO-DISPLAY,
              p_priz TYPE rfpdo-fordpriz OBLIGATORY,
              p_sofz TYPE fordsofz DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS:   p_prib TYPE rfpdo-fordprib,
              p_begl TYPE rfpdo-fordbegl,
              p_sofb TYPE fordsofb,
              P_NOSU TYPE FORDNOSU DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b3.

START-OF-SELECTION.

  LOOP AT  s_rzawe.
    IF s_rzawe-low <> 'C' AND
       s_rzawe-low <> 'P' AND
       s_rzawe-low <> 'Q' AND
       s_rzawe-low <> '8'.
      WRITE: / 'Payment Methods C, P, Q, or 8 are allowed...'.
      STOP.
    ENDIF.
  ENDLOOP.

  IF p_merge = 'X'.
    PERFORM triiger_sapfpaym_merge.
    WAIT UP TO 60 SECONDS.
  ENDIF.
  PERFORM get_data.
  IF gt_reguhm IS NOT INITIAL.
    IF s_rzawe-low = 'Q' OR   "JPMC In-House Check
       s_rzawe-low = '8' OR   "Toronto Dominion In-House Check
       s_rzawe-low = 'P'.     "Toronto Dominion In-House Check Post-Dated
      PERFORM trigger_zap_check.
    ELSE.
      PERFORM trigger_rffous_c.
    ENDIF.
  ELSE.
    SKIP 2.
    WRITE: / '*********************Following Checks already exist**********************'.
    LOOP AT gt_msg INTO gs_msg.
      WRITE: / gs_msg-text.
    ENDLOOP.
    WRITE: / '*********************End of Checks already exist**********************'.
    SKIP 4.
    WRITE: / 'No Data to process....'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Extract Payment Run Date and Run IDs
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_reguhm TYPE TABLE OF reguhm,
        ls_reguhm TYPE reguhm,
        ls_msg TYPE ty_msg,
        lt_payr TYPE TABLE OF payr,
        ls_payr TYPE payr.

  CLEAR: gt_reguhm,
         gt_msg.

  IF s_laufi IS INITIAL.
    SELECT * FROM reguhm INTO TABLE lt_reguhm
         WHERE laufd IN s_laufd.
  ELSE.
    SELECT * FROM reguhm INTO TABLE lt_reguhm
         WHERE laufd IN s_laufd
           AND laufi IN s_laufi .
  ENDIF.
  IF lt_reguhm IS NOT INITIAL.
    SELECT * FROM payr INTO TABLE lt_payr
           FOR ALL ENTRIES IN lt_reguhm
             WHERE zbukr = lt_reguhm-zbukr
               AND hbkid = lt_reguhm-hbkid
               AND hktid = lt_reguhm-hktid
               AND rzawe = lt_reguhm-rzawe
               AND laufd = lt_reguhm-laufd_m
               AND laufi = lt_reguhm-laufi_m
               AND vblnr = lt_reguhm-vblnr
               AND voidr = space.

  ENDIF.

  IF s_rzawe-low = 'Q' OR
     s_rzawe-low = '8'.
    CLEAR    lt_payr[].
  ENDIF.

*If Check is already print for a payment document, Do not trigger it again
  LOOP AT lt_reguhm INTO ls_reguhm.
    READ TABLE lt_payr INTO ls_payr WITH KEY zbukr = ls_reguhm-zbukr
                                             laufd = ls_reguhm-laufd_m
                                             laufi = ls_reguhm-laufi_m
                                             vblnr = ls_reguhm-vblnr.
    IF sy-subrc = 0.
      CONCATENATE 'Check number already exist for ' ls_reguhm-zbukr
                                                    ls_reguhm-laufd_m
                                                    ls_reguhm-laufi_m
                                                    ls_payr-checf
                                                    ls_payr-vblnr
                                               INTO ls_msg-text
                                               SEPARATED BY space.
      APPEND ls_msg TO gt_msg.
    ELSE.
      APPEND ls_reguhm TO gt_reguhm.
    ENDIF.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM gt_reguhm COMPARING laufd_m laufi_m zbukr hbkid hktid.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  TRIGGER_RFFOAVIS_FPAYM
*&---------------------------------------------------------------------*
*       Trigger Check Print program
*----------------------------------------------------------------------*
FORM trigger_rffous_c .

  DATA: ls_reguhm TYPE reguhm,
        listob TYPE TABLE OF abaplist,
        listas TYPE TABLE OF listzeile.
  DATA: xdate(10) TYPE c,
        gv_msg1 TYPE TABLE OF bdcmsgcoll,
        ls_msg1 TYPE bdcmsgcoll,
        lv_chck TYPE i.
  DATA: lt_abaplist TYPE TABLE OF abaplist,
        ls_abaplist TYPE abaplist,
        lt_list TYPE list_string_table,
        ls_list TYPE LINE OF list_string_table,
        ls_msg TYPE ty_msg.

  CLEAR: messtab.
  IF gt_reguhm[] IS NOT INITIAL.
    WRITE: /
'***** Following Batches are passed to the Check Print Program for processing *****'.
  ENDIF.
  LOOP AT gt_reguhm INTO ls_reguhm.
    CLEAR: lt_abaplist,
           lt_list.
    break sahmad.
    SUBMIT rffous_c EXPORTING LIST TO MEMORY
           AND RETURN
           WITH sel_zawe IN s_rzawe
           WITH zw_laufi = ls_reguhm-laufi_m
           WITH zw_laufd = ls_reguhm-laufd_m
           WITH zw_zbukr-low = ls_reguhm-zbukr
           WITH par_anzp = '0'
           WITH par_stap = p_stap
           WITH sel_hbki-low = ls_reguhm-hbkid " IN s_hbki
           WITH sel_hkti-low = ls_reguhm-hktid " IN s_hkti
           WITH par_zdru = p_zdru
           WITH par_priz = p_priz
           WITH par_begl = p_begl
           WITH par_prib = p_prib
           WITH par_sofb = p_sofb
           WITH par_sofz = p_sofz
           WITH PAR_NOSU = p_NOSU.


    WRITE: / ls_reguhm-laufd_m,
             ls_reguhm-laufi_m,
             ls_reguhm-zbukr,
             ls_reguhm-hbkid,
             ls_reguhm-hktid.

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
  WRITE: / '*********************Following Checks already exist**********************'.
  LOOP AT gt_msg INTO ls_msg.
    WRITE: / ls_msg-text.
  ENDLOOP.
  WRITE: / '*****COMPLETED*****'.

ENDFORM.                    " TRIGGER_RFFOUS_C
*&---------------------------------------------------------------------*
*&      Form  TRIIGER_SAPFPAYM_MERGE
*&---------------------------------------------------------------------*
* Merge program group together payments from several payment runs and
* transfer them together on payment media
* Configuration of BCM / Bank Accounting
*----------------------------------------------------------------------*
FORM triiger_sapfpaym_merge .

  DATA: lv_number           TYPE tbtcjob-jobcount,
        lv_name             TYPE tbtcjob-jobname VALUE 'SAPFPAYM_MERGE',
        lv_print_parameters TYPE pri_params.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_name
    IMPORTING
      jobcount         = lv_number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc = 0.
    lv_print_parameters-PRIMM = space.
    lv_print_parameters-PRSAP = space.
    lv_print_parameters-FOOTL = space.
    SUBMIT sapfpaym_merge
       WITH so_lfd IN s_laufd
       WITH so_lfi IN s_laufi
       WITH so_hbkid IN s_hbki
       WITH so_hktid IN s_hkti
       WITH so_rzawe IN s_rzawe
       TO SAP-SPOOL
       SPOOL PARAMETERS lv_print_parameters
       WITHOUT SPOOL DYNPRO
       VIA JOB lv_name NUMBER lv_number
       AND RETURN.
    IF sy-subrc = 0.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_number
          jobname              = lv_name
          strtimmed            = 'X'
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.
      IF sy-subrc <> 0.
        WRITE : / 'Unable to close job...'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " TRIIGER_SAPFPAYM_MERGE
*&---------------------------------------------------------------------*
*&      Form  TRIGGER_ZAP_CHECK
*&---------------------------------------------------------------------*
*In case of Payment method "Q", trigger program ZFAPR021_IN_HOUSE_CHECK
*----------------------------------------------------------------------*
FORM TRIGGER_ZAP_CHECK .

  CONSTANTS: c_frmnm_jpmc_ck2 TYPE sobj_name VALUE 'YF110_PRENUM_CK2',
             c_frmnm_td_ca    TYPE sobj_name VALUE 'ZFAPF021_TD_CA',
             c_frmnm_td_us    TYPE sobj_name VALUE 'ZFAPF021_TD_US',
             c_frmnm_8  TYPE sobj_name VALUE 'ZFAPF021_TD_CA',
             c_tstrn    TYPE c         VALUE ' '.
  DATA: ls_reguhm TYPE reguhm,
        listob TYPE TABLE OF abaplist,
        listas TYPE TABLE OF listzeile.
  DATA: xdate(10) TYPE c,
        gv_msg1 TYPE TABLE OF bdcmsgcoll,
        ls_msg1 TYPE bdcmsgcoll,
        lv_chck TYPE i.
  DATA: lt_abaplist TYPE TABLE OF abaplist,
        ls_abaplist TYPE abaplist,
        lt_list TYPE list_string_table,
        ls_list TYPE LINE OF list_string_table,
        ls_msg TYPE ty_msg.
  DATA: lv_bjpmt TYPE flag,
        lv_btd8c TYPE flag,
        lv_btd8u TYPE flag,
        lv_btdpc TYPE flag,
        lv_btdpu TYPE flag,
        lv_frmnm TYPE sobj_name.

  CLEAR: messtab.
  IF gt_reguhm[] IS NOT INITIAL.
    WRITE: /
'***** Following Batches are passed to the Check Print Program (ZAP_CHECK) for processing *****'.
  ENDIF.

  CLEAR                  ls_reguhm.
  LOOP AT gt_reguhm INTO ls_reguhm.

    CLEAR: lt_abaplist,
           lt_list.
    break sahmad.

    CLEAR  lv_bjpmt.
    CLEAR  lv_btd8c.
    CLEAR  lv_btd8u.
    CLEAR  lv_btdpc.
    CLEAR  lv_btdpu.
    CLEAR  lv_frmnm.

    IF     ( ( s_rzawe-low               EQ 'Q'   )     ).
      MOVE     abap_true                 TO lv_bjpmt.
      MOVE     c_frmnm_jpmc_ck2          TO lv_frmnm.
    ELSEIF ( ( s_rzawe-low               EQ '8'   ) AND
             ( ls_reguhm-waers           EQ 'CAD' )     ).
      MOVE     abap_true                 TO lv_btd8c.
      MOVE     c_frmnm_td_ca             TO lv_frmnm.
    ELSEIF ( ( s_rzawe-low               EQ '8'   ) AND
             ( ls_reguhm-waers           EQ 'USD' )     ).
      MOVE     abap_true                 TO lv_btd8u.
      MOVE     c_frmnm_td_us             TO lv_frmnm.
    ELSEIF ( ( s_rzawe-low               EQ 'P'   ) AND
             ( ls_reguhm-waers           EQ 'CAD' )     ).
      MOVE     abap_true                 TO lv_btdpc.
      MOVE     c_frmnm_td_ca             TO lv_frmnm.
    ELSEIF ( ( s_rzawe-low               EQ 'P'   ) AND
             ( ls_reguhm-waers           EQ 'USD' )     ).
      MOVE     abap_true                 TO lv_btdpu.
      MOVE     c_frmnm_td_us             TO lv_frmnm.
    ENDIF.

    SUBMIT ZFAPR021_IN_HOUSE_CHECK EXPORTING LIST TO MEMORY
           AND RETURN
           WITH p_laufi     = ls_reguhm-laufi
           WITH p_laufd     = ls_reguhm-laufd
           WITH s_zbukr-low = ls_reguhm-zbukr
           WITH p_hbkid     = ls_reguhm-hbkid
           WITH p_hktid     = ls_reguhm-hktid
           WITH rb_bjpmt    = lv_bjpmt
           WITH rb_btd8c    = lv_btd8c
           WITH rb_btd8u    = lv_btd8u
           WITH rb_btdpc    = lv_btdpc
           WITH rb_btdpu    = lv_btdpu
           WITH p_rzawe     = s_rzawe-low
           WITH p_frmnm     = lv_frmnm
           WITH p_prntr     = p_priz
           WITH p_tstrn     = c_tstrn.
*          WITH p_checf     =
    WRITE: / ls_reguhm-laufd_m,
             ls_reguhm-laufi_m,
             ls_reguhm-zbukr,
             ls_reguhm-hbkid,
             ls_reguhm-hktid.

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

    CLEAR  ls_reguhm.
  ENDLOOP.

  SKIP 2.
  IF sy-batch IS NOT INITIAL.
    WRITE: / 'For Detail message, Please check the Background Job Log...'.
  ENDIF.
  SKIP 2.
  WRITE: / '*********************Following Checks already exist**********************'.
  LOOP AT gt_msg INTO ls_msg.
    WRITE: / ls_msg-text.
  ENDLOOP.
  WRITE: / '*****COMPLETED*****'.

ENDFORM.                    " TRIGGER_ZAP_CHECK
