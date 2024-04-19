*&---------------------------------------------------------------------*
*& Report  ZFAPR22
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapr22.

TABLES: swwwihead,
        sww_cont.
TYPES: BEGIN OF ty_output,
        doctype  TYPE sww_cont-value,
        wi_id    TYPE swwwihead-wi_id,
        wi_cd    TYPE swwwihead-wi_cd,
        mmbelnr  TYPE rbkp-belnr,
        mmgjahr  TYPE rbkp-gjahr,
        fibukrs  TYPE rbkp-bukrs,
        fibelnr  TYPE bkpf-belnr,
        figjahr  TYPE bkpf-gjahr,
        zfbdt    TYPE bseg-zfbdt,
        bldat    TYPE bkpf-bldat,
        budat    TYPE bkpf-budat,
        days_dif TYPE i,
      END OF ty_output.
DATA: gt_output  TYPE TABLE OF ty_output,
      gt_output1 LIKE gt_output WITH HEADER LINE,
      gs_output  TYPE ty_output,
      gt_comp_tab TYPE TABLE OF rstrucinfo WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_wi_cd FOR swwwihead-wi_cd OBLIGATORY,
                s_dtype FOR sww_cont-element NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_invall RADIOBUTTON GROUP grp1 DEFAULT 'X',
            r_mminv1 RADIOBUTTON GROUP grp1,
            r_mminv2 RADIOBUTTON GROUP grp1,
            r_fiinv1 RADIOBUTTON GROUP grp1,
            r_fiinv2 RADIOBUTTON GROUP grp1,
            r_fiinv3 RADIOBUTTON GROUP grp1,
            r_fiinv4 RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: r_both   RADIOBUTTON GROUP grp2 DEFAULT 'X',
            r_archiv RADIOBUTTON GROUP grp2,
            r_sapdoc RADIOBUTTON GROUP grp2.
SELECTION-SCREEN END OF BLOCK b3.

START-OF-SELECTION.

  PERFORM misc_actions.
  PERFORM get_data.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .
  CONSTANTS: lc_ts30001128 TYPE swwwihead-wi_rh_task VALUE 'TS30001128',
             lc_filenet TYPE swwwihead-wi_creator VALUE 'FILENET',
             lc_cancelled TYPE swwwihead-wi_stat VALUE 'CANCELLED'.
  DATA:   lv_belnr TYPE bkpf-belnr,
          lv_gjahr TYPE bkpf-gjahr,
          lv_bukrs TYPE bkpf-bukrs,
          lt_head TYPE TABLE OF swwwihead,
          ls_head TYPE swwwihead,
          lt_cont TYPE TABLE OF sww_cont,
          ls_cont TYPE sww_cont,
          lt_bkpf TYPE TABLE OF bkpf,
          ls_bkpf TYPE bkpf,
          lt_rbkp TYPE TABLE OF rbkp,
          ls_rbkp TYPE rbkp,
          ls_scontainer TYPE swr_cont,
          lt_scontainer TYPE TABLE OF swr_cont,
          lt_objects TYPE TABLE OF swr_cont,
          ls_objects TYPE swr_cont.
*      lv_zficmnp, "AP Credit memo, Non PO
*      lv_zficmpo, "AP Credit memo, PO
*      lv_zfiinvnp , "AP Invoice, Non-PO
*      lv_zfiinvpo , "AP Invoice, PO
*      lv_zmminv , "MM Invoice
*      lv_zmminvcany. "MM Invoice support(Any)

  SELECT * FROM swwwihead INTO TABLE lt_head
    WHERE wi_cd IN s_wi_cd
      AND wi_creator = lc_filenet
      AND wi_rh_task = lc_ts30001128
      AND wi_stat <> lc_cancelled.
  IF lt_head IS INITIAL.
    WRITE : / 'No data to process'.
    STOP.
  ENDIF.
  SELECT * FROM sww_cont INTO TABLE lt_cont
    FOR ALL ENTRIES IN lt_head
    WHERE wi_id = lt_head-wi_id
      AND element = 'DOCUMENTTYPE'
      AND value IN s_dtype.
  CLEAR: gt_output.
  LOOP AT lt_head INTO ls_head.
    CLEAR: ls_cont,
           ls_scontainer,
           lt_scontainer,
           lt_objects,
           ls_objects,
           gs_output.
    READ TABLE lt_cont INTO ls_cont WITH KEY ls_head-wi_id.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id = ls_head-wi_id
*     LANGUAGE                       = SY-LANGU
*     USER                           = SY-UNAME
*     BUFFERED_ACCESS                = 'X'
*   IMPORTING
*     RETURN_CODE                    =
*     IFS_XML_CONTAINER              =
*     IFS_XML_CONTAINER_SCHEMA       =
    TABLES
      simple_container               = lt_scontainer
*     MESSAGE_LINES                  =
*     MESSAGE_STRUCT                 =
*     SUBCONTAINER_BOR_OBJECTS       =
      subcontainer_all_objects       = lt_objects.

    READ TABLE lt_scontainer INTO ls_scontainer WITH KEY
                         element = '_WI_COMP_EVENT_NAME'.
*Document assigned to SAP document
*    IF ls_scontainer-value = 'ASSIGNED'.
    READ TABLE lt_objects INTO ls_objects WITH KEY
                      element = 'OBJECTS'.
    "Only unassigned archive
    IF r_archiv is NOT INITIAL AND
       ls_objects-value is NOT INITIAL.
       CONTINUE.
    ENDIF.
    "Only assigned archive
    IF r_sapdoc is NOT INITIAL AND
       ls_objects-value is INITIAL.
       CONTINUE.
    ENDIF.
    IF ls_objects-value IS NOT INITIAL.
      IF ls_cont-value = 'ZFICMNP'  OR ls_cont-value = 'ZFICMPO' OR
         ls_cont-value = 'ZFIINVNP' OR ls_cont-value = 'ZFIINVPO'.
        lv_bukrs = ls_objects-value(4).
        lv_belnr = ls_objects-value+5(10).
        lv_gjahr = ls_objects-value+15(4).
*read FI document

      ELSEIF ls_cont-value = 'ZMMINV' OR ls_cont-value = 'ZMMINVCANY'.
        lv_belnr = ls_objects-value(10).
        lv_gjahr = ls_objects-value+11(4).
*read MM document

      ENDIF.
      APPEND gs_output TO gt_output.
    ELSE.
      APPEND gs_output TO gt_output.
    ENDIF.
*    ELSE.
*      APPEND gs_output TO gt_output.
*    ENDIF.

  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  MISC_ACTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM misc_actions .

  REFRESH s_dtype.
  CLEAR s_dtype.

*MM Invoice
  IF r_invall IS NOT INITIAL OR
     r_mminv1 IS NOT INITIAL.
    s_dtype-sign = 'I'.
    s_dtype-option = 'EQ'.
    s_dtype-low = 'ZMMINV'.
*s_dtype-HIGH = space.
    APPEND s_dtype.
  ENDIF.
*MM Invoice Support (ANY)
  IF r_invall IS NOT INITIAL OR
     r_mminv2 IS NOT INITIAL.
    s_dtype-sign = 'I'.
    s_dtype-option = 'EQ'.
    s_dtype-low = 'ZMMINVCANY'.
    APPEND s_dtype.
  ENDIF.
*AP INVOICE, NON-PO
  IF r_invall IS NOT INITIAL OR
     r_fiinv1 IS NOT INITIAL.
    s_dtype-sign = 'I'.
    s_dtype-option = 'EQ'.
    s_dtype-low = 'ZFIINVNP'.
    APPEND s_dtype.
  ENDIF.
*AP INVOICE, PO
  IF r_invall IS NOT INITIAL OR
     r_fiinv2 IS NOT INITIAL.
    s_dtype-sign = 'I'.
    s_dtype-option = 'EQ'.
    s_dtype-low = 'ZFIINVPO'.
    APPEND s_dtype.
  ENDIF.
*AP CREDIT MEMO, NON-P0
  IF r_invall IS NOT INITIAL OR
     r_fiinv3 IS NOT INITIAL.
    s_dtype-sign = 'I'.
    s_dtype-option = 'EQ'.
    s_dtype-low = 'ZFICMNP'.
    APPEND s_dtype.
  ENDIF.
*AP CREDIT MEMO, PO
  IF r_invall IS NOT INITIAL OR
     r_fiinv4 IS NOT INITIAL.
    s_dtype-sign = 'I'.
    s_dtype-option = 'EQ'.
    s_dtype-low = 'ZFICMPO'.
    APPEND s_dtype.
  ENDIF.

************Get Components
  REFRESH gt_comp_tab.
  CALL FUNCTION 'GET_COMPONENT_LIST'
    EXPORTING
      program    = sy-repid
      fieldname  = 'GT_OUTPUT1'
    TABLES
      components = gt_comp_tab[].

ENDFORM.                    " MISC_ACTIONS
