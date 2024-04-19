*&---------------------------------------------------------------------*
*& Report  ZCHANGE_PARKED_NPO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZCHANGE_PARKED_NPO  no standard page heading line-size 255.


include bdcrecx1.

TYPE-POOLS: truxs.

TYPES: BEGIN OF t_datatab,
      BUKRS TYPE BUKRS,
      BELNR TYPE BELNR_D,
      GJAHR TYPE CHAR4,
      ZLSCH type SCHZW_BSEG,
      BVTYP TYPE BVTYP,
      EMPFB TYPE EMPFB,
      END OF t_datatab.
DATA: w_app type c.
DATA: w_objkey LIKE swotobjid-objkey,
      w_wi LIKE SWR_STRUCT-WORKITEMID.

DATA: it_datatab type standard table of t_datatab,
      wa_datatab type t_datatab.
DATA: t_cont TYPE TABLE OF SWR_CONT WITH HEADER LINE.
DATA: T_worklist TYPE swrtwihdr,
      w_worklist TYPE swr_wihdr,
      w_return   TYPE sysubrc.

DATA: it_raw TYPE truxs_t_text_data.

PARAMETERS: p_file TYPE  rlgrap-filename.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.

start-of-selection.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      I_LINE_HEADER        = 'X'
      I_TAB_RAW_DATA       = IT_RAW
      I_FILENAME           = p_file
    TABLES
      I_TAB_CONVERTED_DATA = IT_DATATAB[]
    EXCEPTIONS
      CONVERSION_FAILED    = 1
      OTHERS               = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  perform open_group.


  LOOP AT IT_DATATAB INTO WA_DATATAB.
    clear: w_app, w_wi.
    refresh t_cont.

    CONCATENATE WA_DATATAB-bukrs WA_DATATAB-belnr WA_DATATAB-gjahr INTO w_objkey.
    select single wi from ZFIT_AP_WI_KEY into w_wi where objkey = w_objkey.
    IF sy-subrc ne 0.
      CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
        EXPORTING
          objtype     = 'FIPP'
          objkey      = w_objkey
        IMPORTING
          return_code = w_return
        TABLES
          worklist    = T_worklist.

      DELETE T_worklist WHERE wi_rh_task NE 'WS02000002'.
      read TABLE T_worklist INTO w_worklist with key wi_stat = 'STARTED'.
      SELECT SINGLE wi_id from swwwihead into w_wi
        WHERE wi_type = 'W' and
              wi_stat in ('READY', 'STARTED', 'SELECTED') and
              top_wi_id = w_worklist-wi_id.
    ENDIF.

    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        WORKITEM_ID      = w_wi
      TABLES
        SIMPLE_CONTAINER = t_cont.
    READ TABLE T_CONT WITH KEY ELEMENT = 'DECISIONTYPE'.
    IF T_CONT-VALUE = 'POST'.
      w_app = 'X'.
    ENDIF.

    perform bdc_dynpro      using 'SAPMF05V' '0100'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'RF05V-BUKRS'
                                  WA_DATATAB-bukrs.
    perform bdc_field       using 'RF05V-BELNR'
                                  WA_DATATAB-belnr.
    perform bdc_field       using 'RF05V-GJAHR'
                                  WA_DATATAB-gjahr.
    perform bdc_dynpro      using 'SAPMF05A' '1100'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=PAYM'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'INVFO-ACCNT'.
    perform bdc_dynpro      using 'SAPMF05A' '1100'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'INVFO-ZLSCH'
                                  WA_DATATAB-zlsch.
    IF NOT WA_DATATAB-empfb IS INITIAL.
    perform bdc_field       using 'INVFO-EMPFB'
                                  WA_DATATAB-empfb.
    ENDIF.
    perform bdc_field       using 'INVFO-BVTYP'
                                  WA_DATATAB-bvtyp.
    perform bdc_dynpro      using 'SAPMF05A' '1100'.
    IF w_app = 'X'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=PBBP'.
    ELSE.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=BP'.
    ENDIF.
    perform bdc_transaction using 'FBV2'.
    clear WA_DATATAB.
  ENDLOOP.

  perform close_group.
