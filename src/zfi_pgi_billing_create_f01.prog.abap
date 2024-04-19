*&---------------------------------------------------------------------*
*&  Include           ZFI_PGI_BILLING_CREATE_F01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_PGI_BILLING_CREATE                        *
* Program Include    :   ZFI_PGI_BILLING_CREATE_F01                    *
* Author             :   AKMADASU                                      *
* Date               :   Jan 23, 2022                                  *
* Technical Contact  :   Ashok Madasu                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   To cretae PGI and Billing                     *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 23-Jan-2022  AKMADASU    D30K931986-Initial development              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  CLEAR:GT_LIPS[].
  SELECT * FROM LIPS INTO TABLE GT_LIPS
                     WHERE      VBELN IN S_VBELN.
  IF SY-SUBRC IS INITIAL.
    SORT GT_LIPS BY VBELN POSNR.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CRETAE_PGI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRETAE_PGI .
  DATA:lv_error1 TYPE XFELD,
       lv_error2 TYPE XFELD,
       lv_error3 TYPE XFELD,
       lv_error4 TYPE XFELD,
       lv_error5 TYPE XFELD,
       lv_error6 TYPE XFELD,
       lv_error7 TYPE XFELD,
       lv_vbeln  TYPE VBELN_VL,
       lv_message TYPE string,
       lv_error8 TYPE XFELD.
  CLEAR: GT_OUTPUT[].
  LOOP AT GT_LIPS INTO GS_LIPS.
    LV_VBELN = GS_LIPS-VBELN.
    AT NEW VBELN.
      CLEAR:GS_VBKOK,GT_VBKOK[],GS_VERKO, GT_VERKO[].
      GS_VBKOK-VBTYP_VL = 'J'.
      GS_VBKOK-VBELN_VL = GS_LIPS-VBELN.  "Delibery Number
      GS_VBKOK-WABUC    = 'X'.            "automatically PGI selection

      APPEND GS_VBKOK TO GT_VBKOK.

      GS_VERKO-OBJECT = '01'.
      GS_VERKO-OBJKEY = GS_LIPS-VBELN.
      APPEND: GS_VERKO TO GT_VERKO.
    ENDAT.

    GS_VBPOK-VBELN_VL = GS_LIPS-VBELN. "DELIVERY
    GS_VBPOK-POSNR_VL = GS_LIPS-POSNR. "DELIVERY ITEM
    GS_VBPOK-POSNN    = GS_LIPS-POSNR. "DELIVERY ITEM
    GS_VBPOK-VBELN    = GS_LIPS-VBELN. "PICK ORDER
    GS_VBPOK-VBTYP_N  = 'Q'.
    GS_VBPOK-PIKMG    = GS_LIPS-LFIMG.
    GS_VBPOK-MEINS    = GS_LIPS-MEINS.
    GS_VBPOK-CHARG    = GS_LIPS-CHARG.
    GS_VBPOK-MATNR    = GS_LIPS-MATNR.
    GS_VBPOK-WERKS    = GS_LIPS-WERKS.
    APPEND GS_VBPOK TO GT_VBPOK.
    CLEAR:GS_VBPOK.
    AT END OF VBELN.
      DO.
        CALL FUNCTION 'ENQUEUE_EMMARCE'
          EXPORTING
            MANDT          = SY-MANDT
            MATNR          = GS_LIPS-MATNR
            WERKS          = GS_LIPS-WERKS
          EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.
        IF SY-SUBRC = 1.
*  AND SY-INDEX = 1..
* Implement suitable error handling here
          WAIT UP TO 2 SECONDS.
          CONTINUE.
        ELSEIF SY-SUBRC = 0.
          EXIT.
        ENDIF.
      ENDDO.

      CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
        EXPORTING
          VBKOK_WA                     = GS_VBKOK
          SYNCHRON                     = ' '
          NO_MESSAGES_UPDATE_1         = ' '
          COMMIT                       = 'X'
          DELIVERY                     = GS_LIPS-VBELN
          UPDATE_PICKING               = 'X'
          NICHT_SPERREN_1              = 'X'
*         IF_CONFIRM_CENTRAL           = ' '
*         IF_WMPP                      = ' '
*         IF_GET_DELIVERY_BUFFERED     = ' '
*         IF_NO_GENERIC_SYSTEM_SERVICE = ' '
*         IF_DATABASE_UPDATE_1         = '1'
*         IF_NO_INIT_1                 = ' '
*         IF_NO_READ_1                 = ' '
*         IF_ERROR_MESSAGES_SEND       = 'X'
*         IF_NO_BUFFER_REFRESH         = ' '
*         IT_PARTNER_UPDATE            =
*         IT_SERNR_UPDATE              =
*         IF_NO_REMOTE_CHG_1           = ' '
*         IF_NO_MES_UPD_PACK           = ' '
*         IF_LATE_DELIVERY_UPD         = ' '
*         IF_TXT_REINITIALIZE          =
*         IF_BOR_INIT                  = ' '
*         SPE_MES_NO_SEND_NODIAL       =
*         IT_LECOMP_1                  =
        IMPORTING
          EF_ERROR_ANY                 = lv_error1
          EF_ERROR_IN_ITEM_DELETION    = lv_error2
          EF_ERROR_IN_POD_UPDATE       = lv_error3
          EF_ERROR_IN_INTERFACE        = lv_error4
          EF_ERROR_IN_GOODS_ISSUE      = lv_error5
          EF_ERROR_IN_FINAL_CHECK      = lv_error6
          EF_ERROR_PARTNER_UPDATE      = lv_error7
          EF_ERROR_SERNR_UPDATE        = lv_error8
        TABLES
          VBPOK_TAB                    = GT_VBPOK
          VERKO_TAB                    = GT_VERKO.
      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
        WAIT UP TO 1 SECONDS.
        CALL FUNCTION 'DEQUEUE_EMMARCE'
          EXPORTING
            MANDT = SY-MANDT
            MATNR = GS_LIPS-MATNR
            WERKS = GS_LIPS-WERKS.
        PERFORM CREATE_BILLING.
      ENDIF.
      CLEAR :GS_VBKOK,GS_VERKO,GS_LIPS,GT_VBPOK[],GT_VERKO[].
    ENDAT.

  ENDLOOP.
ENDFORM.                    " CRETAE_PGI
*&---------------------------------------------------------------------*
*&      Form  CREATE_BILLING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_BILLING .

  DATA: LT_BAPIVBRK TYPE TABLE OF BAPIVBRK,
        LT_RETURN   TYPE TABLE OF BAPIRET1,
        LT_SUCCESS  TYPE TABLE OF BAPIVBRKSUCCESS,
        LT_CREATION TYPE TABLE OF BAPICREATORDATA,
        LS_CREATION TYPE BAPICREATORDATA,
        LS_SUCCESS  TYPE BAPIVBRKSUCCESS,
        LS_RETURN   TYPE BAPIRET1,
        LV_ERR_MSG  TYPE STRING,
        LS_BAPIVBRK TYPE BAPIVBRK.
  LS_BAPIVBRK-REF_DOC = GS_LIPS-VBELN.
  LS_BAPIVBRK-REF_DOC_CA = 'J'.
  APPEND LS_BAPIVBRK TO LT_BAPIVBRK.
  CLEAR: LS_BAPIVBRK.
  IF LT_BAPIVBRK IS NOT INITIAL.
    LS_CREATION-CREATED_BY = SY-UNAME.
    LS_CREATION-CREATED_ON = SY-DATUM.

    CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
      EXPORTING
        CREATORDATAIN   = LS_CREATION
*       TESTRUN         =
*       POSTING         =
      TABLES
        BILLINGDATAIN   = LT_BAPIVBRK
*       CONDITIONDATAIN =
*       CCARDDATAIN     =
*       TEXTDATAIN      =
*       ERRORS          =
        RETURN          = LT_RETURN
        SUCCESS         = LT_SUCCESS.
    IF LT_SUCCESS IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
      if sy-subrc is INITIAL.
        WAIT UP TO 1 SECONDS.
        CONCATENATE GS_LIPS-VBELN 'Billing created sucessfully' into lv_err_msg SEPARATED BY space..
*        write: / lv_err_msg.
        gs_output-vbeln = gs_lips-vbeln.
        gs_output-msg  = lv_err_msg.
        append gs_output to gt_output.
        clear:gs_output.
      else.
        CONCATENATE GS_LIPS-VBELN 'Error in Billing creation' into lv_err_msg SEPARATED BY space..
*        write: / lv_err_msg.
        gs_output-vbeln = gs_lips-vbeln.
        gs_output-msg  = lv_err_msg.
        append gs_output to gt_output.
        clear:gs_output.
      endif.
    ELSE.
      CLEAR: LS_RETURN,LV_ERR_MSG.
      READ TABLE LT_RETURN INTO LS_RETURN INDEX 1.
      IF SY-SUBRC IS INITIAL.
        IF LS_RETURN-MESSAGE  IS INITIAL and
          LS_RETURN-MESSAGE_V1 IS INITIAL and
          LS_RETURN-MESSAGE_V2 is INITIAL and
          LS_RETURN-MESSAGE_V3 is INITIAL and
          LS_RETURN-MESSAGE_V4 is INITIAL.
          CONCATENATE GS_LIPS-VBELN
                   'Error while creating billing Document'  INTO LV_ERR_MSG SEPARATED BY space.
        ELSE.
          CONCATENATE GS_LIPS-VBELN
                      LS_RETURN-MESSAGE
                      LS_RETURN-MESSAGE_V1
                      LS_RETURN-MESSAGE_V2
                      LS_RETURN-MESSAGE_V3
                      LS_RETURN-MESSAGE_V4 INTO LV_ERR_MSG SEPARATED BY space.
        ENDIF.
      ENDIF.
      gs_output-vbeln = gs_lips-vbeln.
      gs_output-msg  = lv_err_msg.
      append gs_output to gt_output.
      clear:gs_output.
    ENDIF.
  ENDIF.

ENDFORM.                    " CREATE_BILLING
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .

  DATA:LV_REPID TYPE SY-REPID,
       ls_fc_str    type slis_fieldcat_alv,
       LT_FIELDCAT  type SLIS_T_FIELDCAT_ALV.
  LV_REPID = SY-REPID.

  ls_fc_str-fieldname = 'VBELN'.
  ls_fc_str-key       = ' '.
  ls_fc_str-seltext_l   = 'Delivery Number'.
  ls_fc_str-ddictxt   = 'L'.
  append ls_fc_str to lt_fieldcat.
  clear ls_fc_str.

  ls_fc_str-fieldname = 'MSG'.
  ls_fc_str-key       = ' '.
  ls_fc_str-seltext_l   = 'Message'.
  ls_fc_str-ddictxt   = 'L'.
  append ls_fc_str to lt_fieldcat.
  clear ls_fc_str..
*  SORT gt_alv_data.
  IF GT_OUTPUT IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = LV_REPID
        IT_FIELDCAT        = LT_FIELDCAT
*       IT_EVENTS          = GT_EVENTS
*       is_print           = gd_prntparams
        I_SAVE             = 'X'
      TABLES
        T_OUTTAB           = GT_OUTPUT
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.

    IF SY-SUBRC <> 0.
      WRITE: /'Alv report could not be generated'.
    ENDIF.
  ENDIF.
ENDFORM.                    " ALV_DISPLAY
