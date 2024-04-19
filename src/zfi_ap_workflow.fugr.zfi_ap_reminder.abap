FUNCTION ZFI_AP_REMINDER.

* This function is not used anymore replaced with program
*ZFIR_AP_REMINDER_CHECK

**"----------------------------------------------------------------------
**"*"Local Interface:
**"  IMPORTING
**"     REFERENCE(IMP_BUKRS) TYPE  BKPF-BUKRS
**"     REFERENCE(IMP_BELNR) TYPE  BKPF-BELNR
**"     REFERENCE(IMP_GJAHR) TYPE  BKPF-GJAHR
**"     REFERENCE(IMP_MANAGER) TYPE  WFSYST-INITIATOR
**"----------------------------------------------------------------------
**&---------------------------------------------------------------------*
**& Function Module    :  ZFI_AP_REMINDER                               *
**& Author             :  Shankar Balasubrmaniam                        *
**& Creation Date      :  19-Sep-2011                                   *
**& Object ID          :  NA                                            *
**& Application Area   :  NA                                            *
**& Description        :  Reminder                                      *
**&---------------------------------------------------------------------*
**----------------------------------------------------------------------*
**                      Modification Log(Latest Version on Top)         *
**----------------------------------------------------------------------*
** Version No    :                                                      *
** Date          :                                                      *
** Modified By   :                                                      *
** Correction No :                                                      *
** Description   :                                                      *
**----------------------------------------------------------------------*
*************************************************************************
****    Send mail to employee with attachment    ************************
*************************************************************************
**** Declarations for attachment creation
*
**** Declarations for attachment creation
*  DATA: lst_doc_chng   TYPE sodocchgi1,
*        ltp_tab_lines  TYPE sy-tabix,
*        ltp_body_start TYPE sy-tabix,
*        lta_objtxt     TYPE swftlisti1,
*        lta_objpack    TYPE swftpcklst,
*        lst_objpack    TYPE sopcklsti1,
*        lta_objbin     TYPE swftlisti1,
*        lst_objbin     TYPE solisti1,
*        lta_reclist    TYPE bubas_t_receiver,
*        lst_reclist    TYPE somlreci1,
*        lta_shortcut   TYPE zfitt_shortcut_parameter,
*        lst_shortcut   TYPE zfis_shortcut_parameter,
*        ltp_content    TYPE string,
*        ltp_usnam      TYPE usnam.
*
*  DATA: ltp_user_id    TYPE usr02-bname,
*        ltp_email_id   TYPE ad_smtpadr,
*        ltp_user_prefix TYPE char2,
*        ltp_user_name  TYPE char14.
*
*  DATA: ltp_persnr   TYPE adrp-persnumber,
*        ltp_addrnr   TYPE adrc-addrnumber.
**         lta_adsmtp   TYPE crd_tt_smtp,
**         lst_adsmtp   TYPE adsmtp,
**         ltp_persaddr TYPE addr3_sel,
**         ltp_uname    TYPE xubname.
*
**** Pass the required parameters and create the shortcut
*  REFRESH: lta_shortcut.
*  lst_shortcut-fieldname  = zif_fi_constants=>gc_screen_field_bukrs.   "'RF05V-BUKRS'.
*  lst_shortcut-fieldvalue = imp_bukrs.
*  INSERT lst_shortcut INTO TABLE lta_shortcut.
*  lst_shortcut-fieldname = zif_fi_constants=>gc_screen_field_belnr.
*  lst_shortcut-fieldvalue = imp_belnr.
*  INSERT lst_shortcut INTO TABLE lta_shortcut.
*  lst_shortcut-fieldname = zif_fi_constants=>gc_screen_field_gjahr.
*  lst_shortcut-fieldvalue = imp_gjahr.
*  INSERT lst_shortcut INTO TABLE lta_shortcut.
*
** Get User Name of the Manager
*  ltp_user_name = imp_manager.
*  SHIFT ltp_user_name LEFT DELETING LEADING 'US'.
*  ltp_user_id = ltp_user_name.
** Create Shortcut for the SAP Link
*
*  CALL FUNCTION 'ZFI_CREATE_SHORTCUT_TCODE'
*    EXPORTING
*      imp_recipient_user_id = sy-uname
*      imp_transaction       = zif_fi_constants=>gc_tcode_fbv0
*      imp_shortcut_param    = lta_shortcut
*    IMPORTING
*      exp_content           = ltp_content.
*
** Notification Subject for AP Approval
*  lst_doc_chng-obj_descr = 'Reminder'.
*
*
** Creation of the entry for the document
*  DESCRIBE TABLE lta_objtxt LINES ltp_tab_lines.
*  CLEAR: lst_objpack-transf_bin.
*  lst_objpack-head_start = 1.
*  lst_objpack-head_num   = 0.
*  lst_objpack-body_start = 1.
*  lst_objpack-body_num   = ltp_tab_lines.
*  lst_objpack-doc_type   = 'RAW'.
*  INSERT lst_objpack INTO TABLE lta_objpack.
*
** Populate attachment content
*  CLEAR : ltp_tab_lines, lta_objbin.
*  CONCATENATE ltp_content lst_objbin-line INTO lst_objbin-line.
*  INSERT lst_objbin INTO TABLE lta_objbin.
*
*
*  DESCRIBE TABLE lta_objbin LINES ltp_tab_lines.
** Creation of the entry for the compressed attachment
*  lst_objpack-transf_bin = 'X'. "Will get content from content_bin
*  lst_objpack-head_start = 1.
*  lst_objpack-head_num   = 1.
*  lst_objpack-body_start = 1.
*  lst_objpack-body_num   = ltp_tab_lines.
*  lst_objpack-doc_type   = 'EXT'.
*  lst_objpack-obj_name   = 'SAPSHORTCUTMAIL'.
*  CLEAR lst_objpack-obj_descr.
*  CONCATENATE imp_bukrs '_' imp_belnr '_' imp_gjahr '_Display.SAP' INTO lst_objpack-obj_descr.
*  lst_objpack-doc_size   = ltp_tab_lines * 255.
*  INSERT lst_objpack INTO TABLE lta_objpack.
*
*
** Get and Set the target recipent(s)
*  CLEAR lta_reclist.
*
**-----------------------------------------------------------------------------------------------------------------
** Get and Set the target recipent(s)
**-----------------------------------------------------------------------------------------------------------------
*  REFRESH: lta_reclist.
** Get Creaters Name; Send a copy of the reminder
*  SELECT SINGLE usnam INTO ltp_usnam FROM bkpf WHERE bukrs = imp_bukrs AND belnr = imp_belnr AND gjahr = imp_gjahr.
*  IF sy-subrc IS INITIAL.
*    ltp_user_id = ltp_usnam.
**   Get EMail ID of Creator
*
*    CALL FUNCTION 'SUSR_USER_ADDRESSKEY_GET'
*      EXPORTING
*        BNAME             = ltp_user_id
*      IMPORTING
*        PERSNUMBER        = ltp_persnr
*        ADDRNUMBER        = ltp_addrnr
*      EXCEPTIONS
*        ADDRESS_NOT_FOUND = 1
*        OTHERS            = 2.
*    IF SY-SUBRC <> 0.
**      RAISE EMAIL_ID_NOT_MAINTAINED.
*    ENDIF.
*
*    if sy-subrc eq 0.
*      SELECT SINGLE smtp_addr from adr6 INTO ltp_email_id WHERE ADDRNUMBER = ltp_addrnr and PERSNUMBER = ltp_persnr .
*      IF sy-subrc = 0.
*        lst_reclist-receiver = ltp_email_id.
*        lst_reclist-copy = 'X'.
*        lst_reclist-rec_type = 'U'.
*        INSERT  lst_reclist INTO TABLE lta_reclist.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IF ltp_user_id IS NOT INITIAL.
*    ltp_user_id = imp_manager+2(12).
**   Get EMail ID of Creator
*    CALL FUNCTION 'SUSR_USER_ADDRESSKEY_GET'
*      EXPORTING
*        BNAME             = ltp_user_id
*      IMPORTING
*        PERSNUMBER        = ltp_persnr
*        ADDRNUMBER        = ltp_addrnr
*      EXCEPTIONS
*        ADDRESS_NOT_FOUND = 1
*        OTHERS            = 2.
*    IF SY-SUBRC <> 0.
**      RAISE EMAIL_ID_NOT_MAINTAINED.
*    ENDIF.
*    if sy-subrc eq 0.
*      SELECT SINGLE smtp_addr from adr6 INTO ltp_email_id WHERE ADDRNUMBER = ltp_addrnr and PERSNUMBER = ltp_persnr .
*      IF sy-subrc = 0.
*        lst_reclist-receiver = ltp_email_id.
*        lst_reclist-rec_type = 'U'.
*        INSERT  lst_reclist INTO TABLE lta_reclist.
*      ENDIF.
*    endif.
*  endif.
*
**-----------------------------------------------------------------------------------------------------------------
** Send Mail to All
**-----------------------------------------------------------------------------------------------------------------
** Sending the document to recipients with the shortcut attachment
*  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
*    EXPORTING
*      document_data              = lst_doc_chng
*      put_in_outbox              = 'X'
*      commit_work                = 'X'
*    TABLES
*      packing_list               = lta_objpack
*      contents_bin               = lta_objbin
*      contents_txt               = lta_objtxt
*      receivers                  = lta_reclist
*    EXCEPTIONS
*      too_many_receivers         = 1
*      document_not_sent          = 2
*      operation_no_authorization = 4
*      OTHERS                     = 99.




ENDFUNCTION.
