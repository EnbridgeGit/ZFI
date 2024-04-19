*&---------------------------------------------------------------------*
*& Report  ZFI_REPORT_WRAPPER_FORM                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_REPORT_WRAPPER_FORM                       *
*& Include Name       :  ZFI_REPORT_WRAPPER_FORM                       *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  19-Oct-2020                                   *
*& Change Request     :  CHG0191644                                    *
*& Purpose            :  Wrapper program to send FBL1N output in CSV   *
*&                       format as an email attachment                 *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 19-Oct 2020  AHMADT        CHG0191644 D30K930705 Initial Development*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_MAIL .
    TRY.
        cl_bcs_convert=>string_to_solix(
          EXPORTING
            iv_string   = gv_string
            iv_codepage = '4103'  "for MS Excel
            iv_add_bom  = 'X'
          IMPORTING
            et_solix  = gv_binary_content
            ev_size   = gv_size ).
      CATCH cx_bcs.
        MESSAGE e445(so).
    ENDTRY.


    TRY .
        go_send_request = cl_bcs=>create_persistent( ).

        APPEND text-002 TO gv_main_text.

        go_document = cl_document_bcs=>create_document(
                i_type    = 'RAW'
                i_text    =  gv_main_text
                i_subject = text-001 )."email subject

*      spread sheet as attachment to document object

        gv_attachment_name = text-003.
        go_document->add_attachment(
          i_attachment_type    = 'csv' "'xls'
          i_attachment_subject = gv_attachment_name    "Name of Excel File
          i_attachment_size    = gv_size
          i_att_content_hex    = gv_binary_content ).


*     add document object to send request
        go_send_request->set_document( go_document ).

        LOOP AT s_mailto.
          gv_email = s_mailto-low.
          go_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
          go_send_request->add_recipient( go_recipient ).
        ENDLOOP.


*     send document
        gv_sent_to_all = go_send_request->send( i_with_error_screen = 'X' ).

        COMMIT WORK.

        IF gv_sent_to_all IS INITIAL.
          MESSAGE i500(sbcoms) WITH s_mailto.
        ELSE.
          MESSAGE s022(so).
        ENDIF.

      CATCH cx_bcs INTO go_bcs_exception.
        MESSAGE i865(so) WITH go_bcs_exception->error_type.
    ENDTRY.
ENDFORM.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  REFRESH : gt_tab[], gt_list[], gt_str_tab[], gt_final[].
SUBMIT (p_name) USING SELECTION-SET p_var
                  EXPORTING LIST TO MEMORY AND RETURN.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = gt_tab
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF  sy-subrc = 0.

    CALL FUNCTION 'LIST_TO_ASCI'
      IMPORTING
        list_string_ascii  = gt_list
      TABLES
        listobject         = gt_tab
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      MESSAGE text-008 TYPE 'E'.
    ENDIF.

    ELSE.
      MESSAGE text-008 TYPE 'E'.
  ENDIF.
  LOOP AT gt_list INTO gs_list.
    IF gs_list CS '-----' OR gs_list IS INITIAL.

    ELSE.
      SPLIT gs_list AT '|' INTO TABLE gt_str_tab.
      DELETE gt_str_tab INDEX 1.
      LOOP AT gt_str_tab INTO gs_str_tab.
*
        CASE sy-tabix.
          WHEN 1.
            CONDENSE gs_str_tab NO-GAPS.
            gs_final-cleared_items = gs_str_tab.
          WHEN 2.
            gs_final-cocd          = gs_str_tab.
          WHEN 3.
            gs_final-vendor        = gs_str_tab.
          WHEN 4.
            gs_final-payee         = gs_str_tab.
          WHEN 5.
            gs_final-reference     = gs_str_tab.
          WHEN 6.
            gs_final-documentno    = gs_str_tab.
          WHEN 7.
            gs_final-type          = gs_str_tab.
          WHEN 8.
            gs_final-doc_date      = gs_str_tab.
          WHEN 9.
            gs_final-gl_indicator  = gs_str_tab.
          WHEN 10.
            gs_final-due_date      = gs_str_tab.
          WHEN 11.
            gs_final-pmnt_block    = gs_str_tab.
          WHEN 12.
            gs_final-pay_method    = gs_str_tab.
          WHEN 13.
            gs_final-bnkt          = gs_str_tab.
          WHEN 14.
            IF gs_str_tab IS NOT INITIAL.
              CONDENSE gs_str_tab NO-GAPS.
              CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
                CHANGING
                   value = gs_str_tab.
            ENDIF.
            gs_final-amount        = gs_str_tab.
          WHEN 15.
            gs_final-curr          = gs_str_tab.
          WHEN 16.
            gs_final-payt          = gs_str_tab.
          WHEN 17.
            gs_final-net_due_dt    = gs_str_tab.
          WHEN 18.
            gs_final-clearing      = gs_str_tab.
          WHEN 19.
            gs_final-clear_doc     = gs_str_tab.
          WHEN 20.
            gs_final-text          = gs_str_tab.
          WHEN 21.
            gs_final-ref_key_3     = gs_str_tab.
          WHEN 22.
            gs_final-check_no      = gs_str_tab.
          WHEN 23.
            gs_final-pmt_mth_su    = gs_str_tab.
          WHEN 24.
            gs_final-doc_head_text = gs_str_tab.
          WHEN 25.
            IF gs_str_tab IS NOT INITIAL.
            CONDENSE gs_str_tab NO-GAPS.
            CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
              CHANGING
                value = gs_str_tab.
            ENDIF.
            gs_final-disc_base_amt = gs_str_tab.

          WHEN OTHERS.
        ENDCASE.
        CLEAR  gs_str_tab.
      ENDLOOP.
      APPEND gs_final TO gt_final.
      CLEAR gs_final.
    ENDIF.
    CLEAR gs_list.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_ATTACHMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_ATTACHMENT .
READ TABLE gt_final INTO gs_final INDEX 1.
  DELETE gt_final WHERE pmnt_block IS NOT INITIAL.
  gs_final-cleared_items = text-009.
  gs_final-gl_indicator  = text-010.
  gs_final-pmnt_block    = text-004.
  gs_final-pay_method    = text-005.
  gs_final-amount        = text-006.
  gs_final-disc_base_amt = text-007.
  gs_final-due_date      = text-011.
  INSERT gs_final INTO gt_final INDEX 1.

  LOOP AT gt_final INTO gs_final.
    CONCATENATE gv_string
                gs_final-cleared_items  gc_tab
                gs_final-cocd           gc_tab
                gs_final-vendor         gc_tab
                gs_final-payee          gc_tab
                gs_final-reference      gc_tab
                gs_final-documentno     gc_tab
                gs_final-type           gc_tab
                gs_final-doc_date       gc_tab
                gs_final-gl_indicator   gc_tab
                gs_final-due_date       gc_tab
                gs_final-pmnt_block     gc_tab
                gs_final-pay_method     gc_tab
                gs_final-bnkt           gc_tab
                gs_final-amount         gc_tab
                gs_final-curr           gc_tab
                gs_final-payt           gc_tab
                gs_final-net_due_dt     gc_tab
                gs_final-clearing       gc_tab
                gs_final-clear_doc      gc_tab
                gs_final-text           gc_tab
                gs_final-ref_key_3      gc_tab
                gs_final-check_no       gc_tab
                gs_final-pmt_mth_su     gc_tab
                gs_final-doc_head_text  gc_tab
                gs_final-disc_base_amt  gc_crlf
           INTO gv_string.
    CLEAR gs_final.
  ENDLOOP.
ENDFORM.                    " FILL_ATTACHMENT
