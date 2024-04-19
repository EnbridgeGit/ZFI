*&---------------------------------------------------------------------*
*&  Include           ZFI_DELETED_PARKED_FORMS
*&---------------------------------------------------------------------*
FORM f_create_content .
* columns are separated by gc_TAB
* line ends with gc_CRLF

  CONCATENATE text-008       "excel file heading
              gc_crlf gc_crlf
         INTO gv_string.

* Column Names of Excel file that will be attached in the email
  CONCATENATE gv_string
              text-009  gc_tab
              text-010  gc_tab
              text-011  gc_tab
              text-012  gc_tab
              text-013  gc_tab
              text-014  gc_tab
              text-015  gc_crlf
         INTO gv_string.

* data lines
  SELECT bukrs
         belnr
         gjahr
         blart
         cpudt
         awtyp
         awkey
    FROM bkpf
    INTO TABLE gt_bkpf
   WHERE bukrs IN s_bukrs AND
         belnr IN s_belnr AND
         gjahr IN s_gjahr AND
         blart IN s_blart AND
         cpudt IN s_cpudt AND
         bstat = gc_bstat.         "passing 'Z' to bstat as documents' status which have been deleted is 'Z'

  LOOP AT gt_bkpf INTO gs_bkpf.

    IF gs_bkpf-awtyp = 'BKPF'.
      gv_objectclass = 'BELEGV'.
      gv_bukrs1 = gs_bkpf-bukrs.

      IF strlen( gv_bukrs1 ) = 1.
        CONCATENATE sy-mandt
                    gs_bkpf-bukrs
                    ` ` ` ` ` `
                    gs_bkpf-bukrs
                    ` ` ` ` ` `
                    gs_bkpf-belnr
                    gs_bkpf-gjahr
               INTO gv_objectid .
      ELSEIF strlen( gv_bukrs1 ) = 2.
        CONCATENATE sy-mandt
                    gs_bkpf-bukrs
                    ` ` ` `
                    gs_bkpf-bukrs
                    ` ` ` `
                    gs_bkpf-belnr
                    gs_bkpf-gjahr
               INTO gv_objectid .
      ELSEIF strlen( gv_bukrs1 ) = 3.
        CONCATENATE sy-mandt
                    gs_bkpf-bukrs
                    ` `
                    gs_bkpf-bukrs
                    ` `
                    gs_bkpf-belnr
                    gs_bkpf-gjahr
               INTO gv_objectid .
      ELSEIF strlen( gv_bukrs1 ) = 4.
        CONCATENATE sy-mandt
                    gs_bkpf-bukrs
                    gs_bkpf-bukrs
                    gs_bkpf-belnr
                    gs_bkpf-gjahr
               INTO gv_objectid .
      ENDIF.

    ELSEIF gs_bkpf-awtyp = 'RMRP'.
           gv_objectclass = 'INCOMINGINVOICE'.
           gv_objectid = gs_bkpf-awkey.
    ENDIF.

    CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
      EXPORTING
        archive_handle             = 0
        date_of_change             = '00000000'
        objectclass                = gv_objectclass
        objectid                   = gv_objectid
        time_of_change             = '000000'
        username                   = space
      TABLES
        i_cdhdr                    = gt_cdhdr
      EXCEPTIONS
        no_position_found          = 1
        wrong_access_to_archive    = 2
        time_zone_conversion_error = 3
        OTHERS                     = 4.

    IF sy-subrc = 0.

      READ TABLE gt_cdhdr INTO gs_cdhdr INDEX 1.

      gv_entry_date+0(2) = gs_bkpf-cpudt+6(2).
      gv_entry_date+2(1) = gc_slash.
      gv_entry_date+3(2) = gs_bkpf-cpudt+4(2).
      gv_entry_date+5(1) = gc_slash.
      gv_entry_date+6(4) = gs_bkpf-cpudt+0(4).

      gv_parked_date+0(2) = gs_cdhdr-udate+6(2).
      gv_parked_date+2(1) = gc_slash.
      gv_parked_date+3(2) = gs_cdhdr-udate+4(2).
      gv_parked_date+5(1) = gc_slash.
      gv_parked_date+6(4) = gs_cdhdr-udate+0(4).

      gv_parked_time+0(2) = gs_cdhdr-utime+0(2).
      gv_parked_time+2(1) = gc_colon.
      gv_parked_time+3(2) = gs_cdhdr-utime+2(2).
      gv_parked_time+5(1) = gc_colon.
      gv_parked_time+6(2) = gs_cdhdr-utime+4(2).

      CONCATENATE gv_string
                  gs_bkpf-belnr      gc_tab
                  gs_bkpf-bukrs      gc_tab
                  gv_entry_date      gc_tab
                  gs_bkpf-gjahr      gc_tab
                  gs_cdhdr-username  gc_tab
                  gv_parked_time     gc_tab
                  gv_parked_date     gc_crlf
             INTO gv_string.
      CLEAR: gt_cdhdr,gs_cdhdr,gs_bkpf.

    ENDIF.
  ENDLOOP.

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
ENDFORM.                    " F_CREATE_CONTENT


*&---------------------------------------------------------------------*
*&      Form  F_DATE_CONVERT
*&---------------------------------------------------------------------*
*       To convert date from yyyymmdd format to dd.mm.yyyy format
*----------------------------------------------------------------------*
FORM f_date_convert .
  gv_low_date+0(2) = s_cpudt-low+6(2).
  gv_low_date+2(1) = gc_dot.
  gv_low_date+3(2) = s_cpudt-low+4(2).
  gv_low_date+5(1) = gc_dot.
  gv_low_date+6(4) = s_cpudt-low+0(4).

  gv_high_date+0(2) = s_cpudt-high+6(2).
  gv_high_date+2(1) = gc_dot.
  gv_high_date+3(2) = s_cpudt-high+4(2).
  gv_high_date+5(1) = gc_dot.
  gv_high_date+6(4) = s_cpudt-high+0(4).
ENDFORM.                    " F_DATE_CONVERT


*&---------------------------------------------------------------------*
*&      Form  F_SYSTEM_CHECK
*&---------------------------------------------------------------------*
*       To check from which system, program is being run from
*----------------------------------------------------------------------*
FORM f_system_check .
  IF sy-sysid      = 'P01' OR sy-sysid = 'D30' OR sy-sysid = 'D22'.
    gv_system_name = text-016.
  ELSEIF sy-sysid  = 'P11' OR sy-sysid = 'Q11' OR sy-sysid = 'C11'.
    gv_system_name = text-017.
  ELSEIF sy-sysid  = 'DEC' OR sy-sysid = 'QEC' OR sy-sysid = 'PEC'.
    gv_system_name = text-018.
  ENDIF.
ENDFORM.                    " F_SYSTEM_CHECK


*&---------------------------------------------------------------------*
*&      Form  F_ATTACHMENT_NAME
*&---------------------------------------------------------------------*
*       To create attachment name in required format
*----------------------------------------------------------------------*
FORM f_attachment_name .
  CONCATENATE text-019
              text-020
              gv_system_name
              text-020
              gv_low_date
              text-020
              gv_high_date
         INTO gv_attachment_name.
ENDFORM.                    " F_ATTACHMENT_NAME


*&---------------------------------------------------------------------*
*&      Form  F_SEND_MAIL
*&---------------------------------------------------------------------*
*       To attach body and excel file, and send an email
*----------------------------------------------------------------------*
FORM f_send_mail.
  TRY.

      go_send_request = cl_bcs=>create_persistent( ).    "to create persistent send request

      APPEND text-021 TO gv_main_text.  "email body
      APPEND INITIAL LINE TO gv_main_text.
      CONCATENATE text-022
                  gv_system_name
                  text-023
                  gv_low_date
                  text-024
                  gv_high_date
             INTO gv_string_1
             SEPARATED BY space.

      APPEND gv_string_1 TO gv_main_text.
      APPEND INITIAL LINE TO gv_main_text.
      APPEND text-025 TO gv_main_text.


      go_document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    =  gv_main_text
        i_subject = text-008 ).       "email subject

*     add the spread sheet as attachment to document object
      go_document->add_attachment(
        i_attachment_type    = 'xls'
        i_attachment_subject = gv_attachment_name    "Name of Excel File
        i_attachment_size    = gv_size
        i_att_content_hex    = gv_binary_content ).

*     add document object to send request
      go_send_request->set_document( go_document ).

*     add recipient (e-mail address)
*     create recipient object

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

* exception handling
    CATCH cx_bcs INTO go_bcs_exception.
      MESSAGE i865(so) WITH go_bcs_exception->error_type.
  ENDTRY.
ENDFORM.                    " F_SEND_MAIL


*&---------------------------------------------------------------------*
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
*   refresh all golbal internal tables,work areas and variables
*----------------------------------------------------------------------*
FORM f_refresh .
  CLEAR:gs_bkpf,
        gs_validation.

  REFRESH:gt_bkpf,
          gt_validation.

  CLEAR:gv_main_text,
        gv_binary_content,
        gv_size,
        gv_sent_to_all,
        gv_email,
        gv_address,
        gv_string,
        gv_string_1,
        gv_bukrs,
        gv_low_date,
        gv_high_date,
        gv_entry_date,
        gv_system_name,
        gv_attachment_name.

ENDFORM.                    " F_REFRESH
