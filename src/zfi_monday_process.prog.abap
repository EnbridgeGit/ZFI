*&---------------------------------------------------------------------*
*& Report  ZFI_MONDAY_PROCESS
*&
*&---------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 10-Oct-2018                                          *
* Created By    : SPDURGAVARAPU                                        *
* Correction No : CHG0126429                                           *
* Transport No   :  D30K929172                                         *
* Description : Monday Automation Process                              *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 16-Jan-2020  JOOKONTR     D30K930392 CHG0170328 Changes to the file  *
*                                                  output file lenght  *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 14-AUG-2020  KMB          D30K930651 CHG0188574 ZFI_MONDAY_PROCESS   *
*                                      ZFI_TUESDAY_PROCESS are not able*
*                                      to write to SAP Fileshare in US *
*                                      s/m & to UNIX servr in UG/SW s/m*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT  zfi_monday_process.
TABLES:somlreci1.
PARAMETERS: p_file LIKE  rlgrap-filename OBLIGATORY,
            p_job TYPE btcjob.
PARAMETERS : p_path TYPE   ibipparms-path OBLIGATORY, "presentation server path for AL11
             p_spath TYPE    ibipparms-path OBLIGATORY.
SELECT-OPTIONS: s_email FOR somlreci1-receiver NO INTERVALS.

*BOC by KMB on 14.8.2020 CHG0188574
PARAMETERS : p_app   RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmnd, "Application Server
             p_pre   RADIOBUTTON GROUP rad2.
*EOC by KMB on 14.8.2020 CHG0188574

*& Begin of changes by JOOKONTR for CHG0170328
TYPES: BEGIN OF ty_soli,
        line(1000) TYPE c,
       END OF ty_soli.
*& End of changes by JOOKONTR for CHG0170328
DATA:  "wa_soli TYPE soli,    " Commented by JOOKONTR for CHG0170328
      wa_soli TYPE ty_soli, " Added by JOOKONTR for CHG0170328
      wa_soli1 TYPE string,
*       lt_soli TYPE STANDARD TABLE OF soli.  " Commented by JOOKONTR for CHG0170328
      lt_soli TYPE TABLE OF ty_soli.   " Added by JOOKONTR for CHG0170328
DATA: lv_path TYPE string,
      lv_path1 TYPE string,
      lv_spath TYPE string.

*BOC by KMB on 14.8.2020 CHG0188574
DATA : lv_pathd TYPE string VALUE '/usr/sap/interfaces/',
       lv_prec TYPE string VALUE '/usr/',
       lv_pathc TYPE string.

*AT SELECTION-SCREEN OUTPUT.
AT SELECTION-SCREEN.
  CLEAR lv_pathc.
  CONCATENATE lv_pathd sy-sysid '/' INTO lv_pathc.
  IF p_pre = 'X' AND ( p_path CS lv_prec OR p_spath CS lv_prec ).
    MESSAGE e000(zfi01) WITH text-009.
  ENDIF.
   IF p_app = 'X' AND ( p_path NS  lv_pathc OR p_spath NS lv_pathc ).
    MESSAGE e000(zfi01) WITH text-009.
  ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*Providing F4 help for user to enter file name
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = p_file
      IMPORTING
        serverfile       = p_file
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE 'File Path not found' TYPE 'E'.
    ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
*BOC by KMB on 14.8.2020 CHG0188574
  IF p_pre = 'X'.
*EOC by KMB on 14.8.2020 CHG0188574
    CALL FUNCTION 'F4_FILENAME'
      IMPORTING
        file_name = p_path.
*BOC by KMB on 14.8.2020 CHG0188574
  ELSEIF p_app = 'X'.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = p_path
      IMPORTING
        serverfile       = p_path
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE 'File Path not found' TYPE 'E'.
    ENDIF.
  ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_spath.
*BOC by KMB on 14.8.2020 CHG0188574
  IF p_pre = 'X'.
*EOC by KMB on 14.8.2020 CHG0188574
    CALL FUNCTION 'F4_FILENAME'
      IMPORTING
        file_name = p_spath.
*BOC by KMB on 14.8.2020 CHG0188574
  ELSEIF p_app = 'X'.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = p_file
      IMPORTING
        serverfile       = p_file
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE 'File Path not found' TYPE 'E'.
    ENDIF.
  ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574

START-OF-SELECTION.
  lv_path = p_path.
  lv_spath = p_spath.
  PERFORM al11_file.
  PERFORM spool_file.
  PERFORM send_mail.
*&---------------------------------------------------------------------*
*&      Form  AL11_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM al11_file .
  DATA lv_block_len TYPE i.
  DATA lv_package_len TYPE i.
  DATA: lv_date TYPE sy-datum.
  lv_date = sy-datum.
  CONSTANTS blocksize TYPE i VALUE 524287.

*BOC by KMB on 14.8.2020 CHG0188574
  DATA : lv_string        TYPE string,
         lv_msg           TYPE text100.
*EOC by KMB on 14.8.2020 CHG0188574

*Read the data from application server
  OPEN DATASET p_file FOR INPUT IN TEXT  MODE ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    DO.
      READ DATASET p_file INTO wa_soli-line MAXIMUM LENGTH blocksize LENGTH lv_block_len.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      IF lv_block_len > 0.
        lv_package_len = lv_package_len + lv_block_len.
        APPEND wa_soli TO lt_soli.
        CLEAR:wa_soli.
      ENDIF.
    ENDDO.
    CLOSE DATASET p_file.
  ENDIF.
  CLEAR:lv_path1.

  lv_path1 = lv_path.
  CONCATENATE lv_path lv_date+4(2)  lv_date+6(2)  lv_date+0(4) '_' 'EAST' '_' 'Errors' '.txt' INTO lv_path.


*BOC by KMB on 14.8.2020 CHG0188574
  IF p_pre = 'X'.
*EOC by KMB on 14.8.2020 CHG0188574

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING "     bin_filesize          = lv_package_len
          filename                = lv_path
          filetype                = 'ASC'
          append                  = 'X'
          write_field_separator   = 'X'
          dat_mode                = 'X'
        TABLES
          data_tab                = lt_soli
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.
      IF sy-subrc EQ 0.
*Implement suitable error handling here
      ENDIF.

*BOC by KMB on 14.8.2020 CHG0188574
  ENDIF.
  IF p_app = 'X'.

      OPEN DATASET lv_path   FOR APPENDING IN TEXT MODE
                             ENCODING DEFAULT MESSAGE lv_msg.
      IF     ( sy-subrc NE 0 ).
        WRITE:   /001  text-008, lv_msg.
        MESSAGE  e000(zfi01) WITH text-008 lv_msg.
      ELSEIF sy-subrc = 0.
        LOOP AT lt_soli INTO wa_soli.
          lv_string =  wa_soli.
          CLEAR:wa_soli.
          TRANSFER lv_string TO lv_path.
        ENDLOOP.
      ENDIF.

      CLOSE DATASET lv_path.

  ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574

ENDFORM.                    " AL11_FILE
*&---------------------------------------------------------------------*
*&      Form  SPOOL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM spool_file .
  DATA: lv_jobcount TYPE btcjobcnt,
        lv_stepcount TYPE btcstepcnt.
  " DATA lv_package_len TYPE i VALUE 524287.
  TYPES: BEGIN OF ty_spoolid,
          jobname  TYPE btcjob,
          jobcount  TYPE btcjobcnt,
          stepcount  TYPE btcstepcnt,
          spoolid TYPE btclistid,
        END  OF ty_spoolid.
  DATA: lt_spoolid TYPE STANDARD TABLE OF ty_spoolid,
        wa_spoolid TYPE ty_spoolid.
  DATA: lv_string TYPE string,
        lv_xstring TYPE xstring.
  TYPES: BEGIN OF ty_spool_data,
               data(2048) TYPE c,
         END OF ty_spool_data.
  DATA:  lt_spool_xls TYPE STANDARD TABLE OF ty_spool_data,
        wa_spool_xls TYPE ty_spool_data,
        lt_xls_spool TYPE STANDARD TABLE OF soli.
  DATA: lv_spoolid TYPE rspoid.
  DATA: "lv_count TYPE i,
        lv_counter TYPE c VALUE '1',
        lv_date TYPE sy-datum.

*BOC by KMB on 14.8.2020 CHG0188574
  DATA : lv_string_n      TYPE string,
         lv_msg           TYPE text100.
*EOC by KMB on 14.8.2020 CHG0188574

  lv_date = sy-datum.
  CLEAR: lv_jobcount, lv_stepcount,lt_spoolid[].
  SELECT  jobcount stepcount UP TO 1 ROWS FROM tbtco
      INTO (lv_jobcount, lv_stepcount )
        WHERE jobname = p_job
        AND strtdate EQ lv_date
        AND status = 'F'
        ORDER BY jobcount DESCENDING.
  ENDSELECT.
  IF sy-subrc = 0.
    SELECT jobname jobcount stepcount spoolid
           FROM tbtc_spoolid
           INTO TABLE lt_spoolid
           WHERE jobname = p_job
            AND  jobcount = lv_jobcount.
    IF sy-subrc = 0.
**Read the spool
      LOOP AT lt_spoolid INTO wa_spoolid.
        lv_spoolid = wa_spoolid-spoolid.
*FM called that returns the Spool Request Number data into and internal table
        CLEAR lt_spool_xls.
        CALL FUNCTION 'RSPO_RETURN_ABAP_SPOOLJOB'
          EXPORTING
            rqident              = lv_spoolid                     "Spool Request Number
            first_line           = 1
          TABLES
            buffer               = lt_spool_xls                            "Internal table that will have the Spool Request No data
          EXCEPTIONS
            no_such_job          = 1
            not_abap_list        = 2
            job_contains_no_data = 3
            selection_empty      = 4
            no_permission        = 5
            can_not_access       = 6
            read_error           = 7
            OTHERS               = 8.
        IF sy-subrc IS INITIAL.
*To convert the spool data into excel format
          CALL FUNCTION 'SO_RAW_TO_RTF'
            TABLES
              objcont_old = lt_spool_xls               "Internal table having spool data
              objcont_new = lt_xls_spool.           "Int table having Excel format data converted from Spool data

*To convert the string into xstring format for mail sending

          CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
            EXPORTING
              text   = lv_string "String of String data type
            IMPORTING
              buffer = lv_xstring "String of XString data type
            EXCEPTIONS
              failed = 1
              OTHERS = 2.
          IF sy-subrc IS INITIAL.
**Download files to presentation server.

** File Name
            IF lv_counter = 1.
              lv_spath = p_spath.
              CONCATENATE lv_spath lv_date+4(2)  lv_date+6(2)  lv_date+0(4) '_' 'EAST' '_' 'Spoolerror' '.xls' INTO lv_spath.
            ELSEIF lv_counter = 2.
              lv_spath = p_spath.
              CONCATENATE lv_spath lv_date+4(2)  lv_date+6(2)  lv_date+0(4) '_' 'EAST' '_' 'Spool' '.txt' INTO lv_spath.
            ENDIF.

*BOC by KMB on 14.8.2020 CHG0188574
            IF p_pre = 'X'.
*EOC by KMB on 14.8.2020 CHG0188574

                CALL METHOD cl_gui_frontend_services=>gui_download
                  EXPORTING
                    filename              = lv_spath
                    write_field_separator = 'X'
                  CHANGING
                    data_tab              = lt_spool_xls.

                lv_counter = lv_counter + 1.

*BOC by KMB on 14.8.2020 CHG0188574
            ENDIF.
            IF p_app = 'X'.

                OPEN DATASET lv_spath   FOR APPENDING IN TEXT MODE
                                       ENCODING DEFAULT MESSAGE lv_msg.
                IF     ( sy-subrc NE 0 ).
                  WRITE:   /001  text-008, lv_msg.
                  MESSAGE  e000(zfi01) WITH text-008 lv_msg.
                ELSEIF sy-subrc = 0.
                  LOOP AT lt_spool_xls INTO wa_spool_xls.
                    lv_string_n =  wa_spool_xls.
                    TRANSFER lv_string_n TO lv_spath.
                    CLEAR:wa_spool_xls, lv_string_n.
                  ENDLOOP.
                ENDIF.

                CLOSE DATASET lv_spath.

                lv_counter = lv_counter + 1.
            ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574

          ENDIF.
        ENDIF.
        CLEAR:wa_spoolid.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " SPOOL_FILE
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail .

  DATA: ls_docdata     TYPE sodocchgi1,   "Mail subject
         lt_receiver    TYPE TABLE OF somlrec90 WITH HEADER LINE,  "Recepient
         lt_content     TYPE STANDARD TABLE OF soli      WITH HEADER LINE."Main body

  CLEAR lt_receiver.

***********************************************************************
*Create the list of recipients
***********************************************************************
  LOOP AT s_email.
    lt_receiver-receiver = s_email-low.
    lt_receiver-rec_type = 'U'.
    lt_receiver-express = 'X'.
    APPEND lt_receiver.
  ENDLOOP.

  CLEAR ls_docdata.
*    Mail Subject
  ls_docdata-obj_name = text-001.
  ls_docdata-obj_langu = sy-langu.


  CLEAR lt_content[].
*    Mail Body
  CLEAR lt_content.
  lt_content-line = text-002.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = ' '.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = text-003.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = ' '.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = lv_path1.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = ' '.
  APPEND lt_content.
*
*  CLEAR lt_content.
*  lt_content-line = lv_spath.
*  APPEND lt_content.
*
*  CLEAR lt_content.
*  lt_content-line = ' '.
*  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = text-005.
  APPEND lt_content.
  CLEAR lt_content.
  lt_content-line = text-007.
  APPEND lt_content.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_docdata
    TABLES
      object_content             = lt_content
      receivers                  = lt_receiver
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE text-006 TYPE 'S'.
  ENDIF.

ENDFORM.                    " SEND_MAIL
