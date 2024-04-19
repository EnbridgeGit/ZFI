*&---------------------------------------------------------------------*
*& Report  ZFAPR22_VIOLATING_ROLES
*&
*&---------------------------------------------------------------------*
*&Change Log:
*&
*& 03/10/2019 AHMADT  - D30K930194  CHG0161491
*&                      The program is modified in such a way that
*&                      multiple recipient email IDs can be given at
*&                      the selection screen.
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapr22_violating_roles.
*Start of changes by AHMADT on 03/10/2019 for CHG0161491
TABLES: agr_users,somlreci1.
*End of changes by AHMADT on 03/10/2019 for CHG0161491
TYPES: BEGIN OF ty_output,
       uname TYPE agr_users-uname,
       role  TYPE agr_users-agr_name,
       END OF ty_output.
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.
DATA: ge_mail   TYPE string,
      ge_name   TYPE string,
      ge_domain TYPE string,
      gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_role1 TYPE agr_users-agr_name OBLIGATORY.
SELECT-OPTIONS: s_roles FOR agr_users-agr_name NO INTERVALS OBLIGATORY,
                s_users FOR agr_users-uname.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_email RADIOBUTTON GROUP rad1 DEFAULT 'X'
                                    USER-COMMAND cmd,
            r_alv   RADIOBUTTON GROUP rad1.
*Start of changes by AHMADT on 03/10/2019 for CHG0161491
*             p_email TYPE string MODIF ID dsp.
SELECT-OPTIONS: s_email FOR somlreci1-receiver NO INTERVALS. "enabling multiple recipients
*End of changes by AHMADT on 03/10/2019 for CHG0161491
SELECTION-SCREEN END OF BLOCK b2.

*Start of changes by AHMADT on 03/10/2019 for CHG0161491
*    AT SELECTION-SCREEN ON p_email.
*   if p_email is not INITIAL.
*     ge_mail = p_email.
AT SELECTION-SCREEN ON s_email.
  IF s_email-low IS NOT INITIAL.
    ge_mail = s_email-low.
*End of changes by AHMADT on 03/10/2019 for CHG0161491
    FIND REGEX
    '^([a-z](?:\w|-|\.)*)@((\w|-|(\.?!\.)))+\.[[:alpha:]]{2,4}$'
    IN ge_mail
    IGNORING CASE
    SUBMATCHES ge_name ge_domain.
    IF sy-subrc <> 0.
      MESSAGE e000(zfi01) WITH
       'Enter correct email address for errors' '' '' ''.
      STOP.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  PERFORM toggle_functionality.

START-OF-SELECTION.
  IF r_email IS NOT INITIAL AND
*Start of changes by AHMADT on 03/10/2019 for CHG0161491
*    p_email is INITIAL.
     s_email-low IS INITIAL.
*End of changes by AHMADT on 03/10/2019 for CHG0161491
    WRITE : / 'Input Email address....'.
    STOP.
  ENDIF.
  PERFORM get_data.
  IF r_email IS NOT INITIAL.
    PERFORM send_email.
  ENDIF.
  IF gt_output[] IS NOT INITIAL.
    IF r_email IS NOT INITIAL.
*       PERFORM send_email.
    ELSE.
      PERFORM alv_output.
    ENDIF.
  ELSE.
    WRITE : / 'Security Role: ', p_role1.
    WRITE : / 'There is no conflicting role data to output.'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toggle_functionality .
  LOOP AT SCREEN.
* Set the screen fields to display only
    IF  screen-group1 EQ gc_modif_id_dsp.
      screen-input = 0.
    ENDIF.
    IF r_alv = 'X'.
      IF screen-group1 = 'ALV'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 = 'DSP'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'ALV'.
        screen-input = 0.
      ENDIF.
      IF screen-group1 = 'DSP'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.
ENDFORM.                    " TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: lv_tabix TYPE sy-tabix,
        lt_agr_users1 TYPE TABLE OF agr_users,
        ls_agr_users1 TYPE agr_users,
        lt_agr_users2 TYPE TABLE OF agr_users,
        ls_agr_users2 TYPE agr_users.

  CLEAR: gt_output,
         gs_output.
  SELECT * FROM agr_users INTO TABLE lt_agr_users1
      WHERE agr_name = p_role1
        AND uname IN s_users
        AND to_dat >= sy-datum.

  SELECT * FROM agr_users INTO TABLE lt_agr_users2
      WHERE agr_name IN s_roles
        AND uname IN s_users
        AND to_dat >= sy-datum.
  SORT lt_agr_users1 BY uname agr_name.
  SORT lt_agr_users2 BY uname agr_name.
  LOOP AT lt_agr_users1 INTO ls_agr_users1.
    CLEAR lv_tabix.
    READ TABLE lt_agr_users2 WITH KEY uname = ls_agr_users1-uname
                                           TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_agr_users2 INTO ls_agr_users2 FROM lv_tabix.
      IF ls_agr_users1-uname <> ls_agr_users2-uname.
        EXIT.
      ENDIF.
      gs_output-uname = ls_agr_users1-uname.
      gs_output-role  = ls_agr_users2-agr_name.
      APPEND gs_output TO gt_output.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_email .

  DATA: lt_objtxt    TYPE TABLE OF solisti1,
        lt_objpack   TYPE TABLE OF sopcklsti1,
*Start of changes by AHMADT on 03/10/2019 for CHG0161491
*      lt_reclist   TYPE TABLE OF somlreci1,
        lt_receiver  TYPE TABLE OF somlrec90,
*End of changes by AHMADT on 03/10/2019 for CHG0161491
        lt_objhead   TYPE soli_tab.

  DATA: lv_lines     TYPE i,
        lv_string    TYPE string,
        lwa_objpack  TYPE sopcklsti1,
        lwa_objtxt   TYPE solisti1,
        lwa_doc_chng TYPE sodocchgi1,
*Start of changes by AHMADT on 03/10/2019 for CHG0161491
*        lwa_reclist  TYPE somlreci1,
        lwa_receiver TYPE somlrec90,
*End of changes by AHMADT on 03/10/2019 for CHG0161491
        lv_space     TYPE char01  VALUE ' '.


  CONSTANTS: lc_f(1)      TYPE c VALUE 'F',
             lc_u(1)      TYPE c VALUE 'U',
             lc_int(3)    TYPE c VALUE 'INT',
             lc_htm(3)    TYPE c VALUE 'HTM',
             lc_hyphen(1) TYPE c VALUE '-',
             lc_log(3)    TYPE c VALUE 'LOG',
             lc_x(1)      TYPE c VALUE 'X'.

* Prepare Email Content
  PERFORM build_mail_content CHANGING lt_objtxt.

* Object with main text of the mail.
  lwa_objtxt = lv_space.
  APPEND lwa_objtxt TO lt_objtxt.
  CLEAR lwa_objtxt.

  DESCRIBE TABLE lt_objtxt LINES lv_lines.

  CONCATENATE text-003
              p_role1
              lc_hyphen
              lc_log
              INTO lv_string
              SEPARATED BY space.

  lwa_doc_chng-obj_descr  = lv_string.
  lwa_doc_chng-sensitivty = lc_f.
  lwa_doc_chng-doc_size   = lv_lines * 255.

* Pack to main body.
  lwa_objpack-head_start = 1.
  lwa_objpack-head_num   = 0.
  lwa_objpack-body_start = 1.
  lwa_objpack-body_num   = lv_lines.
  lwa_objpack-doc_type   = lc_htm.
  APPEND lwa_objpack TO lt_objpack.
  CLEAR lwa_objpack.

*Start of changes by AHMADT on 03/10/2019 for CHG0161491
*  lwa_reclist-copy = lc_x.
* Map Email ID(s)
*  lwa_reclist-receiver   = p_email.
*  lwa_reclist-rec_type   = lc_u.
*  lwa_reclist-com_type   = lc_int.
*  lwa_reclist-notif_del  = lc_x.
*  lwa_reclist-notif_ndel = lc_x.
*  lwa_reclist-copy       = space.
*  APPEND lwa_reclist TO lt_reclist.
  LOOP AT s_email.
    lwa_receiver-receiver = s_email-low.
    lwa_receiver-rec_type = 'U'.
    lwa_receiver-express = 'X'.
    APPEND lwa_receiver TO lt_receiver.
  ENDLOOP.
*End of changes by AHMADT on 03/10/2019 for CHG0161491

* Funcion module for sending email.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = lwa_doc_chng
      put_in_outbox              = lc_x
      commit_work                = lc_x
    TABLES
      packing_list               = lt_objpack
      object_header              = lt_objhead
      contents_txt               = lt_objtxt
*Start of changes by AHMADT on 03/10/2019 for CHG0161491
*     receivers                  = lt_reclist
      receivers                  = lt_receiver
*End of changes by AHMADT on 03/10/2019 for CHG0161491
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc <> 0.
    MESSAGE i000(zfi01) WITH text-006.
  ELSE.

**Call program to push mail from SAP mail outbox
*    SUBMIT rsconn01 WITH mode = lc_int
*                    WITH output = space
*                    AND RETURN.
    WRITE: /'Error log is emailed.'.

  ENDIF.
ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  ALV_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_output .
  DATA:     lv_title       TYPE sytitle,
            lv_title2      TYPE sytitle,
            lv_cnt         TYPE i,
            ls_key         TYPE salv_s_layout_key,
            lo_table       TYPE REF TO cl_salv_table,
            lo_layout      TYPE REF TO cl_salv_layout,
            lo_functions   TYPE REF TO cl_salv_functions,
            lo_display     TYPE REF TO cl_salv_display_settings,
            lo_columns     TYPE REF TO cl_salv_columns_table,
            lo_column      TYPE REF TO cl_salv_column_table,
            lo_content     TYPE REF TO cl_salv_form_element,
            lo_grid        TYPE REF TO cl_salv_form_layout_grid,
            lo_events_salv TYPE REF TO cl_salv_events_table.
*          lo_event       TYPE REF TO lcl_event_handler.

  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*          r_container    = lr_con1
*          container_name = 'ALV_CON1'
        IMPORTING
          r_salv_table   = lo_table
        CHANGING
          t_table        = gt_output.
    CATCH cx_salv_msg .
  ENDTRY.
*Function settings
  lo_functions = lo_table->get_functions( ).
  lo_functions->set_all( abap_true ).
*Display Setting
  lo_display = lo_table->get_display_settings( ).

  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
*Event
*  lo_events_salv = lo_table->get_event( ).
*  CREATE OBJECT lo_event.
*  SET HANDLER: lo_event->hotspot_click
*               FOR lo_events_salv.
*Set layout
  lo_layout = lo_table->get_layout( ).
  ls_key-report = sy-repid.
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*  CALL METHOD lo_layout->set_initial_layout
*    EXPORTING
*      value = p_vari.
*Get columns
  CALL METHOD lo_table->get_columns
    RECEIVING
      value = lo_columns.
*****Change ALV Fields  - title etc.
*  PERFORM alv_fields USING lo_columns lo_column.
******Set ALV Header
*... Create top_of_list contents.
  CONCATENATE 'Security Role: ' p_role1 INTO lv_title
                                    SEPARATED BY space.
  CREATE OBJECT lo_grid.
  lo_grid->create_label(
        row    = 1
        column = 1
        text   = lv_title ).

  lv_cnt = 1.
  LOOP AT s_roles.
    IF lv_cnt = 1.
      CONCATENATE 'Conflicting Security Roles: ' s_roles-sign s_roles-option
                                                 s_roles-low INTO lv_title2
                                                 SEPARATED BY space.
    ELSE.
      CONCATENATE s_roles-sign s_roles-option s_roles-low INTO lv_title2
                                                     SEPARATED BY space.
    ENDIF.
    CREATE OBJECT lo_grid.
    lo_grid->create_label(
      row    = 1
      column = 1
      text   = lv_title2 ).
  ENDLOOP.
  "------------------------

  lo_content = lo_grid.
  lo_table->set_top_of_list( lo_content ).
******Display ALV
  CALL METHOD lo_table->display.

ENDFORM.                    " ALV_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_mail_content  CHANGING p_lt_objtxt TYPE table.

  DATA: lwa_objtxt TYPE solisti1,
        lv_cnt     TYPE i.
  "lwa_email_data   TYPE ty_email_data,


*Prepare HTML mail header
  CONCATENATE '<html>'
              '<body>'
              '<h4 style="font-family:arial"><caption><b><u>'
              text-003 p_role1
              '</u></b><caption></h4>'
              '<ul>'
              '</ul>'
         INTO lwa_objtxt
         SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "-----------------------table
  lwa_objtxt = text-011.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td>'
              INTO lwa_objtxt
              SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  CONCATENATE text-008 '</td><td>'
              text-009 '</td><td>'
              text-010 '</td><td>'
              INTO lwa_objtxt
              SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  LOOP AT s_roles.
    CONCATENATE  '<tr> <td>'
          s_roles-sign '</td><td>'
          s_roles-option '</td><td>'
          s_roles-low '</td><td>'
          INTO lwa_objtxt
          SEPARATED BY space.
    APPEND lwa_objtxt TO p_lt_objtxt.
  ENDLOOP.
  MOVE: '</table> <br>' TO lwa_objtxt.
  APPEND: lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "--------------end of table
  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td>'
              INTO lwa_objtxt
              SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE text-006 '</td><td>'
              text-004 '</td><td>'
              text-005 '</td><td>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  IF gt_output[] IS NOT INITIAL.
    LOOP AT gt_output INTO gs_output.
      CONCATENATE  '<tr> <td>'
                  sy-sysid '</td><td>'
                  gs_output-uname '</td><td>'
                  gs_output-role '</td><td>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.
    ENDLOOP.
  ELSE. "no output data
    CONCATENATE  '<tr> <td>'
                    sy-sysid '</td><td>'
                    text-007 '</td><td>'
                    space    '</td><td>'
                    INTO lwa_objtxt
                    SEPARATED BY space.

    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
  ENDIF.
  MOVE: '</table> <br>' TO lwa_objtxt.
  APPEND: lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
*End of HTML text here
  CONCATENATE '</body>'
              '</html>'
         INTO lwa_objtxt
         SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
ENDFORM.                    " BUILD_MAIL_CONTENT
