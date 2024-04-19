*&---------------------------------------------------------------------*
*& Report  Z_TRANSPORTS_WITH_ID_OBJECTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_transports_with_id_objects.
TYPE-POOLS: trwbo.
TABLES: e071.
TYPES: BEGIN OF ty_obj,
       pgmid    TYPE e071-pgmid,
       object   TYPE e071-object,
       obj_name TYPE e071-obj_name,
       strkorr1 TYPE e070-strkorr,
       trkorr1  TYPE e070-trkorr,
       as4user1  TYPE e070-as4user,
       as4date1  TYPE e070-as4date,
       as4time1  TYPE e070-as4time,
       trfunction1(15), " type e070-TRFUNCTION,
       trstatus1(15), " type e070-trstatus,
       strkorr2 TYPE e070-strkorr,
       trkorr2  TYPE e070-trkorr,
       as4user2  TYPE e070-as4user,
       as4date2  TYPE e070-as4date,
       as4time2  TYPE e070-as4time,
       trfunction2(15), " type e070-TRFUNCTION,
       trstatus2(15), " type e070-trstatus,
       END OF ty_obj.
TYPES: BEGIN OF ty_keys,
       pgmid    TYPE e071k-pgmid,
       object   TYPE e071k-object,
       obj_name TYPE e071k-objname,
       tabkey   TYPE e071k-tabkey,
       strkorr1 TYPE e070-strkorr,
       trkorr1  TYPE e070-trkorr,
       as4user1  TYPE  e070-as4user,
       as4date1  TYPE  e070-as4date,
       as4time1  TYPE e070-as4time,
*       tabkey1   TYPE e071k-tabkey,
       strkorr2 TYPE e070-strkorr,
       trkorr2  TYPE e070-trkorr,
       as4user2  TYPE  e070-as4user,
       as4date2  TYPE  e070-as4date,
       as4time2  TYPE e070-as4time,
*       tabkey2   TYPE e071k-tabkey,
       END OF ty_keys.
TYPES: BEGIN OF ty_attributes,
       attribute TYPE e070a-attribute,
       strkorr1  TYPE e070-strkorr,
       trkorr1   TYPE e070-trkorr,
       as4user1   TYPE e070-as4user,
       as4date1   TYPE e070-as4date,
       as4time1   TYPE e070-as4time,
*       reference1 TYPE e070a-reference,
       strkorr2  TYPE e070-strkorr,
       trkorr2   TYPE e070-trkorr,
       as4user2   TYPE e070-as4user,
       as4date2   TYPE e070-as4date,
       as4time2   TYPE e070-as4time,
*       reference2 TYPE e070a-reference,
       END OF ty_attributes.
DATA: gt_obj_output TYPE TABLE OF ty_obj,
      gs_obj_output LIKE LINE OF gt_obj_output,
      gt_key_output TYPE TABLE OF ty_keys,
      gs_key_output LIKE LINE OF gt_key_output,
      gt_attributes_output TYPE TABLE OF ty_attributes,
      gs_attributes_output LIKE LINE OF gt_attributes_output.
DATA: gt_trwbo_requests TYPE trwbo_requests,
      gs_trwbo_requests LIKE LINE OF gt_trwbo_requests,
      gt_trwbo_request_headers TYPE trwbo_request_headers,
      gs_rwbo_request_headers LIKE LINE OF gt_trwbo_request_headers,
      gs_objects    TYPE e071,
      gs_keys       TYPE e071k,
      gs_attributes TYPE e070a,
      gt_objects TYPE TABLE OF e071,
      gt_objects_c TYPE TABLE OF e071,
      gs_objects_c LIKE LINE OF gt_objects_c,
      gt_keys TYPE TABLE OF e071k,
      gt_keys_c TYPE TABLE OF e071k,
      gs_keys_c LIKE LINE OF gt_keys_c,
      gt_attributes TYPE TABLE OF e070a,
      gt_attributes_c TYPE TABLE OF e070a,
      gs_attributes_c LIKE LINE OF gt_attributes_c,
      gt_requests   TYPE trwbo_requests.

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

DATA:BEGIN OF gt_tab OCCURS 0,         "Text file format
           text1(20),
     END OF gt_tab.
DATA: BEGIN OF gt_msg OCCURS 0,
           text1(100),
      END OF gt_msg.
DATA: gt_copy_tab LIKE TABLE OF gt_tab,
      gs_copy_tab LIKE LINE OF gt_tab.
*&---------------------------------------------------------------------*
*&       Class LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: double_click1 FOR EVENT double_click OF cl_salv_events_table
                         IMPORTING row column,
             hotspot_click1 FOR EVENT link_click OF cl_salv_events_table
                         IMPORTING row column,
             double_click2 FOR EVENT double_click OF cl_salv_events_table
                         IMPORTING row column,
             hotspot_click2 FOR EVENT link_click OF cl_salv_events_table
                         IMPORTING row column,
             double_click3 FOR EVENT double_click OF cl_salv_events_table
                         IMPORTING row column,
             hotspot_click3 FOR EVENT link_click OF cl_salv_events_table
                         IMPORTING row column.
ENDCLASS.               "LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_handler
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD double_click1.
    PERFORM handle_click1 USING row column.
  ENDMETHOD.                    "double_click1
  METHOD hotspot_click1.
    PERFORM handle_click1 USING row column.
  ENDMETHOD.                    "hotspot_click1
  METHOD double_click2.
    PERFORM handle_click2 USING row column.
  ENDMETHOD.                    "double_click2
  METHOD hotspot_click2.
    PERFORM handle_click2 USING row column.
  ENDMETHOD.                    "hotspot_click2
  METHOD double_click3.
    PERFORM handle_click3 USING row column.
  ENDMETHOD.                    "double_click3
  METHOD hotspot_click3.
    PERFORM handle_click3 USING row column.
  ENDMETHOD.                    "hotspot_click3
ENDCLASS.               "lcl_event_handler


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*SELECT-OPTIONS : s_tran FOR e071-trkorr.
PARAMETERS: p_rad2 RADIOBUTTON GROUP grp1 DEFAULT 'X',
            p_rad1 RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_obname FOR e071-obj_name.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_file TYPE rfpdo-lboxfile,
            p_sdat TYPE e070-as4date  DEFAULT sy-datum.
*            p_edat type e070-AS4DATE  default sy-datum.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',txt File,*.txt'
      static    = 'X'
    CHANGING
      file_name = p_file.

AT SELECTION-SCREEN ON BLOCK b2.
  IF p_rad1 = 'X' AND
     p_file = space.
    MESSAGE e000(zfi01) WITH 'Enter Text file' '' '' '' .
    STOP.
  ENDIF.

START-OF-SELECTION.

  CLEAR: gt_tab,
         gt_msg,
         gt_obj_output,
         gt_key_output,
         gt_attributes_output,
         gt_trwbo_requests,
         gt_trwbo_request_headers,
         gt_objects,
         gt_objects_c,
         gt_keys,
         gt_keys_c,
         gt_attributes,
         gt_attributes_c,
         gt_requests.
  IF p_rad1 = 'X'.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 5
        text       = 'Uploading Text file'.

    PERFORM upload_from_pc.
  ELSE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 5
        text       = 'Extracting data from database'.
    PERFORM get_data.
  ENDIF.
  IF gt_tab[] IS INITIAL OR
     gt_msg IS NOT INITIAL.
    PERFORM error_messages.
  ELSE.
    IF s_obname IS INITIAL.
*     PERFORM process_transports.
      PERFORM process_transport1.
    ELSE.
*     PERFORM process_transports_ob.
      PERFORM process_transports_ob1.
    ENDIF.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 80
        text       = 'Preparing to display'.
    CALL SCREEN 100.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FROM_PC
*&---------------------------------------------------------------------*
*       Upload file from PC
*----------------------------------------------------------------------*
FORM upload_from_pc .

  DATA:   lt_auszug TYPE STANDARD TABLE OF string.
  DATA:   lv_auszug_file TYPE string.

  REFRESH lt_auszug[].
  lv_auszug_file = p_file.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename        = lv_auszug_file
*      HAS_FIELD_SEPARATOR = 'X'
      filetype        = 'ASC'
    CHANGING
      data_tab        = lt_auszug
    EXCEPTIONS
      file_open_error = 1
      file_read_error = 2
      OTHERS          = 18.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE e503(fv) WITH p_file+2 p_file+0(2) INTO gt_msg-text1.
      APPEND gt_msg.
    WHEN 2.
      MESSAGE e503(fv) WITH p_file+2 p_file+0(2) INTO gt_msg-text1.
      APPEND gt_msg.
    WHEN OTHERS.
  ENDCASE.
  CLEAR gt_copy_tab.
  IF s_obname IS NOT INITIAL.
    gt_copy_tab[] =  lt_auszug[].
    PERFORM extract_objects.
  ELSE.
    gt_tab[] = lt_auszug[].
  ENDIF.

ENDFORM.                    " UPLOAD_FROM_PC
*&---------------------------------------------------------------------*
*&      Form  PROCESS_TRANSPORTS
*&---------------------------------------------------------------------*
*       Process transports and find transports which have identical objects.
*----------------------------------------------------------------------*
FORM process_transports .

*LOOP AT s_tran.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = 'Processing Transports'.

  DATA: lv_trkorr TYPE trkorr,
        lt_dd07t TYPE TABLE OF dd07t,
        ls_dd07t LIKE LINE OF lt_dd07t.
  DATA: lv_chck(1),
        ls_obj_output LIKE LINE OF gt_obj_output,
        ls_key_output LIKE LINE OF gt_key_output.

  SELECT * INTO TABLE lt_dd07t FROM dd07t
                      WHERE ( domname = 'TRSTATUS' OR
                              domname = 'TRFUNCTION' )
                        AND ddlanguage = 'EN'.

  LOOP AT gt_tab.
    lv_trkorr = gt_tab-text1.


    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = lv_trkorr "s_tran-low
      IMPORTING
        et_request_headers = gt_trwbo_request_headers
        et_requests        = gt_trwbo_requests
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    LOOP AT gt_trwbo_requests INTO gs_trwbo_requests.
      CHECK gs_trwbo_requests-h-strkorr <> space.
      LOOP AT gs_trwbo_requests-objects INTO gs_objects.
        APPEND gs_objects TO gt_objects.
      ENDLOOP.
      LOOP AT gs_trwbo_requests-keys INTO gs_keys.
        APPEND gs_keys TO gt_keys.
*           insert gs_keys inTO table gt_keys.
      ENDLOOP.
      LOOP AT gs_trwbo_requests-attributes INTO gs_attributes.
        APPEND gs_attributes TO gt_attributes.
      ENDLOOP.
      APPEND gs_trwbo_requests TO gt_requests.
    ENDLOOP.
  ENDLOOP.
**********Now Read identical objects.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 15
      text       = 'Identifying Identical objects'.

  SORT gt_objects BY  pgmid object obj_name trkorr.
  SORT gt_keys BY  pgmid object objname tabkey trkorr.
  SORT gt_attributes BY  attribute trkorr.
*****************
*  if s_obname is initial.
  DELETE ADJACENT DUPLICATES FROM gt_objects COMPARING pgmid object obj_name trkorr.
  DELETE ADJACENT DUPLICATES FROM gt_keys COMPARING pgmid object objname tabkey trkorr.
  DELETE ADJACENT DUPLICATES FROM gt_attributes COMPARING attribute trkorr.
*  endif.
******************
  gt_objects_c[] = gt_objects.
  gt_keys_c[] = gt_keys[].
  gt_attributes_c[] = gt_attributes[].

  LOOP AT gt_objects INTO gs_objects.
    READ TABLE gt_objects_c WITH KEY pgmid = gs_objects-pgmid
                                    object = gs_objects-object
                                    obj_name = gs_objects-obj_name TRANSPORTING NO FIELDS.
    LOOP AT gt_objects_c INTO gs_objects_c
                         WHERE pgmid = gs_objects-pgmid
                           AND object = gs_objects-object
                           AND obj_name = gs_objects-obj_name.
      CHECK gs_objects_c-trkorr <> gs_objects-trkorr.
*      READ TABLE gt_obj_output WITH KEY trkorr1 = gs_objects-trkorr   "1
*                                        trkorr2 = gs_objects_c-trkorr "2
*                                        pgmid = gs_objects_c-pgmid
*                                        object = gs_objects_c-object
*                                        obj_name = gs_objects_c-obj_name TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
*      READ TABLE gt_obj_output WITH KEY trkorr1 = gs_objects_c-trkorr "2
*                                        trkorr2 = gs_objects-trkorr   "1
*                                        pgmid = gs_objects_c-pgmid
*                                        object = gs_objects_c-object
*                                        obj_name = gs_objects_c-obj_name TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
      CLEAR lv_chck.
      READ TABLE gt_obj_output WITH KEY pgmid = gs_objects_c-pgmid
                                        object = gs_objects_c-object
                                        obj_name = gs_objects_c-obj_name TRANSPORTING NO FIELDS.
      LOOP AT gt_obj_output INTO ls_obj_output WHERE pgmid = gs_objects_c-pgmid
                                                 AND object = gs_objects_c-object
                                                 AND obj_name = gs_objects_c-obj_name.
        IF ls_obj_output-trkorr1 = gs_objects-trkorr AND
           ls_obj_output-trkorr2 = gs_objects_c-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
        IF ls_obj_output-trkorr1 = gs_objects_c-trkorr AND
           ls_obj_output-trkorr2 = gs_objects-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
*if TR is not in Col1 then check Col2. if it is there no need to proceed.
        IF ls_obj_output-trkorr1 <> gs_objects-trkorr AND
           ls_obj_output-trkorr2 = gs_objects-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.

      ENDLOOP.
      CHECK lv_chck <> 'X'.

      CLEAR: gs_trwbo_requests,
             gs_obj_output.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_objects-trkorr.
      gs_obj_output-as4user1 = gs_trwbo_requests-h-as4user.
      gs_obj_output-as4date1 = gs_trwbo_requests-h-as4date.
      gs_obj_output-as4time1 = gs_trwbo_requests-h-as4time.
      gs_obj_output-strkorr1 = gs_trwbo_requests-h-strkorr.
      gs_obj_output-trkorr1 = gs_objects-trkorr.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY domname    =  'TRSTATUS'
                                                 domvalue_l = gs_trwbo_requests-h-trstatus.
      gs_obj_output-trstatus1  = ls_dd07t-ddtext.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY  domname = 'TRFUNCTION'
                                                 domvalue_l = gs_trwbo_requests-h-trfunction.
      gs_obj_output-trfunction1  = ls_dd07t-ddtext.

      CLEAR gs_trwbo_requests.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_objects_c-trkorr.
      gs_obj_output-as4user2 = gs_trwbo_requests-h-as4user.
      gs_obj_output-as4date2 = gs_trwbo_requests-h-as4date.
      gs_obj_output-as4time2 = gs_trwbo_requests-h-as4time.
      gs_obj_output-strkorr2 = gs_trwbo_requests-h-strkorr.
      gs_obj_output-trkorr2 = gs_objects_c-trkorr.
      gs_obj_output-pgmid = gs_objects_c-pgmid.
      gs_obj_output-object = gs_objects_c-object.
      gs_obj_output-obj_name = gs_objects_c-obj_name.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY domname    =  'TRSTATUS'
                                                 domvalue_l = gs_trwbo_requests-h-trstatus.
      gs_obj_output-trstatus2  = ls_dd07t-ddtext.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY  domname = 'TRFUNCTION'
                                                 domvalue_l = gs_trwbo_requests-h-trfunction.
      gs_obj_output-trfunction2  = ls_dd07t-ddtext.
      APPEND gs_obj_output TO gt_obj_output.
    ENDLOOP.
  ENDLOOP.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 20
      text       = 'Identifying identical Keys'.
  DATA: lv_tabix TYPE sy-tabix.
  LOOP AT gt_keys INTO gs_keys.

    READ TABLE gt_keys_c WITH KEY pgmid = gs_keys-pgmid
                                  object = gs_keys-object
                                  objname = gs_keys-objname
                                  tabkey = gs_keys-tabkey BINARY SEARCH TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT gt_keys_c INTO gs_keys_c  FROM lv_tabix.
*                                        WHERE pgmid = gs_keys-pgmid
*                                        AND object = gs_keys-object
*                                        AND objname = gs_keys-objname
*                                        and tabkey = gs_keys-tabkey.
      IF gs_keys_c-pgmid <> gs_keys-pgmid OR
         gs_keys_c-object <> gs_keys-object OR
         gs_keys_c-objname <> gs_keys-objname OR
         gs_keys_c-tabkey <> gs_keys-tabkey.
        EXIT.
      ENDIF.
      CHECK gs_keys_c-trkorr <> gs_keys-trkorr.
**************
*      READ TABLE gt_key_output WITH KEY trkorr1 = gs_keys-trkorr   "1
*                                          trkorr2 = gs_keys_c-trkorr "2
*                                          pgmid = gs_keys_c-pgmid
*                                          object = gs_keys_c-object
*                                          obj_name = gs_keys_c-objname
*                                          tabkey = gs_keys_c-tabkey TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
*      READ TABLE gt_key_output WITH KEY   trkorr1 = gs_keys_c-trkorr   "2
*                                          trkorr2 = gs_keys-trkorr "1
*                                          pgmid = gs_keys_c-pgmid
*                                          object = gs_keys_c-object
*                                          obj_name = gs_keys_c-objname
*                                          tabkey = gs_keys_c-tabkey TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
*************
      CLEAR lv_chck.
      READ TABLE gt_key_output WITH KEY pgmid = gs_keys_c-pgmid
                                        object = gs_keys_c-object
                                        obj_name = gs_keys_c-objname
                                        tabkey = gs_keys_c-tabkey TRANSPORTING NO FIELDS.

      LOOP AT gt_key_output INTO ls_key_output WHERE pgmid = gs_keys_c-pgmid
                                                 AND object = gs_keys_c-object
                                                 AND obj_name = gs_keys_c-objname
                                                 AND tabkey = gs_keys_c-tabkey.
        IF ls_key_output-trkorr1 = gs_keys-trkorr AND
           ls_key_output-trkorr2 = gs_keys_c-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
        IF ls_key_output-trkorr1 = gs_keys_c-trkorr AND
           ls_key_output-trkorr2 = gs_keys-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
*if TR is not in Col1 then check Col2. if it is there no need to proceed.
        IF ls_key_output-trkorr1 <> gs_keys-trkorr AND
           ls_key_output-trkorr2 = gs_keys-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.

      ENDLOOP.
      CHECK lv_chck <> 'X'.
      CLEAR: gs_trwbo_requests,
             gs_key_output.
      gs_key_output-pgmid = gs_keys_c-pgmid.
      gs_key_output-object = gs_keys_c-object.
      gs_key_output-obj_name = gs_keys_c-objname.
      gs_key_output-tabkey = gs_keys_c-tabkey.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_keys-trkorr.
      gs_key_output-as4user1 = gs_trwbo_requests-h-as4user.
      gs_key_output-as4date1 = gs_trwbo_requests-h-as4date.
      gs_key_output-as4time1 = gs_trwbo_requests-h-as4time.
      gs_key_output-strkorr1 = gs_trwbo_requests-h-strkorr.
      gs_key_output-trkorr1 = gs_keys-trkorr.
*      gs_key_output-tabkey1   = gs_keys-tabkey.
      CLEAR gs_trwbo_requests.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_keys_c-trkorr.
      gs_key_output-as4user2 = gs_trwbo_requests-h-as4user.
      gs_key_output-as4date2 = gs_trwbo_requests-h-as4date.
      gs_key_output-as4time2 = gs_trwbo_requests-h-as4time.
      gs_key_output-strkorr2 = gs_trwbo_requests-h-strkorr.
      gs_key_output-trkorr2 = gs_keys_c-trkorr.
*      gs_key_output-tabkey2   = gs_keys_c-tabkey.
      APPEND gs_key_output TO gt_key_output.
    ENDLOOP.
  ENDLOOP.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Identifying identical attributes'.
  LOOP AT gt_attributes INTO gs_attributes.
    READ TABLE gt_attributes_c WITH KEY  attribute = gs_attributes-attribute
                                         TRANSPORTING NO FIELDS.
    LOOP AT gt_attributes_c INTO gs_attributes_c
                         WHERE attribute = gs_attributes-attribute.
      CHECK gs_attributes_c-trkorr <> gs_attributes-trkorr.
      READ TABLE gt_attributes_output WITH KEY
                                           trkorr1 = gs_attributes-trkorr   "1
                                           trkorr2 = gs_attributes_c-trkorr "2
                                           attribute = gs_attributes_c-attribute TRANSPORTING NO FIELDS.
      CHECK sy-subrc <> 0.
      READ TABLE gt_attributes_output WITH KEY
                                           trkorr1 = gs_attributes_c-trkorr   "2
                                           trkorr2 = gs_attributes-trkorr "1
                                           attribute = gs_attributes_c-attribute TRANSPORTING NO FIELDS.
      CHECK sy-subrc <> 0.
      CLEAR: gs_trwbo_requests,
             gs_attributes_output.
      gs_attributes_output-attribute = gs_attributes_c-attribute.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_attributes-trkorr.
      gs_attributes_output-as4user1 = gs_trwbo_requests-h-as4user.
      gs_attributes_output-as4date1 = gs_trwbo_requests-h-as4date.
      gs_attributes_output-as4time1 = gs_trwbo_requests-h-as4time.
      gs_attributes_output-strkorr1 = gs_trwbo_requests-h-strkorr.
      gs_attributes_output-trkorr1 = gs_attributes-trkorr.
*      gs_attributes_output-reference1 = gs_attributes-reference.
      CLEAR  gs_trwbo_requests.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_attributes_c-trkorr.
      gs_attributes_output-as4user2 = gs_trwbo_requests-h-as4user.
      gs_attributes_output-as4date2 = gs_trwbo_requests-h-as4date.
      gs_attributes_output-as4time2 = gs_trwbo_requests-h-as4time.
      gs_attributes_output-strkorr2 = gs_trwbo_requests-h-strkorr.
      gs_attributes_output-trkorr2 = gs_attributes_c-trkorr.
*      gs_attributes_output-reference2 = gs_attributes_c-reference.
      APPEND gs_attributes_output TO gt_attributes_output.

    ENDLOOP.
  ENDLOOP.

*objects (table E071 - Primary Key: TRKORR + AS4POS)
*	TRKORR, AS4POS, PGMID, OBJECT, OBJ_NAME, OBJFUNC, LOCKFLAG, GENNUM, LANG, ACTIVITY
*KEYS (table E071K - Primary Key: TRKORR, PGMID, OBJECT, OBJNAME, AS4POS)
*
*ATTRIBUTES (table: E070A, Primary Key: TRKORR,POS)
*	TRKORR, POS, ATTRIBUTE, REFERENCE
ENDFORM.                    " PROCESS_TRANSPORTS
*&---------------------------------------------------------------------*
*&      Form  ERROR_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM error_messages .

  IF gt_tab[] IS INITIAL.
    WRITE: / 'No lines for processing.'..
  ENDIF.
  IF gt_msg[] IS NOT INITIAL.
    WRITE: / 'Following errors occured during processing.'.
    LOOP AT gt_msg.
      WRITE: / '->', gt_msg-text1.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " ERROR_MESSAGES
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STAT_01'.
  SET TITLEBAR 'T01'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  DATA: lr_table1 TYPE REF TO cl_salv_table,
        lr_table2 TYPE REF TO cl_salv_table,
        lr_table3 TYPE REF TO cl_salv_table,
        lr_functions TYPE REF TO cl_salv_functions,
        lr_con1 TYPE REF TO cl_gui_custom_container,
        lr_con2 TYPE REF TO cl_gui_custom_container,
        lr_con3 TYPE REF TO cl_gui_custom_container,
        lr_display TYPE REF TO cl_salv_display_settings,
        lr_event TYPE REF TO lcl_event_handler,
        lr_events_salv TYPE REF TO cl_salv_events_table,
        lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column TYPE REF TO cl_salv_column_table,
        lv_lines TYPE n LENGTH 5,
        lv_msg TYPE lvc_title.
******************Container 1*************************
  CREATE OBJECT lr_con1
    EXPORTING
*    parent                      =
      container_name              = 'ALV_CON1'
*    style                       =
*    lifetime                    = lifetime_default
*    repid                       =
*    dynnr                       =
*    no_autodef_progid_dynnr     =
*  EXCEPTIONS
*    cntl_error                  = 1
*    cntl_system_error           = 2
*    create_error                = 3
*    lifetime_error              = 4
*    lifetime_dynpro_dynpro_link = 5
*    others                      = 6
      .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
          r_container    = lr_con1
          container_name = 'ALV_CON1'
        IMPORTING
          r_salv_table   = lr_table1
        CHANGING
          t_table        = gt_obj_output.
    CATCH cx_salv_msg .
  ENDTRY.
*Function settings
  lr_functions = lr_table1->get_functions( ).
  lr_functions->set_all( abap_true ).
*Display Setting
  DESCRIBE TABLE gt_obj_output LINES lv_lines.
  CONCATENATE 'Transports with Identical Objects, Total rows ' lv_lines INTO lv_msg RESPECTING BLANKS.
  lr_display = lr_table1->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display->set_list_header( lv_msg ).
*Event
  lr_events_salv = lr_table1->get_event( ).
  CREATE OBJECT lr_event.
  SET HANDLER: lr_event->double_click1
               lr_event->hotspot_click1
               FOR lr_events_salv.
*Hotspot
  CALL METHOD lr_table1->get_columns
    RECEIVING
      value = lr_columns.
  TRY.
      lr_column ?= lr_columns->get_column( 'STRKORR1' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.
  TRY.
      lr_column ?= lr_columns->get_column( 'STRKORR2' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.
  TRY.
      lr_column ?= lr_columns->get_column( 'TRKORR2' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.
  TRY.
      lr_column ?= lr_columns->get_column( 'TRKORR1' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.
*Set the column Header
  TRY.
      lr_column ?= lr_columns->get_column( 'TRFUNCTION1' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_long_text
    EXPORTING
      value = 'Type'.
  CALL METHOD lr_column->set_short_text
    EXPORTING
      value = 'Type'.

  TRY.
      lr_column ?= lr_columns->get_column( 'TRSTATUS1' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_long_text
    EXPORTING
      value = 'Status'.
  CALL METHOD lr_column->set_short_text
    EXPORTING
      value = 'Status'.

  TRY.
      lr_column ?= lr_columns->get_column( 'TRFUNCTION2' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_long_text
    EXPORTING
      value = 'Type'.
  CALL METHOD lr_column->set_short_text
    EXPORTING
      value = 'Type'.

  TRY.
      lr_column ?= lr_columns->get_column( 'TRSTATUS2' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_long_text
    EXPORTING
      value = 'Status'.
  CALL METHOD lr_column->set_short_text
    EXPORTING
      value = 'Status'.
*******output length
  TRY.
      lr_column ?= lr_columns->get_column( 'OBJ_NAME' ).
      CALL METHOD lr_column->set_output_length
        EXPORTING
          value = 40.
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_table1->display.
******************Container 2*************************
  CREATE OBJECT lr_con2
    EXPORTING
*    parent                      =
      container_name              = 'ALV_CON2'
*    style                       =
*    lifetime                    = lifetime_default
*    repid                       =
*    dynnr                       =
*    no_autodef_progid_dynnr     =
*  EXCEPTIONS
*    cntl_error                  = 1
*    cntl_system_error           = 2
*    create_error                = 3
*    lifetime_error              = 4
*    lifetime_dynpro_dynpro_link = 5
*    others                      = 6
      .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
          r_container    = lr_con2
          container_name = 'ALV_CON2'
        IMPORTING
          r_salv_table   = lr_table2
        CHANGING
          t_table        = gt_key_output.
    CATCH cx_salv_msg .
  ENDTRY.
*Functions
  lr_functions = lr_table2->get_functions( ).
  lr_functions->set_all( abap_true ).
*Display Setting
  lr_display = lr_table2->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display->set_list_header('Transports with Identical Keys').
**Event
  lr_events_salv = lr_table2->get_event( ).
  CREATE OBJECT lr_event.
  SET HANDLER: lr_event->double_click2
               lr_event->hotspot_click2
               FOR lr_events_salv.
*Hotspot
  CALL METHOD lr_table2->get_columns
    RECEIVING
      value = lr_columns.
  TRY.
      lr_column ?= lr_columns->get_column( 'STRKORR1' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.
  TRY.
      lr_column ?= lr_columns->get_column( 'STRKORR2' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      CALL METHOD lr_column->set_cell_type
        EXPORTING
          value = if_salv_c_cell_type=>hotspot.
      lr_column ?= lr_columns->get_column( 'TRKORR2' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.
  TRY.
      lr_column ?= lr_columns->get_column( 'TRKORR1' ).
    CATCH cx_salv_not_found .
  ENDTRY.
  CALL METHOD lr_column->set_cell_type
    EXPORTING
      value = if_salv_c_cell_type=>hotspot.
*******output length
  TRY.
      lr_column ?= lr_columns->get_column( 'TABKEY' ).
      CALL METHOD lr_column->set_output_length
        EXPORTING
          value = 30.
     CALL METHOD lr_column->set_cell_type
    EXPORTING
      value  = IF_SALV_C_CELL_TYPE=>hotspot.
    CATCH cx_salv_not_found .
  ENDTRY.


  CALL METHOD lr_table2->display.
******************Container 3*************************
*CREATE OBJECT lr_con3
*  EXPORTING
**    parent                      =
*    container_name              = 'ALV_CON3'
**    style                       =
**    lifetime                    = lifetime_default
**    repid                       =
**    dynnr                       =
**    no_autodef_progid_dynnr     =
**  EXCEPTIONS
**    cntl_error                  = 1
**    cntl_system_error           = 2
**    create_error                = 3
**    lifetime_error              = 4
**    lifetime_dynpro_dynpro_link = 5
**    others                      = 6
*    .
*IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*
*TRY.
*CALL METHOD cl_salv_table=>factory
*  EXPORTING
**    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*    r_container    = lr_con3
*    container_name = 'ALV_CON3'
*  IMPORTING
*    r_salv_table   = lr_table3
*  CHANGING
*    t_table        = gt_attributes_output.
* CATCH cx_salv_msg .
*ENDTRY.
**Functions
*lr_functions = lr_table3->get_functions( ).
*lr_functions->set_all( abap_true ).
**Display Setting
* lr_display = lr_table3->get_display_settings( ).
* lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
* lr_display->set_list_header('Transports with Identical Attributes').
***Event
*   lr_events_salv = lr_table3->get_event( ).
*   CREATE OBJECT lr_event.
*   set handler: lr_event->double_click3
*                lr_event->hotspot_click3
*                for lr_events_salv.
**Hotspot
*  CALL METHOD lr_table3->get_columns
*    receiving
*      value  = lr_columns.
*  TRY.
*    lr_column ?= lr_columns->get_column( 'STRKORR1' ).
*  CATCH cx_salv_not_found .
*  ENDTRY.
*  CALL METHOD lr_column->set_cell_type
*    EXPORTING
*      value  = IF_SALV_C_CELL_TYPE=>hotspot.
*  TRY.
*    lr_column ?= lr_columns->get_column( 'STRKORR2' ).
*  CATCH cx_salv_not_found .
*  ENDTRY.
*  try.
*  CALL METHOD lr_column->set_cell_type
*    EXPORTING
*      value  = IF_SALV_C_CELL_TYPE=>hotspot.
*    lr_column ?= lr_columns->get_column( 'TRKORR2' ).
*
*  CATCH cx_salv_not_found .
*  ENDTRY.
*  CALL METHOD lr_column->set_cell_type
*    EXPORTING
*      value  = IF_SALV_C_CELL_TYPE=>hotspot.
*  TRY.
*    lr_column ?= lr_columns->get_column( 'TRKORR1' ).
*  CATCH cx_salv_not_found .
*  ENDTRY.
*  CALL METHOD lr_column->set_cell_type
*    EXPORTING
*      value  = IF_SALV_C_CELL_TYPE=>hotspot.
*********
*CALL METHOD lr_table3->display.
ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  HANDLE_CLICK1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_click1  USING    p_row TYPE salv_de_row
                             p_column TYPE salv_de_column.
*Read the talbe with row-id .
  DATA: lv_trkorr TYPE e070-trkorr,
        ls_output LIKE LINE OF gt_obj_output.
  READ TABLE gt_obj_output INTO ls_output INDEX p_row.
  CASE p_column.
    WHEN 'STRKORR1'.
      lv_trkorr = ls_output-strkorr1.
    WHEN 'STRKORR2'.
      lv_trkorr = ls_output-strkorr2.
    WHEN 'TRKORR1'.
      lv_trkorr = ls_output-trkorr1.
    WHEN 'TRKORR2'.
      lv_trkorr = ls_output-trkorr2.
  ENDCASE.
  CHECK lv_trkorr IS NOT INITIAL.
  CALL FUNCTION 'TR_PRESENT_REQUEST'
   EXPORTING
     iv_trkorr          = lv_trkorr
*   IV_HIGHLIGHT       = 'X'
*   IS_POPUP           =
*   IV_SHOWONLY        = ' '
            .

ENDFORM.                    " HANDLE_CLICK1
*&---------------------------------------------------------------------*
*&      Form  HANDLE_CLICK2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_click2  USING    p_row TYPE salv_de_row
                             p_column TYPE salv_de_column.
  DATA: lv_trkorr TYPE e070-trkorr,
        ls_output LIKE LINE OF gt_key_output,
        lv_chck.
  DATA: ls_key          TYPE e071k,
        lt_keys         TYPE tr_keys,
        ps_object          TYPE e071,
        lv_answer.

DATA: lv_display_mode TYPE c LENGTH 20,
      lv_table        TYPE dd02l-tabname.

  READ TABLE gt_key_output INTO ls_output INDEX p_row.
  CASE p_column.
    WHEN 'STRKORR1'.
      lv_trkorr = ls_output-strkorr1.
    WHEN 'STRKORR2'.
      lv_trkorr = ls_output-strkorr2.
    WHEN 'TRKORR1'.
      lv_trkorr = ls_output-trkorr1.
    WHEN 'TRKORR2'.
      lv_trkorr = ls_output-trkorr2.
    WHEN 'TABKEY'.
      lv_chck = 'X'.
  ENDCASE.
IF LV_CHCK = 'X'.
*object: D30K916134          000001R3TRTABUTVARVC                                                                                                                  K
*BREAK SAHMAD.

if ls_output-object = 'TABU'.
   CALL FUNCTION 'POPUP_TO_CONFIRM'
     EXPORTING
*      TITLEBAR                    = 'Select Transport'
*      DIAGNOSE_OBJECT             = ' '
       text_question               = 'Please Select one of the transports'
      TEXT_BUTTON_1               = ls_output-strkorr1
*      ICON_BUTTON_1               = ' '
      TEXT_BUTTON_2               = ls_output-strkorr2
*      ICON_BUTTON_2               = ' '
*      DEFAULT_BUTTON              = '1'
       DISPLAY_CANCEL_BUTTON       = ' ' "'X'
*      USERDEFINED_F1_HELP         = ' '
*      START_COLUMN                = 25
*      START_ROW                   = 6
*      POPUP_TYPE                  =
*      IV_QUICKINFO_BUTTON_1       = ' '
*      IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      ANSWER                      = lv_answer
*    TABLES
*      PARAMETER                   =
*    EXCEPTIONS
*      TEXT_NOT_FOUND              = 1
*      OTHERS                      = 2
             .
   IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
   clear: ls_key, lt_keys, ps_object.
if lv_answer = '1'.
    SELECT SINGLE * FROM E071K INTO LS_KEY WHERE TRKORR = LS_OUTPUT-TRKORR1
                                             AND PGMID  = LS_OUTPUT-PGMID
                                             AND OBJECT  = LS_OUTPUT-OBJECT
                                             AND OBJNAME = LS_OUTPUT-OBJ_NAME
                                             AND TABKEY = LS_OUTPUT-TABKEY.
    SELECT * FROM e071K INTO TABLE LT_KEYS WHERE TRKORR = LS_OUTPUT-TRKORR1.
    SELECT SINGLE * INTO PS_OBJECT FROM E071 WHERE TRKORR = LS_OUTPUT-TRKORR1
                                               AND PGMID  = LS_OUTPUT-PGMID
                                             AND OBJECT  = LS_key-MASTERTYPE
                                             AND OBJ_NAME = LS_key-MASTERNAME.
else.
  SELECT SINGLE * FROM E071K INTO LS_KEY WHERE TRKORR = LS_OUTPUT-TRKORR1
                                             AND PGMID  = LS_OUTPUT-PGMID
                                             AND OBJECT  = LS_OUTPUT-OBJECT
                                             AND OBJNAME = LS_OUTPUT-OBJ_NAME
                                             AND TABKEY = LS_OUTPUT-TABKEY.
    SELECT * FROM e071K INTO TABLE LT_KEYS WHERE TRKORR = LS_OUTPUT-TRKORR1.
    SELECT SINGLE * INTO PS_OBJECT FROM E071 WHERE TRKORR = LS_OUTPUT-TRKORR1
                                               AND PGMID  = LS_OUTPUT-PGMID
                                             AND OBJECT  = LS_key-MASTERTYPE
                                             AND OBJ_NAME = LS_key-MASTERNAME.
endif.
ps_object-object = ls_output-object.


lv_display_mode = 'SINGLE_KEY'.

lv_table = ps_object-obj_name.

  CALL FUNCTION 'TRINT_DISPLAY_TABLE_CONTENTS'
       EXPORTING
            iv_display_mode = lv_display_mode
            iv_table        = lv_table
            it_keys         = lt_keys
            iv_tabkey       = ls_key-tabkey
       EXCEPTIONS
            OTHERS          = 1.
  IF sy-subrc <> 0.
     MESSAGE s000(zfi01) WITH 'No table contents' '' '' '' .
  ENDIF.

*CALL FUNCTION 'TRKLE_SHOW_TABLE_CONTENT'
*    EXPORTING
*      is_object      = ps_object
*      it_keys        = lt_keys
*      is_current_key = ls_key
*    EXCEPTIONS
*      invalid_key    = 1
*      OTHERS         = 2.
else.
  MESSAGE e000(zfi01) WITH 'Only Table objects can have table contents' '' '' '' .
endif.
ELSE.

  CHECK lv_trkorr IS NOT INITIAL.
  CALL FUNCTION 'TR_PRESENT_REQUEST'
   EXPORTING
     iv_trkorr          = lv_trkorr
*       IV_HIGHLIGHT       = 'X'
*       IS_POPUP           =
*       IV_SHOWONLY        = ' '
            .
ENDIF.
ENDFORM.                    " HANDLE_CLICK2
*&---------------------------------------------------------------------*
*&      Form  HANDLE_CLICK3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_click3  USING    p_row TYPE salv_de_row
                             p_column TYPE salv_de_column.
  DATA: lv_trkorr TYPE e070-trkorr,
        ls_output LIKE LINE OF gt_attributes_output.
  READ TABLE gt_attributes_output INTO ls_output INDEX p_row.
  CASE p_column.
    WHEN 'STRKORR1'.
      lv_trkorr = ls_output-strkorr1.
    WHEN 'STRKORR2'.
      lv_trkorr = ls_output-strkorr2.
  ENDCASE.
  CHECK lv_trkorr IS NOT INITIAL.
  CALL FUNCTION 'TR_PRESENT_REQUEST'
   EXPORTING
     iv_trkorr          = lv_trkorr
*   IV_HIGHLIGHT       = 'X'
*   IS_POPUP           =
*   IV_SHOWONLY        = ' '
            .
ENDFORM.                    " HANDLE_CLICK3
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get transports from database
*----------------------------------------------------------------------*
FORM get_data .
  DATA: lt_e070 TYPE TABLE OF e070,
        ls_e070 LIKE LINE OF lt_e070,
        lt_e071 TYPE TABLE OF e071,
        ls_e071 LIKE LINE OF lt_e071.
  DATA: lv_obname TYPE e071-obj_name.

  SELECT * INTO TABLE lt_e070 FROM e070
                              WHERE strkorr = space
                                AND  as4date >= p_sdat." and
  "AS4DATE <= p_edat ).
  IF s_obname IS INITIAL.
    LOOP AT lt_e070 INTO ls_e070.
      gt_tab-text1 = ls_e070-trkorr.
      APPEND gt_tab.
    ENDLOOP.
  ELSE.
    LOOP AT lt_e070 INTO ls_e070.
      CLEAR: lt_e071.
      SELECT * INTO TABLE lt_e071 FROM e071 WHERE trkorr = ls_e070-trkorr
                                              AND obj_name IN s_obname.
      CHECK sy-subrc = 0.
*         loop at lt_e071 into ls_e071.
*              gt_tab-text1 = ls_e071-TRKORR.
*              append gt_tab.
*         endloop.
      gt_tab-text1 = ls_e070-trkorr.
      APPEND gt_tab.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_OBJECTS
*&---------------------------------------------------------------------*
*       Process objects
*----------------------------------------------------------------------*
FORM extract_objects .
  DATA: lv_trkorr TYPE e071-trkorr,
        lv_obname TYPE e071-obj_name,
        lt_e071 TYPE TABLE OF e071,
        ls_e071 LIKE LINE OF lt_e071.
  LOOP AT gt_copy_tab INTO gs_copy_tab.
    CLEAR: lt_e071.
    lv_trkorr = gs_copy_tab-text1.
    SELECT * INTO TABLE lt_e071 FROM e071 WHERE trkorr = lv_trkorr
                                            AND obj_name IN s_obname.
    CHECK sy-subrc = 0.
    APPEND gs_copy_tab TO gt_tab.

  ENDLOOP.
ENDFORM.                    " EXTRACT_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  PROCESS_TRANSPORTS_OB
*&---------------------------------------------------------------------*
*       Process Transports for objects
*----------------------------------------------------------------------*
FORM process_transports_ob .

*LOOP AT s_tran.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = 'Processing Transports'.

  DATA: lv_trkorr TYPE trkorr,
        lt_dd07t TYPE TABLE OF dd07t,
        ls_dd07t LIKE LINE OF lt_dd07t.
  DATA: ls_key_output LIKE LINE OF gt_key_output,
        lv_chck(1),
        ls_obj_output LIKE LINE OF gt_obj_output.
  SELECT * INTO TABLE lt_dd07t FROM dd07t
                      WHERE ( domname = 'TRSTATUS' OR
                              domname = 'TRFUNCTION' )
                        AND ddlanguage = 'EN'.

  LOOP AT gt_tab.
    lv_trkorr = gt_tab-text1.


    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = lv_trkorr "s_tran-low
      IMPORTING
        et_request_headers = gt_trwbo_request_headers
        et_requests        = gt_trwbo_requests
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    LOOP AT gt_trwbo_requests INTO gs_trwbo_requests.
      CHECK gs_trwbo_requests-h-strkorr <> space.
      LOOP AT gs_trwbo_requests-objects INTO gs_objects.
        APPEND gs_objects TO gt_objects.
      ENDLOOP.
      LOOP AT gs_trwbo_requests-keys INTO gs_keys.
        APPEND gs_keys TO gt_keys.
      ENDLOOP.
      LOOP AT gs_trwbo_requests-attributes INTO gs_attributes.
        APPEND gs_attributes TO gt_attributes.
      ENDLOOP.
      APPEND gs_trwbo_requests TO gt_requests.
    ENDLOOP.
  ENDLOOP.
**********Now Read identical objects.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 15
      text       = 'Identifying Identical objects'.

  SORT gt_objects BY  obj_name trkorr.
*  sort gt_keys by  objname tabkey trkorr.
  SORT gt_attributes BY  attribute trkorr.
*****************
******************
  gt_objects_c[] = gt_objects.
  gt_keys_c[] = gt_keys[].
  gt_attributes_c[] = gt_attributes[].

  LOOP AT gt_objects INTO gs_objects.
    READ TABLE gt_objects_c WITH KEY obj_name = gs_objects-obj_name TRANSPORTING NO FIELDS.
    LOOP AT gt_objects_c INTO gs_objects_c
                         WHERE obj_name = gs_objects-obj_name.
      CHECK gs_objects_c-trkorr <> gs_objects-trkorr.
      CLEAR lv_chck.
      READ TABLE gt_obj_output WITH KEY obj_name = gs_objects_c-obj_name TRANSPORTING NO FIELDS.
      LOOP AT gt_obj_output INTO ls_obj_output WHERE obj_name = gs_objects_c-obj_name.
        IF ls_obj_output-trkorr1 = gs_objects-trkorr AND
           ls_obj_output-trkorr2 = gs_objects_c-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
        IF ls_obj_output-trkorr1 = gs_objects_c-trkorr AND
           ls_obj_output-trkorr2 = gs_objects-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
*if TR is not in Col1 then check Col2. if it is there no need to proceed.
        IF ls_obj_output-trkorr1 <> gs_objects-trkorr AND
           ls_obj_output-trkorr2 = gs_objects-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.

      ENDLOOP.
      CHECK lv_chck <> 'X'.

      CLEAR: gs_trwbo_requests,
             gs_obj_output.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_objects-trkorr.
      gs_obj_output-as4user1 = gs_trwbo_requests-h-as4user.
      gs_obj_output-as4date1 = gs_trwbo_requests-h-as4date.
      gs_obj_output-as4time1 = gs_trwbo_requests-h-as4time.
      gs_obj_output-strkorr1 = gs_trwbo_requests-h-strkorr.
      gs_obj_output-trkorr1 = gs_objects-trkorr.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY domname    =  'TRSTATUS'
                                                 domvalue_l = gs_trwbo_requests-h-trstatus.
      gs_obj_output-trstatus1  = ls_dd07t-ddtext.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY  domname = 'TRFUNCTION'
                                                 domvalue_l = gs_trwbo_requests-h-trfunction.
      gs_obj_output-trfunction1  = ls_dd07t-ddtext.

      CLEAR gs_trwbo_requests.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_objects_c-trkorr.
      gs_obj_output-as4user2 = gs_trwbo_requests-h-as4user.
      gs_obj_output-as4date2 = gs_trwbo_requests-h-as4date.
      gs_obj_output-as4time2 = gs_trwbo_requests-h-as4time.
      gs_obj_output-strkorr2 = gs_trwbo_requests-h-strkorr.
      gs_obj_output-trkorr2 = gs_objects_c-trkorr.
      gs_obj_output-pgmid = gs_objects_c-pgmid.
      gs_obj_output-object = gs_objects_c-object.
      gs_obj_output-obj_name = gs_objects_c-obj_name.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY domname    =  'TRSTATUS'
                                                 domvalue_l = gs_trwbo_requests-h-trstatus.
      gs_obj_output-trstatus2  = ls_dd07t-ddtext.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY  domname = 'TRFUNCTION'
                                                 domvalue_l = gs_trwbo_requests-h-trfunction.
      gs_obj_output-trfunction2  = ls_dd07t-ddtext.
      APPEND gs_obj_output TO gt_obj_output.
    ENDLOOP.
  ENDLOOP.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 20
      text       = 'Identifying identical Keys'.
  LOOP AT gt_keys INTO gs_keys.
    READ TABLE gt_keys_c WITH KEY objname = gs_keys-objname
                                  tabkey = gs_keys-tabkey BINARY SEARCH TRANSPORTING NO FIELDS.
    LOOP AT gt_keys_c INTO gs_keys_c  WHERE objname = gs_keys-objname
                                        AND tabkey = gs_keys-tabkey.
      CHECK gs_keys_c-trkorr <> gs_keys-trkorr.
*      READ TABLE gt_key_output WITH KEY trkorr1 = gs_keys-trkorr   "1
*                                          trkorr2 = gs_keys_c-trkorr "2
*                                          pgmid = gs_keys_c-pgmid
*                                          object = gs_keys_c-object
*                                          obj_name = gs_keys_c-objname
*                                          tabkey = gs_keys_c-tabkey TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
*      READ TABLE gt_key_output WITH KEY   trkorr1 = gs_keys_c-trkorr   "2
*                                          trkorr2 = gs_keys-trkorr "1
*                                          pgmid = gs_keys_c-pgmid
*                                          object = gs_keys_c-object
*                                          obj_name = gs_keys_c-objname
*                                          tabkey = gs_keys_c-tabkey TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
*************
      CLEAR lv_chck.
      READ TABLE gt_key_output WITH KEY obj_name = gs_keys_c-objname
                                        tabkey = gs_keys_c-tabkey TRANSPORTING NO FIELDS.

      LOOP AT gt_key_output INTO ls_key_output WHERE obj_name = gs_keys_c-objname
                                                 AND tabkey = gs_keys_c-tabkey.
        IF ls_key_output-trkorr1 = gs_keys-trkorr AND
           ls_key_output-trkorr2 = gs_keys_c-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
        IF ls_key_output-trkorr1 = gs_keys_c-trkorr AND
           ls_key_output-trkorr2 = gs_keys-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
*if TR is not in Col1 then check Col2. if it is there no need to proceed.
        IF ls_key_output-trkorr1 <> gs_keys-trkorr AND
           ls_key_output-trkorr2 = gs_keys-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.

      ENDLOOP.
      CHECK lv_chck <> 'X'.
***********************
      CLEAR: gs_trwbo_requests,
             gs_key_output.
      gs_key_output-pgmid = gs_keys_c-pgmid.
      gs_key_output-object = gs_keys_c-object.
      gs_key_output-obj_name = gs_keys_c-objname.
      gs_key_output-tabkey = gs_keys_c-tabkey.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_keys-trkorr.
      gs_key_output-as4user1 = gs_trwbo_requests-h-as4user.
      gs_key_output-as4date1 = gs_trwbo_requests-h-as4date.
      gs_key_output-as4time1 = gs_trwbo_requests-h-as4time.
      gs_key_output-strkorr1 = gs_trwbo_requests-h-strkorr.
      gs_key_output-trkorr1 = gs_keys-trkorr.
*      gs_key_output-tabkey1   = gs_keys-tabkey.
      CLEAR gs_trwbo_requests.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_keys_c-trkorr.
      gs_key_output-as4user2 = gs_trwbo_requests-h-as4user.
      gs_key_output-as4date2 = gs_trwbo_requests-h-as4date.
      gs_key_output-as4time2 = gs_trwbo_requests-h-as4time.
      gs_key_output-strkorr2 = gs_trwbo_requests-h-strkorr.
      gs_key_output-trkorr2 = gs_keys_c-trkorr.
*      gs_key_output-tabkey2   = gs_keys_c-tabkey.
      APPEND gs_key_output TO gt_key_output.
    ENDLOOP.
  ENDLOOP.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Identifying identical attributes'.
  LOOP AT gt_attributes INTO gs_attributes.
    READ TABLE gt_attributes_c WITH KEY  attribute = gs_attributes-attribute
                                         TRANSPORTING NO FIELDS.
    LOOP AT gt_attributes_c INTO gs_attributes_c
                         WHERE attribute = gs_attributes-attribute.
      CHECK gs_attributes_c-trkorr <> gs_attributes-trkorr.
      READ TABLE gt_attributes_output WITH KEY
                                           trkorr1 = gs_attributes-trkorr   "1
                                           trkorr2 = gs_attributes_c-trkorr "2
                                           attribute = gs_attributes_c-attribute TRANSPORTING NO FIELDS.
      CHECK sy-subrc <> 0.
      READ TABLE gt_attributes_output WITH KEY
                                           trkorr1 = gs_attributes_c-trkorr   "2
                                           trkorr2 = gs_attributes-trkorr "1
                                           attribute = gs_attributes_c-attribute TRANSPORTING NO FIELDS.
      CHECK sy-subrc <> 0.
      CLEAR: gs_trwbo_requests,
             gs_attributes_output.
      gs_attributes_output-attribute = gs_attributes_c-attribute.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_attributes-trkorr.
      gs_attributes_output-as4user1 = gs_trwbo_requests-h-as4user.
      gs_attributes_output-as4date1 = gs_trwbo_requests-h-as4date.
      gs_attributes_output-as4time1 = gs_trwbo_requests-h-as4time.
      gs_attributes_output-strkorr1 = gs_trwbo_requests-h-strkorr.
      gs_attributes_output-trkorr1 = gs_attributes-trkorr.
*      gs_attributes_output-reference1 = gs_attributes-reference.
      CLEAR  gs_trwbo_requests.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_attributes_c-trkorr.
      gs_attributes_output-as4user2 = gs_trwbo_requests-h-as4user.
      gs_attributes_output-as4date2 = gs_trwbo_requests-h-as4date.
      gs_attributes_output-as4time2 = gs_trwbo_requests-h-as4time.
      gs_attributes_output-strkorr2 = gs_trwbo_requests-h-strkorr.
      gs_attributes_output-trkorr2 = gs_attributes_c-trkorr.
*      gs_attributes_output-reference2 = gs_attributes_c-reference.
      APPEND gs_attributes_output TO gt_attributes_output.

    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " PROCESS_TRANSPORTS_OB
*&---------------------------------------------------------------------*
*&      Form  PROCESS_TRANSPORT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_transport1 .
*LOOP AT s_tran.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = 'Processing Transports'.

  DATA: lv_trkorr TYPE trkorr,
        lt_dd07t TYPE TABLE OF dd07t,
        ls_dd07t LIKE LINE OF lt_dd07t.
  DATA: lv_chck(1),
        ls_obj_output LIKE LINE OF gt_obj_output,
        ls_key_output LIKE LINE OF gt_key_output.
  DATA: lv_tabix TYPE sy-tabix,
        lv_cnt TYPE i.

  SELECT * INTO TABLE lt_dd07t FROM dd07t
                      WHERE ( domname = 'TRSTATUS' OR
                              domname = 'TRFUNCTION' )
                        AND ddlanguage = 'EN'.

  LOOP AT gt_tab.
    lv_trkorr = gt_tab-text1.


    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = lv_trkorr "s_tran-low
      IMPORTING
        et_request_headers = gt_trwbo_request_headers
        et_requests        = gt_trwbo_requests
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    LOOP AT gt_trwbo_requests INTO gs_trwbo_requests.
      CHECK gs_trwbo_requests-h-strkorr <> space.
      LOOP AT gs_trwbo_requests-objects INTO gs_objects.
        APPEND gs_objects TO gt_objects.
      ENDLOOP.
      LOOP AT gs_trwbo_requests-keys INTO gs_keys.
        APPEND gs_keys TO gt_keys.
*           insert gs_keys inTO table gt_keys.
      ENDLOOP.
      LOOP AT gs_trwbo_requests-attributes INTO gs_attributes.
        APPEND gs_attributes TO gt_attributes.
      ENDLOOP.
      APPEND gs_trwbo_requests TO gt_requests.
    ENDLOOP.
  ENDLOOP.
**********Now Read identical objects.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 15
      text       = 'Identifying Identical objects'.

  SORT gt_objects BY  pgmid object obj_name trkorr.
  SORT gt_keys BY  pgmid object objname tabkey trkorr.
  SORT gt_attributes BY  attribute trkorr.
*****************
*  if s_obname is initial.
  DELETE ADJACENT DUPLICATES FROM gt_objects COMPARING pgmid object obj_name trkorr.
  DELETE ADJACENT DUPLICATES FROM gt_keys COMPARING pgmid object objname tabkey trkorr.
  DELETE ADJACENT DUPLICATES FROM gt_attributes COMPARING attribute trkorr.
*  endif.
******************
  gt_objects_c[] = gt_objects.
  gt_keys_c[] = gt_keys[].
  gt_attributes_c[] = gt_attributes[].
*****************
*  break sahmad.
  DELETE ADJACENT DUPLICATES FROM gt_keys COMPARING pgmid object objname tabkey.
  DELETE ADJACENT DUPLICATES FROM gt_objects COMPARING pgmid object obj_name.
  LOOP AT gt_objects INTO gs_objects.
    READ TABLE gt_objects_c WITH KEY pgmid = gs_objects-pgmid
                                     object = gs_objects-object
                                     obj_name = gs_objects-obj_name TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    lv_cnt = 0.
    LOOP AT gt_objects_c INTO gs_objects_c FROM lv_tabix.
*                         WHERE pgmid = gs_objects-pgmid
*                           AND object = gs_objects-object
*                           AND obj_name = gs_objects-obj_name.
      IF gs_objects_c-pgmid <> gs_objects-pgmid OR
         gs_objects_c-object <> gs_objects-object OR
         gs_objects_c-obj_name <> gs_objects-obj_name.
        EXIT.
      ENDIF.
*      check gs_objects_c-trkorr <> gs_objects-trkorr.
*      clear lv_chck.
*      READ TABLE gt_obj_output WITH KEY pgmid = gs_objects_c-pgmid
*                                        object = gs_objects_c-object
*                                        obj_name = gs_objects_c-obj_name TRANSPORTING NO FIELDS.
*      loop at gt_obj_output into ls_obj_output where pgmid = gs_objects_c-pgmid
*                                                 and object = gs_objects_c-object
*                                                 and obj_name = gs_objects_c-obj_name.
*              if ls_obj_output-trkorr1 = gs_objects-trkorr and
*                 ls_obj_output-trkorr2 = gs_objects_c-trkorr.
*                 lv_chck = 'X'.
*                 exit.
*              endif.
*              if ls_obj_output-trkorr1 = gs_objects_c-trkorr and
*                 ls_obj_output-trkorr2 = gs_objects-trkorr.
*                 lv_chck = 'X'.
*                 exit.
*              endif.
**if TR is not in Col1 then check Col2. if it is there no need to proceed.
*              if ls_obj_output-trkorr1 <> gs_objects-trkorr and
*                 ls_obj_output-trkorr2 = gs_objects-trkorr.
*                 lv_chck = 'X'.
*                 exit.
*              endif.
*
*      endloop.
*      check lv_chck <> 'X'.
      lv_cnt = lv_cnt + 1.
      IF lv_cnt = 1.
        CLEAR: gs_trwbo_requests,
               gs_obj_output.
        READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_objects_c-trkorr.
        gs_obj_output-as4user1 = gs_trwbo_requests-h-as4user.
        gs_obj_output-as4date1 = gs_trwbo_requests-h-as4date.
        gs_obj_output-as4time1 = gs_trwbo_requests-h-as4time.
        gs_obj_output-strkorr1 = gs_trwbo_requests-h-strkorr.
        gs_obj_output-trkorr1 = gs_objects_c-trkorr.
        CLEAR ls_dd07t.
        READ TABLE lt_dd07t INTO ls_dd07t WITH KEY domname    =  'TRSTATUS'
                                                   domvalue_l = gs_trwbo_requests-h-trstatus.
        gs_obj_output-trstatus1  = ls_dd07t-ddtext.
        CLEAR ls_dd07t.
        READ TABLE lt_dd07t INTO ls_dd07t WITH KEY  domname = 'TRFUNCTION'
                                                   domvalue_l = gs_trwbo_requests-h-trfunction.
        gs_obj_output-trfunction1  = ls_dd07t-ddtext.
      ELSE.
        CLEAR gs_trwbo_requests.
        READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_objects_c-trkorr.
        gs_obj_output-as4user2 = gs_trwbo_requests-h-as4user.
        gs_obj_output-as4date2 = gs_trwbo_requests-h-as4date.
        gs_obj_output-as4time2 = gs_trwbo_requests-h-as4time.
        gs_obj_output-strkorr2 = gs_trwbo_requests-h-strkorr.
        gs_obj_output-trkorr2 = gs_objects_c-trkorr.
        gs_obj_output-pgmid = gs_objects_c-pgmid.
        gs_obj_output-object = gs_objects_c-object.
        gs_obj_output-obj_name = gs_objects_c-obj_name.
        CLEAR ls_dd07t.
        READ TABLE lt_dd07t INTO ls_dd07t WITH KEY domname    =  'TRSTATUS'
                                                   domvalue_l = gs_trwbo_requests-h-trstatus.
        gs_obj_output-trstatus2  = ls_dd07t-ddtext.
        CLEAR ls_dd07t.
        READ TABLE lt_dd07t INTO ls_dd07t WITH KEY  domname = 'TRFUNCTION'
                                                   domvalue_l = gs_trwbo_requests-h-trfunction.
        gs_obj_output-trfunction2  = ls_dd07t-ddtext.
        APPEND gs_obj_output TO gt_obj_output.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 40
      text       = 'Identifying identical Keys'.

  LOOP AT gt_keys INTO gs_keys.

    READ TABLE gt_keys_c WITH KEY pgmid = gs_keys-pgmid
                                  object = gs_keys-object
                                  objname = gs_keys-objname
                                  tabkey = gs_keys-tabkey BINARY SEARCH TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    lv_cnt = 0.
    LOOP AT gt_keys_c INTO gs_keys_c  FROM lv_tabix.
*                                        WHERE pgmid = gs_keys-pgmid
*                                        AND object = gs_keys-object
*                                        AND objname = gs_keys-objname
*                                        and tabkey = gs_keys-tabkey.
      IF gs_keys_c-pgmid <> gs_keys-pgmid OR
         gs_keys_c-object <> gs_keys-object OR
         gs_keys_c-objname <> gs_keys-objname OR
         gs_keys_c-tabkey <> gs_keys-tabkey.
        EXIT.
      ENDIF.
      lv_cnt = lv_cnt + 1.
      IF lv_cnt = 1.
        CLEAR: gs_trwbo_requests,
               gs_key_output.
        gs_key_output-pgmid = gs_keys_c-pgmid.
        gs_key_output-object = gs_keys_c-object.
        gs_key_output-obj_name = gs_keys_c-objname.
        gs_key_output-tabkey = gs_keys_c-tabkey.
        READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_keys_c-trkorr.
        gs_key_output-as4user1 = gs_trwbo_requests-h-as4user.
        gs_key_output-as4date1 = gs_trwbo_requests-h-as4date.
        gs_key_output-as4time1 = gs_trwbo_requests-h-as4time.
        gs_key_output-strkorr1 = gs_trwbo_requests-h-strkorr.
        gs_key_output-trkorr1 = gs_keys_c-trkorr.
      ELSE.
        CLEAR gs_trwbo_requests.
        READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_keys_c-trkorr.
        gs_key_output-as4user2 = gs_trwbo_requests-h-as4user.
        gs_key_output-as4date2 = gs_trwbo_requests-h-as4date.
        gs_key_output-as4time2 = gs_trwbo_requests-h-as4time.
        gs_key_output-strkorr2 = gs_trwbo_requests-h-strkorr.
        gs_key_output-trkorr2 = gs_keys_c-trkorr.
*             gs_key_output-tabkey2   = gs_keys_c-tabkey.
        APPEND gs_key_output TO gt_key_output.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
*   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*     EXPORTING
*       PERCENTAGE       = 50
*       TEXT             = 'Identifying identical attributes' .
*  LOOP AT gt_attributes INTO gs_attributes.
*      read table gt_attributes_c with key  attribute = gs_attributes-attribute
*                                           transporting no fields.
*    LOOP AT gt_attributes_c INTO gs_attributes_c
*                         WHERE attribute = gs_attributes-attribute.
*      check gs_attributes_c-trkorr <> gs_attributes-trkorr.
*      READ TABLE gt_attributes_output WITH KEY
*                                           trkorr1 = gs_attributes-trkorr   "1
*                                           trkorr2 = gs_attributes_c-trkorr "2
*                                           attribute = gs_attributes_c-attribute TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
*      READ TABLE gt_attributes_output WITH KEY
*                                           trkorr1 = gs_attributes_c-trkorr   "2
*                                           trkorr2 = gs_attributes-trkorr "1
*                                           attribute = gs_attributes_c-attribute TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
*      CLEAR: gs_trwbo_requests,
*             gs_attributes_output.
*      gs_attributes_output-attribute = gs_attributes_c-attribute.
*      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_attributes-trkorr.
*      gs_attributes_output-as4user1 = gs_trwbo_requests-h-as4user.
*      gs_attributes_output-as4date1 = gs_trwbo_requests-h-as4date.
*      gs_attributes_output-as4time1 = gs_trwbo_requests-h-as4time.
*      gs_attributes_output-strkorr1 = gs_trwbo_requests-h-strkorr.
*      gs_attributes_output-trkorr1 = gs_attributes-trkorr.
**      gs_attributes_output-reference1 = gs_attributes-reference.
*      CLEAR  gs_trwbo_requests.
*      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_attributes_c-trkorr.
*      gs_attributes_output-as4user2 = gs_trwbo_requests-h-as4user.
*      gs_attributes_output-as4date2 = gs_trwbo_requests-h-as4date.
*      gs_attributes_output-as4time2 = gs_trwbo_requests-h-as4time.
*      gs_attributes_output-strkorr2 = gs_trwbo_requests-h-strkorr.
*      gs_attributes_output-trkorr2 = gs_attributes_c-trkorr.
**      gs_attributes_output-reference2 = gs_attributes_c-reference.
*      APPEND gs_attributes_output TO gt_attributes_output.
*
*    ENDLOOP.
*  ENDLOOP.
ENDFORM.                    " PROCESS_TRANSPORT1
*&---------------------------------------------------------------------*
*&      Form  PROCESS_TRANSPORTS_OB1
*&---------------------------------------------------------------------*
*       Process Object Transports
*----------------------------------------------------------------------*
FORM process_transports_ob1 .

*LOOP AT s_tran.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 10
      text       = 'Processing Transports'.

  DATA: lv_trkorr TYPE trkorr,
        lt_dd07t TYPE TABLE OF dd07t,
        ls_dd07t LIKE LINE OF lt_dd07t.
  DATA: ls_key_output LIKE LINE OF gt_key_output,
        lv_chck(1),
        ls_obj_output LIKE LINE OF gt_obj_output.
  SELECT * INTO TABLE lt_dd07t FROM dd07t
                      WHERE ( domname = 'TRSTATUS' OR
                              domname = 'TRFUNCTION' )
                        AND ddlanguage = 'EN'.

  LOOP AT gt_tab.
    lv_trkorr = gt_tab-text1.


    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = lv_trkorr "s_tran-low
      IMPORTING
        et_request_headers = gt_trwbo_request_headers
        et_requests        = gt_trwbo_requests
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    LOOP AT gt_trwbo_requests INTO gs_trwbo_requests.
      CHECK gs_trwbo_requests-h-strkorr <> space.
      LOOP AT gs_trwbo_requests-objects INTO gs_objects.
        APPEND gs_objects TO gt_objects.
      ENDLOOP.
      LOOP AT gs_trwbo_requests-keys INTO gs_keys.
        APPEND gs_keys TO gt_keys.
      ENDLOOP.
      LOOP AT gs_trwbo_requests-attributes INTO gs_attributes.
        APPEND gs_attributes TO gt_attributes.
      ENDLOOP.
      APPEND gs_trwbo_requests TO gt_requests.
    ENDLOOP.
  ENDLOOP.
**********Now Read identical objects.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 15
      text       = 'Identifying Identical objects'.

  SORT gt_objects BY  obj_name trkorr.
  SORT gt_keys BY  objname tabkey trkorr.
  SORT gt_attributes BY  attribute trkorr.
*****************
******************
  gt_objects_c[] = gt_objects.
  gt_keys_c[] = gt_keys[].
  gt_attributes_c[] = gt_attributes[].
********************
  DELETE ADJACENT DUPLICATES FROM gt_keys COMPARING objname tabkey.

  LOOP AT gt_objects INTO gs_objects.
    READ TABLE gt_objects_c WITH KEY obj_name = gs_objects-obj_name TRANSPORTING NO FIELDS.
    LOOP AT gt_objects_c INTO gs_objects_c
                         WHERE obj_name = gs_objects-obj_name.
      CHECK gs_objects_c-trkorr <> gs_objects-trkorr.
      CLEAR lv_chck.
      READ TABLE gt_obj_output WITH KEY obj_name = gs_objects_c-obj_name TRANSPORTING NO FIELDS.
      LOOP AT gt_obj_output INTO ls_obj_output WHERE obj_name = gs_objects_c-obj_name.
        IF ls_obj_output-trkorr1 = gs_objects-trkorr AND
           ls_obj_output-trkorr2 = gs_objects_c-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
        IF ls_obj_output-trkorr1 = gs_objects_c-trkorr AND
           ls_obj_output-trkorr2 = gs_objects-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.
*if TR is not in Col1 then check Col2. if it is there no need to proceed.
        IF ls_obj_output-trkorr1 <> gs_objects-trkorr AND
           ls_obj_output-trkorr2 = gs_objects-trkorr.
          lv_chck = 'X'.
          EXIT.
        ENDIF.

      ENDLOOP.
      CHECK lv_chck <> 'X'.

      CLEAR: gs_trwbo_requests,
             gs_obj_output.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_objects-trkorr.
      gs_obj_output-as4user1 = gs_trwbo_requests-h-as4user.
      gs_obj_output-as4date1 = gs_trwbo_requests-h-as4date.
      gs_obj_output-as4time1 = gs_trwbo_requests-h-as4time.
      gs_obj_output-strkorr1 = gs_trwbo_requests-h-strkorr.
      gs_obj_output-trkorr1 = gs_objects-trkorr.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY domname    =  'TRSTATUS'
                                                 domvalue_l = gs_trwbo_requests-h-trstatus.
      gs_obj_output-trstatus1  = ls_dd07t-ddtext.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY  domname = 'TRFUNCTION'
                                                 domvalue_l = gs_trwbo_requests-h-trfunction.
      gs_obj_output-trfunction1  = ls_dd07t-ddtext.

      CLEAR gs_trwbo_requests.
      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_objects_c-trkorr.
      gs_obj_output-as4user2 = gs_trwbo_requests-h-as4user.
      gs_obj_output-as4date2 = gs_trwbo_requests-h-as4date.
      gs_obj_output-as4time2 = gs_trwbo_requests-h-as4time.
      gs_obj_output-strkorr2 = gs_trwbo_requests-h-strkorr.
      gs_obj_output-trkorr2 = gs_objects_c-trkorr.
      gs_obj_output-pgmid = gs_objects_c-pgmid.
      gs_obj_output-object = gs_objects_c-object.
      gs_obj_output-obj_name = gs_objects_c-obj_name.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY domname    =  'TRSTATUS'
                                                 domvalue_l = gs_trwbo_requests-h-trstatus.
      gs_obj_output-trstatus2  = ls_dd07t-ddtext.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t WITH KEY  domname = 'TRFUNCTION'
                                                 domvalue_l = gs_trwbo_requests-h-trfunction.
      gs_obj_output-trfunction2  = ls_dd07t-ddtext.
      APPEND gs_obj_output TO gt_obj_output.
    ENDLOOP.
  ENDLOOP.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 40
      text       = 'Identifying identical Keys'.
********************************************
  DATA: lv_tabix TYPE sy-tabix,
        lv_cnt TYPE i.
  LOOP AT gt_keys INTO gs_keys.

    READ TABLE gt_keys_c WITH KEY objname = gs_keys-objname
                                  tabkey = gs_keys-tabkey BINARY SEARCH TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    lv_cnt = 0.
    LOOP AT gt_keys_c INTO gs_keys_c  FROM lv_tabix.
*                                        WHERE pgmid = gs_keys-pgmid
*                                        AND object = gs_keys-object
*                                        AND objname = gs_keys-objname
*                                        and tabkey = gs_keys-tabkey.
      IF gs_keys_c-objname <> gs_keys-objname OR
         gs_keys_c-tabkey <> gs_keys-tabkey.
        EXIT.
      ENDIF.
      lv_cnt = lv_cnt + 1.
      IF lv_cnt = 1.
        CLEAR: gs_trwbo_requests,
               gs_key_output.
        gs_key_output-pgmid = gs_keys_c-pgmid.
        gs_key_output-object = gs_keys_c-object.
        gs_key_output-obj_name = gs_keys_c-objname.
        gs_key_output-tabkey = gs_keys_c-tabkey.
        READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_keys_c-trkorr.
        gs_key_output-as4user1 = gs_trwbo_requests-h-as4user.
        gs_key_output-as4date1 = gs_trwbo_requests-h-as4date.
        gs_key_output-as4time1 = gs_trwbo_requests-h-as4time.
        gs_key_output-strkorr1 = gs_trwbo_requests-h-strkorr.
        gs_key_output-trkorr1 = gs_keys_c-trkorr.
      ELSE.
        CLEAR gs_trwbo_requests.
        READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_keys_c-trkorr.
        gs_key_output-as4user2 = gs_trwbo_requests-h-as4user.
        gs_key_output-as4date2 = gs_trwbo_requests-h-as4date.
        gs_key_output-as4time2 = gs_trwbo_requests-h-as4time.
        gs_key_output-strkorr2 = gs_trwbo_requests-h-strkorr.
        gs_key_output-trkorr2 = gs_keys_c-trkorr.
*             gs_key_output-tabkey2   = gs_keys_c-tabkey.
        APPEND gs_key_output TO gt_key_output.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
*******************
*   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*     EXPORTING
*       PERCENTAGE       = 50
*       TEXT             = 'Identifying identical attributes' .
*  LOOP AT gt_attributes INTO gs_attributes.
*      read table gt_attributes_c with key  attribute = gs_attributes-attribute
*                                           transporting no fields.
*    LOOP AT gt_attributes_c INTO gs_attributes_c
*                         WHERE attribute = gs_attributes-attribute.
*      check gs_attributes_c-trkorr <> gs_attributes-trkorr.
*      READ TABLE gt_attributes_output WITH KEY
*                                           trkorr1 = gs_attributes-trkorr   "1
*                                           trkorr2 = gs_attributes_c-trkorr "2
*                                           attribute = gs_attributes_c-attribute TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
*      READ TABLE gt_attributes_output WITH KEY
*                                           trkorr1 = gs_attributes_c-trkorr   "2
*                                           trkorr2 = gs_attributes-trkorr "1
*                                           attribute = gs_attributes_c-attribute TRANSPORTING NO FIELDS.
*      CHECK sy-subrc <> 0.
*      CLEAR: gs_trwbo_requests,
*             gs_attributes_output.
*      gs_attributes_output-attribute = gs_attributes_c-attribute.
*      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_attributes-trkorr.
*      gs_attributes_output-as4user1 = gs_trwbo_requests-h-as4user.
*      gs_attributes_output-as4date1 = gs_trwbo_requests-h-as4date.
*      gs_attributes_output-as4time1 = gs_trwbo_requests-h-as4time.
*      gs_attributes_output-strkorr1 = gs_trwbo_requests-h-strkorr.
*      gs_attributes_output-trkorr1 = gs_attributes-trkorr.
**      gs_attributes_output-reference1 = gs_attributes-reference.
*      CLEAR  gs_trwbo_requests.
*      READ TABLE gt_requests INTO gs_trwbo_requests WITH KEY h-trkorr = gs_attributes_c-trkorr.
*      gs_attributes_output-as4user2 = gs_trwbo_requests-h-as4user.
*      gs_attributes_output-as4date2 = gs_trwbo_requests-h-as4date.
*      gs_attributes_output-as4time2 = gs_trwbo_requests-h-as4time.
*      gs_attributes_output-strkorr2 = gs_trwbo_requests-h-strkorr.
*      gs_attributes_output-trkorr2 = gs_attributes_c-trkorr.
**      gs_attributes_output-reference2 = gs_attributes_c-reference.
*      APPEND gs_attributes_output TO gt_attributes_output.
*
*    ENDLOOP.
*  ENDLOOP.
ENDFORM.                    " PROCESS_TRANSPORTS_OB1


*object: D30K916134          000001R3TRTABUTVARVC                                                                                                                  K
*DATA: ls_key          TYPE e071k,
*        lt_keys         TYPE tr_keys.
*ps_object          TYPE e071
*
*CALL FUNCTION 'TRKLE_SHOW_TABLE_CONTENT'
*    EXPORTING
*      is_object      = ps_object
*      it_keys        = lt_keys
*      is_current_key = ls_key
*    EXCEPTIONS
*      invalid_key    = 1
*      OTHERS         = 2.
