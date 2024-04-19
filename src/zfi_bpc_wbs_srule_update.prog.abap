*&---------------------------------------------------------------------*
*& Report  ZPS_BPC_WBS_SRULE_UPDATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFI_BPC_WBS_SRULE_UPDATE.
TYPE-POOLS:SLIS.
TABLES:proj.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:s_pspnr FOR proj-pspnr,
               s_vbukr FOR proj-vbukr,
               s_erdat FOR proj-erdat.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
PARAMETERS:       rb_appl  RADIOBUTTON GROUP rbg1  "Application Server "
                           DEFAULT 'X'
                           USER-COMMAND cmd.
PARAMETERS:       p_fpath1 TYPE localfile."DEFAULT '/usr/sap/interfaces/S01/BPC/'.     "Filepath-Output      "

PARAMETERS:       rb_pres  RADIOBUTTON GROUP rbg1. "Presentation Server"
PARAMETERS:       p_file   type STRING." DEFAULT 'H:\SAPTEMP\PP_EXTRACT_PS_'.
SELECTION-SCREEN: END   OF BLOCK ssb2.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:c_test type c as CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.
***********data declarations ***************
TYPES:BEGIN OF ty_final,
pbukr TYPE prps-pbukr,
pspnr TYPE prps-pspnr,
pspid TYPE proj-pspid,
sprog TYPE proj-sprog,
eprog TYPE proj-eprog,
erdat TYPE prps-erdat,
objke TYPE ausp-objek,
atinn TYPE ausp-atinn,
atwrt TYPE ausp-atwrt,
MSG   TYPE STRING,
      END OF ty_final,
      BEGIN OF ty_proj,
        pspnr TYPE proj-pspnr,
        pspid TYPE proj-pspid,
        erdat TYPE proj-erdat,
        vbukr TYPE proj-vbukr,
        sprog TYPE proj-sprog,
        eprog TYPE proj-eprog,
      END OF ty_proj,
      BEGIN OF ty_prps,
        pspnr TYPE prps-pspnr,
        objnr TYPE char50, "prps-objnr,
        erdat TYPE prps-erdat,
        pbukr TYPE prps-pbukr,
      END OF ty_prps,
      BEGIN OF ty_ausp,
        objek TYPE ausp-objek,
        atinn TYPE ausp-atinn,
        atwrt TYPE ausp-atwrt,
      END OF ty_ausp,
      BEGIN OF ty_prhi,
        posnr TYPE prhi-posnr,
        psphi TYPE prhi-psphi,
      END OF ty_prhi.

DATA:gt_final      TYPE TABLE OF ty_final,
     gs_final      TYPE ty_final,
     gt_proj       TYPE TABLE OF ty_proj,
     gs_proj       TYPE ty_proj,
     gt_prps       TYPE TABLE OF ty_prps,
     gs_prps       TYPE ty_prps,
     gt_ausp       TYPE TABLE OF ty_ausp,
     gs_ausp       TYPE ty_ausp,
     gt_prhi       TYPE TABLE OF ty_prhi,
     gs_prhi       TYPE ty_prhi,
     gs_prj_def    TYPE bapi_project_definition,
     gs_prj_def_u  TYPE bapi_project_definition_up,
     gt_meth_prj   TYPE TABLE OF bapi_method_project,
     gs_meth_prj   TYPE bapi_method_project,
     gt_activity   TYPE TABLE OF bapi_act_element,
     gs_activity   TYPE bapi_act_element,
     gt_activity_u TYPE TABLE OF bapi_act_element_upd,
     gs_activity_u TYPE bapi_act_element_upd,
     gs_return     TYPE bapireturn1,
     gt_message    TYPE TABLE OF bapi_meth_message,
     gs_message    TYPE bapi_meth_message,
     gt_wbs_ele    TYPE TABLE OF BAPI_WBS_ELEMENT,
     gs_wbs_ele    TYPE BAPI_WBS_ELEMENT,
     gt_wbs_ele_u  TYPE TABLE OF BAPI_WBS_ELEMENT_UPDATE,
     gs_wbs_ele_u  TYPE BAPI_WBS_ELEMENT_UPDATE,
     gt_fieldcatalog type slis_t_fieldcat_alv,
     gs_fieldcatalog like LINE OF gt_fieldcatalog.


INITIALIZATION.

  CONCATENATE 'H:\SAPTEMP\PP_EXTRACT_PS_' sy-datum SY-UZEIT '.csv' into p_file.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid '/BPC/' into p_fpath1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath1.
  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    EXPORTING
*     DIRECTORY        = ' '
      filemask         = '*.*'
    IMPORTING
      serverfile       = p_fpath1
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

START-OF-SELECTION.
  SELECT pspnr pspid erdat vbukr sprog eprog
         FROM proj
         INTO TABLE gt_proj
         WHERE pspnr IN s_pspnr
         AND   erdat IN s_erdat
         AND   vbukr IN s_vbukr.
  IF gt_proj is INITIAL. "gt_prhi[] IS NOT INITIAL.
    SELECT pspnr objnr erdat pbukr
           FROM prps
           INTO TABLE gt_prps
           FOR ALL ENTRIES IN gt_proj
            where   pspnr  = gt_proj-pspnr
           AND   erdat  = gt_proj-erdat
           AND   pbukr  = gt_proj-vbukr.

  ENDIF.
  IF gt_prps[] IS NOT INITIAL.
    SELECT objek atinn atwrt
           FROM ausp
           INTO TABLE gt_ausp
           FOR ALL ENTRIES IN gt_prps
           WHERE objek  = gt_prps-objnr
           AND   atinn  = '0000000631'."PROJECT_CNTR_NUMBER'.

  ENDIF.
  SORT:gt_proj BY pspnr,
       gt_prps BY objnr,
       gt_ausp BY objek,
       gt_prhi BY posnr.
  LOOP AT gt_prps INTO gs_prps.
    READ TABLE gt_proj INTO gs_proj WITH KEY pspnr = gs_prps-pspnr BINARY SEARCH."gs_prhi-psphi BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE gt_ausp INTO gs_ausp WITH KEY objek = gs_prps-objnr BINARY SEARCH.
      IF sy-subrc = 0.
        gs_final-objke  = gs_ausp-objek.
        gs_final-atinn  = gs_ausp-atinn.
        gs_final-atwrt  = gs_ausp-atwrt.
      ELSE.
        gs_final-objke  = gs_prps-objnr.
        gs_final-atwrt  = 'N/A'.
      ENDIF.
      gs_final-sprog  = gs_proj-sprog.
      gs_final-eprog  = gs_proj-eprog.
      gs_final-pspnr  = gs_prps-pspnr.
      gs_final-pspid  = gs_proj-pspid.
      gs_final-erdat  = gs_prps-erdat.
      gs_final-pbukr  = gs_prps-pbukr.
      APPEND gs_final TO gt_final.
      CLEAR gs_final.
    ENDIF.
  ENDLOOP.
  LOOP AT gt_final INTO gs_final.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        INPUT  = gs_final-pspid
      IMPORTING
        OUTPUT = gs_prj_def-project_definition.
    gs_meth_prj-refnumber          = '000001'.
    gs_meth_prj-objecttype         = 'WBS-ELEMENT'.         "#EC NOTEXT
    gs_meth_prj-method             = 'Update'.              "#EC NOTEXT
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = gs_final-pspnr
      IMPORTING
        OUTPUT = gs_meth_prj-objectkey.


    APPEND gs_meth_prj TO gt_meth_prj.
    CLEAR gs_meth_prj.
    gs_meth_prj-method = 'Save'.                            "#EC NOTEXT
    APPEND gs_meth_prj TO gt_meth_prj.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = gs_final-pspnr
      IMPORTING
        OUTPUT = gs_wbs_ele-WBS_ELEMENT.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        INPUT  = gs_final-pspid
      IMPORTING
        OUTPUT = gs_wbs_ele-PROJECT_DEFINITION.

*        = .
    gs_wbs_ele-USER_FIELD_KEY     = 'ZALELEM'.
    gs_wbs_ele-USER_FIELD_CHAR20_2   = gs_final-atwrt.
    IF gs_final-sprog IS NOT INITIAL.
      gs_wbs_ele-user_field_date1      = gs_final-sprog.
    ELSE.
      gs_wbs_ele-user_field_date1      = '99991231'.
    ENDIF.
    IF gs_final-eprog IS NOT INITIAL.
      gs_wbs_ele-user_field_date2      = gs_final-eprog.
    ELSE.
      gs_wbs_ele-user_field_date2      = '99991231'.
    ENDIF.

    APPEND gs_wbs_ele TO gt_wbs_ele.
    gs_wbs_ele-WBS_ELEMENT           = 'X'.
    gs_wbs_ele-PROJECT_DEFINITION    = 'X'.
    gs_wbs_ele_u-USER_FIELD_CHAR20_2 = 'X'.
    gs_wbs_ele_u-user_field_date1    = 'X'.
    gs_wbs_ele_u-user_field_date2    = 'X'.
    APPEND gs_wbs_ele_u TO gt_wbs_ele_u.
    CALL FUNCTION 'BAPI_PROJECT_MAINTAIN'
      EXPORTING
        i_project_definition       = gs_prj_def
        i_project_definition_upd   = gs_prj_def_u
      IMPORTING
        return                     = gs_return
      TABLES
        i_method_project           = gt_meth_prj
        i_wbs_element_table_update = gt_wbs_ele_u
        i_wbs_element_table        = gt_wbs_ele
        e_message_table            = gt_message.
    IF gs_return-type NE 'E'.
      READ TABLE gt_message into gs_message INDEX 1.
      gs_final-msg = gs_message-message_text.
      if c_test is INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      endif.
    else.
      READ TABLE gt_message into gs_message INDEX 1.
      gs_final-msg = gs_message-message_text.
    ENDIF.
    modify gt_final from gs_final.
    REFRESH:gt_meth_prj,gt_wbs_ele,gt_wbs_ele_u,gt_message.
    CLEAR:gs_prj_def,gs_prj_def_u,gs_return,gs_message.

  ENDLOOP.
  IF     ( rb_pres IS NOT INITIAL and p_file is not INITIAL ).

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_file
      TABLES
        data_tab                = gt_final
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
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    WRITE: 'File Outputed Successfully to: ', p_file.

  ELSEIF ( rb_appl IS NOT INITIAL and p_fpath1 is not INITIAL ).

    DATA: lv_msg          TYPE text100,
           lv_string       TYPE string,
           lv_text         TYPE text1000.
    CONCATENATE P_FPATH1 'PP_EXTRACT_PS_' sy-datum SY-UZEIT '.TXT' INTO P_FPATH1.
    IF   ( gt_final[]            IS NOT INITIAL ).
      OPEN     DATASET p_fpath1
               FOR OUTPUT IN TEXT MODE MESSAGE lv_msg ENCODING DEFAULT.
      IF sy-subrc NE 0.
        MESSAGE  'File Opening Error' type 'E'.
        RETURN.
      ENDIF.
      CLEAR                                 lv_string.
      LOOP AT  gt_final into gs_final. "   INTO lv_string.
        CONCATENATE  gS_final-pbukr
                     gS_final-pspnr
          gs_final-pspid
          gs_final-sprog
          gs_final-eprog
          gs_final-erdat
          gs_final-objke
          gs_final-atinn
          gs_final-atwrt
          gs_final-msg INTO lv_string SEPARATED BY '|'.
        CLEAR                               lv_text.
        MOVE   lv_string                 TO lv_text.

        TRANSFER                            lv_text
                                         TO p_fpath1.

        CLEAR  lv_string.
      ENDLOOP.

* Close the file
      CLOSE    DATASET  p_fpath1.
    ENDIF.

  ENDIF.

END-OF-SELECTION.

  PERFORM f_display.
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DISPLAY .

  PERFORM f_BUILD_FIELDCATALOG .
  if gt_final[] is not INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = sy-repid
        IT_FIELDCAT        = gt_FIELDCATALOG
      TABLES
        T_OUTTAB           = gt_final[]
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  else.
    WRITE: 'No data found.'.
  endif.


ENDFORM.                    " F_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUILD_FIELDCATALOG .

  Perform field_cat using 'PBUKR' 'Asset Class' .
  Perform field_cat using 'PSPNR' 'WBS Element' .
  Perform field_cat using 'PSPID' 'Project Definition' .
  Perform field_cat using 'SPROG' 'Forecast start date ' .
  Perform field_cat using 'EPROG' 'Forecast finish date' .
  Perform field_cat using 'ERDAT' 'Created Date' .
  Perform field_cat using 'PBUKR' 'Company code' .
  Perform field_cat using 'OBJEK' 'Key of object' .
  Perform field_cat using 'ATINN' 'Internal characteristic' .
  Perform field_cat using 'ATWRT' 'Characteristic Value' .
  Perform field_cat using 'MSG'   'Sucess/Error message' .
ENDFORM.                    " F_BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0868   text
*      -->P_0869   text
*----------------------------------------------------------------------*
FORM FIELD_CAT  USING    p_field p_label.
  gs_fieldcatalog-fieldname   = p_field.
  gs_fieldcatalog-seltext_m   = p_label.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear  gs_fieldcatalog.

ENDFORM.                    " FIELD_CAT
