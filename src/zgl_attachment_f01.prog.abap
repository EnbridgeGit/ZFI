*&---------------------------------------------------------------------*
*&  Include           ZGL_ATTACHMENT_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZGL_ATTACHMENT                                 *
* Include            :  ZGL_ATTACHMENT_F01                             *
* Author             :  Manvitha Dadi                                  *
* Date               :  28-Jul-2021                                    *
* Technical Contact  :  Ashok Madasu/Manvitha Dadi                     *
* Purpose            :  Attach any file from appplication server       *
*                       to FB03 Screen                                 *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 28-Jul-2021  DADIM         D30K931398 CHG0221895 - Create custom     *
*                                       tcode for FB03/FBV3 attachments*
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_data .

  DATA : lv_text TYPE text200.
  CLEAR: gt_content[], gt_objhead[].
  CLEAR: gv_belnr,gv_file,gv_filename,gv_extension,gv_ep_note.
  CLEAR: gs_content,gs_fol_id,gs_object,gs_obj_data,gs_obj_id,gs_folmem_k,gs_note.

  IF p_appl IS NOT INITIAL.
    PERFORM open_dataset.
  ELSE.
    PERFORM gui_upload.
  ENDIF.

* Convert Accounting Document.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_belnr
    IMPORTING
      output = gv_belnr.

* Convert to BIN
  CALL FUNCTION 'SO_CONVERT_CONTENTS_BIN'
    EXPORTING
      it_contents_bin = gt_content[]
    IMPORTING
      et_contents_bin = gt_content[].
  IF sy-subrc <> 0.
    WRITE: / text-006.
    EXIT.
  ENDIF.

* Get folder id
  CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
    EXPORTING
      region                = 'B'
    IMPORTING
      folder_id             = gs_fol_id
    EXCEPTIONS
      communication_failure = 1
      owner_not_exist       = 2
      system_failure        = 3
      x_error               = 4
      OTHERS                = 5.
* Sy-subrc check not required
  IF sy-subrc <> 0.
    WRITE: / text-003.
    EXIT.
  ENDIF.

* Get file name and extension
  CALL FUNCTION 'CRM_EMAIL_SPLIT_FILENAME'
    EXPORTING
      iv_path      = gv_file
    IMPORTING
      ev_filename  = gv_filename
      ev_extension = gv_extension.

  IF sy-subrc EQ 0.
* Object header
    CLEAR gs_content.
    CONCATENATE text-018 gv_filename INTO gs_content.
    APPEND gs_content TO gt_objhead.
    CLEAR gs_content.
  ENDIF.

  gs_object-objkey+0(4) = p_bukrs.
  CONCATENATE gv_belnr p_gjahr INTO gs_object-objkey+4(14).

* For example, business object name for PO is BUS2012,
* business object for PR is BUS2105,
* business object for Vendor is LFA1 etc
  gs_object-objtype = gc_bkpf.
  gs_obj_data-objsns = 'O'.
  gs_obj_data-objla = sy-langu.
  CONCATENATE text-002 sy-uname INTO gs_obj_data-objdes
                                SEPARATED BY space.
  gs_obj_data-file_ext = gv_extension.
  TRANSLATE gs_obj_data-file_ext TO UPPER CASE.

* This is very important step. If your object size does not match with the input
* file size, then your object might get attached, but it will show error while you
* try to open it.
* If you have a way, where you can read the input file size directly, then assign
* it directly else, use the below formula
  gs_obj_data-objlen =  lines( gt_content ) * 255.

* Insert data
  CALL FUNCTION 'SO_OBJECT_INSERT'
    EXPORTING
      folder_id                  = gs_fol_id
      object_type                = gc_ext
      object_hd_change           = gs_obj_data
    IMPORTING
      object_id                  = gs_obj_id
    TABLES
      objhead                    = gt_objhead
      objcont                    = gt_content
    EXCEPTIONS
      active_user_not_exist      = 1
      communication_failure      = 2
      component_not_available    = 3
      dl_name_exist              = 4
      folder_not_exist           = 5
      folder_no_authorization    = 6
      object_type_not_exist      = 7
      operation_no_authorization = 8
      owner_not_exist            = 9
      parameter_error            = 10
      substitute_not_active      = 11
      substitute_not_defined     = 12
      system_failure             = 13
      x_error                    = 14
      OTHERS                     = 15.
  IF sy-subrc = 0 AND gs_object-objkey IS NOT INITIAL.
    gs_folmem_k-foltp = gs_fol_id-objtp.
    gs_folmem_k-folyr = gs_fol_id-objyr.
    gs_folmem_k-folno = gs_fol_id-objno.
* Please note: gs_fol_id and gs_obj_id are different work areas
    gs_folmem_k-doctp = gs_obj_id-objtp.
    gs_folmem_k-docyr = gs_obj_id-objyr.
    gs_folmem_k-docno = gs_obj_id-objno.
    gv_ep_note = gs_folmem_k.
    gs_note-objtype = gc_msg.
    gs_note-objkey = gv_ep_note.
* Link it
    CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
      EXPORTING
        obj_rolea      = gs_object
        obj_roleb      = gs_note
        relationtype   = gc_atta
      EXCEPTIONS
        no_model       = 1
        internal_error = 2
        unknown        = 3
        OTHERS         = 4.
    IF sy-subrc EQ 0.
* Commit it
      COMMIT WORK.
      WRITE:/ text-007.
    ELSE.
      WRITE: / text-008.
      EXIT.
    ENDIF.
  ELSE.
    WRITE: / text-009.
    EXIT.
  ENDIF.

ENDFORM.                    " UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_APPL_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_appl_path .

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = p_file1
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_APPL_PATH
*&---------------------------------------------------------------------*
*&      Form  GET_PRES_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pres_path .

  DATA:    lv_repid TYPE syrepid,
           lv_dynnr TYPE sydynnr,
           lv_file  TYPE localfile.

  CLEAR : lv_repid,lv_dynnr.
  MOVE     sy-repid TO lv_repid.
  MOVE     sy-dynnr TO lv_dynnr.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = lv_repid
      dynpro_number = lv_dynnr
    CHANGING
      file_name     = lv_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF         ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE lv_file TO p_file2.

ENDFORM.                    " GET_PRES_PATH
*&---------------------------------------------------------------------*
*&      Form  OPEN_DATASET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM open_dataset .

  DATA: lv_msg_text TYPE text80.

*   Open corresponding DMS files
  OPEN DATASET p_file1 FOR INPUT IN BINARY MODE MESSAGE lv_msg_text.
  IF sy-subrc EQ 0.
    WHILE sy-subrc = 0.
      READ DATASET p_file1 INTO gs_content.
* Do not put Sy-subrc eq 0 here. Please add the last line of the file,
* though sy-subrc may fail
      APPEND  gs_content TO gt_content.
      CLEAR gs_content.
    ENDWHILE.
* Close file
    CLOSE DATASET p_file1.
  ELSE.
    WRITE: text-011, lv_msg_text, p_file1.
    STOP.
  ENDIF.
  gv_file = p_file1.

ENDFORM.                    " OPEN_DATASET
*&---------------------------------------------------------------------*
*&      Form  GUI_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gui_upload .

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = p_file2
      filetype                = gc_asc
    TABLES
      data_tab                = gt_content
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    WRITE: text-012.
    STOP.
  ENDIF.
  gv_file = p_file2.

ENDFORM.                    " GUI_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_NET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_net .

  DATA : lv_docid TYPE saeardoid,
         lv_doctp TYPE saedoktyp,
         lv_file  TYPE saepfad,
         lv_objid TYPE saeobjid,
         lv_doc   TYPE saedoktyp,
         lv_obj   TYPE saeobjart,
         lv_path  TYPE toaat-filename,
         lv_descr TYPE toaat-descr,
         lv_creator TYPE syuname.

  CLEAR : gv_belnr,lv_docid,lv_doctp,lv_file,lv_objid,lv_doc,
          lv_obj,lv_path,gv_extension,lv_descr,gv_file,lv_creator.

  IF p_appl IS NOT INITIAL.
    gv_file = p_file1.
  ELSE.
    gv_file = p_file2.
  ENDIF.

* Convert Accounting Document.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_belnr
    IMPORTING
      output = gv_belnr.

  CALL FUNCTION 'CRM_EMAIL_SPLIT_FILENAME'
    EXPORTING
      iv_path      = gv_file
    IMPORTING
      ev_filename  = gv_filename
      ev_extension = gv_extension.

  TRANSLATE gv_extension TO UPPER CASE.
  IF gv_extension NOT IN gr_extension.
    WRITE : / text-016.
    EXIT.
  ENDIF.

  CASE gv_extension.
    WHEN gc_dc.
      lv_doctp = gc_doc.
      lv_obj = gc_doc.
    WHEN gc_dx.
      lv_doctp = gc_docx.
      lv_obj = gc_docx.
    WHEN gc_xs.
      lv_doctp = gc_xls.
      lv_obj = gc_xls.
    WHEN gc_xx.
      lv_doctp = gc_xlsx.
      lv_obj = gc_xlsx.
    WHEN gc_pf.
      lv_doctp = gc_pdf.
      lv_obj = gc_pdf.
    WHEN gc_zp.
      lv_doctp = gc_zip.
      lv_obj = gc_zip.
    WHEN OTHERS.
  ENDCASE.

  lv_file = gv_file.
  lv_path = gv_filename.
  lv_doc = gv_extension.

  CALL FUNCTION 'ARCHIVOBJECT_CREATE_FILE'
    EXPORTING
      archiv_id                = gc_a1
      document_type            = lv_doctp
      path                     = lv_file
      vscan_profile            = gc_scms
    IMPORTING
      archiv_doc_id            = lv_docid
    EXCEPTIONS
      error_archiv             = 1
      error_communicationtable = 2
      error_upload             = 3
      error_kernel             = 4
      blocked_by_policy        = 5
      OTHERS                   = 6.

  IF sy-subrc = 0.

    lv_objid+0(4) = p_bukrs.
    CONCATENATE gv_belnr p_gjahr INTO lv_objid+4(14).
    CONCATENATE text-002 sy-uname INTO lv_descr
                                SEPARATED BY space.

    lv_creator = sy-uname.

    CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
      EXPORTING
        archiv_id             = gc_a1
        arc_doc_id            = lv_docid
        ar_object             = lv_obj
        object_id             = lv_objid
        sap_object            = gc_bkpf
        doc_type              = lv_doc
        filename              = lv_path
        descr                 = lv_descr
        creator               = lv_creator
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.
    IF sy-subrc = 0.
      WRITE : / text-007.
    ELSE.
      WRITE : / text-017.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " UPLOAD_NET
*&---------------------------------------------------------------------*
*&      Form  INITIALISATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialisation .

  gs_extension-sign = 'I'.
  gs_extension-option = 'EQ'.
  gs_extension-low = gc_dc.
  APPEND gs_extension TO gr_extension.

  gs_extension-sign = 'I'.
  gs_extension-option = 'EQ'.
  gs_extension-low = gc_dx.
  APPEND gs_extension TO gr_extension.

  gs_extension-sign = 'I'.
  gs_extension-option = 'EQ'.
  gs_extension-low = gc_xs.
  APPEND gs_extension TO gr_extension.

  gs_extension-sign = 'I'.
  gs_extension-option = 'EQ'.
  gs_extension-low = gc_xx.
  APPEND gs_extension TO gr_extension.

  gs_extension-sign = 'I'.
  gs_extension-option = 'EQ'.
  gs_extension-low = gc_pf.
  APPEND gs_extension TO gr_extension.

  gs_extension-sign = 'I'.
  gs_extension-option = 'EQ'.
  gs_extension-low = gc_zp.
  APPEND gs_extension TO gr_extension.

  CONCATENATE text-019 sy-sysid '/' INTO p_file1.

ENDFORM.                    " INITIALISATION
