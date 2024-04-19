REPORT  zfapr019_parkedinv_noworkflow.
*&---------------------------------------------------------------------*
*& Report  ZFAPR019_PARKEDINV_NOWORKFLOW                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*  Author:      Brian Boundy                                           *
*  Date:        July, 2012.                                            *
*  Description: This uses ZFAPR018 Workflow list to list invoice with  *
*               no active workflow attached to them.                   *
*                                                                      *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 06/25/2014                                           *
* Modified By   : Praveena Anusuri                                     *
* Object ID     : SDP67946                                             *
* Description   : Display document contents and send email.            *
*----------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 09/24/2014                                           *
* Modified By   : Praveena Anusuri                                     *
* Object ID     : SDP69517                                             *
* Description   : Display parked invoices with no workflow.            *
*----------------------------------------------------------------------*
* Version No    : 3.0                                                  *
* Date          : 11/23/2014                                           *
* Modified By   : Praveena Anusuri                                     *
* Object ID     : SDP73685                                             *
* Description   : Added additional fields on the selection screen.     *
*                 Fiscal year, Company code, Vendor, User and Invoice  *
*                 amount.                                              *
*----------------------------------------------------------------------*
*Change-by Date         SDP        Change
*SAHMAD    2014/12/16   67979      Add Document type to output
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 4.0                                                  *
* Date          : 01/06/2015                                           *
* Modified By   : Santosh Kapase                                       *
* Object ID     : SDP79836                                             *
* Description   : Added additional fields on the selection screen.     *
*                 (1) Header Txt (PO Number) and  (2) Entry Date.      *
*----------------------------------------------------------------------*

TABLES: vbkpf,
        vbsegk,
        t001,
        lfa1,
        usr21.
TYPE-POOLS: slis.
TYPES:  BEGIN OF ty_nonpo,
          year        LIKE vbkpf-gjahr,
          docnum      LIKE vbkpf-belnr,
          cocode      LIKE vbkpf-ausbk,
        END OF ty_nonpo,

        BEGIN OF ty_invoice,
          year        LIKE vbkpf-gjahr,
          docnum      LIKE vbkpf-belnr,
          blart       LIKE vbkpf-blart,
          cocode      LIKE vbkpf-ausbk,
          vendor      TYPE vbsegk-lifnr,
          amount      LIKE vbsegk-dmbtr,
          sgtxt       LIKE vbsegk-sgtxt,
          cpudt       LIKE vbkpf-cpudt,
          usnam       LIKE vbkpf-usnam,
          tcode       LIKE vbkpf-tcode,
          "type(3)     TYPE c,
          awkey       TYPE char70,     "(+)PANUSURI Ticket 67946
          rec_fnam    TYPE so_rc_fnam, "(+)PANUSURI Ticket 67946
          doc_content TYPE string,     "(+)PANUSURI Ticket 67946
        END OF ty_invoice.

*BOI by PANUSURI Ticket 67946
TYPES: BEGIN OF ty_srbcsbrel,
       instid_a       TYPE sibfboriid,
       instid_b       TYPE sibfboriid,
       END OF ty_srbcsbrel,
       BEGIN OF ty_srbcsbrel_tmp,
       instid_a       TYPE sibfboriid,
       instid_b       TYPE os_guid,
       END OF ty_srbcsbrel_tmp,
       BEGIN OF ty_bcst_sr,
       os_guid        TYPE os_guid,
       doc_cls        TYPE os_guid,
       doc_oid        TYPE os_guid,
       END OF ty_bcst_sr,
       BEGIN OF ty_sood,
       objtp          TYPE so_obj_tp,
       objyr          TYPE so_obj_yr,
       objno          TYPE so_obj_no,
       if_doc_bcs     TYPE os_guid,
       if_doc_cls     TYPE os_guid,
       END OF ty_sood,
       BEGIN OF ty_sofm,
       foltp          TYPE so_fol_tp,
       folyr          TYPE so_fol_yr,
       folno          TYPE so_fol_no,
       doctp          TYPE so_doc_tp,
       docyr          TYPE so_doc_yr,
       docno          TYPE so_doc_no,
       END OF ty_sofm.

DATA: lt_srbcsbrel      TYPE STANDARD TABLE OF ty_srbcsbrel,
      lwa_srbcsbrel     TYPE ty_srbcsbrel,
      lt_srbcsbrel_tmp  TYPE STANDARD TABLE OF ty_srbcsbrel_tmp,
      lwa_srbcsbrel_tmp TYPE ty_srbcsbrel_tmp,
      lt_bcst_sr        TYPE STANDARD TABLE OF ty_bcst_sr,
      lwa_bcst_sr       TYPE ty_bcst_sr,
      lt_sood           TYPE STANDARD TABLE OF ty_sood,
      lwa_sood          TYPE ty_sood,
      lt_sofm           TYPE STANDARD TABLE OF ty_sofm,
      lwa_sofm          TYPE ty_sofm,
      lwa_filter        TYPE sofilteri1,
      lt_receiver_list  TYPE STANDARD TABLE OF soreclsti1,
      lwa_receiver_list TYPE soreclsti1,
      lv_document_id    TYPE sofolenti1-doc_id,
      lwa_data          TYPE sofolenti1,
      lt_content        TYPE STANDARD TABLE OF solisti1,
      lwa_content       TYPE solisti1,
      lv_content        TYPE string,
      lv_address        TYPE adr6-smtp_addr,
      lit_text          TYPE bcsy_text.

CONSTANTS: lc_reltype  TYPE oblreltype VALUE 'BCSAPPLSRQ',
           lc_typeid_a TYPE sibftypeid VALUE 'BUS2081',
           lc_catid_a  TYPE sibfcatid  VALUE 'BO'.

FIELD-SYMBOLS: <fs_invoice> TYPE ty_invoice.
*EOI by PANUSURI Ticket 67946

DATA:   gs_workflow TYPE ty_nonpo,
        gt_workflow LIKE TABLE OF gs_workflow,
        gs_invoice TYPE ty_invoice,
        gt_invoice LIKE TABLE OF gs_invoice,
        ls_vbkpf    TYPE vbkpf,
        ls_vbsegk   TYPE vbsegk,
        gv_return   TYPE integer,
        gv_lines    TYPE integer,
        gv_percent  TYPE integer,
        gv_mod      TYPE integer,
        gv_curper   TYPE integer.
*BOI by PANUSURI Ticket 69517
DATA:   lv_objtype  TYPE swotobjid-objtype,
        lv_doc      TYPE awkey,
        lv_objkey   TYPE swotobjid-objkey,
        lt_worklist TYPE STANDARD TABLE OF swr_wihdr.
*EOI by PANUSURI Ticket 69517

DATA: lv_rmwwr TYPE rmwwr.

SELECTION-SCREEN BEGIN OF BLOCK a1.
SELECT-OPTIONS: s_belnr  FOR vbkpf-belnr,
*BOI by PANUSURI Ticket 73685
                s_gjahr  FOR vbkpf-gjahr,
                s_bukrs  FOR t001-bukrs,
                s_user   FOR usr21-bname,
                s_lifnr  FOR lfa1-lifnr,
                s_dmbtr  FOR vbsegk-dmbtr,
*EOI by PANUSURI Ticket 73685
*BOI by SAKAPSE Ticket 79836
               S_SGTXT FOR VBSEGK-SGTXT,
               S_CPUDT FOR VBKPF-CPUDT.
*EOI by SAKAPSE Ticket 79836
SELECTION-SCREEN END OF BLOCK a1.

***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
START-OF-SELECTION.

*  PERFORM get_db_data. "(-)PANUSURI ticket 69517
  PERFORM get_invoice_detail.
  PERFORM display_alv.

***********************************************************************
*                      Get data from DB                               *
***********************************************************************
FORM get_db_data.
  DATA: ls_swwwihead  TYPE swwwihead,
        ls_lastitem   TYPE swwwihead,
        lt_swwwihead  LIKE TABLE OF ls_swwwihead,

        ls_container  TYPE swr_cont,
        lt_container  LIKE TABLE OF ls_container,

        ls_recipient  TYPE swragent,
        lt_recipient  LIKE TABLE OF ls_recipient.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 1
      text       = 'Looking for AP Workflow'.

  SELECT *
    FROM swwwihead
    INTO CORRESPONDING FIELDS OF TABLE lt_swwwihead
    WHERE wi_stat <> 'CANCELLED'
      AND wi_stat <> 'COMPLETED'
      AND ( wi_rh_task = 'WS02000002'
        OR  wi_rh_task = 'WS02000003' )
  .

  DESCRIBE TABLE lt_swwwihead LINES gv_lines.

  gv_percent = gv_lines DIV 20.

  LOOP AT lt_swwwihead INTO ls_swwwihead.
    gv_mod = sy-tabix MOD gv_percent.

    IF gv_mod = 0.
      gv_curper = sy-tabix DIV gv_percent.
      gv_curper = gv_curper * 4.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = gv_curper
          text       = 'Getting Workflow Detail'.
    ENDIF.

    IF ls_swwwihead-wi_rh_task = 'WS02000002'.
      "Non PO
      CLEAR gs_workflow.
      CLEAR lt_container.
      CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
        EXPORTING
          workitem_id      = ls_swwwihead-wi_id
        IMPORTING
          return_code      = gv_return
        TABLES
          simple_container = lt_container.

      IF gv_return = 0.
        LOOP AT lt_container INTO ls_container.
          CASE ls_container-element.
            WHEN 'FIPP'.
              gs_workflow-cocode  = ls_container-value+20(4).
              gs_workflow-docnum  = ls_container-value+24(10).
              gs_workflow-year    = ls_container-value+34(4).
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ELSEIF ls_swwwihead-wi_rh_task = 'WS02000003'.
      "SRO Workflow
      CLEAR gs_workflow.
      CLEAR lt_container.
      CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
        EXPORTING
          workitem_id      = ls_swwwihead-wi_id
        IMPORTING
          return_code      = gv_return
        TABLES
          simple_container = lt_container.
      IF gv_return = 0.
        LOOP AT lt_container INTO ls_container.
          CASE ls_container-element.
            WHEN 'INVOICE'.
              gs_workflow-docnum  = ls_container-value+20(10).
              gs_workflow-year    = ls_container-value+30(4).
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDIF.
    APPEND gs_workflow TO gt_workflow.
  ENDLOOP.
ENDFORM.                    "get_db_data


***********************************************************************
*                      Get data from DB                               *
***********************************************************************
FORM get_invoice_detail.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = 'Getting All Invoices'.

*  SORT gt_workflow.  "(-)PANUSURI ticket 69517

*  SELECT ausbk belnr gjahr bstat cpudt usnam tcode
  "(-)PANUSURI ticket 67946
  SELECT ausbk belnr gjahr blart bstat cpudt usnam tcode awtyp awkey
                               "(+)PANUSURI ticket 67946
    INTO CORRESPONDING FIELDS OF ls_vbkpf
    FROM vbkpf
    WHERE ausbk IN s_bukrs            "(+)PANUSURI Ticket 73685
    AND   belnr IN s_belnr
    AND   gjahr IN s_gjahr           "(+)PANUSURI Ticket 73685
     AND  cpudt IN s_cpudt .          "(+)SAKAPSE Ticket 79386

    IF sy-subrc = 0.
      CLEAR ls_vbsegk.
      SELECT SINGLE dmbtr sgtxt lifnr
        INTO CORRESPONDING FIELDS OF ls_vbsegk
        FROM vbsegk
        WHERE ausbk = ls_vbkpf-ausbk
        AND   belnr = ls_vbkpf-belnr
        AND   gjahr = ls_vbkpf-gjahr
        AND   dmbtr IN s_dmbtr       "(+)PANUSURI Ticket 73685
        AND   lifnr IN s_lifnr       "(+)PANUSURI Ticket 73685
        AND  sgtxt IN s_sgtxt.      "(+)SAKAPSE Ticket 79836


      IF sy-subrc = 0.

*BOC by PANUSURI Ticket 69517
*    READ TABLE gt_workflow INTO gs_workflow
*      WITH KEY year   = ls_vbkpf-gjahr
*               docnum = ls_vbkpf-belnr.
*    "cocode = ls_vbkpf-ausbk.
*
*    IF sy-subrc = 0.
*      "Check if it has a Company Code
*      IF gs_workflow-cocode IS INITIAL.
*        "PO Invoice, with Workflow
*        CONTINUE.
*      ELSE.
*        "NonPOInvoice, confirm right CoCode
*        READ TABLE gt_workflow INTO gs_workflow
*          WITH KEY year   = ls_vbkpf-gjahr
*                   docnum = ls_vbkpf-belnr
*                   cocode = ls_vbkpf-ausbk.
*        IF sy-subrc = 0.
*          "NPO Invoice, with Workflwo
*          CONTINUE.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*EOC by PANUSURI Ticket 69517
*BOI by PANUSURI Ticket 69517
        CLEAR: lv_objtype,
               lv_doc,
               lv_objkey,
               gv_return.
        REFRESH: lt_worklist.

        IF ls_vbkpf-awtyp = 'RMRP'.
          lv_objtype = 'BUS2081'.
          CONCATENATE ls_vbkpf-awkey(10) ls_vbkpf-awkey+10(4) INTO lv_objkey.
        ELSEIF ls_vbkpf-awtyp = 'BKPF'.
          lv_objtype = 'FIPP'.
*      CONCATENATE ls_vbkpf-awkey(10) ls_vbkpf-awkey+14(4) INTO lv_doc.
          CONCATENATE ls_vbkpf-belnr ls_vbkpf-gjahr INTO lv_doc.
          IF strlen( ls_vbkpf-ausbk ) = '2'.
            CONCATENATE ls_vbkpf-ausbk ' ' lv_doc INTO lv_objkey SEPARATED BY space.
          ELSEIF strlen( ls_vbkpf-ausbk ) = '3'.
            CONCATENATE ls_vbkpf-ausbk lv_doc INTO lv_objkey SEPARATED BY space.
          ELSE.
            CONCATENATE ls_vbkpf-ausbk lv_doc INTO lv_objkey.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
          EXPORTING
            objtype                  = lv_objtype
            objkey                   = lv_objkey
            top_level_items          = ' '
            selection_status_variant = '0000'
            text                     = 'X'
            output_only_top_level    = ' '
            language                 = sy-langu
            determine_task_filter    = ' '
          IMPORTING
            return_code              = gv_return
          TABLES
            worklist                 = lt_worklist.
        IF gv_return = 0.
          IF lt_worklist IS NOT INITIAL.  "(+)PANUSURI Ticket 69517
            DELETE lt_worklist WHERE wi_rh_task = 'TS30001128'."(+)PANUSURI Ticket 69517
            IF lt_worklist IS NOT INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF."(+)PANUSURI Ticket 69517
        ENDIF.
*EOI by PANUSURI Ticket 69517

        "If we get here, we need to add to list
        gs_invoice-year   = ls_vbkpf-gjahr.
        gs_invoice-docnum = ls_vbkpf-belnr.
        gs_invoice-blart  = ls_vbkpf-blart.
        gs_invoice-cocode = ls_vbkpf-ausbk.
        gs_invoice-cpudt  = ls_vbkpf-cpudt.
        gs_invoice-usnam  = ls_vbkpf-usnam.
        gs_invoice-tcode  = ls_vbkpf-tcode.
        gs_invoice-awkey  = ls_vbkpf-awkey. "(+)PANUSURI Ticket 67946

        IF ls_vbsegk IS NOT INITIAL.
          gs_invoice-vendor = ls_vbsegk-lifnr.
          gs_invoice-amount = ls_vbsegk-dmbtr.
          gs_invoice-sgtxt  = ls_vbsegk-sgtxt.
          "gs_invoice-type   = 'PO'.
          SHIFT gs_invoice-vendor LEFT DELETING LEADING '0'.
        ELSE.
          "gs_invoice-type = 'NPO'.
        ENDIF.

        "Check for just the invoice
        APPEND gs_invoice TO gt_invoice.

      ENDIF.
    ENDIF.
  ENDSELECT.

*BOI by PANUSURI Ticket 67946
  PERFORM get_document_content.
*EOI by PANUSURI Ticket 67946

ENDFORM.                    "get_invoice_detail

***********************************************************************
*                      Display ALV Grid                               *
***********************************************************************
FORM display_alv.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat LIKE LINE OF lt_fieldcat,
        ls_layout   TYPE slis_layout_alv,
        ls_variant  LIKE disvariant,
        lt_sort     TYPE slis_t_sortinfo_alv,
        ls_sort     LIKE LINE OF lt_sort.
  "Create field catalog
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'YEAR'.
  ls_fieldcat-seltext_m   = 'Year'.
  ls_fieldcat-col_pos     = 1.
  ls_fieldcat-outputlen   = 5.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'DOCNUM'.
  ls_fieldcat-seltext_m   = 'Parked Doc'.
  ls_fieldcat-col_pos     = 2.
  ls_fieldcat-outputlen   = 10.
  ls_fieldcat-hotspot     = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'BLART'.
  ls_fieldcat-seltext_m   = 'Doc.Type'.
  ls_fieldcat-col_pos     = 3.
  ls_fieldcat-outputlen   = 3.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'COCODE'.
  ls_fieldcat-seltext_m   = 'CoCd'.
  ls_fieldcat-col_pos     = 4.
  ls_fieldcat-outputlen   = 4.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'VENDOR'.
  ls_fieldcat-seltext_m   = 'Vendor'.
  ls_fieldcat-col_pos     = 5.
  ls_fieldcat-outputlen   = 6.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'AMOUNT'.
  ls_fieldcat-seltext_m   = 'Amount'.
  ls_fieldcat-col_pos     = 6.
  ls_fieldcat-outputlen   = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'SGTXT'.
  ls_fieldcat-seltext_m   = 'Header Txt'.
  ls_fieldcat-col_pos     = 7.
  ls_fieldcat-outputlen   = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'CPUDT'.
  ls_fieldcat-seltext_m   = 'Entry Date'.
  ls_fieldcat-col_pos     = 8.
  ls_fieldcat-outputlen   = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'USNAM'.
  ls_fieldcat-seltext_m   = 'User'.
  ls_fieldcat-col_pos     = 9.
  ls_fieldcat-outputlen   = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'TCODE'.
  ls_fieldcat-seltext_m   = 'TCode'.
  ls_fieldcat-col_pos     = 10.
  ls_fieldcat-outputlen   = 5.
  APPEND ls_fieldcat TO lt_fieldcat.

*  CLEAR ls_fieldcat.
*  ls_fieldcat-fieldname   = 'TYPE'.
*  ls_fieldcat-seltext_m   = 'PO/NPO'.
*  ls_fieldcat-col_pos     = 6.
*  ls_fieldcat-outputlen   = 4.
*  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'AWKEY'.
  ls_fieldcat-seltext_m   = 'Invoice Document'.
  ls_fieldcat-no_out      = 'X'.
  ls_fieldcat-tech        = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'REC_FNAM'.
  ls_fieldcat-seltext_m   = 'Recipient List'.
  ls_fieldcat-col_pos     = 11.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'DOC_CONTENT'.
  ls_fieldcat-seltext_m   = 'Document Content'.
  ls_fieldcat-col_pos     = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_layout-colwidth_optimize = 'X'.

  "Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat             = lt_fieldcat
      is_layout               = ls_layout
      i_callback_program      = sy-repid
                                                                                                                                                                                                       "i_callback_top_of_page  = 'ALV_TOP_OF_PAGE'
      i_callback_user_command = 'ALV_USER_COMMAND'
      i_save                  = 'A'
      is_variant              = ls_variant
      it_sort                 = lt_sort
*     it_events               = i_event
    TABLES
      t_outtab                = gt_invoice
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

ENDFORM.                    "display_alv


*************************************************************
*                        TOP OF PAGE                        *
*************************************************************
FORM alv_user_command USING lv_ucomm     LIKE sy-ucomm
                            ls_selfield TYPE slis_selfield.

  IF ls_selfield-tabindex LE 0 OR ls_selfield-sumindex > 0.
    EXIT.
  ENDIF.
  CLEAR gs_invoice.
  READ TABLE gt_invoice INTO gs_invoice INDEX ls_selfield-tabindex.

  CASE lv_ucomm.
    WHEN '&IC1'.
      CASE ls_selfield-fieldname.
        WHEN 'DOCNUM'.
          "IF gs_invoice-type = 'NPO'.
          SET PARAMETER ID 'BUK' FIELD gs_invoice-cocode.
          SET PARAMETER ID 'BLP' FIELD gs_invoice-docnum.
          SET PARAMETER ID 'GJR' FIELD gs_invoice-year.
          CALL TRANSACTION 'FBV3' AND SKIP FIRST SCREEN.
          "ELSE.
          "SET PARAMETER ID 'RBN' FIELD gs_invoice-docnum.
          "SET PARAMETER ID 'GJR' FIELD gs_invoice-year.
          "CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
          "ENDIF.
      ENDCASE.
  ENDCASE.
ENDFORM.                    "alv_user_command


*************************************************************
*                        TOP OF PAGE                        *
*************************************************************
FORM alv_top_of_page.
  DATA: ls_line TYPE slis_listheader.
  DATA: lt_top_of_page TYPE slis_t_listheader.

*1- Heading Line: Type H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = sy-title.             "sy-title.
  APPEND ls_line TO lt_top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.
ENDFORM.                    "ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  GET_DOCUMENT_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_document_content .
  DATA: gt_invoice_tmp TYPE STANDARD TABLE OF ty_invoice.

  IF gt_invoice IS NOT INITIAL.
    SELECT instid_a
           instid_b
           FROM srbcsbrel
           INTO TABLE lt_srbcsbrel
           FOR ALL ENTRIES IN gt_invoice
           WHERE reltype  = lc_reltype
           AND   instid_a = gt_invoice-awkey
           AND   typeid_a = lc_typeid_a
           AND   catid_a  = lc_catid_a.
    IF sy-subrc = 0.
      SORT lt_srbcsbrel BY instid_a instid_b DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_srbcsbrel COMPARING instid_a.
    ENDIF.
  ENDIF.
  IF lt_srbcsbrel IS NOT INITIAL.
    LOOP AT lt_srbcsbrel INTO lwa_srbcsbrel.
      lwa_srbcsbrel_tmp-instid_a = lwa_srbcsbrel-instid_a.
      lwa_srbcsbrel_tmp-instid_b = lwa_srbcsbrel-instid_b.
      APPEND lwa_srbcsbrel_tmp TO lt_srbcsbrel_tmp.
      CLEAR lwa_srbcsbrel_tmp.
    ENDLOOP.
    SELECT os_guid
           doc_cls
           doc_oid
           FROM bcst_sr
           INTO TABLE lt_bcst_sr
           FOR ALL ENTRIES IN lt_srbcsbrel_tmp
           WHERE os_guid = lt_srbcsbrel_tmp-instid_b
           AND   doc_cls <> space
           AND   doc_oid <> space.
    IF lt_bcst_sr IS NOT INITIAL.
      SELECT objtp
             objyr
             objno
             if_doc_bcs
             if_doc_cls
             FROM sood
             INTO TABLE lt_sood
             FOR ALL ENTRIES IN lt_bcst_sr
             WHERE if_doc_bcs = lt_bcst_sr-doc_oid
             AND   if_doc_cls = lt_bcst_sr-doc_cls.
    ENDIF.
    IF lt_sood IS NOT INITIAL.
      SELECT foltp
             folyr
             folno
             doctp
             docyr
             docno
             FROM sofm
             INTO TABLE lt_sofm
             FOR ALL ENTRIES IN lt_sood
             WHERE doctp = lt_sood-objtp
             AND   docyr = lt_sood-objyr
             AND   docno = lt_sood-objno.
      IF sy-subrc = 0.
        SORT lt_sofm BY doctp docyr docno.
        DELETE ADJACENT DUPLICATES FROM lt_sofm COMPARING doctp docyr
                                                          docno.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT gt_invoice ASSIGNING <fs_invoice>.
    CLEAR: lwa_srbcsbrel_tmp,
           lwa_bcst_sr,
           lwa_sood,
           lwa_sofm,
           lv_document_id,
           lwa_filter,
           lwa_content,
           lwa_receiver_list.
    READ TABLE lt_srbcsbrel_tmp INTO lwa_srbcsbrel_tmp WITH KEY
                                instid_a = <fs_invoice>-awkey.
    IF sy-subrc = 0.
      READ TABLE lt_bcst_sr INTO lwa_bcst_sr WITH KEY
                            os_guid = lwa_srbcsbrel_tmp-instid_b.
      IF sy-subrc = 0.
        READ TABLE lt_sood INTO lwa_sood WITH KEY
                           if_doc_bcs = lwa_bcst_sr-doc_oid
                           if_doc_cls = lwa_bcst_sr-doc_cls.
        IF sy-subrc = 0.
          READ TABLE lt_sofm INTO lwa_sofm WITH KEY
                             doctp = lwa_sood-objtp
                             docyr = lwa_sood-objyr
                             docno = lwa_sood-objno.
          IF sy-subrc = 0.
            CLEAR: lv_document_id,
                   lwa_filter.
            REFRESH: lt_content,
                     lt_receiver_list.
            CONCATENATE lwa_sofm-foltp lwa_sofm-folyr lwa_sofm-folno
                        lwa_sofm-doctp lwa_sofm-docyr lwa_sofm-docno
                        INTO lv_document_id.

            lwa_filter-rec_list = 'X'.

            CALL FUNCTION 'SO_DOCUMENT_READ_API1'
              EXPORTING
                document_id                = lv_document_id
                filter                     = lwa_filter
              TABLES
                object_content             = lt_content
                receiver_list              = lt_receiver_list
              EXCEPTIONS
                document_id_not_exist      = 1
                operation_no_authorization = 2
                x_error                    = 3
                OTHERS                     = 4.

            IF sy-subrc = 0.
              CLEAR lv_address.
              READ TABLE lt_receiver_list INTO lwa_receiver_list
                                                        INDEX 1.
              IF sy-subrc = 0.
                <fs_invoice>-rec_fnam = lwa_receiver_list-rec_fnam.
                lv_address = lwa_receiver_list-rec_fnam.
              ENDIF.
*             BOI by PANUSURI Ticket 73685
              IF lwa_receiver_list-receiver IN s_user.
              ELSE.
                CONTINUE.
              ENDIF.
*             EOI by PANUSURI Ticket 73685
              IF lt_content IS NOT INITIAL.
                CLEAR: lv_content,
                       lit_text.
                REFRESH lit_text.
                READ TABLE lt_content INTO lwa_content INDEX 1.
                IF sy-subrc = 0.
                  lv_content = lwa_content-line.
                  APPEND lwa_content-line TO lit_text.
                  CLEAR lwa_content.
                ENDIF.
                REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
                IN lv_content WITH ''.
                <fs_invoice>-doc_content = lv_content.
              ENDIF.
              PERFORM send_mail.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*   BOI by PANUSURI Ticket 73685
    IF s_user IS NOT INITIAL.
      IF lwa_receiver_list-receiver IN s_user.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.
*   EOI by PANUSURI Ticket 73685

    APPEND <fs_invoice> TO gt_invoice_tmp."(+)PANUSURI Ticket 73685

    CLEAR: lwa_srbcsbrel_tmp,
           lwa_bcst_sr,
           lwa_sood,
           lwa_sofm,
           lwa_filter.
  ENDLOOP.
  gt_invoice[] = gt_invoice_tmp[].       "(+)PANUSURI Ticket 73685

  REFRESH: lt_srbcsbrel,
           lt_srbcsbrel_tmp,
           lt_bcst_sr,
           lt_sood,
           lt_sofm,
           gt_invoice_tmp.

ENDFORM.                    " GET_DOCUMENT_CONTENT
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_mail .
  DATA:  lcl_sendrequest       TYPE REF TO cl_bcs,
           lcl_document          TYPE REF TO cl_document_bcs,
           lcl_sender            TYPE REF TO cl_sapuser_bcs,
           lif_recipient         TYPE REF TO if_recipient_bcs,
           lcl_bcsexception      TYPE REF TO cx_bcs.

  DATA:   lv_mailsubject TYPE so_obj_des,
          lv_senttoall   TYPE os_boolean,
          lv_mailbody    TYPE so_text255.

  TRY.
*     Create persistent send request
      lcl_sendrequest = cl_bcs=>create_persistent( ).

*     Create document and subject of the email
      lcl_document = cl_document_bcs=>create_document(
                     i_type         = 'RAW'
                     i_text         = lit_text
                     i_length       = '14'
                     i_subject      = 'Please Approve').

*     Add document to send request
      CALL METHOD lcl_sendrequest->set_document( lcl_document ).

*     Set sender
      lcl_sender = cl_sapuser_bcs=>create( sy-uname ).

      CALL METHOD lcl_sendrequest->set_sender
        EXPORTING
          i_sender = lcl_sender.

*     Add recipient (e-mail address)
      lif_recipient = cl_cam_address_bcs=>create_internet_address( lv_address ).

      CALL METHOD lcl_sendrequest->add_recipient
        EXPORTING
          i_recipient = lif_recipient
          i_express   = 'X'.

*     Send document
      CALL METHOD lcl_sendrequest->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = lv_senttoall ).

      COMMIT WORK.
    CATCH cx_bcs INTO lcl_bcsexception.
  ENDTRY.

ENDFORM.                    " SEND_MAIL
