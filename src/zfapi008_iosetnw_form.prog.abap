*&---------------------------------------------------------------------*
*& Report  ZFAPI008_IOSETNW                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI008_IOSETNW                              *
*& Include Name       :  ZFAPI008_IOSETNW_FORM                         *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  22-May-2020                                   *
*& Change Request     :  CHG0180384                                    *
*& Purpose            :  Extract Internal Order and Network Settlement *
*&                       data                                          *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 22-May-2020  AHMADT        D30K930537 CHG0180384 Initial Development*
*&                            D30K930582                               *
*&                            D30K930596                               *
*& 01-Jul-2020  AHMADT        D30K930610 CHG0185539 Added 2 Reciever   *
*&                            D30K930633 types and alphanumeric field  *
*&                            D30K930637 adjustments                   *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_SS_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ss_output .
  LOOP AT SCREEN.
    IF screen-group1 EQ 'DSP'.
      IF screen-name = 'P_OBJTTL'.
        screen-input  = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF p_pres = 'X'.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PRE'.
          screen-input = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        WHEN 'APP'.
          screen-input = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ELSE.
    IF p_app = 'X'.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'PRE'.
            screen-input = 0.
            screen-invisible = 1.
            MODIFY SCREEN.
          WHEN 'APP'.
            screen-input = 1.
            screen-invisible = 0.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_SS_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_ORDER_EXTRACT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_order_extract .
  CLEAR: gt_aufk[],gt_cobrb[],gt_cobrb_k[],gt_cobrb_h[],
         gt_cobrb_p[],gt_prps1[], gt_cskt[], gt_skat[],
         gt_cobrb_a[], gt_anla[],gt_cobrb_1[]. " Added by AHMADT for CHG0185539

  SELECT aufnr objnr ktext erdat aedat autyp
    FROM aufk
INTO TABLE gt_aufk
   WHERE ( autyp = 1 OR
           autyp = 30 )
     AND   aufnr IN s_aufnr
     AND ( erdat IN s_erdat
      OR   aedat IN s_aedat ).
  IF gt_aufk IS NOT INITIAL.
    SORT gt_aufk BY objnr ASCENDING.
             SELECT objnr
                    perbz
                    prozs
                    letja
                    avorg
                    konty
                    bukrs
                    hkont
                    kostl
                    aufnr         "Added by AHMADT for CHG0185539 on 18/09/2020
                    ps_psp_pnr
                    anln1         "Added by AHMADT for CHG0185539 on 18/09/2020
               FROM cobrb
         INTO TABLE gt_cobrb
 FOR ALL ENTRIES IN gt_aufk
              WHERE objnr = gt_aufk-objnr.
    IF sy-subrc = 0.
* Start of changes by AHMADT for CHG0185539
      SORT gt_cobrb BY aufnr.
      gt_cobrb_1 = gt_cobrb.
      DELETE ADJACENT DUPLICATES FROM gt_cobrb_1 COMPARING aufnr.
             SELECT aufnr
                    objnr
                    ktext
                    erdat
                    aedat
                    autyp
              FROM  aufk
         INTO TABLE gt_aufk_a
 FOR ALL ENTRIES IN gt_cobrb_1
              WHERE aufnr = gt_cobrb_1-aufnr.
        IF gt_aufk_a IS NOT INITIAL.
           SORT gt_aufk_a BY aufnr.
        ENDIF.
* End of changes by AHMADT for CHG0185539
      SORT gt_cobrb BY objnr DESCENDING
                       avorg ASCENDING
                       prozs DESCENDING
                       letja DESCENDING.
      DELETE gt_cobrb WHERE avorg = space.

      gt_cobrb_k = gt_cobrb.
      SORT gt_cobrb_k BY kostl.
      DELETE ADJACENT DUPLICATES FROM gt_cobrb_k COMPARING kostl.
      SELECT spras kostl ktext
        FROM cskt
  INTO TABLE gt_cskt FOR ALL ENTRIES IN gt_cobrb_k
       WHERE spras = 'EN'
         AND kostl = gt_cobrb_k-kostl.
      IF sy-subrc = 0.
        SORT gt_cskt BY kostl.
      ENDIF.
      gt_cobrb_h = gt_cobrb.
      SORT gt_cobrb_h BY hkont.
      DELETE ADJACENT DUPLICATES FROM gt_cobrb_h COMPARING hkont.
      SELECT spras saknr txt20
        FROM skat
    INTO TABLE gt_skat FOR ALL ENTRIES IN gt_cobrb_h
       WHERE saknr = gt_cobrb_h-hkont
         AND spras = 'EN'.
      IF sy-subrc = 0.
        SORT gt_skat BY saknr.
      ENDIF.
      gt_cobrb_p = gt_cobrb.
      SORT gt_cobrb_p BY ps_psp_pnr.
      DELETE ADJACENT DUPLICATES FROM gt_cobrb_p COMPARING ps_psp_pnr.
      SELECT pspnr post1
        FROM prps
    INTO TABLE gt_prps1 FOR ALL ENTRIES IN gt_cobrb_p
       WHERE pspnr = gt_cobrb_p-ps_psp_pnr.
      IF sy-subrc = 0.
        SORT gt_prps1 BY pspnr.
      ENDIF.
* Start of changes by AHMADT for CHG0185539
      gt_cobrb_a = gt_cobrb.
      SORT gt_cobrb_a BY anln1.
      DELETE ADJACENT DUPLICATES FROM gt_cobrb_a COMPARING anln1.
             SELECT anln1
                    anln2
                    txt50
               FROM anla
         INTO TABLE gt_anla
 FOR ALL ENTRIES IN gt_cobrb_a
              WHERE anln1 = gt_cobrb_a-anln1.
       IF sy-subrc = 0.
         SORT gt_anla BY anln1.
       ENDIF.
* End of changes by AHMADT for CHG0185539
    ENDIF.
  ENDIF.

*  Populate Internal Table for Internal Orders
  CLEAR: gs_aufk.
  gs_order-sender_type     = text-005.
  gs_order-receiver_type   = text-006.
  gs_order-sender          = text-007.
  gs_order-receiver        = text-008.
  gs_order-receiver_type   = text-009.
  gs_order-receiver_text   = text-010.
  gs_order-sender_text     = text-011.
  gs_order-percent         = text-012.
  gs_order-settlement_type = text-013.
  gs_order-activity        = text-014.
  INSERT gs_order INTO gt_order INDEX 1.
  CLEAR: GS_ORDER.

  LOOP AT gt_aufk INTO gs_aufk.
    SHIFT gs_aufk-aufnr LEFT DELETING LEADING '0'.
    gs_order-sender        =   gs_aufk-aufnr.                "2.Sender
    gs_order-sender_text   =   gs_aufk-ktext.                "6.Sender_text
    gs_order-sender_type   =   text-015.                     "1.Sender Type = ORD

    READ TABLE gt_cobrb INTO gs_cobrb
                        WITH KEY objnr = gs_aufk-objnr.

    IF sy-subrc EQ 0.
      gs_order-percent      =   gs_cobrb-prozs.                "7.Percent
      CALL FUNCTION 'CONVERSION_EXIT_PERBZ_OUTPUT'
        EXPORTING
          input  = gs_cobrb-perbz
        IMPORTING
          output = gs_order-settlement_type.      "8.Settlement Type

      IF gs_cobrb-avorg     =   'KOAO'.                        "9.Activty
        gs_order-activity   =    text-019.
      ELSEIF gs_cobrb-avorg =   'KOAP'.
        gs_order-activity   =    text-020.
      ENDIF.

      CASE gs_cobrb-konty.
        WHEN 'KS'.
          READ TABLE gt_cskt  INTO gs_cskt  WITH KEY  kostl = gs_cobrb-kostl BINARY SEARCH.
          IF sy-subrc = 0.
            gs_order-receiver      = gs_cobrb-kostl.        "3.Receiver
            gs_order-receiver_type = text-016.              "4.Receiver Type
            gs_order-receiver_text = gs_cskt-ktext.         "5.Receiver Text
          ENDIF.

        WHEN 'PR'.
          READ TABLE gt_prps1 INTO gs_prps1 WITH KEY  pspnr = gs_cobrb-ps_psp_pnr BINARY SEARCH.
          IF  sy-subrc = 0.
            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
              EXPORTING
                input  = gs_cobrb-ps_psp_pnr
              IMPORTING
                output = gv_pnr.

            SHIFT gv_pnr LEFT DELETING LEADING '0'.
* Start of changes by AHMADT for CHG0185539
            IF strlen( gv_pnr ) < 14.
              CONCATENATE gc_zero gv_pnr
                     INTO gv_pnr.
            ENDIF.
* End of changes by AHMADT for CHG0185539
            gs_order-receiver      = gv_pnr.                "3.Receiver
            gs_order-receiver_type = text-017.              "4.Receiver Type
            gs_order-receiver_text = gs_prps1-post1.        "5.Receiver Text
          ENDIF.

        WHEN 'SK'.
          READ TABLE gt_skat  INTO gs_skat  WITH KEY  saknr = gs_cobrb-hkont BINARY SEARCH.
          IF sy-subrc = 0.
            SHIFT gs_cobrb-hkont LEFT DELETING LEADING '0'.
            gs_order-receiver      = gs_cobrb-hkont.        "3.Receiver
            gs_order-receiver_type = text-018.              "4.Receiver Type
            gs_order-receiver_text = gs_skat-txt20.         "5.Receiver Text
          ENDIF.
* Start of changes by AHMADT for CHG0185539
         WHEN 'AN'.
           READ TABLE gt_anla INTO gs_anla WITH KEY anln1 = gs_cobrb-anln1 BINARY SEARCH.
           IF sy-subrc = 0.
            SHIFT gs_anla-anln1 LEFT DELETING LEADING '0'.
            CONCATENATE gs_anla-anln1
                        gc_dash
                        gs_anla-anln2
                   INTO gs_order-receiver.                  "3.Receiver
            gs_order-receiver_type = text-028.              "4.Receiver Type
            gs_order-receiver_text = gs_anla-txt50.         "5.Receiver Text
           ENDIF.

         WHEN 'OR'.
            READ TABLE gt_aufk_a INTO gs_aufk_a WITH KEY aufnr = gs_cobrb-aufnr BINARY SEARCH.
            IF sy-subrc = 0.
               gs_order-receiver_text = gs_aufk_a-ktext.     "5.Receiver Text
            ENDIF.
            SHIFT gs_cobrb-aufnr LEFT DELETING LEADING '0'.
            gs_order-receiver      = gs_cobrb-aufnr.        "3.Receiver
            gs_order-receiver_type = text-029.
* End of changes by AHMADT for CHG0185539
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
    SHIFT gs_cobrb-bukrs LEFT DELETING LEADING '0'.
    SHIFT gs_cobrb-kostl LEFT DELETING LEADING '0'.
    REPLACE ALL OCCURRENCES OF gc_comma IN gs_order-sender_text   WITH gc_under.
    REPLACE ALL OCCURRENCES OF gc_comma IN gs_order-receiver_text WITH gc_under.
* Start of changes by AHMADT for CHG0185539
      CONDENSE gs_order-sender_type     NO-GAPS.
      CONDENSE gs_order-receiver_type   NO-GAPS.
      CONDENSE gs_order-settlement_type NO-GAPS.
      CONDENSE gs_order-activity        NO-GAPS.
      CONDENSE gs_order-percent         NO-GAPS.
* End of changes by AHMADT for CHG0185539

    IF gs_order-receiver IS NOT INITIAL.
      APPEND gs_order TO gt_order.
    ENDIF.
    CLEAR: gs_aufk,gs_order, gs_cobrb,gs_skat,gs_cskt,gs_prps1,gv_pnr,
           gs_anla,gs_aufk_a. "Added by AHMADT for CHG0185539
  ENDLOOP.
ENDFORM.                    " F_ORDER_EXTRACT
*&---------------------------------------------------------------------*
*&      Form  F_SS_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ss_initialization.

  CLEAR gv_obj_id.
  SELECT SINGLE value1
           FROM zfit_xparam
           INTO gv_obj_id
          WHERE paramtype = gc_paramtype
            AND subtype   = gc_subtype
            AND key1      = gc_obj_id. "#EC WARNOK Message Code WRN 1305

  IF gv_obj_id IS NOT INITIAL.
    p_objid = gv_obj_id.
  ENDIF.

  CLEAR gv_objttl.
  SELECT SINGLE value1                "#EC WARNOK Message Code WRN 1305
           FROM zfit_xparam
           INTO gv_objttl
          WHERE paramtype = gc_paramtype
            AND subtype   = gc_subtype
            AND key1      = gc_obj_ttl.

  IF gv_objttl IS NOT INITIAL.
    p_objttl = gv_objttl.
  ENDIF.

  CLEAR gt_zfit[].
  SELECT paramtype subtype key1 value1 value2
    FROM zfit_xparam
 INTO TABLE gt_zfit
    WHERE paramtype = gc_rep
      AND subtype   = gc_file_path.

  IF sy-subrc = 0.
    SORT gt_zfit BY key1.
    READ TABLE gt_zfit INTO gs_zfit WITH KEY key1 = '1' BINARY SEARCH.
    IF sy-subrc = 0.
      gs_zfit-value2+15(8) = sy-datum.
      gs_zfit-value2+23(1) = gc_under.
      gs_zfit-value2+24(6) = sy-uzeit.
      gs_zfit-value2+30(4) = gc_csv.
      CONCATENATE gs_zfit-value1 gs_zfit-value2 INTO p_ord_a.

    ENDIF.

    READ TABLE gt_zfit INTO gs_zfit WITH KEY key1 = '2' BINARY SEARCH.
    IF sy-subrc = 0.
      gs_zfit-value2+11(8) = sy-datum.
      gs_zfit-value2+19(1) = gc_under.
      gs_zfit-value2+20(6) = sy-uzeit.
      gs_zfit-value2+26(4) = gc_csv.
      CONCATENATE gs_zfit-value1 gs_zfit-value2 INTO p_wbs_a.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_SS_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  F_DOWN_TO_PRES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_down_to_pres USING gt_table
                          gv_path1.
  CLEAR gv_path.
  gv_path = gv_path1.
  IF gv_path IS NOT INITIAL.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = gv_path
        filetype                = 'ASC'
        write_field_separator   = 'X'
      CHANGING
        data_tab                = gt_table
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
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    MESSAGE text-025 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " F_DOWN_TO_PRES
*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_ORDER_APP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_upload_order_app.
  IF p_ord_a IS NOT INITIAL.
    OPEN DATASET p_ord_a FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      MESSAGE text-027 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    LOOP AT gt_order INTO gs_order.
      MOVE gs_order-percent TO gv_percent.
      CONCATENATE gs_order-sender_type
                  gs_order-receiver_type
                  gs_order-sender
                  gs_order-receiver
                  gs_order-sender_text
                  gs_order-receiver_text
                  gv_percent
                  gs_order-settlement_type
                  gs_order-activity
             INTO gv_data
     SEPARATED BY ','.
      TRANSFER gv_data TO p_ord_a.
      CLEAR: gs_order, gv_data, gv_percent.
    ENDLOOP.
    CLOSE DATASET p_ord_a.
  ENDIF.
ENDFORM.                    " F_UPLOAD_ORDER_APP
*&---------------------------------------------------------------------*
*&      Form  F_WBS_EXTRACT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_wbs_extract.
  CLEAR gt_prps[].
  SELECT pspnr poski psphi post1 erdat aedat
    FROM prps
INTO TABLE gt_prps
   WHERE pspnr IN s_pspnr
     AND ( erdat IN s_perdat
      OR aedat IN s_paedat ).
  IF sy-subrc <> 0.
    SORT gt_prps BY pspnr.
  ENDIF.

  CLEAR gs_proj.
  gs_proj-pspnr = text-021.
  gs_proj-poski = text-022.
  gs_proj-psphi = text-023.
  gs_proj-post1 = text-024.
  APPEND gs_proj TO gt_proj.

  LOOP AT  gt_prps INTO gs_prps.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = gs_prps-pspnr
      IMPORTING
        output = gs_proj-pspnr.
    gs_proj-poski = gs_prps-poski.

    CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
      EXPORTING
        input  = gs_prps-psphi
      IMPORTING
        output = gs_proj-psphi.
    gs_proj-post1 = gs_prps-post1.
*    Start of changes by AHMADT for CHG0185539
*    REPLACE ALL OCCURRENCES OF gc_comma IN gs_prps-post1 WITH gc_under.
    REPLACE ALL OCCURRENCES OF gc_comma
                            IN gs_proj-post1
                          WITH gc_under.
*    End of changes by AHAMDT for CHG0185539
    APPEND gs_proj TO gt_proj.
    CLEAR :gs_proj,gs_prps.
  ENDLOOP.
ENDFORM.                    " F_WBS_EXTRACT
*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_PROJECT_APP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_upload_project_app .
  IF p_wbs_a IS NOT INITIAL.
    OPEN DATASET p_wbs_a FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      MESSAGE text-027 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    LOOP AT gt_proj INTO gs_proj.
      CONCATENATE gs_proj-pspnr
                  gs_proj-poski
                  gs_proj-psphi
                  gs_proj-post1
             INTO gv_data
     SEPARATED BY ','.
      TRANSFER gv_data TO p_wbs_a.
      CLEAR : gs_proj ,gv_data.
    ENDLOOP.
    CLOSE DATASET p_wbs_a.
  ENDIF.
ENDFORM.                    " F_UPLOAD_PROJECT_APP
