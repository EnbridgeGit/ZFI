*----------------------------------------------------------------------*
***INCLUDE LZAP_IAP_INVOICEF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_WBS_CONVERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM F_WBS_CONVERT  USING    lv_wbs_elem TYPE char24
                    CHANGING cv_WBS_ELEM TYPE PS_POSNR.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
    EXPORTING
      INPUT  = lv_wbs_elem
    IMPORTING
      OUTPUT = cv_WBS_ELEM.


ENDFORM.                    " F_WBS_CONVERT
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GL_CONVERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ACTG_GLACCT  text
*      <--P_LS_ACTG_DATA_GL_ACCOUNT  text
*----------------------------------------------------------------------*
FORM F_GL_CONVERT  USING    LV_GLACCT TYPE HKONT
                   CHANGING CV_GL_ACCOUNT TYPE SAKNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LV_GLACCT
    IMPORTING
      OUTPUT = CV_GL_ACCOUNT.


ENDFORM.                    " F_GL_CONVERT
*eject
*&---------------------------------------------------------------------*
*&      Form  F_COSTCENTER_CONVERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM F_COSTCENTER_CONVERT  USING    LV_COSTCNTR TYPE KOSTl
                           CHANGING CV_COSTCENTER TYPE KOSTL.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LV_COSTCNTR
    IMPORTING
      OUTPUT = CV_COSTCENTER.


ENDFORM.                    " F_COSTCENTER_CONVERT
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  CLEAR    gt_doc_detl[].
  CLEAR    gs_doc_detl.


  CLEAR    gs_lfa1.

  CLEAR    gv_awtyp.

  CLEAR    gv_check.
  CLEAR    gt_doc_hdr[].
  CLEAR    gt_doc_item[].
  CLEAR    gt_doc_actg[].
  CLEAR    gt_return[].



ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_prepare_document_detail_data
*&---------------------------------------------------------------------*
*       Prepare the document detail data
*----------------------------------------------------------------------*
FORM f_prepare_document_detail_data.

  DATA:    ls_ekko                     TYPE gty_ekko,
           ls_ekpo                     TYPE gty_ekpo,
           lt_ekkn                     TYPE gtt_ekkn.

  DATA:    lv_ebeln                    TYPE ebeln,
           lv_ebelp                    TYPE ebelp,
           lv_zekkn                    TYPE dzekkn.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_tabix_fr                 TYPE sytabix,
           lv_tabix_to                 TYPE sytabix,
           lv_seqn                     TYPE numc06,
           lv_actgseqn                 TYPE numc3,
           lv_fl_actg                  TYPE xflag.

  FIELD-SYMBOLS: <fs_doc_item>         TYPE zaps_iap_doc_item,
                 <fs_doc_actg>         TYPE zaps_iap_doc_actg,
                 <fs_ekkn>             TYPE gty_ekkn.

  SORT     gt_doc_item         ASCENDING BY hdrseqn itemseqn.
  SORT     gt_doc_actg         ASCENDING BY hdrseqn itemseqn actgseqn.

* Read the document header
  CLEAR                                     gs_doc_hdr.
  READ     TABLE gt_doc_hdr            INTO gs_doc_hdr INDEX 1.
  IF     ( sy-subrc NE 0 ).
*** ERROR - Document header not found
    RETURN.
  ENDIF.

* Determine the reference transaction
  CASE     gs_doc_hdr-doccatg.
    WHEN     gc_doccatg_po_inv.
      MOVE     gc_awtyp_rmrp             TO gv_awtyp.
    WHEN     gc_doccatg_po_cm.
      MOVE     gc_awtyp_rmrp             TO gv_awtyp.
    WHEN     gc_doccatg_npo_inv.
      MOVE     gc_awtyp_bkpf             TO gv_awtyp.
    WHEN     gc_doccatg_npo_cm.
      MOVE     gc_awtyp_bkpf             TO gv_awtyp.
    WHEN     OTHERS.
*** ERROR - Invalid document category
  ENDCASE.

*eject
* Validate the header data
  PERFORM  f_validate_header       CHANGING gs_doc_hdr.

  CLEAR                                     gs_doc_detl.
  MOVE     gs_doc_hdr-hdrseqn            TO gs_doc_detl-hdrseqn.
  MOVE     gv_awtyp                      TO gs_doc_detl-awtyp.
  MOVE     gs_doc_hdr-supplier           TO gs_doc_detl-lifnr.

  IF   ( ( gs_doc_hdr-supplier           IS NOT INITIAL ) AND
         ( gs_lfa1-lifnr                 IS NOT INITIAL )     ).
    MOVE   gs_lfa1-txjcd                 TO gs_doc_detl-txjcd_sf.
    MOVE   gs_lfa1-txjcd                 TO gs_doc_detl-txjcd_poa.
  ENDIF.

* Ensure there is a dummy item entry for accounting entries where the
* item sequence number is blank or '000'.  The following processing
* requires that all accounting entries have corresponding item entries
  CLEAR                                     gs_doc_actg.
  READ     TABLE gt_doc_actg           INTO gs_doc_actg INDEX 1.
  IF     ( sy-subrc EQ 0 ).

    IF   ( ( gs_doc_actg-itemseqn        IS INITIAL ) OR
           ( gs_doc_actg-itemseqn        EQ '000'   )    ).

      CLEAR                                 gs_doc_item.
      READ     TABLE gt_doc_item       INTO gs_doc_item INDEX 1.
      IF     ( sy-subrc NE 0 ).
        CLEAR                               gs_doc_item.
        MOVE     '999'                   TO gs_doc_item-itemseqn.
      ENDIF.

      IF   ( ( gs_doc_item-itemseqn      IS INITIAL ) OR
             ( gs_doc_item-itemseqn      EQ '000'   )    ).
      ELSE.
        CLEAR                               gs_doc_item.
        MOVE     gs_doc_hdr-hdrseqn      TO gs_doc_item-hdrseqn.
        MOVE     '000'                   TO gs_doc_item-itemseqn.
        INSERT   gs_doc_item           INTO gt_doc_item INDEX 1.
      ENDIF.

    ENDIF.

  ENDIF.

*eject
* Get the related data required for posting
  CLEAR    lv_seqn.
  CLEAR    lv_fl_actg.

  lv_tabix    = 1.
  lv_tabix_to = LINES( gt_doc_actg ).

* Loop at the item entries
  LOOP AT  gt_doc_item            ASSIGNING <fs_doc_item>.

    IF     ( <fs_doc_item>-itemseqn      IS INITIAL ).
      MOVE     '000'                     TO <fs_doc_item>-itemseqn.
    ENDIF.

* Select the PO data
    CLEAR    ls_ekko.
    CLEAR    ls_ekpo.
    CLEAR    lt_ekkn[].

    IF       ( gv_awtyp                  EQ gc_awtyp_rmrp ).

      IF   ( ( <fs_doc_item>-itemseqn    NE '000'       ) AND
             ( <fs_doc_item>-pohdr       IS NOT INITIAL )     ).

        CLEAR                               lv_ebeln.
        MOVE     <fs_doc_item>-pohdr     TO lv_ebeln.
        CLEAR                               lv_ebelp.
        MOVE     <fs_doc_item>-poitem    TO lv_ebelp.

        PERFORM  f_get_po_data       TABLES lt_ekkn
                                      USING lv_ebeln
                                            lv_ebelp
                                   CHANGING ls_ekko
                                            ls_ekpo.

      ENDIF.

    ENDIF.

* Validate the item data
    PERFORM  f_validate_item       CHANGING <fs_doc_item>.

    CLEAR                                   gs_doc_detl-itemseqn.
    MOVE     <fs_doc_item>-itemseqn      TO gs_doc_detl-itemseqn.

*eject
* Loop at the accounting entries
    CLEAR    lv_fl_actg.

    lv_tabix_fr = lv_tabix.

    LOOP AT  gt_doc_actg          ASSIGNING <fs_doc_actg>
                                       FROM lv_tabix_fr TO lv_tabix_to.
      lv_tabix = sy-tabix.

      IF     ( <fs_doc_actg>-itemseqn    IS INITIAL ).
        MOVE     '000'                   TO <fs_doc_actg>-itemseqn.
      ENDIF.
      IF     ( <fs_doc_actg>-actgseqn    IS INITIAL ).
        MOVE     '000'                   TO <fs_doc_actg>-actgseqn.
      ENDIF.

      IF     ( <fs_doc_actg>-itemseqn    LT <fs_doc_item>-itemseqn ).
        CONTINUE.
      ELSEIF ( <fs_doc_actg>-itemseqn    GT <fs_doc_item>-itemseqn ).
        EXIT.
      ENDIF.

      lv_fl_actg = abap_true.

* Validate the accounting data
      PERFORM  f_validate_accounting
                                   CHANGING <fs_doc_actg>.

      ADD      1                         TO lv_seqn.

      CLEAR                                 gs_doc_detl-seqn.
      MOVE     lv_seqn                   TO gs_doc_detl-seqn.
      CLEAR                                 gs_doc_detl-actgseqn.
      MOVE     <fs_doc_actg>-actgseqn    TO gs_doc_detl-actgseqn.

      CLEAR                                 gs_doc_detl-xunpl.
      CLEAR                                 gs_doc_detl-bukrs.
      CLEAR                                 gs_doc_detl-vbund.
      CLEAR                                 gs_doc_detl-prctr.
      CLEAR                                 gs_doc_detl-hkont.
      CLEAR                                 gs_doc_detl-kostl.
      CLEAR                                 gs_doc_detl-kosar.
      CLEAR                                 gs_doc_detl-aufnr.
      CLEAR                                 gs_doc_detl-wbselem.
      CLEAR                                 gs_doc_detl-posnr.
      CLEAR                                 gs_doc_detl-posid.
      CLEAR                                 gs_doc_detl-nplnr.
      CLEAR                                 gs_doc_detl-vornr.
      CLEAR                                 gs_doc_detl-paobjnr.
      CLEAR                                 gs_doc_detl-werks.
      CLEAR                                 gs_doc_detl-stort.
      CLEAR                                 gs_doc_detl-profl.
      CLEAR                                 gs_doc_detl-zzloc.
      CLEAR                                 gs_doc_detl-txjcd_poo.
      CLEAR                                 gs_doc_detl-zztxjcd_st.

*eject
* Check the PO for an unplanned account assignment
      IF       ( gv_awtyp                EQ gc_awtyp_rmrp ).

        IF   ( ( <fs_doc_item>-itemseqn  NE '000'       ) AND
               ( <fs_doc_actg>-itemseqn  NE '000'       ) AND
               ( <fs_doc_item>-pohdr     IS NOT INITIAL )     ).

          CLEAR                             lv_ebeln.
          MOVE   <fs_doc_item>-pohdr     TO lv_ebeln.
          CLEAR                             lv_ebelp.
          MOVE   <fs_doc_item>-poitem    TO lv_ebelp.
          CLEAR                             lv_zekkn.
          MOVE   <fs_doc_actg>-acctasgn  TO lv_zekkn.

          READ   TABLE lt_ekkn    ASSIGNING <fs_ekkn>
                           WITH KEY ebeln = lv_ebeln
                                    ebelp = lv_ebelp
                                    zekkn = lv_zekkn.
          IF     ( sy-subrc NE 0 ).
            MOVE     abap_true           TO gs_doc_detl-xunpl.
          ENDIF.

        ENDIF.

      ENDIF.

      IF     ( <fs_doc_actg>-compcode    IS NOT INITIAL ).
        CLEAR                               gs_doc_detl-bukrs.
        MOVE   <fs_doc_actg>-compcode    TO gs_doc_detl-bukrs.
      ENDIF.

      IF   ( ( <fs_doc_actg>-glacct      IS NOT INITIAL ) AND
             ( gs_ska1-saknr             IS NOT INITIAL )     ).
        CLEAR                               gs_doc_detl-hkont.
        MOVE   gs_ska1-saknr             TO gs_doc_detl-hkont.
      ENDIF.

*eject
      IF   ( ( <fs_doc_actg>-costcntr    IS NOT INITIAL ) AND
             ( gs_csks-kostl             IS NOT INITIAL )     ).
        CLEAR                               gs_doc_detl-prctr.
        MOVE   gs_csks-prctr             TO gs_doc_detl-prctr.
        CLEAR                               gs_doc_detl-kostl.
        MOVE   gs_csks-kostl             TO gs_doc_detl-kostl.
        CLEAR                               gs_doc_detl-kosar.
        MOVE   gs_csks-kosar             TO gs_doc_detl-kosar.
        CLEAR                               gs_doc_detl-werks.
        MOVE   gs_csks-werks             TO gs_doc_detl-werks.
        CLEAR                               gs_doc_detl-txjcd_poo.
        MOVE   gs_csks-txjcd             TO gs_doc_detl-txjcd_poo.
      ENDIF.

      IF   ( ( <fs_doc_actg>-intlordr    IS NOT INITIAL ) AND
             ( gs_aufk-aufnr             IS NOT INITIAL )     ).
        CLEAR                               gs_doc_detl-prctr.
        MOVE   gs_aufk-prctr             TO gs_doc_detl-prctr.
        CLEAR                               gs_doc_detl-aufnr.
        MOVE   gs_aufk-aufnr             TO gs_doc_detl-aufnr.
        CLEAR                               gs_doc_detl-werks.
        MOVE   gs_aufk-werks             TO gs_doc_detl-werks.
        CLEAR                               gs_doc_detl-stort.
        MOVE   gs_aufk-stort             TO gs_doc_detl-stort.
        CLEAR                               gs_doc_detl-txjcd_poo.
        MOVE   gs_aufk-txjcd             TO gs_doc_detl-txjcd_poo.
      ENDIF.

      IF   ( ( <fs_doc_actg>-wbselem     IS NOT INITIAL ) AND
             ( gs_prps-posnr             IS NOT INITIAL )     ).
        CLEAR                               gs_doc_detl-prctr.
        MOVE   gs_prps-prctr             TO gs_doc_detl-prctr.
        CLEAR                               gs_doc_detl-wbselem.
        MOVE   gs_prps-wbselem           TO gs_doc_detl-wbselem.
        CLEAR                               gs_doc_detl-posnr.
        MOVE   gs_prps-posnr             TO gs_doc_detl-posnr.
        CLEAR                               gs_doc_detl-posid.
        MOVE   gs_prps-posid             TO gs_doc_detl-posid.
        CLEAR                               gs_doc_detl-werks.
        MOVE   gs_prps-werks             TO gs_doc_detl-werks.
        CLEAR                               gs_doc_detl-stort.
        MOVE   gs_prps-stort             TO gs_doc_detl-stort.
        CLEAR                               gs_doc_detl-profl.
        MOVE   gs_prps-profl             TO gs_doc_detl-profl.
        CLEAR                               gs_doc_detl-txjcd_poo.
        MOVE   gs_prps-txjcd             TO gs_doc_detl-txjcd_poo.
      ENDIF.

*eject
      IF   ( ( <fs_doc_actg>-network     IS NOT INITIAL ) AND
             ( <fs_doc_actg>-activity    IS NOT INITIAL ) AND
             ( gs_afko-aufnr             IS NOT INITIAL ) AND
             ( gs_afvc-vornr             IS NOT INITIAL )     ).
        CLEAR                               gs_doc_detl-prctr.
        MOVE   gs_afvc-prctr             TO gs_doc_detl-prctr.
        CLEAR                               gs_doc_detl-nplnr.
        MOVE   gs_afko-aufnr             TO gs_doc_detl-nplnr.
        CLEAR                               gs_doc_detl-vornr.
        MOVE   gs_afvc-vornr             TO gs_doc_detl-vornr.
        CLEAR                               gs_doc_detl-werks.
        MOVE   gs_afvc-werks             TO gs_doc_detl-werks.
        CLEAR                               gs_doc_detl-txjcd_poo.
        MOVE   gs_afvc-txjcd             TO gs_doc_detl-txjcd_poo.
      ENDIF.

      IF     ( <fs_doc_actg>-prftsegmno  IS NOT INITIAL ).
* UG only
      ENDIF.

      IF   ( ( <fs_doc_actg>-lctncode    IS NOT INITIAL ) AND
             ( gs_location-location      IS NOT INITIAL )     ).
        CLEAR                               gs_doc_detl-zzloc.
        MOVE   gs_location-location      TO gs_doc_detl-zzloc.
      ENDIF.

* Set the destination tax jurisdiction
      PERFORM  f_set_destination_tax_juris
                                      USING ls_ekpo
                                   CHANGING gs_doc_detl.

      APPEND   gs_doc_detl               TO gt_doc_detl.

    ENDLOOP.

*eject
* If the doc. does not contain accounting data, then gen. the entries
    IF       ( gv_awtyp                  EQ gc_awtyp_rmrp ).

      IF   ( ( lv_fl_actg                IS INITIAL       ) AND
             ( <fs_doc_item>-itemseqn    NE '000'         )     ).

        CLEAR    lv_actgseqn.

        LOOP AT  lt_ekkn          ASSIGNING <fs_ekkn>.

* Generate the account coding from the purchase order
          PERFORM  f_generate_coding_from_po
                                      USING <fs_ekkn>.

          ADD      1                     TO lv_seqn.
          ADD      1                     TO lv_actgseqn.

          CLEAR                             gs_doc_detl-seqn.
          MOVE     lv_seqn               TO gs_doc_detl-seqn.
          CLEAR                             gs_doc_detl-actgseqn.
          MOVE     lv_actgseqn           TO gs_doc_detl-actgseqn.

          CLEAR                             gs_doc_detl-xunpl.
          CLEAR                             gs_doc_detl-bukrs.
          CLEAR                             gs_doc_detl-vbund.
          CLEAR                             gs_doc_detl-prctr.
          CLEAR                             gs_doc_detl-hkont.
          CLEAR                             gs_doc_detl-kostl.
          CLEAR                             gs_doc_detl-kosar.
          CLEAR                             gs_doc_detl-aufnr.
          CLEAR                             gs_doc_detl-wbselem.
          CLEAR                             gs_doc_detl-posnr.
          CLEAR                             gs_doc_detl-posid.
          CLEAR                             gs_doc_detl-nplnr.
          CLEAR                             gs_doc_detl-vornr.
          CLEAR                             gs_doc_detl-paobjnr.
          CLEAR                             gs_doc_detl-werks.
          CLEAR                             gs_doc_detl-stort.
          CLEAR                             gs_doc_detl-profl.
          CLEAR                             gs_doc_detl-zzloc.
          CLEAR                             gs_doc_detl-txjcd_poo.
          CLEAR                             gs_doc_detl-zztxjcd_st.

          IF     ( ls_ekpo-bukrs         IS NOT INITIAL ).
            CLEAR                           gs_doc_detl-bukrs.
            MOVE   ls_ekpo-bukrs         TO gs_doc_detl-bukrs.
          ENDIF.

          IF   ( ( <fs_ekkn>-sakto       IS NOT INITIAL ) AND
                 ( gs_ska1-saknr         IS NOT INITIAL )     ).
            CLEAR                           gs_doc_detl-hkont.
            MOVE   gs_ska1-saknr         TO gs_doc_detl-hkont.
          ENDIF.

*eject
          IF   ( ( <fs_ekkn>-kostl       IS NOT INITIAL ) AND
                 ( gs_csks-kostl         IS NOT INITIAL )     ).
            CLEAR                           gs_doc_detl-prctr.
            MOVE   gs_csks-prctr         TO gs_doc_detl-prctr.
            CLEAR                           gs_doc_detl-kostl.
            MOVE   gs_csks-kostl         TO gs_doc_detl-kostl.
            CLEAR                           gs_doc_detl-kosar.
            MOVE   gs_csks-kosar         TO gs_doc_detl-kosar.
            CLEAR                           gs_doc_detl-werks.
            MOVE   gs_csks-werks         TO gs_doc_detl-werks.
            CLEAR                           gs_doc_detl-txjcd_poo.
            MOVE   gs_csks-txjcd         TO gs_doc_detl-txjcd_poo.
          ENDIF.

          IF   ( ( <fs_ekkn>-aufnr       IS NOT INITIAL ) AND
                 ( gs_aufk-aufnr         IS NOT INITIAL )     ).
            CLEAR                           gs_doc_detl-prctr.
            MOVE   gs_aufk-prctr         TO gs_doc_detl-prctr.
            CLEAR                           gs_doc_detl-aufnr.
            MOVE   gs_aufk-aufnr         TO gs_doc_detl-aufnr.
            CLEAR                           gs_doc_detl-werks.
            MOVE   gs_aufk-werks         TO gs_doc_detl-werks.
            CLEAR                           gs_doc_detl-stort.
            MOVE   gs_aufk-stort         TO gs_doc_detl-stort.
            CLEAR                           gs_doc_detl-txjcd_poo.
            MOVE   gs_aufk-txjcd         TO gs_doc_detl-txjcd_poo.
          ENDIF.

          IF   ( ( <fs_ekkn>-ps_psp_pnr  IS NOT INITIAL ) AND
                 ( gs_prps-posnr         IS NOT INITIAL )     ).
            CLEAR                           gs_doc_detl-prctr.
            MOVE   gs_prps-prctr         TO gs_doc_detl-prctr.
            CLEAR                           gs_doc_detl-wbselem.
            MOVE   gs_prps-wbselem       TO gs_doc_detl-wbselem.
            CLEAR                           gs_doc_detl-posnr.
            MOVE   gs_prps-posnr         TO gs_doc_detl-posnr.
            CLEAR                           gs_doc_detl-posid.
            MOVE   gs_prps-posid         TO gs_doc_detl-posid.
            CLEAR                           gs_doc_detl-werks.
            MOVE   gs_prps-werks         TO gs_doc_detl-werks.
            CLEAR                           gs_doc_detl-stort.
            MOVE   gs_prps-stort         TO gs_doc_detl-stort.
            CLEAR                           gs_doc_detl-profl.
            MOVE   gs_prps-profl         TO gs_doc_detl-profl.
            CLEAR                           gs_doc_detl-txjcd_poo.
            MOVE   gs_prps-txjcd         TO gs_doc_detl-txjcd_poo.
          ENDIF.

*eject
          IF   ( ( <fs_ekkn>-aufpl       IS NOT INITIAL ) AND
                 ( <fs_ekkn>-aplzl       IS NOT INITIAL ) AND
                 ( gs_afko-aufnr         IS NOT INITIAL ) AND
                 ( gs_afvc-aufpl         IS NOT INITIAL )     ).
            CLEAR                           gs_doc_detl-prctr.
            MOVE   gs_afvc-prctr         TO gs_doc_detl-prctr.
            CLEAR                           gs_doc_detl-nplnr.
            MOVE   gs_afko-aufnr         TO gs_doc_detl-nplnr.
            CLEAR                           gs_doc_detl-vornr.
            MOVE   gs_afvc-vornr         TO gs_doc_detl-vornr.
            CLEAR                           gs_doc_detl-werks.
            MOVE   gs_afvc-werks         TO gs_doc_detl-werks.
            CLEAR                           gs_doc_detl-txjcd_poo.
            MOVE   gs_afvc-txjcd         TO gs_doc_detl-txjcd_poo.
          ENDIF.

          IF     ( <fs_ekkn>-paobjnr     IS NOT INITIAL ).
* UG only
          ENDIF.

          IF     ( <fs_ekkn>-zzloc       IS NOT INITIAL ).
            CLEAR                           gs_doc_detl-zzloc.
            MOVE   <fs_ekkn>-zzloc       TO gs_doc_detl-zzloc.
          ENDIF.

* Set the destination tax jurisdiction
          PERFORM  f_set_destination_tax_juris
                                      USING ls_ekpo
                                   CHANGING gs_doc_detl.

          APPEND   gs_doc_detl           TO gt_doc_detl.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_prepare_document_detail_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_po_data
*&---------------------------------------------------------------------*
*       Get the PO data
*----------------------------------------------------------------------*
FORM f_get_po_data
  TABLES   ct_ekkn                     TYPE gtt_ekkn
  USING    iv_ebeln                    TYPE ebeln
           iv_ebelp                    TYPE ebelp
  CHANGING cs_ekko                     TYPE gty_ekko
           cs_ekpo                     TYPE gty_ekpo.

  DATA:    ls_ekko                     TYPE gty_ekko,
           lt_ekpo                     TYPE gtt_ekpo,
           lt_ekkn                     TYPE gtt_ekkn.

  FIELD-SYMBOLS: <fs_ekkn>             TYPE gty_ekkn.

  CLEAR    cs_ekko.
  CLEAR    cs_ekpo.
  CLEAR    ct_ekkn[].

  IF     ( ( gs_ekko-ebeln               EQ iv_ebeln ) AND
           ( gs_ekpo-ebeln               EQ iv_ebeln ) AND
           ( gs_ekpo-ebelp               EQ iv_ebelp )     ).
    MOVE     gs_ekko                     TO cs_ekko.
    MOVE     gs_ekpo                     TO cs_ekpo.
    LOOP AT  gt_ekkn              ASSIGNING <fs_ekkn>
                                      WHERE ebeln = iv_ebeln
                                        AND ebelp = iv_ebelp.
      APPEND <fs_ekkn>                   TO ct_ekkn.
    ENDLOOP.
    RETURN.
  ENDIF.

*eject
  CLEAR    ls_ekko.
  CLEAR    lt_ekpo[].
  CLEAR    lt_ekkn[].

  CLEAR                                     gs_ekko.
  READ     TABLE gt_ekko               INTO gs_ekko
                                   WITH KEY ebeln = iv_ebeln.
  IF     ( sy-subrc NE 0 ).

    SELECT   SINGLE ebeln  bukrs  bstyp  bsart
                    loekz  statu  aedat  lifnr
      INTO   ls_ekko
      FROM   ekko
     WHERE   ebeln = iv_ebeln.
    IF     ( sy-subrc EQ 0 ).
      CLEAR                                 gs_ekko.
      MOVE     ls_ekko                   TO gs_ekko.
      APPEND   ls_ekko                   TO gt_ekko.
      SORT     gt_ekko         ASCENDING BY ebeln.
    ELSE.
*** ERROR - PO not found
      RETURN.
    ENDIF.

    SELECT   ebeln  ebelp  loekz  aedat
             matnr  bukrs  werks  lgort
             matkl  meins  knttp
      INTO   TABLE lt_ekpo
      FROM   ekpo
     WHERE   ebeln = iv_ebeln.
    IF     ( sy-subrc EQ 0 ).
      APPEND   LINES OF lt_ekpo          TO gt_ekpo.
      SORT     gt_ekpo         ASCENDING BY ebeln ebelp.
    ENDIF.

    SELECT   ebeln  ebelp  zekkn  loekz  aedat
             vproz  sakto  kostl  aufnr  kokrs
             paobjnr       prctr  ps_psp_pnr
             nplnr  aufpl  aplzl  zzloc  zzref
             mwskz  txjcd
      INTO   TABLE lt_ekkn
      FROM   ekkn
     WHERE   ebeln = iv_ebeln.
    IF     ( sy-subrc EQ 0 ).
      APPEND   LINES OF lt_ekkn          TO gt_ekkn.
      SORT     gt_ekkn         ASCENDING BY ebeln ebelp zekkn.
    ENDIF.

  ENDIF.

*eject
  MOVE     gs_ekko                       TO cs_ekko.

  CLEAR                                     gs_ekpo.
  READ     TABLE gt_ekpo               INTO gs_ekpo
                                   WITH KEY ebeln = iv_ebeln
                                            ebelp = iv_ebelp.
  IF     ( sy-subrc EQ 0 ).
    MOVE     gs_ekpo                     TO cs_ekpo.
  ELSE.
*** ERROR - PO item not found
  ENDIF.

  LOOP AT  gt_ekkn                ASSIGNING <fs_ekkn>
                                      WHERE ebeln = iv_ebeln
                                        AND ebelp = iv_ebelp.
    APPEND   <fs_ekkn>                   TO ct_ekkn.
  ENDLOOP.

ENDFORM.                    " f_get_po_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_header
*&---------------------------------------------------------------------*
*       Validate the header data
*----------------------------------------------------------------------*
FORM f_validate_header
  CHANGING cs_doc_hdr                  TYPE gty_doc_hdr.

  CLEAR    gs_lfa1.

* Validate the vendor data

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = cs_doc_hdr-supplier
    IMPORTING
      OUTPUT = cs_doc_hdr-supplier.

  SELECT   SINGLE lifnr  txjcd
    INTO   gs_lfa1
    FROM   lfa1
   WHERE   lifnr = cs_doc_hdr-supplier.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gs_lfa1.
*** ERROR - Invalid supplier id
  ENDIF.

ENDFORM.                    " f_validate_header
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_item
*&---------------------------------------------------------------------*
*       Validate the item data
*----------------------------------------------------------------------*
FORM f_validate_item
  CHANGING cs_doc_item                 TYPE gty_doc_item.

ENDFORM.                    " f_validate_item
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_accounting
*&---------------------------------------------------------------------*
*       Validate the accounting data
*----------------------------------------------------------------------*
FORM f_validate_accounting
  CHANGING cs_doc_actg                 TYPE gty_doc_actg.

  CLEAR    gs_ska1.
  CLEAR    gs_csks.
  CLEAR    gs_aufk.
  CLEAR    gs_prps.
  CLEAR    gs_afko.
  CLEAR    gs_afvc.
  CLEAR    gs_location.

* Validate the G/L account
  IF     ( cs_doc_actg-glacct            IS NOT INITIAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = cs_doc_actg-glacct
      IMPORTING
        OUTPUT = cs_doc_actg-glacct.

    SELECT   SINGLE ktopl  saknr
      INTO   gs_ska1
      FROM   ska1
     WHERE   saknr = cs_doc_actg-glacct.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_ska1.
***   ERROR - Invalid G/L account
    ENDIF.

  ENDIF.

*eject
* Validate the cost center
  IF     ( cs_doc_actg-costcntr          IS NOT INITIAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = cs_doc_actg-costcntr
      IMPORTING
        OUTPUT = cs_doc_actg-costcntr.

    SELECT   SINGLE kokrs  kostl  kosar
                    txjcd  prctr  werks  objnr
      INTO   gs_csks
      FROM   csks
     WHERE   kostl = cs_doc_actg-costcntr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_csks.
***   ERROR - Invalid cost center
    ENDIF.

  ENDIF.

* Validate the internal order
  IF     ( cs_doc_actg-intlordr          IS NOT INITIAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = cs_doc_actg-intlordr
      IMPORTING
        OUTPUT = cs_doc_actg-intlordr.

    SELECT   SINGLE aufnr  werks  stort
                    objnr  prctr  txjcd
      INTO   gs_aufk
      FROM   aufk
     WHERE   aufnr = cs_doc_actg-intlordr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_aufk.
***   ERROR - Invalid internal order
    ENDIF.

  ENDIF.

* Validate the WBS element
  IF     ( cs_doc_actg-wbselem           IS NOT INITIAL ).

    PERFORM  f_validate_wbs_element   USING cs_doc_actg-wbselem.

  ENDIF.

*eject
* Validate the network
  IF   ( ( cs_doc_actg-network           IS NOT INITIAL ) OR
         ( cs_doc_actg-activity          IS NOT INITIAL )    ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = cs_doc_actg-network
      IMPORTING
        OUTPUT = cs_doc_actg-network.

    SELECT   SINGLE aufnr  aufpl
      INTO   gs_afko
      FROM   afko
     WHERE   aufnr = cs_doc_actg-network.
    IF     ( sy-subrc EQ 0 ).

* Validate the activity

      CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
        EXPORTING
          INPUT  = cs_doc_actg-activity
        IMPORTING
          OUTPUT = cs_doc_actg-activity.

      SELECT   SINGLE aufpl  aplzl  vornr
                      werks  objnr  txjcd  prctr
        INTO   gs_afvc
        FROM   afvc
       WHERE   aufpl = gs_afko-aufpl
         AND   vornr = cs_doc_actg-activity.
      IF     ( sy-subrc NE 0 ).
        CLEAR  gs_afvc.
***     ERROR - Invalid activity
      ENDIF.

    ELSE.
      CLEAR  gs_afko.
***   ERROR - Invalid network
    ENDIF.

  ENDIF.

*eject
* Validate the profitability segment
  IF     ( cs_doc_actg-lctncode          IS NOT INITIAL ).

* UG system only - table CE41100_ACCT

  ENDIF.

* Validate the location code
  IF     ( cs_doc_actg-lctncode          IS NOT INITIAL ).

    SELECT   SINGLE *
      INTO   gs_location
      FROM   zfit_location
     WHERE   location = cs_doc_actg-lctncode.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_location.
***   ERROR - Invalid location code
    ENDIF.

  ENDIF.

ENDFORM.                    " f_validate_accounting
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_wbs_element
*&---------------------------------------------------------------------*
*       Validate the WBS element
*----------------------------------------------------------------------*
FORM f_validate_wbs_element
  USING    iv_wbselem                  TYPE char24.

  DATA:    lv_posnr                    TYPE ps_posnr,
           lv_posid                    TYPE ps_posid,
           lv_objnr                    TYPE j_objnr,
           lv_pspnr                    TYPE ps_intnr,
           lv_profl                    TYPE profidproj,
           ls_bapiret2                 TYPE bapiret2.

  CLEAR    lv_posnr.
  CLEAR    lv_posid.
  CLEAR    lv_objnr.
  CLEAR    lv_pspnr.
  CLEAR    lv_profl.
  CLEAR    ls_bapiret2.

  CALL FUNCTION 'ZPS_WBS_VALIDATE'
    EXPORTING
      INPUT              = iv_wbselem
    IMPORTING
      OUTPUT             = lv_posid
      PSPNR              = lv_posnr
      OBJNR              = lv_objnr
      BAPIRET2           = ls_bapiret2
    EXCEPTIONS
      WBS_TYPE_INVALID   = 1
      WBS_FORMAT_INVALID = 2
      WBS_DOES_NOT_EXIST = 3
      OTHERS             = 4.

  IF     ( sy-subrc NE 0 ).
*** ERROR - Invalid WBS element
    RETURN.
  ENDIF.

*eject
  SELECT   SINGLE pspnr  posid  objnr  psphi
                  astnr  prctr  werks  txjcd  stort
    INTO   gs_prps
    FROM   prps
   WHERE   pspnr = lv_posnr.
  IF     ( sy-subrc EQ 0 ).

    CLEAR                                   gs_prps-wbselem.
    MOVE     iv_wbselem                  TO gs_prps-wbselem.

    SELECT   SINGLE pspnr  profl
      INTO  (lv_pspnr, lv_profl)
      FROM   proj
     WHERE   pspnr = gs_prps-psphi.
    IF     ( sy-subrc EQ 0 ).

      CLEAR                                 gs_prps-profl.
      MOVE     lv_profl                  TO gs_prps-profl.

    ENDIF.

  ELSE.
    CLEAR  gs_prps.
  ENDIF.

ENDFORM.                    " f_validate_wbs_element
*eject
*&---------------------------------------------------------------------*
*&      Form  f_generate_coding_from_po
*&---------------------------------------------------------------------*
*       Generate the account coding from the purchase order
*----------------------------------------------------------------------*
FORM f_generate_coding_from_po
  USING    is_ekkn                     TYPE gty_ekkn.

  DATA:    lv_wbselem                  TYPE char24.

  DATA:    lv_pspnr                    TYPE ps_intnr,
           lv_profl                    TYPE profidproj.

  CLEAR    gs_ska1.
  CLEAR    gs_csks.
  CLEAR    gs_aufk.
  CLEAR    gs_prps.
  CLEAR    gs_afko.
  CLEAR    gs_afvc.
  CLEAR    gs_location.

* Select the G/L account
  IF     ( is_ekkn-sakto                 IS NOT INITIAL ).

    SELECT   SINGLE ktopl  saknr
      INTO   gs_ska1
      FROM   ska1
     WHERE   saknr = is_ekkn-sakto.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_ska1.
    ENDIF.

  ENDIF.

* Select the cost center
  IF     ( is_ekkn-kostl                 IS NOT INITIAL ).

    SELECT   SINGLE kokrs  kostl  kosar
                    txjcd  prctr  werks  objnr
      INTO   gs_csks
      FROM   csks
     WHERE   kostl = is_ekkn-kostl.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_csks.
    ENDIF.

  ENDIF.

*eject
* Select the internal order
  IF     ( is_ekkn-aufnr                 IS NOT INITIAL ).

    SELECT   SINGLE aufnr  werks  stort
                    objnr  prctr  txjcd
      INTO   gs_aufk
      FROM   aufk
     WHERE   aufnr = is_ekkn-aufnr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_aufk.
    ENDIF.

  ENDIF.

* Select the WBS element
  IF     ( is_ekkn-ps_psp_pnr            IS NOT INITIAL ).

    SELECT   SINGLE pspnr  posid  objnr  psphi
                    astnr  prctr  werks  txjcd  stort
      INTO   gs_prps
      FROM   prps
     WHERE   pspnr = is_ekkn-ps_psp_pnr.
    IF     ( sy-subrc EQ 0 ).

      CLEAR    lv_wbselem.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          INPUT  = is_ekkn-ps_psp_pnr
        IMPORTING
          OUTPUT = lv_wbselem.

      CLEAR                                 gs_prps-wbselem.
      MOVE     lv_wbselem                TO gs_prps-wbselem.

      SELECT   SINGLE pspnr  profl
        INTO  (lv_pspnr, lv_profl)
        FROM   proj
       WHERE   pspnr = gs_prps-psphi.
      IF     ( sy-subrc EQ 0 ).

        CLEAR                               gs_prps-profl.
        MOVE     lv_profl                TO gs_prps-profl.

      ENDIF.

    ELSE.
      CLEAR  gs_prps.
    ENDIF.

  ENDIF.

*eject
* Select the network
  IF   ( ( gs_ekkn-aufpl                 IS NOT INITIAL ) OR
         ( gs_ekkn-aplzl                 IS NOT INITIAL )    ).

    SELECT   SINGLE aufnr  aufpl
      INTO   gs_afko
      FROM   afko
     WHERE   aufpl = gs_ekkn-aufpl.
    IF     ( sy-subrc EQ 0 ).

* Select the activity
      SELECT   SINGLE aufpl  aplzl  vornr
                      werks  objnr  txjcd  prctr
        INTO   gs_afvc
        FROM   afvc
       WHERE   aufpl = gs_ekkn-aufpl
         AND   aplzl = gs_ekkn-aplzl.
      IF     ( sy-subrc NE 0 ).
        CLEAR  gs_afvc.
      ENDIF.

    ELSE.
      CLEAR  gs_afko.
    ENDIF.

  ENDIF.

* Validate the profitability segment
  IF     ( gs_ekkn-paobjnr               IS NOT INITIAL ).

* UG system only - table CE41100_ACCT

  ENDIF.

* Validate the location code
  IF     ( gs_ekkn-zzloc                 IS NOT INITIAL ).

    SELECT   SINGLE *
      INTO   gs_location
      FROM   zfit_location
     WHERE   location = gs_ekkn-zzloc.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_location.
    ENDIF.

  ENDIF.

ENDFORM.                    " f_generate_coding_from_po
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_destination_tax_juris
*&---------------------------------------------------------------------*
*       Set the destination tax jurisdiction
*----------------------------------------------------------------------*
FORM f_set_destination_tax_juris
  USING    is_ekpo                     TYPE gty_ekpo
  CHANGING cs_doc_detl                 TYPE gty_doc_detl.

  DATA:    ls_location_data            TYPE com_jur,
           ls_location_err             TYPE com_err,
           ls_location_results         TYPE gty_location_results,
           lt_location_results         TYPE gtt_location_results.

  DATA:    lv_werks                    TYPE ewerk.

* Set the tax jurisdiction for a non-PO document
  IF     ( cs_doc_detl-awtyp             EQ gc_awtyp_bkpf ).

    IF   ( ( cs_doc_detl-zzloc           IS NOT INITIAL ) AND
           ( gs_location-location        IS NOT INITIAL )     ).

      CLEAR                                 ls_location_data.
      MOVE     gs_location-country       TO ls_location_data-country.
      MOVE     gs_location-state         TO ls_location_data-state.
      MOVE     gs_location-county        TO ls_location_data-county.
      MOVE     gs_location-city          TO ls_location_data-city.
      MOVE     gs_location-zipcode       TO ls_location_data-zipcode.

      CLEAR    ls_location_err.
      CLEAR    ls_location_results.
      CLEAR    lt_location_results[].

      CALL FUNCTION 'RFC_DETERMINE_JURISDICTION' DESTINATION 'VERTEX'
        EXPORTING
          LOCATION_DATA    = ls_location_data "#EC ENHOK Message Code STE 1308
        IMPORTING
          LOCATION_ERR     = ls_location_err
        TABLES
          LOCATION_RESULTS = lt_location_results.

*eject
      IF   ( ( ls_location_err-retcode   EQ '0'     ) AND
             ( ls_location_err-errcode   EQ 0000    )     ).

        READ     TABLE lt_location_results  INTO gs_location_results
                                           INDEX 1.
        IF     ( sy-subrc EQ 0 ).
          MOVE   gs_location_results-txjcd TO cs_doc_detl-zztxjcd_st.
        ELSE.
          CLEAR  gs_location_results.
        ENDIF.

      ELSE.

*** ERROR - Invalid location results (VERTEX)

      ENDIF.

    ENDIF.

* Set the tax jurisdiction for a PO document
  ELSEIF ( cs_doc_detl-awtyp             EQ gc_awtyp_rmrp ).

    CLEAR    lv_werks.

    IF       ( cs_doc_detl-werks         IS INITIAL ).
      MOVE     is_ekpo-werks             TO lv_werks.
    ELSE.
      MOVE     cs_doc_detl-werks         TO lv_werks.
    ENDIF.

    IF       ( lv_werks                  IS NOT INITIAL ).

      CLEAR    gs_t001w.
      SELECT   SINGLE werks  txjcd
        INTO   gs_t001w
        FROM   t001w
       WHERE   werks = lv_werks.
      IF     ( sy-subrc EQ 0 ).
        MOVE   gs_t001w-txjcd            TO cs_doc_detl-zztxjcd_st.
      ENDIF.

    ENDIF.

*eject
    IF     ( ( is_ekpo-ebeln             IS NOT INITIAL ) AND
             ( is_ekpo-ebelp             IS NOT INITIAL )     ).

      CLEAR    lv_werks.

      IF     ( is_ekpo-knttp             EQ 'P' ).

        MOVE   cs_doc_detl-werks         TO lv_werks.

      ELSE.

        CLEAR    gs_ec_tax.
        SELECT   SINGLE *
          INTO   gs_ec_tax
          FROM   zmmt_ec_tax
         WHERE   zvirtualplant = is_ekpo-werks
           AND   lgort         = is_ekpo-lgort.
        IF     ( sy-subrc EQ 0 ).

          MOVE   gs_ec_tax-zrealplant    TO lv_werks.

        ENDIF.

      ENDIF.

      IF       ( lv_werks                IS NOT INITIAL ).

        CLEAR    gs_t001w.
        SELECT   SINGLE werks  txjcd
          INTO   gs_t001w
          FROM   t001w
         WHERE   werks = lv_werks.
        IF     ( sy-subrc EQ 0 ).
          MOVE   gs_t001w-txjcd          TO cs_doc_detl-txjcd_poo.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  IF       ( cs_doc_detl-zztxjcd_st      IS INITIAL ).
    MOVE     cs_doc_detl-txjcd_poo       TO cs_doc_detl-zztxjcd_st.
  ENDIF.

ENDFORM.                    " f_set_destination_tax_juris
*&---------------------------------------------------------------------*
*&      Form  F_VENDOR_CONVERT
*&---------------------------------------------------------------------*
*  To get Leading Zeroes
*----------------------------------------------------------------------*

FORM F_VENDOR_CONVERT  USING    LV_SUPPLIER type char10
                       CHANGING CV_VENDOR_NO type kunnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LV_SUPPLIER
    IMPORTING
      OUTPUT = CV_VENDOR_NO.


ENDFORM.                    " F_VENDOR_CONVERT
