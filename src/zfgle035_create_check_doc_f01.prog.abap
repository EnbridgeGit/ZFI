*&---------------------------------------------------------------------*
*&  Include           ZFGLE035_CREATE_CHECK_DOC_F01
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFGLE035_CREATE_CHECK_DOC                         *
*  Include:          ZFGLE035_CREATE_CHECK_DOC_F01                     *
*  Author:           John Hartung                                      *
*  Date:             December 11, 2013                                 *
*  Application Area: FICO GL                                           *
*                                                                      *
*  Description:      GL Create Check Document from G/L Posting         *
*                                                                      *
*                    FI G/L documents are queried and found items      *
*                    are used to create Check Documents.  These        *
*                    Check documents are required in order for         *
*                    FEBAN processing to automatically clear the       *
*                    postings once the actual check is cashed.         *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By           Transport  Description                         *
* -------- ------------ ---------- ----------------------------------- *
* 12/11/13 JRHARTUNG    DECK911473 SDP Tkt 56835 - Initial Program     *
*                                  Development                         *
* 08/14/19 JRHARTUNG    DECK920075 CHG0156718 - ROW Checks - process   *
*                                  changes - retrieve Bank Key and     *
*                                  Bank Account from table SKB1        *
* 10/07/19 JRHARTUNG    DECK920217 CHG0162277 - ROW Checks - correct   *
*                       DECK920219 code to adjust the length of the    *
*                                  check number                        *
* 10/31/19 JRHARTUNG    D30K930244 CHG0160254 - Do not adjust check #  *
* 06/05/20 AHMADT       D30K930584 CHG0180658 - ROW checks ACK failure *
*                                  due to missing address              *
* 02/09/21 AHMADT       D30K930866 CHG0206262 - Unable to create checks*
*                       D30K930876 in UGL for Right of Way             *
*----------------------------------------------------------------------*
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  f_lvc_variant_default_get
*&---------------------------------------------------------------------*
*       Get the default LVC display variant
*----------------------------------------------------------------------*
FORM f_lvc_variant_default_get.

  CLEAR                                gs_variant.
  CLEAR                                gs_variant_p.
  MOVE     gv_repid                 TO gs_variant-report.
  MOVE     gv_repid                 TO gs_variant_p-report.

  IF     ( p_dspvar IS NOT INITIAL ).
    RETURN.
  ENDIF.

* Get the default LVC display variant

  CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE        = 'A'
    CHANGING
      CS_VARIANT    = gs_variant_p
    EXCEPTIONS
      WRONG_INPUT   = 1
      NOT_FOUND     = 2
      PROGRAM_ERROR = 3
      OTHERS        = 4.

  IF ( sy-subrc EQ 0 ).
    CLEAR                              p_dspvar.
    MOVE     gs_variant_p-variant   TO p_dspvar.
  ELSE.
*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_lvc_variant_default_get
*eject
*&---------------------------------------------------------------------*
*&      Form  f_lvc_variant_f4
*&---------------------------------------------------------------------*
*       Value request "drop-down" for the LVC display variant
*----------------------------------------------------------------------*
FORM f_lvc_variant_f4.

* Display list of LVC variants

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = gs_variant
      I_SAVE        = 'A'
    IMPORTING
      E_EXIT        = gv_flag_exit
      ES_VARIANT    = gs_variant_p
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF ( sy-subrc EQ 2 ).
    MESSAGE ID sy-msgid TYPE 'S' NUMBER   sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF ( gv_flag_exit IS INITIAL ).
      CLEAR                            p_dspvar.
      MOVE     gs_variant_p-variant TO p_dspvar.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_lvc_variant_f4
*eject
*&---------------------------------------------------------------------*
*&      Form  f_lvc_variant_exists
*&---------------------------------------------------------------------*
*       Validate that the LVC display variant exists
*----------------------------------------------------------------------*
FORM f_lvc_variant_exists.

  IF ( NOT ( p_dspvar IS INITIAL ) ).

    CLEAR                              gs_variant_p.
    MOVE     gs_variant             TO gs_variant_p.
    CLEAR                              gs_variant_p-variant.
    MOVE     p_dspvar               TO gs_variant_p-variant.

* Check that the LVC display variant exists

    CALL FUNCTION 'LVC_VARIANT_EXISTENCE_CHECK'
      EXPORTING
        I_SAVE        = 'A'
      CHANGING
        CS_VARIANT    = gs_variant_p
      EXCEPTIONS
        WRONG_INPUT   = 1
        NOT_FOUND     = 2
        PROGRAM_ERROR = 3
        OTHERS        = 4.

    IF ( sy-subrc EQ 0 ).
      CLEAR                            gs_variant.
      MOVE     gs_variant_p         TO gs_variant.
    ELSE.
      CLEAR                            gs_variant.
      MOVE     gv_repid             TO gs_variant-report.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CLEAR                              gs_variant.
    MOVE       gv_repid             TO gs_variant-report.
  ENDIF.

ENDFORM.                    " f_lvc_variant_exists
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_sel_opts
*&---------------------------------------------------------------------*
*       Validate the select-options
*----------------------------------------------------------------------*
FORM f_validate_sel_opts.

  CLEAR    gv_flag_exit.

* Check that either the posting date or the entry date is selected
  IF   ( ( s_budat[]                IS INITIAL ) AND
         ( s_cpudt[]                IS INITIAL )     ).

    MESSAGE  i000  WITH text-e01.

    CLEAR                              gv_flag_exit.
    MOVE     gc_x                   TO gv_flag_exit.

  ENDIF.

ENDFORM.                    " f_validate_sel_opts
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  DATA:    lwa_tbsl_vendor             TYPE ty_wa_tbsl,
           lra_tbsl_vendor             LIKE LINE OF grt_tbsl_vendor.

  CLEAR    git_xparam[].
  CLEAR    git_skb1[].
  CLEAR    git_item[].
  ClEAR    git_fld_cat_rpt1[].
  CLEAR    git_crt_chk_rpt[].

* Select the program parameters
  SELECT   *
    INTO   TABLE git_xparam
    FROM   zfit_xparam
   WHERE   paramtype = gc_paramtype
     AND   subtype   = gc_subtype.
  IF     ( sy-subrc EQ 0 ).
    SORT   git_xparam ASCENDING BY paramtype subtype
                                   key1 key2 key3 key4 key5.
  ELSE.
    CLEAR  git_xparam[].
  ENDIF.

  CLEAR    grt_tbsl_vendor[].
  CLEAR    git_tbsl_vendor[].

* Get the posting keys used in AP documents
  SELECT   bschl  shkzg  koart  xumsw
    INTO   TABLE git_tbsl_vendor
    FROM   tbsl
   WHERE   koart = gc_koart_vendor.
  IF     ( sy-subrc EQ 0 ).

    SORT     git_tbsl_vendor   ASCENDING BY bschl.

    CLEAR                                   lwa_tbsl_vendor.
    LOOP AT  git_tbsl_vendor           INTO lwa_tbsl_vendor.
      CLEAR                                 lra_tbsl_vendor.
      MOVE   'I'                         TO lra_tbsl_vendor-sign.
      MOVE   'EQ'                        TO lra_tbsl_vendor-option.
      MOVE   lwa_tbsl_vendor-bschl       TO lra_tbsl_vendor-low.
      APPEND lra_tbsl_vendor             TO grt_tbsl_vendor.
      CLEAR  lwa_tbsl_vendor.
    ENDLOOP.

  ELSE.
    CLEAR  git_tbsl_vendor[].
  ENDIF.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_data
*&---------------------------------------------------------------------*
*       Select the data
*----------------------------------------------------------------------*
FORM f_select_data.

  DATA:    lwa_bkpf            TYPE ty_wa_bkpf,
           lit_bkpf            TYPE ty_it_bkpf,
           lit_bkpf_p          TYPE ty_it_bkpf,
           lwa_bseg_key        TYPE ty_wa_bseg_key,
           lit_bseg_key        TYPE ty_it_bseg_key,
           lit_bseg            TYPE ty_it_bseg.

  DATA:    lv_blocksize        TYPE syindex,
           lv_index            TYPE syindex,
           lv_index_lo         TYPE syindex,
           lv_index_hi         TYPE syindex.

* Select the accounting document headers
  CLEAR    lit_bkpf[].
  SELECT   bukrs  belnr  gjahr  blart  bldat  budat  monat
           cpudt  cputm  aedat  upddt  wwert  usnam  tcode
           bvorg  xblnr  stblg  stjah  bktxt  waers  bstat
           xnetb  glvor  grpid  awtyp  awkey  hwaer  hwae2
           xstov  stodt  xmwst  ausbk  awsys  stgrd  ppnam
           xreversal     rldnr  ldgrp  xsplit
    INTO   TABLE  lit_bkpf
    FROM   bkpf
   WHERE   bukrs  IN s_bukrs
     AND   belnr  IN s_belnr
     AND   gjahr  IN s_gjahr
     AND   budat  IN s_budat
     AND   cpudt  IN s_cpudt
     AND   blart  IN s_blart.
  IF ( sy-subrc NE 0 ).
    CLEAR  lit_bkpf[].
    RETURN.
  ENDIF.

  SORT     lit_bkpf ASCENDING BY bukrs belnr gjahr.

*eject
* Perform blocking
  lv_blocksize      = gc_blocksize.
  IF ( lv_blocksize < 1 ).
    lv_blocksize    = 1.
  ENDIF.

* Get the document items in batches
  DO.

* Calculate the low and high indices for the batch of document items
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lv_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lv_blocksize ).

* Build the batch of documents
    CLEAR             lit_bkpf_p[].
    APPEND   LINES OF lit_bkpf
                 FROM lv_index_lo
                   TO lv_index_hi
                   TO lit_bkpf_p.

    IF   ( lit_bkpf_p[] IS INITIAL ).
      EXIT.
    ENDIF.

* Build the batch of document item keys
    CLEAR    lit_bseg_key[].

    CLEAR                              lwa_bkpf.
    LOOP AT  lit_bkpf_p           INTO lwa_bkpf.
      CLEAR                            lwa_bseg_key.
      MOVE   lwa_bkpf-bukrs         TO lwa_bseg_key-bukrs.
      MOVE   lwa_bkpf-belnr         TO lwa_bseg_key-belnr.
      MOVE   lwa_bkpf-gjahr         TO lwa_bseg_key-gjahr.
      APPEND                           lwa_bseg_key
                                    TO lit_bseg_key.
      CLEAR  lwa_bkpf.
    ENDLOOP.

*   PERFORM  f_issue_message USING text-pm3 SPACE.

*eject
* Select the accounting document items
    CLEAR    lit_bseg[].
    SELECT   bukrs  belnr  gjahr  buzei  augdt  augbl  bschl
             koart  shkzg  dmbtr  wrbtr  zuonr  sgtxt  vbund
             kokrs  kostl  aufnr  xauto  hkont  lifnr  xref1
             xref2  xref3
      INTO   TABLE  lit_bseg
      FROM   bseg   FOR ALL ENTRIES IN lit_bseg_key
     WHERE   bukrs  =   lit_bseg_key-bukrs
       AND   belnr  =   lit_bseg_key-belnr
       AND   gjahr  =   lit_bseg_key-gjahr.
    IF ( sy-subrc NE 0 ).
      CLEAR  lit_bseg[].
    ELSE.
      IF   ( s_hkont[] IS NOT INITIAL ).
        DELETE   lit_bseg WHERE shkzg     NE 'H'.
        DELETE   lit_bseg WHERE hkont NOT IN s_hkont.
      ENDIF.
    ENDIF.

    PERFORM  f_build_item_table     TABLES  lit_bkpf_p
                                            lit_bseg.

  ENDDO.

ENDFORM.                    " f_select_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_build_item_table
*&---------------------------------------------------------------------*
*       Build the item table
*----------------------------------------------------------------------*
FORM f_build_item_table
  TABLES   iit_bkpf TYPE ty_it_bkpf
           iit_bseg TYPE ty_it_bseg.

  DATA:    lwa_bkpf   TYPE ty_wa_bkpf,
           lwa_bseg   TYPE ty_wa_bseg,
           lwa_skb1   TYPE ty_wa_skb1,
           lwa_item   TYPE ty_wa_item,
           lwa_xparam TYPE ty_wa_xparam.

  SORT     iit_bkpf ASCENDING BY bukrs belnr gjahr.
  SORT     iit_bseg ASCENDING BY bukrs belnr gjahr buzei.

  CLEAR    lwa_bkpf.

  CLEAR                                lwa_bseg.
  LOOP AT  iit_bseg               INTO lwa_bseg.

    AT NEW gjahr.
      CLEAR                            lwa_bkpf.
      READ     TABLE iit_bkpf     INTO lwa_bkpf
                              WITH KEY bukrs = lwa_bseg-bukrs
                                       belnr = lwa_bseg-belnr
                                       gjahr = lwa_bseg-gjahr
                         BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CLEAR  lwa_bkpf.
      ENDIF.
    ENDAT.

* Get the house bank and bank account short keys
    IF     ( ( lwa_skb1-bukrs       EQ lwa_bseg-bukrs ) AND
             ( lwa_skb1-saknr       EQ lwa_bseg-hkont )     ).
    ELSE.

      CLEAR                            lwa_skb1.
      MOVE       lwa_bseg-bukrs     TO lwa_skb1-bukrs.
      MOVE       lwa_bseg-hkont     TO lwa_skb1-saknr.

      PERFORM    f_get_bank_account_keys
                              CHANGING lwa_skb1.

    ENDIF.

*eject
    IF     ( ( lwa_bkpf-bukrs       EQ lwa_bseg-bukrs ) AND
             ( lwa_bkpf-belnr       EQ lwa_bseg-belnr ) AND
             ( lwa_bkpf-gjahr       EQ lwa_bseg-gjahr )     ).
      CLEAR                            lwa_item.
      MOVE     lwa_bseg-bukrs       TO lwa_item-bukrs.
      MOVE     lwa_bseg-belnr       TO lwa_item-belnr.
      MOVE     lwa_bseg-gjahr       TO lwa_item-gjahr.
      MOVE     lwa_bseg-buzei       TO lwa_item-buzei.
      MOVE     lwa_bkpf-blart       TO lwa_item-blart.
      MOVE     lwa_bkpf-budat       TO lwa_item-budat.
      MOVE     lwa_bkpf-cpudt       TO lwa_item-cpudt.
      MOVE     lwa_bkpf-xblnr       TO lwa_item-xblnr.
      MOVE     lwa_bkpf-waers       TO lwa_item-waers.
      MOVE     lwa_bseg-koart       TO lwa_item-koart.
      MOVE     lwa_bseg-shkzg       TO lwa_item-shkzg.
      MOVE     lwa_bseg-dmbtr       TO lwa_item-dmbtr.
      MOVE     lwa_bseg-wrbtr       TO lwa_item-wrbtr.
      MOVE     lwa_bseg-hkont       TO lwa_item-hkont.
      MOVE     lwa_bseg-sgtxt       TO lwa_item-sgtxt.
*     MOVE     gc_hbkid             TO lwa_item-hbkid.
*     MOVE     gc_hktid             TO lwa_item-hktid.
      MOVE     lwa_skb1-hbkid       TO lwa_item-hbkid.
      MOVE     lwa_skb1-hktid       TO lwa_item-hktid.
* Start of changes by AHAMDT for CHG0206262
*      MOVE     gc_zort1             TO lwa_item-zort1.
*      MOVE     gc_zland             TO lwa_item-zland.
* End of changes by AHMADT for CHG0206262

* If the document is a payment, then retrieve the invoice data
      CLEAR                            lwa_xparam.
      READ     TABLE git_xparam   INTO lwa_xparam
                              WITH KEY paramtype = gc_paramtype
                                       subtype   = gc_subtype
                                       key1      = gc_key1
                                       key2      = lwa_bkpf-blart
                                       key3      = lwa_bseg-hkont.
      IF     ( sy-subrc EQ 0 ).

        PERFORM  f_retrieve_invoice_data
                              CHANGING lwa_item.

      ENDIF.

      APPEND                           lwa_item
                                    TO git_item.
    ENDIF.

    CLEAR  lwa_bseg.
  ENDLOOP.

ENDFORM.                    " f_build_item_table
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_bank_account_keys
*&---------------------------------------------------------------------*
*       Get the house bank and bank account short keys
*----------------------------------------------------------------------*
FORM f_get_bank_account_keys
  CHANGING cwa_skb1          TYPE ty_wa_skb1.

  DATA:    lwa_skb1          TYPE ty_wa_skb1.

  DATA:    lv_subrc          TYPE sysubrc,
           lv_tabix          TYPE sytabix.

  IF   ( ( cwa_skb1-bukrs      IS INITIAL ) OR
         ( cwa_skb1-saknr      IS INITIAL )    ).
    RETURN.
  ENDIF.

  CLEAR                           lwa_skb1.
  READ     TABLE git_skb1    INTO lwa_skb1
                         WITH KEY bukrs = cwa_skb1-bukrs
                                  saknr = cwa_skb1-saknr
                    BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.

  IF     ( lv_subrc NE 0 ).

    CLEAR    lwa_skb1.
    SELECT   SINGLE bukrs  saknr  hbkid  hktid
      INTO   lwa_skb1
      FROM   skb1
     WHERE   bukrs = cwa_skb1-bukrs
       AND   saknr = cwa_skb1-saknr.
    IF     ( sy-subrc EQ 0 ).
      INSERT lwa_skb1        INTO git_skb1 INDEX lv_tabix.
    ELSE.
      RETURN.
    ENDIF.

  ENDIF.

  CLEAR                           cwa_skb1.
  MOVE     lwa_skb1            TO cwa_skb1.

ENDFORM.                    " f_get_bank_account_keys
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_data
*&---------------------------------------------------------------------*
*       Process the data
*----------------------------------------------------------------------*
FORM f_process_data.

  DATA:    lwa_item          TYPE ty_wa_item,
           lwa_crt_chk_rpt   TYPE ty_wa_crt_chk_rpt,
           lwa_crt_chk_rpt_p TYPE ty_wa_crt_chk_rpt.

  DATA:    lv_flag_error     TYPE flag,
           lv_cnt            TYPE i.

  CLEAR                                lwa_item.
  LOOP AT  git_item               INTO lwa_item.

    CLEAR                              lwa_crt_chk_rpt.
    MOVE     lwa_item-belnr         TO lwa_crt_chk_rpt-vblnr.
    MOVE     lwa_item-bukrs         TO lwa_crt_chk_rpt-zbukr.
    MOVE     lwa_item-gjahr         TO lwa_crt_chk_rpt-gjahr.
    MOVE     lwa_item-hbkid         TO lwa_crt_chk_rpt-hbkid.
    MOVE     lwa_item-hktid         TO lwa_crt_chk_rpt-hktid.
    SHIFT    lwa_item-xblnr       LEFT DELETING LEADING SPACE.
*   lv_cnt = STRLEN( lwa_item-xblnr ).
*   IF ( ( lv_cnt EQ 6 ) AND ( lwa_item-hbkid+0(3) NE 'BOA' ) ).
*     CONCATENATE '0' lwa_item-xblnr INTO lwa_item-xblnr.
*     CONDENSE        lwa_item-xblnr NO-GAPS.
*   ENDIF.
    MOVE     lwa_item-xblnr         TO lwa_crt_chk_rpt-chect.
    MOVE     lwa_item-wrbtr         TO lwa_crt_chk_rpt-rwbtr.
    MOVE     lwa_item-waers         TO lwa_crt_chk_rpt-waers.
    SHIFT    lwa_item-sgtxt       LEFT DELETING LEADING SPACE.
    MOVE     lwa_item-sgtxt         TO lwa_crt_chk_rpt-znme1.
    MOVE     lwa_item-zort1         TO lwa_crt_chk_rpt-zort1.
    MOVE     lwa_item-zland         TO lwa_crt_chk_rpt-zland.

* Post the check
    IF     ( cb_tstrn               IS INITIAL ).

      CLEAR                            lwa_crt_chk_rpt_p.
      MOVE   lwa_crt_chk_rpt        TO lwa_crt_chk_rpt_p.
      CLEAR                            lv_flag_error.

      PERFORM  f_post_check   CHANGING lwa_crt_chk_rpt_p
                                       lv_flag_error.

*eject
      CLEAR                            lwa_crt_chk_rpt-msgtyp.
      MOVE     lwa_crt_chk_rpt_p-msgtyp
                                    TO lwa_crt_chk_rpt-msgtyp.

      CLEAR                            lwa_crt_chk_rpt-msgid.
      MOVE     lwa_crt_chk_rpt_p-msgid
                                    TO lwa_crt_chk_rpt-msgid.

      CLEAR                            lwa_crt_chk_rpt-msgnr.
      MOVE     lwa_crt_chk_rpt_p-msgnr
                                    TO lwa_crt_chk_rpt-msgnr.
      CLEAR                            lwa_crt_chk_rpt-message.
      MOVE     lwa_crt_chk_rpt_p-message
                                    TO lwa_crt_chk_rpt-message.

      IF     ( lv_flag_error    IS NOT INITIAL ).

        MOVE   gc_color_errr        TO lwa_crt_chk_rpt-linecolor.

      ENDIF.

    ENDIF.

    APPEND                             lwa_crt_chk_rpt
                                    TO git_crt_chk_rpt.

    CLEAR    lwa_item.
  ENDLOOP.

ENDFORM.                    " f_process_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_alv_grid_rpt1
*&---------------------------------------------------------------------*
*       Create the ALV grid
*----------------------------------------------------------------------*
FORM f_create_alv_grid_rpt1.

  IF ( go_custom_cntn_rpt1 IS INITIAL ).

    IF ( cl_gui_alv_grid=>offline( ) IS INITIAL ).

* Create an instance of the custom container for the ALV grid
      CREATE OBJECT go_custom_cntn_rpt1
        EXPORTING
          CONTAINER_NAME              = gc_alv_cntn_rpt1
        EXCEPTIONS
          CNTL_ERROR                  = 1
          CNTL_SYSTEM_ERROR           = 2
          CREATE_ERROR                = 3
          LIFETIME_ERROR              = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5.

      IF ( sy-subrc NE 0 ).
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = gv_repid
            TXT1  = text-E51
            TXT2  = sy-subrc.
        LEAVE PROGRAM.
      ENDIF.

    ENDIF.

* Create an instance of the ALV grid
    CREATE OBJECT go_grid_rpt1
      EXPORTING
        I_PARENT = go_custom_cntn_rpt1.

* Build the ALV field catalog

    PERFORM  f_build_fieldcatalog_rpt1
                                TABLES git_fld_cat_rpt1.

*eject
* Build the ALV layout
    CLEAR                              gs_layout.
    MOVE     gc_x                   TO gs_layout-zebra.
    MOVE     'LINECOLOR'            TO gs_layout-info_fname.
    MOVE     gc_x                   TO gs_layout-cwidth_opt.
    MOVE     text-R01               TO gs_layout-grid_title.
    MOVE     'A'                    TO gs_layout-sel_mode.

* At event "PRINT_END_OF_PAGE", two lines must be reserved
    CLEAR                              gs_print.
    MOVE     2                      TO gs_print-reservelns.

* Create an instance of the event receiver
    CREATE OBJECT go_event_receiver.

* Link event handler methods
* When ALV control raises the event, the method is automatically called
    SET HANDLER go_event_receiver->handle_top_of_page
            FOR go_grid_rpt1.
    SET HANDLER go_event_receiver->handle_end_of_page
            FOR go_grid_rpt1.
    SET HANDLER go_event_receiver->handle_top_of_list
            FOR go_grid_rpt1.
    SET HANDLER go_event_receiver->handle_end_of_list
            FOR go_grid_rpt1.

    CALL METHOD go_grid_rpt1->set_table_for_first_display
      EXPORTING
        IS_LAYOUT       = gs_layout
        IS_PRINT        = gs_print
        IS_VARIANT      = gs_variant
        I_SAVE          = 'A'
      CHANGING
        IT_OUTTAB       = git_crt_chk_rpt[]
        IT_FIELDCATALOG = git_fld_cat_rpt1[].

  ELSE.

    CALL METHOD go_grid_rpt1->refresh_table_display.

  ENDIF.

* Set focus to ensure that the cursor is active in the control
  IF ( cl_gui_alv_grid=>offline( ) IS INITIAL ).

    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        CONTROL = go_grid_rpt1.

  ENDIF.

ENDFORM.                    " f_create_alv_grid_rpt1
*eject
*&---------------------------------------------------------------------*
*&      Form  f_build_fieldcatalog_rpt1
*&---------------------------------------------------------------------*
*       Build the ALV field catalog
*----------------------------------------------------------------------*
FORM f_build_fieldcatalog_rpt1
  TABLES   cit_field_catalog           TYPE lvc_t_fcat.

  FIELD-SYMBOLS: <lfs_field_catalog>   TYPE lvc_s_fcat.

* Build the field catalog according to the ALV structure
  REFRESH                cit_field_catalog.
  CLEAR                  cit_field_catalog.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = gc_alv_stru_rpt1
    CHANGING
      CT_FIELDCAT      = cit_field_catalog[].

  CLEAR    cit_field_catalog.
  LOOP AT  cit_field_catalog ASSIGNING <lfs_field_catalog>.

    CASE <lfs_field_catalog>-fieldname.

      WHEN     'MSGID'.
*       CLEAR                          <lfs_field_catalog>-tech.
*       MOVE     gc_x               TO <lfs_field_catalog>-tech.

      WHEN     'LINECOLOR'.
        CLEAR                          <lfs_field_catalog>-tech.
        MOVE     gc_x               TO <lfs_field_catalog>-tech.

    ENDCASE.

    CLEAR  cit_field_catalog.
  ENDLOOP.

ENDFORM.                    " f_build_fieldcatalog_rpt1
*eject
*&---------------------------------------------------------------------*
*&      Form  f_exit_alv_screen_rpt1
*&---------------------------------------------------------------------*
*       Exit the summary report ALV screen and return
*----------------------------------------------------------------------*
FORM f_exit_alv_screen_rpt1.

* Free space held by ALV
  IF ( go_custom_cntn_rpt1 IS NOT INITIAL ).

    CALL METHOD go_grid_rpt1->free.
    CLEAR       go_grid_rpt1.

    CALL METHOD go_custom_cntn_rpt1->free.
    CLEAR       go_custom_cntn_rpt1.

  ENDIF.

* Synchronize the automation queue

  CALL METHOD cl_gui_cfw=>flush.

  IF ( sy-subrc NE 0 ).

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        TITEL = gv_repid
        TXT1  = text-E53
        TXT2  = sy-subrc.

  ENDIF.

  IF     ( ( gv_ucomm EQ 'BACK' ) OR
           ( gv_ucomm EQ 'EXIT' )    ).

* Return to list processing
    SET      SCREEN 0.
    LEAVE    SCREEN.

  ELSEIF   ( gv_ucomm EQ 'CANC' ).

* Exit program
    LEAVE    PROGRAM.

  ENDIF.

ENDFORM.                    " f_exit_alv_screen_rpt1
*eject
*&---------------------------------------------------------------------*
*&      Form  f_post_check
*&---------------------------------------------------------------------*
*       Post the check
*----------------------------------------------------------------------*
FORM f_post_check
  CHANGING cwa_crt_chk_rpt        TYPE ty_wa_crt_chk_rpt
           cv_flag_error          TYPE flag.

  DATA:    lwa_bdcopts            TYPE ctu_params,
           lwa_bdcmsg             TYPE ty_wa_bdcmsg,
           lit_bdcmsg             TYPE ty_it_bdcmsg,
           lv_tcode               TYPE sytcode,
           lv_message_text_output TYPE bapi_msg,
           lv_message_text        TYPE bapi_msg.

  CLEAR    cv_flag_error.

* Populate the bdcdata
  CLEAR    git_bdcdata[].

  PERFORM  f_populate_bdcdata USING    cwa_crt_chk_rpt.

* Call the transaction
  CLEAR                                lv_tcode.
  MOVE     gc_tcode                 TO lv_tcode.
  CLEAR                                lwa_bdcopts.
  MOVE     'N'                      TO lwa_bdcopts-dismode.
  MOVE     'S'                      TO lwa_bdcopts-updmode.
  MOVE     'X'                      TO lwa_bdcopts-defsize.

  CLEAR    lit_bdcmsg[].

  CALL     TRANSACTION  lv_tcode
                 USING  git_bdcdata
          OPTIONS FROM  lwa_bdcopts
         MESSAGES INTO  lit_bdcmsg.

  IF   ( sy-subrc NE 0 ).

    CLEAR    lv_message_text.

* Process the errors
    CLEAR                              lwa_bdcmsg.
    LOOP AT  lit_bdcmsg           INTO lwa_bdcmsg
                                 WHERE ( ( msgtyp EQ 'E' ) OR
                                         ( msgtyp EQ 'A' )    )
                                   AND (   msgnr  NE '298'    ).

*eject
* Format the message
      CLEAR    lv_message_text_output.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = lwa_bdcmsg-msgid
          MSGNR               = lwa_bdcmsg-msgnr
          MSGV1               = lwa_bdcmsg-msgv1
          MSGV2               = lwa_bdcmsg-msgv2
          MSGV3               = lwa_bdcmsg-msgv3
          MSGV4               = lwa_bdcmsg-msgv4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = lv_message_text_output.

      CONCATENATE                      lv_message_text
                                       lv_message_text_output '/'
                                  INTO lv_message_text
                          SEPARATED BY SPACE.

      IF   ( cwa_crt_chk_rpt-msgnr  IS INITIAL ).
        CLEAR                          cwa_crt_chk_rpt-msgtyp.
        MOVE      lwa_bdcmsg-msgtyp TO cwa_crt_chk_rpt-msgtyp.
        CLEAR                          cwa_crt_chk_rpt-msgid.
        MOVE      lwa_bdcmsg-msgid  TO cwa_crt_chk_rpt-msgid.
        CLEAR                          cwa_crt_chk_rpt-msgnr.
        MOVE      lwa_bdcmsg-msgnr  TO cwa_crt_chk_rpt-msgnr.
      ENDIF.

      CLEAR  lwa_bdcmsg.
    ENDLOOP.

    CLEAR                              cwa_crt_chk_rpt-message.
    MOVE     lv_message_text        TO cwa_crt_chk_rpt-message.
    CLEAR                              cv_flag_error.
    MOVE     gc_x                   TO cv_flag_error.

  ELSE.

    CLEAR                              cwa_crt_chk_rpt-message.
    MOVE     text-p01               TO cwa_crt_chk_rpt-message.

  ENDIF.

ENDFORM.                    " f_post_check
*eject
*&---------------------------------------------------------------------*
*&      Form  f_populate_bdcdata
*&---------------------------------------------------------------------*
*       Populate the bdcdata
*----------------------------------------------------------------------*
FORM f_populate_bdcdata
  USING    iwa_crt_chk_rpt TYPE ty_wa_crt_chk_rpt.

*          Start of changes by AHMADT for CHG0180658
  CONSTANTS: lc_ON TYPE CHAR2 VALUE 'ON'.
*          End of changes by AHMADT for CHG0180658

  PERFORM: f_fill_bdcdata USING 'SAPMFCHK' '0500' 'X'   ' '  ' ',
           f_fill_bdcdata USING ''  ''  '' 'BDC_CURSOR' 'PAYR-VBLNR',
           f_fill_bdcdata USING ''  ''  '' 'PAYR-VBLNR'
                                           iwa_crt_chk_rpt-vblnr,
           f_fill_bdcdata USING ''  ''  '' 'PAYR-ZBUKR'
                                           iwa_crt_chk_rpt-zbukr,
           f_fill_bdcdata USING ''  ''  '' 'PAYR-GJAHR'
                                           iwa_crt_chk_rpt-gjahr,
           f_fill_bdcdata USING ''  ''  '' 'PAYR-HBKID'
                                           iwa_crt_chk_rpt-hbkid,
           f_fill_bdcdata USING ''  ''  '' 'PAYR-HKTID'
                                           iwa_crt_chk_rpt-hktid,
           f_fill_bdcdata USING ''  ''  '' 'PAYR-CHECT'
                                           iwa_crt_chk_rpt-chect,
           f_fill_bdcdata USING ''  ''  '' 'BDC_OKCODE' '/00',
           f_fill_bdcdata USING 'SAPMFCHK' '0501' 'X'   ' '  ' ',
           f_fill_bdcdata USING ''  ''  '' 'BDC_CURSOR' 'PAYR-ZNME1'.
*  Start of changes by AHMADT for CHG0206262
**--START OF CHANGES BY AKMADASU FOR CHG0206262
*           f_fill_bdcdata USING ''  ''  '' 'PAYR-ZNME1'
*                                           iwa_crt_chk_rpt-znme1,
*  IF iwa_crt_chk_rpt-znme1 IS NOT INITIAL.
*    PERFORM        f_fill_bdcdata USING ''  ''  '' 'PAYR-ZNME1'
*                                            iwa_crt_chk_rpt-znme1.
*  else.
*    PERFORM        f_fill_bdcdata USING ''  ''  '' 'PAYR-ZNME1'
*                                                    'OTROW'.
*  endif.
**     f_fill_bdcdata USING ''  ''  '' 'PAYR-ZORT1'
**                                      iwa_crt_chk_rpt-zort1,
*  if iwa_crt_chk_rpt-zort1 is not INITIAL.
*    perform:f_fill_bdcdata USING ''  ''  '' 'PAYR-ZORT1'
*                                       iwa_crt_chk_rpt-zort1.
*  else.
*    perform:f_fill_bdcdata USING ''  ''  '' 'PAYR-ZORT1'
*                                     'HOUSTON'.
*  endif.
**    f_fill_bdcdata USING ''  ''  '' 'PAYR-ZLAND'
**                                  iwa_crt_chk_rpt-zland,
*  IF  iwa_crt_chk_rpt-zland is NOT INITIAL.
*  PERFORM  f_fill_bdcdata USING ''  ''  '' 'PAYR-ZLAND'
*                                  iwa_crt_chk_rpt-zland.
*  else.
*    PERFORM  f_fill_bdcdata USING ''  ''  '' 'PAYR-ZLAND'
*                                  'US'.
*  ENDIF.

**--END OF CHANGES BY AKMADASU FOR CHG0206262
*  End of changes by AHMADT for CHG0206262
*          Start of changes by AHMADT for CHG0180658
PERFORM:  f_fill_bdcdata USING ''  ''  '' 'PAYR-ZREGI' lc_ON,
*          End of changes by AHMADT for CHG0180658
  f_fill_bdcdata USING ''  ''  '' 'BDC_OKCODE' '=UPDA'.

ENDFORM.                    " f_populate_bdcdata
*eject
*&---------------------------------------------------------------------*
*&      Form  f_fill_bdcdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_fill_bdcdata
  USING    value(p_program)
           value(p_dynpro)
           value(p_dynbegin)
           value(p_fnam)
           value(p_fval).

  DATA:    lwa_bdcdata TYPE ty_wa_bdcdata.

  CLEAR    lwa_bdcdata.

  IF     ( p_dynbegin EQ gc_x ).
    lwa_bdcdata-program  = p_program.
    lwa_bdcdata-dynpro   = p_dynpro.
    lwa_bdcdata-dynbegin = p_dynbegin.
  ELSE.
    lwa_bdcdata-fnam     = p_fnam.
    lwa_bdcdata-fval     = p_fval.
  ENDIF.

  APPEND   lwa_bdcdata TO git_bdcdata.

ENDFORM.                    " f_fill_bdcdata
*eject
*&---------------------------------------------------------------------*
*&      Form  f_retrieve_invoice_data
*&---------------------------------------------------------------------*
*       Retrieve the invoice data
*----------------------------------------------------------------------*
FORM f_retrieve_invoice_data
  CHANGING cwa_item   TYPE ty_wa_item.

  DATA:    lit_bseg   TYPE ty_it_bseg,
           lwa_bseg   TYPE ty_wa_bseg,
           lit_bsak   TYPE ty_it_bsak,
           lwa_bsak   TYPE ty_wa_bsak.

* Select the accounting document items
  CLEAR    lit_bseg[].
  SELECT   bukrs  belnr  gjahr  buzei  augdt  augbl  bschl
           koart  shkzg  dmbtr  wrbtr  zuonr  sgtxt  vbund
           kokrs  kostl  aufnr  xauto  hkont  lifnr  xref1
           xref2  xref3
    INTO   TABLE  lit_bseg
    FROM   bseg
   WHERE   bukrs  = cwa_item-bukrs
     AND   belnr  = cwa_item-belnr
     AND   gjahr  = cwa_item-gjahr.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

* Read the vendor line item
  CLEAR                                lwa_bseg.
  READ     TABLE lit_bseg         INTO lwa_bseg
                              WITH KEY koart = gc_koart_vendor.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

* Select the clearing information
  CLEAR    lit_bsak[].
  SELECT   bukrs  lifnr  umsks  umskz  augdt  augbl  zuonr
           gjahr  belnr  buzei  xblnr  blart  bschl  sgtxt
    INTO   TABLE lit_bsak
    FROM   bsak
   WHERE   bukrs = lwa_bseg-bukrs
     AND   lifnr = lwa_bseg-lifnr
     AND   augdt = lwa_bseg-augdt
     AND   augbl = lwa_bseg-augbl.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

*eject
* Read the invoice item
  CLEAR                                lwa_bsak.
  LOOP AT  lit_bsak               INTO lwa_bsak.
    IF       ( lwa_bsak-bschl       IN grt_tbsl_vendor ).
      MOVE     lwa_bsak-xblnr       TO cwa_item-xblnr.
      MOVE     lwa_bsak-sgtxt       TO cwa_item-sgtxt.
      EXIT.
    ENDIF.
    CLEAR  lwa_bsak.
  ENDLOOP.

ENDFORM.                    " f_retrieve_invoice_data
