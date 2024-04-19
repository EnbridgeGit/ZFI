*&---------------------------------------------------------------------*
*&  Include           ZFFIC001_PWC_AUDIT_EXTRACT_F01
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFFIC001_PWC_AUDIT_EXTRACT                        *
*  Include:          ZFFIC001_PWC_AUDIT_EXTRACT_F01                    *
*  Author:           John Hartung                                      *
*  Date:             August 24, 2017                                   *
*  Application Area: FICO                                              *
*                                                                      *
*  Description:      FI PWC Audit Extract F01 (Forms - Subroutines)    *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 24Aug17  JRHARTUNG ACR-4863 DECK917823, DECK917837, DECK917853       *
*                    PWC Audit Extract - initial program creation      *
*----------------------------------------------------------------------*
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  CLEAR    gt_t001[].
  CLEAR    gt_bkpf_key[].
  CLEAR    gt_bkpf[].
  CLEAR    gt_bseg[].

  CLEAR    gv_fdlm1.
  CLEAR    gv_file_seqn.
  CLEAR    gv_file_size.
  CLEAR    gv_file_size_brk.
  CLEAR    gv_file_name.
  CLEAR    gv_file_name_hdr.
  CLEAR    gv_file_name_itm.
  CLEAR    gv_file_name_trn.
  CLEAR    gv_doc_count.

  CLEAR                                     gv_datum_run.
  MOVE     sy-datum                      TO gv_datum_run.
  CLEAR                                     gv_uzeit_run.
  MOVE     sy-uzeit                      TO gv_uzeit_run.

  IF       ( rb_g1ex1                    IS NOT INITIAL ).

    IF     ( p_fdlm1                     IS INITIAL ).
      MOVE   cl_abap_char_utilities=>horizontal_tab
                                         TO gv_fdlm1.
    ELSE.
      MOVE   p_fdlm1                     TO gv_fdlm1.
    ENDIF.

    gv_file_size_brk = p_fbrk1 * 1000000.
    gv_file_size     = gv_file_size_brk + 1.

    IF         ( ( p_fpth1               IS NOT INITIAL ) AND
                 ( p_fnam1               IS NOT INITIAL ) AND
                 ( p_fext1               IS NOT INITIAL )     ).
      CONCATENATE  p_fpth1
                   p_fnam1             INTO gv_file_name.
    ENDIF.

  ENDIF.

*eject
* Select the company code data
  SELECT   bukrs  butxt  land1  waers  ktopl
    INTO   TABLE gt_t001
    FROM   t001.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_t001 ASCENDING BY bukrs.
  ELSE.
    CLEAR  gt_t001[].
  ENDIF.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_gl_detail_data
*&---------------------------------------------------------------------*
*       Extract the G/L detail data
*----------------------------------------------------------------------*
FORM f_gl_detail_data.

  DATA:    ls_doc_key_1                TYPE ty_wa_doc_key,
           ls_doc_key_2                TYPE ty_wa_doc_key,
           ls_bkpf_key                 TYPE ty_wa_bkpf_key,
           lt_bkpf_key                 TYPE ty_it_bkpf_key,
           ls_bkpf                     TYPE ty_wa_bkpf,
           lt_bseg                     TYPE ty_it_bseg.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_tabix_lo                 TYPE sytabix,
           lv_tabix_hi                 TYPE sytabix,
           lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex,
           lv_blocksize                TYPE syindex,
           lv_lines                    TYPE syindex.

  FIELD-SYMBOLS: <fs_bseg>             TYPE ty_wa_bseg.

  lv_blocksize = p_dpbc1.

  CLEAR    gt_bkpf_key[].

* Select the document keys
  SELECT   bukrs  belnr  gjahr  monat
    INTO   TABLE gt_bkpf_key
    FROM   bkpf
   WHERE   bukrs IN s_bukrs1
     AND   belnr IN s_belnr1
     AND   gjahr IN s_gjahr1
     AND   monat IN s_monat1.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_bkpf_key[].
    RETURN.
  ENDIF.

  SORT     gt_bkpf_key ASCENDING BY gjahr  monat  bukrs  belnr.

*eject
* Process the accounting documents in batches
  DO.

* Calculate the low and high indices for the batch of WBS elements
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lv_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lv_blocksize ).

* Build the batch of WBS elements
    CLEAR             lt_bkpf_key[].
    APPEND   LINES OF gt_bkpf_key
                 FROM lv_index_lo
                   TO lv_index_hi
                   TO lt_bkpf_key.

    IF     ( lt_bkpf_key[] IS INITIAL ).
      EXIT.
    ENDIF.

    CLEAR    gt_bkpf[].
    CLEAR    gt_bseg[].

* Select the accounting document headers
    SELECT   *
      INTO   TABLE gt_bkpf
      FROM   bkpf FOR ALL ENTRIES IN lt_bkpf_key
     WHERE   bukrs = lt_bkpf_key-bukrs
       AND   belnr = lt_bkpf_key-belnr
       AND   gjahr = lt_bkpf_key-gjahr.
    IF     ( sy-subrc EQ 0 ).

      SORT   gt_bkpf ASCENDING BY mandt bukrs belnr gjahr.

*eject
* Select the accounting document items
      SELECT   *
        INTO   TABLE gt_bseg
        FROM   bseg FOR ALL ENTRIES IN gt_bkpf
       WHERE   bukrs = gt_bkpf-bukrs
         AND   belnr = gt_bkpf-belnr
         AND   gjahr = gt_bkpf-gjahr.
      IF     ( sy-subrc EQ 0 ).

        SORT   gt_bseg ASCENDING BY mandt bukrs belnr gjahr buzei.

      ELSE.
        CLEAR  gt_bseg[].
      ENDIF.
    ELSE.
      CLEAR  gt_bkpf[].
      CLEAR  gt_bseg[].
    ENDIF.

    IF     ( gt_bkpf[] IS INITIAL ).
      CONTINUE.
    ENDIF.

*eject
* Format and extract the G/L detail data
    DESCRIBE TABLE gt_bseg            LINES lv_lines.

    lv_tabix    = 1.
    lv_tabix_hi = lv_lines.

    CLEAR                                   ls_bkpf.
    LOOP AT  gt_bkpf                   INTO ls_bkpf.

      CLEAR                                 ls_doc_key_1.
      MOVE     ls_bkpf-bukrs             TO ls_doc_key_1-bukrs.
      MOVE     ls_bkpf-belnr             TO ls_doc_key_1-belnr.
      MOVE     ls_bkpf-gjahr             TO ls_doc_key_1-gjahr.

      lv_tabix_lo = lv_tabix.

      CLEAR    lt_bseg[].

      LOOP AT  gt_bseg            ASSIGNING <fs_bseg>
                                       FROM lv_tabix_lo
                                         TO lv_tabix_hi.
        lv_tabix = sy-tabix.

        CLEAR                               ls_doc_key_2.
        MOVE     <fs_bseg>-bukrs         TO ls_doc_key_2-bukrs.
        MOVE     <fs_bseg>-belnr         TO ls_doc_key_2-belnr.
        MOVE     <fs_bseg>-gjahr         TO ls_doc_key_2-gjahr.

        IF     ( ls_doc_key_2            LT ls_doc_key_1 ).
        ELSEIF ( ls_doc_key_2            EQ ls_doc_key_1 ).
          APPEND <fs_bseg>               TO lt_bseg.
        ELSE.
          EXIT.
        ENDIF.

      ENDLOOP.

*eject
      IF       ( gv_file_size            GT gv_file_size_brk ).
        CLEAR    gv_file_size.

        PERFORM  f_file_break         USING gv_file_name
                                   CHANGING gv_file_name_hdr
                                            gv_file_name_itm
                                            gv_file_name_trn.

      ENDIF.

      PERFORM  f_format_gl_detail_data
                                     TABLES lt_bseg
                                      USING ls_bkpf.

      ADD      1                         TO gv_doc_count.

      CLEAR  ls_bkpf.
    ENDLOOP.

  ENDDO.

  PERFORM  f_file_break               USING SPACE
                                   CHANGING gv_file_name_hdr
                                            gv_file_name_itm
                                            gv_file_name_trn.

ENDFORM.                    " f_gl_detail_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_gl_detail_data
*&---------------------------------------------------------------------*
*       Format and extract the G/L detail data
*----------------------------------------------------------------------*
FORM f_format_gl_detail_data
  TABLES   it_bseg                     TYPE ty_it_bseg
  USING    is_bkpf                     TYPE ty_wa_bkpf.

  DATA:    ls_out_gl_detl_hdr          TYPE ty_wa_out_gl_detl,
           ls_out_gl_detl              TYPE ty_wa_out_gl_detl.

  DATA:    ls_journal_id               TYPE ty_wa_journal_id.

  DATA:    lv_amount                   TYPE char20,
           lv_quantity                 TYPE char20,
           lv_rate                     TYPE char20,
           lv_string                   TYPE STRING.

  FIELD-SYMBOLS: <fs_bseg>             TYPE ty_wa_bseg.

  IF     ( is_bkpf-bukrs IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    ls_out_gl_detl_hdr.
  CLEAR    ls_out_gl_detl.

* Read the company code data
  IF     ( gs_t001-bukrs                 EQ is_bkpf-bukrs ).
  ELSE.
    CLEAR                                   gs_t001.
    READ     TABLE gt_t001             INTO gs_t001
                                   WITH KEY bukrs = is_bkpf-bukrs.
    IF     ( sy-subrc NE 0 ).
      CLEAR                                 gs_t001.
    ENDIF.
  ENDIF.

*eject
* Build the output header
* Journal ID
  CLEAR                                     ls_journal_id.
  MOVE     is_bkpf-monat                 TO ls_journal_id-monat.
  MOVE     ','                           TO ls_journal_id-comma_1.
  MOVE     is_bkpf-gjahr                 TO ls_journal_id-gjahr.
  MOVE     ','                           TO ls_journal_id-comma_2.
  MOVE     is_bkpf-bukrs                 TO ls_journal_id-bukrs.
  MOVE     ','                           TO ls_journal_id-comma_3.
  MOVE     is_bkpf-belnr                 TO ls_journal_id-belnr.
  CONCATENATE '"' ls_journal_id '"'    INTO
                                       ls_out_gl_detl_hdr-journal_id.

* Header Description
  CLEAR                                     lv_string.
  MOVE     is_bkpf-bktxt                 TO lv_string.
  PERFORM  f_string_remove_spcl_char  USING p_fdlm1
                                   CHANGING lv_string.
  CONCATENATE '"' lv_string '"'        INTO ls_out_gl_detl_hdr-bktxt.

* Source
  CONCATENATE '"' is_bkpf-glvor '/'
                  is_bkpf-awtyp '"'    INTO ls_out_gl_detl_hdr-source.

* Business Unit Code
  CONCATENATE '"' is_bkpf-bukrs '"'    INTO ls_out_gl_detl_hdr-bukrs.

* Fiscal Year
  MOVE            is_bkpf-gjahr          TO ls_out_gl_detl_hdr-gjahr.

* Period
  MOVE            is_bkpf-monat          TO ls_out_gl_detl_hdr-monat.

* Effective Date
  MOVE            is_bkpf-budat          TO ls_out_gl_detl_hdr-budat.

* Entry Date
  MOVE            is_bkpf-cpudt          TO ls_out_gl_detl_hdr-cpudt.

* User ID
  CONCATENATE '"' is_bkpf-usnam '/'
                  is_bkpf-ppnam '"'    INTO ls_out_gl_detl_hdr-userid.

* Transaction Code
  CONCATENATE '"' is_bkpf-tcode '"'    INTO ls_out_gl_detl_hdr-tcode.

* Amount Currency (Document Currency)
  CONCATENATE '"' is_bkpf-waers '"'    INTO ls_out_gl_detl_hdr-waers.

*eject
* Last Modified Date
  MOVE            is_bkpf-upddt          TO ls_out_gl_detl_hdr-upddt.

* Last Modified By
  MOVE            ls_out_gl_detl_hdr-userid
                                         TO ls_out_gl_detl_hdr-mod_by.

* Approved Date
  MOVE       '""'                        TO ls_out_gl_detl_hdr-appr_dt.

* Approved By
  MOVE       '""'                        TO ls_out_gl_detl_hdr-appr_by.

* Entry Time
  MOVE            is_bkpf-cputm+0(4)     TO ls_out_gl_detl_hdr-cputm.

* Local Amount Currency
  CONCATENATE '"' is_bkpf-hwaer '"'    INTO ls_out_gl_detl_hdr-hwaer.

* Reversal Journal ID
  CONCATENATE '"' is_bkpf-stblg '"'    INTO ls_out_gl_detl_hdr-stblg.

* Client
  CONCATENATE '"' is_bkpf-mandt '"'    INTO ls_out_gl_detl_hdr-mandt.

* Document Status
  CONCATENATE '"' is_bkpf-bstat '"'    INTO ls_out_gl_detl_hdr-bstat.

* Document Type
  CONCATENATE '"' is_bkpf-blart '"'    INTO ls_out_gl_detl_hdr-blart.

* Changed On
  MOVE            is_bkpf-aedat          TO ls_out_gl_detl_hdr-aedat.

* Recurring Entry Document Number
  CONCATENATE '"' is_bkpf-dbblg '"'    INTO ls_out_gl_detl_hdr-dbblg.

* Reverse Document Fiscal Year
  MOVE            is_bkpf-stjah          TO ls_out_gl_detl_hdr-stjah.

* Branch Number
  CONCATENATE '"' is_bkpf-brnch '"'    INTO ls_out_gl_detl_hdr-brnch.

*eject
* Exchange Rate
  IF         ( is_bkpf-kursf           IS INITIAL ).
    MOVE       '0.00000'               TO ls_out_gl_detl_hdr-kursf.
  ELSE.
    CLEAR                                 lv_rate.
    WRITE      is_bkpf-kursf           TO lv_rate
                                 DECIMALS 5.
    TRANSLATE  lv_rate              USING ', '.
    CONDENSE   lv_rate            NO-GAPS.
    MOVE       lv_rate                 TO ls_out_gl_detl_hdr-kursf.
  ENDIF.

* Document Indicator
  CONCATENATE '"' is_bkpf-xrueb '"'    INTO ls_out_gl_detl_hdr-xrueb.

* Document Reversal Indicator
  CONCATENATE '"' is_bkpf-xstov '"'    INTO ls_out_gl_detl_hdr-xstov.

* Source Company Code
  CONCATENATE '"' is_bkpf-ausbk '"'    INTO ls_out_gl_detl_hdr-ausbk.

* Net Document Type
  CONCATENATE '"' is_bkpf-xnetb '"'    INTO ls_out_gl_detl_hdr-xnetb.

* Reference Document Number
  CLEAR                                     lv_string.
  MOVE     is_bkpf-xblnr                 TO lv_string.
  PERFORM  f_string_remove_spcl_char  USING p_fdlm1
                                   CHANGING lv_string.
  CONCATENATE '"' lv_string '"'        INTO ls_out_gl_detl_hdr-xblnr.

* Local Amount 2nd Currency
  CONCATENATE '"' is_bkpf-hwae2 '"'    INTO ls_out_gl_detl_hdr-hwae2.

* Document Date
  MOVE            is_bkpf-bldat          TO ls_out_gl_detl_hdr-bldat.

* Translation Date
  MOVE            is_bkpf-wwert          TO ls_out_gl_detl_hdr-wwert.

* Batch Input Session Name
  CONCATENATE '"' is_bkpf-grpid '"'    INTO ls_out_gl_detl_hdr-grpid.

* Cross-Company Posting
  CONCATENATE '"' is_bkpf-bvorg '"'    INTO ls_out_gl_detl_hdr-bvorg.

*eject
* Acounting Document Number
  CONCATENATE '"' is_bkpf-belnr '"'    INTO ls_out_gl_detl_hdr-belnr.

* Business Transaction
  CONCATENATE '"' is_bkpf-glvor '"'    INTO ls_out_gl_detl_hdr-glvor.

* Reference Transaction
  CONCATENATE '"' is_bkpf-awtyp '"'    INTO ls_out_gl_detl_hdr-awtyp.

* User Name
  CONCATENATE '"' is_bkpf-usnam '"'    INTO ls_out_gl_detl_hdr-usnam.

* Parked By
  CONCATENATE '"' is_bkpf-ppnam '"'    INTO ls_out_gl_detl_hdr-ppnam.

*eject
  IF       ( rb_g2fm1                    IS NOT INITIAL ).

    PERFORM  f_output_gl_detail_hdr   USING ls_out_gl_detl_hdr.

  ELSEIF   ( rb_g2fm2                    IS NOT INITIAL ).

    IF     ( it_bseg[] IS INITIAL ).

      MOVE     '000'                     TO ls_out_gl_detl_hdr-buzei.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-sgtxt.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-xauto.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-lifnr.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-ebeln.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-bschl.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-hkont.
      MOVE     '0.00'                    TO ls_out_gl_detl_hdr-dmbtr.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-shkzg.
      MOVE     '0.00'                    TO ls_out_gl_detl_hdr-wrbtr.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-rpt_curr.
      MOVE     '0.00'                    TO ls_out_gl_detl_hdr-pswbt.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-xnegp.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-buzid.
      MOVE     '00000000'                TO ls_out_gl_detl_hdr-augdt.
      MOVE     '00000000'                TO ls_out_gl_detl_hdr-augcp.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-augbl.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-koart.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-bewar.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-altkt.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-vorgn.
      MOVE     '0.00'                    TO ls_out_gl_detl_hdr-dmbe2.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-kostl.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-saknr.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-anfbu.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-ktosl.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-vbeln.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-prctr.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-pswsl.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-kokrs.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-gsber.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-vname.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-umskz.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-zuonr.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-kunnr.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-matnr.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-werks.
      MOVE     '0.000'                   TO ls_out_gl_detl_hdr-menge.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-meins.
      MOVE     '""'                      TO ls_out_gl_detl_hdr-anbwa.

      PERFORM  f_output_gl_detail_trn USING ls_out_gl_detl_hdr.

      RETURN.
    ENDIF.

  ENDIF.

*eject
  LOOP AT  it_bseg                ASSIGNING <fs_bseg>.

    CLEAR                                   ls_out_gl_detl.
    MOVE     ls_out_gl_detl_hdr          TO ls_out_gl_detl.

* Journal ID Line Number
    MOVE            <fs_bseg>-buzei      TO ls_out_gl_detl-buzei.

* Line Description
    CLEAR                                   lv_string.
    MOVE     <fs_bseg>-sgtxt             TO lv_string.
    PERFORM  f_string_remove_spcl_char
                                      USING p_fdlm1
                                   CHANGING lv_string.
    CONCATENATE '"' lv_string '"'      INTO ls_out_gl_detl-sgtxt.

* Auto Created
    CONCATENATE '"' <fs_bseg>-xauto '"'
                                       INTO ls_out_gl_detl-xauto.

* Vendor
    CONCATENATE '"' <fs_bseg>-lifnr '"'
                                       INTO ls_out_gl_detl-lifnr.

* Purchasing Document
    CONCATENATE '"' <fs_bseg>-ebeln '"'
                                       INTO ls_out_gl_detl-ebeln.

* Posting Key
    CONCATENATE '"' <fs_bseg>-bschl '"'
                                       INTO ls_out_gl_detl-bschl.

* G/L Account Number
    CONCATENATE '"' <fs_bseg>-hkont '"'
                                       INTO ls_out_gl_detl-hkont.

* Amount in Local Currency
    IF         ( <fs_bseg>-dmbtr         IS INITIAL ).
      MOVE       '0.00'                  TO ls_out_gl_detl-dmbtr.
    ELSE.
      CLEAR                                 lv_amount.
      WRITE      <fs_bseg>-dmbtr         TO lv_amount
                                   CURRENCY gs_t001-waers.
      TRANSLATE  lv_amount            USING ', '.
      CONDENSE   lv_amount          NO-GAPS.
      MOVE       lv_amount               TO ls_out_gl_detl-dmbtr.
    ENDIF.

*eject
* Amount Credit Debit Indicator
    CONCATENATE '"' <fs_bseg>-shkzg '"'
                                       INTO ls_out_gl_detl-shkzg.

* Reporting Amount in Document Currency
    IF         ( <fs_bseg>-wrbtr         IS INITIAL ).
      MOVE       '0.00'                  TO ls_out_gl_detl-wrbtr.
    ELSE.
      CLEAR                                 lv_amount.
      WRITE      <fs_bseg>-wrbtr         TO lv_amount
                                   CURRENCY is_bkpf-waers.
      TRANSLATE  lv_amount            USING ', '.
      CONDENSE   lv_amount          NO-GAPS.
      MOVE       lv_amount               TO ls_out_gl_detl-wrbtr.
    ENDIF.

* Reporting Amount Document Currency
    MOVE        '""'                     TO ls_out_gl_detl-rpt_curr.

* Local Amount
    IF         ( <fs_bseg>-pswbt         IS INITIAL ).
      MOVE       '0.00'                  TO ls_out_gl_detl-pswbt.
    ELSE.
      CLEAR                                 lv_amount.
      WRITE      <fs_bseg>-pswbt         TO lv_amount
                                   CURRENCY <fs_bseg>-pswsl.
      TRANSLATE  lv_amount            USING ', '.
      CONDENSE   lv_amount          NO-GAPS.
      MOVE       lv_amount               TO ls_out_gl_detl-pswbt.
    ENDIF.

* Reversal Indicator
    CONCATENATE '"' <fs_bseg>-xnegp '"'
                                       INTO ls_out_gl_detl-xnegp.

* Line Item Identifier
    CONCATENATE '"' <fs_bseg>-buzid '"'
                                       INTO ls_out_gl_detl-buzid.

* Clearing Date
    MOVE            <fs_bseg>-augdt      TO ls_out_gl_detl-augdt.

* Clearing Entry Date
    MOVE            <fs_bseg>-augcp      TO ls_out_gl_detl-augcp.

* Clearing Document
    CONCATENATE '"' <fs_bseg>-augbl '"'
                                       INTO ls_out_gl_detl-augbl.

*eject
* Account Type
    CONCATENATE '"' <fs_bseg>-koart '"'
                                       INTO ls_out_gl_detl-koart.

* Transaction Type
    CONCATENATE '"' <fs_bseg>-bewar '"'
                                       INTO ls_out_gl_detl-bewar.

* Group Account Number
    CONCATENATE '"' <fs_bseg>-altkt '"'
                                       INTO ls_out_gl_detl-altkt.

* G/L Transaction Type
    CONCATENATE '"' <fs_bseg>-vorgn '"'
                                       INTO ls_out_gl_detl-vorgn.

* Amount in 2nd Local Currency
    IF         ( <fs_bseg>-dmbe2         IS INITIAL ).
      MOVE       '0.00'                  TO ls_out_gl_detl-dmbe2.
    ELSE.
      CLEAR                                 lv_amount.
      WRITE      <fs_bseg>-dmbe2         TO lv_amount
                                   CURRENCY is_bkpf-hwae2.
      TRANSLATE  lv_amount            USING ', '.
      CONDENSE   lv_amount          NO-GAPS.
      MOVE       lv_amount               TO ls_out_gl_detl-dmbe2.
    ENDIF.

* Cost Center
    CONCATENATE '"' <fs_bseg>-kostl '"'
                                       INTO ls_out_gl_detl-kostl.

* G/L Account Number
    CONCATENATE '"' <fs_bseg>-saknr '"'
                                       INTO ls_out_gl_detl-saknr.

* Bill-Of-Exchange Company Code
    CONCATENATE '"' <fs_bseg>-anfbu '"'
                                       INTO ls_out_gl_detl-anfbu.

* Transaction
    CONCATENATE '"' <fs_bseg>-ktosl '"'
                                       INTO ls_out_gl_detl-ktosl.

* Billing Document
    CONCATENATE '"' <fs_bseg>-vbeln '"'
                                       INTO ls_out_gl_detl-vbeln.

*eject
* Profit Center
    CONCATENATE '"' <fs_bseg>-prctr '"'
                                       INTO ls_out_gl_detl-prctr.

* G/L Currency
    CONCATENATE '"' <fs_bseg>-pswsl '"'
                                       INTO ls_out_gl_detl-pswsl.

* Controlling Area
    CONCATENATE '"' <fs_bseg>-kokrs '"'
                                       INTO ls_out_gl_detl-kokrs.

* Business Area
    CONCATENATE '"' <fs_bseg>-gsber '"'
                                       INTO ls_out_gl_detl-gsber.

* Joint Venture
    CONCATENATE '"' <fs_bseg>-vname '"'
                                       INTO ls_out_gl_detl-vname.

* Special G/L Indicator
    CONCATENATE '"' <fs_bseg>-umskz '"'
                                       INTO ls_out_gl_detl-umskz.

* Assignment Number
    CLEAR                                   lv_string.
    MOVE     <fs_bseg>-zuonr             TO lv_string.
    PERFORM  f_string_remove_spcl_char
                                      USING p_fdlm1
                                   CHANGING lv_string.
    CONCATENATE '"' lv_string '"'      INTO ls_out_gl_detl-zuonr.

* Customer Number
    CONCATENATE '"' <fs_bseg>-kunnr '"'
                                       INTO ls_out_gl_detl-kunnr.

* Material Number
    CONCATENATE '"' <fs_bseg>-matnr '"'
                                       INTO ls_out_gl_detl-matnr.

* Plant
    CONCATENATE '"' <fs_bseg>-werks '"'
                                       INTO ls_out_gl_detl-werks.

*eject
* Quantity
    IF         ( <fs_bseg>-menge         IS INITIAL ).
      MOVE       '0.000'                 TO ls_out_gl_detl-menge.
    ELSE.
      CLEAR                                 lv_quantity.
      WRITE      <fs_bseg>-menge         TO lv_quantity
                                       UNIT <fs_bseg>-meins.
      TRANSLATE  lv_quantity          USING ', '.
      CONDENSE   lv_quantity        NO-GAPS.
      MOVE       lv_quantity             TO ls_out_gl_detl-menge.
    ENDIF.

* Base Unit-Of-Measure
    CONCATENATE '"' <fs_bseg>-meins '"'
                                       INTO ls_out_gl_detl-meins.

* Asset Transaction Type
    CONCATENATE '"' <fs_bseg>-anbwa '"'
                                       INTO ls_out_gl_detl-anbwa.

    IF       ( rb_g2fm1                  IS NOT INITIAL ).

      PERFORM  f_output_gl_detail_itm USING ls_out_gl_detl.

    ELSEIF   ( rb_g2fm2                  IS NOT INITIAL ).

      PERFORM  f_output_gl_detail_trn USING ls_out_gl_detl.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_format_gl_detail_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_output_gl_detail_hdr
*&---------------------------------------------------------------------*
*       Output the G/L detail data - Header (BKPF)
*----------------------------------------------------------------------*
FORM f_output_gl_detail_hdr
  USING    is_out_gl_detl              TYPE ty_wa_out_gl_detl.

  DATA:    lv_string                   TYPE STRING,
           lv_strlen                   TYPE syindex.

  CLEAR                                     lv_string.
  CONCATENATE                is_out_gl_detl-mandt
                             is_out_gl_detl-bukrs
                             is_out_gl_detl-monat
                             is_out_gl_detl-gjahr
                             is_out_gl_detl-belnr
                             is_out_gl_detl-bktxt
                             is_out_gl_detl-tcode
                             is_out_gl_detl-glvor
                             is_out_gl_detl-awtyp
                             is_out_gl_detl-budat
                             is_out_gl_detl-cpudt
                             is_out_gl_detl-usnam
                             is_out_gl_detl-waers
                             is_out_gl_detl-upddt
                             is_out_gl_detl-ppnam
                             is_out_gl_detl-cputm
                             is_out_gl_detl-hwaer
                             is_out_gl_detl-stblg
                             is_out_gl_detl-bstat
                             is_out_gl_detl-blart
                             is_out_gl_detl-aedat
                             is_out_gl_detl-dbblg
                             is_out_gl_detl-stjah
                             is_out_gl_detl-brnch
                             is_out_gl_detl-kursf
                             is_out_gl_detl-xrueb
                             is_out_gl_detl-xstov
                             is_out_gl_detl-ausbk
                             is_out_gl_detl-xnetb
                             is_out_gl_detl-xblnr
                             is_out_gl_detl-hwae2
                             is_out_gl_detl-bldat
                             is_out_gl_detl-wwert
                             is_out_gl_detl-grpid
                             is_out_gl_detl-bvorg
                                       INTO lv_string
                               SEPARATED BY gv_fdlm1.

  TRANSFER lv_string                     TO gv_file_name_hdr.

ENDFORM.                    " f_output_gl_detail_hdr
*eject
*&---------------------------------------------------------------------*
*&      Form  f_output_gl_detail_itm
*&---------------------------------------------------------------------*
*       Output the G/L detail data - Item (BSEG)
*----------------------------------------------------------------------*
FORM f_output_gl_detail_itm
  USING    is_out_gl_detl              TYPE ty_wa_out_gl_detl.

  DATA:    lv_string                   TYPE STRING,
           lv_strlen                   TYPE syindex.

  CLEAR                                     lv_string.
  CONCATENATE                is_out_gl_detl-mandt
                             is_out_gl_detl-bukrs
                             is_out_gl_detl-belnr
                             is_out_gl_detl-buzei
                             is_out_gl_detl-sgtxt
                             is_out_gl_detl-gjahr
                             is_out_gl_detl-hkont
                             is_out_gl_detl-dmbtr
                             is_out_gl_detl-shkzg
                             is_out_gl_detl-wrbtr
                             is_out_gl_detl-pswbt
                             is_out_gl_detl-xnegp
                             is_out_gl_detl-buzid
                             is_out_gl_detl-xauto
                             is_out_gl_detl-ebeln
                             is_out_gl_detl-augdt
                             is_out_gl_detl-augcp
                             is_out_gl_detl-augbl
                             is_out_gl_detl-bschl
                             is_out_gl_detl-koart
                             is_out_gl_detl-bewar
                             is_out_gl_detl-altkt
                             is_out_gl_detl-vorgn
                             is_out_gl_detl-dmbe2
                             is_out_gl_detl-kostl
                             is_out_gl_detl-saknr
                             is_out_gl_detl-anfbu
                             is_out_gl_detl-ktosl
                             is_out_gl_detl-vbeln
                             is_out_gl_detl-prctr
                             is_out_gl_detl-pswsl
                             is_out_gl_detl-kokrs
                             is_out_gl_detl-gsber
                             is_out_gl_detl-vname
                             is_out_gl_detl-umskz
                             is_out_gl_detl-zuonr
                             is_out_gl_detl-kunnr
                             is_out_gl_detl-lifnr
                             is_out_gl_detl-matnr
                             is_out_gl_detl-werks
                             is_out_gl_detl-menge
                             is_out_gl_detl-meins
                             is_out_gl_detl-anbwa
                                       INTO lv_string
                               SEPARATED BY gv_fdlm1.
*eject
  lv_strlen = STRLEN( lv_string ).

  ADD      lv_strlen                     TO gv_file_size.

  TRANSFER lv_string                     TO gv_file_name_itm.

ENDFORM.                    " f_output_gl_detail_itm
*eject
*&---------------------------------------------------------------------*
*&      Form  f_output_gl_detail_trn
*&---------------------------------------------------------------------*
*       Output the G/L detail data
*----------------------------------------------------------------------*
FORM f_output_gl_detail_trn
  USING    is_out_gl_detl              TYPE ty_wa_out_gl_detl.

  DATA:    lv_string                   TYPE STRING,
           lv_strlen                   TYPE syindex.

  CLEAR                                     lv_string.
  CONCATENATE                is_out_gl_detl-journal_id
                             is_out_gl_detl-buzei
                             is_out_gl_detl-bktxt
                             is_out_gl_detl-sgtxt
                             is_out_gl_detl-source
                             is_out_gl_detl-xauto
                             is_out_gl_detl-lifnr
                             is_out_gl_detl-ebeln
                             is_out_gl_detl-bschl
                             is_out_gl_detl-bukrs
                             is_out_gl_detl-gjahr
                             is_out_gl_detl-monat
                             is_out_gl_detl-budat
                             is_out_gl_detl-cpudt
                             is_out_gl_detl-userid
                             is_out_gl_detl-tcode
                             is_out_gl_detl-hkont
                             is_out_gl_detl-dmbtr
                             is_out_gl_detl-shkzg
                             is_out_gl_detl-waers
                             is_out_gl_detl-upddt
                             is_out_gl_detl-mod_by
                             is_out_gl_detl-appr_dt
                             is_out_gl_detl-appr_by
                             is_out_gl_detl-cputm
                             is_out_gl_detl-wrbtr
                             is_out_gl_detl-rpt_curr
                             is_out_gl_detl-pswbt
                             is_out_gl_detl-hwaer
                             is_out_gl_detl-xnegp
                             is_out_gl_detl-stblg
                             is_out_gl_detl-mandt
                             is_out_gl_detl-bstat
                             is_out_gl_detl-blart
                             is_out_gl_detl-aedat
                             is_out_gl_detl-dbblg
                             is_out_gl_detl-stjah
                             is_out_gl_detl-brnch
                             is_out_gl_detl-kursf
                             is_out_gl_detl-xrueb
                             is_out_gl_detl-xstov

*eject
                             is_out_gl_detl-ausbk
                             is_out_gl_detl-xnetb
                             is_out_gl_detl-xblnr
                             is_out_gl_detl-hwae2
                             is_out_gl_detl-bldat
                             is_out_gl_detl-wwert
                             is_out_gl_detl-grpid
                             is_out_gl_detl-bvorg
                             is_out_gl_detl-buzid
                             is_out_gl_detl-augdt
                             is_out_gl_detl-augcp
                             is_out_gl_detl-augbl
                             is_out_gl_detl-koart
                             is_out_gl_detl-bewar
                             is_out_gl_detl-altkt
                             is_out_gl_detl-vorgn
                             is_out_gl_detl-dmbe2
                             is_out_gl_detl-kostl
                             is_out_gl_detl-saknr
                             is_out_gl_detl-anfbu
                             is_out_gl_detl-ktosl
                             is_out_gl_detl-vbeln
                             is_out_gl_detl-prctr
                             is_out_gl_detl-pswsl
                             is_out_gl_detl-kokrs
                             is_out_gl_detl-gsber
                             is_out_gl_detl-vname
                             is_out_gl_detl-umskz
                             is_out_gl_detl-zuonr
                             is_out_gl_detl-kunnr
                             is_out_gl_detl-matnr
                             is_out_gl_detl-werks
                             is_out_gl_detl-menge
                             is_out_gl_detl-meins
                             is_out_gl_detl-anbwa
                                       INTO lv_string
                               SEPARATED BY gv_fdlm1.

  lv_strlen = STRLEN( lv_string ).

  ADD      lv_strlen                     TO gv_file_size.

  TRANSFER lv_string                     TO gv_file_name_trn.

ENDFORM.                    " f_output_gl_detail_trn
*eject
*&---------------------------------------------------------------------*
*&      Form  f_string_remove_spcl_char
*&---------------------------------------------------------------------*
*       Remove special characters from string
*----------------------------------------------------------------------*
FORM f_string_remove_spcl_char
  USING    iv_delm                     TYPE char1
  CHANGING cv_string                   TYPE STRING.

  DATA:    lv_i                        TYPE syindex,
           lv_j                        TYPE syindex,
           lv_char                     TYPE char1,
           lv_string                   TYPE text256.

  IF ( cv_string IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_string.
  MOVE     cv_string                     TO lv_string.

  lv_i   = STRLEN( lv_string ).

  DO       lv_i TIMES.
    lv_j = sy-index - 1.

    CLEAR                                   lv_char.
    MOVE     lv_string+lv_j(1)           TO lv_char.

    IF   ( ( lv_char                     LT ' '     ) OR
           ( lv_char                     GT '~'     ) OR
           ( lv_char                     EQ iv_delm )    ).
      MOVE   SPACE                       TO lv_string+lv_j(1).
    ENDIF.

  ENDDO.

  CLEAR                                     cv_string.
  MOVE     lv_string                     TO cv_string.

ENDFORM.                    " f_string_remove_spcl_char
*eject
*&---------------------------------------------------------------------*
*&      Form  f_file_break
*&---------------------------------------------------------------------*
*       File break
*----------------------------------------------------------------------*
FORM f_file_break
  USING    iv_file_name                TYPE text256
  CHANGING cv_file_name_hdr            TYPE text256
           cv_file_name_itm            TYPE text256
           cv_file_name_trn            TYPE text256.

  DATA:    lv_file_name                TYPE text256,
           lv_file_name_hdr            TYPE text256,
           lv_file_name_itm            TYPE text256,
           lv_file_name_trn            TYPE text256,
           lv_doc_count                TYPE numc10.

  IF     ( cv_file_name_hdr              IS NOT INITIAL ).

    CLOSE    DATASET cv_file_name_hdr.

  ENDIF.

  IF     ( cv_file_name_itm              IS NOT INITIAL ).

    CLOSE    DATASET cv_file_name_itm.

  ENDIF.

  IF     ( cv_file_name_trn              IS NOT INITIAL ).

    CLOSE    DATASET cv_file_name_trn.

  ENDIF.

  IF   ( ( cv_file_name_hdr              IS NOT INITIAL ) OR
         ( cv_file_name_trn              IS NOT INITIAL )    ).

    lv_doc_count = gv_doc_count.

    MESSAGE  s000 WITH text-m01 lv_doc_count.
    WRITE: / text-m01, lv_doc_count.

  ENDIF.

  CLEAR    cv_file_name_hdr.
  CLEAR    cv_file_name_itm.
  CLEAR    cv_file_name_trn.

  IF     ( iv_file_name                  IS INITIAL ).
    RETURN.
  ENDIF.

*eject
  CLEAR    lv_file_name.
  CLEAR    lv_file_name_hdr.
  CLEAR    lv_file_name_itm.
  CLEAR    lv_file_name_trn.

  MOVE     iv_file_name                  TO lv_file_name.

  ADD      1                             TO gv_file_seqn.

  IF     ( iv_file_name                  CS '###'      ).
    REPLACE  '###'                       IN lv_file_name
                                       WITH gv_file_seqn.
  ENDIF.
  IF     ( iv_file_name                  CS 'YYYYMMDD' ).
    REPLACE  'YYYYMMDD'                  IN lv_file_name
                                       WITH gv_datum_run.
  ENDIF.
  IF     ( iv_file_name                  CS 'HHMMSS'   ).
    REPLACE  'HHMMSS'                    IN lv_file_name
                                       WITH gv_uzeit_run.
  ENDIF.

*eject
  IF     ( rb_g2fm1                      IS NOT INITIAL ).

    CONCATENATE                             lv_file_name '_hdr.'
                                            p_fext1
                                       INTO lv_file_name_hdr.

    OPEN     DATASET lv_file_name_hdr   FOR OUTPUT
                                         IN TEXT MODE
                                   ENCODING DEFAULT.
    IF     ( sy-subrc EQ 0 ).
      MOVE   lv_file_name_hdr            TO cv_file_name_hdr.
    ENDIF.

    CONCATENATE                             lv_file_name '_itm.'
                                            p_fext1
                                       INTO lv_file_name_itm.

    OPEN     DATASET lv_file_name_itm   FOR OUTPUT
                                         IN TEXT MODE
                                   ENCODING DEFAULT.
    IF     ( sy-subrc EQ 0 ).
      MOVE   lv_file_name_itm            TO cv_file_name_itm.
    ENDIF.

  ENDIF.

  IF     ( rb_g2fm2                      IS NOT INITIAL ).

    CONCATENATE                             lv_file_name '_trn.'
                                            p_fext1
                                       INTO lv_file_name_trn.

    OPEN     DATASET lv_file_name_trn   FOR OUTPUT
                                         IN TEXT MODE
                                   ENCODING DEFAULT.
    IF     ( sy-subrc EQ 0 ).
      MOVE   lv_file_name_trn            TO cv_file_name_trn.
    ENDIF.

  ENDIF.

  IF     ( p_chdr1                       IS NOT INITIAL ).
    PERFORM  f_output_gl_detail_col.
  ENDIF.

ENDFORM.                    " f_file_break
*eject
*&---------------------------------------------------------------------*
*&      Form  f_output_gl_detail_col
*&---------------------------------------------------------------------*
*       Format the G/L detail column headings
*----------------------------------------------------------------------*
FORM f_output_gl_detail_col.

  DATA:    lv_string                   TYPE STRING,
           lv_strlen                   TYPE syindex.

  IF       ( rb_g2fm1                    IS NOT INITIAL ).

    CLEAR                                   lv_string.
    CONCATENATE    text-c32 "is_out_gl_detl-mandt
                   text-c10 "is_out_gl_detl-bukrs
                   text-c12 "is_out_gl_detl-monat
                   text-c11 "is_out_gl_detl-gjahr
                   text-c91 "is_out_gl_detl-belnr
                   text-c03 "is_out_gl_detl-bktxt
                   text-c16 "is_out_gl_detl-tcode
                   text-c92 "is_out_gl_detl-glvor
                   text-c93 "is_out_gl_detl-awtyp
                   text-c13 "is_out_gl_detl-budat
                   text-c14 "is_out_gl_detl-cpudt
                   text-c94 "is_out_gl_detl-usnam
                   text-c20 "is_out_gl_detl-waers
                   text-c21 "is_out_gl_detl-upddt
                   text-c95 "is_out_gl_detl-ppnam
                   text-c25 "is_out_gl_detl-cputm
                   text-c29 "is_out_gl_detl-hwaer
                   text-c31 "is_out_gl_detl-stblg
                   text-c33 "is_out_gl_detl-bstat
                   text-c34 "is_out_gl_detl-blart
                   text-c35 "is_out_gl_detl-aedat
                   text-c36 "is_out_gl_detl-dbblg
                   text-c37 "is_out_gl_detl-stjah
                   text-c38 "is_out_gl_detl-brnch
                   text-c39 "is_out_gl_detl-kursf
                   text-c40 "is_out_gl_detl-xrueb
                   text-c41 "is_out_gl_detl-xstov
                   text-c42 "is_out_gl_detl-ausbk
                   text-c43 "is_out_gl_detl-xnetb
                   text-c44 "is_out_gl_detl-xblnr
                   text-c45 "is_out_gl_detl-hwae2
                   text-c46 "is_out_gl_detl-bldat
                   text-c47 "is_out_gl_detl-wwert
                   text-c48 "is_out_gl_detl-grpid
                   text-c49 "is_out_gl_detl-bvorg
                                       INTO lv_string
                               SEPARATED BY gv_fdlm1.

    TRANSFER lv_string                   TO gv_file_name_hdr.

*eject
    CLEAR                                   lv_string.
    CONCATENATE    text-c32 "is_out_gl_detl-mandt
                   text-c10 "is_out_gl_detl-bukrs
                   text-c91 "is_out_gl_detl-belnr
                   text-c02 "is_out_gl_detl-buzei
                   text-c04 "is_out_gl_detl-sgtxt
                   text-c11 "is_out_gl_detl-gjahr
                   text-c17 "is_out_gl_detl-hkont
                   text-c18 "is_out_gl_detl-dmbtr
                   text-c19 "is_out_gl_detl-shkzg
                   text-c26 "is_out_gl_detl-wrbtr
                   text-c28 "is_out_gl_detl-pswbt
                   text-c30 "is_out_gl_detl-xnegp
                   text-c50 "is_out_gl_detl-buzid
                   text-c06 "is_out_gl_detl-xauto
                   text-c08 "is_out_gl_detl-ebeln
                   text-c51 "is_out_gl_detl-augdt
                   text-c52 "is_out_gl_detl-augcp
                   text-c53 "is_out_gl_detl-augbl
                   text-c09 "is_out_gl_detl-bschl
                   text-c54 "is_out_gl_detl-koart
                   text-c55 "is_out_gl_detl-bewar
                   text-c56 "is_out_gl_detl-altkt
                   text-c57 "is_out_gl_detl-vorgn
                   text-c58 "is_out_gl_detl-dmbe2
                   text-c59 "is_out_gl_detl-kostl
                   text-c60 "is_out_gl_detl-saknr
                   text-c61 "is_out_gl_detl-anfbu
                   text-c62 "is_out_gl_detl-ktosl
                   text-c63 "is_out_gl_detl-vbeln
                   text-c64 "is_out_gl_detl-prctr
                   text-c65 "is_out_gl_detl-pswsl
                   text-c66 "is_out_gl_detl-kokrs
                   text-c67 "is_out_gl_detl-gsber
                   text-c68 "is_out_gl_detl-vname
                   text-c69 "is_out_gl_detl-umskz
                   text-c70 "is_out_gl_detl-zuonr
                   text-c71 "is_out_gl_detl-kunnr
                   text-c07 "is_out_gl_detl-lifnr
                   text-c72 "is_out_gl_detl-matnr
                   text-c73 "is_out_gl_detl-werks
                   text-c74 "is_out_gl_detl-menge
                   text-c75 "is_out_gl_detl-meins
                   text-c76 "is_out_gl_detl-anbwa
                                       INTO lv_string
                               SEPARATED BY gv_fdlm1.

    lv_strlen = STRLEN( lv_string ).

    ADD      lv_strlen                   TO gv_file_size.

    TRANSFER lv_string                   TO gv_file_name_itm.

*eject
  ELSEIF   ( rb_g2fm2                    IS NOT INITIAL ).

    CLEAR                                   lv_string.
    CONCATENATE    text-c01 "is_out_gl_detl-journal_id
                   text-c02 "is_out_gl_detl-buzei
                   text-c03 "is_out_gl_detl-bktxt
                   text-c04 "is_out_gl_detl-sgtxt
                   text-c05 "is_out_gl_detl-source
                   text-c06 "is_out_gl_detl-xauto
                   text-c07 "is_out_gl_detl-lifnr
                   text-c08 "is_out_gl_detl-ebeln
                   text-c09 "is_out_gl_detl-bschl
                   text-c10 "is_out_gl_detl-bukrs
                   text-c11 "is_out_gl_detl-gjahr
                   text-c12 "is_out_gl_detl-monat
                   text-c13 "is_out_gl_detl-budat
                   text-c14 "is_out_gl_detl-cpudt
                   text-c15 "is_out_gl_detl-userid
                   text-c16 "is_out_gl_detl-tcode
                   text-c17 "is_out_gl_detl-hkont
                   text-c18 "is_out_gl_detl-dmbtr
                   text-c19 "is_out_gl_detl-shkzg
                   text-c20 "is_out_gl_detl-waers
                   text-c21 "is_out_gl_detl-upddt
                   text-c22 "is_out_gl_detl-mod_by
                   text-c23 "is_out_gl_detl-appr_dt
                   text-c24 "is_out_gl_detl-appr_by
                   text-c25 "is_out_gl_detl-cputm
                   text-c26 "is_out_gl_detl-wrbtr
                   text-c27 "is_out_gl_detl-rpt_curr
                   text-c28 "is_out_gl_detl-pswbt
                   text-c29 "is_out_gl_detl-hwaer
                   text-c30 "is_out_gl_detl-xnegp
                   text-c31 "is_out_gl_detl-stblg
                   text-c32 "is_out_gl_detl-mandt
                   text-c33 "is_out_gl_detl-bstat
                   text-c34 "is_out_gl_detl-blart
                   text-c35 "is_out_gl_detl-aedat
                   text-c36 "is_out_gl_detl-dbblg
                   text-c37 "is_out_gl_detl-stjah
                   text-c38 "is_out_gl_detl-brnch
                   text-c39 "is_out_gl_detl-kursf
                   text-c40 "is_out_gl_detl-xrueb
                   text-c41 "is_out_gl_detl-xstov

*eject
                   text-c42 "is_out_gl_detl-ausbk
                   text-c43 "is_out_gl_detl-xnetb
                   text-c44 "is_out_gl_detl-xblnr
                   text-c45 "is_out_gl_detl-hwae2
                   text-c46 "is_out_gl_detl-bldat
                   text-c47 "is_out_gl_detl-wwert
                   text-c48 "is_out_gl_detl-grpid
                   text-c49 "is_out_gl_detl-bvorg
                   text-c50 "is_out_gl_detl-buzid
                   text-c51 "is_out_gl_detl-augdt
                   text-c52 "is_out_gl_detl-augcp
                   text-c53 "is_out_gl_detl-augbl
                   text-c54 "is_out_gl_detl-koart
                   text-c55 "is_out_gl_detl-bewar
                   text-c56 "is_out_gl_detl-altkt
                   text-c57 "is_out_gl_detl-vorgn
                   text-c58 "is_out_gl_detl-dmbe2
                   text-c59 "is_out_gl_detl-kostl
                   text-c60 "is_out_gl_detl-saknr
                   text-c61 "is_out_gl_detl-anfbu
                   text-c62 "is_out_gl_detl-ktosl
                   text-c63 "is_out_gl_detl-vbeln
                   text-c64 "is_out_gl_detl-prctr
                   text-c65 "is_out_gl_detl-pswsl
                   text-c66 "is_out_gl_detl-kokrs
                   text-c67 "is_out_gl_detl-gsber
                   text-c68 "is_out_gl_detl-vname
                   text-c69 "is_out_gl_detl-umskz
                   text-c70 "is_out_gl_detl-zuonr
                   text-c71 "is_out_gl_detl-kunnr
                   text-c72 "is_out_gl_detl-matnr
                   text-c73 "is_out_gl_detl-werks
                   text-c74 "is_out_gl_detl-menge
                   text-c75 "is_out_gl_detl-meins
                   text-c76 "is_out_gl_detl-anbwa
                                       INTO lv_string
                               SEPARATED BY gv_fdlm1.

    lv_strlen = STRLEN( lv_string ).

    ADD      lv_strlen                   TO gv_file_size.

    TRANSFER lv_string                   TO gv_file_name_trn.

  ENDIF.

ENDFORM.                    " f_output_gl_detail_col
