*&---------------------------------------------------------------------*
*&  Include           ZFAPI129_INVOICE_WF_ITEMS_F02
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI129_INVOICE_WF_ITEMS                      *
* Include Program    :  ZFAPI129_INVOICE_WF_ITEMS_F02                  *
* Author             :  Vijay Rajaputra                                *
* Creation Date      :  05-Nov-2018                                    *
* Application Area   :  FICO                                           *
* Technical Contact  :  Vijay Rajaputra                                *
*                                                                      *
* Purpose            :  Subroutines Include Program                    *
*                        - Invoice Extract Coding                      *
*                                                                      *
*----------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 05-Nov-2018  VRAJAPUTRA  D30K929291  CHG0130803  Initial development *
*                          D30K929374                                  *
* 22-Jan-2019  VRAJAPUTRA  D30K929505  CHG0135339  Duplicate checks    *
*                          D30K929531                                  *
* 31-Jan-2019  VRAJAPUTRA  D30K929554  CHG0136094  Duplicate checks    *
* 13-Feb-2019  VRAJAPUTRA  D30K929576  CHG0137162  Additions to Extract*
* 16-Apr-2019  KBANERJEE   D30K929770  ENHC0025317-Change IAP logic to *
*                                      include deleted parked invoices *
*                                      in extract-both for PO and non-PO
*&---------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_parked_invoices
*&---------------------------------------------------------------------*
*       Process parked invoices
*----------------------------------------------------------------------*
FORM f_process_parked_invoices.

  DATA:    lt_lfa1_key                 TYPE gtt_lfa1_key,
           ls_lfa1_key                 TYPE gty_lfa1_key,
           lt_lfa1                     TYPE gtt_lfa1_key,
           ls_lfa1                     TYPE gty_lfa1_key,
           lt_vbsegk_key               TYPE gtt_vbsegk_key,
           lt_vbsegk_key_p             TYPE gtt_vbsegk_key,
           lt_vbsegk                   TYPE gtt_vbsegk,
           ls_vbsegk                   TYPE gty_vbsegk,
           lt_vbkpf                    TYPE gtt_vbkpf,
           ls_vbkpf                    TYPE gty_vbkpf,
           lt_invc_data                TYPE gtt_invc_data,
           ls_invc_data                TYPE gty_invc_data,
           ls_object                   TYPE gty_object.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS:     lc_blocksize          TYPE syindex VALUE 5000.
  FIELD-SYMBOLS: <fs_vbsegk_key>       TYPE gty_vbsegk_key,
                 <fs_vbsegk>           TYPE gty_vbsegk.

  IF     ( cb_prkd IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_lfa1_key[].
  CLEAR    ls_lfa1_key.
  CLEAR    lt_lfa1[].
  CLEAR    ls_lfa1.
  CLEAR    lt_vbsegk_key[].
  CLEAR    lt_vbsegk_key_p[].
  CLEAR    lt_vbsegk[].
  CLEAR    ls_vbsegk.
  CLEAR    lt_vbkpf[].
  CLEAR    ls_vbkpf.
  CLEAR    lt_invc_data[].
  CLEAR    ls_invc_data.

*eject
* Select the parked documents key
  SELECT   ausbk  belnr  gjahr  lifnr
    INTO   TABLE lt_vbsegk_key
    FROM   vbsegk
   WHERE   ausbk IN s_bukrs
     AND   belnr IN s_belnr
     AND   gjahr IN s_gjahr
     AND   lifnr IN s_lifnr.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SORT     lt_vbsegk_key ASCENDING BY lifnr.

* Build the vendor key
  LOOP AT  lt_vbsegk_key          ASSIGNING <fs_vbsegk_key>.

    IF     ( <fs_vbsegk_key>-lifnr       NE ls_lfa1_key-lifnr   ).
      CLEAR                                 ls_lfa1_key.
      MOVE   <fs_vbsegk_key>-lifnr       TO ls_lfa1_key-lifnr.
      APPEND ls_lfa1_key                 TO lt_lfa1_key.
    ENDIF.

  ENDLOOP.

*eject
* Filter the invoice documents by vendor account group
  IF     ( s_ktokk[] IS NOT INITIAL ).

    SORT     lt_lfa1_key       ASCENDING BY lifnr.
    DELETE   ADJACENT DUPLICATES       FROM lt_lfa1_key
                                  COMPARING lifnr.

    SELECT   lifnr
      INTO   TABLE lt_lfa1
      FROM   lfa1 FOR ALL ENTRIES IN lt_lfa1_key
     WHERE   lifnr  = lt_lfa1_key-lifnr
       AND   lifnr IN s_lifnr
       AND   ktokk IN s_ktokk.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    SORT     lt_lfa1           ASCENDING BY lifnr.
    DELETE   ADJACENT DUPLICATES       FROM lt_lfa1
                                  COMPARING lifnr.

    LOOP AT  lt_vbsegk_key        ASSIGNING <fs_vbsegk_key>.
      lv_tabix = sy-tabix.

      IF     ( <fs_vbsegk_key>-lifnr     EQ ls_lfa1-lifnr ).
        CONTINUE.
      ENDIF.

      CLEAR                            ls_lfa1.
      READ     TABLE lt_lfa1      INTO ls_lfa1
                              WITH KEY lifnr = <fs_vbsegk_key>-lifnr
                         BINARY SEARCH.
      IF     ( sy-subrc EQ 0 ).
        CONTINUE.
      ENDIF.

      DELETE   lt_vbsegk_key          INDEX lv_tabix.

    ENDLOOP.

  ENDIF.

  CLEAR    lt_lfa1_key[].
  CLEAR    lt_lfa1[].

*eject
* Process the invoice documents in batches
  SORT     lt_vbsegk_key       ASCENDING BY ausbk belnr gjahr.
  DELETE   ADJACENT DUPLICATES         FROM lt_vbsegk_key
                                  COMPARING ausbk belnr gjahr.

  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch of invoice document keys
    CLEAR           lt_vbsegk_key_p[].
    APPEND LINES OF lt_vbsegk_key
               FROM lv_index_lo
                 TO lv_index_hi
                 TO lt_vbsegk_key_p.

    IF     ( lt_vbsegk_key_p[] IS INITIAL ).
      EXIT.
    ENDIF.

*eject
    CLEAR    lt_vbsegk[].
    CLEAR    lt_vbkpf[].
    CLEAR    lt_invc_data[].

    SELECT   ausbk  belnr  gjahr  bzkey  bukrs  buzei  bschl
             umskz  umsks  shkzg  mwskz  dmbtr  wrbtr  zuonr
             sgtxt  lifnr  zfbdt  zterm  zbd1t  zbd2t  zbd3t
             skfbt  sknto  wskto  zlsch  zlspr  xref3
      INTO   TABLE lt_vbsegk
      FROM   vbsegk FOR ALL ENTRIES IN lt_vbsegk_key_p
     WHERE   ausbk = lt_vbsegk_key_p-ausbk
       AND   belnr = lt_vbsegk_key_p-belnr
       AND   gjahr = lt_vbsegk_key_p-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_vbsegk[].
    ENDIF.

    SELECT   ausbk  bukrs  belnr  gjahr  bstat  blart  bldat
             budat  monat  cpudt  cputm  aedat  usnam  xblnr
             waers  hwaer  kursf  awtyp  awkey
      INTO   TABLE lt_vbkpf
      FROM   vbkpf FOR ALL ENTRIES IN lt_vbsegk_key_p
     WHERE   ausbk = lt_vbsegk_key_p-ausbk
       AND   bukrs = lt_vbsegk_key_p-ausbk
       AND   belnr = lt_vbsegk_key_p-belnr
       AND   gjahr = lt_vbsegk_key_p-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_vbkpf[].
    ENDIF.

    DELETE   lt_vbsegk       WHERE ( ( umskz IS NOT INITIAL ) OR
                                     ( umsks IS NOT INITIAL )    ).

    DELETE   lt_vbkpf        WHERE   ( budat NOT IN s_budat ).

    DELETE   lt_vbkpf        WHERE ( ( cpudt NOT IN s_cpudt ) AND
                                     ( aedat NOT IN s_cpudt )     ).

    SORT     lt_vbsegk    ASCENDING BY ausbk belnr gjahr bzkey.
    SORT     lt_vbkpf     ASCENDING BY ausbk belnr gjahr.

*eject
* Process each individual invoice document
    CLEAR                                   ls_vbsegk.
    READ     TABLE lt_vbsegk           INTO ls_vbsegk INDEX 1.

    LOOP AT    lt_vbsegk          ASSIGNING <fs_vbsegk>.

      IF   ( ( <fs_vbsegk>-belnr         NE ls_vbsegk-belnr ) OR
             ( <fs_vbsegk>-ausbk         NE ls_vbsegk-ausbk ) OR
             ( <fs_vbsegk>-gjahr         NE ls_vbsegk-gjahr )    ).
        CLEAR                               ls_vbsegk.
        MOVE   <fs_vbsegk>               TO ls_vbsegk.

        CLEAR                               ls_invc_data.
        PERFORM  f_summarize_invoice TABLES lt_invc_data
                                      USING 'P'
                                   CHANGING ls_invc_data.

        IF       ( ls_invc_data-bukrs    IS NOT INITIAL ).

          PERFORM  f_process_invoice_workflow
                                      USING ls_invc_data-awtyp
                                            ls_invc_data-instid
                                            ls_invc_data.

        ENDIF.

        CLEAR    lt_invc_data[].
        CLEAR    ls_invc_data.

      ENDIF.

      IF   ( ( <fs_vbsegk>-belnr         NE ls_vbkpf-belnr ) OR
             ( <fs_vbsegk>-ausbk         NE ls_vbkpf-ausbk ) OR
             ( <fs_vbsegk>-gjahr         NE ls_vbkpf-gjahr )    ).
        CLEAR                               ls_vbkpf.
        READ     TABLE lt_vbkpf        INTO ls_vbkpf
                                   WITH KEY ausbk = <fs_vbsegk>-ausbk
                                            belnr = <fs_vbsegk>-belnr
                                            gjahr = <fs_vbsegk>-gjahr
                              BINARY SEARCH.
        IF     ( sy-subrc NE 0 ).
          CLEAR  ls_vbkpf.
        ENDIF.
      ENDIF.

*eject
      CLEAR                                 ls_invc_data.
      MOVE-CORRESPONDING <fs_vbsegk>     TO ls_invc_data.
      MOVE-CORRESPONDING ls_vbkpf        TO ls_invc_data.
      CLEAR                                 ls_invc_data-bukrs.
      MOVE     ls_vbkpf-ausbk            TO ls_invc_data-bukrs.
      APPEND   ls_invc_data              TO lt_invc_data.

    ENDLOOP.

    IF     ( lt_invc_data[] IS NOT INITIAL ).

      CLEAR                                 ls_invc_data.
      PERFORM  f_summarize_invoice   TABLES lt_invc_data
                                      USING 'P'
                                   CHANGING ls_invc_data.

      IF       ( ls_invc_data-bukrs      IS NOT INITIAL ).

        PERFORM  f_process_invoice_workflow
                                      USING ls_invc_data-awtyp
                                            ls_invc_data-instid
                                            ls_invc_data.

        CLEAR    lt_invc_data[].
        CLEAR    ls_invc_data.

      ENDIF.

    ENDIF.

  ENDDO.

ENDFORM.                    " f_process_parked_invoices
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_open_invoices
*&---------------------------------------------------------------------*
*       Process open invoices
*----------------------------------------------------------------------*
FORM f_process_open_invoices.

  DATA:    lt_lfa1_key                 TYPE gtt_lfa1_key,
           ls_lfa1_key                 TYPE gty_lfa1_key,
           lt_lfa1                     TYPE gtt_lfa1_key,
           ls_lfa1                     TYPE gty_lfa1_key,
           lt_bsik_key                 TYPE gtt_bsak_key,
           lt_bsik_key_p               TYPE gtt_bsak_key,
           lt_bseg                     TYPE gtt_bseg,       "D30K929505
           ls_bseg                     TYPE gty_bseg,       "D30K929505
           lt_bkpf                     TYPE gtt_bkpf,
           ls_bkpf                     TYPE gty_bkpf,
           lt_invc_data                TYPE gtt_invc_data,
           ls_invc_data                TYPE gty_invc_data,
           ls_object                   TYPE gty_object.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS:     lc_blocksize          TYPE syindex VALUE 5000.

  FIELD-SYMBOLS: <fs_bsik_key>         TYPE gty_bsak_key,
                 <fs_bseg>             TYPE gty_bseg.       "D30K929505

  IF     ( cb_open IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_lfa1_key[].
  CLEAR    ls_lfa1_key.
  CLEAR    lt_lfa1[].
  CLEAR    ls_lfa1.
  CLEAR    lt_bsik_key[].
  CLEAR    lt_bsik_key_p[].
  CLEAR    lt_bseg[].                                       "D30K929505
  CLEAR    ls_bseg.                                         "D30K929505
  CLEAR    lt_bkpf[].
  CLEAR    ls_bkpf.
  CLEAR    lt_invc_data[].
  CLEAR    ls_invc_data.

*eject
* Select the open documents key
  SELECT   bukrs  lifnr  augdt  augbl  gjahr  belnr
    INTO   TABLE lt_bsik_key
    FROM   bsik
   WHERE   bukrs IN s_bukrs
     AND   lifnr IN s_lifnr
     AND   gjahr IN s_gjahr
     AND   belnr IN s_belnr
     AND   budat IN s_budat.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SORT     lt_bsik_key ASCENDING BY lifnr.

* Build the vendor key
  LOOP AT  lt_bsik_key            ASSIGNING <fs_bsik_key>.

    IF     ( <fs_bsik_key>-lifnr         NE ls_lfa1_key-lifnr   ).
      CLEAR                                 ls_lfa1_key.
      MOVE   <fs_bsik_key>-lifnr         TO ls_lfa1_key-lifnr.
      APPEND ls_lfa1_key                 TO lt_lfa1_key.
    ENDIF.

  ENDLOOP.

*eject
* Filter the invoice documents by vendor account group
  IF     ( s_ktokk[] IS NOT INITIAL ).

    SORT     lt_lfa1_key       ASCENDING BY lifnr.
    DELETE   ADJACENT DUPLICATES       FROM lt_lfa1_key
                                  COMPARING lifnr.

    SELECT   lifnr
      INTO   TABLE lt_lfa1
      FROM   lfa1 FOR ALL ENTRIES IN lt_lfa1_key
     WHERE   lifnr  = lt_lfa1_key-lifnr
       AND   lifnr IN s_lifnr
       AND   ktokk IN s_ktokk.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    SORT     lt_lfa1           ASCENDING BY lifnr.
    DELETE   ADJACENT DUPLICATES       FROM lt_lfa1
                                  COMPARING lifnr.

    LOOP AT  lt_bsik_key          ASSIGNING <fs_bsik_key>.
      lv_tabix = sy-tabix.

      IF     ( <fs_bsik_key>-lifnr       EQ ls_lfa1-lifnr ).
        CONTINUE.
      ENDIF.

      CLEAR                                 ls_lfa1.
      READ     TABLE lt_lfa1           INTO ls_lfa1
                                   WITH KEY lifnr = <fs_bsik_key>-lifnr
                              BINARY SEARCH.
      IF     ( sy-subrc EQ 0 ).
        CONTINUE.
      ENDIF.

      DELETE   lt_bsik_key            INDEX lv_tabix.

    ENDLOOP.

  ENDIF.

  CLEAR    lt_lfa1_key[].
  CLEAR    lt_lfa1[].

*eject
* Process the invoice documents in batches
  SORT     lt_bsik_key         ASCENDING BY bukrs belnr gjahr.
  DELETE   ADJACENT DUPLICATES         FROM lt_bsik_key
                                  COMPARING bukrs belnr gjahr.

  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch of invoice document keys
    CLEAR           lt_bsik_key_p[].
    APPEND LINES OF lt_bsik_key
               FROM lv_index_lo
                 TO lv_index_hi
                 TO lt_bsik_key_p.

    IF     ( lt_bsik_key_p[] IS INITIAL ).
      EXIT.
    ENDIF.

*eject
    CLEAR    lt_bseg[].                                     "D30K929505
    CLEAR    lt_bkpf[].
    CLEAR    lt_invc_data[].

    SELECT   bukrs  belnr  gjahr  buzei  augdt  augbl  bschl """K929505
             koart  umskz  umsks  shkzg  mwskz  dmbtr  wrbtr """K929505
             zuonr  sgtxt  lifnr  zfbdt  zterm  zbd1t  zbd2t """K929505
             zbd3t  skfbt  sknto  wskto  zlsch  zlspr  xref3 """K929505
      INTO   TABLE lt_bseg                                  "D30K929505
      FROM   bseg FOR ALL ENTRIES IN lt_bsik_key_p          "D30K929505
     WHERE   bukrs = lt_bsik_key_p-bukrs                    "D30K929505
       AND   belnr = lt_bsik_key_p-belnr                    "D30K929505
       AND   gjahr = lt_bsik_key_p-gjahr.                   "D30K929505
    IF     ( sy-subrc EQ 0 ).                               "D30K929505
      DELETE lt_bseg WHERE koart <> 'K'.                    "D30K929505
    ELSE.                                                   "D30K929505
      CLEAR  lt_bseg[].                                     "D30K929505
    ENDIF.                                                  "D30K929505

    SELECT   bukrs  belnr  gjahr  blart  bldat  budat  monat  cpudt
             cputm  aedat  usnam  bvorg  xblnr  waers  kursf  bstat
             awtyp  awkey  hwaer  ausbk
      INTO   TABLE lt_bkpf
      FROM   bkpf FOR ALL ENTRIES IN lt_bsik_key_p
     WHERE   bukrs = lt_bsik_key_p-bukrs
       AND   belnr = lt_bsik_key_p-belnr
       AND   gjahr = lt_bsik_key_p-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_bkpf[].
    ENDIF.

    DELETE   lt_bseg         WHERE ( ( umskz IS NOT INITIAL ) OR "29505
                                     ( umsks IS NOT INITIAL )    ). "05

    DELETE   lt_bkpf         WHERE ( ( cpudt NOT IN s_cpudt ) AND
                                     ( aedat NOT IN s_cpudt )     ).

    SORT     lt_bseg      ASCENDING BY bukrs belnr gjahr buzei. "929505
    SORT     lt_bkpf      ASCENDING BY bukrs belnr gjahr.

*eject
* Process each individual invoice document
    CLEAR                                   ls_bseg.        "D30K929505
    READ     TABLE lt_bseg             INTO ls_bseg INDEX 1. """K929505

    LOOP AT    lt_bseg            ASSIGNING <fs_bseg>.      "D30K929505

      IF   ( ( <fs_bseg>-belnr           NE ls_bseg-belnr ) OR "K929505
             ( <fs_bseg>-bukrs           NE ls_bseg-bukrs ) OR "K929505
             ( <fs_bseg>-gjahr           NE ls_bseg-gjahr )    ). "9505
        CLEAR                               ls_bseg.        "D30K929505
        MOVE   <fs_bseg>                 TO ls_bseg.        "D30K929505

        CLEAR                               ls_invc_data.
        PERFORM  f_summarize_invoice TABLES lt_invc_data
                                      USING 'O'
                                   CHANGING ls_invc_data.

        IF       ( ls_invc_data-bukrs    IS NOT INITIAL ).

          PERFORM  f_process_invoice_workflow
                                      USING ls_invc_data-awtyp
                                            ls_invc_data-instid
                                            ls_invc_data.

        ENDIF.

        CLEAR    lt_invc_data[].
        CLEAR    ls_invc_data.

      ENDIF.

      IF   ( ( <fs_bseg>-belnr           NE ls_bkpf-belnr ) OR "K929505
             ( <fs_bseg>-bukrs           NE ls_bkpf-bukrs ) OR "K929505
             ( <fs_bseg>-gjahr           NE ls_bkpf-gjahr )    ). "9505
        CLEAR                               ls_bkpf.
        READ     TABLE lt_bkpf         INTO ls_bkpf         "D30K929505
                                   WITH KEY bukrs = <fs_bseg>-bukrs "05
                                            belnr = <fs_bseg>-belnr "05
                                            gjahr = <fs_bseg>-gjahr "05
                              BINARY SEARCH.                "D30K929505
        IF     ( sy-subrc NE 0 ).
          CLEAR  ls_bkpf.
        ENDIF.
      ENDIF.

*eject
      CLEAR                                 ls_invc_data.
      MOVE-CORRESPONDING ls_bkpf         TO ls_invc_data.
      MOVE-CORRESPONDING <fs_bseg>       TO ls_invc_data.   "D30K929505
      CLEAR                                 ls_invc_data-bukrs.
      MOVE     ls_bkpf-bukrs             TO ls_invc_data-bukrs.
      APPEND   ls_invc_data              TO lt_invc_data.

    ENDLOOP.

    IF     ( lt_invc_data[] IS NOT INITIAL ).

      CLEAR                                 ls_invc_data.
      PERFORM  f_summarize_invoice   TABLES lt_invc_data
                                      USING 'O'
                                   CHANGING ls_invc_data.

      IF       ( ls_invc_data-bukrs      IS NOT INITIAL ).

        PERFORM  f_process_invoice_workflow
                                      USING ls_invc_data-awtyp
                                            ls_invc_data-instid
                                            ls_invc_data.

        CLEAR    lt_invc_data[].
        CLEAR    ls_invc_data.

      ENDIF.

    ENDIF.

  ENDDO.

ENDFORM.                    " f_process_open_invoices
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_cleared_invoices
*&---------------------------------------------------------------------*
*       Process cleared invoices
*----------------------------------------------------------------------*
FORM f_process_cleared_invoices.

  DATA:    lt_lfa1_key                 TYPE gtt_lfa1_key,
           ls_lfa1_key                 TYPE gty_lfa1_key,
           lt_lfa1                     TYPE gtt_lfa1_key,
           ls_lfa1                     TYPE gty_lfa1_key,
           lt_bsak_key                 TYPE gtt_bsak_key,
           lt_bsak_key_p               TYPE gtt_bsak_key,
           lt_bseg                     TYPE gtt_bseg,       "D30K929505
           ls_bseg                     TYPE gty_bseg,       "D30K929505
           lt_bkpf                     TYPE gtt_bkpf,
           ls_bkpf                     TYPE gty_bkpf,
           lt_invc_data                TYPE gtt_invc_data,
           ls_invc_data                TYPE gty_invc_data,
           ls_object                   TYPE gty_object.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS:     lc_blocksize          TYPE syindex VALUE 5000.

  FIELD-SYMBOLS: <fs_bsak_key>         TYPE gty_bsak_key,
                 <fs_bseg>             TYPE gty_bseg.       "D30K929505

  IF     ( cb_clrd IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_lfa1_key[].
  CLEAR    ls_lfa1_key.
  CLEAR    lt_lfa1[].
  CLEAR    ls_lfa1.
  CLEAR    lt_bsak_key[].
  CLEAR    lt_bsak_key_p[].
  CLEAR    lt_bseg[].                                       "D30K929505
  CLEAR    ls_bseg.                                         "D30K929505
  CLEAR    lt_bkpf[].
  CLEAR    ls_bkpf.
  CLEAR    lt_invc_data[].
  CLEAR    ls_invc_data.

*eject
* Select the cleared documents key
  SELECT   bukrs  lifnr  augdt  augbl  gjahr  belnr
    INTO   TABLE lt_bsak_key
    FROM   bsak
   WHERE   bukrs IN s_bukrs
     AND   lifnr IN s_lifnr
     AND   gjahr IN s_gjahr
     AND   belnr IN s_belnr
     AND   budat IN s_budat
     AND   cpudt IN s_cpudt.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SORT     lt_bsak_key ASCENDING BY lifnr.

* Delete the clearing documents (payments)
  LOOP AT  lt_bsak_key            ASSIGNING <fs_bsak_key>.
    lv_tabix = sy-tabix.

    IF       ( <fs_bsak_key>-belnr       EQ <fs_bsak_key>-augbl ).

      DELETE   lt_bsak_key            INDEX lv_tabix.

    ELSE.

      IF     ( <fs_bsak_key>-lifnr       NE ls_lfa1_key-lifnr   ).
        CLEAR                               ls_lfa1_key.
        MOVE   <fs_bsak_key>-lifnr       TO ls_lfa1_key-lifnr.
        APPEND ls_lfa1_key               TO lt_lfa1_key.
      ENDIF.

    ENDIF.

  ENDLOOP.

*eject
* Filter the invoice documents by vendor account group
  IF     ( s_ktokk[] IS NOT INITIAL ).

    SORT     lt_lfa1_key       ASCENDING BY lifnr.
    DELETE   ADJACENT DUPLICATES       FROM lt_lfa1_key
                                  COMPARING lifnr.

    SELECT   lifnr
      INTO   TABLE lt_lfa1
      FROM   lfa1 FOR ALL ENTRIES IN lt_lfa1_key
     WHERE   lifnr  = lt_lfa1_key-lifnr
       AND   lifnr IN s_lifnr
       AND   ktokk IN s_ktokk.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    SORT     lt_lfa1           ASCENDING BY lifnr.
    DELETE   ADJACENT DUPLICATES       FROM lt_lfa1
                                  COMPARING lifnr.

    LOOP AT  lt_bsak_key          ASSIGNING <fs_bsak_key>.
      lv_tabix = sy-tabix.

      IF     ( <fs_bsak_key>-lifnr       EQ ls_lfa1-lifnr ).
        CONTINUE.
      ENDIF.

      CLEAR                                 ls_lfa1.
      READ     TABLE lt_lfa1           INTO ls_lfa1
                                   WITH KEY lifnr = <fs_bsak_key>-lifnr
                              BINARY SEARCH.
      IF     ( sy-subrc EQ 0 ).
        CONTINUE.
      ENDIF.

      DELETE   lt_bsak_key            INDEX lv_tabix.

    ENDLOOP.

  ENDIF.

  CLEAR    lt_lfa1_key[].
  CLEAR    lt_lfa1[].

*eject
* Process the invoice documents in batches
  SORT     lt_bsak_key         ASCENDING BY bukrs belnr gjahr.
  DELETE   ADJACENT DUPLICATES         FROM lt_bsak_key
                                  COMPARING bukrs belnr gjahr.

  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch of invoice document keys
    CLEAR           lt_bsak_key_p[].
    APPEND LINES OF lt_bsak_key
               FROM lv_index_lo
                 TO lv_index_hi
                 TO lt_bsak_key_p.

    IF     ( lt_bsak_key_p[] IS INITIAL ).
      EXIT.
    ENDIF.

*eject
    CLEAR    lt_bseg[].                                     "D30K929505
    CLEAR    lt_bkpf[].
    CLEAR    lt_invc_data[].

    SELECT   bukrs  belnr  gjahr  buzei  augdt  augbl  bschl """K929505
             koart  umskz  umsks  shkzg  mwskz  dmbtr  wrbtr """K929505
             zuonr  sgtxt  lifnr  zfbdt  zterm  zbd1t  zbd2t """K929505
             zbd3t  skfbt  sknto  wskto  zlsch  zlspr  xref3 """K929505
      INTO   TABLE lt_bseg                                  "D30K929505
      FROM   bseg FOR ALL ENTRIES IN lt_bsak_key_p          "D30K929505
     WHERE   bukrs = lt_bsak_key_p-bukrs                    "D30K929505
       AND   belnr = lt_bsak_key_p-belnr                    "D30K929505
       AND   gjahr = lt_bsak_key_p-gjahr.                   "D30K929505
    IF     ( sy-subrc EQ 0 ).                               "D30K929505
      DELETE lt_bseg WHERE koart <> 'K'.                    "D30K929505
    ELSE.                                                   "D30K929505
      CLEAR  lt_bseg[].                                     "D30K929505
    ENDIF.                                                  "D30K929505

    SELECT   bukrs  belnr  gjahr  blart  bldat  budat  monat  cpudt
             cputm  aedat  usnam  bvorg  xblnr  waers  kursf  bstat
             awtyp  awkey  hwaer  ausbk
      INTO   TABLE lt_bkpf
      FROM   bkpf FOR ALL ENTRIES IN lt_bsak_key_p
     WHERE   bukrs = lt_bsak_key_p-bukrs
       AND   belnr = lt_bsak_key_p-belnr
       AND   gjahr = lt_bsak_key_p-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_bkpf[].
    ENDIF.

    DELETE   lt_bseg         WHERE ( ( umskz IS NOT INITIAL ) OR "29505
                                     ( umsks IS NOT INITIAL )    ). "05

    DELETE   lt_bkpf         WHERE ( ( cpudt NOT IN s_cpudt ) AND
                                     ( aedat NOT IN s_cpudt )     ).

    SORT     lt_bseg      ASCENDING BY bukrs belnr gjahr buzei. "929505
    SORT     lt_bkpf      ASCENDING BY bukrs belnr gjahr.

*eject
* Process each individual invoice document
    CLEAR                                   ls_bseg.        "D30K929505
    READ     TABLE lt_bseg             INTO ls_bseg INDEX 1. " "K929505

    LOOP AT    lt_bseg            ASSIGNING <fs_bseg>.      "D30K929505

      IF   ( ( <fs_bseg>-belnr           NE ls_bseg-belnr ) OR "K929505
             ( <fs_bseg>-bukrs           NE ls_bseg-bukrs ) OR "K929505
             ( <fs_bseg>-gjahr           NE ls_bseg-gjahr )    ). "9505
        CLEAR                               ls_bseg.        "D30K929505
        MOVE   <fs_bseg>                 TO ls_bseg.        "D30K929505

        CLEAR                               ls_invc_data.
        PERFORM  f_summarize_invoice TABLES lt_invc_data
                                      USING 'C'
                                   CHANGING ls_invc_data.

        IF       ( ls_invc_data-bukrs    IS NOT INITIAL ).

          PERFORM  f_process_invoice_workflow
                                      USING ls_invc_data-awtyp
                                            ls_invc_data-instid
                                            ls_invc_data.

        ENDIF.

        CLEAR    lt_invc_data[].
        CLEAR    ls_invc_data.

      ENDIF.

      IF   ( ( <fs_bseg>-belnr           NE ls_bkpf-belnr ) OR "K929505
             ( <fs_bseg>-bukrs           NE ls_bkpf-bukrs ) OR "K929505
             ( <fs_bseg>-gjahr           NE ls_bkpf-gjahr )    ). "9505
        CLEAR                               ls_bkpf.
        READ     TABLE lt_bkpf         INTO ls_bkpf         "D30K929505
                                   WITH KEY bukrs = <fs_bseg>-bukrs "05
                                            belnr = <fs_bseg>-belnr "05
                                            gjahr = <fs_bseg>-gjahr "05
                              BINARY SEARCH.                "D30K929505
        IF     ( sy-subrc NE 0 ).
          CLEAR  ls_bkpf.
        ENDIF.
      ENDIF.

*eject
      CLEAR                                 ls_invc_data.
      MOVE-CORRESPONDING ls_bkpf         TO ls_invc_data.
      MOVE-CORRESPONDING <fs_bseg>       TO ls_invc_data.   "D30K929505
      CLEAR                                 ls_invc_data-bukrs.
      MOVE     ls_bkpf-bukrs             TO ls_invc_data-bukrs.
      APPEND   ls_invc_data              TO lt_invc_data.

    ENDLOOP.

    IF     ( lt_invc_data[] IS NOT INITIAL ).

      CLEAR                                 ls_invc_data.
      PERFORM  f_summarize_invoice   TABLES lt_invc_data
                                      USING 'C'
                                   CHANGING ls_invc_data.

      IF       ( ls_invc_data-bukrs      IS NOT INITIAL ).

        PERFORM  f_process_invoice_workflow
                                      USING ls_invc_data-awtyp
                                            ls_invc_data-instid
                                            ls_invc_data.

        CLEAR    lt_invc_data[].
        CLEAR    ls_invc_data.

      ENDIF.

    ENDIF.

  ENDDO.

ENDFORM.                    " f_process_cleared_invoices
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_invoice_data
*&---------------------------------------------------------------------*
*       Get the parked, open, or cleared invoice
*----------------------------------------------------------------------*
FORM f_get_invoice_data
  USING    iv_awtyp                    TYPE awtyp
           iv_instid                   TYPE sibfboriid
  CHANGING cs_invc_data                TYPE gty_invc_data.

  DATA:    lt_invc_data                TYPE gtt_invc_data,
           ls_invc_data                TYPE gty_invc_data,
           lt_bkpf                     TYPE gtt_bkpf,
           ls_bkpf                     TYPE gty_bkpf,
           lt_bseg                     TYPE gtt_bseg,       "D30K929505
           lt_vbsegk                   TYPE gtt_vbsegk.

  DATA:    lv_bukrs                    TYPE bukrs,
           lv_belnr                    TYPE belnr_d,
           lv_gjahr                    TYPE gjahr,
           lv_awkey                    TYPE awkey,
           lv_lifnr                    TYPE lifnr,
           lv_stage                    TYPE char1.

  FIELD-SYMBOLS: <fs_bkpf>             TYPE gty_bkpf,
                 <fs_bseg>             TYPE gty_bseg,       "D30K929505
                 <fs_vbsegk>           TYPE gty_vbsegk.

  CLEAR    cs_invc_data.

  IF   ( ( iv_awtyp  IS INITIAL ) OR
         ( iv_instid IS INITIAL )    ).
    RETURN.
  ENDIF.

  CLEAR    lt_invc_data[].
  CLEAR    ls_invc_data.
  CLEAR    lt_bkpf[].
  CLEAR    ls_bkpf.
  CLEAR    lt_bseg[].                                       "D30K929505
  CLEAR    lt_vbsegk[].

  CLEAR    lv_bukrs.
  CLEAR    lv_belnr.
  CLEAR    lv_gjahr.
  CLEAR    lv_awkey.
  CLEAR    lv_lifnr.
  CLEAR    lv_stage.

*eject
* Get the non-PO invoice data
  IF     ( iv_awtyp EQ 'BKPF' ).

    MOVE     iv_instid+00(04)            TO lv_bukrs.
    MOVE     iv_instid+04(10)            TO lv_belnr.
    MOVE     iv_instid+14(04)            TO lv_gjahr.

    IF   ( ( lv_bukrs                NOT IN s_bukrs ) OR
           ( lv_belnr                NOT IN s_belnr ) OR
           ( lv_gjahr                NOT IN s_gjahr )    ).
      RETURN.
    ENDIF.

* Select the accounting document header
    SELECT   SINGLE bukrs  belnr  gjahr  blart  bldat  budat  monat
             cpudt  cputm  aedat  usnam  bvorg  xblnr  waers  kursf
             bstat  awtyp  awkey  hwaer  ausbk
      INTO   ls_bkpf
      FROM   bkpf
     WHERE   bukrs = lv_bukrs
       AND   belnr = lv_belnr
       AND   gjahr = lv_gjahr.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    IF     ( ls_bkpf-budat           NOT IN s_budat ).
      RETURN.
    ENDIF.

*eject
* Get the PO invoice data
  ELSEIF ( iv_awtyp EQ 'RMRP' ).

    MOVE     iv_instid                   TO lv_awkey.

* Select the accounting document header
    SELECT   bukrs  belnr  gjahr  blart  bldat  budat  monat  cpudt
             cputm  aedat  usnam  bvorg  xblnr  waers  kursf  bstat
             awtyp  awkey  hwaer  ausbk
      INTO   TABLE lt_bkpf
      FROM   bkpf
     WHERE   awtyp = iv_awtyp
       AND   awkey = lv_awkey.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    IF     ( lines( lt_bkpf[] )          EQ 1 ).

      READ     TABLE lt_bkpf           INTO ls_bkpf INDEX 1.

    ELSE.

      LOOP AT    lt_bkpf          ASSIGNING <fs_bkpf>.
        IF     ( <fs_bkpf>-bvorg+10(04)  EQ <fs_bkpf>-bukrs ).
          MOVE   <fs_bkpf>               TO ls_bkpf.
          EXIT.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF     ( ls_bkpf-bukrs IS INITIAL ).
      RETURN.
    ENDIF.

    IF   ( ( ls_bkpf-bukrs           NOT IN s_bukrs ) OR
           ( ls_bkpf-belnr           NOT IN s_belnr ) OR
           ( ls_bkpf-gjahr           NOT IN s_gjahr ) OR
           ( ls_bkpf-budat           NOT IN s_budat )    ).
      RETURN.
    ENDIF.

  ENDIF.

*eject
* Select the parked vendor item data
  IF     ( ls_bkpf-bstat                 EQ 'Z' ).          "D30K929505
*BEGIN OF  CHANGES BY KBANERJEE FOR ENHC0025317
    MOVE-CORRESPONDING ls_bkpf       TO ls_invc_data.
    APPEND             ls_invc_data  TO lt_invc_data.
*END OF CHANGES BY KBANERJEE FOR ENHC0025317
  ELSEIF ( ls_bkpf-bstat                 EQ 'V' ).          "D30K929505

    SELECT   ausbk  belnr  gjahr  bzkey  bukrs  buzei  bschl
             umskz  umsks  shkzg  mwskz  dmbtr  wrbtr  zuonr
             sgtxt  lifnr  zfbdt  zterm  zbd1t  zbd2t  zbd3t
             skfbt  sknto  wskto  zlsch  zlspr  xref3
      INTO   TABLE lt_vbsegk
      FROM   vbsegk
     WHERE   ausbk = ls_bkpf-bukrs
       AND   belnr = ls_bkpf-belnr
       AND   gjahr = ls_bkpf-gjahr.
    IF     ( sy-subrc EQ 0 ).

      MOVE     'P'                       TO lv_stage.

      LOOP AT  lt_vbsegk          ASSIGNING <fs_vbsegk>.
        CLEAR                               ls_invc_data.
        MOVE-CORRESPONDING <fs_vbsegk>   TO ls_invc_data.
        MOVE-CORRESPONDING ls_bkpf       TO ls_invc_data.
        APPEND             ls_invc_data  TO lt_invc_data.
      ENDLOOP.

    ELSE.

      CLEAR  lt_vbsegk[].

    ENDIF.

*eject
* Select the open and cleared vendor item data              "D30K929505
  ELSEIF ( ls_bkpf-bukrs                 IS NOT INITIAL ).

    SELECT   bukrs  belnr  gjahr  buzei  augdt  augbl  bschl """K929505
             koart  umskz  umsks  shkzg  mwskz  dmbtr  wrbtr """K929505
             zuonr  sgtxt  lifnr  zfbdt  zterm  zbd1t  zbd2t """K929505
             zbd3t  skfbt  sknto  wskto  zlsch  zlspr  xref3 """K929505
      INTO   TABLE lt_bseg                                  "D30K929505
      FROM   bseg                                           "D30K929505
     WHERE   bukrs = ls_bkpf-bukrs                          "D30K929505
       AND   belnr = ls_bkpf-belnr                          "D30K929505
       AND   gjahr = ls_bkpf-gjahr.                         "D30K929505
    IF     ( sy-subrc EQ 0 ).                               "D30K929505

      DELETE lt_bseg WHERE koart <> 'K'.                    "D30K929505

      MOVE       'C'                     TO lv_stage.       "D30K929505

      LOOP AT    lt_bseg          ASSIGNING <fs_bseg>.      "D30K929505

        IF     ( <fs_bseg>-augbl         IS INITIAL ).      "D30K929505
          MOVE   'O'                     TO lv_stage.       "D30K929505
        ENDIF.                                              "D30K929505

        IF     ( <fs_bseg>-augbl         EQ <fs_bseg>-belnr ). "K929505
          CLEAR  ls_invc_data.                              "D30K929505
        ENDIF.                                              "D30K929505

        CLEAR                               ls_invc_data.   "D30K929505
        MOVE-CORRESPONDING ls_bkpf       TO ls_invc_data.   "D30K929505
        MOVE-CORRESPONDING <fs_bseg>     TO ls_invc_data.   "D30K929505
        APPEND             ls_invc_data  TO lt_invc_data.   "D30K929505

      ENDLOOP.                                              "D30K929505

    ELSE.                                                   "D30K929505
      CLEAR  lt_bseg[].                                     "D30K929505
    ENDIF.                                                  "D30K929505

  ENDIF.

  IF       ( lt_invc_data[]              IS INITIAL ).
    RETURN.
  ENDIF.

*eject
* Summarize the vendor item data
  CLEAR      ls_invc_data.

  IF       ( lt_invc_data[]              IS NOT INITIAL ).

    PERFORM  f_summarize_invoice     TABLES lt_invc_data
                                      USING lv_stage
                                   CHANGING ls_invc_data.

  ELSE.

    MOVE-CORRESPONDING ls_bkpf           TO ls_invc_data.
    MOVE     'P'                         TO ls_invc_data-stage.

  ENDIF.

* Filter the vendor number and vendor account group
  IF       ( ls_invc_data-lifnr          IS NOT INITIAL ).

    IF     ( ls_invc_data-lifnr      NOT IN s_lifnr ).
      RETURN.
    ENDIF.

    SELECT   SINGLE lifnr
      INTO   lv_lifnr
      FROM   lfa1
     WHERE   lifnr  = ls_invc_data-lifnr
       AND   ktokk IN s_ktokk.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

  ENDIF.

  IF       ( ls_invc_data-bukrs          IS INITIAL ).
    RETURN.
  ENDIF.

  cs_invc_data = ls_invc_data.

ENDFORM.                    " f_get_invoice_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_summarize_invoice
*&---------------------------------------------------------------------*
*       Summarize the vendor items
*----------------------------------------------------------------------*
FORM f_summarize_invoice
  TABLES   it_invc_data                TYPE gtt_invc_data
  USING    iv_stage                    TYPE char1
  CHANGING cs_invc_data                TYPE gty_invc_data.

  DATA:    ls_invc_data                TYPE gty_invc_data,
           ls_object                   TYPE gty_object.

  DATA:    lv_lifnr                    TYPE lifnr,
           lv_wskto                    TYPE wskto,
           lv_fl_delete                TYPE xflag.

  FIELD-SYMBOLS: <fs_invc_data>        TYPE gty_invc_data.

  CONSTANTS:     lc_cad                TYPE waers VALUE 'CAD',
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0140727
                 lc_zcar               TYPE awtyp VALUE 'ZCARS'.
*END OF CHANGES BY KBANERJEE FOR CHG0140727
  CLEAR    cs_invc_data.

  IF     ( it_invc_data[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    ls_invc_data.
  CLEAR    ls_object.
  CLEAR    lv_lifnr.
  CLEAR    lv_fl_delete.

  READ     TABLE   it_invc_data        INTO ls_invc_data INDEX 1.
*BEGIN OF COMMENT BY KBANERJEE FOR ENHC0025317
*  IF     ( ls_invc_data-bstat            EQ 'Z' ).          "D30K929505
*    RETURN.                                                 "D30K929505
*  ENDIF.                                                    "D30K929505
*END OF COMMENT BY KBANERJEE FOR ENHC0025317
  MOVE     ls_invc_data-lifnr            TO lv_lifnr.

  CLEAR                                     ls_invc_data-stage.
*BEGIN OF CHANGES BY KBANERJEE FOR ENHC0025317
  IF     ( ls_invc_data-bstat            EQ 'Z' ).
    MOVE     'D'                         TO ls_invc_data-stage.
  ELSE.
*END OF CHANGES BY KBANERJEE FOR ENHC0025317
    MOVE     iv_stage                    TO ls_invc_data-stage.
*BEGIN OF CHANGES BY KBANERJEE FOR ENHC0025317
  ENDIF.
*END OF CHANGES BY KBANERJEE FOR ENHC0025317
  CLEAR                                     ls_invc_data-instid.
  IF     ( ls_invc_data-awtyp            CS 'BKPF' ).
    CLEAR                                   ls_invc_data-awtyp.
    MOVE   'BKPF'                        TO ls_invc_data-awtyp.
    MOVE   ls_invc_data-bukrs            TO ls_invc_data-instid+00(04).
    MOVE   ls_invc_data-belnr            TO ls_invc_data-instid+04(10).
    MOVE   ls_invc_data-gjahr            TO ls_invc_data-instid+14(04).
  ELSEIF ( ls_invc_data-awtyp            EQ 'RMRP' ).
    MOVE   ls_invc_data-awkey            TO ls_invc_data-instid.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0140727
  ELSEIF ( ls_invc_data-awtyp            EQ lc_zcar ).
    MOVE   lc_zcar                       TO ls_invc_data-awtyp.
    MOVE   ls_invc_data-bukrs            TO ls_invc_data-instid+00(04).
    MOVE   ls_invc_data-belnr            TO ls_invc_data-instid+04(10).
    MOVE   ls_invc_data-gjahr            TO ls_invc_data-instid+14(04).
*END OF CHANGES BY KBANERJEE FOR CHG0140727
  ELSE.
    RETURN.
  ENDIF.

*eject
  CLEAR    ls_invc_data-dmbtr.
  CLEAR    ls_invc_data-wrbtr.
  CLEAR    ls_invc_data-skfbt.
  CLEAR    ls_invc_data-sknto.
  CLEAR    ls_invc_data-wskto.

  LOOP AT  it_invc_data           ASSIGNING <fs_invc_data>.

    IF       ( <fs_invc_data>-shkzg      EQ 'H' ).
      SUBTRACT <fs_invc_data>-dmbtr    FROM ls_invc_data-dmbtr.
      SUBTRACT <fs_invc_data>-wrbtr    FROM ls_invc_data-wrbtr.
      SUBTRACT <fs_invc_data>-skfbt    FROM ls_invc_data-skfbt.
      SUBTRACT <fs_invc_data>-sknto    FROM ls_invc_data-sknto.
      SUBTRACT <fs_invc_data>-wskto    FROM ls_invc_data-wskto.
    ELSE.
      ADD      <fs_invc_data>-dmbtr      TO ls_invc_data-dmbtr.
      ADD      <fs_invc_data>-wrbtr      TO ls_invc_data-wrbtr.
      ADD      <fs_invc_data>-skfbt      TO ls_invc_data-skfbt.
      ADD      <fs_invc_data>-sknto      TO ls_invc_data-sknto.
      ADD      <fs_invc_data>-wskto      TO ls_invc_data-wskto.
    ENDIF.

    IF       (  ls_invc_data-xblnr       IS INITIAL ).
      MOVE     <fs_invc_data>-xblnr      TO ls_invc_data-xblnr.
    ENDIF.

    IF       ( <fs_invc_data>-lifnr      NE lv_lifnr ).
      MOVE     abap_true                 TO lv_fl_delete.
    ENDIF.

  ENDLOOP.

*  IF ( ( ls_invc_data-xblnr          IS     INITIAL ) OR "DECK919276
  IF   ( lv_fl_delete                IS NOT INITIAL ).      "DECK919276
    RETURN.
  ENDIF.

* Calculate the discount percent
  IF     ( ls_invc_data-wrbtr EQ 0 ).
    ls_invc_data-disc_prcnt = 9999 / 10.
  ELSEIF ( ls_invc_data-wskto EQ 0 ).
    ls_invc_data-disc_prcnt = 0.
  ELSE.
    ls_invc_data-disc_prcnt = ls_invc_data-wskto /
                              ls_invc_data-wrbtr * 100.
  ENDIF.

*eject
* Calculate the net due date

  CALL FUNCTION 'NET_DUE_DATE_GET'
    EXPORTING
      i_zfbdt = ls_invc_data-zfbdt
      i_zbd1t = ls_invc_data-zbd1t
      i_zbd2t = ls_invc_data-zbd2t
      i_zbd3t = ls_invc_data-zbd3t
      i_shkzg = ls_invc_data-shkzg
      i_rebzg = space
      i_koart = 'K'
    IMPORTING
      e_faedt = ls_invc_data-due_date.

* Calculate the amount in reporting currency and the exchange rate
  IF       ( ls_invc_data-waers          EQ lc_cad ). "Doc.Curr.

    MOVE     ls_invc_data-wrbtr          TO ls_invc_data-rprtg_amt.
    MOVE     1                           TO ls_invc_data-rprtg_fx.

  ELSEIF   ( ls_invc_data-hwaer          EQ lc_cad ). "Local Curr.

    MOVE     ls_invc_data-dmbtr          TO ls_invc_data-rprtg_amt.

    IF     ( ls_invc_data-dmbtr EQ 0 ) OR ( ls_invc_data-wrbtr EQ 0 ).
      ls_invc_data-rprtg_fx = 0.
    ELSE.
      ls_invc_data-rprtg_fx = ls_invc_data-dmbtr / ls_invc_data-wrbtr.
    ENDIF.

*eject
  ELSE.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        client           = sy-mandt
        date             = ls_invc_data-budat
        foreign_amount   = ls_invc_data-wrbtr
        foreign_currency = ls_invc_data-waers
        local_currency   = lc_cad
        type_of_rate     = p_kurst
        read_tcurr       = abap_true
      IMPORTING
        exchange_rate    = ls_invc_data-rprtg_fx
        local_amount     = ls_invc_data-rprtg_amt
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.
    IF ( sy-subrc NE 0 ).
      CLEAR    ls_invc_data-rprtg_fx.
      CLEAR    ls_invc_data-rprtg_amt.
*     MESSAGE  ID sy-msgid             TYPE sy-msgty
*          NUMBER sy-msgno             INTO lv_text
*            WITH sy-msgv1                  sy-msgv2
*                 sy-msgv3                  sy-msgv4.
*     WRITE: / lv_text.
    ENDIF.

  ENDIF.

*eject
* Calculate the discount amount in reporting currency
  IF       ( ls_invc_data-waers          EQ lc_cad ). "Doc.Curr.
  ELSEIF   ( ls_invc_data-hwaer          EQ lc_cad ). "Local Curr.
    CLEAR                                   ls_invc_data-wskto.
    MOVE     ls_invc_data-sknto          TO ls_invc_data-wskto.
  ELSE.

    CLEAR    lv_wskto.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        client           = sy-mandt
        date             = ls_invc_data-budat
        foreign_amount   = ls_invc_data-wskto
        foreign_currency = ls_invc_data-waers
        local_currency   = lc_cad
        type_of_rate     = p_kurst
        read_tcurr       = abap_true
      IMPORTING
        local_amount     = lv_wskto
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.
    IF ( sy-subrc EQ 0 ).

      CLEAR                                 ls_invc_data-wskto.
      MOVE     lv_wskto                  TO ls_invc_data-wskto.

    ELSE.
*     MESSAGE  ID sy-msgid             TYPE sy-msgty
*          NUMBER sy-msgno             INTO lv_text
*            WITH sy-msgv1                  sy-msgv2
*                 sy-msgv3                  sy-msgv4.
*     WRITE: / lv_text.
    ENDIF.

  ENDIF.

* Transfer data to the output structure
  MOVE       ls_invc_data                TO cs_invc_data.

ENDFORM.                    " f_summarize_invoice
