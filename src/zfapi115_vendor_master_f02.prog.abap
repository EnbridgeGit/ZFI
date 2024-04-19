*&---------------------------------------------------------------------*
*& Report  ZFAPI115_VENDOR_MASTER_F02
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI115_VENDOR_MASTER                        *
* Include            :   ZFAPI115_VENDOR_MASTER_F02                    *
* Author             :   Vijay Rajaputra                               *
* Date               :   03-Mar-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Vendor Master Extraction for IAP               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 03-Mar-2018  VRAJAPUTRA  D30K928789 CHG0105961 - Initial development *
*                          D30K928848                                  *
*&---------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_full_extract
*&---------------------------------------------------------------------*
*       Perform a full extract
*----------------------------------------------------------------------*
FORM f_get_full_extract.

  DATA:    lt_lfa1_key                 TYPE gtt_lfa1_key.

  DATA:    lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS: lc_blocksize              TYPE syindex VALUE 1000.

  SELECT   lifnr    ktokk    loevm    sperr
*           zz_iap_vendor_id  zz_iap_ritm_id
    INTO   TABLE gt_lfa1_key
    FROM   lfa1
   WHERE   lifnr IN s_lifnr
     AND   ktokk IN s_ktokk.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  IF       ( cb_inblk                    IS INITIAL ).
    DELETE   gt_lfa1_key              WHERE sperr IS NOT INITIAL.
  ENDIF.
  IF       ( cb_indel                    IS INITIAL ).
    DELETE   gt_lfa1_key              WHERE loevm IS NOT INITIAL.
  ENDIF.

  SORT     gt_lfa1_key         ASCENDING BY lifnr.

*eject
* Process the vendors in batches
  DO.

* Calculate the low and high indices for the batch of vendors
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch of vendors
    CLEAR             lt_lfa1_key[].
    APPEND   LINES OF gt_lfa1_key
                 FROM lv_index_lo
                   TO lv_index_hi
                   TO lt_lfa1_key.

    IF     ( lt_lfa1_key[] IS INITIAL ).
      EXIT.
    ENDIF.

    PERFORM  f_process_full_extract  TABLES lt_lfa1_key.

  ENDDO.

ENDFORM.                    " f_get_full_extract
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_full_extract
*&---------------------------------------------------------------------*
*       Process the full extract
*----------------------------------------------------------------------*
FORM f_process_full_extract
  TABLES   it_lfa1_key                 TYPE gtt_lfa1_key.

  CLEAR    gt_lfa1[].
  CLEAR    gt_lfbk[].
* CLEAR    gt_tbchain21[]. do not remove the comment '*'
  CLEAR    gt_lfb1[].
  CLEAR    gt_lfza[].
  CLEAR    gt_lfbw[].
  CLEAR    gt_lfm1[].
  CLEAR    gt_wyt3[].
  CLEAR    gt_knvk[].
  CLEAR    gt_adrc[].
  CLEAR    gt_adr2[].
  CLEAR    gt_adr3[].
  CLEAR    gt_adr6[].

  IF     ( it_lfa1_key[] IS INITIAL ).
    RETURN.
  ENDIF.

* Select lfa1
  SELECT   lifnr  land1  name1  name2  adrnr
           mcod1  mcod2  bahns  begru  konzs
           ktokk  lnrza  loevm  sperr  sperm
           stcd1  stcd2  telf1  telfx  sperz
           xlfza
*           zz_iap_vendor_id
    INTO   TABLE gt_lfa1
    FROM   lfa1 FOR ALL ENTRIES IN it_lfa1_key
   WHERE   lifnr = it_lfa1_key-lifnr.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_lfa1 ASCENDING BY lifnr.
  ELSE.
    CLEAR  gt_lfa1[].
  ENDIF.

*eject
* Select lfbk
  IF     ( gt_lfa1[] IS NOT INITIAL ).

    SELECT   lifnr  banks  bankl  bankn
             bkont  bvtyp  bkref
      INTO   TABLE gt_lfbk
      FROM   lfbk FOR ALL ENTRIES IN gt_lfa1
     WHERE   lifnr = gt_lfa1-lifnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfbk ASCENDING BY lifnr banks bankl bankn.
    ELSE.
      CLEAR  gt_lfbk[].
    ENDIF.

  ENDIF.

* Select tbchain21
  IF     ( gt_tbchain21[] IS INITIAL ).

    SELECT   *
      INTO   TABLE gt_tbchain21
      FROM   tbchain21.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_tbchain21 ASCENDING BY mandt banksrec bankkrec
                                       banknrec waers.
    ELSE.
      CLEAR  gt_tbchain21[].
    ENDIF.

  ENDIF.

*eject
* Select lfb1
  SELECT   lifnr  bukrs  sperr  loevm  zuawa
           akont  zwels  zahls  zterm  fdgrv
           busab  lnrzb  zindt  reprf  hbkid
           xpore  qsskz  mindk  uzawe  qsrec
           qland  xlfzb  cerdt
    INTO   TABLE gt_lfb1
    FROM   lfb1 FOR ALL ENTRIES IN it_lfa1_key
   WHERE   lifnr  = it_lfa1_key-lifnr
     AND   bukrs IN s_bukrs.
  IF     ( sy-subrc EQ 0 ).

    IF       ( cb_inblk                  IS INITIAL ).
      DELETE   gt_lfb1                WHERE sperr IS NOT INITIAL.
    ENDIF.

    IF       ( cb_indel                  IS INITIAL ).
      DELETE   gt_lfb1                WHERE loevm IS NOT INITIAL.
    ENDIF.

    SORT   gt_lfb1 ASCENDING BY lifnr bukrs.

  ELSE.
    CLEAR  gt_lfb1[].
  ENDIF.

*eject
* Select lfza at the vendor level
  IF     ( gt_lfa1[] IS NOT INITIAL ).

    SELECT   lifnr  bukrs  empfk
      INTO   TABLE gt_lfza
      FROM   lfza FOR ALL ENTRIES IN gt_lfa1
     WHERE   lifnr = gt_lfa1-lifnr
       AND   bukrs = SPACE.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_lfza[].
    ENDIF.

  ENDIF.

* Select lfza at the vendor company level
  IF     ( gt_lfb1[] IS NOT INITIAL ).

    SELECT   lifnr  bukrs  empfk
      APPENDING TABLE gt_lfza
      FROM   lfza FOR ALL ENTRIES IN gt_lfb1
     WHERE   lifnr = gt_lfb1-lifnr
       AND   bukrs = gt_lfb1-bukrs
       AND   bukrs > SPACE.

  ENDIF.

  IF     ( gt_lfza[] IS NOT INITIAL ).

    SORT   gt_lfza ASCENDING BY lifnr bukrs empfk.

  ENDIF.

*eject
* Select lfbw
  IF     ( gt_lfb1[] IS NOT INITIAL ).

    SELECT   lifnr  bukrs  witht  wt_subjct
             qsrec  wt_wtstcd     wt_withcd
      INTO   TABLE gt_lfbw
      FROM   lfbw FOR ALL ENTRIES IN gt_lfb1
     WHERE   lifnr = gt_lfb1-lifnr
       AND   bukrs = gt_lfb1-bukrs.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfbw ASCENDING BY lifnr bukrs witht.
    ELSE.
      CLEAR  gt_lfbw[].
    ENDIF.

  ENDIF.

* Select lfm1
  SELECT   lifnr  ekorg  sperm  loevm  waers
           zterm  inco1  inco2  webre  kalsk
           kzaut  meprf  eikto
    INTO   TABLE gt_lfm1
    FROM   lfm1 FOR ALL ENTRIES IN it_lfa1_key
   WHERE   lifnr  = it_lfa1_key-lifnr
     AND   ekorg IN s_ekorg.
  IF     ( sy-subrc EQ 0 ).

    IF       ( cb_indel                  IS INITIAL ).
      DELETE   gt_lfm1                WHERE loevm IS NOT INITIAL.
    ENDIF.

    SORT   gt_lfm1 ASCENDING BY lifnr ekorg.

  ELSE.
    CLEAR  gt_lfm1[].
  ENDIF.

* Select wyt3
  IF     ( gt_lfm1[] IS NOT INITIAL ).

    SELECT   lifnr  ekorg  parvw
             parza  lifn2  defpa
      INTO   TABLE gt_wyt3
      FROM   wyt3 FOR ALL ENTRIES IN gt_lfm1
     WHERE   lifnr = gt_lfm1-lifnr
       AND   ekorg = gt_lfm1-ekorg.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_wyt3 ASCENDING BY lifnr ekorg parvw parza.
    ELSE.
      CLEAR  gt_wyt3[].
    ENDIF.

  ENDIF.

*eject
  IF     ( gt_lfa1[] IS NOT INITIAL ).

* Select knvk
    SELECT   parnr  namev  name1  abtnr
             telf1  pafkt  lifnr  prsnr
      INTO   TABLE gt_knvk
      FROM   knvk FOR ALL ENTRIES IN gt_lfa1
     WHERE   lifnr = gt_lfa1-lifnr.
    IF     ( sy-subrc EQ 0 ).
      SORT     gt_knvk ASCENDING BY lifnr parnr.
    ELSE.
      CLEAR    gt_knvk[].
    ENDIF.

* Select adrc
    SELECT   addrnumber  name1       city1
             post_code1  street      str_suppl2
             country     region
      INTO   TABLE gt_adrc
      FROM   adrc FOR ALL ENTRIES IN gt_lfa1
     WHERE   addrnumber = gt_lfa1-adrnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_adrc ASCENDING BY addrnumber.
    ELSE.
      CLEAR  gt_adrc[].
    ENDIF.

* Select adr2
    SELECT   addrnumber  persnumber  tel_number  tel_extens
      INTO   TABLE gt_adr2
      FROM   adr2 FOR ALL ENTRIES IN gt_lfa1
     WHERE   addrnumber = gt_lfa1-adrnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_adr2 ASCENDING BY addrnumber persnumber.
    ELSE.
      CLEAR  gt_adr2[].
    ENDIF.

*eject
* Select adr3
    SELECT   addrnumber  persnumber  fax_number  fax_extens  faxnr_long
      INTO   TABLE gt_adr3
      FROM   adr3 FOR ALL ENTRIES IN gt_lfa1
     WHERE   addrnumber = gt_lfa1-adrnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_adr3 ASCENDING BY addrnumber persnumber.
    ELSE.
      CLEAR  gt_adr3[].
    ENDIF.

* Select adr6
    SELECT   addrnumber  persnumber  smtp_addr
      INTO   TABLE gt_adr6
      FROM   adr6 FOR ALL ENTRIES IN gt_lfa1
     WHERE   addrnumber = gt_lfa1-adrnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_adr6 ASCENDING BY addrnumber persnumber.
    ELSE.
      CLEAR  gt_adr6[].
    ENDIF.

  ENDIF.

* Process the records
  PERFORM  f_process_records         TABLES it_lfa1_key.

ENDFORM.                    " f_process_full_extract
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_delta_extract
*&---------------------------------------------------------------------*
*       Perform a delta extract
*----------------------------------------------------------------------*
FORM f_get_delta_extract.

  DATA:    lt_lfa1_key                 TYPE gtt_lfa1_key.

  DATA:    lt_editpos                  TYPE STANDARD TABLE OF cdred.

  DATA:    lr_tabname                  TYPE RANGE OF tabname,
           ls_tabname                  LIKE LINE OF lr_tabname.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_addrnumber               TYPE ad_addrnum.

  FIELD-SYMBOLS: <fs_editpos>          TYPE cdred.

*eject
* Build object keys for object class "KRED"
  CLEAR    lr_tabname[].
  CLEAR                                    ls_tabname.
  MOVE     'I'                          TO ls_tabname-sign.
  MOVE     'EQ'                         TO ls_tabname-option.
  MOVE     'LFA1'                       TO ls_tabname-low.
  APPEND   ls_tabname                   TO lr_tabname.
  MOVE     'LFBK'                       TO ls_tabname-low.
  APPEND   ls_tabname                   TO lr_tabname.
  MOVE     'LFB1'                       TO ls_tabname-low.
  APPEND   ls_tabname                   TO lr_tabname.
  MOVE     'LFZA'                       TO ls_tabname-low.
  APPEND   ls_tabname                   TO lr_tabname.
  MOVE     'LFBW'                       TO ls_tabname-low.
  APPEND   ls_tabname                   TO lr_tabname.
  MOVE     'LFM1'                       TO ls_tabname-low.
  APPEND   ls_tabname                   TO lr_tabname.
  MOVE     'WYT3'                       TO ls_tabname-low.
  APPEND   ls_tabname                   TO lr_tabname.
  MOVE     'KNVK'                       TO ls_tabname-low.
  APPEND   ls_tabname                   TO lr_tabname.
  SORT     lr_tabname         ASCENDING BY low.

  CLEAR    lt_editpos[].

  CALL FUNCTION 'CHANGEDOCUMENT_READ'
    EXPORTING
      DATE_OF_CHANGE             = s_date-low
      OBJECTCLASS                = 'KRED'
      DATE_UNTIL                 = s_date-high
    TABLES
      EDITPOS                    = lt_editpos
    EXCEPTIONS
      NO_POSITION_FOUND          = 1
      WRONG_ACCESS_TO_ARCHIVE    = 2
      TIME_ZONE_CONVERSION_ERROR = 3
      OTHERS                     = 4.

  IF     ( sy-subrc EQ 0 ).

    SORT     lt_editpos             BY objectid  ASCENDING
                                       tabname   ASCENDING
                                       tabkey    ASCENDING
                                       udate     DESCENDING
                                       utime     DESCENDING.

    DELETE   ADJACENT DUPLICATES  FROM lt_editpos
                             COMPARING objectid
                                       tabname
                                       tabkey.

  ELSE.
* Implement suitable error handling here
  ENDIF.

*eject
  LOOP AT  lt_editpos             ASSIGNING <fs_editpos>.

    IF     ( <fs_editpos>-tabname    NOT IN lr_tabname ).
      CONTINUE.
    ENDIF.

    CASE     <fs_editpos>-tabname.
      WHEN     'LFA1'.

        CLEAR                                    gs_lfa1_key.
        MOVE     <fs_editpos>-tabkey+03(10)   TO gs_lfa1_key-lifnr.
        MOVE     <fs_editpos>-chngind         TO gs_lfa1_key-chngind.
        APPEND   gs_lfa1_key                  TO gt_lfa1_key.
        CLEAR                                    gs_lifnr.
        MOVE     <fs_editpos>-objectid+00(10) TO gs_lifnr.
        APPEND   gs_lifnr                     TO gt_lifnr.

      WHEN     'LFBK'.

        CLEAR                                    gs_lfbk_key.
        MOVE     <fs_editpos>-tabkey+03(10)   TO gs_lfbk_key-lifnr.
        MOVE     <fs_editpos>-tabkey+13(03)   TO gs_lfbk_key-banks.
        MOVE     <fs_editpos>-tabkey+16(15)   TO gs_lfbk_key-bankl.
        MOVE     <fs_editpos>-tabkey+31(18)   TO gs_lfbk_key-bankn.
        MOVE     <fs_editpos>-chngind         TO gs_lfbk_key-chngind.
        APPEND   gs_lfbk_key                  TO gt_lfbk_key.
        CLEAR                                    gs_lifnr.
        MOVE     <fs_editpos>-objectid+00(10) TO gs_lifnr.
        APPEND   gs_lifnr                     TO gt_lifnr.

      WHEN     'LFB1'.

        CLEAR                                    gs_lfb1_key.
        MOVE     <fs_editpos>-tabkey+03(10)   TO gs_lfb1_key-lifnr.
        MOVE     <fs_editpos>-tabkey+13(04)   TO gs_lfb1_key-bukrs.
        MOVE     <fs_editpos>-chngind         TO gs_lfb1_key-chngind.
        APPEND   gs_lfb1_key                  TO gt_lfb1_key.
        CLEAR                                    gs_lifnr.
        MOVE     <fs_editpos>-objectid+00(10) TO gs_lifnr.
        APPEND   gs_lifnr                     TO gt_lifnr.

      WHEN     'LFZA'.

        CLEAR                                    gs_lfza_key.
        MOVE     <fs_editpos>-tabkey+03(10)   TO gs_lfza_key-lifnr.
        MOVE     <fs_editpos>-tabkey+13(04)   TO gs_lfza_key-bukrs.
        MOVE     <fs_editpos>-tabkey+17(10)   TO gs_lfza_key-empfk.
        MOVE     <fs_editpos>-chngind         TO gs_lfza_key-chngind.
        APPEND   gs_lfza_key                  TO gt_lfza_key.
        CLEAR                                    gs_lifnr.
        MOVE     <fs_editpos>-objectid+00(10) TO gs_lifnr.
        APPEND   gs_lifnr                     TO gt_lifnr.

*eject
      WHEN     'LFBW'.

        CLEAR                                    gs_lfbw_key.
        MOVE     <fs_editpos>-tabkey+03(10)   TO gs_lfbw_key-lifnr.
        MOVE     <fs_editpos>-tabkey+13(04)   TO gs_lfbw_key-bukrs.
        MOVE     <fs_editpos>-tabkey+17(02)   TO gs_lfbw_key-witht.
        MOVE     <fs_editpos>-chngind         TO gs_lfbw_key-chngind.
        APPEND   gs_lfbw_key                  TO gt_lfbw_key.
        CLEAR                                    gs_lifnr.
        MOVE     <fs_editpos>-objectid+00(10) TO gs_lifnr.
        APPEND   gs_lifnr                     TO gt_lifnr.

      WHEN     'LFM1'.

        CLEAR                                    gs_lfm1_key.
        MOVE     <fs_editpos>-tabkey+03(10)   TO gs_lfm1_key-lifnr.
        MOVE     <fs_editpos>-tabkey+13(04)   TO gs_lfm1_key-ekorg.
        MOVE     <fs_editpos>-chngind         TO gs_lfm1_key-chngind.
        APPEND   gs_lfm1_key                  TO gt_lfm1_key.
        CLEAR                                    gs_lifnr.
        MOVE     <fs_editpos>-objectid+00(10) TO gs_lifnr.
        APPEND   gs_lifnr                     TO gt_lifnr.

      WHEN     'WYT3'.

        CLEAR                                    gs_wyt3_key.
        MOVE     <fs_editpos>-tabkey+03(10)   TO gs_wyt3_key-lifnr.
        MOVE     <fs_editpos>-tabkey+13(04)   TO gs_wyt3_key-ekorg.
        MOVE     <fs_editpos>-tabkey+27(02)   TO gs_wyt3_key-parvw.
        MOVE     <fs_editpos>-tabkey+29(03)   TO gs_wyt3_key-parza.
        MOVE     <fs_editpos>-chngind         TO gs_wyt3_key-chngind.
        APPEND   gs_wyt3_key                  TO gt_wyt3_key.
        CLEAR                                    gs_lifnr.
        MOVE     <fs_editpos>-objectid+00(10) TO gs_lifnr.
        APPEND   gs_lifnr                     TO gt_lifnr.

      WHEN     'KNVK'.

        CLEAR                                    gs_lfa1_key.
        MOVE     <fs_editpos>-objectid+00(10) TO gs_lfa1_key-lifnr.
        MOVE     '~'                          TO gs_lfa1_key-chngind.
        APPEND   gs_lfa1_key                  TO gt_lfa1_key.
        CLEAR                                    gs_lifnr.
        MOVE     <fs_editpos>-objectid+00(10) TO gs_lifnr.
        APPEND   gs_lifnr                     TO gt_lifnr.

      WHEN     OTHERS.
    ENDCASE.

  ENDLOOP.

*eject
* Build object keys for object class "BCHAINP"
  CLEAR    lt_editpos[].

  CALL FUNCTION 'CHANGEDOCUMENT_READ'
    EXPORTING
      DATE_OF_CHANGE             = s_date-low
      OBJECTCLASS                = 'BCHAINP'
      DATE_UNTIL                 = s_date-high
    TABLES
      EDITPOS                    = lt_editpos
    EXCEPTIONS
      NO_POSITION_FOUND          = 1
      WRONG_ACCESS_TO_ARCHIVE    = 2
      TIME_ZONE_CONVERSION_ERROR = 3
      OTHERS                     = 4.

  IF     ( sy-subrc EQ 0 ).

    SORT     lt_editpos                  BY objectid  ASCENDING
                                            tabname   ASCENDING
                                            tabkey254 ASCENDING
                                            udate     DESCENDING
                                            utime     DESCENDING.

    DELETE   ADJACENT DUPLICATES       FROM lt_editpos
                                  COMPARING objectid
                                            tabname
                                            tabkey254.

  ELSE.
* Implement suitable error handling here
  ENDIF.

*eject
  LOOP AT  lt_editpos             ASSIGNING <fs_editpos>.

    IF     ( <fs_editpos>-tabname        NE 'TBCHAIN21' ).
      CONTINUE.
    ENDIF.

    CLEAR                                   gs_tbchain21_key.
    MOVE   <fs_editpos>-tabkey254+03(03) TO gs_tbchain21_key-banksrec.
    MOVE   <fs_editpos>-tabkey254+06(15) TO gs_tbchain21_key-bankkrec.
    MOVE   <fs_editpos>-tabkey254+21(35) TO gs_tbchain21_key-banknrec.
    MOVE   <fs_editpos>-tabkey254+56(05) TO gs_tbchain21_key-waers.
    MOVE   <fs_editpos>-tabkey254+61(03) TO gs_tbchain21_key-bankssnd.
    MOVE   <fs_editpos>-tabkey254+64(15) TO gs_tbchain21_key-bankksnd.
    MOVE   <fs_editpos>-chngind          TO gs_tbchain21_key-chngind.
    APPEND gs_tbchain21_key              TO gt_tbchain21_key.

    CLEAR    gs_lfbk.
    SELECT   lifnr  banks  bankl  bankn
             bkont  bvtyp  bkref
      INTO   gs_lfbk
      FROM   lfbk
     WHERE   banks = gs_tbchain21_key-banksrec
       AND   bankl = gs_tbchain21_key-bankkrec
       AND   bankn = gs_tbchain21_key-banknrec.

      CLEAR                                 gs_lfbk_key.
      MOVE     gs_lfbk-lifnr             TO gs_lfbk_key-lifnr.
      MOVE     gs_lfbk-banks             TO gs_lfbk_key-banks.
      MOVE     gs_lfbk-bankl             TO gs_lfbk_key-bankl.
      MOVE     gs_lfbk-bankn             TO gs_lfbk_key-bankn.
      MOVE     '~'                       TO gs_lfbk_key-chngind.
      APPEND   gs_lfbk_key               TO gt_lfbk_key.
      CLEAR                                 gs_lifnr.
      MOVE     gs_lfbk-lifnr             TO gs_lifnr.
      APPEND   gs_lifnr                  TO gt_lifnr.

      CLEAR  gs_lfbk.
    ENDSELECT.

  ENDLOOP.

*eject
* Build object keys for object class "ADRESSE" - vendor address
  CLEAR    lr_tabname[].
  CLEAR                                     ls_tabname.
  MOVE     'I'                           TO ls_tabname-sign.
  MOVE     'EQ'                          TO ls_tabname-option.
  MOVE     'ADRC'                        TO ls_tabname-low.
  APPEND   ls_tabname                    TO lr_tabname.
  MOVE     'ADR2'                        TO ls_tabname-low.
  APPEND   ls_tabname                    TO lr_tabname.
  MOVE     'ADR3'                        TO ls_tabname-low.
  APPEND   ls_tabname                    TO lr_tabname.
  MOVE     'ADR6'                        TO ls_tabname-low.
  APPEND   ls_tabname                    TO lr_tabname.
  SORT     lr_tabname          ASCENDING BY low.

  CLEAR    lt_editpos[].

  CALL FUNCTION 'CHANGEDOCUMENT_READ'
    EXPORTING
      DATE_OF_CHANGE             = s_date-low
      OBJECTCLASS                = 'ADRESSE'
      DATE_UNTIL                 = s_date-high
    TABLES
      EDITPOS                    = lt_editpos
    EXCEPTIONS
      NO_POSITION_FOUND          = 1
      WRONG_ACCESS_TO_ARCHIVE    = 2
      TIME_ZONE_CONVERSION_ERROR = 3
      OTHERS                     = 4.

  IF     ( sy-subrc EQ 0 ).

    SORT     lt_editpos                  BY objectid  ASCENDING
                                            tabname   ASCENDING
                                            tabkey    ASCENDING
                                            udate     DESCENDING
                                            utime     DESCENDING.

    DELETE   ADJACENT DUPLICATES       FROM lt_editpos
                                  COMPARING objectid
                                            tabname
                                            tabkey.

  ELSE.
* Implement suitable error handling here
  ENDIF.

*eject
  LOOP AT  lt_editpos             ASSIGNING <fs_editpos>.

    IF     ( <fs_editpos>-tabname    NOT IN lr_tabname ).
      CONTINUE.
    ENDIF.

    CLEAR                                   lv_addrnumber.
    MOVE     <fs_editpos>-tabkey+03(10)  TO lv_addrnumber.

    CLEAR    gs_lifnr.
    SELECT   lifnr
      INTO   gs_lifnr
      FROM   lfa1
     WHERE   adrnr = lv_addrnumber.

      CLEAR                                 gs_lfa1_key.
      MOVE     gs_lifnr-lifnr            TO gs_lfa1_key-lifnr.
      MOVE     '~'                       TO gs_lfa1_key-chngind.
      APPEND   gs_lfa1_key               TO gt_lfa1_key.
      APPEND   gs_lifnr                  TO gt_lifnr.

      CLEAR  gs_lifnr.
    ENDSELECT.

  ENDLOOP.

*eject
* Build object keys for object class "ADRESSE3" - contact address
  CLEAR    lt_editpos[].

  CALL FUNCTION 'CHANGEDOCUMENT_READ'
    EXPORTING
      DATE_OF_CHANGE             = s_date-low
      OBJECTCLASS                = 'ADRESSE3'
      DATE_UNTIL                 = s_date-high
    TABLES
      EDITPOS                    = lt_editpos
    EXCEPTIONS
      NO_POSITION_FOUND          = 1
      WRONG_ACCESS_TO_ARCHIVE    = 2
      TIME_ZONE_CONVERSION_ERROR = 3
      OTHERS                     = 4.

  IF     ( sy-subrc EQ 0 ).

    SORT     lt_editpos                  BY objectid  ASCENDING
                                            tabname   ASCENDING
                                            tabkey    ASCENDING
                                            udate     DESCENDING
                                            utime     DESCENDING.

    DELETE   ADJACENT DUPLICATES       FROM lt_editpos
                                  COMPARING objectid
                                            tabname
                                            tabkey.

  ELSE.
* Implement suitable error handling here
  ENDIF.

*eject
  LOOP AT  lt_editpos             ASSIGNING <fs_editpos>.

    IF     ( <fs_editpos>-tabname    NOT IN lr_tabname ).
      CONTINUE.
    ENDIF.

    CLEAR                                   lv_addrnumber.
    MOVE     <fs_editpos>-tabkey+03(10)  TO lv_addrnumber.

    CLEAR    gs_lifnr.
    SELECT   lifnr
      INTO   gs_lifnr
      FROM   lfa1
     WHERE   adrnr = lv_addrnumber.

      CLEAR                                 gs_lfa1_key.
      MOVE     gs_lifnr-lifnr            TO gs_lfa1_key-lifnr.
      MOVE     '~'                       TO gs_lfa1_key-chngind.
      APPEND   gs_lfa1_key               TO gt_lfa1_key.
      APPEND   gs_lifnr                  TO gt_lifnr.

      CLEAR  gs_lifnr.
    ENDSELECT.

  ENDLOOP.

  IF     ( gt_lifnr[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     gt_lfa1_key         ASCENDING BY lifnr.
  DELETE   ADJACENT DUPLICATES         FROM gt_lfa1_key
                                  COMPARING lifnr.
  SORT     gt_lfbk_key         ASCENDING BY lifnr banks bankl bankn.
  DELETE   ADJACENT DUPLICATES         FROM gt_lfbk_key
                                  COMPARING lifnr banks bankl bankn.
  SORT     gt_lfb1_key         ASCENDING BY lifnr bukrs.
  DELETE   ADJACENT DUPLICATES         FROM gt_lfb1_key
                                  COMPARING lifnr bukrs.
  SORT     gt_lfza_key         ASCENDING BY lifnr bukrs empfk.
  DELETE   ADJACENT DUPLICATES         FROM gt_lfza_key
                                  COMPARING lifnr bukrs empfk.
  SORT     gt_lfbw_key         ASCENDING BY lifnr bukrs witht.
  DELETE   ADJACENT DUPLICATES         FROM gt_lfbw_key
                                  COMPARING lifnr bukrs witht.
  SORT     gt_lfm1_key         ASCENDING BY lifnr ekorg.
  DELETE   ADJACENT DUPLICATES         FROM gt_lfm1_key
                                  COMPARING lifnr ekorg.
  SORT     gt_wyt3_key         ASCENDING BY lifnr ekorg parvw parza.
  DELETE   ADJACENT DUPLICATES         FROM gt_wyt3_key
                                  COMPARING lifnr ekorg parvw parza.

*eject
  CLEAR    gt_lfa1[].
  CLEAR    gt_lfbk[].
* CLEAR    gt_tbchain21[]. do not remove the comment '*'
  CLEAR    gt_lfb1[].
  CLEAR    gt_lfza[].
  CLEAR    gt_lfbw[].
  CLEAR    gt_lfm1[].
  CLEAR    gt_wyt3[].
  CLEAR    gt_knvk[].
  CLEAR    gt_adrc[].
  CLEAR    gt_adr3[].
  CLEAR    gt_adr6[].

* Select lfa1
  IF     ( gt_lfa1_key[] IS NOT INITIAL ).

    SELECT   lifnr  land1  name1  name2  adrnr
             mcod1  mcod2  bahns  begru  konzs
             ktokk  lnrza  loevm  sperr  sperm
             stcd1  stcd2  telf1  telfx  sperz
             xlfza
*             zz_iap_vendor_id
      INTO   TABLE gt_lfa1
      FROM   lfa1 FOR ALL ENTRIES IN gt_lfa1_key
     WHERE   lifnr = gt_lfa1_key-lifnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfa1 ASCENDING BY lifnr.
    ELSE.
      CLEAR  gt_lfa1[].
    ENDIF.

  ENDIF.

*eject
* Select lfbk
  IF     ( gt_lfbk_key[] IS NOT INITIAL ).

    SELECT   lifnr  banks  bankl  bankn
             bkont  bvtyp  bkref
      INTO   TABLE gt_lfbk
      FROM   lfbk FOR ALL ENTRIES IN gt_lfbk_key
     WHERE   lifnr = gt_lfbk_key-lifnr
       AND   banks = gt_lfbk_key-banks
       AND   bankl = gt_lfbk_key-bankl
       AND   bankn = gt_lfbk_key-bankn.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfbk ASCENDING BY lifnr banks bankl bankn.
    ELSE.
      CLEAR  gt_lfbk[].
    ENDIF.

    CLEAR                                   gs_lfbk_key.
    LOOP AT  gt_lfbk_key               INTO gs_lfbk_key.
      READ     TABLE gt_lfbk       WITH KEY lifnr = gs_lfbk_key-lifnr
                                            banks = gs_lfbk_key-banks
                                            bankl = gs_lfbk_key-bankl
                                            bankn = gs_lfbk_key-bankn
                              BINARY SEARCH
                     TRANSPORTING NO FIELDS.
      lv_subrc = sy-subrc.
      lv_tabix = sy-tabix.
      IF     ( lv_subrc NE 0 ).
        CLEAR                               gs_lfbk.
        MOVE     gs_lfbk_key-lifnr       TO gs_lfbk-lifnr.
        MOVE     gs_lfbk_key-banks       TO gs_lfbk-banks.
        MOVE     gs_lfbk_key-bankl       TO gs_lfbk-bankl.
        MOVE     gs_lfbk_key-bankn       TO gs_lfbk-bankn.
        MOVE     'D'                     TO gs_lfbk-chngind.
        INSERT   gs_lfbk               INTO gt_lfbk INDEX lv_tabix.
      ENDIF.

      CLEAR  gs_lfbk_key.
    ENDLOOP.

  ENDIF.

*eject
* Select tbchain21
  IF     ( gt_tbchain21[] IS INITIAL ).

    SELECT   *
      INTO   TABLE gt_tbchain21
      FROM   tbchain21.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_tbchain21 ASCENDING BY mandt banksrec bankkrec
                                       banknrec waers.
    ELSE.
      CLEAR  gt_tbchain21[].
    ENDIF.

  ENDIF.

* Select lfb1
  IF     ( gt_lfb1_key[] IS NOT INITIAL ).

    SELECT   lifnr  bukrs  sperr  loevm  zuawa
             akont  zwels  zahls  zterm  fdgrv
             busab  lnrzb  zindt  reprf  hbkid
             xpore  qsskz  mindk  uzawe  qsrec
             qland  xlfzb  cerdt
      INTO   TABLE gt_lfb1
      FROM   lfb1 FOR ALL ENTRIES IN gt_lfb1_key
     WHERE   lifnr = gt_lfb1_key-lifnr
       AND   bukrs = gt_lfb1_key-bukrs.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfb1 ASCENDING BY lifnr bukrs.
    ELSE.
      CLEAR  gt_lfb1[].
    ENDIF.

  ENDIF.

*eject
* Select lfza
  IF     ( gt_lfza_key[] IS NOT INITIAL ).

    SELECT   lifnr  bukrs  empfk
      INTO   TABLE gt_lfza
      FROM   lfza FOR ALL ENTRIES IN gt_lfza_key
     WHERE   lifnr = gt_lfza_key-lifnr
       AND   bukrs = gt_lfza_key-bukrs
       AND   empfk = gt_lfza_key-empfk.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfza ASCENDING BY lifnr bukrs empfk.
    ELSE.
      CLEAR  gt_lfza[].
    ENDIF.

    CLEAR                                   gs_lfza_key.
    LOOP AT  gt_lfza_key               INTO gs_lfza_key.
      READ     TABLE gt_lfza       WITH KEY lifnr = gs_lfza_key-lifnr
                                            bukrs = gs_lfza_key-bukrs
                                            empfk = gs_lfza_key-empfk
                              BINARY SEARCH
                     TRANSPORTING NO FIELDS.
      lv_subrc = sy-subrc.
      lv_tabix = sy-tabix.
      IF     ( lv_subrc NE 0 ).
        CLEAR                               gs_lfza.
        MOVE     gs_lfza_key-lifnr       TO gs_lfza-lifnr.
        MOVE     gs_lfza_key-bukrs       TO gs_lfza-bukrs.
        MOVE     gs_lfza_key-empfk       TO gs_lfza-empfk.
        MOVE     'D'                     TO gs_lfza-chngind.
        INSERT   gs_lfza               INTO gt_lfza INDEX lv_tabix.
      ENDIF.

      CLEAR  gs_lfza_key.
    ENDLOOP.

  ENDIF.

*eject
* Select lfbw
  IF     ( gt_lfbw_key[] IS NOT INITIAL ).

    SELECT   lifnr  bukrs  witht  wt_subjct
             qsrec  wt_wtstcd     wt_withcd
      INTO   TABLE gt_lfbw
      FROM   lfbw FOR ALL ENTRIES IN gt_lfbw_key
     WHERE   lifnr = gt_lfbw_key-lifnr
       AND   bukrs = gt_lfbw_key-bukrs
       AND   witht = gt_lfbw_key-witht.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfbw ASCENDING BY lifnr bukrs witht.
    ELSE.
      CLEAR  gt_lfbw[].
    ENDIF.

    CLEAR                                   gs_lfbw_key.
    LOOP AT  gt_lfbw_key               INTO gs_lfbw_key.
      READ     TABLE gt_lfbw       WITH KEY lifnr = gs_lfbw_key-lifnr
                                            bukrs = gs_lfbw_key-bukrs
                                            witht = gs_lfbw_key-witht
                              BINARY SEARCH
                     TRANSPORTING NO FIELDS.
      lv_subrc = sy-subrc.
      lv_tabix = sy-tabix.
      IF     ( lv_subrc NE 0 ).
        CLEAR                               gs_lfbw.
        MOVE     gs_lfbw_key-lifnr       TO gs_lfbw-lifnr.
        MOVE     gs_lfbw_key-bukrs       TO gs_lfbw-bukrs.
        MOVE     gs_lfbw_key-witht       TO gs_lfbw-witht.
        MOVE     'D'                     TO gs_lfbw-chngind.
        INSERT   gs_lfbw               INTO gt_lfbw INDEX lv_tabix.
      ENDIF.

      CLEAR  gs_lfbw_key.
    ENDLOOP.

  ENDIF.

* Select lfm1
  IF     ( gt_lfm1_key[] IS NOT INITIAL ).

    SELECT   lifnr  ekorg  sperm  loevm  waers
             zterm  inco1  inco2  webre  kalsk
             kzaut  meprf  eikto
      INTO   TABLE gt_lfm1
      FROM   lfm1 FOR ALL ENTRIES IN gt_lfm1_key
     WHERE   lifnr = gt_lfm1_key-lifnr
       AND   ekorg = gt_lfm1_key-ekorg.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfm1 ASCENDING BY lifnr ekorg.
    ELSE.
      CLEAR  gt_lfm1[].
    ENDIF.

  ENDIF.

*eject
* Select wyt3
  IF     ( gt_wyt3_key[] IS NOT INITIAL ).

    SELECT   lifnr  ekorg  parvw
             parza  lifn2  defpa
      INTO   TABLE gt_wyt3
      FROM   wyt3 FOR ALL ENTRIES IN gt_wyt3_key
     WHERE   lifnr = gt_wyt3_key-lifnr
       AND   ekorg = gt_wyt3_key-ekorg
       AND   parvw = gt_wyt3_key-parvw
       AND   parza = gt_wyt3_key-parza.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_wyt3 ASCENDING BY lifnr ekorg parvw parza.
    ELSE.
      CLEAR  gt_wyt3[].
    ENDIF.

    CLEAR                                   gs_wyt3_key.
    LOOP AT  gt_wyt3_key               INTO gs_wyt3_key.
      READ     TABLE gt_wyt3       WITH KEY lifnr = gs_wyt3_key-lifnr
                                            ekorg = gs_wyt3_key-ekorg
                                            parvw = gs_wyt3_key-parvw
                                            parza = gs_wyt3_key-parza
                              BINARY SEARCH
                     TRANSPORTING NO FIELDS.
      lv_subrc = sy-subrc.
      lv_tabix = sy-tabix.
      IF     ( lv_subrc NE 0 ).
        CLEAR                               gs_wyt3.
        MOVE     gs_wyt3_key-lifnr       TO gs_wyt3-lifnr.
        MOVE     gs_wyt3_key-ekorg       TO gs_wyt3-ekorg.
        MOVE     gs_wyt3_key-parvw       TO gs_wyt3-parvw.
        MOVE     gs_wyt3_key-parza       TO gs_wyt3-parza.
        MOVE     'D'                     TO gs_wyt3-chngind.
        INSERT   gs_wyt3               INTO gt_wyt3 INDEX lv_tabix.
      ENDIF.

      CLEAR  gs_wyt3_key.
    ENDLOOP.

  ENDIF.

*eject
  IF     ( gt_lfa1[] IS NOT INITIAL ).

* Select knvk
    SELECT   parnr  namev  name1  abtnr
             telf1  pafkt  lifnr  prsnr
      INTO   TABLE gt_knvk
      FROM   knvk FOR ALL ENTRIES IN gt_lfa1
     WHERE   lifnr = gt_lfa1-lifnr.
    IF     ( sy-subrc EQ 0 ).
      SORT     gt_knvk ASCENDING BY lifnr parnr.
    ELSE.
      CLEAR    gt_knvk[].
    ENDIF.

* Select adrc
    SELECT   addrnumber  name1       city1
             post_code1  street      str_suppl2
             country     region
      INTO   TABLE gt_adrc
      FROM   adrc FOR ALL ENTRIES IN gt_lfa1
     WHERE   addrnumber = gt_lfa1-adrnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_adrc ASCENDING BY addrnumber.
    ELSE.
      CLEAR  gt_adrc[].
    ENDIF.

* Select adr2
    SELECT   addrnumber  persnumber  tel_number  tel_extens
      INTO   TABLE gt_adr2
      FROM   adr2 FOR ALL ENTRIES IN gt_lfa1
     WHERE   addrnumber = gt_lfa1-adrnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_adr2 ASCENDING BY addrnumber persnumber.
    ELSE.
      CLEAR  gt_adr2[].
    ENDIF.

* Select adr3
    SELECT   addrnumber  persnumber  fax_number  fax_extens  faxnr_long
      INTO   TABLE gt_adr3
      FROM   adr3 FOR ALL ENTRIES IN gt_lfa1
     WHERE   addrnumber = gt_lfa1-adrnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_adr3 ASCENDING BY addrnumber persnumber.
    ELSE.
      CLEAR  gt_adr3[].
    ENDIF.

*eject
* Select adr6
    SELECT   addrnumber  persnumber  smtp_addr
      INTO   TABLE gt_adr6
      FROM   adr6 FOR ALL ENTRIES IN gt_lfa1
     WHERE   addrnumber = gt_lfa1-adrnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_adr6 ASCENDING BY addrnumber persnumber.
    ELSE.
      CLEAR  gt_adr6[].
    ENDIF.

  ENDIF.

* Process the records
  SORT     gt_lifnr ASCENDING            BY lifnr.
  DELETE   ADJACENT DUPLICATES         FROM gt_lifnr
                                  COMPARING lifnr.

  CLEAR    lt_lfa1_key[].
  SELECT   lifnr    ktokk    loevm    sperr
*           zz_iap_vendor_id  zz_iap_ritm_id
    INTO   TABLE lt_lfa1_key
    FROM   lfa1 FOR ALL ENTRIES IN gt_lifnr
   WHERE   lifnr  = gt_lifnr-lifnr
     AND   ktokk IN s_ktokk.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  PERFORM  f_process_records         TABLES lt_lfa1_key.

ENDFORM.                    " F_GET_DELTA_EXTRACT
