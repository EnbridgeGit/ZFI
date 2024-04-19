*&---------------------------------------------------------------------*
*&  Include           ZFAPI126_PURCHASE_ORDER_F01
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
*& Program Name       :  ZFAPI126_PURCHASE_ORDER                       *
*& Include            :  ZFAPI126_PURCHASE_ORDER_F01                   *
*& Author             :  Paul Karunakar                                *
*& Creation Date      :  02-Apr-2018                                   *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Purchase Order data from each of the three    *
*&                       SAP instances will be extracted in a          *
*&                       delimited file and sent to IAP.               *
*&                                                                     *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 02-Apr-2018  KPAL        D30K928646  CHG0107206 Initial Development  *
*                          D30K928846, D30K928994                      *
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_filepath_filename
*&---------------------------------------------------------------------*
*       Get the application server filepath and filename
*----------------------------------------------------------------------*
FORM f_get_filepath_filename
  CHANGING cv_filepath  TYPE string
           cv_filename  TYPE string.

  CLEAR    cv_filepath.
  CLEAR    cv_filename.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = gc_lgcfilpth
    IMPORTING
      file_name        = cv_filepath
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  IF     ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE     gc_filename    TO cv_filename.

ENDFORM.                    " f_get_filepath_filename
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_f4_help_file_path
*&---------------------------------------------------------------------*
FORM f_get_f4_help_file_path.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = p_path2
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF     ( sy-subrc NE 0 ).
    MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_get_f4_help_file_path
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_f4_help_file_path1
*&---------------------------------------------------------------------*
FORM f_get_f4_help_file_path1.

  DATA: lv_repid                       TYPE sy-repid,
        lv_dynnr                       TYPE sy-dynnr,
        lv_file                        TYPE rlgrap-filename.

  CLEAR                                     lv_repid.
  MOVE     sy-repid                      TO lv_repid.
  CLEAR                                     lv_dynnr.
  MOVE     sy-dynnr                      TO lv_dynnr.

  CLEAR    lv_file.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = lv_repid
      dynpro_number = lv_dynnr
    CHANGING
      file_name     = lv_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF     ( sy-subrc EQ 0 ).

    MOVE   lv_file   TO p_path1.

  ELSE.

    MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.                    " f_get_f4_help_file_path1
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  DATA:    lv_filename                 TYPE text128,
           lv_date_time_stamp          TYPE char10.

  CLEAR    gt_t006a[].
  CLEAR    gt_ebeln[].
  CLEAR    gt_output[].

  CLEAR    gv_cn_recs_total.
  CLEAR    gv_cn_recs_file.
  CLEAR    gv_cn_files.
  CLEAR    gv_filename.
  CLEAR    gv_filename_p.

* Set the maximum number of records in a file
  gv_cn_recs_max = p_maxrec.

* Set the filename
  IF   ( ( rb_appl                       IS NOT INITIAL ) AND
         ( p_file2                       IS NOT INITIAL )     ).

    CLEAR                                   lv_filename.
    MOVE       p_file2                   TO lv_filename.

    IF     ( ( lv_filename               CS 'SSSSS'     ) AND
             ( p_erpid                   IS NOT INITIAL )     ).
      REPLACE  'SSSSS'                   IN lv_filename
                                       WITH p_erpid.
    ENDIF.

    IF       ( lv_filename               CS 'YYMMDDHHMM' ).
      CLEAR                                 lv_date_time_stamp.
      MOVE     sy-datum+2(6)             TO lv_date_time_stamp+0(6).
      MOVE     sy-uzeit+0(4)             TO lv_date_time_stamp+6(4).
      REPLACE  'YYMMDDHHMM'              IN lv_filename
                                       WITH lv_date_time_stamp.
    ENDIF.

    MOVE       lv_filename               TO gv_filename_p.

  ENDIF.

*eject
* Units of measure
  SELECT   msehi  mseh3
    INTO   TABLE gt_t006a
    FROM   t006a
   WHERE   spras = 'E'.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_t006a ASCENDING BY msehi.
  ELSE.
    CLEAR  gt_t006a[].
  ENDIF.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_main
*&---------------------------------------------------------------------*
*       Main process
*----------------------------------------------------------------------*
FORM f_process_main.

  DATA:    lv_cn_recs   TYPE char12.

* Open the application server dataset
  IF       ( rb_appl      IS NOT INITIAL ).
    PERFORM  f_open_dataset.
  ELSEIF   ( rb_pres      IS NOT INITIAL ).
    PERFORM  f_create_header_rec.
  ENDIF.

* Process the purchase orders
  PERFORM  f_process_purchase_orders.

* Close the application server dataset
  IF       ( rb_appl      IS NOT INITIAL ).
    IF     ( gv_filename  IS NOT INITIAL ).
      CLOSE  DATASET gv_filename.
    ENDIF.
  ENDIF.

* Download the output data
  IF       ( rb_pres      IS NOT INITIAL ).
    PERFORM  f_download_output.
  ENDIF.

* Write the record counts to the spool
  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_total              TO lv_cn_recs.
  WRITE:  /001 text-061,                035 lv_cn_recs.

ENDFORM.                    " f_process_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_purchase_orders
*&---------------------------------------------------------------------*
*       Process the purchase orders
*----------------------------------------------------------------------*
FORM f_process_purchase_orders.

  DATA:    lt_ebeln                    TYPE gtt_ebeln,
           ls_ebeln                    TYPE gty_ebeln,
           lt_packno                   TYPE gtt_packno,
           ls_packno                   TYPE gty_packno,
           lt_editpos                  TYPE gtt_editpos.

  DATA:    lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS: lc_blocksize              TYPE syindex VALUE 250.

  FIELD-SYMBOLS: <fs_editpos>          TYPE gty_editpos.

  CLEAR    lt_ebeln[].
  CLEAR    lt_packno[].
  CLEAR    lt_editpos[].

  IF     ( rb_fload IS NOT INITIAL ).

    SELECT   ebeln
      INTO   TABLE gt_ebeln
      FROM   ekko
     WHERE   ebeln IN s_ebeln
       AND   bsart IN s_bsart
       AND   bukrs IN s_bukrs
       AND   aedat in s_aedat.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_ebeln[].
    ENDIF.

*eject
  ELSEIF ( rb_dload IS NOT INITIAL ).

* Change in purchase order (step 1)
    CLEAR    lt_editpos[].

    CALL FUNCTION 'CHANGEDOCUMENT_READ'
      EXPORTING
        date_of_change             = s_date-low
        objectclass                = 'EINKBELEG'
        date_until                 = s_date-high
      TABLES
        editpos                    = lt_editpos
      EXCEPTIONS
        no_position_found          = 1
        wrong_access_to_archive    = 2
        time_zone_conversion_error = 3
        OTHERS                     = 4.

    IF     ( sy-subrc EQ 0 ).

      LOOP AT  lt_editpos         ASSIGNING <fs_editpos>.
        IF     ( ( <fs_editpos>-tabname  EQ 'EKKO' ) OR
                 ( <fs_editpos>-tabname  EQ 'EKPO' ) OR
                 ( <fs_editpos>-tabname  EQ 'EKKN' ) OR
                 ( <fs_editpos>-tabname  EQ 'EKPA' ) OR
                 ( <fs_editpos>-tabname  EQ 'EKET' )    ).
          CLEAR                             ls_ebeln.
          MOVE     <fs_editpos>-objectid(10)
                                         TO ls_ebeln-ebeln.
          APPEND   ls_ebeln              TO gt_ebeln.
        ENDIF.
      ENDLOOP.

    ENDIF.

*eject
* Change in service entry sheet (step 2)
    CLEAR    lt_editpos[].

    CALL FUNCTION 'CHANGEDOCUMENT_READ'
      EXPORTING
        date_of_change             = s_date-low
        objectclass                = 'MM_SERVICE'
        date_until                 = s_date-high
      TABLES
        editpos                    = lt_editpos
      EXCEPTIONS
        no_position_found          = 1
        wrong_access_to_archive    = 2
        time_zone_conversion_error = 3
        OTHERS                     = 4.

    IF     ( sy-subrc EQ 0 ).

      CLEAR    lt_ebeln[].
      CLEAR    lt_packno[].

      LOOP AT  lt_editpos         ASSIGNING <fs_editpos>.
        IF       ( <fs_editpos>-tabname  EQ 'ESUH' ).
          CLEAR                             ls_packno.
          MOVE     <fs_editpos>-objectid(10)
                                         TO ls_packno-packno.
          APPEND   ls_packno             TO lt_packno.
        ENDIF.
      ENDLOOP.

      IF     ( lt_packno[] IS NOT INITIAL ).

        SORT     lt_packno     ASCENDING BY packno.
        DELETE   ADJACENT DUPLICATES   FROM lt_packno
                                  COMPARING packno.

        SELECT   ebeln
          INTO   TABLE lt_ebeln
          FROM   ekpo FOR ALL ENTRIES IN lt_packno
         WHERE   packno = lt_packno-packno.
        IF     ( sy-subrc EQ 0 ).
          APPEND   LINES OF lt_ebeln     TO gt_ebeln.
        ENDIF.

      ENDIF.

    ENDIF.

*eject
* Change in purchase order history (step 3)
    CLEAR    lt_ebeln[].

    SELECT   ebeln
      INTO   TABLE lt_ebeln
      FROM   ekbe
     WHERE   cpudt IN s_date.
    IF     ( sy-subrc EQ 0 ).
      APPEND   LINES OF lt_ebeln         TO gt_ebeln.
    ENDIF.

  ENDIF.

  SORT     gt_ebeln            ASCENDING BY ebeln.
  DELETE   ADJACENT DUPLICATES         FROM gt_ebeln
                                  COMPARING ebeln.

* Process the purchasing documents in batches
  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch
    CLEAR             lt_ebeln[].
    APPEND   LINES OF gt_ebeln
                 FROM lv_index_lo
                   TO lv_index_hi
                   TO lt_ebeln.

    IF     ( lt_ebeln[] IS INITIAL ).
      EXIT.
    ENDIF.

    PERFORM  f_process_purch_order_batch  TABLES lt_ebeln.

  ENDDO.

ENDFORM.                    " f_process_purchase_orders
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_purch_order_batch
*&---------------------------------------------------------------------*
*       Process the batch of purchase orders
*----------------------------------------------------------------------*
FORM f_process_purch_order_batch
  TABLES   it_ebeln                    TYPE gtt_ebeln.

  DATA:    lt_ebeln                    TYPE gtt_ebeln,
           ls_ebeln                    TYPE gty_ebeln,
           lt_packno                   TYPE gtt_packno,
           ls_packno                   TYPE gty_packno,
           ls_ekko                     TYPE gty_ekko,
           ls_esuh                     TYPE gty_esuh,
           ls_t006a                    TYPE gty_t006a,
           lt_return                   TYPE gtt_return,
           ls_return                   TYPE gty_return,
           lt_po_ttls                  TYPE gtt_po_ttls,
           ls_po_ttls                  TYPE gty_po_ttls,
           ls_final                    TYPE gty_final,
           lt_output                   TYPE gtt_output,
           ls_output                   TYPE gty_output.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_sumlimit                 TYPE char16,
           lv_actvalue                 TYPE char16,
           lv_commitment               TYPE char16,
           lv_menge                    TYPE char17,
           lv_meins                    TYPE char3,
           lv_netpr                    TYPE char16,
           lv_netwr                    TYPE char16,
           lv_brtwr                    TYPE char16,
           lv_vproz                    TYPE char5,
           lv_paobjnr                  TYPE char10,
           lv_ps_psp_pnr               TYPE char24,
           lv_vornr                    TYPE vornr,
           lv_qty_out_ordr             TYPE bstmg,
           lv_qty_out_ordr_c           TYPE char17,
           lv_qty_out_invc             TYPE bstmg,
           lv_qty_out_invc_c           TYPE char17,
           lv_amt_out_cmtm             TYPE bprei,
           lv_amt_out_cmtm_c           TYPE char16.

  CONSTANTS: lc_00                     TYPE numc2 VALUE '00',
             lc_01                     TYPE numc2 VALUE '01'.

  FIELD-SYMBOLS: <fs_ekpo>             TYPE gty_ekpo,
                 <fs_ekkn>             TYPE gty_ekkn.

*eject
  IF     ( it_ebeln[] IS INITIAL ).
    RETURN.
  ENDIF.

* Select the PO data
  CLEAR    gt_ekko[].
  CLEAR    gt_ekpo[].
  CLEAR    gt_ekkn[].
  CLEAR    gt_esuh[].
  CLEAR    lt_packno[].

  SELECT   ebeln  bukrs  bstyp  bsart  loekz  statu
           aedat  ernam  lifnr  zterm  ekorg  ekgrp
           waers  bedat  llief  lifre  memorytype
           zzariba_approver
    INTO   TABLE gt_ekko
    FROM   ekko FOR ALL ENTRIES IN it_ebeln
   WHERE   ebeln = it_ebeln-ebeln.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SELECT   ebeln  ebelp  loekz  statu  aedat  txz01
           matnr  bukrs  werks  lgort  menge  meins
           bprme  netpr  netwr  brtwr  mwskz  pstyp
           knttp  vrtkz  wepos  webre  txjcd  packno
           xersy  afnam
    INTO   TABLE gt_ekpo
    FROM   ekpo FOR ALL ENTRIES IN it_ebeln
   WHERE   ebeln = it_ebeln-ebeln.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_ekpo[].
  ENDIF.

  SELECT   ebeln  ebelp  zekkn  menge  vproz  sakto
           gsber  kostl  vbeln  vbelp  anln1  anln2
           aufnr  wempf  kokrs  paobjnr       prctr
           ps_psp_pnr    nplnr  aufpl  aplzl  lstar
    INTO   TABLE gt_ekkn
    FROM   ekkn FOR ALL ENTRIES IN it_ebeln
   WHERE   ebeln = it_ebeln-ebeln.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_ekkn[].
  ENDIF.

*eject
* Select the service entry sheets
  LOOP AT  gt_ekpo                ASSIGNING <fs_ekpo>.
    IF       ( <fs_ekpo>-packno          IS NOT INITIAL ).
      CLEAR                                 ls_packno.
      MOVE     <fs_ekpo>-packno          TO ls_packno-packno.
      APPEND   ls_packno                 TO lt_packno.
    ENDIF.
  ENDLOOP.

  IF     ( lt_packno[] IS NOT INITIAL ).

    SORT     lt_packno         ASCENDING BY packno.
    DELETE   ADJACENT DUPLICATES       FROM lt_packno
                                  COMPARING packno.

    SELECT   packno  sumlimit  commitment  actvalue  waers
      INTO   TABLE gt_esuh
      FROM   esuh FOR ALL ENTRIES IN lt_packno
     WHERE   packno = lt_packno-packno.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_esuh[].
    ENDIF.

  ENDIF.

  SORT     gt_ekko             ASCENDING BY ebeln.
  DELETE   ADJACENT DUPLICATES         FROM gt_ekko
                                  COMPARING ebeln.

  SORT     gt_ekpo             ASCENDING BY ebeln ebelp.
  DELETE   ADJACENT DUPLICATES         FROM gt_ekpo
                                  COMPARING ebeln ebelp.

  SORT     gt_ekkn             ASCENDING BY ebeln ebelp zekkn.
  DELETE   ADJACENT DUPLICATES         FROM gt_ekkn
                                  COMPARING ebeln ebelp zekkn.

  SORT     gt_esuh             ASCENDING BY packno.
  DELETE   ADJACENT DUPLICATES         FROM gt_esuh
                                  COMPARING packno.

*eject
* Process the data by purchasing document
  LOOP AT  gt_ekpo                ASSIGNING <fs_ekpo>.

    AT NEW ebeln.

      CLEAR    lt_output[].
      CLEAR    lt_return[].
      CLEAR    lt_po_ttls[].

      CLEAR                                 ls_ekko.
      READ     TABLE gt_ekko           INTO ls_ekko
                                   WITH KEY ebeln = <fs_ekpo>-ebeln
                              BINARY SEARCH.
      IF     ( sy-subrc EQ 0 ).

        CALL FUNCTION 'BAPI_PO_GETDETAIL1'
          EXPORTING
            PURCHASEORDER    = <fs_ekpo>-ebeln
          TABLES
            RETURN           = lt_return
            POHISTORY_TOTALS = lt_po_ttls.

        CLEAR                               ls_return.
        READ     TABLE lt_return       INTO ls_return
                                   WITH KEY type = 'E'.
        IF     ( sy-subrc EQ 0 ).
          CLEAR  lt_po_ttls[].
        ENDIF.

      ELSE.
        CLEAR  ls_ekko.
      ENDIF.

    ENDAT.

    IF     ( ls_ekko-ebeln IS INITIAL ).
      CONTINUE.
    ENDIF.

*eject
* Read the service entry sheet
    CLEAR    ls_esuh.

    IF     ( <fs_ekpo>-packno            IS NOT INITIAL ).

      READ     TABLE gt_esuh           INTO ls_esuh
                                   WITH KEY packno = <fs_ekpo>-packno
                              BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CLEAR  ls_esuh.
      ENDIF.

    ENDIF.

* Read the PO item totals
    CLEAR    ls_po_ttls.

    IF     ( lt_po_ttls[]                IS NOT INITIAL ).

      READ     TABLE lt_po_ttls        INTO ls_po_ttls
                                   WITH KEY po_item   = <fs_ekpo>-ebelp
                                            serial_no = lc_00.
      IF     ( sy-subrc NE 0 ).
        CLEAR                               ls_po_ttls.
        READ   TABLE lt_po_ttls        INTO ls_po_ttls
                                   WITH KEY po_item   = <fs_ekpo>-ebelp
                                            serial_no = lc_01.
        IF   ( sy-subrc NE 0 ).
          CLEAR  ls_po_ttls.
        ENDIF.
      ENDIF.

    ENDIF.

*eject
    IF     ( ls_esuh-packno              IS NOT INITIAL ).

* Overall limit
      CLEAR                                 lv_sumlimit.
      WRITE    ls_esuh-sumlimit          TO lv_sumlimit
                                   CURRENCY ls_esuh-waers.
      TRANSLATE                             lv_sumlimit USING ', - '.
      CONDENSE                              lv_sumlimit NO-GAPS.
      IF     ( ls_esuh-sumlimit          LT 0 ).
        CONCATENATE '-' lv_sumlimit    INTO lv_sumlimit.
      ENDIF.

* Actual value
      CLEAR                                 lv_actvalue.
      WRITE    ls_esuh-actvalue          TO lv_actvalue
                                   CURRENCY ls_esuh-waers.
      TRANSLATE                             lv_actvalue USING ', - '.
      CONDENSE                              lv_actvalue NO-GAPS.
      IF     ( ls_esuh-actvalue          LT 0 ).
        CONCATENATE '-' lv_actvalue    INTO lv_actvalue.
      ENDIF.

* Expected value
      CLEAR                                 lv_commitment.
      WRITE    ls_esuh-commitment        TO lv_commitment
                                   CURRENCY ls_esuh-waers.
      TRANSLATE                             lv_commitment USING ', - '.
      CONDENSE                              lv_commitment NO-GAPS.
      IF     ( ls_esuh-commitment        LT 0 ).
        CONCATENATE '-' lv_commitment  INTO lv_commitment.
      ENDIF.

    ENDIF.

*eject
* Quantity
    CLEAR                                   lv_menge.
    WRITE    <fs_ekpo>-menge             TO lv_menge
                                       UNIT <fs_ekpo>-meins.
    TRANSLATE                               lv_menge USING ', - '.
    CONDENSE                                lv_menge NO-GAPS.
    IF     ( <fs_ekpo>-menge             LT 0 ).
      CONCATENATE '-' lv_menge         INTO lv_menge.
    ENDIF.

* Units of measure
    CLEAR    lv_meins.

    IF     ( ls_t006a-msehi              NE <fs_ekpo>-meins ).
      CLEAR                                 ls_t006a.
      READ     TABLE gt_t006a          INTO ls_t006a
                                    WITH KEY msehi = <fs_ekpo>-meins
                               BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CLEAR  ls_t006a.
      ENDIF.
    ENDIF.

    MOVE     ls_t006a-mseh3              TO lv_meins.


* Net order price
    CLEAR                                   lv_netpr.
    WRITE    <fs_ekpo>-netpr             TO lv_netpr
                                   CURRENCY ls_ekko-waers.
    TRANSLATE                               lv_netpr USING ', - '.
    CONDENSE                                lv_netpr NO-GAPS.
    IF     ( <fs_ekpo>-netpr             LT 0 ).
      CONCATENATE '-' lv_netpr         INTO lv_netpr.
    ENDIF.

* Net order value
    CLEAR                                   lv_netwr.
    WRITE    <fs_ekpo>-netwr             TO lv_netwr
                                   CURRENCY ls_ekko-waers.
    TRANSLATE                               lv_netwr USING ', - '.
    CONDENSE                                lv_netwr NO-GAPS.
    IF     ( <fs_ekpo>-netwr             LT 0 ).
      CONCATENATE '-' lv_netwr         INTO lv_netwr.
    ENDIF.

* Gross order value
    CLEAR                                   lv_brtwr.
    WRITE    <fs_ekpo>-brtwr             TO lv_brtwr
                                   CURRENCY ls_ekko-waers.
    TRANSLATE                               lv_brtwr USING ', - '.
    CONDENSE                                lv_brtwr NO-GAPS.
    IF     ( <fs_ekpo>-brtwr             LT 0 ).
      CONCATENATE '-' lv_brtwr         INTO lv_brtwr.
    ENDIF.

*eject
* Outstanding order quantity
    CLEAR                                   lv_qty_out_ordr.
    ADD        <fs_ekpo>-menge           TO lv_qty_out_ordr.
    SUBTRACT   ls_po_ttls-deliv_qty    FROM lv_qty_out_ordr.
    IF       ( lv_qty_out_ordr           LT 0 ).
      CLEAR                                 lv_qty_out_ordr.
    ENDIF.
    CLEAR                                   lv_qty_out_ordr_c.
    WRITE      lv_qty_out_ordr           TO lv_qty_out_ordr_c
                                       UNIT <fs_ekpo>-meins.
    TRANSLATE                               lv_qty_out_ordr_c
                                      USING ', - '.
    CONDENSE                                lv_qty_out_ordr_c NO-GAPS.

* Outstanding invoice quantity
    CLEAR                                   lv_qty_out_invc.
    ADD        <fs_ekpo>-menge           TO lv_qty_out_invc.
    SUBTRACT   ls_po_ttls-iv_qty_po    FROM lv_qty_out_invc.
    IF       ( lv_qty_out_invc           LT 0 ).
      CLEAR                                 lv_qty_out_invc.
    ENDIF.
    CLEAR                                   lv_qty_out_invc_c.
    WRITE      lv_qty_out_invc           TO lv_qty_out_invc_c
                                       UNIT <fs_ekpo>-meins.
    TRANSLATE                               lv_qty_out_invc_c
                                      USING ', - '.
    CONDENSE                                lv_qty_out_invc_c NO-GAPS.

* Outstanding commitment amount
    CLEAR                                   lv_amt_out_cmtm.
    ADD        <fs_ekpo>-netpr           TO lv_amt_out_cmtm.
    SUBTRACT   ls_po_ttls-val_iv_for   FROM lv_amt_out_cmtm.
    IF       ( lv_amt_out_cmtm           LT 0 ).
      CLEAR                                 lv_amt_out_cmtm.
    ENDIF.
    CLEAR                                   lv_amt_out_cmtm_c.
    WRITE      lv_amt_out_cmtm           TO lv_amt_out_cmtm_c
                                   CURRENCY ls_ekko-waers.
    TRANSLATE                               lv_amt_out_cmtm_c
                                      USING ', - '.
    CONDENSE                                lv_amt_out_cmtm_c NO-GAPS.

*eject
* Format the final data
    CLEAR                                   ls_final.
    MOVE     p_erpid                     TO ls_final-erpid.
    MOVE     ls_ekko-ebeln               TO ls_final-ebeln.
    MOVE     ls_ekko-lifnr               TO ls_final-lifnr.
    MOVE     ls_ekko-bsart               TO ls_final-bsart.
    MOVE     ls_ekko-bstyp               TO ls_final-bstyp.
    MOVE     ls_ekko-bukrs               TO ls_final-bukrs.
    MOVE     ls_ekko-ekorg               TO ls_final-ekorg.
    MOVE     ls_ekko-ekgrp               TO ls_final-ekgrp.
    MOVE     ls_ekko-zterm               TO ls_final-zterm.
    MOVE     ls_ekko-waers               TO ls_final-waers.

    CONCATENATE ls_ekko-aedat+0(4) '-'
                ls_ekko-aedat+4(2) '-'
                ls_ekko-aedat+6(2)     INTO ls_final-aedat_ekko.

    MOVE     ls_ekko-ernam               TO ls_final-ernam.

    CONCATENATE ls_ekko-bedat+0(4) '-'
                ls_ekko-bedat+4(2) '-'
                ls_ekko-bedat+6(2)     INTO ls_final-bedat.

    MOVE     ls_ekko-loekz               TO ls_final-loekz_ekko.
    MOVE     ls_ekko-llief               TO ls_final-llief.
    MOVE     ls_ekko-lifre               TO ls_final-lifre.
    MOVE     ls_ekko-zzariba_approver    TO ls_final-zzariba_approver.
    MOVE     ls_ekko-memorytype          TO ls_final-memorytype.
    MOVE     <fs_ekpo>-loekz             TO ls_final-loekz_ekpo.
    MOVE     lv_sumlimit                 TO ls_final-sumlimit.
    MOVE     lv_actvalue                 TO ls_final-actvalue.
    MOVE     lv_commitment               TO ls_final-commitment.
    MOVE     <fs_ekpo>-ebelp             TO ls_final-ebelp.

    CONCATENATE <fs_ekpo>-aedat+0(4) '-'
                <fs_ekpo>-aedat+4(2) '-'
                <fs_ekpo>-aedat+6(2)   INTO ls_final-aedat_ekpo.

*eject
    MOVE     <fs_ekpo>-matnr             TO ls_final-matnr.
    MOVE     <fs_ekpo>-bukrs             TO ls_final-bukrs_ekpo.
    MOVE     <fs_ekpo>-txz01             TO ls_final-txz01.
    MOVE     lv_menge                    TO ls_final-menge_ekpo.
    MOVE     lv_meins                    TO ls_final-meins.
    MOVE     <fs_ekpo>-werks             TO ls_final-werks.
    MOVE     <fs_ekpo>-lgort             TO ls_final-lgort.
    MOVE     lv_menge                    TO ls_final-menge_ekpo_oq.
    MOVE     lv_qty_out_ordr_c           TO ls_final-qty_out_ordr.
    MOVE     lv_qty_out_invc_c           TO ls_final-qty_out_invc.
    MOVE     <fs_ekpo>-bprme             TO ls_final-bprme.
    MOVE     lv_netpr                    TO ls_final-netpr.
    MOVE     lv_netwr                    TO ls_final-netwr.
    MOVE     lv_brtwr                    TO ls_final-brtwr.
    MOVE     lv_amt_out_cmtm_c           TO ls_final-amt_out_cmtm.
    MOVE     <fs_ekpo>-mwskz             TO ls_final-mwskz.
    MOVE     <fs_ekpo>-txjcd             TO ls_final-txjcd.
    MOVE     <fs_ekpo>-knttp             TO ls_final-knttp.
    MOVE     <fs_ekpo>-xersy             TO ls_final-xersy.
    MOVE     <fs_ekpo>-webre             TO ls_final-webre.
    MOVE     <fs_ekpo>-vrtkz             TO ls_final-vrtkz.
    MOVE     <fs_ekpo>-wepos             TO ls_final-wepos.
    MOVE     <fs_ekpo>-pstyp             TO ls_final-pstyp.
    MOVE     <fs_ekpo>-vrtkz             TO ls_final-vrtkz_dk.
    MOVE     <fs_ekpo>-afnam             TO ls_final-afnam.

*eject
    READ     TABLE gt_ekkn        ASSIGNING <fs_ekkn>
                                   WITH KEY ebeln = <fs_ekpo>-ebeln
                                            ebelp = <fs_ekpo>-ebelp
                              BINARY SEARCH.
    lv_subrc = sy-subrc.
    lv_tabix = sy-tabix.

    IF     ( lv_subrc NE 0 ).

      CLEAR                                 ls_output.
      PERFORM  f_format_output        USING ls_final
                                   CHANGING ls_output.
      APPEND   ls_output                 TO lt_output.

    ELSE.

      LOOP AT  gt_ekkn            ASSIGNING <fs_ekkn>
                                       FROM lv_tabix.

        IF   ( ( <fs_ekkn>-ebeln         NE <fs_ekpo>-ebeln ) OR
               ( <fs_ekkn>-ebelp         NE <fs_ekpo>-ebelp )    ).
          EXIT.
        ENDIF.

*eject
* Quantity
        CLEAR                               lv_menge.
        WRITE    <fs_ekkn>-menge         TO lv_menge
                                       UNIT <fs_ekpo>-meins.
        TRANSLATE                           lv_menge USING ', - '.
        CONDENSE                            lv_menge NO-GAPS.
        IF     ( <fs_ekkn>-menge         LT 0 ).
          CONCATENATE '-' lv_menge     INTO lv_menge.
        ENDIF.

* Percentage
        CLEAR    lv_vproz.

        IF     ( <fs_ekpo>-vrtkz         EQ '2' ).

          WRITE     <fs_ekkn>-vproz      TO lv_vproz
                                   DECIMALS 1.

        ENDIF.

*eject
* Profitability segment
        CLEAR    lv_paobjnr.

        IF     ( <fs_ekkn>-paobjnr       IS NOT INITIAL ).

          MOVE   <fs_ekkn>-paobjnr       TO lv_paobjnr.

        ENDIF.

* WBS element
        CLEAR    lv_ps_psp_pnr.

        IF     ( <fs_ekkn>-ps_psp_pnr    IS NOT INITIAL ).

          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = <fs_ekkn>-ps_psp_pnr
            IMPORTING
              OUTPUT = lv_ps_psp_pnr.

        ENDIF.

* Activity
        CLEAR    lv_vornr.

        IF   ( ( <fs_ekkn>-aufpl         IS NOT INITIAL ) AND
               ( <fs_ekkn>-aplzl         IS NOT INITIAL )     ).

          SELECT   SINGLE vornr
            INTO   lv_vornr
            FROM   afvc
           WHERE   aufpl = <fs_ekkn>-aufpl
             AND   aplzl = <fs_ekkn>-aplzl.
          IF     ( sy-subrc NE 0 ).
            CLEAR  lv_vornr.
          ENDIF.

        ENDIF.

*eject
        CLEAR                               ls_final-zekkn.
        MOVE     <fs_ekkn>-zekkn         TO ls_final-zekkn.
        CLEAR                               ls_final-menge_ekkn.
        MOVE     lv_menge                TO ls_final-menge_ekkn.
        CLEAR                               ls_final-open_qty.
        CLEAR                               ls_final-vproz.
        MOVE     lv_vproz                TO ls_final-vproz.
        CLEAR                               ls_final-sakto.
        MOVE     <fs_ekkn>-sakto         TO ls_final-sakto.
        CLEAR                               ls_final-gsber.
        MOVE     <fs_ekkn>-gsber         TO ls_final-gsber.
        CLEAR                               ls_final-kostl.
        MOVE     <fs_ekkn>-kostl         TO ls_final-kostl.
        CLEAR                               ls_final-anln1.
        MOVE     <fs_ekkn>-anln1         TO ls_final-anln1.
        CLEAR                               ls_final-anln2.
        MOVE     <fs_ekkn>-anln2         TO ls_final-anln2.
        CLEAR                               ls_final-aufnr.
        MOVE     <fs_ekkn>-aufnr         TO ls_final-aufnr.
        CLEAR                               ls_final-paobjnr.
        MOVE     lv_paobjnr              TO ls_final-paobjnr.
        CLEAR                               ls_final-prctr.
        MOVE     <fs_ekkn>-prctr         TO ls_final-prctr.
        CLEAR                               ls_final-ps_psp_pnr.
        MOVE     lv_ps_psp_pnr           TO ls_final-ps_psp_pnr.
        CLEAR                               ls_final-nplnr.
        MOVE     <fs_ekkn>-nplnr         TO ls_final-nplnr.
        CLEAR                               ls_final-vornr.
        MOVE     lv_vornr                TO ls_final-vornr.
        CLEAR                               ls_final-wempf.
        MOVE     <fs_ekkn>-wempf         TO ls_final-wempf.
        CLEAR                               ls_final-kokrs.
        MOVE     <fs_ekkn>-kokrs         TO ls_final-kokrs.
        CLEAR                               ls_final-lstar.
        MOVE     <fs_ekkn>-lstar         TO ls_final-lstar.

        CLEAR                               ls_output.
        PERFORM  f_format_output      USING ls_final
                                   CHANGING ls_output.
        APPEND   ls_output               TO lt_output.

      ENDLOOP.

    ENDIF.

    AT END OF ebeln.

      PERFORM  f_transfer_output     TABLES lt_output.

    ENDAT.

  ENDLOOP.

ENDFORM.                    " f_process_purch_order_batch
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_output
*&---------------------------------------------------------------------*
*       Format the output
*----------------------------------------------------------------------*
FORM f_format_output
  USING    is_final                    TYPE gty_final
  CHANGING cs_output                   TYPE gty_output.

  DATA:    ls_output                   TYPE gty_output,
           lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_final>            TYPE gty_final,
                 <fs_attr>             TYPE ANY.

  CLEAR    cs_output.
  CLEAR    ls_output.

  ASSIGN   is_final                      TO <fs_final>.

  DESCRIBE FIELD <fs_final> TYPE lv_dtype COMPONENTS lv_cn_comp.

  DO lv_cn_comp TIMES.

    ASSIGN COMPONENT sy-index OF STRUCTURE <fs_final> TO <fs_attr>.
    IF     ( sy-index EQ 1 ).
      ls_output = <fs_attr>.
    ELSE.
      CONCATENATE ls_output <fs_attr>
             INTO ls_output SEPARATED BY gc_delim.
    ENDIF.

  ENDDO.

  MOVE     ls_output                     TO cs_output.

ENDFORM.                    " f_format_output
*eject
*&---------------------------------------------------------------------*
*&      Form  f_transfer_output
*&---------------------------------------------------------------------*
*       Format the output
*----------------------------------------------------------------------*
FORM f_transfer_output
  TABLES   it_output                   TYPE gtt_output.

  DATA:    lv_cn_recs                  TYPE i,
           lv_cn_recs_buff             TYPE i.

  FIELD-SYMBOLS: <fs_output>           TYPE gty_output.

  lv_cn_recs_buff = LINES( it_output ).

  IF     ( rb_appl                       IS INITIAL ).

    APPEND   LINES OF it_output          TO gt_output.

    ADD      lv_cn_recs_buff             TO gv_cn_recs_file.
    ADD      lv_cn_recs_buff             TO gv_cn_recs_total.

    RETURN.

  ENDIF.

* Check if the maximum number of records per file is exceeded
  lv_cn_recs = lv_cn_recs_buff + gv_cn_recs_file.

  IF       ( lv_cn_recs                  GT gv_cn_recs_max ).

* Close the file
    IF   ( ( p_maxrec                    IS NOT INITIAL ) AND
           ( gv_filename                 IS NOT INITIAL )     ).

      CLOSE    DATASET gv_filename.

      CLEAR    gv_filename.
      CLEAR    gv_cn_recs_file.

    ENDIF.

  ENDIF.

* Open the file
  IF       ( gv_filename                 IS INITIAL ).

    PERFORM  f_open_dataset.

  ENDIF.

*eject
  IF       ( gv_filename                 IS NOT INITIAL ).

    LOOP AT       it_output       ASSIGNING <fs_output>.
      TRANSFER   <fs_output>             TO gv_filename.
    ENDLOOP.

    ADD      lv_cn_recs_buff             TO gv_cn_recs_file.
    ADD      lv_cn_recs_buff             TO gv_cn_recs_total.

  ENDIF.

ENDFORM.                    " f_transfer_output
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_dataset
*&---------------------------------------------------------------------*
*       Open the application server dataset
*----------------------------------------------------------------------*
FORM f_open_dataset.

  DATA:    lv_filename                 TYPE text256,
           lv_numc3                    TYPE numc3,
           lv_numc4                    TYPE numc4,
           lv_msg                      TYPE text100.

  ADD        1                           TO gv_cn_files.

  CLEAR                                     lv_filename.
  MOVE       gv_filename_p               TO lv_filename.

  IF       ( lv_filename                 CS 'NNNN' ).
    CLEAR                                   lv_numc4.
    MOVE     gv_cn_files                 TO lv_numc4.
    REPLACE  'NNNN'                      IN lv_filename
                                       WITH lv_numc4.
  ELSEIF   ( lv_filename                 CS 'NNN'  ).
    CLEAR                                   lv_numc3.
    MOVE     gv_cn_files                 TO lv_numc3.
    REPLACE  'NNN'                       IN lv_filename
                                       WITH lv_numc3.
  ENDIF.

  CLEAR                                     gv_filename.
  CONCATENATE   p_path2   lv_filename  INTO gv_filename.

  OPEN     DATASET gv_filename FOR OUTPUT IN TEXT MODE
                               ENCODING DEFAULT MESSAGE lv_msg.
  IF     ( sy-subrc NE 0 ).
    WRITE:   /001                 text-093, lv_msg.
    MESSAGE  e000(zfi01)     WITH text-093  lv_msg.
  ENDIF.

  PERFORM  f_create_header_rec.

ENDFORM.                    " f_open_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_header_rec
*&---------------------------------------------------------------------*
*       Create the header record
*----------------------------------------------------------------------*
FORM f_create_header_rec.

  DATA:    ls_output                   TYPE gty_output.

  CLEAR                                     ls_output.
  CONCATENATE text-c01  text-c02  text-c03  text-c04
              text-c05  text-c06  text-c07  text-c08
              text-c09  text-c10  text-c11  text-c12
              text-c13  text-c14  text-c15  text-c16
              text-c17  text-c18  text-c19  text-c20
              text-c21  text-c22  text-c23  text-c24
              text-c25  text-c26  text-c27  text-c28
              text-c29  text-c30  text-c31  text-c32
              text-c33  text-c34  text-c35  text-c36
              text-c37  text-c38  text-c39  text-c40
              text-c41  text-c42  text-c43  text-c44
              text-c45  text-c46  text-c47  text-c48
              text-c70
              text-c49  text-c50  text-c51  text-c52
              text-c53  text-c54  text-c55  text-c56
              text-c57  text-c58  text-c59  text-c60
              text-c61  text-c62  text-c63  text-c64
              text-c65  text-c66       INTO ls_output
                               SEPARATED BY gc_delim.

  IF       ( rb_appl                     IS NOT INITIAL ).
    TRANSFER ls_output                   TO gv_filename.
  ELSEIF   ( rb_pres                     IS NOT INITIAL ).
    APPEND   ls_output                   TO gt_output.
  ENDIF.

  ADD      1                             TO gv_cn_recs_file.
  ADD      1                             TO gv_cn_recs_total.

ENDFORM.                    " f_create_header_rec
*eject
*&---------------------------------------------------------------------*
*&      Form  f_download_output
*&---------------------------------------------------------------------*
*       Download the output
*----------------------------------------------------------------------*
FORM f_download_output.

  DATA:    lv_filename  TYPE string.

  IF     ( p_path1        IS INITIAL ).
    RETURN.
  ENDIF.

  IF     ( gt_output[]    IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                      lv_filename.
  MOVE     p_path1        TO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = lv_filename
    TABLES
      DATA_TAB                = gt_output
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.

  IF     ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_download_output
