*&---------------------------------------------------------------------*
*&  Include           ZFAPI113_PAYMENT_HISTORY_F01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :  ZFAPI113_PAYMENT_HISTORY                       *
* Include            :  ZFAPI113_PAYMENT_HISTORY_F01                   *
* Author             :  Kalinga Keshari Rout                           *
* Creation Date      :  February 01, 2018                              *
* Object ID          :                                                 *
* Application Area   :  FICO                                           *
* Description        :  The Payment History interface lists all paid   *
*                       invoices with their corresponding payment      *
*                       information.  It includes all payments that    *
*                       have been processed from Checks, EFT Payments, *
*                       Wire Transfers or any other form of payment.   *
*                       The Payment History file should include one    *
*                       record per invoice AND payment.  Multiple      *
*                       invoices paid on the same payment should have  *
*                       one record for each invoice.  Multiple         *
*                       payments for the same invoice should have one  *
*                       record for each payment.                       *
*----------------------------------------------------------------------*
*                       Modification Log                               *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 09-Apr-2018  KROUT       D30K928652  CHG0100818#Initial development  *
*                          D30K928716, D30K928741, D30K928766,         *
*                          D30K928774, D30K928886                      *
*----------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_PATH2  text
*      <--P_P_PATH4  text
*----------------------------------------------------------------------*
FORM f_get_file_path  CHANGING cv_file1 TYPE ANY .

  CONSTANTS:  lc_path1  TYPE filepath-pathintern VALUE 'ZFAPI113_PAYMENT_HISTORY'.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = lc_path1
    IMPORTING
      file_name        = cv_file1
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Default File Name
  CONCATENATE p_erpid '_PAYMENT_HISTORY_'  sy-datum '_' sy-uzeit '.CSV' INTO p_file2.

ENDFORM.                    " F_GET_FILE_PATH
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_f4_help_file_path .

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = p_path2
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_GET_F4_HELP_FILE_PATH
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_f4_help_file_path1 .

  DATA: lv_repid TYPE sy-repid,
        lv_dynnr TYPE sy-dynnr,
        lv_file  TYPE rlgrap-filename.

  lv_repid = sy-repid.
  lv_dynnr = sy-dynnr.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = lv_repid
      dynpro_number = lv_dynnr
    CHANGING
      file_name     = lv_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE lv_file TO p_path1.

ENDFORM.                    " F_GET_F4_HELP_FILE_PATH1
*eject
*&---------------------------------------------------------------------*
*&      Form  f_display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_display_data .

  DATA: lv_file    TYPE string,
        lv_records TYPE string,
        lv_lines   TYPE i .

  MOVE     p_path1 TO lv_file .

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                        = lv_file
*   IMPORTING
    TABLES
      data_tab                        = gt_final
    EXCEPTIONS
     file_write_error                 = 1
     no_batch                         = 2
     gui_refuse_filetransfer          = 3
     invalid_type                     = 4
     no_authority                     = 5
     unknown_error                    = 6
     header_not_allowed               = 7
     separator_not_allowed            = 8
     filesize_not_allowed             = 9
     header_too_long                  = 10
     dp_error_create                  = 11
     dp_error_send                    = 12
     dp_error_write                   = 13
     unknown_dp_error                 = 14
     access_denied                    = 15
     dp_out_of_memory                 = 16
     disk_full                        = 17
     dp_timeout                       = 18
     file_not_found                   = 19
     dataprovider_exception           = 20
     control_flush_error              = 21
     OTHERS                           = 22 .

  IF sy-subrc <> 0.

  ENDIF.

* Output the total number of records extracted
  CLEAR                         lv_lines .
  DESCRIBE TABLE gt_final LINES lv_lines .
  lv_records = lv_lines .
  WRITE: / text-009, lv_records .

ENDFORM.                    " f_display_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
*       Select the clearing document keys
*----------------------------------------------------------------------*
FORM f_get_data.

  DATA:    lt_initial                  TYPE gtt_initial,
           ls_initial                  TYPE gty_initial,
           ls_initial_p                TYPE gty_initial.

  DATA:    lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS:
           lc_blocksize                TYPE syindex VALUE 5000.

* Open the output dataset
  IF     ( p_app IS NOT INITIAL ).

    PERFORM  f_open_dataset.

  ENDIF.

  CLEAR    gt_final[].
  CLEAR    gt_initial[].

  SELECT   bukrs  lifnr  augdt  augbl
    FROM   bsak
    INTO   TABLE gt_initial
   WHERE   bukrs IN s_bukrs
     AND   lifnr IN s_lifnr
     AND   augdt IN s_augdt
     AND   augbl IN s_augbl
     AND   gjahr IN s_gjahr
     AND   belnr IN s_belnr
     AND   cpudt IN s_cpudt.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_initial          ASCENDING BY bukrs augdt augbl lifnr.
    DELETE         ADJACENT DUPLICATES FROM gt_initial
                                  COMPARING bukrs augdt augbl lifnr.
  ELSE.
    CLEAR  gt_initial[].
  ENDIF.

*eject
* Process the cleared items in batches
* DO.
*
* Calculate the low and high indices for the batch of cleared items
*   lv_index    =     sy-index.
*   lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
*   lv_index_hi = (   lv_index       * lc_blocksize ).
*
* Build the batch of order numbers
*   CLEAR             lt_initial[].
*   APPEND   LINES OF gt_initial
*                FROM lv_index_lo
*                  TO lv_index_hi
*                  TO lt_initial.
*
*   IF     ( lt_initial[] IS INITIAL ).
*     EXIT.
*   ENDIF.
*
*   PERFORM  f_select_bsak_details   TABLES lt_initial.
*
* ENDDO.

  CLEAR      lt_initial[].
  CLEAR      ls_initial.
  CLEAR      ls_initial_p.
  CLEAR      lv_index.

  LOOP AT    gt_initial                INTO ls_initial.

    IF   ( ( ls_initial-bukrs            NE ls_initial_p-bukrs ) OR
           ( ls_initial-augdt            NE ls_initial_p-augdt ) OR
           ( ls_initial-augbl            NE ls_initial_p-augbl )    ).
      CLEAR                                 ls_initial_p.
      MOVE   ls_initial                  TO ls_initial_p.

      IF       ( lv_index                GE lc_blocksize ).
        CLEAR    lv_index.

        SORT     lt_initial ASCENDING BY lifnr bukrs augdt augbl.

        PERFORM  f_select_bsak_details   TABLES lt_initial.

        CLEAR    lt_initial[].

      ENDIF.

      ADD    1                           TO lv_index.

    ENDIF.

    APPEND   ls_initial                  TO lt_initial.

    CLEAR    ls_initial.
  ENDLOOP.

*eject
  IF       ( lt_initial[]                IS NOT INITIAL ).

    SORT     lt_initial ASCENDING BY lifnr bukrs augdt augbl.

    PERFORM  f_select_bsak_details   TABLES lt_initial.

  ENDIF.

* Close the output dataset
  IF     ( p_app IS NOT INITIAL ).

    PERFORM  f_close_dataset.

  ENDIF.

ENDFORM.                    " f_get_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_bsak_details
*&---------------------------------------------------------------------*
*       Select the cleared documents and associated vendor data
*----------------------------------------------------------------------*
FORM f_select_bsak_details
  TABLES   it_initial                  TYPE gtt_initial.

  DATA:    lt_detail                   TYPE gtt_bsak,
           ls_detail                   TYPE gty_bsak.

  FIELD-SYMBOLS: <fs_detail>           TYPE gty_bsak.

  IF     ( it_initial[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    gt_detail[].
  CLEAR    gt_lfa1[].

  SELECT   bukrs  lifnr  umsks  umskz  augdt
           augbl  zuonr  gjahr  belnr  buzei
           budat  bldat  cpudt  waers  xblnr
           blart  bschl  shkzg  gsber  dmbtr
           wrbtr  mwsts  sgtxt  zfbdt  zterm
           zbd1t  zbd2t  zbd3t  sknto  wskto
           zlsch  rebzg  bstat  dmbe2  penfc  vertn
    INTO   TABLE gt_detail
    FROM   bsak FOR ALL ENTRIES IN it_initial
   WHERE   lifnr = it_initial-lifnr
     AND   bukrs = it_initial-bukrs
     AND   augdt = it_initial-augdt
     AND   augbl = it_initial-augbl.
  IF     ( sy-subrc EQ 0 ).

    SELECT   lifnr  name1
      INTO   TABLE gt_lfa1
      FROM   lfa1 FOR ALL ENTRIES IN gt_detail
     WHERE   lifnr = gt_detail-lifnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfa1 ASCENDING BY lifnr.
    ENDIF.

    SORT   gt_detail ASCENDING BY bukrs umsks umskz
                                  augdt augbl gjahr belnr.

*eject
    CLEAR    lt_detail[].
    CLEAR    ls_detail.

    LOOP AT  gt_detail            ASSIGNING <fs_detail>.

      IF   ( ( ls_detail-bukrs           NE <fs_detail>-bukrs ) OR
             ( ls_detail-umsks           NE <fs_detail>-umsks ) OR
             ( ls_detail-umskz           NE <fs_detail>-umskz ) OR
             ( ls_detail-augdt           NE <fs_detail>-augdt ) OR
             ( ls_detail-augbl           NE <fs_detail>-augbl )    ).

        CLEAR                               ls_detail.
        MOVE     <fs_detail>             TO ls_detail.

        IF       ( lt_detail[]           IS NOT INITIAL ).

          PERFORM  f_process_cleared_docs   TABLES lt_detail.

        ENDIF.

        CLEAR    lt_detail[].

      ENDIF.

      APPEND   <fs_detail>               TO lt_detail.

    ENDLOOP.

    IF       ( lt_detail[]               IS NOT INITIAL ).

      PERFORM  f_process_cleared_docs       TABLES lt_detail.

    ENDIF.

    IF     ( p_app IS NOT INITIAL ).

      PERFORM  f_transfer_data.

    ENDIF.

  ENDIF.

ENDFORM.                    " f_select_bsak_details
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_cleared_docs
*&---------------------------------------------------------------------*
*       Process the cleared documents
*----------------------------------------------------------------------*
FORM f_process_cleared_docs
  TABLES   it_detail                   TYPE gtt_bsak.

  DATA:    lt_detail                   TYPE gtt_bsak,
           ls_detail                   TYPE gty_bsak.

  DATA:    lv_lifnr                    TYPE lifnr,
           lv_fl_delete                TYPE xflag.

  FIELD-SYMBOLS: <fs_detail>           TYPE gty_bsak.

  IF     ( it_detail[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_detail[].
  CLEAR    ls_detail.

  CLEAR    lv_lifnr.
  CLEAR    lv_fl_delete.

  READ TABLE it_detail                 INTO ls_detail INDEX 1.

  IF     ( ( ls_detail-umsks             IS NOT INITIAL ) OR
           ( ls_detail-umskz             IS NOT INITIAL )    ).
    lv_fl_delete = abap_true.
  ENDIF.

  IF     ( ( ls_detail-augbl             NE ls_detail-belnr ) AND
           ( ls_detail-xblnr             IS INITIAL         )     ).
    lv_fl_delete = abap_true.
  ENDIF.

  MOVE       ls_detail-lifnr             TO lv_lifnr.

  LOOP AT  it_detail                   FROM 2
                                  ASSIGNING <fs_detail>.

    IF     ( ( <fs_detail>-augbl         NE <fs_detail>-belnr ) AND
             ( <fs_detail>-xblnr         IS INITIAL           )     ).
      lv_fl_delete = abap_true.
    ENDIF.

    IF     ( lv_lifnr                    NE <fs_detail>-lifnr ).
      lv_fl_delete = abap_true.
    ENDIF.

*eject
    IF         ( ( ls_detail-bukrs       NE <fs_detail>-bukrs ) OR
                 ( ls_detail-belnr       NE <fs_detail>-belnr ) OR
                 ( ls_detail-gjahr       NE <fs_detail>-gjahr )    ).

      APPEND       ls_detail             TO lt_detail.
      CLEAR                                 ls_detail.
      MOVE         <fs_detail>           TO ls_detail.

    ELSE.

      IF         ( <fs_detail>-shkzg     EQ ls_detail-shkzg ).
        ADD        <fs_detail>-dmbtr     TO ls_detail-dmbtr.
        ADD        <fs_detail>-wrbtr     TO ls_detail-wrbtr.
        ADD        <fs_detail>-dmbe2     TO ls_detail-dmbe2.
        ADD        <fs_detail>-wskto     TO ls_detail-wskto.
        ADD        <fs_detail>-mwsts     TO ls_detail-mwsts.
        ADD        <fs_detail>-sknto     TO ls_detail-sknto.
      ELSE.
        SUBTRACT   <fs_detail>-dmbtr   FROM ls_detail-dmbtr.
        SUBTRACT   <fs_detail>-wrbtr   FROM ls_detail-wrbtr.
        SUBTRACT   <fs_detail>-dmbe2   FROM ls_detail-dmbe2.
        SUBTRACT   <fs_detail>-wskto   FROM ls_detail-wskto.
        SUBTRACT   <fs_detail>-mwsts   FROM ls_detail-mwsts.
        SUBTRACT   <fs_detail>-sknto   FROM ls_detail-sknto.
        IF       ( ls_detail-dmbtr       LT 0 ).
          MULTIPLY ls_detail-dmbtr       BY -1.
          MULTIPLY ls_detail-wrbtr       BY -1.
          MULTIPLY ls_detail-dmbe2       BY -1.
          MULTIPLY ls_detail-wskto       BY -1.
          MULTIPLY ls_detail-mwsts       BY -1.
          MULTIPLY ls_detail-sknto       BY -1.
          IF     ( ls_detail-shkzg       EQ 'S' ).
            CLEAR                           ls_detail-shkzg.
            MOVE   'H'                   TO ls_detail-shkzg.
          ELSEIF ( ls_detail-shkzg       EQ 'H' ).
            CLEAR                           ls_detail-shkzg.
            MOVE   'S'                   TO ls_detail-shkzg.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    AT LAST.
      APPEND   ls_detail                 TO lt_detail.
    ENDAT.

  ENDLOOP.

  IF     ( lv_fl_delete IS NOT INITIAL ).
    RETURN.
  ENDIF.

  PERFORM  f_create_final_data       TABLES lt_detail.

ENDFORM.                    " f_process_cleared_docs
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_final_data
*&---------------------------------------------------------------------*
*       Create the final data
*----------------------------------------------------------------------*
FORM f_create_final_data
  TABLES   it_detail                   TYPE gtt_bsak.

  DATA:    ls_detail                   TYPE gty_bsak,
           ls_detail_p                 TYPE gty_bsak,
           lt_bkpf                     TYPE gtt_bkpf,
           ls_bkpf                     TYPE gty_bkpf,
           ls_bkpf_p                   TYPE gty_bkpf,
           lt_toa01                    TYPE gtt_toa01,
           ls_toa01                    TYPE gty_toa01.

  DATA:    lv_faedt                    TYPE rfpos-faedt,
           lv_zfbdt                    TYPE dzfbdt,
           lv_kursf                    TYPE p LENGTH 9 DECIMALS 5,
           lv_object_id                TYPE saeobjid,
           lv_char16                   TYPE char16,
           lv_char16_p                 TYPE char16,
           lv_char12                   TYPE char12,
           lv_text                     TYPE text200.

  CONSTANTS: lc_sep                    TYPE char1 VALUE '_'.

  IF     ( it_detail[] IS INITIAL ).
    RETURN.
  ENDIF.

* Read the payment (clearing) document from the detail table
  CLEAR                                     ls_detail .
  READ TABLE it_detail                 INTO ls_detail INDEX 1 .

  CLEAR                                     ls_detail_p .
  READ TABLE it_detail                 INTO ls_detail_p
                                   WITH KEY belnr = ls_detail-augbl .
  IF     ( sy-subrc NE 0 ) .
    CLEAR    ls_detail_p .
  ENDIF .

* Payment total
  CLEAR                                     lv_char16_p.
  WRITE    ls_detail_p-wrbtr             TO lv_char16_p
                                   CURRENCY ls_detail_p-waers .
  TRANSLATE                                 lv_char16_p USING ', - '.
  CONDENSE                                  lv_char16_p NO-GAPS.
  IF   ( ( ls_detail_p-shkzg             EQ 'S' ) OR
         ( ls_detail_p-wrbtr             EQ  0  )    ).
  ELSE.
    CONCATENATE '-' lv_char16_p        INTO lv_char16_p.
  ENDIF.

*eject
* Select the accounting document headers
  SELECT   bukrs  belnr  gjahr  cpudt  usnam
           tcode  kursf  awtyp  awkey  hwaer  hwae2
    INTO   TABLE lt_bkpf
    FROM   bkpf FOR ALL ENTRIES IN it_detail
   WHERE   bukrs = it_detail-bukrs
     AND   belnr = it_detail-belnr
     AND   gjahr = it_detail-gjahr.
  IF     ( sy-subrc NE 0 ) .
    CLEAR  lt_bkpf[] .
  ENDIF .

* Read the payment (clearing) document from the accounting documents
  CLEAR                                     ls_bkpf_p.
  READ     TABLE lt_bkpf               INTO ls_bkpf_p
                                   WITH KEY belnr = ls_detail-augbl.
  IF     ( sy-subrc EQ 0 ).
    IF         ( ls_bkpf_p-tcode         EQ 'FB08' ).
      CLEAR                                 ls_bkpf.
      LOOP AT    lt_bkpf               INTO ls_bkpf.
        IF     ( ls_bkpf-tcode           EQ 'F110' ).
          RETURN.
        ENDIF.
        CLEAR    ls_bkpf.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Loop at the invoices
  CLEAR    ls_detail .
  LOOP AT  it_detail INTO ls_detail .

    IF     ( ls_detail-augbl EQ ls_detail-belnr ) .
      CLEAR  ls_detail .
      CONTINUE .
    ENDIF .

* Read the accounting document header
    CLEAR                                   ls_bkpf .
    READ TABLE lt_bkpf                 INTO ls_bkpf
                                   WITH KEY bukrs = ls_detail-bukrs
                                            belnr = ls_detail-belnr
                                            gjahr = ls_detail-gjahr .
    IF     ( sy-subrc NE 0 ) .
      CLEAR    ls_bkpf .
    ENDIF .

* Read the vendor
    CLEAR                                   gs_lfa1 .
    READ TABLE gt_lfa1                 INTO gs_lfa1
                                   WITH KEY lifnr = ls_detail-lifnr
                              BINARY SEARCH .
    IF     ( sy-subrc NE 0 ) .
      CLEAR    gs_lfa1 .
    ENDIF.

*eject
*******  fill the final table data
    CLEAR  gs_final.

    MOVE-CORRESPONDING ls_detail TO gs_final .

* Late payment fee is not a required field in ATCAT, please leave this blank.
* (but also because there are no amounts found in any of our production systems)
    CLEAR gs_final-penfc .

* Comment
    PERFORM f_check_text CHANGING gs_final-sgtxt .

* ULIDs
    CONCATENATE ls_detail-belnr ls_detail-bukrs ls_detail-gjahr ls_detail-augbl
                             INTO  gs_final-ulid  SEPARATED BY lc_sep.
    CONCATENATE ls_detail-augbl ls_detail-bukrs ls_detail-gjahr
                             INTO  gs_final-culid SEPARATED BY lc_sep.
    CONCATENATE ls_detail-belnr ls_detail-bukrs ls_detail-gjahr
                             INTO  gs_final-iulid SEPARATED BY lc_sep.
    CONCATENATE ls_detail-lifnr ls_detail-bukrs
                             INTO  gs_final-vulid SEPARATED BY lc_sep.
    CONCATENATE ls_detail-belnr ls_detail-bukrs ls_detail-gjahr
                             INTO  gs_final-aulid SEPARATED BY lc_sep.

* Payment total
    MOVE   lv_char16_p                   TO gs_final-wrbtr.  "payment total

* Invoice total and invoice amount paid
    CLEAR                                   lv_char16.
    WRITE    ls_detail-wrbtr             TO lv_char16
                                   CURRENCY ls_detail-waers .
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( ls_detail-shkzg             EQ 'S' ) OR
           ( ls_detail-wrbtr             EQ  0  )    ).
    ELSE.
      CONCATENATE '-' lv_char16        INTO lv_char16.
    ENDIF.

    MOVE   lv_char16                     TO gs_final-iwrbtr. "invoice total
    MOVE   lv_char16                     TO gs_final-twrbtr. "invoice amount paid

* Payment discount amount
    CLEAR                                   lv_char16.
    WRITE    ls_detail-wskto             TO lv_char16
                                   CURRENCY ls_detail-waers .
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( ls_detail-shkzg             EQ 'S' ) OR
           ( ls_detail-wskto             EQ  0  )    ).
    ELSE.
      CONCATENATE '-' lv_char16        INTO lv_char16.
    ENDIF.

    MOVE   lv_char16                     TO gs_final-wskto.  "payment discount amount

*eject
* Sales tax total
    CLEAR                                   lv_char16.
    WRITE    ls_detail-mwsts             TO lv_char16
                                   CURRENCY ls_detail-waers .
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( ls_detail-shkzg             EQ 'S' ) OR
           ( ls_detail-mwsts             EQ  0  )    ).
    ELSE.
      CONCATENATE '-' lv_char16        INTO lv_char16.
    ENDIF.

    MOVE   lv_char16                     TO gs_final-mwsts.  "sales tax total

* Cash discount amount
    CLEAR                                   lv_char16.
    WRITE    ls_detail-sknto             TO lv_char16
                                   CURRENCY ls_detail-waers .
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( ls_detail-shkzg             EQ 'S' ) OR
           ( ls_detail-sknto             EQ  0  )    ).
    ELSE.
      CONCATENATE '-' lv_char16        INTO lv_char16.
    ENDIF.

    MOVE   lv_char16                     TO gs_final-sknto.  "Cash discount amount

* Payment date
*   WRITE ls_detail-augdt TO gs_final-paydat    MM/DD/YYYY .
    PERFORM  f_format_date            USING ls_detail-augdt
                                   CHANGING gs_final-paydat.

* Invoice date
*   WRITE ls_detail-bldat TO gs_final-invdat    MM/DD/YYYY .
    PERFORM  f_format_date            USING ls_detail-bldat
                                   CHANGING gs_final-invdat.

* Payment due date
    CLEAR lv_faedt .

    CALL FUNCTION 'NET_DUE_DATE_GET'
      EXPORTING
        i_zfbdt = ls_detail-zfbdt
        i_zbd1t = ls_detail-zbd1t
        i_zbd2t = ls_detail-zbd2t
        i_zbd3t = ls_detail-zbd3t
        i_shkzg = ls_detail-shkzg
        i_rebzg = ls_detail-rebzg
        i_koart = 'K'
      IMPORTING
        e_faedt = lv_faedt.

*   WRITE lv_faedt        TO gs_final-payduedat MM/DD/YYYY .
    PERFORM  f_format_date            USING lv_faedt
                                   CHANGING gs_final-payduedat.

*eject
* Payment discount due date
    lv_zfbdt = ls_detail-zfbdt + ls_detail-zbd1t.

*   WRITE lv_zfbdt        TO gs_final-paydscdat MM/DD/YYYY .
    PERFORM  f_format_date            USING lv_zfbdt
                                   CHANGING gs_final-paydscdat.

* Invoice received date - to be replaced with custom field in bkpf
*   WRITE ls_detail-budat TO gs_final-invrcvdat MM/DD/YYYY.
    PERFORM  f_format_date            USING ls_detail-budat
                                   CHANGING gs_final-invrcvdat.

* Invoice entry date
*   WRITE ls_detail-budat TO gs_final-inventdat MM/DD/YYYY .
    PERFORM  f_format_date            USING ls_detail-budat
                                   CHANGING gs_final-inventdat.

* Post date
*   WRITE ls_detail-cpudt TO gs_final-postdat   MM/DD/YYYY .
    PERFORM  f_format_date            USING ls_detail-cpudt
                                   CHANGING gs_final-postdat.

* Clear date
*   WRITE ls_detail-augdt TO gs_final-cleardat  MM/DD/YYYY .
    PERFORM  f_format_date            USING ls_detail-augdt
                                   CHANGING gs_final-cleardat.

* Invoice approval date
*   WRITE ls_detail-cpudt TO gs_final-invappdat MM/DD/YYYY .
    PERFORM  f_format_date            USING ls_detail-cpudt
                                   CHANGING gs_final-invappdat.

* Voucher origin
    MOVE  p_erpid         TO gs_final-origin .

* Payment status
    gs_final-bstat = 'P' .

    IF     ( gs_lfa1-lifnr IS NOT INITIAL ).

* Vendor name
      gs_final-name1 = gs_lfa1-name1 .

    ENDIF.

*eject
    IF     ( ls_bkpf-bukrs IS NOT INITIAL ).

* Exchange Rate Trx To Rpt
      IF     ( ls_detail-waers EQ gc_cad ) .
        lv_kursf = 1 .
      ELSEIF ( ls_bkpf-hwaer   EQ gc_cad ) .
        IF   ( ls_detail-dmbtr EQ 0      ) OR
             ( ls_detail-wrbtr EQ 0      ) .
          lv_kursf = 0 .
        ELSE .
          lv_kursf = ls_detail-dmbtr / ls_detail-wrbtr .
        ENDIF .
      ELSEIF ( ls_bkpf-hwae2   EQ gc_cad ) .
        IF   ( ls_detail-dmbe2 EQ 0      ) OR
             ( ls_detail-wrbtr EQ 0      ) .
          lv_kursf = 0 .
        ELSE .
          lv_kursf = ls_detail-dmbe2 / ls_detail-wrbtr .
        ENDIF .
      ELSE .
        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
            client           = sy-mandt
            date             = ls_detail-budat
            foreign_amount   = ls_detail-wrbtr
            foreign_currency = ls_detail-waers
            local_currency   = gc_cad
            type_of_rate     = p_kurst
            read_tcurr       = gc_x
          IMPORTING
            exchange_rate    = lv_kursf
          EXCEPTIONS
            no_rate_found    = 1
            overflow         = 2
            no_factors_found = 3
            no_spread_found  = 4
            derived_2_times  = 5
            OTHERS           = 6.
        IF ( sy-subrc NE 0 ) .
          lv_kursf = 0 .
          MESSAGE  ID sy-msgid             TYPE sy-msgty
               NUMBER sy-msgno             INTO lv_text
                 WITH sy-msgv1                  sy-msgv2
                      sy-msgv3                  sy-msgv4 .
          WRITE: / lv_text .
        ENDIF .
      ENDIF .

      CLEAR                                   lv_char12 .
      WRITE    lv_kursf                    TO lv_char12
                                     DECIMALS 5 .
      CONDENSE                                lv_char12 NO-GAPS .

      gs_final-xrt_tc2rc  = lv_char12 .
      gs_final-waers      = ls_detail-waers .

*eject
* Exchange Rate Trx To Acctg
      IF     ( ls_detail-waers EQ ls_bkpf-hwaer ) .
        lv_kursf = 1 .
      ELSE .
        IF   ( ls_bkpf-kursf GE 0 ) .
          lv_kursf = ls_bkpf-kursf .
        ELSE .
          lv_kursf = 1 / ( ls_bkpf-kursf * -1 ) .
        ENDIF .
      ENDIF .

      CLEAR                                   lv_char12 .
      WRITE    lv_kursf                    TO lv_char12
                                     DECIMALS 5 .
      CONDENSE                                lv_char12 NO-GAPS .

      gs_final-xrt_tc2ac  = lv_char12 .
      gs_final-hwaer      = ls_bkpf-hwaer .

* Invoice source
      gs_final-tcode      = ls_bkpf-tcode .

* Entered by
      gs_final-usnam      = ls_bkpf-usnam .

* Purchase order indicator
      IF   ( ls_bkpf-awtyp EQ 'RMRP' ) .

        gs_final-poind = 'YES' .

        CLEAR                               lv_object_id .
        MOVE     ls_bkpf-awkey           TO lv_object_id .

      ELSE .

        gs_final-poind = 'NO' .

        CLEAR                               lv_object_id .
        MOVE     ls_bkpf-bukrs           TO lv_object_id+00(04) .
        MOVE     ls_bkpf-belnr           TO lv_object_id+04(10) .
        MOVE     ls_bkpf-gjahr           TO lv_object_id+14(04) .

      ENDIF .

*eject
* Select the link table 1 entry
      SELECT   *
        INTO   TABLE lt_toa01
        FROM   toa01
       WHERE   sap_object IN ('BKPF', 'BUS2081')
         AND   object_id   = lv_object_id .
      IF     ( sy-subrc EQ 0 ) .

        CLEAR                               ls_toa01 .
        LOOP AT  lt_toa01              INTO ls_toa01 .
          IF ( ( ls_toa01-ar_object      EQ 'ZMMINV'   ) OR
               ( ls_toa01-ar_object      EQ 'ZFIINVNP' ) OR
               ( ls_toa01-ar_object      EQ 'ZFIINVPO' )    ) .
            EXIT .
          ENDIF .
          CLEAR  ls_toa01 .
        ENDLOOP .
        IF     ( ls_toa01-sap_object     IS INITIAL ) .
          CLEAR                             ls_toa01 .
          READ TABLE lt_toa01          INTO ls_toa01 INDEX 1 .
        ENDIF .

        MOVE     ls_toa01-object_id      TO gs_final-urn .
        MOVE     ls_toa01-arc_doc_id     TO gs_final-linkurl1 .
        MOVE     'InvoiceLink'           TO gs_final-linktype1 .

      ELSE .
        CLEAR  ls_toa01 .
      ENDIF .

    ENDIF .

    APPEND gs_final TO gt_final .

    CLEAR  gs_final .

    CLEAR  ls_detail .
  ENDLOOP .

ENDFORM.                    " f_create_final_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_text
*&---------------------------------------------------------------------*
*       Check the text for control characters and field delimiters
*----------------------------------------------------------------------*
FORM f_check_text  CHANGING cv_ktext  TYPE sgtxt .

  DATA : lv_string    TYPE char50,
         lv_ind       TYPE syindex,
         lv_char      TYPE c,
         lv_len       TYPE i .

  CONSTANTS : lc_sep1 TYPE c LENGTH 2  VALUE '|~',
              lc_sep2 TYPE c LENGTH 2  VALUE '~|',
              lc_rep1 TYPE c LENGTH 2  VALUE '|-',
              lc_rep2 TYPE c LENGTH 2  VALUE '-|' .

  CLEAR: lv_ind, lv_char, lv_len .

  CLEAR                lv_string .
  MOVE     cv_ktext TO lv_string .

  REPLACE ALL OCCURRENCES OF lc_sep1 IN lv_string WITH lc_rep1 .
  REPLACE ALL OCCURRENCES OF lc_sep2 IN lv_string WITH lc_rep2 .

*--> Any Other Spl Chars less than Space replace by Space
  lv_len = STRLEN( lv_string ) .

  DO lv_len TIMES .
    lv_ind = sy-index - 1 .

    CLEAR                           lv_char .
    MOVE     lv_string+lv_ind(1) TO lv_char .
    IF     ( lv_char LT ' ' ) .
      MOVE   SPACE TO lv_string+lv_ind(1) .
    ENDIF .

  ENDDO .

  CLEAR                 cv_ktext .
  MOVE     lv_string TO cv_ktext .

ENDFORM.                    " f_check_text
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_dataset
*&---------------------------------------------------------------------*
*       Open the output dataset
*----------------------------------------------------------------------*
FORM f_open_dataset .

  DATA:    lv_msg_text(80) TYPE c .

  IF     ( p_app IS INITIAL ) .
    RETURN .
  ENDIF .

  CONCATENATE p_path2 p_erpid p_file2 INTO gv_filename .

  OPEN     DATASET gv_filename FOR OUTPUT IN TEXT MODE
                               ENCODING DEFAULT
                               MESSAGE lv_msg_text .
  IF     ( sy-subrc NE 0 ) .
    WRITE: text-014, lv_msg_text, gv_filename .
    EXIT .
  ENDIF .

  CLEAR    gv_rec_count .

  PERFORM  f_create_header .

ENDFORM.                    " f_open_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_close_dataset
*&---------------------------------------------------------------------*
*       Close the output dataset
*----------------------------------------------------------------------*
FORM f_close_dataset .

  DATA:    lv_records      TYPE string .

  IF     ( p_app IS INITIAL ) .
    RETURN .
  ENDIF .

  CLOSE    DATASET gv_filename .

* Output the total number of records extracted
  lv_records = gv_rec_count .
  WRITE: / text-009, lv_records .

ENDFORM.                    " f_close_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_header
*&---------------------------------------------------------------------*
*       Create the header record in the output dataset
*----------------------------------------------------------------------*
FORM f_create_header .

  DATA : lv_string       TYPE STRING .

  CONSTANTS : lc_sep(2)  TYPE c VALUE '|~' .

  CLEAR    lv_string .

  CONCATENATE text-101 text-102 text-103 text-104 text-105
              text-106 text-107 text-108 text-109 text-110
              text-111 text-112 text-113 text-114 text-115
              text-116 text-117 text-118 text-119 text-120
              text-121 text-122 text-123 text-124 text-125
              text-126 text-127 text-128 text-129 text-130
              text-131 text-132 text-133 text-134 text-135
              text-136 text-137 text-138 text-139 text-140
              text-141 text-142 text-143 text-144 text-145
              text-146 text-147        INTO lv_string
                               SEPARATED BY lc_sep.

  TRANSFER lv_string TO gv_filename .

  ADD      1         TO gv_rec_count .

ENDFORM.                    " f_create_header
*eject
*&---------------------------------------------------------------------*
*&      Form  f_transfer_data
*&---------------------------------------------------------------------*
*       Transfer data to the output dataset
*----------------------------------------------------------------------*
FORM f_transfer_data .

  DATA : lv_string           TYPE string,
         lv_dtype            TYPE c,
         lv_count            TYPE i,
         lv_lines            TYPE i .

  CONSTANTS : lc_sep(2)      TYPE c  VALUE '|~' .

  FIELD-SYMBOLS : <ls_final> TYPE gty_final,
                  <ls_attr>  TYPE any .

  IF     ( gt_final[] IS INITIAL ) .
    RETURN .
  ENDIF .

  DESCRIBE FIELD <ls_final> TYPE lv_dtype COMPONENTS lv_count .

* Transfer data
  LOOP AT  gt_final ASSIGNING <ls_final> .

    CLEAR  lv_string.

    DO lv_count TIMES .

      ASSIGN COMPONENT sy-index OF STRUCTURE <ls_final> TO <ls_attr> .
      IF sy-index = 1 .
        lv_string = <ls_attr> .
      ELSE .
        CONCATENATE lv_string  <ls_attr>
               INTO lv_string SEPARATED BY lc_sep .
      ENDIF .

    ENDDO .

    TRANSFER lv_string TO gv_filename .

  ENDLOOP .

  DESCRIBE TABLE gt_final LINES lv_lines .
  ADD      lv_lines          TO gv_rec_count .

  CLEAR    gt_final[] .

ENDFORM.                    " f_transfer_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_date
*&---------------------------------------------------------------------*
*       Format the date "MM/DD/YYYY"
*----------------------------------------------------------------------*
FORM f_format_date
  USING    iv_datum                    TYPE sydatum
  CHANGING cv_date                     TYPE char10.

  CLEAR    cv_date.

  IF     ( iv_datum IS INITIAL ).
    RETURN.
  ENDIF.

  CONCATENATE iv_datum+4(2) '/'
              iv_datum+6(2) '/'
              iv_datum+0(4)     INTO cv_date.

ENDFORM.                    " f_format_date
