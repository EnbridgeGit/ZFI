*&---------------------------------------------------------------------*
*& Report  ZFI_CONVEY_WH_TAX_RPT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapi030_convey_wh_tax_rpt MESSAGE-ID zfi_workflow.

TABLES: bsak,                                    "Cleared Vendor Items "
        bkpf,                                    "Accounting Doc Header"
        bseg.                                    "Accounting Doc Item  "

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF ty_wa_bsak,                      "Cleared Vendor Items "
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        augdt            TYPE augdt,             "Clearing Date        "
        augbl            TYPE augbl,             "Clearing Doc Number  "
        gjahr            TYPE gjahr,             "Fiscal Year          "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
       END   OF ty_wa_bsak,

        ty_it_bsak       TYPE STANDARD TABLE OF ty_wa_bsak.

TYPES: BEGIN OF ty_wa_lifnr_bukrs,               "Vendor Company       "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        bukrs            TYPE bukrs,             "Company Code         "
       END   OF ty_wa_lifnr_bukrs,

        ty_it_lifnr_bukrs
                         TYPE STANDARD TABLE OF ty_wa_lifnr_bukrs.

TYPES: BEGIN OF ty_wa_bkpf_key,                  "Accounting Doc Key   "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
       END   OF ty_wa_bkpf_key,

        ty_it_bkpf_key   TYPE STANDARD TABLE OF ty_wa_bkpf_key.

TYPES: BEGIN OF ty_wa_bkpf,                      "Accounting Document  "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        budat            TYPE budat,             "Posting Date         "
        waers            TYPE waers,             "Currency Key         "
       END   OF ty_wa_bkpf,

        ty_it_bkpf       TYPE STANDARD TABLE OF ty_wa_bkpf.

*eject
TYPES: BEGIN OF ty_wa_bseg,                      "Accounting Doc Item  "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Accounting Doc Item  "
        augdt            TYPE augdt,             "Clearing Date        "
        augbl            TYPE augbl,             "Clearing Doc Number  "
        bschl            TYPE bschl,             "Posting Key          "
        koart            TYPE koart,             "Account Type         "
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        qsskz            TYPE qsskz,             "Withholding Tax Code "
        wrbtr            TYPE wrbtr,             "Amount In Doc Currency
        ktosl            TYPE ktosl,             "Transaction Key      "
        hkont            TYPE hkont,             "General Ledger Account
        lifnr            TYPE lifnr,             "Vendor Account Number"
       END   OF ty_wa_bseg,

        ty_it_bseg       TYPE STANDARD TABLE OF ty_wa_bseg.

TYPES: BEGIN OF ty_wa_t001,                      "Company Codes        "
        bukrs            TYPE bukrs,             "Company Code         "
        waers            TYPE waers,             "Currency Key         "
        ktopl            TYPE ktopl,             "Chart Of Accounts    "
       END   OF ty_wa_t001,

        ty_it_t001       TYPE STANDARD TABLE OF ty_wa_t001.

TYPES: BEGIN OF ty_wa_t030,                      "Standard Accounts Tbl"
        ktopl            TYPE ktopl,             "Chart Of Accounts    "
        konts            TYPE saknr,             "G/L Account Number   "
        konth            TYPE saknr,             "G/L Account Number   "
       END   OF ty_wa_t030,

        ty_it_t030       TYPE STANDARD TABLE OF ty_wa_t030.

TYPES: BEGIN OF ty_wa_with_item,                 "Acct.Item Withholding"
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Accounting Line Item "
        witht            TYPE witht,             "Withhldng Tax Type Ind
        qsshb            TYPE wt_bs1,            "Withhldng Tax Base Amt
        qbshb            TYPE wt_wt1,            "Withhldng Tax Amount "
        qsrec            TYPE wt_qsrec,          "Recipient Type       "
       END   OF ty_wa_with_item,

        ty_it_with_item  TYPE STANDARD TABLE OF ty_wa_with_item.

TYPES: BEGIN OF ty_wa_lfbw,                      "Vendor Withholding   "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        bukrs            TYPE bukrs,             "Company Code         "
        witht            TYPE witht,             "Withholding Tax Type "
        wt_wtstcd        TYPE wt_wtstcd,         "Withholding Tax ID   "
       END   OF ty_wa_lfbw,

        ty_it_lfbw       TYPE STANDARD TABLE OF ty_wa_lfbw.

*eject
TYPES: BEGIN OF ty_wa_final,                     "Final Output Records "
        tin              TYPE char16,
        ein              TYPE char9,
        st               TYPE char5,
        formt            TYPE char9,
        typ              TYPE char4,
        trandt           TYPE char10,
        amtst            TYPE char8,
        amtfed           TYPE char20,
        accnt            TYPE char10,
        glaccnt          TYPE char12,
        grp              TYPE char7,
        source           TYPE char6,
        desc             TYPE char12,
        chap             TYPE char7,
       END   OF ty_wa_final,

        ty_it_final      TYPE STANDARD TABLE OF ty_wa_final.

TYPES: BEGIN OF ty_wa_controls,                  "Controls Output Record
        desc             TYPE char30,
        rec              TYPE char5,
        amt              TYPE char20,
       END   OF ty_wa_controls,

        ty_it_controls   TYPE STANDARD TABLE OF ty_wa_controls.

*eject
************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   git_bsak         TYPE ty_it_bsak.        "Cleared Vendor Items "

DATA:   git_lifnr_bukrs  TYPE ty_it_lifnr_bukrs. "Vendor Company       "

DATA:   git_bkpf_key     TYPE ty_it_bkpf_key.    "Accounting Doc Key   "

DATA:   git_bkpf         TYPE ty_it_bkpf.        "Accounting Document  "

DATA:   git_bseg         TYPE ty_it_bseg.        "Accounting Doc Item  "

DATA:   git_t001         TYPE ty_it_t001.        "Company Codes        "

DATA:   git_t030         TYPE ty_it_t030.        "Vendor Withholding Typ

DATA:   git_with_item    TYPE ty_it_with_item.   "Acct.Item Withholding"

DATA:   git_lfbw         TYPE ty_it_lfbw.        "Vendor Withholding Typ

DATA:   git_final        TYPE ty_it_final.       "Final Output Records "

DATA:   git_controls     TYPE ty_it_controls.    "Controls Output Record

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************

* Select Options
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECT-OPTIONS:   s_bukrs  FOR bsak-bukrs        "Company Code         "
                           OBLIGATORY.
SELECT-OPTIONS:   s_lifnr  FOR bsak-lifnr        "Vendor Account Number"
                           OBLIGATORY.
SELECT-OPTIONS:   s_augdt  FOR bsak-augdt.       "Clearing Date        "
SELECT-OPTIONS:   s_augbl  FOR bsak-augbl.       "Clearing Doc Number  "
SELECT-OPTIONS:   s_belnr  FOR bsak-belnr.       "Accounting Doc Number"
SELECT-OPTIONS:   s_gjahr  FOR bsak-gjahr.       "Fiscal Year          "
SELECTION-SCREEN: END   OF BLOCK ssb1.

* Run Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
PARAMETERS:       rb_appl  RADIOBUTTON GROUP rbg1  "Application Server "
                           DEFAULT 'X'
                           USER-COMMAND cmd.
PARAMETERS:       rb_pres  RADIOBUTTON GROUP rbg1. "Presentation Server"
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb3 WITH FRAME TITLE text-sb3.
PARAMETERS:       p_fpath1 TYPE localfile        "Filepath-Output      "
                           MODIF ID fp1.
PARAMETERS:       p_fpath2 TYPE localfile        "Filename-Output      "
                           MODIF ID fp1.
SELECTION-SCREEN: END   OF BLOCK ssb3.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb4 WITH FRAME TITLE text-sb4.
PARAMETERS:       p_file   LIKE rlgrap-filename,
                  p_file2  LIKE rlgrap-filename.
SELECTION-SCREEN: END   OF BLOCK ssb4.
SELECTION-SCREEN: END   OF BLOCK ssb2.

*eject
DATA: BEGIN OF t_final OCCURS 0,
       tin TYPE char16,
       ein TYPE char9,
       st  TYPE char5,
       formt TYPE char9,
       typ TYPE char4,
       trandt TYPE char10,
       amtst TYPE char8,
       amtfed TYPE char20,
       accnt TYPE char10,
       glaccnt TYPE char12,
       grp TYPE char7,
       source TYPE char6,
       desc TYPE char12,
       chap TYPE char7,
      END OF t_final.

DATA: BEGIN OF t_controls OCCURS 0,
       desc TYPE char30,
       rec TYPE char5,
       amt TYPE char20,
      END OF t_controls.

DATA: w_skat LIKE skat,
      w_t030 LIKE t030,
      w_ktopl TYPE ktopl,
      w_rec TYPE i,
      w_amt LIKE bseg-wrbtr,
      w_ind TYPE i.

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF ( rb_appl       EQ 'X' ). "file appl-srvr radiobutton
      IF   ( screen-name EQ 'P_FPATH1') OR ( screen-name EQ 'P_FPATH2').
        screen-input = 1.
        MODIFY   SCREEN.
      ENDIF.
      IF   ( screen-name EQ 'P_FILE') OR ( screen-name EQ 'P_FILE2').
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.
    ELSEIF ( rb_pres       EQ 'X' ). "input file pres-srvr radiobutton
      IF   ( screen-name EQ 'P_FPATH1') OR ( screen-name EQ 'P_FPATH2').
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.
      IF   ( screen-name EQ 'P_FILE') OR ( screen-name EQ 'P_FILE2').
        screen-input = 1.
        MODIFY   SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF       ( rb_appl IS NOT INITIAL ).
    CLEAR    p_file.
    CLEAR    p_file2.
  ENDIF.
  IF       ( rb_pres IS NOT INITIAL ).
    CLEAR    p_fpath1.
    CLEAR    p_fpath2.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file2.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE2'
    IMPORTING
      file_name  = p_file2.

AT SELECTION-SCREEN.

************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

  IF rb_pres = 'X'.
    IF p_file IS INITIAL.
      MESSAGE e000 WITH 'Please Specify Details File'.
    ENDIF.
    IF p_file2 IS INITIAL.
      MESSAGE e000 WITH 'Please Specify Controls File'.
    ENDIF.
  ENDIF.
  IF rb_appl = 'X'.
    IF p_fpath1 IS INITIAL.
      MESSAGE e000 WITH 'Maintain Details File in XParam'.
    ENDIF.
    IF p_fpath2 IS INITIAL.
      MESSAGE e000 WITH 'Maintain Controls File in XParam'.
    ENDIF.
  ENDIF.

* Initial the data elements
  PERFORM  f_initial_data_elements.

* Get the document data
  PERFORM  f_get_document_data.

************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

* Create the file column headings
  PERFORM  f_create_header.

* Merge the data into the extract records
  PERFORM  f_merge_data.

  CLEAR            t_final[].
  t_final[]    = git_final[].
  CLEAR            t_controls[].
  t_controls[] = git_controls[].

  IF     ( rb_pres IS NOT INITIAL ).

    CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
      EXPORTING
        i_filename        = p_file
      TABLES
        i_tab_sap_data    = t_final
      EXCEPTIONS
        conversion_failed = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      WRITE:/ 'Error while downloading Details File'.
    ELSE.
      WRITE:/ 'Details File Downloaded'.
    ENDIF.

    CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
      EXPORTING
        i_filename        = p_file2
      TABLES
        i_tab_sap_data    = t_controls
      EXCEPTIONS
        conversion_failed = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      WRITE:/ 'Error while downloading Controls File'.
    ELSE.
      WRITE:/ 'Controls File Downloaded'.
    ENDIF.

  ELSEIF ( rb_appl IS NOT INITIAL ).

    PERFORM  write_file.

  ENDIF.

*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  CLEAR    git_bsak[].
  CLEAR    git_lifnr_bukrs[].
  CLEAR    git_bkpf_key[].
  CLEAR    git_bkpf[].
  CLEAR    git_bseg[].
  CLEAR    git_t001[].
  CLEAR    git_t030[].
  CLEAR    git_lifnr_bukrs[].
  CLEAR    git_with_item[].
  CLEAR    git_lfbw[].
  CLEAR    git_final[].
  CLEAR    git_controls[].

* Select the company code data
  SELECT   bukrs  waers  ktopl
    INTO   TABLE git_t001
    FROM   t001.
  IF     ( sy-subrc EQ 0 ).
    SORT   git_t001 ASCENDING BY bukrs.
  ELSE.
    CLEAR  git_t001[].
  ENDIF.

* Set the file date-time stamps
  IF     ( rb_appl                       IS NOT INITIAL ).
    IF     ( p_fpath1                    CS 'YYYYMMDD' ).
      REPLACE  'YYYYMMDD'              WITH sy-datum
                                       INTO p_fpath1.
    ENDIF.
    IF     ( p_fpath1                    CS 'HHMMSS' ).
      REPLACE  'HHMMSS'                WITH sy-uzeit
                                       INTO p_fpath1.
    ENDIF.
    IF     ( p_fpath2                    CS 'YYYYMMDD' ).
      REPLACE  'YYYYMMDD'              WITH sy-datum
                                       INTO p_fpath2.
    ENDIF.
    IF     ( p_fpath2                    CS 'HHMMSS' ).
      REPLACE  'HHMMSS'                WITH sy-uzeit
                                       INTO p_fpath2.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_document_data
*&---------------------------------------------------------------------*
*       Get the document data
*----------------------------------------------------------------------*
FORM f_get_document_data.

  DATA:    lwa_bsak                    TYPE ty_wa_bsak,
           lwa_lifnr_bukrs             TYPE ty_wa_lifnr_bukrs,
           lwa_bkpf_key                TYPE ty_wa_bkpf_key,
           lit_bkpf_key                TYPE ty_it_bkpf_key,
           lwa_bseg                    TYPE ty_wa_bseg,
           lit_bseg                    TYPE ty_it_bseg,
           lwa_t030                    TYPE ty_wa_t030,
           lwa_with_item               TYPE ty_wa_with_item,
           lwa_with_item_p             TYPE ty_wa_with_item,
           lit_with_item               TYPE ty_it_with_item.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex,
           lv_bukrs                    TYPE bukrs.

  CONSTANTS:
           lc_blocksize                TYPE syindex VALUE 20.

  CLEAR    lv_bukrs.

* Select the cleared vendor items
  SELECT   bukrs  lifnr  augdt  augbl  gjahr  belnr
    INTO   TABLE git_bsak
    FROM   bsak
   WHERE   bukrs IN s_bukrs
     AND   lifnr IN s_lifnr
     AND   augdt IN s_augdt
     AND   augbl IN s_augbl
     AND   gjahr IN s_gjahr
     AND   belnr IN s_belnr.
  IF     ( sy-subrc NE 0 ).
    CLEAR  git_bsak[].
    RETURN.
  ENDIF.

*eject
* Build the accounting document key
  CLEAR                                     lwa_bsak.
  LOOP AT  git_bsak                    INTO lwa_bsak.
    CLEAR                                   lwa_lifnr_bukrs.
    MOVE     lwa_bsak-lifnr              TO lwa_lifnr_bukrs-lifnr.
    MOVE     lwa_bsak-bukrs              TO lwa_lifnr_bukrs-bukrs.
    APPEND                                  lwa_lifnr_bukrs
                                         TO git_lifnr_bukrs.
    CLEAR                                   lwa_bkpf_key.
    MOVE     lwa_bsak-bukrs              TO lwa_bkpf_key-bukrs.
    MOVE     lwa_bsak-belnr              TO lwa_bkpf_key-belnr.
    MOVE     lwa_bsak-gjahr              TO lwa_bkpf_key-gjahr.
    APPEND                                  lwa_bkpf_key
                                         TO git_bkpf_key.
    CLEAR  lwa_bsak.
  ENDLOOP.

  SORT     git_lifnr_bukrs     ASCENDING BY lifnr bukrs.
  DELETE   ADJACENT DUPLICATES         FROM git_lifnr_bukrs
                                  COMPARING lifnr bukrs.

  SORT     git_bkpf_key        ASCENDING BY bukrs belnr gjahr.
  DELETE   ADJACENT DUPLICATES         FROM git_bkpf_key
                                  COMPARING bukrs belnr gjahr.

* Select the accounting documents
  IF     ( git_bkpf_key[]                IS NOT INITIAL ).

    SELECT   bukrs  belnr  gjahr  budat  waers
      INTO   TABLE git_bkpf
      FROM   bkpf FOR ALL ENTRIES IN git_bkpf_key
     WHERE   bukrs = git_bkpf_key-bukrs
       AND   belnr = git_bkpf_key-belnr
       AND   gjahr = git_bkpf_key-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  git_bkpf[].
    ENDIF.

*eject
* Select the accounting document items in batches
    DO.

* Calculate the low and high indices for the batch of order numbers
      lv_index    =     sy-index.
      lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
      lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch of accounting document numbers
      CLEAR             lit_bkpf_key[].
      APPEND   LINES OF git_bkpf_key
                   FROM lv_index_lo
                     TO lv_index_hi
                     TO lit_bkpf_key.

      IF     ( lit_bkpf_key[] IS INITIAL ).
        EXIT.
      ENDIF.

      CLEAR    lit_bseg[].
      SELECT   bukrs  belnr  gjahr  buzei
               augdt  augbl  bschl  koart
               shkzg  qsskz  wrbtr  ktosl
               hkont  lifnr
        INTO   TABLE lit_bseg
        FROM   bseg FOR ALL ENTRIES IN lit_bkpf_key
       WHERE   bukrs = lit_bkpf_key-bukrs
         AND   belnr = lit_bkpf_key-belnr
         AND   gjahr = lit_bkpf_key-gjahr.
      IF     ( sy-subrc EQ 0 ).
        SORT   lit_bseg        ASCENDING BY bukrs belnr gjahr buzei.
      ELSE.
        CLEAR  lit_bseg[].
      ENDIF.

*eject
* Filter the accounting document items
      CLEAR                                 lwa_bseg.
      LOOP AT  lit_bseg                INTO lwa_bseg.

        IF       ( lwa_bseg-bukrs        NE lv_bukrs ).
          CLEAR                             lv_bukrs.
          MOVE     lwa_bseg-bukrs        TO lv_bukrs.
          PERFORM  f_get_wh_accounts  USING lv_bukrs.
        ENDIF.

        IF       ( lwa_bseg-hkont        IS INITIAL ).
        ELSEIF   ( lwa_bseg-hkont        EQ lwa_t030-konts ).
          APPEND   lwa_bseg              TO git_bseg.
        ELSE.
          CLEAR                             lwa_t030.
          READ     TABLE git_t030      INTO lwa_t030
                                   WITH KEY konts = lwa_bseg-hkont.
          IF     ( sy-subrc EQ 0 ).
            APPEND lwa_bseg              TO git_bseg.
          ELSE.
            CLEAR  lwa_t030.
          ENDIF.
        ENDIF.

        CLEAR  lwa_bseg.
      ENDLOOP.

    ENDDO.

  ENDIF.

*eject
* Select the withholding tax items
  IF     ( git_bseg[]                    IS NOT INITIAL ).

    SELECT   bukrs     belnr     gjahr     buzei
             witht     wt_qsshb  wt_qbshb  qsrec
      INTO   TABLE lit_with_item
      FROM   with_item FOR ALL ENTRIES IN git_bseg
     WHERE   bukrs  = git_bseg-bukrs
       AND   belnr  = git_bseg-belnr
       AND   gjahr  = git_bseg-gjahr.
    IF     ( sy-subrc EQ 0 ).

      CLEAR                                 lwa_with_item.
      LOOP AT  lit_with_item           INTO lwa_with_item.
        lv_tabix = sy-tabix.
        CLEAR                               lwa_with_item-buzei.
        MODIFY lit_with_item           FROM lwa_with_item
                                      INDEX lv_tabix.
        CLEAR  lwa_with_item.
      ENDLOOP.

      SORT     lit_with_item   ASCENDING BY bukrs belnr gjahr
                                            buzei witht qsrec.

      CLEAR                                 lwa_with_item_p.
      CLEAR                                 lwa_with_item.
      LOOP AT  lit_with_item           INTO lwa_with_item.

        IF     ( ( lwa_with_item-bukrs   NE lwa_with_item_p-bukrs ) OR
                 ( lwa_with_item-belnr   NE lwa_with_item_p-belnr ) OR
                 ( lwa_with_item-gjahr   NE lwa_with_item_p-gjahr ) OR
                 ( lwa_with_item-witht   NE lwa_with_item_p-witht ) OR
                 ( lwa_with_item-qsrec   NE lwa_with_item_p-qsrec ) ).
          IF     ( lwa_with_item_p-bukrs IS NOT INITIAL ).
            APPEND                          lwa_with_item_p
                                         TO git_with_item.
          ENDIF.
          CLEAR                             lwa_with_item_p.
          MOVE     lwa_with_item         TO lwa_with_item_p.
        ELSE.
          ADD      lwa_with_item-qsshb   TO lwa_with_item_p-qsshb.
          ADD      lwa_with_item-qbshb   TO lwa_with_item_p-qbshb.
        ENDIF.

        AT LAST.
          APPEND                            lwa_with_item_p
                                         TO git_with_item.
        ENDAT.

        CLEAR  lwa_with_item.
      ENDLOOP.

    ENDIF.

  ENDIF.

  CLEAR    lit_with_item[].

* Select the vendor withholding configuration
  IF     ( git_lifnr_bukrs[]             IS NOT INITIAL ).

    SELECT   lifnr  bukrs  witht  wt_wtstcd
      INTO   TABLE git_lfbw
      FROM   lfbw FOR ALL ENTRIES IN git_lifnr_bukrs
     WHERE   lifnr = git_lifnr_bukrs-lifnr
       AND   bukrs = git_lifnr_bukrs-bukrs.
    IF     ( sy-subrc NE 0 ).
      CLEAR  git_lfbw[].
    ENDIF.

  ENDIF.

ENDFORM.                    " f_get_document_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_wh_accounts
*&---------------------------------------------------------------------*
*       Get the withholding tax accounts
*----------------------------------------------------------------------*
FORM f_get_wh_accounts
  USING    iv_bukrs                    TYPE bukrs.

  DATA:    lwa_t001                    TYPE ty_wa_t001,
           lwa_t030                    TYPE ty_wa_t030.

  CONSTANTS:
           lc_ktosl_wh                 TYPE ktosl VALUE 'WIT'.

  CLEAR                                     lwa_t001.
  READ     TABLE git_t001              INTO lwa_t001
                                   WITH KEY bukrs = iv_bukrs
                              BINARY SEARCH.
  IF     ( sy-subrc NE 0 ).
    CLEAR        git_t030[].
    RETURN.
  ENDIF.

  CLEAR                                     lwa_t030.
  READ     TABLE git_t030              INTO lwa_t030
                                      INDEX 1.
  IF     ( sy-subrc NE 0 ).
    CLEAR        lwa_t030.
  ENDIF.

  IF         ( ( lwa_t030-ktopl          EQ lwa_t001-ktopl ) AND
               ( lwa_t030-ktopl          IS NOT INITIAL    )     ).
    RETURN.
  ENDIF.

  CLEAR    git_t030[].
  SELECT   ktopl  konts  konth
    INTO   TABLE git_t030
    FROM   t030
   WHERE   ktopl = lwa_t001-ktopl
     AND   ktosl = lc_ktosl_wh.
  IF     ( sy-subrc EQ 0 ).
    SORT   git_t030            ASCENDING BY ktopl konts konth.
    DELETE ADJACENT DUPLICATES         FROM git_t030.
  ELSE.
    CLEAR  git_t030[].
  ENDIF.

ENDFORM.                    " f_get_wh_accounts
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_header
*&---------------------------------------------------------------------*
*       Create the file column headings
*----------------------------------------------------------------------*
FORM f_create_header.

  DATA:    lwa_final                   TYPE ty_wa_final.

  CLEAR                                     lwa_final.
  MOVE     text-h01                      TO lwa_final-tin.
  MOVE     text-h02                      TO lwa_final-ein.
  MOVE     text-h03                      TO lwa_final-st.
  MOVE     text-h04                      TO lwa_final-formt.
  MOVE     text-h05                      TO lwa_final-typ.
  MOVE     text-h06                      TO lwa_final-trandt.
  MOVE     text-h07                      TO lwa_final-amtst.
  MOVE     text-h08                      TO lwa_final-amtfed.
  MOVE     text-h09                      TO lwa_final-accnt.
  MOVE     text-h10                      TO lwa_final-glaccnt.
  MOVE     text-h11                      TO lwa_final-grp.
  MOVE     text-h12                      TO lwa_final-source.
  MOVE     text-h13                      TO lwa_final-desc.
  MOVE     text-h14                      TO lwa_final-chap.
  APPEND                                    lwa_final
                                         TO git_final.

ENDFORM.                    " f_create_header
*eject
*&---------------------------------------------------------------------*
*&      Form  f_merge_data
*&---------------------------------------------------------------------*
*       Merge the data into the extract records
*----------------------------------------------------------------------*
FORM f_merge_data.

  DATA:    lwa_bsak                    TYPE ty_wa_bsak,
           lwa_bkpf                    TYPE ty_wa_bkpf,
           lwa_bseg                    TYPE ty_wa_bseg,
           lwa_with_item               TYPE ty_wa_with_item,
           lwa_lfbw                    TYPE ty_wa_lfbw,
           lwa_final                   TYPE ty_wa_final,
           lwa_controls                TYPE ty_wa_controls.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_idx1                     TYPE sytabix,
           lv_idx2                     TYPE sytabix,
           lv_key1                     TYPE char18,
           lv_key2                     TYPE char18,
           lv_key3                     TYPE char18.

  CLEAR    w_rec.
  CLEAR    w_amt.

  SORT     git_bsak            ASCENDING BY bukrs belnr gjahr.
  SORT     git_bkpf            ASCENDING BY bukrs belnr gjahr.
  SORT     git_bseg            ASCENDING BY bukrs belnr gjahr buzei.
  SORT     git_with_item       ASCENDING BY bukrs belnr gjahr buzei.
  SORT     git_lfbw            ASCENDING BY lifnr bukrs.

  CLEAR    lwa_bkpf.
  CLEAR    lwa_bseg.
  CLEAR    lwa_with_item.
  CLEAR    lwa_lfbw.

* Initial the indices
  CLEAR                                     lv_tabix.
  MOVE     1                             TO lv_tabix.
  CLEAR                                     lv_idx1.
  MOVE     1                             TO lv_idx1.
  CLEAR                                     lv_idx2.
  DESCRIBE TABLE git_with_item        LINES lv_idx2.

* Loop at the accounting document items
  CLEAR                                     lwa_bseg.
  LOOP AT  git_bseg                    INTO lwa_bseg.

    IF   ( ( lwa_bseg-bukrs              EQ lwa_bkpf-bukrs ) AND
           ( lwa_bseg-belnr              EQ lwa_bkpf-belnr ) AND
           ( lwa_bseg-gjahr              EQ lwa_bkpf-gjahr )     ).
      CLEAR  lwa_bseg.
      CONTINUE.
    ENDIF.

*eject
* Read the associated data tables
    CLEAR                                   lwa_bkpf.
    READ     TABLE git_bkpf            INTO lwa_bkpf
                                   WITH KEY bukrs = lwa_bseg-bukrs
                                            belnr = lwa_bseg-belnr
                                            gjahr = lwa_bseg-gjahr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lwa_bkpf.
      CONTINUE.
    ENDIF.

    CLEAR                                   lwa_bkpf.
    READ     TABLE git_bkpf            INTO lwa_bkpf
                                   WITH KEY bukrs = lwa_bseg-bukrs
                                            belnr = lwa_bseg-belnr
                                            gjahr = lwa_bseg-gjahr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lwa_bkpf.
      CONTINUE.
    ENDIF.

    CLEAR                                   lwa_bsak.
    READ     TABLE git_bsak            INTO lwa_bsak
                                   WITH KEY bukrs = lwa_bseg-bukrs
                                            belnr = lwa_bseg-belnr
                                            gjahr = lwa_bseg-gjahr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR        lwa_bsak.
    ENDIF.

*eject
* Reset the starting index position to the last index position read
    CLEAR                                   lv_idx1.
    MOVE     lv_tabix                    TO lv_idx1.

* Set the accounting document key
    CLEAR                                   lv_key1.
    MOVE     lwa_bseg-bukrs              TO lv_key1+00(04).
    MOVE     lwa_bseg-belnr              TO lv_key1+04(10).
    MOVE     lwa_bseg-gjahr              TO lv_key1+14(04).

    CLEAR                                   lwa_with_item.
    LOOP AT  git_with_item             INTO lwa_with_item
                                       FROM lv_idx1
                                         TO lv_idx2.
      lv_tabix = sy-tabix.

* Set the withholding item key
      CLEAR                                 lv_key2.
      MOVE     lwa_with_item-bukrs       TO lv_key2+00(04).
      MOVE     lwa_with_item-belnr       TO lv_key2+04(10).
      MOVE     lwa_with_item-gjahr       TO lv_key2+14(04).

* Exit loop if the withholding item key exceeds the clearing doc key
      IF       ( lv_key1                 LT lv_key2 ).
        EXIT.
      ELSEIF   ( lv_key1                 GT lv_key2 ).
      ELSEIF   ( lv_key1                 EQ lv_key2 ).
        CLEAR                               lv_key3.
        MOVE     lv_key1                 TO lv_key3.

        CLEAR                               lwa_final.

        IF     ( lwa_with_item-qbshb     EQ 0 ).
          CLEAR  lwa_with_item.
          CONTINUE.
        ELSEIF ( lwa_with_item-witht     EQ 'FB' ).
          MOVE   'M'                     TO lwa_final-formt.
          MOVE   'F'                     TO lwa_final-typ.
        ELSEIF ( lwa_with_item-witht     EQ '03' ).
          MOVE   'F'                     TO lwa_final-formt.
          MOVE   'N'                     TO lwa_final-typ.
          MOVE   '3'                     TO lwa_final-chap.
        ELSEIF ( lwa_with_item-witht     EQ '04' ) OR
               ( lwa_with_item-witht     EQ '05' ).
          MOVE   'F'                     TO lwa_final-formt.
          MOVE   'N'                     TO lwa_final-typ.
          MOVE   '4'                     TO lwa_final-chap.
        ELSE.
          CLEAR  lwa_with_item.
          CONTINUE.
        ENDIF.

*eject
        CLEAR                               lwa_lfbw.
        READ TABLE git_lfbw            INTO lwa_lfbw
                                   WITH KEY lifnr = lwa_bsak-lifnr
                                            bukrs = lwa_bseg-bukrs
                                            witht = lwa_with_item-witht
                              BINARY SEARCH.
        IF     ( sy-subrc NE 0 ).
          CLEAR      lwa_lfbw.
        ENDIF.

        MOVE     lwa_lfbw-wt_wtstcd      TO lwa_final-tin.
        MOVE     lwa_bseg-bukrs          TO lwa_final-ein.
        MOVE     lwa_bsak-lifnr          TO lwa_final-accnt.
        MOVE     lwa_bseg-hkont          TO lwa_final-glaccnt.
        MOVE     lwa_bseg-belnr          TO lwa_final-desc.

        WRITE    lwa_bkpf-budat          TO lwa_final-trandt
                                            MM/DD/YYYY.

        MOVE     lwa_with_item-qbshb     TO lwa_final-amtfed.

        PERFORM  f_format_amount   CHANGING lwa_final-amtfed.

        APPEND                              lwa_final
                                         TO git_final.
        ADD      1                       TO w_rec.
        ADD      lwa_with_item-qbshb     TO w_amt.

      ENDIF.

      CLEAR  lwa_with_item.
    ENDLOOP.

    IF     ( lv_key3                     IS INITIAL ).
      CLEAR  lv_key3.
    ELSE.
      CLEAR  lv_key3.
    ENDIF.

    CLEAR  lwa_bseg.
  ENDLOOP.

  CLEAR                                     lwa_controls.
  MOVE     text-c01                      TO lwa_controls-desc.
  MOVE     w_rec                         TO lwa_controls-rec.
  MOVE     w_amt                         TO lwa_controls-amt.

  PERFORM  f_format_amount         CHANGING lwa_controls-amt.

  APPEND                                    lwa_controls
                                         TO git_controls.

ENDFORM.                    " f_merge_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_amount
*&---------------------------------------------------------------------*
*       Format the amount
*----------------------------------------------------------------------*
FORM f_format_amount
  CHANGING cv_amount TYPE char20.

  IF           ( cv_amount CS '-' ).
    TRANSLATE    cv_amount USING '- '.
    CONDENSE     cv_amount NO-GAPS.
    SHIFT        cv_amount RIGHT BY 1 PLACES.
    MOVE '-'  TO cv_amount+0(1).
  ENDIF.

  SHIFT          cv_amount RIGHT DELETING TRAILING SPACE.

ENDFORM.                    " f_format_amount
*eject
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
*       Write the file to the application server
*----------------------------------------------------------------------*
FORM write_file .
  DATA: lv_msg          TYPE text100,
        lv_string       TYPE string,
        lv_text         TYPE text1000.

  IF   ( t_final[]            IS NOT INITIAL ).
    OPEN     DATASET p_fpath1
             FOR OUTPUT IN TEXT MODE MESSAGE lv_msg ENCODING DEFAULT.
    IF sy-subrc NE 0.
      MESSAGE e000 WITH 'File Opening Error'.
      RETURN.
    ENDIF.
    CLEAR                                 lv_string.
    LOOP AT  t_final. "   INTO lv_string.
      CONCATENATE t_final-tin t_final-ein t_final-st t_final-formt t_final-typ t_final-trandt t_final-amtst t_final-amtfed t_final-accnt
      t_final-glaccnt t_final-grp t_final-source t_final-desc t_final-chap INTO lv_string SEPARATED BY '|'.
      CLEAR                               lv_text.
      MOVE   lv_string                 TO lv_text.

      TRANSFER                            lv_text
                                       TO p_fpath1.

      CLEAR  lv_string.
    ENDLOOP.

* Close the file
    CLOSE    DATASET  p_fpath1.
  ENDIF.
  WAIT UP TO 1 SECONDS.

  IF   ( t_controls[]            IS NOT INITIAL ).
    OPEN     DATASET p_fpath2
             FOR OUTPUT IN TEXT MODE MESSAGE lv_msg ENCODING DEFAULT.
    IF sy-subrc NE 0.
      MESSAGE e000 WITH 'File Opening Error'.
      RETURN.
    ENDIF.
    CLEAR                                 lv_string.
    LOOP AT  t_controls. "   INTO lv_string.
      CONCATENATE t_controls-desc t_controls-rec t_controls-amt INTO lv_string SEPARATED BY '|'.
      CLEAR                               lv_text.
      MOVE   lv_string                 TO lv_text.

      TRANSFER                            lv_text
                                       TO p_fpath2.

      CLEAR  lv_string.
    ENDLOOP.

* Close the file
    CLOSE    DATASET  p_fpath2.
  ENDIF.
ENDFORM.                    " WRITE_FILE
