*&---------------------------------------------------------------------*
*& Report  ZFI_CONVEY_1099_RPT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFI_CONVEY_1099_RPT MESSAGE-ID ZFI_WORKFLOW.

TABLES: bsak,                                               "DECK915499
        lfbw.                                               "DECK915499

* Begin changes - insert code                               "DECK915499

TYPES: BEGIN OF ty_wa_t001,                      "Company Codes        "
        bukrs            TYPE bukrs,             "Company Code         "
        land1            TYPE land1,             "Country Key          "
        adrnr            TYPE adrnr,             "Address Number       "
       END   OF ty_wa_t001.

TYPES: BEGIN OF ty_wa_adrc,                      "Addresses            "
        adrnr            TYPE ad_addrnum,        "Address Number       "
        region           TYPE regio,             "Region               "
       END   OF ty_wa_adrc.

TYPES: BEGIN OF ty_wa_lfbw,                      "Vendor Withholding Tax
        lifnr            TYPE lifnr,             "Vendor Account Number"
        bukrs            TYPE bukrs,             "Company Code         "
        witht            TYPE witht,             "Withholding Tax Type "
        qsrec            TYPE wt_qsrec,          "Vendor Recipient Type"
        tin              TYPE wt_wtstcd,         "Withholding Tax ID Nbr
        box              TYPE wt_withcd,         "Withholding Tax Code "
        exempt           TYPE wt_exnr,           "Exemptn Certificte Nbr
       END   OF ty_wa_lfbw.

TYPES: BEGIN OF ty_wa_bukrs_lifnr,               "Vendor Company Key
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
       END   OF ty_wa_bukrs_lifnr.

TYPES: BEGIN OF ty_wa_bsak,                      "AP Cleared Documents "
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        augbl            TYPE augbl,             "Clearing Document Nbr"
        gjahr            TYPE gjahr,             "Fiscal Year
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        qsshb            TYPE qsshb,             "Withhldng Tax Base Amt
        qbshb            TYPE qbshb,             "Withhldng Tax Amount "
       END   OF ty_wa_bsak.

*eject
TYPES: BEGIN OF ty_wa_with_item,                 "Acct.Item Withholding"
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Accounting Doc Item  "
        witht            TYPE witht,             "Withholding Tax Type "
        wt_withcd        TYPE wt_withcd,         "Withholding Tax Code "
        qsshh            TYPE wt_bs,             "Withhldng Tax Base Amt
        qbshh            TYPE wt_bs1,            "Withhldng Tax Base Amt
       END   OF ty_wa_with_item.

TYPES: BEGIN OF ty_wa_lfa1,                      "Vendor Master (General
        lifnr            TYPE lifnr,             "Vendor Number        "
        land1            TYPE land1_gp,          "Country Key          "
        name             TYPE name1_gp,          "Name                 "
        city             TYPE ort01_gp,          "City                 "
        zip              TYPE pstlz,             "Postal Code          "
        state            TYPE regio,             "Region (State, Prov.)"
        street           TYPE stras_gp,          "House Number And Stret
       END   OF ty_wa_lfa1.

* End changes   - insert code                               "DECK915499

*eject
DATA: Begin of T_FINAL OCCURS 0,
       TIN TYPE CHAR16,
       EIN TYPE CHAR9,
       ven type char10,
       NAME TYPE CHAR40,
       ADDR TYPE CHAR40,
       CITY TYPE CHAR20,
       STATE TYPE CHAR5,
       ZIP TYPE CHAR10,
       ASTATE TYPE CHAR8,
       BOX1 TYPE CHAR20,
       BOX2 TYPE CHAR20,
       BOX3 TYPE CHAR20,
       BOX4 TYPE CHAR20,
       BOX5 TYPE CHAR20,
       BOX6 TYPE CHAR20,
       BOX7 TYPE CHAR20,
       BOX8 TYPE CHAR20,
       BOX9 TYPE CHAR20,
       BOX10 TYPE CHAR20,
       BOX11 TYPE CHAR20,
       BOX12 TYPE CHAR20,
       BOX13 TYPE CHAR20,
       BOX14 TYPE CHAR20,
       BOX15 TYPE CHAR20,
       BOX16 TYPE CHAR20,
       BOX17 TYPE CHAR20,
       BOX18 TYPE CHAR20,
       OIL TYPE CHAR10,
       LOC TYPE CHAR12,
       MCC TYPE CHAR10,
       MCCC TYPE CHAR10,
      END OF T_FINAL.

DATA: Begin of T_controls OCCURS 0,
       DESC TYPE CHAR30,
       AMT TYPE CHAR20,
      END OF T_Controls.

*eject
* Begin changes - replace code                              "DECK915499

DATA:   gwa_t001 TYPE ty_wa_t001.

DATA: BEGIN OF t_t001 OCCURS 0.
        INCLUDE STRUCTURE gwa_t001.
*     BUKRS LIKE T001-BUKRS,
*     LAND1 LIKE T001-LAND1,
DATA: END OF t_t001.

DATA:   gwa_adrc TYPE ty_wa_adrc.

DATA: BEGIN OF t_adrc OCCURS 0.
        INCLUDE STRUCTURE gwa_adrc.
*     ADRNR LIKE ADRC-ADDRNUMBER,
*     REGION LIKE ADRC-REGION,
DATA: END OF t_adrc.

DATA:   gwa_lfbw TYPE ty_wa_lfbw.

DATA: BEGIN OF t_lfbw OCCURS 0.
        INCLUDE STRUCTURE gwa_lfbw.
*     lifnr like lfbw-lifnr,
*     bukrs like lfbw-bukrs,
*     TIN   like lfbw-WT_WTSTCD,
*     BOX like lfbw-WT_WITHCD,
DATA: END OF t_lfbw.

DATA:   gwa_bukrs_lifnr TYPE ty_wa_bukrs_lifnr.

DATA: BEGIN OF t_bukrs_lifnr OCCURS 0.
        INCLUDE STRUCTURE gwa_bukrs_lifnr.
DATA: END OF t_bukrs_lifnr.

DATA:   gwa_bsak TYPE ty_wa_bsak.

DATA: BEGIN OF t_bsak OCCURS 0.
        INCLUDE STRUCTURE gwa_bsak.
*     BUKRS LIKE BSAK-BUKRS,
*     LIFNR LIKE BSAK-LIFNR,
*     AUGBL LIKE BSAK-AUGBL,
*     BELNR LIKE BSAK-BELNR,
*     qsshb like BSAK-QSSHB,
DATA: END OF t_bsak.

DATA: BEGIN OF t_bsak2 OCCURS 0.
        INCLUDE STRUCTURE gwa_bsak.
*     BUKRS LIKE BSAK-BUKRS,
*     LIFNR LIKE BSAK-LIFNR,
*     AUGBL LIKE BSAK-AUGBL,
*     BELNR LIKE BSAK-BELNR,
*     qsshb like BSAK-QSSHB,
DATA: END OF t_bsak2.

*eject
DATA: BEGIN OF t_bsak3 OCCURS 0.
        INCLUDE STRUCTURE gwa_bsak.
*     BUKRS LIKE BSAK-BUKRS,
*     LIFNR LIKE BSAK-LIFNR,
*     AUGBL LIKE BSAK-AUGBL,
*     BELNR LIKE BSAK-BELNR,
*     qsshb like BSAK-QSSHB,
DATA: END OF t_bsak3.

DATA:   gwa_with_item TYPE ty_wa_with_item.

DATA: BEGIN OF t_witem OCCURS 0.
        INCLUDE STRUCTURE gwa_with_item.
*     BUKRS LIKE WITH_ITEM-BUKRS,
*     BELNR LIKE WITH_ITEM-BELNR,
*     GJAHR LIKE WITH_ITEM-GJAHR,
*     QSSHB LIKE WITH_ITEM-WT_QSSHH,
DATA: END OF t_witem.

DATA:   gwa_lfa1 TYPE ty_wa_lfa1.

DATA: BEGIN OF T_ADD OCCURS 0.
        INCLUDE STRUCTURE gwa_lfa1.
*     lifnr like LFA1-lifnr,
*     LAND1 LIKE LFA1-LAND1,
*     NAME like LFA1-NAME1,
*     CITY LIKE LFA1-ORT01,
*     ZIP LIKE LFA1-PSTLZ,
*     STATE LIKE LFA1-REGIO,
*     STREET LIKE LFA1-STRAS,
DATA: END OF T_ADD.

* End changes   - replace code                              "DECK915499

DATA: W_IND TYPE I.

*eject
SELECTION-SCREEN: SKIP 1.                                   "DECK915499
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1. "15499
SELECT-OPTIONS:   s_bukrs  FOR bsak-bukrs.                  "DECK915499
SELECT-OPTIONS:   s_lifnr  FOR bsak-lifnr OBLIGATORY.       "DECK915499
SELECT-OPTIONS:   s_augbl  FOR bsak-augbl.                  "DECK915499
SELECT-OPTIONS:   s_gjahr  FOR bsak-gjahr.                  "DECK915499
SELECT-OPTIONS:   s_augdt  FOR bsak-augdt.                  "DECK915499
SELECT-OPTIONS:   s_blart  FOR bsak-blart.                  "DECK915499
SELECT-OPTIONS:   s_witht  FOR lfbw-witht.                  "DECK915499
SELECTION-SCREEN: END   OF BLOCK ssb1.                      "DECK915499
SELECTION-SCREEN: SKIP 1.                                   "DECK915499
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2. "15499
PARAMETERS:       p_file   TYPE localfile OBLIGATORY,       "DECK915499
                  p_file2  TYPE localfile OBLIGATORY.       "DECK915499
SELECTION-SCREEN: END   OF BLOCK ssb2.                      "DECK915499

INITIALIZATION.                                             "DECK915499
  PERFORM  f_initialization.                                "DECK915499

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
  IF P_FILE IS INITIAL.
    Message E000 with 'Please Specify Details File'.
  ENDIF.

  IF P_FILE2 IS INITIAL.
    Message E000 with 'Please Specify Controls File'.
  ENDIF.

START-OF-SELECTION.

  perform get_document_data.

END-OF-SELECTION.
  perform create_header.

  perform merge_data.

  CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
    EXPORTING
      I_FILENAME        = P_FILE
    TABLES
      I_TAB_SAP_DATA    = t_final
    EXCEPTIONS
      CONVERSION_FAILED = 1
      OTHERS            = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    write:/ 'Error while downloading Details File'.
  else.
    write:/ 'Details File Downloaded'.
  ENDIF.

  CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
    EXPORTING
      I_FILENAME        = P_FILE2
    TABLES
      I_TAB_SAP_DATA    = t_controls
    EXCEPTIONS
      CONVERSION_FAILED = 1
      OTHERS            = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    write:/ 'Error while downloading Controls File'.
  else.
    write:/ 'Controls File Downloaded'.
  ENDIF.

*eject
*&---------------------------------------------------------------------*
*&      Form  GET_DOCUMENT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DOCUMENT_DATA_OLD.                                 "DECK915499

* R_TYPE-SIGN = 'I'.
* R_TYPE-OPTION = 'EQ'.
* R_TYPE-LOW = 'FB'.
* APPEND R_TYPE.
* R_TYPE-LOW = 'FE'.
* APPEND R_TYPE.

* SELECT LIFNR BUKRS WT_WTSTCD WT_WITHCD FROM LFBW INTO TABLE T_LFBW
  SELECT   lifnr  bukrs  witht  qsrec  wt_wtstcd  wt_withcd  wt_exnr
     INTO  TABLE t_lfbw
     FROM  lfbw
     WHERE LIFNR IN S_LIFNR AND
           BUKRS IN S_BUKRS AND
           WITHT IN s_witht.
  IF SY-SUBRC = 0.
*   SELECT BUKRS LIFNR AUGBL BELNR QSSHB FROM BSAK INTO TABLE T_BSAK
    SELECT   bukrs  lifnr  augbl  gjahr  belnr  shkzg  qsshb  qbshb
      INTO  TABLE t_bsak
      FROM  bsak
      FOR ALL ENTRIES IN T_LFBW
      WHERE BUKRS = T_LFBW-BUKRS AND
            LIFNR = T_LFBW-LIFNR AND
            AUGDT IN S_AUGDT AND
            AUGBL IN S_AUGBL AND
            GJAHR IN S_GJAHR.
*            BLART = 'ZP'.
    IF SY-SUBRC = 0.
      SELECT BUKRS BELNR GJAHR WT_QSSHH FROM WITH_ITEM INTO TABLE T_WITEM
        FOR ALL ENTRIES IN T_BSAK
        WHERE BUKRS = T_BSAK-BUKRS AND
              BELNR = T_BSAK-AUGBL AND
              GJAHR IN S_GJAHR.
    ENDIF.
* Confirm that the document is a payment clearing doc
    LOOP AT T_BSAK.
      W_IND = SY-TABIX.
      IF T_BSAK-AUGBL NE T_BSAK-BELNR.
        DELETE T_BSAK INDEX W_IND.
      ENDIF.
    ENDLOOP.

*eject
    T_BSAK2[] = T_BSAK[].
    T_BSAK3[] = T_BSAK[].

    SORT T_BSAK2 BY BUKRS ASCENDING.
    DELETE ADJACENT DUPLICATES FROM T_BSAK2 COMPARING BUKRS.

    SELECT BUKRS LAND1 ADRNR FROM T001 INTO TABLE T_T001
      FOR ALL ENTRIES IN T_BSAK2
      WHERE BUKRS = T_BSAK2-BUKRS.
    IF SY-SUBRC = 0.
      SELECT ADDRNUMBER REGION FROM ADRC INTO TABLE T_ADRC
        FOR ALL ENTRIES IN T_T001
        WHERE ADDRNUMBER = T_T001-ADRNR.
    ENDIF.

    SELECT LIFNR NAME1 ORT01 PSTLZ REGIO STRAS FROM LFA1 INTO TABLE T_ADD
      FOR ALL ENTRIES IN T_LFBW
      WHERE LIFNR = T_LFBW-LIFNR.

    SORT T_ADD BY LIFNR ASCENDING.
    DELETE ADJACENT DUPLICATES FROM T_ADD COMPARING LIFNR.
  ENDIF.

ENDFORM.                    " GET_DOCUMENT_DATA
*eject
*&---------------------------------------------------------------------*
*&      Form  CREATE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_HEADER .
  CLEAR T_FINAL.
  T_FINAL-TIN = 'TIN'.
  T_FINAL-EIN = 'PAYER EIN'.
  T_FINAL-VEN = 'Vendor'.
  T_FINAL-NAME = 'Name'.
  T_FINAL-ADDR = 'Address'.
  T_FINAL-CITY = 'City'.
  T_FINAL-STATE = 'State'.
  T_FINAL-ZIP = 'ZIP'.
  T_FINAL-ASTATE = 'AltState'.
  T_FINAL-BOX1 = 'Box_1'.
  T_FINAL-BOX2 = 'Box_2'.
  T_FINAL-BOX3 = 'Box_3'.
  T_FINAL-BOX4 = 'Box_4'.
  T_FINAL-BOX5 = 'Box_5'.
  T_FINAL-BOX6 = 'Box_6'.
  T_FINAL-BOX7 = 'Box_7'.
  T_FINAL-BOX8 = 'Box_8'.
  T_FINAL-BOX9 = 'Box_9'.
  T_FINAL-BOX10 = 'Box_10'.
  T_FINAL-BOX11 = 'Box_11'.
  T_FINAL-BOX12 = 'Box_12'.
  T_FINAL-BOX13 = 'Box_13'.
  T_FINAL-BOX14 = 'Box_14'.
  T_FINAL-BOX15 = 'Box_15'.
  T_FINAL-BOX16 = 'Box_16'.
  T_FINAL-BOX17 = 'Box_17'.
  T_FINAL-BOX18 = 'Box_18'.
  T_FINAL-OIL = 'OilGasInd'.
  T_FINAL-LOC = 'LocWithheld'.
  T_FINAL-MCC = 'MCC_RPRT'.
  T_FINAL-MCCC = 'MCC_Code'.
  APPEND T_FINAL.
ENDFORM.                    " CREATE_HEADER
*eject
*&---------------------------------------------------------------------*
*&      Form  MERGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MERGE_DATA_OLD.                                        "DECK915499
  DATA: W_REC TYPE I,
        W_PAY TYPE I,
        W_PAYEE TYPE I,
        W_AMT LIKE BSAK-QSSHB.

  CLEAR: W_PAYEE, w_pay.

  LOOP AT T_BSAK.

*    IF NOT T_BSAK-QSSHB GT 0.
    READ TABLE T_WITEM WITH KEY BUKRS = T_BSAK-BUKRS BELNR = T_BSAK-AUGBL.
    IF SY-SUBRC = 0 AND NOT T_WITEM-QSSHH GT 0.
      CONTINUE.
    ELSEIF SY-SUBRC NE 0.
      CONTINUE.
    ELSE.
      T_BSAK-QSSHB = T_WITEM-QSSHH.
    ENDIF.
*    ENDIF.

    READ TABLE T_BSAK3 WITH KEY LIFNR = T_BSAK-LIFNR.
    IF SY-SUBRC = 0.
      W_PAYEE = W_PAYEE + 1.
      DELETE T_BSAK3 WHERE LIFNR = T_BSAK-LIFNR.
    ENDIF.

    READ TABLE T_BSAK2 WITH KEY BUKRS = T_BSAK-BUKRS.
    IF SY-SUBRC = 0.
      W_PAY = W_PAY + 1.
      DELETE T_BSAK2 WHERE BUKRS = T_BSAK-BUKRS.
    ENDIF.

    CLEAR: T_FINAL.
    READ TABLE T_LFBW WITH KEY LIFNR = T_BSAK-LIFNR BUKRS = T_BSAK-BUKRS.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    READ TABLE T_ADD WITH KEY LIFNR = T_BSAK-LIFNR.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

*eject
    T_FINAL-TIN = T_LFBW-TIN.
    T_FINAL-EIN = T_BSAK-BUKRS.
    T_FINAL-VEN = T_BSAK-LIFNR.
    T_FINAL-NAME = T_ADD-NAME.
    T_FINAL-ADDR = T_ADD-STREET.
    T_FINAL-CITY = T_ADD-CITY.
    T_FINAL-STATE = T_ADD-STATE.
    T_FINAL-ZIP = T_ADD-ZIP.

    READ TABLE T_T001 WITH KEY BUKRS = T_BSAK-BUKRS.
    IF SY-SUBRC = 0.
      READ TABLE T_ADRC WITH KEY ADRNR = T_T001-ADRNR.
      IF SY-SUBRC = 0.
        T_FINAL-ASTATE = T_ADRC-REGION.
      ENDIF.
    ENDIF.

    CASE T_LFBW-BOX.
      WHEN '01'.
        T_FINAL-BOX1 = T_BSAK-QSSHB.
      WHEN '02'.
        T_FINAL-BOX2 = T_BSAK-QSSHB.
      WHEN '03'.
        T_FINAL-BOX3 = T_BSAK-QSSHB.
      WHEN '04'.
        T_FINAL-BOX4 = T_BSAK-QSSHB.
      WHEN '06'.
        T_FINAL-BOX6 = T_BSAK-QSSHB.
      WHEN '07'.
        T_FINAL-BOX7 = T_BSAK-QSSHB.
      WHEN '14'.
        T_FINAL-BOX14 = T_BSAK-QSSHB.
      WHEN OTHERS.

    ENDCASE.

    APPEND T_FINAL.
    W_REC = W_REC + 1.
    W_AMT = W_AMT + T_BSAK-QSSHB.

  ENDLOOP.

  T_CONTROLS-DESC = 'Total Records'.
  T_CONTROLS-AMT = W_REC.
  APPEND T_CONTROLS.
  T_CONTROLS-DESC = 'Total Payers'.
  T_CONTROLS-AMT = W_PAY.
  APPEND T_CONTROLS.
  T_CONTROLS-DESC = 'Total Payees'.
  T_CONTROLS-AMT = W_PAYEE.
  APPEND T_CONTROLS.
  T_CONTROLS-DESC = 'Total Dollars'.
  T_CONTROLS-AMT = W_AMT.
  APPEND T_CONTROLS.
ENDFORM.                    " MERGE_DATA
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initialization                              "DECK915499
*&---------------------------------------------------------------------*
*       Initialization
*----------------------------------------------------------------------*
FORM f_initialization.                                      "DECK915499

  DATA:    lwa_witht                   LIKE LINE OF s_witht.

  CLEAR    t_final[].
  CLEAR    t_controls[].
  CLEAR    t_t001[].
  CLEAR    t_adrc[].
  CLEAR    t_lfbw[].
  CLEAR    t_bukrs_lifnr[].
  CLEAR    t_bsak[].
  CLEAR    t_bsak2[].
  CLEAR    t_bsak3[].
  CLEAR    t_witem[].
  CLEAR    t_add[].

  CLEAR                                     lwa_witht.
  MOVE     'I'                           TO lwa_witht-sign.
  MOVE     'EQ'                          TO lwa_witht-option.
  MOVE     'FB'                          TO lwa_witht-low.
  APPEND   lwa_witht                     TO s_witht.
  CLEAR                                     lwa_witht-low.
  MOVE     'FE'                          TO lwa_witht-low.
  APPEND   lwa_witht                     TO s_witht.

ENDFORM.                    " f_initialization              "DECK915499
*eject
*&---------------------------------------------------------------------*
*&      Form  get_document_data                             "DECK915499
*&---------------------------------------------------------------------*
*       Select the document data
*----------------------------------------------------------------------*
FORM get_document_data.                                     "DECK915499

  DATA:    lv_tabix TYPE sytabix.

* Select the vendor withholding tax type configuration
  SELECT   lifnr  bukrs  witht  qsrec  wt_wtstcd  wt_withcd  wt_exnr
    INTO   TABLE t_lfbw
    FROM   lfbw
   WHERE   lifnr IN s_lifnr
     AND   bukrs IN s_bukrs
     AND   witht IN s_witht.
  IF     ( sy-subrc NE 0 ).
    CLEAR  t_lfbw[].
    RETURN.
  ENDIF.

  SORT     t_lfbw              ASCENDING BY lifnr bukrs witht.
  DELETE   ADJACENT DUPLICATES         FROM t_lfbw
                                  COMPARING lifnr bukrs witht.

  CLEAR    t_lfbw.
  LOOP AT  t_lfbw.
    CLEAR                                   t_bukrs_lifnr.
    MOVE   t_lfbw-bukrs                  TO t_bukrs_lifnr-bukrs.
    MOVE   t_lfbw-lifnr                  TO t_bukrs_lifnr-lifnr.
    APPEND                                  t_bukrs_lifnr.
    CLEAR  t_lfbw.
  ENDLOOP.

  SORT     t_bukrs_lifnr       ASCENDING BY bukrs lifnr.
  DELETE   ADJACENT DUPLICATES         FROM t_bukrs_lifnr
                                  COMPARING bukrs lifnr.

*eject
* Select the vendor invoices
  SELECT   bukrs  lifnr  augbl  gjahr  belnr  shkzg  qsshb  qbshb
    INTO   TABLE t_bsak
    FROM   bsak FOR ALL ENTRIES IN t_bukrs_lifnr
   WHERE   bukrs  = t_bukrs_lifnr-bukrs
     AND   lifnr  = t_bukrs_lifnr-lifnr
     AND   augdt IN s_augdt
     AND   augbl IN s_augbl
     AND   gjahr IN s_gjahr
     AND   blart IN s_blart.
  IF     ( sy-subrc NE 0 ).
    CLEAR  t_bsak[].
  ENDIF.

* Confirm that the document is a payment clearing document
  CLEAR    t_bsak.
  LOOP AT  t_bsak.
    lv_tabix = sy-tabix.
    IF     ( t_bsak-augbl              EQ t_bsak-belnr ).
    ELSE.
      DELETE   t_bsak               INDEX lv_tabix.
    ENDIF.
    CLEAR  t_bsak.
  ENDLOOP.

  SORT     t_bsak              ASCENDING BY bukrs lifnr augbl gjahr.
  DELETE   ADJACENT DUPLICATES         FROM t_bsak
                                  COMPARING bukrs lifnr augbl gjahr.

* Select the accounting document withholding items
  IF     ( t_bsak[] IS NOT INITIAL ).

    SELECT   bukrs  belnr  gjahr  buzei  witht
             wt_withcd  wt_qsshh  wt_qbshh
      INTO   TABLE t_witem
      FROM   with_item FOR ALL ENTRIES IN t_bsak
     WHERE   bukrs  = t_bsak-bukrs
       AND   belnr  = t_bsak-augbl
       AND   gjahr  = t_bsak-gjahr
       AND   witht IN s_witht.
    IF     ( sy-subrc EQ 0 ).
      SORT   t_witem           ASCENDING BY bukrs belnr gjahr
                                            buzei witht.
      DELETE ADJACENT DUPLICATES       FROM t_witem
                                  COMPARING bukrs belnr gjahr
                                            buzei witht.
    ELSE.
      CLEAR  t_witem[].
    ENDIF.

  ENDIF.

*eject
* Select the vendor address data
  IF     ( t_lfbw[] IS NOT INITIAL ).

    SELECT   lifnr  land1  name1  ort01  pstlz  regio  stras
      INTO   TABLE t_add
      FROM   lfa1 FOR ALL ENTRIES IN t_lfbw
     WHERE   lifnr = t_lfbw-lifnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   t_add             ASCENDING BY lifnr.
      DELETE ADJACENT DUPLICATES       FROM t_add
                                  COMPARING lifnr.
    ELSE.
      CLEAR  t_add[].
    ENDIF.

  ENDIF.

  t_bsak2[] = t_bsak[].
  SORT     t_bsak2 ASCENDING BY bukrs.
  DELETE   ADJACENT DUPLICATES FROM t_bsak2 COMPARING bukrs.

  t_bsak3[] = t_bsak[].
  SORT     t_bsak3 ASCENDING BY lifnr.
  DELETE   ADJACENT DUPLICATES FROM t_bsak3 COMPARING lifnr.

  IF     ( t_bsak2[] IS NOT INITIAL ).

    SELECT   bukrs  land1  adrnr
      INTO   TABLE t_t001
      FROM   t001 FOR ALL ENTRIES IN t_bsak2
     WHERE   bukrs = t_bsak2-bukrs.
    IF     ( sy-subrc EQ 0 ).
      SORT     t_t001 ASCENDING BY bukrs.
      SELECT   addrnumber  region
        INTO   TABLE t_adrc
        FROM   adrc FOR ALL ENTRIES IN t_t001
       WHERE   addrnumber = t_t001-adrnr.
      IF     ( sy-subrc EQ 0 ).
        SORT   t_adrc ASCENDING BY adrnr.
      ELSE.
        CLEAR  t_adrc[].
      ENDIF.
    ELSE.
      CLEAR  t_t001[].
    ENDIF.

  ENDIF.

ENDFORM.                    " get_document_data             "DECK915499
*eject
*&---------------------------------------------------------------------*
*&      Form  merge_data                                    "DECK915499
*&---------------------------------------------------------------------*
*       Merge the document data
*----------------------------------------------------------------------*
FORM merge_data.                                            "DECK915499

  DATA:    w_rec        TYPE I,
           w_pay        TYPE I,
           w_payee      TYPE I,
           w_amt        TYPE wt_bs.

  DATA:    lv_subrc     TYPE sysubrc,
           lv_tabix     TYPE sytabix,
           lv_tabix_2   TYPE sytabix,
           lv_idx1      TYPE syindex,
           lv_idx2      TYPE syindex,
           lv_key1      TYPE char14,
           lv_key2      TYPE char14,
           lv_box1      TYPE wt_bs,
           lv_box2      TYPE wt_bs,
           lv_box3      TYPE wt_bs,
           lv_box4      TYPE wt_bs,
           lv_box5      TYPE wt_bs,
           lv_box6      TYPE wt_bs,
           lv_box7      TYPE wt_bs,
           lv_box14     TYPE wt_bs,
           lv_fl_out    TYPE flag.

  SORT     t_lfbw         ASCENDING BY lifnr bukrs.
  SORT     t_bsak         ASCENDING BY bukrs belnr gjahr.
  SORT     t_witem        ASCENDING BY bukrs belnr gjahr buzei witht.

  CLEAR    w_rec.
  CLEAR    w_pay.
  CLEAR    w_payee.
  CLEAR    w_amt.

* Initial the indices
  CLEAR                                     lv_tabix.
  MOVE     1                             TO lv_tabix.
  CLEAR                                     lv_idx1.
  MOVE     1                             TO lv_idx1.
  CLEAR                                     lv_idx2.
  DESCRIBE TABLE t_witem              LINES lv_idx2.

*eject
* Loop through the payments internal table
  CLEAR    t_bsak.
  LOOP AT  t_bsak.

    CLEAR    t_final.
    CLEAR    lv_box1.
    CLEAR    lv_box2.
    CLEAR    lv_box3.
    CLEAR    lv_box4.
    CLEAR    lv_box5.
    CLEAR    lv_box6.
    CLEAR    lv_box7.
    CLEAR    lv_box14.
    CLEAR    lv_fl_out.

* Read the associated tables
    CLEAR          t_lfbw.
    READ     TABLE t_lfbw          WITH KEY lifnr = t_bsak-lifnr
                                            bukrs = t_bsak-bukrs
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CONTINUE.
    ENDIF.

    CLEAR          t_add.
    READ     TABLE t_add           WITH KEY lifnr = t_bsak-lifnr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR        t_add.
    ENDIF.

    CLEAR          t_adrc.
    CLEAR          t_t001.
    READ     TABLE t_t001          WITH KEY bukrs = t_bsak-bukrs
                              BINARY SEARCH.
    IF     ( sy-subrc EQ 0 ).
      READ   TABLE t_adrc          WITH KEY adrnr = t_t001-adrnr
                              BINARY SEARCH.
      IF   ( sy-subrc NE 0 ).
        CLEAR      t_adrc.
        CLEAR      t_t001.
      ENDIF.
    ELSE.
      CLEAR        t_adrc.
      CLEAR        t_t001.
    ENDIF.

*eject
* Reset the starting index position to the last index position read
    CLEAR                                   lv_idx1.
    MOVE     lv_tabix                    TO lv_idx1.

* Set the clearing document key
    CLEAR                                   lv_key1.
    MOVE     t_bsak-bukrs                TO lv_key1+00(04).
    MOVE     t_bsak-augbl                TO lv_key1+04(10).

* Loop through the withholding items
    CLEAR    t_witem.
    LOOP AT  t_witem                   FROM lv_idx1 TO lv_idx2.
      lv_tabix = sy-tabix.

* Set the withholding item key
      CLEAR                                 lv_key2.
      MOVE     t_witem-bukrs             TO lv_key2+00(04).
      MOVE     t_witem-belnr             TO lv_key2+04(10).

* Exit loop if the withholding item key exceeds the clearing doc key
      IF       ( lv_key1                 LT lv_key2 ).
        EXIT.
      ELSEIF   ( lv_key1                 GT lv_key2 ).
      ELSEIF   ( lv_key1                 EQ lv_key2 ).

        IF     ( t_witem-qsshh           EQ 0 ).
        ELSE.

          IF     ( t_final-ein           IS INITIAL ).
            MOVE   t_lfbw-tin            TO t_final-tin.
            MOVE   t_bsak-bukrs          TO t_final-ein.
            MOVE   t_bsak-lifnr          TO t_final-ven.
            MOVE   t_add-name            TO t_final-name.
            MOVE   t_add-street          TO t_final-addr.
            MOVE   t_add-city            TO t_final-city.
            MOVE   t_add-state           TO t_final-state.
            MOVE   t_add-zip             TO t_final-zip.
            MOVE   t_adrc-region         TO t_final-astate.
          ENDIF.

*eject
          CASE     t_witem-wt_withcd.
            WHEN   '01'.
              ADD  t_witem-qsshh         TO lv_box1.
              ADD  t_witem-qsshh         TO w_amt.
              MOVE 'X'                   TO lv_fl_out.
            WHEN   '02'.
              ADD  t_witem-qsshh         TO lv_box2.
              ADD  t_witem-qsshh         TO w_amt.
              MOVE 'X'                   TO lv_fl_out.
            WHEN   '03'.
              ADD  t_witem-qsshh         TO lv_box3.
              ADD  t_witem-qsshh         TO w_amt.
              MOVE 'X'                   TO lv_fl_out.
            WHEN   '04'.
              ADD  t_witem-qsshh         TO lv_box4.
              ADD  t_witem-qsshh         TO w_amt.
              MOVE 'X'                   TO lv_fl_out.
            WHEN   '06'.
              ADD  t_witem-qsshh         TO lv_box6.
              ADD  t_witem-qsshh         TO w_amt.
              MOVE 'X'                   TO lv_fl_out.
            WHEN   '07'.
              ADD  t_witem-qsshh         TO lv_box7.
              ADD  t_witem-qsshh         TO w_amt.
              MOVE 'X'                   TO lv_fl_out.
            WHEN   '14'.
              ADD  t_witem-qsshh         TO lv_box14.
              ADD  t_witem-qsshh         TO w_amt.
              MOVE 'X'                   TO lv_fl_out.
            WHEN OTHERS.
          ENDCASE.

        ENDIF.

      ENDIF.

      CLEAR  t_witem.
    ENDLOOP.

    IF   ( ( t_final-ein                 IS NOT INITIAL ) AND
           ( lv_fl_out                   IS NOT INITIAL )     ).

      PERFORM  f_format_amount     USING    lv_box1
                                   CHANGING t_final-box1.
      PERFORM  f_format_amount     USING    lv_box2
                                   CHANGING t_final-box2.
      PERFORM  f_format_amount     USING    lv_box3
                                   CHANGING t_final-box3.
      PERFORM  f_format_amount     USING    lv_box4
                                   CHANGING t_final-box4.
      PERFORM  f_format_amount     USING    lv_box6
                                   CHANGING t_final-box6.
      PERFORM  f_format_amount     USING    lv_box7
                                   CHANGING t_final-box7.
      PERFORM  f_format_amount     USING    lv_box14
                                   CHANGING t_final-box14.

      APPEND   t_final.

      ADD      1 TO w_rec.

*eject
      CLEAR          t_bsak2.
      READ     TABLE t_bsak2       WITH KEY bukrs = t_bsak-bukrs
                              BINARY SEARCH.
      lv_subrc  = sy-subrc.
      lv_tabix_2 = sy-tabix.
      IF     ( lv_subrc EQ 0 ).
        ADD    1 TO w_pay.
        DELETE t_bsak2 INDEX lv_tabix_2.
      ENDIF.

      CLEAR          t_bsak3.
      READ     TABLE t_bsak3       WITH KEY lifnr = t_bsak-lifnr
                              BINARY SEARCH.
      lv_subrc  = sy-subrc.
      lv_tabix_2 = sy-tabix.
      IF     ( lv_subrc EQ 0 ).
        ADD    1 TO w_payee.
        DELETE t_bsak3 INDEX lv_tabix_2.
      ENDIF.

    ENDIF.

    CLEAR  t_bsak.
  ENDLOOP.

  t_controls-desc = 'Total Records'.
  t_controls-amt  = w_rec.
  APPEND   t_controls.
  t_controls-desc = 'Total Payers'.
  t_controls-amt  = w_pay.
  APPEND   t_controls.
  t_controls-desc = 'Total Payees'.
  t_controls-amt  = w_payee.
  APPEND   t_controls.
  t_controls-desc = 'Total Dollars'.

  PERFORM  f_format_amount         USING    w_amt
                                   CHANGING t_controls-amt.

  APPEND   t_controls.

ENDFORM.                    " merge_data                    "DECK915499
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_amount                               "DECK915499
*&---------------------------------------------------------------------*
*       Format the amount
*----------------------------------------------------------------------*
FORM f_format_amount                                        "DECK915499
  USING    iv_amount TYPE  wt_bs
  CHANGING cv_amount TYPE char20.

  CLEAR          cv_amount.

  IF           ( iv_amount          IS INITIAL ).
    RETURN.
  ENDIF.

  MOVE           iv_amount          TO cv_amount.

  IF           ( cv_amount          CS '-' ).
    TRANSLATE    cv_amount       USING '- '.
    CONDENSE     cv_amount     NO-GAPS.
    SHIFT        cv_amount    RIGHT BY 1 PLACES.
    MOVE '-'  TO cv_amount+0(1).
  ENDIF.

  SHIFT          cv_amount    RIGHT DELETING TRAILING SPACE.

ENDFORM.                    " f_format_amount               "DECK915499
