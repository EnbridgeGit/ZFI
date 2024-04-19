*&---------------------------------------------------------------------*
*& Report  ZFI_CONVEY_1042_RPT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFI_CONVEY_1042_RPT MESSAGE-ID ZFI_WORKFLOW.

TABLES: bsak,                                               "DECK915396
        lfbw.                                               "DECK915396

* Begin changes - insert code                               "DECK915396

TYPES: BEGIN OF ty_wa_t001,                      "Company Codes        "
        bukrs            TYPE bukrs,             "Company Code         "
        land1            TYPE land1,             "Country Key          "
       END   OF ty_wa_t001.

TYPES: BEGIN OF ty_wa_lfbw,                      "Vendor Withholding Tax
        lifnr            TYPE lifnr,             "Vendor Account Number"
        bukrs            TYPE bukrs,             "Company Code         "
        witht            TYPE witht,             "Withholding Tax Type "
        qsrec            TYPE wt_qsrec,          "Vendor Recipient Type"
        tin              TYPE wt_wtstcd,         "Withholding Tax ID Nbr
        tcode            TYPE wt_withcd,         "Withholding Tax Code "
        exempt           TYPE wt_exnr,           "Exemptn Certificte Nbr
       END   OF ty_wa_lfbw.

TYPES: BEGIN OF ty_wa_bukrs_lifnr,               "Vendor Company Key
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
       END   OF ty_wa_bukrs_lifnr.

TYPES: BEGIN OF ty_wa_bsak,                      "AP Cleared Documents "
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        umsks            TYPE umsks,             "Special G/L Trans Type
        umskz            TYPE umskz,             "Special G/L Indicator"
        augdt            TYPE augdt,             "Clearing Date        "
        augbl            TYPE augbl,             "Clearing Document Nbr"
        zuonr            TYPE dzuonr,            "Assignment           "
        gjahr            TYPE gjahr,             "Fiscal Year          "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        buzei            TYPE buzei,             "Accounting Doc Item  "
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        qsshb            TYPE qsshb,             "Withhldng Tax Base Amt
        qbshb            TYPE qbshb,             "Withhldng Tax Amount "
       END   OF ty_wa_bsak.

TYPES: BEGIN OF ty_wa_with_item,                 "Acct.Item Withholding"
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Accounting Line Item "
        witht            TYPE witht,             "Withhldng Tax Type Ind
        qsshh            TYPE wt_bs,             "Withhldng Tax Base Amt
        qbshh            TYPE wt_bs1,            "Withhldng Tax Base Amt
        qsrec            TYPE wt_qsrec,          "Recipient Type       "
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

* End changes   - insert code                               "DECK915396

DATA: Begin of T_FINAL OCCURS 0,
       PTIN        TYPE CHAR5,
       TIN         TYPE CHAR15,
       ACCT        TYPE CHAR15,
       NAME        TYPE CHAR40,
       ADDR        TYPE CHAR40,
       CITY        TYPE CHAR20,
       ZIP         TYPE CHAR10,
       RCODE       TYPE CHAR10,
       R4CODE      TYPE CHAR10,
       COUNTRY     TYPE CHAR12,
       RCOUNTRY    TYPE CHAR15,
       REGION      TYPE CHAR12,
       DESC        TYPE CHAR15,
       ICODE       TYPE CHAR12,
       BOXCH3      TYPE CHAR6,
       ECODE       TYPE CHAR12,
       WRATE       TYPE CHAR20,
       BOXCH4      TYPE CHAR6,
       CH4ECODE    TYPE CHAR13,
       CH4WHRATE   TYPE CHAR20,
       AMT         TYPE CHAR20,
       ACTAMTWH    TYPE char20,
       WAMT        TYPE CHAR20,
       SALLOW      TYPE CHAR20,
       SWH         TYPE CHAR20,
       RAMT        TYPE CHAR20,
       PIND        TYPE CHAR20,
       INTER       TYPE CHAR20,
       NQTIN       TYPE CHAR20,
       NQTYP       TYPE CHAR20,
       NQNAME      TYPE CHAR20,
       NQNAME2     TYPE CHAR20,
       NQNAME3     TYPE CHAR20,
       NQADD       TYPE CHAR15,
       NQADD2      TYPE CHAR15,
       NQCTY       TYPE CHAR15,
       NQST        TYPE CHAR15,
       NQZIP       TYPE CHAR15,
       NQCOU       TYPE CHAR15,
       NQCCODE     TYPE CHAR15,
       NQPROV      TYPE CHAR15,
       SPID        TYPE CHAR15,
       SNAME       TYPE CHAR15,
       SNAME2      TYPE CHAR15,
       SEIN        TYPE CHAR15,
       FRGNID      TYPE CHAR15,
       WHAGTS      TYPE CHAR15,
       TTLWH       TYPE CHAR20,
       STINTYP     TYPE CHAR15,
       AMTWHIND    TYPE CHAR15,
      END OF T_FINAL.

DATA: Begin of T_controls OCCURS 0,
       DESC TYPE CHAR30,
       AMT TYPE CHAR20,
      END OF T_Controls.

* Begin changes - replace code                              "DECK915396

DATA:   gwa_t001 TYPE ty_wa_t001.

DATA: BEGIN OF T_T001 OCCURS 0.
        INCLUDE STRUCTURE gwa_t001.
*     BUKRS LIKE T001-BUKRS,
*     LAND1 LIKE T001-LAND1,
DATA: END OF T_T001.

DATA:   gwa_lfbw TYPE ty_wa_lfbw.

DATA: BEGIN OF T_LFBW OCCURS 0.
        INCLUDE STRUCTURE gwa_lfbw.
*     lifnr like lfbw-lifnr,
*     bukrs like lfbw-bukrs,
*     QSREC LIKE LFBW-QSREC,
*     TIN   like lfbw-WT_WTSTCD,
*     TCODE LIKE lfbw-WT_WITHCD,
*     EXEMPT LIKE LFBW-WT_EXNR,
DATA: END OF T_LFBW.

DATA:   gwa_bukrs_lifnr TYPE ty_wa_bukrs_lifnr.

DATA: BEGIN OF T_bukrs_lifnr OCCURS 0.
        INCLUDE STRUCTURE gwa_bukrs_lifnr.
DATA: END OF T_bukrs_lifnr.

DATA:   gwa_bsak TYPE ty_wa_bsak.

DATA: BEGIN OF T_BSAK OCCURS 0.
        INCLUDE STRUCTURE gwa_bsak.
*     BUKRS LIKE BSAK-BUKRS,
*     LIFNR LIKE BSAK-LIFNR,
*     AUGBL LIKE BSAK-AUGBL,
*     BELNR LIKE BSAK-BELNR,
*     QSSHB LIKE BSAK-QSSHB,
*     QBSHB LIKE BSAK-QBSHB,
DATA: END OF T_BSAK.

DATA: BEGIN OF T_BSAK2 OCCURS 0.
        INCLUDE STRUCTURE gwa_bsak.
*     BUKRS LIKE BSAK-BUKRS,
*     LIFNR LIKE BSAK-LIFNR,
*     AUGBL LIKE BSAK-AUGBL,
*     BELNR LIKE BSAK-BELNR,
*     QSSHB LIKE BSAK-QSSHB,
*     QBSHB LIKE BSAK-QBSHB,
DATA: END OF T_BSAK2.

DATA: BEGIN OF T_BSAK3 OCCURS 0.
        INCLUDE STRUCTURE gwa_bsak.
*     BUKRS LIKE BSAK-BUKRS,
*     LIFNR LIKE BSAK-LIFNR,
*     AUGBL LIKE BSAK-AUGBL,
*     BELNR LIKE BSAK-BELNR,
*     QSSHB LIKE BSAK-QSSHB,
*     QBSHB LIKE BSAK-QBSHB,
DATA: END OF T_BSAK3.

DATA:   gwa_with_item TYPE ty_wa_with_item.

DATA: BEGIN OF T_WITEM OCCURS 0.
        INCLUDE STRUCTURE gwa_with_item.
*     BUKRS LIKE WITH_ITEM-BUKRS,
*     BELNR LIKE WITH_ITEM-BELNR,
*     GJAHR LIKE WITH_ITEM-GJAHR,
*     QSSHH LIKE WITH_ITEM-WT_QSSHH,
*     QBSHH LIKE WITH_ITEM-WT_QBSHH,
DATA: END OF T_WITEM.

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

* End changes   - replace code                              "DECK915396

DATA: W_IND TYPE I.

SELECTION-SCREEN: SKIP 1.                                   "DECK915396
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1. "15396
SELECT-OPTIONS:   s_bukrs  FOR bsak-bukrs.                  "DECK915396
SELECT-OPTIONS:   s_lifnr  FOR bsak-lifnr OBLIGATORY.       "DECK915396
SELECT-OPTIONS:   s_belnr  FOR bsak-belnr.                  "DECK915396
SELECT-OPTIONS:   s_gjahr  FOR bsak-gjahr.                  "DECK915396
SELECT-OPTIONS:   s_budat  FOR bsak-budat.                  "DECK915396
SELECT-OPTIONS:   s_blart  FOR bsak-blart.                  "DECK915396
SELECT-OPTIONS:   s_witht  FOR lfbw-witht.                  "DECK915396
SELECTION-SCREEN: END   OF BLOCK ssb1.                      "DECK915396
SELECTION-SCREEN: SKIP 1.                                   "DECK915396
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2. "15396
PARAMETERS:       p_file   TYPE localfile OBLIGATORY,       "DECK915396
                  p_file2  TYPE localfile OBLIGATORY.       "DECK915396
SELECTION-SCREEN: END   OF BLOCK ssb2.                      "DECK915396

INITIALIZATION.                                             "DECK915396
  PERFORM  f_initialization.                                "DECK915396

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
FORM GET_DOCUMENT_DATA_OLD.                                 "DECK915396
* R_TYPE-SIGN = 'I'.
* R_TYPE-OPTION = 'EQ'.
* R_TYPE-LOW = '03'.
* APPEND R_TYPE.
* R_TYPE-LOW = '42'.
* APPEND R_TYPE.

  SELECT BUKRS LAND1 FROM T001 INTO TABLE T_T001
    WHERE BUKRS IN S_BUKRS AND
          LAND1 = 'CA'.

  SELECT LIFNR BUKRS QSREC WT_WTSTCD WT_WITHCD WT_EXNR FROM LFBW INTO TABLE T_LFBW
     WHERE LIFNR IN S_LIFNR AND
           BUKRS IN S_BUKRS AND
           WITHT IN S_WITHT.
  IF SY-SUBRC = 0.
    SELECT BUKRS LIFNR AUGBL BELNR SHKZG QSSHB QBSHB FROM BSAK INTO TABLE T_BSAK
      FOR ALL ENTRIES IN T_LFBW
      WHERE BUKRS = T_LFBW-BUKRS AND
            LIFNR = T_LFBW-LIFNR AND
            GJAHR IN S_GJAHR AND
            BELNR IN S_BELNR AND
            BUDAT IN S_BUDAT AND
            BLART NE 'ZP'.

    IF SY-SUBRC = 0.
      LOOP AT T_BSAK.
        W_IND = SY-TABIX.
        READ TABLE T_T001 WITH KEY BUKRS = T_BSAK-BUKRS.
        IF SY-SUBRC = 0.
          DELETE T_BSAK WHERE BUKRS = T_BSAK-BUKRS.
          CONTINUE.
        ENDIF.
        IF T_BSAK-AUGBL = T_BSAK-BELNR.
          DELETE T_BSAK INDEX W_IND.
        ENDIF.
      ENDLOOP.
      T_BSAK2[] = T_BSAK[].
      T_BSAK3[] = T_BSAK[].

      SELECT BUKRS BELNR GJAHR WT_QSSHH WT_QBSHH FROM WITH_ITEM INTO TABLE T_WITEM
        FOR ALL ENTRIES IN T_BSAK
        WHERE BUKRS = T_BSAK-BUKRS AND
              BELNR = T_BSAK-BELNR AND
              GJAHR IN S_GJAHR.
    ENDIF.

    SELECT LIFNR LAND1 NAME1 ORT01 PSTLZ REGIO STRAS FROM LFA1 INTO TABLE T_ADD
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
FORM CREATE_HEADER_OLD.                                     "DECK915697

  CLEAR T_FINAL.
  T_FINAL-PTIN = 'PTIN'.
  T_FINAL-TIN = 'TIN'.
  T_FINAL-ACCT = 'ACCOUNTNO'.
  T_FINAL-NAME = 'Name'.
  T_FINAL-ADDR = 'Address'.
  T_FINAL-CITY = 'City'.
  T_FINAL-ZIP = 'ZIP'.
  T_FINAL-RCODE = 'RECIPCODE'.
  T_FINAL-COUNTRY = 'CountryCode'.
  T_FINAL-RCOUNTRY = 'ResCountryCode'.
  T_FINAL-DESC = 'DESCRIPTION'.
  T_FINAL-ICODE = 'INCOMECODE'.
  T_FINAL-ECODE = 'EXEMPTCODE'.
  T_FINAL-AMT = 'PAYMENTAMT'.
  T_FINAL-WAMT = 'WITHHELDAMT'.
  T_FINAL-WRATE = 'WITHHOLDRATE'.
  T_FINAL-SALLOW = 'STUDENTALLOWANCE'.
  T_FINAL-SWH = 'STATEWITHHELD'.
  T_FINAL-RAMT = 'REPAYMENTAMT'.
  T_FINAL-PIND = 'PRORATAIND'.
  T_FINAL-INTER = 'INTERMEDIARY'.
  T_FINAL-NQTIN = 'NQTIN'.
  T_FINAL-NQTYP = 'NQTINTYPE'.
  T_FINAL-NQNAME = 'NQNAME'.
  T_FINAL-NQNAME2 = 'NQNAME2'.
  T_FINAL-NQNAME3 = 'NQNAME3'.
  T_FINAL-NQADD = 'NQADDRESS'.
  T_FINAL-NQADD2 = 'NQADDRESS2'.
  T_FINAL-NQCTY = 'NQCITY'.
  T_FINAL-NQST = 'NQSTATE'.
  T_FINAL-NQZIP = 'NQZIP'.
  T_FINAL-NQCOU = 'NQCOUNTRY'.
  T_FINAL-NQCCODE = 'NQCOUNTRYCODE'.
  T_FINAL-NQPROV = 'NQPROVINCE'.
  T_FINAL-SPID = 'SPID'.
  T_FINAL-SNAME = 'SNAME'.
  T_FINAL-SNAME2 = 'SNAME2'.
  T_FINAL-SEIN = 'SEIN'.
  T_FINAL-FRGNID = 'FRGNID'.
  T_FINAL-WHAGTS = 'WHAGTS'.
  T_FINAL-TTLWH = 'TTLWHCRDT'.
  T_FINAL-STINTYP = 'STINTYPE'.
  T_FINAL-AMTWHIND = 'AMTWITHHELDIND'.
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
FORM MERGE_DATA_OLD.                                        "DECK915697
  DATA: W_REC TYPE I,
        W_PAY TYPE I,
        W_PAYEE TYPE I,
        W_AMT LIKE BSAK-QSSHB.

  DATA: lv_subrc TYPE sysubrc,                              "DECK915396
        lv_tabix TYPE sytabix.                              "DECK915396

  CLEAR: W_PAYEE, w_pay.

  CLEAR    T_BSAK.                                          "DECK915396
  LOOP AT  T_BSAK.                                          "DECK915396

    CLEAR          t_witem.                                 "DECK915396
    READ     TABLE t_witem         WITH KEY bukrs = t_bsak-bukrs "15396
                                            belnr = t_bsak-belnr "15396
                                            gjahr = t_bsak-gjahr "15396
                              BINARY SEARCH.                "DECK915396
    IF SY-SUBRC = 0 AND T_WITEM-QSSHH = 0.
      CONTINUE.
    ELSEIF SY-SUBRC NE 0.
      CONTINUE.
    ELSE.
      T_BSAK-QSSHB = T_WITEM-QSSHH * -1.                    "DECK915396
      T_BSAK-QBSHB = T_WITEM-QBSHH * -1.                    "DECK915396
*     T_BSAK-QSSHB = ABS( T_WITEM-QSSHH ).                  "DECK915396
*     T_BSAK-QBSHB = ABS( T_WITEM-QBSHH ).                  "DECK915396
    ENDIF.

    CLEAR: T_FINAL.

    CLEAR          t_lfbw.                                  "DECK915396
    READ     TABLE t_lfbw          WITH KEY lifnr = t_bsak-lifnr "15396
                                            bukrs = t_bsak-bukrs "15396
                              BINARY SEARCH.                "DECK915396
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    CLEAR          t_add.                                   "DECK915396
    READ     TABLE t_add           WITH KEY lifnr = t_bsak-lifnr "15396
                              BINARY SEARCH.                "DECK915396
    IF     ( sy-subrc NE 0 ).                               "DECK915396
      CLEAR        t_add.                                   "DECK915396
    ENDIF.                                                  "DECK915396

    CLEAR          t_bsak2.                                 "DECK915396
    READ     TABLE t_bsak2         WITH KEY bukrs = t_bsak-bukrs "15396
                              BINARY SEARCH.                "DECK915396
    lv_subrc = sy-subrc.                                    "DECK915396
    lv_tabix = sy-tabix.                                    "DECK915396
    IF     ( lv_subrc EQ 0 ).                               "DECK915396
      ADD    1 TO w_pay.                                    "DECK915396
      DELETE t_bsak2 INDEX lv_tabix.                        "DECK915396
    ENDIF.                                                  "DECK915396

    CLEAR          t_bsak3.                                 "DECK915396
    READ     TABLE t_bsak3         WITH KEY lifnr = t_bsak-lifnr "15396
                              BINARY SEARCH.                "DECK915396
    lv_subrc = sy-subrc.                                    "DECK915396
    lv_tabix = sy-tabix.                                    "DECK915396
    IF     ( lv_subrc EQ 0 ).                               "DECK915396
      ADD    1 TO w_payee.                                  "DECK915396
      DELETE t_bsak3 INDEX lv_tabix.                        "DECK915396
    ENDIF.                                                  "DECK915396

    T_FINAL-PTIN = T_BSAK-BUKRS.
    T_FINAL-TIN = T_LFBW-TIN.
    T_FINAL-ACCT = T_BSAK-LIFNR.
    T_FINAL-NAME = T_ADD-NAME.
    T_FINAL-ADDR = T_ADD-STREET.
    T_FINAL-CITY = T_ADD-CITY.
    T_FINAL-ZIP = T_ADD-ZIP.
    T_FINAL-RCODE = T_LFBW-QSREC.
    T_FINAL-COUNTRY = T_ADD-LAND1.
    T_FINAL-RCOUNTRY = T_ADD-LAND1.
    T_FINAL-DESC = T_BSAK-BELNR.
    T_FINAL-ICODE = T_LFBW-TCODE.

    IF T_ADD-LAND1 = 'CA'.
      MOVE     t_add-state            TO t_final-nqprov.    "DECK915396
*     SELECT SINGLE REGIO FROM LFA1 INTO T_FINAL-NQPROV     "DECK915396
*       WHERE LIFNR = T_BSAK-LIFNR.                         "DECK915396
    ENDIF.

    IF T_LFBW-EXEMPT IS INITIAL.
      T_FINAL-ECODE = '04'.
    ELSE.
      T_FINAL-ECODE = T_LFBW-EXEMPT.
    ENDIF.

    T_FINAL-AMT = T_BSAK-QSSHB.

    PERFORM  f_format_amount          USING t_final-amt.    "DECK915420

    T_FINAL-WAMT = T_BSAK-QBSHB.

    PERFORM  f_format_amount          USING t_final-wamt.   "DECK915420

    T_FINAL-WRATE = T_BSAK-QBSHB / T_BSAK-QSSHB.

    PERFORM  f_format_amount          USING t_final-wrate.  "DECK915420

    T_FINAL-TTLWH = T_BSAK-QBSHB.

    PERFORM  f_format_amount          USING t_final-ttlwh.  "DECK915420

    APPEND T_FINAL.
    W_REC = W_REC + 1.
    W_AMT = W_AMT + T_BSAK-QSSHB.

    CLEAR  T_BSAK.                                          "DECK915396
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

  PERFORM  f_format_amount          USING t_controls-amt.   "DECK915420

  APPEND T_CONTROLS.

ENDFORM.                    " MERGE_DATA
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initialization                              "DECK915396
*&---------------------------------------------------------------------*
*       Initialization
*----------------------------------------------------------------------*
FORM f_initialization.

  DATA:    lwa_blart                   LIKE LINE OF s_blart,
           lwa_witht                   LIKE LINE OF s_witht.

  CLEAR    t_final[].
  CLEAR    t_controls[].
  CLEAR    t_t001[].
  CLEAR    t_lfbw[].
  CLEAR    t_bukrs_lifnr[].
  CLEAR    t_bsak[].
  CLEAR    t_bsak2[].
  CLEAR    t_bsak3[].
  CLEAR    t_witem[].
  CLEAR    t_add[].

  CLEAR                                     lwa_blart.
  MOVE     'E'                           TO lwa_blart-sign.
  MOVE     'EQ'                          TO lwa_blart-option.
  MOVE     'ZP'                          TO lwa_blart-low.
  APPEND   lwa_blart                     TO s_blart.

  CLEAR                                     lwa_witht.
  MOVE     'I'                           TO lwa_witht-sign.
  MOVE     'EQ'                          TO lwa_witht-option.
  MOVE     '03'                          TO lwa_witht-low.
  APPEND   lwa_witht                     TO s_witht.
  CLEAR                                     lwa_witht-low.
  MOVE     '04'                          TO lwa_witht-low.
  APPEND   lwa_witht                     TO s_witht.
  CLEAR                                     lwa_witht-low.
  MOVE     '05'                          TO lwa_witht-low.
  APPEND   lwa_witht                     TO s_witht.
  CLEAR                                     lwa_witht-low.
  MOVE     '42'                          TO lwa_witht-low.
  APPEND   lwa_witht                     TO s_witht.
  CLEAR                                     lwa_witht-low.
  MOVE     '43'                          TO lwa_witht-low.
  APPEND   lwa_witht                     TO s_witht.
  CLEAR                                     lwa_witht-low.
  MOVE     '44'                          TO lwa_witht-low.
  APPEND   lwa_witht                     TO s_witht.

ENDFORM.                    " f_initialization              "DECK915396
*eject
*&---------------------------------------------------------------------*
*&      Form  get_document_data                             "DECK915396
*&---------------------------------------------------------------------*
*       Select the document data
*----------------------------------------------------------------------*
FORM get_document_data.                                     "DECK915396

  DATA:    t_bsak_p  LIKE t_bsak  OCCURS 0 WITH HEADER LINE,
           t_witem_p LIKE t_witem OCCURS 0 WITH HEADER LINE.

  DATA:    lv_tabix  TYPE sytabix.

  CLEAR    t_bsak_p[].
  CLEAR    t_witem_p[].

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

  LOOP AT  t_lfbw.
    CLEAR                                   t_bukrs_lifnr.
    MOVE   t_lfbw-bukrs                  TO t_bukrs_lifnr-bukrs.
    MOVE   t_lfbw-lifnr                  TO t_bukrs_lifnr-lifnr.
    APPEND                                  t_bukrs_lifnr.
  ENDLOOP.

  SORT     t_bukrs_lifnr       ASCENDING BY bukrs lifnr.
  DELETE   ADJACENT DUPLICATES         FROM t_bukrs_lifnr
                                  COMPARING bukrs lifnr.

*eject
* Select the vendor invoices
  IF     ( t_bukrs_lifnr[] IS NOT INITIAL ).
    SELECT   bukrs  lifnr  umsks  umskz  augdt  augbl  zuonr
             gjahr  belnr  buzei  shkzg  qsshb  qbshb
      INTO   TABLE t_bsak_p
      FROM   bsak FOR ALL ENTRIES IN t_bukrs_lifnr
     WHERE   bukrs  = t_bukrs_lifnr-bukrs
       AND   lifnr  = t_bukrs_lifnr-lifnr
       AND   gjahr IN s_gjahr
       AND   belnr IN s_belnr
       AND   budat IN s_budat
       AND   blart IN s_blart.
    IF     ( sy-subrc EQ 0 ).

      LOOP AT  t_bsak_p.
        lv_tabix = sy-tabix.
        IF     ( t_bsak_p-augbl          EQ t_bsak_p-belnr ).
          DELETE   t_bsak_p           INDEX lv_tabix.
        ELSE.
          CLEAR                             t_bsak_p-umsks.
          CLEAR                             t_bsak_p-umskz.
          CLEAR                             t_bsak_p-zuonr.
          MODIFY   t_bsak_p           INDEX lv_tabix.
        ENDIF.
      ENDLOOP.

      SORT     t_bsak_p        ASCENDING BY bukrs lifnr umsks umskz
                                            augdt augbl zuonr gjahr
                                            belnr buzei.

      LOOP AT  t_bsak_p.
        AT NEW belnr.
          CLEAR                             t_bsak.
          MOVE     t_bsak_p-bukrs        TO t_bsak-bukrs.
          MOVE     t_bsak_p-lifnr        TO t_bsak-lifnr.
          MOVE     t_bsak_p-augdt        TO t_bsak-augdt.
          MOVE     t_bsak_p-augbl        TO t_bsak-augbl.
          MOVE     t_bsak_p-gjahr        TO t_bsak-gjahr.
          MOVE     t_bsak_p-belnr        TO t_bsak-belnr.
        ENDAT.
        IF       ( t_bsak_p-shkzg        EQ 'H' ).
          SUBTRACT t_bsak_p-qsshb      FROM t_bsak-qsshb.
          SUBTRACT t_bsak_p-qbshb      FROM t_bsak-qbshb.
        ELSE.
          ADD      t_bsak_p-qsshb        TO t_bsak-qsshb.
          ADD      t_bsak_p-qbshb        TO t_bsak-qbshb.
        ENDIF.
        AT END OF belnr.
          APPEND                            t_bsak.
          CLEAR                             t_bsak.
        ENDAT.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CLEAR    t_bsak_p[].

*eject
* Select the accounting document withholding items
  IF     ( t_bsak[] IS NOT INITIAL ).
    SELECT   bukrs     belnr     gjahr     buzei
             witht     wt_qsshh  wt_qbshh  qsrec
      INTO   TABLE t_witem_p
      FROM   with_item FOR ALL ENTRIES IN t_bsak
     WHERE   bukrs  = t_bsak-bukrs
       AND   belnr  = t_bsak-belnr
       AND   gjahr  = t_bsak-gjahr
       AND   witht IN s_witht.
    IF     ( sy-subrc EQ 0 ).

      LOOP AT  t_witem_p.
        lv_tabix = sy-tabix.
        CLEAR                               t_witem_p-buzei.
        MODIFY   t_witem_p            INDEX lv_tabix.
      ENDLOOP.

      SORT   t_witem_p         ASCENDING BY bukrs belnr gjahr
                                            buzei witht qsrec.

      CLEAR    t_witem.
      CLEAR    t_witem_p.
      LOOP AT  t_witem_p.

        IF     ( ( t_witem_p-bukrs       NE t_witem-bukrs ) OR
                 ( t_witem_p-belnr       NE t_witem-belnr ) OR
                 ( t_witem_p-gjahr       NE t_witem-gjahr ) OR
                 ( t_witem_p-witht       NE t_witem-witht ) OR
                 ( t_witem_p-qsrec       NE t_witem-qsrec )    ).
          IF     ( t_witem-bukrs         IS NOT INITIAL ).
            APPEND                          t_witem.
          ENDIF.
          CLEAR                             t_witem.
          MOVE     t_witem_p             TO t_witem.
        ELSE.
          ADD      t_witem_p-qsshh       TO t_witem-qsshh.
          ADD      t_witem_p-qbshh       TO t_witem-qbshh.
        ENDIF.

        AT LAST.
          APPEND                            t_witem.
          CLEAR                             t_witem.
        ENDAT.

      ENDLOOP.
    ENDIF.
  ENDIF.

  CLEAR    t_witem_p[].

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

ENDFORM.                    " get_document_data             "DECK915396
*eject
*&---------------------------------------------------------------------*
*&      Form  create_header                                 "DECK915697
*&---------------------------------------------------------------------*
*       Create the column headings
*----------------------------------------------------------------------*
FORM create_header.                                         "DECK915697

  CLEAR                                     t_final.

  MOVE     text-h01                      TO t_final-ptin.
  MOVE     text-h02                      TO t_final-tin.
  MOVE     text-h03                      TO t_final-acct.
  MOVE     text-h04                      TO t_final-name.
  MOVE     text-h05                      TO t_final-addr.
  MOVE     text-h06                      TO t_final-city.
  MOVE     text-h07                      TO t_final-zip.
  MOVE     text-h08                      TO t_final-rcode.
  MOVE     text-h09                      TO t_final-r4code.
  MOVE     text-h10                      TO t_final-country.
  MOVE     text-h11                      TO t_final-rcountry.
  MOVE     text-h12                      TO t_final-region.
  MOVE     text-h13                      TO t_final-desc.
  MOVE     text-h14                      TO t_final-icode.
  MOVE     text-h15                      TO t_final-boxch3.
  MOVE     text-h16                      TO t_final-ecode.
  MOVE     text-h17                      TO t_final-wrate.
  MOVE     text-h18                      TO t_final-boxch4.
  MOVE     text-h19                      TO t_final-ch4ecode.
  MOVE     text-h20                      TO t_final-ch4whrate.
  MOVE     text-h21                      TO t_final-amt.
  MOVE     text-h22                      TO t_final-actamtwh.
  MOVE     text-h23                      TO t_final-wamt.
  MOVE     text-h24                      TO t_final-sallow.
  MOVE     text-h25                      TO t_final-swh.
  MOVE     text-h26                      TO t_final-ramt.
  MOVE     text-h27                      TO t_final-pind.
  MOVE     text-h28                      TO t_final-inter.
  MOVE     text-h29                      TO t_final-nqtin.
  MOVE     text-h30                      TO t_final-nqtyp.
  MOVE     text-h31                      TO t_final-nqname.
  MOVE     text-h32                      TO t_final-nqname2.
  MOVE     text-h33                      TO t_final-nqname3.
  MOVE     text-h34                      TO t_final-nqadd.
  MOVE     text-h35                      TO t_final-nqadd2.
  MOVE     text-h36                      TO t_final-nqcty.
  MOVE     text-h37                      TO t_final-nqst.
  MOVE     text-h38                      TO t_final-nqzip.
  MOVE     text-h39                      TO t_final-nqcou.
  MOVE     text-h40                      TO t_final-nqccode.
  MOVE     text-h41                      TO t_final-nqprov.
  MOVE     text-h42                      TO t_final-spid.
  MOVE     text-h43                      TO t_final-sname.
  MOVE     text-h44                      TO t_final-sname2.
  MOVE     text-h45                      TO t_final-sein.
  MOVE     text-h46                      TO t_final-frgnid.
  MOVE     text-h47                      TO t_final-whagts.
  MOVE     text-h48                      TO t_final-ttlwh.
  MOVE     text-h49                      TO t_final-stintyp.
  MOVE     text-h50                      TO t_final-amtwhind.

  APPEND                                    t_final.

ENDFORM.                    " create_header                 "DECK915697
*eject
*&---------------------------------------------------------------------*
*&      Form  merge_data                                    "DECK915697
*&---------------------------------------------------------------------*
*       Merge the selected data into the output table
*----------------------------------------------------------------------*
FORM merge_data.

  DATA:    w_rec        TYPE I,
           w_pay        TYPE I,
           w_payee      TYPE I,
           w_amt        TYPE qsshb.

  DATA:    lv_subrc     TYPE sysubrc,
           lv_tabix     TYPE sytabix,
           lv_wrate_p1  TYPE p LENGTH 9 DECIMALS 4,
           lv_wrate_p2  TYPE p LENGTH 9 DECIMALS 2,
           lv_wrate     TYPE char20.

  SORT     t_lfbw              ASCENDING BY lifnr bukrs.
  SORT     t_bsak              ASCENDING BY bukrs belnr gjahr.
  SORT     t_witem             ASCENDING BY bukrs belnr gjahr witht.

  CLEAR    w_rec.
  CLEAR    w_pay.
  CLEAR    w_payee.
  CLEAR    w_amt.

* Loop through the withholding tax items
  CLEAR    t_witem.
  LOOP AT  t_witem.

    IF   ( t_witem-qsshh EQ 0 ).
      CONTINUE.
    ENDIF.

    CLEAR    t_final.

* Read the associated tables
    CLEAR          t_bsak.
    READ     TABLE t_bsak          WITH KEY bukrs = t_witem-bukrs
                                            belnr = t_witem-belnr
                                            gjahr = t_witem-gjahr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CONTINUE.
    ENDIF.

    CLEAR          t_lfbw.
    READ     TABLE t_lfbw          WITH KEY lifnr = t_bsak-lifnr
                                            bukrs = t_bsak-bukrs
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CONTINUE.
    ENDIF.

*eject
    CLEAR          t_add.
    READ     TABLE t_add           WITH KEY lifnr = t_bsak-lifnr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR        t_add.
    ENDIF.

* Build the output record
    t_bsak-qsshb = t_witem-qsshh * -1.
    t_bsak-qbshb = t_witem-qbshh * -1.

    MOVE     t_bsak-bukrs                TO t_final-ptin.
    MOVE     t_lfbw-tin                  TO t_final-tin.
    MOVE     t_bsak-lifnr                TO t_final-acct.
    MOVE     t_add-name                  TO t_final-name.
    MOVE     t_add-street                TO t_final-addr.
    MOVE     t_add-city                  TO t_final-city.
    MOVE     t_add-zip                   TO t_final-zip.
    MOVE     t_add-land1                 TO t_final-country.
    MOVE     t_add-land1                 TO t_final-rcountry.
    MOVE     t_bsak-belnr                TO t_final-desc.
    MOVE     t_lfbw-tcode                TO t_final-icode.
    MOVE     '0'                         TO t_final-amtwhind.

    CLEAR    lv_wrate_p1.
    CLEAR    lv_wrate_p2.
    CLEAR    lv_wrate.

    lv_wrate_p1 = ( t_bsak-qbshb * 100 ) / t_bsak-qsshb.
    lv_wrate_p2 = ROUND( VAL = lv_wrate_p1 PREC = 2 MODE = 6 ).

    lv_wrate    = lv_wrate_p2.

    IF     ( t_add-land1                 EQ 'CA' ).
      MOVE   t_add-state                 TO t_final-region.
    ENDIF.

    IF   ( ( t_witem-witht               EQ '03' ) OR
           ( t_witem-witht               EQ '42' )    ).

      MOVE   t_witem-qsrec               TO t_final-rcode.
      MOVE   '1'                         TO t_final-boxch3.
      MOVE   lv_wrate                    TO t_final-wrate.

      IF   ( ( lv_wrate_p2               EQ 0  ) OR
             ( t_bsak-qbshb              EQ 0  )    ).
        MOVE '04'                        TO t_final-ecode.
      ELSEIF ( lv_wrate_p2               EQ 30 ).
        MOVE '00'                        TO t_final-ecode.
      ENDIF.

      MOVE   '15'                        TO t_final-ch4ecode.
      MOVE   '0'                         TO t_final-ch4whrate.

    ENDIF.

    IF   ( ( t_witem-witht               EQ '04' ) OR
           ( t_witem-witht               EQ '05' ) OR
           ( t_witem-witht               EQ '43' ) OR
           ( t_witem-witht               EQ '44' )    ).

      MOVE   t_witem-qsrec               TO t_final-r4code.
      MOVE   '1'                         TO t_final-boxch4.
      MOVE   lv_wrate                    TO t_final-ch4whrate.

      IF   ( ( lv_wrate_p2               EQ 0  ) OR
             ( t_bsak-qbshb              EQ 0  )    ).
        MOVE '15'                        TO t_final-ch4ecode.
      ELSEIF ( lv_wrate_p2               EQ 30 ).
        MOVE '00'                        TO t_final-ch4ecode.
      ENDIF.

*     MOVE   '15'                        TO t_final-ecode.
*     MOVE   '0'                         TO t_final-wrate.

    ENDIF.

*eject
    t_final-amt = t_bsak-qsshb.

    PERFORM  f_format_amount          USING t_final-amt.

    t_final-wamt = t_bsak-qbshb.

    PERFORM  f_format_amount          USING t_final-wamt.

    PERFORM  f_format_amount          USING t_final-wrate.

    PERFORM  f_format_amount          USING t_final-ch4whrate.

    t_final-ttlwh = t_bsak-qbshb.

    PERFORM  f_format_amount          USING t_final-ttlwh.

    APPEND   t_final.

    ADD      1            TO w_rec.

    ADD      t_bsak-qsshb TO w_amt.

    CLEAR          t_bsak2.
    READ     TABLE t_bsak2         WITH KEY bukrs = t_bsak-bukrs
                              BINARY SEARCH.
    lv_subrc = sy-subrc.
    lv_tabix = sy-tabix.
    IF     ( lv_subrc EQ 0 ).
      ADD      1          TO w_pay.
      DELETE   t_bsak2 INDEX lv_tabix.
    ENDIF.

    CLEAR          t_bsak3.
    READ     TABLE t_bsak3         WITH KEY lifnr = t_bsak-lifnr
                              BINARY SEARCH.
    lv_subrc = sy-subrc.
    lv_tabix = sy-tabix.
    IF     ( lv_subrc EQ 0 ).
      ADD      1          TO w_payee.
      DELETE   t_bsak3 INDEX lv_tabix.
    ENDIF.

    CLEAR  t_witem.
  ENDLOOP.

*eject
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
  t_controls-amt  = w_amt.

  PERFORM  f_format_amount            USING t_controls-amt.

  APPEND   t_controls.

ENDFORM.                    " merge_data                    "DECK915697
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_amount                               "DECK915420
*&---------------------------------------------------------------------*
*       Format the amount
*----------------------------------------------------------------------*
FORM f_format_amount                                        "DECK915420
  CHANGING cv_amount TYPE char20.

  IF           ( cv_amount CS '-' ).
    TRANSLATE    cv_amount USING '- '.
    CONDENSE     cv_amount NO-GAPS.
    SHIFT        cv_amount RIGHT BY 1 PLACES.
    MOVE '-'  TO cv_amount+0(1).
  ENDIF.

  SHIFT          cv_amount RIGHT DELETING TRAILING SPACE.

ENDFORM.                    " f_format_amount               "DECK915420
