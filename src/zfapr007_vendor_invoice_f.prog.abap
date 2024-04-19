*&---------------------------------------------------------------------*
*&  Include           ZFAPR007_VENDOR_INVOICE_F
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPR007_VENDOR_INVOICE_F                     *
*& Author             :  Sajjad Ahmad                                  *
*& Creation Date      :  Nov 09, 2011                                  *
*& Object ID          :  R_P2C_AP_007/Vendor Invoices with special     *
*                        Handeling Report                              *
*& Application Area   :  FI-AP                                         *
*& Description        :  Vendor Invoices with special Handeling Report *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  CLEAR_MEMORY
*&---------------------------------------------------------------------*
*      Sub Routine to Clear the Global
*----------------------------------------------------------------------*
FORM clear_memory.

*Clear the Global work area and Variables
  CLEAR: gwa_bkpf,
         gwa_bseg,
         gwa_lfa1,
         gwa_payr,
         gwa_final,
         gv_bkpf,
         gv_bseg,
         git_payr1.
*Clear the Global Internal Table
  REFRESH: git_bkpf,
           git_bseg,
           git_lfa1,
           git_payr,
           git_payr1,
           git_final,
           git_fcat[].
  CLEAR: gt_bsik.


ENDFORM.                    " CLEAR_MEMORY

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_VENDOR
*&---------------------------------------------------------------------*
*       Sub Routine to Validate the Vendor
*----------------------------------------------------------------------*
FORM f_validate_vendor .

*Validate the Vendor
  DATA: lit_lifnr TYPE STANDARD TABLE OF lifnr.
  IF rb_1 = gc_x.
    IF s_lifnr[] IS NOT INITIAL.
      SELECT lifnr
        FROM lfa1
        INTO TABLE lit_lifnr
        WHERE lifnr IN s_lifnr.

      IF sy-subrc NE 0.
        MESSAGE e023 WITH s_lifnr-low. "Vendor & Doesn't Exist
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_VALIDATE_VENDOR
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_COMPANYCODE
*&---------------------------------------------------------------------*
*       Sub Routine to Validate the Company Code
*----------------------------------------------------------------------*

FORM f_validate_companycode .
  CONSTANTS lc_x   TYPE char1 VALUE 'X'.
*Validate the Company Code
  DATA: lit_bukrs TYPE STANDARD TABLE OF bukrs.
  IF rb_1 = gc_x.
    IF s_bukrs[] IS NOT INITIAL.
      SELECT bukrs
       FROM t001
       INTO TABLE lit_bukrs
       WHERE bukrs IN s_bukrs.

      IF sy-subrc NE 0.
        MESSAGE e024 WITH s_bukrs-low. "Company Code & Doesn't Exist
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATE_COMPANYCODE
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_DEPARTMENT
*&---------------------------------------------------------------------*
*      Sub Routine to Validate the Department
*----------------------------------------------------------------------*

FORM f_validate_department .
*Validate the Department
  DATA: lit_kostl TYPE STANDARD TABLE OF kostl.
  IF rb_1 = gc_x.
    IF s_kostl[] IS NOT INITIAL.
      SELECT kostl
       FROM csks
       INTO TABLE lit_kostl
       WHERE kostl IN s_kostl.

      IF sy-subrc NE 0.
        MESSAGE e025 WITH s_kostl-low. "Department & Doesn't Exist
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATE_DEPARTMENT

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_FISCAL_PERIOD
*&---------------------------------------------------------------------*
*      Sub Routine to Validate the Fiscal Period
*----------------------------------------------------------------------*

FORM f_validate_fiscal_period .
*Validate Fiscal Period
  DATA: lit_monat TYPE STANDARD TABLE OF monat.
  IF rb_1 = gc_x.
    IF s_monat-low IS NOT INITIAL.
      IF s_monat-low NOT BETWEEN 1 AND 16.
        MESSAGE e027 WITH s_monat-low. "Fiscal Period should be 1 to 16
      ENDIF.
    ENDIF.
    IF s_monat-high IS NOT INITIAL.
      IF s_monat-high NOT BETWEEN 1 AND 16.
        MESSAGE e027 WITH s_monat-high. "Fiscal Period should be 1 to 16
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATE_FISCAL_PERIOD

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_DOCUMENT_TYPE
*&---------------------------------------------------------------------*
*      Sub Routine to Validate the Document Date
*----------------------------------------------------------------------*

FORM f_validate_document_type .
*Validate the Document Date
  DATA: lit_blart TYPE STANDARD TABLE OF blart.
  IF rb_1 = gc_x.
    IF s_blart[] IS NOT INITIAL.
      SELECT blart
       FROM t003
       INTO TABLE lit_blart
       WHERE blart IN s_blart.

      IF sy-subrc NE 0.
        MESSAGE e029 WITH s_blart-low. "Document Date & Doesn't Exist
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATE_DOCUMENT_TYPE
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_DOCUMENT_NUMBER
*&---------------------------------------------------------------------*
*      Sub Routine to Validate the Document Number
*----------------------------------------------------------------------*

FORM f_validate_document_number.
*Validate the Document Number
  DATA: lit_belnr TYPE STANDARD TABLE OF belnr_d.
  IF rb_1 = gc_x.
    IF s_belnr[] IS NOT INITIAL.
      IF p_gjahr IS INITIAL.
        MESSAGE e000 WITH 'Enter Fiscal Year in Selection Criteria 1' '' '' ''.
*        exit.
      ENDIF.
      SELECT belnr
       FROM bkpf
       INTO TABLE lit_belnr
       WHERE  bukrs IN s_bukrs
          AND belnr IN s_belnr
          AND gjahr = p_gjahr
          AND blart IN s_blart.

      IF sy-subrc NE 0.
        MESSAGE e030 WITH s_belnr-low. "Document Number & Doesn't Exist
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATE_DOCUMENT_NUMBER

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_REFERENCE_NUMBER
*&---------------------------------------------------------------------*
*      Sub Routine to Validate the Reference Number
*----------------------------------------------------------------------*

FORM f_validate_reference_number .
*Validate the Reference Number
  DATA: lit_xblnr TYPE STANDARD TABLE OF xblnr1.

  IF rb_1 = gc_x.
    IF s_xblnr[] IS NOT INITIAL.
      SELECT xblnr
       FROM bkpf
       INTO TABLE lit_xblnr
       WHERE  bukrs IN s_bukrs
          AND gjahr = p_gjahr
          AND blart IN s_blart
          AND xblnr IN s_xblnr.

      IF sy-subrc NE 0.
        MESSAGE e031 WITH s_xblnr-low. "Reference Number & Doesn't Exist
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATE_REFERENCE_NUMBER

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_VENDOR
*&---------------------------------------------------------------------*
*       Sub Routine to Validate the Vendor
*----------------------------------------------------------------------*
FORM f_validate_vendor1.
*Validate the Vendor
  DATA: lit_lifnr TYPE STANDARD TABLE OF lifnr.
  IF rb_2 = gc_x.
    IF s_lifnr1[] IS NOT INITIAL.
      SELECT lifnr
        FROM lfa1
        INTO TABLE lit_lifnr
        WHERE lifnr IN s_lifnr1.

      IF sy-subrc NE 0.
        MESSAGE e023 WITH s_lifnr1-low. "Vendor & Doesn't Exist
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATE_VENDOR
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_COMPANYCODE
*&---------------------------------------------------------------------*
*       Sub Routine to Validate the Company Code1
*----------------------------------------------------------------------*

FORM f_validate_companycode1 .

*Validate the Company Code
  DATA: lit_bukrs TYPE STANDARD TABLE OF bukrs.
  IF rb_2 = gc_x.
    IF s_bukrs1[] IS NOT INITIAL.
      SELECT bukrs
       FROM t001
       INTO TABLE lit_bukrs
       WHERE bukrs IN s_bukrs1.

      IF sy-subrc NE 0.
        MESSAGE e024 WITH s_bukrs1-low. "Company Code & Doesn't Exist
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATE_COMPANYCODE
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_LIST
*&---------------------------------------------------------------------*
*      Sub Routine to Prepare and Display ALV List
*----------------------------------------------------------------------*
FORM f_display_alv_list .
*Prepare ALV List Display
  DATA: lwa_layout TYPE slis_layout_alv." Layout
  TYPE-POOLS: slis.
*Constants Declartion
  CONSTANTS:  lc_bukrs   TYPE slis_fieldname VALUE 'BUKRS',
              lc_lifnr   TYPE slis_fieldname VALUE 'LIFNR',
              lc_name1   TYPE slis_fieldname VALUE 'NAME1',
              lc_xblnr   TYPE slis_fieldname VALUE 'XBLNR',
              lc_blart   TYPE slis_fieldname VALUE 'BLART',
              lc_belnr   TYPE slis_fieldname VALUE 'BELNR',
              lc_augbl   TYPE slis_fieldname VALUE 'AUGBL',
              lc_chect   TYPE slis_fieldname VALUE 'CHECT',
              lc_zlsch   TYPE slis_fieldname VALUE 'ZLSCH',
              lc_uzawe   TYPE slis_fieldname VALUE 'UZAWE',
              lc_tdline   TYPE slis_fieldname VALUE 'TDLINE',
              lc_sgtxt   type slis_fieldname VALUE 'SGTXT',
              lc_bseg    TYPE slis_fieldname VALUE 'BSIK', "'BSEG',
              lc_lfa1    TYPE slis_fieldname VALUE 'LFA1',
              lc_payr    TYPE slis_fieldname VALUE 'PAYR',
              lc_bkpf    TYPE slis_fieldname VALUE 'BKPF',
              lc_pridt   TYPE slis_fieldname VALUE 'PRIDT',
              lc_tline   TYPE slis_fieldname VALUE 'TLINE',
              lc_command TYPE slis_formname  VALUE 'USER_COMMAND',
              lc_x       TYPE char1           VALUE 'X'.

*Check the data is found in table BSEG.
*  IF gv_bseg = lc_x.

  IF rb_1 = 'X'.
*Fill the Field Catelog
    PERFORM: fill_cat USING lc_bukrs lc_bseg,
             fill_cat USING lc_lifnr lc_bseg,
             fill_cat USING lc_name1 lc_lfa1,
             fill_cat USING lc_xblnr lc_bkpf,
             fill_cat USING lc_blart lc_bkpf,
             fill_cat USING lc_belnr lc_bseg,
             fill_cat USING lc_augbl lc_bseg,
             fill_cat USING lc_chect lc_payr,
             fill_cat USING lc_zlsch lc_bseg,
             fill_cat USING lc_uzawe lc_bseg,
             fill_cat USING lc_pridt lc_payr,
             fill_cat USING lc_sgtxt lc_bseg,
             fill_cat USING lc_tdline lc_tline.
  ELSE.

    SORT git_final BY bukrs augbl chect  belnr lifnr.
*      *Fill the Field Catelog
    PERFORM: fill_cat USING lc_bukrs lc_bseg,
             fill_cat USING lc_chect lc_payr,
             fill_cat USING lc_augbl lc_bseg,
             fill_cat USING lc_belnr lc_bseg,
             fill_cat USING lc_lifnr lc_bseg,
             fill_cat USING lc_name1 lc_lfa1,
             fill_cat USING lc_xblnr lc_bkpf,
             fill_cat USING lc_blart lc_bkpf,
             fill_cat USING lc_zlsch lc_bseg,
             fill_cat USING lc_uzawe lc_bseg,
             fill_cat USING lc_pridt lc_payr,
             fill_cat USING lc_sgtxt lc_bseg,
             fill_cat USING lc_tdline lc_tline.
  ENDIF.

*Optimize the coloumn width
*    lwa_layout-colwidth_optimize     = lc_x.
*Display the Report in ALV with Intercative
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      it_fieldcat             = git_fcat[]
      i_callback_user_command = lc_command
      is_layout               = lwa_layout
    TABLES
      t_outtab                = git_final.
*  ENDIF.
ENDFORM.                    " F_DISPLAY_ALV_LIST
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*     Sub Routine Call the Transaction FB03 With the corresponding docNo
*----------------------------------------------------------------------*
*      -->P_UCOMM      Function cod
*      -->P_SELFIELD   Structure with selected line value
*----------------------------------------------------------------------*
FORM user_command USING p_ucomm    TYPE sy-ucomm
                        p_selfield TYPE slis_selfield.
*Call the Transaction FB03 With the corresponding document No.
  DATA: lwa_final TYPE ty_final.
*Constants Declartion
  CONSTANTS: lc_belnr  TYPE slis_fieldname VALUE 'BELNR',
             lc_chect  TYPE slis_fieldname VALUE 'CHECT',
             lc_xblnr  TYPE slis_fieldname VALUE 'XBLNR',
             lc_ic1    TYPE sy-ucomm       VALUE '&IC1',
             lc_buk    TYPE char3          VALUE 'BUK',
             lc_bln    TYPE char3          VALUE 'BLN',
             lc_gjr    TYPE char3          VALUE 'GJR',
             lc_chk    TYPE char3          VALUE 'CHK',
             lc_hbk    TYPE char3          VALUE 'HBK',
             lc_hkt    TYPE char3          VALUE 'HKT',
             lc_fb03   TYPE char4          VALUE 'FB03',
             lc_fch1   TYPE char4          VALUE 'FCH1'.
*Call the Tcode FB03 when clicked on document / reference document No.
  IF p_selfield-fieldname = lc_belnr OR
     p_selfield-fieldname = lc_xblnr AND
                  p_ucomm = lc_ic1.

    READ TABLE git_final INTO lwa_final INDEX p_selfield-tabindex.
    IF sy-subrc = 0 .
      SET PARAMETER ID: lc_buk FIELD lwa_final-bukrs,
                        lc_bln FIELD lwa_final-belnr,
                        lc_gjr FIELD p_gjahr.
*Call the Transaction FB03
      CALL TRANSACTION lc_fb03 AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.

*Call the Tcode FCH1 when clicked on Check Number.
  IF p_selfield-fieldname = lc_chect AND
                  p_ucomm = lc_ic1.

    READ TABLE git_final INTO lwa_final INDEX p_selfield-tabindex.
    IF sy-subrc = 0 .
      SET PARAMETER ID: lc_buk FIELD lwa_final-bukrs,
                        lc_chk FIELD lwa_final-chect,
                        lc_hbk FIELD lwa_final-hbkid,
                        lc_hkt FIELD lwa_final-hktid.
*Call the Transaction FCH1
      CALL TRANSACTION lc_fch1 AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FILL_CAT
*&---------------------------------------------------------------------*
*       Sub Routine to Fill the Field Catelog
*----------------------------------------------------------------------*
*      -->P_fieldname     Field Name
*      -->P_tabname       Reference Field
*----------------------------------------------------------------------*
FORM fill_cat USING  p_fieldname TYPE slis_fieldname
                     p_tabname   TYPE slis_tabname.

  DATA: lwa_fcat TYPE slis_fieldcat_alv.
*Constants Declartion
  CONSTANTS: lc_belnr  TYPE slis_fieldname VALUE 'BELNR',
             lc_xblnr  TYPE slis_fieldname VALUE 'XBLNR',
             lc_chect  TYPE slis_fieldname VALUE 'CHECT',
             lc_final  TYPE slis_fieldname VALUE 'GIT_FINAL',
             lc_x      TYPE char1          VALUE 'X',
             lc_tdline   TYPE slis_fieldname VALUE 'TDLINE',
             lc_sgtxt  type  slis_fieldname VALUE 'SGTXT'.

  gv_count = gv_count + 1.

  lwa_fcat-col_pos       = gv_count.
  lwa_fcat-fieldname     = p_fieldname.
  lwa_fcat-tabname       = lc_final.
  lwa_fcat-ref_fieldname = p_fieldname.
  lwa_fcat-ref_tabname   = p_tabname.
  IF p_fieldname = lc_tdline.
    lwa_fcat-outputlen = 60.
    lwa_fcat-seltext_l = 'Item Long Text'.
    lwa_fcat-seltext_m = 'Item Long Text'.
    lwa_fcat-seltext_s = 'Item Long Text'.
  ENDIF.
  IF p_fieldname = lc_sgtxt.
    lwa_fcat-outputlen = 20.
    lwa_fcat-seltext_l = 'Document Type'.
    lwa_fcat-seltext_m = 'Document Type'.
    lwa_fcat-seltext_s = 'Document Type'.
  ENDIF.
  IF p_fieldname = lc_belnr OR
     p_fieldname = lc_xblnr OR
     p_fieldname = lc_chect.
    lwa_fcat-hotspot = lc_x.
  ENDIF.
  APPEND lwa_fcat TO git_fcat.
  CLEAR lwa_fcat.
ENDFORM.                    " FILL_CAT
*&---------------------------------------------------------------------*
*&      Form  F_NO_DATA_FOUND
*&---------------------------------------------------------------------*
*      Sub Routine to Write No data found
*----------------------------------------------------------------------*
FORM f_no_data_found .
*Display Nodata Found
  WRITE : text-002."'No Data Found For Fhe Selection'.
ENDFORM.                    " F_NO_DATA_FOUND


*&---------------------------------------------------------------------*
*&      Form  F_GET_PAYR_DATA_CID
*&---------------------------------------------------------------------*
*      Sub Routine to Get the PAYR Table data
*----------------------------------------------------------------------*
FORM f_get_payr_data_cid .
*Get the PAYR Table data
  SELECT  zbukr hbkid hktid rzawe chect lifnr vblnr gjahr pridt
          FROM payr INTO TABLE git_payr1 WHERE zbukr IN s_bukrs1
                                           AND lifnr IN s_lifnr1
                                           AND pridt IN s_date
                                           AND hbkid IN s_hbkid
                                           AND rzawe IN s_rzawe
                                           AND uzawe IN s_uzawe
                                           AND chect IN s_chect.
  SORT git_payr1 BY zbukr lifnr vblnr.

ENDFORM.                    " F_GET_PAYR_DATA_CID
*----------------------------------------------------------------------*
*&      Form  F_DISABLE_INPUT_FIELDS
*&---------------------------------------------------------------------*
*       Sub Routine to Disable the Selection Screen Inputs
*----------------------------------------------------------------------*

FORM f_disable_input_fields .
  CONSTANTS: lc_x   TYPE char1 VALUE 'X',
             lc_cls TYPE char3 VALUE 'CLS',
             lc_clr TYPE char3 VALUE 'CLR'.
  IF rb_1 = lc_x.
    LOOP AT SCREEN.
      IF screen-group1 = lc_cls.
        screen-input = 0. " Disable the input
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    REFRESH : s_bukrs1[],
              s_lifnr1[],
              s_date[].
  ELSEIF rb_2 = lc_x.
    LOOP AT SCREEN.
      IF screen-group1 = lc_clr.
        screen-input = 0. " Disable the input
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    REFRESH : s_lifnr[],
              s_bukrs[],
              s_kostl[],
              s_monat[],
              s_budat[],
              s_blart[],
              s_belnr[],
              s_xblnr[].

    CLEAR: p_gjahr.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = lc_cls.
        screen-input = 0. " Disable the input
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_DISABLE_INPUT_FIELDS
*&---------------------------------------------------------------------*
*&      Form  GET_BSIK_BSAK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bsik_bsak .
  CLEAR: gt_bsik.

  SELECT  *
*bukrs belnr gjahr buzei augbl zlsch uzawe lifnr hbkid hktid
           FROM  bsak INTO CORRESPONDING FIELDS OF  TABLE  gt_bsik
                                        FOR ALL ENTRIES IN git_bkpf
                                        WHERE bukrs = git_bkpf-bukrs
                                          AND belnr = git_bkpf-belnr
                                          AND gjahr = git_bkpf-gjahr
                                          AND augbl NE git_bkpf-belnr
                                          AND lifnr IN s_lifnr
                                          AND kostl IN s_kostl.

*                                            WHERE lifnr IN s_lifnr
*                                              AND kostl IN s_kostl
*                                          "
*                                              AND bukrs IN s_bukrs
*                                              AND belnr IN s_belnr
*                                              AND gjahr = p_gjahr
*                                              AND budat IN s_budat
*                                              AND monat IN s_monat
*                                              AND blart IN s_blart
*                                              AND xblnr IN s_xblnr
*                                              AND augbl NE git_bkpf-belnr.

  SELECT  *
*    bukrs belnr gjahr buzei augbl zlsch uzawe lifnr hbkid hktid
          FROM  bsik APPENDING TABLE gt_bsik
                                        FOR ALL ENTRIES IN git_bkpf
                                        WHERE bukrs = git_bkpf-bukrs
                                          AND belnr = git_bkpf-belnr
                                          AND gjahr = git_bkpf-gjahr
                                          AND augbl NE git_bkpf-belnr
                                          AND lifnr IN s_lifnr
                                          AND kostl IN s_kostl.
*                                          WHERE lifnr IN s_lifnr
*                                               AND kostl IN s_kostl
*             "
*                                               AND bukrs IN s_bukrs
*                                               AND belnr IN s_belnr
*                                               AND gjahr = p_gjahr
*                                               AND budat IN s_budat
*                                               AND monat IN s_monat
*                                               AND blart IN s_blart
*                                               AND xblnr IN s_xblnr.


ENDFORM.                    " GET_BSIK_BSAK
*&---------------------------------------------------------------------*
*&      Form  GET_BKPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_bkpf .

  SELECT  bukrs belnr gjahr blart xblnr awkey FROM bkpf
                                    INTO TABLE git_bkpf
                                    Where bukrs IN s_bukrs
                                      AND belnr IN s_belnr
                                      AND gjahr = p_gjahr
                                      AND budat IN s_budat
                                      AND monat IN s_monat
                                      AND blart IN s_blart
                                      AND xblnr IN s_xblnr.
*                             FOR ALL ENTRIES IN gt_bsik
*                             WHERE bukrs = gt_bsik-bukrs
*                               AND belnr = gt_bsik-belnr
*                               AND gjahr = gt_bsik-gjahr
*                               AND blart = gt_bsik-blart.
ENDFORM.                    " GET_BKPF
*&---------------------------------------------------------------------*
*&      Form  GET_PAYR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_payr .
  DATA: ls_payr LIKE LINE OF git_payr,
        ls_bsik LIKE LINE OF gt_bsik.

  CLEAR: git_payr.

  LOOP AT gt_bsik INTO ls_bsik WHERE
                                augbl IS NOT INITIAL.
    SELECT  zbukr hbkid hktid rzawe chect vblnr gjahr pridt
            FROM payr APPENDING CORRESPONDING FIELDS OF TABLE git_payr
            WHERE zbukr = ls_bsik-bukrs
              AND vblnr = ls_bsik-augbl.
  ENDLOOP.

  SORT git_payr BY zbukr hbkid hktid rzawe vblnr.

ENDFORM.                    " GET_PAYR
*&---------------------------------------------------------------------*
*&      Form  GET_BSIK_BSAK_CID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_bsik_bsak_cid .

  SELECT *
* bukrs belnr gjahr buzei augbl zlsch uzawe lifnr hbkid hktid
          FROM  bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsik
          FOR ALL ENTRIES IN git_payr1
         WHERE bukrs = git_payr1-zbukr
           AND augbl = git_payr1-vblnr
           AND gjahr = git_payr1-gjahr
           AND belnr NE git_payr1-vblnr
           AND lifnr = git_payr1-lifnr.

  SELECT  *
*bukrs belnr gjahr buzei augbl zlsch uzawe lifnr hbkid hktid
          FROM  bsik APPENDING TABLE gt_bsik FOR ALL ENTRIES IN git_payr1
          WHERE bukrs = git_payr1-zbukr
            AND augbl = git_payr1-vblnr
            AND gjahr = git_payr1-gjahr
            AND belnr NE git_payr1-vblnr
            AND lifnr = git_payr1-lifnr.

ENDFORM.                    " GET_BSIK_BSAK_CID
*&---------------------------------------------------------------------*
*&      Form  GET_BKPF_CID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_bkpf_cid .

  SELECT  bukrs belnr gjahr blart xblnr awkey FROM bkpf
          INTO TABLE git_bkpf FOR ALL ENTRIES IN gt_bsik
          WHERE bukrs = gt_bsik-bukrs
            AND belnr = gt_bsik-belnr
            AND gjahr = gt_bsik-gjahr.

  SORT git_bkpf BY bukrs belnr gjahr.
ENDFORM.                    " GET_BKPF_CID
*&---------------------------------------------------------------------*
*&      Form  FILL_FINAL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_final_table .

  DATA: lv_object TYPE thead-tdobject VALUE 'RBKP',
        lv_name TYPE thead-tdname,
        lt_tline TYPE TABLE OF tline,
        ls_tline LIKE LINE OF lt_tline,
        ls_rbkp TYPE rbkp,
        lv_belnr TYPE rbkp-belnr,
        lv_gjahr TYPE rbkp-gjahr,
        lv_tabix TYPE sy-tabix.

  SORT git_payr BY zbukr vblnr.
*Prepare Final Internal Table
  LOOP AT gt_bsik INTO gs_bsik.
    CLEAR: ls_rbkp,
           lv_belnr,
           lv_gjahr.
    gwa_final-bukrs = gs_bsik-bukrs.
    gwa_final-lifnr = gs_bsik-lifnr.
    gwa_final-belnr = gs_bsik-belnr.
    gwa_final-augbl = gs_bsik-augbl.
    gwa_final-zlsch = gs_bsik-zlsch.
    gwa_final-uzawe = gs_bsik-uzawe.
    gwa_final-xblnr = gs_bsik-xblnr.
    gwa_final-blart = gs_bsik-blart.
    if gs_bsik-sgtxt = 'Parked Document'.
       gwa_final-sgtxt = gs_bsik-sgtxt.
    else.
       gwa_final-sgtxt = 'Open / Cleared Doc'.
    endif.
    READ TABLE git_bkpf INTO gwa_bkpf WITH KEY bukrs   = gs_bsik-bukrs
                                               belnr   = gs_bsik-belnr
                                               gjahr   = gs_bsik-gjahr
                                               BINARY SEARCH.
    IF sy-subrc = 0.
*           gwa_final-xblnr = gwa_bkpf-xblnr.
*           gwa_final-blart = gwa_bkpf-blart.
      lv_belnr = gwa_bkpf-awkey(10).
      lv_gjahr = gwa_bkpf-awkey+10(4).
    ENDIF.
    IF lv_belnr IS NOT INITIAL.
      SELECT SINGLE * FROM rbkp INTO ls_rbkp WHERE belnr = lv_belnr
                                               AND gjahr = lv_gjahr.
    ENDIF.
    IF ls_rbkp-belnr IS NOT INITIAL.  "PO Base Invoice
      CONCATENATE lv_belnr lv_gjahr INTO lv_name RESPECTING BLANKS.
      lv_object = 'RBKP'.
    ELSE.  "Non-PO Base invoice
      CONCATENATE gs_bsik-bukrs gs_bsik-belnr gs_bsik-gjahr '001' INTO lv_name RESPECTING BLANKS.
      lv_object = 'DOC_ITEM'.
    ENDIF.
    CLEAR: gwa_final-tdline,
           lt_tline.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = '0001'
        language                = sy-langu
        name                    = lv_name
        object                  = lv_object
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
    ENDIF.
    IF lt_tline IS NOT INITIAL.
      READ TABLE lt_tline INTO ls_tline INDEX 1.
      gwa_final-tdline = ls_tline-tdline.
    ENDIF.
    CLEAR: gwa_lfa1,
           lv_tabix.
    READ TABLE git_lfa1 INTO gwa_lfa1 WITH KEY lifnr = gs_bsik-lifnr
                                               BINARY SEARCH.
    gwa_final-name1 = gwa_lfa1-name1.
    READ TABLE git_payr WITH KEY zbukr = gs_bsik-bukrs
                                 vblnr = gs_bsik-augbl TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND gwa_final TO git_final.
      CLEAR gwa_final.
    ELSE.
      lv_tabix = sy-tabix.
      LOOP AT git_payr INTO gwa_payr FROM lv_tabix.
        IF gwa_payr-zbukr <> gs_bsik-bukrs OR
           gwa_payr-vblnr <> gs_bsik-augbl.
          EXIT.
        ENDIF.
        gwa_final-chect = gwa_payr-chect.
        gwa_final-pridt = gwa_payr-pridt.
        gwa_final-hbkid = gwa_payr-hbkid.
        gwa_final-hktid = gwa_payr-hktid.

        APPEND gwa_final TO git_final.
        CLEAR gwa_final-chect.
        CLEAR gwa_final-pridt.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT git_final BY bukrs lifnr blart belnr.
ENDFORM.                    " FILL_FINAL_TABLE
*&---------------------------------------------------------------------*
*&      Form  FILL_FINAL_TABLE_CID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM fill_final_table_cid .

  DATA: lv_object TYPE thead-tdobject VALUE 'RBKP',
        lv_name TYPE thead-tdname,
        lt_tline TYPE TABLE OF tline,
        ls_tline LIKE LINE OF lt_tline,
        ls_rbkp TYPE rbkp,
        lv_belnr TYPE rbkp-belnr,
        lv_gjahr TYPE rbkp-gjahr,
        lv_tabix TYPE sy-tabix.

  SORT git_payr1 BY zbukr lifnr vblnr.

*Prepare Final Internal Table
  LOOP AT gt_bsik INTO gs_bsik.
    CLEAR gwa_final-tdline.
    gwa_final-bukrs = gs_bsik-bukrs.
    gwa_final-lifnr = gs_bsik-lifnr.
    gwa_final-belnr = gs_bsik-belnr.
    gwa_final-augbl = gs_bsik-augbl.
    gwa_final-zlsch = gs_bsik-zlsch.
    gwa_final-uzawe = gs_bsik-uzawe.
    gwa_final-xblnr = gs_bsik-xblnr.
    gwa_final-blart = gs_bsik-blart.
    if gs_bsik-sgtxt = 'Parked Document'.
       gwa_final-sgtxt = gs_bsik-sgtxt.
    else.
       gwa_final-sgtxt = 'Open / Cleared Doc'.
    endif.
    READ TABLE git_bkpf INTO gwa_bkpf WITH KEY bukrs   = gs_bsik-bukrs
                                               belnr   = gs_bsik-belnr
                                               gjahr   = gs_bsik-gjahr
                                               BINARY SEARCH.
    IF sy-subrc = 0.
      lv_belnr = gwa_bkpf-awkey(10).
      lv_gjahr = gwa_bkpf-awkey+10(4).
    ENDIF.
    IF lv_belnr IS NOT INITIAL.
      SELECT SINGLE * FROM rbkp INTO ls_rbkp WHERE belnr = lv_belnr
                                               AND gjahr = lv_gjahr.
    ENDIF.
    IF ls_rbkp-belnr IS NOT INITIAL.  "PO Base Invoice
      CONCATENATE lv_belnr lv_gjahr INTO lv_name RESPECTING BLANKS.
      lv_object = 'RBKP'.
    ELSE.  "Non-PO Base invoice
      CONCATENATE gs_bsik-bukrs gs_bsik-belnr gs_bsik-gjahr '001' INTO lv_name RESPECTING BLANKS.
      lv_object = 'DOC_ITEM'.
    ENDIF.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = '0001'
        language                = sy-langu
        name                    = lv_name
        object                  = lv_object
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
    ENDIF.
    IF lt_tline IS NOT INITIAL.
      READ TABLE lt_tline INTO ls_tline INDEX 1.
      gwa_final-tdline = ls_tline-tdline.
    ENDIF.
    CLEAR: gwa_lfa1,
           lv_tabix.
    READ TABLE git_lfa1 INTO gwa_lfa1 WITH KEY lifnr = gs_bsik-lifnr
                                                   BINARY SEARCH.
    gwa_final-name1 = gwa_lfa1-name1.
    READ TABLE git_payr1 WITH KEY zbukr = gs_bsik-bukrs
                                  lifnr = gs_bsik-lifnr
                                  vblnr = gs_bsik-augbl TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND gwa_final TO git_final.
      CLEAR gwa_final.
    ELSE.
      lv_tabix = sy-tabix.
      LOOP AT git_payr1 INTO gwa_payr1 FROM lv_tabix.
        IF gwa_payr1-zbukr <> gs_bsik-bukrs OR
           gwa_payr1-lifnr <> gs_bsik-lifnr OR
           gwa_payr1-vblnr <> gs_bsik-augbl.
          EXIT.
        ENDIF.
        gwa_final-chect = gwa_payr1-chect.
        gwa_final-pridt = gwa_payr1-pridt.
        gwa_final-hbkid = gwa_payr1-hbkid.
        gwa_final-hktid = gwa_payr1-hktid.

        APPEND gwa_final TO git_final.
        CLEAR: gwa_final-chect,
               gwa_final-pridt.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  SORT git_final BY bukrs lifnr blart belnr.
ENDFORM.                    " FILL_FINAL_TABLE_CID
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       Progress indicator
*----------------------------------------------------------------------*
FORM progress_indicator  USING   p1
                                 p2.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p1
      text       = p2.
ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  SEL_CRITERIA1
*&---------------------------------------------------------------------*
*       Selection Criteria 1
*----------------------------------------------------------------------*
FORM sel_criteria1 .

  PERFORM progress_indicator USING 0
          'Extracting Vendor Open / Clear Items'.
  PERFORM get_bkpf.
  check git_bkpf[] is not initial.
  PERFORM get_bsik_bsak.
  PERFORM progress_indicator USING 25
          'Extracting Vendor Parked Document'.
  PERFORM get_parked_document.

  CHECK gt_bsik[] IS NOT INITIAL.
*  PERFORM get_bkpf.
  PERFORM progress_indicator USING 35
          'Extracting Vendor Information'.
  PERFORM get_lfa1_data.
  PERFORM progress_indicator USING 50
          'Extracting Payment Medium Information'.
  PERFORM get_payr.

  PERFORM progress_indicator USING 80
          'Preparing data for output'.
  PERFORM fill_final_table.
ENDFORM.                    " SEL_CRITERIA1
*&---------------------------------------------------------------------*
*&      Form  SEL_CRITERIA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_criteria2 .

  PERFORM progress_indicator USING 0
          'Extracting Payment Medium Information'.
  PERFORM f_get_payr_data_cid.

  CHECK git_payr1[] IS NOT INITIAL.

  PERFORM progress_indicator USING 30
          'Extracting Vendor Open / Clear Items'.
  PERFORM get_bsik_bsak_cid.

  CHECK gt_bsik[] IS NOT INITIAL.

  PERFORM get_bkpf_cid.
  PERFORM progress_indicator USING 50
          'Extracting Vendor Information'.
  PERFORM get_lfa1_data.
  PERFORM progress_indicator USING 80
          'Preparing data for output'.
  PERFORM fill_final_table_cid.
ENDFORM.                    " SEL_CRITERIA2
*&---------------------------------------------------------------------*
*&      Form  GET_LFA1_DATA
*&---------------------------------------------------------------------*
*       Vendor Information
*----------------------------------------------------------------------*
FORM get_lfa1_data .


  SELECT lifnr name1 FROM lfa1 INTO TABLE git_lfa1
                     FOR ALL ENTRIES IN gt_bsik
                     WHERE lifnr = gt_bsik-lifnr.
  SORT git_lfa1 BY lifnr.

ENDFORM.                    " GET_LFA1_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PARKED_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_parked_document .
  TYPES: BEGIN OF ty_pdoc,
          bukrs TYPE vbkpf-bukrs,
          lifnr TYPE vbsegk-lifnr,
          belnr TYPE vbkpf-belnr,
          blart TYPE vbkpf-blart,
          gjahr TYPE vbkpf-gjahr,
          zlsch TYPE vbsegk-zlsch,
          uzawe TYPE vbsegk-uzawe,
          xblnr TYPE vbkpf-xblnr,
          awkey TYPE vbkpf-awkey,
          awtyp TYPE vbkpf-awtyp,
         END OF ty_pdoc.
  DATA: lt_pdoc TYPE TABLE OF ty_pdoc,
        ls_pdoc LIKE LINE OF lt_pdoc.

  DATA:  fl_belnr TYPE vbkpf-belnr,
         fl_gjahr TYPE vbkpf-gjahr,
         fl_ebeln TYPE rseg-ebeln.
  data: lt_vbkpf type table of FVBKPF,
        ls_vbkpf type FVBKPF,
        lt_vbsec type table of FVBSEC,
        ls_vbsec type FVBSEC,
        lt_vbseg type table of FVBSEG,
        ls_vbseg type FVBSEG,
        lt_vbset type table of FVBSET,
        ls_vbset type FVBSET.



SELECT a~bukrs b~lifnr a~belnr a~blart a~gjahr b~zlsch b~uzawe a~xblnr a~awkey a~awtyp
          INTO CORRESPONDING FIELDS OF TABLE lt_pdoc FROM vbkpf AS a
               INNER JOIN vbsegk AS b ON a~belnr = b~belnr
                                     AND a~gjahr = b~gjahr
                                   WHERE b~lifnr IN s_lifnr
*                                   AND kostl IN s_kostl
                                     AND a~bukrs IN s_bukrs
                                     AND a~belnr IN s_belnr
                                     AND a~gjahr = p_gjahr
                                     AND a~budat IN s_budat
                                     AND a~monat IN s_monat
                                     AND a~blart IN s_blart.
  LOOP AT lt_pdoc INTO ls_pdoc.
      clear: lt_vbkpf,
             lt_vbsec,
             lt_vbseg,
             lt_vbset,
             gs_bsik.
      CALL FUNCTION 'PRELIMINARY_POSTING_DOC_READ'
        EXPORTING
          belnr                         = ls_pdoc-belnr
          bukrs                         = ls_pdoc-bukrs
          gjahr                         = ls_pdoc-gjahr
        tables
          t_vbkpf                       = lt_vbkpf
          t_vbsec                       = lt_vbsec
          t_vbseg                       = lt_vbseg
          t_vbset                       = lt_vbset
*         T_VACSPLT                     =
*         T_VSPLTWT                     =
       EXCEPTIONS
         DOCUMENT_LINE_NOT_FOUND       = 1
         DOCUMENT_NOT_FOUND            = 2
         INPUT_INCOMPLETE              = 3
         OTHERS                        = 4 .
      IF sy-subrc <> 0.

      ENDIF.
      loop at lt_vbseg into ls_vbseg where lifnr = ls_pdoc-lifnr.
              check ls_vbseg-kostl is not initial.
              gs_bsik-kostl = ls_vbseg-kostl.
      endloop.
*    break sahmad.
    fl_belnr = ls_pdoc-awkey(10).
    fl_gjahr = ls_pdoc-awkey+10(4).

*    select single USNAM into FL_USNAM from rbkp
*        where belnr = fl_belnr
*          and gjahr = fl_gjahr.
    CLEAR: fl_ebeln.
    SELECT SINGLE ebeln INTO fl_ebeln FROM rseg WHERE belnr = fl_belnr
                                                  AND gjahr = fl_gjahr
                                                  AND ebeln IN s_xblnr.
    gs_bsik-bukrs = ls_pdoc-bukrs.
    gs_bsik-lifnr = ls_pdoc-lifnr.
    gs_bsik-belnr = ls_pdoc-belnr.
*    gs_bsik-augbl = ls_pdoc-augbl.
    gs_bsik-zlsch = ls_pdoc-zlsch.
    gs_bsik-uzawe = ls_pdoc-uzawe.
    gs_bsik-xblnr = fl_ebeln.
    gs_bsik-blart = ls_pdoc-blart.
    gs_bsik-gjahr = ls_pdoc-gjahr.
    gs_bsik-SGTXT = 'Parked Document'.
    APPEND gs_bsik TO gt_bsik.
  ENDLOOP.
  IF s_xblnr IS NOT INITIAL.
    DELETE gt_bsik WHERE xblnr NOT IN s_xblnr.
  ENDIF.
  if s_kostl is not initial.
     delete gt_bsik where kostl not in s_kostl.
  endif.
ENDFORM.                    " GET_PARKED_DOCUMENT
