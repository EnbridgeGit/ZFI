*&---------------------------------------------------------------------*
*& Report  ZFAPR005_VEN_ACTIVITY
*&
*&---------------------------------------------------------------------*
************************************************************************
*  Author:      Sajjad Ahmad                                           *
*  Date:        Oct 13, 2011.                                          *
*  Issue Log:                                                          *
*  Description:                                                        *
*     - Vendor without Activity
************************************************************************
*CHANGES:                                                              *
************************************************************************

REPORT  zfapr005_ven_activity.



TABLES: lfa1,               "Vendor Master (General Section)
        lfb1,               "Vendor Master (Company Code)
        bkpf,               "JUST FOR REFERENCE PURPOSE
        lfc1.
****************************************
TYPES: BEGIN OF ty_output,
          bukrs  TYPE bkpf-bukrs,
          lifnr  TYPE lfa1-lifnr,
          name1  TYPE lfa1-name1,
          adrs1(90),
          ktokk  TYPE lfa1-ktokk,
          adrnr  TYPE lfa1-adrnr,
          cpudt  TYPE bkpf-cpudt,
*additional fields if require for this report
*          blvdr  TYPE lfa1-sperr,
*          dfvdr  TYPE lfa1-loevm,
*          blcocd TYPE lfb1-sperr,
*          dfcocd TYPE lfb1-loevm,
*          dtotl  TYPE umxxs,
*          ctotl  TYPE umxxh,
      END OF ty_output.


DATA: gr_container TYPE REF TO cl_gui_custom_container,
      gr_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_layout    TYPE lvc_s_layo.
DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE ok_code,
      lt_output TYPE TABLE OF ty_output,
      wa_output TYPE ty_output.

**************SELECTION SCREEN************************************
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-003.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_bukrs  FOR  bkpf-bukrs OBLIGATORY,
                s_gjahr  FOR  bkpf-gjahr OBLIGATORY NO-EXTENSION,
                s_monat  FOR  bkpf-monat OBLIGATORY DEFAULT '01' TO 16 NO-EXTENSION,
                s_lifnr  FOR  lfa1-lifnr OBLIGATORY .       "DECK900950
SELECTION-SCREEN END OF BLOCK box.

**************END OF SELECTION SCREEN*******************************
INITIALIZATION.
  s_gjahr-low  = sy-datum+0(4) - 1.
  s_gjahr-high = sy-datum+0(4).
  APPEND s_gjahr.

START-OF-SELECTION.
*write: 'Under development'.
*Stop.

  break shamad.

  PERFORM get_data.
  PERFORM alv_display.

*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*      ALV Display
*----------------------------------------------------------------------*
FORM alv_display .
*if gr_container is initial.
  CREATE OBJECT gr_container
    EXPORTING
*     PARENT                      =
      container_name              = 'ALV_CONTAINER1'
*     STYLE                       =
*     LIFETIME                    = lifetime_default
*     REPID                       =
*     DYNNR                       =
*     NO_AUTODEF_PROGID_DYNNR     =
*     EXCEPTIONS
*     CNTL_ERROR                  = 1
*     CNTL_SYSTEM_ERROR           = 2
*     CREATE_ERROR                = 3
*     LIFETIME_ERROR              = 4
*     LIFETIME_DYNPRO_DYNPRO_LINK = 5
*     others                      = 6
      .
  IF sy-subrc <> 0.
*                                MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                                           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CREATE OBJECT gr_alvgrid
    EXPORTING
*    I_SHELLSTYLE      = 0
*    I_LIFETIME        =
      i_parent          = gr_container
*    I_APPL_EVENTS     = space
*    I_PARENTDBG       =
*    I_APPLOGPARENT    =
*    I_GRAPHICSPARENT  =
*    I_NAME            =
*    I_FCAT_COMPLETE   = SPACE
*  EXCEPTIONS
*    ERROR_CNTL_CREATE = 1
*    ERROR_CNTL_INIT   = 2
*    ERROR_CNTL_LINK   = 3
*    ERROR_DP_CREATE   = 4
*    others            = 5
      .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  PERFORM field_catalog.
  PERFORM prepare_layout.

*endif.
  CALL METHOD gr_alvgrid->set_table_for_first_display
    EXPORTING
*    I_BUFFER_ACTIVE               =
*    I_BYPASSING_BUFFER            =
*    I_CONSISTENCY_CHECK           =
*    I_STRUCTURE_NAME              =
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
      is_layout                     = gs_layout
*    IS_PRINT                      =
*    IT_SPECIAL_GROUPS             =
*    IT_TOOLBAR_EXCLUDING          =
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
*    IT_EXCEPT_QINFO               =
*    IR_SALV_ADAPTER               =
    CHANGING
      it_outtab                     = lt_output[]
      it_fieldcatalog               = gt_fieldcat
*    IT_SORT                       =
*    IT_FILTER                     =
*  EXCEPTIONS
*    INVALID_PARAMETER_COMBINATION = 1
*    PROGRAM_ERROR                 = 2
*    TOO_MANY_LINES                = 3
*    others                        = 4
          .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL SCREEN 100.

ENDFORM.                    " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
*       Build Field Catalog for ALV Display
*----------------------------------------------------------------------*
FORM field_catalog .

  DATA ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = 'BUKRS'.
  ls_fcat-ref_table = 'BKPF'.
  ls_fcat-ref_field = 'BUKRS'.
  ls_fcat-outputlen = '10'.
*  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = ''.
*  ls_fcat-seltext = ''.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'KTOKK'.
  ls_fcat-ref_table = 'LFA1'.
  ls_fcat-ref_field = 'KTOKK'.
  ls_fcat-outputlen = '5'.
*  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = ''.
*  ls_fcat-seltext = ''.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'LIFNR'.
  ls_fcat-ref_table = 'LFA1'.
  ls_fcat-ref_field = 'LIFNR'.
  ls_fcat-outputlen = '12'.
*  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = ''.
*  ls_fcat-seltext = ''.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'NAME1'.
  ls_fcat-ref_table = 'LFA1'.
  ls_fcat-ref_field = 'NAME1'.
  ls_fcat-outputlen = '35'.
*  ls_fcat-just      = 'X'.
  ls_fcat-coltext = 'Vendor Name'.
  ls_fcat-seltext = 'Vendor Name'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ADRS1'.
*  ls_fcat-ref_table = 'BKPF'.
*  ls_fcat-ref_field = 'KTOKK'.
  ls_fcat-outputlen = '100'.
*  ls_fcat-just      = 'X'.
  ls_fcat-coltext = 'Vendor Address'.
  ls_fcat-seltext = 'Vendor Address'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ADRNR'.
  ls_fcat-ref_table = 'LFA1'.
  ls_fcat-ref_field = 'ADRNR'.
  ls_fcat-outputlen = '10'.
*  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = ''.
*  ls_fcat-seltext = ''.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'CPUDT'.
  ls_fcat-ref_table = 'BKPF'.
  ls_fcat-ref_field = 'CPUDT'.
  ls_fcat-outputlen = '12'.
*  ls_fcat-just      = 'X'.
  ls_fcat-coltext = 'Date of last Activity'.
  ls_fcat-seltext = 'Date of last Activity'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

*  ls_fcat-fieldname = 'DTOTL'.
*  ls_fcat-ref_table = 'LFC1'.
*  ls_fcat-ref_field = 'UM01S'.
*  ls_fcat-outputlen = '15'.
**  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = 'Debit Total'.
*  ls_fcat-seltext = 'Debit Total'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.
*
*  ls_fcat-fieldname = 'CTOTL'.
*  ls_fcat-ref_table = 'LFC1'.
*  ls_fcat-ref_field = 'UM01H'.
*  ls_fcat-outputlen = '15'.
**  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = 'Credit Total'.
*  ls_fcat-seltext = 'Credit Total'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.
*
*  ls_fcat-fieldname = 'BLVDR'.
*  ls_fcat-ref_table = 'LFA1'.
*  ls_fcat-ref_field = 'SPERR'.
*  ls_fcat-outputlen = '2'.
**  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = 'BL-VDR'.
*  ls_fcat-seltext = 'BL_VDR'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.
*
*  ls_fcat-fieldname = 'BLCOCD'.
*  ls_fcat-ref_table = 'LFB1'.
*  ls_fcat-ref_field = 'SPERR'.
*  ls_fcat-outputlen = '2'.
**  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = 'BL-CoCd'.
*  ls_fcat-seltext = 'BL_CoCd'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.
*
*  ls_fcat-fieldname = 'DFVDR'.
*  ls_fcat-ref_table = 'LFA1'.
*  ls_fcat-ref_field = 'LOEVM'.
*  ls_fcat-outputlen = '2'.
**  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = 'DF-VDR'.
*  ls_fcat-seltext = 'DF-VDR'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.
*
*  ls_fcat-fieldname = 'DFCOCD'.
*  ls_fcat-ref_table = 'LFB1'.
*  ls_fcat-ref_field = 'LOEVM'.
*  ls_fcat-outputlen = '2'.
**  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = 'DF-CoCd'.
*  ls_fcat-seltext = 'DF-CoCd'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.

ENDFORM.                    " FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get Data
*----------------------------------------------------------------------*
FORM get_data .

  TYPES: BEGIN OF ty_final_data,
          bukrs TYPE bkpf-bukrs,
          lifnr TYPE bseg-lifnr,
          cpudt TYPE bkpf-cpudt,
         END OF ty_final_data.
  TYPES: BEGIN OF ty_acct_data,
          lifnr TYPE bseg-lifnr,
          cpudt TYPE bkpf-cpudt,
         END OF ty_acct_data.

  DATA: lt_bkpf TYPE TABLE OF bkpf,
        wa_bkpf TYPE bkpf,
        lt_bseg TYPE TABLE OF bseg,
        wa_bseg TYPE bseg,
        lt_lfa1 TYPE TABLE OF lfa1,
        wa_lfa1 TYPE lfa1,
        lt_lfb1 TYPE TABLE OF lfb1,
        wa_lfb1 TYPE lfb1,
        lt_old_bseg TYPE TABLE OF bseg.
  DATA: lt_gjahr TYPE RANGE OF bkpf-bukrs,
        wa_gjahr LIKE LINE OF lt_gjahr,
        lt_monat TYPE RANGE OF bkpf-monat,
        wa_monat LIKE LINE OF lt_monat.

  DATA: lt_old_bkpf TYPE TABLE OF bkpf,
        lt_acct_data TYPE TABLE OF ty_acct_data,
        wa_acct_data LIKE LINE OF lt_acct_data.

**************
  DATA: lt_bsik TYPE TABLE OF bsik,
        wa_bsik TYPE bsik,
        lt_old_bsik TYPE TABLE OF bsik,
        lt_bsak TYPE TABLE OF bsak,
        wa_bsak LIKE LINE OF lt_bsak.
*************
  CLEAR: lt_lfa1,
         lt_lfb1,
         lt_bkpf,
         lt_bseg,
         lt_old_bseg.

  SELECT * INTO TABLE lt_lfa1 FROM lfa1
    WHERE lifnr IN s_lifnr.
  IF lt_lfa1 IS INITIAL.
    WRITE: / 'No Vendor exist for the given selection criteria'.
    STOP.
  ENDIF.
  SELECT * INTO TABLE lt_lfb1 FROM lfb1
    WHERE lifnr IN s_lifnr AND
          bukrs IN s_bukrs.
*Vendor Open items and Cleared Items
  SELECT * INTO TABLE lt_bsik FROM bsik
                              WHERE bukrs IN s_bukrs
                                AND gjahr IN s_gjahr
                                AND monat IN s_monat
                                AND lifnr IN s_lifnr.
  SELECT * APPENDING TABLE lt_bsik FROM bsak
                              WHERE bukrs IN s_bukrs
                                AND gjahr IN s_gjahr
                                AND monat IN s_monat
                                AND lifnr IN s_lifnr.
***************************************

  LOOP AT lt_lfa1 INTO wa_lfa1.
    CLEAR: wa_bseg,
           wa_lfb1,
           wa_output.
*read table lt_bseg into wa_bseg with key lifnr = wa_lfa1-lifnr.
    READ TABLE lt_bsik WITH KEY lifnr = wa_lfa1-lifnr TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.
    READ TABLE lt_lfb1 INTO wa_lfb1 WITH KEY lifnr = wa_lfa1-lifnr.
*  check sy-subrc = 0.
    wa_output-bukrs = wa_lfb1-bukrs.
    wa_output-lifnr = wa_lfa1-lifnr.
    wa_output-ktokk = wa_lfa1-ktokk.
    wa_output-adrnr = wa_lfa1-adrnr.
    wa_output-name1 = wa_lfa1-name1.
    CONCATENATE wa_lfa1-stras wa_lfa1-ort01 wa_lfa1-pstlz wa_lfa1-regio
                               INTO wa_output-adrs1 SEPARATED BY space.
*Additional fields if require for ouput
*    MOVE:   wa_lfa1-sperr TO wa_output-blvdr,
*            wa_lfa1-loevm TO wa_output-dfvdr,
*            wa_lfb1-sperr TO wa_output-blcocd,
*            wa_lfb1-loevm TO wa_output-dfcocd.

*******************************Logic for additional fields if require
*    SELECT * FROM  lfc1
*    WHERE  lifnr = wa_lfa1-lifnr
*      AND  bukrs = wa_lfb1-bukrs
*      AND  gjahr IN s_gjahr.
*      IF sy-subrc = 0.
*        ADD lfc1-um01s THEN lfc1-um02s UNTIL lfc1-um16s GIVING
*            wa_output-dtotl ACCORDING TO s_monat.
*        ADD lfc1-um01h THEN lfc1-um02h UNTIL lfc1-um16h GIVING
*            wa_output-ctotl ACCORDING TO s_monat.
*      ENDIF.
*    ENDSELECT.
*************************************
    APPEND wa_output TO lt_output.
  ENDLOOP.
*************now select last transaction date of vendor
*Get Prior document data for vendor because it needs to pull cpudt
  IF lt_output IS NOT INITIAL.
    READ TABLE s_gjahr INTO wa_gjahr INDEX 1.
    READ TABLE s_monat INTO wa_monat INDEX 1.
    CLEAR: lt_old_bseg,
           lt_old_bkpf.
    CLEAR lt_acct_data.
    SELECT * INTO TABLE lt_old_bsik FROM bsik
                              WHERE bukrs IN s_bukrs
                                AND gjahr <= wa_gjahr-low
                                AND lifnr IN s_lifnr.
    SELECT * APPENDING TABLE lt_old_bsik FROM bsak
                              WHERE bukrs IN s_bukrs
                                AND gjahr <= wa_gjahr-low
                                AND lifnr IN s_lifnr.
    IF lt_old_bsik IS NOT INITIAL.
      LOOP AT lt_old_bsik INTO wa_bsik.
        IF wa_bsik-gjahr = wa_gjahr-low AND
            wa_bsik-gjahr <= wa_monat-low.
          CONTINUE.
        ENDIF.
        wa_acct_data-lifnr = wa_bsik-lifnr.
        wa_acct_data-cpudt = wa_bsik-cpudt.
        APPEND wa_acct_data TO lt_acct_data.
      ENDLOOP.
    ENDIF.
    IF lt_acct_data IS NOT INITIAL.
      SORT lt_acct_data BY lifnr cpudt DESCENDING.
      LOOP AT lt_output INTO wa_output.
        CLEAR: wa_acct_data.
        READ TABLE lt_acct_data INTO wa_acct_data
                                WITH KEY lifnr = wa_output-lifnr.
        CHECK sy-subrc = 0.
        wa_output-cpudt = wa_acct_data-cpudt.
        MODIFY lt_output FROM wa_output TRANSPORTING cpudt.
      ENDLOOP.
    ENDIF.
  ELSE.
    WRITE: / 'Selection Criteria does not have data for output'.
    STOP.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_layout .

  gs_layout-zebra  = 'X'.
  gs_layout-smalltitle = 'X'.
  gs_layout-sel_mode = 'A'.

ENDFORM.                    " PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STAT_01'.
  SET TITLEBAR 'T01'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
