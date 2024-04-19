class ZCL_IM_FI_AP_AC_DOCUMENT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_FI_AP_AC_DOCUMENT
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_AC_DOCUMENT .
*"* protected components of class ZCL_IM_FI_AP_AC_DOCUMENT
*"* do not include other source files here!!!
protected section.
private section.
*"* private components of class ZCL_IM_FI_AP_AC_DOCUMENT
*"* do not include other source files here!!!

  methods GENERATE_FATCA_POPUP
    importing
      !IM_DOCUMENT type ACC_DOCUMENT .
ENDCLASS.



CLASS ZCL_IM_FI_AP_AC_DOCUMENT IMPLEMENTATION.


METHOD generate_fatca_popup.

  DATA: lv_doc       TYPE belnr_d,
        lv_yr        TYPE gjahr,
        r_witht      TYPE RANGE OF lfbw-witht,
        r_witht_line LIKE LINE  OF r_witht,
        ltp_objtype  TYPE swo_objtyp,
        lta_worklist TYPE swrtwihdr,
        lwa_worklist TYPE swr_wihdr,
        lt_cont      TYPE TABLE OF swr_cont,
        lwa_cont     TYPE swr_cont,
        ltp_objkey   TYPE swotobjid-objkey,
        lv_qsshb     TYPE wt_bs1,
        lv_bukrs     TYPE bukrs,
        lv_fl_us     TYPE xflag,
        lv_fl_cdn    TYPE xflag,
        lv_ans       TYPE xfeld,
        lwa_item     TYPE accit.

  lv_doc  = im_document-header-awref.

  READ TABLE im_document-item INTO lwa_item WITH KEY koart = 'K'.
  IF im_document-header-awtyp = 'RMRP'.
    lv_yr    = im_document-header-aworg+0(4).
    lv_bukrs = lwa_item-bukrs.
  ELSE.
    lv_bukrs = im_document-header-aworg+0(4).
    lv_yr    = im_document-header-aworg+4(4).
  ENDIF.

  REFRESH r_witht.
  r_witht_line-sign   = 'I'.
  r_witht_line-option = 'EQ'.
  r_witht_line-low    = '03'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '42'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '04'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '43'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '05'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '44'.
  APPEND r_witht_line TO r_witht.
  "Don't trigger popup message when running in batch mode
  IF sy-binpt IS INITIAL.
    SELECT SINGLE wt_qsshb FROM with_item INTO lv_qsshb
                           WHERE bukrs = lv_bukrs
                           AND   belnr = lv_doc
                           AND   gjahr = lv_yr
                           AND   witht IN r_witht.

    IF sy-subrc = 0 AND lv_qsshb IS INITIAL .
      CALL FUNCTION 'Z_POPUP_WITH_2_OPTIONS'
        EXPORTING
          textline1    = 'Was all or part of the services on this'
          textline2    = 'invoice performed in the continental U.S.?'
          textline3    = '(Note: If AP has already processed this invoice'
          textline4    = 'for US Withholding Tax, answer No)'
          text_option1 = 'Yes'
          text_option2 = 'No'
          title        = 'FATCA Requirement'
        IMPORTING
          answer       = lv_ans.
    ENDIF.
  ENDIF.
* Set RCOFATCA container element
  IF im_document-header-awtyp NE 'RMRP'.
    ltp_objtype = 'FIPP'.
* Respecting blanks in statement "Concatenate" is needed because if company code
* is less than 4 characters in length then we need to build
* LTP_OBJKEY with the blank..e.g UGL 01002021092015
    CONCATENATE lv_bukrs lv_doc lv_yr INTO ltp_objkey RESPECTING BLANKS.
    CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
      EXPORTING
        objtype  = ltp_objtype
        objkey   = ltp_objkey
      TABLES
        worklist = lta_worklist.

    DELETE lta_worklist WHERE wi_rh_task NE 'WS02000002'.
    READ TABLE lta_worklist INTO lwa_worklist WITH KEY wi_type = 'F'.
    IF sy-subrc = 0.
      lwa_cont-element = 'RCOFATCA'.
      IF lv_ans = '1'.
        lwa_cont-value = 'X'.
      ELSE.
        lwa_cont-value = ''.
      ENDIF.

      APPEND lwa_cont TO lt_cont.

      CALL FUNCTION 'SAP_WAPI_WRITE_CONTAINER'
        EXPORTING
          workitem_id      = lwa_worklist-wi_id
        TABLES
          simple_container = lt_cont.
    ENDIF.
  ENDIF.  "'RMRP' endif
  IF lv_ans = '1'.
    MESSAGE e000(zfi_workflow) WITH 'Please Reject Invoice Back to AP for US WH Tax' DISPLAY LIKE 'I'.
  ENDIF.

ENDMETHOD.


METHOD if_ex_ac_document~change_after_check.
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_AP_AC_DOCUMENT(BAdi Implementations)      *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  12-Oct-2011                                   *
*& Object ID          :                                                *
*& Application Area   :  FI                                            *
*& Transport Request  :                                                *
*& Description        :  Display Error message if Route code is blank  *
*                        or invalid Route code.                        *
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *

*----------------------------------------------------------------------*
* Version No    : 03                                                   *
* Date          : June 19, 2015                                        *
* Modified By   : JRHARTUNG                                            *
* Correction No : D30K925946                                           *
* Description   : Include FATCA messages for withholding tax           *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : MZH01                                                *
* Date          : June 22, 2015                                        *
* Modified By   : Zakir Hossain (MZHOSSAIN)                            *
* Correction No : D30K925946                                           *
* Description   : Generate FATCA Pop-up when button "Save as Complete" *
*                 in transaction FV60. Also update workflow container  *
*                 with FATCA flag                                      *
*----------------------------------------------------------------------*
* Version No    : 04                                                   *
* Date          : April 08, 2019                                       *
* Modified By   : Ashok Kumar(MADASUAK)                                *
* Correction No : D30K929754                                           *
* Description   :Supress error message for RN Document type CHG0137057 *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : MZH01                                                  *
* Date          : Feb 14, 2023                                         *
* Modified By   : Mohammed Hossain(MZH01)                              *
* Correction No : D30K929754                                           *
* Description   :Remove RFC call for US system for DOA Migration project*
*----------------------------------------------------------------------*
  "Added to fix cars
  "Keep existing header text.
  ex_document-header-bktxt = im_document-header-bktxt.

  CONSTANTS:  gc_set_name(12)  TYPE c VALUE 'FIAPDOCTYPES',
                 gc_valsign(1)    TYPE c VALUE 'I',
                 gc_valoption(2)  TYPE c VALUE 'EQ',
                 gc_31(2)         TYPE c VALUE '31',
                 gc_21(2)         TYPE c VALUE '21',
                 gc_32(2)         TYPE c VALUE '32',
                 gc_39(2)         TYPE c VALUE '39',
                 gc_22(2)         TYPE c VALUE '22',
                 gc_29(2)         TYPE c VALUE '29'.
  DATA:   lwa_setleaf           TYPE setleaf,
          lit_item              TYPE TABLE OF accit.

  DATA: lwa_item              TYPE accit,
        ltp_rc_owner          TYPE wfsyst-initiator,"username,
        ltp_log_dest          TYPE tb_rfcdest,
        lwa_zfit_valid_blart  TYPE zfit_valid_blart,
        lv_username           TYPE xubname,
        lv_bname              TYPE bname.
**--START OF CHANGES BY AKMADASU FOR CHG0137057
  CONSTANTS: lc_miro  TYPE char4 VALUE 'MIRO',
             lc_x     TYPE char1 VALUE 'X',
             lc_rn    TYPE char2 VALUE 'RN',
             lc_re    TYPE char2 VALUE 'RE',
             lc_rf    TYPE char2 VALUE 'RF',
             lc_zf    TYPE char2 VALUE 'ZF'.
  DATA:      lv_flag  TYPE flag.
**--END OF CHANGES BY AKMADASU FOR CHG0137057
*Start of Ticket 22498 changes
  lit_item[]  = im_document-item[].

  "Get Vendor line item
  LOOP AT lit_item INTO lwa_item WHERE lifnr IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-tcode = 'FV60' OR sy-tcode = 'F-63' OR sy-tcode = 'FBV1' OR sy-tcode = 'FBV4'
     OR sy-tcode = 'MIRO' OR sy-tcode = 'MIR7' OR sy-tcode = 'MIR4'.  "MZH01
*    lit_item[]  = im_document-item[].     Ticket 22498
*
*    "Get Vendor line item
*    LOOP AT lit_item INTO lwa_item WHERE lifnr IS NOT INITIAL.
*      EXIT.
*    ENDLOOP.

    SELECT SINGLE * INTO lwa_zfit_valid_blart
       FROM zfit_valid_blart
       WHERE blart = lwa_item-blart.

    IF sy-subrc = 0.

      "Check for blank Route code
**--START OF CHANGES BY AKMADASU FOR CHG0137057
      CLEAR:lv_flag.
      IF sy-tcode = lc_miro AND ( lwa_zfit_valid_blart-blart = lc_rn OR
                                  lwa_zfit_valid_blart-blart = lc_re OR
                                  lwa_zfit_valid_blart-blart = lc_rf OR
                                  lwa_zfit_valid_blart-blart = lc_zf ).
        lv_flag = lc_x.
      ENDIF.
**--END OF CHANGES BY AKMADASU FOR CHG0137057
      IF lwa_item-xref3 IS INITIAL
**--START OF CHANGES BY AKMADASU FOR CHG0137057
        AND lv_flag = space.
**--END OF CHANGES BY AKMADASU FOR CHG0137057
        MESSAGE e030(zfi_workflow).
      ELSE.

        "Get Logical Destination

*        Begin of MZH01
*        CALL FUNCTION 'ZFI_GET_LOGICAL_DEST'
*          EXPORTING
*            IMP_PARAMTYPE = 'ECCUS'
*          IMPORTING
*            EXP_RFCDEST   = LTP_LOG_DEST.

        ltp_log_dest = 'NONE'.
*        end of MZH01

**--START OF CHANGES BY AKMADASU FOR CHG0137057
        IF lv_flag IS INITIAL.
**--END OF CHANGES BY AKMADASU FOR CHG0137057
          "Get route code from US Box
*          Begin of MZH01
*          CALL FUNCTION 'ZFI_GET_ROUTCODE' DESTINATION ltp_log_dest
*          End of MZH01
          CALL FUNCTION 'ZFI_GET_ROUTCODE'  "MZH01
            EXPORTING
              imp_route_code = lwa_item-xref3
            IMPORTING
              exp_rc_owner   = ltp_rc_owner
            EXCEPTIONS
              nobody_found.
          IF sy-subrc <> 0.
            "Check for Invalid Route code
            MESSAGE e029(zfi_workflow).
          ENDIF.

          "Check if user is valid in the system.
          lv_username = ltp_rc_owner+2(12).

          CALL FUNCTION 'ZBBC_CHECK_VALID_USER'
            EXPORTING
              imp_uname      = lv_username
            EXCEPTIONS
              user_not_valid = 1
              OTHERS         = 2.

          IF sy-subrc <> 0.
            "User does not exist.
            MESSAGE e046(zfi_workflow) WITH lv_username.
          ENDIF.
**--START OF CHANGES BY AKMADASU FOR CHG0137057
        ENDIF.
**--END OF CHANGES BY AKMADASU FOR CHG0137057
      ENDIF.
    ENDIF.
  ENDIF.
************PAYMENT METHOD REQUIRED CHECK, Ticket 22498
  CLEAR lwa_setleaf.
** Check document type present in setname
  SELECT SINGLE *
    INTO lwa_setleaf FROM setleaf
    WHERE setname   = gc_set_name  AND
          valsign   = gc_valsign   AND
          valoption = gc_valoption AND
          valfrom   = lwa_item-blart.
** Comment below IF to ADD Posting key 20 Credit memo
  IF sy-subrc = 0 AND ( lwa_item-bschl = gc_31 OR
                        lwa_item-bschl = gc_21 OR
                        lwa_item-bschl = gc_32 OR
                        lwa_item-bschl = gc_22 )." OR
*  Perform coding for Payment method field validation
    IF lwa_item-zlsch IS INITIAL.
      MESSAGE e059(zfi_workflow).
    ENDIF.
  ENDIF.

* Begin changes - replace code  JRHARTUNG 06/19/15 SDP57796 D30K925946 D30K926079

  CONSTANTS:
        gc_x                    TYPE xflag
                                VALUE 'X'.

  DATA: lv_lifnr                TYPE lifnr,
        lv_bukrs                TYPE bukrs,
        lv_fl_us                TYPE xflag,
        lv_fl_cdn               TYPE xflag,
        lv_ans                  TYPE xfeld,
        lv_date                 TYPE datum,
        lv_exdt                 TYPE wt_exdt.

  DATA: lwa_lfbw                TYPE lfbw,
        lit_lfbw                TYPE STANDARD TABLE OF lfbw.

*  FATCA IMPLEMENTATION
  IF   ( ( sy-ucomm EQ 'BP' )   OR   ( sy-ucomm EQ 'PARK' ) OR
         ( sy-ucomm EQ 'PB' )   OR   ( sy-ucomm EQ 'COMP' )    ).

    CLEAR    lit_item[].

    lit_item[] = im_document-item[].

*    READ THE VENDOR ITEM
    CLEAR                            lwa_item.
    READ     TABLE lit_item     INTO lwa_item
                            WITH KEY koart = 'K'.
    IF     ( sy-subrc NE 0 ).
      CLEAR        lwa_item.
    ENDIF.

    IF       ( lwa_item-lifnr     IS NOT INITIAL ).

*      SELECT THE WITHHOLDING TAX TYPES
      CLEAR                          lv_lifnr.
      MOVE     lwa_item-lifnr     TO lv_lifnr.
      CLEAR                          lv_bukrs.
      MOVE     lwa_item-bukrs     TO lv_bukrs.

      CLEAR    lit_lfbw[].
      SELECT   *
        INTO   TABLE lit_lfbw
        FROM   lfbw
       WHERE   lifnr = lv_lifnr
         AND   bukrs = lv_bukrs.
      IF     ( sy-subrc EQ 0 ).
        DELETE lit_lfbw WHERE wt_subjct IS INITIAL.
      ELSE.
        CLEAR  lit_lfbw[].
      ENDIF.

*        CHECK FOR US OR CANADIAN WITHHOLDING
      CLEAR    lv_fl_us.
      CLEAR    lv_fl_cdn.

      CLEAR                          lwa_lfbw.
      "Don't trigger popup message when running in batch mode
      IF sy-binpt IS INITIAL.
        LOOP AT  lit_lfbw         INTO lwa_lfbw.

          IF   ( ( lv_fl_us                 IS INITIAL ) ) AND
               ( ( lwa_lfbw-witht      BETWEEN '03' AND '05' ) OR
                 ( lwa_lfbw-witht      BETWEEN '42' AND '44' )    ).

            lv_fl_us = gc_x.

            CALL FUNCTION 'Z_POPUP_WITH_2_OPTIONS'
              EXPORTING
                textline1    = 'Vendor subject to US Withholding'
                textline2    = 'Tax'
                text_option1 = 'Ok'
                text_option2 = 'Cancel'
                title        = 'FATCA Requirement'
              IMPORTING
                answer       = lv_ans.

          ENDIF.

          IF   ( ( lv_fl_cdn                IS INITIAL ) ) AND
               ( ( lwa_lfbw-witht           EQ 'NC' ) OR
                 ( lwa_lfbw-witht           EQ 'NG' )    ).

            lv_fl_cdn = gc_x.

            CALL FUNCTION 'Z_POPUP_WITH_2_OPTIONS'
              EXPORTING
                textline1    = 'Vendor subject to CDN Withholding'
                textline2    = 'Tax'
                text_option1 = 'Ok'
                text_option2 = 'Cancel'
                title        = 'FATCA Requirement'
              IMPORTING
                answer       = lv_ans.

          ENDIF.

          CLEAR  lwa_lfbw.
        ENDLOOP.

*         CHECK FOR W8 REQUIREMENT
        CLEAR                          lwa_lfbw.
        LOOP AT  lit_lfbw         INTO lwa_lfbw.

          IF   ( ( lwa_lfbw-witht      BETWEEN '03' AND '05' ) OR
                 ( lwa_lfbw-witht      BETWEEN '42' AND '44' )    ).

            CLEAR    lv_date.
            lv_date = lwa_item-bldat + 60.

            CLEAR    lv_exdt.
            SELECT   SINGLE zindt
              INTO   lv_exdt
              FROM   lfb1
             WHERE   lifnr = lv_lifnr
               AND   bukrs = lv_bukrs.
            IF     ( sy-subrc NE 0 ).
              CLEAR  lv_exdt.
            ENDIF.

            IF   ( ( lv_exdt        IS NOT INITIAL ) AND
                   ( lv_exdt        LE lv_date     )     ).

              CALL FUNCTION 'Z_POPUP_WITH_2_OPTIONS'
                EXPORTING
                  textline1    = 'Please receive current W8 form for'
                  textline2    = 'vendor attached to this invoice'
                  text_option1 = 'Ok'
                  text_option2 = 'Cancel'
                  title        = 'FATCA Requirement'
                IMPORTING
                  answer       = lv_ans.

            ENDIF.

          ELSE.

            CLEAR    lwa_lfbw.

          ENDIF.

          IF     ( lwa_lfbw-lifnr   IS NOT INITIAL ).
            EXIT.
          ENDIF.

          CLEAR  lwa_lfbw.
        ENDLOOP.

      ENDIF. "Sy-binpt
    ENDIF.
  ENDIF.

* End changes   - replace code  JRHARTUNG 06/19/15 SDP57796 D30K925946 D30K926079


*  BEGIN OF CHANGES - MZH01
*Generate FATCA pop-up
  IF ( sy-ucomm = 'PBBP' OR sy-ucomm = 'BU' ).
    CALL METHOD generate_fatca_popup(
      EXPORTING
        im_document = im_document ).
  ENDIF.
*END OF CHANGES - MZH01

*start of SDP88562
  "---------------if WBS is set for allocation only, dont allow to post to FI
  "IM_DOCUMENT-ITEM-PS_PSP_PNR
  DATA: lv_posid TYPE prps-posid,
        lv_usr10 TYPE prps-usr10.
  IF sy-tcode <> 'CJ8G' AND
     sy-tcode <> 'CJ88' AND
     sy-tcode <> 'ASKB' AND
     sy-tcode <> 'ASKBN'  AND
     sy-cprog <> 'RKO7CJ8G' AND
     sy-cprog <> 'RAPERB2000' .
    lit_item[]  = im_document-item[].
    LOOP AT lit_item INTO lwa_item.
      IF lwa_item-ps_psp_pnr IS NOT INITIAL.
        CLEAR: lv_posid,
               lv_usr10.
        SELECT SINGLE usr10 INTO lv_usr10 FROM prps WHERE pspnr = lwa_item-ps_psp_pnr.
        IF lv_usr10 IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              input  = lwa_item-ps_psp_pnr
            IMPORTING
              output = lv_posid.
          MESSAGE e000(zfi_workflow) WITH 'WBS ' lv_posid 'used as an allocation element only, No FI posting allowed' ''.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
*end of SDP88562
ENDMETHOD.


METHOD if_ex_ac_document~change_initial.
  ex_document-header-bktxt = im_document-header-bktxt.
ENDMETHOD.


method IF_EX_AC_DOCUMENT~IS_ACCTIT_RELEVANT.
endmethod.


method IF_EX_AC_DOCUMENT~IS_COMPRESSION_REQUIRED.
endmethod.


method IF_EX_AC_DOCUMENT~IS_SUPPRESSED_ACCT.
endmethod.
ENDCLASS.
