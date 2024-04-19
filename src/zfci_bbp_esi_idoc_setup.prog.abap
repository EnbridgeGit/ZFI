*&---------------------------------------------------------------------*
*& Report  ZFCI_BBP_ESI_IDOC_SETUP                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*& This program creates IDOC segment E1BBPESIRFQH / E1BBPESIRFQI, and  *
*& IDOC type BBPESIRFQ. Before executing the program, please review    *
*& consulting note 1050852 for SRM e-Sourcing integration with ERP and:*
*&                                                                     *
*&1. Create development class / package BBP_ESI per instruction in   *
*&   the above note. The IDOC objects to be created will be organized  *
*&   under BBP_ESI.                                                  *
*&2. Have the corresponding transport request ready.                   *
*&3. Open the connection to SAP OSS system, as you may need to register*
*&   the IDOC objects and provide the access key from OSS system in    *
*&   order to continue.                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zfci_bbp_esi_idoc_setup                 .

TABLES: edisdef,
        sed4struc,
        edbas.

DATA:  g_header     LIKE edisegmhd,
       g_definition LIKE edisegmdef,
       gt_structure LIKE edisegstru OCCURS 5 WITH HEADER LINE.

CONSTANTS: c_seg_rfqh LIKE sed4struc-object VALUE 'E1BBPESIRFQH',
           c_seg_rfqi LIKE sed4struc-object VALUE 'E1BBPESIRFQI',
           c_typ_rfq  LIKE sed4struc-object VALUE 'BBPESIRFQ'.

* Selection screen
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_r1 RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_t1(40) DEFAULT 'Create IDOC Segment: E1BBPESIRFQH'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_r2 RADIOBUTTON GROUP g1.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_t2(40) DEFAULT 'Create IDOC Segment: E1BBPESIRFQI'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_r3 RADIOBUTTON GROUP g1.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_t3(40) DEFAULT 'Create IDOC Type:    BBPESIRFQ'.
SELECTION-SCREEN END OF LINE.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

START-OF-SELECTION.

  IF NOT p_r1 IS INITIAL.
    PERFORM idoc_segment_verification USING c_seg_rfqh.
    REFRESH: gt_structure.
    CLEAR: g_header, g_definition.

* copy segment definition info
    g_header-segtyp = c_seg_rfqh.
    g_header-presp = sy-uname.
    g_header-pwork = sy-uname.
    g_header-descrp = 'ESI RFQ Header'.

* Copy segment field info
    gt_structure-pos = '1'.
    gt_structure-fieldname = 'BELNR'.
    gt_structure-rollname = 'EDI_BELNR'.
    gt_structure-expleng = '35'.
    APPEND gt_structure.

    gt_structure-pos = '2'.
    gt_structure-fieldname = 'DOCDESC'.
    gt_structure-rollname = 'CHAR256'.
    gt_structure-expleng = '256'.
    APPEND gt_structure.

    gt_structure-pos = '3'.
    gt_structure-fieldname = 'RCOMP'.
    gt_structure-rollname = 'EDI_ORGID'.
    gt_structure-expleng = '35'.
    APPEND gt_structure.

    gt_structure-pos = '4'.
    gt_structure-fieldname = 'BUKRS'.
    gt_structure-rollname = 'EDI_ORGID'.
    gt_structure-expleng = '35'.
    APPEND gt_structure.

    gt_structure-pos = '5'.
    gt_structure-fieldname = 'EKORG'.
    gt_structure-rollname = 'EDI_ORGID'.
    gt_structure-expleng = '35'.
    APPEND gt_structure.

    gt_structure-pos = '6'.
    gt_structure-fieldname = 'BKGRP'.
    gt_structure-rollname = 'EDI_ORGID'.
    gt_structure-expleng = '35'.
    APPEND gt_structure.

    PERFORM idoc_segment_creation USING c_seg_rfqh.
  ELSEIF NOT p_r2 IS INITIAL.
    PERFORM idoc_segment_verification USING c_seg_rfqi.
    REFRESH: gt_structure.
    CLEAR: g_header, g_definition.

* copy segment definition info
    g_header-segtyp = c_seg_rfqi.
    g_header-presp = sy-uname.
    g_header-pwork = sy-uname.
    g_header-descrp = 'ESI RFQ Item'.

* Copy segment field info
    gt_structure-pos = '1'.
    gt_structure-fieldname = 'PO_ITEM'.
    gt_structure-rollname = 'EDI1082_A'.
    gt_structure-expleng = '6'.
    APPEND gt_structure.

    gt_structure-pos = '2'.
    gt_structure-fieldname = 'MATERIAL'.
    gt_structure-rollname = 'EDI_IDTNR'.
    gt_structure-expleng = '35'.
    APPEND gt_structure.

    gt_structure-pos = '3'.
    gt_structure-fieldname = 'SHORT_TEXT'.
    gt_structure-rollname = 'EDI_KTEXT'.
    gt_structure-expleng = '70'.
    APPEND gt_structure.

    gt_structure-pos = '4'.
    gt_structure-fieldname = 'MAT_GRP'.
    gt_structure-rollname = 'EDI_MATKL'.
    gt_structure-expleng = '9'.
    APPEND gt_structure.

    gt_structure-pos = '5'.
    gt_structure-fieldname = 'TARGET_QTY'.
    gt_structure-rollname = 'EDI_MENGE'.
    gt_structure-expleng = '15'.
    APPEND gt_structure.

    gt_structure-pos = '6'.
    gt_structure-fieldname = 'PO_UNIT_ISO'.
    gt_structure-rollname = 'EDI_MENEE'.
    gt_structure-expleng = '3'.
    APPEND gt_structure.

    gt_structure-pos = '7'.
    gt_structure-fieldname = 'PLANT'.
    gt_structure-rollname = 'WERKS_D'.
    gt_structure-expleng = '4'.
    APPEND gt_structure.

    gt_structure-pos = '8'.
    gt_structure-fieldname = 'ITEM_CAT'.
    gt_structure-rollname = 'PSTYP_EDI'.
    gt_structure-expleng = '1'.
    APPEND gt_structure.

    gt_structure-pos = '9'.
    gt_structure-fieldname = 'ITEM_CAT_TEXT'.
    gt_structure-rollname = 'PTEXT_D'.
    gt_structure-expleng = '20'.
    APPEND gt_structure.

    gt_structure-pos = '10'.
    gt_structure-fieldname = 'PREQ_NO'.
    gt_structure-rollname = 'EDI_BELNR'.
    gt_structure-expleng = '35'.
    APPEND gt_structure.

    gt_structure-pos = '11'.
    gt_structure-fieldname = 'PREQ_ITEM'.
    gt_structure-rollname = 'EDI1082_A'.
    gt_structure-expleng = '6'.
    APPEND gt_structure.

    gt_structure-pos = '12'.
    gt_structure-fieldname = 'DELIV_DATE'.
    gt_structure-rollname = 'EDIDAT8'.
    gt_structure-expleng = '8'.
    APPEND gt_structure.

    gt_structure-pos = '13'.
    gt_structure-fieldname = 'C_AMT_BAPI'.
    gt_structure-rollname = 'EDI5004_F'.
    gt_structure-expleng = '18'.
    APPEND gt_structure.

    gt_structure-pos = '14'.
    gt_structure-fieldname = 'PRICE_UNIT'.
    gt_structure-rollname = 'EDI_BMNG2'.
    gt_structure-expleng = '15'.
    APPEND gt_structure.

    gt_structure-pos = '15'.
    gt_structure-fieldname = 'CURRENCY_ISO'.
    gt_structure-rollname = 'EDI6345_A'.
    gt_structure-expleng = '3'.
    APPEND gt_structure.

    gt_structure-pos = '16'.
    gt_structure-fieldname = 'BASE_UOM_ISO'.
    gt_structure-rollname = 'EDI_MENEE'.
    gt_structure-expleng = '3'.
    APPEND gt_structure.

    gt_structure-pos = '17'.
    gt_structure-fieldname = 'SRV_ITEM'.
    gt_structure-rollname = 'EDI1082_A'.
    gt_structure-expleng = '6'.
    APPEND gt_structure.

    gt_structure-pos = '18'.
    gt_structure-fieldname = 'UM_CONV'.
    gt_structure-rollname = 'CHAR30'.
    gt_structure-expleng = '30'.
    APPEND gt_structure.

    PERFORM idoc_segment_creation USING c_seg_rfqi.
  ELSEIF NOT p_r3 IS INITIAL.
    PERFORM idoc_type_validation.

    PERFORM idoc_type_creation.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-name+0(1) = 'H'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  idoc_segment_verification
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_SEG_RFQH  text
*----------------------------------------------------------------------*
FORM idoc_segment_verification  USING    p_c_seg_rfqh.
* Verify if segments already exists
  SELECT SINGLE * FROM edisdef WHERE segtyp = p_c_seg_rfqh.
  IF sy-subrc = 0.
    WRITE:/
    'Specified IDOC segment already exists and can not be created.'.
    STOP.
  ENDIF.
ENDFORM.                    " idoc_segment_verification
*&---------------------------------------------------------------------*
*&      Form  idoc_segment_creation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM idoc_segment_creation USING p_seg TYPE sed4struc-object.
  DATA: tab1_e071k TYPE e071k OCCURS 0,
        tab2_e071k TYPE e071k OCCURS 0,
        tab1_ko200 TYPE ko200 OCCURS 0,
        tab2_ko200 TYPE ko200 OCCURS 0,
        l_result LIKE sy-subrc,
        devclass LIKE tadir-devclass,
        order LIKE e070-trkorr.

* set transport
  PERFORM cts_tables_create(sapledij) TABLES tab1_ko200
                                   tab1_e071k
                                   tab2_ko200
                                   tab2_e071k
                            USING p_seg
                                  devclass.
* check transport
  PERFORM cts_objects_check(sapledij) TABLES tab1_ko200
                                   tab1_e071k
                                   tab2_ko200
                                   tab2_e071k
                            USING  order.


* lock segemnt
  CALL FUNCTION 'ENQUEUE_EDISEGMENT'
       EXPORTING
            segtyp         = p_seg
       EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

  IF sy-subrc <> 0.
    WRITE:/ 'IDOC segment is already locked.'.
    STOP.
  ENDIF.

* create segment
  PERFORM create_segment(sapledij) TABLES   gt_structure
                                   CHANGING g_header
                                            g_definition
                                            l_result.
* clean up segment when in error
  IF l_result <> 0.
    CALL FUNCTION 'SEGMENT_CLEAN_UP'
         EXPORTING
              segmenttyp = p_seg.
    WRITE:/ 'ERROR: Specified IDOC segment is not created.'.
  ELSE.
* insert transport
    PERFORM cts_objects_insert(sapledij) TABLES tab1_ko200
                                                tab1_e071k
                                                tab2_ko200
                                                tab2_e071k
                                         USING  order
                                                p_seg.

    WRITE:/ 'Specified IDOC segment is successfully created.'.
  ENDIF.

* unlock segment
  CALL FUNCTION 'DEQUEUE_EDISEGMENT'
       EXPORTING
            segtyp = p_seg
       EXCEPTIONS
            OTHERS = 1.
ENDFORM.                    " ido_segment_creation
*&---------------------------------------------------------------------*
*&      Form  idoc_type_validation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM idoc_type_validation .
* Check existens of E1BBPESIRFQH
  SELECT SINGLE * FROM edisdef WHERE segtyp = c_seg_rfqh.
  IF sy-subrc <> 0.
    WRITE:/ 'IDOC segment E1BBPESIRFQH does not exist.'.
    STOP.
  ENDIF.

* Check existens of E1BBPESIRFQI
  SELECT SINGLE * FROM edisdef WHERE segtyp = c_seg_rfqi.
  IF sy-subrc <> 0.
    WRITE:/ 'IDOC segment E1BBPESIRFQI does not exist.'.
    STOP.
  ENDIF.

* Check existens of BBPESIRFQ
  SELECT SINGLE * FROM edbas WHERE idoctyp = c_typ_rfq.
  IF sy-subrc = 0.
    WRITE:/
    'Specified IDOC type already exists and can not be created.'.
    STOP.
  ENDIF.
ENDFORM.                    " idoc_type_validation
*&---------------------------------------------------------------------*
*&      Form  idoc_type_creation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM idoc_type_creation .
  DATA: devclass LIKE tadir-devclass,
        order LIKE e070-trkorr,
        l_rc LIKE sy-subrc,
        pi_attributes LIKE edi_iapi05,
        pi_idoctyp LIKE edi_iapi00-idoctyp,
        pt_syntax LIKE  edi_iapi02 OCCURS 0 WITH HEADER LINE,
        lt_idocsyn  LIKE idocsyn  OCCURS 1 WITH HEADER LINE.

* lock IDOC type
  pi_idoctyp = c_typ_rfq.
  CALL FUNCTION 'IDOCTYPE_LOCK'
       EXPORTING
            pi_idoctyp = pi_idoctyp
       EXCEPTIONS
            lock_error = 3
            OTHERS     = 99.

  IF sy-subrc <> 0.
    WRITE:/ 'IDOC type is already locked.'.
    STOP.
  ENDIF.

* pop-up transport window
  CALL FUNCTION 'IDOCTYPE_TRANSPORT'
       EXPORTING
            pi_idoctyp      = pi_idoctyp
            pi_devclass     = devclass
            pi_operation    = 'I'
       CHANGING
            pc_order        = order
       EXCEPTIONS
            transport_error = 4
            display_only    = 10
            OTHERS          = 99.

* create table entries
  pi_attributes-descrp = 'SRM E-Sourcing RFQ IDOC for RFP'.
  pi_attributes-presp = sy-uname.
  pi_attributes-pwork = sy-uname.

  pt_syntax-nr = '0001'.
  pt_syntax-segtyp	= c_seg_rfqh.
  pt_syntax-occmin	= '0000000001'.
  pt_syntax-occmax	= '0000000001'.
  pt_syntax-hlevel	= '01'.
  APPEND pt_syntax.

  pt_syntax-nr = '0002'.
  pt_syntax-segtyp	= c_seg_rfqi.
  pt_syntax-occmin	= '0000000001'.
  pt_syntax-occmax	= '0000999999'.
  pt_syntax-hlevel	= '02'.
  APPEND pt_syntax.

  PERFORM f14_create_edbas(sapledim) USING pi_idoctyp
                                           pi_attributes
                                           l_rc.
  IF l_rc EQ 0.
    PERFORM f14_create_edbast(sapledim) USING pi_idoctyp
                                              pi_attributes
                                              l_rc.
    IF l_rc EQ 0.
* convert PT_SYNTAX to LT_IDOCSYN (database structure)
      PERFORM fa1_adapt_idoctype_syntax(sapledim) TABLES pt_syntax
                                                         lt_idocsyn
                                                  USING  pi_idoctyp
                                                         l_rc.
      IF l_rc EQ 0.
        PERFORM f14_create_idocsyn(sapledim) TABLES lt_idocsyn[]
                                             USING  pi_idoctyp
                                                    l_rc.
      ENDIF.
    ENDIF.
  ENDIF.

  IF l_rc = 0.
    WRITE:/ 'Specified IDOC type is successfully created.'.
  ELSE.
    WRITE:/ 'ERROR: Specified IDOC type is not created.'.
  ENDIF.

* unlock IDOC type
  CALL FUNCTION 'IDOCTYPE_UNLOCK'
       EXPORTING
            pi_idoctyp = pi_idoctyp.
ENDFORM.                    " idoc_type_creation
