REPORT zfapi044 MESSAGE-ID zs.
*-----------------------------------------------------------------------
*  Programmer: M DeMeester
*
*  This abap will accept a generic excel spread and create a BDC session
*  using transaction MIRO to update the accounting for Framework PO
*  th additional fields.
*
*  Transaction - MIRO

*  Table RBCO contains the Account Assignment
*-----------------------------------------------------------------------
* CHANGE LOG
*-----------------------------------------------------------------------
* 2011/06/07 btboundy TR783 Complete Re-write for multiple docs/file
* 2011/06/07 btboundy TR783 Change WBS to 12 characters, and put in
*                     different field
* 2010/11/09 btboundy TR782 Increased line size
* 2010/10/21 btboundy TR782 Added -01 counter to Reference Value.
* 2008/02/08 mdemeet  TR483 New abap.
*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
"TYPE-POOLS: abap.

DATA: BEGIN OF bdcdata OCCURS 0.               "batch input data
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

TYPES: BEGIN OF ty_miro,
        bldat           LIKE rbkp-bldat,        "Invoice date
        bukrs           LIKE rbkp-bukrs,        "Company code
        waers           LIKE rbkp-waers,        "Currency
        xblnr           LIKE rbkp-xblnr,        "Reference
        sgtxt           LIKE invfo-sgtxt,       "Header
        blank1(1)       TYPE c,                 "Postkey - not used
        blank2(1)       TYPE c,                 "Vendor - not used
        blank3(1)       TYPE c,                 "Compcd - not used
        wrbtr(12)       TYPE c, "rbco-wrbtr,    "Invoice Amount
        blank4(1)       TYPE c,                 "Descrip - not used
        saknr           LIKE rbco-saknr,        "G/L Account
        kostl           LIKE rbco-kostl,        "Cost Centre
        aufnr           LIKE rbco-aufnr,        "I/O
        nplnr           LIKE rbco-nplnr,        "Network
        vornr           LIKE rbco-vornr,        "Activity
        ps_psp_pnr(15)  TYPE c, "ps_psp_pnr,    "WBS
        mwskz           LIKE rbco-mwskz,        "Tax code
        ebeln           LIKE ekko-ebeln,        "PO Number
        ebelp           LIKE ekpo-ebelp,        "Item number
        totamt(12)      TYPE c, "invfo-wrbtr,   "Total amount of Invoice
        dbcr(2)         TYPE c,                 "Debit/Credit
        blank5(1)       TYPE c,                 "Not used
      END OF ty_miro.

TYPES: BEGIN OF ty_miro_hed,
        bldat           LIKE rbkp-bldat,        "Invoice date
        bukrs           LIKE rbkp-bukrs,        "Company code
        waers           LIKE rbkp-waers,        "Currency
        xblnr           LIKE rbkp-xblnr,        "Reference
        sgtxt           LIKE invfo-sgtxt,       "Header
        ebeln           LIKE ekko-ebeln,        "PO Number
        ebelp           LIKE ekpo-ebelp,        "Item number
        totamt(12)      TYPE c, "invfo-wrbtr,   "Total amount of Invoice
        dbcr(2)         TYPE c,                 "Debit/Credit
      END OF ty_miro_hed.

TYPES: BEGIN OF ty_miro_line,
        wrbtr(12)       TYPE c, "rbco-wrbtr,    "Invoice Amount
        saknr           LIKE rbco-saknr,        "G/L Account
        kostl           LIKE rbco-kostl,        "Cost Centre
        aufnr           LIKE rbco-aufnr,        "I/O
        nplnr           LIKE rbco-nplnr,        "Network
        vornr           LIKE rbco-vornr,        "Activity
        ps_psp_pnr(15)  TYPE c, "ps_psp_pnr,    "WBS
        mwskz           LIKE rbco-mwskz,        "Tax code
      END OF ty_miro_line.

DATA: ls_miro       TYPE          ty_miro,
      lt_miro       LIKE TABLE OF ls_miro,

      ls_miro_bdc   TYPE          ty_miro,
      lt_miro_bdc   LIKE TABLE OF ls_miro,

      ls_miro_hed   TYPE          ty_miro_hed,
      ls_miro_line  TYPE          ty_miro_line,
      ls_miro_gst   TYPE          ty_miro_line,
      lt_miro_line  LIKE TABLE OF ls_miro_line.


DATA: lv_curline TYPE n.


***Reading Input File***
DATA: msg(80)       TYPE          c,
      lv_input(400) TYPE          c,
      ls_splits     TYPE          string,
      lt_splits     LIKE TABLE OF ls_splits.


CONSTANTS:      delimtr(1) TYPE c VALUE ','.
FIELD-SYMBOLS:  <curcol>   TYPE ANY.


DATA: lv_bukrs        LIKE rbkp-bukrs,
      lv_headererr    TYPE string,
      lv_totamt(12)   TYPE c,
      lv_linecount    TYPE i,
      lv_hedcount(2)  TYPE n.

*-----------------------------------------------------------------------
*   S E L E C T I O N - S C R E E N
*-----------------------------------------------------------------------
PARAMETER: p_infile LIKE filename-fileextern
                      DEFAULT '/usr/sap/interfaces/P01/FAPFOUP/eastfo.dat',
           p_tcode LIKE bbkpf-tcode DEFAULT 'MIRO'    OBLIGATORY MODIF ID tc VISIBLE LENGTH 4, "Transaction
           p_group LIKE bgr00-group DEFAULT 'ZAP_FO'  OBLIGATORY, "BDC sessionname
           p_saknr LIKE bseg-saknr  DEFAULT '256950'  OBLIGATORY. "G/L Tax Account


*-----------------------------------------------------------------------
*   I N I T I A L I Z A T I O N
*-----------------------------------------------------------------------
INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'TC'.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.





*-----------------------------------------------------------------------
*    S T A R T - O F - S E L E C T I O N
*-----------------------------------------------------------------------
START-OF-SELECTION.
  SHIFT p_saknr LEFT DELETING LEADING '0'.

  PERFORM read_input_file.
  PERFORM translate_input_file.
  PERFORM create_bdc.
  WRITE: /'Program completed successfully.'.




*----------------------------------------------------------------------*
*  This routine reads the delimited input file
*  and splits it into its various components.
*----------------------------------------------------------------------*
FORM  read_input_file.
  OPEN DATASET p_infile  FOR INPUT  IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_infile msg.
    STOP.
  ENDIF.


  CLEAR lt_miro.

  DO.
    READ DATASET p_infile INTO lv_input.
    IF sy-subrc <> 0.           "Exit when file is completely read in
      EXIT.
    ENDIF.

    IF sy-index = 1.
      CONTINUE.
    ENDIF.

    CLEAR lt_splits.
    SPLIT lv_input AT delimtr INTO TABLE lt_splits.

    CLEAR ls_miro.
    LOOP AT lt_splits INTO ls_splits.
      ASSIGN COMPONENT sy-tabix
             OF STRUCTURE ls_miro
             TO <curcol>.
      MOVE ls_splits TO <curcol>.
    ENDLOOP.

    APPEND ls_miro TO lt_miro.
  ENDDO.



  CLOSE DATASET p_infile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' p_infile msg.
    STOP.
  ENDIF.
ENDFORM.                    "read_input_file

*----------------------------------------------------------------------*
*  This will check for errors and prep the file for input into BDC
*----------------------------------------------------------------------*
FORM translate_input_file.
  DATA: lv_hnew     TYPE string,
        lv_hold     TYPE string.

  DATA: lv_ebelp        LIKE ekpo-ebelp.


  CLEAR: lv_hold, lv_hnew.

  LOOP AT lt_miro INTO ls_miro.
    CONCATENATE ls_miro-bldat ls_miro-bukrs ls_miro-waers
                ls_miro-xblnr ls_miro-sgtxt ls_miro-ebeln
      INTO lv_hnew.

    IF sy-tabix = 1.
      lv_bukrs = ls_miro-bukrs.
    ENDIF.

    IF lv_hnew <> '00000000'.
      """""Header processing""""""
      IF lv_hold IS NOT INITIAL.
        PERFORM insert_header.
      ENDIF.

      IF lv_totamt <> 0.
        WRITE:/'Total amount does not equal sum of line items! Header Ref:', lv_headererr.
      ENDIF.

      "Save the header.
      CLEAR ls_miro_hed.
      MOVE-CORRESPONDING ls_miro TO ls_miro_hed.

      lv_headererr = ls_miro_hed-xblnr.

      IF lv_bukrs <> ls_miro_hed-bukrs.
        WRITE:/'Different Company Code! Expected:', lv_bukrs, ' Received:', ls_miro_hed-bukrs, ' Header Ref:', lv_headererr.
      ENDIF.

      "Default blank DBCR to Debit
      IF ls_miro_hed-dbcr IS INITIAL.
        ls_miro_hed-dbcr = 'DB'.
      ELSEIF ls_miro_hed-dbcr = 'DR'.
        ls_miro_hed-dbcr = 'DB'.
      ELSEIF ls_miro_hed-dbcr = 'DB'.

      ELSEIF ls_miro_hed-dbcr = 'CR'.

      ELSE.
        WRITE:/'Unknown DB/CR indicator in the header! Header Ref:', lv_headererr.
      ENDIF.

      "Check for negative header amount
      IF ls_miro_hed-totamt < 0.
        WRITE:/'Header amount is negative! Header Ref:', lv_headererr.
      ENDIF.

      "Save the total amount
      lv_totamt = ls_miro_hed-totamt.
      CLEAR ls_miro_hed-totamt.

      lv_hedcount = 0.

      lv_hold = lv_hnew.
      lv_ebelp = ls_miro-ebelp.
    ENDIF.

    IF lv_linecount = 8.
      IF lv_hedcount = 0.
        lv_hedcount = 1.
      ENDIF.
      PERFORM insert_header.
    ENDIF.

    lv_linecount = lv_linecount + 1.

    """""Line processing""""""
    IF ls_miro-ebelp <> lv_ebelp AND ls_miro-saknr <> p_saknr.
      "Insert a new header for each line item change.
      PERFORM insert_header.
      lv_ebelp = ls_miro-ebelp.
    ENDIF.

    "Check the line for consistent DB/CR witht he current header.
    IF ls_miro-dbcr <> ''.
      "Blank entries are ok
      IF ls_miro-dbcr <> ls_miro_hed-dbcr.
        "If not blank, they better be the same
        WRITE:/'DB/CR Mismatch, header and items! Header Ref:', lv_headererr.
      ENDIF.
    ENDIF.

    "Check for negative dollar amount
    IF ls_miro-wrbtr < 0.
      WRITE:/'Item amount is negative! Header Ref:', lv_headererr.
    ENDIF.

    "Move the line to the line table.
    CLEAR ls_miro_line.
    MOVE-CORRESPONDING ls_miro TO ls_miro_line.
    APPEND ls_miro_line TO lt_miro_line.

    COMPUTE ls_miro_hed-totamt = ls_miro_hed-totamt + ls_miro_line-wrbtr.
  ENDLOOP.

  "Insert last set of information
  PERFORM insert_header.

ENDFORM.                    "translate_input_file

*----------------------------------------------------------------------*
*  This will create the bdc session
*----------------------------------------------------------------------*
FORM insert_header.
  "Add header to bdc table.
  CLEAR ls_miro_bdc.
  MOVE-CORRESPONDING ls_miro_hed TO ls_miro_bdc.

  IF lv_hedcount > 0.
    CONCATENATE ls_miro_hed-xblnr '-' lv_hedcount INTO ls_miro_bdc-xblnr.
    lv_hedcount = lv_hedcount + 1.
  ENDIF.

  APPEND ls_miro_bdc TO lt_miro_bdc.


  COMPUTE lv_totamt = lv_totamt - ls_miro_bdc-totamt.

  "Add items to bdc table.
  CLEAR ls_miro_gst.
  LOOP AT lt_miro_line INTO ls_miro_line.
    IF ls_miro_line-saknr = p_saknr.
      IF ls_miro_gst IS INITIAL.
        MOVE-CORRESPONDING ls_miro_line TO ls_miro_gst.
      ELSE.
        WRITE:/'Multiple tax codes entered for a single Entry! Header Ref:', lv_headererr.
      ENDIF.
    ELSE.
      CLEAR ls_miro_bdc.
      MOVE-CORRESPONDING ls_miro_line TO ls_miro_bdc.
      APPEND ls_miro_bdc TO lt_miro_bdc.
    ENDIF.
  ENDLOOP.
  IF ls_miro_gst IS NOT INITIAL.
    CLEAR ls_miro_bdc.
    MOVE-CORRESPONDING ls_miro_gst TO ls_miro_bdc.
    APPEND ls_miro_bdc TO lt_miro_bdc.
  ENDIF.

  CLEAR: lt_miro_line, ls_miro_hed-totamt.
  lv_linecount = 0.
ENDFORM.                    "insert_header

*----------------------------------------------------------------------*
*  This will create the bdc session
*----------------------------------------------------------------------*
FORM create_bdc.
  DATA: lv_saved(1) TYPE c.

  LOOP AT lt_miro_bdc INTO ls_miro_bdc.

    AT FIRST.
      SET PARAMETER ID 'BUK' FIELD ls_miro_bdc-bukrs.
      PERFORM open_bdc_session.
    ENDAT.

    IF ls_miro_bdc-bldat > 0.
      "Header
      IF sy-tabix > 1.
        "Submit previous headers.
        IF lv_saved = ''.
          PERFORM screen_6410_back.
          PERFORM bdc_screen USING 'SAPLMR1M'        '6000'.
          PERFORM bdc_field  USING 'BDC_OKCODE'     '=BU'.
        ENDIF.
        PERFORM insert_bdc_session.
      ENDIF.

      PERFORM header_screen_6000.

      PERFORM line_screen_6000.

      PERFORM bdc_screen  USING 'SAPLMR1M'    '6000'.
      PERFORM bdc_field  USING 'BDC_OKCODE'    '=KO001'.

      PERFORM bdc_screen USING 'SAPLMR1M'      '6410'.
      lv_curline = 0.
      lv_saved = ''.
    ELSE.
      "Line Item

      "Check for gst line.
      IF ls_miro_bdc-saknr = p_saknr.
        "Do GST portion on PO.
        PERFORM screen_6410_back.
        PERFORM screen_6000_tax.
        PERFORM bdc_screen USING 'SAPLMR1M'        '6000'.
        PERFORM bdc_field  USING 'BDC_OKCODE'     '=BU'.
        lv_saved = 'X'.
      ELSE.
        PERFORM line_screen_6410.
        "Add item to Current PO Line.
      ENDIF.
    ENDIF.


    AT LAST.
      "Submit this header and close the bdc.
      PERFORM insert_bdc_session.
      PERFORM close_bdc_session.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "create_bdc

*-----------------------  HEADER_SCREEN_6000 ---------------------------
*-----------------------------------------------------------------------
FORM header_screen_6000.
  DATA: lv_bktxt LIKE invfo-bktxt.

  PERFORM bdc_screen  USING 'SAPLMR1M'    '6000'.
*BASIC DATA
  CASE ls_miro_bdc-dbcr.
    WHEN 'DB'.
      PERFORM bdc_field  USING 'RM08M-VORGANG' '1'.
    WHEN 'CR'.
      PERFORM bdc_field  USING 'RM08M-VORGANG' '2'.
  ENDCASE.

  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLMR1M                                6005HEADER_AND_ITEMS'.
  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLFDCB                                0010HEADER_SCREEN'.
  PERFORM bdc_field  USING 'BDC_CURSOR'  'INVFO-XBLNR'.

  PERFORM bdc_field  USING 'INVFO-BLDAT' ls_miro_bdc-bldat.
  PERFORM bdc_field  USING 'INVFO-BUDAT' sy-datum.
  PERFORM bdc_field  USING 'INVFO-XBLNR' ls_miro_bdc-xblnr.
  PERFORM bdc_field  USING 'INVFO-WRBTR' ls_miro_bdc-totamt.
  PERFORM bdc_field  USING 'INVFO-XMWST' 'X'. "Calculate Tax
  PERFORM bdc_field  USING 'INVFO-SGTXT' ls_miro_bdc-sgtxt.
  PERFORM bdc_field  USING 'BDC_OKCODE'    '/00'.

*DETAILS TAB
  PERFORM bdc_screen USING 'SAPLMR1M'    '6000'.
  PERFORM bdc_field  USING 'BDC_OKCODE'    '=HEADER_FI'.

  PERFORM bdc_screen USING 'SAPLMR1M'    '6000'.
  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLMR1M                                6005HEADER_AND_ITEMS'.
  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLFDCB                                0150HEADER_SCREEN'.
  CONCATENATE 'PO' ls_miro_bdc-ebeln INTO lv_bktxt.
  PERFORM bdc_field  USING 'INVFO-BKTXT' lv_bktxt.
  PERFORM bdc_field  USING 'BDC_OKCODE'    '/00'.

  PERFORM bdc_screen USING 'SAPLMR1M'    '6000'.


*PO INFORMATION
  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLMR1M                                6020TABS'.
  PERFORM bdc_field  USING 'RM08M-REFERENZBELEGTYP' '1'.
  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLMR1M                                6211REFERENZBELEG'.
  PERFORM bdc_field  USING 'RM08M-EBELN'     ls_miro_bdc-ebeln.   "PO number on header
*  PERFORM bdc_field  USING 'RM08M-EBELP'     ls_miro_bdc-ebelp.   "Item number on PO
  PERFORM bdc_field  USING 'RM08M-XWARE_BNK' '1'.
  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLMR1M                                6310ITEM'.
  PERFORM bdc_field  USING 'RM08M-ITEM_LIST_VERSION' '7_6310'.
  PERFORM bdc_field  USING 'BDC_OKCODE'    '/00'.


ENDFORM.                    "header_screen_6000


*-----------------------  LINE_SCREEN_6000 ---------------------------
*-----------------------------------------------------------------------
FORM line_screen_6000.
  PERFORM bdc_screen  USING 'SAPLMR1M'    '6000'.
* For Account Assignment
  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLMR1M                                6020TABS'.
  PERFORM bdc_field  USING 'RM08M-REFERENZBELEGTYP' '1'.
  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLMR1M                                6211REFERENZBELEG'.
  PERFORM bdc_field  USING 'BDC_CURSOR'	'RM08M-EBELP'.
*  PERFORM bdc_field  USING 'RM08M-EBELN'     ls_miro_bdc-ebeln.   "PO number on header
  PERFORM bdc_field  USING 'RM08M-EBELP'     ls_miro_bdc-ebelp.   "Item number on PO
  PERFORM bdc_field  USING 'RM08M-XWARE_BNK' '1'.
  PERFORM bdc_field  USING 'BDC_SUBSCR'
    'SAPLMR1M                                6310ITEM'.
  PERFORM bdc_field  USING 'RM08M-ITEM_LIST_VERSION' '7_6310'.
  PERFORM bdc_field  USING 'BDC_OKCODE'    '/00'.

ENDFORM.                    "line_screen_6000


*----------------------------------------------------------------------*
FORM line_screen_6410.

  DATA:  fieldname LIKE bdcdata-fnam.




*  perform BDC_FIELD  USING 'BDC_CURSOR'    'DRSEG_CO-MWSKZ(08)'.
  lv_curline = lv_curline + 1.


  CONCATENATE 'DRSEG_CO-WRBTR(' lv_curline ')' INTO fieldname.
  PERFORM bdc_field  USING   fieldname    ls_miro_bdc-wrbtr.   "amount

  CONCATENATE 'DRSEG_CO-MWSKZ(' lv_curline ')' INTO fieldname.
  PERFORM bdc_field  USING   fieldname    ls_miro_bdc-mwskz.   "taxcode

  CONCATENATE 'DRSEG_CO-SAKNR(' lv_curline ')' INTO fieldname.
  PERFORM bdc_field  USING   fieldname    ls_miro_bdc-saknr.   "G/L

  CONCATENATE 'DRSEG_CO-SELKZ(' lv_curline ')' INTO fieldname.
  PERFORM bdc_field  USING   fieldname    'X'.      "Line selection

  DATA: lng TYPE i.
  lng = STRLEN( ls_miro_bdc-kostl ).
  IF lng > 0.
    CONCATENATE 'DRSEG_CO-KOSTL(' lv_curline ')' INTO fieldname.
    PERFORM bdc_field  USING   fieldname    ls_miro_bdc-kostl.  "Cost centre
  ENDIF.


  IF ls_miro_bdc-ps_psp_pnr > 0.
    CONCATENATE 'DRSEG_CO-PS_PSP_PNR(' lv_curline ')' INTO fieldname.
    PERFORM bdc_field  USING   fieldname    ls_miro_bdc-ps_psp_pnr.  "WBS
  ENDIF.

  lng = STRLEN( ls_miro_bdc-nplnr ).
  IF lng > 0.
    CONCATENATE 'DRSEG_CO-NPLNR(' lv_curline ')' INTO fieldname.
    PERFORM bdc_field  USING   fieldname    ls_miro_bdc-nplnr.  "Network
  ENDIF.

  lng = STRLEN( ls_miro_bdc-vornr ).
  IF lng > 0.
    CONCATENATE 'DRSEG_CO-VORNR(' lv_curline ')' INTO fieldname.
    PERFORM bdc_field  USING   fieldname    ls_miro_bdc-vornr.  "Activity
  ENDIF.

  lng = STRLEN( ls_miro_bdc-aufnr ).
  IF lng > 0.
    CONCATENATE 'DRSEG_CO-AUFNR(' lv_curline ')' INTO fieldname.
    PERFORM bdc_field  USING   fieldname    ls_miro_bdc-aufnr.  "Order
  ENDIF.

ENDFORM.                    "line_screen_6410



*-----------------------------------------------------------------------
* Tax entry
*-----------------------------------------------------------------------
FORM screen_6000_tax.
  PERFORM bdc_screen USING 'SAPLMR1M' '6000'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=ITEMS_G/L'.
  PERFORM bdc_screen USING 'SAPLMR1M' '6000'.
  PERFORM bdc_field  USING 'BDC_SUBSCR'
  'SAPLFSKB                                0100SACHKONTO'.
  PERFORM bdc_field  USING 'ACGL_ITEM-HKONT(01)' ls_miro_bdc-saknr.
  PERFORM bdc_field  USING 'ACGL_ITEM-WRBTR(01)' ls_miro_bdc-wrbtr.
ENDFORM.                    "screen_6000_tax

*-----------------------------------------------------------------------
* Exit from the item screen
*-----------------------------------------------------------------------
FORM screen_6410_back.

  PERFORM bdc_screen USING 'SAPLMR1M'      '6410'.
  PERFORM bdc_field  USING 'BDC_OKCODE'    'PREUFEN'.
  PERFORM bdc_field  USING 'BDC_CURSOR'    'YDRSEG-WRBTR'.
  PERFORM bdc_field  USING 'BDC_OKCODE'    '/EBACK'.

ENDFORM.                    "screen_6410_back

*---------------------------  BDC_SCREEN -------------------------------
*  - This routine adds an entry to the table BDCDATA with screen
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*-----------------------------------------------------------------------
FORM bdc_screen USING program dynpro.

  CLEAR bdcdata.
  bdcdata-program = program.
  bdcdata-dynpro = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.

ENDFORM.                    "BDC_SCREEN

*-----------------------------  BDC_FIELD-------------------------------
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*-----------------------------------------------------------------------
FORM bdc_field USING fnam fval.

  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD




*------------------------  OPEN_BDC_SESSION  -------------------------
* - This routine just simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM open_bdc_session.

  DATA: lv_holddate LIKE sy-datum.
  lv_holddate = sy-datum - 1.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = p_group
      holddate          = lv_holddate
      keep              = 'X'
      user              = sy-uname
    EXCEPTIONS
      group_invalid     = 1
      group_is_locked   = 2
      holddate_invalid  = 3
      internal_error    = 4
      queue_error       = 5
      running           = 6
      system_lock_error = 7
      user_invalid      = 8.

  IF sy-subrc <> 0.
    MESSAGE e004 WITH p_group sy-subrc.
  ENDIF.

ENDFORM.                    "open_bdc_session

*-----------------------  INSERT_BDC_SESSION  --------------------------
* Inserts the BDC data for one transaction into the batch input session.
*-----------------------------------------------------------------------
FORM insert_bdc_session.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = p_tcode
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.

  IF sy-subrc <> 0.
    MESSAGE e013 WITH sy-subrc.
  ENDIF.
  CLEAR bdcdata.
  REFRESH bdcdata.

ENDFORM.                    "insert_bdc_session

*-------------------------  CLOSE_BDC_SESSION  -------------------------
* - This routine simply closes the current batch input session.
*-----------------------------------------------------------------------
FORM close_bdc_session.

  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1
      queue_error = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
    MESSAGE e027 WITH sy-subrc.
  ENDIF.
ENDFORM.                    "close_bdc_session
