REPORT zffir002 MESSAGE-ID zs.
*-----------------------------------------------------------------------
*  This report copied from the EAST.  Please make any changes to both
*  the EAST and the WEST. The authorization check is only for the West.
*  When moving to the WEST uncomment TR569 lines!
*-----------------------------------------------------------------------
************************************************************************
* 2010/09/20 btboundy TR782 Add Document/Invoice Date and Clearing Date
* 2009/11/16 lritchie TR569 Use all 10 digits of cost center (WEST ONLY)
* 2009/11/16 lritchie TR342 Use all 10 digits of cost center
*                           Setup company code selection like West
* 2003/06/04 mdemeest 1019 Eliminate recurring entries template.
* 2003/04/30 mdemeest 1019 Creating excel spreadsheet.  Also adding
*                          Network selection.
* 2000/11/24 mdemeest #--- Added posting date as selection criteria
*                          at the request of Union Energy(Marie Erickson
* 2000/02/10 mdemeest #636 Fix vendor selection
* 2000/02/08 mdemeest #636 Download CC/IO/WBS to Excel
* 1999/11/03 mdemeest #636 Extract information from SAP into
*                          report for audits.  Also allow information
*                          to be downloaded to Excel by using pf8.
*
************************************************************************
TABLES:
  BKPF,         " Accounting Document Header
  BSEG,         " Accounting Document Segment
  LFA1,         " Vendor Master (General Section)
  PRPS,         " WBS (Work Breakdown Structure) Element Master Data
  MAKT,         " Material Descriptions
  PROJ,         " Project definition
  T024E,        " Purchasing Organizations
  SETHIER.      " Interface Structure: Set Hierarchy

*-------------------  SELECTION SCREEN  --------------------------------
SELECTION-SCREEN BEGIN OF BLOCK box7 WITH FRAME TITLE text-021.

SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-023.
* PARAMETER:    P_BUKRS LIKE BKPF-BUKRS OBLIGATORY MEMORY ID BUK,       "TR342
*               P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4) OBLIGATORY. "TR342
PARAMETER:      p_gjahr LIKE bkpf-gjahr DEFAULT sy-datum(4) OBLIGATORY. "TR342

SELECT-OPTIONS: s_monat FOR  bkpf-monat OBLIGATORY DEFAULT '01' TO '12',
                s_bukrs FOR  bkpf-bukrs NO INTERVALS,       "TR342
                s_bldat FOR bkpf-bldat,                     "TR782
                s_budat FOR  bkpf-budat NO INTERVALS,
                s_blart FOR  bkpf-blart,      " Document Type
                s_lifnr FOR bseg-lifnr.       " Account Number of Vendor or Creditor
*                s_augdt FOR bseg-augdt.                     "TR782
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-017.
SELECT-OPTIONS: s_hkont FOR  bseg-hkont NO INTERVALS,
                s_aufnr FOR  bseg-aufnr NO INTERVALS,
                s_kostl FOR  bseg-kostl NO INTERVALS,
                s_projk FOR  bseg-projk NO INTERVALS,
                s_nplnr FOR  bseg-nplnr NO INTERVALS
*                ,s_pspid for  proj-pspid no-display,         "tr569
*                s_pspnr for  prps-pspnr no-display,         "tr569
*                s_werks for  bseg-werks no-display,         "tr569
*                s_ekorg for  t024e-ekorg no-display        "tr569
                .
SELECTION-SCREEN END OF BLOCK box2.

select-options: s_ccgrp for sethier-shortname no intervals no-display.
                                                            "TR569

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-100.
PARAMETERS:     p_rprt  RADIOBUTTON  GROUP rbcr,       "Print Report
                p_excl  RADIOBUTTON  GROUP rbcr.       "Spreadsheet
SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN END OF BLOCK box7.

*-----------------------------------------------------------------------
DATA:  retcode          LIKE sy-subrc,        " Return Value of ABAP Statements
       w_option(11)     TYPE c  VALUE 'START_EXCEL',
       w_head01(120)    TYPE c,
       w_head02(120)    TYPE c,
       w_repttl         LIKE sy-title.        " Contents of Title Line

DATA: BEGIN OF prot_header OCCURS 1,
        spaltenname(20) TYPE c,
        ddic_table(5)   TYPE c,
        ddic_field(5)   TYPE c,
        key             TYPE c,
      END OF prot_header.

DATA  errortab          LIKE hrerror  OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF excltab  OCCURS 10000,           "Excel spreadsheet
      bukrs    LIKE bkpf-bukrs,               " Company Code
      hkont    LIKE bseg-hkont,               " General Ledger Account
      belnr    LIKE bkpf-belnr,               " Accounting Document Number
      blart    LIKE bkpf-blart,               " Document Type
      bldat    LIKE bkpf-bldat,               " Document Date in Document TR782
      budat    LIKE bkpf-budat,               " Posting Date in the Document
      bschl    LIKE bseg-bschl,               " Posting Key
      dmbtr(15)  TYPE c,                      "like bseg-dmbtr,
      pswsl    LIKE bseg-pswsl,               " Update Currency for General Ledger Transaction Figures
      lifnr    LIKE bseg-lifnr,               " Account Number of Vendor or Creditor
      name1    LIKE lfa1-name1,                             " Name 1
      augdt    LIKE bseg-augdt,               " Clearing Date TR782
      accting(24) TYPE c,

      END OF excltab.

DATA: BEGIN OF watab  OCCURS 10000,           "WA for Excel spreadsheet
      bukrs    LIKE bkpf-bukrs,               " Company Code
      belnr    LIKE bkpf-belnr,               " Accounting Document Number
      blart    LIKE bkpf-blart,               " Document Type
      bldat    LIKE bkpf-bldat,               " Document Date in Document TR782
      budat    LIKE bkpf-budat,               " Posting Date in the Document
      lifnr    LIKE bseg-lifnr,               " Account Number of Vendor or Creditor
      augdt    LIKE bseg-augdt,               " Clearing Date  TR872
      END OF watab.

DATA: wa-name  LIKE lfa1-name1.                             " Name 1
DATA: wa-zwels LIKE lfb1-zwels.               " List of the Payment Methods to be Considered
DATA: wa-maktx LIKE makt-maktx.               " Material Description (Short Text)
DATA: prv-lifnr LIKE bseg-lifnr.              " Account Number of Vendor or Creditor
DATA: tmp-lifnr LIKE bseg-lifnr.              " Account Number of Vendor or Creditor

DATA: objnr    LIKE prps-objnr.               " Object number


DATA:  len1      TYPE i,       "Selection field lengths
       len2      TYPE i,
       len3      TYPE i,
       len4      TYPE i.

*-----------------------------------------------------------------------
* Check that only one grouping is entered: G/L, CC, I/O or WBS and
* send back an appropriate message
*-----------------------------------------------------------------------
AT SELECTION-SCREEN.
  len1 = STRLEN( s_hkont ).
  len2 = STRLEN( s_aufnr ).
  len3 = STRLEN( s_kostl ).
  len4 = STRLEN( s_projk ).
  IF s_projk+3(8) = 0.
    len4 = 0.
  ENDIF.

  IF len4 <> 0.                    "Require transaction to proper format
    CONCATENATE 'PR' s_projk+3(8) INTO objnr.
    SELECT SINGLE * FROM prps
       WHERE objnr = objnr.
    IF sy-subrc <> 0.
      MESSAGE e100 WITH 'Invalid WBS element'.
    ENDIF.
  ENDIF.

* len4 = strlen( s_projk ).
  IF ( len1 =  0 AND len2 =  0 AND len3 =  0  AND len4 = 0 ).
    MESSAGE e100 WITH 'Must enter one selection'.
  ELSE.
    IF ( len1 <> 0 AND len2 =  0 AND len3 =  0  AND len4 = 0  ) OR
       ( len1 =  0 AND len2 <> 0 AND len3 =  0  AND len4 = 0  ) OR
       ( len1 =  0 AND len2 =  0 AND len3 <> 0  AND len4 = 0  ) OR
       ( len1 =  0 AND len2 =  0 AND len3 =  0  AND len4 <> 0 ) OR
       ( len1 <> 0 AND len2 <> 0 AND len3 =  0  AND len4 =  0 ) OR
       ( len1 <> 0 AND len2 =  0 AND len3 <> 0  AND len4 =  0 ) OR
       ( len1 <> 0 AND len2 =  0 AND len3 =  0  AND len4 <> 0 ).
    ELSE.
      MESSAGE e100 WITH text-017.  "Error message for too many entries
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------

*  S_PSPNR[] = S_PROJK[].                                    "TR569
*  INCLUDE ZFICO_AUTH_CHECK_INCLUDE.                         "TR569
*  INCLUDE ZNONFICO_AUTH_CHECK_INCLUDE.                      "TR569

START-OF-SELECTION.
*  PERFORM FICO_AUTHORIZATION_CHECK.                         "TR569
*  PERFORM NONFICO_AUTHORIZATION_CHECK.                      "TR569
  PERFORM select_bkpf_documents.
  PERFORM select_bseg_documents_vendor.
  PERFORM select_bseg_documents_clrdt.                      "TR872
  PERFORM select_bseg_documents.


*-----------------------------------------------------------------------
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Determine Vendor Numbers'
    EXCEPTIONS
      OTHERS = 1.
*-----------------------------------------------------------------------
* loops thru detail records and applies vendor #
*-----------------------------------------------------------------------
  LOOP AT excltab.
    SELECT SINGLE * FROM lfa1
       WHERE lifnr = excltab-lifnr.
    IF sy-subrc = 0.
      excltab-name1 = lfa1-name1.
    ENDIF.
    MODIFY excltab.
* ENDIF.

  ENDLOOP.     " LOOP AT EXCLTAB

  SORT excltab BY lifnr belnr.
  PERFORM create_excel_spreadsheet.

*&---------------------------------------------------------------------*
*&      Form  Create_excel_spreadsheet
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_EXCEL_SPREADSHEET.
  PERFORM prot_header.

*-------------------------- Report Title Line --------------------------
  MOVE sy-repid         TO w_repttl.
  MOVE '-'              TO w_repttl+8(1).
  MOVE text-ttl         TO w_repttl+10(30).

*----------------------------  W_HEAD01  -------------------------------
*----------------------------  W_HEAD02  -------------------------------

  IF p_rprt = 'X'.
    CLEAR w_option.
    IF sy-batch = 'X'.
      w_option = 'LINESSELMOD:1'.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
    EXPORTING
      basic_list_title    = w_repttl
      file_name           = sy-cprog
      head_line1          = w_head01
      head_line2          = w_head02
      additional_options  = w_option
    IMPORTING
      return_code         = retcode
    TABLES
      data_tab            = excltab
      fieldname_tab       = prot_header
      error_tab           = errortab
    EXCEPTIONS
      download_problem    = 1
      no_data_tab_entries = 2
      table_mismatch      = 3
      print_problems      = 4
      OTHERS              = 5.
  IF  sy-subrc = 2.
    WRITE: /1  'No data selected for download'.
  ELSEIF sy-subrc <> 0.
    WRITE: /1 'TABLE DOWNLOAD UNSUCCESSFUL - reason = ', sy-subrc.
  ENDIF.

ENDFORM.                    "Create_excel_spreadsheet

*&---------------------------------------------------------------------*
*&      Form  prot_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PROT_HEADER.
  MOVE text-002   TO prot_header-spaltenname.      "Company code
  APPEND prot_header.
  MOVE text-001   TO prot_header-spaltenname.      "G/L Account
  APPEND prot_header.
  MOVE text-005   TO prot_header-spaltenname.      "Document #
  APPEND prot_header.
  MOVE text-008   TO prot_header-spaltenname.      "Document type
  APPEND prot_header.
  MOVE text-030   TO prot_header-spaltenname.      "Inventory date TR782
  APPEND prot_header.
  MOVE text-006   TO prot_header-spaltenname.      "Posting Date
  APPEND prot_header.
  MOVE text-024   TO prot_header-spaltenname.      "Posting Key
  APPEND prot_header.
  MOVE text-010   TO prot_header-spaltenname.      "Amount
  APPEND prot_header.
  MOVE text-031   TO prot_header-spaltenname.      "Currency
  APPEND prot_header.
  MOVE text-004   TO prot_header-spaltenname.      "Vendor
  APPEND prot_header.
  MOVE text-032   TO prot_header-spaltenname.      "Vendor Name
  APPEND prot_header.
  MOVE text-033   TO prot_header-spaltenname.      "Clearing Date
  APPEND prot_header.
  MOVE text-020   TO prot_header-spaltenname.      "CC/IO/WBS/Network
  APPEND prot_header.

ENDFORM.                    "prot_header

*---------------------  SELECT_BKPF_DOCUMENTS  -------------------------
*  Select all documents that satisfy the criteria
*-----------------------------------------------------------------------
FORM SELECT_BKPF_DOCUMENTS.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Select all Document Headers that satisfy criteria'
    EXCEPTIONS
      OTHERS = 1.

  SELECT *  FROM BKPF
*    WHERE BUKRS = P_BURKS                                  "TR 342
     WHERE bukrs IN s_bukrs                                 "TR 342
       AND gjahr = p_gjahr
       AND monat IN s_monat
       AND blart IN s_blart
       AND bldat IN s_bldat                                 "TR782
       AND budat IN s_budat
       AND tcode <> 'FBD1'.       "Eliminates recurring entries template
    MOVE bkpf-bukrs     TO watab-bukrs.
    MOVE bkpf-belnr     TO watab-belnr.
    MOVE bkpf-bldat     TO watab-bldat.                     "TR782
    MOVE bkpf-budat     TO watab-budat.
    MOVE bkpf-blart     TO watab-blart.
    APPEND watab.
  ENDSELECT.                                "End of BKPF
ENDFORM.                    "SELECT_BKPF_DOCUMENTS

*------------------------  SELECT_BSEG_DOCUMENTS  ----------------------
* Select all document details that satisfy the criteria
*-----------------------------------------------------------------------
FORM SELECT_BSEG_DOCUMENTS.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Select all Document Details that satisfy criteria'
    EXCEPTIONS
      OTHERS = 1.

  LOOP AT watab.
    SELECT * FROM BSEG
       WHERE bukrs = watab-bukrs
         AND belnr = watab-belnr
*         AND augdt IN s_augdt
         AND gjahr = p_gjahr
*        and LIFNR <> SPACE
         AND hkont IN s_hkont
         AND aufnr IN s_aufnr
         AND kostl IN s_kostl
         AND projk IN s_projk
         AND nplnr IN s_nplnr.

      MOVE watab-bukrs     TO excltab-bukrs.
      MOVE watab-belnr     TO excltab-belnr.
      MOVE watab-bldat     TO excltab-bldat.                "TR782
      MOVE watab-budat     TO excltab-budat.
      MOVE watab-blart     TO excltab-blart.
      MOVE watab-lifnr     TO excltab-lifnr.
      MOVE watab-augdt     TO excltab-augdt.                "TR782

      MOVE bseg-pswsl       TO excltab-pswsl.
      MOVE bseg-dmbtr       TO excltab-dmbtr.
      MOVE bseg-bschl       TO excltab-bschl.
      MOVE bseg-hkont       TO excltab-hkont.


      IF bseg-shkzg = 'H'.
        excltab-dmbtr = excltab-dmbtr * -1.
      ENDIF.

      IF bseg-kostl <> ' '.
*         MOVE BSEG-KOSTL+5(5) TO excltab-ACCTING.          "TR342
        MOVE bseg-kostl      TO excltab-accting.            "TR342
      ELSEIF bseg-aufnr <> ' '.
        MOVE bseg-aufnr      TO excltab-accting.
      ELSEIF bseg-projk <> ' '.
        WRITE: bseg-projk    TO excltab-accting.
      ELSEIF bseg-nplnr <> ' '.
        MOVE bseg-nplnr      TO excltab-accting.
      ENDIF.
      APPEND excltab.
    ENDSELECT.     " SELECT FROM BSEG
  ENDLOOP.     " LOOP AT WATAB
ENDFORM.                    "SELECT_BSEG_DOCUMENTS


*--------------------  SELECT_BSEG_DOCUMENT_VENDOR  --------------------
*  get the vendor number from the opposite side of the transaction.
*-----------------------------------------------------------------------
FORM  SELECT_BSEG_DOCUMENTS_VENDOR.

  LOOP AT watab.
    SELECT SINGLE * FROM bseg
      WHERE bukrs = watab-bukrs
        AND belnr = watab-belnr
        AND gjahr = p_gjahr
        AND lifnr IN s_lifnr
        AND ( ( buzei = '001' AND lifnr <> ' ' )
           OR ( buzei <> '001' AND lifnr <> ' ') ).
*        and buzei = '001'       "Vendor always on line 001 of document
*        and lifnr in s_lifnr.

    IF sy-subrc = '0'.
      MOVE bseg-lifnr TO watab-lifnr.
      MODIFY watab.
    ELSE.
      DELETE watab.
    ENDIF.

  ENDLOOP.     " LOOP AT WATAB
ENDFORM.                    "SELECT_BSEG_DOCUMENTS_VENDOR

*--------------------  SELECT_BSEG_DOCUMENT_VENDOR  --------------------
*  get the vendor number from the opposite side of the transaction.
* Added for TR782
*-----------------------------------------------------------------------
FORM  SELECT_BSEG_DOCUMENTS_CLRDT.

  LOOP AT watab.
    SELECT SINGLE * FROM bseg
      WHERE bukrs = watab-bukrs
        AND belnr = watab-belnr
        AND gjahr = p_gjahr
*        AND lifnr IN s_augdt
        AND buzei = '001'.       "Clearing Date always on line 001 of document


    IF sy-subrc = '0'.
      MOVE bseg-augdt TO watab-augdt.
      MODIFY watab.
    ENDIF.

  ENDLOOP.     " LOOP AT WATAB
ENDFORM.                    "SELECT_BSEG_DOCUMENTS_CLRDT
