*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPR007_VENDOR_INVOICE_REPORT                *
*& Author             :  Sajjad Ahmad                                  *
*& Creation Date      :  Nov 07, 2011                                   *
*& Object ID          :  R_P2C_AP_007-Vendor Invoices with special     *
*                        Handeling Report                              *
*& Application Area   :  FI-AP                                         *
*& Description        :  Vendor Invoices with special Handeling Report *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
*Changed By  Date      Version     Description                         *
*SAHMAD      20111207  D30K918792  Performance Issue due to BSEG table *
*----------------------------------------------------------------------*

REPORT zfapr007_vendor_invoice_report NO STANDARD PAGE HEADING
                                              MESSAGE-ID zfi01.

*----------------------------------------------------------------------*
* Include - Data Declarations                                          *
*----------------------------------------------------------------------*

INCLUDE zfapr007_vendor_invoice_t.

**********************************************************************
*         S E L E C T I O N     S C R E E N                          *
**********************************************************************
*skip 2.
PARAMETER: rb_1 TYPE c RADIOBUTTON GROUP gp1 USER-COMMAND cmd.
SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_lifnr FOR  gwa_lfa1-lifnr MODIF ID clr,    "Vendor Number
                s_bukrs FOR  gwa_bkpf-bukrs MODIF ID clr, "OBLIGATORY MODIF ID clr, "Co-Code
                s_kostl FOR  gv_kostl MODIF ID clr.          "Department(cost Center )
PARAMETERS      p_gjahr TYPE gjahr MODIF ID clr. "OBLIGATORY MODIF ID clr . "Fiscal Year
SELECT-OPTIONS: s_monat FOR  gv_monat MODIF ID clr ,         "Fiscal Period
                s_budat FOR  gv_budat MODIF ID clr,         "Posting Date
                s_blart FOR  gwa_bkpf-blart MODIF ID clr,    "Document Type
                s_belnr FOR  gwa_bkpf-belnr MODIF ID clr,   "Document Number
                s_xblnr FOR  gwa_bkpf-xblnr MODIF ID clr.    "PO Number
SELECTION-SCREEN END OF BLOCK  bk1 .

PARAMETER: rb_2 TYPE c RADIOBUTTON GROUP gp1.
SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_bukrs1 FOR gwa_bkpf-bukrs MODIF ID cls, "OBLIGATORY MODIF ID cls, "Co-Code
                s_date   FOR gwa_payr-pridt MODIF ID cls,    " Check Issue Date
                s_hbkid FOR  gwa_payr-hbkid MODIF ID cls,    "House Bank
                s_lifnr1  FOR gwa_lfa1-lifnr MODIF ID cls,    "Vendor Number
                s_rzawe  FOR gwa_payr-rzawe MODIF ID cls, "Payment method
                s_uzawe FOR gwa_payr-uzawe MODIF ID cls, "Payment method supplement
                s_chect FOR gwa_payr-chect MODIF ID cls.
SELECTION-SCREEN END OF BLOCK  bk2 .

**********************************************************************
*         AT-SELECTION-SCREEN                                        *
**********************************************************************

AT SELECTION-SCREEN ON BLOCK bk1.
  IF rb_1 = 'X' AND
     sy-ucomm = 'ONLI'.
    IF s_bukrs[] IS INITIAL.
      MESSAGE i000 WITH 'Enter Company Code in Selection Criteria 1' '' '' ''.
      STOP.
    ENDIF.
    IF p_gjahr IS INITIAL.
      MESSAGE i000 WITH 'Enter Fiscal Year in Selection Criteria 1' '' '' ''.
      STOP.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON BLOCK bk2.
  IF rb_2 = 'X' AND
     sy-ucomm = 'ONLI'.
    IF s_bukrs1[] IS INITIAL.
      MESSAGE i000 WITH 'Enter Company Code in Selection Criteria 2' '' '' ''.
      STOP.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON s_lifnr.
*Validate the Vendor
  PERFORM f_validate_vendor.

AT SELECTION-SCREEN ON s_bukrs.
*Validate the Company Code
  PERFORM f_validate_companycode.

AT SELECTION-SCREEN ON s_kostl.
*Validate the Department
  PERFORM f_validate_department.

AT SELECTION-SCREEN ON s_monat.
*Validate the Fiscal Period
  PERFORM f_validate_fiscal_period.

AT SELECTION-SCREEN ON s_blart.
*Validate the Document Type
  PERFORM f_validate_document_type.

AT SELECTION-SCREEN ON s_belnr.
*Validate the Document Date
  PERFORM f_validate_document_number.

AT SELECTION-SCREEN ON s_xblnr.
*Validate the Reference Number
  PERFORM f_validate_reference_number.

AT SELECTION-SCREEN ON s_bukrs1.
*Validate the Company Code
  PERFORM f_validate_companycode1.

AT SELECTION-SCREEN ON s_lifnr1.
*Validate the Vendor
  PERFORM f_validate_vendor1.

AT SELECTION-SCREEN OUTPUT.
*Disable the Selection screen Iputs
  PERFORM f_disable_input_fields.

**********************************************************************
*       START-OF-SELECTION                                           *
**********************************************************************
START-OF-SELECTION.

  PERFORM clear_memory.
*break sahmad.
  IF rb_1 = gc_x.   "Selection Criteria 1
    PERFORM sel_criteria1.
  ELSE.   "Selection Criteria 2
    PERFORM sel_criteria2.
  ENDIF.
*Prepare ALV List Display
  IF git_final[] IS NOT INITIAL.
    PERFORM f_display_alv_list.
  ELSE.
    WRITE: / 'No Data Found for given selection criteria'.
  ENDIF.
*----------------------------------------------------------------------*
* Include - Subroutines                                                *
*----------------------------------------------------------------------*
  INCLUDE zfapr007_vendor_invoice_f.
