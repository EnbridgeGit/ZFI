*&---------------------------------------------------------------------*
*&  Include           ZFAPR007_VENDOR_INVOICE_T
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPR007_VENDOR_INVOICE_T                     *
*& Author             :  Sajjad Ahmad                                  *
*& Creation Date      :  Nov 07, 2011                                  *
*& Object ID          :  R_P2C_AP_007-Vendor Invoices with special     *
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
TYPE-POOLS: slis.
************************************************************************
*          TYPES  Declaration                                          *
************************************************************************
******* Types Declaration for BKPF(Accounting Document Header)
TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bukrs,  "Company Code
         belnr TYPE belnr_d,"Accounting Document Number
         gjahr TYPE gjahr,  "Fiscal Year
         blart TYPE blart,  "Document Type
         xblnr TYPE xblnr1, "Reference Document Number
         awkey type bkpf-awkey,
       END OF ty_bkpf.

******* Types Declaration for BSEG (Accounting Document Segment)
TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,  "Company Code
         belnr TYPE belnr_d,"Accounting Document Number
         gjahr TYPE gjahr,  "Fiscal Year
         buzei TYPE buzei,  "Number of Line Item Within Accounting Document
         augbl TYPE augbl,  "Document Number of the Clearing Document
         zlsch TYPE schzw_bseg,"Payment Method
         uzawe TYPE uzawe,  "Payment Method Supplement
         lifnr TYPE lifnr,  "Account Number of Vendor or Creditor
         hbkid TYPE hbkid,  "Short Key for a House Bank
         hktid TYPE hktid,  "ID for Account Details
       END OF ty_bseg.

******* Types Declaration for LFA1 (Vendor Master)
TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lifnr,   "Vendor Number
         name1 TYPE name1_gp,"Vendor Name
       END OF ty_lfa1.

******* Types Declaration for PAYR(Payment Medium File)
TYPES: BEGIN OF ty_payr,
         zbukr TYPE dzbukr,   "Paying company code
         hbkid TYPE hbkid,    "Short Key for a House Bank
         hktid TYPE hktid,    "ID for Account Details
         rzawe TYPE dzlsch,   "Payment Method
         chect TYPE chect,    "Check number
         vblnr TYPE vblnr,   "Document Number of the Payment Document
         gjahr TYPE gjahr, " Fiscal Year
         pridt TYPE pridt,   " Print Date/ Check Issue Date
         uzawe type uzawe,
       END OF ty_payr.
******* Types Declaration for PAYR(Payment Medium File)
TYPES: BEGIN OF ty_payr1,
         zbukr TYPE dzbukr,   "Paying company code
         hbkid TYPE hbkid,    "Short Key for a House Bank
         hktid TYPE hktid,    "ID for Account Details
         rzawe TYPE dzlsch,   "Payment Method
         chect TYPE chect,    "Check number
         lifnr TYPE lifnr,    " Vendor Number
         vblnr TYPE vblnr,   "Document Number of the Payment Document
         gjahr TYPE gjahr, " Fiscal Year
         pridt TYPE pridt,   " Print Date/ Check Issue Date
       END OF ty_payr1.

******* Types Declaration for Final Table
TYPES: BEGIN OF ty_final,
         bukrs TYPE bukrs,   "Company Code
         lifnr TYPE lifnr,   " Vendor Number
         name1 TYPE name1_gp," Vendor Name
         xblnr TYPE xblnr1,  "Reference Document Number
         blart TYPE blart,   "Document Type
         belnr TYPE belnr_d, "Accounting Document Number
         augbl TYPE augbl,   "Document Number of the Clearing Document
         chect TYPE chect,   "Check number
         zlsch TYPE schzw_bseg,"Payment Method
         uzawe TYPE uzawe,   "Payment Method Supplement
         pridt TYPE pridt,   " Print Date Check Issue Date
         hbkid TYPE hbkid,  "Short Key for a House Bank
         hktid TYPE hktid,  "ID for Account Details
         sgtxt type bsik-sgtxt,
         TdLINE type TDLINE, "Invoice long text
       END OF ty_final.

************************************************************************
*        WORK AREA DECLARATION                                         *
************************************************************************

DATA: gwa_bkpf  TYPE ty_bkpf,
      gwa_bseg  TYPE ty_bseg,
      gwa_lfa1  TYPE ty_lfa1,
      gwa_payr  TYPE ty_payr,
      gwa_payr1 TYPE ty_payr1,
      gwa_final TYPE ty_final.

************************************************************************
*        INTERNAL TABLE DECLARATION                                    *
************************************************************************

DATA: git_bkpf  TYPE STANDARD TABLE OF ty_bkpf,
      git_bseg  TYPE STANDARD TABLE OF ty_bseg,
      git_lfa1  TYPE STANDARD TABLE OF ty_lfa1,
      git_payr  TYPE STANDARD TABLE OF ty_payr,
      git_payr1 TYPE STANDARD TABLE OF ty_payr1,
      git_final TYPE STANDARD TABLE OF ty_final,
      git_fcat  TYPE slis_t_fieldcat_alv.
data: gt_bsik type standard table of bsik,
      gs_bsik like line of gt_bsik.
************************************************************************
*        VARIABLE DECLARATION                                          *
************************************************************************

DATA:  gv_count TYPE i,     "Count for Coloum Number
       gv_kostl TYPE kostl, "Cost Center
       gv_monat TYPE monat, "Fiscal Period
       gv_budat TYPE budat, "Posting Date in the Document
       gv_bkpf  TYPE char1, " Flag for Table data
       gv_bseg  TYPE char1. " Flag for Table data

************************************************************************
*        VARIABLE DECLARATION                                          *
************************************************************************
CONSTANTS: gc_x  TYPE char1 VALUE 'X'.
