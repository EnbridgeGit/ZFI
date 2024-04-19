FUNCTION ZAP_IAP_UG_TAXCODE_PO_GRP2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(HEADER_DATA) LIKE  ZAPS_IAP_DOC_HDR STRUCTURE
*"        ZAPS_IAP_DOC_HDR
*"  TABLES
*"      ITEM_DATA STRUCTURE  BAPI_INCINV_CREATE_ITEM
*"      TAXDATA STRUCTURE  BAPI_INCINV_CREATE_TAX
*"----------------------------------------------------------------------
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Function Module:  ZAP_IAP_UG_TAXCODE_GRP2                           *
*  Author:           Vijay Rajaputra                                   *
*  Date:             AUG, 31 2018                                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      Post IAP Documents for PO Invoices Tax Code       *
*                    Group 2 ( H4, M5, M7, Q2, T1, T3 )                *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 08/31/18 VRAJAPUTRA D30K928897 - CHG0109670 - Initial Development    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
  DATA : ls_header     TYPE ZAPS_IAP_DOC_HDR,
         lt_item       TYPE TABLE OF BAPI_INCINV_CREATE_ITEM,
         lt_tax        TYPE TABLE OF BAPI_INCINV_CREATE_TAX,
         ls_item       TYPE BAPI_INCINV_CREATE_ITEM,
         ls_tax        TYPE BAPI_INCINV_CREATE_TAX.




  CLEAR : ls_header,lt_item,lt_tax,ls_item,ls_tax.

*-> Data transfer to Local fields.
  ls_header = HEADER_DATA.
  lt_item[] = ITEM_DATA[].
  lt_tax[]  = TAXDATA[].
*BREAK vrajaputra.
*----------------------------------------------------------------------*
  CLEAR : ls_tax,ls_item.

**  CLEAR ls_tax.
**  ls_tax-tax_amount = ls_header-taxamnt.
**  ls_tax-TAX_CODE = ls_header-TAXCODE.
**  APPEND ls_tax to lt_tax .
**  CLEAR ls_tax.


*  LOOP AT lt_item INTO ls_item.
*    ls_tax-tax_amount = ls_item-taxamnt.
*    ls_tax-TAX_CODE = ls_item-TAXCODE.
*    APPEND ls_tax to lt_tax .
*    CLEAR ls_tax.
*  ENDLOOP.

* Returning data to FM
  ITEM_DATA[] = lt_item[].
  TAXDATA[]    = lt_tax[].



ENDFUNCTION.
