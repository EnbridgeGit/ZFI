FUNCTION zfi_iap_xref_vm.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IAP_DOC_ID STRUCTURE  ZAPS_IAP_DOC_ID
*"      IAP_XREF_VM STRUCTURE  ZAPS_IAP_XREF_VM
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Func Group Name    :   ZFI_IAP                                       *
* Function Module    :   ZFI_IAP_XREF_VM                               *
* Author             :   John Hartung                                  *
* Date               :   Apr 11, 2018                                  *
* Technical Contact  :   John Hartung                                  *
*                                                                      *
* Purpose            :   IAP-SAP Vendor Cross Reference                *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 11-Apr-2018  JRHARTUNG   D30K928751  CHG0107999 Initial development  *
*----------------------------------------------------------------------*

  DATA:    lt_iap_doc_id     TYPE STANDARD TABLE OF zaps_iap_doc_id,
           lt_iap_xref_vm    TYPE STANDARD TABLE OF zaps_iap_xref_vm.

  CLEAR    IAP_XREF_VM[].

  CLEAR    lt_iap_doc_id[].
  CLEAR    lt_iap_xref_vm[].

  lt_iap_doc_id[] = IAP_DOC_ID[].

  SELECT   mandt  zz_iap_vendor_id  lifnr  ktokk
    INTO   TABLE lt_iap_xref_vm
    FROM   lfa1 FOR ALL ENTRIES IN lt_iap_doc_id
   WHERE   zz_iap_vendor_id = lt_iap_doc_id-iap_doc_id.
  IF     ( sy-subrc EQ 0 ).
    IAP_XREF_VM[] = lt_iap_xref_vm[].
  ENDIF.

ENDFUNCTION.
