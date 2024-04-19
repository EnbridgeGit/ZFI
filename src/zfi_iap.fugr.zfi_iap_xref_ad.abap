FUNCTION zfi_iap_xref_ad.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IAP_DOC_ID STRUCTURE  ZAPS_IAP_DOC_ID
*"      IAP_XREF_AD STRUCTURE  ZAPT_IAP_XREF_AD
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Func Group Name    :   ZFI_IAP                                       *
* Function Module    :   ZFI_IAP_XREF_AD                               *
* Author             :   John Hartung                                  *
* Date               :   Apr 11, 2018                                  *
* Technical Contact  :   John Hartung                                  *
*                                                                      *
* Purpose            :   IAP-SAP Invoice Cross Reference               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 11-Apr-2018  JRHARTUNG   D30K928751  CHG0107998 Initial development  *
*----------------------------------------------------------------------*

  DATA:    lt_iap_doc_id     TYPE STANDARD TABLE OF zaps_iap_doc_id,
           lt_iap_xref_ad    TYPE STANDARD TABLE OF zapt_iap_xref_ad.

  CLEAR    IAP_XREF_AD[].

  CLEAR    lt_iap_doc_id[].
  CLEAR    lt_iap_xref_ad[].

  lt_iap_doc_id[] = IAP_DOC_ID[].

  SELECT   *
    INTO   TABLE lt_iap_xref_ad
    FROM   zapt_iap_xref_ad FOR ALL ENTRIES IN lt_iap_doc_id
   WHERE   zz_iap_ritm_doc = lt_iap_doc_id-iap_doc_id.
  IF     ( sy-subrc EQ 0 ).
    IAP_XREF_AD[] = lt_iap_xref_ad[].
  ENDIF.

ENDFUNCTION.
