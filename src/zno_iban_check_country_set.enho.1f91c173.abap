"Name: \FU:CHECK_IBAN\SE:BEGIN\EI
ENHANCEMENT 0 ZNO_IBAN_CHECK_COUNTRY_SET.
*------------------------------------------------------------------------*
*Changed By   Date      Description
* S.Ahmad     20120905  Added logic to remove IBAN Check in case country *
*                       maintained in SETLEAF.
*------------------------------------------------------------------------*

  TYPES : BEGIN OF TY_setleaf,
            SETCLASS  TYPE  SETLEAF-SETCLASS,
            SUBCLASS  TYPE  SETLEAF-SUBCLASS,
            SETNAME    TYPE   SETLEAF-SETNAME,
            LINEID     TYPE   SETLEAF-LINEID,
            VALSIGN   TYPE  SETLEAF-VALSIGN,
            VALOPTION  TYPE   SETLEAF-VALOPTION,
            VALFROM   TYPE  SETLEAF-VALFROM,
            VALTO      TYPE   SETLEAF-VALTO,
            SEQNR     TYPE  SETLEAF-SEQNR,
          end of ty_setleaf.

 DATA : lit_setleaf         type table of ty_setleaf,
        lwa_setleaf         type ty_setleaf,
        lwa_country_code(2) type c,
        lv_no_iban_check(1) type c,
        lv_bank_code        type tiban-banks,
        lv_no_iban(6)       type c  value 'NOIBAN'.


constants: lc_tcode_fk01(3) type c               value 'K01',
           lc_tcode_fk02(3) type c               value 'K02',
           lc_tcode_fk03(3) type c               value 'K03',
           lc_setname       type setleaf-setname value 'NOIBANCTRY'.


clear :    lwa_setleaf,
           lwa_country_code,
           lv_no_iban_check.

refresh  : lit_setleaf.

SELECT SETCLASS SUBCLASS SETNAME LINEID VALSIGN VALOPTION VALFROM VALTO SEQNR
   from setleaf
   into table lit_setleaf
   where setname = lc_setname.
 if sy-subrc = 0.
  if  ( sy-tcode+1 = lc_tcode_fk01 ) or
      ( sy-tcode+1 = lc_tcode_fk02 ) or
      ( sy-tcode+1 = lc_tcode_fk03 ) .

     clear : lv_bank_code.
     IMPORT lv_bank_code from MEMORY ID lv_no_iban.
    if sy-subrc = 0.
       lwa_country_code = lv_bank_code.
       free memory id lv_no_iban.
       clear: lv_bank_code.
    endif.

    if lwa_country_code is not initial.
     loop at lit_setleaf into lwa_setleaf.
       if ( lwa_country_code = lwa_setleaf-VALFROM ) or
          ( lwa_country_code = lwa_setleaf-VALTO ).
            lv_no_iban_check = 'X'.
       endif.
     endloop.
    endif.
  endif.
 endif.
 check lv_no_iban_check ne 'X'.
ENDENHANCEMENT.
