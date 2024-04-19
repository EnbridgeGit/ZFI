"Name: \FU:MAINTAIN_IBAN_DIAL\SE:BEGIN\EI
ENHANCEMENT 0 ZNO_IBAN_CHECK_PASS_BANK_INFO.
*------------------------------------------------------------------------*
*Changed By   Date      Description
*S.Ahmad      20120905  Added logic to remove IBAN Check in case country *
*                       maintained in SETLEAF.
*------------------------------------------------------------------------*
data : lv_bank_code        type tiban-banks,
         lv_no_iban(6)       type c value 'NOIBAN'.

constants: lc_tcode_fk01(3) type c               value 'K01',
           lc_tcode_fk02(3) type c               value 'K02',
           lc_tcode_fk03(3) type c               value 'K03'.

if ( sy-tcode+1 = lc_tcode_fk01 ) or
   ( sy-tcode+1 = lc_tcode_fk02 ) or
   ( sy-tcode+1 = lc_tcode_fk03 ) .
   clear : lv_bank_code.
   lv_bank_code = i_banks.
   export lv_bank_code to memory id lv_no_iban.
endif.

ENDENHANCEMENT.
