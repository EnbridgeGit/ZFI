"Name: \PR:SAPLFDCB\FO:ZTERM_TEXT\SE:BEGIN\EI
ENHANCEMENT 0 ZFI_DEFAULT_PAYMENTTERM.
*----------------------------------------------------------------------*
*Revision # MZH01                                 Name: Zakir Hossain  *
*SDP Ticket # 57796                               Date: 9/29/2015      *
*Description: Change the logic for WT tax type                         *
*&---------------------------------------------------------------------*

*Begin of MZH01
types: begin of ty_witht,
         witht type SETVALMIN,
       end of ty_witht.

data: lv_witht   type WITHT,
      lt_witht   type STANDARD TABLE OF ty_witht,
      ls_witht   like LINE OF lt_witht,
      lv_subject type WT_SUBJCT.

ranges: r_witht for lv_witht.

  select VALFROM into table lt_witht from setleaf where SETNAME = 'WHTTYPE'.
  if sy-subrc eq 0.
    loop at lt_witht into ls_witht.
      r_witht-sign   = 'I'.
      r_witht-option = 'EQ'.
      r_witht-low    = ls_witht-witht.
      append r_witht.
    endloop.
  select single wt_subjct from LFBW
                          into lv_subject
                          where LIFNR = BSEG-LIFNR
                          AND   BUKRS = BSEG-BUKRS
                          and   witht in r_witht.
     if sy-subrc eq 0 and lv_subject = 'X'.
      MESSAGE ID 'ZFI01' TYPE 'I' NUMBER 000 WITH 'Vendor' BSEG-LIFNR 'subject to withholding tax'.
      EXIT.
     endif.
  endif.
*  End of MZH01
ENDENHANCEMENT.
