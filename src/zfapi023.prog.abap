REPORT ZFAPI009 NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65
       MESSAGE-ID ZS.
************************************************************************
*  Program ID:  ZFAPI009
*  Author:      M DeMeester
*  Date:        Feb 14, 2002.
*  Description: This program will read a file of names and addresses
*               and write name & address on form letter in proper
*               position.
************************************************************************
* 2002/02/12 mdemeest #--- Name & address on form letter
************************************************************************
TABLES: LFA1,                          "vendor mast details
        LFB1.                          "company vendor
DATA: BEGIN OF VENDOR occurs 0,
*      CHKDGCB(1),
      LINE1(60),
      Line2(60),
      line3(60),
      line4(60),
      line5(60),
      line6(60),
      END OF VENDOR.

data: wa_keydate like sy-datum.
DATA: POSTCD(10),
      ZIPCODE(7),
      TOWN_NAME(23),
      REGION_CD(02),
      TOWN_SW(1),
      STREET_SW(1),
      BOX_SW(1),
      NAME2_SW(1),
      NAME3_SW(1),
      POBOX(5),
      LONG_NAME(70),
      LEN1 TYPE I,
      LEN2 TYPE I,
      LONGZIP(13).

data: returnx like bapireturn.

data:  wa_ctr  type i.

data: begin of wa_keybalance occurs 10.
      include structure bapi3008_3.
data: end of wa_keybalance.

data:  x  like wa_keybalance-lc_bal.

data:  wa_addresses(160)     type c.
*-------------------------  SELECTION SCREEN  --------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
parameters: p_filein(130)
                 default '/usr/sap/interfaces/P01/CFAP001/ZFAPI023.SAP'
                  lower case.
*parameters:     p_bukrs  like lfb1-bukrs obligatory default 'UGL',
*               p_gjahr  like payr-gjahr obligatory default sy-datum(4),
*                p_min    like bseg-dmbtr obligatory default 1,
*               p_max    like bseg-dmbtr obligatory default 9999999999,
*                p_land1  like lfa1-land1.
*SELECT-OPTIONS: s_lifnr  for lfa1-lifnr,
*                S_KTOKK FOR LFA1-KTOKK,
*                s_knozs for lfa1-konzs.
* purchasing group
SELECTION-SCREEN END OF BLOCK BOX.
*-----------------------------------------------------------------------
*DATA MSG_TEXT(50).
*DATA: PAYEE_FILE(188) TYPE C.

open dataset p_filein for input in text mode.

do.
     read dataset p_filein into wa_addresses.
     if sy-subrc <> '0'.
        close dataset p_filein.
        exit.
     endif.

     if wa_addresses+0(4) = '\row'.                      "New Vendor
        perform print_address.
        move 0 to wa_ctr.
     endif.
     if wa_addresses(20) = '\b\cf9\cbpat11\li60'.
        add 1 to wa_ctr.
     endif.
     if wa_addresses(1) = '\' or wa_addresses(1) = '{'
                              or wa_addresses(1) = '}'.
        else.
     case wa_ctr.
        when 1.                                      "Vendor Number
        when 2.                                      "Vendor Name
          move wa_addresses+0(40)   to vendor-line1.
        when 3.                                      "Vendor Address
          move wa_addresses+0(40)   to vendor-line2.
        when 4.                                      "Vendor Country
          move wa_addresses+0(40)   to vendor-line5.
        when 5.                                      "Vendor Postal Code
          move wa_addresses+0(40)   to vendor-line4.
        when 6.                                      "Vendor City
          move wa_addresses+0(40)   to vendor-line3.
      endcase.
    endif.
enddo.

form print_address.

     new-page.
     skip 11.

     write: /12 vendor-line1.
     write: /12 vendor-line2.
     write: /12 vendor-line3.
     write: /12 vendor-line4.
     write: /12 vendor-line5.
     write: /12 vendor-line6.

endform.

*-----------------------  CHECK_RANGE  ---------------------------------
*  Check amounts for the date ending Dec. 31 of entered year
*-----------------------------------------------------------------------
form check_range.

* call function 'BAPI_AP_ACC_GETKEYDATEBALANCE'
*     exporting
*           companycode            = p_bukrs
*           vendor                 = lfa1-lifnr
*           keydate                = wa_keydate
*       importing
*           return                 = returnx
*       tables
*           keybalance             = wa_keybalance.

*       if returnx-type = ' '.              "Valid return
*         x = abs( wa_keybalance-lc_bal ).
*          if x between p_min and p_max.
*             move '0' to sy-subrc.
*         else.
*            move '4' to sy-subrc.
*         endif.
*      else.
*          move '4'    to sy-subrc.
*       endif.

endform.












