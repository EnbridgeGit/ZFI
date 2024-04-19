REPORT ZFAPI009 NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65
       MESSAGE-ID ZS.
************************************************************************
*  Program ID:  ZFAPI009
*  Author:      M DeMeester
*  Date:        Nov 18, 1997.
*  Description: This program will read the vendor table, extract
*               vendor name as well as address and write
*               name & address on form letter in proper position.
************************************************************************
* 2002/02/12 mdemeest #--- Name & address on form letter
* 97/11/18 md7140 #287 Create list of fax numbers
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

data: wa_fromdate like sy-datum.
data: wa_todate   like sy-datum.

*DATA: POSTCD(10),
*     ZIPCODE(7),
*  X*    TOWN_NAME(23),
*      REGION_CD(02),
*      TOWN_SW(1),
*      STREET_SW(1),
*      BOX_SW(1),
*     NAME2_SW(1),
*      NAME3_SW(1),
*      POBOX(5),
*      LONG_NAME(70),
*      LEN1 TYPE I,
*      LEN2 TYPE I,
*      LONGZIP(13).


data: returnx like bapireturn.


data: begin of wa_lineitems occurs 12.
      include structure bapi3008_2.
data: end of wa_lineitems.

data:  wa_lc_amount  like wa_lineitems-lc_amount.

data:  begin of wa_vendors     occurs 2000,
       name1    like lfa1-name1,
       lifnr    like lfa1-lifnr,
       lc_amount like wa_lc_amount,
       end of wa_vendors.


*-------------------------  SELECTION SCREEN  --------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
*parameters labels(128)
*                 default '/usr/sap/interfaces/P01/CFAP001/ZFAPI009.SAP'
*                  lower case.
parameters:     p_bukrs  like lfb1-bukrs obligatory default 'UGL',
                p_gjahr  like payr-gjahr obligatory default sy-datum(4),
                p_min    like bseg-dmbtr obligatory default 1,
                p_max    like bseg-dmbtr obligatory default 9999999999.
SELECT-OPTIONS: s_lifnr  for lfa1-lifnr,
                s_land1  for lfa1-land1 no intervals,
                S_KTOKK FOR LFA1-KTOKK,
                s_knozs for lfa1-konzs.
* purchasing group
SELECTION-SCREEN END OF BLOCK BOX.
*-----------------------------------------------------------------------
DATA MSG_TEXT(50).
DATA: PAYEE_FILE(188) TYPE C.

* build date - the first to last date of the requested year
  move p_gjahr  to wa_fromdate(4).
  move '0131'   to wa_fromdate+4(4).
  move p_gjahr  to wa_todate(4).
  move '1231'   to wa_todate+4(4).

SELECT * FROM LFA1
   WHERE lifnr in s_lifnr         "Vendor Number
     and KTOKK IN S_KTOKK         "Vendor Group
     and land1 in s_land1.         "Country where sent.
   select single * from lfb1      "Determine if vendor in requested Comp
       where lifnr = lfa1-lifnr
         and bukrs = p_bukrs.

   if sy-subrc = '0'.
      perform check_range.
      if sy-subrc = '0'.
         clear vendor.
         move lfa1-name1 to vendor-line1.
         move lfa1-name2 to vendor-line2.
         if lfa1-pfach <> space.
            concatenate: 'P.O. Box' lfa1-pfach into vendor-line3
                                            separated by space.
         else.
            move lfa1-stras to vendor-line3.
         endif.
         concatenate: lfa1-ort01 ',' lfa1-regio into vendor-line4.
         move lfa1-pstlz to vendor-line5.

         new-page.
         skip 9.

         write: /12 vendor-line1.
         write: /12 vendor-line2.
         write: /12 vendor-line3.
         write: /12 vendor-line4.
         write: /12 vendor-line5.
         write: /12 vendor-line6.
     endif.
   endif.

ENDSELECT.

sort wa_vendors by name1.
new-page.
write: /1 'PARAMETERS'.
write: /1  'COMPANY', 12 p_bukrs.
write: /1  'YEAR',    12 p_gjahr.
write: /1  'RANGE',   12 p_min,    30 p_max.
write: /1  'GROUP',   12 S_ktokk+3(8).
uline.
skip 1.
loop at wa_vendors.
     write: /1 wa_vendors-name1, 40 wa_vendors-lc_amount,
                                                    70 wa_vendors-lifnr.
endloop.
*-----------------------  CHECK_RANGE  ---------------------------------
*  Check amounts for the date ending Dec. 31 of entered year
*-----------------------------------------------------------------------
form check_range.

 call function 'BAPI_AP_ACC_GETSTATEMENT'
      exporting
           companycode            = p_bukrs
           vendor                 = lfa1-lifnr
           date_from              = wa_fromdate
           date_to                = wa_todate
       importing
           return                 = returnx
       tables
           lineitems             = wa_lineitems.

       if returnx-type = ' '.              "Valid return
          move 0 to wa_lc_amount.
          loop at wa_lineitems.
            if  wa_lineitems-db_cr_ind = 'S'.
                compute wa_lc_amount = wa_lc_amount
                                               + wa_lineitems-lc_amount.
            endif.
          endloop.
          if wa_lc_amount between p_min and p_max.
             move '0' to sy-subrc.
             move lfa1-lifnr   to wa_vendors-lifnr.
             move lfa1-name1   to wa_vendors-name1.
             move wa_lc_amount to wa_vendors-lc_amount.
             append wa_vendors.
          else.
             move '4' to sy-subrc.
          endif.
       else.
          move '4'    to sy-subrc.
      endif.

endform.
