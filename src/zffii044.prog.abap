REPORT ZFFII044 MESSAGE-ID ZS line-size 255 line-count 65.
************************************************************************
* 2013/03/06 gymana   SDP41636/SDP41651
*                     Replace header text with english descriptions and
*                     turn on fields 35-37.
* 2013/02/11 mohammad SDP29338 Increase the length of ref. doc field
* 2009/06/24 mdemeest $--- Add non-matching PO's to PO file
* 2009/05/20 mdemeest #--- Spend Analysis - new specs for BIQ
* 2009/06/08 mdemeest #___ J. Ludwig request for selection by clearing
*                          date
* 2009/06/09 mdemeest #___ J. Ludwig request for selection by USNAM
*
************************************************************************
* NOTES : Used function CLOI_PUT_SIGN_IN_FRONT
*         All commas in the data are replaced with _ so that the .csv
*         file is formatted appropriately
*         Memory ID - Will default to entered user parameters
*                     COMPANY CODE - parameter is BUK
*                     CHART OF ACCOUNTS - parameter is KPL
*-----------------------------------------------------------------------
TABLES: BKPF,           "Document Header
        BSEG,           "Document Detail
        T001,           "Company Code
        LFA1,           "Vendor Information
        mseg,           "PO Detail
        t007s,          "Tax Code Descriptions
        skat,           "Chart of Accounts
        ekpo,           "Purchase order detail
        t001w,          "Plant name
        tbslt,          "Posting Key Description
        t161t,          "PO Type Description
        t023t,          "Material Group Description
        ekko,           "PO Header
        ekbe,           "Goods/Invoice Receipt table
        t003t,          "Document Type Code Descriptions
        sadr,           "Address table
        payr,           "Cheque table
        afih,           "Maintenance Activity type
        mara.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER: PHYFILE LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/biq.csv',
           RECSIZE TYPE I DEFAULT 1000,

           PHYFILE2 LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/po.csv',
           RECSIZE2 TYPE I DEFAULT 150,

           PHYFILE3 LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/doc.csv',
           RECSIZE3 TYPE I DEFAULT 100.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 2.

SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
parameter:      p_dtsouc(4) type c default 'EAST'.
PARAMETER:      P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4),
                P_ktopl like skat-ktopl obligatory memory id KPL.
select-options: s_bukrs for bkpf-bukrs no intervals MEMORY ID BUK,
                s_usnam for bkpf-usnam no intervals,
                s_monat for bkpf-monat no intervals obligatory,
                s_blart for bkpf-blart no intervals,
                s_budat for bkpf-budat,
                s_augdt for bseg-augdt,
                s_belnr for bkpf-belnr.
selection-screen end of block box3.

selection-screen comment 1(80) text-900.


* Global Data Definitions
DATA: RECORD(1000).
DATA: RECORD2(150).
DATA: RECORD3(100).

DATA: BEGIN OF WAmaster occurs 0,
      BUTXT LIKE T001-BUTXT,          "Company Name(1)
      BELNR LIKE BKPF-BELNR,          "Document#   (2)
      LIFNR LIKE BSEG-LIFNR,          "Vendor      (3)
      ktokk like lfa1-ktokk,          "VendorGroup (4)
      name1 like lfa1-name1,          "VendorName  (5)
      land1 like lfa1-land1,          "Country     (6)
      stras like lfa1-stras,          "Street      (7)
      ort01 like lfa1-ort01,          "City        (8)
      regio like lfa1-regio,          "Province/St (9)
      pstlz like lfa1-pstlz,          "Postal code (10)
      filler1(3) type c,              "N/A         (11)
      BUDAT(10)  type c,              "Postdate    (12)
      tdmbtr(16) type c,              "TotAmt in LC(13)
      augdt(10)  type c,              "Clearing Dt (14)
      paymeth(8) TYPE C,              "Payment Meth 15)
*      xblnr like bkpf-xblnr,          "ReferenceDoc(16)   "SDP29388
      xblnr(17)  TYPE C,              "ReferenceDoc(17)    "SDP29388
      bldat(10)  type c,              "DocumentDate(17)
      saknr like bseg-saknr,          "G/L         (18)
      hkont like bseg-hkont,          "LineItemAcct(19)
      txt20 like skat-txt20,          "G/L AcctDesc(20)
      sgtxt  like bseg-sgtxt,         "LineItemTxt (21)
* append bseg-buzei (21)
      dmbtr(16) type c,               "Item Amt    (22)
*      projk  like bseg-projk,         "WBS         (23)
      posid like prps-posid,          "WBS         (23)
      taxtext(50) type c,             "Tax Code+Desc(24)
      pname1 like t001w-name1,        "Plant Name  (25)
      port01 like t001w-ort01,        "Shipto Dest (26)
      ebeln  like bseg-ebeln,         "PurchOrder  (27)
      zterm like bseg-zterm,          "Pay Terms   (28)
      chect(10) type c,               "Check/Eft#  (29)
      cad(4) type c,                  "Currency    (30)
      datasrce(4) type c,             "East/West   (31)
      ltext  like tbslt-ltext,        "PstKyCd&Desc(32)
      bktxt  like bkpf-bktxt,         "DocHeaderTxt(33)
      filler8(3),                     "N/A         (34)
      ebelp(5) type c,                "PO Item no. (35)      "SDP41651
      matkl(25) type c,               "MatGrp Desc (36)      "SDP41651
      matdesc(47) type c,             "Mat#&Desc.  (37)      "SDP41651
      fillerc(3),                     "N/A         (38)
      fillerd(3),                     "N/A         (39)
      fillere(3),                     "N/A         (40)
      fillerf(3),                     "N/A         (41)
      fillerg(3),                     "N/A         (42)

      END of WAMASTER.

DATA: BEGIN of WAPO  occurs 0,
      ebeln  like bseg-ebeln,        "PO number    (1)
      grdate(10) type c,              "GR date      (2)
      ebelp  like ekpo-ebelp,        "Item #       (3)
      matkl(25) type c,              "MatGroup Desc(4)
      batxt(30) type c,              "PO Type Desc (5)
      matdesc(47) type c,            "Mat# & Desc  (6)
      filler1(3) type c,             "N/A          (7)
      filler2(3) type c,             "N/A          (8)
      filler3(3) type c,             "N/A          (9)
      END of WAPO.


DATA: BEGIN of WADOC occurs 0,
      belnr   like bkpf-belnr,       "Document      (1)
      blart(23) type c,  lart,       "Document Type (2)
      usnam   like bkpf-usnam,       "Entered by    (3)
      filler1(3) type c,             "N/A           (4)
      filler2(3) type c,             "N/A           (5)
      END of WADOC.

DATA: BEGIN OF wabase occurs 0,

      filler1(3) type c,              "N/A         (11)
      cad(4) type c,                  "Currency    (30)
      datasrce(4) type c,             "East/West   (31)
      filler8(3),                     "N/A         (34)
      filler9(3),                     "N/A         (35)
      fillera(3),                     "N/A         (36)
      fillerb(3),                     "N/A         (37)
      fillerc(3),                     "N/A         (38)
      fillerd(3),                     "N/A         (39)
      fillere(3),                     "N/A         (40)
      fillerf(3),                     "N/A         (41)
      fillerg(3),                     "N/A         (42)
      END of wabase.

DATA: BEGIN OF WAbasebkpf occurs 0,
      BUTXT LIKE T001-BUTXT,          "Company Name(1)
      BELNR LIKE BKPF-BELNR,          "Document#   (2)
      LIFNR LIKE BSEG-LIFNR,          "Vendor      (3)
      ktokk like lfa1-ktokk,          "VendorGroup (4)
      name1 like lfa1-name1,          "VendorName  (5)
      land1 like lfa1-land1,          "Country     (6)
      stras like lfa1-stras,          "Street      (7)
      ort01 like lfa1-ort01,          "City        (8)
      regio like lfa1-regio,          "Province/St (9)
      pstlz like lfa1-pstlz,          "Postal code (10)
      filler1(3) type c,              "N/A         (11)
      BUDAT(10)  type c,              "Postdate    (12)
      tdmbtr(16) type c,              "TotAmt in LC(13)
      augdt(10) type c,               "Clearing Dt (14)
      paymeth(8) TYPE C,              "Payment Meth 15)
      xblnr like bkpf-xblnr,          "ReferenceDoc(16)
      bldat(10)  type c,              "DocumentDate(17)
      saknr like bseg-saknr,          "G/L         (18)
      sgtxt  like bseg-sgtxt,         "LineItemTxt (21)
      pname1 like t001w-name1,        "Plant Name  (25)
      zterm like bseg-zterm,          "Pay Terms   (28)
      chect(10) type c,               "Check/Eft#  (29)
      cad(4) type c,                  "Currency    (30)
      datasrce(4) type c,             "East/West   (31)
      bktxt  like bkpf-bktxt,         "DocHeaderTxt(33)
      filler8(3),                     "N/A         (34)
      filler9(3),                     "N/A         (35)
      fillera(3),                     "N/A         (36)
      fillerb(3),                     "N/A         (37)
      fillerc(3),                     "N/A         (38)
      fillerd(3),                     "N/A         (39)
      fillere(3),                     "N/A         (40)
      fillerf(3),                     "N/A         (41)
      fillerg(3),                     "N/A         (42)
      END of wabasebkpf.


data:  out_vornr like afvc-vornr.
data:  include_rec(1) type c.
data:  wacond(100) type c.

INITIALIZATION.
   move sy-sysid to: phyfile+20(3),
                     phyfile2+20(3),
                     phyfile3+20(3).

*-----------------------------------------------------------------------
* OPEN OUTPUT FILES
*-----------------------------------------------------------------------
start-of-selection.

PERFORM OPEN_OUTPUT_FILE.

perform initialize_wabase.
*-----------------------------------------------------------------------
* 1. SELECT ALL RECORDS FROM BKPF BASED ON THE SELECTION CRITERIA as
*    entered in the variant
* 2.
* 3.
* 4.
*-----------------------------------------------------------------------

SELECT * FROM BKPF
  WHERE BUKRS in S_BUKRS
     AND GJAHR = P_GJAHR
     AND MONAT in s_MONAT
     and usnam in s_usnam
     and blart in s_blart
     and budat in s_budat
     and belnr in s_belnr.

   perform initialize_wabasebkpf.

   if include_rec = 'X'...
      perform bseg_routine.
    endif.

ENDSELECT.                                       "END of BKPF

CLOSE DATASET: PHYFILE, PHYFILE2.
************************************************************************
* Routine to convert the logical file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
************************************************************************
FORM OPEN_OUTPUT_FILE.
    OPEN DATASET: PHYFILE FOR OUTPUT IN TEXT MODE,
                  PHYFILE2 FOR OUTPUT IN TEXT MODE,
                  PHYFILE3 FOR OUTPUT IN TEXT MODE.
    CONCATENATE text-101 text-102 text-103 text-104 text-105  "SDP41636
                text-106 text-107 text-108 text-109 text-110  "SDP41636
                text-111 text-112 text-113 text-114 text-115  "SDP41636
                text-116 text-117 text-118 text-119 text-120  "SDP41636
                text-121 text-122 text-123 text-124 text-125  "SDP41636
                text-126 text-127 text-128 text-129 text-130  "SDP41636
                text-131 text-132 text-133 text-134 text-135  "SDP41636
                text-136 text-137 text-138 text-139 text-140  "SDP41636
                text-141 text-142                             "SDP41636
                INTO record SEPARATED BY ','.
    TRANSFER RECORD to PHYFILE LENGTH RECSIZE."Output Master


    CONCATENATE text-201 text-202 text-203 text-204 text-205  "SDP41636
                text-206 text-207 text-208 text-209           "SDP41636
                into record2 separated by ','.
    transfer record2 to phyfile2 length recsize2.   "Output po header

    CONCATENATE text-301 text-302 text-303 text-304 text-305  "SDP41636
                INTO record3 SEPARATED BY ','.                "SDP41636
    transfer record3 to phyfile3 length recsize3.    "Output docheader


ENDFORM.
*---------------------- MOVE_INFO_BKPF ---------------------------------

*--------------------------- INITIALIZE_WABASE--------------------------
* This routine intializes the fields that will remain static in the
* WAMASTER layout.  These will be moved to WABASEBKPF each time a
* new HEADER record is read.
*-----------------------------------------------------------------------
FORM INITIALIZE_WABASE.
   move 'N/A'    to: wabase-filler1,
                     wabase-filler9,
                     wabase-fillera, wabase-fillerb,
                     wabase-fillerc, wabase-fillerd,
                     wabase-fillere, wabase-fillerf,
                     wabase-fillerg.
   move 'CAN'    to  wabase-cad.
   move p_dtsouc to wabase-datasrce.
* per D. Bossy, column 34 (wabase-filler8) should not have anything in
* it

ENDFORM.

*--------------------------- INITIALIZE_WABASE--------------------------
* This routine intializes the fields that will remain static for the
* header record.  These will be moved to WABASEBSEG each time a
* new HEADER record is read - once for each header.
*-----------------------------------------------------------------------
FORM INITIALIZE_WABASEBKPF.
   clear wabasebkpf.
   refresh wabasebkpf.
   move-corresponding wabase to wabasebkpf.
   select single * from t001
      where bukrs = bkpf-bukrs.
   if sy-subrc = '0'.
      move t001-butxt to wabasebkpf-butxt.
   endif.

   move-corresponding bkpf to wabasebkpf.
   perform format_date using wabasebkpf-bldat.
   perform format_date using wabasebkpf-budat.
   perform vendor_information.
   replace all occurrences of ',' in wabasebkpf-name1 with '_'.
   replace all occurrences of ',' in wabasebkpf-stras with '_'.
   replace all occurrences of ',' in wabasebkpf-bktxt with '_'.
   replace all occurrences of ',' in wabasebkpf-xblnr with '_'.


ENDFORM.

*-------------------------- Vendor Information -------------------------
* The vendor number is on item line 1 in field LIFNR of BSEG.
* Get the first line item of the document and if the vendor number is
* filled in, get the vendor record, containing name, address, etc and
* fill in the relevant fields in wabasebkpf.  The information is the
* same for all line items on the document.
*-----------------------------------------------------------------------
form vendor_information.
  clear include_rec.
  select single * from bseg
    where bukrs = bkpf-bukrs
      and belnr = bkpf-belnr
      and gjahr = bkpf-gjahr
      and augdt in s_augdt
      and buzei = '001'.

    if sy-subrc = '0'.
       move 'X' to include_rec.
       perform doc_routine.    "Document record

       select single * from lfa1
         where lifnr = bseg-lifnr.
       if sy-subrc = '0'.
*         perform po_info_routine.
         move-corresponding lfa1 to wabasebkpf.
         replace all occurrences of  ',' in wabasebkpf-name1 with '_'.
         replace all occurrences of  ',' in wabasebkpf-ort01 with '_'.
       endif.
       move bseg-dmbtr to wabasebkpf-tdmbtr. "Document Total
       move bseg-sgtxt to wabasebkpf-sgtxt.  "Line Item Text 001
       move bseg-zterm to wabasebkpf-zterm.  "Terms of Business
       move bseg-saknr to wabasebkpf-saknr.  "G/L Acct
       move bseg-augdt to wabasebkpf-augdt.  "Clearing Date
       perform format_date using wabasebkpf-augdt.
* This is opposite of what is normal so that the detail amounts
* sum up to the total amount.  (posting key is ignored)
       if bseg-shkzg = 'S'.
          compute wabasebkpf-tdmbtr = wabasebkpf-tdmbtr * -1.
       endif.
       call function 'CLOI_PUT_SIGN_IN_FRONT'
           changing value = wabasebkpf-tdmbtr.




*-------------------------- PAYMETH -----------------------------------
* Field 15 - PAYMETH
* Field 29 - EFT/CHEQUE#
* 1. Based on the first 3 digits of the clearing document number
* 2. EAST is different from WEST

      case p_dtsouc.
        when 'EAST'.
          case bseg-augbl+0(3).
            when '101'.
              move 'REVERSAL' to wabasebkpf-paymeth.
              perform check_payr_routine.
            when '150'.
              move 'CHEQUE' to wabasebkpf-paymeth.
              perform check_payr_routine.
            when '200'.
              move 'CHEQUE' to wabasebkpf-paymeth.
              perform check_payr_routine.
            when '300'.
              move 'EFT'      to wabasebkpf-paymeth.
              move bseg-augbl to wabasebkpf-chect.
            when others.
              move 'XXXX'   to wabasebkpf-paymeth.
          endcase.
        when 'WEST'.
          case bseg-augbl+0(4).
            when '0122'.
              move 'CHEQUE' to wabasebkpf-paymeth.
              perform check_payr_routine.
            when '0123'.
              move 'CHEQUE' to wabasebkpf-paymeth.
              perform check_payr_routine.
            when '0125'.
              write: /1 bkpf-belnr.
              move 'REVERSAL' to wabasebkpf-paymeth.
              perform check_payr_routine.
            when '5000'.
              move 'EFT'    to wabasebkpf-paymeth.
              move bseg-augbl to wabasebkpf-chect.
            when others.
              move 'XXXX'   to wabasebkpf-paymeth.
          endcase.
        when others.
              move 'XXXX'   to wabasebkpf-paymeth.
      endcase.



    endif.

endform.

form bseg_routine.
  clear wamaster.
  move-corresponding wabasebkpf to wamaster.
  select * from bseg
    where bukrs = bkpf-bukrs
      and belnr = bkpf-belnr
      and gjahr = bkpf-gjahr
      and buzei > '001'.



*------------------------------------------------------
*  fields 25 & 26
    if bseg-ebeln <> 0.
       perform po_info_routine.
    endif.
*------------------------------------------------------
     .
     if bseg-ebeln <> ' '.
*        and ( bseg-bschl = '86' or bseg-bschl = '96') .
         perform po_receipts_routine.

     endif.



*
*---------------------------------------------------------
* These fields are in all BSEG records but the information
* needs to come from BSEG Line Item #1 which was preserved
* in the wabasebkpf temporary record
*
    move-corresponding bseg to wamaster.

    move wabasebkpf-zterm to wamaster-zterm.
    move wabasebkpf-lifnr to wamaster-lifnr.
    move wabasebkpf-saknr to wamaster-saknr.
    move wabasebkpf-pname1 to wamaster-pname1.
    move wabasebkpf-xblnr  to wamaster-xblnr.
* when exported to excel, doesn't become an exponential number
    concatenate wamaster-xblnr '_' into wamaster-xblnr.

    move wabasebkpf-augdt  to wamaster-augdt.
*----------------------------------------------------------
    move bseg-dmbtr to wamaster-dmbtr.

    if bseg-shkzg = 'H'.
       compute wamaster-dmbtr = wamaster-dmbtr * -1.
       call function 'CLOI_PUT_SIGN_IN_FRONT'
           changing value = wamaster-dmbtr.
    endif.


*-------------------------- TXT20 -------------------------------------
* Field 20 - G/L Account description

   select single * from skat
      where spras = sy-langu
        and ktopl = p_ktopl
        and saknr = bseg-hkont.
   if sy-subrc = '0'.
      replace all occurrences of  ',' in skat-txt20 with '_'.
      move skat-txt20 to wamaster-txt20.
   else.
      move 'XXXX'     to wamaster-txt20.
   endif.

*------------------------  SGTXT  -------------------------------------

*  Field 21 - SGTXT - Line Item Text
*  1.  Line Item text on line item
*  2.  Line Item text from line item 001
*  3.  If no line item texts, then XXXX

    if wamaster-sgtxt = ' '.   "Populates line 001 sgtxt
       move wabasebkpf-sgtxt to wamaster-sgtxt.
      endif.
    if wamaster-sgtxt = ' '.
       move 'XXXX'           to wamaster-sgtxt.
    endif.
    concatenate bseg-buzei '_' wamaster-sgtxt into wamaster-sgtxt.
    replace all occurrences of  ',' in wamaster-sgtxt with '_'.
    replace all occurrences of  '"' in wamaster-sgtxt with '_'.
*----------------------------------------------------------------
*----------------------- PROJK --------------------------------------
* Field 23 - PROJK - Translate WBS object number to Company WBS number

 case bseg-projk.

     when '00000000'.
       case p_dtsouc.
          when 'EAST'.
             move 'XXXX' to wamaster-posid.
          when 'WEST'.
             case bseg-nplnr.
                when space.
                  case bseg-aufnr.
                    when space.
                      move 'XXXX' to wamaster-posid.
                    when others.
                      select single * from afih
                         where aufnr = bseg-aufnr
                           and ilart = '18'.
                      if sy-subrc  = '0'.
                         move bseg-aufnr to wamaster-posid.
                      else.
                         move 'XXXX' to wamaster-posid.
                      endif.
                   endcase.
                when others.
                  perform get_activity.
                  concatenate bseg-nplnr out_vornr
                     into wamaster-posid separated by '_'.
            endcase.
       endcase.
     when others.
       write bseg-projk to wamaster-posid.
   endcase.



*---------------------- TAXTEXT -------------------------------------

* Field 24 - TAXTEXT - Tax Code + Description

*---------------------------------------------------------------------
    move bseg-mwskz to wamaster-taxtext.
    select single * from t007s
      where spras = sy-langu
        and kalsm = 'TAXCA'
        and mwskz = bseg-mwskz.
    if sy-subrc = 0.
       replace all occurrences of  ',' in t007s-text1 with '_'.
       concatenate bseg-mwskz t007s-text1
         into wamaster-taxtext separated by '_'.
    endif.
*-----------------------------------------------------------------------
*--------------------------- Posting Key -------------------------------
* Field 32  - concatenate posting key (BSEG-BSCHL) & its description
*             LTEXT
*-----------------------------------------------------------------------
    move bseg-bschl to wamaster-ltext.
    select single * from TBSLT
       where spras = sy-langu
         and bschl = bseg-bschl.
    if sy-subrc = '0'.
       concatenate bseg-bschl tbslt-ltext
          into wamaster-ltext separated by '_'.
    endif.
*----------------------------------------------------------------------
    if wamaster-ebeln = ' '.                                  "SDP41651
       wamaster-ebelp   = 0.                                  "SDP41651
       wamaster-matkl   = ' '.                                "SDP41651
       wamaster-matdesc = ' '.                                "SDP41651
    else.                                                     "SDP41651
       wamaster-ebelp   = wapo-ebelp.                         "SDP41651
       wamaster-matkl   = wapo-matkl.                         "SDP41651
       wamaster-matdesc = wapo-matdesc.                       "SDP41651
    endif.                                                    "SDP41651

    concatenate wamaster-butxt     wamaster-belnr
                wamaster-lifnr     wamaster-ktokk
                wamaster-name1     wamaster-land1
                wamaster-stras     wamaster-ort01
                wamaster-regio     wamaster-pstlz
                wamaster-filler1   wamaster-budat
                wamaster-tdmbtr    wamaster-augdt
                wamaster-paymeth   wamaster-xblnr
                wamaster-bldat     wamaster-saknr
                wamaster-hkont     wamaster-txt20
                wamaster-sgtxt     wamaster-dmbtr
                wamaster-posid     wamaster-taxtext
                wamaster-pname1    wamaster-port01
                wamaster-ebeln     wamaster-zterm
                wamaster-chect     wamaster-cad
                wamaster-datasrce  wamaster-ltext
                wamaster-bktxt     wamaster-filler8
                wamaster-ebelp     wamaster-matkl             "SDP41651
                wamaster-matdesc   wamaster-fillerc           "SDP41651
                wamaster-fillerd   wamaster-fillere
                wamaster-fillerf   wamaster-fillerg
                  into record separated by ','.

      TRANSFER RECORD to PHYFILE LENGTH RECSIZE."Output Master


  endselect.



endform.

form doc_routine.
   clear wadoc.
   move-corresponding bkpf to wadoc.
   move 'N/A' to: wadoc-filler1, wadoc-filler2.

*------------------------- BLART --------------------------
* Field 2 - BLART - Code & Description
*----------------------------------------------------------
   move bkpf-blart to wadoc-blart.
   select single * from t003t
     where spras = sy-langu
       and blart = bkpf-blart.
   if sy-subrc = '0'.
     concatenate bkpf-blart t003t-ltext into wadoc-blart
       separated by '_'.
   endif.
*----------------------------------------------------------

   concatenate  wadoc-belnr    wadoc-blart
                wadoc-usnam    wadoc-filler1
                wadoc-filler2
                   into record3 separated by ','.

   transfer record3 to phyfile3 length recsize3. "Output DocHeader


endform.

form po_info_routine.
   move 'XXXX' to wamaster-port01.
   select single * from ekpo
        where ebeln = bseg-ebeln
          and werks <> space.
     select single * from t001W
       where werks = ekpo-werks.
     if sy-subrc = '0'.
       concatenate bseg-werks t001w-name1
          into wabasebkpf-pname1 separated by '_'.
       move t001w-ort01 to wamaster-port01.
     endif.

     select single * from sadr
         where adrnr = ekpo-adrnr.
     if sy-subrc = '0'.
        move sadr-ort01 to wamaster-port01.
     endif.
     replace all occurrences of ',' in wamaster-port01 with '_'.

endform.


form po_receipts_routine.
  clear wapo.
  move 'N/A' to:  wapo-filler1, wapo-filler2, wapo-filler3.
  move bseg-ebeln          to wapo-ebeln.
  move bseg-ebelp          to wapo-ebelp.
  select single * from ekko
     where ebeln = bseg-ebeln.

  if sy-subrc = '0'.
     move ekko-bsart to wapo-batxt.

     select single * from t161t
       where spras = sy-langu
         and bsart = ekko-bsart
         and bstyp = ekko-bstyp.
     if sy-subrc = '0'.
        concatenate ekko-bsart t161t-batxt
          into wapo-batxt separated by '_'.
     endif.

     select * from ekpo
       where ebeln = bseg-ebeln
         and ebelp = bseg-ebelp..

      concatenate ekpo-matnr+12(6) ekpo-txz01
          into wapo-matdesc separated by '_'.
      replace all occurrences of ',' in wapo-matdesc
                     with  '_'.
      replace all occurrences of '"' in wapo-matdesc
                     with '_'.
      move ekpo-matkl  to wapo-matkl.

      select single * from t023t
         where spras = sy-langu
           and matkl = ekpo-matkl.
      if sy-subrc = '0'.
        concatenate ekpo-matkl t023t-wgbez into wapo-matkl
          separated by '_'.
      endif.


*----------------------------------------------------------
* Goods receipt date
*
    if bseg-bschl = '86' or bseg-bschl = '96'.

       move 'menge = bseg-menge and xblnr = wabasebkpf-xblnr' to wacond.
       perform read_ekbe.                     "exact match on quantity
       if sy-subrc = '0'.
*           write: /1 '2goodsreceipt exact match on quantity'.
           move ekbe-budat to wapo-grdate.
           perform write_po_record.
        else.

        move 'dmbtr = bseg-dmbtr and xblnr = wabasebkpf-xblnr' to wacond
.
        perform read_ekbe.                  "exact match on amount
        if sy-subrc = '0'.
*           write: /1 '2agoodsreceipt exact match on amount'.
           move ekbe-budat to wapo-grdate.
           perform write_po_record.
        else.

        move 'menge = bseg-menge'  to wacond.
        perform read_ekbe.      "generic match on quantity
        if sy-subrc = '0'.
*           write: /1 '3goodsreceipt generic match on quantity'.
           move ekbe-budat to wapo-grdate.
           perform write_po_record.
        else.

        move 'dmbtr = bseg-dmbtr' to wacond.    "generic match on amount
        perform read_ekbe.
        if sy-subrc = '0'.
*           write: /1 '4goods receipt generic match on amount'.
           move ekbe-budat to wapo-grdate.
           perform write_po_record.
        else.

*          write: /1 '5goodsrecipt no match to po'.
           move  '99991231'       to wapo-grdate.   "po but no match
           perform write_po_record.


        endif. endif. endif. endif.
  else.
*       write: /1 '6goodsreceipt - bschl neither 86 nor 96'.
        move  'XXXXXXXX'       to wapo-grdate.
        perform write_po_record.

  endif.
  endselect.
  endif.
endform.

form get_activity..

  CALL FUNCTION 'READ_NETWORK_NPLNR_VORNR'
       EXPORTING
            APLZL     = bseg-APLZL
            AUFPL     = bseg-AUFPL
       IMPORTING
            VORNR     = OUT_VORNR
       EXCEPTIONS
            NOT_FOUND = 01.

  if sy-subrc = '0'.
  else.
     move 'XXXX' to out_vornr.
  endif.

endform.

form check_payr_routine.
   .
   select single * from PAYR
      where zbukr = bseg-bukrs
        and gjahr = bseg-gjahr
        and vblnr = bseg-augbl.
   if sy-subrc = '0'.
      move payr-chect to wabasebkpf-chect.
   else.
      move 'XXXX' to wabasebkpf-chect.
   endif.
endform.

form format_date changing date.
                 .

data:  indate(10)  type c.
data:  outdate(10) type c.

*  clear indate.
  move date to indate.
  move 'mm/dd/yyyy' to outdate.
  move indate+0(4) to outdate+6(4).
  move indate+4(2) to outdate+0(2).
  move indate+6(2) to outdate+3(2).
  move outdate to date.
endform.

form read_ekbe.
     select single * from ekbe
        where ebeln = bseg-ebeln
        and ebelp = bseg-ebelp
        and vgabe = '1'
        and (wacond).
endform.

form write_po_record.
    perform format_date changing wapo-grdate.
    concatenate wapo-ebeln    wapo-grdate
                wapo-ebelp    wapo-matkl
                wapo-batxt    wapo-matdesc
                wapo-filler1  wapo-filler2
                wapo-filler3
                         into record2 separated by ','.
    transfer record2 to phyfile2 length recsize2. "PO Info
endform.
