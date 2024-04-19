REPORT ZfAPR006 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
      MESSAGE-ID U2.
************************************************************************
*  Author:      M DeMeester
*  Description: Accepts info in variants and produces a letter filling
*               in info in script for wire transfers
*               Uses form Z_WIRE_TRANS

************************************************************************
* 2001/04/12 mdemeest #--- Original program
************************************************************************
tables: t001,                 "Company code table
        t012,
        t012k,                "House Bank Accounts
        bnka,                 "Bank Master Record
        tcurt,                "Currency code table
        t015m.                "Month name table

data:   p_amt_ch(13).
data:   valdt_nam(10).
*----------------------  SELECTION SCREEN  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-999.
SELECTION-SCREEN SKIP 1.
parameters:
            p_bukrs    like T001-bukrs  obligatory,        "Company Code
            p_hbkid    like t012-hbkid  obligatory,        "To Bank
            p_waers    like tcurt-waers obligatory,        "Currency
            p_fax      like lfb1-tlfxs.                    "Fax
selection-screen skip 1.
parameters:
            p_amt      like bsak-wrbtr  obligatory,        "Amount
            p_valdt    like sy-datum default sy-datum obligatory.
selection-screen skip 1.
parameters:
            p_inst1(70) type c,                           "Payment Inst
            p_inst2(70) type c,                           "Payment Inst
            p_inst3(70) type c.                           "Payment Inst
selection-screen skip 1.
parameters:
            p_benef(50)    type c.                        "Beneficiary
selection-screen skip 1.
parameters:
            p_dtl1(70)  type c,                           "Pay Details
            p_dtl2(70)  type c,                           "Pay Details
            p_dtl3(70)  type c.                           "Pay Details
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.

data: begin of wa              occurs 1,
       transit   like t012-bankl,                          "Transit #
       name1(45) type c,                                   "Attention to
       banka     like bnka-banka,                          "Bank Name
       stras     like bnka-stras,                          "Bank Addr
       ort01     like bnka-ort01,                          "Bank City
       bankn     like t012k-bankn,                         "Bank Acct #
      end of wa.

*-------------------  TOP-OF-PAGE  -------------------------------------
* Report Title & Column Headings
*-----------------------------------------------------------------------
TOP-OF-PAGE.
*-------------------------  INITIALIZATION  ----------------------------
*  Check to ensure person accessing program is allowed to
*-----------------------------------------------------------------------
INITIALIZATION.
*ALL FUNCTION 'AUTHORITY_CHECK_PROG'.
*f sy-subrc <> '0'.
*  write: / 'You do not have the authority to execute ZFAPR006'.
*ndif.

*---------------------- START-of-SELECTION -----------------------------
START-OF-SELECTION.

clear wa.

select single * from T001           "Company code translation
  where bukrs = p_bukrs.

select single * from t012
  where bukrs = p_bukrs
    and hbkid = p_hbkid.
if sy-subrc = '0'.
    move t012-bankl to wa-transit.
    if t012-name1 <> space.                         "If addressee exists
       concatenate 'ATTENTION:' t012-name1 into wa-name1
                                     separated by space.
    endif.
endif.

select single * from bnka           "Bank Master Record for name & addr
    where banks = t012-banks
      and bankl = t012-bankl.
if sy-subrc = '0'.
    move bnka-banka to wa-banka.
    move bnka-stras to wa-stras.
    move bnka-ort01 to wa-ort01.
endif.

select single * from tcurt          "Currency translation
  where spras = sy-langu
    and waers = p_waers.

select single * from t012k          "Get Bank Account number
  where bukrs = p_bukrs
    and hbkid = p_hbkid
    and hktid = p_waers.
if sy-subrc = '0'.
   move t012k-bankn to wa-bankn.
endif.

move p_amt to p_amt_ch.
shift p_amt_ch left deleting leading ' '.

select single * from t015m
  where spras = sy-langu
    and monum = p_valdt+4(2).
move t015m-monam to valdt_nam.

perform open_form.
perform write_form.
perform close_form.

*--------------------------- SUBROUTINES -------------------------------
*
*---------------------------- OPEN_FORM --------------------------------
form open_form.
  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
             FORM    = 'Z_WIRE_TRANS'
             LANGU   = sy-langu
       exceptions
             FORM    = 1.
endform.

Form write_form.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
             WINDOW = 'MAIN'


       EXCEPTIONS
             WINDOW = 1
             ELEMENT = 2.

endform.

Form close_form.
  CALL FUNCTION 'CLOSE_FORM'.


endform.
