REPORT ZFAPI030 MESSAGE-ID ZS line-count 65 line-size 132.
************************************************************************
*  Programmer: MaryLou De Meester
*
*  Brief Description:
*     - This ABAP is a program to extract data from 5 tables - BSAK
*       BSIS & BSAS, LFA1 and SKAT.
************************************************************************
* ---------------------- CHANGE LOG ------------------------------------
*
*
* 09/20/2004 mdemeest #1207 - New abap
*
************************************************************************
TABLES: BSIS,   "Open G/L Line Items
        BSAS,   "Cleared G/L Line Items
        BSAK,   "Vendor Line Items - Cleared
        LFA1,   "Vendor
        SKAT.   "Chart of Accounts

*=======================================================================
* SELECTION SCREEN
*=======================================================================
selection-screen   begin of block a with frame title text-001.
parameter:  p_ktopl  like skat-ktopl,              "Chart of Accounts
            p_skatfl like filename-fileextern obligatory
              default '/usr/sap/interfaces/C11/OUTFILES/skat.csv'.
selection-screen   end of block a.
selection-screen skip 1.
selection-screen  begin of block b with frame title text-002.
PARAMETERS: P_BUKRS  LIKE BSAk-BUKRS,              "Company Code
            P_gjahr  like bsak-gjahr.              "Fiscal year
select-options:
            s_monat  for bsak-monat obligatory,    "Period
            s_blart  for bsak-blart.              "Document Type
selection-screen   end of block b.
selection-screen   skip 1.
selection-screen   begin of block  c with frame title text-003.
parameter: p_bsakfl  like filename-fileextern obligatory
             default '/usr/sap/interfaces/C11/OUTFILES/bsak.csv',
           p_bsisfl  like filename-fileextern obligatory
             default '/usr/sap/interfaces/C11/OUTFILES/bsis.csv',
           p_bsasfl  like filename-fileextern obligatory
             default '/usr/sap/interfaces/C11/OUTFILES/bsas.csv',
           p_lfa1fl  like filename-fileextern obligatory
             default '/usr/sap/interfaces/C11/OUTFILES/lfa1.csv'.
selection-screen   end of block c.

data: msg(100).
data: initial_bsak(82)        type c.
data: begin of out_bsak        occurs 0,    "Total Length 82
        bukrs         like bsak-bukrs,   "4
        del1(1) type c,
        lifnr         like bsak-lifnr,   "10
        del2(1) type c,
        augbl         like bsak-augbl,   "10
        del3(1) type c,
        gjahr         like bsak-gjahr,   "4
        del4(1) type c,
        belnr         like bsak-belnr,   "10
        del5(1) type c,
        xblnr         like bsak-xblnr,   "16
        del6(1) type c,
        blart         like bsak-blart,   "2
        del7(1) type c,
        mwskz         like bsak-mwskz,   "2
        del8(1) type c,
        dmbtr(16) type c,         "like bsak-dmbtr,   "16
        end of out_bsak.

*----------------------- Open G/L Items  -------------------------------
data: initial_bsis(185)  type c.
data: begin of out_bsis        occurs 0,    "Total Length 177
        bukrs         like bsis-bukrs,    "4
        del1(1) type c,
        hkont         like bsis-hkont,    "10
        del2(1) type c,
        belnr         like bsis-belnr,    "10
        del3(1) type c,
        blart         like bsis-blart,    "2
        del4(1) type c,
        shkzg         like bsis-shkzg,    "1
        del5(1) type c,
        mwskz         like bsak-mwskz,    "2
        del6(1)   type c,
        dmbtr(16) type c,                "16    like bsak-dmbtr,
        del7(1)   type c,
        wrbtr(16) type c,                "16    like bsis-wrbtr,
        del8(1)   type c,
        mwsts(16) type c,                "16    like bsis-mwsts,
        del9(1)   type c,
        sgtxt         like bsis-sgtxt,   "50
        dela(1) type c,
        kostl         like bsis-kostl,   "10
        delb(1) type c,
        projk(24) type c,                "24 like bsis-projk,
        delc(1) type c,
        aufnr         like bsis-aufnr,   "12
      end of out_bsis.

*----------------------- Cleared G/L Items  ----------------------------
data: initial_bsas(185) type c.
data: begin of out_bsas       occurs 0,
        bukrs         like bsas-bukrs,
        del1(1) type c,
        hkont         like bsas-hkont,
        del2(1) type c,
        belnr         like bsas-belnr,
        del3(1) type c,
        blart         like bsas-blart,
        del4(1) type c,
        shkzg         like bsas-shkzg,
        del5(1) type c,
        mwskz         like bsas-mwskz,
        del6(1)   type c,
        dmbtr(16) type c,                    "like bsas-dmbtr,
        del7(1)   type c,
        wrbtr(16) type c,                    "like bsas-wrbtr,
        del8(1)   type c,
        mwsts(16) type c,                    "like bsas-mwsts,
        del9(1)   type c,
        sgtxt         like bsas-sgtxt,
        dela(1) type c,
        kostl         like bsas-kostl,
        delb(1) type c,
        projk(24) type c,                    "24 like bsis-projk,
        delc(1) type c,
        aufnr         like bsis-aufnr,

      end of out_bsas.

data: initial_skat(109) type c.
data: begin of out_skat          occurs 0,
        ktopl         like skat-ktopl,  "4
        del1(1) type c,
        saknr         like skat-saknr,  "10
        del2(1) type c,
        txt20         like skat-txt20,  "20
        del3(1) type c,
        txt50         like skat-txt50,  "50
        del4(1) type c,
        mcod1         like skat-mcod1,  "25
      end of out_skat.

data:  initial_lfa1(92) type c.
data:  begin of out_lfa1          occurs 0,
        lifnr         like lfa1-lifnr,          "10  Vendor Number
        del1(1) type c,
        name1         like lfa1-name1,          "35  Vendor Name
        del2(1) type c,
        ort01         like lfa1-ort01,          "35  Vendor City
        del3(1) type c,
        regio         like lfa1-regio,          "3 Vendor Province/State
        del4(1) type c,
        land1         like lfa1-land1,          "3   Vendor Country
       end of out_lfa1.



*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================
*AT SELECTION-SCREEN.
*  PERFORM CHECK_FILE.

*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.
  open dataset p_bsakfl for output in text mode message msg.
  perform select_from_bsak.
  close dataset p_bsakfl.

  open dataset p_bsisfl for output in text mode message msg.
  perform select_from_bsis.
  close dataset p_bsisfl.

  open dataset p_bsasfl for output in text mode message msg.
  perform select_from_bsas.
  close dataset p_bsasfl.

  open dataset p_lfa1fl for output in text mode message msg.
  perform select_from_lfa1.
  close dataset p_lfa1fl.

  open dataset p_skatfl for output in text mode message msg.
  perform select_from_skat.
  close dataset p_skatfl.


*------------------------  SELECT_FROM_BSAK  ---------------------------
*  Total length of file = 82 bytes including the commas
*-----------------------------------------------------------------------
FORM select_from_bsak.
  clear out_bsak.
  move ',' to:  out_bsak-del1, out_bsak-del2, out_bsak-del3,
                out_bsak-del4, out_bsak-del5, out_bsak-del6,
                out_bsak-del7, out_bsak-del8.
  move out_bsak  to initial_bsak.

  select * from bsak
     where bukrs  = p_bukrs
       and gjahr  = p_gjahr
       and monat in s_monat
       and blart in s_blart.                   .
  move initial_bsak to out_bsak.
  move-corresponding bsak to out_bsak.
  transfer out_bsak to p_bsakfl length 82.
  endselect.

endform.
*-----------------------------------------------------------------------

form select_from_bsis.
  clear out_bsis.
  move ',' to: out_bsis-del1, out_bsis-del2, out_bsis-del3,
               out_bsis-del4, out_bsis-del5, out_bsis-del6,
               out_bsis-del7, out_bsis-del8, out_bsis-del9,
               out_bsis-dela, out_bsis-delb, out_bsis-delc.
  move out_bsis  to initial_bsis.

  select * from bsis
     where bukrs = p_bukrs
       and gjahr = p_gjahr
       and monat in s_monat
       and blart in s_blart.
  move initial_bsis       to out_bsis.           .
  move-corresponding bsis to out_bsis.
  translate out_bsis-sgtxt using ', '.
  WRITE BSIS-PROJK TO OUT_BSIS-PROJK                                   .

  transfer out_bsis to p_bsisfl length 185.

  endselect.

endform.
*-----------------------------------------------------------------------

form select_from_bsas.
  clear out_bsas.
  move ',' to:  out_bsas-del1, out_bsas-del2, out_bsas-del3,
                out_bsas-del4, out_bsas-del5, out_bsas-del6,
                out_bsas-del7, out_bsas-del8, out_bsas-del9,
                out_bsas-dela, out_bsas-delb, out_bsas-delc.
  move out_bsas to initial_bsas.
  select * from bsas
     where bukrs = p_bukrs
       and gjahr = p_gjahr
       and monat in s_Monat
       and blart in s_blart.
  move initial_bsas       to out_bsas.            .
  move-corresponding bsas to out_bsas.
  translate out_bsas-sgtxt using ', '.

  transfer out_bsas to p_bsasfl length 185.

  endselect.

endform.
*-----------------------------------------------------------------------

form select_from_lfa1.
* initialize the temporary file once and then refresh the out_lfa1 table
    clear out_lfa1.
    move ',' to:  out_lfa1-del1, out_lfa1-del2,
                  out_lfa1-del3, out_lfa1-del4.
    move out_lfa1 to initial_lfa1.

*-----------------------------------------------------------------------
* get all vendor records from LFA1.
    select * from lfa1.   " where lifnr = p_lifnr.
      move initial_lfa1 to out_lfa1.
      move-corresponding lfa1 to out_lfa1.
      translate out_lfa1-ort01 using ', '.
      translate out_lfa1-name1 using ', '.
      translate out_bsas-sgtxt using ', '.
      transfer out_lfa1 to p_lfa1fl length 92.
    endselect.

endform.
*------------------------ SELECT_FROM_SKAT  ----------------------------
form select_from_skat.
    clear out_skat.
    move ',' to:  out_skat-del1, out_skat-del2,
                  out_skat-del3, out_skat-del4.
    move out_skat to initial_skat.
    select * from skat
        where spras = 'EN'
          and ktopl = p_ktopl.
      move initial_skat          to out_skat.
      move-corresponding skat to out_skat.
      transfer out_skat to p_skatfl length 109.
    endselect.

endform.
