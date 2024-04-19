REPORT ZFFII043 MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-SIZE 255.
*
*  THIS ABAP IS USED IN BOTH THE EAST AND THE WEST.  PLEASE MAKE SAME
*  CHANGES TO BOTH
*
************************************************************************
* 2008/06/11 mdemeest TR342 Multiple company code selection
* 2003/06/04 mdemeest 1019 Eliminate recurring entries template
* 2003/05/23 mdemeest 1019 Copied from EAST SAP and removed selections
*                          from variant S_MATNR, S_AUFNR and S_KOSTL and
*                          S_HKONT
************************************************************************
TABLES: DD03l,              "Table/Fields
        DD04T,              "Headers/descriptions of fields
        BKPF, BSEG,
        LFA1,               "Vendor Name
        LFB1,
        skat,               "Account Description
        MAKT.               "Material Description

*----------------------------------------------------------------------
* BKPF Information
*----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX10 WITH FRAME TITLE TEXT-100.
select-options: s_fld1 for dd03l-fieldname   no intervals..
PARAMETER: PHYFILE LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/BKPF.CSV'.
PARAMETER: RECSIZE TYPE I DEFAULT 118.

selection-screen begin of block box11 with frame title text-101.

parameter:      p_where1(75) type c default 'BUKRS in s_value1',
                p_where2(75) type c default 'AND GJAHR in s_value2',
                p_where3(75) type c,
                p_where4(75) type c,
                p_where5(75) type c.

select-options: s_value1 for bkpf-xblnr,
                s_value2 for bkpf-xblnr,
                s_value3 for bkpf-xblnr,
                s_value4 for bkpf-xblnr,
                s_value5 for bkpf-xblnr.

selection-screen end of block box11.
SELECTION-SCREEN END OF BLOCK BOX10.

*----------------------------------------------------------------------
SKIP 1.
*----------------------------------------------------------------------
* BSEG Information
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX20 WITH FRAME TITLE TEXT-200.
select-options: s_fld2 for dd03l-fieldname   no intervals..
PARAMETER: PHYFILE2 LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/BSEG.CSV'.
PARAMETER: RECSIZE2 TYPE I DEFAULT 205.

selection-screen begin of block box21 with frame title text-201.

parameter:      p_where6(75) type c default 'MONAT in s_value6',
                p_where7(75) type c default 'AND BLART in s_value7',
                p_where8(75) type c,
                p_where9(75) type c.


select-options: s_value6  for bseg-sgtxt,
                s_value7  for bseg-sgtxt,
                s_value8  for bseg-sgtxt,
                s_value9  for bseg-sgtxt.


selection-screen end of block box21.
SELECTION-SCREEN END OF BLOCK BOX20.
*-----------------------------------------------------------------------



SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-001.
select-options: s_lifnr for bseg-lifnr.
Parameter:      P_ktopl like skat-ktopl default 'COAT',
                p_desc  as checkbox default 'x',
                p_desc2 as checkbox default 'x'.

SELECTION-SCREEN END OF BLOCK BOX3.

*--------------------------------------------------------------
* Variables for BKPF file
*--------------------------------------------------------------

data: wa_condtabl(75) type c occurs 0.
data: wadisp(3) type c.
data: tmpfile(1000) type c.


data: begin of wadd03l  occurs 0,
      fieldname like dd03l-fieldname,
      reqfield(20) type c,
      leng      like dd03l-leng,
      decimals  like dd03l-decimals,
      displ(5)  type c,
      rollname  like dd03l-rollname,
      scrtext   like dd04t-scrtext_s,
      end of wadd03l.

*--------------------------------------------------------------
* Variables for BSEG file
*--------------------------------------------------------------

data: wa_condtabl2(75) type c occurs 0.
data: wadisp2(4) type c.
data: tmpfile2(2500) type c.
data: wamaktx like makt-maktx.
data: watxt20 like skat-txt20.

data: begin of wadd03l2  occurs 0,
      fieldname like dd03l-fieldname,
      reqfield(20) type c,
      leng      like dd03l-leng,
      decimals  like dd03l-decimals,
      displ(5)  type c,
      rollname  like dd03l-rollname,
      scrtext   like dd04t-scrtext_s,
      end of wadd03l2.



*-----------------------------------------------------------------------

data: wabkpflength(5) type c.
data: wabseglength(5) type c.


data: wabkpf like BKPF occurs 0
             with header line.
data: wabseg like BSEG occurs 0
             with header line.


initialization.
  move sy-sysid to:  phyfile+20(3), phyfile2+20(3).


start-of-selection.

PERFORM PRINT_VARIANT.

new-page.

*----------------------------------------------------------------------
* this routine takes BKPF fields are determines length and displacement
* in output file.
*----------------------------------------------------------------------
loop at s_fld1.
  move s_fld1+3(10) to wadd03l-fieldname.
  append wadd03l.
endloop.


loop at wadd03l.
  select * from dd03l
     where tabname = 'BKPF'
       and fieldname = wadd03l-fieldname.
   move-corresponding dd03l to wadd03l.
   select single * from dd04t
      where rollname = wadd03l-rollname
        and ddlanguage = sy-langu.
     if sy-subrc = '0'.
        move dd04t-scrtext_s to wadd03l-scrtext.
     endif.
   modify wadd03l.
  endselect.
endloop.



move 0 to wadisp.
loop at wadd03l.
  move wadisp to wadd03l-displ.
  if phyfile cs '.csv'.
    add 1            to wadisp.
  endif.
  add wadd03l-leng to wadisp.
  if wadd03l-decimals > 0.     "Allows for explicit decimal point.
     add 1         to wadisp.
  endif.
  modify wadd03l.
  "calculate total length of BKPF file
  wabkpflength = wadd03l-displ + wadd03l-leng + 1.
endloop.

*----------------------------------------------------------------------
* write fields, lengths and displacements for files
*----------------------------------------------------------------------
write: /1 'FIELDS, LENGTHS & DISPLACEMENTS for TABLE ** BKPF ** '.
loop at wadd03l.
  write: /1 wadd03l-fieldname(8), ' ', wadd03l-leng(6), ' ',
wadd03l-displ, ' ', wadd03l-scrtext.
endloop.
skip 1.
write: /1 'TOTAL LENGTH of BKPF FILE = ', wabkpflength.
skip 5.

*----------------------------------------------------------------------
* this routine takes BSEG fields are determines length and displacement
* in output file (BSEG).
*----------------------------------------------------------------------
loop at s_fld2.
  move s_fld2+3(10) to wadd03l2-fieldname.
  append wadd03l2.
  if p_desc = 'x' and s_fld2+3(10) = 'MATNR'.
     move 'MAKTX' to wadd03l2-fieldname.
     append wadd03l2.
  endif.
  if p_desc2 = 'x' and s_fld2+3(10) = 'SAKNR'.
     move 'TXT20' to wadd03l2-fieldname.
     append wadd03l2.
  endif.

endloop.

data:  watable  like bbseg-tbnam.

loop at wadd03l2.
  case wadd03l2-fieldname."determine which table name is used in select
    when 'MAKTX'.
       move 'MAKT' to watable.
    when 'TXT20'.
       move 'SKAT' to watable.
    when others.
       move 'BSEG' to watable.
  endcase.

  select * from dd03l
     where tabname = watable
       and fieldname = wadd03l2-fieldname.
   move-corresponding dd03l to wadd03l2.
   select single * from dd04t
      where rollname = wadd03l2-rollname
        and ddlanguage = sy-langu.
      if sy-subrc = '0'.
        move dd04t-scrtext_s to wadd03l2-scrtext.
     endif.
   modify wadd03l2.
  endselect.

endloop.



move 0 to wadisp2.
loop at wadd03l2.
  move wadisp2 to wadd03l2-displ.
  if phyfile2 cs '.csv'.
     add 1            to wadisp2.
  endif.
  add wadd03l2-leng to wadisp2.
  if wadd03l2-decimals > 0.     "Allows for explicit decimal point.
     add 1         to wadisp2.
  endif.
  modify wadd03l2.
  "calculate total length of BSEG file
  wabseglength = wadd03l2-displ + wadd03l2-leng + 1.
endloop.

*----------------------------------------------------------------------
* write fields, lengths and displacements for files
*----------------------------------------------------------------------
write: /1 'FIELDS, LENGTHS & DISPLACEMENTS for TABLE ** BSEG ** '.
loop at wadd03l2.
  write: /1 wadd03l2-fieldname(8), ' ', wadd03l2-leng(6), ' ',
wadd03l2-displ, wadd03l2-scrtext.
endloop.
skip 1.
write: /1 'TOTAL LENGTH of BSEG FILE = ', wabseglength.

*----------------------------------------------------------------------
* BKPF conditions for SELECT statement
*----------------------------------------------------------------------

clear wa_condtabl.
append p_where1 to wa_condtabl.
append p_where2 to wa_condtabl.
append p_where3 to wa_condtabl.
append p_where4 to wa_condtabl.
append p_where5 to wa_condtabl.

*-----------------------------------------------------------------------
* BSEG conditions for SELECT statement
*-----------------------------------------------------------------------
clear wa_condtabl2.
append p_where6 to wa_condtabl2.
append p_where7 to wa_condtabl2.
append p_where8 to wa_condtabl2.
append p_where9 to wa_condtabl2.


perform open_output_files.

  clear: tmpfile, tmpfile2.

  if phyfile cs '.csv'.
     translate tmpfile using ' ,'.
  endif.

  if phyfile2 cs '.csv'.
     translate tmpfile2 using ' ,'.
  endif.


*-----------------------------------------------------------------
*  This routine processes all the BKPF table information.

* select all header records from bkpf based on selection parameters.

SELECT * FROM BKPF into table wabkpf
   where (wa_condtabl).

loop at wabkpf.
   perform move_bkpf_to_outrec.
endloop.



*----------------------  ROUTINES --------------------------------------


*-------------------------  OPEN_OUTPUT_FILES -------------------------*
*   This routine opens both files and writes out the column headers as
*   entered in the variant.
*----------------------------------------------------------------------*
FORM OPEN_OUTPUT_FILES.

  OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
  OPEN DATASET PHYFILE2 FOR OUTPUT IN TEXT MODE.

* Column headers
  clear: tmpfile, tmpfile2.

  loop at wadd03l.
     concatenate: tmpfile wadd03l-fieldname  ',' into tmpfile.
  endloop.
  transfer tmpfile to phyfile length recsize.

  loop at wadd03l2.
     concatenate: tmpfile2 wadd03l2-fieldname  ',' into tmpfile2.
  endloop.
  transfer tmpfile2 to phyfile2 length recsize2.

ENDFORM.

*---------------------------- MOVE_BKPF_TO_OUTREC ----------------------
* This routine move the requested BKPF fields to the output area in the
* same order as entered in the variant
*-----------------------------------------------------------------------

form move_bkpf_to_outrec.
    loop at wadd03l.
      case wadd03l-fieldname.
         when 'MANDT'.
           move wabkpf-mandt to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'BUKRS'.
           move wabkpf-bukrs to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'BELNR'.
           move wabkpf-belnr to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'GJAHR'.
           move wabkpf-gjahr to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'BLART'.
           move wabkpf-blart to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'BLDAT'.
           move wabkpf-bldat to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'BUDAT'.
           move wabkpf-budat to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'MONAT'.
           move wabkpf-monat to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'CPUDT'.
           move wabkpf-cpudt to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'CPUTM'.
           move wabkpf-cputm to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'AEDAT'.
           move wabkpf-aedat to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'UPDDT'.
           move wabkpf-upddt to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'WWERT'.
           move wabkpf-wwert to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'USNAM'.
           move wabkpf-usnam to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'TCODE'.
           move wabkpf-tcode to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'BVORG'.
           move wabkpf-bvorg to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'XBLNR'.
           translate: wabkpf-xblnr using ',_'.
           move wabkpf-xblnr to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'DBBLG'.
           move wabkpf-dbblg to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'STBLG'.
           move wabkpf-stblg to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'STJAH'.
           move wabkpf-stjah to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'BKTXT'.
           translate: wabkpf-bktxt using '"_'.
           move wabkpf-bktxt to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'WAERS'.
           move wabkpf-waers to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'KURSF'.
           move wabkpf-kursf to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'KZWRS'.
           move wabkpf-kzwrs to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'KZKRS'.
           move wabkpf-kzkrs to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'BUDAT'.
           move wabkpf-budat to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'BSTAT'.
           move wabkpf-bstat to tmpfile+wadd03l-displ(wadd03l-leng).
         when 'HWAER'.
           move wabkpf-hwaer to tmpfile+wadd03l-displ(wadd03l-leng).
* add additional BKPF fields here
         when others.
           skip 3.
           write: /1 wadd03l-fieldname(8), ' not in BKPF extract.'.
           write: /1 'If field required, modify ', sy-repid.
         endcase.
      endloop.   "end of wadd03l



      perform get_matching_bseg.

   endform.

*-----------------------------  GET_MATCHING_BSEG ----------------------
* This routine retrieves all BSEG records that match the key of the BKPF
* file restricted by the BSEG selection conditions as entered in the
* variant.
*-----------------------------------------------------------------------

form get_matching_bseg.
  refresh wabseg.
  select single * from bseg into wabseg  "This routine determines if
     where bukrs = wabkpf-bukrs          "document is in vendor
       and belnr = wabkpf-belnr          "selection criteria
       and gjahr = wabkpf-gjahr
       and lifnr in s_lifnr.

  if sy-subrc = '0'.
     select * from bseg into wabseg
        where bukrs = wabkpf-bukrs
          and belnr = wabkpf-belnr
          and gjahr = wabkpf-gjahr.
          perform move_bseg_to_outrec2.
          transfer tmpfile to phyfile length recsize.
     endselect.
  endif.

endform.

*-------------------------------- MOVE_BSEG_TO_OUTREC2 -----------------
* This routine moves the requested BSEG fields to the output record in
* the same order as listed in the variant.
*-----------------------------------------------------------------------

form move_bseg_to_outrec2.
   loop at wadd03l2.
      case wadd03l2-fieldname.
         when 'MANDT'.
           move wabseg-mandt to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'BUKRS'.
           move wabseg-bukrs to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'BELNR'.
           move wabseg-belnr to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'GJAHR'.
           move wabseg-gjahr to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'BUZEI'.
           move wabseg-buzei to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'BSCHL'.
           move wabseg-bschl to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'KOART'.
           move wabseg-koart to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'SHKZG'.
           move wabseg-shkzg to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'GSBER'.
           move wabseg-gsber to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'MWSKZ'.
           move wabseg-mwskz to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'DMBTR'.
           move wabseg-dmbtr to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'WRBTR'.
           move wabseg-wrbtr to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'MWART'.
           move wabseg-mwart to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'ZUONR'.
           move wabseg-zuonr to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'SGTXT'.
           translate wabseg-sgtxt using ',_'.
           move wabseg-sgtxt to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'BEWAR'.
           move wabseg-bewar to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'VORGN'.
           move wabseg-vorgn to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'KOSTL'.
           move wabseg-kostl to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'VBELN'.
           move wabseg-vbeln to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'VBEL2'.
           move wabseg-vbel2 to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'SAKNR'.
           move wabseg-saknr to tmpfile2+wadd03l2-displ(wadd03l2-leng).
           clear watxt20.
           select single * from SKAT
             where ktopl = p_ktopl
               and spras = sy-langu
               and saknr = wabseg-saknr.
           if sy-subrc = '0'.
              move skat-txt20 to watxt20.
           endif.
         when 'TXT20'.
           move watxt20      to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'HKONT'.
           move wabseg-hkont to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'KUNNR'.
           move wabseg-kunnr to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'LIFNR'.
           move wabseg-lifnr to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'MATNR'.
           move wabseg-matnr to tmpfile2+wadd03l2-displ(wadd03l2-leng).
           clear wamaktx.
           select single * from MAKT
             where matnr = wabseg-matnr
               and spras = sy-langu.
           if sy-subrc = '0'.
             move makt-maktx to wamaktx.
           endif.
         when 'MAKTX'.
             move wamaktx    to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'EBELN'.
           move wabseg-ebeln to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'AUGDT'.
           move wabseg-augdt to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'ZLSCH'.
           move wabseg-zlsch to tmpfile2+wadd03l2-displ(wadd03l2-leng).
         when 'ZTERM'.
           move wabseg-zterm to tmpfile2+wadd03l2-displ(wadd03l2-leng).
* add additional BSEG fields here
         when others.
           skip 3.
           write: /1 wadd03l2-fieldname(8), ' not in BSEG extract.'.
           write: /1 'If field required, modify ', sy-repid.
         endcase.
    endloop.
    transfer tmpfile2 to phyfile2 length recsize2.


endform.

*-------------------------- PRINT VARIANT ------------------------------
* This routine prints all the entries in the variant
*-----------------------------------------------------------------------

FORM PRINT_VARIANT.

data: begin of t_scr_image occurs 0,
      line(120) type c,
      end of t_scr_image.


call function 'PRINT_SELECTIONS'
    exporting
          mode      = '1'
          rname     = sy-cprog
          rvariante = sy-slset
    tables
          infotab   = t_scr_image.

loop at t_scr_image.
  write: / t_scr_image-line+2.
endloop.
ENDFORM.

*-----------------------------------------------------------------------
