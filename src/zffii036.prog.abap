REPORT  ZFFII036 message-id ZS.

*----------------------------------------------------------------------*
*  Author    : MaryLou De Meester               SAP : East
*  Date      : Sept, 2006                Program Type : Interface
*  Issue Log : TR304
*----------------------------------------------------------------------*
*  Title : Split deduction file into PAYJRNL & EARL files
*
*  Description:
*     The deduction file will be split into 2 files, PAYJRNL & EARL,
*     each with layout that will go into RFBIBL00.
*----------------------------------------------------------------------*
* 2011/12/07 - TR928 - gymana -   Change ZBSEG length from 664 to 684
* 2009/02/11 - TR580 - mdemeest - Change BBKPF length from 284 to 309
* 2007/09/18 - TR423 - mdemeest - Ceridian payroll changes
* 2007/04/04 - TR414 - mdemeest - multiple company codes in 1 EARL file
* 2007/03/30 - TR    - mdemeest - Change EARL posting key for credits
*                                 from 31 to 29 - original code was
*                                 coded incorrectly
* 2006/09/13 - TR302 - mdemeest - New abap based on Sanjeev Chibbers
*                                 specs - as listed in the description
*----------------------------------------------------------------------*
tables:  dd03l,                  "Table fields table
         zferlc.

field-symbols: <F1>.
data:  char(21)    type c,
       nodata(1)   value '/'.

*----------------------------------------------------------------------
* BDC files
*----------------------------------------------------------------------
data:  z_bgr00 like bgr00.
data:  z_bbkpf like bbkpf.
data:  z_zbseg like zbseg.

data:  begin of bdcdata occurs 500.
       include structure bdcdata.
data:  end of bdcdata.


data:  begin of ideductns  occurs 0,
       payenddate(8)    type c,              "Payend date
       runid(10)        type c,              "Runid
       emplnum(6)       type c,              "Employee number
       sbukrs           like zfhcc-sbukrs,   "Company Code
       position_type(2) type c,              "Position type
       location(4)      type c,              "Region/Branch
       deptid(5)        type c,              "Department id
       home_cc(10)      type c,              "Home cost centre
       deduction_cd(3)  type c,              "Deduction code
       c_e(1)           type c,              "Company/Employee Ind
       pjgrp like zfead-pjgrp,               "Pay Journal Group
       ecgrp like zfead-ecgrp,               "Earning/Cost Group
       amount(11)       type c,              "Amount
       saknr like zfpaj-saknr,               "GL Account
       kostl like zfpaj-kostl,               "Cost Centre
       aufnr like zfpaj-aufnr,               "Order
       postdate(8)      type c,              "Calculated posting date
       osded(1)         type c,              "Outsourced deduction

       end of ideductns.

data:  begin of sdeductns  occurs 0,
       sbukrs           like zfhcc-sbukrs,   "Company Code

       payenddate(8)    type c,              "Payend date
       runid(10)        type c,              "Runid
       emplnum(7)       type c,              "Employee number
*       sbukrs           like zfhcc-sbukrs,   "Company Code
       position_type(2) type c,              "Position type
       location(4)      type c,              "Region/Branch
       deptid(5)        type c,              "Department id
       home_cc(10)      type c,              "Home cost centre
       deduction_cd(3)  type c,              "Deduction code
       c_e(1)           type c,              "Company/Employee Ind
       pjgrp like zfead-pjgrp,               "Pay Journal Group
       ecgrp like zfead-ecgrp,               "Earning/Cost Group
       amount(11)       type c,              "Amount
       saknr like zfpaj-saknr,               "GL Account
       kostl like zfpaj-kostl,               "Cost Centre
       aufnr like zfpaj-aufnr,               "Order
       postdate(8)      type c,              "Calculated posting date
       osded(1)         type c,              "Outsourced deduction
       end of sdeductns.

data: begin of searl          occurs 0,
      wakey2         like z_zbseg-newbs,
      pyacer         like zbseg-hkont,
      deduction_cd(6)  type c,              "Deduction code
      amount(11) type p decimals 2,
      end of searl.

*data:  begin of wafile       occurs 0,
*       filename(100),
*       end of wafile.

data:  tmp_sbukrs    like ideductns-sbukrs,
       tmp_monat     like bbkpf-monat,
       tmp_xblnr      like bbkpf-xblnr.

data:  wapayenddate(8)       type c,
       wapostdate(8)         type c,
       wakey1         like z_zbseg-newbs,
       wakey2         like z_zbseg-newbs.





*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
selection-screen begin of block box0 with frame.

selection-screen begin of block box1 with frame title text-100.

parameters: p_dedfl  like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/deductions.dat'.
selection-screen end of block box1.

selection-screen begin of block box2 with frame title text-101.

parameters: p_tcode like bbkpf-tcode obligatory default 'FB01',
            p_blart like bbkpf-blart obligatory default 'PY',
            p_waers like bbkpf-waers obligatory default 'CAD',
            p_bktxt like bbkpf-bktxt obligatory
                                     default 'Ceridian - SAP Payroll'.
selection-screen skip 1.

selection-screen begin of block box3 with frame title text-102.
parameters: p_pyjrnl like bgr00-group obligatory default 'ZFI_PYR_JRNL',
            p_payj   like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/payjrnl.dat',
            p_pyacct like zbseg-hkont obligatory default '251628'.
selection-screen end of block box3.

selection-screen begin of block box5 with frame title text-104.
parameters: p_pyclr  like bgr00-group obligatory default 'ZFI_PYR_CLR',
            p_payc   like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/payclear.dat',
            p_pycact like zbseg-hkont obligatory default '251628'.
selection-screen end of block box5.


selection-screen begin of block box4 with frame title text-103.
parameters: p_pyearl like bgr00-group obligatory default 'ZFI_PYR_EARL',
            p_earl   like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/earl.dat'.
*            p_pyacer like zbseg-hkont obligatory default '142301'.

selection-screen end of block box4.

selection-screen end of block box2.




selection-screen skip.
*PARAMETERS: p_group  like bgr00-group obligatory default
*                     'ZFI-BANLOANC'.

selection-screen skip.
selection-screen skip.
*PARAMETERS: p_dsize(4) type c obligatory default '999'.

selection-screen end of block box0.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
at selection-screen output.
*  replace wrk_symbolic with sy-sysid into: p_filein, p_fileot.
*  condense: p_filein no-gaps, p_fileot no-gaps.

*---------------------------------------------------------------------*
* start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
  perform OPEN_FILES.                                 "open files
  perform read_file.
                                                     "read into wa
* move info required for all transaction headers from the first
* deduction record to temporary fields
  loop at ideductns.
*  move ideductns-sbukrs     to tmp_sbukrs.

    move ideductns-postdate        to wapostdate.
*    move ideductns-postdate+2(2)   to wapostdate+4(2).
*    move ideductns-postdate+0(2)   to wapostdate+6(2).

    move ideductns-postdate+4(2)   to tmp_monat.
    move ideductns-payenddate      to wapayenddate.
*    move ideductns-payenddate+4(4) to wapayenddate+0(4).
*    move ideductns-payenddate+2(2) to wapayenddate+4(2).
*    move ideductns-payenddate+0(2) to wapayenddate+6(2).
    concatenate ideductns-runid ' -' wapayenddate+4(4)
                                           into tmp_xblnr.
    exit.
  endloop.

* Pay Journal
  perform summarize_by_account_cmpy.
  sort sdeductns by sbukrs saknr.
  perform create_payjrnl_file.
  perform create_outsourced_clear_file.
* EARL
* TR414 -----------------------------
*  perform create_earl_file_old.
*  perform earl_summary.
*  close dataset: p_dedfl.     "close files
  refresh sdeductns.
  perform select_earl_records.
  sort sdeductns by sbukrs.
  perform create_earl_file.
* TR414 end --------------------------

  close dataset: p_dedfl, p_payj, p_payc, p_earl.

  write: /1 'End of Processing'.

* HOUSEKEEPING ---------------------------------------------------------
* This routine verifies that each file identified in the .ini file
* exists with the same runid and that payroll runid has not been posted.
* To rerun this job, the runid must be deleted from the runid file
* If the runid has already been posted, the program will abend with the
* message 'JOB CANCELLED - RUN ID HAS ALREADY BEEN POSTED
*-----------------------------------------------------------------------
form open_files.
  open dataset p_dedfl for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_dedfl. "msg.
    exit.
  endif.

  open dataset p_earl for output in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_earl. "msg.
    exit.
  endif.

  open dataset p_payj for output in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_payj. "msg.
    exit.
  endif.

  open dataset p_payc for output in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_payc. "msg.
    exit.
  endif.



endform.

form read_file.
  do.
    read dataset p_dedfl into ideductns.
    if sy-subrc <> '0'.
       exit.
    endif.
    append ideductns.
  enddo.

endform.

form summarize_by_account_cmpy.
  loop at ideductns.
    read table sdeductns with key sbukrs = ideductns-sbukrs
                                   saknr = ideductns-saknr.
    if sy-subrc = '0'.
       add ideductns-amount to sdeductns-amount.
       modify sdeductns index sy-tabix.
    else.
       move-corresponding ideductns to sdeductns.
       append sdeductns.
    endif.
  endloop.
endform.

form create_payjrnl_file.
  perform init_structures using 'BGR00'.
  perform update_document_header using p_pyjrnl p_payj.
  loop at sdeductns.

        perform init_structures using 'BBKPF'.
        perform update_transaction_header using p_payj.
        perform determine_posting_key
                  using sdeductns-amount wakey1 wakey2 'JRNL'.

        perform update_trans_detail_payjournal using p_payj.

  endloop.
endform.

form create_outsourced_clear_file.
   perform init_structures using 'BGR00'.
   perform update_document_header using p_pyclr p_payc.
  loop at sdeductns.
  if sdeductns-osded = 'X'.
       perform init_structures using 'BBKPF'.
       perform update_transaction_header using p_payc.
       perform determine_posting_key
                   using sdeductns-amount wakey2 wakey1 'JRNL'.
       perform update_trans_detail_payjournal using p_payc.
  endif.
  endloop.
endform.


form create_earl_file_old.
  perform init_structures using 'BGR00'.
  perform update_document_header using p_pyearl p_earl.
  perform init_structures using 'BBKPF'.
  perform update_transaction_header using p_earl.

  loop at ideductns.
    select single * from ZFERLC where edcod = ideductns-deduction_cd.
    if sy-subrc = '0'.
       perform init_structures using 'ZBSEG'.
       move '2'                to z_zbseg-stype.
       move 'ZBSEG'            to z_zbseg-tbnam.
       perform determine_posting_key
                          using ideductns-amount wakey1 wakey2 'EARL'.
       move zferlc-newum        to z_zbseg-newum.
       move wakey1             to z_zbseg-newbs.
       move 'I0'               to z_zbseg-mwskz.
       concatenate 'E' ideductns-emplnum  into z_zbseg-newko.
       move ideductns-amount   to z_zbseg-wrbtr.
       move wapostdate to z_zbseg-zuonr.
       concatenate ideductns-deduction_cd ' for PED: ' wapayenddate
                              into z_zbseg-sgtxt.
       transfer Z_ZBSEG to p_earl length 684.

       move wakey2 to searl-wakey2.
       move ideductns-amount to searl-amount.
*       move p_pyacer         to searl-pyacer.
       move ideductns-saknr        to searl-pyacer. "from ZFPAJ
       move ideductns-deduction_cd to searl-deduction_cd.
       collect searl.

*       perform init_structures using 'ZBSEG'.
*       move 'ZBSEG'            to z_zbseg-tbnam.
*       move '2'                to z_zbseg-stype.
*       move wakey2             to z_zbseg-newbs.
*       move p_pyacer           to z_zbseg-newko.
*       move ideductns-amount   to z_zbseg-wrbtr.
*      move wapostdate to z_zbseg-zuonr.
*       concatenate ideductns-deduction_cd ' for PED: ' wapayenddate
*                              into z_zbseg-sgtxt.

*       transfer Z_ZBSEG to p_earl length 664.

    endif.
  endloop.
endform.

form earl_summary.
  loop at searl.
      perform init_structures using 'ZBSEG'.
       move 'ZBSEG'            to z_zbseg-tbnam.
       move '2'                to z_zbseg-stype.
       move searl-wakey2       to z_zbseg-newbs.
       move searl-pyacer       to z_zbseg-newko.
       move searl-amount       to z_zbseg-wrbtr.
       move wapostdate         to z_zbseg-zuonr.
       concatenate searl-deduction_cd ' for PED: ' wapayenddate
                               into z_zbseg-sgtxt.

       transfer Z_ZBSEG to p_earl length 664.
  endloop.
endform.

form init_structures using tabname.
  select * from DD03L where tabname = tabname.
     clear char.
     char(2) = 'Z_'.
     char+2(5) = tabname.
     char+7(1) = '-'.
     char+8    = DD03L-fieldname.
     assign (char) to <F1>.
     <F1> = nodata.
  endselect.
endform.

form update_document_header using pgroup pfile.
  move '0'         to z_bgr00-stype.
  move pgroup      to z_bgr00-group.
  move sy-mandt    to z_bgr00-mandt.
  move 'BATCH'     to z_bgr00-usnam.
  compute z_bgr00-start = sy-datum - 1.
  transfer Z_BGR00 to pfile length 38.
endform.

form update_transaction_header using pfile.
  move '1'              to z_bbkpf-stype.
  move p_tcode          to z_bbkpf-tcode.
*  compute z_bbkpf-bldat = sy-datum - 1.
  move sy-datum         to z_bbkpf-bldat.
  move p_blart          to z_bbkpf-blart.
  move sdeductns-sbukrs to z_bbkpf-bukrs.
  move wapostdate       to z_bbkpf-budat.
  move p_waers          to z_bbkpf-waers.
  move tmp_xblnr        to z_bbkpf-xblnr.
  move p_bktxt          to z_bbkpf-bktxt.
  transfer Z_BBKPF      to pfile length 309.

endform.

form update_trans_detail_payjournal using pfile.
    perform init_structures using 'ZBSEG'.
    move '2'                to z_zbseg-stype.
    move 'ZBSEG'            to z_zbseg-tbnam.

*    perform determine_posting_key
*                   using sdeductns-amount wakey1 wakey2 'JRNL'.
    move wakey1             to z_zbseg-newbs.
    move sdeductns-saknr    to z_zbseg-newko.
    move sdeductns-amount   to z_zbseg-wrbtr.
    move wapostdate to z_zbseg-zuonr.
    concatenate sdeductns-deduction_cd ' for PED: ' wapayenddate
                            into z_zbseg-sgtxt.
*---------------------------------------------------------------------
*  Fill in AUFNR (internal order) only if there is an entry.  If AUFNR
*  is filled in, then employee is required.  Since the amount is at a
*  summarized level,  fill in with a dummy employee number ie 9999999.
*-----------------------------------------------------------------------
data: length(4) type n.
    compute length = strlen( sdeductns-aufnr ).
    if length > 0.
        move sdeductns-aufnr to z_zbseg-aufnr.
        move '9999999'       to z_zbseg-pernr.
    endif.
*---------------------------------------------------------------------

    transfer Z_ZBSEG to pfile length 664.

    perform init_structures using 'ZBSEG'.
    move 'ZBSEG'            to z_zbseg-tbnam.
    move '2'                to z_zbseg-stype.
    move wakey2             to z_zbseg-newbs.
    move p_pyacct           to z_zbseg-newko.
    move sdeductns-amount   to z_zbseg-wrbtr.
*    concatenate 'for PED: ' wapayenddate
*                            into z_zbseg-sgtxt.
    concatenate sdeductns-deduction_cd ' for PED: ' wapayenddate
                            into z_zbseg-sgtxt.


    move wapostdate to z_zbseg-zuonr.
    transfer Z_ZBSEG to pfile length 664.

endform.

form determine_posting_key using amount key1 key2 filename.
data:  waamount like sdeductns-amount.
* Determine posting keys
   compute waamount = amount * -1.

   if filename = 'JRNL'.
      if  waamount > 0.
          move '40' to key1.
          move '50' to key2.
       else.
          move '50' to key1.
          move '40' to key2.
       endif.
   else.
      if  waamount > 0.
*                           move '31' to key1.  "2007/03/30
          move '29' to key1.                    "2007/03/30
          move '50' to key2.
       else.
          move '39' to key1.
          move '40' to key2.
       endif.

    endif.


* Amount to be posted into SAP must always be positive
   if amount > 0.
   else.
      compute amount = amount * -1.
   endif.
*  PeopleSoft sending a file without decimals.  Assume 2 decimal points
   compute amount = amount / 100.
endform.

* TR414---------
form select_earl_records.
  loop at ideductns.
    select single * from zferlc where edcod = ideductns-deduction_cd.
    if sy-subrc = '0'.
       move-corresponding ideductns to sdeductns.
       append sdeductns.
    endif.
  endloop.
endform.

form create_earl_file.
  perform init_structures using 'BGR00'.
  perform update_document_header using p_pyearl p_earl.

  loop at sdeductns.
    at new sbukrs.
       perform init_structures using 'BBKPF'.
       perform update_transaction_header using p_earl.
    endat.

    select single * from ZFERLC where edcod = sdeductns-deduction_cd.
    if sy-subrc = '0'.
       perform init_structures using 'ZBSEG'.
       move '2'                to z_zbseg-stype.
       move 'ZBSEG'            to z_zbseg-tbnam.
       perform determine_posting_key
                          using sdeductns-amount wakey1 wakey2 'EARL'.
       move zferlc-newum        to z_zbseg-newum.
       move wakey1             to z_zbseg-newbs.
       move 'I0'               to z_zbseg-mwskz.
       concatenate '0000' sdeductns-emplnum  into z_zbseg-newko.
       move sdeductns-amount   to z_zbseg-wrbtr.
       move wapostdate to z_zbseg-zuonr.
       concatenate sdeductns-deduction_cd ' for PED: ' wapayenddate
                              into z_zbseg-sgtxt.
       transfer Z_ZBSEG to p_earl length 664.

       move wakey2 to searl-wakey2.
       move sdeductns-amount to searl-amount.
*       move p_pyacer         to searl-pyacer.
       move sdeductns-saknr        to searl-pyacer. "from ZFPAJ
       move sdeductns-deduction_cd to searl-deduction_cd.
       collect searl.
    at end of sbukrs.
       perform earl_summary.
       refresh searl.
    endat.

    endif.
  endloop.
endform.


* TR414 end ----
