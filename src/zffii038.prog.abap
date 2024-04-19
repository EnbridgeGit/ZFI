REPORT  ZFFII038 message-id ZS.

*----------------------------------------------------------------------*
*  Author    : MaryLou De Meester               SAP : East
*  Date      : Sept, 2006                Program Type : Interface
*  Issue Log : TR306
*----------------------------------------------------------------------*
*  Title : Split distribution file into LABOUR & COMPANY
*
*  Description:
*     The distribution file will be split into 2 files, LABDIST  CMPDIST
*     each with layout that will go into RFBIBL00.
*----------------------------------------------------------------------*
* 2011/12/07 TR928 - gymana - Change ZBSEG length from 664 to 684
* 2010/04/29 TR466 - gymana - Corrected code to load header info on
*                             the first read of the input file.
* 2009/02/11 TR580 - mdemeest - Change BBKPF length from 284 to 309
* 2007/05/10 TR431 - mdemeest - Jennifer Verwegen requested that the *
*                               description contains payenddate not * *
*                               runid
* 2006/10/23- TR306- mdemeest - New abap based on Sanjeev Chibbers
*                               specs - as listed in the description
*----------------------------------------------------------------------*
tables:  dd03l.                  "Table fields table

field-symbols: <F1>.
data:  char(21)    type c,
       nodata(1)   value '/',
       wactr(4)    type n value 0,      "Counter for #of lines in BDC
       wa_1strec   type c value 'Y',             "TR466
       wa_amountn  type p decimals 2,
       wa_hoursn   type p decimals 3,
       wa_amt(5)   type c value '0.00',
*       wa_hours(6) type c value '  0.00 '.
       wa_hours(6) type c value '     0 '.

*----------------------------------------------------------------------
* BDC files
*----------------------------------------------------------------------
data:  z_bgr00 like bgr00.
data:  z_bbkpf like bbkpf.
data:  z_zbseg like zbseg.

data:  begin of bdcdata occurs 500.
       include structure bdcdata.
data:  end of bdcdata.

*-----------------------------------------------------------------------
*  This layout matches the odistribution file from ZFFII037
*-----------------------------------------------------------------------
data:  begin of idistribution  occurs 0,
       payenddate(8)    type c,              "Payend date
       runid(10)        type c,              "Runid
       emplnum(6)       type c,              "Employee number
       sbukrs           like zfhcc-sbukrs,   "Company Code
       jobclass(2)      type c,              "Job class
       earnings_cd(3)   type c,              "Earnings code
       amount(11)       type c,              "Amount
       hours(7)         type c,              "Regular hours
       kostl            like bbseg-kostl,    "Cost Centre
       aufnr            like bbseg-aufnr,    "Order
       projn            like bbseg-projn,    "WBS
       hkont            like bbseg-hkont,    "GL Account
       postdate(8)      type c,              "Postdate
       c_e(1)           type c,              "Comp/Emp dist code
       dist_meth(1)     type c,              "Distribution Method
       Loctn            like zfgrd-loctn,    "Location Field
       depmt            like zfgrd-depmt,    "Department

*       saknr like zfpaj-saknr,               "GL Account

       end of idistribution.

*data:  begin of wafile       occurs 0,
*       filename(100),
*       end of wafile.

*ata:  tmp_sbukrs    like ideductns-sbukrs,
*      tmp_monat     like bbkpf-monat,
data:  tmp_xblnr      like bbkpf-xblnr.
data:  totamt like idistribution-amount.

data:  wapayenddate(8)       type c,
       wapostdate(8)         type c,
       wakey          like z_zbseg-newbs.


data:  wabukrs like idistribution-sbukrs,
       wabukrshold like idistribution-sbukrs.



*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
selection-screen begin of block box0 with frame.

selection-screen begin of block box1 with frame title text-100.

parameters: p_dist    like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/distribution.dat'.
selection-screen end of block box1.

selection-screen begin of block box2 with frame title text-101.

parameters: p_tcode like bbkpf-tcode obligatory default 'FB01',
            p_blart like bbkpf-blart obligatory default 'PY',
            p_waers like bbkpf-waers obligatory default 'CAD',
            p_bktxt like bbkpf-bktxt obligatory
                                     default 'Ceridian - SAP Payroll',
            p_items(3) type c default '940'.
selection-screen skip 1.

selection-screen begin of block box3 with frame title text-102.
parameters: p_plab like bgr00-group obligatory default 'ZFI_PYR_LABR',
            p_olab   like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/empdist.dat',
            p_pyacct like zbseg-hkont obligatory default '251628'.
selection-screen end of block box3.

selection-screen begin of block box4 with frame title text-103.
parameters: p_pcmp  like bgr00-group obligatory default 'ZFI_PYR_CMPY',
            p_ocmp   like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/cmpdist.dat',
            p_pyacer like zbseg-hkont obligatory default '251628'.

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
  perform read_file.                                  "read file
*-----------------------------------------------------------------------
* create Labour Distribution file for RFBIBL00
*-----------------------------------------------------------------------
  perform init_structures using 'BGR00'.
  perform update_document_header using p_plab p_olab.
  perform init_structures using 'BBKPF'.
  perform update_transaction_header using p_olab.
  clear: wactr, totamt.
  loop at idistribution.
     if idistribution-c_e = 'E'.
       perform update_detail_segments using p_olab.
    endif.
  endloop.
  perform update_detail_segments_sum using p_olab.

*-----------------------------------------------------------------------
* create Company Distribution file for RFBIBL00
*-----------------------------------------------------------------------
  move wabukrshold to wabukrs.
  perform init_structures using 'BGR00'.
  perform update_document_header using p_pcmp p_ocmp.
  perform init_structures using 'BBKPF'.
  perform update_transaction_header using p_ocmp.
  clear: wactr, totamt.
  loop at idistribution.
*     move idistribution-sbukrs to wabukrs.
    if idistribution-c_e = 'C'.
       perform update_detail_segments using p_ocmp.
    endif.
  endloop.
  perform update_detail_segments_sum using p_ocmp.

**    move idistribution-postdate+4(4)   to wapostdate+0(4).
**    move idistribution-postdate+2(2)   to wapostdate+4(2).
**    move idistribution-postdate+0(2)   to wapostdate+6(2).

**    move idistribution-postdate+6(2)   to tmp_monat.

  close dataset: p_dist, p_olab, p_ocmp.                  "close files

  write: /1 'End of Processing'.

*-----------------------------------------------------------------------
form open_files.
  open dataset p_dist for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_dist. "msg.
    exit.
  endif.

  open dataset p_olab for output in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_olab. "msg.
    exit.
  endif.

  open dataset p_ocmp for output in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_ocmp. "msg.
    exit.
  endif.

endform.

form read_file.
  do.
    read dataset p_dist into idistribution.
    if sy-subrc <> '0'.
       exit.
    endif.
    if wa_1strec = 'Y'.
       move idistribution-sbukrs          to wabukrs.
       move idistribution-sbukrs          to wabukrshold.
       move idistribution-payenddate      to wapayenddate.
       move idistribution-postdate        to wapostdate.
       concatenate idistribution-runid ' -' wapayenddate+4(4)
                                              into tmp_xblnr.
       move 'N' to wa_1strec.
    endif.

    if idistribution-amount <> 0.  "eliminate 0 amt records
       append idistribution.
    endif.
  enddo.

  sort idistribution by sbukrs emplnum hkont.
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
  move wabukrs          to z_bbkpf-bukrs.
  move wapostdate       to z_bbkpf-budat.
  move p_waers          to z_bbkpf-waers.
  move tmp_xblnr        to z_bbkpf-xblnr.
  move p_bktxt          to z_bbkpf-bktxt.
  transfer Z_BBKPF      to pfile length 309.

endform.




form update_detail_segments using pfile.
data:  wa_distmeth(15) type c.
  if wactr => p_items or idistribution-sbukrs <> wabukrs.
     perform update_detail_segments_sum using pfile.
     move idistribution-sbukrs to wabukrs.
     perform update_transaction_header using pfile.

     clear: wactr, totamt.
  endif.

  perform init_structures using 'ZBSEG'.
  move '2'                    to z_zbseg-stype.
  move 'ZBSEG'                to z_zbseg-tbnam.
  move idistribution-emplnum  to z_zbseg-pernr.
  perform determine_posting_key using idistribution-amount
                                      idistribution-hours wakey.

  move wakey             to z_zbseg-newbs.
*  move p_pyacct           to z_zbseg-newko.
  move idistribution-amount   to z_zbseg-wrbtr.
  clear wa_distmeth.
  case idistribution-dist_meth.
     when 'V'.  move 'TimeSheetDist:' to wa_distmeth.
     when 'C'.  move 'CallOut Dist:'  to wa_distmeth.
     when 'E'.  move 'StdDist:'       to wa_distmeth.
     when 'H'.  move 'HomeDist:'      to wa_distmeth.
     when 'G'.  move 'GroupDist:'     to wa_distmeth.
     when 'P'.  move 'ProratedDist:'  to wa_distmeth.
     when 'F'.  move 'FixedDist:'     to wa_distmeth.
   endcase.
* description   SGTXT
* TR431 - changed from runid to payenddate
*   concatenate idistribution-earnings_cd ' for PED: '
*               idistribution-runid wa_distmeth
*               idistribution-postyp into z_zbseg-sgtxt.
   concatenate idistribution-earnings_cd 'for PED:'
               idistribution-payenddate '-' wa_distmeth
               idistribution-jobclass into z_zbseg-sgtxt
               separated by space.
   if idistribution-dist_meth = 'G'.
      concatenate z_zbseg-sgtxt ' Loc:' idistribution-loctn
                  into z_zbseg-sgtxt.
   endif.
*end of description .----------------
  if idistribution-hours <> wa_hours.   "0
     move idistribution-hours   to z_zbseg-menge.
     move 'H'                   to z_zbseg-meins.
  endif.
  move idistribution-hkont   to z_zbseg-newko.
  move wapostdate to z_zbseg-zuonr.
  if idistribution-kostl <> space.
     move idistribution-kostl to z_zbseg-kostl.
  elseif idistribution-aufnr <> space.
     move idistribution-aufnr to z_zbseg-aufnr.
  elseif idistribution-projn <> space.
     move idistribution-projn to z_zbseg-projk.
  endif.
  if wakey = '40'.
     add idistribution-amount to totamt.
  else.
     subtract idistribution-amount from totamt.
  endif.
  transfer Z_ZBSEG to pfile length 684.
  add 001 to wactr.

*  if wactr => p_items or idistribution-sbukrs <> wabukrs.
*     perform update_detail_segments_sum using pfile.
*     perform update_transaction_header using pfile.
*     move idistribution-sbukrs to wabukrs.
*
*     clear: wactr, totamt.
*  endif.
endform.

form update_detail_segments_sum using pfile.
  perform init_structures using 'ZBSEG'.
  move '2'                    to z_zbseg-stype.
  move 'ZBSEG'                to z_zbseg-tbnam.
   move totamt to wa_amountn.
   if  wa_amountn < wa_amt.   "Offsetting amount
       move '40' to wakey.
       compute totamt = totamt * -1.
   else.
       move '50' to wakey.
   endif.

  move wakey             to z_zbseg-newbs.
  move p_pyacct    to z_zbseg-newko.
  move totamt   to z_zbseg-wrbtr.
*  concatenate idistribution-earnings_cd ' for PED: '
*               idistribution-runid
*               idistribution-postyp into z_zbseg-sgtxt.
  concatenate 'For PED: ' idistribution-runid into z_zbseg-sgtxt.

*  move idistribution-hours   to z_zbseg-menge.
*  move 'H'                   to z_zbseg-meins.
  move wapostdate to z_zbseg-zuonr.
*  if idistribution-kostl <> space.
*     move idistribution-kostl to z_zbseg-kostl.
*  elseif idistribution-aufnr <> space.
*     move idistribution-aufnr to z_zbseg-aufnr.
*  elseif idistribution-projn <> space.
*     move idistribution-projn to z_zbseg-projk.
*  endif.
  transfer Z_ZBSEG to pfile length 664.


endform.

form determine_posting_key using amount hours key.

* Determine posting keys.
* The amount, whether positive or negative determines the posting key.
* Amount to be posted into SAP must always be positive.  The number
* of hours must also be postive.
*
   move amount to wa_amountn.
   move hours  to wa_hoursn.
   if wa_amountn < wa_amt.
      move '50' to key.
      compute amount = wa_amountn * -1.
*      compute hours = wa_hoursn * -1.
   else.
      move '40' to key.
   endif.
   compute hours = abs( wa_hoursn ).

endform.
