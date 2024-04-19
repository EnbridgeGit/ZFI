REPORT  ZFFII034 message-id ZS.

*----------------------------------------------------------------------*
*  Author    : MaryLou De Meester               SAP : East
*  Date      : Sept, 2006                Program Type : Interface
*  Issue Log : TR302
*----------------------------------------------------------------------*
*  Title : Accept Peoplesoft files & modify files for SAP.
*  Description:
*     - Read a set of PeopleSoft files, validate that they belong
*       together by verifying a unique runid.
*----------------------------------------------------------------------*
* 2009/03/27 - TR423 - mdemeest - Lower --> UPPER
* 2008/04/23 - TR423 - mdemeest - eliminate message regarding 997/998
*                               - add SLU with employee# in report
* 2007/09/18 - TR423 - mdemeest - Ceridian payroll changes
* 2006/09/13 - TR302 - mdemeest - New abap based on Sanjeev Chibbers
*                                 specs - as listed in the description
*----------------------------------------------------------------------*

tables:  zfhcc,               "Map Peoplesoft to SAP Company Codes
         zfhed,
         zfpjd.

data:  wa_postdate(8)   type c,
       wapayenddate(8)  type c,
       wasamount(15)    type c,
       inputrec(150)    type c.


*-----------------------------------------------------------------------
*  Each input table from PeopleSoft is converted to an SAP
*  internal table each of which will be written to a generic file for
*  input to all the other abaps in this Payroll interface
*-----------------------------------------------------------------------


data:  begin of igrosspay,       "File from PeopleSoft
        payenddate(8)    type c,              "Payend date
        totalgross(11)   type c,              "Total Gross
       end of igrosspay.

data:  begin of ogrosspay occurs 0,
        payenddate(8)    type c,              "Payend date
        totalgross(11)   type c,              "Total Gross
       end of ogrosspay.

data:  begin of ideductns,       "File from PeopleSoft
       payenddate(8)    type c,              "Payend date
       emplnum(6)       type c,              "Employee number
       hbukrs           like zfhcc-hbukrs,   "Company Code for P/S
       deduction_cd(3)  type c,              "Deduction code
       c_e(1)           type c,              "Company/Employee Ind
       amount(11)       type c,              "Amount
       cheque_date(8)   type c,              "Cheque Issue Date
       end of ideductns.

data: begin of waded,
       payenddate(8)    type c,
       emplnum(6)       type c,
       sbukrs           like zfhcc-sbukrs,
       deduction_cd(3)  type c,
       c_e(1)           type c,
       amount(11)       type c,
       cheque_date(8)   type c,
       postdate(8)      type c,
       end of waded.


data:  begin of odeductns  occurs 0,
       payenddate(8)    type c,              "Payend date
       emplnum(6)       type c,              "Employee number
       sbukrs           like zfhcc-sbukrs,   "Company Code
       deduction_cd(3)  type c,              "Deduction code
       c_e(1)           type c,              "Company/Employee Ind
       amount(11)       type c,              "Amount
       cheque_date(8)   type c,              "Cheque Issue Date
       postdate(8)      type c,              "Calculated posting date
       end of odeductns.

data:  begin of ibisearns,       "File from PeopleSoft
        payenddte(8)     type c,              "Payend date
        emplnum(6)       type c,              "Employee Number
        jobclass(2)      type c,              "Job class
        hbukrs           like zfhcc-hbukrs,   "Company Code from P/S
        earnings_cd(3)   type c,              "Earnings code
        reg_hrs(6)       type c,              "Regular hours
        reg_earn(11)     type c,              "Regular earnings
       end of ibisearns.

data:  begin of obisearns   occurs 0,
        sbukrs           like zfhcc-sbukrs,   "Company Code from SAP
        emplnum(6)       type c,              "Employee Number
        earnings_cd(3)   type c,              "Earnings code
        payenddte(8)     type c,              "Payend date
        jobclass(2)      type c,              "Job class
        reg_hrs(6)       type c,              "Regular hours
        reg_earn(11)     type c,              "Regular earnings
        postdate(8)      type c,              "Calculated posting date
        c_e(1)           type c,              "Calculated empl/company
       end of obisearns.

data:  begin of sbisearns   occurs 0,
        sbukrs           like zfhcc-sbukrs,   "Company Code from SAP
        emplnum(6)       type c,              "Employee Number
        earnings_cd(6)   type c,              "Earnings code
        payenddte(8)     type c,              "Payend date
        jobclass(2)      type c,              "Position type
        reg_hrs(6)       type c,              "Regular hours
        reg_earn(11)     type c,              "Regular earnings
        postdate(8)      type c,              "Calculated posting date
        c_e(1)           type c,              "Calculated empl/company
       end of sbisearns.

data:  begin of istddist,       "File from PeopleSoft
       hbukrs           like zfhcc-hbukrs,   "Company Code from P/S
       emplnum(6)       type c,              "Employee Number
       percent(6)       type c,              "Percent
       co_type(3)       type c,              "Cost Object type
       cost_object(26)  type c,              "Cost Object
       end of istddist.

data:  begin of ostddist  occurs 0,
       sbukrs           like zfhcc-sbukrs,   "Company Code from SAP
       emplnum(6)       type c,              "Employee Number
       percent(6)       type c,              "Percent
       co_type(3)       type c,              "Cost Object type
       cost_object(25)  type c,              "Cost Object
       end of ostddist.

data:  begin of ilabdist,       "File from PeopleSoft
       payenddte(8)     type c,              "Payend date
       emplnum(6)       type c,              "Employee Number
       earnings_cd(3)   type c,              "Earnings code
       hours(6)         type c,              "Hours
       amount(11)       type c,              "Amount
       cost_object(15)  type c,              "Cost Object
       end of ilabdist.

data:  begin of olabdist  occurs 0,
       payenddte(8)     type c,              "Payend date
       emplnum(6)       type c,              "Employee Number
       earnings_cd(3)   type c,              "Earnings code
       hours(6)         type c,              "Hours
       amount(11)        type c,              "Amount
       cost_object(25)  type c,              "Cost Object
       end of olabdist.

data:  begin of iemployeeinfo,   "File from PeopleSoft
        regioncode(12)   type c,              "Region Code
        location(4)      type c,              "Location Code
        emplnum(6)       type c,              "Employee Number
        home_cc(5)       type c,              "Home Cost Centre
        hbukrs           like zfhcc-hbukrs,   "Company Code
        jobclass(2)      type c,              "Job class
        deptid(5)        type c,
        division(4)      type c,
       end of iemployeeinfo.

data:  begin of oemployeeinfo  occurs 0,
        sbukrs           like zfhcc-sbukrs,   "Company code
        emplnum(6)       type c,              "Employee Number
        regioncode(12)   type c,              "Region Code
        deptid(5)        type c,              "Department id
        location(4)      type c,              "Region/Branch
        jobclass(2) type c,                   "Job Class
        home_cc(10)      type c,              "Home cost centre
        division(4)      type c,              "Division
       end of oemployeeinfo.

data:  begin of wafile       occurs 0,
       filename(100),
       end of wafile.

data:  waruncntl(80) type c.
data: wa_amt like odeductns-amount.


data:  warunid(30), wafilename(100), wafilename1(100).

*data: delimeter value '09' type X.
*data: inrec(400), outrec(2000), infile(70), outfile(70).

field-symbols: <F1>.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
selection-screen begin of block box0 with frame.
selection-screen begin of block box1 with frame title text-100.

parameters: p_filenm like filename-fileextern obligatory default
                     '/usr/sap/interfaces/scripts/payrollfiles2.ini',
            p_runtmp like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/runid.tmp',
            p_src    like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057'.

selection-screen skip.

select-options: s_dedcd for ideductns-deduction_cd no intervals.

select-options: s_cddis for ideductns-deduction_cd no intervals.
*PARAMETERS: p_group  like bgr00-group obligatory default
*                     'ZFI-BANLOANC'.

selection-screen skip.
selection-screen skip.
*PARAMETERS: p_dsize(4) type c obligatory default '999'.

selection-screen end of block box1.
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
  perform OPEN_FILES.

* HOUSEKEEPING ---------------------------------------------------------
* This routine verifies that each file identified in the .ini file
* exists with the same runid and that payroll runid has not been posted.
* To rerun this job, the runid must be deleted from the runid file
* If the runid has already been posted, the program will abend with the
* message 'JOB CANCELLED - RUN ID HAS ALREADY BEEN POSTED
*-----------------------------------------------------------------------
  read dataset p_runtmp into warunid.
  concatenate p_src '/data/runidctl.dat' into wafilename1.
  open dataset wafilename1 for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with wafilename1. "msg.
    exit.
  endif.


data:  waerror(1) type c.
data:  warunidctl(20) type c.
data:  runidlength  type i.
  do.
    read dataset wafilename1 into warunidctl.
    if sy-subrc <> '0'. exit. endif.                  " ==> end of file
    runidlength = strlen( warunidctl ) - 8.
    if warunid = warunidctl(runidlength).
       message E030 with warunid.
    endif.
  enddo.


  do.
   read dataset p_filenm into wafile.
   append wafile.
   if sy-subrc <> 0.
      exit.
   endif.
*  concatenate: '/usr/sap/interfaces/D30/IFFI057/'
   concatenate: p_src '/' wafile-filename+5(30) '_'  warunid
                 into wafilename.
   open dataset wafilename for input in text mode.        " message msg
   if ( sy-subrc <> 0 ).
     message E002 with wafilename. "msg.
     exit.
   endif.
*-----------------------------------------------------------------------
* This routine takes each file and converts it according to specific
* rules.  Each file is then written out to be processed by other abaps
* in the Payroll interface
* - PeopleSoft company code must be mapped to SAP company code
*----------------------------------------------------------------------

* PROCESSING input files

  case wafile-filename+12(30).
    when 'GROS'.
       perform output_grosspay.
    when 'DED'.
       perform output_deductns.
    when 'EARN'.
       perform output_bisearns.
    when 'STND'.
       perform output_stddist.
    when 'LAB'.
       perform output_labdist.
    when 'HRIS'.
       perform output_empinfo.
  endcase.

  close dataset: wafilename.

  enddo.

   perform determine_payenddate.

   loop at odeductns.
     concatenate warunid wapayenddate into warunid.
     transfer warunid to wafilename1.
     exit.
   endloop.

  close dataset: p_filenm.

*  Summarize earnings table by company code, employee, earnings code.
 sort sbisearns by sbukrs emplnum  earnings_cd.

 refresh obisearns.
 loop at sbisearns.
   read table obisearns with key sbukrs = sbisearns-sbukrs
                                   emplnum = sbisearns-emplnum
                                   earnings_cd = sbisearns-earnings_cd.
   if sy-subrc = '0'.
      add sbisearns-reg_earn    to obisearns-reg_earn.
      add sbisearns-reg_hrs     to obisearns-reg_hrs.
      modify obisearns index sy-tabix.
   else.
      move-corresponding sbisearns to obisearns.
      append obisearns.
   endif.

 endloop.


 perform determine_paypostdate.

* once the data has been processed, write all the internal tables to
* generic file names.

 perform write_files.
 perform calculate_files_totals.
 perform report_on_codes.

*-----------------------------------------------------------------------
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_FILES.
  DATA: MSG(100).
*-----------------------------------------------------------------------
  open dataset p_filenm for input in text mode message msg.
  if ( sy-subrc <> 0 ).
    message E002 with p_filenm msg.
  endif.
  open dataset p_runtmp for input in text mode message msg.
  if ( sy-subrc <> 0 ).
    message E002 with p_runtmp msg.
  endif.
*-----------------------------------------------------------------------
*  open dataset p_fileot for output in text mode message msg.
*  if ( sy-subrc <> 0 ).
*    message E002 with outfile msg.
*  endif.
ENDFORM.

form close_files.
  close dataset: p_filenm, p_runtmp.
endform.

form output_grosspay.

 do.
   read dataset wafilename into inputrec.
   if sy-subrc <> '0'.
      exit.
   endif.
   split inputrec at '|' into
         igrosspay-payenddate igrosspay-totalgross.
   move-corresponding igrosspay to ogrosspay.
   append ogrosspay.
 enddo.
endform.


form output_deductns.
 do.
   read dataset wafilename into inputrec.
   if sy-subrc <> '0'.
      exit.
   endif.
   split inputrec at '|' into
       ideductns-payenddate   ideductns-emplnum
       ideductns-hbukrs       ideductns-deduction_cd
       ideductns-c_e          ideductns-amount
       ideductns-cheque_date.
   if ideductns-deduction_cd in s_dedcd.
*-------------------------------------------------------------
*                        removed 2008/04/23 per Andy Tattersal
*      write: / '9xx record for employee ', ideductns-emplnum,
*                              ' ', ideductns-deduction_cd.
*-------------------------------------------------------------
   else.
      move-corresponding ideductns to odeductns.
      select single * from zfhcc where hbukrs = ideductns-hbukrs.
      if sy-subrc = '0'.
         move zfhcc-sbukrs to odeductns-sbukrs.
         append odeductns.
      else.
         write: /1 'Deduction file Company Code error', ideductns+8(32).
      endif.
   endif.
 enddo.
loop at odeductns.
* move company deductions to earnings
    if odeductns-c_e = 'C'.
       clear obisearns.
       move-corresponding odeductns to sbisearns.
       move odeductns-deduction_cd to sbisearns-earnings_cd.
       move odeductns-amount       to sbisearns-reg_earn.
       move '000000'               to sbisearns-reg_hrs.
       append sbisearns.
       move 'D'                     to odeductns-c_e.
       modify odeductns.
    endif.
endloop.


data: tmp_sbukrs like odeductns-sbukrs.
data: tmp_chequedt like odeductns-cheque_date.
data:  camount like waded-amount.   "Company total
data:  eamount like waded-amount.   "Employee total
data:  gamount like waded-amount.   "Gross total

* calculate the "NET" amount ie 999 code
sort odeductns by payenddate emplnum.
loop at odeductns.
  move odeductns-sbukrs to tmp_sbukrs.
  move odeductns-cheque_date to tmp_chequedt.
  at new emplnum.
     clear: camount, eamount, gamount.
     move odeductns to waded.
     move '999' to waded-deduction_cd.
     move 'E'   to waded-c_e.
     move tmp_sbukrs to waded-sbukrs.
     move tmp_chequedt to waded-cheque_date.
     move space        to waded-postdate.

     clear waded-amount.
  endat.
  if odeductns-c_e = 'D'.  "C changed to D in above code
     add odeductns-amount to camount.
  endif.

  if odeductns-c_e = 'E' or odeductns-c_e = 'D'.
     if odeductns-deduction_cd = 'GRO'.
        add odeductns-amount to gamount.
     else.
        select single * from ZFHED
          where hed   = odeductns-deduction_cd
            and coemp = odeductns-c_e.
        if sy-subrc = '0'.
           select single * from ZFPJD
             where ecgrp = zfhed-pjgrp.
           if sy-subrc = '0'.
              if zfpjd-osded = 'X'.
              else.
                 add odeductns-amount to eamount.
              endif.
           endif.
        elseif odeductns-c_e = 'E'.
            compute wa_amt = odeductns-amount / 100.
            write: / 'ZFFII034: HED code missing from ZFHED table '.
            write: / 'Employee= ', odeductns-emplnum,
                     'HED code = ', odeductns-deduction_cd,
                          'C-E = ', odeductns-c_e,
                       'Amount = ', wa_amt.
            write: /.
        endif.
     endif.
  endif.

  at end of emplnum.
     if gamount = 0.
        compute waded-amount = gamount + camount.
     else.
        compute waded-amount = gamount + camount - eamount.
     endif.
     move-corresponding waded to odeductns.
     insert odeductns.
  endat.

endloop.

sort odeductns by payenddate emplnum.
endform.

form output_bisearns.
*----------------------------------------------------------------------
*  There are earnings records being passed to SAP but are not to be
*  posted into SAP.  In order to balance the GROSSPAY record, the amount
*  of these records needs to be totalled.
*  So the GROSSPAY record from PeopleSoft =
*    the earnings of Company Codes to be posted into SAP
*    plus the earnings that are NOT posted into SAP
*-----------------------------------------------------------------------

      clear wasamount.
 do.
   read dataset wafilename into inputrec.
   if sy-subrc <> '0'.
         exit.
   endif.
   split inputrec at '|' into
       ibisearns-payenddte    ibisearns-emplnum
       ibisearns-jobclass     ibisearns-hbukrs
       ibisearns-earnings_cd  ibisearns-reg_hrs
       ibisearns-reg_earn.

   clear sbisearns.
   move-corresponding ibisearns to sbisearns.
   move 'E'                     to sbisearns-c_e.
   select single * from zfhcc where hbukrs = ibisearns-hbukrs.
   if sy-subrc = '0'.
      move zfhcc-sbukrs to sbisearns-sbukrs.
      append sbisearns.
   else.
      add ibisearns-reg_earn to wasamount.
      write: /1 'Earnings  file Company Code error', ibisearns+8(31).
   endif.
 enddo.
 compute wasamount = wasamount / 100.

endform.

form output_stddist.
data:  ctr type i.
 do.
   read dataset wafilename into inputrec.
   if sy-subrc <> '0'.
      exit.
   endif.
   split inputrec at '|' into
       istddist-hbukrs        istddist-emplnum
       istddist-percent       istddist-co_type
       istddist-cost_object.


       ctr = strlen( istddist-cost_object ).
       if ctr = 0.
         move space to istddist-cost_object.
       else.
*         compute ctr = ctr - 1.
         move istddist-cost_object(ctr) to istddist-cost_object.
       endif.



   clear ostddist.
   move-corresponding istddist to ostddist.
   select single * from zfhcc where hbukrs = istddist-hbukrs.
   if sy-subrc = '0'.
      move zfhcc-sbukrs to ostddist-sbukrs.
      append ostddist.
   else.
      write: /1 'Stddist   file Company Code error', istddist+3(40).
   endif.
 enddo.

endform.

form output_labdist.
data: ctr type i.

 do.
   read dataset wafilename into inputrec.
   if sy-subrc <> '0'.
      exit.
   endif.
   split inputrec at '|' into
       ilabdist-payenddte    ilabdist-emplnum
       ilabdist-earnings_cd  ilabdist-hours
       ilabdist-amount       ilabdist-cost_object.
       ctr = strlen( ilabdist-cost_object ).
*       compute ctr = ctr - 1.
       move ilabdist-cost_object(ctr) to ilabdist-cost_object.


   move-corresponding ilabdist to olabdist.

   append olabdist.
 enddo.

endform.


form output_empinfo.
 do.
   read dataset wafilename into inputrec.
    if sy-subrc <> '0'.
      exit.
   endif.
   split inputrec at '|' into
       iemployeeinfo-regioncode   iemployeeinfo-location
       iemployeeinfo-emplnum      iemployeeinfo-home_cc
       iemployeeinfo-hbukrs       iemployeeinfo-jobclass
       iemployeeinfo-deptid       iemployeeinfo-division.
  .
   move-corresponding iemployeeinfo to oemployeeinfo.
   move iemployeeinfo-regioncode to oemployeeinfo-location.
*   move iemployeeinfo-branchcode to oemployeeinfo-location+2(2).
   select single * from zfhcc where hbukrs = iemployeeinfo-hbukrs.
   if sy-subrc = '0'.
      move zfhcc-sbukrs to oemployeeinfo-sbukrs.
      append oemployeeinfo.
   else.
    write: /1 'Employee  file Company Code error', iemployeeinfo+16(25).

   endif.
 enddo.
endform.
*-------------------- DETERMINE_PAYPOSTDATE ---------------------------
*  The Posting Date is the first record in the DEDUCTIONS file
*  (This logic can handles STIP runs)
*-----------------------------------------------------------------------

form determine_paypostdate.
clear wa_postdate.

 loop at odeductns.
   move odeductns-cheque_date to wa_postdate.
   exit.
 endloop.

* Find the first "N" in the file
*loop at odeductns.
*  if odeductns-off_on = 'N'.
*     move odeductns-cheque_date to wa_postdate.
*     exit.
*  endif.
*endloop.

* No "N" in file, so must find first 'Y"
*if wa_postdate is initial.
*   loop at odeductns.
*     if odeductns-off_on = 'Y'.
*        move odeductns-cheque_date to wa_postdate.
*        exit.
*     endif.
*   endloop.
*endif.
endform.

*----------------------  DETERMINE_PAYENDATE ---------------------------
*  Determine payendate for posting into the runid control table
*  All records in file have the same pay end date.
*-----------------------------------------------------------------------

form determine_payenddate.
clear wapayenddate.
loop at odeductns.
  move odeductns-payenddate to wapayenddate.
  exit.
endloop.

endform.



*--------------------  WRITE_FILES  ------------------------------------
* Output processed files
*-----------------------------------------------------------------------
form write_files.
  loop at wafile.
  concatenate p_src '/' wafile-filename+5(30) into wafilename.
  if ( sy-subrc <> 0 ).
    message E002 with wafilename. "msg.
    exit.
  endif.

  open dataset wafilename for output in text mode.

  case wafile-filename+12(30).
    when 'GROS'.
       perform write_grosspay.
    when 'DED'.
       perform write_deductns.
    when 'EARN'.
       perform write_bisearns.
    when 'STND'.
       perform write_stddist.
    when 'LAB'.
       perform write_labdist.
    when 'HRIS'.
       perform write_empinfo.
  endcase.
  endloop.
endform.

form write_grosspay.
  loop at ogrosspay.
  transfer ogrosspay to wafilename.
  endloop.
endform.

form write_deductns.
  loop at odeductns.
     move wa_postdate to odeductns-postdate.
     transfer odeductns to wafilename.
  endloop.
endform.

*----------------------------------------------------------------------
* earnings records that were created from the deduction file, do not
* have the payendate, position_type and full/part time indicator.
* Just before the record is written, any with missing information will
* be filled in from the employee workarea.
*-----------------------------------------------------------------------
form write_bisearns.
  loop at obisearns.
     move wa_postdate to obisearns-postdate.
     move wapayenddate to obisearns-payenddte.
     if obisearns-c_e = 'C'.
       read table oemployeeinfo with key emplnum = obisearns-emplnum.
       if sy-subrc = '0'.
*          move oemployeeinfo-full_part      to obisearns-full_part.
          move oemployeeinfo-jobclass  to  obisearns-jobclass.
       endif.
     endif.
     transfer obisearns to wafilename.
  endloop.
endform.

form write_stddist.
  loop at ostddist.
  transfer ostddist to wafilename.
  endloop.
endform.

form write_labdist.
  loop at olabdist.
  transfer olabdist to wafilename.
  endloop.
endform.

form write_empinfo.
  loop at oemployeeinfo.
  transfer oemployeeinfo to wafilename.
  endloop.
endform.


form calculate_files_totals.
data:  watotalgross(15) type c.
    clear watotalgross.
    loop at ogrosspay.
      add ogrosspay-totalgross to watotalgross.
    endloop.
    compute watotalgross = watotalgross / 100.
    write: /1 'RunId = ', warunid, 50 'BDC Created on:', sy-datum.
    skip 2.

    write: /1 'Total Gross from Ceridian (grosspay.dat)            =',
                                                           watotalgross.
    write: /1 'Less Earnings in Company Codes not in SAP           =',
                                                           wasamount.

data: waearnsgross(15) type c,
      waearnsreg_hrs(15) type c.
     clear: waearnsgross, waearnsreg_hrs.
     loop at obisearns.
       if obisearns-c_e = 'E'.
          add obisearns-reg_earn to waearnsgross.
          add obisearns-reg_hrs  to waearnsreg_hrs.
       endif.
     endloop.
     compute waearnsgross = waearnsgross / 100.
     write: /1 'Total Employee (E) Gross Earnings (bisearns.dat)    =',
                                                           waearnsgross.
     write: /.
endform.

*---------------------------  REPORT_ON_CODES  -----------------------
form report_on_codes.
    loop at odeductns.
      compute wa_amt = odeductns-amount / 100.
      if odeductns-deduction_cd in s_cddis.
      write: /1 odeductns-deduction_cd, ' for employee ',
               odeductns-emplnum, ' on deductions file.  Amount ',
               wa_amt.
      endif.
    endloop.

    loop at obisearns.
      compute wa_amt = obisearns-reg_earn / 100.
      if obisearns-earnings_cd in s_cddis.
        write: /1 obisearns-earnings_cd, ' for employee',
               obisearns-emplnum, ' on earnings file.  Amount ',
               wa_amt..
      endif.
    endloop.

endform.
