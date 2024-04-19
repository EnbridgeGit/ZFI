REPORT  ZFFII035 message-id ZS.

*----------------------------------------------------------------------*
*  Author    : MaryLou De Meester               SAP : East
*  Date      : Sept, 2006                Program Type : Interface
*  Issue Log : TR303
*----------------------------------------------------------------------*
*  Title : Creation of deductions file with code translations
*  Description:
*     For each record in the deduction file, find the employee info
*     merging the information into one output file.  As well pick up
*     accounting information from various tables.
*----------------------------------------------------------------------*
* 2007/09/18 - TR423 - mdemeest - Ceridian payroll changes
* 2006/09/13 - TR302 - mdemeest - New abap based on Sanjeev Chibbers
*                                 specs - as listed in the description
*----------------------------------------------------------------------*

tables:  zfhed,           "Earning Deduction Table
         zfpjd.           "Payroll journal

data: irunid(20)        type c.              "Run id

data:  begin of ideductns  occurs 0,
       payenddate(8)    type c,              "Payend date
       emplnum(6)       type c,              "Employee number
       sbukrs           like zfhcc-sbukrs,   "Company Code
       deduction_cd(3)  type c,              "Deduction code
       c_e(1)           type c,              "Company/Employee Ind
       amount(11)       type c,              "Amount
       cheque_date(8)   type c,              "Cheque Issue Date
       postdate(8)      type c,              "Calculated posting date
       end of ideductns.

data:  begin of iemployeeinfo  occurs 0,
       sbukrs           like zfhcc-sbukrs,   "Company code
       emplnum(6)       type c,              "Employee Number
       regioncode(12)   type c,              "Region code
       deptid(5)        type c,              "Department id
       location(4)      type c,              "Location
       jobclass(2)      type c,              "Job class
       home_cc(10)      type c,              "Home cost centre
       division(4)      type c,              "Division
       end of iemployeeinfo.

data:  begin of odeductns  occurs 0,
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
       osded(1)         type c,              "outsource does A/P

       end of odeductns.


data:  begin of wafile       occurs 0,
       filename(100),
       end of wafile.


field-symbols: <F1>.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
selection-screen begin of block box0 with frame.

selection-screen begin of block box1 with frame title text-100.

parameters: p_dedfl  like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/sapeastded',
            p_empfl  like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/sapeasthris',
            p_runid  like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/runid.tmp'.

selection-screen end of block box1.

selection-screen begin of block box2 with frame title text-101.

parameters: p_odedfl  like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/deductions.dat'.
selection-screen end of block box2.

selection-screen begin of block box3 with frame title text-102.
select-options: s_empl for odeductns-emplnum.
selection-screen end of block box3.




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
  perform read_files.                                 "read into wa
  perform create_output_deduction_file.               "process files
  close dataset: p_dedfl, p_empfl, p_runid, p_odedfl. "close files

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

  open dataset p_empfl for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_empfl. "msg.
    exit.
  endif.

  open dataset p_runid for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_runid. "msg.
    exit.
  endif.
  read dataset p_runid into irunid.

  open dataset p_odedfl for output in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_odedfl. "msg.
    exit.
  endif.



endform.

form read_files.
  do.
    read dataset p_dedfl into ideductns.
    if sy-subrc <> '0'.
       exit.
    endif.
    append ideductns.
  enddo.

  do.
    read dataset p_empfl into iemployeeinfo.
    if sy-subrc <> '0'.
       exit.
    endif.
    append iemployeeinfo.
  enddo.

endform.

form create_output_deduction_file.
  loop at ideductns.
     clear odeductns.
*     move irunid                   to odeductns-runid.
     move irunid+3(9)             to odeductns-runid+0(9).
     move irunid+13(1)            to odeductns-runid+9(1).
     move-corresponding ideductns to odeductns.

* get employee information by matching employee number from deductions
* to employee number on employee file.

  read table iemployeeinfo with key sbukrs  = ideductns-sbukrs
                                    emplnum = ideductns-emplnum.
  if sy-subrc = '0'.
     move-corresponding iemployeeinfo to odeductns.
  endif.

* get codes from Earnings Deduction table
* if code does not exist in ZFEAD table, put the deduction code
* in the G/L account spot so that the client can easily make correction.
     select single * from zfhed
         where   hed = ideductns-deduction_cd
           and coemp = ideductns-c_e.
     if sy-subrc = '0'.
        move zfhed-pjgrp to odeductns-pjgrp.
        move zfhed-ecgrp to odeductns-ecgrp.

* get codes from the Payroll Journal table
* 1. Check for both code & company
* 2. If failure, check only code.
* 3. If 1. or 2. success, move info from table
* 4. Failure in both 1. & 2., move the deduction code to acct number
* this will help in clearing the BDC session created in the next abap
       select single * from zfpjd
          where ecgrp = odeductns-pjgrp
            and sbukrs = odeductns-sbukrs.
          if sy-subrc <> '0'.
             select single * from zfpjd
               where ecgrp = odeductns-pjgrp.
          endif.
       if sy-subrc = '0'.
          move zfpjd-saknr to odeductns-saknr.
          move zfpjd-kostl to odeductns-kostl.
          move zfpjd-aufnr to odeductns-aufnr.
          move zfpjd-osded to odeductns-osded.
       else.

          move ideductns-deduction_cd to odeductns-saknr.
       endif.
     else.
        write: /1 'Deduction code missing from ZFHED',
                   ideductns-deduction_cd,
                   ideductns-c_e.
        move ideductns-deduction_cd to odeductns-saknr.
    endif.
     append odeductns.

     if odeductns-emplnum in s_empl.

        transfer odeductns to p_odedfl.
     endif.
  endloop.


endform.
