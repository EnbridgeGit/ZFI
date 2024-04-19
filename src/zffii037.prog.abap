REPORT  ZFFII037 message-id ZS.

*----------------------------------------------------------------------*
*  Author    : MaryLou De Meester               SAP : East
*  Date      : Oct, 2006                Program Type : Interface
*  Issue Log : TR305
*----------------------------------------------------------------------*
*  Title : Creation of earnings file with code translations
*  Description:
*     For each record in the earnings file, find the employee,
*     standard distribution and labour distribution information
*     merging the information into one output file.  As well pick up
*     accounting information from various tables.
*----------------------------------------------------------------------*
* 2008/06/03 - TR423 - mdemeest - Fix labour distribution followed by
*                                 standard distribution - also hours
*                                 calculation.
* 2006/09/13 - TR305 - mdemeest - New abap based on Sanjeev Chibbers
*                                 specs - as listed in the description
*----------------------------------------------------------------------*

tables:  zfhed,           "Earning Deduction Table
         zfpot,           "Position Type Table
         zfeac,           "Earnings Cost Table
         zfgrd.           "Grade Distribution Table

data:  wapostdate(8)    type c,
       wapayenddate(8)  type c.
data:  begin of where_clause          occurs 0,
       condition(30) type c,
       end of where_clause.


data: irunid(20)        type c.              "Run id
data: acctinginfo(3)    type c.
data: wamethod(1) type c.


*------------------------- LABOUR DISTRIBUTION INPUT -------------------
* This layout matches the file created by ZFFII034
*-----------------------------------------------------------------------
data:  begin of ilabdist  occurs 0,
       payendate(8)     type c,                 "Payend date
       emplnum(6)       type c,                 "Employee Number
       earnings_cd(3)   type c,                 "Earnings code
       hours(6)         type c,                 "Hours
       amount(11)       type c,                 "Amount
       cost_object(25)  type c,                 "Cost Object
       end of ilabdist.

data:  begin of walabdist  occurs 0,
       payendate(8)     type c,                 "Payend date
       emplnum(6)       type c,                 "Employee Number
       earnings_cd(3)   type c,                 "Earnings code
       hours(6)         type c,                 "Hours
       amount(11)        type c,                 "Amount
       cost_object(25)  type c,                 "Cost Object
       prorate_pct(6)   type c,                 "Prorated Percent
       earncd_pct(6)    type c,                 "Earn code Percent
       end of walabdist.


*------------------------- STANDARD DISTRIBUTION INPUT -----------------
* This layout matches the file created by ZFFII034
*-----------------------------------------------------------------------
data:  begin of istddist  occurs 0,
       sbukrs           like zfhcc-sbukrs,      "Company Code
       emplnum(6)       type c,                 "Employee Number
       percent(6)       type c,                 "Percent
       co_type(3)       type c,                 "Cost Object type
       cost_object(25)  type c,                 "Cost Object
       end of istddist.

*------------------------- EARNINGS FILE INPUT -------------------------
* This layout matches the file created by ZFFII034
*----------------------------------------------------------------------
data:  begin of ibisearns occurs 0,
       sbukrs           like zfhcc-sbukrs,      "Company Code from SAP
       emplnum(6)       type c,                 "Employee Number
       earnings_cd(3)   type c,                 "Earnings code
       payenddte(8)     type c,                 "Payend Date
*       postyp           like zfpot-postyp,      "Position type
*       reg_tmp(1)       type c,                 "Reg/Temporary Ind
*       off_on(1)        type c,                 "Off/On cycle indicator
*       full_part(1)     type c,                 "Full/Part time ind
       jobclass(2)      type c,                 "Job class
       reg_hrs(6)       type c,                 "Regular hours
       reg_earn(11)     type c,                 "Regular earnings
       postdate(8)      type c,                 "Calculated Posting Date
       c_e(1)           type c,                 "Company/Employee ind
       end of ibisearns.
*------------------------- EARNINGS FILE OUTPUT ------------------------
* This file used only for checking out results
*-------------------------------------------------------------------
data:  begin of wabisearns occurs 0,
       payenddte(8)     type c,                 "Payend Date
       runid(10)        type c,                 "Run id
       emplnum(6)       type c,                 "Employee Number
       sbukrs           like zfhcc-sbukrs,      "Company Code from SAP
       jobclass(2)      type c,                 "Job Class
       c_e(1)           type c,                 "Company/Employee Ind.
       Loctn            like zfgrd-loctn,      "Location Field
       depmt            like zfgrd-depmt,      "Department
       home_cc(10)      type c,               "Home Cost Centre
       earnings_cd(3)   type c,                 "Earnings code
       reg_earn(11)     type c,                 "Regular earnings
       reg_hrs(6)       type c,                 "Regular hours
       totdisthrs(6)    type c,                 "Total dist hours (calc)
       totdistamt(11)   type c,                 "Total dist amt (calc)
       pjgrp            like zfead-pjgrp,       "Pay Journal Group
       ecgrp            like zfead-ecgrp,       "Earning/Cost
       kstar            like zfeac-kstar,       "GL Account
       cotcd            like zfeac-cotcd,       "Cost Object Code
       coobj            like zfeac-coobj,       "Cost Object Number
       fglcg            like zfeac-fglcg,       "First GL Method
       sglcg            like zfeac-sglcg,       "Second GL Method
       zfgrd_pct(6)     type c,                 "ZFGRD Percent ("G")
       postdate(8)      type c,                 "Calculated Posting Date
       end of wabisearns.

data:  begin of wasumemp      occurs 0,
       emplnum(6)       type c,             "Employee number
       totdisthrs(6)    type c,             "Total REG+OV* hrs from Dist
       totearnhrs(6)    type c,             "Total REG+OV* hrs from Earn
       end of wasumemp.

data:  begin of odistribution   occurs 0,
       payenddte(8)     type c,                 "Payend Date
       runid(10)        type c,                 "Run id
       emplnum(6)       type c,                 "Employee number
       sbukrs           like zfhcc-sbukrs,      "Company code
       jobclass(2)      type c,                 "Job class
       earnings_cd(3)   type c,                 "Earnings code
       amount(11)       type c,                 "Amount
       hours(7)         type c,                 "Regular hours
       kostl            like bbseg-kostl,       "Cost Centre
       aufnr            like bbseg-aufnr,       "Internal Order
       projn            like bbseg-projn,       "WBS
       hkont            like bbseg-hkont,       "GL Account
       postdate(8)      type c,                 "Postdate
       c_e(1)           type c,                 "Company/Employee Ind.
       dist_meth        type c,                 "Distribution MethodF
       Loctn            like zfgrd-loctn,      "Location Field
       depmt            like zfgrd-depmt,      "Department

       end of odistribution.

data:  begin of iemployeeinfo  occurs 0,
       sbukrs           like zfhcc-sbukrs,   "Company code
       emplnum(6)       type c,              "Employee Number
       regioncode(12)   type c,              "Region code
       deptid(5)        type c,              "Department id
       location(4)      type c,              "Location
       jobclass(2)      type c,              "Job class
*       estbht           like zfpot-estbht,   "Establishment area
       home_cc(10)      type c,              "Home cost centre
       division(4)      TYPE C,              "Division
       end of iemployeeinfo.




data:  begin of wafile       occurs 0,
       filename(100),
       end of wafile.


field-symbols: <F1>.
data: wa_reg_hrs(6) type p decimals 2.
data: wa_totdisthrs(6) type p decimals 2.
data: wa_dist_hrs(6)   type p decimals 2.
data: wa_regearn(6)    type p decimals 2.
data: wa_amount(6)     type p decimals 2.
data: wa_hours(6)      type p decimals 2.
data: wa_vhours(6) type c,
      wa_vamt(10)  type c.


*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
selection-screen begin of block box0 with frame.

selection-screen begin of block box1 with frame title text-100.

parameters: p_earn   like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/sapeastearn',
            p_empfl  like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/sapeasthris',
            p_labd   like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/sapeastlab',
            p_stdd   like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/sapeaststnd',
            p_runid  like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/runid.tmp'.

selection-screen end of block box1.

selection-screen begin of block box2 with frame title text-101.

parameters: p_oearns  like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/earnings.dat',
            p_odist   like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI057/distribution.dat'.

selection-screen end of block box2.

selection-screen begin of block box3 with frame title text-102.
select-options:  s_empl for ibisearns-emplnum.
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
  perform create_bisearns_wa.
  perform calculate_totdisthrs_by_empl.
  perform merge_earnings_and_employee.
  perform create_earnings_file.                       "process files
  perform calculations_distribution.
  perform create_line_items.
  perform round_pennies.
  perform write_bisearns.
  perform write_distribution.
  perform final_check.

  close dataset: p_earn, p_oearns, p_odist,            "close files
                 p_empfl, p_labd,  p_stdd.

  write: /1 'End of Processing'.

* HOUSEKEEPING ---------------------------------------------------------
* This routine verifies that each file identified in the .ini file
* exists with the same runid and that payroll runid has not been posted.
* To rerun this job, the runid must be deleted from the runid file
* If the runid has already been posted, the program will abend with the
* message 'JOB CANCELLED - RUN ID HAS ALREADY BEEN POSTED
*-----------------------------------------------------------------------
form open_files.
  open dataset p_earn for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_earn. "msg.
    exit.
  endif.

  open dataset p_empfl for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_empfl. "msg.
    exit.
  endif.

  open dataset p_stdd for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_empfl. "msg.
    exit.
  endif.


  open dataset p_labd for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_empfl. "msg.
    exit.
  endif.

  open dataset p_runid for input in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_runid. "msg.
    exit.
  endif.



  open dataset p_oearns for output in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_oearns. "msg.
    exit.
  endif.

  open dataset p_odist for output in text mode.
  if ( sy-subrc <> 0 ).
    message E002 with p_odist. "msg.
    exit.
  endif.

  read dataset p_runid into irunid.



endform.

form read_files.

  do.
    read dataset p_earn into ibisearns.
    if sy-subrc <> '0'.
       exit.
    endif.
    append ibisearns.
  enddo.

  do.
    read dataset p_empfl into iemployeeinfo.
    if sy-subrc <> '0'.
       exit.
    endif.
    append iemployeeinfo.
  enddo.

  do.
    read dataset p_labd into ilabdist.
    if sy-subrc <> '0'.
       exit.
    endif.
    append ilabdist.
  enddo.
  sort ilabdist by payendate emplnum earnings_cd.

  do.
    read dataset p_stdd into istddist.
    if sy-subrc <> '0'.
       exit.
    endif.
    append istddist.
  enddo.
  sort istddist by sbukrs emplnum.


endform.

form create_bisearns_wa.
  loop at ibisearns to 2.
    move ibisearns-postdate      to wapostdate.
    move ibisearns-payenddte     to wapayenddate.
*    move ibisearns-postdate+4(4) to wapostdate+0(4).
*    move ibisearns-postdate+2(2) to wapostdate+4(2).
*    move ibisearns-postdate+0(2) to wapostdate+6(2).
*    move ibisearns-payenddte+4(4) to wapayenddate+0(4).
*    move ibisearns-payenddte+2(2) to wapayenddate+4(2).
*    move ibisearns-payenddte+0(2) to wapayenddate+6(2).

  endloop.
  loop at ibisearns.
    move-corresponding ibisearns to wabisearns.
*    move irunid                  to wabisearns-runid.
     move irunid+3(9)            to wabisearns-runid+0(9).
     move irunid+13(1)           to wabisearns-runid+9(1).
*    move 0                       to wabisearns-dist_hrs.
    move 0                       to wabisearns-totdisthrs.
    move wapayenddate            to wabisearns-payenddte.
    move wapostdate              to wabisearns-postdate.
    if ibisearns-emplnum in s_empl.
       append wabisearns.
    endif.
  endloop.
endform.


*---------------------  CALCULATE_TOTDISTHRS_BY_EMPL -------------------
form calculate_totdisthrs_by_empl.
* 1.This routine totals all REG & OVERTIME distribution hours for an
*   employee (OV1=003,OV2=004,OV3=010,OV6=006;OV4,OV5 have been
*   eliminated;REG=001) to be used for prorated percents and updates *
*   wasumemp
* 2.This routine totals all distribution hours by employee and
*   earnings code and updates wabisearns internal table.
*
  loop at wabisearns.
*     if wabisearns-earnings_cd = 'REG' or
*        wabisearns-earnings_cd+0(2) = 'OV'.
      if wabisearns-earnings_cd = '001' or
         wabisearns-earnings_cd = '003' or
         wabisearns-earnings_cd = '004' or
         wabisearns-earnings_cd = '006' or
         wabisearns-earnings_cd = '010'.
           read table wasumemp with key emplnum = wabisearns-emplnum.
           if sy-subrc = '0'.
              add wabisearns-reg_hrs to wasumemp-totearnhrs.
              modify wasumemp INDEX sy-tabix.
           else.
              move wabisearns-emplnum to wasumemp-emplnum.
              move 0                  to wasumemp-totdisthrs.
              move wabisearns-reg_hrs to wasumemp-totearnhrs.
              append wasumemp.
           endif.
     endif.
  endloop.

  loop at ilabdist.
*     if ilabdist-earnings_cd = 'REG' or   CERIDIAN
*        ilabdist-earnings_cd+0(2) = 'OV'. REPLACEMENT
      if wabisearns-earnings_cd = '001' or
         wabisearns-earnings_cd = '003' or
         wabisearns-earnings_cd = '004' or
         wabisearns-earnings_cd = '006' or
         wabisearns-earnings_cd = '010'.
           read table wasumemp with key emplnum = ilabdist-emplnum.
           if sy-subrc = '0'.
              add ilabdist-hours to wasumemp-totdisthrs.
              modify wasumemp INDEX sy-tabix.
           else.
              move ilabdist-emplnum to wasumemp-emplnum.
              move ilabdist-hours   to wasumemp-totdisthrs.
              move 0                to wasumemp-totearnhrs.
              append wasumemp.
           endif.
     endif.

     read table wabisearns with key emplnum = ilabdist-emplnum
                                    earnings_cd = ilabdist-earnings_cd.
     if sy-subrc = '0'.
         add ilabdist-hours  to wabisearns-totdisthrs.
         add ilabdist-amount to wabisearns-totdistamt.
         modify wabisearns INDEX sy-tabix.
     endif.
  endloop.
endform.
*-----------------------------------------------------------------------

form create_earnings_file.
  loop at wabisearns.

    select single * from zfhed where hed = wabisearns-earnings_cd
                                 and coemp = wabisearns-c_e.
    if sy-subrc = '0'.
       move zfhed-pjgrp to wabisearns-pjgrp.
       move zfhed-ecgrp to wabisearns-ecgrp.
*    else.
*       move wabisearns-earnings_cd to wabisearns-coobj.
*       move 'IO'                   to wabisearns-cotcd.
    endif.

    if wabisearns-pjgrp = 'EARNING'.

       select single * from zfeac where eagrp = wabisearns-ecgrp
                    and ( sbukrs = wabisearns-sbukrs or sbukrs = ' ' ).
       if sy-subrc = '0'.
          case zfeac-pcetab.
            when 'E'.
              move zfeac-kstar   to wabisearns-kstar.
              move zfeac-cotcd   to wabisearns-cotcd.
              move zfeac-coobj   to wabisearns-coobj.
              move zfeac-fglcg   to wabisearns-fglcg.
              move zfeac-sglcg   to wabisearns-sglcg.
            when 'P'.
              perform get_zfpot.
              if sy-subrc = '0'.
                 move zfpot-fglcg   to wabisearns-fglcg.
                 move zfpot-sglcg   to wabisearns-sglcg.
                 case zfpot-cotab.
                   when 'G'.
                    move zfpot-pkstar to wabisearns-kstar.
                    perform get_zfgrd.
*                    select single * from ZFGRD where
*                          ( ecgrp = wabisearns-ecgrp or ecgrp = ' ' )
*                     and ( estbht = wabisearns-estbht or estbht = ' ' )
*                      and ( loctn  = wabisearns-loctn  or loctn = ' ' )
*                     and ( depmt  = wabisearns-depmt  or depmt = ' ' ).
                    if sy-subrc = '0'.
                       move 'G'           to wabisearns-fglcg.
                       move zfgrd-cotcd   to wabisearns-cotcd.
                       move zfgrd-coobj   to wabisearns-coobj.
                       move zfgrd-prctg   to wabisearns-zfgrd_pct.
                    endif.
                   when OTHERS.
                    move zfpot-pkstar  to wabisearns-kstar.
                    move zfpot-cotcd   to wabisearns-cotcd.
                    move zfpot-coobj   to wabisearns-coobj.
                 endcase.                           "end of COTAB case
              endif.
            when OTHERS.
              write: /1 'zfeac-pcetab = ', 'OTHERS'.
          endcase.                                  "end of PCETAB case
       endif.                                       "End of ZFEAC select

    endif.                                   "End of EARNING test
    modify wabisearns.
  endloop.
endform.

form merge_earnings_and_employee.
  loop at wabisearns.
    read TABLE iemployeeinfo with key sbukrs = wabisearns-sbukrs
                                      emplnum = wabisearns-emplnum.
    if sy-subrc = '0'.
       move iemployeeinfo-deptid        to wabisearns-depmt.
*       move iemployeeinfo-estbht        to wabisearns-estbht.
       move iemployeeinfo-location      to wabisearns-loctn.
       move iemployeeinfo-home_cc       to wabisearns-home_cc.
       modify wabisearns.
    endif.
  endloop.
endform.

form write_bisearns.
  loop at wabisearns.
    transfer wabisearns to p_oearns.
  endloop.
endform.

form write_distribution.
  sort odistribution by sbukrs hkont emplnum.
  loop at odistribution.
      transfer odistribution to p_odist.
  endloop.
endform.


form calculations_distribution.

  loop at ilabdist.
    at new emplnum.
       read TABLE wasumemp with key emplnum = ilabdist-emplnum.
    endat.
    move-corresponding ilabdist to walabdist.
    move 0 to: walabdist-prorate_pct, walabdist-earncd_pct.

* Prorate percent
     read table wabisearns with key emplnum = ilabdist-emplnum
                                    earnings_cd = ilabdist-earnings_cd.
     if sy-subrc = '0'.
        if wasumemp-totdisthrs = 0.
           move 0 to walabdist-prorate_pct.
*        elseif ( walabdist-earnings_cd = 'REG' or     CERIDIAN
*             walabdist-earnings_cd(2) = 'OV' ).       REPLACEMENT
         elseif wabisearns-earnings_cd = '001' or
         wabisearns-earnings_cd = '003' or
         wabisearns-earnings_cd = '004' or
         wabisearns-earnings_cd = '006' or
         wabisearns-earnings_cd = '010'.
             compute walabdist-prorate_pct =
                     walabdist-hours / wasumemp-totdisthrs.
             else.
                 move  0 to walabdist-prorate_pct.
        endif.
     endif.
* Earnings code percent
    read table wabisearns with key emplnum = ilabdist-emplnum
                                   earnings_cd = ilabdist-earnings_cd.
    if sy-subrc = '0'.
       if wabisearns-totdisthrs = 0.
          compute walabdist-earncd_pct =
                         walabdist-amount * 100 / wabisearns-totdistamt.
       else.
          if wabisearns-reg_hrs > wabisearns-totdisthrs.
             compute walabdist-earncd_pct =
                        walabdist-hours * 100 / wabisearns-reg_hrs.
          else.
             compute walabdist-earncd_pct =
                          walabdist-hours * 100 / wabisearns-totdisthrs.
          endif.
       endif.
    endif.

    append walabdist.
  endloop.

endform.

form create_line_items.

loop at wabisearns.
  move '   '   to acctinginfo.
  compute wa_reg_hrs = wabisearns-reg_hrs / 100.
  compute wa_totdisthrs = wabisearns-totdisthrs / 100.
  compute wa_regearn = wabisearns-reg_earn / 100.
  case wabisearns-fglcg.
    when 'V'.           perform V_C.
    when 'E'.           perform distribution_method_E.
    when 'G'.           perform distribution_method_G.
    when 'F'.           perform distribution_method_F.
    when 'C'.           perform V_C.
    when 'H'.           perform distribution_method_H.
    when 'P'.           perform distribution_method_P.
    when  OTHERS.       perform distribution_method_H.

  endcase.
  if acctinginfo = '   '.
     case wabisearns-sglcg.
       when 'V'.           perform V_C.
       when 'E'.           perform distribution_method_E.
       when 'G'.           perform distribution_method_G.
       when 'F'.           perform distribution_method_F.
       when 'C'.           perform V_C.
       when 'H'.           perform distribution_method_H.
       when 'P'.           perform distribution_method_P.
       when OTHERS.        perform distribution_method_H.

     endcase.
  endif.
  if acctinginfo = '   '.
     perform distribution_method_H.
  endif.
endloop.
endform.

form base_distribution.
   clear odistribution.
   move wabisearns-payenddte   to odistribution-payenddte.
   move wapostdate             to odistribution-postdate.
   move wabisearns-runid       to odistribution-runid.
   move wabisearns-emplnum     to odistribution-emplnum.
   move wabisearns-sbukrs      to odistribution-sbukrs.
   move wabisearns-jobclass    to odistribution-jobclass.
   move wabisearns-earnings_cd to odistribution-earnings_cd.
   if wabisearns-kstar = space.
      move wabisearns-earnings_cd to odistribution-hkont.
      write: / 'Earnings code to be mapped is', wabisearns-earnings_cd.
   else.
      move wabisearns-kstar       to odistribution-hkont.
   endif.
   move wabisearns-c_e         to odistribution-c_e.
   move wabisearns-fglcg       to odistribution-dist_meth.
   if wabisearns-fglcg = 'H'.
      move wabisearns-home_cc     to odistribution-kostl.
   endif.

   case wabisearns-cotcd.
       when 'WBS'.
          move wabisearns-coobj     to odistribution-projn.
       when 'IO'.
          move wabisearns-coobj     to odistribution-aufnr.
       when 'CC'.
          move wabisearns-coobj     to odistribution-kostl.

   endcase.
endform.

form distribution_method_P.
* proration requires regular and/or overtime hours.  If neither, then
* use the secondary method.
      clear wamethod.
      perform base_distribution.
      read table walabdist with key emplnum = wabisearns-emplnum.
      loop at walabdist from sy-tabix
           where emplnum = wabisearns-emplnum.
         if walabdist-earnings_cd = '001' or
            walabdist-earnings_cd = '003' or
            walabdist-earnings_cd = '004' or
            walabdist-earnings_cd = '006' or
            walabdist-earnings_cd = '010'.
            move 'F' to wamethod.
         endif.
      endloop.

data:  wadec type p.
data:  wazero(6)  type c  value '0.0000'.
data:  wapct   type p  decimals 4.
data:  wahours type p decimals 3.
      read table walabdist with key emplnum = wabisearns-emplnum.
      if sy-subrc = '0' and wamethod = 'F'.
         loop at walabdist from sy-tabix
                 where emplnum = wabisearns-emplnum.
          move walabdist-prorate_pct to wapct.
          if wapct = wazero.
          else.
           compute wa_amount = wa_regearn * wapct.
           move wa_amount to odistribution-amount.
           compute wa_hours = wa_reg_hrs * wapct.
           move wa_hours to odistribution-hours.
           perform move_labour_accounting.
           append odistribution.
           move 'YES' to acctinginfo.
          endif.
         endloop.
*      else.
*         case wabisearns-sglcg.
*            when 'E'. perform distribution_method_E.
*            when 'H'. perform distribution_method_H.
*
*         endcase.
     endif.
endform.


form distribution_method_F.
      perform base_distribution.
      compute wa_amount = wa_regearn.
      move wa_amount to odistribution-amount.
      move wa_reg_hrs to odistribution-hours.
      append odistribution.
      move 'YES' to acctinginfo.

endform.

form distribution_method_G.
      perform get_zfgrd.
      perform base_distribution.
      compute wa_amount = ( wa_regearn * wabisearns-zfgrd_pct / 100 ).
      move wa_amount to odistribution-amount.
      move wa_reg_hrs to odistribution-hours.
      move wabisearns-loctn  to odistribution-loctn.
      move wabisearns-depmt  to odistribution-depmt.
      append odistribution.
      move 'YES' to acctinginfo.

endform.

form V_C.

*  compute wa_reg_hrs = wabisearns-reg_hrs / 100.
*  compute wa_totdisthrs = wabisearns-totdisthrs / 100.
*  compute wa_regearn = wabisearns-reg_earn / 100.

  read table walabdist with key emplnum = wabisearns-emplnum
                                earnings_cd = wabisearns-earnings_cd.
  if sy-subrc = '0'.
     if ( wa_reg_hrs = wa_totdisthrs ) or
        ( wa_reg_hrs < wa_totdisthrs ).
          loop at walabdist from sy-tabix
                where emplnum = wabisearns-emplnum.
             if wabisearns-earnings_cd = walabdist-earnings_cd.
                perform base_distribution.
                perform move_labour_accounting.
                compute wa_amount
                             = walabdist-earncd_pct * wa_regearn / 100.
                move wa_amount to odistribution-amount.
                compute odistribution-hours = walabdist-hours / 100.
                append odistribution.
                move 'YES' to acctinginfo.

             endif.
          endloop.                               "end of walabdist loop
      elseif wa_reg_hrs > wa_totdisthrs.
      clear: wa_vhours, wa_vamt.
         loop at walabdist from sy-tabix
                       where emplnum = wabisearns-emplnum.
           if wabisearns-earnings_cd = walabdist-earnings_cd.
             perform base_distribution.
             perform move_labour_accounting.
             compute wa_amount
                             = walabdist-earncd_pct * wa_regearn / 100.
             compute wa_hours = walabdist-earncd_pct * wa_reg_hrs / 100.
             move wa_amount to odistribution-amount.
             move wa_hours to odistribution-hours.
             append odistribution.
             move '   ' to acctinginfo.              "2008/06/03
*            move 'YES' to acctinginfo.              "2008/06/03
             add wa_amount to wa_vamt.
             add wa_hours  to wa_vhours.
            endif.
          endloop.
          compute wa_regearn  = wa_regearn - wa_vamt.
          compute wa_reg_hrs = wa_reg_hrs - wa_vhours.
       endif.

  endif.
endform.

form distribution_method_E.
data:  wa_hrs like wa_hours.
      read table istddist with key emplnum = wabisearns-emplnum.
      if sy-subrc = '0'.
         loop at istddist from sy-tabix
            where emplnum = wabisearns-emplnum.
            move istddist-co_type     to wabisearns-cotcd.
            move istddist-cost_object to wabisearns-coobj.
            perform base_distribution.
            move 'E' to odistribution-dist_meth.
               compute wa_amount
                  = ( wa_regearn * istddist-percent / 100 ).
               move wa_amount to odistribution-amount.
               compute wa_hrs
                  = ( wa_reg_hrs * istddist-percent / 100 ).
               move wa_hrs     to odistribution-hours.
*              move wa_reg_hrs to odistribution-hours.
               append odistribution.
               move 'YES' to acctinginfo.
         endloop.
       endif.
endform.

form distribution_method_H.
      perform base_distribution.
      move 'H' to odistribution-dist_meth.
      compute wa_amount = wa_regearn.
      move wa_amount to odistribution-amount.
      move wa_reg_hrs to odistribution-hours.
      move wabisearns-home_cc to odistribution-kostl.

      append odistribution.
      move 'YES' to acctinginfo.

endform.

form round_pennies.
data:  wa_round(10) type c.
data:  wa_amt(11)   type c.
data:  wa_tmp_earnsamt(10) type c.
data:  wa_earnings_cd(6) type c.
   sort odistribution by sbukrs emplnum earnings_cd amount.
   clear wa_round.
   loop at odistribution.
     move odistribution-amount to wa_amt.
     move odistribution-earnings_cd to wa_earnings_cd.
     at new emplnum.
       clear wa_round.
     endat.
     at new earnings_cd.
       clear wa_round.
     endat.
     add odistribution-amount to wa_round.
     at end of earnings_cd.
       read table wabisearns with key emplnum = odistribution-emplnum
                               earnings_cd = odistribution-earnings_cd.
       compute wa_tmp_earnsamt = wabisearns-reg_earn / 100.
         if wa_round <> wa_tmp_earnsamt.
             compute wa_amt = wa_amt +  wa_tmp_earnsamt - wa_round.
         endif.
     endat.
     move wa_amt to odistribution-amount.
     modify odistribution.
   endloop.
endform.

form move_labour_accounting.
data:  length(4)  type n.
           compute length = strlen( walabdist-cost_object ).
           clear: odistribution-kostl,
                  odistribution-aufnr,
                  odistribution-projn.
           case length.
             when '5'.
               move walabdist-cost_object to odistribution-kostl.
             when '6'.
               move walabdist-cost_object to odistribution-aufnr.
             when OTHERS.
                if length = 0.
                    write: /1 'no accounting available'.
                    move wabisearns-home_cc to odistribution-kostl.
                else.
                    move walabdist-cost_object to odistribution-projn.
                endif.
             endcase.

 endform.

*------------------------  FINAL CHECK --------------------------------
*  This routine compares the sum of the Earnings Amount file (input)
*  and the Distribution file (output) based on the range of employee
*  numbers entered.  To determine the employee in error, keep
*  decreasing the range of employee numbers.
*-----------------------------------------------------------------------
form final_check.
data:  wabisamte(15) type c.
data:  wabisamtc(15) type c.
data:  wabisamtt(15) type c.

data:  wadistamt(15) type c.
  clear: wabisamte, wabisamtc.
  loop at ibisearns.
     if ibisearns-emplnum in s_empl.
        if ibisearns-c_e = 'E'.
          compute wabisamte = wabisamte + ibisearns-reg_earn.
     else.
          compute wabisamtc = wabisamtc + ibisearns-reg_earn.
        endif.
     endif.
  endloop.
  clear wadistamt.
  loop at odistribution.
    compute wadistamt = wadistamt + odistribution-amount.
  endloop.
  compute wabisamte = wabisamte / 100.
  compute wabisamtc = wabisamtc / 100.
  compute wabisamtt = wabisamte + wabisamtc.
  write: /1 'Input earnings (E) = ', wabisamte.
  write: /1 'Input earnings (C) = ', wabisamtc.
  write: /1 'Calculated Total   = ', wabisamtt.
  write: /1 'For Distribution   = ', wadistamt.
endform.

*-----------------------------------------------------------------------
*  ZFPOT will always have ecgrp & postyp filled in
*  Company Code (bukrs) and Establishment Area (estbht) optional.
*  This routine checks all fields and if failure, then removes one of
*  the optional fields.  There will always be a default record with only
*  the Earnings Group & Position Number filled in.
*-----------------------------------------------------------------------
form get_zfpot.
   select single * from ZFPOT where eagrp = wabisearns-ecgrp
                                and ( postyp = wabisearns-jobclass )
                                and ( bukrs  = wabisearns-sbukrs ).
*                                and ( estbht = wabisearns-estbht ).
   if sy-subrc = '0'.
   else.
   select single * from ZFPOT where eagrp = wabisearns-ecgrp
                                and ( postyp = wabisearns-jobclass ).
*                                and ( estbht = wabisearns-estbht ).
   if sy-subrc = '0'.
   else.
   select single * from ZFPOT where eagrp = wabisearns-ecgrp
                                and ( postyp = wabisearns-jobclass ).
   if sy-subrc = '0'.
   else.
     write: /1 wabisearns-ecgrp, wabisearns-jobclass, wabisearns-sbukrs.
*     wabisearns-estbht, 'NO ENTRY IN TABLE'.
   endif.
   endif.
   endif.
endform.

form get_zfgrd.
    select single * from ZFGRD where ecgrp = wabisearns-ecgrp
*                                 and estbht = wabisearns-estbht
                                 and loctn  = wabisearns-loctn
                                 and depmt  = wabisearns-depmt.
    if sy-subrc = 0.
       perform zfgrd_acct.
    else.
    select single * from ZFGRD where ecgrp = wabisearns-ecgrp
*                                 and estbht = wabisearns-estbht
                                 and loctn  = wabisearns-loctn.
    if sy-subrc = '0'.
       perform zfgrd_acct.
    else.
    select single * from ZFGRD where ecgrp = wabisearns-ecgrp
                                 and loctn  = wabisearns-loctn
                                 and depmt  = wabisearns-depmt.
    if sy-subrc = '0'.
       perform zfgrd_acct.
    else.
    select single * from ZFGRD where ecgrp = wabisearns-ecgrp
                                 and loctn  = wabisearns-loctn.
    if sy-subrc = '0'.
       perform zfgrd_acct.
    else.
    select single * from ZFGRD where ecgrp = wabisearns-ecgrp.
    if sy-subrc = '0'.
       perform zfgrd_acct.
    else.
    select single * from ZFGRD where ecgrp = ' '
                                 and estbht = ' '
                                 and loctn = ' '
                                 and depmt = ' '.
    if sy-subrc = '0'.
       move zfgrd-cotcd to wabisearns-cotcd.
       move zfgrd-coobj to wabisearns-coobj.
       move zfgrd-prctg to wabisearns-zfgrd_pct.
    else.
       write: /1 wabisearns-emplnum, wabisearns-ecgrp,
*                 wabisearns-estbht,
                 wabisearns-loctn.
    endif.          "all 4 fields spaces
    endif.          "matching ecgrp
    endif.          "matching group & location
    endif.          "matching group, location, dept
    endif.          "matching group, estbht, location
    endif.          "matching all 4 fields

endform.

form zfgrd_acct.
    move zfgrd-cotcd to wabisearns-cotcd.
    move zfgrd-coobj to wabisearns-coobj.
    move zfgrd-prctg to wabisearns-zfgrd_pct.
endform.

*form look_at_secondary.
*   case wabisearns-sglcg.
*      when 'V'.     perform V_C.
*      when 'C'.     perform V_C.
*      when 'E'.     perform distribution_method_E.
*      when 'G'.     perform distribution_method_G.
*      when 'F'.     perform distribution_method_F.
*      when 'H'.     perform distribution_method_H.
*      when  OTHERS. perform distribution_method_H.
*   endcase.
*endform.
