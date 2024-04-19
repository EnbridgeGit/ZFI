REPORT  ZFFII031 message-id ZS NO STANDARD PAGE HEADING
                              LINE-SIZE 130
                              LINE-COUNT 65.

*----------------------------------------------------------------------*
*  Author    : Adel D. Mendoza                    SAP : East
*  Date      : July, 2006                Program Type : Interface
*  Issue Log : TR150
*----------------------------------------------------------------------*
*  Title : Mercator Replacement - IFFI056 Customer Count Interface
*  Description:
*     - Create two ABAP programs to replace 3 Mercator mapping
*     - programs and 2 C++ scripts. The first ABAP will format the
*     - file, sort and remove duplicate records. The second ABAP will
*     - format the file for SAP posting. The ABAP should also report
*     - the count automatically and either print or e-mail the report
*     - to the client.
*----------------------------------------------------------------------*
*  2006/07/26 - gymana - Made coding corrections as a result of unit
*                        testing.
*  2007/09/10 - gymana - M1/M2 rate class split. Adding rate class to
*                        input & output file layouts.
*  2008/03/19 - gymana - TR547: Added premise to file layout.
*  2012/08/21 - gymana - SDP23236 (Q159): Commented out logic to change
*                        the bill date of all cycle 1 recs when the
*                        cycle 1 bill date occurs late in the previous
*                        month.
*----------------------------------------------------------------------*
tables: t247.

data: begin of wa_hdr,
        CYC01YYYY(04)    type C,  "Billing Cycle 01 Year
        CYC01MM(02)      type C,  "Billing Cycle 01 Month
        CYC01DD(02)      type C,  "Billing Cycle 01 Day
        fa(1)            type C,
        CYC20YYYY(04)    type C,  "Billing Cycle 20 Year
        CYC20MM(02)      type C,  "Billing Cycle 20 Month
        CYC20DD(02)      type C.  "Billing Cycle 20 Day
data: end of wa_hdr.

data: begin of wa_dtl,
        fa(3)            type C,
        CUSTOMER(07)     type C,  "Banner customer number
        fb(1)            type C,
        PREMISE(07)      type C,  "Banner premise number
        fc(1)            type C,
        GLCL(04)         type C,  "GL class code
        fd(1)            type C,
        SCLS(02)         type C,  "Service class code
        fe(1)            type C,
        RATECLS(04)      type C,  "Rate class code
        ff(1)            type C,
        GECA(06)         type C,  "Geographical area code
        fg(1)            type C,
        TRANDATE_DAY(02) type C,  "Banner billing day (01-31)
        fh(1)            type C,
        TRANDATE_MON(03) type C,  "Banner billing month (JAN-DEC)
        fi(1)            type C,
        TRANDATE_YR(02)  type C,  "Banner billing year ('06)
        fj(1)            type C,
        CUSTTYPE(04)     type C,  "Customer type
        fk(1)            type C,
        CREDIT(01)       type C.  "Credit / Debit indicator
data: end of wa_dtl.

data: begin of wa_out occurs 0,
        CUSTOMER(07)     type C,  "Banner customer number
        PREMISE(07)      type C,  "Banner premise number
        GLCL(04)         type C,  "GL class code
        SCLS(02)         type C,  "Service class code
        RATECLS(04)      type C,  "Rate class code
        GECA(06)         type C,  "Geographical area code
        TRANDATE(08)     type C,  "Transaction Date
        CUSTTYPE(04)     type C,  "Customer type
        CREDIT(01)       type C.  "Credit / Debit indicator
data: end of wa_out.

data: begin of wa_nod occurs 0,
        CUSTOMER(07)     type C,  "Banner customer number
        PREMISE(07)      type C,  "Banner premise number
        GLCL(04)         type C,  "GL class code
        SCLS(02)         type C,  "Service class code
        RATECLS(04)      type C,  "Rate class code
        GECA(06)         type C,  "Geographical area code
        TRANDATE(08)     type C.  "Transaction Date ('01312006')
data: end of wa_nod.

data: begin of wa_grp occurs 0,
        PREMISE(07)      type C,  "Banner premise number
        GLCL(04)         type C,  "GL class code
        SCLS(02)         type C,  "Service class code
        RATECLS(04)      type C,  "Rate class code
        GECA(06)         type C,  "Geographical area code
        TRANDATE(08)     type C,  "Transaction Date
        GRPCNT(8)        type C.
data: end of wa_grp.

data: wa_premise(07) type c.
data: wa_glcl(04)    type c.
data: wa_scls(02)    type c.
data: wa_ratecls(04) type c.
data: wa_geca(06)    type c.
data: wa_date(08)    type c.
data: wa_cntr        type i.

data: cycle01        like sy-datum(6).
data: cycle20        like sy-datum(6).
data: transdt        like sy-datum(6).
data: transyr        like sy-datum(4).
data: monthno        like t247-mnr.
data: banmthno       like wa_hdr-CYC20MM.
data: banyear        like wa_hdr-CYC20YYYY.
data: banmonth       like t247-ltx.
data: banmonth3      like t247-ktx.
data: trandt_dd(02)   type c.
data: trandt_mm(02)   type c.
data: trandt_yr(04)   type c.

data: recread    type i.
data: recwrit    type i.

data: grpread    type i.
data: grpwrit    type i.
data: grpcntr    type i.
data: grpcount(08)    type c.

data: delimeter value '09' type X.
data: inrec(400), outrec(2000), infile(70), outfile(70), cntrec(80).
data: wrk_symbolic(4) type C value '$sys'.

data: countfile like filename-fileextern.

*----------------------------------------------------------------------
* selection screen
*----------------------------------------------------------------------
selection-screen begin of block box0 with frame.
selection-screen begin of block box1 with frame title text-100.

PARAMETERS: p_file1 like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/BANNER/ccnthdr.dat',

            p_file2 like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/BANNER/ccntdat.dat'.

selection-screen skip.
PARAMETERS: p_fileot like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/BANNER/bancount.dat'.

selection-screen end of block box1.
selection-screen end of block box0.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
at selection-screen output.
  replace wrk_symbolic with sy-sysid into: p_file1, p_file2, p_fileot.
  condense: p_file1 no-gaps, p_file2 no-gaps, p_fileot no-gaps.

*----------------------------------------------------------------------
* start-of-selection
*----------------------------------------------------------------------
START-OF-SELECTION.
  perform OPEN_FILES.
  perform F_REMOVE_DUPLICATES.
  perform F_GROUP_COUNTS.
  perform F_REPORT.
  close dataset: p_file1, p_file2, p_fileot.


*----------------------------------------------------------------------
* form F_REPORT
*----------------------------------------------------------------------
form F_REPORT.
  data: msg(100).

  select single * from t247 where SPRAS = 'EN'
                            and MNR = banmthno.
  move t247-ltx to banmonth.
  move t247-ktx to banmonth3.
  shift banmonth right deleting trailing space.

  format intensified on.
  write: /3  'ZFFII031',
          40 'BANNER CUSTOMER COUNT REPORT'.
  write: 100 'DATE:', sy-datum, '@', sy-uzeit.
  write: /3 'IFFI056',
          45 banmonth, banyear.
  format intensified off.
  skip .
  write: /30 'Banner Records read   :',
          67 recread.
  write: /30 'Records written to SAP:',
          67 grpwrit.

  skip.
  format intensified on.
  shift banmonth left deleting leading space.
  write: /30 'Customer count for', banmonth,
          59 ':',
          67 grpcntr.
  format intensified off.

*  format intensified on.
*  write: /001 'REMOVE DUPLICATES:'.
*  format intensified off.
*  write: /003 'Records read   :', recread.
*  write: /003 'Records written:', recwrit.
*  skip 2.
*  format intensified on.
*  write: /001 'GROUP COUNT:'.
*  format intensified off.
*  write: /003 'Records read   :', grpread.
*  write: /003 'Records written:', grpwrit.
*  write: /003 'Group count    :', grpcntr.

* Write count to a file for historical purposes.

  concatenate '/usr/sap/interfaces/P01/BANNER/custcnt_'
               banyear banmthno banmonth3 '.txt' into countfile.

  open dataset countfile for output in text mode message msg.
  if ( sy-subrc <> 0 ).
    message E002 with outfile msg.
  endif.
  move grpcntr to grpcount.
  concatenate banyear banmthno grpcount into cntrec.
  transfer cntrec to countfile.

  close dataset: countfile.
endform.                    "F_REPORT

*----------------------------------------------------------------------
* form F_GROUP_COUNTS
*----------------------------------------------------------------------
form F_GROUP_COUNTS.
  refresh wa_grp. clear wa_grp.
  grpread = 0. grpwrit = 0. grpcntr = 0. wa_cntr = 0.
*  sort wa_nod by glcl scls ratecls geca trandate.
  sort wa_nod by glcl scls geca trandate.
  loop at wa_nod.
    add 1 to grpread.
*    if ( ( wa_glcl <> wa_nod-glcl ) or ( wa_scls <> wa_nod-scls ) or
*        ( wa_ratecls <> wa_nod-ratecls ) or
*        ( wa_geca <> wa_nod-geca ) or ( wa_date <> wa_nod-trandate ) ).
    if ( ( wa_glcl <> wa_nod-glcl ) or ( wa_scls <> wa_nod-scls ) or
       ( wa_geca <> wa_nod-geca ) or ( wa_date <> wa_nod-trandate ) ).
      if wa_cntr > 0.
        add 1 to grpwrit.
        add wa_cntr to grpcntr.
        move wa_premise    to wa_grp-PREMISE.
        move wa_glcl       to wa_grp-GLCL.
        move wa_scls       to wa_grp-SCLS.
        move wa_ratecls    to wa_grp-RATECLS.
        move wa_geca       to wa_grp-GECA.
        move wa_date       to wa_grp-trandate.
        move wa_cntr       to wa_grp-GRPCNT.
        append wa_grp. clear wa_grp.
      endif.
      move wa_nod-premise to wa_premise.
      move wa_nod-glcl to wa_glcl.
      move wa_nod-scls to wa_scls.
      move wa_nod-ratecls to wa_ratecls.
      move wa_nod-geca to wa_geca.
      move wa_nod-trandate to wa_date.
      wa_cntr = 1.
    else.
      add 1 to wa_cntr.
    endif.
  endloop.

* write last record and update final counts

  add 1 to grpwrit.
  add wa_cntr to grpcntr.
  move wa_premise    to wa_grp-PREMISE.
  move wa_glcl       to wa_grp-GLCL.
  move wa_scls       to wa_grp-SCLS.
  move wa_ratecls    to wa_grp-RATECLS.
  move wa_geca       to wa_grp-GECA.
  move wa_date       to wa_grp-trandate.
  move wa_cntr       to wa_grp-GRPCNT.
  append wa_grp. clear wa_grp.

*- create output file
  loop at wa_grp.
    TRANSFER wa_grp to P_FILEOT.
  endloop.
endform.                    "F_GROUP_COUNTS

*----------------------------------------------------------------------
* form F_REMOVE_DUPLICATES
*----------------------------------------------------------------------
form F_REMOVE_DUPLICATES.
*- extract header file
  clear: wa_hdr, INREC.
  read dataset p_file1 into INREC.
  if ( INREC is initial or sy-subrc <> 0 ).
    message E019 with infile 'Header file is empty!'.
    stop.
  else.
    move INREC to wa_hdr.
  endif.
  move wa_hdr+0(6) to cycle01.
  move wa_hdr+9(6) to cycle20.
  move wa_hdr-CYC20MM to banmthno.
  move wa_hdr-CYC20YYYY to banyear.

*- extract detail file
  refresh wa_out.
  do.
    clear: wa_dtl, INREC.
    read dataset p_file2 into INREC.
    if ( INREC is initial or sy-subrc <> 0 ).
      exit.
    else.
      move INREC to wa_dtl.
      translate wa_dtl-geca using ' 0'.
      move-corresponding wa_dtl to wa_out.

      transyr = wa_dtl-TRANDATE_YR + 2000.
      perform get_month_num using wa_dtl-TRANDATE_MON
          changing monthno.
      concatenate transyr monthno into transdt.

*-    change transaction day
      move wa_dtl-trandate_day to trandt_dd.
*      if cycle01 < cycle20.                                      "Q159
*        if transdt < cycle20.                                    "Q159
*          move '01' to trandt_dd.                                "Q159
*        endif.                                                   "Q159
*      endif.                                                     "Q159

*-    change transaction month
      move monthno to trandt_mm.
*      if cycle01 < cycle20.                                      "Q159
*        if transdt < cycle20.                                    "Q159
*          move wa_hdr-cyc20mm to trandt_mm.                      "Q159
*        endif.                                                   "Q159
*      endif.                                                     "Q159

*-    change year
      move transyr to trandt_yr.
      concatenate trandt_mm trandt_dd trandt_yr into wa_out-trandate.
      append wa_out. clear wa_out.
    endif.
  enddo.

*- sort and remove duplicates
  refresh wa_nod. clear: wa_nod.
  recread = 0. recwrit = 0.
  sort wa_out by customer premise trandate glcl scls.

*  Uncomment this code to create a zbis103.srt file for
*  comparison. Make sure you change the outfile name

*  loop at wa_out.
*     transfer wa_out to p_fileot.
*  endloop.

  loop at wa_out.
    add 1 to recread.
    if ( ( wa_nod-customer <> wa_out-customer ) or
         ( wa_nod-premise  <> wa_out-premise  ) ).
      add 1 to recwrit.
      move-corresponding wa_out to wa_nod.
      append wa_nod.
    endif.
  endloop.

*  Uncomment this code to create a zbis103.urd file for
*  comparision. Make sure you change the outfile name

*  loop at wa_nod.
*     transfer wa_nod to p_fileot.
*  endloop.
endform.                    "F_REMOVE_DUPLICATES

*----------------------------------------------------------------------
* get Month (Number eg. 01 to 12)
*----------------------------------------------------------------------
form get_month_num using Imotxt changing Emonthno.
  select single * from t247 where SPRAS = 'EN'
                              and KTX   = Imotxt.
  if sy-subrc = 0.
    move t247-MNR to Emonthno.
  endif.
endform.                    "get_month_num

*----------------------------------------------------------------------
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*----------------------------------------------------------------------
FORM OPEN_FILES.
  DATA: MSG(100).
* ----------------------------------------------------------
  open dataset p_file1 for input in text mode message msg.
  if ( sy-subrc <> 0 ).
    message E002 with infile msg.
  endif.
* ----------------------------------------------------------
  open dataset p_file2 for input in text mode message msg.
  if ( sy-subrc <> 0 ).
    message E002 with infile msg.
  endif.
* ----------------------------------------------------------
  open dataset p_fileot for output in text mode message msg.
  if ( sy-subrc <> 0 ).
    message E002 with outfile msg.
  endif.
ENDFORM.                    "OPEN_FILES
