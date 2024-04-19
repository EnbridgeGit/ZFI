REPORT ZFFII011 MESSAGE-ID ZS line-count 65 line-size 1023.
************************************************************************
*  Extract Project Costings Information
************************************************************************
* 2002/06/21 mdemeest #--- Copied IFFII001 and made modifications
*                          Read spreadsheet and load to interal table
*                          Sort spreadsheet by SAP G/L
*                          Read G/L and match to spreadsheet and produce
*                          report
*                          Download an an excel spreadsheet
************************************************************************
TABLES: proj,                "Project master
        prps,                "WBS master
        cosp,                "External postings
        coss.                "Internal postings.



* Selection Screen
*selection-screen.
select-options:
           s_vernr             for proj-vernr
                                        no intervals.
PARAMETER:
           p_psproj(9)          type c   obligatory,
           p_bukrs              like skb1-bukrs obligatory,
           p_bu(5)      type c  obligatory,
           p_date  like sy-datum.    "Document Date
*           p_ledger(10) type c  obligatory default 'local' lower case.
*ARAMETER:  FILE1   like filenameci-fileextern
*                default '/usr/sap/interfaces/P01/CFMM001/GL.SAP',
PARAMETER:  filein  like filenameci-fileextern
                 default '/usr/sap/interfaces/P01/CFMM001/glin.txt'.
PARAMETER:  fileout  like filenameci-fileextern
                 default '/usr/sap/interfaces/P01/CFMM001/psout.txt'.
*           recsize type i default 90,
*           TABLE2 like RSEUX-CDT_VALUE default 'MAKT',
*           FILE2 like filenameci-fileextern
*                 default '/usr/sap/interfaces/P01/CFMM001/MAKT.SAP',
*           recsize1 type i default 98.
*PARAMETER: LFILE LIKE FILENAME-FILEINTERN.
*PARAMETER: RECSIZE TYPE I.


*---------------------- SAP to PS Spreadsheet Info ---------------------
data: inrec(400) type c.             "Comma delimited record from xls

data: begin of ps_fields occurs 30,
         ps_field_det(20) type c.
data: end of ps_fields.

data: begin of xref        occurs 500,
        sapgl(10)         type c,
        sapdesc(20)       type c,
        dept(10)          type c,
        psacct(10)        type c,
        electacct(10)     type c,
        gaap(6)           type c,
        type(10)          type c,
        descr(20)         type c,
        rtdesc(20)        type c,
        rcto(10)          type c,
        rcfrom(10)        type c,
        project(20)       type c,
        activity(20)      type c,
        process(20)       type c,
        product(20)       type c,
        affiliate(20)     type c,
        location(20)      type c.
data:  end of xref.
*-----------------------------------------------------------------------
* Global Data Definitions
DATA: PHYFILE LIKE FILENAMECI-FILEEXTERN.
DATA: PHYFILEout LIKE FILENAMECI-FILEEXTERN.
data: recsize type i.

data: wa_wkgxxx like cosp-wkg001.
data: wa_megxxx like cosp-meg001.

data: status(40) type c.           "All statuses from function call
data: wa_status(4) type c.         "Recent WBS Status from function call

data:  wa_balance like glt0-tsl01.
data:  wa_temp    like glt0-tsl01.

DATA: RECORD(90).

DATA: BEGIN OF wa_pc OCCURS 25000,
      posid(11)         type c,   "SAP WBS
      sapacct(6)        type c,   "SAP Acct
      unit(5)           type c,   "Business unit - based on company code
      project(20)       type c,   "PS Project - based on resp person
      activity(15)      type c,   "Activity - based on job&WBS
      resource(40)      type c,   "N/A Resource ID
      rcfrom(40)        type c,   "N/A Resource ID from
      bugl(5)           type c,   "Business Unit GL = unit
      journal(10)       type c,   "N/A Journal
      jrnl_dt(4)        type c,   "N/A Journal Date
      unpost_seq        type i,   "N/A Unpost Seq
      jrnl_line         type i,   "N/A Journal Line
      fsyr(4)           type c,
      acctperiod(2)     type c,
      account(30)       type c,
      altacct(10)       type c,
      restype(5)        type c,
      operatingunit(30) type c,
      deptid(30)        type c,
      d_process(30)     type c,
      d_location(20)    type c,
      product(6)        type c,
      d_cust_seg(5)     type c,
*      d_prod_mth        like sy-datum,
      d_prod_mth(10)    type c,
      affiliate(5)      type c,
      bus_unit_gl_fr(5) type c,  "N/A Business Unit GL From
      curr_cd(3)        type c,
      statistics_cd(3)  type c,  "N/A Statistics Code
      ledger_group(10)  type c,  "N/A Ledger Group
      analysis_type(3)  type c,
      res_cat(5)        type c,
      res_sub_cat(5)    type c,
      res_user1(10)     type c,  "N/A Res User 1
      res_user2(10)     type c,  "N/A Res User 2
      res_user3(10)     type c,  "N/A Res User 3
      res_user4(10)     type c,  "N/A Res User 4
      res_user5(10)     type c,  "N/A Res User 5
      trans_dt          like sy-datum,  "Date of Conversion
      accounting_dt     like sy-datum,  "Date of Transaction
      oprid(30)         type c,  "Operator Id  Always 'CONVERS'
      dttm_stamp        like sy-uzeit,
      jnrl_ln_ref(10)   type c,  "N/A Journal Line Reference
      open_item_stat(1) type c,  "N/A Open Item Status
      line_descr(30)    type c,  "N/A Line Description
      jrnl_line_stat(1) type c,  "N/A Journal Line Status
      jrnl_line_dt(4)   type c,  "N/A Journal Line Date
      foreign_curr(3)   type c,  "Foreign Currency Always 'CAD'
      rt_type(5)        type c,  "Rt Type Always 'CRRNT'
      foreign_amt(10)   type c,  "Foreign Amount  same as Resource_AMT
      rate_mult(15)     type c,  "Rate Multiplier  Always 1
      rate_div(15)      type c,
      cur_effdt         like sy-datum,  "Date of Conversion
      process_inst(10)  type c,  "Process Instance  Always 0
      pc_distrib_stat(1) type c, "PC Distribution status   Always 'N'
      gl_distrib_stat(1) type c, "GL Distribution status   Always 'Y'
      proj_trans_type(3) type c, "N/A Project Trans Type
      proj_trans_code(5) type c, "N/A Project Trans Code
      resource_status(1) type c, "N/A Resource Status
      descr(30)          type c, "Description
      system_source(3)   type c, "System source            Always 'CNV'
      resqty(10)         type c, "Transaction Line Quantity
      resamt(12)         type c, "Transaction Line Amt
      wbsstat(10)        type c, "WBS Status
      END OF wa_pc.

DATA: BEGIN OF wa_pcsub OCCURS 25000,
      fsyr(4)           type c,
      posid(11)         type c,   "SAP WBS
      sapacct(6)        type c,   "SAP Acct
      resamt        like cosp-wkg001,
      wbsstat(10)        type c, "WBS Status
      end of wa_pcsub.

DATA: BEGIN OF wa OCCURS 25000,
      posid(11)         type c,   "SAP WBS
      sapacct(6)        type c,   "SAP Acct
      resamt like cosp-wkg001,
      end of wa.

*-----------------------------------------------------------------------
tables:  dd03l,
         t001.

field-symbols: <F1> .

data:  t_code          like bmm00-tcode value 'FB01',
       CHAR(21)        type c,
       nodata(40)      type c value '/'.

data:  amt_40 like cosp-wkg001.
data:  amt_50 like cosp-wkg001.
data:  amt_total like cosp-wkg001.
data:  last_record(1) type c.

data:  wa_record(2200)  type c.

data:  date  like sy-datum.

data:  begin of bdcdata occurs 100.
            include structure bdcdata.
data:  end of bdcdata.

data:  begin of z_bgr00.
            include structure bgr00.
data:  end of z_bgr00.

data:  begin of z_bbkpf.
            include structure bbkpf.
data:  end of z_bbkpf.

data:  begin of z_bbseg.
*            include structure bbseg.
          include structure zbseg.
data:  end of z_bbseg.



  perform OPEN_FILE.
  perform process_excel_spreadsheet.
  sort xref by sapgl.

*-----------------------------------------------------------------------
* Build report required by PeopleSoft
*-----------------------------------------------------------------------
  new-page.
  perform print_headings.

  SELECT * FROM proj                    "Selection based on respon. code
     where  vbukr = p_bukrs
       and  vernr in s_vernr.
     if  proj-pspid+5(2) co '0123456789'.  "Eliminate templates
        select * from prps
          where psphi = proj-pspnr.
          perform get_wbs_status.

          select * from cosp
              where objnr = prps-objnr
                and ( versn = '000' or versn = '020') .
           if cosp-kstar+4(6) = '491001' or cosp-kstar+4(6) = '491002'.
                 continue.
             else.
                perform move_info_to_wa_pc.
                perform move_cosp_info.
             endif.
           endselect.                        " end of COSP

          select * from coss
              where objnr = prps-objnr
                and ( versn = '000' or versn = '020' ).
           if  coss-kstar+4(6) = '491001' or coss-kstar+4(6) = '491002'.
              else.
                perform move_info_to_wa_pc.
                perform move_coss_info.
              endif.
           endselect.                        " end of COSs

        endselect.                           " end of PRPS
     endif.

  endselect.                        " end of PROJ selection

  perform print_info.
  select single * from t001
      where bukrs = p_bukrs.
  perform create_subtable.
  sort wa_pcsub by fsyr posid sapacct.
  perform create_bdc.
  close dataset: filein.


form move_info_to_wa_pc.
  move prps-posid                    to wa_pc-posid.
  move p_bu                          to wa_pc-unit.
  move p_bu                          to wa_pc-bugl.
  move p_psproj                      to wa_pc-project.
  move prps-posid+4(7)               to wa_pc-activity.
  move 'CAD'                         to wa_pc-curr_cd.
  move 'CONVERS'                     to wa_pc-oprid.
  move sy-datum                      to wa_pc-trans_dt.
  move sy-datum                      to wa_pc-accounting_dt.
  move sy-uzeit                      to wa_pc-dttm_stamp.
  move 'CAD'                         to wa_pc-foreign_curr.
  move 'CRRNT'                       to wa_pc-rt_type.
  move '1'                           to wa_pc-rate_mult.
  move '1'                           to wa_pc-rate_div.
  move sy-datum                      to wa_pc-cur_effdt.
  move '0'                           to wa_pc-process_inst.
  move 'N'                           to wa_pc-pc_distrib_stat.
  move 'I'                           to wa_pc-gl_distrib_stat.
  move 'Converted from SAP'          to wa_pc-descr.
  move 'CNV'                         to wa_pc-system_source.

  move wa_status                     to wa_pc-wbsstat.

  perform match_excel_spreadsheet.
endform.

form move_cosp_info.
  move cosp-gjahr                    to wa_pc-fsyr.
  move cosp-kstar+4(6)               to wa_pc-sapacct.

  if cosp-wrttp = '01'.
     move 'BUD'                      to wa_pc-analysis_type.
  elseif cosp-wrttp = '04'.
     move 'ACT'                      to wa_pc-analysis_type.
  else.
     move '???'                      to wa_pc-analysis_type.
  endif.



  if cosp-wkg001 <> '0'.
     move '01'                         to wa_pc-acctperiod.
     concatenate: cosp-gjahr '01' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg001                  to wa_wkgxxx.
     move cosp-meg001                  to wa_pc-resqty.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg002 <> 0.
     move '02'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '02' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg002                to wa_wkgxxx.
     move cosp-meg002                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg003 <> 0.
     move '03'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '03' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg003                to wa_wkgxxx.
     move cosp-meg003                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg004 <> 0.
     move '04'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '04' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg004                to wa_wkgxxx.
     move cosp-meg004                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg005 <> 0.
     move '05'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '05' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg005                to wa_wkgxxx.
     move cosp-meg005                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg006 <> 0.
     move '06'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '06' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg006                to wa_wkgxxx.
     move cosp-meg006                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg007 <> 0.
     move '07'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '07' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg007                to wa_wkgxxx.
     move cosp-meg007                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg008 <> 0.
     move '08'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '08' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg008                to wa_wkgxxx.
     move cosp-meg008                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg009 <> 0.
     move '09'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '09' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg009                to wa_wkgxxx.
     move cosp-meg009                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg010 <> 0.
     move '10'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '10' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg010                to wa_wkgxxx.
     move cosp-meg010                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg011 <> '0'.
     move '11'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '11' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg011                to wa_wkgxxx.
     move cosp-meg011                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if cosp-wkg012 <> '0'.
     move '12'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '12' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg012                to wa_wkgxxx.
     move cosp-meg012                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.
  if cosp-wkg013 <> '0'.
     move '00'                       to wa_pc-acctperiod.
     concatenate: cosp-gjahr '13' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move cosp-wkg013                to wa_wkgxxx.
     move cosp-meg013                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.
endform.

form move_coss_info.
  move coss-gjahr                    to wa_pc-fsyr.
  move coss-kstar+4(6)               to wa_pc-sapacct.

  if coss-wrttp = '01'.
     move 'BUD'                      to wa_pc-analysis_type.
  elseif coss-wrttp = '04'.
     move 'ACT'                      to wa_pc-analysis_type.
  else.
     move '???'                      to wa_pc-analysis_type.
  endif.


  if coss-wkg001 <> '0'.
     move '01'                         to wa_pc-acctperiod.
     concatenate: coss-gjahr '01' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg001                  to wa_wkgxxx.
     move coss-meg001                  to wa_pc-resqty.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg002 <> 0.
     move '02'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '02' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg002                to wa_wkgxxx.
     move coss-meg002                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg003 <> 0.
     move '03'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '03' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg003                to wa_wkgxxx.
     move coss-meg003                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg004 <> 0.
     move '04'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '04' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg004                to wa_wkgxxx.
     move coss-meg004                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg005 <> 0.
     move '05'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '05' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg005                to wa_wkgxxx.
     move coss-meg005                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg006 <> 0.
     move '06'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '06' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg006                to wa_wkgxxx.
     move coss-meg006                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg007 <> 0.
     move '07'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '07' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg007                to wa_wkgxxx.
     move coss-meg007                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg008 <> 0.
     move '08'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '08' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg008                to wa_wkgxxx.
     move coss-meg008                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg009 <> 0.
     move '09'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '09' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg009                to wa_wkgxxx.
     move coss-meg009                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg010 <> 0.
     move '10'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '10' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg010                to wa_wkgxxx.
     move coss-meg010                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg011 <> '0'.
     move '11'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '11' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg011                to wa_wkgxxx.
     move coss-meg011                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.

  if coss-wkg012 <> '0'.
     move '12'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '12' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg012                to wa_wkgxxx.
     move coss-meg012                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.
  if coss-wkg013 <> '0'.
     move '00'                       to wa_pc-acctperiod.
     concatenate: coss-gjahr '13' '01' into wa_pc-d_prod_mth
                                              separated by '/'.
     move coss-wkg013                to wa_wkgxxx.
     move coss-meg013                to wa_megxxx.
     perform amts_qty.
     append wa_pc.
  endif.
endform.




form print_info.
   loop at wa_pc.
   write: / wa_pc-posid          under text-200,    "SAP P/S WBS
            wa_pc-sapacct        under text-201,    "SAP G/L Acct
            wa_pc-unit           under text-001,    "P/S Business Unit
            wa_pc-project        under text-002,    "P/S Project
            wa_pc-activity       under text-003,    "P/S Activity
            wa_pc-resource       under text-004,    "Resource
            wa_pc-rcfrom         under text-005,    "Resource From
            wa_pc-bugl           under text-006,    "Business_Unit_GL
            wa_pc-fsyr           under text-011,    "Fiscal Year
            wa_pc-acctperiod     under text-012,    "Account Period
            wa_pc-account        under text-013,    "Account
            wa_pc-restype        under text-015,    "Resource type
            wa_pc-operatingunit  under text-016,    "Operating Unit
            wa_pc-deptid         under text-017,    "DeptId
            wa_pc-d_process      under text-018,    "Process
            wa_pc-d_location     under text-019,    "Location
            wa_pc-product        under text-020,    "Product
            wa_pc-d_cust_seg     under text-021,    "CustSegment
            wa_pc-d_prod_mth     under text-022,    "ProdMth
            wa_pc-affiliate      under text-023,    "Affiliate
            wa_pc-curr_cd        under text-025,    "Currency Code
            wa_pc-analysis_type  under text-028,    "Analysis Type
            wa_pc-trans_dt       under text-036,    "Conversion Date
            wa_pc-accounting_dt  under text-037,
                                                   "Transaction Date
            wa_pc-oprid          under text-038, "Oprid Always 'CONVERS'
            wa_pc-dttm_stamp     under text-039,    "Conversion Time
            wa_pc-foreign_curr   under text-045,    "Foreign Currency
            wa_pc-rt_type        under text-046,    "Rate Type
            wa_pc-foreign_amt    under text-047,    "= resource amt
            wa_pc-rate_mult      under text-048,    "Rate Multiplier
            wa_pc-rate_div       under text-049,
            wa_pc-cur_effdt      under text-050,
            wa_pc-process_inst   under text-051,
            wa_pc-pc_distrib_stat under text-052,
            wa_pc-gl_distrib_stat under text-053,
            wa_pc-descr           under text-057,
            wa_pc-system_source   under text-058,
            wa_pc-resqty          under text-116,    "Resource Quantity
            wa_pc-resamt          under text-117,    "Resource amount
            wa_pc-wbsstat         under text-202.
*   uline.

*   at last.
*     sum.
*     write: /30 'TOTAL',
*         (10)wa_pc-amount under text-014 decimals 2.  "Amount
*   endat.

   endloop.
endform.



form MNPP_mapping.
endform.






*--------------------------  OPEN_FILE ---------------------------------
* Routine to convert the logical file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
*-----------------------------------------------------------------------
FORM OPEN_FILE.
*   CALL FUNCTION 'FILE_GET_NAME'
*      EXPORTING
*         LOGICAL_FILENAME        = LFILE
*      IMPORTING
*         FILE_NAME               = PHYFILE
*      EXCEPTIONS
*         FILE_NOT_FOUND          = 1
*         OTHERS                  = 2.
*   IF SY-SUBRC <> 0.
*      MESSAGE E018.
*   ELSE.
      OPEN dataset filein for input in text mode.
      open dataset fileout for output in text mode.
*     OPEN DATASET FILE1  FOR OUTPUT IN TEXT MODE.
*   ENDIF.
ENDFORM.

*----------------------- PROCESS_EXCEL_SPREADSHEET ---------------------
*  Read spreadsheet - each line being a record
*  Parse record splitting at commas
*  Move parsed fields to internal table - This table will be used as
*  cross reference between SAP and PeopleSoft
*-----------------------------------------------------------------------
form process_excel_spreadsheet.
  do.
    clear inrec.
    read dataset filein into inrec.
*  write: / inrec.
    if sy-subrc <> 0.
       exit.
    else.
       clear ps_fields.
       split inrec at ',' into table ps_fields.
*      loop at ps_fields.
*        write: / sy-tabix, ps_fields-ps_field_det.
*      endloop.
    endif.

    clear xref.
    loop at ps_fields.              "moves to internal table -xref
      case sy-tabix.
        when 1.
            move ps_fields-ps_field_det to xref-sapgl.
        when 2.
            move ps_fields-ps_field_det to xref-sapdesc.
        when 3.
            move ps_fields-ps_field_det to xref-dept.
        when 4.                                         "Extra column
        when 5.
*           move ps_fields-ps_field_det to xref-psacct.
        when 6.
            move ps_fields-ps_field_det to xref-psacct.
        when 7.
        when 8.
            move ps_fields-ps_field_det to xref-type.
        when 9.
            move ps_fields-ps_field_det to xref-descr.
        when 10.
            move ps_fields-ps_field_det to xref-rtdesc.
        when 11.
            move ps_fields-ps_field_det to xref-rcto.
        when 12.
            move ps_fields-ps_field_det to xref-rcfrom.
        when 13.
            move ps_fields-ps_field_det to xref-project.
        when 14.
            move ps_fields-ps_field_det to xref-activity.
        when 15.
            move ps_fields-ps_field_det to xref-process.
        when 16.
            move ps_fields-ps_field_det to xref-product.
        when 17.
            move ps_fields-ps_field_det to xref-affiliate.
        when 18.
            move ps_fields-ps_field_det to xref-location.
      endcase.
    endloop.
    append xref.
  enddo.

*   loop at xref.
*     write: / xref-sapgl, xref-dept, xref-psacct, xref-type,
*              xref-rtdesc, xref-rcto, xref-rcfrom, xref-project,
*              xref-activity, xref-process, xref-product,
*              xref-affiliate, xref-location.
*   endloop.


endform.

*----------------------- PRINT_HEADINGS --------------------------------
* PeopleSoft Report Headings
*-----------------------------------------------------------------------
form print_headings.
  write: / text-200, text-201,
           text-001, text-002, text-003, text-004, text-005,
           text-006, text-007, text-008, text-009, text-010,
           text-011, text-012, text-013, text-014, text-015,
           text-016, text-017, text-018, text-019, text-020,
           text-021, text-022, text-023, text-024, text-025,
           text-026, text-027, text-028, text-029, text-030,
           text-031, text-032, text-033, text-034, text-035,
           text-036, text-037, text-038, text-039, text-040,
           text-041, text-042, text-043, text-044, text-045,
           text-046, text-047, text-048, text-049, text-050,
           text-051, text-052, text-053, text-054, text-055,
           text-056, text-057, text-058, text-059, text-060,
           text-061, text-062, text-063, text-064, text-065,
           text-066, text-067, text-068, text-069, text-070,
           text-071, text-072, text-073, text-074, text-075,
           text-076, text-077, text-078, text-079, text-080,
           text-081, text-082, text-083, text-084, text-085,
           text-086, text-087, text-088, text-089, text-090,
           text-091, text-092, text-093, text-094, text-095,
           text-096, text-097, text-098, text-099, text-100,
           text-101, text-102, text-103, text-104, text-105,
           text-106, text-107, text-108, text-109, text-110,
           text-111, text-112, text-113, text-114, text-115,
           text-116, text-117, text-118, text-119, text-120,
           text-121, text-122, text-123, text-124, text-125,
           text-126, text-127, text-128,
           text-202.

endform.

*-------------------- MATCH_EXCEL_SPREADSHEET --------------------------
* Using SAP G/L number, match to SAP G/L number in spreadsheet and
* pick up all associated information.
*-----------------------------------------------------------------------
form match_excel_spreadsheet.
  read table xref with key sapgl(6) = cosp-kstar+4(6) binary search.
  if sy-subrc = '0'.
     move xref-psacct              to wa_pc-account.
     move xref-type                to wa_pc-restype.
     if p_bu = '10715'.
        move 'A267'                to wa_pc-operatingunit.
        move 'A267'                to wa_pc-deptid.
     elseif p_bu = '10717'.
        move 'A269'                to wa_pc-operatingunit.
        move 'A269'                to wa_pc-deptid.
     endif.
                          .
     move xref-process             to wa_pc-d_process.
     move xref-product             to wa_pc-product.
     move xref-affiliate           to wa_pc-affiliate.
     move xref-location            to wa_pc-d_location.
  else.
     write: / cosp-kstar, ' not found in spreadsheet. Please verify'.

  endif.
endform.
*-----------------------------------------------------------------------

form amts_qty.
  move wa_wkgxxx                    to: wa_pc-resamt, wa_pc-foreign_amt.
  if cosp-kstar+4(6) = '100490' or cosp-kstar+4(6) = '110490' or
     cosp-kstar+4(6) = '115090' or cosp-kstar+4(6) = '115900'.
     if wa_wkgxxx <> 0.
        if p_bu = '10715'.
           move 'A271'               to wa_pc-operatingunit.
           move 'A271'               to wa_pc-deptid.
        elseif p_bu = '10717'.
           move 'A273'               to wa_pc-operatingunit.
           move 'A273'               to wa_pc-deptid.
        endif.
      endif.
   endif.
endform.
                              .
form get_wbs_status.
  call function 'AIP9_STATUS_READ'
    exporting
            i_objnr = prps-objnr
            i_spras = sy-langu
    importing
            e_sysst = status
     exceptions
            others = 1.

     clear wa_status.
     if status cs 'CLSD'.
        move 'CLSD' to wa_status.
     elseif status cs 'TECO'.
        move 'TECO' to wa_status.
     elseif status cs 'REL'.
        move 'REL'  to wa_status.
     elseif status cs 'CRTD'.
        move 'CRTD' to wa_status.
     endif.
endform.




*---------------------------  CREATE_BDC -------------------------------
form create_bdc.
  loop at wa_pcsub.
    at new fsyr.
       perform init_structures using 'BGR00'.
       perform write_bgr00.
       perform init_structures using 'BBKPF'.
       perform write_bbkpf.
       perform init_structures using 'BBSEG'.
     endat.

     perform write_bbseg.

     at end of fsyr.
        move 'X' to last_record.
        compute amt_total = amt_40 + amt_50.
        perform write_bbseg.
     endat.
  endloop.

endform.


form init_structures using tabname.
  select * from dd03l where tabname = tabname.
      clear CHAR.
      CHAR(2) = 'Z_'.
      CHAR+2(5) = tabname.
      CHAR+7(1) = '-'.
      CHAR+8 = dd03l-fieldname.
      assign (CHAR) to <F1>.
      <F1> = nodata.
  endselect.

endform.



form write_bgr00.
    move '0'                   to Z_bgr00-stype.
    move 'ZFFII011'            to z_bgr00-group.
    move sy-mandt              to z_bgr00-mandt.
    move sy-uname              to z_bgr00-usnam.
    transfer z_bgr00 to fileout length 200.
endform.



form write_bbkpf.
    move '1'                    to z_bbkpf-stype.
    move t_code                 to z_bbkpf-tcode.
    move p_date                 to date.
    write date                  to z_bbkpf-budat. "posting date
    write sy-datum              to z_bbkpf-bldat. "document date

    move 'ZM'                   to z_bbkpf-blart.
    move p_bukrs                to z_bbkpf-bukrs.
    move t001-waers             to z_bbkpf-waers.

    move 'Balance trans into P/Soft' to z_bbkpf-bktxt.
    move 'To P/Soft'                 to z_bbkpf-xblnr.

    move z_bbkpf to wa_record.
    move wa_record+21(200) to wa_record+5(220).
    transfer wa_record to fileout length 232.

endform.

form write_bbseg.

data:  wa_temp_hkont  like z_bbseg-hkont.

    move '2'                     to z_bbseg-stype.
    move 'ZBSEG'                 to z_bbseg-tbnam.
    move 'Balance transf. to P/Soft' to z_bbseg-sgtxt.
    if last_record = 'X'.
       if amt_total > 0.
          move '40' to z_bbseg-newbs.
          move amt_total to z_bbseg-wrbtr.
*          compute z_bbseg-wrbtr = amt_total *  ( - 1 ) .
          move '199998'           to z_bbseg-hkont.
          clear z_bbseg-projk.
       else.
          move '50' to z_bbseg-newbs.
          compute z_bbseg-wrbtr = amt_total *  ( - 1 ) .
          move '199998' to z_bbseg-hkont.
          clear z_bbseg-projk.
       endif.
    else.
       move wa_pcsub-sapacct to z_bbseg-hkont.
       move wa_pcsub-posid to z_bbseg-projk.
       if  wa_pcsub-resamt > 0.
           move '50' to Z_bbseg-newbs.
           move wa_pcsub-resamt      to z_bbseg-wrbtr.
           add wa_pcsub-resamt to amt_40.

       else.
           move '40' to z_bbseg-newbs.
           compute z_bbseg-wrbtr = wa_pcsub-resamt * ( - 1 ).
           add wa_pcsub-resamt to amt_50.
       endif.
    endif.

*    move wa_pcsub-sapacct         to z_bbseg-hkont.     "account
*    move wa_pcsub-posid           to z_bbseg-projk.
    move z_bbseg to wa_record.
    move wa_record+31(2000)       to wa_record+11(2020).
    transfer wa_record            to fileout length 2167.
endform.

form create_subtable.

   loop at wa_pc.
     if wa_pc-analysis_type = 'ACT'. "Only ACTUALS are put into BDC

        clear wa_pcsub.
        move wa_pc-fsyr           to wa_pcsub-fsyr.
        if wa_pc-fsyr < sy-datum(4).
           case wa_pc-wbsstat.
             when 'REL'.
                move '115090'  to wa_pcsub-sapacct.
             when 'CRTD'.
                move '115090'  to wa_pcsub-sapacct.
             when 'TECO'.
                move '100490'  to wa_pcsub-sapacct.
             when 'CLSD'.
                move '100490'  to wa_pcsub-sapacct.
           endcase.
       else.
         move wa_pc-posid       to wa_pcsub-posid.
         move wa_pc-sapacct     to wa_pcsub-sapacct.
       endif.


        move wa_pc-resamt          to wa_pcsub-resamt.
        collect wa_pcsub.
     endif.     "end of test for 'ACT'.
   endloop.

*   loop at wa_pc.
*     write: / wa_pc-resamt.
*     at end of fsyr.
*        sum.
*        write: / wa_pc-resamt.
*     endat.
*   endloop.

*   loop at wa_pcsub.
*     write: / wa_pcsub-fsyr, wa_pcsub-posid, wa_pcsub-sapacct,
*     wa_pcsub-resamt.
*   endloop.
endform.
