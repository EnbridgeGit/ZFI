REPORT ZFARR001 NO STANDARD PAGE HEADING LINE-SIZE 170.
******************************************************************
*      Owner: Union Gas Ltd                                      *
* Programmer: ML De Meester                                      *
*       Date: July 2001                                          *
*                                                                *
* The following program will generate a Customer Aging Report    *
******************************************************************

TABLES: BSID,           "Accounting: Secondary Index for Customers
        KNB1,           "Customer Master (Company Code)
        kna1,           "General data in Customer Master
        knkk,           "Customer Master Credit Mgt Control Area data
        bkpf,           "Accounting Header
        bseg,           "Accounting Detail
        bsega,          "Accounting Detail structure
        t001.           "Company Code

constants:
    w_age_0    type i   value 0,
    w_age_30   type i   value 30,
    w_age_60   type i   value 60,
    w_age_90   type i   value 90.

data: inv_tot    like bsid-wrbtr.

DATA:  begin of t_age_dtl  occurs 10000,
          bukrs       like bsid-bukrs,     "Company code
          kunnr       like kna1-kunnr,     "Customer number
          matnr       like bseg-matnr,     "Document type
          belnr       like bsid-belnr,     "Document Number
          buzei       like bsid-buzei,     "Line Item Number
          name        like kna1-name1,     "Name of customer
          gjahr       like bsid-gjahr,     "Fiscal Year
          bldat       like bsid-bldat,     "Document Date
          budat       like bsid-budat,     "Posting Date
          xblnr       like bsid-xblnr,     "Old document number
          duedate     like bsid-budat,     "Calculated due date
          age         type i,
          curamt      like bsid-wrbtr,     "Amt due immediately
          30amt       like bsid-wrbtr,     "Amt due within 30 days
          60amt       like bsid-wrbtr,     "Amt due within 60 days
          90amt       like bsid-wrbtr,     "Amt due within 90 days
          over90      like bsid-wrbtr,     "Amt due after 90 days
       end of t_age_dtl.

data: wa_name like t_age_dtl-name.


SELECTION-SCREEN BEGIN OF BLOCK BORDER WITH FRAME.

selection-screen skip.

selection-screen begin of block box1 with frame title text-001.
parameters:
        p_bukrs        like bsid-bukrs  obligatory memory id BUK,
        p_waers        like bsid-waers default 'CAD' obligatory.
select-options:
        s_akont        for knb1-akont                obligatory.
selection-screen end of block box1.

selection-screen skip.

SELECTION-SCREEN END OF BLOCK BORDER.

TOP-OF-PAGE.
  write: /1 text-rpt, sy-repid, 70 text-ttl,
         140 text-dte, sy-datum, text-amp, sy-uzeit.
  write: / text-clt under text-rpt, sy-mandt, sy-sysid,
           text-002 under text-ttl,
           text-pge under text-dte, sy-pagno.
  write: 80 sy-datum.
  write: / text-018 under text-rpt, p_waers,              "Currency
           t001-butxt under text-ttl.                     "Company Name
  uline.
  write: /.
  write: / text-015, text-004, text-003, text-005, text-006, text-007,
           text-008, text-009, text-010, text-011, text-012,
           text-014.
  write: /.
  uline.

INITIALIZATION.



* The following will highlight the screen's output for certain texts. *

AT SELECTION-SCREEN OUTPUT.
*LOOP AT SCREEN.
*  CHECK SCREEN-GROUP1 = 'ABC'.
*  SCREEN-INTENSIFIED = '1'.
*  MODIFY SCREEN.
*ENDLOOP.

***********************BEGINNING OF MAIN PROGRAM************************

START-OF-SELECTION.
*-----------------------------------------------------------------------
* select all records that match the entered company code and entered
* currency and have not yet been cleared.
*-----------------------------------------------------------------------
  select * from bsid
    where bukrs = p_bukrs
      and augdt = '00000000'
      and waers = p_waers.
    select single * from kna1
       where kunnr = bsid-kunnr.

    move space          to wa_name.           "Customer Name
    if  sy-subrc = '0'.
        move kna1-name1 to wa_name.
    endif.

    select * from knb1
      where kunnr = bsid-kunnr
        and bukrs = bsid-bukrs
        and akont in s_akont.
      clear knkk.
      select single * from knkk
        where kunnr = bsid-kunnr
          and kkber = bsid-bukrs.
      perform save_entry.
    endselect.             "End of KNB1
  endselect.               "End of BSID


  sort t_age_dtl by bukrs kunnr matnr belnr buzei.

  perform produce_report.

*----------------------  SAVE_ENTRY  -----------------------------------
*  Release all pertinent info to an internal table for further
*  processing
*-----------------------------------------------------------------------
form save_entry.
   clear t_age_dtl.
*------------------------------------------- Gets Material# from BSEG
   select * from bseg
      where bukrs = bsid-bukrs
        and belnr = bsid-belnr
        and gjahr = bsid-gjahr
        and buzei <> '001'.
       move bseg-matnr to t_age_dtl-matnr.
*-----------------------------------------------------------------------
   t_age_dtl-kunnr  = bsid-kunnr.
   t_age_dtl-name   = wa_name.
   t_age_dtl-bukrs  = bsid-bukrs.
   t_age_dtl-belnr  = bsid-belnr.
   t_age_dtl-gjahr  = bsid-gjahr.
   t_age_dtl-buzei  = bsid-buzei.             "line number
*   t_age_dtl-matnr  = bsid-matnr.
   t_age_dtl-bldat  = bsid-bldat.             "document date
   t_age_dtl-budat  = bsid-budat.             "invoice date
   t_age_dtl-xblnr  = bsid-xblnr.

   if bsid-zbd2t ne 0.                        "Calculate due date
      t_age_dtl-duedate = bsid-bldat = bsid-zbd2t.
   else.
      t_age_dtl-duedate = bsid-bldat + bsid-zbd1t.
   endif.

   t_age_dtl-age = SY-DATUM - t_age_dtl-duedate.

   if bsid-shkzg = 'H'.                       "--> negative
      bsid-wrbtr = bsid-wrbtr * -1.
   endif.

*----------------------------------------------------------------------
*  Put amounts into proper slot depending on age
*----------------------------------------------------------------------
   if t_age_dtl-age le w_age_0.
      t_age_dtl-curamt = bseg-wrbtr.
   elseif t_age_dtl-age le w_age_30.
      t_age_dtl-30amt  = bseg-wrbtr.
   elseif t_age_dtl-age le w_age_60.
      t_age_dtl-60amt  = bseg-wrbtr.
   elseif t_age_dtl-age le w_age_90.
      t_age_dtl-90amt  = bseg-wrbtr.
   else.
      t_age_dtl-over90 = bseg-wrbtr.       "over 90 days
   endif.

   collect t_age_dtl.
  endselect.
endform.

*------------------------  PRODUCE_REPORT  -----------------------------
* write report in company, customer, document order
*-----------------------------------------------------------------------
form produce_report.

  loop at t_age_dtl.
    move t_age_dtl-name to wa_name.
    at new bukrs.        "--> get new company code & new page
       select single * from t001
         where bukrs = t_age_dtl-bukrs.
       new-page.
    endat.

    at new kunnr.                                 "Customer Break
       format color 5.
       write: / t_age_dtl-kunnr under text-015, wa_name.
       format color off.
    endat.

    at new matnr.
       format color 3.
       write: / t_age_dtl-matnr under text-004.
       format color off.
    endat.

    perform write_amt.

    write:  t_age_dtl-belnr  under text-003,
            t_age_dtl-bldat  under text-005,
            t_age_dtl-budat  under text-006,
            t_age_dtl-xblnr  under text-012.

    at end of matnr.                             "Material Total
      sum.
      uline.
      format color 3.
      perform write_amt.
      write: text-017 under text-004, t_age_dtl-matnr.
      format color off.
      uline.
      write: /.
    endat.

    at end of kunnr.                              "Customer Total
      sum.
      uline.
      format color 5.
      perform write_amt.
      write: text-016 under text-015, t_age_dtl-kunnr.
      format color off.
    endat.

    at end of bukrs.                               "Company Total
      sum.
      uline.
      format color 4.
      perform write_amt.
      write:  text-013 under text-015.
      format color off.
      uline.
    endat.

  endloop.

  write: / text-end under text-ttl.
endform.

*----------------------------  WRITE_AMT  ------------------------------
*  writes all the amount fields, both for detail and totals
*-----------------------------------------------------------------------
form write_amt.
    write: / t_age_dtl-curamt under text-007,
             t_age_dtl-30amt  under text-008,
             t_age_dtl-60amt  under text-009,
             t_age_dtl-90amt  under text-010,
             t_age_dtl-over90 under text-011.
    inv_tot =  t_age_dtl-curamt + t_age_dtl-30amt +
               t_age_dtl-60amt  + t_age_dtl-90amt +
               t_age_dtl-over90.
    write:  inv_tot under text-014.
endform.


















