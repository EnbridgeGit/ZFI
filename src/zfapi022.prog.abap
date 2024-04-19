report zfapi022 message-id 00  no standard page heading
             line-size 132 line-count 65.
*__________________________________________________________________
* NOTE:  Invoice Document Numbers will be sent to CARS if the WebMethods
*        adaptor is active - Check with BIS - AP Module Expert before
*        executing this abap manually.
*__________________________________________________________________
* Program      :  ZFAPI022
* Created ON   :  October 11, 2001
* Created By   :  Mary Lou DeMeester
*__________________________________________________________________
* This program reads BKPF DATA TO DETERMINE the information that needs
* to be sent to CARS.  This information is then posted to CARS.
*__________________________________________________________________
* 2010/02/19 lritchie TR785 Add payment terms
* 2010/02/12 lritchie TR782 Change titles from POSTING to ENTRY,
*                           add document date, sub-total,
*                           some performance improvements
* 2009/01/21 mdemeest TR342 Add 'END of REPORT' to printout so that
*                           there is always a report.
* 2008/09/10 mdemeest TR277 Add CPUTM (Entry time) to report
* 2002/04/15 mdemeest       Changed selection criteria BUDAT to CPUDT
* 2001/12/13 mdemeest CARS  Added LIFNR (Vendor #) to report
* 2001/10/11 mdemeest CARS  Original Program
*__________________________________________________________________


tables: bkpf,       "Accounting Document Header
        bseg.       "Accounting Detail

data:   v_total_item_count      type i,
        v_vendor_item_count     type i,
        v_total_dollars         like bseg-dmbtr,
        v_vendor_dollars        like bseg-dmbtr,
        v_prev_zterm            like bseg-zterm,
        v_prev_lifnr            like bseg-lifnr,
        v_prev_bldat            like bkpf-bldat.

data:   begin of wa  occurs 0,
         lifnr         like bseg-lifnr,     "Vendor #
         belnr         like bkpf-belnr,     "Document #        L= 10
         gjahr         like bkpf-gjahr,     "fiscal year       L= 4
         cpudt         like bkpf-cpudt,     "Entry date        L= 8
         cputm         like bkpf-cputm,     "Entry time
         bktxt         like bkpf-bktxt,     "CARS Reference #  L = 25
         xblnr         like bkpf-xblnr,     "Reference         L = 16
         dmbtr         like bseg-dmbtr,     "Amount (not sent to CARS)
         bldat         like bkpf-bldat,     "Document date
         zterm         like bseg-zterm,     "Payment terms
       end of wa.

data:  wa_data(100) type c.

*___________________________________________________________________
* parameters
*__________________________________________________________________
selection-screen begin of block blk1 with frame.

selection-screen begin of block box2 with frame title text-100.

parameters:
             p_file radiobutton group rbcr,     "Flat File
             p_outfl like filename-fileextern default
                     '/usr/sap/interfaces/P01/IFMM005/carsinv.dat',
             p_func radiobutton group rbcr,     "Function Call
             rfcdest(32) default 'Z_ACTIVEWORKS_INVOICE_NO'.

selection-screen end of block box2.

parameters:  p_ccode like bkpf-bukrs     memory id buk    obligatory,
             p_date  like sy-datum       default sy-datum obligatory,
             p_awtyp like bkpf-awtyp,
             p_awsys like bkpf-awsys.

selection-screen end of block blk1.

*___________________________________________________________________
* Event : start-of-selection.... open file to OUTPUT
*____________________________________________________________________
start-of-selection.

  if p_file = 'X'.
    perform open_file.
  endif.

* Select all possible Invoices that satisfy selection criteria.
  select bukrs belnr gjahr bldat cpudt cputm xblnr bktxt into
         (bkpf-bukrs, bkpf-belnr, bkpf-gjahr, bkpf-bldat,
          bkpf-cpudt, bkpf-cputm, bkpf-xblnr, bkpf-bktxt)
    from bkpf
    where bukrs = p_ccode                "Company code
      and cpudt = p_date                 "CPU date
      and awtyp = p_awtyp                "Reference Procedure
      and awsys = p_awsys.               "Logical System

    select single shkzg dmbtr lifnr zterm into
           (bseg-shkzg, bseg-dmbtr, bseg-lifnr, bseg-zterm)
       from bseg
       where bukrs = bkpf-bukrs
         and belnr = bkpf-belnr
         and gjahr = bkpf-gjahr
         and buzei = '001'.

    clear wa.

    if bseg-shkzg = 'S'.
      compute bseg-dmbtr = bseg-dmbtr * -1.
    endif.

    move bkpf-belnr to wa-belnr.
    move bkpf-gjahr to wa-gjahr.
    move bkpf-bldat to wa-bldat.
    move bkpf-cpudt to wa-cpudt.
    move bkpf-cputm to wa-cputm.
    move bkpf-bktxt to wa-bktxt.
    move bkpf-xblnr to wa-xblnr.
    move bseg-dmbtr to wa-dmbtr.
    move bseg-lifnr to wa-lifnr.
    move bseg-zterm to wa-zterm.
    append wa.
  endselect.

* Call WebMethods to send information to the CARS application.


* Call ACTIVE WORKS to send the email.

  sort wa by lifnr bldat zterm belnr.

  loop at wa.
    if v_prev_lifnr <> wa-lifnr.
      if v_prev_lifnr <> ' '.
        skip 3.
        write: / text-021, v_vendor_item_count,
                 text-022 under text-014, v_vendor_dollars under text-015.
        new-page.
        v_prev_lifnr = wa-lifnr.
        v_prev_bldat = wa-bldat.
        clear: v_vendor_item_count, v_vendor_dollars.
      else.
        v_prev_lifnr = wa-lifnr.
        v_prev_bldat = wa-bldat.
        v_prev_zterm = wa-zterm.
      endif.
    endif.

    if v_prev_bldat <> wa-bldat.
      v_prev_bldat = wa-bldat.
      v_prev_zterm = wa-zterm.
      new-page.
    endif.

    if v_prev_zterm <> wa-zterm.
        v_prev_zterm = wa-zterm.
      new-page.
    endif.

    v_total_item_count = v_total_item_count + 1.
    v_total_dollars = v_total_dollars + wa-dmbtr.
    v_vendor_item_count = v_vendor_item_count + 1.
    v_vendor_dollars = v_vendor_dollars + wa-dmbtr.

    write: / wa-lifnr under text-018,
             wa-belnr under text-011,
             wa-cpudt under text-012,
             text-019,
             wa-cputm,
             wa-bldat under text-020,
             wa-zterm under text-023,
             wa-xblnr under text-013,
             wa-bktxt under text-014,
             wa-dmbtr under text-015.

    if p_file = 'X'.
      concatenate wa-belnr wa-gjahr wa-cpudt
                  wa-xblnr wa-bktxt
                  into wa_data separated by ','.
      transfer wa_data to p_outfl.
    elseif p_func = 'X'.
      call function 'Z_ACTIVEWORKS_INVOICE_NO' in background task
        destination rfcdest
        exporting
          carsbelnr = wa-belnr              "SAP document number
          carsgjahr = wa-gjahr              "Fiscal year
          carsbudat = wa-cpudt              "Document entry date
          carsbktxt = wa-bktxt              "Document header
          carsxblnr = wa-xblnr.             "Reference
    endif.

    at last.
      skip 3.
      write: / text-021, v_vendor_item_count,
               text-022 under text-014, v_vendor_dollars under text-015.
      new-page.
      skip 3.
      write: / text-016, v_total_item_count,
               text-017 under text-014, v_total_dollars under text-015.
    endat.

  endloop.

  skip 3.
  write: /50 '---------- END of REPORT -----------------------'.

  if p_file = 'X'.
    perform close_file.
  else.
    if p_func = 'X'.
      commit work.
    endif.
  endif.

top-of-page.

  write: /1 text-rpt,  sy-repid, 40 text-001,
                              90 text-dte, sy-datum, text-amp, sy-uzeit.
  write: / text-clt under text-rpt, sy-mandt under sy-repid, sy-sysid,
         (10) p_date using edit mask '____/__/__' under text-001,
         text-pge under text-dte, sy-pagno under sy-datum.
  uline.
  write: /1 text-018, 11 text-011, 24 text-012, 49 text-020, 62 text-023,
          73 text-013, 92 text-014,120 text-015.
  uline.
  skip 1.

*&---------------------------------------------------------------------*
*&      Form  open_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form open_file.

  data: msg_text(30) type c.
  open dataset p_outfl for output in text mode message msg_text.

endform.                    "open_file

*&---------------------------------------------------------------------*
*&      Form  close_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form close_file.

  close dataset p_outfl.

endform.                    "close_file
