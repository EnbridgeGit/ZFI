report zfapr010 no standard page heading line-size 132 line-count 65.

************************************************************************
*  Author:      MaryLou De Meester                                     *
*  Date:        September 24, 2009                                     *
*  Issue Log:   TR___                                                  *
*  Description:                                                        *
*     - The purpose of this program is to produce the listing of       *
*       duplicate invoices - either within a company code or           *
*       across several company codes                                   *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*2010/01/07 TR782 L Ritchie   - Include duplicates when credit $       *
*                               equal debit $                          *
*2009/11/17 TR648 L Ritchie   - Skip data outside time period          *
*2009/09/24 TR___ M Demeester - New abap requested by DBossy/JLudwig   *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************

tables:  bkpf, "Document Header for posting date
         bseg, "Get vendor
         trdirt.                    "Program Name Table

data:  begin of it_bsip   occurs 0,
       xblnr  like bkpf-xblnr,
       budat  like bkpf-budat,
       bukrs  like bkpf-bukrs,
       gjahr  like bkpf-gjahr,
       bldat  like bkpf-bldat,
       waers  like bkpf-waers,
       belnr  like bkpf-belnr,
       blart  like bkpf-blart,
       lifnr  like bseg-lifnr,
       wrbtr  like bseg-wrbtr,
       wrbtr_positive like bseg-wrbtr,                   "LER 20100107
       count type i.
data:  end of it_bsip.

data: begin of dup_bsip  occurs 0,
      lifnr like bseg-lifnr,
      xblnr like bkpf-xblnr,
      wrbtr(12)  type c,
      wrbtr_positive like bseg-wrbtr,                   "LER 20100107
      count like it_bsip-count.
data:  end of dup_bsip.

data:  begin of rit_bsip   occurs 0,
       sbudat like bkpf-bldat,
       xblnr  like bkpf-xblnr,
       wrbtr  like bseg-wrbtr,
       wrbtr_positive like bseg-wrbtr,                   "LER 20100107
       bukrs  like bkpf-bukrs,
       budat  like bkpf-budat,
       bldat  like bkpf-bldat,
       lifnr  like bseg-lifnr,
       waers  like bkpf-waers,
       belnr  like bkpf-belnr,
       gjahr  like bkpf-gjahr,
       blart  like bkpf-blart,
       count type i.
data:  end of rit_bsip.

data: begin of wacond  occurs 0,
      condition(75) type c,
      end of wacond.

data: begin of tbl_keep occurs 0,
        xblnr   like bkpf-xblnr,
      end of tbl_keep.

data: newdtind like sy-datum.
*---------------------------------------------------------------------*
* TOP of PAGE - contains titles and parameters entered on screen

top-of-page.

  new-page.
  write: /1 text-rpt, sy-repid(20), trdirt-text+10(50),
         90 text-dte, sy-datum, text-amp, sy-uzeit.
  write: / text-clt under text-rpt,
           sy-mandt under sy-repid,
           text-pge under text-dte, sy-pagno under sy-datum.
  write: /.
  write: /1 text-100, 12 text-101, 30 text-102, 42 text-103,
         49 text-104, 60 text-105, 67 text-106,
         85 text-107, 96 text-110, 102 text-109 , 113 text-108 .
  uline.
*----------------------SCREEN -----------------------------------------*

  selection-screen begin of block box1 with frame title text-001.
  select-options:
           s_bukrs for bkpf-bukrs,                    "Company Code
           s_gjahr for bkpf-gjahr default sy-datum(4) obligatory,
                                                      "Fiscal year
           s_budat for bkpf-bldat,                    "Posting date
           s_lifnr for bseg-lifnr,                    "Vendor
           s_xblnr for bkpf-xblnr,                    "Reference Numer
           s_belnr for bkpf-belnr,                    "Document Number
           s_blart for bkpf-blart.
  parameter: p_days(3) type c default 10.             "Number of Days

  selection-screen end of block box1.

initialization.

  refresh s_budat.
*AT SELECTION-SCREEN output.
  data: startdt like sy-datum,
        enddt   like sy-datum.
  refresh s_budat.
  compute startdt = sy-datum - 180.
  move 'IBT'       to s_budat+0(3).
  move startdt     to s_budat+3(8).
  move sy-datum    to s_budat+11(8).
  append s_budat.

  refresh s_gjahr.
  move 'IBT'         to s_gjahr+0(3).
  move s_budat+3(4)  to s_gjahr+3(4).
  move s_budat+11(4) to s_gjahr+7(4).
  append s_gjahr.


start-of-selection.

  select single * from trdirt                       "ABAP NAME
    where name  = 'ZFAPR010'
      and sprsl = sy-langu.
  compute newdtind = sy-datum - p_days.

* Because FISCAL YEAR is required, it needs to go first.
* Any following condition will have  AND at the beginning of the
* condition.

  move 'GJAHR in s_gjahr' to wacond-condition.
  append wacond.

* These are optional parameters.
  if s_bukrs <> ' '.
    move 'and BUKRS in s_bukrs' to wacond-condition.
    append wacond.
  endif.

  if s_belnr <> ' '.
    move 'and BELNR in s_belnr' to wacond-condition.
    append wacond.
  endif.

  if s_blart <> ' '.
    move 'and BLART in s_blart' to wacond-condition.
    append wacond.
  endif.

  if s_budat <> ' '.
    move 'and BUDAT in s_budat' to wacond-condition.
    append wacond.
  endif.

  if s_xblnr <> ' '.
    move 'and XBLNR in s_xblnr' to wacond-condition.
    append wacond.
  endif.

*-------------------------------------------------------------
*  Select all documents posted based on the parameters in variant
*-------------------------------------------------------------
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text   = 'Selecting BKPF records'
    exceptions
      others = 1.

  select xblnr budat gjahr bukrs waers bldat belnr blart
         into (it_bsip-xblnr, it_bsip-budat, it_bsip-gjahr,
               it_bsip-bukrs, it_bsip-waers, it_bsip-bldat,
               it_bsip-belnr, it_bsip-blart)
         from bkpf
         where (wacond).

    select single * from bseg
       where bukrs = it_bsip-bukrs
         and belnr = it_bsip-belnr
         and gjahr = it_bsip-gjahr
         and lifnr <> space
         and lifnr in s_lifnr.

    if sy-subrc = '0'.
      move bseg-lifnr to it_bsip-lifnr.
      move bseg-wrbtr to it_bsip-wrbtr.
      move bseg-wrbtr to it_bsip-wrbtr_positive.                     "LER 2010/01/07
      if bseg-shkzg = 'H'.
        compute it_bsip-wrbtr = it_bsip-wrbtr * -1.
      endif.
      move 1 to it_bsip-count.

      append it_bsip.
    endif.
  endselect.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text   = 'Creating report ZFAPR010'
    exceptions
      others = 1.

**** start of changes LER 2010/01/07 - duplicates are on the absolute value

*  sort it_bsip by xblnr ascending.
*  loop at it_bsip.
*    move-corresponding it_bsip to dup_bsip.
*    move it_bsip-wrbtr to dup_bsip-wrbtr.
*    collect dup_bsip.
*  endloop.

  sort it_bsip by lifnr xblnr wrbtr_positive.
  loop at it_bsip.
    if sy-tabix = 1.
      move-corresponding it_bsip to dup_bsip.
      move it_bsip-wrbtr to dup_bsip-wrbtr.
      move it_bsip-wrbtr_positive to dup_bsip-wrbtr_positive.
      continue.
    endif.
    if it_bsip-lifnr <> dup_bsip-lifnr or
       it_bsip-xblnr <> dup_bsip-xblnr or
       it_bsip-wrbtr_positive <> dup_bsip-wrbtr_positive.
      if dup_bsip-count > 1.                       "save the duplicate
        append dup_bsip.
      endif.
      move-corresponding it_bsip to dup_bsip.
      move it_bsip-wrbtr to dup_bsip-wrbtr.
      move it_bsip-wrbtr_positive to dup_bsip-wrbtr_positive.
    else.                                         " duplicates can be credits or debits
      dup_bsip-count = dup_bsip-count + 1.
    endif.
  endloop.
  if dup_bsip-count > 1.                       "save the duplicate
    append dup_bsip.
  endif.

** This creates a table of all records that have duplicate
** informatation based on the variant.
*  loop at dup_bsip.
*    if dup_bsip-count > 1.
*    else.
*      delete dup_bsip.
*    endif.
*  endloop.

****  end of changes  LER 2010/01/07

* At the end of this routine, only records to be reported are left
* in IT_BSIP

  loop at it_bsip.
    read table dup_bsip
              with key lifnr = it_bsip-lifnr
                       xblnr = it_bsip-xblnr                         "LER 2010/01/07
                       wrbtr_positive = it_bsip-wrbtr_positive       "LER 2010/01/07
                       binary search.                                "LER 2010/01/07
*                       xblnr = it_bsip-xblnr                        "LER 2010/01/07
*                       wrbtr = it_bsip-wrbtr.                       "LER 2010/01/07
    if sy-subrc = '0'.
    else.
      delete it_bsip.
    endif.
  endloop.

* move to reporting table and sort

  sort it_bsip by xblnr ascending
                  wrbtr_positive ascending                         "LER 2010/01/07
*                 wrbtr ascending                                  "LER 2010/01/07
*                 budat descending.                                "LER 2010/01/07
                  budat descending                                 "LER 2010/01/07
                  belnr descending.                                "LER 2010/01/07

  data: save_budat like rit_bsip-budat.
  clear save_budat.
  loop at it_bsip.
    move it_bsip-budat to save_budat.
    at new xblnr.
      move save_budat to rit_bsip-sbudat.
    endat.

    move-corresponding it_bsip to rit_bsip.
    append rit_bsip.
  endloop.

  perform drop_docs_outside_last_days.

*  sort rit_bsip by xblnr  ascending                               "LER 2010/01/07
*                   wrbtr  ascending                               "LER 2010/01/07
*                   budat  descending.                             "LER 2010/01/07


*  Produces report
  data: test_date like bkpf-budat.
  loop at rit_bsip.

    move rit_bsip-budat to test_date.

    at new xblnr.
      uline.
    endat.

    write: /.

    if rit_bsip-budat > newdtind.
      write '   ***   ' under text-100.
    endif.

    write:  rit_bsip-xblnr under text-101,
            rit_bsip-bldat under text-102,
            rit_bsip-bukrs under text-103,
            rit_bsip-lifnr under text-104,
            rit_bsip-waers under text-105,
            rit_bsip-wrbtr under text-106,
            rit_bsip-belnr under text-107,
            rit_bsip-budat under text-109,
            rit_bsip-blart under text-110.

    at end of xblnr.
      new-line.
      uline at 113(30).
      uline.
*       write: '__________________________________' under text-108.
    endat.

  endloop.
  skip 2.
  perform print_selection_screen.

*-----------------------------------------------------------------------
form print_selection_screen.

  data:  begin of t_scr_image occurs 0,
           line(120) type c,
          end of t_scr_image.
  call function 'PRINT_SELECTIONS'
    exporting
      mode      = 'I'
      rname     = sy-cprog
      rvariante = sy-slset
    tables
      infotab   = t_scr_image.
  loop at t_scr_image.
    write : / t_scr_image-line+2.
  endloop.

endform.                    "print_selection_screen
*&---------------------------------------------------------------------*
*&      Form  DROP_DOCS_OUTSIDE_LAST_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form drop_docs_outside_last_days .

  clear tbl_keep.
  refresh tbl_keep.

* identify the documents inside the last days range
  loop at rit_bsip.
    if rit_bsip-budat >= newdtind.
      tbl_keep-xblnr = rit_bsip-xblnr.
      append tbl_keep.
    endif.

  endloop.

  sort tbl_keep.
  delete adjacent duplicates from tbl_keep.

  loop at rit_bsip.
    read table tbl_keep with key xblnr = rit_bsip-xblnr
                                 binary search.
    if sy-subrc <> 0.
      delete rit_bsip.
    endif.
  endloop.

endform.                    " DROP_DOCS_OUTSIDE_LAST_DAYS
