REPORT ZFAPR007 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
      MESSAGE-ID U2.
************************************************************************
*  Author:      M DeMeester
*  Description: Produces a unique list of One-Time Vendors
*               Requested by Sherry DeWitte, Audit

************************************************************************
* 2004/04/15 mdemeest #--- Original program
************************************************************************
*CHANGES:
* 2012/08/13 M Khan   TR995 Change C: drive to H: drive with
*                           directory, file selection using F4 &
*                           move the hard-coded file path/name to
*                           variant.
************************************************************************

tables: BSEC,                 "One Time Account Data Document Segment
        bkpf,                 "Accounting Header
        BSEG.                 "Accounting Detail Segment

*----------------------  SELECTION SCREEN  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-999.
SELECTION-SCREEN SKIP 1.

parameters:
      p_lifnr like bseg-lifnr obligatory.   "One time vendor code
select-options:
      s_bukrs  for bseg-bukrs,              "Company Code
      s_gjahr  for bseg-gjahr,              "Fiscal year
      s_bldat  for bkpf-bldat,              "Document Date
      s_budat  for bkpf-budat,              "Posting Date

      s_name1  for bsec-name1.              "Vendor Name

parameters:
      p_xcpdk  like bsec-xcpdk default 'X'. "One Time Vendor Indicator
selection-screen skip 1.

selection-screen begin of block box1 with frame title text-001.
parameter:  b_rprt radiobutton group butn,   "report format
            b_excl radiobutton group butn,   "excel format
            p_file like rlgrap-filename DEFAULT 'h:\'.     "TR995
selection-screen end of block box1.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.

data: begin of xclitab              occurs 0,
      bukrs like bsec-bukrs,                 "company code
      gjahr like bsec-gjahr,                 "fiscal year
      lifnr like bseg-lifnr,                 "one-time vendor code
      belnr like bsec-belnr,                 "Document number
      name1 like bsec-name1,                 "name
      stras like bsec-stras,                 "street # and name
      ort01 like bsec-ort01,                 "city
      regio like bsec-regio,                 "province
      pstlz like bsec-pstlz,                 "postal code
      count(5) type c,                       "# of documents  TR995
*      count type i,                          "# of documents TR995
      empfg like bsec-empfg,                 "one time payee id

*      buzei like bsec-buzei,                 "line number

      end of xclitab.


*-------------------  TOP-OF-PAGE  -------------------------------------
* Report Title & Column Headings
*-----------------------------------------------------------------------
TOP-OF-PAGE.
write: /1 text-rpt, sy-repid, text-ttl,
      100 text-dte, sy-datum, text-amp, sy-uzeit.
write: / text-clt under text-rpt, sy-mandt, sy-sysid,
         text-pge under text-dte, sy-pagno under sy-datum.
uline.
write: /1 text-002, text-003, text-004, text-005,
          text-006, text-007, text-008,
          text-010, text-009.
uline.
*-------------------------  INITIALIZATION  ----------------------------
*  Check to ensure person accessing program is allowed to
*-----------------------------------------------------------------------
INITIALIZATION.

*AT SELECTION-SCREEN.
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      P_FILE = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILE.
    ENDIF.
  ENDIF.
*   PERFORM F4_FILENAME USING INFILE.
*End of TR995 changes
*---------------------- START-of-SELECTION -----------------------------
START-OF-SELECTION.
if sy-batch = 'X'.
   write: /1 'This program must be run in foreground'.
   stop.
endif.

perform select_vendor.

sort xclitab by bukrs gjahr lifnr belnr name1.
if b_rprt = 'X'.
   perform produce_report.
else.
   perform display_excel_sheet.
endif.
 .
*------------------------  SELECT_VENDOR  ------------------------------
* select all records that match the selection criteria.
*-----------------------------------------------------------------------

form select_vendor.
  select bukrs gjahr lifnr belnr
     from bseg
     into xclitab
     where bukrs in s_bukrs
       and gjahr in s_gjahr
       and lifnr = p_lifnr.
       select single * from bkpf
           where bukrs = xclitab-bukrs
             and belnr = xclitab-belnr
             and bldat in s_bldat
             and budat in s_budat.
       if sy-subrc = '0'.
          select single * from bsec
              where bukrs = xclitab-bukrs
                and belnr = xclitab-belnr
                and gjahr = xclitab-gjahr
                and name1 in s_name1
                and xcpdk = p_xcpdk.
            move 1 to xclitab-count.
            clear xclitab-belnr.             "forces summary by name
            move: bsec-name1  to xclitab-name1,
                  bsec-stras  to xclitab-stras,
                  bsec-ort01  to xclitab-ort01,
                  bsec-regio  to xclitab-regio,
                  bsec-pstlz  to xclitab-pstlz,
                  bsec-empfg  to xclitab-empfg.
*                bsec-buzei  to xclitab-buzei.
                  collect xclitab.

       endif.

 endselect.
endform.

*--------------------------  PRODUCE_REPORT  ---------------------------
* write all entries in table to report
*-----------------------------------------------------------------------
form produce_report.

  loop at xclitab.
    write:/  xclitab-bukrs under text-002, xclitab-gjahr under text-003,
             xclitab-name1 under text-004, xclitab-stras under text-005,
             xclitab-ort01 under text-006, xclitab-regio under text-007,
             xclitab-pstlz under text-008,
             xclitab-count under text-010,
             xclitab-lifnr under text-009.
  endloop.
endform.

*---------------------------  DISPLAY_EXCEL_SHEET  ---------------------
*  Display report as an excel spreadsheet - foreground only
*-----------------------------------------------------------------------
form display_excel_sheet.

data: begin of lt_fnames occurs 0,
      text(60) type c,
      end of lt_fnames.

      lt_fnames-text = text-002.
      append lt_fnames.
      lt_fnames-text = text-003.
      append lt_fnames.
      lt_fnames-text = text-009.
      append lt_fnames.
      lt_fnames-text = ' '.
      append lt_fnames.
      lt_fnames-text = text-004.
      append lt_fnames.
      lt_fnames-text = text-005.
      append lt_fnames.
      lt_fnames-text = text-006.
      append lt_fnames.
      lt_fnames-text = text-007.
      append lt_fnames.
      lt_fnames-text = text-008.
      append lt_fnames.
      lt_fnames-text = text-010.
      append lt_fnames.

      call function 'MS_EXCEL_OLE_STANDARD_DAT'
      exporting
*           FILE_NAME                 = 'C:\SAPTEMP'  "TR995
           FILE_NAME                 = p_file         "TR995
           CREATE_PIVOT              = 0
      tables
*          pivot_field_tab           =
           DATA_TAB                  = XCLITAB
           fieldnames                = lt_fnames
      exceptions
           file_not_exist            = 1
           filename_expected         = 2
           communication_error       = 3
           ole_object_method_error   = 4
           ole_object_property_error = 5
           invalid_filename          = 6
           invalid_pivot_fields      = 7
           download_problem          = 8
           others                    = 9.
       if sy-subrc <> '0'.
          write: /1 'Pivot table error, sy-subrc = ', sy-subrc.
       endif.
endform.
