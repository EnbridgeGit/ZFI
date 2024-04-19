REPORT ZFAPI033 MESSAGE-ID ZS line-count 65 line-size 132.
************************************************************************
*  Programmer: MaryLou De Meester
*
*  Brief Description:
*     - This ABAP is a program to extract data based on the Payment
*       Proposal
*  NOTE:  Test for DEBITS, CREDITS & DISCOUNTS
************************************************************************
* ---------------------- CHANGE LOG ------------------------------------
* 2012/08/10 M Khan   TR995 Change C: drive to H: drive with
*                           directory, file selection using F4 &
*                           move the hard-coded file path/name to
*                           variant.
*
* 2004/12/13 mdemeest #____ - If entered, used alternate payee code
*                             rather than vendor #.
* 2004/12/07 mdemeest #____ - Sum amounts by document type.  Amount
*                             figures should factor in discount.
* 2004/11/19 mdemeest #____ - New abap
*
************************************************************************
TABLES: REGUP,   "Payment Proposal
        LFA1,    "Vendor
        LFBK.    "Vendor Banking Information

*=======================================================================
* SELECTION SCREEN
*=======================================================================
selection-screen   begin of block a with frame title text-001.
parameter:  p_laufd  like regup-laufd,             "Proposal Date
            p_laufi  like regup-laufi.             "Proposal Id
selection-screen   end of block a.
selection-screen skip 2.
selection-screen  begin of block b with frame title text-002.
PARAMETERS: P_rprt  radiobutton group rbcr,        "Report Selection
            P_excl  radiobutton group rbcr.        "Excel Spreadsheet
parameter   p_file like rlgrap-filename DEFAULT 'H:\SAPTEMP\ZFAPI033'.
                                                                 "TR995
selection-screen   end of block b.

type-pools: slis.

data:  apchar(1) type x value '27'.

data:  begin of wa         occurs 0,
       laufd        like regup-laufd,
       laufi        like regup-laufi,
       zbukr        like regup-zbukr,
       lifnr(5) type c,       "like regup-lifnr,
       name1        like lfa1-name1,
       vblnr        like regup-vblnr,
*       wrbtr        like regup-wrbtr,
       wrbtr(13) type c,      "        like regup-wrbtr,
       bankl(16) type c, "        like lfbk-bankl,
       bankn        like lfbk-bankn,
       lifnra       like regup-lifnr,
       end of wa.


*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================
*AT SELECTION-SCREEN.
*  PERFORM CHECK_FILE.
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
*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.
  select * from regup
     where laufd = p_laufd
       and laufi = p_laufi
       and xvorl = ' '.
     clear wa.
     move regup-laufd to wa-laufd.
     move regup-laufi to wa-laufi.
     move regup-zbukr to wa-zbukr.
     if regup-empfg =  ' '.
        move regup-lifnr+5(5)  to wa-lifnr.
        move regup-lifnr       to wa-lifnra.
     else.
        move regup-empfg+6(5)  to wa-lifnr.
        move regup-empfg+1(10) to wa-lifnra.
     endif.
     compute wa-wrbtr = regup-wrbtr - regup-sknto.
     if regup-shkzg = 'S'.
        compute wa-wrbtr = wa-wrbtr * -1.
     endif.
*     move regup-wrbtr to wa-wrbtr.
     move regup-vblnr to wa-vblnr.
     select single * from lfa1
        where lifnr = wa-lifnra.
        if sy-subrc = '0'.
           move lfa1-name1 to wa-name1.
        endif.
     select single * from lfbk
        where lifnr = wa-lifnra.
        if sy-subrc = '0'.
           move lfbk-bankn to wa-bankn.
           if p_rprt = 'X'.
              move lfbk-bankn to wa-bankn.
              move lfbk-bankl to wa-bankl.
           else.
              concatenate apchar lfbk-bankn into wa-bankn.
              concatenate apchar lfbk-bankl into wa-bankl.

           endif.
        endif.
*     append wa.    "per Laurie Callow 2004/12/07
      collect wa.
  endselect.

sort wa by laufd laufi zbukr lifnr.


if p_rprt = 'X'.
   perform output_alv.
else.
   perform output_excel.
endif.

form output_alv.
data:  FIELDCAT  type SLIS_T_FIELDCAT_ALV,
       fc_str    type slis_fieldcat_alv,
       layout    type slis_layout_alv,
       title     type lvc_title,
       repid     like sy-repid,
       variant   like disvariant,
       sort      type slis_t_sortinfo_alv,
       sort_str  type slis_sortinfo_alv.

       repid = sy-repid.
       layout-colwidth_optimize = 'X'.
       variant-report = repid.

       clear fc_str.

       fc_str-fieldname = 'ZBUKR'.
       fc_str-key       = ' '.
       fc_str-seltext_l   = text-011.
       fc_str-ddictxt   = 'L'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'LIFNR'.
       fc_str-key       = ' '.
       fc_str-seltext_l = text-012.
       fc_str-ddictxt   = 'L'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'NAME1'.
       fc_str-key       = ' '.
       fc_str-seltext_l = text-013.
       fc_str-ddictxt   = 'L'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'VBLNR'.
       fc_str-key       = ' '.
       fc_str-seltext_l = text-017.
       fc_str-ddictxt   = 'L'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'WRBTR'.
       fc_str-key       = ' '.
       fc_str-seltext_l = text-016.
       fc_str-ddictxt   = 'L'.
       fc_str-do_sum    = 'X'.                  "Sum the amount field
       fc_str-just      = 'R'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'BANKL'.
       fc_str-key       = ' '.
       fc_str-seltext_l = text-014.
       fc_str-ddictxt   = 'L'.
       append fc_str to fieldcat.
       clear fc_str.

       fc_str-fieldname = 'BANKN'.
       fc_str-key       = ' '.
       fc_str-seltext_l = text-015.
       fc_str-ddictxt   = 'L'.
       append fc_str to fieldcat.
       clear fc_str.

* display ALV - Report
  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
          it_fieldcat     = fieldcat
          is_layout       = layout
          i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
          i_callback_program     = repid
       tables
          t_outtab               = wa
       exceptions
          program                = 1
          others                 = 2.


endform.

form alv_top_of_page.
  data: ls_line type slis_listheader.
  data: lt_top_of_page type slis_t_listheader.
  data: datum1(10).
  data: uzeit1(10).

* heading line 1
  clear ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = sy-title.
  append ls_line to lt_top_of_page.

* heading line 2
  clear ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'CLIENT: '.
  concatenate sy-sysid sy-mandt into ls_line-info separated by space.
  append ls_line to lt_top_of_page.

* head line 3
  clear ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'DATE: '.
  write sy-datum to datum1 dd/mm/yyyy.
  write sy-uzeit to uzeit1 using edit mask '__:__:__'.
  concatenate datum1 '@' uzeit1 into ls_line-info separated by space.
  append ls_line to lt_top_of_page.

* head line 4
  clear ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'FOR: '.
  write p_laufd to datum1 dd/mm/yyyy.
  concatenate datum1 ' - ' p_laufi into ls_line-info separated by space.
  append ls_line to lt_top_of_page.


  call function 'REUSE_ALV_COMMENTARY_WRITE'
       exporting
           it_list_commentary = lt_top_of_page.

endform.

form output_excel.
  data:  begin of lt_fnames occurs 0,
           text(60)  type c,
         end of lt_fnames.

*************************************
*       laufd        like regup-laufd,
*       laufi        like regup-laufi,
*       zbukr        like regup-zbukr,
*       lifnr(5) type c,       "like regup-lifnr,
*       name1        like lfa1-name1,
*       vblnr        like regup-vblnr,

*       wrbtr        like regup-wrbtr,

*       bankl(16) type c, "        like lfbk-bankl,
*       bankn        like lfbk-bankn,
*       lifnra       like regup-lifnr,
*
**************************
  lt_fnames-text = text-018.
  append lt_fnames.
  lt_fnames-text = text-019.
  append lt_fnames.
  lt_fnames-text = text-011.
  append lt_fnames.
  lt_fnames-text = text-012.
  append lt_fnames.
  lt_fnames-text = text-013.
  append lt_fnames.
  lt_fnames-text = text-017.
  append lt_fnames.
  lt_fnames-text = text-016.
  append lt_fnames.
  lt_fnames-text = text-014.
  append lt_fnames.
  lt_fnames-text = text-015.
  append lt_fnames.
  lt_fnames-text = text-012.
  append lt_fnames.

  call function 'MS_EXCEL_OLE_STANDARD_DAT'
     exporting
*       file_name               = 'C:\SAPTEMP\ZFAPI033' "TR995
        file_name               = P_FILE                "TR995
        create_pivot            = '0'
     tables
        data_tab                = wa
        fieldnames              = lt_fnames
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
    if sy-subrc <> 0.
       write: / 'ZFAPI033 Error - sy-subrc = ', sy-subrc.
    endif.

endform.
