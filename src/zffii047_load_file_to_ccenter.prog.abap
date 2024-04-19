REPORT zffii047_load_file_to_ccenter.
************************************************************************
*  Author:      Jyoti Sharma                                           *
*  Date:        09 ,sep, 2014.                                         *
*  Description:                                                        *
*     - The purpose of this program is to upload plan data for Cost    *
*       Center using excel file as input.                              *
*       This program must be run foreground.                           *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
*  Copied from SAP rkplub01 --- standard template for customizing      *
*  by customers for their own plan file layouts.                       *
*----------------------------------------------------------------------*
************************************************************************
TABLES:
  cska,         "Cost Elements (Data Dependent on Chart of Accounts
  aufk.         "Order master data

* Internal table for plan records
DATA: BEGIN OF irku01ja OCCURS 20.
        INCLUDE STRUCTURE rku01ja.
DATA: END OF irku01ja.
DATA: BEGIN OF irku01ja_tmp OCCURS 20.
        INCLUDE STRUCTURE rku01ja.
DATA: END OF irku01ja_tmp.

DATA: BEGIN OF zrku01_cur.
        INCLUDE STRUCTURE rku01_cur.
DATA: END OF zrku01_cur.

DATA: type LIKE rlgrap-filetype VALUE 'ASC',
      msg(100) TYPE c.

DATA:  reccnt TYPE p VALUE 0,
       comcnt TYPE p VALUE 0,
       errord TYPE p VALUE 0,
       erract TYPE p VALUE 0,
       w_vrgng LIKE cosp-vrgng VALUE 'RKP1'.



TYPES: BEGIN OF kcde_intern_struc.
        INCLUDE STRUCTURE  kcde_cells.
TYPES: END OF kcde_intern_struc.

DATA: exceltab TYPE kcde_intern_struc OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
*  Here is the layout and fields for the INREC file                    *
*----------------------------------------------------------------------*
DATA: BEGIN OF inrec OCCURS 100,
           kostl     TYPE csks-kostl, "AUFNR     LIKE AUFK-AUFNR,
           kstar     LIKE aufk-kstar,
           jan(16)   TYPE c,
           feb(16)   TYPE c,
           mar(16)   TYPE c,
           apr(16)   TYPE c,
           may(16)   TYPE c,
           jun(16)   TYPE c,
           jul(16)   TYPE c,
           aug(16)   TYPE c,
           sep(16)   TYPE c,
           oct(16)   TYPE c,
           nov(16)   TYPE c,
           dec(16)   TYPE c,
     END OF inrec.

*----------------------------------------------------------------------*
*  Used to capture orders master data errors                           *
*----------------------------------------------------------------------*
DATA: BEGIN OF orderr OCCURS 100,
           kostl     LIKE csks-kostl,
           kstar     LIKE aufk-kstar,
           jan(16)   TYPE c,
           feb(16)   TYPE c,
           mar(16)   TYPE c,
           apr(16)   TYPE c,
           may(16)   TYPE c,
           jun(16)   TYPE c,
           jul(16)   TYPE c,
           aug(16)   TYPE c,
           sep(16)   TYPE c,
           oct(16)   TYPE c,
           nov(16)   TYPE c,
           dec(16)   TYPE c,
 END OF orderr.

*----------------------------------------------------------------------*
*  Used to capture errors for g/l accounts or cost elements            *
*----------------------------------------------------------------------*
DATA: BEGIN OF acterr OCCURS 100,
           kostl     LIKE csks-kostl,
           kstar     LIKE cska-kstar,
           jan(16)   TYPE c,
           feb(16)   TYPE c,
           mar(16)   TYPE c,
           apr(16)   TYPE c,
           may(16)   TYPE c,
           jun(16)   TYPE c,
           jul(16)   TYPE c,
           aug(16)   TYPE c,
           sep(16)   TYPE c,
           oct(16)   TYPE c,
           nov(16)   TYPE c,
           dec(16)   TYPE c,
 END OF acterr.

*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-000.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 25(25) text-105.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(75) text-106.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN END OF BLOCK box2.
PARAMETERS:
     p_kokrs LIKE csks-kokrs DEFAULT '10',
     p_versn LIKE cosp-versn OBLIGATORY,
     p_gjahr LIKE cosp-gjahr DEFAULT sy-datum+0(4),
     perab LIKE rku01ja-perab DEFAULT '01',
     perbi LIKE rku01ja-perbi DEFAULT '12',
     file_in LIKE rlgrap-filename,
*             DEFAULT 'H:\saptemp\ordersplan.xls' , "TR995
*            DEFAULT 'C:\saptemp\ordersplan.xls' , "TR995
     p_delta(1) TYPE c NO-DISPLAY,
     p_commit(1) TYPE c DEFAULT 'X' NO-DISPLAY,
     p_update(1) TYPE c DEFAULT 'X' NO-DISPLAY.
SELECTION-SCREEN SKIP 1.
PARAMETERS:
     p_rows      TYPE i DEFAULT 999 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK box1.

*----------------------------------------------------------------------*
*************************************************************************
*Start Of TR995 changes
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_in.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      file_in = wit_filename_tab.
    ELSE.
      CLEAR file_in.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON file_in.
  PERFORM check_file_path.
*End of TR995 changes
*----------------------------------------------------------------------*
*  Upload EXCEL data                                                   *
*----------------------------------------------------------------------*
  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      filename                = file_in
      i_begin_col             = 1
      i_begin_row             = 6
      i_end_col               = 14
      i_end_row               = 999
    TABLES
      intern                  = exceltab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
*IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_FOR_INTERACTION'
      EXPORTING
        headline = '!! ERROR !!'
        text1    = 'Unsuccessful EXCEL Upload '
        text2    = 'Please check the file path/name and try again'
        text3    = ' '
        text4    = 'Press OK Button to Continue'
        button_1 = 'OK'.
    STOP.
  ENDIF.

*    SKIP 2.
*       WRITE: / 'SY-SUBRC = ', SY-SUBRC, ' UNSUCCESSFUL EXCEL UPLOAD'.
*    SKIP 2.
*    STOP.
* ENDIF.

  LOOP AT exceltab.
    CASE exceltab-col.

      WHEN 1.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = exceltab-value
          IMPORTING
            output = inrec-kostl.
*        CONCATENATE '000000' exceltab-value INTO inrec-kostl."inrec-aufnr.
      WHEN 2.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = exceltab-value
          IMPORTING
            output = inrec-kstar.

*      CONCATENATE '0000' exceltab-value INTO inrec-kstar.

      WHEN 3.  MOVE exceltab-value TO inrec-jan.
      WHEN 4.  MOVE exceltab-value TO inrec-feb.
      WHEN 5.  MOVE exceltab-value TO inrec-mar.
      WHEN 6.  MOVE exceltab-value TO inrec-apr.
      WHEN 7.  MOVE exceltab-value TO inrec-may.
      WHEN 8.  MOVE exceltab-value TO inrec-jun.
      WHEN 9.  MOVE exceltab-value TO inrec-jul.
      WHEN 10. MOVE exceltab-value TO inrec-aug.
      WHEN 11. MOVE exceltab-value TO inrec-sep.
      WHEN 12. MOVE exceltab-value TO inrec-oct.
      WHEN 13. MOVE exceltab-value TO inrec-nov.
      WHEN 14. MOVE exceltab-value TO inrec-dec.
      WHEN OTHERS.
    ENDCASE.
    AT END OF row.
      IF inrec-kostl <> space.
        APPEND inrec.
      ENDIF.
      CLEAR  inrec.
    ENDAT.
  ENDLOOP.

  LOOP AT inrec.
    ADD 1 TO reccnt.
    ADD 1 TO comcnt.
  ENDLOOP.

*----------------------------------------------------------------------*
*  The data just read into INREC should be checked for master data     *
*  errors in both cost centres and accounts.  Will be done with 2 forms*
*  so error types can be distinguished. Two further files for          *
*  processing will be created.                                         *
*----------------------------------------------------------------------*
  LOOP AT inrec.
    PERFORM check_sap_order_number.
  ENDLOOP.

  LOOP AT inrec.
    PERFORM check_sap_cost_element.
  ENDLOOP.

*----------------------------------------------------------------------*
*  Move the valid records to the plan record structure IRKU01JA which  *
*  will be passed to the function K_COSTS_PLAN_INTERFACE_PERIOD        *
*----------------------------------------------------------------------*

  LOOP AT inrec.
    irku01ja-gjahr  = p_gjahr.
    irku01ja-twaer  = 'CAD'.          "Currency
    irku01ja-kostl  = inrec-kostl.
    irku01ja-kstar  = inrec-kstar.
    irku01ja-wtg001 = inrec-jan.
    irku01ja-wtg002 = inrec-feb.
    irku01ja-wtg003 = inrec-mar.
    irku01ja-wtg004 = inrec-apr.
    irku01ja-wtg005 = inrec-may.
    irku01ja-wtg006 = inrec-jun.
    irku01ja-wtg007 = inrec-jul.
    irku01ja-wtg008 = inrec-aug.
    irku01ja-wtg009 = inrec-sep.
    irku01ja-wtg010 = inrec-oct.
    irku01ja-wtg011 = inrec-nov.
    irku01ja-wtg012 = inrec-dec.
    APPEND irku01ja.
  ENDLOOP.

  zrku01_cur-wtg_man = 'X'.

  irku01ja_tmp[] =  irku01ja[].

*3. call plan interface for primary costs
  CALL FUNCTION 'K_COSTS_PLAN_INTERFACE_PERIOD'
    EXPORTING
      bltxt            = 'Plan Data Load'
      delta            = p_delta
      gjahr            = p_gjahr
      kokrs            = p_kokrs
      messages_show    = 'X'
      perab            = perab
      perbi            = perbi
*     RPLAN            = 'CO-PLAN1'
      versn            = p_versn
      vrgng            = w_vrgng
      irku01_cur       = zrku01_cur
      commit           = p_commit
      update_values    = p_update
    TABLES
      irku01ja         = irku01ja
    EXCEPTIONS
      messages_occured = 01.

  IF sy-subrc is INITIAL.
*  *success
    PERFORM display_alv.

  ELSE.
*----------------------------------------------------------------------*
*  Write statistics to the screen separated by error type              *
*----------------------------------------------------------------------*
*    IF p_commit = ' '.
    WRITE:   / '*** Test run of plan file load, no records loaded ***'(007).

*    ENDIF.
    SKIP 1.

    WRITE: / text-dte, sy-datum.                           "Date
    WRITE: / text-amp, sy-uzeit.                           "Time
    WRITE: / text-clt, sy-mandt, sy-sysid.                 "Client
    WRITE: / text-101, p_versn.
    WRITE: / text-102, p_gjahr.
    WRITE: / text-103, perab.
    WRITE: / text-104, perbi.
    SKIP 2.

    WRITE:    / 'Records read from source file:', 26 file_in.
    WRITE:    / 'Number of Records:', reccnt.
    SKIP 1.

    WRITE:    / 'I/O   ', 13 'Account'.
    LOOP AT inrec.
      WRITE:    / inrec-kostl, inrec-kstar.
    ENDLOOP.
    SKIP 2.

    WRITE:    / 'Not Existing Cost Center :', errord.

    IF NOT orderr[] IS INITIAL.
      WRITE:    / 'I/O   ', 13 'Account'.
      LOOP AT orderr.
        WRITE:    / orderr-kostl, orderr-kstar.
      ENDLOOP.
    ENDIF.

    SKIP 2.
    WRITE:    / 'Not Existing Cost Elements:', erract.
    IF NOT acterr[] IS INITIAL.
      WRITE:    / 'I/O   ', 13 'Account'.
      LOOP AT acterr.
        WRITE:    / acterr-kostl, acterr-kstar.
      ENDLOOP.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------------*
*  Check to see if order # on record in INREC is valid                 *
*----------------------------------------------------------------------*
FORM check_sap_order_number.
  DATA w_kostl TYPE kostl.
  SELECT SINGLE kostl INTO w_kostl
    FROM csks
    WHERE kostl = inrec-kostl.

  IF sy-subrc NE 0.
    ADD 1 TO errord.
    MOVE-CORRESPONDING inrec TO orderr.
    APPEND orderr.
    DELETE inrec.
    ADD -1 TO comcnt.
  ENDIF.
ENDFORM.                    "CHECK_SAP_ORDER_NUMBER

*----------------------------------------------------------------------*
*  It was indicated on the parameters for running the program that the *
*  plan file was created using the SAP Chart of Accounts.  Are all of  *
*  the accounts used valid SAP accounts?                               *
*----------------------------------------------------------------------*
FORM check_sap_cost_element.
  SELECT SINGLE * FROM cska
    WHERE ktopl = 'COAT'
      AND kstar = inrec-kstar.
  IF sy-subrc = 0.
    EXIT.
  ELSE.
    ADD 1 TO erract.
    MOVE-CORRESPONDING inrec TO acterr.
    APPEND acterr.
    DELETE inrec.
    ADD -1 TO comcnt.
  ENDIF.
ENDFORM.                    "CHECK_SAP_COST_ELEMENT

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = file_in
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
  TYPE-POOLS:
   slis.

  DATA:
   t_fieldcat TYPE slis_t_fieldcat_alv,
   fs_fieldcat LIKE LINE OF t_fieldcat,
   fs_layout TYPE slis_layout_alv ,
   w_color(3) ,
   w_row TYPE i,
   w_fieldname(20),
   lt_events TYPE slis_t_event,
  lw_events LIKE LINE OF lt_events,

   w_prog TYPE sy-repid.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'GJAHR'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 1.
  fs_fieldcat-seltext_l = 'Year'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'TWAER'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 2.
  fs_fieldcat-seltext_l = 'Currency'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'KOSTL'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 3.
  fs_fieldcat-seltext_l = 'Cost Center'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'KSTAR'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 4.
  fs_fieldcat-seltext_l = 'Cost Element'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG001'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 5.
  fs_fieldcat-seltext_l = 'Jan'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG002'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 6.
  fs_fieldcat-seltext_l = 'Feb'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG003'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 7.
  fs_fieldcat-seltext_l = 'March'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG004'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 8.
  fs_fieldcat-seltext_l = 'April'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG005'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 9.
  fs_fieldcat-seltext_l = 'May'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG006'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 10.
  fs_fieldcat-seltext_l = 'June'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG007'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 11.
  fs_fieldcat-seltext_l = 'July'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG008'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 12.
  fs_fieldcat-seltext_l = 'Aug'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG009'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 13.
  fs_fieldcat-seltext_l = 'Sep'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG010'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 14.
  fs_fieldcat-seltext_l = 'Oct'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG011'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 15.
  fs_fieldcat-seltext_l = 'Nov'.
  APPEND fs_fieldcat TO t_fieldcat.

  CLEAR : fs_fieldcat.
  fs_fieldcat-fieldname = 'WTG012'.
  fs_fieldcat-tabname = 'IRKU01JA_TMP'.
  fs_fieldcat-col_pos = 16.
  fs_fieldcat-seltext_l = 'Dec'.
  APPEND fs_fieldcat TO t_fieldcat.


  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = w_prog
      is_layout          = fs_layout
      it_fieldcat        = t_fieldcat
    TABLES
      t_outtab           = irku01ja_tmp
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DISPLAY_ALV
*----------------------------------------------------------------------*
*                                                                      *
*                END OF PROGRAM                                        *
*                                                                      *
*----------------------------------------------------------------------*
