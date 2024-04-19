REPORT  zfapr013_reverseddoc MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       April 2011                                              *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  This program will extract reversed document information             *
************************************************************************
*CHANGES:                                                              *
* Date       By      Issue  Description                                *
* 2012/08/07 M Khan   TR995 Change C: drive to H: drive with           *
*                           directory and file selection using F4      *
************************************************************************

TYPE-POOLS: abap.
TABLES: bkpf.

DATA:  BEGIN OF ls_bkpf.
        INCLUDE STRUCTURE bkpf.
DATA:  END OF ls_bkpf.

DATA: lt_bkpf     LIKE TABLE OF ls_bkpf,
      st_datarec  TYPE string,
      t_data      LIKE TABLE OF st_datarec,
      lv_bktxt    TYPE string.


DATA: msg(80)     TYPE c,
      lv_string   TYPE string,
      lv_local(1) TYPE c,
      lv_int      TYPE integer,
      lv_premonth LIKE sy-datum,
      lv_gjahr(4) TYPE c,
      lv_monat(2) TYPE c,
      lv_sys(7)   TYPE c.

CONSTANTS: delimtr  TYPE c VALUE ',',
           c_quote  TYPE c VALUE '"'.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.


SELECT-OPTIONS:
s_bukrs     FOR bkpf-bukrs      OBLIGATORY,
s_gjahr     FOR bkpf-gjahr      OBLIGATORY,
s_monat     FOR bkpf-monat,
s_rever     FOR bkpf-xreversal  OBLIGATORY.

SELECTION-SCREEN END OF BLOCK a1.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local     RADIOBUTTON GROUP rad2
                          DEFAULT 'X',
            p_lfile     TYPE        rfpdo-rfbifile
*                         DEFAULT 'C:\SAPTEMP\Rev_Doc.csv',   "TR995
                          DEFAULT 'H:\SAPTEMP\Rev_Doc.csv',    "TR995
            p_server    RADIOBUTTON GROUP rad2,
            p_sfile     LIKE        rfpdo-rfbifile.

SELECTION-SCREEN END OF BLOCK b1.



*************************************************************************
*************************************************************************
INITIALIZATION.

  CALL FUNCTION 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'
    EXPORTING
      i_date_old = sy-datum
    IMPORTING
      e_date_new = lv_premonth.

  lv_gjahr = lv_premonth+0(4).
  lv_monat = lv_premonth+4(2).


  CLEAR: s_gjahr, s_monat.
  REFRESH: s_gjahr, s_monat.

  s_gjahr-sign    = 'I'.
  s_gjahr-option  = 'EQ'.
  s_gjahr-low     = lv_gjahr.
  s_gjahr-high    = '0000'.
  APPEND s_gjahr.

  s_monat-sign    = 'I'.
  s_monat-option  = 'EQ'.
  s_monat-low     = lv_monat.
  s_monat-high    = '00'.
  APPEND s_monat.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/IFAPR13/' INTO p_sfile.


  "Write the file name.
  CASE sy-sysid.
    WHEN 'D30' OR 'D22' OR 'P01' OR 'S01' OR 'S02' OR 'Q02'.
      lv_sys = 'EAST'.
    WHEN 'C11' OR 'Q11' OR 'P11' OR 'SBX' OR 'S11' OR 'Q12'.
      lv_sys = 'WEST'.
    WHEN OTHERS.
      lv_sys = 'UNKNOWN'.
  ENDCASE.
  CONCATENATE p_sfile 'Rev_Docs_' lv_sys '-' lv_gjahr '-' lv_monat '.csv' INTO p_sfile.

*----------------------------------------------------------------------*
*       AT SELECTION-SCREEN
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN.
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lfile.
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
      p_lfile = WIT_FILENAME_TAB.
    ELSE.
      CLEAR p_lfile.
    ENDIF.
  ENDIF.
*   PERFORM F4_FILENAME USING INFILE.
*End of TR995 changes
*************************************************************************
*************************************************************************
START-OF-SELECTION.

  IF p_local = 'X'.
    lv_local = abap_true.
  ELSE.
    lv_local = abap_false.
  ENDIF.


  PERFORM get_db_data.
  PERFORM sumarize_data.
  PERFORM print_report.


*----------------------------------------------------------------------*
FORM get_db_data.

  SELECT bukrs belnr budat gjahr blart usnam stblg bktxt
         stgrd xreversal
    INTO CORRESPONDING FIELDS OF TABLE lt_bkpf
    FROM bkpf
    WHERE bukrs     IN s_bukrs
      AND gjahr     IN s_gjahr
      AND monat     IN s_monat
      AND xreversal IN s_rever
    .

ENDFORM.                    "get_db_data


*----------------------------------------------------------------------*
FORM sumarize_data.
*  SORT t_coep ASCENDING BY gjahr perio objnr kstar.
*
*  LOOP AT t_coep INTO s_coep.
**Last sorted field is kstar
*    AT END OF kstar.
*      SUM.
*      APPEND s_coep TO t_coep_sum.
*    ENDAT.
*  ENDLOOP.

ENDFORM.                    "sumarize_data


*----------------------------------------------------------------------*
FORM print_report.

  "Header
  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011
              INTO st_datarec SEPARATED BY delimtr.

  APPEND st_datarec TO t_data.


  LOOP AT lt_bkpf INTO ls_bkpf.

    CONCATENATE c_quote ls_bkpf-bktxt c_quote INTO lv_bktxt.


    CONCATENATE lv_sys ls_bkpf-bukrs ls_bkpf-belnr ls_bkpf-budat
                ls_bkpf-gjahr ls_bkpf-blart ls_bkpf-usnam
                ls_bkpf-stblg lv_bktxt ls_bkpf-stgrd
                ls_bkpf-xreversal
                INTO st_datarec SEPARATED BY delimtr.
    APPEND st_datarec TO t_data.
  ENDLOOP.

  IF lv_local = abap_false.

    PERFORM open_serverfile.

    LOOP AT t_data INTO st_datarec.
      TRANSFER st_datarec TO p_sfile.
    ENDLOOP.

    PERFORM close_serverfile.
    WRITE: 'File Outputed Successfully to: ', p_sfile.

  ELSE.
    lv_string = p_lfile.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_string
      TABLES
        data_tab                = t_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    WRITE: 'File Outputed Successfully to: ', p_lfile.
  ENDIF.



ENDFORM.                    "print_report



*----------------------------------------------------------------------*
FORM open_serverfile.
  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_sfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "open_serverfile.


*----------------------------------------------------------------------*
FORM close_serverfile.
  CLOSE DATASET p_sfile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH 'unsuccessfl close' p_sfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "close_serverfile.
