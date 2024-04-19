FUNCTION ZFI_EXCEL_TO_RICEF.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FILENAME) TYPE  RLGRAP-FILENAME
*"     REFERENCE(I_BEGIN_COL) TYPE  I
*"     REFERENCE(I_BEGIN_ROW) TYPE  I
*"     REFERENCE(I_END_COL) TYPE  I
*"     REFERENCE(I_END_ROW) TYPE  I
*"     REFERENCE(SHEETNAME) TYPE  I
*"  TABLES
*"      INTERN STRUCTURE  ZFI_ALSMEX_TABLINE
*"  EXCEPTIONS
*"      INCONSISTENT_PARAMETERS
*"      UPLOAD_OLE
*"----------------------------------------------------------------------

DATA: excel_tab     TYPE  ty_t_sender.
  DATA: ld_separator  TYPE  c.
  DATA: application   TYPE  ole2_object,
        workbook      TYPE  ole2_object,
        range         TYPE  ole2_object,
        worksheet     TYPE  ole2_object.
  DATA: h_cell        TYPE  ole2_object,
        h_cell1       TYPE  ole2_object.
  DATA: ld_rc             TYPE i.

  DEFINE m_message.
    case sy-subrc.
      when 0.
      when 1.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      when others. raise upload_ole.
    endcase.
  END-OF-DEFINITION.


* check parameters
  IF i_begin_row > i_end_row. RAISE inconsistent_parameters. ENDIF.
  IF i_begin_col > i_end_col. RAISE inconsistent_parameters. ENDIF.

* Get TAB-sign for separation of fields
  CLASS cl_abap_char_utilities DEFINITION LOAD.
  ld_separator = cl_abap_char_utilities=>horizontal_tab.

* open file in Excel
  IF application-header = space OR application-handle = -1.
    CREATE OBJECT application 'Excel.Application'.
    m_message.
  ENDIF.
  CALL METHOD OF application 'Workbooks' = workbook.
  m_message.
  CALL METHOD OF workbook 'Open' EXPORTING #1 = filename.
  m_message.

*  set property of application 'Visible' = 1.
*  m_message.

*- Insert Begin - VRAJAMAN - 10/09/2006
  CALL METHOD OF application 'Worksheets' =
    worksheet EXPORTING #1 = sheetname.
  m_message.

  CALL METHOD OF worksheet 'Activate'.
  m_message.
*- End of Insert - VRAJAMAN - 10/09/2006

  GET PROPERTY OF application 'ACTIVESHEET' = worksheet.
  m_message.

* mark whole spread sheet
  CALL METHOD OF worksheet 'Cells' = h_cell
    EXPORTING #1 = i_begin_row #2 = i_begin_col.
  m_message.

  CALL METHOD OF worksheet 'Cells' = h_cell1
    EXPORTING #1 = i_end_row #2 = i_end_col.
  m_message.

  CALL METHOD OF worksheet 'RANGE' = range
    EXPORTING #1 = h_cell #2 = h_cell1.
  m_message.

  CALL METHOD OF range 'SELECT'.
  m_message.

* copy marked area (whole spread sheet) into Clippboard
  CALL METHOD OF range 'COPY'.
  m_message.

* read clipboard into ABAP
  CALL METHOD cl_gui_frontend_services=>clipboard_import
    IMPORTING
      data       = excel_tab
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 4.
  IF sy-subrc <> 0.
    MESSAGE a037(alsmex).
  ENDIF.
* Begin of changes for EMEA Wave 0
*  PERFORM SEPARATED_TO_INTERN_CONVERT TABLES EXCEL_TAB INTERN
*                                      USING  LD_SEPARATOR.
  PERFORM separated_to_intern_convert1 TABLES excel_tab intern
                                      USING  ld_separator.
* End of changes for EMEA Wave 0
* clear clipboard
  REFRESH excel_tab.
  CALL METHOD cl_gui_frontend_services=>clipboard_export
    IMPORTING
      data       = excel_tab
    CHANGING
      rc         = ld_rc
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 4.
* quit Excel and free ABAP Object - unfortunately, this does not kill
* the Excel process
  CALL METHOD OF application 'QUIT'.
  m_message.

  FREE OBJECT h_cell.       m_message.
  FREE OBJECT h_cell1.      m_message.
  FREE OBJECT range.        m_message.
  FREE OBJECT worksheet.    m_message.
  FREE OBJECT workbook.     m_message.
  FREE OBJECT application.  m_message.




ENDFUNCTION.
