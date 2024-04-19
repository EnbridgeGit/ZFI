*&---------------------------------------------------------------------*
*& Report  ZFI_UG_DEL_UNIX_FILE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFI_UG_DEL_UNIX_FILE.

TABLES: EPSF.


PARAMETERS: P_DEL RADIOBUTTON GROUP R1 DEFAULT 'X' USER-COMMAND ACT,
            P_MV  RADIOBUTTON GROUP R1.

PARAMETERS: P_DIR       TYPE EPSDIRNAM.
PARAMETERS: P_FMASK     TYPE EPSF-EPSFILNAM.
PARAMETERS: P_PCDIR     TYPE ESEFTFRONT  MODIF ID DAP.


AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.

INITIALIZATION.
  P_DIR   = '/usr/sap/interfaces/Q02/ERP_DATA_CONV'.
  P_PCDIR = '\\SAPFILESHARE.gtna.gt.ds\ERP_DATA_CONV\SAP\QA\UG_PR_QA_Q02'.

START-OF-SELECTION.

  IF P_DEL EQ 'X'.
    PERFORM DELETE_FILES.
  ELSE.
    PERFORM MOVE_FILES.
  ENDIF.


END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  DELETE_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_FILES .

  DATA: LV_FPATH     TYPE EPSF-EPSPATH,
        LV_FILENAME  TYPE EPSF-EPSFILNAM,
        LV_LONG_PATH TYPE EPS2PATH.

  DATA: LT_DIR_LIST TYPE STANDARD TABLE OF EPSFILI,
        LV_AAPL TYPE RCGFILETR-FTAPPL,
        LV_FRONTEND TYPE STRING,
        LV_OPEN_ERROR TYPE ESP1_BOOLEAN,
        LV_OS_MESSAGE TYPE C.

  FIELD-SYMBOLS: <DIR_LIST> LIKE LINE OF LT_DIR_LIST.

  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      DIR_NAME               = P_DIR
      FILE_MASK              = P_FMASK
    TABLES
      DIR_LIST               = LT_DIR_LIST
    EXCEPTIONS
      INVALID_EPS_SUBDIR     = 1
      SAPGPARAM_FAILED       = 2
      BUILD_DIRECTORY_FAILED = 3
      NO_AUTHORIZATION       = 4
      READ_DIRECTORY_FAILED  = 5
      TOO_MANY_READ_ERRORS   = 6
      EMPTY_DIRECTORY_LIST   = 7
      OTHERS                 = 8.

  LOOP AT LT_DIR_LIST ASSIGNING <DIR_LIST>.
    CLEAR: LV_FPATH, LV_LONG_PATH.
    LV_FILENAME = <DIR_LIST>-NAME.
    CALL FUNCTION 'EPS_DELETE_FILE'
      EXPORTING
        FILE_NAME              = LV_FILENAME
        DIR_NAME               = P_DIR
      IMPORTING
        FILE_PATH              = LV_FPATH
        EV_LONG_FILE_PATH      = LV_LONG_PATH
      EXCEPTIONS
        INVALID_EPS_SUBDIR     = 1
        SAPGPARAM_FAILED       = 2
        BUILD_DIRECTORY_FAILED = 3
        NO_AUTHORIZATION       = 4
        BUILD_PATH_FAILED      = 5
        DELETE_FAILED          = 6
        OTHERS                 = 7.
    IF SY-SUBRC EQ 0.
      WRITE: / LV_FPATH, ' - Deleted'.
    ELSE.
      WRITE: / 'Delete Error - ', LV_FPATH.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DELETE_FILES

*&---------------------------------------------------------------------*
*&      Form  MOVE_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOVE_FILES .



  TYPES: T_LINE(1) TYPE X.

  DATA: LT_DIR_LIST   TYPE STANDARD TABLE OF EPSFILI,
        LV_AAPL       TYPE RCGFILETR-FTAPPL,
        LV_FRONTEND   TYPE STRING,
        LV_OPEN_ERROR TYPE ESP1_BOOLEAN,
        LV_OS_MESSAGE TYPE C,
        I_TAB         TYPE STANDARD TABLE OF T_LINE,
        I_WA(1)       TYPE X,
        LV_FN         TYPE STRING.

  FIELD-SYMBOLS: <DIR_LIST> LIKE LINE OF LT_DIR_LIST.

  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      DIR_NAME               = P_DIR
      FILE_MASK              = P_FMASK
    TABLES
      DIR_LIST               = LT_DIR_LIST
    EXCEPTIONS
      INVALID_EPS_SUBDIR     = 1
      SAPGPARAM_FAILED       = 2
      BUILD_DIRECTORY_FAILED = 3
      NO_AUTHORIZATION       = 4
      READ_DIRECTORY_FAILED  = 5
      TOO_MANY_READ_ERRORS   = 6
      EMPTY_DIRECTORY_LIST   = 7
      OTHERS                 = 8.

  LOOP AT LT_DIR_LIST ASSIGNING <DIR_LIST>.
    CONCATENATE P_DIR <DIR_LIST>-NAME   INTO LV_AAPL SEPARATED BY '/'.
    CONCATENATE P_PCDIR <DIR_LIST>-NAME INTO LV_FRONTEND SEPARATED BY '\'.
*      CALL FUNCTION 'C13Z_FILE_DOWNLOAD_ASCII'
*        EXPORTING
*          i_file_front_end    = lv_frontend
*          i_file_appl         = lv_aapl
*          i_file_overwrite    = esp1_true
*        IMPORTING
*          e_flg_open_error    = lv_open_error
*          e_os_message        = lv_os_message
*        EXCEPTIONS
*          fe_file_open_error  = 1
*          fe_file_exists      = 2
*          fe_file_write_error = 3
*          ap_no_authority     = 4
*          ap_file_open_error  = 5
*          ap_file_empty       = 6
*          OTHERS              = 7.

    OPEN DATASET LV_AAPL FOR INPUT IN BINARY MODE.
    DO.
      CLEAR I_WA.
      READ DATASET LV_AAPL INTO I_WA.
      IF SY-SUBRC <> 0.
        EXIT.
      ELSE.
        APPEND I_WA TO I_TAB.
      ENDIF.
    ENDDO.
    CLOSE DATASET LV_AAPL.

    LV_FN = LV_FRONTEND.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
      EXPORTING
        FILENAME                = LV_FN
        FILETYPE                = 'BIN'
        APPEND                  = ' '
      CHANGING
        DATA_TAB                = I_TAB
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        OTHERS                  = 24.

    IF SY-SUBRC EQ 0.
      WRITE: / LV_AAPL, '- Copied into -',LV_FRONTEND.
    ELSE.
      WRITE: / 'Error - ', LV_AAPL.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MOVE_FILES

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN.

  LOOP AT SCREEN.
    IF P_DEL EQ 'X'.
      IF SCREEN-GROUP1 EQ 'DAP'.
        SCREEN-INPUT     = 0.   "Hidden
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.

    ELSEIF P_MV EQ 'X'.
      IF SCREEN-GROUP1 EQ 'DAP'.
        SCREEN-INPUT     = 1.   "Visible
        SCREEN-INVISIBLE = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN
