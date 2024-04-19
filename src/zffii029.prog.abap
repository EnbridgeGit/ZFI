REPORT ZFFII029.
************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        APRIL, 2006.                                           *
*  Track # :    TR240                                                  *
*  Description:                                                        *
*     - The purpose of this program is to upload plan data for internal*
*       orders using excel file as input.                              *
*       This program must be run foreground.                           *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
*  Copied from SAP rkplub01 --- standard template for customizing      *
*  by customers for their own plan file layouts.                       *
*----------------------------------------------------------------------*
*Date       By       Issue Description                                 *
*2012/07/31 M Khan   TR995 Change C: drive to H: drive with            *
*                           directory, file selection using F4         *
*                                                                      *
************************************************************************
TABLES:
  CSKA,         "Cost Elements (Data Dependent on Chart of Accounts
  AUFK.         "Order master data

* Internal table for plan records
DATA: BEGIN OF IRKU01JA OCCURS 20.
        INCLUDE STRUCTURE RKU01JA.
DATA: END OF IRKU01JA.

DATA: BEGIN OF ZRKU01_CUR.
        INCLUDE STRUCTURE RKU01_CUR.
DATA: END OF ZRKU01_CUR.

DATA: TYPE LIKE RLGRAP-FILETYPE VALUE 'ASC',
      MSG(100) TYPE C.

DATA:  RECCNT TYPE P VALUE 0,
       COMCNT TYPE P VALUE 0,
       ERRORD TYPE P VALUE 0,
       ERRACT TYPE P VALUE 0,
       W_VRGNG LIKE COSP-VRGNG VALUE 'RKP1'.



TYPES: BEGIN OF KCDE_INTERN_STRUC.
          INCLUDE STRUCTURE  KCDE_CELLS.
TYPES: END OF KCDE_INTERN_STRUC.

DATA: EXCELTAB TYPE KCDE_INTERN_STRUC OCCURS 0 with header line.

*----------------------------------------------------------------------*
*  Here is the layout and fields for the INREC file                    *
*----------------------------------------------------------------------*
DATA: BEGIN OF INREC OCCURS 100,
           AUFNR     LIKE AUFK-AUFNR,
           KSTAR     LIKE AUFK-KSTAR,
           JAN(16)   TYPE C,
           FEB(16)   TYPE C,
           MAR(16)   TYPE C,
           APR(16)   TYPE C,
           MAY(16)   TYPE C,
           JUN(16)   TYPE C,
           JUL(16)   TYPE C,
           AUG(16)   TYPE C,
           SEP(16)   TYPE C,
           OCT(16)   TYPE C,
           NOV(16)   TYPE C,
           DEC(16)   TYPE C,
     END OF INREC.

*----------------------------------------------------------------------*
*  Used to capture orders master data errors                           *
*----------------------------------------------------------------------*
DATA: BEGIN OF ORDERR OCCURS 100,
           AUFNR     LIKE AUFK-AUFNR,
           KSTAR     LIKE AUFK-KSTAR,
           JAN(16)   TYPE C,
           FEB(16)   TYPE C,
           MAR(16)   TYPE C,
           APR(16)   TYPE C,
           MAY(16)   TYPE C,
           JUN(16)   TYPE C,
           JUL(16)   TYPE C,
           AUG(16)   TYPE C,
           SEP(16)   TYPE C,
           OCT(16)   TYPE C,
           NOV(16)   TYPE C,
           DEC(16)   TYPE C,
 END OF ORDERR.

*----------------------------------------------------------------------*
*  Used to capture errors for g/l accounts or cost elements            *
*----------------------------------------------------------------------*
DATA: BEGIN OF ACTERR OCCURS 100,
           AUFNR     LIKE AUFK-AUFNR,
           KSTAR     LIKE AUFK-KSTAR,
           JAN(16)   TYPE C,
           FEB(16)   TYPE C,
           MAR(16)   TYPE C,
           APR(16)   TYPE C,
           MAY(16)   TYPE C,
           JUN(16)   TYPE C,
           JUL(16)   TYPE C,
           AUG(16)   TYPE C,
           SEP(16)   TYPE C,
           OCT(16)   TYPE C,
           NOV(16)   TYPE C,
           DEC(16)   TYPE C,
 END OF ACTERR.

*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 25(25) TEXT-105.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(75) TEXT-106.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN END OF BLOCK BOX2.
PARAMETERS:
     P_KOKRS LIKE CSKS-KOKRS DEFAULT '10',
     P_VERSN LIKE COSP-VERSN OBLIGATORY,
     P_GJAHR LIKE COSP-GJAHR DEFAULT SY-DATUM+0(4),
     PERAB LIKE RKU01JA-PERAB DEFAULT '01',
     PERBI LIKE RKU01JA-PERBI DEFAULT '12',
     FILE_IN LIKE RLGRAP-FILENAME
             DEFAULT 'H:\saptemp\ordersplan.xls' , "TR995
*            DEFAULT 'C:\saptemp\ordersplan.xls' , "TR995
     P_DELTA(1) TYPE C,
     P_COMMIT(1) TYPE C DEFAULT 'X',
     P_UPDATE(1) TYPE C DEFAULT 'X'.
SELECTION-SCREEN SKIP 1.
PARAMETERS:
     P_ROWS      TYPE I DEFAULT 999 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX1.

*----------------------------------------------------------------------*
*************************************************************************
*Start Of TR995 changes
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILE_IN.
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
      FILE_IN = WIT_FILENAME_TAB.
    ELSE.
      CLEAR FILE_IN.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON FILE_IN.
  PERFORM CHECK_FILE_PATH.
*End of TR995 changes
*----------------------------------------------------------------------*
*  Upload EXCEL data                                                   *
*----------------------------------------------------------------------*
CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
  EXPORTING
    FILENAME                      = FILE_IN
    I_BEGIN_COL                   = 1
    I_BEGIN_ROW                   = 6
    I_END_COL                     = 14
    I_END_ROW                     = 999
  TABLES
    INTERN                        = EXCELTAB
 EXCEPTIONS
   INCONSISTENT_PARAMETERS       = 1
   UPLOAD_OLE                    = 2
   OTHERS                        = 3
          .
*IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

 IF SY-SUBRC <> 0.
    CALL FUNCTION 'POPUP_FOR_INTERACTION'
         EXPORTING
         HEADLINE    = '!! ERROR !!'
         TEXT1       = 'Unsuccessful EXCEL Upload '
         TEXT2       = 'Please check the file path/name and try again'
         TEXT3       = ' '
         TEXT4       = 'Press OK Button to Continue'
         BUTTON_1    = 'OK'.
     STOP.
 ENDIF.

*    SKIP 2.
*       WRITE: / 'SY-SUBRC = ', SY-SUBRC, ' UNSUCCESSFUL EXCEL UPLOAD'.
*    SKIP 2.
*    STOP.
* ENDIF.

 LOOP AT EXCELTAB.
      CASE EXCELTAB-COL.
          WHEN 1.  CONCATENATE '000000' EXCELTAB-VALUE INTO INREC-AUFNR.
          WHEN 2.  CONCATENATE '0000' EXCELTAB-VALUE INTO INREC-KSTAR.
          WHEN 3.  MOVE EXCELTAB-VALUE TO INREC-JAN.
          WHEN 4.  MOVE EXCELTAB-VALUE TO INREC-FEB.
          WHEN 5.  MOVE EXCELTAB-VALUE TO INREC-MAR.
          WHEN 6.  MOVE EXCELTAB-VALUE TO INREC-APR.
          WHEN 7.  MOVE EXCELTAB-VALUE TO INREC-MAY.
          WHEN 8.  MOVE EXCELTAB-VALUE TO INREC-JUN.
          WHEN 9.  MOVE EXCELTAB-VALUE TO INREC-JUL.
          WHEN 10. MOVE EXCELTAB-VALUE TO INREC-AUG.
          WHEN 11. MOVE EXCELTAB-VALUE TO INREC-SEP.
          WHEN 12. MOVE EXCELTAB-VALUE TO INREC-OCT.
          WHEN 13. MOVE EXCELTAB-VALUE TO INREC-NOV.
          WHEN 14. MOVE EXCELTAB-VALUE TO INREC-DEC.
          WHEN OTHERS.
      ENDCASE.
      AT END OF ROW.
         IF INREC-AUFNR <> SPACE.
            APPEND INREC.
         ENDIF.
         CLEAR  INREC.
      ENDAT.
 ENDLOOP.

LOOP AT INREC.
  ADD 1 TO RECCNT.
  ADD 1 TO COMCNT.
ENDLOOP.

*----------------------------------------------------------------------*
*  The data just read into INREC should be checked for master data     *
*  errors in both cost centres and accounts.  Will be done with 2 forms*
*  so error types can be distinguished. Two further files for          *
*  processing will be created.                                         *
*----------------------------------------------------------------------*
LOOP AT INREC.
  PERFORM CHECK_SAP_ORDER_NUMBER.
ENDLOOP.

LOOP AT INREC.
    PERFORM CHECK_SAP_COST_ELEMENT.
ENDLOOP.

*----------------------------------------------------------------------*
*  Move the valid records to the plan record structure IRKU01JA which  *
*  will be passed to the function K_COSTS_PLAN_INTERFACE_PERIOD        *
*----------------------------------------------------------------------*

LOOP AT INREC.
  IRKU01JA-GJAHR  = P_GJAHR.
  IRKU01JA-TWAER  = 'CAD'.          "Currency
  IRKU01JA-AUFNR  = INREC-AUFNR.
  IRKU01JA-KSTAR  = INREC-KSTAR.
  IRKU01JA-WTG001 = INREC-JAN.
  IRKU01JA-WTG002 = INREC-FEB.
  IRKU01JA-WTG003 = INREC-MAR.
  IRKU01JA-WTG004 = INREC-APR.
  IRKU01JA-WTG005 = INREC-MAY.
  IRKU01JA-WTG006 = INREC-JUN.
  IRKU01JA-WTG007 = INREC-JUL.
  IRKU01JA-WTG008 = INREC-AUG.
  IRKU01JA-WTG009 = INREC-SEP.
  IRKU01JA-WTG010 = INREC-OCT.
  IRKU01JA-WTG011 = INREC-NOV.
  IRKU01JA-WTG012 = INREC-DEC.
  APPEND IRKU01JA.
ENDLOOP.

ZRKU01_CUR-WTG_MAN = 'X'.

*3. call plan interface for primary costs
CALL FUNCTION 'K_COSTS_PLAN_INTERFACE_PERIOD'
     EXPORTING
          BLTXT            = 'Plan Data Load'
          DELTA            = P_DELTA
          GJAHR            = P_GJAHR
          KOKRS            = P_KOKRS
          MESSAGES_SHOW    = 'X'
          PERAB            = PERAB
          PERBI            = PERBI
*          RPLAN            = 'CO-PLAN1'
          VERSN            = P_VERSN
          VRGNG            = W_VRGNG
          IRKU01_CUR       = ZRKU01_CUR
          COMMIT           = P_COMMIT
          UPDATE_VALUES    = P_UPDATE
     TABLES
          IRKU01JA         = IRKU01JA
     EXCEPTIONS
          MESSAGES_OCCURED = 01.

*----------------------------------------------------------------------*
*  Write statistics to the screen separated by error type              *
*----------------------------------------------------------------------*
IF P_COMMIT = ' '.
WRITE:   / '*** Test run of plan file load, no records loaded ***'(007).
ENDIF.
SKIP 1.

WRITE: / TEXT-DTE, SY-DATUM.                           "Date
WRITE: / TEXT-AMP, SY-UZEIT.                           "Time
WRITE: / TEXT-CLT, SY-MANDT, SY-SYSID.                 "Client
WRITE: / TEXT-101, P_VERSN.
WRITE: / TEXT-102, P_GJAHR.
WRITE: / TEXT-103, PERAB.
WRITE: / TEXT-104, PERBI.
SKIP 2.

WRITE:    / 'Records read from source file:', 26 FILE_IN.
WRITE:    / 'Number of Records:', RECCNT.
SKIP 1.

WRITE:    / 'I/O   ', 13 'Account'.
LOOP AT INREC.
  WRITE:    / INREC-AUFNR, INREC-KSTAR.
ENDLOOP.
SKIP 2.

WRITE:    / 'Not Existing Internal Orders :', ERRORD.

IF NOT ORDERR[] IS INITIAL.
   WRITE:    / 'I/O   ', 13 'Account'.
   LOOP AT ORDERR.
        WRITE:    / ORDERR-AUFNR, ORDERR-KSTAR.
   ENDLOOP.
ENDIF.

SKIP 2.
WRITE:    / 'Not Existing Cost Elements:', ERRACT.
IF NOT ACTERR[] IS INITIAL.
   WRITE:    / 'I/O   ', 13 'Account'.
   LOOP AT ACTERR.
        WRITE:    / ACTERR-AUFNR, ACTERR-KSTAR.
   ENDLOOP.
ENDIF.

*----------------------------------------------------------------------*
*  Check to see if order # on record in INREC is valid                 *
*----------------------------------------------------------------------*
FORM CHECK_SAP_ORDER_NUMBER.
DATA W_AUFNR.
  SELECT SINGLE AUFNR INTO W_AUFNR
    FROM AUFK
    WHERE AUFNR = INREC-AUFNR.

  IF SY-SUBRC NE 0.
    ADD 1 TO ERRORD.
    MOVE-CORRESPONDING INREC TO ORDERR.
    APPEND ORDERR.
    DELETE INREC.
    ADD -1 TO COMCNT.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*  It was indicated on the parameters for running the program that the *
*  plan file was created using the SAP Chart of Accounts.  Are all of  *
*  the accounts used valid SAP accounts?                               *
*----------------------------------------------------------------------*
FORM CHECK_SAP_COST_ELEMENT.
  SELECT SINGLE * FROM CSKA
    WHERE KTOPL = 'COAT'
      AND KSTAR = INREC-KSTAR.
  IF SY-SUBRC = 0.
    EXIT.
  ELSE.
    ADD 1 TO ERRACT.
    MOVE-CORRESPONDING INREC TO ACTERR.
    APPEND ACTERR.
    DELETE INREC.
    ADD -1 TO COMCNT.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM CHECK_FILE_PATH.
DATA: sep_file type string,
      sep_path type string,
      lv_bol TYPE C.        "abap_bool.

*Separate Path and file
     CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
       EXPORTING
         FULL_NAME           = FILE_IN
      IMPORTING
        STRIPPED_NAME       = sep_file
        FILE_PATH           = sep_path
      EXCEPTIONS
        X_ERROR             = 1
        OTHERS              = 2
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

IF sep_path CS 'C:' OR sep_path CS 'c:'.
   MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH TEXT-098.
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
   CONCATENATE TEXT-099 sep_path sep_file into SEP_PATH.
   MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH SEP_PATH.
ENDIF.
ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*                                                                      *
*                END OF PROGRAM                                        *
*                                                                      *
*----------------------------------------------------------------------*
