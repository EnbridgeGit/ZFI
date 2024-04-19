REPORT  ZFFII045 MESSAGE-ID ZS.

************************************************************************
*  Client:    Spectra Energy.                                          *
*  Author:    Mohammad T. Khan.                                        *
*  Date:      August, 2009.                                            *
*  Track #:   TR582.                                                   *
*                                                                      *
*  Description:                                                        *
*     - The purpose of this program is to create a BDC session for     *
*       posting Ledger Group Journal Entries from the EXCEL sheet using*
*       Trans. FB01L.                                                  *
*  NOTE: The BDC session name must be in row 3 & col 3 of spread sheet.*
************************************************************************
* ---------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
*--------  ---- ------- ---------------------------------------------- *
*03/04/09  580  M Khan  Screen changes for upgrade to ECC 6.0          *
*                                                                      *
*10/07/12  995  M Khan  Change C: drive to H: drive with               *
*                           directory, file selection using F4         *
************************************************************************
TABLES: SKB1,                             "Cost Center Master Data
        TBSL.                             "Posting Key

* Batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

* Working Data
DATA:   TBLDAT TYPE D,
        TBUDAT TYPE D,
        W_BDC_SESSION(12) TYPE C,
        W_LDGRP   LIKE BKPF-LDGRP,      "Ledger Group
        W_BUKRS   LIKE BKPF-BUKRS.      "Co code holder for SKB1 extract

*Formated Input record
DATA:
  BEGIN OF XLTAB OCCURS 1,
    BUKRS    LIKE BKPF-BUKRS,           "Company Code
    BLART    LIKE BKPF-BLART,           "Document Type
    BLDAT    LIKE BKPF-BLDAT,           "Document date
    BUDAT    LIKE BKPF-BUDAT,           "Posting  date
    MONAT    LIKE BKPF-MONAT,           "PERIOD
    WAERS    LIKE BKPF-WAERS,           "Currency Code
    XBLNR    LIKE BKPF-XBLNR,           "Reference
    BKTXT    LIKE BKPF-BKTXT,           "Document Header Text
    BSCHL    LIKE BSEG-BSCHL,           "Posting Key
    SAKNR    LIKE BSEG-SAKNR,           "GL Account
    NEWBW    LIKE BSEG-ANBWA,           "Transaction Type
**    MWSKZ    LIKE BSEG-MWSKZ,           "Tax Code
    NEWBK    LIKE BBSEG-NEWBK,          "Company code for next line item
*    WRBTR(15) TYPE N,                   "Amount               "TR580
    WRBTR(15) TYPE C,                   "$                     "TR580
    MENGE(15) TYPE C,                   "Quantity
    MEINS    LIKE BSEG-MEINS,           "UOM
    SGTXT    LIKE BSEG-SGTXT,           "Line Item Description
    KOSTL    LIKE BSEG-KOSTL,           "Cost Center
    PROJN    LIKE BSEG-PROJN,           "Project
    AUFNR    LIKE BSEG-AUFNR,           "Order #
    MATNR    LIKE BSEG-MATNR,           "Material #
    PERNR    LIKE BSEG-PERNR,           "Personell #
    WERKS    LIKE BSEG-WERKS,           "Plant Code
    FIELD1   LIKE RKEAK-FIELD,          "Customer
    FIELD2   LIKE RKEAK-FIELD,          "Sales Org
    FIELD3   LIKE RKEAK-FIELD,          "Branch
    FIELD4   LIKE RKEAK-FIELD,          "Sector
    FIELD5   LIKE RKEAK-FIELD,          "Segment
    FIELD6   LIKE RKEAK-FIELD,          "Rate Class
    FIELD7   LIKE RKEAK-FIELD,          "Service Class
    FIELD15  LIKE RKEAK-FIELD,          "S.A. Number
  END OF XLTAB.

TYPES: BEGIN OF KCDE_INTERN_STRUC.
          INCLUDE STRUCTURE  KCDE_CELLS.
TYPES: END OF KCDE_INTERN_STRUC.

DATA: EXCELTAB TYPE KCDE_INTERN_STRUC OCCURS 0 with header line.

*======================================================================*
*              SELECTION SCREEN                                        *
*======================================================================*
*
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
 PARAMETERS: P_FILEIN LIKE RLGRAP-FILENAME OBLIGATORY DEFAULT
                      'H:\saptemp\generic upload ledger.xls'.
*                     'C:\saptemp\generic upload ledger.xls'.
SELECTION-SCREEN END OF BLOCK BOX.

*======================================================================*
*Start Of TR995 changes
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILEIN.
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
      P_FILEIN = WIT_FILENAME_TAB.
    ELSE.
      CLEAR P_FILEIN.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN ON P_FILEIN.
  PERFORM CHECK_FILE_PATH.
*End of TR995 changes
*======================================================================*
*              Main Processing Block                                   *
*======================================================================*
    PERFORM UPLOAD_EXCE_TO_INTERNAL_TAB.
    IF XLTAB[] IS INITIAL.
       STOP.
    ELSE.
       PERFORM DATA_CHECKS.
       PERFORM CREATE_BATCH_INPUT.
    ENDIF.

*======================================================================*
*              Upload EXCEL Data                                       *
*======================================================================*
FORM UPLOAD_EXCE_TO_INTERNAL_TAB.
CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
  EXPORTING
    FILENAME                      = P_FILEIN
    I_BEGIN_COL                   = 1
    I_BEGIN_ROW                   = 1
    I_END_COL                     = 32
    I_END_ROW                     = 999
  TABLES
    INTERN                        = EXCELTAB
 EXCEPTIONS
   INCONSISTENT_PARAMETERS        = 1
   UPLOAD_OLE                     = 2
   OTHERS                         = 3
          .
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

 LOOP AT EXCELTAB.
      IF EXCELTAB-ROW = 3 AND EXCELTAB-COL = 3.
         MOVE EXCELTAB-VALUE TO W_BDC_SESSION.
      ENDIF.
      IF EXCELTAB-ROW = 4 AND EXCELTAB-COL = 3.
         MOVE EXCELTAB-VALUE TO W_LDGRP.
      ENDIF.
      IF EXCELTAB-ROW < 6.
         CONTINUE.
      ENDIF.

      CASE EXCELTAB-COL.
          WHEN 1.  MOVE EXCELTAB-VALUE TO XLTAB-BUKRS.
          WHEN 2.  MOVE EXCELTAB-VALUE TO XLTAB-BLART.
          WHEN 3.  MOVE EXCELTAB-VALUE TO XLTAB-BLDAT.
          WHEN 4.  MOVE EXCELTAB-VALUE TO XLTAB-BUDAT.
          WHEN 5.  MOVE EXCELTAB-VALUE TO XLTAB-MONAT.
          WHEN 6.  MOVE EXCELTAB-VALUE TO XLTAB-WAERS.
          WHEN 7.  MOVE EXCELTAB-VALUE TO XLTAB-XBLNR.
          WHEN 8.  MOVE EXCELTAB-VALUE TO XLTAB-BKTXT.
          WHEN 9.  MOVE EXCELTAB-VALUE TO XLTAB-BSCHL.
          WHEN 10. CONCATENATE '0000' EXCELTAB-VALUE INTO XLTAB-SAKNR.
          WHEN 11. MOVE EXCELTAB-VALUE TO XLTAB-NEWBW.
**          WHEN 12. MOVE EXCELTAB-VALUE TO XLTAB-MWSKZ.
          WHEN 12. MOVE EXCELTAB-VALUE TO XLTAB-NEWBK.
          WHEN 13. WRITE EXCELTAB-VALUE TO XLTAB-WRBTR LEFT-JUSTIFIED.
          WHEN 14. MOVE EXCELTAB-VALUE TO XLTAB-MENGE.
          WHEN 15. MOVE EXCELTAB-VALUE TO XLTAB-MEINS.
          WHEN 16. MOVE EXCELTAB-VALUE TO XLTAB-SGTXT.
          WHEN 17. MOVE EXCELTAB-VALUE TO XLTAB-KOSTL.
          WHEN 18. MOVE EXCELTAB-VALUE TO XLTAB-PROJN.
          WHEN 19. MOVE EXCELTAB-VALUE TO XLTAB-AUFNR.
          WHEN 20. MOVE EXCELTAB-VALUE TO XLTAB-MATNR.
          WHEN 21. MOVE EXCELTAB-VALUE TO XLTAB-PERNR.
          WHEN 22. MOVE EXCELTAB-VALUE TO XLTAB-WERKS.
          WHEN 23. MOVE EXCELTAB-VALUE TO XLTAB-FIELD1.
          WHEN 24. MOVE EXCELTAB-VALUE TO XLTAB-FIELD2.
          WHEN 25. MOVE EXCELTAB-VALUE TO XLTAB-FIELD3.
          WHEN 26. MOVE EXCELTAB-VALUE TO XLTAB-FIELD4.
          WHEN 27. MOVE EXCELTAB-VALUE TO XLTAB-FIELD5.
          WHEN 28. MOVE EXCELTAB-VALUE TO XLTAB-FIELD6.
          WHEN 29. MOVE EXCELTAB-VALUE TO XLTAB-FIELD7.
          WHEN 30. MOVE EXCELTAB-VALUE TO XLTAB-FIELD15.
          WHEN OTHERS.
      ENDCASE.
      AT END OF ROW.
         APPEND XLTAB.
         CLEAR  XLTAB.
      ENDAT.
 ENDLOOP.
ENDFORM.
*======================================================================*
*DATA CHECK: Total of Debit & Credit amount in sheet should be equal   *
*======================================================================*
FORM DATA_CHECKS.

DATA: AMT_DEBIT LIKE BSEG-WRBTR,
      AMT_CREDIT LIKE BSEG-WRBTR.

LOOP AT XLTAB.
     CLEAR TBSL-SHKZG.
     SELECT SINGLE SHKZG INTO TBSL-SHKZG
       FROM TBSL
      WHERE BSCHL = XLTAB-BSCHL.

     IF TBSL-SHKZG = 'H'.
        ADD XLTAB-WRBTR TO AMT_CREDIT.
     ELSE.
        ADD XLTAB-WRBTR TO AMT_DEBIT.
     ENDIF.
ENDLOOP.
* Total Amount for column (WRBTR) of excel sheet must be zero.
     IF AMT_DEBIT <> AMT_CREDIT.
        WRITE: /1 TEXT-101.
        STOP.
     ENDIF.
ENDFORM.

*======================================================================*
*                FORM CREATE_BATCH_INPUT                               *
* - This is the main routine of the program which reads each record    *
*   from the input file and creates the batch input data.              *
*======================================================================*
FORM CREATE_BATCH_INPUT.

DATA:   TRANS_IN_PROCESS(1),            "flag - trans in process
        OPEN_SESSION(1) VALUE 'X'.      "flag - batch session open?

  REFRESH BDCDATA.
  CLEAR XLTAB.

  LOOP AT XLTAB.

*   Open batch session, if not open.
    IF OPEN_SESSION = 'X'.
       PERFORM OPEN_BATCH_SESSION.
       MOVE ' ' TO OPEN_SESSION.
    ENDIF.

*   Document header record....
        IF XLTAB-BUKRS <> SPACE.
           MOVE XLTAB-BUKRS TO W_BUKRS.
           IF TRANS_IN_PROCESS = 'X'.
              PERFORM CLOSE_TRANSACTION.
              CLEAR TRANS_IN_PROCESS.
           ENDIF.
           PERFORM START_NEW_TRANSACTION.
           TRANS_IN_PROCESS = 'X'.
        ENDIF.

*   Next line item
        PERFORM START_NEXT_LINEITEM.

*Get Field Status Group Using G/L Account Number.
    SELECT SINGLE FSTAG INTO SKB1-FSTAG
      FROM SKB1
     WHERE BUKRS = W_BUKRS
       AND SAKNR = XLTAB-SAKNR.

    CASE SKB1-FSTAG.
         WHEN 'BREV' OR 'GASS' OR 'REVN' OR 'RVOT'.
               PERFORM GL_LINE_ITEM_PROFITIBILITY.
         WHEN 'EXMM' OR 'PAYR' OR 'PROJ' OR 'STCF'.
               PERFORM GL_LINE_ITEM_NON_PROFITIBILITY.
         WHEN 'BSHT'.
               PERFORM GL_LINE_ITEM_BALANCE_SHEET.
         WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

  PERFORM CLOSE_TRANSACTION.
  PERFORM CLOSE_SESSION.
ENDFORM.

*======================================================================*
*                 FORM START_NEW_TRANSACTION                           *
* - This routine provides the BDC mapping for the initial screen in    *
* the transaction.                                                     *
*======================================================================*
FORM START_NEW_TRANSACTION.

* Header Data Screen (initial screen)
  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' XLTAB-BLDAT.
  PERFORM BDC_FIELD  USING 'BKPF-BLART' XLTAB-BLART.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' XLTAB-BUKRS.
  PERFORM BDC_FIELD  USING 'BKPF-MONAT' XLTAB-MONAT.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' XLTAB-BUDAT.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' XLTAB-WAERS.
  PERFORM BDC_FIELD  USING 'BKPF-LDGRP' W_LDGRP.
  PERFORM BDC_FIELD  USING 'BKPF-XBLNR' XLTAB-XBLNR.
  PERFORM BDC_FIELD  USING 'BKPF-BKTXT' XLTAB-BKTXT.

ENDFORM.

*======================================================================*
*                 FORM CLOSE_TRANSACTION                               *
* - This routine closes the current transaction.                       *
*======================================================================*
FORM CLOSE_TRANSACTION.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'BU'.          "Post Document
  PERFORM INSERT_SESSION.

ENDFORM.

*======================================================================*
*                   FORM START_NEXT_LINEITEM                           *
* - This routine enters the posting key and account for the next line  *
* item.  This was put in a seperate routine for clarity as only these  *
* 2-3 fields appear on the previous screen.  The rest of the line item *
* information appears on a subsequent screen in the transaction.       *
*======================================================================*
FORM START_NEXT_LINEITEM.

   SELECT SINGLE FSTAG INTO SKB1-FSTAG
     FROM SKB1
    WHERE BUKRS = W_BUKRS
      AND SAKNR = XLTAB-SAKNR.
   IF SKB1-FSTAG = 'STCF'.
   PERFORM BDC_FIELD  USING 'RF05A-NEWBW' XLTAB-NEWBW. "Transaction Type
   ENDIF.
   PERFORM BDC_FIELD  USING 'RF05A-NEWBS' XLTAB-BSCHL.
   PERFORM BDC_FIELD  USING 'RF05A-NEWKO' XLTAB-SAKNR.
   PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.

ENDFORM.

*======================================================================*
*This is the BDC mapping for screen when entering a G/L line item for  *
*Balance Sheet Account.                                                *
*======================================================================*
FORM GL_LINE_ITEM_BALANCE_SHEET.

  PERFORM BDC_SCREEN USING 'SAPMF05A' '0300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  XLTAB-WRBTR.
  PERFORM BDC_FIELD  USING 'BSEG-MENGE'  XLTAB-MENGE.
  PERFORM BDC_FIELD  USING 'BSEG-MEINS'  XLTAB-MEINS.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  XLTAB-SGTXT.
  PERFORM BDC_FIELD  USING 'DKACB-FMORE' ' '.
  PERFORM BDC_FIELD  USING 'RF05A-NEWBK' XLTAB-NEWBK.

ENDFORM.
*======================================================================*
*This is the BDC mapping for screen when entering a G/L line item with *
*Profitibility Segment                                                 *
*======================================================================*
FORM GL_LINE_ITEM_PROFITIBILITY.

  PERFORM BDC_SCREEN USING 'SAPMF05A' '0300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  XLTAB-WRBTR.
**  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'  XLTAB-MWSKZ.
  PERFORM BDC_FIELD  USING 'BSEG-MENGE'  XLTAB-MENGE.
  PERFORM BDC_FIELD  USING 'BSEG-MEINS'  XLTAB-MEINS.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  XLTAB-SGTXT.
  PERFORM BDC_FIELD  USING 'RF05A-NEWBK' XLTAB-NEWBK.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ZK'.          "More data
  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'COBL-MATNR'  XLTAB-MATNR.
  PERFORM BDC_FIELD  USING 'COBL-WERKS'  XLTAB-WERKS.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.        "or use ENTE

*Profitibility Segment
   PERFORM BDC_SCREEN USING  'SAPLKEAK'      '0300'.
   PERFORM BDC_FIELD  USING  'RKEAK-FIELD(01)' XLTAB-FIELD1.
   PERFORM BDC_FIELD  USING  'RKEAK-FIELD(08)' XLTAB-FIELD2.
   PERFORM BDC_FIELD  USING  'BDC_OKCODE'    '=P+'.  "TR580
   PERFORM BDC_SCREEN USING  'SAPLKEAK'      '0300'. "TR580
   PERFORM BDC_FIELD  USING  'RKEAK-FIELD(12)' XLTAB-FIELD3.
   PERFORM BDC_FIELD  USING  'RKEAK-FIELD(02)' XLTAB-FIELD4.
   PERFORM BDC_FIELD  USING  'RKEAK-FIELD(03)' XLTAB-FIELD5.
   PERFORM BDC_FIELD  USING  'RKEAK-FIELD(08)' XLTAB-FIELD6.
   PERFORM BDC_FIELD  USING  'RKEAK-FIELD(09)' XLTAB-FIELD7.
   PERFORM BDC_FIELD  USING  'BDC_OKCODE'    '=P+'.  "TR580
   PERFORM BDC_SCREEN USING  'SAPLKEAK'      '0300'. "TR580
   PERFORM BDC_FIELD  USING  'RKEAK-FIELD(03)' XLTAB-FIELD15.
   PERFORM BDC_FIELD  USING  'BDC_OKCODE'    '/00'.
   PERFORM BDC_SCREEN USING 'SAPMF05A' '0330'.
ENDFORM.

*======================================================================*
*This is the BDC mapping for screen when entering a G/L line item with *
*Non Profitibility Segment                                             *
*======================================================================*
FORM GL_LINE_ITEM_NON_PROFITIBILITY.

  PERFORM BDC_SCREEN USING 'SAPMF05A'      '0300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'    XLTAB-WRBTR.
**IF SKB1-FSTAG = 'EXMM'.
**   PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'    XLTAB-MWSKZ.
**ENDIF.
  PERFORM BDC_FIELD  USING 'BSEG-MENGE'    XLTAB-MENGE.
  PERFORM BDC_FIELD  USING 'BSEG-MEINS'    XLTAB-MEINS.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'    XLTAB-SGTXT.
  PERFORM BDC_FIELD  USING 'RF05A-NEWBK'   XLTAB-NEWBK.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ZK'.          "More data
  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'COBL-KOSTL'    XLTAB-KOSTL.
  PERFORM BDC_FIELD  USING 'COBL-PS_POSID' XLTAB-PROJN.
  PERFORM BDC_FIELD  USING 'COBL-AUFNR'    XLTAB-AUFNR.
  PERFORM BDC_FIELD  USING 'COBL-PERNR'    XLTAB-PERNR.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.        "or use ENTE
  PERFORM BDC_SCREEN USING 'SAPMF05A' '0330'.

ENDFORM.

*======================================================================*
*     FORM OPEN_BATCH_SESSION                                          *
* - This routine just simply opens up a new batch input session.       *
*======================================================================*
FORM OPEN_BATCH_SESSION.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = W_BDC_SESSION
*            HOLDDATE          = SY-DATUM
            KEEP              = 'X'
            USER              = SY-UNAME
       EXCEPTIONS
            GROUP_INVALID     = 1
            GROUP_IS_LOCKED   = 2
            HOLDDATE_INVALID  = 3
            INTERNAL_ERROR    = 4
            QUEUE_ERROR       = 5
            RUNNING           = 6
            SYSTEM_LOCK_ERROR = 7
            USER_INVALID      = 8.

  IF SY-SUBRC <> 0.
    MESSAGE E004 WITH W_BDC_SESSION.
  ENDIF.

ENDFORM.

*======================================================================*
*     FORM INSERT_SESSION                                              *
* - This routine inserts the BDC data for one transaction into the     *
*   batch input session.                                               *
*======================================================================*
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'FB01L'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    MESSAGE E013 WITH SY-SUBRC.
  ENDIF.

  REFRESH BDCDATA.                 "Refresh BDCData

ENDFORM.

*======================================================================*
*     FORM CLOSE_SESSION                                               *
* - This routine simply closes the current batch input session.        *
*======================================================================*
FORM CLOSE_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN
            QUEUE_ERROR.
  IF SY-SUBRC = 0.
    MESSAGE I003 WITH W_BDC_SESSION.
  ENDIF.
ENDFORM.

*======================================================================*
*                FORM BDC_SCREEN                                       *
*----------------------------------------------------------------------*
*  Description:                                                        *
*  - This routine adds an entry to the table BDCDATA with screen       *
*    information from a particular transaction.  This is used as part  *
*    of the process for creating data for batch input.                 *
*                                                                      *
*  Parameters:                                                         *
*      -->  PROGRAM - Program name of the screen                       *
*           DNYPRO  - Screen number                                    *
*======================================================================*
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.
*======================================================================*
*               FORM BDC_FIELD                                         *
*----------------------------------------------------------------------*
*  Description:                                                        *
*  - This routine adds an entry to the table BDCDATA with field        *
*    information from a particular transaction.  This is used as part  *
*    of the process for creating data for batch input.                 *
*                                                                      *
*  Parameters:                                                         *
*      -->  FNAM - name of the field on the screen                     *
*           FVAL - value to be entered for that field on the           *
*                  screen                                              *
*======================================================================*
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

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
         FULL_NAME           = P_FILEIN
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
