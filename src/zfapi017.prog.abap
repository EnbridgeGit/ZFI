REPORT ZFAPI017 MESSAGE-ID ZS.
************************************************************************
*  Author:    Mohammad Khan.
*
*  Date:      January, 2000.
*
*  Description:
*     - The purpose of this program is to create a BDC session to post
*       the invoices coming from Propperty Tax System using transaction
*       "FB01".
*
************************************************************************
* ---------------------- CHANGE LOG ------------------------------------
* 2012/10/01 - gymana - Ehp5
*              Replace logical file coding with conventional open
*              dataset logic.
*
* 2009/10/13 mdemeest TR___ - Property Tax G/L comes from file, not
*                             variant
*
************************************************************************
DATA: BEGIN OF itab OCCURS 500,                                   "EHP5
              irec(264),                                          "EHP5
      END OF itab.                                                "EHP5
DATA: INREC(264).                                "Input record

* Batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

* Input file name with path
DATA: INFILE(70).

* Dates Data
DATA:   TBLDAT TYPE D,
        TBUDAT TYPE D,
        ZBLDAT(10),                        "date in user format
        ZBUDAT(10).


* Name of batch input session.

DATA:
  BEGIN OF INREC1,
    STYPE   LIKE BBKPF-STYPE,
    BUDAT   LIKE BBKPF-BUDAT,
    BLDAT   LIKE BBKPF-BLDAT,
    BUKRS   LIKE BBKPF-BUKRS,
    WAERS   LIKE BBKPF-WAERS,
    XBLNR   LIKE BBKPF-XBLNR,
    BKTXT   LIKE BBKPF-BKTXT,
  END OF INREC1.

  DATA:
  BEGIN OF INREC2,
    STYPE   LIKE BBKPF-STYPE,
    NEWBS   LIKE BBSEG-NEWBS,
    NAME1   LIKE BBSEG-NAME1,
    NAME2   LIKE BBSEG-NAME2,
    STRAS   LIKE BBSEG-STRAS,
    ORT01   LIKE BBSEG-ORT01,
    PSTLZ   LIKE BBSEG-PSTLZ,
    REGIO   LIKE BBSEG-REGIO,
    WRBTR   LIKE BBSEG-WRBTR,
    NEWKO   LIKE BBSEG-NEWKO,
    SGTXT   LIKE BBSEG-SGTXT,
  END OF INREC2.

*=======================================================================
* SELECTION SCREEN
*=======================================================================
* Logical file name, Session name, Document Type, Default tax code,
* Payment term, Country.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-000.   "EHP5
PARAMETERS: P_TCODE  LIKE TSTC-TCODE
                     DEFAULT 'FB01' OBLIGATORY,
            P_GRUPID LIKE APQI-GROUPID
                     DEFAULT 'ZAP_PROP_TAX' OBLIGATORY,
            P_GLACCT LIKE RF05A-NEWKO
                     DEFAULT '256530' OBLIGATORY,
            P_BLART  LIKE BKPF-BLART
                     DEFAULT 'K3' OBLIGATORY,
            P_ZTERM  LIKE BSEG-ZTERM
                     DEFAULT 'N00' OBLIGATORY,
            P_LAND1  LIKE BSEG-LANDL
                     DEFAULT 'CA' OBLIGATORY.

SELECTION-SCREEN SKIP.                                            "EHP5
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-003.   "EHP5
PARAMETERS: p_local RADIOBUTTON GROUP gr1,                        "EHP5
            p_unix  RADIOBUTTON GROUP gr1 DEFAULT 'X'.            "EHP5
                                                                  "EHP5
SELECTION-SCREEN SKIP.                                            "EHP5
PARAMETERS: p_locfil LIKE filename-fileextern,                    "EHP5
            p_infile LIKE filename-fileextern.                    "EHP5
SELECTION-SCREEN END OF BLOCK box2.                               "EHP5
SELECTION-SCREEN END OF BLOCK box1.                               "EHP5

*======================================================================*
INITIALIZATION.
  MOVE '/usr/sap/interfaces/P01/FAPPTAX/INVOICES.chk'             "EHP5
       TO p_infile.                                               "EHP5
  MOVE 'H:\' TO p_locfil.                                         "EHP5

*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================
*AT SELECTION-SCREEN.                                             "EHP5
*     FORM CHECK_FILE                                             "EHP5

*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.
  PERFORM validate_p_infile.                                      "EHP5
  PERFORM CREATE_BATCH_INPUT.

*---------------------------------------------------------------------*
*       validate_p_infile
*---------------------------------------------------------------------*
FORM validate_p_infile.                                           "EHP5
                                                                  "EHP5
  DATA: l_file TYPE string.                                       "EHP5
  DATA: lt_itab TYPE STANDARD TABLE OF string.                    "EHP5
                                                                  "EHP5
* For Unix Files                                                  "EHP5
  IF p_unix = 'X'.                                                "EHP5
    OPEN DATASET p_infile FOR INPUT IN TEXT MODE.                 "EHP5
    IF sy-subrc NE 0.                                             "EHP5
      MESSAGE e002 WITH p_infile.                                 "EHP5
                                                                  "EHP5
      STOP.                                                       "EHP5
    ELSE.                                                         "EHP5
      DO.                                                         "EHP5
        READ DATASET p_infile INTO itab-irec.                     "EHP5
        IF sy-subrc NE 0.                                         "EHP5
          EXIT.                                                   "EHP5
        ELSE.                                                     "EHP5
          APPEND itab.                                            "EHP5
        ENDIF.                                                    "EHP5
      ENDDO.                                                      "EHP5
    ENDIF.                                                        "EHP5
  ELSE.                                                           "EHP5
* For Local Files.                                                "EHP5
    l_file = p_locfil.                                            "EHP5
                                                                  "EHP5
    CALL METHOD cl_gui_frontend_services=>gui_upload              "EHP5
      EXPORTING                                                   "EHP5
        filename                = l_file                          "EHP5
        filetype                = 'ASC'                           "EHP5
      CHANGING                                                    "EHP5
        data_tab                = lt_itab                         "EHP5
      EXCEPTIONS                                                  "EHP5
        file_open_error         = 1                               "EHP5
        file_read_error         = 2                               "EHP5
        OTHERS                  = 18.                             "EHP5
                                                                  "EHP5
    IF sy-subrc NE 0.                                             "EHP5
      MESSAGE E002 WITH p_locfil.                                 "EHP5
    else.                                                         "EHP5
      itab[] = lt_itab[].                                         "EHP5
    ENDIF.                                                        "EHP5
                                                                  "EHP5
  ENDIF.                                                          "EHP5
                                                                  "EHP5
ENDFORM.                    "validate_p_infile                    "EHP5

*-----------------------------------------------------------------------
*     FORM CHECK_FILE
*-----------------------------------------------------------------------
*  - Routine to check logical file and convert it to physical file.
*  It then attempts to open the physical file to determine if there
*  are any errors reading it.
*-----------------------------------------------------------------------
* EHP5 - GYMANA - 2012/10/02
*-----------------------------------------------------------------------
*FORM CHECK_FILE.
*
*  DATA: MSG(100).
*
*Get Physical File Name
*
*  CALL FUNCTION 'FILE_GET_NAME'
*       EXPORTING
*            CLIENT           = SY-MANDT
*            LOGICAL_FILENAME = LGCLFILE
*            OPERATING_SYSTEM = SY-OPSYS
*       IMPORTING
*            FILE_NAME        = INFILE
*       EXCEPTIONS
*            FILE_NOT_FOUND   = 01.
*  IF ( SY-SUBRC = 1 ).
*    MESSAGE E001 WITH LGCLFILE.
*  ENDIF.

*Open File

*  OPEN DATASET INFILE FOR INPUT IN TEXT MODE MESSAGE MSG.
*  IF ( SY-SUBRC <> 0 ).
*    MESSAGE E002 WITH INFILE MSG.
*  ENDIF.
*
*ENDFORM.

*-----------------------------------------------------------------------
*     FORM CREATE_BATCH_INPUT
*-----------------------------------------------------------------------
* - This is the main routine of the program which reads each record
*   from the input file and creates the batch input data.
*-----------------------------------------------------------------------
FORM CREATE_BATCH_INPUT.

  DATA: MSG(100),                           "open file - system message
        FIRST_DOC(1),                       "flag - first document?
        OPEN_SESSION(1) VALUE 'X',          "flag - batch session open?
        W_NEWKO   LIKE RF05A-NEWKO,         "G/L Account holder field
        OLD_NEWBS LIKE ZBSEG-NEWBS.         "Previous Posting key

  REFRESH BDCDATA.

  FIRST_DOC = 'X'.

* Process file data.
  LOOP AT itab.
*    MOVE itab-irec TO inrec.

*   If file is empty, stop run.
*    IF ( SY-INDEX = 1 ).
*      IF ( SY-SUBRC = 4 ).
*        MESSAGE I028.
*        STOP.
*      ENDIF.
*    ENDIF.
*
*   When end of file, Exit.
*    IF SY-SUBRC <> 0.
*       EXIT.
*    ENDIF.
*
*   Open batch session, if not open.
    IF OPEN_SESSION = 'X'.
       PERFORM OPEN_BATCH_SESSION.
       MOVE ' ' TO OPEN_SESSION.
    ENDIF.

    CASE itab-irec(1).
*     document header record....
      WHEN '1'.
        CLEAR OLD_NEWBS.
*       process rest of previous document (if not first one)
        IF ( FIRST_DOC = SPACE ).
          PERFORM FINISH_TRANSACTION.
        ENDIF.
        REFRESH BDCDATA.
        INREC1 = itab-irec.
        PERFORM START_NEW_TRANSACTION.
        CLEAR: FIRST_DOC.
*     Document line item record....
      WHEN '2'.
        INREC2 = itab-irec.

*        IF ( INREC2-NEWBS = '40' ) OR ( INREC2-NEWBS = '50' ).
*            W_NEWKO = P_GLACCT.

        IF ( ( ( INREC2-NEWBS = '40' ) OR ( INREC2-NEWBS = '50' ) )
           and inrec2-newko = '/////////////////' ).
            W_NEWKO = P_GLACCT.

        ELSE.
            W_NEWKO = INREC2-NEWKO.
        ENDIF.

        PERFORM START_NEXT_ITEM USING
               INREC2-NEWBS W_NEWKO OLD_NEWBS.

*     Posting key '21' or '31' indicates vendor line item
        IF ( INREC2-NEWBS = '21' ) OR ( INREC2-NEWBS = '31' ).
          PERFORM ADD_VENDOR_DESC.
          PERFORM ADD_VENDOR_LINE_ITEM.
        ELSE.
          PERFORM ADD_GL_LINE_ITEM.
        ENDIF.
        OLD_NEWBS = INREC2-NEWBS.
    ENDCASE.
  ENDLOOP.

  PERFORM FINISH_TRANSACTION.

*  CLOSE DATASET INFILE.
  PERFORM CLOSE_SESSION.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
* - This routine just simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = P_GRUPID
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
    MESSAGE E004 WITH P_GRUPID.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM INSERT_SESSION
*-----------------------------------------------------------------------
* - This routine inserts the BDC data for one transaction into the
*   batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = P_TCODE
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

ENDFORM.


*-----------------------------------------------------------------------
*     FORM CLOSE_SESSION
*-----------------------------------------------------------------------
* - This routine simply closes the current batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN
            QUEUE_ERROR.
  IF SY-SUBRC = 0.
    MESSAGE I003 WITH P_GRUPID.
  ENDIF.
ENDFORM.


*-----------------------------------------------------------------------
*     FORM START_NEW_TRANSACTION
*-----------------------------------------------------------------------
* - This routine provides the BDC mapping for the initial screen in
* the transaction.
*-----------------------------------------------------------------------
FORM START_NEW_TRANSACTION.


* convert dates to user-default format
  TBLDAT = INREC1-BLDAT.
  TBUDAT = INREC1-BUDAT.
  WRITE TBLDAT TO ZBLDAT DD/MM/YYYY.
  WRITE TBUDAT TO ZBUDAT DD/MM/YYYY.
* Header data screen (initial screen)
  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
***  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' ZBUDAT.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' ZBLDAT.
  PERFORM BDC_FIELD  USING 'BKPF-BLART' P_BLART.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' INREC1-BUKRS.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' ZBLDAT.
***  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' ZBUDAT.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' INREC1-WAERS.
  PERFORM BDC_FIELD  USING 'BKPF-XBLNR' INREC1-XBLNR.
  PERFORM BDC_FIELD  USING 'BKPF-BKTXT' INREC1-BKTXT.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM FINISH_TRANSACTION
*-----------------------------------------------------------------------
* - This routine finishes the BDC mapping for the transaction.
*-----------------------------------------------------------------------
FORM FINISH_TRANSACTION.

    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
* Pop-up window "Coding block" appears - select F8 to continue
    PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.

  PERFORM INSERT_SESSION.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM START_NEXT_ITEM
*-----------------------------------------------------------------------
* - This routine enters the posting key and account for the next line
* item.  This was put in a seperate routine for clarity as only these
* 2 fields appear on the previous screen.  The rest of the line item
* information appears on a subsequent screen in the transaction.
*
*  Parameters
*     --> F_NEWBS - Posting key
*         F_NEWKO - account number
*         O_NEWBS - posting key of previous line item
*-----------------------------------------------------------------------
FORM START_NEXT_ITEM USING F_NEWBS F_NEWKO O_NEWBS.

  PERFORM BDC_FIELD  USING 'RF05A-NEWBS' F_NEWBS.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' F_NEWKO.
* coding block appears after a G/L line item is entered
  IF ( O_NEWBS <> '21' ) AND ( O_NEWBS <> '31' ) AND
     ( O_NEWBS <> SPACE ).
      PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM ADD_VENDOR_DESC
*-----------------------------------------------------------------------
* - When one-time vendors are used an additional screen appears in
* which specific information about the vendor is entered (such as
* name and address).
*-----------------------------------------------------------------------
FORM ADD_VENDOR_DESC.

* Screen: Enter vendor invoice
  MOVE INREC1-BLDAT+4(4) TO INREC2-NAME2+27(4).
  PERFORM BDC_SCREEN USING 'SAPLFCPD' '100'.
  PERFORM BDC_FIELD  USING 'BSEC-NAME1' INREC2-NAME1.
  PERFORM BDC_FIELD  USING 'BSEC-NAME2' INREC2-NAME2.
  PERFORM BDC_FIELD  USING 'BSEC-STRAS' INREC2-STRAS.
  PERFORM BDC_FIELD  USING 'BSEC-ORT01' INREC2-ORT01.
  PERFORM BDC_FIELD  USING 'BSEC-LAND1' P_LAND1.
  PERFORM BDC_FIELD  USING 'BSEC-PSTLZ' INREC2-PSTLZ.
  PERFORM BDC_FIELD  USING 'BSEC-REGIO' INREC2-REGIO.


ENDFORM.


*-----------------------------------------------------------------------
*     FORM ADD_VENDOR_LINE_ITEM
*-----------------------------------------------------------------------
* - This is the BDC mapping for the screen when entering a vendor line
* item.
*-----------------------------------------------------------------------
FORM ADD_VENDOR_LINE_ITEM.

  DATA: TZFBDT TYPE D,
        ZZFBDT(10).

* Create vendor item screen
  PERFORM BDC_SCREEN USING 'SAPMF05A' '0302'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' INREC2-WRBTR.
  PERFORM BDC_FIELD  USING 'BSEG-ZTERM' P_ZTERM.
  PERFORM BDC_FIELD  USING 'BSEG-ZFBDT' ZBLDAT.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT' INREC2-SGTXT.
ENDFORM.


*-----------------------------------------------------------------------
*     FORM ADD_GL_LINE_ITEM
*-----------------------------------------------------------------------
* - This is the BDC mapping for the screen when entering a G/L line
* item.
*-----------------------------------------------------------------------
FORM ADD_GL_LINE_ITEM.

* Screen: Create G/L account item
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' INREC2-WRBTR.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT' INREC2-SGTXT.
* coding block screen
**  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.       ?
**  PERFORM BDC_FIELD  USING 'COBL-KOSTL' ZBSEG-KOSTL.
**  PERFORM BDC_FIELD  USING 'COBL-AUFNR' ZBSEG-AUFNR.
**  PERFORM BDC_FIELD  USING 'COBL-PS_PSP_PNR' ZBSEG-PROJK.
**  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
**  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.

ENDFORM.



*-----------------------------------------------------------------------
*     FORM BDC_SCREEN
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with screen
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM BDC_FIELD
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  FNAM - name of the field on the screen
*           FVAL - value to be entered for that field on the
*                  screen
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.

  DATA: TVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.
