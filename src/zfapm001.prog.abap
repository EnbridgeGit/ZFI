REPORT ZFAPM001 MESSAGE-ID ZS.
*----------------------------------------------------------------------*
*  Author: Selwyn Rodricks                                             *
*          OmniLogic Systems Group                                     *
*  Brief Description:                                                  *
*  - For ERS, an invoice is generated when goods are received.  The    *
*    delivery note number on the Goods Receipt needs to be transferred *
*    to the invoice document and picked up in the payment tables       *
*    so that we can put the delivery note number on the cheque as a    *
*    reference                                                         *
*----------------------------------------------------------------------*
* 2006/05/31 - mdemeest 4.7 Added year in matching to elimiate duplicate
* 2006/05/25 - mdemeest 4.7 Change from XBLNR to BKTXT because new SAP
*                           program RMMR1MRS is populating XBLNR
* 00/06/20 mdemeest ---- Fix delivery note and add allocation info

TABLES: BKPF,                          "Accounting document header
        EKBE.                          "History of Purchasing Document

DATA:   START_DT   LIKE SY-DATUM.      "Date from which to select inv's

* batch input data structure
DATA: BEGIN OF BDCDATA  OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END   OF BDCDATA.

DATA: BEGIN OF ITAB  OCCURS 100,       "Internal table to hold data
        BELNR LIKE BKPF-BELNR,         "Document #
        gjahr like bkpf-gjahr,         "Year
        XBLNR LIKE BKPF-XBLNR,         "Ref.document #
        EBELN LIKE EKBE-EBELN,         "Purchasing document #
        EBELP LIKE EKBE-EBELP.         "Item #
DATA: END   OF ITAB.

SELECTION-SCREEN BEGIN OF BLOCK P_BLOCK WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP.

PARAMETERS: P_BUKRS     LIKE BKPF-BUKRS DEFAULT 'UGL',   "Company code
            P_BLART     LIKE BKPF-BLART DEFAULT 'ER',    "Document type
            P_BWART     LIKE EKBE-BWART DEFAULT '101',   "Movement type
            NUMDAYS(3)  TYPE N    DEFAULT 7,   "# of prev.days to search
            P_GROUP     LIKE APQI-GROUPID DEFAULT 'ERS_DEL_NOTE'.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK P_BLOCK.

*----------------------------------------------------------------------*
*     Start of Main Processing Block
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM FIND_ERS_DOCUMENTS.          "Select data & populate int.tabl
  PERFORM OPEN_SESSION.                "Open BDC session
  PERFORM CREATE_DATA.                 "Submit BDC data to session
  PERFORM CLOSE_SESSION.               "Close BDC session

* Write out a message if nothing to process
  READ TABLE ITAB INDEX 1.
  IF SY-SUBRC <> 0.
    WRITE: TEXT-101.                   "No documents found
  ENDIF.


*-----------------------------------------------------------------------
*   FORM FIND_ERS_DOCUMENTS
*-----------------------------------------------------------------------
*  - Select from BKPF rows with blank reference document number
*-----------------------------------------------------------------------
FORM FIND_ERS_DOCUMENTS.
* Select records that were posted by ERS program in the last x # of days
  START_DT = SY-DATUM - NUMDAYS.
  SELECT * FROM  BKPF WHERE
                 BUKRS = P_BUKRS   AND "Company code
                 BLART = P_BLART   AND "Document type
                 bktxt = space     and  "Document header text    "4.7
*                 XBLNR = SPACE     AND "Ref.Doc.#               "4.7
                 BUDAT >= START_DT.    "Posting date
*  use EKBE to get the purchase order# & item#
    SELECT * FROM EKBE WHERE
                 BELNR = BKPF-XBLNR and
                 gjahr = BKPF-GJAHR.                "BKPF-BELNR.  4.7
        CLEAR ITAB.
        MOVE: BKPF-BELNR TO ITAB-BELNR,
              BKPF-gjahr to itab-gjahr,
              EKBE-EBELN TO ITAB-EBELN,
              EKBE-EBELP TO ITAB-EBELP.
        APPEND ITAB.
    ENDSELECT.
  ENDSELECT.

* Now read EKBE to find the material doc# and update ITAB with ref.doc#.
* Since one BKPF document is created for a single or many material
* documents depending on the variant selected when running MRRS,
* the following code reads the first material document that matches
* the purchase order.  Item and Movement Type are used only to reduce
* the target records.

  LOOP AT ITAB.
    SELECT * FROM EKBE WHERE
            EBELN = ITAB-EBELN AND     "Purchasing document
            EBELP = ITAB-EBELP AND     "Item
            gjahr = itab-gjahr and     "Year
            BWART = P_BWART.           "Movement type
      IF NOT EKBE-XBLNR IS INITIAL.
        MOVE: EKBE-XBLNR TO ITAB-XBLNR.
        MODIFY ITAB INDEX SY-TABIX.
        IF SY-SUBRC <> 0.
          WRITE: / TEXT-102, EKBE-XBLNR.
        ENDIF.
*        EXIT.                                        "mdemeest 00/06/21
      ENDIF.
    ENDSELECT.
  ENDLOOP.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM CREATE_DATA
*-----------------------------------------------------------------------
FORM CREATE_DATA.

  LOOP AT ITAB.
    REFRESH BDCDATA.
    CLEAR BDCDATA.
*   1st screen for basic document data required to retrive it
    PERFORM BDC_SCREEN USING 'SAPMF05L'    '0100'.
    PERFORM BDC_FIELD  USING 'RF05L-BELNR' ITAB-BELNR.
    PERFORM BDC_FIELD  USING 'RF05L-BUKRS' P_BUKRS.

    PERFORM BDC_SCREEN USING 'SAPMF05L'    '700'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/5'.        "Choose


    PERFORM BDC_SCREEN USING 'SAPMF05L'    '1710'.
    perform bdc_field using  'BKPF-XBLNR'  itab-xblnr.  "Reference
*    perform BDC_FIELD  USING 'BKPF-BKTXT'   itab-xblnr.            "4.7

    PERFORM BDC_SCREEN USING 'SAPMF05L'    '700'.
    PERFORM BDC_FIELD  USING 'BDC_CURSOR'  '07/05'.     " Currsor
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'  '/2'.        "Choose


    perform bdc_screen using 'SAPMF05L'    '0302'.
    perform bdc_field  using 'BSEG-ZUONR'  itab-xblnr.  "Assignment
    perform bdc_field  using 'BSEG-SGTXT'
               'Evaluated receipt settlement'.
    perform bdc_field  using 'BDC_OKCODE'  '/11'.       "Save

    PERFORM INSERT_SESSION.
    WRITE: / ITAB-BELNR, ITAB-GJAHR, ITAB-XBLNR.
  ENDLOOP.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM OPEN_SESSION
*-----------------------------------------------------------------------
* - This routine opens up a new batch input session.
*-----------------------------------------------------------------------
FORM OPEN_SESSION.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = P_GROUP
*           HOLDDATE          =
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
    MESSAGE E004.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM CLOSE_SESSION
*-----------------------------------------------------------------------
* - This routine closes a batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_SESSION.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.
  IF SY-SUBRC <> 0.
    WRITE: / 'BDC Close Group Error. rc=', SY-SUBRC.
    EXIT.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM INSERT_SESSION
*-----------------------------------------------------------------------
* - This routine inserts the BDC data for one transaction into the
*   batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'FB02'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    WRITE: / 'Error inserting data into session.'.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM BDC_SCREEN
*-----------------------------------------------------------------------
*   Description:
*   - This routine adds an entry to the table BDCDATA with screen
*     information from a particular transaction.  This is used as part
*     of the process for creating data for batch input.
*
*   Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.


*-----------------------------------------------------------------------
*  FORM BDC_FIELD
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

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.
