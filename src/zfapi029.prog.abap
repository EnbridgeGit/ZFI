REPORT ZFAPI029 MESSAGE-ID ZS.
************************************************************************
*  Programmer: Lee Haire
*              OmniLogic Systems Group
*  Brief Description:
*     - This ABAP is an interface to load vendor invoices.  This can
*  normally be done with RFBIBL00 but additional long text needs to
*  be added to the invoice.  Hence, this ABAP was written from scratch
*  to accomodate this requirement.  Note that the same structures
*  required by RFBIBL00 are used here with an additional record type
*  for long text.
************************************************************************
* ---------------------- CHANGE LOG ------------------------------------
* 2003/07/17 mdemeest 1017 Create abap
* 2014/08/08 M.Khan  Tkt22498 Add Payee and Payment Method.
*
************************************************************************
*                DIFFERENCES between EAST & WEST
*-----------------------------------------------------------------------
*          EAST                   |               WEST
*-----------------------------------------------------------------------
* ZBSEG                           | BBSEG
* n/a                             | nplnr & vornr
*=======================================================================
*ABLES: LFA1,                                     "vendor master
*       T077K.                                    "vendor account groups

DATA: INREC(1700).                                "input record
data: inrec46b(1700).                             "4.6B
* input record structure - session header
DATA: BEGIN OF ZBGR00.
        INCLUDE STRUCTURE BGR00.
DATA: END OF ZBGR00.
* input record structure - document header
DATA: BEGIN OF ZBBKPF.
        INCLUDE STRUCTURE BBKPF.
DATA: END OF ZBBKPF.
* input record structure - document line item
DATA: BEGIN OF ZBSEG.
        INCLUDE STRUCTURE ZBSEG.                            "DIFFERENCES
DATA: EMPFB TYPE EMPFB.                           "Ticket 22498
DATA: END OF ZBSEG.
* input record stucture - additional text
DATA: BEGIN OF TEXTTAB OCCURS 1,
        STR(203),
      END OF TEXTTAB.
* list of one time vendor account groups
DATA: BEGIN OF OTV OCCURS 1,
        KTOKK LIKE LFA1-KTOKK,
      END OF OTV.
* batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.
* real name of input file
DATA: INFILE(70).
* transaction code for batch input
DATA: G_TCODE LIKE TSTC-TCODE.
* name of batch input session.
DATA: BDCNAME LIKE ZBGR00-GROUP.
* current count of FI line items in the invoice
DATA: FICOUNT(1) TYPE N.
* check stub text indicator (to allow a slash '/' in BDC fields)
DATA: TEXT_IND(1) TYPE C VALUE 'N'.                      " Mradsma-98/05
*=======================================================================
* SELECTION SCREEN
*=======================================================================
* logical file name
PARAMETERS: LGCLFILE LIKE FILENAME-FILEINTERN
                     DEFAULT 'ZIFAP00?' OBLIGATORY.
* format text in SAPScript editor?
PARAMETERS: EDFORMAT AS CHECKBOX DEFAULT 'X'.

*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================
AT SELECTION-SCREEN.
  PERFORM CHECK_FILE.

*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.
  PERFORM ONE_TIME_VENDOR_LIST.
  PERFORM CREATE_BATCH_INPUT.


*-----------------------------------------------------------------------
*     FORM CHECK_FILE
*-----------------------------------------------------------------------
*  - Routine to check logical file and convert it to physical file.
*  It then attempts to open the physical file to determine if there
*  are any errors reading it.
*-----------------------------------------------------------------------
FORM CHECK_FILE.

  DATA: MSG(100).                           "open file - system message

  CALL FUNCTION 'FILE_GET_NAME'
       EXPORTING
            CLIENT           = SY-MANDT
            LOGICAL_FILENAME = LGCLFILE
            OPERATING_SYSTEM = SY-OPSYS
       IMPORTING
            FILE_NAME        = INFILE
       EXCEPTIONS
            FILE_NOT_FOUND   = 01.
  IF ( SY-SUBRC = 1 ).
    MESSAGE E001 WITH LGCLFILE.
  ENDIF.
  OPEN DATASET INFILE FOR INPUT IN TEXT MODE MESSAGE MSG.
  IF ( SY-SUBRC <> 0 ).
    MESSAGE E002 WITH INFILE MSG.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*  FORM ONE_TIME_VENDOR_LIST
*-----------------------------------------------------------------------
* - A little routine to create an internal table with all one-time
* vendor account groups.
*-----------------------------------------------------------------------
FORM ONE_TIME_VENDOR_LIST.

  SELECT KTOKK INTO OTV-KTOKK FROM T077K WHERE XCPDS = 'X'.
    APPEND OTV.
  ENDSELECT.
  SORT OTV.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM CREATE_BATCH_INPUT
*-----------------------------------------------------------------------
* - This is the main routine of the program which reads each record
*   from the input file and creates the batch input data.
* - Note that each text record read (type 3) is not processed
*   immediately.  Because all text must be added at once in the
*   SAPScript editor, all text records will be gathered together in
*   an internal table first.  Then before the next document is to be
*   processed this text is added all at once.  This also guarantees
*   that all text records that belong to one document will be posted
*   together even if they happen to be seperated in the input file
*   (for example, by a line item record).
*-----------------------------------------------------------------------
FORM CREATE_BATCH_INPUT.

  DATA: MSG(100),                           "open file - system message
        FIRST_DOC(1),                       "flag - first document?
        OTFLAG(1),                          "flag - one-time vendor?
        OLD_NEWBS LIKE ZBSEG-NEWBS,         "previous posting key
        old_newbk like zbseg-newbk.

  REFRESH BDCDATA.

  FIRST_DOC = 'X'.
* do until we have read in the complete file.
  DO.
    CLEAR: OTFLAG.
   READ DATASET INFILE INTO INREC.
*    READ DATASET INFILE INTO INREC46b.  "4.6B
*   check for empty file
    IF ( SY-INDEX = 1 ).
      IF ( SY-SUBRC = 4 ).
        MESSAGE I028.
        STOP.
      ENDIF.
    ENDIF.
*   exit loop when end of file
    IF ( SY-SUBRC <> 0 ).
      EXIT.
    ENDIF.
*----------------------------- 4.6B Changes ---------------------------
*case inrec.
*  when '0'.
*    clear inrec.
*    inrec = inrec46b.
*  when '1'.
*    clear inrec.
*     inrec(5)      = inrec46b(5).
*     inrec+21(189) = inrec46b+5(189).
*     inrec+210(1)  = '/'.                    "Branch number
*    inrec+214(1)  = '/'.                    "Number of pages of invoice
*    inrec+217(1)  = '/'.                    "Reason for reversal
*    inrec+218(1)  = '/'.                    "Indirect Exchange Rate
*  when '2'.
*    clear inrec.
*    inrec(11)     = inrec46b(11).
*    inrec+31(622) = inrec46b+11(622).
*  when '3'.
*    clear inrec.
*    inrec = inrec46b.
*endcase.
*-------------------------- End of 4.6B Changes -----------------------
    CASE INREC(1).
*     session header record....
      WHEN '0'.
        ZBGR00 = INREC.
        ZBGR00-START = SY-DATUM - 1.                          " 4.6B
*       if this is not the first document, finish the last document
*       and close out the current session.
        IF ( FIRST_DOC = SPACE ).
          PERFORM FINISH_TRANSACTION.
          PERFORM CLOSE_SESSION.
        ENDIF.
        PERFORM OPEN_BATCH_SESSION.
        FIRST_DOC = 'X'.
*     document header record....
      WHEN '1'.
        CLEAR OLD_NEWBS.
        clear old_newbk.
*       process rest of previous document (if not first one)
        IF ( FIRST_DOC = SPACE ).
          PERFORM FINISH_TRANSACTION.
        ENDIF.
        REFRESH TEXTTAB.
        REFRESH BDCDATA.
        ZBBKPF = INREC.
        G_TCODE = ZBBKPF-TCODE.
        PERFORM START_NEW_TRANSACTION.
        CLEAR: FIRST_DOC, FICOUNT.
*     document line item record....
      WHEN '2'.
        ZBSEG = INREC.
        If ( zbseg-newbs = '21' ) or ( zbseg-newbs = '31' ).
            clear old_newbk.
        else.
            old_newbk = zbseg-newbk.
        endif.
        PERFORM START_NEXT_ITEM
            USING ZBSEG-NEWBS ZBSEG-HKONT OLD_NEWBS old_newbk.
*       posting key '21' or '31' indicates vendor line item
        IF ( ZBSEG-NEWBS = '21' ) OR ( ZBSEG-NEWBS = '31' ).
          PERFORM CHECK_ONE_TIME_VENDOR USING OTFLAG.
          IF ( OTFLAG = 'X' ).
            PERFORM ADD_VENDOR_DESC.
          ENDIF.
          PERFORM ADD_VENDOR_LINE_ITEM.
        ELSE.
          FICOUNT = FICOUNT + 1.
          PERFORM ADD_GL_LINE_ITEM.
        ENDIF.
        OLD_NEWBS = ZBSEG-NEWBS.

*     additional text record - collect all text in internal table
      WHEN '3'.
        CLEAR TEXTTAB.
        TEXTTAB-STR = INREC+1(200).
        APPEND TEXTTAB.
    ENDCASE.
  ENDDO.

* select F14 - Document overview
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/14'.
* pop-up window "Coding block" appears - select F8 to continue
  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.
* process rest of very last document
  PERFORM ADD_TEXT.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
  PERFORM INSERT_SESSION.

  CLOSE DATASET INFILE.
  PERFORM CLOSE_SESSION.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
* - This routine just simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM OPEN_BATCH_SESSION.

* check system ID
   IF ( SY-MANDT <> ZBGR00-MANDT ).
      MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH
         'Input client number differs from the system client'.
   ENDIF.
* if XKEEP flag is not set to 'X' or ' ' - set it to an 'X'
  IF ( ZBGR00-XKEEP <> 'X' ) OR ( ZBGR00-XKEEP <> SPACE ).
    ZBGR00-XKEEP = 'X'.
  ENDIF.
* if user wasn't passed in file header record, use logon user
  IF ( ZBGR00-USNAM = SPACE ) OR ( ZBGR00-USNAM(1) = '/' ).
    ZBGR00-USNAM = SY-UNAME.
  ENDIF.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = ZBGR00-GROUP
            HOLDDATE          = ZBGR00-START
            KEEP              = ZBGR00-XKEEP
            USER              = ZBGR00-USNAM
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
    MESSAGE E004 WITH ZBGR00-GROUP.
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
            TCODE          = G_TCODE
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
    MESSAGE I003 WITH ZBGR00-GROUP.
  ENDIF.
ENDFORM.


*-----------------------------------------------------------------------
*     FORM START_NEW_TRANSACTION
*-----------------------------------------------------------------------
* - This routine provides the BDC mapping for the initial screen in
* the transaction.
*-----------------------------------------------------------------------
FORM START_NEW_TRANSACTION.

  DATA: TBLDAT TYPE D,
        TBUDAT TYPE D,
        ZBLDAT(10),                    "date in user format
        ZBUDAT(10).

* convert dates to user-default format
  TBLDAT = ZBBKPF-BLDAT.
  TBUDAT = ZBBKPF-BUDAT.
  WRITE TBLDAT TO ZBLDAT DD/MM/YYYY.
  WRITE TBUDAT TO ZBUDAT DD/MM/YYYY.
* Header data screen (initial screen)
  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' ZBLDAT.
  PERFORM BDC_FIELD  USING 'BKPF-BLART' ZBBKPF-BLART.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' ZBBKPF-BUKRS.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' ZBUDAT.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' ZBBKPF-WAERS.
  PERFORM BDC_FIELD  USING 'BKPF-XBLNR' ZBBKPF-XBLNR.
  PERFORM BDC_FIELD  USING 'BKPF-BKTXT' ZBBKPF-BKTXT.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM FINISH_TRANSACTION
*-----------------------------------------------------------------------
* - This routine finishes the BDC mapping for the transaction.
*-----------------------------------------------------------------------
FORM FINISH_TRANSACTION.

* select F14 - Document overview
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/14'.
* pop-up window "Coding block" appears - select F8 to continue
  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.
  PERFORM ADD_TEXT.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
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
FORM START_NEXT_ITEM USING F_NEWBS F_NEWKO O_NEWBS o_newbk.   "ML

  PERFORM BDC_FIELD  USING 'RF05A-NEWBS' F_NEWBS.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' F_NEWKO.
  perform bdc_field  using 'RF05a-NEWBK' O_NEWBK.
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

* Screen: Enter vendor invoice: address and bank data
  PERFORM BDC_SCREEN USING 'SAPLFCPD' '100'.
  PERFORM BDC_FIELD  USING 'BSEC-NAME1' ZBSEG-NAME1.
  PERFORM BDC_FIELD  USING 'BSEC-NAME2' ZBSEG-NAME2.
  PERFORM BDC_FIELD  USING 'BSEC-NAME3' ZBSEG-NAME3.
  PERFORM BDC_FIELD  USING 'BSEC-NAME4' ZBSEG-NAME4.
  PERFORM BDC_FIELD  USING 'BSEC-STRAS' ZBSEG-STRAS.
  PERFORM BDC_FIELD  USING 'BSEC-PFACH' ZBSEG-PFACH.
  PERFORM BDC_FIELD  USING 'BSEC-ORT01' ZBSEG-ORT01.
  PERFORM BDC_FIELD  USING 'BSEC-LAND1' ZBSEG-LAND1.
  PERFORM BDC_FIELD  USING 'BSEC-PSTL2' ZBSEG-PSTL2.
  PERFORM BDC_FIELD  USING 'BSEC-PSTLZ' ZBSEG-PSTLZ.
  PERFORM BDC_FIELD  USING 'BSEC-REGIO' ZBSEG-REGIO.
  PERFORM BDC_FIELD  USING 'BSEC-STCD1' ZBSEG-STCD1.


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
* PERFORM BDC_SCREEN USING 'SAPMF05A' '2302'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '0302'.                  " 4.6B
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' ZBSEG-WRBTR.
  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ' ZBSEG-MWSKZ.
  IF ( ZBSEG-ZFBDT <> SPACE ) AND ( ZBSEG-ZFBDT(1) <> '/' ).
*   convert date to user-default format
    TZFBDT = ZBSEG-ZFBDT.
    WRITE TZFBDT TO ZZFBDT DD/MM/YYYY.
    PERFORM BDC_FIELD  USING 'BSEG-ZFBDT' ZZFBDT.
  ENDIF.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT' ZBSEG-SGTXT.
  PERFORM BDC_FIELD  USING 'BSEG-WSKTO' ZBSEG-WSKTO.
  if zbseg-skfbt <> space.                          "2001/02/28 mdemeest
     perform bdc_field using 'BSEG-SKFBT' ZBSEG-SKFBT.
  endif.
  PERFORM BDC_FIELD  USING 'BSEG-ZTERM' ZBSEG-ZTERM.
  PERFORM BDC_FIELD  USING 'BSEG-ZLSCH' ZBSEG-ZLSCH.
  PERFORM BDC_FIELD  USING 'BSEG-EMPFB' ZBSEG-EMPFB.
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
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' ZBSEG-WRBTR.
  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ' ZBSEG-MWSKZ.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT' ZBSEG-SGTXT.
  if zbseg-xskrl = 'X'.                                "2001/03/06 PHH
     perform BDC_FIELD using 'BSEG-XSKRL' ZBSEG-XSKRL.
  endif.
* coding block screen
  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
  PERFORM BDC_FIELD  USING 'COBL-KOSTL' ZBSEG-KOSTL.
  PERFORM BDC_FIELD  USING 'COBL-AUFNR' ZBSEG-AUFNR.
  PERFORM BDC_FIELD  USING 'COBL-PS_PSP_PNR' ZBSEG-PROJK.
* perform BDC_FIELD  USING 'COBL-NPLNR' ZBSEG-NPLNR.        "DIFFERENCES
* perform BDC_FIELD  USING 'COBL-VORNR' ZBSEG-VORNR.        "DIFFERENCES
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
*  PERFORM BDC_FIELD  USING 'RF05A-NEWBK' ZBSEG-NEWBK.          "ML

ENDFORM.


*-----------------------------------------------------------------------
*     FORM ADD_TEXT
*-----------------------------------------------------------------------
* - This is the routine to add the long text to the invoice.  The long
* text is entered in the SAPScript editor.
* - Note that the text must be added in 71 character increments since
* each line in the SAPScript editor is 72 characters long.
* - Also note that if each line of text is not explicitly made a new
* line in the editor, then the editor will do its own formatting when
* the text is saved.  There are 2 ways to prevent this formatting:
*   a.) Only enter the text up to the 71st column.  If there is no
*       character in the 72nd column then the text is unaffected by
*       the formatting.  An important SAPScript note is that this
*       text is then treated as one continuous line when the text
*       element is referenced in a layout set.
*   b.) Each line of text entered must be made a new line which can
*       be done by adding a '/' in the tag column.  This can be
*       be accomplished by selecting F6 after each line is added.
*       This affects SAPScript in that when this text object is
*       referenced in a layout set the text will be treated as
*       seperate lines each 72 characters long (as opposed to one
*       continuous line).
* - Note that we will start each line with one additional space. This
* is to prevent automatic concatenation of 2 words when one word ends
* on the 72 column and the next word starts in column 1 of the next
* line.
* - This choice of automatic editor formatting vs. making each line
* seperate is given as a parameter to the program.
*-----------------------------------------------------------------------
FORM ADD_TEXT.

* local variables
  DATA: TEXT_LEN TYPE I,
        COUNTER(2) TYPE N,
        TAGNO(2) TYPE N,
        OFF TYPE I,
        LINEOFF TYPE I,
        TEXT_RECS TYPE I,
        LINETEXT(72).
* screen field names of SAPscript editor and tag lines.
  DATA: EDITOR_LINE(16) VALUE 'RSTXT-TXLINE(  )',
        TAG(20) VALUE 'RSTXT-TXPARGRAPH(  )'.
* editor line numbers of which tag colums where tags will be placed
*  DATA: BEGIN OF TAGLINES OCCURS 5,
*          NUM(2) TYPE N,
*        END OF TAGLINES.


  DESCRIBE TABLE TEXTTAB LINES TEXT_RECS.
  CHECK TEXT_RECS > 0.

  COUNTER = 1.
* select Extras->texts from overview screen
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'TEXT'.
* select 3rd line from pop-up window "Texts in Accounting Document"
* and then F2 for detailed text (this text is allocated for the
* "Cheque Stub Message")
  PERFORM BDC_SCREEN USING 'SAPLFTXT' '100'.
*  PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'RTEXT-SELKZ(3)'.  "4.6b
  perform bdc_field using 'RTEXT-SELKZ(3)' 'X'.              "4.6B
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/2'.

  PERFORM BDC_SCREEN USING 'SAPLSTXX' '1100'.
  TAG+17(2) = '02'.
  PERFORM BDC_FIELD  USING TAG '* '.
* add the text in the SAPScript editor 71 characters at a time
  LOOP AT TEXTTAB.
* each new line of text reports separately ------ start of Mradsma-98/05
    TEXT_IND = 'Y'.                                      " Mradsma-98/05
    COUNTER = COUNTER + 1.                               " Mradsma-98/05
    IF EDFORMAT = SPACE                                  " Mradsma-98/05
    AND COUNTER > 2.                                     " Mradsma-98/05
      TAG+17(2) = COUNTER.                               " Mradsma-98/05
      PERFORM BDC_FIELD USING TAG '/ '.                  " Mradsma-98/05
    ENDIF.                                               " Mradsma-98/05
* ------------------------------------------------- end of Mradsma-98/05
    CLEAR: OFF, LINEOFF.
    LINEOFF = 1.
    TEXT_LEN = STRLEN( TEXTTAB-STR ).
*   keep track of line numbers to add tags to
*    CLEAR TAGLINES.
*    TAGLINES-NUM = COUNTER + 1.
*    APPEND TAGLINES.
    WHILE ( OFF < TEXT_LEN ).
      CLEAR: LINETEXT.
*     counter = counter + 1.
      EDITOR_LINE+13(2) = COUNTER.
*     add extra space to beginning of text line.
      LINETEXT+LINEOFF(71) = TEXTTAB-STR+OFF(71).
*      LINETEXT = TEXTTAB-STR+OFF(71).
      PERFORM BDC_FIELD  USING EDITOR_LINE LINETEXT.
      OFF = OFF + 71.
      CLEAR: LINEOFF.
*     if editor format was not checked - select F6 (line)
      CHECK EDFORMAT = SPACE.
* only create new line if more text ------------- start of Mradsma-98/05
      IF OFF < TEXT_LEN.                                 " Mradsma-98/05
        COUNTER = COUNTER + 1.                           " Mradsma-98/05
        TAG+17(2) = COUNTER.                             " Mradsma-98/05
        PERFORM BDC_FIELD USING TAG '/ '.                " Mradsma-98/05
      ENDIF.                                             " Mradsma-98/05
*     perform bdc_field  using 'BDC_OKCODE' '/6'.        " Mradsma-98/05
*     perform bdc_screen using 'SAPLSTXX' '1100'.        " Mradsma-98/05
    ENDWHILE.                                            " Mradsma-98/05
    TEXT_IND = 'N'.                                      " Mradsma-98/05
* ------------------------------------------------- end of Mradsma-98/05
  ENDLOOP.
* add '*' to tag column of first line of text and '/' for each complete
* subsequent line of text
*  LOOP AT TAGLINES.
*    IF SY-TABIX = 1.
*      TAG+17(2) = TAGLINES-NUM.
*      PERFORM BDC_FIELD  USING TAG '* '.
*    ELSE.
*      TAG+17(2) = TAGLINES-NUM.
*      PERFORM BDC_FIELD  USING TAG '/ '.
*    ENDIF.
*  ENDLOOP.
* select F11 to save the text

  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'TXEX'.   "4.6B

*  PERFORM BDC_SCREEN USING 'SAPLSTXX' '1100'.
* select F3 to exit from editor
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/3'.   "4.6B
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'TXEX'.  "4.6b


* select F13 to continue from pop-up window
  PERFORM BDC_SCREEN USING 'SAPLFTXT' '100'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/13'.  "4.6B
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'BACK'.  "4.6B
* returns to overview screen
  PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM CHECK_ONE_TIME_VENDOR
*-----------------------------------------------------------------------
* - This routine determines if the vendor is a one-time vendor or not.
*
*  Parameters:
*      <--  ONE_TIME - flag is set if vendor is a one-time vendor
*-----------------------------------------------------------------------
FORM CHECK_ONE_TIME_VENDOR USING ONE_TIME.

  DATA: KTOKK_KEY LIKE LFA1-KTOKK.

  CLEAR ONE_TIME.
  SELECT SINGLE KTOKK INTO KTOKK_KEY FROM LFA1
                      WHERE LIFNR = ZBSEG-HKONT.
  READ TABLE OTV WITH KEY KTOKK_KEY BINARY SEARCH.
  IF SY-SUBRC = 0.
    ONE_TIME = 'X'.
  ENDIF.

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

  TVAL = FVAL(1).
* if field is not BDC OKCODE, only add to BDC table if not initialized
* with ' ' or '/'.
* and if not processing the check stub text fields       " Mradsma-98/05
  IF FNAM <> 'BDC_OKCODE'
  AND TEXT_IND = 'N'.                                    " Mradsma-98/05
    CHECK FVAL <> SPACE AND TVAL <> '/'.
  ENDIF.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.
