REPORT zfapi001 MESSAGE-ID zs.
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
* 2012/11/21 GYMANA  EHP5  Removed all logical file coding and replaced
*                          with conventional OPEN DATASET logic
*12/01/2011  M Khan  TR846 Changes to automatically calculate Tax.
* 2006/04/05 gymana   4.7  Removed MaryLou's inrec46b layout coding
*                          since the Mercator maps are being replaced by
*                          ABAPs and the BDC layouts will now be in
*                          sync with the BBKPF/BGR00/ZBSEG structures.
* 2001/03/06 mdemeest PHH  Added discount/no discount flag
* 2001/02/26 mdemeest PHH  Added posting of SKFBT -Amount without
*                          discount
* 2000/09/20 mdemeest 4.6B Changed okcodes for TEXT screens
*
* 2000/09/08 gymana   4.6B Modified 4.6B code to handle type 3 record.
*
* 2000/06/10 mdemeest 4.6B Accept expanded 4.6B layout
*
* May 98 - MRadsma - facilitate the use of text in line by line format
*                    rather than word wrap.  Changes can be found by
*                    searching for MRadsma-98/05
************************************************************************
*ABLES: LFA1,                                     "vendor master
*       T077K.                                    "vendor account groups

TABLES: bseg.

DATA: inrec(1700).                                "input record
* input record structure - session header
DATA: BEGIN OF zbgr00.
        INCLUDE STRUCTURE bgr00.
DATA: END OF zbgr00.
* input record structure - document header
DATA: BEGIN OF zbbkpf.
        INCLUDE STRUCTURE bbkpf.
DATA: END OF zbbkpf.
* input record structure - document line item
DATA: BEGIN OF zbseg.
        INCLUDE STRUCTURE zbseg.
DATA: END OF zbseg.
* input record stucture - additional text
DATA: BEGIN OF texttab OCCURS 1,
        str(203),
      END OF texttab.
* list of one time vendor account groups
DATA: BEGIN OF otv OCCURS 1,
        ktokk LIKE lfa1-ktokk,
      END OF otv.
* batch input data
DATA: BEGIN OF bdcdata OCCURS 500.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
* client ID
DATA: wrk_symbolic(4) TYPE c VALUE '$sys'.
* real name of input file
DATA: infile(70).
* transaction code for batch input
DATA: g_tcode LIKE tstc-tcode.
* name of batch input session.
DATA: bdcname LIKE zbgr00-group.
* current count of FI line items in the invoice
DATA: ficount(1) TYPE n.
* check stub text indicator (to allow a slash '/' in BDC fields)
DATA: text_ind(1) TYPE c VALUE 'N'.                      " Mradsma-98/05
*=======================================================================
* SELECTION SCREEN
*=======================================================================
* logical file name
PARAMETERS: p_xref3 TYPE bseg-xref3 OBLIGATORY.
PARAMETERS: p_infile LIKE filename-fileextern OBLIGATORY.         "Ehp5
* format text in SAPScript editor?
PARAMETERS: edformat AS CHECKBOX DEFAULT 'X'.

*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================
*AT SELECTION-SCREEN.
*  PERFORM CHECK_FILE.
AT SELECTION-SCREEN.                                              "Ehp5
  REPLACE wrk_symbolic WITH sy-sysid INTO p_infile.               "Ehp5
  CONDENSE: p_infile NO-GAPS.                                     "Ehp5
*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.
  PERFORM open_file.                                              "Ehp5
  PERFORM one_time_vendor_list.
  PERFORM create_batch_input.
*-----------------------------------------------------------------------
FORM open_file.                                                   "Ehp5
  "Ehp5
  DATA: msg(100).         "open file - system message             "Ehp5
  "Ehp5
  OPEN DATASET p_infile FOR INPUT IN TEXT MODE MESSAGE msg.       "Ehp5
  IF ( sy-subrc <> 0 ).                                           "Ehp5
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '002' WITH p_infile msg.      "Ehp5
  ENDIF.                                                          "Ehp5
  "Ehp5
ENDFORM.                    "OPEN_FILE                            "Ehp5

*-----------------------------------------------------------------------
*  FORM ONE_TIME_VENDOR_LIST
*-----------------------------------------------------------------------
* - A little routine to create an internal table with all one-time
* vendor account groups.
*-----------------------------------------------------------------------
FORM one_time_vendor_list.

  SELECT ktokk INTO otv-ktokk FROM t077k WHERE xcpds = 'X'.
    APPEND otv.
  ENDSELECT.
  SORT otv.

ENDFORM.                    "ONE_TIME_VENDOR_LIST


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
FORM create_batch_input.

  DATA: msg(100),                           "open file - system message
        first_doc(1),                       "flag - first document?
        otflag(1),                          "flag - one-time vendor?
        old_newbs LIKE zbseg-newbs.         "previous posting key

  REFRESH bdcdata.

  first_doc = 'X'.
* do until we have read in the complete file.
  DO.
    CLEAR: otflag.
    READ DATASET p_infile INTO inrec.
*   check for empty file
    IF ( sy-index = 1 ).
      IF ( sy-subrc = 4 ).
        MESSAGE i028.
        STOP.
      ENDIF.
    ENDIF.
*   exit loop when end of file
    IF ( sy-subrc <> 0 ).
      EXIT.
    ENDIF.

    CASE inrec(1).
*     session header record....
      WHEN '0'.
        zbgr00 = inrec.
        zbgr00-start = sy-datum - 1.                          " 4.6B
*       if this is not the first document, finish the last document
*       and close out the current session.
        IF ( first_doc = space ).
          PERFORM finish_transaction.
          PERFORM close_session.
        ENDIF.
        PERFORM open_batch_session.
        first_doc = 'X'.
*     document header record....
      WHEN '1'.
        CLEAR old_newbs.
*       process rest of previous document (if not first one)
        IF ( first_doc = space ).
          PERFORM finish_transaction.
        ENDIF.
        REFRESH texttab.
        REFRESH bdcdata.
        zbbkpf = inrec.
        g_tcode = zbbkpf-tcode.
        PERFORM start_new_transaction.
        CLEAR: first_doc, ficount.
*     document line item record....
      WHEN '2'.
        zbseg = inrec.
        PERFORM start_next_item USING zbseg-newbs zbseg-hkont old_newbs.
*       posting key '21' or '31' indicates vendor line item
        IF ( zbseg-newbs = '21' ) OR ( zbseg-newbs = '31' ).
          PERFORM check_one_time_vendor USING otflag.
          IF ( otflag = 'X' ).
            PERFORM add_vendor_desc.
          ENDIF.
          PERFORM add_vendor_line_item.
        ELSE.
          ficount = ficount + 1.
          PERFORM add_gl_line_item.
        ENDIF.
        old_newbs = zbseg-newbs.
*     additional text record - collect all text in internal table
      WHEN '3'.
        CLEAR texttab.
        texttab-str = inrec+1(200).
        APPEND texttab.
    ENDCASE.
  ENDDO.

  PERFORM add_text.
*************New lines
  PERFORM bdc_field  USING 'BDC_OKCODE' 'BP'. "'PBBP'. "Save as Complete
  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
****************************
** select F14 - Document overview
*  PERFORM bdc_field  USING 'BDC_OKCODE' '/14'.
** pop-up window "Coding block" appears - select F8 to continue
*  PERFORM bdc_screen USING 'SAPLKACB' '002'.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.
*  PERFORM bdc_screen USING 'SAPMF05A' '700'.
** process rest of very last document
*  PERFORM add_text.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '/11'.
  PERFORM insert_session.

  CLOSE DATASET p_infile.                                         "Ehp5
  PERFORM close_session.

ENDFORM.                    "CREATE_BATCH_INPUT


*-----------------------------------------------------------------------
*     FORM OPEN_BATCH_SESSION
*-----------------------------------------------------------------------
* - This routine just simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM open_batch_session.

* check system ID
  IF ( sy-mandt <> zbgr00-mandt ).
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH
       'Input client number differs from the system client'.
  ENDIF.
* if XKEEP flag is not set to 'X' or ' ' - set it to an 'X'
  IF ( zbgr00-xkeep <> 'X' ) OR ( zbgr00-xkeep <> space ).
    zbgr00-xkeep = 'X'.
  ENDIF.
* if user wasn't passed in file header record, use logon user
  IF ( zbgr00-usnam = space ) OR ( zbgr00-usnam(1) = '/' ).
    zbgr00-usnam = sy-uname.
  ENDIF.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = zbgr00-group
      holddate          = zbgr00-start
      keep              = zbgr00-xkeep
      user              = zbgr00-usnam
    EXCEPTIONS
      group_invalid     = 1
      group_is_locked   = 2
      holddate_invalid  = 3
      internal_error    = 4
      queue_error       = 5
      running           = 6
      system_lock_error = 7
      user_invalid      = 8.

  IF sy-subrc <> 0.
    MESSAGE e004 WITH zbgr00-group.
  ENDIF.

ENDFORM.                    "OPEN_BATCH_SESSION


*-----------------------------------------------------------------------
*     FORM INSERT_SESSION
*-----------------------------------------------------------------------
* - This routine inserts the BDC data for one transaction into the
*   batch input session.
*-----------------------------------------------------------------------
FORM insert_session.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = g_tcode
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.
  IF sy-subrc <> 0.
    MESSAGE e013 WITH sy-subrc.
  ENDIF.

ENDFORM.                    "INSERT_SESSION


*-----------------------------------------------------------------------
*     FORM CLOSE_SESSION
*-----------------------------------------------------------------------
* - This routine simply closes the current batch input session.
*-----------------------------------------------------------------------
FORM close_session.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open
      queue_error.
  IF sy-subrc = 0.
    MESSAGE i003 WITH zbgr00-group.
  ENDIF.
ENDFORM.                    "CLOSE_SESSION


*-----------------------------------------------------------------------
*     FORM START_NEW_TRANSACTION
*-----------------------------------------------------------------------
* - This routine provides the BDC mapping for the initial screen in
* the transaction.
*-----------------------------------------------------------------------
FORM start_new_transaction.

  DATA: tbldat TYPE d,
        tbudat TYPE d,
        zbldat(10),                    "date in user format
        zbudat(10).

* convert dates to user-default format
  tbldat = zbbkpf-bldat.
  tbudat = zbbkpf-budat.
  WRITE tbldat TO zbldat DD/MM/YYYY.
  WRITE tbudat TO zbudat DD/MM/YYYY.
* Header data screen (initial screen)
  PERFORM bdc_screen USING 'SAPMF05A' '100'.
  PERFORM bdc_field  USING 'BKPF-BLDAT' zbldat.
  PERFORM bdc_field  USING 'BKPF-BLART' zbbkpf-blart.
  PERFORM bdc_field  USING 'BKPF-BUKRS' zbbkpf-bukrs.
  PERFORM bdc_field  USING 'BKPF-BUDAT' zbudat.
  PERFORM bdc_field  USING 'BKPF-WAERS' zbbkpf-waers.
  PERFORM bdc_field  USING 'BKPF-XBLNR' zbbkpf-xblnr.
  PERFORM bdc_field  USING 'BKPF-BKTXT' zbbkpf-bktxt.

ENDFORM.                    "START_NEW_TRANSACTION


*-----------------------------------------------------------------------
*     FORM FINISH_TRANSACTION
*-----------------------------------------------------------------------
* - This routine finishes the BDC mapping for the transaction.
*-----------------------------------------------------------------------
FORM finish_transaction.

*** select F14 - Document overview
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/14'.
*** pop-up window "Coding block" appears - select F8 to continue
*  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
*  PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.
  PERFORM add_text.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.
*************New lines
  PERFORM bdc_field  USING 'BDC_OKCODE' 'BP'. "'PBBP'. "Save as Complete
  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.
************************
  PERFORM insert_session.

ENDFORM.                    "FINISH_TRANSACTION


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
FORM start_next_item USING f_newbs f_newko o_newbs.

  PERFORM bdc_field  USING 'RF05A-NEWBS' f_newbs.
  PERFORM bdc_field  USING 'RF05A-NEWKO' f_newko.
* coding block appears after a G/L line item is entered
  IF ( o_newbs <> '21' ) AND ( o_newbs <> '31' ) AND
     ( o_newbs <> space ).
    PERFORM bdc_screen USING 'SAPLKACB' '002'.
    PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.
  ENDIF.

ENDFORM.                    "START_NEXT_ITEM


*-----------------------------------------------------------------------
*     FORM ADD_VENDOR_DESC
*-----------------------------------------------------------------------
* - When one-time vendors are used an additional screen appears in
* which specific information about the vendor is entered (such as
* name and address).
*-----------------------------------------------------------------------
FORM add_vendor_desc.

* Screen: Enter vendor invoice: address and bank data
  PERFORM bdc_screen USING 'SAPLFCPD' '100'.
  PERFORM bdc_field  USING 'BSEC-NAME1' zbseg-name1.
  PERFORM bdc_field  USING 'BSEC-NAME2' zbseg-name2.
  PERFORM bdc_field  USING 'BSEC-NAME3' zbseg-name3.
  PERFORM bdc_field  USING 'BSEC-NAME4' zbseg-name4.
  PERFORM bdc_field  USING 'BSEC-STRAS' zbseg-stras.
  PERFORM bdc_field  USING 'BSEC-PFACH' zbseg-pfach.
  PERFORM bdc_field  USING 'BSEC-ORT01' zbseg-ort01.
  PERFORM bdc_field  USING 'BSEC-LAND1' zbseg-land1.
  PERFORM bdc_field  USING 'BSEC-PSTL2' zbseg-pstl2.
  PERFORM bdc_field  USING 'BSEC-PSTLZ' zbseg-pstlz.
  PERFORM bdc_field  USING 'BSEC-REGIO' zbseg-regio.
  PERFORM bdc_field  USING 'BSEC-STCD1' zbseg-stcd1.


ENDFORM.                    "ADD_VENDOR_DESC


*-----------------------------------------------------------------------
*     FORM ADD_VENDOR_LINE_ITEM
*-----------------------------------------------------------------------
* - This is the BDC mapping for the screen when entering a vendor line
* item.
*-----------------------------------------------------------------------
FORM add_vendor_line_item.

  DATA: tzfbdt TYPE d,
        zzfbdt(10).

* Create vendor item screen
* PERFORM BDC_SCREEN USING 'SAPMF05A' '2302'.
  PERFORM bdc_screen USING 'SAPMF05A' '0302'.                  " 4.6B
  PERFORM bdc_field  USING 'BSEG-WRBTR' zbseg-wrbtr.
  PERFORM bdc_field  USING 'BSEG-MWSKZ' zbseg-mwskz.
  IF ( zbseg-zfbdt <> space ) AND ( zbseg-zfbdt(1) <> '/' ).
*   convert date to user-default format
    tzfbdt = zbseg-zfbdt.
    WRITE tzfbdt TO zzfbdt DD/MM/YYYY.
    PERFORM bdc_field  USING 'BSEG-ZFBDT' zzfbdt.
  ENDIF.
  PERFORM bdc_field  USING 'BSEG-SGTXT' zbseg-sgtxt.
  PERFORM bdc_field  USING 'BSEG-WSKTO' zbseg-wskto.
  IF zbseg-skfbt <> space.                          "2001/02/28 mdemeest
    PERFORM bdc_field USING 'BSEG-SKFBT' zbseg-skfbt.
  ENDIF.
  PERFORM bdc_field  USING 'BSEG-ZTERM' zbseg-zterm.
  PERFORM bdc_field  USING 'BSEG-ZLSCH' zbseg-zlsch.
  PERFORM bdc_field  USING 'BDC_OKCODE' 'ZK'.
*Route Code Owner
  PERFORM bdc_screen USING 'SAPMF05A' '0332'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.
  PERFORM bdc_field  USING 'BSEG-XREF3' p_xref3.

ENDFORM.                    "ADD_VENDOR_LINE_ITEM


*-----------------------------------------------------------------------
*     FORM ADD_GL_LINE_ITEM
*-----------------------------------------------------------------------
* - This is the BDC mapping for the screen when entering a G/L line
* item.
*-----------------------------------------------------------------------
FORM add_gl_line_item.

* Screen: Create G/L account item
  PERFORM bdc_screen USING 'SAPMF05A' '300'.
  PERFORM bdc_field  USING 'BSEG-WRBTR' zbseg-wrbtr.
  PERFORM bdc_field  USING 'BSEG-MWSKZ' zbseg-mwskz.
  PERFORM bdc_field  USING 'BSEG-SGTXT' zbseg-sgtxt.
  IF zbseg-xskrl = 'X'.                                "2001/03/06 PHH
    PERFORM bdc_field USING 'BSEG-XSKRL' zbseg-xskrl.
  ENDIF.
  IF zbbkpf-xmwst = 'X'.                               "TR846
    PERFORM bdc_field USING 'BKPF-XMWST' zbbkpf-xmwst.
  ENDIF.

* coding block screen
  PERFORM bdc_screen USING 'SAPLKACB' '002'.
  PERFORM bdc_field  USING 'COBL-KOSTL' zbseg-kostl.
  PERFORM bdc_field  USING 'COBL-AUFNR' zbseg-aufnr.
  PERFORM bdc_field  USING 'COBL-PS_PSP_PNR' zbseg-projk.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.
  PERFORM bdc_screen USING 'SAPMF05A' '300'.

ENDFORM.                    "ADD_GL_LINE_ITEM


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
FORM add_text.

* local variables
  DATA: text_len TYPE i,
        counter(2) TYPE n,
        tagno(2) TYPE n,
        off TYPE i,
        lineoff TYPE i,
        text_recs TYPE i,
        linetext(72).
* screen field names of SAPscript editor and tag lines.
  DATA: editor_line(16) VALUE 'RSTXT-TXLINE(  )',
        tag(20) VALUE 'RSTXT-TXPARGRAPH(  )'.
* editor line numbers of which tag colums where tags will be placed
*  DATA: BEGIN OF TAGLINES OCCURS 5,
*          NUM(2) TYPE N,
*        END OF TAGLINES.


  DESCRIBE TABLE texttab LINES text_recs.
  CHECK text_recs > 0.

  counter = 1.
* select Extras->texts from overview screen
  PERFORM bdc_field  USING 'BDC_OKCODE' 'TEXT'.
* select 3rd line from pop-up window "Texts in Accounting Document"
* and then F2 for detailed text (this text is allocated for the
* "Cheque Stub Message")
  PERFORM bdc_screen USING 'SAPLFTXT' '100'.
*  PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'RTEXT-SELKZ(3)'.  "4.6b
  PERFORM bdc_field USING 'RTEXT-SELKZ(3)' 'X'.              "4.6B
  PERFORM bdc_field  USING 'BDC_OKCODE' '/2'.

  PERFORM bdc_screen USING 'SAPLSTXX' '1100'.
  tag+17(2) = '02'.
  PERFORM bdc_field  USING tag '* '.
* add the text in the SAPScript editor 71 characters at a time
  LOOP AT texttab.
* each new line of text reports separately ------ start of Mradsma-98/05
    text_ind = 'Y'.                                      " Mradsma-98/05
    counter = counter + 1.                               " Mradsma-98/05
    IF edformat = space                                  " Mradsma-98/05
    AND counter > 2.                                     " Mradsma-98/05
      tag+17(2) = counter.                               " Mradsma-98/05
      PERFORM bdc_field USING tag '/ '.                  " Mradsma-98/05
    ENDIF.                                               " Mradsma-98/05
* ------------------------------------------------- end of Mradsma-98/05
    CLEAR: off, lineoff.
    lineoff = 1.
    text_len = strlen( texttab-str ).
*   keep track of line numbers to add tags to
*    CLEAR TAGLINES.
*    TAGLINES-NUM = COUNTER + 1.
*    APPEND TAGLINES.
    WHILE ( off < text_len ).
      CLEAR: linetext.
*     counter = counter + 1.
      editor_line+13(2) = counter.
*     add extra space to beginning of text line.
      linetext+lineoff(71) = texttab-str+off(71).
*      LINETEXT = TEXTTAB-STR+OFF(71).
      PERFORM bdc_field  USING editor_line linetext.
      off = off + 71.
      CLEAR: lineoff.
*     if editor format was not checked - select F6 (line)
      CHECK edformat = space.
* only create new line if more text ------------- start of Mradsma-98/05
      IF off < text_len.                                 " Mradsma-98/05
        counter = counter + 1.                           " Mradsma-98/05
        tag+17(2) = counter.                             " Mradsma-98/05
        PERFORM bdc_field USING tag '/ '.                " Mradsma-98/05
      ENDIF.                                             " Mradsma-98/05
*     perform bdc_field  using 'BDC_OKCODE' '/6'.        " Mradsma-98/05
*     perform bdc_screen using 'SAPLSTXX' '1100'.        " Mradsma-98/05
    ENDWHILE.                                            " Mradsma-98/05
    text_ind = 'N'.                                      " Mradsma-98/05
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

  PERFORM bdc_field  USING 'BDC_OKCODE' 'TXEX'.   "4.6B

*  PERFORM BDC_SCREEN USING 'SAPLSTXX' '1100'.
* select F3 to exit from editor
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/3'.   "4.6B
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'TXEX'.  "4.6b


* select F13 to continue from pop-up window
  PERFORM bdc_screen USING 'SAPLFTXT' '100'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/13'.  "4.6B
  PERFORM bdc_field  USING 'BDC_OKCODE' 'BACK'.  "4.6B
* returns to overview screen
  PERFORM bdc_screen USING 'SAPMF05A' '700'.

ENDFORM.                    "ADD_TEXT


*-----------------------------------------------------------------------
*     FORM CHECK_ONE_TIME_VENDOR
*-----------------------------------------------------------------------
* - This routine determines if the vendor is a one-time vendor or not.
*
*  Parameters:
*      <--  ONE_TIME - flag is set if vendor is a one-time vendor
*-----------------------------------------------------------------------
FORM check_one_time_vendor USING one_time.

  DATA: ktokk_key LIKE lfa1-ktokk.

  CLEAR one_time.
  SELECT SINGLE ktokk INTO ktokk_key FROM lfa1
                      WHERE lifnr = zbseg-hkont.
  READ TABLE otv WITH KEY ktokk_key BINARY SEARCH.
  IF sy-subrc = 0.
    one_time = 'X'.
  ENDIF.

ENDFORM.                    "CHECK_ONE_TIME_VENDOR


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
FORM bdc_screen USING program dynpro.

  CLEAR bdcdata.
  bdcdata-program = program.
  bdcdata-dynpro = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.

ENDFORM.                    "BDC_SCREEN


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
FORM bdc_field USING fnam fval.

  DATA: tval.

  tval = fval(1).
* if field is not BDC OKCODE, only add to BDC table if not initialized
* with ' ' or '/'.
* and if not processing the check stub text fields       " Mradsma-98/05
  IF fnam <> 'BDC_OKCODE'
  AND text_ind = 'N'.                                    " Mradsma-98/05
    CHECK fval <> space AND tval <> '/'.
  ENDIF.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD



*PROGRAM: sapmf05a
*SCREEN: 0302
*SCREEN FIELD: bseg-zlsch
*PROGRAM: sapmf05a
*SCREEN: 0332
*SCREEN FIELD: bseg-xref3
