REPORT ZFAPI025 MESSAGE-ID ZS.
************************************************************************
*
*COPY OF PROGRAM ZFAPI025 FOR TESTING PURPOSE ONLY.
*
*  Author:    Mohammad Khan.
*
*  Date:      February, 2002.
*
*  Issue Log: 948
*
*  Description:
*     - The purpose of this program is to create a BDC session to post
*       the invoices coming from Lands System using transaction
*       "FB01".
*
************************************************************************
* ---------------------- CHANGE LOG ------------------------------------
* 2004/05/20 mdemeest #--- Expand cheque text comments from 70 to 200
*                          LANDAT is currently limiting the # of
*                          characters to 100.
************************************************************************

DATA: INREC(264).                                "Input record


* Batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

* input record stucture - additional text
DATA: BEGIN OF TEXTTAB OCCURS 1,
        STR(71),
      END OF TEXTTAB.

* Input file name with path
DATA: INFILE(70).

* Dates Data
DATA:   TBLDAT TYPE D,
        TBUDAT TYPE D,
        ZBLDAT(10),                        "date in user format
        ZBUDAT(10),
        FLENGTH TYPE I,
        COUNT3(2) TYPE N,
        BPOINT  TYPE I,
        WSPOSID LIKE COBL-PS_POSID.        "23/05/02 CHANGES


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
    LGTXT(70),
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
  NEWKO   LIKE BBSEG-NEWKO,       "Cost Element
  MWSKZ   LIKE BBSEG-MWSKZ,       "Tax Code
  SGTXT   LIKE BBSEG-SGTXT,
  LAND1   LIKE BBSEG-LAND1,
END OF INREC2.

DATA:
BEGIN OF INREC3,
  STYPE   LIKE BBKPF-STYPE,
  NEWBS   LIKE BBSEG-NEWBS,
  WRBTR   LIKE BBSEG-WRBTR,
  NEWKO   LIKE BBSEG-NEWKO,       "Cost Element
  PS_POSID LIKE COBL-PS_POSID,    "WBS or Cost Center or Int. Order
  SGTXT(200),                     "2004/05/20 increased from 70
*  SGTXT   LIKE BBSEG-SGTXT,
END OF INREC3.

*=======================================================================
* SELECTION SCREEN
*=======================================================================
PARAMETERS: P_FILEIN  LIKE FILENAME-FILEEXTERN DEFAULT
           '/usr/sap/interfaces/D30/IFAP001/LANDINVC.chk' OBLIGATORY,
P_TCODE  LIKE TSTC-TCODE
         DEFAULT 'FB01' OBLIGATORY,
P_GRUPID LIKE APQI-GROUPID
         DEFAULT 'ZAP_LANDINVC' OBLIGATORY,
P_BLART  LIKE BKPF-BLART
         DEFAULT 'K3' OBLIGATORY,
P_ZTERM  LIKE BSEG-ZTERM
         DEFAULT 'N00' OBLIGATORY,
P_LAND1  LIKE BSEG-LANDL
         DEFAULT 'CA' OBLIGATORY.

*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================
AT SELECTION-SCREEN.
  PERFORM CHECK_FILE.

*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.

  PERFORM CREATE_BATCH_INPUT.


*-----------------------------------------------------------------------
*     FORM CHECK_FILE
*-----------------------------------------------------------------------
*  - Routine to check logical file and convert it to physical file.
*  It then attempts to open the physical file to determine if there
*  are any errors reading it.
*-----------------------------------------------------------------------
FORM CHECK_FILE.

  DATA: MSG(100).

*Open File

  OPEN DATASET P_FILEIN FOR INPUT IN TEXT MODE MESSAGE MSG.
  IF ( SY-SUBRC <> 0 ).
    MESSAGE E002 WITH INFILE MSG.
  ENDIF.

ENDFORM.


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

* Do until the end of file.
  DO.
    READ DATASET P_FILEIN INTO INREC.

*   If file is empty, stop run.
    IF ( SY-INDEX = 1 ).
      IF ( SY-SUBRC = 4 ).
        MESSAGE I028.
        STOP.
      ENDIF.
    ENDIF.

*   When end of file, Exit.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.

*   Open batch session, if not open.
    IF OPEN_SESSION = 'X'.
      PERFORM OPEN_BATCH_SESSION.
      MOVE ' ' TO OPEN_SESSION.
    ENDIF.

    CASE INREC(1).
*     document header record....
      WHEN '1'.
        CLEAR OLD_NEWBS.
*       process rest of previous document (if not first one)
        IF ( FIRST_DOC = SPACE ).
          PERFORM FINISH_TRANSACTION.
        ENDIF.
        REFRESH BDCDATA.
        INREC1 = INREC.
        CLEAR:  TEXTTAB, COUNT3.
        REFRESH TEXTTAB.
        PERFORM ADD_TEXT_RECORDS USING INREC1-LGTXT.
        PERFORM START_NEW_TRANSACTION.
        CLEAR: FIRST_DOC.
*     Document line item record....
      WHEN '2'.
        INREC2 = INREC.

        PERFORM START_NEXT_ITEM USING
               INREC2-NEWBS INREC2-NEWKO OLD_NEWBS.

        PERFORM ADD_VENDOR_DESC.
        PERFORM ADD_VENDOR_LINE_ITEM.
        OLD_NEWBS = INREC2-NEWBS.

      WHEN '3'.
        INREC3 = INREC.
        ADD 1 TO COUNT3.
        PERFORM START_NEXT_ITEM USING
               INREC3-NEWBS INREC3-NEWKO OLD_NEWBS.
        PERFORM ADD_TEXT_RECORDS USING INREC3-SGTXT.
        PERFORM ADD_GL_LINE_ITEM.
        OLD_NEWBS = INREC3-NEWBS.

    ENDCASE.
  ENDDO.

  PERFORM FINISH_TRANSACTION.

  CLOSE DATASET INFILE.
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
*                        DD_TEXT_RECORDS
*-----------------------------------------------------------------------
* - This routine puts all the text data to be printed on the stub in
*   TEXTTAB, which is printed at the end of each transaction.
*   INREC3-SGTXT will be parsed into its various words and reformatted
*   in groups of 50 characters or less breaking at full words.
*   There the 200 characters could potentially create 5 lines of
*   cheque text.  LANDAT (as of 2004/05/20) is limiting the cheque text
*   info to 100 characters - 2 lines for certain, 3rd line is possible
*-----------------------------------------------------------------------
FORM ADD_TEXT_RECORDS USING WRK_TEXT.
* deleted 2004/05/20
*  DATA: WLENGTH TYPE I,
*        BFLAG   VALUE 'N'.
*  MOVE 51 TO BPOINT.
*  WLENGTH = STRLEN( WRK_TEXT ).
*  IF WLENGTH > 0.
*     IF WLENGTH > 51.
*        IF WRK_TEXT+BPOINT(1) <> SPACE.
*           WHILE BFLAG = 'N'.
*              BPOINT = BPOINT - 1.
*              IF WRK_TEXT+BPOINT(1) = SPACE.
*                 MOVE 'Y' TO BFLAG.
*              ENDIF.
*              IF BPOINT < 20.
*                 MOVE 'Y' TO BFLAG.
*              ENDIF.
*           ENDWHILE.
*        ENDIF.
*        MOVE WRK_TEXT+0(BPOINT) TO TEXTTAB-STR.
*        APPEND TEXTTAB.
*        CLEAR  TEXTTAB.
*        BPOINT = BPOINT + 1.
*        MOVE WRK_TEXT+BPOINT    TO TEXTTAB-STR.
*        APPEND TEXTTAB.
*        CLEAR TEXTTAB.
*     ELSE.
*        MOVE WRK_TEXT+0(BPOINT) TO TEXTTAB-STR.
*        APPEND TEXTTAB.
*        CLEAR TEXTTAB.
*     ENDIF.
*  ENDIF.
*-----------------------------------------------------------------------
* This is the replacement code as of May 20,2004.  mdemeest
*-----------------------------------------------------------------------
data:  word_1(30) type c.
data:  begin of word_table occurs 100,
           words(30) type c,
       end of word_table.
data:  tmplng        type i,
       wordlng       type i,
       totlng        type i,
       tmp_text(50)  type c.

  split wrk_text at ' ' into table word_table.

  clear tmp_text.
  loop at word_table.
* calculate length of string as each word is added
     tmplng  = strlen( tmp_text ).
     wordlng = strlen( word_table-words ).
     totlng  = tmplng + wordlng.
* if at the max., write string to TEXTTAB and start a new string
     if  totlng > 49.
         move tmp_text  to texttab.
         append texttab.
         clear tmp_text.
     endif.
* eliminates the leading space before the first word of first comment
     if tmplng = 0.
        concatenate tmp_text word_table-words into tmp_text.
     else.
        concatenate tmp_text word_table-words into tmp_text
                               separated by ' '.
     endif.
     endloop.

     move tmp_text to texttab.
     append texttab.

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
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' ZBLDAT.
  PERFORM BDC_FIELD  USING 'BKPF-BLART' P_BLART.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' INREC1-BUKRS.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' ZBLDAT.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' INREC1-WAERS.
  PERFORM BDC_FIELD  USING 'BKPF-XBLNR' INREC1-XBLNR.
  PERFORM BDC_FIELD  USING 'BKPF-BKTXT' INREC1-BKTXT.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM FINISH_TRANSACTION
*-----------------------------------------------------------------------
* - This routine finishes the BDC mapping for the transaction.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FORM FINISH_TRANSACTION.

* select F14 - Document overview
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/14'.
  PERFORM POPUP_SCREEN.
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
FORM START_NEXT_ITEM USING F_NEWBS F_NEWKO O_NEWBS.

  PERFORM BDC_FIELD  USING 'RF05A-NEWBS' F_NEWBS.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' F_NEWKO.
  IF COUNT3 > 1.
     PERFORM POPUP_SCREEN.
  ENDIF.

ENDFORM.

*--------------------------------------------------------------------

FORM POPUP_SCREEN.

  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.

* 23/05/02 CHANGES
*  IF FLENGTH = 5.
*     PERFORM BDC_FIELD  USING 'COBL-KOSTL' INREC3-PS_POSID.
*  ELSEIF FLENGTH = 6.
*     PERFORM BDC_FIELD  USING 'COBL-AUFNR' INREC3-PS_POSID.
*  ELSE.
*     PERFORM BDC_FIELD  USING 'COBL-PS_POSID' INREC3-PS_POSID.
*  ENDIF.
  IF FLENGTH = 5.
     PERFORM BDC_FIELD  USING 'COBL-KOSTL' WSPOSID.
  ELSEIF FLENGTH = 6.
     PERFORM BDC_FIELD  USING 'COBL-AUFNR' WSPOSID.
  ELSE.
     PERFORM BDC_FIELD  USING 'COBL-PS_POSID' WSPOSID.
  ENDIF.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.

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
  MOVE INREC1-BLDAT+4(4) TO INREC2-NAME2+31(4).
  PERFORM BDC_SCREEN USING 'SAPLFCPD' '100'.
  PERFORM BDC_FIELD  USING 'BSEC-NAME1' INREC2-NAME1.
  PERFORM BDC_FIELD  USING 'BSEC-NAME2' INREC2-NAME2.
  PERFORM BDC_FIELD  USING 'BSEC-STRAS' INREC2-STRAS.
  PERFORM BDC_FIELD  USING 'BSEC-ORT01' INREC2-ORT01.
*  PERFORM BDC_FIELD  USING 'BSEC-LAND1' P_LAND1.
  PERFORM BDC_FIELD  USING 'BSEC-LAND1' INREC2-LAND1.
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
  PERFORM BDC_FIELD  USING 'BKPF-XMWST' 'X'.
  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ' INREC2-MWSKZ.
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
*  IF BPOINT > 50.                       "removed 2004/05/20 mdemeest
     MOVE 49 TO BPOINT.
*  ENDIF.                                "removed 2004/05/20 mdemeest
  FLENGTH = STRLEN( INREC3-PS_POSID ).
  MOVE INREC3-PS_POSID TO WSPOSID.         "23/05/02 CHANGES
* Screen: Create G/L account item
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' INREC3-WRBTR.
  PERFORM BDC_FIELD  USING 'BSEG-SGTXT' INREC3-SGTXT+0(BPOINT).

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

*-----------------------------------------------------------------------
*     FORM ADD_TEXT
*-----------------------------------------------------------------------
FORM ADD_TEXT.

* local variables
  DATA: COUNTER(3) TYPE N,
        TAGNO(3) TYPE N,
        OFF TYPE I,
        TEXT_RECS TYPE I,
        LINETEXT(72).

* screen field names of SAPscript editor and tag lines.
  DATA: EDITOR_LINE(17) VALUE 'RSTXT-TXLINE(   )',
        TAG(21) VALUE 'RSTXT-TXPARGRAPH(   )'.
  CLEAR TEXT_RECS.
  DESCRIBE TABLE TEXTTAB LINES TEXT_RECS.
  CHECK TEXT_RECS > 0.
  ADD +1 TO TEXT_RECS.
  COUNTER = 2.

* select Extras->texts from overview screen
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'TEXT'.

* select 3rd line from pop-up window "Texts in Accounting Document"
* and then F2 for detailed text (this text is allocated for the
* "Cheque Stub Message")
  PERFORM BDC_SCREEN USING 'SAPLFTXT' '100'.
  perform bdc_field using 'RTEXT-SELKZ(3)' 'X'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/2'.
**************************** Mohammad's code ************************88
*  PERFORM BDC_SCREEN USING 'SAPLSTXX' '1100'.
*  TAG+17(3) = '002'.
*  PERFORM BDC_FIELD  USING TAG '* '.
**  CLEAR TEXT_RECS.

** add the text in the SAPScript editor 70 characters at a time
*  LOOP AT TEXTTAB.
**      COUNTER = COUNTER + 1.
*     CLEAR: LINETEXT.
*      EDITOR_LINE+13(3) = COUNTER.
**     add extra space to beginning of text line.
*      LINETEXT+1(70) = TEXTTAB-STR.
*      PERFORM BDC_FIELD  USING EDITOR_LINE LINETEXT.
** only create new line if more text -------------
*      IF TEXT_RECS > COUNTER.
*        COUNTER = COUNTER + 1.
*        TAG+17(3) = COUNTER.
*        PERFORM BDC_FIELD USING TAG '/ '.
*      ENDIF.
*  ENDLOOP.
***********************  end of mohammad's code ************************
data: texttab_ctr type i.
  loop at texttab.
    texttab_ctr = texttab_ctr + 1.
    if  counter = 2.           "Beginning of each stub text page
        perform bdc_screen using 'SAPLSTXX' '1100'.
        tag+17(3) = '002'.
        perform BDC_FIELD using TAG '*'.
    endif.

    clear:  linetext.
    editor_line+13(3) = counter.
    linetext+1(70) = texttab-str.
    perform bdc_field using EDITOR_LINE linetext.
    counter = counter + 1.
    tag+17(003) = counter.
    perform bdc_field using tag         '/'.
    if  texttab_ctr > text_recs.
        perform BDC_FIELD using 'BDC_OKCODE' '    '.
        exit.
    else.
        if counter > 20.
           perform BDC_FIELD using 'BDC_OKCODE' 'POSF'.
           counter = 2.
*        else.
*           counter = counter + 1.
        endif.
    endif.
  endloop.



  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'TXEX'.

* select F13 to continue from pop-up window
  PERFORM BDC_SCREEN USING 'SAPLFTXT' '100'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'BACK'.
* returns to overview screen
  PERFORM BDC_SCREEN USING 'SAPMF05A' '700'.

ENDFORM.
