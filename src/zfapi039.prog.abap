REPORT zfapi039 MESSAGE-ID zs.
************************************************************************
*  Programmer: M DeMeester
*  This abap uses standard file layouts to create a parked document
*  invoice BDC session.

*  Copied from ZFAPI014 - ZFAPI014 was developed in 4.6C and the
*  vendors pass a 4.6C file layout.  For the future, vendors should
*  pass all required data and an abap will reformat it into the
*  layout required by ZFAPI039.
*
*  Transaction  - F-63
************************************************************************
* NOTE:  Differences between EAST & WEST - to find all the differences
*        do a find on DIFFERENCES
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
*                EAST              |           WEST
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
* include structure ZBSEG          | include structure BBSEG
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
************************************************************************
* ---------------------- CHANGE LOG ------------------------------------
* 2007/05/24 mdemeet TR418 Copied ZFAPI014 as a base and removed all the
*                          reformatting.
* 2011/11/14 gymana TR928 - Added some tax code logic for parked
*                           documents
* 2012/04/27 gymana TR928 - Adding code to populate missing 'New Co.Cd'
* 2014/05/07 M khan Tkt22498 - Payment Run Automation.
*                             There are new fields Payment method & Payer
*                             in the input file, make changes accordingly.
************************************************************************

DATA: inrec(1700).                             "input record

*data: inrec_old(1700).                         "4.6B

DATA: BEGIN OF zbgr00.                         "input - session header
        INCLUDE STRUCTURE bgr00.
DATA: END OF zbgr00.

DATA: BEGIN OF zbbkpf.                      "input - document header
        INCLUDE STRUCTURE bbkpf.
DATA: END OF zbbkpf.

DATA: BEGIN OF zbbseg.                      "input - document line it
        INCLUDE STRUCTURE zbseg.                            "DIFFERENCES
DATA   EMPFB TYPE EMPFB.                  ""Payee Tkt22498
DATA: END OF zbbseg.

DATA: BEGIN OF bdcdata OCCURS 0.          "batch input data
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: g_tcode LIKE tstc-tcode.              "transaction code for BDC
DATA: bdcname LIKE zbgr00-group.            "name of batch input session
DATA: amount(10).
DATA: db_cr(1)        TYPE c.
DATA: scrn_300_flg(1) TYPE c.
DATA: sgtxt           LIKE  zbseg-sgtxt.
DATA: first_doc       TYPE c.
DATA: tran_cnt(5)     TYPE p.
DATA: wrk_symbolic(4) TYPE c VALUE '$sys'.
DATA: old_newbk       LIKE zbseg-newbk.


*-----------------  SELECTION SCREEN -----------------------------------
PARAMETER:  infile LIKE filename-fileextern
              DEFAULT '/usr/sap/interfaces/$sys/FAPUPLOAD/park.sap'.

*---------------------- INITIALIZATION ---------------------------------
INITIALIZATION.
  REPLACE wrk_symbolic WITH sy-sysid INTO: infile.
  CONDENSE: infile NO-GAPS.


*------------------ AT SELECTION-SCREEN --------------------------------
AT SELECTION-SCREEN.

  PERFORM check_file.

*------------------ START-OF-SELECTION ---------------------------------
START-OF-SELECTION.
  PERFORM create_batch_input.

*-------------------------- CHECK_FILE ---------------------------------
FORM check_file.

  DATA:  msg(100)  TYPE c.

  OPEN DATASET infile FOR INPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH infile msg.
  ENDIF.

ENDFORM.                    "CHECK_FILE


*-----------------------  CREATE_BATCH_INPUT ---------------------------
*     FORM CREATE_BATCH_INPUT
*-----------------------------------------------------------------------
* Reads each record from the input file and creates batch input data
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

 DATA: msg(100).                             "open file - system message

  REFRESH bdcdata.

  "do until entire file processed

  DO.
    READ DATASET infile INTO inrec.
    CASE sy-subrc.
      WHEN '4'.
        PERFORM end_of_file_processing.
        EXIT.
    ENDCASE.

    CASE inrec(1).

      WHEN '0'.                           "session header record
        zbgr00 = inrec.
        MOVE space TO zbgr00-start.       "transaction wants it blank

        IF sy-mandt+1(1) = '5'.           "==> production client
          IF zbgr00-mandt <> sy-mandt.
            WRITE: / 'Client Number Error'.
            MESSAGE e029 WITH zbgr00-mandt sy-mandt.
            STOP.
          ENDIF.
        ENDIF.
        PERFORM open_batch_session.
        first_doc = 'X'.
        db_cr = 'X'.

      WHEN '1'.                           "document header record
        IF first_doc = 'X'.
          MOVE  ' ' TO first_doc.
        ELSE.
*       perform screen_002.
          PERFORM bdc_screen USING 'SAPLF040' '300'.
          PERFORM bdc_field USING 'BDC_OKCODE' 'BP  '.
          PERFORM bdc_screen USING 'SAPLKACB'  '0002'.
          PERFORM bdc_field USING 'BDC_OKCODE' 'ENTE'.

          PERFORM insert_session.
          db_cr = 'X'.
          scrn_300_flg = ' '.
        ENDIF.

        REFRESH bdcdata.

        zbbkpf = inrec.
        g_tcode = zbbkpf-tcode.

        PERFORM screen_0100.
*   clear: first_doc, ficount.
*   document line item record....

      WHEN '2'.                          "document line item record
        zbbseg = inrec.
        If ( zbbseg-newbs = '21' ) or ( zbbseg-newbs = '31' ).
            clear old_newbk.
        else.
            old_newbk = zbbseg-newbk.
        endif.
        IF db_cr = 'X'.
*         next posting key on scrn 100
          PERFORM screen_posting_key USING old_newbk.
          PERFORM screen_302.
          CLEAR db_cr.
        ELSE.
          IF scrn_300_flg = 'X'.
*       perform screen_002.
            PERFORM bdc_screen USING 'SAPLF040' '0300'.
          ENDIF.
          PERFORM screen_posting_key USING old_newbk.
          IF scrn_300_flg = 'X'.
            PERFORM bdc_screen USING 'SAPLKACB'   '0002'.
            PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.
          ENDIF.
          PERFORM screen_0300.
          PERFORM screen_002.
*      perform BDC_SCREEN USING 'SAPLF040' '0300'.
          MOVE 'X' TO scrn_300_flg.
        ENDIF.
    ENDCASE.
  ENDDO.

ENDFORM.                    "CREATE_BATCH_INPUT

*&---------------------------------------------------------------------*
*&      Form  end_of_file_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM end_of_file_processing.

*    perform screen_002.
  PERFORM bdc_screen USING 'SAPLF040' '300'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'BP  '.
  PERFORM bdc_screen USING 'SAPLKACB'  '0002'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'ENTE'.

  PERFORM insert_session.          "Insert last record into batch
  PERFORM close_session.           "File totally processed
  CLOSE DATASET infile.
*    STOP.

ENDFORM.                    "end_of_file_processing

*------------------------  OPEN_BATCH_SESSION  -------------------------
* - This routine just simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM open_batch_session.

* if XKEEP flag is not set to 'X' or ' ' - set it to an 'X'
  IF ( zbgr00-xkeep <> 'X' ) OR ( zbgr00-xkeep <> space ).
    zbgr00-xkeep = 'X'.
  ENDIF.
* if BATCH user wasn't passed in file header record, use logon user
  IF  zbgr00-usnam <> 'BATCH'.
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


*-------------------------  INSERT_SESSION  ----------------------------
* Inserts the BDC data for one transaction into the batch input session.
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


*---------------------------  CLOSE_SESSION  ---------------------------
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


*-----------------------  SCREEN_0100  ---------------------------------
* Povides the BDC mapping for the initial screen in the transaction
*-----------------------------------------------------------------------
FORM screen_0100.

  DATA: tbldat TYPE d,
        tbudat TYPE d,
        zbldat(10),                 "date in user format
        zbudat(10).

  tbldat = zbbkpf-bldat.           "convert dates to user default format
  tbudat = zbbkpf-budat.
  WRITE tbldat TO zbldat DD/MM/YYYY.
  WRITE tbudat TO zbudat DD/MM/YYYY.

  PERFORM bdc_screen USING 'SAPLF040' '100'.
  PERFORM bdc_field  USING 'BKPF-BLDAT' zbldat.
  PERFORM bdc_field  USING 'BKPF-BLART' zbbkpf-blart.
  PERFORM bdc_field  USING 'BKPF-BUKRS' zbbkpf-bukrs.
  PERFORM bdc_field  USING 'BKPF-BUDAT' zbudat.
  PERFORM bdc_field  USING 'BKPF-WAERS' zbbkpf-waers.
  PERFORM bdc_field  USING 'BKPF-XBLNR' zbbkpf-xblnr.
  PERFORM bdc_field  USING 'BKPF-BKTXT' zbbkpf-bktxt.

ENDFORM.                    "SCREEN_0100

*----------------------  SCREEN_POSTING_KEY_0100  ----------------------
* Posting account info resides on ZBSEG record
*-----------------------------------------------------------------------
FORM screen_posting_key USING o_newbk.
  PERFORM bdc_field  USING 'RF05V-NEWBS' zbbseg-newbs.
  PERFORM bdc_field  USING 'RF05V-NEWKO' zbbseg-hkont.           "Vendor
* Do not post this field for screen 100.
  IF db_cr <> 'X'.
     PERFORM bdc_field  USING 'RF05V-NEWBK' o_newbk.             "TR928
  ENDIF.

ENDFORM.                    "SCREEN_POSTING_KEY

*&---------------------------------------------------------------------*
*&      Form  SCREEN_302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_302.
  amount = zbbseg-wrbtr.
  PERFORM bdc_screen USING 'SAPLF040' '302'.
  PERFORM bdc_field  USING 'BSEG-WRBTR' amount.
* Turn on Calculate Tax checkbox                                  "TR928
  PERFORM bdc_field  USING 'BKPF-XMWST' zbbkpf-xmwst.             "TR928
  PERFORM bdc_field  USING 'BSEG-MWSKZ' zbbseg-mwskz.             "TR928
  PERFORM bdc_field  USING 'BSEG-ZTERM' zbbseg-zterm.             "TR928
  PERFORM bdc_field  USING 'BSEG-ZFBDT' zbbseg-zfbdt.             "TR928
  PERFORM bdc_field  USING 'BSEG-ZLSCH' zbbseg-zlsch.          "Tkt22498
  PERFORM bdc_field  USING 'BSEG-EMPFB' zbbseg-empfb.          "Tkt22498
  PERFORM bdc_field  USING 'BSEG-UZAWE' zbbseg-uzawe.             "TR928
  PERFORM bdc_field  USING 'BSEG-SGTXT' zbbseg-sgtxt.             "TR928
  PERFORM bdc_field  USING 'BDC_OKCODE' '=ZK'.                    "TR928

  PERFORM bdc_screen USING 'SAPLF040' '0332'.                     "TR928
  PERFORM bdc_field  USING 'BSEG-XREF3' zbbseg-xref3.             "TR928
  PERFORM bdc_field  USING 'BDC_OKCODE' '=SB'.                    "TR928

ENDFORM.                                                    "SCREEN_302

*&---------------------------------------------------------------------*
*&      Form  SCREEN_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_0300.

  PERFORM bdc_screen USING 'SAPLF040'  '0300'.
  amount = zbbseg-wrbtr.
  PERFORM bdc_field  USING 'BSEG-WRBTR' amount.
  PERFORM bdc_field  USING 'BSEG-MWSKZ' zbbseg-mwskz.
  PERFORM bdc_field  USING 'BSEG-SGTXT' zbbseg-sgtxt.             "TR928
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '=BP'.                   "TR928
*  PERFORM BDC_FIELD  USING 'BSEG-ZFBDT' ' '. "ZBBSEG-ZFBDT.

ENDFORM.                    "SCREEN_0300
*------------------  SCREEN_002  ---------------------------------------
* Coding block - pop-up window appears - select F8 to continue
*-----------------------------------------------------------------------
FORM screen_002.
*       PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'BP  '.
  PERFORM bdc_screen USING 'SAPLKACB' '002'.
  IF zbbseg-aufnr <> space.
    PERFORM bdc_field  USING 'COBL-AUFNR' zbbseg-aufnr."ZBBSEG-AUFNR.
  ENDIF.
  IF zbbseg-projk <> space.
    PERFORM bdc_field  USING 'COBL-PS_PSP_PNR' zbbseg-projk.
  ENDIF.
  IF zbbseg-kostl <> space.
    PERFORM bdc_field  USING 'COBL-KOSTL'      zbbseg-kostl.
  ENDIF.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.
ENDFORM.                                                    "SCREEN_002


*---------------------------  BDC_SCREEN -------------------------------
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

*-----------------------------  BDC_FIELD-------------------------------
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

*  DATA: tval.
*
*  tval = fval.
  IF FNAM <> 'BDC_OKCODE'.
     CHECK FVAL <> '/' AND FVAL <> SPACE.
  ENDIF.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD
