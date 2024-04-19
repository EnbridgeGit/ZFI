REPORT zfapi014 MESSAGE-ID zs.
************************************************************************
*  Programmer: M DeMeester
*
*  This abap creates a BDC session from a flat file to create parked
*  invoices for Bank of Montreal and to be posted into SAP.
*  Transaction  - F-63
*  Logical File - ZFAPI014_01
************************************************************************
*  Special Note - because of file size limitations
*   - Vendor Number(HKONT)  is  MATNR
*   - GL Account   (HKONT)  is  VALUT
*   - WBS Element  (PROJK)  is  MENGE
************************************************************************
* ---------------------- CHANGE LOG ------------------------------------
* 2012/10/02 GYMANA EHP5 Replace Logical File coding with conventional
*                        open dataset logic for local & Unix files.
* 2006/05/19 mdemeet 4.7 New fields added in zbseg - so had to split the
*                        608 bytes to accomodate PERNR.
* 00/08/16 mdemeest 4.6b bbpkf-tcode has expanded from 4 to 20 bytes
*                        manipulate input file to accomodate
* 99/06/07 mdemeest upgrade Comments out a "GO"

* 98/09/15 md7140   #466 Original specifications
************************************************************************
*tables: lfa1,                                    "vendor master
*       t077k.                                    "vendor account groups
DATA: BEGIN OF itab OCCURS 500,                             "EHP5
              irec(1700),                                   "EHP5
      END OF itab.                                          "EHP5
*DATA: inrec(1700).                             "input record
*DATA: inrec_old(1700).                                      "4.6B

DATA: BEGIN OF zbgr00.                         "input - session header
        INCLUDE STRUCTURE bgr00.
DATA: END OF zbgr00.

DATA: BEGIN OF zbbkpf.                      "input - document header
        INCLUDE STRUCTURE bbkpf.
DATA: END OF zbbkpf.

DATA: BEGIN OF zbbseg.                      "input - document line it
        INCLUDE STRUCTURE zbseg.
DATA: END OF zbbseg.

DATA: BEGIN OF bdcdata OCCURS 0.          "batch input data
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: infile(70).                           "physical file path
DATA: g_tcode LIKE tstc-tcode.              "transaction code for BDC
DATA: bdcname LIKE zbgr00-group.            "name of batch input session
DATA: amount(10).
DATA: db_cr(1)        TYPE c.
DATA: sgtxt LIKE  zbseg-sgtxt.
DATA: first_doc       TYPE c.
DATA: tran_cnt(5)     TYPE p.

*-----------------  SELECTION SCREEN -----------------------------------
*PARAMETERS: LGCLFILE LIKE FILENAME-FILEINTERN
*                    DEFAULT 'ZFAPI014_01' OBLIGATORY."logical file name
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.   "EHP5
PARAMETERS: p_local RADIOBUTTON GROUP gr1,                        "EHP5
            p_unix  RADIOBUTTON GROUP gr1 DEFAULT 'X'.            "EHP5
                                                                  "EHP5
SELECTION-SCREEN SKIP.                                            "EHP5
PARAMETERS: p_locfil LIKE filename-fileextern,                    "EHP5
            p_infile LIKE filename-fileextern.                    "EHP5
SELECTION-SCREEN END OF BLOCK box1.                               "EHP5

*----------------------- INITIALIZATION --------------------------------
INITIALIZATION.
  MOVE '/usr/sap/interfaces/P01/FAPPCARD/ZFAPI014.CHK'            "EHP5
       TO p_infile.                                               "EHP5
  MOVE 'H:\' TO p_locfil.                                         "EHP5

*------------------ AT SELECTION-SCREEN --------------------------------
*AT SELECTION-SCREEN.                                             "EHP5
*  PERFORM CHECK_FILE.                                            "EHP5

*------------------ START-OF-SELECTION ---------------------------------
START-OF-SELECTION.
  PERFORM validate_p_infile.                                      "EHP5
  PERFORM create_batch_input.

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

*-------------------------- CHECK_FILE ---------------------------------
*  Converts the logical file to physical file.  Then attempts to open
*  it to determine if there are any errors reading it.
*-----------------------------------------------------------------------
* EHP5 - GYMANA - 2012/10/02 - Disable routine
*-----------------------------------------------------------------------
*FORM check_file.
*
*  DATA: msg(100).                         "open file - system message
*
*  CALL FUNCTION 'FILE_GET_NAME'
*    EXPORTING
*      client           = sy-mandt
*      logical_filename = lgclfile
*      operating_system = sy-opsys
*    IMPORTING
*      file_name        = infile
*    EXCEPTIONS
*      file_not_found   = 01.
*
*  IF ( sy-subrc = 1 ).
*    MESSAGE e001 WITH lgclfile.
*  ENDIF.
*
*  OPEN DATASET infile FOR INPUT IN TEXT MODE MESSAGE msg.
*  IF ( sy-subrc <> 0 ).
*    MESSAGE e002 WITH infile msg.
*  ENDIF.
*
*ENDFORM.                    "CHECK_FILE

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
* READ DATASET INFILE INTO INREC.                                  "4.6B
*  READ DATASET infile INTO inrec_old.                       "4.6B

* DO.                                    "do until entire file processed
*    IF ( sy-index = 1 ).                   "check for empty file
*      IF ( sy-subrc = 4 ).
*        MESSAGE i028.
*        STOP.
*      ENDIF.
*    ENDIF.

 LOOP AT itab.
*  CASE INREC(1).                                                  "4.6B
*   CASE inrec_old(1).                                       "4.6B

    CASE itab-irec(1).
      WHEN '0'.                           "session header record
*    ZBGR00 = INREC.                                              "EHP5
     zbgr00 = itab-irec.                                          "EHP5

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
          PERFORM screen_002.

          PERFORM insert_session.
          db_cr = 'X'.
          CLEAR sgtxt.
        ENDIF.

        REFRESH bdcdata.

*  Transaction Code changed from 4 to 20.                          "4.6B
*    ZBBKPF = INREC.                                               "4.6B
        zbbkpf(5) = itab-irec(5).                                  "4.6B
        zbbkpf+21(211) = itab-irec+5(211).                         "4.6B
        g_tcode = zbbkpf-tcode.
        PERFORM screen_0100.
*     document line item record....
      WHEN '2'.                          "document line item record

*   ZBBSEG = INREC.                                                "4.6B
        CLEAR zbbseg.                                              "4.6B
        zbbseg(11) = itab-irec(11).                                "4.6B
        zbbseg+31(102) = itab-irec+11(102).                         "4.7
*   PERNR notin file from AP and occupies next 8 bytes in 4.7 zbseg "4.7
        zbbseg+141(543) = itab-irec+113(543).                       "4.7


        IF db_cr = 'X'.
          PERFORM screen_posting_key_0100.
          sgtxt = zbbseg-sgtxt.
          PERFORM screen-2302.
          CLEAR db_cr.
        ELSE.
          PERFORM screen_posting_key_2302.
          PERFORM screen_0300.
        ENDIF.
    ENDCASE.
* READ DATASET INFILE INTO INREC.                                  "UGL
*   READ DATASET infile INTO inrec_old.                            "UGL
*    IF sy-subrc = '4'.                 "End of file - finish & close
*      PERFORM screen_002.              "Finish off last rec
*      PERFORM insert_session.          "Insert last record into batch
*      PERFORM close_session.           "File totally processed
*      CLOSE DATASET infile.
*      STOP.
*    ENDIF.
  ENDLOOP.

  PERFORM close_session.

ENDFORM.                    "CREATE_BATCH_INPUT


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
FORM screen_posting_key_0100.
  PERFORM bdc_field  USING 'RF05V-NEWBS' zbbseg-newbs.
  PERFORM bdc_field  USING 'RF05V-NEWKO' zbbseg-matnr.      "Vendor
* perform bdc_field  using 'RF05V-NEWKO' '23871'.                  "TEST
* perform bdc_field      using 'BDC_OKCODE' 'GO  '. "ENTER    1999/06/07
ENDFORM.                    "SCREEN_POSTING_KEY_0100

*----------------------  SCREEN_POSTING_KEY_2302  ----------------------
* Posting account info resides on ZBSEG record
*-----------------------------------------------------------------------
FORM screen_posting_key_2302.
  PERFORM bdc_field  USING 'RF05V-NEWBS' zbbseg-newbs.
  PERFORM bdc_field  USING 'RF05V-NEWKO' zbbseg-valut(6)."GL Account
*  perform bdc_field  using 'RF05V-NEWKO' '420010'.                "TEST
  PERFORM bdc_field      USING 'BDC_OKCODE' 'GO  '.      "ENTER
ENDFORM.                    "SCREEN_POSTING_KEY_2302

*&---------------------------------------------------------------------*
*&      Form  SCREEN-2302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen-2302.
  amount = zbbseg-wrbtr+6(10).
*  PERFORM BDC_SCREEN USING 'SAPLF040' '2302'.                      "UGL
  PERFORM bdc_screen USING 'SAPLF040' '302'.                        "UGL

  PERFORM bdc_field  USING 'BSEG-WRBTR' amount.
  PERFORM bdc_field  USING 'BKPF-XMWST' 'X'.
  PERFORM bdc_field  USING 'BSEG-MWSKZ' zbbseg-mwskz.
  PERFORM bdc_field  USING 'BSEG-SGTXT' sgtxt.
ENDFORM.                    "SCREEN-2302

*&---------------------------------------------------------------------*
*&      Form  SCREEN_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_0300.

  PERFORM bdc_screen USING 'SAPLF040'  '0300'.
  amount = zbbseg-wrbtr+6(10).
  PERFORM bdc_field  USING 'BSEG-WRBTR' amount.
  PERFORM bdc_field  USING 'BSEG-MWSKZ' zbbseg-mwskz.
  PERFORM bdc_field  USING 'BSEG-SGTXT' sgtxt.
  PERFORM bdc_field  USING 'BSEG-ZFBDT' zbbseg-zfbdt.

ENDFORM.                    "SCREEN_0300
*------------------  SCREEN_002  ---------------------------------------
* Coding block - pop-up window appears - select F8 to continue
*-----------------------------------------------------------------------
FORM screen_002.
  PERFORM bdc_field  USING 'BDC_OKCODE' 'BP  '.
  PERFORM bdc_screen USING 'SAPLKACB' '002'.
  PERFORM bdc_field  USING 'COBL-AUFNR'      zbbseg-aufnr.
*      perform bdc_field  using 'COBL-AUFNR'      '      '.        "TEST
  PERFORM bdc_field  USING 'COBL-PS_PSP_PNR' zbbseg-menge.
  PERFORM bdc_field  USING 'COBL-KOSTL'      zbbseg-kostl.
*      perform bdc_field  using 'COBL-KOSTL'      '20200'.         "TEST
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

  DATA: tval.

  tval = fval(1).
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD
