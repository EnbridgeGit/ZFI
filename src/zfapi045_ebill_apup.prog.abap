REPORT zfapi045_ebill_apup MESSAGE-ID zs.
************************************************************************
*  Author:    Brian Boundy.
*  Date:      February 2011.
*
*  Description:
*     - The purpose of this program is to input an Excel spreadsheet and
*       create a file that can be used as input into ZFAPI046 which
*       creates the BDC session.
*
*       The client receives an excel spreadsheet & saves it to the
*       appropriate directory as "apup.csv".  This directory is
*       entered in ZFAPI046 variant in the "INPUT FILE" name
*
*       The first line of the excel spreadsheet contain header
*       info and are ignored by ZFAPI046 ( sy-tabix < 2)
*
*-----------------------------------------------------------------------
* CHANGES
* BTBOUNDY - 2013/03/19 - SDP44048 Allow multiple GST codes

************************************************************************
* NOTE:  Differences between EAST & WEST - to find all the differences
*        do a find on DIFFERENCES
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
*                EAST              |           WEST
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
* bkpf-blart default K3            | bkpf-blart default K2
* include structure ZBSEG          | include structure BBSEG
* n/a                              | NPLNR & VORNR are used
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
* form COMMON_MOVES_FOR_ZBSEG - MOVE BBSEG ....
*
************************************************************************
TABLES:  dd03l,
         lfa1,                                   "Vendor Master
         lfb1,                                   "Vendor Company
         bseg.

DATA: inrec(400).
DATA: outrec(2000).

DATA:  BEGIN OF wa          OCCURS 0,
          bldat(10)         TYPE c,               "Invoice Date
          bukrs             LIKE bkpf-bukrs,      "Company Code
          waers             LIKE bkpf-waers,      "Currency
          xblnr             LIKE bkpf-xblnr,      "Document #
          bktxt             LIKE bkpf-bktxt,      "Header Text
          bschl             LIKE bseg-bschl,      "Posting Key
          lifnr             LIKE lfa1-lifnr,      "Vendor
          newbk             LIKE bkpf-bukrs,      "Other Company code
          wrbtr(16)         TYPE c,               "Vendor Amount
          sgtxt             LIKE bseg-sgtxt,      "Detail text
          saknr             LIKE bseg-saknr,      "G/L
          kostl             LIKE bseg-kostl,      "Cost Centre
          aufnr             LIKE bseg-aufnr,      "I/O
          nplnr             LIKE bseg-nplnr,      "Network
          vornr             LIKE bbseg-vornr,     "Activity
          projk(24)         TYPE c,               "WBS
          zuonr             LIKE bseg-zuonr,      "Allocation ID
          name1             LIKE bsec-name1,      "Vendor Name
          stras             LIKE bsec-stras,      "Vendor Street Address
          ort01             LIKE bsec-ort01,      "Vendor City
          pstlz             LIKE bsec-pstlz,      "Vendor Postal Code
          rest(10)          TYPE c,               "Rest of record
       END OF wa.
*-----------------------------------------------------------------------
FIELD-SYMBOLS: <f1>.
DATA:    char(21)    TYPE c,
         nodata(1)   VALUE '/'.
DATA: wa_lifnr LIKE lfa1-lifnr.
*-----------------------------------------------------------------------
* Input file name with path
DATA: infile(70).
* Output file name with path
DATA: outfile(70).

*-----------------------------------------------------------------------
* Dates Data
DATA:   tbldat TYPE d,
        tbudat TYPE d,
        zbldat(10),                        "date in user format
        zbudat(10),
        flength TYPE i,
        count3(2) TYPE n,
        wsposid LIKE cobl-ps_posid.        "23/05/02 CHANGES

DATA:  BEGIN OF z_bgr00.
        INCLUDE STRUCTURE bgr00.
DATA:  END OF z_bgr00.

DATA:  BEGIN OF z_bbkpf.
        INCLUDE STRUCTURE bbkpf.
DATA:  END OF z_bbkpf.

DATA:  BEGIN OF z_zbseg.
        INCLUDE STRUCTURE zbseg.                             "DIFFERENCES
DATA:  END OF z_zbseg.
*=======================================================================
* SELECTION SCREEN consists of
*  - input & output file names
*  - transaction code (F-43)
*  - transaction type (KN)
*  - tax code (WEST - XX;  EAST - I0)
*  - country code (CA)
*  - GST internal order so that the GST record does not have a tax code
*=======================================================================
PARAMETERS:
    p_filein LIKE filename-fileextern         OBLIGATORY
      DEFAULT '/usr/sap/interfaces/P01/EBILLING/apup.ftp', "DIFFERENCES
    p_fileot  LIKE filename-fileextern        OBLIGATORY
      DEFAULT '/usr/sap/interfaces/P01/EBILLING/ZFAPI045.dat', "DIFFERENCES
    p_tcode  LIKE tstc-tcode   DEFAULT 'F-43' OBLIGATORY,
    p_blart  LIKE bkpf-blart   DEFAULT 'K3'   OBLIGATORY,    "DIFFERENCES
    p_mwskz  LIKE bseg-mwskz   DEFAULT 'I0'   OBLIGATORY,
    p_land1  LIKE bseg-landl   DEFAULT 'CA'   OBLIGATORY.
"p_gst    LIKE bseg-saknr                  OBLIGATORY.

SELECT-OPTIONS:
    s_gst    FOR bseg-saknr OBLIGATORY NO INTERVALS.


*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================
INITIALIZATION.
  MOVE sy-sysid TO p_fileot+20(3).
  MOVE sy-sysid TO p_filein+20(3).


*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.
  PERFORM open_files.
  PERFORM input_file_to_wa.
  PERFORM create_output_file.
*------------------------  INPUT_FILE_TO_WA  ---------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields.
*-----------------------------------------------------------------------
FORM input_file_to_wa.
  DO.
    READ DATASET p_filein INTO inrec.
    IF inrec(1) = ' '.
      EXIT.
    ELSEIF sy-index < 2.
*                      ignore record - first 4 lines are column headers
    ELSEIF  inrec(7) = '|||||||' .
    ELSEIF  inrec(1)  = 'I'.
    ELSE.
      SPLIT inrec AT '|' INTO wa-bldat   wa-bukrs   wa-waers
                              wa-xblnr   wa-bktxt   wa-bschl
                              wa-lifnr   wa-newbk   wa-wrbtr
                              wa-sgtxt   wa-saknr
                              wa-kostl   wa-aufnr
                              wa-nplnr   wa-vornr   wa-projk
                              wa-zuonr   wa-name1   wa-stras
                              wa-ort01   wa-pstlz
                              wa-rest.
      CONCATENATE '0000' wa-saknr INTO wa-saknr.
      IF wa-lifnr CA 'E'.               "Employee vendors have an 'E'
      ELSE.
        SHIFT wa-lifnr RIGHT DELETING TRAILING space.
        TRANSLATE wa-lifnr USING ' 0'.
      ENDIF.
      APPEND wa.
    ENDIF.
  ENDDO.
ENDFORM.                    "INPUT_FILE_TO_WA

*==========================  OPEN_FILE =================================
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*=======================================================================
FORM open_files.

  DATA: msg(100).
*-----------------------------------------------------------------------
  OPEN DATASET p_filein FOR INPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH infile msg.
  ENDIF.
*-----------------------------------------------------------------------
  OPEN DATASET p_fileot FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH outfile msg.
  ENDIF.

ENDFORM.                    "OPEN_FILES



*===========================  CREATE_OUTPUT_FILE =======================
*  This routine loops thru the spreadsheet data and creates a standard
*  file to be input into ZFAPI029
*=======================================================================

FORM create_output_file.
  DATA:  first_document(1)  TYPE c  VALUE 'X'.

  LOOP AT wa.
    CASE wa-bldat(1).
      WHEN ' '.
        PERFORM init_structures USING 'BBSEG'.            "DIFFERENCSE
        PERFORM common_moves_for_zbseg.
        MOVE wa-saknr             TO z_zbseg-hkont.
        IF wa-saknr IN s_gst. "= p_gst+4(6).
          CLEAR z_zbseg-mwskz.
        ELSE.
          MOVE p_mwskz           TO z_zbseg-mwskz.
        ENDIF.
        TRANSFER z_zbseg TO p_fileot LENGTH 2373.
      WHEN OTHERS.
        IF first_document = 'X'.
          PERFORM init_structures USING 'BGR00'.
          MOVE '0'                  TO z_bgr00-stype.
          move 'ZAPELEGAL'          to z_bgr00-group.
          MOVE sy-mandt             TO z_bgr00-mandt.           "client
          MOVE sy-uname             TO z_bgr00-usnam.
          TRANSFER z_bgr00 TO p_fileot LENGTH 38.
          MOVE ' ' TO first_document.
        ENDIF.

        PERFORM init_structures USING 'BBKPF'.
        MOVE '1'                  TO z_bbkpf-stype.
        MOVE p_tcode              TO z_bbkpf-tcode.
        MOVE sy-datum             TO z_bbkpf-budat.
        MOVE p_blart              TO z_bbkpf-blart.
        MOVE wa-bukrs             TO z_bbkpf-bukrs.
        CONCATENATE wa-bldat(4) wa-bldat+5(2) wa-bldat+8(2) INTO z_bbkpf-bldat.
        "move wa-bldat             to z_bbkpf-bldat.
        MOVE sy-datum+4(2)        TO z_bbkpf-monat.
        MOVE wa-waers             TO z_bbkpf-waers.
        MOVE wa-xblnr             TO z_bbkpf-xblnr.
        MOVE wa-bktxt             TO z_bbkpf-bktxt.
        TRANSFER z_bbkpf TO p_fileot LENGTH 232.

        PERFORM init_structures USING 'ZBSEG'. "DIFFERENCES
        PERFORM common_moves_for_zbseg.
        IF wa-saknr IN s_gst. " = p_gst+4(6).
        ELSE.
          MOVE p_mwskz           TO z_zbseg-mwskz.
        ENDIF.
        IF wa-lifnr = '0000OTRISK'. "OTRISK is prepended with 0.
          "CONCATENATE 'ELegal-' wa-zuonr INTO z_zbseg-zuonr.
          move 'OTRISK'          TO z_zbseg-hkont.
          MOVE wa-name1          TO z_zbseg-name1.
          MOVE wa-stras          TO z_zbseg-stras.
          MOVE wa-ort01          TO z_zbseg-ort01.
          MOVE wa-pstlz          TO z_zbseg-pstlz.
        ELSE.
          PERFORM vendor_info.
        ENDIF.
        TRANSFER z_zbseg TO p_fileot LENGTH 2373.
        .
    ENDCASE.
  ENDLOOP.
ENDFORM.                    "create_output_file

*============================== VENDOR_INFO  ===========================
*  This routine moves the vendor info to the standard BBSEG layout
*=======================================================================
FORM vendor_info.
  SELECT SINGLE * FROM lfa1
    WHERE lifnr = wa-lifnr.
  IF sy-subrc = '0'.
    MOVE wa-lifnr            TO z_zbseg-hkont.
    MOVE lfa1-name1          TO z_zbseg-name1.
    MOVE lfa1-name2          TO z_zbseg-name2.
    MOVE lfa1-stras          TO z_zbseg-stras.
    MOVE lfa1-ort01          TO z_zbseg-ort01.
    MOVE lfa1-pstlz          TO z_zbseg-pstlz.

    SELECT SINGLE * FROM lfb1
       WHERE lifnr = wa-lifnr
         AND bukrs = wa-bukrs.
    IF sy-subrc = '0'.
      MOVE lfb1-zterm          TO z_zbseg-zterm.
    ENDIF.
  ENDIF.
ENDFORM.                    "vendor_info

*========================== COMMON_MOVES_FOR ZBSEG =====================
*  This routine moves fields into BBSEG layout
*=======================================================================

FORM common_moves_for_zbseg.
  MOVE '2'                  TO z_zbseg-stype.
  MOVE 'ZBSEG'              TO z_zbseg-tbnam.  "DIFFERENCES
  MOVE wa-bschl             TO z_zbseg-newbs.
  MOVE wa-wrbtr             TO z_zbseg-wrbtr.
  MOVE wa-kostl             TO z_zbseg-kostl.
  MOVE wa-aufnr             TO z_zbseg-aufnr.
  CONCATENATE 'ELegal-' wa-zuonr '-' wa-sgtxt INTO z_zbseg-sgtxt.
  "move wa-sgtxt             to z_zbseg-sgtxt.
  MOVE wa-projk             TO z_zbseg-projk.
*     move wa-nplnr             to z_zbseg-nplnr.           "DIFFERENCES
*     move wa-vornr             to z_zbseg-vornr.           "DIFFERENCES
  MOVE wa-newbk             TO z_zbseg-newbk.
ENDFORM.                    "common_moves_for_ZBSEG

*======================  INIT_STRUCTURES  =============================
*  Used to initialize the record to '/'
*======================================================================
FORM init_structures USING tabname.
  SELECT * FROM dd03l WHERE tabname = tabname.
    CLEAR char.
    char(2) = 'Z_'.
    char+2(5) = tabname.
    if tabname = 'BBSEG'.           "DIFFERENCE
       char+2(5) = 'ZBSEG'.         "
    endif.                          "
    char+7(1) = '-'.
    char+8    = dd03l-fieldname.
    ASSIGN (char) TO <f1>.
    <f1> = nodata.
  ENDSELECT.
ENDFORM.                    "init_structures
