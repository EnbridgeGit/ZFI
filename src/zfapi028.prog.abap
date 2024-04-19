REPORT ZFAPI028 MESSAGE-ID ZS.
************************************************************************
*  Author:    M L De Meester.
*
*  Date:      May 2003.
*
*  Issue Log:
*
*  Description:
*     - The purpose of this program is to input an Excel spreadsheet and
*       create a file that can be used as input into ZFAPI029 which
*       creates the BDC session.
*
*       The client receives an excel spreadsheet & saves it to the
*       appropriate directory as "apup.csv".  This directory is
*       entered in ZFAPI028 variant in the "INPUT FILE" name
*
*       The first line of the excel spreadsheet contain header
*       info and are ignored by ZFAPI028 ( sy-tabix < 2)
*
*-----------------------------------------------------------------------
* CHANGES
* 2014/08/08 M.Khan  Tkt22498 Add Payee and Payment Method.
* 2007/06/11 mdemeest TR418 - Vendor numbers for Employees need to ber
*                             treated differently
* 2004/01/07 mdemeest #--- Changed number of title lines from 4 to 1
*                          Data starts on row 2.
* 2003/05/06 mdemeest 1017 Created abap.
* 2020/06/30 AKMADASU D30K930623 CHG0186403 - REGIO field update

************************************************************************
* NOTE:  Differences between EAST & WEST - to find all the differences
*        do a find on DIFFERENCES
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
*                EAST              |           WEST
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
* include structure ZBSEG          | include structure BBSEG
* zbseg transfer is 639 bytes      | bbseg transfer is 2373
* n/a                              | NPLNR & VORNR are used
* - - - - - - - - - - - - - - - - -|- - - - - - - - - - - - - - - - - -
* form COMMON_MOVES_FOR_ZBSEG - MOVE BBSEG ....
*
************************************************************************
tables:  DD03L,
         lfa1,                                   "Vendor Master
         lfb1.                                   "Vendor Company

data: inrec(400).
data: OUTREC(2000).

data:  begin of wa         occurs 0,
         bldat(8)          type c,               "Invoice Date
         bukrs             like bkpf-bukrs,      "Company Code
         waers             like bkpf-waers,      "Currency
         xblnr             like bkpf-xblnr,      "Document #
         bktxt             like bkpf-bktxt,      "Header Text
         bschl             like bseg-bschl,      "Posting Key
         lifnr             like lfa1-lifnr,      "Vendor
         empfb             like bseg-empfb,      "Payee Tkt22498
         newbk             like bkpf-bukrs,      "Other Company code
         wrbtr(12)         type c,               "Vendor Amount
         zlsch             LIKE bseg-zlsch,      "Payment method Tkt22498
         sgtxt             like bseg-sgtxt,      "Detail text
         saknr             like bseg-saknr,      "G/L
         kostl             like bseg-kostl,      "Cost Centre
         aufnr             like bseg-aufnr,      "I/O
         nplnr             like bseg-nplnr,      "Network
         vornr             like bbseg-vornr,     "Activity
         projk(24)         type c,               "WBS
         rest(10)          type c,               "Rest of record
       end of wa.
*-----------------------------------------------------------------------
field-symbols: <F1>.
data:    char(21)    type c,
         nodata(1)   value '/'.
data: wa_lifnr like lfa1-lifnr.
*-----------------------------------------------------------------------
* Input file name with path
data: infile(70).
* Output file name with path
DATA: OUTFILE(70).

*-----------------------------------------------------------------------
* Dates Data
DATA:   TBLDAT TYPE D,
        TBUDAT TYPE D,
        ZBLDAT(10),                        "date in user format
        ZBUDAT(10),
        FLENGTH TYPE I,
        COUNT3(2) TYPE N,
        WSPOSID LIKE COBL-PS_POSID.        "23/05/02 CHANGES

data:  begin of Z_BGR00.
       include structure bgr00.
data:  end of Z_BGR00.

data:  begin of Z_BBKPF.
       include structure bbkpf.
data:  end of Z_BBKPF.

data:  begin of Z_ZBSEG.
       include structure zbseg.                             "DIFFERENCES
data   EMPFB TYPE EMPFB.            "Payee Ticket22498
data:  end of Z_ZBSEG.
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
    p_filein like filename-fileextern         obligatory
      default '/usr/sap/interfaces/P01/FAPUPLOAD/apup.ftp',
    P_fileot  like filename-fileextern        obligatory
      default '/usr/sap/interfaces/P01/FAPUPLOAD/ZFAPI028.dat',
    P_TCODE  LIKE TSTC-TCODE   default 'F-43' obligatory,
    P_BLART  LIKE BKPF-BLART   default 'KN'   obligatory,
    P_mwskz  like bseg-mwskz   default 'I0'   obligatory,
    P_LAND1  LIKE BSEG-LANDL   default 'CA'   obligatory,
    p_gst    like bseg-saknr                  obligatory.


*=======================================================================
* SELECTION SCREEN PROCESSING
*=======================================================================
INITIALIZATION.
  move sy-sysid to p_fileot+20(3).
  move sy-sysid to p_filein+20(3).

AT SELECTION-SCREEN.
  PERFORM OPEN_FILES.

*=======================================================================
*     Start of Main Processing Block
*=======================================================================
START-OF-SELECTION.
  perform INPUT_FILE_TO_WA.
  PERFORM CREATE_OUTPUT_FILE.
*------------------------  INPUT_FILE_TO_WA  ---------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields.
*-----------------------------------------------------------------------
form INPUT_FILE_TO_WA.
   do.
    read dataset p_filein into inrec.
    if inrec(1) = ' '.
       exit.
    elseif sy-index < 2.
*                      ignore record - first 4 lines are column headers
    elseif  inrec(7) = ',,,,,,,' .
    elseif  inrec(1)  = 'I'.
    else.
      split inrec at ',' into wa-bldat   wa-bukrs   wa-waers
                              wa-xblnr   wa-bktxt   wa-bschl
                              wa-lifnr   wa-empfb   wa-newbk "Ticket22498
                              wa-wrbtr   wa-zlsch   wa-sgtxt "Ticket22498
                              wa-saknr   wa-kostl   wa-aufnr
                              wa-nplnr   wa-vornr   wa-projk
                              wa-rest.
      if wa-lifnr ca 'E' or wa-lifnr ca 'H'.               "Employee vendors have an 'E'
      else.
         shift wa-lifnr right deleting trailing space.
         translate wa-lifnr using ' 0'.
      endif.
      append wa.
    endif.
  enddo.
endform.

*==========================  OPEN_FILE =================================
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*=======================================================================
FORM OPEN_FILES.

  DATA: MSG(100).
*-----------------------------------------------------------------------
  open dataset p_filein for input in text mode message msg.
  if ( sy-subrc <> 0 ).
    message E002 with infile msg.
  endif.
*-----------------------------------------------------------------------
  OPEN DATASET P_FILEOT FOR OUTPUT IN TEXT MODE MESSAGE MSG.
  if ( sy-subrc <> 0 ).
    message E002 with outFILE msg.
  endif.

ENDFORM.



*===========================  CREATE_OUTPUT_FILE =======================
*  This routine loops thru the spreadsheet data and creates a standard
*  file to be input into ZFAPI029
*=======================================================================

form create_output_file.
data:  first_document(1)  type c  value 'X'.

  loop at wa.
    case wa-bldat(1).
    when ' '.
      perform INIT_STRUCTURES USING 'ZBSEG'.            "East is ZBSEG
      perform common_moves_for_ZBSEG.
      move wa-saknr             to z_zbseg-hkont.
      if wa-saknr(6) = p_gst+4(6).
         clear z_zbseg-mwskz.
      else.
      move p_mwskz           to z_zbseg-mwskz.
      endif.
      transfer Z_ZBSEG to P_FILEOT length 664.              "DIFFERENCES
    when others.
      if first_document = 'X'.
         PERFORM INIT_STRUCTURES USING 'BGR00'.
         move '0'                  to z_bgr00-stype.
         case p_tcode.
           when 'F-43'.
             concatenate 'ZAP' wa-xblnr(9) into z_bgr00-group.
           when 'F-63'.
             concatenate 'ZAP_PK' wa-xblnr(6) into z_bgr00-group.
           endcase.
         move sy-mandt             to z_bgr00-mandt.           "client
         move sy-uname             to z_bgr00-usnam.
         transfer Z_BGR00 to P_FILEOT length 38.
         move ' ' to first_document.
      endif.

      perform init_structures using 'BBKPF'.
      move '1'                  to z_bbkpf-stype.
      move p_tcode              to z_bbkpf-tcode.
      move sy-datum             to z_bbkpf-budat.
      move p_blart              to z_bbkpf-blart.
      move wa-bukrs             to z_bbkpf-bukrs.
      move wa-bldat             to z_bbkpf-bldat.
      move sy-datum+4(2)        to z_bbkpf-monat.
      move wa-waers             to z_bbkpf-waers.
      move wa-xblnr             to z_bbkpf-xblnr.
      move wa-bktxt             to z_bbkpf-bktxt.
      transfer Z_BBKPF to P_FILEOT length 232.

      perform init_structures using 'ZBSEG'.
      perform common_moves_for_ZBSEG.
      if wa-saknr(6) = p_gst+4(6).
      else.
         move p_mwskz           to z_zbseg-mwskz.
      endif.
      perform vendor_info.
      transfer Z_ZBSEG to p_fileot length 694.  "Ticket22498 "DIFFERENCES
*      transfer Z_ZBSEG to p_fileot length 664.              "DIFFERENCES
.
    endcase.
  endloop.
endform.

*============================== VENDOR_INFO  ===========================
*  This routine moves the vendor info to the standard BBSEG layout
*=======================================================================
form vendor_info.
  select single * from lfa1
    where lifnr = wa-lifnr.
  if sy-subrc = '0'.
     move wa-lifnr            to Z_zbseg-hkont.
     move lfa1-name1          to z_zbseg-name1.
     move lfa1-name2          to z_zbseg-name2.
     move lfa1-stras          to z_zbseg-stras.
     move lfa1-ort01          to z_zbseg-ort01.
     move lfa1-pstlz          to z_zbseg-pstlz.
**--start of changes by akmadasu CHG0186403 D30K930623
     move lfa1-regio          to z_zbseg-regio.
**-- end of changes by akmadasu CHG0186403
  select single * from lfb1
     where lifnr = wa-lifnr
       and bukrs = wa-bukrs.
  if sy-subrc = '0'.
     move lfb1-zterm          to z_zbseg-zterm.
  endif.
  endif.
endform.

*========================== COMMON_MOVES_FOR ZBSEG =====================
*  This routine moves fields into BBSEG layout
*=======================================================================

form common_moves_for_ZBSEG.
      move '2'                  to z_zbseg-stype.
      move 'ZBSEG'              to z_zbseg-tbnam.
      move wa-bschl             to z_zbseg-newbs.
      move wa-wrbtr             to z_zbseg-wrbtr.
      move wa-empfb             to z_zbseg-empfb.  "Ticket 22498
      move wa-zlsch             to z_zbseg-zlsch.  "Ticket 22498
      move wa-kostl             to z_zbseg-kostl.
      move wa-aufnr             to z_zbseg-aufnr.
      move wa-sgtxt             to z_zbseg-sgtxt.
      move wa-projk             to z_zbseg-projk.
*     move wa-nplnr             to z_zbseg-nplnr.           "DIFFERENCES
*     move wa-vornr             to z_zbseg-vornr.           "DIFFERENCES
      move wa-newbk             to z_zbseg-newbk.
endform.

*======================  INIT_STRUCTURES  =============================
*  Used to initialize the record to '/'
*======================================================================
form init_structures using tabname.
  select * from DD03L where tabname = tabname.
    clear char.
    char(2) = 'Z_'.
    char+2(5) = tabname.
    char+7(1) = '-'.
    char+8    = DD03L-FIELDNAME.
    assign (char) to <F1>.
    <F1> = nodata.
  endselect.
endform.
