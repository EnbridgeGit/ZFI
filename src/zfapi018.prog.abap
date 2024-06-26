REPORT ZFAPI018 LINE-SIZE 255 MESSAGE-ID ZS.
*----------------------------------------------------------------------
*Author: Mohammad Khan
*Date  : January, 2001.
*Description:
*        This program retrieves checks information printed against the
*       invoices generated by Property tax system.
*
*CHANGES:
*  mdemeest 2008/07/24 Additional selection for BSAK for uniqueness
*  mdemeest 2008/05/28 Fixed selection for first cheques of new company
*                      code >= rather than > (2 spots)
*  mdemeest 2003/06/27 Added company code (BUKRS) to select for BKPF
*                      to get the correct document.  The same document
*                      numbers are used within different company codes.
*  mdemeest 2003/04/08 Changed from BSEG to BSAK to speed up processing
*  mdemeest 2003/04/04 Changed maximum date of zaldt to pridt.  Also
*                      added in a date selection criteria.
*  mokhan 2001/12/05 If the posting is done in the current year and
*                    cheque date is the next fiscal year then PAYR table
*                    has curent year as fiscal year but BSEG has next
*                    year as fiscal year. Find "FISCALEND" to see the
*                    changes.
*----------------------------------------------------------------------
TABLES: BKPF,                "Accounting Document Header
        PAYR,                "Payment Medium File
*        APQI,                "Queue info definition      "05/18 changes
*        BSEG.                "Accounting Document Segment
        BSAK.

*DATA:P_OUTFILE(70) TYPE C.

DATA: BEGIN OF OUTAB OCCURS 0,
      CHECT    LIKE PAYR-CHECT,
      VBLNR    LIKE PAYR-VBLNR,
      XBLNR    LIKE BKPF-XBLNR,
      ZALDT    LIKE PAYR-ZALDT,
      BKTXT    LIKE BKPF-BKTXT,
      RWBTR(14)     TYPE C.
DATA: END OF OUTAB.

DATA: S_BELNR LIKE BSEG-BELNR.
DATA: S_BUKRS LIKE BSEG-BUKRS.
DATA: W_YEAR  LIKE PAYR-GJAHR.                          "FISCALEND
DATA: TYPE LIKE RLGRAP-FILETYPE VALUE 'ASC'.


SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
      PARAMETERS:  P_OUTFIL    LIKE FILENAME-FILEEXTERN DEFAULT
                          '/usr/sap/interfaces/D30/IFAP018/CHEQUES.TXT',
                   P_DATE LIKE PAYR-pridT.                   "2003/04/04
SELECTION-SCREEN END OF BLOCK BOX.

*----------------------------------------------------------------------
* THIS IS THE MAIN BODY OF LOGIC.
*----------------------------------------------------------------------
 DATA: MSG(100),                           "open file - system message
       S_DATE LIKE APQI-CREDATE.

 OPEN DATASET P_OUTFIL FOR OUTPUT IN TEXT MODE MESSAGE MSG.
    IF SY-SUBRC NE '0'.
      MESSAGE E002 WITH P_OUTFIL MSG.
      STOP.
    ELSE.

*   SELECT MAX( CREDATE ) FROM APQI INTO S_DATE     "05/18 changes
*          WHERE GROUPID = 'ZAP_PROP_TAX'.          "05/18 changes
*        IF SY-SUBRC <> 0.                          "05/18 changes
*           MESSAGE E019   WITH 'NO SESSION FOUND'. "05/18 changes
*           STOP.                                   "05/18 changes
*        ENDIF.                                     "05/18 changes

IF P_DATE <> '00000000'.
   MOVE P_DATE TO S_DATE.
ELSE.
   SELECT MAX( pridt ) FROM PAYR INTO S_DATE        "2003/04/04 changes
          WHERE LIFNR = 'OTPTAX'.
endif.                                              "2003/04/04 changes

   SELECT * FROM PAYR
*        WHERE ZALDT => S_DATE                      "05/18 changes
        WHERE pridT = S_DATE
             AND LIFNR = 'OTPTAX'
             AND VBLNR >= '2000000000'.      "Don't pick manual entries
          IF SY-SUBRC = 0.
             PERFORM BUILD_TABLE.
          ENDIF.
   ENDSELECT.


**    SORT OUTAB BY BKTXT.

  LOOP AT OUTAB.
    TRANSFER OUTAB TO P_OUTFIL.
*    WRITE: /1 OUTAB.              "FOR TESTING ONLY
  ENDLOOP.

   CLOSE DATASET P_OUTFIL.
ENDIF.

*&---------------------------------------------------------------------*
*&      Form  BUILD_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_TABLE.
  clear outab.
  W_YEAR      = PAYR-GJAHR.                               "FISCALEND

 OUTAB-CHECT = PAYR-CHECT.
 OUTAB-VBLNR = PAYR-VBLNR.
 OUTAB-ZALDT = PAYR-ZALDT.
* OUTAB-RWBTR = PAYR-RWBTR.  "Summarized at cheque level, require detail

 PERFORM GET_BKPFDATA.
* APPEND OUTAB.
ENDFORM.                    " BUILD_TABLE

*&---------------------------------------------------------------------*
*&
*&      Form  GET_BKPFDATA
*&---------------------------------------------------------------------*

FORM GET_BKPFDATA.

  select * from bsak
      where lifnr = payr-lifnr
        and umsks = ' '
        and umskz = ' '
        and augbl = payr-VBLNR
        and belnr >= '6000000000'
        and gjahr = payr-gjahr
        and bukrs = payr-zbukr.          "2008/07/24

        move bsak-wrbtr to outab-rwbtr.
        if bsak-shkzg = 'H'.
           move '-' to outab-rwbtr+13(1).
        endif.

        select single * from bkpf
            where bukrs = bsak-bukrs
              and belnr = bsak-belnr
              and gjahr = bsak-gjahr.
            if sy-subrc = '0'.
               OUTAB-XBLNR = BKPF-XBLNR.
               OUTAB-BKTXT = BKPF-BKTXT.

            else.
               outab-xblnr = '**BKPF NOT FOUND'.
               outab-bktxt = '**BKPF NOT FOUND'.
            endif.
      append outab.
   endselect.




*  CLEAR: S_BELNR, S_BUKRS.                          "FISCALEND
*  PERFORM GET_BSEG_DOCUMENT.
*  IF S_BELNR IS INITIAL.                            "FISCALEND
*     W_YEAR = W_YEAR + 1.                           "FISCALEND
*     PERFORM GET_BSEG_DOCUMENT.                     "FISCALEND
*     IF S_BELNR IS INITIAL.                         "FISCALEND
*        OUTAB-XBLNR = '**BSEG NOT FOUND'.           "FISCALEND
*        OUTAB-BKTXT = '**BSEG NOT FOUND'.           "FISCALEND
*     ENDIF.                                         "FISCALEND
*  ENDIF.                                            "FISCALEND


*  SELECT * FROM BKPF
*       WHERE BELNR = S_BELNR   AND
*             BUKRS = S_BUKRS   AND
*             GJAHR = W_YEAR.                        "FISCALEND
*             GJAHR = PAYR-GJAHR.                   "FISCALEND

*       IF SY-SUBRC = 0.
*          OUTAB-XBLNR = BKPF-XBLNR.
*          OUTAB-BKTXT = BKPF-BKTXT.
*       ELSE.
*          OUTAB-XBLNR = '**NOT FOUND'.
*          OUTAB-BKTXT = '**NOT FOUND'.
*       ENDIF.
*       EXIT.
*  ENDSELECT.

ENDFORM.                    " GET_BKPFDATA

*&---------------------------------------------------------------------*
*&      Form  GET_BSEG_DOCUMENT
*&---------------------------------------------------------------------*
FORM GET_BSEG_DOCUMENT.

*   SELECT * FROM BSEG
*    WHERE   BUKRS = PAYR-ZBUKR
*      AND   BELNR > '6000000000'
*      AND   GJAHR = PAYR-GJAHR                           "FISCALEND
*      AND   GJAHR = W_YEAR                                "FISCALEND
*      AND   AUGBL = PAYR-VBLNR.

*       IF  SY-SUBRC = 0.
*           IF BSEG-AUGBL <> BSEG-BELNR.
*              MOVE BSEG-BELNR TO S_BELNR.
*              MOVE BSEG-BUKRS TO S_BUKRS.
*           ENDIF.
*       ENDIF.
*   ENDSELECT.


ENDFORM.                    " GET_BSEG_DOCUMENT
