REPORT zfapi024 LINE-SIZE 100 MESSAGE-ID zs.
*----------------------------------------------------------------------
*Author: Mohammad Khan
*Date  : March, 2002.
*Description:
*        This program retrieves checks information printed against the
*       invoices generated by Lands system.
*
*CHANGES: By:mokhan, Date:2002/12/23, Issue log: Not Available
*         Make this program more flexable for date of data extraction
*         through variants (optionally). Also change the sequence of
*         where clause items according to their sequence in table BKPF.
*
*----------------------------------------------------------------------
TABLES: bkpf,                "Accounting Document Header
        payr,                "Payment Medium File
        bseg.                "Accounting Document Segment


DATA: BEGIN OF outab OCCURS 0,
      chect    LIKE payr-chect,
      vblnr    LIKE payr-vblnr,
      xblnr    LIKE bkpf-xblnr,
      zaldt    LIKE payr-zaldt,
      bktxt    LIKE bkpf-bktxt,
      rwbtr(14)     TYPE c.
DATA: END OF outab.

DATA: s_belnr LIKE bseg-belnr.
DATA: s_bukrs LIKE bseg-bukrs.
DATA: w_year  LIKE payr-gjahr.
DATA: type LIKE rlgrap-filetype VALUE 'ASC'.


SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME.
PARAMETERS:  p_outfil    LIKE filename-fileextern DEFAULT
 '/usr/sap/interfaces/D30/IFAP001/LANDCHEQ.TXT',
             p_date LIKE payr-zaldt.       "Changes 2002/12/23
SELECTION-SCREEN END OF BLOCK box.

*----------------------------------------------------------------------
* THIS IS THE MAIN BODY OF LOGIC.
*----------------------------------------------------------------------
DATA: msg(100),                           "open file - system message
      s_date LIKE apqi-credate.
DATA: lv_tabix TYPE sy-tabix,
      lt_payr TYPE TABLE OF payr,
      lt_bse_clr TYPE TABLE OF bse_clr,
      lt_bkpf TYPE TABLE OF bkpf,
      ls_payr TYPE payr,
      ls_bse  TYPE bse_clr,
      ls_bkpf TYPE bkpf.

clear:  outab.
REFRESH: outab.

OPEN DATASET p_outfil FOR OUTPUT IN TEXT MODE MESSAGE msg.
IF sy-subrc NE '0'.
  MESSAGE e002 WITH p_outfil msg.
  STOP.
ELSE.
  IF p_date <> '00000000'.                         "Changes 2002/12/23
    MOVE p_date TO s_date.                        "Changes 2002/12/23
  ELSE.                                            "Changes 2002/12/23
    SELECT MAX( zaldt ) FROM payr INTO s_date
           WHERE lifnr = 'OTLAND'.
  ENDIF.                                           "Changes 2002/12/23
*  SELECT * FROM payr
*       WHERE zaldt = s_date
*            AND lifnr = 'OTLAND'.
*    IF sy-subrc = 0.
*      PERFORM build_table.
*    ENDIF.
*  ENDSELECT.
  "-----------------SDP85439
  CLEAR: lt_payr,
         lt_bse_clr,
         lt_bkpf.
  SELECT * FROM payr INTO TABLE lt_payr
      WHERE lifnr = 'OTLAND'
        AND zaldt = s_date.
  IF lt_payr[] IS NOT INITIAL.
    SELECT * FROM bse_clr INTO TABLE lt_bse_clr
      FOR ALL ENTRIES IN lt_payr
      WHERE bukrs_clr = lt_payr-zbukr
        AND belnr_clr = lt_payr-vblnr
        AND gjahr_clr = lt_payr-gjahr.
    IF lt_bse_clr[] IS NOT INITIAL.
      SELECT * FROM bkpf INTO TABLE lt_bkpf
       FOR ALL ENTRIES IN lt_bse_clr
               WHERE bukrs = lt_bse_clr-bukrs
         AND belnr = lt_bse_clr-belnr
         AND gjahr = lt_bse_clr-gjahr.
    ENDIF.
  ENDIF.
  SORT lt_payr BY zbukr vblnr gjahr.
  SORT lt_bse_clr BY bukrs_clr belnr_clr gjahr_clr.
  SORT lt_bkpf BY bukrs belnr gjahr.
  LOOP AT lt_payr INTO ls_payr.
    CLEAR lv_tabix.
    outab-chect = ls_payr-chect.
    outab-vblnr = ls_payr-vblnr.
    outab-zaldt = ls_payr-zaldt.
*    outab-rwbtr = ls_payr-rwbtr.
    READ TABLE lt_bse_clr WITH KEY bukrs_clr = ls_payr-zbukr
                                   belnr_clr = ls_payr-vblnr
                                   gjahr_clr = ls_payr-gjahr
                                   TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
      LOOP AT lt_bse_clr INTO ls_bse FROM lv_tabix.
        IF ls_bse-bukrs_clr <> ls_payr-zbukr OR
           ls_bse-belnr_clr <> ls_payr-vblnr OR
           ls_bse-gjahr_clr <> ls_payr-gjahr.
           EXIT.
        ENDIF.
        outab-rwbtr = ls_bse-wrbtr.
        CLEAR ls_bkpf.
        READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = ls_bse-bukrs
                                                 belnr = ls_bse-belnr
                                                 gjahr = ls_bse-gjahr.
        IF sy-subrc = 0.
          outab-xblnr = ls_bkpf-xblnr.
          outab-bktxt = ls_bkpf-bktxt.
        ELSE.
          outab-xblnr = '**NOT FOUND'.
          outab-bktxt = '**NOT FOUND'.
        ENDIF.
        APPEND outab.
      ENDLOOP.
    ELSE.
      outab-xblnr = '**BSE_CLR NOT FOUND'.
      outab-bktxt = '**BSE_CLR NOT FOUND'.
      APPEND outab.
    ENDIF.
  ENDLOOP.
  "-----------------End SDP85439
  LOOP AT outab.
    TRANSFER outab TO p_outfil.
    WRITE: /1 outab.              "FOR TESTING ONLY
  ENDLOOP.

  CLOSE DATASET p_outfil.
ENDIF.

**&---------------------------------------------------------------------*
**&      Form  BUILD_TABLE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM build_table.
*
*  w_year      = payr-gjahr.
*  PERFORM get_bkpfdata.
*  outab-chect = payr-chect.
*  outab-vblnr = payr-vblnr.
*  outab-zaldt = payr-zaldt.
*  outab-rwbtr = payr-rwbtr.
*
*  APPEND outab.
*ENDFORM.                    " BUILD_TABLE
*
**&---------------------------------------------------------------------*
**&
**&      Form  GET_BKPFDATA
**&---------------------------------------------------------------------*
*
*FORM get_bkpfdata.
*
*  CLEAR: s_belnr, s_bukrs.
*  PERFORM get_bseg_document.
*  IF s_belnr IS INITIAL.
*    w_year = w_year + 1.
*    PERFORM get_bseg_document.
*    IF s_belnr IS INITIAL.
*      outab-xblnr = '**BSEG NOT FOUND'.
*      outab-bktxt = '**BSEG NOT FOUND'.
*    ENDIF.
*  ENDIF.
*
*
*  SELECT * FROM bkpf
*       WHERE bukrs = s_bukrs   AND      "Changes 2002/12/23
*             belnr = s_belnr   AND      "Changes 2002/12/23
*             gjahr = w_year.
*
*    IF sy-subrc = 0.
*      outab-xblnr = bkpf-xblnr.
*      outab-bktxt = bkpf-bktxt.
*    ELSE.
*      outab-xblnr = '**NOT FOUND'.
*      outab-bktxt = '**NOT FOUND'.
*    ENDIF.
*    EXIT.
*  ENDSELECT.
*
*ENDFORM.                    " GET_BKPFDATA
*
**&---------------------------------------------------------------------*
**&      Form  GET_BSEG_DOCUMENT
**&---------------------------------------------------------------------*
*FORM get_bseg_document.
*
*  SELECT * FROM bseg
*   WHERE   bukrs = payr-zbukr
*     AND   belnr > '6000000000'
*     AND   gjahr = w_year
*     AND   augbl = payr-vblnr.
*
*    IF  sy-subrc = 0.
*      IF bseg-augbl <> bseg-belnr.
*        MOVE bseg-belnr TO s_belnr.
*        MOVE bseg-bukrs TO s_bukrs.
*      ENDIF.
*    ENDIF.
*  ENDSELECT.
*
*
*ENDFORM.                    " GET_BSEG_DOCUMENT
