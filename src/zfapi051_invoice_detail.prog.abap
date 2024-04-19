*&---------------------------------------------------------------------*
*& Report  ZFAPI051_INVOICE_DETAIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapi051_invoice_detail.
TABLES: cdhdr,
        bsik,
        bkpf.
TYPES: BEGIN OF ty_output,
       bukrs(5),
       d1(1),
       belnr(10),
       d12(1),
       augbl(10),
       d16(1),
       hkont(10),
       d14(1),
       lifnr(10),
       d10(1),
       shkzg(1),
       d15(1),
       wrbtr(13),
       d2(1),
*       rebzg(10),
       XBLNR(16),
       d3(1),
       zfbdt(10),
       d4(1),
       bldat(10),
       d5(1),
       waers(5),
       d6(1),
       waers1(5),
       d7(1),
       sgtxt(50),
       d8(1),
       dmbtr(13),
       d9(1),
       usnam(12),
       d11(1),
       budat(10),
      END OF ty_output.

DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output,
      gt_bkpf TYPE TABLE OF bkpf,
      gs_bkpf TYPE bkpf,
      gt_bseg TYPE TABLE OF bseg,
      gs_bseg TYPE bseg.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR bkpf-bukrs OBLIGATORY,
                s_blart for bkpf-blart OBLIGATORY.
PARAMETERS: p_sdat TYPE bkpf-budat DEFAULT sy-datum,
            p_edat TYPE bkpf-budat DEFAULT sy-datum,
            p_file LIKE filenameci-fileextern
            DEFAULT '/usr/sap/interfaces/P01/IFFI063/EAST_AP_INVOICEDETAIL.txt'
            OBLIGATORY.
*             NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON BLOCK b1.
  IF p_sdat > p_edat.
    MESSAGE i000(zfi01) WITH 'From-Date should be <= To-Date' .
    STOP.
  ENDIF.

START-OF-SELECTION.

  CLEAR: gt_bkpf,
         gt_bseg,
         gt_output.

  PERFORM get_data.
  IF gt_output[] IS NOT INITIAL.
    PERFORM download_data.
  ELSE.
    WRITE: / 'No data to process'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lv_tabix TYPE sy-tabix.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = 'Extracting BKPF Documents Data '.

  SELECT * INTO TABLE gt_bkpf FROM bkpf
      WHERE bukrs IN s_bukrs
        AND ( budat >= p_sdat AND budat <= p_edat )
        AND blart in s_blart.
  IF gt_bkpf[] IS INITIAL.
    WRITE : / ' No data to process....'.
    STOP.
  ENDIF.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 15
      text       = 'Extracting BSEG Documents Data '.

  SELECT * INTO TABLE gt_bseg FROM bseg
      FOR ALL ENTRIES IN gt_bkpf
      WHERE bukrs = gt_bkpf-bukrs
        AND belnr = gt_bkpf-belnr
        AND gjahr = gt_bkpf-gjahr.
  SORT gt_bkpf BY bukrs gjahr belnr.
  SORT gt_bseg BY bukrs gjahr belnr.

*FILE Header
  gs_output-d1  = '|'.
  gs_output-d2  = '|'.
  gs_output-d3  = '|'.
  gs_output-d4  = '|'.
  gs_output-d5  = '|'.
  gs_output-d6  = '|'.
  gs_output-d7  = '|'.
  gs_output-d8  = '|'.
  gs_output-d9  = '|'.
  gs_output-d10  = '|'.
  gs_output-d11  = '|'.
  gs_output-d12  = '|'.
*  gs_output-d13  = '|'.
  gs_output-d14  = '|'.
  gs_output-d15  = '|'.
  gs_output-d16  = '|'.
  gs_output-bukrs = 'BUKRS'.
  gs_output-wrbtr = 'WRBTR'.
*  gs_output-rebzg = 'REBZG'.
  gs_output-XBLNR = 'XBLNR'.
  gs_output-augbl = 'AUGBL'.
  gs_output-zfbdt = 'ZFBDT'.
  gs_output-bldat = 'BLDAT'.
  gs_output-waers = 'WAERS'.
  gs_output-waers1 = 'WAERS'.
  gs_output-sgtxt = 'SGTXT'.
  gs_output-dmbtr = 'DMBTR'.
  gs_output-lifnr = 'LIFNR'.
  gs_output-usnam = 'USNAM'.
  gs_output-budat = 'BUDAT'.
  gs_output-belnr = 'BELNR'.
  gs_output-lifnr = 'LIFNR'.
  gs_output-hkont = 'HKONT'.
  gs_output-shkzg = 'SHKZG'.
  APPEND gs_output TO gt_output.
*********************
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Processing Data '.
  LOOP AT gt_bkpf INTO gs_bkpf.
    gs_output-d1  = '|'.
    gs_output-d2  = '|'.
    gs_output-d3  = '|'.
    gs_output-d4  = '|'.
    gs_output-d5  = '|'.
    gs_output-d6  = '|'.
    gs_output-d7  = '|'.
    gs_output-d8  = '|'.
    gs_output-d9  = '|'.
    gs_output-d10  = '|'.
    gs_output-d11  = '|'.
    gs_output-d12  = '|'.
*  gs_output-d13  = '|'.
    gs_output-d14  = '|'.
    gs_output-d15  = '|'.
    gs_output-d16  = '|'.
    gs_output-bldat = gs_bkpf-bldat.
    gs_output-waers = gs_bkpf-waers.
    gs_output-waers1 = gs_bkpf-waers.
    gs_output-usnam = gs_bkpf-usnam.
    gs_output-budat = gs_bkpf-budat.
    gs_output-bukrs = gs_bkpf-bukrs.
    gs_output-XBLNR = gs_bkpf-xblnr.
    READ TABLE gt_bseg WITH KEY bukrs = gs_bkpf-bukrs
                                gjahr = gs_bkpf-gjahr
                                belnr = gs_bkpf-belnr
                       TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
      LOOP AT gt_bseg INTO gs_bseg FROM lv_tabix.
        IF gs_bseg-bukrs <> gs_bkpf-bukrs OR
           gs_bseg-belnr <> gs_bkpf-belnr OR
           gs_bseg-gjahr <> gs_bkpf-gjahr.
          EXIT.
        ENDIF.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
                                   IN gs_bseg-lifnr WITH space.
        REPLACE ALL OCCURRENCES OF '|'
                                   IN gs_bseg-lifnr WITH space.
        REPLACE ALL OCCURRENCES OF '#'
                                   IN gs_bseg-lifnr WITH space.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
                                   IN gs_bseg-sgtxt WITH space.
        REPLACE ALL OCCURRENCES OF '|'
                                   IN gs_bseg-sgtxt WITH space.
        REPLACE ALL OCCURRENCES OF '#'
                                   IN gs_bseg-sgtxt WITH space.
        gs_output-augbl = gs_bseg-augbl.
        gs_output-wrbtr = gs_bseg-wrbtr.
*        gs_output-rebzg = gs_bseg-rebzg.
        gs_output-zfbdt = gs_bseg-zfbdt.
        gs_output-sgtxt = gs_bseg-sgtxt.
        gs_output-dmbtr = gs_bseg-dmbtr.
        gs_output-lifnr = gs_bseg-lifnr.
        gs_output-belnr = gs_bseg-belnr.
        gs_output-hkont = gs_bseg-hkont.
        gs_output-shkzg = gs_bseg-shkzg.
        APPEND gs_output TO gt_output.
      ENDLOOP.
    ELSE.
      gs_output-augbl = space.
      gs_output-wrbtr = space.
*      gs_output-rebzg = space.
      gs_output-zfbdt = space.
      gs_output-sgtxt = space.
      gs_output-dmbtr = space.
      gs_output-lifnr = space.
      gs_output-belnr = space.
      gs_output-hkont = space.
      gs_output-shkzg = space.
      APPEND gs_output TO gt_output.
    ENDIF.
  ENDLOOP.

*sort gt_output by bukrs lifnr ebeln.
*sort gt_output by bukrs g.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       Download file
*----------------------------------------------------------------------*
FORM download_data .
  DATA: recsize TYPE i VALUE 100.

  OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE: 'Unable to Open File. Please check text file, security or path'.
    STOP.
  ENDIF.
  LOOP AT gt_output INTO gs_output.
    TRANSFER gs_output TO p_file LENGTH recsize.
  ENDLOOP.
  CLOSE DATASET p_file.
  IF sy-subrc = 0 .
    WRITE: / 'Download process is completed at ', p_file.
  ELSE.
    WRITE: 'Unable to Close File. Please check security or path'.
  ENDIF.

ENDFORM.                    " DOWNLOAD_DATA
