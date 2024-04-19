*&---------------------------------------------------------------------*
*& Report  ZFAPI050_AP_DISBURSEMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapi050_ap_disbursement.

TABLES: bsak,
        reguh.
TYPES: BEGIN OF ty_output,
        bukrs(5),
        d1(1),
        belnr(10),
        d14(1),
        augbl(10),
        d15(1),
        augdt(10),
        d2(1),
        chect(13),
        d3(1),
*        KIDNO(30),
        blart(2),
        d4(1),
        rzawe(5),
        d5(1),
        waers(5),
        d6(1),
        rebzg(10),
        d7(1),
        budat(10),
        d8(1),
        lifnr(10),
        d9(1),
        empfb(10),
        d10(1),
        pycur(5),
        d11(1),
        pyamt(13),
        d12(1),
        zaldt(10),
        d13(1),
        payr_waers(10),
        d16(1),
        SHKZG(5),
        d17(1),
        hkont(10),
      END OF ty_output.

DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output,
      gt_bsak TYPE TABLE OF bsak,
      gs_bsak TYPE bsak,
      gt_reguh TYPE TABLE OF reguh,
      gs_reguh TYPE reguh,
      gt_payr TYPE TABLE OF payr,
      gs_payr TYPE payr,
      gt_bkpf TYPE TABLE OF bkpf,
      gt_regup type table of regup,
      gs_regup type regup,
      gt_bsak_open type table of bsak,
      gs_bsak_open type bsak,
      gs_bkpf type bkpf.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_gjahr TYPE bsak-gjahr OBLIGATORY.
SELECT-OPTIONS: s_bukrs FOR bsak-bukrs OBLIGATORY,
                s_blart FOR bsak-blart OBLIGATORY,
                s_laufi FOR reguh-laufi NO-DISPLAY.
PARAMETERS: p_sdat TYPE bsak-augdt DEFAULT sy-datum,
            p_edat TYPE bsak-augdt DEFAULT sy-datum,
            p_file LIKE filenameci-fileextern
            DEFAULT '/usr/sap/interfaces/P01/IFFI063/EAST_DISBURSEMENT.txt'
            OBLIGATORY.
*             NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON BLOCK b1.
  IF p_sdat > p_edat.
    MESSAGE i000(zfi01) WITH 'From-Date should be <= To-Date' .
    STOP.
  ENDIF.

INITIALIZATION.
  s_blart-sign = 'I'.
  s_blart-option = 'EQ'.
  s_blart-low = 'ZE'.
  APPEND s_blart.
  s_blart-low = 'ZF'.
  APPEND s_blart.
  s_blart-low = 'ZP'.
  APPEND s_blart.
  s_blart-low = 'ZU'.
  APPEND s_blart.
  s_blart-low = 'ZV'.
  APPEND s_blart.

START-OF-SELECTION.

  CLEAR: gt_bsak,
         gt_reguh,
         gt_payr,
         gt_output,
         gt_bkpf,
         gt_regup,
         gt_bsak_open.
*Exclude BCM data
  CLEAR: s_laufi.
  s_laufi-sign = 'E'.
  s_laufi-option = 'BT'.
  s_laufi-low = '00001B'.
  s_laufi-high = '99999B'.
  APPEND s_laufi.

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

  DATA: lv_tabix TYPE sy-tabix,
        ls_bkpf TYPE bkpf,
        lt_payr type table of payr,
        lv_gjahr type payr-gjahr,
        lt_reguh TYPE TABLE OF reguh,
        lv_bukrs type bsak-bukrs,
        lv_augbl type bsak-augbl.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 5
      text       = 'Extracting Documents Data '.

*Pull only clearing documents
  SELECT * INTO TABLE gt_bsak FROM bsak
      WHERE bukrs IN s_bukrs
        AND gjahr = p_gjahr
        AND ( augdt >= p_sdat AND augdt <= p_edat )
        AND blart IN s_blart.
  IF gt_bsak[] IS INITIAL.
    WRITE : / ' No data to process....'.
    STOP.
  ENDIF.
  SELECT * INTO TABLE gt_bkpf FROM bkpf
    FOR ALL ENTRIES IN gt_bsak
    WHERE bukrs = gt_bsak-bukrs
      AND belnr = gt_bsak-belnr
      AND gjahr = gt_bsak-gjahr.

     clear: gt_reguh,
            gt_payr.
      LOOP AT gt_bsak into gs_bsak.
           clear: gs_bkpf.
           READ TABLE gt_bkpf into gs_bkpf
                with key bukrs = gs_bsak-bukrs
                         belnr = gs_bsak-belnr
                         gjahr = gs_bsak-gjahr.
          IF gs_bkpf-BVORG is not INITIAL. "Cross Company
              lv_bukrs = gs_bkpf-bvorg+10(4). "paying company code
              lv_augbl = gs_bkpf-bvorg(10).
           else.
              lv_bukrs = gs_bsak-bukrs.
              lv_augbl = gs_bsak-augbl.
           ENDIF.
           select * APPENDING table gt_reguh from reguh
             where "LAUFD = gs_bsak-budat
               zbukr = lv_bukrs
               and lifnr = gs_bsak-lifnr
               and vblnr = lv_augbl "gs_bsak-augbl
               and zaldt = gs_bsak-budat
               and XVORL = space
               and LAUFI  in s_laufi.

****************************************************
              select * APPENDING table gt_payr from payr
                  WHERE zbukr = lv_bukrs
                    AND lifnr = gs_bsak-lifnr
                    AND vblnr = lv_augbl "gs_bsak-augbl
                    AND gjahr = gs_bsak-gjahr.
      ENDLOOP.

*FILE Header
  gs_output-d1   = '|'.
  gs_output-d2   = '|'.
  gs_output-d3   = '|'.
  gs_output-d4   = '|'.
  gs_output-d5   = '|'.
  gs_output-d6   = '|'.
  gs_output-d7   = '|'.
  gs_output-d8   = '|'.
  gs_output-d9   = '|'.
  gs_output-d10  = '|'.
  gs_output-d11  = '|'.
  gs_output-d12  = '|'.
  gs_output-d13  = '|'.
  gs_output-d14  = '|'.
  gs_output-d15  = '|'.
  gs_output-d16  = '|'.
  gs_output-d17  = '|'.
  gs_output-bukrs = 'BUKRS'.
  gs_output-augdt = 'AUGDT'.
  gs_output-chect = 'CHECT'.
*   gs_output-KIDNO = 'KIDNO'.
  gs_output-blart = 'BLART'.
  gs_output-belnr = 'BELNR'.
  gs_output-augbl = 'AUGBL'.
  gs_output-rzawe = 'RZAWE'.
  gs_output-waers = 'WAERS'.
  gs_output-rebzg = 'REBZG'.
  gs_output-budat = 'BUDAT'.
  gs_output-lifnr = 'LIFNR'.
  gs_output-empfb = 'EMPFB'.
  gs_output-pycur = 'PYCUR'.
  gs_output-pyamt = 'DMBTR'.
  gs_output-zaldt = 'ZALDT'.
  gs_output-payr_waers = 'PAYR_WAERS'.
  gs_output-SHKZG = 'SHKZG'.
  gs_output-hkont = 'HKONT'.
  APPEND gs_output TO gt_output.
*********************
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Processing Data '.

  SORT gt_bsak  BY bukrs lifnr augbl budat.
  SORT gt_reguh BY zbukr lifnr vblnr zaldt.
  SORT gt_payr  BY zbukr lifnr vblnr gjahr.

  LOOP AT gt_bsak INTO gs_bsak.
    CLEAR: ls_bkpf,
           gs_reguh,
           gs_output,
           gs_bkpf.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
                               IN gs_bsak-lifnr WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               IN gs_bsak-lifnr WITH space.
    REPLACE ALL OCCURRENCES OF '#'
                               IN gs_bsak-lifnr WITH space.
    READ TABLE gt_bkpf INTO ls_bkpf WITH KEY bukrs = gs_bsak-bukrs
                                             belnr = gs_bsak-belnr
                                             gjahr = gs_bsak-gjahr.
    gs_output-d1   = '|'.
    gs_output-d2   = '|'.
    gs_output-d3   = '|'.
    gs_output-d4   = '|'.
    gs_output-d5   = '|'.
    gs_output-d6   = '|'.
    gs_output-d7   = '|'.
    gs_output-d8   = '|'.
    gs_output-d9   = '|'.
    gs_output-d10  = '|'.
    gs_output-d11  = '|'.
    gs_output-d12  = '|'.
    gs_output-d13  = '|'.
    gs_output-d14  = '|'.
    gs_output-d15  = '|'.
    gs_output-d16  = '|'.
    gs_output-d17  = '|'.
    gs_output-bukrs = gs_bsak-bukrs.
    gs_output-augdt = gs_bsak-augdt.
*         gs_output-KIDNO = gs_bsak-KIDNO.
    gs_output-rebzg = gs_bsak-rebzg.
    gs_output-budat = ls_bkpf-budat.
    gs_output-lifnr = gs_bsak-lifnr.
    gs_output-empfb = gs_bsak-empfb.
    gs_output-pycur = gs_bsak-pycur.
    gs_output-pyamt = gs_bsak-dmbtr.
    gs_output-blart = gs_bsak-blart.
    gs_output-belnr = gs_bsak-belnr.
    gs_output-augbl = gs_bsak-augbl.
    gs_output-SHKZG = gs_bsak-shkzg.
    gs_output-hkont = gs_bsak-HKONT.
    READ TABLE gt_bkpf into gs_bkpf
                with key bukrs = gs_bsak-bukrs
                         belnr = gs_bsak-belnr
                         gjahr = gs_bsak-gjahr.
    IF gs_bkpf-BVORG is not INITIAL. "Cross Company
              lv_bukrs = gs_bkpf-bvorg+10(4). "paying company code
              lv_augbl = gs_bkpf-bvorg(10).
    else.
              lv_bukrs = gs_bsak-bukrs.
              lv_augbl = gs_bsak-augbl.
    ENDIF.

    READ TABLE gt_reguh WITH KEY
                        zbukr = lv_bukrs
                        lifnr = gs_bsak-lifnr
                        vblnr = lv_augbl "gs_bsak-augbl
                        zaldt = gs_bsak-budat
                        TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
       LOOP AT gt_reguh INTO gs_reguh FROM lv_tabix.
           IF  gs_reguh-zbukr <> lv_bukrs OR
               gs_reguh-lifnr <> gs_bsak-lifnr OR
               gs_reguh-vblnr <> lv_augbl or "gs_bsak-augbl or
               gs_reguh-zaldt <> gs_bsak-budat.
               EXIT.
           ENDIF.
        gs_output-rzawe = gs_reguh-rzawe.
        gs_output-waers = gs_reguh-waers.

        READ TABLE gt_payr WITH KEY
                           zbukr = gs_reguh-zbukr
                           lifnr = gs_reguh-lifnr
                           vblnr = gs_reguh-vblnr
                           gjahr = gs_bsak-gjahr
                           TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lv_tabix = sy-tabix.
          LOOP AT gt_payr INTO gs_payr FROM lv_tabix.
            IF gs_payr-zbukr <> gs_reguh-zbukr OR
               gs_payr-lifnr <> gs_reguh-lifnr OR
               gs_payr-vblnr <> gs_reguh-vblnr or
               gs_payr-gjahr <> gs_bsak-gjahr.
              EXIT.
            ENDIF.
            gs_output-zaldt = gs_payr-zaldt.
            gs_output-payr_waers = gs_payr-waers.
            IF gs_reguh-rzawe CO 'COP'.
              gs_output-chect = gs_payr-chect.
            ENDIF.
            APPEND gs_output TO gt_output.
          ENDLOOP.
        ELSE.
          APPEND gs_output TO gt_output.
        ENDIF.
      ENDLOOP.
***************************************
    ELSE.
      APPEND gs_output TO gt_output.
    ENDIF.
  ENDLOOP.
*  SORT gt_output BY bukrs lifnr.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       Download file
*----------------------------------------------------------------------*
FORM download_data .
  DATA: recsize TYPE i VALUE 180.

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

ENDFORM.                    " DOWNLOAD_DAT
