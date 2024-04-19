*&---------------------------------------------------------------------*
*& Report  ZFAPI049_AP_INVOICEDATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFAPI049_AP_INVOICEDATA.

tables: cdhdr,
        bsik.
types: BEGIN OF ty_output,
        BUKRS(5),
        d1(1),
        BELNR(10),
        d7(1),
       	WRBTR(13),
        d2(1),
       	ZFBDT(10),
        d3(1),
       	LIFNR(10),
        d4(1),
       	REBZG(10),
        d5(1),
       	WAERS(5),
        d6(1),
        WAERS1(5),
*         EBELN(10),
*        d7(1),
*         AEDAT(10),
      END OF ty_output.

data: gt_output TYPE TABLE OF ty_output,
      gs_output type ty_output,
      gt_bsik type TABLE OF bsik,
      gs_bsik type bsik,
      gt_ekko type TABLE OF ekko,
      gs_ekko type ekko.

SELECTION-SCREEN BEGIN OF block b1 WITH FRAME TITLE text-001.
  PARAMETERS: P_GJAHR TYPE BSIK-GJAHR OBLIGATORY.
  SELECT-OPTIONS: S_BUKRS FOR BSIK-BUKRS OBLIGATORY.
  PARAMETERS: p_sdat type BSIK-BUDAT DEFAULT sy-datum,
              p_edat type BSIK-BUDAT DEFAULT sy-datum,
              p_file LIKE filenameci-fileextern
              DEFAULT '/usr/sap/interfaces/P01/IFFI063/EAST_AP_INVOICEDATA.txt'
              OBLIGATORY.
*             NO-DISPLAY.
SELECTION-SCREEN end of block b1.

AT SELECTION-SCREEN ON BLOCK b1.
   IF p_sdat > p_edat.
      MESSAGE i000(zfi01) WITH 'From-Date should be <= To-Date' .
      Stop.
   ENDIF.
START-OF-SELECTION.

clear: gt_bsik,
       gt_ekko,
       gt_output.

perform get_data.
if gt_output[] is not initial.
   perform download_data.
else.
   write: / 'No data to process'.
endif.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form GET_DATA .

data: lv_Tabix type sy-tabix.

CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
 EXPORTING
   PERCENTAGE       = 0
   TEXT             = 'Extracting Documents Data '.

  SELECT * into table GT_BSIK from BSIK
      where bukrs in s_bukrs
        and gjahr = p_gjahr
        and ( budat >= p_sdat and budat <= p_edat ).
  IF gt_bsik[] IS INITIAL.
     write : / ' No data to process....'.
     stop.
  ENDIF.
*  SELECT * into table GT_EKKO from EKKO
*      for all entries in gt_bsik
*      where bukrs = gt_bsik-bukrs
*        and ebeln = gt_bsik-ebeln
*        and lifnr = gt_bsik-lifnr.
  sort gt_bsik by bukrs lifnr ebeln.
*  sort gt_ekko by bukrs lifnr ebeln.
*FILE Header
   gs_output-d1  = '|'.
   gs_output-d2  = '|'.
   gs_output-d3  = '|'.
   gs_output-d4  = '|'.
   gs_output-d5  = '|'.
   gs_output-d6  = '|'.
   gs_output-d7  = '|'.
   gs_output-bukrs = 'BUKRS'.
   gs_output-wrbtr = 'WRBTR'.
   gs_output-zfbdt = 'ZFBDT'.
   gs_output-lifnr = 'LIFNR'.
   gs_output-rebzg = 'REBZG'.
   gs_output-waers = 'WAERS'.
   gs_output-waers1 = 'WAERS'.
   gs_output-BELNR = 'BELNR'.
*   gs_output-ebeln = 'EBELN'.
*   gs_output-aedat = 'AEDAT'.
   APPEND gs_output to gt_output.
*********************
   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
   EXPORTING
     PERCENTAGE       = 50
     TEXT             = 'Processing Data '.
  LOOP AT gt_bsik into gs_bsik.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_bsik-lifnr WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_bsik-lifnr WITH space.
    REPLACE ALL OCCURRENCES OF '#'
                               in gs_bsik-lifnr WITH space.
       gs_output-d1  = '|'.
       gs_output-d2  = '|'.
       gs_output-d3  = '|'.
       gs_output-d4  = '|'.
       gs_output-d5  = '|'.
       gs_output-d6  = '|'.
       gs_output-d7  = '|'.
       gs_output-bukrs = gs_bsik-bukrs.
       gs_output-wrbtr = gs_bsik-wrbtr.
       gs_output-zfbdt = gs_bsik-zfbdt.
       gs_output-lifnr = gs_bsik-lifnr.
       gs_output-rebzg = gs_bsik-rebzg.
       gs_output-waers = gs_bsik-waers.
       gs_output-waers1 = gs_bsik-waers.
       gs_output-belnr = gs_bsik-belnr.
*       READ TABLE gt_ekko with key bukrs = gs_bsik-bukrs
*                                   ebeln = gs_bsik-ebeln
*                                   lifnr = gs_bsik-lifnr
*                          TRANSPORTING NO FIELDS.
*       if sy-subrc = 0.
*          lv_tabix = sy-tabix.
*          LOOP AT gt_Ekko INTO gs_ekko FROM lv_tabix.
*               IF gs_ekko-bukrs <> gs_bsik-bukrs or
*                  gs_ekko-ebeln <> gs_bsik-ebeln or
*                  gs_ekko-lifnr <> gs_bsik-lifnr.
*                  exit.
*               ENDIF.
*               gs_output-ebeln = gs_ekko-ebeln.
*               gs_output-aedat = gs_ekko-aedat.
*               APPEND gs_output to gt_output.
*          ENDLOOP.
*       else.
*         gs_output-ebeln = space.
*         gs_output-aedat = space.
         append gs_output to gt_output.
*       endif.
  ENDLOOP.

*sort gt_output by bukrs lifnr ebeln.
sort gt_output by bukrs lifnr.

endform.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       Download file
*----------------------------------------------------------------------*
form DOWNLOAD_DATA .
  data: recsize type i VALUE 100.

OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
IF sy-subrc <> 0.
   WRITE: 'Unable to Open File. Please check text file, security or path'.
   stop.
ENDIF.
LOOP AT gt_output INTO gs_output.
   TRANSFER gs_output TO p_file LENGTH recsize.
ENDLOOP.
CLOSE DATASET p_file.
IF sy-subrc = 0 .
  WRITE: / 'Download process is completed at ', p_file.
else.
  WRITE: 'Unable to Close File. Please check security or path'.
ENDIF.

endform.                    " DOWNLOAD_DATA
