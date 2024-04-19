*&---------------------------------------------------------------------*
*& Report  ZFAPI048_VENDOR_SAS99
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFAPI048_VENDOR_SAS99.

tables: cdhdr.
types: BEGIN OF ty_output,
*       d18(1),
       LIFNR(10),
       d1(1),
       NAME1(35),
       d2(1),
       STRAS(35),
       d3(1),
       PSTLZ(10),
       d4(1),
       PFACH(10),
       d5(1),
       STCD2(11),
       d6(1),
       BUkRS(5),
       d17(1),
       ZTERM(5),
       d7(1),
       BEGRU(4),
       d8(1),
       TELF1(16),
       d9(1),
       ORT01(35),
       d10(1),
       ORT02(35),
       d11(1),
       REGIO(5),
       d12(1),
       udate(10),
       d13(1),
       LNRZA(10),
       d14(1),
       STCD1(16),
       d15(1),
       USERNAME(12),
       d16(1),
       XCPDK(5),
*       d19(1),
       END OF ty_output.

data: gt_lfa1 type table of lfa1,
      gs_lfa1 type lfa1,
      gt_cdhdr type table of cdhdr,
      gs_cdhdr TYPE cdhdr,
      gt_output TYPE TABLE OF ty_output,
      gs_output type ty_output,
      gt_lfb1 type table of lfb1,
      gs_lfb1 type lfb1.

SELECTION-SCREEN BEGIN OF block b1 WITH FRAME TITLE text-001.
  PARAMETERS: p_sdat type cdhdr-udate DEFAULT sy-datum,
              p_edat type cdhdr-udate DEFAULT sy-datum,
              p_file LIKE filenameci-fileextern
              DEFAULT '/usr/sap/interfaces/P01/IFFI063/EAST_VMF.txt'
              OBLIGATORY.
*             NO-DISPLAY.
SELECTION-SCREEN end of block b1.

AT SELECTION-SCREEN ON BLOCK b1.
   IF p_sdat > p_edat.
      MESSAGE i000(zfi01) WITH 'From-Date should be <= To-Date' .
      Stop.
   ENDIF.
START-OF-SELECTION.

clear: gt_lfa1,
       gt_cdhdr,
       gt_lfb1,
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

data: lv_objectclas type cdhdr-objectclas VALUE 'KRED',
      lv_objectid type cdhdr-objectid.
types: BEGIN OF ty_cdhdr,
       objectclas type cdhdr-objectclas,
       objectid   type cdhdr-objectid,
       END OF ty_cdhdr.
data: ls_cdhdr type ty_cdhdr,
      lt_cdhdr type TABLE OF ty_cdhdr,
      lv_tabix type i.

CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
 EXPORTING
   PERCENTAGE       = 0
   TEXT             = 'Extracting Vendor Data '.

Select * FROM LFA1 into table gt_LFA1.
IF gt_lfa1 is INITIAL.
   write: / 'No Vendor data to process....'.
   stop.
ENDIF.
CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
 EXPORTING
   PERCENTAGE       = 15
   TEXT             = 'Extracting Company Specific Vendor Data '.
select * from lfb1 into table gt_lfb1
          FOR ALL ENTRIES IN gt_lfa1
          where lifnr = gt_lfa1-lifnr.

sort gt_lfa1 by lifnr.
sort gt_lfb1 by lifnr bukrs.

LOOP AT gt_lfa1 INTO gs_lfa1.
     ls_cdhdr-objectclas = 'KRED'.
     ls_cdhdr-objectid = gs_lfa1-lifnr.
     append ls_cdhdr to lt_cdhdr.
ENDLOOP.
CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
 EXPORTING
   PERCENTAGE       = 25
   TEXT             = 'Extracting Vendor changed Data '.
clear: gt_cdhdr.
select * from cdhdr INTO TABLE gt_cdhdr
         FOR ALL ENTRIES IN lt_cdhdr
         where OBJECTCLAS = lt_cdhdr-OBJECTCLAS
           and OBJECTID   = lt_cdhdr-objectid
           and ( udate >= p_sdat and
                 udate <= p_edat ).
sort gt_cdhdr by objectid udate.

CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
 EXPORTING
   PERCENTAGE       = 50
   TEXT             = 'Processing Data '.
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
    gs_output-d10 = '|'.
    gs_output-d11 = '|'.
    gs_output-d12 = '|'.
    gs_output-d13 = '|'.
    gs_output-d14 = '|'.
    gs_output-d15 = '|'.
    gs_output-d16 = '|'.
    gs_output-d17 = '|'.
*    gs_output-d18 = '"'.
*    gs_output-d19 = '"'.
    gs_output-LNRZA = 'LNRZA'.
    gs_output-STCD1 = 'STCD1'.
    gs_output-XCPDK = 'XCPDK'.
    gs_output-LIFNR = 'LIFNR'.
    gs_output-NAME1 = 'NAME1'.
    gs_output-STRAS = 'STRAS'.
    gs_output-PSTLZ = 'PSTLZ'.
    gs_output-PFACH = 'PFACH'.
    gs_output-STCD2 = 'STCD2'.
    gs_output-ZTERM = 'ZTERM'..
    gs_output-BEGRU = 'BEGRU'.
    gs_output-TELF1 = 'TELF1'.
    gs_output-ORT01 = 'ORT01'.
    gs_output-ORT02 = 'ORT02'.
    gs_output-REGIO = 'REGIO'.
    gs_output-udate = 'UDATE'.
    gs_output-USERNAME = 'USERNAME'.
    gs_output-BUKRS = 'BUKRS'.
    APPEND gs_output to gt_output.
****************
LOOP AT gt_lfa1 INTO gs_lfa1.
    clear: gs_output,
           gs_lfb1.
*           gt_cdhdr.

    lv_objectid = gs_lfa1-lifnr.
*    select * FROM cdhdr into table gt_cdhdr
*                 WHERE OBJECTCLAS = lv_objectclas
*                   and OBJECTID   = lv_objectid
*                   and ( udate >= p_sdat and
*                         udate <= p_edat ).
*    sort gt_cdhdr by objectid udate.

*READ TABLE gt_lfb1 INTO gs_lfb1
*         with key lifnr = gs_lfa1-lifnr.
    gs_output-d1  = '|'.
    gs_output-d2  = '|'.
    gs_output-d3  = '|'.
    gs_output-d4  = '|'.
    gs_output-d5  = '|'.
    gs_output-d6  = '|'.
    gs_output-d7  = '|'.
    gs_output-d8  = '|'.
    gs_output-d9  = '|'.
    gs_output-d10 = '|'.
    gs_output-d11 = '|'.
    gs_output-d12 = '|'.
    gs_output-d13 = '|'.
    gs_output-d14 = '|'.
    gs_output-d15 = '|'.
    gs_output-d16 = '|'.
    gs_output-d17 = '|'.
*    gs_output-d18 = '"'.
*    gs_output-d19 = '"'.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-STCD1 WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-STCD1 WITH space.
     REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-STCD1 WITH space.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-name1 WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-name1 WITH space.
     REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-name1 WITH space.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-STras WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-STras WITH space.
    REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-STras WITH space.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-pstlz WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-pstlz WITH space.
     REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-pstlz WITH space.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-pfach WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-pfach WITH space.
    REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-pfach WITH space.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-STcd2 WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-STcd2 WITH space.
     REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-STcd2 WITH space.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-telf1 WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-telf1 WITH space.
     REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-telf1 WITH space.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-ort01 WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-ort01 WITH space.
     REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-ort01 WITH space.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-ort02 WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-ort02 WITH space.
    REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-ort02 WITH space.
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF
                               in gs_LFA1-regio WITH space.
    REPLACE ALL OCCURRENCES OF '|'
                               in gs_LFA1-regio WITH space.
    REPLACE ALL OCCURRENCES OF '#'
                               in gs_LFA1-regio WITH space.
    gs_output-LNRZA = gs_LFA1-LNRZA.
    gs_output-STCD1 = gs_LFA1-STCD1.
    gs_output-XCPDK = gs_LFA1-XCPDK.
    gs_output-LIFNR = gs_lfa1-lifnr.
    gs_output-NAME1 = gs_lfa1-name1.
    gs_output-STRAS = gs_lfa1-stras.
    gs_output-PSTLZ = gs_lfa1-pstlz.
    gs_output-PFACH = gs_lfa1-pfach.
    gs_output-STCD2 = gs_lfa1-stcd2.
*    gs_output-ZTERM = gs_lfb1-zterm.
    gs_output-BEGRU = gs_lfa1-begru.
    gs_output-TELF1 = gs_lfa1-telf1.
    gs_output-ORT01 = gs_lfa1-ort01.
    gs_output-ORT02 = gs_lfa1-ort02.
    gs_output-REGIO = gs_lfa1-regio.

    READ TABLE gt_cdhdr with key OBJECTID = lv_objectid
                        TRANSPORTING NO FIELDS.
    lv_tabix = sy-TABIX.
    LOOP AT gt_cdhdr INTO gs_cdhdr FROM lv_tabix.
         if gs_cdhdr-OBJECTID  <> lv_objectid.
            exit.
         endif.
         gs_output-udate = gs_cdhdr-udate.
         gs_output-USERNAME = gs_CDHDR-USERNAME.
    ENDLOOP.
    IF gs_output-udate is INITIAL.
       gs_output-udate = 'N/C'.
    ENDIF.
    READ TABLE gt_lfb1 with key lifnr = gs_lfa1-lifnr
                        TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.
     LOOP AT gt_lfb1 INTO gs_lfb1 FROM lv_tabix.
         if gs_lfb1-lifnr <> gs_lfa1-lifnr.
            exit.
         endif.
         gs_output-bukrs = gs_lfb1-bukrs.
         gs_output-ZTERM = gs_lfb1-zterm.
         APPEND gs_output to gt_output.
    ENDLOOP.
   ELSE.
     APPEND gs_output to gt_output.
   ENDIF.
ENDLOOP.



endform.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       Download file
*----------------------------------------------------------------------*
form DOWNLOAD_DATA .
  data: recsize type i VALUE 325.

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
