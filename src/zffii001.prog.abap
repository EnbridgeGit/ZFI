REPORT zffii001 MESSAGE-ID zs.
************************************************************************
* 2012/05/22 SAHMAD   #    New CRA requirements
* 2008/11/07 mdemeest #636 Changes for November 2008 audit requested
*                          by Leo Marentette for TIM KETTLEWELL, CRA
*                          Create 1 file using MARA, MAKT, MLAN, MARC
* 2001/03/14 mdemeest #--- Revenue Canada GST Audit
* 1999/04/05 mdemeest #639 Extract MARA info and create flat file for
*                          Revenue Canada

************************************************************************
TYPES: BEGIN OF ty_t001,
         mandt TYPE t001-mandt, "3
         sp1(1),
         bukrs TYPE t001-bukrs, "4
         sp2(1),
         butxt TYPE t001-butxt,	"25
         sp3(1),
         ort01 TYPE t001-ort01,	"25
         sp4(1),
         land1 TYPE t001-land1, "	3
         sp5(1),
         waers TYPE t001-waers, "	5
         sp6(1),
         spras TYPE t001-spras, "	1
         sp7(1),
         ktopl TYPE t001-ktopl, "	4
         sp8(1),
         periv TYPE t001-periv, "	2
       END OF ty_t001.
DATA: BEGIN OF t001_header OCCURS 1,
       mandt(5) VALUE 'MANDT',
       sp1   TYPE c VALUE '~',
       bukrs(5) VALUE 'BUKRS',
       sp2   TYPE c VALUE '~',
       butxt(25) VALUE 'BUTXT',
       sp3   TYPE c VALUE '~',
       ort01(25) VALUE 'ORT01',
       sp4   TYPE c VALUE '~',
       land1(8) VALUE 'LAND1',
       sp5   TYPE c VALUE '~',
       waers(5) VALUE 'WAERS',
       sp6   TYPE c VALUE '~',
       spras(5) VALUE 'SPRAS',
       sp7   TYPE c VALUE '~',
       ktopl(5) VALUE 'KTOPL',
       sp8   TYPE c VALUE '~',
       periv(5) VALUE 'PERIV',
      END OF t001_header.
TYPES: BEGIN OF ty_t001w,
        mandt TYPE t001w-mandt, "  3
        sp1(1),
        werks TYPE t001w-werks, "  4
        sp2(1),
        name1 TYPE t001w-name1, "  30
        sp3(1),
        name2 TYPE t001w-name2, "  30
        sp4(1),
        ort01 TYPE t001w-ort01, "  25
        sp5(1),
        land1 TYPE t001w-land1,   "3
        sp6(1),
        regio TYPE t001w-regio, "  3
       END OF ty_t001w.
DATA: BEGIN OF t001w_header OCCURS 1,
        mandt(5) VALUE 'MANDT',
        sp1   TYPE c VALUE '~',
        werks(5) VALUE 'WERKS',
        sp2   TYPE c VALUE '~',
        name1(30) VALUE 'NAME1',
        sp3   TYPE c VALUE '~',
        name2(30) VALUE 'NAME2',
        sp4   TYPE c VALUE '~',
        ort01(8) VALUE 'ORT01',
        sp5   TYPE c VALUE '~',
        land1(5) VALUE 'LAND1',
        sp6   TYPE c VALUE '~',
        regio(5) VALUE 'REGIO',
      END OF t001w_header.
TYPES: BEGIN OF ty_t003t,
        mandt TYPE t003t-mandt, "	3
        sp1(1),
        spras TYPE t003t-spras, "	1
        sp2(1),
        blart TYPE t003t-blart, "	2
        sp3(1),
        ltext TYPE t003t-ltext, "	20
       END OF ty_t003t.
DATA: BEGIN OF t003t_header OCCURS 1,
       mandt(5) VALUE 'MANDT',
       sp1   TYPE c VALUE '~',
       spras(5) VALUE 'SPRAS',
       sp2   TYPE c VALUE '~',
       blart(5) VALUE 'BLART',
       sp3   TYPE c VALUE '~',
       ltext(20) VALUE 'LTEXT',
      END OF t003t_header.
TYPES: BEGIN OF ty_t005t,
        mandt TYPE t005t-mandt, "	3
        sp1(1),
        spras TYPE t005t-spras, "	1
        sp2(1),
        land1 TYPE t005t-land1, "	3
        sp3(1),
        landx TYPE t005t-landx, "	15
       END OF ty_t005t.
DATA: BEGIN OF t005t_header OCCURS 1,
        mandt(5) VALUE 'MANDT',
        sp1   TYPE c VALUE '~',
        spras(5) VALUE 'SPRAS',
        sp2   TYPE c VALUE '~',
        land1(5) VALUE 'LAND1',
        sp3   TYPE c VALUE '~',
        landx(15) VALUE 'LANDX',
      END OF t005t_header.
TYPES: BEGIN OF ty_t007s,
        mandt TYPE t007s-mandt, "3
        sp1(1),
        spras TYPE t007s-spras, "1
        sp2(1),
        kalsm TYPE t007s-kalsm, "6
        sp3(1),
        mwskz TYPE t007s-mwskz, "2
        sp4(1),
        text1 TYPE t007s-text1, "50
       END OF ty_t007s.
DATA: BEGIN OF t007s_header OCCURS 1,
        mandt(5) VALUE 'MANDT',
        sp1   TYPE c VALUE '~',
        spras(5) VALUE 'SPRAS',
        sp2   TYPE c VALUE '~',
        kalsm(6) VALUE 'KALSM',
        sp3   TYPE c VALUE '~',
        mwskz(5) VALUE 'MWSKZ',
        sp4   TYPE c VALUE '~',
        text1(50) VALUE 'TEXT1',
      END OF t007s_header.

TYPES: BEGIN OF ty_tbslt,
        mandt TYPE tbslt-mandt,	"3
        sp1(1),
        spras TYPE tbslt-spras, "1
        sp2(1),
        bschl TYPE tbslt-bschl, "2
        sp3(1),
        umskz TYPE tbslt-umskz, "	1
        sp4(1),
        ltext TYPE tbslt-ltext, "	20
      END OF ty_tbslt.
DATA: BEGIN OF tbslt_header OCCURS 1,
       mandt(5) VALUE 'MANDT',
       sp1   TYPE c VALUE '~',
       spras(5) VALUE 'SPRAS',
       sp2   TYPE c VALUE '~',
       bschl(5) VALUE 'BSCHL',
       sp3   TYPE c VALUE '~',
       umskz(5) VALUE 'UMSKZ',
       sp4   TYPE c VALUE '~',
       ltext(20) VALUE 'LTEXT',
      END OF tbslt_header.
TYPES: BEGIN OF ty_tabwt,
        mandt TYPE tabwt-mandt, "	3
        sp1(1),
        spras TYPE tabwt-spras, "	1
        sp2(1),
        bwasl TYPE tabwt-bwasl, "	3
        sp3(1),
        bwatxt TYPE tabwt-bwatxt, "	50
      END OF ty_tabwt.
DATA: BEGIN OF tabwt_header OCCURS 1,
       mandt(5) VALUE 'MANDT',
       sp1   TYPE c VALUE '~',
       spras(5) VALUE 'SPRAS',
       sp2   TYPE c VALUE '~',
       bwsal(6) VALUE 'BWASL',
       sp3   TYPE c VALUE '~',
       bwatxt(5) VALUE 'BWATXT',
      END OF tabwt_header.
TYPES: BEGIN OF ty_tvstt,
        mandt TYPE tvstt-mandt, "	3
        sp1(1),
        spras TYPE tvstt-spras, "	1
        sp2(1),
        vstel TYPE tvstt-vstel, "	4
        sp3(1),
        vtext TYPE tvstt-vtext, "	30
      END OF ty_tvstt.
DATA: BEGIN OF tvstt_header OCCURS 1,
        mandt(5) VALUE 'MANDT',
        sp1   TYPE c VALUE '~',
        spras(5) VALUE 'SPRAS',
        sp2   TYPE c VALUE '~',
        vstel(5) VALUE 'VSTEL',
        sp3   TYPE c VALUE '~',
        vtext(30) VALUE 'VTEXT',
      END OF tvstt_header.
TYPES: BEGIN OF ty_cskt,
        mandt TYPE cskt-mandt, "  3
        sp1(1),
        spras TYPE cskt-spras, "  1
        sp2(1),
        kokrs TYPE cskt-kokrs, "  4
        sp3(1),
        kostl TYPE cskt-kostl, "  10
        sp4(1),
        datbi TYPE cskt-datbi, "  8
        sp5(1),
        ktext TYPE cskt-ktext, "  20
        sp6(1),
        ltext TYPE cskt-ltext, "  40
        sp7(1),
        mctxt TYPE cskt-mctxt, "  20
      END OF ty_cskt.
DATA: BEGIN OF cskt_header OCCURS 1,
        mandt(5) VALUE 'MANDT',
        sp1   TYPE c VALUE '~',
        spras(5) VALUE 'SPRAS',
        sp2   TYPE c VALUE '~',
        kokrs(5) VALUE 'KOKRS',
        sp3   TYPE c VALUE '~',
        kostl(10) VALUE 'KOSTL',
        sp4   TYPE c VALUE '~',
        datbi(8) VALUE 'DATBI',
        sp5  TYPE c VALUE '~',
        ktext(20) VALUE 'KTEXT',
        sp6   TYPE c VALUE '~',
        ltext(40) VALUE 'LTEXT',
        sp7   TYPE c VALUE '~',
        mctxt(20) VALUE 'MCTXT',
      END OF cskt_header.
DATA: gt_t001 TYPE TABLE OF ty_t001,
      gs_t001 TYPE ty_t001,
      gt_t001w TYPE TABLE OF ty_t001w,
      gs_t001w TYPE ty_t001w,
      gt_t003t TYPE TABLE OF ty_t003t,
      gs_t003t TYPE ty_t003t,
      gt_t005t TYPE TABLE OF ty_t005t,
      gs_t005t TYPE ty_t005t,
      gt_t007s TYPE TABLE OF ty_t007s,
      gs_t007s TYPE ty_t007s,
      gt_tbslt TYPE TABLE OF ty_tbslt,
      gs_tbslt TYPE ty_tbslt,
      gt_tabwt TYPE TABLE OF ty_tabwt,
      gs_tabwt TYPE ty_tabwt,
      gt_tvstt TYPE TABLE OF ty_tvstt,
      gs_tvstt TYPE ty_tvstt,
      gt_cskt TYPE TABLE OF ty_cskt,
      gs_cskt TYPE ty_cskt.
* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETER: table LIKE rseux-cdt_value DEFAULT 'MARA' NO-DISPLAY,
           file1 LIKE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/MARA.SAP' OBLIGATORY,
           p_t001  TYPE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/T001.SAP' OBLIGATORY,
           p_t001w TYPE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/T001W.SAP' OBLIGATORY,
           p_t003t  TYPE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/T003T.SAP' OBLIGATORY,
           p_t005t  TYPE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/T005T.SAP' OBLIGATORY,
           p_t007s  TYPE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/T007S.SAP' OBLIGATORY,
           p_tbslt  TYPE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/TBSLT.SAP' OBLIGATORY,
           p_tabwt  TYPE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/TABWT.SAP' OBLIGATORY,
           p_tvstt  TYPE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/TVSTT.SAP' OBLIGATORY,
           p_cskt  TYPE filenameci-fileextern
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/CSKT.SAP' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF block b2 WITH FRAME TITLE text-002.
  PARAMETER: p_spras TYPE cskt-spras DEFAULT 'EN' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.
*           recsize type i default 87.

*PARAMETER: TABLE2 LIKE RSEUX-CDT_VALUE DEFAULT 'MARC',
*           FILE2 like filenameci-fileextern
*                 default '/usr/sap/interfaces/P01/CFMM001/MARC.SAP',
*           recsize2 type i default 25.


TABLES: mara, makt, mlan, marc.


DATA: record(90).

DATA: recsize TYPE i VALUE 110.

DATA: BEGIN OF wamara OCCURS 50000,
      mandt LIKE mara-mandt,
      sp1   TYPE c VALUE '~',
      matnr LIKE mara-matnr,
      sp2   TYPE c VALUE '~',
      mtart LIKE mara-mtart,
      sp3   TYPE c VALUE '~',
      matkl LIKE mara-matkl,
      sp4   TYPE c VALUE '~',
      meins LIKE mara-meins,
      sp5   TYPE c VALUE '~',
      werks LIKE marc-werks,
      sp6   TYPE c VALUE '~',
      maktx LIKE makt-maktx,
*      taxm1 like mlan-taxm1,
*      taxm2 like mlan-taxm2,
*      taxm3 like mlan-taxm2,
*      taxm4 like mlan-taxm2,
*      taxm5 like mlan-taxm2,
*      taxm6 like mlan-taxm2,

      END OF wamara.
DATA: BEGIN OF wamara_header OCCURS 1,
      mandt(5) VALUE 'MANDT',
      sp1   TYPE c VALUE '~',
      matnr(18) VALUE 'MATNR',
      sp2   TYPE c VALUE '~',
      mtart(5) VALUE 'MTART',
      sp3   TYPE c VALUE '~',
      matkl(9) VALUE 'MATKL',
      sp4   TYPE c VALUE '~',
      meins(5) VALUE 'MEINS',
      sp5   TYPE c VALUE '~',
      werks(5) VALUE 'WERKS',
      sp6   TYPE c VALUE '~',
      maktx(40) VALUE 'MAKTX',

      END OF wamara_header.

DATA: BEGIN OF wamarc OCCURS 0,
      mandt LIKE marc-mandt,
      matnr LIKE marc-matnr,
      werks LIKE marc-werks,
      END OF wamarc.

START-OF-SELECTION.

  CLEAR: gt_t001,
         gs_t001,
         gt_t001w,
         gs_t001w,
         gt_t003t,
         gs_t003t,
         gt_t005t,
         gs_t005t,
         gt_t007s,
         gs_t007s,
         gt_tbslt,
         gs_tbslt,
         gt_tabwt,
         gs_tabwt,
         gt_tvstt,
         gs_tvstt,
         gt_cskt,
         gs_cskt.

  PERFORM get_data.
  PERFORM open_output_file.
  PERFORM transfer_mara.
  PERFORM output_other_files.
  PERFORM close_files.


************************************************************************

FORM open_output_file.
  OPEN DATASET file1  FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open MARA file for output.'.
  ENDIF.
  OPEN DATASET p_t001  FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open T001 file for output.'.
  ENDIF.
  OPEN DATASET p_t001w  FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open T001W file for output.'.
  ENDIF.
  OPEN DATASET p_t003t FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open T003T file for output.'.
  ENDIF.
  OPEN DATASET p_t005t  FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open T005T file for output.'.
  ENDIF.
  OPEN DATASET p_t007s FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open T007S file for output.'.
  ENDIF.
  OPEN DATASET p_tbslt FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open TBSLT file for output.'.
  ENDIF.
  OPEN DATASET p_tabwt FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open TABWT file for output.'.
  ENDIF.
  OPEN DATASET p_tvstt FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open TVSTT file for output.'.
  ENDIF.
  OPEN DATASET p_cskt FOR OUTPUT IN TEXT MODE.
  IF sy-subrc <> 0.
    WRITE : / ' Unable to open CSKT file for output.'.
  ENDIF.
*    OPEN DATASET FILE2  FOR OUTPUT IN TEXT MODE.
ENDFORM.                    "OPEN_OUTPUT_FILE



*---------------------- MOVE_MARA+MLAN_MAKT_INFO------------------------
FORM move_mara_mlan_makt_info.
  CLEAR wamara.

*-----------------------------------------------------------------------
* This moves all the required MARA fields to WAMARA.  Only fields with
* same attributes in source and target will move.
* For 2008, madt, matnr, mtart, matkl, meins.
*-----------------------------------------------------------------------

  MOVE-CORRESPONDING mara TO wamara.

*-----------------------------------------------------------------------
* Move description for MAKT field to WAMARA internal table.
*-----------------------------------------------------------------------
  SELECT SINGLE * FROM makt
    WHERE matnr = wamara-matnr.
  IF sy-subrc = '0'.
    MOVE makt-maktx TO wamara-maktx.
  ELSE.
    MOVE 'Description not available' TO wamara-maktx.
  ENDIF.

*-----------------------------------------------------------------------
* Move tax codes from MLAN to the WAMARA internal table.
*-----------------------------------------------------------------------
  SELECT SINGLE * FROM mlan
    WHERE matnr = wamara-matnr.
  IF sy-subrc = '0'.
    MOVE-CORRESPONDING mlan TO wamara.
  ENDIF.

ENDFORM.                    "MOVE_MARA_MLAN_MAKT_INFO

*-----------------------------------------------------------------------
* Information for a material is the same for each & every plant
* So only the plant number changes.
*-----------------------------------------------------------------------

FORM plant_selection.
*  select * from marc
*    where matnr = wamara-matnr.
*    move marc-mandt to wamarc-mandt.
*    move marc-matnr to wamarc-matnr.
*    move marc-werks to wamarc-werks.
*    TRANSFER WAMARC TO FILE2 LENGTH RECSIZE2.  "Output record
*  endselect.
*
*  TRANSFER WAMARA TO FILE1 LENGTH RECSIZE.  "Output record

ENDFORM.                    "PLANT_SELECTION
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_MARA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_mara .
  TRANSFER wamara_header TO file1 LENGTH recsize.

  SELECT * FROM mara.
    CLEAR wamara.
*  PERFORM MOVE_MARA_MLAN_MAKT_INFO.      "Move info to work area
*  PERFORM PLANT_SELECTION.
********
    MOVE-CORRESPONDING mara TO wamara.
    SELECT SINGLE maktx INTO wamara-maktx
        FROM makt
        WHERE matnr = wamara-matnr
          AND spras = 'EN'.
    SELECT * FROM marc
      WHERE matnr = wamara-matnr.
      MOVE marc-werks TO wamara-werks.
      wamara-sp1 = '~'.
      wamara-sp2 = '~'.
      wamara-sp3 = '~'.
      wamara-sp4 = '~'.
      wamara-sp5 = '~'.
      wamara-sp6 = '~'.
      TRANSFER wamara TO file1 LENGTH recsize.  "Output record
    ENDSELECT.
********
  ENDSELECT.

ENDFORM.                    " TRANSFER_MARA
*&---------------------------------------------------------------------*
*&      Form  CLOSE_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM close_files .

  CLOSE DATASET: file1, p_t001, p_t001w, p_t003t, p_t005t,
                 p_t007s, p_tbslt, p_tabwt, p_tvstt, p_cskt.
  IF sy-subrc = 0.
    WRITE: / 'Process is completed.'.
    WRITE: / file1.
    WRITE: / p_t001.
    WRITE: / p_t001w.
    WRITE: / p_t003t.
    WRITE: / p_t003t.
    WRITE: / p_t005t.
    WRITE: / p_t007s.
    WRITE: / p_tbslt.
    WRITE: / p_tabwt.
    WRITE: / p_tvstt.
    WRITE: / p_cskt.
  ELSE.
    WRITE : / 'Error in Closing output file, please check'.
  ENDIF.
ENDFORM.                    " CLOSE_FILES
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  CONSTANTS: c_taxca TYPE t007s-kalsm VALUE 'TAXCA'.

  SELECT * FROM t001 INTO CORRESPONDING FIELDS OF TABLE gt_t001
                                          WHERE spras = p_spras.

  SELECT * FROM t001w INTO CORRESPONDING FIELDS OF TABLE gt_t001w
                                            WHERE spras = p_spras.

  SELECT * FROM t003t INTO CORRESPONDING FIELDS OF TABLE gt_t003t
                                            WHERE spras = p_spras.

  SELECT * FROM t005t INTO CORRESPONDING FIELDS OF TABLE gt_t005t
                                            WHERE spras = p_spras.

  SELECT * FROM t007s INTO CORRESPONDING FIELDS OF TABLE gt_t007s
                                            WHERE spras = p_spras
                                              AND kalsm = c_taxca.

  SELECT * FROM tbslt INTO CORRESPONDING FIELDS OF TABLE gt_tbslt
                                            WHERE spras = p_spras.

  SELECT * FROM tabwt INTO CORRESPONDING FIELDS OF TABLE gt_tabwt
                                            WHERE spras = p_spras.

  SELECT * FROM tvstt INTO CORRESPONDING FIELDS OF TABLE gt_tvstt
                                            WHERE spras = p_spras.

  SELECT * FROM cskt INTO CORRESPONDING FIELDS OF TABLE gt_cskt
                                            WHERE spras = p_spras.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_OTHER_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM output_other_files .

  IF gt_t001[] IS INITIAL.
    WRITE : / 'No T001 data for output'.
  ENDIF.
  TRANSFER t001_header TO p_t001.
  LOOP AT gt_t001 INTO gs_t001.
    gs_t001-sp1 = '~'.
    gs_t001-sp2 = '~'.
    gs_t001-sp3 = '~'.
    gs_t001-sp4 = '~'.
    gs_t001-sp5 = '~'.
    gs_t001-sp6 = '~'.
    gs_t001-sp7 = '~'.
    gs_t001-sp8 = '~'.
    TRANSFER gs_t001 TO p_t001.
  ENDLOOP.

  IF gt_t001w[] IS INITIAL.
    WRITE: / 'No T001W data for output'.
  ENDIF.
  TRANSFER t001w_header TO p_t001w.
  LOOP AT gt_t001w INTO gs_t001w.
    gs_t001w-sp1 = '~'.
    gs_t001w-sp2 = '~'.
    gs_t001w-sp3 = '~'.
    gs_t001w-sp4 = '~'.
    gs_t001w-sp5 = '~'.
    gs_t001w-sp6 = '~'.
    TRANSFER gs_t001w TO p_t001w.
  ENDLOOP.

  IF gt_t003t[] IS INITIAL.
    WRITE: / 'No T003T data for output'.
  ENDIF.
  TRANSFER t003t_header TO p_t003t.
  LOOP AT gt_t003t INTO gs_t003t.
    gs_t003t-sp1 = '~'.
    gs_t003t-sp2 = '~'.
    gs_t003t-sp3 = '~'.
    TRANSFER gs_t003t TO p_t003t.
  ENDLOOP.

  IF gt_t005t[] IS INITIAL.
    WRITE : / 'No T005T data for output'.
  ENDIF.
  TRANSFER t005t_header TO p_t005t.
  LOOP AT gt_t005t INTO gs_t005t.
    gs_t005t-sp1 = '~'.
    gs_t005t-sp2 = '~'.
    gs_t005t-sp3 = '~'.
    TRANSFER gs_t005t TO p_t005t.
  ENDLOOP.

  IF gt_t007s[] IS INITIAL.
    WRITE : / 'No T007S data for output'.
  ENDIF.
  TRANSFER t007s_header TO p_t007s.
  LOOP AT gt_t007s INTO gs_t007s.
    gs_t007s-sp1 = '~'.
    gs_t007s-sp2 = '~'.
    gs_t007s-sp3 = '~'.
    gs_t007s-sp4 = '~'.
    TRANSFER gs_t007s TO p_t007s.
  ENDLOOP.

  IF gt_tbslt[] IS INITIAL.
    WRITE: / 'No TBSLT data for output'.
  ENDIF.
  TRANSFER tbslt_header TO p_tbslt.
  LOOP AT gt_tbslt INTO gs_tbslt.
    gs_tbslt-sp1 = '~'.
    gs_tbslt-sp2 = '~'.
    gs_tbslt-sp3 = '~'.
    gs_tbslt-sp4 = '~'.
    TRANSFER gs_tbslt TO p_tbslt.
  ENDLOOP.

  IF gt_tabwt[] IS INITIAL.
    WRITE : / 'No TABWT data for output'.
  ENDIF.
  TRANSFER tabwt_header TO p_tabwt.
  LOOP AT gt_tabwt INTO gs_tabwt.
    gs_tabwt-sp1 = '~'.
    gs_tabwt-sp2 = '~'.
    gs_tabwt-sp3 = '~'.
    TRANSFER gs_tabwt TO p_tabwt.
  ENDLOOP.

  IF gt_tvstt[] IS INITIAL .
    WRITE : / 'No TVSTT data for output'.
  ENDIF.
  TRANSFER tvstt_header TO p_tvstt.
  LOOP AT gt_tvstt INTO gs_tvstt.
    gs_tvstt-sp1 = '~'.
    gs_tvstt-sp2 = '~'.
    gs_tvstt-sp3 = '~'.
    TRANSFER gs_tvstt TO p_tvstt.
  ENDLOOP.

  IF gt_cskt[] IS INITIAL.
    WRITE: / 'No CSKT data for output'.
  ENDIF.
  TRANSFER cskt_header TO p_cskt.
  LOOP AT gt_cskt INTO gs_cskt.
    gs_cskt-sp1 = '~'.
    gs_cskt-sp2 = '~'.
    gs_cskt-sp3 = '~'.
    gs_cskt-sp4 = '~'.
    gs_cskt-sp5 = '~'.
    gs_cskt-sp6 = '~'.
    gs_cskt-sp7 = '~'.
    TRANSFER gs_cskt TO p_cskt.
  ENDLOOP.
ENDFORM.                    " OUTPUT_OTHER_FILES
