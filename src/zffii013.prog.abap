*&---------------------------------------------------------------------*
*& Report  ZFFII013
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zffii013.
TABLES: anla, anlz, ankt.

TYPES: BEGIN OF ty_anla,
        mandt TYPE  anla-mandt,
        sp1   TYPE c ,
        bukrs TYPE  anla-bukrs,
        sp2   TYPE c ,
        anln1 TYPE  anla-anln1,
        sp3   TYPE c ,
        anln2 TYPE  anla-anln2,
        sp4   TYPE c ,
        anlkl TYPE  anla-anlkl,
        sp5   TYPE c ,
        txa50 TYPE  anla-txa50,
        sp6   TYPE c ,
        txt50 TYPE  anla-txt50,
        sp7   TYPE c ,
        aktiv TYPE  anla-aktiv,
        sp8   TYPE c ,
        deakt TYPE  anla-deakt,
       END OF   ty_anla.
TYPES: BEGIN OF ty_anlz,
        mandt TYPE anlz-mandt,
        sp1   TYPE c ,
        bukrs TYPE anlz-bukrs,
        sp2   TYPE c ,
        anln1 TYPE anlz-anln1,
        sp3   TYPE c ,
        bdatu TYPE anlz-bdatu,
        sp4   TYPE c ,
        adatu TYPE anlz-adatu,
        sp5   TYPE c ,
        kostl TYPE anlz-kostl,
        sp6   TYPE c ,
        werks TYPE anlz-werks,
        sp7   TYPE c ,
        stort TYPE anlz-stort,
       END OF ty_anlz.
TYPES: BEGIN OF ty_ankt,
        mandt TYPE ankt-mandt,
        sp1   TYPE c ,
        spras TYPE ankt-spras,
        sp2   TYPE c ,
        anlkl TYPE ankt-anlkl,
        sp3   TYPE c ,
        txk20 TYPE ankt-txk20,
        sp4   TYPE c ,
        txk50 TYPE ankt-txk50,
        sp5   TYPE c ,
        txt50 TYPE ankt-txt50,
        sp6   TYPE c ,
        txa50 TYPE ankt-txa50,
       END OF ty_ankt.

DATA: BEGIN OF anlz_header OCCURS 1,
      mandt(5) VALUE 'MANDT',
      sp1   TYPE c VALUE '~',
      bukrs(5) VALUE 'BUKRS',
      sp2   TYPE c VALUE '~',
      anln1(12) VALUE 'ANLN1',
      sp3   TYPE c VALUE '~',
      bdatu(8) VALUE 'BDATU',
      sp5   TYPE c VALUE '~',
      adatu(8) VALUE 'ADATU',
      sp6   TYPE c VALUE '~',
      kostl(10) VALUE 'KOSTL',
      sp7   TYPE c VALUE '~',
      werks(5) VALUE 'WERKS',
      sp8   TYPE c VALUE '~',
      stort(10) VALUE 'STORT',
      END OF anlz_header.
DATA: BEGIN OF ankt_header OCCURS 1,
      mandt(5) VALUE 'MANDT',
      sp1   TYPE c VALUE '~',
      spras(5) VALUE 'SPRAS',
      sp2   TYPE c VALUE '~',
      anlkl(8) VALUE 'ANLKL',
      sp3   TYPE c VALUE '~',
      txk20(20) VALUE 'TXK20',
      sp4   TYPE c VALUE '~',
      txk50(50) VALUE 'TXK50',
      sp5   TYPE c VALUE '~',
      txt50(50) VALUE 'TXT50',
      sp6   TYPE c VALUE '~',
      txa50(50) VALUE 'TXA50',
      END OF ankt_header.
DATA: BEGIN OF anla_header OCCURS 1,
      mandt(5) VALUE 'MANDT',
      sp1   TYPE c VALUE '~',
      bukrs(5) VALUE 'BUKRS',
      sp2   TYPE c VALUE '~',
      anln1(12) VALUE 'ANLN1',
      sp3   TYPE c VALUE '~',
      anln2(5) VALUE 'ANLN2',
      sp4   TYPE c VALUE '~',
      anlkl(8) VALUE 'ANLKL',
      sp5   TYPE c VALUE '~',
      txa50(50) VALUE 'TXA50',
      sp6   TYPE c VALUE '~',
      txt50(50) VALUE 'TXT50',
      sp7   TYPE c VALUE '~',
      aktiv(8) VALUE 'AKTIV',
      sp8   TYPE c VALUE '~',
      deakt(8) VALUE 'DEAKT',
      END OF anla_header.
DATA: gt_anla TYPE TABLE OF ty_anla,
      gt_anlz TYPE TABLE OF ty_anlz,
      gt_ankt TYPE TABLE OF ty_ankt,
      gs_anla TYPE ty_anla,
      gs_anlz TYPE ty_anlz,
      gs_ankt TYPE ty_ankt.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETER: p_anla_f LIKE filenameci-fileextern
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/ANLA.SAP' OBLIGATORY.
PARAMETER: p_anlz_f LIKE filenameci-fileextern
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/ANLZ.SAP' OBLIGATORY.
PARAMETER: p_ankt_f LIKE filenameci-fileextern
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/ANKT.SAP' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_bukrs1 FOR anla-bukrs,
                s_anln11 FOR anla-anln1,
                s_anln21 FOR anla-anln2,
                s_anlkl  FOR anla-anlkl.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_bukrs2 FOR anlz-bukrs,
                s_anln12 FOR anlz-anln1,
                s_anln22 FOR anlz-anln2.
SELECTION-SCREEN END OF BLOCK b3.

START-OF-SELECTION.

  CLEAR: gt_anla,
         gt_anlz,
         gt_ankt.

  PERFORM get_data.
  PERFORM output_data.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  SELECT * FROM anla INTO CORRESPONDING FIELDS OF TABLE gt_anla
    WHERE bukrs IN s_bukrs1 AND
          anln1 IN s_anln11 AND
          anln2 IN s_anln21 AND
          anlkl IN s_anlkl.

  SELECT * FROM anlz INTO CORRESPONDING FIELDS OF TABLE gt_anlz
    WHERE bukrs IN s_bukrs2 AND
          anln1 IN s_anln12 AND
          anln2 IN s_anln22.

  SELECT * FROM ankt INTO CORRESPONDING FIELDS OF TABLE gt_ankt.

  IF gt_anla IS INITIAL.
    WRITE : / 'No ANLA Data for output'.
  ENDIF.
  IF gt_anlz IS INITIAL.
    WRITE: / 'No ANLZ data for output'.
  ENDIF.
  IF gt_ankt IS INITIAL.
    WRITE: / 'No ANKT data for output'.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_data .

  SORT gt_anla BY bukrs anln1 anln2.
  SORT gt_anlz BY bukrs anln1. " anln2.
  SORT gt_ankt BY anlkl.
*****
  OPEN DATASET p_anla_f FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE: / 'Unable to open ANLA file for output ', p_anla_f.
    STOP.
  ENDIF.
  TRANSFER anla_header TO p_anla_f. " LENGTH recsize.
  LOOP AT gt_anla INTO gs_anla.
       gs_anla-sp1 = '~'.
       gs_anla-sp2 = '~'.
       gs_anla-sp3 = '~'.
       gs_anla-sp4 = '~'.
       gs_anla-sp5 = '~'.
       gs_anla-sp6 = '~'.
       gs_anla-sp7 = '~'.
       gs_anla-sp8 = '~'.
    TRANSFER gs_anla TO p_anla_f.
  ENDLOOP.
  CLOSE DATASET p_anla_f.
*****
  OPEN DATASET p_anlz_f FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE: / 'Unable to open ANLZ file for output ', p_anlz_f.
    STOP.
  ENDIF.
  TRANSFER anlz_header TO p_anlz_f.
  LOOP AT gt_anlz INTO gs_anlz.
       gs_anlz-sp1 = '~'.
       gs_anlz-sp2 = '~'.
       gs_anlz-sp3 = '~'.
       gs_anlz-sp4 = '~'.
       gs_anlz-sp5 = '~'.
       gs_anlz-sp6 = '~'.
       gs_anlz-sp7 = '~'.
       TRANSFER gs_anlz to p_anlz_f.
  ENDLOOP.
  CLOSE DATASET p_anlz_f.
*****
  OPEN DATASET p_ankt_f FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE: / 'Unable to open ANKT file for output ', p_ankt_f.
    STOP.
  ENDIF.
  TRANSFER ankt_header TO p_ankt_f.
  LOOP AT gt_ankt INTO gs_ankt.
       gs_ankt-sp1 = '~'.
       gs_ankt-sp2 = '~'.
       gs_ankt-sp3 = '~'.
       gs_ankt-sp4 = '~'.
       gs_ankt-sp5 = '~'.
       gs_ankt-sp6 = '~'.
    TRANSFER gs_ankt to p_ankt_f.
  ENDLOOP.
  CLOSE DATASET p_ankt_f.
*****
  WRITE: / p_anla_f.
  WRITE: / p_anlz_f.
  WRITE: / p_ankt_f.
  WRITE: / 'Process has been completed'.
ENDFORM.                    " OUTPUT_DATA
