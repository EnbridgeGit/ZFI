*&---------------------------------------------------------------------*
*&  Include           ZFGLI007_SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.
PARAMETERS: p_infile TYPE filenameci-fileextern OBLIGATORY.

SELECTION-SCREEN SKIP.

PARAMETERS: p_oufile TYPE filenameci-fileextern OBLIGATORY.

SELECTION-SCREEN SKIP.

PARAMETERS: p_erfile TYPE filenameci-fileextern OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-T03.
PARAMETERS: p_bukrs TYPE bkpf-bukrs OBLIGATORY DEFAULT 'UGL',
            p_blart TYPE bkpf-blart OBLIGATORY DEFAULT 'S8',
            P_waers TYPE bkpf-waers OBLIGATORY DEFAULT 'CAD',
            p_xblnr TYPE bkpf-xblnr OBLIGATORY DEFAULT 'CIS',
            p_bktxt TYPE bkpf-bktxt OBLIGATORY DEFAULT 'CIS - Interface Gas',
            p_meins TYPE bseg-meins OBLIGATORY DEFAULT 'M3',
            p_vkorg TYPE vkorg      OBLIGATORY DEFAULT 'Z001',
            p_group TYPE APQ_GRPN   OBLIGATORY,
            p_kunnr TYPE kna1-kunnr OBLIGATORY DEFAULT 'CIS_LUG',
            p_hkont TYPE hkont OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B3.

AT SELECTION-SCREEN ON BLOCK B1.
  DATA filename_len TYPE i.

  filename_len = strlen( p_infile ).
  filename_len = filename_len - 1.
  IF p_infile+filename_len(1)  = '/'.
    MESSAGE E100 WITH 'Please enter an input file name'.
  ENDIF.

  clear filename_len.
  filename_len = strlen( p_oufile ).
  filename_len = filename_len - 1.
  IF p_oufile+filename_len(1)  = '/'.
    MESSAGE E100 WITH 'Please enter an output file name'.
  ENDIF.

  clear filename_len.
  filename_len = strlen( p_erfile ).
  filename_len = filename_len - 1.
  IF p_erfile+filename_len(1)  = '/'.
    MESSAGE E100 WITH 'Please enter an error file name'.
  ENDIF.

AT SELECTION-SCREEN ON p_bukrs.
  SELECT SINGLE * FROM T001 WHERE bukrs = p_bukrs.
    IF sy-subrc <> 0.
      MESSAGE E100 WITH 'Invalid company code' p_bukrs.
    ENDIF.

AT SELECTION-SCREEN ON p_blart.
  SELECT SINGLE * FROM T003 WHERE blart = p_blart AND xsybl = 'X'.
    IF sy-subrc <> 0.
      MESSAGE E100 WITH 'Invalid document type' p_blart.
    ENDIF.

AT SELECTION-SCREEN ON p_waers.
  SELECT SINGLE * FROM TCURC WHERE waers = p_waers.
    IF sy-subrc <> 0.
      MESSAGE E100 WITH 'Invalid currency code' p_waers.
    ENDIF.

AT SELECTION-SCREEN ON p_meins.
  SELECT SINGLE * FROM T006 WHERE msehi = p_meins.
    IF sy-subrc <> 0.
      MESSAGE E100 WITH 'Invalid UoM' p_meins.
    ENDIF.
