FUNCTION zfi_ic_posted_docs.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_BLART) TYPE  BKPF-BLART
*"     VALUE(IM_GJAHR) TYPE  BKPF-GJAHR
*"     VALUE(IM_BUDATS) TYPE  BKPF-BUDAT
*"     VALUE(IM_BUDATE) TYPE  BKPF-BUDAT
*"     VALUE(IM_XBLNR) TYPE  BKPF-XBLNR OPTIONAL
*"  TABLES
*"      TB_BKPF STRUCTURE  BKPF
*"      TB_BSEG STRUCTURE  BSEG
*"      TB_PRPS STRUCTURE  PRPS
*"----------------------------------------------------------------------

  DATA: lv_pspnr TYPE prps-pspnr,
        lt_prps  LIKE table of tb_prps.

  IF im_xblnr IS NOT INITIAL.

    SELECT * FROM bkpf
      INTO CORRESPONDING FIELDS OF TABLE tb_bkpf
      WHERE blart = im_blart
        AND gjahr = im_gjahr
        AND budat >= im_budats
        AND budat <= im_budate
        AND xblnr = im_xblnr.
  ELSE.
    SELECT * FROM bkpf
      INTO CORRESPONDING FIELDS OF TABLE tb_bkpf
      WHERE blart = im_blart
        AND gjahr = im_gjahr
        AND budat >= im_budats
        AND budat <= im_budate.
  ENDIF. "im_xblnr is not initial

  IF tb_bkpf[] IS NOT INITIAL.
    SELECT * FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE tb_bseg
      FOR ALL ENTRIES IN tb_bkpf
      WHERE belnr = tb_bkpf-belnr
        AND bukrs = tb_bkpf-bukrs
        AND gjahr = tb_bkpf-gjahr.
  ENDIF. "tb_bkpf is not initial

  IF tb_bseg[] IS NOT INITIAL.
    LOOP AT tb_bseg.
      IF tb_bseg-projk IS NOT INITIAL.
        lv_pspnr = tb_bseg-projk.
        SELECT * FROM prps
          INTO CORRESPONDING FIELDS OF TABLE lt_prps
          WHERE pspnr = lv_pspnr.
        if sy-subrc = 0.
          append lines of lt_prps to tb_prps.
        endif.
      ENDIF. "lv_objnr is not initial
    ENDLOOP. "tb_bseg
  ENDIF. "tb_bseg is not initial
ENDFUNCTION.
