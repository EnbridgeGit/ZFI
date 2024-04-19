FUNCTION ZPS_WBS_VALIDATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CLIKE
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CLIKE
*"     VALUE(PSPNR) TYPE  PS_POSNR
*"     VALUE(OBJNR) TYPE  J_OBJNR
*"     VALUE(BAPIRET2) TYPE  BAPIRET2
*"  EXCEPTIONS
*"      WBS_TYPE_INVALID
*"      WBS_FORMAT_INVALID
*"      WBS_DOES_NOT_EXIST
*"----------------------------------------------------------------------
* Copy from US System as need as part of ZAP_INVOICE_POST
*----------------------------------------------------------------------*

  FIELD-SYMBOLS: <MASK>.

  DATA:
     lv_pspnr TYPE ps_posnr,
     lv_objnr TYPE j_objnr,
     MSG_TYPE VALUE 'E',
     MASKE LIKE PRPS-POSID,
     BEGIN OF INTTAB OCCURS 10,
       POS LIKE SY-FDPOS,
       FORMAT(1),
     END OF INTTAB.

*=====================================================================*
  CLEAR FLAG_EXIT.
*.ausstieg falls nummer initial.......................................*
  MOVE INPUT TO MASKE.
  IF MASKE IS INITIAL.
    EXIT.
  ENDIF.

  CLEAR   OUTPUT.
  CLEAR   PSPNR.
  CLEAR   BAPIRET2.

*  PERFORM CJPN_PROJEKTNUMMER_UNEDIT CHANGING INPUT
*                                             MSG_TYPE
*                                             OUTPUT
*                                             BAPIRET2.
*
*  IF       ( bapiret2-type   IS NOT INITIAL ).
*    IF   ( ( bapiret2-type   EQ 'CJ'        ) AND
*           ( bapiret2-number EQ '011'       )     ).
*      RAISE WBS_TYPE_INVALID.
*    ELSE.
*      RAISE WBS_FORMAT_INVALID.
*    ENDIF.
*  ENDIF.

  CLEAR    lv_pspnr.
  CLEAR    lv_objnr.
  SELECT   SINGLE pspnr objnr
    INTO  (lv_pspnr, lv_objnr)
    FROM   prps
   WHERE   posid = output.
  IF     ( sy-subrc NE 0 ).
    RAISE WBS_DOES_NOT_EXIST.
  ENDIF.

  PSPNR = lv_pspnr.
  OBJNR = lv_objnr.



ENDFUNCTION.
