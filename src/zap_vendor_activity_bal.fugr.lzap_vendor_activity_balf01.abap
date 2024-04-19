*----------------------------------------------------------------------*
***INCLUDE LZAP_VENDOR_ACTIVITY_BALF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           LZAP_VENDOR_ACTIVITY_BALF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LFA1>  text
*      -->P_DMBTR  text
*      -->P_SHKZG  text
*      <--P_<OUTPUT>  text
*----------------------------------------------------------------------*
FORM assign_values  USING    p_dmbtr  TYPE any
                             p_shkzg  TYPE any
                             p_waers  TYPE any
                             p_lifnr  TYPE any
                             p_plifnr TYPE any.

  FIELD-SYMBOLS: <lfa1>   LIKE LINE OF gt_lfa1.

  IF p_lifnr EQ p_plifnr. "Same vendor as previous loop pass vendor
    IF p_shkzg = 'H'.
      <output>-credit_tot = <output>-credit_tot + p_dmbtr.
    ELSEIF p_shkzg = 'S'.
      <output>-debit_tot  = <output>-debit_tot + p_dmbtr.
    ENDIF.
  ELSE. "New Vendor
    READ TABLE gt_lfa1 ASSIGNING <lfa1>
                       WITH KEY lifnr = p_lifnr
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      APPEND INITIAL LINE TO gt_output ASSIGNING <output>.
      <output>-lifnr    = <lfa1>-lifnr.
      <output>-name1    = <lfa1>-name1.
      <output>-stras    = <lfa1>-stras.
      <output>-ort01    = <lfa1>-ort01.
      <output>-regio    = <lfa1>-regio.
      <output>-land1    = <lfa1>-land1.
      <output>-date     = <lfa1>-date.
      <output>-ktokk    = <lfa1>-ktokk.
      <output>-currency = p_waers.
      IF p_shkzg = 'H'.
        <output>-credit_tot = p_dmbtr.
      ELSEIF p_shkzg = 'S'.
        <output>-debit_tot  = p_dmbtr.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ASSIGN_VALUES

*&---------------------------------------------------------------------*
*&      Form  GET_PO_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_po_details USING p_cpudt TYPE acc_t_ra_date.

  DATA: lt_po      TYPE STANDARD TABLE OF ty_po,
        lt_ekbe_e  TYPE STANDARD TABLE OF ty_ekbe_e,
        lt_ekbe_q  TYPE STANDARD TABLE OF ty_ekbe_q,
        lv_lifnr   TYPE lifnr,
        lv_ebeln   TYPE ebeln,
        lv_tabix   TYPE sytabix,
        lv_dmbtr   TYPE dmbtr,
        lv_menge   TYPE ekpo-menge,
        lv_brtwr   TYPE ekpo-brtwr,
        lv_lines   TYPE i.

  FIELD-SYMBOLS: <ekko_ekpo> LIKE LINE OF lt_po,
                 <ekbe_e> LIKE LINE OF lt_ekbe_e,
                 <ekbe_q> LIKE LINE OF lt_ekbe_q,
                 <lfa1> LIKE LINE OF gt_lfa1.

  TYPES: BEGIN OF ty_ebeln,
         ebeln TYPE ebeln,
       END OF ty_ebeln.
  DATA: lt_ebeln TYPE STANDARD TABLE OF ty_ebeln,
        ls_ebeln LIKE LINE OF lt_ebeln.

  SELECT a~lifnr a~ebeln b~ebelp a~aedat b~brtwr b~menge
                            INTO TABLE lt_po
                            FROM ekko AS a
                            INNER JOIN ekpo AS b
                            ON a~ebeln EQ b~ebeln
                            FOR ALL ENTRIES IN gt_lfa1
                            WHERE a~lifnr EQ gt_lfa1-lifnr
                            AND   a~aedat IN p_cpudt.
  IF sy-subrc EQ 0.
    SORT lt_po.
    SELECT ebeln ebelp zekkn vgabe gjahr belnr buzei bewtp menge
                                   INTO TABLE lt_ekbe_e
                                   FROM ekbe
                                   FOR ALL ENTRIES IN lt_po
                                   WHERE ebeln EQ lt_po-ebeln
                                   AND   ebelp = lt_po-ebelp
                                   AND   bewtp = 'E'.

    SELECT ebeln ebelp zekkn vgabe gjahr belnr buzei bewtp dmbtr INTO TABLE lt_ekbe_q
                                   FROM ekbe
                                   FOR ALL ENTRIES IN lt_po
                                   WHERE ebeln = lt_po-ebeln
                                   AND   bewtp = 'Q'.
  ENDIF.

  lv_lines = lines( lt_po ).
  LOOP AT lt_po ASSIGNING <ekko_ekpo>.
    lv_tabix = sy-tabix.
    IF lv_lifnr NE <ekko_ekpo>-lifnr.
      IF lv_menge IS NOT INITIAL.
        <output>-po_bal_qty = lv_menge - <output>-po_bal_qty.
        CLEAR: lv_menge.
      ENDIF.

      IF <output>-po_tot IS ASSIGNED.
        <output>-po_bal_amt = <output>-po_tot - <output>-po_bal_amt.
      ENDIF.

      READ TABLE gt_lfa1 ASSIGNING <lfa1>
                         WITH KEY lifnr = <ekko_ekpo>-lifnr
                         BINARY SEARCH.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO gt_output ASSIGNING <output>.
        <output>-lifnr    = <lfa1>-lifnr.
        <output>-name1    = <lfa1>-name1.
        <output>-stras    = <lfa1>-stras.
        <output>-ort01    = <lfa1>-ort01.
        <output>-regio    = <lfa1>-regio.
        <output>-land1    = <lfa1>-land1.
        <output>-date     = <lfa1>-date.
        <output>-ktokk    = <lfa1>-ktokk.
        <output>-po_tot   = <ekko_ekpo>-brtwr.
        lv_menge          = <ekko_ekpo>-menge.

      ENDIF.
    ELSE. "New Vendor
      <output>-po_tot   = <output>-po_tot + <ekko_ekpo>-brtwr.
      lv_menge = lv_menge + <ekko_ekpo>-menge.
    ENDIF.

    LOOP AT lt_ekbe_e ASSIGNING <ekbe_e>
                      WHERE ebeln EQ <ekko_ekpo>-ebeln
                      AND   ebelp EQ <ekko_ekpo>-ebelp.

      <output>-po_bal_qty = ( <output>-po_bal_qty + <ekbe_e>-menge ).
    ENDLOOP.

    LOOP AT lt_ekbe_q ASSIGNING <ekbe_q>
                      WHERE ebeln EQ <ekko_ekpo>-ebeln
                      AND   ebelp EQ <ekko_ekpo>-ebelp.
      lv_dmbtr = <ekbe_q>-dmbtr.
      <output>-po_bal_amt = <output>-po_bal_amt + lv_dmbtr.
    ENDLOOP.

    lv_lifnr = <ekko_ekpo>-lifnr.

    IF lv_lines EQ lv_tabix.
      IF lv_menge IS NOT INITIAL.
        <output>-po_bal_qty = lv_menge - <output>-po_bal_qty.
        CLEAR: lv_menge.
      ENDIF.

      <output>-po_bal_amt = <output>-po_tot - <output>-po_bal_amt.

    ENDIF.
  ENDLOOP.


ENDFORM.                    " GET_PO_DETAILS
