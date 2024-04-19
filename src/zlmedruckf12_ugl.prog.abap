*2009/01/20 TR580 mdemeest - Upgrade for ERP 6.0/SAPWeaver 7.00     UGL
*                           All changes indicated with UGL in       UGL
*                           rightmost column.                       UGL
*&---------------------------------------------------------------------*
*&      Form  READ_HEADER_DATA
*&---------------------------------------------------------------------*
* die Adressnummer vom Lieferanten oder vom Kunden wird temporär
* auch in das Feld pekko-adrnr gestellt und im Formular abgefragt
* falls pekko-adrnr und pekko-adrn2 initial sind werden die
* sadr-Felder ausgegeben
*----------------------------------------------------------------------*
FORM read_header_data.

*----------------------- Changes for ERP 6.0/SAPWeaver 7.00 -------- UGL
* Retrieve the description for the TERMS of Payment                  UGL

  select single * from T052U WHERE ZTERM = EKKO-ZTERM.              "UGL
*
*---------------------------- End of Changes ----------------------- UGL


* Lesen Überschrifttabellen -------------------------------------------*
  SELECT SINGLE * FROM t166u WHERE spras EQ ekko-spras
                             AND   druvo EQ xdruvo
                             AND   bstyp EQ ekko-bstyp
                             AND   bsart EQ ekko-bsart.

* Lesen Versandanschrift ( Lieferant/Lieferwerk )
  PERFORM read_address USING nast-parvw nast-parnr ekko.

* Lesen Werksanschrift -----------------------------------------------*
  IF pekko-werks NE space.
    PERFORM get_plant_address USING    pekko-werks
                              CHANGING pekko-adrnr sadr.
  ENDIF.

* Lesen Lieferantenanschrift ------------------------------------------*
  IF pekko-emlif NE space.
    PERFORM get_vendor_address USING     pekko-emlif
                               CHANGING  pekko-adrnr.
  ENDIF.

* Lesen Kundenanschrift
  IF  pekko-kunnr NE space.
    PERFORM get_customer_address USING     pekko-kunnr
                                 CHANGING  pekko-adrnr.
  ENDIF.

* Ausgabeformat Lieferdatum Kopfebene
  IF  pekko-eindt NE 0.
    CALL FUNCTION 'PERIOD_AND_DATE_CONVERT_OUTPUT'
      EXPORTING
        internal_date      = pekko-eindt
        internal_period    = pekko-lpein
        language           = ekko-spras
        country            = lfa1-land1
      IMPORTING
        external_date      = pekko-lfdat
        external_period    = pekko-lpein
        external_printtext = pekko-pritx.
  ENDIF.

*Lesen Einkäufergruppe  und Einkaufsorganisation----------------------*
  SELECT SINGLE * FROM t024 WHERE ekgrp EQ ekko-ekgrp.
  SELECT SINGLE * FROM t024e WHERE ekorg EQ ekko-ekorg.
  SELECT SINGLE * FROM tinct WHERE spras EQ ekko-spras
                             AND   inco1 EQ ekko-inco1.
  SELECT SINGLE * FROM t001 WHERE bukrs EQ ekko-bukrs.

* Aufbereiten Zahlungsbedingungen -------------------------------------*
  IF nast-kappl NE 'EL'.
    CLEAR t052.
    REFRESH zbtxt.
    IF ekko-zterm EQ space.
      t052-ztag1 = ekko-zbd1t.
      t052-zprz1 = ekko-zbd1p.
      t052-ztag2 = ekko-zbd2t.
      t052-zprz2 = ekko-zbd2p.
      t052-ztag3 = ekko-zbd3t.
    ENDIF.

    CALL FUNCTION 'FI_PRINT_ZTERM'
      EXPORTING
        i_zterm         = ekko-zterm
        i_langu         = ekko-spras
        i_xt052u        = 'X'
        i_t052          = t052
      TABLES
        t_ztext         = zbtxt
      EXCEPTIONS
        zterm_not_found = 01.

    READ TABLE zbtxt INDEX 1.
    IF sy-subrc EQ 0.
      pekko-zbtxt = zbtxt-line.
      DELETE zbtxt INDEX 1.
    ENDIF.
  ENDIF.

*Lesen Absagegrund
  IF ekko-absgr NE space.
    SELECT SINGLE * FROM t165m WHERE spras EQ ekko-spras
                               AND   absgr EQ ekko-absgr.
  ENDIF.
ENDFORM.                               " READ_HEADER_DATA
