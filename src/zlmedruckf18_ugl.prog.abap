*&---------------------------------------------------------------------*
*&      Form  PRINT_ITEM_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
* Im Neu- und Änderungsdruck werden die Bestellmenge ohne
* Berücksichtigung von WE's ausgegeben.
* Im Mahndruck wird dagegen die offnen Menge ( menge - wemng )
* berücksichtigt.

* docu: =============
* docu: PRINT_ITEM_PO
* docu: =============

FORM print_item_po
         TABLES t_xekpo STRUCTURE ekpo
                t_xeket STRUCTURE eket
                t_xekkn STRUCTURE ekkn
                t_xaend STRUCTURE xaend
         USING zz TYPE i
               p_mflag
               p_from_memory.


* docu: initialize
  DATA: f1 TYPE f,
        h_subrc LIKE sy-subrc,
        h_tabix LIKE sy-tabix,
        lt_lines TYPE i,                                    "802359
        l_elementn TYPE c VALUE space,                      "802359
        l_xeket TYPE STANDARD TABLE OF eket,                "802359
        s_ekpo LIKE ekpo,
        l_ekkn LIKE ekkn,
        srv_quantity LIKE esll-menge,                       "737535
        p_chngind LIKE t_xaend-chngind.

  DATA: BEGIN OF t_xekeh OCCURS 0.
          INCLUDE STRUCTURE ekeh.
  DATA: END OF t_xekeh.

  REFRESH t_xekeh.

  s_ekpo = ekpo.
* docu: Positionszeile
*- In den Positionszeilen kein Seitenumbruch ---------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'PROTECT'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

  IF ekpo-uebpo NE space.
    IF sekpo-ebelp EQ ekpo-uebpo AND sekpo-first_varpos NE ekpo-uptyp.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_HEADER_VATNR_1'
        EXCEPTIONS
          OTHERS  = 01.
      sekpo-first_varpos = ekpo-uptyp.
    ENDIF.
  ENDIF.

* docu: Positionszeilen Anfrage
  IF ekko-bstyp EQ bstyp-anfr.
    IF ekpo-pstyp NE 6.                                     "789825
      PERFORM menge_ausgeben USING ekpo-ktmng t006-decan rm06p-prmg1.
    ENDIF.                                                  "789825
    PERFORM set_timeflag TABLES t_xeket
                         CHANGING rm06p-phtx2.              "497280
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'ITEM_LINE_A'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ELSE.
* positionieren für Matrixdruck ---------------------------------------*
    IF ( ekpo-uptyp EQ uptyp-var OR ekpo-uptyp EQ uptyp-lot ) AND
                                  p_mflag NE space AND zz >= 1.

      LOOP AT xpekpov WHERE uebpo EQ ekpo-uebpo AND ebelp IS INITIAL.
        MOVE xpekpov TO pekpov.
      ENDLOOP.
    ENDIF.

* docu: Erste Positionszeile bei Bestellung und Rahmenvertrag
    IF ekpo-uptyp NE uptyp-var AND
       ekpo-uptyp NE uptyp-mpn AND
       ekpo-uptyp NE uptyp-gti AND                          " 622794
       ekpo-uptyp NE uptyp-sls.

*----------------------- Start ERP 6.0/SAPWeaver 7.00 ---------- UGL

      IF tnapr-kschl = 'LPMA'.                                      "UGL
        "Processing scheduling doc reminder                       "UGL
        "Calculating quantity overdue                             "UGL
        eket-dabmg = rm06p-prmg2 - eket-wemng.                    "UGL
      ELSE.                                                         "UGL
        "Calculating QUANTITY OUTSTANDING                         "UGL
      ENDIF.                                                        "UGL

      SELECT SINGLE * FROM eket WHERE ebeln = ekpo-ebeln            "UGL
                                  AND ebelp = ekpo-ebelp            "UGL
                                  AND etenr = '0001'.               "UGL
      "find out invoice to address                                  "UGL
      SELECT SINGLE * FROM t001w WHERE werks = 'P112'.              "UGL

      CALL FUNCTION 'WRITE_FORM'                                    "UGL
            EXPORTING                                               "UGL
              element = 'ADDRESS'                                   "UGL
              window  = 'INVTO'                                     "UGL
            EXCEPTIONS                                              "UGL
              OTHERS  = 01.                                         "UGL
*
*---------------------- End of Changes ------------------------  UGL


      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_LINE_1'
        EXCEPTIONS
          OTHERS  = 01.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_LINE_1_VAR'
        EXCEPTIONS
          OTHERS  = 01.
      CLEAR sy-subrc.
      CLEAR econf_out.
      LOOP AT tconf_out WHERE ebelp EQ ekpo-ebelp.
        MOVE tconf_out TO econf_out.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_LINE_1_VAR_CONF_OUT'
          EXCEPTIONS
            OTHERS  = 01.
        CLEAR sy-subrc.
      ENDLOOP.
    ENDIF.
    CLEAR sy-subrc.
* docu: Positionszeile ohne Preis
    IF ekpo-meins NE space.
      IF ekpo-netpr EQ 0 OR
         ekpo-prsdr EQ space OR
         ekpo-bprme NE ekpo-meins.
        IF ekko-bstyp EQ bstyp-best.
          PERFORM menge_ausgeben USING ekpo-menge t006-decan
                                       rm06p-prmg1.
* Zusatz für Bestätigungen , bereits bestätigte Menge ausgeben
          CLEAR rm06p-prmg2.
          IF xdruvo EQ aufb AND pekpo-bsmng LT ekpo-menge
                             AND pekpo-bsmng GT 0 AND
              ( ekpo-netpr EQ 0 OR ekpo-prsdr EQ space ).
            PERFORM menge_ausgeben USING pekpo-bsmng *t006-decan
                                     rm06p-prmg2.
          ENDIF.

          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'ITEM_LINE_2F'
            EXCEPTIONS
              OTHERS  = 01.
          CLEAR sy-subrc.
        ELSE.
          PERFORM menge_ausgeben USING ekpo-ktmng t006-decan
                                       rm06p-prmg1.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'ITEM_LINE_2R'
            EXCEPTIONS
              OTHERS  = 01.
          CLEAR sy-subrc.
        ENDIF.
      ENDIF.
    ENDIF.
* docu: Positionszeile mit Preis
    IF ekpo-netpr NE 0 AND
       ekpo-prsdr NE space.
      s_ekpo = ekpo.          "wg menge und netpr und netwr
* Menge in Bestellpreismengeneinheit ----------------------------------*
      IF ekpo-bprme NE ekpo-meins.
        IF ekko-bstyp EQ bstyp-best.
          ekpo-menge = f1 = ekpo-menge * ekpo-bpumz / ekpo-bpumn.
        ELSE.
          ekpo-ktmng = f1 = ekpo-ktmng * ekpo-bpumz / ekpo-bpumn.
        ENDIF.
      ENDIF.
      IF ekko-bstyp EQ bstyp-best.
        PERFORM menge_ausgeben USING ekpo-menge *t006-decan
                                     rm06p-prmg1.
* Zusatz für Bestätigungen , bereits bestätigte Menge ausgeben
        CLEAR rm06p-prmg2.
        IF xdruvo EQ aufb AND pekpo-bsmng LT s_ekpo-menge
                          AND pekpo-bsmng GT 0.
          PERFORM menge_ausgeben USING pekpo-bsmng *t006-decan
                                       rm06p-prmg2.
        ENDIF.
* Preise ausgeben
        READ TABLE tkomvd INDEX 1.
        IF sy-subrc EQ 0 OR
           ( ekpo-uebpo NE space AND pekpov-xkondv EQ space ) OR
           ekpo-uptyp EQ uptyp-lot.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'ITEM_LINE_3F'
            EXCEPTIONS
              OTHERS  = 01.
          CLEAR sy-subrc.
        ELSE.
          IF ( ekpo-attyp NE attyp-sam AND ekpo-attyp NE attyp-lot )
             OR pekpov-menge EQ 0 OR
             ekpo-upvor EQ space.
            CALL FUNCTION 'WRITE_FORM'
              EXPORTING
                element = 'ITEM_LINE_3F_PRICE'
              EXCEPTIONS
                OTHERS  = 01.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
              EXPORTING
                element = 'ITEM_LINE_3F'
              EXCEPTIONS
                OTHERS  = 01.
            CLEAR sy-subrc.
* Menge in Bestellpreismengeneinheit ----------------------------------*
            IF ekpo-bprme NE ekpo-meins.
* pekpov-menge in bprme, da ekpo-menge auch in bprme
              pekpov-menge = f1 =
               pekpov-menge * ekpo-bpumz / ekpo-bpumn.
            ENDIF.
            PERFORM menge_ausgeben USING
                    pekpov-menge t006-decan pekpov-prmng.
            rm06p-prmg1 = pekpov-prmng.
            ekpo-netwr = ekpo-netwr * pekpov-menge / ekpo-menge.
            CALL FUNCTION 'WRITE_FORM'
              EXPORTING
                element = 'ITEM_LINE_3F_PRICE'
              EXCEPTIONS
                OTHERS  = 01.
          ENDIF.
          CLEAR sy-subrc.
        ENDIF.
      ELSE.
        PERFORM menge_ausgeben USING ekpo-ktmng *t006-decan
                                     rm06p-prmg1.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_LINE_3R'
          EXCEPTIONS
            OTHERS  = 01.
        CLEAR sy-subrc.
      ENDIF.
      ekpo = s_ekpo.
    ENDIF.
  ENDIF.

*ENHANCEMENT-POINT LMEDRUCKF18_01 SPOTS ES_SAPLMEDRUCK.
* docu: Ausgeben Lieferdatum pro Position
*  IF pekpo-eindt NE 0 AND                                  " 960022
  IF ekko-bstyp NE bstyp-anfr AND                           " 960022
    ekko-bstyp NE bstyp-anfr AND
    ekko-bstyp NE bstyp-lfpl.
    IF s_ekpo-pstyp NE pstyp-dien.                          " 549464
      IF pekpo-wemng GT 0 AND ekpo-elikz EQ space.          " 960022
* Quantity to be urged should be the difference bewteen confirmed
* quantity and delivered quantity                           "581492
        CASE xdruvo.                                        "581492
          WHEN mahn OR lpma.                                "581492
            CLEAR pekpo-wemng.                              "1049315
            LOOP AT t_xeket WHERE ebelp = pekpo-ebelp.      "581492
              pekpo-wemng = pekpo-wemng + t_xeket-menge.    "1049315
            ENDLOOP.                                        "581492
          WHEN OTHERS.                                      "581492
            pekpo-wemng = s_ekpo-menge - pekpo-wemng.       "581492
        ENDCASE.                                            "581492
        PERFORM menge_ausgeben USING pekpo-wemng t006-decan rm06p-prmg2.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_INFO_WEMNG'
          EXCEPTIONS
            OTHERS  = 01.
        CLEAR sy-subrc.
      ENDIF.                                                "#306670
*   ELSE.                                                   " 549464
* An ELSE-path is not necessary, as for services the EKPO-MENGE
* will always be 1, WEMNG will reflect the number of entry sheets
* entered for this item. So a comparison between MENGE and WEMNG
* is completely useless for services
    ENDIF.                                                  " 549464

    IF pekpo-eindt NE 0 AND                                 " 960022
       ekko-bstyp NE bstyp-anfr AND                         " 960022
       ekko-bstyp NE bstyp-lfpl.                            " 960022


      PERFORM set_timeflag TABLES t_xeket
                           CHANGING rm06p-phtx2.
      PERFORM time_into_printform.
      PERFORM mahntext USING eket-mahnz.
      IF ( NOT rm06p-pritx IS INITIAL ) OR
           ( NOT rm06p-lfdat IS INITIAL ) OR
           ( NOT rm06p-phtxt IS INITIAL  ).
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_INFO_DATE'
          EXCEPTIONS
            OTHERS  = 01.
        CLEAR sy-subrc.
      ENDIF.
    ENDIF.                                                  " 960022
  ENDIF.

* Lieferdatum zusammen mit den Positionszeilen drucken-----------------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ENDPROTECT'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

* docu: MPN-Abwicklung
  PERFORM htnmat_print.

  CLEAR econf_out.
  LOOP AT tconf_out WHERE ebelp EQ ekpo-ebelp.
    MOVE tconf_out TO econf_out.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'ITEM_INFO_CONF_OUT'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ENDLOOP.

* docu: Ausgeben Positionsinfo-Fenster
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'ITEM_INFO_2'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

  IF NOT ekvkp IS INITIAL.
* Ausgeben Fenster mit VKP-Informationen (IS-R)
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'ITEM_INFO_VKP'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ENDIF.

  IF ekpo-uptyp NE uptyp-lot.
* docu: Positionstexte ausgeben
    CLEAR sy-subrc.
    IF xdruvo EQ aend OR xdruvo EQ lpae.
      LOOP AT t_xaend WHERE ebelp EQ ekpo-ebelp
                    AND   text_case EQ 'X'.
        EXIT.
      ENDLOOP.
    ENDIF.
    IF sy-subrc EQ 0.
      LOOP AT xt166p.
        MOVE xt166p TO t166p.
        IF t166p-tdobject EQ 'EKPO'.
          t166p-txnam(10) = ekko-ebeln.
          t166p-txnam+10(5) = ekpo-ebelp.
        ENDIF.
        PERFORM lesen_ttxit USING xt166p-titdr
                                  xt166p-tdobject xt166p-tdid.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_TEXT'
          EXCEPTIONS
            OTHERS  = 01.
        CLEAR sy-subrc.
      ENDLOOP.
    ENDIF.

* docu: QM-Texte ausgeben
    LOOP AT qm_text_key.
      thead-tdspras  = ekko-spras.
      thead-tdname   = qm_text_key-tdname.
      thead-tdobject = qm_text_key-tdobject.
      thead-tdid     = qm_text_key-tdid.
      ttxit-tdtext   = qm_text_key-tdtext.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'QM_TEXT'
        EXCEPTIONS
          OTHERS  = 01.
      CLEAR sy-subrc.
    ENDLOOP.

* docu: Ausgeben Hinweis
*       , daß zu jeder Lieferung ein Zeugnis erwartet wird--*
    IF ekpo-zgtyp NE space AND zg_kz NE space AND
       ( xdruvo EQ neu OR
         xdruvo EQ aend ).
*  Interpretieren Zeugnistyp
      SELECT SINGLE * FROM tq05 WHERE zgtyp EQ ekpo-zgtyp.
      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM tq05t WHERE sprache EQ ekko-spras
                                   AND   zgtyp   EQ ekpo-zgtyp.
        IF tq05-zglief NE space.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'ITEM_INFO_QM_1'
            EXCEPTIONS
              OTHERS  = 01.
        ELSE.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'ITEM_INFO_QM_2'
            EXCEPTIONS
              OTHERS  = 01.
        ENDIF.
      ENDIF.
      CLEAR sy-subrc.
    ENDIF.

* docu: Ausgeben Mehrere Liefertermine
    h_subrc = 0.
    IF ekko-bstyp = bstyp-kont.
      CALL FUNCTION 'ME4S_CONTRACT_TYPE_WITH_SCHED'
        EXPORTING
          ktype  = ekko-bsart
        IMPORTING
          return = h_subrc.
    ENDIF.
    IF h_subrc     = 0           AND
       ekko-bstyp  NE bstyp-lfpl.                           "802359
*Loop to find schedule lines for the current item.          "802359
*l_elementn to CHECK PARTIALLY DELIVERED                    "802359
      CLEAR: l_elementn,
             lt_lines.
      LOOP AT t_xeket WHERE ebeln = ekpo-ebeln AND
                            ebelp = ekpo-ebelp.
        APPEND t_xeket TO l_xeket.
*        IF t_xeket-wemng GT 0.
        IF t_xeket-wemng GT 0 AND                           "932937
           ekpo-pstyp NE pstyp-dien.                        "932937
          l_elementn = 'X'.
        ENDIF.
      ENDLOOP.
      DESCRIBE TABLE l_xeket LINES lt_lines.
      REFRESH l_xeket.

      READ TABLE t_xeket WITH KEY ebeln = ekpo-ebeln
                                ebelp = ekpo-ebelp.
*   CHECK IF PARTIALLY DELIVERED, MULTIPLE SCHEDULE LINES
*      IF sy-subrc EQ 0 AND ( l_elementn EQ 'X' OR lt_lines GT 1 OR
*t_xeket-etenr GT 1 ).                                       "802359
      IF sy-subrc IS INITIAL AND lt_lines GT 1.             "932937
        IF ekpo-pstyp NE pstyp-text.                        "802359
          IF xdruvo EQ neu OR
             ekko-bstyp EQ bstyp-anfr.
            elementn = 'ITEM_SCHEDULE_HEADER_NEW'.
          ELSE.
            elementn = 'ITEM_SCHEDULE_HEADER'.
          ENDIF.
* Muß Text 'Uhrzeit' ausgegeben werden? -------------------------------*
          PERFORM set_timeflag TABLES t_xeket
                               CHANGING etuhrtxt.
* kein Seitenumbruch bei Einteilungen ---------------------------------*
          xprotect = 'X'.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'PROTECT'
            EXCEPTIONS
              OTHERS  = 01.
          CLEAR sy-subrc.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = elementn
            EXCEPTIONS
              OTHERS  = 01.
          CLEAR sy-subrc.

          LOOP AT t_xeket WHERE ebeln EQ ekpo-ebeln AND
                              ebelp EQ ekpo-ebelp.
            IF sy-subrc EQ 0.                               "802359
              MOVE t_xeket TO eket.
              IF NOT ( xdruvo EQ lpma OR xdruvo EQ mahn ).
                eket-menge = eket-menge - eket-wemng.
              ENDIF.
              CHECK eket-menge GT 0.
         PERFORM menge_ausgeben USING eket-menge t006-decan rm06p-prmg1.
* Mahntext ------------------------------------------------------------*
              PERFORM mahntext USING eket-mahnz.
              PERFORM time_into_printform.
              IF timeflag NE space.
                SHIFT rm06p-phtxt RIGHT BY 7 PLACES.
                rm06p-phtxt(5) = pekpo-tprin.
              ENDIF.
* Lieferdatum in Druckdarstellung -------------------------------------*
              CLEAR: pekpo-lpein, rm06p-lfdat, rm06p-pritx.
              CALL FUNCTION 'PERIOD_AND_DATE_CONVERT_OUTPUT'
                EXPORTING
                  internal_date      = eket-eindt
                  internal_period    = eket-lpein
                  language           = ekko-spras
                  country            = lfa1-land1
                IMPORTING
                  external_date      = rm06p-lfdat
                  external_period    = pekpo-lpein
                  external_printtext = rm06p-pritx.
* Lieferdatum in Wochen- oder Monatsdarstellung -----------------------*
              IF rm06p-pritx NE space.
                CALL FUNCTION 'WRITE_FORM'
                  EXPORTING
                    element = 'ITEM_SCHEDULE'
                  EXCEPTIONS
                    OTHERS  = 01.
                CLEAR sy-subrc.
* Lieferdatum tagesgenau ----------------------------------------------*
              ELSE.
                CALL FUNCTION 'WRITE_FORM'
                  EXPORTING
                    element = 'ITEM_SCHEDULE_DAY'
                  EXCEPTIONS
                    OTHERS  = 01.
                CLEAR sy-subrc.
              ENDIF.
* Seitenumbruch nach 1. Einteilung möglich ----------------------------*
              IF xprotect NE space.
                CLEAR xprotect.
                CALL FUNCTION 'WRITE_FORM'
                  EXPORTING
                    element = 'ENDPROTECT'
                  EXCEPTIONS
                    OTHERS  = 01.
                CLEAR sy-subrc.
* Überschrift Einteilungen - Folgeseiten -----------------------------*
                CALL FUNCTION 'WRITE_FORM'
                  EXPORTING
                    element  = elementn
                    type     = 'TOP'
                    function = 'APPEND'
                  EXCEPTIONS
                    OTHERS   = 01.
                CLEAR sy-subrc.
              ENDIF.
            ENDIF.                                          "802359
          ENDLOOP.
        ENDIF.                                              "802359

* Überschrift Einteilungen - Folgeseiten löschen ---------------------*
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element  = elementn
            function = 'DELETE'
            type     = 'TOP'
          EXCEPTIONS
            OTHERS   = 01.
        CLEAR sy-subrc.
      ENDIF.
    ENDIF.

* docu: Ausgeben Komponenten
    IF ekko-bstyp EQ bstyp-best AND
       ekpo-pstyp EQ pstyp-lohn.
      PERFORM ausgabe_comp TABLES t_xeket t_xekeh.
    ENDIF.

* docu: Ausgeben Konditionen
    IF ekko-bstyp NE bstyp-kont AND ekko-stako EQ space.
      IF ( s_ekpo-attyp EQ attyp-sam OR s_ekpo-attyp EQ attyp-lot ) AND
         p_mflag NE space AND zz >= 1 AND s_ekpo-upvor NE space.
        LOOP AT t_xekpo WHERE ebelp EQ s_ekpo-ebelp
                          AND attyp EQ attyp-sam.
          LOOP AT tkomvd.
            IF sy-tabix EQ 1.
              IF t_xekpo-uptyp EQ uptyp-var.
                CALL FUNCTION 'WRITE_FORM'
                  EXPORTING
                    element = 'ITEM_INFO_VAR_COND'
                  EXCEPTIONS
                    OTHERS  = 01.
              ELSE.
               IF t_xekpo-attyp EQ attyp-sam AND t_xekpo-upvor NE space.
                  CALL FUNCTION 'WRITE_FORM'
                    EXPORTING
                      element = 'ITEM_INFO_SATNR_COND'
                    EXCEPTIONS
                      OTHERS  = 01.
                ENDIF.
              ENDIF.
              CLEAR sy-subrc.
            ENDIF.
            komvd = tkomvd.
            IF komvd-kpein IS INITIAL.
              CALL FUNCTION 'WRITE_FORM'
                EXPORTING
                  element = 'ITEM_CONDITIONS'
                EXCEPTIONS
                  OTHERS  = 01.
              CLEAR sy-subrc.
            ELSE.
              CALL FUNCTION 'WRITE_FORM'
                EXPORTING
                  element = 'ITEM_CONDITIONS_UNIT'
                EXCEPTIONS
                  OTHERS  = 01.
              CLEAR sy-subrc.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ELSE.
        LOOP AT tkomvd.
          IF sy-tabix EQ 1.
            IF ekpo-uptyp EQ uptyp-var.
              CALL FUNCTION 'WRITE_FORM'
                EXPORTING
                  element = 'ITEM_INFO_VAR_COND'
                EXCEPTIONS
                  OTHERS  = 01.
            ELSE.
              IF ekpo-attyp EQ attyp-sam AND ekpo-upvor NE space.
                CALL FUNCTION 'WRITE_FORM'
                  EXPORTING
                    element = 'ITEM_INFO_SATNR_COND'
                  EXCEPTIONS
                    OTHERS  = 01.
              ENDIF.
            ENDIF.
            CLEAR sy-subrc.
          ENDIF.
          komvd = tkomvd.
          IF komvd-kpein IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
              EXPORTING
                element = 'ITEM_CONDITIONS'
              EXCEPTIONS
                OTHERS  = 01.
            CLEAR sy-subrc.
          ELSE.
            CALL FUNCTION 'WRITE_FORM'
              EXPORTING
                element = 'ITEM_CONDITIONS_UNIT'
              EXCEPTIONS
                OTHERS  = 01.
            CLEAR sy-subrc.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      PERFORM ausgabe_stammkonditionen.
    ENDIF.

* docu: Ausgeben Auftragsbestätigungspflicht, wenn gesetzt
    IF ekpo-kzabs NE space AND
       ekpo-labnr EQ space AND
       pekko-kzabs EQ space AND
       pekko-labnr EQ space AND
       xdruvo NE aufb.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_INFO_ACKNOW'
        EXCEPTIONS
          OTHERS  = 01.
      CLEAR sy-subrc.
    ENDIF.

* docu: Ausgeben Werksanschrift pro Position
    IF pekko-adrnr IS INITIAL AND
       pekko-adrn2 IS INITIAL AND
       pekko-emlif IS INITIAL AND
       pekko-kunnr IS INITIAL AND
       pekko-werks IS INITIAL.
      IF NOT ( ekpo-adrnr IS INITIAL
               AND ekpo-adrn2 IS INITIAL
               AND sadr IS INITIAL ).
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_DELADDRESS'
          EXCEPTIONS
            OTHERS  = 01.
      ENDIF.
      CLEAR sy-subrc.
    ENDIF.
    IF ( NOT pekpo-wempf IS INITIAL OR NOT pekpo-ablad IS INITIAL )
          AND ( pekko-wempf IS INITIAL AND pekko-ablad IS INITIAL ).
      CLEAR ekkn.
      ekkn-wempf = pekpo-wempf.
      ekkn-ablad = pekpo-ablad.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_RECIEVER'
        EXCEPTIONS
          OTHERS  = 01.
    ENDIF.
    IF pekko-wempf IS INITIAL AND pekko-ablad IS INITIAL AND
       pekpo-wempf IS INITIAL AND pekpo-ablad IS INITIAL AND
       NOT t_xekkn[] IS INITIAL.
      h_tabix = 1.
      LOOP AT t_xekkn INTO l_ekkn
                      WHERE ebeln EQ ekpo-ebeln
                      AND   ebelp EQ ekpo-ebelp
                      AND ( ablad NE space OR wempf NE space ).
        IF h_tabix EQ 1.
          h_tabix = 2.
          CLEAR ekkn.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'ITEM_RECIEVER'
            EXCEPTIONS
              OTHERS  = 01.
        ENDIF.
        ekkn = l_ekkn.
        IF NOT ekkn-menge IS INITIAL.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'ITEM_RECIEVER_WQ'
            EXCEPTIONS
              OTHERS  = 01.
        ELSEIF NOT ekkn-vproz IS INITIAL.
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              element = 'ITEM_RECIEVER_WP'
            EXCEPTIONS
              OTHERS  = 01.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.                               "UPTYP-LOT

* docu: Änderungshinweise ausgeben
  CLEAR xaend-ctxnr.
  CLEAR t166t.                                              "932937
  LOOP AT t_xaend WHERE ebelp EQ ekpo-ebelp.
    SELECT SINGLE * FROM t166t WHERE spras = ekko-spras
                                 AND ctxnr = t_xaend-ctxnr.
    IF sy-subrc EQ 0 AND xaend-ctxnr NE t_xaend-ctxnr.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'CHANGE_REMARKS'
        EXCEPTIONS
          OTHERS  = 01.
      CLEAR sy-subrc.
    ENDIF.
    xaend-ctxnr = t_xaend-ctxnr.
  ENDLOOP.

  IF ekko-bstyp = bstyp-best.                              "begin 393369
*    IF ekpo-pstyp NE pstyp-dien.                  "932937 "737535
* Vermerk bei Neudruck, wenn Menge bereits geliefert
    READ TABLE t_xeket
           WITH KEY ebeln = ekpo-ebeln ebelp = ekpo-ebelp.
* In case a position has been delivered partially.
*      IF sy-subrc = 0 AND t_xeket-wemng GT 0 AND t_xeket-wemng LT
*  t_xeket-menge.                                            "737535
    IF sy-subrc = 0.                                        "932937
      IF ( ekpo-pstyp NE pstyp-dien AND t_xeket-wemng GT 0  "932937
         AND t_xeket-wemng LT t_xeket-menge ) OR            "932937
         ( ekpo-pstyp EQ pstyp-dien AND t_xeket-wemng GT 0  "932937
         AND ekpo-elikz IS INITIAL ).                       "932937
        SELECT SINGLE chtxt FROM  t166t
               INTO   t166t-chtxt
               WHERE  spras  = ekko-spras
               AND    ctxnr  = 'P15'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'CHANGE_REMARKS'
          EXCEPTIONS
            OTHERS  = 01.
        CLEAR sy-subrc.
      ELSEIF ( ekpo-pstyp EQ pstyp-dien AND                 "932937
               t_xeket-wemng GT 0 AND                       "932937
               NOT ekpo-elikz IS INITIAL ).                 "932937
        SELECT SINGLE chtxt FROM  t166t                     "932937
               INTO   t166t-chtxt                           "932937
               WHERE  spras  = ekko-spras                   "932937
               AND    ctxnr  = 'P14'.                       "932937
        CALL FUNCTION 'WRITE_FORM'                          "932937
          EXPORTING                                         "932937
            element = 'CHANGE_REMARKS'                      "932937
          EXCEPTIONS                                        "932937
            OTHERS  = 01.                                   "932937
        CLEAR sy-subrc.                                     "932937
      ENDIF.                                                "932937
* If a position has been delivered completely, it is deleted
* from Xeket and therefore sy-subrc <> 0.
    ELSEIF sy-subrc <> 0.
      IF ekpo-pstyp <> pstyp-text.                          "501661
        SELECT SINGLE chtxt FROM  t166t
               INTO   t166t-chtxt
               WHERE  spras  = ekko-spras
               AND    ctxnr  = 'P14'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'CHANGE_REMARKS'
          EXCEPTIONS
            OTHERS  = 01.
        CLEAR sy-subrc.
      ENDIF.                                                "501661
    ENDIF.                                                  "end 393369
** Only when ekpo-lebre is set, the booked quantity in the service
** entry sheet will be correctly saved in EKET.
*    ELSEIF ekpo-lebre EQ 'X'.                               "737535
*      CLEAR srv_quantity.
*      CALL FUNCTION 'MS_READ_SERVICES'
*        EXPORTING
*          i_hpackno = ekpo-packno
*        TABLES
*          t_esll    = tesll.
*      LOOP AT tesll.
*        srv_quantity = srv_quantity + tesll-menge.
*      ENDLOOP.
** Vermerk bei Neudruck, wenn Menge bereits geliefert
*      READ TABLE t_xeket
*         WITH KEY ebeln = ekpo-ebeln ebelp = ekpo-ebelp.
** In case a position has been delivered partially.
*      IF sy-subrc = 0 AND t_xeket-wemng GT 0 AND t_xeket-wemng LT
*  srv_quantity.
*        SELECT SINGLE chtxt FROM  t166t
*               INTO   t166t-chtxt
*               WHERE  spras  = ekko-spras
*               AND    ctxnr  = 'P15'.
*        CALL FUNCTION 'WRITE_FORM'
*          EXPORTING
*            element = 'CHANGE_REMARKS'
*          EXCEPTIONS
*            OTHERS  = 01.
*        CLEAR sy-subrc.
*      ELSEIF sy-subrc = 0 AND t_xeket-wemng GT 0 AND t_xeket-wemng EQ
*srv_quantity.
*        SELECT SINGLE chtxt FROM  t166t
*       INTO   t166t-chtxt
*       WHERE  spras  = ekko-spras
*       AND    ctxnr  = 'P14'.
*        CALL FUNCTION 'WRITE_FORM'
*          EXPORTING
*            element = 'CHANGE_REMARKS'
*          EXCEPTIONS
*            OTHERS  = 01.
*        CLEAR sy-subrc.
*      ENDIF.                                                "737535
*    ENDIF.
  ENDIF.

* docu: Dienstleistungspaket ausgeben
  IF ( ekpo-pstyp EQ pstyp-dien  OR                         "932937
    ( ekpo-pstyp EQ pstyp-lohn AND                          "932937
      NOT ekpo-packno IS INITIAL ) ) AND                    "932937
    ekpo-elikz IS INITIAL.                                  "932937
    p_chngind = t_xaend-chngind.
    PERFORM select_services USING space p_from_memory p_chngind.
    CALL FUNCTION 'PLAN_READ_FOR_PURCHASE_ORDER'
      EXPORTING
        ebeln               = ekko-ebeln
        ebelp               = ekpo-ebelp
      TABLES
        mpos_tab            = mpos_tab
        zykl_tab            = zykl_tab
      EXCEPTIONS
        no_maintenance_item = 1
        OTHERS              = 2.
    IF sy-subrc EQ 0.
      PERFORM print_maintance_schedule.
    ENDIF.
  ENDIF.

* docu: Ausgabe des Rechnungsplans
  IF NOT ekpo-fplnr IS INITIAL.
    PERFORM print_invoicing_schedule.
  ENDIF.

* docu: Ausgabe zugeordneter Dokumente
  DATA xflag.
  LOOP AT doktab.
    IF doktab-objky+10(5) = ekpo-ebelp.
      IF xflag IS INITIAL.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'ITEM_DVS_HEADER'
          EXCEPTIONS
            OTHERS  = 01.
        xflag = 'X'.
      ENDIF.
      MOVE-CORRESPONDING doktab TO drad.
      drat-dktxt = doktab-dktxt.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_DVS_DOCUMENT'
        EXCEPTIONS
          OTHERS  = 01.
    ENDIF.
  ENDLOOP.

* docu:  Ausgabe von Additionals (Verkaufshilfsmitteln).
*  Die Dokumente müssen durch den Kunden angepasst werden.
  LOOP AT l_addis_in_orders.
    MOVE-CORRESPONDING l_addis_in_orders TO wtad_buying_print_addi.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'ITEM_ADDI_INFO'
      EXCEPTIONS
        OTHERS  = 01.
    LOOP AT l_addis_in_orders-addi_buying_extra_text_info
                       INTO wtad_buying_print_extra_text.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'ITEM_ADDI_EXTRA_TEXT'
        EXCEPTIONS
          OTHERS  = 01.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                               " PRINT_ITEM_PO
