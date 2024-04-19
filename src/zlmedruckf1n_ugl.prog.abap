* 2009/01/20 TR580 mdemeest Upgrade to ERP 6.0/SAPWeaver          UGL
*                           All changes indicated with UGL in     UGL
*                           rightmost column                      UGL
*&---------------------------------------------------------------------*
*&      Form  PRINT_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_header TABLES lt_xaend STRUCTURE xaend
                  CHANGING p_retco.

  TABLES: adrc.                                                   "UGL


* Folgeseitenzaehler -------------------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'NEXTPAGE'
      window  = 'NEXTPAGE'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

* Referenzangaben nur wenn gefüllt -----------------------------------*
  IF ekko-angnr NE space AND nast-kappl NE 'EL'.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'REFERENCE'
        window  = 'REFERENC'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ENDIF.

* Werksanschrift, wenn in allen Positionen gleich ---------------------*

*-------------------------- ERP 6.0 -----------------------------  UGL
* To get CONSIGNEE address when it has been entered/changed        UGL
  CLEAR: sadr1,                                                   "UGL
         adrc.                                                    "UGL
*  IF pekko-adrn2 EQ space AND pekko-adrnr NE space.              "UGL
  IF pekko-adrnr NE space.                                        "UGL
    SELECT SINGLE * FROM adrc                                     "UGL
                       WHERE addrnumber = pekko-adrnr.            "UGL
*BOI by PANUSURI ticket 71397
  ELSEIF pekko-adrn2 NE space.                                    "UGL
    SELECT SINGLE * FROM adrc                                     "UGL
                       WHERE addrnumber = pekko-adrn2.            "UGL
*  MOVE-CORRESPONDING adrc TO sadr1.                              "UGL
  ENDIF.                                                          "UGL
  IF adrc IS NOT INITIAL.                                         "UGL
    MOVE: adrc-title TO sadr1-anred,                              "UGL
          adrc-name1 TO sadr1-name1,                              "UGL
          adrc-name2 TO sadr1-name2,                              "UGL
          adrc-name3 TO sadr1-name3,                              "UGL
          adrc-name4 TO sadr1-name4,                              "UGL
          adrc-street TO sadr1-stras,                             "UGL
          adrc-city1 TO sadr1-ort01,                              "UGL
          adrc-city2 TO sadr1-ort02,                              "UGL
          adrc-post_code1 TO sadr1-pstlz,                         "UGL
          adrc-country TO sadr1-land1,                            "UGL
          adrc-region TO sadr1-regio.                             "UGL
  ENDIF.                                                          "UGL
*EOI by PANUSURI ticket 71397
*------------------------ end of ERP 6.0 Changes ----------------- UGL

  IF pekko-werks NE space OR
     pekko-kunnr NE space OR
     pekko-adrn2 NE space OR
     pekko-emlif NE space OR
     pekko-adrnr NE space.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER_DELADDRESS'
        window  = 'CONSGNEE'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ENDIF.

* Termine -------------------------------------------------------------*
  IF nast-kappl NE 'EL'.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element  = 'HEADER_DATES'
        window   = 'DELDATE'
        function = 'APPEND'
      EXCEPTIONS
        OTHERS   = 01.
    CLEAR sy-subrc.
  ENDIF.

* Lieferdatum, wenn in allen Positionen gleich ------------------------*
  IF pekko-lfdat NE space AND ekko-bstyp NE bstyp-lfpl.     "910057
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element  = 'DELDATE'
        window   = 'DELDATE'
        function = 'APPEND'
      EXCEPTIONS
        OTHERS   = 01.
    CLEAR sy-subrc.
  ENDIF.


* Main-Fenster --------------------------------------------------------*
* Bedingungen / Zahlungsbedingungen -----------------------------------*
  IF nast-kappl NE 'EL'.
    ekko-inco1 = pekko-inco1.
    ekko-inco2 = pekko-inco2.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'TERMS'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
    CLEAR rm06p-phtxt.
    LOOP AT zbtxt.
      rm06p-phtxt+14(50) = zbtxt-line.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TERMS_OF_PAYMENT'
        EXCEPTIONS
          OTHERS  = 01.
      CLEAR sy-subrc.
    ENDLOOP.
  ENDIF.

* Kopfzusatzinformationen ausgeben ------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'HEADER_INFO'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

* Zielwert, wenn vorhanden --------------------------------------------*
  IF ekko-ktwrt NE 0 AND nast-kappl NE 'EL'.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER_TARGET_VALUE'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ENDIF.

* Kopftexte ausgeben -------------------------------------------------*
  LOOP AT xt166k.
    MOVE xt166k TO t166k.
    CASE t166k-tdobject.
      WHEN 'EKKO'.
        t166k-txnam(10)   = ekko-ebeln.
      WHEN 'LFA1'.
        t166k-txnam(10)   = ekko-lifnr.
      WHEN 'LFM1'.
        t166k-txnam(10)   = ekko-lifnr.
        t166k-txnam+10(4) = ekko-ekorg.
    ENDCASE.
    PERFORM lesen_ttxit USING xt166k-titdr xt166k-tdobject xt166k-tdid.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER_TEXT'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ENDLOOP.

* Änderungshinweise ausgeben -----------------------------------------*
  CLEAR xaend-ctxnr.
  LOOP AT lt_xaend WHERE ebelp EQ '00000'.
    CLEAR xt166k.
    READ TABLE xt166k WITH KEY tdid = lt_xaend-textart.
    SELECT SINGLE * FROM t166t WHERE spras = ekko-spras
                               AND ctxnr = lt_xaend-ctxnr.
    IF sy-subrc EQ 0 AND xaend-ctxnr NE lt_xaend-ctxnr.
      IF (     xt166k         IS INITIAL AND
               lt_xaend-ctxnr NE 'S4' )  OR
         ( NOT xt166k         IS INITIAL AND
               lt_xaend-ctxnr EQ 'S4' ).
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'CHANGE_REMARKS'
          EXCEPTIONS
            OTHERS  = 01.
        CLEAR sy-subrc.
        xaend-ctxnr = lt_xaend-ctxnr.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Auftragsbestätigungspflicht, wenn in allen Positionen gleich --------*
  IF pekko-kzabs NE space AND
     pekko-labnr EQ space AND
     xdruvo NE mahn AND
     xdruvo NE lpma AND
     xdruvo NE aufb.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'HEADER_INFO_ACKNOW'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ENDIF.

* Absagegrund ausgeben

  IF ekko-absgr NE space.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'REASON_FOR_REJECTION'
      EXCEPTIONS
        OTHERS  = 01.
    CLEAR sy-subrc.
  ENDIF.


ENDFORM.                               " PRINT_HEADER
