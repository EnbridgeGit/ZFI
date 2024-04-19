* 2009/01/19 TR580 mdemeest Upgrade to ERP 6.0/SAPWeaver 7.00       UGL
*                           All changes identified with UGL in      UGL
*                           rightmost column                        UGL
*&---------------------------------------------------------------------*
*&      Form  PRINT_TOTAL_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_total_line.

* Gesamtsumme ausgeben ------------------------------------------------*
  IF pekko-prsdr NE space.
    komk-waerk = ekko-waers.           "1.2
    IF ekko-bstyp NE bstyp-kont AND ekko-stako EQ space.
      READ TABLE tkomvd INDEX 1.
      IF sy-subrc EQ 0.    "nur, wenn noch Kopfrabatte kommen
* IF KOMK-SUPOS NE KOMK-FKWRT.     "geht nicht wegen FKWRT-Rundung!!
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'TOTAL_AMOUNT_ITEMS'
          EXCEPTIONS
            OTHERS  = 01.
        CLEAR sy-subrc.
* Ausgeben Kopf-Konditionen ------------------------------------------*
        LOOP AT tkomvd.
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
*
*------------------------ Start of ERP 6.0 changes ---- ------------ UGL
* Find out the condition value for non-deductible tax                UGL
*
      komvd-kwert = 0.                                              "UGL
      loop at tkomv.                                                "UGL
        if tkomv-kschl = 'NAVS'.                                    "UGL
           komvd-kwert = komvd-kwert + tkomv-kwert.                 "UGL
        endif.                                                      "UGL
      endloop.                                                      "UGL
*------------------------- End of ERP 6.0 changes ----------------  "UGL

      IF  ekko-bedat > '19990101' AND
       ( ekko-waers = 'DEM' OR ekko-waers = 'FRF' OR ekko-waers = 'ATS'
      OR ekko-waers = 'BEF' OR ekko-waers = 'FIM' OR ekko-waers = 'IEP'
      OR ekko-waers = 'ITL' OR ekko-waers = 'LUF' OR ekko-waers = 'NLG'
         OR ekko-waers = 'PTE' OR ekko-waers = 'ESP' ).

        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
            date             = ekko-bedat
            foreign_amount   = komk-fkwrt
            foreign_currency = ekko-waers
            local_currency   = 'EUR'
          IMPORTING
            local_amount     = euro-price
          EXCEPTIONS
            no_rate_found    = 1
            overflow         = 2
            no_factors_found = 3
            no_spread_found  = 4
            OTHERS           = 5.
        sum-euro-price = euro-price.
      ELSE.
        sum-euro-price = 0.
      ENDIF.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'TOTAL_AMOUNT'
        EXCEPTIONS
          OTHERS  = 01.
      CLEAR sy-subrc.
    ENDIF.
  ENDIF.

IF ekko-bstyp EQ bstyp-kont OR ekko-stako NE space. "beim Kontrakt immer
    ekpo-ebelp = '00000'.
    PERFORM ausgabe_stammkonditionen.
  ENDIF.

  IF nast-kappl EQ 'EL'.
* Ueberschrift-Element ermitteln --------------------------------------*
    CASE ekko-bstyp.
      WHEN bstyp-best. elementn = 'ITEM_HEADER_F'.
      WHEN bstyp-anfr. elementn = 'ITEM_HEADER_A'.
      WHEN bstyp-kont. elementn = 'ITEM_HEADER_R'.
      WHEN bstyp-lfpl. elementn = 'ITEM_HEADER_R'.
    ENDCASE.

* Keine Positionsüberschrift mehr -------------------------------------*
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element  = elementn
        function = 'DELETE'
        type     = 'TOP'
      EXCEPTIONS
        OTHERS   = 01.
    CLEAR sy-subrc.
  ENDIF.

ENDFORM.                               " PRINT_TOTAL_LINE
