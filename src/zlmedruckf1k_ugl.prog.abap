* 2009/01/19 TR580 mdemeest - Upgrade to 4.6 ERP/&.00 SAPWEAVER      UGL
*                             All changes indicated with UGL in      UGL
*                             rightmost column                       UGL
*                             This change is to add YEAR END message UGL
*                             to PO between 1016 and 1215 each year  UGL
*&---------------------------------------------------------------------*
*&      Form  ENDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ende CHANGING p_retco.

* Unterschrift -------------------------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'LAST'
    EXCEPTIONS
      OTHERS  = 01.
  CLEAR sy-subrc.

* Folgeseitenzaehler löschen -----------------------------------------*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element  = 'NEXTPAGE'
      window   = 'NEXTPAGE'
      function = 'DELETE'
    EXCEPTIONS
      OTHERS   = 01.
  CLEAR sy-subrc.

*--------------------------- Start of ERP 6.0 --------------------- UGL
* YearEnd Message to appear on PO's between 1014 and 1216 each year UGL
*
  CONCATENATE sy-datum+4(2) sy-datum+6(2) INTO mthday.             "UGL
  IF mthday > '1014' AND                                           "UGL
     mthday < '1216'.                                              "UGL
    CALL FUNCTION 'WRITE_FORM'                                    "UGL
         EXPORTING                                                "UGL
            element = 'YEAREND_MSG'                               "UGL
         EXCEPTIONS                                               "UGL
            OTHERS = 1.                                           "UGL
  ENDIF.                                                           "UGL
  CLEAR sy-subrc.                                                  "UGL

*ENHANCEMENT-POINT ENDE_01 SPOTS ES_SAPLMEDRUCK STATIC.
* Ende Formulardruck --------------------------------------------------*
*ENHANCEMENT-SECTION     ENDE_02 SPOTS ES_SAPLMEDRUCK.
  CALL FUNCTION 'CLOSE_FORM'
    IMPORTING
      result = result
    EXCEPTIONS
      OTHERS = 1.
*END-ENHANCEMENT-SECTION.
  IF NOT sy-subrc IS INITIAL.
    p_retco = '1'.
    PERFORM protocol_update USING '142' ekko-ebeln
                                   space space space.
    EXIT.
  ENDIF.
*ENHANCEMENT-POINT ENDE_03 SPOTS ES_SAPLMEDRUCK.
  IF result-tdspoolid NE space.
    spoolid = result-tdspoolid.
    PERFORM protocol_update USING '320' spoolid space space space.
  ENDIF.

  IF NOT result-tdfaxid  IS INITIAL OR                      " 422131
     NOT result-tdmailid IS INITIAL.                        " 422131
    CLEAR syst-msgv1.                                       " 422131
    IF NOT result-tdfaxid IS INITIAL.                       " 422131
      syst-msgv1 = result-tdfaxid.                          " 422131
    ELSEIF result-tdmailid IS INITIAL.                      " 422131
      syst-msgv1 = result-tdmailid.                         " 422131
    ENDIF.                                                  " 422131
    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'                    " 422131
         EXPORTING                                          " 422131
              msg_arbgb = 'VN'                              " 422131
              msg_nr    = '095'                             " 422131
              msg_ty    = 'I'                               " 422131
              msg_v1    = syst-msgv1                        " 422131
         EXCEPTIONS                                         " 422131
              OTHERS    = 1.                                " 422131
  ENDIF.                                                    " 422131

  IF enjpreview NE 'X'.                                     "855263
    IF result-userexit EQ 'C' OR
        result-userexit EQ 'E'.
      p_retco = '9'.
    ENDIF.
  ENDIF.

  CLEAR enjpreview.                                         "855263

ENDFORM.                               " ENDE

*---------------------------------------------------------------------*
*       FORM update_release                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  t_xekpo                                                       *
*  -->  t_xekek                                                       *
*  -->  t_xekeh                                                       *
*  -->  p_druvo                                                       *
*  -->  p_kschl                                                       *
*---------------------------------------------------------------------*
FORM update_release TABLES t_xekpo STRUCTURE ekpo
                           t_xekek STRUCTURE ekek
                           t_xekeh STRUCTURE ekeh
                    USING  p_druvo
                           p_kschl.
  CLEAR t161m.
  SELECT SINGLE druab INTO t161m-druab FROM t161m
                         WHERE kvewe = 'B'
                         AND   kappl = 'EL'
                         AND   druvo = p_druvo
                         AND   kschl = p_kschl.

  IF NOT sy-subrc = 0 OR NOT t161m-druab = 'X'.
    EXIT.
  ENDIF.
*ENHANCEMENT-POINT ENDE_04 SPOTS ES_SAPLMEDRUCK.

*ENHANCEMENT-SECTION     ENDE_05 SPOTS ES_SAPLMEDRUCK.
  DELETE t_xekek WHERE webrl EQ '1' OR webrl EQ '2'.
*END-ENHANCEMENT-SECTION.
  CALL FUNCTION 'ME_UPDATE_FROM_PRINTING'
    TABLES
      i_ekek = t_xekek
      i_ekeh = t_xekeh
      i_ekpo = t_xekpo
    EXCEPTIONS
      OTHERS = 1.



ENDFORM.                    "update_release
