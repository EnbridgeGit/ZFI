* 6.0
* QKWP7HK006579 10042006 Errors in connection with ALV-list       939729
*----------------------------------------------------------------------*
*   INCLUDE ZPMO_CCC010_F02                                            *
*----------------------------------------------------------------------*

***********************************************************************
*                        Protocolling forms                           *
***********************************************************************
* Begin of Syntax-1.
FORM Display_Protokoll USING P_Error type c.
* Replaces.
*FORM Display_Protokoll USING P_Error.
*End of Syntax-1.

* Alles anzeigen: ALV  /  Nur Fehler: Standard List.
  IF RB_All = 'X'.
    PERFORM ALV_Display." USING T_Protokoll.
  ELSE.
    PERFORM Std_List_Display." USING T_Protokoll.
  ENDIF.
  IF P_Error = 'X'.
    SKIP 2.
    FORMAT RESET.
    WRITE: / TEXT-F10.
    WRITE: / Text-F11.
  ENDIF.
ENDFORM.





FORM ALV_Display. " USING T_Protokoll TYPE Type_T_Protokoll.
*
DATA: Fieldcat    TYPE Slis_Fieldcat_ALV,
      TCat        TYPE Slis_T_Fieldcat_ALV,
      Layout      TYPE Slis_Layout_ALV,
      T_Event     TYPE Slis_T_Event,
      WA_Event    TYPE Slis_ALV_Event,
      ALV_repid     LIKE sy-repid.
*
  REFRESH TCat.
* Fields für die Ausgabe einrichten
  PERFORM FCat TABLES TCAT USING 'INDEX_DATASET'               TEXT-K01.
  PERFORM FCat TABLES TCAT USING 'FEHLER_TEXT'                 TEXT-K02.
  PERFORM FCat TABLES TCAT USING 'DATA-TXN_NB'                 TEXT-K03.
  PERFORM FCat TABLES TCAT USING 'DATA-BILL_ACC_NB'            TEXT-K04.
* Begin of SSN_Nb
  IF SSN_Proc = 'X'.
    PERFORM FCat TABLES TCAT USING 'DATA-SOCIAL_SEC_NB'        TEXT-K20.
  ENDIF.
  PERFORM FCat TABLES TCAT USING 'CONV_DATA-PERNR'             TEXT-K05.
* Replaces
*  PERFORM FCat TABLES TCAT USING 'DATA-PERNR'                 TEXT-K05.
* End of SSN_Nb
  PERFORM FCat TABLES TCAT USING 'DATA-ACCOUNT_TYPE'           TEXT-K06.
  PERFORM FCat TABLES TCAT USING 'DATA-MIS_IND'                TEXT-K07.
  PERFORM FCat TABLES TCAT USING 'DATA-CHARGE_DATE'            TEXT-K08.
  PERFORM FCat TABLES TCAT USING 'DATA-SIGN'                   TEXT-K09.
  PERFORM FCat TABLES TCAT USING 'DATA-LOCAL_CHARGE_AMOUNT'    TEXT-K10.
*Begin of QKWK006579
* PERFORM FCat TABLES TCAT USING 'DATA-LOCAL_DECIMAL_PLACE_NB' TEXT-K11.
  PERFORM FCat TABLES TCAT USING 'DATA-LOCAL_DEC_PLACE'        TEXT-K11.
*End   of QKWK006579
  PERFORM FCat TABLES TCAT USING 'DATA-LOCAL_CURR_CODE'        TEXT-K12.
  PERFORM FCat TABLES TCAT USING 'CONV_DATA-UMS_LAND'          TEXT-K13.
*Begin of QKWK006579
* PERFORM FCat TABLES TCAT USING 'DCONV_DATA-UMS_MWST'         TEXT-K14.
  PERFORM FCat TABLES TCAT USING 'CONV_DATA-UMS_MWST'          TEXT-K14.
*End   of QKWK006579
  PERFORM FCat TABLES TCAT USING 'DATA-CURR_EXCHANGE_RATE'     TEXT-K15.
  PERFORM FCat TABLES TCAT USING 'DATA-BILLED_AMOUNT'          TEXT-K16.
  PERFORM FCat TABLES TCAT USING 'CONV_DATA-UMSGEBUR'          TEXT-K17.
  PERFORM FCat TABLES TCAT USING 'CONV_DATA-UMSTOTAL'          TEXT-K18.
  PERFORM FCat TABLES TCAT USING 'CONV_DATA-TEXT_LANG'         TEXT-K19.
*
  clear Fieldcat.
  Fieldcat-Ref_Tabname = 'T_PROTOKOLL'.
  Fieldcat-Fieldname = 'COLOR'.
  Fieldcat-Tech = 'X'.
  APPEND Fieldcat to TCat.
* Layout einrichten
  Layout-Colwidth_Optimize = 'X'.
  Layout-Detail_Initial_Lines = 'X'.
  Layout-Detail_Popup = 'X'.
  Layout-Info_Fieldname = 'COLOR'.
* Kopfteil einrichten
  WA_Event-Name = Slis_ev_Top_of_Page.
  WA_Event-Form = 'TOP_OF_PAGE'.
  APPEND WA_Event TO T_Event.
  ALV_repid = sy-repid.
*
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
            I_CALLBACK_PROGRAM       = ALV_repid
            IS_LAYOUT                = Layout
            IT_FIELDCAT              = TCat
            IT_EVENTS                = T_Event
       TABLES
            T_OUTTAB                 = T_Protokoll
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.
  IF SY-SUBRC <> 0.
  ENDIF.
*
ENDFORM.





FORM FCat TABLES TCat TYPE Slis_T_Fieldcat_ALV
          USING  P_Feld TYPE c
                 P_Text TYPE c.
*
DATA: Fieldcat TYPE Slis_Fieldcat_ALV.
*
  clear Fieldcat.
  Fieldcat-Ref_Tabname = 'T_PROTOKOLL'.
  Fieldcat-Fieldname = P_Feld.
  Fieldcat-Seltext_m = P_Text.
  APPEND Fieldcat to TCat.
*
ENDFORM.






FORM Std_List_Display. " USING T_Protokoll TYPE Type_T_Protokoll.
* Display nur für fehler
DATA: Idx_c(6)           TYPE c,
      Head_Output(130)   TYPE c,
      Intens(1)          TYPE c.

  ULINE.
  LOOP AT T_Protokoll INTO A_Protokoll.
    CLEAR Head_Output.
    Idx_c = A_Protokoll-Index_Dataset.
    FORMAT COLOR COL_NEGATIVE.
    IF NOT A_Protokoll-Conv_Status = 0.
      IF Intens = 'X'.
        FORMAT INTENSIFIED OFF.
        INTENS = ' '.
      ELSE.
        FORMAT INTENSIFIED ON.
        INTENS = 'X'.
      ENDIF.
      CONCATENATE Text-O01 Idx_c Text-O03 A_Protokoll-Fehler_Text
                        INTO Head_Output SEPARATED BY SPACE.
      CONDENSE Head_Output.
      CONCATENATE Text-O04 A_Protokoll-Fehler_Wert
                        INTO Head_Output+85 SEPARATED BY SPACE.
      WRITE: /1 sy-vline,
             (130) Head_Output COLOR COL_NEGATIVE,
             132 sy-vline.
    ENDIF.
  ENDLOOP.
  ULINE.
ENDFORM.



FORM TOP_OF_PAGE.   "#EC CALLED

DATA: C_OK_C(6) TYPE c,
      C_KO_C(6) TYPE c.

DATA: T_Header  TYPE Slis_t_ListHeader,
      WA_Header TYPE Slis_ListHeader.

  WA_Header-Typ = 'H'.
  CONCATENATE Text-D01 Header_SAP-Cardcomp INTO WA_Header-Info
                            SEPARATED BY SPACE.
  APPEND WA_Header to T_Header.
  WA_Header-Typ = 'H'.
  CONCATENATE Text-D02 Header_SAP-AbrDatum Into WA_Header-Info
                            SEPARATED BY SPACE.
  APPEND WA_Header to T_Header.
  WA_Header-Typ = 'H'.
  CONCATENATE Text-D03 Header_SAP-LfdNum Into WA_Header-Info
                            SEPARATED BY SPACE.
  APPEND WA_Header to T_Header.
  C_OK_C = Conv_OK.
  C_KO_C = ConV_KO.
  WA_Header-Typ = 'H'.
  CONCATENATE Text-D05 C_OK_C INTO WA_Header-Info SEPARATED BY SPACE.
  APPEND WA_Header to T_Header.
  WA_Header-Typ = 'H'.
  CONCATENATE Text-D06 C_KO_C INTO WA_HEADER-Info SEPARATED BY SPACE.
  APPEND WA_Header to T_Header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
          IT_LIST_COMMENTARY = T_Header.

ENDFORM.
