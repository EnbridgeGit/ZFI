*----------------------------------------------------------------------*
* THIS IS NOT A STANDARD PROGRAM.
* IT IS ONLY INCLUDED FOR CONVENIENCE IN CASE IT WILL BE NEEDED.
* ONLY USE IT AFTER CONSULTING WITH SAP.
*
* MODS:
* IFRS - gymana - June 24, 2009
*        This Z version of RFSEPA02 has been created according to
*        Note 175960.
*----------------------------------------------------------------------*
REPORT RFSEPA02 MESSAGE-ID FH LINE-SIZE 80.

INCLUDE FSACHCDF.
INCLUDE FSACHCDV.

TABLES: BSIS,
        BSAS,
        BSEG,
        BKPF,
        GLT0,
        T001.

DATA: BEGIN OF T_BSXX OCCURS 100.
        INCLUDE STRUCTURE BSIS.
DATA: END OF T_BSXX.

DATA: BEGIN OF T_LISTE OCCURS 300,
        GJAHR    LIKE BKPF-GJAHR,
        BELNR    LIKE BKPF-BELNR,
        SORT     TYPE I,
        TABLE(4) TYPE C,
        TYPE     TYPE C,
        OK       TYPE C,
      END OF T_LISTE.

DATA: D_MAX         LIKE SY-TFILL VALUE '100',
      D_BELNR       LIKE BSEG-BELNR,
*     D_TFILL       LIKE SY-TFILL.        "<<<< DELETE - NOTE 70203
      D_TFILL       LIKE SY-TFILL,     "<<<< INSERT - NOTE 70203
      D_STORNO      TYPE C.            "<<<< INSERT - NOTE 70203

DATA: BALANCE_ITEMS   LIKE GLT0-HSL01,
      BALANCE_ACCOUNT LIKE GLT0-HSL01.

*>>>> BEGIN OF INSERTION - NOTE 66156 <<<<
DATA: XGLEDTAB LIKE GLEDTAB OCCURS 1 WITH HEADER LINE.
DATA: BEGIN OF BK_METHODE,
        HWAE1 LIKE T001-WAERS,
        HWAE2 LIKE T001-WAERS,
        HWAE3 LIKE T001-WAERS,
        CURT1 LIKE X001-CURT2,
        CURT2 LIKE X001-CURT2,
        CURT3 LIKE X001-CURT2,
      END OF BK_METHODE.

DATA: BEGIN OF T_BALANCE_ITEMS_HW OCCURS 1,
        WAERS1 LIKE T001-WAERS,
        VALUE1 LIKE GLT0-HSLVT,
        WAERS2 LIKE T001-WAERS,
        VALUE2 LIKE GLT0-HSLVT,
        WAERS3 LIKE T001-WAERS,
        VALUE3 LIKE GLT0-HSLVT,
      END OF T_BALANCE_ITEMS_HW.

DATA: BEGIN OF T_BALANCE_ACCOUNT_HW OCCURS 1,
        WAERS1 LIKE T001-WAERS,
        VALUE1 LIKE GLT0-HSLVT,
        WAERS2 LIKE T001-WAERS,
        VALUE2 LIKE GLT0-HSLVT,
        WAERS3 LIKE T001-WAERS,
        VALUE3 LIKE GLT0-HSLVT,
      END OF T_BALANCE_ACCOUNT_HW.

DATA: BEGIN OF T_BALANCE_ITEMS_TW OCCURS 1,
        WAERS LIKE GLT0-RTCUR,
        VALUE LIKE GLT0-TSLVT,
      END OF T_BALANCE_ITEMS_TW.

DATA: BEGIN OF T_BALANCE_ACCOUNT_TW OCCURS 10,
        WAERS LIKE GLT0-RTCUR,
        VALUE LIKE GLT0-TSLVT,
      END OF T_BALANCE_ACCOUNT_TW.
*>>>> END OF INSERTION - NOTE 66156 <<<<

DATA: NUM_BSEG_SELECT LIKE SY-TFILL,
      NUM_BSEG_UPDATE LIKE SY-TFILL,
      NUM_BSIS_INSERT LIKE SY-TFILL,
      NUM_BSIS_SELECT LIKE SY-TFILL,
      NUM_BSIS_UPDATE LIKE SY-TFILL,
      NUM_BSAS_INSERT LIKE SY-TFILL,
      NUM_BSAS_SELECT LIKE SY-TFILL,
      NUM_BSAS_UPDATE LIKE SY-TFILL.

CONSTANTS: CHAR_I     TYPE C VALUE 'I',
           CHAR_S     TYPE C VALUE 'S',
           CHAR_U     TYPE C VALUE 'U'.

*-----------------------------------------------------------------------

*PARAMETER:     P_BUKRS  LIKE BSEG-BUKRS OBLIGATORY.
                                       "<<<< DELETE NOTE - 62751
*PARAMETER:     P_SAKNR  LIKE BSEG-HKONT OBLIGATORY.
                                       "<<<< DELETE NOTE - 62751
*>>>> BEGIN OF INSERTION - NOTE 62751 <<<<
PARAMETERS:     P_BUKRS  LIKE BKPF-BUKRS OBLIGATORY.
PARAMETERS:     P_SAKNR  LIKE SKA1-SAKNR OBLIGATORY
                                  MATCHCODE OBJECT SAKO.
*>>>> END OF INSERTION - NOTE 62751 <<<<

SELECT-OPTIONS P_BELNR  FOR  BSEG-BELNR.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
PARAMETERS:    P_LISTE  AS CHECKBOX DEFAULT 'X'.    " list protocol on
SELECTION-SCREEN END OF BLOCK B01.

*-----------------------------------------------------------------------
*INITIALIZATION.                                "DELETE - NOTE 175960
*  CALL FUNCTION 'POPUP_DISPLAY_TEXT'           "DELETE - NOTE 175960
*       EXPORTING                               "DELETE - NOTE 175960
*            POPUP_TITLE    = ' '               "DELETE - NOTE 175960
*            TEXT_OBJECT    = 'RFSEPA02'        "DELETE - NOTE 175960
*       EXCEPTIONS                              "DELETE - NOTE 175960
*            TEXT_NOT_FOUND = 1                 "DELETE - NOTE 175960
*            OTHERS         = 2.                "DELETE - NOTE 175960
*                                               "DELETE - NOTE 175960
*  LEAVE PROGRAM.                               "DELETE - NOTE 175960

AT SELECTION-SCREEN ON P_SAKNR.
  PERFORM CHECK_ACCOUNT.               " locked for posting?
  PERFORM CHECK_ACCOUNT2.              "<<<< INSERT - NOTE 66156
* PERFORM ENQUEUE_ACCOUNT.             "<<<< DELETE - NOTE 65191

*-----------------------------------------------------------------------

START-OF-SELECTION.
  PERFORM ENQUEUE_ACCOUNT.             "<<<< INSERT - NOTE 65191
  PERFORM CHECK_AUTHORITY_SKA1_BUK.
  PERFORM CHECK_AUTHORITY_SKA1_BES.
* PERFORM CHECK_ACCOUNT_BALANCE.       "<<<< DELETE - NOTE 66156
  PERFORM CHECK_ACCOUNT_BALANCE_2.     "<<<< INSERT - NOTE 66156

*--- BSIS --------------------------------------------------------------
  CLEAR D_BELNR.
  CLEAR T_LISTE. REFRESH T_LISTE.
  CLEAR T_BSXX.  REFRESH T_BSXX.
  DO.
    PERFORM SELECT_ITEMS USING 'BSIS' D_TFILL.
    IF D_TFILL EQ 0.
      EXIT.
    ENDIF.
    LOOP AT T_BSXX.
      PERFORM BKPF_UPDATE.             "<<<< INSERT - NOTE 70203
      PERFORM BSEG_UPDATE.
      PERFORM BSXX_UPDATE USING 'BSIS'.
    ENDLOOP.
    PERFORM WRITE_LIST_PROTOCOL.
    COMMIT WORK.
  ENDDO.

*--- BSAS --------------------------------------------------------------
  CLEAR D_BELNR.
  CLEAR T_LISTE. REFRESH T_LISTE.
  CLEAR T_BSXX.  REFRESH T_BSXX.
  DO.
    PERFORM SELECT_ITEMS USING 'BSAS' SY-TFILL.
    IF SY-TFILL EQ 0.
      EXIT.
    ENDIF.
    LOOP AT T_BSXX.
      PERFORM BSEG_UPDATE.
      PERFORM BSXX_UPDATE USING 'BSAS'.
    ENDLOOP.
    PERFORM WRITE_LIST_PROTOCOL.
    COMMIT WORK.
  ENDDO.
  PERFORM WRITE_PROTOCOL.
*-----------------------------------------------------------------------

END-OF-SELECTION.
  PERFORM CHANGE_ACCOUNT.
  PERFORM DEQUEUE_ACCOUNT.

*-----------------------------------------------------------------------

TOP-OF-PAGE.
  ULINE.
  WRITE:/   SY-VLINE,
         2  TEXT-001,
         25 P_SAKNR INTENSIFIED OFF,
         80 SY-VLINE.
  WRITE:/   SY-VLINE,
         2  TEXT-002,
         25 P_BUKRS INTENSIFIED OFF,
         80 SY-VLINE.
  ULINE.
  IF P_LISTE = 'X'.
    WRITE:/          SY-VLINE,
              2(10)  TEXT-S01 COLOR COL_HEADING,
              12     SY-VLINE,
              13(4)  TEXT-S02 COLOR COL_HEADING,
              17     SY-VLINE,
              18(62) TEXT-S03 COLOR COL_HEADING,
              80     SY-VLINE.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  SELECT_ITEMS
*&---------------------------------------------------------------------*
FORM SELECT_ITEMS USING TABLE TYPE C
                        FILL  LIKE SY-TFILL.
  SELECT * FROM (TABLE) INTO TABLE T_BSXX
*                         UP TO D_MAX ROWS
                       WHERE BUKRS EQ P_BUKRS
                         AND HKONT EQ P_SAKNR
                         AND BELNR IN P_BELNR
                         AND XOPVW EQ SPACE.
  FILL = SY-DBCNT.
*--- PROTOCOL ----------------------------------------------------------
  LOOP AT T_BSXX.
    PERFORM SAVE_PROTOCOL USING T_BSXX-BELNR T_BSXX-GJAHR
                                1 TABLE CHAR_S 1.
  ENDLOOP.

  SORT T_BSXX BY MANDT BUKRS BELNR GJAHR.

ENDFORM.                               " SELECT_ITEMS

*&---------------------------------------------------------------------*
*&      Form  BSEG_UPDATE
*&---------------------------------------------------------------------*
FORM BSEG_UPDATE.
  SELECT SINGLE * FROM BSEG WHERE BUKRS = T_BSXX-BUKRS
                            AND   GJAHR = T_BSXX-GJAHR
                            AND   BELNR = T_BSXX-BELNR
                            AND   BUZEI = T_BSXX-BUZEI.
  IF BSEG-KTOSL = 'MVA' OR BSEG-KTOSL = 'VVA'.
    MESSAGE A145(FH) WITH BSEG-HKONT.
  ENDIF.
  IF BSEG-KOART = 'M'.
    MESSAGE A144(FH) WITH P_SAKNR BSEG-BSCHL.
  ENDIF.

  UPDATE BSEG SET XOPVW = 'X'
            WHERE BUKRS EQ T_BSXX-BUKRS
              AND GJAHR EQ T_BSXX-GJAHR
              AND BELNR EQ T_BSXX-BELNR
              AND BUZEI EQ T_BSXX-BUZEI.
*--- PROTOCOL ----------------------------------------------------------
  PERFORM SAVE_PROTOCOL USING T_BSXX-BELNR T_BSXX-GJAHR
                              2 'BSEG' CHAR_U SY-DBCNT.
ENDFORM.                               " BSEG_UPDATE

*&---------------------------------------------------------------------*
*&      Form  BSIS_UPDATE
*&---------------------------------------------------------------------*
FORM BSXX_UPDATE USING TABLE TYPE C.
  T_BSXX-XOPVW = 'X'.
  IF D_STORNO = 'X' AND TABLE = 'BSIS'."<<<< INSERT - NOTE 70203
    T_BSXX-XSTOV = SPACE.              "<<<< INSERT - NOTE 70203
  ENDIF.                               "<<<< INSERT - NOTE 70203
  UPDATE (TABLE) FROM T_BSXX.
*--- PROTOCOL ----------------------------------------------------------
  PERFORM SAVE_PROTOCOL USING T_BSXX-BELNR T_BSXX-GJAHR
                              1 TABLE CHAR_U SY-DBCNT.
ENDFORM.                               " BSIS_UPDATE

*&---------------------------------------------------------------------*
*&      Form  CHECK_ACCOUNT_BALANCE
*&---------------------------------------------------------------------*
FORM CHECK_ACCOUNT_BALANCE.

  DATA:   D_GJAHR LIKE BSEG-GJAHR.
* Account balance
  PERFORM READ_T001 USING P_BUKRS.
  PERFORM READ_CURRENT_YEAR CHANGING D_GJAHR.
  PERFORM READ_GLDB USING P_BUKRS P_SAKNR D_GJAHR.
  PERFORM READ_BSIS USING P_BUKRS P_SAKNR.
  IF BALANCE_ITEMS NE BALANCE_ACCOUNT.
    MESSAGE E099.
*   Summe der Einzelposten stimmt nicht mit Kontensaldo überein. ->
  ENDIF.
ENDFORM.                               " CHECK_ACCOUNT_BALANCE

*&---------------------------------------------------------------------*
*&      Form  READ_T001
*&---------------------------------------------------------------------*
FORM READ_T001 USING BUKRS LIKE T001-BUKRS.
  SELECT SINGLE * FROM T001 WHERE BUKRS = BUKRS.
  IF SY-SUBRC <> 0.
    MESSAGE E001 WITH BUKRS.
  ENDIF.
ENDFORM.                               " READ_T001

*&---------------------------------------------------------------------*
*&      Form  READ_CURRENT_YEAR
*&---------------------------------------------------------------------*
FORM READ_CURRENT_YEAR CHANGING GJAHR LIKE T009B-BDATJ.
  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
       EXPORTING
            I_DATE         = SY-DATUM
            I_PERIV        = T001-PERIV
       IMPORTING
            E_GJAHR        = GJAHR
       EXCEPTIONS
            T009_NOTFOUND  = 1
            INPUT_FALSE    = 1
            T009B_NOTFOUND = 1.
ENDFORM.                               " READ_CURRENT_YEAR

*&---------------------------------------------------------------------*
*&      Form  READ_GLDB
*&---------------------------------------------------------------------*
FORM READ_GLDB USING XBUKRS LIKE GLT0-BUKRS
                     XSAKNR LIKE GLT0-RACCT
                     XGJAHR LIKE GLT0-RYEAR.

  SELECT * FROM GLT0
           WHERE RLDNR = '00'
           AND   RRCTY = '0'
           AND   RVERS = '001'
           AND   BUKRS = XBUKRS
           AND   RYEAR = XGJAHR
           AND   RACCT = XSAKNR.
    PERFORM CALCULATE_BALANCE.
  ENDSELECT.

ENDFORM.                               " READ_GLDB

*---------------------------------------------------------------------*
*       FORM CALCULATE_BALANCE                                        *
*---------------------------------------------------------------------*
FORM CALCULATE_BALANCE.
  DATA: BALANCE  LIKE GLT0-TSL01.

  BALANCE_ACCOUNT = BALANCE_ACCOUNT + GLT0-HSLVT.
  DO GLT0-RPMAX TIMES
     VARYING BALANCE FROM GLT0-HSL01 NEXT GLT0-HSL02.
    BALANCE_ACCOUNT = BALANCE_ACCOUNT + BALANCE.
  ENDDO.

ENDFORM.                               " CALCULATE_BALANCE
*&---------------------------------------------------------------------*
*&      Form  READ_BSIS
*&---------------------------------------------------------------------*
FORM READ_BSIS USING BUKRS LIKE BSIS-BUKRS
                     SAKNR LIKE BSIS-HKONT.

  SELECT * FROM  BSIS
         WHERE  BUKRS = BUKRS
         AND    HKONT = SAKNR.
    IF BSIS-SHKZG = 'S'.
      BALANCE_ITEMS = BALANCE_ITEMS  + BSIS-DMBTR.
    ELSE.
      BALANCE_ITEMS = BALANCE_ITEMS  - BSIS-DMBTR.
    ENDIF.
  ENDSELECT.
ENDFORM.                               " READ_BSIS

*&---------------------------------------------------------------------*
*&      Form  CHANGE_ACCOUNT
*&---------------------------------------------------------------------*
FORM CHANGE_ACCOUNT.

  SKB1       = *SKB1.
  SKB1-XOPVW = 'X'.

  OBJECTID    = T001-KTOPL.
  OBJECTID+4  = SKB1-SAKNR.
  OBJECTID+14 = SKB1-BUKRS.
  UPD_SKB1    = 'U'.
  UTIME       = SY-UZEIT.
  UDATE       = SY-DATUM.
  USERNAME    = SY-UNAME.
  PERFORM CD_CALL_SACH.
  CALL FUNCTION 'GL_ACCOUNT_IN_COMPANY_UPDATE'
       EXPORTING
            I_SKB1 = SKB1.
  COMMIT WORK.

ENDFORM.                               " CHANGE_ACCOUNT

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ACCOUNT
*&---------------------------------------------------------------------*
FORM ENQUEUE_ACCOUNT.
  CALL FUNCTION 'ENQUEUE_EFSKB1'
       EXPORTING
            BUKRS          = P_BUKRS
            SAKNR          = P_SAKNR
       EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2.
  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE E042 WITH P_SAKNR P_BUKRS.
    WHEN 2.
      MESSAGE E038 WITH P_SAKNR P_BUKRS.
  ENDCASE.
ENDFORM.                               " ENQUEUE_ACCOUNT

*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_ACCOUNT
*&---------------------------------------------------------------------*
FORM DEQUEUE_ACCOUNT.
  CALL FUNCTION 'DEQUEUE_EFSKB1'
       EXPORTING
            BUKRS          = P_BUKRS
            SAKNR          = P_SAKNR
       EXCEPTIONS
            SYSTEM_FAILURE = 1.
ENDFORM.                               " DEQUEUE_ACCOUNT

*---------------------------------------------------------------------*
*       FORM WRITE_LIST_PROTOCOL                                      *
*---------------------------------------------------------------------*
*       writes list protocol                                          *
*---------------------------------------------------------------------*
FORM WRITE_LIST_PROTOCOL.
  DATA: TEXT(62)   TYPE C,
        SAVE_BELNR LIKE BKPF-BELNR.

  DESCRIBE TABLE T_LISTE LINES SY-TFILL.
  CHECK P_LISTE = 'X'.
  CHECK SY-TFILL > 0.

  SORT T_LISTE BY GJAHR BELNR SORT TYPE.

  LOOP AT T_LISTE.
    CHECK T_LISTE-OK = 1.
    CASE T_LISTE-TABLE.
      WHEN 'BSEG'.
        CASE T_LISTE-TYPE.
          WHEN CHAR_S.
            TEXT = TEXT-BE1.
          WHEN CHAR_U.
            TEXT = TEXT-BE2.
        ENDCASE.
      WHEN 'BSIS'.
        CASE T_LISTE-TYPE.
          WHEN CHAR_S.
            TEXT = TEXT-XX1.
            REPLACE '&TAB' WITH T_LISTE-TABLE INTO TEXT.
          WHEN CHAR_U.
            TEXT = TEXT-XX2.
            REPLACE '&TAB' WITH T_LISTE-TABLE INTO TEXT.
          WHEN CHAR_I.
            TEXT = TEXT-XX3.
            REPLACE '&TAB' WITH T_LISTE-TABLE INTO TEXT.
        ENDCASE.
      WHEN 'BSAS'.
        CASE T_LISTE-TYPE.
          WHEN CHAR_S.
            TEXT = TEXT-XX1.
            REPLACE '&TAB' WITH T_LISTE-TABLE INTO TEXT.
          WHEN CHAR_U.
            TEXT = TEXT-XX2.
            REPLACE '&TAB' WITH T_LISTE-TABLE INTO TEXT.
          WHEN CHAR_I.
            TEXT = TEXT-XX3.
            REPLACE '&TAB' WITH T_LISTE-TABLE INTO TEXT.
        ENDCASE.
    ENDCASE.

    AT NEW BELNR.
      ULINE.
      WRITE:/       SY-VLINE,
              2     T_LISTE-BELNR COLOR COL_KEY,
              12    SY-VLINE,
              13    T_LISTE-GJAHR COLOR COL_KEY,
              17    SY-VLINE.
    ENDAT.

    IF SAVE_BELNR <> T_LISTE-BELNR.
      WRITE: 18(62) TEXT COLOR COL_NORMAL INTENSIFIED OFF,
                80  SY-VLINE.
      SAVE_BELNR = T_LISTE-BELNR.
    ELSE.
      WRITE:/       SY-VLINE,
             12     SY-VLINE,
             17     SY-VLINE,
             18(62) TEXT COLOR COL_NORMAL INTENSIFIED OFF,
             80     SY-VLINE.
    ENDIF.

  ENDLOOP.
  ULINE.

ENDFORM.                               " WRITE_LIST_PROTOCOL

*---------------------------------------------------------------------*
*       FORM WRITE_PROTOCOL                                           *
*---------------------------------------------------------------------*
*       writes normal protocol                                        *
*---------------------------------------------------------------------*
FORM WRITE_PROTOCOL.
  DATA: TEXT(62)   TYPE C.

*- selected items-----
  IF NOT ( NUM_BSIS_SELECT = 0  ).
    TEXT = TEXT-NX1.
    REPLACE '&TAB' WITH 'BSIS' INTO TEXT.
    WRITE:/        SY-VLINE,
            2(58)  TEXT COLOR COL_KEY INTENSIFIED ON,
            60(1)  SY-VLINE,
            61(19) NUM_BSIS_SELECT COLOR COL_NORMAL INTENSIFIED OFF,
            80     SY-VLINE.
  ENDIF.
  IF NOT ( NUM_BSAS_SELECT = 0 ).
    TEXT = TEXT-NX1.
    REPLACE '&TAB' WITH 'BSAS' INTO TEXT.
    WRITE:/        SY-VLINE,
            2(58)  TEXT COLOR COL_KEY INTENSIFIED ON,
            60(1)  SY-VLINE,
            61(19) NUM_BSAS_SELECT COLOR COL_NORMAL INTENSIFIED OFF,
            80     SY-VLINE.
  ENDIF.
  IF NOT ( NUM_BSEG_SELECT = 0 ).
    WRITE:/       SY-VLINE,
            2(58) TEXT-NB1 COLOR COL_KEY INTENSIFIED ON,
            60(1)  SY-VLINE,
            61(19) NUM_BSEG_SELECT COLOR COL_NORMAL INTENSIFIED OFF,
            80     SY-VLINE.
  ENDIF.
*- changed items-----
  IF NUM_BSIS_UPDATE > 0.
    TEXT = TEXT-NX2.
    REPLACE '&TAB' WITH 'BSIS' INTO TEXT.
    WRITE:/        SY-VLINE,
             2(58) TEXT COLOR COL_KEY INTENSIFIED ON,
            60(1)  SY-VLINE,
            61(19) NUM_BSIS_UPDATE COLOR COL_NORMAL INTENSIFIED OFF,
            80     SY-VLINE.
  ENDIF.
  IF NUM_BSAS_UPDATE > 0.
    TEXT = TEXT-NX2.
    REPLACE '&TAB' WITH 'BSAS' INTO TEXT.
    WRITE:/        SY-VLINE,
             2(58) TEXT COLOR COL_KEY INTENSIFIED ON,
             60(1)  SY-VLINE,
             61(19) NUM_BSAS_UPDATE COLOR COL_NORMAL INTENSIFIED OFF,
             80     SY-VLINE.
  ENDIF.
  IF NUM_BSEG_UPDATE > 0.
    WRITE:/        SY-VLINE,
             2(58) TEXT-NB2 COLOR COL_KEY INTENSIFIED ON,
            60(1)  SY-VLINE,
            61(19) NUM_BSEG_UPDATE COLOR COL_NORMAL INTENSIFIED OFF,
            80     SY-VLINE.
  ENDIF.

  ULINE.

  ADD NUM_BSIS_SELECT TO NUM_BSAS_SELECT.
  IF NUM_BSAS_SELECT = 0.
    ULINE.
    WRITE:/       SY-VLINE,
            2(78) TEXT-NSB COLOR COL_NEGATIVE,
            80    SY-VLINE.
    ULINE.
  ENDIF.

ENDFORM.

*>>>> BEGIN OF INSERTION - NOTE 66156
*&---------------------------------------------------------------------*
*&      Form  CHECK_ACCOUNT_BALANCE_2
*&---------------------------------------------------------------------*
FORM CHECK_ACCOUNT_BALANCE_2.

  DATA:   D_GJAHR LIKE BSEG-GJAHR.
* Account balance
  PERFORM READ_T001 USING P_BUKRS.
  PERFORM READ_CURRENT_YEAR CHANGING D_GJAHR.
  PERFORM READ_CURRENCY_T001A USING P_BUKRS.
* Transaktionswährung & Hauswährung
  PERFORM READ_GLDB_2 USING P_BUKRS P_SAKNR D_GJAHR.
  PERFORM READ_BSIS_2 USING P_BUKRS P_SAKNR.

* BEGIN OF INSERTION
  LOOP AT T_BALANCE_ITEMS_TW WHERE VALUE = 0.
    DELETE T_BALANCE_ITEMS_TW.
  ENDLOOP.

  LOOP AT T_BALANCE_ACCOUNT_TW WHERE VALUE = 0.
    DELETE T_BALANCE_ACCOUNT_TW.
  ENDLOOP.

  LOOP AT T_BALANCE_ITEMS_HW WHERE VALUE1 = 0
                             AND   VALUE2 = 0
                             AND   VALUE3 = 0.
    DELETE T_BALANCE_ITEMS_HW.
  ENDLOOP.

  LOOP AT T_BALANCE_ACCOUNT_HW WHERE VALUE1 = 0
                               AND   VALUE2 = 0
                               AND   VALUE3 = 0.
    DELETE T_BALANCE_ACCOUNT_HW.
  ENDLOOP.
* END OF INSERTION

  IF T_BALANCE_ITEMS_HW[] <> T_BALANCE_ACCOUNT_HW[] OR
     T_BALANCE_ITEMS_TW[] <> T_BALANCE_ACCOUNT_TW[].
    PERFORM DEQUEUE_ACCOUNT.
    MESSAGE E099.
*   Summe der Einzelposten stimmt nicht mit Kontensaldo überein. ->
  ENDIF.
ENDFORM.                               " CHECK_ACCOUNT_BALANCE_2

*&---------------------------------------------------------------------*
*&      Form  READ_GLDB_2
*&---------------------------------------------------------------------*
FORM READ_GLDB_2 USING XBUKRS LIKE GLT0-BUKRS
                       XSAKNR LIKE GLT0-RACCT
                       XGJAHR LIKE GLT0-RYEAR.

  DATA: TYPE(3) TYPE C,
        CURT    LIKE BK_METHODE-CURT1.
  DATA: RESULT LIKE T_BALANCE_ACCOUNT_TW OCCURS 1 WITH HEADER LINE.

  CLEAR T_BALANCE_ACCOUNT_TW.
  CLEAR T_BALANCE_ACCOUNT_HW.

* Transaktionswährung
  CLEAR RESULT. REFRESH RESULT.
  CLEAR XGLEDTAB. REFRESH XGLEDTAB.
  TYPE = 'TW'.
  XGLEDTAB-RLDNR      = '00'.
  APPEND XGLEDTAB.
  PERFORM READ_LEDGER TABLES RESULT XGLEDTAB
                              USING XBUKRS XSAKNR XGJAHR TYPE.

  LOOP AT RESULT.
    T_BALANCE_ACCOUNT_TW-WAERS = RESULT-WAERS.
    T_BALANCE_ACCOUNT_TW-VALUE = RESULT-VALUE.
    APPEND T_BALANCE_ACCOUNT_TW.
  ENDLOOP.

* 1. Hauswährung
  CLEAR RESULT. REFRESH RESULT.
  CLEAR XGLEDTAB. REFRESH XGLEDTAB.
  TYPE = 'HW1'.
  CURT = BK_METHODE-CURT1.
  XGLEDTAB-RLDNR      = '00'.
  APPEND XGLEDTAB.

  PERFORM READ_LEDGER TABLES RESULT XGLEDTAB
                              USING XBUKRS XSAKNR XGJAHR TYPE.

  READ TABLE RESULT INDEX 1.
  T_BALANCE_ACCOUNT_HW-WAERS1 = RESULT-WAERS.
  T_BALANCE_ACCOUNT_HW-VALUE1 = RESULT-VALUE.

*2. Hauswährung
  IF BK_METHODE-CURT2 <> SPACE.
    CLEAR RESULT. REFRESH RESULT.
    CLEAR XGLEDTAB. REFRESH XGLEDTAB.
    TYPE = 'HW2'.
    CURT = BK_METHODE-CURT2.
    IF CURT = '30'.
      XGLEDTAB-RLDNR      = '00'.
      XGLEDTAB-CURRNUMBER = '3'.
    ELSE.
      CALL FUNCTION 'G_GIVE_LEDGERS_FOR_GLT0'
           EXPORTING
                BUKRS  = XBUKRS
                CURTP  = CURT
           TABLES
                LEDTAB = XGLEDTAB.
      DESCRIBE TABLE XGLEDTAB LINES SY-TFILL.
      IF SY-TFILL GT 1. SORT XGLEDTAB. ENDIF.
      READ TABLE XGLEDTAB INDEX 1.
    ENDIF.

    PERFORM READ_LEDGER TABLES RESULT XGLEDTAB
                                USING XBUKRS XSAKNR XGJAHR TYPE.

    READ TABLE RESULT INDEX 1.
    T_BALANCE_ACCOUNT_HW-WAERS2 = RESULT-WAERS.
    T_BALANCE_ACCOUNT_HW-VALUE2 = RESULT-VALUE.
  ENDIF.
  IF BK_METHODE-CURT3 <> SPACE.
*3. Hauswährung
    CLEAR RESULT. REFRESH RESULT.
    CLEAR XGLEDTAB. REFRESH XGLEDTAB.
    TYPE = 'HW3'.
    CURT = BK_METHODE-CURT3.
    IF CURT = '30'.
      XGLEDTAB-RLDNR      = '00'.
      XGLEDTAB-CURRNUMBER = '3'.
    ELSE.
      CALL FUNCTION 'G_GIVE_LEDGERS_FOR_GLT0'
           EXPORTING
                BUKRS  = XBUKRS
                CURTP  = CURT
           TABLES
                LEDTAB = XGLEDTAB.
      DESCRIBE TABLE XGLEDTAB LINES SY-TFILL.
      IF SY-TFILL GT 1. SORT XGLEDTAB. ENDIF.
      READ TABLE XGLEDTAB INDEX 1.
    ENDIF.

    PERFORM READ_LEDGER TABLES RESULT XGLEDTAB
                         USING XBUKRS XSAKNR XGJAHR TYPE.

    READ TABLE RESULT INDEX 1.
    T_BALANCE_ACCOUNT_HW-WAERS3 = RESULT-WAERS.
    T_BALANCE_ACCOUNT_HW-VALUE3 = RESULT-VALUE.
  ENDIF.
  APPEND T_BALANCE_ACCOUNT_HW.

  SORT T_BALANCE_ACCOUNT_TW.

ENDFORM.                               " READ_GLDB_2

*&---------------------------------------------------------------------*
*&      Form  READ_BSIS_2
*&---------------------------------------------------------------------*
FORM READ_BSIS_2 USING BUKRS LIKE BSIS-BUKRS
                       SAKNR LIKE BSIS-HKONT.

  CLEAR T_BALANCE_ITEMS_HW. REFRESH T_BALANCE_ITEMS_HW.
  CLEAR T_BALANCE_ITEMS_TW. REFRESH T_BALANCE_ITEMS_TW.

  SELECT * FROM  BSIS
         WHERE  BUKRS = BUKRS
         AND    HKONT = SAKNR.

    IF ( BSIS-PSWSL IS INITIAL OR BSIS-PSWBT IS INITIAL ) AND
         BSIS-XARCH NE 'X'.

      SELECT SINGLE * FROM BSEG WHERE BUKRS = BSIS-BUKRS
                                AND   BELNR = BSIS-BELNR
                                AND   GJAHR = BSIS-GJAHR
                                AND   BUZEI = BSIS-BUZEI.
      IF SY-SUBRC <> 0.
        MESSAGE A500(FE) WITH 'BSEG nicht gefunden'.        "#EC NOTEXT
      ENDIF.

      BSIS-PSWSL = BSEG-PSWSL.
      BSIS-PSWBT = BSEG-PSWBT.
    ENDIF.

    IF BSIS-SHKZG = 'S'.
      T_BALANCE_ITEMS_HW-WAERS1 = BK_METHODE-HWAE1.
      T_BALANCE_ITEMS_HW-WAERS2 = BK_METHODE-HWAE2.
      T_BALANCE_ITEMS_HW-WAERS3 = BK_METHODE-HWAE3.
      T_BALANCE_ITEMS_HW-VALUE1 = BSIS-DMBTR.
      T_BALANCE_ITEMS_HW-VALUE2 = BSIS-DMBE2.
      T_BALANCE_ITEMS_HW-VALUE3 = BSIS-DMBE3.
      T_BALANCE_ITEMS_TW-WAERS  = BSIS-PSWSL.
      T_BALANCE_ITEMS_TW-VALUE  = BSIS-PSWBT.
    ELSE.
      T_BALANCE_ITEMS_HW-WAERS1 = BK_METHODE-HWAE1.
      T_BALANCE_ITEMS_HW-WAERS2 = BK_METHODE-HWAE2.
      T_BALANCE_ITEMS_HW-WAERS3 = BK_METHODE-HWAE3.
      T_BALANCE_ITEMS_HW-VALUE1 = - BSIS-DMBTR.
      T_BALANCE_ITEMS_HW-VALUE2 = - BSIS-DMBE2.
      T_BALANCE_ITEMS_HW-VALUE3 = - BSIS-DMBE3.
      T_BALANCE_ITEMS_TW-WAERS  = BSIS-PSWSL.
      T_BALANCE_ITEMS_TW-VALUE  = - BSIS-PSWBT.
    ENDIF.
    COLLECT T_BALANCE_ITEMS_HW.
    COLLECT T_BALANCE_ITEMS_TW.
  ENDSELECT.

  SORT T_BALANCE_ITEMS_TW.

ENDFORM.                               " READ_BSIS_2

*&---------------------------------------------------------------------*
*&      Form  CHECK_ACCOUNT2
*&---------------------------------------------------------------------*
*       Check some things for XOPVW
*----------------------------------------------------------------------*
FORM CHECK_ACCOUNT2.
  TABLES: TBSL, T030.

  DATA: CHECK_BSCHL LIKE BSIS-BSCHL OCCURS 10 WITH HEADER LINE.

* Belege schon archiviert?
  SELECT * FROM BSIS WHERE BUKRS EQ P_BUKRS
                     AND   HKONT EQ P_SAKNR
                     AND   XARCH EQ 'X'.
    MESSAGE E146.
*    Es sind schon Belege archiviert worden. Änderung ist nicht möglich
  ENDSELECT.

* Vorgangsschlüssel MVA und VVA sind nicht erlaubt.
  SELECT * FROM T030 WHERE KTOPL = T001-KTOPL
                      AND  ( KTOSL = 'MVA' OR KTOSL = 'VVA' ).
    IF T030-KONTS = P_SAKNR OR T030-KONTH = P_SAKNR.
      MESSAGE E145(FH) WITH P_SAKNR.
*    Konto wird in Kontenfindung für Vorgang MVA oder VVA verwendet
    ENDIF.
  ENDSELECT.
* Buchungsschlüsseln dürfen nicht für KOART = M sein
  SELECT BSCHL INTO TABLE CHECK_BSCHL FROM BSIS
                                      WHERE BUKRS EQ P_BUKRS
                                      AND   HKONT EQ P_SAKNR.
  SORT CHECK_BSCHL.
  DELETE ADJACENT DUPLICATES FROM CHECK_BSCHL.
  LOOP AT CHECK_BSCHL.
    SELECT SINGLE KOART INTO TBSL-KOART FROM TBSL
                                        WHERE BSCHL = CHECK_BSCHL.
    IF SY-SUBRC NE 0.
      MESSAGE E143(FH) WITH CHECK_BSCHL.
*      Verwendeter Buchungsschlüssel & ist nicht mehr in der Kontenfindu
    ELSEIF TBSL-KOART = 'M'.
      MESSAGE E144(FH) WITH P_SAKNR CHECK_BSCHL.
*       Konto & kann nicht geändert werden -> Langtext
    ENDIF.
  ENDLOOP.

ENDFORM.                               " CHECK_ACCOUNT2
*>>>> END OF INSERTION - NOTE 66156 <<<<

INCLUDE FSACHCDC.
*INCLUDE EPA00F00.                      "DELETE - NOTE 175960
INCLUDE ZEPA00F00.                      "INSERT - NOTE 175960

*&---------------------------------------------------------------------*
*&      Form  READ_CURRENCY_T001A
*&---------------------------------------------------------------------*
FORM READ_CURRENCY_T001A USING P_BUKRS LIKE T001-BUKRS.
  TABLES: X001.
  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
       EXPORTING
            I_BUKRS                = P_BUKRS
       IMPORTING
            E_X001                 = X001
       EXCEPTIONS
            CURRENCY_2_NOT_DEFINED = 1
            CURRENCY_3_NOT_DEFINED = 2
            OTHERS                 = 6.
  BK_METHODE-HWAE1 = T001-WAERS.
  BK_METHODE-HWAE2 = X001-HWAE2.
  BK_METHODE-HWAE3 = X001-HWAE3.
  BK_METHODE-CURT1 = '10'.
  BK_METHODE-CURT2 = X001-CURT2.
  BK_METHODE-CURT3 = X001-CURT3.

ENDFORM.                               " READ_CURRENCY_T001A

*&---------------------------------------------------------------------*
*&      Form  READ_LEDGER
*&---------------------------------------------------------------------*
FORM READ_LEDGER TABLES   RESULT      STRUCTURE T_BALANCE_ACCOUNT_TW
                          XGLEDTAB    STRUCTURE GLEDTAB
                 USING    XBUKRS      LIKE      GLT0-BUKRS
                          XSAKNR      LIKE      GLT0-RACCT
                          XGJAHR      LIKE      GLT0-RYEAR
                          TYPE        TYPE      C.

  DATA: BALANCE LIKE GLT0-TSLVT.

  DATA:    I_GJAHR     LIKE GLT0-RYEAR.
  STATICS: I_RECURSIV  TYPE I.
  CHECK I_RECURSIV < 2.

  CLEAR RESULT. REFRESH RESULT.

  READ TABLE XGLEDTAB INDEX 1.

  SELECT * FROM GLT0
           WHERE RLDNR = XGLEDTAB-RLDNR
           AND   RRCTY = '0'
           AND   RVERS = '001'
           AND   BUKRS = XBUKRS
           AND   RYEAR = XGJAHR
           AND   RACCT = XSAKNR.
    CASE TYPE.
      WHEN 'TW'.
        RESULT-WAERS = GLT0-RTCUR.
        RESULT-VALUE = GLT0-TSLVT.
        DO GLT0-RPMAX TIMES
           VARYING BALANCE FROM GLT0-TSL01 NEXT GLT0-TSL02.
          RESULT-VALUE = RESULT-VALUE + BALANCE.
        ENDDO.
      WHEN 'HW1'.
        RESULT-WAERS = BK_METHODE-HWAE1.
        RESULT-VALUE = GLT0-HSLVT.
        DO GLT0-RPMAX TIMES
           VARYING BALANCE FROM GLT0-HSL01 NEXT GLT0-HSL02.
          RESULT-VALUE = RESULT-VALUE + BALANCE.
        ENDDO.
      WHEN 'HW2' OR 'HW3'.
        IF XGLEDTAB-CURRNUMBER = '3'.
          GLT0-HSLVT   = GLT0-KSLVT.
          GLT0-HSL01 = GLT0-KSL01. GLT0-HSL02 = GLT0-KSL02.
          GLT0-HSL03 = GLT0-KSL03. GLT0-HSL04 = GLT0-KSL04.
          GLT0-HSL05 = GLT0-KSL05. GLT0-HSL06 = GLT0-KSL06.
          GLT0-HSL07 = GLT0-KSL07. GLT0-HSL08 = GLT0-KSL08.
          GLT0-HSL09 = GLT0-KSL09. GLT0-HSL10 = GLT0-KSL10.
          GLT0-HSL11 = GLT0-KSL11. GLT0-HSL12 = GLT0-KSL12.
          GLT0-HSL13 = GLT0-KSL13. GLT0-HSL14 = GLT0-KSL14.
          GLT0-HSL15 = GLT0-KSL15. GLT0-HSL16 = GLT0-KSL16.
        ENDIF.
        IF TYPE = 'HW2'.
          RESULT-WAERS = BK_METHODE-HWAE2.
        ELSE.
          RESULT-WAERS = BK_METHODE-HWAE3.
        ENDIF.
        RESULT-VALUE = GLT0-HSLVT.
        DO GLT0-RPMAX TIMES
           VARYING BALANCE FROM GLT0-HSL01 NEXT GLT0-HSL02.
          RESULT-VALUE = RESULT-VALUE + BALANCE.
        ENDDO.
    ENDCASE.
    COLLECT RESULT.
  ENDSELECT.

  IF SY-SUBRC = 0.
    CLEAR I_RECURSIV.
  ELSE.
    ADD 1 TO I_RECURSIV.
    I_GJAHR = XGJAHR - 1.
    PERFORM READ_LEDGER TABLES  RESULT
                                XGLEDTAB
                        USING   XBUKRS
                                XSAKNR
                                I_GJAHR
                                TYPE.
  ENDIF.

ENDFORM.                               " READ_LEDGER

* BEGIN OF INSERTION - NOTE 70203
*&---------------------------------------------------------------------*
*&      Form  BKPF_UPDATE                   P30K137533
*&---------------------------------------------------------------------*
FORM BKPF_UPDATE.
*--- Stornovormerkung löschen? -----------------------------------------
  CLEAR D_STORNO.
  SELECT SINGLE * FROM BKPF WHERE BUKRS = T_BSXX-BUKRS
                            AND   GJAHR = T_BSXX-GJAHR
                            AND   BELNR = T_BSXX-BELNR.
  IF BKPF-STBLG NE SPACE.
    D_STORNO = 'X'.
  ENDIF.
ENDFORM.                               " BKPF_UPDATE
* END OF INSERTION - NOTE 70203
