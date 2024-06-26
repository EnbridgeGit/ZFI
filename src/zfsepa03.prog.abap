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
REPORT RFSEPA03 MESSAGE-ID FH LINE-SIZE 80.

INCLUDE FSACHCDF.
INCLUDE FSACHCDV.

TABLES: BSIS,
        BSEG,
        T001.

DATA: BEGIN OF T_BSIS OCCURS 100.
        INCLUDE STRUCTURE BSIS.
DATA: END OF T_BSIS.

DATA: BEGIN OF T_LISTE OCCURS 300,
        GJAHR    LIKE BKPF-GJAHR,
        BELNR    LIKE BKPF-BELNR,
        SORT     TYPE I,
        TABLE(4) TYPE C,
        TYPE     TYPE C,
        OK       TYPE C,
      END OF T_LISTE.

DATA: D_MAX         LIKE SY-TFILL VALUE '100',
      D_BELNR       LIKE BSEG-BELNR.

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
PARAMETERS:    P_LISTE AS CHECKBOX DEFAULT 'X'.    " Protokoll
SELECTION-SCREEN END OF BLOCK B01.

*-----------------------------------------------------------------------
*INITIALIZATION.                               "DELETE - NOTE 175960
*  CALL FUNCTION 'POPUP_DISPLAY_TEXT'          "DELETE - NOTE 175960
*       EXPORTING                              "DELETE - NOTE 175960
*            POPUP_TITLE    = ' '              "DELETE - NOTE 175960
*            TEXT_OBJECT    = 'RFSEPA03'.      "DELETE - NOTE 175960
*                                              "DELETE - NOTE 175960
*  LEAVE PROGRAM.                              "DELETE - NOTE 175960

AT SELECTION-SCREEN ON P_SAKNR.
  PERFORM CHECK_ACCOUNT.               " locked for posting?
  PERFORM CHECK_ACCOUNT3.              "<<<< INSERT - NOTE 66156
* PERFORM ENQUEUE_ACCOUNT.             "<<<< DELETE - NOTE 65191

*-----------------------------------------------------------------------

START-OF-SELECTION.
  PERFORM ENQUEUE_ACCOUNT.             "<<<< INSERT - NOTE 65191
  PERFORM CHECK_AUTHORITY_SKA1_BUK.
  PERFORM CHECK_AUTHORITY_SKA1_BES.
  PERFORM READ_T001 USING P_BUKRS.
*--- BSIS --------------------------------------------------------------
  DO.
    PERFORM SELECT_ITEMS USING SY-TFILL.
    IF SY-TFILL EQ 0.
      EXIT.
    ENDIF.
    LOOP AT T_BSIS.
      PERFORM BSEG_UPDATE.
      PERFORM BSIS_UPDATE.
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
*&      Form  READ_T001
*&---------------------------------------------------------------------*
FORM READ_T001 USING BUKRS LIKE T001-BUKRS.
  SELECT SINGLE * FROM T001 WHERE BUKRS = BUKRS.
  IF SY-SUBRC <> 0.
    MESSAGE E001 WITH BUKRS.
  ENDIF.
ENDFORM.                               " READ_T001

*&---------------------------------------------------------------------*
*&      Form  SELECT_ITEMS
*&---------------------------------------------------------------------*
FORM SELECT_ITEMS USING FILL LIKE SY-TFILL.
  SELECT * FROM BSIS INTO TABLE T_BSIS
*                      UP TO D_MAX ROWS
                    WHERE BUKRS EQ P_BUKRS
                      AND HKONT EQ P_SAKNR
                      AND BELNR IN P_BELNR
                      AND XOPVW EQ 'X'.
  FILL = SY-DBCNT.
*--- PROTOCOL ----------------------------------------------------------
  LOOP AT T_BSIS.
    PERFORM SAVE_PROTOCOL USING T_BSIS-BELNR T_BSIS-GJAHR
                                1 'BSIS' CHAR_S 1.
  ENDLOOP.

  SORT T_BSIS BY MANDT BUKRS BELNR GJAHR.

ENDFORM.                               " SELECT_ITEMS

*&---------------------------------------------------------------------*
*&      Form  BSEG_UPDATE
*&---------------------------------------------------------------------*
FORM BSEG_UPDATE.
  SELECT SINGLE * FROM BSEG WHERE BUKRS = T_BSIS-BUKRS
                            AND   GJAHR = T_BSIS-GJAHR
                            AND   BELNR = T_BSIS-BELNR
                            AND   BUZEI = T_BSIS-BUZEI.
  IF BSEG-KTOSL = 'SKV' OR BSEG-KTOSL = 'WVW' OR BSEG-KTOSL = 'SGA'.
     MESSAGE A147.
  ENDIF.
  UPDATE BSEG SET XOPVW = SPACE
            WHERE BUKRS EQ T_BSIS-BUKRS
              AND GJAHR EQ T_BSIS-GJAHR
              AND BELNR EQ T_BSIS-BELNR
              AND BUZEI EQ T_BSIS-BUZEI.
*--- PROTOCOL ----------------------------------------------------------
  PERFORM SAVE_PROTOCOL USING T_BSIS-BELNR T_BSIS-GJAHR
                              2 'BSEG' CHAR_U SY-DBCNT.
ENDFORM.                               " BSEG_UPDATE

*&---------------------------------------------------------------------*
*&      Form  BSIS_UPDATE
*&---------------------------------------------------------------------*
FORM BSIS_UPDATE.
  T_BSIS-XOPVW = SPACE.
  UPDATE BSIS FROM T_BSIS.
*--- PROTOCOL -------------------------------------------
  PERFORM SAVE_PROTOCOL USING T_BSIS-BELNR T_BSIS-GJAHR
                              1 'BSIS' CHAR_U SY-DBCNT.
ENDFORM.                               " BSIS_UPDATE



*&---------------------------------------------------------------------*
*&      Form  CHANGE_ACCOUNT
*&---------------------------------------------------------------------*
FORM CHANGE_ACCOUNT.

  SKB1       = *SKB1.
  SKB1-XOPVW = SPACE.

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
            SAKNR          = P_SAKNR.
ENDFORM.                               " DEQUEUE_ACCOUNT

*---------------------------------------------------------------------*
*       FORM WRITE_LIST_PROTOCOL
*---------------------------------------------------------------------*
*       writes list protocol                                          *
*---------------------------------------------------------------------*
FORM WRITE_LIST_PROTOCOL.

  DATA: TEXT(62)   TYPE C.

  DATA: SAVE_BELNR LIKE BKPF-BELNR.

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
*    writes normal protocol                                           *
*---------------------------------------------------------------------*
FORM WRITE_PROTOCOL.

*- selected items-----
  IF NOT ( NUM_BSIS_SELECT = 0  ).
    WRITE:/        SY-VLINE,
            2(58)  TEXT-NX1 COLOR COL_KEY INTENSIFIED ON,
            60(1)  SY-VLINE,
            61(19) NUM_BSIS_SELECT COLOR COL_NORMAL INTENSIFIED OFF,
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
    WRITE:/        SY-VLINE,
             2(58) TEXT-NX2 COLOR COL_KEY INTENSIFIED ON,
            60(1)  SY-VLINE,
            61(19) NUM_BSIS_UPDATE COLOR COL_NORMAL INTENSIFIED OFF,
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

INCLUDE FSACHCDC.
*INCLUDE EPA00F00.                      "DELETE - NOTE 175960
INCLUDE ZEPA00F00.                      "INSERT - NOTE 175960

*>>>> BEGIN OF INSERTION - NOTE 66156
*&---------------------------------------------------------------------*
*&      Form  CHECK_ACCOUNT3
*&---------------------------------------------------------------------*
FORM CHECK_ACCOUNT3.
  TABLES: T030.
* Belege schon archiviert?
  SELECT * FROM BSIS WHERE BUKRS EQ P_BUKRS
                     AND   HKONT EQ P_SAKNR
                     AND   XARCH EQ 'X'.
    MESSAGE E146.
*    Es sind schon Belege archiviert worden. Änderung ist nicht möglich
  ENDSELECT.

* Keine Bankverrechnungskonten
  SELECT * FROM T030  WHERE KTOPL = T001-KTOPL
                        AND  ( KTOSL = 'SKV' OR
                               KTOSL = 'WVW' OR
                               KTOSL = 'SGA' ).
    IF T030-KONTS = P_SAKNR OR T030-KONTH = P_SAKNR.
      MESSAGE E147.
*     Konto kann nicht geändert werden -> Langtext
    ENDIF.
  ENDSELECT.
ENDFORM.                               " CHECK_ACCOUNT3

*>>>> END OF INSERTION - NOTE 66156 <<<<
