*&---------------------------------------------------------------------*
*&  Include           ZFI_WBS_STATUSCHANGE_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZFI_WBS_STATUSCHANGE                           *
* Include            :  ZFI_WBS_STATUSCHANGE_FRM                       *
* Author             :  Ashok Madasu                                   *
* Date               :  20-Aug-2018                                    *
* Technical Contact  :  Ashok Madasu                                   *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  WBS status change report                       *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 22/08/2019   akmadasu    D30K930092  CHG0138619 - in-service and     *
*                          D30K930231  completion date updation to WBSe*
* 22/08/2019   JOOKONTR    D30K930297  CHG0164542 Changes in all       *
*                                      radio buttions options          *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  CLEAR:GT_PROJ[],GT_PRHI[],GT_PRPS[],GT_JEST[].
  IF R_CLSD IS NOT INITIAL OR R_REV IS NOT INITIAL OR RT_CLSD IS NOT INITIAL  "Added Condition by JOOKONTR CHG0138619
                           OR R_TTOR IS NOT INITIAL OR R_RTOT IS NOT INITIAL  "Added Condition by JOOKONTR CHG0164542
                           OR R_CRTOR IS NOT INITIAL.  "Added Condition by JOOKONTR CHG0164542
    SELECT PSPNR
           PSPID " ADDED BY AKMADASU FOR CHG0138619
           FROM PROJ INTO TABLE GT_PROJ WHERE PSPNR IN S_PSPNR1.
    IF SY-SUBRC IS INITIAL.
      SELECT POSNR PSPHI FROM PRHI INTO TABLE GT_PRHI
        FOR ALL ENTRIES IN GT_PROJ
        WHERE PSPHI = GT_PROJ-PSPNR.
      IF SY-SUBRC IS INITIAL.
        SELECT PSPNR
               POSID " ADDED BY AKMADASU FOR CHG0138619
               PSPHI " ADDED BY AKMADASU FOR CHG0138619
               STUFE " ADDED BY JOOKONTR FOR CHG0138619
               OBJNR FROM PRPS INTO TABLE GT_PRPS FOR ALL ENTRIES IN GT_PRHI
                                           WHERE PSPNR = GT_PRHI-POSNR.
        IF SY-SUBRC IS INITIAL.
          SORT GT_PRPS BY PSPNR.
          SELECT OBJNR STAT INACT FROM JEST INTO TABLE GT_JEST
                            FOR ALL ENTRIES IN GT_PRPS
                            WHERE OBJNR = GT_PRPS-OBJNR
                            AND   INACT  = SPACE.
          IF SY-SUBRC IS INITIAL.
            SORT GT_JEST BY OBJNR
                            STAT. " added by akmadasu CHG0138619.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT PSPNR
           POSID " ADDED BY AKMADASU FOR CHG0138619
           PSPHI  " ADDED BY AKMADASU FOR CHG0138619
           STUFE " ADDED BY JOOKONTR FOR CHG0138619
*           OBJNR FROM PRPS INTO TABLE gt_prps WHERE PSPNR IN S_PSPNR.    "Commented by JOOKONTR CHG0164542
           OBJNR FROM PRPS INTO TABLE GT_PRPS_TMP WHERE PSPNR IN S_PSPNR. "ADDED BY JOOKONTR CHG0164542
    IF SY-SUBRC IS INITIAL.
*&--Start of code added by JOOKONTR CHG0164542
      SELECT POSNR PSPHI FROM PRHI INTO TABLE GT_PRHI
        FOR ALL ENTRIES IN GT_PRPS_TMP
        WHERE PSPHI = GT_PRPS_TMP-PSPHI.
      IF SY-SUBRC EQ 0.
        SELECT PSPNR
               POSID
               PSPHI
               STUFE
               OBJNR FROM PRPS INTO TABLE GT_PRPS
          FOR ALL ENTRIES IN GT_PRHI WHERE PSPNR = GT_PRHI-POSNR.
        IF SY-SUBRC EQ 0.
          CLEAR GS_PRPS.
*           READ TABLE GT_PRPS_TEMP INTO Gs_PRPS with KEY STUFE
        ENDIF.
      ENDIF.
*&--End of code added by JOOKONTR CHG0164542
      SORT GT_PRPS BY PSPNR.
      SELECT OBJNR STAT INACT FROM JEST INTO TABLE GT_JEST
                        FOR ALL ENTRIES IN GT_PRPS
                        WHERE OBJNR = GT_PRPS-OBJNR
                        AND   INACT  = SPACE.
      IF SY-SUBRC IS INITIAL.
        SORT GT_JEST BY OBJNR
                        STAT. " added by akmadasu CHG0138619
      ENDIF.
    ENDIF.
  ENDIF.
*&-- Srtst of Code added by akmadasu CHG0138619
  SELECT * FROM TJ02T
           INTO TABLE GT_TJ02T
           FOR ALL ENTRIES IN GT_JEST
           WHERE ISTAT EQ GT_JEST-STAT
           AND SPRAS EQ SY-LANGU.
  IF SY-SUBRC EQ 0.
    SORT GT_TJ02T BY ISTAT.
  ENDIF.
*&-- End of code added by akmadasu CHG0138619
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_STATUS .
  DATA: GS_RETURN TYPE BAPIRETURN1,
        GT_WBS    TYPE TABLE OF BAPI_WBS_MNT_SYSTEM_STATUS,
        GS_WBS    TYPE  BAPI_WBS_MNT_SYSTEM_STATUS,
        GT_BAPIRET2 TYPE TABLE OF BAPIRET2,
        GS_BAPIRET2 TYPE BAPIRET2,
        GT_RESULT TYPE TABLE OF BAPI_STATUS_RESULT,
        GS_RESULT TYPE BAPI_STATUS_RESULT.
**--START OF CHANGES BY AKMADASU CHG0138619
  TYPES: BEGIN OF LTY_PROJ,
         PSPNR TYPE PS_INTNR,
         PSPID TYPE PS_PSPID,
         END OF LTY_PROJ.
  DATA: LV_PLFAZ(10) TYPE C,
        LV_PLSEZ(10) TYPE C,
        LV_EPROG(10) TYPE C,
        LV_SPROG(10) TYPE C.
  DATA:LT_PROJ TYPE TABLE OF LTY_PROJ,
       LS_PROJ TYPE LTY_PROJ.
  CONSTANTS: LC_REL  TYPE CHAR3 VALUE 'REL',
           LC_CRTD TYPE CHAR4 VALUE 'CRTD',
           LC_TECO TYPE CHAR4 VALUE 'TECO',
           LC_X    TYPE FLAG  VALUE 'X'.
*&--Start of code added by JOOKONTR FOR CHG0138619
  IF RT_CLSD IS NOT INITIAL.
    R_CLSD = RT_CLSD.
  ENDIF.
*&--Start of code added by JOOKONTR FOR CHG0138619

**--end OF CHANGES BY AKMADASU CHG0138619
  LOOP AT GT_PRPS INTO GS_PRPS.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = GS_PRPS-PSPNR
      IMPORTING
        OUTPUT = GS_FINAL-PSPNR.
    IF GS_FINAL-PSPNR+3(2) EQ '11' AND GS_FINAL-PSPNR+6(3) CA SY-ABCDE.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          INPUT  = GS_PRPS-PSPNR
        IMPORTING
          OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*&--Start of code added by JOOKONTR FOR CHG0164542
      CLEAR GS_JEST.
      READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                      BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        CLEAR GS_TJ02T.
        READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                   SPRAS = 'EN'
                                                   BINARY SEARCH.
        IF SY-SUBRC EQ 0.
        ENDIF.
      ENDIF.
*&--End of code added by JOOKONTR FOR CHG0164542
      IF R_RTOT IS NOT INITIAL.
        GS_FINAL-ST_NEW = 'TECO'.
        GS_FINAL-ST_OLD = GS_TJ02T-TXT04.  " 'REL'.
      ELSEIF R_TTOR IS NOT INITIAL.
        GS_FINAL-ST_NEW = 'REL'.
        GS_FINAL-ST_OLD = GS_TJ02T-TXT04.   " 'TECO'.
      ELSEIF R_CLSD IS NOT INITIAL.
        GS_FINAL-ST_NEW = 'CLAD'.
        GS_FINAL-ST_OLD = GS_TJ02T-TXT04.   " 'REL/TECO'.
      ELSEIF R_REV IS NOT INITIAL.
        GS_FINAL-ST_NEW = 'TECO'.
        GS_FINAL-ST_OLD = GS_TJ02T-TXT04.   " 'CLSD'.
**-- START OF CHANGES BY AKMADASU FOR CHG0138619
*      ELSEIF R_CRTOR is NOT INITIAL.
      ELSEIF R_REV IS NOT INITIAL.
        GS_FINAL-ST_NEW = LC_REL. " 'REL'.
        GS_FINAL-ST_OLD = LC_CRTD. " 'CRTD'.
**-- END OF CHANGES BY AKMDASU FOR CHG0138619
      ENDIF.
      GS_FINAL-MESSAGE = 'WBS Element contains character value'.
      APPEND GS_FINAL TO GT_FINAL.
      CLEAR:GS_PRPS,GS_FINAL.
      CONTINUE.
    ELSE.
      IF R_RTOT = 'X'.
        READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                                 STAT = 'I0002'
                                                 BINARY SEARCH. " ADDED BY AKMADASU CHG0138619
        IF SY-SUBRC IS INITIAL." AND GS_JEST-STAT EQ 'I0002'.
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*&--Start of code added by JOOKONTR CHG0164542
          READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                     SPRAS = 'EN'
                                                     BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
*        GS_FINAL-PSPNR = GS_PRPS-PSPNR.
          GS_FINAL-ST_NEW = 'TECO'.
*          GS_FINAL-ST_OLD = 'REL'. Commented by JOOKNTR CHG0164542
          APPEND GS_FINAL TO GT_FINAL.
          CLEAR:GS_FINAL,GS_JEST.
        ELSE.
*&--Start of code added by JOOKONTR CHG0164542
          CLEAR GS_JEST.
          READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                 STAT = 'I0001' "Created
                                 INACT = '' BINARY SEARCH.
          IF SY-SUBRC IS NOT INITIAL.
            READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                       STAT = 'I0002'   "Release
                       INACT = '' BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                         STAT = 'I0045'  "TECO
                         INACT = '' BINARY SEARCH.
              IF SY-SUBRC IS NOT INITIAL.
                READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                           STAT = 'I0046'   "Closed
                           INACT = '' BINARY SEARCH.
              ENDIF.
            ENDIF.
          ENDIF.
          IF GS_JEST IS NOT INITIAL.
            CLEAR GS_TJ02T.
            READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                       SPRAS = 'EN'
                                                       BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
            ENDIF.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*          GS_FINAL-ST_NEW = 'TECO'.      " Commented by JOOKONTR CHG0164542
*          GS_FINAL-ST_OLD = 'REL'.       " Commented by JOOKONTR CHG0164542
          GS_FINAL-MESSAGE = 'WBS Element not in release status'.
*          APPEND GS_FINAL TO GT_FINAL.     "Commented by JOOKONTR CHG0164542
          APPEND GS_FINAL TO GT_FINAL_ER.
          CLEAR:GS_FINAL.
        ENDIF.
**-- START OF CHANGES BY AKMADASU CHG0138619
      ELSEIF R_CRTOR = LC_X. " 'X' .
        READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                                  STAT = 'I0001'
                                                  BINARY SEARCH.
        IF SY-SUBRC IS INITIAL." AND GS_JEST-STAT EQ 'I0002'.
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*&--Start of code added by JOOKONTR  CHG0164542
          READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                     SPRAS = 'EN'
                                                     BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
*        GS_FINAL-PSPNR = GS_PRPS-PSPNR.
          GS_FINAL-ST_NEW = LC_REL. " 'REL'.
*          GS_FINAL-ST_OLD = LC_CRTD. " 'CRTD'. Commented by JOOKNTR CHG0164542
          APPEND GS_FINAL TO GT_FINAL.
          CLEAR:GS_FINAL,GS_JEST.
        ELSE.
*&--Start of code added by JOOKONTR CHG0164542
          CLEAR GS_JEST.
          READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                 STAT = 'I0001' "Created
                                 INACT = '' BINARY SEARCH.
          IF SY-SUBRC IS NOT INITIAL.
            READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                       STAT = 'I0002'   "Release
                       INACT = '' BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                         STAT = 'I0045'  "TECO
                         INACT = '' BINARY SEARCH.
              IF SY-SUBRC IS NOT INITIAL.
                READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                           STAT = 'I0046' "Closed
                           INACT = '' BINARY SEARCH.
              ENDIF.
            ENDIF.
          ENDIF.
          IF GS_JEST IS NOT INITIAL.
            CLEAR GS_TJ02T.
            READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                       SPRAS = 'EN'
                                                       BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
            ENDIF.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*          GS_FINAL-ST_NEW = LC_REL. " 'REL'.     " Commented by JOOKONTR CHG0164542
*          GS_FINAL-ST_OLD = LC_CRTD. " 'CRTD'.   " Commented by JOOKONTR CHG0164542
          GS_FINAL-MESSAGE = TEXT-603.
          APPEND GS_FINAL TO GT_FINAL_ER.
          CLEAR:GS_FINAL.
        ENDIF.
**-- END OF CHANGES BY AKMADASU FOR CHG0138619
      ELSEIF R_TTOR = 'X'.
        READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                                 STAT = 'I0045'
                                                 BINARY SEARCH. " ADDED BY AKMADASU CHG0138619
        IF SY-SUBRC IS INITIAL." AND GS_JEST-STAT EQ 'I0045'.

          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR."GS_PRPS-PSPNR.
*&--Start of code added by JOOKONTR CHG0164542
          READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                     SPRAS = 'EN'
                                                     BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
*        GS_FINAL-PSPNR = GS_PRPS-PSPNR.
          GS_FINAL-ST_NEW = 'REL'.
*          GS_FINAL-ST_OLD = 'TECO'. Commented by Jookontr CHG0164542
          APPEND GS_FINAL TO GT_FINAL.
          CLEAR:GS_FINAL,GS_JEST.
        ELSE.
*&--Start of code added by JOOKONTR CHG0164542
          CLEAR GS_JEST.
          READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                 STAT = 'I0001' "Created
                                 INACT = '' BINARY SEARCH.
          IF SY-SUBRC IS NOT INITIAL.
            READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                       STAT = 'I0002'   "Release
                       INACT = '' BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                         STAT = 'I0045'  "TECO
                         INACT = '' BINARY SEARCH.
              IF SY-SUBRC IS NOT INITIAL.
                READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                           STAT = 'I0046'   "Closed
                           INACT = '' BINARY SEARCH.
              ENDIF.
            ENDIF.
          ENDIF.
          IF GS_JEST IS NOT INITIAL.
            CLEAR GS_TJ02T.
            READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                       SPRAS = 'EN'
                                                       BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
            ENDIF.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*          GS_FINAL-ST_NEW = 'REL'.     " Commented by JOOKONTR CHG0164542
*          GS_FINAL-ST_OLD = 'TECO'.    " Commented by JOOKONTR CHG0164542
          GS_FINAL-MESSAGE = 'WBS Element not in TECO status'.
*          APPEND GS_FINAL TO GT_FINAL.     "Commented by JOOKONTR CHG0164542
          APPEND GS_FINAL TO GT_FINAL_ER.
          CLEAR:GS_FINAL.
        ENDIF.
      ELSEIF R_CLSD = 'X'.
        READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                                 STAT = 'I0045'
                                                 BINARY SEARCH. " ADDED BY AKMADASU CHG0138619
        IF SY-SUBRC IS INITIAL." AND GS_JEST-STAT EQ 'I0045'.

          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR."GS_PRPS-PSPNR.
*&--Start of code added by JOOKONTR CHG0164542
          READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                     SPRAS = 'EN'
                                                     BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
          GS_FINAL-ST_NEW = 'CLSD'.
*          GS_FINAL-ST_OLD = 'TECO'. Commented by JOOKONTR CHG0164542
          APPEND GS_FINAL TO GT_FINAL.
          CLEAR:GS_FINAL,GS_JEST.
        ELSE.
*&--Start of code added by JOOKONTR CHG0164542
          CLEAR GS_JEST.
          READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                 STAT = 'I0001' "Created
                                 INACT = '' BINARY SEARCH.
          IF SY-SUBRC IS NOT INITIAL.
            READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                       STAT = 'I0002'   "Release
                       INACT = '' BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                         STAT = 'I0045'  "TECO
                         INACT = '' BINARY SEARCH.
              IF SY-SUBRC IS NOT INITIAL.
                READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                           STAT = 'I0046'   "Closed
                           INACT = '' BINARY SEARCH.
              ENDIF.
            ENDIF.
          ENDIF.
          IF GS_JEST IS NOT INITIAL.
            CLEAR GS_TJ02T.
            READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                       SPRAS = 'EN'
                                                       BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
            ENDIF.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
*&--Start of code commented by JOOKONTR CHG0164542
*          READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
*                                         STAT = 'I0002'
*                                         BINARY SEARCH. " ADDED BY AKMADASU CHG0138619
*          IF SY-SUBRC IS INITIAL." AND GS_JEST-STAT EQ 'I0002'.
*            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*              EXPORTING
*                INPUT  = GS_PRPS-PSPNR
*              IMPORTING
*                OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*            GS_FINAL-ST_NEW = 'CLSD'.
*            GS_FINAL-ST_OLD = 'REL'.
*            APPEND GS_FINAL TO GT_FINAL.
*            CLEAR:GS_FINAL,GS_JEST.
*          ELSE.
**&-- Start of code added by akmadasu CHG0138619
*            READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR.
*            IF SY-SUBRC EQ 0.
*              READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT BINARY SEARCH.
*              IF SY-SUBRC IS INITIAL.
*                GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
*                CLEAR: GS_TJ02T.
*              ENDIF.
*              CLEAR: GS_JEST.
*            ENDIF.
*&--End of code commented by JOOKONTR CHG0164542
*&-- End of code added by akmadasu CHG0138619
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*          GS_FINAL-ST_NEW = 'CLSD'.      " Commented by JOOKONTR CHG0164542
          GS_FINAL-MESSAGE = 'WBS Element not in TECO status'.
          APPEND GS_FINAL TO GT_FINAL_ER.  "Added by akmadasu CHG0138619
          CLEAR:GS_FINAL.
*          ENDIF.
        ENDIF.
      ELSEIF R_REV IS NOT INITIAL.
        READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                         STAT = 'I0046' BINARY SEARCH.
        IF SY-SUBRC IS INITIAL." AND GS_JEST-STAT EQ 'I0002'.
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*&--Start of code added by JOOKONTR CHG0164542
          READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                     SPRAS = 'EN'
                                                     BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
          GS_FINAL-ST_NEW = 'TECO'.
*          GS_FINAL-ST_OLD = 'CLSD'. Commented by JOOKONTR CHG0164542
          APPEND GS_FINAL TO GT_FINAL.
          CLEAR:GS_FINAL,GS_JEST.
        ELSE.
*&--Start of code added by JOOKONTR CHG0164542
          CLEAR GS_JEST.
          READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                                 STAT = 'I0001' "Created
                                 INACT = '' BINARY SEARCH.
          IF SY-SUBRC IS NOT INITIAL.
            READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                       STAT = 'I0002'   "Release
                       INACT = '' BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                         STAT = 'I0045'  "TECO
                         INACT = '' BINARY SEARCH.
              IF SY-SUBRC IS NOT INITIAL.
                READ TABLE GT_JEST INTO GS_JEST WITH KEY OBJNR = GS_PRPS-OBJNR
                           STAT = 'I0046'   "Closed
                           INACT = '' BINARY SEARCH.
              ENDIF.
            ENDIF.
          ENDIF.
          IF GS_JEST IS NOT INITIAL.
            CLEAR GS_TJ02T.
            READ TABLE GT_TJ02T INTO GS_TJ02T WITH KEY ISTAT = GS_JEST-STAT
                                                       SPRAS = 'EN'
                                                       BINARY SEARCH.
            IF SY-SUBRC EQ 0.
              GS_FINAL-ST_OLD = GS_TJ02T-TXT04.
            ENDIF.
          ENDIF.
*&--End of code added by JOOKONTR CHG0164542
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              INPUT  = GS_PRPS-PSPNR
            IMPORTING
              OUTPUT = GS_FINAL-PSPNR." GS_PRPS-PSPNR.
*          GS_FINAL-ST_NEW = 'TECO'.        " Commented by JOOKONTR CHG0164542
*          GS_FINAL-ST_OLD = 'CLSD'.        " Commented by JOOKONTR CHG0164542
          GS_FINAL-MESSAGE = 'WBS Element not in Closed status'.
*          APPEND GS_FINAL TO GT_FINAL.     "Commented by JOOKONTR CHG0164542
          APPEND GS_FINAL TO GT_FINAL_ER.
          CLEAR:GS_FINAL.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR:GS_PRPS.
  ENDLOOP.
**--START OF CHANGES BY AKMADASU CHG0138619
  IF GT_PRPS IS NOT INITIAL.
    SELECT PSPNR PSPID FROM PROJ INTO TABLE LT_PROJ
                       FOR ALL ENTRIES IN GT_PRPS
                       WHERE PSPNR = GT_PRPS-PSPHI.
    IF SY-SUBRC IS INITIAL.
      SORT LT_PROJ BY PSPNR.
    ENDIF.
  ENDIF.
*  ENDIF.
  GT_FINAL_TEMP[] = GT_FINAL[].
  SORT :GT_FINAL_TEMP BY PSPNR.
**-- END OF CHANGES BY AKMADASU CHG0138619
  LOOP AT GT_FINAL INTO GS_FINAL.
**--START of changes by akmadasu CHG0138619
    CLEAR:GV_FLAG_D,GV_FLAG_A.
    IF R_REV IS INITIAL. "Condition added by JOOKONTR CHG0164542
      IF GS_FINAL-ST_OLD = 'REL' AND GS_FINAL-ST_NEW = 'TECO'.
        GV_FLAG_D = 'X'.
      ENDIF.
    ENDIF.
    IF GS_FINAL-ST_NEW = 'CLSD' AND ( GS_FINAL-ST_OLD = 'REL' OR GS_FINAL-ST_OLD = 'TECO' ).
      GV_FLAG_D = 'X'.
    ENDIF.
    IF GS_FINAL-ST_OLD = 'CRTD' AND GS_FINAL-ST_NEW = 'REL'.
      GV_FLAG_A = 'X'.
    ENDIF.
*&--Start of code added by JOOKONTR CHG0164542
    IF ( GS_FINAL-ST_OLD = 'CLSD' AND GS_FINAL-ST_NEW = 'TECO' )
      OR ( GS_FINAL-ST_OLD = 'TECO' AND GS_FINAL-ST_NEW = 'REL' ).
      CLEAR: GT_WBS[],GT_RESULT[],GT_BAPIRET2[],GS_RETURN.
      CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
      GS_WBS-WBS_ELEMENT = GS_FINAL-PSPNR.
      GS_WBS-SET_SYSTEM_STATUS = GS_FINAL-ST_NEW.
      IF ( GS_FINAL-ST_OLD = LC_REL
        OR GS_FINAL-ST_OLD = LC_CRTD )  " ADDED BY AKMADASU CHG0138619
        AND R_REV IS INITIAL.           "Added by JOOKONTR CHG0164542
        CLEAR GS_WBS-UNDO_SYSTEM_STATUS.
      ELSE.
        GS_WBS-UNDO_SYSTEM_STATUS = GS_FINAL-ST_OLD.
      ENDIF.
      APPEND GS_WBS TO GT_WBS.
      CLEAR: GS_RESULT,GS_WBS.
      CALL FUNCTION 'BAPI_BUS2054_SET_STATUS'
        IMPORTING
          RETURN              = GS_RETURN
        TABLES
          I_WBS_SYSTEM_STATUS = GT_WBS
**       I_WBS_USER_STATUS   =
          E_RESULT            = GT_RESULT.

      IF GT_RESULT[] IS INITIAL.
        CALL FUNCTION 'BAPI_PS_PRECOMMIT'
          TABLES
            ET_RETURN = GT_BAPIRET2.

        IF GT_BAPIRET2[] IS NOT INITIAL.
          READ TABLE GT_BAPIRET2 INTO GS_BAPIRET2 WITH KEY TYPE = 'E'.
          IF SY-SUBRC IS NOT INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.

            IF SY-SUBRC IS INITIAL.
              GS_FINAL-MESSAGE = 'Status has been changed sucessfully'.
            ELSE.
              GS_FINAL-MESSAGE = 'Status has not been changed sucessfuly'.
            ENDIF.
          ELSE.
**&--Start of code added by JOOKONTR CHG0164542
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            CLEAR GS_FINAL-ST_NEW.
**&--End of code added by JOOKONTR CHG0164542
            GS_FINAL-MESSAGE = GS_BAPIRET2-MESSAGE.
          ENDIF.
        ENDIF.
      ELSE.
**&--Start of code added by JOOKONTR CHG0164542
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR GS_FINAL-ST_NEW.
**&--End of code added by JOOKONTR CHG0164542
        READ TABLE GT_RESULT INTO GS_RESULT WITH KEY MESSAGE_TYPE = 'E'.
        IF SY-SUBRC IS INITIAL.
          GS_FINAL-MESSAGE = GS_RESULT-MESSAGE_TEXT.
        ENDIF.
      ENDIF.
**    ENDIF.
      MODIFY GT_FINAL FROM GS_FINAL TRANSPORTING ST_NEW MESSAGE. "ST_NEW Added by JOOKONTR CHG0164542
      CLEAR:GS_FINAL,GS_RESULT,GS_BAPIRET2.
    ENDIF.
*&--End of ode added by JOOKONTR CHG0164542
    IF GV_FLAG_D = 'X'.
**&--Start of code added by JOOKONTR CHG0164542
      CLEAR: GT_WBS[],GT_RESULT[],GT_BAPIRET2[],GS_RETURN.
      CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
      GS_WBS-WBS_ELEMENT = GS_FINAL-PSPNR.
      GS_WBS-SET_SYSTEM_STATUS = GS_FINAL-ST_NEW.
      IF ( GS_FINAL-ST_OLD = LC_REL
        OR GS_FINAL-ST_OLD = LC_CRTD    " ADDED BY AKMADASU CHG0138619
        OR GS_FINAL-ST_OLD = LC_TECO )  " ADDED BY JOOKONTR CHG0164542
        AND R_REV IS INITIAL.           " Added by JOOKONTR CHG0164542
        CLEAR GS_WBS-UNDO_SYSTEM_STATUS.
      ELSE.
        GS_WBS-UNDO_SYSTEM_STATUS = GS_FINAL-ST_OLD.
      ENDIF.
      APPEND GS_WBS TO GT_WBS.
      CLEAR: GS_RESULT,GS_WBS.
      CALL FUNCTION 'BAPI_BUS2054_SET_STATUS'
        IMPORTING
          RETURN              = GS_RETURN
        TABLES
          I_WBS_SYSTEM_STATUS = GT_WBS
*         I_WBS_USER_STATUS   =
          E_RESULT            = GT_RESULT.

      IF GT_RESULT[] IS INITIAL.
        CALL FUNCTION 'BAPI_PS_PRECOMMIT'
          TABLES
            ET_RETURN = GT_BAPIRET2.

        IF GT_BAPIRET2[] IS NOT INITIAL.
          READ TABLE GT_BAPIRET2 INTO GS_BAPIRET2 WITH KEY TYPE = 'E'.
          IF SY-SUBRC IS NOT INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.
            WAIT UP TO 1 SECONDS.
            READ TABLE GT_FINAL_TEMP INTO GS_FINAL_TEMP WITH KEY PSPNR = GS_FINAL-PSPNR BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
                EXPORTING
                  INPUT  = GS_FINAL_TEMP-PSPNR
                IMPORTING
                  OUTPUT = GS_FINAL_TEMP-PSPNR.
            ENDIF.
            READ TABLE GT_PRPS INTO GS_PRPS WITH KEY PSPNR = GS_FINAL_TEMP-PSPNR BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              READ TABLE LT_PROJ INTO LS_PROJ WITH KEY PSPNR = GS_PRPS-PSPHI BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
                  EXPORTING
                    INPUT  = LS_PROJ-PSPID
                  IMPORTING
                    OUTPUT = GS_PRJ_DEF-PROJECT_DEFINITION.
              ENDIF.
            ENDIF.
            GS_METH_PRJ-REFNUMBER          = '000001'.
            GS_METH_PRJ-OBJECTTYPE         = 'WBS-ELEMENT'. "#EC NOTEXT
            GS_METH_PRJ-METHOD             = 'Update'.      "#EC NOTEXT
            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
              EXPORTING
                INPUT  = GS_FINAL_TEMP-PSPNR
              IMPORTING
                OUTPUT = GS_METH_PRJ-OBJECTKEY.
            APPEND GS_METH_PRJ TO GT_METH_PRJ.
            CLEAR GS_METH_PRJ.
            GS_METH_PRJ-METHOD = 'Save'.                    "#EC NOTEXT
            APPEND GS_METH_PRJ TO GT_METH_PRJ.
            CLEAR GS_METH_PRJ.
            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
              EXPORTING
                INPUT  = GS_FINAL_TEMP-PSPNR
              IMPORTING
                OUTPUT = GS_WBS_ELE-WBS_ELEMENT.
            CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
              EXPORTING
                INPUT  = GS_PROJ-PSPID
              IMPORTING
                OUTPUT = GS_WBS_ELE-PROJECT_DEFINITION.
            GS_WBS_ELE-USER_FIELD_KEY     = 'ZALELEM'.
            IF GS_FINAL-ST_OLD = 'REL' AND GS_FINAL-ST_NEW = 'TECO'.
              IF GS_PRPS-STUFE = '1'. "Added by JOOKONTR FOR CHG0138619
                GS_WBS_ELE-USER_FIELD_DATE1      = P_USR08.
                GS_WBS_ELE_U-USER_FIELD_DATE1    = 'X'.
              ENDIF. "Added by JOOKONTR
            ENDIF.
            IF GS_FINAL-ST_NEW = 'CLSD' AND ( GS_FINAL-ST_OLD = 'REL' OR GS_FINAL-ST_OLD = 'TECO' ).
              IF RT_CLSD NE 'X'.  "Added by JOOKONTR FOR CHG0138619
                IF GS_PRPS-STUFE = '1'. "Added by JOOKONTR FOR CHG0138619
                  GS_WBS_ELE-USER_FIELD_DATE2      = P_USR09.
                  GS_WBS_ELE_U-USER_FIELD_DATE2    = 'X'.
                ENDIF."Added by JOOKONTR FOR CHG0138619
              ENDIF."Added by JOOKONTR FOR CHG0138619
            ENDIF.
            APPEND GS_WBS_ELE TO GT_WBS_ELE.
            GS_WBS_ELE-WBS_ELEMENT           = 'X'.
            GS_WBS_ELE-PROJECT_DEFINITION    = 'X'.
            GS_WBS_ELE_U-USER_FIELD_KEY     = 'X'.
            APPEND GS_WBS_ELE_U TO GT_WBS_ELE_U.
            CLEAR:GS_WBS_ELE_U,GV_CONT,GS_WBS_ELE.
            CALL FUNCTION 'BAPI_PROJECT_MAINTAIN'
              EXPORTING
                I_PROJECT_DEFINITION       = GS_PRJ_DEF
                I_PROJECT_DEFINITION_UPD   = GS_PRJ_DEF_U
              IMPORTING
                RETURN                     = GS_RETURN1
              TABLES
                I_METHOD_PROJECT           = GT_METH_PRJ
                I_WBS_ELEMENT_TABLE_UPDATE = GT_WBS_ELE_U
                I_WBS_ELEMENT_TABLE        = GT_WBS_ELE
                E_MESSAGE_TABLE            = GT_MESSAGE.
            IF GS_RETURN1-TYPE NE 'E'.
              READ TABLE GT_MESSAGE INTO GS_MESSAGE INDEX 1.
              GS_FINAL-MESSAGE = GS_MESSAGE-MESSAGE_TEXT.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.
              WAIT UP TO 1 SECONDS.
              IF SY-SUBRC IS INITIAL.
                GS_FINAL-MESSAGE = 'Status has been changed sucessfully'.
              ELSE.
                GS_FINAL-MESSAGE = 'Status has not been changed sucessfuly'.
              ENDIF.
            ELSE.
**&--Start of code added by JOOKONTR CHG0164542
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              CLEAR GS_FINAL-ST_NEW.
**&--End of code added by JOOKONTR CHG0164542
              READ TABLE GT_MESSAGE INTO GS_MESSAGE INDEX 1.
              GS_FINAL-MESSAGE = GS_MESSAGE-MESSAGE_TEXT.
              GV_CONT ='X'.
            ENDIF.
            MODIFY GT_FINAL FROM GS_FINAL TRANSPORTING ST_NEW MESSAGE.   "ST_NEW Added by JOOKONTR CHG0164542
            REFRESH:GT_METH_PRJ,GT_WBS_ELE,GT_WBS_ELE_U,GT_MESSAGE.
            CLEAR:GS_PRJ_DEF,GS_PRJ_DEF_U,GS_RETURN1,GS_MESSAGE.
            IF GV_CONT = 'X'.
              CONTINUE.
            ENDIF.
          ELSE.
**&--Start of code added by JOOKONTR CHG0164542
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            CLEAR GS_FINAL-ST_NEW.
**&--End of code added by JOOKONTR CHG0164542
            GS_FINAL-MESSAGE = GS_BAPIRET2-MESSAGE.
          ENDIF.
        ENDIF.
      ELSE.
**&--Start of code added by JOOKONTR CHG0164542
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR GS_FINAL-ST_NEW.
**&--End of code added by JOOKONTR CHG0164542
        READ TABLE GT_RESULT INTO GS_RESULT WITH KEY MESSAGE_TYPE = 'E'.
        IF SY-SUBRC IS INITIAL.
          GS_FINAL-MESSAGE = GS_RESULT-MESSAGE_TEXT.
        ENDIF.
      ENDIF.
**    ENDIF.
      MODIFY GT_FINAL FROM GS_FINAL TRANSPORTING ST_NEW MESSAGE. "ST_NEW Added by JOOKONTR CHG0164542
      CLEAR:GS_FINAL,GS_RESULT,GS_BAPIRET2.
**&--End of code added by JOOKONTR CHG0164542
*&--Start of code commented by JOOKONTR CHG0164542
******      READ TABLE GT_FINAL_TEMP INTO GS_FINAL_TEMP WITH KEY PSPNR = GS_FINAL-PSPNR BINARY SEARCH.
******      IF SY-SUBRC IS INITIAL.
******        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
******          EXPORTING
******            INPUT  = GS_FINAL_TEMP-PSPNR
******          IMPORTING
******            OUTPUT = GS_FINAL_TEMP-PSPNR.
******      ENDIF.
******      READ TABLE GT_PRPS INTO GS_PRPS WITH KEY PSPNR = GS_FINAL_TEMP-PSPNR BINARY SEARCH.
******      IF SY-SUBRC IS INITIAL.
******        READ TABLE LT_PROJ INTO LS_PROJ WITH KEY PSPNR = GS_PRPS-PSPHI BINARY SEARCH.
******        IF SY-SUBRC IS INITIAL.
******          CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
******            EXPORTING
******              INPUT  = LS_PROJ-PSPID
******            IMPORTING
******              OUTPUT = GS_PRJ_DEF-PROJECT_DEFINITION.
******        ENDIF.
******      ENDIF.
******      GS_METH_PRJ-REFNUMBER          = '000001'.
******      GS_METH_PRJ-OBJECTTYPE         = 'WBS-ELEMENT'.       "#EC NOTEXT
******      GS_METH_PRJ-METHOD             = 'Update'.            "#EC NOTEXT
******      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
******        EXPORTING
******          INPUT  = GS_FINAL_TEMP-PSPNR
******        IMPORTING
******          OUTPUT = GS_METH_PRJ-OBJECTKEY.
******      APPEND GS_METH_PRJ TO GT_METH_PRJ.
******      CLEAR GS_METH_PRJ.
******      GS_METH_PRJ-METHOD = 'Save'.                          "#EC NOTEXT
******      APPEND GS_METH_PRJ TO GT_METH_PRJ.
******      CLEAR GS_METH_PRJ.
******      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
******        EXPORTING
******          INPUT  = GS_FINAL_TEMP-PSPNR
******        IMPORTING
******          OUTPUT = GS_WBS_ELE-WBS_ELEMENT.
******      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
******        EXPORTING
******          INPUT  = GS_PROJ-PSPID
******        IMPORTING
******          OUTPUT = GS_WBS_ELE-PROJECT_DEFINITION.
******      GS_WBS_ELE-USER_FIELD_KEY     = 'ZALELEM'.
******      IF GS_FINAL-ST_OLD = 'REL' AND GS_FINAL-ST_NEW = 'TECO'.
******        GS_WBS_ELE-USER_FIELD_DATE1      = P_USR08.
******        GS_WBS_ELE_U-USER_FIELD_DATE1    = 'X'.
******      ENDIF.
******      IF GS_FINAL-ST_NEW = 'CLSD' AND ( GS_FINAL-ST_OLD = 'REL' OR GS_FINAL-ST_OLD = 'TECO' ).
******        IF RT_CLSD NE 'X'.  "Added by JOOKONTR FOR CHG0138619
******          IF GS_PRPS-STUFE = '1'. "Added by JOOKONTR FOR CHG0138619
******            GS_WBS_ELE-USER_FIELD_DATE2      = P_USR09.
******            GS_WBS_ELE_U-USER_FIELD_DATE2    = 'X'.
******          ENDIF."Added by JOOKONTR FOR CHG0138619
******        ENDIF."Added by JOOKONTR FOR CHG0138619
******      ENDIF.
******      APPEND GS_WBS_ELE TO GT_WBS_ELE.
******      GS_WBS_ELE-WBS_ELEMENT           = 'X'.
******      GS_WBS_ELE-PROJECT_DEFINITION    = 'X'.
******      GS_WBS_ELE_U-USER_FIELD_KEY     = 'X'.
******      APPEND GS_WBS_ELE_U TO GT_WBS_ELE_U.
******      CLEAR:GS_WBS_ELE_U,GV_CONT,GS_WBS_ELE.
******      CALL FUNCTION 'BAPI_PROJECT_MAINTAIN'
******        EXPORTING
******          I_PROJECT_DEFINITION       = GS_PRJ_DEF
******          I_PROJECT_DEFINITION_UPD   = GS_PRJ_DEF_U
******        IMPORTING
******          RETURN                     = GS_RETURN1
******        TABLES
******          I_METHOD_PROJECT           = GT_METH_PRJ
******          I_WBS_ELEMENT_TABLE_UPDATE = GT_WBS_ELE_U
******          I_WBS_ELEMENT_TABLE        = GT_WBS_ELE
******          E_MESSAGE_TABLE            = GT_MESSAGE.
******      IF GS_RETURN1-TYPE NE 'E'.
******        READ TABLE GT_MESSAGE INTO GS_MESSAGE INDEX 1.
******        GS_FINAL-MESSAGE = GS_MESSAGE-MESSAGE_TEXT.
******        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
******          EXPORTING
******            WAIT = 'X'.
******
******      ELSE.
******        READ TABLE GT_MESSAGE INTO GS_MESSAGE INDEX 1.
******        GS_FINAL-MESSAGE = GS_MESSAGE-MESSAGE_TEXT.
******        GV_CONT ='X'.
******      ENDIF.
******      MODIFY GT_FINAL FROM GS_FINAL TRANSPORTING MESSAGE.
******      REFRESH:GT_METH_PRJ,GT_WBS_ELE,GT_WBS_ELE_U,GT_MESSAGE.
******      CLEAR:GS_PRJ_DEF,GS_PRJ_DEF_U,GS_RETURN1,GS_MESSAGE.
******      IF GV_CONT = 'X'.
******        CONTINUE.
******      ENDIF.
******      WAIT UP TO 2 SECONDS. " Code added by JOOKONTR
*&--Start of code commented by JOOKONTR CHG0164542
    ELSEIF GV_FLAG_A ='X'.
      CLEAR: LV_PLFAZ,LV_PLSEZ,LV_EPROG.
      READ TABLE GT_FINAL_TEMP INTO GS_FINAL_TEMP WITH KEY PSPNR = GS_FINAL-PSPNR BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
          EXPORTING
            INPUT  = GS_FINAL_TEMP-PSPNR
          IMPORTING
            OUTPUT = GS_FINAL_TEMP-PSPNR.
      ENDIF.
      READ TABLE GT_PRPS INTO GS_PRPS WITH KEY PSPNR = GS_FINAL_TEMP-PSPNR BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        READ TABLE LT_PROJ INTO LS_PROJ WITH KEY PSPNR = GS_PRPS-PSPHI BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
            EXPORTING
              INPUT  = LS_PROJ-PSPID
            IMPORTING
              OUTPUT = GS_PRJ_DEF-PROJECT_DEFINITION.
        ENDIF.
      ENDIF.
      PERFORM BDC_DYNPRO      USING 'SAPLCJWB' '0100'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    '*PROJ-PSPID'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=MDTB'.
      PERFORM BDC_FIELD       USING '*PROJ-PSPID'
                                    GS_PRJ_DEF-PROJECT_DEFINITION.
      PERFORM BDC_FIELD       USING '*PRPS-POSID'
                                    ''.
      PERFORM BDC_DYNPRO      USING 'SAPLCJWB' '0998'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=BU'.
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          DATE_INTERNAL            = P_PLFAZ
        IMPORTING
          DATE_EXTERNAL            = LV_PLFAZ
        EXCEPTIONS
          DATE_INTERNAL_IS_INVALID = 1
          OTHERS                   = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

      PERFORM BDC_FIELD       USING 'PROJ-PLFAZ'
                                    LV_PLFAZ.
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          DATE_INTERNAL            = P_PLSEZ
        IMPORTING
          DATE_EXTERNAL            = LV_PLSEZ
        EXCEPTIONS
          DATE_INTERNAL_IS_INVALID = 1
          OTHERS                   = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      PERFORM BDC_FIELD       USING 'PROJ-PLSEZ'
                                    LV_PLSEZ.

      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          DATE_INTERNAL            = SY-DATUM
        IMPORTING
          DATE_EXTERNAL            = LV_SPROG
        EXCEPTIONS
          DATE_INTERNAL_IS_INVALID = 1
          OTHERS                   = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      PERFORM BDC_FIELD       USING 'PROJ-SPROG'
                              LV_SPROG.

      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          DATE_INTERNAL            = P_EPROG
        IMPORTING
          DATE_EXTERNAL            = LV_EPROG
        EXCEPTIONS
          DATE_INTERNAL_IS_INVALID = 1
          OTHERS                   = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.
      PERFORM BDC_FIELD       USING 'PROJ-EPROG'
                                    LV_EPROG.
      PERFORM BDC_TRANSACTION USING 'CJ02'.
      IF GV_CONT = 'X'.
        SORT GT_MESSTAB BY MSGV1.
        DELETE ADJACENT DUPLICATES FROM GT_MESSTAB COMPARING ALL FIELDS.
        CLEAR:GV_MESSAGE.
        LOOP AT GT_MESSTAB INTO GS_MESSTAB.
          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              ID        = GS_MESSTAB-MSGID
              LANG      = 'EN'
              NO        = GS_MESSTAB-MSGNR
              V1        = GS_MESSTAB-MSGV1
              V2        = GS_MESSTAB-MSGV2
              V3        = GS_MESSTAB-MSGV3
              V4        = GS_MESSTAB-MSGV4
            IMPORTING
              MSG       = GV_MESSAGE
            EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.
          IF SY-SUBRC <> 0.
            GV_MESSAGE = 'Error while updating the record'.
          ENDIF.
        ENDLOOP.
        GS_FINAL-MESSAGE = GV_MESSAGE.
        CLEAR GS_FINAL-ST_NEW. "Added by JOOKONTR for
        MODIFY GT_FINAL FROM GS_FINAL TRANSPORTING ST_NEW MESSAGE. "ST_NEW Added by JOOKONTR CHG0164542
        CONTINUE.
      ENDIF.
*&--Start of code added by JOOKONTR CHG0164542
      CLEAR: GT_WBS[],GT_RESULT[],GT_BAPIRET2[],GS_RETURN.
      CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
      GS_WBS-WBS_ELEMENT = GS_FINAL-PSPNR.
      GS_WBS-SET_SYSTEM_STATUS = GS_FINAL-ST_NEW.
      IF ( GS_FINAL-ST_OLD = LC_REL
        OR GS_FINAL-ST_OLD = LC_CRTD )  " ADDED BY AKMADASU CHG0138619
        AND R_REV IS INITIAL.           "Added by JOOKONTR CHG0164542
        CLEAR GS_WBS-UNDO_SYSTEM_STATUS.
      ELSE.
        GS_WBS-UNDO_SYSTEM_STATUS = GS_FINAL-ST_OLD.
      ENDIF.
      APPEND GS_WBS TO GT_WBS.
      CLEAR: GS_RESULT,GS_WBS.
      CALL FUNCTION 'BAPI_BUS2054_SET_STATUS'
        IMPORTING
          RETURN              = GS_RETURN
        TABLES
          I_WBS_SYSTEM_STATUS = GT_WBS
**       I_WBS_USER_STATUS   =
          E_RESULT            = GT_RESULT.

      IF GT_RESULT[] IS INITIAL.
        CALL FUNCTION 'BAPI_PS_PRECOMMIT'
          TABLES
            ET_RETURN = GT_BAPIRET2.

        IF GT_BAPIRET2[] IS NOT INITIAL.
          READ TABLE GT_BAPIRET2 INTO GS_BAPIRET2 WITH KEY TYPE = 'E'.
          IF SY-SUBRC IS NOT INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.

            IF SY-SUBRC IS INITIAL.
              GS_FINAL-MESSAGE = 'Status has been changed sucessfully'.
            ELSE.
              GS_FINAL-MESSAGE = 'Status has not been changed sucessfuly'.
            ENDIF.
          ELSE.
**&--Start of code added by JOOKONTR CHG0164542
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            CLEAR GS_FINAL-ST_NEW.
**&--End of code added by JOOKONTR CHG0164542
            GS_FINAL-MESSAGE = GS_BAPIRET2-MESSAGE.
          ENDIF.
        ENDIF.
      ELSE.
**&--Start of code added by JOOKONTR CHG0164542
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CLEAR GS_FINAL-ST_NEW.
**&--End of code added by JOOKONTR CHG0164542
        READ TABLE GT_RESULT INTO GS_RESULT WITH KEY MESSAGE_TYPE = 'E'.
        IF SY-SUBRC IS INITIAL.
          GS_FINAL-MESSAGE = GS_RESULT-MESSAGE_TEXT.
        ENDIF.
      ENDIF.
**    ENDIF.
      MODIFY GT_FINAL FROM GS_FINAL TRANSPORTING ST_NEW MESSAGE. "ST_NEW Added by JOOKONTR CHG0164542
      CLEAR:GS_FINAL,GS_RESULT,GS_BAPIRET2.
*&--End of code dded by JOOKONTR CHG0164542

    ELSE.
      " EXECUTE BELOW LOGIC.

    ENDIF.
**-- end of changes by akmadasu CHG0138619
**    IF GV_FLAG_D IS INITIAL.
*&--Start of code commented by JOOKONTR CHG0164542
***    CLEAR: GT_WBS[],GT_RESULT[],GT_BAPIRET2[],GS_RETURN.
***    CALL FUNCTION 'BAPI_PS_INITIALIZATION'.
***    GS_WBS-WBS_ELEMENT = GS_FINAL-PSPNR.
***    GS_WBS-SET_SYSTEM_STATUS = GS_FINAL-ST_NEW.
***    IF ( GS_FINAL-ST_OLD = LC_REL
***      OR GS_FINAL-ST_OLD = LC_CRTD )  " ADDED BY AKMADASU CHG0138619
***      AND R_REV IS INITIAL.           "Added by JOOKONTR CHG0164542
***      CLEAR GS_WBS-UNDO_SYSTEM_STATUS.
***    ELSE.
***      GS_WBS-UNDO_SYSTEM_STATUS = GS_FINAL-ST_OLD.
***    ENDIF.
***    APPEND GS_WBS TO GT_WBS.
***    CLEAR: GS_RESULT,GS_WBS.
***    CALL FUNCTION 'BAPI_BUS2054_SET_STATUS'
***      IMPORTING
***        RETURN              = GS_RETURN
***      TABLES
***        I_WBS_SYSTEM_STATUS = GT_WBS
*****       I_WBS_USER_STATUS   =
***        E_RESULT            = GT_RESULT.
***
***    IF GT_RESULT[] IS INITIAL.
***      CALL FUNCTION 'BAPI_PS_PRECOMMIT'
***        TABLES
***          ET_RETURN = GT_BAPIRET2.
***
***      IF GT_BAPIRET2[] IS NOT INITIAL.
***        READ TABLE GT_BAPIRET2 INTO GS_BAPIRET2 WITH KEY TYPE = 'E'.
***        IF SY-SUBRC IS NOT INITIAL.
***          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
***            EXPORTING
***              WAIT = 'X'.
***
***** IMPORTING
*****   RETURN        =
***          .
***          IF SY-SUBRC IS INITIAL.
***            GS_FINAL-MESSAGE = 'Status has been changed sucessfully'.
***          ELSE.
***            GS_FINAL-MESSAGE = 'Status has not been changed sucessfuly'.
***          ENDIF.
***        ELSE.
***          GS_FINAL-MESSAGE = GS_BAPIRET2-MESSAGE.
***        ENDIF.
***      ENDIF.
***    ELSE.
***      READ TABLE GT_RESULT INTO GS_RESULT WITH KEY MESSAGE_TYPE = 'E'.
***      IF SY-SUBRC IS INITIAL.
***        GS_FINAL-MESSAGE = GS_RESULT-MESSAGE_TEXT.
***      ENDIF.
***    ENDIF.
*****    ENDIF.
***    MODIFY GT_FINAL FROM GS_FINAL TRANSPORTING MESSAGE.
***    CLEAR:GS_FINAL,GS_RESULT,GS_BAPIRET2.
*&--End of code commented by JOOKONTR CHG0164542
  ENDLOOP.
*&--Start of code added by akmadasu
  APPEND LINES OF GT_FINAL_ER TO GT_FINAL.
  CLEAR GT_FINAL_ER.
  SORT GT_FINAL BY PSPNR.  "Added by JOOKONTR
*&--End of code added by akmadasu
ENDFORM.                    " UPDATE_STATUS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = SY-REPID
*     i_callback_top_of_page  = 'TOP-OF-PAGE' "see FORM
*     I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*     is_layout               = gd_layout
      IT_FIELDCAT             = FIELDCATALOG[]
*     i_save                  = 'X'
    TABLES
      T_OUTTAB                = GT_FINAL
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.
  IF SY-SUBRC <> 0.
  ENDIF.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .
  PERFORM FIELD_CAT USING 'PSPNR' 'WBS Element' .
  PERFORM FIELD_CAT USING 'ST_NEW' 'New status' .
  PERFORM FIELD_CAT USING 'ST_OLD' 'Old status' .
  PERFORM FIELD_CAT USING 'MESSAGE' 'Message' .
ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0015   text
*      -->P_0016   text
*----------------------------------------------------------------------*
FORM FIELD_CAT  USING P_FIELD P_LABEL.

  FIELDCATALOG-FIELDNAME   = P_FIELD.
  FIELDCATALOG-SELTEXT_M   = P_LABEL.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.
ENDFORM.                    " FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1059   text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION USING TCODE.
  DATA: LV_MSTRING(480).
  DATA: LV_MODE TYPE C.
  DATA: LV_SUBRC LIKE SY-SUBRC.
  REFRESH GT_MESSTAB.
  IF GT_BDCDATA IS NOT INITIAL.
    LV_MODE = 'N'.
    CALL TRANSACTION 'CJ02' USING GT_BDCDATA
                             MODE LV_MODE" CTUMODE
                           UPDATE 'S'"CUPDATE
                    MESSAGES INTO GT_MESSTAB.
    IF SY-SUBRC IS INITIAL.
      "DO NOTHING
    ELSE.
      GV_CONT ='X'.
    ENDIF.
  ENDIF.
ENDFORM.                    " BDC_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1054   text
*      -->P_P_EPROG  text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING IV_FNAM TYPE ANY
                     IV_FVAL TYPE ANY..


  GS_BDCDATA-FNAM = IV_FNAM.
  GS_BDCDATA-FVAL = IV_FVAL.
  APPEND GS_BDCDATA TO GT_BDCDATA.
  CLEAR GS_BDCDATA.
ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1024   text
*      -->P_1025   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING IV_PROGRAM TYPE ANY
                        IV_DYNPRO TYPE ANY.
  GS_BDCDATA-PROGRAM  = IV_PROGRAM.
  GS_BDCDATA-DYNPRO   = IV_DYNPRO.
  GS_BDCDATA-DYNBEGIN = 'X'.
  APPEND GS_BDCDATA TO GT_BDCDATA.
  CLEAR:GS_BDCDATA.
ENDFORM.                    " BDC_DYNPRO
