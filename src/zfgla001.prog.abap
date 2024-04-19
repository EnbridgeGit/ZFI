REPORT ZFGLA001 MESSAGE-ID ZF.
*----------------------------------------------------------------------*
* Owner: Centra/Union Gas Ltd. - BIS                                   *
* Author: Selwyn Rodricks                                              *
*         OmniLogic Systems Group                                      *
* Date: October 02, 1996                                               *
* Request ID: DRCO0080                                                 *
*                                                                      *
* Brief Description:                                                   *
* This program creates a batch input session with G/L postings in FI   *
* with an assignment to a profitability segment number.                *
*1998/07/09  Janet Reid - renaming bdc session
*----------------------------------------------------------------------*


TABLES: ZFPSA,                         "Profitability Segment Assignmt
        ZF006,                         "Variant data
        BSEG,                          "Accounting document segment
        CE41100,                       "Union/Centra Operations
        CE31100,                       "Union/Centra Operations
        MARA,                          "Material Master: General Data
        SKB1.                          "G/L account master

FIELD-SYMBOLS:  <VALFD>.
DATA:  FULL_VALFD(16) VALUE 'CE31100-     001'.

* batch input data structure
DATA: BEGIN OF BDCDATA  OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END   OF BDCDATA.

DATA: BEGIN OF ITAB  OCCURS 100.       "Internal table for % data
        INCLUDE STRUCTURE ZFPSA.
DATA:   PAOBJNR LIKE CE41100-PAOBJNR.  "Prof.segment#
DATA:   WRBTR   LIKE BSEG-WRBTR.       "Amount to be posted
DATA:   VF_AMT  LIKE BSEG-WRBTR.       "Value field amount
DATA: END   OF ITAB.

DATA: TEMPDATE      LIKE SY-DATUM,     "Date for document date
      LASTDATE      LIKE SY-DATUM,     "Date for posting date
      TEMPWRBTR(13),                   "Amount
      R_BSCHL       LIKE BSEG-BSCHL,   "Posting key -Opposite of P_BSCHL
      FIRST_TIME(1),                   "Indicator for loop passes
      VF_TOTAL      LIKE BSEG-WRBTR,   "Value Field Total Amount
      VF_TOTAL2     LIKE BSEG-WRBTR,   "Value Field Total Amount
      A_TOTAL       LIKE BSEG-WRBTR,   "Total Amount for calc. allocatn
      I_TOTAL       LIKE BSEG-WRBTR,   "Total Amount for checking + or -
      RO_AMT        LIKE BSEG-WRBTR,   "Rounding off difference
      LAST_TABIX    LIKE SY-TABIX,     "Last line item in internal table
      LYYYY(4),                        "Year - Lower limit
      HYYYY(4),                        "Year - Upper limit
      PERCT_TOTAL   LIKE ZFPSA-PERCT.  "Percent total

SELECTION-SCREEN BEGIN OF BLOCK INPUTS WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK RUNTYPE WITH FRAME TITLE TEXT-002.
PARAMETERS: R1 RADIOBUTTON GROUP RAD1,
            R2 RADIOBUTTON GROUP RAD1. "DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK RUNTYPE.

SELECTION-SCREEN BEGIN OF BLOCK VARIANT WITH FRAME TITLE TEXT-003.
PARAMETERS: P_BUPER  LIKE BSEG-ABPER   DEFAULT SY-DATUM,
            P_BUKRS  LIKE BSEG-BUKRS   DEFAULT 'UGL',   "Company code
            P_SAKNR  LIKE BSEG-SAKNR   MATCHCODE OBJECT SAKO,
            P_BSCHL  LIKE BSEG-BSCHL   DEFAULT '50',
            P_SAKNR2 LIKE BSEG-SAKNR   MATCHCODE OBJECT SAKO,
            P_WRBTR  LIKE BSEG-WRBTR,
            P_WAERS  LIKE ZF006-WAERS  DEFAULT 'CAD',
            P_ALLNR  LIKE ZFPSA-ALLNR,
            P_VALFD(5),
            P_ARTNR  LIKE MARA-MATNR MATCHCODE OBJECT MAT1,
            P_YTD                   ,
            P_ALLTY  LIKE ZFPSA-ALLTY,
            P_GROUP  LIKE APQI-GROUPID DEFAULT 'ZPA_PROFSEGM'.
SELECTION-SCREEN END OF BLOCK VARIANT.

SELECTION-SCREEN END OF BLOCK INPUTS.

AT SELECTION-SCREEN.
* Process type check
  IF R1 IS INITIAL AND R2 IS INITIAL.
    MESSAGE E000.
  ENDIF.

* Posting Period check
  IF R1 = 'X'.
    LYYYY = SY-DATUM(4) - 1.
    HYYYY = SY-DATUM(4) + 1.
    IF ( P_BUPER(4) < LYYYY ) OR ( P_BUPER(4) > HYYYY ).
      MESSAGE E001 WITH P_BUPER(4).
    ENDIF.

    IF ( P_BUPER+4(2) < '01' ) OR ( P_BUPER+4(2) > '13' ).
      MESSAGE E002 WITH P_BUPER+4(2).
    ENDIF.
  ENDIF.

* Posting Key check
  IF R1 = 'X'.
    IF P_BSCHL <> '40' AND P_BSCHL <> '50'.
      MESSAGE E003 WITH P_BSCHL.
    ENDIF.
  ENDIF.

* Allocation Type check
  IF R1 = 'X'.
    IF P_ALLTY <> 'P' AND P_ALLTY <> 'H' AND P_ALLTY <> 'S'.
      MESSAGE E005 WITH P_ALLTY.
    ENDIF.
  ENDIF.


*----------------------------------------------------------------------*
*     Start of Main Processing Block
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF P_WAERS IS INITIAL.
    MOVE 'CAD' TO P_WAERS.
  ENDIF.

  IF R2 = 'X'.
    SELECT * FROM ZF006.
      WRITE: / 'ZF006 Key=', ZF006-RCKEY, 'Type=', ZF006-ALLTY,
               'Amount=',ZF006-WRBTR.
    WRITE: / '--------------------------------------------------------'.
      MOVE:  ZF006-BUPER TO P_BUPER,
             ZF006-BUKRS TO P_BUKRS,
             ZF006-SAKNR TO P_SAKNR,
             ZF006-BSCHL TO P_BSCHL,
             ZF006-SAKN2 TO P_SAKNR2,
             ZF006-WRBTR TO P_WRBTR,
             ZF006-WAERS TO P_WAERS,
             ZF006-ALLNR TO P_ALLNR,
             ZF006-VALFD TO P_VALFD,
             ZF006-ARTNR TO P_ARTNR,
             ZF006-YTDFG TO P_YTD,
             ZF006-ALLTY TO P_ALLTY,
             ZF006-GROUPID TO P_GROUP.
      IF P_WAERS IS INITIAL.
        MOVE 'CAD' TO P_WAERS.
      ENDIF.
      PERFORM MAIN.
    ENDSELECT.
  ELSE.
    PERFORM MAIN.
  ENDIF.


*---------------------------------------------------------------------*
*       FORM MAIN                   Creates one document
*---------------------------------------------------------------------*
FORM MAIN.
* First, get the field status group for the account
  SELECT SINGLE * FROM SKB1 WHERE BUKRS = P_BUKRS AND
                                  SAKNR = P_SAKNR.

  IF P_BSCHL = '50'.
    R_BSCHL = '40'.
  ELSE.
    R_BSCHL = '50'.
  ENDIF.
  VF_TOTAL  = 0.
  VF_TOTAL2 = 0.

  CASE P_ALLTY.
*** PERCENT method
    WHEN 'P'.
      CLEAR ITAB.
      REFRESH ITAB.
      SELECT * FROM ZFPSA INTO TABLE ITAB WHERE
                                BUKRS = P_BUKRS AND
                                ALLTY = P_ALLTY AND
                                ALLNR = P_ALLNR.

      IF SY-SUBRC <> 0.
        WRITE: / TEXT-301.
        NEW-LINE.
        NEW-LINE.
        EXIT.
      ENDIF.

      CLEAR PERCT_TOTAL.
      LOOP AT ITAB.
        PERCT_TOTAL = PERCT_TOTAL + ITAB-PERCT.
      ENDLOOP.

      IF PERCT_TOTAL <> 100.
        WRITE: / TEXT-302, PERCT_TOTAL.
        WRITE: / TEXT-303.
        EXIT.
      ENDIF.

      LOOP AT ITAB.
        ITAB-WRBTR = P_WRBTR * ITAB-PERCT / 100.
        ITAB-VF_AMT = 0.          "This is to initialize $20202020202020
        MODIFY ITAB.
      ENDLOOP.

      PERFORM ADJUST_DIFFERENCE.

*** HIGH LEVEL Allocation
    WHEN 'H'.
      CLEAR ITAB.
      REFRESH ITAB.
      SELECT * FROM ZFPSA WHERE
                          BUKRS = P_BUKRS AND
                          ALLTY = P_ALLTY AND
                          ALLNR = P_ALLNR.

        SELECT SINGLE * FROM MARA WHERE
                                  MATNR = P_ARTNR.

        SELECT * FROM CE41100 WHERE
                              PRDHA = MARA-PRDHA.
          IF NOT ZFPSA-KUNDE_PA IS INITIAL.
            CHECK  CE41100-KNDNR = ZFPSA-KUNDE_PA.
          ENDIF.
          IF NOT ZFPSA-BUKRS    IS INITIAL.
            CHECK CE41100-BUKRS = ZFPSA-BUKRS.
          ENDIF.
          IF NOT ZFPSA-VKORG    IS INITIAL.
            CHECK CE41100-VKORG = ZFPSA-VKORG.
          ENDIF.
          IF NOT ZFPSA-VTWEG    IS INITIAL.
            CHECK CE41100-VTWEG = ZFPSA-VTWEG.
          ENDIF.
          IF NOT ZFPSA-SPART    IS INITIAL.
            CHECK CE41100-SPART = ZFPSA-SPART.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWDVN IS INITIAL.
            CHECK CE41100-WWDVN = ZFPSA-RKEG_WWDVN.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWSCT IS INITIAL.
            CHECK CE41100-WWSCT = ZFPSA-RKEG_WWSCT.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWSEG IS INITIAL.
            CHECK CE41100-WWSEG = ZFPSA-RKEG_WWSEG.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWRAT IS INITIAL.
            CHECK CE41100-WWRAT = ZFPSA-RKEG_WWRAT.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWREG IS INITIAL.
            CHECK CE41100-WWREG = ZFPSA-RKEG_WWREG.
          ENDIF.


          FULL_VALFD+8(5) = P_VALFD.

          SELECT * FROM CE31100 WHERE
                                PAOBJNR = CE41100-PAOBJNR AND
                                PLIKZ   = '0'.                "Actual
            TRANSLATE P_YTD TO UPPER CASE.
            IF P_YTD =  'Y' OR P_YTD = 'X'.
              CHECK CE31100-PERBL(4) = P_BUPER(4).
            ELSE.
              CHECK CE31100-PERBL(4) = P_BUPER(4).
              CHECK CE31100-PERBL+5(2) = P_BUPER+4(2).
            ENDIF.

            MOVE-CORRESPONDING ZFPSA TO ITAB.
*           Prof.Segment# should be 0.
*           MOVE CE41100-PAOBJNR TO ITAB-PAOBJNR.
            ASSIGN (FULL_VALFD) TO <VALFD>.
            MOVE: <VALFD> TO ITAB-VF_AMT.
            IF ITAB-VF_AMT <> 0.
*             APPEND ITAB.
              COLLECT ITAB.
            ENDIF.
          ENDSELECT.
        ENDSELECT.
      ENDSELECT.

      IF SY-SUBRC <> 0.
        WRITE: / TEXT-301.
        NEW-LINE.
        NEW-LINE.
        EXIT.
      ENDIF.

      LOOP AT ITAB.
        AT FIRST.
          SUM.
          VF_TOTAL  = ITAB-VF_AMT.
          VF_TOTAL2 = ITAB-VF_AMT.
          EXIT.
        ENDAT.
      ENDLOOP.

      IF VF_TOTAL < 0.
        VF_TOTAL = - VF_TOTAL.
      ENDIF.

      LOOP AT ITAB.
        ITAB-WRBTR = ITAB-VF_AMT * P_WRBTR / VF_TOTAL.
        IF ITAB-WRBTR <> 0.
          MODIFY ITAB.
        ELSE.
          DELETE ITAB.
        ENDIF.
      ENDLOOP.

      PERFORM ADJUST_DIFFERENCE.

*** SEGMENT Allocation
    WHEN 'S'.
      CLEAR ITAB.
      REFRESH ITAB.
      SELECT * FROM ZFPSA WHERE
                          BUKRS = P_BUKRS AND
                          ALLTY = P_ALLTY AND
                          ALLNR = P_ALLNR.

        SELECT SINGLE * FROM MARA WHERE
                                  MATNR = P_ARTNR.

        SELECT * FROM CE41100 WHERE
                              PRDHA = MARA-PRDHA.
          IF NOT ZFPSA-KUNDE_PA IS INITIAL.
            CHECK  CE41100-KNDNR = ZFPSA-KUNDE_PA.
          ENDIF.
          IF NOT ZFPSA-BUKRS    IS INITIAL.
            CHECK CE41100-BUKRS = ZFPSA-BUKRS.
          ENDIF.
          IF NOT ZFPSA-VKORG    IS INITIAL.
            CHECK CE41100-VKORG = ZFPSA-VKORG.
          ENDIF.
          IF NOT ZFPSA-VTWEG    IS INITIAL.
            CHECK CE41100-VTWEG = ZFPSA-VTWEG.
          ENDIF.
          IF NOT ZFPSA-SPART    IS INITIAL.
            CHECK CE41100-SPART = ZFPSA-SPART.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWDVN IS INITIAL.
            CHECK CE41100-WWDVN = ZFPSA-RKEG_WWDVN.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWSCT IS INITIAL.
            CHECK CE41100-WWSCT = ZFPSA-RKEG_WWSCT.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWSEG IS INITIAL.
            CHECK CE41100-WWSEG = ZFPSA-RKEG_WWSEG.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWRAT IS INITIAL.
            CHECK CE41100-WWRAT = ZFPSA-RKEG_WWRAT.
          ENDIF.
          IF NOT ZFPSA-RKEG_WWREG IS INITIAL.
            CHECK CE41100-WWREG = ZFPSA-RKEG_WWREG.
          ENDIF.

          FULL_VALFD+8(5) = P_VALFD.

          SELECT * FROM CE31100 WHERE
                                PAOBJNR = CE41100-PAOBJNR AND
                                PLIKZ   = '0'.                "Actual
            TRANSLATE P_YTD TO UPPER CASE.
            IF P_YTD =  'Y' OR P_YTD = 'X'.
              CHECK CE31100-PERBL(4) = P_BUPER(4).
            ELSE.
              CHECK CE31100-PERBL(4) = P_BUPER(4).
              CHECK CE31100-PERBL+5(2) = P_BUPER+4(2).
            ENDIF.

            MOVE-CORRESPONDING ZFPSA TO ITAB.
            MOVE CE41100-PAOBJNR TO ITAB-PAOBJNR.
            ASSIGN (FULL_VALFD) TO <VALFD>.
            MOVE: <VALFD> TO ITAB-VF_AMT.
            IF ITAB-VF_AMT <> 0.
              APPEND ITAB.
            ENDIF.
          ENDSELECT.
        ENDSELECT.
      ENDSELECT.

      IF SY-SUBRC <> 0.
        WRITE: / TEXT-301.
        EXIT.
      ENDIF.

      LOOP AT ITAB.
        AT FIRST.
          SUM.
          VF_TOTAL  = ITAB-VF_AMT.
          VF_TOTAL2 = ITAB-VF_AMT.
          EXIT.
        ENDAT.
      ENDLOOP.

      IF VF_TOTAL < 0.
        VF_TOTAL = - VF_TOTAL.
      ENDIF.

      LOOP AT ITAB.
        ITAB-WRBTR = ITAB-VF_AMT * P_WRBTR / VF_TOTAL.
        IF ITAB-WRBTR <> 0.
          MODIFY ITAB.
        ELSE.
          DELETE ITAB.
        ENDIF.
      ENDLOOP.

      PERFORM ADJUST_DIFFERENCE.

    WHEN OTHERS.
      WRITE: / TEXT-306.
  ENDCASE.

*----------------------------------------------------------------------

  LOOP AT ITAB.
    EXIT.
  ENDLOOP.

  IF SY-TFILL > 300.
    WRITE: / TEXT-304.
    WRITE: / TEXT-305.
    SKIP.SKIP.
    EXIT.
  ENDIF.

  IF SY-TFILL > 0.
    PERFORM LAST_DAY_OF_MONTH.         "Determine posting date
    PERFORM OPEN_SESSION.              "Open BDC session
    TRANSLATE P_ALLTY TO UPPER CASE.

    CASE P_ALLTY.
      WHEN 'P'.
        PERFORM CREATE_DATA_P.         "Create & Submit BDC to session
      WHEN 'H'.
        PERFORM CREATE_DATA_H.         "Create & Submit BDC to session
      WHEN 'S'.
        PERFORM CREATE_DATA_S.         "Create & Submit BDC to session
      WHEN OTHERS.
    ENDCASE.

    PERFORM CLOSE_SESSION.             "Close BDC session
  ELSE.
    WRITE: / 'No matching data.  Session was not created'.
  ENDIF.
ENDFORM.

*-----------------------------------------------------------------------
*     FORM CREATE_DATA_P.     (BDC session for PERCENTAGE)
*-----------------------------------------------------------------------
FORM CREATE_DATA_P.
  REFRESH BDCDATA.
  CLEAR BDCDATA.

* 1st screen
  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  WRITE SY-DATUM TO TEMPDATE.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT'   TEMPDATE.          "Doc.Dt
  WRITE LASTDATE TO TEMPDATE.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT'   TEMPDATE.          "Post.Dt
  PERFORM BDC_FIELD  USING 'BKPF-BLART'   'S3'.              "Doc.Type
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS'   P_BUKRS.           "Co.Code
  PERFORM BDC_FIELD  USING 'BKPF-MONAT'   P_BUPER+4(2).      "Period
  PERFORM BDC_FIELD  USING 'BKPF-WAERS'   P_WAERS.           "Currency
  CLEAR FIRST_TIME.
* For each item in the internal table, a set of entries are made below:
  LOOP AT ITAB.
*   Line items
    IF FIRST_TIME = 'N'.
      PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
    ENDIF.

    IF ITAB-WRBTR >= 0.
      IF VF_TOTAL2 >= 0.
        PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  P_BSCHL.     "Post.Key
      ELSE.
        PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  R_BSCHL.     "Post.Key
      ENDIF.
    ELSE.
      IF VF_TOTAL2 >= 0.
        PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  R_BSCHL.     "Post.Key
      ELSE.
        PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  P_BSCHL.     "Post.Key
      ENDIF.
    ENDIF.

    PERFORM BDC_FIELD  USING 'RF05A-NEWKO'  P_SAKNR.        "Account

    IF FIRST_TIME = 'N'.
      PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.          "Continue
    ENDIF.

*     2nd screen
    PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
    WRITE ITAB-WRBTR TO TEMPWRBTR.
    IF ITAB-WRBTR < 0.
      TEMPWRBTR = - TEMPWRBTR.
    ENDIF.
    PERFORM BDC_FIELD  USING 'BSEG-WRBTR'   TEMPWRBTR.      "Amt
    PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'   'O0'.             "Tax cd

    PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
    IF NOT ITAB-KOSTL IS INITIAL.
      IF SKB1-FSTAG <> 'RVOT' AND
         SKB1-FSTAG <> 'BSHT' AND
         SKB1-FSTAG <> 'BREV'.
        PERFORM BDC_FIELD  USING 'COBL-KOSTL'   ITAB-KOSTL. "Cost Centr
      ENDIF.
    ENDIF.
    PERFORM BDC_FIELD  USING 'COBL-MATNR'   P_ARTNR.        "Material#
    PERFORM BDC_FIELD  USING 'COBL-PAOBJNR' '0'.            "Just get by
    PERFORM BDC_FIELD  USING 'DKACB-XERGO'  'X'.            "Detail on
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.           "Continue

*   Popup screen
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '0300'.
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(1)' ITAB-KUNDE_PA.  "Customer
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(2)' ITAB-VKORG.     "Sales org
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(6)' ITAB-RKEG_WWSCT."Sector
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(3)' ITAB-VTWEG.     "DistCha
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(4)' ITAB-SPART.     "Divisin
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(5)' ITAB-RKEG_WWDVN."Div/D
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(7)' ITAB-RKEG_WWSEG."Segment
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(8)' ITAB-RKEG_WWRAT."Rate Cl
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.             "Continue

    PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.            "Continue
    FIRST_TIME = 'N'.
  ENDLOOP.
* Offset item
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  IF P_BSCHL = '50'.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '40'.
  ELSE.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '50'.
  ENDIF.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' P_SAKNR2.

  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.            "Continue

  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.

  WRITE P_WRBTR TO TEMPWRBTR.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' TEMPWRBTR.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.            "Continue

  PERFORM INSERT_SESSION.

ENDFORM.


*-----------------------------------------------------------------------
*     FORM CREATE_DATA_H.     (BDC session for HIGH)
*-----------------------------------------------------------------------
FORM CREATE_DATA_H.
  REFRESH BDCDATA.
  CLEAR BDCDATA.

* 1st screen
  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  WRITE SY-DATUM TO TEMPDATE.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT'   TEMPDATE.          "Doc.Dt
  WRITE LASTDATE TO TEMPDATE.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT'   TEMPDATE.          "Post.Dt
  PERFORM BDC_FIELD  USING 'BKPF-BLART'   'S3'.              "Doc.Type
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS'   P_BUKRS.           "Co.Code
  PERFORM BDC_FIELD  USING 'BKPF-MONAT'   P_BUPER+4(2).      "Period
  PERFORM BDC_FIELD  USING 'BKPF-WAERS'   P_WAERS.           "Currency
  CLEAR FIRST_TIME.
* For each item in the internal table, a set of entries are made below:
  LOOP AT ITAB.
*   Line items
    IF FIRST_TIME = 'N'.
      PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
    ENDIF.

    IF ITAB-WRBTR >= 0.
      IF VF_TOTAL2 >= 0.
        PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  P_BSCHL.     "Post.Key
      ELSE.
        PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  R_BSCHL.     "Post.Key
      ENDIF.
    ELSE.
      IF VF_TOTAL2 >= 0.
        PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  R_BSCHL.     "Post.Key
      ELSE.
        PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  P_BSCHL.     "Post.Key
      ENDIF.
    ENDIF.

    PERFORM BDC_FIELD  USING 'RF05A-NEWKO'  P_SAKNR.        "Account

    IF FIRST_TIME = 'N'.
      PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.          "Continue
    ENDIF.

*     2nd screen
    PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
    WRITE ITAB-WRBTR TO TEMPWRBTR.
    IF ITAB-WRBTR < 0.
      TEMPWRBTR = - TEMPWRBTR.
    ENDIF.
    PERFORM BDC_FIELD  USING 'BSEG-WRBTR'   TEMPWRBTR.      "Amt
    PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'   'O0'.             "Tax cd

    PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.

    IF NOT ITAB-KOSTL IS INITIAL.
      IF SKB1-FSTAG <> 'RVOT' AND
         SKB1-FSTAG <> 'BSHT' AND
         SKB1-FSTAG <> 'BREV'.
        PERFORM BDC_FIELD  USING 'COBL-KOSTL'   ITAB-KOSTL."Cost Centr
      ENDIF.
    ENDIF.

    PERFORM BDC_FIELD  USING 'COBL-MATNR'   P_ARTNR.        "Material#
*   PERFORM BDC_FIELD  USING 'COBL-PAOBJNR' ITAB-PAOBJNR.   "Prof.Segmen
    PERFORM BDC_FIELD  USING 'COBL-PAOBJNR' '0'.            "Prof.Segmen
    PERFORM BDC_FIELD  USING 'DKACB-XERGO'  'X'.            "Detail on
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.           "Continue

*   Popup screen
    PERFORM BDC_SCREEN USING 'SAPLKEAK' '0300'.
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(1)' ITAB-KUNDE_PA.  "Customer
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(2)' ITAB-VKORG.     "Sales org
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(6)' ITAB-RKEG_WWSCT."Sector
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(3)' ITAB-VTWEG.     "DistCha
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(4)' ITAB-SPART.     "Divisin
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(5)' ITAB-RKEG_WWDVN."Div/D
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(7)' ITAB-RKEG_WWSEG."Segment
    PERFORM BDC_FIELD  USING 'RKEAK-FIELD(8)' ITAB-RKEG_WWRAT."Rate Cl
    PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.             "Continue

    PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.

    IF NOT ITAB-KOSTL IS INITIAL.
      IF SKB1-FSTAG <> 'RVOT' AND
         SKB1-FSTAG <> 'BSHT' AND
         SKB1-FSTAG <> 'BREV'.
        PERFORM BDC_FIELD  USING 'COBL-KOSTL'   ITAB-KOSTL.  "Cost Centr
      ENDIF.
    ENDIF.

    PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.           "Continue

    FIRST_TIME = 'N'.
  ENDLOOP.
* Offset item
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  IF P_BSCHL = '50'.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '40'.
  ELSE.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '50'.
  ENDIF.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' P_SAKNR2.

  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.            "Continue

  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.

  WRITE P_WRBTR TO TEMPWRBTR.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' TEMPWRBTR.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.            "Continue

  PERFORM INSERT_SESSION.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM CREATE_DATA_S.       (BDC session for SEGMENT)
*-----------------------------------------------------------------------
FORM CREATE_DATA_S.
  REFRESH BDCDATA.
  CLEAR BDCDATA.
* 1st screen
  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  WRITE SY-DATUM TO TEMPDATE.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT'   TEMPDATE.          "Doc.Dt
  WRITE LASTDATE TO TEMPDATE.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT'   TEMPDATE.          "Post.Dt
  PERFORM BDC_FIELD  USING 'BKPF-BLART'   'S3'.              "Doc.Type
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS'   P_BUKRS.           "Co.Code
  PERFORM BDC_FIELD  USING 'BKPF-MONAT'   P_BUPER+4(2).      "Period
  PERFORM BDC_FIELD  USING 'BKPF-WAERS'   P_WAERS.           "Currency
  CLEAR FIRST_TIME.
* For total item in the internal table, a set of entries are made below:
  LOOP AT ITAB.
    AT END OF PAOBJNR.
      SUM.
*     Line items
      IF FIRST_TIME = 'N'.
        PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
      ENDIF.
      IF ITAB-WRBTR >= 0.
        IF VF_TOTAL2 >= 0.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  P_BSCHL.     "Post.Key
        ELSE.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  R_BSCHL.     "Post.Key
        ENDIF.
      ELSE.
        IF VF_TOTAL2 >= 0.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  R_BSCHL.     "Post.Key
        ELSE.
          PERFORM BDC_FIELD  USING 'RF05A-NEWBS'  P_BSCHL.     "Post.Key
        ENDIF.
      ENDIF.

      PERFORM BDC_FIELD  USING 'RF05A-NEWKO'  P_SAKNR.        "Account

      IF FIRST_TIME = 'N'.
        PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
        PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.          "Continue
      ENDIF.

*     2nd screen
      PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
      WRITE ITAB-WRBTR TO TEMPWRBTR.
      IF ITAB-WRBTR < 0.
        TEMPWRBTR = - TEMPWRBTR.
      ENDIF.
      PERFORM BDC_FIELD  USING 'BSEG-WRBTR'   TEMPWRBTR.    "Amt
      PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'   'O0'.             "Tax cd

      PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
      IF NOT ITAB-KOSTL IS INITIAL.
        IF SKB1-FSTAG <> 'RVOT' AND
           SKB1-FSTAG <> 'BSHT' AND
           SKB1-FSTAG <> 'BREV'.
          PERFORM BDC_FIELD  USING 'COBL-KOSTL'   ITAB-KOSTL."Cost Centr
        ENDIF.
      ENDIF.
      PERFORM BDC_FIELD  USING 'COBL-MATNR'   P_ARTNR.        "Material#
      PERFORM BDC_FIELD  USING 'COBL-PAOBJNR' ITAB-PAOBJNR. "Prof.segmen
      PERFORM BDC_FIELD  USING 'DKACB-XERGO'  'X'.            "Detail on
      PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.           "Continue

*     Popup screen
      PERFORM BDC_SCREEN USING 'SAPLKEAK' '0300'.
      PERFORM BDC_FIELD  USING 'RKEAK-FIELD(2)' ITAB-VKORG.  "Sales org.
      PERFORM BDC_FIELD  USING 'RKEAK-FIELD(6)' ITAB-RKEG_WWSCT."Sector
      PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.            "Continue

      PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
      PERFORM BDC_FIELD  USING 'COBL-PAOBJNR' ITAB-PAOBJNR. "Prof.segmen
      PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.            "Continue
      FIRST_TIME = 'N'.
    ENDAT.
  ENDLOOP.

* Offset item
  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
  IF P_BSCHL = '50'.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '40'.
  ELSE.
    PERFORM BDC_FIELD  USING 'RF05A-NEWBS' '50'.
  ENDIF.
  PERFORM BDC_FIELD  USING 'RF05A-NEWKO' P_SAKNR2.

  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.            "Continue

  PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.

  WRITE P_WRBTR TO TEMPWRBTR.
  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' TEMPWRBTR.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.

  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/8'.            "Continue

  PERFORM INSERT_SESSION.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM OPEN_SESSION
*-----------------------------------------------------------------------
* - This routine opens up a new batch input session.
*-----------------------------------------------------------------------
FORM OPEN_SESSION.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = P_GROUP
*           HOLDDATE          =
            KEEP              = 'X'
            USER              = SY-UNAME
       EXCEPTIONS
            GROUP_INVALID     = 1
            GROUP_IS_LOCKED   = 2
            HOLDDATE_INVALID  = 3
            INTERNAL_ERROR    = 4
            QUEUE_ERRORID     = 5
            RUNNING           = 6
            SYSTEM_LOCK_ERROR = 7
            USER_INVALIDD     = 8.

  IF SY-SUBRC <> 0.
    MESSAGE E004.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM CLOSE_SESSION
*-----------------------------------------------------------------------
* - This routine closes a batch input session.
*-----------------------------------------------------------------------
FORM CLOSE_SESSION.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.
  IF SY-SUBRC <> 0.
    WRITE: / 'BDC Close Group Error. rc=', SY-SUBRC.
    EXIT.
  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
*   FORM INSERT_SESSION
*-----------------------------------------------------------------------
* - This routine inserts the BDC data for one transaction into the
*   batch input session.
*-----------------------------------------------------------------------
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'F-02'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    WRITE: / 'Error inserting data into session'.
  ELSE.
    WRITE: / 'Batch input session submitted'.
  ENDIF.
  ULINE.
  SKIP. SKIP.
ENDFORM.


*-----------------------------------------------------------------------
*   FORM BDC_SCREEN
*-----------------------------------------------------------------------
*   Description:
*   - This routine adds an entry to the table BDCDATA with screen
*     information from a particular transaction.  This is used as part
*     of the process for creating data for batch input.
*
*   Parameters:
*      -->  PROGRAM - Program name of the screen
*           DNYPRO  - Screen number
*-----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.


*-----------------------------------------------------------------------
*  FORM BDC_FIELD
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds an entry to the table BDCDATA with field
*    information from a particular transaction.  This is used as part
*    of the process for creating data for batch input.
*
*  Parameters:
*      -->  FNAM - name of the field on the screen
*           FVAL - value to be entered for that field on the
*                  screen
*-----------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.


*-----------------------------------------------------------------------
*  FORM ADJUST_DIFFERENCE.
*-----------------------------------------------------------------------
*  Description:
*  - This routine adds difference between total of calculated allocation
*    amount and user entered allocation amount to the last line item
*-----------------------------------------------------------------------
FORM ADJUST_DIFFERENCE.
  SORT ITAB BY PAOBJNR.
  CLEAR: A_TOTAL, I_TOTAL, RO_AMT, LAST_TABIX.
  LOOP AT ITAB.
    LAST_TABIX = SY-TFILL.
    AT FIRST.
      SUM.
      A_TOTAL     = ITAB-WRBTR.
      I_TOTAL     = ITAB-WRBTR.
      EXIT.
    ENDAT.
  ENDLOOP.

  IF A_TOTAL < 0.
    A_TOTAL = - A_TOTAL.
  ENDIF.
  RO_AMT = P_WRBTR - A_TOTAL.          "Rounding off difference
  IF RO_AMT <> 0.
    READ TABLE ITAB INDEX LAST_TABIX.
    IF SY-SUBRC = 0.
      IF VF_TOTAL2 > 0.
        ITAB-WRBTR = ITAB-WRBTR + RO_AMT.
      ELSE.
        ITAB-WRBTR = ITAB-WRBTR - RO_AMT.
      ENDIF.
      MODIFY ITAB INDEX SY-TABIX.
    ENDIF.
  ENDIF.

  LOOP AT ITAB.
    WRITE: / ITAB-KOSTL,
             ITAB-VKORG, ITAB-VTWEG, ITAB-SPART,
             ITAB-RKEG_WWDVN, ITAB-RKEG_WWSCT, ITAB-RKEG_WWSEG,
             ITAB-RKEG_WWRAT, ITAB-RKEG_WWREG,
             ITAB-PAOBJNR, ITAB-WRBTR.
    IF P_ALLTY <> 'P'.
      WRITE: ITAB-VF_AMT.
    ELSE.
      WRITE: ITAB-PERCT, '%'.
    ENDIF.
    IF ITAB-KOSTL IS INITIAL AND
       SKB1-FSTAG <> 'RVOT'  AND
       SKB1-FSTAG <> 'BSHT'  AND
       SKB1-FSTAG <> 'BREV'.
      WRITE: 'Error. Cost Center Missing'.
    ENDIF.
    IF NOT ITAB-KOSTL IS INITIAL AND
       SKB1-FSTAG = 'RVOT'  AND
       SKB1-FSTAG = 'BSHT'  AND
       SKB1-FSTAG = 'BREV'.
      WRITE: 'Warning. Cost Center Ignored'.
    ENDIF.

  ENDLOOP.
  WRITE: / '--------------------------------------------------------'.
  WRITE: / SY-TFILL, 'Matching Entries'.

ENDFORM.

*-----------------------------------------------------------------------
*  FORM LAST_DAY_OF_MONTH
*-----------------------------------------------------------------------
*  Description:
*  - Posting date determination routine - it is the last day of month
*-----------------------------------------------------------------------
FORM LAST_DAY_OF_MONTH.
  WRITE P_BUPER TO LASTDATE.
  LASTDATE+4(2) = LASTDATE+4(2) + 1.
  IF LASTDATE+4(2) = '13' OR LASTDATE+4(2) = '14'.
    LASTDATE+4(2) = '01'.
    LASTDATE(4) = LASTDATE(4) + 1.
  ENDIF.
  LASTDATE+6(2) = '01'.
  LASTDATE = LASTDATE - 1.
ENDFORM.
