REPORT ZFGLR019 NO STANDARD PAGE HEADING LINE-SIZE 160
                LINE-COUNT 65 MESSAGE-ID PP.

************************************************************************
*
*   PROGRAM:    ZFGLR019
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas
*   DATE:       May 1998.
*
*   The purpose of this program is to produce a report listing G/L
*   account line item activity.  Report is produced in account and line
*   item text sequence with totals at the account level and at the
*   end of the report.  Selection is at company code, G/L account,
*   posting date AND document number or allocation number or
*   document date or document type or posting key.
*
************************************************************************

TABLES:   T001,                         " Company codes
          BSIS,                         " Secondary index for G/L accts
          SKAT.                         " G/L accounts mstr, description

DATA:     BEGIN OF REPTAB OCCURS 1000,                  " report data
            HKONT       LIKE BSIS-HKONT,                " G/L account
            BELNR       LIKE BSIS-BELNR,                " document nbr
            BUDAT       LIKE BSIS-BUDAT,                " posting date
            BLDAT       LIKE BSIS-BLDAT,                " document date
            BLART       LIKE BSIS-BLART,                " document type
            BSCHL       LIKE BSIS-BSCHL,                " posting key
            XBLNR       LIKE BSIS-XBLNR,                " reference doc
            VBUND       LIKE BSIS-VBUND,                " trading partnr
            WAERS       LIKE BSIS-WAERS,                " currency key
            WRBTR       LIKE COSS-WKG001,               " amount
            SGTXT       LIKE BSIS-SGTXT,                " line item text
          END OF REPTAB.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(35) TEXT-002.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
    PARAMETERS P_BUKRS       LIKE BSIS-BUKRS OBLIGATORY DEFAULT 'UGL'.
    SELECT-OPTIONS S_HKONT   FOR BSIS-HKONT.
    SELECT-OPTIONS S_BUDAT   FOR BSIS-BUDAT.
  SELECTION-SCREEN END OF BLOCK BOX2.

  SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-017.
    SELECT-OPTIONS S_XBLNR   FOR BSIS-XBLNR.
    SELECT-OPTIONS S_ZUONR   FOR BSIS-ZUONR.
    SELECT-OPTIONS S_BLDAT   FOR BSIS-BLDAT.
    SELECT-OPTIONS S_BLART   FOR BSIS-BLART.
    SELECT-OPTIONS S_BSCHL   FOR BSIS-BSCHL.
  SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* clear tables
REFRESH REPTAB.
CLEAR REPTAB.

* get company name
SELECT SINGLE * FROM T001
WHERE  BUKRS = P_BUKRS.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* extract required data
START-OF-SELECTION.

  SELECT * FROM BSIS
  WHERE BUKRS EQ P_BUKRS
  AND   HKONT IN S_HKONT
  AND   BUDAT IN S_BUDAT.

    IF ( S_XBLNR-LOW  = SPACE
    AND  S_ZUONR-LOW  = SPACE
    AND  S_BLDAT-LOW  = '00000000'
    AND  S_BLART-LOW  = SPACE
    AND  S_BSCHL-LOW  = SPACE )
    OR ( S_XBLNR-LOW <> SPACE      AND BSIS-XBLNR IN S_XBLNR )
    OR ( S_ZUONR-LOW <> SPACE      AND BSIS-ZUONR IN S_ZUONR )
    OR ( S_BLDAT-LOW <> '00000000' AND BSIS-BLDAT IN S_BLDAT )
    OR ( S_BLART-LOW <> SPACE      AND BSIS-BLART IN S_BLART )
    OR ( S_BSCHL-LOW <> SPACE      AND BSIS-BSCHL IN S_BSCHL ).

      REPTAB-HKONT   = BSIS-HKONT.
      REPTAB-BELNR   = BSIS-BELNR.
      REPTAB-BUDAT   = BSIS-BUDAT.
      REPTAB-BLDAT   = BSIS-BLDAT.
      REPTAB-BLART   = BSIS-BLART.
      REPTAB-BSCHL   = BSIS-BSCHL.
      REPTAB-XBLNR   = BSIS-XBLNR.
      REPTAB-VBUND   = BSIS-VBUND.
      REPTAB-WAERS   = BSIS-WAERS.
      REPTAB-WRBTR   = BSIS-WRBTR.
      IF BSIS-SHKZG  = 'H'.
        REPTAB-WRBTR = REPTAB-WRBTR * -1.
      ENDIF.
      REPTAB-SGTXT   = BSIS-SGTXT.
      APPEND REPTAB.
      CLEAR REPTAB.

    ENDIF.
  ENDSELECT.

* sort the report table by gl account, line item text and document nbr
  SORT REPTAB BY HKONT SGTXT BELNR.

* output the report
  LOOP AT REPTAB.

    AT NEW HKONT.
      CLEAR SKAT.
      SELECT SINGLE * FROM SKAT
      WHERE  SPRAS EQ 'E'
      AND    KTOPL EQ 'COAT'
      AND    SAKNR EQ REPTAB-HKONT.
      NEW-PAGE.
    ENDAT.

    PERFORM WRITE_DETAIL.

    AT END OF HKONT.
      SUM.
      PERFORM WRITE_ACCOUNT_TOTAL.
    ENDAT.

    AT LAST.
      SUM.
      PERFORM WRITE_REPORT_TOTAL.
    ENDAT.

  ENDLOOP.

END-OF-SELECTION.

************************************************************************
*  This section outputs the report headings
************************************************************************
FORM WRITE_HEADER.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 SY-DATUM
            , 071 T001-BUTXT
            , 149 TEXT-001, SY-PAGNO
            , 160 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 069 TEXT-002
            ,     SY-REPID UNDER TEXT-001
            , 160 SY-VLINE.
     WRITE:   /01 SY-VLINE
            , 003 TEXT-003
            ,     REPTAB-HKONT
            ,     SKAT-TXT50
            , 160 SY-VLINE.
     ULINE.
     WRITE:   /03 TEXT-004
            , 016 TEXT-005
            , 029 TEXT-006
            , 042 TEXT-007
            , 047 TEXT-008
            , 052 TEXT-009
            , 071 TEXT-010
            , 081 TEXT-011
            , 086 TEXT-012
            , 109 TEXT-013.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

************************************************************************
*  This section outputs the detail lines to the report
************************************************************************
FORM WRITE_DETAIL.
     WRITE: /     REPTAB-BELNR     UNDER TEXT-004
            ,     REPTAB-BUDAT     UNDER TEXT-005
            ,     REPTAB-BLDAT     UNDER TEXT-006
            ,     REPTAB-BLART     UNDER TEXT-007
            ,     REPTAB-BSCHL     UNDER TEXT-008
            ,     REPTAB-XBLNR     UNDER TEXT-009
            ,     REPTAB-VBUND     UNDER TEXT-010
            ,     REPTAB-WAERS     UNDER TEXT-011
            ,     REPTAB-WRBTR     UNDER TEXT-012
            ,     REPTAB-SGTXT     UNDER TEXT-013.
     PERFORM SHOWVLINE.
ENDFORM.

************************************************************************
*  These sections output the total lines to the report.
************************************************************************
FORM WRITE_ACCOUNT_TOTAL.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /   TEXT-015         UNDER TEXT-004
            ,     REPTAB-HKONT
            ,     REPTAB-WRBTR     UNDER TEXT-012.
     PERFORM SHOWVLINE_TOT.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_REPORT_TOTAL.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /   TEXT-016         UNDER TEXT-004
            ,     REPTAB-WRBTR     UNDER TEXT-012.
     PERFORM SHOWVLINE_TOT.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

************************************************************************
*  These sections write vertical lines between the columns of the report
************************************************************************
FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 014 SY-VLINE
            , 027 SY-VLINE
            , 040 SY-VLINE
            , 045 SY-VLINE
            , 050 SY-VLINE
            , 069 SY-VLINE
            , 079 SY-VLINE
            , 087 SY-VLINE
            , 107 SY-VLINE
            , 160 SY-VLINE.
ENDFORM.

FORM SHOWVLINE_TOT.
     WRITE:   001 SY-VLINE
            , 087 SY-VLINE
            , 107 SY-VLINE
            , 160 SY-VLINE.
ENDFORM.

************************************************************************
*  This is the end, my freind
************************************************************************
