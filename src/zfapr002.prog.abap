REPORT ZFAPR002 NO STANDARD PAGE HEADING LINE-SIZE 80.
******************************************************************
*      Owner: Centra/Union Gas Ltd. - BIS                        *
* Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)    *
*       Date: August 23, 1996                                    *
* Request ID: DRAP0134                                           *
*                                                                *
* The following program will generate a control-break report     *
* based on the document type 'ZE' and 'ZP' for the Incorrect     *
* GST Calculation Report.  Totals will be generated for both     *
* document types.                                                *
******************************************************************
TABLES: BSIS,   "Accounting: Secondary Index for G/L Accounts
        BSEG,   "Accounting Document Segment
        SKB1,   "G/L Account
        SKAT,   "G/L Account Master Record Description
        T001A.  "Company Codes

DATA:  BEGIN OF WTAB OCCURS 500,
        TXT20  LIKE SKAT-TXT20,        "Description of the Account
        HKONT  LIKE BSIS-HKONT,        "Account Number
        BLART  LIKE BSIS-BLART,        "Document Type
        DMBTR  LIKE BSIS-DMBTR,        "Dollar Amount
        BELNR  LIKE BSEG-BELNR,        "Document Ref. No.
       END OF WTAB,
       LN_CNTR TYPE I.                 "Line Counter for page break

PARAMETERS: P_ACCNT LIKE SKB1-SAKNR DEFAULT '256950'
                    MODIF ID ABC,
            P_COMPNY LIKE T001A-BUKRS DEFAULT 'UGL'
                    MODIF ID ABC,
            P_YEAR LIKE BSIS-GJAHR DEFAULT SY-DATUM(4)
                    MODIF ID ABC.

* The following will highlight the screen's output for certain texts. *
AT SELECTION-SCREEN OUTPUT.
 LOOP AT SCREEN.
  CHECK SCREEN-GROUP1 = 'ABC'.
  SCREEN-INTENSIFIED = '1'.
  MODIFY SCREEN.
 ENDLOOP.

************************************************************************
START-OF-SELECTION.
 PERFORM INITIALIZE.
 PERFORM GETINFO.
 PERFORM WRT_HDG.
 PERFORM PROCESS.
END-OF-SELECTION.
************************************************************************

* This routine clears and sets values to certain variables. *
FORM INITIALIZE.
 IF P_ACCNT <> '0000256950'.
    STOP.
 ENDIF.
 REFRESH WTAB.
 MOVE '60' TO LN_CNTR.
ENDFORM.

* This routine will get all the pertaining information desired. *
FORM GETINFO.
 SELECT * FROM BSIS WHERE HKONT = P_ACCNT
          AND BUKRS = P_COMPNY AND GJAHR = P_YEAR
          AND ( BLART = 'ZP' OR BLART = 'ZE' )
          ORDER BY BLART.
     SELECT SINGLE * FROM BSEG WHERE BUKRS = BSIS-BUKRS
              AND BELNR = BSIS-BELNR AND BUZEI = BSIS-BUZEI
              AND GJAHR = BSIS-GJAHR.
         SELECT * FROM SKAT WHERE  KTOPL = 'COAT'
                  AND  SPRAS = 'E' AND SAKNR = BSEG-HKONT.
             PERFORM BUILDTAB.
         ENDSELECT.
 ENDSELECT.
 IF SY-SUBRC <> 0.
    STOP.
 ENDIF.
ENDFORM.

* This routine will write the headings. *
FORM WRT_HDG.
 FORMAT INTENSIFIED OFF.
 NEW-PAGE NO-TITLE.
 WRITE: /1 SY-DATUM, 28 TEXT-001, 75 SY-PAGNO.
 ULINE: /1(80).
 SKIP.
 WRITE: /1 TEXT-009, P_YEAR, 24 TEXT-008, 63 TEXT-010, P_COMPNY.
 ULINE: /1(80).
 WRITE: /1 TEXT-002, 15 TEXT-003, 35 TEXT-004, 59 TEXT-005, 70 TEXT-006.
 ULINE: /1(80).
 MOVE '8' TO LN_CNTR.
 FORMAT INTENSIFIED ON.
ENDFORM.

* This routine will process the internal table for specifed outputs. *
FORM PROCESS.
 SORT WTAB BY BLART.
 LOOP AT WTAB.
   PERFORM WRT_DET.
   AT END OF BLART.
      ULINE: /50(15).
      SKIP.
      SUM.
      FORMAT COLOR COL_TOTAL ON.
      WRITE: /1 WTAB-HKONT, 15 WTAB-TXT20, 38 WTAB-BLART.
      WRITE: 49 '$', 50 WTAB-DMBTR.
      WRITE: 70 TEXT-007.
      FORMAT COLOR COL_TOTAL OFF.
      SKIP.
      LN_CNTR = LN_CNTR + 4.
   ENDAT.
 ENDLOOP.
ENDFORM.

* This routine will build the WTAB table. *
FORM BUILDTAB.
 CLEAR: WTAB.
 MOVE:  BSIS-HKONT TO WTAB-HKONT,
        BSIS-DMBTR TO WTAB-DMBTR,
        BSEG-BELNR TO WTAB-BELNR,
        BSIS-BLART TO WTAB-BLART,
        SKAT-TXT20 TO WTAB-TXT20.
 APPEND WTAB.
ENDFORM.

* This routine will write the detail line. *
FORM WRT_DET.
 IF LN_CNTR >= 59.
    PERFORM WRT_HDG.
 ENDIF.
 WRITE: /50 WTAB-DMBTR, 70 WTAB-BELNR.
 LN_CNTR = LN_CNTR + 1.
ENDFORM.
