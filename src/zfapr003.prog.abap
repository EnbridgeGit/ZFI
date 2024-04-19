REPORT ZFAPR003 NO STANDARD PAGE HEADING LINE-SIZE 132.
******************************************************************
*      Owner: Centra/Union Gas Ltd. - BIS                        *
* Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)    *
*       Date: December 9th, 1996                                 *
* Request ID: DRAP0137                                          *
*                                                                *
* The following program will generate a report for payments      *
* (checques issued) that have been cashed before their due dates.*
******************************************************************
TABLES: LFA1,                      "Vendor Master - General Selection
        PAYR,                      "Payment Transfer Medium File
        REGUP.                     "Processed Items From Payment Program

DATA:  TMPDATE  LIKE SY-DATUM,     "Variable to store temporary date.
       LN_CNTR  TYPE I,            "Line Counter
       REC_CNTR TYPE I,            "Record Counter
       TOTAL(8) TYPE P DECIMALS 2. "Final Total

SELECTION-SCREEN BEGIN OF BLOCK INPUTS WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP.

PARAMETERS:
  P_FISCAL LIKE GLT0-RYEAR DEFAULT SY-DATUM(4)   "Fiscal Year
           OBLIGATORY MODIF ID ABC.
PARAMETERS:
  P_CMPNY  LIKE GLT0-BUKRS memory id BUK         "Company Code
           OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK INPUTS.

* The following will highlight the screen's output for certain texts. *
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.
***********************BEGINNING OF MAIN PROGRAM************************

START-OF-SELECTION.
 PERFORM INITIALIZE.
 PERFORM GET_DATA.
 PERFORM FINAL_RTN.
END-OF-SELECTION.

*******************************FORMS************************************

* This routine will initalize the following variables. *
FORM INITIALIZE.
 MOVE '60'         TO LN_CNTR.
 WRITE: '00000000' TO TMPDATE.
ENDFORM.

* This routine will get all appropriate payment data. *
FORM GET_DATA.
 SELECT * FROM REGUP WHERE ZBUKR = P_CMPNY
          AND GJAHR = P_FISCAL.
   SELECT * FROM PAYR WHERE ZBUKR = REGUP-ZBUKR
            AND VBLNR = REGUP-VBLNR AND GJAHR = REGUP-GJAHR
            AND LIFNR = REGUP-LIFNR
            ORDER BY LIFNR CHECT.
     SELECT SINGLE * FROM LFA1 WHERE LIFNR = PAYR-LIFNR.
       IF PAYR-BANCD < REGUP-ZFBDT AND PAYR-BANCD NE TMPDATE.
          PERFORM WRT_DETAIL.
       ENDIF.
   ENDSELECT.
 ENDSELECT.
 IF SY-SUBRC NE 0.
    FORMAT COLOR 6 INVERSE ON.
    WRITE: /1 TEXT-016.
    FORMAT COLOR 6 INVERSE OFF.
 ENDIF.
ENDFORM.

* This routine will write the headings for the report. *
FORM HEADINGS.
 NEW-PAGE NO-TITLE.
 FORMAT INTENSIFIED ON.
 WRITE: /1 TEXT-002,  15 P_FISCAL,  44 TEXT-015,
       107 TEXT-003, 122 P_CMPNY,      SY-PAGNO.
 ULINE: /1(132).
 WRITE: /1 TEXT-004,  14 TEXT-004,  36 TEXT-004,  57 TEXT-008,
        71 TEXT-009,  93 TEXT-008, 100 TEXT-011, 111 TEXT-013,
       122 TEXT-014.
 WRITE: /1 TEXT-019,  14 TEXT-006,  36 TEXT-007,  57 TEXT-005,
        71 TEXT-005,  93 TEXT-010, 100 TEXT-012, 111 TEXT-012,
       122 TEXT-012.
 ULINE: /1(132).
 FORMAT INTENSIFIED OFF.
 MOVE '8' TO LN_CNTR.
ENDFORM.

* This routine will write the detailed line of the report. *
FORM WRT_DETAIL.
 IF LN_CNTR >= 59.
    PERFORM HEADINGS.
 ENDIF.
 TOTAL    = TOTAL + REGUP-WRBTR.
 REC_CNTR = REC_CNTR + 1.
 WRITE: /1 LFA1-LIFNR,  14 LFA1-NAME1(20),  36 LFA1-STRAS(20),
        57 PAYR-CHECT,  71 REGUP-XBLNR(12), 84 REGUP-WRBTR,
       100 PAYR-BANCD, 111 REGUP-ZFBDT,    122 REGUP-BLDAT.
 LN_CNTR = LN_CNTR + 1.
ENDFORM.

* This routine will write the final line - totals for the report. *
FORM FINAL_RTN.
 IF TOTAL EQ 0.
    FORMAT COLOR 6 INVERSE ON.
    WRITE: /1 TEXT-016.
    FORMAT COLOR 6 INVERSE OFF.
 ELSE.
    IF LN_CNTR >= 57.
       PERFORM HEADINGS.
    ENDIF.
    FORMAT COLOR COL_TOTAL INTENSIFIED ON.
    SKIP.
    WRITE: /1 TEXT-017, 10 REC_CNTR,TEXT-018, 82 '$', 83 TOTAL.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
 ENDIF.
ENDFORM.
