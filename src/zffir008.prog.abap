REPORT ZFFIR008 NO STANDARD PAGE HEADING LINE-SIZE 147
                LINE-COUNT 58 MESSAGE-ID PP.

************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        March, 2005.                                           *
*  Issue Log:   1073                                                   *
*  Description:                                                        *
*     - The purpose of this program is to produce the reports          *
*       Intercompany Receivable/Payable Reports.                       *
*                                                                      *
*                                                                      *
*CHANGES:                                                              *
* 2005/08/24 mkhan TR105 Select Actual amount only as PLAN amount is   *
*                        going to be introduces in FI.                 *
*                                                                      *
************************************************************************

TABLES: T001U,          "Clearing Between Company Codes
        LFC1,           "LFC1 Vendor master (transaction figures)
        GLT0.           "G/L account master record transaction figures
DATA:
  BEGIN OF REPTAB OCCURS 0,
    VBUKR  LIKE T001U-VBUKR,      "Company Code Which Is Being Posted To
    AMT_MHPC TYPE P DECIMALS 2,    "$-MHPC which will be Cleared Against
    AMT_MHPM TYPE P DECIMALS 2,    "$-MHPM which will be Cleared Against
    AMT_SC96 TYPE P DECIMALS 2,    "$-SC96 which will be Cleared Against
    AMT_SCPM TYPE P DECIMALS 2,    "$-SCPM which will be Cleared Against
    AMT_SCPP TYPE P DECIMALS 2,    "$-SCPP which will be Cleared Against
    AMT_UGL  TYPE P DECIMALS 2,    "$-UGL  which will be Cleared Against
    AMT_OTHR TYPE P DECIMALS 2,    "$-other which will be Clered Against
    AMT_TOTL TYPE P DECIMALS 2,    "$-total which will be Clered Against
   END OF REPTAB.

DATA: BEGIN OF STRUC1,
       UM01H LIKE LFC1-UM01H,
       UM02H LIKE LFC1-UM01H,
       UM03H LIKE LFC1-UM01H,
       UM04H LIKE LFC1-UM01H,
       UM05H LIKE LFC1-UM01H,
       UM06H LIKE LFC1-UM01H,
       UM07H LIKE LFC1-UM01H,
       UM08H LIKE LFC1-UM01H,
       UM09H LIKE LFC1-UM01H,
       UM10H LIKE LFC1-UM01H,
       UM11H LIKE LFC1-UM01H,
       UM12H LIKE LFC1-UM01H,
       UM13H LIKE LFC1-UM01H,
       UM14H LIKE LFC1-UM01H,
       UM15H LIKE LFC1-UM01H,
       UM16H LIKE LFC1-UM01H,
      END OF STRUC1.

DATA: BEGIN OF STRUC2,
       UM01S LIKE LFC1-UM01S,
       UM02S LIKE LFC1-UM01S,
       UM03S LIKE LFC1-UM01S,
       UM04S LIKE LFC1-UM01S,
       UM05S LIKE LFC1-UM01S,
       UM06S LIKE LFC1-UM01S,
       UM07S LIKE LFC1-UM01S,
       UM08S LIKE LFC1-UM01S,
       UM09S LIKE LFC1-UM01S,
       UM10S LIKE LFC1-UM01S,
       UM11S LIKE LFC1-UM01S,
       UM12S LIKE LFC1-UM01S,
       UM13S LIKE LFC1-UM01S,
       UM14S LIKE LFC1-UM01S,
       UM15S LIKE LFC1-UM01S,
       UM16S LIKE LFC1-UM01S,
      END OF STRUC2.

DATA:
   W_HEAD01(50)  TYPE C,
   W_DOLLARS     LIKE GLT0-HSL01,
   W_PERIOF      LIKE T001B-FRPE1,
   TOT_UMH       LIKE W_DOLLARS,
   TOT_UMS       LIKE W_DOLLARS.

DATA: W_RRCTY LIKE GLT0-RRCTY,
      W_RVERS LIKE GLT0-RVERS.

*                                  TR105.
CONSTANTS: TRUE    TYPE BOOLEAN VALUE 'X',
           FLASE   TYPE BOOLEAN VALUE '-',
           UNKNOWN TYPE BOOLEAN VALUE ' '.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS: S_VBUKR FOR T001U-VBUKR.

PARAMETERS:
       P_FYEAR  LIKE GLT0-RYEAR  DEFAULT SY-DATUM+0(4) OBLIGATORY,
       P_PERIOF LIKE T001B-FRPE1                       OBLIGATORY.
SELECTION-SCREEN SKIP 1.
*                                                              TR105
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-128.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER: B_ACTUAL RADIOBUTTON GROUP BUTN DEFAULT 'X' USER-COMMAND RAD.
SELECTION-SCREEN COMMENT 4(15) TEXT-003.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETER:   B_PLAN   RADIOBUTTON GROUP BUTN.  "Plan
SELECTION-SCREEN COMMENT 4(15) TEXT-004.
SELECTION-SCREEN POSITION 22.
PARAMETERS: P_RVERS LIKE GLT0-RVERS.

SELECTION-SCREEN COMMENT 26(15) TEXT-005.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN END OF BLOCK BOX.

******************************************************************TR105
AT SELECTION-SCREEN OUTPUT.
   CASE TRUE.
        WHEN B_ACTUAL.
             LOOP AT SCREEN.
                  IF SCREEN-NAME = 'P_RVERS'.
                     SCREEN-INPUT = '0'.
                     CLEAR: P_RVERS.
                     MODIFY SCREEN.
                  ENDIF.
             ENDLOOP.

        WHEN B_PLAN.
             LOOP AT SCREEN.
                  IF SCREEN-NAME = 'P_RVERS'.
                     SCREEN-INPUT = '1'.
                     IF P_RVERS = SPACE.
                        P_RVERS = '002'.
                     ENDIF.
                     MODIFY SCREEN.
                  ENDIF.
             ENDLOOP.
   ENDCASE.
************************************************************************
********************** START-OF-SELECTION ******************************
************************************************************************
START-OF-SELECTION.
  IF P_PERIOF = '012'.
     MOVE '016' TO W_PERIOF.
  ELSE.
     MOVE P_PERIOF TO W_PERIOF.
  ENDIF.

  PERFORM GET_RECEIVABLE_DATA.
*  IF NOT REPTAB[] IS INITIAL.
     CONCATENATE TEXT-101 P_FYEAR '/' P_PERIOF INTO W_HEAD01
                          SEPARATED BY SPACE.
     PERFORM PAGE_HEADING.
     PERFORM WRITE_DATA.
*  ENDIF.

  CLEAR   REPTAB.
  REFRESH REPTAB.
  PERFORM GET_PAYABLE_DATA.
*  IF NOT REPTAB[] IS INITIAL.
     SKIP 6.
     CONCATENATE TEXT-102 P_FYEAR '/' P_PERIOF INTO W_HEAD01
                          SEPARATED BY SPACE.
     PERFORM PAGE_HEADING.
     PERFORM WRITE_DATA.
*  ENDIF.

************************************************************************
*************************GET_RECEIVABLE_DATA****************************
************************************************************************
FORM GET_RECEIVABLE_DATA.

 IF B_ACTUAL = 'X'.
    MOVE 0 TO W_RRCTY.
    WRITE '001' TO W_RVERS.
 ELSE.
    MOVE 1 TO W_RRCTY.
    MOVE P_RVERS TO W_RVERS.
 ENDIF.

 SELECT VBUKR ABUKR KONTS
   INTO (T001U-VBUKR, T001U-ABUKR, T001U-KONTS)
   FROM T001U
  WHERE VBUKR IN S_VBUKR.

   SELECT * FROM GLT0
    WHERE   RLDNR = '00'                  "Ledger Only
      AND   RRCTY = W_RRCTY               "                TR105
      AND   RVERS = W_RVERS               "                TR105
      AND   BUKRS = T001U-VBUKR           "Company Code
      AND   RYEAR = P_FYEAR               "Year
      AND   RACCT = T001U-KONTS.          "G/L Account #

     CLEAR: W_DOLLARS.
     ADD GLT0-HSL01 FROM 1 TO W_PERIOF GIVING W_DOLLARS.
     W_DOLLARS = W_DOLLARS + GLT0-HSLVT.           "Add Bal.Carried fwd
     PERFORM BUILD_REPORT_TABLE USING W_DOLLARS T001U-VBUKR T001U-ABUKR.

   ENDSELECT.
 ENDSELECT.

ENDFORM.

************************************************************************
****************************GET_PAYABLE_DATA****************************
************************************************************************
FORM GET_PAYABLE_DATA.

SELECT VBUKR ABUKR KONTH BSCHH
  INTO (T001U-VBUKR, T001U-ABUKR, T001U-KONTH, T001U-BSCHH)
  FROM T001U
 WHERE VBUKR IN S_VBUKR.

IF T001U-BSCHH = '31'.
   SELECT  * FROM  LFC1
    WHERE  LIFNR = T001U-KONTH
      AND  BUKRS = T001U-VBUKR
      AND  GJAHR = P_FYEAR.
     CLEAR: W_DOLLARS, STRUC1, STRUC2.
     MOVE-CORRESPONDING LFC1 TO STRUC1.
     MOVE-CORRESPONDING LFC1 TO STRUC2.
     ADD STRUC1-UM01H FROM 1 TO W_PERIOF GIVING TOT_UMH.
     ADD STRUC2-UM01S FROM 1 TO W_PERIOF GIVING TOT_UMS.
     W_DOLLARS = TOT_UMS + LFC1-UMSAV - TOT_UMH.
     PERFORM BUILD_REPORT_TABLE USING W_DOLLARS T001U-VBUKR T001U-ABUKR.
    ENDSELECT.
 ELSE.
    SELECT * FROM GLT0
     WHERE   RLDNR = '00'                  "Ledger Only
       AND   RRCTY = W_RRCTY               "                  TR105
       AND   RVERS = W_RVERS               "                  TR105
       AND   BUKRS = T001U-VBUKR           "Company Code
       AND   RYEAR = P_FYEAR               "Year
       AND   RACCT = T001U-KONTH.          "G/L Account #

     CLEAR: W_DOLLARS.
     ADD GLT0-HSL01 FROM 1 TO W_PERIOF GIVING W_DOLLARS.
     W_DOLLARS = W_DOLLARS + GLT0-HSLVT.           "Add Bal.Carried fwd
     PERFORM BUILD_REPORT_TABLE USING W_DOLLARS T001U-VBUKR T001U-ABUKR.
   ENDSELECT.
 ENDIF.
 ENDSELECT.

ENDFORM.

************************************************************************
**************************BUILD_REPORT_TABLE****************************
************************************************************************
FORM BUILD_REPORT_TABLE USING W_DOLLARS W_VBUKR W_ABUKR.

    CASE W_ABUKR.
         WHEN 'MHPC'.
               MOVE W_DOLLARS TO REPTAB-AMT_MHPC.
         WHEN 'MHPM'.
               MOVE W_DOLLARS TO REPTAB-AMT_MHPM.
         WHEN 'SC96'.
               MOVE W_DOLLARS TO REPTAB-AMT_SC96.
         WHEN 'SCPM'.
               MOVE W_DOLLARS TO REPTAB-AMT_SCPM.
         WHEN 'SCPP'.
               MOVE W_DOLLARS TO REPTAB-AMT_SCPP.
         WHEN 'UGL '.
               MOVE W_DOLLARS TO REPTAB-AMT_UGL.
         WHEN OTHERS.
               MOVE W_DOLLARS TO REPTAB-AMT_OTHR.
    ENDCASE.

    MOVE W_DOLLARS TO REPTAB-AMT_TOTL.
    MOVE W_VBUKR TO REPTAB-VBUKR.
    COLLECT REPTAB.
    CLEAR   REPTAB.

ENDFORM.

**----------------------------------------------------------------------
*
**               WRITE_DATA.
*
**----------------------------------------------------------------------
*
FORM WRITE_DATA.
SORT REPTAB BY VBUKR.
LOOP AT REPTAB.
*WRITE:/ REPTAB-VBUKR    UNDER TEXT-C01, REPTAB-AMT_MHPC UNDER TEXT-C02,
*        REPTAB-AMT_MHPM UNDER TEXT-C03, REPTAB-AMT_SC96 UNDER TEXT-C04,
*        REPTAB-AMT_SCPM UNDER TEXT-C05, REPTAB-AMT_SCPP UNDER TEXT-C06,
*        REPTAB-AMT_UGL  UNDER TEXT-C07, REPTAB-AMT_OTHR UNDER TEXT-C08,
*        REPTAB-AMT_TOTL UNDER TEXT-C01.
 WRITE:/ REPTAB-VBUKR    UNDER TEXT-C01, 13 REPTAB-AMT_MHPC,
        30 REPTAB-AMT_MHPM, 47 REPTAB-AMT_SC96,
        64 REPTAB-AMT_SCPM, 81 REPTAB-AMT_SCPP,
        98 REPTAB-AMT_UGL, 115 REPTAB-AMT_OTHR,
        132 REPTAB-AMT_TOTL.
AT LAST.
 SUM.
 ULINE.
 WRITE: /2 TEXT-C09,        13 REPTAB-AMT_MHPC,
        30 REPTAB-AMT_MHPM, 47 REPTAB-AMT_SC96,
        64 REPTAB-AMT_SCPM, 81 REPTAB-AMT_SCPP,
        98 REPTAB-AMT_UGL, 115 REPTAB-AMT_OTHR,
        132 REPTAB-AMT_TOTL.
ULINE.
ENDAT.
ENDLOOP.
ENDFORM.

**----------------------------------------------------------------------
*
**               TOP-OF-PAGE.
*
**----------------------------------------------------------------------
*
TOP-OF-PAGE.
WRITE: /.
**----------------------------------------------------------------------
*
**               PAGE_HEADING.
*
**----------------------------------------------------------------------
*

FORM PAGE_HEADING.
  WRITE: /1 TEXT-RPT, SY-REPID,  48 W_HEAD01,
        113 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
  ULINE.

  WRITE: /2 TEXT-C01, 25 TEXT-C02, 42 TEXT-C03, 59 TEXT-C04,
         76 TEXT-C05, 93 TEXT-C06, 110 TEXT-C07, 126 TEXT-C08,
         142 TEXT-C09.

   ULINE.
ENDFORM.
************************************************************************
**************************END OF PROGRAM********************************
************************************************************************
