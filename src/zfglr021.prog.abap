REPORT ZFGLR021 NO STANDARD PAGE HEADING MESSAGE-ID ZS
                                         LINE-COUNT 65
                                         LINE-SIZE  120.

************************************************************************
*  Client:     Duke Energy.                                            *
*  Date:       January 2005                                            *
*  Author:     Mohammad Khan                                           *
*  Program Description:                                                *
*  This program will download G/L data                                 *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************

TABLES:  BKPF,     "BKPF Accounting Document Header
         BSEG.     "BSEG Accounting Document Segment

DATA: BEGIN OF ITAB OCCURS 0,
*fields for output file.
      BUKRS   LIKE   BKPF-BUKRS,
      HKONT   LIKE   BSEG-HKONT,
      BELNR   LIKE   BKPF-BELNR,
      BKTXT   LIKE   BKPF-BKTXT,
      MONAT   LIKE   BKPF-MONAT,
      GJAHR   LIKE   BKPF-GJAHR,
      CPUDT   LIKE   BKPF-CPUDT,
      BUDAT   LIKE   BKPF-BUDAT,
      USNAM   LIKE   BKPF-USNAM,
      BUZEI   LIKE   BSEG-BUZEI,
      PSWBT   LIKE   BSEG-PSWBT,
      SHKZG   LIKE   BSEG-SHKZG,
      PSWSL   LIKE   BSEG-PSWSL,
      KURSF   LIKE   BKPF-KURSF,
      BVORG   LIKE   BKPF-BVORG,
      DBBLG   LIKE   BKPF-DBBLG,
      STBLG   LIKE   BKPF-STBLG,
      STJAH   LIKE   BKPF-STJAH,
      KOSTL   LIKE   BSEG-KOSTL,
      XBLNR   LIKE   BKPF-XBLNR,
      BLART   LIKE   BKPF-BLART,
*fields for summry report
      DMBTR   LIKE   BSEG-DMBTR,
      END OF ITAB.

DATA: BEGIN OF COMPTAB OCCURS 0,     "Number of records by company
      BUKRS   LIKE BKPF-BUKRS,
      KOUNT   TYPE I,
      END OF COMPTAB.

 DATA: DETL_REC   TYPE STRING,
       SMRY_REC   TYPE STRING,
       FIRST_TIME TYPE C VALUE 'Y',
       W_BUKRS    LIKE BKPF-BUKRS,
       W_HKONT    LIKE BSEG-HKONT,
       W_DMBTR    LIKE BSEG-DMBTR,
       W_TEXT01(12) TYPE C,
       W_TEXT02(25) TYPE C,
       W_TEXT03(12) TYPE C,
       W_LOCATION TYPE C VALUE 'E',
       MSG(80)    TYPE C,
       DET_DOLLARS  LIKE BSEG-DMBTR,
       COMP_DOLLARS LIKE BSEG-DMBTR,
       TOT_DOLLARS  LIKE BSEG-DMBTR,
       W_FILE       LIKE RFPDO-RFBIFILE.

CONSTANTS: DELIMTR  TYPE C VALUE '|'.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME.
PARAMETERS:
  P_YEAR  LIKE BKPF-GJAHR OBLIGATORY.                      "Fiscal year
SELECT-OPTIONS:
  S_BUKRS  FOR BKPF-BUKRS OBLIGATORY,                      "CompanyCode
  S_BUDAT  FOR BKPF-BUDAT OBLIGATORY,                      "Posting Date
  S_HKONT  FOR BSEG-HKONT.                                 "G/L Account#
PARAMETERS:
  P_FILE        LIKE RFPDO-RFBIFILE.                       "File name
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 18(72) TEXT-007.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  CSVFILE       LIKE RFPDO-RFBIFILE.                       "File name
SELECTION-SCREEN END OF BLOCK A1.

************************************************************************
*Initializiation
INITIALIZATION.

IF SY-HOST+0(3) = 'WEI'.
   MOVE 'W' TO W_LOCATION.
ENDIF.
CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3) '/AUDIT01/'
              W_LOCATION 'GLDATA' INTO P_FILE.

CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3) '/AUDIT01/'
              W_LOCATION 'GLSUMMRY.csv' INTO CSVFILE.

************************************************************************

*Start Of Selection
START-OF-SELECTION.
 PERFORM GET_DB_DATA.
 PERFORM PRINT_REPORT.

************************************************************************
FORM GET_DB_DATA.

  SELECT BUKRS BELNR BKTXT MONAT GJAHR CPUDT BUDAT USNAM KURSF
         BVORG DBBLG STBLG STJAH XBLNR BLART
    INTO (BKPF-BUKRS, BKPF-BELNR, BKPF-BKTXT, BKPF-MONAT, BKPF-GJAHR,
          BKPF-CPUDT, BKPF-BUDAT, BKPF-USNAM, BKPF-KURSF, BKPF-BVORG,
          BKPF-DBBLG, BKPF-STBLG, BKPF-STJAH, BKPF-XBLNR, BKPF-BLART)

    FROM BKPF
   WHERE BUKRS IN S_BUKRS
     AND GJAHR  = P_YEAR
     AND BUDAT IN S_BUDAT
     AND BSTAT  = ' '.                     "NEW

     PERFORM MOVE_BKPF_DATA.

  SELECT HKONT BUZEI PSWBT SHKZG PSWSL KOSTL DMBTR
    INTO (BSEG-HKONT, BSEG-BUZEI, BSEG-PSWBT, BSEG-SHKZG,
          BSEG-PSWSL, BSEG-KOSTL, BSEG-DMBTR)
    FROM BSEG
   WHERE BUKRS =  BKPF-BUKRS
     AND BELNR =  BKPF-BELNR
     AND GJAHR =  BKPF-GJAHR
     AND HKONT IN S_HKONT.

     PERFORM MOVE_BSEG_DATA.
  ENDSELECT.
 ENDSELECT.
ENDFORM.

************************************************************************
FORM MOVE_BKPF_DATA.
     CLEAR  ITAB.
     MOVE: BKPF-BUKRS  TO  ITAB-BUKRS,
           BKPF-BELNR  TO  ITAB-BELNR,
           BKPF-BKTXT  TO  ITAB-BKTXT,
           BKPF-MONAT  TO  ITAB-MONAT,
           BKPF-GJAHR  TO  ITAB-GJAHR,
           BKPF-CPUDT  TO  ITAB-CPUDT,
           BKPF-BUDAT  TO  ITAB-BUDAT,
           BKPF-USNAM  TO  ITAB-USNAM,
           BKPF-KURSF  TO  ITAB-KURSF,
           BKPF-BVORG  TO  ITAB-BVORG,
           BKPF-DBBLG  TO  ITAB-DBBLG,
           BKPF-STBLG  TO  ITAB-STBLG,
           BKPF-STJAH  TO  ITAB-STJAH,
           BKPF-XBLNR  TO  ITAB-XBLNR,
           BKPF-BLART  TO  ITAB-BLART.
ENDFORM.

************************************************************************
FORM MOVE_BSEG_DATA.

     MOVE: BSEG-HKONT  TO  ITAB-HKONT,
           BSEG-BUZEI  TO  ITAB-BUZEI,
           BSEG-PSWBT  TO  ITAB-PSWBT,
           BSEG-SHKZG  TO  ITAB-SHKZG,
           BSEG-PSWSL  TO  ITAB-PSWSL,
           BSEG-KOSTL  TO  ITAB-KOSTL,
           BSEG-DMBTR  TO  ITAB-DMBTR.
    APPEND ITAB.

    CLEAR: ITAB-HKONT, ITAB-BUZEI, ITAB-PSWBT, ITAB-SHKZG, ITAB-PSWSL,
           ITAB-KOSTL, ITAB-DMBTR.

ENDFORM.

************************************************************************
FORM PRINT_REPORT.
DATA: W_PSWBT(15)  TYPE C,
      W_KURSF(12)  TYPE C,
      W_DMBTR(15)  TYPE C,
      W_DOLLAR(16) TYPE C.

      MOVE TEXT-102 TO W_TEXT01.
      MOVE TEXT-103 TO W_TEXT03.

  MOVE P_FILE TO W_FILE.
  PERFORM OPEN_SUMMRY_FILE.
  CONCATENATE TEXT-101 TEXT-102 TEXT-103 INTO SMRY_REC
                   SEPARATED BY ','.
  TRANSFER SMRY_REC TO CSVFILE.
  CLEAR:   SMRY_REC.

  SORT ITAB BY BUKRS HKONT.
  LOOP AT ITAB.
    AT NEW BUKRS.
       PERFORM DO_FILE_SET_UP USING ITAB-BUKRS.
    ENDAT.
       CLEAR: W_PSWBT, W_DMBTR.
       IF ITAB-SHKZG = 'S'.
          MOVE: ITAB-PSWBT TO W_PSWBT,
                ITAB-DMBTR TO W_DMBTR.
       ELSE.
          W_PSWBT = ITAB-PSWBT * -1.
          W_DMBTR = ITAB-DMBTR * -1.
       ENDIF.

       MOVE  ITAB-KURSF TO W_KURSF.

       CONCATENATE ITAB-BELNR ITAB-BKTXT ITAB-MONAT ITAB-GJAHR
        ITAB-CPUDT ITAB-BUDAT ITAB-USNAM ITAB-BUZEI ITAB-HKONT
        W_PSWBT    ITAB-SHKZG ITAB-PSWSL W_KURSF    ITAB-BVORG
        ITAB-DBBLG ITAB-STBLG ITAB-STJAH ITAB-BUKRS ITAB-KOSTL
        ITAB-XBLNR ITAB-BLART W_DMBTR
        INTO DETL_REC SEPARATED BY DELIMTR.
       CONDENSE DETL_REC NO-GAPS.
       TRANSFER DETL_REC TO P_FILE.

    MOVE ITAB-BUKRS TO COMPTAB-BUKRS.
    ADD  1          TO COMPTAB-KOUNT.

    MOVE: ITAB-HKONT TO W_HKONT,
          ITAB-BUKRS TO W_BUKRS.

    IF ITAB-SHKZG = 'S'.
       DET_DOLLARS  = DET_DOLLARS  + ITAB-DMBTR.
       COMP_DOLLARS = COMP_DOLLARS + ITAB-DMBTR.
       TOT_DOLLARS  = TOT_DOLLARS  + ITAB-DMBTR.
    ELSE.
       DET_DOLLARS  = DET_DOLLARS  - ITAB-DMBTR.
       COMP_DOLLARS = COMP_DOLLARS - ITAB-DMBTR.
       TOT_DOLLARS  = TOT_DOLLARS  - ITAB-DMBTR.
    ENDIF.

    AT END OF HKONT.
       WRITE:/ W_BUKRS UNDER TEXT-101, W_HKONT UNDER W_TEXT01,
               DET_DOLLARS UNDER W_TEXT03.
       MOVE DET_DOLLARS TO W_DOLLAR.
       PERFORM PUT_SIGN_IN_FRONT CHANGING W_DOLLAR.       "NEW
       CONCATENATE W_BUKRS W_HKONT W_DOLLAR INTO SMRY_REC
                   SEPARATED BY ','.
       TRANSFER SMRY_REC TO CSVFILE.
       CLEAR:   SMRY_REC, DET_DOLLARS.
    ENDAT.

    AT END OF BUKRS.
       APPEND COMPTAB.
       CLEAR  COMPTAB.
       ULINE.
       WRITE:/1 TEXT-105, COMP_DOLLARS UNDER W_TEXT03.
       CLEAR: COMP_DOLLARS.
       ULINE.
    ENDAT.

    AT LAST.
       WRITE:/1 TEXT-106, TOT_DOLLARS UNDER W_TEXT03.
       PERFORM CLOSE_ALL_FILES.
       ULINE.
    ENDAT.
  ENDLOOP.
  PERFORM WRITE_STATISTICS.
ENDFORM.

FORM BUILD_CSV_FILE USING W_TEXT99.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM PUT_SIGN_IN_FRONT.                                        *
*----------------------------------------------------------------------*

FORM PUT_SIGN_IN_FRONT CHANGING VALUE.
  DATA: TEXT1(1) TYPE C.

  SEARCH VALUE FOR '-'.
  IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
    SPLIT VALUE AT '-' INTO VALUE TEXT1.
    CONDENSE VALUE.
    CONCATENATE '-' VALUE INTO VALUE.
  ELSE.
    CONDENSE VALUE.
  ENDIF.
ENDFORM.

************************************************************************
FORM OPEN_SUMMRY_FILE.
  OPEN DATASET CSVFILE FOR OUTPUT IN TEXT MODE MESSAGE MSG.
  IF SY-SUBRC NE '0'.
     MESSAGE E002 WITH CSVFILE MSG.
     STOP.
  ENDIF.
ENDFORM.

************************************************************************
FORM CLOSE_ALL_FILES.
        CLOSE DATASET CSVFILE.
        IF SY-SUBRC NE '0'.
           MESSAGE E019 WITH 'unsuccessfl close' CSVFILE MSG.
        STOP.
        ENDIF.

        CLOSE DATASET P_FILE.
        IF SY-SUBRC NE '0'.
           MESSAGE E019 WITH 'unsuccessfl close' P_FILE MSG.
        STOP.
        ENDIF.
ENDFORM.
************************************************************************
FORM DO_FILE_SET_UP USING W_BUKRS.

     IF FIRST_TIME = 'N'.
        CLOSE DATASET P_FILE.
        IF SY-SUBRC NE '0'.
           MESSAGE E019 WITH 'unsuccessfl close' P_FILE MSG.
        STOP.
        ENDIF.
     ELSE.
        MOVE 'N' TO FIRST_TIME.
     ENDIF.

*   CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3) '/IFFI060/'
*               W_LOCATION 'GLDATA-' W_BUKRS '.txt' INTO P_FILE.

   CONCATENATE W_FILE '_' W_BUKRS '.txt' INTO P_FILE.

        OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE MESSAGE MSG.
        IF SY-SUBRC NE '0'.
           MESSAGE E002 WITH P_FILE MSG.
        STOP.
        ENDIF.

ENDFORM.

************************************************************************

FORM WRITE_STATISTICS.
     MOVE TEXT-107 TO W_TEXT01.
     MOVE TEXT-108 TO W_TEXT02.
     CLEAR W_TEXT03.
     NEW-PAGE.
     LOOP AT COMPTAB.
          WRITE:/ COMPTAB-BUKRS UNDER TEXT-101,
                  COMPTAB-KOUNT UNDER W_TEXT01.
     ENDLOOP.

ENDFORM.
*----------------------  TOP-OF-PAGE  ----------------------------------

TOP-OF-PAGE.
    WRITE: /1 TEXT-RPT, SY-REPID, 25 TEXT-001,               "Report Id
           54 TEXT-FRM, S_BUDAT-LOW, TEXT-TOO, S_BUDAT-HIGH,
           93 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.        "Date,Time
    WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,    "Client
             37 W_TEXT02, 93 TEXT-PGE, SY-PAGNO.             "Page
    ULINE.
    WRITE: /16 TEXT-101, 35 W_TEXT01, 57 W_TEXT03.
    ULINE.
