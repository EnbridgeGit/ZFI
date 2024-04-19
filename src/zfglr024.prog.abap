REPORT ZFGLR024 NO STANDARD PAGE HEADING MESSAGE-ID ZS
                                         LINE-COUNT 65
                                         LINE-SIZE  120.

************************************************************************
*  Client:     Duke Energy.                                            *
*  Date:       MAY 2005                                                *
*  Author:     Mohammad Khan                                           *
*  Program Description:                                                *
*  This program will download G/L data and produces summry report.     *
*  This program is a clone of ZFGLR021 and changes are made to add     *
*  and changes some fields, produce single files instead of one file   *
*  per company as per issue log 1101. Date format is changed and header*
*  record added to the file as well.                                   *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue   By      Date    Description                                   *
* TR94  Mohammad 05-7-26 Create Touch file on the server so that Maestro
*                        can pic the file and put it on NT server. Also*
*                        keep the option for not to create Touch file  *
*                        through a check box.                          *
************************************************************************

TABLES:  BKPF,    "BKPF Accounting Document Header
         BSEG,    "BSEG Accounting Document Segment
         SKAT,    "G/L Acct Master Rec (Chart of Accounts: Description)
         USREFUS, "Reference user for internet applications
         USR21,   "Assign user name address key
         ADRP.    "Persons (central address administration)

DATA: BEGIN OF ITAB OCCURS 0,
*fields for output file.
      BUKRS   LIKE   BKPF-BUKRS,                "Company code
      HKONT   LIKE   BSEG-HKONT,                "G/L Account
      TXT50   LIKE   SKAT-TXT50,                "G/L Acct. Description
      BELNR   LIKE   BKPF-BELNR,                "Document number
      BKTXT   LIKE   BKPF-BKTXT,                "Document Header Text
      MONAT   LIKE   BKPF-MONAT,                "Fiscal period
      GJAHR   LIKE   BKPF-GJAHR,                "Fiscal year
      CPUDT   LIKE   BKPF-CPUDT,                "Acct doc entry date
      BUDAT   LIKE   BKPF-BUDAT,                "Posting date in the doc
      USNAM   LIKE   BKPF-USNAM,                "User id
      NAME_TEXT LIKE ADRP-NAME_TEXT,            "Name (First, last)
      USERALIAS LIKE USREFUS-USERALIAS,         "Internet user alias
      BUZEI   LIKE   BSEG-BUZEI,                "# of Line In Acct Doc
      WRBTR   LIKE   BSEG-WRBTR,                "Amount in doc currency
      SGTXT   LIKE   BSEG-SGTXT,                "item Text
      SHKZG   LIKE   BSEG-SHKZG,                "Debit/credit indicator
      WAERS   LIKE   BKPF-WAERS,                "Currency Key
      KURSF   LIKE   BKPF-KURSF,                "Exchange rate
      BVORG   LIKE   BKPF-BVORG,  "# of Cross-Company Code Posting Trans
      DBBLG   LIKE   BKPF-DBBLG,                "Recurring Entry Doc #
      STBLG   LIKE   BKPF-STBLG,                "Reverse Document Number
      STJAH   LIKE   BKPF-STJAH,                "Reverse Doc Fiscal Year
      KOSTL   LIKE   BSEG-KOSTL,                "Cost Center
      XBLNR   LIKE   BKPF-XBLNR,                "Reference doc number
      BLART   LIKE   BKPF-BLART,                "Document type
      DMBTR   LIKE   BSEG-DMBTR,                "Amount in local curency
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
       W_LOCATION TYPE C VALUE 'e',
       W_KTOPL      LIKE SKAT-KTOPL VALUE 'COAT',
       MSG(80)    TYPE C,
       DET_DOLLARS  LIKE BSEG-DMBTR,
       COMP_DOLLARS LIKE BSEG-DMBTR,
       TOT_DOLLARS  LIKE BSEG-DMBTR,
       W_FILE       LIKE RFPDO-RFBIFILE.

*                                  TR94.
CONSTANTS: TRUE    TYPE BOOLEAN VALUE 'X',
           FLASE   TYPE BOOLEAN VALUE '-',
           UNKNOWN TYPE BOOLEAN VALUE ' '.


CONSTANTS: DELIMTR  TYPE C VALUE '|'.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME.
PARAMETERS:
  P_YEAR  LIKE BKPF-GJAHR OBLIGATORY.                      "Fiscal year
SELECT-OPTIONS:
  S_BUKRS  FOR BKPF-BUKRS OBLIGATORY,                      "CompanyCode
  S_BUDAT  FOR BKPF-BUDAT OBLIGATORY,                      "Posting Date
  S_HKONT  FOR BSEG-HKONT.                                 "G/L Account#
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  P_FILE        LIKE RFPDO-RFBIFILE.                       "File name
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  CSVFILE       LIKE RFPDO-RFBIFILE.                       "File name
SELECTION-SCREEN SKIP 1.                     "TR94

PARAMETERS: P_TUCH AS CHECKBOX DEFAULT 'X' user-command RAD.
PARAMETERS:                                  "TR94
  TUCHFILE      LIKE RFPDO-RFBIFILE.         "TR94        "File name
SELECTION-SCREEN END OF BLOCK A1.

************************************************************************
*Initializiation
INITIALIZATION.

MOVE TRUE TO P_TUCH.                                          "TR94

IF SY-HOST+0(3) = 'WEI'.
   MOVE 'w'    TO W_LOCATION.
   MOVE 'WECA' TO W_KTOPL.            "Chart of account
ENDIF.
CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3) '/IFFI063/'
              W_LOCATION 'gldata_intrnl.txt' INTO P_FILE.

CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3) '/IFFI063/'
              W_LOCATION 'glsummry_intrnl.csv' INTO CSVFILE.

*CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3) '/IFFI063/'  "TR94
*              W_LOCATION 'GLINTRNL.tch' INTO TUCHFILE.        "TR94

*******************************************************************TR94
AT SELECTION-SCREEN OUTPUT.
   CASE TRUE.
        WHEN P_TUCH.
             LOOP AT SCREEN.
                  IF SCREEN-NAME = 'TUCHFILE'.
                     SCREEN-INPUT = '1'.
                     CONCATENATE '/usr/sap/interfaces/' SY-SYSID+0(3)
                    '/IFFI063/' W_LOCATION 'glintrnl.tch' INTO TUCHFILE.
                     MODIFY SCREEN.
                  ENDIF.
             ENDLOOP.

        WHEN OTHERS.
             LOOP AT SCREEN.
                  IF SCREEN-NAME = 'TUCHFILE'.
                     CLEAR TUCHFILE.
                     SCREEN-INPUT = '0'.
                     MODIFY SCREEN.
                  ENDIF.
             ENDLOOP.
   ENDCASE.

************************************************************************

*Start Of Selection
START-OF-SELECTION.
 PERFORM GET_DB_DATA.
 PERFORM PRINT_REPORT.
 IF P_TUCH = 'X'.                                  "TR94
    IF NOT ITAB[] IS INITIAL.                      "TR94
       PERFORM CREATE_TOUCH_FILE.                  "TR94
    ENDIF.                                         "TR94
 ENDIF.                                            "TR94

************************************************************************
FORM GET_DB_DATA.

  SELECT BUKRS BELNR BKTXT MONAT GJAHR CPUDT BUDAT USNAM KURSF
         BVORG DBBLG STBLG STJAH XBLNR BLART WAERS
    INTO (BKPF-BUKRS, BKPF-BELNR, BKPF-BKTXT, BKPF-MONAT, BKPF-GJAHR,
          BKPF-CPUDT, BKPF-BUDAT, BKPF-USNAM, BKPF-KURSF, BKPF-BVORG,
          BKPF-DBBLG, BKPF-STBLG, BKPF-STJAH, BKPF-XBLNR, BKPF-BLART,
          BKPF-WAERS)

    FROM BKPF
   WHERE BUKRS IN S_BUKRS
     AND GJAHR  = P_YEAR
     AND BUDAT IN S_BUDAT
     AND BSTAT  = ' '.

     PERFORM MOVE_BKPF_DATA.

  SELECT HKONT BUZEI WRBTR SHKZG KOSTL DMBTR SGTXT
    INTO (BSEG-HKONT, BSEG-BUZEI, BSEG-WRBTR, BSEG-SHKZG,
          BSEG-KOSTL, BSEG-DMBTR, BSEG-SGTXT)
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
           BKPF-USNAM  TO  ITAB-USNAM,
           BKPF-KURSF  TO  ITAB-KURSF,
           BKPF-BVORG  TO  ITAB-BVORG,
           BKPF-DBBLG  TO  ITAB-DBBLG,
           BKPF-STBLG  TO  ITAB-STBLG,
           BKPF-STJAH  TO  ITAB-STJAH,
           BKPF-XBLNR  TO  ITAB-XBLNR,
           BKPF-BLART  TO  ITAB-BLART,
           BKPF-WAERS  TO  ITAB-WAERS.

           CONCATENATE BKPF-CPUDT+4(2) BKPF-CPUDT+6(2)
                       BKPF-CPUDT+0(4) INTO ITAB-CPUDT.
           CONCATENATE BKPF-BUDAT+4(2) BKPF-BUDAT+6(2)
                       BKPF-BUDAT+0(4) INTO ITAB-BUDAT.

 IF BKPF-USNAM = 'BATCH' OR BKPF-USNAM = 'BATCHMAN'.
    MOVE BKPF-USNAM    TO  ITAB-USERALIAS.
 ELSE.
    PERFORM GET_EMPLOYEE_NUMBER.
 ENDIF.
    PERFORM GET_USER_NAME.
ENDFORM.

************************************************************************
FORM MOVE_BSEG_DATA.

     MOVE: BSEG-HKONT  TO  ITAB-HKONT,
           BSEG-BUZEI  TO  ITAB-BUZEI,
           BSEG-WRBTR  TO  ITAB-WRBTR,
           BSEG-SHKZG  TO  ITAB-SHKZG,
           BSEG-KOSTL  TO  ITAB-KOSTL,
           BSEG-DMBTR  TO  ITAB-DMBTR,
           BSEG-SGTXT  TO  ITAB-SGTXT.

    PERFORM GET_ACCOUNT_DESCRIPTION.
    APPEND ITAB.

    CLEAR: ITAB-HKONT, ITAB-BUZEI, ITAB-WRBTR,
           ITAB-SHKZG, ITAB-KOSTL, ITAB-DMBTR,
           ITAB-SGTXT, ITAB-TXT50.

ENDFORM.

************************************************************************
FORM GET_ACCOUNT_DESCRIPTION.

 SELECT SINGLE TXT50 INTO SKAT-TXT50
   FROM SKAT
  WHERE SPRAS = 'EN'
    AND KTOPL = W_KTOPL
    AND SAKNR = BSEG-HKONT.

  IF SY-SUBRC = 0.
    MOVE SKAT-TXT50 TO ITAB-TXT50.
  ELSE.
    CONCATENATE BSEG-HKONT ' - NOT FOUND IN TABLE SKAT' INTO ITAB-TXT50.
  ENDIF.

ENDFORM.
************************************************************************
FORM GET_EMPLOYEE_NUMBER.

 SELECT SINGLE USERALIAS INTO USREFUS-USERALIAS
   FROM USREFUS
  WHERE BNAME = BKPF-USNAM.

  IF SY-SUBRC = 0.
     MOVE USREFUS-USERALIAS TO ITAB-USERALIAS.
  ELSE.
  CONCATENATE BKPF-USNAM ' - NOT FOUND IN TABLE USREFUS'
                             INTO ITAB-USERALIAS.
  ENDIF.

ENDFORM.
************************************************************************
FORM GET_USER_NAME.

 SELECT SINGLE PERSNUMBER INTO USR21-PERSNUMBER
   FROM USR21
  WHERE BNAME = BKPF-USNAM.

  IF SY-SUBRC = 0.
     SELECT SINGLE NAME_TEXT INTO ADRP-NAME_TEXT
       FROM ADRP
      WHERE PERSNUMBER = USR21-PERSNUMBER.

      IF SY-SUBRC = 0.
         MOVE ADRP-NAME_TEXT TO ITAB-NAME_TEXT.
      ELSE.
         CONCATENATE USR21-PERSNUMBER 'NOT FOUND IN TABLE ADRP'
                                       INTO ITAB-NAME_TEXT.
      ENDIF.
  ELSE.
     CONCATENATE BKPF-USNAM 'NOT FOUND IN TABLE USR21'
                             INTO ITAB-NAME_TEXT.
  ENDIF.

ENDFORM.
************************************************************************
FORM PRINT_REPORT.
DATA: W_WRBTR(15)  TYPE C,
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
  PERFORM OPEN_DETAIL_FILE.

*Append header line to the file
  CONCATENATE TEXT-011 TEXT-012 TEXT-013 TEXT-014
              TEXT-015 TEXT-016 TEXT-017 TEXT-018
              TEXT-019 TEXT-020 TEXT-021 TEXT-022
              TEXT-023 TEXT-024 TEXT-025 TEXT-026 TEXT-027
              TEXT-028 TEXT-029 TEXT-030 TEXT-031 TEXT-032
              TEXT-033 TEXT-034 TEXT-035 TEXT-036
                        INTO DETL_REC SEPARATED BY DELIMTR.
  TRANSFER DETL_REC TO P_FILE.
  CLEAR DETL_REC.

  LOOP AT ITAB.
       CLEAR: W_WRBTR, W_DMBTR.
       IF ITAB-SHKZG = 'S'.
          MOVE: ITAB-WRBTR TO W_WRBTR,
                ITAB-DMBTR TO W_DMBTR.
       ELSE.
          W_WRBTR = ITAB-WRBTR * -1.
          W_DMBTR = ITAB-DMBTR * -1.
       ENDIF.

       MOVE  ITAB-KURSF TO W_KURSF.

       CONCATENATE ITAB-BELNR ITAB-BKTXT ITAB-MONAT ITAB-GJAHR
        ITAB-CPUDT ITAB-BUDAT ITAB-USNAM ITAB-NAME_TEXT
        ITAB-USERALIAS        ITAB-BUZEI ITAB-HKONT ITAB-TXT50
        W_WRBTR    ITAB-SGTXT ITAB-SHKZG ITAB-WAERS W_KURSF
        ITAB-BVORG ITAB-DBBLG ITAB-STBLG ITAB-STJAH ITAB-BUKRS
        ITAB-KOSTL ITAB-XBLNR ITAB-BLART W_DMBTR
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
       PERFORM PUT_SIGN_IN_FRONT CHANGING W_DOLLAR.
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
***************************************************************** TR94
FORM CREATE_TOUCH_FILE.
     OPEN DATASET TUCHFILE FOR OUTPUT IN TEXT MODE MESSAGE MSG.
        IF SY-SUBRC NE '0'.
           MESSAGE E002 WITH TUCHFILE MSG.
        STOP.
        ENDIF.

  TRANSFER TEXT-010 TO TUCHFILE.

     CLOSE DATASET TUCHFILE.
        IF SY-SUBRC NE '0'.
           MESSAGE E019 WITH 'unsuccessfl close' TUCHFILE MSG.
        STOP.
        ENDIF.

ENDFORM.
************************************************************************
FORM OPEN_DETAIL_FILE.
     OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE MESSAGE MSG.
        IF SY-SUBRC NE '0'.
           MESSAGE E002 WITH P_FILE MSG.
        STOP.
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
