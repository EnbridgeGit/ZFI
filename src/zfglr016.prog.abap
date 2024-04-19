REPORT ZFGLR016 LINE-COUNT 65 LINE-SIZE 132 NO STANDARD PAGE HEADING
       MESSAGE-ID FB.
*----------------------------------------------------------------------*
*  Author: Selwyn Rodricks                                             *
*          OmniLogic Systems Group                                     *
*  Brief Description:                                                  *
*  - FI - Cashed Checks Reconciliation Validation                      *
*----------------------------------------------------------------------*

TABLES: PAYR.                          "Payment transfer medium file

TABLES: T001,                          " Company codes
        T012,                          " House banks
        T012K,                         " House bank accounts
        BHDGD.                   " Common data area batch heading rout

DATA: HBKID          LIKE T012K-HBKID,
      HDCNT          TYPE I,
      HKTID          LIKE T012K-HKTID.
DATA: VALUT          LIKE SY-DATUM.
DATA: STATUS(1)      TYPE C.
DATA: WAERS          LIKE BKPF-WAERS,
      WRBTR          LIKE BSEG-WRBTR.

DATA: BELNR          LIKE BKPF-BELNR,
      BLDAT          LIKE BKPF-BLDAT,
      BUDAT          LIKE BKPF-BUDAT.

DATA: CHAR10(10)     TYPE C,
      CHAR17(17)     TYPE C,
      CHAR60(60)     TYPE C,
      CHAR130(130)   TYPE C,
      CHAR132(132)   TYPE C.
DATA: DOLLARS LIKE BSEG-WRBTR.


RANGES: R_RZAWE FOR PAYR-RZAWE.

SELECTION-SCREEN  BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-100.
PARAMETERS: CK_FILE      LIKE RFPDO1-FEBAUSZF,
            PCUPLOAD     LIKE RFPDO1-FEBPCUPLD.
SELECTION-SCREEN  END OF BLOCK 1.

*---- checks  ---------------------------------------------------------*
DATA: BEGIN OF CHECKS OCCURS 100,
        REC(300) TYPE C,
      END OF CHECKS.

*---- head record of check format -------------------------------------*
DATA: BEGIN OF HEAD.
        INCLUDE STRUCTURE CHECK_HEAD.
DATA: END OF HEAD.

*----- single check record  -------------------------------------------*
DATA: BEGIN OF CKREC.
        INCLUDE STRUCTURE CHECK_REC.
DATA: END OF CKREC.

DATA: BEGIN OF ITAB OCCURS 200,
        PDATE  LIKE CKREC-PDATE,       "Encashment data
        DIFF   LIKE BSEG-WRBTR.        "Difference
DATA: END OF ITAB.





AT SELECTION-SCREEN ON BLOCK 1.
  IF PCUPLOAD = 'X'.
    IF  CK_FILE+1(1) NE ':'.
      SET CURSOR FIELD 'CK_FILE'.
      MESSAGE E706.
    ENDIF.
  ENDIF.

START-OF-SELECTION.

  PERFORM READ_DATA.                   "Load internal tables
  PERFORM SUMMARY.                     "Totals



*---------------------------------------------------------------*
*  FORM READ_DATA.
*---------------------------------------------------------------*
*  Read Data from PC or UNIX-File-System into table CHECKS      *
*---------------------------------------------------------------*
FORM READ_DATA.
  REFRESH: CHECKS.
  IF PCUPLOAD = 'X'.
*   upload data from pc-drive
    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              FILENAME        = CK_FILE
              FILETYPE        = 'ASC'
         TABLES
              DATA_TAB        = CHECKS
         EXCEPTIONS
              FILE_OPEN_ERROR = 1
              FILE_READ_ERROR = 2.
    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE E704 WITH CK_FILE+2 CK_FILE+0(2).
      WHEN 2.
        MESSAGE E705 WITH CK_FILE+2 CK_FILE+0(2).
      WHEN OTHERS.
    ENDCASE.
  ELSE.
*   upload data from unix-file-system
*------- open check-file  --------------------------------------
    OPEN DATASET CK_FILE IN TEXT MODE FOR INPUT.
    IF SY-SUBRC NE 0.
      MESSAGE E002 WITH CK_FILE.
    ENDIF.

*------- store checks in internal Table CHECKS ------------------
    DO.
      CLEAR CHECKS.
      READ DATASET CK_FILE INTO CHECKS-REC.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
      APPEND CHECKS.
    ENDDO.
    CLOSE DATASET CK_FILE.
  ENDIF.


  LOOP AT CHECKS.
    CASE CHECKS-REC+0(1).
      WHEN '1'.
*       Header-Record
        MOVE CHECKS-REC TO HEAD.
        PERFORM PROCESS_HEADER.

      WHEN '5'.
*       Data-Record
        MOVE CHECKS-REC TO CKREC.
        PERFORM PROCESS_SINGLE_CHECK.
    ENDCASE.
  ENDLOOP.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM PROCESS_HEADER                                           *
*---------------------------------------------------------------------*
FORM PROCESS_HEADER.
  CLEAR: HBKID, HKTID.
  HDCNT = HDCNT + 1.
  FORMAT COLOR COL_HEADING.

*  initialize header
  CLEAR:  VALUT,
          BUDAT,
          BLDAT,
          WAERS,
          STATUS.

  T012-BANKL  = HEAD-BANKL.
  T012K-BANKN = HEAD-ACCNR.

  CALL FUNCTION 'GET_BANK_ACCOUNT'
       EXPORTING
            I_BANKL                = T012-BANKL
            I_BANKN                = T012K-BANKN
       IMPORTING
            E_T012                 = T012
            E_T012K                = T012K
       EXCEPTIONS
            BANK_ACCOUNT_NOT_FOUND = 01
            BANK_KEY_FALSE         = 02
            BANK_KEY_NOT_FOUND     = 03
            MULTIPLE_BANK_ACCOUNT  = 04
            INPUT_WRONG            = 05.

  CASE SY-SUBRC.
    WHEN 0.
*     alles ok
    WHEN 1.
      MESSAGE A736(FB) WITH HEAD-BANKL  HEAD-ACCNR.
    WHEN 2.
      MESSAGE A735(FB) WITH HEAD-BANKL  HEAD-ACCNR.
    WHEN 3.
      MESSAGE A755(FB) WITH HEAD-BANKL.
    WHEN 4.
      MESSAGE A757(FB) WITH HEAD-ACCNR  HEAD-BANKL.
    WHEN OTHERS.
      MESSAGE A736(FB) WITH HEAD-BANKL  HEAD-ACCNR.
  ENDCASE.



  HBKID = T012K-HBKID.
  HKTID = T012K-HKTID.


  SELECT SINGLE * FROM T001 WHERE BUKRS = T012K-BUKRS.
  IF SY-SUBRC NE 0.
    MESSAGE A701 WITH T012K-BUKRS.
  ENDIF.

* fill Range with Check-Payment-method
  REFRESH R_RZAWE.
  CALL FUNCTION 'GET_CHECK_PAYMENT_METHODS'
       EXPORTING
            I_LAND1 = T001-LAND1
       TABLES
            T_RZAWE = R_RZAWE.


* print new header-record
  FORMAT COLOR COL_HEADING.
* IF HDCNT = 1.
  CLEAR:  BHDGD-LINE1, BHDGD-LINE2.
  BHDGD-LINE1 = SY-TITLE.
  CHAR60      = TEXT-003.
  BHDGD-LINE2 = CHAR60.
  BHDGD-INIFL = '0'.
  BHDGD-BUKRS = T012K-BUKRS.
  BHDGD-UNAME = SY-UNAME.
  BHDGD-REPID = SY-REPID.
  NEW-PAGE.
* ELSE.
*   WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.
* ENDIF.
  WRITE:  /1      SY-VLINE,
           2      TEXT-030,
                  HDCNT,
           132    SY-VLINE.
  WRITE:  /1      SY-VLINE,
           2      TEXT-031,
           17(10) HEAD-BANKL,
           29(19) TEXT-032,
           49(05) HBKID,
           132    SY-VLINE.
  WRITE:  /1      SY-VLINE,
           2      TEXT-033,
           17(10) HEAD-ACCNR,
           29(19) TEXT-034,
           49(05) HKTID,
           132    SY-VLINE.
  ULINE.

  FORMAT RESET.
ENDFORM.



*----------------------------------------------------------------------*
* FORM PROCESS_SINGLE_CHECK.                                           *
*----------------------------------------------------------------------*
*  Check if check is in PAYR and not cashed                            *
*----------------------------------------------------------------------*
FORM PROCESS_SINGLE_CHECK.
*-------- prenumbered checks ------------------------------------------*
  CLEAR: ITAB.
  SELECT * FROM PAYR WHERE ZBUKR =  T012K-BUKRS
                      AND   HBKID =  HBKID
                      AND   HKTID =  HKTID
                      AND   RZAWE IN R_RZAWE
                      AND   CHECT =  CKREC-CKNUM.

    PAYR-RWBTR = - PAYR-RWBTR.
    WRITE: /'SAP--->',
               PAYR-CHECT,             "Check#
               PAYR-VBLNR,             "SAP pmt doc#
               PAYR-RWBTR,             "Amount
               PAYR-ZALDT.             "Check dt
    ITAB-DIFF = ITAB-DIFF + PAYR-RWBTR.
  ENDSELECT.
  IF SY-SUBRC <> 0.
    WRITE: /'SAP---> Not found'.
  ENDIF.

  DOLLARS = CKREC-AMOUNT / 100.
  ITAB-DIFF = ITAB-DIFF - DOLLARS.
  WRITE: /'BANK-->',
             CKREC-CKNUM,              "CHECK NUMBER
          36 DOLLARS,                  "AMOUNT IN DOLLARS
             CKREC-PDATE,              "CHECK ENCASHMENT DATE
             ITAB-DIFF.                "Difference

  IF ITAB-DIFF <> 0.
    ITAB-PDATE = CKREC-PDATE.
    APPEND ITAB.
  ENDIF.

  IF CKREC-PDATE < PAYR-ZALDT.
    WRITE: 'Cashed Early'.
  ENDIF.
  ULINE.
ENDFORM.


*----------------------------------------------------------------------*
* SUMMARY                                                              *
*----------------------------------------------------------------------*
FORM SUMMARY.
  SKIP.SKIP.SKIP.ULINE.
  WRITE: /'Differences by Cashed Dates:'.
  ULINE.
  SORT ITAB BY PDATE.
  LOOP AT ITAB.
    AT END OF PDATE.
      SUM.
      WRITE: / ITAB-PDATE, ITAB-DIFF.
    ENDAT.
  ENDLOOP.
ENDFORM.


* top of page
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         37 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
         80 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-001 UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10



  WRITE: / TEXT-101, 69 TEXT-103.
  WRITE: / TEXT-102.
  ULINE.
