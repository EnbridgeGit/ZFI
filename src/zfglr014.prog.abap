REPORT ZFGLR014 LINE-COUNT 65 LINE-SIZE 80  NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
*  Author: Selwyn Rodricks                                             *
*          OmniLogic Systems Group                                     *
*  Brief Description:                                                  *
*  - FI - Checks cashed within a selected date range                   *
***** ***** ***** ***** ****** ***** ***** ***** ***** ***** ***** *****
*    modified by Nancy Gilligan, OmniLogic 98/10     D30K906141        *
*      - standardized header, set company code based on client         *
*----------------------------------------------------------------------*

TABLES: PAYR,                        "Payment transfer medium file
        T001.                        "Company Code       ngilligan 98/10

PARAMETERS:     ZBUKR  LIKE PAYR-ZBUKR,"Comp.Code
                HBKID  LIKE PAYR-HBKID,"Bank ID
                HKTID  LIKE PAYR-HKTID."Account ID
SELECT-OPTIONS: BANCD  FOR PAYR-BANCD OBLIGATORY. "Encashment Dt

DATA: BEGIN OF ITAB OCCURS 200,
        BANCD LIKE PAYR-BANCD,
        CHECT LIKE PAYR-CHECT,
        VBLNR LIKE PAYR-VBLNR,
        RWBTR LIKE PAYR-RWBTR,
        ZALDT LIKE PAYR-ZALDT.
DATA: END OF ITAB.

* initialization                                         ngilligan 98/10
INITIALIZATION.                                         "ngilligan 98/10
IF SY-MANDT+2(1) = '1'.                                 "ngilligan 98/10
     ZBUKR = 'UEC'.                                     "ngilligan 98/10
ELSEIF  SY-MANDT+2(1) = '0'.                            "ngilligan 98/10
     ZBUKR = 'UGL'.                                     "ngilligan 98/10
ENDIF.                                                  "ngilligan 98/10




START-OF-SELECTION.
  CLEAR ITAB.
  REFRESH ITAB.

* get company code text                                  ngilligan 98/10
SELECT SINGLE BUTXT FROM T001 INTO T001-BUTXT WHERE BUKRS = ZBUKR.

  SELECT * FROM PAYR WHERE ZBUKR =  ZBUKR AND
                           HBKID =  HBKID AND
                           HKTID =  HKTID AND
                           BANCD IN BANCD.
    CHECK PAYR-VOIDR IS INITIAL.
    IF PAYR-RWBTR <> 0.
      MOVE-CORRESPONDING PAYR TO ITAB.
      ITAB-RWBTR = - ITAB-RWBTR.
      APPEND ITAB.
    ENDIF.
  ENDSELECT.

  IF SY-SUBRC <> 0.
    WRITE: / 'No cashed cheques found'.
    EXIT.
  ENDIF.


  SORT ITAB BY BANCD CHECT.
  LOOP AT ITAB.
    WRITE: / ITAB-BANCD,               "Encashment dt
             ITAB-CHECT,               "Check#
             ITAB-RWBTR,               "Amount
             ITAB-VBLNR,               "SAP pmt doc#
             ITAB-ZALDT.               "Check dt
    AT END OF BANCD.
      SUM.
      ULINE.
      WRITE: / ITAB-BANCD,             "Encashment dt
            12 'Sub Total',
            26 ITAB-RWBTR.             "Amount
      ULINE.
      SKIP.
    ENDAT.

    AT LAST.
      SUM.
      ULINE.
      WRITE: /12 'TOTAL',
              26 ITAB-RWBTR.           "Amount
      ULINE.
    ENDAT.
  ENDLOOP.

END-OF-SELECTION .



* top of page
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         23 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
         53 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-001 UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 1.                                         "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10
  WRITE: /20 TEXT-111, ZBUKR.
  WRITE: /20 TEXT-112, HBKID.
  WRITE: /20 TEXT-113, HKTID.
  WRITE: /20 TEXT-114, BANCD-LOW.
  IF NOT BANCD-HIGH IS INITIAL.
    WRITE: '-', BANCD-HIGH.
  ENDIF.
  ULINE.
  WRITE: / TEXT-101.
  WRITE: 47 TEXT-102.
  ULINE.
