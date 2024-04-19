REPORT ZFGLR020 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132.

***********************************************************************
*     Program: ZFGLR020                                               *
*  Programmer: G. YMANA                                               *
*      Client: Union Gas                                              *
*        Date: 2001/09/14                                             *
*                                                                     *
* This program will report on the GL figures coming out of Banner     *
* and summarize them by total amount by 'Gas', 'Rental', & 'Other'    *
*                                                                     *
* 2002/12/03 - GYMANA - Added serv. type 'GLIB' to be recognized as   *
*                       GAS.                                          *
* 2005/08/22 - GYMANA - All input records now have consumption volumes*
*                       Modified report to only show consumption from *
*                       Commodity records.                            *
* 2005/08/30 - GYMANA - Added additional checks for GL class codes:   *
*                       OWNS,FRAN,CMP,CGC.  These codes should have   *
*                       their volumes reported.                       *
* 2006/01/24 - GYMANA - Modified input record to accommodate new rate *
*                       Rider fields and show consumption on all      *
*                       records                                       *
*                                                                     *
***********************************************************************
DATA: INREC(250),
      MSG_TEXT(50).

DATA: BEGIN OF INTAB OCCURS 300,
       SERV_TYPE(5)     TYPE C,             "Service Type
       SERV_CAT(4)      TYPE C,             "Service Category
       CLASS_CD(4)      TYPE C,             "GL Class Code
       SERV_CLASS(2)    TYPE C,             "Service Class
       ADJ_CODE(4)      TYPE C,
       TRAN_AMT(14)     TYPE P DECIMALS 2,  "Transaction Amount
       BUDG_AMT(12)     TYPE P DECIMALS 2,  "Budget Amount
       BUDTRN_DIFF(12)  TYPE P DECIMALS 2,  "Budget Trans Difference
       CONS_AMT(14)     TYPE P DECIMALS 3,  "Consumption Amount
       NUM_OF_CUSTS(6)  TYPE N,             "No. of Customers
       EFF_DATE         TYPE D,             "Effective Date
      END OF INTAB.

DATA:
      POST_DATE          TYPE D,                "Posted Date
      LN_CNTR            TYPE I VALUE 0,        "Line Counter
      PREV_CLASS_CD      LIKE INTAB-CLASS_CD VALUE SPACE,
      RPT_CLASS_CD       LIKE INTAB-CLASS_CD,
      SUM_TRAN_AMT       LIKE INTAB-TRAN_AMT,
      SUM_BUDG_AMT       LIKE INTAB-BUDG_AMT,
      SUM_BUDTRN_DIFF    LIKE INTAB-BUDTRN_DIFF,
      SUM_CONS_AMT       LIKE INTAB-CONS_AMT,
      SUM_NUM_OF_CUSTS   LIKE INTAB-NUM_OF_CUSTS,
      GRAND_TRAN_AMT     LIKE INTAB-TRAN_AMT,
      GRAND_BUDG_AMT     LIKE INTAB-BUDG_AMT,
      GRAND_BUDTRN_DIFF  LIKE INTAB-BUDTRN_DIFF,
      GRAND_CONS_AMT     LIKE INTAB-CONS_AMT,
      GRAND_NUM_OF_CUSTS LIKE INTAB-NUM_OF_CUSTS.


************************* SELECTION SCREEN *****************************
SELECTION-SCREEN SKIP.
SELECTION-SCREEN begin of block box2 with frame.
PARAMETERS: DATAFILE(128) type c lower case
            DEFAULT '/usr/sap/interfaces/P01/BANNER/zbis100.chk'.
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN SKIP.

*********************BEGINNING OF MAIN PROGRAM *************************

START-OF-SELECTION.
 PERFORM INITIALIZE.
 READ DATASET DATAFILE INTO INREC.
 IF SY-SUBRC EQ 0.
    PERFORM GET_DATA.
    PERFORM GENERATE_GAS_RPT.
    PERFORM GENERATE_RENT_RPT.
    PERFORM GENERATE_OTHER_RPT.
 ENDIF.
END-OF-SELECTION.

*************************BEGINNING OF FORMS*****************************

* This routine will initialize some variables *
FORM INITIALIZE.
 REFRESH: INTAB.
 MOVE '60' TO LN_CNTR.

 OPEN DATASET DATAFILE FOR INPUT IN TEXT MODE
              MESSAGE MSG_TEXT.
 IF SY-SUBRC NE 0.
    WRITE: 'File cannot be opened. Reason: ', MSG_TEXT.
    EXIT.
 ENDIF.
ENDFORM.

* This routine will get all info. for the cleared items selected. *
FORM GET_DATA.
 MOVE INREC(8) TO POST_DATE.
 DO.
   PERFORM POP_INTAB.
   READ DATASET DATAFILE INTO INREC.
   IF SY-SUBRC NE 0.
      EXIT.
   ENDIF.
 ENDDO.
ENDFORM.

* This routine will populate the table INTAB. *
FORM POP_INTAB.
 CLEAR: INTAB.
 IF INREC+8(3) EQ 'GAS' OR
    INREC+8(4) EQ 'CHRT' OR
    INREC+8(4) EQ 'GLIB' OR
    INREC+8(4) EQ 'PEAK'.
    MOVE 'GAS  ' TO INTAB-SERV_TYPE.
 ELSEIF INREC+8(4) EQ 'RENT' OR
        INREC+8(4) EQ 'RNGV' OR
        INREC+8(4) EQ 'RVRA'.
        MOVE 'RENT ' TO INTAB-SERV_TYPE.
 ELSE.
        MOVE 'OTHER' TO INTAB-SERV_TYPE.
 ENDIF.

 MOVE:   INREC+16(4)  TO INTAB-CLASS_CD,
         INREC+46(13) TO INTAB-TRAN_AMT.
 IF INREC+45(1) EQ '-'.
    INTAB-TRAN_AMT = INTAB-TRAN_AMT * -1.
 ENDIF.
 IF INTAB-SERV_TYPE EQ 'GAS  '.
    MOVE:  INREC+12(4)  TO INTAB-SERV_CAT,
           INREC+20(2)  TO INTAB-SERV_CLASS,
           INREC+103(4) TO INTAB-ADJ_CODE,
           INREC+60(11) TO INTAB-BUDG_AMT,
           INREC+85(6)  TO INTAB-NUM_OF_CUSTS,
           INREC+91(8)  TO INTAB-EFF_DATE,
           INREC+72(13) TO INTAB-CONS_AMT.
* All records now contain volumes. Only Commodity records
* ('C' on 4th pos) and codes OWNS,FRAN,CMP,CGC
* will have their volumes passed to the report.      GY
*    IF INREC+19(1) EQ 'C' OR
*       INREC+16(4) EQ 'OWNS' OR
*       INREC+16(4) EQ 'FRAN' OR
*       INREC+16(3) EQ 'CMP' OR
*       INREC+16(3) EQ 'CGC'.
*       MOVE INREC+72(13) TO INTAB-CONS_AMT.
*    ELSE.
*       MOVE 0 TO INTAB-CONS_AMT.
*    ENDIF.
    IF INREC+59(1) EQ '-'.
       INTAB-BUDG_AMT = INTAB-BUDG_AMT * -1.
    ENDIF.
    IF INREC+71(1) EQ '-'.
       INTAB-CONS_AMT = INTAB-CONS_AMT * -1.
    ENDIF.
    IF INREC+28(1) EQ 'Y' AND INTAB-BUDG_AMT NE 0.
       INTAB-BUDTRN_DIFF = INTAB-BUDG_AMT - INTAB-TRAN_AMT.
    ELSE.
       INTAB-BUDTRN_DIFF = 0.
    ENDIF.
 ENDIF.
 COLLECT INTAB.
ENDFORM.

* This routine will process all relevant gas info. and display it. *
FORM GENERATE_GAS_RPT.
 SORT: INTAB BY SERV_TYPE CLASS_CD SERV_CLASS EFF_DATE.
 LOOP AT INTAB.
   IF INTAB-SERV_TYPE EQ 'GAS  '.
      IF LN_CNTR >= 59.
         PERFORM WRT_GAS_HDG.
      ENDIF.
      AT NEW CLASS_CD.
         SKIP.
         LN_CNTR = LN_CNTR + 1.
      ENDAT.
      IF INTAB-CLASS_CD <> PREV_CLASS_CD.
         MOVE: INTAB-CLASS_CD TO RPT_CLASS_CD,
               INTAB-CLASS_CD TO PREV_CLASS_CD.
      ELSE.
         MOVE SPACE TO RPT_CLASS_CD.
      ENDIF.
      WRITE:  / RPT_CLASS_CD UNDER TEXT-005.
      WRITE:  17 INTAB-CLASS_CD, 21 INTAB-SERV_CLASS, INTAB-ADJ_CODE, 29 'TOTAL'.
      WRITE:  (16) INTAB-TRAN_AMT UNDER TEXT-007 DECIMALS 2,
              (16) INTAB-BUDG_AMT UNDER TEXT-008 DECIMALS 2,
              (16) INTAB-BUDTRN_DIFF UNDER TEXT-009 DECIMALS 2,
              (16) INTAB-CONS_AMT UNDER TEXT-010 DECIMALS 3,
               (7) INTAB-NUM_OF_CUSTS UNDER TEXT-011,
               (10) INTAB-EFF_DATE UNDER TEXT-013 USING
                    EDIT MASK '____/__/__'.
      LN_CNTR = LN_CNTR + 1.
      ADD: INTAB-TRAN_AMT TO SUM_TRAN_AMT,
           INTAB-TRAN_AMT TO GRAND_TRAN_AMT.
      ADD: INTAB-BUDG_AMT TO SUM_BUDG_AMT,
           INTAB-BUDG_AMT TO GRAND_BUDG_AMT.
      ADD: INTAB-BUDTRN_DIFF TO SUM_BUDTRN_DIFF,
           INTAB-BUDTRN_DIFF TO GRAND_BUDTRN_DIFF.
      ADD: INTAB-CONS_AMT TO SUM_CONS_AMT,
           INTAB-CONS_AMT TO GRAND_CONS_AMT.
      ADD: INTAB-NUM_OF_CUSTS TO SUM_NUM_OF_CUSTS,
           INTAB-NUM_OF_CUSTS TO GRAND_NUM_OF_CUSTS.
      AT END OF CLASS_CD.
         SKIP.
         WRITE: /17 'TOTAL'.
         WRITE (16) SUM_TRAN_AMT UNDER TEXT-007 DECIMALS 2.
         WRITE (16) SUM_BUDG_AMT UNDER TEXT-008 DECIMALS 2.
         WRITE (16) SUM_BUDTRN_DIFF UNDER TEXT-009 DECIMALS 2.
         WRITE (16) SUM_CONS_AMT UNDER TEXT-010 DECIMALS 3.
         WRITE (7) SUM_NUM_OF_CUSTS UNDER TEXT-011.
         LN_CNTR = LN_CNTR + 2.
         CLEAR: SUM_TRAN_AMT, SUM_BUDG_AMT, SUM_BUDTRN_DIFF.
         CLEAR: SUM_CONS_AMT, SUM_NUM_OF_CUSTS.
      ENDAT.
   ENDIF.
 ENDLOOP.
 SKIP.
 WRITE /19 'GRAND TOTAL'.
 WRITE (16) GRAND_TRAN_AMT UNDER TEXT-007 DECIMALS 2.
 WRITE (16) GRAND_BUDG_AMT UNDER TEXT-008 DECIMALS 2.
 WRITE (16) GRAND_BUDTRN_DIFF UNDER TEXT-009 DECIMALS 2.
 WRITE (16) GRAND_CONS_AMT UNDER TEXT-010 DECIMALS 3.
 WRITE (7)  GRAND_NUM_OF_CUSTS UNDER TEXT-011.
 LN_CNTR = LN_CNTR + 2.
ENDFORM.

* This routine writes the GAS report headings. *
FORM WRT_GAS_HDG.
 NEW-PAGE.
 CLEAR LN_CNTR.
 FORMAT INTENSIFIED ON.
 SKIP.
 WRITE: /1 TEXT-001, 39 TEXT-002, 70 POST_DATE.
 WRITE: 106 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
 WRITE: /1 TEXT-004, 41 '----------', 56 INTAB-SERV_TYPE.
 WRITE  66 '----------'.
 WRITE: 121 TEXT-PGE, SY-PAGNO.
 SKIP 2.
 WRITE: /3 TEXT-005, 17 TEXT-006, 34 TEXT-007, 54 TEXT-008.
 WRITE: 73 TEXT-009, 92 TEXT-010, 112 TEXT-011, 122 TEXT-013.
 SKIP.
 MOVE '7' TO LN_CNTR.
 FORMAT INTENSIFIED OFF.
ENDFORM.

* This routine will process all relevant rental info. and display it. *
FORM GENERATE_RENT_RPT.
 CLEAR SUM_TRAN_AMT.
 PERFORM WRT_RENT_HDG.
 SORT: INTAB BY SERV_TYPE CLASS_CD.
 LOOP AT INTAB.
   IF INTAB-SERV_TYPE EQ 'RENT '.
      IF LN_CNTR >= 59.
         PERFORM WRT_RENT_HDG.
      ENDIF.
      WRITE / INTAB-CLASS_CD UNDER TEXT-012.
      WRITE (16) INTAB-TRAN_AMT UNDER TEXT-007 DECIMALS 2.
      LN_CNTR = LN_CNTR + 1.
      ADD INTAB-TRAN_AMT TO SUM_TRAN_AMT.
   ENDIF.
 ENDLOOP.
 SKIP.
 WRITE: / 'GRAND TOTAL' UNDER TEXT-012.
 WRITE (16) SUM_TRAN_AMT UNDER TEXT-007 DECIMALS 2.
 LN_CNTR = LN_CNTR + 2.
 CLEAR: SUM_TRAN_AMT.
ENDFORM.

* This routine writes the RENT report headings. *
FORM WRT_RENT_HDG.
 NEW-PAGE.
 CLEAR LN_CNTR.
 FORMAT INTENSIFIED ON.
 SKIP.
 WRITE: /1 TEXT-001, 39 TEXT-002, 70 POST_DATE.
 WRITE: 106 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
 WRITE: /1 TEXT-004, 41 '----------', 56 'RENTAL', 66 '----------'.
 WRITE: 121 TEXT-PGE, SY-PAGNO.
 SKIP.
 WRITE: /19 TEXT-012, 36 TEXT-007.
 SKIP.
 MOVE '6' TO LN_CNTR.
 FORMAT INTENSIFIED OFF.
ENDFORM.

* This routine will process all relevant Other info. and display it. *
FORM GENERATE_OTHER_RPT.
 CLEAR SUM_TRAN_AMT.
 PERFORM WRT_OTHER_HDG.
 SORT: INTAB BY SERV_TYPE CLASS_CD.
 LOOP AT INTAB.
   IF INTAB-SERV_TYPE EQ 'OTHER'.
      IF LN_CNTR >= 59.
         PERFORM WRT_OTHER_HDG.
      ENDIF.
      WRITE / INTAB-CLASS_CD UNDER TEXT-012.
      WRITE (16) INTAB-TRAN_AMT UNDER TEXT-007 DECIMALS 2.
      LN_CNTR = LN_CNTR + 1.
      ADD INTAB-TRAN_AMT TO SUM_TRAN_AMT.
   ENDIF.
 ENDLOOP.
 SKIP.
 WRITE: / 'GRAND TOTAL' UNDER TEXT-012.
 WRITE (16) SUM_TRAN_AMT UNDER TEXT-007 DECIMALS 2.
 LN_CNTR = LN_CNTR + 2.
 CLEAR: SUM_TRAN_AMT.
ENDFORM.

* This routine writes the RENT report headings. *
FORM WRT_OTHER_HDG.
 NEW-PAGE.
 CLEAR LN_CNTR.
 FORMAT INTENSIFIED ON.
 SKIP.
 WRITE: /1 TEXT-001, 39 TEXT-002, 70 POST_DATE.
 WRITE: 106 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
 WRITE: /1 TEXT-004, 41 '----------', 56 'OTHER', 66 '----------'.
 WRITE: 121 TEXT-PGE, SY-PAGNO.
 SKIP.
 WRITE: /19 TEXT-012, 36 TEXT-007.
 SKIP.
 MOVE '6' TO LN_CNTR.
 FORMAT INTENSIFIED OFF.
ENDFORM.
