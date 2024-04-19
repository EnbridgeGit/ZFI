*&---------------------------------------------------------------------*
*& Report  ZLSDC014_BANNER_SALEORDER_SUMM
*&
*&**********************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        January 2015.                                          *
*  Ticket #:    76173                                                  *
*  Description:                                                        *
*     - The purpose of this program is to summerize Banner Sales Order *
*       file. The Output File zbis100salesorder.dat created by program *
*       ZLSDC001_BANNER_GL_SPLIT will be used as input to this program.*
*&---------------------------------------------------------------------*
*&CHANGES****
*&
*&---------------------------------------------------------------------*

REPORT  ZLSDC014_BANNER_SALEORDER_SUMM.

* Input/Output file format

DATA:  BEGIN OF IN_TAB occurs 0,
       PROCESS_DATE       TYPE D,
       SERV_TYPE(4)       TYPE C,
       SERV_CAT(4)        TYPE C,
       GL_CLASSCD(4)      TYPE C,
       SERV_CLASS(2)      TYPE C,
       TOWN_CD(2)         TYPE C,
       MUNIC_CD(4)        TYPE C,
       BUDGET_IND(1)      TYPE C,
       CUST_NUM(16)       TYPE N,
       TRANS_AMT_SIGN(1)  TYPE C,
       TRANS_AMT(13)      TYPE C,
       CUST_CHRG_SIGN(1)  TYPE C,
       CUST_CHRG(11)      TYPE C,
       CONS_SIGN(1)       TYPE C,
       CONS(13)           TYPE C,
       NO_OF_CUSTS(6)     TYPE N,
       EFF_DATE           TYPE D,
       RATE_CLASS(4)      TYPE C,
       END OF IN_TAB.

DATA:  BEGIN OF SORDER_TAB occurs 0.
           INCLUDE STRUCTURE IN_TAB.
DATA:  END OF SORDER_TAB.

DATA:  CURR_KEY_FIELD(16) TYPE C,
       PREV_KEY_FIELD(16) TYPE C,
       S_AMT           TYPE NAFAL,
       S_CHRG          TYPE NAFAL,
       S_CONS          TYPE pw_bmsch.
CONSTANTS: delimtr  TYPE c VALUE ','.
*------------------------  Selection Screen  --------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
PARAMETER: INFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY.
SELECTION-SCREEN SKIP.
* Output Files
PARAMETERS:
OUTFILE LIKE FILENAMECI-FILEEXTERN OBLIGATORY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK BOX.

*---------------------------------------------------------------------*
*------------------------  Initialization  ---------------------------*
*---------------------------------------------------------------------*
INITIALIZATION.
CONCATENATE:
'/usr/sap/interfaces/' SY-SYSID(3) '/BANNER/zbis100salesorder.dat'
                                                        INTO INFILE,
'/usr/sap/interfaces/' SY-SYSID(3) '/BANNER/zbis100Sumsalesorder.dat'
                                                        INTO OUTFILE.

***********************************************************************
START-OF-SELECTION.
  OPEN DATASET INFILE FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
     MESSAGE E006(ZM) WITH INFILE.
  ENDIF.

  OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC NE 0.
     MESSAGE E006(ZM) WITH OUTFILE.
  ENDIF.

  DO.
    CLEAR IN_TAB.
    READ DATASET INFILE INTO IN_TAB.
    IF SY-SUBRC <> '0'.
      EXIT.
    ENDIF.
    APPEND IN_TAB.
    CLEAR  IN_TAB.
  ENDDO.
  SORT IN_TAB BY GL_CLASSCD SERV_CLASS TOWN_CD MUNIC_CD RATE_CLASS.

  LOOP AT IN_TAB.
   CONCATENATE IN_TAB-GL_CLASSCD IN_TAB-SERV_CLASS IN_TAB-TOWN_CD
               IN_TAB-MUNIC_CD IN_TAB-RATE_CLASS INTO CURR_KEY_FIELD.
   IF PREV_KEY_FIELD = SPACE.
      MOVE CURR_KEY_FIELD TO PREV_KEY_FIELD.
      PERFORM MOVE_COMMON_DATA.
      PERFORM ADD_AMT_AND_QTY.
   ELSEIF CURR_KEY_FIELD = PREV_KEY_FIELD.
      PERFORM ADD_AMT_AND_QTY.
   ELSE.
      PERFORM BUILD_TAB_DATA.
      PERFORM MOVE_COMMON_DATA.
      PERFORM ADD_AMT_AND_QTY.
      MOVE CURR_KEY_FIELD TO PREV_KEY_FIELD.
  ENDIF.
 ENDLOOP.
  IF SORDER_TAB <> SPACE.
     PERFORM BUILD_TAB_DATA.
  ENDIF.
PERFORM TRANSFER_DATA.

***********************************************************************
FORM BUILD_TAB_DATA.
      IF S_AMT < 0. MOVE '-' TO SORDER_TAB-TRANS_AMT_SIGN.
                    S_AMT = S_AMT * -1.
      ELSE. MOVE '+' TO SORDER_TAB-TRANS_AMT_SIGN.
      ENDIF.
      IF S_CHRG < 0. MOVE '-' TO SORDER_TAB-CUST_CHRG_SIGN.
                     S_CHRG = S_CHRG * -1.
      ELSE. MOVE '+' TO SORDER_TAB-CUST_CHRG_SIGN.
      ENDIF.
      IF S_CONS < 0. MOVE '-' TO SORDER_TAB-CONS_SIGN.
                     S_CONS = S_CONS * -1.
      ELSE. MOVE '+' TO SORDER_TAB-CONS_SIGN.
      ENDIF.
      MOVE: S_AMT  TO SORDER_TAB-TRANS_AMT,
            S_CHRG TO SORDER_TAB-CUST_CHRG,
            S_CONS TO SORDER_TAB-CONS.
      APPEND SORDER_TAB.
      CLEAR: SORDER_TAB, S_AMT, S_CHRG, S_CONS.
ENDFORM.

***********************************************************************
FORM MOVE_COMMON_DATA.
      MOVE:  IN_TAB-PROCESS_DATE TO SORDER_TAB-PROCESS_DATE,
             IN_TAB-SERV_TYPE    TO SORDER_TAB-SERV_TYPE,
             IN_TAB-SERV_CAT     TO SORDER_TAB-SERV_CAT,
             IN_TAB-GL_CLASSCD   TO SORDER_TAB-GL_CLASSCD,
             IN_TAB-SERV_CLASS   TO SORDER_TAB-SERV_CLASS,
             IN_TAB-TOWN_CD      TO SORDER_TAB-TOWN_CD,
             IN_TAB-MUNIC_CD     TO SORDER_TAB-MUNIC_CD,
             IN_TAB-BUDGET_IND   TO SORDER_TAB-BUDGET_IND,
             IN_TAB-CUST_NUM     TO SORDER_TAB-CUST_NUM,
             IN_TAB-NO_OF_CUSTS  TO SORDER_TAB-NO_OF_CUSTS,
             IN_TAB-EFF_DATE     TO SORDER_TAB-EFF_DATE,
             IN_TAB-RATE_CLASS TO SORDER_TAB-RATE_CLASS.
ENDFORM.

FORM ADD_AMT_AND_QTY.
     IF IN_TAB-TRANS_AMT_SIGN = '-'.
        S_AMT  = S_AMT  + ( IN_TAB-TRANS_AMT * -1 ).
     ELSE.
        S_AMT  = S_AMT  + IN_TAB-TRANS_AMT.
     ENDIF.

     IF IN_TAB-CUST_CHRG_SIGN = '-'.
        S_CHRG = S_CHRG + ( IN_TAB-CUST_CHRG * -1 ).
     ELSE.
        S_CHRG = S_CHRG + IN_TAB-CUST_CHRG.
     ENDIF.

     IF IN_TAB-CONS_SIGN = '-'.
        S_CONS  = S_CONS  + ( IN_TAB-CONS * -1 ).
     ELSE.
        S_CONS  = S_CONS  + IN_TAB-CONS.
     ENDIF.
ENDFORM.

FORM TRANSFER_DATA.
     IF NOT SORDER_TAB[] IS INITIAL.
        LOOP AT SORDER_TAB.
        TRANSFER SORDER_TAB TO OUTFILE.
        ENDLOOP.
     ENDIF.
ENDFORM.
