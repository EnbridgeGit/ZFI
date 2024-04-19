REPORT ZROLLUPMNTHLY MESSAGE-ID ZA
       NO STANDARD PAGE HEADING LINE-SIZE 255.
*----------------------------------------------------------------------*
*  Author    : Glenn Ymana               SAP : West
*  Date      : Nov, 2009                 Program Type : Interface
*  Issue Log : TR582 IFRS Project
*----------------------------------------------------------------------*
*  Title : Monthly Roll Up from ledger 0L to R1
*  Description:
*     This ABAP will execute the transaction FAGL25 with all active
*     company codes using the current year and previous period as the
*     range.  Since FAGL25 can only execute one company code at a time
*     All company codes are loaded into the ABAP variant and a loop is
*     set up to process each company code.
*----------------------------------------------------------------------*

TABLES: FAGLFLEXT.  "G/L Account Master table - Used for field def only

DATA:
      W_FIRST(1),
      W_FIRST_TIME(1)     TYPE C.

DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: NODATA VALUE ' '.              "nodata

*---------------------------------------------------------------------*
*   S E L E C T I O N - S C R E E N
*---------------------------------------------------------------------*
*- General Data Selection
SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:
  S_BUKRS  FOR FAGLFLEXT-RBUKRS NO INTERVALS.          "CompanyCode

PARAMETERS:
  P_FAGLV(20) TYPE C DEFAULT 'R1_FLEX1',
  P_LEDGER    LIKE FAGLFLEXT-RLDNR
                   DEFAULT '0L' OBLIGATORY,            "Ledger
  P_FRMDTE    LIKE SY-DATUM OBLIGATORY,                "From Date
  P_TODATE    LIKE SY-DATUM OBLIGATORY,                "To Date
  P_GROUP     LIKE APQI-GROUPID   DEFAULT 'Z_FAGL25' OBLIGATORY.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_CFWD   AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN COMMENT  4(24) TEXT-003.
    PARAMETERS: P_CFWDYR LIKE SY-DATUM+0(4).
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_SPPER    AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN COMMENT  4(20) TEXT-004.
  SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: P_DETSTA   AS CHECKBOX DEFAULT 'X'.
*    SELECTION-SCREEN COMMENT  4(20) TEXT-005.
*  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B2.


SELECTION-SCREEN END OF BLOCK A1.

START-OF-SELECTION.

REFRESH BDCDATA.
W_FIRST = 'Y'.
LOOP AT S_BUKRS.
  IF W_FIRST = 'Y'.
     W_FIRST = 'N'.
     PERFORM OPEN_BDC_SESSION.
     W_FIRST_TIME = 'Y'.
  ENDIF.

  PERFORM BDC_SCREEN      USING 'SAPMGLRV' '2000'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'T807-ROLLUP'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/8'.
  PERFORM BDC_FIELD       USING 'T807-ROLLUP'
                                P_FAGLV.
  PERFORM BDC_SCREEN      USING 'SAPMGLRV' '0100'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=CLOS'.
  PERFORM BDC_FIELD       USING 'T802G-VALUE(01)'
                                S_BUKRS-LOW.
*  PERFORM BDC_FIELD       USING 'T802G-VALUE(02)'
*                                P_LEDGER.
  PERFORM BDC_SCREEN      USING 'SAPMGLRV' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=CONT'.
  PERFORM BDC_FIELD       USING 'RGLRO-FROM_DATE'
                                P_FRMDTE.
  PERFORM BDC_FIELD       USING 'RGLRO-CARRY_FORW'
                                P_CFWD.
  PERFORM BDC_FIELD       USING 'RGLRO-CARRY_YEAR'
                                P_CFWDYR.
  PERFORM BDC_FIELD       USING 'RGLRO-TO_DATE'
                                P_TODATE.
  PERFORM BDC_FIELD       USING 'RGLRO-SP_PER'
                                P_SPPER.
*  PERFORM BDC_FIELD       USING 'RGLRO-DETAIL_STA'
*                                P_DETSTA.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/8'.
  PERFORM INSERT_SESSION.
  REFRESH BDCDATA.
ENDLOOP.
PERFORM CLOSE_BDC_SESSION.

*&---------------------------------------------------------------------*
*&      Form  BDC_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    Screen Name
*      -->DYNPRO     Screen Number
*----------------------------------------------------------------------*
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.                    "BDC_SCREEN

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       Add the screen field to the table BDCDATA
*----------------------------------------------------------------------*
*      -->FNAM       Field Name
*      -->FVAL       Field Value
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  IF FVAL <> NODATA.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
  ENDIF.

ENDFORM.                    "BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  INSERT_SESSION
*&---------------------------------------------------------------------*
*       Insert the FAGL25 transaction into the BDC session
*----------------------------------------------------------------------*
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      TCODE          = 'FAGL25'
    TABLES
      DYNPROTAB      = BDCDATA
    EXCEPTIONS
      INTERNAL_ERROR = 1
      NOT_OPEN       = 2
      QUEUE_ERROR    = 3
      TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    MESSAGE E013 WITH SY-SUBRC.
  ENDIF.

ENDFORM.                    "INSERT_SESSION

*-----------------------------------------------------------------------
*     FORM OPEN_BDC_GROUP
*-----------------------------------------------------------------------
* - This routine just simply opens up a new batch input session.
*-----------------------------------------------------------------------
FORM OPEN_BDC_SESSION.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      CLIENT            = SY-MANDT
      GROUP             = P_GROUP
      KEEP              = 'X'
      USER              = SY-UNAME
    EXCEPTIONS
      GROUP_INVALID     = 1
      GROUP_IS_LOCKED   = 2
      HOLDDATE_INVALID  = 3
      INTERNAL_ERROR    = 4
      QUEUE_ERROR       = 5
      RUNNING           = 6
      SYSTEM_LOCK_ERROR = 7
      USER_INVALID      = 8.

  IF SY-SUBRC <> 0.
    MESSAGE E004 WITH P_GROUP.
  ENDIF.

ENDFORM.                    "OPEN_BDC_GROUP

*----------------------------------------------------------------------*
*  FORM CLOSE_BDC_GROUP                                                *
*                                                                      *
*  This form will close the BDC session.                               *
*----------------------------------------------------------------------*
FORM CLOSE_BDC_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'.     " close BDC session
ENDFORM.                    "CLOSE_BDC_GROUP
