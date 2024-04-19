REPORT ZFAPI016 NO STANDARD PAGE HEADING LINE-SIZE 120
                LINE-COUNT 65 MESSAGE-ID PP.

************************************************************************
*
*   PROGRAM:    ZFAPI016
*   PROGRAMMER: M De Meester
*   CLIENT:     Union Gas
*   DATE:       November 1999
*
*   The purpose of this program is to create a BDC session that will
*   change a clerk id en masse on the vendor table.  The new vendor
*   clerk is entered in the variant.
*
************************************************************************
* CHANGES
* 2003/03/25 mdemeest ____ Added Account Group as parameter
*-----------------------------------------------------------------------

TABLES:   LFA1,                         "Vendor Master
          LFB1.                         " Vendor master (company code)

*ata:     upds(6)       type n,                         " update counter
*         errs(6)       type n,                         " error counter
*         skip(6)       type n,                         " upd not reqd
*         trx_proc(1)   value 'N',                      " process mode
*         trx_upd(1)    value 'S'.                      " update mode

DATA: BEGIN OF UPDTAB OCCURS 5000,
       NAME1            LIKE LFA1-NAME1,                "Vendor Name
       LIFNR            LIKE LFA1-LIFNR,                "Vendor Number
       BUKRS            LIKE LFB1-BUKRS,                "Company code
       BUSAB            LIKE LFB1-BUSAB,                "Clerk Id
       KONZS            LIKE LFA1-KONZS,                "Group Key
       ktokk            like lfa1-ktokk,                "Account Group
      END OF UPDTAB.

DATA: BEGIN OF BDCDATA OCCURS 100.
      INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

*------------------------  SELECTION SCREEN  ---------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(30) TEXT-001.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
  PARAMETERS: P_RPT AS CHECKBOX    DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
  PARAMETERS: P_BUKRS     LIKE LFB1-BUKRS  OBLIGATORY.    "Company Code
  SELECT-OPTIONS: S_LIFNR FOR LFB1-LIFNR,                 "Vendor Number
                  S_NAME1 FOR LFA1-NAME1,                 "Vendor Name
                  S_KONZS FOR LFA1-KONZS NO INTERVALS,    "Group Key
                  S_KTOKK for lfa1-ktokk no intervals.    "Account Group
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
  PARAMETERS: P_BUSAB     LIKE LFB1-BUSAB  OBLIGATORY.    "Clerk Id

SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

*-----------------------------------------------------------------------
************************************************************************
* set up the printing of report headers with correct company name
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID, 20 TEXT-001,
         54 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
  ULINE.
  WRITE: /1 TEXT-002, 37 TEXT-004, 50 TEXT-006,
                      60 TEXT-009, 70 TEXT-011, 80 text-014.
  WRITE: / TEXT-003 UNDER TEXT-002, TEXT-005 UNDER TEXT-004,
           50 TEXT-007, 54 TEXT-008,
           TEXT-010 UNDER TEXT-009, TEXT-012 UNDER TEXT-011,
           text-015 under text-014.
  ULINE.

*------------------------  AT SELECTION-SCREEN  ------------------------

AT SELECTION-SCREEN.
  IF ( S_LIFNR  IS INITIAL ) AND
     ( S_NAME1  IS INITIAL ).
     MESSAGE E100 WITH 'Must enter Vendor Name and/or Vendor Number'.
  ENDIF.
*-------------------  START of SELECTION  ------------------------------
START-OF-SELECTION.
  PERFORM GET_DATA.
  SORT UPDTAB BY NAME1 LIFNR.
  PERFORM PRINT_DATA.
  IF SY-TFILL > 0.
     IF P_RPT <> 'X'.             "Report Only = X --> Create BDC
        PERFORM CREATE_BDC.
     ENDIF.
  ELSE.
     WRITE: / TEXT-013 UNDER TEXT-001.
  ENDIF.

END-OF-SELECTION.

FORM GET_DATA.
 SELECT * FROM LFA1
   WHERE LIFNR IN S_LIFNR
     AND NAME1 IN S_NAME1
     AND KONZS IN S_KONZS
     AND KTOKK in s_ktokk.                                   "2003/03/25
   SELECT SINGLE * FROM LFB1
     WHERE LIFNR = LFA1-LIFNR
       AND BUKRS = P_BUKRS.
   IF SY-SUBRC = '0'.
      MOVE LFA1-NAME1 TO UPDTAB-NAME1.
      MOVE LFA1-LIFNR TO UPDTAB-LIFNR.
      MOVE LFB1-BUKRS TO UPDTAB-BUKRS.
      MOVE LFB1-BUSAB TO UPDTAB-BUSAB.
      MOVE LFA1-KONZS TO UPDTAB-KONZS.
      move lfa1-ktokk to updtab-ktokk.
      APPEND UPDTAB.
   ENDIF.
 ENDSELECT.
ENDFORM.

*-------------------------  PRINT_DATA  --------------------------------
FORM PRINT_DATA.
  LOOP AT UPDTAB.
     WRITE: / UPDTAB-LIFNR UNDER TEXT-004, UPDTAB-NAME1 UNDER TEXT-002,
              UPDTAB-BUKRS UNDER TEXT-009, UPDTAB-KONZS UNDER TEXT-011,
              UPDTAB-BUSAB UNDER TEXT-007, P_BUSAB      UNDER TEXT-008,
              updtab-ktokk under text-014.
      ENDLOOP.
ENDFORM.

*--------------------------- CREATE_BDC --------------------------------
************************************************************************
*  update the vendor with the new payment method
************************************************************************

FORM CREATE_BDC.
   PERFORM OPEN_BDC_SESSION.
   PERFORM INSERT_RECORDS.
   PERFORM CLOSE_BDC_SESSION.
ENDFORM.

*----------------------- INSERT_RECORDS --------------------------------
FORM INSERT_RECORDS.
  LOOP AT UPDTAB.
   PERFORM SCREEN_HEADER USING 'SAPMF02K'       '0106'     'X'.
   PERFORM SCREEN_FIELD  USING 'RF02K-LIFNR'    UPDTAB-LIFNR.
   PERFORM SCREEN_FIELD  USING 'RF02K-BUKRS'    UPDTAB-BUKRS.
   PERFORM SCREEN_FIELD  USING 'RF02K-D0220'    'X'.

   PERFORM SCREEN_HEADER USING 'SAPMF02K'       '0220'     'X'.
   PERFORM SCREEN_FIELD  USING 'LFB1-BUSAB'     P_BUSAB.
   PERFORM SCREEN_FIELD  USING 'BDC_OKCODE'     '/11'.     " select

   PERFORM INSERT_SESSION.

  REFRESH BDCDATA.
  CLEAR BDCDATA.
 ENDLOOP.

ENDFORM.


************************************************************************
*  Listed below are subroutines to open, close and process BDC data
************************************************************************

FORM OPEN_BDC_SESSION.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = 'ZAP_CLERK'
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
    MESSAGE E699 WITH 'Could not open BDC session'.
  ENDIF.
* tbldat = sy-datum.
* concatenate p_year '1231' into tbudat.
* write tbldat to zbldat dd/mm/yyyy.
* write tbudat to zbudat dd/mm/yyyy.
ENDFORM.

FORM CLOSE_BDC_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN
            QUEUE_ERROR.
  IF SY-SUBRC <> 0.
    MESSAGE E699 WITH 'Could not close the BDC session'.
  ENDIF.
ENDFORM.

FORM INSERT_SESSION.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'FK02'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    MESSAGE E699 WITH 'Could not insert to the BDC session'.
  ENDIF.
ENDFORM.

FORM Screen_Header using program screen indicator.
     clear BDCData.
     BDCDATA-PROGRAM             = PROGRAM.
     BDCDATA-DYNPRO              = SCREEN.
     BDCDATA-DYNBEGIN            = INDICATOR.
     append BDCData.
ENDFORM.

FORM Screen_Field using fnam fval.
     clear BDCData.
     BDCDATA-FNAM                = FNAM.
     BDCDATA-FVAL                = FVAL.
     append BDCData.
ENDFORM.
************************************************************************
*  This is the end my friend.
************************************************************************
