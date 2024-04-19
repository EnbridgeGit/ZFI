REPORT ZFAPI010 NO STANDARD PAGE HEADING LINE-SIZE 80
                LINE-COUNT 65 MESSAGE-ID PP.

************************************************************************
*
*   PROGRAM:    ZFAPI010
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas
*   DATE:       February 1998.
*
*   The purpose of this program is update the vendor payment method to
*   a '1' for all Centra vendors.
*   This is a one time program.
*
************************************************************************

TABLES:   LFB1.                         " Vendor master (company code)

DATA:     UPDS(6)       TYPE N,                         " update counter
          ERRS(6)       TYPE N,                         " error counter
          SKIP(6)       TYPE N,                         " upd not reqd
          TRX_PROC(1)   VALUE 'N',                      " process mode
          TRX_UPD(1)    VALUE 'S'.                      " update mode

DATA: BEGIN OF UPDTAB OCCURS 1000,
          LIFNR         LIKE LFB1-LIFNR,                " vendor number
          ZWELS         LIKE LFB1-ZWELS.                " payment method
DATA: END OF UPDTAB.

DATA: BEGIN OF BDCDATA OCCURS 100.
      INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(30) TEXT-001.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(27) TEXT-006.
      PARAMETERS: P_BUKRS     LIKE LFB1-BUKRS
                              OBLIGATORY DEFAULT 'CGO'.
   SELECTION-SCREEN END OF LINE.
   SELECT-OPTIONS: S_LIFNR     FOR LFB1-LIFNR.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(27) TEXT-015.
      PARAMETERS: P_ZWELS     LIKE LFB1-ZWELS
                              OBLIGATORY DEFAULT '1'.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* set up the printing of report headers with correct company name
TOP-OF-PAGE.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 TEXT-001
            , 066 TEXT-011, SY-MANDT
            , 080 SY-VLINE.
     WRITE:   /01 SY-VLINE
            , 003 TEXT-002, SY-DATUM
            , 065 TEXT-012, SY-PAGNO
            , 080 SY-VLINE.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 010 TEXT-003
            , 025 TEXT-013
            , 040 TEXT-014
            , 055 TEXT-016
            , 080 SY-VLINE.
     ULINE.

* extract required data
START-OF-SELECTION.

  SELECT * FROM LFB1
  WHERE BUKRS EQ P_BUKRS
  AND   LIFNR IN S_LIFNR.
    UPDTAB-LIFNR = LFB1-LIFNR.
    UPDTAB-ZWELS = LFB1-ZWELS.
    APPEND UPDTAB.
  ENDSELECT.

  SORT UPDTAB BY LIFNR.

  LOOP AT UPDTAB.
    IF LFB1-ZWELS <> P_ZWELS.
      PERFORM UPDATE_VENDOR.
    ELSE.
      WRITE: /01 SY-VLINE
           , 010 UPDTAB-LIFNR
           , 025 UPDTAB-ZWELS
           , 080 SY-VLINE.
      SKIP = SKIP + 1.
    ENDIF.
  ENDLOOP.

  ULINE.
  WRITE: /01 SY-VLINE
       , 010 TEXT-004
       , 040 UPDS
       , 080 SY-VLINE.
  WRITE: /01 SY-VLINE
       , 010 TEXT-005
       , 040 ERRS
       , 080 SY-VLINE.
  WRITE: /01 SY-VLINE
       , 010 TEXT-010
       , 040 SKIP
       , 080 SY-VLINE.
  ULINE.

END-OF-SELECTION.

************************************************************************
*  update the vendor with the new payment method
************************************************************************

FORM UPDATE_VENDOR.

   PERFORM SCREEN_HEADER USING 'SAPMF02K'       '0106'     'X'.
   PERFORM SCREEN_FIELD  USING 'RF02K-LIFNR'    UPDTAB-LIFNR.
   PERFORM SCREEN_FIELD  USING 'RF02K-BUKRS'    P_BUKRS.
   PERFORM SCREEN_FIELD  USING 'RF02K-D0215'    'X'.

   PERFORM SCREEN_HEADER USING 'SAPMF02K'       '0215'     'X'.
   PERFORM SCREEN_FIELD  USING 'LFB1-ZWELS'     P_ZWELS.
   PERFORM SCREEN_FIELD  USING 'BDC_OKCODE'     '/11'.     " select

   CALL TRANSACTION 'FK02' USING  BDCDATA         " data source
                           MODE   TRX_PROC        " Process (N, E, or A)
                           UPDATE TRX_UPD.        " update mode (S or N)
  IF SY-SUBRC = 0.
    WRITE: /01 SY-VLINE
         , 010 UPDTAB-LIFNR
         , 025 UPDTAB-ZWELS
         , 040 P_ZWELS
         , 055 TEXT-008
         , 080 SY-VLINE.
    UPDS = UPDS + 1.
  ELSE.
    FORMAT INTENSIFIED OFF.
    WRITE: /01 SY-VLINE
         , 010 UPDTAB-LIFNR
         , 025 UPDTAB-ZWELS
         , 055 TEXT-009
         , 080 SY-VLINE.
    FORMAT INTENSIFIED ON.
    ERRS = ERRS + 1.
  ENDIF.
  REFRESH BDCDATA.
  CLEAR BDCDATA.

ENDFORM.

FORM SCREEN_HEADER USING PROGRAM SCREEN INDICATOR.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = SCREEN.
  BDCDATA-DYNBEGIN = INDICATOR.
  APPEND BDCDATA.
ENDFORM.

FORM SCREEN_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.

************************************************************************
*  This is the end my freind.
************************************************************************
