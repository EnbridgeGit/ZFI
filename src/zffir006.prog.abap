REPORT ZFFIR006 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170.
*
************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        February, 2003                                         *
*  Description:                                                        *
*     - The purpose of this program is to produce GL Account List.     *
************************************************************************
* CHANGES:                                                             *
* Date:    Issue:    By:     Remarks                                   *
*                                                                      *
*                                                                      *
************************************************************************

TABLES: SKA1,              "G/L Account Master (Chart of Accounts)
        SKB1,              "G/L Account Master (Company code)
        SKAT.              "G/L Account Master Record (Chart of Accounts
DATA:
  BEGIN OF ITAB  OCCURS 0,
   SAKNR     LIKE SKA1-SAKNR,         "G/L Account
   TXT50     LIKE SKAT-TXT50,         "G/L Account Long Text
   TXT20     LIKE SKAT-TXT20,         "G/L Account Short Text
   XSPEB1     LIKE SKA1-XSPEB,        "Acct.Blkd for Post -Chart of Acct
   XLOEV      LIKE SKA1-XLOEV,        "Acct.Mrkd for Delet-Chart of Acct
   XSPEB2     LIKE SKB1-XSPEB,        "Acct.Blkd for Post -Company Code
   XLOEB      LIKE SKB1-XLOEB,        "Acct.Mrkd for Delet-Company code
  END OF ITAB.

DATA:
  BEGIN OF REPTAB  OCCURS 0,
   SAKNR      LIKE SKA1-SAKNR,        "G/L Account
   XSPEB1     LIKE SKA1-XSPEB,        "Acct.Blkd for Post -Chart of Acct
   XSPEB2     LIKE SKB1-XSPEB,        "Acct.Blkd for Post -Company Code
   XLOEV      LIKE SKA1-XLOEV,        "Acct.Mrkd for Delet-Chart of Acct
   XLOEB      LIKE SKB1-XLOEB,        "Acct.Mrkd for Delet-Company code
   TXT50      LIKE SKAT-TXT50,        "G/L Account Long Text
   TXT20      LIKE SKAT-TXT20,        "G/L Account Short Text
   ACTXT(80)  TYPE C,                 "Accounting note
  END OF REPTAB.

DATA: BEGIN OF PROT_HEADER OCCURS 1,
        SPALTENNAME(20) TYPE C,
        DDIC_TABLE(5) TYPE C,
        DDIC_FIELD(5) TYPE C,
        KEY TYPE C,
      END OF PROT_HEADER.

DATA:  BEGIN OF WHEAD.
      INCLUDE STRUCTURE THEAD.
DATA:  END OF WHEAD.

DATA:  BEGIN OF WLINE OCCURS 0.
      INCLUDE STRUCTURE TLINE.
DATA:  END OF WLINE.

DATA  ERRORTAB    LIKE HRERROR    OCCURS 0 WITH HEADER LINE.
DATA: RETCODE LIKE SY-SUBRC.


*DATA: WORK_OPTION(11)   TYPE C VALUE 'START_EXCEL'.
DATA: WORK_HD01(30)     TYPE C,
      WORK_HD02(30)     TYPE C,
      WORK_HD03(30)     TYPE C,
      WORK_TTL LIKE SY-TITLE VALUE ' Account Listing With Description'.

************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  S_SAKNR FOR SKA1-SAKNR.
PARAMETERS:      P_KTOPL LIKE SKA1-KTOPL OBLIGATORY,
                 P_BUKRS LIKE SKB1-BUKRS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_ALL       RADIOBUTTON GROUP GRP1,    "All Accounts
            P_NO_BLK    RADIOBUTTON GROUP GRP1,    "Not blocked
            P_NO_DEL    RADIOBUTTON GROUP GRP1,    "Not delete mark
            P_NO_BND    RADIOBUTTON GROUP GRP1,    "Not block/delete
            P_BLOKED     RADIOBUTTON GROUP GRP1,   "Blocked
            P_MARKED    RADIOBUTTON GROUP GRP1,    "Deletion Marked
            P_BNMARK    RADIOBUTTON GROUP GRP1.    "Blocked & Marked
SELECTION-SCREEN END OF BLOCK BOX2.

*SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
*PARAMETERS:     P_EXCL RADIOBUTTON  GROUP GRP2,   "EXCEL SHEET
*                P_RPRT RADIOBUTTON  GROUP GRP2.   "PRINT REPORT
*SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.
************************************************************************
START-OF-SELECTION.
************************************************************************
SELECT SKA1~SAKNR SKAT~TXT50 SKAT~TXT20 SKA1~XSPEB SKA1~XLOEV
       SKB1~XSPEB SKB1~XLOEB
  INTO TABLE ITAB
  FROM ( ( SKA1 INNER JOIN SKB1
             ON SKA1~SAKNR = SKB1~SAKNR )
                INNER JOIN SKAT
             ON SKA1~KTOPL = SKAT~KTOPL
            AND SKA1~SAKNR = SKAT~SAKNR )
 WHERE SKA1~KTOPL =  P_KTOPL
   AND SKA1~SAKNR IN S_SAKNR
   AND SKB1~BUKRS =  P_BUKRS
   AND SKAT~SPRAS = 'EN'.

 IF P_NO_BLK = 'X'.                         "ALL BUT NOT BLOCKED
    DELETE ITAB WHERE  XSPEB1 = 'X'
                   OR  XSPEB2 = 'X'.
 ELSEIF P_NO_DEL = 'X'.                     "ALL BUT NOT MARK FOR DELETE
    DELETE ITAB WHERE  XLOEV  = 'X'
                   OR  XLOEB  = 'X'.
 ELSEIF P_NO_BND = 'X'.                     "ALL BUT NOT BLOCK/DELETE
    DELETE ITAB WHERE  XSPEB1 = 'X'
                   OR  XSPEB2 = 'X'.
    DELETE ITAB WHERE  XLOEV  = 'X'
                   OR  XLOEB  = 'X'.
 ELSEIF P_BLOKED = 'X'.                     "BLOCKED ONLY
    DELETE ITAB WHERE  XSPEB1 <> 'X'
                  AND  XSPEB2 <> 'X'.
 ELSEIF P_MARKED = 'X'.                     "MARKED FOR DELETE ONLY
    DELETE ITAB WHERE  XLOEV  <> 'X'
                  AND  XLOEB  <> 'X'.
 ELSEIF P_BNMARK = 'X'.                     "BLOCKED & MARKED FOR DELETE
    DELETE ITAB WHERE  XSPEB1 <> 'X'
                  AND  XSPEB2 <> 'X'
                  AND  XLOEV  <> 'X'
                  AND  XLOEB  <> 'X'.
 ENDIF.

 PERFORM BUILD_REPTAB.

IF NOT REPTAB[] IS INITIAL.
*   IF P_EXCL <> 'X'.
*      CLEAR WORK_OPTION.
      CONCATENATE 'Chart:' P_KTOPL INTO WORK_HD01 SEPARATED BY SPACE.
      CONCATENATE 'Company:' P_BUKRS INTO WORK_HD02 SEPARATED BY SPACE.
      CONCATENATE SY-DATUM+0(4) SY-DATUM+4(2) SY-DATUM+6(2)
                  INTO WORK_HD03 SEPARATED BY '/'.
      CONCATENATE 'As Of:' WORK_HD03 INTO WORK_HD03 SEPARATED BY SPACE.
*   ENDIF.
   PERFORM REPORT_PRINTING.
ENDIF.

*--------------------------BUILD_REPTAB --------------------------------
* Routine used to build the report table                               *
*-----------------------------------------------------------------------
FORM BUILD_REPTAB.

 LOOP AT ITAB.
      MOVE-CORRESPONDING ITAB TO REPTAB.
      PERFORM GET_ACCOUNTING_NOTE.
 ENDLOOP.

ENDFORM.
*--------------------------GET_ACCOUNTING_NOTE--------------------------
* Routine used to get accounting note info                             *
*-----------------------------------------------------------------------
FORM GET_ACCOUNTING_NOTE.

 DATA: WORK_TDNAME LIKE THEAD-TDNAME.

 CONCATENATE ITAB-SAKNR P_KTOPL INTO WORK_TDNAME.

CALL FUNCTION 'READ_TEXT'
     EXPORTING
          CLIENT                  = SY-MANDT
          ID                      = '0002'      "Accounting Note
          LANGUAGE                = 'E'
          NAME                    = WORK_TDNAME
          OBJECT                  = 'SKA1'
          ARCHIVE_HANDLE          = 0
     IMPORTING
          HEADER                  = WHEAD
     TABLES
          LINES                   = WLINE
     EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8
          .
IF SY-SUBRC <> 0.
   CLEAR REPTAB-ACTXT.
   APPEND REPTAB.
   CLEAR  REPTAB.
ELSE.
   LOOP AT WLINE.
        MOVE WLINE-TDLINE TO REPTAB-ACTXT.
        APPEND REPTAB.
        CLEAR  REPTAB.
*        EXIT.
   ENDLOOP.
ENDIF.

ENDFORM.
*--------------------------REPORT_PRINTING.    -------------------------
* Routine used to create the report or excel sheet                     *
*-----------------------------------------------------------------------
FORM REPORT_PRINTING.
PERFORM PROT_HEADER.

    CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
          EXPORTING
            BASIC_LIST_TITLE        = WORK_TTL
            HEAD_LINE1              = WORK_HD01
            HEAD_LINE2              = WORK_HD02
            HEAD_LINE3              = WORK_HD03
            FILE_NAME               = SY-CPROG
*            ADDITIONAL_OPTIONS      = WORK_OPTION
          IMPORTING
               RETURN_CODE          = RETCODE
          TABLES
               DATA_TAB             = REPTAB
               FIELDNAME_TAB        = PROT_HEADER
               ERROR_TAB            = ERRORTAB
          EXCEPTIONS
               DOWNLOAD_PROBLEM     = 1
               NO_DATA_TAB_ENTRIES  = 2
               TABLE_MISMATCH       = 3
               PRINT_PROBLEMS       = 4
               OTHERS               = 5.
  IF SY-SUBRC NE 0.
    WRITE: /1 'TABLE DOWNLOAD UNSUCCESSFULL - REASON = ', SY-SUBRC.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM PROT_HEADER                                              *
*---------------------------------------------------------------------*
FORM PROT_HEADER.
  MOVE TEXT-CL1 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL2 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL3 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL4 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL5 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL6 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL7 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
  MOVE TEXT-CL8 TO PROT_HEADER-SPALTENNAME.
  APPEND PROT_HEADER.
ENDFORM.
