*&---------------------------------------------------------------------*
*& Report  ZFFII050
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFFII050.

TABLES: AUFK,
        CDHDR.

TYPES: BEGIN OF TY_OUTPUT,
        ID(20),
        NAME(40),
        LONG_NAME(50),
        SDATE(10),
        EDATE(10),
        STATUS(1),
        HEIR(30),
        BUKRS(4),
       END OF TY_OUTPUT.

CONSTANTS:
        GC_MODIF_ID_DSP  TYPE CHAR3              "ModifID-Display Only "
                         VALUE 'DSP'.
DATA: GV_FILENAME TYPE STRING,
      GT_OUTPUT TYPE TABLE OF TY_OUTPUT,
      GT_OUTPUT1 TYPE TABLE OF TY_OUTPUT,
      GS_OUTPUT TYPE TY_OUTPUT.

*Data declaration for full extraction
CONSTANTS: GC_TAB TYPE ABAP_CHAR1 VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.

TYPES: BEGIN OF TY_IO,
        PROJ_ID    TYPE TEXT35,
        PROJ_NAME  TYPE AUFTEXT,
        PROJ_LDESC TYPE C,
        PROJ_START TYPE TEXT10,
        PROJ_END   TYPE TEXT10,
        PROJ_STAT  TYPE C,
        PROJ_HIER  TYPE TEXT40,
        PROJ_BUKRS TYPE BUKRS,
       END OF TY_IO.

TYPES: BEGIN OF TY_AUFK,
      AUFNR      TYPE AUFNR,
      AUART      TYPE AUFART,
      KTEXT      TYPE AUFTEXT,
      BUKRS      TYPE BUKRS,
      STAT       TYPE J_STATUS,
      OBJNR      TYPE J_OBJNR,
     END OF TY_AUFK.

DATA: GT_IO   TYPE STANDARD TABLE OF TY_IO,
      GT_AUFK TYPE STANDARD TABLE OF TY_AUFK,
      GT_FILE_DATA TYPE TRUXS_T_TEXT_DATA.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: R_SERVER  RADIOBUTTON GROUP RAD1 DEFAULT 'X'
                                      USER-COMMAND CMD,
            P_SFILE   LIKE        RFPDO-RFBIFILE MODIF ID SRV, "dsp,
            R_LOCAL   RADIOBUTTON GROUP RAD1,
            P_FILE    TYPE        RFPDO-RFBIFILE DEFAULT 'H:\'
                                                  MODIF ID LCL.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS: R_DELTA TYPE C RADIOBUTTON GROUP RD,
            R_FULL  TYPE C RADIOBUTTON GROUP RD.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: S_BUKRS FOR AUFK-BUKRS DEFAULT 'UGL' OBLIGATORY,
                S_AUART FOR AUFK-AUART OBLIGATORY,
                S_DATE FOR  AUFK-ERDAT NO-EXTENSION OBLIGATORY,
                S_OBJECT FOR CDHDR-OBJECTID NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.
  S_DATE-SIGN = 'I'.
  S_DATE-OPTION = 'BT'.
  S_DATE-LOW = SY-DATUM - 7.
  S_DATE-HIGH = SY-DATUM.
  APPEND S_DATE.

  S_AUART-SIGN = 'I'.
  S_AUART-OPTION = 'BT'.
  S_AUART-LOW = '1100'.
  S_AUART-HIGH = '3400'.
  APPEND S_AUART.

  CONCATENATE '/usr/sap/interfaces/' SY-SYSID '/WORKDAY/CO/' INTO P_SFILE.

AT SELECTION-SCREEN OUTPUT.
  PERFORM  TOGGLE_FUNCTIONALITY.

START-OF-SELECTION.

  IF S_DATE-LOW IS INITIAL OR
     S_DATE-HIGH IS INITIAL.
    WRITE: / 'Enter correct date on selection screen..'.
    STOP.
  ENDIF.
  IF S_DATE-LOW > S_DATE-HIGH.
    WRITE: / 'From Date is higher than TO Date..'.
    STOP.
  ENDIF.

  IF R_SERVER IS NOT INITIAL AND
       P_SFILE IS INITIAL.
    WRITE : / 'Enter Application Server File Path Only'.
    STOP.
  ENDIF.
  IF R_LOCAL IS NOT INITIAL AND
     P_FILE IS INITIAL.
    WRITE : / 'Enter PC File Path Only'.
    STOP.
  ENDIF.

  IF R_DELTA EQ 'X'.
    IF R_LOCAL IS NOT INITIAL.
      IF SY-SYSID <> 'P01'.
        CONCATENATE P_FILE 'TEST_SAP_EAST_IO_' SY-DATUM SY-UZEIT '.TXT'
                                   INTO   GV_FILENAME.
      ELSE.
        CONCATENATE P_FILE 'SAP_EAST_IO_' SY-DATUM SY-UZEIT'.TXT'
                                   INTO   GV_FILENAME.
      ENDIF.
    ELSE.
      IF SY-SYSID <> 'P01'.
        CONCATENATE P_SFILE 'TEST_SAP_EAST_IO_' SY-DATUM SY-UZEIT '.TXT'
                                       INTO   GV_FILENAME.
      ELSE.
        CONCATENATE P_SFILE 'SAP_EAST_IO_' SY-DATUM SY-UZEIT'.TXT'
                                       INTO   GV_FILENAME.
      ENDIF.
    ENDIF.
  ELSE.  "Full file
    IF R_LOCAL IS NOT INITIAL.
      IF SY-SYSID <> 'P01'.
        CONCATENATE P_FILE 'TEST_SE_ACTIVE_IO.TXT'
                                   INTO   GV_FILENAME.
      ELSE.
        CONCATENATE P_FILE 'SE_ACTIVE_IO.TXT'
                                   INTO   GV_FILENAME.
      ENDIF.
    ELSE.
      IF SY-SYSID <> 'P01'.
        CONCATENATE P_SFILE 'TEST_SE_ACTIVE_IO.TXT'
                                       INTO   GV_FILENAME.
      ELSE.
        CONCATENATE P_SFILE 'SE_ACTIVE_IO.TXT'
                                       INTO   GV_FILENAME.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: GT_OUTPUT,
         GT_OUTPUT1,
         GS_OUTPUT.
  "Get data

  IF R_DELTA EQ 'X'.
    PERFORM GET_DATA.
    PERFORM DOWNLOAD_DATA.
    IF GT_OUTPUT[] IS INITIAL.
      SKIP 2.
      WRITE: / ' No IO data to download, Empty file has been created.'.
    ENDIF.
  ELSE.  " Full File processing
    PERFORM EXTRACT_FULL_FILE_DATA.
    PERFORM PROCESS_FULL_FILE_DATA.
    PERFORM DELIMIT_DATA USING GT_IO[].
    PERFORM WRITE_FILE.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOGGLE_FUNCTIONALITY .
  LOOP AT SCREEN.
* Set the screen fields to display only
    IF  SCREEN-GROUP1 EQ GC_MODIF_ID_DSP.
      SCREEN-INPUT = 0.
    ENDIF.
    IF R_LOCAL = 'X'.
      IF SCREEN-GROUP1 = 'LCL'.
        SCREEN-INPUT = 1.
      ENDIF.
      IF SCREEN-GROUP1 = 'SRV'.
        SCREEN-INPUT = 0.
      ENDIF.
    ELSE.
      IF SCREEN-GROUP1 = 'LCL'.
        SCREEN-INPUT = 0.
      ENDIF.
      IF SCREEN-GROUP1 = 'SRV'.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.
ENDFORM.                    " TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA .
  CONSTANTS: LC_I0001 TYPE JCDS-STAT VALUE 'I0001',   "CRTD
             LC_I0002 TYPE JCDS-STAT VALUE 'I0002',   "Rel
             LC_I0013 TYPE JCDS-STAT VALUE 'I0013',   "Del
             LC_I0043 TYPE JCDS-STAT VALUE 'I0043',   "Lock
             LC_I0045 TYPE JCDS-STAT VALUE 'I0045',   "TECO
             LC_I0046 TYPE JCDS-STAT VALUE 'I0046',   "Completed
             LC_I0064 TYPE JCDS-STAT VALUE 'I0064',   "Acct Lock
             LC_I0076 TYPE JCDS-STAT VALUE 'I0076'.   "Del
  DATA: LV_OBJECTCLAS TYPE CDHDR-OBJECTCLAS VALUE 'RKAUFTRAG',
        LV_TABNAME TYPE CDPOS-TABNAME VALUE 'COAS',
        LV_FNAME TYPE CDPOS-FNAME VALUE 'KTEXT',
        LV_01 TYPE AUFK-AUART VALUE '01',
        LV_0500 TYPE AUFK-AUART VALUE '0500',
        LV_AUFNR(12),
        LV_TABIX TYPE SY-TABIX,
        LV_FIND TYPE XFELD,
        LT_AUFK TYPE TABLE OF AUFK,
        LS_AUFK TYPE AUFK,
        LT_JCDS TYPE TABLE OF JCDS,
        LS_JCDS TYPE JCDS,
        LT_JEST TYPE TABLE OF JEST,
        LS_JEST TYPE JEST,
        LT_CDHDR TYPE TABLE OF CDHDR,
        LS_CDHDR TYPE CDHDR,
        LT_CDPOS TYPE TABLE OF CDPOS,
        LS_CDPOS TYPE CDPOS.

  SELECT * FROM AUFK INTO TABLE LT_AUFK
    WHERE BUKRS IN S_BUKRS
      "AND autyp = lv_01
      AND AUART IN S_AUART
      AND ( ( ERDAT >= S_DATE-LOW AND ERDAT <= S_DATE-HIGH ) OR
            ( AEDAT >= S_DATE-LOW AND AEDAT <= S_DATE-HIGH ) ).
  IF LT_AUFK[] IS INITIAL.
    WRITE: / 'No data to output for IOs...'.
    "STOP.
    EXIT.
  ENDIF.
  SELECT * FROM JCDS INTO TABLE LT_JCDS
  FOR ALL ENTRIES IN LT_AUFK
  WHERE OBJNR = LT_AUFK-OBJNR
    AND ( STAT = LC_I0002 OR STAT = LC_I0013 OR
          STAT = LC_I0043 OR STAT = LC_I0045 OR
          STAT = LC_I0046 OR STAT = LC_I0064 OR
          STAT = LC_I0076 )   " OR stat = lc_i0001 )
    AND INACT = SPACE
    "AND chind = 'U'
    AND ( UDATE >= S_DATE-LOW AND UDATE <= S_DATE-HIGH ).
  "build IT for object list
  CLEAR S_OBJECT.
  REFRESH S_OBJECT.
  LOOP AT LT_AUFK INTO LS_AUFK.
    S_OBJECT-SIGN = 'I'.
    S_OBJECT-OPTION = 'EQ'.
    S_OBJECT-LOW = LS_AUFK-AUFNR.
    APPEND S_OBJECT.
  ENDLOOP.

  SELECT * FROM CDHDR INTO TABLE LT_CDHDR
    WHERE OBJECTCLAS = LV_OBJECTCLAS
      AND OBJECTID IN S_OBJECT
      AND ( UDATE >= S_DATE-LOW AND UDATE <= S_DATE-HIGH ).
  IF LT_CDHDR[] IS NOT INITIAL.
    SELECT * FROM CDPOS INTO TABLE LT_CDPOS
      FOR ALL ENTRIES IN LT_CDHDR
      WHERE OBJECTCLAS = LT_CDHDR-OBJECTCLAS
        AND OBJECTID = LT_CDHDR-OBJECTID
        AND CHANGENR = LT_CDHDR-CHANGENR
        AND TABNAME  = LV_TABNAME
        AND FNAME    = LV_FNAME.
  ENDIF.
  SELECT * FROM JEST INTO TABLE LT_JEST
    FOR ALL ENTRIES IN LT_AUFK
    WHERE OBJNR = LT_AUFK-OBJNR
    AND ( STAT = LC_I0046 OR STAT = LC_I0013 OR
          STAT = LC_I0064 OR STAT = LC_I0002 OR
          STAT = LC_I0045 OR STAT = LC_I0076 OR
          STAT = LC_I0043 OR STAT = LC_I0001 )
    AND INACT = SPACE.
  "AND chgnr = '001'.
  SORT LT_JCDS BY OBJNR UDATE UTIME.
  SORT LT_JEST BY OBJNR.
  LOOP AT LT_AUFK INTO LS_AUFK.
    CLEAR: GS_OUTPUT,
           LS_JCDS,
           LS_CDHDR,
           LS_CDPOS,
           LS_JEST,
           LV_FIND,
           LV_TABIX.
    "if I0001 status (CRTD) then do not output
    READ TABLE LT_JEST INTO LS_JEST WITH KEY OBJNR = LS_AUFK-OBJNR
                                             STAT  = LC_I0001.
    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.
    LV_AUFNR = LS_AUFK-AUFNR.
    SHIFT LV_AUFNR LEFT DELETING LEADING '0'.
    IF LS_AUFK-AUART = LV_0500.  "Statistical order
      CONCATENATE 'SO.E.' LV_AUFNR INTO GS_OUTPUT-ID
                                    SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'IO.E.' LV_AUFNR INTO GS_OUTPUT-ID
                                    SEPARATED BY SPACE.
    ENDIF.
*    REPLACE ALL OCCURRENCES OF '"' in ls_aufk-ktext WITH space.
    IF LS_AUFK-KTEXT IS INITIAL.
      GS_OUTPUT-NAME = 'No Description'.
    ELSE.
      GS_OUTPUT-NAME = LS_AUFK-KTEXT.
    ENDIF.
    GS_OUTPUT-LONG_NAME = SPACE.
    GS_OUTPUT-SDATE = '1900/01/01'.
    GS_OUTPUT-EDATE = '9999/12/31'.
    GS_OUTPUT-HEIR = 'Spectra East Projects'.
    GS_OUTPUT-BUKRS = LS_AUFK-BUKRS.
    "Changed data
    IF LS_AUFK-AEDAT IS NOT INITIAL.
      IF LS_AUFK-AEDAT <> LS_AUFK-ERDAT.   "created & change dates are different.
        READ TABLE LT_CDHDR INTO LS_CDHDR WITH KEY OBJECTID = LS_AUFK-AUFNR.
        IF SY-SUBRC = 0.
          READ TABLE LT_CDPOS INTO LS_CDPOS WITH KEY OBJECTID = LS_CDHDR-OBJECTID.
          "changenr = ls_cdhdr-changenr.
          IF SY-SUBRC = 0.
            "gs_output-Proj_name = ls_cdpos-value_new.
            LV_FIND = 'X'.
          ENDIF.
        ENDIF.
        IF LV_FIND IS INITIAL.
          CLEAR: LV_TABIX, LV_FIND.
          "check if any change to status
          READ TABLE LT_JCDS WITH KEY OBJNR = LS_AUFK-OBJNR
                                     TRANSPORTING NO FIELDS.
          LV_TABIX = SY-TABIX.
          LOOP AT LT_JCDS INTO LS_JCDS FROM LV_TABIX.
            IF LS_JCDS-OBJNR <> LS_AUFK-OBJNR.
              EXIT.
            ENDIF.
            IF LS_JCDS-STAT = LC_I0013 OR LS_JCDS-STAT = LC_I0076 OR
               LS_JCDS-STAT = LC_I0043 OR LS_JCDS-STAT = LC_I0046 OR
               LS_JCDS-STAT = LC_I0064.     " OR ls_jcds-stat = lc_i0001.
              "gs_output-status = 'I'.
              LV_FIND = 'X'.
            ENDIF.
            IF LS_JCDS-STAT = LC_I0002 OR LS_JCDS-STAT = LC_I0045.
              "gs_output-status = 'A'.
              LV_FIND = 'X'.
            ENDIF.
          ENDLOOP.
        ENDIF.
        "if change record does not have name change or status change then do not output.
        CHECK LV_FIND <> SPACE.
      ENDIF.
    ENDIF.
    "--------get status
    "if inactive record is there then no need to check active status.
    CLEAR: LV_TABIX, LV_FIND.
    READ TABLE LT_JEST WITH KEY OBJNR = LS_AUFK-OBJNR
                                TRANSPORTING NO FIELDS.
    LV_TABIX = SY-TABIX.
    LOOP AT LT_JEST INTO LS_JEST FROM LV_TABIX.
      IF LS_JEST-OBJNR <> LS_AUFK-OBJNR.
        EXIT.
      ENDIF.
      CHECK LS_JEST-STAT = LC_I0046 OR LS_JEST-STAT = LC_I0013 OR
            LS_JEST-STAT = LC_I0064 OR LS_JEST-STAT = LC_I0076 OR
            LS_JEST-STAT = LC_I0043.
      GS_OUTPUT-STATUS = 'I'.
      LV_FIND = 'X'.
      EXIT.
    ENDLOOP.
    IF LV_FIND IS INITIAL.
      CLEAR: LV_TABIX, LV_FIND.
      READ TABLE LT_JEST WITH KEY OBJNR = LS_AUFK-OBJNR
                                  TRANSPORTING NO FIELDS.
      LV_TABIX = SY-TABIX.
      LOOP AT LT_JEST INTO LS_JEST FROM LV_TABIX.
        IF LS_JEST-OBJNR <> LS_AUFK-OBJNR.
          EXIT.
        ENDIF.
        "status RElL / TECO
        CHECK LS_JEST-STAT = LC_I0002 OR LS_JEST-STAT = LC_I0045.
        GS_OUTPUT-STATUS = 'A'.
        LV_FIND = 'X'.
        EXIT.
      ENDLOOP.
    ENDIF.
    "---------------
    IF LV_FIND IS NOT INITIAL.
      APPEND GS_OUTPUT TO GT_OUTPUT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DOWNLOAD_DATA .
  IF R_SERVER IS NOT INITIAL.
    PERFORM DOWNLOAD_SERVER.
  ELSE.
    PERFORM DOWNLOAD_PC.
  ENDIF.
ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DOWNLOAD_SERVER .

  DATA: LV_TABFIELD(1) TYPE X VALUE '09',
           LV_STRING TYPE STRING,
           LV_CRLF TYPE C.

  LV_CRLF = CL_ABAP_CHAR_UTILITIES=>CR_LF.

  OPEN DATASET GV_FILENAME FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC <> 0.
    WRITE:/ 'Unable to open file for output.'.
  ELSE.

    LOOP AT GT_OUTPUT INTO GS_OUTPUT.
      CONCATENATE GS_OUTPUT-ID
                  GS_OUTPUT-NAME
                  GS_OUTPUT-LONG_NAME
                  GS_OUTPUT-SDATE
                  GS_OUTPUT-EDATE
                  GS_OUTPUT-STATUS
                  GS_OUTPUT-HEIR
                  GS_OUTPUT-BUKRS
      INTO LV_STRING
       SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
      TRANSFER LV_STRING TO GV_FILENAME.
    ENDLOOP.
    CLOSE DATASET GV_FILENAME.
    WRITE: / 'File is downloaded', GV_FILENAME.
  ENDIF.

ENDFORM.                    " DOWNLOAD_SERVER
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DOWNLOAD_PC .
  DATA: LV_FILENAME TYPE STRING.

  MOVE GV_FILENAME TO LV_FILENAME.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
    EXPORTING
      FILENAME                  = LV_FILENAME
      FILETYPE                  = 'ASC'
      WRITE_FIELD_SEPARATOR     = 'X'
      TRUNC_TRAILING_BLANKS_EOL = SPACE "'X'
    CHANGING
      DATA_TAB                  = GT_OUTPUT
    EXCEPTIONS
      FILE_WRITE_ERROR          = 1
      NO_BATCH                  = 2
      GUI_REFUSE_FILETRANSFER   = 3
      INVALID_TYPE              = 4
      NO_AUTHORITY              = 5
      UNKNOWN_ERROR             = 6
      HEADER_NOT_ALLOWED        = 7
      SEPARATOR_NOT_ALLOWED     = 8
      FILESIZE_NOT_ALLOWED      = 9
      HEADER_TOO_LONG           = 10
      DP_ERROR_CREATE           = 11
      DP_ERROR_SEND             = 12
      DP_ERROR_WRITE            = 13
      UNKNOWN_DP_ERROR          = 14
      ACCESS_DENIED             = 15
      DP_OUT_OF_MEMORY          = 16
      DISK_FULL                 = 17
      DP_TIMEOUT                = 18
      FILE_NOT_FOUND            = 19
      DATAPROVIDER_EXCEPTION    = 20
      CONTROL_FLUSH_ERROR       = 21
      NOT_SUPPORTED_BY_GUI      = 22
      ERROR_NO_GUI              = 23
      OTHERS                    = 24.
  IF SY-SUBRC <> 0.
    WRITE: / 'Error with downloading file at PC ', SY-SUBRC.
  ELSE.
    WRITE: / 'Successfully created at ', GV_FILENAME.
  ENDIF.
ENDFORM.                    " DOWNLOAD_PC

*&---------------------------------------------------------------------*
*&      Form  EXTRACT_FULL_FILE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXTRACT_FULL_FILE_DATA .

  TYPES: BEGIN OF TY_INACTIVE,
           OBJNR TYPE JEST-OBJNR,
           STAT  TYPE JEST-STAT,
         END OF TY_INACTIVE.

  DATA: LT_INACTIVES TYPE STANDARD TABLE OF TY_INACTIVE,
        LV_TABIX     TYPE SYTABIX.

  RANGES: R_ISTATUS FOR JEST-STAT.  "Inactive status

  FIELD-SYMBOLS: <ACTIVE>   LIKE LINE OF GT_AUFK,
                 <INACTIVE> LIKE LINE OF LT_INACTIVES.

* Fill range table with inactive status
  R_ISTATUS-SIGN   = 'I'.
  R_ISTATUS-OPTION = 'EQ'.
  R_ISTATUS-LOW    = 'I0013'.
  APPEND R_ISTATUS.

  R_ISTATUS-LOW    = 'I0043'.
  APPEND R_ISTATUS.

  R_ISTATUS-LOW    = 'I0046'.
  APPEND R_ISTATUS.

  R_ISTATUS-LOW    = 'I0064'.
  APPEND R_ISTATUS.

  R_ISTATUS-LOW    = 'I0076'.
  APPEND R_ISTATUS.

  REFRESH: GT_AUFK.
  SELECT AUFK~AUFNR AUFK~AUART AUFK~KTEXT
         AUFK~BUKRS JEST~STAT JEST~OBJNR
           FROM AUFK
           INNER JOIN JEST ON JEST~OBJNR EQ AUFK~OBJNR
           INTO TABLE GT_AUFK
           WHERE BUKRS IN S_BUKRS
           AND   AUART IN S_AUART
           AND  ( JEST~STAT EQ 'I0002' OR JEST~STAT EQ 'I0045' )
           AND  JEST~INACT EQ ''.
  IF SY-SUBRC EQ 0.
    SELECT OBJNR STAT
             FROM JEST
             INTO TABLE LT_INACTIVES
             FOR ALL ENTRIES IN GT_AUFK
             WHERE OBJNR EQ GT_AUFK-OBJNR
             AND   STAT  IN R_ISTATUS
             AND   INACT EQ ''.
    IF SY-SUBRC EQ 0.
*    Record may have both inactive and active status so eliminate
*    records with both status
      SORT LT_INACTIVES.
      LOOP AT GT_AUFK ASSIGNING <ACTIVE>.
        LV_TABIX = SY-TABIX.
        READ TABLE LT_INACTIVES ASSIGNING <INACTIVE>
                                WITH KEY OBJNR = <ACTIVE>-OBJNR
                                BINARY SEARCH.
        IF SY-SUBRC EQ 0.
          DELETE GT_AUFK INDEX LV_TABIX.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " EXTRACT_FULL_FILE_DATA

*&---------------------------------------------------------------------*
*&      Form  PROCESS_FULL_FILE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_FULL_FILE_DATA .

  FIELD-SYMBOLS: <AUFK> LIKE LINE OF GT_AUFK,
                 <IO>   LIKE LINE OF GT_IO.

  LOOP AT GT_AUFK ASSIGNING <AUFK>.
    APPEND INITIAL LINE TO GT_IO ASSIGNING <IO>.
    SHIFT <AUFK>-AUFNR LEFT DELETING LEADING '0'.
    IF <AUFK>-AUART EQ '0500'.
      CONCATENATE 'SO.E.' <AUFK>-AUFNR
                  INTO <IO>-PROJ_ID SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'IO.E.' <AUFK>-AUFNR
                  INTO <IO>-PROJ_ID SEPARATED BY SPACE.
    ENDIF.

    IF <AUFK>-KTEXT NE ''.
      <IO>-PROJ_NAME = <AUFK>-KTEXT.
    ELSE.
      <IO>-PROJ_NAME = 'No Description'.
    ENDIF.

    <IO>-PROJ_STAT  = 'A'.
    <IO>-PROJ_START = '1900/01/01'.
    <IO>-PROJ_END   = '9999/12/31'.
    <IO>-PROJ_HIER  = 'Spectra East Projects'.
    <IO>-PROJ_BUKRS = <AUFK>-BUKRS.
  ENDLOOP.

ENDFORM.                    " PROCESS_FULL_FILE_DATA

*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATA_BY_DELIMITER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELIMIT_DATA USING PT_SAP_DATA TYPE STANDARD TABLE.

  REFRESH: GT_FILE_DATA.

  CHECK  PT_SAP_DATA[] IS NOT INITIAL.

  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
    EXPORTING
      I_FIELD_SEPERATOR    = GC_TAB
    TABLES
      I_TAB_SAP_DATA       = PT_SAP_DATA
    CHANGING
      I_TAB_CONVERTED_DATA = GT_FILE_DATA
    EXCEPTIONS
      CONVERSION_FAILED    = 1
      OTHERS               = 2.

  IF SY-SUBRC <> 0.
    WRITE: / 'Data delimit error'.
  ENDIF.

ENDFORM.                    " CONVERT_DATA_BY_DELIMITER
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_FILE .

  IF R_SERVER IS NOT INITIAL.
    PERFORM WRITE_SERVER_FILE.
  ELSE.
    PERFORM WRITE_PC_FILE.
  ENDIF.

ENDFORM.                    " WRITE_FILE
*&---------------------------------------------------------------------*
*&      Form  WRITE_SERVER_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_SERVER_FILE .

  FIELD-SYMBOLS: <FILE_DATA> LIKE LINE OF GT_FILE_DATA.

  CHECK GT_FILE_DATA[] IS NOT INITIAL.
  OPEN DATASET GV_FILENAME FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF SY-SUBRC <> 0.
    WRITE:/ 'Unable to open file for output.'.
  ELSE.
    LOOP AT GT_FILE_DATA ASSIGNING <FILE_DATA>.
      TRANSFER <FILE_DATA> TO GV_FILENAME.
    ENDLOOP.
    CLOSE DATASET GV_FILENAME.
    WRITE: / 'File is downloaded', GV_FILENAME.
  ENDIF.

ENDFORM.                    " WRITE_SERVER_FILE
*&---------------------------------------------------------------------*
*&      Form  WRITE_PC_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_PC_FILE .

  DATA: LV_FILENAME TYPE STRING.

  CHECK GT_FILE_DATA[] IS NOT INITIAL.

  MOVE GV_FILENAME TO LV_FILENAME.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
    EXPORTING
      FILENAME                  = LV_FILENAME
      FILETYPE                  = 'ASC'
      WRITE_FIELD_SEPARATOR     = 'X'
      TRUNC_TRAILING_BLANKS_EOL = SPACE "'X'
    CHANGING
      DATA_TAB                  = GT_FILE_DATA
    EXCEPTIONS
      FILE_WRITE_ERROR          = 1
      NO_BATCH                  = 2
      GUI_REFUSE_FILETRANSFER   = 3
      INVALID_TYPE              = 4
      NO_AUTHORITY              = 5
      UNKNOWN_ERROR             = 6
      HEADER_NOT_ALLOWED        = 7
      SEPARATOR_NOT_ALLOWED     = 8
      FILESIZE_NOT_ALLOWED      = 9
      HEADER_TOO_LONG           = 10
      DP_ERROR_CREATE           = 11
      DP_ERROR_SEND             = 12
      DP_ERROR_WRITE            = 13
      UNKNOWN_DP_ERROR          = 14
      ACCESS_DENIED             = 15
      DP_OUT_OF_MEMORY          = 16
      DISK_FULL                 = 17
      DP_TIMEOUT                = 18
      FILE_NOT_FOUND            = 19
      DATAPROVIDER_EXCEPTION    = 20
      CONTROL_FLUSH_ERROR       = 21
      NOT_SUPPORTED_BY_GUI      = 22
      ERROR_NO_GUI              = 23
      OTHERS                    = 24.
  IF SY-SUBRC <> 0.
    WRITE: / 'Error with downloading file at PC ', SY-SUBRC.
  ELSE.
    WRITE: / 'Successfully created at ', GV_FILENAME.
  ENDIF.

ENDFORM.                    " WRITE_PC_FILE
