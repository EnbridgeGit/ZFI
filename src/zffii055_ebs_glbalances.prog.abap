*&---------------------------------------------------------------------*
*& Report  ZFFII055_EBS_GLBALANCES
*&
*&---------------------------------------------------------------------*
*Client:     COG.
**
*Date:       19 FEBRUARY 2021
**
*Author:    Sweta Srivastava
**
*Program Description: Extraction Report # To filter COG Related trial
*balance and update Oracle-EBS general Ledger using file
************************************************************************
*----------------------------  CHANGE LOG  ----------------------------*
* Date      Programmer TR#         Description                         *
*&-------   ---------- ----------  ------------------------------------*
*03/21/2022 NAGIRIR    D30K932101 1.Primary key removed to fld SELDATE *
*                                 of table ZFIT_EGDTIMESTMP, add check *
*                                 to INSERT statement so that timestamp*
*                                 would get created in table only when *
*                                 file created in AL11 folder          *
*                                2.Comment/Change code so that NO Empty*
*                                file would get created when NO DATA   *
*                                3.Overcome UTC Time Delay of 4Hours   *
*04/11/2022 NAGIRIR   D30K932157 1.Touch file logic addition           *
*                                2.Remove block"Selection screen" when *
*                                  "Touch File" Radio buttion selected *
*                                3.Touch File name/path is non-editable*
*                                4.Create .tch only when .JNN exists   *
*                                5.NO Duplicate .JNN/.tch file on AL11 *
*                                6.Add Timestamp logic to Display also *
*05/02/2022 NAGIRIR D30K932165   1.Remove Timestamp logic Display Func *
*04/19/2023 APPUKUTA  D30K932428   Remove 4 Hour Timelag               *
*05/18/2023 Ajith A   D30K932472   Re-instate Timestamp logic          *
*05/30/2023 Ajith A   D30K932426   Convert to Local Timezone           *
************************************************************************

REPORT  zffii055_ebs_glbalances.

CONSTANTS c_prog TYPE char40 VALUE 'ZFFII055_EBS_GLBALANCES'.
TYPES: BEGIN OF gty_fagl,
        year  TYPE gjahr,
        docnr TYPE belnr_d,
        rldnr TYPE fagl_rldnr,
        bukrs TYPE bukrs,
        awtyp TYPE awtyp,
        racct TYPE racct,
        rcntr TYPE kostl,
        rassc TYPE rassc,
        hsl   TYPE vlcur12,
        monat TYPE monat,
       END OF gty_fagl.

TYPES: BEGIN OF gty_seg,
lob          TYPE zefslob_lobid,"BUKRS,
lob_name     TYPE zefslob_lobtxt,
contseg      TYPE zefscs_cs,"CHAR3,
contseg_name TYPE zefscs_cs_txt, "CHAR10,
cost_center  TYPE zefscc_ccid,"CHAR5,
costc_name   TYPE zefscc_cctxt,"CHAR20,
gl_acct      TYPE zefs_ac,"CHAR5,
sub_acct     TYPE zefs_subid,"CHAR6,
int_lob      TYPE zefsilob_ilobid,
int_lobname  TYPE zefsilob_ilobtxt,
l_amount     TYPE vlcur12,
END OF gty_seg.
TYPES: BEGIN OF gty_time_frame,
  runnr     TYPE lfdnr6,
  date_from TYPE d,
  time_from TYPE t,
  stmp_from TYPE timestamp,
  date_to   TYPE d,
  time_to   TYPE t,
  stmp_to   TYPE timestamp,
END OF gty_time_frame.


TYPES: tt_seg TYPE STANDARD TABLE OF gty_seg.
DATA: gv_flag TYPE c. " Added for D30K932101
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.

PARAMETERS: p_disp   RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND a,
            p_ext  RADIOBUTTON GROUP rad1 ,
            p_touch RADIOBUTTON GROUP rad1. " Add for Change D30K932157
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: p_file    TYPE        btch0000-text80 MODIF ID fil
                        DEFAULT '/usr/sap/interfaces/&/ORACLE/'.                      "+++WAYCHALJ
SELECTION-SCREEN END OF BLOCK b1.
*{Begin of Change D30K932157
SELECTION-SCREEN BEGIN OF BLOCK d1 WITH FRAME TITLE text-004.
PARAMETERS: p_tcfile LIKE rfpdo-rfbifile MODIF ID tch.
SELECTION-SCREEN END OF BLOCK d1.
*End of Change D30K932157 }
SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME TITLE text-003.
PARAMETERS: p_cc     TYPE   bukrs OBLIGATORY DEFAULT 'EGD' MODIF ID sel," Add for Change D30K932157
            p_year   TYPE   t009b-bdatj OBLIGATORY MODIF ID sel," DEFAULT '2021'," Add for Change D30K932157
            p_rldnr  TYPE   rldnr OBLIGATORY DEFAULT '0L' MODIF ID sel," Add for Change D30K932157
            p_period TYPE   t009b-poper OBLIGATORY MODIF ID sel." DEFAULT '02'." Add for Change D30K932157
SELECTION-SCREEN END OF BLOCK c1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screenoutput.

AT SELECTION-SCREEN.
  PERFORM validation.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM start_of_selection.


*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .

  p_year = sy-datum+0(4).

  p_period = sy-datum+4(2) - 1.
*{Begin of Change D30K932157
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3)
              '/ORACLE/' 'Trigger.tch' INTO p_tcfile ##NO_TEXT.
*End of Change D30K932157 }

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validation .
  IF p_file IS NOT INITIAL.
    REPLACE FIRST OCCURRENCE OF '&' IN p_file WITH sy-sysid.  "+++WAYCHALJ
    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        directory                   = p_file
      EXCEPTIONS
*       PFL_DIR_NOT_EXIST           = 1
*       PFL_PERMISSION_DENIED       = 2
*       PFL_CANT_BUILD_DATASET_NAME = 3
*       PFL_FILE_NOT_EXIST          = 4
*       PFL_AUTHORIZATION_MISSING   = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      MESSAGE 'Directory name is incorrect'(021) TYPE 'E'.
    ENDIF.
*    CONCATENATE P_FILE SY-SYSID(3) 'Oracle EBS file.dat' INTO P_FILE.
  ENDIF.
ENDFORM.                    " VALIDATION
*&---------------------------------------------------------------------*
*&      Form  GET_TIMESTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_TIMESTAMP  text
*      <--P_LS_TIMEFRAME  text
*----------------------------------------------------------------------*
FORM get_timestamp  CHANGING ls_timestamp TYPE zfit_egdtimestmp
                             ls_timeframe  TYPE gty_time_frame.


*  DATA LS_TIMEFRAME  TYPE GTY_TIME_FRAME.
  DATA:"LV_ANSWER TYPE CHAR10,
       lv_tzone TYPE systzonlo .               " <== Insert D30K932426
*       UTC  TYPE TZONREF-TZONE VALUE 'EST'.     <== Delete D30K932426

* --- Read selection date/time of last Daily run in zfit_egdtimestmp.
  CLEAR: ls_timeframe , ls_timestamp, ls_timestamp.

  SELECT * FROM zfit_egdtimestmp
  INTO ls_timestamp  UP TO 1 ROWS WHERE zbukrs = p_cc    AND
*                          SEQNR = LV_SEQNR AND
                          prog  = c_prog   AND
                          p_year  = p_year AND
                          period = p_period
                          ORDER BY timestamp_to DESCENDING.
  ENDSELECT.

  IF sy-subrc = 0.
* --- Create new selection-from date/time
    ls_timeframe-stmp_from = ls_timestamp-timestamp_to.

  ELSE.
    ls_timeframe-stmp_from = '00000000000000'.

  ENDIF.
* --- Create new selection-to date/time
  GET TIME.

  CLEAR: lv_tzone .          " <== Insert D30K932426
  lv_tzone = syst-zonlo .    " <== Insert D30K932426

* Following line Deleted     D30K932426
*  CONVERT DATE SY-DATUM TIME SY-UZEIT INTO TIME STAMP LS_TIMEFRAME-STMP_TO TIME ZONE UTC.
  CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP   " <== Insert D30K932426
          ls_timeframe-stmp_to TIME ZONE lv_tzone .     " <== Insert D30K932426
*  CONCATENATE SY-DATUM SY-UZEIT INTO LS_TIMEFRAME-STMP_TO.
*  ls_timeframe-stmp_to+12(2) = '00'. "round down on whole minutes


* --- Prepare new ZTINTERFACES line
  CLEAR ls_timestamp.
  ls_timestamp-prog     = c_prog.
  ls_timestamp-zbukrs   = p_cc.
  ls_timestamp-seldate  = sy-datum.
  ls_timestamp-period   = p_period.
  ls_timestamp-p_year   = p_year.
  ls_timestamp-timestamp_to   = ls_timeframe-stmp_to.
  ls_timestamp-timestamp_from = ls_timeframe-stmp_from .
*split timestamps into date/time.
  cl_abap_tstmp=>systemtstmp_utc2syst(
          EXPORTING  utc_tstmp = ls_timeframe-stmp_to
          IMPORTING  syst_date = ls_timeframe-date_to    " System Date
                     syst_time = ls_timeframe-time_to ).


ENDFORM.                    " GET_TIMESTAMP
*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_of_selection .


  TYPES : BEGIN OF lty_bkpf,
          bukrs TYPE bukrs,
          belnr TYPE belnr_d,
          gjahr TYPE gjahr,
          awtyp TYPE awtyp,
          awkey TYPE vbeln_vf,
          END OF lty_bkpf,

          BEGIN OF lty_vbrp,
          vbeln TYPE vbeln_vf,
          aubel TYPE vbeln_va,
          END OF lty_vbrp,

          BEGIN OF lty_costc,
          zkostl       TYPE kostl,
          zefscc_ccid  TYPE zefscc_ccid,
          zefscc_cctxt TYPE zefscc_cctxt,
          END OF lty_costc,

          BEGIN OF lty_bseg,
          belnr  TYPE belnr_d,
          kostl  TYPE kostl,
          END OF lty_bseg,

          BEGIN OF lty_skb1,
          bukrs         TYPE bukrs,
          saknr         TYPE saknr,
          altkt         TYPE altkt_skb1,"CHAR10 ,
          END OF lty_skb1,

          BEGIN OF lty_sub,
          zefs_ac       TYPE char7,
          zefs_subid    TYPE char6,
          END OF lty_sub,

          BEGIN OF lty_ilob,
          zvbund            TYPE vbund,
          zefsilob_ilobid   TYPE zefsilob_ilobid,
          zefsilob_ilobtxt  TYPE zefsilob_ilobtxt,
          END OF lty_ilob,

          BEGIN OF lty_contseg,
           vbeln TYPE vbeln,
           bzirk TYPE bzirk,
          END OF lty_contseg,

          BEGIN OF lty_cs,
          zbzirk    TYPE bzirk,
          zefscs_cs  TYPE zefscs_cs,
          zefscs_cstxt TYPE zefscs_cs_txt,
          altkt TYPE zaltkt_cs,
          END OF lty_cs,

          BEGIN OF lty_saleso,
          vbeln TYPE vbeln,
          belnr TYPE belnr_d,
          END OF lty_saleso.


  DATA: ls_timestamp TYPE zfit_egdtimestmp,
        ls_timeframe TYPE gty_time_frame,
        lt_bkpf      TYPE STANDARD TABLE OF lty_bkpf,
        lt_vbrp      TYPE STANDARD TABLE OF lty_vbrp,
        lt_bseg      TYPE STANDARD TABLE OF lty_bseg,
        lt_costc     TYPE STANDARD TABLE OF lty_costc,
        lt_filter    TYPE STANDARD TABLE OF zfit_efsfilter,
        lt_skb1      TYPE STANDARD TABLE OF lty_skb1,
        lt_sub       TYPE STANDARD TABLE OF lty_sub,
*        LT_DOCFLOW   TYPE                   TDT_DOCFLOW,
        lt_seg       TYPE STANDARD TABLE OF gty_seg,
        lt_contseg   TYPE STANDARD TABLE OF lty_contseg,
        lt_efscs     TYPE STANDARD TABLE OF lty_cs,
        lt_saleso    TYPE STANDARD TABLE OF lty_saleso,
        lt_ilob      TYPE STANDARD TABLE OF lty_ilob.
  DATA:  lt_fagl      TYPE STANDARD TABLE OF gty_fagl,
         lt_fagl_vbrk TYPE STANDARD TABLE OF gty_fagl.
  DATA:  ls_costc     TYPE lty_costc,
         ls_bseg     TYPE lty_bseg,
         ls_vbrp     TYPE lty_vbrp,
         ls_seg       TYPE gty_seg,
         ls_saleso    TYPE lty_saleso,
         ls_filter    TYPE zfit_efsfilter,
*         LS_DOCFLOW   LIKE LINE OF LT_DOCFLOW,
         ls_ilob      TYPE  lty_ilob,
         ls_skb1      TYPE  lty_skb1,
         ls_efscs     TYPE  lty_cs,
         ls_contseg   TYPE  lty_contseg,
         ls_sub       TYPE  lty_sub,
         ls_bkpf      TYPE  lty_bkpf.
  DATA: lv_lobid      TYPE zefslob_lobid,
        lv_lobname    TYPE zefslob_lobtxt,
        lv_amount     TYPE vlcur12,
        lv_vbeln      TYPE vbeln.
  DATA: lv_utc_to_est TYPE timestamp. " Add for Change D30K932101

  DATA: gv_thead TYPE thead, " Add for change D30K932501
        gt_tline1 LIKE tline OCCURS 0 WITH HEADER LINE,
        gs_edit TYPE txline,
        gv_data TYPE string. "D30K932501

  FIELD-SYMBOLS: <lfs_fagl>    TYPE gty_fagl.

  IF p_ext IS NOT INITIAL." OR P_DISP IS NOT INITIAL." Added for D30K932157 #Commented for D30K932165
* Get last ran timestamp for file type ASRFVS
    PERFORM get_timestamp CHANGING ls_timestamp
                                   ls_timeframe.
  ENDIF.
  REFRESH: lt_fagl ,  lt_bkpf ,  lt_costc , lt_skb1 , lt_sub, lt_bseg ,
           lt_seg , lt_contseg , lt_efscs , lt_saleso , lt_ilob ,
           lt_filter , lt_fagl_vbrk.

  IF p_ext IS NOT INITIAL.
*{Begin of Change D30K932157
    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        directory                   = p_file
*       WRITE_CHECK                 = ' '
        filname                     = 'COG*.*'
*       DIRECTORY_LONG              =
      EXCEPTIONS
        pfl_dir_not_exist           = 1
        pfl_permission_denied       = 2
        pfl_cant_build_dataset_name = 3
        pfl_file_not_exist          = 4
        OTHERS                      = 5.
    IF sy-subrc = 0.
      MESSAGE 'NEW File Can Not Be Created As .JNN File already EXISTS In Directory'(006) TYPE 'E'.
    ELSEIF sy-subrc = 4.
      " Continue as file not existed
    ELSEIF sy-subrc = 2.
      MESSAGE 'Authorization Error'(009) TYPE 'E'.
    ELSEIF sy-subrc = 1 OR sy-subrc = 3 OR sy-subrc = 5.
      MESSAGE 'Directory Not Exist OR Data Set Error OR Others'(010) TYPE 'E'.
    ENDIF.
*End of Change D30K932157 }
*{Begin of Change D30K932101
* Following lines deleted        D30K932428
*  CLEAR: LV_UTC_TO_EST.
    lv_utc_to_est = ls_timestamp-timestamp_to.   " <== Re-instate D30K932472
*  CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
*   EXPORTING
*     TIMESTAMP_IN          = LV_UTC_TO_EST
*     TIMEZONE              = 'UTC'
*     DURATION              = 14400 " 4 Hours time addition to UTC to convert to EST
*     UNIT                  = 'S'
*   IMPORTING
*     TIMESTAMP_OUT         = LV_UTC_TO_EST
*   EXCEPTIONS
*     TIMESTAMP_ERROR       = 1
*     OTHERS                = 2           .
* End of Deletion                D30K932428
    ls_timestamp-timestamp_to = lv_utc_to_est.
*End of Change D30K932101 }
    SELECT ryear
           docnr
           rldnr
           rbukrs
           awtyp
           racct
           rcntr
           rassc
           SUM( hsl )
           poper
           INTO TABLE lt_fagl
           FROM faglflexa
           WHERE "RACCT NOT IN ( SELECT SAKNR FROM  ZFIT_EFSFILTER WHERE BUKRS = P_CC ) AND
                 ryear = p_year AND
                 rldnr    = p_rldnr AND
                 rbukrs   = p_cc AND
                 poper    = p_period   AND         " <== Changed D30K932472
* Following line deleted         D30K932428
* Following line re-instated     D30K932472
                ( timestamp GT ls_timestamp-timestamp_from AND timestamp LE ls_timestamp-timestamp_to )
           GROUP BY  ryear
                     docnr
                     rldnr
                     rbukrs
                     awtyp
                     racct
                     rcntr
                     rassc
                     poper.
  ELSEIF
    p_disp IS NOT INITIAL.
*{Begin of Change D30K932157
*Adding timestamp logic to make sure both Display & Extration functinos would work similar.
*{Begin of Comment D30K932165
    " CLEAR: LV_UTC_TO_EST.
    "  LV_UTC_TO_EST = LS_TIMESTAMP-TIMESTAMP_TO.
    "  CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
    "   EXPORTING
    "     TIMESTAMP_IN          = LV_UTC_TO_EST
    "     TIMEZONE              = 'UTC'
    "     DURATION              = 14400 " 4 Hours time addition to UTC to convert to EST
    "     UNIT                  = 'S'
    "   IMPORTING
    "     TIMESTAMP_OUT         = LV_UTC_TO_EST
    "   EXCEPTIONS
    "     TIMESTAMP_ERROR       = 1
    "     OTHERS                = 2           .
    "  LS_TIMESTAMP-TIMESTAMP_TO = LV_UTC_TO_EST.
*End of COmment D30K932165 }
*End of Change D30K932157 }
    SELECT ryear
       docnr
       rldnr
       rbukrs
       awtyp
       racct
       rcntr
       rassc
       SUM( hsl )
       poper
       INTO TABLE lt_fagl
       FROM faglflexa
       WHERE "RACCT NOT IN ( SELECT SAKNR FROM  ZFIT_EFSFILTER WHERE BUKRS = P_CC ) AND
             ryear = p_year AND
             rldnr    = p_rldnr AND
             rbukrs   = p_cc AND
             poper    = p_period  AND  "#Commented for D30K932165
       ( timestamp GT ls_timestamp-timestamp_from AND timestamp LE ls_timestamp-timestamp_to )  "trp 13/6
"      ( TIMESTAMP GT LS_TIMESTAMP-TIMESTAMP_FROM AND TIMESTAMP LE LS_TIMESTAMP-TIMESTAMP_TO ) " Added for D30K932157 #Commented for D30K932165
       GROUP BY  ryear
                 docnr
                 rldnr
                 rbukrs
                 awtyp
                 racct
                 rcntr
                 rassc
                 poper.
  ENDIF.
  IF lt_fagl IS NOT INITIAL ."SY-SUBRC = 0.
    lt_fagl_vbrk = lt_fagl.
    DELETE lt_fagl_vbrk WHERE awtyp NE 'VBRK'.
    IF lt_fagl_vbrk IS NOT INITIAL.
      SELECT bukrs belnr gjahr  awtyp awkey
             FROM bkpf INTO TABLE lt_bkpf
             FOR ALL ENTRIES IN lt_fagl_vbrk
             WHERE bukrs = lt_fagl_vbrk-bukrs AND
                   belnr = lt_fagl_vbrk-docnr AND
                   gjahr = lt_fagl_vbrk-year.
      IF sy-subrc = 0.
* To get Sales order based on Billing
        SELECT vbeln aubel FROM vbrp
          INTO TABLE lt_vbrp
          FOR ALL ENTRIES IN lt_bkpf
                   WHERE vbeln = lt_bkpf-awkey. "+0(10)
        IF sy-subrc = 0.
          SELECT vbeln bzirk FROM vbkd INTO TABLE lt_contseg
            FOR ALL ENTRIES IN lt_vbrp
            WHERE vbeln = lt_vbrp-aubel AND bzirk NE ''.
          IF lt_contseg IS NOT INITIAL.
            SELECT zbzirk
                   zefscs_cs
                   zefscs_cstxt
                   altkt FROM zfit_efscs INTO TABLE lt_efscs
              FOR ALL ENTRIES IN lt_contseg WHERE zbzirk = lt_contseg-bzirk.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
* Line of Business Id and Name
    SELECT SINGLE zefslob_lobid
                  zefslob_lobtxt FROM zfit_efslob
           INTO (lv_lobid , lv_lobname) WHERE zbukrs = p_cc.

* Filter table to replace GL
    SELECT * FROM zfit_efsfilter
             INTO TABLE lt_filter
             WHERE bukrs = p_cc.
* Cost Center and Cost center desc.
    SELECT zkostl zefscc_ccid zefscc_cctxt FROM zfit_efscc
                                       INTO TABLE lt_costc . "#EC CI_NOWHERE
    IF sy-subrc = 0.
      SELECT belnr kostl FROM bsis
                         INTO TABLE lt_bseg
*                       PACKAGE SIZE 5000
                         FOR ALL ENTRIES IN lt_fagl
                         WHERE bukrs = lt_fagl-bukrs AND
                               gjahr = lt_fagl-year AND
                               belnr = lt_fagl-docnr AND
                               kostl NE ''.
*    ENDSELECT.

    ENDIF.

* GL Account , gl acciunt name
    SELECT bukrs saknr altkt FROM skb1 INTO TABLE lt_skb1
                       BYPASSING BUFFER FOR ALL ENTRIES IN lt_fagl
                       WHERE bukrs = p_cc AND saknr = lt_fagl-racct.
    IF sy-subrc = 0.
* Sub Account
      SELECT zefs_ac
            zefs_subid FROM zfit_efssub INTO TABLE lt_sub . "#EC CI_NOWHERE "FOR ALL ENTRIES IN LT_SKB1 WHERE ZEFS_AC = LT_SKB1-ALTKT.

    ENDIF.

* Internal line of Business
*    SELECT zvbund      "trp
*           zefsilob_ilobid
*           zefsilob_ilobtxt FROM zfit_efsilob INTO TABLE lt_ilob
*           FOR ALL ENTRIES IN lt_fagl WHERE zvbund = lt_fagl-rassc.
  ENDIF.

  SORT :lt_costc BY zkostl ,  lt_skb1 BY saknr ,lt_ilob BY zvbund ,
        lt_saleso BY belnr , lt_bseg BY belnr , lt_filter BY saknr altkt,
        lt_sub BY zefs_ac, lt_contseg BY vbeln, lt_efscs BY altkt, lt_vbrp BY vbeln,
        lt_bkpf BY belnr.
  CLEAR lv_amount.


  LOOP AT lt_fagl ASSIGNING <lfs_fagl>.
    CLEAR: ls_saleso, ls_contseg, ls_efscs,ls_costc, ls_ilob ,ls_sub, ls_skb1 ,
           ls_ilob , ls_bseg, ls_seg,ls_filter,ls_bkpf, ls_vbrp.
* Line of Business Id and Name
    ls_seg-lob           = lv_lobid.
    ls_seg-lob_name      = lv_lobname.

* Cost Center and Cost center desc.
    READ TABLE lt_bseg INTO ls_bseg WITH  KEY belnr = <lfs_fagl>-docnr BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_costc INTO ls_costc WITH  KEY zkostl = ls_bseg-kostl BINARY SEARCH.
      IF sy-subrc = 0.
        ls_seg-cost_center = ls_costc-zefscc_ccid.
        ls_seg-costc_name  = ls_costc-zefscc_cctxt.
      ELSE.
        ls_seg-cost_center = '00000'.
*      LS_SEG-COSTC_NAME  = 'DEFAULT'.
      ENDIF.
    ELSE.
      ls_seg-cost_center = '00000'.
*      LS_SEG-COSTC_NAME  = 'DEFAULT'.
    ENDIF.

* Gl Account
* Begin of changes for D30K932501 "Adding ILOB values
    READ TABLE lt_skb1 INTO ls_skb1 WITH KEY saknr = <lfs_fagl>-racct BINARY SEARCH.
    IF sy-subrc = 0.
      IF ls_skb1-altkt IS NOT INITIAL.
        CONCATENATE <lfs_fagl>-racct <lfs_fagl>-bukrs INTO gv_data.
        CONCATENATE ls_skb1-saknr ls_skb1-bukrs INTO gv_data.

        gv_thead-tdid = '0001'.
        gv_thead-tdspras = 'EN'.
        gv_thead-tdname =  gv_data.
        gv_thead-tdobject = 'SKB1'.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
           client                        = sy-mandt
            id                           = gv_thead-tdid
            language                     = gv_thead-tdspras
            name                         = gv_thead-tdname
            object                       = gv_thead-tdobject
*         ARCHIVE_HANDLE               = 0
*         LOCAL_CAT                    = ' '
*       IMPORTING
*         HEADER                       =
*         OLD_LINE_COUNTER             =
          TABLES
            lines                        = gt_tline1
         EXCEPTIONS
           id                            = 1
           language                      = 2
           name                          = 3
           not_found                     = 4
           object                        = 5
           reference_check               = 6
           wrong_access_to_archive       = 7
           OTHERS                        = 8
                  .
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        LOOP AT gt_tline1.
          gs_edit = gt_tline1-tdline.
        ENDLOOP.
clear gt_tline1[].

      ENDIF.
  "End of changes for D30K932501 " Adding ILOB values

      READ TABLE lt_filter INTO ls_filter WITH KEY saknr = <lfs_fagl>-racct
                                                    altkt = ls_skb1-altkt BINARY SEARCH.
      IF sy-subrc = 0.
        SHIFT ls_filter-raltkt LEFT DELETING LEADING '0'.
        ls_seg-gl_acct = ls_filter-raltkt+0(5).
      ELSE.
        SHIFT ls_skb1-altkt LEFT DELETING LEADING '0'.
        ls_seg-gl_acct = ls_skb1-altkt+0(5).
      ENDIF.

*  Sub Account
      READ TABLE lt_sub INTO ls_sub WITH KEY zefs_ac =  ls_skb1-altkt BINARY SEARCH. "#EC WARNOK
      IF sy-subrc = 0.
        ls_seg-sub_acct = ls_sub-zefs_subid.
      ELSE.
        ls_seg-sub_acct = '000000'.
      ENDIF.

* Contribution Segment
*      SHIFT LS_SKB1-ALTKT LEFT DELETING LEADING '0'.
      READ TABLE lt_efscs INTO ls_efscs  WITH KEY altkt =  ls_seg-gl_acct BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE lt_bkpf INTO ls_bkpf WITH KEY belnr = <lfs_fagl>-docnr BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE lt_vbrp INTO ls_vbrp WITH KEY vbeln = ls_bkpf-awkey BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE lt_contseg INTO ls_contseg WITH KEY vbeln = ls_vbrp-aubel BINARY SEARCH.
            IF sy-subrc = 0.
              CLEAR ls_efscs.
              READ TABLE lt_efscs INTO ls_efscs  WITH KEY zbzirk = ls_contseg-bzirk
                                                          altkt = ls_seg-gl_acct.
              IF sy-subrc = 0.
                ls_seg-contseg        = ls_efscs-zefscs_cs.
                ls_seg-contseg_name   = ls_efscs-zefscs_cstxt.
              ELSE.
                ls_seg-contseg = '000'.
                ls_seg-contseg_name  = 'DEFAULT'.
              ENDIF.
            ELSE.
              ls_seg-contseg = '000'.
              ls_seg-contseg_name  = 'DEFAULT'.
            ENDIF.
          ELSE.
            ls_seg-contseg = '000'.
            ls_seg-contseg_name  = 'DEFAULT'.
          ENDIF.
        ELSE.
          ls_seg-contseg = '000'.
          ls_seg-contseg_name  = 'DEFAULT'.
        ENDIF.
      ELSE.
        ls_seg-contseg = '000'.
        ls_seg-contseg_name  = 'DEFAULT'.
      ENDIF.
    ELSE.
      ls_seg-contseg = '000'.
      ls_seg-contseg_name  = 'DEFAULT'.
    ENDIF.
*  Internal line of Business
    READ TABLE lt_ilob INTO ls_ilob WITH KEY zvbund = <lfs_fagl>-rassc BINARY SEARCH.
    IF sy-subrc = 0.
*        ls_seg-int_lob     = ls_ilob-zefsilob_ilobid. " for D30K932502
      ls_seg-int_lobname = ls_ilob-zefsilob_ilobtxt.
    ENDIF.
    IF gs_edit IS NOT INITIAL. " for D30K932501
      ls_seg-int_lob = gs_edit.
      CLEAR gs_edit.
      else.
        ls_seg-int_lob = '00000'.
    ENDIF.                      " for D30K932501

    ls_seg-l_amount      = <lfs_fagl>-hsl.
    COLLECT ls_seg INTO lt_seg.
    lv_amount = ls_seg-l_amount + lv_amount.
    clear gt_tline1[].

*    REFRESH LT_DOCFLOW.
  ENDLOOP.
  DELETE lt_seg WHERE l_amount IS INITIAL OR  l_amount = '0.00'.
  IF p_disp IS NOT INITIAL.
    PERFORM alv_display USING lt_seg.
  ELSEIF
    p_ext IS NOT INITIAL.
    IF lv_amount NE 0.
      MESSAGE text-026 TYPE 'E'.

    ENDIF.
*{Begin of Chang D30K932101
    IF lt_seg IS NOT INITIAL.
      PERFORM outfile USING lt_seg.
      CHECK gv_flag IS NOT INITIAL." Added for Change D30K932101
      PERFORM update_table USING ls_timestamp.
    ELSE.
      MESSAGE text-036 TYPE 'I'.
    ENDIF.
*End of Change D30K932101 }
*{Begin of Change D30K932157
  ELSEIF p_touch IS NOT INITIAL.
    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        directory                   = p_file
*       WRITE_CHECK                 = ' '
        filname                     = 'COG*.*'
*       DIRECTORY_LONG              =
      EXCEPTIONS
        pfl_dir_not_exist           = 1
        pfl_permission_denied       = 2
        pfl_cant_build_dataset_name = 3
        pfl_file_not_exist          = 4
        OTHERS                      = 5.
    IF sy-subrc = 0.
      PERFORM create_touch_file.
      MESSAGE 'Touch File Is Generated'(007) TYPE 'S'.
      LEAVE TO LIST-PROCESSING.
    ELSEIF sy-subrc = 4.
      MESSAGE 'Touch File Can Not Be Created As NO .JNN File In Directory'(005) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE TO LIST-PROCESSING.
    ELSEIF sy-subrc = 2.
      MESSAGE 'Authorization Error'(009) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE TO LIST-PROCESSING.
    ELSEIF sy-subrc = 1 OR sy-subrc = 3 OR sy-subrc = 5.
      MESSAGE 'Directory Not Exist OR Data Set Error OR Others'(010) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE TO LIST-PROCESSING.
    ENDIF.
*End of Change D30K932157 }
  ENDIF.
ENDFORM.                    " START_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_display USING lt_seg TYPE tt_seg.

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.
*        LV_EXCEPTION     TYPE  REF TO  CX_ROOT.

  DATA: o_alv TYPE REF TO cl_salv_table.
  TRY.
      CALL METHOD cl_salv_table=>factory
*    EXPORTING
*      LIST_DISPLAY   = IF_SALV_C_BOOL_SAP=>FALSE
*      R_CONTAINER    =
*      CONTAINER_NAME =
        IMPORTING
          r_salv_table   = o_alv
        CHANGING
          t_table        = lt_seg
          .

      lr_functions = o_alv->get_functions( ).
      lr_functions->set_all( abap_true ).
      o_alv->display( ).
    CATCH cx_salv_msg .
      MESSAGE text-022 TYPE 'E'.
  ENDTRY.

ENDFORM.                    " ALV_DISPLAY
" FIELDCATLOG
*&---------------------------------------------------------------------*
*&      Form  OUTFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SEG  text
*----------------------------------------------------------------------*
FORM outfile  USING   lt_seg TYPE tt_seg.
  CONSTANTS: "LC_BATCHID      TYPE CHAR20 VALUE 'EFS Batch ID',

             lc_status       TYPE char3  VALUE 'NEW',
             lc_jesource     TYPE char3  VALUE 'COG',
             lc_jecateg      TYPE char7  VALUE 'Other' ##NO_TEXT,
             lc_currcode     TYPE char3  VALUE 'CAD',
             lc_createdby    TYPE char1  VALUE '1',
             lc_actflag      TYPE char1  VALUE 'A',
             lc_jedesc       TYPE char11 VALUE 'COG JOURNAL',
             lc_linedesc     TYPE char30 VALUE 'COG JOURNAL ENTRY'.
*             LC_LANG         TYPE SY-LANGU VALUE 'EN'.
  DATA: ls_data    TYPE string,
        lv_flag    TYPE flag.

  DATA:   lv_date     TYPE bkpf-bldat,
          lv_s_date   TYPE char11,
          lv_acc_date TYPE char11,
          lv_gl       TYPE char34,
          lv_batchid  TYPE char20,
          lv_amount1(15)   TYPE c.

  DATA :ls_segment TYPE gty_seg.
*  Accounting Date.
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_year
*     I_MONMET       = '00'
      i_periv        = 'K1'
      i_poper        = p_period
    IMPORTING
      e_date         = lv_date
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc NE 0.
    MESSAGE text-025 TYPE 'E'.
  ENDIF.
  PERFORM convert_datemonth_shortname USING    lv_date
                                      CHANGING lv_s_date.
  lv_acc_date = lv_s_date.
  CLEAR :lv_s_date ,lv_date.
*  Date created.
  lv_date = sy-datum.
  PERFORM convert_datemonth_shortname USING    lv_date
                                        CHANGING lv_s_date.

*  CONCATENATE P_FILE SY-SYSID(3) 'Oracle EBS file.dat' INTO P_FILE.
  CONCATENATE p_file 'COG_' sy-datum sy-uzeit '.JNN' INTO p_file.
  OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
  IF sy-subrc NE 0.
    MESSAGE text-034 TYPE 'E'.
    RETURN.
  ENDIF.
  ls_data = 'SOF'.
  TRANSFER ls_data TO p_file.
  CLEAR ls_data.
  NEW-LINE.
*  GET TIME STAMP FIELD LV_TIMESTMP.
*  LV_TIMES = LV_TIMESTMP.
  lv_batchid = text-023.
  CONCATENATE lv_batchid sy-datum sy-uzeit INTO lv_batchid." SEPARATED BY '-'.

  LOOP AT  lt_seg INTO ls_segment.
    CLEAR lv_amount1.
    lv_amount1 = ls_segment-l_amount.
*    LV_AMOUNT1 = LV_AMOUNT.
    SHIFT lv_amount1 RIGHT DELETING TRAILING space.
    TRANSLATE lv_amount1 USING ' 0'.

    IF ls_segment-l_amount GE 0.
*      CONCATENATE '+' LV_AMOUNT1 INTO LV_AMOUNT1.
      REPLACE lv_amount1+0(1) IN lv_amount1 WITH '+'.
    ELSE.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_amount1.
    ENDIF.

    CONCATENATE ls_segment-lob
                ls_segment-contseg
                ls_segment-cost_center
                ls_segment-gl_acct
                ls_segment-sub_acct
                ls_segment-int_lob INTO lv_gl SEPARATED BY '.'   .

    CONCATENATE lv_batchid
                text-024"LV_LEGAL_ENAME
                lc_status
                lc_jesource
                lc_jecateg
                lv_acc_date
                lc_currcode
                lv_s_date
                lc_createdby
                lc_actflag
                lv_amount1
                lc_jedesc
                lv_gl
                lc_linedesc   INTO ls_data SEPARATED BY '~'.
    TRANSFER ls_data TO p_file.
    AT LAST.
      lv_flag = 'C_X'.
      ls_data = 'EOF'.
      TRANSFER ls_data TO p_file.
    ENDAT.
    CLEAR: ls_data , lv_gl , ls_segment. "LV_ACC_DATE, LV_S_DATE ,
  ENDLOOP.
  IF lv_flag IS NOT INITIAL.
    gv_flag = 'X'. " Added for Change D30K932101
    MESSAGE text-030 TYPE 'I'.
  ELSE.
    MESSAGE text-036 TYPE 'I'.
  ENDIF.
*  CLEAR LV_TIMES.
  CLOSE DATASET p_file.
ENDFORM.                    " OUTFILE
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATEMONTH_SHORTNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_DATE  text
*      <--P_LV_ACC_DATE  text
*      <--P_CALL  text
*      <--P_FUNCTION  text
*      <--P_'ISP_GET_MONTH_NAME'  text
*      <--P_EXPORTING  text
*      <--P_DATE  text
*      <--P_=  text
*      <--P_SY_DATUM  text
*      <--P_LANGUAGE  text
*      <--P_=  text
*      <--P_LC_LANG  text
*      <--P_IMPORTING  text
*      <--P_SHORTTEXT  text
*      <--P_=  text
*      <--P_LV_SHORT_DATE  text
*----------------------------------------------------------------------*
FORM convert_datemonth_shortname  USING    lv_date TYPE sy-datum
                                  CHANGING lv_s_date TYPE char11.
  CONSTANTS :lc_lang         TYPE sy-langu VALUE 'E'.
  DATA : lv_short_date TYPE t247-ktx.

  CALL FUNCTION 'ISP_GET_MONTH_NAME'
    EXPORTING
      date         = lv_date
      language     = lc_lang
*     MONTH_NUMBER = '00'
    IMPORTING
      shorttext    = lv_short_date
    EXCEPTIONS
*     CALENDAR_ID  = 1
*     DATE_ERROR   = 2
*     NOT_FOUND    = 3
*     WRONG_INPUT  = 4
      OTHERS       = 5.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CONCATENATE lv_date+6(2) lv_short_date lv_date+0(4) INTO lv_s_date SEPARATED BY '-'.
ENDFORM.                    " CONVERT_DATEMONTH_SHORTNAME
*&---------------------------------------------------------------------*
*&      Form  SCREENOUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screenoutput .

  IF p_disp = 'X'.
    LOOP AT SCREEN.
      IF  screen-group1 = 'FIL'.

        screen-active = 0.
*        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
*{Begin of Change D30K932157
      IF screen-group1 = 'TCH' .
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
*End of Change D30K932157 }
    ENDLOOP.
  ELSEIF p_ext = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'FIL' .

        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
*{Begin of Change D30K932157
      IF screen-group1 = 'TCH' .
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF p_touch = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'FIL' OR screen-group1 = 'SEL'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'TCH' .
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_TCFILE' .
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
*End of Change D30K932157 }
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SCREENOUTPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_TIMESTAMP  text
*----------------------------------------------------------------------*
FORM update_table  USING    ls_timestamp TYPE zfit_egdtimestmp.

  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      mode_rstable   = 'E'
      tabname        = 'ZFIT_EGDTIMESTMP'
*     VARKEY         = LV_ENQKEY
      x_tabname      = 'X'
      x_varkey       = 'X'
*     _SCOPE         = '2'
      _wait          = 'X'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2.
  CASE sy-subrc.
    WHEN 1.
      " locking user is in sy-msgv1
*      RAISE FOREIGN_LOCK.
    WHEN 2.
*      RAISE SYSTEM_FAIL.
  ENDCASE.

  INSERT zfit_egdtimestmp  FROM ls_timestamp.
  IF sy-subrc = 0.
    COMMIT WORK.
*    AND SY-SUBRC <> 4.
*    RAISE SYSTEM_FAIL.
  ENDIF.
  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      mode_rstable = 'E'
      tabname      = 'ZFIT_EGDTIMESTMP'
      x_tabname    = 'X'
      x_varkey     = 'X'.
*     _SCOPE       = '3'
*     _SYNCHRON    = ' '
*    EXCEPTIONS
*      OTHERS       = 1.
ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  CREATE_TOUCH_FILE
*&---------------------------------------------------------------------*
*Form Created for Change D30K932157
*----------------------------------------------------------------------*
FORM create_touch_file .
  DATA: lv_msg(80) TYPE c.
  OPEN DATASET p_tcfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS MESSAGE lv_msg.
  IF sy-subrc NE '0'.
    MESSAGE lv_msg TYPE 'I'.
    STOP.
  ENDIF.
  TRANSFER text-011 TO p_tcfile.
  CLOSE DATASET p_tcfile.
  IF sy-subrc NE '0'.
    MESSAGE 'Unsuccessfl Close'(012) TYPE 'I'.
    STOP.
  ENDIF.
ENDFORM.                    " CREATE_TOUCH_FILE
