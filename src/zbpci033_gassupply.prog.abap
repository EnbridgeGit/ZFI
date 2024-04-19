REPORT  zbpci033_gassupply MESSAGE-ID zs.
************************************************************************
*  Project:    COG                                                     *
*  Date:       July    2021                                            *
*  Author:     Durgaprakash                                            *
*  Program Description:                                                *
*  This program will extract data for GAS SUpply Deferral for          *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date       Issue    By      Description                               *
************************************************************************
TABLES: mkpf, mseg, keko, ekko.

TYPES: BEGIN OF ty_mkpf,
    mjahr LIKE mkpf-mjahr,
    budat LIKE mkpf-budat,
    bldat LIKE mkpf-bldat,
    frbnr LIKE mkpf-frbnr,
    bsart LIKE ekko-bsart,
    bwart LIKE mseg-bwart,
    werks LIKE mseg-werks,
    lifnr LIKE mseg-lifnr,
    lgort LIKE mseg-lgort,
    matnr LIKE mseg-matnr,
    ebeln LIKE mseg-ebeln,

    kalnr LIKE keko-kalnr,
    kadky LIKE keko-kadky,

    dmbtr LIKE mseg-dmbtr,
    bualt LIKE mseg-bualt,
    menge LIKE mseg-menge,
  END OF ty_mkpf,

BEGIN OF ty_mkpf_sum,
    werks LIKE mseg-werks,
    budat LIKE mkpf-budat,
    lifnr LIKE mseg-lifnr,
    frbnr LIKE mkpf-frbnr,
    bsart LIKE ekko-bsart,
    kalnr LIKE keko-kalnr,
    kadky LIKE keko-kadky,

    dmbtr LIKE mseg-dmbtr,
    bualt LIKE mseg-bualt,
    menge LIKE mseg-menge,
END OF ty_mkpf_sum.

DATA: lv_budats LIKE mkpf-budat,
      lv_budate LIKE mkpf-budat.

DATA: lv_local      TYPE integer,
      ls_mkpf       TYPE ty_mkpf,
      lt_mkpf       LIKE TABLE OF ls_mkpf,
      ls_mkpf_sum   TYPE ty_mkpf_sum,
      lt_mkpf_sum   LIKE TABLE OF ls_mkpf_sum,
      lt_mkpf_final LIKE TABLE OF ls_mkpf_sum,
      lv_gpreis     LIKE ckis-gpreis,
      lv_alb        LIKE mseg-dmbtr,
      st_datarec    TYPE string,
      t_data        LIKE TABLE OF st_datarec.

DATA: msg(80)           TYPE c,
      lv_account(25)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(15)    TYPE c,
      lv_entity(15)     TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_customer(15)   TYPE c,
      lv_dso(6)         TYPE c,
      lv_service(10)    TYPE c,
      lv_paths(10)      TYPE c,
      lv_projectst(12)  TYPE c,
      lv_amount(20)     TYPE c.

DATA: tuchfile          LIKE rfpdo-rfbifile.

CONSTANTS: c_delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

PARAMETERS: p_perio  TYPE jahrper     OBLIGATORY.

SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_matnr  LIKE mseg-matnr  DEFAULT 'NATGAS' OBLIGATORY.
SELECT-OPTIONS:
    "s_budat  FOR  mkpf-budat   DEFAULT sy-datum OBLIGATORY NO-EXTENSION,
    s_werks  FOR  mseg-werks,
    s_lgort  FOR  mseg-lgort  DEFAULT 'A001',
    s_bwart  FOR  mseg-bwart,
    s_bsart  FOR  ekko-bsart.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK B11 WITH FRAME TITLE TEXT-111.
PARAMETERS:P_BUDATS TYPE BUDAT,
           P_BUDATE TYPE BUDAT.
SELECTION-SCREEN END OF BLOCK B11.

SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_file    TYPE        string DEFAULT 'H:\SAPTEMP\GASSupply.csv',
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile.

SELECTION-SCREEN END OF BLOCK c1.

*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/cog/' INTO csvfile.
  s_bwart-sign   = 'I'.
  s_bwart-option = 'EQ'.
  s_bwart-low    = '101'.
  APPEND s_bwart.
  s_bwart-low    = '102'.
  APPEND s_bwart.
  s_bwart-low    = '122'.
  APPEND s_bwart.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File' ##NO_TEXT,
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  IF p_local = 'X'.
    PERFORM check_file_path.
  ENDIF.
*************************************************************************
*************************************************************************
START-OF-SELECTION.

  CONCATENATE p_perio(4) p_perio+5(2) '01' INTO lv_budats.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_budats
    IMPORTING
      last_day_of_month = lv_budate
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    "Write
    WRITE: /'Incorrect Date' ##NO_TEXT.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CONCATENATE csvfile 'cog.tch' INTO tuchfile.
  CONCATENATE csvfile 'SAP-' lv_budats(4) '-' lv_budats+4(2) '-COGSup.csv' INTO csvfile.

  IF p_local = 'X'.
    lv_local = 1.
  ELSE.
    lv_local = 0.
  ENDIF.

  PERFORM get_db_data.
  PERFORM sumarize_data.
  PERFORM print_report.
  IF lv_local = 0.
    PERFORM create_touch_file.
  ENDIF.

*----------------------------------------------------------------------*
FORM get_db_data.
  IF P_BUDATS IS NOT INITIAL AND P_BUDATE IS NOT INITIAL.
    SELECT mkpf~mjahr mkpf~budat mkpf~bldat mseg~lgort
           mkpf~frbnr ekko~bsart mseg~matnr mseg~werks
           mseg~bwart mseg~lifnr mseg~ebeln
           mseg~dmbtr mseg~bualt mseg~menge
           keko~kalnr keko~kadky
      INTO CORRESPONDING FIELDS OF TABLE lt_mkpf
      FROM ( ( ( mkpf INNER JOIN mseg
                        ON mseg~mblnr = mkpf~mblnr AND
                           mseg~mjahr = mkpf~mjahr )
                      INNER JOIN keko
                        ON keko~matnr = mseg~matnr  AND
                           keko~werks = mseg~werks )
                      INNER JOIN ekko
                        ON ekko~ebeln = mseg~ebeln )

      WHERE  mkpf~mjahr =  p_perio(4)
        AND  mkpf~budat => lv_budats
        AND  mkpf~budat =< lv_budate
        AND  mseg~bwart IN s_bwart
        AND  mseg~werks IN s_werks
        AND  mseg~lgort IN s_lgort
        AND  mseg~matnr =  p_matnr
        AND  keko~bwdat =< P_BUDATS
        AND  keko~bidat => P_BUDATE
        AND  keko~feh_sta = 'FR'                            "sdp54955
        AND  ekko~bsart IN s_bsart.
  else.
    SELECT mkpf~mjahr mkpf~budat mkpf~bldat mseg~lgort
           mkpf~frbnr ekko~bsart mseg~matnr mseg~werks
           mseg~bwart mseg~lifnr mseg~ebeln
           mseg~dmbtr mseg~bualt mseg~menge
           keko~kalnr keko~kadky
      INTO CORRESPONDING FIELDS OF TABLE lt_mkpf
      FROM ( ( ( mkpf INNER JOIN mseg
                        ON mseg~mblnr = mkpf~mblnr AND
                           mseg~mjahr = mkpf~mjahr )
                      INNER JOIN keko
                        ON keko~matnr = mseg~matnr  AND
                           keko~werks = mseg~werks )
                      INNER JOIN ekko
                        ON ekko~ebeln = mseg~ebeln )

      WHERE  mkpf~mjahr =  p_perio(4)
        AND  mkpf~budat => lv_budats
        AND  mkpf~budat =< lv_budate
        AND  mseg~bwart IN s_bwart
        AND  mseg~werks IN s_werks
        AND  mseg~lgort IN s_lgort
        AND  mseg~matnr =  p_matnr
        AND  keko~bwdat =< mkpf~budat
        AND  keko~bidat => mkpf~budat
        AND  keko~feh_sta = 'FR'                            "sdp54955
        AND  ekko~bsart IN s_bsart.
  endif.
ENDFORM.                    "get_db_data

*----------------------------------------------------------------------*
FORM sumarize_data.

  LOOP AT lt_mkpf INTO ls_mkpf.
    MOVE-CORRESPONDING ls_mkpf TO ls_mkpf_sum.

    "Adjust amounts on Movement Type
    CASE ls_mkpf-bwart.
      WHEN 102 OR 104 OR 122 OR 512 OR 522.
        ls_mkpf_sum-dmbtr = ls_mkpf-dmbtr * -1.
        ls_mkpf_sum-bualt = ls_mkpf-bualt * -1.
        ls_mkpf_sum-menge = ls_mkpf-menge * -1.
    ENDCASE.

    "Standardize the date
    ls_mkpf_sum-budat = ls_mkpf-budat.
    ls_mkpf_sum-budat+6(2) = '00'.

    "If frbnr is blank, and TYPE = ZLOC this should be COMM
    IF ls_mkpf-frbnr IS INITIAL AND ls_mkpf-bsart = 'ZLOC'.
      ls_mkpf_sum-frbnr = 'COMM'.
    ELSEIF ls_mkpf-frbnr = 'FUEL'.  " AND ls_mkpf-bsart = 'ZLOC'.
      ls_mkpf_sum-frbnr = 'COMM'.
    ELSE.
      ls_mkpf_sum-frbnr = ls_mkpf-frbnr.
    ENDIF.

    APPEND ls_mkpf_sum TO lt_mkpf_sum.
  ENDLOOP.

  SORT lt_mkpf_sum BY werks budat lifnr frbnr bsart kalnr kadky.

  LOOP AT lt_mkpf_sum INTO ls_mkpf_sum.
    AT END OF kadky.
      SUM.
      APPEND ls_mkpf_sum TO lt_mkpf_final.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "sumarize_data

*----------------------------------------------------------------------*
FORM print_report.

  DATA: t_bpcmap TYPE STANDARD TABLE OF zfit_bpcmap,
        w_bpcmap TYPE zfit_bpcmap.

  REFRESH: t_bpcmap.

  SELECT * FROM zfit_bpcmap INTO TABLE t_bpcmap.        "#EC CI_NOWHERE

  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011 text-012 text-013
              INTO st_datarec SEPARATED BY c_delimtr.

  APPEND st_datarec TO t_data.

*Set the hard-coded values.

  lv_category     = 'Actual' ##NO_TEXT.
  lv_datasrc      = 'SAP_COG_Supply'.
  lv_intco        = 'No_Intco'.
*  lv_rptcurrency  = 'GJ'. "'LC'.
  lv_dso          = 'No_DSO'.
  lv_projectst    = 'No_ProjectST'.

  LOOP AT lt_mkpf_final INTO ls_mkpf_sum.
    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WERKS'
                                                   value = ls_mkpf_sum-werks.
    IF w_bpcmap-entity IS NOT INITIAL.
      lv_entity = w_bpcmap-entity.
    ELSE.
      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'BSART'
                                                     value = ls_mkpf_sum-bsart. " to check
      IF w_bpcmap-entity IS NOT INITIAL.
        lv_entity = w_bpcmap-entity.
      ELSE.
        CONCATENATE 'UNK_' ls_mkpf_sum-werks INTO lv_entity.
      ENDIF.
    ENDIF.

    lv_time = ls_mkpf_sum-budat.

    SHIFT ls_mkpf_sum-lifnr LEFT BY 5 PLACES.
*    CONCATENATE 'COG_' ls_mkpf_sum-lifnr INTO lv_customer.
    lv_customer = 'no_customer'.

    lv_service = 'No_Service'."ls_mkpf_sum-frbnr.

    lv_paths = ls_mkpf_sum-bsart.

    IF ls_mkpf_sum-dmbtr <> 0.
      lv_account = 'Supp_Cost_Land_WACOG'.
      lv_rptcurrency  = 'LC'.
      lv_amount = ls_mkpf_sum-dmbtr.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_amount.
      CONCATENATE lv_account lv_category lv_datasrc lv_entity
                  lv_intco lv_rptcurrency lv_time lv_customer lv_dso
                  lv_service lv_paths lv_projectst lv_amount
                  INTO st_datarec SEPARATED BY c_delimtr.

      APPEND st_datarec TO t_data.

    ENDIF.

    IF ls_mkpf_sum-bualt <> 0.
      lv_account = 'Supp_Cost_GRIR'.
      lv_rptcurrency  = 'LC'.
      lv_amount = ls_mkpf_sum-bualt.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_amount.
      CONCATENATE lv_account lv_category lv_datasrc lv_entity
            lv_intco lv_rptcurrency lv_time lv_customer lv_dso
            lv_service lv_paths lv_projectst lv_amount
            INTO st_datarec SEPARATED BY c_delimtr.

      APPEND st_datarec TO t_data.
    ENDIF.

    IF ls_mkpf_sum-menge <> 0.
      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WERKS'
                                                 value = ls_mkpf_sum-werks.
      IF w_bpcmap-account IS NOT INITIAL.
        lv_account = w_bpcmap-account.
        lv_rptcurrency  = w_bpcmap-rptcurrency.
      ENDIF.

      lv_amount = ls_mkpf_sum-menge.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_amount.
      CONCATENATE lv_account lv_category lv_datasrc lv_entity
            lv_intco lv_rptcurrency lv_time lv_customer lv_dso
            lv_service lv_paths lv_projectst lv_amount
            INTO st_datarec SEPARATED BY c_delimtr.

      APPEND st_datarec TO t_data.

      "Get ALB BR Price
      SELECT SINGLE gpreis
        INTO lv_gpreis
        FROM ckis
        WHERE kalnr = ls_mkpf_sum-kalnr
          AND kadky = ls_mkpf_sum-kadky
          AND elemt = '10'.                             "#EC CI_NOFIRST

      IF lv_gpreis <> 0.
        lv_account = 'Supp_Cost_Alberta'.
        lv_rptcurrency  = 'LC'.
        lv_alb = ( lv_gpreis / 1000 ) * ls_mkpf_sum-menge.
        lv_amount = lv_alb.
        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            value = lv_amount.
        CONCATENATE lv_account lv_category lv_datasrc lv_entity
              lv_intco lv_rptcurrency lv_time lv_customer lv_dso
              lv_service lv_paths lv_projectst lv_amount
              INTO st_datarec SEPARATED BY c_delimtr.

        APPEND st_datarec TO t_data.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_local = 0.

    PERFORM open_csvfile.

    LOOP AT t_data INTO st_datarec.
      TRANSFER st_datarec TO csvfile.
    ENDLOOP.

    PERFORM close_csvfile.
*    WRITE: 'File Output Successfully to: ', csvfile.
    WRITE: text-014, csvfile.

  ELSE.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_file
      TABLES
        data_tab                = t_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*    WRITE: 'File Output Successfully to: ', p_file.
    WRITE: text-014, p_file.
  ENDIF.

ENDFORM.                    "print_report

*----------------------------------------------------------------------*
FORM open_csvfile.
  OPEN DATASET csvfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "OPEN_CSVFILE

*----------------------------------------------------------------------*
FORM close_csvfile.
  CLOSE DATASET csvfile.
  IF sy-subrc NE '0'.
*    MESSAGE e019 WITH 'unsuccessfl close' csvfile msg.
    MESSAGE e019 WITH text-015 csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    "CLOSE_ALL_FILES

*&---------------------------------------------------------------------*
*Create Touch File.
*----------------------------------------------------------------------*
FORM create_touch_file.
  OPEN DATASET tuchfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH tuchfile msg.
    STOP.
  ENDIF.

*  TRANSFER 'This is a touch file' TO tuchfile.
  TRANSFER text-016 TO tuchfile.

  CLOSE DATASET tuchfile.
  IF sy-subrc NE '0'.
*    MESSAGE e019 WITH 'unsuccessfl close' tuchfile msg.
    MESSAGE e019 WITH text-015 tuchfile msg.
    STOP.
  ENDIF.

ENDFORM.                    "CREATE_TOUCH_FILE

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098 ##TEXT_POOL.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path ##TEXT_POOL.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
