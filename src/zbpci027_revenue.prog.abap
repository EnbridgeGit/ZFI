REPORT  zbpci027_revenue MESSAGE-ID zs.
************************************************************************
*  Project:    COG                                                     *
*  Date:       July 2021                                               *
*  Author:     Jaydeep Waychal                                            *
*  Program Description:                                                *
*  This program will extract data for revenue banner actuals for       *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
TABLES: ce11100.

TYPES:  BEGIN OF ty_ce11100,
          vrgar       LIKE ce11100-vrgar,
          gjahr       LIKE ce11100-gjahr,
          perde       LIKE ce11100-perde,
          kndnr       LIKE ce11100-kndnr,
          bukrs       LIKE ce11100-bukrs,
          werks       LIKE ce11100-werks,
          rbeln       LIKE ce11100-rbeln,
          wwseg       LIKE ce11100-wwseg,
          wwprg       LIKE ce11100-wwprg,
          wwsub       LIKE ce11100-wwsub,
          wwrat       LIKE ce11100-wwrat,
          wwser       LIKE ce11100-wwser,
          wwsld       LIKE ce11100-wwsld,
          wwpce       LIKE ce11100-wwpce,
          wwrsn       LIKE ce11100-wwrsn,
          vvbvl       LIKE ce11100-vvbvl,
          vvbrv       LIKE ce11100-vvbrv,
        END OF ty_ce11100.

TYPES:  BEGIN OF ty_output,
          account(20)     TYPE c,
          category(6)     TYPE c,
          datasrc(20)     TYPE c,
          entity(3)       TYPE c,
          intco(8)        TYPE c,
          rptcurrency(2)  TYPE c,
          time(8)         TYPE c,
          customer(16)    TYPE c,
          dso(2)          TYPE c,
          service(13)     TYPE c,
          projectst(12)   TYPE c,
          paths(7)        TYPE c,
          amount(15)      TYPE p DECIMALS 3,
        END OF ty_output.

DATA: lv_local      TYPE integer,
      wa_ce11100    LIKE coep,
      s_ce11100     TYPE ty_ce11100,"ce11100, "ty_ce11100,
      t_ce11100     TYPE STANDARD TABLE OF ty_ce11100,
      t_ce11100_sum TYPE STANDARD TABLE OF ty_ce11100,      "ce11100,
      s_output      TYPE ty_output,
      t_output      LIKE TABLE OF s_output,
      t_output_sum  LIKE TABLE OF s_output,
      st_datarec    TYPE string,
      t_data        LIKE TABLE OF st_datarec.

DATA: msg(80)           TYPE c,
      lv_account(20)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(20)    TYPE c,
      lv_entity(3)      TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_customer(16)   TYPE c,
      lv_dso(2)         TYPE c,
      lv_service(13)    TYPE c,
      lv_paths(7)       TYPE c,
      lv_projectst(12)  TYPE c,
      lv_amount(15)     TYPE c,
      w_vol_amt(15)     TYPE p DECIMALS 3,                  "SDP60315
      w_dol_amt(15)     TYPE p DECIMALS 2.                  "SDP60315

DATA: tuchfile          LIKE rfpdo-rfbifile.
DATA :it_tvarvc     TYPE TABLE OF tvarvc,   "D30K932532
      it_tvarvc1     TYPE TABLE OF tvarvc,
      wa_tvarvc      TYPE tvarvc,
      lv_mnth      TYPE char2,
      lv_yr        TYPE char4,
      lv_data      TYPE char7,
      count        TYPE string.  "D30K932532

CONSTANTS: c_delimtr  TYPE c VALUE ','.

************************************************************************
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

PARAMETERS:
p_bukrs     LIKE t001-bukrs OBLIGATORY,
p_paledg    LIKE ce11100-paledger OBLIGATORY,
p_perio     LIKE ce11100-perio  OBLIGATORY.


SELECT-OPTIONS:
s_vrgar     FOR ce11100-vrgar OBLIGATORY,
s_kndnr     FOR ce11100-kndnr OBLIGATORY,
s_wwpce     FOR ce11100-wwpce OBLIGATORY.

SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_file    TYPE string
                      DEFAULT 'H:\SAPTEMP\REVBanner.csv',
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile.

SELECTION-SCREEN END OF BLOCK b1.

*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/cog/' INTO csvfile.

*************************************************************************
*************************************************************************



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

 "Begin of changes D30K932532
 "Making period/year dynamic for Selection screen variant.
AT SELECTION-SCREEN .
  lv_yr = sy-datum+00(04) .
  lv_mnth = sy-datum+04(02) - 01.

  IF lv_mnth = '0'.
    CLEAR : lv_mnth,
            lv_yr.
    lv_mnth = '12'.
    lv_yr = sy-datum+00(04) - 01.
  ENDIF.

  count =   strlen(   lv_mnth    ) .
  IF count eq 1.
    CONCATENATE lv_yr '00' lv_mnth INTO lv_data.
  ELSEIF count eq 2.
    CONCATENATE lv_yr '0' lv_mnth INTO lv_data.
  ENDIF.
  SELECT * FROM tvarvc INTO TABLE it_tvarvc WHERE name = 'ZBPCI027_PERIOD_YEAR' AND type = 'P'.
  LOOP AT it_tvarvc INTO wa_tvarvc.
    wa_tvarvc-low = lv_data.
    APPEND wa_tvarvc TO it_tvarvc1.
    CLEAR wa_tvarvc.
  ENDLOOP.
  MODIFY tvarvc FROM TABLE it_tvarvc1.
  clear : lv_data,
          lv_mnth.
*  "End of changes D30K932532 .

************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'cog.tch' INTO tuchfile.

  IF p_bukrs = 'UGL'.
    CONCATENATE csvfile 'SAP-' p_perio(4) '-' p_perio+5(2) '-Ban.csv' INTO csvfile.
  ELSEIF p_bukrs = 'EGD'.
    CONCATENATE csvfile 'SAP-EGD-' p_perio(4) '-' p_perio+5(2) '-Ban.csv' INTO csvfile.
  ENDIF.

  IF p_local = 'X'.
    lv_local = 1.
  ELSE.
    lv_local = 0.
  ENDIF.

  PERFORM get_db_data.
*  PERFORM sumarize_data.
  PERFORM print_report.
  IF lv_local = 0.
    PERFORM create_touch_file.
  ENDIF.
*----------------------------------------------------------------------*
FORM get_db_data.

  SELECT vrgar gjahr perde kndnr bukrs werks rbeln wwseg wwprg wwsub
         wwrat wwser wwsld wwpce wwrsn SUM( vvbvl ) SUM( vvbrv )
   INTO  TABLE t_ce11100_sum
      FROM ce11100
      WHERE paledger  = p_paledg
        AND vrgar     IN s_vrgar
        AND versi     = ''
        AND perio     = p_perio
        AND kndnr     IN s_kndnr
        AND bukrs     = p_bukrs
        AND kokrs     = '10'
        AND wwpce     IN s_wwpce
    GROUP BY vrgar gjahr perde kndnr bukrs werks rbeln wwseg wwprg
    wwsub wwrat wwser wwsld wwpce wwrsn.

ENDFORM.                    "get_db_data

*----------------------------------------------------------------------*
*FORM sumarize_data.
**add WWRSN in sort
*  SORT t_ce11100 ASCENDING BY
*                 gjahr perde wwser wwprg wwrat werks wwsld wwpce wwseg wwrsn.
*
*  CLEAR t_ce11100_sum.
*
*  LOOP AT t_ce11100 INTO s_ce11100.
*
*    AT END OF wwrsn.
*      SUM.
*      APPEND s_ce11100 TO t_ce11100_sum.
*    ENDAT.
*  ENDLOOP.
*
*ENDFORM.                    "sumarize_data

*----------------------------------------------------------------------*
FORM print_report.
  TYPES: BEGIN OF ty_bkpf,
           bukrs TYPE bkpf-bukrs,
           belnr TYPE bkpf-belnr,
           gjahr TYPE bkpf-gjahr,
           blart TYPE bkpf-blart,
         END OF ty_bkpf.
  DATA: ls_bkpf TYPE bkpf,
        lv_date1 TYPE sy-datum,
        lv_date2 TYPE sy-datum.

  DATA: t_bpcmap TYPE STANDARD TABLE OF zfit_bpcmaprev,
        w_bpcmap TYPE zfit_bpcmaprev,
        it_bkpf TYPE STANDARD TABLE OF ty_bkpf,
        w_bkpf TYPE ty_bkpf.

  REFRESH: t_bpcmap,it_bkpf.

  SELECT * FROM zfit_bpcmaprev INTO TABLE t_bpcmap.     "#EC CI_NOWHERE

  IF NOT t_ce11100_sum IS INITIAL.
    SELECT bukrs belnr gjahr blart INTO TABLE it_bkpf
        FROM bkpf
        FOR ALL ENTRIES IN t_ce11100_sum
        WHERE bukrs = t_ce11100_sum-bukrs
          AND belnr = t_ce11100_sum-rbeln
          AND gjahr = t_ce11100_sum-gjahr.
  ENDIF.
  CLEAR t_output.

*Set the hard-coded values.

  lv_category     = 'Actual' ##NO_TEXT.
  lv_entity       = p_bukrs.
  lv_intco        = 'No_IntCo'.
  lv_rptcurrency  = 'LC'.
  lv_projectst    = 'No_ProjectST'.


  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011 text-012 text-013
              INTO st_datarec SEPARATED BY c_delimtr.

  APPEND st_datarec TO t_data.

  DATA: lv_blart(2) TYPE c.

  LOOP AT t_ce11100_sum INTO s_ce11100.
    CLEAR:  st_datarec, lv_blart, lv_account, lv_time, lv_customer,w_bkpf,
            lv_dso, lv_service, lv_paths, lv_amount, ls_bkpf, lv_date2.

    READ TABLE it_bkpf INTO w_bkpf WITH KEY bukrs = s_ce11100-bukrs
                                            belnr = s_ce11100-rbeln
                                            gjahr = s_ce11100-gjahr.

    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'BLART'
                                               value = w_bkpf-blart. "'R5' OR 'S5' OR 'S6'
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.


    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWPCE'
                                               value = s_ce11100-wwpce. "390590/390595
    IF w_bpcmap-value1 IS NOT INITIAL.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWPCE'
                                               value = s_ce11100-wwpce "390595
                                               value1 = s_ce11100-kndnr "CISLVR200
                                               value2 = s_ce11100-vrgar. "F
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.
    IF w_bpcmap-datasrc IS NOT INITIAL.
      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWRSN'
                                                 value = s_ce11100-wwrsn "ZES, ZRV
                                                 value1 = s_ce11100-kndnr.
      IF sy-subrc = 0.
        lv_datasrc = w_bpcmap-datasrc.
      ELSE.
        lv_datasrc = 'SAP_Unbilled'.
      ENDIF.
    ELSE.
      lv_datasrc = 'SAP_Banner'.
    ENDIF.
    CONCATENATE s_ce11100-gjahr s_ce11100-perde+1(2) '00' INTO lv_time.


    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWSER'
                                               value = s_ce11100-wwser.
    IF w_bpcmap-customer IS NOT INITIAL.
      lv_customer = w_bpcmap-customer.
    ELSE.
      lv_customer = s_ce11100-wwser.
    ENDIF.

    lv_dso = s_ce11100-wwprg.


    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWRAT'
                                               value = s_ce11100-wwrat.
    IF w_bpcmap-customer IS NOT INITIAL.
      lv_customer = w_bpcmap-customer.
    ENDIF.
    IF w_bpcmap-bukrs IS NOT INITIAL.
      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWRAT'
                                                 value = s_ce11100-wwrat
                                                 bukrs = p_bukrs.
    ENDIF.
    IF w_bpcmap-service IS NOT INITIAL.
      lv_service = w_bpcmap-service.
    ELSE.
      lv_service = s_ce11100-wwrat.
    ENDIF.



    IF s_ce11100-wwsld = ''.
      IF w_bpcmap-paths IS NOT INITIAL.
        lv_paths = w_bpcmap-paths.
      ELSE.
        lv_paths = 'DA_S203'.
      ENDIF.
    ELSE.
      CONCATENATE 'DA_' s_ce11100-wwsld INTO lv_paths.
    ENDIF.
*Gas volume

    IF s_ce11100-vvbvl <> 0.
      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWSUB'
                                                 value = s_ce11100-wwsub
                                                 value1 = lv_service
                                                 value2 = lv_datasrc
                                                 bukrs = p_bukrs.
      IF sy-subrc NE 0.
        CLEAR w_bpcmap.
        READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWSUB'
                                                   value = s_ce11100-wwsub
                                                   value1 = ''.
      ENDIF.
      IF w_bpcmap-account IS NOT INITIAL AND w_bpcmap-account NE 'X'.
        lv_account = w_bpcmap-account.
*    ENDIF.
        lv_amount = s_ce11100-vvbvl.

        s_output-account      = lv_account.
        s_output-category     = lv_category.
        s_output-datasrc      = lv_datasrc.
        s_output-entity       = lv_entity.
        s_output-intco        = lv_intco.
        s_output-rptcurrency  = lv_rptcurrency.
        s_output-time         = lv_time.
        s_output-customer     = lv_customer.
        s_output-dso          = lv_dso.
        s_output-service      = lv_service.
        s_output-paths        = lv_paths.
        s_output-projectst    = lv_projectst.
        s_output-amount       = lv_amount.

        APPEND s_output TO t_output.
      ENDIF.
    ENDIF.

*Revenue
    IF s_ce11100-vvbrv <> 0.

      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWSUB'
                                                 value = s_ce11100-wwsub.

      IF w_bpcmap-account = 'X'.  " s_ce11100-wwsub = 'SS'
        CLEAR w_bpcmap.
        READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWRAT'
                                                   value = s_ce11100-wwrat.
        IF w_bpcmap-account IS NOT INITIAL.
          CONCATENATE s_ce11100-wwpce+4 '_' s_ce11100-wwsub '_GS' INTO lv_account.
        ELSE.
          CONCATENATE s_ce11100-wwpce+4 '_' s_ce11100-wwsub '_Del' INTO lv_account.
        ENDIF.

      ELSE.
        CONCATENATE s_ce11100-wwpce+4 '_' s_ce11100-wwsub INTO lv_account.
      ENDIF.

      lv_amount = s_ce11100-vvbrv.

      s_output-account      = lv_account.
      s_output-category     = lv_category.
      s_output-datasrc      = lv_datasrc.
      s_output-entity       = lv_entity.
      s_output-intco        = lv_intco.
      s_output-rptcurrency  = lv_rptcurrency.
      s_output-time         = lv_time.
      s_output-customer     = lv_customer.
      s_output-dso          = lv_dso.
      s_output-service      = lv_service.
      s_output-paths        = lv_paths.
*      s_output-projectst    = lv_projectst.
      s_output-amount       = lv_amount.

      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWSEG'
                                                 value = s_ce11100-wwseg.
      IF w_bpcmap-projectst IS NOT INITIAL.
        s_output-projectst  = w_bpcmap-projectst.
      ELSE.
        s_output-projectst    = lv_projectst.
      ENDIF.

      APPEND s_output TO t_output.

    ENDIF.

  ENDLOOP.

*Summarize the final data.
  SORT t_output ASCENDING BY account time customer dso service datasrc paths.
*  SORT t_output ASCENDING BY account time customer dso service paths.

  CLEAR t_output_sum.

  LOOP AT t_output INTO s_output.
*Last sorted field is paths
    AT END OF paths.
      SUM.
      APPEND s_output TO t_output_sum.
    ENDAT.
  ENDLOOP.


*Add output table to the csvfile.
  LOOP AT t_output_sum INTO s_output.

    IF s_output-account = 'GenServ_vol' OR
       s_output-account = 'D_1113_vol'  OR
       s_output-account = 'GenServ_vol_C1' OR
       s_output-account = 'GenServ_vol_C2' .
      IF s_output-amount < 0.                               "SDP60315
        s_output-amount = s_output-amount * -1.             "SDP60315
        lv_amount = s_output-amount.                        "SDP60315
        SHIFT lv_amount LEFT DELETING LEADING ' '.          "SDP60315
        CONCATENATE '-' lv_amount INTO lv_amount.           "SDP60315
      ELSE.                                                 "SDP60315
        lv_amount = s_output-amount.                        "SDP60315
        SHIFT lv_amount LEFT DELETING LEADING ' '.          "SDP60315
      ENDIF.                                                "SDP60315
                                                            "SDP60315
    ELSE.                                                   "SDP60315
                                                            "SDP60315
      IF s_output-amount < 0.                               "SDP60315
        s_output-amount = s_output-amount * -1.             "SDP60315
        MOVE s_output-amount TO w_dol_amt.                  "SDP60315
        lv_amount = w_dol_amt.                              "SDP60315
        SHIFT lv_amount LEFT DELETING LEADING ' '.          "SDP60315
        CONCATENATE '-' lv_amount INTO lv_amount.           "SDP60315
      ELSE.                                                 "SDP60315
        MOVE s_output-amount TO w_dol_amt.                  "SDP60315
        lv_amount = w_dol_amt.                              "SDP60315
        SHIFT lv_amount LEFT DELETING LEADING ' '.          "SDP60315
      ENDIF.                                                "SDP60315
                                                            "SDP60315
    ENDIF.                                                  "SDP60315

    CONCATENATE   s_output-account s_output-category s_output-datasrc
                  s_output-entity  s_output-intco s_output-rptcurrency
                  s_output-time s_output-customer s_output-dso
                  s_output-service s_output-paths s_output-projectst
                  lv_amount
                  INTO st_datarec SEPARATED BY c_delimtr.

    APPEND st_datarec TO t_data.
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

  TRANSFER text-016 TO tuchfile.

  CLOSE DATASET tuchfile.
  IF sy-subrc NE '0'.
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
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
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
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
