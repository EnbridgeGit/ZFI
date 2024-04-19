REPORT  zbpci026_cogactual MESSAGE-ID zs.
************************************************************************
*  Date:       June 2021                                               *
*  Author:     Jaydeep Waychal                                         *
*  Program Description:                                                *
*  This program will extract data for COG actuals for                  *
*  the BPC application.                                                *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Date     Issue  By      Description                                   *

************************************************************************
TABLES: coep, coepr, ce11100, faglflext.

TYPES:  BEGIN OF ty_coep,
          perio   LIKE coep-perio,
          gjahr   LIKE coep-gjahr,
          bukrs   LIKE coep-bukrs,
          versn   LIKE coep-versn,
          wrttp   LIKE coep-wrttp,
          objnr   LIKE coep-objnr,
          kstar   LIKE coep-kstar,
          wogbtr  LIKE coep-wogbtr,
          belnr   LIKE coep-belnr,
          kokrs   LIKE coep-kokrs,
          del_rec TYPE char01,
        END OF ty_coep,

         BEGIN OF ty_coepr,
          perio   LIKE coepr-perio,
          gjahr   LIKE coepr-gjahr,
          versn   LIKE coepr-versn,
          wrttp   LIKE coepr-wrttp,
          objnr   LIKE coepr-objnr,
          stagr   LIKE coepr-stagr,
          smebtr  LIKE coepr-smebtr,
        END OF ty_coepr,

         BEGIN OF ty_coepl,
          perio   LIKE coepl-perio,
          gjahr   LIKE coepl-gjahr,
          versn   LIKE coepl-versn,
          wrttp   LIKE coepl-wrttp,
          objnr   LIKE coepl-objnr,
          lstbtr  LIKE coepl-lstbtr,
        END OF ty_coepl.


DATA: BEGIN OF ty_ce11100,
        gjahr       LIKE ce11100-gjahr,
        perde       LIKE ce11100-perde,
        werks       LIKE ce11100-werks,
        wwprg       LIKE ce11100-wwprg,
        wwrat       LIKE ce11100-wwrat,
        wwser       LIKE ce11100-wwser,
        wwsld       LIKE ce11100-wwsld,
        vvcgg       LIKE ce11100-vvcgg,
      END OF   ty_ce11100.

DATA: lv_local      TYPE integer,
      lv_perio      TYPE ce11100-perio,
      wa_coep       LIKE coep,
      s_coep        TYPE ty_coep,
      t_cobk        TYPE TABLE OF cobk,
      s_cobk        TYPE cobk,
      t_coep        LIKE TABLE OF s_coep,
      t_coep_sum    LIKE TABLE OF s_coep,
      s_ce11100     LIKE ty_ce11100,
      t_ce11100     LIKE TABLE OF s_ce11100,
      t_ce11100_sum LIKE TABLE OF s_ce11100,
      s_coepr       TYPE ty_coepr,
      t_coepr       LIKE TABLE OF s_coepr,
      t_coepr_sum   LIKE TABLE OF s_coepr,
      s_coepl       TYPE ty_coepl,
      t_coepl       LIKE TABLE OF s_coepl,
      t_coepl_sum   LIKE TABLE OF s_coepl,
      st_datarec    TYPE string,
      t_data        LIKE TABLE OF st_datarec.

DATA: msg(80)           TYPE c,
      lv_account(11)    TYPE c,
      lv_category(6)    TYPE c,
      lv_datasrc(9)     TYPE c,
      lv_entity(15)     TYPE c,
      lv_intco(8)       TYPE c,
      lv_rptcurrency(2) TYPE c,
      lv_time(8)        TYPE c,
      lv_customer(11)   TYPE c,
      lv_dso(6)         TYPE c,
      lv_service(10)    TYPE c,
      lv_paths(11)      TYPE c,
      lv_projectst(12)  TYPE c,
      lv_amount(20)     TYPE c.

DATA: tuchfile          LIKE rfpdo-rfbifile.

DATA: t_csks_coepr TYPE STANDARD TABLE OF csks,
      t_csks_coepl TYPE STANDARD TABLE OF csks,
      s_csks TYPE csks.

CONSTANTS: c_delimtr  TYPE c VALUE ','.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

PARAMETERS:
p_bukrs   TYPE t001-bukrs OBLIGATORY,
p_perio   LIKE ce11100-perio OBLIGATORY.

SELECT-OPTIONS:
s_objnr   FOR coep-objnr    OBLIGATORY,
s_wrttp   FOR coep-wrttp    OBLIGATORY DEFAULT '4',
s_vrgar   FOR ce11100-vrgar OBLIGATORY,
s_kstar   FOR coep-kstar,
s_versn   FOR coep-versn,
s_stagr   FOR coepr-stagr,
s_kndnr     FOR ce11100-kndnr.

SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS:
            p_local   RADIOBUTTON GROUP rad1 DEFAULT 'X',
            p_file    TYPE        string DEFAULT 'H:\SAPTEMP\COGActual.csv',
            p_server  RADIOBUTTON GROUP rad1,
            csvfile   LIKE        rfpdo-rfbifile.
SELECTION-SCREEN END OF BLOCK b1.

*************************************************************************
*************************************************************************
INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/cog/' INTO csvfile.

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

*************************************************************************
START-OF-SELECTION.

  CONCATENATE csvfile 'cog.tch' INTO tuchfile.

  IF p_bukrs = 'EGD'.
    CONCATENATE csvfile 'SAP-EGD-' p_perio(4) '-' p_perio+5(2) '-COG.csv' INTO csvfile.
  ELSEIF p_bukrs = 'UGL'.
    CONCATENATE csvfile 'SAP-' p_perio(4) '-' p_perio+5(2) '-COG.csv' INTO csvfile.
  ENDIF.

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

  CLEAR: t_cobk.

  SELECT perio objnr gjahr wrttp kstar versn wogbtr bukrs
         belnr kokrs
    INTO CORRESPONDING FIELDS OF TABLE t_coep
    FROM coep
    WHERE perio =  p_perio+4(3)
      AND objnr IN s_objnr
      AND gjahr =  p_perio(4)
      AND wrttp IN s_wrttp
      AND kstar IN s_kstar
      AND versn IN s_versn
      AND bukrs = p_bukrs.
  IF t_coep[] IS NOT INITIAL.
    SELECT * FROM cobk INTO TABLE t_cobk
      FOR ALL ENTRIES IN t_coep
      WHERE kokrs = t_coep-kokrs
        AND belnr = t_coep-belnr.
  ENDIF.

  SELECT perio objnr gjahr wrttp stagr versn smebtr
    INTO CORRESPONDING FIELDS OF TABLE t_coepr
    FROM coepr
    WHERE perio = p_perio+4(3)
      AND gjahr = p_perio(4)
      AND stagr IN s_stagr.                             "#EC CI_NOFIRST

  IF sy-subrc = 0.
    SELECT * FROM csks INTO TABLE t_csks_coepr
      FOR ALL ENTRIES IN t_coepr
      WHERE kokrs = '10' AND
      kostl = t_coepr-objnr+6(10)
      AND bukrs = p_bukrs.
  ENDIF.

  SELECT perio objnr gjahr wrttp versn lstbtr
    INTO CORRESPONDING FIELDS OF TABLE t_coepl
    FROM coepl
    WHERE perio = p_perio+4(3)
      AND gjahr = p_perio(4).                           "#EC CI_NOFIRST

  IF sy-subrc = 0.
    SELECT * FROM csks INTO TABLE t_csks_coepl
      FOR ALL ENTRIES IN t_coepl
      WHERE kokrs = '10' AND
      kostl = t_coepl-objnr+6(10)
      AND bukrs = p_bukrs.
  ENDIF.

  SELECT gjahr perde werks wwprg wwrat wwser wwsld sum( vvcgg )
    INTO TABLE t_ce11100_sum
    FROM ce11100
    WHERE paledger  = '01'
      AND vrgar     IN s_vrgar
      AND versi     = ''
      AND perio     = p_perio
      AND kndnr     IN s_kndnr
      AND artnr     = 'NATGAS'
      AND bukrs     = p_bukrs
      AND wwprg     = 'SY'
    GROUP BY gjahr perde werks wwprg wwrat wwser wwsld.
ENDFORM.                    "get_db_data

*----------------------------------------------------------------------*
FORM sumarize_data.

  CONSTANTS: lc_390686 TYPE coep-kstar VALUE '0000390686'.
  DATA: ls_cobk TYPE cobk.

  SORT t_coep ASCENDING BY gjahr perio kstar.

  LOOP AT t_coep INTO s_coep.
    IF s_coep-kstar = lc_390686.
      CLEAR ls_cobk.
      READ TABLE t_cobk INTO ls_cobk WITH KEY kokrs = s_coep-kokrs
                                              belnr = s_coep-belnr.
      IF ls_cobk-blart = 'WE'.
        s_coep-del_rec = 'X'.
        MODIFY t_coep FROM s_coep TRANSPORTING del_rec.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE t_coep WHERE del_rec = 'X'.
  SORT t_coep ASCENDING BY gjahr perio kstar.
  LOOP AT t_coep INTO s_coep.
*Last sorted field is kstar
    AT END OF kstar.
      SUM.
      APPEND s_coep TO t_coep_sum.
    ENDAT.
  ENDLOOP.

  SORT t_coepr ASCENDING BY gjahr perio stagr.

  LOOP AT t_coepr INTO s_coepr.
    CLEAR s_csks.
    READ TABLE t_csks_coepr INTO s_csks WITH KEY kostl = s_coepr-objnr+6(10).
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    AT END OF stagr.
      SUM.
      APPEND s_coepr TO t_coepr_sum.
    ENDAT.
  ENDLOOP.

  SORT t_coepl ASCENDING BY gjahr perio objnr.

  LOOP AT t_coepl INTO s_coepl.
    CLEAR s_csks.
    READ TABLE t_csks_coepl INTO s_csks WITH KEY kostl = s_coepl-objnr+6(10).
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    AT END OF objnr.
      SUM.
      APPEND s_coepl TO t_coepl_sum.
    ENDAT.
  ENDLOOP.

*  SORT t_ce11100 ASCENDING BY gjahr perde werks wwrat wwser wwprg wwsld.
*
*  LOOP AT t_ce11100 INTO s_ce11100.
**Last sorted field is wwsld
*    AT END OF wwsld.
*      SUM.
*      APPEND s_ce11100 TO t_ce11100_sum.
*    ENDAT.
*  ENDLOOP.

ENDFORM.                    "sumarize_data

*----------------------------------------------------------------------*
FORM print_report.

  CONSTANTS: lc_390686 TYPE coep-kstar VALUE '0000390686'.

  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008 text-009 text-010
              text-011 text-012 text-013
              INTO st_datarec SEPARATED BY c_delimtr.

  APPEND st_datarec TO t_data.

*Set the hard-coded values.

  lv_category     = 'Actual' ##NO_TEXT.
  lv_datasrc      = 'SAP_COG'.
  lv_rptcurrency  = 'LC'.
  lv_customer     = 'No_Customer'.
  lv_dso          = 'No_DSO'.
  lv_service      = 'No_Service'.
  lv_projectst    = 'No_ProjectST'.

  DATA: lv_poski(16) TYPE c.
  DATA: t_bpcmap TYPE STANDARD TABLE OF zfit_bpcmapcog,
        w_bpcmap TYPE zfit_bpcmapcog.

  REFRESH: t_bpcmap.

  SELECT * FROM zfit_bpcmapcog INTO TABLE t_bpcmap.     "#EC CI_NOWHERE

  LOOP AT t_coep_sum INTO s_coep.
    CLEAR:  st_datarec, lv_account, lv_entity, lv_paths, lv_intco, lv_time,
            lv_amount, w_bpcmap.

    lv_account = s_coep-kstar+4.
* Entity & Path based on Object Number (OBJNR)
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'OBJNR'
                                               value = s_coep-objnr(2).
    IF w_bpcmap-entity IS INITIAL.
      lv_entity = p_bukrs.
    ELSE.
      lv_entity = s_coep-objnr+6(10).
      SHIFT lv_entity LEFT DELETING LEADING '0'.
    ENDIF.

    IF w_bpcmap-paths = 'X'.
      CONCATENATE 'IO_' s_coep-objnr+8 INTO lv_paths.
      SHIFT lv_paths LEFT DELETING LEADING '0'.
    ELSEIF w_bpcmap-paths IS NOT INITIAL AND w_bpcmap-paths NE 'X'.
      lv_paths  = w_bpcmap-paths.
    ELSE.
      lv_paths  = 'No_COGPaths'.
    ENDIF.

* Customer. Service based on Cost Center (KSTAR)
    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'KSTAR'
                                               value = s_coep-kstar. "'0000390686'
    IF sy-subrc NE 0.
      lv_customer     = 'No_Customer'.
      lv_service      = 'No_Service'.
    ELSE.
      lv_customer  = w_bpcmap-customer. "'COGYCR'.
      lv_service   = w_bpcmap-service.  "'YCR_Serv'.
      lv_paths     = w_bpcmap-paths.    "'No_PATHS'.
    ENDIF.


    SELECT SINGLE affil
      INTO lv_intco
      FROM zacctnew
      WHERE cocode  = s_coep-bukrs
        AND glacct  = s_coep-kstar .

    IF lv_intco = ''.
      SELECT SINGLE affil
        INTO lv_intco
        FROM zacctnew
        WHERE cocode  = ''
          AND glacct  = s_coep-kstar.
      IF lv_intco = ''.
        lv_intco = 'No_IntCo'.
      ELSE.
        CONCATENATE 'IC_' lv_intco INTO lv_intco.
      ENDIF.
    ELSE.
      CONCATENATE 'IC_' lv_intco INTO lv_intco.
    ENDIF.

    CONCATENATE s_coep-gjahr s_coep-perio+1(2) '00' INTO lv_time.

    IF s_coep-wogbtr < 0.
      "If negative convert to positive and add '-' infront.
      s_coep-wogbtr = s_coep-wogbtr * -1.
      lv_amount = s_coep-wogbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_coep-wogbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.

    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
                lv_service lv_paths lv_projectst lv_amount
                INTO st_datarec SEPARATED BY c_delimtr.

    IF s_coep-wogbtr <> 0.
      APPEND st_datarec TO t_data.
    ENDIF.

  ENDLOOP.

  LOOP AT t_coepr_sum INTO s_coepr.
    CLEAR:  st_datarec, lv_account, lv_entity, lv_paths, lv_intco, lv_time, lv_amount.

    lv_rptcurrency  = 'GJ'.

* ACCOUNT based on Statistical Group (STAGR)
    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'STAGR'
                                               value = s_coepr-stagr.
    IF w_bpcmap-account IS INITIAL. " Statistical Group (STAGR) not availabe check OBJNR
      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'OBJNR'
                                                 value = s_coepr-objnr+11(2). " 78 or 73
      IF w_bpcmap-account IS INITIAL. "if OBJNR not available
        lv_account = 'MISADJ'.
      ELSE.
        lv_account = w_bpcmap-account.  "SKF
      ENDIF.
    ELSE.
      lv_account = w_bpcmap-account.  " 'COG_SUPPLY'
    ENDIF.

* Entity & Path based on Object Number (OBJNR)
    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'OBJNR'
                                               value = s_coepr-objnr(2).
    IF w_bpcmap-entity IS INITIAL.
      lv_entity = p_bukrs.
    ELSE.
      lv_entity = s_coepr-objnr+6(10).
      SHIFT lv_entity LEFT DELETING LEADING '0'.
    ENDIF.

    IF w_bpcmap-paths = 'X'.
      CONCATENATE 'IO_' s_coepr-objnr+8 INTO lv_paths.
      SHIFT lv_paths LEFT DELETING LEADING '0'.
    ELSEIF w_bpcmap-paths IS NOT INITIAL AND w_bpcmap-paths NE 'X'.
      lv_paths  = w_bpcmap-paths.
    ELSE.
      lv_paths  = 'No_COGPaths'.
    ENDIF.


    lv_intco = 'No_IntCo'.

    CONCATENATE s_coepr-gjahr s_coepr-perio+1(2) '00' INTO lv_time.

    IF s_coepr-smebtr < 0.
      "If negative convert to positive and add '-' infront.
      s_coepr-smebtr = s_coepr-smebtr * -1.
      lv_amount = s_coepr-smebtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_coepr-smebtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.

    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
                lv_service lv_paths lv_projectst lv_amount
                INTO st_datarec SEPARATED BY c_delimtr.

    IF s_coepr-smebtr <> 0.
      APPEND st_datarec TO t_data.
    ENDIF.

  ENDLOOP.

  LOOP AT t_coepl_sum INTO s_coepl.
    CLEAR:  st_datarec, lv_account, lv_entity, lv_paths, lv_intco, lv_time, lv_amount.

    lv_rptcurrency  = 'GJ'.
* Account based on Object Number (OBJNR)
    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'OBJNR'
                                               value = s_coepl-objnr+16(6).
    IF w_bpcmap-account IS INITIAL.
      CONTINUE.
    ELSE.
      lv_account = w_bpcmap-account.
    ENDIF.

* Entity & Path based on Object Number (OBJNR)
    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'OBJNR'
                                               value = s_coepl-objnr(2).
    IF w_bpcmap-entity IS INITIAL.
      lv_entity = p_bukrs.
    ELSE.
      lv_entity = s_coepl-objnr+6(10).
      SHIFT lv_entity LEFT DELETING LEADING '0'.
    ENDIF.

    IF w_bpcmap-paths = 'X'.
      CONCATENATE 'IO_' s_coepl-objnr+8 INTO lv_paths.
      SHIFT lv_paths LEFT DELETING LEADING '0'.
    ELSEIF w_bpcmap-paths IS NOT INITIAL AND w_bpcmap-paths NE 'X'.
      lv_paths  = w_bpcmap-paths.
    ELSE.
      lv_paths  = 'No_COGPaths'.
    ENDIF.

    lv_intco = 'No_IntCo'.

    CONCATENATE s_coepl-gjahr s_coepl-perio+1(2) '00' INTO lv_time.

    IF s_coepl-lstbtr < 0.
      "If negative convert to positive and add '-' infront.
      s_coepl-lstbtr = s_coepl-lstbtr * -1.
      lv_amount = s_coepl-lstbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_coepl-lstbtr.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.

    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
                lv_service lv_paths lv_projectst lv_amount
                INTO st_datarec SEPARATED BY c_delimtr.

    IF s_coepl-lstbtr <> 0.
      APPEND st_datarec TO t_data.
    ENDIF.

  ENDLOOP.
************************
** Append CE111100 Data.
************************
*Set the hard-coded values.
  lv_category     = 'Actual' ##NO_TEXT.
  lv_datasrc      = 'SAP_COG'.
  lv_entity       = p_bukrs.
  lv_intco        = 'No_IntCo'.
  lv_rptcurrency  = 'LC'.
  lv_projectst    = 'No_ProjectST'.

  LOOP AT t_ce11100_sum INTO s_ce11100.

    CLEAR: lv_account, lv_time, lv_customer, lv_dso, lv_service, lv_paths, lv_amount.

    CONCATENATE s_ce11100-gjahr s_ce11100-perde+1(2) '00' INTO lv_time.

    "Account 302002/302003
* Customer based on Plant (WERKS), Rate Class(WWRAT), Service(WWSER)
    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WERKS'
                                               value = s_ce11100-werks.
    IF w_bpcmap-customer IS INITIAL.
      lv_customer = s_ce11100-wwser.
    ELSE.
      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWRAT'
                                                 value = s_ce11100-wwrat.
      IF w_bpcmap-customer IS INITIAL.
        IF s_ce11100-wwser IS INITIAL.
          lv_customer = 'No_Customer_GS'.
        ELSE.
          lv_customer = 'No_Customer_Cont'.
        ENDIF.
      ELSE.
        CLEAR w_bpcmap.
        READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWSER'
                                                   value = s_ce11100-wwser.
        IF w_bpcmap-customer IS INITIAL.
          lv_customer = 'No_Customer_GS'.
        ELSE.
          lv_customer = s_ce11100-wwser.
        ENDIF.
      ENDIF.
    ENDIF.


    lv_dso = s_ce11100-wwprg.
* Service based on Rate Class (WWRAT)
    IF s_ce11100-wwrat IS INITIAL.
      lv_service = 'No_Service_GS'.
    ELSE.
      CLEAR w_bpcmap.
      READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WWRAT'
                                                 value = s_ce11100-wwrat.
      IF NOT w_bpcmap-service IS INITIAL.
        lv_service = w_bpcmap-service.
      ELSE.
        lv_service = s_ce11100-wwrat.
      ENDIF.
    ENDIF.


    CONCATENATE 'DA_' s_ce11100-wwsld INTO lv_paths.
* Account, Entity based on Plant (WERKS)
    CLEAR w_bpcmap.
    READ TABLE t_bpcmap INTO w_bpcmap WITH KEY field = 'WERKS'
                                               value = s_ce11100-werks.
    IF w_bpcmap-account IS INITIAL.
      CONCATENATE 'UNK_' s_ce11100-werks INTO lv_account.
    ELSE.
      lv_account = w_bpcmap-account.
      lv_entity  = w_bpcmap-entity.
    ENDIF.


    IF s_ce11100-vvcgg < 0.
      "If negative convert to positive and add '-' infront.
      s_ce11100-vvcgg = s_ce11100-vvcgg * -1.
      lv_amount = s_ce11100-vvcgg.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
      CONCATENATE '-' lv_amount INTO lv_amount.
    ELSE.
      lv_amount = s_ce11100-vvcgg.
      SHIFT lv_amount LEFT DELETING LEADING ' '.
    ENDIF.

    CONCATENATE lv_account lv_category lv_datasrc lv_entity
                lv_intco lv_rptcurrency lv_time lv_customer lv_dso
                lv_service lv_paths lv_projectst lv_amount
                INTO st_datarec SEPARATED BY c_delimtr.

    IF lv_amount EQ '0.00' OR lv_amount EQ '0'.
      " DO NOTHING
    ELSE.
      APPEND st_datarec TO t_data.
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
*    WRITE: 'File Output Successfull to: ', p_file.
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
    MESSAGE e019 WITH text-016 csvfile msg.
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
  TRANSFER text-015 TO tuchfile.

  CLOSE DATASET tuchfile.
  IF sy-subrc NE '0'.
*    MESSAGE e019 WITH 'unsuccessfl close' tuchfile msg.
    MESSAGE e019 WITH text-016 tuchfile msg.
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
