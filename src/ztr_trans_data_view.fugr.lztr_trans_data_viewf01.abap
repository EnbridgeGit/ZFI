*&---------------------------------------------------------------------*
*&  Include           LZTR_TRANS_DATA_VIEWF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       Fetch Data
*----------------------------------------------------------------------*
FORM f_get_data.

* Get Transaction data
  SELECT *
    FROM vtbfha
    INTO TABLE git_vtbfha
    WHERE bukrs IN gr_bukrs AND
          rfha IN gr_rfha AND
          (  sanlf EQ gc_fixed_term
       OR    sanlf EQ gc_dep_notice )
      AND (  saktiv EQ gc_active
       OR    saktiv EQ gc_concluded ).

  CHECK NOT git_vtbfha IS INITIAL.

  SORT  git_vtbfha BY bukrs ASCENDING
                      rfha ASCENDING.

* Get logical systme of the client
  SELECT SINGLE *
    FROM t000
    INTO gwa_t000
    WHERE mandt EQ sy-mandt.

*   Get currency for company code selected
  SELECT bukrs waers
    FROM t001
    INTO TABLE git_t001
    FOR ALL ENTRIES IN git_vtbfha
    WHERE bukrs EQ git_vtbfha-bukrs.

  CHECK NOT git_t001 IS INITIAL.
  SORT git_t001 BY bukrs.

* get Transaction Flow
  SELECT *
    FROM vtbfhapo
    INTO TABLE git_vtbfhapo
    FOR ALL ENTRIES IN git_vtbfha
    WHERE  bukrs EQ git_vtbfha-bukrs
      AND  rfha  EQ git_vtbfha-rfha.

  CHECK NOT git_vtbfhapo IS INITIAL.

  SELECT  bukrs
          rfha
          rfhazu
          nordext
    FROM vtbfhazu
    INTO TABLE git_vtbfhazu
    FOR ALL ENTRIES IN git_vtbfhapo
    WHERE bukrs EQ git_vtbfhapo-bukrs
      AND rfha  EQ git_vtbfhapo-rfha
      AND rfhazu EQ git_vtbfhapo-rfhazu.

  CHECK NOT git_vtbfhazu IS INITIAL.
  SORT git_vtbfhazu BY bukrs rfha rfhazu.

ENDFORM.                    " F_GET_DATA

*eject
*&---------------------------------------------------------------------*
*&      Form  F_FILL_OUTPUT
*&---------------------------------------------------------------------*
*       fill output
*----------------------------------------------------------------------*
FORM f_fill_output CHANGING pi_data_view TYPE zfitr_tran.

  DATA: lv_odd_even    TYPE char01 ,
        lv_number(5)   TYPE n,
        lv_checked     TYPE char01,
        lv_rem         TYPE i,
        lv_rfhazu      TYPE vtbfhapo-rfhazu,
        lv_rfha_last   TYPE vtbfhapo-rfha,
        lv_rfha_new    TYPE vtbfhapo-rfha,
        lv_flag(1)     TYPE c.

* Get the highest number of transaction activity
  SORT: git_vtbfhapo BY bukrs rfha ASCENDING rfhazu DESCENDING,
        git_vtbfha BY bukrs rfha DESCENDING.

  LOOP AT git_vtbfhapo INTO gwa_vtbfhapo.

*    CLEAR lv_rfhazu.
*    lv_rfhazu = gwa_vtbfhapo-rfhazu.
** If..Endif block to determine the Transaction is different from previous iteration
*    IF sy-tabix = 1.
*      lv_rfha_new = gwa_vtbfhapo-rfha.
*    ELSEIF lv_rfha_new NE gwa_vtbfhapo-rfha.
*      lv_rfha_new = gwa_vtbfhapo-rfha.
*    ENDIF.
*
** Check if the highest number is odd or even
*    IF lv_rfha_last NE lv_rfha_new.
*      lv_rfha_last = lv_rfha_new.
*      CLEAR : lv_rem,
*              lv_number,
*              lv_checked,
*              lv_odd_even.
*      lv_rem = lv_rfhazu MOD 2.
**   Odd number
*      IF lv_rem NE 0.
*        lv_odd_even = gc_true.
*      ENDIF.
*      lv_number = lv_rfhazu.
*    ENDIF.
*
** If..Endif block to determine the finalize the activity figure
** This figure will be carried for further condition checking
*    IF lv_number NE gwa_vtbfhapo-rfhazu
*      AND lv_number GT gwa_vtbfhapo-rfhazu.
*
*      IF NOT lv_checked IS INITIAL.
*        lv_number = lv_number - 2.
*      ELSE.
*        IF NOT lv_odd_even IS INITIAL.
*          lv_number = lv_number - 1.
*        ELSE.
*          lv_number = lv_number - 2.
*        ENDIF.
*        lv_checked = gc_true.
*      ENDIF.
*    ENDIF.
*
**   Get records as required
*    IF lv_number EQ gwa_vtbfhapo-rfhazu.
      READ TABLE git_vtbfha INTO gwa_vtbfha WITH KEY bukrs = gwa_vtbfhapo-bukrs
                                                     rfha  = gwa_vtbfhapo-rfha.

      IF ( sy-subrc = 0  ) AND ( ( gwa_vtbfha-RFHAZUL eq gwa_vtbfhapo-rfhazu AND ( GWA_VTBFHAPO-SSPRGRD  EQ 3  OR GWA_VTBFHAPO-SSPRGRD  EQ 0 ) )  OR  ( gwa_vtbfha-RFHAZUL NE gwa_vtbfhapo-rfhazu AND GWA_VTBFHAPO-SSPRGRD  EQ 0 ) ) .
        MOVE:     gwa_vtbfha-kontrh     TO gwa_tranview-kontrh,
                  gwa_vtbfha-upuser     TO gwa_tranview-upuser,
                  gwa_vtbfha-dupdat     TO gwa_tranview-dupdat,
                  gwa_vtbfha-tuptim     TO gwa_tranview-tuptim,
                  gwa_vtbfha-sanlf      TO gwa_tranview-sanlf,
                  gwa_vtbfha-rportb     TO gwa_tranview-rportb,
                  gwa_vtbfha-dblfz      TO gwa_tranview-dblfz,
                  gwa_vtbfha-skalid     TO gwa_tranview-skalid,
                  gwa_vtbfha-skalid2    TO gwa_tranview-skalid2,
                  gwa_vtbfha-delfz      TO gwa_tranview-delfz,
                  gwa_vtbfha-zuond      TO gwa_tranview-zuond,
                  gwa_vtbfha-refer      TO gwa_tranview-refer,
                  gwa_vtbfha-rcomvalcl  TO gwa_tranview-rcomvalcl,
                  gwa_vtbfha-wgschft    TO gwa_tranview-wgschft,
                  gwa_vtbfha-sgsart     TO gwa_tranview-sgsart,
                  gwa_vtbfha-sfhaart    TO gwa_tranview-sfhaart.
*      ENDIF.
      READ TABLE git_t001 INTO gwa_t001 WITH KEY bukrs = gwa_vtbfhapo-bukrs BINARY SEARCH.

      IF sy-subrc = 0.
        MOVE: gwa_t001-waers TO gwa_tranview-waers.
      ENDIF.

      READ TABLE git_vtbfhazu INTO gwa_vtbfhazu WITH KEY bukrs  = gwa_vtbfhapo-bukrs
                                                         rfha   = gwa_vtbfhapo-rfha
                                                         rfhazu = gwa_vtbfhapo-rfhazu
                                                BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE  gwa_vtbfhazu-nordext  TO gwa_tranview-nordext.
      ENDIF.
      MOVE  gwa_t000-logsys       TO gwa_tranview-logsys.

      MOVE: gwa_vtbfhapo-mandt     TO gwa_tranview-mandt,
            gwa_vtbfhapo-bukrs     TO gwa_tranview-bukrs,
            gwa_vtbfhapo-sfhazba   TO gwa_tranview-sfhazba,
            gwa_vtbfhapo-sbkklas   TO gwa_tranview-sbkklas,
            gwa_vtbfhapo-ssign     TO gwa_tranview-ssign,
            gwa_vtbfhapo-sbewebe   TO gwa_tranview-sbewebe,
            gwa_vtbfhapo-skoart    TO gwa_tranview-skoart,
            gwa_vtbfhapo-pkond     TO gwa_tranview-pkond,
            gwa_vtbfhapo-bnwhr     TO gwa_tranview-bnwhr,
            gwa_vtbfhapo-dfaell    TO gwa_tranview-dfaell,
            gwa_vtbfhapo-rfhazu    TO gwa_tranview-rfhazu,
            gwa_vtbfhapo-nstufe    TO gwa_tranview-nstufe,
            gwa_vtbfhapo-atage     TO gwa_tranview-atage,
            gwa_vtbfhapo-rfha      TO gwa_tranview-rfha,
            gwa_vtbfhapo-cruser    TO gwa_tranview-cruser,
            gwa_vtbfhapo-dcrdat    TO gwa_tranview-dcrdat,
            gwa_vtbfhapo-tuptim    TO gwa_tranview-tuptim,
            gwa_vtbfhapo-rpzahl    TO gwa_tranview-rpzahl,
            gwa_vtbfhapo-rpbank    TO gwa_tranview-rpbank,
            gwa_vtbfhapo-prkey     TO gwa_tranview-prkey,
            gwa_vtbfhapo-belnr     TO gwa_tranview-belnr,
            gwa_vtbfhapo-belnr2    TO gwa_tranview-belnr2,
            gwa_vtbfhapo-dzterm    TO gwa_tranview-dzterm,
            gwa_vtbfhapo-wprice    TO gwa_tranview-wprice,
            gwa_vtbfhapo-dbuchung  TO gwa_tranview-dbuchung,
            gwa_vtbfhapo-rfhazb    TO gwa_tranview-rfhazb,
            gwa_vtbfhapo-bzbetr    TO gwa_tranview-bzbetr,
            gwa_vtbfhapo-khwkurs   TO gwa_tranview-khwkurs,
            gwa_vtbfhapo-bhwbetr   TO gwa_tranview-bhwbetr,
            gwa_vtbfhapo-rkondgr   TO gwa_tranview-rkondgr,
            gwa_vtbfhapo-szbmeth   TO gwa_tranview-szbmeth,
            gwa_vtbfhapo-sherkunft TO gwa_tranview-sherkunft,
            gwa_vtbfhapo-tcrtim    TO gwa_tranview-tcrtim,
            gwa_vtbfhapo-wzbetr    TO gwa_tranview-wzbetr,
            gwa_vtbfhapo-rahabki   TO gwa_tranview-rahabki, "23569
            gwa_vtbfhapo-rahktid   TO gwa_tranview-rahktid, "23569
            gwa_vtbfhapo-SSPRGRD   TO gwa_tranview-SSPRGRD. "23569

      APPEND gwa_tranview TO pi_data_view.

    ENDIF.

    CLEAR : gwa_vtbfhapo,
            gwa_vtbfhazu,
            gwa_t001,
            gwa_vtbfha,
            gwa_tranview.

  ENDLOOP.

ENDFORM.                    " F_FILL_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_MAKE_RANGES
*&---------------------------------------------------------------------*
*       Make Ranges
*----------------------------------------------------------------------*
FORM f_make_ranges USING pi_bukrs TYPE ANY TABLE
                         pi_rfha  TYPE ANY TABLE.
  DATA : lwa_bukrs TYPE bukrs,
         lwa_rbukrs LIKE LINE OF gr_bukrs,
         lwa_rfha TYPE tb_rfha,
         lwa_rrfha LIKE LINE OF gr_rfha.

  REFRESH : gr_bukrs,
            gr_rfha.
  LOOP AT pi_bukrs INTO lwa_bukrs.
    lwa_rbukrs-sign = 'I'.
    lwa_rbukrs-option = 'EQ'.
    lwa_rbukrs-low = lwa_bukrs.
    APPEND lwa_rbukrs TO gr_bukrs.
  ENDLOOP.

  LOOP AT pi_rfha INTO lwa_rfha.
    lwa_rrfha-sign = 'I'.
    lwa_rrfha-option = 'EQ'.
    lwa_rrfha-low = lwa_rfha.
    APPEND lwa_rrfha TO gr_rfha.
  ENDLOOP.


ENDFORM.                    "f_make_ranges
