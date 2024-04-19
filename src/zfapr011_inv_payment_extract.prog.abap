REPORT  zzbbfap_audit2 MESSAGE-ID zs.
************************************************************************
*  Client:     Spectra Energy.                                         *
*  Date:       January 2011                                            *
*  Author:     Brian Boundy                                            *
*  Program Description:                                                *
*  Extract for Audit.                                                  *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue   By      Date    Description                                   *
************************************************************************

TABLES: rbkp.
TYPE-POOLS : abap.

TYPES:          ty_rbkp     TYPE rbkp,
                BEGIN OF ty_rbkp_key,
                  mandt LIKE rbkp-mandt,
                  belnr LIKE rbkp-belnr,
                  gjahr LIKE rbkp-gjahr,
                END OF ty_rbkp_key,
                ty_rseg     TYPE rseg,
                ty_bkpf     TYPE bkpf,
                BEGIN OF ty_bkpf_key,
                  mandt LIKE bkpf-mandt,
                  bukrs LIKE bkpf-bukrs,
                  belnr LIKE bkpf-belnr,
                  gjahr LIKE bkpf-gjahr,
                END OF ty_bkpf_key,
                ty_reguh    TYPE reguh,
                ty_payr     TYPE payr,
                ty_bseg     TYPE bseg,
                BEGIN OF ty_bseg_key,
                  mandt LIKE bseg-mandt,
                  bukrs LIKE bseg-bukrs,
                  gjahr LIKE bseg-gjahr,
                  buzei LIKE bseg-buzei,
                  belnr LIKE bseg-belnr,
                END OF ty_bseg_key,
                ty_bsis     TYPE bsis,
                ty_bsas     TYPE bsas,
                ty_bsik     TYPE bsik,
                ty_bsak     TYPE bsak.




TYPES:        BEGIN OF ty_reguh_lookup,
                mandt LIKE reguh-mandt,
                laufd LIKE reguh-laufd,
                laufd2 LIKE reguh-laufd,
                laufi LIKE reguh-laufi,
                xvorl LIKE reguh-xvorl,
                zbukr LIKE reguh-zbukr,
                lifnr LIKE reguh-lifnr,
                kunnr LIKE reguh-kunnr,
                empfg LIKE reguh-empfg,
                vblnr LIKE reguh-vblnr,
              END OF ty_reguh_lookup,
              BEGIN OF ty_payr_lookup,
                mandt LIKE payr-mandt,
                zbukr LIKE payr-zbukr,
                hbkid LIKE payr-hbkid,
                hktid LIKE payr-hktid,
                rzawe LIKE payr-rzawe,
                chect LIKE payr-chect,
                gjahr LIKE payr-gjahr,
                vblnr LIKE payr-vblnr,
              END OF ty_payr_lookup,
              BEGIN OF ty_bsis_lookup,
                mandt LIKE bsis-mandt,
                bukrs LIKE bsis-bukrs,
                hkont LIKE bsis-hkont,
                augdt LIKE bsis-augdt,
                augbl LIKE bsis-augbl,
                zuonr LIKE bsis-zuonr,
                gjahr LIKE bsis-gjahr,
                belnr LIKE bsis-belnr,
                buzei LIKE bsis-buzei,
              END OF ty_bsis_lookup,
              BEGIN OF ty_bsas_lookup,
                mandt LIKE bsas-mandt,
                bukrs LIKE bsas-bukrs,
                hkont LIKE bsas-hkont,
                augdt LIKE bsas-augdt,
                augbl LIKE bsas-augbl,
                zuonr LIKE bsas-zuonr,
                gjahr LIKE bsas-gjahr,
                belnr LIKE bsas-belnr,
                buzei LIKE bsas-buzei,
              END OF ty_bsas_lookup,
              BEGIN OF ty_bsik_lookup,
                mandt LIKE bsik-mandt,
                bukrs LIKE bsik-bukrs,
                lifnr LIKE bsik-lifnr,
                umsks LIKE bsik-umsks,
                umskz LIKE bsik-umskz,
                augdt LIKE bsik-augdt,
                augbl LIKE bsik-augbl,
                zuonr LIKE bsik-zuonr,
                gjahr LIKE bsik-gjahr,
                belnr LIKE bsik-belnr,
                buzei LIKE bsik-buzei,
              END OF ty_bsik_lookup,
              BEGIN OF ty_bsak_lookup,
                mandt LIKE bsak-mandt,
                bukrs LIKE bsak-bukrs,
                lifnr LIKE bsak-lifnr,
                umsks LIKE bsak-umsks,
                umskz LIKE bsak-umskz,
                augdt LIKE bsak-augdt,
                augbl LIKE bsak-augbl,
                zuonr LIKE bsak-zuonr,
                gjahr LIKE bsak-gjahr,
                belnr LIKE bsak-belnr,
                buzei LIKE bsak-buzei,
              END OF ty_bsak_lookup.




DATA:         s_reguh_lookup  TYPE          ty_reguh_lookup,
              t_reguh_lookup  LIKE TABLE OF s_reguh_lookup,
              s_payr_lookup   TYPE          ty_payr_lookup,
              t_payr_lookup   LIKE TABLE OF s_payr_lookup,
              s_bsis_lookup   TYPE          ty_bsis_lookup,
              t_bsis_lookup   LIKE TABLE OF s_bsis_lookup,
              s_bsas_lookup   TYPE          ty_bsas_lookup,
              t_bsas_lookup   LIKE TABLE OF s_bsas_lookup,
              s_bsik_lookup   TYPE          ty_bsik_lookup,
              t_bsik_lookup   LIKE TABLE OF s_bsik_lookup,
              s_bsak_lookup   TYPE          ty_bsak_lookup,
              t_bsak_lookup   LIKE TABLE OF s_bsak_lookup.


DATA:           msg(80)       TYPE          c,
                lv_string     TYPE          string,
                lv_datetime   TYPE          string,
                lv_count      TYPE          i,
                lv_lines      TYPE          i,
                lv_system     TYPE          c,

                s_rbkp        TYPE          ty_rbkp,
                t_rbkp        LIKE TABLE OF s_rbkp,
                s_rbkp_key    TYPE          ty_rbkp_key,
                t_rbkp_key    LIKE TABLE OF s_rbkp_key,
                s_rseg        TYPE          ty_rseg,
                t_rseg        LIKE TABLE OF s_rseg,
                s_bkpf        TYPE          ty_bkpf,
                t_bkpf        LIKE TABLE OF s_bkpf,
                s_bkpf_key    TYPE          ty_bkpf_key,
                t_bkpf_key    LIKE TABLE OF s_bkpf_key,
                s_reguh       TYPE          ty_reguh,
                t_reguh       LIKE TABLE OF s_reguh,
                s_payr        TYPE          ty_payr,
                t_payr        LIKE TABLE OF s_payr,
                s_bseg        TYPE          ty_bseg,
                t_bseg        LIKE TABLE OF s_bseg,
                s_bseg_key    TYPE          ty_bseg_key,
                t_bseg_key    LIKE TABLE OF s_bseg_key,
                s_bsis        TYPE          ty_bsis,
                t_bsis        LIKE TABLE OF s_bsis,
                s_bsas        TYPE          ty_bsas,
                t_bsas        LIKE TABLE OF s_bsas,
                s_bsik        TYPE          ty_bsik,
                t_bsik        LIKE TABLE OF s_bsik,
                s_bsak        TYPE          ty_bsak,
                t_bsak        LIKE TABLE OF s_bsak,

                lv_datarec    TYPE          string,
                t_data        LIKE TABLE OF lv_datarec,
                lv_linenum    TYPE          string,
                t_linenum     LIKE TABLE OF lv_linenum,
                idetails      TYPE          abap_compdescr_tab,
                xdetails      TYPE          abap_compdescr,
                ref_table_des TYPE REF TO cl_abap_structdescr.

DATA:           out_rbkp      LIKE          rfpdo-rfbifile,
                out_rseg      LIKE          rfpdo-rfbifile,
                out_bkpf      LIKE          rfpdo-rfbifile,
                out_reguh     LIKE          rfpdo-rfbifile,
                out_payr      LIKE          rfpdo-rfbifile,
                out_bseg      LIKE          rfpdo-rfbifile,
                out_bsis      LIKE          rfpdo-rfbifile,
                out_bsas      LIKE          rfpdo-rfbifile,
                out_bsik      LIKE          rfpdo-rfbifile,
                out_bsak      LIKE          rfpdo-rfbifile,
                out_stats     LIKE          rfpdo-rfbifile.


FIELD-SYMBOLS:  <curfile>     LIKE          rfpdo-rfbifile,
                <curcol>      TYPE          ANY.

CONSTANTS:      delimtr       TYPE          c               VALUE '|'.


*************************************************************************
*************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
SELECT-OPTIONS:
  s_bldat   FOR rbkp-bldat,
  s_budat   FOR rbkp-budat,
  s_cpudt   FOR rbkp-cpudt.
PARAMETERS:
  p_cutoff  TYPE dats.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
  p_folder LIKE filenameci-fileextern.
SELECTION-SCREEN END OF BLOCK b1.

*************************************************************************
*************************************************************************
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/ACE/' INTO p_folder.

*************************************************************************
*************************************************************************
START-OF-SELECTION.

  PERFORM set_filenames.

  PERFORM get_rbkp_data.
  PERFORM out_rbkp_data.
  CLEAR: t_rbkp.
  PERFORM get_rseg_data.
  PERFORM out_rseg_data.
  CLEAR: t_rseg, t_rbkp_key.

  PERFORM get_bkpf_data.
  PERFORM out_bkpf_data.
  CLEAR: t_bkpf.
  PERFORM get_reguh_data.
  PERFORM out_reguh_data.
  CLEAR: t_reguh, t_reguh_lookup.
  PERFORM get_payr_data.
  PERFORM out_payr_data.
  CLEAR: t_payr, t_payr_lookup.

  PERFORM get_bseg_data.
  PERFORM out_bseg_data.
  CLEAR: t_bseg, t_bkpf_key.

  PERFORM get_bsis_data.
  PERFORM out_bsis_data.
  CLEAR: t_bsis, t_bsis_lookup.
  PERFORM get_bsas_data.
  PERFORM out_bsas_data.
  CLEAR: t_bsas, t_bsas_lookup.
  PERFORM get_bsik_data.
  PERFORM out_bsik_data.
  CLEAR: t_bsik, t_bsas_lookup.
  PERFORM get_bsak_data.
  PERFORM out_bsak_data.
  CLEAR: t_bsak, t_bsak_lookup, t_bseg_key.
  PERFORM out_stats_data.


*************************************************************************
*************************************************************************

*----------------------------------------------------------------------*
FORM set_filenames.

  "Set the system variable
  CASE sy-sysid+0(3).
    WHEN 'S01' OR 'S02' OR 'Q02' OR 'D30' OR 'D22' OR 'P01'.
      lv_system = 'E'.
    WHEN 'SBX' OR 'S11' OR 'Q12' OR 'C11' OR 'Q11' OR 'P11'.
      lv_system = 'W'.
    WHEN OTHERS.
      lv_system = 'O'.
  ENDCASE.

  "Check the final character, for a / and append it if neccesary.
  lv_count = STRLEN( p_folder ).
  lv_count = lv_count - 1.

  IF p_folder+lv_count <> '/'.
    CONCATENATE p_folder '/' INTO p_folder.
  ENDIF.

  CONCATENATE sy-datum sy-uzeit INTO lv_datetime.

  CONCATENATE p_folder lv_system '_RBKP_'   lv_datetime '.dat' INTO out_rbkp.
  CONCATENATE p_folder lv_system '_RSEG_'   lv_datetime '.dat' INTO out_rseg.
  CONCATENATE p_folder lv_system '_BKPF_'   lv_datetime '.dat' INTO out_bkpf.
  CONCATENATE p_folder lv_system '_REGUH_'  lv_datetime '.dat' INTO out_reguh.
  CONCATENATE p_folder lv_system '_BSEG_'   lv_datetime '.dat' INTO out_bseg.
  CONCATENATE p_folder lv_system '_BSIS_'   lv_datetime '.dat' INTO out_bsis.
  CONCATENATE p_folder lv_system '_BSAS_'   lv_datetime '.dat' INTO out_bsas.
  CONCATENATE p_folder lv_system '_BSIK_'   lv_datetime '.dat' INTO out_bsik.
  CONCATENATE p_folder lv_system '_BSAK_'   lv_datetime '.dat' INTO out_bsak.
  CONCATENATE p_folder lv_system '_PAYR_'   lv_datetime '.dat' INTO out_payr.
  CONCATENATE p_folder lv_system '_STATS_'  lv_datetime '.dat' INTO out_stats.


ENDFORM.                    "set_filenames





*----------------------------------------------------------------------*
FORM get_rbkp_data.

  CLEAR: t_rbkp, t_rbkp_key.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE t_rbkp
    FROM rbkp
    WHERE bldat IN s_bldat
      AND budat IN s_budat
      AND cpudt IN s_cpudt
    .

  "Build key table.
  LOOP AT t_rbkp INTO s_rbkp.
    s_rbkp_key-mandt = s_rbkp-mandt.
    s_rbkp_key-belnr = s_rbkp-belnr.
    s_rbkp_key-gjahr = s_rbkp-gjahr.
    APPEND s_rbkp_key TO t_rbkp_key.
  ENDLOOP.

ENDFORM.                    "get_rbkp_data


*----------------------------------------------------------------------*
FORM out_rbkp_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'RBKP' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_rbkp INTO s_rbkp.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_rbkp TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'RBKP:  ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_rbkp TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_rbkp_data





*----------------------------------------------------------------------*
FORM get_rseg_data.

  CLEAR t_rseg.

  LOOP AT t_rbkp_key INTO s_rbkp_key.
    SELECT *
      INTO CORRESPONDING FIELDS OF s_rseg
      FROM rseg CLIENT SPECIFIED
      WHERE mandt = s_rbkp_key-mandt
        AND belnr = s_rbkp_key-belnr
        AND gjahr = s_rbkp_key-gjahr.

      APPEND s_rseg TO t_rseg.

    ENDSELECT.

  ENDLOOP.

ENDFORM.                    "get_rseg_data


*----------------------------------------------------------------------*
FORM out_rseg_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'RSEG' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_rseg INTO s_rseg.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_rseg TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'RSEG:  ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_rseg TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.
ENDFORM.                    "out_rseg_data





*----------------------------------------------------------------------*
FORM get_bkpf_data.

  CLEAR: t_bkpf, t_bkpf_key.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE t_bkpf
    FROM bkpf
    WHERE bldat IN s_bldat
      AND budat IN s_budat
      AND cpudt IN s_cpudt
    .

  "Build key table.
  LOOP AT t_bkpf INTO s_bkpf.
    s_bkpf_key-mandt = s_bkpf-mandt.
    s_bkpf_key-bukrs = s_bkpf-bukrs.
    s_bkpf_key-belnr = s_bkpf-belnr.
    s_bkpf_key-gjahr = s_bkpf-gjahr.
    APPEND s_bkpf_key TO t_bkpf_key.
  ENDLOOP.

ENDFORM.                    "get_rbkp_data


*----------------------------------------------------------------------*
FORM out_bkpf_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'BKPF' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_bkpf INTO s_bkpf.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_bkpf TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'BKPF:  ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_bkpf TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_bkpf_data





*----------------------------------------------------------------------*
FORM get_reguh_data.

  CLEAR t_reguh.

* Get a local version of reguh
  SELECT mandt laufd laufi xvorl zbukr lifnr kunnr empfg vblnr
    INTO CORRESPONDING FIELDS OF TABLE t_reguh_lookup
    FROM reguh
      WHERE laufd > p_cutoff.


  LOOP AT t_reguh_lookup INTO s_reguh_lookup.

    s_reguh_lookup-laufd2 = s_reguh_lookup-laufd(4).

    modify t_reguh_lookup from s_reguh_lookup.
  ENDLOOP.

  SORT t_reguh_lookup ASCENDING BY mandt zbukr vblnr laufd2.



  LOOP AT t_bkpf_key INTO s_bkpf_key.

    CLEAR s_reguh_lookup.

    READ TABLE t_reguh_lookup  INTO s_reguh_lookup
             WITH KEY mandt = s_bkpf_key-mandt
                      zbukr = s_bkpf_key-bukrs
                      vblnr = s_bkpf_key-belnr
                      laufd2 = s_bkpf_key-gjahr
      BINARY SEARCH.


    IF sy-subrc = 0.

      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF s_reguh
        FROM reguh CLIENT SPECIFIED
        WHERE mandt = s_reguh_lookup-mandt
          AND laufd = s_reguh_lookup-laufd
          AND laufi = s_reguh_lookup-laufi
          AND xvorl = s_reguh_lookup-xvorl
          AND zbukr = s_reguh_lookup-zbukr
          AND lifnr = s_reguh_lookup-lifnr
          AND kunnr = s_reguh_lookup-kunnr
          AND empfg = s_reguh_lookup-empfg
          AND vblnr = s_reguh_lookup-vblnr
        .

      APPEND s_reguh TO t_reguh.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "get_reguh_data


*----------------------------------------------------------------------*
FORM out_reguh_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'REGUH' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_reguh INTO s_reguh.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_reguh TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'REGUH: ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_reguh TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_reguh_data





*----------------------------------------------------------------------*
FORM get_payr_data.

  CLEAR t_payr.

* Get a local version of reguh
  SELECT mandt zbukr hbkid hktid rzawe chect gjahr vblnr
    INTO CORRESPONDING FIELDS OF TABLE t_payr_lookup
    FROM payr.

  SORT t_payr_lookup ASCENDING BY mandt gjahr vblnr.



  LOOP AT t_bkpf_key INTO s_bkpf_key.

    CLEAR s_payr_lookup.

    READ TABLE t_payr_lookup  INTO s_payr_lookup
             WITH KEY mandt = s_bkpf_key-mandt
                      gjahr = s_bkpf_key-gjahr
                      vblnr = s_bkpf_key-belnr
      BINARY SEARCH.


    IF sy-subrc = 0.

      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF s_payr
        FROM payr CLIENT SPECIFIED
        WHERE mandt = s_payr_lookup-mandt
          AND zbukr = s_payr_lookup-zbukr
          AND hbkid = s_payr_lookup-hbkid
          AND hktid = s_payr_lookup-hktid
          AND rzawe = s_payr_lookup-rzawe
          AND chect = s_payr_lookup-chect
        .

      APPEND s_payr TO t_payr.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "get_payr_data


*----------------------------------------------------------------------*
FORM out_payr_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'PAYR' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_payr INTO s_payr.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_payr TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'PAYR:  ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_payr TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_payr_data





*----------------------------------------------------------------------*
FORM get_bseg_data.

  CLEAR: t_bseg, t_bseg_key.

  LOOP AT t_bkpf_key INTO s_bkpf_key.
    SELECT *
      INTO CORRESPONDING FIELDS OF s_bseg
      FROM bseg CLIENT SPECIFIED
      WHERE mandt = s_bkpf_key-mandt
        AND bukrs = s_bkpf_key-bukrs
        AND belnr = s_bkpf_key-belnr
        AND gjahr = s_bkpf_key-gjahr.

      APPEND s_bseg TO t_bseg.

    ENDSELECT.

  ENDLOOP.


  "Build key table.
  LOOP AT t_bseg INTO s_bseg.
    s_bseg_key-mandt = s_bseg-mandt.
    s_bseg_key-bukrs = s_bseg-bukrs.
    s_bseg_key-gjahr = s_bseg-gjahr.
    s_bseg_key-buzei = s_bseg-buzei.
    s_bseg_key-belnr = s_bseg-belnr.
    APPEND s_bseg_key TO t_bseg_key.
  ENDLOOP.


ENDFORM.                    "get_bseg_data


*----------------------------------------------------------------------*
FORM out_bseg_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'BSEG' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_bseg INTO s_bseg.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_bseg TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'BSEG:  ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_bseg TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_bseg_data





*----------------------------------------------------------------------*
FORM get_bsis_data.

  CLEAR t_bsis.

* Get a local version of reguh
  SELECT mandt bukrs hkont augdt augbl zuonr gjahr belnr buzei
    INTO CORRESPONDING FIELDS OF TABLE t_bsis_lookup
    FROM bsis
    WHERE budat > p_cutoff.

  SORT t_bsis_lookup ASCENDING BY mandt bukrs gjahr buzei belnr.



  LOOP AT t_bseg_key INTO s_bseg_key.

    CLEAR s_bsis_lookup.

    READ TABLE t_bsis_lookup  INTO s_bsis_lookup
             WITH KEY mandt = s_bseg_key-mandt
                      bukrs = s_bseg_key-bukrs
                      gjahr = s_bseg_key-gjahr
                      buzei = s_bseg_key-buzei
                      belnr = s_bseg_key-belnr
      BINARY SEARCH.


    IF sy-subrc = 0.

      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF s_bsis
        FROM bsis CLIENT SPECIFIED
        WHERE mandt = s_bsis_lookup-mandt
          AND bukrs = s_bsis_lookup-bukrs
          AND hkont = s_bsis_lookup-hkont
          AND augdt = s_bsis_lookup-augdt
          AND augbl = s_bsis_lookup-augbl
          AND zuonr = s_bsis_lookup-zuonr
          AND gjahr = s_bsis_lookup-gjahr
          AND belnr = s_bsis_lookup-belnr
          AND buzei = s_bsis_lookup-buzei
        .

      APPEND s_bsis TO t_bsis.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "get_bsis_data


*----------------------------------------------------------------------*
FORM out_bsis_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'BSIS' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_bsis INTO s_bsis.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_bsis TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'BSIS:  ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_bsis TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_bsis_data





*----------------------------------------------------------------------*
FORM get_bsas_data.

  CLEAR t_bsas.

* Get a local version of reguh
  SELECT mandt bukrs hkont augdt augbl zuonr gjahr belnr buzei
    INTO CORRESPONDING FIELDS OF TABLE t_bsas_lookup
    FROM bsas
    WHERE budat > p_cutoff.

  SORT t_bsas_lookup ASCENDING BY mandt bukrs gjahr buzei belnr.



  LOOP AT t_bseg_key INTO s_bseg_key.

    CLEAR s_bsas_lookup.

    READ TABLE t_bsas_lookup  INTO s_bsas_lookup
             WITH KEY mandt = s_bseg_key-mandt
                      bukrs = s_bseg_key-bukrs
                      gjahr = s_bseg_key-gjahr
                      buzei = s_bseg_key-buzei
                      belnr = s_bseg_key-belnr
      BINARY SEARCH.


    IF sy-subrc = 0.

      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF s_bsas
        FROM bsas CLIENT SPECIFIED
        WHERE mandt = s_bsas_lookup-mandt
          AND bukrs = s_bsas_lookup-bukrs
          AND hkont = s_bsas_lookup-hkont
          AND augdt = s_bsas_lookup-augdt
          AND augbl = s_bsas_lookup-augbl
          AND zuonr = s_bsas_lookup-zuonr
          AND gjahr = s_bsas_lookup-gjahr
          AND belnr = s_bsas_lookup-belnr
          AND buzei = s_bsas_lookup-buzei
        .

      APPEND s_bsas TO t_bsas.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "get_bsas_data


*----------------------------------------------------------------------*
FORM out_bsas_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'BSAS' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_bsas INTO s_bsas.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_bsas TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'BSAS:  ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_bsas TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_bsas_data





*----------------------------------------------------------------------*
FORM get_bsik_data.

  CLEAR t_bsik.

* Get a local version of reguh
  SELECT mandt bukrs lifnr umsks umskz augdt augbl zuonr gjahr belnr buzei
    INTO CORRESPONDING FIELDS OF TABLE t_bsik_lookup
    FROM bsik
    WHERE budat > p_cutoff.

  SORT t_bsik_lookup ASCENDING BY mandt bukrs gjahr buzei belnr.



  LOOP AT t_bseg_key INTO s_bseg_key.

    CLEAR s_bsik_lookup.

    READ TABLE t_bsik_lookup  INTO s_bsik_lookup
             WITH KEY mandt = s_bseg_key-mandt
                      bukrs = s_bseg_key-bukrs
                      gjahr = s_bseg_key-gjahr
                      buzei = s_bseg_key-buzei
                      belnr = s_bseg_key-belnr
      BINARY SEARCH.


    IF sy-subrc = 0.

      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF s_bsik
        FROM bsik CLIENT SPECIFIED
        WHERE mandt = s_bsik_lookup-mandt
          AND bukrs = s_bsik_lookup-bukrs
          AND lifnr = s_bsik_lookup-lifnr
          AND umsks = s_bsik_lookup-umsks
          AND umskz = s_bsik_lookup-umskz
          AND augdt = s_bsik_lookup-augdt
          AND augbl = s_bsik_lookup-augbl
          AND zuonr = s_bsik_lookup-zuonr
          AND gjahr = s_bsik_lookup-gjahr
          AND belnr = s_bsik_lookup-belnr
          AND buzei = s_bsik_lookup-buzei
        .

      APPEND s_bsik TO t_bsik.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "get_bsik_data


*----------------------------------------------------------------------*
FORM out_bsik_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'BSIK' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_bsik INTO s_bsik.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_bsik TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'BSIK:  ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_bsik TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_bsik_data





*----------------------------------------------------------------------*
FORM get_bsak_data.

  CLEAR t_bsak.

* Get a local version of reguh
  SELECT mandt bukrs lifnr umsks umskz augdt augbl zuonr gjahr belnr buzei
    INTO CORRESPONDING FIELDS OF TABLE t_bsak_lookup
    FROM bsak
    WHERE budat > p_cutoff.

  SORT t_bsak_lookup ASCENDING BY mandt bukrs gjahr buzei belnr.



  LOOP AT t_bseg_key INTO s_bseg_key.

    CLEAR s_bsak_lookup.

    READ TABLE t_bsak_lookup  INTO s_bsak_lookup
             WITH KEY mandt = s_bseg_key-mandt
                      bukrs = s_bseg_key-bukrs
                      gjahr = s_bseg_key-gjahr
                      buzei = s_bseg_key-buzei
                      belnr = s_bseg_key-belnr
      BINARY SEARCH.


    IF sy-subrc = 0.

      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF s_bsak
        FROM bsak CLIENT SPECIFIED
        WHERE mandt = s_bsak_lookup-mandt
          AND bukrs = s_bsak_lookup-bukrs
          AND lifnr = s_bsak_lookup-lifnr
          AND umsks = s_bsak_lookup-umsks
          AND umskz = s_bsak_lookup-umskz
          AND augdt = s_bsak_lookup-augdt
          AND augbl = s_bsak_lookup-augbl
          AND zuonr = s_bsak_lookup-zuonr
          AND gjahr = s_bsak_lookup-gjahr
          AND belnr = s_bsak_lookup-belnr
          AND buzei = s_bsak_lookup-buzei
        .

      APPEND s_bsak TO t_bsak.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "get_bsak_data


*----------------------------------------------------------------------*
FORM out_bsak_data.

  CLEAR: t_data.

* Get the structure of the table.
  ref_table_des ?= cl_abap_typedescr=>describe_by_name( 'BSAK' ).
  idetails[] = ref_table_des->components[].

  CLEAR lv_datarec.
  LOOP AT idetails INTO xdetails.
    IF sy-tabix = 1.
      lv_datarec = xdetails-name.
    ELSE.
      CONCATENATE lv_datarec xdetails-name INTO lv_datarec SEPARATED BY delimtr.
    ENDIF.
  ENDLOOP.
  APPEND lv_datarec TO t_data.


  lv_lines = 0.
  LOOP AT t_bsak INTO s_bsak.
    CLEAR: lv_datarec.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE s_bsak TO <curcol>.
      IF sy-subrc = 0.
        lv_string = <curcol>.
        IF sy-index = 1.
          lv_datarec = lv_string.
        ELSE.
          CONCATENATE lv_datarec lv_string INTO lv_datarec SEPARATED BY delimtr.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    lv_lines = lv_lines + 1.
    REPLACE ALL OCCURRENCES OF ' |' IN lv_datarec WITH '|'.
    REPLACE ALL OCCURRENCES OF '| ' IN lv_datarec WITH '|'.
    APPEND lv_datarec TO t_data.
  ENDLOOP.

  lv_string = lv_lines.
  CONCATENATE 'BSAK:  ' lv_string INTO lv_linenum.
  APPEND lv_linenum TO t_linenum.


  ASSIGN out_bsak TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_data INTO lv_datarec.
    TRANSFER lv_datarec TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_bsak_data





*----------------------------------------------------------------------*
FORM out_stats_data.

  CLEAR: t_data.

  ASSIGN out_stats TO <curfile>.

  PERFORM open_csvfile.

  LOOP AT t_linenum INTO lv_linenum.
    TRANSFER lv_linenum TO <curfile>.
  ENDLOOP.

  PERFORM close_csvfile.

ENDFORM.                    "out_bsak_data




*----------------------------------------------------------------------*
FORM open_csvfile.
  OPEN DATASET <curfile> FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    WRITE msg.
    STOP.
  ENDIF.
ENDFORM.                    "OPEN_CSVFILE


*----------------------------------------------------------------------*
FORM close_csvfile.
  CLOSE DATASET <curfile>.
  IF sy-subrc NE '0'.
    WRITE msg.
    STOP.
  ELSE.
    WRITE:/ 'File Outputed Successfully to: ', <curfile>.
  ENDIF.
ENDFORM.                    "CLOSE_ALL_FILES
