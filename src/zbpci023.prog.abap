*&---------------------------------------------------------------------*
*& Report  ZBPCI020
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zbpci023.
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: hotspot_click  FOR EVENT link_click OF cl_salv_events_table
                         IMPORTING row column.
ENDCLASS.               "LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_handler
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD hotspot_click.
    PERFORM handle_click USING row column.
  ENDMETHOD.                    "hotspot_click1
ENDCLASS.               "lcl_event_handler

TABLES: anla,
        anek,
        anep.
TYPES: BEGIN OF ty_file_output,
        asset_class TYPE char11,
        comma1 TYPE c,
        asset_cate TYPE char08,
        comma2 TYPE c,
        asset_datasrc TYPE char10,
        comma3 TYPE c,
        asset_entity  TYPE char10,
        comma4 TYPE c,
        asset_plant   TYPE char04,
        comma5 TYPE c,
        asset_time    TYPE char08,
        comma6 TYPE c,
        asset_ttype   TYPE char14,
        comma7 TYPE c,
        asset_amount  TYPE char15,
       END OF ty_file_output.
TYPES: BEGIN OF ty_alv_output,
        bukrs TYPE anek-bukrs,
        anlkl TYPE anla-anlkl,
        anln1 TYPE anek-anln1,
        anln2 TYPE anek-anln2,
        gjahr TYPE anek-gjahr,
        monat TYPE anek-monat,
        lnran TYPE anek-lnran,
        budat TYPE anek-budat,
        bldat TYPE anek-bldat,
        bwasl TYPE anep-bwasl,
*        anbtr(15) TYPE c, "anep-anbtr,
        anbtr TYPE anep-anbtr,
        kostl TYPE anlz-kostl,
        werks TYPE anlz-werks,
*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
        hkont TYPE bsis-hkont,
        shkzg TYPE bsis-shkzg,
        wrbtr TYPE bsis-wrbtr,
        waers TYPE bsis-waers,
        bschl TYPE bsis-bschl,
        bzdat TYPE auak-bzdat,
        tcode TYPE bkpf-tcode,
        belnr TYPE bsis-belnr,
        sgtxt TYPE bsis-sgtxt,
        xarch TYPE bsis-xarch,
        erdat TYPE anla-erdat,
        ernam TYPE anla-ernam,
        ktogr TYPE anla-ktogr,
        sernr TYPE anla-sernr,
        zujhr TYPE anla-zujhr,
        posnr TYPE anla-posnr,
        ord41 TYPE anla-ord41,
        ord42 TYPE anla-ord42,
        ord43 TYPE anla-ord43,
        TXT50 type ANLA-TXT50,
        TXA50 type ANLA-TXA50,
        MENGE type anla-MENGE,
        ZUGDT TYPE ANLA-ZUGDT,
        AKTIV type ANLA-AKTIV,
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
       END OF ty_alv_output.

*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
TYPES: BEGIN OF ty_file_output_n,
        hkont TYPE bsis-hkont,
        comma1 TYPE c,
        shkzg TYPE bsis-shkzg,
        comma2 TYPE c,
        wrbtr TYPE char16,
        comma3 TYPE c,
        waers TYPE char5,
        comma4 TYPE c,
        bschl TYPE bsis-bschl,
        comma5 TYPE c,
        bzdat TYPE char8,
        comma6 TYPE c,
        tcode TYPE bkpf-tcode,
        comma7 TYPE c,
        belnr TYPE bsis-belnr,
        comma8 TYPE c,
        sgtxt TYPE bsis-sgtxt,
        comma9 TYPE c,
        xarch TYPE bsis-xarch,
        comma18 TYPE c,
        erdat TYPE char8,
        comma10 TYPE c,
        ernam TYPE anla-ernam,
        comma11 TYPE c,
        ktogr TYPE anla-ktogr,
        comma12 TYPE c,
        sernr TYPE anla-sernr,
        comma13 TYPE c,
        zujhr TYPE char4,
        comma14 TYPE c,
        posnr TYPE char8,
        comma15 TYPE c,
        ord41 TYPE anla-ord41,
        comma16 TYPE c,
        ord42 TYPE anla-ord42,
        comma17 TYPE c,
        ord43 TYPE anla-ord43,
       END OF ty_file_output_n.
DATA: gt_file_output_n TYPE TABLE OF ty_file_output_n,
      gs_file_output_n TYPE ty_file_output_n.
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL

DATA: gt_file_output TYPE TABLE OF ty_file_output,
      gs_file_output TYPE ty_file_output,
      gt_alv_output  TYPE TABLE OF ty_alv_output,
      gs_alv_output  TYPE ty_alv_output.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR  anla-bukrs DEFAULT 'UGL' OBLIGATORY,
                s_anlkl FOR  anla-anlkl.
PARAMETERS    : p_gjahr TYPE anek-gjahr DEFAULT '2014' OBLIGATORY,
                p_monat TYPE anek-monat DEFAULT '01' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_pc RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmd,
            p_file TYPE rfpdo-lboxfile DEFAULT 'H:\SAPTEMP\AALoad.csv', " OBLIGATORY.
            r_server RADIOBUTTON GROUP rad2,
            p_sfile TYPE rfpdo-rfbifile, " OBLIGATORY..
            c_tfile AS CHECKBOX,
            p_tcfile TYPE rfpdo-rfbifile,
*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
            c_pp AS CHECKBOX,
            p_pp TYPE rfpdo-rfbifile.
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/' INTO p_sfile.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/' INTO p_tcfile.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/' INTO p_pp.
  "'/BPC/SAP-' p_gjahr '-AALoad.csv' INTO p_sfile.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_values.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  PERFORM get_file.
  PERFORM get_pc_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sfile.
  PERFORM get_server_file.

START-OF-SELECTION.


  IF c_tfile IS NOT INITIAL AND
     r_server IS INITIAL.
    WRITE: / 'Server file is not selected, Touch file cannot be created.'.
    STOP.
  ENDIF.
  IF c_tfile IS NOT INITIAL AND
    p_tcfile IS INITIAL.
    WRITE : / 'Input Touch File path and name.'.
    STOP.
  ENDIF.
*CONCATENATE p_sfile 'SAP-' p_monat p_gjahr '-AA_periodic.csv' INTO p_sfile.

*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
  IF c_pp IS NOT INITIAL AND
    p_pp IS INITIAL.
    WRITE : / 'Input PP Extract File path and name.'.
    STOP.
  ENDIF.
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL

  IF s_anlkl[] IS INITIAL.
    WRITE : / 'Asset Class is required in selection screen..'.
    STOP.
  ENDIF.
  IF p_file IS INITIAL AND
     r_pc IS NOT INITIAL.
    WRITE : / 'PC File is required is required in selection screen..'.
    STOP.
  ENDIF.
  IF p_sfile IS INITIAL AND
     r_server IS NOT INITIAL.
    WRITE : / 'Server File is required is required in selection screen..'.
    STOP.
  ENDIF.

  CLEAR:  gt_file_output,
          gs_file_output,
          gt_alv_output,
          gs_alv_output.

  PERFORM get_data.
  PERFORM download_data.
  PERFORM display_alv.
*&---------------------------------------------------------------------*
*&      Form  SET_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_values .

**  break sahmad.
*
*  CONSTANTS: lc_x   TYPE char1 VALUE 'X'.
*
*  IF r_pc = lc_x.
*    p_file = 'H:\SAPTEMP\'.
*  ELSE.
*    CLEAR p_file.
*  ENDIF.

ENDFORM.                    " SET_VALUES
*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_file .

  IF r_pc = 'X'.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        mask      = ',DAT File,*.csv'
        static    = 'X'
      CHANGING
        file_name = p_file.
  ELSE.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
*       DIRECTORY        = ' '
        filemask         = '*.*'
      IMPORTING
        serverfile       = p_file
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lv_append TYPE c,
        lv_tabix  TYPE sy-tabix,
        lv_tabix2 TYPE sy-tabix,
        lv_amount TYPE anep-anbtr,
        lt_anla TYPE TABLE OF anla,
        ls_anla TYPE anla,
        lt_anlz TYPE TABLE OF anlz,
        ls_anlz TYPE anlz,
        lt_anek TYPE TABLE OF anek,
        ls_anek TYPE anek,
        lt_anep TYPE TABLE OF anep,
        lt_anep_t3 TYPE TABLE OF anep,
        ls_anep TYPE anep.

*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
  TYPES:BEGIN OF ty_anep,
        awkey type awkey,
        end of ty_anep,
        BEGIN OF ty_anep_a,
        BUKRS TYPE BUKRS,
        ANLN1 type ANLN1,
        ANLN2 type ANLN2,
        GJAHR type GJAHR,
          LNRAN type LNRAN,
          BELNR type BELNR_d,
        END OF ty_anep_a,
        BEGIN OF ty_bkpf,
        BUKRS type BUKRS,
          BELNR type BELNR_d,
          GJAHR type GJAHR,
          tcode type tcode,
          AWKEY type AWKEY,
        END OF ty_bkpf,
        BEGIN OF ty_bsis,
          HKONT type HKONT,
          belnr type belnr_d,
          waers type waers,
          bschl type bschl,
          shkzg type SHKZG,
          wrbtr type wrbtr,
          sgtxt type sgtxt,
          xarch type xarch,
        END OF ty_bsis,
        BEGIN OF ty_aufk,
          belnr TYPE belnr_d,
          bzdat type bzdat,
        END OF ty_aufk.
  TYPES : BEGIN OF lty_t095,
          ktopl TYPE ktopl,
          ktogr TYPE hkont,
          afabe TYPE afabe,
          END OF lty_t095.
  DATA : lt_t095 TYPE TABLE OF lty_t095,
         ls_t095 TYPE lty_t095,
         lt_t095_n TYPE TABLE OF lty_t095,
         ls_t095_n TYPE lty_t095,
         lt_bsis TYPE TABLE OF bsis,
         ls_bsis TYPE bsis,
         lt_auak TYPE TABLE OF auak,
         ls_auak TYPE auak,
         lt_bkpf TYPE TABLE OF bkpf,
         ls_bkpf TYPE bkpf.

  data:gt_anep type TABLE OF ty_anep,
       gs_anep type ty_anep,
       gt_anep_a type TABLE OF ty_anep_a,
       gs_anep_a type ty_anep_a,
       gt_bsis type TABLE OF ty_bsis,
       gs_bsis type ty_bsis,
       gt_auak type TABLE OF ty_aufk,
       gs_auak type  ty_aufk,
       gt_bkpf type TABLE OF ty_bkpf,
       gs_bkpf type ty_bkpf,

        lt_anep_t2 TYPE TABLE OF anep,
        lt_anep_t TYPE TABLE OF anep,
                ls_anep_t TYPE anep.
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
  SELECT * FROM anla INTO TABLE lt_anla
    WHERE bukrs IN s_bukrs
      AND anlkl IN s_anlkl
      AND xanlgr <> 'X'.
  IF lt_anla IS INITIAL.
    WRITE : / 'No Data to process for selection criteria.'.
    STOP.
  ENDIF.
  IF lt_anla IS NOT INITIAL.
    SELECT * FROM anek INTO TABLE lt_anek
      FOR ALL ENTRIES IN lt_anla
      WHERE bukrs = lt_anla-bukrs
        AND anln1 = lt_anla-anln1
        AND anln2 = lt_anla-anln2
        AND gjahr = p_gjahr
        AND monat = p_monat.
    IF lt_anek IS INITIAL.
      WRITE : / 'No Data to process for selection criteria.'.
      STOP.
    ENDIF.
    SELECT * FROM anlz INTO TABLE lt_anlz
     FOR ALL ENTRIES IN lt_anla
     WHERE bukrs = lt_anla-bukrs
       AND anln1 = lt_anla-anln1
       AND anln2 = lt_anla-anln2.
    SELECT * FROM anep INTO TABLE lt_anep
      FOR ALL ENTRIES IN lt_anek
      WHERE bukrs = lt_anek-bukrs
        AND anln1 = lt_anek-anln1
        AND anln2 = lt_anek-anln2
        AND gjahr = lt_anek-gjahr
        AND lnran = lt_anek-lnran.

  ENDIF.
*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
 if c_pp is not INITIAL.
  LOOP AT lt_anep into ls_anep.

    gs_anep-awkey = ls_anep-belnr.
    append gs_anep to gt_anep.
    clear:gs_anep.
    clear:ls_anep.
  ENDLOOP.
  if gt_anep[] is not INITIAL.
    select BUKRS BELNR GJAHR tcode AWKEY from bkpf into TABLE gt_bkpf
                                   FOR ALL ENTRIES IN gt_anep
                                   where bukrs in s_bukrs
                                   and gjahr  = p_gjahr
                                   and AWKEY = gt_anep-awkey.
    IF sy-subrc is INITIAL.
      SELECT HKONT
            belnr
            waers
            bschl
            shkzg
            wrbtr
            sgtxt
            xarch  from bsis INTO TABLE gt_bsis
                    FOR ALL ENTRIES IN gt_bkpf
                    where bukrs in s_bukrs
                                   and gjahr  = p_gjahr
        and belnr = gt_bkpf-belnr.
      SELECT belnr bzdat from auak into TABLE gt_auak
                         FOR ALL ENTRIES IN gt_bkpf
                         where belnr = gt_bkpf-belnr.
    ENDIF.
  endif.
  endif.
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
*Prepare table for ALV output
  SORT lt_anla BY bukrs anln1 anln2.
  SORT lt_anek BY bukrs anln1 anln2 gjahr monat.
  SORT lt_anep BY bukrs anln1 anln2 gjahr lnran.
  SORT lt_anlz BY bukrs anln1 anln2.
  LOOP AT lt_anla INTO ls_anla.
    CLEAR: gs_alv_output,
           ls_anlz,
           lv_append.
*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
    if c_pp is not INITIAL.
    gs_alv_output-erdat = ls_anla-erdat.
    gs_alv_output-ernam = ls_anla-ernam.
    gs_alv_output-ktogr = ls_anla-ktogr.
    gs_alv_output-sernr = ls_anla-sernr.
    gs_alv_output-zujhr = ls_anla-zujhr.
    gs_alv_output-posnr = ls_anla-posnr.
    gs_alv_output-ord41 = ls_anla-ord41.
    gs_alv_output-ord42 = ls_anla-ord42.
    gs_alv_output-ord43 = ls_anla-ord43.
    gs_alv_output-TXT50 = ls_anla-TXT50.
    gs_alv_output-MENGE = ls_anla-MENGE.
    gs_alv_output-ZUGDT = ls_anla-ZUGDT.
    gs_alv_output-AKTIV = ls_anla-AKTIV.
    gs_alv_output-TXA50 = ls_anla-TXA50.
    endif.
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
    gs_alv_output-bukrs = ls_anla-bukrs.
    gs_alv_output-anln1 = ls_anla-anln1.
    gs_alv_output-anln2 = ls_anla-anln2.
    gs_alv_output-anlkl = ls_anla-anlkl.

    READ TABLE lt_anlz INTO ls_anlz WITH KEY bukrs = ls_anla-bukrs
                                             anln1 = ls_anla-anln1
                                             anln2 = ls_anla-anln2.
    gs_alv_output-kostl = ls_anlz-kostl.
    gs_alv_output-werks = ls_anlz-werks.
    READ TABLE lt_anek WITH KEY bukrs = ls_anla-bukrs
                                anln1 = ls_anla-anln1
                                anln2 = ls_anla-anln2
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    CHECK sy-subrc = 0.

*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
if c_pp is not INITIAL.
    READ TABLE lt_anep into ls_anep WITH KEY bukrs = ls_anla-bukrs
                                anln1 = ls_anla-anln1
                                anln2 = ls_anla-anln2.

    IF sy-subrc is INITIAL.
      READ TABLE gt_bkpf into gs_bkpf with key awkey = ls_anep-belnr.
      IF sy-subrc is INITIAL.
        gs_alv_output-tcode = gs_bkpf-tcode.
        READ TABLE gt_bsis into gs_bsis with key belnr = gs_bkpf-belnr.
        IF sy-subrc is INITIAL.

          gs_alv_output-hkont = gs_bsis-hkont.
          gs_alv_output-shkzg = gs_bsis-shkzg.
          gs_alv_output-wrbtr = gs_bsis-wrbtr.
          gs_alv_output-waers = gs_bsis-waers.
          gs_alv_output-bschl = gs_bsis-bschl.
          gs_alv_output-belnr = gs_bsis-belnr.
          gs_alv_output-sgtxt = gs_bsis-sgtxt.
          gs_alv_output-xarch = gs_bsis-xarch.
          READ TABLE gt_auak INTO gs_auak WITH KEY belnr = gs_bsis-belnr.
          IF sy-subrc = 0.
            gs_alv_output-bzdat = gs_auak-bzdat.
          ENDIF.
        ENDIF.

      ENDIF.
      gs_alv_output-BZDAT = ls_anep-BZDAT.
    ENDIF.
    endif.
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL

    LOOP AT lt_anek INTO ls_anek FROM lv_tabix.
      IF ls_anek-bukrs <> ls_anla-bukrs OR
         ls_anek-anln1 <> ls_anla-anln1 OR
         ls_anek-anln2 <> ls_anla-anln2.
        EXIT.
      ENDIF.
      gs_alv_output-gjahr = ls_anek-gjahr.
      gs_alv_output-monat = ls_anek-monat.
      gs_alv_output-lnran = ls_anek-lnran.
      gs_alv_output-budat = ls_anek-budat.
      gs_alv_output-bldat = ls_anek-bldat.
      READ TABLE lt_anep WITH KEY bukrs = ls_anek-bukrs
                                  anln1 = ls_anek-anln1
                                  anln2 = ls_anek-anln2
                                  gjahr = ls_anek-gjahr
                                  lnran = ls_anek-lnran
                                  TRANSPORTING NO FIELDS.
      lv_tabix2 = sy-tabix.
      CHECK sy-subrc = 0.
      LOOP AT lt_anep INTO ls_anep FROM lv_tabix2.
        IF ls_anek-bukrs <> ls_anep-bukrs OR
           ls_anek-anln1 <> ls_anep-anln1 OR
           ls_anek-anln2 <> ls_anep-anln2 OR
           ls_anek-gjahr <> ls_anep-gjahr OR
           ls_anek-lnran <> ls_anep-lnran.
          EXIT.
        ENDIF.
        CHECK ls_anep-anbtr <> 0.
        gs_alv_output-bwasl = ls_anep-bwasl.
*        gs_alv_output-anbtr = ls_anep-anbtr.
        lv_amount = ls_anep-anbtr.
***Start of comment by AKMADASU
*        IF lv_amount < 0.
*          gs_alv_output-anbtr = lv_amount.
*          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*            CHANGING
*              value = gs_alv_output-anbtr.
**           gs_alv_output-anbtr = lv_amount.
*        ELSE.
***End of comment by AKMADASU
          gs_alv_output-anbtr = lv_amount.
*        ENDIF.                                  "Commented by AKMADASU
*        CONDENSE gs_alv_output-anbtr NO-GAPS.   "Commented by AKMADASU
*        SHIFT gs_alv_output-anbtr RIGHT.        "Commented by AKMADASU
        APPEND gs_alv_output TO gt_alv_output.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
*Clear Memory
  CLEAR: lt_anek,
         lt_anep,
         lt_anlz,
         lt_anla.
*Prepare table for file output
  LOOP AT gt_alv_output INTO gs_alv_output.
    gs_file_output-comma1 = ','.
    gs_file_output-comma2 = ','.
    gs_file_output-comma3 = ','.
    gs_file_output-comma4 = ','.
    gs_file_output-comma5 = ','.
    gs_file_output-comma6 = ','.
    gs_file_output-comma7 = ','.
    gs_file_output-asset_cate    = 'Actual'.
    gs_file_output-asset_ttype   = gs_alv_output-bwasl.
    gs_file_output-asset_datasrc = 'SAP_AA'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_alv_output-anlkl
      IMPORTING
        output = gs_alv_output-anlkl.
    CONCATENATE 'AC_' gs_alv_output-anlkl INTO gs_file_output-asset_class.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_alv_output-kostl
      IMPORTING
        output = gs_alv_output-kostl.
    gs_file_output-asset_entity  =  gs_alv_output-kostl.
    gs_file_output-asset_plant   =  gs_alv_output-werks.
    CONCATENATE  gs_alv_output-gjahr gs_alv_output-monat '00' INTO gs_file_output-asset_time.
    gs_file_output-asset_amount  =  gs_alv_output-anbtr.
    APPEND gs_file_output TO gt_file_output.
*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
  IF c_pp IS NOT INITIAL.
    gs_file_output_n-comma1 = ','.
    gs_file_output_n-comma2 = ','.
    gs_file_output_n-comma3 = ','.
    gs_file_output_n-comma4 = ','.
    gs_file_output_n-comma5 = ','.
    gs_file_output_n-comma6 = ','.
    gs_file_output_n-comma7 = ','.
    gs_file_output_n-comma8 = ','.
    gs_file_output_n-comma9 = ','.
    gs_file_output_n-comma10 = ','.
    gs_file_output_n-comma11 = ','.
    gs_file_output_n-comma12 = ','.
    gs_file_output_n-comma13 = ','.
    gs_file_output_n-comma14 = ','.
    gs_file_output_n-comma15 = ','.
    gs_file_output_n-comma16 = ','.
    gs_file_output_n-comma17 = ','.
    gs_file_output_n-comma18 = ','.
    gs_file_output_n-erdat = gs_alv_output-erdat.
    gs_file_output_n-ernam = gs_alv_output-ernam.
    gs_file_output_n-ktogr = gs_alv_output-ktogr.
    gs_file_output_n-sernr = gs_alv_output-sernr.
    gs_file_output_n-zujhr = gs_alv_output-zujhr.
    gs_file_output_n-posnr = gs_alv_output-posnr.
    gs_file_output_n-ord41 = gs_alv_output-ord41.
    gs_file_output_n-ord42 = gs_alv_output-ord42.
    gs_file_output_n-ord43 = gs_alv_output-ord43.
    gs_file_output_n-hkont = gs_alv_output-hkont.
    gs_file_output_n-shkzg = gs_alv_output-shkzg.
    gs_file_output_n-wrbtr = gs_alv_output-wrbtr.
    gs_file_output_n-waers = gs_alv_output-waers.
    gs_file_output_n-bschl = gs_alv_output-bschl.
    gs_file_output_n-belnr = gs_alv_output-belnr.
    gs_file_output_n-sgtxt = gs_alv_output-sgtxt.
    gs_file_output_n-xarch = gs_alv_output-xarch.
    gs_file_output_n-bzdat = gs_alv_output-bzdat.
    gs_file_output_n-tcode = gs_alv_output-tcode.
    APPEND gs_file_output_n TO gt_file_output_n.
    ENDIF.
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_data .
  IF r_pc = 'X'.
    PERFORM pc_download.
  ELSE.
    PERFORM server_download.
  ENDIF.
ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  PC_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pc_download .
  DATA: lv_filename TYPE string.
*Download Canadian file
  MOVE p_file TO lv_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
      trunc_trailing_blanks_eol = space "'X'
    CHANGING
      data_tab                  = gt_file_output
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
*       TITEL =
        msgid = 'PG'
        msgty = 'I'
        msgno = '016'
        msgv1 = 'Error with downloading CSV file at PC '
*       MSGV2 =
*       MSGV3 =
*       MSGV4 =
      .

*    WRITE: / 'Error with downloading CSV file at PC ', sy-subrc.
  ENDIF.
ENDFORM.                    " PC_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  SERVER_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM server_download .

  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
*       TITEL =
        msgid = 'PG'
        msgty = 'I'
        msgno = '016'
        msgv1 = 'Unable to open CSV file for output on Server.'
*       MSGV2 =
*       MSGV3 =
*       MSGV4 =
      .
*    WRITE:/ 'Unable to open text file for output.'.
  ELSE.
    LOOP AT gt_file_output INTO gs_file_output.
      TRANSFER gs_file_output TO p_sfile.
    ENDLOOP.
    CLOSE DATASET p_sfile.
  ENDIF.
******************Blank Touch File
  OPEN DATASET p_tcfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
*       TITEL =
        msgid = 'PG'
        msgty = 'I'
        msgno = '016'
        msgv1 = 'Unable to open Touch file for output on Server.'
*       MSGV2 =
*       MSGV3 =
*       MSGV4 =
      .

  ELSE.
    CLOSE DATASET p_tcfile.
  ENDIF.
*BOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
  IF c_pp IS NOT INITIAL.
******************PP Extract File
  OPEN DATASET p_pp FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
*       TITEL =
        msgid = 'PG'
        msgty = 'I'
        msgno = '016'
        msgv1 = 'Unable to open CSV file for output on Server.'
*       MSGV2 =
*       MSGV3 =
*       MSGV4 =
      .
*    WRITE:/ 'Unable to open text file for output.'.
  ELSE.
    LOOP AT gt_file_output_n INTO gs_file_output_n.
      TRANSFER gs_file_output_n TO p_pp.
    ENDLOOP.
    CLOSE DATASET p_pp.
  ENDIF.
  ENDIF.
*EOC by AKMADASU on 19.6.2018 for PP Design builds for UGL
ENDFORM.                    " SERVER_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv .
  DATA:   ls_key         TYPE salv_s_layout_key,
          lo_table       TYPE REF TO cl_salv_table,
          lo_layout      TYPE REF TO cl_salv_layout,
          lo_functions   TYPE REF TO cl_salv_functions,
          lo_display     TYPE REF TO cl_salv_display_settings,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column_table,  "#EC NEEDED
          lo_content     TYPE REF TO cl_salv_form_element,
          lo_grid        TYPE REF TO cl_salv_form_layout_grid,
          lo_events_salv TYPE REF TO cl_salv_events_table,
          lo_event       TYPE REF TO lcl_event_handler.

  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*          r_container    = lr_con1
*          container_name = 'ALV_CON1'
        IMPORTING
          r_salv_table   = lo_table
        CHANGING
          t_table        = gt_alv_output.
    CATCH cx_salv_msg .                                 "#EC NO_HANDLER
  ENDTRY.
*Function settings
  lo_functions = lo_table->get_functions( ).
  lo_functions->set_all( abap_true ).
*Display Setting
  lo_display = lo_table->get_display_settings( ).

  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
*Event
  lo_events_salv = lo_table->get_event( ).
  CREATE OBJECT lo_event.
  SET HANDLER: lo_event->hotspot_click
               FOR lo_events_salv.
*Set layout
  lo_layout = lo_table->get_layout( ).
  ls_key-report = sy-repid.
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*  CALL METHOD lo_layout->set_initial_layout
*    EXPORTING
*      value = p_vari.
*Get columns
  CALL METHOD lo_table->get_columns
    RECEIVING
      value = lo_columns.
*****Change ALV Fields  - title etc.
  PERFORM alv_fields USING lo_columns lo_column.
******Display ALV
  CALL METHOD lo_table->display.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  HANDLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_click   USING iv_row TYPE salv_de_row
                          iv_column TYPE salv_de_column.

  DATA: ls_alv_output TYPE ty_alv_output.

  READ TABLE gt_alv_output INTO ls_alv_output INDEX iv_row.
  CHECK sy-subrc = 0.
*  CASE iv_column.
*      WHEN 'ANLN1'.
  SET PARAMETER ID 'AN1' FIELD ls_alv_output-anln1.
  SET PARAMETER ID 'AN2' FIELD ls_alv_output-anln2.
  SET PARAMETER ID 'BUK' FIELD ls_alv_output-bukrs.
  CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
*  ENDCASE.


ENDFORM.                    " HANDLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_fields  USING    io_columns TYPE REF TO cl_salv_columns_table
                          io_column  TYPE REF TO cl_salv_column_table.
  DATA: lv_short_text TYPE scrtext_s,
          lv_med_text TYPE scrtext_m,
          lv_long_text TYPE scrtext_l.
*****hot spot
  TRY.
      io_column ?= io_columns->get_column( 'ANLN1' ).
      CALL METHOD io_column->set_cell_type
        EXPORTING
          value = if_salv_c_cell_type=>hotspot.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      io_column ?= io_columns->get_column( 'ANBTR' ).
      CALL METHOD io_column->set_alignment
        EXPORTING
          value = if_salv_c_alignment=>right.
      lv_short_text = 'Amount'.
      lv_med_text = 'Amount'.
      lv_long_text = 'Amount'.
      CALL METHOD io_column->set_short_text
        EXPORTING
          value = lv_short_text.
      CALL METHOD io_column->set_medium_text
        EXPORTING
          value = lv_med_text.
      CALL METHOD io_column->set_long_text
        EXPORTING
          value = lv_long_text.
    CATCH cx_salv_not_found .
  ENDTRY.
***Start of change by AKMADASU
**Hide culumn
   TRY.
      io_column ?= io_columns->get_column( 'WRBTR' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
IF c_pp IS INITIAL.
******************  1
     TRY.
      io_column ?= io_columns->get_column( 'HKONT' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  2
     TRY.
      io_column ?= io_columns->get_column( 'SHKZG' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  3
     TRY.
      io_column ?= io_columns->get_column( 'WRBTR' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  4
     TRY.
      io_column ?= io_columns->get_column( 'WAERS' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  5
     TRY.
      io_column ?= io_columns->get_column( 'BSCHL' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  6
     TRY.
      io_column ?= io_columns->get_column( 'BZDAT' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  7
     TRY.
      io_column ?= io_columns->get_column( 'TCODE' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  8
     TRY.
      io_column ?= io_columns->get_column( 'BELNR' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  9
     TRY.
      io_column ?= io_columns->get_column( 'SGTXT' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  10
     TRY.
      io_column ?= io_columns->get_column( 'XARCH' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  11
     TRY.
      io_column ?= io_columns->get_column( 'ERDAT' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  12
     TRY.
      io_column ?= io_columns->get_column( 'ERNAM' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  13
     TRY.
      io_column ?= io_columns->get_column( 'KTOGR' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
*ENDIF.
******************  14
     TRY.
      io_column ?= io_columns->get_column( 'SERNR' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  15
     TRY.
      io_column ?= io_columns->get_column( 'ZUJHR' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  16
     TRY.
      io_column ?= io_columns->get_column( 'POSNR' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  17
     TRY.
      io_column ?= io_columns->get_column( 'ORD41' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  18
     TRY.
      io_column ?= io_columns->get_column( 'ORD42' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
******************  19
     TRY.
      io_column ?= io_columns->get_column( 'ORD43' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  20
     TRY.
      io_column ?= io_columns->get_column( 'TXT50' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  21
     TRY.
      io_column ?= io_columns->get_column( 'TXA50' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  22
     TRY.
      io_column ?= io_columns->get_column( 'MENGE' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  23
     TRY.
      io_column ?= io_columns->get_column( 'ZUGDT' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.

******************  24
     TRY.
      io_column ?= io_columns->get_column( 'AKTIV' ).
      CALL METHOD io_column->SET_VISIBLE
        EXPORTING
          value =  if_salv_c_bool_sap=>false.
    CATCH cx_salv_not_found .
  ENDTRY.
ENDIF.
***End of change by AKMADASU
ENDFORM.                    "alv_fields
*&---------------------------------------------------------------------*
*&      Form  GET_PC_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_pc_file .

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',DAT File,*.csv'
      static    = 'X'
    CHANGING
      file_name = p_file.
ENDFORM.                    " GET_PC_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_SERVER_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_server_file .

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    EXPORTING
*     DIRECTORY        = ' '
      filemask         = '*.*'
    IMPORTING
      serverfile       = p_sfile
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " GET_SERVER_FILE
