*&---------------------------------------------------------------------*
*&  Include           ZFI_READ_MD_FOR_IS_SCR
*&---------------------------------------------------------------------*

PARAMETERS: p_tabs TYPE dd02l-tabname AS LISTBOX VISIBLE LENGTH 40 DEFAULT 'ADR6' USER-COMMAND tab .

SELECTION-SCREEN BEGIN OF BLOCK pkg WITH FRAME TITLE text-022.
PARAMETERS: p_nowhr AS CHECKBOX.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_nr  RADIOBUTTON GROUP rd5 USER-COMMAND rd4. " Read all database records
SELECTION-SCREEN COMMENT (20) text-rb1.
PARAMETERS: p_nrec TYPE i.  "No. of records
PARAMETERS: p_pk  RADIOBUTTON GROUP rd5.         " Read data by package
SELECTION-SCREEN COMMENT 41(10) text-rb2.
PARAMETERS: p_pkg TYPE i DEFAULT '1000000000'.       " Package Size
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_rec(6) TYPE n.  "Single record size

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rbr  RADIOBUTTON GROUP rd5.        " Read data record br record
SELECTION-SCREEN COMMENT (40) text-rb3.
PARAMETERS: p_rn TYPE i DEFAULT '200000'.  "Record Size
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK pkg.

SELECTION-SCREEN BEGIN OF BLOCK alvf WITH FRAME TITLE text-017.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rd_alv  RADIOBUTTON GROUP rd1 USER-COMMAND rd1 MODIF ID alv. " Display ALV report
SELECTION-SCREEN COMMENT (12) text-cb1 MODIF ID alv.
PARAMETERS: rd_file RADIOBUTTON GROUP rd1 DEFAULT 'X' MODIF ID alf.  "Write Files
SELECTION-SCREEN COMMENT 18(12) text-cb2 MODIF ID alf.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK alvf.

SELECTION-SCREEN BEGIN OF BLOCK fph WITH FRAME TITLE text-020.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rd_pc  RADIOBUTTON GROUP rd4 USER-COMMAND rd4 MODIF ID rd4. " Generate PC file
SELECTION-SCREEN COMMENT (15) text-cb8   MODIF ID rd4.
PARAMETERS: rd_app RADIOBUTTON GROUP rd4 MODIF ID rd5 DEFAULT 'X'.      " Generate app server file
SELECTION-SCREEN COMMENT 21(25) text-cb7 MODIF ID rd5.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_file  TYPE localfile MODIF ID rd6 DEFAULT 'H:\MDM_Data_Profiling_Samples\'.  " PC File Name
PARAMETERS: p_appf  TYPE localfile MODIF ID rd7.                " Application server File Name
PARAMETERS: p_man   AS CHECKBOX MODIF ID rd7 USER-COMMAND man.  " Application server file path manual override
SELECTION-SCREEN END OF BLOCK fph.

SELECTION-SCREEN BEGIN OF BLOCK fspl WITH FRAME TITLE text-016.
PARAMETERS: p_recs  TYPE i DEFAULT '200000' MODIF ID rd2 ,            " Split by No of records
            rd_xlsx RADIOBUTTON GROUP rd3  MODIF ID rd2, "            .XLSX file format
            rd_xls  RADIOBUTTON GROUP rd3  MODIF ID rd2,              " .XLS file format
            rd_csv  RADIOBUTTON GROUP rd3 MODIF ID rd2 DEFAULT 'X',             " .csv file format
            rd_tab  RADIOBUTTON GROUP rd3 MODIF ID rd2 .             " Tab delimited file


SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rd_delim  RADIOBUTTON GROUP rd3  MODIF ID rd2.           " Delimiter input from user
SELECTION-SCREEN COMMENT (14) text-cb9   MODIF ID rd2.
PARAMETERS: p_delim(4) TYPE c MODIF ID rd2.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_rcom TYPE char2. " Replace Comma with
SELECTION-SCREEN END OF BLOCK fspl.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-100.
SELECT-OPTIONS: s_lifnr FOR gv_lifnr .
SELECT-OPTIONS: s_bukrs FOR gv_bukrs .
SELECT-OPTIONS: s_pspnr FOR gv_pspnr .
SELECT-OPTIONS: s_prps  FOR gv_prps.
SELECT-OPTIONS: s_aufnr FOR gv_aufnr.
SELECT-OPTIONS: s_werks FOR gv_werks.
SELECT-OPTIONS: s_stand FOR gv_stand.
SELECT-OPTIONS: s_tplnr FOR gv_tplnr.
SELECT-OPTIONS: s_banks FOR gv_banks.
SELECT-OPTIONS: s_bankl FOR gv_bankl.
SELECT-OPTIONS: s_hbkid FOR gv_hbkid.
SELECT-OPTIONS: s_kunnr FOR gv_kunnr.
SELECT-OPTIONS: s_lgort FOR gv_lgort.
SELECT-OPTIONS: s_matnr FOR gv_matnr.
SELECT-OPTIONS: s_anln1 FOR gv_anln1.
SELECT-OPTIONS: s_anln2 FOR gv_anln1.
SELECT-OPTIONS: s_equnr FOR equi-equnr.
SELECT-OPTIONS: s_datbi	FOR equz-datbi.
SELECT-OPTIONS: s_eqlfn	FOR equz-eqlfn.
SELECT-OPTIONS: s_spras FOR eqkt-spras.
SELECT-OPTIONS: s_iloan FOR iloa-iloan.
SELECT-OPTIONS: s_stlan	FOR eqst-stlan.
SELECT-OPTIONS: s_stlnr	FOR eqst-stlnr.
SELECT-OPTIONS: s_stlal	FOR eqst-stlal.
SELECT-OPTIONS: s_objnr FOR jest-objnr.
SELECT-OPTIONS: s_stat  FOR jest-stat.
SELECT-OPTIONS: s_objek	FOR ausp-objek.
SELECT-OPTIONS: s_atinn	FOR ausp-atinn.
SELECT-OPTIONS: s_atzhl	FOR ausp-atzhl.
SELECT-OPTIONS: s_mafid	FOR ausp-mafid.
SELECT-OPTIONS: s_klart	FOR ausp-klart.
SELECT-OPTIONS: s_adzhl	FOR ausp-adzhl.
SELECT-OPTIONS: s_pnum1 FOR prop-pnum1.
SELECT-OPTIONS: s_hsnum FOR prop-hsnum.
SELECT-OPTIONS: s_versp FOR prop-versp.
SELECT-OPTIONS: s_infnr FOR eina-infnr.
SELECT-OPTIONS: s_bwkey	FOR mbew-bwkey.
SELECT-OPTIONS: s_bwtar	FOR mbew-bwtar.
SELECT-OPTIONS: s_adr61 FOR adr6-addrnumber.
SELECT-OPTIONS: s_adr62 FOR adr6-persnumber.
SELECT-OPTIONS: s_adr63 FOR adr6-date_from.
SELECT-OPTIONS: s_adr64 FOR adr6-consnumber.
SELECT-OPTIONS: s_ebeln FOR ekko-ebeln.
SELECT-OPTIONS: s_ekorg FOR lfm1-ekorg.
SELECT-OPTIONS: s_kappl	FOR nast-kappl.
SELECT-OPTIONS: s_objky	FOR nast-objky.
SELECT-OPTIONS: s_kschl	FOR nast-kschl.
SELECT-OPTIONS: s_parnr	FOR nast-parnr.
SELECT-OPTIONS: s_parvw	FOR nast-parvw.
SELECT-OPTIONS: s_erdat	FOR nast-erdat.
SELECT-OPTIONS: s_eruhr	FOR nast-eruhr.
SELECT-OPTIONS: s_ekgrp FOR t024-ekgrp.
SELECT-OPTIONS: s_ebelp FOR ekpo-ebelp.
SELECT-OPTIONS: s_etenr FOR eket-etenr.
SELECT-OPTIONS: s_belnr FOR bkpf-belnr.
SELECT-OPTIONS: s_gjahr FOR bkpf-gjahr.
SELECT-OPTIONS: s_buzei FOR bseg-buzei.
SELECT-OPTIONS: s_istat FOR tj02t-istat.
SELECT-OPTIONS: s_lednr	FOR bpja-lednr.
SELECT-OPTIONS: s_posit	FOR bpja-posit.
SELECT-OPTIONS: s_trgkz	FOR bpja-trgkz.
SELECT-OPTIONS: s_wrttp	FOR bpja-wrttp.
SELECT-OPTIONS: s_geber	FOR bpja-geber.
SELECT-OPTIONS: s_versn	FOR bpja-versn.
SELECT-OPTIONS: s_vorga	FOR bpja-vorga.
SELECT-OPTIONS: s_twaer	FOR bpja-twaer.
SELECT-OPTIONS: s_subvo	FOR bpja-subvo.
SELECT-OPTIONS: s_gnjhr	FOR bpja-gnjhr.
SELECT-OPTIONS: s_farea	FOR bpja-farea.
SELECT-OPTIONS: s_posnr FOR prte-posnr.
SELECT-OPTIONS: s_blart FOR bkpf-blart.
SELECT-OPTIONS: s_bldat FOR bkpf-bldat.
SELECT-OPTIONS: s_budat FOR bkpf-budat.
SELECT-OPTIONS: s_monat FOR bkpf-monat.
SELECT-OPTIONS: s_xblnr FOR bkpf-xblnr.
SELECT-OPTIONS: s_dbblg FOR bkpf-dbblg.
SELECT-OPTIONS: s_stblg FOR bkpf-stblg.
SELECT-OPTIONS: s_stjah FOR bkpf-stjah.
SELECT-OPTIONS: s_bstat FOR bkpf-bstat.
SELECT-OPTIONS: s_ausbk FOR vbsegk-ausbk.
SELECT-OPTIONS: s_bzkey FOR vbsegk-bzkey.
SELECT-OPTIONS: s_umsks FOR bsik-umsks.
SELECT-OPTIONS: s_umskz FOR bsik-umskz.
SELECT-OPTIONS: s_augdt FOR bsik-augdt.
SELECT-OPTIONS: s_augbl FOR bsik-augbl.
SELECT-OPTIONS: s_zuonr FOR bsik-zuonr.
SELECT-OPTIONS: s_kokrs FOR coej-kokrs.
SELECT-OPTIONS: s_perbl FOR coej-perbl.
SELECT-OPTIONS: s_kostl FOR csks-kostl.
SELECT-OPTIONS: s_zekkn FOR ekkn-zekkn.
SELECT-OPTIONS: s_ihpa3 FOR ihpa-counter.
SELECT-OPTIONS: s_clint FOR kssk-clint.
SELECT-OPTIONS: s_msehi FOR t006-msehi.
SELECT-OPTIONS: s_matkl FOR t023t-matkl.
SELECT-OPTIONS: s_vernr FOR tcj04-vernr.
SELECT-OPTIONS: s_obtyp FOR tj03-obtyp.
SELECT-OPTIONS: s_langu FOR tkt09-langu.
SELECT-OPTIONS: s_mblnr	FOR mseg-mblnr.
SELECT-OPTIONS: s_mjahr	FOR mseg-mjahr.
SELECT-OPTIONS: s_zeile	FOR mseg-zeile.
SELECT-OPTIONS: s_cuobj	FOR inob-cuobj.
SELECT-OPTIONS: s_relid    FOR stxl-relid.
SELECT-OPTIONS: s_tdobje   FOR stxl-tdobject.
SELECT-OPTIONS: s_tdname   FOR stxl-tdname.
SELECT-OPTIONS: s_tdid     FOR stxl-tdid.
SELECT-OPTIONS: s_tdspra   FOR stxl-tdspras.
SELECT-OPTIONS: s_srtf2    FOR stxl-srtf2.
SELECT-OPTIONS: s_stlty    FOR stas-stlty.
SELECT-OPTIONS: s_stlkn    FOR stas-stlkn.
SELECT-OPTIONS: s_stasz    FOR stas-stasz.
SELECT-OPTIONS: s_qmnum    FOR qmfe-qmnum.
SELECT-OPTIONS: s_fenum    FOR qmfe-fenum.
SELECT-OPTIONS: s_sapobj FOR toa01-sap_object.
SELECT-OPTIONS: s_aobjid  FOR toa01-object_id.
SELECT-OPTIONS: s_archid  FOR toa01-archiv_id.
SELECT-OPTIONS: s_arcdid FOR toa01-arc_doc_id.
SELECT-OPTIONS: s_dokar	 FOR drad-dokar.
SELECT-OPTIONS: s_doknr	 FOR drad-doknr.
SELECT-OPTIONS: s_obky   FOR drad-objky.
SELECT-OPTIONS: s_pspnr2  FOR prps-pspnr.
SELECT-OPTIONS: s_pbukr   FOR prps-pbukr.
SELECT-OPTIONS: s_dokar2  FOR drat-dokar.
SELECT-OPTIONS: s_doknr2  FOR drat-doknr.
SELECT-OPTIONS: s_dokvr	  FOR drat-dokvr.
SELECT-OPTIONS: s_doktl	  FOR drat-doktl.
SELECT-OPTIONS: s_langu2  FOR drat-langu.
SELECT-OPTIONS: s_objyr   FOR srgbtbrel-reltype.
SELECT-OPTIONS: s_objtp   FOR srgbtbrel-instid_a.
SELECT-OPTIONS: s_objno   FOR srgbtbrel-typeid_a.
SELECT-OPTIONS: s_tidb    FOR srgbtbrel-typeid_b.
SELECT-OPTIONS: s_aufpl  	FOR afvc-aufpl.
SELECT-OPTIONS: s_aplzl	  FOR afvc-aplzl.

SELECTION-SCREEN END OF BLOCK sel.

AT SELECTION-SCREEN OUTPUT.

  PERFORM create_tables_list.   " Create Drop-down list for Table selection
  PERFORM fill_selection_texts. " Populate Selection texts dynamically
  PERFORM get_table_keys.       " Get Table keys for table key fields
  PERFORM modify_scrn.          " Modify screen parameters

  IF rd_file EQ 'X' AND rd_app EQ 'X' AND p_man EQ ''.
    PERFORM get_filename.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM browse_directory USING p_file.

AT SELECTION-SCREEN.
  IF p_tabs EQ ''.
    MESSAGE 'You MUST enter Table Name' TYPE 'E'.
  ENDIF.

  IF p_tabs EQ 'STXL' AND p_nr EQ 'X'.
    MESSAGE 'You MUST select "By Package" for STXL' TYPE 'E'.
  ENDIF.

  IF p_tabs EQ 'SOOD' AND p_nr EQ 'X'.
    MESSAGE 'You MUST select "By Package" for SOOD' TYPE 'E'.
  ENDIF.

  IF p_tabs EQ 'SRGBTBREL' AND p_nr EQ 'X'.
    MESSAGE 'You MUST select "By Package" for SRGBTBREL' TYPE 'E'.
  ENDIF.

  IF rd_delim EQ 'X' AND p_delim EQ ''.
    MESSAGE 'You MUST enter file Delimiter' TYPE 'E'.
  ENDIF.
