*&---------------------------------------------------------------------*
*&  Include           ZFI_TUESDAY_PROCESS_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_TUESDAY_PROCESS                           *
* Program Include    :   ZFI_TUESDAY_PROCESS_F01                       *
* Author             :                                                 *
* Date               :   Oct 17, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   PRRW Tuesday Process Automation               *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 17-Oct-2018  CPALYAM     D30K929184-Initial development              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 14-AUG-2020  KMB          D30K930651 CHG0188574 ZFI_MONDAY_PROCESS   *
*                                      ZFI_TUESDAY_PROCESS are not able*
*                                      to write to SAP Fileshare in US *
*                                      s/m & to UNIX servr in UG/SW s/m*
*                           D30K930663                                 *
*                           D30K930685                                 *
*                           D30K930693                                 *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_PRRW
*&---------------------------------------------------------------------*
*       To get data from PRRW Transaction output
*----------------------------------------------------------------------*
FORM f_get_prrw .
  DATA: ls_opt           TYPE ctu_params,
        lv_date(10)      TYPE c.

  CONSTANTS lc_prrw(4)   TYPE c VALUE 'PRRW'.

  CLEAR: ls_opt,
         gt_bdcdata[],
         gt_messages[],
         gt_nod[],
         gt_partial[],
         gt_posted[].

  ls_opt-dismode = 'N'.
  ls_opt-defsize = gc_x.
  ls_opt-updmode = 'S'.

  PERFORM f_bdc_dynpro      USING 'SAPMP56F' '0100'.
  PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                'CREA_SO-LOW'.
  PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                '=CRET'.
  PERFORM f_bdc_field       USING 'CREA_SO-LOW'
                                '*'.

  CLEAR lv_date.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal            = sy-datum
    IMPORTING
      date_external            = lv_date
    EXCEPTIONS
      date_internal_is_invalid = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  PERFORM f_bdc_field       USING 'CRDAT_SO-LOW'
                                lv_date.

  PERFORM f_bdc_dynpro      USING 'SAPMSSY0' '0120'.
  PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                '=&F03'.
  PERFORM f_bdc_dynpro      USING 'SAPMP56F' '0100'.
  PERFORM f_bdc_field       USING 'BDC_OKCODE'
                                '/ECBAC'.
  PERFORM f_bdc_field       USING 'BDC_CURSOR'
                                'CREA_SO-LOW'.

  IF NOT gt_bdcdata[] IS INITIAL.

    CALL TRANSACTION lc_prrw USING         gt_bdcdata
                             OPTIONS FROM  ls_opt
                             MESSAGES INTO gt_messages.

    IMPORT result_table FROM MEMORY ID 'MID_PRRW'.

    LOOP AT result_table.
      CLEAR: gs_nod,
             gs_partial.

      IF result_table-status EQ 11.       " No documents created
        gs_nod = result_table.
        APPEND gs_nod TO gt_nod.
      ELSEIF result_table-status EQ 35.   " Partially posted
        gs_partial = result_table.
        APPEND gs_partial TO gt_partial.
      ELSEIF result_table-status EQ 50 OR " Documents posted
             result_table-status EQ 22.   " Zero postings posted
        APPEND result_table TO gt_posted.
      ENDIF.
    ENDLOOP.

    FREE MEMORY ID 'MID_PRRW'.

  ENDIF.

ENDFORM.                    " F_GET_PRRW
*&---------------------------------------------------------------------*
*&      form f_bdc_dynpro
*&---------------------------------------------------------------------*
FORM f_bdc_dynpro USING iv_program TYPE any
                        iv_dynpro TYPE any.

  CLEAR gs_bdcdata.
  gs_bdcdata-program = iv_program.
  gs_bdcdata-dynpro = iv_dynpro.
  gs_bdcdata-dynbegin = 'X'.
  APPEND gs_bdcdata TO gt_bdcdata.

ENDFORM.                    "f_bdc_dynpro
*&---------------------------------------------------------------------*
*&      form f_bdc_field
*&---------------------------------------------------------------------*
FORM f_bdc_field USING iv_fnam TYPE any
                       iv_fval TYPE any.
  DATA lv_fval(150).
  lv_fval = iv_fval.
  CONDENSE lv_fval.

  CLEAR gs_bdcdata.
  gs_bdcdata-fnam = iv_fnam.
  gs_bdcdata-fval = lv_fval.
  APPEND gs_bdcdata TO gt_bdcdata.

ENDFORM.                    "f_bdc_field
*&---------------------------------------------------------------------*
*&      Form  F_DELETE_RUN
*&---------------------------------------------------------------------*
*       To delete the data (copied from Standard)
*----------------------------------------------------------------------*
FORM f_delete_run .

  LOOP AT gt_nod INTO gs_nod.
    IF result_table-simu <> 'X'.
      SELECT * FROM ptrv_perio INTO TABLE ptrv_perio_itab
               WHERE runid = gs_nod-runid.
      IF sy-subrc = 0.
        LOOP AT ptrv_perio_itab.
          CLEAR ptrv_perio_itab-uebrf.
          CLEAR ptrv_perio_itab-runid.
          MODIFY ptrv_perio_itab TRANSPORTING uebrf runid.
        ENDLOOP.
        MODIFY ptrv_perio FROM TABLE ptrv_perio_itab.
      ENDIF.
    ENDIF.

*  ... delete from PEVSH, PEVST, PEVAT
    CALL FUNCTION 'HR_EVAL_RUN_DELETE'
      EXPORTING
        type  = 'TR'
        runid = gs_nod-runid.

    SELECT * FROM ptrv_doc_hd INTO TABLE ptrv_doc_hd_itab
             WHERE runid = gs_nod-runid.

    IF sy-subrc = 0.
      IF NOT ptrv_doc_hd_itab[] IS INITIAL.

        SELECT * FROM ptrv_doc_it INTO TABLE ptrv_doc_it_itab
               FOR ALL ENTRIES IN ptrv_doc_hd_itab
                 WHERE awref = ptrv_doc_hd_itab-awref
                   AND aworg = ptrv_doc_hd_itab-aworg.

        SELECT * FROM ptrv_doc_tax INTO TABLE ptrv_doc_tax_itab
               FOR ALL ENTRIES IN ptrv_doc_hd_itab
                 WHERE awref = ptrv_doc_hd_itab-awref
                   AND aworg = ptrv_doc_hd_itab-aworg.

        LOOP AT ptrv_doc_hd_itab.
          DELETE FROM ptrv_doc_mess
                 WHERE awref = ptrv_doc_hd_itab-awref.
        ENDLOOP.
      ENDIF.
    ENDIF.

    DELETE ptrv_doc_hd FROM TABLE ptrv_doc_hd_itab.
    DELETE ptrv_doc_it FROM TABLE ptrv_doc_it_itab.
    DELETE ptrv_doc_tax FROM TABLE ptrv_doc_tax_itab.
    DELETE FROM ptrv_rot_awkey WHERE runid = gs_nod-runid.
    DELETE FROM ptrv_rprpostd WHERE runid = gs_nod-runid.

    DELETE result_table WHERE runid = result_table-runid.
  ENDLOOP.

ENDFORM.                    " F_DELETE_RUN
*&---------------------------------------------------------------------*
*&      Form  F_DOC_READ
*&---------------------------------------------------------------------*
*       To read the next level doc details (copied from Standard)
*----------------------------------------------------------------------*
FORM f_doc_read
                TABLES   it_result            TYPE gtt_evrun
                USING    iv_flag              TYPE c.

  TYPES: BEGIN OF lt_final,
         icon(15),                    " Status
         runid      TYPE p_evnum,     " Posting run number
         awref      TYPE awref,       " Posting document
         awlin      TYPE num10,       " Posting doc. line
         pernr      TYPE pernr_d,     " Personnel Number
         exbel      TYPE hr_zuonr,    " Assignment
         bukrs      TYPE bukrs,       " Company Code
         blart      TYPE hr_bldat,    " Document Type
         budat      TYPE hr_budat,    " Posting date
         ktosl      TYPE ktosl,       " Transaction key
         hkont      TYPE hkont,       " Account number
         hkonttxt   TYPE txt20_skat,  " Account name
         waers      TYPE waers,       " Currency
         wrbtr      TYPE acbtr,       " Gross amount
         mwskz      TYPE mwskz,       " Tax code
         hkonttax   TYPE hkont,       " Gen.ledger accnt (tax)
         steur_soll TYPE acbtr,       " Tax debits
         steur_haben TYPE acbtr,      " Tax credits
         sgtxt      TYPE sgtxt,       " Text
         kokrs      TYPE kokrs,       " Controlling Area
         kostl      TYPE kostl,       " Cost Center
         aufnr      TYPE aufnr,       " Order
         kstrg      TYPE kstrg,       " Cost Object
         posnr      TYPE ps_psp_pnr,  " WBS element
         nplnr      TYPE nplnr,       " Network
         umdat      TYPE ptrv_dattn,  " Reference Date of Trip
         antrg      TYPE antrg,       " Request/Trip Ind.
         abrec      TYPE abrec,       " Settlement Status
         fwbas      TYPE acbtr,       " Net amount
         END OF   lt_final,

         ltt_final  TYPE STANDARD TABLE OF lt_final,

         BEGIN OF lt_fname,
         fname(25)  TYPE c,
         END OF   lt_fname,

         ltt_fname  TYPE STANDARD TABLE OF lt_fname.

  DATA lv_path      TYPE string.

  TABLES: ptrv_doc_hd.
  "SELECT-OPTIONS: p_runid FOR ptrv_doc_hd-runid.
  DATA: BEGIN OF grundliste OCCURS 0,
*        selected_row,                                      "WBGK040881
          selected_row LIKE ptk04-uebern,                   "WBGK040881
*       icon  LIKE ptrv_approval-icon, "Status  "WBGK049386 "TEMP
          icon(15),                                            "TEMP
          runid LIKE ptrv_doc_hd-runid,  "Buchungslaufnummer
          awref LIKE ptrv_doc_it-awref,  "Awkey
          aworg LIKE ptrv_doc_it-aworg,  "Awkey
          awlin LIKE ptrv_doc_it-awlin,  "Awkey
          uebnr LIKE ptrv_doc_hd-uebnr,  "übergreifende Nr.   "WBGK002801
          budat LIKE ptrv_doc_hd-budat,  "Buchungsdatum
          bldat LIKE ptrv_doc_hd-bldat,  "Belegdatum
          xblnr LIKE ptrv_doc_hd-xblnr,  "Referenz-Belegnummer
          blart LIKE ptrv_doc_hd-blart,  "Belegart
          hkont LIKE ptrv_doc_it-hkont,  "KontoNr.
          hkonttxt LIKE ptrv_epk-ktext,  "Kontobezeichnung
          kunnr LIKE ptrv_doc_it-kunnr,  "Debitor
          kunnrtxt LIKE ptrv_epk-ktext,  "Name zum Debitor
          lifnr LIKE ptrv_doc_it-lifnr,  "Kreditor
          lifnrtxt LIKE ptrv_epk-ktext,  "Name zum Kreditor
          vbund LIKE ptrv_doc_it-vbund,  "Partnergesellschaft "WKUK029301
          kontobez LIKE ptrv_epk-ktext,  "alle 3 Kontonamen
          curtp LIKE ptrv_doc_it-curtp,  "Währungstyp
          wrbtr LIKE ptrv_doc_it-wrbtr,  "Brutto
          fwbas LIKE ptrv_doc_it-fwbas,  "Netto = Brutto - kon.Vorst.
          netto_soll LIKE ptrv_doc_it-wrbtr,
          steur_soll LIKE ptrv_doc_tax-wrbtr,
          netto_haben LIKE ptrv_doc_it-wrbtr,
          steur_haben LIKE ptrv_doc_tax-wrbtr,
          hkonttax LIKE ptrv_doc_tax-hkont,   "Nr. Steuerkonto
          brutto_soll LIKE ptrv_doc_it-wrbtr,
          brutto_haben LIKE ptrv_doc_it-wrbtr,
          mwskz LIKE ptrv_doc_it-mwskz,  "Umsatzst.kennz.
          txjcd LIKE ptrv_doc_it-txjcd,  "Tax Jurisdiction Code
          pernr LIKE ptrv_doc_it-pernr,  "Personalnummer
          waers LIKE ptrv_doc_it-waers,  "Währung
          bukrs LIKE ptrv_doc_it-bukrs,  "Buchungskreis
          gsber LIKE ptrv_doc_it-gsber,  "Geschäftsbereich
          kokrs LIKE ptrv_doc_it-kokrs,  "Kostenrechnungskreis
          kostl LIKE ptrv_doc_it-kostl,  "Kostenstelle
          aufnr LIKE ptrv_doc_it-aufnr,  "Auftragsnummer
          kstrg LIKE ptrv_doc_it-kstrg,  "Kostenträger
          posnr LIKE ptrv_doc_it-posnr,                       "PSP
          nplnr LIKE ptrv_doc_it-nplnr,  "Netzplan
          vornr LIKE ptrv_doc_it-vornr,  "Vorgang
          kdauf LIKE ptrv_doc_it-kdauf,  "Kundenauftrag
          kdpos LIKE ptrv_doc_it-kdpos,  "Positionsnummer im kdauf
          fistl LIKE ptrv_doc_it-fistl,  "Finanzstelle
          fipos LIKE ptrv_doc_it-fipos,  "Finanzposition
          fipex LIKE ptrv_doc_it-fipex,  "lange Finanzposition "XCIPSFIPEX
          geber LIKE ptrv_doc_it-geber,  "Fonds
          budget_period LIKE ptrv_doc_it-budget_period,
                                         "Budget Period ZFJ EhP5
          fkber LIKE ptrv_doc_it-fkber,  "Funktionsbereich   "QIZK002508
          grant_nbr LIKE ptrv_doc_it-grant_nbr,  "Grant      "QIZK002508
          prctr LIKE ptrv_doc_it-prctr,  "Profit Center       "WKUK015412
          exbel LIKE ptrv_doc_it-exbel,  "Zuordnungsnummer
          sgtxt LIKE ptrv_doc_it-sgtxt,  "Positionstext
          kokey LIKE ptrv_doc_it-kokey,  "Zuordnung Kontierung
          datv1 LIKE ptrv_doc_it-datv1,  "Reiseabschnitt: Beginndatum
          datb1 LIKE ptrv_doc_it-datb1,  "Reiseabschnitt: Endedatum
          antrg LIKE ptrv_doc_it-antrg,  "Kennzeichen Reiseantrag
          abrec LIKE ptrv_doc_it-abrec,  "Kennzeichen: offen/abzur.
          ktosl LIKE ptrv_doc_it-ktosl,  "Vorgangsschlüssel it
          ktosltax LIKE ptrv_doc_tax-ktosl,   "Vorgangsschlüssel tax
          kschl LIKE ptrv_doc_tax-kschl, "Steuerkondition
          msatz LIKE ptrv_doc_tax-msatz, "MwSt.satz
          gjahr LIKE ptrv_doc_hd-gjahr,  "Geschäftsjahr
          monat LIKE ptrv_doc_hd-monat,  "Geschäftsmonat
          receiver LIKE ptrv_doc_hd-receiver, "ALE-Zielsystem "WBGK034117
          xnegp LIKE ptrv_doc_it-xnegp,  "Negative posting    "VRD_CEE_RU
          umdat LIKE ptrv_doc_it-umdat,  "Reference date      "VRD_CEE_RU
        END OF grundliste.
***


  DATA: BEGIN OF text_for_cost_accnt OCCURS 20,
     bukrs     LIKE grundliste-bukrs,
     skonto    LIKE grundliste-hkont,
     skontotxt LIKE grundliste-hkonttxt,
   END OF   text_for_cost_accnt,

*       Puffern der Namen zum personenbezogenen Kreditor;
   BEGIN OF creditor_getdetail OCCURS 100,
     lifnr     LIKE grundliste-lifnr,
     lifnrtxt  LIKE grundliste-lifnrtxt,
   END OF   creditor_getdetail,

*       Puffern der Namen zum personenbezogenen Debitor;
   BEGIN OF debtor_getdetail OCCURS 100,
     kunnr     LIKE grundliste-kunnr,
     kunnrtxt  LIKE grundliste-kunnrtxt,
   END OF   debtor_getdetail,

*       Progress Indicator;
   lin_p(16) TYPE p DECIMALS 8,
   rest(16)  TYPE p DECIMALS 8,
   show  TYPE i,
   modiv TYPE i,
   sytabix LIKE sy-tabix.

  DATA:
        ptrv_rprpostd_itab                                  "WBGK010620
          LIKE ptrv_rprpostd OCCURS 0 WITH HEADER LINE,     "WBGK010620

        ptrv_doc_tax_itab
          LIKE ptrv_doc_tax OCCURS 0 WITH HEADER LINE,
        tax_puffer
          LIKE ptrv_doc_tax OCCURS 2 WITH HEADER LINE,
        ptrv_doc_hd_itab
          LIKE ptrv_doc_hd OCCURS 0 WITH HEADER LINE.
  DATA: lt_ptrv_cancel_key TYPE TABLE OF ptrv_cancel_key,
        lines_of_tax_puffer TYPE i,
        vendor_info LIKE bapi1008_4,
          custom_info LIKE bapi1007_4.
  FIELD-SYMBOLS: <adr_grndlst> LIKE LINE OF grundliste.
  "<adr_fieldct> LIKE LINE OF fieldcat.

  DATA: lt_grund    TYPE ltt_final,
        lt_grundf   TYPE ltt_final,
        lv_tabix    TYPE sytabix,
        lt_final    TYPE ltt_final,
        ls_final    TYPE lt_final,
        ls_final1   TYPE lt_final,
        lt_fname    TYPE ltt_fname,
        ls_fname    TYPE lt_fname,
        lv_text(8)  TYPE c,
        lv_index(2) TYPE c.

*BOC by KMB on 14.8.2020 CHG0188574
  DATA : lv_string        TYPE string,
         lv_msg           TYPE text100,
         lv_wrbtr TYPE string,
         lv_steur_soll TYPE string,
         lv_steur_haben TYPE string,
         lv_fwbas TYPE string.
*EOC by KMB on 14.8.2020 CHG0188574


  FIELD-SYMBOLS <fs> TYPE any.

  IF NOT it_result[] IS INITIAL. " To duplicates available as runid is key

    CLEAR: grundliste[],
           lt_final[].

    SELECT  a~runid
        b~awref
        b~aworg
        b~awlin
        a~uebnr                                             "WBGK002801
        a~budat
        a~bldat
        a~gjahr
        a~monat
        a~xblnr
        a~blart
        a~receiver                                          "WBGK034117
        b~hkont
        b~kunnr
        b~lifnr
        b~vbund                                             "WKUK029301
        b~curtp
        b~waers
        b~wrbtr
        b~bukrs
        b~gsber
        b~kokrs
        b~kostl
        b~aufnr
        b~kstrg
        b~posnr
        b~nplnr
        b~vornr
        b~kdauf
        b~kdpos
        b~fistl
        b~fipos
        b~geber
        b~budget_period                                 "ZFJ EhP5
        b~fkber                                             "QIZK002508
        b~grant_nbr                                         "QIZK002508
        b~fipex                                         "XCIPSFIPEX
        b~prctr                                             "WKUK015412
        b~exbel
        b~mwskz
        b~pernr
        b~sgtxt
        b~kokey
        b~datv1
        b~datb1
        b~antrg
        b~abrec
        b~txjcd
        b~ktosl
        b~xnegp
        b~umdat
          INTO    CORRESPONDING FIELDS OF TABLE grundliste
FROM    ptrv_doc_hd AS a INNER JOIN ptrv_doc_it AS b
ON      a~awref = b~awref
AND     a~aworg = b~aworg
FOR ALL ENTRIES IN it_result
WHERE   a~runid = it_result-runid.
    " AND     a~awref IN p_awref
    "AND     a~aworg IN p_aworg.

    SELECT awref commi checked FROM ptrv_doc_hd
                       INTO CORRESPONDING FIELDS
                       OF TABLE ptrv_doc_hd_itab
                       FOR ALL ENTRIES IN it_result
                       WHERE runid = it_result-runid.
    "AND   awref IN p_awref.

    SORT ptrv_doc_hd_itab BY awref.

    IF lines( ptrv_doc_hd_itab ) > 0.
      SELECT * FROM ptrv_cancel_key INTO TABLE lt_ptrv_cancel_key FOR ALL
      ENTRIES IN
         ptrv_doc_hd_itab WHERE ( awref_cancelled =
                          ptrv_doc_hd_itab-awref OR
                                  cancel_awref = ptrv_doc_hd_itab-awref ).

    ENDIF.

    SELECT awref type FROM ptrv_rprpostd
                      INTO CORRESPONDING FIELDS
                      OF TABLE ptrv_rprpostd_itab
                      FOR ALL ENTRIES IN it_result
                      WHERE runid = it_result-runid .

    IF NOT ptrv_doc_hd_itab[] IS INITIAL.
      SELECT * FROM ptrv_doc_tax
             APPENDING CORRESPONDING FIELDS OF TABLE ptrv_doc_tax_itab
             FOR ALL ENTRIES IN ptrv_doc_hd_itab
             WHERE awref = ptrv_doc_hd_itab-awref.
      SORT ptrv_doc_tax_itab BY awref aworg awlin.
    ENDIF.

    LOOP AT grundliste ASSIGNING <adr_grndlst>.

      <adr_grndlst>-fwbas = <adr_grndlst>-wrbtr.

      READ TABLE ptrv_doc_tax_itab WITH KEY awref = <adr_grndlst>-awref
                                            aworg = <adr_grndlst>-aworg
                                            awlin = <adr_grndlst>-awlin
                                            BINARY SEARCH.

      WHILE ( ptrv_doc_tax_itab-awref = <adr_grndlst>-awref AND
              ptrv_doc_tax_itab-aworg = <adr_grndlst>-aworg AND
              ptrv_doc_tax_itab-awlin = <adr_grndlst>-awlin ).
        sytabix = sy-tabix + 1.
        MOVE-CORRESPONDING ptrv_doc_tax_itab TO tax_puffer.
        APPEND tax_puffer.
        lines_of_tax_puffer = lines_of_tax_puffer + 1.
        READ TABLE ptrv_doc_tax_itab INDEX sytabix.
        IF sy-subrc <> 0.
          CLEAR ptrv_doc_tax_itab-awref.
        ENDIF.
      ENDWHILE.

      IF lines_of_tax_puffer = 1.
        <adr_grndlst>-hkonttax = tax_puffer-hkont.
        <adr_grndlst>-ktosltax = tax_puffer-ktosl.
        <adr_grndlst>-kschl    = tax_puffer-kschl.
        <adr_grndlst>-msatz    = tax_puffer-msatz.
      ELSEIF lines_of_tax_puffer > 1.
        <adr_grndlst>-hkonttax = '*'.
        <adr_grndlst>-ktosltax = '*'.
        <adr_grndlst>-kschl    = '*'.
        <adr_grndlst>-msatz    = 0.
      ENDIF.

      LOOP AT tax_puffer WHERE kstat = space.
        IF tax_puffer-wrbtr > 0.
          <adr_grndlst>-steur_soll =
          <adr_grndlst>-steur_soll + tax_puffer-wrbtr.
        ELSE.
          <adr_grndlst>-steur_haben =
          <adr_grndlst>-steur_haben + tax_puffer-wrbtr.
        ENDIF.
        <adr_grndlst>-wrbtr = <adr_grndlst>-wrbtr + tax_puffer-wrbtr.
      ENDLOOP.

      REFRESH tax_puffer.
      CLEAR lines_of_tax_puffer.

* Aufspalten des Brutto-/Nettobetrags in Soll (>0) und Haben
      IF <adr_grndlst>-wrbtr > 0.
        <adr_grndlst>-brutto_soll = <adr_grndlst>-wrbtr.
        <adr_grndlst>-netto_soll  = <adr_grndlst>-fwbas.
      ELSEIF <adr_grndlst>-wrbtr < 0.
        <adr_grndlst>-brutto_haben = <adr_grndlst>-wrbtr.
        <adr_grndlst>-netto_haben  = <adr_grndlst>-fwbas.
      ENDIF.

* Lesen des Kurztexts zum Hauptbuchkonto/Sachkonto aus Tab. SKAT
      IF NOT <adr_grndlst>-hkont IS INITIAL.

        READ TABLE text_for_cost_accnt WITH KEY
                                       bukrs  = <adr_grndlst>-bukrs
                                       skonto = <adr_grndlst>-hkont.
        IF sy-subrc <> 0.

          CALL FUNCTION 'HRCA_FIND_TEXT_FOR_COST_ACCNT'
            EXPORTING
              bukrs      = <adr_grndlst>-bukrs
              skonto     = <adr_grndlst>-hkont
            IMPORTING
              skontotext = <adr_grndlst>-hkonttxt
            EXCEPTIONS
              OTHERS     = 0.

          text_for_cost_accnt-bukrs     = <adr_grndlst>-bukrs.
          text_for_cost_accnt-skonto    = <adr_grndlst>-hkont.
          text_for_cost_accnt-skontotxt = <adr_grndlst>-hkonttxt.
          APPEND text_for_cost_accnt.

        ELSE.
          <adr_grndlst>-hkonttxt = text_for_cost_accnt-skontotxt.
        ENDIF.

      ENDIF.
* Lesen des Namens zum personenbezogenen Kreditor
      IF NOT <adr_grndlst>-lifnr IS INITIAL.

        READ TABLE creditor_getdetail WITH KEY
                                      lifnr = <adr_grndlst>-lifnr.

        IF sy-subrc <> 0.

          CALL FUNCTION 'HRCA_CREDITOR_GETDETAIL'
            EXPORTING
              creditorid              = <adr_grndlst>-lifnr
              comp_code               = <adr_grndlst>-bukrs
            IMPORTING
              creditor_general_detail = vendor_info
            EXCEPTIONS
              not_found               = 1
              OTHERS                  = 2.
          IF sy-subrc = 0.
            <adr_grndlst>-lifnrtxt = vendor_info-name.
          ENDIF.
          creditor_getdetail-lifnr    = <adr_grndlst>-lifnr.
          creditor_getdetail-lifnrtxt = <adr_grndlst>-lifnrtxt.
          APPEND creditor_getdetail.
        ELSE.
          <adr_grndlst>-lifnrtxt = creditor_getdetail-lifnrtxt.
        ENDIF.
      ENDIF.
* Lesen des Namens zum personenbezogenen Debitor
      IF NOT <adr_grndlst>-kunnr IS INITIAL.
        READ TABLE debtor_getdetail WITH KEY kunnr = <adr_grndlst>-kunnr.
        IF sy-subrc <> 0.
          CALL FUNCTION 'HRCA_DEBTOR_GETDETAIL'
            EXPORTING
              debtorid               = <adr_grndlst>-kunnr
              comp_code              = <adr_grndlst>-bukrs
            IMPORTING
              debitor_general_detail = custom_info
            EXCEPTIONS
              not_found              = 1
              OTHERS                 = 2.
          IF sy-subrc = 0.
            <adr_grndlst>-kunnrtxt = custom_info-name.
          ENDIF.

          debtor_getdetail-kunnr    = <adr_grndlst>-kunnr.
          debtor_getdetail-kunnrtxt = <adr_grndlst>-kunnrtxt.
          APPEND debtor_getdetail.

        ELSE.
          <adr_grndlst>-kunnrtxt = debtor_getdetail-kunnrtxt.
        ENDIF.
      ENDIF.

      IF ( <adr_grndlst>-hkont IS INITIAL ) AND
         ( NOT <adr_grndlst>-lifnr IS INITIAL ).
        <adr_grndlst>-hkont = <adr_grndlst>-lifnr.         "Kreditor
      ELSEIF ( <adr_grndlst>-hkont IS INITIAL ) AND
             ( NOT <adr_grndlst>-kunnr IS INITIAL ).
        <adr_grndlst>-hkont = <adr_grndlst>-kunnr.         "Debitor
      ENDIF.

      IF NOT <adr_grndlst>-kunnrtxt IS INITIAL.
        <adr_grndlst>-kontobez = <adr_grndlst>-kunnrtxt.
      ELSEIF NOT <adr_grndlst>-lifnrtxt IS INITIAL.
        <adr_grndlst>-kontobez = <adr_grndlst>-lifnrtxt.
      ELSEIF NOT <adr_grndlst>-hkonttxt IS INITIAL.
        <adr_grndlst>-kontobez = <adr_grndlst>-hkonttxt.
      ENDIF.

      READ TABLE ptrv_doc_hd_itab WITH KEY awref = <adr_grndlst>-awref
                                           aworg = <adr_grndlst>-aworg
           BINARY SEARCH.
      IF ptrv_doc_hd_itab-commi = 'X'
         OR ptrv_doc_hd_itab-checked = 'X'.
        READ TABLE lt_ptrv_cancel_key WITH KEY
                 awref_cancelled = <adr_grndlst>-awref TRANSPORTING NO
                 FIELDS.
        IF sy-subrc IS NOT INITIAL.
*          <adr_grndlst>-icon = '@08@'.     "Green light; positive
          <adr_grndlst>-icon = 'OK'.     "Green light; positive
        ELSE.
*          <adr_grndlst>-icon = '@B6\Q' && text-a21.
          <adr_grndlst>-icon = 'OK'.     "Green light; positive
        ENDIF.
      ELSE.
*     Begin WBGK010620;
        READ TABLE ptrv_rprpostd_itab WITH KEY
                                      awref = <adr_grndlst>-awref.
        IF sy-subrc = 0.
          IF ptrv_rprpostd_itab-type = 'E' OR
             ptrv_rprpostd_itab-type = 'A'.
*            <adr_grndlst>-icon = '@0A@'. "Red light; negative
            <adr_grndlst>-icon = 'Error'. "Red light; negative
          ELSE.
*            <adr_grndlst>-icon = '@09@'. "Yellow light; neutral
            <adr_grndlst>-icon = 'Warning'. "Yellow light; neutral
          ENDIF.
        ELSE.
*     End WBGK010620;
          <adr_grndlst>-icon = space.
        ENDIF.
      ENDIF.

      CLEAR ls_final.
      MOVE-CORRESPONDING <adr_grndlst> TO ls_final.
      APPEND ls_final TO lt_final.

    ENDLOOP.

* Populate Field Names
    DO 29 TIMES.
      CLEAR: lv_text,
             lv_index.

      lv_index = sy-index.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_index
        IMPORTING
          output = lv_index.

      CONCATENATE 'text-F' lv_index INTO lv_text.
      ASSIGN (lv_text) TO <fs>.

      IF <fs> IS ASSIGNED.
        CLEAR ls_fname.
        ls_fname-fname = <fs>.
        APPEND ls_fname TO lt_fname.
        UNASSIGN <fs>.
      ENDIF.
    ENDDO.

    CLEAR lv_path.

    CONCATENATE p_fpth1a sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) '\' INTO lv_path.

    IF iv_flag IS INITIAL.
      CONCATENATE lv_path sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) '_' text-010 '_' text-011 '.xls' INTO lv_path.

*BOC by KMB on 14.8.2020 CHG0188574
      IF p_pre = 'X'.
*EOC by KMB on 14.8.2020 CHG0188574


          CALL METHOD cl_gui_frontend_services=>gui_download
            EXPORTING
              filename              = lv_path
              write_field_separator = 'X'
              fieldnames            = lt_fname[]
            CHANGING
              data_tab              = lt_final[].


*BOC by KMB on 14.8.2020 CHG0188574
      ENDIF.
      IF p_app = 'X'.


        CLEAR lv_path.
*BOC by KMB on 17.9.2020 CHG0188574
*        CONCATENATE p_fpth1a sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) '_' text-010 '_' text-011 '.xls' INTO lv_path.
        CONCATENATE p_fpth1a sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) '_' text-010 '_' text-011 '.csv' INTO lv_path.
*EOC by KMB on 17.9.2020 CHG0188574

*BOC by KMB on 22.9.2020 CHG0188574
*          OPEN DATASET lv_path   FOR APPENDING IN TEXT MODE
          OPEN DATASET lv_path   FOR OUTPUT IN TEXT MODE
*EOC by KMB on 22.9.2020 CHG0188574
                                 ENCODING DEFAULT MESSAGE lv_msg.
          IF     ( sy-subrc NE 0 ).
            WRITE:   /001  text-008, lv_msg.
            MESSAGE  e000(zfi01) WITH text-008 lv_msg.
          ELSEIF sy-subrc = 0.
            CLEAR lv_string.
*BOC by KMB on 17.9.2020 CHG0188574
*            LOOP AT lt_fname INTO ls_fname.
*              lv_string = ls_fname-fname.
*              CLEAR:ls_fname.
*              TRANSFER lv_string TO lv_path.
*            ENDLOOP.
          CONCATENATE text-f01 text-f02 text-f03 text-f04 text-f05 text-f06 text-f07 text-f08 text-f09 text-f10
                      text-f11 text-f12 text-f13 text-f14 text-f15 text-f16 text-f17 text-f18 text-f19 text-f20
                      text-f21 text-f22 text-f23 text-f01 text-f24 text-f25 text-f26 text-f27 text-f28 text-f29
          INTO lv_string SEPARATED BY ','.
          TRANSFER lv_string TO lv_path.
*EOC by KMB on 17.9.2020 CHG0188574

            CLEAR lv_string.
*BOC by KMB on 22.9.2020 CHG0188574
*            LOOP AT lt_grundf INTO ls_final.
            LOOP AT lt_final INTO ls_final.
"EOC by KMB on 22.9.2020 CHG0188574

              lv_wrbtr = ls_final-wrbtr.
              lv_steur_soll = ls_final-steur_soll.
              lv_steur_haben = ls_final-steur_haben.
              lv_fwbas = ls_final-fwbas.

              CONCATENATE ls_final-icon
                          ls_final-runid
                          ls_final-awref
                          ls_final-awlin
                          ls_final-pernr
                          ls_final-exbel
                          ls_final-bukrs
                          ls_final-blart
                          ls_final-budat
                          ls_final-ktosl
                          ls_final-hkont
                          ls_final-hkonttxt
                          ls_final-waers
                          lv_wrbtr
                          ls_final-mwskz
                          ls_final-hkonttax
                          lv_steur_soll
                          lv_steur_haben
                          ls_final-sgtxt
                          ls_final-kokrs
                          ls_final-kostl
                          ls_final-aufnr
                          ls_final-kstrg
                          ls_final-posnr
                          ls_final-nplnr
                          ls_final-umdat
                          ls_final-antrg
                          ls_final-abrec
                          lv_fwbas
              INTO  lv_string SEPARATED BY ','.
              CLEAR:ls_final, lv_wrbtr, lv_steur_soll, lv_steur_haben, lv_fwbas.

              TRANSFER lv_string TO lv_path.
            ENDLOOP.
          ENDIF.

          CLOSE DATASET lv_path.

      ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574


    ELSE.

      CLEAR lt_grund[].
      lt_grund[] = lt_final[].
      SORT: lt_grund      BY runid,
            lt_final[]    BY runid.

      DELETE ADJACENT DUPLICATES FROM lt_grund COMPARING runid.

      LOOP AT lt_grund INTO ls_final.
        READ TABLE lt_final WITH KEY runid = ls_final-runid
                                         TRANSPORTING NO FIELDS
                                         BINARY SEARCH.
        IF sy-subrc EQ 0.

          CLEAR: lv_tabix,
                 lt_grundf[].

          lv_tabix = sy-tabix.

          LOOP AT lt_final INTO ls_final1 FROM lv_tabix.
            IF ls_final1-runid NE ls_final-runid.
              EXIT.
            ENDIF.
            APPEND ls_final1 TO lt_grundf.
          ENDLOOP.

*BOC by KMB on 14.8.2020 CHG0188574
          IF p_pre = 'X'.
*EOC by KMB on 14.8.2020 CHG0188574


            CLEAR lv_path.

            CONCATENATE p_fpth1a sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) '\' INTO lv_path.

            CONCATENATE lv_path sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) '_' text-010 '_' text-012 '_' ls_final-runid '.xls' INTO lv_path.



              CALL METHOD cl_gui_frontend_services=>gui_download
                EXPORTING
                  filename              = lv_path
                  write_field_separator = 'X'
                  fieldnames            = lt_fname[]
                CHANGING
                  data_tab              = lt_grundf[].

*BOC by KMB on 14.8.2020 CHG0188574
          ENDIF.
          IF p_app = 'X'.

** File Name
            CLEAR lv_path.
*BOC by KMB on 17.9.2020 CHG0188574
*            CONCATENATE p_fpth1a sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) '_' text-010 '_' text-012 '_' ls_final-runid '.xls' INTO lv_path.
            CONCATENATE p_fpth1a sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) '_' text-010 '_' text-012 '_' ls_final-runid '.csv' INTO lv_path.
*EOC by KMB on 17.9.2020 CHG0188574

              OPEN DATASET lv_path   FOR APPENDING IN TEXT MODE
                                     ENCODING DEFAULT MESSAGE lv_msg.
              IF     ( sy-subrc NE 0 ).
                WRITE:   /001  text-008, lv_msg.
                MESSAGE  e000(zfi01) WITH text-008 lv_msg.
              ELSEIF sy-subrc = 0.
                CLEAR lv_string.
*BOC by KMB on 17.9.2020 CHG0188574
*            LOOP AT lt_fname INTO ls_fname.
*              lv_string = ls_fname-fname.
*              CLEAR:ls_fname.
*              TRANSFER lv_string TO lv_path.
*            ENDLOOP.
          CONCATENATE text-f01 text-f02 text-f03 text-f04 text-f05 text-f06 text-f07 text-f08 text-f09 text-f10
                      text-f11 text-f12 text-f13 text-f14 text-f15 text-f16 text-f17 text-f18 text-f19 text-f20
                      text-f21 text-f22 text-f23 text-f01 text-f24 text-f25 text-f26 text-f27 text-f28 text-f29
          INTO lv_string SEPARATED BY ','.
          TRANSFER lv_string TO lv_path.
*EOC by KMB on 17.9.2020 CHG0188574

                CLEAR lv_string.
                LOOP AT lt_grundf INTO ls_final.
                  lv_wrbtr = ls_final-wrbtr.
                  lv_steur_soll = ls_final-steur_soll.
                  lv_steur_haben = ls_final-steur_haben.
                  lv_fwbas = ls_final-fwbas.

                  CONCATENATE ls_final-icon
                              ls_final-runid
                              ls_final-awref
                              ls_final-awlin
                              ls_final-pernr
                              ls_final-exbel
                              ls_final-bukrs
                              ls_final-blart
                              ls_final-budat
                              ls_final-ktosl
                              ls_final-hkont
                              ls_final-hkonttxt
                              ls_final-waers
                              lv_wrbtr
                              ls_final-mwskz
                              ls_final-hkonttax
                              lv_steur_soll
                              lv_steur_haben
                              ls_final-sgtxt
                              ls_final-kokrs
                              ls_final-kostl
                              ls_final-aufnr
                              ls_final-kstrg
                              ls_final-posnr
                              ls_final-nplnr
                              ls_final-umdat
                              ls_final-antrg
                              ls_final-abrec
                              lv_fwbas
                  INTO  lv_string SEPARATED BY ','.
                  CLEAR:ls_final, lv_wrbtr, lv_steur_soll, lv_steur_haben, lv_fwbas.
                  TRANSFER lv_string TO lv_path.
                ENDLOOP.
              ENDIF.

              CLOSE DATASET lv_path.

          ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574

        ENDIF.
      ENDLOOP.

* To Send a mail to FI team to reverse partial documents
      PERFORM f_send_mail.

    ENDIF.
  ENDIF.

ENDFORM.                    " F_DOC_READ
*&---------------------------------------------------------------------*
*&      Form  F_SEND_MAIL
*&---------------------------------------------------------------------*
*       To Send Mail
*----------------------------------------------------------------------*
FORM f_send_mail .

  DATA: ls_docdata     TYPE sodocchgi1,   "Mail subject
        lt_receiver    TYPE TABLE OF somlrec90 WITH HEADER LINE,  "Recepient
        lt_content     TYPE STANDARD TABLE OF soli WITH HEADER LINE."Main body

  CLEAR lt_receiver[].

***********************************************************************
*Create the list of recipients
***********************************************************************
  LOOP AT s_email.
    lt_receiver-receiver = s_email-low.
    lt_receiver-rec_type = 'U'.
    lt_receiver-express = 'X'.
    APPEND lt_receiver.
  ENDLOOP.

*    Mail Subject
  CLEAR ls_docdata.
  ls_docdata-obj_name = text-001.
  ls_docdata-obj_descr = text-005.
  ls_docdata-obj_langu = sy-langu.

*    Mail Body
  CLEAR lt_content[].
  CLEAR lt_content.
  lt_content-line = text-002.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = ' '.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = text-003.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = ' '.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = text-004.
  APPEND lt_content.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      object_content             = lt_content
      receivers                  = lt_receiver
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc = 0.

  ENDIF.

ENDFORM.                    " F_SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  F_USER_MAIL
*&---------------------------------------------------------------------*
* To send a mail to Business about the Files
*----------------------------------------------------------------------*
FORM f_user_mail .

  DATA: ls_docdata     TYPE sodocchgi1,   " Mail subject
        lt_receiver    TYPE TABLE OF somlrec90 WITH HEADER LINE,  " Recepient
        lt_content     TYPE STANDARD TABLE OF soli WITH HEADER LINE, " Main body
        lv_path        TYPE string.

  CLEAR lt_receiver.

***********************************************************************
*Create the list of recipients
***********************************************************************
  LOOP AT s_umail.
    lt_receiver-receiver = s_umail-low.
    lt_receiver-rec_type = 'U'.
    lt_receiver-express = 'X'.
    APPEND lt_receiver.
  ENDLOOP.

*    Mail Subject
  CLEAR ls_docdata.
  ls_docdata-obj_name = text-021.
  ls_docdata-obj_descr = text-025.
  ls_docdata-obj_langu = sy-langu.

*    Mail Body
  CLEAR lt_content[].
  CLEAR lt_content.
  lt_content-line = text-002.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = ' '.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = text-023.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = ' '.
  APPEND lt_content.

  CLEAR lv_path.

  CONCATENATE p_fpth1a sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) INTO lv_path.

  CLEAR lt_content.
  lt_content-line = ' '.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = lv_path.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = ' '.
  APPEND lt_content.

  CLEAR lt_content.
  lt_content-line = text-004.
  APPEND lt_content.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      object_content             = lt_content
      receivers                  = lt_receiver
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc = 0.

  ENDIF.

ENDFORM.                    " F_USER_MAIL
*&---------------------------------------------------------------------*
*&      Form  F_SPOOL_FILE
*&---------------------------------------------------------------------*
*     To Download Spool File
*----------------------------------------------------------------------*
FORM f_spool_file .


  TYPES: BEGIN       OF lty_spoolid,
          jobname    TYPE btcjob,
          jobcount   TYPE btcjobcnt,
          stepcount  TYPE btcstepcnt,
          spoolid    TYPE btclistid,
        END          OF lty_spoolid,

        BEGIN        OF lty_spool_data,
          data(2048) TYPE c,
        END          OF lty_spool_data.

  DATA: lt_spoolid   TYPE STANDARD TABLE OF lty_spoolid,
        lt_spool_xls TYPE STANDARD TABLE OF lty_spool_data,
        ls_spoolid   TYPE lty_spoolid,
        ls_spool_xls TYPE lty_spool_data,
        lt_xls_spool TYPE STANDARD TABLE OF soli.

  DATA: lv_jobcount  TYPE btcjobcnt,
        lv_stepcount TYPE btcstepcnt,
        lv_string    TYPE string,
        lv_xstring   TYPE xstring,
        lv_spoolid   TYPE rspoid,
        lv_counter   TYPE c VALUE '1',
        lv_date      TYPE sy-datum,
        lv_spath     TYPE string.

*BOC by KMB on 14.8.2020 CHG0188574
  DATA : lv_string_n      TYPE string,
         lv_msg           TYPE text100.
*EOC by KMB on 14.8.2020 CHG0188574

  CLEAR: lv_jobcount, lv_stepcount,lt_spoolid[], lv_date.

  lv_date = sy-datum.

  SELECT  jobcount stepcount UP TO 1 ROWS FROM tbtco
      INTO (lv_jobcount, lv_stepcount )
        WHERE jobname = p_job
        AND strtdate EQ lv_date
        AND status = 'F'
        ORDER BY jobcount DESCENDING.
  ENDSELECT.
  IF sy-subrc = 0.
    SELECT jobname jobcount stepcount spoolid
           FROM tbtc_spoolid
           INTO TABLE lt_spoolid
           WHERE jobname = p_job
            AND  jobcount = lv_jobcount.
    IF sy-subrc = 0.
**Read the spool
      LOOP AT lt_spoolid INTO ls_spoolid.

        CLEAR: lv_spoolid,
               lt_spool_xls[].

        lv_spoolid = ls_spoolid-spoolid.

*FM called that returns the Spool Request Number data into and internal table

        CALL FUNCTION 'RSPO_RETURN_ABAP_SPOOLJOB'
          EXPORTING
            rqident              = lv_spoolid                     "Spool Request Number
            first_line           = 1
          TABLES
            buffer               = lt_spool_xls                    "Internal table that will have the Spool Request No data
          EXCEPTIONS
            no_such_job          = 1
            not_abap_list        = 2
            job_contains_no_data = 3
            selection_empty      = 4
            no_permission        = 5
            can_not_access       = 6
            read_error           = 7
            OTHERS               = 8.
        IF sy-subrc IS INITIAL.

*To convert the spool data into excel format
          CLEAR lt_xls_spool[].
          CALL FUNCTION 'SO_RAW_TO_RTF'
            TABLES
              objcont_old = lt_spool_xls            " Internal table having spool data
              objcont_new = lt_xls_spool.           " Int table having Excel format data converted from Spool data

**Download files to presentation server.

*BOC by KMB on 14.8.2020 CHG0188574
          IF p_pre = 'X'.
*EOC by KMB on 14.8.2020 CHG0188574

** File Name
            CLEAR lv_spath.
            lv_spath = p_fpth1a.
            CONCATENATE p_fpth1a sy-datum+4(2)  sy-datum+6(2)  sy-datum+0(4) '\' INTO lv_spath.
*BOC by KMB on 3.9.2020 CHG0188574
*            CONCATENATE lv_spath lv_date+4(2)  lv_date+6(2)  lv_date+0(4) '_' 'EAST' '_' 'Posted' '_' 'Spool' '.xls' INTO lv_spath.
            CONCATENATE lv_spath lv_date+4(2)  lv_date+6(2)  lv_date+0(4) '_' 'EAST' '_' 'Posted' '_' 'Spool' '_' sy-uzeit '.xls' INTO lv_spath.
*EOC by KMB on 3.9.2020 CHG0188574

              CALL METHOD cl_gui_frontend_services=>gui_download
                EXPORTING
                  filename              = lv_spath
                  write_field_separator = 'X'
                CHANGING
                  data_tab              = lt_spool_xls.

*BOC by KMB on 14.8.2020 CHG0188574

          ENDIF.
          IF p_app = 'X'.

** File Name
            CLEAR lv_spath.
            lv_spath = p_fpth1a.
*BOC by KMB on 3.9.2020 CHG0188574
*            CONCATENATE p_fpth1a lv_date+4(2)  lv_date+6(2)  lv_date+0(4) '_' 'WEST' '_' 'Posted' '_' 'Spool' '.xls' INTO lv_spath.
            CONCATENATE p_fpth1a lv_date+4(2)  lv_date+6(2)  lv_date+0(4) '_' 'EAST' '_' 'Posted' '_' 'Spool' '_' sy-uzeit '.xls' INTO lv_spath.


*              OPEN DATASET lv_spath   FOR APPENDING IN TEXT MODE
              OPEN DATASET lv_spath   FOR OUTPUT IN TEXT MODE
*EOC by KMB on 3.9.2020 CHG0188574
                                              ENCODING DEFAULT MESSAGE lv_msg.
              IF     ( sy-subrc NE 0 ).
                WRITE:   /001  text-008, lv_msg.
                MESSAGE  e000(zfi01) WITH text-008 lv_msg.
              ELSEIF sy-subrc = 0.
                LOOP AT lt_spool_xls INTO ls_spool_xls.
                  lv_string_n =  ls_spool_xls.
                  TRANSFER lv_string_n TO lv_spath.
                  CLEAR:ls_spool_xls, lv_string_n.
                ENDLOOP.
              ENDIF.

              CLOSE DATASET lv_spath.

          ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574


        ENDIF.
        CLEAR:ls_spoolid.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_SPOOL_FILE
