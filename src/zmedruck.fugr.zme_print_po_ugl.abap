FUNCTION zme_print_po_ugl.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IX_NAST) LIKE  NAST STRUCTURE  NAST
*"     REFERENCE(IX_DRUVO) TYPE  DRUVO
*"     VALUE(DOC) TYPE  MEEIN_PURCHASE_DOC_PRINT
*"     REFERENCE(IX_SCREEN)
*"     REFERENCE(IX_XFZ) OPTIONAL
*"     REFERENCE(IX_MFLAG) DEFAULT SPACE
*"     REFERENCE(IX_FROM_MEMORY) DEFAULT SPACE
*"     VALUE(IX_TOA_DARA) LIKE  TOA_DARA STRUCTURE  TOA_DARA OPTIONAL
*"     VALUE(IX_ARC_PARAMS) LIKE  ARC_PARAMS STRUCTURE  ARC_PARAMS
*"       OPTIONAL
*"     REFERENCE(IX_FONAM) LIKE  TNAPR-FONAM OPTIONAL
*"     REFERENCE(IX_STO) DEFAULT SPACE
*"     REFERENCE(IX_DUNNING_ITEM) TYPE  C DEFAULT SPACE
*"  EXPORTING
*"     REFERENCE(EX_RETCO)
*"----------------------------------------------------------------------

* Die folgenden Daten stehen global (also auch in den Unterprogrammen)
* zur Verfügung und dürfen nicht geändert werden:
* doc-xekko  ->  ekko
* doc-xpekko ->  pekko
* doc-xekpo  ->  ekpo
* doc-xpekpo ->  pekpo
* ix_nast    ->  nast
* ix_druvo   ->  xdruvo

*todo: mahnung von Lieferplan mit Abrufdokumentation
*      die globalen Felder Preisdruck kopfkond müssen gesetzt werden

* docu: ===========
* docu: ME_PRINT_PO
* docu: ===========

  DATA  z TYPE i.
  DATA cs_isc_print_item_ds TYPE isc_print_item_ds.         "IS2ERP.
*ENHANCEMENT-POINT me_print_po_01 SPOTS es_saplmedruck.
*$*$-Start: ME_PRINT_PO_01------------------------------------------$*$*
*ENHANCEMENT 16  ISAUTO_SCH_EMM_SAPLMEDRUCK.    "active version
* last gr auto2.0af
* populate form interface
*  move doc-xisautoeklwes to cs_isc_print_item_ds-isautoeklwes.
*ENDENHANCEMENT.
*$*$-End:   ME_PRINT_PO_01------------------------------------------$*$*


* Witch data will be printed
  def_break '051 - Printing '.                              "#EC *

* docu: initialize
  REFRESH: xpekpov, tconf_out, addr_groups, error_table, htnamp,
     tfpltdr, zykl_tab, mpos_tab, tekomd, d_tkomv, tkomvd, d_tkomvd,
     mdpmx, mdsbx, xtmsi2, xt165p, xt166k, xt166p, xt166a, xthead,
     xekek, tekpo, teket, uekek, tkomk, tkomv.
  CLEAR: komk.
  CLEAR: pekpov.                                            "625895
  CLEAR: sto_flag.                                          "670912

  ekko = doc-xekko.
  pekko = doc-xpekko.
  MOVE ix_nast TO nast.
  xdruvo = ix_druvo.
  xfz = ix_xfz.
  tkomv[] = doc-xtkomv.
  sto_flag = ix_sto.                                        "670912
  date_on_item = ix_dunning_item.                           "Fashion

* docu: Kopfdaten lesen
  PERFORM read_header_data.

* docu: Formularausgabe vorbereiten
  IF NOT ix_from_memory IS INITIAL.
    PERFORM prepare_formular USING 'X' ix_toa_dara ix_arc_params
                                   ix_fonam
                             CHANGING ex_retco.
  ELSE.
    PERFORM prepare_formular USING ix_screen ix_toa_dara ix_arc_params
                                   ix_fonam
                             CHANGING ex_retco.
  ENDIF.
  CHECK ex_retco EQ 0.

* docu: Zuordnungstabelle füllen
  IF NOT doc-xpekko-hsamm IS INITIAL.
    PERFORM varianten_daten TABLES doc-xekpo.
  ENDIF.

* docu: Kopftexte lesen
  PERFORM read_header_text TABLES doc-xaend.

* docu: Kopfkonditionen lesen
  IF ix_from_memory IS INITIAL.
    IF ekko-bstyp NE bstyp-kont AND ekko-stako EQ space.
      CALL FUNCTION 'PRICING_REFRESH'
        TABLES
          tkomk = tkomk
          tkomv = tkomv.
      CALL FUNCTION 'RV_PRICE_PRINT_REFRESH'
        TABLES
          tkomv = tkomv.
    ELSE.
      CALL FUNCTION 'ME_RESET_CONTRACT_CONDITIONS'.
    ENDIF.
  ENDIF.
  IF ix_nast-kappl NE 'EL'.
    PERFORM read_header_cond TABLES doc-xekpo USING ix_from_memory.
  ENDIF.

* docu: Kopfdaten ausgeben
  PERFORM print_header TABLES doc-xaend
                       CHANGING ex_retco.
  CHECK ex_retco EQ 0.

* docu: Ausgabe Positionsüberschrift
  IF nast-kappl NE 'EL'.
    READ TABLE doc-xekpo INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      PERFORM ausgabe_pos_ueb.
    ENDIF.
  ENDIF.

* docu: Beginn der Ausgabe von Positionszeilen
  CLEAR sekpo.                                              "525332
  IF doc-xpekko-hsamm EQ space.
*BOC by PANUSURI Ticket 88669
*BOI by PANUSURI Ticket 57610
*Print CLM Contract Number                                          "UGL
*    CLEAR gwa_ekpo.                                                 "UGL
*    LOOP AT doc-xekpo INTO gwa_ekpo WHERE loekz = ''                "UGL
*                    AND konnr <> '' AND ktpnr <> ''.                "UGL
**      CALL FUNCTION 'WRITE_FORM'                                   "UGL
**        EXPORTING                                                  "UGL
**          element = 'CLM                                           "UGL
**          window  = 'ZCL'                                          "UGL
**        EXCEPTIONS                                                 "UGL
**          OTHERS  = 01.                                            "UGL
*      EXIT.                                                         "UGL
*    ENDLOOP.                                                        "UGL
*EOI by PANUSURI Ticket 57610
*EOC by PANUSURI Ticket 88669
    LOOP AT doc-xekpo INTO ekpo.
      CLEAR: pekpo, rm06p.
      READ TABLE doc-xpekpo INTO pekpo WITH KEY ekpo-ebelp.
      PERFORM prepare_item TABLES doc-xaend.
      PERFORM prepare_item_additional_data TABLES doc-xekkn
                                           USING 0 ix_mflag.
      PERFORM prepare_item_text TABLES doc-xekkn doc-xaend.
      IF ix_nast-kappl NE 'EL'.
        PERFORM prepare_item_cond TABLES doc-xekpo
                                  USING 0 ix_mflag.
      ENDIF.

*     docu: Positionsausgabe anstossen
      IF ix_nast-kappl EQ 'EL'.
        PERFORM print_item_ds TABLES doc-xeket doc-xekek doc-xekeh
                                     doc-xpekpo
                              CHANGING cs_isc_print_item_ds.
      ELSE.
        PERFORM print_item_po TABLES doc-xekpo doc-xeket
                                     doc-xekkn doc-xaend
                              USING 0 ix_mflag ix_from_memory.
      ENDIF.
    ENDLOOP.
  ELSE.
*   docu: Beleg mit Unterpositionen
    z = 0.
    LOOP AT xpekpov WHERE uptyp EQ uptyp-var              " note 505162
                       OR uptyp EQ uptyp-lot.             " note 505162
      LOOP AT tconf_out WHERE ebelp EQ xpekpov-ebelp.
        z = z + 1.
      ENDLOOP.
      EXIT.
    ENDLOOP.
    IF ix_mflag EQ space OR z < 1.

*     docu: Varianten/Lot-bestellung ohne Matrix ausgeben
      MOVE-CORRESPONDING ekko TO xekpokey.
      LOOP AT xpekpov.
        MOVE xpekpov TO pekpov.
        CLEAR: pekpo, rm06p.
        IF xpekpov-ebelp NE space.
          IF NOT xpekpov-uebpo IS INITIAL AND
             NOT xpekpov-uebpo EQ sekpo-ebelp.
*          HAUPTPOSITION BEREITSTELLEN
            CLEAR sekpo.
            MOVE xpekpov-uebpo TO xekpokey-ebelp.
            READ TABLE doc-xekpo INTO sekpo WITH KEY xekpokey.
          ENDIF.
          MOVE xpekpov-ebelp TO xekpokey-ebelp.
        ELSE.
          MOVE xpekpov-uebpo TO xekpokey-ebelp.
        ENDIF.
*    AUSZUGEBENDE POSITION BEREITSTELLEN
        READ TABLE doc-xekpo INTO ekpo WITH KEY xekpokey.
        READ TABLE doc-xpekpo INTO pekpo WITH KEY ekpo-ebelp. "625848
        PERFORM prepare_item  TABLES doc-xaend.
        PERFORM prepare_item_additional_data TABLES doc-xekkn
                                             USING 0 ix_mflag.
        PERFORM prepare_item_text TABLES doc-xekkn doc-xaend.
        PERFORM prepare_item_cond TABLES doc-xekpo
                                  USING 0 ix_mflag.

*       docu: Positionsausgabe anstossen
        IF ix_nast-kappl EQ 'EL'.

          PERFORM print_item_ds TABLES doc-xeket doc-xekek doc-xekeh
                                     doc-xpekpo
                                 CHANGING cs_isc_print_item_ds.
          "$$
          "$$
          "$$
        ELSE.
          PERFORM print_item_po TABLES doc-xekpo doc-xeket
                                       doc-xekkn doc-xaend
                                USING z ix_mflag ix_from_memory.
        ENDIF.
      ENDLOOP.
    ELSE.
*   docu: Varianten/Lot-positionen mit Matrix ausgeben
*   auch für ein variantenbildendes Merkmal
      REFRESH kond.
      MOVE-CORRESPONDING ekko TO xekpokey.
      LOOP AT xpekpov.
        CLEAR: pekpo, rm06p.
        READ TABLE doc-xpekpo INTO pekpo WITH KEY ekpo-ebelp.
        MOVE xpekpov TO pekpov.
        IF xpekpov-ebelp NE space.
          IF NOT xpekpov-uebpo IS INITIAL AND
             NOT xpekpov-uebpo EQ sekpo-ebelp.
*          HAUPTPOSITION BEREITSTELLEN
            CLEAR sekpo.
            MOVE xpekpov-uebpo TO xekpokey-ebelp.
            READ TABLE doc-xekpo INTO sekpo WITH KEY xekpokey.
          ENDIF.
          MOVE xpekpov-ebelp TO xekpokey-ebelp.
        ELSE.
          MOVE xpekpov-uebpo TO xekpokey-ebelp.
        ENDIF.
*    AUSZUGEBENDE POSITION BEREITSTELLEN
        READ TABLE doc-xekpo INTO ekpo WITH KEY xekpokey.
*    Ausgabe vor nächster Hauptposition
        ON CHANGE OF ekpo-uebpo.       "??? ekpo oder xekpo ???
          IF ekpo-uptyp IS INITIAL.
            PERFORM varianten.
            PERFORM m_ausgeben.
            PERFORM abweichung TABLES doc-xekpo doc-xeket
                               USING ekpo.
            PERFORM aenderungen TABLES doc-xaend doc-xpekpo
                                USING ekpo.
          ENDIF.
        ENDON.

        IF ( ekpo-uptyp NE uptyp-var ) AND
           ( ekpo-uptyp NE uptyp-lot ).
          PERFORM prepare_item TABLES doc-xaend.
          PERFORM prepare_item_additional_data TABLES doc-xekkn
                                               USING z ix_mflag.
          PERFORM prepare_item_text TABLES doc-xekkn doc-xaend.
          PERFORM prepare_item_cond TABLES doc-xekpo
                                    USING z ix_mflag.
*         docu: Positionsausgabe anstossen
          IF ix_nast-kappl EQ 'EL'.
            PERFORM print_item_ds TABLES doc-xeket doc-xekek doc-xekeh
                                     doc-xpekpo
                                   CHANGING cs_isc_print_item_ds.
            "$$
            "$$
            "$$
          ELSE.
            PERFORM print_item_po TABLES doc-xekpo doc-xeket
                                         doc-xekkn doc-xaend
                                  USING z ix_mflag ix_from_memory.
          ENDIF.
        ELSE.
          PERFORM sammel.
        ENDIF.
      ENDLOOP.
      IF ( ekpo-uptyp EQ uptyp-var ) OR ( ekpo-uptyp EQ uptyp-lot ).
        PERFORM varianten.
        PERFORM m_ausgeben.
        PERFORM abweichung TABLES doc-xekpo doc-xeket
                           USING ekpo.
        PERFORM aenderungen TABLES doc-xaend doc-xpekpo
                            USING ekpo.
      ENDIF.
    ENDIF.
  ENDIF.
* docu: Ende der Ausgabe von Positionszeilen

* docu: Kopfkonditionen lesen
  IF ix_nast-kappl NE 'EL'.
    PERFORM read_header_cond TABLES doc-xekpo USING ix_from_memory.
  ENDIF.

* docu: Kopfkonditionen und Summen ausgeben.
  PERFORM print_total_line.

* docu: Bemerkung zu Kopfänderungen ausgeben
  PERFORM print_appendix TABLES doc-xaend.

* docu: Formular schließen
  PERFORM ende CHANGING ex_retco.

*- nast-sndex wird als Kennzeichen für Probedruck verwendet
  IF ix_nast-kappl  EQ 'EL'    AND ix_screen  IS INITIAL AND
     ix_from_memory IS INITIAL AND nast-sndex IS INITIAL.
* missing environment for limiting update print dependend data
    IF sy-ucomm NE '9ANZ' AND sy-ucomm NE '9DPR'.
      PERFORM update_release TABLES doc-xekpo doc-xekek doc-xekeh
                             USING  xdruvo ix_nast-kschl.
    ENDIF.
  ENDIF.
*perform terms_conditions .
ENDFUNCTION.                                             "#EC CI_VALPAR
