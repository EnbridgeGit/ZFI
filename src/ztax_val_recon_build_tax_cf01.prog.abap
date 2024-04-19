*----------------------------------------------------------------------*
***INCLUDE ZTAX_VALIDATION_BUILD_TAX_CF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .
* Countries get Procedure (pricing, output control, acct. det., costing,...)
  SELECT SINGLE kalsm INTO kalsm
         FROM t005
         WHERE land1 = 'US'.

  PERFORM build_tax_code_tables.


*  grir-sign = 'I'.
*  grir-option = 'EQ'.
*  grir-low = '230150'.
*  APPEND grir.
*
*  grir-low = '230151'.
*  APPEND grir.
*
*  grir-low = '230155'.
*  APPEND grir.
*
*  grir-low = '230158'.
*  APPEND grir.
*
*  grir-low = '230159'.
*  APPEND grir.

*document types for the vendor data
*  REFRESH blart1.
*  blart1-sign = 'I'.
*  blart1-option = 'EQ'.
*
*  blart1-low = 'KR'.
*  APPEND blart1.
*  blart1-low = 'KA'.
*  APPEND blart1.
*  blart1-low = 'KG'.
*  APPEND blart1.
*  blart1-low = 'KN'.
*  APPEND blart1.
*  blart1-low = 'RE'.
*  APPEND blart1.
*  blart1-low = 'RN'.
*  APPEND blart1.

ENDFORM.                    " initialization

*&---------------------------------------------------------------------*
*&      Form  build_tax_code_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_tax_code_tables .

* Build table of valid tax codes
  SELECT * FROM t007a
         WHERE kalsm = kalsm AND
               mwart = 'V'.
    mwskz-sign = 'I'.
    mwskz-option = 'EQ'.
    mwskz-low = t007a-mwskz.
    APPEND mwskz.
  ENDSELECT.
*
* Build table of exempt tax codes
  mwskz_nt-sign = 'I'.
  mwskz_nt-option = 'EQ'.
  mwskz_nt-low = 'I0'.
  APPEND mwskz_nt.
*
  mwskz_nt-sign = 'I'.
  mwskz_nt-option = 'EQ'.
  mwskz_nt-low = 'I6'.
  APPEND mwskz_nt.
*
  mwskz_nt-sign = 'I'.
  mwskz_nt-option = 'EQ'.
  mwskz_nt-low = 'V0'.
  APPEND mwskz_nt.
*
  mwskz_nt-sign = 'I'.
  mwskz_nt-option = 'EQ'.
  mwskz_nt-low = 'G0'.
  APPEND mwskz_nt.

ENDFORM.                    " build_tax_code_tables
*&---------------------------------------------------------------------*
*&      Form  Main_select
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM main_select .

*** get the data and populate FIH and FIP
* Accounting Document Header
  SELECT bkpf~bukrs bkpf~belnr bkpf~gjahr
         bkpf~waers bkpf~blart bkpf~bvorg
         bkpf~awtyp bkpf~awkey bkpf~budat bkpf~usnam bkpf~xblnr
         bkpf~tcode bkpf~kursf bkpf~xmwst bkpf~bktxt bkpf~bldat
         INTO  (item-bukrs, item-belnr,
                item-gjahr, item-waers, item-blart, item-bvorg,
                item-awtyp, item-awkey, item-budat, item-usnam,
                item-xblnr, item-tcode, item-kursf, item-xmwst,
                item-bktxt, item-bldat)
         FROM bkpf
         WHERE bukrs IN bukrs AND
               belnr IN belnr AND
               gjahr IN gjahr AND
               blart IN blart1 AND
               budat IN budat1 AND
               cpudt IN cpudt AND
               waers IN waers AND
               usnam IN usnam AND
               bstat = space.
*sjb - eliminate / in ALV report.. SAP is substituing the '/' for the '-' in exchange rate
    item-kursf = ABS( item-kursf ).

* Document Header: Invoice Receipt
    IF item-awtyp = 'RMRP'.  "Invoice Verification
      SELECT rmwwr wmwst1
             lifnr belnr
             beznk xblnr zbd1t
             zbd2t zbd3t zbd1p zbd2p
             zfbdt rebzg
             INTO (item-rmwwr, item-wmwst1,
                   item-lifnr, item-belnri,
                   item-beznk, item-xblnr,
                   item-zbd1t, item-zbd2t, item-zbd3t, item-zbd1p,
                   item-zbd2p, item-zfbdt, item-rebzg)
             FROM rbkp
             WHERE belnr = item-awkey(10) AND
                   gjahr = item-awkey+10(4) AND
                   bukrs = item-bukrs AND
                   belnr IN belnri AND
                   lifnr IN lifnr.
        REFRESH: inv_items, inv_sum. CLEAR: inv_items, inv_sum.
* Document Item: Incoming Invoice
        SELECT wrbtr txjcd mwskz menge buzei ebeln ebelp werks bstme
               bnkan zekkn shkzg
               INTO CORRESPONDING FIELDS OF TABLE inv_items
               FROM rseg
               WHERE belnr = item-belnri AND
                     gjahr = item-awkey+10(4) AND
                     ebeln IN ebeln AND
                     mwskz IN mwskz AND
                     werks IN werks.
        LOOP AT inv_items.
* Document Item, Incoming Invoice, Account Assignment
          SELECT SINGLE saknr kostl kokrs aufnr anln1 anln2 ps_psp_pnr "#EC *
                        nplnr wrbtr txjcd mwskz menge buzei bnkan_fw "#EC *
                        vbeln vbelp
                  INTO CORRESPONDING FIELDS OF inv_items    "#EC *
                 FROM rbco                                  "#EC *
                 WHERE belnr = item-belnri AND              "#EC *
                       gjahr = item-awkey+10(4) AND         "#EC *
                       buzei = inv_items-buzei.             "#EC *
          IF sy-subrc = 0.
            MODIFY inv_items.
          ELSE.
            inv_items-bnkan_fw = inv_items-bnkan.
            MODIFY inv_items.
          ENDIF.
        ENDLOOP.
        CLEAR acc_ass_not_found.
* Document Item, Incoming Invoice, Account Assignment
        SELECT buzei saknr kostl kokrs aufnr anln1 anln2 ps_psp_pnr
                      wrbtr txjcd mwskz menge buzei bnkan_fw shkzg
                      cobl_nr nplnr vbeln vbelp
               INTO CORRESPONDING FIELDS OF rbco
               FROM rbco
               WHERE belnr = item-belnri AND
                     gjahr = item-awkey+10(4).
          SORT inv_items BY buzei cobl_nr.
          READ TABLE inv_items WITH KEY buzei = rbco-buzei
                                        cobl_nr = rbco-cobl_nr
               BINARY SEARCH.
          IF sy-subrc NE 0.
            MOVE-CORRESPONDING rbco TO inv_items.           "#EC *
            APPEND inv_items.
          ENDIF.
        ENDSELECT.
        IF sy-subrc NE 0.
          acc_ass_not_found = 'X'.
        ENDIF.

* Document Item: Incoming Invoice for Material
        SELECT buzei wrbtr txjcd mwskz menge bnkan_fw shkzg
               INTO CORRESPONDING FIELDS OF rbma
               FROM rbma
               WHERE belnr = item-belnri AND
                     gjahr = item-awkey+10(4).
          MOVE-CORRESPONDING rbma TO inv_items.
          APPEND inv_items.
        ENDSELECT.
        IF sy-subrc NE 0.
          acc_ass_not_found = 'X'.
        ENDIF.

        LOOP AT inv_items.
          IF inv_items-buzei NE 0 AND
             inv_items-cobl_nr = 0 AND
             NOT inv_items-saknr IS INITIAL.
            DELETE inv_items.
          ELSE.
            MOVE-CORRESPONDING inv_items TO inv_sum.
            COLLECT inv_sum.
          ENDIF.
          IF inv_items-buzei = 0.
            CLEAR: inv_items-ebeln, inv_items-ebelp, inv_items-werks,
                   inv_items-bstme, inv_items-zekkn.
            MODIFY inv_items.
          ENDIF.
        ENDLOOP.

        LOOP AT inv_items.
* Document Item: Incoming Invoice
          SELECT SINGLE ebeln ebelp werks
                 INTO (inv_items-ebeln, inv_items-ebelp, inv_items-werks)
                 FROM rseg
                 WHERE belnr = item-belnri AND
                       gjahr = item-awkey+10(4) AND
                       buzei = inv_items-buzei.
          MOVE-CORRESPONDING inv_items TO item.
          item-buzeii = inv_items-buzei.
          CHECK item-saknr IN saknr OR
                item-saknr IN grir OR
                item-saknr IS INITIAL. "(inventory purchase)
          IF ttxd-xextn = 'S'.
            CHECK item-txjcd+2(2) IN regio.
          ELSE.
            CHECK item-txjcd(2) IN regio.
          ENDIF.
          CHECK item-ebeln IN ebeln.
*          IF NOT item-werks IS INITIAL.
            CHECK item-mwskz IN mwskz.
*          ENDIF.
          CHECK item-werks IN werks.
          CHECK item-kostl IN kostl.
          CHECK item-lifnr IN lifnr.
          CHECK NOT item-lifnr IS INITIAL.
          item-wrbtr = inv_items-wrbtr + inv_items-bnkan_fw.
* Purchasing Document Item
          SELECT SINGLE matnr matkl txz01 adrnr adrn2 mwskz wepos lgort knttp
                 INTO (item-matnr, item-matkl, item-txz01, item-adrnr, item-adrn2,
                     item-mwskz_po, item-wepos, item-lgort, item-knttp)
                 FROM ekpo
                 WHERE ebeln = item-ebeln AND
                       ebelp = item-ebelp.

          IF item-kokrs IS INITIAL.
* Account Assignment in Purchasing Document
            SELECT SINGLE kokrs INTO item-kokrs FROM ekkn
                   WHERE ebeln = item-ebeln AND
                         ebelp = item-ebelp AND
                         zekkn = item-zekkn.
            IF sy-subrc NE 0.
              SELECT SINGLE kokrs INTO item-kokrs FROM ekkn "#EC *
                     WHERE ebeln = item-ebeln AND           "#EC *
                           ebelp = item-ebelp.              "#EC *
            ENDIF.
          ENDIF.
          CLEAR: item-act_tax, item-act_tax_1, item-act_tax_2,
                 item-act_tax_3, item-act_tax_4.
          CLEAR inv_net.
          PERFORM determine_net USING inv_net.
          IF NOT inv_net = 0 AND
             NOT item-wrbtr = 0 AND
             NOT item-mwskz IN mwskz_nt.
            item-act_tax = item-wmwst1 / inv_net * item-wrbtr.
          ELSE.
            item-act_tax = 0.
          ENDIF.
          IF item-act_tax = 0.
* Accounting Document Segment
            SELECT txgrp buzei mwskz txjcd
                   INTO (bseg-txgrp, item-buzei, bseg-mwskz, bseg-txjcd)
                   FROM bseg
                   WHERE bukrs = item-bukrs AND
                         belnr = item-belnr AND
                         gjahr = item-gjahr AND
                         ( buzid = 'W' OR
                           buzid = 'S' OR
                           buzid = 'P' OR
                           buzid = 'M') AND
*                           ebeln = item-ebeln AND
*                           ebelp = item-ebelp AND
*************************Intercompany MIRO has blank PO and KTOSL = BUV
                         ( ( ebeln = item-ebeln AND
                             ebelp = item-ebelp AND
                             ktosl NE 'BUV' ) OR
                           ( ktosl EQ 'BUV' ) ) AND
                         mwskz = item-mwskz AND
                         txjcd = item-txjcd.
              CLEAR: item-act_tax, item-act_tax_1, item-act_tax_2,
                     item-act_tax_3, item-act_tax_4.
* Tax Data Document Segment
              IF bseg-mwskz IN mwskz_nt.
                SELECT fwste shkzg fwbas txjlv kschl
                     INTO (bset-fwste, bset-shkzg, bset-fwbas,
                           bset-txjlv, bset-kschl)
                     FROM bset
                     WHERE bukrs = item-bukrs AND
                           belnr = item-belnr AND
                           gjahr = item-gjahr AND
                           buzei = item-buzei AND
                           mwskz = bseg-mwskz AND           "071907
                           txjdp = bseg-txjcd AND           "071907
                           ( ktosl = 'NVV' OR
                             ktosl LIKE 'VS%' ).
                  IF bset-txjlv IS INITIAL.
                    bset-txjlv = bset-kschl+2(1).
                  ENDIF.
                  IF bset-txjlv = '1'.
                    CLEAR item-act_tax_4.
                  ENDIF.
                  abs_wrbtr = ABS( item-wrbtr ).
                  READ TABLE inv_sum WITH KEY mwskz = bseg-mwskz
                                              txjcd = bseg-txjcd.
                  IF sy-subrc = 0.
                    IF ttxd-xtxit IS INITIAL.
*                    abs_fwbas = ABS( inv_sum-wrbtr ) + ABS( inv_sum-bnkan_fw ).
                      IF ( inv_sum-wrbtr GE 0 AND   " SJB  Start  04/26/10 new
                           inv_sum-bnkan_fw GE 0 ) OR
                         ( inv_sum-wrbtr LE 0 AND
                           inv_sum-bnkan_fw LE 0 ).
                        abs_fwbas = ABS( inv_sum-wrbtr ) +
                        ABS( inv_sum-bnkan_fw ).
                      ELSEIF inv_sum-wrbtr GE 0 AND
                           inv_sum-bnkan_fw LE 0.
                        abs_fwbas = ABS( inv_sum-wrbtr ) -
                        ABS( inv_sum-bnkan_fw ).
                      ELSEIF inv_sum-wrbtr LE 0 AND
                           inv_sum-bnkan_fw GE 0.
                        abs_fwbas = ABS( inv_sum-wrbtr ) -
                        ABS( inv_sum-bnkan_fw ).
                      ENDIF.    " END
                    ELSE.
                      abs_fwbas = ABS( bset-fwbas ).
                    ENDIF.
                  ENDIF.

                  IF bset-shkzg = 'S'.
                    item-act_tax = item-act_tax + bset-fwste.
                    IF bset-txjlv = '1'.  "State
                      item-act_tax_1 = bset-fwste.
                    ELSEIF bset-txjlv = '2'.  "County
                      item-act_tax_2 = bset-fwste.
                    ELSEIF bset-txjlv = '3'.  "City
                      item-act_tax_3 = bset-fwste.
                    ELSE. "Secondary taxes
                      item-act_tax_4 = item-act_tax_4 + bset-fwste.
                    ENDIF.
                  ELSE.
                    item-act_tax = item-act_tax - bset-fwste.
                    IF bset-txjlv = '1'.  "State
                      item-act_tax_1 = bset-fwste * -1.
                    ELSEIF bset-txjlv = '2'.  "County
                      item-act_tax_2 = bset-fwste * -1.
                    ELSEIF bset-txjlv = '3'.  "City
                      item-act_tax_3 = bset-fwste * -1.
                    ELSE. "Secondary taxes
                      item-act_tax_4 = item-act_tax_4 - bset-fwste.
                    ENDIF.
                  ENDIF.

                ENDSELECT.

              ELSE.
                SELECT fwste shkzg fwbas txjlv kschl
                       INTO (bset-fwste, bset-shkzg, bset-fwbas,
                             bset-txjlv, bset-kschl)
                       FROM bset
                       WHERE bukrs = item-bukrs AND
                             belnr = item-belnr AND
                             gjahr = item-gjahr AND
                             mwskz = bseg-mwskz AND         "071907
                             txjdp = bseg-txjcd AND         "071907
                             ( ktosl = 'NVV' OR
                               ktosl LIKE 'VS%' ).
                  IF bset-txjlv IS INITIAL.
                    bset-txjlv = bset-kschl+2(1).
                  ENDIF.
                  IF bset-txjlv = '1'.
                    CLEAR item-act_tax_4.
                  ENDIF.
                  abs_wrbtr = ABS( item-wrbtr ).
                  READ TABLE inv_sum WITH KEY mwskz = bseg-mwskz
                                              txjcd = bseg-txjcd.
                  IF sy-subrc = 0.
                    IF ttxd-xtxit IS INITIAL.
*                    abs_fwbas = ABS( inv_sum-wrbtr ) + ABS( inv_sum-bnkan_fw ).
                      IF ( inv_sum-wrbtr GE 0 AND   " SJB  Start  04/26/10 new
                           inv_sum-bnkan_fw GE 0 ) OR
                         ( inv_sum-wrbtr LE 0 AND
                           inv_sum-bnkan_fw LE 0 ).
                        abs_fwbas = ABS( inv_sum-wrbtr ) +
                        ABS( inv_sum-bnkan_fw ).
                      ELSEIF inv_sum-wrbtr GE 0 AND
                           inv_sum-bnkan_fw LE 0.
                        abs_fwbas = ABS( inv_sum-wrbtr ) -
                        ABS( inv_sum-bnkan_fw ).
                      ELSEIF inv_sum-wrbtr LE 0 AND
                           inv_sum-bnkan_fw GE 0.
                        abs_fwbas = ABS( inv_sum-wrbtr ) -
                        ABS( inv_sum-bnkan_fw ).
                      ENDIF.    " END
                    ELSE.
                      abs_fwbas = ABS( bset-fwbas ).
                    ENDIF.
                  ENDIF.

                  IF bset-shkzg = 'S'.
                    item-act_tax = item-act_tax + bset-fwste.
                    IF bset-txjlv = '1'.  "State
                      item-act_tax_1 = bset-fwste.
                    ELSEIF bset-txjlv = '2'.  "County
                      item-act_tax_2 = bset-fwste.
                    ELSEIF bset-txjlv = '3'.  "City
                      item-act_tax_3 = bset-fwste.
                    ELSE. "Secondary taxes
                      item-act_tax_4 = item-act_tax_4 + bset-fwste.
                    ENDIF.
                  ELSE.
                    item-act_tax = item-act_tax - bset-fwste.
                    IF bset-txjlv = '1'.  "State
                      item-act_tax_1 = bset-fwste * -1.
                    ELSEIF bset-txjlv = '2'.  "County
                      item-act_tax_2 = bset-fwste * -1.
                    ELSEIF bset-txjlv = '3'.  "City
                      item-act_tax_3 = bset-fwste * -1.
                    ELSE. "Secondary taxes
                      item-act_tax_4 = item-act_tax_4 - bset-fwste.
                    ENDIF.
                  ENDIF.

                ENDSELECT.

              ENDIF.

              IF abs_fwbas > 0 AND abs_wrbtr > 0.
                item-act_tax = item-act_tax / abs_fwbas * abs_wrbtr.
                item-act_tax_1 = item-act_tax_1 / abs_fwbas * abs_wrbtr.
                item-act_tax_2 = item-act_tax_2 / abs_fwbas * abs_wrbtr.
                item-act_tax_3 = item-act_tax_3 / abs_fwbas * abs_wrbtr.
                item-act_tax_4 = item-act_tax_4 / abs_fwbas * abs_wrbtr.
              ELSE.
                item-act_tax = 0.
                item-act_tax_1 = 0.
                item-act_tax_2 = 0.
                item-act_tax_3 = 0.
                item-act_tax_4 = 0.
              ENDIF.

            ENDSELECT.
            IF item-act_tax = 0 AND
               NOT item-bvorg IS INITIAL.
* Tax Data Document Segment
              SELECT fwste shkzg txjlv kschl
                     INTO (bset-fwste, bset-shkzg, bset-txjlv,
                           bset-kschl)
                     FROM bset
                     WHERE bukrs = item-bvorg+10(4) AND
                           belnr = item-bvorg(10) AND
                           gjahr = item-gjahr AND
                           ( ktosl = 'NVV' OR
                             ktosl LIKE 'VS%' ).
                IF bset-txjlv IS INITIAL.
                  bset-txjlv = bset-kschl+2(1).
                ENDIF.
                IF bset-txjlv = '1'.
                  CLEAR item-act_tax_4.
                ENDIF.
                IF bset-shkzg = 'S'.
                  item-act_tax = item-act_tax + bset-fwste.
                  IF bset-txjlv = '1'.  "State
                    item-act_tax_1 = bset-fwste.
                  ELSEIF bset-txjlv = '2'.  "County
                    item-act_tax_2 = bset-fwste.
                  ELSEIF bset-txjlv = '3'.  "City
                    item-act_tax_3 = bset-fwste.
                  ELSE. "Secondary taxes
                    item-act_tax_4 = item-act_tax_4 + bset-fwste.
                  ENDIF.
                ELSE.
                  item-act_tax = item-act_tax - bset-fwste.
                  IF bset-txjlv = '1'.  "State
                    item-act_tax_1 = bset-fwste * -1.
                  ELSEIF bset-txjlv = '2'.  "County
                    item-act_tax_2 = bset-fwste * -1.
                  ELSEIF bset-txjlv = '3'.  "City
                    item-act_tax_3 = bset-fwste * -1.
                  ELSE. "Secondary taxes
                    item-act_tax_4 = item-act_tax_4 - bset-fwste.
                  ENDIF.
                ENDIF.
              ENDSELECT.
              IF NOT inv_net = 0 AND
                 NOT item-wrbtr = 0.
                item-act_tax = item-act_tax / inv_net *
                               item-wrbtr.
                item-act_tax_1 = item-act_tax_1 / inv_net *
                               item-wrbtr.
                item-act_tax_2 = item-act_tax_2 / inv_net *
                               item-wrbtr.
                item-act_tax_3 = item-act_tax_3 / inv_net *
                               item-wrbtr.
                item-act_tax_4 = item-act_tax_4 / inv_net *
                               item-wrbtr.
              ELSE.
                item-act_tax = 0.
              ENDIF.
            ENDIF.
          ELSE.
* Accounting Document Segment
            SELECT txgrp buzei mwskz txjcd
                   INTO (bseg-txgrp, item-buzei, bseg-mwskz, bseg-txjcd)
                   FROM bseg
                   WHERE bukrs = item-bukrs AND
                         belnr = item-belnr AND
                         gjahr = item-gjahr AND
                         ( buzid = 'W' OR
                           buzid = 'S' OR
                           buzid = 'P' OR
                           buzid = 'M') AND
*                           ebeln = item-ebeln AND
*                           ebelp = item-ebelp AND
*************************Intercompany MIRO has blank PO and KTOSL = BUV
                         ( ( ebeln = item-ebeln AND
                             ebelp = item-ebelp AND
                             ktosl NE 'BUV' ) OR
                           ( ktosl EQ 'BUV' ) ) AND
                         mwskz = item-mwskz AND
                         txjcd = item-txjcd.
              CLEAR: item-act_tax_1, item-act_tax_2,
                     item-act_tax_3, item-act_tax_4.
* Tax Data Document Segment
              SELECT fwste shkzg fwbas txjlv kschl
                     INTO (bset-fwste, bset-shkzg, bset-fwbas,
                           bset-txjlv, bset-kschl)
                     FROM bset
                     WHERE bukrs = item-bukrs AND
                           belnr = item-belnr AND
                           gjahr = item-gjahr AND
                           mwskz = bseg-mwskz AND           "071907
                           txjdp = bseg-txjcd AND           "071907
                           ( ktosl = 'NVV' OR
                             ktosl LIKE 'VS%' ).
                IF bset-txjlv IS INITIAL.
                  bset-txjlv = bset-kschl+2(1).
                ENDIF.
                IF bset-txjlv = '1'.
                  CLEAR item-act_tax_4.
                ENDIF.
                abs_wrbtr = ABS( item-wrbtr ).
                READ TABLE inv_sum WITH KEY mwskz = bseg-mwskz
                                            txjcd = bseg-txjcd.
                IF sy-subrc = 0.
                  IF ttxd-xtxit IS INITIAL.
*                    abs_fwbas = ABS( inv_sum-wrbtr ) + ABS( inv_sum-bnkan_fw ).
                    IF ( inv_sum-wrbtr GE 0 AND       " SJB Start 04/26/10 new
                         inv_sum-bnkan_fw GE 0 ) OR
                       ( inv_sum-wrbtr LE 0 AND
                         inv_sum-bnkan_fw LE 0 ).
                      abs_fwbas = ABS( inv_sum-wrbtr ) +
                      ABS( inv_sum-bnkan_fw ).
                    ELSEIF inv_sum-wrbtr GE 0 AND
                         inv_sum-bnkan_fw LE 0.
                      abs_fwbas = ABS( inv_sum-wrbtr ) -
                      ABS( inv_sum-bnkan_fw ).
                    ELSEIF inv_sum-wrbtr LE 0 AND
                         inv_sum-bnkan_fw GE 0.
                      abs_fwbas = ABS( inv_sum-wrbtr ) -
                      ABS( inv_sum-bnkan_fw ).
                    ENDIF.              " End
                  ELSE.
                    abs_fwbas = ABS( bset-fwbas ).
                  ENDIF.
                ENDIF.

                IF bset-shkzg = 'S'.
                  IF bset-txjlv = '1'.  "State
                    item-act_tax_1 = bset-fwste.
                  ELSEIF bset-txjlv = '2'.  "County
                    item-act_tax_2 = bset-fwste.
                  ELSEIF bset-txjlv = '3'.  "City
                    item-act_tax_3 = bset-fwste.
                  ELSE. "Secondary taxes
                    item-act_tax_4 = item-act_tax_4 + bset-fwste.
                  ENDIF.
                ELSE.
                  IF bset-txjlv = '1'.  "State
                    item-act_tax_1 = bset-fwste * -1.
                  ELSEIF bset-txjlv = '2'.  "County
                    item-act_tax_2 = bset-fwste * -1.
                  ELSEIF bset-txjlv = '3'.  "City
                    item-act_tax_3 = bset-fwste * -1.
                  ELSE. "Secondary taxes
                    item-act_tax_4 = item-act_tax_4 - bset-fwste.
                  ENDIF.
                ENDIF.

              ENDSELECT.
              IF abs_fwbas > 0 AND abs_wrbtr > 0.
                item-act_tax_1 = item-act_tax_1 / abs_fwbas * abs_wrbtr.
                item-act_tax_2 = item-act_tax_2 / abs_fwbas * abs_wrbtr.
                item-act_tax_3 = item-act_tax_3 / abs_fwbas * abs_wrbtr.
                item-act_tax_4 = item-act_tax_4 / abs_fwbas * abs_wrbtr.
              ELSE.
                item-act_tax = 0.
                item-act_tax_1 = 0.
                item-act_tax_2 = 0.
                item-act_tax_3 = 0.
                item-act_tax_4 = 0.
              ENDIF.
            ENDSELECT.

          ENDIF.
          IF item-shkzg = 'H'.
            item-wrbtr = item-wrbtr * -1.
            item-rmwwr = item-rmwwr * -1.
          ENDIF.
          IF item-bvorg(10) NE item-belnr AND
             NOT item-bvorg IS INITIAL.
            CLEAR: item-act_tax, item-act_tax_1,
            item-act_tax_2, item-act_tax_3, item-act_tax_4.
          ENDIF.
          new_tax_tot = item-act_tax_1 + item-act_tax_2 +
                         item-act_tax_3 + item-act_tax_4.
          new_tax_tot = ABS( new_tax_tot ).
          old_tax_tot = ABS( item-act_tax ).
          IF old_tax_tot NE new_tax_tot.
            item-act_tax = item-act_tax_1 + item-act_tax_2 +
                           item-act_tax_3 + item-act_tax_4.
          ENDIF.
** Change sign of total invoice amount. Determine sign from vendor item on FI invoice
          DATA: vend_shkzg LIKE bseg-shkzg.
          CLEAR vend_shkzg.
* Accounting Document Segment
          SELECT SINGLE shkzg INTO vend_shkzg FROM bseg     "#EC *
                 WHERE bukrs = item-bukrs AND               "#EC *
                       belnr = item-belnr AND               "#EC *
                       gjahr = item-gjahr AND               "#EC *
                       koart = 'K'.
          IF vend_shkzg = 'S'.
            item-rmwwr = ABS( item-rmwwr ).
            item-rmwwr = item-rmwwr * -1.
          ELSE.
            item-rmwwr = ABS( item-rmwwr ).
          ENDIF.
          IF NOT item-aufnr is INITIAL.
            SELECT SINGLE BUKRS FROM AUFK INTO item-bbukrs
              WHERE AUFNR = item-aufnr.
            IF item-bbukrs is INITIAL.
              IF NOT item-ps_psp_pnr is INITIAL.
                SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
                  WHERE PSPNR = item-ps_psp_pnr.
              ELSEIF NOT item-kostl is INITIAL.
                SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
                  WHERE KOKRS = item-kokrs AND
                        KOSTL = item-kostl.
              ENDIF.
            ENDIF.
          ELSEIF NOT item-ps_psp_pnr is INITIAL.
            SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
              WHERE PSPNR = item-ps_psp_pnr.
          ELSEIF NOT item-kostl is INITIAL.
            SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
              WHERE KOKRS = item-kokrs AND
                    KOSTL = item-kostl.
          ENDIF.
          IF item-bbukrs in BBUKRS.
            APPEND item.
          ENDIF.

        ENDLOOP.
      ENDSELECT.
    ELSE.  "FI invoices
      IF item-bvorg IS INITIAL.  "Regular postings
        REFRESH: bseg_tab,
                 bset_tab.
* Tax Data Document Segment
        SELECT * FROM bset INTO TABLE bset_tab
                WHERE bukrs = item-bukrs AND
                     belnr = item-belnr AND
                     gjahr = item-gjahr AND
                     ( ktosl = 'NVV' OR
                       ktosl LIKE 'VS%' ).

*       Addded by Kushal to calculate total taxes from BSET - START
        LOOP AT bset_tab.

          bset-fwste  = bset_tab-fwste.
          item-txjcd  = bset_tab-txjdp.
          txjcd_old   = bset_tab-txjcd.
          bset-shkzg  = bset_tab-shkzg.
          bset-fwbas  = bset_tab-fwbas.
          bset-txjlv  = bset_tab-txjlv.
          bset-kschl  = bset_tab-kschl.

          IF bset-txjlv IS INITIAL.
            bset-txjlv = bset-kschl+2(1).
          ENDIF.
          " calculate total taxes
          item-tot_tax = item-tot_tax + bset-fwste.

          " set taxes at various levels (1, 2, 3 and (4 + 5 + 6))
          IF bset-txjlv = '1'.  "State
            item-tot_tax_1 = item-tot_tax_1 + bset-fwste.
          ELSEIF bset-txjlv = '2'.  "County
            item-tot_tax_2 = item-tot_tax_2 + bset-fwste.
          ELSEIF bset-txjlv = '3'.  "City
            item-tot_tax_3 = item-tot_tax_3 + bset-fwste.
          ELSE. "Secondary taxes
            item-tot_tax_4 = item-tot_tax_4 + bset-fwste.
          ENDIF.
          IF item-txjcd IS INITIAL.
            item-txjcd = txjcd_old.
          ENDIF.

        ENDLOOP.
*       Addded by Kushal to calculate total taxes from BSET - END
        SELECT * FROM bseg APPENDING TABLE bseg_tab
               WHERE bukrs = item-bukrs AND
                     belnr = item-belnr AND
                     gjahr = item-gjahr.
        CLEAR txgrp_sav.
        LOOP AT bseg_tab WHERE koart = 'K' AND
                               ( bschl = '31' OR
                                 bschl = '21' OR
                                 bschl = '32' OR
                                 bschl = '22' ).
          item-lifnr = bseg_tab-lifnr.
          item-zbd1t = bseg_tab-zbd1t.
          item-zbd2t = bseg_tab-zbd2t.
          item-zbd3t = bseg_tab-zbd3t.
          item-zbd1p = bseg_tab-zbd1p.
          item-zbd2p = bseg_tab-zbd2p.
          item-zfbdt = bseg_tab-zfbdt.
          item-faede_shkzg = bseg_tab-shkzg.
          IF bseg_tab-shkzg = 'S'.
            item-rmwwr = bseg_tab-wrbtr * -1.
          ELSE.
            item-rmwwr = bseg_tab-wrbtr.
          ENDIF.
        ENDLOOP.
*        IF sy-subrc = 0.
        REFRESH navfw_tab.
        LOOP AT bseg_tab WHERE koart NE 'K' AND
                               buzid NE 'T' AND
                               mwskz IN mwskz AND
*                                 wrbtr ne 0.
*                txbfw ne 0.     "Changed this for Nissan as KR/KG
               ( txbfw NE 0 OR  "without tax would not show otherwise
                                 ( txbfw = 0 AND
                                   txgrp NE space ) ).

          navfw_tab-txgrp = bseg_tab-txgrp.
          navfw_tab-navfw = bseg_tab-navfw.
          COLLECT navfw_tab.
        ENDLOOP.
        LOOP AT bseg_tab WHERE koart NE 'K' AND
                               buzid NE 'T' AND
                               mwskz IN mwskz AND
*                                 wrbtr ne 0.
*                txbfw ne 0.     "Changed this for Nissan as KR/KG
               ( txbfw NE 0 OR  "without tax would not show otherwise
                                 ( txbfw = 0 AND
                                   txgrp NE space ) ).

          item-kokrs = bseg_tab-kokrs.
          item-kostl = bseg_tab-kostl.
          item-txz01 = bseg_tab-sgtxt.
          item-saknr = bseg_tab-hkont.
*          item-zzloc = bseg_tab-zzloc.
          IF NOT item-aufnr is INITIAL.
            SELECT SINGLE BUKRS FROM AUFK INTO item-bbukrs
              WHERE AUFNR = item-aufnr.
            IF item-bbukrs is INITIAL.
              IF NOT item-ps_psp_pnr is INITIAL.
                SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
                  WHERE PSPNR = item-ps_psp_pnr.
              ELSEIF NOT item-kostl is INITIAL.
                SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
                  WHERE KOKRS = item-kokrs AND
                        KOSTL = item-kostl.
              ENDIF.
            ENDIF.
          ELSEIF NOT item-ps_psp_pnr is INITIAL.
            SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
              WHERE PSPNR = item-ps_psp_pnr.
          ELSEIF NOT item-kostl is INITIAL.
            SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
              WHERE KOKRS = item-kokrs AND
                    KOSTL = item-kostl.
          ENDIF.
*          select single desc40_n into item-desc40_n from zapt_location
*                 where zzloc = bseg_tab-zzloc.
          IF item-txjcd IS INITIAL.
            item-txjcd = bseg_tab-txjcd.
          ENDIF.
          IF item-tcode = 'MRKO'.
            CLEAR: mrko_bukrs, mrko_belnr, mrko_gjahr.

* Accounting Document Header
            SELECT SINGLE bukrs belnr gjahr                 "#EC *
                   INTO (mrko_bukrs, mrko_belnr, mrko_gjahr) "#EC *
                   FROM bkpf                                "#EC *
                   WHERE awtyp = 'MKPF' AND                 "#EC *
                         bukrs = bseg_tab-bukrs AND      "#EC *    "SJB
                         awkey = bseg_tab-zuonr.            "#EC *
            IF sy-subrc = 0.

* Accounting Document Segment
              SELECT SINGLE hkont INTO item-saknr           "#EC *
                     FROM bseg                              "#EC *
                     WHERE bukrs = mrko_bukrs AND           "#EC *
                           belnr = mrko_belnr AND           "#EC *
                           gjahr = mrko_gjahr AND           "#EC *
                           koart = 'M'.
            ENDIF.
          ENDIF.
          item-mwskz = bseg_tab-mwskz.
          item-buzei = bseg_tab-buzei.
          item-aufnr = bseg_tab-aufnr.
          item-nplnr = bseg_tab-nplnr.
          item-anln1 = bseg_tab-anln1.
          item-anln2 = bseg_tab-anln2.
          item-ps_psp_pnr = bseg_tab-projk.
          item-vbeln = bseg_tab-vbel2.
          item-vbelp = bseg_tab-posn2.
          item-ebeln = bseg_tab-ebeln.
          item-ebelp = bseg_tab-ebelp.
          item-zekkn = bseg_tab-zekkn.
          item-wrbtr_sav = bseg_tab-wrbtr.
          item-menge = bseg_tab-menge.
          item-bstme = bseg_tab-meins.
          item-shkzg = bseg_tab-shkzg.
          item-sgtxt = bseg_tab-sgtxt.
*************For consignment invoices get the account from
*************material document instead
*            if item-tcode = 'MRKO'.
*              select single sakto kostl aufnr anln1 anln2
*                            ps_psp_pnr kokrs matnr werks dmbtr
*                     into (item-saknr, item-kostl, item-aufnr,
*                           item-anln1, item-anln2, item-ps_psp_pnr,
*                           item-kokrs, item-matnr, item-werks,
*                           bseg_tab-txbfw)
*                     from mseg
*                     where mblnr = bseg_tab-zuonr(10) and
*                           mjahr = bseg_tab-zuonr+10(4) and
*                           zeile = bseg_tab-sgtxt+45(4).
*
**              select single bukrs belnr gjahr
**                     into (mrko_bukrs, mrko_belnr, mrko_gjahr)
**                     from bkpf
**                     where awtyp = 'MKPF' and
**                           awkey = bseg_tab-zuonr.
**              if sy-subrc = 0.
**                mrko_buzei = bseg_tab-sgtxt+46(4).
**                mrko_buzei = mrko_buzei * 2.
**                select single hkont kostl aufnr anln1 anln2
**                              projk kokrs matnr werks
**                       into (item-saknr, item-kostl, item-aufnr,
**                             item-anln1, item-anln2, item-ps_psp_pnr,
**                             item-kokrs, item-matnr, item-werks)
**                       from bseg
**                       where bukrs = mrko_bukrs and
**                             belnr = mrko_belnr and
**                             gjahr = mrko_gjahr and
**                             buzei = mrko_buzei and
***                            koart = 'M'.
**                             ktosl = 'GBB'.
**              endif.
*            endif.
          IF bseg_tab-txgrp NE '000' AND bseg_tab-txbfw = 0 AND
             item-blart NE 'WE' AND
             item-tcode NE 'MRKO'.
            CHECK bseg_tab-txgrp NE txgrp_sav.
          ENDIF.
          txgrp_sav = bseg_tab-txgrp.
          CLEAR accrual_flag.
          IF bseg_tab-txbfw NE 0.
            item-wrbtr = bseg_tab-txbfw.
          ELSEIF bseg_tab-txbfw = 0 AND bseg_tab-navfw NE 0 AND
                 bseg_tab-wrbtr NE bseg_tab-navfw.
            item-wrbtr = bseg_tab-wrbtr - bseg_tab-navfw.
          ELSE.
            item-wrbtr = bseg_tab-wrbtr.
            accrual_flag = 'X'.
          ENDIF.
          CLEAR: item-act_tax, item-act_tax_1, item-act_tax_2,
                 item-act_tax_3, item-act_tax_4.
          item-act_tax = bseg_tab-navfw.
          CLEAR: item-matnr, item-matkl, item-txz01, item-adrnr,
                 item-mwskz_po, item-wepos, item-werks, item-lgort,
                 item-knttp.

* Purchasing Document Item
          SELECT SINGLE matnr matkl txz01 adrnr adrn2 mwskz wepos werks lgort knttp
                   INTO (item-matnr, item-matkl, item-txz01, item-adrnr, item-adrn2,
                      item-mwskz_po, item-wepos, item-werks, item-lgort, item-knttp)
                     FROM ekpo
                     WHERE ebeln = item-ebeln AND
                           ebelp = item-ebelp.
          IF sy-subrc = 0.
            item-saknr_orig = item-saknr.

* Account Assignment in Purchasing Document
            SELECT SINGLE sakto INTO item-saknr FROM ekkn
                   WHERE ebeln = item-ebeln AND
                         ebelp = item-ebelp AND
                         zekkn = item-zekkn.
          ENDIF.

*            if item-tcode ne 'MRKO'.
*              clear: item-matnr, item-matkl, item-txz01, item-adrnr,
*                     item-mwskz_po, item-wepos, item-werks, item-lgort,
*                     item-knttp.
*              select single matnr matkl txz01 adrnr mwskz wepos werks
*                            lgort knttp
*                       into (item-matnr, item-matkl, item-txz01,
*                             item-adrnr,item-mwskz_po, item-wepos,
*                             item-werks, item-lgort, item-knttp)
*                         from ekpo
*                         where ebeln = item-ebeln and
*                               ebelp = item-ebelp.
*              if sy-subrc = 0.
*                item-saknr_orig = item-saknr.
*                select single sakto into item-saknr from ekkn
*                       where ebeln = item-ebeln and
*                             ebelp = item-ebelp and
*                             zekkn = item-zekkn.
*              endif.
*            endif.

*            select fwste txjdp shkzg fwbas txjlv kschl txgrp
*                 into (bset-fwste, item-txjcd, bset-shkzg, bset-fwbas,
*                         bset-txjlv, bset-kschl, bset-txgrp)
*                   from bset
*                   where bukrs = item-bukrs and
*                         belnr = item-belnr and
*                         gjahr = item-gjahr and
*                         ( txgrp = bseg_tab-txgrp or txgrp = 0 ) and
*                         ( ktosl = 'NVV' or
*                           ktosl like 'VS%' ).
          DESCRIBE TABLE navfw_tab LINES navfw_tab_lines.
          READ TABLE navfw_tab WITH KEY txgrp = bseg_tab-txgrp.

          IF ( bseg_tab-navfw EQ navfw_tab-navfw AND
               bseg_tab-txgrp NE 0 ) OR
             navfw_tab_lines = 1.

*if navfw_tab-navfw  0
            LOOP AT bset_tab
                  WHERE txgrp = bseg_tab-txgrp
                  OR    txgrp = 0.

              bset-fwste  = bset_tab-fwste.
              item-txjcd  = bset_tab-txjdp.
              txjcd_old   = bset_tab-txjcd.
              bset-shkzg  = bset_tab-shkzg.
              bset-fwbas  = bset_tab-fwbas.
              bset-txjlv  = bset_tab-txjlv.
              bset-kschl  = bset_tab-kschl.
              bset-txgrp  = bset_tab-txgrp.

              IF bset-txjlv IS INITIAL.
                bset-txjlv = bset-kschl+2(1).
              ENDIF.
              IF bset-fwbas NE bseg_tab-txbfw AND
                 bset-fwbas NE 0 AND
                 bseg_tab-txbfw NE 0.
                bset-fwste = bset-fwste / bset-fwbas * bseg_tab-txbfw.
              ENDIF.
              IF bseg_tab-navfw = 0.
                item-act_tax = item-act_tax + bset-fwste.
              ENDIF.
              IF bset-txjlv = '1'.  "State
                item-act_tax_1 = bset-fwste.
              ELSEIF bset-txjlv = '2'.  "County
                item-act_tax_2 = bset-fwste.
              ELSEIF bset-txjlv = '3'.  "City
                item-act_tax_3 = bset-fwste.
              ELSE. "Secondary taxes
                item-act_tax_4 = item-act_tax_4 + bset-fwste.
              ENDIF.
              IF item-txjcd IS INITIAL.
                item-txjcd = txjcd_old.
              ENDIF.

            ENDLOOP.
          ENDIF.

          IF item-act_tax NE 0 AND
             NOT accrual_flag IS INITIAL.
            item-wrbtr = item-wrbtr - item-act_tax.
          ENDIF.

          IF bseg_tab-shkzg = 'H'.
            item-wrbtr = item-wrbtr * -1.
            item-act_tax = item-act_tax * -1.
            item-act_tax_1 = item-act_tax_1 * -1.
            item-act_tax_2 = item-act_tax_2 * -1.
            item-act_tax_3 = item-act_tax_3 * -1.
            item-act_tax_4 = item-act_tax_4 * -1.
            item-menge = item-menge * -1.
          ENDIF.

          IF bset-txgrp NE 0 AND navfw_tab_lines NE 1.
            bset_tax = item-act_tax_1 + item-act_tax_2 +
                       item-act_tax_3 + item-act_tax_4.
            IF bset_tax NE item-act_tax.
              item-act_tax = bset_tax.
            ENDIF.
          ELSE.
            item-act_tax_1 = item-act_tax * item-tot_tax_1 /
                                       item-tot_tax.
            item-act_tax_2 = item-act_tax * item-tot_tax_2 /
                                       item-tot_tax.
            item-act_tax_3 = item-act_tax * item-tot_tax_3 /
                                       item-tot_tax.
            item-act_tax_4 = item-act_tax * item-tot_tax_4 /
                                       item-tot_tax.
          ENDIF.

          IF ttxd-xextn = 'S'.
            CHECK item-txjcd+2(2) IN regio.
          ELSE.
            CHECK item-txjcd(2) IN regio.
          ENDIF.
          CHECK item-saknr IN saknr OR
                item-saknr IN grir.
          CHECK item-werks IN werks.
          CHECK item-belnri IN belnri.
          CHECK item-ebeln IN ebeln.
          CHECK item-lifnr IN lifnr.
          CHECK NOT item-lifnr IS INITIAL.
          CHECK item-matnr IN matnr.
          CHECK item-matkl IN matkl.
          CHECK item-kostl IN kostl.
          IF NOT item-aufnr is INITIAL.
            SELECT SINGLE BUKRS FROM AUFK INTO item-bbukrs
              WHERE AUFNR = item-aufnr.
            IF item-bbukrs is INITIAL.
              IF NOT item-ps_psp_pnr is INITIAL.
                SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
                  WHERE PSPNR = item-ps_psp_pnr.
              ELSEIF NOT item-kostl is INITIAL.
                SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
                  WHERE KOKRS = item-kokrs AND
                        KOSTL = item-kostl.
              ENDIF.
            ENDIF.
          ELSEIF NOT item-ps_psp_pnr is INITIAL.
            SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
              WHERE PSPNR = item-ps_psp_pnr.
          ELSEIF NOT item-kostl is INITIAL.
            SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
              WHERE KOKRS = item-kokrs AND
                    KOSTL = item-kostl.
          ENDIF.
          CHECK item-bbukrs in BBUKRS.
          APPEND item.
        ENDLOOP.
********Akzo Nobel: get FI AP docs posted without tax code and exactly
********one vendor line
        IF sy-subrc NE 0.
          CLEAR vendor_lines.
          LOOP AT bseg_tab WHERE koart = 'K' AND
                                 ( bschl = '31' OR
                                   bschl = '21' OR
                                   bschl = '32' OR
                                   bschl = '22' ).
            vendor_lines = vendor_lines + 1.
          ENDLOOP.
          IF vendor_lines = 1.
            LOOP AT bseg_tab WHERE koart NE 'K' AND
                                   buzid NE 'T' AND
                                   mwskz = space.
              item-kokrs = bseg_tab-kokrs.
              item-kostl = bseg_tab-kostl.
              item-txz01 = bseg_tab-sgtxt.
              item-saknr = bseg_tab-hkont.
              item-mwskz = bseg_tab-mwskz.
              item-buzei = bseg_tab-buzei.
              item-aufnr = bseg_tab-aufnr.
              item-nplnr = bseg_tab-nplnr.
              item-anln1 = bseg_tab-anln1.
              item-anln2 = bseg_tab-anln2.
              item-ps_psp_pnr = bseg_tab-projk.
              item-vbeln = bseg_tab-vbel2.
              item-vbelp = bseg_tab-posn2.
              item-ebeln = bseg_tab-ebeln.
              item-ebelp = bseg_tab-ebelp.
              item-zekkn = bseg_tab-zekkn.
              item-wrbtr_sav = bseg_tab-wrbtr.
              item-menge = bseg_tab-menge.
              item-bstme = bseg_tab-meins.
              item-shkzg = bseg_tab-shkzg.
              item-wrbtr = bseg_tab-wrbtr.
              item-sgtxt = bseg_tab-sgtxt.
*              item-zzloc = bseg_tab-zzloc.
              IF NOT item-aufnr is INITIAL.
                SELECT SINGLE BUKRS FROM AUFK INTO item-bbukrs
                  WHERE AUFNR = item-aufnr.
                IF item-bbukrs is INITIAL.
                  IF NOT item-ps_psp_pnr is INITIAL.
                    SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
                      WHERE PSPNR = item-ps_psp_pnr.
                  ELSEIF NOT item-kostl is INITIAL.
                    SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
                      WHERE KOKRS = item-kokrs AND
                            KOSTL = item-kostl.
                  ENDIF.
                ENDIF.
              ELSEIF NOT item-ps_psp_pnr is INITIAL.
                SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
                  WHERE PSPNR = item-ps_psp_pnr.
              ELSEIF NOT item-kostl is INITIAL.
                SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
                  WHERE KOKRS = item-kokrs AND
                        KOSTL = item-kostl.
              ENDIF.
*              select single desc40_n into item-desc40_n from zapt_location
*                     where zzloc = bseg_tab-zzloc.
              IF item-txjcd IS INITIAL.
                item-txjcd = bseg_tab-txjcd.
              ENDIF.
              CLEAR: item-act_tax, item-act_tax_1, item-act_tax_2,
                     item-act_tax_3, item-act_tax_4.
              CLEAR: item-matnr, item-matkl, item-txz01, item-adrnr,
                     item-mwskz_po, item-wepos, item-werks, item-lgort,
                     item-knttp, item-belnri.

              IF ttxd-xextn = 'S'.
                CHECK item-txjcd+2(2) IN regio.
              ELSE.
                CHECK item-txjcd(2) IN regio.
              ENDIF.
              CHECK item-saknr IN saknr OR
                    item-saknr IN grir.
              CHECK item-werks IN werks.
              CHECK item-belnri IN belnri.
              CHECK item-ebeln IN ebeln.
              CHECK item-lifnr IN lifnr.
              CHECK NOT item-lifnr IS INITIAL.
              CHECK item-matnr IN matnr.
              CHECK item-matkl IN matkl.
              CHECK item-kostl IN kostl.
              IF bseg_tab-shkzg = 'H'.
                item-wrbtr = item-wrbtr * -1.
              ENDIF.
              CHECK item-bbukrs in BBUKRS.

              APPEND item.
            ENDLOOP.
          ENDIF.
        ENDIF.
** End Akzo
*        ENDIF.
      ELSE.     "Intercompany postings
        "skip the primary posting containing the intercompany account
        t_bukrs = item-bvorg+10(4).
        t_belnr = item-bvorg(10).

* Intercompany posting procedures
        SELECT * FROM bvor
               WHERE bvorg = item-bvorg.

          SELECT SINGLE lifnr wrbtr                         "#EC *
                 INTO (item-lifnr, item-rmwwr)              "#EC *
                 FROM bseg                                  "#EC *
                 WHERE bukrs = bvor-bukrs AND               "#EC *
                       belnr = bvor-belnr AND               "#EC *
                       gjahr = bvor-gjahr AND               "#EC *
                       koart = 'K'.
          IF sy-subrc = 0.
            EXIT.
          ENDIF.
        ENDSELECT.

        REFRESH: bseg_tab,
                 bset_tab.
* Tax Data Document Segment
        SELECT * FROM bset INTO TABLE bset_tab
                WHERE bukrs = t_bukrs AND
                     belnr = t_belnr AND
                     gjahr = item-gjahr AND
                     ( ktosl = 'NVV' OR
                       ktosl LIKE 'VS%' ).

*       Addded by Kushal to calculate total taxes from BSET - START
        LOOP AT bset_tab.

          bset-fwste  = bset_tab-fwste.
          item-txjcd  = bset_tab-txjdp.
          txjcd_old   = bset_tab-txjcd.
          bset-shkzg  = bset_tab-shkzg.
          bset-fwbas  = bset_tab-fwbas.
          bset-txjlv  = bset_tab-txjlv.
          bset-kschl  = bset_tab-kschl.

          IF bset-txjlv IS INITIAL.
            bset-txjlv = bset-kschl+2(1).
          ENDIF.
          " calculate total taxes
          item-tot_tax = item-tot_tax + bset-fwste.

          " set taxes at various levels (1, 2, 3 and (4 + 5 + 6))
          IF bset-txjlv = '1'.  "State
            item-tot_tax_1 = item-tot_tax_1 + bset-fwste.
          ELSEIF bset-txjlv = '2'.  "County
            item-tot_tax_2 = item-tot_tax_2 + bset-fwste.
          ELSEIF bset-txjlv = '3'.  "City
            item-tot_tax_3 = item-tot_tax_3 + bset-fwste.
          ELSE. "Secondary taxes
            item-tot_tax_4 = item-tot_tax_4 + bset-fwste.
          ENDIF.
          IF item-txjcd IS INITIAL.
            item-txjcd = txjcd_old.
          ENDIF.

        ENDLOOP.
*       Addded by Kushal to calculate total taxes from BSET - END
        SELECT * FROM bseg APPENDING TABLE bseg_tab
               WHERE bukrs = item-bukrs AND
                     belnr = item-belnr AND
                     gjahr = item-gjahr.
        CLEAR txgrp_sav.
        LOOP AT bseg_tab WHERE koart = 'K' AND
                               ( bschl = '31' OR
                                 bschl = '21' OR
                                 bschl = '32' OR
                                 bschl = '22' ).
          item-lifnr = bseg_tab-lifnr.
          item-zbd1t = bseg_tab-zbd1t.
          item-zbd2t = bseg_tab-zbd2t.
          item-zbd3t = bseg_tab-zbd3t.
          item-zbd1p = bseg_tab-zbd1p.
          item-zbd2p = bseg_tab-zbd2p.
          item-zfbdt = bseg_tab-zfbdt.
          item-faede_shkzg = bseg_tab-shkzg.
          IF bseg_tab-shkzg = 'S'.
            item-rmwwr = bseg_tab-wrbtr * -1.
          ELSE.
            item-rmwwr = bseg_tab-wrbtr.
          ENDIF.
        ENDLOOP.
*        IF sy-subrc = 0.
        REFRESH navfw_tab.
        LOOP AT bseg_tab WHERE koart NE 'K' AND
                               buzid NE 'T' AND
                               mwskz IN mwskz AND
*                                 wrbtr ne 0.
*                txbfw ne 0.     "Changed this for Nissan as KR/KG
               ( txbfw NE 0 OR  "without tax would not show otherwise
                                 ( txbfw = 0 AND
                                   txgrp NE space ) ).

          navfw_tab-txgrp = bseg_tab-txgrp.
          navfw_tab-navfw = bseg_tab-navfw.
          COLLECT navfw_tab.
        ENDLOOP.
        LOOP AT bseg_tab WHERE koart NE 'K' AND
                               buzid NE 'T' AND
                               mwskz IN mwskz AND
*                                 wrbtr ne 0.
*                txbfw ne 0.     "Changed this for Nissan as KR/KG
               ( txbfw NE 0 OR  "without tax would not show otherwise
                                 ( txbfw = 0 AND
                                   txgrp NE space ) ).

          item-kokrs = bseg_tab-kokrs.
          item-kostl = bseg_tab-kostl.
          item-txz01 = bseg_tab-sgtxt.
          item-saknr = bseg_tab-hkont.
*          item-zzloc = bseg_tab-zzloc.
          IF NOT item-aufnr is INITIAL.
            SELECT SINGLE BUKRS FROM AUFK INTO item-bbukrs
              WHERE AUFNR = item-aufnr.
            IF item-bbukrs is INITIAL.
              IF NOT item-ps_psp_pnr is INITIAL.
                SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
                  WHERE PSPNR = item-ps_psp_pnr.
              ELSEIF NOT item-kostl is INITIAL.
                SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
                  WHERE KOKRS = item-kokrs AND
                        KOSTL = item-kostl.
              ENDIF.
            ENDIF.
          ELSEIF NOT item-ps_psp_pnr is INITIAL.
            SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
              WHERE PSPNR = item-ps_psp_pnr.
          ELSEIF NOT item-kostl is INITIAL.
            SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
              WHERE KOKRS = item-kokrs AND
                    KOSTL = item-kostl.
          ENDIF.
*          select single desc40_n into item-desc40_n from zapt_location
*                 where zzloc = bseg_tab-zzloc.
          IF item-tcode = 'MRKO'.
            CLEAR: mrko_bukrs, mrko_belnr, mrko_gjahr.

* Accounting Document Header
            SELECT SINGLE bukrs belnr gjahr                 "#EC *
                   INTO (mrko_bukrs, mrko_belnr, mrko_gjahr) "#EC *
                   FROM bkpf                                "#EC *
                   WHERE awtyp = 'MKPF' AND                 "#EC *
                         bukrs = bseg_tab-bukrs AND      "#EC *    "SJB
                         awkey = bseg_tab-zuonr.            "#EC *
            IF sy-subrc = 0.

* Accounting Document Segment
              SELECT SINGLE hkont INTO item-saknr           "#EC *
                     FROM bseg                              "#EC *
                     WHERE bukrs = mrko_bukrs AND           "#EC *
                           belnr = mrko_belnr AND           "#EC *
                           gjahr = mrko_gjahr AND           "#EC *
                           koart = 'M'.
            ENDIF.
          ENDIF.
          item-mwskz = bseg_tab-mwskz.
          item-buzei = bseg_tab-buzei.
          item-aufnr = bseg_tab-aufnr.
          item-nplnr = bseg_tab-nplnr.
          item-anln1 = bseg_tab-anln1.
          item-anln2 = bseg_tab-anln2.
          item-ps_psp_pnr = bseg_tab-projk.
          item-ebeln = bseg_tab-ebeln.
          item-ebelp = bseg_tab-ebelp.
          item-vbeln = bseg_tab-vbel2.
          item-vbelp = bseg_tab-posn2.
          item-zekkn = bseg_tab-zekkn.
          item-wrbtr_sav = bseg_tab-wrbtr.
          item-menge = bseg_tab-menge.
          item-bstme = bseg_tab-meins.
          item-shkzg = bseg_tab-shkzg.
          item-sgtxt = bseg_tab-sgtxt.
*************For consignment invoices get the account from
*************material document instead
*            if item-tcode = 'MRKO'.
*              select single sakto kostl aufnr anln1 anln2
*                            ps_psp_pnr kokrs matnr werks dmbtr
*                     into (item-saknr, item-kostl, item-aufnr,
*                           item-anln1, item-anln2, item-ps_psp_pnr,
*                           item-kokrs, item-matnr, item-werks,
*                           bseg_tab-txbfw)
*                     from mseg
*                     where mblnr = bseg_tab-zuonr(10) and
*                           mjahr = bseg_tab-zuonr+10(4) and
*                           zeile = bseg_tab-sgtxt+45(4).
*
**              select single bukrs belnr gjahr
**                     into (mrko_bukrs, mrko_belnr, mrko_gjahr)
**                     from bkpf
**                     where awtyp = 'MKPF' and
**                           awkey = bseg_tab-zuonr.
**              if sy-subrc = 0.
**                mrko_buzei = bseg_tab-sgtxt+46(4).
**                mrko_buzei = mrko_buzei * 2.
**                select single hkont kostl aufnr anln1 anln2
**                              projk kokrs matnr werks
**                       into (item-saknr, item-kostl, item-aufnr,
**                             item-anln1, item-anln2, item-ps_psp_pnr,
**                             item-kokrs, item-matnr, item-werks)
**                       from bseg
**                       where bukrs = mrko_bukrs and
**                             belnr = mrko_belnr and
**                             gjahr = mrko_gjahr and
**                             buzei = mrko_buzei and
***                            koart = 'M'.
**                             ktosl = 'GBB'.
**              endif.
*            endif.
          IF bseg_tab-txgrp NE '000' AND bseg_tab-txbfw = 0 AND
             item-blart NE 'WE' AND
             item-tcode NE 'MRKO'.
            CHECK bseg_tab-txgrp NE txgrp_sav.
          ENDIF.
          txgrp_sav = bseg_tab-txgrp.
          CLEAR accrual_flag.
          IF bseg_tab-txbfw NE 0.
            item-wrbtr = bseg_tab-txbfw.
          ELSEIF bseg_tab-txbfw = 0 AND bseg_tab-navfw NE 0 AND
                 bseg_tab-wrbtr NE bseg_tab-navfw.
            item-wrbtr = bseg_tab-wrbtr - bseg_tab-navfw.
          ELSE.
            item-wrbtr = bseg_tab-wrbtr.
            accrual_flag = 'X'.
          ENDIF.
          CLEAR: item-act_tax, item-act_tax_1, item-act_tax_2,
                 item-act_tax_3, item-act_tax_4.
          item-act_tax = bseg_tab-navfw.
          CLEAR: item-matnr, item-matkl, item-txz01, item-adrnr,
                 item-mwskz_po, item-wepos, item-werks, item-lgort,
                 item-knttp.

* Purchasing Document Item
          SELECT SINGLE matnr matkl txz01 adrnr adrn2 mwskz wepos werks lgort knttp
                   INTO (item-matnr, item-matkl, item-txz01, item-adrnr, item-adrn2,
                      item-mwskz_po, item-wepos, item-werks, item-lgort, item-knttp)
                     FROM ekpo
                     WHERE ebeln = item-ebeln AND
                           ebelp = item-ebelp.
          IF sy-subrc = 0.
            item-saknr_orig = item-saknr.

* Account Assignment in Purchasing Document
            SELECT SINGLE sakto INTO item-saknr FROM ekkn
                   WHERE ebeln = item-ebeln AND
                         ebelp = item-ebelp AND
                         zekkn = item-zekkn.
          ENDIF.

*            if item-tcode ne 'MRKO'.
*              clear: item-matnr, item-matkl, item-txz01, item-adrnr,
*                     item-mwskz_po, item-wepos, item-werks, item-lgort,
*                     item-knttp.
*              select single matnr matkl txz01 adrnr mwskz wepos werks
*                            lgort knttp
*                       into (item-matnr, item-matkl, item-txz01,
*                             item-adrnr,item-mwskz_po, item-wepos,
*                             item-werks, item-lgort, item-knttp)
*                         from ekpo
*                         where ebeln = item-ebeln and
*                               ebelp = item-ebelp.
*              if sy-subrc = 0.
*                item-saknr_orig = item-saknr.
*                select single sakto into item-saknr from ekkn
*                       where ebeln = item-ebeln and
*                             ebelp = item-ebelp and
*                             zekkn = item-zekkn.
*              endif.
*            endif.

*            select fwste txjdp shkzg fwbas txjlv kschl txgrp
*                 into (bset-fwste, item-txjcd, bset-shkzg, bset-fwbas,
*                         bset-txjlv, bset-kschl, bset-txgrp)
*                   from bset
*                   where bukrs = item-bukrs and
*                         belnr = item-belnr and
*                         gjahr = item-gjahr and
*                         ( txgrp = bseg_tab-txgrp or txgrp = 0 ) and
*                         ( ktosl = 'NVV' or
*                           ktosl like 'VS%' ).
          DESCRIBE TABLE navfw_tab LINES navfw_tab_lines.
          READ TABLE navfw_tab WITH KEY txgrp = bseg_tab-txgrp.

          IF ( bseg_tab-navfw EQ navfw_tab-navfw AND
               bseg_tab-txgrp NE 0 ) OR
             navfw_tab_lines = 1.

*if navfw_tab-navfw  0
            LOOP AT bset_tab
                  WHERE txgrp = bseg_tab-txgrp
                  OR    txgrp = 0.

              bset-fwste  = bset_tab-fwste.
              item-txjcd  = bset_tab-txjdp.
              txjcd_old   = bset_tab-txjcd.
              bset-shkzg  = bset_tab-shkzg.
              bset-fwbas  = bset_tab-fwbas.
              bset-txjlv  = bset_tab-txjlv.
              bset-kschl  = bset_tab-kschl.
              bset-txgrp  = bset_tab-txgrp.

              IF bset-txjlv IS INITIAL.
                bset-txjlv = bset-kschl+2(1).
              ENDIF.
              IF bset-fwbas NE bseg_tab-txbfw AND
                 bset-fwbas NE 0 AND
                 bseg_tab-txbfw NE 0.
                bset-fwste = bset-fwste / bset-fwbas * bseg_tab-txbfw.
              ENDIF.
              IF bseg_tab-navfw = 0.
                item-act_tax = item-act_tax + bset-fwste.
              ENDIF.
              IF bset-txjlv = '1'.  "State
                item-act_tax_1 = bset-fwste.
              ELSEIF bset-txjlv = '2'.  "County
                item-act_tax_2 = bset-fwste.
              ELSEIF bset-txjlv = '3'.  "City
                item-act_tax_3 = bset-fwste.
              ELSE. "Secondary taxes
                item-act_tax_4 = item-act_tax_4 + bset-fwste.
              ENDIF.
              IF item-txjcd IS INITIAL.
                item-txjcd = txjcd_old.
              ENDIF.

            ENDLOOP.
          ENDIF.

          IF item-act_tax NE 0 AND
             NOT accrual_flag IS INITIAL.
            item-wrbtr = item-wrbtr - item-act_tax.
          ENDIF.

          IF bseg_tab-shkzg = 'H'.
            item-wrbtr = item-wrbtr * -1.
            item-act_tax = item-act_tax * -1.
            item-act_tax_1 = item-act_tax_1 * -1.
            item-act_tax_2 = item-act_tax_2 * -1.
            item-act_tax_3 = item-act_tax_3 * -1.
            item-act_tax_4 = item-act_tax_4 * -1.
            item-menge = item-menge * -1.
          ENDIF.

          IF bset-txgrp NE 0 AND navfw_tab_lines NE 1.
            bset_tax = item-act_tax_1 + item-act_tax_2 +
                       item-act_tax_3 + item-act_tax_4.
            IF bset_tax NE item-act_tax.
              item-act_tax = bset_tax.
            ENDIF.
          ELSE.
            item-act_tax_1 = item-act_tax * item-tot_tax_1 /
                                       item-tot_tax.
            item-act_tax_2 = item-act_tax * item-tot_tax_2 /
                                       item-tot_tax.
            item-act_tax_3 = item-act_tax * item-tot_tax_3 /
                                       item-tot_tax.
            item-act_tax_4 = item-act_tax * item-tot_tax_4 /
                                       item-tot_tax.
          ENDIF.

          IF ttxd-xextn = 'S'.
            CHECK item-txjcd+2(2) IN regio.
          ELSE.
            CHECK item-txjcd(2) IN regio.
          ENDIF.
          CHECK item-saknr IN saknr OR
                item-saknr IN grir.
          CHECK item-werks IN werks.
          CHECK item-belnri IN belnri.
          CHECK item-ebeln IN ebeln.
          CHECK item-lifnr IN lifnr.
          CHECK NOT item-lifnr IS INITIAL.
          CHECK item-matnr IN matnr.
          CHECK item-matkl IN matkl.
          CHECK item-kostl IN kostl.
          IF NOT item-aufnr is INITIAL.
            SELECT SINGLE BUKRS FROM AUFK INTO item-bbukrs
              WHERE AUFNR = item-aufnr.
            IF item-bbukrs is INITIAL.
              IF NOT item-ps_psp_pnr is INITIAL.
                SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
                  WHERE PSPNR = item-ps_psp_pnr.
              ELSEIF NOT item-kostl is INITIAL.
                SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
                  WHERE KOKRS = item-kokrs AND
                        KOSTL = item-kostl.
              ENDIF.
            ENDIF.
          ELSEIF NOT item-ps_psp_pnr is INITIAL.
            SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
              WHERE PSPNR = item-ps_psp_pnr.
          ELSEIF NOT item-kostl is INITIAL.
            SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
              WHERE KOKRS = item-kokrs AND
                    KOSTL = item-kostl.
          ENDIF.
          CHECK item-bbukrs in BBUKRS.
          APPEND item.
        ENDLOOP.
********Akzo Nobel: get FI AP docs posted without tax code and exactly
********one vendor line
        IF sy-subrc NE 0.
          CLEAR vendor_lines.
          LOOP AT bseg_tab WHERE koart = 'K' AND
                                 ( bschl = '31' OR
                                   bschl = '21' OR
                                   bschl = '32' OR
                                   bschl = '22' ).
            vendor_lines = vendor_lines + 1.
          ENDLOOP.
          IF vendor_lines = 1.
            LOOP AT bseg_tab WHERE koart NE 'K' AND
                                   buzid NE 'T' AND
                                   mwskz = space.
              item-kokrs = bseg_tab-kokrs.
              item-kostl = bseg_tab-kostl.
              item-txz01 = bseg_tab-sgtxt.
              item-saknr = bseg_tab-hkont.
              item-mwskz = bseg_tab-mwskz.
              item-buzei = bseg_tab-buzei.
              item-aufnr = bseg_tab-aufnr.
              item-nplnr = bseg_tab-nplnr.
              item-anln1 = bseg_tab-anln1.
              item-anln2 = bseg_tab-anln2.
              item-ps_psp_pnr = bseg_tab-projk.
              item-vbeln = bseg_tab-vbel2.
              item-vbelp = bseg_tab-posn2.
              item-ebeln = bseg_tab-ebeln.
              item-ebelp = bseg_tab-ebelp.
              item-zekkn = bseg_tab-zekkn.
              item-wrbtr_sav = bseg_tab-wrbtr.
              item-menge = bseg_tab-menge.
              item-bstme = bseg_tab-meins.
              item-shkzg = bseg_tab-shkzg.
              item-wrbtr = bseg_tab-wrbtr.
              item-sgtxt = bseg_tab-sgtxt.
*              item-zzloc = bseg_tab-zzloc.
              IF NOT item-aufnr is INITIAL.
                SELECT SINGLE BUKRS FROM AUFK INTO item-bbukrs
                  WHERE AUFNR = item-aufnr.
                IF item-bbukrs is INITIAL.
                  IF NOT item-ps_psp_pnr is INITIAL.
                    SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
                      WHERE PSPNR = item-ps_psp_pnr.
                  ELSEIF NOT item-kostl is INITIAL.
                    SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
                      WHERE KOKRS = item-kokrs AND
                            KOSTL = item-kostl.
                  ENDIF.
                ENDIF.
              ELSEIF NOT item-ps_psp_pnr is INITIAL.
                SELECT SINGLE PBUKR FROM PRPS INTO item-bbukrs
                  WHERE PSPNR = item-ps_psp_pnr.
              ELSEIF NOT item-kostl is INITIAL.
                SELECT SINGLE BUKRS FROM CSKS INTO item-bbukrs
                  WHERE KOKRS = item-kokrs AND
                        KOSTL = item-kostl.
              ENDIF.
*              select single desc40_n into item-desc40_n from zapt_location
*                     where zzloc = bseg_tab-zzloc.
              CLEAR: item-act_tax, item-act_tax_1, item-act_tax_2,
                     item-act_tax_3, item-act_tax_4.
              CLEAR: item-matnr, item-matkl, item-txz01, item-adrnr,
                     item-mwskz_po, item-wepos, item-werks, item-lgort,
                     item-knttp, item-belnri.
              IF ttxd-xextn = 'S'.
                CHECK item-txjcd+2(2) IN regio.
              ELSE.
                CHECK item-txjcd(2) IN regio.
              ENDIF.
              CHECK item-saknr IN saknr OR
                    item-saknr IN grir.
              CHECK item-werks IN werks.
              CHECK item-belnri IN belnri.
              CHECK item-ebeln IN ebeln.
              CHECK item-lifnr IN lifnr.
              CHECK NOT item-lifnr IS INITIAL.
              CHECK item-matnr IN matnr.
              CHECK item-matkl IN matkl.
              CHECK item-kostl IN kostl.
              IF bseg_tab-shkzg = 'H'.
                item-wrbtr = item-wrbtr * -1.
              ENDIF.
              CHECK item-bbukrs in BBUKRS.
              APPEND item.
            ENDLOOP.
          ENDIF.
        ENDIF.
** End Akzo
*        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR item.
  ENDSELECT.

** Add additional journal postings to allow report totals to reconcile to GL
  IF NOT recon IS INITIAL.
    SELECT bsis~bukrs bsis~belnr bsis~gjahr bsis~buzei bsis~blart bsis~budat
           bsis~bldat bsis~waers bkpf~awtyp bkpf~awkey bsis~wrbtr bsis~hkont AS saknr
           bsis~shkzg
           bkpf~usnam bkpf~bvorg bkpf~xblnr bkpf~tcode bkpf~bktxt bsis~sgtxt
           bsis~prctr bsis~wrbtr AS glamt bsis~buzid
           bsis~vbund
           INTO CORRESPONDING FIELDS OF TABLE item_bsis
                       FROM bsis JOIN bkpf ON ( bsis~bukrs = bkpf~bukrs AND
                                                bsis~belnr = bkpf~belnr AND
                                                  bsis~gjahr = bkpf~gjahr )
                                           WHERE bsis~bukrs IN bukrs AND
                                                 bsis~hkont IN saknr AND
                                                 bkpf~bstat = space AND
                                                 bkpf~budat IN budat1 AND
                                                 bsis~buzid NE 'K' AND
                                                 bsis~buzid NE 'W' AND
                                                 bsis~buzid NE 'T'.


  ENDIF.

ENDFORM.                    " Main_select
*&---------------------------------------------------------------------*
*&      Form  add_tax_and_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_tax_and_fields .


  REFRESH ekbe_excl.

  LOOP AT item.
*   PERFORM recalculate_tax.
    IF NOT recon IS INITIAL.
      PERFORM merge_item_bsis.
    ENDIF.
    PERFORM additional_fields.
    IF item-tax_dif > 0.
      IF item-tax_dif > tolov.
        MODIFY item.
      ELSE.
        DELETE item.
      ENDIF.
    ELSEIF item-tax_dif < 0.
      diff = ABS( item-tax_dif ).
      IF diff > tolun.
        MODIFY item.
      ELSE.
        DELETE item.
      ENDIF.
    ELSEIF item-tax_dif = 0.
      IF tolov = 0 OR
         tolun = 0.
        MODIFY item.
      ELSE.
        DELETE item.
      ENDIF.
    ENDIF.
    IF NOT item-saknr IN saknr.
      DELETE item.
    ENDIF.
    IF NOT item-prctr IN prctr.
      DELETE item.
    ENDIF.
    IF NOT item-vbund IN vbund.
      DELETE item.
    ENDIF.
  ENDLOOP.

  LOOP AT item_bsis INTO item_bsis_wa.
*Begin of change by somsx
    if item_bsis_wa-vbund is initial.
      item_bsis_wa-vbund = item_bsis_wa-bukrs.
    endif.
*End of change by somsx
    item_bsis_wa-rec_type = 'V'.
    item_bsis_wa-glamt = item_bsis_wa-wrbtr.
    IF item_bsis_wa-shkzg = 'H'.
      item_bsis_wa-glamt = item_bsis_wa-glamt * -1.
      item_bsis_wa-wrbtr = item_bsis_wa-wrbtr * -1.
    ENDIF.
    IF item_bsis_wa-buzid = 'M'.
      SELECT SINGLE ebeln ebelp menge meins AS bstme lifnr
             INTO CORRESPONDING FIELDS OF item_bsis_wa
             FROM bseg
             WHERE bukrs = item_bsis_wa-bukrs AND
                   belnr = item_bsis_wa-belnr AND
                   gjahr = item_bsis_wa-gjahr AND
                   buzei = item_bsis_wa-buzei.
      SELECT SINGLE name1 regio INTO (item_bsis_wa-name1, item_bsis_wa-vend_regio)
             FROM lfa1
             WHERE lifnr = item_bsis_wa-lifnr.
      SELECT SINGLE mwskz txjcd matnr matkl txz01
             INTO (item_bsis_wa-mwskz_po, item_bsis_wa-txjcd, item_bsis_wa-matnr,
                   item_bsis_wa-matkl, item_bsis_wa-txz01)
             FROM ekpo
             WHERE ebeln = item_bsis_wa-ebeln AND
                   ebelp = item_bsis_wa-ebelp.
** Material Group Name
      SELECT SINGLE wgbez INTO item_bsis_wa-wgbez FROM t023t
             WHERE spras = sy-langu AND
                   matkl = item_bsis_wa-matkl.
    else.
      SELECT SINGLE mwskz txjcd
             INTO CORRESPONDING FIELDS OF item_bsis_wa
             FROM bseg
             WHERE bukrs = item_bsis_wa-bukrs AND
                   belnr = item_bsis_wa-belnr AND
                   gjahr = item_bsis_wa-gjahr AND
                   buzei = item_bsis_wa-buzei.
    ENDIF.
** Currency conversion factor
    DATA: gdatu LIKE tcurf-gdatu,
          budat_inv LIKE tcurf-gdatu,
          ext_date(10),
          t001_waers LIKE t001-waers.
    WRITE item_bsis_wa-budat TO ext_date.

    SELECT SINGLE waers INTO t001_waers FROM t001 WHERE bukrs = item_bsis_wa-bukrs.

* invert date for tcurf - Convert date to the internal format(GDATU) accepted by currency table
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = ext_date
      IMPORTING
        output = budat_inv.

* Conversion Factors - Get currency conversion factors from the table tcurf.
    SELECT ffact gdatu INTO (item_bsis_wa-ffact, gdatu)
           FROM tcurf
           WHERE kurst = 'M' AND
                 fcurr = item_bsis_wa-waers AND
                 tcurr = t001_waers AND
                 gdatu GE budat_inv                       "#EC PORTABLE
        ORDER BY gdatu ASCENDING.
      EXIT.
    ENDSELECT.
    IF item_bsis_wa-kursf IS INITIAL OR sy-subrc NE 0.
      item_bsis_wa-kursf = 1.
      item_bsis_wa-ffact = 1.
    ENDIF.

    MODIFY TABLE item_bsis FROM item_bsis_wa.
  ENDLOOP.

  APPEND LINES OF item_bsis TO item.

  SORT item BY bukrs belnr gjahr.

ENDFORM.                    " add_tax_and_fields

*&---------------------------------------------------------------------*
*&      Form  determine_prod_code
*&---------------------------------------------------------------------*
*       This routine is specific to each customer. Depends on how
*       Taxware product code is determined in user-exit
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM determine_prod_code USING xprcd.
*
** Tax Keys
*  SELECT SINGLE procd INTO xprcd FROM t007a
*         WHERE kalsm = kalsm AND
*               mwskz = item-mwskz.
*
*ENDFORM.                    " determine_prod_code
*&---------------------------------------------------------------------*
*&      Form  output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM output.

  FIELD-SYMBOLS <fs> TYPE ANY.
  DATA: txt_out(1000),
        txt1(50).

  DATA: lv_string TYPE STRING.                              "DECK914346

  IF NOT o_file IS INITIAL.
    OPEN DATASET o_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      WRITE: / 'Error opening file:', o_file.
      STOP.
    ELSE.
      PERFORM insert_header.
    ENDIF.
  ENDIF.

  LOOP AT item.
    CLEAR ztax_validate1.
    MOVE-CORRESPONDING item TO ztax_validate1.
    PERFORM convert_txjcd.
    PERFORM get_gltxt.
    PERFORM calc_tax_percent.

* Begin changes - insert code  JRHARTUNG 02/24/15  SDP82202  DECK914346
*                                                            DECK914356
    CLEAR                                        lv_string.
    MOVE     ztax_validate1-txz01             TO lv_string.
    PERFORM  f_string_remove_spcl_char  CHANGING lv_string.
    CLEAR                                        ztax_validate1-txz01.
    MOVE     lv_string                        TO ztax_validate1-txz01.

    CLEAR                                        lv_string.
    MOVE     ztax_validate1-bktxt             TO lv_string.
    PERFORM  f_string_remove_spcl_char  CHANGING lv_string.
    CLEAR                                        ztax_validate1-bktxt.
    MOVE     lv_string                        TO ztax_validate1-bktxt.

    CLEAR                                        lv_string.
    MOVE     ztax_validate1-sgtxt_ven         TO lv_string.
    PERFORM  f_string_remove_spcl_char  CHANGING lv_string.
    CLEAR                         ztax_validate1-sgtxt_ven.
    MOVE     lv_string         TO ztax_validate1-sgtxt_ven.

    CLEAR                                        lv_string.
    MOVE     ztax_validate1-sgtxt             TO lv_string.
    PERFORM  f_string_remove_spcl_char  CHANGING lv_string.
    CLEAR                                        ztax_validate1-sgtxt.
    MOVE     lv_string                        TO ztax_validate1-sgtxt.
*                                                            DECK914356
* End changes   - insert code  JRHARTUNG 02/24/15  SDP82202  DECK914346

    IF NOT o_file IS INITIAL.
      CLEAR txt_out.
      DO.
        ASSIGN COMPONENT sy-index OF
               STRUCTURE ztax_validate1 TO <fs>.
        IF sy-subrc <> 0. EXIT. ENDIF.
        WRITE <fs> TO txt1 LEFT-JUSTIFIED.
        REPLACE ALL OCCURRENCES OF '"' IN txt1 WITH ' '.
        REPLACE ALL OCCURRENCES OF '|' IN txt1 WITH ' '.
        IF sy-index = 1.
          txt_out = txt1.
        ELSE.
*Begin of change by somsx
          IF sy-index NE 22 AND  "exclude GL recon amount and
             sy-index NE 23 AND
             sy-index NE 68 AND  "Tax rate PO
             sy-index NE 69 AND  "Calculated Tax Amount
             sy-index NE 70 AND  "Tax Difference

             sy-index NE 72 AND "ARCHIV_ID
             sy-index NE 73 AND "SAP_OBJECT
             sy-index NE 74 AND "OBJECT_ID
             sy-index NE 75 AND "AR_OBJECT
             sy-index NE 85 AND "Last GR quantity
             sy-index NE 86.    "GR Year
            IF recon IS INITIAL AND
               ( sy-index = 87 OR  "RecType
                 sy-index = 88 ).  "GLAMT
*End of change by somsx  : Numbers increased by 1
            ELSE.
              CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
      TRANSFER txt_out TO o_file.
      IF sy-subrc NE 0.
        WRITE: / 'Error writing document', ztax_validate1-belnr,
                 ztax_validate1-gjahr, 'to file', o_file.
        STOP.
      ENDIF.
    ELSE.
      APPEND ztax_validate1 TO alv_grid.
    ENDIF.
  ENDLOOP.

  if o_file is initial and sy-batch is initial.
    call screen 100.
  elseif o_file is initial and not sy-batch is initial.
    perform build_spool_list.
  else.
    write: / 'Output file', o_file, 'created'.
  endif.

ENDFORM.                    " output
*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.
  variant-report = sy-repid.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
    "$$
    CREATE OBJECT grid1
      EXPORTING
        i_parent = g_custom_container.
    "$$


    PERFORM alv_fieldcat CHANGING it_fieldcat.

*    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'                  "#EC *
*      EXPORTING                                             "#EC *
*        i_structure_name       = 'ZTAX_VALIDATE1'           "#EC *
*      CHANGING                                              "#EC *
*        ct_fieldcat            = it_fieldcat[]              "#EC *
*      EXCEPTIONS                                            "#EC *
*        inconsistent_interface = 1                          "#EC *
*        program_error          = 2                          "#EC *
*        OTHERS                 = 3.                         "#EC *
*    IF sy-subrc <> 0.
*    ENDIF.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        i_structure_name = 'ZTAX_VALIDATE1'
        is_variant       = variant
        i_save           = 'A'
        is_layout        = gs_layout
      CHANGING
        it_outtab        = alv_grid
        it_fieldcatalog  = it_fieldcat.
    IF sy-subrc <> 0.
    ENDIF.

    SET HANDLER lcl_event_receiver=>handle_user_command
                lcl_event_receiver=>handle_menu_button
                lcl_event_receiver=>handle_toolbar
            lcl_event_receiver=>respond_hotspot_click FOR ALL INSTANCES.
*
********

* raise event TOOLBAR:
    CALL METHOD grid1->set_toolbar_interactive.
    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        control = grid1.


  ENDIF.
ENDMODULE.                    "PBO OUTPUT
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE pai INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      CLEAR answer.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar      = 'Exit Program?'
          text_question = 'Are you sure you want to exit?'
        IMPORTING
          answer        = answer.
      IF answer = '1'.
        PERFORM exit_program.
      ENDIF.
    WHEN 'BACK'.
      CLEAR answer.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar      = 'Exit Program?'
          text_question = 'Are you sure you want to exit?'
        IMPORTING
          answer        = answer.
      IF answer = '1'.
        SET SCREEN 0.
        LEAVE SCREEN.
      ENDIF.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                    "PAI INPUT
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM exit_program.
*  CALL METHOD G_CUSTOM_CONTAINER->FREE.
*  CALL METHOD CL_GUI_CFW=>FLUSH.
  LEAVE PROGRAM.
ENDFORM.                    "EXIT_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  convert_regio
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_regio.

** Need to convert state to numeric Vertex format
  LOOP AT regio.
    CASE regio-low.
      WHEN 'AL'.
        regio-low = '01'.
      WHEN 'AK'.
        regio-low = '02'.
      WHEN 'AZ'.
        regio-low = '03'.
      WHEN 'AR'.
        regio-low = '04'.
      WHEN 'CA'.
        regio-low = '05'.
      WHEN 'CO'.
        regio-low = '06'.
      WHEN 'CT'.
        regio-low = '07'.
      WHEN 'DE'.
        regio-low = '08'.
      WHEN 'DC'.
        regio-low = '09'.
      WHEN 'FL'.
        regio-low = '10'.
      WHEN 'GA'.
        regio-low = '11'.
      WHEN 'HI'.
        regio-low = '12'.
      WHEN 'ID'.
        regio-low = '13'.
      WHEN 'IL'.
        regio-low = '14'.
      WHEN 'IN'.
        regio-low = '15'.
      WHEN 'IA'.
        regio-low = '16'.
      WHEN 'KS'.
        regio-low = '17'.
      WHEN 'KY'.
        regio-low = '18'.
      WHEN 'LA'.
        regio-low = '19'.
      WHEN 'ME'.
        regio-low = '20'.
      WHEN 'MD'.
        regio-low = '21'.
      WHEN 'MA'.
        regio-low = '22'.
      WHEN 'MI'.
        regio-low = '23'.
      WHEN 'MN'.
        regio-low = '24'.
      WHEN 'MS'.
        regio-low = '25'.
      WHEN 'MO'.
        regio-low = '26'.
      WHEN 'MT'.
        regio-low = '27'.
      WHEN 'NE'.
        regio-low = '28'.
      WHEN 'NV'.
        regio-low = '29'.
      WHEN 'NH'.
        regio-low = '30'.
      WHEN 'NJ'.
        regio-low = '31'.
      WHEN 'NM'.
        regio-low = '32'.
      WHEN 'NY'.
        regio-low = '33'.
      WHEN 'NC'.
        regio-low = '34'.
      WHEN 'ND'.
        regio-low = '35'.
      WHEN 'OH'.
        regio-low = '36'.
      WHEN 'OK'.
        regio-low = '37'.
      WHEN 'OR'.
        regio-low = '38'.
      WHEN 'PA'.
        regio-low = '39'.
      WHEN 'RI'.
        regio-low = '40'.
      WHEN 'SC'.
        regio-low = '41'.
      WHEN 'SD'.
        regio-low = '42'.
      WHEN 'TN'.
        regio-low = '43'.
      WHEN 'TX'.
        regio-low = '44'.
      WHEN 'UT'.
        regio-low = '45'.
      WHEN 'VT'.
        regio-low = '46'.
      WHEN 'VA'.
        regio-low = '47'.
      WHEN 'WA'.
        regio-low = '48'.
      WHEN 'WV'.
        regio-low = '49'.
      WHEN 'WI'.
        regio-low = '50'.
      WHEN 'WY'.
        regio-low = '51'.
      WHEN 'CN'.
        regio-low = '70'.
      WHEN 'PR'.
        regio-low = '72'.
      WHEN 'VI'.
        regio-low = '78'.
    ENDCASE.
    CASE regio-high.
      WHEN 'AL'.
        regio-high = '01'.
      WHEN 'AK'.
        regio-high = '02'.
      WHEN 'AZ'.
        regio-high = '03'.
      WHEN 'AR'.
        regio-high = '04'.
      WHEN 'CA'.
        regio-high = '05'.
      WHEN 'CO'.
        regio-high = '06'.
      WHEN 'CT'.
        regio-high = '07'.
      WHEN 'DE'.
        regio-high = '08'.
      WHEN 'DC'.
        regio-high = '09'.
      WHEN 'FL'.
        regio-high = '10'.
      WHEN 'GA'.
        regio-high = '11'.
      WHEN 'HI'.
        regio-high = '12'.
      WHEN 'ID'.
        regio-high = '13'.
      WHEN 'IL'.
        regio-high = '14'.
      WHEN 'IN'.
        regio-high = '15'.
      WHEN 'IA'.
        regio-high = '16'.
      WHEN 'KS'.
        regio-high = '17'.
      WHEN 'KY'.
        regio-high = '18'.
      WHEN 'LA'.
        regio-high = '19'.
      WHEN 'ME'.
        regio-high = '20'.
      WHEN 'MD'.
        regio-high = '21'.
      WHEN 'MA'.
        regio-high = '22'.
      WHEN 'MI'.
        regio-high = '23'.
      WHEN 'MN'.
        regio-high = '24'.
      WHEN 'MS'.
        regio-high = '25'.
      WHEN 'MO'.
        regio-high = '26'.
      WHEN 'MT'.
        regio-high = '27'.
      WHEN 'NE'.
        regio-high = '28'.
      WHEN 'NV'.
        regio-high = '29'.
      WHEN 'NH'.
        regio-high = '30'.
      WHEN 'NJ'.
        regio-high = '31'.
      WHEN 'NM'.
        regio-high = '32'.
      WHEN 'NY'.
        regio-high = '33'.
      WHEN 'NC'.
        regio-high = '34'.
      WHEN 'ND'.
        regio-high = '35'.
      WHEN 'OH'.
        regio-high = '36'.
      WHEN 'OK'.
        regio-high = '37'.
      WHEN 'OR'.
        regio-high = '38'.
      WHEN 'PA'.
        regio-high = '39'.
      WHEN 'RI'.
        regio-high = '40'.
      WHEN 'SC'.
        regio-high = '41'.
      WHEN 'SD'.
        regio-high = '42'.
      WHEN 'TN'.
        regio-high = '43'.
      WHEN 'TX'.
        regio-high = '44'.
      WHEN 'UT'.
        regio-high = '45'.
      WHEN 'VT'.
        regio-high = '46'.
      WHEN 'VA'.
        regio-high = '47'.
      WHEN 'WA'.
        regio-high = '48'.
      WHEN 'WV'.
        regio-high = '49'.
      WHEN 'WI'.
        regio-high = '50'.
      WHEN 'WY'.
        regio-high = '51'.
      WHEN 'CN'.
        regio-high = '70'.
      WHEN 'PR'.
        regio-high = '72'.
      WHEN 'VI'.
        regio-high = '78'.
    ENDCASE.
    MODIFY regio.
  ENDLOOP.

ENDFORM.                    " convert_regio
*&---------------------------------------------------------------------*
*&      Form  recalculate_tax
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recalculate_tax.

  DATA: menge_i TYPE i.
* G/L account master (company code)
  SELECT SINGLE mwskz INTO skb1-mwskz FROM skb1
         WHERE bukrs = item-bukrs AND
               saknr = item-saknr.

  IF NOT skb1-mwskz IS INITIAL AND
     NOT item-mwskz IN mwskz_nt.
* Vendor Master (General Section)
    SELECT SINGLE name1 txjcd regio
           INTO (item-name1, lfa1-txjcd, item-vend_regio)
           FROM lfa1
           WHERE lifnr = item-lifnr.

    REFRESH: i_tax_cal_item_in, o_tax_cal_item_out,
             o_tax_cal_jur_level_out.

* Tax Keys
    SELECT SINGLE txind procd
            INTO (t007a-txind, i_tax_cal_item_in-prod_code)
            FROM t007a
           WHERE kalsm = kalsm AND
                 mwskz = item-mwskz.

    i_sap_control_data-app_server = sy-host.
    i_sap_control_data-sap_version = sy-saprl.
    i_sap_control_data-interf_version = ttxd-intversion.

    i_tax_cal_head_in-syst_name = sy-sysid.
    i_tax_cal_head_in-client = sy-mandt.
    i_tax_cal_head_in-comp_code = item-bukrs.
    i_tax_cal_head_in-currency = item-waers.
    i_tax_cal_head_in-curr_dec = '002'.
    i_tax_cal_head_in-txjcd_l1 = ttxd-leng1.
    i_tax_cal_head_in-txjcd_l2 = ttxd-leng2.
    i_tax_cal_head_in-txjcd_l3 = ttxd-leng3.
    i_tax_cal_head_in-txjcd_l4 = ttxd-leng4.
    i_tax_cal_head_in-tax_per_item = ttxd-xtxit.
    i_tax_cal_head_in-nr_line_items = 1.
    i_tax_cal_item_in-item_no = 1.
    i_tax_cal_item_in-country = 'US'.

*    PERFORM determine_prod_code USING i_tax_cal_item_in-prod_code.   SJB

    menge_i = item-menge.

    PERFORM sub_convert_num USING menge_i
                            CHANGING i_tax_cal_item_in-quantity.

    i_tax_cal_item_in-unit = item-bstme.
    i_tax_cal_item_in-apar_ind =  'V'.
    i_tax_cal_item_in-tax_type = t007a-txind.
    i_tax_cal_item_in-tax_date = item-budat.
    i_tax_cal_item_in-txjcd_st = item-txjcd.
    i_tax_cal_item_in-txjcd_sf = lfa1-txjcd.
    i_tax_cal_item_in-txjcd_poa = lfa1-txjcd.
    i_tax_cal_item_in-txjcd_poo = item-txjcd.
    IF i_tax_cal_item_in-txjcd_sf IS INITIAL.
      i_tax_cal_item_in-txjcd_sf = item-txjcd.
    ENDIF.
    IF i_tax_cal_item_in-txjcd_poa IS INITIAL.
      i_tax_cal_item_in-txjcd_poa = item-txjcd.
    ENDIF.

    PERFORM sub_convert_num USING item-wrbtr
                            CHANGING i_tax_cal_item_in-amount.

    i_tax_cal_item_in-gross_amount = i_tax_cal_item_in-amount.
    APPEND i_tax_cal_item_in.

    CALL FUNCTION 'RFC_CALCULATE_TAXES_DOC'                 "#EC *
      DESTINATION ttxd-rfcdest                              "#EC *
      EXPORTING                                             "#EC *
        i_sap_control_data      = i_sap_control_data        "#EC *
        i_tax_cal_head_in       = i_tax_cal_head_in         "#EC *
      IMPORTING                                             "#EC *
        o_ext_control_data      = o_ext_control_data        "#EC *
        o_com_err_doc           = o_com_err_doc             "#EC *
      TABLES                                                "#EC *
        i_tax_cal_item_in       = i_tax_cal_item_in         "#EC *
        o_tax_cal_item_out      = o_tax_cal_item_out        "#EC *
        o_tax_cal_jur_level_out = o_tax_cal_jur_level_out.  "#EC *

    IF sy-subrc = 0 AND o_com_err_doc-retcode = 0.          "#EC *
      READ TABLE o_tax_cal_item_out INDEX 1.                "#EC *
      item-cal_tax = o_tax_cal_item_out-taxamov.            "#EC *
      item-cal_tax = item-cal_tax / 100.                    "#EC *
      IF item-shkzg = 'H'.                                  "#EC *
        item-cal_tax = item-cal_tax * -1.                   "#EC *
      ENDIF.                                                "#EC *
    ENDIF.                                                  "#EC *
  ENDIF.                                                    "#EC *
  item-tax_dif = ABS( item-act_tax ) - ABS( item-cal_tax ). "#EC *

**On credits, the tax difference sign changes
  IF item-shkzg = 'H'.
    item-tax_dif = item-tax_dif * -1.
  ENDIF.


ENDFORM.                    " recalculate_tax

*&---------------------------------------------------------------------*
*&      Form  SUB_CONVERT_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_AUDIT_GROSSAMT  text
*      <--P_COM_TAX_AMOUNT  text
*----------------------------------------------------------------------*
FORM sub_convert_num USING    p_t_audit_grossamt TYPE any
                     CHANGING p_com_tax_amount TYPE any.

  p_com_tax_amount = p_t_audit_grossamt.

  REPLACE ALL OCCURRENCES OF ',' IN p_com_tax_amount WITH ' '.   " SJB
  REPLACE ALL OCCURRENCES OF '.' IN p_com_tax_amount WITH ' '.   " SJB
  CONDENSE p_com_tax_amount NO-GAPS.

  IF p_com_tax_amount(3) = '000' OR
     p_com_tax_amount = space.
    p_com_tax_amount(3) = '0   '.
  ENDIF.

ENDFORM.                    " SUB_CONVERT_NUM
*&---------------------------------------------------------------------*
*&      Form  determine_net
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INV_NET  text
*----------------------------------------------------------------------*
FORM determine_net USING p_inv_net TYPE any.

  DATA: temp LIKE rbco-wrbtr.
* Document Item, Incoming Invoice, Account Assignment
  SELECT wrbtr INTO temp FROM rbco
         WHERE belnr = item-belnri AND
               gjahr = item-gjahr AND
               NOT mwskz IN mwskz_nt.
    p_inv_net = p_inv_net + temp.
  ENDSELECT.
  IF sy-subrc NE 0.
* Document Item: Incoming Invoice
    SELECT wrbtr INTO temp FROM rseg
           WHERE belnr = item-belnri AND
                 gjahr = item-gjahr AND
                 NOT mwskz IN mwskz_nt.
      p_inv_net = p_inv_net + temp.
    ENDSELECT.
  ENDIF.
  p_inv_net = p_inv_net + item-beznk.


ENDFORM.                    " determine_net

*---------------------------------------------------------------------*
*       FORM INSERT_ROW_IN_BDC_TAB                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  BDC                                                           *
*  -->  BEGIN                                                         *
*  -->  NAME                                                          *
*  -->  VALUE                                                         *
*---------------------------------------------------------------------*
FORM insert_row_in_bdc_tab
            TABLES bdc                                      "#EC *
            STRUCTURE bdcdata                               "#EC *
                USING begin name value TYPE any.
  CLEAR bdc.
  IF begin = 'X'.
    MOVE: name  TO bdc-program,
          value TO bdc-dynpro,
          'X'   TO bdc-dynbegin.
  ELSE.
    MOVE: name  TO bdc-fnam,
          value TO bdc-fval.
  ENDIF.
  APPEND bdc.
ENDFORM.                    "INSERT_ROW_IN_BDC_TAB
*&---------------------------------------------------------------------*
*&      Form  convert_txjcd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_txjcd.
** Taxware
  IF ttxd-xextn = 'T' OR
     ttxd-xextn = 'A'.
    ztax_validate1-regio = ztax_validate1-txjcd(2).
  ELSEIF ttxd-xextn = 'V'.
** Vertex
    CASE ztax_validate1-txjcd(2).
      WHEN '01'.
        ztax_validate1-regio = 'AL'.
      WHEN '02'.
        ztax_validate1-regio = 'AK'.
      WHEN '03'.
        ztax_validate1-regio = 'AZ'.
      WHEN '04'.
        ztax_validate1-regio = 'AR'.
      WHEN '05'.
        ztax_validate1-regio = 'CA'.
      WHEN '06'.
        ztax_validate1-regio = 'CO'.
      WHEN '07'.
        ztax_validate1-regio = 'CT'.
      WHEN '08'.
        ztax_validate1-regio = 'DE'.
      WHEN '09'.
        ztax_validate1-regio = 'DC'.
      WHEN '10'.
        ztax_validate1-regio = 'FL'.
      WHEN '11'.
        ztax_validate1-regio = 'GA'.
      WHEN '12'.
        ztax_validate1-regio = 'HI'.
      WHEN '13'.
        ztax_validate1-regio = 'ID'.
      WHEN '14'.
        ztax_validate1-regio = 'IL'.
      WHEN '15'.
        ztax_validate1-regio = 'IN'.
      WHEN '16'.
        ztax_validate1-regio = 'IA'.
      WHEN '17'.
        ztax_validate1-regio = 'KS'.
      WHEN '18'.
        ztax_validate1-regio = 'KY'.
      WHEN '19'.
        ztax_validate1-regio = 'LA'.
      WHEN '20'.
        ztax_validate1-regio = 'ME'.
      WHEN '21'.
        ztax_validate1-regio = 'MD'.
      WHEN '22'.
        ztax_validate1-regio = 'MA'.
      WHEN '23'.
        ztax_validate1-regio = 'MI'.
      WHEN '24'.
        ztax_validate1-regio = 'MN'.
      WHEN '25'.
        ztax_validate1-regio = 'MS'.
      WHEN '26'.
        ztax_validate1-regio = 'MO'.
      WHEN '27'.
        ztax_validate1-regio = 'MT'.
      WHEN '28'.
        ztax_validate1-regio = 'NE'.
      WHEN '29'.
        ztax_validate1-regio = 'NV'.
      WHEN '30'.
        ztax_validate1-regio = 'NH'.
      WHEN '31'.
        ztax_validate1-regio = 'NJ'.
      WHEN '32'.
        ztax_validate1-regio = 'NM'.
      WHEN '33'.
        ztax_validate1-regio = 'NY'.
      WHEN '34'.
        ztax_validate1-regio = 'NC'.
      WHEN '35'.
        ztax_validate1-regio = 'ND'.
      WHEN '36'.
        ztax_validate1-regio = 'OH'.
      WHEN '37'.
        ztax_validate1-regio = 'OK'.
      WHEN '38'.
        ztax_validate1-regio = 'OR'.
      WHEN '39'.
        ztax_validate1-regio = 'PA'.
      WHEN '40'.
        ztax_validate1-regio = 'RI'.
      WHEN '41'.
        ztax_validate1-regio = 'SC'.
      WHEN '42'.
        ztax_validate1-regio = 'SD'.
      WHEN '43'.
        ztax_validate1-regio = 'TN'.
      WHEN '44'.
        ztax_validate1-regio = 'TX'.
      WHEN '45'.
        ztax_validate1-regio = 'UT'.
      WHEN '46'.
        ztax_validate1-regio = 'VT'.
      WHEN '47'.
        ztax_validate1-regio = 'VA'.
      WHEN '48'.
        ztax_validate1-regio = 'WA'.
      WHEN '49'.
        ztax_validate1-regio = 'WV'.
      WHEN '50'.
        ztax_validate1-regio = 'WI'.
      WHEN '51'.
        ztax_validate1-regio = 'WY'.
      WHEN '70'.
        CASE ztax_validate1-txjcd+2(3).
          WHEN '001'.
            ztax_validate1-regio = 'AB'.
          WHEN '003'.
            ztax_validate1-regio = 'BC'.
          WHEN '005'.
            ztax_validate1-regio = 'MB'.
          WHEN '007'.
            ztax_validate1-regio = 'NB'.
          WHEN '009'.
            ztax_validate1-regio = 'NF'.
          WHEN '011'.
            ztax_validate1-regio = 'NT'.
          WHEN '013'.
            ztax_validate1-regio = 'NS'.
          WHEN '015'.
            ztax_validate1-regio = 'ON'.
          WHEN '017'.
            ztax_validate1-regio = 'PE'.
          WHEN '019'.
            ztax_validate1-regio = 'QC'.
          WHEN '021'.
            ztax_validate1-regio = 'SK'.
          WHEN '023'.
            ztax_validate1-regio = 'YT'.
          WHEN '025'.
            ztax_validate1-regio = 'NU'.
        ENDCASE.
      WHEN '72'.
        ztax_validate1-regio = 'PR'.
      WHEN '78'.
        ztax_validate1-regio = 'VI'.
    ENDCASE.
  ELSEIF ttxd-xextn = space.
** SAP internal / adjaust for each client.
    CASE ztax_validate1-txjcd(2).
      WHEN '01'.
        ztax_validate1-regio = 'AL'.
      WHEN '02'.
        ztax_validate1-regio = 'AK'.
      WHEN '04'.
        ztax_validate1-regio = 'AZ'.
      WHEN '05'.
        ztax_validate1-regio = 'AR'.
      WHEN '06'.
        ztax_validate1-regio = 'CA'.
      WHEN '08'.
        ztax_validate1-regio = 'CO'.
      WHEN '09'.
        ztax_validate1-regio = 'CT'.
      WHEN '10'.
        ztax_validate1-regio = 'DE'.
*      WHEN '09'.
*        ztax_validate1-regio = 'DC'.
      WHEN '12'.
        ztax_validate1-regio = 'FL'.
      WHEN '13'.
        ztax_validate1-regio = 'GA'.
      WHEN '15'.
        ztax_validate1-regio = 'HI'.
      WHEN '16'.
        ztax_validate1-regio = 'ID'.
      WHEN '17'.
        ztax_validate1-regio = 'IL'.
      WHEN '18'.
        ztax_validate1-regio = 'IN'.
      WHEN '19'.
        ztax_validate1-regio = 'IA'.
      WHEN '20'.
        ztax_validate1-regio = 'KS'.
      WHEN '21'.
        ztax_validate1-regio = 'KY'.
      WHEN '22'.
        ztax_validate1-regio = 'LA'.
      WHEN '23'.
        ztax_validate1-regio = 'ME'.
      WHEN '24'.
        ztax_validate1-regio = 'MD'.
      WHEN '25'.
        ztax_validate1-regio = 'MA'.
      WHEN '26'.
        ztax_validate1-regio = 'MI'.
      WHEN '27'.
        ztax_validate1-regio = 'MN'.
      WHEN '28'.
        ztax_validate1-regio = 'MS'.
      WHEN '29'.
        ztax_validate1-regio = 'MO'.
      WHEN '30'.
        ztax_validate1-regio = 'MT'.
      WHEN '31'.
        ztax_validate1-regio = 'NE'.
      WHEN '32'.
        ztax_validate1-regio = 'NV'.
      WHEN '33'.
        ztax_validate1-regio = 'NH'.
      WHEN '34'.
        ztax_validate1-regio = 'NJ'.
      WHEN '35'.
        ztax_validate1-regio = 'NM'.
      WHEN '36'.
        ztax_validate1-regio = 'NY'.
      WHEN '37'.
        ztax_validate1-regio = 'NC'.
      WHEN '37'.
        ztax_validate1-regio = 'ND'.
      WHEN '39'.
        ztax_validate1-regio = 'OH'.
      WHEN '40'.
        ztax_validate1-regio = 'OK'.
      WHEN '41'.
        ztax_validate1-regio = 'OR'.
      WHEN '42'.
        ztax_validate1-regio = 'PA'.
      WHEN '44'.
        ztax_validate1-regio = 'RI'.
      WHEN '45'.
        ztax_validate1-regio = 'SC'.
      WHEN '46'.
        ztax_validate1-regio = 'SD'.
      WHEN '47'.
        ztax_validate1-regio = 'TN'.
      WHEN '48'.
        ztax_validate1-regio = 'TX'.
      WHEN '49'.
        ztax_validate1-regio = 'UT'.
      WHEN '50'.
        ztax_validate1-regio = 'VT'.
      WHEN '51'.
        ztax_validate1-regio = 'VA'.
      WHEN '53'.
        ztax_validate1-regio = 'WA'.
      WHEN '54'.
        ztax_validate1-regio = 'WV'.
      WHEN '55'.
        ztax_validate1-regio = 'WI'.
      WHEN '56'.
        ztax_validate1-regio = 'WY'.
      WHEN OTHERS.
        ztax_validate1-regio = ztax_validate1-txjcd(2).
    ENDCASE.
  ELSEIF ttxd-xextn = 'S'.
** Sabrix
    ztax_validate1-regio = ztax_validate1-txjcd+2(2).
*  ELSE.
*** SAP internal tax processing
*    IF ztax_validate1-txjcd(2) = 'CN'.
*      ztax_validate1-regio = ztax_validate1-txjcd+2(2).
*    ELSE.
*      ztax_validate1-regio = ztax_validate1-txjcd(2).
*    ENDIF.
  ENDIF.

ENDFORM.                    " convert_txjcd
*&---------------------------------------------------------------------*
*&      Form  calc_tax_percent
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_tax_percent.
  DATA: temp LIKE bseg-wrbtr.

  CLEAR temp.
  IF ( ztax_validate1-wrbtr NE 0 AND
       ztax_validate1-act_tax NE 0 ) AND
     ( ABS( ztax_validate1-wrbtr ) GT ABS( ztax_validate1-act_tax ) ).
    temp = 100 / ABS( ztax_validate1-wrbtr ) * ABS( ztax_validate1-act_tax ).
    IF temp < 100.
      ztax_validate1-act_tax_rate = temp.
    ENDIF.
  ELSE.
    ztax_validate1-act_tax_rate = 0.
  ENDIF.

ENDFORM.                    " calc_tax_percent
*&---------------------------------------------------------------------*
*&      Form  get_gltxt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gltxt.

  IF ktopl IS INITIAL.

* Company Codes
    SELECT SINGLE ktopl INTO ktopl
      FROM t001
      WHERE bukrs = ztax_validate1-bukrs.
  ENDIF.

* G/L Account Master Record (Chart of Accounts: Description)
  SELECT SINGLE txt20 INTO ztax_validate1-txt20
    FROM skat
    WHERE spras = sy-langu AND
          ktopl = ktopl AND
          saknr = ztax_validate1-saknr.

ENDFORM.                    " get_gltxt
*&---------------------------------------------------------------------*
*&      Form  additional_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM additional_fields .
  DATA: street LIKE adrc-street,
        city LIKE adrc-city1,
        regio LIKE adrc-region,
        post_code1 LIKE adrc-post_code1,
        adrnr LIKE adrc-addrnumber,
        date_from LIKE sy-datum,
        ir_date LIKE sy-datum,                              "#EC *
        gr_wrbtr LIKE ekbe-wrbtr,                           "#EC *
        grir_wrbtr LIKE ekbe-arewr,                         "#EC *
        variance LIKE ekbe-arewr,
        gr_found,                                           "#EC *
        gr_processed,                                       "#EC *
        ir_menge LIKE ekbe-menge,                           "#EC *
        gr_menge LIKE ekbe-menge,                           "#EC *
        belnr LIKE bseg-belnr,
        gjahr LIKE bseg-gjahr,
        awkey LIKE bkpf-awkey,
        bukrs LIKE bkpf-bukrs,
        bwmod LIKE t001k-bwmod,
        bklas LIKE mbew-bklas,
        ktopl LIKE t001-ktopl,
        adrnr_so LIKE vbpa-adrnr.

  DATA: zbd1t(3), zbd2t(3), zbd3t(3), zbd1p(6), zbd2p(6).
  DATA: faede LIKE faede.


**Change sign of posted tax amount
  IF item-act_tax_1 < 0 AND item-act_tax > 0.
    item-act_tax = item-act_tax * -1.
  ENDIF.

** Goods Receipt Date
  IF NOT item-ebeln IS INITIAL.
    REFRESH ekbe_tab.
    CLEAR: gr_wrbtr, ir_date, grir_wrbtr, variance, gr_found.

* EKBE - History per Purchasing Document
* EKKO - Purchasing Document Header
    SELECT * FROM ekbe INTO CORRESPONDING FIELDS OF TABLE ekbe_tab
           UP TO 1 ROWS
           WHERE ebeln = item-ebeln AND
                 ebelp = item-ebelp AND
                 vgabe = '1'. "Goods Receipt
*           order by budat descending.
*    sort ekbe_tab by budat descending.
    IF sy-subrc NE 0.
      REFRESH: it_ekko_a, it_ekbe_a.
      PERFORM arch_ekko_data TABLES  it_ekko_a
                                     it_ekpo_a
                                     it_ekkn_a
                                     it_ekbe_a
                               USING item-ebeln
                                     item-ebelp.
      LOOP AT it_ekbe_a WHERE ebelp = item-ebelp AND
                              vgabe = '1'.
        MOVE-CORRESPONDING it_ekbe_a TO ekbe_tab.
        APPEND ekbe_tab.
        EXIT.
      ENDLOOP.
    ENDIF.


    LOOP AT ekbe_tab.
      IF ekbe_tab-shkzg = 'H'.
        ekbe_tab-wrbtr = ekbe_tab-wrbtr * -1.
        ekbe_tab-arewr = ekbe_tab-arewr * -1.
        ekbe_tab-menge = ekbe_tab-menge * -1.
      ENDIF.
      SELECT SINGLE bukrs INTO ekbe_tab-bukrs
             FROM ekko
             WHERE ebeln = ekbe_tab-ebeln.
      IF sy-subrc NE 0.
        READ TABLE it_ekko_a INDEX 1.
        IF sy-subrc = 0.
          ekbe_tab-bukrs = it_ekko_a-bukrs.
        ENDIF.
      ENDIF.
      MODIFY ekbe_tab.
    ENDLOOP.

**test
*    SELECT a~shkzg
*           a~wrbtr
*           a~arewr
*           a~menge
*           a~budat
*           a~belnr
*           a~gjahr
*           a~matnr
*           b~bukrs
*      INTO (ekbe_tab-shkzg,
*            ekbe_tab-wrbtr,
*            ekbe_tab-arewr,
*            ekbe_tab-menge,
*            ekbe_tab-budat,
*            ekbe_tab-belnr,
*            ekbe_tab-gjahr,
*            ekbe_tab-matnr,
*            ekbe_tab-bukrs)
*       FROM ( ekbe AS a JOIN ekko AS b             " SJB
*         ON   a~mandt = b~mandt AND
*              a~ebeln = b~ebeln )
*       up to 1 rows
*       WHERE a~ebeln = item-ebeln AND
*             a~ebelp = item-ebelp AND
*             b~ebeln = item-ebeln AND
*             a~vgabe = '1' "Goods Receipt
*       order by a~budat descending.
*
*      IF ekbe_tab-shkzg = 'H'.
*        ekbe_tab-wrbtr = ekbe_tab-wrbtr * -1.
*        ekbe_tab-arewr = ekbe_tab-arewr * -1.
*        ekbe_tab-menge = ekbe_tab-menge * -1.
*      ENDIF.
*
*      APPEND ekbe_tab.
*    ENDSELECT.
**test

    IF sy-subrc = 0.
*      SORT ekbe_tab BY budat DESCENDING.
      LOOP AT ekbe_tab.
        item-grdat = ekbe_tab-budat.
        item-grdoc = ekbe_tab-belnr.
        item-grjah = ekbe_tab-gjahr.
        item-grmng = ekbe_tab-menge.
        IF item-saknr IS INITIAL OR
           item-saknr IN grir.  "inventory purchase
          CLEAR: bwmod, bklas, ktopl.
          SELECT SINGLE bwmod INTO bwmod FROM t001k
                 WHERE bwkey = item-werks.
          SELECT SINGLE bklas INTO bklas FROM mbew
                 WHERE matnr = item-matnr AND
                       bwkey = item-werks.
          SELECT SINGLE ktopl INTO ktopl FROM t001
                 WHERE bukrs = item-bukrs.
          SELECT SINGLE konts INTO item-saknr FROM t030
                 WHERE ktopl = ktopl AND
                       ktosl = 'BSX' AND "inventory account
                       bwmod = bwmod AND
                       bklas = bklas.
*          CLEAR awkey.
*          CONCATENATE ekbe_tab-belnr ekbe_tab-gjahr INTO awkey.
*
** Accounting Document Header
*          SELECT SINGLE bukrs belnr gjahr INTO (bukrs, belnr, gjahr)"#EC *
*                 FROM bkpf                                  "#EC *
*                 WHERE awtyp = 'MKPF' AND                   "#EC *
*                       awkey = awkey AND                    "#EC *
*                       bukrs = ekbe_tab-bukrs AND"#EC * " added - performance SJB
*                       blart LIKE 'W%'.                     "#EC *
*
*          IF sy-subrc = 0.
** Accounting Document Segment
*            SELECT SINGLE hkont INTO item-saknr             "#EC *
*                   FROM bseg                                "#EC *
*                   WHERE bukrs = bukrs AND                  "#EC *
*                         belnr = belnr AND                  "#EC *
*                         gjahr = gjahr AND                  "#EC *
*                         buzid NE 'W' AND                   "#EC *
*                         ( matnr = ekbe_tab-matnr OR        "#EC *
*                           ( ebeln = item-ebeln AND         "#EC *
*                             ebelp = item-ebelp ) ) AND     "#EC *
*                         ( bschl = '86' OR                  "#EC *
*                           bschl = '96' ).                  "#EC *
*            IF sy-subrc NE 0.
** Accounting Document Segment
*              SELECT SINGLE hkont INTO item-saknr           "#EC *
*                     FROM bseg                              "#EC *
*                     WHERE bukrs = bukrs AND                "#EC *
*                           belnr = belnr AND                "#EC *
*                           gjahr = gjahr AND                "#EC *
*                           buzid = 'M' AND                  "#EC *
*                           ( matnr = ekbe_tab-matnr OR      "#EC *
*                             ( ebeln = item-ebeln AND       "#EC *
*                               ebelp = item-ebelp ) ).      "#EC *
*              IF sy-subrc NE 0.
** Accounting Document Segment
*                SELECT SINGLE hkont INTO item-saknr         "#EC *
*                       FROM bseg                            "#EC *
*                       WHERE bukrs = bukrs AND              "#EC *
*                             belnr = belnr AND              "#EC *
*                             gjahr = gjahr AND              "#EC *
*                             buzid = 'P' AND                "#EC *
*                             ( matnr = ekbe_tab-matnr OR    "#EC *
*                               ( ebeln = item-ebeln AND     "#EC *
*                                 ebelp = item-ebelp ) ).    "#EC *
*              ENDIF.
*            ENDIF.
*          ENDIF.
        ENDIF.
        EXIT.
      ENDLOOP.

    ELSE. "goods receipt does not exist - get account from IR
      CLEAR item-grdat.
      IF item-saknr IS INITIAL OR
         item-saknr IN grir.  "inventory purchase

** Accounting Document Segment
*        SELECT SINGLE hkont INTO item-saknr                 "#EC *
*               FROM bseg                                    "#EC *
*               WHERE bukrs = item-bukrs AND                 "#EC *
*                     belnr = item-belnr AND                 "#EC *
*                     gjahr = item-gjahr AND                 "#EC *
*                     buzid = 'W' AND                        "#EC *
*                     ( matnr = ekbe_tab-matnr OR            "#EC *
*                       ( ebeln = item-ebeln AND             "#EC *
*                         ebelp = item-ebelp ) ).            "#EC *
** Get inventory account via account assignment table t030
        CLEAR: bwmod, bklas, ktopl.
        SELECT SINGLE bwmod INTO bwmod FROM t001k
               WHERE bwkey = item-werks.
        SELECT SINGLE bklas INTO bklas FROM mbew
               WHERE matnr = item-matnr AND
                     bwkey = item-werks.
        SELECT SINGLE ktopl INTO ktopl FROM t001
               WHERE bukrs = item-bukrs.
        SELECT SINGLE konts INTO item-saknr FROM t030
               WHERE ktopl = ktopl AND
                     ktosl = 'BSX' AND "inventory account
                     bwmod = bwmod AND
                     bklas = bklas.
      ENDIF.
    ENDIF.
  ENDIF.

  item-glbtr = item-wrbtr + item-act_tax - item-irout + variance.
** Vendor name and vendor state - Vendor Master (General Section)
  IF item-name1 IS INITIAL OR item-vend_regio IS INITIAL.
    SELECT SINGLE name1 regio INTO (item-name1, item-vend_regio)
           FROM lfa1
           WHERE lifnr = item-lifnr.
  ENDIF.
** Ship-to address
  CLEAR: adrnr, street,  city, regio, post_code1.
  IF NOT item-adrn2 IS INITIAL.    " added SJB 05/03/10
    adrnr = item-adrn2.
  ELSEIF NOT item-adrnr IS INITIAL.
    adrnr = item-adrnr.
  ELSE.

* Determination of Address from Plant and Storage Location
    SELECT SINGLE adrnr
               FROM twlad
               INTO adrnr
               WHERE werks = item-werks AND
                     lgort = item-lgort AND
                     lfdnr = '1'.
    IF adrnr IS INITIAL.

* Plants/Branches
      SELECT SINGLE adrnr name1
                 FROM t001w
                 INTO (adrnr, item-name1_w)
                 WHERE werks = item-werks.
    ENDIF.
  ENDIF.

* Addresses (Business Address Services)
  SELECT street city1 region post_code1 date_from
             FROM adrc
             INTO (street, city, regio, post_code1, date_from)
             WHERE addrnumber = adrnr
             ORDER BY date_from DESCENDING.
    EXIT.
  ENDSELECT.
  CONCATENATE street ',' city ',' regio ',' post_code1 INTO item-adrst.
  IF item-adrst = ',,,'.
    CLEAR item-adrst.
  ENDIF.

**Plant Name
  IF item-name1_w IS INITIAL AND
     NOT item-werks IS INITIAL.
    SELECT SINGLE name1 INTO item-name1_w
           FROM t001w
           WHERE werks = item-werks.
  ENDIF.



** Cost Center Category
  PERFORM determine_kosar
             USING item-budat
                   item-bukrs
                   item-kostl
                   item-kokrs
                   item-aufnr
                   item-nplnr
                   item-anln1
                   item-anln2
                   item-ps_psp_pnr
          CHANGING item-kosar
                   item-kostl.

* Cost center type texts
  SELECT SINGLE ktext INTO item-kosar_txt FROM tkt05
         WHERE spras = sy-langu AND
               kosar = item-kosar.

** Cost Center Name
  SELECT ktext datbi INTO (item-ktext, date_from) FROM cskt
            WHERE spras = sy-langu AND
                  kokrs = item-kokrs AND
                  kostl = item-kostl
            ORDER BY datbi DESCENDING.
    EXIT.
  ENDSELECT.

** Order Type and Order description
  IF NOT item-aufnr IS INITIAL.
    SELECT SINGLE auart ktext INTO (item-auart, item-ktext_aufk)
           FROM aufk
           WHERE aufnr = item-aufnr.
  ENDIF.
  IF NOT item-nplnr IS INITIAL.
    SELECT SINGLE auart ktext INTO (item-auart, item-ktext_aufk)
           FROM aufk
           WHERE aufnr = item-nplnr.
  ENDIF.

** Sales Order jurisdiction code
  IF NOT item-vbeln IS INITIAL AND
     NOT item-vbelp IS INITIAL.
    CLEAR adrnr_so.
    SELECT SINGLE adrnr INTO adrnr_so FROM vbpa
           WHERE vbeln = item-vbeln AND
                 posnr = item-vbelp AND
                 parvw = 'WE'.
    IF sy-subrc NE 0.
      SELECT SINGLE adrnr INTO adrnr_so FROM vbpa
             WHERE vbeln = item-vbeln AND
                   posnr = '000000' AND
                   parvw = 'WE'.
    ENDIF.
    IF NOT adrnr_so IS INITIAL.
      SELECT SINGLE taxjurcode INTO item-txjcd_so
             FROM adrc
             WHERE addrnumber = adrnr_so.
    ENDIF.
  ENDIF.

** Optical Archive Detail

*  DATA: object_id LIKE toa01-object_id.
*  DATA: sap_object LIKE toa01-sap_object.
*  CLEAR: object_id, sap_object.
*  IF NOT item-belnri IS INITIAL.
*    sap_object = 'BUS2081'.
*    CONCATENATE item-belnri item-gjahr INTO object_id.
*  ELSE.
*    sap_object = 'BKPF'.
*    CONCATENATE item-bukrs item-belnr item-gjahr INTO object_id.
*  ENDIF.
*  SELECT SINGLE arc_doc_id archiv_id sap_object object_id ar_object
*         INTO (item-arc_doc_id, item-archiv_id, item-sap_object,
*               item-object_id, item-ar_object)
*         FROM toa01
*         WHERE sap_object = sap_object AND
*               object_id = object_id.

** Callaway archive system
  CONCATENATE item-bukrs item-belnr item-gjahr item-buzei INTO item-arc_doc_id.

** Additional FI line item detail from vendor line
  SELECT SINGLE sgtxt augdt augbl auggj
         INTO (item-sgtxt_ven, item-augdt, item-augbl, item-auggj)
         FROM bseg
         WHERE bukrs = item-bukrs AND
               belnr = item-belnr AND
               gjahr = item-gjahr AND
               koart = 'K'.

** Check number
  SELECT SINGLE chect INTO item-chect
         FROM payr
         WHERE zbukr = item-bukrs AND
               vblnr = item-augbl AND
               gjahr = item-gjahr.

** Additional FI line item prctr from expense/inventory item
  SELECT SINGLE prctr sgtxt vbund  "somsx added vbund
         INTO (item-prctr, item-sgtxt, item-vbund)
         FROM bseg
         WHERE bukrs = item-bukrs AND
               belnr = item-belnr AND
               gjahr = item-gjahr AND
               buzei = item-buzei.
*Begin of change by somsx
  if item-vbund is initial.
    item-vbund = item-bukrs.
  endif.
*End of change by somsx
  IF item-prctr IS INITIAL.
    SELECT prctr INTO item-prctr
           FROM bseg
           WHERE bukrs = item-bukrs AND
                 belnr = item-belnr AND
                 gjahr = item-gjahr AND NOT
                 prctr = space.
      EXIT.
    ENDSELECT.

  ENDIF.

** Currency conversion factor
  DATA: gdatu LIKE tcurf-gdatu,
        budat_inv LIKE tcurf-gdatu,
        ext_date(10),
        t001_waers LIKE t001-waers.
  WRITE item-budat TO ext_date.

  SELECT SINGLE waers INTO t001_waers FROM t001 WHERE bukrs = item-bukrs.

* invert date for tcurf - Convert date to the internal format(GDATU) accepted by currency table
  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = ext_date
    IMPORTING
      output = budat_inv.

* Conversion Factors - Get currency conversion factors from the table tcurf.
  SELECT ffact gdatu INTO (item-ffact, gdatu)
         FROM tcurf
         WHERE kurst = 'M' AND
               fcurr = item-waers AND
               tcurr = t001_waers AND
               gdatu GE budat_inv                         "#EC PORTABLE
      ORDER BY gdatu ASCENDING.
    EXIT.
  ENDSELECT.
  IF item-kursf IS INITIAL OR sy-subrc NE 0.
    item-kursf = 1.
    item-ffact = 1.
  ENDIF.


** Material Group Name
  SELECT SINGLE wgbez INTO item-wgbez FROM t023t
            WHERE spras = sy-langu AND
                  matkl = item-matkl.
** Special treatment for manual tax accruals
*  if item-saknr = mantax.
*    item-act_tax = item-wrbtr.
*    if ( item-rmwwr < 0 and item-act_tax > 0 ) or
*       ( item-rmwwr > 0 and item-act_tax < 0 ).
*     item-act_tax = item-act_tax * -1.
*     item-wrbtr = item-wrbtr * -1.
*    endif.
*    clear: item-act_tax_1, item-act_tax_2, item-act_tax_3,
*           item-act_tax_4.
*  endif.
** Payment Term Text and Net Due Date

  CLEAR faede.
*  select single shkzg into faede-shkzg from bseg
*            where bukrs = item-bukrs and
*                  belnr = item-belnr and
*                  gjahr = item-gjahr and
*                  koart = 'K'.
  faede-shkzg = item-faede_shkzg.
  faede-koart = 'K'.
  faede-zfbdt = item-zfbdt.
  faede-zbd1t = item-zbd1t.
  faede-zbd2t = item-zbd2t.
  faede-zbd3t = item-zbd3t.
  faede-rebzg = item-rebzg.

* Calculate the Due Date of the Invoice.
  CALL FUNCTION 'DETERMINE_DUE_DATE'                        "#EC *
    EXPORTING                                               "#EC *
      i_faede = faede                                       "#EC *
    IMPORTING                                               "#EC *
      e_faede = faede                                       "#EC *
    EXCEPTIONS                                              "#EC *
      OTHERS  = 1.                                          "#EC *
  item-netdt = faede-netdt.

  CLEAR item-zterm_text.
  zbd1t = item-zbd1t.
  zbd2t = item-zbd2t.
  zbd3t = item-zbd3t.
  zbd1p = item-zbd1p.
  zbd2p = item-zbd2p.

  PERFORM delete_zeros USING zbd1p.
  PERFORM delete_zeros USING zbd2p.

*-------------- Baseline Date = Due Date -------------------------------
  IF item-zbd1t = 0 AND item-zbd1p = 0.
    IF item-zfbdt > sy-datum.                               "Note398569
      item-zterm_text = 'Due on Base Date'.                 "Note398569
    ELSE.
      item-zterm_text = 'Due immediately'.
    ENDIF.                                                  "Note398569
    EXIT.
  ENDIF.

*------------- Only ZBD1T ----------------------------------------------
  IF item-zbd1p = 0.
    CONCATENATE zbd1t 'Days net' INTO item-zterm_text
                SEPARATED BY space.
    EXIT.
  ENDIF.

*------------- ZBD1T ZBD1P ZBD2T ---------------------------------------
  IF item-zbd2p = 0.
    IF item-zbd2t NE 0.
      CONCATENATE zbd1t 'Days' zbd1p '%,'
                  zbd2t 'Days net' INTO item-zterm_text
                  SEPARATED BY space.
    ELSE.
      CONCATENATE zbd1t 'Days' zbd1p '%' INTO item-zterm_text
                  SEPARATED BY space.
    ENDIF.
    EXIT.
  ENDIF.

*------------- Completely filled ---------------------------------------
  IF item-zbd3t NE 0.
    CONCATENATE zbd1t 'Days' zbd1p '%,'
                zbd2t 'Days' zbd2p '%,'
                zbd3t 'Days net' INTO item-zterm_text
                SEPARATED BY space.
  ELSE.
    CONCATENATE zbd1t 'Days' zbd1p '%,'
                zbd2t 'Days' zbd2p '%' INTO item-zterm_text
                SEPARATED BY space.
  ENDIF.


ENDFORM.                    " additional_fields

*&--------------------------------------------------------------------*
*&      Form  delete_zeros
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_ZBD1X    text
*---------------------------------------------------------------------*
FORM delete_zeros
     USING p_zbd1x TYPE c.
*  DATA i TYPE i.
  SHIFT p_zbd1x RIGHT DELETING TRAILING space.
  SHIFT p_zbd1x RIGHT DELETING TRAILING '0'.
  SHIFT p_zbd1x RIGHT DELETING TRAILING '.'.
  SHIFT p_zbd1x LEFT DELETING LEADING space.
ENDFORM.                               " DELETE_ZEROS
*&---------------------------------------------------------------------*
*&      Form  insert_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_header .
  DATA: txt_out(1500),
        txt1(100).

  txt_out = 'Company Code'.
  txt1 = 'Purchasing Company Code'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Accounting Document Number'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Fiscal Year'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Accounting Document Line Item'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Document type'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Invoice Document'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Invoice document Line Item'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Reference Document Number'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Posting Date'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Last GR Date'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Vendor Number'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Vendor Name'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Vendor State'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'User name'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Currency Key'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Exchange Rate'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'CurrConversionFact'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Total Invoice Amount'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Invoice Line Item Net Amount'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Delivery costs share of item value'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
*  txt1 = 'Amount of  GR/IR adjustment'.
*  concatenate txt_out txt1 into txt_out separated by '|'.
*  txt1 = 'Amout reconciling to GL'.
*  concatenate txt_out txt1 into txt_out separated by '|'.
  txt1 = 'Payment Terms'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Due Date for Net Payment'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Quantity'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Order unit'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Tax Jurisdiction Code'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'State'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'G/L Account Number'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'G/L account short text'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Auto Tax Calc'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Tax Code'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Tax Code PO'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'AccAssCategory'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Cost Center'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Cost Center Name'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Order Number'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Order Type'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Network'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Order Desc'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Asset'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Sub Asset'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'WBS'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'CC Category'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'CC Category Desc'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Controlling Area'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'SO Number'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'SO Line Item'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'SO JurCode'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'PO Number'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'PO Line Item'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Plant'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Plant Name'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Storage Location'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Ship-To Address'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Goods Receipt Indicator'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Material Number'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Material Group'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Description of material group'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Short text'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Posted Tax Amount'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Posted State Tax Amount'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Posted County Tax Amount'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Posted City Tax Amount'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Posted Secondary Tax Amount'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Actual Tax Rate'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
*  txt1 = 'Actual Tax Rate PO'.
*  concatenate txt_out txt1 into txt_out separated by '|'.
*  txt1 = 'System Calculated Tax'.
*  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
*  txt1 = 'Tax Difference'.
*  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Archive ID'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Header Text'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Vendor Item Text'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Expense Item Text'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Clearing Date'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Clearing Document'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Check Number'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Document Date'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Profit Center'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Last GR Doc'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'Cross-Company Document'.                          "DECK914346
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.   "DECK914346
  txt1 = 'Location'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'GL Company Code'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  txt1 = 'LocDescription'.
  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
*  txt1 = 'Last GR Qty'.
*  CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
* txt1 = 'EMatrix ID'.
* CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  IF NOT recon IS INITIAL.
    txt1 = 'RecType'.
    CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
    txt1 = 'Recon Amount'.
    CONCATENATE txt_out txt1 INTO txt_out SEPARATED BY '|'.
  ENDIF.


  TRANSFER txt_out TO o_file.
ENDFORM.                    " insert_header
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_KOSAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITEM_BUDAT  text
*      -->P_ITEM_BUKRS  text
*      -->P_ITEM_KOSTL  text
*      -->P_ITEM_KOKRS  text
*      -->P_ITEM_AUFNR  text
*      -->P_ITEM_ANLN1  text
*      -->P_ITEM_ANLN2  text
*      -->P_ITEM_PS_PSP_PNR  text
*      <--P_ITEM_KOSAR  text
*----------------------------------------------------------------------*
FORM determine_kosar USING tax_date TYPE any
                           bukrs    TYPE any
                           kostl    TYPE any
                           kokrs    TYPE any
                           aufnr    TYPE any
                           nplnr    TYPE any
                           anln1    TYPE any
                           anln2    TYPE any
                           ps_psp_pnr TYPE any
                  CHANGING csks_kosar TYPE any         " SJB
*                 CHANGING csks-kosar
                           costcenter TYPE any.

  DATA: order TYPE aufnr.

  IF NOT kostl IS INITIAL.                          "Cost Center
    costcenter = kostl.
    CLEAR csks_kosar.
*   CLEAR csks-kosar.

* Cost Center Master Data
*   SELECT kosar INTO csks-kosar FROM csks
    SELECT kosar INTO csks_kosar FROM csks
           WHERE kokrs = kokrs AND
                 kostl = kostl AND
                 datbi > tax_date.
      EXIT.
    ENDSELECT.
  ELSE.
    IF NOT aufnr IS INITIAL OR
       NOT nplnr IS INITIAL.                        "Order

      IF aufnr IS INITIAL AND
         NOT nplnr IS INITIAL.
        order = nplnr.
      ELSEIF NOT aufnr IS INITIAL AND
             nplnr IS INITIAL.
        order = aufnr.
      ELSE.
        order = aufnr.
      ENDIF.
* Order master data
      SELECT SINGLE akstl kostv cycle kokrs
             INTO CORRESPONDING FIELDS OF aufk
             FROM aufk
             WHERE aufnr = order.

      IF sy-subrc = 0.
        CLEAR costcenter.
        IF NOT aufk-kostv IS INITIAL.
          costcenter = aufk-kostv.
        ENDIF.
        IF NOT aufk-cycle IS INITIAL.
          costcenter = aufk-cycle.
        ENDIF.
        IF NOT aufk-akstl IS INITIAL.
          costcenter = aufk-akstl.
        ENDIF.
*       CLEAR csks-kosar.
        CLEAR csks_kosar.

* Cost Center Master Data
*       SELECT kosar INTO csks-kosar FROM csks
        SELECT kosar INTO csks_kosar FROM csks
               WHERE kokrs = aufk-kokrs AND
                     kostl = costcenter AND
                     datbi > tax_date.
          EXIT.
        ENDSELECT.
      ENDIF.
    ELSEIF NOT anln1 IS INITIAL.                    "Asset
      CLEAR anlz-kostl.

* Time-Dependent Asset Allocations
      SELECT kostl INTO CORRESPONDING FIELDS OF anlz
             FROM anlz
             WHERE bukrs = bukrs AND
                   anln1 = anln1 AND
                   anln2 = anln2 AND
                   bdatu > tax_date.
        EXIT.
      ENDSELECT.
      IF NOT anlz-kostl IS INITIAL.
        costcenter = anlz-kostl.
        CLEAR csks-kosar.

* Cost Center Master Data
*       SELECT kosar INTO csks-kosar FROM csks
        SELECT kosar INTO csks_kosar FROM csks
               WHERE kokrs = kokrs AND
                     kostl = anlz-kostl AND
                     datbi > tax_date.
          EXIT.
        ENDSELECT.
      ENDIF.
    ELSEIF NOT ps_psp_pnr IS INITIAL.               "Project
      CLEAR prps-fkstl.

* WBS (Work Breakdown Structure) Element Master Data
      SELECT SINGLE fkstl INTO prps-fkstl
             FROM prps
             WHERE pspnr = ps_psp_pnr.
      IF NOT prps-fkstl IS INITIAL.
        costcenter = prps-fkstl.
*       CLEAR csks-kosar.
        CLEAR csks_kosar.

* Cost Center Master Data
*       SELECT kosar INTO csks-kosar FROM csks
        SELECT kosar INTO csks_kosar FROM csks
               WHERE kokrs = kokrs AND
                     kostl = prps-fkstl AND
                     datbi > tax_date.
          EXIT.
        ENDSELECT.
      ENDIF.
    ELSE.                                       "No account assignment
*     CLEAR csks-kosar.
      CLEAR csks_kosar.
      CLEAR costcenter.
    ENDIF.
  ENDIF.

ENDFORM.                    " determine_kosar=
*&---------------------------------------------------------------------*
*&      Form  get_pc_filename
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_O_FILE  text
*----------------------------------------------------------------------*
*FORM get_pc_filename USING p_file TYPE any.
*
*  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
*        EXPORTING
**         directory        = '\usr\sap\'
*           directory        = '\'
**        filemask         = ''
*        IMPORTING
*          serverfile       = p_file
*        EXCEPTIONS
*          canceled_by_user = 1
*          OTHERS           = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                    " get_pc_filename

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_transfer.
**get input file on server
  OPEN DATASET file_srv FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e103 WITH file_srv.
  ENDIF.

  DO.
    READ DATASET file_srv INTO int01.
    IF sy-subrc = 0.
      APPEND int01.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

**write file to PC
  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      filename                = file_pc
      filetype                = 'ASC'
    IMPORTING
      filelength              = v_length
    TABLES
      data_tab                = int01
    EXCEPTIONS
      file_open_error         = 1
      file_write_error        = 2
      invalid_filesize        = 3
      invalid_type            = 4
      no_batch                = 5
      unknown_error           = 6
      invalid_table_width     = 7
      gui_refuse_filetransfer = 8
      customer_error          = 9
      OTHERS                  = 10.
  IF sy-subrc <> 0.
    MESSAGE e104 WITH file_pc.
  ELSE.
    MESSAGE s105 WITH file_pc.
  ENDIF.

  CLEAR: file_srv, file_pc.

ENDFORM.                    " file_transfer

**************************
** Archive access routines
**************************

*&---------------------------------------------------------------------*
*&      Form  Get data from Cost center detail
*&---------------------------------------------------------------------*
*       Get Archive data from  Cost Centre detail
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  object_read_opened
*&---------------------------------------------------------------------*
FORM object_read_opened  TABLES   it_rel_tab
                                  it_coep_a
                                  it_cobk_a
                                  it_ekkn_a
                                  it_ekbe_a
                                  it_ekko_a
                                  it_ekpo_a
                                  it_bseg_a
                                  it_bkpf_a
                                  it_bset_a
                                  it_vbpa_a
                                  it_vbrk_a
                                  it_vbrp_a
                                  it_vbfa_a
                                  it_konv_a
                          USING value(g_read_handle) LIKE sy-tabix
                                value(l_archobj) LIKE arch_def-object
                         value(g_commit_cnt) LIKE arch_usr-arch_comit
                           g_read_cnt TYPE i
                           g_reload_cnt TYPE i.

*Internal table
  DATA: BEGIN OF lit_struc OCCURS 0,
          tabname LIKE dd02l-tabname,
          as4local LIKE dd02l-tabname,
          tabclass LIKE dd02l-tabclass,
        END OF lit_struc.

  DATA: BEGIN OF table_wa ,
        table LIKE arch_stat-tabname,
        END  OF table_wa.

  DATA: lit_data(2048) TYPE c OCCURS 1 WITH HEADER LINE.
  DATA: l_structure LIKE arch_stat-tabname,
        l_lin TYPE i.


  CLEAR:it_table_org1,
        lit_struc,
        it_table_org2,
        table_wa,
        it_rel_tab.
  REFRESH:it_table_org1[],
          lit_struc[],
          it_table_org2[],
          it_rel_tab[].

  CLEAR:g_duprec,
        g_read_cnt,
        g_reload_cnt,
        g_object_cnt.

*read all tables data related to this handle
  DO.
    ADD 1 TO g_object_cnt.
    CLEAR l_structure.
    CALL FUNCTION 'ARCHIVE_GET_NEXT_RECORD'
      EXPORTING
        archive_handle          = g_read_handle
      IMPORTING
        record                  = lit_data
        record_structure        = l_structure
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    IF NOT l_structure IS INITIAL.
      table_wa-table = l_structure.
      APPEND table_wa TO it_rel_tab.
    ENDIF.

    APPEND lit_data.
    CLEAR l_lin.
    FIELD-SYMBOLS: <fs_struc> TYPE c.

    CASE l_structure.

      WHEN  'COBK'.
        ASSIGN it_cobk_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_cobk_a.


      WHEN 'COEP'.
        ASSIGN it_coep_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_coep_a.

      WHEN 'EKKN'.
        ASSIGN it_ekkn_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_ekkn_a.


      WHEN 'EKBE'.
        ASSIGN it_ekbe_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_ekbe_a.


      WHEN 'EKKO'.
        ASSIGN it_ekko_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_ekko_a.


      WHEN 'EKPO'.
        ASSIGN it_ekpo_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_ekpo_a.

      WHEN 'BSEG'.
        ASSIGN it_bseg_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_bseg_a.

      WHEN 'BKPF'.
        ASSIGN it_bkpf_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_bkpf_a.
*fetch only bkpf data
        IF g_bkpf_only = c_x.
          EXIT.
        ENDIF.
      WHEN  'BSET'.
        ASSIGN it_bset_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_bset_a.

      WHEN 'VBPA'.
        ASSIGN it_vbpa_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_vbpa_a.


      WHEN 'VBRK'.
        ASSIGN it_vbrk_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_vbrk_a.


      WHEN 'VBRP'.
        ASSIGN it_vbrp_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_vbrp_a.


      WHEN 'VBFA'.
        ASSIGN it_vbfa_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_vbfa_a.


      WHEN 'KONV'.
        ASSIGN it_konv_a TO <fs_struc> CASTING.
        <fs_struc> = lit_data.
        APPEND it_konv_a.

    ENDCASE.
    REFRESH lit_data.
    CLEAR g_object_cnt.


  ENDDO.

ENDFORM.                    " object_read_opened
*&---------------------------------------------------------------------*
*&      Form  get_arch_report_detail
*&---------------------------------------------------------------------*
*       Get Archive data from  Purchasing document
*----------------------------------------------------------------------*

FORM get_arch_report_detail  TABLES   it_covp STRUCTURE it_covp
                                      it_ekkn_a STRUCTURE it_ekkn_a
                                      it_ekbe_a STRUCTURE it_ekbe_a
                                      it_ekko_a STRUCTURE it_ekko_a.

  DATA: l_archobj LIKE arch_def-object VALUE 'MM_EKKO'.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref,
        g_ref1,
        it_archkey1,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_ekkn_a,
        it_ekbe_a,
       it_ekko_a.

  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[],
          it_archkey1[],
          it_rel_tab[],
          it_ekkn_a[],
          it_ekbe_a[],
          it_ekko_a[].


  IF ( <fs_value> IS ASSIGNED ) OR ( <fs_wa> IS ASSIGNED ) OR
    ( <fs_archivekey> IS ASSIGNED ) OR ( <fs_ebeln> IS ASSIGNED ) OR
    ( <fs_archiveofs> IS ASSIGNED ).
    UNASSIGN:<fs_value> ,
             <fs_wa>,
             <fs_archivekey>,
             <fs_ebeln>,
             <fs_archiveofs>.
  ENDIF.
*Fetch the Archive infostructure details for the archiving object
  SELECT aind_str1~archindex
               aind_str1~itype
               aind_str1~otyp
               aind_str1~object
               aind_str2~active
               aind_str2~gentab
               FROM aind_str1 INNER JOIN aind_str2
               ON aind_str1~archindex = aind_str2~archindex
                INTO TABLE IT_INFSTR
               WHERE object EQ 'MM_EKKO'
                AND  aind_str1~archindex EQ g_infstr1 "ZSAP_MM_REBEL
                AND  aind_str1~itype EQ c_i
                AND aind_str1~otyp EQ c_o
                AND aind_str2~active EQ c_x.


*Creation of Dynamic internal table.

  IF NOT it_infstr[] IS INITIAL.
    READ TABLE it_infstr WITH KEY archindex = g_infstr1.
  ENDIF.

  SELECT *
         FROM dd03l
         INTO TABLE it_dd03l
         WHERE tabname EQ it_infstr-gentab.

  IF NOT sy-subrc IS INITIAL.
*exit routine
    EXIT.
  ENDIF.

  SORT it_dd03l.

  LOOP AT it_dd03l.
    CLEAR ls_fieldcat.
    ls_fieldcat-row_pos         =  0.
    ls_fieldcat-col_pos         =  it_dd03l-position.
    ls_fieldcat-fieldname       =  it_dd03l-fieldname.
    ls_fieldcat-tabname         =  '1'.
    ls_fieldcat-datatype        =  it_dd03l-datatype.
    ls_fieldcat-reptext         =  'Type'.
    ls_fieldcat-outputlen       =  it_dd03l-leng.
    ls_fieldcat-coltext         = 'Type'.
    ls_fieldcat-key             = it_dd03l-keyflag.
    ls_fieldcat-emphasize       = 'C010'.
    APPEND ls_fieldcat TO it_fieldcatalog.
  ENDLOOP.
*create dynamic internal table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = it_fieldcatalog
    IMPORTING
      ep_table                  = g_ref
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.

  ASSIGN g_ref->* TO <fs_value>.


  READ TABLE it_infstr INDEX 1.

  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
    SELECT * FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
             FOR ALL ENTRIES IN it_covp
             WHERE ebeln EQ it_covp-ebeln
               AND ebelp EQ it_covp-ebelp.

    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.
    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.

    IF NOT <fs_value>[] IS INITIAL.
      LOOP AT <fs_value> INTO <fs_wa>.

        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_wa> TO <fs_ebeln>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey1-ebeln = <fs_ebeln>.
        it_archkey1-arch_key = <fs_archivekey>.
        it_archkey1-arch_offset = <fs_archiveofs>.

        APPEND it_archkey1.
        CLEAR it_archkey1  .

*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'MM_EKKO'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.
        IF sy-subrc = 0.
*Get Purchasing Document details
          PERFORM object_read_opened
                     TABLES it_rel_tab
                            it_coep_a
                            it_cobk_a
                            it_ekkn_a
                            it_ekbe_a
                            it_ekko_a
                            it_ekpo_a
                            it_bseg_a
                            it_bkpf_a
                            it_bset_a
                            it_vbpa_a
                            it_vbrk_a
                            it_vbrp_a
                            it_vbfa_a
                            it_konv_a
                     USING g_read_handle
                           l_archobj
                           g_commit_cnt
                           g_read_cnt
                           g_reload_cnt    .

        ENDIF.

*close the archive file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_arch_report_detail
*&---------------------------------------------------------------------*
*&      Form  arch_bseg_data
*&---------------------------------------------------------------------*
*       Get Archive data from  Acoounting Document
*----------------------------------------------------------------------*
FORM arch_bseg_data  TABLES  it_bseg_a STRUCTURE it_bseg_a
                             it_bkpf_a STRUCTURE it_bkpf_a
                             it_bset_a STRUCTURE it_bset_a
                     USING   value(w_bukrs) LIKE bkpf-bukrs
                             value(w_belnr) LIKE bkpf-belnr
                             value(w_gjahr) LIKE bkpf-gjahr
                             value(v_tabix) LIKE sy-tabix.


  DATA:l_archobj LIKE arch_def-object VALUE 'FI_DOCUMNT'.


  CLEAR:g_ref1,
        it_archkey2,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_bseg_a,
        it_bkpf_a,
        it_bset_a.

  REFRESH:it_archkey2[],
          it_rel_tab[],
          it_bseg_a[],
          it_bkpf_a[],
          it_bset_a[].



  IF  ( <fs_wa> IS ASSIGNED ) OR
    ( <fs_archivekey> IS ASSIGNED ) OR ( <fs_bukrs> IS ASSIGNED ) OR
    ( <fs_archiveofs> IS ASSIGNED ) OR ( <fs_belnr> IS ASSIGNED ) OR
     ( <fs_gjahr> IS ASSIGNED ).
*unassign
    UNASSIGN:<fs_wa>,
             <fs_archivekey>,
             <fs_bukrs>,
             <fs_belnr>,
             <fs_gjahr>,
             <fs_archiveofs>.
  ENDIF.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref.
  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[].

  IF ( <fs_value> IS ASSIGNED ).
    UNASSIGN: <fs_value>.
  ENDIF.

*Fetch the Archive infostructure details for the archiving object
  SELECT aind_str1~archindex
         aind_str1~itype
         aind_str1~otyp
         aind_str1~object
         aind_str2~active
         aind_str2~gentab
         FROM aind_str1 INNER JOIN aind_str2
         ON aind_str1~archindex = aind_str2~archindex
          INTO TABLE IT_INFSTR
         WHERE object EQ 'FI_DOCUMNT'
          AND  aind_str1~archindex EQ g_infstr2
          AND  aind_str1~itype EQ c_i
          AND aind_str1~otyp EQ c_o
          AND aind_str2~active EQ c_x.


  IF g_ref_bseg IS INITIAL.

    IF NOT it_infstr[] IS INITIAL.
      READ TABLE it_infstr WITH KEY archindex = g_infstr2.
    ENDIF.

    SELECT *
           FROM dd03l
           INTO TABLE it_dd03l
           WHERE tabname EQ it_infstr-gentab.

    IF NOT sy-subrc IS INITIAL.
*exit the routine
      EXIT.
    ENDIF.

    SORT it_dd03l.

    LOOP AT it_dd03l.
      CLEAR ls_fieldcat.
      ls_fieldcat-row_pos         =  0.
      ls_fieldcat-col_pos         =  it_dd03l-position.
      ls_fieldcat-fieldname       =  it_dd03l-fieldname.
      ls_fieldcat-tabname         =  '1'.
      ls_fieldcat-datatype        =  it_dd03l-datatype.
      ls_fieldcat-reptext         =  'Type'.
      ls_fieldcat-outputlen       =  it_dd03l-leng.
      ls_fieldcat-coltext         = 'Type'.
      ls_fieldcat-key             = it_dd03l-keyflag.
      ls_fieldcat-emphasize       = 'C010'.
      APPEND ls_fieldcat TO it_fieldcatalog.
    ENDLOOP.
*dynamic internal table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = it_fieldcatalog
      IMPORTING
        ep_table                  = g_ref_bseg
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.

  ENDIF.
  ASSIGN g_ref_bseg->* TO <fs_value>.

  READ TABLE it_infstr INDEX 1.

  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
    SELECT * FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
             WHERE bukrs EQ w_bukrs
               AND belnr EQ w_belnr
               AND gjahr EQ w_gjahr.

    IF sy-subrc NE 0.
      EXIT.
    ENDIF.


    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.

    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.

    IF NOT <fs_value>[] IS INITIAL.
      LOOP AT <fs_value> INTO <fs_wa>.

        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_wa> TO <fs_bukrs>.
        ASSIGN COMPONENT 'BELNR' OF STRUCTURE <fs_wa> TO <fs_belnr>.
        ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_wa> TO <fs_gjahr>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey2-bukrs = <fs_bukrs>.
        it_archkey2-belnr = <fs_belnr>.
        it_archkey2-gjahr = <fs_gjahr>.
        it_archkey2-arch_key = <fs_archivekey>.
        it_archkey2-arch_offset = <fs_archiveofs>.

        APPEND it_archkey2.
        CLEAR it_archkey2.
*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'FI_DOCUMNT'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        IF sy-subrc = 0.

*Get Acoounting Document details

          PERFORM object_read_opened
                    TABLES it_rel_tab
                           it_coep_a
                           it_cobk_a
                           it_ekkn_a
                           it_ekbe_a
                           it_ekko_a
                           it_ekpo_a
                           it_bseg_a
                           it_bkpf_a
                           it_bset_a
                           it_vbpa_a
                           it_vbrk_a
                           it_vbrp_a
                           it_vbfa_a
                           it_konv_a
                    USING g_read_handle
                          l_archobj
                          g_commit_cnt
                          g_read_cnt
                          g_reload_cnt   .
        ENDIF.
*close the archive file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " arch_bseg_data
*&---------------------------------------------------------------------*
*&      Form  arch_bseg_data_AWKEY
*&---------------------------------------------------------------------*
*       Get Archive data from  Acoounting Document via AWKEY
*----------------------------------------------------------------------*
FORM arch_bseg_data_awkey  TABLES  it_bseg_a STRUCTURE it_bseg_a
                             it_bkpf_a STRUCTURE it_bkpf_a
                             it_bset_a STRUCTURE it_bset_a
                     USING   value(w_bukrs) LIKE bkpf-bukrs
                             value(w_awkey) LIKE bkpf-awkey
                             value(w_awtyp) LIKE bkpf-awtyp
                             value(v_tabix) LIKE sy-tabix.


  DATA:l_archobj LIKE arch_def-object VALUE 'FI_DOCUMNT'.


  CLEAR:g_ref1,
        it_archkey2,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_bseg_a,
        it_bkpf_a,
        it_bset_a.

  REFRESH:it_archkey2[],
          it_rel_tab[],
          it_bseg_a[],
          it_bkpf_a[],
          it_bset_a[].



  IF  ( <fs_wa> IS ASSIGNED ) OR
    ( <fs_archivekey> IS ASSIGNED ) OR ( <fs_bukrs> IS ASSIGNED ) OR
    ( <fs_archiveofs> IS ASSIGNED ) OR ( <fs_belnr> IS ASSIGNED ) OR
     ( <fs_gjahr> IS ASSIGNED ).
*unassign
    UNASSIGN:<fs_wa>,
             <fs_archivekey>,
             <fs_bukrs>,
             <fs_belnr>,
             <fs_gjahr>,
             <fs_archiveofs>.
  ENDIF.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref.
  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[].

  IF ( <fs_value> IS ASSIGNED ).
    UNASSIGN: <fs_value>.
  ENDIF.

*Fetch the Archive infostructure details for the archiving object
  SELECT aind_str1~archindex
         aind_str1~itype
         aind_str1~otyp
         aind_str1~object
         aind_str2~active
         aind_str2~gentab
         FROM aind_str1 INNER JOIN aind_str2
         ON aind_str1~archindex = aind_str2~archindex
          INTO TABLE IT_INFSTR
         WHERE object EQ 'FI_DOCUMNT'
          AND  aind_str1~archindex EQ g_infstr2
          AND  aind_str1~itype EQ c_i
          AND aind_str1~otyp EQ c_o
          AND aind_str2~active EQ c_x.


  IF g_ref_bseg IS INITIAL.

    IF NOT it_infstr[] IS INITIAL.
      READ TABLE it_infstr WITH KEY archindex = g_infstr2.
    ENDIF.

    SELECT *
           FROM dd03l
           INTO TABLE it_dd03l
           WHERE tabname EQ it_infstr-gentab.

    IF NOT sy-subrc IS INITIAL.
*exit the routine
      EXIT.
    ENDIF.

    SORT it_dd03l.

    LOOP AT it_dd03l.
      CLEAR ls_fieldcat.
      ls_fieldcat-row_pos         =  0.
      ls_fieldcat-col_pos         =  it_dd03l-position.
      ls_fieldcat-fieldname       =  it_dd03l-fieldname.
      ls_fieldcat-tabname         =  '1'.
      ls_fieldcat-datatype        =  it_dd03l-datatype.
      ls_fieldcat-reptext         =  'Type'.
      ls_fieldcat-outputlen       =  it_dd03l-leng.
      ls_fieldcat-coltext         = 'Type'.
      ls_fieldcat-key             = it_dd03l-keyflag.
      ls_fieldcat-emphasize       = 'C010'.
      APPEND ls_fieldcat TO it_fieldcatalog.
    ENDLOOP.
*dynamic internal table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = it_fieldcatalog
      IMPORTING
        ep_table                  = g_ref_bseg
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.

  ENDIF.
  ASSIGN g_ref_bseg->* TO <fs_value>.

  READ TABLE it_infstr INDEX 1.

  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
    SELECT * FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
             WHERE awkey EQ w_awkey
               AND awtyp EQ w_awtyp
               AND bukrs EQ w_bukrs.

    IF sy-subrc NE 0.
      EXIT.
    ENDIF.


    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.

    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.

    IF NOT <fs_value>[] IS INITIAL.
      LOOP AT <fs_value> INTO <fs_wa>.

        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_wa> TO <fs_bukrs>.
        ASSIGN COMPONENT 'BELNR' OF STRUCTURE <fs_wa> TO <fs_belnr>.
        ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_wa> TO <fs_gjahr>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey2-bukrs = <fs_bukrs>.
        it_archkey2-belnr = <fs_belnr>.
        it_archkey2-gjahr = <fs_gjahr>.
        it_archkey2-arch_key = <fs_archivekey>.
        it_archkey2-arch_offset = <fs_archiveofs>.

        APPEND it_archkey2.
        CLEAR it_archkey2.
*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'FI_DOCUMNT'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        IF sy-subrc = 0.

*Get Acoounting Document details

          PERFORM object_read_opened
                    TABLES it_rel_tab
                           it_coep_a
                           it_cobk_a
                           it_ekkn_a
                           it_ekbe_a
                           it_ekko_a
                           it_ekpo_a
                           it_bseg_a
                           it_bkpf_a
                           it_bset_a
                           it_vbpa_a
                           it_vbrk_a
                           it_vbrp_a
                           it_vbfa_a
                           it_konv_a
                    USING g_read_handle
                          l_archobj
                          g_commit_cnt
                          g_read_cnt
                          g_reload_cnt   .
        ENDIF.
*close the archive file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " arch_bseg_data_awkey
*&---------------------------------------------------------------------*
*&      Form  arch_vbpa_data
*&---------------------------------------------------------------------*
*       Get Archive data from  Sales Document
*----------------------------------------------------------------------*
FORM arch_vbpa_data  TABLES   it_vbpa_a STRUCTURE it_vbpa_a
                     USING  value(w_belnr) LIKE covp-refbn.

  DATA: l_archobj LIKE arch_def-object VALUE 'SD_VBAK'.

  CLEAR:g_ref1,
        it_archkey3,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_vbpa_a.

  REFRESH:it_archkey3[],
          it_rel_tab[],
          it_vbpa_a[].

  IF ( <fs_wa> IS ASSIGNED ) OR ( <fs_archivekey> IS ASSIGNED ) OR
     ( <fs_vbeln> IS ASSIGNED ) OR ( <fs_archiveofs> IS ASSIGNED ).
*unassign
    UNASSIGN:<fs_wa>,
             <fs_archivekey>,
             <fs_vbeln>,
             <fs_archiveofs>.
  ENDIF.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref.
  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[].

  IF ( <fs_value> IS ASSIGNED ).
    UNASSIGN: <fs_value>.
  ENDIF.
*Fetch the Archive infostructure details for the archiving object
  SELECT aind_str1~archindex
         aind_str1~itype
         aind_str1~otyp
         aind_str1~object
         aind_str2~active
         aind_str2~gentab
         FROM aind_str1 INNER JOIN aind_str2
         ON aind_str1~archindex = aind_str2~archindex
          INTO TABLE IT_INFSTR
         WHERE object EQ 'SD_VBAK'
          AND  aind_str1~archindex EQ g_infstr3
          AND  aind_str1~itype EQ c_i
          AND aind_str1~otyp EQ c_o
          AND aind_str2~active EQ c_x.


  IF g_ref_vbak IS INITIAL.
*Creation of Dynamic internal table..

    IF NOT it_infstr[] IS INITIAL.
      READ TABLE it_infstr WITH KEY archindex = g_infstr3.
    ENDIF.

    SELECT *
           FROM dd03l
           INTO TABLE it_dd03l
           WHERE tabname EQ it_infstr-gentab.


    IF NOT sy-subrc IS INITIAL.
*exit routine
      EXIT.
    ENDIF.

    SORT it_dd03l.

    LOOP AT it_dd03l.
      CLEAR ls_fieldcat.
      ls_fieldcat-row_pos         =  0.
      ls_fieldcat-col_pos         =  it_dd03l-position.
      ls_fieldcat-fieldname       =  it_dd03l-fieldname.
      ls_fieldcat-tabname         =  '1'.
      ls_fieldcat-datatype        =  it_dd03l-datatype.
      ls_fieldcat-reptext         =  'Type'.
      ls_fieldcat-outputlen       =  it_dd03l-leng.
      ls_fieldcat-coltext         = 'Type'.
      ls_fieldcat-key             = it_dd03l-keyflag.
      ls_fieldcat-emphasize       = 'C010'.
      APPEND ls_fieldcat TO it_fieldcatalog.
    ENDLOOP.
*dynamic internal table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = it_fieldcatalog
      IMPORTING
        ep_table                  = g_ref_vbak
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.
  ENDIF.
  ASSIGN g_ref_vbak->* TO <fs_value>.

  READ TABLE it_infstr INDEX 1.
  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
    SELECT * FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
                 WHERE vbeln EQ w_belnr.

    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.

    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.

    IF NOT <fs_value>[] IS INITIAL.
      LOOP AT <fs_value> INTO <fs_wa>.


        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <fs_wa> TO <fs_vbeln>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey3-vbeln = <fs_vbeln>.
        it_archkey3-arch_key = <fs_archivekey>.
        it_archkey3-arch_offset = <fs_archiveofs>.

        APPEND it_archkey3.
        CLEAR it_archkey3.
*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'SD_VBAK'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        IF sy-subrc IS INITIAL.
*Get Sales Document details
          PERFORM object_read_opened
                    TABLES it_rel_tab
                           it_coep_a
                           it_cobk_a
                           it_ekkn_a
                           it_ekbe_a
                           it_ekko_a
                           it_ekpo_a
                           it_bseg_a
                           it_bkpf_a
                           it_bset_a
                           it_vbpa_a
                           it_vbrk_a
                           it_vbrp_a
                           it_vbfa_a
                           it_konv_a
                    USING g_read_handle
                          l_archobj
                          g_commit_cnt
                          g_read_cnt
                          g_reload_cnt  .

        ENDIF.
*close the archive file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " arch_vbpa_data
*&---------------------------------------------------------------------*
*&      Form  get_arch_bkpf
*&---------------------------------------------------------------------*
*       Get Archive data from  Acoounting document
*----------------------------------------------------------------------*
FORM get_arch_bkpf TABLES bukrs STRUCTURE s_bukrs
                          belnr STRUCTURE s_belnr
                          gjahr STRUCTURE s_gjahr
                          blart STRUCTURE s_blart
                          budat1 STRUCTURE s_budat
                          it_bkpf_a
                          it_bseg_a
                          it_bset_a
                          it_bvor_a.


  DATA:l_archobj LIKE arch_def-object VALUE 'FI_DOCUMNT'.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref,
        g_ref1,
        it_archkey2,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_bseg_a,
        it_bkpf_a,
        it_bset_a,
        it_bvor_a.

  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[],
          it_archkey2[],
          it_rel_tab[],
          it_bseg_a[],
          it_bkpf_a[],
          it_bset_a[],
          it_bvor_a[].


  IF ( <fs_value> IS ASSIGNED ) OR ( <fs_wa> IS ASSIGNED ) OR
    ( <fs_archivekey> IS ASSIGNED ) OR ( <fs_bukrs> IS ASSIGNED ) OR
    ( <fs_archiveofs> IS ASSIGNED ) OR ( <fs_belnr> IS ASSIGNED ) OR
     ( <fs_gjahr> IS ASSIGNED ).
*unassign field symbols
    UNASSIGN:<fs_value>,
             <fs_wa>,
             <fs_archivekey>,
             <fs_bukrs>,
             <fs_belnr>,
             <fs_gjahr>,
            <fs_archiveofs>.
  ENDIF.

*Fetch the Archive infostructure details for the archiving object

  SELECT   aind_str1~archindex
           aind_str1~itype
           aind_str1~otyp
           aind_str1~object
           aind_str2~active
           aind_str2~gentab
           FROM aind_str1 INNER JOIN aind_str2
           ON aind_str1~archindex = aind_str2~archindex
            INTO TABLE IT_INFSTR
           WHERE object EQ 'FI_DOCUMNT'
            AND  aind_str1~archindex EQ g_infstr2
            AND  aind_str1~itype EQ c_i
            AND aind_str1~otyp EQ c_o
            AND aind_str2~active EQ c_x.



  IF NOT it_infstr[] IS INITIAL.
    READ TABLE it_infstr WITH KEY archindex = g_infstr2.
  ENDIF.

  SELECT *
        FROM dd03l
        INTO TABLE it_dd03l
        WHERE tabname EQ it_infstr-gentab.
  IF NOT sy-subrc IS INITIAL.
*exit routine
    EXIT.
  ENDIF.

  SORT it_dd03l.

  LOOP AT it_dd03l.
    CLEAR ls_fieldcat.
    ls_fieldcat-row_pos         =  0.
    ls_fieldcat-col_pos         =  it_dd03l-position.
    ls_fieldcat-fieldname       =  it_dd03l-fieldname.
    ls_fieldcat-tabname         =  '1'.
    ls_fieldcat-datatype        =  it_dd03l-datatype.
    ls_fieldcat-reptext         =  'Type'.
    ls_fieldcat-outputlen       =  it_dd03l-leng.
    ls_fieldcat-coltext         = 'Type'.
    ls_fieldcat-key             = it_dd03l-keyflag.
    ls_fieldcat-emphasize       = 'C010'.
    APPEND ls_fieldcat TO it_fieldcatalog.
  ENDLOOP.

*Creation of Dynamic internal table.
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_fieldcatalog
    IMPORTING
      ep_table        = g_ref.

  ASSIGN g_ref->* TO <fs_value>.


  READ TABLE it_infstr INDEX 1.

  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
*    SELECT * FROM (it_infstr-gentab)
*             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
*             WHERE bukrs IN bukrs
*               AND belnr IN belnr
*               AND gjahr IN gjahr
*               AND blart IN blart
*               AND budat IN budat1.
    SELECT DISTINCT bukrs belnr gjahr archivekey archiveofs xblnr blart awtyp awkey awsys
             FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
             WHERE bukrs IN bukrs
               AND belnr IN belnr
               AND gjahr IN gjahr
               AND blart IN blart
               AND budat IN budat1.

    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.

    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.

    IF NOT <fs_value>[] IS INITIAL.

      LOOP AT <fs_value> INTO <fs_wa>.

        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_wa> TO <fs_bukrs>.
        ASSIGN COMPONENT 'BELNR' OF STRUCTURE <fs_wa> TO <fs_belnr>.
        ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_wa> TO <fs_gjahr>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey2-bukrs = <fs_bukrs>.
        it_archkey2-belnr = <fs_belnr>.
        it_archkey2-gjahr = <fs_gjahr>.
        it_archkey2-arch_key = <fs_archivekey>.
        it_archkey2-arch_offset = <fs_archiveofs>.

        APPEND it_archkey2.
        CLEAR it_archkey2.
*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'FI_DOCUMNT'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ELSE.

          REFRESH:lt_bkpf[],
                  lt_bseg[],
                  lt_bset[],
                  lt_bvor[].
*reading the archive file
          CALL FUNCTION 'FI_DOCUMENT_ARCH_READ_SINGLE'
            EXPORTING
              i_bukrs      = <fs_bukrs>
              i_belnr      = <fs_belnr>
              i_gjahr      = <fs_gjahr>
              i_archiv_key = <fs_archivekey>
              i_offst      = <fs_archiveofs>
            TABLES
              c_abkpf      = lt_bkpf
              c_bseg       = lt_bseg
              c_bset       = lt_bset
              c_bvor       = lt_bvor.
*bkpf data
          IF NOT lt_bkpf[] IS INITIAL.
            LOOP AT lt_bkpf.
              it_bkpf_a = lt_bkpf.
              APPEND it_bkpf_a.
              CLEAR it_bkpf_a.
            ENDLOOP.
          ENDIF.
*bseg data
          IF NOT lt_bseg[] IS INITIAL.
            LOOP AT lt_bseg.
              it_bseg_a = lt_bseg.
              APPEND it_bseg_a.
              CLEAR it_bseg_a.
            ENDLOOP.
          ENDIF.
*bset data
          IF NOT lt_bset[] IS INITIAL.
            LOOP AT lt_bset.
              it_bset_a = lt_bset.
              APPEND it_bset_a.
              CLEAR it_bset_a.
            ENDLOOP.
          ENDIF.
*bvor data
          IF NOT lt_bvor[] IS INITIAL.
            LOOP AT lt_bvor.
              it_bvor_a = lt_bvor.
              APPEND it_bvor_a.
              CLEAR it_bvor_a.
            ENDLOOP.
          ENDIF.

        ENDIF.
*close the file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_arch_bkpf
*&------------------------------------------------------------*
*&      Form  arch_ekko_data
*&------------------------------------------------------------*
*       Get Archive data from  Purchasing Document
*----------------------------------------------------------------------*

FORM arch_ekko_data TABLES  it_ekko_a STRUCTURE it_ekko_a
                            it_ekpo_a STRUCTURE it_ekpo_a
                            it_ekkn_a STRUCTURE it_ekkn_a
                            it_ekbe_a STRUCTURE it_ekbe_a
                     USING  value(w_ebeln) LIKE rseg-ebeln
                            value(w_ebelp) LIKE rseg-ebelp.

  DATA:l_archobj LIKE arch_def-object VALUE 'MM_EKKO'.

  CLEAR:g_ref1,
        it_archkey1,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_ekkn_a,
        it_ekbe_a,
        it_ekko_a.

  REFRESH:it_archkey1[],
          it_rel_tab[],
          it_ekkn_a[],
          it_ekbe_a[],
          it_ekko_a[].


  IF ( <fs_wa> IS ASSIGNED ) OR ( <fs_archivekey> IS ASSIGNED ) OR
     ( <fs_ebeln> IS ASSIGNED ) OR ( <fs_archiveofs> IS ASSIGNED ).
*unassign field symbols
    UNASSIGN:<fs_wa>,
             <fs_archivekey>,
             <fs_ebeln>,
             <fs_archiveofs>.
  ENDIF.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref.

  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[].

  IF ( <fs_value> IS ASSIGNED ).
    UNASSIGN: <fs_value>.
  ENDIF.
*Fetch the Archive infostructure details for the archiving object
  SELECT aind_str1~archindex
               aind_str1~itype
               aind_str1~otyp
               aind_str1~object
               aind_str2~active
               aind_str2~gentab
               FROM aind_str1 INNER JOIN aind_str2
               ON aind_str1~archindex = aind_str2~archindex
                INTO TABLE IT_INFSTR
               WHERE object EQ 'MM_EKKO'
                AND  aind_str1~archindex EQ g_infstr1
                AND  aind_str1~itype EQ c_i
                AND aind_str1~otyp EQ c_o
                AND aind_str2~active EQ c_x.


  IF g_ref_ekko IS INITIAL.

*Creation of Dynamic internal table.

    IF NOT it_infstr[] IS INITIAL.
      READ TABLE it_infstr WITH KEY archindex = g_infstr1.
    ENDIF.
    SELECT *
           FROM dd03l
           INTO TABLE it_dd03l
           WHERE tabname EQ it_infstr-gentab.

    IF NOT sy-subrc IS INITIAL.
*exit routine
      EXIT.
    ENDIF.

    SORT it_dd03l.

    LOOP AT it_dd03l.
      CLEAR ls_fieldcat.
      ls_fieldcat-row_pos         =  0.
      ls_fieldcat-col_pos         =  it_dd03l-position.
      ls_fieldcat-fieldname       =  it_dd03l-fieldname.
      ls_fieldcat-tabname         =  '1'.
      ls_fieldcat-datatype        =  it_dd03l-datatype.
      ls_fieldcat-reptext         =  'Type'.
      ls_fieldcat-outputlen       =  it_dd03l-leng.
      ls_fieldcat-coltext         = 'Type'.
      ls_fieldcat-key             = it_dd03l-keyflag.
      ls_fieldcat-emphasize       = 'C010'.
      APPEND ls_fieldcat TO it_fieldcatalog.
    ENDLOOP.
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = it_fieldcatalog
      IMPORTING
        ep_table        = g_ref_ekko.
  ENDIF.
  ASSIGN g_ref_ekko->* TO <fs_value>.

  READ TABLE it_infstr INDEX 1.

  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
    SELECT * FROM (it_infstr-gentab)
           INTO CORRESPONDING FIELDS OF TABLE <fs_value>
           WHERE ebeln EQ w_ebeln
             AND ebelp EQ w_ebelp.


    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.
    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.

    IF NOT <fs_value>[] IS INITIAL.
      LOOP AT <fs_value> INTO <fs_wa>.

        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_wa> TO <fs_ebeln>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey1-ebeln = <fs_ebeln>.
        it_archkey1-arch_key = <fs_archivekey>.
        it_archkey1-arch_offset = <fs_archiveofs>.

        APPEND it_archkey1.
        CLEAR it_archkey1  .

*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'MM_EKKO'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.
        IF sy-subrc IS INITIAL.
*Get Purchasing Document details
          PERFORM object_read_opened
                     TABLES it_rel_tab
                            it_coep_a
                            it_cobk_a
                            it_ekkn_a
                            it_ekbe_a
                            it_ekko_a
                            it_ekpo_a
                            it_bseg_a
                            it_bkpf_a
                            it_bset_a
                            it_vbpa_a
                            it_vbrk_a
                            it_vbrp_a
                            it_vbfa_a
                            it_konv_a
                     USING g_read_handle
                           l_archobj
                           g_commit_cnt
                           g_read_cnt
                           g_reload_cnt  .

        ENDIF.

*close the archive file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " arch_ekko_data
*&---------------------------------------------------------------------*
*&      Form  arch_get_bkpf
*&---------------------------------------------------------------------*
*       Get Archive data from  Accounting document
*----------------------------------------------------------------------*
FORM arch_get_bkpf  TABLES   it_bkpf_mkpf STRUCTURE it_bkpf_mkpf
                    USING    value(bseg_tab_zuonr) LIKE bseg-zuonr.

  DATA:l_archobj LIKE arch_def-object VALUE 'FI_DOCUMNT'.

  CLEAR:g_ref1,
        it_archkey2,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_bkpf_mkpf,
        it_bseg_a,
        it_bkpf_a,
        it_bset_a.


  REFRESH:it_archkey2[],
          it_rel_tab[],
          it_bkpf_mkpf[],
          it_bseg_a[],
          it_bkpf_a[],
          it_bset_a[].


  IF ( <fs_wa> IS ASSIGNED )    OR ( <fs_archivekey> IS ASSIGNED ) OR
     ( <fs_bukrs> IS ASSIGNED ) OR ( <fs_belnr> IS ASSIGNED ) OR
     ( <fs_gjahr> IS ASSIGNED ) OR ( <fs_archiveofs> IS ASSIGNED ).
*unassign field symbols
    UNASSIGN:<fs_wa>,
             <fs_archivekey>,
             <fs_bukrs>,
             <fs_belnr>,
             <fs_gjahr>,
             <fs_archiveofs>.
  ENDIF.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref.

  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[].

  IF ( <fs_value> IS ASSIGNED ).
    UNASSIGN: <fs_value>.
  ENDIF.
*Fetch the Archive infostructure details for the archiving object
  SELECT aind_str1~archindex
               aind_str1~itype
               aind_str1~otyp
               aind_str1~object
               aind_str2~active
               aind_str2~gentab
               FROM aind_str1 INNER JOIN aind_str2
               ON aind_str1~archindex = aind_str2~archindex
                INTO TABLE IT_INFSTR
               WHERE object EQ 'FI_DOCUMNT'
                AND  aind_str1~archindex EQ g_infstr2
                AND  aind_str1~itype EQ c_i
                AND aind_str1~otyp EQ c_o
                AND aind_str2~active EQ c_x.


  IF g_ref_bkpf IS INITIAL.
*Creation of Dynamic internal table.

    IF NOT it_infstr[] IS INITIAL.
      READ TABLE it_infstr WITH KEY archindex = g_infstr2.
    ENDIF.

    SELECT *
           FROM dd03l
           INTO TABLE it_dd03l
           WHERE tabname EQ it_infstr-gentab.
    IF NOT sy-subrc IS INITIAL.
*exit routine
      EXIT.
    ENDIF.

    SORT it_dd03l.

    LOOP AT it_dd03l.
      CLEAR ls_fieldcat.
      ls_fieldcat-row_pos         =  0.
      ls_fieldcat-col_pos         =  it_dd03l-position.
      ls_fieldcat-fieldname       =  it_dd03l-fieldname.
      ls_fieldcat-tabname         =  '1'.
      ls_fieldcat-datatype        =  it_dd03l-datatype.
      ls_fieldcat-reptext         =  'Type'.
      ls_fieldcat-outputlen       =  it_dd03l-leng.
      ls_fieldcat-coltext         = 'Type'.
      ls_fieldcat-key             = it_dd03l-keyflag.
      ls_fieldcat-emphasize       = 'C010'.
      APPEND ls_fieldcat TO it_fieldcatalog.
    ENDLOOP.
*dynamic internal table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = it_fieldcatalog
      IMPORTING
        ep_table        = g_ref_bkpf.
  ENDIF.
  ASSIGN g_ref_bkpf->* TO <fs_value>.

*archive infostructure
  READ TABLE it_infstr INDEX 1.

  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
    SELECT * FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
             WHERE awkey EQ  bseg_tab_zuonr
               AND awtyp EQ 'MKPF'.

    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.
    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.

    IF NOT <fs_value>[] IS INITIAL.
      LOOP AT <fs_value> INTO <fs_wa>.

        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_wa> TO <fs_bukrs>.
        ASSIGN COMPONENT 'BELNR' OF STRUCTURE <fs_wa> TO <fs_belnr>.
        ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_wa> TO <fs_gjahr>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.
*account doc detaisl
        it_archkey2-bukrs = <fs_bukrs>.
        it_archkey2-belnr = <fs_belnr>.
        it_archkey2-gjahr = <fs_gjahr>.
        it_archkey2-arch_key = <fs_archivekey>.
        it_archkey2-arch_offset = <fs_archiveofs>.

        APPEND it_archkey2.
        CLEAR it_archkey2  .

*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'FI_DOCUMNT'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.
        IF sy-subrc IS INITIAL.
*Get Accounting Document details
          PERFORM object_read_opened
                     TABLES it_rel_tab
                            it_coep_a
                            it_cobk_a
                            it_ekkn_a
                            it_ekbe_a
                            it_ekko_a
                            it_ekpo_a
                            it_bseg_a
                            it_bkpf_a
                            it_bset_a
                            it_vbpa_a
                            it_vbrk_a
                            it_vbrp_a
                            it_vbfa_a
                            it_konv_a
                     USING g_read_handle
                           l_archobj
                           g_commit_cnt
                           g_read_cnt
                           g_reload_cnt  .
        ENDIF.
*close the archive file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.

  it_bkpf_mkpf[] = it_bkpf_a[].

  CLEAR it_bkpf_mkpf.
  REFRESH:it_bkpf_a[].

ENDFORM.                    " arch_get_bkpf
*&---------------------------------------------------------------------*
*&      Form  get_zfr0acqr_arch_bkpf
*&---------------------------------------------------------------------*
*       Get Archive data from  Accounting document
*----------------------------------------------------------------------*
FORM get_zfr0acqr_arch_bkpf  TABLES   it_bkpf_a
                                     USING    p_bukrs
                                      p_gjahr
                                      p_monat.

  DATA:l_archobj LIKE arch_def-object VALUE 'FI_DOCUMNT'.

  CLEAR:g_ref1,
        it_archkey2,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_bkpf_mkpf,
        it_bseg_a,
        it_bkpf_a,
        it_bset_a.


  REFRESH:it_archkey2[],
          it_rel_tab[],
          it_bkpf_mkpf[],
          it_bseg_a[],
          it_bkpf_a[],
          it_bset_a[].

*fetch only BKPF data from archive file
  g_bkpf_only = c_x.

  IF ( <fs_wa> IS ASSIGNED )    OR ( <fs_archivekey> IS ASSIGNED ) OR
     ( <fs_bukrs> IS ASSIGNED ) OR ( <fs_belnr> IS ASSIGNED ) OR
     ( <fs_gjahr> IS ASSIGNED ) OR ( <fs_archiveofs> IS ASSIGNED ).
*unasign field symbols
    UNASSIGN:<fs_wa>,
             <fs_archivekey>,
             <fs_bukrs>,
             <fs_belnr>,
             <fs_gjahr>,
            <fs_archiveofs>.
  ENDIF.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref.

  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[].

  IF ( <fs_value> IS ASSIGNED ).
    UNASSIGN: <fs_value>.
  ENDIF.
*Fetch the Archive infostructure details for the archiving object
  SELECT aind_str1~archindex
               aind_str1~itype
               aind_str1~otyp
               aind_str1~object
               aind_str2~active
               aind_str2~gentab
               FROM aind_str1 INNER JOIN aind_str2
               ON aind_str1~archindex = aind_str2~archindex
                INTO TABLE IT_INFSTR
               WHERE object EQ 'FI_DOCUMNT'
                AND  aind_str1~archindex EQ g_infstr2
                AND  aind_str1~itype EQ c_i
                AND aind_str1~otyp EQ c_o
                AND aind_str2~active EQ c_x.


  IF g_ref_bkpf IS INITIAL.
*Creation of Dynamic internal table.

    IF NOT it_infstr[] IS INITIAL.
      READ TABLE it_infstr WITH KEY archindex = g_infstr2.
    ENDIF.

    SELECT *
           FROM dd03l
           INTO TABLE it_dd03l
           WHERE tabname EQ it_infstr-gentab.
    IF NOT sy-subrc IS INITIAL.
*exit routine
      EXIT.
    ENDIF.

    SORT it_dd03l.

    LOOP AT it_dd03l.
      CLEAR ls_fieldcat.
      ls_fieldcat-row_pos         =  0.
      ls_fieldcat-col_pos         =  it_dd03l-position.
      ls_fieldcat-fieldname       =  it_dd03l-fieldname.
      ls_fieldcat-tabname         =  '1'.
      ls_fieldcat-datatype        =  it_dd03l-datatype.
      ls_fieldcat-reptext         =  'Type'.
      ls_fieldcat-outputlen       =  it_dd03l-leng.
      ls_fieldcat-coltext         = 'Type'.
      ls_fieldcat-key             = it_dd03l-keyflag.
      ls_fieldcat-emphasize       = 'C010'.
      APPEND ls_fieldcat TO it_fieldcatalog.
    ENDLOOP.
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = it_fieldcatalog
      IMPORTING
        ep_table        = g_ref_bkpf.
  ENDIF.

  ASSIGN g_ref_bkpf->* TO <fs_value>.


  READ TABLE it_infstr INDEX 1.

  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
    SELECT * FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
             WHERE bukrs EQ p_bukrs
               AND gjahr EQ p_gjahr
               AND monat EQ p_monat.

    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.
    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.

    IF NOT <fs_value>[] IS INITIAL.
      LOOP AT <fs_value> INTO <fs_wa>.

        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_wa> TO <fs_bukrs>.
        ASSIGN COMPONENT 'BELNR' OF STRUCTURE <fs_wa> TO <fs_belnr>.
        ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_wa> TO <fs_gjahr>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey2-bukrs = <fs_bukrs>.
        it_archkey2-belnr = <fs_belnr>.
        it_archkey2-gjahr = <fs_gjahr>.
        it_archkey2-arch_key = <fs_archivekey>.
        it_archkey2-arch_offset = <fs_archiveofs>.

        APPEND it_archkey2.
        CLEAR it_archkey2  .

*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'FI_DOCUMNT'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.
        IF sy-subrc IS INITIAL.
*Get Accounting Document header details
          PERFORM object_read_opened
                     TABLES it_rel_tab
                            it_coep_a
                            it_cobk_a
                            it_ekkn_a
                            it_ekbe_a
                            it_ekko_a
                            it_ekpo_a
                            it_bseg_a
                            it_bkpf_a
                            it_bset_a
                            it_vbpa_a
                            it_vbrk_a
                            it_vbrp_a
                            it_vbfa_a
                            it_konv_a
                     USING g_read_handle
                           l_archobj
                           g_commit_cnt
                           g_read_cnt
                           g_reload_cnt  .
        ENDIF.
*close the archive file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CLEAR g_bkpf_only.

ENDFORM.                    " get_zfr0acqr_arch_bkpf
*&---------------------------------------------------------------------*
*&      Form  get_zfr0acqr_arch_vbrk
*&---------------------------------------------------------------------*
*       Get Archive data from  Billing Document and Sales document
*----------------------------------------------------------------------*
FORM get_zfr0acqr_arch_vbrk  TABLES   it_vbrk_a STRUCTURE it_vbrk_a
                                      it_vbrp_a STRUCTURE it_vbrp_a
                                      it_vbfa_a STRUCTURE it_vbfa_a
                                      it_vbpa_a STRUCTURE it_vbpa_a
                                      it_konv_a STRUCTURE it_konv_a
                             USING    p_belnr.

  DATA:l_archobj LIKE arch_def-object VALUE 'SD_VBRK'.

  CLEAR:g_ref1,
        it_archkey4,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_vbrk_a,
        it_vbrp_a,
        it_vbfa_a,
        it_konv_a.

  REFRESH:it_archkey4[],
          it_rel_tab[],
          it_vbrk_a[],
          it_vbrp_a[],
          it_vbfa_a[],
          it_konv_a[].

  IF ( <fs_wa> IS ASSIGNED ) OR ( <fs_archivekey> IS ASSIGNED ) OR
     ( <fs_vbeln> IS ASSIGNED ) OR ( <fs_archiveofs> IS ASSIGNED ).
*unassign field symbols
    UNASSIGN:<fs_wa>,
             <fs_archivekey>,
             <fs_vbeln>,
             <fs_archiveofs>.
  ENDIF.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref.
  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[].

  IF ( <fs_value> IS ASSIGNED ).
    UNASSIGN: <fs_value>.
  ENDIF.

*Fetch the Archive infostructure details for the archiving object.

  SELECT aind_str1~archindex
          aind_str1~itype
          aind_str1~otyp
          aind_str1~object
          aind_str2~active
          aind_str2~gentab
          FROM aind_str1 INNER JOIN aind_str2
          ON aind_str1~archindex = aind_str2~archindex
           INTO TABLE IT_INFSTR
          WHERE object EQ 'SD_VBRK'
           AND  aind_str1~archindex EQ g_infstr4
           AND  aind_str1~itype EQ c_i
           AND aind_str1~otyp EQ c_o
           AND aind_str2~active EQ c_x.

  IF g_ref_vbrk IS INITIAL.
*Creation of Dynamic internal table..
    IF NOT it_infstr[] IS INITIAL.
      READ TABLE it_infstr WITH KEY archindex = g_infstr4.
    ENDIF.

    SELECT *
          FROM dd03l
          INTO TABLE it_dd03l
          WHERE tabname EQ it_infstr-gentab.
    IF NOT sy-subrc IS INITIAL.
*    exit routine
      EXIT.
    ENDIF.

    SORT it_dd03l.

    LOOP AT it_dd03l.
      CLEAR ls_fieldcat.
      ls_fieldcat-row_pos         =  0.
      ls_fieldcat-col_pos         =  it_dd03l-position.
      ls_fieldcat-fieldname       =  it_dd03l-fieldname.
      ls_fieldcat-tabname         =  '1'.
      ls_fieldcat-datatype        =  it_dd03l-datatype.
      ls_fieldcat-reptext         =  'Type'.
      ls_fieldcat-outputlen       =  it_dd03l-leng.
      ls_fieldcat-coltext         = 'Type'.
      ls_fieldcat-key             = it_dd03l-keyflag.
      ls_fieldcat-emphasize       = 'C010'.
      APPEND ls_fieldcat TO it_fieldcatalog.
    ENDLOOP.
*dynamic internal table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = it_fieldcatalog
      IMPORTING
        ep_table                  = g_ref_vbrk
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.
  ENDIF.

  ASSIGN g_ref_vbrk->* TO <fs_value>.

  READ TABLE it_infstr INDEX 1.
  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.

    SELECT * FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
             WHERE vbeln EQ p_belnr.


    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.

    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.
    IF NOT <fs_value>[] IS INITIAL.
      LOOP AT <fs_value> INTO <fs_wa>.


        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <fs_wa> TO <fs_vbeln>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey4-vbeln = <fs_vbeln>.
        it_archkey4-arch_key = <fs_archivekey>.
        it_archkey4-arch_offset = <fs_archiveofs>.

        APPEND it_archkey4.
        CLEAR it_archkey4.

*Get read handle for  object.

        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'SD_VBRK'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        IF sy-subrc IS INITIAL.
*Get Billing Document and Sales document details.

          PERFORM object_read_opened
                    TABLES it_rel_tab
                           it_coep_a
                           it_cobk_a
                           it_ekkn_a
                           it_ekbe_a
                           it_ekko_a
                           it_ekpo_a
                           it_bseg_a
                           it_bkpf_a
                           it_bset_a
                           it_vbpa_a
                           it_vbrk_a
                           it_vbrp_a
                           it_vbfa_a
                           it_konv_a
                    USING g_read_handle
                          l_archobj
                          g_commit_cnt
                          g_read_cnt
                          g_reload_cnt  .

        ENDIF.
*close the file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.


ENDFORM.                    " get_zfr0acqr_arch_vbrk
*&---------------------------------------------------------------------*
*&      Form  get_zfr0acqr_arch_vbpa
*&---------------------------------------------------------------------*
*       Get Archive data from Sales Document Partner
*----------------------------------------------------------------------*

FORM get_zfr0acqr_arch_vbpa  TABLES   it_vbpa_a STRUCTURE it_vbpa_a
                             USING     p_belnr.

  DATA:l_archobj LIKE arch_def-object VALUE 'SD_VBAK'.

  CLEAR:g_ref1,
        it_archkey3,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_vbpa_a.

  REFRESH:it_archkey3[],
          it_rel_tab[],
          it_vbpa_a[].

  IF ( <fs_wa> IS ASSIGNED ) OR ( <fs_archivekey> IS ASSIGNED ) OR
     ( <fs_vbeln> IS ASSIGNED ) OR ( <fs_archiveofs> IS ASSIGNED ).
*unassign field symbols
    UNASSIGN:<fs_wa>,
             <fs_archivekey>,
             <fs_vbeln>,
             <fs_archiveofs>.
  ENDIF.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref.
  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[].

  IF ( <fs_value> IS ASSIGNED ).
    UNASSIGN: <fs_value>.
  ENDIF.
*Fetch the Archive infostructure details for the archiving object
  SELECT   aind_str1~archindex
                aind_str1~itype
                aind_str1~otyp
                aind_str1~object
                aind_str2~active
                aind_str2~gentab
                FROM aind_str1 INNER JOIN aind_str2
                ON aind_str1~archindex = aind_str2~archindex
                 INTO TABLE IT_INFSTR
                WHERE object EQ 'SD_VBAK'
                 AND  aind_str1~archindex EQ g_infstr3
                 AND  aind_str1~itype EQ c_i
                 AND aind_str1~otyp EQ c_o
                 AND aind_str2~active EQ c_x.


  IF g_ref_vbap IS INITIAL.

*Creation of Dynamic internal table..
    IF NOT it_infstr[] IS INITIAL.
      READ TABLE it_infstr WITH KEY archindex = g_infstr3.
    ENDIF.

    SELECT *
           FROM dd03l
           INTO TABLE it_dd03l
           WHERE tabname EQ it_infstr-gentab.

    IF NOT sy-subrc IS INITIAL.
*exit routine
      EXIT.
    ENDIF.

    SORT it_dd03l.

    LOOP AT it_dd03l.
      CLEAR ls_fieldcat.
      ls_fieldcat-row_pos         =  0.
      ls_fieldcat-col_pos         =  it_dd03l-position.
      ls_fieldcat-fieldname       =  it_dd03l-fieldname.
      ls_fieldcat-tabname         =  '1'.
      ls_fieldcat-datatype        =  it_dd03l-datatype.
      ls_fieldcat-reptext         =  'Type'.
      ls_fieldcat-outputlen       =  it_dd03l-leng.
      ls_fieldcat-coltext         = 'Type'.
      ls_fieldcat-key             = it_dd03l-keyflag.
      ls_fieldcat-emphasize       = 'C010'.
      APPEND ls_fieldcat TO it_fieldcatalog.
    ENDLOOP.
*dynamic internal table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = it_fieldcatalog
      IMPORTING
        ep_table                  = g_ref_vbap
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.
  ENDIF.
  ASSIGN g_ref_vbap->* TO <fs_value>.

  READ TABLE it_infstr INDEX 1.
  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
    SELECT * FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
             WHERE vbeln EQ p_belnr
               AND posnr EQ '000000'.


    IF NOT sy-subrc IS INITIAL.
      EXIT.
    ENDIF.

    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.

    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.
*
    IF NOT <fs_value>[] IS INITIAL.
      LOOP AT <fs_value> INTO <fs_wa>.


        ASSIGN COMPONENT 'VBELN' OF STRUCTURE <fs_wa> TO <fs_vbeln>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey3-vbeln = <fs_vbeln>.
        it_archkey3-arch_key = <fs_archivekey>.
        it_archkey3-arch_offset = <fs_archiveofs>.

        APPEND it_archkey3.
        CLEAR it_archkey3.
*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'SD_VBAK'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        IF sy-subrc IS INITIAL.
*Get Sales Document Partner.
          PERFORM object_read_opened
                    TABLES it_rel_tab
                           it_coep_a
                           it_cobk_a
                           it_ekkn_a
                           it_ekbe_a
                           it_ekko_a
                           it_ekpo_a
                           it_bseg_a
                           it_bkpf_a
                           it_bset_a
                           it_vbpa_a
                           it_vbrk_a
                           it_vbrp_a
                           it_vbfa_a
                           it_konv_a
                    USING g_read_handle
                          l_archobj
                          g_commit_cnt
                          g_read_cnt
                          g_reload_cnt  .

        ENDIF.
*close the file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.


ENDFORM.                    " get_zfr0acqr_arch_vbpa
*&---------------------------------------------------------------------*
*&      Form  get_zfrovatr_arch_bkpf
*&---------------------------------------------------------------------*
*       Get Archive data from  Accounting Document header
*----------------------------------------------------------------------*
FORM get_zfrovatr_arch_bkpf  TABLES bukrs
                                    gjahr
                                    monat
                                   it_bkpf_a.
  DATA:l_archobj LIKE arch_def-object VALUE 'FI_DOCUMNT'.

  CLEAR:it_infstr,
        it_dd03l,
        ls_fieldcat,
        it_fieldcatalog,
        g_ref,
        g_ref1,
        it_archkey5,
        g_read_handle,
        g_commit_cnt,
        g_read_cnt,
        g_reload_cnt,
        it_rel_tab,
        it_bkpf_a,
        it_bseg_a.

  REFRESH:it_infstr[],
          it_dd03l[],
          it_fieldcatalog[],
          it_archkey5[],
          it_rel_tab[],
          it_bseg_a[],
          it_bkpf_a[].


*get only bkpf data from the file
  g_bkpf_only = c_x.

  IF ( <fs_value> IS ASSIGNED ) OR ( <fs_wa> IS ASSIGNED ) OR
    ( <fs_archivekey> IS ASSIGNED ) OR ( <fs_bukrs> IS ASSIGNED ) OR
    ( <fs_archiveofs> IS ASSIGNED ) OR ( <fs_gjahr> IS ASSIGNED ) OR
    ( <fs_monat> IS ASSIGNED ).
*unassign field symbols
    UNASSIGN:<fs_value>,
             <fs_wa>,
             <fs_archivekey>,
             <fs_bukrs>,
             <fs_gjahr>,
             <fs_monat>,
             <fs_archiveofs>.
  ENDIF.
*Fetch the Archive infostructure details for the archiving object
  SELECT aind_str1~archindex
         aind_str1~itype
         aind_str1~otyp
         aind_str1~object
         aind_str2~active
         aind_str2~gentab
         FROM aind_str1 INNER JOIN aind_str2
         ON aind_str1~archindex = aind_str2~archindex
          INTO TABLE IT_INFSTR
         WHERE object EQ 'FI_DOCUMNT'
          AND  aind_str1~archindex EQ g_infstr2
          AND  aind_str1~itype EQ c_i
          AND aind_str1~otyp EQ c_o
          AND aind_str2~active EQ c_x.


*Creation of Dynamic internal table.

  IF NOT it_infstr[] IS INITIAL.
    READ TABLE it_infstr WITH KEY archindex = g_infstr2.
  ENDIF.

  SELECT *
        FROM dd03l
        INTO TABLE it_dd03l
        WHERE tabname EQ it_infstr-gentab.

  IF NOT sy-subrc IS INITIAL.
*exit routine
    EXIT.
  ENDIF.

  SORT it_dd03l.

  LOOP AT it_dd03l.
    CLEAR ls_fieldcat.
    ls_fieldcat-row_pos         =  0.
    ls_fieldcat-col_pos         =  it_dd03l-position.
    ls_fieldcat-fieldname       =  it_dd03l-fieldname.
    ls_fieldcat-tabname         =  '1'.
    ls_fieldcat-datatype        =  it_dd03l-datatype.
    ls_fieldcat-reptext         =  'Type'.
    ls_fieldcat-outputlen       =  it_dd03l-leng.
    ls_fieldcat-coltext         = 'Type'.
    ls_fieldcat-key             = it_dd03l-keyflag.
    ls_fieldcat-emphasize       = 'C010'.
    APPEND ls_fieldcat TO it_fieldcatalog.
  ENDLOOP.
*dynamic internal table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_fieldcatalog
    IMPORTING
      ep_table        = g_ref.

  ASSIGN g_ref->* TO <fs_value>.

  READ TABLE it_infstr INDEX 1.

  IF <fs_value> IS ASSIGNED.
* Fetch  data from  infostructure.
    SELECT * FROM (it_infstr-gentab)
             INTO CORRESPONDING FIELDS OF TABLE <fs_value>
             WHERE bukrs IN bukrs
               AND gjahr IN gjahr
               AND awtyp EQ 'VBRK'
               AND monat IN monat.

    CLEAR:g_object_cnt,
          g_duprec,
          g_read_cnt,
          g_reload_cnt.

    READ TABLE it_infstr INDEX 1.

    CREATE DATA g_ref1 LIKE LINE OF <fs_value>.
    ASSIGN g_ref1->* TO <fs_wa>.

    IF NOT <fs_value>[] IS INITIAL.

      LOOP AT <fs_value> INTO <fs_wa>.

        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_wa> TO <fs_bukrs>.
        ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_wa> TO <fs_gjahr>.
        ASSIGN COMPONENT 'MONAT' OF STRUCTURE <fs_wa> TO <fs_monat>.
        ASSIGN COMPONENT 'ARCHIVEKEY' OF STRUCTURE <fs_wa> TO
        <fs_archivekey>.
        ASSIGN COMPONENT 'ARCHIVEOFS' OF STRUCTURE <fs_wa> TO
        <fs_archiveofs>.

        it_archkey5-bukrs = <fs_bukrs>.
        it_archkey5-gjahr = <fs_gjahr>.
        it_archkey5-monat = <fs_monat>.
        it_archkey5-arch_key = <fs_archivekey>.
        it_archkey5-arch_offset = <fs_archiveofs>.

        APPEND it_archkey5.
        CLEAR it_archkey5.
*Get read handle for  object
        CALL FUNCTION 'ARCHIVE_READ_OBJECT'
          EXPORTING
            object         = 'FI_DOCUMNT'
            archivkey      = <fs_archivekey>
            offset         = <fs_archiveofs>
          IMPORTING
            archive_handle = g_read_handle
          EXCEPTIONS
            OTHERS         = 1.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        IF sy-subrc IS INITIAL.
*Get Accounting Document header details
          PERFORM object_read_opened
                     TABLES it_rel_tab
                            it_coep_a
                            it_cobk_a
                            it_ekkn_a
                            it_ekbe_a
                            it_ekko_a
                            it_ekpo_a
                            it_bseg_a
                            it_bkpf_a
                            it_bset_a
                            it_vbpa_a
                            it_vbrk_a
                            it_vbrp_a
                            it_vbfa_a
                            it_konv_a
                     USING g_read_handle
                           l_archobj
                           g_commit_cnt
                           g_read_cnt
                           g_reload_cnt   .

        ENDIF.
*close the file.
        PERFORM archive_close_file USING g_read_handle.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CLEAR g_bkpf_only.

ENDFORM.                    " get_zfrovatr_arch_bkpf
*&---------------------------------------------------------------------*
*&      Form  archive_close_file
*&---------------------------------------------------------------------*
*   close the archive file
*----------------------------------------------------------------------*
FORM archive_close_file  USING p_handle.
*close file
  CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
    EXPORTING
      archive_handle          = p_handle
    EXCEPTIONS
      internal_error          = 1
      wrong_access_to_archive = 2
      OTHERS                  = 3.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE i004." 'Unable to close archive session'.
    EXIT.
  ENDIF.
ENDFORM.                    " archive_close_file
*&---------------------------------------------------------------------*
*&      Form  convert_regio_internal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_regio_internal .
** Need to convert state to numeric format
  LOOP AT regio.
    CASE regio-low.
      WHEN 'AL'.
        regio-low = '01'.
      WHEN 'AK'.
        regio-low = '02'.
      WHEN 'AZ'.
        regio-low = '04'.
      WHEN 'AR'.
        regio-low = '05'.
      WHEN 'CA'.
        regio-low = '06'.
      WHEN 'CO'.
        regio-low = '08'.
      WHEN 'CT'.
        regio-low = '09'.
      WHEN 'DE'.
        regio-low = '10'.
*      WHEN 'DC'.
*        regio-low = '09'.
      WHEN 'FL'.
        regio-low = '12'.
      WHEN 'GA'.
        regio-low = '13'.
      WHEN 'HI'.
        regio-low = '15'.
      WHEN 'ID'.
        regio-low = '16'.
      WHEN 'IL'.
        regio-low = '17'.
      WHEN 'IN'.
        regio-low = '18'.
      WHEN 'IA'.
        regio-low = '19'.
      WHEN 'KS'.
        regio-low = '20'.
      WHEN 'KY'.
        regio-low = '21'.
      WHEN 'LA'.
        regio-low = '22'.
      WHEN 'ME'.
        regio-low = '23'.
      WHEN 'MD'.
        regio-low = '24'.
      WHEN 'MA'.
        regio-low = '25'.
      WHEN 'MI'.
        regio-low = '26'.
      WHEN 'MN'.
        regio-low = '27'.
      WHEN 'MS'.
        regio-low = '28'.
      WHEN 'MO'.
        regio-low = '29'.
      WHEN 'MT'.
        regio-low = '30'.
      WHEN 'NE'.
        regio-low = '31'.
      WHEN 'NV'.
        regio-low = '32'.
      WHEN 'NH'.
        regio-low = '33'.
      WHEN 'NJ'.
        regio-low = '34'.
      WHEN 'NM'.
        regio-low = '35'.
      WHEN 'NY'.
        regio-low = '36'.
      WHEN 'NC'.
        regio-low = '37'.
      WHEN 'ND'.
        regio-low = '38'.
      WHEN 'OH'.
        regio-low = '39'.
      WHEN 'OK'.
        regio-low = '40'.
      WHEN 'OR'.
        regio-low = '41'.
      WHEN 'PA'.
        regio-low = '42'.
      WHEN 'RI'.
        regio-low = '44'.
      WHEN 'SC'.
        regio-low = '45'.
      WHEN 'SD'.
        regio-low = '46'.
      WHEN 'TN'.
        regio-low = '47'.
      WHEN 'TX'.
        regio-low = '48'.
      WHEN 'UT'.
        regio-low = '49'.
      WHEN 'VT'.
        regio-low = '50'.
      WHEN 'VA'.
        regio-low = '51'.
      WHEN 'WA'.
        regio-low = '53'.
      WHEN 'WV'.
        regio-low = '54'.
      WHEN 'WI'.
        regio-low = '55'.
      WHEN 'WY'.
        regio-low = '56'.
*      WHEN 'CN'.
*        regio-low = '70'.
*      WHEN 'PR'.
*        regio-low = '72'.
*      WHEN 'VI'.
*        regio-low = '78'.
    ENDCASE.
    MODIFY regio.
  ENDLOOP.

ENDFORM.                    " convert_regio_internal
*&---------------------------------------------------------------------*
*&      Form  MERGE_ITEM_BSIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM merge_item_bsis .
  READ TABLE item_bsis INTO item_bsis_wa WITH TABLE KEY
       bukrs = item-bukrs
       belnr = item-belnr
       gjahr = item-gjahr
       buzei = item-buzei.

  IF sy-subrc = 0.
    item-rec_type = 'I'.
    item-glamt = item_bsis_wa-wrbtr.
    IF item_bsis_wa-shkzg = 'H'.
      item-glamt = item-glamt * -1.
    ENDIF.
    MODIFY item.
    DELETE TABLE item_bsis WITH TABLE KEY
       bukrs = item-bukrs
       belnr = item-belnr
       gjahr = item-gjahr
       buzei = item-buzei.
  ELSE.
    item-rec_type = 'I'.
    MODIFY item.
  ENDIF.

ENDFORM.                    " MERGE_ITEM_BSIS

*&---------------------------------------------------------------------*
*&      Form  build_spool_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_spool_list.

  data: or_doc  type ref to cl_gui_docking_container .

  PERFORM alv_fieldcat CHANGING it_fieldcat.

  variant-report = sy-repid.
  CREATE OBJECT grid1
    EXPORTING
      i_parent = or_doc.
  CALL METHOD grid1->set_table_for_first_display
    EXPORTING
      i_structure_name = 'ZTAX_VALIDATE1'
      is_variant       = variant
      i_save           = 'A'
      is_layout        = gs_layout
    CHANGING
      it_outtab        = alv_grid
      it_fieldcatalog  = it_fieldcat.

ENDFORM.                    " build_spool_list
*eject
*&---------------------------------------------------------------------*
*&      Form  f_string_remove_spcl_char                     "DECK914346
*&---------------------------------------------------------------------*
*       Remove special, non-printable, characters from a string
*----------------------------------------------------------------------*
FORM f_string_remove_spcl_char                              "DECK914346
  CHANGING cv_string      TYPE STRING.

  DATA:  lv_i             TYPE syindex,
         lv_j             TYPE syindex,
         lv_k             TYPE syindex,
         lv_char(1)       TYPE c,
         lv_string_i(250) TYPE c,
         lv_string_o(250) TYPE c.

  IF ( cv_string IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lv_string_i.
  MOVE     cv_string                TO lv_string_i.

  CLEAR    lv_string_o.

  lv_i   = STRLEN( lv_string_i ).
  lv_j   = 0.
  lv_k   = 0.

  DO       lv_i TIMES.

    CLEAR                              lv_char.
    MOVE     lv_string_i+lv_k(1)    TO lv_char.
    ADD      1                      TO lv_k.

    IF   ( ( lv_char                GE ' ' ) AND
           ( lv_char                LE '~' )     ).
      MOVE   lv_char                TO lv_string_o+lv_j(1).
      ADD    1                      TO lv_j.
    ENDIF.

  ENDDO.

  CLEAR                                cv_string.
  MOVE     lv_string_o              TO cv_string.

ENDFORM.                    " f_string_remove_spcl_char     "DECK914346
