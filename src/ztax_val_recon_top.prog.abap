*----------------------------------------------------------------------*
***INCLUDE ZSJB_VALIDATION_TOP .
*----------------------------------------------------------------------*

TABLES: bsak, rbkp, bseg, bkpf, rbco, t001w, com_err, t001, com_tax, ttxp,"#EC *
        ttxd, lfa1, rseg, csks, t007a, ekpo, tcurx, t005, t100,"#EC *
        bset, skb1, aufk, anlz, prps, rbma, bvor, bsis.           "#EC *

* replaces external table ztax_validate
TYPES: BEGIN OF ztax_validate1_i,
        bukrs like bkpf-bukrs,      " Posting Company Code
        vbund like bseg-vbund,      " Purchasing CC  "somsx
        belnr TYPE belnr_d,         " Accounting Document Number
        gjahr TYPE gjahr,           " Fiscal Year
        buzei TYPE buzei,           " Number of Line Item Within Accounting Document
        blart TYPE blart,           " Document type
        belnri TYPE re_belnr,       " Document number of an invoice document
        buzeii TYPE rblgp,          " Document item in invoice document
        xblnr TYPE xblnr,           " Reference Document Number
        budat TYPE budat,           " Posting Date in the Document
        grdat TYPE budat,           " Goods Receipt Posting Date
        lifnr TYPE lifnr,           " Account Number of Vendor or Creditor
        name1 TYPE name1_gp,        " Vendor Name
        vend_regio TYPE regio,      " Vendor State
        usnam TYPE usnam,           " User Name
        waers TYPE waers,           " Currency
        kursf TYPE kursf,           " Exchange rate
        ffact TYPE ffact_curr,      " Currency Conversion Factor
        rmwwr TYPE rmwwr,           " Gross invoice amount in document currency
        wrbtr TYPE wrbtr,           " Amount in document currency
        bnkan_fw TYPE bnk_anteil,   " Delivery costs' share of item value
        irout TYPE wrbtr,           " GR/IR Adjustment
        glbtr TYPE wrbtr,           " Reconciliation Amt
        zterm_text TYPE text1,      " Payment Terms
        netdt TYPE netdt,           " Due Date for Net Payment
        menge TYPE menge_d,         " Quantity
        bstme TYPE bstme,           " Order Unit
        txjcd TYPE txjcd,           " Jurisdiction for Tax Calculation - Tax Jurisdiction Code
        regio LIKE lfa1-regio,      " Region (State, Province, County)
        saknr TYPE saknr,           " G/L Account Number
        txt20 TYPE txt20_skat,      " G/L account short text
        xmwst TYPE xmwst,           " Calculate tax automatically
        mwskz TYPE mwskz,           " Tax on Sales/Purchases Code
        mwskz_po TYPE mwskz,        " Tax Code PO
        knttp TYPE knttp,           " Account assignment category
        kostl TYPE kostl,           " Cost Center
        ktext TYPE ktext,           " Cost Center Name
        aufnr TYPE aufnr,           " Order Number
        auart type aufart,          " Order Type
        nplnr type nplnr,           " Network
        ktext_aufk type AUFTEXT,    " Order Description
        anln1 TYPE anln1,           " Main Asset Number
        anln2 TYPE anln2,           " Asset Subnumber
        ps_psp_pnr TYPE ps_psp_pnr, " WBS
        kosar TYPE kosar,           " Cost Center Category
        kosar_txt TYPE dxstext,     " CostCenterCatDesc
        kokrs TYPE kokrs,           " Controlling Area
        vbeln type vbeln_va,        " Sales Order
        vbelp type posnr_va,        " Sales Order line Item
        txjcd_so type txjcd,        " Ship-to jurisdiction from Sales Order
        ebeln TYPE ebeln,           " PO Number
        ebelp TYPE ebelp,           " PO Line Number
        werks TYPE werks_d,         " Plant
        name1_w type name1,         " Plant name
        lgort TYPE lgort_d,         " Storage Location
        adrst TYPE pbr_rstre,       " Ship-To Adress
        wepos TYPE wepos,           " Goods Receipt Indicator
        matnr TYPE matnr,           " Material Number
        matkl TYPE matkl,           " Material Group
        wgbez TYPE wgbez,           " Description of material group
        txz01 TYPE txz01,           " Short text
        act_tax TYPE wmwst,         " Posted Tax Amount
        act_tax_1 TYPE wmwst,       " Posted State Tax Amount
        act_tax_2 TYPE wmwst,       " Posted County Tax
        act_tax_3 TYPE wmwst,       " Posted City Tax
        act_tax_4 TYPE wmwst,       " Posted Secondary Tax
        act_tax_rate TYPE lhpc1,    " Actual Tax Rate
        act_tax_rate_po TYPE lhpc2, " Tax Rate PO
        cal_tax TYPE wert8,         " Calculated Tax Amount
        tax_dif TYPE hwsdiff,       " Tax Difference Amout
*
        arc_doc_id like toa01-arc_doc_id,
        archiv_id like toa01-archiv_id,
        sap_object like toa01-sap_object,
        object_id like toa01-object_id,
        ar_object like toa01-ar_object,
        bktxt like bkpf-bktxt,
        sgtxt_ven like bseg-sgtxt,
        sgtxt like bseg-sgtxt,
        augdt like bseg-augdt,
        augbl like bseg-augbl,
        chect like payr-chect,
        bldat type bldat,            " Document Date
        prctr like bseg-prctr,
        grdoc like ekbe-belnr,
        grmng like ekbe-menge,
        grjah like ekbe-gjahr,
        rec_type,                   "Record Type (Invoice/GL)
        glamt like bseg-wrbtr,
        bvorg like bkpf-bvorg,
*        zzloc like bseg-zzloc,      "Location Code
        bbukrs like bseg-bukrs,    "GL company Code
        DESC40_N like zapt_location-DESC40_N, "Location Description
      END OF ztax_validate1_i.

DATA: ztax_validate1 TYPE STANDARD TABLE OF ztax_validate1_i WITH HEADER LINE."#EC *

types: begin of item_line,
       bukrs LIKE bkpf-bukrs,
       vbund like bseg-vbund,  "somsx
       belnr LIKE bkpf-belnr,
       gjahr LIKE bkpf-gjahr,
       buzei LIKE bseg-buzei,
       blart LIKE bkpf-blart,
       belnri LIKE rbkp-belnr,
       buzeii LIKE rseg-buzei,
       budat LIKE bkpf-budat,
       bldat like bkpf-bldat,
       lifnr LIKE bsak-lifnr,
       name1 LIKE lfa1-name1,
       vend_regio LIKE lfa1-regio,
       waers LIKE bsak-waers,
       kursf LIKE bkpf-kursf,
       ffact LIKE tcurf-ffact,
       awtyp LIKE bkpf-awtyp,
       awkey LIKE bkpf-awkey,
       rmwwr TYPE wt_wt,               "LIKE rbkp-rmwwr,    "DECK914334
       wmwst1 TYPE wt_wt,              "LIKE rbkp-wmwst1,   "DECK914334
       wrbtr TYPE wt_wt,               "LIKE rbco-wrbtr,    "DECK914334
       wrbtr_sav TYPE wt_wt,           "LIKE rbco-wrbtr,    "DECK914334
       irout TYPE wt_wt,               "LIKE rbco-wrbtr,    "DECK914334
       glbtr TYPE wt_wt,               "LIKE rbco-wrbtr,    "DECK914334
       beznk LIKE rbkp-beznk,
       bnkan_fw LIKE rseg-bnkan,
       menge LIKE rbco-menge,
       bstme LIKE rseg-bstme,
       txjcd LIKE rbco-txjcd,
       saknr LIKE rbco-saknr,
       saknr_orig LIKE rbco-saknr,
       shkzg LIKE bseg-shkzg,
       mwskz LIKE rbco-mwskz,
       xmwst LIKE bkpf-xmwst,
       knttp LIKE ekpo-knttp,
       kostl LIKE rbco-kostl,
       aufnr LIKE bseg-aufnr,
       auart like aufk-auart,
       nplnr like bseg-nplnr,
       ktext_aufk like aufk-ktext,
       anln1 LIKE rbco-anln1,
       anln2 LIKE rbco-anln2,
       ps_psp_pnr LIKE rbco-ps_psp_pnr,
       kosar LIKE csks-kosar,
       kosar_txt LIKE tkt05-ktext,
       kokrs LIKE rbco-kokrs,
       vbeln like bseg-vbel2,
       vbelp like bseg-posn2,
       txjcd_so like adrc-taxjurcode,
       ebeln LIKE rseg-ebeln,
       ebelp LIKE rseg-ebelp,
       zekkn LIKE bseg-zekkn,
       werks LIKE rseg-werks,
       name1_w like t001w-name1,
       lgort LIKE ekpo-lgort,
       matnr LIKE ekpo-matnr,
       matkl LIKE ekpo-matkl,
       txz01 LIKE ekpo-txz01,
       usnam LIKE bkpf-usnam,
       bvorg LIKE bkpf-bvorg,
       act_tax TYPE wt_wt,             "LIKE rbkp-wmwst1,   "DECK914334
       cal_tax TYPE wt_wt,             "LIKE rbkp-wmwst1,   "DECK914334
       tax_dif TYPE wt_wt,             "LIKE rbkp-wmwst1,   "DECK914334
       adrst LIKE ztax_validate1-adrst,
       ktext LIKE cskt-ktext,
       wgbez LIKE t023t-wgbez,
       act_tax_rate_po LIKE ztax_validate1-act_tax_rate_po,
       mwskz_po LIKE ekpo-mwskz,
       mwskz_ir LIKE rbco-mwskz,
       act_tax_1 LIKE ztax_validate1-act_tax_1,
       act_tax_2 LIKE ztax_validate1-act_tax_2,
       act_tax_3 LIKE ztax_validate1-act_tax_3,
       act_tax_4 LIKE ztax_validate1-act_tax_4,
       wepos LIKE ekpo-wepos,
       xblnr LIKE rbkp-xblnr,
       zterm_text LIKE ztax_validate1-zterm_text,
       netdt LIKE ztax_validate1-netdt,
       adrnr LIKE ekpo-adrnr,
       adrn2 LIKE ekpo-adrn2,
       zbd1t LIKE rbkp-zbd1t,
       zbd2t LIKE rbkp-zbd2t,
       zbd3t LIKE rbkp-zbd3t,
       zbd1p LIKE rbkp-zbd1p,
       zbd2p LIKE rbkp-zbd2p,
       zfbdt LIKE rbkp-zfbdt,
       rebzg LIKE rbkp-rebzg,
       faede_shkzg LIKE bseg-shkzg,
       grdat LIKE ekbe-budat,
       grdoc like ekbe-belnr,
       grjah like ekbe-gjahr,
       grmng like ekbe-menge,
       tcode LIKE bkpf-tcode,
       tot_tax TYPE wt_wt,   "LIKE rbkp-wmwst1, "total tax  "DECK914334
       tot_tax_1 LIKE ztax_validate1-act_tax_1,
       tot_tax_2 LIKE ztax_validate1-act_tax_2,
       tot_tax_3 LIKE ztax_validate1-act_tax_3,
       tot_tax_4 LIKE ztax_validate1-act_tax_4,
       arc_doc_id like toa01-arc_doc_id,
       archiv_id like toa01-archiv_id,
       sap_object like toa01-sap_object,
       object_id like toa01-object_id,
       ar_object like toa01-ar_object,
       bktxt like bkpf-bktxt,
       sgtxt_ven like bseg-sgtxt,
       sgtxt like bseg-sgtxt,
       augdt like bseg-augdt,
       augbl like bseg-augbl,
       auggj like payr-gjahr,
       chect like payr-chect,
       prctr like bseg-prctr,
       rec_type,
       glamt TYPE wt_wt,               "LIKE bseg-wrbtr,    "DECK914334
       buzid like bseg-buzid,
*       zzloc like bseg-zzloc,
       bbukrs like bseg-bukrs,
       desc40_n like zapt_location-desc40_n,
      END OF item_line.

data: item type standard table of item_line with header line.

data: item_bsis type hashed table of item_line with unique key
      bukrs
      belnr
      gjahr
      buzei.

data: item_bsis_wa type item_line.

DATA: BEGIN OF inv_items OCCURS 0,
        wrbtr LIKE rseg-wrbtr,
        txjcd LIKE rseg-txjcd,
        saknr LIKE rbco-saknr,
        mwskz LIKE rbco-mwskz,
        kostl LIKE rbco-kostl,
        kokrs LIKE rbco-kokrs,
        menge LIKE rbco-menge,
        buzei LIKE rseg-buzei,
        cobl_nr LIKE rbco-cobl_nr,
        aufnr LIKE rbco-aufnr,
        nplnr like rbco-nplnr,
        vbeln like rbco-vbeln,
        vbelp like rbco-vbelp,
        bnkan_fw LIKE rbco-bnkan_fw,
        bnkan LIKE rseg-bnkan,
        anln1 LIKE rbco-anln1,
        anln2 LIKE rbco-anln2,
        ps_psp_pnr LIKE rbco-ps_psp_pnr,
        ebeln LIKE rseg-ebeln,
        ebelp LIKE rseg-ebelp,
        werks LIKE rseg-werks,
        bstme LIKE rseg-bstme,
        zekkn LIKE rseg-zekkn,
        shkzg LIKE rseg-shkzg,
      END OF inv_items.

DATA: BEGIN OF inv_sum OCCURS 0,
        mwskz LIKE bseg-mwskz,
        txjcd LIKE bseg-txjcd,
        wrbtr LIKE bset-hwbas,
        bnkan_fw LIKE rseg-bnkan,
      END OF inv_sum.

DATA: BEGIN OF navfw_tab OCCURS 0,
        txgrp(3),
        navfw LIKE bseg-navfw,
      END OF navfw_tab.

DATA: BEGIN OF document OCCURS 0,                           "#EC *
       bukrs LIKE bsak-bukrs,                               "#EC *
       belnr LIKE bsak-belnr,                               "#EC *
       gjahr LIKE bsak-gjahr,                               "#EC *
      END OF document.                                      "#EC *

DATA: BEGIN OF bseg_tab OCCURS 0.
        INCLUDE STRUCTURE bseg.
DATA: END OF bseg_tab.

DATA: BEGIN OF bset_tab OCCURS 0.
        INCLUDE STRUCTURE bset.
DATA: END OF bset_tab.

DATA: BEGIN OF bseg_sav OCCURS 0.                           "#EC *
        INCLUDE STRUCTURE bseg.                             "#EC *
DATA: END OF bseg_sav.                                      "#EC *

DATA: BEGIN OF ekbe_tab OCCURS 0,
         ebeln like ekbe-ebeln,
         shkzg LIKE ekbe-shkzg,
         wrbtr LIKE ekbe-wrbtr,
         arewr LIKE ekbe-arewr,
         menge LIKE ekbe-menge,
         budat LIKE ekbe-budat,
         belnr LIKE ekbe-belnr,
         gjahr LIKE ekbe-gjahr,
         matnr LIKE ekbe-matnr,
         bukrs LIKE ekko-bukrs,
*        INCLUDE STRUCTURE ekbe.
*DATA:   augbl LIKE bseg-augbl,
*        augdt LIKE bseg-augdt.
*DATA: END OF ekbe_tab.
    END OF ekbe_tab.


TYPES: BEGIN OF ekbe_excl_line,
        belnr LIKE ekbe-belnr,
        buzei LIKE ekbe-buzei,
      END OF ekbe_excl_line.

DATA: BEGIN OF INT01 OCCURS 0,
        LINE(1400) TYPE C,
      END OF INT01.

DATA: ekbe_excl TYPE HASHED TABLE OF ekbe_excl_line WITH UNIQUE KEY
belnr buzei.                                                "#EC *

DATA: ekbe_excl_wa TYPE ekbe_excl_line.                     "#EC *

DATA: BEGIN OF messtab OCCURS 10.                           "#EC *
        INCLUDE STRUCTURE bdcmsgcoll.                       "#EC *
DATA: END OF messtab.                                       "#EC *

DATA: BEGIN OF error_tab OCCURS 0,                          "#EC *
       text(200),                                           "#EC *
      END OF error_tab.                                     "#EC *

DATA: BEGIN OF process_tab OCCURS 0,                        "#EC *
       text(200),                                           "#EC *
      END OF process_tab.                                   "#EC *

DATA: BEGIN OF bdc_tab OCCURS 30.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdc_tab.

DATA: conv_curr TYPE i.                                     "#EC *

DATA: V_LENGTH(10).
DATA: adrnr LIKE vbpa-adrnr,                                "#EC *
      ktopl LIKE t001-ktopl,
      kwert LIKE konv-kwert,                                "#EC *
      wrbtr LIKE rbco-wrbtr,                                "#EC *
      abs_wrbtr LIKE rbco-wrbtr,
      info_msg(50),
      abs_fwbas LIKE bset-fwbas,
      bnkan_fw LIKE rbco-bnkan_fw,                          "#EC *
      tax   LIKE konv-kwert,                                "#EC *
      txjcd LIKE adrc-taxjurcode,                           "#EC *
      ibeln TYPE i,                                         "#EC *
      run_mode,                                             "#EC *
      lin TYPE i,
      lin1 type i,
      lin2 type i,
      lin3 type i,
      count TYPE i,                                         "#EC *
      amt_tmp LIKE rbkp-wmwst1,                             "#EC *
      tax_base LIKE rbco-wrbtr,                             "#EC *
      dif_tax LIKE rbco-wrbtr,                              "#EC *
      dif_total LIKE rbco-wrbtr,                            "#EC *
      error,                                                "#EC *
      tax_pct TYPE p DECIMALS 3,                            "#EC *
      len TYPE i,                                           "#EC *
      accrual_cnt TYPE i,                                   "#EC *
      process_cnt TYPE i,                                   "#EC *
      error_cnt TYPE i,                                     "#EC *
      accrual_attempt_cnt TYPE i,                           "#EC *
      accrual_attempt_amt LIKE rbco-wrbtr,                  "#EC *
      process_attempt_cnt TYPE i,                           "#EC *
      process_total_cnt TYPE i,                             "#EC *
      accrual_amt LIKE rbco-wrbtr,                          "#EC *
      process_op_cnt TYPE i,                                "#EC *
      process_op_amt LIKE rbco-wrbtr,                       "#EC *
      bukrs_posting LIKE t001-bukrs,                        "#EC *
      txjcd_temp LIKE bset-txjcd,
      inv_net LIKE rseg-wrbtr,
      diff LIKE rseg-wrbtr,
      txgrp LIKE bset-txgrp,
      txgrp_sav LIKE bset-txgrp,
      t_bukrs LIKE bkpf-bukrs,
      t_belnr LIKE bkpf-belnr,
      g_repid LIKE sy-repid,
      g_max TYPE i VALUE 100,                               "#EC *
      gs_layout   TYPE lvc_s_layo,
      gs_toolbar  TYPE stb_button,
      tolov LIKE bseg-wrbtr,
      tolun LIKE bseg-wrbtr,
      bset_tax LIKE rbkp-wmwst1,
      mrko_bukrs LIKE bkpf-bukrs,
      mrko_belnr LIKE bkpf-belnr,
      mrko_gjahr LIKE bkpf-gjahr,
      mrko_buzei(4),                                        "#EC *
      txjcd_old  LIKE bset-txjcd,
      navfw_tab_lines TYPE n,
      acc_ass_not_found,                                    "#EC *
      kalsm LIKE t005-kalsm,
      new_tax_tot TYPE wt_wt,          "LIKE rbkp-wmwst1,   "DECK914334
      old_tax_tot TYPE wt_wt,          "LIKE rbkp-wmwst1,   "DECK914334
      answer,
      accrual_flag,
      vendor_lines type i,
      glamt like bseg-wrbtr,
      glacc like bseg-hkont,
      ebeln_recon like bseg-ebeln,
      ebelp_recon like bseg-ebelp,
      shkzg_recon like bseg-shkzg,
      buzid like bseg-buzid.

************************************************************************
*       Data Declaration for RFC_CALCULATE_TAXES_DOC
************************************************************************

DATA: i_sap_control_data LIKE sap_control_data.
DATA: i_tax_cal_head_in LIKE tax_cal_head_in00.
DATA: BEGIN OF i_tax_cal_item_in OCCURS 0.
        INCLUDE STRUCTURE tax_cal_item_in00.
DATA: END OF i_tax_cal_item_in.
DATA: BEGIN OF o_tax_cal_item_out OCCURS 0.
        INCLUDE STRUCTURE tax_cal_item_out00.
DATA: END OF o_tax_cal_item_out.
DATA: BEGIN OF o_tax_cal_jur_level_out OCCURS 0.
        INCLUDE STRUCTURE tax_cal_jur_level_out00.
DATA: END OF o_tax_cal_jur_level_out.
DATA: o_ext_control_data LIKE ext_control_data.             "#EC *
DATA: o_com_err_doc LIKE com_err_doc.

***

DATA: alv_grid TYPE TABLE OF ztax_validate1_i.
DATA: ok_code LIKE sy-ucomm,
      g_container TYPE scrfname VALUE 'ZTAX_VALIDATE_CONT1',
      grid1  TYPE REF TO cl_gui_alv_grid,
      variant TYPE disvariant,
      g_custom_container TYPE REF TO cl_gui_custom_container.
CLASS lcl_event_receiver DEFINITION DEFERRED.
INCLUDE <icon>.
DATA: event_receiver TYPE REF TO lcl_event_receiver.        "#EC *
DATA: it_fieldcat TYPE lvc_t_fcat.
DATA: ls_fieldcat TYPE lvc_s_fcat.


*
* Constants
*
DATA: dialog VALUE ' '.    "#EC *      "Set to X to post in dialog mode


************************
** Archive access
************************
************************************************************************
*                                TABLES                                *
************************************************************************
TYPE-POOLS: slis.
************************************************************************
*                            INTERNAL TABLES                           *
************************************************************************

DATA: BEGIN OF it_onkrs OCCURS 0,
         objnr LIKE onrks-objnr,
         kostl LIKE onrks-kostl,
         aufnr LIKE onror-aufnr.
DATA: END OF it_onkrs.

DATA: BEGIN OF it_coep_a OCCURS 0.
        INCLUDE STRUCTURE coep.
DATA: END OF it_coep_a.

DATA: BEGIN OF it_cobk_a OCCURS 0.
        INCLUDE STRUCTURE cobk.
DATA: END OF it_cobk_a.

DATA: BEGIN OF it_covp OCCURS 0,
        objnr LIKE covp-objnr,
        kstar LIKE covp-kstar,
        belnr LIKE covp-belnr,
        ebeln LIKE covp-ebeln,
        ebelp LIKE covp-ebelp,
        gjahr LIKE covp-gjahr,
        refbn LIKE covp-refbn,
        blart LIKE covp-blart,
        budat LIKE covp-budat,
        bldat LIKE covp-bldat,
        buzei LIKE covp-buzei,
        wkgbtr LIKE covp-wkgbtr,
        wogbtr LIKE covp-wogbtr,
        owaer  LIKE covp-owaer,
        bukrs LIKE covp-bukrs,
        awtyp LIKE covp-awtyp,
        aworg LIKE covp-aworg,
        refbk LIKE covp-refbk,
        refgj LIKE covp-refgj,
        sgtxt LIKE covp-sgtxt,
        perio LIKE covp-perio,
        refbz LIKE coep-refbz.
DATA: END OF it_covp.

DATA: BEGIN OF it_ekkn_a OCCURS 0.
        INCLUDE STRUCTURE ekkn.
DATA: END OF it_ekkn_a.

DATA: BEGIN OF it_ekbe_a OCCURS 0.
        INCLUDE STRUCTURE ekbe.
DATA: END OF it_ekbe_a.

DATA: BEGIN OF it_ekko_a OCCURS 0.
        INCLUDE STRUCTURE ekko.
DATA: END OF it_ekko_a.

DATA: BEGIN OF it_ekpo_a OCCURS 0.
        INCLUDE STRUCTURE ekpo.
DATA: END OF it_ekpo_a.

DATA: BEGIN OF it_bseg_a OCCURS 0.
        INCLUDE STRUCTURE bseg.
DATA: END OF it_bseg_a.

DATA: BEGIN OF it_bkpf_a OCCURS 0.
        INCLUDE STRUCTURE bkpf.
DATA: END OF it_bkpf_a.

DATA: BEGIN OF it_vbpa_a OCCURS 0.
        INCLUDE STRUCTURE vbpa.
DATA: END OF it_vbpa_a.

DATA: BEGIN OF it_bset_a OCCURS 0.
        INCLUDE STRUCTURE bset.
DATA: END OF it_bset_a.

DATA: BEGIN OF it_bkpf_mkpf OCCURS 0.
        INCLUDE STRUCTURE bkpf.
DATA: END OF it_bkpf_mkpf.

DATA: BEGIN OF it_vbrk_a OCCURS 0.
        INCLUDE STRUCTURE vbrk.
DATA: END OF it_vbrk_a.

DATA: BEGIN OF it_vbrp_a OCCURS 0.
        INCLUDE STRUCTURE vbrp.
DATA: END OF it_vbrp_a.

DATA: BEGIN OF it_vbfa_a OCCURS 0.
        INCLUDE STRUCTURE vbfa.
DATA: END OF it_vbfa_a.

DATA: BEGIN OF it_konv_a OCCURS 0.
        INCLUDE STRUCTURE konv.
DATA: END OF it_konv_a.

DATA: BEGIN OF it_infstr OCCURS 0,
         archindex LIKE aind_str1-archindex,
         itype     LIKE aind_str1-itype,
         otyp      LIKE aind_str1-otyp,
         object    LIKE aind_str1-object,
         active    LIKE aind_str2-active,
         gentab    LIKE aind_str2-gentab,
       END OF it_infstr.


DATA: BEGIN OF it_archkey OCCURS 0,
       objnr LIKE coep-objnr,
       arch_key LIKE arch_idx_s-archivekey,
       arch_offset LIKE arch_idx_s-obj_offset,
    END OF it_archkey.

DATA: BEGIN OF it_archkey1 OCCURS 0,
       ebeln LIKE ekko-ebeln,
       arch_key LIKE arch_idx_s-archivekey,
       arch_offset LIKE arch_idx_s-obj_offset,
    END OF it_archkey1.

DATA: BEGIN OF it_archkey2 OCCURS 0,
       bukrs LIKE bkpf-bukrs,
       belnr LIKE bkpf-belnr,
       gjahr LIKE bkpf-gjahr,
       arch_key LIKE arch_idx_s-archivekey,
       arch_offset LIKE arch_idx_s-obj_offset,
    END OF it_archkey2.

DATA: BEGIN OF it_archkey3 OCCURS 0,
       vbeln LIKE vbrk-vbeln,
       arch_key LIKE arch_idx_s-archivekey,
       arch_offset LIKE arch_idx_s-obj_offset,
    END OF it_archkey3.

DATA: BEGIN OF it_archkey4 OCCURS 0,
       vbeln LIKE vbrk-vbeln ,
       arch_key LIKE arch_idx_s-archivekey,
       arch_offset LIKE arch_idx_s-obj_offset,
    END OF it_archkey4.

DATA: BEGIN OF it_archkey5 OCCURS 0,
       bukrs LIKE bkpf-bukrs,
       gjahr LIKE bkpf-gjahr,
       monat LIKE bkpf-monat,
       arch_key LIKE arch_idx_s-archivekey,
       arch_offset LIKE arch_idx_s-obj_offset,
    END OF it_archkey5.



DATA: BEGIN OF it_rel_tab OCCURS 0,
        table LIKE dd02v-tabname,
        END OF it_rel_tab.


DATA: BEGIN OF it_table_org1 OCCURS 0,
       table LIKE dd02v-tabname,
       text    LIKE dd02v-ddtext,
       END OF it_table_org1.

DATA: BEGIN OF it_table_org2 OCCURS 0,
       table LIKE dd02v-tabname,
       text    LIKE dd02v-ddtext,
       END OF it_table_org2.

DATA : it_dd03l LIKE dd03l OCCURS 0 WITH HEADER LINE.

DATA : it_fieldcatalog TYPE lvc_t_fcat.

DATA: BEGIN OF it_prog2 OCCURS 0,
        line(80) TYPE c,
        END OF it_prog2.

DATA: BEGIN OF it_prog1 OCCURS 0,
        line(80) TYPE c,
        END OF it_prog1.
DATA: BEGIN OF lt_bkpf OCCURS 0.
        INCLUDE STRUCTURE abkpf. "bkpf sobhan
DATA: END OF lt_bkpf.

DATA: BEGIN OF lt_bseg OCCURS 0.
        INCLUDE STRUCTURE bseg. "bkpf sobhan
DATA: END OF lt_bseg.

DATA: BEGIN OF lt_bset OCCURS 0.
        INCLUDE STRUCTURE bset. "bkpf sobhan
DATA: END OF lt_bset.

DATA: BEGIN OF lt_bvor OCCURS 0.
        INCLUDE STRUCTURE bvor. "bvor sobhan
DATA: END OF lt_bvor.
************************************************************************
*                             FIELD SYMBOLS                            *
************************************************************************
FIELD-SYMBOLS:  <fs_value> TYPE table ,
                <fs_wa> TYPE ANY,
                <fs_archivekey> TYPE ANY,
                <fs_archiveofs> TYPE ANY,
                <fs_objnr> TYPE ANY,
                <fs_bukrs> TYPE ANY,
                <fs_belnr> TYPE ANY,
                <fs_gjahr> TYPE ANY,
                <fs_ebeln> TYPE ANY,
                <fs_vbeln> TYPE ANY,
                <fs_monat> TYPE ANY.

************************************************************************
*                            GLOBAL DATA DECLARATIONS                  *
************************************************************************
DATA:g_ref TYPE REF TO data,
     g_ref_vbrk TYPE REF TO data,
     g_ref_ekko TYPE REF TO data,
     g_ref_bkpf TYPE REF TO data,
     g_ref_vbap TYPE REF TO data,
     g_ref_vbak TYPE REF TO data,
     g_ref_bseg TYPE REF TO data,
     g_ref1 TYPE REF TO data.

DATA: g_infstr LIKE aind_str1-archindex VALUE 'SAP_CO_ITEM_001',
**    g_infstr1 LIKE aind_str1-archindex VALUE 'SAP_DRB_MM_EKKO',
      g_infstr1 LIKE aind_str1-archindex VALUE 'Z_MM_EKKO_GP',
**    g_infstr2 LIKE aind_str2-archindex VALUE 'SAP_FI_DOC_DRB1',
      g_infstr2 like aind_str2-archindex value 'SAP_FI_DOC_002',
      g_infstr3 LIKE aind_str2-archindex VALUE 'SAP_DRB_VBAK_02',
      g_infstr4 LIKE aind_str2-archindex VALUE 'SAP_SD_VBRK_001'.

DATA: g_object_cnt       TYPE i,
      g_read_cnt         TYPE i,
      g_reload_cnt       TYPE i,
      g_duprec           TYPE c,
      g_bkpf_only,
      g_read_handle LIKE sy-tabix,
      g_commit_cnt       LIKE arch_usr-arch_comit.



DATA: g_flag_vbpa TYPE c VALUE 'X',
      g_flag_vbak TYPE c VALUE 'X',
      g_flag_ekko TYPE c VALUE 'X',
      g_flag_mkpf TYPE c VALUE 'X',
      g_flag_bkpf TYPE c VALUE 'X',
      g_flag_vbrk TYPE c VALUE 'X',
      g_flag_bseg TYPE c VALUE 'X'.

CONSTANTS:c_x TYPE c VALUE 'X',
          c_i LIKE aind_str1-itype VALUE 'I',
          c_o LIKE aind_str1-otyp VALUE 'O'.
