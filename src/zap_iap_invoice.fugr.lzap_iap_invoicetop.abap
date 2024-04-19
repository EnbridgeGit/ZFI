FUNCTION-POOL ZAP_IAP_INVOICE.              "MESSAGE-ID ..
* INCLUDE LZFI_IAP_INVOICED...               " Local class definition
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  gty_doc_hdr      TYPE zaps_iap_doc_hdr,  "IAP Document Header  "
        gtt_doc_hdr      TYPE STANDARD TABLE OF gty_doc_hdr.

TYPES:  gty_doc_item     TYPE zaps_iap_doc_item, "IAP Document Item    "
        gtt_doc_item     TYPE STANDARD TABLE OF gty_doc_item.

TYPES:  gty_doc_actg     TYPE zaps_iap_doc_actg, "IAP Document Accountng
        gtt_doc_actg     TYPE STANDARD TABLE OF gty_doc_actg.

TYPES:  gty_doc_detl     TYPE zaps_iap_doc_detl, "IAP Document Detail  "
        gtt_doc_detl     TYPE STANDARD TABLE OF gty_doc_detl.

TYPES:  gty_return       TYPE zaps_iap_doc_actg, "Return - Messages    "
        gtt_return       TYPE STANDARD TABLE OF gty_return.

TYPES: BEGIN OF gty_lfa1,                        "Vendor Master (Generl"
        lifnr            TYPE lifnr,
        txjcd            TYPE txjcd,
       END   OF gty_lfa1.

TYPES: BEGIN OF gty_ska1,                        "G/L Account          "
        ktopl            TYPE ktopl,
        saknr            TYPE saknr,
       END   OF gty_ska1.

TYPES: BEGIN OF gty_csks,                        "Cost Center          "
        kokrs            TYPE kokrs,
        kostl            TYPE kostl,
        kosar            TYPE kosar,
        txjcd            TYPE txjcd,
        prctr            TYPE prctr,
        werks            TYPE werks_d,
        objnr            TYPE j_objnr,
       END   OF gty_csks.

TYPES: BEGIN OF gty_aufk,                        "Internal Order       "
        aufnr            TYPE aufnr,
        werks            TYPE werks_d,
        stort            TYPE aufstort,
        objnr            TYPE j_objnr,
        prctr            TYPE prctr,
        txjcd            TYPE txjcd,
       END   OF gty_aufk.

TYPES: BEGIN OF gty_prps,                        "WBS Element          "
        posnr            TYPE ps_posnr,
        posid            TYPE ps_posid,
        objnr            TYPE j_objnr,
        psphi            TYPE ps_psphi,
        astnr            TYPE ps_astnr,
        prctr            TYPE prctr,
        werks            TYPE werks_d,
        txjcd            TYPE txjcd,
        stort            TYPE ps_stort,
        wbselem          TYPE char24,
        profl            TYPE profidproj,
       END   OF gty_prps.

TYPES: BEGIN OF gty_afko,                        "Network Activity Link"
        aufnr            TYPE aufnr,
        aufpl            TYPE co_aufpl,
       END   OF gty_afko.

TYPES: BEGIN OF gty_afvc,                        "Activity in a Network"
        aufpl            TYPE co_aufpl,
        aplzl            TYPE co_aplzl,
        vornr            TYPE vornr,
        werks            TYPE werks_d,
        objnr            TYPE j_objnr,
        txjcd            TYPE txjcd,
        prctr            TYPE prctr,
       END   OF gty_afvc.                        "Activity in a Network"

TYPES:  gty_location     TYPE zfit_location.     "Location Code

TYPES:  gty_location_results                     "Loctn Results - VERTEX
                         TYPE com_jur,

        gtt_location_results
                         TYPE STANDARD TABLE OF gty_location_results.

TYPES: BEGIN OF gty_ekko,                        "Purchase Order Header"
        ebeln            TYPE ebeln,
        bukrs            TYPE bukrs,
        bstyp            TYPE ebstyp,
        bsart            TYPE esart,
        loekz            TYPE eloek,
        statu            TYPE estak,
        aedat            TYPE erdat,
        lifnr            TYPE elifn,
       END   OF gty_ekko,

        gtt_ekko         TYPE STANDARD TABLE OF gty_ekko.

TYPES: BEGIN OF gty_ekpo,                        "Purchase Order Item  "
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        loekz            TYPE eloek,
        aedat            TYPE paedt,
        matnr            TYPE matnr,
        bukrs            TYPE bukrs,
        werks            TYPE ewerk,
        lgort            TYPE lgort_d,
        matkl            TYPE matkl,
        meins            TYPE bstme,
        knttp            TYPE knttp,
       END   OF gty_ekpo,

        gtt_ekpo         TYPE STANDARD TABLE OF gty_ekpo.

TYPES: BEGIN OF gty_ekkn,                        "Purchase Order Accntng
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        zekkn            TYPE dzekkn,
        loekz            TYPE kloek,
        aedat            TYPE erdat,
        vproz            TYPE vproz,
        sakto            TYPE saknr,
        kostl            TYPE kostl,
        aufnr            TYPE aufnr,
        kokrs            TYPE kokrs,
        paobjnr          TYPE rkeobjnr,
        prctr            TYPE prctr,
        ps_psp_pnr       TYPE ps_psp_pnr,
        nplnr            TYPE nplnr,
        aufpl            TYPE co_aufpl,
        aplzl            TYPE co_aplzl,
        zzloc            TYPE zzloc,
        zzref            TYPE zzref,
        mwskz            TYPE mwskz,
        txjcd            TYPE txjcd,
       END   OF gty_ekkn,

        gtt_ekkn         TYPE STANDARD TABLE OF gty_ekkn.

TYPES: BEGIN OF gty_t001w,                       "Plant                "
        werks            TYPE werks_d,
        txjcd            TYPE txjcd,
       END   OF gty_t001w.

TYPES:  gty_ec_tax       TYPE zmmt_ec_tax.       "E&C Virtual Plant    "

************************************************************************
*                               Constants                              *
************************************************************************
CONSTANTS:
        gc_doccatg_po_inv                        "PO Invoice           "
                        TYPE char3    VALUE 'PO',
        gc_doccatg_po_cm                         "PO Credit Memo       "
                        TYPE char3    VALUE 'PCM',
        gc_doccatg_npo_inv                       "Non-PO Invoice       "
                        TYPE char3    VALUE 'NPO',
        gc_doccatg_npo_cm                        "Non-PO Credit Memo   "
                        TYPE char3    VALUE 'NCM',
        gc_awtyp_bkpf                            "Ref.Transaction-FB01 "
                        TYPE awtyp    VALUE 'BKPF',
        gc_awtyp_rmrp                            "Ref.Transaction-MIRO "
                        TYPE awtyp    VALUE 'RMRP',
        gc_x            TYPE C        VALUE 'X',
        gc_e            TYPE C        VALUE 'E',
        gc_s            TYPE C        VALUE 'S',
        gc_hi           TYPE MWSKZ    VALUE 'HI',
        gc_m5           TYPE MWSKZ    VALUE 'M5',
        gc_h4           TYPE MWSKZ    VALUE 'H4',
        gc_em           TYPE MWSKZ    VALUE 'EM',
        gc_et           TYPE MWSKZ    VALUE 'ET',
        gc_h1           TYPE MWSKZ    VALUE 'H1',
        gc_ha           TYPE MWSKZ    VALUE 'HA',
        gc_hb           TYPE MWSKZ    VALUE 'HB',
        gc_hp           TYPE MWSKZ    VALUE 'HP',
        gc_ht           TYPE MWSKZ    VALUE 'HT',
        gc_hu           TYPE MWSKZ    VALUE 'HU',
        gc_t2           TYPE MWSKZ    VALUE 'T2',
        gc_m7           TYPE MWSKZ    VALUE 'M7',
        gc_q2           TYPE MWSKZ    VALUE 'Q2',
        gc_T1           TYPE MWSKZ    VALUE 'T1',
        gc_t3           TYPE MWSKZ    VALUE 'T3',
        gc_jc           TYPE MWSKZ    VALUE 'JC',
        gc_jz           TYPE MWSKZ    VALUE 'JZ',
        gc_rc           TYPE MWSKZ    VALUE 'RC',
        gc_re           TYPE MWSKZ    VALUE 'RE',
        gc_rz           TYPE MWSKZ    VALUE 'RZ',
        gc_sc           TYPE MWSKZ    VALUE 'SC',
        gc_sz           TYPE MWSKZ    VALUE 'SZ'.

************************************************************************
*                               Variables                              *
************************************************************************
DATA:   gv_check         TYPE xflag,             "Check - Simulate     "
        gv_awtyp         TYPE awtyp.             "Reference Transaction"

************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gs_doc_hdr       TYPE gty_doc_hdr,       "IAP Document Header  "
        gs_doc_item      TYPE gty_doc_item,      "IAP Document Item    "
        gs_doc_actg      TYPE gty_doc_actg,      "IAP Document Accountng
        gs_doc_detl      TYPE gty_doc_detl,      "IAP Document Detail  "
*        gs_return        TYPE gty_return,        "Return - Messages    "
        gs_lfa1          TYPE gty_lfa1,          "Vendor Master (Generl"
        gs_ska1          TYPE gty_ska1,          "G/L Account          "
        gs_csks          TYPE gty_csks,          "Cost Center          "
        gs_aufk          TYPE gty_aufk,          "Internal Order       "
        gs_prps          TYPE gty_prps,          "WBS Element          "
        gs_afko          TYPE gty_afko,          "Network Activity Link"
        gs_afvc          TYPE gty_afvc,          "Activity in a Network"
        gs_location      TYPE gty_location,      "Location             "
        gs_location_results                      "Loctn Results - VERTEX
                         TYPE gty_location_results,
        gs_ekko          TYPE gty_ekko,          "Purchase Order Header"
        gs_ekpo          TYPE gty_ekpo,          "Purchase Order Item  "
        gs_ekkn          TYPE gty_ekkn,          "Purchase Order Accntng
        gs_t001w         TYPE gty_t001w,         "Plant                "
        gs_ec_tax        TYPE gty_ec_tax.        "E&C Virtual Plant    "

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_doc_hdr       TYPE gtt_doc_hdr,       "IAP Document Header  "
        gt_doc_item      TYPE gtt_doc_item,      "IAP Document Item    "
        gt_doc_actg      TYPE gtt_doc_actg,      "IAP Document Accountng
        gt_doc_detl      TYPE gtt_doc_detl,      "IAP Document Detail  "
        gt_return        TYPE gtt_return,        "Return - Messages    "
        gt_ekko          TYPE gtt_ekko,          "Purchase Order Header"
        gt_ekpo          TYPE gtt_ekpo,          "Purchase Order Item  "
        gt_ekkn          TYPE gtt_ekkn.          "Purchase Order Accntng
