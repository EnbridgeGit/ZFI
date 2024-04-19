*&---------------------------------------------------------------------*
*&  Include           ZFAPI125_GOODS_RECEIPT_TOP
*&---------------------------------------------------------------------*
************************************************************************
*                            Enbridge Energy                           *
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI125_GOODS_RECEIPT                        *
*& Author             :  Kalinga Keshari Rout                          *
*& Creation Date      :  March 26, 2018                                *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Program extracts goods receipt history data   *
*                        for both cases full load and delta load and   *
*&                       file created application server.              "
*&---------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 26-Mar-2018  KROUT       D30K928872  CHG0106485  Initial Development *
*                          D30K928904, D30K928931, D30K928955,         *
*                          D30K928981, D30K929077                      *
*----------------------------------------------------------------------*

TYPE-POOLS: vrm.

TABLES: sscrfields, ekko.

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF gty_t006a,                  "Units Of Measure          "
        msehi            TYPE msehi,
        mseh3            TYPE mseh3,
       END   OF gty_t006a.

TYPES:  gtt_t006a        TYPE STANDARD TABLE OF gty_t006a.

TYPES: BEGIN OF gty_ekko,                   "Purchasing Document Header"
        ebeln            TYPE ebeln,
        bsart            TYPE esart,
        loekz            TYPE eloek,
        aedat            TYPE erdat,
       END   OF gty_ekko.

TYPES:  gtt_ekko         TYPE STANDARD TABLE OF gty_ekko.

TYPES: BEGIN OF gty_ekpo,                   "Purchasing Document Item  "
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        loekz            TYPE eloek,
        meins            TYPE bstme,
        webre            TYPE webre,
        xersy            TYPE xersy,
       END   OF gty_ekpo.

TYPES:  gtt_ekpo         TYPE STANDARD TABLE OF gty_ekpo.

TYPES: BEGIN OF gty_ekbe,                   "Purchasing History        "
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        gjahr            TYPE mjahr,
        belnr            TYPE mblnr,
        buzei            TYPE mblpo,
        vgabe            TYPE vgabe,
        zekkn            TYPE dzekkn,
        bewtp            TYPE bewtp,
        bwart            TYPE bwart,
        menge            TYPE menge_d,
        wrbtr            TYPE wrbtr,
        waers            TYPE waers,
        shkzg            TYPE shkzg,
        xblnr            TYPE xblnr1,
        lfgja            TYPE lfbja,
        lfbnr            TYPE lfbnr,
        lfpos            TYPE lfpos,
        cpudt            TYPE cpudt,
       END   OF gty_ekbe.

TYPES:  gtt_ekbe         TYPE STANDARD TABLE OF gty_ekbe.

*eject
TYPES: BEGIN OF gty_po_hist_key,            "Purchasing History Key    "
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        gjahr            TYPE mjahr,
        belnr            TYPE mblnr,
        buzei            TYPE mblpo,
       END   OF gty_po_hist_key.

TYPES:  gtt_po_hist_key  TYPE STANDARD TABLE OF gty_po_hist_key.

TYPES: BEGIN OF gty_po_hist,                "Purchasing History        "
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        gjahr            TYPE mjahr,
        belnr            TYPE mblnr,
        buzei            TYPE mblpo,
        vgabe            TYPE vgabe,
        zekkn            TYPE dzekkn,
        bewtp            TYPE bewtp,
        bwart            TYPE bwart,
        menge            TYPE menge_d,
        meins            TYPE bstme,
        wrbtr            TYPE wrbtr,
        waers            TYPE waers,
        shkzg            TYPE shkzg,
        xblnr            TYPE xblnr1,
        lfgja            TYPE lfbja,
        lfbnr            TYPE lfbnr,
        lfpos            TYPE lfpos,
        cpudt            TYPE cpudt,
       END   OF gty_po_hist.

TYPES:  gtt_po_hist      TYPE STANDARD TABLE OF gty_po_hist.

TYPES: BEGIN OF gty_doc_sum,                "Document Summary          "
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        gjahr            TYPE mjahr,
        belnr            TYPE mblnr,
        buzei            TYPE mblpo,
        menge            TYPE menge_d,
        meins            TYPE bstme,
        wrbtr            TYPE wrbtr,
        waers            TYPE waers,
        xblnr            TYPE xblnr,
        menge_open       TYPE menge_d,
        wrbtr_open       TYPE wrbtr,
       END   OF gty_doc_sum.

TYPES:  gtt_doc_sum      TYPE STANDARD TABLE OF gty_doc_sum.

*eject
TYPES: BEGIN OF gty_final,                  "Final Layout              "
          erpid          TYPE char5,        "ERP_Id                    "
          belnr          TYPE mblnr,        "Material_Doc_Number       "
          gjahr          TYPE mjahr,        "Material_Doc_Year         "
          buzei          TYPE mblpo,        "Material_Doc_Item         "
          xblnr          TYPE xblnr1,       "Delivery_Note             "
          ebeln          TYPE ebeln,        "PO_Number                 "
          ebelp          TYPE ebelp,        "PO_Item_Number            "
          menge          TYPE char17,       "Quantity                  "
          meins          TYPE bstme,        "Order_Price_Unit          "
          wrbtr          TYPE char16,       "Amount                    "
          waers          TYPE waers,        "Currency                  "
          ind_del        TYPE char3,        "Deletion_Indicator        "
          ind_inv        TYPE char3,        "Invoiced_Indicator        "
          menge_open     TYPE char17,       "Open_Quantity             "
          wrbtr_open     TYPE char16,       "Open_Amount               "
          status         TYPE char1,        "Status                    "
       END   OF gty_final.

TYPES:  gtt_final        TYPE STANDARD TABLE OF gty_final.

TYPES:  gty_output       TYPE string.       "Output                    "

TYPES:  gtt_output       TYPE STANDARD TABLE OF gty_output.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_delim         TYPE char2 VALUE '|~',
        gc_ucomm_onli    TYPE sscrfields-ucomm VALUE 'ONLI',
        gc_lgcfilpth     TYPE pathintern
                         VALUE 'ZFAPI125_GOODS_RECEIPT',
        gc_filename      TYPE fileintern
                         VALUE 'SSSSS_Goods_Rcpt_YYMMDDHHMM_NNN.CSV'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_cn_recs_total TYPE i,
        gv_cn_recs_max   TYPE i,
        gv_cn_recs_file  TYPE i,
        gv_cn_files      TYPE i,
        gv_filename      TYPE text256,
        gv_filename_p    TYPE text256.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_t006a         TYPE gtt_t006a,    "Units Of Measure          "
        gt_ekko          TYPE gtt_ekko,     "Purchasing Document Header"
        gt_po_hist       TYPE gtt_po_hist,  "Purchasing History        "
        gt_output        TYPE gtt_output.   "Output                    "
