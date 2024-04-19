*&---------------------------------------------------------------------*
*&  Include           ZFAPI126_PURCHASE_ORDER_TOP
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
*& Program Name       :  ZFAPI126_PURCHASE_ORDER                       *
*& Include            :  ZFAPI126_PURCHASE_ORDER_TOP                   *
*& Author             :  Paul Karunakar                                *
*& Creation Date      :  02-Apr-2018                                   *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Purchase Order data from each of the three    *
*&                       SAP instances will be extracted in a          *
*&                       delimited file and sent to IAP.               *
*&                                                                     *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 02-Apr-2018  KPAL        D30K928646  CHG0107206 Initial Development  *
*                          D30K928846, D30K928994                      *
************************************************************************

TYPE-POOLS: vrm.

TABLES: sscrfields, ekko.

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF gty_ebeln,                  "Purchasing Document Number"
        ebeln            TYPE ebeln,
       END   OF gty_ebeln.

TYPES:  gtt_ebeln        TYPE STANDARD TABLE OF gty_ebeln.

TYPES: BEGIN OF gty_packno,                 "Package Number            "
        packno           TYPE packno,
       END   OF gty_packno.

TYPES:  gtt_packno       TYPE STANDARD TABLE OF gty_packno.

TYPES:  gty_editpos      TYPE cdred.        "Change Documents          "

TYPES:  gtt_editpos      TYPE STANDARD TABLE OF gty_editpos.

TYPES: BEGIN OF gty_t006a,                  "Units Of Measure          "
        msehi            TYPE msehi,
        mseh3            TYPE mseh3,
       END   OF gty_t006a.

TYPES:  gtt_t006a        TYPE STANDARD TABLE OF gty_t006a.

TYPES: BEGIN OF gty_ekko,                   "Purchasing Document Header"
        ebeln            TYPE ebeln,
        bukrs            TYPE bukrs,
        bstyp            TYPE ebstyp,
        bsart            TYPE esart,
        loekz            TYPE eloek,
        statu            TYPE estak,
        aedat            TYPE erdat,
        ernam            TYPE ernam,
        lifnr            TYPE elifn,
        zterm            TYPE dzterm,
        ekorg            TYPE ekorg,
        ekgrp            TYPE bkgrp,
        waers            TYPE waers,
        bedat            TYPE ebdat,
        llief            TYPE llief,
        lifre            TYPE lifre,
        memorytype       TYPE memorytype,
        zzariba_approver TYPE z_ariba_approver,
       END   OF gty_ekko.

TYPES:  gtt_ekko         TYPE STANDARD TABLE OF gty_ekko.

*eject
TYPES: BEGIN OF gty_ekpo,                   "Purchasing Document Item  "
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        loekz            TYPE eloek,
        statu            TYPE astat,
        aedat            TYPE paedt,
        txz01            TYPE txz01,
        matnr            TYPE matnr,
        bukrs            TYPE bukrs,
        werks            TYPE ewerk,
        lgort            TYPE lgort_d,
        menge            TYPE bstmg,
        meins            TYPE bstme,
        bprme            TYPE bbprm,
        netpr            TYPE bprei,
        netwr            TYPE bwert,
        brtwr            TYPE bbwert,
        mwskz            TYPE mwskz,
        pstyp            TYPE pstyp,
        knttp            TYPE knttp,
        vrtkz            TYPE vrtkz,
        wepos            TYPE wepos,
        webre            TYPE webre,
        txjcd            TYPE txjcd,
        packno           TYPE packno,
        xersy            TYPE xersy,
        afnam            TYPE afnam,
       END   OF gty_ekpo.

TYPES:  gtt_ekpo         TYPE STANDARD TABLE OF gty_ekpo.

*eject
TYPES: BEGIN OF gty_ekkn,                   "Purch. Doc. Account Asgnmnt
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        zekkn            TYPE dzekkn,
        menge            TYPE menge_d,
        vproz            TYPE vproz,
        sakto            TYPE saknr,
        gsber            TYPE gsber,
        kostl            TYPE kostl,
        vbeln            TYPE vbeln_co,
        vbelp            TYPE posnr_co,
        anln1            TYPE anln1,
        anln2            TYPE anln2,
        aufnr            TYPE aufnr,
        wempf            TYPE wempf,
        kokrs            TYPE kokrs,
        paobjnr          TYPE rkeobjnr,
        prctr            TYPE prctr,
        ps_psp_pnr       TYPE ps_psp_pnr,
        nplnr            TYPE nplnr,
        aufpl            TYPE co_aufpl,
        aplzl            TYPE cim_count,
        lstar            TYPE lstar,
       END   OF gty_ekkn.

TYPES:  gtt_ekkn         TYPE STANDARD TABLE OF gty_ekkn.

TYPES: BEGIN OF gty_esuh,                   "Services Management Header"
        packno           TYPE packno,
        sumlimit         TYPE sumlimit,
        commitment       TYPE commitment,
        actvalue         TYPE actvalue,
        waers            TYPE waers,
       END OF gty_esuh.

TYPES:  gtt_esuh         TYPE STANDARD TABLE OF gty_esuh.

TYPES:  gty_return       TYPE bapiret2.

TYPES:  gtt_return       TYPE STANDARD TABLE OF gty_return.

TYPES:  gty_po_ttls      TYPE bapiekbes.

TYPES:  gtt_po_ttls      TYPE STANDARD TABLE OF gty_po_ttls.

*eject
TYPES: BEGIN OF gty_final,
        erpid            TYPE char5,        "ERP_Id                    "
        ebeln            TYPE ebeln,        "PO_Number                 "
        lifnr            TYPE elifn,        "Vendor_Number             "
        bsart            TYPE esart,        "Purchasing_Doc_Type       "
        bstyp            TYPE ebstyp,       "Purchasing_Doc_Category   "
        bukrs            TYPE bukrs,        "Company_Code              "
        ekorg            TYPE ekorg,        "Purchase_Organization     "
        ekgrp            TYPE bkgrp,        "Purchase_Group            "
        zterm            TYPE dzterm,       "Payment_Terms             "
        waers            TYPE waers,        "Currency                  "
        aedat_ekko       TYPE char10,       "Created_Date              "
        ernam            TYPE ernam,        "Created_By                "
        bedat            TYPE char10,       "Document_Date             "
        loekz_ekko       TYPE eloek,        "Deletion_Indicator        "
        llief            TYPE llief,        "Supplying_Vendor          "
        lifre            TYPE lifre,        "Invoicing_Party           "
        zzariba_approver TYPE z_ariba_approver, "Service_Confirmer     "
        memorytype       TYPE memorytype,   "Category_Of_Incompleteness"
        loekz_ekpo       TYPE eloek,        "Deletion_Indicator_Item   "
        sumlimit         TYPE char16,       "Overall_Limit             "
        actvalue         TYPE char16,       "Actual_Value              "
        commitment       TYPE char16,       "Expected_Value            "
        ebelp            TYPE ebelp,        "PO_Item_Number            "
        aedat_ekpo       TYPE char10,       "Item_Changed_Date         "
        matnr            TYPE matnr,        "Material                  "
        bukrs_ekpo       TYPE bukrs,        "Item_Company_Code         "
        txz01            TYPE txz01,        "PO_Item_Text              "
        menge_ekpo       TYPE char17,       "PO_Quantity               "
        meins            TYPE bstme,        "Order_Unit                "
        werks            TYPE ewerk,        "Plant                     "
        lgort            TYPE lgort_d,      "Storage_Location          "
        menge_ekpo_oq    TYPE char17,       "Order_Quantity            "
        qty_out_ordr     TYPE char17,       "Outstanding_Order_Quantity"
        qty_out_invc     TYPE char17,       "Outstanding_Invoice_Quanti"
        bprme            TYPE bbprm,        "Order_Price_Unit          "
        netpr            TYPE char16,       "Net_Order_Price           "
        netwr            TYPE char16,       "Net_Order_Value           "
        brtwr            TYPE char16,       "Gross_Order_Value         "
        amt_out_cmtm     TYPE char16,       "Outstanding_Commitment_Amo"
        mwskz            TYPE mwskz,        "Tax_Code                  "
        txjcd            TYPE txjcd,        "Tax_Jurisdiction          "
        knttp            TYPE knttp,        "Acct_Assignment_Category  "
        xersy            TYPE xersy,        "ERS                       "
        webre            TYPE webre,        "GR_Invoice_Verif          "
        vrtkz            TYPE vrtkz,        "Distribution_Indicator    "
        wepos            TYPE wepos,        "Goods_Receipt_Indicator   "
        pstyp            TYPE pstyp,        "Item_Category             "
        vrtkz_dk         TYPE vrtkz,        "Distribution_Key          "
        afnam            TYPE afnam,        "Name of Requisitioner/Requ"
*eject
        zekkn            TYPE char2,        "Account_Assignment_Seq_No "
        menge_ekkn       TYPE char17,       "Distribution_Quantity     "
        open_qty         TYPE char17,       "Open_Quantity             "
        vproz            TYPE char5,        "Percentage                "
        sakto            TYPE saknr,        "GL_Account                "
        gsber            TYPE gsber,        "Business_Area             "
        kostl            TYPE kostl,        "Cost_Center               "
        anln1            TYPE anln1,        "Main_Asset_Number         "
        anln2            TYPE anln2,        "Asset_Sub_Number          "
        aufnr            TYPE aufnr,        "Order_Number              "
        paobjnr          TYPE char10,       "Profitability_Segment     "
        prctr            TYPE prctr,        "Profit_Center             "
        ps_psp_pnr       TYPE char24,       "WBS_Element               "
        nplnr            TYPE nplnr,        "Network_Id                "
        vornr            TYPE vornr,        "Activity_Code             "
        wempf            TYPE wempf,        "Goods_Recipient           "
        kokrs            TYPE kokrs,        "Controlling_Area          "
        lstar            TYPE lstar,        "Activity_Type             "
       END OF gty_final.

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
                         VALUE 'ZFAPI126_PURCHASE_ORDER',
        gc_filename      TYPE fileintern
                         VALUE 'SSSSS_Purc_Ordr_YYMMDDHHMM_NNN.CSV'.

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
        gt_ebeln         TYPE gtt_ebeln,    "Purchasing Document Number"
        gt_ekko          TYPE gtt_ekko,     "Purchasing Document Header"
        gt_ekpo          TYPE gtt_ekpo,     "Purchasing Document Item  "
        gt_ekkn          TYPE gtt_ekkn,     "Purch. Doc. Account Asgnmnt
        gt_esuh          TYPE gtt_esuh,     "Services Management Header"
        gt_output        TYPE gtt_output.   "Output                    "
