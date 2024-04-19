*&---------------------------------------------------------------------*
*& Report  ZFAPI112_OPEN_INVOICES
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI112_OPEN_INVOICES                        *
* Author             :   Paul Karunakar                                *
* Date               :   Feb 1, 2018                                   *
* Technical Contact  :   John Hartung                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Extract interface of Open and Parked          *
*                        Invoices to IAP                               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  KPAL        D30K928583  CHG0100815  Initial Development *
*                          D30K928768, D30K928884                      *
*----------------------------------------------------------------------*

*=======================================================================
* TABLES
*=======================================================================
TABLES: sscrfields, bsik, bkpf.

*eject
*=======================================================================
* GLOBAL TYPES
*=======================================================================
TYPE-POOLS: vrm.

TYPES: BEGIN OF gty_bsik,
        bukrs            TYPE bukrs,
        belnr            TYPE belnr_d,
        gjahr            TYPE gjahr,
        buzei            TYPE buzei,
        lifnr            TYPE lifnr,
        umsks            TYPE umsks,
        umskz            TYPE umskz,
        augdt            TYPE augdt,
        augbl            TYPE augbl,
        zuonr            TYPE dzuonr,
        budat            TYPE budat,
        bldat            TYPE bldat,
        waers            TYPE waers,
        xblnr            TYPE xblnr1,
        blart            TYPE blart,
        bschl            TYPE bschl,
        shkzg            TYPE shkzg,
        gsber            TYPE gsber,
        dmbtr            TYPE dmbtr,
        wrbtr            TYPE wrbtr,
        wmwst            TYPE wmwst,
        sgtxt            TYPE sgtxt,
        zterm            TYPE dzterm,
        bstat            TYPE bstat_d,
        dmbe2            TYPE dmbe2,
        vertn            TYPE ranl,
       END   OF gty_bsik,

        gtt_bsik         TYPE STANDARD TABLE OF gty_bsik.

TYPES: BEGIN OF gty_lfa1,
        lifnr            TYPE lifnr,
        name1            TYPE name1_gp,
       END   OF gty_lfa1,

        gtt_lfa1         TYPE STANDARD TABLE OF gty_lfa1.

*eject
TYPES: BEGIN OF gty_bkpf,
        bukrs            TYPE bukrs,
        belnr            TYPE belnr_d,
        gjahr            TYPE gjahr,
        budat            TYPE budat,
        usnam            TYPE usnam,
        tcode            TYPE tcode,
        waers            TYPE waers,
        kursf            TYPE kursf,
        bstat            TYPE bstat_d,
        awtyp            TYPE awtyp,
        hwaer            TYPE hwaer,
        hwae2            TYPE hwae2,
       END   OF gty_bkpf,

        gtt_bkpf         TYPE STANDARD TABLE OF gty_bkpf.

TYPES: BEGIN OF gty_vbsegk,
        ausbk            TYPE ausbk,
        belnr            TYPE belnr_d,
        gjahr            TYPE gjahr,
        bzkey            TYPE buzei,
        bukrs            TYPE bukrs,
        bschl            TYPE bschl,
        umskz            TYPE umskz,
        umsks            TYPE umsks,
        shkzg            TYPE shkzg,
        gsber            TYPE gsber,
        dmbtr            TYPE dmbtr,
        dmbe2            TYPE dmbe2,
        wrbtr            TYPE wrbtr,
        wmwst            TYPE wmwst,
        sgtxt            TYPE sgtxt,
        lifnr            TYPE lifnr,
        zterm            TYPE dzterm,
        vertn            TYPE ranl,
       END OF  gty_vbsegk,

        gtt_vbsegk       TYPE STANDARD TABLE OF gty_vbsegk.

*eject
TYPES: BEGIN OF gty_vbkpf,
        ausbk            TYPE ausbk,
        bukrs            TYPE bukrs,
        belnr            TYPE belnr_d,
        gjahr            TYPE gjahr,
        bstat            TYPE bstat_d,
        blart            TYPE blart,
        bldat            TYPE bldat,
        budat            TYPE budat,
        usnam            TYPE usnam,
        tcode            TYPE tcode,
        xblnr            TYPE xblnr1,
        waers            TYPE waers,
        hwaer            TYPE hwaer,
        hwae2            TYPE hwae2,
        kursf            TYPE kursf,
        awtyp            TYPE awtyp,
       END   OF gty_vbkpf,

        gtt_vbkpf        TYPE STANDARD TABLE OF gty_vbkpf.

TYPES:  gty_data_dl      TYPE string,

        gtt_data_dl      TYPE STANDARD TABLE OF gty_data_dl.

*eject
TYPES: BEGIN OF gty_data,
        ap_ulid          TYPE  c LENGTH 20, "Accounts Payable ULID
        inv_ulid         TYPE  c LENGTH 20, "Invoice ULID
        inv_num          TYPE  c LENGTH 16, "Invoice Number
        inv_dat          TYPE  c LENGTH 10, "Invoice Date
        inv_total        TYPE  c LENGTH 16, "Invoice Total
        inv_amt          TYPE  c LENGTH 16, "Invoice Amount Paid
        inv_src          TYPE  c LENGTH 20, "Invoice Source
        inv_type         TYPE  c LENGTH 02, "Invoice Type
        vchr_num         TYPE  c LENGTH 10, "Voucher Number
        ven_ulid         TYPE  c LENGTH 15, "Vendor ULID
        ven_num          TYPE  c LENGTH 10, "Vendor Number
        xrt_tc2rc        TYPE  c LENGTH 12, "FXRate Trans.Curr. to Rprt.
        trans_curr       TYPE  c LENGTH 03, "Transaction Currency
        comp_code        TYPE  c LENGTH 04, "Company Code
        paym_terms       TYPE  c LENGTH 04, "Payment Terms
        po_ind           TYPE  c LENGTH 03, "Purchase Order Indicator
        inv_rcvd         TYPE  c LENGTH 10, "Invoice Received Date
        inv_entry        TYPE  c LENGTH 10, "Invoice Entry Date
        paym_total       TYPE  c LENGTH 16, "Payment Total
        paym_date        TYPE  c LENGTH 10, "Payment Date
        ven_name         TYPE  c LENGTH 35, "Vendor Name
        bus_unit         TYPE  c LENGTH 04, "Business Unit
        erp_id           TYPE  c LENGTH 05, "Voucher Origin
        entrd_by         TYPE  c LENGTH 12, "Entered By
        comment          TYPE  c LENGTH 50, "Comment
        tax_amt          TYPE  c LENGTH 16, "Sales Tax Amount
        xrt_tc2ac        TYPE  c LENGTH 12, "FXRate Trans.Curr. to Actg.
        acctg_curr       TYPE  c LENGTH 03, "Accounting Currency
        urn              TYPE  c LENGTH 20, "URN
        link_url1        TYPE  c LENGTH 200, "Link URL 1
        link_typ1        TYPE  c LENGTH 20, "Link Type 1
        fisc_year        TYPE  c LENGTH 04, "Fiscal Year
        pstg_key         TYPE  c LENGTH 02, "Posting Key
        paym_stat        TYPE  c LENGTH 01, "Payment Status
        contract         TYPE  c LENGTH 13, "Contract Number
       END   OF gty_data,

        gtt_data         TYPE STANDARD TABLE OF  gty_data.

*eject
*=======================================================================
* GLOBAL CONSTANTS
*=======================================================================
CONSTANTS:
        gc_x             TYPE c                  VALUE 'X',
        gc_ucomm_onli    TYPE sscrfields-ucomm   VALUE 'ONLI',
        gc_cad           TYPE char3              VALUE 'CAD'.

*=======================================================================
* GLOBAL VARIABLES
*=======================================================================
DATA:   gv_filename      TYPE text255,
        gv_rec_count     TYPE i,
        gv_ulid          TYPE string,
        gv_vulid         TYPE string.

*=======================================================================
* GLOBAL STRUCTURES AND INTERNAL TABLES
*=======================================================================
DATA:   gt_bsik          TYPE gtt_bsik,
        gs_bsik          TYPE gty_bsik,
        gt_bkpf          TYPE gtt_bkpf,
        gs_bkpf          TYPE gty_bkpf,
        gt_lfa1          TYPE gtt_lfa1,
        gs_lfa1          TYPE gty_lfa1,
        gt_vbsegk        TYPE gtt_vbsegk,
        gs_vbsegk        TYPE gty_vbsegk,
        gt_vbkpf         TYPE gtt_vbkpf,
        gs_vbkpf         TYPE gty_vbkpf,
        gt_vlfa1         TYPE gtt_lfa1,
        gs_vlfa1         TYPE gty_lfa1,
        gt_data          TYPE gtt_data,
        gs_data          TYPE gty_data,
        gt_data_dl       TYPE gtt_data_dl,
        gs_data_dl       TYPE gty_data_dl.
