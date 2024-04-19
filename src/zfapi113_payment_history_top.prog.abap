*&---------------------------------------------------------------------*
*&  Include           ZFAPI113_PAYMENT_HISTORY_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
*----------------------------------------------------------------------*
* Program Name       :  ZFAPI113_PAYMENT_HISTORY                       *
* Include            :  ZFAPI113_PAYMENT_HISTORY_TOP                   *
* Author             :  Kalinga Keshari Rout                           *
* Creation Date      :  February 01, 2018                              *
* Object ID          :                                                 *
* Application Area   :  FICO                                           *
* Description        :  The Payment History interface lists all paid   *
*                       invoices with their corresponding payment      *
*                       information.  It includes all payments that    *
*                       have been processed from Checks, EFT Payments, *
*                       Wire Transfers or any other form of payment.   *
*                       The Payment History file should include one    *
*                       record per invoice AND payment.  Multiple      *
*                       invoices paid on the same payment should have  *
*                       one record for each invoice.  Multiple         *
*                       payments for the same invoice should have one  *
*                       record for each payment.                       *
*----------------------------------------------------------------------*
*                       Modification Log                               *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 09-Apr-2018  KROUT       D30K928652  CHG0100818#Initial development  *
*                          D30K928716, D30K928741, D30K928766,         *
*                          D30K928774, D30K928886                      *
*----------------------------------------------------------------------*

*eject
TYPES : BEGIN OF gty_bsak,
         bukrs         TYPE bukrs,
         lifnr         TYPE lifnr,
         umsks         TYPE umsks,
         umskz         TYPE umskz,
         augdt         TYPE augdt,
         augbl         TYPE augbl,
         zuonr         TYPE dzuonr,
         gjahr         TYPE gjahr,
         belnr         TYPE belnr_d,
         buzei         TYPE buzei,
         budat         TYPE budat,
         bldat         TYPE bldat,
         cpudt         TYPE cpudt,
         waers         TYPE waers,
         xblnr         TYPE xblnr1,
         blart         TYPE blart,
         bschl         TYPE bschl,
         shkzg         TYPE shkzg,
         gsber         TYPE gsber,
         dmbtr         TYPE dmbtr,
         wrbtr         TYPE wrbtr,
         mwsts         TYPE mwsts,
         sgtxt         TYPE sgtxt,
         zfbdt         TYPE dzfbdt,
         zterm         TYPE dzterm,
         zbd1t         TYPE dzbd1t,
         zbd2t         TYPE dzbd2t,
         zbd3t         TYPE dzbd3t,
         sknto         TYPE sknto,
         wskto         TYPE wskto,
         zlsch         TYPE dzlsch,
         rebzg         TYPE rebzg,
         bstat         TYPE bstat_d,
         dmbe2         TYPE dmbe2,
         penfc         TYPE penalty,
         vertn         TYPE ranl,
        END   OF gty_bsak .

TYPES : gtt_bsak       TYPE STANDARD TABLE OF gty_bsak .

*eject
TYPES : BEGIN OF gty_lfa1,
         lifnr         TYPE lifnr,
         name1         TYPE name1_gp,
        END   OF gty_lfa1 .

TYPES : gtt_lfa1       TYPE STANDARD TABLE OF gty_lfa1 .

TYPES : BEGIN OF gty_bkpf,
         bukrs         TYPE bukrs,
         belnr         TYPE belnr_d,
         gjahr         TYPE gjahr,
         cpudt         TYPE cpudt,
         usnam         TYPE usnam,
         tcode         TYPE tcode,
         kursf         TYPE kursf,
         awtyp         TYPE awtyp,
         awkey         TYPE awkey,
         hwaer         TYPE hwaer,
         hwae2         TYPE hwae2,
        END   OF gty_bkpf .

TYPES : gtt_bkpf       TYPE STANDARD TABLE OF gty_bkpf .

TYPES : gty_toa01      TYPE toa01 .

TYPES : gtt_toa01      TYPE STANDARD TABLE OF gty_toa01 .

TYPES : BEGIN OF gty_initial,
         bukrs         TYPE bukrs,
         lifnr         TYPE lifnr,
         augdt         TYPE augdt,
         augbl         TYPE augbl,
        END   OF gty_initial .

TYPES : gtt_initial    TYPE STANDARD TABLE OF gty_initial .

*eject
TYPES : BEGIN OF gty_final,
         ulid          TYPE text40,        " PaymentHistoryULID
         culid         TYPE text20,        " ChequeULID
         augbl         TYPE augbl,         " PaymentNumber
         wrbtr         TYPE text20,        " PaymentTotal
         paydat        TYPE text10,        " PaymentDate
         zlsch         TYPE dzlsch,        " PaymentMethod
         iulid         TYPE text20,        " InvoiceULID
         xblnr         TYPE xblnr1,        " InvoiceNumber
         invdat        TYPE text10,        " InvoiceDate
         iwrbtr        TYPE text20,        " InvoiceTotal
         twrbtr        TYPE text20,        " InvoiceAmountPaid
         tcode         TYPE tcode,         " InvoiceSource
         blart         TYPE blart,         " InvoiceType
         vulid         TYPE text20,        " VendorULID
         lifnr         TYPE lifnr,         " VendorNumber
         xrt_tc2rc     TYPE text20,        " ExchangeRateTrxToRpt
         waers         TYPE waers,         " TransactionCurrencyCountryCode
         bukrs         TYPE bukrs,         " CompanyCode
         zterm         TYPE dzterm,        " PaymentTerms
         belnr         TYPE belnr_d,       " VoucherNumber
         poind         TYPE char3,         " PurchaseOrderIndicator
         payduedat     TYPE text10,        " PaymentDueDate
         paydscdat     TYPE text10,        " PaymentDiscountDueDate
         wskto         TYPE text20,        " PaymentDiscountAmount
         invrcvdat     TYPE text10,        " InvoiceReceivedDate
         name1         TYPE name1_gp,      " VendorName
         gsber         TYPE gsber,         " BusinessUnit
         usnam         TYPE usnam,         " EnteredBy
         origin        TYPE char5,         " VoucherOrigin
         penfc         TYPE text20,        " LatePaymentFees
         aulid         TYPE text20,        " APULID
         inventdat     TYPE text10,        " InvoiceEntryDate
         postdat       TYPE text10,        " PostDate
         cleardat      TYPE text10,        " ClearDate
         bstat         TYPE bstat_d,       " PaymentStatus
         sgtxt         TYPE sgtxt,         " Comment
         mwsts         TYPE text20,        " SalesTaxTotal
         sknto         TYPE text20,        " CashDiscountAmount
         xrt_tc2ac     TYPE text20,        " ExchangeRateTrxToAcctg
         hwaer         TYPE hwaer,         " AccountingCurrencyCountryCode
         urn           TYPE text20,        " URN
         linkurl1      TYPE saeardoid,     " LinkURL1
         linktype1     TYPE text12,        " LinkType1
         gjahr         TYPE gjahr,         " FiscalYear
         bschl         TYPE bschl,         " PostingKey
         invappdat     TYPE text10,        " InvoiceApprovalDate
         vertn         TYPE ranl,          " ContractNumber
        END   OF gty_final .

TYPES : gtt_final      TYPE STANDARD TABLE OF gty_final .

*eject
TYPES : BEGIN OF gty_header,
         ulid          TYPE string,        " PaymentHistoryULID
         culid         TYPE string,        " ChequeULID
         augbl         TYPE string,        " PaymentNumber
         wrbtr         TYPE string,        " PaymentTotal
         paydat        TYPE string,        " PaymentDate
         zlsch         TYPE string,        " PaymentMethod
         iulid         TYPE string,        " InvoiceULID
         xblnr         TYPE string,        " InvoiceNumber
         invdat        TYPE string,        " InvoiceDate
         iwrbtr        TYPE string,        " InvoiceTotal
         twrbtr        TYPE string,        " InvoiceAmountPaid
         isource       TYPE string,        " InvoiceSource
         blart         TYPE string,        " InvoiceType
         vulid         TYPE string,        " VendorULID
         lifnr         TYPE string,        " VendorNumber
         xrt_tc2rc     TYPE string,        " ExchangeRateTrxToRpt
         waers         TYPE string,        " TransactionCurrencyCountryCode
         bukrs         TYPE string,        " CompanyCode
         zterm         TYPE string,        " PaymentTerms
         belnr         TYPE string,        " VoucherNumber
         poind         TYPE string,        " PurchaseOrderIndicator
         payduedat     TYPE string,        " PaymentDueDate
         paydscdat     TYPE string,        " PaymentDiscountDueDate
         wskto         TYPE string,        " PaymentDiscountAmount
         invrcvdat     TYPE string,        " InvoiceReceivedDate
         name1         TYPE string,        " VendorName
         gsber         TYPE string,        " BusinessUnit
         usnam         TYPE string,        " EnteredBy
         origin        TYPE string,        " VoucherOrigin
         penfc         TYPE string,        " LatePaymentFees
         aulid         TYPE string,        " APULID
         inventdat     TYPE string,        " InvoiceEntryDate
         postdat       TYPE string,        " PostDate
         cleardat      TYPE string,        " ClearDate
         bstat         TYPE string,        " PaymentStatus
         sgtxt         TYPE string,        " Comment
         mwsts         TYPE string,        " SalesTaxTotal
         sknto         TYPE string,        " CashDiscountAmount
         xrt_tc2ac     TYPE string,        " ExchangeRateTrxToAcctg
         hwaer         TYPE string,        " AccountingCurrencyCountryCode
         urn           TYPE string,        " URN
         linkurl1      TYPE string,        " LinkURL1
         linktype1     TYPE string,        " LinkType1
         gjahr         TYPE string,        " FiscalYear
         bschl         TYPE string,        " PostingKey
         invappdat     TYPE string,        " InvoiceApprovalDate
         vertn         TYPE string,        " ContractNumber
        END   OF gty_header .

*eject
CONSTANTS :
       gc_x            TYPE c                VALUE 'X' ,
       gc_cad          TYPE waers            VALUE 'CAD' ,
       gc_ucomm_onli   TYPE sscrfields-ucomm VALUE 'ONLI' .

DATA : gv_filename     TYPE text255,
       gv_rec_count    TYPE i .

DATA : gt_t052         TYPE STANDARD TABLE OF t052,
       gs_t052         TYPE t052,
       gt_initial      TYPE gtt_initial,
       gt_detail       TYPE gtt_bsak,
       gs_detail       TYPE gty_bsak,
       gt_final        TYPE gtt_final,
       gs_final        TYPE gty_final,
       gt_lfa1         TYPE gtt_lfa1,
       gs_lfa1         TYPE gty_lfa1 .
