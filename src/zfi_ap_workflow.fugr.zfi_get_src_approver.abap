FUNCTION zfi_get_src_approver.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_INVOICE) TYPE  RBKP-BELNR
*"     REFERENCE(IMP_YEAR) TYPE  RBKP-GJAHR
*"  EXPORTING
*"     REFERENCE(EXP_SRCAPPROVER) TYPE  EKKO-ZZARIBA_APPROVER
*"----------------------------------------------------------------------

  DATA :  lv_ebeln          LIKE ekko-ebeln,
          lv_aribaapprover  TYPE z_ariba_approver,
          lv_user           TYPE xubname.

  DATA:
      s_headerdata        LIKE bapi_incinv_detail_header,
      s_addressdata       LIKE bapi_incinv_detail_addressdata,
      tab_itemdata        LIKE bapi_incinv_detail_item OCCURS 0 WITH HEADER LINE ,
      tab_accountingdata  LIKE bapi_incinv_detail_account OCCURS 0,
      tab_glaccountdata   LIKE bapi_incinv_detail_gl_account OCCURS 0,
      tab_materialdata    LIKE bapi_incinv_detail_material OCCURS 0,
      tab_taxdata         LIKE bapi_incinv_detail_tax OCCURS 0,
      tab_withtaxdata     LIKE bapi_incinv_detail_withtax OCCURS 0,
      tab_vendorsplitdata LIKE bapi_incinv_detail_vendorsplit OCCURS 0,
      tab_return          LIKE bapiret2 OCCURS 0.

  CALL FUNCTION 'BAPI_INCOMINGINVOICE_GETDETAIL'
    EXPORTING
      invoicedocnumber    = imp_invoice
      fiscalyear          = imp_year
    IMPORTING
      headerdata          = s_headerdata
      addressdata         = s_addressdata
    TABLES
      itemdata            = tab_itemdata
      accountingdata      = tab_accountingdata
      glaccountdata       = tab_glaccountdata
      materialdata        = tab_materialdata
      taxdata             = tab_taxdata
      withtaxdata         = tab_withtaxdata
      vendoritemsplitdata = tab_vendorsplitdata
      return              = tab_return.

  LOOP AT tab_itemdata.
    lv_ebeln = tab_itemdata-po_number.
  ENDLOOP.

  "Get Approver
  SELECT SINGLE zzariba_approver
    FROM ekko
    INTO lv_aribaapprover
    WHERE ebeln = lv_ebeln
  .

  SELECT SINGLE zsvr_conf_sap_id
    FROM zaribaaprv
    INTO lv_user
    WHERE zariba_approver = lv_aribaapprover
  .

  CONCATENATE 'US' lv_user INTO exp_srcapprover.
ENDFUNCTION.
