FUNCTION ZAP_IAP_INVOICE_POST .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(CHECK) TYPE  XFLAG
*"  TABLES
*"      IAP_HDR STRUCTURE  ZAPS_IAP_DOC_HDR
*"      IAP_ITEM STRUCTURE  ZAPS_IAP_DOC_ITEM OPTIONAL
*"      IAP_ACTG STRUCTURE  ZAPS_IAP_DOC_ACTG OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Function Module:  ZAP_IAP_INVOICE_POST                              *
*  Author:           Vijay Rajaputra                                   *
*  Date:             Aug, 01 2018                                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      Post IAP Documents for PO and Non-PO Invoices     *
*                    and Credit Memos                                  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 08/01/18 VRAJAPUTRA D30K928897 - CHG0109670 - Initial Development    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*-->LOcal Variables Declaration


  DATA : LT_ITEM_DATA      TYPE TABLE OF BAPI_INCINV_CREATE_ITEM,
         LT_TAXDATA        TYPE TABLE OF BAPI_INCINV_CREATE_TAX,
*         LT_WITHTAXDATA    TYPE TABLE OF BAPI_INCINV_CREATE_WITHTAX,
         LT_ACTG_DATA      TYPE TABLE OF BAPI_INCINV_CREATE_ACCOUNT,
         LT_GL_ACTG_DATA   TYPE TABLE OF BAPI_INCINV_CREATE_GL_ACCOUNT,
         LT_RETURN         TYPE TABLE OF BAPIRET2,
         LT_ACCOUNTPAYABLE TYPE TABLE OF BAPIACAP09,
         LT_CURRENCYAMOUNT TYPE TABLE OF BAPIACCR09,
         LT_ACCOUNTWT      TYPE TABLE OF BAPIACWT09,
         LT_ACCOUNTGL      TYPE TABLE OF BAPIACGL09,
         LT_ACCOUNTTAX     TYPE TABLE OF BAPIACTX09,
         LT_EXTN           TYPE TABLE OF BAPIACEXTC,
*         LT_DOC            TYPE TABLE OF ZAPS_IAP_DOC,
         LT_HEADER         TYPE TABLE OF ZAPS_IAP_DOC_HDR,
         LT_ITEM           TYPE TABLE OF ZAPS_IAP_DOC_ITEM,
         LT_ACTG           TYPE TABLE OF ZAPS_IAP_DOC_ACTG,

         LS_HEADER_DATA    TYPE BAPI_INCINV_CREATE_HEADER,
         LS_ITEM_DATA      TYPE BAPI_INCINV_CREATE_ITEM,
*         LS_TAXDATA        TYPE BAPI_INCINV_CREATE_TAX,
*         LS_WITHTAXDATA    TYPE BAPI_INCINV_CREATE_WITHTAX,
         LS_ACTG_DATA      TYPE BAPI_INCINV_CREATE_ACCOUNT,
         LS_GL_ACTG_DATA   TYPE BAPI_INCINV_CREATE_GL_ACCOUNT,
         LS_XREF_UPDATE    TYPE ZAPT_IAP_XREF_AD,  " to update entry in custom table
         LS_RETURN         TYPE BAPIRET2,
         LS_DOCUMENTHEADER TYPE BAPIACHE09,
         LS_ACCOUNTPAYABLE TYPE BAPIACAP09,
         LS_CURRENCYAMOUNT TYPE BAPIACCR09,
         LS_ACCOUNTWT      TYPE BAPIACWT09,
         LS_ACCOUNTGL      TYPE BAPIACGL09,
         LS_EXTN           TYPE BAPIACEXTC,
         LS_HEADER         TYPE ZAPS_IAP_DOC_HDR,
         LS_ITEM           TYPE ZAPS_IAP_DOC_ITEM,
         LS_ACTG           TYPE ZAPS_IAP_DOC_ACTG,

         LV_DOCCATG        TYPE C LENGTH 3,
         LV_TAXCODE        TYPE C LENGTH 2,
         LV_DOCUMENT       TYPE I,
         LV_BELNR          TYPE BELNR_D,
         LV_GJAHR          TYPE GJAHR,
         LV_RITM           TYPE Z_IAP_RITM_DOC.

  CLEAR : lv_belnr, lv_gjahr,lv_doccatg,lv_ritm,lv_document,
          lv_doccatg, lv_ritm, ls_header.


* Initial the data elements
  PERFORM  f_initial_data_elements.  " Clearing Global Variables
  CLEAR : lt_header[], lt_item[],lt_actg[]. " Clearing LOcal Variables.

  gv_check      = CHECK.
  gt_doc_hdr[]  = IAP_HDR[].
  gt_doc_item[] = IAP_ITEM[].
  gt_doc_actg[] = IAP_ACTG[].



* Prepare the document detail data
  PERFORM  f_prepare_document_detail_data.

  lt_header[] = IAP_HDR[].
  lt_item[]   = IAP_ITEM[].
  lt_actg[]   = IAP_ACTG[].

  read TABLE lt_header INTO ls_header INDEX 1.
  IF sy-subrc is INITIAL.
    lv_doccatg = ls_header-doccatg.
    lv_taxcode = ls_header-taxcode.
    lv_ritm = ls_header-RITM.
  ENDIF.

*---> Filling Data structure of BAPI

  IF lv_doccatg = gc_doccatg_po_inv " 'PO'
  OR lv_doccatg = gc_doccatg_po_cm . "'PCM'.   " PO Invoice and PO Credit Memo

* Header Data
    CLEAR ls_header.
    LOOP AT lt_header INTO ls_header.

      IF lv_doccatg = gc_doccatg_po_inv."'PO'.
        MOVE gc_x                  TO ls_header_data-invoice_ind. " Invoice Indicator = 'X' for PO
      ELSE.
        CLEAR ls_header_data-invoice_ind. " Invoice Indicator is space for PCM
      ENDIF.
      MOVE gc_x                    TO ls_header_data-SIMULATION .  " By Default First it need to trigger Simulation.
      MOVE ls_header-COMPCODE      TO ls_header_data-COMP_CODE. " Company Code
      MOVE ls_header-INVOICE       TO ls_header_data-REF_DOC_NO. " Reference Document Number
      MOVE ls_header-DOCTYPE       TO ls_header_data-DOC_TYPE. " Documnet type
      MOVE ls_header-INVCDATE      TO ls_header_data-DOC_DATE. " Invoice Date
      MOVE ls_header-POSTDATE      TO ls_header_data-PSTNG_DATE. " Posting type
      MOVE ls_header-HDRTEXT       TO ls_header_data-HEADER_TXT. " Documnet Header text
*    MOVE ls_header-SGLIND    to ls_header_data- . " Indicator: post invoice
*----> Need to adjust any standard Functionality
      REPLACE all OCCURRENCES OF ',' in ls_header-invcamnt WITH space." Amount Field formatting
      CONDENSE  ls_header-invcamnt.
*---->
      MOVE ls_header-INVCAMNT      TO ls_header_data-GROSS_AMOUNT. " Gross Invoice Amount in Document Currency
      MOVE ls_header-CURRCODE      TO ls_header_data-CURRENCY. " Currency Key
      MOVE ls_header-CALCTAX       TO ls_header_data-CALC_TAX_IND. " Calculate tax automatically
      MOVE ls_header-BLINEDATE     TO ls_header_data-BLINE_DATE. " Date
      MOVE ls_header-PAYMTERMS     TO ls_header_data-PMNTTRMS. " Terms of Payment Key
      MOVE ls_header-PAYMMTHD      TO ls_header_data-PYMT_METH.
      MOVE ls_header-PAYMMTHDSPLM  TO ls_header_data-PMTMTHSUPL.
      MOVE ls_header-PAYMBLOCK     TO ls_header_data-PMNT_BLOCK. " Payment Block Key
      MOVE ls_header-PAYEE         TO ls_header_data-PAYEE_PAYER.
      MOVE ls_header-PARTNRBNK     TO ls_header_data-PARTNER_BK.
      MOVE ls_header-HOUSEBNK      TO ls_header_data-HOUSEBANKID. " House Bank
      MOVE ls_header-UNPLDELVCOSTS TO ls_header_data-DEL_COSTS.
      MOVE ls_header-CASHDISCAMNT  TO ls_header_data-DSCT_AMOUNT.
*-->Tax
*      MOVE LS_HEADER-TAXAMNT       TO LS_TAXDATA-TAX_AMOUNT.
*      MOVE LS_HEADER-TAXCODE       TO LS_TAXDATA-TAX_CODE.
*      APPEND LS_TAXDATA            TO LT_TAXDATA.

*-->With Hold Tax
*      MOVE LS_HEADER-WTHTAXTYPE    TO LS_WITHTAXDATA-WI_TAX_TYPE.
*      MOVE LS_HEADER-WTHTAXCODE    TO LS_WITHTAXDATA-WI_TAX_CODE.
*      MOVE LS_HEADER-WTHBASAMNT    TO LS_WITHTAXDATA-WI_TAX_BASE.
*      MOVE LS_HEADER-WTHTAXAMNT    TO LS_WITHTAXDATA-WI_TAX_AMT.
*      MOVE LS_HEADER-WTHTAXTYPE2   TO LS_WITHTAXDATA-WI_TAX_TYPE.
*      MOVE LS_HEADER-WTHTAXCODE2   TO LS_WITHTAXDATA-WI_TAX_CODE.
*      MOVE LS_HEADER-WTHBASAMNT2   TO LS_WITHTAXDATA-WI_TAX_BASE.
*      MOVE LS_HEADER-WTHTAXAMNT2   TO LS_WITHTAXDATA-WI_TAX_AMT.
*      APPEND LS_WITHTAXDATA        TO LT_WITHTAXDATA.

*--> Custom Table Update
      ls_xref_update-ZZ_IAP_RITM_DOC   = ls_header-RITM.
      ls_xref_update-BUKRS             = ls_header-COMPCODE.
      ls_xref_update-XBLNR             = ls_header-INVOICE.
      ls_xref_update-AWTYP             = ls_header-doccatg.
*      CLEAR LS_HEADER.
*      CLEAR: LS_TAXDATA, LS_WITHTAXDATA.
    ENDLOOP.

*--> Item Details.
    LOOP AT lt_item INTO ls_item.
      "HDRSEQN
      MOVE LS_ITEM-ITEMSEQN   TO ls_item_data-INVOICE_DOC_ITEM. " Document Item in Invoice Document
      MOVE LS_ITEM-POHDR      TO ls_item_data-PO_NUMBER. " PO Number
      MOVE LS_ITEM-POITEM     TO ls_item_data-PO_ITEM. " PO Number
      MOVE LS_ITEM-REFDOC     TO ls_item_data-REF_DOC. " Ref Document
      MOVE LS_ITEM-REFDOCYEAR TO ls_item_data-REF_DOC_YEAR. " Ref Doc Year
      MOVE LS_ITEM-REFDOCITEM TO ls_item_data-REF_DOC_IT. "
      MOVE LS_ITEM-DBTCRD     TO ls_item_data-DE_CRE_IND. "
      MOVE LS_ITEM-TAXCODE    TO ls_item_data-TAX_CODE. "
*----> Need to adjust any standard Functionality
      " To avaoid Cooma's in currency
      REPLACE all OCCURRENCES OF ',' in ls_item-amount WITH space.
      CONDENSE  ls_item-amount.
*---->
      MOVE LS_ITEM-AMOUNT     TO ls_item_data-ITEM_AMOUNT.
      MOVE LS_ITEM-QUANTITY   TO ls_item_data-QUANTITY.
      MOVE LS_ITEM-POUOM      TO ls_item_data-PO_UNIT.
      MOVE LS_ITEM-SHEETNO    TO ls_item_data-SHEET_NO.
      MOVE LS_ITEM-GRIRCLEAR  TO ls_item_data-GRIR_CLEAR_SRV.
      MOVE LS_ITEM-ITEMDESC   TO ls_item_data-ITEM_TEXT. " Document Item in Invoice Document

      Append ls_item_data to lt_item_data.
      CLEAR ls_item_data.
    ENDLOOP.

*--> Accounting Details.
    LOOP AT lt_actg INTO ls_actg.

      IF ls_actg-ACTGSEQN = '999'. " Create GLACCOUNTDATA entries
        MOVE ls_actg-itemseqn  TO LS_GL_ACTG_DATA-INVOICE_DOC_ITEM. "
        MOVE LS_ACTG-DBTCRD    TO LS_GL_ACTG_DATA-DB_CR_IND. "
        MOVE LS_ACTG-TAXCODE   TO LS_GL_ACTG_DATA-TAX_CODE. " TAX CODE
        MOVE LS_ACTG-AMOUNT    TO LS_GL_ACTG_DATA-ITEM_AMOUNT. " AMOUNT
        MOVE LS_ACTG-QUANTITY  TO LS_GL_ACTG_DATA-QUANTITY. " QUANTITY
        MOVE LS_ACTG-COMPCODE  TO LS_GL_ACTG_DATA-COMP_CODE. "
        MOVE LS_ACTG-COSTCNTR  TO LS_GL_ACTG_DATA-COSTCENTER. "
        MOVE LS_ACTG-INTLORDR  TO LS_GL_ACTG_DATA-ORDERID.
        MOVE LS_ACTG-NETWORK   TO LS_GL_ACTG_DATA-NETWORK.
        MOVE LS_ACTG-ACTIVITY  TO LS_GL_ACTG_DATA-ACTIVITY.
        MOVE LS_ACTG-ITEMDESC  TO LS_GL_ACTG_DATA-ITEM_TEXT. "

        PERFORM f_gl_convert   USING LS_ACTG-GLACCT   " G/L ACCOUNT conversion
                            CHANGING LS_GL_ACTG_DATA-GL_ACCOUNT.
        PERFORM f_wbs_convert  USING LS_ACTG-WBSELEM   " Conversion External to Internal WBS Element Number
                            CHANGING LS_GL_ACTG_DATA-WBS_ELEM.



        APPEND LS_GL_ACTG_DATA TO LT_GL_ACTG_DATA.
        CLEAR LS_GL_ACTG_DATA.

      ELSE. " Create ACCOUNTINGDATA entries

        MOVE LS_ACTG-ITEMSEQN      TO LS_ACTG_DATA-INVOICE_DOC_ITEM. "

        IF LS_ACTG-ACCTASGN is INITIAL.
          CLEAR LS_ACTG_DATA-SERIAL_NO.  " Need to be Space
        ELSE.
          MOVE gc_x     TO LS_ACTG_DATA-XUNPL. "
        ENDIF.

        MOVE LS_ACTG-TAXCODE      TO LS_ACTG_DATA-TAX_CODE. " TAX CODE
        MOVE LS_ACTG-AMOUNT       TO LS_ACTG_DATA-ITEM_AMOUNT. " AMOUNT
        MOVE LS_ACTG-QUANTITY     TO LS_ACTG_DATA-QUANTITY. " QUANTITY
        MOVE LS_ACTG-POUOM        TO LS_ACTG_DATA-PO_UNIT. " UNIT
        MOVE LS_ACTG-COSTCNTR     TO LS_ACTG_DATA-COSTCENTER. "
        MOVE LS_ACTG-INTLORDR     TO LS_ACTG_DATA-ORDERID.
        MOVE LS_ACTG-NETWORK      TO LS_ACTG_DATA-NETWORK.
        MOVE LS_ACTG-ACTIVITY     TO LS_ACTG_DATA-ACTIVITY.
        PERFORM f_gl_convert   USING LS_ACTG-GLACCT          " G/L ACCOUNT Conversion to Internal format
                            CHANGING LS_ACTG_DATA-GL_ACCOUNT.
        PERFORM f_wbs_convert  USING LS_ACTG-WBSELEM        " Conversion External to Internal WBS Element Number
                            CHANGING LS_ACTG_DATA-WBS_ELEM.
        APPEND LS_ACTG_DATA       TO LT_ACTG_DATA.
        CLEAR LS_ACTG_DATA.
      ENDIF.
    ENDLOOP.
*-------------------------------------------------------------
*--> Tax Lines calculation UG Specific logic need to add
*-- As per Initail Testings No need of any Tax Calculations for PO.
*    BREAK vrajaputra.
*    CASE lv_taxcode.
*
*        " Group1 -- Similer Tax Codes : EM, ET, H1, HA, HB, HP, HT, HU, T2
*      WHEN gc_em or gc_et or gc_h1 or gc_ha or gc_hb or gc_hp or gc_ht or gc_hu or gc_t2.
*
*
*        " Group2 -- Similer Tax Codes : H4, M5, M7, Q2, T1, T3
*      WHEN  gc_h4 or gc_m5 or gc_m7 or gc_q2 or gc_t1 or gc_t3.
*
**          CALL FUNCTION 'ZAP_IAP_UG_TAXCODE_PO_GRP2'
**            EXPORTING
**              HEADER_DATA = ls_header
**            TABLES
**              ITEM_DATA   = lt_item_data
**              TAXDATA     = lt_taxdata.
*
*        " Group3 -- Similer Tax Codes : JC, JZ, RC, RE, RZ, SC, SZ
*      WHEN gc_jc or gc_jz or gc_rc or gc_re or gc_rz or gc_sc or gc_sz.
*
*        " Group4 -- Tax Code : HI
*      WHEN gc_hi.
*
*    ENDCASE.
*-------------------------------------------------------------

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE1'
      EXPORTING
        HEADERDATA                = ls_header_data
*   ADDRESSDATA               =
   INVOICESTATUS             = '5'
* IMPORTING
*   INVOICEDOCNUMBER          =
*   FISCALYEAR                =
      TABLES
        ITEMDATA                = lt_item_data
*      ACCOUNTINGDATA            = lt_actg_data
*      GLACCOUNTDATA             = lt_gl_actg_data
*   MATERIALDATA              =
     TAXDATA                   = lt_TAXDATA
*     WITHTAXDATA               = lt_WITHTAXDATA
*   VENDORITEMSPLITDATA       =
        RETURN                    = lt_return
*    EXTENSIONIN               =
*   EXTENSIONOUT              =
*   TM_ITEMDATA               =
              .
*--> Testing purpose

    SORT lt_return by type.

    CLEAR : lv_belnr, lv_gjahr.

    IF check = SPACE AND lt_return is INITIAL. " Posting and No errors while simulation
      CLEAR  ls_header_data-SIMULATION.  " For Posting Simulation Check is Clear

      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE1'
        EXPORTING
          HEADERDATA          = ls_header_data
*         ADDRESSDATA         =
          INVOICESTATUS       = '5'
        IMPORTING
          INVOICEDOCNUMBER    = lv_belnr
          FISCALYEAR          = lv_gjahr
        TABLES
          ITEMDATA            = lt_item_data
*         ACCOUNTINGDATA      = lt_actg_data
*         GLACCOUNTDATA       = lt_gl_actg_data
*         MATERIALDATA        =
          TAXDATA             = lt_TAXDATA
*         WITHTAXDATA         = lt_WITHTAXDATA
*         VENDORITEMSPLITDATA =
          RETURN              = lt_return
*         EXTENSIONIN         =
*         EXTENSIONOUT        =
*         TM_ITEMDATA         =
        .
*      ENDIF.


      IF lt_return is INITIAL and  lv_belnr is NOT INITIAL .
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
           WAIT          = gc_x "'X'
*         IMPORTING
*           RETURN        =
                  .
*--> Update the RITM Number in custom table.
        CLEAR : ls_xref_update.
        ls_xref_update-ZZ_IAP_RITM_DOC  = lv_ritm.
        ls_xref_update-BUKRS = ls_header_data-COMP_CODE.
        ls_xref_update-BELNR =  lv_belnr.
        ls_xref_update-GJAHR =  lv_GJAHR.
        ls_xref_update-XBLNR = ls_header_data-REF_DOC_NO.
        ls_xref_update-AWTYP = gc_awtyp_rmrp." 'RMRP'.
        CONCATENATE lv_belnr lv_GJAHR INTO ls_xref_update-AWKEY.
        CONDENSE ls_xref_update-AWKEY .
        ls_xref_update-DOCCATG = lv_doccatg.
        INSERT ZAPT_IAP_XREF_AD  FROM ls_xref_update.

*--> Return Table Update with Document Details.
        CLEAR : ls_return.
        ls_return-type = gc_s."'S'.
        CONCATENATE lv_ritm text-000 lv_belnr INTO ls_return-message.
        APPEND ls_return to lt_return.
        CLEAR ls_return.
      ENDIF.
    ENDIF.


  ELSEIF lv_doccatg = gc_doccatg_npo_inv " 'NPO'
      or lv_doccatg = gc_doccatg_npo_cm. " 'NCM'

* Header Data

    CLEAR ls_header.
    LOOP AT LT_HEADER INTO LS_HEADER.
      MOVE LS_HEADER-COMPCODE      TO LS_DOCUMENTHEADER-COMP_CODE. " COMPANY CODE
      MOVE LS_HEADER-INVOICE       TO LS_DOCUMENTHEADER-REF_DOC_NO. " REFERENCE DOCUMENT NUMBER
      MOVE LS_HEADER-DOCTYPE       TO LS_DOCUMENTHEADER-DOC_TYPE. " DOCUMNET TYPE
      MOVE LS_HEADER-INVCDATE      TO LS_DOCUMENTHEADER-DOC_DATE. " INVOICE DATE
      MOVE LS_HEADER-POSTDATE      TO LS_DOCUMENTHEADER-PSTNG_DATE. " POSTING TYPE
      MOVE LS_HEADER-PERIOD        TO LS_DOCUMENTHEADER-FIS_PERIOD. " POSTING TYPE
      MOVE LS_HEADER-HDRTEXT       TO LS_DOCUMENTHEADER-HEADER_TXT. " DOCUMNET HEADER TEXT
      MOVE SY-UNAME                TO LS_DOCUMENTHEADER-USERNAME. " Testing Purpose

*--> Vendor Line Bulid
      LV_DOCUMENT = LV_DOCUMENT + 1.
      MOVE LV_DOCUMENT             TO  LS_ACCOUNTPAYABLE-ITEMNO_ACC.  " Sequence Number Generation.

      PERFORM f_vendor_convert   USING LS_HEADER-SUPPLIER   " Vendor Conversion
                              CHANGING LS_ACCOUNTPAYABLE-VENDOR_NO.

*      MOVE LS_HEADER-SUPPLIER      TO  LS_ACCOUNTPAYABLE-VENDOR_NO. "
      MOVE LS_HEADER-COMPCODE      TO LS_ACCOUNTPAYABLE-COMP_CODE. " COMPANY CODE
      MOVE LS_HEADER-SGLIND        TO LS_ACCOUNTPAYABLE-SP_GL_IND. " INDICATOR: POST INVOICE
*      MOVE LS_HEADER-TAXAMNT       TO LS_ACCOUNTPAYABLE-TAX_AMOUNT.
*      IF LS_HEADER-TAXCODE = gc_u1."'U1'." or 'I1'.
*        MOVE 'GBP'                   TO LS_ACCOUNTPAYABLE-PYMT_CUR.  " FOT U1 Added on 22-06
*      ELSE.
      MOVE 'USD'                   TO LS_ACCOUNTPAYABLE-PYMT_CUR.  " FOT A2 Added on 25-07
*      ENDIF.

      MOVE LS_HEADER-INVCAMNT      TO LS_ACCOUNTPAYABLE-PYMT_AMT.  " FOT U1 Added on 22-06

      MOVE LS_HEADER-TAXCODE       TO LS_ACCOUNTPAYABLE-TAX_CODE.
      MOVE LS_HEADER-BLINEDATE     TO LS_ACCOUNTPAYABLE-BLINE_DATE. " DATE
      MOVE LS_HEADER-PAYMTERMS     TO LS_ACCOUNTPAYABLE-PMNTTRMS. " TERMS OF PAYMENT KEY
      MOVE LS_HEADER-PAYMMTHD      TO LS_ACCOUNTPAYABLE-PYMT_METH.
      MOVE LS_HEADER-PAYMMTHDSPLM  TO LS_ACCOUNTPAYABLE-PMTMTHSUPL.
      MOVE LS_HEADER-PAYMBLOCK     TO LS_ACCOUNTPAYABLE-PMNT_BLOCK. " PAYMENT BLOCK KEY
      MOVE LS_HEADER-PAYEE         TO LS_ACCOUNTPAYABLE-ALT_PAYEE.
      MOVE LS_HEADER-PARTNRBNK     TO LS_ACCOUNTPAYABLE-PARTNER_BK.
      MOVE LS_HEADER-HOUSEBNK      TO LS_ACCOUNTPAYABLE-BANK_ID. " HOUSE BANK
      APPEND LS_ACCOUNTPAYABLE     TO LT_ACCOUNTPAYABLE.

*      MOVE LV_DOCUMENT           TO LS_ACCOUNTWT-ITEMNO_ACC.  " Sequence Number
      MOVE LS_HEADER-WTHTAXTYPE  TO LS_ACCOUNTWT-WT_TYPE.
      MOVE LS_HEADER-WTHTAXCODE  TO LS_ACCOUNTWT-WT_CODE.
      MOVE LS_HEADER-WTHBASAMNT  TO LS_ACCOUNTWT-BAS_AMT_TC.
      MOVE LS_HEADER-WTHTAXAMNT  TO LS_ACCOUNTWT-MAN_AMT_TC.
      MOVE LS_HEADER-WTHTAXTYPE2 TO LS_ACCOUNTWT-WT_TYPE.
      MOVE LS_HEADER-WTHTAXCODE2 TO LS_ACCOUNTWT-WT_CODE.
      MOVE LS_HEADER-WTHBASAMNT2 TO LS_ACCOUNTWT-BAS_AMT_TC.
      MOVE LS_HEADER-WTHTAXAMNT2 TO LS_ACCOUNTWT-MAN_AMT_TC.
      APPEND LS_ACCOUNTWT        TO LT_ACCOUNTWT.

      MOVE LV_DOCUMENT             TO LS_CURRENCYAMOUNT-ITEMNO_ACC.  " Sequence Number
      MOVE LS_HEADER-INVCAMNT      TO LS_CURRENCYAMOUNT-AMT_DOCCUR. " GROSS INVOICE AMOUNT IN DOCUMENT CURRENCY
      MOVE LS_HEADER-TAXAMNT       TO LS_CURRENCYAMOUNT-TAX_AMT.
      MOVE LS_HEADER-INVCAMNT      TO LS_CURRENCYAMOUNT-AMT_BASE.
      MOVE LS_HEADER-CURRCODE      TO LS_CURRENCYAMOUNT-CURRENCY. " CURRENCY KEY
      MOVE LS_HEADER-CURRCODE      TO LS_CURRENCYAMOUNT-CURRENCY_ISO. " CURRENCY ISO KEY
      LS_CURRENCYAMOUNT-AMT_DOCCUR = LS_CURRENCYAMOUNT-AMT_DOCCUR * -1 .  " Amount is -Ve
      LS_CURRENCYAMOUNT-TAX_AMT    = LS_CURRENCYAMOUNT-TAX_AMT  * -1.




      MOVE LS_HEADER-CASHDISCAMNT  TO LS_CURRENCYAMOUNT-DISC_AMT.
*    MOVE LS_HEADER-CALCTAX       TO LS_HEADER_DATA-CALC_TAX_IND. " CALCULATE TAX AUTOMATICALLY
      APPEND LS_CURRENCYAMOUNT TO LT_CURRENCYAMOUNT.

      MOVE ls_header-ROUTECODE      TO ls_ACCOUNTGL-ref_Key_3.
*      APPEND ls_ACCOUNTGL to lt_ACCOUNTGL.

      CLEAR: ls_ACCOUNTPAYABLE, ls_ACCOUNTWT.
    ENDLOOP.

    CLEAR : LS_CURRENCYAMOUNT-TAX_AMT,LS_CURRENCYAMOUNT-AMT_BASE.

*--> Accounting

    LOOP AT lt_actg INTO ls_actg.

      LV_DOCUMENT = LV_DOCUMENT + 1.
      MOVE LV_DOCUMENT       TO  LS_ACCOUNTGL-ITEMNO_ACC.  " Sequence Number Generation.
      MOVE LS_ACTG-DBTCRD    TO ls_ACCOUNTGL-DE_CRE_IND. "
      MOVE LS_ACTG-TAXCODE   TO ls_ACCOUNTGL-TAX_CODE. " TAX CODE
      MOVE LV_DOCUMENT       TO LS_CURRENCYAMOUNT-ITEMNO_ACC.  " Sequence Number
      MOVE LS_ACTG-AMOUNT    TO ls_CURRENCYAMOUNT-AMT_DOCCUR. " AMOUNT
      MOVE LS_ACTG-AMOUNT    TO ls_CURRENCYAMOUNT-AMT_BASE. " Base AMOUNT
      MOVE LS_ACTG-COMPCODE  TO ls_ACCOUNTGL-COMP_CODE. "
      MOVE LS_ACTG-INTLORDR  TO ls_ACCOUNTGL-ORDERID.
      MOVE LS_ACTG-WBSELEM   TO ls_ACCOUNTGL-WBS_ELEMENT.
      MOVE LS_ACTG-NETWORK   TO ls_ACCOUNTGL-NETWORK.
      MOVE LS_ACTG-ACTIVITY  TO ls_ACCOUNTGL-ACTIVITY.
      MOVE LS_ACTG-ITEMDESC  TO ls_ACCOUNTGL-ITEM_TEXT.


      PERFORM f_gl_convert         USING LS_ACTG-GLACCT           " GL Account to Internal format Conversion
                                CHANGING ls_ACCOUNTGL-GL_ACCOUNT.

      PERFORM f_costcenter_convert USING LS_ACTG-COSTCNTR         " COST CENTER to Internal Format Conversion
                                CHANGING ls_ACCOUNTGL-COSTCENTER.


      APPEND ls_ACCOUNTGL TO lt_ACCOUNTGL.
      APPEND ls_CURRENCYAMOUNT TO lt_CURRENCYAMOUNT.

      CLEAR ls_ACCOUNTGL.
      IF LS_ACTG-LCTNCODE IS NOT INITIAL.       " Extension Fields Updation
        ls_extn-FIELD1 = 'LOCATION'.
        ls_extn-FIELD2 = LS_ACTG-LCTNCODE.
        ls_extn-FIELD3 = LV_DOCUMENT.
      ENDIF.

      APPEND  ls_extn to lt_extn.
    ENDLOOP.


*---------->Tax Lines Logic.

    CASE lv_taxcode.
        " Group1 -- Similer Tax Codes : EM, ET, H1, HA, HB, HP, HT, HU, T2
      WHEN gc_em or gc_et or gc_h1 or gc_ha or
           gc_hb or gc_hp or gc_ht or gc_hu or gc_t2.
        break vrajaputra.
        CALL FUNCTION 'ZAP_IAP_UG_TAXCODE_GRP1'
          EXPORTING
            HEADER_DATA    = ls_header
          TABLES
            GL_LINES       = lt_accountgl
            CURRENCYAMOUNT = lt_currencyamount
            ACCOUNTTAX     = lt_ACCOUNTTAX.

        " Group2 -- Similer Tax Codes : H4, M5, M7, Q2, T1, T3
      WHEN gc_h4 or gc_m5 or gc_m7 or gc_q2 or gc_t1 or gc_t3.
        CALL FUNCTION 'ZAP_IAP_UG_TAXCODE_GRP2'
          EXPORTING
            HEADER_DATA    = ls_header
          TABLES
            GL_LINES       = lt_accountgl
            CURRENCYAMOUNT = lt_currencyamount
            ACCOUNTTAX     = lt_ACCOUNTTAX.

        " Group3 -- Similer Tax Codes : JC, JZ, RC, RE, RZ, SC, SZ
      WHEN gc_jc or gc_jz or gc_rc or gc_re or gc_rz or gc_sc or gc_sz.
        CALL FUNCTION 'ZAP_IAP_UG_TAXCODE_GRP3'
          EXPORTING
            HEADER_DATA    = ls_header
          TABLES
            GL_LINES       = lt_accountgl
            CURRENCYAMOUNT = lt_currencyamount
            ACCOUNTTAX     = lt_ACCOUNTTAX.

      WHEN gc_hi. " Group4 -- Tax Code : HI
        CALL FUNCTION 'ZAP_IAP_UG_TAXCODE_GRP4'
          EXPORTING
            HEADER_DATA    = ls_header
          TABLES
            GL_LINES       = lt_accountgl
            CURRENCYAMOUNT = lt_currencyamount
            ACCOUNTTAX     = lt_ACCOUNTTAX.
    ENDCASE.

*------> Tax Lines Logic

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        DOCUMENTHEADER    = ls_DOCUMENTHEADER
*       CUSTOMERCPD       =
*       CONTRACTHEADER    =
      TABLES
        ACCOUNTGL         = lt_ACCOUNTGL
*       ACCOUNTRECEIVABLE =
        ACCOUNTPAYABLE    = lt_ACCOUNTPAYABLE
*       ACCOUNTTAX        = lt_ACCOUNTTAX
*       CURRENCYAMOUNT    = lt_CURRENCYAMOUNT  " No currency first time check
*       CRITERIA          =
*       VALUEFIELD        =
        EXTENSION1        = lt_extn
        RETURN            = lt_return
*       PAYMENTCARD       =
*       CONTRACTITEM      =
*       EXTENSION2        =
*       REALESTATE        =
        ACCOUNTWT         = lt_ACCOUNTWT.

    CLEAR ls_return.
    READ TABLE lt_return INTO ls_return with key type = gc_e."'E'.

    IF sy-subrc is INITIAL.
      return[] = lt_return[].  " Returning Messages.
      EXIT.
    ELSE.
      CLEAR lt_return.
    ENDIF.
*--> Call BAPI with currency Amounts

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        DOCUMENTHEADER    = ls_DOCUMENTHEADER
*       CUSTOMERCPD       =
*       CONTRACTHEADER    =
      TABLES
        ACCOUNTGL         = lt_ACCOUNTGL
*       ACCOUNTRECEIVABLE =
        ACCOUNTPAYABLE    = lt_ACCOUNTPAYABLE
        ACCOUNTTAX        = lt_ACCOUNTTAX
        CURRENCYAMOUNT    = lt_CURRENCYAMOUNT
*       CRITERIA          =
*       VALUEFIELD        =
        EXTENSION1        = lt_extn
        RETURN            = lt_return
*       PAYMENTCARD       =
*       CONTRACTITEM      =
*       EXTENSION2        =
*       REALESTATE        =
*       ACCOUNTWT         =                                             lt_ACCOUNTWT
      .

    CLEAR ls_return.
    READ TABLE lt_return INTO ls_return with KEY type = gc_e."'E'.
    IF sy-subrc is INITIAL.
      return[] = lt_return[].  " Returning Messages.
      exit.
    else.  " NO error Occurs the Post the Document

*--> Need to check for RITM is already Processed or not.
*      CLEAR : ls_xref_update.   " Nott Required as this check happening at Wrapper program
*      SELECT SINGLE * from ZAPT_IAP_XREF_AD
*                      into ls_xref_update
*                     WHERE ZZ_IAP_RITM_DOC = lv_ritm.
*      IF sy-subrc is INITIAL.  " If Entry Exists then no posting
*
*        CLEAR : ls_return.
*        ls_return-type = 'E'.
*        CONCATENATE lv_ritm ': Already Processed :' INTO ls_return-message.
*        APPEND ls_return to lt_return.
*        CLEAR ls_return.
*
*      else.

      IF CHECK = SPACE. " POST after Simulation, no errors then only
        CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          DOCUMENTHEADER          = ls_DOCUMENTHEADER
*           CUSTOMERCPD             =
*           CONTRACTHEADER          =
*         IMPORTING
*           OBJ_TYPE                =
*           OBJ_KEY                 =
*           OBJ_SYS                 =
        TABLES
         ACCOUNTGL               = lt_ACCOUNTGL
*          ACCOUNTRECEIVABLE       =
         ACCOUNTPAYABLE          = lt_ACCOUNTPAYABLE
         ACCOUNTTAX              = lt_ACCOUNTTAX
         CURRENCYAMOUNT          =  lt_CURRENCYAMOUNT
*          CRITERIA                =
*          VALUEFIELD              =
       EXTENSION1                = lt_extn
        RETURN                  = lt_return
*         PAYMENTCARD             =
*         CONTRACTITEM            =
*         EXTENSION2              =
*         REALESTATE              =
*          ACCOUNTWT                = lt_ACCOUNTWT
                .

        CLEAR ls_return.
        READ TABLE lt_return into ls_return with key type = gc_e."'E'.
        IF sy-subrc is NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
             WAIT          = gc_x " 'X'
*         IMPORTING
*           RETURN        =
                    .
*--> Update the RITM Number in custom table.
          CLEAR ls_return.
          READ TABLE lt_return into ls_return with key type = gc_s."'S'.
          IF sy-subrc is INITIAL.
            CLEAR : ls_xref_update.
            ls_xref_update-ZZ_IAP_RITM_DOC  = lv_ritm.
            ls_xref_update-BUKRS = ls_return-message_v2+10(4).
            ls_xref_update-BELNR = ls_return-message_v2+0(10).
            ls_xref_update-GJAHR = ls_return-message_v2+14(4).
            ls_xref_update-XBLNR = LS_DOCUMENTHEADER-REF_DOC_NO.
            CONCATENATE ls_xref_update-BELNR ls_xref_update-GJAHR INTO ls_xref_update-AWKEY.
            CONDENSE ls_xref_update-AWKEY.
            ls_xref_update-AWTYP = gc_awtyp_bkpf."'BKPF'.
            ls_xref_update-DOCCATG = lv_doccatg.
            INSERT ZAPT_IAP_XREF_AD  FROM ls_xref_update.
          ENDIF.
        ENDIF.
      ENDIF.
*      ENDIF.
    ENDIF.
  ENDIF.    " Document Category Check
  CLEAR return[].
  return[] = lt_return[].  " Returning Messages.


ENDFUNCTION.
