*----------------------------------------------------------------------*
***INCLUDE ZTAX_VALIDATION_ALV_FIELDCAF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM alv_fieldcat CHANGING it_fieldcat TYPE lvc_t_fcat.

  gs_layout-cwidth_opt = 'X'.
  gs_layout-zebra      = 'X'.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BUKRS'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BUKRS'.
  ls_fieldcat-ref_table = 'BKPF'.
  APPEND ls_fieldcat TO it_fieldcat.
*Begin of change by somsx
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VBUND'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'VBUND'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-seltext   = 'Purch. CC'.
  ls_fieldcat-scrtext_s = 'Purch. CC'.
  ls_fieldcat-scrtext_m = 'Purch. CC'.
  ls_fieldcat-scrtext_l = 'Purch. CC'.
  APPEND ls_fieldcat TO it_fieldcat.
*End of change by somsx
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BELNR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-ref_field = 'BELNR'.
  ls_fieldcat-ref_table = 'BKPF'.
  ls_fieldcat-reptext   = 'FI DocNo'.
  ls_fieldcat-seltext   = 'FI DocNo'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GJAHR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'GJAHR'.
  ls_fieldcat-ref_table = 'BKPF'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BUZEI'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BUZEI'.
  ls_fieldcat-ref_table = 'BSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BLART'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BLART'.
  ls_fieldcat-ref_table = 'BKPF'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BELNRI'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-ref_field = 'BELNR'.
  ls_fieldcat-ref_table = 'RBKP'.
  ls_fieldcat-reptext   = 'Inv DocNo'.
  ls_fieldcat-seltext   = 'Inv DocNo'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BUZEII'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BUZEI'.
  ls_fieldcat-ref_table = 'RSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'XBLNR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'XBLNR'.
  ls_fieldcat-ref_table = 'BKPF'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BUDAT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BUDAT'.
  ls_fieldcat-ref_table = 'BKPF'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GRDAT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'SRS_WEDAT'.
  ls_fieldcat-ref_table = 'SRS_PDC_HELP'.
  ls_fieldcat-seltext   = 'Last GR Date'.
  ls_fieldcat-scrtext_s = 'Last GR Date'.
  ls_fieldcat-scrtext_m = 'Last GR Date'.
  ls_fieldcat-scrtext_l = 'Last GR Date'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LIFNR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-lzero = ' '.
  ls_fieldcat-no_convext = ' '.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-ref_field = 'LIFNR'.
  ls_fieldcat-ref_table = 'LFA1'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NAME1'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'NAME1'.
  ls_fieldcat-ref_table = 'LFA1'.
  ls_fieldcat-seltext   = 'Vendor Name'.
  ls_fieldcat-scrtext_s = 'Vendor Name'.
  ls_fieldcat-scrtext_m = 'Vendor Name'.
  ls_fieldcat-scrtext_l = 'Vendor Name'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VEND_REGIO'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'REGIO'.
  ls_fieldcat-ref_table = 'LFA1'.
  ls_fieldcat-reptext   = 'Vendor State'.
  ls_fieldcat-seltext   = 'Vendor State'.
  ls_fieldcat-scrtext_s = 'Vendor State'.
  ls_fieldcat-scrtext_m = 'Vendor State'.
  ls_fieldcat-scrtext_l = 'Vendor State'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'USNAM'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BNAME'.
  ls_fieldcat-ref_table = 'USR01'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'WAERS'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WAERS'.
  ls_fieldcat-ref_table = 'BKPF'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KURSF'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'KURSF'.
  ls_fieldcat-ref_table = 'BKPF'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'FFACT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'FFACT'.
  ls_fieldcat-ref_table = 'TCURF'.
  ls_fieldcat-seltext   = 'CurrConversionFact'.
  ls_fieldcat-scrtext_s = 'CurrConversionFact'.
  ls_fieldcat-scrtext_m = 'CurrConversionFact'.
  ls_fieldcat-scrtext_l = 'CurrConversionFact'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'RMWWR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'RMWWR'.
  ls_fieldcat-ref_table = 'RBKP'.
  ls_fieldcat-reptext   = 'Total Invoice Amount'.
  ls_fieldcat-seltext   = 'Total Invoice Amount'.
  ls_fieldcat-scrtext_s = 'Total Invoice Amount'.
  ls_fieldcat-scrtext_m = 'Total Invoice Amount'.
  ls_fieldcat-scrtext_l = 'Total Invoice Amount'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'WRBTR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WRBTR'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-reptext   = 'Item Net Amount'.
  ls_fieldcat-seltext   = 'Item Net Amount'.
  ls_fieldcat-scrtext_s = 'Item Net Amount'.
  ls_fieldcat-scrtext_m = 'Item Net Amount'.
  ls_fieldcat-scrtext_l = 'Item Net Amount'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BNKAN_FW'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BNKAN_FW'.
  ls_fieldcat-ref_table = 'RBCO'.
  ls_fieldcat-seltext   = 'Delivery costs share of item value'.
  ls_fieldcat-scrtext_s = 'Delivery costs share of item value'.
  ls_fieldcat-scrtext_m = 'Delivery costs share of item value'.
  ls_fieldcat-scrtext_l = 'Delivery costs share of item value'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'IROUT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-ref_field = 'WRBTR'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-seltext   = 'Amount of GR/IR adjustment'.
  ls_fieldcat-scrtext_s = 'Amount of GR/IR adjustment'.
  ls_fieldcat-scrtext_m = 'Amount of GR/IR adjustment'.
  ls_fieldcat-scrtext_l = 'Amount of GR/IR adjustment'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GLBTR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-ref_field = 'WRBTR'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-seltext   = 'Amount reconciling to GL'.
  ls_fieldcat-scrtext_s = 'Amount reconciling to GL'.
  ls_fieldcat-scrtext_m = 'Amount reconciling to GL'.
  ls_fieldcat-scrtext_l = 'Amount reconciling to GL'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ZTERM_TEXT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-scrtext_s = 'Payment Terms'.
  ls_fieldcat-scrtext_m = 'Payment Terms'.
  ls_fieldcat-scrtext_l = 'Payment Terms'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NETDT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'NETDT'.
  ls_fieldcat-ref_table = 'FAEDE'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MENGE'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'MENGE'.
  ls_fieldcat-ref_table = 'RBCO'.
  ls_fieldcat-qfieldname = 'BSTME'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BSTME'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BSTME'.
  ls_fieldcat-ref_table = 'RSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TXJCD'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'TXJCD'.
  ls_fieldcat-ref_table = 'RSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'REGIO'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'REGIO'.
  ls_fieldcat-ref_table = 'LFA1'.
  ls_fieldcat-seltext   = 'Tax State'.
  ls_fieldcat-reptext   = 'Tax State'.
  ls_fieldcat-scrtext_s = 'Tax State'.
  ls_fieldcat-scrtext_m = 'Tax State'.
  ls_fieldcat-scrtext_l = 'Tax State'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SAKNR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-ref_field = 'HKONT'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-scrtext_s = 'G/L Account Number'.
  ls_fieldcat-scrtext_m = 'G/L Account Number'.
  ls_fieldcat-scrtext_l = 'G/L Account Number'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TXT20'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'TXT20'.
  ls_fieldcat-ref_table = 'SKAT'.
  ls_fieldcat-scrtext_s = 'G/L account Descr'.
  ls_fieldcat-scrtext_m = 'G/L account Descr'.
  ls_fieldcat-scrtext_l = 'G/L account Descr'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'XMWST'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'XMWST'.
  ls_fieldcat-ref_table = 'BKPF'.
  ls_fieldcat-scrtext_s = 'Auto Tax Calc'.
  ls_fieldcat-scrtext_m = 'Auto Tax Calc'.
  ls_fieldcat-scrtext_l = 'Auto Tax Calc'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MWSKZ'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'MWSKZ'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-reptext   = 'Tax Code Inv'.
  ls_fieldcat-seltext   = 'Tax Code Inv'.
  ls_fieldcat-scrtext_s = 'Tax Code Inv'.
  ls_fieldcat-scrtext_m = 'Tax Code Inv'.
  ls_fieldcat-scrtext_l = 'Tax Code Inv'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MWSKZ_PO'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'MWSKZ'.
  ls_fieldcat-ref_table = 'EKPO'.
  ls_fieldcat-reptext   = 'Tax Code PO'.
  ls_fieldcat-seltext   = 'Tax Code PO'.
  ls_fieldcat-scrtext_s = 'Tax Code PO'.
  ls_fieldcat-scrtext_m = 'Tax Code PO'.
  ls_fieldcat-scrtext_l = 'Tax Code PO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KNTTP'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'KNTTP'.
  ls_fieldcat-ref_table = 'EKPO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KOSTL'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-ref_field = 'KOSTL'.
  ls_fieldcat-ref_table = 'BSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KTEXT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'KTEXT'.
  ls_fieldcat-ref_table = 'CSKT'.
  ls_fieldcat-reptext   = 'Cost Center Desc'.
  ls_fieldcat-seltext   = 'Cost Center Desc'.
  ls_fieldcat-scrtext_s = 'Cost Center Desc'.
  ls_fieldcat-scrtext_m = 'Cost Center Desc'.
  ls_fieldcat-scrtext_l = 'Cost Center Desc'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AUFNR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'AUFNR'.
  ls_fieldcat-ref_table = 'RBCO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AUART'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'AUART'.
  ls_fieldcat-ref_table = 'AUFK'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NPLNR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'NPLNR'.
  ls_fieldcat-ref_table = 'RBCO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KTEXT_AUFK'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'KTEXT'.
  ls_fieldcat-ref_table = 'AUFK'.
  ls_fieldcat-reptext   = 'Order Desc'.
  ls_fieldcat-seltext   = 'Order Desc'.
  ls_fieldcat-scrtext_s = 'Order Desc'.
  ls_fieldcat-scrtext_m = 'Order Desc'.
  ls_fieldcat-scrtext_l = 'Order Desc'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ANLN1'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-ref_field = 'ANLN1'.
  ls_fieldcat-ref_table = 'RBCO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ANLN2'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-ref_field = 'ANLN2'.
  ls_fieldcat-ref_table = 'RBCO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PS_PSP_PNR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'PS_PSP_PNR'.
  ls_fieldcat-ref_table = 'RBCO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VBELN'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-ref_field = 'VBELN'.
  ls_fieldcat-ref_table = 'RBCO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VBELP'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-ref_field = 'VBELP'.
  ls_fieldcat-ref_table = 'RBCO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TXJCD_SO'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'TAXJURCODE'.
  ls_fieldcat-ref_table = 'ADRC'.
  ls_fieldcat-seltext   = 'JurCd Sales Order'.
  ls_fieldcat-reptext   = 'JurCd Sales Order'.
  ls_fieldcat-scrtext_s = 'JurCd Sales Order'.
  ls_fieldcat-scrtext_m = 'JurCd Sales Order'.
  ls_fieldcat-scrtext_l = 'JurCd Sales Order'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KOSAR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'KOSAR'.
  ls_fieldcat-ref_table = 'CSKS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KOSAR_TXT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-reptext   = 'CC Category Desc'.
  ls_fieldcat-seltext   = 'CC Category Desc'.
  ls_fieldcat-scrtext_s = 'CC Category Desc'.
  ls_fieldcat-scrtext_m = 'CC Category Desc'.
  ls_fieldcat-scrtext_l = 'CC Category Desc'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'KOKRS'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'KOKRS'.
  ls_fieldcat-ref_table = 'RBCO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'EBELN'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-ref_field = 'EBELN'.
  ls_fieldcat-ref_table = 'EKKO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'EBELP'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'EBELP'.
  ls_fieldcat-ref_table = 'EKPO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'WERKS'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WERKS'.
  ls_fieldcat-ref_table = 'EKPO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NAME1_W'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'NAME1'.
  ls_fieldcat-ref_table = 'T001W'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LGORT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'LGORT'.
  ls_fieldcat-ref_table = 'EKPO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ADRST'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-reptext   = 'Ship-To Address'.
  ls_fieldcat-seltext   = 'Ship-To Address'.
  ls_fieldcat-scrtext_s = 'Ship-To Address'.
  ls_fieldcat-scrtext_m = 'Ship-To Address'.
  ls_fieldcat-scrtext_l = 'Ship-To Address'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'WEPOS'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WEPOS'.
  ls_fieldcat-ref_table = 'EKPO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MATNR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'MATNR'.
  ls_fieldcat-ref_table = 'EKPO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MATKL'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'MATKL'.
  ls_fieldcat-ref_table = 'MARA'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'WGBEZ'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WGBEZ'.
  ls_fieldcat-ref_table = 'T023T'.
  ls_fieldcat-reptext   = 'Material group Desc'.
  ls_fieldcat-seltext   = 'Material group Desc'.
  ls_fieldcat-scrtext_s = 'Material group Desc'.
  ls_fieldcat-scrtext_m = 'Material group Desc'.
  ls_fieldcat-scrtext_l = 'Material group Desc'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TXZ01'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'TXZ01'.
  ls_fieldcat-ref_table = 'EKPO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ACT_TAX'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WRBTR'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-reptext   = 'Posted Tax Amount'.
  ls_fieldcat-seltext   = 'Posted Tax Amount'.
  ls_fieldcat-scrtext_s = 'Posted Tax Amount'.
  ls_fieldcat-scrtext_m = 'Posted Tax Amount'.
  ls_fieldcat-scrtext_l = 'Posted Tax Amount'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ACT_TAX_1'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WRBTR'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-reptext   = 'Posted State Tax Amount'.
  ls_fieldcat-seltext   = 'Posted State Tax Amount'.
  ls_fieldcat-scrtext_s = 'Posted State Tax Amount'.
  ls_fieldcat-scrtext_m = 'Posted State Tax Amount'.
  ls_fieldcat-scrtext_l = 'Posted State Tax Amount'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ACT_TAX_2'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WRBTR'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-reptext   = 'Posted County Tax Amount'.
  ls_fieldcat-seltext   = 'Posted County Tax Amount'.
  ls_fieldcat-scrtext_s = 'Posted County Tax Amount'.
  ls_fieldcat-scrtext_m = 'Posted County Tax Amount'.
  ls_fieldcat-scrtext_l = 'Posted County Tax Amount'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ACT_TAX_3'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WRBTR'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-reptext   = 'Posted City Tax Amount'.
  ls_fieldcat-seltext   = 'Posted City Tax Amount'.
  ls_fieldcat-scrtext_s = 'Posted City Tax Amount'.
  ls_fieldcat-scrtext_m = 'Posted City Tax Amount'.
  ls_fieldcat-scrtext_l = 'Posted City Tax Amount'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ACT_TAX_4'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'WRBTR'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-reptext   = 'Posted Secondary Tax Amount'.
  ls_fieldcat-seltext   = 'Posted Secondary Tax Amount'.
  ls_fieldcat-scrtext_s = 'Posted Secondary Tax Amount'.
  ls_fieldcat-scrtext_m = 'Posted Secondary Tax Amount'.
  ls_fieldcat-scrtext_l = 'Posted Secondary Tax Amount'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ACT_TAX_RATE'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'LHPC1'.
  ls_fieldcat-ref_table = 'T5N2Q'.
  ls_fieldcat-reptext   = 'Actual Tax Rate'.
  ls_fieldcat-seltext   = 'Actual Tax Rate'.
  ls_fieldcat-scrtext_s = 'Actual Tax Rate'.
  ls_fieldcat-scrtext_m = 'Actual Tax Rate'.
  ls_fieldcat-scrtext_l = 'Actual Tax Rate'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ACT_TAX_RATE_PO'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-scrtext_s = 'Actual Tax Rate PO'.
  ls_fieldcat-scrtext_m = 'Actual Tax Rate PO'.
  ls_fieldcat-scrtext_l = 'Actual Tax Rate PO'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CAL_TAX'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-scrtext_s = 'System Calculated Tax'.
  ls_fieldcat-scrtext_m = 'System Calculated Tax'.
  ls_fieldcat-scrtext_l = 'System Calculated Tax'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TAX_DIF'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-scrtext_s = 'Tax Difference'.
  ls_fieldcat-scrtext_m = 'Tax Difference'.
  ls_fieldcat-scrtext_l = 'Tax Difference'.
  ls_fieldcat-cfieldname = 'WAERS'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ARC_DOC_ID'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-ref_field = 'ARC_DOC_ID'.
  ls_fieldcat-ref_table = 'TOA01'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ARCHIV_ID'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-scrtext_s = 'Archive ID'.
  ls_fieldcat-scrtext_m = 'Archive ID'.
  ls_fieldcat-scrtext_l = 'Archive ID'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SAP_OBJECT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-scrtext_s = 'SAP Object'.
  ls_fieldcat-scrtext_m = 'SAP Object'.
  ls_fieldcat-scrtext_l = 'SAP Object'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'OBJECT_ID'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-scrtext_s = 'Object ID'.
  ls_fieldcat-scrtext_m = 'Object ID'.
  ls_fieldcat-scrtext_l = 'Object ID'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AR_OBJECT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-scrtext_s = 'Archive Object'.
  ls_fieldcat-scrtext_m = 'Archive Object'.
  ls_fieldcat-scrtext_l = 'Archive Object'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BKTXT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BKTXT'.
  ls_fieldcat-ref_table = 'BKPF'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SGTXT_VEN'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'SGTXT'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-reptext   = 'Vendor Item Text'.
  ls_fieldcat-seltext   = 'Vendor Item Text'.
  ls_fieldcat-scrtext_s = 'Vendor Item Text'.
  ls_fieldcat-scrtext_m = 'Vendor Item Text'.
  ls_fieldcat-scrtext_l = 'Vendor Item Text'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SGTXT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'SGTXT'.
  ls_fieldcat-ref_table = 'BSEG'.
  ls_fieldcat-reptext   = 'Expense Item Text'.
  ls_fieldcat-seltext   = 'Expense Item Text'.
  ls_fieldcat-scrtext_s = 'Expense Item Text'.
  ls_fieldcat-scrtext_m = 'Expense Item Text'.
  ls_fieldcat-scrtext_l = 'Expense Item Text'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AUGDT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'AUGDT'.
  ls_fieldcat-ref_table = 'BSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AUGBL'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'AUGBL'.
  ls_fieldcat-ref_table = 'BSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CHECT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'CHECT'.
  ls_fieldcat-ref_table = 'PAYR'.
  ls_fieldcat-reptext   = 'Check Number'.
  ls_fieldcat-seltext   = 'Check Number'.
  ls_fieldcat-scrtext_s = 'Check Number'.
  ls_fieldcat-scrtext_m = 'Check Number'.
  ls_fieldcat-scrtext_l = 'Check Number'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BLDAT'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BLDAT'.
  ls_fieldcat-ref_table = 'BKPF'.
  ls_fieldcat-reptext   = 'Document Date'.
  ls_fieldcat-seltext   = 'Document Date'.
  ls_fieldcat-scrtext_s = 'Document Date'.
  ls_fieldcat-scrtext_m = 'Document Date'.
  ls_fieldcat-scrtext_l = 'Document Date'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PRCTR'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'PRCTR'.
  ls_fieldcat-ref_table = 'BSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GRDOC'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BELNR'.
  ls_fieldcat-ref_table = 'EKBE'.
  ls_fieldcat-hotspot = 'X'.
  ls_fieldcat-reptext   = 'Last GR Doc'.
  ls_fieldcat-seltext   = 'Last GR Doc'.
  ls_fieldcat-scrtext_s = 'Last GR Doc'.
  ls_fieldcat-scrtext_m = 'Last GR Doc'.
  ls_fieldcat-scrtext_l = 'Last GR Doc'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GRMNG'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-ref_field = 'MENGE'.
  ls_fieldcat-ref_table = 'EKBE'.
  ls_fieldcat-qfieldname = 'BSTME'.
  ls_fieldcat-reptext   = 'Last GR Qty'.
  ls_fieldcat-seltext   = 'Last GR Qty'.
  ls_fieldcat-scrtext_s = 'Last GR Qty'.
  ls_fieldcat-scrtext_m = 'Last GR Qty'.
  ls_fieldcat-scrtext_l = 'Last GR Qty'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GRJAH'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-no_out = 'X'.
  ls_fieldcat-ref_field = 'GJAHR'.
  ls_fieldcat-ref_table = 'EKBE'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BVORG'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BVORG'.
  ls_fieldcat-ref_table = 'BKPF'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ZZLOC'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'ZZLOC'.
  ls_fieldcat-ref_table = 'BSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BBUKRS'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'BUKRS'.
  ls_fieldcat-ref_table = 'BSEG'.
  APPEND ls_fieldcat TO it_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'DESC40_N'.
  ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
  ls_fieldcat-ref_field = 'DESC40_N'.
  ls_fieldcat-ref_table = 'ZAPT_LOCATION'.
  APPEND ls_fieldcat TO it_fieldcat.

  IF NOT recon IS INITIAL.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'REC_TYPE'.
    ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
    ls_fieldcat-scrtext_s = 'RecType'.
    ls_fieldcat-scrtext_m = 'Record Type'.
    ls_fieldcat-scrtext_l = 'Record Type'.
    APPEND ls_fieldcat TO it_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GLAMT'.
    ls_fieldcat-tabname   ='ZTAX_VALIDATE1'.
    ls_fieldcat-seltext   = 'Recon Amount'.
    ls_fieldcat-scrtext_s = 'Recon Amount'.
    ls_fieldcat-scrtext_m = 'Recon Amount'.
    ls_fieldcat-scrtext_l = 'Recon Amount'.
    ls_fieldcat-cfieldname = 'WAERS'.
    APPEND ls_fieldcat TO it_fieldcat.
  ENDIF.

ENDFORM.                    " ALV_FIELDCAT
