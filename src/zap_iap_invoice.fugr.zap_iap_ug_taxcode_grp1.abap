FUNCTION ZAP_IAP_UG_TAXCODE_GRP1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(HEADER_DATA) LIKE  ZAPS_IAP_DOC_HDR STRUCTURE
*"        ZAPS_IAP_DOC_HDR
*"  TABLES
*"      GL_LINES STRUCTURE  BAPIACGL09
*"      CURRENCYAMOUNT STRUCTURE  BAPIACCR09
*"      ACCOUNTTAX STRUCTURE  BAPIACTX09
*"----------------------------------------------------------------------
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Function Module:  ZAP_IAP_UG_TAXCODE_GRP1                           *
*  Author:           Vijay Rajaputra                                   *
*  Date:             AUG, 21 2018                                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      Post IAP Documents for Non-PO Invoices Tax Code   *
*                    Group 1 ( EM, ET, H1, HA, HB, HP, HT, HU, T2 )    *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 08/21/18 VRAJAPUTRA D30K928897 - CHG0109670 - Initial Development    *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************

  DATA : ls_header     TYPE ZAPS_IAP_DOC_HDR,
         lt_MWDAT      TYPE TABLE OF RTAX1U15,
         ls_MWDAT      TYPE RTAX1U15,
         lt_accounttax TYPE TABLE OF BAPIACTX09,
*         lt_atax_temp  TYPE TABLE OF BAPIACTX09,
         ls_accounttax TYPE BAPIACTX09,
         lt_gl         TYPE TABLE OF BAPIACGL09,
*         lt_tax        TYPE TABLE OF BAPIACTX09,
*         ls_tax        TYPE BAPIACTX09,
*         LT_EXTN       TYPE TABLE OF BAPIACEXTC,
*         LS_EXTN       TYPE BAPIACEXTC,
         lt_currency   TYPE TABLE OF BAPIACCR09,
*         lt_location   TYPE TABLE OF zfit_location,
*         ls_location   TYPE zfit_location,
         ls_currency   TYPE BAPIACCR09.
*         ls_curr       TYPE BAPIACCR09,
*         ls_curr1      TYPE BAPIACCR09,
*         lv_FWNAV      TYPE FWSTE,
*         lv_FWNVV      TYPE FWSTE,
*         lv_FWSTE      TYPE FWSTE,
*         lv_FWAST      TYPE FWSTE.

  field-symbols:  <lfs_curr> TYPE BAPIACCR09,
                 " <lfs_curr_GL> TYPE BAPIACCR09,
                  <lfs_gl>   TYPE BAPIACGL09.

  DATA : lv_bukrs      TYPE bukrs,
         lv_mwskz      TYPE mwskz,
         lv_WMWST      TYPE WMWST,
         lv_msatz      TYPE msatz,
         lv_TXJCD      TYPE TXJCD,
         lv_WAERS      TYPE  WAERS,
         lv_WRBTR      TYPE WRBTR,
         lv_amount     TYPE wrbtr,
         lv_base       TYPE wrbtr,
*         lv_index      TYPE sy-index,
*         lv_gl_amount  TYPE wrbtr,
*         lv_tax_amount TYPE wrbtr,
         lv_itemno     TYPE POSNR_ACC,
         ls_ACCDATA    TYPE BSEG.



**--> Tax Type Line Identify
*  TYPES : BEGIN OF lty_gl_tax,
*          ITEMNO  TYPE POSNR_ACC,
*          amount  TYPE wrbtr,
*          tax     TYPE wrbtr,
*          flag    TYPE char1, " For check Taxable or non Taxable line.
*          END OF lty_gl_tax.
*
*          BEGIN OF lty_loc,
*          location TYPE ZZLOC,
*          END OF lty_loc.



*  data : "lt_gl_tax        TYPE TABLE OF lty_gl_tax,
*         ls_gl_tax        TYPE lty_gl_tax.
*         lt_loc           TYPE TABLE OF lty_loc,
*         ls_loc           TYPE lty_loc,
*         ls_location_data TYPE com_jur,
*         lt_txjcd         TYPE TABLE OF com_jur,
*         ls_txjcd         TYPE com_jur.


  CLEAR : ls_header, lt_gl,lt_currency,lv_TXJCD,lv_itemno.

*-> Data transfer to Local fields.
  ls_header = HEADER_DATA.
  lt_gl[] = GL_LINES[].
  lt_currency[] = CURRENCYAMOUNT[].
*  lt_extn[] = EXTN[].

  SORT lt_currency by ITEMNO_ACC DESCENDING.
  CLEAR ls_currency.
  READ TABLE lt_currency INTO ls_currency INDEX 1. " to get the hight ITEM NO.
  lv_itemno = ls_currency-itemno_acc.
  CLEAR ls_currency.
  SORT lt_currency by ITEMNO_ACC ASCENDING.

  CLEAR lv_amount.
  UNASSIGN <LFS_GL>.


  LOOP AT lt_gl ASSIGNING <lfs_gl>.

*    CLEAR lv_index.
*    lv_index = sy-tabix.

    lv_bukrs = ls_header-COMPCODE.
    lv_mwskz = ls_header-TAXCODE.
*  lv_TXJCD = ''.
    lv_WAERS = ls_header-CURRCODE.

*--> Filling Accounting Data Structure
    CLEAR: ls_ACCDATA,ls_currency, lv_WRBTR .

    READ TABLE lt_currency ASSIGNING <lfs_curr> with key ITEMNO_ACC = <lfs_gl>-ITEMNO_ACC.
    ls_currency = <lfs_curr>.
    IF sy-subrc is INITIAL.
      move ls_currency-AMT_DOCCUR TO ls_ACCDATA-DMBTR.
      lv_amount = lv_amount + ls_currency-AMT_DOCCUR.  " For Cumulative amounts of Taxable lines which need to ba calculate for GLs
      lv_WRBTR = ls_ACCDATA-DMBTR.
    ENDIF.




    ls_ACCDATA-BUKRS = <lfs_gl>-comp_code.
    ls_ACCDATA-MWSKZ = <lfs_gl>-Tax_code.
    ls_ACCDATA-KOSTL = <lfs_gl>-costcenter.
    ls_ACCDATA-HKONT = <lfs_gl>-gl_account.


    CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
      EXPORTING
        I_BUKRS           = lv_bukrs
        I_MWSKZ           = lv_mwskz
        I_TXJCD           = lv_TXJCD
        I_WAERS           = lv_WAERS
        I_WRBTR           = lv_WRBTR
*       I_ZBD1P           = 0
*       I_PRSDT           =
*       I_PROTOKOLL       =
*       I_TAXPS           =
*       I_ACCNT_EXT       =
        I_ACCDATA         = ls_ACCDATA
*      IMPORTING
*        E_FWNAV           = lv_FWNAV
*        E_FWNVV           = lv_FWNVV
*        E_FWSTE           = lv_FWSTE
*        E_FWAST           = lv_FWAST
      TABLES
        T_MWDAT           = lt_MWDAT
      EXCEPTIONS
        BUKRS_NOT_FOUND   = 1
        COUNTRY_NOT_FOUND = 2
        MWSKZ_NOT_DEFINED = 3
        MWSKZ_NOT_VALID   = 4
        KTOSL_NOT_FOUND   = 5
        KALSM_NOT_FOUND   = 6
        PARAMETER_ERROR   = 7
        KNUMH_NOT_FOUND   = 8
        KSCHL_NOT_FOUND   = 9
        UNKNOWN_ERROR     = 10
        ACCOUNT_NOT_FOUND = 11
        TXJCD_NOT_VALID   = 12
        OTHERS            = 13.
    IF SY-SUBRC <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*    Break vrajaputra.
    CLEAR lv_WMWST.
    LOOP AT lt_MWDAT INTO ls_MWDAT.
      lv_itemno  =  lv_itemno + 1.
      lv_msatz = lv_msatz + ls_mwdat-msatz.
      IF ls_MWDAT-WMWST GT '0.00'.
        lv_WMWST =  lv_WMWST + ls_MWDAT-WMWST.
      ENDIF.
* Recaclulating the Base Amount ( Morgan: 22-08)
* Gl Amount need to divide by 1+ Tax %)
      lv_base = lv_amount / ( 1 + ( ls_mwdat-msatz / 100 ) ). " Base Amount

*--> Adjust the Amount BAse Amount of GL Line in Currency Table.
      <lfs_curr>-Amt_doccur = lv_base.
      <lfs_curr>-Amt_base = lv_base.

*--->Accounttax TAbles fill
      ls_accounttax-ITEMNO_ACC = lv_itemno.
      ls_accounttax-GL_Account = ls_mwdat-HKONT.

      ls_accounttax-TAX_RATE = ls_mwdat-msatz.
      ls_accounttax-acct_key = ls_mwdat-ktosl.
      ls_accounttax-COND_KEY = ls_mwdat-kschl.
      ls_accounttax-TAX_CODE = <lfs_gl>-tax_code.
      ls_accounttax-TAX_RATE = ls_mwdat-MSATZ.
      ls_accounttax-TAXJURCODE = ls_mwdat-TXJCD.
      ls_accounttax-TAXJURCODE_DEEP = ls_mwdat-TXJCD_DEEP.
*      ls_accounttax-ITEMNO_TAX = <lfs_gl>-ITEMNO_ACC.
      APPEND ls_accounttax to lt_accounttax .

      CLEAR :  ls_currency-ITEMNO_ACC,  ls_currency-AMT_DOCCUR.
      ls_currency-ITEMNO_ACC = lv_itemno. " Tax Line
      ls_currency-AMT_DOCCUR = ( lv_amount - lv_base ).
      ls_currency-AMT_BASE = lv_base .

      append ls_currency to lt_currency.
      CLEAR ls_accounttax.
    ENDLOOP.


*    IF lv_WMWST is NOT INITIAL.  " Tax Amounts for each GL Calculation  "
*      ls_gl_tax-ITEMNO = <lfs_gl>-ITEMNO_ACC.
**      ls_gl_tax-Amount = lv_WMWST.
*      ls_gl_tax-tax = lv_WMWST.
*      ls_gl_tax-flag = 'X'.
*      append ls_gl_tax to lt_gl_tax.
*      CLEAR ls_gl_tax.
*    ENDIF.
*-->GL Lines adjust with lv_TXJCD and Tax Lines.
*    <lfs_gl>-taxJURCODE = lv_TXJCD.
*    <lfs_gl>-ITEMNO_TAX = lv_itemno.  " Not Required

  ENDLOOP.

*  UNASSIGN <lfs_gl>.
**  break vrajaputra.
**--> Based on taxable lines GL/Currency Amounts need to be adjust.
*  CLEAR ls_gl_tax.
*  LOOP AT lt_gl_tax INTO ls_gl_tax.
*    READ TABLE lt_currency ASSIGNING <lfs_curr> with key ITEMNO_ACC = ls_gl_tax-itemno.
*    IF sy-subrc is INITIAL.
**->Adjusting the GL Line Amount
*      <lfs_curr>-AMT_DOCCUR =  <lfs_curr>-AMT_DOCCUR + ls_gl_tax-tax.
*    ENDIF.
*  ENDLOOP.

  UNASSIGN <lfs_curr>.
  sort   lt_currency by itemno_acc ASCENDING.
  read TABLE lt_currency ASSIGNING <lfs_curr> INDEX 1.
  IF sy-subrc is INITIAL.
    <lfs_curr>-amt_base = lv_base.
    <lfs_curr>-tax_amt = <lfs_curr>-amt_doccur + lv_base.
  ENDIF.




*  UNASSIGN <lfs_curr>.
*  CLEAR  lv_gl_amount.
**--> For roundig issue.
*  UNASSIGN <lfs_curr>.
*  LOOP AT lt_currency ASSIGNING <lfs_curr>.
*    lv_gl_amount = lv_gl_amount + <lfs_curr>-AMT_DOCCUR.
*  ENDLOOP.
**  BREAK Vrajaputra.
*  if lv_gl_amount is NOT INITIAL.
*    CLEAR lv_index.
*    lv_index = 2. " First GL
*    READ TABLE lt_currency ASSIGNING <lfs_curr> index lv_index.
*    IF sy-subrc is INITIAL.
*      <lfs_curr>-AMT_DOCCUR = <lfs_curr>-AMT_DOCCUR - lv_gl_amount.
*    ENDIF.
*  endif.
*  BREAK VRAJAPUTRA.
*
*  CLEAR lv_gl_amount.
*  CLEAR: ls_accounttax,ls_currency.
*  LOOP AT lt_currency ASSIGNING <lfs_curr> WHERE AMT_DOCCUR IS INITIAL .
*    READ TABLE lt_accounttax INTO ls_accounttax WITH KEY itemno_acc = <lfs_curr>-itemno_acc.
*    IF sy-subrc is INITIAL.
*      IF LS_ACCOUNTTAX-TAX_RATE is NOT INITIAL.
*        IF LS_ACCOUNTTAX-GL_ACCOUNT is INITIAL.  " I Series -- Sum up the tax and Add to GL Line.
**          READ TABLE lt_currency ASSIGNING <lfs_curr> with key itemno_acc = ls_accounttax-itemno_tax.  " To update the amount in corresponding GL
**          IF sy-subrc is INITIAL.
**            <lfs_curr_GL>-AMT_DOCCUR =  <lfs_curr_GL>-AMT_DOCCUR + ( <lfs_curr>-AMT_BASE * ( LS_ACCOUNTTAX-TAX_RATE / 100 ) ).
**          ENDIF.
*        ELSE.
*          LS_ACCOUNTTAX-TAX_RATE = ( LS_ACCOUNTTAX-TAX_RATE / 100 ).
*          lv_gl_amount = <lfs_curr>-AMT_BASE * LS_ACCOUNTTAX-TAX_RATE.
*          <lfs_curr>-AMT_DOCCUR =  ( lv_gl_amount  ) * '-1'.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

* Returning data to FM

  CURRENCYAMOUNT[] = lt_currency[].
  GL_LINES[]       = lt_gl[].
  accounttax[] = lt_accounttax[].






ENDFUNCTION.
