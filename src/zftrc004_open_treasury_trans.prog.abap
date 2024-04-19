REPORT  zftrc004_open_treasury_trans.
*&---------------------------------------------------------------------*
*& Report  ZFTRC004_OPEN_TREASURY_TRANS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
TYPE-POOLS: truxs.


DATA : filename TYPE string,
      it_raw TYPE truxs_t_text_data,
      error TYPE i.

DATA: gwa_gen TYPE bapi_ftr_create,
      gwa_cp TYPE bapi_ftr_create_cp,
      gwa_swap TYPE bapi_ftr_create_swap,
      gwa_iri TYPE bapi_ftr_create_irate.

CONSTANTS gc_x TYPE c VALUE 'X'.

DATA: BEGIN OF git_file_data_cp OCCURS 0,
  bukrs TYPE  bukrs , " COMPANY_CODE
  vvsart  TYPE  vvsart  , " PRODUCT_TYPE
  tb_sfhaart  TYPE  tb_sfhaart  , " TRANSACTION_TYPE
  tb_rfha TYPE  tb_rfha , " EXTERNAL_TRANSACTION_NUMBER
  tb_kunnr_new  TYPE  tb_kunnr_new  , " PARTNER
  tb_dvtrab TYPE  tb_dvtrab , " CONTRACT_DATE
  rportb  TYPE  rportb  , " PORTFOLIO
  tb_zuond  TYPE  tb_zuond  , " ASSIGNMENT
  tb_nordext  TYPE  tb_nordext  , " EXTERNAL_REFERENCE
  tb_refer  TYPE  tb_refer  , " INTERNAL_REFERENCE
  tpm_com_val_class TYPE  tpm_com_val_class , " VALUATION_CLASS
  tb_wgschft TYPE tb_wgschft, "CURRENCY
  tb_dblfz TYPE tb_dblfz,  "START_TERM
  tb_sinclbe TYPE tb_sinclbe , "START_INCLUSIVE
  tb_delfz TYPE tb_delfz, "END_TERM
  tb_sfhazba TYPE tb_sfhazba, "FLOW_TYPE
  bapitm_bsaldo TYPE bapitm_bsaldo, "AMOUNT
  pkond TYPE pkond, "INTEREST_RATE
  szbmeth TYPE  szbmeth, "INTEREST_CALC_METH
  tfmskalidwt   TYPE tfmskalidwt, "INTEREST_CALENDAR
  tb_snpvcal TYPE tb_snpvcal, "NET_PRESENT_VAL_CAL

      END OF git_file_data_cp.

DATA: BEGIN OF git_file_data_swap OCCURS 0,
bukrs TYPE  bukrs , " COMPANY_CODE
vvsart  TYPE  vvsart  , " PRODUCT_TYPE
tb_sfhaart  TYPE  tb_sfhaart  , " TRANSACTION_TYPE
tb_rfha TYPE  tb_rfha , " EXTERNAL_TRANSACTION_NUMBER
tb_kunnr_new  TYPE  tb_kunnr_new  , " PARTNER
tb_dvtrab TYPE  tb_dvtrab , " CONTRACT_DATE
rportb  TYPE  rportb  , " PORTFOLIO
tb_zuond  TYPE  tb_zuond  , " ASSIGNMENT
tb_nordext  TYPE  tb_nordext  , " EXTERNAL_REFERENCE
tb_refer  TYPE  tb_refer  , " INTERNAL_REFERENCE
tpm_com_val_class TYPE  tpm_com_val_class , " VALUATION_CLASS
tb_dblfz  TYPE  tb_dblfz  , " START_TERM
tb_sinclbe  TYPE  tb_sinclbe  , " START_INCLUSIVE
tb_delfz  TYPE  tb_delfz  , " END_TERM
tb_sincle TYPE  tb_sincle , " END_INCLUSIVE
skalid1 TYPE  skalid  ,                                     " CALENDAR1
skalid2 TYPE  skalid  ,                                     " CALENDAR2
bapitm_bsaldo_p TYPE  bapitm_bsaldo , " PAY_SIDE_NOMINAL_AMOUNT
tb_wkapbet_p  TYPE  tb_wkapbet  , " PAY_SIDE_CURRENCY
dguel_kp_p  TYPE  dguel_kp  , " PAY_INTEREST_EFFECTIVE_FROM
tb_dfaell1_p  TYPE  tb_dfaell1  , " PAY_INTEREST_FIRST_DUE_DATE
ammrhy_p  TYPE  ammrhy  , " PAY_INTEREST_FREQUENCY_MONTH
tb_pzinssv_p  TYPE  tb_pzinssv  , " PAY_INTEREST_RATE
szsref_p  TYPE  szsref  , " PAY_INTEREST_RATE_REF
tb_zzs_p  TYPE  tb_zzs  , " PAY_INTEREST_SPREAD
szbmeth_p TYPE  szbmeth , " PAY_INTEREST_CALC_METHOD
tfmskalidwt_p TYPE  tfmskalidwt , " PAY_INTEREST_CALENDAR_ID
bapitm_bsaldo_r TYPE  bapitm_bsaldo , " RECEIVE_SIDE_NOMINAL_AMOUNT
tb_wkapbet_r  TYPE  tb_wkapbet  , " RECEIVE_SIDE_CURRENCY
dguel_kp_r  TYPE  dguel_kp  , " RECEIVE_INTEREST_EFFECTIVE_FRO
tb_dfaell1_r  TYPE  tb_dfaell1  , " RECEIVE_INTEREST_FIRST_DUE_DAT
ammrhy_r  TYPE  ammrhy  , " RECEIVE_INTEREST_FREQUENCY_MON
tb_pzinssv_r  TYPE  tb_pzinssv  , " RECEIVE_INTEREST_RATE
szsref_r  TYPE  szsref  , " RECEIVE_INTEREST_RATE_REF
tb_zzs_r  TYPE  tb_zzs  , " RECEIVE_INTEREST_SPREAD
szbmeth_r TYPE  szbmeth , " RECEIVE_INTEREST_CALC_METHOD
tfmskalidwt_r TYPE  tfmskalidwt , " RECEIVE_INTEREST_CALENDAR_ID


END OF git_file_data_swap.

DATA: BEGIN OF git_file_data_iri OCCURS 0,

bukrs TYPE  bukrs , " COMPANY_CODE
vvsart  TYPE  vvsart  , " PRODUCT_TYPE
tb_sfhaart  TYPE  tb_sfhaart  , " TRANSACTION_TYPE
tb_rfha TYPE  tb_rfha , " EXTERNAL_TRANSACTION_NUMBER
tb_kunnr_new  TYPE  tb_kunnr_new  , " PARTNER
tb_dvtrab TYPE  tb_dvtrab , " CONTRACT_DATE
rportb  TYPE  rportb  , " PORTFOLIO
tb_zuond  TYPE  tb_zuond  , " ASSIGNMENT
tb_nordext  TYPE  tb_nordext  , " EXTERNAL_REFERENCE
tb_refer  TYPE  tb_refer  , " INTERNAL_REFERENCE
tpm_com_val_class TYPE  tpm_com_val_class , " VALUATION_CLASS
 tb_wgschft      TYPE  tb_wgschft,"Currency
tb_dblfz      TYPE  tb_dblfz,"Term Start
tb_delfz      TYPE  tb_delfz,"Term End
bapitm_bsaldo      TYPE  bapitm_bsaldo,"Amount or balance
tb_irate_structure      TYPE  tb_irate_structure,"Treasury: Interest Form
tb_sfhazba      TYPE  tb_sfhazba,"Flow Type
skoart      TYPE  skoart,"Condition Type (Smallest Subdivision of Condition Records)
szsref      TYPE  szsref,"Reference Interest Rate
tm_pzins      TYPE  tm_pzins,"Interest rate
bapitm_interest_rate_amount      TYPE  bapitm_interest_rate_amount,"Interest Amount
szbmeth      TYPE  szbmeth,"Interest Calculation Method
tb_srhythm      TYPE  tb_srhythm,"Frequency Indicator
tb_arhytm      TYPE  tb_arhytm,"Defined Frequency in Days or Months
tb_sinclbe      TYPE  tb_sinclbe,"Calculation Period: Start Inclusive vs. End Inclusive
tb_szinskap      TYPE  tb_szinskap,"Capitalize Interest
tb_sfranze      TYPE  tb_sfranze,"Shift Due Date Back to End of Term
tb_svwerk      TYPE  tb_svwerk,"Working Day Shift for Calculation Day
tb_sfwerk      TYPE  tb_sfwerk,"Working Day Shift for Due Date
tfm_sround      TYPE  tfm_sround,"Rounding Category
tfmskalidwt      TYPE  tfmskalidwt,"Interest Calendar
tb_repaym_structure      TYPE  tb_repaym_structure,"Treasury: Repayment Form
bapitm_repayment_amount      TYPE  bapitm_repayment_amount,"Repayment Amount
dguel_kp      TYPE  dguel_kp,"Condition Item Effecitve from
tbfaeltz      TYPE  tbfaeltz,"First Due Date for Interest
dvalut      TYPE  dvalut,"Calculation Date
tb_arhytm_unit      TYPE  tb_arhytm_unit,"Treasury: Unit of Frequency
tb_zzs      TYPE  tb_zzs,"Interest Markup/Markdown
tfm_pkond1stper      TYPE  tfm_pkond1stper,"Interest Rate for the First Period
tb_jzinsre      TYPE  tb_jzinsre,"Exponential Interest Calculation
tbvfaetz      TYPE  tbvfaetz,"Shift due date towards end of interest period
tb_updaterule      TYPE  tb_updaterule,"Treasury: Update for Calculation Date/Due Date
tb_irate_formula      TYPE  tb_irate_formula,"FTR: Interest Formula

      END OF git_file_data_iri.

DATA: git_error_data_bapi TYPE bapiret2 OCCURS 0 WITH HEADER LINE.



SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:xls_file TYPE rlgrap-filename.


SELECTION-SCREEN:END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_appn RADIOBUTTON GROUP g1,
            p_presn  RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN:END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS:rb_comm  RADIOBUTTON GROUP g2,
           rb_irs  RADIOBUTTON GROUP g2,
           rb_iri  RADIOBUTTON GROUP g2.
SELECTION-SCREEN:END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
PARAMETERS:rb_test  RADIOBUTTON GROUP g3,
           rb_prod  RADIOBUTTON GROUP g3.
SELECTION-SCREEN:END OF BLOCK b4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR xls_file.

  PERFORM f4_func_help. "F4 FUNCTIONALITY


START-OF-SELECTION.

  IF rb_comm = 'X'.
    PERFORM f_commercial_paper.
  ENDIF.
  IF rb_irs = 'X'.
    PERFORM f_interest_rate_swap.
  ENDIF.
  IF rb_iri = 'X'.
    PERFORM f_interest_rate_instrument.
  ENDIF.

END-OF-SELECTION.

*
*&---------------------------------------------------------------------*
*&      Form  F4_FUNC_HELP
*&---------------------------------------------------------------------*
*       F4 FUNCTIONALITY

FORM f4_func_help .
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
*     FIELD_NAME    = ' '
    IMPORTING
      file_name     = xls_file.
ENDFORM.                    " F4_FUNC_HELP



*&---------------------------------------------------------------------*
*&      Form  F_COMMERCIAL_PAPER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_commercial_paper .


  PERFORM fupload_xls_file_comm. "UPLOAD EXCEL FILE

  IF sy-subrc = 0.

    LOOP AT git_file_data_cp.
      CLEAR: gwa_gen, gwa_cp.

      gwa_gen-company_code   = git_file_data_cp-bukrs  . " Company Code
      gwa_gen-product_type   =       git_file_data_cp-vvsart . " Product Type
      gwa_gen-transaction_type   =       git_file_data_cp-tb_sfhaart . " Financial Transaction Type
      gwa_gen-external_transaction_number  =       git_file_data_cp-tb_rfha  . " EXTERNAL_TRANSACTION_NUMBER
      gwa_gen-partner   =       git_file_data_cp-tb_kunnr_new  .  "  Business Partner Number
      gwa_gen-contract_date  =       git_file_data_cp-tb_dvtrab  . " Contract Conclusion Date
      gwa_gen-portfolio   =       git_file_data_cp-rportb  .  "  Portfolio
      gwa_gen-assignment   =       git_file_data_cp-tb_zuond . " Assignment
      gwa_gen-external_reference   =       git_file_data_cp-tb_nordext . " External Reference
      gwa_gen-internal_reference   =       git_file_data_cp-tb_refer . " Internal Reference
      gwa_gen-valuation_class  =       git_file_data_cp-tpm_com_val_class  . " General Valuation Class
      gwa_cp-currency   = git_file_data_cp-tb_wgschft .            "Currency of transaction
      gwa_cp-start_term =  git_file_data_cp-tb_dblfz.             "Start date
      gwa_cp-start_inclusive = git_file_data_cp-tb_sinclbe. " start_inclusive
      gwa_cp-end_term   = git_file_data_cp-tb_delfz  .             "End date
      gwa_cp-flow_type = git_file_data_cp-tb_sfhazba. " Flow_type
      gwa_cp-amount      = git_file_data_cp-bapitm_bsaldo   . "Nominal Amount
      gwa_cp-interest_rate = git_file_data_cp-pkond. "interest rate
      gwa_cp-interest_calc_meth = git_file_data_cp-szbmeth. "INTEREST_CALC_METH
      gwa_cp-net_present_val_cal = git_file_data_cp-tb_snpvcal."npv calculation
      gwa_cp-interest_calendar = git_file_data_cp-tfmskalidwt. "Calendar


      IF  rb_prod = 'X' .

        CALL FUNCTION 'BAPI_FTR_CP_CREATE'
          EXPORTING
            commercialpaper            =  gwa_cp
            generalcontractdata        =  gwa_gen
*           TESTRUN                    = ' '
          IMPORTING
            financialtransaction       = gwa_gen-external_transaction_number
            companycode                = gwa_gen-company_code
          TABLES
            return                     =  git_error_data_bapi.

        IF NOT git_error_data_bapi[] IS INITIAL.
          LOOP AT git_error_data_bapi.
            IF git_error_data_bapi-type = 'E'.
              error = 1 + error.
              WRITE:   / git_error_data_bapi-type, git_error_data_bapi-message,git_error_data_bapi-field.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              IF sy-subrc = 0.
                WRITE: / 'Contract deal' , gwa_gen-external_transaction_number, 'is successfully created'.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF git_error_data_bapi[] IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          IF sy-subrc = 0.
            WRITE: / 'Contract deal' , gwa_gen-external_transaction_number, 'is successfully created'.
          ENDIF.
        ENDIF.

      ELSEIF rb_test = 'X'.

        CALL FUNCTION 'BAPI_FTR_CP_CREATE'
          EXPORTING
            commercialpaper      = gwa_cp
            generalcontractdata  = gwa_gen
            testrun              = 'X'
          IMPORTING
            financialtransaction = gwa_gen-external_transaction_number
            companycode          = gwa_gen-company_code
          TABLES
            return               = git_error_data_bapi.

        LOOP AT git_error_data_bapi.
          WRITE:   / git_error_data_bapi-type, git_error_data_bapi-message,git_error_data_bapi-field.
          IF git_error_data_bapi-type = 'E'.
            error = 1 + error.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.
  ENDIF.
  IF error = 0.
    WRITE: /  'Input file is error free'.
  ENDIF.

ENDFORM.                    " F_COMMERCIAL_PAPER
*&---------------------------------------------------------------------*
*&      Form  F_INTEREST_RATE_SWAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_interest_rate_swap .

  PERFORM fupload_xls_file_swap.

  IF sy-subrc = 0.

    LOOP AT git_file_data_swap.
      CLEAR: gwa_gen, gwa_swap.

      gwa_gen-company_code   = git_file_data_swap-bukrs  . " Company Code
      gwa_gen-product_type   =       git_file_data_swap-vvsart . " Product Type
      gwa_gen-transaction_type   =       git_file_data_swap-tb_sfhaart . " Financial Transaction Type
      gwa_gen-external_transaction_number  =       git_file_data_swap-tb_rfha  . " Financial Transaction
      gwa_gen-partner   =       git_file_data_swap-tb_kunnr_new  .  "  Business Partner Number
      gwa_gen-contract_date  =       git_file_data_swap-tb_dvtrab  . " Contract Conclusion Date
      gwa_gen-portfolio   =       git_file_data_swap-rportb  .  "  Portfolio
      gwa_gen-assignment   =       git_file_data_swap-tb_zuond . " Assignment
      gwa_gen-external_reference   =       git_file_data_swap-tb_nordext . " External Reference
      gwa_gen-internal_reference   =       git_file_data_swap-tb_refer . " Internal Reference
      gwa_gen-valuation_class  =       git_file_data_swap-tpm_com_val_class  . " General Valuation Class
      gwa_swap-start_term   =       git_file_data_swap-tb_dblfz  .  "  Term Start
      gwa_swap-start_inclusive   =       git_file_data_swap-tb_sinclbe . " Calculation Period: Start Inclusive vs
      gwa_swap-end_term   =       git_file_data_swap-tb_delfz  .  "  Term End
      gwa_swap-end_inclusive   =       git_file_data_swap-tb_sincle  . " End of Term Inclusive Indicator
      gwa_swap-calendar1   =       git_file_data_swap-skalid1  . " Factory calendar
      gwa_swap-calendar2   =       git_file_data_swap-skalid2  . " Factory calendar
      gwa_swap-pay_side_nominal_amount   =       git_file_data_swap-bapitm_bsaldo_p  . " Amount or balance
      gwa_swap-pay_side_currency   =       git_file_data_swap-tb_wkapbet_p . " Currency of Principal Amount - Financial Product
      gwa_swap-pay_interest_effective_from   =       git_file_data_swap-dguel_kp_p . " Condition Item Effecitve from
      gwa_swap-pay_interest_first_due_date   =       git_file_data_swap-tb_dfaell1_p . " First due date
      gwa_swap-pay_interest_frequency_month   =       git_file_data_swap-ammrhy_p  .  "  Frequency in months
      gwa_swap-pay_interest_rate   =       git_file_data_swap-tb_pzinssv_p . " Agreed interest rate - financial product
      gwa_swap-pay_interest_rate_ref   =       git_file_data_swap-szsref_p . " Reference Interest Rate
      gwa_swap-pay_interest_spread   =       git_file_data_swap-tb_zzs_p . " Interest Markup/Markdown
      gwa_swap-pay_interest_calc_method  =       git_file_data_swap-szbmeth_p  . " Interest Calculation Method
      gwa_swap-pay_interest_calendar_id  =       git_file_data_swap-tfmskalidwt_p  . " Interest Calendar
      gwa_swap-receive_side_nominal_amount   =       git_file_data_swap-bapitm_bsaldo_r  . " Amount or balance
      gwa_swap-receive_side_currency   =       git_file_data_swap-tb_wkapbet_r . " Currency of Principal Amount - Financial Product
      gwa_swap-receive_interest_effective_fro   =       git_file_data_swap-dguel_kp_r  .  "  Condition Item Effecitve from
      gwa_swap-receive_interest_first_due_dat   =       git_file_data_swap-tb_dfaell1_r  .  "  First due date
      gwa_swap-receive_interest_frequency_mon   =       git_file_data_swap-ammrhy_r  .  "  Frequency in months
      gwa_swap-receive_interest_rate   =       git_file_data_swap-tb_pzinssv_r . " Agreed interest rate - financial product
      gwa_swap-receive_interest_rate_ref   =       git_file_data_swap-szsref_r . " Reference Interest Rate
      gwa_swap-receive_interest_spread   =       git_file_data_swap-tb_zzs_r . " Interest Markup/Markdown
      gwa_swap-receive_interest_calc_method  =       git_file_data_swap-szbmeth_r  . " Interest Calculation Method
      gwa_swap-receive_interest_calendar_id  =       git_file_data_swap-tfmskalidwt_r  . " Interest Calendar



      IF  rb_prod = 'X' .

        CALL FUNCTION 'BAPI_FTR_SWAP_CREATE'
          EXPORTING
            otcinterestswap            = gwa_swap
            generalcontractdata        = gwa_gen
*           TESTRUN                    = ' '
          IMPORTING
            financialtransaction       = gwa_gen-external_transaction_number
           companycode                = gwa_gen-company_code
         TABLES
           return                     =  git_error_data_bapi.

         IF NOT git_error_data_bapi[] IS INITIAL.
          LOOP AT git_error_data_bapi.
            IF git_error_data_bapi-type = 'E'.
              error = 1 + error.
              WRITE:   / git_error_data_bapi-type, git_error_data_bapi-message,git_error_data_bapi-field.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              IF sy-subrc = 0.
                WRITE: / 'Contract deal' , gwa_gen-external_transaction_number, 'is successfully created'.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF git_error_data_bapi[] IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          IF sy-subrc = 0.
            WRITE: / 'Contract deal' , gwa_gen-external_transaction_number, 'is successfully created'.
          ENDIF.
        ENDIF.


      ELSEIF rb_test = 'X'.


        CALL FUNCTION 'BAPI_FTR_SWAP_CREATE'
          EXPORTING
            otcinterestswap      = gwa_swap
            generalcontractdata  = gwa_gen
            testrun              = 'X'
          IMPORTING
            financialtransaction = gwa_gen-external_transaction_number
            companycode          = gwa_gen-company_code
          TABLES
            return               = git_error_data_bapi.

        LOOP AT git_error_data_bapi.
          WRITE:   / git_error_data_bapi-type, git_error_data_bapi-message,git_error_data_bapi-field.
          IF git_error_data_bapi-type = 'E'.
            error = 1 + error.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.
  ENDIF.
  IF error = 0.
    WRITE: /  'Input file is error free'.
  ENDIF.

ENDFORM.                    " F_INTEREST_RATE_SWAP
*&---------------------------------------------------------------------*
*&      Form  F_INTEREST_RATE_INSTRUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_interest_rate_instrument .

  PERFORM fupload_xls_file_iri. "UPLOAD EXCEL FILE ELSE.

  IF sy-subrc = 0.
    LOOP AT git_file_data_iri.
      CLEAR: gwa_gen, gwa_iri.

      gwa_gen-company_code   = git_file_data_iri-bukrs  . " Company Code
      gwa_gen-product_type   =       git_file_data_iri-vvsart . " Product Type
      gwa_gen-transaction_type   =       git_file_data_iri-tb_sfhaart . " Financial Transaction Type
      gwa_gen-external_transaction_number  =       git_file_data_iri-tb_rfha  . " Financial Transaction
      gwa_gen-partner   =       git_file_data_iri-tb_kunnr_new  .  "  Business Partner Number
      gwa_gen-contract_date  =       git_file_data_iri-tb_dvtrab  . " Contract Conclusion Date
      gwa_gen-portfolio   =       git_file_data_iri-rportb  .  "  Portfolio
      gwa_gen-assignment   =       git_file_data_iri-tb_zuond . " Assignment
      gwa_gen-external_reference   =       git_file_data_iri-tb_nordext . " External Reference
      gwa_gen-internal_reference   =       git_file_data_iri-tb_refer . " Internal Reference
      gwa_gen-valuation_class  =       git_file_data_iri-tpm_com_val_class  . " General Valuation Class
      gwa_iri-currency    =     git_file_data_iri-tb_wgschft.
      gwa_iri-start_term    =     git_file_data_iri-tb_dblfz.
      gwa_iri-end_term    =     git_file_data_iri-tb_delfz.
      gwa_iri-nominal_amount    =     git_file_data_iri-bapitm_bsaldo.
      gwa_iri-interest_rate_structure    =     git_file_data_iri-tb_irate_structure.
      gwa_iri-flow_type    =     git_file_data_iri-tb_sfhazba.
      gwa_iri-interest_condition_type    =     git_file_data_iri-skoart.
      gwa_iri-interest_rate_reference    =     git_file_data_iri-szsref.
      gwa_iri-interest_rate    =     git_file_data_iri-tm_pzins.
      gwa_iri-interest_rate_amount    =     git_file_data_iri-bapitm_interest_rate_amount.
      gwa_iri-interest_calc_meth    =     git_file_data_iri-szbmeth.
      gwa_iri-frequency_category    =     git_file_data_iri-tb_srhythm.
      gwa_iri-frequency    =     git_file_data_iri-tb_arhytm.
      gwa_iri-start_inclusive    =     git_file_data_iri-tb_sinclbe.
      gwa_iri-interest_capital    =     git_file_data_iri-tb_szinskap.
      gwa_iri-shift_due_date_to_end    =     git_file_data_iri-tb_sfranze.
      gwa_iri-wkday_shift_value_date    =     git_file_data_iri-tb_svwerk.
      gwa_iri-wkday_shift_due_date    =     git_file_data_iri-tb_sfwerk.
      gwa_iri-round_amnt_gen_flow    =     git_file_data_iri-tfm_sround.
      gwa_iri-interest_calendar_id    =     git_file_data_iri-tfmskalidwt.
      gwa_iri-repay_structure    =     git_file_data_iri-tb_repaym_structure.
      gwa_iri-repay_condition_type    =     git_file_data_iri-skoart.
      gwa_iri-repay_amount    =     git_file_data_iri-bapitm_repayment_amount.
      gwa_iri-repay_frequency_cat    =     git_file_data_iri-tb_srhythm.
      gwa_iri-repay_frequency    =     git_file_data_iri-tb_arhytm.
      gwa_iri-repay_day_shift_value_date    =     git_file_data_iri-tb_svwerk.
      gwa_iri-repay_shift_due_date_to_end    =     git_file_data_iri-tb_sfranze.
      gwa_iri-effective_from    =     git_file_data_iri-dguel_kp.
      gwa_iri-interest_first_due_date    =     git_file_data_iri-tbfaeltz.
      gwa_iri-interest_calculation_date    =     git_file_data_iri-dvalut.
      gwa_iri-frequency_unit    =     git_file_data_iri-tb_arhytm_unit.
      gwa_iri-interest_markup_markdown    =     git_file_data_iri-tb_zzs.
      gwa_iri-interest_rate_first_period    =     git_file_data_iri-tfm_pkond1stper.
      gwa_iri-interest_calculation_exponent    =     git_file_data_iri-tb_jzinsre.
      gwa_iri-date_determination_shift_days    =     git_file_data_iri-tbvfaetz.
      gwa_iri-calculation_update_rule    =     git_file_data_iri-tb_updaterule.
      gwa_iri-formula    =     git_file_data_iri-tb_irate_formula.
      gwa_iri-repay_effective_from    =     git_file_data_iri-dguel_kp.
      gwa_iri-repay_first_due_date    =     git_file_data_iri-tbfaeltz.
      gwa_iri-repay_calculation_date    =     git_file_data_iri-dvalut.
      gwa_iri-repay_frequency_unit    =     git_file_data_iri-tb_arhytm_unit.



      IF  rb_prod = 'X' .

        CALL FUNCTION 'BAPI_FTR_IRATE_CREATE'
          EXPORTING
            interestrateinstrument       = gwa_iri
            generalcontractdata          = gwa_gen
*           TESTRUN                      = ' '
         IMPORTING
           financialtransaction         = gwa_gen-external_transaction_number
           companycode                  = gwa_gen-company_code
         TABLES
           return                       = git_error_data_bapi.

         IF NOT git_error_data_bapi[] IS INITIAL.
          LOOP AT git_error_data_bapi.
            IF git_error_data_bapi-type = 'E'.
              error = 1 + error.
              WRITE:   / git_error_data_bapi-type, git_error_data_bapi-message,git_error_data_bapi-field.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              IF sy-subrc = 0.
                WRITE: / 'Contract deal' , gwa_gen-external_transaction_number, 'is successfully created'.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF git_error_data_bapi[] IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          IF sy-subrc = 0.
            WRITE: / 'Contract deal' , gwa_gen-external_transaction_number, 'is successfully created'.
          ENDIF.
        ENDIF.

      ELSEIF rb_test = 'X'.

        CALL FUNCTION 'BAPI_FTR_IRATE_CREATE'
          EXPORTING
            interestrateinstrument = gwa_iri
            generalcontractdata    = gwa_gen
            testrun                = 'X'
          IMPORTING
            financialtransaction   = gwa_gen-external_transaction_number
            companycode            = gwa_gen-company_code
          TABLES
            return                 = git_error_data_bapi.

        LOOP AT git_error_data_bapi.
          WRITE:   / git_error_data_bapi-type, git_error_data_bapi-message,git_error_data_bapi-field.
          IF git_error_data_bapi-type = 'E'.
            error = 1 + error.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.
  ENDIF.
  IF error = 0.
    WRITE: /  'Input file is error free'.
  ENDIF.
ENDFORM.                    " F_INTEREST_RATE_INSTRUMENT
*&---------------------------------------------------------------------*
*&      Form  FUPLOAD_XLS_FILE_COMM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fupload_xls_file_comm .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
         EXPORTING
*         I_FIELD_SEPERATOR          =
          i_line_header              = 'X'
           i_tab_raw_data             = it_raw
           i_filename                 = xls_file
         TABLES
           i_tab_converted_data       = git_file_data_cp[]
*       EXCEPTIONS
*         CONVERSION_FAILED          = 1
*         OTHERS                     = 2
                 .
  IF sy-subrc <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.
ENDFORM.                    " FUPLOAD_XLS_FILE_COMM


*&---------------------------------------------------------------------*
*&      Form  FUPLOAD_XLS_FILE_SWAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fupload_xls_file_swap .
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*    I_FIELD_SEPERATOR          =
     i_line_header              = 'X'
      i_tab_raw_data             = it_raw
      i_filename                 = xls_file
    TABLES
      i_tab_converted_data       = git_file_data_swap[]
*  EXCEPTIONS
*    CONVERSION_FAILED          = 1
*    OTHERS                     = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " FUPLOAD_XLS_FILE_SWAP

*&---------------------------------------------------------------------*
*&      Form  fupload_xls_file_iri
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fupload_xls_file_iri .
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
         EXPORTING
*         I_FIELD_SEPERATOR          =
          i_line_header              ='X'
           i_tab_raw_data             = it_raw
           i_filename                 = xls_file
         TABLES
           i_tab_converted_data       = git_file_data_iri[]
*       EXCEPTIONS
*         CONVERSION_FAILED          = 1
*         OTHERS                     = 2
                 .
  IF sy-subrc <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.
ENDFORM.                    " FUPLOAD_XLS_FILE_IRI
