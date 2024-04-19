*&---------------------------------------------------------------------*
*&  Include           ZFTVR001_CREDIT_CARD_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_addendum_0,
       accnt_number     TYPE zaccnt_number,
       ifile_rec_number TYPE zifile_rec_number,
       AMOUNT_ORG       TYPE ZFTV_addendum_0-AMOUNT_ORG,
       txn_amount       TYPE ztxn_amount,
       txn_date         TYPE ztxn_date,
       merchant_name    TYPE zmerchant_name,
       merchant_city    TYPE zmerchant_city,
       merchant_state   TYPE zmerchant_state,
       merchant_country TYPE zmerchant_country,
       org_curr_code    TYPE zorg_curr_code,
       CONV_RATE        TYPE ZFTV_addendum_0-CONV_RATE,
       END OF ty_addendum_0.

TYPES: BEGIN OF ty_addendum_1,
       record_idntifier TYPE zrecord_idntifier,
       issuer_ica       TYPE zissuer_ica,
       issuer_number    TYPE zissuer_number,
       corp_number      TYPE zcorp_number,
       addendum_type    TYPE zaddendum_type,
       accnt_number     TYPE zaccnt_number,
       ifile_rec_number TYPE zifile_rec_number,
       product_code     TYPE zproduct_code,
       item_description TYPE zitem_description,
       item_qty         TYPE zitem_qty,
       ext_item_amt     TYPE zext_item_amt,
       reference        TYPE zreference,
       END OF ty_addendum_1.

TYPES: BEGIN OF ty_addendum_2,
       record_idntifier TYPE zrecord_idntifier,
       issuer_ica       TYPE zissuer_ica,
       issuer_number    TYPE zissuer_number,
       corp_number      TYPE zcorp_number,
       addendum_type    TYPE zaddendum_type,
       accnt_number     TYPE zaccnt_number,
       ifile_rec_number TYPE zifile_rec_number,
       passenger_name   TYPE zpassenger_name,
       departure_date   TYPE zdeparture_date,
       city_of_origin   TYPE zcity_of_origin,
       ta_name          TYPE zta_name,
       ticket_number    TYPE zticket_number,
       total_fare       TYPE ztotal_fare,
       reference        TYPE zreference,
       END OF ty_addendum_2.

TYPES: BEGIN OF ty_addendum_21,
       record_idntifier TYPE zrecord_idntifier,
       issuer_ica       TYPE zissuer_ica,
       issuer_number    TYPE zissuer_number,
       corp_number      TYPE zcorp_number,
       addendum_type    TYPE zaddendum_type,
       accnt_number     TYPE zaccnt_number,
       ifile_rec_number TYPE zifile_rec_number,
       carrier_code     TYPE zcarrier_code,
       city_of_origin   TYPE zcity_of_origin,
       travel_date      TYPE ztravel_date,
       city_of_dest     TYPE zcity_of_dest,
       ref_ifile_rec    TYPE zifile_rec_number,
       END OF ty_addendum_21.

TYPES: BEGIN OF ty_addendum_3,
       record_idntifier TYPE zrecord_idntifier,
       issuer_ica       TYPE zissuer_ica,
       issuer_number    TYPE zissuer_number,
       corp_number      TYPE zcorp_number,
       addendum_type    TYPE zaddendum_type,
       accnt_number     TYPE zaccnt_number,
       ifile_rec_number TYPE zifile_rec_number,
       arrival_date     TYPE zarrival_date,
       departure_date   TYPE zdeparture_date,
       folio_number     TYPE zfolio_number,
       property_phone   TYPE zproperty_phone,
       reference        TYPE zreference,
       END OF ty_addendum_3.

TYPES: BEGIN OF ty_addendum_4,
       record_idntifier TYPE zrecord_idntifier,
       issuer_ica       TYPE zissuer_ica,
       issuer_number    TYPE zissuer_number,
       corp_number      TYPE zcorp_number,
       addendum_type    TYPE zaddendum_type,
       accnt_number     TYPE zaccnt_number,
       ifile_rec_number TYPE zifile_rec_number,
       rental_agr_no    TYPE zrental_agr_no,
       renter_name      TYPE zrenter_name,
       rtl_ret_city     TYPE zrtl_ret_city,
       rtl_ret_state    TYPE zrtl_ret_state,
       rtl_return_date  TYPE zrtl_return_date,
       reference        TYPE zreference,
       END OF ty_addendum_4.

TYPES: BEGIN OF ty_addendum_6,
       record_idntifier TYPE zrecord_idntifier,
       issuer_ica       TYPE zissuer_ica,
       issuer_number    TYPE zissuer_number,
       corp_number      TYPE zcorp_number,
       addendum_type    TYPE zaddendum_type,
       accnt_number     TYPE zaccnt_number,
       ifile_rec_number TYPE zifile_rec_number,
       mchant_st_addr   TYPE zmchant_st_addr,
       mfuel_unit_price TYPE zmfuel_unit_price,
       mfuel_qty        TYPE zmfuel_qty,
       mfuel_sale_amt   TYPE zmfuel_sale_amt,
       driver_no        TYPE zdriver_no,
       reference        TYPE zreference,
       END OF ty_addendum_6.

TYPES: BEGIN OF ty_pa0001,
       pernr TYPE pa0001-pernr,
       sname TYPE pa0001-sname,
       END OF ty_pa0001.

TYPES: BEGIN OF ty_pa0105,
       pernr TYPE pa0105-pernr,
       subty TYPE pa0105-subty,
       objps TYPE pa0105-objps,
       sprps TYPE pa0105-sprps,
       endda TYPE pa0105-endda,
       begda TYPE pa0105-begda,
       seqnr TYPE pa0105-seqnr,
       usrid TYPE pa0105-usrid,
       END OF ty_pa0105.

TYPES: BEGIN OF ty_card_number,
       accnt_number TYPE zaccnt_number,
       pernr        TYPE pa0001-pernr,
       emp_name     TYPE pa0001-sname,
       END OF ty_card_number.

TYPES: BEGIN OF ty_pa0001_check_expense,
       pernr TYPE pa0001-pernr,
       kostl TYPE pa0001-kostl,
       bukrs TYPE pa0001-bukrs,
       persa TYPE pa0001-werks,
       persg TYPE pa0001-persg,
       persk TYPE pa0001-persk,
       vdsk1 TYPE pa0001-vdsk1,
       sname TYPE pa0001-sname,
       END OF ty_pa0001_check_expense.

TYPES: BEGIN OF ty_pa0105_check_expense,
       pernr TYPE pa0105-pernr,
       endda TYPE pa0105-endda,
       END OF ty_pa0105_check_expense.

TYPES: BEGIN OF ty_company_code,
       pernr TYPE pa0001-pernr,
       bukrs TYPE pa0001-bukrs,
       END OF ty_company_code.

TYPES: BEGIN OF ty_cost_center,
       pernr TYPE pa0001-pernr,
       kostl TYPE pa0001-kostl,
       END OF ty_cost_center.

TABLES: pa0001.

*----------------------------------------------------------------------*
* Internal Tables                                                      *
*----------------------------------------------------------------------*

DATA: git_pa0001                  TYPE TABLE OF ty_pa0001,
      git_pa0105                  TYPE TABLE OF ty_pa0105,
      git_card_number             TYPE TABLE OF ty_card_number,
      git_addendum_0              TYPE TABLE OF ty_addendum_0,
      git_addendum_0_temp         TYPE TABLE OF ty_addendum_0,
      git_addendum_1              TYPE TABLE OF ty_addendum_1,
      git_addendum_2              TYPE TABLE OF ty_addendum_2,
      git_addendum_21             TYPE TABLE OF ty_addendum_21,
      git_addendum_3              TYPE TABLE OF ty_addendum_3,
      git_addendum_4              TYPE TABLE OF ty_addendum_4,
      git_addendum_6              TYPE TABLE OF ty_addendum_6,
      git_tree_display_temp       TYPE TABLE OF snodetext,
      git_tree_display            TYPE TABLE OF snodetext,
      git_tree_display_txn        TYPE TABLE OF snodetext,
      git_pa0001_check_expense    TYPE STANDARD TABLE OF ty_pa0001_check_expense,
      gwa_pa0001_check_expense    TYPE ty_pa0001_check_expense,
*      gt_pa0105_check_expense    TYPE STANDARD TABLE OF ty_pa0105_check_expense,
*      wa_pa0105_check_expense    TYPE ty_pa0105_check_expense,
      gc_end_date_check_expense   TYPE sy-datum VALUE '99991231',
      gv_authp_check_expense      TYPE c,
      git_company_code            TYPE STANDARD TABLE OF ty_company_code,
      gwa_company_code            TYPE ty_company_code,
      git_cost_center             TYPE STANDARD TABLE OF ty_cost_center,
      gwa_cost_center             TYPE ty_cost_center,
      gwa_pa0001                  type ty_pa0001.
*----------------------------------------------------------------------*
* Constants                                                            *
*----------------------------------------------------------------------*

CONSTANTS: gc_x(1)   TYPE c VALUE 'X',
           gc_0011   TYPE pa0105-usrty VALUE '0011',
           gc_own(3) TYPE c VALUE 'OWN'.

*----------------------------------------------------------------------*
* Range Object                                                         *
*----------------------------------------------------------------------*

RANGES: r_company_code_ex FOR pa0001-bukrs,
        r_cost_center_ex  FOR pa0001-kostl.
