*----------------------------------------------------------------------*
*   INCLUDE ZPMO_CCC010_T01                                            *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_main_sap,
         linetype  LIKE ptrv_cccmain-linetype,
         pernr     LIKE ptrv_cccmain-pernr,
         pname     LIKE ptrv_cccmain-pname,
         cardnum   LIKE ptrv_cccmain-cardnum,
         cardflag  LIKE ptrv_cccmain-cardflag,
         umsdatum  LIKE ptrv_cccmain-umsdatum,
         ums_zeit  LIKE ptrv_cccmain-ums_zeit,
         sp_kateg  LIKE ptrv_cccmain-sp_kateg,
         spestext  LIKE ptrv_cccmain-spestext,
         s_h_flag  LIKE ptrv_cccmain-s_h_flag,
         umsbetrg  LIKE ptrv_cccmain-umsbetrg,
         umsnachk  LIKE ptrv_cccmain-umsnachk,
         umswaehr(3),
         ums_land  LIKE ptrv_cccmain-ums_land,
         ums_mwst  LIKE ptrv_cccmain-ums_mwst,
         ums_kurs  LIKE ptrv_cccmain-ums_kurs,
         ums_heim  LIKE ptrv_cccmain-ums_heim,
         umsgebur  LIKE ptrv_cccmain-umsgebur,
         umstotal  LIKE ptrv_cccmain-umstotal,
         addcount  LIKE ptrv_cccmain-addcount,
         text_lang LIKE ptrv_cccmain-text_lang,
         umsdoknr  LIKE ptrv_cccmain-umsdoknr,
       END OF type_main_sap.

TYPES: BEGIN OF type_info_ax_asc,
         info(2000),
       END OF type_info_ax_asc,

       BEGIN OF type_header_ax,
         rec_type(1),
         filler_1(1),
         bus_unit_nb(9),
         filler_2(1),
         rec_nb(9),
         filler_3(1),
         rep_prefix(2),
         rep_nb(5),
         rep_version(3),
         filler_4(1),
         rep_version_date(8),
         filler_5(1),
         rep_create_date(8),
         void(31),
         file_version_nb(3),
         filler_6(1896),
       END OF type_header_ax,

       BEGIN OF type_info_ax,
         rec_type(1),
         req_contr_acc_nb(19),
         contr_acc_name(35),
         bill_contr_acc_nb(19),
         bill_contr_acc_name(35),
         fips(4),
         bill_acc_nb(19),
         card_name(26),
         pernr(10),
         cost_centre(10),
         univ_nb(25),
         social_sec_nb(10),
         language(1),
         corp_id(19),
         supplier_ref_nb(11),
         sign(1),
         billed_amount(15),
         billed_tax_amount(15),
         billed_curr(3),
         billed_dec_place(1),
         local_charge_amount(15),
         local_tax_amount(15),
         local_curr_code(3),
         local_dec_place(1),
         curr_exchange_rate(15),
         txn_type_code(2),
         financial_cat(1),
         account_type(5),
         orig_basic_contr_acc_nb(19),
         orig_cm_contr_acc_nb(19),
         txn_nb(15),
         charge_date(8),
         business_proc_date(8),
         bill_date(8),
         cm_ref_nb(17),
         mis_ind(2),
         roc_id(13),
         descr1(42),
         descr2(42),
         descr3(42),
         descr4(42),
         descr_end(206),
         se_nb(10),
         se_chain_code(10),
         se_name1(40),
         se_name2(40),
         se_street1(40),
         se_street2(40),
         se_city(40),
         se_state(6),
         se_zip_code(15),
         se_country_name(35),
         se_country_code(3),
         se_corp_status_code(1),
         se_purch_card_code(2),
         se_purch_card_owner(2),
         sic_code(4),
         se_sales_tax_collected(1),
         ship_postal_code(15),
         ship_city(30),
         ship_country(30),
         ship_state(6),
         dda_nb(22),
         bank_routing_nb(4),
         us_cm_aba_nb(4),
         spool_nb(22),
         funds_access_log_time(8),
         machine_nb(8),
         terminal_loc(40),
         network_route(10),
         terminal_route(10),
         cash_batch_nb(6),
         mis_sub_ind(3),
         vat_percent(5),
         capture_center_code(1),
         mercator_key(21),
         transaction_fee_ind(3),
         void(582),
       END OF type_info_ax,

       BEGIN OF type_info_sap_asc,
         void(800),
       END OF type_info_sap_asc.

* Begin of trailer
TYPES: BEGIN OF type_info_ax_trl,
         info(2000),
       END OF type_info_ax_trl,

       BEGIN OF type_trailer,
         rec_type(1),
       END OF type_trailer.

* Begin of SSN_Nb
TYPES: BEGIN OF type_ssn,
         perid LIKE pa0002-perid,
       END OF type_ssn,

       BEGIN OF type_pernr,
         perid LIKE pa0002-perid,
         pernr LIKE pa0002-pernr,
* Begin of SSNDate
         begda LIKE pa0002-begda,
         endda LIKE pa0002-endda,
* End of SSNDate
       END OF type_pernr,

       BEGIN OF type_concurr_ssn,
          sign(1)    TYPE c,
          option(2)  TYPE c,
          low  LIKE pa0002-perid,
          high LIKE pa0002-perid,
       END OF type_concurr_ssn.
* End of SSN_Nb

TYPES: BEGIN OF type_protokoll,
         index_dataset   TYPE  i,
         conv_status(2)  TYPE  c,
         fehler_text(50) TYPE  c,
         fehler_wert(30) TYPE  c,
         color(3)        TYPE  c,
         data            TYPE  type_info_ax,
         conv_data       TYPE  type_main_sap,
       END OF type_protokoll,
       type_t_protokoll TYPE STANDARD TABLE OF type_protokoll.

TYPES: BEGIN OF type_ccc_sum,
         linetype(1),
         accnumber(10),
         sumpurchase(15)  TYPE n,
         sumaddcharge(15) TYPE n,
         sumtotal(15)     TYPE n,
         numberrecords(8) TYPE n,
       END OF type_ccc_sum.

TYPES:
       BEGIN OF type_car_details,
         rental_date(8),
         rental_loc(30),
         return_date(8),
         void(160),
       END OF type_car_details,

       BEGIN OF type_hotel_details,
         hotel_descr(23),
         guest_name(35),
         arr_date(8),
         dep_date(8),
         duration(3),
         void(129),
       END OF type_hotel_details,

       BEGIN OF type_tel_details,
         from_city(30),
         from_state(6),
         to_number(15),
         ref_nb(18),
         call_time(4),
         call_year(6),
         call_date(4),
         duration(4),
         void(131),
       END OF type_tel_details,

       BEGIN OF type_air_details,
         dep_date(8),
         routing(27),
         class(8),
         carrier(16),
         void(147),
       END OF type_air_details,

       BEGIN OF type_retail_details,
         dep_name(28),
         void1(20),
         time(5),
         descr1(20),
         void2(133),
       END OF type_retail_details,

       BEGIN OF type_restaurant_details,
         bill_nb(10),
         descr1(20),
         charge(12),
         tip(12),
         void(152),
       END OF type_restaurant_details,

       BEGIN OF type_rail_details,
         deploc(20),
         first_dest(20),
         void(166),
       END OF type_rail_details,

       BEGIN OF type_mail_details,
         descr(30),
         bill_nb(9),
         void(167),
       END OF type_mail_details,

       BEGIN OF type_insurance_details,
         prem_id(7),
         policy_name(20),
         policy_nb(12),
         void1(20),
         validity(20),
         void(127),
       END OF type_insurance_details,

       BEGIN OF type_oil_details,
         descr(28),
         bill_nb(10),
         void(168),
       END OF type_oil_details,

       BEGIN OF type_event_details,
         name(30),
         date(8),
         void(168),
       END OF type_event_details,

       BEGIN OF type_various_details,
         descr(40),
         vois(166),
       END OF type_various_details.

TYPES: BEGIN OF ty_ztet_mcc,
       mcc     TYPE ztet_mcc-mcc,
       te_code TYPE ztet_mcc-te_code,
       te_type TYPE ztet_mcc-te_type,
       END OF ty_ztet_mcc.

TYPES: BEGIN OF ty_card_number,
       card_number TYPE p0105-usrid,
       END OF ty_card_number.

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

TYPES: BEGIN OF ty_pa0001,
       pernr TYPE pa0001-pernr,
       subty TYPE pa0001-subty,
       objps TYPE pa0001-objps,
       sprps TYPE pa0001-sprps,
       endda TYPE pa0001-endda,
       begda TYPE pa0001-begda,
       seqnr TYPE pa0001-seqnr,
       ename TYPE pa0001-ename,
       END OF ty_pa0001.

TYPES: BEGIN OF ty_tcurc,
       waers TYPE tcurc-waers,
       isocd TYPE tcurc-isocd,
       altwr TYPE tcurc-altwr,
       END OF ty_tcurc.

TYPES: BEGIN OF ty_addendum,
       record_idntifier TYPE zrecord_idntifier,
       issuer_ica       TYPE zissuer_ica,
       issuer_number    TYPE zissuer_number,
       corp_number      TYPE zcorp_number,
       addendum_type    TYPE zaddendum_type,
       accnt_number     TYPE zaccnt_number,
       ifile_rec_number TYPE zifile_rec_number,
       END OF ty_addendum.

DATA: git_addendum_0  TYPE TABLE OF ZFTV_ADDENDUM_0,
      git_addendum_01 TYPE TABLE OF zftv_addendum_01,
      git_addendum_1  TYPE TABLE OF zftv_addendum_1,
      git_addendum_11 TYPE TABLE OF zftv_addendum_11,
      git_addendum_2  TYPE TABLE OF zftv_addendum_2,
      git_addendum_21 TYPE TABLE OF zftv_addendum_21,
      git_addendum_3  TYPE TABLE OF zftv_addendum_3,
      git_addendum_4  TYPE TABLE OF zftv_addendum_4,
      git_addendum_5  TYPE TABLE OF zftv_addendum_5,
      git_addendum_6  TYPE TABLE OF zftv_addendum_6,
      git_addendum_61 TYPE TABLE OF zftv_addendum_61,
      git_addendum_7  TYPE TABLE OF zftv_addendum_7,
      git_ztet_mcc    TYPE TABLE OF ty_ztet_mcc,
      git_card_number TYPE TABLE OF ty_card_number,
      git_pa0105      TYPE TABLE OF ty_pa0105,
      git_pa0001      TYPE TABLE OF ty_pa0001,
      git_tcurc       TYPE TABLE OF ty_tcurc,
      git_add_00      TYPE TABLE OF ty_addendum.

DATA: gwa_addendum_0  TYPE zftv_addendum_0,
      gwa_addendum_01 TYPE zftv_addendum_01,
      gwa_addendum_1  TYPE zftv_addendum_1,
      gwa_addendum_11 TYPE zftv_addendum_11,
      gwa_addendum_2  TYPE zftv_addendum_2,
      gwa_addendum_21 TYPE zftv_addendum_21,
      gwa_addendum_3  TYPE zftv_addendum_3,
      gwa_addendum_4  TYPE zftv_addendum_4,
      gwa_addendum_5  TYPE zftv_addendum_5,
      gwa_addendum_6  TYPE zftv_addendum_6,
      gwa_addendum_61 TYPE zftv_addendum_61,
      gwa_addendum_7  TYPE zftv_addendum_7,
      gwa_ztet_mcc    TYPE ty_ztet_mcc,
      gwa_card_number TYPE ty_card_number,
      gwa_pa0105      TYPE ty_pa0105,
      gwa_pa0001      TYPE ty_pa0001,
      gwa_tcurc       TYPE ty_tcurc.


DATA: gv_card_number(19) TYPE c,
      gv_txn_date(8)     TYPE c,
      gv_reference(6)    TYPE c,
      gv_ref_ifile_21(6) TYPE c,
      gv_ref_ifile_61(6) TYPE c,
      gv_total_amt(15)   TYPE c.

DATA: git_xparam TYPE TABLE OF zfit_xparam.

CONSTANTS: gc_paramtype TYPE zparamtype    VALUE 'I_P2C_TV_001',
           gc_subtype   TYPE zparamsubtype VALUE 'MASTER_CARD',
           gc_currency  TYPE tcurc-isocd   VALUE  'USD',
           gc_exch      TYPE PTRV_CCC_EXCHANGE_RATE  VALUE  '1.0000000'.
