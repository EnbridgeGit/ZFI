*&---------------------------------------------------------------------*
*&  Include           ZFTV001_CREDIT_CARD_F
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  f_validate_pernr
*&---------------------------------------------------------------------*
*       Validate Personnel Number
*----------------------------------------------------------------------*
FORM f_validate_pernr.

  DATA: lv_pernr TYPE pa0003-pernr.

  SELECT SINGLE pernr
         FROM pa0003
         INTO lv_pernr
         WHERE pernr IN s_pernr.

  IF sy-subrc <> 0.
    MESSAGE e000(zfi01) WITH text-040.
  ENDIF.

ENDFORM.                    "f_validate_pernr

*&---------------------------------------------------------------------*
*&      Form  f_validate_bukrs
*&---------------------------------------------------------------------*
*       Validate Company Code
*----------------------------------------------------------------------*
FORM f_validate_bukrs.

  DATA: lv_bukrs TYPE t001-bukrs.

  SELECT SINGLE bukrs
         FROM t001
         INTO lv_bukrs
         WHERE bukrs IN s_bukrs.

  IF sy-subrc <> 0.
    MESSAGE e000(zfi01) WITH text-041.
  ENDIF.

ENDFORM.                    "f_validate_bukrs

*&---------------------------------------------------------------------*
*&      Form  f_validate_kostl
*&---------------------------------------------------------------------*
*       Validate Cost Center
*----------------------------------------------------------------------*
FORM f_validate_kostl.

  DATA: lv_kostl TYPE csks-kostl.

  SELECT SINGLE kostl
         FROM csks
         INTO lv_kostl
         WHERE kostl IN s_kostl.

  IF sy-subrc <> 0.
    MESSAGE e000(zfi01) WITH text-042.
  ENDIF.

ENDFORM.                    "f_validate_kostl

*&---------------------------------------------------------------------*
*&      Form  f_select_pa0105
*&---------------------------------------------------------------------*
*       Get Employee Number and Employee Name
*----------------------------------------------------------------------*

FORM f_select_pa0001.

*Begin of asingh
  CLEAR : gwa_pa0001.
*   Adding authorized entries to TABLE GIT_PA0001 which is further used for displaying data.
  IF git_pa0001_check_expense[] IS NOT INITIAL.
    LOOP AT git_pa0001_check_expense INTO gwa_pa0001_check_expense.

      gwa_pa0001-pernr = gwa_pa0001_check_expense-pernr.
      gwa_pa0001-sname = gwa_pa0001_check_expense-sname.
      APPEND  gwa_pa0001 TO git_pa0001.
      CLEAR : gwa_pa0001.

    ENDLOOP.
  ENDIF.
*  SELECT pernr sname
*         FROM pa0001
*         INTO TABLE git_pa0001
*         WHERE pernr IN s_pernr AND
*               bukrs IN s_bukrs AND
*               kostl IN s_kostl.
*End of asingh
  IF git_pa0001 IS INITIAL.
    MESSAGE i000(zfi01) WITH text-039.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    "f_select_pa0001

*&---------------------------------------------------------------------*
*&      Form  f_select_pa0105
*&---------------------------------------------------------------------*
*       Get Employee Credit Card details from PA0105/0011
*----------------------------------------------------------------------*
FORM f_select_pa0105.

  DATA: lit_pa0001 TYPE TABLE OF ty_pa0001.

  IF git_pa0001[] IS NOT INITIAL.

    lit_pa0001 = git_pa0001.
    SORT lit_pa0001 BY pernr.
    DELETE ADJACENT DUPLICATES FROM lit_pa0001
           COMPARING pernr.

    SELECT pernr subty objps sprps endda begda seqnr usrid
           FROM pa0105
           INTO TABLE git_pa0105
           FOR ALL ENTRIES IN lit_pa0001
           WHERE pernr = lit_pa0001-pernr AND
                 usrty = gc_0011.

* If data not found, display message and exit
    IF git_pa0105 IS INITIAL.
      MESSAGE i000(zfi01) WITH text-039.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.

ENDFORM.                    "f_select_pa0105

*&---------------------------------------------------------------------*
*&      Form  f_select_addendum_0
*&---------------------------------------------------------------------*
*       Get Addendum 0 Details - Financial Transaction Record
*----------------------------------------------------------------------*
FORM f_select_addendum_0.

  DATA: lit_pa0105 TYPE TABLE OF ty_pa0105.

  DATA: lwa_card_number TYPE ty_card_number,
        lwa_pa0105      TYPE ty_pa0105,
        lwa_pa0001      TYPE ty_pa0001.

  IF git_pa0105[] IS NOT INITIAL.

    lit_pa0105 = git_pa0105.
    SORT lit_pa0105 BY usrid.
    DELETE ADJACENT DUPLICATES FROM lit_pa0105
           COMPARING usrid.

* Build a table containing Employee Number, Employee Name
* and Credit Card Number
    LOOP AT lit_pa0105 INTO lwa_pa0105.

      lwa_card_number-accnt_number = lwa_pa0105-usrid+2(16).
      lwa_card_number-pernr        = lwa_pa0105-pernr.

      READ TABLE git_pa0001 INTO lwa_pa0001
                 WITH KEY pernr = lwa_pa0105-pernr.
      IF sy-subrc = 0.
        lwa_card_number-emp_name = lwa_pa0001-sname.
      ENDIF.

      APPEND lwa_card_number TO git_card_number.
      CLEAR: lwa_card_number,
             lwa_pa0001.

    ENDLOOP.

    SELECT accnt_number ifile_rec_number AMOUNT_ORG txn_amount txn_date merchant_name
           merchant_city  merchant_state merchant_country org_curr_code CONV_RATE
           FROM zftv_addendum_0
           INTO TABLE git_addendum_0
           FOR ALL ENTRIES IN git_card_number
           WHERE accnt_number = git_card_number-accnt_number AND
                 txn_date IN s_tdate.

    IF git_addendum_0 IS NOT INITIAL.

      git_addendum_0_temp = git_addendum_0.
      SORT git_addendum_0_temp BY accnt_number.
      DELETE ADJACENT DUPLICATES FROM git_addendum_0_temp
             COMPARING accnt_number.
    ELSE.
      MESSAGE i000(zfi01) WITH text-039.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.

ENDFORM.                    "f_select_addendum_0

*&---------------------------------------------------------------------*
*&      Form  f_select_addendum_1
*&---------------------------------------------------------------------*
*       Get Addendum 1 details - Purchasing Card Addendum
*----------------------------------------------------------------------*
FORM f_select_addendum_1.

  IF git_addendum_0_temp IS NOT INITIAL.

    SELECT record_idntifier issuer_ica issuer_number corp_number addendum_type
           accnt_number ifile_rec_number product_code item_description item_qty
           ext_item_amt reference
           FROM zftv_addendum_1
           INTO TABLE git_addendum_1
           FOR ALL ENTRIES IN git_addendum_0_temp
           WHERE accnt_number = git_addendum_0_temp-accnt_number AND
                 txn_date IN s_tdate.

  ENDIF.

ENDFORM.                    "f_select_addendum_1

*&---------------------------------------------------------------------*
*&      Form  f_select_addendum_2
*&---------------------------------------------------------------------*
*       Get Addendum 2 details - Passenger Transport Addendum
*----------------------------------------------------------------------*
FORM f_select_addendum_2.

  IF git_addendum_0_temp IS NOT INITIAL.

    SELECT record_idntifier issuer_ica issuer_number corp_number addendum_type
           accnt_number ifile_rec_number passenger_name departure_date
           city_of_origin ta_name ticket_number total_fare reference
           FROM zftv_addendum_2
           INTO TABLE git_addendum_2
           FOR ALL ENTRIES IN git_addendum_0_temp
           WHERE accnt_number = git_addendum_0_temp-accnt_number AND
                 txn_date IN s_tdate.

  ENDIF.

ENDFORM.                    "f_select_addendum_2

*&---------------------------------------------------------------------*
*&      Form  f_select_addendum_21
*&---------------------------------------------------------------------*
*       Get Addendum 21 details - Passenger Transport Leg Addendum
*----------------------------------------------------------------------*
FORM f_select_addendum_21.

  IF git_addendum_0_temp IS NOT INITIAL.

    SELECT record_idntifier issuer_ica issuer_number corp_number addendum_type
           accnt_number ifile_rec_number carrier_code city_of_origin
           travel_date city_of_dest ref_ifile_rec
           FROM zftv_addendum_21
           INTO TABLE git_addendum_21
           FOR ALL ENTRIES IN git_addendum_0_temp
           WHERE accnt_number = git_addendum_0_temp-accnt_number AND
                 txn_date IN s_tdate.

  ENDIF.

ENDFORM.                    "f_select_addendum_21

*&---------------------------------------------------------------------*
*&      Form  f_select_addendum_3
*&---------------------------------------------------------------------*
*       Get Addendum 3 details - Lodging Addendum
*----------------------------------------------------------------------*
FORM f_select_addendum_3.

  IF git_addendum_0_temp IS NOT INITIAL.

    SELECT record_idntifier issuer_ica issuer_number corp_number addendum_type
           accnt_number ifile_rec_number arrival_date departure_date
           folio_number property_phone reference
           FROM zftv_addendum_3
           INTO TABLE git_addendum_3
           FOR ALL ENTRIES IN git_addendum_0_temp
           WHERE accnt_number = git_addendum_0_temp-accnt_number AND
                 txn_date IN s_tdate.

  ENDIF.

ENDFORM.                    "f_select_addendum_3

*&---------------------------------------------------------------------*
*&      Form  f_select_addendum_4
*&---------------------------------------------------------------------*
*       Get Addendum 4 details - Car Rental Addendum
*----------------------------------------------------------------------*
FORM f_select_addendum_4.

  IF git_addendum_0_temp IS NOT INITIAL.

    SELECT record_idntifier issuer_ica issuer_number corp_number addendum_type
           accnt_number ifile_rec_number rental_agr_no renter_name rtl_ret_city
           rtl_ret_state rtl_return_date reference
           FROM zftv_addendum_4
           INTO TABLE git_addendum_4
           FOR ALL ENTRIES IN git_addendum_0_temp
           WHERE accnt_number = git_addendum_0_temp-accnt_number AND
                 txn_date IN s_tdate.

  ENDIF.

ENDFORM.                    "f_select_addendum_4

*&---------------------------------------------------------------------*
*&      Form  F_SELECT_ADDENDUM_6
*&---------------------------------------------------------------------*
*       Get Addendum 6 details - Fleet Card Addendum
*----------------------------------------------------------------------*
FORM f_select_addendum_6.

  IF git_addendum_0_temp IS NOT INITIAL.

    SELECT record_idntifier issuer_ica issuer_number corp_number addendum_type
           accnt_number ifile_rec_number mchant_st_addr mfuel_unit_price
           mfuel_qty mfuel_sale_amt driver_no reference
           FROM zftv_addendum_6
           INTO TABLE git_addendum_6
           FOR ALL ENTRIES IN git_addendum_0_temp
           WHERE accnt_number = git_addendum_0_temp-accnt_number AND
                 txn_date IN s_tdate.

  ENDIF.

ENDFORM.                    "F_SELECT_ADDENDUM_6

*&---------------------------------------------------------------------*
*&      Form  f_build_tree_data
*&---------------------------------------------------------------------*
*       Build the ALV Tree Data
*----------------------------------------------------------------------*
FORM f_build_tree_data.

  DATA: lwa_addendum_0   TYPE ty_addendum_0.

  DATA: lv_txn_amt       TYPE ztxn_amount,
        lv_card_number   TYPE zaccnt_number.

* Node 1 - Employee Credit Card Transactions
  PERFORM f_construct_tree_data USING 1
                                      35 1 text-001
                                      0  1 space
                                      0  1 space
                                      0  1 space
                                      0  1 space
                                      0  1 space
                                      0  1 space
                                      0  1 space
                                      gc_x.

  APPEND LINES OF git_tree_display_temp TO git_tree_display.
  REFRESH git_tree_display_temp.

  SORT git_addendum_0  BY accnt_number.
  SORT git_addendum_1  BY accnt_number reference.
  SORT git_addendum_2  BY accnt_number reference.
  SORT git_addendum_21 BY accnt_number ref_ifile_rec.
  SORT git_addendum_3  BY accnt_number reference.
  SORT git_addendum_4  BY accnt_number reference.
  SORT git_addendum_6  BY accnt_number reference.

  LOOP AT git_addendum_0 INTO lwa_addendum_0.

    AT FIRST.
* Header for Summary Line
      PERFORM f_construct_tree_data USING 2
                                          20 3 text-002
                                          30 3 text-003
                                          20 3 text-004
                                          25 3 text-005
                                          0  3 space
                                          0  3 space
                                          0  3 space
                                          0  3 space
                                          gc_x.

      APPEND LINES OF git_tree_display_temp TO git_tree_display.
      REFRESH git_tree_display_temp.
    ENDAT.

* Populate all Addendum Details
    PERFORM f_addendum_details USING    lwa_addendum_0
                               CHANGING lv_card_number.

* Accrue the Original Amount for each Employee/Credit Card
    lv_txn_amt = lv_txn_amt + lwa_addendum_0-AMOUNT_ORG. "txn_amount.

* For each Credit Card Number, display the different Addendum Details
    AT END OF accnt_number.

* Populate Summary Details
      PERFORM f_summary_details USING lwa_addendum_0
                                      lv_txn_amt.

      CLEAR: lv_card_number,
             lv_txn_amt.

    ENDAT.

  ENDLOOP.

ENDFORM.                    "f_build_tree_data

*&---------------------------------------------------------------------*
*&      Form  f_addendum_details
*&---------------------------------------------------------------------*
*       Addendum Details
*----------------------------------------------------------------------*
FORM f_addendum_details USING    p_addendum_0  TYPE ty_addendum_0
                        CHANGING p_card_number TYPE zaccnt_number.

  DATA: lv_field1      TYPE seu_text,
        lv_field2      TYPE seu_text,
        lv_field3      TYPE seu_text,
        lv_field4      TYPE seu_text,
        lv_field5      TYPE seu_text,
        lv_field6      TYPE seu_text,
        lv_field7      TYPE seu_text,
        lv_field8      TYPE seu_text.

  DATA: lwa_addendum_0 TYPE ty_addendum_0.

  lwa_addendum_0 = p_addendum_0.

* Addendum 0 Header
  IF p_card_number <> p_addendum_0-accnt_number.
    PERFORM f_construct_tree_data USING 3
                                        30 4 text-015
                                        20 4 text-016
                                        20 4 text-017
                                        20 4 text-014
                                        20 4 text-013
                                        20 4 text-044
                                        20 4 text-045
                                        20 4 text-046
                                        gc_x.

    p_card_number = p_addendum_0-accnt_number.
  ENDIF.

* Addendum 0 Details
  SHIFT p_addendum_0-AMOUNT_ORG right.
  SHIFT p_addendum_0-txn_amount right.
  SHIFT p_addendum_0-CONV_RATE right.
  lv_field1 = p_addendum_0-merchant_name.
  lv_field2 = p_addendum_0-merchant_city.
  lv_field4 = p_addendum_0-txn_date.
  lv_field5 = p_addendum_0-AMOUNT_ORG.
  lv_field6 = p_addendum_0-txn_amount.
  lv_field7 = p_addendum_0-org_curr_code.
  lv_field8 = p_addendum_0-CONV_RATE.
  CONCATENATE p_addendum_0-merchant_state
              p_addendum_0-merchant_country
              INTO lv_field3
              SEPARATED BY ','.

* Display Each Transaction Details
  PERFORM f_construct_tree_data USING 3
                                      30 4 lv_field1
                                      20 4 lv_field2
                                      20 4 lv_field3
                                      20 4 lv_field4
                                      20 4 lv_field5
                                      20 4 lv_field6
                                      20 4 lv_field7
                                      20 4 lv_field8
                                      space.

  CLEAR: lv_field1, lv_field2,
         lv_field3, lv_field4,
         lv_field5, lv_field6,
         lv_field7, lv_field8.

* Populate Purchasing Card Addendum Record Details
  PERFORM f_addendum_1_details USING lwa_addendum_0.

* Populate Passenger Transport Addendum Record Details
  PERFORM f_addendum_2_details USING lwa_addendum_0.

* Populate Lodging Addendum Record Details
  PERFORM f_addendum_3_details USING lwa_addendum_0.

* Populate Car Rental Addendum Record Details
  PERFORM f_addendum_4_details USING lwa_addendum_0.

* Populate Fleet Card Addendum Record Details
  PERFORM f_addendum_6_details USING lwa_addendum_0.

ENDFORM.                    "f_addendum_details

*&---------------------------------------------------------------------*
*&      Form  f_addendum_1_details
*&---------------------------------------------------------------------*
*       Addendum 1 details for ALV Tree
*----------------------------------------------------------------------*
FORM f_addendum_1_details USING p_addendum_0 TYPE ty_addendum_0.

  DATA: lv_card_number TYPE zaccnt_number,
        lv_field1      TYPE seu_text,
        lv_field2      TYPE seu_text,
        lv_field3      TYPE seu_text,
        lv_field4      TYPE seu_text,
        lv_tabix       TYPE i.

  DATA: lwa_addendum_1 TYPE ty_addendum_1.

  READ TABLE git_addendum_1 INTO lwa_addendum_1
             WITH KEY accnt_number = p_addendum_0-accnt_number
                      reference = p_addendum_0-ifile_rec_number
                      BINARY SEARCH.

  lv_tabix = sy-tabix.

  WHILE lwa_addendum_1-accnt_number = p_addendum_0-accnt_number AND
        lwa_addendum_1-reference = p_addendum_0-ifile_rec_number AND
        sy-subrc = 0.

* Addendum 1 Header
    IF lv_card_number <> lwa_addendum_1-accnt_number.
      PERFORM f_construct_tree_data USING 4
                                          15 4 text-034
                                          30 4 text-035
                                          15 4 text-036
                                          20 4 text-037
                                          0  4 space
                                          0  4 space
                                          0  4 space
                                          0  4 space
                                          gc_x.

      lv_card_number = lwa_addendum_1-accnt_number.
    ENDIF.

* Addendum 1 Details
    SHIFT lwa_addendum_1-ext_item_amt right.
    lv_field1 = lwa_addendum_1-product_code.
    lv_field2 = lwa_addendum_1-item_description.
    lv_field3 = lwa_addendum_1-item_qty.
    lv_field4 = lwa_addendum_1-ext_item_amt.

* Build Tree Data
    PERFORM f_construct_tree_data USING 4
                                        15 4 lv_field1
                                        30 4 lv_field2
                                        15 4 lv_field3
                                        20 4 lv_field4
                                        0  4 space
                                        0  4 space
                                        0  4 space
                                        0  4 space
                                        space.

    CLEAR: lv_field1, lv_field2, lv_field3, lv_field4.

    lv_tabix = lv_tabix + 1.

    READ TABLE git_addendum_1 INTO lwa_addendum_1
               INDEX lv_tabix.

  ENDWHILE.

  CLEAR: lv_tabix,
         lv_card_number.

ENDFORM.                    "f_addendum_1_details

*&---------------------------------------------------------------------*
*&      Form  f_addendum_2_details
*&---------------------------------------------------------------------*
*       Addendum 2 details for ALV Tree
*----------------------------------------------------------------------*
FORM f_addendum_2_details USING p_addendum_0 TYPE ty_addendum_0.

  DATA: lwa_addendum_2  TYPE ty_addendum_2.

  DATA: lv_card_number TYPE zaccnt_number,
        lv_field1 TYPE seu_text,
        lv_field2 TYPE seu_text,
        lv_field3 TYPE seu_text,
        lv_field4 TYPE seu_text,
        lv_field5 TYPE seu_text,
        lv_tabix  TYPE i.

  READ TABLE git_addendum_2 INTO lwa_addendum_2
             WITH KEY accnt_number = p_addendum_0-accnt_number
                      reference = p_addendum_0-ifile_rec_number
                      BINARY SEARCH.

  lv_tabix = sy-tabix.

  WHILE lwa_addendum_2-accnt_number = p_addendum_0-accnt_number AND
        lwa_addendum_2-reference = p_addendum_0-ifile_rec_number AND
        sy-subrc = 0.

* Addendum 2 Header
    IF lv_card_number <> p_addendum_0-accnt_number.
      PERFORM f_construct_tree_data USING 4
                                          22 6 text-006
                                          20 6 text-007
                                          20 6 text-008
                                          20 6 text-028
                                          17 6 text-029
                                          0  6 space
                                          0  6 space
                                          0  6 space
                                          gc_x.

      lv_card_number = p_addendum_0-accnt_number.
    ENDIF.

    lv_field1 = lwa_addendum_2-passenger_name.
    SHIFT lv_field1 LEFT DELETING LEADING space.

    lv_field2 = lwa_addendum_2-departure_date.
    lv_field3 = lwa_addendum_2-city_of_origin.
    lv_field4 = lwa_addendum_2-ta_name.
    lv_field5 = lwa_addendum_2-ticket_number.

* Addendum 2 Details
    PERFORM f_construct_tree_data USING 4
                                        22 6 lv_field1
                                        20 6 lv_field2
                                        20 6 lv_field3
                                        20 6 lv_field4
                                        17 6 lv_field5
                                        0  6 space
                                        0  6 space
                                        0  6 space
                                        space.

    CLEAR: lv_field1, lv_field2, lv_field3, lv_field4, lv_field5.

* Addendum 21 Details
    PERFORM f_addendum_21_details USING lwa_addendum_2.

    lv_tabix = lv_tabix + 1.

    READ TABLE git_addendum_2 INTO lwa_addendum_2
               INDEX lv_tabix.

  ENDWHILE.

  CLEAR: lv_tabix,
         lv_card_number.

ENDFORM.                    "f_addendum_2_details

*&---------------------------------------------------------------------*
*&      Form  f_addendum_21_details
*&---------------------------------------------------------------------*
*       Addendum 21 details for ALV Tree
*----------------------------------------------------------------------*
FORM f_addendum_21_details USING p_addendum_2 TYPE ty_addendum_2.

  DATA: lv_card_number TYPE zaccnt_number,
        lv_field1 TYPE seu_text,
        lv_field2 TYPE seu_text,
        lv_field3 TYPE seu_text,
        lv_field4 TYPE seu_text,
        lv_tabix  TYPE i.

  DATA: lwa_addendum_21 TYPE ty_addendum_21.

  READ TABLE git_addendum_21 INTO lwa_addendum_21
             WITH KEY accnt_number = p_addendum_2-accnt_number
                      ref_ifile_rec = p_addendum_2-ifile_rec_number
                      BINARY SEARCH.

  lv_tabix = sy-tabix.

  WHILE lwa_addendum_21-accnt_number = p_addendum_2-accnt_number AND
        lwa_addendum_21-ref_ifile_rec = p_addendum_2-ifile_rec_number AND
        sy-subrc = 0.

* Addendum 21 Header
    IF lv_card_number <> lwa_addendum_21-accnt_number.
      PERFORM f_construct_tree_data USING 5
                                          15 6 text-030
                                          20 6 text-031
                                          15 6 text-032
                                          20 6 text-033
                                          0  6 space
                                          0  6 space
                                          0  6 space
                                          0  6 space
                                          gc_x.

      lv_card_number = lwa_addendum_21-accnt_number.
    ENDIF.

* Addendum 21 Details
    lv_field1 = lwa_addendum_21-carrier_code .
    lv_field2 = lwa_addendum_21-city_of_origin.
    lv_field3 = lwa_addendum_21-travel_date.
    lv_field4 = lwa_addendum_21-city_of_dest.

    PERFORM f_construct_tree_data USING 5
                                        15 6 lv_field1
                                        20 6 lv_field2
                                        15 6 lv_field3
                                        20 6 lv_field4
                                        0  6 space
                                        0  6 space
                                        0  6 space
                                        0  6 space
                                        space.

    CLEAR: lv_field1, lv_field2, lv_field3, lv_field4.

    lv_tabix = lv_tabix + 1.

    READ TABLE git_addendum_21 INTO lwa_addendum_21
               INDEX lv_tabix.

  ENDWHILE.

  CLEAR: lv_tabix,
         lv_card_number.

ENDFORM.                    "f_addendum_21_details

*&---------------------------------------------------------------------*
*&      Form  f_addendum_3_details
*&---------------------------------------------------------------------*
*       Addendum 3 details for ALV Tree
*----------------------------------------------------------------------*
FORM f_addendum_3_details USING p_addendum_0 TYPE ty_addendum_0.

  DATA: lv_card_number TYPE zaccnt_number,
        lv_field1      TYPE seu_text,
        lv_field2      TYPE seu_text,
        lv_field3      TYPE seu_text,
        lv_field4      TYPE seu_text,
        lv_tabix       TYPE i.

  DATA: lwa_addendum_3 TYPE ty_addendum_3.

  READ TABLE git_addendum_3 INTO lwa_addendum_3
             WITH KEY accnt_number = p_addendum_0-accnt_number
                      reference = p_addendum_0-ifile_rec_number
                      BINARY SEARCH.

  lv_tabix = sy-tabix.

  WHILE lwa_addendum_3-accnt_number = p_addendum_0-accnt_number AND
        lwa_addendum_3-reference = p_addendum_0-ifile_rec_number AND
        sy-subrc = 0.

* Addendum 3 Header
    IF lv_card_number <> p_addendum_0-accnt_number.
      PERFORM f_construct_tree_data USING 4
                                          20 7 text-009
                                          20 7 text-010
                                          20 7 text-011
                                          20 7 text-012
                                          0  7 space
                                          0  7 space
                                          0  7 space
                                          0  7 space
                                          gc_x.

      lv_card_number = p_addendum_0-accnt_number.
    ENDIF.

    lv_field1 = lwa_addendum_3-arrival_date.
    lv_field2 = lwa_addendum_3-departure_date.
    lv_field3 = lwa_addendum_3-folio_number.
    lv_field4 = lwa_addendum_3-property_phone.

* Addendum 3 Details
    PERFORM f_construct_tree_data USING 4
                                        20 7 lv_field1
                                        20 7 lv_field2
                                        20 7 lv_field3
                                        20 7 lv_field4
                                        0  7 space
                                        0  7 space
                                        0  7 space
                                        0  7 space
                                        space.

    CLEAR: lv_field1, lv_field2, lv_field3, lv_field4.

    lv_tabix = lv_tabix + 1.

    READ TABLE git_addendum_3 INTO lwa_addendum_3
               INDEX lv_tabix.

  ENDWHILE.

  CLEAR: lv_tabix,
         lv_card_number.

ENDFORM.                    "f_addendum_3_details

*&---------------------------------------------------------------------*
*&      Form  f_addendum_4_details
*&---------------------------------------------------------------------*
*       Addendum 4 details for ALV Tree
*----------------------------------------------------------------------*
FORM f_addendum_4_details USING p_addendum_0 TYPE ty_addendum_0.

  DATA: lv_card_number TYPE zaccnt_number,
        lv_field1      TYPE seu_text,
        lv_field2      TYPE seu_text,
        lv_field3      TYPE seu_text,
        lv_field4      TYPE seu_text,
        lv_field5      TYPE seu_text,
        lv_tabix       TYPE i.

  DATA: lwa_addendum_4 TYPE ty_addendum_4.

  READ TABLE git_addendum_4 INTO lwa_addendum_4
             WITH KEY accnt_number = p_addendum_0-accnt_number
                      reference = p_addendum_0-ifile_rec_number
                      BINARY SEARCH.

  lv_tabix = sy-tabix.

  WHILE lwa_addendum_4-accnt_number = p_addendum_0-accnt_number AND
        lwa_addendum_4-reference = p_addendum_0-ifile_rec_number AND
        sy-subrc = 0.

* Addendum 4 Header
    IF lv_card_number <> p_addendum_0-accnt_number.
      PERFORM f_construct_tree_data USING 4
                                          20 5 text-018
                                          20 5 text-019
                                          20 5 text-020
                                          20 5 text-021
                                          20 5 text-022
                                          0  5 space
                                          0  5 space
                                          0  5 space
                                          gc_x.

      lv_card_number = p_addendum_0-accnt_number.
    ENDIF.

    lv_field1 = lwa_addendum_4-rental_agr_no.
    lv_field2 = lwa_addendum_4-renter_name.
    lv_field3 = lwa_addendum_4-rtl_ret_city.
    lv_field4 = lwa_addendum_4-rtl_ret_state.
    lv_field5 = lwa_addendum_4-rtl_return_date.

* Addendum 4 Details
    PERFORM f_construct_tree_data USING 4
                                        20 5 lv_field1
                                        20 5 lv_field2
                                        20 5 lv_field3
                                        20 5 lv_field4
                                        20 5 lv_field5
                                        0  5 space
                                        0  5 space
                                        0  5 space
                                        space.

    CLEAR: lv_field1, lv_field2, lv_field3, lv_field4, lv_field5.

    lv_tabix = lv_tabix + 1.

    READ TABLE git_addendum_4 INTO lwa_addendum_4
               INDEX lv_tabix.

  ENDWHILE.

  CLEAR: lv_tabix,
         lv_card_number.

ENDFORM.                    "f_addendum_4_details

*&---------------------------------------------------------------------*
*&      Form  f_addendum_6_details
*&---------------------------------------------------------------------*
*       Addendum 6 details for ALV Tree
*----------------------------------------------------------------------*
FORM f_addendum_6_details USING p_addendum_0 TYPE ty_addendum_0.

  DATA: lv_card_number TYPE zaccnt_number,
        lv_field1      TYPE seu_text,
        lv_field2      TYPE seu_text,
        lv_field3      TYPE seu_text,
        lv_field4      TYPE seu_text,
        lv_field5      TYPE seu_text,
        lv_tabix       TYPE i.

  DATA: lwa_addendum_6 TYPE ty_addendum_6.

  READ TABLE git_addendum_6 INTO lwa_addendum_6
             WITH KEY accnt_number = p_addendum_0-accnt_number
                      reference = p_addendum_0-ifile_rec_number
                      BINARY SEARCH.

  lv_tabix = sy-tabix.

  WHILE lwa_addendum_6-accnt_number = p_addendum_0-accnt_number AND
        lwa_addendum_6-reference = p_addendum_0-ifile_rec_number AND
        sy-subrc = 0.

* Addendum 6 Header
    IF lv_card_number <> p_addendum_0-accnt_number.
      PERFORM f_construct_tree_data USING 4
                                          30 9 text-023
                                          20 9 text-024
                                          20 9 text-025
                                          20 9 text-026
                                          20 9 text-027
                                          0  9 space
                                          0  9 space
                                          0  9 space
                                          gc_x.

      lv_card_number = p_addendum_0-accnt_number.
    ENDIF.

* Addendum 6 Details
    SHIFT lwa_addendum_6-mfuel_unit_price right.
    SHIFT lwa_addendum_6-mfuel_sale_amt right.
    SHIFT lwa_addendum_6-mfuel_qty right.
    lv_field1 = lwa_addendum_6-mchant_st_addr.
    lv_field2 = lwa_addendum_6-mfuel_unit_price.
    lv_field3 = lwa_addendum_6-mfuel_qty.
    lv_field4 = lwa_addendum_6-mfuel_sale_amt.
    lv_field5 = lwa_addendum_6-driver_no.

    PERFORM f_construct_tree_data USING 4
                                        30 9 lv_field1
                                        20 9 lv_field2
                                        20 9 lv_field3
                                        20 9 lv_field4
                                        20 9 lv_field5
                                        0  9 space
                                        0  9 space
                                        0  9 space
                                        space.

    CLEAR: lv_field1, lv_field2, lv_field3, lv_field4, lv_field5.

    lv_tabix = lv_tabix + 1.

    READ TABLE git_addendum_6 INTO lwa_addendum_6
               INDEX lv_tabix.

  ENDWHILE.

  CLEAR: lv_tabix,
         lv_card_number.

ENDFORM.                    "f_addendum_6_details

*&---------------------------------------------------------------------*
*&      Form  f_summary_details
*&---------------------------------------------------------------------*
*       Summary Record
*----------------------------------------------------------------------*
FORM f_summary_details USING p_addendum_0 TYPE ty_addendum_0
                             p_txn_amt    TYPE ztxn_amount.

  DATA: lwa_card_number TYPE ty_card_number.

  DATA: lv_field1 TYPE seu_text,
        lv_field2 TYPE seu_text,
        lv_field3 TYPE seu_text,
        lv_field4 TYPE seu_text.

  CONSTANTS: lc_x(12) TYPE c VALUE 'XXXXXXXXXXXX'.

  git_tree_display_txn[] = git_tree_display_temp[].
  REFRESH git_tree_display_temp.

  lv_field3 = p_addendum_0-accnt_number.
  lv_field3+0(12) = lc_x.
  lv_field4 = p_txn_amt.

  SHIFT lv_field4 LEFT DELETING LEADING space.

* Employee Number & Employee Name
  READ TABLE git_card_number INTO lwa_card_number
             WITH KEY accnt_number = p_addendum_0-accnt_number.
  IF sy-subrc = 0.
    lv_field1 = lwa_card_number-pernr.
    lv_field2 = lwa_card_number-emp_name.
  ENDIF.

* Addendum 0 Summary
  PERFORM f_construct_tree_data USING 2
                                      20 3 lv_field1
                                      30 3 lv_field2
                                      20 3 lv_field3
                                      25 3 lv_field4
                                      0  3 space
                                      0  3 space
                                      0  3 space
                                      0  3 space
                                      space.

  APPEND LINES OF git_tree_display_temp TO git_tree_display.
  APPEND LINES OF git_tree_display_txn  TO git_tree_display.

  REFRESH: git_tree_display_temp,
           git_tree_display_txn.

  CLEAR: lv_field1, lv_field2, lv_field3, lv_field4, lwa_card_number.

ENDFORM.                    "f_summary_details

*&---------------------------------------------------------------------*
*&      Form  f_construct_tree_data
*&---------------------------------------------------------------------*
*       Construct Tree data
*----------------------------------------------------------------------*
FORM f_construct_tree_data USING p_level   TYPE snodetext-tlevel
                                 p_length1 TYPE snodetext-tlength1
                                 p_color1  TYPE snodetext-tcolor1
                                 p_text1   TYPE snodetext-text1
                                 p_length2 TYPE snodetext-tlength2
                                 p_color2  TYPE snodetext-tcolor2
                                 p_text2   TYPE snodetext-text2
                                 p_length3 TYPE snodetext-tlength3
                                 p_color3  TYPE snodetext-tcolor3
                                 p_text3   TYPE snodetext-text3
                                 p_length4 TYPE snodetext-tlength4
                                 p_color4  TYPE snodetext-tcolor4
                                 p_text4   TYPE snodetext-text4
                                 p_length5 TYPE snodetext-tlength5
                                 p_color5  TYPE snodetext-tcolor5
                                 p_text5   TYPE snodetext-text5
                                 p_length6 TYPE snodetext-tlength6
                                 p_color6  TYPE snodetext-tcolor6
                                 p_text6   TYPE snodetext-text6
                                 p_length7 TYPE snodetext-tlength7
                                 p_color7  TYPE snodetext-tcolor7
                                 p_text7   TYPE snodetext-text7
                                 p_length8 TYPE snodetext-tlength8
                                 p_color8  TYPE snodetext-tcolor8
                                 p_text8   TYPE snodetext-text8
                                 p_int     TYPE snodetext-intensiv.

  DATA: lwa_tree_display TYPE snodetext.

  lwa_tree_display-tlevel   = p_level.

  lwa_tree_display-tlength1   = p_length1.
  lwa_tree_display-tcolor1    = p_color1.
  lwa_tree_display-text1      = p_text1.
  lwa_tree_display-tintensiv1 = p_int.

  lwa_tree_display-tlength2   = p_length2.
  lwa_tree_display-tcolor2    = p_color2.
  lwa_tree_display-text2      = p_text2.
  lwa_tree_display-tintensiv2 = p_int.

  lwa_tree_display-tlength3   = p_length3.
  lwa_tree_display-tcolor3    = p_color3.
  lwa_tree_display-text3      = p_text3.
  lwa_tree_display-tintensiv3 = p_int.

  lwa_tree_display-tlength4   = p_length4.
  lwa_tree_display-tcolor4    = p_color4.
  lwa_tree_display-text4      = p_text4.
  lwa_tree_display-tintensiv4 = p_int.

  lwa_tree_display-tlength5   = p_length5.
  lwa_tree_display-tcolor5    = p_color5.
  lwa_tree_display-text5      = p_text5.
  lwa_tree_display-tintensiv5 = p_int.

  lwa_tree_display-tlength6   = p_length6.
  lwa_tree_display-tcolor6    = p_color6.
  lwa_tree_display-text6      = p_text6.
  lwa_tree_display-tintensiv6 = p_int.

  lwa_tree_display-tlength7   = p_length7.
  lwa_tree_display-tcolor7    = p_color7.
  lwa_tree_display-text7      = p_text7.
  lwa_tree_display-tintensiv7 = p_int.

  lwa_tree_display-tlength8   = p_length8.
  lwa_tree_display-tcolor8    = p_color8.
  lwa_tree_display-text8      = p_text8.
  lwa_tree_display-tintensiv8 = p_int.

  APPEND lwa_tree_display TO git_tree_display_temp.
  CLEAR lwa_tree_display.

ENDFORM.                    "f_construct_tree_data

*&---------------------------------------------------------------------*
*&      Form  f_display_alv
*&---------------------------------------------------------------------*
*       Display ALV Tree
*----------------------------------------------------------------------*
FORM f_display_alv.

  CALL FUNCTION 'RS_TREE_CONSTRUCT'
    TABLES
      nodetab            = git_tree_display
    EXCEPTIONS
      tree_failure       = 1
      id_not_found       = 2
      wrong_relationship = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.
    MESSAGE i000(zfi01) WITH text-038.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SET PF-STATUS 'ZSTAT_CARD'.

  CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
    EXPORTING
      callback_program = sy-repid
      status           = gc_own
    EXCEPTIONS
      OTHERS           = 1.

  IF sy-subrc <> 0.
    MESSAGE i000(zfi01) WITH text-038.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    "f_display_alv
*&---------------------------------------------------------------------*
*&      Form  DATA_FETCH_AUTHORITY
*&---------------------------------------------------------------------*
*       Prepare data for authority check
*----------------------------------------------------------------------*
FORM data_fetch_authority .

  REFRESH : git_pa0001_check_expense,
            git_company_code,
            git_cost_center,
            r_company_code_ex,
            r_cost_center_ex.

  CLEAR   : gwa_pa0001_check_expense,
            gwa_company_code,
            gwa_cost_center,
            r_company_code_ex,
            r_cost_center_ex.

  SELECT pernr kostl bukrs werks persg persk vdsk1 sname
    FROM pa0001
    INTO TABLE git_pa0001_check_expense
    WHERE pernr IN s_pernr[] AND
          endda = gc_end_date_check_expense AND
          kostl IN s_kostl[] AND
          bukrs IN s_bukrs[].
  IF sy-subrc = 0.

    LOOP AT git_pa0001_check_expense INTO gwa_pa0001_check_expense.

*      Preparing company code which will be checked further for authorization.
      IF gwa_pa0001_check_expense-bukrs IS NOT INITIAL.

        gwa_company_code-pernr = gwa_pa0001_check_expense-pernr.
        gwa_company_code-bukrs = gwa_pa0001_check_expense-bukrs.
        APPEND gwa_company_code TO git_company_code.
        CLEAR: gwa_company_code.
      ENDIF.

*     Preparing cost center which will be checked further for authorization.
      IF gwa_pa0001_check_expense-kostl IS NOT INITIAL.

        gwa_cost_center-pernr = gwa_pa0001_check_expense-pernr.
        gwa_cost_center-kostl = gwa_pa0001_check_expense-kostl.
        APPEND gwa_cost_center TO git_cost_center.
        CLEAR: gwa_cost_center.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF git_company_code[] IS NOT INITIAL.
    SORT git_company_code BY pernr bukrs.
    DELETE ADJACENT DUPLICATES FROM git_company_code COMPARING ALL FIELDS.
  ENDIF.

  IF git_cost_center[] IS NOT INITIAL.
    SORT git_cost_center BY pernr kostl.
    DELETE ADJACENT DUPLICATES FROM git_cost_center COMPARING ALL FIELDS.
  ENDIF.

ENDFORM.                    " DATA_FETCH_AUTHORITY
*&---------------------------------------------------------------------*
*&      Form  COMPANY_CODE_RANGE_EX
*&---------------------------------------------------------------------*
*  Prepare Company Code which needs to be removed from report display
*----------------------------------------------------------------------*
FORM company_code_range_ex .

* Checking company code for authorization.
  IF git_company_code[] IS NOT INITIAL.
    LOOP AT git_company_code INTO gwa_company_code.

      CLEAR: gv_authp_check_expense.
      IF gwa_company_code-pernr IS NOT INITIAL.
        CALL FUNCTION 'PTRV_DETERMINE_AUTHP'
          EXPORTING
            current_pernr = gwa_company_code-pernr
          IMPORTING
            authp         = gv_authp_check_expense.
      ENDIF.

*       Passing Company Code and checking if it has authorization.
      IF gwa_company_code-bukrs IS NOT INITIAL AND gv_authp_check_expense IS NOT INITIAL.

        AUTHORITY-CHECK OBJECT 'P_TRAVL'
                  ID 'AUTHP' FIELD gv_authp_check_expense
                  ID 'BUKRS' FIELD gwa_company_code-bukrs
                  ID 'PERSA' DUMMY
                  ID 'KOSTL' DUMMY
                  ID 'PERSG' DUMMY
                  ID 'PERSK' DUMMY
                  ID 'VDSK1' DUMMY
                  ID 'PTZUO' DUMMY
                  ID 'AUTHF' FIELD 'R'
                  ID 'AUTHS' DUMMY.

        IF sy-subrc NE 0.
          r_company_code_ex-low     = gwa_company_code-bukrs.
          r_company_code_ex-option  = 'EQ'.
          r_company_code_ex-sign    = 'I'.
          APPEND r_company_code_ex.
          CLEAR:r_company_code_ex.
        ENDIF.
      ELSEIF gwa_company_code-bukrs IS NOT INITIAL AND gv_authp_check_expense IS INITIAL.

        AUTHORITY-CHECK OBJECT 'P_TRAVL'
                  ID 'AUTHP' DUMMY
                  ID 'BUKRS' FIELD gwa_company_code-bukrs
                  ID 'PERSA' DUMMY
                  ID 'KOSTL' DUMMY
                  ID 'PERSG' DUMMY
                  ID 'PERSK' DUMMY
                  ID 'VDSK1' DUMMY
                  ID 'PTZUO' DUMMY
                  ID 'AUTHF' FIELD 'R'
                  ID 'AUTHS' DUMMY.

        IF sy-subrc NE 0.
          r_company_code_ex-low     = gwa_company_code-bukrs.
          r_company_code_ex-option  = 'EQ'.
          r_company_code_ex-sign    = 'I'.
          APPEND r_company_code_ex.
          CLEAR:r_company_code_ex.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " COMPANY_CODE_RANGE_EX
*&---------------------------------------------------------------------*
*&      Form  COST_CENTER_RANGE_EX
*&---------------------------------------------------------------------*
*  Prepare Cost Center which needs to be removed from report display
*----------------------------------------------------------------------*
FORM cost_center_range_ex .

* Checking Cost Center for authorization.
  IF git_cost_center[] IS NOT INITIAL.
    LOOP AT git_cost_center INTO gwa_cost_center.

      CLEAR: gv_authp_check_expense.
      IF gwa_cost_center-pernr IS NOT INITIAL.
        CALL FUNCTION 'PTRV_DETERMINE_AUTHP'
          EXPORTING
            current_pernr = gwa_cost_center-pernr
          IMPORTING
            authp         = gv_authp_check_expense.
      ENDIF.

*       Passing Cost Center and checking if it has authorization.
      IF gwa_cost_center-kostl IS NOT INITIAL AND gv_authp_check_expense IS NOT INITIAL.

        AUTHORITY-CHECK OBJECT 'P_TRAVL'
                  ID 'AUTHP' FIELD gv_authp_check_expense
                  ID 'BUKRS' DUMMY
                  ID 'PERSA' DUMMY
                  ID 'KOSTL' FIELD gwa_cost_center-kostl
                  ID 'PERSG' DUMMY
                  ID 'PERSK' DUMMY
                  ID 'VDSK1' DUMMY
                  ID 'PTZUO' DUMMY
                  ID 'AUTHF' FIELD 'R'
                  ID 'AUTHS' DUMMY.

        IF sy-subrc NE 0.
          r_cost_center_ex-low     = gwa_cost_center-kostl.
          r_cost_center_ex-option  = 'EQ'.
          r_cost_center_ex-sign    = 'I'.
          APPEND r_cost_center_ex.
          CLEAR: r_cost_center_ex.
        ENDIF.
      ELSEIF gwa_cost_center-kostl IS NOT INITIAL AND gv_authp_check_expense IS INITIAL.

        AUTHORITY-CHECK OBJECT 'P_TRAVL'
                  ID 'AUTHP' DUMMY
                  ID 'BUKRS' DUMMY
                  ID 'PERSA' DUMMY
                  ID 'KOSTL' FIELD gwa_cost_center-kostl
                  ID 'PERSG' DUMMY
                  ID 'PERSK' DUMMY
                  ID 'VDSK1' DUMMY
                  ID 'PTZUO' DUMMY
                  ID 'AUTHF' FIELD 'R'
                  ID 'AUTHS' DUMMY.

        IF sy-subrc NE 0.
          r_cost_center_ex-low     = gwa_cost_center-kostl.
          r_cost_center_ex-option  = 'EQ'.
          r_cost_center_ex-sign    = 'I'.
          APPEND r_cost_center_ex.
          CLEAR: r_cost_center_ex.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " COST_CENTER_RANGE_EX
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_ERROR_MESSAGE
*&---------------------------------------------------------------------*
*    Display error message after authorization check
*----------------------------------------------------------------------*
FORM authority_error_message .

* Delete duplicate entries
  SORT r_company_code_ex.
  DELETE ADJACENT DUPLICATES FROM r_company_code_ex.

* Delete duplicate entries
  SORT r_cost_center_ex.
  DELETE ADJACENT DUPLICATES FROM r_cost_center_ex.

* Delete Company Code entries which do not have authorization
  IF r_company_code_ex[] IS NOT INITIAL.
    DELETE git_pa0001_check_expense WHERE bukrs IN r_company_code_ex.
  ENDIF.

* Delete Cost Center entries which do not have authorization
  IF r_cost_center_ex[] IS NOT INITIAL.
    DELETE git_pa0001_check_expense WHERE kostl IN r_cost_center_ex.
  ENDIF.

* Issue error message if Company Code has no authorization and there is no value to display
  IF r_company_code_ex[] IS NOT INITIAL AND git_pa0001_check_expense[] IS INITIAL.
    MESSAGE i000(zfi01) WITH text-043.
    LEAVE LIST-PROCESSING.
  ENDIF.

* Issue error message if Cost Center has no authorization and there is no value to display
  IF r_cost_center_ex[] IS NOT INITIAL AND git_pa0001_check_expense[] IS INITIAL.
    MESSAGE i000(zfi01) WITH text-043.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " AUTHORITY_ERROR_MESSAGE
