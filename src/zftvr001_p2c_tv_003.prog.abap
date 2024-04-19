*&---------------------------------------------------------------------*
*& Report  ZFTVR001_P2C_TV_003
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFTVR001_P2C_TV_003.
*----------------------------------------------------------------------*
* Include - Data Declarations                                          *
*----------------------------------------------------------------------*

INCLUDE ZFTVR001_credit_card_top.

*----------------------------------------------------------------------*
* Selection Screen.                                                    *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_pernr FOR pa0001-pernr,
                s_bukrs FOR pa0001-bukrs,
                s_kostl FOR pa0001-kostl,
                s_tdate FOR sy-datum.

SELECTION-SCREEN END OF BLOCK blk1.

*----------------------------------------------------------------------*
* At Selection Screen                                                  *
*----------------------------------------------------------------------*

AT SELECTION-SCREEN.

* Validate Personnel Number
  PERFORM f_validate_pernr.

* Validate Company Code
  PERFORM f_validate_bukrs.

* Validate Cost Center
  PERFORM f_validate_kostl.

*----------------------------------------------------------------------*
* Start-of-Selection                                                   *
*----------------------------------------------------------------------*

START-OF-SELECTION.

* Fetching data for authorization
  PERFORM data_fetch_authority.

* Prepare Company Code which needs to be removed from report display
  PERFORM company_code_range_ex.

* Prepare Cost Center which needs to be removed from report display
  PERFORM cost_center_range_ex.

* Display error message after authorization check
  PERFORM authority_error_message.

* Get Employee Number and Employee Name from PA0001
  PERFORM f_select_pa0001.

* Get Employee Credit Card Details from PA0105/0011
  PERFORM f_select_pa0105.

* Get Financial Transaction Records
  PERFORM f_select_addendum_0.

* Get Purchasing Card Addendum Records
  PERFORM f_select_addendum_1.

* Get Passenger Transport Addendum Records
  PERFORM f_select_addendum_2.

* Get Passenger Transport Leg Addendum Records
  PERFORM f_select_addendum_21.

* Get Lodging Addendum Records
  PERFORM f_select_addendum_3.

* Get Car Rental Addendum Records
  PERFORM f_select_addendum_4.

* Get Fleet Card Addendum Records
  PERFORM f_select_addendum_6.

* Build ALV Tree
  PERFORM f_build_tree_data.

*----------------------------------------------------------------------*
* End-of-Selection                                                     *
*----------------------------------------------------------------------*

END-OF-SELECTION.

* Display ALV Tree
  PERFORM f_display_alv.

*----------------------------------------------------------------------*
* Include - Subroutines                                                *
*----------------------------------------------------------------------*

  INCLUDE zFTV001_credit_card_f.
