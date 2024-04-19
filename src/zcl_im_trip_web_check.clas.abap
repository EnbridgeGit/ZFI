class ZCL_IM_TRIP_WEB_CHECK definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_TRIP_WEB_CHECK
*"* do not include other source files here!!!

  interfaces IF_EX_TRIP_WEB_CHECK .
protected section.
*"* protected components of class ZCL_IM_TRIP_WEB_CHECK
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_TRIP_WEB_CHECK
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_TRIP_WEB_CHECK IMPLEMENTATION.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_ADVANCES.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_CHANGES.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_DEDUCTIONS.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_GENERAL_DATA.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_ITINERARY.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_ITIN_COSTS_SPLIT.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_ADVANCES.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_CCC_RECEIPT.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_DEDUCTIONS.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_ITINERARY.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_MILEAGE.
endmethod.


METHOD if_ex_trip_web_check~user_check_line_of_receipts.
  DATA: ls_bapireturn LIKE LINE OF return,
        lv_messagev1  TYPE symsgv,
        lv_messagev2  TYPE symsgv,
        lv_firma      TYPE t706b1-firma,
        lv_sptxt      TYPE t706b5-sptxt.

  "Check for Expense Type not selected
  IF receipt-spkzl = 'Z000'.
    CLEAR ls_bapireturn.
    lv_messagev1 = receipt-belnr.
    ls_bapireturn-type = 'E'.
    ls_bapireturn-id = 'ZFITA_MSG'.
    ls_bapireturn-number = 020.
    ls_bapireturn-message_v1 = lv_messagev1.
    APPEND ls_bapireturn TO return.
  ENDIF.

  "Force OOP Expense Type on OOP Line ITem
  IF receipt-ccard IS INITIAL.
    SELECT SINGLE firma
      FROM t706b1
      INTO lv_firma
      WHERE morei = 'UG'
        AND spkzl = receipt-spkzl.



    "If payed by company give error
    IF lv_firma = 'X'.
      SELECT SINGLE sptxt
        FROM t706b5
        INTO lv_sptxt
        WHERE morei = 'UG'
          AND spkzl = receipt-spkzl.

      CLEAR ls_bapireturn.
      lv_messagev1 = lv_sptxt.
      lv_messagev2 = receipt-belnr.
      ls_bapireturn-type = 'E'.
      ls_bapireturn-id = 'ZFITA_MSG'.
      ls_bapireturn-number = 011.
      ls_bapireturn-message_v1 = lv_messagev1.
      ls_bapireturn-message_v2 = lv_messagev2.
      APPEND ls_bapireturn TO return.
    ENDIF.
  ENDIF.

  "Check for 0 dollar line items
  IF receipt-betrg = 0.
    CLEAR ls_bapireturn.
    lv_messagev1 = receipt-belnr.
    ls_bapireturn-type = 'E'.
    ls_bapireturn-id = 'ZFITA_MSG'.
    ls_bapireturn-number = 010.
    ls_bapireturn-message_v1 = lv_messagev1.
    APPEND ls_bapireturn TO return.
  ENDIF.

ENDMETHOD.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_LINE_OF_TRANSPORT.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_MILEAGE.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_MILE_COSTS_SPLIT.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_RECEIPTS.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_RECE_COSTS_SPLIT.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TEXT.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TRANSPORTS.
endmethod.


method IF_EX_TRIP_WEB_CHECK~USER_CHECK_TRIP_COSTS_SPLIT.
endmethod.
ENDCLASS.
