*&---------------------------------------------------------------------*
*& Report  Z_CLEAN_ZFTV_ADDENDUM_0
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_CLEAN_ZFTV_ADDENDUM_0.

Data: T_DATA type TABLE OF zftv_addendum_0 with HEADER LINE.
Data: w_lines type i,
      w_char type i,
      w_ind type i,
      w_index type i,
      w_num type char16.

SELECT-OPTIONS: s_date for sy-datum.


START-OF-SELECTION.

  select * from zftv_addendum_0 into table T_Data
    where   TXN_AMOUNT LIKE '%,%' AND
            TXN_DATE in S_DATE.

End-OF-SELECTION.

  DESCRIBE TABLE T_DATA lines w_lines.

  Loop At T_DATA.
    w_index = sy-tabix.
    Perform convert_number changing T_data-txn_amount.
    Perform convert_number changing T_data-amount_org.
    Perform convert_number changing T_data-st_amount.
    Perform convert_number changing T_data-freight_amount.
    Perform convert_number changing T_data-duty_amt.
    Perform convert_number changing T_data-alt_tax_amt.
    modify T_DATA INDEX w_index.
  ENDLOOP.

  UPDATE zftv_addendum_0 FROM TABLE T_DATA.

  write:/ w_lines, 'Records Processed'.
*&---------------------------------------------------------------------*
*&      Form  CONVERT_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_DATA_TXN_AMOUNT  text
*----------------------------------------------------------------------*
FORM CONVERT_NUMBER  CHANGING P_AMOUNT.

  w_char = strlen( P_Amount ).
  clear w_num.
  w_ind = 0.
  Do w_char times.
    IF p_amount+w_ind(1) = ','.
      w_ind = w_ind + 1.
      continue.
    ENDIF.
    concatenate w_num p_amount+w_ind(1) into w_num.
    CONDENSE w_num NO-GAPS.
    w_ind = w_ind + 1.
  ENDDO.

  P_amount = w_num.

ENDFORM.
