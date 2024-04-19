*&---------------------------------------------------------------------*
*&  Include           ZFI_UPDATE_IT0105_TOP
*&---------------------------------------------------------------------*


TYPES :
  BEGIN OF ty_field,
    company_number(15),
    card_create_date(16),
    card_profile_name(50),
    card_account_number TYPE sysid,
    payee_id            TYPE pernr-pernr,
    card_expire_dat(16),
    first_name(60),
    last_name(60),
    status(45),
 END OF ty_field.

*Structure for error message
TYPES : BEGIN OF ty_s_error,
          msg_err(60) TYPE c,
        END OF ty_s_error.

*Internal table decleration
DATA:
   lt_field      TYPE STANDARD TABLE OF ty_field,
   lt_alv        TYPE STANDARD TABLE OF ty_field,
   ls_field      TYPE ty_field,
   lt_p0105      TYPE STANDARD TABLE OF p0105,
   ls_p0105      TYPE p0105,
   lt_bdcdata    LIKE TABLE OF bdcdata.

DATA:
  ls_bdcdata LIKE LINE OF lt_bdcdata,   " Structure type of bdcdata
  ls_str     TYPE string.
* Data decleration
DATA:
  wa_path    TYPE string ,
  lv_error   TYPE string,
  wa_cnt     TYPE i,
  lv_mode    TYPE c,
  wa_cnt1(2) TYPE n,
  lt_output  TYPE TABLE OF ty_s_error,
  ls_output  TYPE ty_s_error,
  lv_endda1  TYPE datum,
  lv_endda(10),
  lv_flag    TYPE c,
  lv_date1   TYPE datum,
  lv_begda(10),

  lv_check   LIKE bapireturn,
  lv_subrc   TYPE sy-subrc.

 DATA: lt_text_data       TYPE truxs_t_text_data,
       lv_filename_string TYPE string.
