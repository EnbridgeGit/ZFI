*&---------------------------------------------------------------------*
*&  Include           ZFI_DELETED_PARKED_TOP
*&---------------------------------------------------------------------*
TABLES: bkpf,t001,t003   ##NEEDED.
TYPES: BEGIN OF gty_bkpf,
  bukrs TYPE bukrs,  "Company Code
  belnr TYPE belnr_d,"Document Number
  gjahr TYPE gjahr,  "fiscal year
  blart TYPE blart,  "Document Type
  cpudt TYPE cpudt,  "Entry Date
  awtyp TYPE awtyp,  "Ref Transaction
  awkey TYPE awkey,  "Reference Key
  END OF gty_bkpf.

TYPES : BEGIN OF gty_validation,
  bukrs TYPE bukrs,
  belnr TYPE belnr_d,
  gjahr TYPE gjahr,
  blart TYPE blart,
  END OF gty_validation.

"work area declaration
DATA:gs_bkpf        TYPE gty_bkpf,
     gs_validation  TYPE gty_validation ##NEEDED,
     gs_cdhdr       TYPE cdhdr.

"internal tables declaration
DATA: gt_bkpf       TYPE STANDARD TABLE OF gty_bkpf       INITIAL SIZE 0,
      gt_validation TYPE STANDARD TABLE OF gty_validation INITIAL SIZE 0,
      gt_cdhdr      TYPE STANDARD TABLE OF cdhdr.

DATA: go_send_request    TYPE REF TO cl_bcs,
      go_document        TYPE REF TO cl_document_bcs,
      go_recipient       TYPE REF TO if_recipient_bcs,
      go_bcs_exception   TYPE REF TO cx_bcs,
      gv_main_text       TYPE bcsy_text,
      gv_binary_content  TYPE solix_tab,
      gv_size            TYPE so_obj_len,
      gv_sent_to_all     TYPE os_boolean,
      gv_email           TYPE ad_smtpadr,
      gv_address         TYPE char260,
      gv_string          TYPE string,
      gv_string_1        TYPE char260,
      gv_bukrs           TYPE bsik-bukrs,
      gv_low_date        TYPE /bev3/ch07t20,
      gv_high_date       TYPE /bev3/ch07t20,
      gv_entry_date      TYPE /bev3/ch07t20,
      gv_system_name     TYPE /bev3/ch07t20,
      gv_attachment_name TYPE sood-objdes,
      gv_parked_date     TYPE /bev3/ch07t20,
      gv_parked_time     TYPE /bev3/ch07t20,
      gv_objectclass     TYPE c LENGTH 15,
      gv_objectid        TYPE cdhdr-objectid,
      gv_bukrs1          TYPE string.


CONSTANTS:
  gc_tab   TYPE c VALUE cl_bcs_convert=>gc_tab,
  gc_crlf  TYPE c VALUE cl_bcs_convert=>gc_crlf,
  gc_bstat TYPE c VALUE 'Z',
  gc_slash TYPE c VALUE '/',
  gc_colon TYPE c VALUE ':',
  gc_dot   TYPE c VALUE '.'.
