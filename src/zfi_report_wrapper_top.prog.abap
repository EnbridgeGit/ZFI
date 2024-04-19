*&---------------------------------------------------------------------*
*& Report  ZFI_REPORT_WRAPPER_TOP                                      *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_REPORT_WRAPPER                            *
*& Include Name       :  ZFI_REPORT_WRAPPER_TOP                        *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  19-Oct-2020                                   *
*& Change Request     :  CHG0191644                                    *
*& Purpose            :  Wrapper program to send FBL1N output in CSV   *
*&                       format as an email attachment                 *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 19-Oct 2020  AHMADT        CHG0191644 D30K930705 Initial Development*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*& --------------------------------------------------------------------*
*&    Global Types                                                     *
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gty_final,
       cleared_items   TYPE ico_augp,
       cocd            TYPE bukrs,
       vendor          TYPE lifnr,
       payee           TYPE empfb,
       reference       TYPE xblnr,
       documentno      TYPE char10,
       type            TYPE char8,
       doc_date        TYPE char10,
       gl_indicator    TYPE umskz,
       due_date        TYPE ico_due,
       pmnt_block      TYPE char10,
       pay_method      TYPE char10,
       bnkt            TYPE bvtyp,
       amount          TYPE char20,
       curr            TYPE char10,
       payt            TYPE char4,
       net_due_dt      TYPE char10,
       clearing        TYPE char10,
       clear_doc       TYPE augbl,
       text            TYPE sgtxt,
       ref_key_3       TYPE xref3,
       check_no        TYPE char15,
       pmt_mth_su      TYPE char8,
       doc_head_text   TYPE bktxt,
       disc_base_amt   TYPE char20,
  END OF gty_final.

*& --------------------------------------------------------------------*
*&    Global Internal Tables                                           *
*&---------------------------------------------------------------------*


DATA: gt_tab      TYPE TABLE OF  abaplist,
      gt_str_tab  TYPE STANDARD TABLE OF string,
      gt_final    TYPE STANDARD TABLE OF gty_final,
      gt_list     TYPE list_string_table.

*& --------------------------------------------------------------------*
*&    Global Work Areas                                                *
*&---------------------------------------------------------------------*

DATA: gs_list     TYPE LINE OF list_string_table,
      gs_final    TYPE gty_final,
      gs_str_tab  TYPE string.

*& --------------------------------------------------------------------*
*&    Global Variables                                                 *
*&---------------------------------------------------------------------*

DATA: gv_string          TYPE string,
      gv_binary_content  TYPE solix_tab,
      gv_size            TYPE so_obj_len,
      gv_main_text       TYPE bcsy_text,
      gv_attachment_name TYPE sood-objdes,
      gv_address         TYPE char260,
      gv_email           TYPE ad_smtpadr,
      gv_sent_to_all     TYPE os_boolean,
      gv_amount          TYPE p LENGTH 12 DECIMALS 3,
      gv_disc            TYPE p LENGTH 12 DECIMALS 3.

*& --------------------------------------------------------------------*
*&    Global Objects                                                   *
*&---------------------------------------------------------------------*

DATA: go_send_request    TYPE REF TO cl_bcs,
      go_document        TYPE REF TO cl_document_bcs,
      go_recipient       TYPE REF TO if_recipient_bcs,
      go_bcs_exception   TYPE REF TO cx_bcs.

*& --------------------------------------------------------------------*
*&    Global Constants                                                 *
*&---------------------------------------------------------------------*
CONSTANTS:   gc_crlf  TYPE c VALUE cl_bcs_convert=>gc_crlf,
             gc_tab   TYPE c VALUE cl_bcs_convert=>gc_tab.
