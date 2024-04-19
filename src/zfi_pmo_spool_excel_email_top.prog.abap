*&---------------------------------------------------------------------*
*&  Include           ZFI_PMO_SPOOL_EXCEL_EMAIL_TOP
*&---------------------------------------------------------------------*
*&-----------------------------------------------------------------------*
*& Report Name          : ZFI_PMO_SPOOL_EXCEL_EMAIL_TOP                  *
*& Author               : KMB                                            *
*& Creation Date        : 23-May-2019                                    *
*& Transport no.        : D30K929860                                     *
*& Object ID            : CHG0147746                                     *
*& Application Area     : FI                                             *
*& Description          : Mail excel with job spool data                 *
*&-----------------------------------------------------------------------*
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 07-06-2019   KMB         D30K929919  CHG0147746 ENHC0025526 Excl       *
*                                      attachement with spool 7.6.2019   *
**************************************************************************
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 24-06-2019   KMB         D30K929963  CHG0147746 ENHC0025526 Excl       *
*                                      attachement with spool 24.6.2019  *
**************************************************************************
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 15-07-2019   KMB         D30K930029  CHG0153285 DFCT0017463 multiple   *
*                                      email in selection screen 15.7.19 *
**************************************************************************

"Types
TYPES : BEGIN OF gty_tab,
        line(256),
        END OF gty_tab.

"global variables
DATA : gv_listident TYPE tbtcp-listident,
       gv_spool TYPE tsp01-rqident,
       gv_doc_type TYPE soodk-objtp, "##NEEDED Message Code DAT 0908
       gv_count TYPE n,
       gv_badpattern TYPE n.

DATA: gv_attachment_name TYPE sood-objdes,
      gv_string TYPE string,
      gv_size           TYPE so_obj_len,
      gv_sent_to_all    TYPE os_boolean,
      gv_email       TYPE ad_smtpadr,
      gv_string_n     TYPE string,
      gv_string_n1     TYPE so_obj_des.

"Global internal tables and Work area
DATA: gt_tab TYPE TABLE OF gty_tab,
      gs_tab TYPE gty_tab,
      gt_str  TYPE RANGE OF string,
      gs_str  LIKE LINE OF gt_str,
      gt_itab(50) OCCURS 0 WITH HEADER LINE,
      gt_itab2(50) OCCURS 0 WITH HEADER LINE,
      gs_itab LIKE gt_itab,
      gs_tab1 TYPE gty_tab,
      gv_index TYPE n,
      gv_addr TYPE SZA5_D0700-SMTP_ADDR."Added by KMB on 15.7.2019 CHG0153285 Date field in selection screen

CONSTANTS:
  gc_tab  TYPE c VALUE cl_bcs_convert=>gc_tab,
  gc_crlf TYPE c VALUE cl_bcs_convert=>gc_crlf.


DATA: gcl_send_request   TYPE REF TO cl_bcs,
      gcl_document       TYPE REF TO cl_document_bcs,
      gif_recipient      TYPE REF TO if_recipient_bcs,
      gex_bcs_exception  TYPE REF TO cx_bcs,
      gt_main_text      TYPE bcsy_text,
      gt_binary_content TYPE solix_tab.

"Field symbols
FIELD-SYMBOLS : <fs_itab> LIKE gt_itab.

"Constants
CONSTANTS : gc_raw TYPE c LENGTH 3 VALUE 'RAW',
            gc_i   TYPE c VALUE 'I',
            gc_e   TYPE c VALUE 'E',
            gc_un  TYPE c VALUE '_',
            gc_cp  TYPE c LENGTH 2 VALUE 'CP',
            gc_st  TYPE c LENGTH 2 VALUE ' *',
            gc_ln  TYPE c LENGTH 16 VALUE '---------------*',
            gc_ln1 TYPE c LENGTH 10 VALUE '|--------*',
            gc_doc TYPE c LENGTH 13 VALUE '|*Document*', "'|DocumentNo|*' Changed by KMB on 24.6.2019 CHG0147746
            gc_ln2 TYPE c VALUE '|'.
