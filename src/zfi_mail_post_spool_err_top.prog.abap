*&---------------------------------------------------------------------*
*&  Include           ZFI_MAIL_POST_SPOOL_ERR_TOP
*&---------------------------------------------------------------------*
*& Report Name          : ZFI_MAIL_POST_SPOOL_ERR                        *
*& Author               : KMB                                            *
*& Creation Date        : 30-Jan-2019                                    *
*& Object ID            : CHG0136225                                     *
*& Application Area     : FI                                             *
*& Description          : Mail Posting spool errors                      *
*&-----------------------------------------------------------------------*
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  -------------------------------   *
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 17-07-2019   KMB         D30K930039  CHG0152154 Issue with the program *
**************************************************************************

"Types
TYPES : BEGIN OF lty_tab,
          line(256),
        END OF lty_tab.

TYPES : BEGIN OF lty_tbtcp,
          listident TYPE tbtcp-listident,
          sdldate   TYPE tbtcp-sdldate,
        END OF lty_tbtcp.
TYPES : BEGIN OF lty_pa0105,
          begda      TYPE pa0105-begda,
          usrid_long TYPE pa0105-usrid_long,
        END OF lty_pa0105.

*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
TYPES : BEGIN OF lty_trip,
          lv_awref TYPE ptrv_doc_it-awref,
          lv_pernr TYPE ptrv_doc_it-pernr,
          lv_kostl TYPE ptrv_doc_it-kostl,
          lv_exbel TYPE ptrv_doc_it-exbel,
          lv_bukrs TYPE ptrv_doc_it-bukrs,
        END OF lty_trip.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019

"Local variables
DATA: lv_spool       TYPE tsp01-rqident,
      lv_line        TYPE string,
      lv_cc          TYPE bukrs,
      lv_cost        TYPE kostl,
      lv_value       TYPE string,
      lv_doc_type    TYPE soodk-objtp,
      lv_date        TYPE tbtcp-sdldate,
      lv_index       TYPE n LENGTH 10,
      lv_awref       TYPE ptrv_doc_it-awref,
      lv_string      TYPE c,
      lv_string1     TYPE c,
      lv_string2(10) TYPE c,
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
      lv_mail        TYPE comm_id_long,
      lv_addr        TYPE sza5_d0700-smtp_addr,
      lv_fiscal      TYPE c LENGTH 4,
      lv_index1      TYPE n LENGTH 10,
      lv_flag        TYPE c,
      lv_lines       TYPE n LENGTH 10.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019

"Local internal tables and Work area
DATA: lt_tab             TYPE TABLE OF lty_tab,
      ls_tab             TYPE lty_tab,
      ls_tab1            TYPE lty_tab,
      lv_str1            TYPE string,
      lv_str2            TYPE string,
      lr_str             TYPE RANGE OF string,
      lr_str1            TYPE RANGE OF string,
      ls_str             LIKE LINE OF lr_str,
      ls_str1            LIKE LINE OF lr_str,
      lt_tbtcp           TYPE TABLE OF lty_tbtcp,
      ls_tbtcp           TYPE lty_tbtcp,
      ls_ptrv_doc_it     TYPE ptrv_doc_it,
      lt_pa0105          TYPE TABLE OF lty_pa0105,
      ls_pa0105          TYPE lty_pa0105,
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
      lt_trip            TYPE TABLE OF lty_trip,
      ls_trip            TYPE lty_trip,
      ls_trip1           TYPE lty_trip,
      lt_mailsubject     TYPE sodocchgi1,
      lt_mailrecipients  TYPE STANDARD TABLE OF somlrec90,
      ls_mailrecipients  TYPE somlrec90,
      lt_mailtxt         TYPE STANDARD TABLE OF soli,
      ls_mailtxt         TYPE soli,
      lt_collect         TYPE STANDARD TABLE OF soli.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019

"Constants
CONSTANTS : lc_u    TYPE c VALUE 'U',
            lc_x    TYPE c VALUE 'X',
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            lc_i    TYPE c VALUE 'I',
            lc_10   TYPE subty VALUE '0010',
            lc_cp   TYPE c LENGTH 2 VALUE 'CP',
            lc_s    TYPE c VALUE '*'.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019

"Selection-Screen
SELECTION-SCREEN : BEGIN OF BLOCK b1.
PARAMETERS : p_job TYPE btcjob DEFAULT 'BSMS0464WP'.
SELECT-OPTIONS : s_email FOR  lv_addr NO INTERVALS OBLIGATORY. "Added by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
SELECTION-SCREEN :END OF BLOCK b1.
