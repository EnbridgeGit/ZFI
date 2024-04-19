*&---------------------------------------------------------------------*
*&  Include           ZFINSYNC_SCHEDULE_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFINSYNC_SCHEDULE                             *
* Include            :   ZFINSYNC_SCHEDULE_TOP                         *
* Author             :   AKMADASU                                      *
* Date               :   Jan 23, 2022                                  *
* Technical Contact  :   Ashok Madasu                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  To Schedule Finance Sync IDOC Process, PGI AMD *
*                    :  Billing Jobs paralelly in BACKGROUND           *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 23-Jan-2022  AKMADASU    D30K931986-Initial development              *
*----------------------------------------------------------------------*

* table declarations needed for the selection screens
TABLES: edidc, tbd55, serial, bdfields.

*--Types declarations
TYPES:BEGIN OF ty_edidc,
        docnum TYPE edi_docnum,
      END OF ty_edidc.
DATA:it_edidc   TYPE TABLE OF ty_edidc,
     it_edidc_t TYPE TABLE OF ty_edidc,
     wa_edidc   TYPE ty_edidc,
     lr_docnum  TYPE TABLE OF bdrg_doc,
     wa_docnum  TYPE bdrg_doc.
DATA:lv_lines  TYPE i,
     lv_packet1 type i,
     lv_jobs   TYPE i,
     lv_packet TYPE sytabix,
     lv_from   TYPE sytabix VALUE 1,
     lv_to     TYPE sytabix.
DATA: lv_job_name         TYPE tbtcjob-jobname,  " Job Name
      lv_job_number       TYPE tbtcjob-jobcount, " Job Count
      lv_msg              TYPE string,
      lv_count            TYPE buzei,
      lv_count1           TYPE CHAR30,
      lv_job_read_jobhead TYPE  tbtcjob,
      lv_tpackt           TYPE sytabix,
      lv_job_closed       TYPE flag,
      v_totjob(2)      TYPE n,
      v_host           TYPE mshost2,
      v_server_name    TYPE msname2,
      v_total_btc      TYPE i,
      v_free_btc       TYPE i,
      lv_low type char50,
      lv_high type char50,
      v_percentage     TYPE p DECIMALS 4,
      v_serv_per       TYPE salv_de_selopt_low,
      v_log_del_date   TYPE salv_de_selopt_low.
DATA : lv_total_btc  TYPE i,
        lv_free_btc   TYPE i,
        lv_percentage TYPE p DECIMALS 4.
TYPES: BEGIN OF lty_job_info,
         job_name      TYPE tbtcjob-jobname,  " Job Name
         job_number    TYPE tbtcjob-jobcount, " Job Count
         finished_flag TYPE flag,             " Finished flag
       END OF lty_job_info.
DATA:lt_job_info       TYPE TABLE OF lty_job_info,
     lst_job_info      TYPE lty_job_info.
FIELD-SYMBOLS: <lfs_job_info>  TYPE lty_job_info.

DATA: BEGIN OF sys_tabl OCCURS 50.
        INCLUDE STRUCTURE msxxlist.
DATA: END OF sys_tabl.

DATA: lt_instances  TYPE TABLE OF rzlliapsrv,
      lst_instances TYPE rzlliapsrv,
      lv_msg_text   TYPE string,
      i_server_tb   TYPE TABLE OF msxxlist,
      lst_server_tb TYPE msxxlist.
CONSTANTS: lc_job    TYPE btcjob VALUE 'BSMS9999DP'.
