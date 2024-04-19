*&---------------------------------------------------------------------*
*&  Include           ZFPSI911_PROJECT_SUM_DATA_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFPSI911_PROJECT_SUM_DATA_TOP                 *
*& Author             :  Kavya M B                                     *
*& Creation Date      :  13/09/2019                                    *
*& Object ID          :  I_P2C_PS_911                                  *
*& Application Area   :  FICO                                          *
*& Description        :  This interface is to send Project System      *
*                        master data and transactions data in a flat   *
*                        file to a dedicated location on Windows that  *
*                        to be picked up by Workato and delivered to   *
*                        destination system with SQL database.         *
*&-------------------------------------------------------------------- *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 20-11-2019   KMB         D30K930293  CHG0159956 - FUT issue          *
* 29-11-2019   KMB         D30K930315  CHG0159956 - FUT issue          *
************************************************************************


TABLES: proj, prps, rpsco.


"Type declarations
TYPES: BEGIN OF lty_rpsco,
       objnr      TYPE rpsco-objnr,
       wrttp      TYPE rpsco-wrttp,
       gjahr      TYPE rpsco-gjahr,
       versn      TYPE rpsco-versn,
       twaer      TYPE rpsco-twaer,
       wlp00      TYPE rpsco-wlp00,
       wlp01      TYPE rpsco-wlp01,
       wlp02      TYPE rpsco-wlp02,
       wlp03      TYPE rpsco-wlp03,
       wlp04      TYPE rpsco-wlp04,
       wlp05      TYPE rpsco-wlp05,
       wlp06      TYPE rpsco-wlp06,
       wlp07      TYPE rpsco-wlp07,
       wlp08      TYPE rpsco-wlp08,
       wlp09      TYPE rpsco-wlp09,
       wlp10      TYPE rpsco-wlp10,
       wlp11      TYPE rpsco-wlp11,
       wlp12      TYPE rpsco-wlp12,
       wlp13      TYPE rpsco-wlp13,
       wlp14      TYPE rpsco-wlp14,
       wlp15      TYPE rpsco-wlp15,
       wlp16      TYPE rpsco-wlp16,
       END OF lty_rpsco,

       BEGIN OF lty_pratx,
       prart TYPE prart,
       pratx TYPE pratx,
       END OF lty_pratx,

       BEGIN OF lty_a_proj,
       pspnr  TYPE proj-pspnr,
       pspid  TYPE proj-pspid,
       post1  TYPE proj-post1,
       objnr  TYPE proj-objnr,
*       astnr  TYPE proj-astnr, "changed by KMB on 20.11.2019 CHG0159956 - FUT issue
       vbukr  TYPE proj-vbukr,
       plfaz  TYPE proj-plfaz,
       plsez  TYPE proj-plsez,
       eprog  TYPE proj-eprog,
       END OF lty_a_proj,

       BEGIN OF lty_f_prps,
       pspnr  TYPE prps-pspnr,
       posid  TYPE prps-posid,
       objnr  TYPE prps-objnr,
       psphi  TYPE prps-psphi,
       stufe  TYPE prps-stufe,
       END OF lty_f_prps,

       BEGIN OF lty_a_prps,
       pspnr      TYPE prps-pspnr,
       posid      TYPE prps-posid,
       post1      TYPE prps-post1,
       objnr      TYPE prps-objnr,
       psphi      TYPE prps-psphi,
       vernr      TYPE prps-vernr,
       prart      TYPE prps-prart,
       stufe      TYPE prps-stufe,
       END OF lty_a_prps,

       BEGIN OF lty_f_final,
       pspnr  TYPE proj-pspid,
       psphi  TYPE c LENGTH 24,
       stufe  TYPE c LENGTH 3,
       wbslv  TYPE c LENGTH 24,
       wrttp  TYPE rpsco-wrttp,
       versn  TYPE rpsco-versn,
       gjahr  TYPE rpsco-gjahr,
*BOC by KMB on 15-11-2019  CHG0159956 - FUT issue fixing
*       wlpitd TYPE c LENGTH 25,
*       wlpytd TYPE c LENGTH 25,
       period TYPE n LENGTH 2,
       amount TYPE c LENGTH 20,
*EOC by KMB on 15-11-2019  CHG0159956 - FUT issue fixing
       twaer  TYPE rpsco-twaer,
       END OF lty_f_final,

       BEGIN OF lty_a_final,
       vbukr  TYPE proj-vbukr,
       prart  TYPE prps-prart,
       pratx  TYPE v_tcj1-pratx,
       pspnr  TYPE prps-posid,
       post1  TYPE prps-post1,
       psphi  TYPE c LENGTH 24,
       stufe  TYPE c LENGTH 3,
       wbslv  TYPE c LENGTH 24,
       verna  TYPE tcj04-verna,
       stat   TYPE jsto-stsma,
       plfaz  TYPE proj-plfaz,
       plsez  TYPE proj-plsez,
       eprog  TYPE proj-eprog,
       gjahr  TYPE rpsco-gjahr,
       total  TYPE c LENGTH 25,
       wlpitd TYPE c LENGTH 25,
       wlpmtd TYPE c LENGTH 25,
       wlpytd TYPE c LENGTH 25,
       twaer  TYPE rpsco-twaer,
*       astnr  TYPE proj-astnr, "changed by KMB on 20.11.2019 CHG0159956 - FUT issue
       END OF lty_a_final,

       BEGIN OF lty_f_proj,
       pspnr TYPE proj-pspnr,
       END OF lty_f_proj,

       BEGIN OF lty_tcj04,
       vernr TYPE tcj04-vernr,
       verna TYPE tcj04-verna,
       END OF lty_tcj04,

       BEGIN OF lty_jest,
       objnr TYPE jest-objnr,
       stat  TYPE jest-stat,
       inact TYPE jest-inact,
       END OF lty_jest,

       BEGIN OF lty_tj30t,
       stsma TYPE tj30t-stsma,
       estat TYPE tj30t-estat,
       txt04 TYPE tj30t-txt04,
       END OF lty_tj30t,

       BEGIN OF lty_prps,
       posid TYPE prps-posid,
       END OF lty_prps,

       BEGIN OF lty_prps_n,
       psphi TYPE prps-psphi,
       END OF lty_prps_n.

"Internal table and work area declarations
DATA: gt_f_rpsco TYPE TABLE OF lty_rpsco,
      gs_f_rpsco TYPE lty_rpsco,
      gt_f_proj  TYPE TABLE OF lty_f_proj,
      gs_f_proj  TYPE lty_f_proj,
      gt_a_rpsco TYPE TABLE OF lty_rpsco,
      gs_a_rpsco TYPE lty_rpsco,
      gt_pratx   TYPE TABLE OF lty_pratx,
      gs_pratx   TYPE lty_pratx,
      gt_tcj04   TYPE TABLE OF lty_tcj04,
      gs_tcj04   TYPE lty_tcj04,
      gt_jest    TYPE TABLE OF lty_jest,
      gt_tj30t   TYPE TABLE OF lty_tj30t,
      gs_tj30t   TYPE lty_tj30t,
      gs_jest    TYPE lty_jest,
      gt_a_proj  TYPE TABLE OF lty_a_proj,
      gs_a_proj  TYPE lty_a_proj,
      gt_f_prps  TYPE TABLE OF lty_f_prps,
      gs_f_prps1  TYPE lty_f_prps,
      gt_a_prps  TYPE TABLE OF lty_a_prps,
      gs_f_prps  TYPE lty_f_prps,
      gs_a_prps  TYPE lty_a_prps,
      gs_a_prps1 TYPE lty_a_prps,
      gt_f_final TYPE TABLE OF lty_f_final,
      gs_f_final TYPE lty_f_final,
      gs_f_fin   TYPE lty_f_final,
      gt_a_final TYPE TABLE OF lty_a_final,
      gs_a_final TYPE lty_a_final,
      gs_a_fin   TYPE lty_a_final,
      gt_a_final1 TYPE TABLE OF lty_a_final,
      gt_f_final1 TYPE TABLE OF lty_f_final,
      gt_psphi TYPE TABLE OF lty_prps_n,
      gs_psphi TYPE lty_prps_n,
      gt_i_wbs TYPE TABLE OF bapi_wbs_elements,
      gs_i_wbs TYPE  bapi_wbs_elements,
      gt_e_wbs TYPE TABLE OF bapi_wbs_element_exp,
      gs_e_wbs TYPE bapi_wbs_element_exp,
      gt_msg TYPE TABLE OF bapi_meth_message,
      gs_msg TYPE bapi_meth_message,
      gt_prps TYPE TABLE OF lty_prps,
      gs_prps TYPE lty_prps.

*BOC by KMB on 15-11-2019  CHG0159956 - FUT issue fixing
DATA :gv_file_size     TYPE syindex,
      lv_file          TYPE string ,
      gt_a_new         TYPE TABLE OF lty_a_final,
      gt_f_new         TYPE TABLE OF lty_f_final,
      lv_strlen        TYPE syindex,
      lv_index1        TYPE n LENGTH 3,
      lv_index_a       TYPE n LENGTH 3,
      lv_left          TYPE n LENGTH 10,
      lv_a_left        TYPE n LENGTH 10,
      lv_actual        TYPE n LENGTH 10,
      lv_a_actual      TYPE n LENGTH 10,
      lv_f_total       TYPE n LENGTH 20,
      lv_a_total       TYPE n LENGTH 20,
      gv_path_f        TYPE string,
      gv_path_a        TYPE string,
      ls_output        TYPE string,
      lv_string        TYPE string,
      lv_msg           TYPE text100,
      gv_file_size_brk TYPE syindex,
      lv_date          TYPE p0001-begda,
      lv_cal_date      TYPE p0001-begda,
      lv_high_year     TYPE rpsco-gjahr,
      gv_first         TYPE c,
      gv_a_first       TYPE c,
      lr_year          TYPE RANGE OF rpsco-gjahr,
      ls_year          LIKE LINE OF lr_year.
*EOC by KMB on 15-11-2019  CHG0159956 - FUT issue fixing

*BOC by KMB on 29.11.2019 CHG0159956 - Adding file size field
DATA : lv_a_name TYPE string,
       lv_file_size     TYPE syindex,
       lv_file_size_f     TYPE syindex,
       lv_name TYPE string,
       lv_left_app_size TYPE syindex,
       lv_left_app_size_f TYPE syindex,
      lv_strlen_f        TYPE syindex,
      gv_file_size_f   TYPE syindex,
      lv_index_new       TYPE n LENGTH 3,
      lv_index_new_f       TYPE n LENGTH 3.
*EOC by KMB on 29.11.2019 CHG0159956 - Adding file size field

"Variables
DATA : gv_wlpitd TYPE rpsco-wlp16,
       gv_wlpytd TYPE rpsco-wlp16,
       gv_flag   TYPE c,
       gv_flag1  TYPE c.

"Constants
CONSTANTS : gc_delim   TYPE c     VALUE ',',
            gc_1       TYPE char2 VALUE '01',
            gc_2       TYPE char2 VALUE '02',
            gc_3       TYPE char2 VALUE '03',
            gc_4       TYPE char2 VALUE '04',
            gc_5       TYPE char2 VALUE '05',
            gc_6       TYPE char2 VALUE '06',
            gc_7       TYPE char2 VALUE '07',
            gc_8       TYPE char2 VALUE '08',
            gc_9       TYPE char2 VALUE '09',
            gc_10      TYPE char2 VALUE '10',
            gc_11      TYPE char2 VALUE '11',
            gc_12      TYPE char2 VALUE '12',
            gc_13      TYPE char2 VALUE '13',
            gc_14      TYPE char2 VALUE '14',
            gc_15      TYPE char2 VALUE '15',
            gc_16      TYPE char2 VALUE '16',
            gc_41      TYPE char2 VALUE '41',
            gc_e       TYPE c     VALUE 'E',
            gc_x       TYPE c     VALUE 'X',
            gc_lang    TYPE sy-langu VALUE 'E'.
