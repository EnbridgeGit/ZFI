FUNCTION-POOL ZTR_TRANS_DATA_VIEW.          "MESSAGE-ID ..

*******changes by Suri 10252011****
******* Global extractor varibales/types/constants ******

TYPE-POOLS: RSSG, SRSC, SBIWA.
TYPE-POOLS: RSAOT, RSA3.
* Constants
CONSTANTS: RSGEN_C_FLAG_ON  VALUE 'X'.
CONSTANTS: RSGEN_C_FLAG_OFF VALUE ' '.
DATA: G_ROUTINE_NAME LIKE SY-REPID.
DATA: G_S_OLTPSOURCE TYPE RSAOT_S_OSOURCE.
DATA: G_S_ROIS       TYPE RSAOT_S_ROIS.
DATA: G_S_RODCHABAS  TYPE RSAOT_S_RODCHABAS.
* Global buffer for parameters passed by SAPI initialization call
DATA: g_s_interface TYPE sbiwa_s_interface.
* Global buffer for requested fields
DATA: g_t_fields TYPE sbiwa_t_fields.
* Global buffer for selection criteria
DATA: g_t_select TYPE sbiwa_t_select.
* Global buffer for selection language
DATA: g_t_langu  TYPE sbiwa_t_langu.
* Global buffer for time interval
DATA: g_s_timeint TYPE sbiwa_s_timeint.
* Global parameters
DATA: g_flag_interface_initialized,
      g_counter_datapakid TYPE sbiwa_s_interface-datapakid.
DATA: G_CURSOR TYPE CURSOR.
DATA: g_no_more_data.
DATA: l_cursor_last    TYPE i,
      n type i,
      g_maxsize type SBIWA_S_INTERFACE-MAXSIZE.


***************************changes by suri 10252011********

*---------------------------------------------------------------------*
*                       Types
*---------------------------------------------------------------------*
*
TYPES: BEGIN OF ty_t000,
          logsys TYPE logsys,
       END OF ty_t000,

       BEGIN OF ty_t001,
           bukrs TYPE t001-bukrs,
           waers TYPE t001-waers,
       END OF ty_t001,


       BEGIN OF ty_vtbfhazu,
         bukrs   TYPE vtbfhazu-bukrs,
         rfha    TYPE vtbfhazu-rfha,
         rfhazu  TYPE vtbfhazu-rfhazu,
         nordext TYPE vtbfhazu-nordext,
       END OF ty_vtbfhazu.
*---------------------------------------------------------------------*
*                      Internal Tables
*---------------------------------------------------------------------*
DATA: git_vtbfha    TYPE TABLE OF vtbfha,
      git_vtbfhapo  TYPE TABLE OF vtbfhapo,
      git_t001      TYPE TABLE OF ty_t001,
      git_vtbfhazu  TYPE TABLE OF ty_vtbfhazu.

DATA : gr_bukrs    TYPE RANGE OF BUKRS,
       gr_rfha     TYPE RANGE OF TB_RFHA.

*---------------------------------------------------------------------*
*                      Work Areas
*---------------------------------------------------------------------*
DATA: gwa_vtbfha    TYPE vtbfha,
      gwa_vtbfhapo  TYPE vtbfhapo,
      gwa_t001      TYPE ty_t001,
      gwa_vtbfhazu  TYPE ty_vtbfhazu,
      gwa_t000      TYPE t000,
      gwa_tranview  TYPE zfitr_tranview.

*---------------------------------------------------------------------*
*                      Variables
*---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&                     Constants
*&---------------------------------------------------------------------*

CONSTANTS: gc_fixed_term   TYPE vtbfha-sanlf  VALUE '510',
           gc_dep_notice   TYPE vtbfha-sanlf  VALUE '520',
           gc_active       TYPE vtbfha-saktiv VALUE '0',
           gc_concluded    TYPE vtbfha-saktiv VALUE '1',
           gc_true         TYPE char01        VALUE 'X'.

DATA: E_DATA_VIEW1 LIKE TABLE OF  ZFITR_TRANVIEW WITH HEADER LINE.
