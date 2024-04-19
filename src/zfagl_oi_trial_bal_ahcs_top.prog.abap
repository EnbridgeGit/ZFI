*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_AHCS_TOP
*&---------------------------------------------------------------------*

TABLES: faglflext, t001.

TYPES: BEGIN OF ty_data,
         ledger        TYPE text40,
         transaction   TYPE text40,
         rep_period    TYPE text10,
         eff_date      TYPE text10,
         currency      TYPE waers,
         compancy_code TYPE bukrs,
         trade_partner TYPE text10,
         kostl         TYPE kostl,
         cfa           TYPE zzref,
         aufnr         TYPE aufnr,
         settle_period TYPE text10,
         wbs           TYPE ps_psp_pnr,
         location      TYPE c,
         prctr         TYPE prctr,
         amount        TYPE hslvt12,
         h_var1        TYPE c,
         h_var2        TYPE c,
         h_var3        TYPE c,
         l_var1        TYPE c,
         l_var2        TYPE c,
         l_var3        TYPE c,
       END OF ty_data,

       BEGIN OF ty_lastday,
         month(2) TYPE c,
         day(2)   TYPE c,
         year(4)  TYPE c,
       END OF ty_lastday,

        BEGIN OF ty_t001,
        bukrs TYPE bukrs,
        waers TYPE waers,
       END OF ty_t001,

         BEGIN OF ty_ytd,
           ryear  TYPE gjahr,
           rpmax  TYPE rpmax,
           rtcur  TYPE rtcur,
           racct  TYPE racct,
           rbukrs	TYPE bukrs,
           rcntr  TYPE kostl,
           prctr  TYPE prctr,
           rassc  TYPE rassc,
           hslvt  TYPE hslvt12,
           hsl01  TYPE hslxx12,
           hsl02  TYPE hslxx12,
           hsl03  TYPE hslxx12,
           hsl04  TYPE hslxx12,
           hsl05  TYPE hslxx12,
           hsl06  TYPE hslxx12,
           hsl07  TYPE hslxx12,
           hsl08  TYPE hslxx12,
           hsl09  TYPE hslxx12,
           hsl10  TYPE hslxx12,
           hsl11  TYPE hslxx12,
           hsl12  TYPE hslxx12,
           hsl13  TYPE hslxx12,
           hsl14  TYPE hslxx12,
           hsl15  TYPE hslxx12,
           hsl16  TYPE hslxx12,
           TIMESTAMP type TIMESTAMP,
         END OF ty_ytd.

*       BEGIN OF ty_ccm,
*         gl_no TYPE c,
*         kostl TYPE kostl,
*       END OF ty_ccm.

DATA: "gt_faglflext TYPE STANDARD TABLE OF faglflext,
      gt_faglflext TYPE STANDARD TABLE OF ty_ytd,
*      gt_ccm       TYPE STANDARD TABLE OF ty_ccm,
      gt_t001      TYPE STANDARD TABLE OF ty_t001,
      gt_last_days TYPE STANDARD TABLE OF ty_lastday,
      gt_file      TYPE truxs_t_text_data,
      gt_usfile    TYPE truxs_t_text_data,
      gv_ccm       TYPE text12,
      gt_data      TYPE STANDARD TABLE OF ty_data.