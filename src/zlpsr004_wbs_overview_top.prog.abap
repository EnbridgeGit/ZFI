*&---------------------------------------------------------------------*
*&  Include           ZLPSR004_WBS_OVERVIEW_TOP
*&---------------------------------------------------------------------*
*Class definition for ALV toolbar
CLASS:lcl_alv_event   DEFINITION DEFERRED.
*Global constants declaration
CONSTANTS:gc_container    TYPE scrfname VALUE 'GC_CONTAINER'.
*Global constants declaration
CONSTANTS:gc_set   TYPE flag VALUE 'X',
          gc_sys   TYPE flag VALUE 'I',
          gc_usr   TYPE flag VALUE 'E',
          gc_error TYPE flag VALUE 'E'.
*Internal tables and work area declaration
DATA: gt_fcat     TYPE lvc_t_fcat,
      gwa_fcat     TYPE lvc_s_fcat,
      gwa_layout   TYPE lvc_s_layo.
*Object declaration
DATA: o_ccont              TYPE REF TO cl_gui_custom_container,   "Custom container object
      o_alvgd              TYPE REF TO cl_gui_alv_grid,   "ALV grid object
      o_alv_toolbar        TYPE REF TO lcl_alv_event,
      o_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.  "Toolbar manager
*Global variables declaration
DATA: gv_pspid  TYPE ps_pspid,
      gv_pspnr  TYPE ps_posnr,
      gv_status TYPE tj02-istat,
      gv_stufe  TYPE ps_stufe.
*Global table types declaration
TYPES:BEGIN OF gty_proj_details,
       pspnr TYPE ps_posnr,
       pspid TYPE ps_pspid,
       plfaz TYPE ps_plfaz,
       plsez TYPE ps_plsez,
       sprog TYPE ps_sprog,
       eprog TYPE ps_eprog,
       psphi TYPE ps_psphi,
  END OF gty_proj_details,
  BEGIN OF gty_wbs_nos,
       wbs   TYPE ps_posnr,
       psphi TYPE ps_psphi,
       pspid TYPE ps_pspid,
  END OF gty_wbs_nos,
BEGIN OF gty_wbs_details,
       pspnr TYPE ps_posnr,
       "posid TYPE ps_posid,
       post1 TYPE ps_post1,
       objnr TYPE j_objnr,
       psphi TYPE prps-psphi,
       verna TYPE prps-verna,
       stufe TYPE prps-stufe,
       kalsm TYPE prps-kalsm,
       loevm TYPE prps-loevm,
       usr01 TYPE prps-usr01,
       usr08 TYPE prps-usr08,
       usr09 TYPE prps-usr09,
  END OF gty_wbs_details,
  BEGIN OF gty_status,
       stat  TYPE j_istat,
       spras TYPE spras,
       desc  TYPE bsvx-sttxt,
  END OF gty_status,
    BEGIN OF gty_status_text,
       stat  TYPE j_istat,
       objnr TYPE objnr,
       spras TYPE spras,
       desc  TYPE bsvx-sttxt,
  END OF gty_status_text,
  BEGIN OF gty_report_details,
       pspid TYPE ps_pspid,
       verna TYPE prps-verna,
       wbs   TYPE ps_posnr,
       kalsm TYPE aufkalsm,
       post1 TYPE ps_post1,
**--START OF CHNAGES BY AKMADASU CHG0138615
       asloc TYPE CHAR20,
**-- END OF CHANGES BY AKMADASU CHG0138615
       stufe TYPE ps_stufe,
       loevm TYPE loevm,
       desc  TYPE bsvx-sttxt,
       usr01 TYPE usr01prps,
       usr08 TYPE ps_sprog,
       usr09 TYPE ps_sprog,
       plfaz TYPE ps_plfaz,
       plsez TYPE ps_plsez,
       sprog TYPE ps_sprog,
       eprog TYPE ps_eprog,
END OF gty_report_details.
*Global internal table declaration
DATA:gt_proj_details   TYPE STANDARD TABLE OF gty_proj_details   INITIAL SIZE 0,
     gt_wbs_nos        TYPE STANDARD TABLE OF gty_wbs_nos        INITIAL SIZE 0,
     gt_wbs_details    TYPE STANDARD TABLE OF gty_wbs_details    INITIAL SIZE 0,
     gt_report_details TYPE STANDARD TABLE OF gty_report_details INITIAL SIZE 0.
*Global work area declaration
DATA:gwa_proj_details   TYPE gty_proj_details,
     gwa_wbs_nos        TYPE gty_wbs_nos,
     gwa_wbs_details    TYPE gty_wbs_details,
     gwa_report_details TYPE gty_report_details.
