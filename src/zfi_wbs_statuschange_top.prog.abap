*&---------------------------------------------------------------------*
*&  Include           ZFI_WBS_STATUSCHANGE_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZFI_WBS_STATUSCHANGE                           *
* Include            :  ZFI_WBS_STATUSCHANGE_TOP                       *
* Author             :  Ashok Madasu                                   *
* Date               :  20-Aug-2018                                    *
* Technical Contact  :  Ashok Madasu                                   *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  WBS status change report                       *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 22/08/2019   akmadasu    D30K930092  CHG0138619 - in-service and     *
*                          D30K930231  completion date updation to WBSe*
*&---------------------------------------------------------------------*


TABLES: prps,prhi,proj.

TYPES: BEGIN OF ty_prps,
       pspnr TYPE prps-pspnr,
       POSID TYPE PS_POSID, " ADDED BY AKMADASU FOR CHG0138619
       PSPHI TYPE PS_PSPHI, " ADDED BY AKMADASU FOR CHG0138619
       STUFE TYPE	PS_STUFE, " ADDED BY JOOKONTR FOR CHG0138619
       objnr TYPE prps-objnr,
       END OF ty_prps,
       BEGIN OF ty_jest,
       objnr TYPE jest-objnr,
       stat  TYPE jest-stat,
       inact TYPE jest-inact,
       END OF ty_jest,
       BEGIN OF ty_final,
       pspnr TYPE ps_posid,
       st_old TYPE jest-stat,
       st_new TYPE jest-stat,
       message TYPE string,
       END OF ty_final.
TYPES: BEGIN OF gty_prhi,
       posnr TYPE ps_posnr,
       psphi TYPE ps_psphi,
       END OF gty_prhi,
       BEGIN OF gty_proj,
       pspnr TYPE ps_intnr,
       PSPID TYPE PS_PSPID, " ADDED BY AKMADASU CHG0138619
       END OF gty_proj.

DATA: gt_prhi TYPE TABLE OF gty_prhi,
      gt_proj TYPE TABLE OF gty_proj,
      gs_proj TYPE gty_proj,
      gs_prhi TYPE gty_prhi.


DATA : gt_prps TYPE TABLE OF ty_prps,
       gt_prps_tmp TYPE TABLE OF ty_prps,
       gs_prps TYPE ty_prps,
       gt_jest TYPE TABLE OF ty_jest,
       gs_jest TYPE ty_jest,
       fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       gt_final TYPE TABLE OF ty_final,
       gt_final_er TYPE TABLE OF ty_final, "Added by akmadasu CHG0138619
       gs_final TYPE ty_final,
       gt_TJ02T TYPE STANDARD TABLE OF TJ02T, "Added by akmadasu CHG0138619
       gs_TJ02T like LINE OF gt_TJ02T.        "Added by akmadasu CHG0138619
**-- start of chnages by akmadasu CHG0138619
DATA:  gt_final_TEMP type TABLE OF ty_final,
       gs_final_TEMP type ty_final,
       gs_prj_def    TYPE bapi_project_definition,
       GV_CONT       TYPE FLAG,
       GV_FLAG_D     TYPE FLAG,
       GV_FLAG_A     TYPE FLAG,
       gt_messtab    TYPE TABLE OF bdcmsgcoll,
       gs_messtab    TYPE bdcmsgcoll,
       gv_message    TYPE char80,
       gt_bdcdata    TYPE TABLE OF bdcdata,
       gs_bdcdata    TYPE bdcdata,
       gs_prj_def_u  TYPE bapi_project_definition_up,
       gt_meth_prj   TYPE TABLE OF bapi_method_project,
       gs_meth_prj   TYPE bapi_method_project,
       gt_activity   TYPE TABLE OF bapi_act_element,
       gs_activity   TYPE bapi_act_element,
       gt_activity_u TYPE TABLE OF bapi_act_element_upd,
       gs_activity_u TYPE bapi_act_element_upd,
       gs_return1    TYPE bapireturn1,
       gt_message    TYPE TABLE OF bapi_meth_message,
       gs_message    TYPE bapi_meth_message,
       gt_wbs_ele    TYPE TABLE OF BAPI_WBS_ELEMENT,
       gs_wbs_ele    TYPE BAPI_WBS_ELEMENT,
       gt_wbs_ele_u  TYPE TABLE OF BAPI_WBS_ELEMENT_UPDATE,
       gs_wbs_ele_u  TYPE BAPI_WBS_ELEMENT_UPDATE.
**-- end of chnages by akmadasu CHG0138619
