*&---------------------------------------------------------------------*
*&  Include           ZLPSI912_WBS_EXTRACT_TOP
*&---------------------------------------------------------------------*
*& This include has been copied from include zlps912_wbs_extract_top
*& for CR CHG0172237 by AHMADT on 13/02/2020
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZLPSI008_WBS_EXTRACT                              *
*  Include:          ZLPSI008_WBS_EXTRACT_TOP                          *
*  Author:           John Hartung                                      *
*  Date:             September 9, 2016                                 *
*  Ticket#:          ACR-2338                                          *
*  Application Area: FICO PS                                           *
*                                                                      *
*  Description:      PS WBS Extract including Long Text                *
*                    TOP Include - Data Declarations                   *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 09/09/16 JRHARTUNG DECK916888 - ACR-2338 - Initial program           *
*                    DECK916894, DECK916910, DECK917113, DECK917151,   *
*                    DECK917204                                        *
* 13/02/20 AHMADT    D30K930427  CHG0172237                            *
*                    Report copied from DEC.Commented all custom fields*
*                    and their logic of structure PRPS_R which were    *
*                    enhanced by using structure CI_PRPS.              *
* 12/10/20 AHMADT    D30K930709  CHG0172237                            *
*                    Added logic to create multiple files for every    *
*                    100000 records.                                   *
*----------------------------------------------------------------------*
************************************************************************

*eject
TABLES: proj,                                    "Project Definition   "
        prps_r,                                  "WBS Hierarchy Reportng
        prte,                                    "WBS Scheduling Data  "
        jsto,                                    "Object Status Info   "
        pstat.                                   "Condensed Status     "

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  ty_wa_xparam     TYPE zfit_xparam,       "Parameter Master     "

        ty_it_xparam     TYPE STANDARD TABLE OF ty_wa_xparam.

TYPES: BEGIN OF ty_wa_proj,                      "Project              "
        pspnr            TYPE ps_intnr,          "Project Def (Internal"
        pspid            TYPE ps_pspid,          "Project Def (External"
        objnr            TYPE j_objnr,           "Object Number        "
       END   OF ty_wa_proj.

TYPES: BEGIN OF ty_wa_prps,                      "WBS Element          "
        posnr            TYPE ps_posnr,          "WBS Number (Internal "
        posid            TYPE ps_posid,          "WBS Number (External "
        post1            TYPE ps_post1,          "WBS Short Description"
        objnr            TYPE j_objnr,           "Object Number        "
        psphi            TYPE ps_psphi,          "Project Number (Intern
        pbukr            TYPE ps_pbukr,          "Company Code         "
        prart            TYPE ps_prart,          "Project Type         "
        stufe            TYPE ps_stufe,          "WBS Level            "
        tplnr            TYPE tplnr,             "Functional Location  "
        werks            TYPE werks_d,           "Plant                "
        usr00            TYPE usr00prps,         "User Defined-PlpSft  "
        loevm            TYPE loevm,             "Deletion Indicator   "
*  Start of comments by AHMADT for CHG0172237
*        zzbud_cat        TYPE zbud_cat,          "Category/Topic       "
*        zzmtr_sta_id     TYPE zmtr_sta_id,       "Budget Year          "
*        zzpl_no          TYPE zpl_no,            "Region               "
*        zzpl_seg         TYPE zpl_seg,           "Pipeline Segment     "
*        zzili_seg        TYPE zili_seg,          "ILI Segment          "
*        zzcomp_sta_id    TYPE zcomp_sta_id,      "Area                 "
*        zztwr_site       TYPE ztwr_site,         "Compliance           "
*        zzloc_cd         TYPE zloc_cd,           "Location Code        "
*        zzca_ex          TYPE zca_ex,            "Capital/Expense      "
*        zzinsv_dt        TYPE zinsv_dt,          "Actual In-Service Date
*        zzfact_rec_date  TYPE zfact_rec_date,    "Facility Rec Receiv Dt
*  End of changes by AHMADT for CHG0172237
        pende            TYPE ps_pende,          "Basic Finish Date    "
       END   OF ty_wa_prps.

TYPES:  ty_wa_pstat      TYPE pstat.             "Condensed Status     "

*eject
TYPES: BEGIN OF ty_wa_t001,                      "Company Code         "
        bukrs            TYPE bukrs,             "Company Code         "
        butxt            TYPE butxt,             "Company Name         "
       END   OF ty_wa_t001,

        ty_it_t001       TYPE HASHED TABLE OF ty_wa_t001
                         WITH UNIQUE KEY bukrs.

TYPES: BEGIN OF ty_wa_t001w,                     "Plant                "
        werks            TYPE werks_d,           "Plant                "
        name1            TYPE name1,             "Plant Name           "
       END   OF ty_wa_t001w,

        ty_it_t001w      TYPE HASHED TABLE OF ty_wa_t001w
                         WITH UNIQUE KEY werks.

TYPES: BEGIN OF ty_wa_tcj1t,                     "Project Type         "
        prart            TYPE ps_prart,          "Project Type         "
        pratx            TYPE ps_pratx,          "Project Type Name    "
       END   OF ty_wa_tcj1t,

        ty_it_tcj1t      TYPE HASHED TABLE OF ty_wa_tcj1t
                         WITH UNIQUE KEY prart.

* Start of changes by AHMADT for CHG0172237
*TYPES: BEGIN OF ty_wa_budcat_val,                "Budget Category Desc "
*        cvalue           TYPE zbud_cat,          "Budget Category/Topic"
*        cdescription     TYPE zce_description,   "Budget Category Desc "
*       END   OF ty_wa_budcat_val,
*
*        ty_it_budcat_val TYPE HASHED TABLE OF ty_wa_budcat_val
*                         WITH UNIQUE KEY cvalue.


*TYPES: BEGIN OF ty_wa_cmpsta_val,                "Area Description     "
*        ovalue           TYPE zcomp_sta_id,      "Area                 "
*        odescription     TYPE zote_description,  "Area Description     "
*       END   OF ty_wa_cmpsta_val,
*
*        ty_it_cmpsta_val TYPE HASHED TABLE OF ty_wa_cmpsta_val
*                         WITH UNIQUE KEY ovalue.

*TYPES: BEGIN OF ty_wa_mtrsta_val,                "Budget Year Desc     "
*        mvalue           TYPE zmtr_sta_id,       "Budget Year          "
*        mdescription     TYPE zmte_description,  "Budget Year Desc     "
*       END   OF ty_wa_mtrsta_val,
*
*        ty_it_mtrsta_val TYPE HASHED TABLE OF ty_wa_mtrsta_val
*                         WITH UNIQUE KEY mvalue.

*TYPES: BEGIN OF ty_wa_pl_val_pval,               "Region Description   "
*        pvalue           TYPE zpl_no,            "Region               "
*        pdescription     TYPE zpte_description,  "Region Description   "
*       END   OF ty_wa_pl_val_pval,
*
*        ty_it_pl_val_pval
*                         TYPE HASHED TABLE OF ty_wa_pl_val_pval
*                         WITH UNIQUE KEY pvalue.
*        End of changes by AHMADT for CHG0172237
*eject
TYPES: BEGIN OF ty_wa_out_txt,                   "Output - Text Line   "
        prart            TYPE ps_prart,          "Project Type         "
        pratx            TYPE ps_pratx,          "Project Type Name    "
* Start of changes by AHAMDT for CHG0172237
*        zzcomp_sta_id    TYPE zcomp_sta_id,      "Area                 "
*        odescription     TYPE zote_description,  "Area Description     "
*        End of changes by AHMADT for CHG0172237
        pbukr            TYPE ps_pbukr,          "Company Code         "
        butxt            TYPE butxt,             "Company Name         "
        werks            TYPE werks_d,           "Plant                "
        name1            TYPE name1,             "Plant Name           "
        stufe            TYPE char3,             "WBS Level            "
        posid            TYPE ps_posid,          "WBS Number (External "
        post1            TYPE ps_post1,          "WBS Description      "
        usr00            TYPE usr00prps,         "User Defined-PlpSft  "
        status           TYPE text100,           "Status System / User "
        tplnr            TYPE tplnr,             "Functional Location  "
* Start of changes by AHMADT for CHG0172237
*        zzloc_cd         TYPE zloc_cd,           "Location Code        "
*        zzpl_no          TYPE zpl_no,            "Region               "
*        pdescription     TYPE zpte_description,  "Region Description   "
*     zzca_ex             TYPE zca_ex,            "Capital/Expense      "
*        cdescription     TYPE zce_description,   "Budget Category Desc "
*        zzpl_seg         TYPE zpl_seg,           "Pipeline Segment     "
*        zzili_seg        TYPE zili_seg,          "ILI Segment          "
*        zztwr_site       TYPE ztwr_site,         "Compliance           "
*        mdescription     TYPE zmte_description,  "Budget Year Desc
* End of changes by AHMADT for CHG0172237
        pende_c          TYPE char10,            "Basic Finish Date    "
*        zzinsv_dt_c      TYPE char10,            "Actual In-Service Date
        zzfact_rec_dt_c  TYPE char10,            "Facility Rec Receiv Dt

        cvalue_c         TYPE char3,             "Category/Topic       "   "
        loevm            TYPE loevm,             "Deletion Indicator   "
        long_text        TYPE STRING,            "WBS Long Text        "
       END   OF ty_wa_out_txt.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_param_out_int TYPE zparamkey          "Outbound Interface   "
                         VALUE 'OUTBOUND_INTERFACE',
        gc_param_obj_id  TYPE zparamkey          "Object ID            "
                         VALUE 'I_CL2C_PS_009',  "Changed from
*       I_CL2C_PS_008 to I_CL2C_PS_009 by AHMADT for CHG0172224

        gc_param_object  TYPE zparamkey          "Object               "
                         VALUE 'OBJECT',
        gc_param_fp_outb TYPE zparamkey          "Outbound File        "
                         VALUE 'OUTBOUND_FILEPATH',
        gc_param_fp_outa TYPE zparamkey          "Outbound Archive File"
                         VALUE 'OUTBOUND_ARCHIVE_FILEPATH',
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP',
        gc_tab           TYPE char1              "Tab Character        "
                         VALUE cl_abap_char_utilities=>horizontal_tab,
*  Start of changes by AHMADT for CHG0172237
        gc_record        TYPE zparamkey VALUE 'RECORD_COUNT'.
*  Start of changes by AHMADT for CHG0172237

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_subrc         TYPE sysubrc,           "Return Code          "
        gv_obj_id        TYPE char14,            "Object ID            "
        gv_datum_run     TYPE sydatum,           "Run Date             "
        gv_uzeit_run     TYPE syuzeit.           "Run Time             "

DATA:   gv_sysid         TYPE sysysid,           "Name Of The SAP System
        gv_uname         TYPE syuname,           "User Name            "
        gv_pagno         TYPE sypagno,           "Current List Page    "
        gv_cprog         TYPE sycprog,           "Calling Program      "
        gv_datum         TYPE char10,            "Current Date of Applic
        gv_uzeit         TYPE char08.            "Current Time of Applic

DATA:   gv_objnr         TYPE j_objnr,           "Object Number        "
        gv_filename      TYPE text255,           "File Path & Name     "
        gv_filename1     TYPE text255,           "File Path & Name     "
        gv_count_recs    TYPE numc10,            "Record Count         "
*  Start of changes by AHMADT for CHG0172237
        gv_file_count    TYPE char3 VALUE '2',   "File count
        gv_file_ext      TYPE char7,
        gv_file_ext_last TYPE char7 VALUE '_1.txt'.
*  End of changes by AHMADT for CHG0172237

************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gs_proj          TYPE ty_wa_proj,        "Project              "
        gs_proj_p        TYPE ty_wa_proj,        "Project              "
        gs_prps          TYPE ty_wa_prps,        "WBS Element          "
        gs_pstat         TYPE ty_wa_pstat,       "Condensed Status     "
        gs_t001          TYPE ty_wa_t001,        "Company Code         "
        gs_t001w         TYPE ty_wa_t001w,       "Plant                "
        gs_tcj1t         TYPE ty_wa_tcj1t.       "Project Type         "
*        gs_budcat_val    TYPE ty_wa_budcat_val,  "Budget Category Desc "
*        gs_cmpsta_val    TYPE ty_wa_cmpsta_val,  "Area Description     "
*        gs_mtrsta_val    TYPE ty_wa_mtrsta_val,  "Budget Year Desc     "
*        gs_pl_val_pval   TYPE ty_wa_pl_val_pval. "Region Description   "

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_xparam        TYPE ty_it_xparam,      "Parameter Master     "
        gt_t001          TYPE ty_it_t001,        "Company Code         "
        gt_t001w         TYPE ty_it_t001w,       "Plant                "
        gt_tcj1t         TYPE ty_it_tcj1t.       "Project Type         "
*        gt_budcat_val    TYPE ty_it_budcat_val,  "Budget Category Desc "
*        gt_cmpsta_val    TYPE ty_it_cmpsta_val,  "Area Description     "
*        gt_mtrsta_val    TYPE ty_it_mtrsta_val,  "Budget Year Desc     "
*        gt_pl_val_pval   TYPE ty_it_pl_val_pval. "Region Description   "

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************

* Run Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_objid  TYPE zparamkey        "Object ID            "
                           VISIBLE LENGTH 13
                           OBLIGATORY
                           MODIF ID DSP.
SELECTION-SCREEN: COMMENT 50(45) p_objdsc.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_test   AS CHECKBOX           "Test Flag            "
                           DEFAULT 'X'.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_inclch AS CHECKBOX           "Incl. Column Headings"
                           DEFAULT 'X'.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 33.
SELECTION-SCREEN: COMMENT  05(22) text-f11.
SELECTION-SCREEN: POSITION 33.
PARAMETERS:       p_asfpof TYPE text255          "File Path-Output File"
                  MODIF ID DSP.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 33.
SELECTION-SCREEN: COMMENT  05(22) text-f12.
SELECTION-SCREEN: POSITION 33.
PARAMETERS:       p_asfnof TYPE text255.         "File Name-Output File"
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 33.
SELECTION-SCREEN: COMMENT  05(22) text-f13.
SELECTION-SCREEN: POSITION 33.
PARAMETERS:       p_asfpaf TYPE text255          "File Path-Archive File
                  MODIF ID DSP.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: END   OF BLOCK ssb1.
