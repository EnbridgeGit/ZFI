*----------------------------------------------------------------------*
* Report Name: ZFII_PIEMINSEXT_TOP
* Author:	     KBANERJEE-Kaushiki Banerjee
* Date:	       October 22nd,2018
*
* Logical Database: NA
* SAPScript name:   NA
* Application Area: FI
* Description:  This tool automates the process of sending information
*               like posted travel expenses, including all detailed
*               information available in the current posting run to
*               Premier Insights vendor.
*               This report has following execution modes
*               1.Display the report
*               2.Download the output in local file presentation server
*               3.Download the output in application server
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 22-OCT-2018  KBANERJEE   D30K929247  CHG0125087:Initial development  *
*                                                                      *
************************************************************************
*Global variables declaration
  DATA:gv_crdt TYPE pevsh-creadate.
*  Global constants declaration
  CONSTANTS:gc_set      TYPE flag           VALUE 'X',
            gc_paramtyp TYPE zparamtype     VALUE 'OUTBOUND_INTERFACE',
            gc_subtype  TYPE zparamsubtype  VALUE 'I_PRM_00001',
            gc_varname  TYPE zparamkey      VALUE 'APP_FILEPATH' ,
            gc_varname1 TYPE zparamkey      VALUE 'FILE_NAME' ,
            gc_varname2 TYPE zparamkey      VALUE 'MAIL_RECPIENTS',
            gc_key1     TYPE zparamkey      VALUE 'SYS_ID',
            gc_key2     TYPE zparamkey      VALUE 'FILE_SYS',
            gc_key3     TYPE zparamkey      VALUE 'EMAIL',
            gc_i        TYPE tvarv_sign     VALUE 'I',
            gc_eq       TYPE tvarv_opti     VALUE 'EQ',
            gc_sucs     TYPE flag           VALUE 'S',
            gc_error    TYPE flag           VALUE 'E'.

*  Class definition for ALV toolbar
  CLASS:lcl_alv_event   DEFINITION DEFERRED.
*  Object declaration
  DATA: o_alvgd              TYPE REF TO cl_gui_alv_grid,   "ALV grid object
        o_dockingcontainer   TYPE REF TO cl_gui_docking_container,
        o_alv_toolbar        TYPE REF TO lcl_alv_event.     "Toolbar
*  Global table types definition
  TYPES :  BEGIN OF gty_app_filepath,
             paramtype TYPE zparamtype,
             subtype   TYPE zparamsubtype,
             key1      TYPE zparamkey,
             key2      TYPE zparamkey,
             key3      TYPE zparamkey,
             value1    TYPE zparamvalue,
           END OF gty_app_filepath,
           BEGIN OF gty_pevsh,
             type     TYPE p_evtyp,
             runid    TYPE p_evnum,
             status   TYPE p_evstatus,
             creadate TYPE hier_fdate,
           END OF gty_pevsh,
           BEGIN OF gty_awkey,
             pernr TYPE pernr_d,
             reinr TYPE reinr,
             runid TYPE tr_evnum,
             awref TYPE awref,
             aworg TYPE aworg,
             awlin TYPE numc10,
           END OF gty_awkey,
           BEGIN OF gty_output,
            pernr       TYPE pernr_d,
            runid       TYPE tr_evnum,
            exbel       TYPE hr_zuonr,
            bukrs       TYPE bukrs,
            blart       TYPE blart,
            budat       TYPE char10,
            ktosl       TYPE ktosl,
            hkont       TYPE hkont,
            hkonttxt    TYPE txt20_skat,
            waers       TYPE waers,
            wrbtr       TYPE acbtr,
            amount      TYPE string,
            mwskz       TYPE mwskz,
            hkonttax    TYPE saknr,
            steur_soll  TYPE hwste,
            tax_dr      TYPE string,
            steur_haben TYPE hwste,
            tax_cr      TYPE string,
            sgtxt       TYPE sgtxt,
            kostl       TYPE kostl,
            aufnr       TYPE aufnr,
            posnr       TYPE ps_posnr,
            reinr       TYPE reinr,
            msatz       TYPE fwste,
            lifnr       TYPE lifnr,
            awref       TYPE awref,
            aworg       TYPE aworg,
            awlin       TYPE numc10,
        END OF gty_output,
        BEGIN OF gty_output_xcl,
            pernr       TYPE pernr_d,
            exbel       TYPE hr_zuonr,
            bukrs       TYPE bukrs,
            blart       TYPE blart,
            budat       TYPE char10,
            ktosl       TYPE ktosl,
            hkont       TYPE hkont,
            hkonttxt    TYPE txt20_skat,
            waers       TYPE waers,
            amount      TYPE string,
            mwskz       TYPE mwskz,
            hkonttax    TYPE saknr,
            steur_soll  TYPE string,"hwste,
            steur_haben TYPE string,"hwste,
            sgtxt       TYPE sgtxt,
            kostl       TYPE kostl,
            aufnr       TYPE aufnr,
            wbs         TYPE char24,
            reinr       TYPE reinr,
            msatz       TYPE fwste,
            lifnr       TYPE lifnr,
        END OF gty_output_xcl,
         BEGIN OF gty_fieldnames,
            line(50) TYPE c,
        END OF gty_fieldnames.
*  Global table types declaration
  TYPES:tt_pevsh       TYPE STANDARD TABLE OF gty_pevsh,
        tt_awkey       TYPE STANDARD TABLE OF gty_awkey,
        tt_output      TYPE STANDARD TABLE OF gty_output,
        tt_fieldnames  TYPE STANDARD TABLE OF gty_fieldnames.
*  Global internal tables declaration
  DATA:gt_app_filepath TYPE STANDARD TABLE OF gty_app_filepath INITIAL SIZE 0,
       gt_file_system  TYPE STANDARD TABLE OF gty_app_filepath INITIAL SIZE 0,
       gt_pevsh        TYPE STANDARD TABLE OF gty_pevsh        INITIAL SIZE 0,
       gt_awkey        TYPE STANDARD TABLE OF gty_awkey        INITIAL SIZE 0,
       gt_fcat         TYPE STANDARD TABLE OF lvc_s_fcat       INITIAL SIZE 0,
       gt_output       TYPE STANDARD TABLE OF gty_output       INITIAL SIZE 0.
*  Global work area declaration
  DATA: gs_layout   TYPE lvc_s_layo.
*Global variables
  DATA:gv_filename        TYPE localfile,
       gv_no_recs         TYPE i,
       gv_err_msg         TYPE char255,
       gv_report          TYPE raldb_repo,
       gv_last_mnth_begin TYPE d,
       gv_last_mnth_end   TYPE d.
