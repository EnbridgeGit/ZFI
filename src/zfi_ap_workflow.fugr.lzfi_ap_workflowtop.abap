*----------------------------------------------------------------------*
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  19-Sep-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :                                                *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*


FUNCTION-POOL ZFI_AP_WORKFLOW MESSAGE-ID B!..               "MESSAGE-ID ..

DATA: gtp_okcode TYPE syucomm,
      gtp_bukrs  TYPE bukrs,
      gtp_belnr  TYPE belnr_d,
      gtp_gjahr  TYPE gjahr,
      gtp_workitem_id TYPE  swr_struct-workitemid,
      gtp_workitem_top TYPE  swr_struct-workitemid,
      gtp_username TYPE username,
      gtp_comment  type zfis_ap_comments-comments.

DATA:
      gtp_user       TYPE username,

      gta_objtxt     TYPE swftlisti1,
      gst_doc_chng   TYPE sodocchgi1,
      gta_objpack    TYPE swftpcklst,
      gta_objbin     TYPE swftlisti1,
      gtp_total_cost TYPE  ptrv_shdr-trip_total,
      gta_reclist    TYPE bubas_t_receiver.

DATA: gtp_decision_type TYPE zfis_ap_workflow-conf_type,
      gtp_user_decision TYPE zfis_ap_workflow-conf_type.

*FUNCTION-POOL BFFM2 MESSAGE-ID B!.

*----------- DDIC-tables -----------------------------------------------
TABLES:  TBE01,                   " Events
         TPS01,                   " Processes
         TBE11,                   " Applications
         TBE12,                   " Partners
         TBE22,                   " Products of partners
         TBE22T,                  " Texts for products
         TBE23,                   " Active products of partners
         TBE24,                   " Products of customers
         TBE24T,                  " Texts for products
         TBE32T.                  " Function modules of partners (texts)

*------------ Tables for special events --------------------------------
TABLES: T042OFI.
*------------------ Contexts -------------------------------------------
CONTEXTS: PP_INFO.
DATA: PP_CT TYPE CONTEXT_PP_INFO.

CONTEXTS: SP_INFO.
DATA: SP_CT TYPE CONTEXT_SP_INFO.

CONTEXTS: CP_INFO.
DATA: CP_CT TYPE CONTEXT_CP_INFO.

*----------- Type Pools ------------------------------------------------
TYPE-POOLS: SBIWA.

*------------------- Internal tables -----------------------------------

*------ serves as a buffer for BF_FUNCTIONS_READ -----------------------
DATA: BEGIN OF EVSTATUS OCCURS 10,
    EVENT LIKE TBE01-EVENT,         "-----\
    LAND  LIKE T005-LAND1,          "------->   KEY AREA
    APPLK LIKE TBE11-APPLK,         "-----/
    CHCKD(1),                       "X = Event was checked
    EMPTY(1),                       "X = No component active
       END OF EVSTATUS.

*----------------analagously for processes ----------------------------
DATA: BEGIN OF PRSTATUS OCCURS 10,
    PROCS LIKE TPS01-PROCS,          "-----\
    LAND  LIKE T005-LAND1 ,          "------->   KEY AREA
    APPLK LIKE TBE11-APPLK,          "-----/
    CHCKD(1),                       "X = Process was checked
    EMPTY(1),                       "X = No component active
      END OF PRSTATUS.

*----------------- Copies of DB-tables ---------------------------------
DATA: BEGIN OF ITBE31 OCCURS 100.
        INCLUDE STRUCTURE TBE31.
DATA: END OF ITBE31.

DATA: BEGIN OF ITBE32 OCCURS 100.
        INCLUDE STRUCTURE TBE32.
DATA: END OF ITBE32.

DATA: BEGIN OF ITBE34 OCCURS 100.
        INCLUDE STRUCTURE TBE34.
DATA: END OF ITBE34.

DATA: BEGIN OF ITPS31 OCCURS 100.
        INCLUDE STRUCTURE TPS31.
DATA: END OF ITPS31.

DATA: BEGIN OF ITPS32 OCCURS 100.
        INCLUDE STRUCTURE TPS32.
DATA: END OF ITPS32.

DATA: BEGIN OF ITPS34 OCCURS 100.
        INCLUDE STRUCTURE TPS34.
DATA: END OF ITPS34.

*------------ Function modules of SAP-applications (events) ------------
DATA: BEGIN OF FMSAPTAB OCCURS 10.
        INCLUDE STRUCTURE FMSAP.
DATA: END OF FMSAPTAB.

*------------ Function modules of partners (events) --------------------
DATA: BEGIN OF FMPRTTAB OCCURS 10.
        INCLUDE STRUCTURE FMPRT.
DATA: END OF FMPRTTAB.

*------------ Function modules of customers (events) -------------------
DATA: BEGIN OF FMCUSTAB OCCURS 10.
        INCLUDE STRUCTURE FMCUS.
DATA: END OF FMCUSTAB.

*------------ Function modules of SAP-applications (processes) ---------
DATA: BEGIN OF APSAPTAB OCCURS 10.
        INCLUDE STRUCTURE APSAP.
DATA: END OF APSAPTAB.

*------------ Function modules of partners (processes) -----------------
DATA: BEGIN OF APPRTTAB OCCURS 10.
        INCLUDE STRUCTURE APPRT.
DATA: END OF APPRTTAB.

*------------ Function modules of customers (processes) ----------------
DATA: BEGIN OF APCUSTAB OCCURS 10.
        INCLUDE STRUCTURE APCUS.
DATA: END OF APCUSTAB.

*------------ Function modules to be called at a given event -----------
DATA: BEGIN OF FMTAB OCCURS 10.
        INCLUDE STRUCTURE FMRFC.
DATA: END OF FMTAB.

*------------ Functions to be called -----------------------------------
DATA: BEGIN OF FCTTAB OCCURS 10.
        INCLUDE STRUCTURE FTEXTS.
DATA:  FUNCT        LIKE TFDIR-FUNCNAME,  "Function module
       RFCDS        LIKE RFCDES-RFCDEST,  "RFC-Destination
        END OF FCTTAB.

*----------------- Texts for CUA-menu ----------------------------------
DATA: BEGIN OF FTEXTSTAB OCCURS 10.
        INCLUDE STRUCTURE FTEXTS.
DATA: END OF FTEXTSTAB.

*-------------- Table for appending additional list lines in CM --------
DATA: BEGIN OF LISTTAB OCCURS 100.
        INCLUDE STRUCTURE CMTEXT.
DATA: END OF LISTTAB.

*-------------- Table for appending additional list lines in EP --------
DATA: BEGIN OF LISTEPTAB OCCURS 6.
        INCLUDE STRUCTURE EPTEXT.
DATA: END OF LISTEPTAB.

*-------------- Include for substituting processes ---------------------
INCLUDE INCLSUBS.

*--------------- Table for adding tax informationields -----------------
DATA: BEGIN OF PUMSEPTAB OCCURS 100.
        INCLUDE STRUCTURE PUMSEP.
DATA: END OF PUMSEPTAB.

*--------------- Table for adding messages -----------------------------
DATA: BEGIN OF MESSAGETAB OCCURS 100.
        INCLUDE STRUCTURE EDIMESSAGE.
DATA: END OF MESSAGETAB.

*-------------- Fields for Choose-Popup --------------------------------
DATA: BEGIN OF FIELDTAB OCCURS 10.
        INCLUDE STRUCTURE HELP_VALUE.
DATA: END OF FIELDTAB.

*-------------- Values for Choose-Popup --------------------------------
DATA: BEGIN OF SHOWTAB OCCURS 100,
  FTEXT LIKE FTEXTS-FTEXT,
  PRDKT LIKE FTEXTS-PRDKT,
      END OF SHOWTAB.

*-------------- Data fields --------------------------------------------
DATA: ABEZ            LIKE OFIWA-XTEXT,    "Aktivität Beim Entspr. Ztpkt
      EMPTY(1)        TYPE C,              "No activity at event
      FUNCT           LIKE TFDIR-FUNCNAME,
      INDIC(1)        TYPE C,              "Indicator
      MEMID(15)       VALUE 'OPENFI',
      MEMIDFMTAB(15)  VALUE 'OPENFI',
      PAKTIV          TYPE C,
      RCODE           LIKE SY-SUBRC,
      RFCDS           LIKE RFCDES-RFCDEST,
      SELECT_VALUE    LIKE HELP_INFO-FLDVALUE,
      SELFIELD        LIKE HELP_INFO-FIELDNAME,
      TABIX           LIKE SY-TABIX,
      XFELD(1)        TYPE C,
      XMTOS(1)        TYPE C,        " 'X'= More than one entry (SAP )
      XNOXLBSE(1)     TYPE C.        " 'X'= Ignore XLBSE


DATA: ACCITLINES      LIKE SY-TFILL,       "Number of acc. line items
      ACCCRLINES      LIKE SY-TFILL.       "Number of acc. line items

*----- Table for substituting Offset Account field of acc. line items -
DATA: BEGIN OF ACCITSUBTAB OCCURS 100.
        INCLUDE STRUCTURE ACCIT_SUBST.
DATA: END OF ACCITSUBTAB.

DATA: g_data_container    TYPE scrfname VALUE 'DATA_CONTAINER',
      obj_alv_container   TYPE REF TO cl_gui_custom_container,
      gr_obj_alv_grid     TYPE REF TO cl_gui_alv_grid,
      gwa_fcat            TYPE lvc_s_fcat,
      gtp_fcat            TYPE lvc_t_fcat.

DATA :lta_user_comment TYPE STANDARD TABLE OF zfit_apprl_cmnt.
DATA : lwa_user_comment TYPE  zfit_apprl_cmnt.
