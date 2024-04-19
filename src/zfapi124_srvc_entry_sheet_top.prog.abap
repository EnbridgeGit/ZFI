*&---------------------------------------------------------------------*
*&  Include           ZFAPI124_SRVC_ENTRY_SHEET_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       : ZFAPI124_SRVC_ENTRY_SHEET                       *
* Author             :                                                 *
* Date               : Mar 26, 2018                                    *
* Technical Contact  : Paul Karunakar                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            : The Service Entry Sheet (SES) master data       *
*                      from each of the three SAP instances will be    *
*                      extracted in a delimited file and sent to IAP.  *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 26-Mar-2018  KPAL        D30K928726  CHG0106361  Initial Development *
*                          D30K928730, D30K928878, D30K928919          *
*                          D30K928927                                  *
*----------------------------------------------------------------------*

TYPE-POOLS: vrm.

TABLES: sscrfields, essr.

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF gty_essr,                   "Service Entry Sheet       "
        lblni            TYPE lblni,
        erdat            TYPE erdat,
        aedat            TYPE aedat,
        lwert            TYPE lwert,
        waers            TYPE waers,
        packno           TYPE packno,
        txz01            TYPE txz01_essr,
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        loekz            TYPE loekz_essr,
        kzabn            TYPE kzabn,
       END   OF gty_essr.

TYPES:  gtt_essr         TYPE STANDARD TABLE OF gty_essr.

TYPES:  gty_ses_accass   TYPE bapieskn.     "Entry Sheet Account Asgnmnt

TYPES:  gtt_ses_accass   TYPE STANDARD TABLE OF gty_ses_accass.

TYPES: BEGIN OF gty_ekbe,                   "Purchasing Document History
        ebeln            TYPE ebeln,
        ebelp            TYPE ebelp,
        vgabe            TYPE vgabe,
        lfbnr            TYPE lfbnr,
       END   OF gty_ekbe.

TYPES:  gtt_ekbe         TYPE STANDARD TABLE OF gty_ekbe.

*eject
TYPES: BEGIN OF gty_final,
        erpid            TYPE char5,  " ERP ID
        lblni            TYPE char10, " SES Number
        txz01            TYPE char40, " Short Text
        ebeln            TYPE char10, " PO Number
        ebelp            TYPE char5,  " PO Item Number
        loekz            TYPE char1,  " SES Deletion Indicator
        kzabn            TYPE char1,  " Acceptance Indicator
        lwert            TYPE char14, " Gross Amount
        waers            TYPE char5,  " Currency Key
        packno           TYPE char10, " Package Number
        serial_no        TYPE char2,  " Serial Number
        delete_ind       TYPE char1,  " Account Assignment Deletion Ind.
        percentage       TYPE char7,  " Percentage
        accass_val       TYPE char30, " Account Assignment Amount
        gl_account       TYPE char10, " G/L Account
        costcenter       TYPE char10, " Cost Center
        order            TYPE char12, " Order Number
        co_area          TYPE char4,  " Controlling Area
        prof_seg         TYPE char10, " Profitability Segment Number
        tax_code         TYPE char2,  " Tax Code
        inv_ind          TYPE char01, " Invoiced Indicator
*       status           TYPE char01, " Status
       END   OF gty_final.

TYPES:  gty_output       TYPE string,       "Output                    "

        gtt_output       TYPE STANDARD TABLE OF gty_output.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_delim         TYPE char2 VALUE '|~',
        gc_ucomm_onli    TYPE sscrfields-ucomm VALUE 'ONLI',
        gc_lgcfilpth     TYPE pathintern
                         VALUE 'ZFAPI124_SRVC_ENTRY_SHEET',
        gc_filename      TYPE fileintern
                         VALUE 'SSSSS_Srvc_Entry_YYMMDDHHMM_NNN.CSV'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_cn_recs_total TYPE i,
        gv_cn_recs_max   TYPE i,
        gv_cn_recs_file  TYPE i,
        gv_cn_files      TYPE i,
        gv_filename      TYPE text256,
        gv_filename_p    TYPE text256.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_essr          TYPE gtt_essr,          "Service Entry Sheet  "
        gt_output        TYPE gtt_output.        "Output               "
