*----------------------------------------------------------------------*
* Report Name: ZFII_PIEMINSEXT_SCR
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
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t16.
SELECT-OPTIONS:s_crtdt FOR gv_crdt OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk1.
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-t17.
PARAMETERS:cb_extrc AS CHECKBOX DEFAULT space USER-COMMAND com.
PARAMETERS:rb_alv RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND com,
           rb_xls RADIOBUTTON GROUP rad1 MODIF ID exr,
           rb_app RADIOBUTTON GROUP rad1 MODIF ID exr.
PARAMETERS:p_xlspth  TYPE rlgrap-filename MODIF ID xls DEFAULT 'C:',
           p_apppth  TYPE rlgrap-filename MODIF ID app.
SELECTION-SCREEN END OF BLOCK blk2.
