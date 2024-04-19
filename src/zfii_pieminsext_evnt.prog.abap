*----------------------------------------------------------------------*
* Report Name: ZFII_PIEMINSEXT_EVNT
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
* 22-OCT-2018  KBANERJEE    D30K929342  CHG0125087:Initial development *
*                                                                      *
************************************************************************
INITIALIZATION.
  PERFORM f_init_actions.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_screen_pbo.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_xlspth.
  PERFORM f_file_f4.

AT SELECTION-SCREEN.
  PERFORM f_reset_rbut.

START-OF-SELECTION.
  PERFORM f_process.

END-OF-SELECTION.
  PERFORM f_output.
