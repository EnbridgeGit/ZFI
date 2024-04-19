*----------------------------------------------------------------------*
* Report Name: ZFIR_PREMINSEXT
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
* 18-SEP-2019  SHAFFES     DECK920174  CHG0159701: Exclude negative    *
*                                      Amount to Premier Insights      *
*                                      vendor. GL Acc: 251040 related  *
*                                      data is ignored from extract    *
************************************************************************
REPORT zfir_preminsext NO STANDARD PAGE HEADING MESSAGE-ID zfi01.
INCLUDE:zfii_pieminsext_top,
        zfii_pieminsext_scr,
        zfii_pieminsext_f01,
        zfii_pieminsext_evnt.
