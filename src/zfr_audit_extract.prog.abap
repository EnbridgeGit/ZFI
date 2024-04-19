*----------------------------------------------------------------------*
* Report Name: ZFR_AUDIT_EXTRACT
* Author:	     KBANERJEE-Kaushiki Banerjee
* Date:	       December 4th,2018
* Logical Database: NA
* SAPScript name:   NA
* Application Area: FI
* Description:  This tool will allow the user with the opportunity to
*               receive the extract for Audit related tables.The extract
*               can be obtained as online report,and can also be
*               downloaded to local PC as excel or other types of files.
*               This program also allows users to send mails to the
*               specific person and attaching the table extract in mail.
*               If no mail receipient is specifiedmail is sent to the
*               person who executes this program.
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 04-DEC-2018  KBANERJEE   D30K929436  CHG0137915 -Initial Development *
* 22-JAN-2019  KBANERJEE   D30K929560  UAT issues resolution           *
*                          D30K929545                                  *
*                          D30K929519                                  *
*                          D30K929542                                  *
*                          D30K929483                                  *
*                          D30K929459                                  *
*                          D30K929696                                  *
* 17-FEB-2020 KBANERJEE    D30K930437 CHG0174059_DFCT0018156-Code fixes*
*                                     to audit extract programs        *
* 30-APR-2021 KOTTAPAN     D30K930935 CHG0206319 - Write BSEG,PRPS     *
*                          D30K931002 files to Application Server      *
* 25-JUN-2021 DADIM        D30K931022 CHG0217805 - Write BKPF          *
*                                     files to Application Server      *
* 05-MAY-2022 DADIM        D30K932167 CHG0244956 - Write BSIS          *
*                                     files to Application Server      *
************************************************************************
REPORT  zfr_audit_extract MESSAGE-ID zfi01 NO STANDARD PAGE HEADING.

INCLUDE :zfi_audit_extract_top,
         zfi_audit_extract_scr,
         zfi_audit_extract_f01,
         zfi_audit_extract_evnt.
