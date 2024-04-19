*----------------------------------------------------------------------*
* Report Name: ZFI_AUDIT_EXTRACT_SCR
* Author:	     KBANERJEE-Kaushiki Banerjee
* Date:	       December 4th,2018
*
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
* 04-DEC-2018  KBANERJEE   D30K929436  CHG0137915 - Initial Development*
* 22-JAN-2019  KBANERJEE   D30K929560  UAT issues resolution           *
*                          D30K929545                                  *
*                          D30K929519                                  *
*                          D30K929542                                  *
*                          D30K929483                                  *
*                          D30K929459                                  *
*                          D30K929696                                  *
* 14-FEB-2020 KBANERJEE   DECK920520  CHG0174059_DFCT0018156-Code fixes*
*                                     to audit extract programs        *
* 30-APR-2021 KOTTAPAN     D30K930935 CHG0206319 - Write BSEG,PRPS     *
*                          D30K931002 files to Application Server      *
************************************************************************
*selection screen-------------------------------------------------------
"Enter Table Name
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-h01.
PARAMETER: p_table TYPE ztabname AS LISTBOX VISIBLE LENGTH 200
                                 OBLIGATORY USER-COMMAND ucomm1.
SELECTION-SCREEN END OF BLOCK blk1.
"Select Options
SELECTION-SCREEN: BEGIN OF BLOCK blk2 WITH FRAME TITLE text-h02.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
SELECT-OPTIONS:  s_vbeln  FOR  lips-vbeln MODIF ID vbn,"Document Number
                 s_posnr  FOR  lips-posnr MODIF ID psn,"Item number
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
                 s_bukrs1 FOR  bkpf-bukrs MODIF ID bkr,"Company Code
                 s_gjahr1 FOR  bkpf-gjahr MODIF ID gjr,"Fiscal Year
                 s_monat1 FOR  bkpf-monat MODIF ID mnt,"Fiscal Period
                 s_budat  FOR  bkpf-budat MODIF ID bdt. " Added by KOTTAPAN for CHG0206319

SELECTION-SCREEN: END OF BLOCK blk2.
"Filter table values
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-h03.
PARAMETERS:     p_field1 TYPE shvalue_d LOWER CASE.
SELECT-OPTIONS: s_field1 FOR gv_fieldname.       "Field1
PARAMETERS:     p_field2 TYPE shvalue_d LOWER CASE.
SELECT-OPTIONS: s_field2 FOR gv_fieldname.       "Field2
PARAMETERS:     p_field3 TYPE shvalue_d LOWER CASE.
SELECT-OPTIONS: s_field3 FOR gv_fieldname.       "Field3
SELECTION-SCREEN END OF BLOCK blk3.
"Run Options
SELECTION-SCREEN: BEGIN OF BLOCK blk4 WITH FRAME TITLE text-h04.
PARAMETERS:rb_alv  RADIOBUTTON GROUP rad1 DEFAULT 'X'
                                          USER-COMMAND ucomm,
           rb_xls  RADIOBUTTON GROUP rad1 ,
           rb_mail RADIOBUTTON GROUP rad1.
PARAMETERS:       p_fpth1  TYPE text128    "File Path            "
                           MODIF ID xls DEFAULT 'H:/'.
PARAMETERS:       p_fnam1  TYPE text12     "File Name            "
                           MODIF ID xls.
PARAMETERS:       p_fext1  TYPE  char4     "File Extension       "
                           LOWER CASE  MODIF ID xls.
PARAMETERS:       p_fdlm1  TYPE char1      "File Delimiter       "
                           DEFAULT '|' MODIF ID xls.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
PARAMETERS:       p_dpbc   TYPE numc5      "Docs Per File Count  "
                           DEFAULT 2500 MODIF ID xls.
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
PARAMETERS:       p_fext2  TYPE  char4     "File Extension       "
                           LOWER CASE  MODIF ID mal.
PARAMETERS:       p_fdlm2  TYPE char1      "File Delimiter       "
                           DEFAULT '|' MODIF ID mal.
PARAMETERS:       p_email  TYPE ad_smtpadr
                           MODIF ID mal.   "Email ID
PARAMETERS:       p_dpbc1  TYPE numc5      "Docs Per Batch Count "
                           DEFAULT 2500 MODIF ID mal.
PARAMETERS:       p_chdr1  AS CHECKBOX DEFAULT 'X' MODIF ID xls.
"Column Header Record "
PARAMETERS:       p_chdr2  AS CHECKBOX DEFAULT 'X' MODIF ID mal.
"Column Header Record "
SELECTION-SCREEN: END   OF BLOCK blk4.
