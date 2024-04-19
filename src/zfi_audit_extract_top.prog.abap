*----------------------------------------------------------------------*
* Report Name: ZFI_AUDIT_EXTRACT_TOP
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
* 04-DEC-2018  KBANERJEE   D30K929436  CHG0137915 -Initial Development *
* 22-JAN-2019  KBANERJEE   D30K929560  UAT issues resolution           *
*                          D30K929545                                  *
*                          D30K929519                                  *
*                          D30K929542                                  *
*                          D30K929483                                  *
*                          D30K929459                                  *
*                          D30K929696                                  *
* 14-FEB-2020 KBANERJEE   DECK920520  CHG0174059_DFCT0018156-Code fixes*
*                                     to audit extract programs        *
* 30-APR-2021 KOTTAPAN     D30K930935 CHG0206319 - Write BSEG, PRPS    *
*                          D30K930978 files to Application Server      *
*                          D30K931002                                  *
* 07-Jun-2021 KOTTAPAN     D30K931022 CHG0216879 Increase field length *
*                        for file headings & write BKPF to Appl server *
************************************************************************
*Data Declaration------------------------------------------------------
TABLES:bkpf,lips.
*Type pools declaration for ALV
TYPE-POOLS: slis.                    " ALV Global Types
*  Object declaration
DATA: o_alvgd              TYPE REF TO cl_gui_alv_grid,
"ALV grid object
      o_dockingcontainer   TYPE REF TO cl_gui_docking_container.
"docking container
*data declaration for dynamic internal table and alv
TYPES:BEGIN OF gty_field_pos,
       position   TYPE tabfdpos,
       fieldname  TYPE fieldname,
      END OF gty_field_pos,
      BEGIN OF gty_wh_fields.
        INCLUDE TYPE fieldnames.
TYPES:range_num TYPE i,
      END OF gty_wh_fields.
TYPES:BEGIN OF gty_tabf4,
      fieldname   TYPE fieldname,
      domname     TYPE domname,
      description TYPE shvalue_d,"as4text,
END OF gty_tabf4.
TYPES:BEGIN OF gty_fieldnames,
       line(50) TYPE c,
     END OF gty_fieldnames,
     BEGIN OF  gty_output ,
       line(5000),
     END OF gty_output.
*Global table types
TYPES:tt_field_pos   TYPE STANDARD TABLE OF gty_field_pos
                     INITIAL SIZE 0,
      tt_ddfields    TYPE STANDARD TABLE OF dfies
                     INITIAL SIZE 0,
      tt_wh_fields   TYPE STANDARD TABLE OF gty_wh_fields
                     INITIAL SIZE 0,
      tt_tabf4       TYPE STANDARD TABLE OF gty_tabf4
                     INITIAL SIZE 0,
      tt_output      TYPE STANDARD TABLE OF gty_output
                     INITIAL SIZE 0,
      tt_fieldnames  TYPE STANDARD TABLE OF gty_fieldnames
                     INITIAL SIZE 0.
*field symbols declaration
FIELD-SYMBOLS <fs_output> TYPE STANDARD TABLE.
DATA:gv_fieldname      TYPE fieldname,
     gv_field_sequence TYPE char90,
     gv_pack_size      TYPE i.
DATA:gs_ddfields       TYPE dfies.
DATA:gr_field1 TYPE STANDARD TABLE OF rsdsselopt INITIAL SIZE 0.
DATA:gr_field2 TYPE STANDARD TABLE OF rsdsselopt INITIAL SIZE 0.
DATA:gr_field3 TYPE STANDARD TABLE OF rsdsselopt INITIAL SIZE 0.
DATA:gr_field4 TYPE STANDARD TABLE OF rsdsselopt INITIAL SIZE 0.
DATA:gr_field5 TYPE STANDARD TABLE OF rsdsselopt INITIAL SIZE 0.
DATA:gr_field6 TYPE STANDARD TABLE OF rsdsselopt INITIAL SIZE 0.
DATA:gr_field7 TYPE STANDARD TABLE OF rsdsselopt INITIAL SIZE 0.
DATA : gt_fieldlist    TYPE STANDARD TABLE OF gty_field_pos
                       INITIAL SIZE 0,
       gt_ddfields     TYPE STANDARD TABLE OF dfies
                       INITIAL SIZE 0,
       gt_selfld_value TYPE STANDARD TABLE OF ddshretval
                       INITIAL SIZE 0,
       gt_wh_fields    TYPE STANDARD TABLE OF gty_wh_fields
                       INITIAL SIZE 0,
       gt_output       TYPE STANDARD TABLE OF gty_output
                       INITIAL SIZE 0,
       gt_tabf4        TYPE STANDARD TABLE OF gty_tabf4
                       INITIAL SIZE 0.
DATA:gt_inttab TYPE REF TO data.
DATA:   gv_file_size     TYPE syindex,         "File Size            "
        gv_file_size_brk TYPE syindex,         "File Size-ControlBreak
        gv_table         TYPE tabname,
        gv_desc          TYPE char90,
        gv_field1        TYPE fieldname,
        gv_field2        TYPE fieldname,
        gv_field3        TYPE fieldname,
        gv_ext2          TYPE char3,
        gv_ext3          TYPE char4,
        gv_attach_length TYPE i.

DATA:gt_fieldcat TYPE lvc_t_fcat.
*  Global work area declaration
CONSTANTS:gc_x     TYPE char1  VALUE 'X',
          gc_bukrs TYPE char30 VALUE 'BUKRS',
          gc_gjahr TYPE char30 VALUE 'GJAHR',
          gc_monat TYPE char30 VALUE 'MONAT',
          gc_spras TYPE char30 VALUE 'SPRAS',
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
          gc_vbeln TYPE char30 VALUE 'VBELN',
          gc_posnr TYPE char30 VALUE 'POSNR',
*END OF CHANGES BY KBANERJEE FOR CHG0174059_DFCT0018156
          gc_bseg  TYPE ddobjname  VALUE 'BSEG', " Added by KOTTAPAN for CHG0206319
          gc_prps  TYPE ddobjname  VALUE 'PRPS', " Added by KOTTAPAN for CHG0206319
          gc_bkpf  TYPE ddobjname  VALUE 'BKPF', " Added by KOTTAPAN for CHG0217805
          gc_bsis  TYPE ddobjname  VALUE 'BSIS'. "Added by DADIM for CHG0244956
DATA:o_extract TYPE REF TO zfi_audit_extract.
