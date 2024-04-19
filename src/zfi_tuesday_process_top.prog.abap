*&---------------------------------------------------------------------*
*&  Include           ZFI_TUESDAY_PROCESS_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_TUESDAY_PROCESS                           *
* Program Include    :   ZFI_TUESDAY_PROCESS_TOP                       *
* Author             :                                                 *
* Date               :   Oct 17, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   PRRW Tuesday Process Automation               *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 17-Oct-2018  CPALYAM     D30K929184-Initial development              *
*----------------------------------------------------------------------*

*=======================================================================
* GLOBAL TYPES
*=======================================================================

TYPES: gtt_evrun    TYPE STANDARD TABLE OF evrun_table.

*=======================================================================
* GLOBAL CONSTANTS
*=======================================================================
CONSTANTS:
        gc_x             TYPE c                  "True/False; Yes/No   "
                         VALUE 'X'.

*=======================================================================
* GLOBAL VARIABLES
*=======================================================================
DATA: gv_smtp_addr     TYPE ad_smtpadr.        "Email Address        "

*=======================================================================
* GLOBAL WORKAREAS
*=======================================================================
DATA: gs_nod          TYPE  evrun_table,
      gs_partial      TYPE  evrun_table,
      gs_bdcdata      TYPE  bdcdata.

*=======================================================================
* GLOBAL INTERNAL TABLES
*=======================================================================

DATA: gt_nod          TYPE gtt_evrun,
      gt_partial      TYPE gtt_evrun,
      gt_posted       TYPE gtt_evrun,
      gt_bdcdata      TYPE STANDARD TABLE OF bdcdata,
      gt_messages     TYPE STANDARD TABLE OF bdcmsgcoll.

* Copied for Standard
DATA: ptrv_perio_itab        LIKE ptrv_perio OCCURS 0
                             WITH HEADER LINE,
      ptrv_doc_hd_itab       LIKE ptrv_doc_hd OCCURS 0
                             WITH HEADER LINE,
      ptrv_doc_it_itab       LIKE ptrv_doc_it OCCURS 0,
      ptrv_doc_tax_itab      LIKE ptrv_doc_tax OCCURS 0.
DATA: result_table LIKE evrun_table OCCURS 100 WITH HEADER LINE.
