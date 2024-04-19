*&---------------------------------------------------------------------*
*&  Include           ZFAPI101_COMPANY_TOP
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI101_COMPANY                              *
* Incude Name        :   ZFAPI101_COMPANY_TOP                          *
* Author             :   Vijay Rajaputra                               *
* Date               :   11-Jan-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Company Codes Extraction  for IAP              *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 15-FEB-2018  VRAJAPUTRA  D30K928562 CHG0100805 - Initial development *
*                          D30K928598                                  *
*&---------------------------------------------------------------------*

* Tables
TABLES : t001,sscrfields.

* Global variables Declaration.
TYPES: BEGIN OF gty_bukrs,
       bukrs TYPE bukrs,
       butxt TYPE butxt,
       land1 TYPE land1,
       waers TYPE waers,
       opvar TYPE opvar,
       END OF gty_bukrs,

       BEGIN OF gty_data,
       col1 TYPE c LENGTH 5,  " ERP System
       col2 TYPE c LENGTH 4,  " Company Code
       col3 TYPE c LENGTH 25, " Name of Company Code or Company Description
       col4 TYPE c LENGTH 1,  " Status Disabled="0" / Active="1"
       col5 TYPE c LENGTH 3,  " Country Key
       col6 TYPE c LENGTH 5,  " Currency Key
       END OF gty_data,

       gtt_bukrs TYPE STANDARD TABLE OF gty_bukrs,
       gtt_data  TYPE STANDARD TABLE OF  gty_data.

DATA : gt_bukrs TYPE STANDARD TABLE OF gty_BUKRS,
       gt_data  TYPE STANDARD TABLE OF gty_data.

CONSTANTS : gc_x           TYPE c VALUE 'X',
            gc_ucomm_onli  TYPE sscrfields-ucomm VALUE 'ONLI'.
