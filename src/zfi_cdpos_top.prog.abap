*&---------------------------------------------------------------------*
*& Report  ZFI_CDPOS_TOP                                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_CDPOS_TOP                                 *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  17-Dec-2020                                   *
*& Change Request     :  CHG0199398                                    *
*& Purpose            :  Process Mining Report                         *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 17-Dec-2020  AHMADT        D30K930775 CHG0199398 Initial Development*
*&                                                                     *
*&---------------------------------------------------------------------*

*& --------------------------------------------------------------------*
*& Global Types                                                        *
*&                                                                     *
*&---------------------------------------------------------------------*

TABLES: cdhdr.

TYPES: BEGIN OF gty_cdhdr,
       objectclas   TYPE cdobjectcl,
       objectid     TYPE cdobjectv,
       changenr     TYPE cdchangenr,
       udate        TYPE cddatum,
       END OF gty_cdhdr.

TYPES: BEGIN OF gty_cdpos,
       objectclas   TYPE cdobjectcl,
       objectid     TYPE cdobjectv,
       changenr     TYPE cdchangenr,
       tabname      TYPE tabname,
       tabkey       TYPE cdtabkey,
       fname        TYPE fieldname,
       chngind      TYPE cdchngind,
       value_new    TYPE cdfldvaln,
       value_old    TYPE cdfldvalo,
       END OF gty_cdpos.

TYPES: BEGIN OF gty_final,
       udate        TYPE cddatum,
       objectclas   TYPE cdobjectcl,
       objectid     TYPE cdobjectv,
       changenr     TYPE cdchangenr,
       tabname      TYPE tabname,
       tabkey       TYPE cdtabkey,
       fname        TYPE fieldname,
       chngind      TYPE cdchngind,
       value_new    TYPE cdfldvaln,
       value_old    TYPE cdfldvalo,
       END OF gty_final.

*& --------------------------------------------------------------------*
*& Internal Tables                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

DATA: gt_cdhdr TYPE STANDARD TABLE OF gty_cdhdr,
      gt_cdpos TYPE STANDARD TABLE OF gty_cdpos,
      gt_final TYPE STANDARD TABLE OF gty_final.

*& --------------------------------------------------------------------*
*& Work Areas                                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
DATA: gs_cdhdr TYPE gty_cdhdr,
      gs_cdpos TYPE gty_cdpos,
      gs_final TYPE gty_final.

*& --------------------------------------------------------------------*
*& Variables                                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
DATA: gv_num_records   TYPE numc10 VALUE 0,
      gv_file_count    TYPE char3 VALUE '1',
      gv_file_ext      TYPE char7,
      gv_file_ext_last TYPE char7 VALUE '.txt',
      gv_path          TYPE text255.

*& --------------------------------------------------------------------*
*& Constants                                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
CONSTANTS: gc_sysid         TYPE char5 VALUE 'sysid'.
