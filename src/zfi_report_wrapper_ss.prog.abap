*&---------------------------------------------------------------------*
*& Report  ZFI_REPORT_WRAPPER_TOP_SS                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_REPORT_WRAPPER                            *
*& Include Name       :  ZFI_REPORT_WRAPPER_SS                         *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  19-Oct-2020                                   *
*& Change Request     :  CHG0191644                                    *
*& Purpose            :  Wrapper program to send FBL1N output in CSV   *
*&                       format as an email attachment                 *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 19-Oct 2020  AHMADT        CHG0191644 D30K930705 Initial Development*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
PARAMETERS: p_name      TYPE progname   OBLIGATORY,
            p_var       TYPE variant    OBLIGATORY.
SELECT-OPTIONS: s_mailto FOR gv_address OBLIGATORY NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.
