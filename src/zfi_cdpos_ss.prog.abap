*&---------------------------------------------------------------------*
*& Report  ZFI_CDPOS_SS                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_CDPOS_SS                                  *
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

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
SELECT-OPTIONS : s_objcls FOR cdhdr-objectclas NO INTERVALS,
                 s_objid  FOR cdhdr-objectid   NO INTERVALS,
                 s_chgnr  FOR cdhdr-changenr   NO INTERVALS,
                 s_udate  FOR cdhdr-udate      OBLIGATORY DEFAULT sy-datum.

PARAMETERS: p_pre_r TYPE c RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND abcd,
            p_app_r TYPE c RADIOBUTTON GROUP rb1.

PARAMETERS: p_app  TYPE text255  OBLIGATORY DEFAULT '/usr/sap/interfaces/sysid/Process_Mining/Out/MINING_PROJECT_DDMMYYYY_HHMMSS.txt'
                                           MODIF ID app,
            p_pres TYPE localfile  MODIF ID pre,
            p_rec  TYPE numc10 MODIF ID rec.
SELECTION-SCREEN END OF BLOCK b1.
