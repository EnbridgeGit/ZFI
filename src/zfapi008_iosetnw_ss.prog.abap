*&---------------------------------------------------------------------*
*& Report  ZFAPI008_IOSETNW                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI008_IOSETNW                              *
*& Include Name       :  ZFAPI008_IOSETNW_SS                           *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  22-May-2020                                   *
*& Change Request     :  CHG0180384                                    *
*& Purpose            :  Extract Internal Order and Network Settlement *
*&                       data                                          *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 22-May-2020  AHMADT        D30K930537 CHG0180384 Initial Development*
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Selection Screen                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
SELECT-OPTIONS :  s_aufnr FOR aufk-aufnr.
SELECT-OPTIONS :  s_erdat FOR aufk-erdat.
SELECT-OPTIONS :  s_aedat FOR aufk-aedat.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t2.
SELECT-OPTIONS : s_pspnr FOR prps-pspnr.
SELECT-OPTIONS : s_perdat FOR prps-erdat.
SELECT-OPTIONS : s_paedat FOR prps-aedat.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE t3.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(09) text-001.
SELECTION-SCREEN: POSITION 33.
PARAMETERS:       p_objid  TYPE programm
                           VISIBLE LENGTH 22 OBLIGATORY.

SELECTION-SCREEN: POSITION 58.
PARAMETERS:       p_objttl TYPE sytitle
                           MODIF ID dsp.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_pres TYPE c RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND abcd,
            p_app  TYPE c RADIOBUTTON GROUP rb1.

SELECTION-SCREEN SKIP.

PARAMETERS:       p_ord_p TYPE localfile MODIF ID pre.
PARAMETERS:       p_ord_a TYPE localfile MODIF ID app.
PARAMETERS:       p_wbs_p TYPE localfile MODIF ID pre.
PARAMETERS:       p_wbs_a TYPE localfile MODIF ID app.
SELECTION-SCREEN END OF BLOCK b3.
