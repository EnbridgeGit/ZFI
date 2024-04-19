*&---------------------------------------------------------------------*
*&  Include           ZFINSYNC_SCHEDULE_SCN
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFINSYNC_SCHEDULE                             *
* Include            :   ZFINSYNC_SCHEDULE_SCN                         *
* Author             :   AKMADASU                                      *
* Date               :   Jan 23, 2022                                  *
* Technical Contact  :   Ashok Madasu                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  To Schedule Finance Sync IDOC Process, PGI AMD *
*                    :  Billing Jobs paralelly in BACKGROUND           *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 23-Jan-2022  AKMADASU    D30K931986-Initial development              *
*----------------------------------------------------------------------*

* Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS docnum FOR edidc-docnum.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: credat FOR edidc-credat,
                cretim FOR edidc-cretim DEFAULT '000000' TO '235959'.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS status FOR edidc-status.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: mestyp FOR edidc-mestyp, "bdfields-mestyp,
                mescod FOR edidc-mescod,
                mesfct FOR edidc-mesfct.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: sndprt FOR edidc-sndprt,
                sndprn FOR edidc-sndprn,
                sndpfc FOR edidc-sndpfc.
SELECTION-SCREEN SKIP 1.
PARAMETERS p_pcksiz LIKE edp13-pcksiz DEFAULT 5.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS test FOR edidc-test NO-EXTENSION.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE TEXT-ti2.
SELECT-OPTIONS obj_type FOR tbd55-obj_type.
SELECTION-SCREEN END   OF BLOCK block2.
SELECTION-SCREEN BEGIN OF BLOCK blockas WITH FRAME TITLE TEXT-tas.
PARAMETERS: p_output TYPE alelist DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK blockas.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
*SELECTION-SCREEN BEGIN OF BLOCK 4 WITH FRAME TITLE TEXT-003.
***PARAMETERS:p_tpackt TYPE syst_tabix
***           DEFAULT '1000000' OBLIGATORY.
*PARAMETERS:p_packet TYPE sytabix
*           DEFAULT '100000' OBLIGATORY.
*SELECTION-SCREEN END OF  BLOCK 4."

SELECTION-SCREEN BEGIN OF BLOCK 6 WITH FRAME TITLE TEXT-004.
PARAMETERS  : p_sergrp  LIKE bdfields-rfcgr DEFAULT ' ' OBLIGATORY.
PARAMETERS:   p_totjob(2) TYPE n DEFAULT '10' OBLIGATORY.
PARAMETERS:   p_percnt(2) TYPE n DEFAULT '10' OBLIGATORY.
SELECTION-SCREEN END OF  BLOCK 6."
SELECTION-SCREEN END   OF BLOCK b2.
