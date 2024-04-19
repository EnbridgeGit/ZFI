FUNCTION-POOL ZAP_BADI_FDCB_BAS_FIELDS.     "MESSAGE-ID ..

*&---------------------------------------------------------------------*
*&  Include           LZAP_BADI_FDCB_BAS_FIELDSTOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Func Group Name    :   ZAP_BADI_FDCB_BAS_FIELDS                      *
* Function Module    :   ZAP_BADI_FDCB_BAS_FIELDS                      *
* Function Include   :   LZAP_BADI_FDCB_BAS_FIELDSTOP                  *
* Author             :   Chaitanya Palyam                              *
* Date               :   Apr 11, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
*                                                                      *
* Purpose            :   MM MIRO RBKP Table Enhancements for IAP       *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 11-Apr-2018  CPALYAM     D30K928828  CHG0109436 Initial development  *
*----------------------------------------------------------------------*

TABLES: invfo.

CONSTANTS:
        exit_name_fdcb_subbas05   TYPE exit_def
                                  VALUE 'BADI_FDCB_SUBBAS05'.

DATA:   o_badi_fdcb_subbas05      TYPE REF TO if_ex_badi_fdcb_subbas05.
