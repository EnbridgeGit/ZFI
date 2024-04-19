*&---------------------------------------------------------------------*
*&  Include           LZAP_BADI_FDCB_BAS_FIELDSI01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Func Group Name    :   ZAP_BADI_FDCB_BAS_FIELDS                      *
* Function Module    :   ZAP_BADI_FDCB_BAS_FIELDS                      *
* Function Include   :   LZAP_BADI_FDCB_BAS_FIELDSI01                  *
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

*&---------------------------------------------------------------------*
*&      Module  receive_actual_data  INPUT
*&---------------------------------------------------------------------*
MODULE receive_actual_data INPUT.

*  object created  ?

  CHECK NOT o_badi_fdcb_subbas05 IS INITIAL.

* get data from main screen

  CALL METHOD o_badi_fdcb_subbas05->get_data_from_screen_object
    IMPORTING
      ex_invfo = invfo.

ENDMODULE.                 " receive_actual_data  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CHECK NOT o_badi_fdcb_subbas05 IS INITIAL.

* put data to main screen

  CALL METHOD o_badi_fdcb_subbas05->put_data_to_screen_object
    EXPORTING
      im_invfo = invfo.

ENDMODULE.                 " user_command_0100  INPUT
