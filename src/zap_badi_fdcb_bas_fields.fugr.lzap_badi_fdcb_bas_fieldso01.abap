*&---------------------------------------------------------------------*
*&  Include           LZAP_BADI_FDCB_BAS_FIELDSO01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Func Group Name    :   ZAP_BADI_FDCB_BAS_FIELDS                      *
* Function Module    :   ZAP_BADI_FDCB_BAS_FIELDS                      *
* Function Include   :   LZAP_BADI_FDCB_BAS_FIELDSO01                  *
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
*&      Module  receive_data  OUTPUT
*&---------------------------------------------------------------------*
MODULE receive_data OUTPUT.

  IF o_badi_fdcb_subbas05 IS INITIAL.

    CALL METHOD cl_exithandler=>get_instance_for_subscreens
      CHANGING
        instance                      = o_badi_fdcb_subbas05
      EXCEPTIONS
        no_reference                  = 1
        no_interface_reference        = 2
        no_exit_interface             = 3
        data_incons_in_exit_managem   = 4
        class_not_implement_interface = 5
        OTHERS                        = 6.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

* object created  ?

  CHECK NOT o_badi_fdcb_subbas05 IS INITIAL.

* get data from main screen

  CALL METHOD o_badi_fdcb_subbas05->get_data_from_screen_object
    IMPORTING
      ex_invfo = invfo.

ENDMODULE.                 " receive_data  OUTPUT
