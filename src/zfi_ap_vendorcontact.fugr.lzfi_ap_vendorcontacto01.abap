*----------------------------------------------------------------------*
***INCLUDE LZFI_AP_VENDORCONTACTO01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  GET_ALL_CONTACTS  OUTPUT
*&---------------------------------------------------------------------*
MODULE get_all_contacts OUTPUT.
  TYPE-POOLS szadr.
  DATA: ls_zvar   LIKE LINE OF gt_zvar,
        lv_id_c   TYPE bapi4003_1-objkey_c,
        lv_id_p   TYPE bapi4003_1-objkey_p,
        ls_addr3  TYPE szadr_addr3_complete,
        ls_addr3t TYPE szadr_addr3_line,
        lt_addr3t LIKE TABLE OF ls_addr3t.

  SELECT *
    FROM zvar
    INTO CORRESPONDING FIELDS OF TABLE gt_zvar
    WHERE programm = 'ZFI_AP_VENDORCONTACT'
      AND varname = 'CONTACT'
  .

  CLEAR gt_contact.



  LOOP AT gt_zvar INTO ls_zvar.
    CLEAR ls_contact.
    "Get data from Customer Master
    SELECT parnr abtnr pafkt
      INTO CORRESPONDING FIELDS OF ls_contact
      FROM knvk
      WHERE lifnr = gs_lfa1-lifnr
        AND abtnr = ls_zvar-value1
        AND pafkt = ls_zvar-value2
    .

      IF sy-subrc = 0.
        "Get Department description
        SELECT SINGLE vtext AS abtnrd
          FROM tsabt
          INTO CORRESPONDING FIELDS OF ls_contact
          WHERE spras = 'E'
            AND abtnr = ls_contact-abtnr
        .

        "Get Person type desciription
        SELECT SINGLE vtext AS pafktd
          FROM tpfkt
          INTO CORRESPONDING FIELDS OF ls_contact
          WHERE spras = 'E'
            AND pafkt = ls_contact-pafkt
        .


        "Get address Number
        lv_id_c = gs_lfa1-lifnr.
        lv_id_p = ls_contact-parnr.

        CALL FUNCTION 'BAPI_ADDRESSCONTPART_GETDETAIL'
          EXPORTING
            obj_type_p     = 'BUS1006001'
            obj_id_p       = lv_id_p
            obj_type_c     = 'KNA1'
            obj_id_c       = lv_id_c
            context        = '0005'      "obj_id_ext = ''
          IMPORTING
            address_number = ls_contact-addr
            person_number  = ls_contact-pers.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        "Get the name of the person
        CALL FUNCTION 'ADDR_PERS_COMP_GET_COMPLETE'
          EXPORTING
            addrnumber                    = ls_contact-addr
            "ADDRHANDLE                   =
            persnumber                    = ls_contact-pers
            "PERSHANDLE                   =
            "ARCHIVE_HANDLE               =
            "iv_current_comm_data          = 'X'
          IMPORTING
            addr3_complete                = ls_addr3
          EXCEPTIONS
            parameter_error               = 1
            address_not_exist             = 2
            person_not_exist              = 3
            internal_error                = 4
            wrong_access_to_archive       = 5
            OTHERS                        = 6
        .
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        lt_addr3t = ls_addr3-addr3_tab.

        READ TABLE lt_addr3t INTO ls_addr3t INDEX 1.

        ls_contact-fname = ls_addr3t-data-name_first.
        ls_contact-lname = ls_addr3t-data-name_last.

        APPEND ls_contact TO gt_contact.

      ENDIF.
    ENDSELECT.
  ENDLOOP.
  IF NOT LINES( gt_contact ) = 0.
    tbl_contact-lines = LINES( gt_contact ).
  ELSE.
    tbl_contact-lines = -1.
  ENDIF.
ENDMODULE.                 " STATUS_9001  OUTPUT
