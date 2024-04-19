FUNCTION zfi_ap_vendorcontact_get_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_KNVK TYPE  FKNVK_TAB
*"----------------------------------------------------------------------
  TYPE-POOLS: szadr.
  DATA: ls_knvk   LIKE LINE OF t_knvk,
        ls_addr3  TYPE szadr_addr3_complete,
        ls_addr3t TYPE szadr_addr3_line,
        lt_addr3t LIKE TABLE OF ls_addr3t.

  LOOP AT t_knvk INTO ls_knvk.
    READ TABLE gt_contact INTO ls_contact WITH KEY parnr = ls_knvk-parnr.
    IF sy-subrc = 0.
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

      ls_knvk-namev = ls_addr3t-data-name_first.
      ls_knvk-name1 = ls_addr3t-data-name_last.
      MODIFY t_knvk FROM ls_knvk.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
