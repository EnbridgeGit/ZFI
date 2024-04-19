*&---------------------------------------------------------------------*
*&  Include           ZXAUFU01
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_ACTVT) LIKE  TACT-ACTVT
*"             VALUE(I_AUFK) LIKE  AUFK STRUCTURE  AUFK
*"       EXCEPTIONS
*"              E_MESSAGE
*"----------------------------------------------------------------------

DATA: lv_paramtype TYPE zparamtype,
      lv_rfc_dest  TYPE tb_rfcdest.

IF i_actvt = '32'. "SAVE

  CASE i_aufk-auart.
    WHEN 'IC01'.

      "VALIDATE System field
      IF i_aufk-user0 <> 'US' AND i_aufk-user0 <> 'SW'.
        MESSAGE text-001 TYPE 'E'.
      ENDIF.
      "VALIDATE company code
      IF i_aufk-user1 IS INITIAL.
        MESSAGE text-002 TYPE 'E'.
      ENDIF.

      "VALIDATE object type
      IF    i_aufk-user2 <> 'PR' AND i_aufk-user2 <> 'NA'
        AND i_aufk-user2 <> 'CC' AND i_aufk-user2 <> 'IO'.
        MESSAGE text-003 TYPE 'E'.
      ENDIF.

      "VALIDATE Object number
      IF i_aufk-user3 IS INITIAL.
        MESSAGE text-004 TYPE 'E'.
      ENDIF.


      CONCATENATE 'ECC' i_aufk-user0(2) INTO lv_paramtype.

      CALL FUNCTION 'ZFI_GET_RFC_DEST'
        EXPORTING
          imp_paramtype = lv_paramtype
        IMPORTING
          exp_rfcdest   = lv_rfc_dest.

      IF lv_rfc_dest IS INITIAL.
        MESSAGE text-005 TYPE 'E'.
      ENDIF.

      "Call RFC to validate
      CALL FUNCTION 'ZFI_IC_INTORD_VALIDATION' DESTINATION lv_rfc_dest
        EXPORTING
          im_cocode            = i_aufk-user1
          im_objtype           = i_aufk-user2
          im_objnumber         = i_aufk-user3
        EXCEPTIONS
          does_not_exist       = 1
          object_locked        = 2
          object_deleted       = 3
          object_not_released  = 4
          invalid_company_code = 5
          OTHERS               = 6.
      IF sy-subrc = 1.
        MESSAGE text-e01 type 'E'.
      ELSEIF sy-subrc = 2.
        MESSAGE text-e02 TYPE 'E'.
      ELSEIF sy-subrc = 3.
        MESSAGE text-e03 TYPE 'E'.
      ELSEIF sy-subrc = 4.
        MESSAGE text-e04 TYPE 'E'.
      ELSEIF sy-subrc = 5.
        MESSAGE text-e05 TYPE 'E'.
      ELSEIF sy-subrc = 6.
        MESSAGE text-e06 TYPE 'E'.
      ENDIF.
    WHEN OTHERS.
      "Do nothing.
  ENDCASE.
ENDIF.
