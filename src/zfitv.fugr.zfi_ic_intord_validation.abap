FUNCTION zfi_ic_intord_validation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_COCODE) TYPE  AUFK-USER1
*"     VALUE(IM_OBJTYPE) TYPE  AUFK-USER2
*"     VALUE(IM_OBJNUMBER) TYPE  AUFK-USER3
*"  EXCEPTIONS
*"      DOES_NOT_EXIST
*"      OBJECT_LOCKED
*"      OBJECT_DELETED
*"      OBJECT_NOT_RELEASED
*"      INVALID_COMPANY_CODE
*"----------------------------------------------------------------------
  DATA: lv_bukrs TYPE bukrs,

        "NA
        lv_aufnrn TYPE afko-aufnr,
        lv_vornr  TYPE afvc-vornr,
        lv_aufpl  TYPE afko-aufpl,

        "CC
        lv_kostl TYPE csks-kostl,

        "IO
        lv_aufnr TYPE aufk-aufnr,
        lv_objnr TYPE jest-objnr,

        "PR
        lv_poski TYPE prps-poski.


  "Validate Company code.
  lv_bukrs  = im_cocode.

  SELECT SINGLE bukrs
    FROM t001
    INTO lv_bukrs
    WHERE bukrs = lv_bukrs.

  IF sy-subrc <> 0.
    RAISE invalid_company_code.
  ENDIF.



  CASE im_objtype.

    WHEN 'NA'.

      SPLIT im_objnumber AT '.' INTO lv_aufnrn lv_vornr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_aufnrn
        IMPORTING
          output = lv_aufnrn.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_vornr
        IMPORTING
          output = lv_vornr.

      SELECT SINGLE aufnr aufpl
        FROM afko
        INTO (lv_aufnrn,lv_aufpl)
        WHERE aufnr = lv_aufnrn.

      IF sy-subrc <> 0.
        RAISE does_not_exist.
      ENDIF.

      SELECT SINGLE vornr
        FROM afvc
        INTO lv_vornr
        WHERE aufpl = lv_aufpl.

      IF sy-subrc <> 0.
        RAISE does_not_exist.
      ENDIF.

    WHEN 'CC'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = im_objnumber
        IMPORTING
          output = lv_kostl.

      "Check if this CC exists
      SELECT SINGLE  kostl
        FROM csks
        INTO lv_kostl
        WHERE kostl = lv_kostl
          AND bukrs = lv_bukrs
          AND kokrs = '10' "SW 01 UG 10 US SECA
          AND datbi >= sy-datum
          AND datab <= sy-datum.

      IF sy-subrc <> 0.
        RAISE does_not_exist.
      ENDIF.


      "Check if this CC is locked
      SELECT SINGLE  kostl
        FROM csks
        INTO lv_kostl
        WHERE kostl = lv_kostl
          AND bukrs = lv_bukrs
          AND kokrs = '10' "SW 01 UG 10 US SECA
          AND datbi >= sy-datum
          AND datab <= sy-datum
          AND bkzkp = ''.

      IF sy-subrc <> 0.
        RAISE object_locked.
      ENDIF.

    WHEN 'IO'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = im_objnumber
        IMPORTING
          output = lv_aufnr.

      "Check if this IO exists
      SELECT SINGLE  aufnr
        FROM aufk
        INTO lv_aufnr
        WHERE aufnr = lv_aufnr
          AND bukrs = lv_bukrs.

      IF sy-subrc <> 0.
        RAISE does_not_exist.
      ENDIF.

      "Check if this IO is deleted
      SELECT SINGLE  aufnr objnr
        FROM aufk
        INTO (lv_aufnr,lv_objnr)
        WHERE aufnr = lv_aufnr
          AND bukrs = lv_bukrs
          AND loekz = ''.

      IF sy-subrc <> 0.
        RAISE object_deleted.
      ENDIF.


      "Check valid status
      SELECT SINGLE objnr
        FROM jest
        INTO lv_objnr
        WHERE objnr = lv_objnr
          AND inact = ''
          AND ( stat = 'I0002'
             OR stat = 'I0045' ).

      IF sy-subrc <> 0.
        RAISE object_not_released.
      ENDIF.

      "Check if status is locked
      SELECT SINGLE objnr
        FROM jest
        INTO lv_objnr
        WHERE objnr = lv_objnr
          AND inact = ''
          AND ( stat = 'I0043'
             OR stat = 'I0076' ).

      IF sy-subrc = 0.
        RAISE object_locked.
      ENDIF.

    WHEN 'PR'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = im_objnumber
        IMPORTING
          output = lv_poski.

      "Check if this WBS exists
      SELECT SINGLE  post1 objnr
        FROM prps
        INTO (lv_poski,lv_objnr)
        WHERE poski = lv_poski
          AND pbukr = lv_bukrs.

      IF sy-subrc <> 0.
        RAISE does_not_exist.
      ENDIF.

      "Check valid status
      SELECT SINGLE objnr
        FROM jest
        INTO lv_objnr
        WHERE objnr = lv_objnr
          AND inact = ''
          AND ( stat = 'I0002'
             OR stat = 'I0045' ).

      IF sy-subrc <> 0.
        RAISE object_not_released.
      ENDIF.

      "Check if status is locked
      SELECT SINGLE objnr
        FROM jest
        INTO lv_objnr
        WHERE objnr = lv_objnr
          AND inact = ''
          AND stat = 'I0043'.

      IF sy-subrc = 0.
        RAISE object_locked.
      ENDIF.

      "Check if status is deleted
      SELECT SINGLE objnr
        FROM jest
        INTO lv_objnr
        WHERE objnr = lv_objnr
          AND inact = ''
          AND stat = 'I0076'.

      IF sy-subrc = 0.
        RAISE object_deleted.
      ENDIF.

  ENDCASE.



ENDFUNCTION.
