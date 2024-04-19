*&---------------------------------------------------------------------*
*&  Include           ZCJAL_F01
*&---------------------------------------------------------------------*

DATA : lt_posid TYPE TABLE OF ps_posid,
       ls_posid TYPE ps_posid.
RANGES : lr_posid FOR prps-posid.
FIELD-SYMBOLS:<lt_del> TYPE ANY TABLE,
                 <fs_del> TYPE any,
                 <lv_msg>   TYPE any.
DATA:lr_del TYPE REF TO data.
CLEAR : lt_posid,ls_posid.

START-OF-SELECTION.

  SELECT posid FROM prps INTO TABLE lt_posid
       WHERE posid IN s_posid AND
             erdat IN s_date AND
             pkokr IN s_kokrs.

  IF sy-subrc = 0.
    LOOP AT lt_posid INTO ls_posid.
      lr_posid(3) = 'IEQ'.
      lr_posid-low = ls_posid.
      APPEND lr_posid.
      CLEAR : ls_posid.
    ENDLOOP.

    SUBMIT rcj_send_project_ale EXPORTING LIST TO MEMORY
                                 WITH kokrs IN s_kokrs
                                 WITH pspid IN lr_posid
                                 WITH recsyst = p_recsys AND RETURN.
    IF sy-subrc = 0.
      WRITE :/ text-002.
    ELSE.
      WRITE :/ text-003.
    ENDIF.

  ELSE.
    MESSAGE text-001 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
