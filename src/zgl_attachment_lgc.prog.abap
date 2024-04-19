*&---------------------------------------------------------------------*
*&  Include           ZGL_ATTACHMENT_LGC
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZGL_ATTACHMENT                                 *
* Include            :  ZGL_ATTACHMENT_LGC                             *
* Author             :  Manvitha Dadi                                  *
* Date               :  28-Jul-2021                                    *
* Technical Contact  :  Ashok Madasu/Manvitha Dadi                     *
* Purpose            :  Attach any file from appplication server       *
*                       to FB03 Screen                                 *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 28-Jul-2021  DADIM         D30K931398 CHG0221895 - Create custom     *
*                                       tcode for FB03/FBV3 attachments*
*&---------------------------------------------------------------------*

DATA : ls_bkpf TYPE bkpf.

INITIALIZATION.

  PERFORM initialisation.

AT SELECTION-SCREEN.

  IF sscrfields-ucomm = gc_ucomm_onli.

    IF p_appl EQ gc_x. " If Application Server Selected
      IF p_file1 IS INITIAL.
        MESSAGE text-015 TYPE 'E'.
        CLEAR sscrfields-ucomm.
      ENDIF.
    ELSE.  " If Presentation Server Selected
      IF p_file2 IS INITIAL.
        MESSAGE text-015 TYPE 'E'.
        CLEAR sscrfields-ucomm.
      ENDIF.
    ENDIF.

    CLEAR : ls_bkpf.
    SELECT SINGLE * FROM bkpf INTO ls_bkpf
      WHERE bukrs = p_bukrs AND
            belnr = p_belnr AND
            gjahr = p_gjahr.
    IF sy-subrc <> 0.
      MESSAGE e000(zfi01) WITH text-004 p_belnr p_bukrs p_gjahr.
    ENDIF.

  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'M1'.
      IF p_pres = space.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 = 'M2'.
      IF p_appl = space .
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file1.

  PERFORM get_appl_path.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file2.

  PERFORM get_pres_path.


START-OF-SELECTION.

  IF p_dir IS NOT INITIAL.
    PERFORM upload_data.
  ELSE.
    PERFORM upload_net.
  ENDIF.
