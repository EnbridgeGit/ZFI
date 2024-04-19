FUNCTION Z_POPUP_TEXT_INPUT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SOURCETEXT) TYPE  CHAR80 OPTIONAL
*"     VALUE(TITEL) TYPE  CHAR_60 OPTIONAL
*"     VALUE(START_COLUMN) DEFAULT 25
*"     VALUE(START_ROW) DEFAULT 6
*"     VALUE(SELF_INCLUDE) TYPE  CHAR1 OPTIONAL
*"     VALUE(SOURCETEXT1) TYPE  CHAR80 OPTIONAL
*"     VALUE(HIDE) TYPE  CHAR1 OPTIONAL
*"     VALUE(TWOLINE) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(ANSWER) TYPE  SYUCOMM
*"     VALUE(USERROLE) TYPE  AGR_NAME
*"  TABLES
*"      T_EXCLUDE STRUCTURE  ZFIT_TNE_ADMIN OPTIONAL
*"      IM_ROLE STRUCTURE  AGR_LOGSYS OPTIONAL
*"  CHANGING
*"     VALUE(TARGETTEXT) TYPE  CHAR80 OPTIONAL
*"     VALUE(TARGETTEXT1) TYPE  CHAR80 OPTIONAL
*"----------------------------------------------------------------------

  DATA: x1 TYPE i VALUE 5,
        y1 TYPE i VALUE 5,
        x2 TYPE i VALUE 19,
        y2 TYPE i VALUE 15.

  x1 = start_column.
  x2 = x1       + 80.
  y1 = start_row.
  y2 = start_row + 2.

  gv_sourcetext = sourcetext.
  gv_sourcetext1 = sourcetext1.
  gv_targettext = targettext.
  gv_targettext1 = targettext1.
  gv_titel      = titel.
  gv_hide       = hide.
  GV_twoline    = twoline.
*  gv_role       = ROLE.

  IT_EXCLUDE[] = T_exclude[].

  IF gv_sourcetext = 'SELECT USER'.
    IF SELF_INCLUDE NE 'X'.
      wa_admin-tneadmin = sy-uname.
      append wa_admin to IT_EXCLUDE.
    ENDIF.
  ENDIF.

  clear R_role.
  refresh r_role.

  r_role-sign = 'I'.
  r_role-option = 'EQ'.
  Loop at IM_ROLE into IMP_Role.
    r_role-low = imp_role-agr_name.
    append r_role.
  ENDLOOP.

  CALL SCREEN 200 STARTING AT x1 y1 ENDING AT x2 y2.
*
  CASE ok_code.
    WHEN 'ENTE'.
      targettext = gv_targettext.
      targettext1 = gv_targetteXt1.

      Loop at IM_ROLE into IMP_Role.
        select single agr_name from agr_users into userrole
          where agr_name = IMP_Role-agr_name AND
                uname = gv_targettext.
        if sy-subrc = 0.
          exit.
        endif.
      ENDLOOP.
    WHEN 'CANC'.
      targettext = gv_targettext.
      targettext1 = gv_targetteXt1.
    WHEN OTHERS.
      CLEAR gv_targettext.
  ENDCASE.
  answer = ok_code.

ENDFUNCTION.

*----------------------------------------------------------------------*
*  MODULE STATUS_0200 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
*
  IF gv_sourcetext CS 'Comments' OR gv_sourcetext CS 'Reason'.
    SET PF-STATUS '0210'.
  ELSE.
    SET PF-STATUS '0200'.
  ENDIF.
  SET TITLEBAR '001' WITH gv_titel.
*
ENDMODULE.                 " STATUS_0200  OUTPUT

*----------------------------------------------------------------------*
*  MODULE USER_COMMAND_0200 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
*
  CASE ok_code.
    WHEN 'ENTE'.
      IF gv_targettext <> space.
        IF gv_sourcetext = 'SELECT USER'.
          IF not R_role[] is initial.
            select single uname from agr_users into w_bname
              where AGR_NAME in R_ROLE AND
                    uname = gv_targettext.
            IF sy-subrc = 0.
              read table it_exclude into wa_admin with key tneadmin = gv_targettext.
              if sy-subrc = 0.
                MESSAGE s000(ZFI_WORKFLOW) WITH 'Invalid User. Choose Another User.'.
              else.
                SET SCREEN 0.
                LEAVE SCREEN.
              ENDIF.
            ELSE.
              MESSAGE s000(ZFI_WORKFLOW) WITH 'User missing Security Role. Choose Another User.'.
            ENDIF.
          ELSE.
            select single bname from USR02 into w_bname
              where bname = gv_targettext.
            IF sy-subrc = 0.
              read table it_exclude into wa_admin with key tneadmin = gv_targettext.
              if sy-subrc = 0.
                MESSAGE s000(ZFI_WORKFLOW) WITH 'Invalid User. Choose Another User.'.
              else.
                SET SCREEN 0.
                LEAVE SCREEN.
              ENDIF.
            ELSE.
              MESSAGE s000(ZFI_WORKFLOW) WITH 'Invalid User ID. Use Drop-down for Selection.'.
            ENDIF.
          ENDIF.
        ELSE.
          SET SCREEN 0.
          LEAVE SCREEN.
        ENDIF.
      ELSE.
        MESSAGE s000(ZFI_WORKFLOW) WITH 'Please Enter Your Comments'(009).
      ENDIF.
      IF gv_targettext1 = space AND gv_must = 'X'.
        MESSAGE s000(ZFI_WORKFLOW) WITH 'Please Enter Your Comments'(009).
      ENDIF.
    WHEN 'CANC'.
      IF gv_targettext <> space.
        SET SCREEN 0.
        LEAVE SCREEN.
      ELSE.
        IF gv_sourcetext = 'SELECT USER'.
          SET SCREEN 0.
          LEAVE SCREEN.
        ELSE.
          MESSAGE s000(ZFI_WORKFLOW) WITH 'Please Enter Your Comments'(009).
        ENDIF.
      ENDIF.
      "    WHEN 'CANC'.
      "      SET SCREEN 0.
      "      LEAVE SCREEN.
    WHEN 'TCOP'.
      gv_targettext = gv_sourcetext.
  ENDCASE.
*
ENDMODULE.                    "USER_COMMAND_0200 INPUT

*----------------------------------------------------------------------*
*  MODULE GET_USER_ID INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE GET_USER_ID INPUT.
*
  DATA: l_user_address LIKE addr3_val,
        ind type i.
  DATA: it_return type STANDARD TABLE OF DDSHRETVAL,
        wa_return like line of it_return.

  DATA: it_role type STANDARD TABLE OF AGR_USERS.

  IF gv_sourcetext = 'SELECT USER'.

    if not r_role[] is initial.
      select * from AGR_USERS into table it_role
        where AGR_NAME in R_ROLE AND
              TO_DAT GE sy-datum.
    endif.

    IF IT_ROLE[] IS INITIAL.
      select bname from USR02 into CORRESPONDING FIELDS OF TABLE it_user
        where USTYP = 'A'.
    else.
      select bname from USR02 into CORRESPONDING FIELDS OF TABLE it_user
        for ALL ENTRIES IN IT_ROLE
        where bname = IT_ROLE-UNAME AND USTYP = 'A'.
    ENDIF.

    loop at it_user into wa_user.
      ind = sy-tabix.
      read table it_exclude into wa_admin with key tneadmin = wa_user-bname.
      if sy-subrc = 0.
        delete it_user index ind.
        continue.
      endif.

      CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
        EXPORTING
          user_name              = wa_user-bname
        IMPORTING
          user_address           = l_user_address
        EXCEPTIONS
          user_address_not_found = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0. EXIT. ENDIF.
      wa_user-name = l_user_address-name_text.
      modify it_user index ind from wa_user TRANSPORTING name.
    ENDLOOP.

    SORT it_user by bname ASCENDING.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD        = 'BNAME'
        WINDOW_TITLE    = 'User Records'
        VALUE_ORG       = 'S'
      TABLES
        VALUE_TAB       = it_user
        RETURN_TAB      = it_return
      EXCEPTIONS
        PARAMETER_ERROR = 1
        NO_VALUES_FOUND = 2
        OTHERS          = 3.

    READ TABLE it_return into wa_return index 1.
    gv_targettext = wa_return-fieldval.

  ENDIF.
*
ENDMODULE.                    "GET_USER_ID INPUT
