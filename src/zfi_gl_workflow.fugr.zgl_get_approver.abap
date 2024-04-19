FUNCTION ZGL_GET_APPROVER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ACTOR_TAB STRUCTURE  SWHACTOR
*"      AC_CONTAINER STRUCTURE  SWCONT
*"  EXCEPTIONS
*"      NOBODY_FOUND
*"----------------------------------------------------------------------
  INCLUDE <cntn01>.

  Data:  LT_ACTORS TYPE STANDARD TABLE OF SWHACTOR,
         WA_ACTOR TYPE SWHACTOR,
         LT_APPROVER LIKE SWHACTOR-OBJID.

  Data: USERID TYPE SWP_INITIA,
        USR(12) type C,
        W_APPEND TYPE C,
        COUNT TYPE I,
        FWDUSER TYPE SWP_INITIA,
        Loop TYPE SWP_PRIO,
        CHECKORG TYPE CHAR1,
        lta_users TYPE STANDARD TABLE OF agr_users,
        lst_users TYPE agr_users.

  CONSTANTS : c_o        TYPE plog-otype   VALUE 'O',
              c_x        TYPE hrrhdb-sort  VALUE 'X',
              c_s        TYPE plog-otype   VALUE 'S',
              c_a008     TYPE plog-subty   VALUE 'A008',
              c_a003     TYPE plog-subty   VALUE 'A003',
              c_B003     TYPE plog-subty   VALUE 'B003',
              c_plvar    TYPE plog-plvar   VALUE '01'.

  DATA: USR_POS LIKE HRP1001-OBJID,
        USR_ORG LIKE HRP1001-SOBID.

  DATA: BEGIN OF T_SOBID OCCURS 0,
        OBJID LIKE HRP1001-OBJID,
        SOBID LIKE HRP1001-SOBID,
        END OF T_SOBID.

  DATA: BEGIN OF T_NAMES OCCURS 0,
        OBJID LIKE HRP1001-OBJID,
        NAME LIKE HRP1000-MC_STEXT,
        END OF T_NAMES.

  DATA: BEGIN OF T_USERS OCCURS 0,
        SCLAS LIKE HRP1001-SCLAS,
        ID LIKE HRP1001-SOBID,
        END OF T_USERS.

  DATA: W_NAMES LIKE T_NAMES,
        W_OBJID LIKE HRP1001-OBJID.

  SWC_GET_ELEMENT AC_CONTAINER 'USER' USERID.
  SWC_GET_ELEMENT AC_CONTAINER 'LOOP' LOOP.
  SWC_GET_ELEMENT AC_CONTAINER 'FWDUSER' FWDUSER.
  SWC_GET_ELEMENT AC_CONTAINER 'CHECKORG' CHECKORG.

  IF CHECKORG = 'X'.
    USR = USERID+2(12).

    SELECT SINGLE OBJID FROM HRP1001 INTO USR_POS
      WHERE OTYPE = 'S' AND
            PLVAR = '01' AND
            BEGDA LE SY-DATUM AND
            ENDDA GE SY-DATUM AND
            SUBTY = C_A008 AND
            SOBID = USR.
    IF SY-SUBRC = 0.
      SELECT SINGLE SOBID FROM HRP1001 INTO USR_ORG
        WHERE OTYPE = 'S' AND
              OBJID = USR_POS AND
              PLVAR = '01' AND
              BEGDA LE SY-DATUM AND
              ENDDA GE SY-DATUM AND
              SUBTY = C_A003.
      IF SY-SUBRC = 0.
        SELECT OBJID SOBID FROM HRP1001 INTO TABLE T_SOBID
          WHERE OTYPE = 'O' AND
                OBJID = USR_ORG AND
                PLVAR = '01' AND
                BEGDA LE SY-DATUM AND
                ENDDA GE SY-DATUM AND
                SUBTY = C_B003.
        IF SY-SUBRC = 0.
          LOOP AT T_SOBID.
            W_OBJID = T_SOBID-SOBID.
            SELECT SINGLE OBJID MC_STEXT FROM HRP1000 INTO W_NAMES
              WHERE PLVAR = '01' AND
                    OTYPE = 'S' AND
                    OBJID = W_OBJID AND
                    BEGDA LE SY-DATUM AND
                    ENDDA GE SY-DATUM.
            APPEND W_NAMES TO T_NAMES.
          ENDLOOP.
          READ TABLE T_NAMES INTO W_NAMES WITH KEY OBJID = USR_POS.

          LOOP AT T_NAMES.
            CLEAR: W_APPEND, T_USERS.
            REFRESH T_USERS.
            SELECT SCLAS SOBID FROM HRP1001 INTO TABLE T_USERS
              WHERE OTYPE = 'S' AND
                    OBJID = T_NAMES-OBJID AND
                    PLVAR = '01' AND
                    BEGDA LE SY-DATUM AND
                    ENDDA GE SY-DATUM AND
                    SUBTY = C_A008.

            CASE T_NAMES-NAME.
              WHEN  'SR. MANAGER'.
                W_APPEND = 'X'.
              WHEN  'MANAGER'.
                IF W_NAMES-NAME = 'SR. MANAGER'.
                ELSE.
                  W_APPEND = 'X'.
                ENDIF.
              WHEN  'TEAM LEAD'.
                IF W_NAMES-NAME = 'SR. MANAGER'.
                ELSEIF W_NAMES-NAME = 'MANAGER'.
                ELSE.
                  W_APPEND = 'X'.
                ENDIF.
              WHEN 'ANALYST'.
                IF W_NAMES-NAME = 'SR. MANAGER'.
                ELSEIF W_NAMES-NAME = 'MANAGER'.
                ELSEIF W_NAMES-NAME = 'TEAM LEAD'.
                ELSEIF W_NAMES-NAME = 'ANALYST'.
                  W_APPEND = 'X'.
                ENDIF.
            ENDCASE.

            IF W_APPEND = 'X'.
              LOOP AT T_USERS.
                IF T_USERS-ID NE USR.
                  wa_actor-otype = 'US'.
                  WA_ACTOR-OBJID = T_USERS-ID.
                  APPEND WA_ACTOR TO LT_ACTORS.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
          ACTOR_TAB[] = LT_ACTORS[].
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    LT_APPROVER = FWDUSER+2(12).
    wa_actor-otype = 'US'.
    WA_ACTOR-OBJID = LT_APPROVER.
    APPEND WA_ACTOR TO LT_ACTORS.

    ACTOR_TAB[] = LT_ACTORS[].
  ENDIF.

  DESCRIBE TABLE ACTOR_TAB LINES COUNT.
  IF COUNT = 0.
    RAISE NOBODY_FOUND.
  ENDIF.




ENDFUNCTION.
