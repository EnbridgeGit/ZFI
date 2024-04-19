*&---------------------------------------------------------------------*
*&      Form  FICO_AUTHORIZATION_CHECK
*&---------------------------------------------------------------------*
FORM FICO_AUTHORIZATION_CHECK.

DATA: L_MSGV1 TYPE MSGV1,
      L_pspnr TYPE STANDARD TABLE OF ZPS_Pspnr_RANGE WITH HEADER LINE,
      prev_posid like prps-posid.

*- Start of Ins. -> C11K918743
*  CHECK NOT ( S_BUKRS[] IS INITIAL ) OR
*        NOT ( S_KOSTL[] IS INITIAL ) OR
*        NOT ( S_CCGRP[] IS INITIAL ) OR
*        NOT ( S_AUFNR[] IS INITIAL ).

*- Authorization Check on Company Code range
   IF ( NOT ( S_BUKRS[] IS INITIAL ) ) OR
      ( S_BUKRS[] IS INITIAL AND
        S_KOSTL[] IS INITIAL AND
        S_CCGRP[] IS INITIAL AND
        S_AUFNR[] IS INITIAL ).

      CALL FUNCTION 'Z_FICO_CUSTOM_RPT_AUTH_CHK'
         EXPORTING
           CHK_BUKRS                     = 'X'
         IMPORTING
           MSGV1                         = L_MSGV1
         TABLES
           R_BUKRS                       = S_BUKRS
           R_KOSTL                       = S_KOSTL
           R_CCGRP                       = S_CCGRP
           R_AUFNR                       = S_AUFNR
         EXCEPTIONS
           AUTH_CHECK_FAILED_BUKRS       = 1
           AUTH_CHECK_FAILED_KOSTL       = 2
           AUTH_CHECK_FAILED_CCGRP       = 3
           AUTH_CHECK_FAILED_AUFNR       = 4
           AUTH_CHECK_FAILED_PRART       = 5
           OTHERS                        = 6.

      IF SY-SUBRC NE 0.
         MESSAGE I135(ZA) WITH L_MSGV1.
         STOP.
      ENDIF.

   ENDIF.

*- Authorization Check on Cost Center/Cost Center Group range
   IF NOT ( S_KOSTL[] IS INITIAL ) OR
      NOT ( S_CCGRP[] IS INITIAL ).

      CALL FUNCTION 'Z_FICO_CUSTOM_RPT_AUTH_CHK'
         EXPORTING
           CHK_KOSTL                     = 'X'
         IMPORTING
           MSGV1                         = L_MSGV1
         TABLES
           R_BUKRS                       = S_BUKRS
           R_KOSTL                       = S_KOSTL
           R_CCGRP                       = S_CCGRP
           R_AUFNR                       = S_AUFNR
         EXCEPTIONS
           AUTH_CHECK_FAILED_BUKRS       = 1
           AUTH_CHECK_FAILED_KOSTL       = 2
           AUTH_CHECK_FAILED_CCGRP       = 3
           AUTH_CHECK_FAILED_AUFNR       = 4
           AUTH_CHECK_FAILED_PRART       = 5
           OTHERS                        = 6.

      IF SY-SUBRC NE 0.
         MESSAGE I135(ZA) WITH L_MSGV1.
         STOP.
      ENDIF.

   ENDIF.

*- Authorization Check on Order range
   IF NOT ( S_AUFNR[] IS INITIAL ).

      CALL FUNCTION 'Z_FICO_CUSTOM_RPT_AUTH_CHK'
         EXPORTING
           CHK_AUFNR                     = 'X'
         IMPORTING
           MSGV1                         = L_MSGV1
         TABLES
           R_BUKRS                       = S_BUKRS
           R_KOSTL                       = S_KOSTL
           R_CCGRP                       = S_CCGRP
           R_AUFNR                       = S_AUFNR
         EXCEPTIONS
           AUTH_CHECK_FAILED_BUKRS       = 1
           AUTH_CHECK_FAILED_KOSTL       = 2
           AUTH_CHECK_FAILED_CCGRP       = 3
           AUTH_CHECK_FAILED_AUFNR       = 4
           AUTH_CHECK_FAILED_PRART       = 5
           OTHERS                        = 6.

      IF SY-SUBRC NE 0.
         MESSAGE I135(ZA) WITH L_MSGV1.
         STOP.
      ENDIF.

   ENDIF.

ENDFORM.                    " FICO_AUTHORIZATION_CHECK
