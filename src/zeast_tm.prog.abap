*&---------------------------------------------------------------------*
*& Report  ZEAST_TM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZEAST_TM.

PARAMETERS: p_view type DD02V-TABNAME.

IF not p_view cp 'Z*'.
  message 'you are not authorized' type 'E'.
ENDIF.
CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
  EXPORTING
    ACTION                               = 'U'
*   CORR_NUMBER                          = '          '
*   GENERATE_MAINT_TOOL_IF_MISSING       = ' '
*   SHOW_SELECTION_POPUP                 = ' '
    VIEW_NAME                            = p_view
*   NO_WARNING_FOR_CLIENTINDEP           = ' '
*   RFC_DESTINATION_FOR_UPGRADE          = ' '
*   CLIENT_FOR_UPGRADE                   = ' '
*   VARIANT_FOR_SELECTION                = ' '
*   COMPLEX_SELCONDS_USED                = ' '
*   CHECK_DDIC_MAINFLAG                  = ' '
*   SUPPRESS_WA_POPUP                    = ' '
* TABLES
*   DBA_SELLIST                          =
*   EXCL_CUA_FUNCT                       =
* EXCEPTIONS
*   CLIENT_REFERENCE                     = 1
*   FOREIGN_LOCK                         = 2
*   INVALID_ACTION                       = 3
*   NO_CLIENTINDEPENDENT_AUTH            = 4
*   NO_DATABASE_FUNCTION                 = 5
*   NO_EDITOR_FUNCTION                   = 6
*   NO_SHOW_AUTH                         = 7
*   NO_TVDIR_ENTRY                       = 8
*   NO_UPD_AUTH                          = 9
*   ONLY_SHOW_ALLOWED                    = 10
*   SYSTEM_FAILURE                       = 11
*   UNKNOWN_FIELD_IN_DBA_SELLIST         = 12
*   VIEW_NOT_FOUND                       = 13
*   MAINTENANCE_PROHIBITED               = 14
*   OTHERS                               = 15
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.
