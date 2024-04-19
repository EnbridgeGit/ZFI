FUNCTION ZTR_TRANSACTION_DATA_VIEW.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  T_BUKRS OPTIONAL
*"     REFERENCE(I_RFHA) TYPE  ZLT_RFHA OPTIONAL
*"     REFERENCE(I_REQUNR) TYPE  SBIWA_S_INTERFACE-REQUNR OPTIONAL
*"     REFERENCE(I_ISOURCE) TYPE  SBIWA_S_INTERFACE-ISOURCE OPTIONAL
*"     REFERENCE(I_MAXSIZE) TYPE  SBIWA_S_INTERFACE-MAXSIZE OPTIONAL
*"     REFERENCE(I_INITFLAG) TYPE  SBIWA_S_INTERFACE-INITFLAG OPTIONAL
*"     REFERENCE(I_UPDMODE) TYPE  SBIWA_S_INTERFACE-UPDMODE OPTIONAL
*"     REFERENCE(I_DATAPAKID) TYPE  SBIWA_S_INTERFACE-DATAPAKID
*"       OPTIONAL
*"     REFERENCE(I_PRIVATEMODE) OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_DATA_VIEW) TYPE  ZFITR_TRAN
*"  TABLES
*"      E_T_DATA TYPE  ZFITR_TRAN OPTIONAL
*"      I_T_SELECT TYPE  SBIWA_T_SELECT OPTIONAL
*"      I_T_FIELDS TYPE  SBIWA_T_FIELDS OPTIONAL
*"  EXCEPTIONS
*"      NO_MORE_DATA
*"      ERROR_PASSED_TO_MESS_HANDLER
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*

*****************changes by suri 10252011***********

DATA: S_CURSOR TYPE CURSOR.
  DATA: G TYPE I,
        S TYPE I.
STATICS: s_fact_count    TYPE i.
* Auxiliary Selection criteria structure
  DATA: L_T_SELECT TYPE SBIWA_T_SELECT.
* Statement table for dynamic select
  STATICS: L_T_DYNAMIC_SELECT TYPE RSAOT_T_DYNAMIC_SELECT.
* Maximum number of lines for DB table
  STATICS: L_MAXSIZE TYPE SBIWA_S_INTERFACE-MAXSIZE.
* Initialization
  IF I_INITFLAG = SBIWA_C_FLAG_ON.
************************************************************************
* Initialization: check input parameters
* buffer input parameters
* prepare data selection
************************************************************************
*    g_updmode = i_updmode.
*   g_isource = i_isource.
*   Invalid second initialization call -> error exit
*    IF NOT g_flag_interface_initialized IS INITIAL.
*
*      log_write 'E'                    "message type
*                'R3'                   "message class
*                '008'                  "message number
*                ' '                    "message variable 1
*                ' '.                   "message variable 2
*      RAISE error_passed_to_mess_handler.
*    ENDIF.
*   Check InfoSource validity
*    CASE I_DSOURCE.
*      WHEN 'ZBI_GR_IR_MAPPING'.
*      WHEN OTHERS.
*        log_write 'E'                  "message type
*                  'R3'                 "message class
*                  '009'                "message number
*                  I_DSOURCE            "message variable 1
*                  ' '.                 "message variable 2
*        RAISE error_passed_to_mess_handler.
*    ENDCASE.
    APPEND LINES OF I_T_SELECT TO G_T_SELECT.
     CALL FUNCTION 'RSAN_FILL_DYNAMICAL_SELECT'
         EXPORTING
              I_T_SELECT                 = G_T_SELECT
         IMPORTING
              E_T_DYNAMIC_SELECT         = L_T_DYNAMIC_SELECT
         EXCEPTIONS
              INVALID_SELECTION_CRITERIA = 1
              .
*    IF SY-SUBRC <> 0.
*      IF 1 = 2. MESSAGE E014(R3). ENDIF.
**     LOG_WRITE 'E'                    "message type
**               'R3'                   "message class
**               '014'                  "message number
**               ' '                    "message variable 1
**               ' '.                   "message variable 2
*      RAISE ERROR_PASSED_TO_MESS_HANDLER.
*    ENDIF.
**************************changes by suri 10252011********************

* Make Ranges to pass them to the Select query
  PERFORM f_make_ranges USING i_bukrs i_rfha.

* Get transaction data for fixed term and deposit notice
  PERFORM f_get_data.

* Fill output
  PERFORM f_fill_output CHANGING e_data_view.

"e_t_data[] = e_data_view.

**************************changes by suri 10252011********************

  e_data_view1[] = e_data_view[].

** pass processed data back to extractor
**     REFRESH C_T_DATA[].
*     REFRESH E_T_DATA[].
**     C_T_DATA[] = L_ZOXEP50051[].
*     E_T_DATA[] = L_ZOXEP50051[].
*     APPEND E_T_DATA.
*     APPEND  LINES OF L_ZOXEP50051 TO L_C_T_DATA_TEMP.
*     CLEAR: V_DIFF, V_LINES, V_COUNTER, V_COUNT.
*     CLEAR: L_ZOXEP50051_1, L_ZOXEP50051_P, L_ZOXEP50051_2.
*     CLEAR: L_ZOXEP50051[].


     DESCRIBE TABLE e_data_view1 LINES N.
     S_FACT_COUNT = N.
ELSE.
************************************************************************
* Data transfer: First Call      OPEN CURSOR + FETCH
*                Following Calls FETCH only
************************************************************************
* IF G_COUNTER_DATAPAKID = 000000.
*   copy the requested amount of data
    IF g_cursor > s_fact_count.
      g_no_more_data = rssg_c_true.
*     set the delta upload time stamp
      RAISE no_more_data.
    ELSE.
*   determine maxsize parameters
    IF G_CURSOR < 1.
       g_cursor = 1.
    ELSE.
       G_CURSOR = G_CURSOR.
    ENDIF.
    IF i_maxsize <= 0.
      g_maxsize = 50000.
    ELSE.
      g_maxsize = i_maxsize.
    ENDIF.
      l_cursor_last = g_cursor + g_maxsize - 1.
      IF l_cursor_last > s_fact_count.
        l_cursor_last = s_fact_count.
      ENDIF.
      IF N > 0.
        INSERT LINES OF e_data_view1
        FROM g_cursor TO l_cursor_last
            INTO TABLE e_t_data.
      ENDIF.
      g_cursor = l_cursor_last + 1.
*   ENDIF.
ENDIF.
    g_counter_datapakid = g_counter_datapakid + 1.
ENDIF.                    "Initialization mode or data extraction ?


***************end of changes by suri 10252011******************


ENDFUNCTION.
