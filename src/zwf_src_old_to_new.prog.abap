*&---------------------------------------------------------------------*
*& Report  ZWF_SRC_OLD_TO_NEW
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZWF_SRC_OLD_TO_NEW.
TABLES: SWWWIHEAD.

DATA: BEGIN OF T_ITEMS OCCURS 0,
      ID TYPE SWW_WIID,
      TXT TYPE SWW_WITEXT,
      STAT TYPE SWW_WISTAT,
      AED TYPE SWW_AED,
      TOP_ID TYPE SWW_WIID,
*      VER TYpe SWD_VERSIO,
      END OF T_ITEMS.

DATA: BEGIN OF T_VER OCCURS 0,
      wfd_id LIKE SWD_HEADER-WFD_ID,
      VER LIKE SWD_HEADER-VERSION,
      END OF T_VER.

RANGES: R_STAT FOR SWWWIHEAD-WI_STAT.

DATA: W_VERSION LIKE SWD_HEADER-VERSION,
      W_VER LIKE SWD_HEADER-VERSION,
      W_DOC LIKE RBKP-BELNR,
      W_YR LIKE RBKP-GJAHR,
      W_RET LIKE SY-SUBRC,
      w_posted type c,
      W_STAT LIKE RBKP-RBSTAT,
      W_KEY LIKE SWR_STRUCT-OBJECT_KEY,
      w_var like SWR_STRUCT-SELSTAVAR,
      lta_worklist TYPE swrtwihdr,
      w_trigger type c,
      w_worklist type SWR_WIHDR.

SELECT-OPTIONS: S_DATE FOR SWWWIHEAD-WI_CD.
PARAMETERS: P_DATE LIKE SYST-DATUM OBLIGATORY.

R_STAT-SIGN = 'I'.
R_STAT-OPTION = 'EQ'.
R_STAT-LOW = 'READY'.
APPEND R_STAT.
R_STAT-LOW = 'STARTED'.
APPEND R_STAT.
R_STAT-LOW = 'CANCELLED'.
APPEND R_STAT.

SELECT WI_ID WI_TEXT WI_STAT WI_AED TOP_WI_ID FROM SWWWIHEAD INTO TABLE T_ITEMS
  WHERE  WI_STAT IN R_STAT AND
         WI_CD IN S_DATE AND
         WI_RH_TASK = 'TS02000010'.

SELECT WFD_ID VERSION FROM SWD_HEADER INTO TABLE T_VER
  WHERE WFD_ID = 'WS02000003'.

SORT T_VER BY VER DESCENDING.
READ TABLE T_VER INDEX 1.
W_VERSION = T_VER-VER.

Write:/ 'Workflow Triggered for Below Invoices'.

loop at t_items where stat = 'CANCELLED' AND AED GE P_DATE.
  CLEAR: W_DOC, W_YR, W_KEY, w_stat, w_posted.

  CALL FUNCTION 'SWI_READ_CONTAINER_OBJECT'
    EXPORTING
      WI_ID       = T_ITEMS-ID
      ELEMENT     = 'INVOICE'
    IMPORTING
      OBJKEY      = W_KEY
    EXCEPTIONS
      READ_FAILED = 1
      OTHERS      = 2.

  W_DOC = W_KEY+0(10).
  W_YR = W_KEY+10(4).

* Check if document already posted
  SELECT SINGLE RBSTAT FROM RBKP INTO W_STAT
    WHERE BELNR = W_DOC AND
          GJAHR = W_YR.

  IF W_STAT = '5'.
    w_posted = 'X'.
  ENDIF.

  IF w_posted = 'X'.
    continue.
  ENDIF.

  IF NOT W_KEY IS INITIAL.
    clear: w_var, w_trigger.
    CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
      EXPORTING
        OBJTYPE                  = 'BUS2081'
        OBJKEY                   = W_KEY
        SELECTION_STATUS_VARIANT = w_var
      TABLES
        WORKLIST                 = lta_worklist.

    DELETE lta_worklist WHERE wi_rh_task NE 'WS02000003'.
    Loop at lta_worklist into w_worklist.
      IF w_worklist-wi_id GT T_ITEMS-ID.
        W_trigger = 'X'.
      ENDIF.
    ENDLOOP.

    IF W_trigger NE 'X'.
      CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
        EXPORTING
          OBJECT_TYPE = 'BUS2081'
          OBJECT_KEY  = W_KEY
          EVENT       = 'COMPLETED'.

      write:/ w_doc, w_yr.
    ENDIF.
  ENDIF.
ENDLOOP.

loop at t_items where stat NE 'CANCELLED'.
  SELECT SINGLE WFD_VERSION FROM SWWWIHEAD INTO W_VER
    WHERE WI_ID = T_ITEMS-TOP_ID.
  IF W_VER LT W_VERSION.

    CLEAR: W_DOC, W_YR, W_KEY, w_stat, w_posted.

    CALL FUNCTION 'SWI_READ_CONTAINER_OBJECT'
      EXPORTING
        WI_ID       = T_ITEMS-ID
        ELEMENT     = 'INVOICE'
      IMPORTING
        OBJKEY      = W_KEY
      EXCEPTIONS
        READ_FAILED = 1
        OTHERS      = 2.

    W_DOC = W_KEY+0(10).
    W_YR = W_KEY+10(4).

    CALL FUNCTION 'SAP_WAPI_ADM_WORKFLOW_CANCEL'
      EXPORTING
        WORKITEM_ID = T_ITEMS-TOP_ID.

* Check if document already posted
    SELECT SINGLE RBSTAT FROM RBKP INTO W_STAT
      WHERE BELNR = W_DOC AND
            GJAHR = W_YR.

    IF W_STAT = '5'.
      w_posted = 'X'.
    ENDIF.

    IF w_posted = 'X'.
      continue.
    ENDIF.

    IF NOT W_KEY IS INITIAL.
      CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
        EXPORTING
          OBJECT_TYPE = 'BUS2081'
          OBJECT_KEY  = W_KEY
          EVENT       = 'COMPLETED'.

      write:/ w_doc, w_yr.
    ENDIF.
  ENDIF.
ENDLOOP.
