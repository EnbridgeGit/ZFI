*&---------------------------------------------------------------------*
*&  Include           ZFAPI129_INVOICE_WF_ITEMS_SCR
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI129_INVOICE_WF_ITEMS                      *
* Include Program    :  ZFAPI129_INVOICE_WF_ITEMS_SCR                  *
* Author             :  Vijay Rajaputra                                *
* Creation Date      :  05-Nov-2018                                    *
* Application Area   :  FICO                                           *
* Technical Contact  :  Vijay Rajaputra                                *
*                                                                      *
* Purpose            :  Screen Elements  Include Program               *
*                                                                      *
*----------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 05-Nov-2018  VRAJAPUTRA  D30K929291  CHG0130803  Initial development *
*                          D30K929374                                  *
* 22-Jan-2019  VRAJAPUTRA  D30K929505  CHG0135339  Duplicate checks    *
*                          D30K929531                                  *
* 31-Jan-2019  VRAJAPUTRA  D30K929554  CHG0136094  Duplicate checks    *
*&---------------------------------------------------------------------*

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************
* Select options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_erpid  TYPE char05.     " ERP ID                   "
SELECT-OPTIONS:   s_bukrs  FOR  bsik-bukrs. " Company Code             "
SELECT-OPTIONS:   s_lifnr  FOR  bsik-lifnr. " Vendor                   "
SELECT-OPTIONS:   s_ktokk  FOR  lfa1-ktokk. " Vendor Account Group     "
SELECT-OPTIONS:   s_gjahr  FOR  bsik-gjahr. " Fiscal Year              "
SELECT-OPTIONS:   s_belnr  FOR  bsik-belnr. " Accounting Document      "
SELECT-OPTIONS:   s_budat  FOR  bsik-budat. " Posting Date             "
PARAMETERS:       p_kurst  TYPE kurst_curr, " Exchange Rate Type       "
                  p_tzone  TYPE tznzone.    " Time Zone                "
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 01.
PARAMETERS:       cb_prkd  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: COMMENT  03(12) text-011.
SELECTION-SCREEN: POSITION 22.
PARAMETERS:       cb_open  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: COMMENT  24(10) text-012.
SELECTION-SCREEN: POSITION 42.
PARAMETERS:       cb_clrd  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: COMMENT  44(13) text-013.
SELECTION-SCREEN: POSITION 60.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_wfiimg TYPE char10      "Invoice Image WF Task     "
                           MODIF ID DSP.
PARAMETERS:       p_wfnpis TYPE char10      "NPO Invoice Start WF Task "
                           MODIF ID DSP.
PARAMETERS:       p_wfnpir TYPE char10      "NPO Invoice RCO WF Task   "
                           MODIF ID DSP.
PARAMETERS:       p_wfnpia TYPE char10      "NPO Invoice Approve WF Task
                           MODIF ID DSP.
PARAMETERS:       p_wfnpip TYPE char10      "NPO Invoice Post WF Task  "
                           MODIF ID DSP.
PARAMETERS:       p_wfpois TYPE char10      "PO Invoice Start WF Task  "
                           MODIF ID DSP.
PARAMETERS:       p_wfpoia TYPE char10      "PO Invoice Approve WF Task"
                           MODIF ID DSP.
PARAMETERS:       p_wfpoip TYPE char10      "PO Invoice Post WF Task   "
                           MODIF ID DSP.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_doctyp FOR toapr-ncitype "Archive Image Doc. Type "
                           NO INTERVALS
                           MODIF ID DSL.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       cb_ximgs AS CHECKBOX      "Extract Unregistered Imags"
                           DEFAULT 'X'.
PARAMETERS:       cb_viaad AS CHECKBOX      "Extract Items via Acc.Doc."
                           DEFAULT 'X'.
PARAMETERS:       cb_viawf AS CHECKBOX      "Extract Items via Workflow"
                           DEFAULT 'X'.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_cpudt  FOR bsik-cpudt   "Extract Date              "
                           NO-EXTENSION.
SELECTION-SCREEN: END   OF BLOCK blk1.

*eject
* Run options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 1.
PARAMETERS:       rb_appl  RADIOBUTTON GROUP rad2
                           DEFAULT 'X'
                           USER-COMMAND cmnd.
SELECTION-SCREEN: COMMENT  03(18) text-031.
SELECTION-SCREEN: POSITION 22.
PARAMETERS:       rb_pres  RADIOBUTTON GROUP rad2.
SELECTION-SCREEN: COMMENT  24(19) text-032.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  01(08) text-035
                           FOR FIELD p_path1
                           MODIF ID M1.
PARAMETERS:       p_path1  TYPE string
                           MODIF ID M1
                           LOWER CASE.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  01(16) text-036
                           FOR FIELD p_file2
                           MODIF ID M2.
PARAMETERS:       p_path2  TYPE string
                           MODIF ID M2
                           LOWER CASE.
PARAMETERS:       p_file2  TYPE string
                           MODIF ID M2
                           LOWER CASE.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_maxrec TYPE NUMC10     "Max Number Records per File"
                           MODIF ID M2.
SELECTION-SCREEN: END   OF BLOCK blk2.

*eject
************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF screen-group1 EQ 'DSP'.
      screen-input    = 0.
      screen-output   = 1.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 EQ 'DSL'.
      IF screen-name CS '-LOW'.
        screen-input  = 0.
        screen-output = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 EQ 'M1'.
      IF rb_appl     EQ space.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 EQ 'M2'.
      IF rb_pres     EQ space.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

*eject
AT SELECTION-SCREEN.

* TRANSLATE p_path2 TO UPPER CASE.
  IF sscrfields-ucomm = 'R3'.
    IF rb_pres = abap_true.
      CLEAR p_path1.
    ELSE.
      CLEAR p_file2.
      CLEAR p_path2.
    ENDIF.
  ENDIF.

  IF sscrfields-ucomm = gc_ucomm_onli.

    IF ( ( ( p_path1 IS INITIAL ) AND ( rb_pres EQ abap_true ) ) OR
         ( ( p_path2 IS INITIAL ) AND ( rb_appl EQ abap_true ) )    ).
      MESSAGE i082(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    IF p_erpid IS INITIAL.
      SET CURSOR FIELD 'P_ERPID'.
      MESSAGE text-101 TYPE 'E'.
    ENDIF.

    IF p_kurst IS INITIAL.
      SET CURSOR FIELD 'P_KURST'.
      MESSAGE text-105 TYPE 'E'.
    ENDIF.

    IF   ( s_cpudt IS INITIAL ).
      MESSAGE text-102 TYPE 'E'.
    ENDIF.

    IF rb_appl IS NOT INITIAL.
      IF p_path2 IS INITIAL.
        MESSAGE text-103 TYPE 'E'.
      ENDIF.
      IF p_file2 IS INITIAL.
        MESSAGE text-104 TYPE 'E'.
      ENDIF.
    ENDIF.

  ENDIF.
