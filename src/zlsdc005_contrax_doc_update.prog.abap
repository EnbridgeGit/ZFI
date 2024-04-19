*&---------------------------------------------------------------------*
*& Report  ZLSDC005_CONTRAX_DOC_UPDATE
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT  zlsdc005_contrax_doc_update.
************************************************************************
*                                                       *
************************************************************************
*  Program:   ZLSDC005_CONTRAX_DOC_UPDATE                              *
*  Author:    Shegar Rajamani                                          *
*  Date:      05/02/2024                                               *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:                          *
*                                                                      *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date      By           Transport   Description                      *
* ---------- ------------ ----------  -------------------------------- *
*                                                                      *
*----------------------------------------------------------------------*

************************************************************************
*                           Selection Screen                           *
************************************************************************

TABLES: bkpf.

DATA: gt_bkdf TYPE TABLE OF bkdf,
      gt_bsec TYPE TABLE OF bsec,
      gt_bsed TYPE TABLE OF bsed,
      gt_bset TYPE TABLE OF bset,
      gt_bkpf TYPE TABLE OF bkpf,
      gt_bseg TYPE TABLE OF bseg,
      gs_bseg TYPE bseg.
TYPES: BEGIN OF ty_tab,
       date1 TYPE bkpf-budat,
       date2 TYPE bkpf-budat,
       END OF ty_tab.
DATA: it_tab TYPE TABLE OF ty_tab,
      wa_tab TYPE ty_tab.
DATA: gt_abuz TYPE TABLE OF abuz,
      gt_acchd TYPE TABLE OF acchd,
      gt_accit TYPE TABLE OF accit,
      gt_acccr TYPE TABLE OF acccr.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE t1.
SELECT-OPTIONS: s_belnr FOR bkpf-belnr NO-EXTENSION NO INTERVALS,
                s_bukrs FOR bkpf-bukrs NO-EXTENSION NO INTERVALS,
                s_gjahr FOR bkpf-gjahr NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.


  CALL FUNCTION 'FI_DOCUMENT_READ'
   EXPORTING
     i_bukrs           = s_bukrs-low
     i_belnr           = s_belnr-low
     i_gjahr           = s_gjahr-low
* IMPORTING
*   E_AWTYP           = E_AWTYP
*   E_AWREF           = E_AWREF
*   E_AWORG           = E_AWORG
*   E_AWSYS           = E_AWSYS
   TABLES
     t_abuz            = gt_abuz
     t_acchd           = gt_acchd
     t_accit           = gt_accit
     t_acccr           = gt_acccr
     t_bkpf            = gt_bkpf
     t_bseg            = gt_bseg
   EXCEPTIONS
     wrong_input       = 1
     not_found         = 2.

  DATA: lv_agreement TYPE bseg-zuonr,
        lv_customer TYPE bseg-kunnr.
  DATA: gt_accchg TYPE TABLE OF accchg,
        gs_accchg TYPE accchg.
  break rajamans.

  SORT gt_bseg BY bschl ASCENDING.
  FIELD-SYMBOLS: <fs_bseg> TYPE bseg.
  LOOP AT gt_bseg ASSIGNING <fs_bseg>.
    IF <fs_bseg>-bschl NE '50'.
      lv_agreement = <fs_bseg>-sgtxt.
      lv_customer  = <fs_bseg>-kunnr.
    ENDIF.
    IF <fs_bseg>-bschl = '50'.
      <fs_bseg>-zuonr = lv_agreement.
      <fs_bseg>-sgtxt = lv_customer.

    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'CHANGE_DOCUMENT'
    TABLES
      t_bkdf = gt_bkdf
      t_bkpf = gt_bkpf
      t_bsec = gt_bsec
      t_bsed = gt_bsed
      t_bseg = gt_bseg
      t_bset = gt_bset.

  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*      IMPORTING
*        RETURN = RETURN.
    IF sy-subrc = 0.
      CLEAR: lv_agreement,
             lv_customer.
    ENDIF.
  ENDIF.
*    ENDIF.
**
**      gs_accchg-fdname = 'ZUONR'.
**      gs_accchg-oldval = <fs_bseg>-zuonr.
**      gs_accchg-newval = lv_agreement.
**      APPEND gs_accchg TO gt_accchg.
**      CLEAR: gs_accchg.
**
**      gs_accchg-fdname = 'ZUONR'.
**      gs_accchg-oldval = <fs_bseg>-zuonr.
**      gs_accchg-newval = lv_agreement.
**      APPEND gs_accchg TO gt_accchg.
**      CLEAR: gs_accchg.
**
**      CALL FUNCTION 'FI_DOCUMENT_CHANGE'
**        EXPORTING
**          x_lock               = 'X'
**          i_bukrs              = s_bukrs-low
**          i_belnr              = s_belnr-low
**          i_gjahr              = s_gjahr-low
**        TABLES
**          t_accchg             = gt_accchg
**        EXCEPTIONS
**          no_reference         = 1
**          no_document          = 2
**          many_documents       = 3
**          wrong_input          = 4
**          overwrite_creditcard = 5.
*      <fs_bseg>-zuonr = lv_agreement.
*      <fs_bseg>-sgtxt = lv_customer.

*  CALL FUNCTION 'CHANGE_DOCUMENT'
*    TABLES
*      t_bkdf     = gt_bkdf
*      t_bkpf     = gt_bkpf
*      t_bsec     = gt_bsec
*      t_bsed     = gt_bsed
*      t_bseg     = gt_bseg
*      t_bset     = gt_bset
**     T_BSEG_ADD =    T_BSEG_ADD
*    .

*  IF sy-subrc = 0.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
**      IMPORTING
**        RETURN = RETURN.
*    IF sy-subrc = 0.
*      CLEAR: lv_agreement,
*             lv_customer.
*    ENDIF.
*  ENDIF.
*  ENDLOOP.
