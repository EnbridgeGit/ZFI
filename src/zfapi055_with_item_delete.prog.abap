*&---------------------------------------------------------------------*
*& Report  ZFAPI055_WITH_ITEM_DELETE
*&
*&---------------------------------------------------------------------*
*&ZAP_WH_CHNG was executed to zero out withhodling tax base amounts
*&on all open parked invoices (entries in WITH_ITEM table will continue
*&to exist but with zero withholding tax base amounts.
*&It was causing the no data batch input for 0332 screen error.
*&To correct this error, this program will delete the document from
*&table WITH_ITEM
*&---------------------------------------------------------------------*

REPORT  zfapi055_with_item_delete.
TABLES: with_item.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_bukrs TYPE with_item-bukrs OBLIGATORY,
            p_gjahr TYPE with_item-gjahr OBLIGATORY.
SELECT-OPTIONS: s_belnr FOR with_item-belnr OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM delete_data.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: lt_with_item TYPE TABLE OF with_item.

  SELECT * FROM with_item INTO TABLE lt_with_item
    WHERE bukrs = p_bukrs
      AND belnr IN s_belnr
      AND gjahr = p_gjahr.
  IF lt_with_item[] IS INITIAL.
    WRITE: / ' No data to delete for given selection criteria..'.
    STOP.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_data .
  DATA: lv_deleted_rows TYPE sy-dbcnt.

  DELETE FROM with_item WHERE bukrs = p_bukrs
                          AND belnr IN s_belnr
                          AND gjahr = p_gjahr.
  lv_deleted_rows = sy-dbcnt.
  COMMIT WORK.
  WRITE : / 'Total deleted records ', lv_deleted_rows.

ENDFORM.                    " DELETE_DATA
