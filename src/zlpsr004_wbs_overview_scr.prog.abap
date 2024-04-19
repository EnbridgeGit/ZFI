*&---------------------------------------------------------------------*
*&  Include           ZLPSR004_WBS_OVERVIEW_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS:s_proj   FOR gv_pspid,
               s_wbs    FOR gv_pspnr,
               s_status FOR gv_status,
               s_level  FOR gv_stufe.
SELECTION-SCREEN END OF BLOCK blk1.
