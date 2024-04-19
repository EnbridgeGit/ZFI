*&---------------------------------------------------------------------*
*& Report  Z_PERIOD_BLOCK
*&
*&---------------------------------------------------------------------*
*& Updates the period block for Operating Concern 1100
*& 2010/03/17 - gymana - SAP Note 13136
*&---------------------------------------------------------------------*

REPORT  Z_PERIOD_BLOCK.
TABLES: TKEBB.
UPDATE TKEBB SET BLOCKF      = 1           " Period Block
                 WHERE ERKRS = '1100'.     " 1100 - Operating Concern
