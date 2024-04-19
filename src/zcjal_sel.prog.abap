*&---------------------------------------------------------------------*
*&  Include           ZCJAL_SEL
*&---------------------------------------------------------------------*

TABLES : proj,prps.

SELECT-OPTIONS: s_kokrs  FOR proj-vkokr DEFAULT '10',
                s_posid  FOR prps-posid
                  MATCHCODE OBJECT prsm,
                s_date  FOR sy-datum DEFAULT syst-datum.
PARAMETERS: p_recsys LIKE t000-logsys DEFAULT 'WARP'.
