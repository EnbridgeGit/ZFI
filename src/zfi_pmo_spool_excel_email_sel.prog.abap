*&---------------------------------------------------------------------*
*&  Include           ZFI_PMO_SPOOL_EXCEL_EMAIL_SEL
*&---------------------------------------------------------------------*
*&-----------------------------------------------------------------------*
*& Report Name          : ZFI_PMO_SPOOL_EXCEL_EMAIL_SEL                  *
*& Author               : KMB                                            *
*& Creation Date        : 04-JUN-2019                                    *
*& Transport no.        : D30K929860                                     *
*& Object ID            : CHG0147746                                     *
*& Application Area     : FI                                             *
*& Description          : Mail excel with job spool data                 *
*&-----------------------------------------------------------------------*
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 07-06-2019   KMB         D30K929919  CHG0147746 ENHC0025526 Excl       *
*                                      attachement with spool 7.6.2019   *
**************************************************************************
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 27-06-2019   KMB         D30K929981  CHG0147746 ENHC0025526 Excl       *
*                                      attachement with spool 3.7.2019   *
**************************************************************************
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 24-06-2019   KMB         D30K930025  CHG0153285 DFCT0017463 date field *
*                                      in selection screen    12.7.2019  *
**************************************************************************
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 15-07-2019   KMB         D30K930029  CHG0153285 DFCT0017463 multiple   *
*                                      email in selection screen 15.7.19 *
**************************************************************************

"Selection screen
SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME.
*BOC by KMB on 15.7.2019 CHG0153285 Date field in selection screen
*  PARAMETERS : p_email TYPE SZA5_D0700-SMTP_ADDR OBLIGATORY.
 SELECT-OPTIONS : s_email FOR gv_addr NO INTERVALS OBLIGATORY.
*EOC by KMB on 15.7.2019 CHG0153285 Date field in selection screen
*BOC by KMB on CHG0147746 ENHC0025526 Excl attachement with spool 3.7.2019
  PARAMETERS : p_spool TYPE tbtcp-listident.
*EOC by KMB on CHG0147746 ENHC0025526 Excl attachement with spool 3.7.2019
  PARAMETERS : p_date TYPE sy-datum. "Added by KMB on 12.7.2019 CHG0153285 Date field in selection screen
SELECTION-SCREEN : END OF BLOCK B1.
