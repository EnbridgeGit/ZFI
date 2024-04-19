*&---------------------------------------------------------------------*
*&  Include           ZFIPS_PROJECTACTUAL_C55_SEL
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZFIPS_PROJECTACTUAL_C55                        *
* Include            :  ZFIPS_PROJECTACTUAL_C55_SEL                    *
* Author             :  Rajeshwar Reddy                                *
* Date               :  12th-Nov-2019                                  *
* Technical Contact  :  Rajeshwar Reddy                                *
* Business Contact   :                                                 *
* Purpose            :  WNew Interface from SAP to C55                 *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 12th-Nov-2019  JOOKONTR    D30K930280 CHG0160132 Initial Development *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 9th-Jul-2020  KMB     D30K930635      CHG0185357 ENHC0029276: YTD    *
*                                       Project actual costs extract to*
*                                       IDF                            *
*&---------------------------------------------------------------------*



SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME.

PARAMETERS:P_PERIO   LIKE COEP-PERIO OBLIGATORY.
SELECT-OPTIONS:S_OBJNR   FOR COEP-OBJNR OBLIGATORY
               DEFAULT 'PR00000000' TO 'PR99999999'.
PARAMETERS:P_GJAHR   LIKE COEP-GJAHR OBLIGATORY.
SELECT-OPTIONS:S_WRTTP   FOR COEP-WRTTP OBLIGATORY DEFAULT '4',
               S_KSTAR   FOR COEP-KSTAR,
               S_VERSN   FOR COEP-VERSN.
SELECTION-SCREEN END OF BLOCK A1.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS : p_c55 TYPE c AS CHECKBOX. "Added by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
PARAMETERS: P_LOCAL   RADIOBUTTON GROUP RAD1 DEFAULT 'X',
*           p_file    TYPE        string DEFAULT 'C:\SAPTEMP\PROActual.csv', "TR995
            P_FILE    TYPE        STRING DEFAULT 'H:\SAPTEMP\PROActual.csv', "TR995
            P_SERVER  RADIOBUTTON GROUP RAD1,
            CSVFILE   LIKE        RFPDO-RFBIFILE.
SELECTION-SCREEN END OF BLOCK B1.

"BOC by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
PARAMETERS : p_idf TYPE c AS CHECKBOX.
PARAMETERS: P_LOCAL1   RADIOBUTTON GROUP RAD2 DEFAULT 'X',
            P_FILE1   TYPE        STRING DEFAULT 'H:\SAPTEMP\PROActual.csv', "TR995
            P_SERVr1  RADIOBUTTON GROUP RAD2,
            CSVFILE1   LIKE        RFPDO-RFBIFILE.
SELECTION-SCREEN END OF BLOCK B2.
"EOC by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
