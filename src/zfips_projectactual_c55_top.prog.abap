*&---------------------------------------------------------------------*
*&  Include           ZFIPS_PROJECTACTUAL_C55_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZFIPS_PROJECTACTUAL_C55                        *
* Include            :  ZFIPS_PROJECTACTUAL_C55_TOP                    *
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

TABLES: COEP.

TYPES:  BEGIN OF TY_COEP,
          PERIO   LIKE COEP-PERIO,
          OBJNR   LIKE COEP-OBJNR,
          GJAHR   LIKE COEP-GJAHR,
          BUKRS   LIKE COEP-BUKRS,
          KSTAR   LIKE COEP-KSTAR,
          WRTTP   LIKE COEP-WRTTP,
          VERSN   LIKE COEP-VERSN,
          WOGBTR  LIKE COEP-WOGBTR,
        END OF TY_COEP.

DATA: LV_LOCAL    TYPE INTEGER,
      LV_LOCAL1    TYPE INTEGER, "Addded by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
      WA_COEP     LIKE COEP,
      S_COEP      TYPE TY_COEP,
      T_COEP      LIKE TABLE OF S_COEP,
      T_COEP1      LIKE TABLE OF S_COEP,"Addded by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
      T_COEP_SUM  LIKE TABLE OF S_COEP,
      T_COEP_SUM1 LIKE TABLE OF S_COEP, "Addded by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
      ST_DATAREC  TYPE STRING,
      GT_DATA      LIKE TABLE OF ST_DATAREC,
      GT_DATA1      LIKE TABLE OF ST_DATAREC. "Addded by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF

DATA: MSG(80)           TYPE C,
      LV_ACCOUNT(6)     TYPE C,
      LV_CATEGORY(6)    TYPE C,
      LV_DATASRC(11)     TYPE C,
      LV_ENTITY(22)     TYPE C,
      LV_INTCO(8)       TYPE C,
      LV_RPTCURRENCY(2) TYPE C,
      LV_TIME(8)        TYPE C,
      LV_PROJECT(16)    TYPE C,
      LV_IOSWBS(16)     TYPE C,
      LV_DIVISION(16)   TYPE C,
      LV_AMOUNT(20)     TYPE C.

DATA: TUCHFILE          LIKE RFPDO-RFBIFILE.
DATA: TUCHFILE1          LIKE RFPDO-RFBIFILE."Added by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF

CONSTANTS: DELIMTR  TYPE C VALUE ','.

TYPES: BEGIN OF TY_ACTUAL,
        ACCOUNT(6)     TYPE C,
        PROJECT(16)    TYPE C,
        IOSWBS(16)     TYPE C,
        WBS            TYPE PS_POSKI,
        AMOUNT         TYPE COEP-WOGBTR,
        CATEGORY(6)    TYPE C,
        DATASRC(11)    TYPE C,
        ENTITY(22)     TYPE C,
        INTCO(8)       TYPE C,
        RPTCURRENCY(2) TYPE C,
        TIME(8)        TYPE C,
        DIVISION(16)   TYPE C,

       END OF TY_ACTUAL.
DATA: GT_ACTUAL TYPE TABLE OF TY_ACTUAL,
      GT_ACTUAL_TEMP TYPE TABLE OF TY_ACTUAL,
      GT_ACTUAL_TEMP1 TYPE TABLE OF TY_ACTUAL, "Addded by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
      GS_ACTUAL TYPE TY_ACTUAL,
      GS_ACTUAL1 TYPE TY_ACTUAL, "Addded by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
      GT_ACTUAL1 TYPE TABLE OF TY_ACTUAL, "Addded by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
      GT_MAPPING TYPE TABLE OF ZFIT_C55_MAPPING,
      GT_MAPPING1 TYPE TABLE OF ZFIT_C55_MAPPING, "Addded by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
      GT_PROJ_REG TYPE TABLE OF ZFI_C55_PROJ_REG,
      GT_PROJ_REG1 TYPE TABLE OF ZFI_C55_PROJ_REG. "Addded by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF

TYPES: BEGIN OF TY_FINAL,
        INV_CODE TYPE ZINVST_CODE,
        LAST_MNTH(10) TYPE C,
        MONTH_DATE(10) TYPE C,
        RES_PCODE(5) TYPE C,
        MONTH_VAL(20) TYPE C,
        MONTH_VALC(20) TYPE C,
        ACC_CODE(50) TYPE C,
        REPLACE TYPE C,
       END OF TY_FINAL.


TYPES: BEGIN OF TY_FINAL1,
        INV_CODE TYPE ZINVST_CODE,
        LAST_MNTH(10) TYPE C,
        MONTH_DATE(10) TYPE C,
        RES_PCODE(5) TYPE C,
        MONTH_VAL(20) TYPE C,
        MONTH_VALC(20) TYPE C,
        ACC_CODE(50) TYPE C,
        REPLACE TYPE C,
        PROJECT(16) TYPE C,
       END OF TY_FINAL1.

DATA: GT_FINAL1 TYPE TABLE OF TY_FINAL1,
      GS_FINAL1 TYPE TY_FINAL1.

DATA: GT_FINAL TYPE TABLE OF TY_FINAL,
      GS_FINAL TYPE TY_FINAL1.
*      GS_FINAL TYPE TY_FINAL.

CONSTANTS: GC_EXCLUDE(7) TYPE C VALUE 'EXCLUDE',
           GC_OA(22) TYPE C VALUE 'OVERHEAD & ALLOCATIONS',
           GC_CO(13) TYPE C VALUE 'CONTRIBUTIONS',
           GC_CC(15) TYPE C VALUE 'COST OF CAPITAL',
           GC_RT(11) TYPE C VALUE 'RETIREMENTS',
           GC_DC(14) TYPE C VALUE 'DIRECT CAPITAL',
           GC_WBS(6) TYPE C VALUE 'WBS_9*',
           GC_Y TYPE C VALUE 'Y',
           GC_IC(2) TYPE C VALUE 'IC',
           GC_DIV(4) TYPE C VALUE 'Div_',
           GC_WBSU(4) TYPE C VALUE 'WBS_'.
