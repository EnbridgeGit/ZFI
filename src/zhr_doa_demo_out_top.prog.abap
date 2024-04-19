*&---------------------------------------------------------------------*
*&  Include           ZHR_DOA_DEMO_OUT_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZHR_DOA_DEMO_OUT_TOP
*&---------------------------------------------------------------------*

*TABLES: pa0001,ZFIMV_DOA,ZFIT_DOA_NEW_NEW, PA0008 , PA0105, sscrfields.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES :ZFIMV_DOA_NEW,PA0001,SSCRFIELDS.


*=======================================================================
* GLOBAL CONSTANTS
*=======================================================================
CONSTANTS:
    GC_FSLASH     TYPE CHAR1            VALUE '\',
    GC_UCOMM_ONLI TYPE SSCRFIELDS-UCOMM VALUE 'ONLI',
    GC_X          TYPE C                VALUE 'X'.
*=======================================================================
* GLOBAL TYPES
*=======================================================================
TYPES :


  BEGIN OF GTY_PERNR, " T_PERNR
    PERNR      TYPE P_PERNR,
  END OF GTY_PERNR,

  BEGIN OF GTY_OUTPUT,
    PERNR      TYPE CHAR40,
    USRID_LONG TYPE CHAR40,
    TRFGR      TYPE CHAR40,
  END OF GTY_OUTPUT,

  BEGIN OF GTY_OUTPUT_FINAL,
    USERNAME   TYPE CHAR40,
    ASL        TYPE CHAR50,
  END OF GTY_OUTPUT_FINAL.

*=======================================================================
* GLOBAL VARIABLES
*=======================================================================
DATA:
      GV_CN_FILES  TYPE I,
      GV_ACTIVE    TYPE C,
      GV_FLAG      TYPE C,
      GV_FILENAME1 TYPE STRING,
      GV_DESK      TYPE LOGSYS,
      GV_PATH_ARCH TYPE STRING,
      GV_PATH_LOG  TYPE STRING,
      GV_PATH_MAIN TYPE STRING,
      GV_SYSID     TYPE LOGSYS,
      GV_TEXT      TYPE TRUXS_T_TEXT_DATA,
      GV_DATA      TYPE STRING,
      GV_AMT       TYPE CHAR50,
      GV_TABIX     TYPE sy-tabix,
      GV_FLAG_C    TYPE C.
*      GV_FNAME2 TYPE STRING,
*      GV_FNAME1 TYPE STRING,
*      GV_FOLD1  TYPE STRING,
*      GV_MSG TYPE STRING,
*      GV_FOLD2          TYPE LOCALFILE,
*      Gv_TAB_DATA TYPE STRING,


*=======================================================================
* GLOBAL WORKAREAS
*=======================================================================
DATA:
      GS_PERNR           TYPE GTY_PERNR,
      GS_PA0008          TYPE PA0008,
      GS_PA0105          TYPE PA0105,
      GS_OUTPUT          TYPE GTY_OUTPUT,
      GS_PA0001          TYPE PA0001,
      GS_PA0000          TYPE PA0000,
      GS_OUTPUT_FINAL    TYPE GTY_OUTPUT_FINAL,
      GS_OUTPUT_FINAL_C  TYPE GTY_OUTPUT_FINAL,
      GS_ZFIT_DOA_NEW        TYPE ZFIT_DOA_NEW.
*=======================================================================
* GLOBAL INTERNAL TABLES
*=======================================================================

DATA:
      GT_ZFIT_DOA_NEW        TYPE STANDARD TABLE OF ZFIT_DOA_NEW INITIAL SIZE 0,
      GT_PERNR           TYPE STANDARD TABLE OF GTY_PERNR WITH HEADER LINE,
      GT_TAB_DATA        TYPE  STANDARD TABLE OF STRING,
      GT_PA0008          TYPE TABLE OF PA0008,
      GT_PA0105          TYPE TABLE OF PA0105,
      GT_PA0001          TYPE TABLE OF PA0001,
      GT_OUTPUT          TYPE STANDARD TABLE OF GTY_OUTPUT,
      GT_OUTPUT_FINAL    TYPE STANDARD TABLE OF GTY_OUTPUT_FINAL,
      GT_OUTPUT_FINAL_C  TYPE STANDARD TABLE OF GTY_OUTPUT_FINAL,
      GT_PA0000          TYPE TABLE OF PA0000.
*      ITAB               TYPE TRUXS_T_TEXT_DATA,
******************************************************************************************************
* REFERRENCE VARIABLES
******************************************************************************************************

DATA : GREF_UTIL TYPE REF TO ZCL_IAP_INTERFACE_UTIL.
