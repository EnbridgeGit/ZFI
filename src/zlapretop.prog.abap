*&---------------------------------------------------------------------*
*&  Include           ZLAPRETOP
*&---------------------------------------------------------------------*
TABLES: LFA1,
        LFB1.
TABLES: T001.
TABLES: TKAF,
        bkpf,
        bsik.

TYPE-POOLS: RSDS, RSFS.

* constands
CONSTANTS: C_B              TYPE C VALUE 'B',
           C_P              TYPE C VALUE 'P',
           C_S              TYPE C VALUE 'S',
           C_X              TYPE C VALUE 'X',
           C_ON(1)          TYPE N VALUE '1',
           C_OFF(1)         TYPE N VALUE '0',
           C_UMSAV(5)       TYPE C VALUE 'UMSAV',
           C_KUMSL(5)       TYPE C VALUE 'KUMSL',
           C_PERDE(5)       TYPE C VALUE 'PERDE',
           C_BUKRS(5)       TYPE C VALUE 'BUKRS',
           C_LIFNR(5)       TYPE C VALUE 'LIFNR',
           C_GSBER(5)       TYPE C VALUE 'GSBER',
           C_BLART(5)       TYPE C VALUE 'BLART',
           C_UMSKZ(5)       TYPE C VALUE 'UMSKZ',
           C_GJAHR(5)       TYPE C VALUE 'GJAHR',
           C_BELNR(5)       TYPE C VALUE 'BELNR',
           C_STIDA(5)       TYPE C VALUE 'STIDA',
           C_MONAT(5)       TYPE C VALUE 'MONAT',
           C_RFRRK10        LIKE RKB1X-TABNAME VALUE 'RFRRK10',
           C_RFRRK20        LIKE RKB1X-TABNAME VALUE 'RFRRK20',
           C_ACCT_TYPE_KRED LIKE T074U-UMSKZ VALUE 'K',
           C_FBRK           LIKE TKAF-APPLC VALUE 'FBRK'.
CONSTANTS: C_9999(4)        TYPE N VALUE '9999',
           C_DYN_SEL(23)    TYPE C
                            VALUE 'FBRK_DYNAMIC_SELECTIONS'.

*     dynamic selections
DATA: GT_DYNSEL        TYPE  RSDS_TEXPR.
DATA: GT_CALLBACK      LIKE LDBCB    OCCURS 0 WITH HEADER LINE.
DATA: GT_SELECTIONS    LIKE RSPARAMS OCCURS 0 WITH HEADER LINE.
DATA: GT_EXPRESSIONS   TYPE RSDS_TEXPR.
DATA: GT_SELECT_FIELDS TYPE RSFS_FIELDS.

DATA: GT_RE_EXTR LIKE RFRRK10 OCCURS 0 WITH HEADER LINE.

DATA: GT_RE_EX20 LIKE RFRRK20 OCCURS 0 WITH HEADER LINE.

DATA: RECHERCHE_WAERS     LIKE T001-WAERS.

DATA: G_STIDA LIKE RFPDO-ALLGSTID.
* fields for progress indicator
DATA: G_PROGRESS_PORTION  TYPE P VALUE 999.
DATA: BEGIN OF GS_PROGRESS,
        READREC   TYPE P,
        RECRD(20) TYPE C,
        LIFNR     LIKE LFA1-LIFNR,
        TEXT(80)  TYPE C,
      END   OF GS_PROGRESS.

* Steering flags
DATA: BEGIN OF GS_FLG,
        PERDE_DRILL_DOWN LIKE C_OFF,
        BUKRS_DRILL_DOWN LIKE C_OFF,
        LIFNR_DRILL_DOWN LIKE C_OFF,
        GSBER_DRILL_DOWN LIKE C_OFF,
        UMSKZ_DRILL_DOWN LIKE C_OFF,
        BLART_DRILL_DOWN LIKE C_OFF,
        GJAHR_DRILL_DOWN LIKE C_OFF,
        BELNR_DRILL_DOWN LIKE C_OFF,
        SPECIAL_GL_TRANS LIKE C_OFF,
        NORMAL_TRANS     LIKE C_OFF,
        LFA1_FIELDS_USED LIKE C_OFF,
        LFB1_FIELDS_USED LIKE C_OFF,
      END   OF GS_FLG.

RANGES: GR_PERDE FOR RFRRD10-PERDE.
RANGES: GR_UMSKZ FOR RFRRD10-UMSKZ.
