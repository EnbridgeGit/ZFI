REPORT ZFAPI034 MESSAGE-ID ZS.

************************************************************************
*  Project:     Mercator replacement                                   *
*  Author:      Mohammad T. Khan                                       *
*  Date:        March, 2005.                                           *
*  Description:                                                        *
*     - The purpose of this program is to map the PHH invoice interface*
*       file to SAP format. It will replace the mapping through        *
*       Mercator.                                                      *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*  Date     TR#    By       Description                                *
*---------- ---  --------- --------------------------------------------*
*12/01/2011 846  M Khan    Changes to automatically calculate Tax.     *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
tables:  DD03L.

DATA: INREC(150).
DATA: MSG(100) TYPE C.

field-symbols: <F1>.
data:          char(21)  type c.
data:          nodata(1) type c value '/'.

*Input file Header record

DATA: BEGIN OF HEADERIN,
        RECTYP(1)    TYPE C,
        BATCHID(12)  TYPE C,
        DOCDATE(8)   TYPE C,
        POSTDATE(8)  TYPE C,
        COMPCODE(4)  TYPE C,
        REFDOC(16)   TYPE C,
        CURRENCY(5)  TYPE C,
      END OF HEADERIN.

*Input file Detail record
DATA: BEGIN OF DETAILIN,
        RECTYP(1)    TYPE C,
        POSTKEY(2)   TYPE C,
        VENDOR(18)   TYPE C,
        AMOUNT(16)   TYPE C,
        DISCOUNT(16) TYPE C,
        TAXCODE(2)   TYPE C,
        INTORDER(10) TYPE C,
        GLACCT(10)   TYPE C,
        WBS(24)      TYPE C,
        PHHTXT(148)  TYPE C,
        COSTCNTR(10) TYPE C,
      END OF DETAILIN.

*Output file Session Header
data: begin of z_bgr00.
      include structure bgr00.
data: end of z_bgr00.

*Output file Document header
data: begin of z_bbkpf.
      include structure bbkpf.
data: end of z_bbkpf.

*Output file Document line item
data: begin of z_zbseg.
      include structure zbseg.
data: end of z_zbseg.


DATA: BEGIN OF sheader,
        STYPE  LIKE  BGR00-STYPE  VALUE '0',
        GROUP  LIKE  BGR00-GROUP,
*        MANDT  LIKE  BGR00-MANDT  VALUE '050',
        MANDT  LIKE  BGR00-MANDT,
        USNAM  LIKE  BGR00-USNAM  VALUE 'BATCH',
        START  LIKE  BGR00-START  VALUE '/       ',
        XKEEP  LIKE  BGR00-XKEEP  VALUE '/',
        NODATA LIKE  BGR00-NODATA VALUE '/',
      END OF sheader.

*Output file Document header
DATA: BEGIN OF DHEADER,
        STYPE  LIKE  BBKPF-STYPE  VALUE '1',
        TCODE(4),
        BLDAT  LIKE  BBKPF-BLDAT,
        BLART  LIKE  BBKPF-BLART,
        BUKRS  LIKE  BBKPF-BUKRS,
        BUDAT  LIKE  BBKPF-BUDAT,
        MONAT  LIKE  BBKPF-MONAT  VALUE '/',
        WAERS  LIKE  BBKPF-WAERS,
        KURSF  LIKE  BBKPF-KURSF  VALUE '/',
        BELNR  LIKE  BBKPF-BELNR  VALUE '/',
        WWERT  LIKE  BBKPF-WWERT  VALUE '/',
        XBLNR  LIKE  BBKPF-XBLNR,
        BVORG  LIKE  BBKPF-BVORG  VALUE '/',
        BKTXT  LIKE  BBKPF-BKTXT  VALUE '/',
        PARGB  LIKE  BBKPF-PARGB  VALUE '/',
        AUGLV  LIKE  BBKPF-AUGLV  VALUE '/',
        VBUND  LIKE  BBKPF-VBUND  VALUE '/',
        XMWST  LIKE  BBKPF-XMWST  VALUE '/',
        DOCID  LIKE  BBKPF-DOCID  VALUE '/',
        BARCD  LIKE  BBKPF-BARCD  VALUE '/',
        STODT  LIKE  BBKPF-STODT  VALUE '/',
        SENDE  LIKE  BBKPF-SENDE  VALUE '/',
      END OF DHEADER.

*Output file Document line item
DATA: BEGIN OF LITEM,
        STYPE  LIKE  ZBSEG-STYPE  VALUE '2',
        TBNAM(10)                 VALUE 'ZBSEG',
        NEWBS  LIKE  ZBSEG-NEWBS,
        NEWUM  LIKE  ZBSEG-NEWUM  VALUE '/',
        NEWBK  LIKE  ZBSEG-NEWBK  VALUE '/',
        WRBTR  LIKE  ZBSEG-WRBTR,
        WMWST  LIKE  ZBSEG-WMWST  VALUE '/',
        MWSKZ  LIKE  ZBSEG-MWSKZ,
        XSKRL  LIKE  ZBSEG-XSKRL,
        KOSTL  LIKE  ZBSEG-KOSTL  VALUE '/',
        AUFNR  LIKE  ZBSEG-AUFNR,
        MATNR  LIKE  ZBSEG-MATNR  VALUE '/',
        MENGE  LIKE  ZBSEG-MENGE  VALUE '            00.00',
        MEINS  LIKE  ZBSEG-MEINS  VALUE '/',
        VALUT  LIKE  ZBSEG-VALUT  VALUE '/',
        ZFBDT  LIKE  ZBSEG-ZFBDT  VALUE '/',
        ZUONR  LIKE  ZBSEG-ZUONR  VALUE '/',
        SGTXT  LIKE  ZBSEG-SGTXT,
        SKFBT  LIKE  ZBSEG-SKFBT,
        WSKTO  LIKE  ZBSEG-WSKTO  VALUE '               0',
        ZTERM  LIKE  ZBSEG-ZTERM  VALUE 'N00',
        ZBD1T  LIKE  ZBSEG-ZBD1T  VALUE '/',
        ZBD1P  LIKE  ZBSEG-ZBD1P  VALUE '/',
        ZBD2T  LIKE  ZBSEG-ZBD2T  VALUE '/',
        ZBD2P  LIKE  ZBSEG-ZBD2P  VALUE '/',
        ZBD3T  LIKE  ZBSEG-ZBD3T  VALUE '/',
        ZLSPR  LIKE  ZBSEG-ZLSPR  VALUE '/',
        ZLSCH  LIKE  ZBSEG-ZLSCH  VALUE '/',
        ZBFIX  LIKE  ZBSEG-ZBFIX  VALUE '/',
        QSSKZ  LIKE  ZBSEG-QSSKZ  VALUE '/',
        QSSHB  LIKE  ZBSEG-QSSHB  VALUE '/',
        QSFBT  LIKE  ZBSEG-QSFBT  VALUE '/',
        REGUL  LIKE  ZBSEG-REGUL  VALUE '/',
        NAME1  LIKE  ZBSEG-NAME1  VALUE '/',
        NAME2  LIKE  ZBSEG-NAME2  VALUE '/',
        NAME3  LIKE  ZBSEG-NAME3  VALUE '/',
        NAME4  LIKE  ZBSEG-NAME4  VALUE '/',
        STRAS  LIKE  ZBSEG-STRAS  VALUE '/',
        ORT01  LIKE  ZBSEG-ORT01  VALUE '/',
        PSTLZ  LIKE  ZBSEG-PSTLZ  VALUE '/',
        LAND1  LIKE  ZBSEG-LAND1  VALUE '/',
        REGIO  LIKE  ZBSEG-REGIO  VALUE '/',
        STCD1  LIKE  ZBSEG-STCD1  VALUE '/',
        STCD2  LIKE  ZBSEG-STCD2  VALUE '/',
        PFACH  LIKE  ZBSEG-PFACH  VALUE '/',
        PSTL2  LIKE  ZBSEG-PSTL2  VALUE '/',
        SPRAS  LIKE  ZBSEG-SPRAS  VALUE '/',
        HKONT  LIKE  ZBSEG-HKONT,
        FWBAS  LIKE  ZBSEG-FWBAS  VALUE '/',
        PROJK  LIKE  ZBSEG-PROJK,
        UZAWE  LIKE  ZBSEG-UZAWE  VALUE '/',
        SENDE  LIKE  ZBSEG-SENDE  VALUE '/',
      END OF LITEM.

DATA: WRK_SYMBOLIC(4) TYPE C VALUE '$sys',
      WRK_AMOUNT      TYPE P DECIMALS 2,
      KOUNT           TYPE I.


SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP 1.
PARAMETER:  INFILE LIKE FILENAME-FILEEXTERN
              DEFAULT '/usr/sap/interfaces/$sys/IFAP015/phhinv.chk'.
SELECTION-SCREEN SKIP 1.
PARAMETER: OUTFILE LIKE FILENAME-FILEEXTERN
              DEFAULT '/usr/sap/interfaces/$sys/IFAP015/phhinv.sap'.
SELECTION-SCREEN SKIP 1.
PARAMETER:
 P_TCODE(4)                 DEFAULT 'FB01',
 P_BLART  LIKE  BBKPF-BLART DEFAULT 'K3',
 P_WAERS  LIKE  BBKPF-WAERS DEFAULT 'CAD',
 P_XMWST  LIKE  BBKPF-XMWST DEFAULT 'X'.               "TR846
SELECTION-SCREEN END OF BLOCK BOX1.

AT SELECTION-SCREEN OUTPUT.
  REPLACE WRK_SYMBOLIC WITH SY-SYSID INTO: INFILE, OUTFILE.
  CONDENSE: INFILE, OUTFILE NO-GAPS.


START-OF-SELECTION.
*Replace system id in the file paths
 REPLACE WRK_SYMBOLIC WITH SY-SYSID INTO: INFILE, OUTFILE.
 CONDENSE INFILE  NO-GAPS.
 CONDENSE OUTFILE NO-GAPS.

*Open Input File
  OPEN DATASET INFILE FOR INPUT IN TEXT MODE MESSAGE MSG.
  IF ( SY-SUBRC <> 0 ).
    MESSAGE E002 WITH INFILE MSG.
  ENDIF.

*Open Output File
  OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE MESSAGE MSG.
  IF ( SY-SUBRC <> 0 ).
    MESSAGE E002 WITH OUTFILE MSG.
  ENDIF.

* Do until the end of file.
  DO.
    READ DATASET INFILE INTO INREC.

*   If file is empty, stop run.
    IF ( SY-INDEX = 1 ).
      IF ( SY-SUBRC = 4 ).
        MESSAGE I028.
        STOP.
      ENDIF.
    ENDIF.

*   When end of file, Exit.
    IF SY-SUBRC <> 0.
       EXIT.
    ENDIF.

    CASE INREC(1).
*Output file document header records
      WHEN '1'.
           perform init_structures using 'BGR00'.
           HEADERIN = INREC.
           z_bgr00-stype = '0'.
           z_bgr00-GROUP = HEADERIN-BATCHID.
           z_bgr00-mandt = SY-MANDT.
           z_bgr00-usnam = 'BATCH'.
           TRANSFER z_bgr00 TO OUTFILE.

           perform init_structures using 'BBKPF'.
           z_bbkpf-stype = '1'.
           z_bbkpf-tcode = P_TCODE.
           z_bbkpf-blart = P_BLART.
           z_bbkpf-waers = P_WAERS.
           IF P_XMWST = 'X'.                        "TR846
              z_bbkpf-xmwst = P_XMWST.              "TR846
           ENDIF.                                   "TR846
           z_bbkpf-BLDAT = HEADERIN-POSTDATE.
           z_bbkpf-BUKRS = HEADERIN-COMPCODE.
           z_bbkpf-BUDAT = HEADERIN-POSTDATE.
           z_bbkpf-XBLNR = HEADERIN-REFDOC.
           TRANSFER z_bbkpf TO OUTFILE.
*Output file document line item records
      WHEN '2'.
           DETAILIN = INREC.
           perform init_structures using 'ZBSEG'.
           z_zbseg-stype = '2'.
           z_zbseg-tbnam = 'ZBSEG'.
           z_zbseg-zterm = 'N00'.
           z_zbseg-menge = '00.00'.
           z_zbseg-wskto = '0'.

           z_zbseg-NEWBS = DETAILIN-POSTKEY.
           MOVE DETAILIN-AMOUNT TO WRK_AMOUNT.
           MOVE WRK_AMOUNT      TO z_zbseg-WRBTR.
           SHIFT z_zbseg-WRBTR RIGHT DELETING TRAILING SPACE.
           IF DETAILIN-TAXCODE = SPACE.
              z_zbseg-MWSKZ = '/'.
           ELSE.
              z_zbseg-MWSKZ = DETAILIN-TAXCODE.
           ENDIF.
           IF DETAILIN-GLACCT = '256950'.
              z_zbseg-XSKRL = 'X'.
           ELSE.
              z_zbseg-XSKRL = '/'.
           ENDIF.
           IF DETAILIN-INTORDER = SPACE.
              z_zbseg-AUFNR = '/'.
           ELSE.
              z_zbseg-AUFNR = DETAILIN-INTORDER.
           ENDIF.
*           IF DETAILIN-PHHTXT = SPACE.
*              z_zbseg-SGTXT  = '/'.
*           ELSE.
              MOVE DETAILIN-PHHTXT TO z_zbseg-SGTXT.
              KOUNT = 0.
              WHILE KOUNT < 147.
                 IF DETAILIN-PHHTXT+KOUNT(1) = SPACE.
                    SHIFT z_zbseg-SGTXT LEFT.
                    KOUNT = KOUNT + 1.
                 ELSE.
                    MOVE 147 TO KOUNT.
                 ENDIF.
              ENDWHILE.
*           ENDIF.
           CLEAR WRK_AMOUNT.
           MOVE DETAILIN-DISCOUNT TO WRK_AMOUNT.
           MOVE WRK_AMOUNT        TO z_zbseg-SKFBT.
           SHIFT z_zbseg-SKFBT RIGHT DELETING TRAILING SPACE.
           IF DETAILIN-POSTKEY = '21' OR DETAILIN-POSTKEY = '31'.
              z_zbseg-HKONT = DETAILIN-VENDOR.
           ELSEIF DETAILIN-GLACCT = SPACE.
                  z_zbseg-HKONT = '/'.
           ELSE.
                  z_zbseg-HKONT = DETAILIN-GLACCT.
           ENDIF.
           IF DETAILIN-WBS = SPACE.
              z_zbseg-PROJK = '/'.
           ELSE.
              z_zbseg-PROJK = DETAILIN-WBS.
           ENDIF.
           TRANSFER z_zbseg TO OUTFILE.

           CLEAR: z_zbseg-NEWBS, z_zbseg-WRBTR, z_zbseg-MWSKZ,
           z_zbseg-XSKRL,
                  z_zbseg-AUFNR, z_zbseg-SGTXT, z_zbseg-SKFBT,
                  z_zbseg-HKONT,
                   z_zbseg-PROJK.
      WHEN OTHERS.
    ENDCASE.
  ENDDO.
   CLOSE DATASET OUTFILE.

   CLEAR MSG.
   MOVE '**** OUTPUT FILE CREATED *****' TO MSG.
   MESSAGE I019 WITH MSG.

********************* END OF PROGRAM ********************

*----------------------- INIT_STRUCTURES ------------------------------
* used to initialize the record to '/'.
*----------------------------------------------------------------------
form init_structures using tablenam.
  select * from DD03L where tabname = tablenam.
    clear char.
    char(2) = 'Z_'.
    char+2(5) = tablenam.
    char+7(1) = '-'.
    char+8 = dd03l-fieldname.
    assign (char) to <F1>.
    <F1> = nodata.
  endselect.
endform.
