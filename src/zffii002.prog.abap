REPORT ZFFII002 MESSAGE-ID ZS.
************************************************************************
* 2012/05/22 SAHMAD   #    New CRA requirements
* 2008/11/07 mdemeest TR636 Audit for November 2008 requested by
*                           Leo Marenette for TIM KETTLEWELL, CRA
* 2001/03/20 mdemeest #874  Selection by company code from LFB1
* 2001/03/15 mdemeest #---  Extract LFA1 from SAP into flat file for
*                           Revenue Canada
*
************************************************************************
TABLES: LFA1, LFB1.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER: PHYFILE LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/LFA1.SAP'.
PARAMETER: RECSIZE TYPE I default 300.
skip 1.
SELECT-OPTIONS: s_bukrs for lfb1-bukrs DEFAULT 'UGL'.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.


* Global Data Definitions
DATA: RECORD(300).

DATA: BEGIN OF WALFA1 OCCURS 100000,
      MANDT LIKE lfa1-MANDT,
      sp1   TYPE c VALUE '~',
      lifnr like lfa1-lifnr,
*      loevm like lfa1-loevm,
      sp2   TYPE c VALUE '~',
      name1 like lfa1-name1,
      sp3   TYPE c VALUE '~',
      name2 like lfa1-name2,
      sp4   TYPE c VALUE '~',
      name3 like lfa1-name3,
      sp5   TYPE c VALUE '~',
      name4 like lfa1-name4,
      sp6   TYPE c VALUE '~',
      stras like lfa1-stras,
      sp7   TYPE c VALUE '~',
      ort01 like lfa1-ort01,
*      ort02 like lfa1-ort02,
      sp8   TYPE c VALUE '~',
      regio like lfa1-regio,
      sp9   TYPE c VALUE '~',
      pstlz like lfa1-pstlz,
*      stceg like lfa1-stceg,
      sp10  TYPE c VALUE '~',
      land1 like lfa1-land1,
      sp11  TYPE c VALUE '~',
      stcd1 like lfa1-stcd1,
      sp12  TYPE c VALUE '~',
      stcd2 like lfa1-stcd2,
      END OF WALFA1.

DATA: BEGIN OF WALFA1_HEADER OCCURS 1,
      MANDT(5) value 'MANDT',
      sp1   TYPE c VALUE '~',
      lifnr(10) value 'LIFNR',
*      loevm like lfa1-loevm,
      sp2   TYPE c VALUE '~',
      name1(35) value 'NAME1',
      sp3   TYPE c VALUE '~',
      name2(35) value 'NAME2',
      sp4   TYPE c VALUE '~',
      name3(35) value 'NAME3',
      sp5   TYPE c VALUE '~',
      name4(35) value 'NAME4',
      sp6   TYPE c VALUE '~',
      stras(35) value 'STRAS',
      sp7   TYPE c VALUE '~',
      ort01(35) value 'ORT01',
*      ort02 like lfa1-ort02,
      sp8   TYPE c VALUE '~',
      regio(5) value 'REGIO',
      sp9   TYPE c VALUE '~',
      pstlz(10) value 'PSTLZ',
*      stceg like lfa1-stceg,
      sp10  TYPE c VALUE '~',
      land1(5) value 'LAND1',
      sp11  TYPE c VALUE '~',
      stcd1(16) value 'STCD1',
      sp12  TYPE c VALUE '~',
      stcd2(11) value 'STCD2',
      END OF WALFA1_header.


PERFORM OPEN_OUTPUT_FILE.

TRANSFER WALFA1_header TO PHYFILE  LENGTH RECSIZE.

Select * from lfb1
   where bukrs IN s_bukrs.

   SELECT * FROM lfa1
      where lifnr = lfb1-lifnr.

      PERFORM MOVE_INFO_LFA1.                       "Move info to wa

      TRANSFER WALFA1 TO PHYFILE  LENGTH RECSIZE.   "Output LFA1

   ENDSELECT.                                       "END of LFA1
endselect.                                          "End of LFB1
WRITE 'Process has been completed'.
CLOSE DATASET: PHYFILE.
************************************************************************
* Routine to convert the logical file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
************************************************************************
FORM OPEN_OUTPUT_FILE.

      OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.

ENDFORM.
*---------------------- MOVE_INFO_BKPF ---------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_INFO_LFA1.
  CLEAR WAlfa1.
  move-corresponding LFA1 to walfa1.
  walfa1-sp1 = '~'.
  walfa1-sp2 = '~'.
  walfa1-sp3 = '~'.
  walfa1-sp4 = '~'.
  walfa1-sp5 = '~'.
  walfa1-sp6 = '~'.
  walfa1-sp7 = '~'.
  walfa1-sp8 = '~'.
  walfa1-sp9 = '~'.
  walfa1-sp10 = '~'.
  walfa1-sp11 = '~'.
  walfa1-sp12 = '~'.

ENDFORM.
