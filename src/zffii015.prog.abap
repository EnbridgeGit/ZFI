REPORT ZFFII015 MESSAGE-ID ZS.
************************************************************************
* 2001/03/20 mdemeest #874 Selection by company code from LFB1
* 2001/03/15 mdemeest #--- Extract LFA1 from SAP into flat file for
*                          Revenue Canada
*
************************************************************************
tables: lfa1, lfb1.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER: PHYFILE LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/LFA1.SAP'.
PARAMETER: RECSIZE TYPE I.
skip 1.
parameter: p_bukrs  like lfb1-bukrs obligatory memory id BUK.
select-options: s_ktokk for lfa1-ktokk no intervals.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
*SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
*PARAMETER: PHYFILE2 LIKE FILENAMECI-FILEEXTERN
*                    DEFAULT '/usr/sap/interfaces/P01/CFMM001/BSEG.SAP'.
*PARAMETER: RECSIZE2 TYPE I.
*SELECTION-SCREEN END OF BLOCK BOX2.
SKIP 1.
*SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
*PARAMETER: P_BUKRS LIKE BKPF-BUKRS DEFAULT 'UGL'.
*PARAMETER: P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4).
*PARAMETER: P_MONAT LIKE BKPF-MONAT OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK BOX3.



* Global Data Definitions
DATA: RECORD(184).

DATA: BEGIN OF WALFA1 OCCURS 50000,       "Length of record = 210
*      MANDT LIKE lfa1-MANDT,
      bukrs like lfb1-bukrs,              "l= 4
      name1 like lfa1-name1,              "l=35
      name2 like lfa1-name2,              "l=35
      lifnr like lfa1-lifnr,              "l=10
      stras like lfa1-stras,              "l=35
      ort01 like lfa1-ort01,              "l=35
      ort02 like lfa1-ort02,              "l=35
      regio like lfa1-regio,              "l= 3
      pstlz like lfa1-pstlz,              "l=10
      land1 like lfa1-land1,              "l= 3
      ktokk like lfa1-ktokk,              "l= 4
      loevm like lfa1-loevm,              "l= 1
      END OF WALFA1.


PERFORM OPEN_OUTPUT_FILE.

Select * from lfb1
   where bukrs = p_bukrs.

   SELECT * FROM lfa1
      where lifnr = lfb1-lifnr
        and ktokk in s_ktokk.

      PERFORM MOVE_INFO_LFA1.                       "Move info to wa
      TRANSFER WALFA1 TO PHYFILE  LENGTH RECSIZE.   "Output LFA1

   ENDSELECT.                                       "END of LFA1
endselect.                                          "End of LFB1

CLOSE DATASET: PHYFILE.
************************************************************************
* Routine to convert the logical file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
************************************************************************
FORM OPEN_OUTPUT_FILE.
*  call function 'FILE_GET_NAME'                        "BKPF
*     exporting
*        logical_filename        = lfile
*     importing
*        file_name               = phyfile
*     exceptions
*        file_not_found          = 1
*        others                  = 2.
*  if sy-subrc <> 0.
*     message e018.
*  else.
      OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.

ENDFORM.
*---------------------- MOVE_INFO_BKPF ---------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_INFO_LFA1.
  CLEAR WAlfa1.
  move: lfb1-bukrs to walfa1-bukrs.
  move: lfa1-lifnr to walfa1-lifnr,
        lfa1-loevm to walfa1-loevm,
        lfa1-name1 to walfa1-name1,
        lfa1-name2 to walfa1-name2,
        lfa1-stras to walfa1-stras,
        lfa1-ort01 to walfa1-ort01,
        lfa1-ort02 to walfa1-ort02,
        lfa1-regio to walfa1-regio,
        lfa1-pstlz to walfa1-pstlz,
        lfa1-land1 to walfa1-land1,
        lfa1-ktokk to walfa1-ktokk.

ENDFORM.





















