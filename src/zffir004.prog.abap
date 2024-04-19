REPORT ZFFIR004 MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-SIZE 255.
*
*  THIS ABAP IS USED IN BOTH THE EAST AND THE WEST.  PLEASE MAKE SAME
*  CHANGES TO BOTH
*
************************************************************************
* 2008/06/11 mdemeest TR342 Multiple company code selection
* 2003/06/04 mdemeest 1019 Eliminate recurring entries template
* 2003/05/23 mdemeest 1019 Copied from EAST SAP and removed selections
*                          from variant S_MATNR, S_AUFNR and S_KOSTL and
*                          S_HKONT
************************************************************************
TABLES: BKPF, BSEG,
        LFA1,               "Vendor Name
        LFB1,
        MAKT.               "Material Description

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-001.
select-options: S_BUKRS for BKPF-BUKRS memory id BUK OBLIGATORY.  "TR342
PARAMETER:      P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4) OBLIGATORY.
*              P_MONAT LIKE BKPF-MONAT OBLIGATORY DEFAULT SY-DATUM+4(2).
select-options: s_monat for bkpf-monat obligatory default '01' to '12'.
SELECT-OPTIONS: S_BLART FOR  BKPF-BLART,
*                S_HKONT FOR  BSEG-HKONT,
                S_LIFNR FOR BSEG-LIFNR.
*
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-031.
PARAMETER:      P_cocd radiobutton          group REPT,
                p_vend radiobutton          group REPT.
SELECTION-SCREEN END OF BLOCK BOX4.


DATA: WA-NAME  LIKE LFA1-NAME1.
DATA: WA-ZWELS LIKE LFB1-ZWELS.
DATA: WA-MAKTX LIKE MAKT-MAKTX.
DATA: PRV-BELNR LIKE BKPF-BELNR.
DATA: PRV-LIFNR LIKE BSEG-LIFNR.
DATA: TMP-LIFNR LIKE BSEG-LIFNR.
DATA: ITEM-COUNT(7) TYPE N VALUE 0.
DATA: ITEM-TOTAL(9) TYPE N VALUE 0.
DATA: DOLLAR-TOTAL(13) TYPE P DECIMALS 2 value 0.
DATA: GRAND-TOTAL(13) TYPE P DECIMALS 2 value 0.
DATA: PPRV-BELNR LIKE BKPF-BELNR.

DATA: BEGIN OF WA OCCURS 50000,
      LIFNR    LIKE BSEG-LIFNR,
      BUKRS LIKE BKPF-BUKRS,
      BELNR LIKE BKPF-BELNR,
      GJAHR LIKE BKPF-GJAHR,
      BLDAT LIKE BKPF-BLDAT,
      MONAT LIKE BKPF-MONAT,
      BLART LIKE BKPF-BLART,

      SHKZG(1) TYPE C,
      DMBTR    LIKE BSEG-DMBTR,
      KOSTL    LIKE BSEG-KOSTL,
      PROJK    LIKE BSEG-PROJK,
      AUFNR    LIKE BSEG-AUFNR,
      SAKNR    LIKE BSEG-SAKNR,
      HKONT    LIKE BSEG-HKONT,

      MATNR    LIKE BSEG-MATNR,
      SGTXT    LIKE BSEG-SGTXT,
      itemcnt(4) type i,
      END OF WA.

*perform open_output_file.

SELECT * FROM BKPF                       "Selection based on fiscal year
   WHERE BUKRS in s_BUKRS                 "and month              "TR342
     AND GJAHR = P_GJAHR
     and monat in s_monat                                      "99/06/07
     AND BLART IN S_BLART
     and tcode <> 'FBD1'.         "Eliminates recurring entries template

   PERFORM MOVE_BKPF_TO_WA.                      "Move info to work area

   PRV-BELNR = WA-BELNR.
   PRV-LIFNR = SPACE.

   SELECT * FROM BSEG
     WHERE BUKRS = BKPF-BUKRS
       AND BELNR = BKPF-BELNR
       AND GJAHR = BKPF-GJAHR
       AND LIFNR IN S_LIFNR.
     PERFORM MOVE_BSEG_TO_WA.                    "Move info to work area

  ENDSELECT.                                     "END of BSEG
ENDSELECT.                                       "END of BKPF

SORT WA BY LIFNR ASCENDING bukrs.                             "TR342

WRITE: /1 TEXT-013, 15 TEXT-014, 50 text-030, 65 TEXT-007, 86 TEXT-010.
.

if p_cocd = 'X'.
   perform Totals_by_Companycode.
else.
   perform Totals_by_Vendor.
endif.


form Totals_by_Companycode.
loop at wa.
 at new lifnr.
   select single name1 from lfa1 into wa-name
     where lifnr = wa-lifnr.
 endat.
 at end of bukrs.
    sum.
    write: /1 wa-lifnr, 15 wa-name, 50 wa-bukrs,
             65 wa-itemcnt no-zero,
             86 wa-dmbtr no-zero.
 endat.
 at end of lifnr.
    sum.
    write: /1 'Total for', wa-name,
             65 wa-itemcnt no-zero,
             86 wa-dmbtr no-zero.
    skip 2.
 endat.
 at last.
    sum.
    write: /1 'Grand Total',
            65 wa-itemcnt no-zero,
            86 wa-dmbtr no-zero.
 endat.


endloop.
endform.

form Totals_by_Vendor.
loop at wa.
 at new lifnr.
   select single name1 from lfa1 into wa-name
     where lifnr = wa-lifnr.
 endat.

 at end of lifnr.
    sum.
    write: /1 wa-lifnr, 15 wa-name,
             65 wa-itemcnt no-zero,
             86 wa-dmbtr no-zero.
 endat.

 at last.
    sum.
    write: /1 'Grand Total',
            65 wa-itemcnt no-zero,
            86 wa-dmbtr no-zero.
 endat.


endloop.
endform.




FORM MOVE_BKPF_TO_WA.
  CLEAR WA.
  MOVE: BKPF-BUKRS TO WA-BUKRS,
        BKPF-BELNR TO WA-BELNR,
        BKPF-GJAHR TO WA-GJAHR,
        BKPF-BLDAT TO WA-BLDAT,
        BKPF-MONAT TO WA-MONAT,
        BKPF-BLART TO WA-BLART.

ENDFORM.

*---------------------- MOVE_BSET_TO_WA  -------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_BSEG_TO_WA.
  CLEAR: WA-SHKZG, WA-DMBTR, WA-KOSTL, WA-PROJK,
         WA-AUFNR, WA-SAKNR, WA-HKONT, WA-LIFNR,
         WA-MATNR, WA-SGTXT.
  MOVE BSEG-DMBTR      TO WA-DMBTR.
  IF BSEG-SHKZG = 'H'.
     compute wa-dmbtr = wa-dmbtr * -1.
  .

*     WA-SHKZG = '-'.
  ENDIF.

  MOVE BSEG-KOSTL      TO WA-KOSTL.
  MOVE BSEG-PROJK      TO WA-PROJK.
  MOVE BSEG-AUFNR      TO WA-AUFNR.
  MOVE BSEG-SAKNR      TO WA-SAKNR.
  MOVE BSEG-HKONT      TO WA-HKONT.
  MOVE BSEG-LIFNR      TO WA-LIFNR.

  MOVE BSEG-MATNR      TO WA-MATNR.
  MOVE BSEG-SGTXT      TO WA-SGTXT.

IF WA-LIFNR <> SPACE.
     move '1' to wa-itemcnt.
     APPEND WA.
  ENDIF.

ENDFORM.
