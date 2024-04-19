REPORT ZFFII016 MESSAGE-ID ZS.
************************************************************************
* Description
* Extract G/L and AP data from SAP for Ernst & Young Audit
************************************************************************
* CHANGES
* 2003/04/14 mdemeest ---- Copied ZFFI004 and made changes for this
*                          particular audit
************************************************************************
TABLES: BKPF, BSEG ,
        SKAT,                 "G/L Account Master
        T001,
        payr,
        adrc,
        LFA1.                 "Vendor file

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER: PHYFILE LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/GL.SAP'.
PARAMETER: RECSIZE TYPE I.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETER: PHYFILE2 LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/AP.SAP'.
PARAMETER: RECSIZE2 TYPE I.
SELECTION-SCREEN END OF BLOCK BOX2.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
PARAMETER: P_BUKRS LIKE BKPF-BUKRS memory id BUK.
PARAMETER: P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4).
select-options: s_MONAT for BKPF-MONAT OBLIGATORY,
                s_blart for bkpf-blart.
SELECTION-SCREEN END OF BLOCK BOX3.


* Global Data Definitions
DATA: RECORD(144).
DATA: RECORD2(259).

data: txt20 like skat-txt20.
data: name1 like lfa1-name1.

DATA: BEGIN OF WA_GL OCCURS 50000,         "Total Record Length = 131
      BUKRS LIKE BKPF-BUKRS,               "l =  4
      BELNR LIKE BKPF-BELNR,               "l = 10
      GJAHR LIKE BKPF-GJAHR,               "l =  4
      monat like bkpf-monat,               "l =  2
      hkont like bseg-hkont,               "l = 10
      txt20 like skat-txt20,               "l = 20
      BLART LIKE BKPF-BLART,               "l =  2
      sgtxt like bseg-sgtxt,               "l = 50
      BUDAT LIKE BKPF-BUDAT,               "l =  8
      shkzg like bseg-shkzg,               "l =  1
      dmbtr(20) type c,                    "l = 20
      END OF WA_GL.

DATA: BEGIN OF WA_AP  OCCURS 100000,       "Total length = 260
      BUKRS    LIKE BKPF-BUKRS,            "L =  4
      BELNR    LIKE BKPF-BELNR,            "L = 10
      GJAHR    LIKE BSEG-GJAHR,            "L =  4
      MONAT    like bkpf-monat,            "L =  2
      BSCHL    LIKE BSEG-BSCHL,            "L =  2
      HKONT    like bseg-hkont,            "L = 10
      txt20    like skat-txt20,            "L = 20
      blart    like bkpf-blart,            "L =  2
      sgtxt    like bseg-sgtxt,            "L = 50
      bldat    like bkpf-bldat,            "L =  8
      budat    like bkpf-budat,            "L =  8
      LIFNR    LIKE BSEG-LIFNR,            "L = 10
      name1    like lfa1-name1,            "L = 35
      xblnr    like bkpf-xblnr,            "L = 16
      SHKZG    LIKE BSEG-SHKZG,            "L =  1
      DMBTR(20) TYPE C,                    "L = 20
      mwskz    like bseg-mwskz,            "L =  2
*     mdshb    like bseg-mdshb,                "NO SUCH FIELD on BSEG
*     mwshb    like bseg-mwshb,                "NO SUCH FIELD on BSEG
      chect    like payr-chect,            "L = 13
      zaldt    like payr-zaldt,            "L =  8
      region   like adrc-region,           "L =  3
      gsber    like bseg-gsber,            "L =  4
      kostl    like bseg-kostl,            "L = 10
      werks    like bseg-werks,            "L =  4
      clerkid  like bkpf-usnam,            "L = 12

      END OF WA_AP.

PERFORM OPEN_OUTPUT_FILE.

Select single * from T001
    where bukrs = p_bukrs.
if sy-subrc = '0'.
else.
   write: / 'Company Code', p_bukrs, 'entered in variant, not valid'.
endif.

SELECT * FROM BKPF                       "Selection based on fiscal year
   WHERE BUKRS = P_BUKRS                 "and month
     AND GJAHR = P_GJAHR
     AND MONAT in s_MONAT
     and blart in s_blart.
     clear: wa_ap, wa_gl.
     perform move_bkpf_gl.
     perform move_bkpf_ap.
     select * from bseg
         where bukrs = bkpf-bukrs
           and belnr = bkpf-belnr
           and gjahr = bkpf-gjahr.

         select single * from skat
            where spras = sy-langu
              and ktopl = t001-ktopl
              and saknr = bseg-hkont.
         clear txt20.
         if sy-subrc = '0'.
            move skat-txt20 to txt20.
         endif.


        PERFORM MOVE_INFO_GL.                 "Move info to work area
        TRANSFER WA_GL TO PHYFILE  LENGTH RECSIZE.     "Output GL file

        PERFORM MOVE_INFO_AP.                 "Move info to work area
        TRANSFER WA_AP TO PHYFILE2 LENGTH RECSIZE2.    "Output AP file
  ENDSELECT.                                     "END of BSEG
ENDSELECT.                                       "END of BKPF

CLOSE DATASET: PHYFILE, PHYFILE2.
************************************************************************
* Routine to convert the logical file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
************************************************************************
FORM OPEN_OUTPUT_FILE.
      OPEN DATASET PHYFILE  FOR OUTPUT IN TEXT MODE.
      OPEN DATASET PHYFILE2 FOR OUTPUT IN TEXT MODE.
* endif.
ENDFORM.
*---------------------- MOVE_INFO_BKPF ---------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_BKPF_GL.
  MOVE: BKPF-BUKRS TO WA_GL-BUKRS,
        BKPF-BELNR TO WA_GL-BELNR,
        BKPF-GJAHR TO WA_GL-GJAHR,
        BKPF-MONAT TO WA_GL-MONAT,
        BKPF-BLART TO WA_GL-BLART,
        BKPF-BUDAT TO WA_GL-BUDAT.
ENDFORM.

FORM MOVE_INFO_GL.
  move: BSEG-HKONT to WA_GL-HKONT,
        BSEG-SGTXT to WA_GL-SGTXT,
        BSEG-SHKZG to WA_GL-SHKZG,
        BSEG-DMBTR to WA_GL-DMBTR,
        txt20      to wa_gl-txt20.
ENDFORM.

*---------------------- MOVE_INFO_BSEG ---------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_BKPF_AP.
  MOVE Bkpf-GJAHR      TO WA_ap-GJAHR.
  move bkpf-monat      to wa_ap-monat.
  move bkpf-blart      to wa_ap-blart.
  move bkpf-bldat      to wa_ap-bldat.
  move bkpf-budat      to wa_ap-budat.
  move bkpf-xblnr      to wa_ap-xblnr.
  move bkpf-usnam      to wa_ap-clerkid.
ENDFORM.

FORM MOVE_INFO_AP.
  MOVE BSEG-BUKRS      TO WA_AP-BUKRS.
  MOVE BSEG-BELNR      TO WA_AP-BELNR.
  MOVE BSEG-BSCHL      TO WA_AP-BSCHL.
  MOVE BSEG-HKONT      TO WA_AP-HKONT.
  move txt20           to wa_ap-txt20.
  move bseg-sgtxt      to wa_ap-sgtxt.
  move bseg-lifnr      to wa_ap-lifnr.
  move bseg-shkzg      to wa_ap-shkzg.
  move bseg-dmbtr      to wa_ap-dmbtr.
  move bseg-mwskz      to wa_ap-mwskz.

  move bseg-gsber      to wa_ap-gsber.
  move bseg-kostl      to wa_ap-kostl.
  move bseg-werks      to wa_ap-werks.

  clear name1.
  if bseg-lifnr <> ' '.
     select single * from lfa1
        where lifnr = bseg-lifnr.
     if sy-subrc = '0'.
        move lfa1-name1 to wa_ap-name1.
        select single * from adrc
            where addrnumber = lfa1-adrnr.
            if sy-subrc = '0'.
               move adrc-region to wa_ap-region.
           endif.
     endif.
  endif.

  if bseg-augbl <> ' '.
     select single * from payr
         where zbukr = bseg-bukrs
           and vblnr = bseg-augbl.
     if sy-subrc = '0'.
        move payr-chect to wa_ap-chect.
        move payr-zaldt to wa_ap-zaldt.
     endif.
  endif.


ENDFORM.
