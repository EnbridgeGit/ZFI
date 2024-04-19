REPORT ZFFII007 MESSAGE-ID ZS.
************************************************************************
* 2002/01/24 mdemeest #--- Use RFC  RKE_CONVERT_PAOBJNR_COPADATA
* 2002/01/22 mdemeest #--- Select from CE1UE01 changed
* 2002/01/14 mdemeest #--- Copied IFFI004 as starting point and added
*                          additional fields required for UEC
*
************************************************************************
* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER: PHYFILE LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/BKPF.SAP'.
PARAMETER: RECSIZE TYPE I.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETER: PHYFILE2 LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/BSEG.SAP'.
PARAMETER: RECSIZE2 TYPE I.
SELECTION-SCREEN END OF BLOCK BOX2.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
PARAMETER: P_BUKRS LIKE BKPF-BUKRS DEFAULT 'UGL'.
PARAMETER: P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4).
PARAMETER: P_MONAT LIKE BKPF-MONAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX3.

TABLES: BKPF, BSEG, ce1ue01.

data: begin of wa_copadata  occurs 50.
      include structure copadata.
data: end of wa_copadata.
* Global Data Definitions
DATA: RECORD(144).
DATA: RECORD2(259).

DATA: BEGIN OF WABKPF OCCURS 50000,
      MANDT LIKE BKPF-MANDT,
      BUKRS LIKE BKPF-BUKRS,
      BELNR LIKE BKPF-BELNR,
      GJAHR LIKE BKPF-GJAHR,
      BLART LIKE BKPF-BLART,
      BLDAT LIKE BKPF-BLDAT,
      BUDAT LIKE BKPF-BUDAT,
      MONAT LIKE BKPF-MONAT,
      CPUDT LIKE BKPF-CPUDT,
*     CPUTM LIKE BKPF-CPUTM,
*     AEDAT LIKE BKPF-AEDAT,
*     UPDDT LIKE BKPF-UPDDT,
      WWERT LIKE BKPF-WWERT, "Required by UEC
      USNAM LIKE BKPF-USNAM, "Required - see note above 2001/09/19
      TCODE LIKE BKPF-TCODE,
*     BVORG LIKE BKPF-BVORG,
      XBLNR LIKE BKPF-XBLNR,
*     DBBLG LIKE BKPF-DBBLG,
      STBLG LIKE BKPF-STBLG,
      STJAH LIKE BKPF-STJAH,
      BKTXT LIKE BKPF-BKTXT,
      WAERS LIKE BKPF-WAERS,
      KURSF(10) TYPE C,      " like bkpf-kursf,
*     KZWRS LIKE BKPF-KZWRS,
*     KZKRS(10) TYPE C,      " like bkpf-kzkrs,
*     BSTAT LIKE BKPF-BSTAT,
*     XNETB LIKE BKPF-XNETB,
      FRATH(16) TYPE C,     " like bkpf-frath,
*     XRUEB LIKE BKPF-XRUEB,
*     GLVOR LIKE BKPF-GLVOR,
      GRPID LIKE BKPF-GRPID,  "Required for UEC
*     DOKID LIKE BKPF-DOKID,
*     ARCID LIKE BKPF-ARCID,
*     IBLAR LIKE BKPF-IBLAR,
*     AWTYP LIKE BKPF-AWTYP,
*     AWKEY LIKE BKPF-AWKEY,
*     FIKRS LIKE BKPF-FIKRS,
      HWAER LIKE BKPF-HWAER,
*     HWAE2 LIKE BKPF-HWAE2,
*     HWAE3 LIKE BKPF-HWAE3,
*     KURS2(10) TYPE C,      "  like bkpf-kurs2,
*     KURS3(10) TYPE C,      "  like bkpf-kurs3,
*     BASW2 LIKE BKPF-BASW2,
*     BASW3 LIKE BKPF-BASW3,
*     UMRD2 LIKE BKPF-UMRD2,
*     UMRD3 LIKE BKPF-UMRD3,
      XSTOV LIKE BKPF-XSTOV,  "Required for UEC
      STODT LIKE BKPF-STODT,  "Required for UEC
      STGRD like BKPF-STGRD,  "Required for UEC
      END OF WABKPF.

DATA: BEGIN OF WABSEG OCCURS 100000,
      MANDT    LIKE BSEG-MANDT,
      BUKRS    LIKE BSEG-BUKRS,
      BELNR    LIKE BSEG-BELNR,
      GJAHR    LIKE BSEG-GJAHR,
      BSCHL    LIKE BSEG-BSCHL,
      KOART    LIKE BSEG-KOART,
      SHKZG    LIKE BSEG-SHKZG,
*     gsber    like bseg-gsber,                " per TIM
      DMBTR(16) TYPE C,                        " like bseg-dmbtr,
      MWSTS(16) TYPE C,                        " like bseg-mwsts,
      FWZUZ(16) TYPE C,                        " like bseg-fwzuz,
*     fillx(13) type c,                        " per TIM
      VORGN    LIKE BSEG-VORGN,
*     kokrs    like bseg-kokrs,                " per TIM
      KOSTL    LIKE BSEG-KOSTL,
      PROJN    LIKE BSEG-PROJN,
      AUFNR    LIKE BSEG-AUFNR,
      VBELN    LIKE BSEG-VBELN,
      VBEL2    LIKE BSEG-VBEL2,
      SAKNR    LIKE BSEG-SAKNR,
      HKONT    LIKE BSEG-HKONT,
      kunnr    like bseg-kunnr,                " per TIM
      LIFNR    LIKE BSEG-LIFNR,
      SKNTO(16) TYPE C,                        " like bseg-sknto,
      MWSK1    LIKE BSEG-MWSK1,
      DMBT1(18) TYPE C,                        " like bseg-dmbt1,
      mwsk2    like bseg-mwsk2,                " per TIM
      dmbt2(18) type c,                        " per TIM
*     mwsk3    like bseg-mwsk3,                " per TIM
*     dmbt3(18) type c,                        " per TIM
*     fill2(13) type c,                        " per TIM
      REBZG    LIKE BSEG-REBZG,
*     zollt    like bseg-zollt,                " per TIM
*     zolld    like bseg-zolld,                " per TIM
      REWRT(18) TYPE C,                        " like bseg-rewrt,
      buzei    like bseg-buzei,
      augdt    like bseg-augdt,
      augbl    like bseg-augbl,
      wrbtr(18) type c,
      sgtxt    like bseg-sgtxt,
      pernr    like bseg-pernr,
      matnr    like bseg-matnr,
*      menge    like bseg-menge,
      menge(16) type c,
      meins    like bseg-meins,
      paobjnr  like bseg-paobjnr,
      wwbrc    like ce1ue01-wwbrc,
      wwdst    like ce1ue01-wwdst,
      wwcsg    like ce1ue01-wwcsg,
      wwprl    like ce1ue01-wwprl,
      END OF WABSEG.

PERFORM OPEN_OUTPUT_FILE.

SELECT * FROM BKPF                       "Selection based on fiscal year
   WHERE BUKRS = P_BUKRS                 "and month
     AND GJAHR = P_GJAHR
     AND MONAT = P_MONAT.

   PERFORM MOVE_INFO_BKPF.                       "Move info to work area
   TRANSFER WABKPF TO PHYFILE  LENGTH RECSIZE.   "Output BKPF

   SELECT * FROM BSEG
     WHERE BUKRS = BKPF-BUKRS
       AND BELNR = BKPF-BELNR
       AND GJAHR = BKPF-GJAHR.

     PERFORM MOVE_INFO_BSEG.                     "Move info to work area
     TRANSFER WABSEG TO PHYFILE2 LENGTH RECSIZE2."Output BSEG
  ENDSELECT.                                     "END of BSEG
ENDSELECT.                                       "END of BKPF

CLOSE DATASET: PHYFILE, PHYFILE2.
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
* endif.

*  call function 'FILE_GET_NAME'                      "BSEG
*     exporting
*        logical_filename        = lfile2
*     importing
*        file_name               = phyfile2
*     exceptions
*        file_not_found          = 1
*        others                  = 2.
*  if sy-subrc <> 0.
*     message e018.
*  else.
      OPEN DATASET PHYFILE2 FOR OUTPUT IN TEXT MODE.
* endif.
ENDFORM.
*---------------------- MOVE_INFO_BKPF ---------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_INFO_BKPF.
  CLEAR WABKPF.
  MOVE: BKPF-MANDT TO WABKPF-MANDT,
        BKPF-BUKRS TO WABKPF-BUKRS,
        BKPF-BELNR TO WABKPF-BELNR,
        BKPF-GJAHR TO WABKPF-GJAHR,
        BKPF-BLART TO WABKPF-BLART,
        BKPF-BLDAT TO WABKPF-BLDAT,
        BKPF-BUDAT TO WABKPF-BUDAT,
        BKPF-MONAT TO WABKPF-MONAT,
        BKPF-CPUDT TO WABKPF-CPUDT,
*       BKPF-CPUTM TO WABKPF-CPUTM,
*       BKPF-AEDAT TO WABKPF-AEDAT,
*       BKPF-UPDDT TO WABKPF-UPDDT,
        BKPF-WWERT TO WABKPF-WWERT,
        BKPF-USNAM TO WABKPF-USNAM,   "2001/09/19
        BKPF-TCODE TO WABKPF-TCODE,
*       BKPF-BVORG TO WABKPF-BVORG,
        BKPF-XBLNR TO WABKPF-XBLNR,
*       BKPF-DBBLG TO WABKPF-DBBLG,
        BKPF-STBLG TO WABKPF-STBLG,
        BKPF-STJAH TO WABKPF-STJAH,
        BKPF-BKTXT TO WABKPF-BKTXT,
        BKPF-WAERS TO WABKPF-WAERS,
        BKPF-KURSF TO WABKPF-KURSF,
*       BKPF-KZWRS TO WABKPF-KZWRS,
*       BKPF-KZKRS TO WABKPF-KZKRS,
*       BKPF-BSTAT TO WABKPF-BSTAT,
*       BKPF-XNETB TO WABKPF-XNETB,
        BKPF-FRATH TO WABKPF-FRATH,
*       BKPF-XRUEB TO WABKPF-XRUEB,
*       BKPF-GLVOR TO WABKPF-GLVOR,
        BKPF-GRPID TO WABKPF-GRPID,
*       BKPF-DOKID TO WABKPF-DOKID,
*       BKPF-ARCID TO WABKPF-ARCID,
*       BKPF-IBLAR TO WABKPF-IBLAR,
*       BKPF-AWTYP TO WABKPF-AWTYP,
*       BKPF-AWKEY TO WABKPF-AWKEY,
*       BKPF-FIKRS TO WABKPF-FIKRS,
        BKPF-HWAER TO WABKPF-HWAER,
*       BKPF-HWAE2 TO WABKPF-HWAE2,
*       BKPF-HWAE3 TO WABKPF-HWAE3,
*       BKPF-KURS2 TO WABKPF-KURS2,
*       BKPF-KURS3 TO WABKPF-KURS3,
*       BKPF-BASW2 TO WABKPF-BASW2,
*       BKPF-BASW3 TO WABKPF-BASW3,
*       BKPF-UMRD2 TO WABKPF-UMRD2,
*       BKPF-UMRD3 TO WABKPF-UMRD3,
        BKPF-XSTOV TO WABKPF-XSTOV,
        BKPF-STODT TO WABKPF-STODT,
        BKPF-STGRD to WABKPF-STGRD.

ENDFORM.

*---------------------- MOVE_INFO_BSEG ---------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_INFO_BSEG.
  CLEAR WABSEG.
  MOVE BSEG-MANDT      TO WABSEG-MANDT.
  MOVE BSEG-BUKRS      TO WABSEG-BUKRS.
  MOVE BSEG-BELNR      TO WABSEG-BELNR.
  MOVE BSEG-GJAHR      TO WABSEG-GJAHR.
  MOVE BSEG-BSCHL      TO WABSEG-BSCHL.
  MOVE BSEG-KOART      TO WABSEG-KOART.
  MOVE BSEG-SHKZG      TO WABSEG-SHKZG.
* move bseg-gsber      to wabseg-gsber.
  MOVE BSEG-DMBTR      TO WABSEG-DMBTR.
  MOVE BSEG-MWSTS      TO WABSEG-MWSTS.
  MOVE BSEG-FWZUZ      TO WABSEG-FWZUZ.
  MOVE BSEG-VORGN      TO WABSEG-VORGN.
* move bseg-kokrs      to wabseg-kokrs.
  MOVE BSEG-KOSTL      TO WABSEG-KOSTL.
  MOVE BSEG-PROJN      TO WABSEG-PROJN.
  MOVE BSEG-AUFNR      TO WABSEG-AUFNR.
  MOVE BSEG-VBELN      TO WABSEG-VBELN.
  MOVE BSEG-VBEL2      TO WABSEG-VBEL2.
  move bseg-saknr      to wabseg-saknr.
  MOVE BSEG-HKONT      TO WABSEG-HKONT.
  move bseg-kunnr      to wabseg-kunnr.
  MOVE BSEG-LIFNR      TO WABSEG-LIFNR.
  MOVE BSEG-SKNTO      TO WABSEG-SKNTO.
  MOVE BSEG-MWSK1      TO WABSEG-MWSK1.
  MOVE BSEG-DMBT1      TO WABSEG-DMBT1.
  move bseg-mwsk2      to wabseg-mwsk2.
  move bseg-dmbt2      to wabseg-dmbt2.
* move bseg-mwsk3      to wabseg-mwsk3.
* move bseg-dmbt3      to wabseg-dmbt3.
  MOVE BSEG-REBZG      TO WABSEG-REBZG.
* move bseg-zollt      to wabseg-zollt.
* move bseg-zolld      to wabseg-zolld.
  MOVE BSEG-REWRT      TO WABSEG-REWRT.
  move bseg-buzei      to wabseg-buzei.
  move bseg-augdt      to wabseg-augdt.
  move bseg-augbl      to wabseg-augbl.
  move bseg-wrbtr      to wabseg-wrbtr.
  move bseg-sgtxt      to wabseg-sgtxt.
  move bseg-pernr      to wabseg-pernr.
  move bseg-matnr      to wabseg-matnr.
  move bseg-menge      to wabseg-menge.
  move bseg-meins      to wabseg-meins.
  move bseg-paobjnr    to wabseg-paobjnr.
  if  bseg-paobjnr = 0.
  else.
    perform get_copa_data.
  endif.
ENDFORM.

form get_copa_data.
   call function 'RKE_CONVERT_PAOBJNR_COPADATA'
      exporting
            bukrs   = bseg-bukrs
            paobjnr = bseg-paobjnr
      tables
            i_copadata = wa_copadata.

      if sy-subrc = '0'.
         loop at wa_copadata.
            if wa_copadata-fnam(2) = 'WW'.
               case wa_copadata-fnam.
                   when 'WWBRC'.
                      move wa_copadata-fval to wabseg-wwbrc.
                   when 'WWDST'.
                      move wa_copadata-fval to wabseg-wwdst.
                   when 'WWCSG'.
                      move wa_copadata-fval to wabseg-wwcsg.
                   when 'WWPRL'.
                      move wa_copadata-fval to wabseg-wwprl.
                endcase.
             endif.
          endloop.
        endif.
endform.








