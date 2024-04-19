REPORT ZFFII014 MESSAGE-ID ZS.
************************************************************************
* 2002/09/11 Mohammad Khan #--- Enlogics data extraction program.
*            This program has been cologned from ZFFII007.
*
*
************************************************************************
TABLES: BKPF, BSEG, ce1ue01.
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
PARAMETER P_BUKRS LIKE BKPF-BUKRS DEFAULT 'EINC'.
SELECT-OPTIONS: P_GJAHR FOR BKPF-GJAHR,
                P_MONAT FOR BKPF-MONAT.

SELECTION-SCREEN END OF BLOCK BOX3.


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
      CPUTM LIKE BKPF-CPUTM,                  "09/12/02
      AEDAT LIKE BKPF-AEDAT,                  "09/12/02
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
      XNETB LIKE BKPF-XNETB,                     "09/12/02
***   FRATH(16) TYPE C,     " like bkpf-frath,   "09/12/02
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
      BUZID    LIKE BSEG-BUZID,               "09/12/02
      BSCHL    LIKE BSEG-BSCHL,
      KOART    LIKE BSEG-KOART,
      UMSKZ    LIKE BSEG-UMSKZ,               "09/12/02
      UMSKS    LIKE BSEG-UMSKS,               "09/12/02
      SHKZG    LIKE BSEG-SHKZG,
      MWSKZ    LIKE BSEG-MWSKZ,               "09/12/02
*     gsber    like bseg-gsber,                " per TIM
      DMBTR(16) TYPE C,                        " like bseg-dmbtr,
      MWSTS(16) TYPE C,                        " like bseg-mwsts,
      FWZUZ(16) TYPE C,                        " like bseg-fwzuz,
*     fillx(13) type c,                        " per TIM
      MWART    LIKE BSEG-MWART,               "09/12/02
      TXGRP    LIKE BSEG-TXGRP,               "09/12/02
      KTOSL    LIKE BSEG-KTOSL,               "09/12/02
      ZUONR    LIKE BSEG-ZUONR,               "09/12/02
      ALTKT    LIKE BSEG-ALTKT,               "09/12/02
      VORGN    LIKE BSEG-VORGN,
*     kokrs    like bseg-kokrs,                " per TIM
      KOSTL    LIKE BSEG-KOSTL,
***   PROJN    LIKE BSEG-PROJN,               "09/12/02
      AUFNR    LIKE BSEG-AUFNR,
***   VBELN    LIKE BSEG-VBELN,               "09/12/02
***   VBEL2    LIKE BSEG-VBEL2,               "09/12/02
      SAKNR    LIKE BSEG-SAKNR,
      HKONT    LIKE BSEG-HKONT,
      kunnr    like bseg-kunnr,                " per TIM
      LIFNR    LIKE BSEG-LIFNR,
      ZFBDT    LIKE BSEG-ZFBDT,               "09/12/02
      ZTERM    LIKE BSEG-ZTERM,               "09/12/02
      ZBD1T(3) TYPE N, "    LIKE BSEG-ZBD1T,          "09/12/02
*      ZBD1T    LIKE BSEG-ZBD1T,               "09/12/02
      SKNTO(16) TYPE C,                        " like bseg-sknto,
      MWSK1    LIKE BSEG-MWSK1,
      DMBT1(18) TYPE C,                        " like bseg-dmbt1,
      mwsk2    like bseg-mwsk2,                " per TIM
      dmbt2(18) type c,                        " per TIM
*     mwsk3    like bseg-mwsk3,                " per TIM
*     dmbt3(18) type c,                        " per TIM
*     fill2(13) type c,                        " per TIM
***   REBZG    LIKE BSEG-REBZG,               "09/12/02
*     zollt    like bseg-zollt,                " per TIM
*     zolld    like bseg-zolld,                " per TIM
***   REWRT(18) TYPE C,            "09/12/02   " like bseg-rewrt,
      buzei    like bseg-buzei,
      augdt    like bseg-augdt,
      augbl    like bseg-augbl,
      wrbtr(18) type c,
      sgtxt    like bseg-sgtxt,
      pernr    like bseg-pernr,
      matnr    like bseg-matnr,
*      menge    like bseg-menge,
      WERKS    LIKE BSEG-WERKS,                "09/12/02
      menge(16) type c,
      meins    like bseg-meins,
      paobjnr  like bseg-paobjnr,
      UZAWE    LIKE BSEG-UZAWE,                "09/12/02
      KNDNR    LIKE CE11100-KNDNR,             "09/12/02
      VKORG    LIKE CE11100-VKORG,             "09/12/02
      WWSCT    LIKE CE11100-WWSCT,             "09/12/02
      WWSEG    LIKE CE11100-WWSEG,             "09/12/02
      PRDHA    LIKE CE11100-PRDHA,             "09/12/02
      WWBRN    LIKE CE11100-WWBRN,             "09/12/02

***      wwbrc    like ce1ue01-wwbrc,          "09/12/02
***      wwdst    like ce1ue01-wwdst,          "09/12/02
***      wwcsg    like ce1ue01-wwcsg,          "09/12/02
***      wwprl    like ce1ue01-wwprl,          "09/12/02
      END OF WABSEG.

PERFORM OPEN_OUTPUT_FILE.

SELECT * FROM BKPF                       "Selection based on fiscal year
   WHERE BUKRS = P_BUKRS                 "and month
     AND GJAHR IN P_GJAHR
     AND MONAT IN P_MONAT.

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
* Routine to open the physical file
* for output in text mode.
************************************************************************
FORM OPEN_OUTPUT_FILE.
      OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
      OPEN DATASET PHYFILE2 FOR OUTPUT IN TEXT MODE.
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
        BKPF-CPUTM TO WABKPF-CPUTM,      "09/12/02
        BKPF-AEDAT TO WABKPF-AEDAT,      "09/12/02
*       BKPF-UPDDT TO WABKPF-UPDDT,
        BKPF-WWERT TO WABKPF-WWERT,
        BKPF-USNAM TO WABKPF-USNAM,      "2001/09/19
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
        BKPF-XNETB TO WABKPF-XNETB,           "09/12/02
***        BKPF-FRATH TO WABKPF-FRATH,        "09/12/02
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
  MOVE BSEG-BUZID      TO WABSEG-BUZID.       "09/12/02
  MOVE BSEG-BSCHL      TO WABSEG-BSCHL.
  MOVE BSEG-KOART      TO WABSEG-KOART.
  MOVE BSEG-UMSKZ      TO WABSEG-UMSKZ.       "09/12/02
  MOVE BSEG-UMSKS      TO WABSEG-UMSKS.       "09/12/02
  MOVE BSEG-SHKZG      TO WABSEG-SHKZG.
  MOVE BSEG-MWSKZ      TO WABSEG-MWSKZ.       "09/12/02
* move bseg-gsber      to wabseg-gsber.
  MOVE BSEG-DMBTR      TO WABSEG-DMBTR.
  MOVE BSEG-MWSTS      TO WABSEG-MWSTS.
  MOVE BSEG-FWZUZ      TO WABSEG-FWZUZ.
  MOVE BSEG-MWART      TO WABSEG-MWART.       "09/12/02
  MOVE BSEG-TXGRP      TO WABSEG-TXGRP.       "09/12/02
  MOVE BSEG-KTOSL      TO WABSEG-KTOSL.       "09/12/02
  MOVE BSEG-ZUONR      TO WABSEG-ZUONR.       "09/12/02
  MOVE BSEG-ALTKT      TO WABSEG-ALTKT.       "09/12/02
  MOVE BSEG-VORGN      TO WABSEG-VORGN.
* move bseg-kokrs      to wabseg-kokrs.
  MOVE BSEG-KOSTL      TO WABSEG-KOSTL.
***  MOVE BSEG-PROJN      TO WABSEG-PROJN.     "09/12/02
  MOVE BSEG-AUFNR      TO WABSEG-AUFNR.
***  MOVE BSEG-VBELN      TO WABSEG-VBELN.     "09/12/02
***  MOVE BSEG-VBEL2      TO WABSEG-VBEL2.     "09/12/02
  move bseg-saknr      to wabseg-saknr.
  MOVE BSEG-HKONT      TO WABSEG-HKONT.
  move bseg-kunnr      to wabseg-kunnr.
  MOVE BSEG-LIFNR      TO WABSEG-LIFNR.
  MOVE BSEG-ZFBDT      TO WABSEG-ZFBDT.       "09/12/02
  MOVE BSEG-ZTERM      TO WABSEG-ZTERM.       "09/12/02
  MOVE BSEG-ZBD1T      TO WABSEG-ZBD1T.       "09/12/02
  MOVE BSEG-SKNTO      TO WABSEG-SKNTO.
  MOVE BSEG-MWSK1      TO WABSEG-MWSK1.
  MOVE BSEG-DMBT1      TO WABSEG-DMBT1.
  move bseg-mwsk2      to wabseg-mwsk2.
  move bseg-dmbt2      to wabseg-dmbt2.
* move bseg-mwsk3      to wabseg-mwsk3.
* move bseg-dmbt3      to wabseg-dmbt3.
***  MOVE BSEG-REBZG      TO WABSEG-REBZG.     "09/12/02
* move bseg-zollt      to wabseg-zollt.
* move bseg-zolld      to wabseg-zolld.
***  MOVE BSEG-REWRT      TO WABSEG-REWRT.     "09/12/02
  move bseg-buzei      to wabseg-buzei.
  move bseg-augdt      to wabseg-augdt.
  move bseg-augbl      to wabseg-augbl.
  move bseg-wrbtr      to wabseg-wrbtr.
  move bseg-sgtxt      to wabseg-sgtxt.
  move bseg-pernr      to wabseg-pernr.
  move bseg-matnr      to wabseg-matnr.
  MOVE BSEG-WERKS      TO WABSEG-WERKS.       "09/12/02
  move bseg-menge      to wabseg-menge.
  move bseg-meins      to wabseg-meins.
  move bseg-paobjnr    to wabseg-paobjnr.
  MOVE BSEG-UZAWE      TO WABSEG-UZAWE.       "09/12/02
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
  if wa_copadata-fnam(2) = 'WW' or wa_copadata-fnam(2) = 'KN' or
     wa_copadata-fnam(2) = 'VK' or wa_copadata-fnam(2) = 'PR'.
               case wa_copadata-fnam.
                   when 'KNDNR'.
                      move wa_copadata-fval to wabseg-kndnr.
                   when 'PRDHA'.
                      move wa_copadata-fval to wabseg-prdha.
                   when 'VKORG'.
                      move wa_copadata-fval to wabseg-vkorg.
                   when 'WWSCT'.
                      move wa_copadata-fval to wabseg-wwsct.
                   when 'WWSEG'.
                      move wa_copadata-fval to wabseg-wwseg.
                   when 'WWBRN'.
                      move wa_copadata-fval to wabseg-wwbrn.
                endcase.
             endif.
          endloop.
        endif.
endform.








