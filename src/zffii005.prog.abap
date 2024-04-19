REPORT zffii005 MESSAGE-ID zs.
************************************************************************
* 2012/05/22 SAHMAD   #    New CRA requirements
* 2008/11/07 mdemeest TR636 - Revenue Canada audit per Leo Marentette
*                             for Tim Kettlewell, CRA
*                             Account Masters
************************************************************************
TABLES: skat,
        ska1,
        skb1,
        glt0,
        faglflext.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER: phyfile LIKE filenameci-fileextern
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/SKAT.SAP'.
*PARAMETER: RECSIZE TYPE I DEFAULT 95.
SELECTION-SCREEN END OF BLOCK box.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETER: phyfile2 LIKE filenameci-fileextern
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/GLT0.SAP'.
*PARAMETER: RECSIZE2 TYPE I default 61.
SELECTION-SCREEN END OF BLOCK box2.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-004.
PARAMETER: phyfile3 LIKE filenameci-fileextern
               DEFAULT '/usr/sap/interfaces/P01/CFMM001/FAGLFLEXT.SAP'.
*PARAMETER: RECSIZE2 TYPE I default 61.
SELECTION-SCREEN END OF BLOCK box4.
SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_ktopl FOR skat-ktopl NO INTERVALS,
                s_bukrs FOR skb1-bukrs NO INTERVALS DEFAULT 'UGL',
                s_gjahr FOR glt0-ryear NO INTERVALS DEFAULT sy-datum(4),
                s_rldnr FOR glt0-rldnr NO INTERVALS DEFAULT '0L'.
SELECTION-SCREEN END OF BLOCK box3.



* Global Data Definitions
DATA: record(144).
DATA: record2(259).

DATA: recsize  TYPE i VALUE 130,
      recsize2 TYPE i VALUE 620,
      recsize3 TYPE i VALUE 620.

DATA: BEGIN OF waskat OCCURS 0,
      mandt LIKE skat-mandt,
      sp1   TYPE c VALUE '~',
      bukrs LIKE skb1-bukrs,
      sp2   TYPE c VALUE '~',
      ktopl LIKE skat-ktopl,
      sp3   TYPE c VALUE '~',
      saknr LIKE skat-saknr,
      sp4   TYPE c VALUE '~',
      txt20 LIKE skat-txt20,
      sp5   TYPE c VALUE '~',
      txt50 LIKE skat-txt50,
      sp6   TYPE c VALUE '~',
      ktoks LIKE ska1-ktoks,
      sp7   TYPE c VALUE '~',
      xbilk LIKE ska1-xbilk,
      sp8   TYPE c VALUE '~',
      bilkt LIKE ska1-bilkt,
      END OF waskat.

DATA: BEGIN OF waskat_header OCCURS 1,
      mandt(5) VALUE 'MANDT',
      sp1   TYPE c VALUE '~',
      bukrs(5) VALUE 'BUKRS',
      sp2   TYPE c VALUE '~',
      ktopl(5) VALUE 'KTOPL',
      sp3   TYPE c VALUE '~',
      saknr(10) VALUE 'SAKNR',
      sp4   TYPE c VALUE '~',
      txt20(20) VALUE 'TXT20',
      sp5   TYPE c VALUE '~',
      txt50(50) VALUE 'TXT50',
      sp6   TYPE c VALUE '~',
      ktoks(5) VALUE 'KTOKS',
      sp7   TYPE c VALUE '~',
      xbilk(5) VALUE 'XBILK',
      sp8   TYPE c VALUE '~',
      bilkt(10) VALUE 'BILKT',
      END OF waskat_header.

DATA: BEGIN OF waglt0 OCCURS 0,
      rldnr LIKE glt0-rldnr,
      sp1   TYPE c VALUE '~',
      rrcty LIKE glt0-rrcty,
      sp2   TYPE c VALUE '~',
      bukrs LIKE glt0-bukrs,
      sp3   TYPE c VALUE '~',
      ryear LIKE glt0-ryear,
      sp4   TYPE c VALUE '~',
      racct LIKE glt0-racct,
      sp5   TYPE c VALUE '~',
      rbusa LIKE glt0-rbusa,
      sp6   TYPE c VALUE '~',
      rtcur LIKE glt0-rtcur,
      sp7   TYPE c VALUE '~',
      drcrk LIKE glt0-drcrk,
      sp8   TYPE c VALUE '~',
      tslvt(15) TYPE c,      "LIKE GLT0-TSLVT,
      sp9   TYPE c VALUE '~',
      hslvt(15) TYPE c,      "LIKE GLT0-HSLVT,
      sp10   TYPE c VALUE '~',
      tsl01(15) TYPE c,
      sp11   TYPE c VALUE '~',
      tsl02(15) TYPE c,
      sp12   TYPE c VALUE '~',
      tsl03(15) TYPE c,
      sp13   TYPE c VALUE '~',
      tsl04(15) TYPE c,
      sp14   TYPE c VALUE '~',
      tsl05(15) TYPE c,
      sp15   TYPE c VALUE '~',
      tsl06(15) TYPE c,
      sp16   TYPE c VALUE '~',
      tsl07(15) TYPE c,
      sp17   TYPE c VALUE '~',
      tsl08(15) TYPE c,
      sp18   TYPE c VALUE '~',
      tsl09(15) TYPE c,
      sp19   TYPE c VALUE '~',
      tsl10(15) TYPE c,
      sp20   TYPE c VALUE '~',
      tsl11(15) TYPE c,
      sp21   TYPE c VALUE '~',
      tsl12(15) TYPE c,
      sp22   TYPE c VALUE '~',
      tsl13(15) TYPE c,
      sp23   TYPE c VALUE '~',
      tsl14(15) TYPE c,
      sp24   TYPE c VALUE '~',
      tsl15(15) TYPE c,
      sp25   TYPE c VALUE '~',
      tsl16(15) TYPE c,
      sp26   TYPE c VALUE '~',
      hsl01(15) TYPE c,
      sp27   TYPE c VALUE '~',
      hsl02(15) TYPE c,
      sp28   TYPE c VALUE '~',
      hsl03(15) TYPE c,
      sp29   TYPE c VALUE '~',
      hsl04(15) TYPE c,
      sp30   TYPE c VALUE '~',
      hsl05(15) TYPE c,
      sp31   TYPE c VALUE '~',
      hsl06(15) TYPE c,
      sp32   TYPE c VALUE '~',
      hsl07(15) TYPE c,
      sp33   TYPE c VALUE '~',
      hsl08(15) TYPE c,
      sp34   TYPE c VALUE '~',
      hsl09(15) TYPE c,
      sp35   TYPE c VALUE '~',
      hsl10(15) TYPE c,
      sp36   TYPE c VALUE '~',
      hsl11(15) TYPE c,
      sp37   TYPE c VALUE '~',
      hsl12(15) TYPE c,
      sp38   TYPE c VALUE '~',
      hsl13(15) TYPE c,
      sp39   TYPE c VALUE '~',
      hsl14(15) TYPE c,
      sp40   TYPE c VALUE '~',
      hsl15(15) TYPE c,
      sp41   TYPE c VALUE '~',
      hsl16(15) TYPE c,
      END OF waglt0.

DATA: BEGIN OF waglt0_header OCCURS 1,
      rldnr(5) VALUE 'RLDNR',
      sp1   TYPE c VALUE '~',
      rrcty(5) VALUE 'RRCTY',
      sp2   TYPE c VALUE '~',
      bukrs(5) VALUE 'BUKRS',
      sp3   TYPE c VALUE '~',
      ryear(5) VALUE 'RYEAR',
      sp4   TYPE c VALUE '~',
      racct(10) VALUE 'RACCT',
      sp5   TYPE c VALUE '~',
      rbusa(5) VALUE 'RBUSA',
      sp6   TYPE c VALUE '~',
      rtcur(5) VALUE 'RTCUR',
      sp7   TYPE c VALUE '~',
      drcrk(5) VALUE 'DRCRK',
      sp8   TYPE c VALUE '~',
      tslvt(15) VALUE 'TSLVT',      "LIKE GLT0-TSLVT,
      sp9   TYPE c VALUE '~',
      hslvt(15) VALUE 'HSLVT',
      sp10   TYPE c VALUE '~',
      tsl01(15) VALUE 'TSL01',
      sp11   TYPE c VALUE '~',
      tsl02(15) VALUE 'TSL02',
      sp12   TYPE c VALUE '~',
      tsl03(15) VALUE 'TSL03',
      sp13   TYPE c VALUE '~',
      tsl04(15) VALUE 'TSL04',
      sp14   TYPE c VALUE '~',
      tsl05(15) VALUE 'TSL05',
      sp15   TYPE c VALUE '~',
      tsl06(15) VALUE 'TSL06',
      sp16   TYPE c VALUE '~',
      tsl07(15) VALUE 'TSL07',
      sp17   TYPE c VALUE '~',
      tsl08(15) VALUE 'TSL08',
      sp18   TYPE c VALUE '~',
      tsl09(15) VALUE 'TSL09',
      sp19   TYPE c VALUE '~',
      tsl10(15) VALUE 'TSL10',
      sp20   TYPE c VALUE '~',
      tsl11(15) VALUE 'TSL11',
      sp21   TYPE c VALUE '~',
      tsl12(15) VALUE 'TSL12',
      sp22   TYPE c VALUE '~',
      tsl13(15) VALUE 'TSL13',
      sp23   TYPE c VALUE '~',
      tsl14(15) VALUE 'TSL14',
      sp24   TYPE c VALUE '~',
      tsl15(15) VALUE 'TSL15',
      sp25   TYPE c VALUE '~',
      tsl16(15) VALUE 'TSL16',
      sp26   TYPE c VALUE '~',
      hsl01(15) VALUE 'HSL01',
      sp27   TYPE c VALUE '~',
      hsl02(15) VALUE 'HSL02',
      sp28   TYPE c VALUE '~',
      hsl03(15) VALUE 'HSL03',
      sp29   TYPE c VALUE '~',
      hsl04(15) VALUE 'HSL04',
      sp30   TYPE c VALUE '~',
      hsl05(15) VALUE 'HSL05',
      sp31   TYPE c VALUE '~',
      hsl06(15) VALUE 'HSL06',
      sp32   TYPE c VALUE '~',
      hsl07(15) VALUE 'HSL07',
      sp33   TYPE c VALUE '~',
      hsl08(15) VALUE 'HSL08',
      sp34   TYPE c VALUE '~',
      hsl09(15) VALUE 'HSL09',
      sp35   TYPE c VALUE '~',
      hsl10(15) VALUE 'HSL10',
      sp36   TYPE c VALUE '~',
      hsl11(15) VALUE 'HSL11',
      sp37   TYPE c VALUE '~',
      hsl12(15) VALUE 'HSL12',
      sp38   TYPE c VALUE '~',
      hsl13(15) VALUE 'HSL13',
      sp39   TYPE c VALUE '~',
      hsl14(15) VALUE 'HSL14',
      sp40   TYPE c VALUE '~',
      hsl15(15) VALUE 'HSL15',
      sp41   TYPE c VALUE '~',
      hsl16(15) VALUE 'HSL16',
      END OF waglt0_header.

DATA: BEGIN OF wafaglflext OCCURS 0,
      rldnr LIKE faglflext-rldnr,
      sp1   TYPE c VALUE '~',
      rrcty LIKE faglflext-rrcty,
      sp2   TYPE c VALUE '~',
      rbukrs LIKE faglflext-rbukrs,
      sp3   TYPE c VALUE '~',
      ryear LIKE faglflext-ryear,
      sp4   TYPE c VALUE '~',
      racct LIKE faglflext-racct,
      sp5   TYPE c VALUE '~',
      rbusa LIKE faglflext-rbusa,
      sp6   TYPE c VALUE '~',
      rtcur LIKE faglflext-rtcur,
      sp7   TYPE c VALUE '~',
      drcrk LIKE faglflext-drcrk,
      sp8   TYPE c VALUE '~',
      tslvt(15) TYPE c,      "LIKE GLT0-TSLVT,
      sp9   TYPE c VALUE '~',
      hslvt(15) TYPE c,      "LIKE GLT0-HSLVT,
      sp10   TYPE c VALUE '~',
      tsl01(15) TYPE c,
      sp11   TYPE c VALUE '~',
      tsl02(15) TYPE c,
      sp12   TYPE c VALUE '~',
      tsl03(15) TYPE c,
      sp13   TYPE c VALUE '~',
      tsl04(15) TYPE c,
      sp14   TYPE c VALUE '~',
      tsl05(15) TYPE c,
      sp15   TYPE c VALUE '~',
      tsl06(15) TYPE c,
      sp16   TYPE c VALUE '~',
      tsl07(15) TYPE c,
      sp17   TYPE c VALUE '~',
      tsl08(15) TYPE c,
      sp18   TYPE c VALUE '~',
      tsl09(15) TYPE c,
      sp19   TYPE c VALUE '~',
      tsl10(15) TYPE c,
      sp20   TYPE c VALUE '~',
      tsl11(15) TYPE c,
      sp21   TYPE c VALUE '~',
      tsl12(15) TYPE c,
      sp22   TYPE c VALUE '~',
      tsl13(15) TYPE c,
      sp23   TYPE c VALUE '~',
      tsl14(15) TYPE c,
      sp24   TYPE c VALUE '~',
      tsl15(15) TYPE c,
      sp25   TYPE c VALUE '~',
      tsl16(15) TYPE c,
      sp26   TYPE c VALUE '~',
      hsl01(15) TYPE c,
      sp27   TYPE c VALUE '~',
      hsl02(15) TYPE c,
      sp28   TYPE c VALUE '~',
      hsl03(15) TYPE c,
      sp29   TYPE c VALUE '~',
      hsl04(15) TYPE c,
      sp30   TYPE c VALUE '~',
      hsl05(15) TYPE c,
      sp31   TYPE c VALUE '~',
      hsl06(15) TYPE c,
      sp32   TYPE c VALUE '~',
      hsl07(15) TYPE c,
      sp33   TYPE c VALUE '~',
      hsl08(15) TYPE c,
      sp34   TYPE c VALUE '~',
      hsl09(15) TYPE c,
      sp35   TYPE c VALUE '~',
      hsl10(15) TYPE c,
      sp36   TYPE c VALUE '~',
      hsl11(15) TYPE c,
      sp37   TYPE c VALUE '~',
      hsl12(15) TYPE c,
      sp38   TYPE c VALUE '~',
      hsl13(15) TYPE c,
      sp39   TYPE c VALUE '~',
      hsl14(15) TYPE c,
      sp40   TYPE c VALUE '~',
      hsl15(15) TYPE c,
      sp41   TYPE c VALUE '~',
      hsl16(15) TYPE c,
      END OF wafaglflext.

DATA: BEGIN OF wafaglflext_header OCCURS 1,
      rldnr(5) VALUE 'RLDNR',
      sp1   TYPE c VALUE '~',
      rrcty(5) VALUE 'RRCTY',
      sp2   TYPE c VALUE '~',
      bukrs(5) VALUE 'BUKRS',
      sp3   TYPE c VALUE '~',
      ryear(5) VALUE 'RYEAR',
      sp4   TYPE c VALUE '~',
      racct(10) VALUE 'RACCT',
      sp5   TYPE c VALUE '~',
      rbusa(5) VALUE 'RBUSA',
      sp6   TYPE c VALUE '~',
      rtcur(5) VALUE 'RTCUR',
      sp7   TYPE c VALUE '~',
      drcrk(5) VALUE 'DRCRK',
      sp8   TYPE c VALUE '~',
      tslvt(15) VALUE 'TSLVT',      "LIKE GLT0-TSLVT,
      sp9   TYPE c VALUE '~',
      hslvt(15) VALUE 'HSLVT',
      sp10   TYPE c VALUE '~',
      tsl01(15) VALUE 'TSL01',
      sp11   TYPE c VALUE '~',
      tsl02(15) VALUE 'TSL02',
      sp12   TYPE c VALUE '~',
      tsl03(15) VALUE 'TSL03',
      sp13   TYPE c VALUE '~',
      tsl04(15) VALUE 'TSL04',
      sp14   TYPE c VALUE '~',
      tsl05(15) VALUE 'TSL05',
      sp15   TYPE c VALUE '~',
      tsl06(15) VALUE 'TSL06',
      sp16   TYPE c VALUE '~',
      tsl07(15) VALUE 'TSL07',
      sp17   TYPE c VALUE '~',
      tsl08(15) VALUE 'TSL08',
      sp18   TYPE c VALUE '~',
      tsl09(15) VALUE 'TSL09',
      sp19   TYPE c VALUE '~',
      tsl10(15) VALUE 'TSL10',
      sp20   TYPE c VALUE '~',
      tsl11(15) VALUE 'TSL11',
      sp21   TYPE c VALUE '~',
      tsl12(15) VALUE 'TSL12',
      sp22   TYPE c VALUE '~',
      tsl13(15) VALUE 'TSL13',
      sp23   TYPE c VALUE '~',
      tsl14(15) VALUE 'TSL14',
      sp24   TYPE c VALUE '~',
      tsl15(15) VALUE 'TSL15',
      sp25   TYPE c VALUE '~',
      tsl16(15) VALUE 'TSL16',
      sp26   TYPE c VALUE '~',
      hsl01(15) VALUE 'HSL01',
      sp27   TYPE c VALUE '~',
      hsl02(15) VALUE 'HSL02',
      sp28   TYPE c VALUE '~',
      hsl03(15) VALUE 'HSL03',
      sp29   TYPE c VALUE '~',
      hsl04(15) VALUE 'HSL04',
      sp30   TYPE c VALUE '~',
      hsl05(15) VALUE 'HSL05',
      sp31   TYPE c VALUE '~',
      hsl06(15) VALUE 'HSL06',
      sp32   TYPE c VALUE '~',
      hsl07(15) VALUE 'HSL07',
      sp33   TYPE c VALUE '~',
      hsl08(15) VALUE 'HSL08',
      sp34   TYPE c VALUE '~',
      hsl09(15) VALUE 'HSL09',
      sp35   TYPE c VALUE '~',
      hsl10(15) VALUE 'HSL10',
      sp36   TYPE c VALUE '~',
      hsl11(15) VALUE 'HSL11',
      sp37   TYPE c VALUE '~',
      hsl12(15) VALUE 'HSL12',
      sp38   TYPE c VALUE '~',
      hsl13(15) VALUE 'HSL13',
      sp39   TYPE c VALUE '~',
      hsl14(15) VALUE 'HSL14',
      sp40   TYPE c VALUE '~',
      hsl15(15) VALUE 'HSL15',
      sp41   TYPE c VALUE '~',
      hsl16(15) VALUE 'HSL16',
      END OF wafaglflext_header.

PERFORM open_output_file.

TRANSFER waskat_header TO phyfile LENGTH recsize.
TRANSFER waglt0_header TO phyfile2 LENGTH recsize2.
TRANSFER wafaglflext_header TO phyfile3 LENGTH recsize2.

PERFORM create_skat_info.
PERFORM create_glt0_info.
PERFORM create_faglflext_info.
WRITE 'Process has been completed'.
CLOSE DATASET: phyfile, phyfile2, phyfile3.

*-------------------------------------------------------------------
FORM create_skat_info.
  SELECT * FROM skat
     WHERE ktopl IN s_ktopl
     AND spras = 'EN'.
    CLEAR waskat.
    MOVE-CORRESPONDING skat TO waskat.
    SELECT SINGLE * FROM ska1
       WHERE ktopl = waskat-ktopl
         AND saknr = waskat-saknr.
    IF sy-subrc = '0'.
      MOVE ska1-ktoks TO waskat-ktoks.
      waskat-xbilk = ska1-xbilk.
      waskat-bilkt = ska1-bilkt.
    ENDIF.

    SELECT * FROM skb1
       WHERE saknr = waskat-saknr
         AND bukrs IN s_bukrs.
      MOVE skb1-bukrs TO waskat-bukrs.
      waskat-sp1 = '~'.
      waskat-sp2 = '~'.
      waskat-sp3 = '~'.
      waskat-sp4 = '~'.
      waskat-sp5 = '~'.
      waskat-sp6 = '~'.
      waskat-sp7 = '~'.
      waskat-sp8 = '~'.
      TRANSFER waskat TO phyfile LENGTH recsize.
    ENDSELECT.                            "end of skb1 selection
  ENDSELECT.                               "end of skat selection
ENDFORM.                    "CREATE_SKAT_INFO

*&---------------------------------------------------------------------*
*&      Form  create_glt0_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_glt0_info.

  SELECT * FROM glt0
    WHERE bukrs IN s_bukrs
      AND ryear IN s_gjahr.

    CLEAR waglt0.
    MOVE-CORRESPONDING glt0 TO waglt0.
    MOVE glt0-tslvt         TO waglt0-tslvt.
    MOVE glt0-hslvt         TO waglt0-hslvt.
    waglt0-tsl01 = glt0-tsl01.
    waglt0-tsl02 = glt0-tsl02.
    waglt0-tsl03 = glt0-tsl03.
    waglt0-tsl04 = glt0-tsl04.
    waglt0-tsl05 = glt0-tsl05.
    waglt0-tsl06 = glt0-tsl06.
    waglt0-tsl07 = glt0-tsl07.
    waglt0-tsl08 = glt0-tsl08.
    waglt0-tsl09 = glt0-tsl09.
    waglt0-tsl10 = glt0-tsl10.
    waglt0-tsl11 = glt0-tsl11.
    waglt0-tsl12 = glt0-tsl12.
    waglt0-tsl13 = glt0-tsl13.
    waglt0-tsl14 = glt0-tsl14.
    waglt0-tsl15 = glt0-tsl15.
    waglt0-tsl16 = glt0-tsl16.
    waglt0-hsl01 = glt0-hsl01.
    waglt0-hsl02 = glt0-hsl02.
    waglt0-hsl03 = glt0-hsl03.
    waglt0-hsl04 = glt0-hsl04.
    waglt0-hsl05 = glt0-hsl05.
    waglt0-hsl06 = glt0-hsl06.
    waglt0-hsl07 = glt0-hsl07.
    waglt0-hsl08 = glt0-hsl08.
    waglt0-hsl09 = glt0-hsl09.
    waglt0-hsl10 = glt0-hsl10.
    waglt0-hsl11 = glt0-hsl11.
    waglt0-hsl12 = glt0-hsl12.
    waglt0-hsl13 = glt0-hsl13.
    waglt0-hsl14 = glt0-hsl14.
    waglt0-hsl15 = glt0-hsl15.
    waglt0-hsl16 = glt0-hsl16.
    waglt0-sp1 = '~'.
    waglt0-sp2 = '~'.
    waglt0-sp3 = '~'.
    waglt0-sp4 = '~'.
    waglt0-sp5 = '~'.
    waglt0-sp6 = '~'.
    waglt0-sp7 = '~'.
    waglt0-sp8 = '~'.
    waglt0-sp9 = '~'.
    waglt0-sp10 = '~'.
    waglt0-sp11 = '~'.
    waglt0-sp12 = '~'.
    waglt0-sp13 = '~'.
    waglt0-sp14 = '~'.
    waglt0-sp15 = '~'.
    waglt0-sp16 = '~'.
    waglt0-sp17 = '~'.
    waglt0-sp18 = '~'.
    waglt0-sp19 = '~'.
    waglt0-sp20 = '~'.
    waglt0-sp21 = '~'.
    waglt0-sp22 = '~'.
    waglt0-sp23 = '~'.
    waglt0-sp24 = '~'.
    waglt0-sp25 = '~'.
    waglt0-sp26 = '~'.
    waglt0-sp27 = '~'.
    waglt0-sp28 = '~'.
    waglt0-sp29 = '~'.
    waglt0-sp30 = '~'.
    waglt0-sp31 = '~'.
    waglt0-sp32 = '~'.
    waglt0-sp33 = '~'.
    waglt0-sp34 = '~'.
    waglt0-sp35 = '~'.
    waglt0-sp36 = '~'.
    waglt0-sp37 = '~'.
    waglt0-sp38 = '~'.
    waglt0-sp39 = '~'.
    waglt0-sp40 = '~'.
    waglt0-sp41 = '~'.
    TRANSFER waglt0 TO phyfile2 LENGTH recsize2.
  ENDSELECT.
ENDFORM.                    "create_glt0_info

*&---------------------------------------------------------------------*
*&      Form  create_faglflext_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_faglflext_info.

  SELECT * FROM faglflext
    WHERE rbukrs IN s_bukrs
      AND ryear IN s_gjahr
      AND rldnr IN s_rldnr.

    CLEAR waglt0.
    MOVE-CORRESPONDING faglflext TO wafaglflext.
    MOVE faglflext-tslvt         TO wafaglflext-tslvt.
    MOVE faglflext-hslvt         TO wafaglflext-hslvt.
    wafaglflext-tsl01 = faglflext-tsl01.
    wafaglflext-tsl02 = faglflext-tsl02.
    wafaglflext-tsl03 = faglflext-tsl03.
    wafaglflext-tsl04 = faglflext-tsl04.
    wafaglflext-tsl05 = faglflext-tsl05.
    wafaglflext-tsl06 = faglflext-tsl06.
    wafaglflext-tsl07 = faglflext-tsl07.
    wafaglflext-tsl08 = faglflext-tsl08.
    wafaglflext-tsl09 = faglflext-tsl09.
    wafaglflext-tsl10 = faglflext-tsl10.
    wafaglflext-tsl11 = faglflext-tsl11.
    wafaglflext-tsl12 = faglflext-tsl12.
    wafaglflext-tsl13 = faglflext-tsl13.
    wafaglflext-tsl14 = faglflext-tsl14.
    wafaglflext-tsl15 = faglflext-tsl15.
    wafaglflext-tsl16 = faglflext-tsl16.
    wafaglflext-hsl01 = faglflext-hsl01.
    wafaglflext-hsl02 = faglflext-hsl02.
    wafaglflext-hsl03 = faglflext-hsl03.
    wafaglflext-hsl04 = faglflext-hsl04.
    wafaglflext-hsl05 = faglflext-hsl05.
    wafaglflext-hsl06 = faglflext-hsl06.
    wafaglflext-hsl07 = faglflext-hsl07.
    wafaglflext-hsl08 = faglflext-hsl08.
    wafaglflext-hsl09 = faglflext-hsl09.
    wafaglflext-hsl10 = faglflext-hsl10.
    wafaglflext-hsl11 = faglflext-hsl11.
    wafaglflext-hsl12 = faglflext-hsl12.
    wafaglflext-hsl13 = faglflext-hsl13.
    wafaglflext-hsl14 = faglflext-hsl14.
    wafaglflext-hsl15 = faglflext-hsl15.
    wafaglflext-hsl16 = faglflext-hsl16.
    wafaglflext-sp1 = '~'.
    wafaglflext-sp2 = '~'.
    wafaglflext-sp3 = '~'.
    wafaglflext-sp4 = '~'.
    wafaglflext-sp5 = '~'.
    wafaglflext-sp6 = '~'.
    wafaglflext-sp7 = '~'.
    wafaglflext-sp8 = '~'.
    wafaglflext-sp9 = '~'.
    wafaglflext-sp10 = '~'.
    wafaglflext-sp11 = '~'.
    wafaglflext-sp12 = '~'.
    wafaglflext-sp13 = '~'.
    wafaglflext-sp14 = '~'.
    wafaglflext-sp15 = '~'.
    wafaglflext-sp16 = '~'.
    wafaglflext-sp17 = '~'.
    wafaglflext-sp18 = '~'.
    wafaglflext-sp19 = '~'.
    wafaglflext-sp20 = '~'.
    wafaglflext-sp21 = '~'.
    wafaglflext-sp22 = '~'.
    wafaglflext-sp23 = '~'.
    wafaglflext-sp24 = '~'.
    wafaglflext-sp25 = '~'.
    wafaglflext-sp26 = '~'.
    wafaglflext-sp27 = '~'.
    wafaglflext-sp28 = '~'.
    wafaglflext-sp29 = '~'.
    wafaglflext-sp30 = '~'.
    wafaglflext-sp31 = '~'.
    wafaglflext-sp32 = '~'.
    wafaglflext-sp33 = '~'.
    wafaglflext-sp34 = '~'.
    wafaglflext-sp35 = '~'.
    wafaglflext-sp36 = '~'.
    wafaglflext-sp37 = '~'.
    wafaglflext-sp38 = '~'.
    wafaglflext-sp39 = '~'.
    wafaglflext-sp40 = '~'.
    wafaglflext-sp41 = '~'.
    TRANSFER wafaglflext TO phyfile3 LENGTH recsize3.
  ENDSELECT.
ENDFORM.                    "create_faglflext_info


************************************************************************
* Routine to convert the logical file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
************************************************************************
FORM open_output_file.
  OPEN DATASET phyfile FOR OUTPUT IN TEXT MODE.
  OPEN DATASET phyfile2 FOR OUTPUT IN TEXT MODE.
  OPEN DATASET phyfile3 FOR OUTPUT IN TEXT MODE.
ENDFORM.                    "OPEN_OUTPUT_FILE
