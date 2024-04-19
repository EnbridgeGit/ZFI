REPORT ZFFII030 message-id ZS NO STANDARD PAGE HEADING
                              LINE-SIZE 130
                              LINE-COUNT 65.

*----------------------------------------------------------------------*
*  Author    : Adel D. Mendoza                    SAP : East
*  Date      : June, 2006                Program Type : Interface
*----------------------------------------------------------------------*
*  Issue Log : TR151
*----------------------------------------------------------------------*
*  Title : Mercator Replacement - IFFI058 Contrax Estimates Interface
*        :                      - IFFI058E Liberty Estimates Interface
*----------------------------------------------------------------------*
*  Issue Log : TR149
*----------------------------------------------------------------------*
*  Title : Mercator Replacement - IFFI053 Contrax Actuals Interface
*        :                      - IFFI053D Liberty DP Billing Interface
*----------------------------------------------------------------------*
*  Description:
*     This ABAP program will replace 4 Mercator mapping
*     programs that take an input file and creates a ZBSEG format
*     BDC session.
*     The new names for the interfaces and BDC sessions are as follows:
*
*     Old Name   Desc                New Name    New BDC name
*
*     IFFI053    Contrax Actuals     IFFI061A    ZFI-CTRX-REV
*     IFFI058    Contrax Estimates   IFFI061E    ZFI-CTRX-EST
*     IFFI053D   Liberty Actuals     IFFI062A    ZFI-DP-REV
*     IFFI058E   Liberty Estimates   IFFI062E    ZFI-DP-EST
*----------------------------------------------------------------------*
*  Changes:
* 2006/11/01 - gymana - Made coding corrections as a result of unit
*                       testing
* 2009/01/16 - gymana - TR580 - 6.0 SAP Upgrade
*                       Modified code to use ZKPAA005 BDC create ABAP.
*                       ZBSEG1-TBNAM now defined using table ZBSEG.
*                       20 byte offset removed from outrec when
*                       creating BBKPF record.
*----------------------------------------------------------------------*

tables:  DD03L,                      "Table fields
         ZFC01,                      "Contrax G/L Account lookup
         ZFC02,                      "Contrax Rate Class lookup
         ZFC03,                      "Contrax Service Class lookup
         ZFC04,                      "Contrax Sales Org derivation
         ZFC06,                      "Contrax Sector lookup
         ZFB03,                      "Banner Town Code lookup
         ZWACOG,                     "WACOG Table
         ZSTCONV.                    "Volume Conversion Table

data: begin of zbseg1,
        stype     like zbseg-stype,    tbnam     like zbseg-tbnam,
        newbs     like zbseg-newbs,    newum     like zbseg-newum,
        newbk     like zbseg-newbk,    wrbtr     like zbseg-wrbtr,
        wmwst     like zbseg-wmwst,    mwskz     like zbseg-mwskz,
        xskrl     like zbseg-xskrl,    kostl     like zbseg-kostl,
        aufnr     like zbseg-aufnr,    matnr     like zbseg-matnr,
        werks(04) type c          ,    menge     like zbseg-menge,
        meins     like zbseg-meins,    valut     like zbseg-valut,
        zfbdt     like zbseg-zfbdt,    zuonr     like zbseg-zuonr,
        sgtxt     like zbseg-sgtxt,    skfbt     like zbseg-skfbt,
        wskto     like zbseg-wskto,    zterm     like zbseg-zterm,
        zbd1t     like zbseg-zbd1t,    zbd1p     like zbseg-zbd1p,
        zbd2t     like zbseg-zbd2t,    zbd2p     like zbseg-zbd2p,
        zbd3t     like zbseg-zbd3t,    zlspr     like zbseg-zlspr,
        zlsch     like zbseg-zlsch,    zbfix     like zbseg-zbfix,
        qsskz     like zbseg-qsskz,    qsshb     like zbseg-qsshb,
        qsfbt     like zbseg-qsfbt,    regul     like zbseg-regul,
        name1     like zbseg-name1,    name2     like zbseg-name2,
        name3     like zbseg-name3,    name4     like zbseg-name4,
        stras     like zbseg-stras,    ort01     like zbseg-ort01,
        pstlz     like zbseg-pstlz,    land1     like zbseg-land1,
        regio     like zbseg-regio,    stcd1     like zbseg-stcd1,
        stcd2     like zbseg-stcd2,    pfach     like zbseg-pfach,
        pstl2     like zbseg-pstl2,    spras     like zbseg-spras,
        hkont     like zbseg-hkont,    fwbas     like zbseg-fwbas,
        projk     like zbseg-projk,    uzawe     like zbseg-uzawe,
        sende     like zbseg-sende,    kndnr(10) type c          ,
        vkorg(04) type c          ,    wwbrn(04) type c          ,
        wwsct(01) type c          ,    wwseg(02) type c          ,
        wwrat(03) type c          ,    wwser(02) type c          ,
        artnr(18) type c          ,    prdha(18) type c          ,
        wwsub(02) type c          ,    wwprg(02) type c          ,
        wwlob(04) type c          ,    wwreg(02) type c          ,
        wwdvs(02) type c          ,    wwrgn(04) type c.
data: end of zbseg1.

data: begin of wa occurs 0,
        APP_YR(04)             type c,  "year transaction applied
        APP_MTH(02)            type c,  "month transaction applied
        CUST_ID(08)            type c,  "customer number
        RATE_CL(12)            type c,  "rate class
        SERV_TYPE(06)          type c,  "service type
        SERV_CL(02)            type c,  "service class
        SEAS_CL(04)            type c,  "seasonal class
        RATE_TYPE(04)          type c,  "rate type
        CHARGE_TYPE(04)        type c,  "charge type
        SR_USAGE(04)           type c,  "sr usage
        ST_SUBTYPE(06)         type c,  "st subtype
        NON_RATE_ITEM_TYP(08)  type c,  "non-rate item type
        TIER_STEP_LVL(02)      type c,  "tier step level
        SECTOR_SIZE(01)        type c,  "sector size
        SECTOR(06)             type c,  "sector
        AMOUNT(16)             type c,  "transaction amount
        VOLUME(18)             type c,  "transaction volume
        CUST_TYPE(01)          type c,  "customer type
        GECA_CODE(06)          type c,  "geographic area
        VOL_UOM(08)            type c,  "volume unit of measure
        SA_NUM(08)             type c.  "contract number
data: end of wa.
data: wa_err  like wa occurs 0 with header line.
data: contrax like wa occurs 0 with header line.

data: begin of isort occurs 0,
        izbd1p(02) type n,                        "seqnum/docsize
        ikostl     like zbseg-kostl,              "/
        ihkont     like zbseg-hkont,              "G/L account
        inewbs     like zbseg-newbs,              "40 or 50
        imatnr     like zbseg-matnr,              "product
        iaufnr     like zbseg-aufnr,              "/
        iwerks(04) type c,                        "plant
        izuonr     like zbseg-zuonr,              "assignment number
        ikndnr(10) type c,                        "1
        ivkorg(04) type c,                        "sales org
        iwwbrn(04) type c,                        "branch
        iwwrat(03) type c,                        "rate class
        iwwser(02) type c,                        "service class
        iwwsct(01) type c,                        "Z
        iwwseg(02) type c,                        "/
        ibudat(08) type c,
        ivolum(15) type c.
        include structure zbseg1.
data: end of isort.
data: isum like isort occurs 0 with header line.

*data: begin of ierr occurs 0.
*        include structure zbseg1.
*data: end of ierr.

data: begin of Z_BGR00.
        include structure bgr00.
data: end of Z_BGR00.

data: begin of Z_BBKPF.
        include structure bbkpf.
data: end of Z_BBKPF.

data: va_AMOUNT  type p decimals 2 value 0,
      va_AMOUNT2 type p decimals 2 value 0,
      va_VOLUME  type p decimals 3 value 0,
      va_MENGE   type p decimals 3 value 0.

data: wa_zbd1p     type i,
      wa_zbd1p_num type p decimals 5 value 0,
      wa_docsize   type i,
      char(21)     type c,
      nodata(1)    value '/',
      wa_wacog     like zwacog-wacog,
      wa_heatv     like zstconv-factor.


data: delimeter value '09' type X.
data: inrec(400), outrec(2000), infile(70), outfile(70).
data: WRK_SYMBOLIC(4) TYPE C VALUE '$sys'.

constants: co_hkont(10) value '0000302469',
           co_kostl(10) value '0000020310'.

ranges: ra_nonrat for contrax-NON_RATE_ITEM_TYP.

field-symbols: <F1>.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
selection-screen begin of block box0 with frame.
selection-screen begin of block box1 with frame title text-100.

PARAMETERS: p_filein like filename-fileextern obligatory default
                     '/usr/sap/interfaces/$sys/IFFI061/ctrx_est.chk',

            p_fileot like filename-fileextern obligatory default
                     '/usr/sap/interfaces/$sys/IFFI061/ctrx_est.sap'.

selection-screen skip.
PARAMETERS: p_group  like bgr00-group obligatory default
                     'ZFI-CTRX-REV'.

selection-screen skip.
PARAMETERS: p_tcode  like tstc-tcode  obligatory default 'FB01',
            p_blart  like bbkpf-blart obligatory default 'S6',
            p_bukrs  like bbkpf-bukrs obligatory default 'UGL',
            p_waers  like bbkpf-waers obligatory default 'CAD',
            p_xblnr  like bbkpf-xblnr obligatory default
                     'CONTRAX',
            p_bktxt  like bbkpf-bktxt obligatory default
                     'Contrax Revenue: Actual'.

selection-screen skip.
PARAMETERS: p_dsize(4) type c obligatory default '499',
            p_aeidx(8) type c obligatory default 'ACTUAL'.

selection-screen end of block box1.
selection-screen end of block box0.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  REPLACE WRK_SYMBOLIC WITH SY-SYSID INTO: P_FILEIN, P_FILEOT.
  CONDENSE: P_FILEIN NO-GAPS, P_FILEOT NO-GAPS.

*---------------------------------------------------------------------*
* start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
  IF P_AEIDX = 'ACTUAL'.
    PERFORM INITIALIZE_DATA.
  ENDIF.

  PERFORM GET_WACOG_HV_VALUES.
  PERFORM OPEN_FILES.
  perform INPUT_FILE_TO_WA.
  perform COLLECT_DATA.

*  compare to zbis254e.vol file
*  loop at contrax.
*     TRANSFER contrax to P_FILEOT.
*  endloop.

  perform XLATCTRXEST.
* compare to zbis254e1.tmp
*  loop at isort.
*     TRANSFER isort to P_FILEOT.
*  endloop.

  perform SUMMARIZE_DATA.

* compare to zbis254e2.tmp
*  loop at isum.
*     TRANSFER isum to P_FILEOT.
*  endloop.

  perform CREATE_OUTPUT_FILE.

  close dataset: P_FILEIN, P_FILEOT.

  IF NOT WA_ERR[] IS INITIAL.
    PERFORM DISPLAY_ERROR_LST.
  ENDIF.


*---------------------------------------------------------------------*
* top of page
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  FORMAT INTENSIFIED OFF.
  WRITE: /001 SY-DATUM,
          050 P_BKTXT,
          117 'Page:', SY-PAGNO.

  WRITE: /    SY-UZEIT UNDER SY-DATUM,
              '  Input File Errors' UNDER P_BKTXT,
              SY-REPID UNDER 'Page:'.

  WRITE: / SY-SYSID under SY-REPID.

  FORMAT INTENSIFIED ON.

  write: / sy-uline.
  write: /002 'Date',
          011 'Cust',
          018 'Rate',
          031 'Serv',
          038 'Serv',
          051 'Seasonal',
          060 'Rate',
          065 'Charge',
          072 'Non Rate',
          085 'Transaction',
          103 'Transaction',
          123 'Contract'.

  write: /001 'Applied',
          011 'No.',
          018 'Class',
          031 'Type',
          038 'Class',
          051 'Class',
          060 'Type',
          065 'Type',
          072 'Item Type',
          085 'Amount',
          103 'Volume',
          123 'Number'.
  write: / sy-uline.
  FORMAT INTENSIFIED OFF.

*---------------------------------------------------------------------*
*  translate records
*---------------------------------------------------------------------*
form XLATCTRXEST.
  refresh: isort, wa_err.   "ierr
  loop at contrax.
    clear ZFC01.
    select single * from ZFC01
       where C_NRTTYP  = contrax-NON_RATE_ITEM_TYP
       and ( C_SVCTYP  = contrax-SERV_TYPE   or C_SVCTYP  = '******' )
       and ( C_CUSTTYP = contrax-CUST_TYPE   or C_CUSTTYP = '*' )
       and ( C_RTETYP  = contrax-RATE_TYPE   or C_RTETYP  = '****' )
       and ( C_CHGTYP  = contrax-CHARGE_TYPE or C_CHGTYP  = '****' )
       and ( C_SRUSE   = contrax-SR_USAGE    or C_SRUSE   = '****' )
       and ( C_STSUB   = contrax-ST_SUBTYPE  or C_STSUB   = '******' )
       and ( C_SCCODE  = contrax-SERV_CL     or C_SCCODE  = '**' ).
    if sy-subrc = 0.
      if ZFC01-c_stsub = 'SPOT'.
         move '0000179016' to ZFC01-saknr.
      endif.
      perform f_CONTRAX_TO_BSEG.
    else.
      perform f_CONTRAX_ERROR.
      move-corresponding contrax to wa_err.
      append wa_err. clear wa_err.
    endif.
  endloop.
endform.

*---------------------------------------------------------------------
* gather all records
*---------------------------------------------------------------------
form f_CONTRAX_TO_BSEG.
  clear: wa_zbd1p, wa_docsize.
  perform INIT_TABLE_RECORD.

  shift contrax-SR_USAGE          left deleting leading space.
  shift contrax-RATE_CL           left deleting leading space.
  shift contrax-NON_RATE_ITEM_TYP left deleting leading space.
  shift contrax-SERV_TYPE         left deleting leading space.
  shift contrax-VOL_UOM           left deleting leading space.

  move p_dsize to wa_docsize.
  wa_zbd1p_num = ABS( sy-tabix / wa_docsize ).
  wa_zbd1p = TRUNC( wa_zbd1p_num ).

  move wa_zbd1p    to isort-IZBD1P.

  perform F_LOOKUP_KOSTL using contrax-RATE_CL
                               contrax-NON_RATE_ITEM_TYP
                               contrax-GECA_CODE
                      changing isort-IKOSTL.

  if isort-IKOSTL is initial.
    move '/' to isort-IKOSTL.
  endif.

  if not zfc01-SAKNR is initial.
    move zfc01-SAKNR to isort-IHKONT.
  endif.

  clear: va_AMOUNT, va_VOLUME.
  move: contrax-AMOUNT to va_AMOUNT, contrax-VOLUME to va_VOLUME.
  if va_AMOUNT < 0 or ( contrax-SR_USAGE = 'FUEL' and va_VOLUME < 0 ).
    isort-INEWBS = '40'.
  else.
    isort-INEWBS = '50'.
  endif.

  if not zfc01-MATNR is initial.
    move zfc01-MATNR to isort-IMATNR.
  endif.

  if not zfc01-BD_G_IND is initial.
    perform F_LOOKUP_IO using contrax-GECA_CODE
                     changing isort-IAUFNR.
  else.
    move '/' to isort-IAUFNR.
  endif.

  perform F_LOOKUP_WERKS using isort-IMATNR
                               contrax-RATE_CL
                               contrax-NON_RATE_ITEM_TYP
                               contrax-GECA_CODE
                      changing isort-IWERKS.

  move contrax-SA_NUM to isort-IZUONR.
  shift isort-IZUONR left deleting leading space.

  perform F_GET_KNDNR using isort-IMATNR
                            contrax-CUST_ID
                   changing isort-IKNDNR.

  perform F_LOOKUP_VKORG using isort-IMATNR
                               contrax-SECTOR_SIZE
                               contrax-RATE_CL
                      changing isort-IVKORG.

  perform F_LOOKUP_WWBRN using isort-IMATNR
                               contrax-GECA_CODE
                      changing isort-IWWBRN.

  perform F_LOOKUP_WWRAT using isort-IMATNR
                               contrax-RATE_CL
                               contrax-NON_RATE_ITEM_TYP
                               contrax-SERV_TYPE
                               contrax-SEAS_CL
                      changing isort-IWWRAT.

  perform F_LOOKUP_WWSER using isort-IMATNR
                               contrax-RATE_CL
                               contrax-NON_RATE_ITEM_TYP
                               contrax-CUST_TYPE
                      changing isort-IWWSER.

  perform F_LOOKUP_WWSCT using isort-IMATNR
                               contrax-SECTOR
                      changing isort-IWWSCT.

  perform F_LOOKUP_WWSEG using isort-IMATNR
                      changing isort-IWWSEG.

  IF P_AEIDX = 'ESTIMATE'.
    concatenate contrax-APP_YR contrax-APP_MTH '01'
       into isort-ibudat.
  ELSE.
    move sy-datum to isort-ibudat.
  ENDIF.

  PERFORM COMMON_MOVES_TO_ISORT.
  APPEND ISORT.

* -----------------------------------------
* offset line items
* -----------------------------------------
  perform F_LOOKUP_KOSTL2 using contrax-NON_RATE_ITEM_TYP
                                isort-IKOSTL
                       changing isort-IKOSTL.

  if isort-IKOSTL is initial.
    move '/' to isort-IKOSTL.
  endif.

  if not zfc01-OACCT is initial.
    move zfc01-OACCT to isort-IHKONT.
  endif.

  clear: va_AMOUNT, va_VOLUME.
  move: contrax-AMOUNT to va_AMOUNT, contrax-VOLUME to va_VOLUME.
  if va_AMOUNT < 0 or ( contrax-SR_USAGE = 'FUEL' and va_VOLUME < 0 ).
    isort-INEWBS = '50'.
  else.
    isort-INEWBS = '40'.
  endif.

  move: '/' to isort-IMATNR, '/' to isort-IAUFNR,
        '/' to isort-IWERKS, '/' to isort-IKNDNR,
        '/' to isort-IVKORG, '/' to isort-IWWBRN,
        '/' to isort-IWWRAT, '/' to isort-IWWSER,
        '/' to isort-IWWSCT, '/' to isort-IWWSEG.

  PERFORM COMMON_MOVES_TO_ISORT.
  if not zfc01-OMWSKZ is initial.
    move zfc01-OMWSKZ to isort-MWSKZ.
  else.
    move '/' to isort-MWSKZ.
  endif.
  move '/' to isort-menge.
  move '/' to isort-meins.
  APPEND ISORT. CLEAR ISORT.

endform.

*---------------------------------------------------------------------
* get KOSTL - Offset account
*---------------------------------------------------------------------
form F_LOOKUP_KOSTL2 using Inonrat Ikostl changing Ekostl.
  clear Ekostl.
  if ( iNONRAT = 'SSOCF' or iNONRAT = 'SPODF' or iNONRAT = 'SDINT' ).
    move '/' to Ekostl.
  else.
    move Ikostl to Ekostl.
  endif.
endform.

*---------------------------------------------------------------------
* COMMON MOVES TO ISORT
*---------------------------------------------------------------------
form COMMON_MOVES_TO_ISORT.
  move '2'           to isort-stype.
  move 'ZBSEG'       to isort-tbnam.

  move  isort-INEWBS  to  isort-NEWBS.

  perform F_COMPUTE_WRBTR using contrax-AMOUNT
                                contrax-VOLUME
                                contrax-SR_USAGE
                       changing isort-WRBTR.

  if not zfc01-MWSKZ is initial.
    move  zfc01-MWSKZ   to  isort-MWSKZ.
  else.
    move '/' to isort-MWSKZ.
  endif.

  move  isort-IKOSTL  to  isort-KOSTL.
  move  isort-IAUFNR  to  isort-AUFNR.
  move  isort-IMATNR  to  isort-MATNR.
  move  isort-IWERKS  to  isort-WERKS.

  perform F_COMPUTE_MENGE using isort-IMATNR
                                contrax-SERV_TYPE
                                contrax-SR_USAGE
                                contrax-VOL_UOM
                                contrax-VOLUME
                                contrax-NON_RATE_ITEM_TYP
                       changing isort-MENGE
                                isort-MEINS.

  clear va_menge.
  move isort-MENGE to va_menge.
  if isort-MENGE is initial or va_menge = 0.
    move '/' to isort-MENGE.
  endif.
  if isort-MEINS is initial.
    move '/' to isort-MEINS.
  endif.

*  perform F_GET_MEINS using isort-IMATNR
*                            contrax-SERV_TYPE
*                            contrax-SR_USAGE
*                            contrax-VOLUME
*                            contrax-RATE_CL
*                            contrax-NON_RATE_ITEM_TYP
*                   changing isort-MEINS.

  move  isort-IZUONR  to  isort-ZUONR.
  move  p_bktxt       to  isort-SGTXT.
*  move  isort-IZBD1P  to  isort-ZBD1P.
  move  isort-IHKONT  to  isort-HKONT.

  move  isort-IKNDNR  to  isort-KNDNR.
  move  isort-IVKORG  to  isort-VKORG.
  move  isort-IWWBRN  to  isort-WWBRN.
  move  isort-IWWSCT  to  isort-WWSCT.
  move  isort-IWWSEG  to  isort-WWSEG.
  move  isort-IWWRAT  to  isort-WWRAT.
  move  isort-IWWSER  to  isort-WWSER.
  move  isort-IMATNR  to  isort-ARTNR.

  clear: isort-wwrgn.

endform.

*---------------------------------------------------------------------
* get MEINS - Not used but saved as reference to mercator logic
*---------------------------------------------------------------------
*form F_GET_MEINS using Imatnr  Isrvtyp Isrusage
*                       Ivolume Iratecl Inonrat  changing Emeins.
*  clear Emeins.
*  if iMATNR <> '/' and ( iSRVTYP <> 'STORE' or iSRUSAGE = 'FUEL' ).
*    if iVOLUME <> 0.
*      if iRATECL is initial.
*        if iNONRAT = 'M7TOTINV'.
*          move 'M3' to Emeins.
*        endif.
*      else.
*        move 'M3' to Emeins.
*      endif.
*    endif.
*  endif.
*  if Emeins is initial.
*    move '/' to Emeins.
*  endif.
*endform.

*---------------------------------------------------------------------
* get MENGE
*---------------------------------------------------------------------
form F_COMPUTE_MENGE using Imatnr  Isrvtyp Isrusage
                           Ivoluom Ivolume Inonrat
                  changing Emenge Emeins.
  clear: Emenge, Emeins.
  if iMATNR <> '/' and ( iSRVTYP <> 'STORE' or iSRUSAGE = 'FUEL' ) or
    ( iNONRAT in RA_NONRAT ).
     if iVOLUOM = 'CUST'.
     else.
       perform F_CONVERT_UOM using Ivoluom Ivolume Inonrat
                             changing Emenge Emeins.
     endif.
  endif.
endform.

*---------------------------------------------------------------------
* convert UOM - convert GJ / MJ to M3
*---------------------------------------------------------------------
form F_CONVERT_UOM using Ivoluom Ivolume Inonrat
                changing Emenge Emeins.
  data: wa_menge      type p decimals 3.
  data  wa_heatv_MJ   like zstconv-factor.

  clear: Emenge, Emeins.

  if ( isort-IHKONT+4(6) = '152005' or
       isort-IHKONT+4(6) = '152100' or
       isort-IHKONT+4(6) = '152500' or
       isort-IHKONT+4(6) = '152600' ).
    move Ivolume to wa_menge.
    move Ivoluom to Emeins.
  else.
    move 'M3' to Emeins.
    if iVOLUOM = 'GJ'.
      wa_heatv_MJ = ( wa_heatv / 1000 ).
      wa_menge = ( Ivolume / wa_heatv_MJ ).
    elseif iVOLUOM = 'MJ'.
      wa_menge = ( Ivolume / wa_heatv ).
    else.
      move Ivolume to wa_menge.
    endif.
  endif.

  if wa_menge = 0.
    move '/' to Emeins.
  endif.

  move wa_menge to Emenge.
* remove the neg. sign from the volume before passing the value back.

  translate Emenge using '- '.
  shift Emenge right deleting trailing space.
endform.

*---------------------------------------------------------------------
* compute WRBTR
*---------------------------------------------------------------------
form F_COMPUTE_WRBTR using Iamount Ivolume Isrusage changing Ewrbtr.
  data: wa_wrbtr    type p decimals 2.
  data: wa_wacog_MJ like zwacog-wacog.
  data: wa_wrbtr2(16) type c.

  clear Ewrbtr.
  if iSRUSAGE = 'FUEL'.
    wa_wacog_MJ = ( wa_wacog / 1000 ).
    wa_wrbtr = ( Ivolume * wa_wacog_MJ ).
  else.
    move Iamount to wa_wrbtr.
  endif.

* remove the negative sign from the amount before passing the value
* back. +ve / -ve is identified by the 40/50 newbs posting key.

  move wa_wrbtr to Ewrbtr.
  translate Ewrbtr using '- '.
  shift Ewrbtr right deleting trailing space.


endform.

*---------------------------------------------------------------------
* get WWSEG
*---------------------------------------------------------------------
form F_LOOKUP_WWSEG using Imatnr changing Ewwseg.
  clear Ewwseg.
  if iMATNR = '/'.
    move '/' to Ewwseg.
  else.
    move 'ZZ' to Ewwseg.
  endif.
endform.

*---------------------------------------------------------------------
* get WWSCT
*---------------------------------------------------------------------
form F_LOOKUP_WWSCT using Imatnr Isector changing Ewwsct.
  clear Ewwsct.
  if iMATNR = '/'.
    move '/' to Ewwsct.
  else.
    select single * from ZFC06 where C_SECTOR = Isector.
    if sy-subrc = 0.
      move zfc06-WWSCT to Ewwsct.
    else.
      move 'Z' to Ewwsct.
    endif.
  endif.
endform.

*---------------------------------------------------------------------
* get WWSER
*---------------------------------------------------------------------
form F_LOOKUP_WWSER using Imatnr Iratecl Inonrat Icustyp
                 changing Ewwser.
  clear Ewwser.
  if iMATNR = '/'.
    move '/' to Ewwser.
  else.
    if iRATECL is initial.
      if iNONRAT = 'M7TOTINV'.
        move '30' to Ewwser.
      else.
        move '/' to Ewwser.
      endif.
    else.
      if ( iRATECL = 'N/A' or iRATECL = 'R1' or iRATECL = 'U2' or
         iRATECL = 'S1' ).
        move '/' to Ewwser.
      else.
        select single * from ZFC02 where C_RATECL = Iratecl.
        if sy-subrc = 0.
          select single * from ZFC03 where C_CUSTYP = Icustyp
                                       and BUKRS    = zfc02-BUKRS.
          if sy-subrc = 0.
            move zfc03-WWSER to Ewwser.
          endif.
        endif.
        if eWWSER is initial.
          concatenate '*' iCUSTYP into Ewwser.
        endif.
      endif.
    endif.
  endif.
endform.

*---------------------------------------------------------------------
* get WWRAT
*---------------------------------------------------------------------
form F_LOOKUP_WWRAT using Imatnr Iratecl Inonrat Isrvtyp Iseacla
                 changing Ewwrat.
  clear Ewwrat.
  if iMATNR = '/' or iRATECL = 'N/A'.
    move '/' to Ewwrat.
  else.
    if iRATECL is initial.
      if iNONRAT = 'M7TOTINV'.
        move 'M7' to Ewwrat.
      elseif iNONRAT = 'GS-CT'.
        move 'T-4' to Ewwrat.
      else.
        move '/' to Ewwrat.
      endif.
    else.
      select single * from ZFC02
        where C_RATECL = Iratecl
          and ( C_SVCTYP = Isrvtyp or C_SVCTYP = '******' )
          and ( C_SEASCL = Iseacla or C_SEASCL = '****' ).
      if sy-subrc = 0.
        move ZFC02-wwrat to Ewwrat.
      else.
        move Iratecl(3) to Ewwrat.
      endif.
    endif.
  endif.
endform.

*---------------------------------------------------------------------
* get WWBRN
*---------------------------------------------------------------------
form F_LOOKUP_WWBRN using Imatnr Igeca changing Ewwbrn.
  clear Ewwbrn.
  if iMATNR = '/'.
    move '/' to Ewwbrn.
  else.
    if not iGECA is initial.
      select single * from ZFB03 where TOWNCODE = Igeca+0(2)
                                   and MUNICODE = Igeca+2(4).
      if sy-subrc = 0.
        move zfb03-WWBRN to Ewwbrn.
      else.
        move iGECA to Ewwbrn.
      endif.
    else.
      move 'UNKN' to Ewwbrn.
    endif.
  endif.
endform.

*---------------------------------------------------------------------
* get VKORG
*---------------------------------------------------------------------
form F_LOOKUP_VKORG using Imatnr Isecsiz Iratecl changing Evkorg.
  clear Evkorg.
  if iMATNR = '/'.
    move '/' to Evkorg.
  else.
    select single * from ZFC02 where C_RATECL = Iratecl.
    if sy-subrc = 0.
      select single * from ZFC04 where C_SECSIZ = Isecsiz
                                   and BUKRS    = zfc02-bukrs.
      if sy-subrc = 0.
        move ZFC04-VKORG to Evkorg.
      endif.
    endif.
    if eVKORG is initial.
      move 'U020' to eVKORG.
    endif.
  endif.
endform.

*---------------------------------------------------------------------
* get KNDNR
*---------------------------------------------------------------------
form F_GET_KNDNR using Imatnr Icustid changing Ekndnr.
  data: wa_custid like contrax-CUST_ID.
  clear eKndnr.
  if iMATNR = '/'.
    move '/' to Ekndnr.
  else.
    move Icustid to wa_custid.
    shift wa_custid left deleting leading space.
    concatenate 'C' wa_custid into Ekndnr.
  endif.
endform.

*---------------------------------------------------------------------
* get plant
*---------------------------------------------------------------------
form F_LOOKUP_WERKS using Imatnr Iratecl Inonrat Igeca
                 changing Ewerks.
  clear Ewerks.
  if iMATNR = '/' or ( iRATECL is initial and iNONRAT <> 'GS-CT' ).
    move '/' to Ewerks.
  else.
    if not iGECA is initial.
      select single * from ZFB03 where TOWNCODE = Igeca+0(2)
                                   and MUNICODE = Igeca+2(4).
      if sy-subrc = 0.
        move zfb03-WERKS to Ewerks.
      endif.
    else.
      move 'S299' to Ewerks.
    endif.
  endif.
  if eWERKS is initial.
    move '/' to eWERKS.
  endif.
endform.

*---------------------------------------------------------------------
* get internal order
*---------------------------------------------------------------------
form F_LOOKUP_IO using Igeca changing Eaufnr.
  clear Eaufnr.
  select single * from ZFB03 where TOWNCODE = Igeca+0(2)
                               and MUNICODE = Igeca+2(4).
  if sy-subrc = 0.
    move zfb03-BD_IO_GAS to Eaufnr.
  else.
    concatenate '*' iGECA '*' into Eaufnr.
  endif.
  If Eaufnr = '**'.
    move '/' to Eaufnr.
  endif.
endform.

*---------------------------------------------------------------------
* get cost center
*---------------------------------------------------------------------
form F_LOOKUP_KOSTL using Iratecl Inonrat Igeca changing Ekostl.
  clear Ekostl.
  if ( iRATECL = '' and
     ( iNONRAT = 'AOC' or iNONRAT = 'TTF' or iNONRAT = 'HSTDATA' ) ).

    perform F_LOOKUP_CC using IGECA changing Ekostl.

  elseif ( iNONRAT = 'SSOCF' or iNONRAT = 'SPODF' or
           iNONRAT = 'SDINT' ).
    move '0000020310' to Ekostl.
  else.
    move '/' to Ekostl.
  endif.
endform.

*---------------------------------------------------------------------
* lookup cost center
*---------------------------------------------------------------------
form F_LOOKUP_CC using Igeca changing Ekostl.
  clear Ekostl.
  select single * from ZFB03 where TOWNCODE = Igeca+0(2)
                               and MUNICODE = Igeca+2(4).
  if sy-subrc = 0.
    move zfb03-CSKOSTL to Ekostl.
  else.
    concatenate '*' iGECA '*' into Ekostl.
  endif.
endform.

*---------------------------------------------------------------------
* gather all records with error
*---------------------------------------------------------------------
form f_CONTRAX_ERROR.
  clear: wa_zbd1p, wa_docsize.
  perform INIT_TABLE_RECORD.

  shift contrax-SR_USAGE left deleting leading space.

  move p_dsize to wa_docsize.
  wa_zbd1p = ABS( sy-tabix / wa_docsize ).
  move wa_zbd1p    to isort-IZBD1P.
  move '2'           to isort-stype.
  move 'ZBSEG'       to isort-tbnam.

  IF P_AEIDX = 'ESTIMATE'.
    concatenate contrax-APP_YR contrax-APP_MTH '01'
       into isort-ibudat.
  ELSE.
    move sy-datum to isort-ibudat.
  ENDIF.

  clear: va_AMOUNT, va_VOLUME.
  move: contrax-AMOUNT to va_AMOUNT, contrax-VOLUME to va_VOLUME.
  if va_AMOUNT < 0 or ( contrax-SR_USAGE = 'FUEL' and va_VOLUME < 0 ).
    isort-NEWBS = '40'.
  else.
    isort-NEWBS = '50'.
  endif.
  move isort-NEWBS to isort-INEWBS.

  isort-WRBTR = contrax-AMOUNT.
  translate isort-WRBTR using '- '.
  shift isort-WRBTR right deleting trailing space.

  isort-ZUONR = contrax-SA_NUM.
  shift isort-ZUONR left deleting leading space.
  move isort-ZUONR to isort-IZUONR.

  clear INREC.
  concatenate contrax-RATE_CL '/' contrax-SERV_TYPE '/'
    contrax-NON_RATE_ITEM_TYP into INREC.
  concatenate 'Contrax Rev Est Error:' INREC into INREC
    separated by space.
  isort-SGTXT = INREC.

  select single * from ZFC01 where C_SVCTYP = 'ERROR'.
  isort-HKONT = zfc01-SAKNR.
  move isort-HKONT to isort-IHKONT.
  APPEND ISORT.

  if va_AMOUNT < 0 or ( contrax-SR_USAGE = 'FUEL' and va_VOLUME < 0 ).
    isort-NEWBS = '50'.
  else.
    isort-NEWBS = '40'.
  endif.
  move isort-NEWBS to isort-INEWBS.
  isort-HKONT = zfc01-OACCT.
  move isort-HKONT to isort-IHKONT.
  clear: isort-wwrgn.
  APPEND ISORT. CLEAR ISORT.

endform.

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - for internal table ISORT
*---------------------------------------------------------------------*
form init_table_record.
  data: vf_index type i value 1.
  while vf_index <= 88.
    ASSIGN COMPONENT vf_index OF STRUCTURE iSORT TO <F1>.
    <F1> = nodata.
    add 1 to vf_index.
  endwhile.
endform.

**---------------------------------------------------------------------*
**  Used to initialize the record to '/' - for internal table IERR
**---------------------------------------------------------------------*
*form init_table_record2.
*  data: vf_index type i value 1.
*  while vf_index <= 68.
*    ASSIGN COMPONENT vf_index OF STRUCTURE iERR TO <F1>.
*    <F1> = nodata.
*    add 1 to vf_index.
*  endwhile.
*endform.

*---------------------------------------------------------------------*
* collect data
*---------------------------------------------------------------------*
form collect_data.
*- prepare records table
  refresh: contrax.
*- collect data
  loop at wa.
    shift wa-AMOUNT   left deleting leading space.
    shift wa-VOLUME   left deleting leading space.
    shift wa-SR_USAGE left deleting leading space.

    clear: va_AMOUNT, va_AMOUNT2, va_VOLUME.
    move: wa-AMOUNT to va_AMOUNT, wa-VOLUME to va_VOLUME.

    if va_AMOUNT <> 0 or va_VOLUME <> 0.
      if ( ( ( va_AMOUNT > 0 and va_VOLUME >= 0 ) or
            ( va_AMOUNT < 0 and va_VOLUME <= 0 ) ) and
             va_AMOUNT <> 0 ) or wa-SR_USAGE = 'FUEL'.
*-       single record
         move-corresponding wa to contrax.
         append contrax. clear contrax.
      else.
*-       double record
         perform DOUBLE_RECORD.
         move-corresponding wa to contrax.
         move '0.00' to contrax-VOLUME.
         append contrax. clear contrax.

*-       double record 2
         perform DOUBLE_RECORD2.
         move-corresponding wa to contrax.
         append contrax. clear contrax.
      endif.
    endif.
  endloop.

endform.

*-----------------------------------------------------------------------
* Double Record
*-----------------------------------------------------------------------
form DOUBLE_RECORD.
  if ( va_AMOUNT > 0 or ( va_AMOUNT = 0 and va_VOLUME < 0 ) ).
    va_AMOUNT2 = va_AMOUNT + '0.01'.
  else.
    va_AMOUNT2 = va_AMOUNT - '0.01'.
  endif.
  move va_AMOUNT2 to wa-AMOUNT.
endform.

*-----------------------------------------------------------------------
* Double Record 2
*-----------------------------------------------------------------------
form DOUBLE_RECORD2.
  if ( va_AMOUNT > 0 or ( va_AMOUNT = 0 and va_VOLUME < 0 ) ).
    wa-AMOUNT = '-0.01'.
  else.
    wa-AMOUNT = '0.01'.
  endif.
endform.

*---------------------------------------------------------------------*
* sum data
*---------------------------------------------------------------------*
form summarize_data.
  data: va_menge type p decimals 3 value 0,
        va_wrbtr type p decimals 2 value 0,
        va_volume(18) type c.


*- prepare table summary
  refresh isum.

*- sort data
  sort isort by izbd1p ikostl ihkont inewbs imatnr iaufnr
                iwerks izuonr ikndnr ivkorg iwwbrn iwwrat
                iwwser iwwsct iwwseg.

* Compare to zbis254e.srt
*  loop at isort.
*     TRANSFER isort to P_FILEOT.
*  endloop.

*- summarize data
  loop at isort.

    if isort-HKONT = co_HKONT.
      move co_KOSTL to isort-kostl.
    endif.

    read table isum with key izbd1p = isort-izbd1p
                             ikostl = isort-ikostl
                             ihkont = isort-ihkont
                             inewbs = isort-inewbs
                             imatnr = isort-imatnr
                             iaufnr = isort-iaufnr
                             iwerks = isort-iwerks
                             izuonr = isort-izuonr
                             ikndnr = isort-ikndnr
                             ivkorg = isort-ivkorg
                             iwwbrn = isort-iwwbrn
                             iwwrat = isort-iwwrat
                             iwwser = isort-iwwser
                             iwwsct = isort-iwwsct
                             iwwseg = isort-iwwseg.

* If there is a match in table isum, then add isort
* amount and volume to isum record (sum up)
* If no match, then isort record is added to isum.

    if sy-subrc = 0.
* sum up dollar amounts.
      move isum-wrbtr to va_wrbtr.
      add isort-wrbtr to va_wrbtr.
      move va_wrbtr to isum-wrbtr.
      shift isum-wrbtr right deleting trailing space.
* sum up non-zero consumption.
      if isort-MENGE <> '/'.
        if isum-menge <> '/'.
           move isum-menge to va_menge.
        else.
           move 0 to va_menge.
        endif.
        add isort-menge to va_menge.
        move va_menge to isum-menge.
        shift isum-menge right deleting trailing space.
        move isort-meins to isum-meins.
      endif.
      modify isum index sy-tabix transporting wrbtr menge meins.
      clear isum.
    else.
*  no match in isum. Add isort to isum.
      if isort-WRBTR = '0' and isort-MENGE = '/'.
      else.
        move-corresponding isort to isum.
        append isum. clear isum.
      endif.
    endif.
  endloop.

*-----------------------------------------------------------------
* Original Mercator logic - not used (TR078)
* - these accounts will now post volumes in GJ
*-----------------------------------------------------------------
*  IF P_AEIDX = 'ACTUAL'.
*    loop at isum.
*      if ( isum-HKONT+4(6) = '152005' or
*           isum-HKONT+4(6) = '152100' or
*           isum-HKONT+4(6) = '152500' or
*           isum-HKONT+4(6) = '152600' ).
*         move '/' to isum-meins.
*         move isum-MENGE to va_volume.
*         shift va_volume left deleting leading space.
*         concatenate P_BKTXT 'Vol:' va_volume 'GJ' into isum-SGTXT
*           separated by space.
*         move '/' to isum-menge.
*         modify isum index sy-tabix transporting SGTXT MENGE MEINS.
*      endif.
*
*      if ( isort-MENGE = '/' and isort-MEINS <> 'M3' )
*         move '/' to isum-meins.
*         move '/' to isum-menge.
*         modify isum index sy-tabix transporting SGTXT MENGE MEINS.
*      endif.
*
*    endloop.
*  ENDIF.

endform.

*---------------------------------------------------------------------*
* create output file
*---------------------------------------------------------------------*
form create_output_file.
  data: mmenge type p decimals 3.
  data: switch type i value 0.

  PERFORM INIT_STRUCTURES USING 'BGR00'.
  move '0'              to z_bgr00-stype.
  move p_group          to z_bgr00-group.
  move sy-mandt         to z_bgr00-mandt.
  move 'BATCH'          to z_bgr00-usnam.
  TRANSFER Z_BGR00 to P_FILEOT LENGTH 38.

  loop at isum.
    at new izbd1p.
      switch = 0.
    endat.
    if switch = 0.
      switch = 1.

      PERFORM INIT_STRUCTURES USING 'BBKPF'.
      move '1'              to z_bbkpf-stype.
      move p_tcode          to z_bbkpf-tcode.
      move sy-datum         to z_bbkpf-bldat.
      move p_blart          to z_bbkpf-blart.
      move p_bukrs          to z_bbkpf-bukrs.
      move isum-ibudat      to z_bbkpf-budat.
      move p_waers          to z_bbkpf-waers.
      move p_xblnr          to z_bbkpf-xblnr.
      move p_bktxt          to z_bbkpf-bktxt.

      clear: z_bbkpf-numpg, z_bbkpf-stgrd, z_bbkpf-kursf_m,
             z_bbkpf-augtx, z_bbkpf-xprfg, z_bbkpf-sende  ,
             outrec.

      move z_bbkpf(216) to outrec.
      TRANSFER outrec to P_FILEOT LENGTH 284.
    endif.

    clear ZBSEG1.
    move-corresponding isum to ZBSEG1.
    translate zbseg1-wrbtr using '- '.
    translate zbseg1-menge using '- '.
    shift zbseg1-wrbtr right deleting trailing space.
    if zbseg1-menge CA '/'.
    else.
      move zbseg1-menge to mmenge.
      clear zbseg1-menge.
      move mmenge to zbseg1-menge.
      shift zbseg1-menge right deleting trailing space.
    endif.
    TRANSFER ZBSEG1 to P_FILEOT LENGTH 703.
  endloop.
endform.

*-----------------------------------------------------------------------
* Report: Display Error List
*-----------------------------------------------------------------------
form DISPLAY_ERROR_LST.
   loop at wa_err.
     write: /001 wa_err-APP_YR,
             006 wa_err-APP_MTH,
             009 wa_err-CUST_ID,
             018 wa_err-RATE_CL,
             031 wa_err-SERV_TYPE,
             038 wa_err-SERV_CL,
             051 wa_err-SEAS_CL,
             060 wa_err-RATE_TYPE,
             065 wa_err-CHARGE_TYPE,
             072 wa_err-NON_RATE_ITEM_TYP,
             085 wa_err-AMOUNT,
             103 wa_err-VOLUME,
             123 wa_err-SA_NUM.
   endloop.

endform.

*-----------------------------------------------------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields.
*-----------------------------------------------------------------------
form INPUT_FILE_TO_WA.
  do.
    clear: wa, INREC.
    read dataset p_filein into INREC.
*    if sy-index < 2.
*    elseif ( INREC is initial or sy-subrc <> 0 ).
    if sy-subrc <> 0.
      exit.
    else.
      split inrec at delimeter into
        wa-APP_YR         wa-APP_MTH      wa-CUST_ID
        wa-RATE_CL        wa-SERV_TYPE    wa-SERV_CL
        wa-SEAS_CL        wa-RATE_TYPE    wa-CHARGE_TYPE
        wa-SR_USAGE       wa-ST_SUBTYPE   wa-NON_RATE_ITEM_TYP
        wa-TIER_STEP_LVL  wa-SECTOR_SIZE  wa-SECTOR
        wa-AMOUNT         wa-VOLUME       wa-CUST_TYPE
        wa-GECA_CODE      wa-VOL_UOM      wa-SA_NUM.
      append wa.
    endif.
  enddo.
endform.

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - FOR BGR00, BBKPF
*---------------------------------------------------------------------*
form init_structures using tabname.
  select * from DD03L where tabname = tabname.
    clear char.
    char(2)   = 'Z_'.
    char+2(5) = tabname.
    char+7(1) = '-'.
    char+8    = DD03L-FIELDNAME.
    assign (char) to <F1>.
    <F1> = nodata.
  endselect.
endform.

*-----------------------------------------------------------------------
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*-----------------------------------------------------------------------
FORM OPEN_FILES.
  DATA: MSG(100).
*-----------------------------------------------------------------------
  open dataset p_filein for input in text mode message msg.
  if ( sy-subrc <> 0 ).
    message E002 with infile msg.
  endif.
*-----------------------------------------------------------------------
  open dataset p_fileot for output in text mode message msg.
  if ( sy-subrc <> 0 ).
    message E002 with outfile msg.
  endif.
*-----------------------------------------------------------------------
*  open dataset p_fileer for output in text mode message msg.
*  if ( sy-subrc <> 0 ).
*    message E002 with outfile msg.
*  endif.
ENDFORM.

*-----------------------------------------------------------------------
*  Initialize data
*-----------------------------------------------------------------------
FORM INITIALIZE_DATA.
  REFRESH: RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'DCCREC'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'DPTRANS'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'TTFEE'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'IMTFEE'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'DCCREVAD'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPOCNW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPOD'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPODF'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPODN'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPODNP'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPODNW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPODSP'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPODSW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPODW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SPODWW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOC'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOCF'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOCNW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOCN'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOCNP'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOCPW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOCSP'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOCSW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOCW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SSOCWW'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'SURPLSS'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'WAIVES'.
  APPEND RA_NONRAT.

  RA_NONRAT-SIGN     =  'I'.
  RA_NONRAT-OPTION   =  'EQ'.
  RA_NONRAT-LOW      =  'WAIVESS'.
  APPEND RA_NONRAT.

ENDFORM.

*-----------------------------------------------------------------------
*  Get WACOG and heating value constants from tables
*-----------------------------------------------------------------------
FORM GET_WACOG_HV_VALUES.

  select single * from zwacog where volume_uom = 'GJ'.
  wa_wacog = zwacog-wacog.

  select single * from zstconv where volume_uom = '103M3'.
  wa_heatv = zstconv-factor.

ENDFORM.
