REPORT  ZFFII033 message-id ZS.

*----------------------------------------------------------------------*
*  Author    : Adel D. Mendoza                    SAP : East
*  Date      : June, 2006                Program Type : Interface
*  Issue Log : TR147
*----------------------------------------------------------------------*
*  Title : Mercator Replacement - IFFI002 Banner Loans Interface
*  Description:
*     - Create one or more ABAP programs to replace 3 Mercator mapping
*     - programs that take an input file and creates a ZBSEG format
*     - BDC session. There are two seperate runs of the interface every
*     - month (IFFI022A - Loan Changes file, IFFI02BB - Loan Setups
*     - file), but the processing is exactly the same. The new ABAP can
*     - process both files using different variants.
*----------------------------------------------------------------------*
* 2006/07/17 - gymana - Made corrections to Adel's original code
*                       due to unit testing.
* 2006/11/13 - gymana - Added posting year/month to variant to allow
*                       the ability to post to a different month.
* 2007/04/09 - mkhan  - TR415
*                       SR (service replacement) indicator field
*                       added in table ZFB06 and it's corresponding
*                       field WBS SR added in table ZFB03 and program
*                       changes are made accordingly.
* 2007/07/04 - gymana - Corrected a bug that caused the program to
*                       abend when a ZFB06 entry was not found and
*                       left a '/' in wrbtr. (F_GET_WRBTR)
* 2009/01/15 - gymana - TR580 - 6.0 SAP Upgrade
*                       Corrected a bug that caused the program to
*                       generate another type '0' bdc record when a
*                       new document group is created.
* 2009/07/02 - gymana - TR466
*                       Modified program to deal with 3 character
*                       rate codes like 'EFT'. Input file sometimes
*                       has the space before the code. Table searching
*                       requires a trailing space.
* 2009/07/02 - gymana - TR466  HST
*                       Modified program to handle the new HST which
*                       replaces PST & GST. Line item entries will now
*                       post to the GST G/L acct# while PST entries will
*                       zeroed out. All references to GST will now be
*                       HST
*----------------------------------------------------------------------*

tables:  DD03L,               "table fields
         ZFB03,
         ZFB06,
         T247.

*Output file Document line item
data: begin of Z_ZBSEG,
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
data: end of Z_ZBSEG.

data: begin of wa,
        GECA_CODE(6)          type C,
        GL_CLASS_CODE(4)      type C,
        RATE_CODE(4)          type C,
        RATE_DESC(35)         type C,
        CUST_CODE(9)          type C,
        PREM_CODE(7)          type C,
        LOAN_TYPE(4)          type C,
        RATE_CD_BILL_DESC(36) type C,
        LOAN_AMT_TEXT(9)      type C,
        LOAN_POINT(1)         type C,
        LOAN_DECIMAL(2)       type C,
        LOAN_AMT_SIGN(1)      type C,
        STATUS(10)            type C,
        DATE_DAY(2)           type C,
        FILLER(1)             type C,
        DATE_MON(3)           type C,
        FILLER2(1)            type C,
        DATE_YEAR(4)          type C.
data: end of wa.

data: begin of itab occurs 0,
        GECA_CODE(6)          type C,
        GL_CLASS_CODE(4)      type C,
        RATE_CODE(4)          type C,
        RATE_DESC(35)         type C,
        CUST_CODE(9)          type C,
        PREM_CODE(7)          type C,
        LOAN_TYPE(4)          type C,
        RATE_CD_BILL_DESC(36) type C,
        LOAN_AMT_TEXT(9)      type C,
        LOAN_POINT(1)         type C,
        LOAN_DECIMAL(2)       type C,
        LOAN_AMT_SIGN(1)      type C,
        STATUS(10)            type C,
        DATUM(8)              type C,
        LOAN_AMT(16)          type C.
data: end of itab.

data: begin of isort occurs 0,
        iseqno(6)  type c,              "seqnum/docsize
        ihkont     like zbseg-hkont,
        inewbs     like zbseg-newbs,
        izterm     like zbseg-zterm,
        iprojk     like zbseg-projk,
        imwskz     like zbseg-mwskz,
        ibudat(08) type c.
        include structure Z_ZBSEG.
data: end of isort.
data: isum like isort occurs 0 with header line.

data: begin of Z_BGR00.
        include structure bgr00.
data: end of Z_BGR00.

data: begin of Z_BBKPF.
        include structure bbkpf.
data: end of Z_BBKPF.

data: wa_offset    like zfb06-saknr.
data: wa_seqno     type i,
      wa_docsize   type i,
      char(21)     type c,
      nodata(1)    value '/'.

data: delimeter value '09' type X.
data: inrec(400), outrec(2000), infile(70), outfile(70).
data: wrk_symbolic(4) type C value '$sys'.

data: va_hst      type p decimals 2.
*data: va_pst      type p decimals 2.                         "HST
data: va_amt      type p decimals 2.
data: va_divisor1 type p decimals 2.
data: va_divisor2 type p decimals 2.

field-symbols: <F1>.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
selection-screen begin of block box0 with frame.
selection-screen begin of block box1 with frame title text-100.

PARAMETERS: p_filein like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI002/zbis209.ftp',

            p_fileot like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI002/zbis209.mrc'.

selection-screen skip.
PARAMETERS: p_group  like bgr00-group obligatory default
                     'ZFI-BANLOANC'.

selection-screen skip.
PARAMETERS: p_usnam  like bgr00-usnam obligatory default 'BATCH',
            p_tcode  like tstc-tcode  obligatory default 'FB01',
            p_blart  like bbkpf-blart obligatory default 'S7',
            p_bukrs  like bbkpf-bukrs obligatory default 'UGL',
            p_waers  like bbkpf-waers obligatory default 'CAD',
            p_xblnr  like bbkpf-xblnr obligatory default
                     'Banner',
            p_bktxt  like bbkpf-bktxt obligatory default
                     'Banner Loan Changes',
            p_hstkon like Z_ZBSEG-hkont obligatory default        "HST
                     '0000256915',                                "HST
*           p_pskon  like Z_ZBSEG-hkont obligatory default        "HST
*                     '0000256510',                               "HST
            p_hstpct(2) type c obligatory default '13',           "HST
*            p_pspct(1) type c obligatory default '8',            "HST
            p_dsize(4) type c obligatory default '999'.

selection-screen skip.
selection-screen begin of line.
selection-screen comment 1(64) text-001.
selection-screen end of line.

PARAMETERS: p_postyr(4) type c,
            p_pstmth(2) type c.

selection-screen end of block box1.
selection-screen end of block box0.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
at selection-screen output.
  replace wrk_symbolic with sy-sysid into: p_filein, p_fileot.
  condense: p_filein no-gaps, p_fileot no-gaps.

*---------------------------------------------------------------------*
* start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
  perform OPEN_FILES.
  perform F_MOVES.
  perform F_BANLOAN2.
  perform F_SUM.
  perform F_CREATE_OUTPUT_FILE.

  close dataset: p_filein, p_fileot.

*---------------------------------------------------------------------*
* create output file
*---------------------------------------------------------------------*
form F_CREATE_OUTPUT_FILE.
  data: switch type i value 0.

  PERFORM INIT_STRUCTURES USING 'BGR00'.           "6.0 Upgrade
  move '0'              to z_bgr00-stype.          "6.0 Upgrade
  move p_group          to z_bgr00-group.          "6.0 Upgrade
  move sy-mandt         to z_bgr00-mandt.          "6.0 Upgrade
  move 'BATCH'          to z_bgr00-usnam.          "6.0 Upgrade
  TRANSFER Z_BGR00 to P_FILEOT.                    "6.0 Upgrade

  loop at isum.
    at new iseqno.
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
      TRANSFER outrec to P_FILEOT.

    endif.

    clear Z_ZBSEG.
    move-corresponding isum to Z_ZBSEG.
    move Z_ZBSEG-wrbtr to va_amt.      "Change wrbtr to correct format
    move va_amt to Z_ZBSEG-wrbtr.
    translate Z_ZBSEG-wrbtr using '- '.
    shift Z_ZBSEG-wrbtr right deleting trailing space.

    TRANSFER Z_ZBSEG to P_FILEOT.
  endloop.
endform.


*-----------------------------------------------------------------------
* form F_SUM
*-----------------------------------------------------------------------
form F_SUM.
  refresh isum.
*- sort data
  sort isort by iseqno ihkont inewbs izterm iprojk imwskz.
*- summarize data
  loop at isort.
    read table isum with key iseqno = isort-iseqno
                             ihkont = isort-ihkont
                             inewbs = isort-inewbs
                             izterm = isort-izterm
                             iprojk = isort-iprojk
                             imwskz = isort-imwskz.
    if sy-subrc = 0.
      add isort-wrbtr  to isum-wrbtr.
      modify isum index sy-tabix transporting wrbtr.
      clear isum.
    else.
      move-corresponding isort to isum.
      append isum. clear isum.
    endif.
  endloop.
endform.

*-----------------------------------------------------------------------
* form F_BANLOAN2
*-----------------------------------------------------------------------
form F_BANLOAN2.
  data: va_tabix like sy-tabix.
  refresh isort.
  va_hst      = ( p_hstpct / 100 ).    "HST %                     HST
*  va_pst      = ( p_pspct / 100 ).    "PST %                     HST
*  va_divisor1 = 1 + va_hst + va_pst.  "1 + GST + PST             HST
  va_divisor2 = 1 + va_hst.           "1 + HST                    HST
  loop at itab.
    move sy-tabix to va_tabix.
    perform F_LEGACY_TO_ZFBSEG using va_tabix.

    select single * from ZFB06 where B_RATE_CD = itab-RATE_CODE.
    if sy-subrc = 0.
       if zfb06-GST_IND = 'X'.
          perform F_HST using va_tabix.                          "HST
       endif.
*       if zfb06-PST_IND = 'X'.                                  "HST
*          perform F_PST using va_tabix.                         "HST
*       endif.                                                   "HST
    endif.

  endloop.
endform.

*-----------------------------------------------------------------------
* form F_PST
*-----------------------------------------------------------------------
*form F_PST using Itabix.                                        "HST
*  clear: wa_seqno, wa_docsize, wa_offset.                       "HST
*  perform INIT_TABLE_RECORD.                                    "HST
*  move '2'              to isort-stype.                         "HST
*  move 'ZBSEG'          to isort-tbnam.                         "HST
*  move p_dsize to wa_docsize.                                   "HST
*  wa_seqno = ABS( Itabix / wa_docsize ).                        "HST
                                                                 "HST
*  move wa_seqno    to isort-ISEQNO.                             "HST
*  move p_pskon     to isort-IHKONT.                             "HST
*  shift isort-IHKONT right deleting trailing space.             "HST
*  translate isort-IHKONT using ' 0'.                            "HST
*  if itab-LOAN_AMT > 0.                                         "HST
*    move '50' to isort-INEWBS.                                  "HST
*  else.                                                         "HST
*    move '40' to isort-INEWBS.                                  "HST
*  endif.                                                        "HST
*                                                                "HST
*  clear va_amt.                                                 "HST
*                                                                "HST
*  va_amt = itab-LOAN_AMT * ( va_pst / va_divisor1 ).            "HST
*                                                                "HST
*  move isort-INEWBS to isort-NEWBS.                             "HST
*  move va_amt to isort-wrbtr.                                   "HST
*  perform F_GET_SGTXT using itab-RATE_CODE changing isort-SGTXT."HST
*  move isort-IHKONT to isort-HKONT.                             "HST
*  move itab-datum to isort-IBUDAT.                              "HST
*                                                                "HST
*  append isort. clear isort.                                    "HST
*                                                                "HST
*endform.                                                        "HST

*-----------------------------------------------------------------------
* form F_HST
*-----------------------------------------------------------------------
form F_HST using Itabix.                                         "HST
  clear: wa_seqno, wa_docsize, wa_offset.
  perform INIT_TABLE_RECORD.
  move '2'              to isort-stype.
  move 'ZBSEG'          to isort-tbnam.
  move p_dsize to wa_docsize.
  wa_seqno = ABS( Itabix / wa_docsize ).

  move wa_seqno    to isort-ISEQNO.
  move p_hstkon     to isort-IHKONT.
  shift isort-IHKONT right deleting trailing space.
  translate isort-IHKONT using ' 0'.
  if itab-LOAN_AMT > 0.
    move '50' to isort-INEWBS.
  else.
    move '40' to isort-INEWBS.
  endif.

  clear va_amt.

  select single * from ZFB06 where B_RATE_CD = itab-RATE_CODE.
  if sy-subrc = 0.
    va_amt = itab-LOAN_AMT * ( va_hst / va_divisor2 ).            "HST
*    if zfb06-PST_IND = 'X'.                                      "HST
*      va_amt = itab-LOAN_AMT * ( va_gst / va_divisor1 ).         "HST
*    else.                                                        "HST
*      va_amt = itab-LOAN_AMT * ( va_gst / va_divisor2 ).         "HST
*    endif.                                                       "HST
  endif.

  move isort-INEWBS to isort-NEWBS.
  move va_amt to isort-wrbtr.
  perform F_GET_SGTXT using itab-RATE_CODE changing isort-SGTXT.
  move isort-IHKONT to isort-HKONT.
  move itab-datum to isort-ibudat.

  append isort. clear isort.

endform.

*-----------------------------------------------------------------------
* form F_LEGACY_TO_ZFBSEG
*-----------------------------------------------------------------------
form F_LEGACY_TO_ZFBSEG using Itabix.
  clear: wa_seqno, wa_docsize, wa_offset.
  perform INIT_TABLE_RECORD.
  move '2'              to isort-stype.
  move 'ZBSEG'          to isort-tbnam.
  move p_dsize to wa_docsize.
  wa_seqno = ABS( Itabix / wa_docsize ).

  move wa_seqno    to isort-ISEQNO.

  perform F_GET_GLACCOUNT using itab-RATE_CODE
                       changing isort-IHKONT
                                wa_offset.
  if itab-LOAN_AMT > 0.
    move '50' to isort-INEWBS.
  else.
    move '40' to isort-INEWBS.
  endif.

  perform F_GET_ZTERM using itab-RATE_CODE changing isort-IZTERM.
  perform F_GET_PROJK using itab-RATE_CODE
                            itab-GECA_CODE changing isort-IPROJK.
  perform F_GEt_MWSKZ using itab-RATE_CODE changing isort-IMWSKZ.

  move isort-INEWBS to isort-NEWBS.

  perform F_GET_WRBTR using itab-RATE_CODE
                            isort-IHKONT
                            itab-LOAN_AMT  changing isort-WRBTR.

  move isort-IMWSKZ to isort-MWSKZ.

  perform F_GET_SGTXT using itab-RATE_CODE changing isort-SGTXT.

  move isort-IHKONT to isort-HKONT.
  move isort-IPROJK to isort-PROJK.

  move itab-datum   to isort-ibudat.
  append isort.

* -------------------------------
* offset line item
* -------------------------------
  move wa_offset to isort-IHKONT.
  if itab-LOAN_AMT > 0.
    move '40' to isort-INEWBS.
  else.
    move '50' to isort-INEWBS.
  endif.
  move nodata to isort-IZTERM.
  move nodata to isort-IPROJK.
  move 'o0' to isort-IMWSKZ.

  move isort-INEWBS to isort-NEWBS.
  move itab-LOAN_AMT to isort-WRBTR.
  move isort-IMWSKZ to isort-MWSKZ.
  move isort-IHKONT to isort-HKONT.
  move isort-IPROJK to isort-PROJK.

  append isort. clear isort.

endform.

*---------------------------------------------------------------------*
* get SGTXT
*---------------------------------------------------------------------*
form F_GET_SGTXT using Iratecode changing Esgtxt.
  select single * from ZFB06 where B_RATE_CD = iRATECODE
                               and SAKNR <> ''.
  if sy-subrc = 0.
    move p_bktxt to Esgtxt.
  else.
    concatenate p_bktxt Iratecode into Esgtxt separated
      by space.
  endif.

endform.

*---------------------------------------------------------------------*
* get WRBTR
*---------------------------------------------------------------------*
form F_GET_WRBTR using Iratecode Ihkont Iloanamt changing Ewrbtr.
  if iHKONT+4(6) <> '160301'.
    select single * from ZFB06 where B_RATE_CD = iRATECODE.
    if sy-subrc = 0.
      perform F_AMT_CALC using zfb06-GST_IND zfb06-PST_IND Iloanamt
                      changing Ewrbtr.
    else.
      move Iloanamt to Ewrbtr.
    endif.
  else.
    move Iloanamt to Ewrbtr.
  endif.
endform.

*---------------------------------------------------------------------*
* calculate WRBTR with HST
*---------------------------------------------------------------------*
form F_AMT_CALC using Igst Ipst Iamt changing Ewrbtr.
  clear va_amt.

  if iGST = 'X'.
    va_amt = ( Iamt - ( Iamt * ( va_hst / va_divisor2 ) ) ).      "HST
*    if iPST = 'X'.                                               "HST
*      va_amt = ( Iamt - ( Iamt * ( va_hst / va_divisor1 ) )      "HST
*                      - ( Iamt * ( va_pst / va_divisor1 ) ) ).   "HST
*    else.                                                        "HST
*      va_amt = ( Iamt - ( Iamt * ( va_hst / va_divisor2 ) ) ).   "HST
*    endif.
  else.
    move Iamt to va_amt.
  endif.
  move va_amt to Ewrbtr.
endform.

*---------------------------------------------------------------------*
* get MWSKZ
*---------------------------------------------------------------------*
form F_GEt_MWSKZ using Iratecode changing Emwskz.
  select single * from ZFB06 where B_RATE_CD = iRATECODE.
  if sy-subrc = 0.
    move zfb06-MWSKZ to Emwskz.
  else.
    move nodata to Emwskz.
  endif.
endform.

*---------------------------------------------------------------------*
* get PROJK
*---------------------------------------------------------------------*
form F_GET_PROJK using Iratecode Igeca changing Eprojk.
  move nodata to Eprojk.
  select single * from ZFB06 where B_RATE_CD = iRATECODE.
  if sy-subrc = 0.
* Start of TR415 changes
* Old code
*    if zfb06-WBS_EF_IND = 'X'.
*      select single * from ZFB03 where TOWNCODE = iGECA+0(2)
*                                   and MUNICODE = iGECA+2(4).
*      if sy-subrc = 0.
*        move zfb03-WBS_EF to Eprojk.
*      endif.
*    elseif zfb06-WBS_ME_IND = 'X'.
*      select single * from ZFB03 where TOWNCODE = iGECA+0(2)
*                                   and MUNICODE = iGECA+2(4).
*      if sy-subrc = 0.
*        move zfb03-WBS_MEXT to Eprojk.
*      endif.
*    elseif not zfb06-WBS_EF is initial.
*      move zfb06-WBS_EF to Eprojk.
*    endif.
*
*New code
      select single * from ZFB03 where TOWNCODE = iGECA+0(2)
                                   and MUNICODE = iGECA+2(4).
      if sy-subrc = 0.
         if zfb06-WBS_EF_IND = 'X'.
            move zfb03-WBS_EF to Eprojk.
         elseif zfb06-WBS_ME_IND = 'X'.
            move zfb03-WBS_MEXT to Eprojk.
         elseif zfb06-WBS_SR_IND = 'X'.         "WBS Service Replacement
            move zfb03-WBS_SR to Eprojk.
         elseif not zfb06-WBS_EF is initial.
            move zfb06-WBS_EF to Eprojk.
         endif.
      endif.
* End of TR415 changes
  endif.
endform.

*---------------------------------------------------------------------*
* get ZTERM
*---------------------------------------------------------------------*
form F_GET_ZTERM using Iratecode changing Ezterm.
  select single * from ZFB06 where B_RATE_CD = iRATECODE.
  if sy-subrc = 0.
    move nodata to Ezterm.
  else.
    move iRATECODE to Ezterm.
  endif.
endform.

*---------------------------------------------------------------------*
* get G/L Account
*---------------------------------------------------------------------*
form F_GET_GLACCOUNT using Iratecode changing Ehkont Eoffset.
  move nodata to Ehkont.
  select single * from ZFB06 where B_RATE_CD = iRATECODE
                               and SAKNR <> ''.
  if sy-subrc = 0.
    move zfb06-SAKNR to Ehkont.
    move zfb06-OACCT to Eoffset.
  else.
    select single * from ZFB06 where B_RATE_CD = 'ERRR'.
    if sy-subrc = 0.
      move zfb06-SAKNR to Ehkont.
      move zfb06-OACCT to Eoffset.
    endif.
  endif.
endform.


*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - for internal table ISORT
*---------------------------------------------------------------------*
form init_table_record.
  data: vf_index type i value 1.
  while vf_index <= 7.
    ASSIGN COMPONENT vf_index OF STRUCTURE iSORT TO <F1>.
    <F1> = nodata.
    add 1 to vf_index.
  endwhile.
  perform init_structures using 'ZBSEG'.
  move-corresponding z_zbseg to isort.
  move '/' to isort-werks.
endform.

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - FOR BGR00, BBKPF, ZBSEG
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
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields and save to internal table itab.
*-----------------------------------------------------------------------
form F_MOVES.

  refresh itab.
  do.
    clear: itab, wa, INREC.
    read dataset p_filein into INREC.

    if ( INREC is initial or sy-subrc <> 0 ).
      exit.
    else.
      move INREC to WA.
      shift WA-RATE_CODE left deleting leading SPACE.
      move-corresponding wa to itab.

      if wa-GECA_CODE+4(2) is initial.
        concatenate wa-GECA_CODE(4) '00' into itab-GECA_CODE.
      endif.

      if wa-LOAN_AMT_SIGN = '-'.
        concatenate wa-LOAN_AMT_TEXT wa-LOAN_POINT wa-LOAN_DECIMAL
          wa-LOAN_AMT_SIGN into itab-LOAN_AMT.
      else.
        move '0' to itab-LOAN_AMT_SIGN.
        concatenate wa-LOAN_AMT_TEXT wa-LOAN_POINT wa-LOAN_DECIMAL
          into itab-LOAN_AMT.
      endif.

      if p_postyr is initial and p_pstmth is initial.
        perform F_FORMAT_DATE using wa-DATE_DAY wa-DATE_MON wa-DATE_YEAR
           changing itab-DATUM.
      else.
        concatenate p_postyr p_pstmth '01' into itab-datum.
      endif.
      append itab.
    endif.
  enddo.
endform.

*-----------------------------------------------------------------------
* format date
*-----------------------------------------------------------------------
form F_FORMAT_DATE using Iday Imon Iyear changing Edate.
  select single * from t247 where spras = 'EN'
                              and ktx   = Imon.
  concatenate Iyear t247-mnr '01' into Edate.
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
ENDFORM.
