REPORT  ZFFII039 MESSAGE-ID ZS.
*----------------------------------------------------------------------*
*  Author    : Glenn Ymana                        SAP : East
*  Date      : DEC, 2006                 Program Type : Interface
*  Issue Log : TR265
*----------------------------------------------------------------------*
*  Title : Return To System Interface (IFFI064)
*
*  Description: This interface will take RTS entries from Contrax and
*               post them to SAP
*
*----------------------------------------------------------------------*
*  Changes:
*  2007/11/30-gymana- TR392 M1/M2 rate class split. Adding rate class
*                     to input file layout and removed all ZFB02 lookups
*                     for rate class.
*  2009/01/16-gymana- TR580 - 6.0 SAP Upgrade
*                     Modified code to use ZKPAA005 BDC create ABAP.
*                     ZBSEG1-TBNAM now defined using table ZBSEG.
*                     20 byte offset removed from outrec when
*                     creating BBKPF record.
*---------------------------------------------------------------------*
tables:  DD03L,                    "Table fields
         ZFB01,                    "Banner Control Table
         ZFB02,                    "Banner Service Class Table
         ZFB03.                    "Banner Town Code Table

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
        wwlob(04) type c          ,    wwreg(04) type c          ,
        wwdvs(02) type c          ,    wwrgn(04) type c.
data: end of zbseg1.

data: begin of rtsinfile occurs 0,
        APP_YR(04)      type c,                   "Year
        APP_MO(02)      type c,                   "Month
        GECA(06)        type c,                   "GECA
        SERV_CL(02)     type c,                   "Service Class
        AMOUNT(16)      type c,                   "Amount
        VOLUME(18)      type c,                   "Volume
        GL_CL(04)       type c,                   "G/L Class code
        RATE_CL(02)     type c.                   "Rate Class code
data: end of rtsinfile.
data: itab like rtsinfile occurs 0 with header line.

data: begin of itab_err occurs 0,
        APP_YR(04)      type c,                   "Year
        APP_MO(02)      type c,                   "Month
        GECA(06)        type c,                   "GECA
        SERV_CL(02)     type c,                   "Service Class
        AMOUNT(16)      type c,                   "Amount
        VOLUME(18)      type c,                   "Volume
        GL_CL(04)       type c,                   "G/L Class code
        RATE_CL(02)     type c,                   "Rate Class code
        ERRMSG(50)      type c.                   "Error Message
data: end of itab_err.

data: begin of isort occurs 0,
        izterm     like zbseg-zterm,              "seqnum/docsize
        ibserv(01) type c,                        "/
        ikostl     like zbseg-kostl,              "/
        ihkont     like zbseg-hkont,              "G/L account
        inewbs     like zbseg-newbs,              "40 or 50
        imatnr     like zbseg-matnr,              "product
        iaufnr     like zbseg-aufnr,              "/
        isgtxt     like zbseg-sgtxt,              "recovery text
        iwerks(04) type c,                        "plant
        iprojk     like zbseg-projk,              "/
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

data: begin of Z_BGR00.
        include structure bgr00.
data: end of Z_BGR00.

data: begin of Z_BBKPF.
        include structure bbkpf.
data: end of Z_BBKPF.

data: wa_zterm     type i,
      wa_docsize   type i,
      char(21)     type c,
      nodata(1)    value '/'.

data: va_AMOUNT  type p decimals 2 value 0,
      va_AMOUNT2 type p decimals 2 value 0,
      va_VOLUME  type p decimals 3 value 0,
      va_MENGE   type p decimals 3 value 0.

data: delimiter value '09' type X.
data: WRK_SYMBOLIC(4) TYPE C VALUE '$sys'.

data: inrec(400),
      outrec(2000),
      infile(70),
      outfile(70).

field-symbols: <F1>.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
selection-screen begin of block box0 with frame.
selection-screen begin of block box1 with frame title text-100.

PARAMETERS: p_filein like filename-fileextern obligatory default
                     '/usr/sap/interfaces/$sys/IFFI064/ccfrts.chk',
            p_fileot like filename-fileextern obligatory default
                     '/usr/sap/interfaces/$sys/IFFI064/ccfrts.sap'.

selection-screen skip.
PARAMETERS: p_group  like bgr00-group obligatory default
                     'ZFI-RTS',
            p_tcode  like tstc-tcode  obligatory default 'FB01'.

selection-screen skip.
PARAMETERS: p_blart  like bbkpf-blart obligatory default 'S4',
            p_bukrs  like bbkpf-bukrs obligatory default 'UGL',
            p_waers  like bbkpf-waers obligatory default 'CAD',
            p_xblnr  like bbkpf-xblnr obligatory default
                     'CCF - RTS',
            p_bktxt  like bbkpf-bktxt obligatory default
                     'Return to System'.

selection-screen skip.
PARAMETERS: p_dsize(4) type c obligatory default '2000'.
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
  perform OPEN_FILES.
  perform INPUT_FILE_TO_WA.
  perform MOVEVOL.

*  for testing purposes
*  loop at itab.
*     TRANSFER itab to P_FILEOT.
*  endloop.

  perform COLLECT_DATA.

*  for testing purposes
*  loop at isort.
*     TRANSFER isort to P_FILEOT.
*  endloop.

  perform SUMMARIZE_DATA.

*  for testing purposes
*  loop at isum.
*     TRANSFER isum to P_FILEOT.
*  endloop.

  perform CREATE_OUTPUT_FILE.
  CLOSE DATASET: P_FILEIN, P_FILEOT.

  if not itab_err[] is initial.
    perform DISPLAY_ERROR_LIST.
  endif.

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
          018 'Service',
          026 'Transaction',
          039 'Transaction'.

  write: /002 'Applied',
          011 'GECA',
          018 'Class',
          026 'Amount',
          039 'Volume',
          052 'G/L Class',
          062 'Rate Class',
          074 'Error Message'.
  write: / sy-uline.

  FORMAT INTENSIFIED OFF.

*---------------------------------------------------------------------*
* collect data
*---------------------------------------------------------------------*
form collect_data.
*- prepare sort table
  refresh: isort, itab_err.
*- collect data
  loop at itab.
    move itab-AMOUNT to va_amount.
    move itab-VOLUME to va_volume.
    clear zfb01.
    select single * from zfb01
     where GLCODE  = itab-GL_CL
       and ( B_SRVCL = itab-SERV_CL or B_SRVCL = '**' ).
    if sy-subrc = 0.
      clear zfb03.
      select single * from zfb03
       where TOWNCODE = itab-GECA+0(2)
         and MUNICODE = itab-GECA+2(4).
      if sy-subrc = 0.
        perform F_ZBSEG_DETAIL.
      else.
        move-corresponding itab to itab_err.
        move
        'Town & Municipality code not found in ZFB03'
          to itab_err-errmsg.
        append itab_err. clear itab_err.
        perform F_ZBSEG_ERROR.
      endif.
    else.
      move-corresponding itab to itab_err.
      move 'G/L Class code not found in table ZFB01'
           to itab_err-errmsg.
      append itab_err. clear itab_err.
      perform F_ZBSEG_ERROR.
    endif.
  endloop.

endform.

*---------------------------------------------------------------------*
* sum data
*---------------------------------------------------------------------*
form summarize_data.
  data: va_menge type p decimals 3 value 0,
        va_wrbtr type p decimals 2 value 0.

  refresh isum.

*- sort data
  sort isort by izterm ibserv ikostl ihkont inewbs imatnr iaufnr
                isgtxt iwerks iprojk ikndnr ivkorg iwwbrn iwwrat
                iwwser iwwsct iwwseg.

* write sorted file.
*  loop at isort.
*     TRANSFER isort to P_FILEOT.
*  endloop.

*- summarize data
  loop at isort.

    read table isum with key izterm = isort-izterm
                             ibserv = isort-ibserv
                             ikostl = isort-ikostl
                             ihkont = isort-ihkont
                             inewbs = isort-inewbs
                             imatnr = isort-imatnr
                             iaufnr = isort-iaufnr
                             isgtxt = isort-isgtxt
                             iwerks = isort-iwerks
                             iprojk = isort-iprojk
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
      move isort-WRBTR to va_wrbtr.
      if va_wrbtr = 0 and isort-MENGE = '/'.
      else.
        move-corresponding isort to isum.
        append isum. clear isum.
      endif.
    endif.
  endloop.
endform.

*---------------------------------------------------------------------*
* create output file
*---------------------------------------------------------------------*
form create_output_file.
  data: wwrbtr type p decimals 2.
  data: switch type i value 0.

  loop at isum.
    at new izterm.
      switch = 0.
    endat.
    if switch = 0.
      switch = 1.
      PERFORM INIT_STRUCTURES USING 'BGR00'.
      move '0'              to z_bgr00-stype.
      move p_group          to z_bgr00-group.
      move sy-mandt         to z_bgr00-mandt.
      move 'BATCH'          to z_bgr00-usnam.
      TRANSFER Z_BGR00 to P_FILEOT LENGTH 38.

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
    if zbseg1-wrbtr CA '.'.
    else.
      move zbseg1-wrbtr to wwrbtr.
      move wwrbtr to zbseg1-wrbtr.
    endif.
    shift zbseg1-wrbtr right deleting trailing space.
    TRANSFER ZBSEG1 to P_FILEOT LENGTH 703.
  endloop.
endform.

*---------------------------------------------------------------------*
* F_ZBSEG_ERROR
*---------------------------------------------------------------------*
form f_ZBSEG_ERROR.
  clear: wa_zterm, wa_docsize.
  PERFORM INIT_STRUCTURE_ISORT.

  move p_dsize to wa_docsize.
  wa_zterm = ABS( sy-tabix / wa_docsize ).
  move wa_zterm      to isort-izterm.
  move '2'           to isort-stype.
  move 'ZBSEG'       to isort-tbnam.

  move sy-datum to isort-ibudat.

*  clear: va_AMOUNT, va_VOLUME.
*  move: itab-AMOUNT to va_AMOUNT, itab-VOLUME to va_VOLUME.
  if va_AMOUNT < 0 or ( va_AMOUNT = 0 and va_VOLUME < 0 ).
    isort-newbs = '40'.
  else.
    isort-newbs = '50'.
  endif.
  move isort-newbs to isort-inewbs.

  isort-WRBTR = itab-AMOUNT.
  translate isort-WRBTR using '- '.
  shift isort-WRBTR right deleting trailing space.

  clear INREC.
  concatenate itab-GECA '/' itab-SERV_CL '/' itab-GL_CL
              into INREC.
  concatenate 'RTS Error:' INREC into INREC
    separated by space.
  isort-SGTXT = INREC.

  select single * from ZFB01 where GLCODE = 'ERR'.
  move zfb01-mwskz to isort-mwskz.
  move zfb01-SAKNR to isort-HKONT.
  move isort-HKONT to isort-IHKONT.
  if zfb01-matnr is initial.
    move: '/' to isort-ikndnr, '/' to isort-ivkorg,
          '/' to isort-iwwbrn, '/' to isort-iwwrat,
          '/' to isort-iwwser, '/' to isort-iwwsct,
          '/' to isort-iwwseg.
  else.
    move zfb03-werks    to isort-iwerks.
    move '1'            to isort-ikndnr.
    perform F_LOOKUP_VKORG.
    move zfb03-wwbrn    to isort-iwwbrn.
    move itab-RATE_CL   to isort-iwwrat.

    select single * from zfb02
     where GLCODE  = itab-GL_CL
       and ( B_SERVCL = itab-SERV_CL or B_SERVCL = '**' ).
    if sy-subrc = 0.
*      move zfb02-ratecl to isort-iwwrat.
      move zfb02-servcl to isort-iwwser.
    else.
*      move '/' to isort-iwwrat.
      move '/' to isort-iwwser.
    endif.

    move 'Z'            to isort-iwwsct.
    move 'ZZ'           to isort-iwwseg.
  endif.

  APPEND ISORT.

*- additional offset line item
  if va_AMOUNT < 0 or ( va_AMOUNT = 0 and va_VOLUME < 0 ).
    isort-NEWBS = '50'.
  else.
    isort-NEWBS = '40'.
  endif.
  move isort-NEWBS  to isort-INEWBS.
  move zfb01-OACCT  to isort-HKONT.
  move zfb01-omwskz to isort-mwskz.
  move isort-HKONT  to isort-IHKONT.
  clear: isort-wwrgn.
  APPEND ISORT. CLEAR ISORT.

endform.

*---------------------------------------------------------------------*
* F_ZBSEG_DETAIL
*---------------------------------------------------------------------*
form F_ZBSEG_DETAIL.
  clear: wa_zterm, wa_docsize.
  PERFORM INIT_STRUCTURE_ISORT.
*- line item

  move p_dsize to wa_docsize.
  wa_zterm = ABS( sy-tabix / wa_docsize ).
* - keys
  move wa_zterm to isort-izterm.
  move zfb01-saknr to isort-ihkont.
  if va_amount < 0 or ( va_amount = 0 and va_VOLUME < 0 ).
    move '40' to isort-inewbs.
  else.
    move '50' to isort-inewbs.
  endif.
  move p_bktxt        to isort-isgtxt.
  move zfb01-matnr    to isort-imatnr.
  if zfb01-matnr is initial.
    move: '/' to isort-ikndnr, '/' to isort-ivkorg,
          '/' to isort-iwwbrn, '/' to isort-iwwrat,
          '/' to isort-iwwser, '/' to isort-iwwsct,
          '/' to isort-iwwseg.
  else.
    move zfb03-werks    to isort-iwerks.
    move '1'            to isort-ikndnr.
    perform F_LOOKUP_VKORG.
    move zfb03-wwbrn    to isort-iwwbrn.
    move itab-RATE_CL   to isort-iwwrat.

    select single * from zfb02
     where GLCODE  = itab-GL_CL
       and ( B_SERVCL = itab-SERV_CL or B_SERVCL = '**' ).
    if sy-subrc = 0.
*      move zfb02-ratecl to isort-iwwrat.
      move zfb02-servcl to isort-iwwser.
    else.
*      move '/' to isort-iwwrat.
      move '/' to isort-iwwser.
    endif.

    move 'Z'            to isort-iwwsct.
    move 'ZZ'           to isort-iwwseg.
  endif.

* -
  concatenate itab-APP_YR itab-APP_MO '01' into isort-ibudat.
  PERFORM COMMON_MOVES_FOR_ISORT.
  move zfb01-mwskz to isort-mwskz.
  append isort.

*- additional offset line item
  move zfb01-oacct to isort-ihkont.
  if va_amount < 0 or ( va_amount = 0 and va_VOLUME < 0 ).
    move '50' to isort-inewbs.
  else.
    move '40' to isort-inewbs.
  endif.

  move: '/' to isort-imatnr, '/' to isort-iwerks,
        '/' to isort-ikndnr, '/' to isort-ivkorg,
        '/' to isort-iwwbrn, '/' to isort-iwwrat,
        '/' to isort-iwwser, '/' to isort-iwwsct,
        '/' to isort-iwwseg.
  PERFORM COMMON_MOVES_FOR_ISORT.
*  move '/'          to isort-menge.
*  move '/'          to isort-meins.
  move zfb01-omwskz to isort-mwskz.

  append isort. clear isort.

endform.

*---------------------------------------------------------------------*
*  F_LOOKUP_VKORG.
*---------------------------------------------------------------------*
form F_LOOKUP_VKORG.
  if zfb03-werks = 'S100'.
    if itab-serv_cl = '21' or itab-serv_cl = '31'.
       move 'U015' to isort-ivkorg.
    else.
       move 'U010' to isort-ivkorg.
    endif.
  else.
    if itab-serv_cl = 'GJ' or itab-serv_cl = 'GS'.
       move 'C015' to isort-ivkorg.
    else.
       move 'C010' to isort-ivkorg.
    endif.
  endif.
 endform.

*---------------------------------------------------------------------*
*  This routine moves fields into ISORT
*---------------------------------------------------------------------*
form common_moves_for_ISORT.
  move '2'           to isort-stype.
  move 'ZBSEG'       to isort-tbnam.
  move isort-inewbs  to isort-newbs.

  move va_amount     to isort-wrbtr.
    translate isort-wrbtr using '- '.
  shift isort-wrbtr right deleting trailing space.

  if va_volume <> 0.
    move va_volume     to isort-menge.
    translate isort-menge using '- '.
    shift isort-menge right deleting trailing space.
    move 'M3' to isort-meins.
  else.
    move '/' to isort-menge.
  endif.

  move isort-ikostl  to isort-kostl.
  move isort-iaufnr  to isort-aufnr.
  move isort-imatnr  to isort-matnr.
  move isort-iwerks  to isort-werks.
  move isort-isgtxt  to isort-sgtxt.
  move isort-ihkont  to isort-hkont.
  move isort-iprojk  to isort-projk.
  move isort-ikndnr  to isort-kndnr.
  move isort-ivkorg  to isort-vkorg.
  move isort-iwwbrn  to isort-wwbrn.
  move isort-iwwsct  to isort-wwsct.
  move isort-iwwseg  to isort-wwseg.
  move isort-iwwrat  to isort-wwrat.
  move isort-iwwser  to isort-wwser.
  move isort-imatnr  to isort-artnr.
endform.

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - FOR TABLE ISORT
*---------------------------------------------------------------------*
form init_structure_isort.
  data: vf_index type i value 1.
  while vf_index <= 87.
    ASSIGN COMPONENT vf_index OF STRUCTURE isort TO <F1>.
    <F1> = nodata.
    add 1 to vf_index.
  endwhile.
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
* form MOVEVOL
*-----------------------------------------------------------------------
form MOVEVOL.
  refresh itab.

  loop at rtsinfile.
    shift rtsinfile-AMOUNT   left deleting leading space.
    shift rtsinfile-VOLUME   left deleting leading space.

    clear: va_AMOUNT, va_AMOUNT2, va_VOLUME.
    move: rtsinfile-AMOUNT to va_AMOUNT,
          rtsinfile-VOLUME to va_VOLUME.

    if va_AMOUNT <> 0 or va_VOLUME <> 0.
      perform WRITE_RECORD.

      if ( rtsinfile-GL_CL+3(1) = 'C' and
           rtsinfile-VOLUME <> 0 and
           rtsinfile-GL_CL(3) <> 'OWN' ).
*    -
        perform CREATE_VOL_RECORD.
        perform CREATE_NEG_VOL_RECORD.
*     -
      elseif rtsinfile-GL_CL+3(1) = ' ' and rtsinfile-GL_CL+2(1) = 'C'
        and rtsinfile-VOLUME <> 0 and rtsinfile-GL_CL(3) <> 'OWN'.
*     -
        perform CREATE_VOL_RECORD.
        perform CREATE_NEG_VOL_RECORD.
*     -
      endif.
    endif.
  endloop.
endform.
*-----------------------------------------------------------------------
* form CREATE_NEG_VOL_RECORD
*-----------------------------------------------------------------------
form CREATE_NEG_VOL_RECORD.
   move-corresponding rtsinfile to itab.

   if rtsinfile-GL_CL+3(1) = ' '.
     concatenate rtsinfile-GL_CL(2) 'D' into itab-GL_CL.
   else.
     concatenate rtsinfile-GL_CL(3) 'D' into itab-GL_CL.
   endif.
   if itab-VOLUME > 0.
     move '-0.01' to itab-AMOUNT.
   else.
     move '0.01' to itab-AMOUNT.
   endif.

   move '0.000' to itab-VOLUME.
   append itab. clear itab.
endform.

*-----------------------------------------------------------------------
* form CREATE_VOL_RECORD
*-----------------------------------------------------------------------
form CREATE_VOL_RECORD.
   move-corresponding rtsinfile to itab.

   if rtsinfile-GL_CL+3(1) = ' '.
     concatenate rtsinfile-GL_CL(2) 'D' into itab-GL_CL.
   else.
     concatenate rtsinfile-GL_CL(3) 'D' into itab-GL_CL.
   endif.
   if itab-VOLUME > 0.
     move '0.01' to itab-AMOUNT.
   else.
     move '-0.01' to itab-AMOUNT.
   endif.

   append itab. clear itab.
endform.

*-----------------------------------------------------------------------
* form WRITE_RECORD
*-----------------------------------------------------------------------
form WRITE_RECORD.
   move-corresponding rtsinfile to itab.

   if ( rtsinfile-GL_CL(3) = 'OWN' or rtsinfile-GL_CL = 'FRAN' or
        rtsinfile-GL_CL(3) = 'CMP' ).
     move rtsinfile-VOLUME to itab-VOLUME.
   else.
     move '0.000' to itab-VOLUME.
   endif.

   append itab. clear itab.
endform.

*-----------------------------------------------------------------------
*  Report: Display Error List
*-----------------------------------------------------------------------
form DISPLAY_ERROR_LIST.
   loop at itab_err.
     write: /002 itab_err-APP_YR,
             007 itab_err-APP_MO,
             011 itab_err-GECA,
             018 itab_err-SERV_CL,
             026 itab_err-AMOUNT,
             039 itab_err-VOLUME,
             052 itab_err-GL_CL,
             062 itab_err-RATE_CL,
             074 itab_err-ERRMSG.
   endloop.

endform.

*-----------------------------------------------------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (rtsinfile), separating
*  the record into its various fields.
*-----------------------------------------------------------------------
form INPUT_FILE_TO_WA.
  do.
    clear: rtsinfile, inrec.
    read dataset p_filein into INREC.
    if sy-subrc <> 0.
      exit.
    else.
      split inrec at delimiter into rtsinfile-APP_YR
                                    rtsinfile-APP_MO
                                    rtsinfile-GECA
                                    rtsinfile-SERV_CL
                                    rtsinfile-AMOUNT
                                    rtsinfile-VOLUME
                                    rtsinfile-GL_CL
                                    rtsinfile-RATE_CL.
      append rtsinfile.
    endif.
  enddo.
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
