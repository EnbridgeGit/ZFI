REPORT ZFFII027 MESSAGE-ID ZS.
*----------------------------------------------------------------------*
*  Author    : Adel D. Mendoza                    SAP : East
*  Date      : May, 2006                 Program Type : Interface
*  Issue Log : TR152
*----------------------------------------------------------------------*
*  Title : Mercator Replacement - IFFI059 Prospective Recovery
*        : Interface
*  Description:
*     - To replace two Mercator mapping programs that take an
*     - input file and creates a ZBSEG format BDC session.
*----------------------------------------------------------------------*
* Transport request no. : D30K912940
*----------------------------------------------------------------------*
tables:  DD03L.                                   "table fields

data: begin of zbseg1,
        stype     like zbseg-stype,    tbnam(10) type c,
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

data: begin of wa occurs 0,
        SAORG(04) type c,                         "Sales Org
        SCLAS(02) type c,                         "Service Class
        RCLAS(03) type c,                         "Rate Class
        PYEAR(08) type c,                         "Paid Year
        BRANC(04) type c,                         "Branch
        PLANT(04) type c,                         "plant
        GLACT(10) type c,                         "G/L Account
        MATNR(18) type c,                         "Product
        REVAM(15) type c,                         "Revenue Amount
        VOLUM(15) type c,                         "Volume
        RECTX(50) type c.                         "Recovery Text
data: end of wa.

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

data: delimeter value '09' type X.

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
                     '/usr/sap/interfaces/P01/BANNER/zbis104.chk',
            p_fileot like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/BANNER/zbis104.mrc'.

selection-screen skip.
PARAMETERS: p_group  like bgr00-group obligatory default
                     'ZFI-PRORECOV',
            p_tcode  like tstc-tcode  obligatory default 'FB01'.

selection-screen skip.
PARAMETERS: p_blart  like bbkpf-blart obligatory default 'S4',
            p_bukrs  like bbkpf-bukrs obligatory default 'UGL',
            p_waers  like bbkpf-waers obligatory default 'CAD',
            p_xblnr  like bbkpf-xblnr obligatory default
                     'PROSPEC RECOVERY',
            p_bktxt  like bbkpf-bktxt obligatory default
                     'Prosp Recovery Interface',
            p_hkont  like zbseg1-hkont obligatory default '140500'.

selection-screen skip.
PARAMETERS: p_dsize(4) type c obligatory default '2000'.
selection-screen end of block box1.
selection-screen end of block box0.

*---------------------------------------------------------------------*
* start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM OPEN_FILES.
  perform INPUT_FILE_TO_WA.
  perform collect_data.
  perform summarize_data.
  PERFORM CREATE_OUTPUT_FILE.
  CLOSE DATASET: P_FILEIN, P_FILEOT.


*---------------------------------------------------------------------*
* collect data
*---------------------------------------------------------------------*
form collect_data.
*- prepare sort table
  refresh: isort.
*- collect data
  loop at wa.
    shift wa-REVAM left deleting leading space.
    shift wa-GLACT left deleting leading space.
    if wa-REVAM(4) = '0.00'.
*     - skip all records with zero amount
    else.
      if wa-GLACT(3) = '179'.
        perform F_ZBSEG_OTHER.
      else.
        perform F_ZBSEG_DETAIL.
      endif.
    endif.
  endloop.
endform.

*---------------------------------------------------------------------*
* sum data
*---------------------------------------------------------------------*
form summarize_data.
*- prepare table summary
  refresh isum.
*- sort data
  sort isort by izterm ibserv ikostl ihkont inewbs imatnr iaufnr
                isgtxt iwerks iprojk ikndnr ivkorg iwwbrn iwwrat
                iwwser iwwsct iwwseg.
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
    if sy-subrc = 0.
      add isort-ivolum to isum-ivolum.
      add isort-wrbtr  to isum-wrbtr.
      modify isum index sy-tabix transporting ivolum wrbtr.
      clear isum.
    else.
      move-corresponding isort to isum.
      append isum. clear isum.
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
      concatenate outrec(5) outrec+21 into outrec.
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
* F_ZBSEG_OTHER
*---------------------------------------------------------------------*
form F_ZBSEG_OTHER.
  clear: wa_zterm, wa_docsize.
  PERFORM INIT_STRUCTURE_ISORT.
*- line item
  move p_dsize to wa_docsize.
  wa_zterm = ABS( sy-tabix / wa_docsize ).
* - keys
  move wa_zterm to isort-izterm.
  move wa-GLACT to isort-ihkont.
  if wa-REVAM CA '-'.
    move '40' to isort-inewbs.
  else.
    move '50' to isort-inewbs.
  endif.
  move wa-RECTX to isort-isgtxt.
  move '1'      to isort-ikndnr.
  move 'Z'      to isort-iwwsct.
* -
  concatenate wa-PYEAR(4) wa-PYEAR+6(2) '01' into isort-ibudat.
  move wa-VOLUM to isort-ivolum.
  PERFORM COMMON_MOVES_FOR_ISORT.
  append isort.

*- additional offset line item
  move p_hkont+4(6) to isort-ihkont.
  if wa-REVAM CA '-'.
    move '50' to isort-inewbs.
  else.
    move '40' to isort-inewbs.
  endif.
  move '/' to isort-ikndnr.
  move '/' to isort-iwwsct.

  PERFORM COMMON_MOVES_FOR_ISORT.
  append isort. clear isort.

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
  move wa-GLACT to isort-ihkont.
  if wa-REVAM CA '-'.
    move '40' to isort-inewbs.
  else.
    move '50' to isort-inewbs.
  endif.
  move wa-MATNR to isort-imatnr.
  move wa-RECTX to isort-isgtxt.
  move wa-PLANT to isort-iwerks.
  move '1'      to isort-ikndnr.
  move wa-SAORG to isort-ivkorg.
  move wa-BRANC to isort-iwwbrn.
  move wa-RCLAS to isort-iwwrat.
  move wa-SCLAS to isort-iwwser.
  move 'Z'      to isort-iwwsct.
* -
  concatenate wa-PYEAR(4) wa-PYEAR+6(2) '01' into isort-ibudat.
  move wa-VOLUM to isort-ivolum.
  PERFORM COMMON_MOVES_FOR_ISORT.
  move 'O0' to isort-mwskz.
  append isort.

*- additional offset line item
  move p_hkont+4(6) to isort-ihkont.
  if wa-REVAM CA '-'.
    move '50' to isort-inewbs.
  else.
    move '40' to isort-inewbs.
  endif.
  move: '/' to isort-imatnr, '/' to isort-iwerks, '/' to isort-ikndnr,
        '/' to isort-ivkorg, '/' to isort-iwwbrn, '/' to isort-iwwrat,
        '/' to isort-iwwser, '/' to isort-iwwsct.

  PERFORM COMMON_MOVES_FOR_ISORT.
  move '/' to isort-mwskz.
  append isort. clear isort.

endform.

*---------------------------------------------------------------------*
*  This routine moves fields into ISORT
*---------------------------------------------------------------------*
form common_moves_for_ISORT.
  move '2'           to isort-stype.
  move 'ZBSEG'       to isort-tbnam.
  move isort-inewbs  to isort-newbs.
  write wa-REVAM     to isort-wrbtr.
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
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields.
*-----------------------------------------------------------------------
form INPUT_FILE_TO_WA.
  do.
    clear: wa, inrec.
    read dataset p_filein into INREC.
    if sy-index < 2.
    elseif INREC is initial.
      exit.
    else.
      split inrec at delimeter into
         wa-SAORG  wa-SCLAS  wa-RCLAS  wa-PYEAR  wa-BRANC  wa-PLANT
         wa-GLACT  wa-MATNR  wa-REVAM  wa-VOLUM  wa-RECTX.
      append wa.
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
