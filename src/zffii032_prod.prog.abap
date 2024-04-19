REPORT  ZFFII032 message-id zs NO STANDARD PAGE HEADING
                               LINE-SIZE 130
                               LINE-COUNT 65.

*----------------------------------------------------------------------*
*  Author    : Adel D. Mendoza                    SAP : East
*  Date      : July, 2006                Program Type : Interface
*  Issue Log : TR150
*----------------------------------------------------------------------*
*  Title : Mercator Replacement - IFFI056 Customer Count Interface
*  Description:
*     - Create two ABAP programs to replace 3 Mercator mapping
*     - programs and 2 C++ scripts. The first ABAP will format the
*     - file, sort and remove duplicate records. The second ABAP will
*     - format the file for SAP posting. The ABAP should also report
*     - the count automatically and either print or e-mail the report
*     - to the client.
*  NOTE: Record length of output file was fixed since blanks in the
*        remaining fields will be truncated. If operating concern 1100
*        is changed, test this abap if the ZZPA1100 record length has
*        been changed. (p_reclen = ZZPA1100 length - 1 byte)
*----------------------------------------------------------------------*
* 2006/07/26 - gymana - Made code corrections as a result of the unit
*                       Testing.
*----------------------------------------------------------------------*


tables: zfb01,
        zfb02,
        zfb03.

data: begin of wa occurs 0,
        GLCL(04)         type C,  "GL class code
        SCLS(02)         type C,  "Service class code
        GECA(06)         type C,  "Geographical area code
        BILLDATE(08)     type C,  "Banner billing date (MMDDYYYY)
        GRPCNT(8)        type C.  "Group count
data: end of wa.

data: begin of wa_err occurs 0,
        GLCL(04)         type C,  "GL class code
        SCLS(02)         type C,  "Service class code
        GECA(06)         type C,  "Geographical area code
        BILLDATE(08)     type C,  "Banner billing date (MMDDYYYY)
        GRPCNT(20)       type C,  "Group count
        ERRMSG(60)       type C.  "Error message
data: end of wa_err.

data: begin of isort occurs 0,
        ikndnr    like zzpa1100-kndnr,
        iartnr    like zzpa1100-artnr,
        ibukrs    like zzpa1100-bukrs,
        ivkorg    like zzpa1100-vkorg,
        iwerks    like zzpa1100-werks,
        iwwbrn    like zzpa1100-wwbrn,
        iwwsct    like zzpa1100-wwsct,
        iwwseg    like zzpa1100-wwseg,
        iwwrat    like zzpa1100-wwrat,
        iwwser    like zzpa1100-wwser,
        ifadat    like zzpa1100-fadat.
        include structure zzpa1100.
data: end of isort.

data: isum like zzpa1100 occurs 0 with header line.

data: begin of Z_ZZPA1100.
        include structure ZZPA1100.
data: end of Z_ZZPA1100.

data: inrec(400), outrec(2000), infile(70), outfile(70).
data: wrk_symbolic(4) type C value '$sys'.

data: wa_seqno     type i,
      char(21)     type c,
      nodata(1)    value '/'.

field-symbols: <F1>.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
selection-screen begin of block box0 with frame.
selection-screen begin of block box1 with frame title text-100.

PARAMETERS: p_filein like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI056/zbis103.ftp',

            p_fileot like filename-fileextern obligatory default
                     '/usr/sap/interfaces/P01/IFFI056/zbis103.mrc'.

selection-screen skip.
PARAMETERS: p_reclen(4) type c obligatory default '256'.

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
  perform F_NUMBCUS1.
  perform F_UNIQUE.
  perform F_CREATE_OUTPUT_FILE.
  close dataset: p_filein, p_fileot.

  if not wa_err[] is initial.
    perform F_REPORT_ERROR.
  else.
    MESSAGE ID 'ZS' TYPE 'I' NUMBER '019' WITH
     'Mapping successfully completed. No errors found.'.
  endif.

*---------------------------------------------------------------------*
* top of page
*---------------------------------------------------------------------*
TOP-OF-PAGE.

  format intensified on.
  write: /1  'ZFFII032',
          40 'BANNER CUSTOMER COUNT ERROR REPORT'.
  write: 100 'DATE:', sy-datum, '@', sy-uzeit.
  write: /1 'IFFI056',
          116 'Page:', sy-pagno.
  format intensified off.
  skip .

  write: / sy-uline.

  write: /001 'G/L Class',
          011 'Serv. Cl',
          023 'Geographical',
          038 'Banner',
          053 'Banner',
          070 'Error'.

  write: /001 'Code',
          011 'Code',
          023 'Area Code',
          038 'Bill Date',
          053 'Group Count',
          070 'Message'.

  write: / sy-uline.
  format intensified off.

*---------------------------------------------------------------------*
* report the errors
*---------------------------------------------------------------------*
form F_REPORT_ERROR.
  data: va_date(10) type C.
  loop at wa_err.
    concatenate wa_err-billdate+4(4) '/' wa_err-billdate(2) '/'
      wa_err-billdate+2(2) into va_date.

    write: /001 wa_err-GLCL,
            011 wa_err-SCLS,
            023 wa_err-GECA,
            038 va_date,
            053 wa_err-GRPCNT,
            070 wa_err-ERRMSG.
  endloop.
endform.

*---------------------------------------------------------------------*
* create output file
*---------------------------------------------------------------------*
form F_CREATE_OUTPUT_FILE.
  data: switch type i value 0.
  loop at isum.
    clear Z_ZZPA1100.
    move-corresponding isum to Z_ZZPA1100.
    TRANSFER Z_ZZPA1100 to P_FILEOT LENGTH P_RECLEN.
  endloop.
endform.

*---------------------------------------------------------------------*
* form F_UNIQUE
*---------------------------------------------------------------------*
form F_UNIQUE.
  refresh isum.
*- sort data
  sort isort by ikndnr iartnr ibukrs ivkorg iwerks
                iwwbrn iwwsct iwwseg iwwrat iwwser ifadat.

* Used for testing purposes only. Compare outfile to zbis103b.srt
*
*  loop at isort.
*    transfer isort to p_fileot.
*  endloop.

  loop at isort.
     move-corresponding isort to isum.
     move sy-tabix to isum-POSNR.
     shift isum-POSNR right deleting trailing space.
     append isum. clear isum.
  endloop.

* Commented out since no summarizing was been done in
* Mercator Numbcus2 map.
*
*- summarize data
*  loop at isort.
*    read table isum with key iseqno = isort-iseqno
*                             ikndnr = isort-ikndnr
*                             iartnr = isort-iartnr
*                             ibukrs = isort-ibukrs
*                             ivkorg = isort-ivkorg
*                             iwerks = isort-iwerks
*                             iwwbrn = isort-iwwbrn
*                             iwwsct = isort-iwwsct
*                             iwwseg = isort-iwwseg
*                             iwwrat = isort-iwwrat
*                             iwwser = isort-iwwser
*                             ifadat = isort-ifadat.
*    if sy-subrc = 0.
*      add isort-vvcut  to isum-vvcut.
*      modify isum index sy-tabix transporting vvcut.
*      clear isum.
*    else.
*      move-corresponding isort to isum.
*      append isum. clear isum.
*    endif.
*  endloop.
endform.

*---------------------------------------------------------------------*
* form F_NUMBCUS1
*---------------------------------------------------------------------*
form F_NUMBCUS1.
  data: va_tabix like sy-tabix.
  refresh: isort, wa_err.
  loop at wa.
    move sy-tabix to va_tabix.
    clear zfb01.
    select single * from ZFB01 where GLCODE = wa-GLCL.
    if sy-subrc = 0.
      clear zfb02.
      select single * from ZFB02 where GLCODE   = wa-GLCL
                                   and B_SERVCL = wa-SCLS.
      if sy-subrc = 0.
        clear zfb03.
        select single * from ZFB03 where TOWNCODE = wa-GECA+0(2)
                                     and MUNICODE = wa-GECA+2(4).
        if sy-subrc = 0.
*         -------------------------------------
          PERFORM F_SINGLE_SETS using va_tabix.
*         -------------------------------------
        else.
          move-corresponding wa to wa_err.
          move
          'Town code & Municipality code not found in table ZFB03'
            to wa_err-errmsg.
          append wa_err. clear wa_err.
        endif.
      else.
        move-corresponding wa to wa_err.
        move
        'G/L Class code & Service Class code not found in table ZFB02'
          to wa_err-errmsg.
        append wa_err. clear wa_err.
      endif.
    else.
      move-corresponding wa to wa_err.
      move 'G/L Class code not found in table ZFB01' to wa_err-errmsg.
      append wa_err. clear wa_err.
    endif.
  endloop.

* Used for testing purposes only. Compare outfile to zbis103a.tmp
* output file from mercator map Numbcus1
*
*  loop at isort.
*    transfer isort to p_fileot.
*  endloop.

endform.

*---------------------------------------------------------------------*
* form F_SINGLE_SETS
*---------------------------------------------------------------------*
form F_SINGLE_SETS using Itabix.
  perform INIT_TABLE_RECORD.

* --------------------------------------
* SORTKEY GROUP
* --------------------------------------

* customer
  move '1'   to isort-IKNDNR.
  shift isort-IKNDNR right deleting trailing space.
  translate isort-IKNDNR using ' 0'.

* material number (ARTNR)
  if not zfb01-MATNR is initial.
    move zfb01-MATNR to isort-IARTNR.
  else.
    concatenate '*GLCC:' wa-GLCL '*' into isort-IARTNR.
  endif.

* company code
  move 'UGL' to isort-IBUKRS.

* sales organization (VKORG)
  if not zfb01-MATNR is initial.
    if zfb03-WERKS = 'S100'.
      if ( wa-SCLS = '21' or wa-SCLS = '31' ).
        move 'U015' to isort-IVKORG.
      else.
        move 'U010' to isort-IVKORG.
      endif.
    else.
      if ( wa-SCLS = '21' or wa-SCLS = '31' ).
        move 'C015' to isort-IVKORG.
      else.
        move 'C010' to isort-IVKORG.
      endif.
    endif.
  endif.

* plant
  if not zfb03-WERKS is initial.
    move zfb03-WERKS to isort-IWERKS.
  else.
    concatenate '*' wa-GECA(2) '*' into isort-IWERKS.
  endif.

* branch
  if not zfb03-WWBRN is initial.
    move zfb03-WWBRN to isort-IWWBRN.
  else.
    move wa-GECA(4) to isort-IWWBRN.
  endif.

* sector
  move 'Z'  to isort-IWWSCT.

* segment
  move 'ZZ' to isort-IWWSEG.

* rate class
  if not zfb02-RATECL is initial.
    move zfb02-RATECL to isort-IWWRAT.
  else.
    move wa-GLCL+2(2) to isort-IWWRAT.
  endif.

* service class
  if not zfb02-SERVCL is initial.
    move zfb02-SERVCL to isort-IWWSER.
  else.
    move wa-SCLS to isort-IWWSER.
  endif.

* transaction date
  concatenate wa-billdate+4(4) wa-billdate(2) wa-billdate+2(2) into
isort-IFADAT.

* -----------------------------------------------
* COMMON MOVES FOR ISORT
* Replace '/' with spaces to avoid RKEVEXT3 abend
* -----------------------------------------------
  move  '1'            to isort-VRGAR.

  move wa-billdate(2) to isort-PERDE.
  move isort-IFADAT    to isort-FADAT.
  move isort-IFADAT    to isort-BUDAT.
  move isort-IKNDNR    to isort-KNDNR.
  move isort-IARTNR    to isort-ARTNR.

  move 'CAD'           to isort-FRWAE.

  move isort-IBUKRS    to isort-BUKRS.
  move isort-IWERKS    to isort-WERKS.
  move isort-IVKORG    to isort-VKORG.
  move ' '             to isort-VTWEG.
  move ' '             to isort-WWDVN.
  move isort-IWWSCT    to isort-WWSCT.
  move isort-IWWSEG    to isort-WWSEG.
  move isort-IWWRAT    to isort-WWRAT.
  move isort-IWWSER    to isort-WWSER.

  move 'M3'            to isort-VVBVL_ME.
  move 'M3'            to isort-VVUVL_ME.
  move 'EA'            to isort-VVCUT_ME.

  move 0               to isort-VVBVL.
  translate isort-VVBVL using ' 0'.
  move 0               to isort-VVUVL.
  translate isort-VVUVL using ' 0'.
  move 0               to isort-VVBRV.
  translate isort-VVBRV using ' 0'.
  move 0               to isort-VVURV.
  translate isort-VVURV using ' 0'.
  move 0               to isort-VVORD.
  translate isort-VVORD using ' 0'.
  move 0               to isort-VVORI.
  translate isort-VVORI using ' 0'.

  move wa-GRPCNT       to isort-VVCUT.
  shift isort-VVCUT right deleting trailing space.
  translate isort-VVCUT using ' 0'.
  move 'BANN'          to isort-WWSID.
  move isort-IWWBRN    to isort-WWBRN.
  move ' '             to isort-WWMRT.
  move ' '             to isort-WWMSC.
  move ' '             to isort-WWLTL.
  move ' '             to isort-CRGRT.
  move ' '             to isort-LINFD.

  append isort. clear isort.

endform.

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - for internal table ISORT
*---------------------------------------------------------------------*
form init_table_record.
  data: vf_index type i value 1.
  while vf_index <= 51.
    ASSIGN COMPONENT vf_index OF STRUCTURE iSORT TO <F1>.
    <F1> = nodata.
    add 1 to vf_index.
  endwhile.
endform.

*-----------------------------------------------------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields and save to internal table itab.
*-----------------------------------------------------------------------
form F_MOVES.
  refresh wa.
  do.
    clear: wa, INREC.
    read dataset p_filein into INREC.
    if ( INREC is initial or sy-subrc <> 0 ).
      exit.
    else.
      move INREC to WA.
      append WA.
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
