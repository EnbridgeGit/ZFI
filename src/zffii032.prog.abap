REPORT  zffii032 MESSAGE-ID zs NO STANDARD PAGE HEADING
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
* 2007/09/10 - gymana - M1/M2 rate class split. Adding rate class to
*                       input file layout and removing all zfb02 rate
*                       class derivation coding.
* 2008/03/19 - gymana - TR547: Adding Premise to error report.
*
* 2011/05/27 - mkhan  - TR804: Cost of Gas
*                       Change logic for field IVKORG.
* 2011/05/30 - mkhan  - TR804 - Cost of Gas
*                       Replace table ZFB03 with ZFGECA and all
*                       references in this program. Table ZFGECA is
*                       exactly like ZFB03 but values are different.
* 2011/09/28 - sahmad - TR804 - Replace table ZFGECA with ZLSDBN002
*                       and all references in this program.
* 2012/04/18 - mkhan  - Q159 - Added new field (WWRSN - Order Reason)
*                       to structure ZZPA1100 and moved the value to
*                       this field from new table ZLSDBCYCDATE.
* 2012/08/21 - gymana - SDP23236 (Q159) Moved cycle 1 date logic from
*                       ZFFII031 to ZFFII032.  This logic changes the
*                       date of all cycle 1 recs when the cycle 1 bill
*                       date occurs late in the previous month.
*----------------------------------------------------------------------*

TABLES: zfb01,
        zfb02,
*        ZFGECA.
        zlsdbn002,
        zlsdbcycdate.

DATA: BEGIN OF wa OCCURS 0,
        premise(07)      TYPE c,  "Premise number
        glcl(04)         TYPE c,  "GL class code
        scls(02)         TYPE c,  "Service class code
        ratecls(04)      TYPE c,  "Rate class code
        geca(06)         TYPE c,  "Geographical area code
        billdate(08)     TYPE c,  "Banner billing date (MMDDYYYY)
        grpcnt(8)        TYPE c.  "Group count
DATA: END OF wa.

DATA: BEGIN OF wa_hdr,
        cyc01yyyy(04)    TYPE c,  "Billing Cycle 01 Year
        cyc01mm(02)      TYPE c,  "Billing Cycle 01 Month
        cyc01dd(02)      TYPE c,  "Billing Cycle 01 Day
        fa(1)            TYPE c,
        cyc20yyyy(04)    TYPE c,  "Billing Cycle 20 Year
        cyc20mm(02)      TYPE c,  "Billing Cycle 20 Month
        cyc20dd(02)      TYPE c.  "Billing Cycle 20 Day
DATA: END OF wa_hdr.

DATA: BEGIN OF wa_err OCCURS 0,
        premise(07)      TYPE c,  "Premise number
        glcl(04)         TYPE c,  "GL class code
        scls(02)         TYPE c,  "Service class code
        ratecls(02)      TYPE c,  "Rate class code
        geca(06)         TYPE c,  "Geographical area code
        billdate(08)     TYPE c,  "Banner billing date (MMDDYYYY)
        grpcnt(20)       TYPE c,  "Group count
        errmsg(60)       TYPE c.  "Error message
DATA: END OF wa_err.

DATA: BEGIN OF isort OCCURS 0,
        ikndnr    LIKE zzpa1100-kndnr,
        iartnr    LIKE zzpa1100-artnr,
        ibukrs    LIKE zzpa1100-bukrs,
        ivkorg    LIKE zzpa1100-vkorg,
        iwerks    LIKE zzpa1100-werks,
        iwwbrn    LIKE zzpa1100-wwbrn,
        iwwsct    LIKE zzpa1100-wwsct,
        iwwseg    LIKE zzpa1100-wwseg,
        iwwrat    LIKE zzpa1100-wwrat,
        iwwser    LIKE zzpa1100-wwser,
        iwwrsn    LIKE zzpa1100-wwrsn,                      "Q159
        ifadat    LIKE zzpa1100-fadat,
        iwwsld    LIKE zzpa1100-wwsld.
        INCLUDE STRUCTURE zzpa1100.
DATA: END OF isort.

DATA: isum LIKE zzpa1100 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF z_zzpa1100.
        INCLUDE STRUCTURE zzpa1100.
DATA: END OF z_zzpa1100.

DATA: inrec(400), outrec(2000), infile(70), outfile(70).
DATA: wrk_symbolic(4) TYPE c VALUE '$sys'.

DATA: wa_seqno      TYPE i,
      char(21)      TYPE c,
      nodata(1)     VALUE '/',
      cycle01       LIKE sy-datum(6),
      cycle20       LIKE sy-datum(6),
      transdt       LIKE sy-datum(6),
      transyr       LIKE sy-datum(4),
      banmthno      LIKE wa_hdr-cyc20mm,
      banyear       LIKE wa_hdr-cyc20yyyy,
      trandt_dd(02) TYPE c,
      trandt_mm(02) TYPE c,
      trandt_yr(04) TYPE c,
      w_billdate    LIKE sy-datum.

FIELD-SYMBOLS: <f1>.

*---------------------------------------------------------------------*
* selection screen
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK box0 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-100.

PARAMETERS: p_filein LIKE filename-fileextern OBLIGATORY DEFAULT
                     '/usr/sap/interfaces/P01/BANNER/bancount.dat',

            p_filehd LIKE filename-fileextern OBLIGATORY DEFAULT
                     '/usr/sap/interfaces/P01/BANNER/ccnthdr.dat',

            p_fileot LIKE filename-fileextern OBLIGATORY DEFAULT
                     '/usr/sap/interfaces/P01/BANNER/bancount.sap'.

SELECTION-SCREEN SKIP.
PARAMETERS: p_reclen(4) TYPE c OBLIGATORY DEFAULT '284'.

SELECTION-SCREEN END OF BLOCK box1.
SELECTION-SCREEN END OF BLOCK box0.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  REPLACE wrk_symbolic WITH sy-sysid INTO: p_filein, p_fileot.
  CONDENSE: p_filein NO-GAPS, p_fileot NO-GAPS.

*---------------------------------------------------------------------*
* start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM open_files.
  PERFORM f_moves.
  PERFORM f_extract_hdr_file.
  PERFORM f_numbcus1.
  PERFORM f_unique.
  PERFORM f_create_output_file.
  CLOSE DATASET: p_filein, p_fileot.

  IF NOT wa_err[] IS INITIAL.
    PERFORM f_report_error.
  ELSE.
    MESSAGE ID 'ZS' TYPE 'I' NUMBER '019' WITH
     'Mapping successfully completed. No errors found.'.
  ENDIF.

*---------------------------------------------------------------------*
* top of page
*---------------------------------------------------------------------*
TOP-OF-PAGE.

  FORMAT INTENSIFIED ON.
  WRITE: /1  'ZFFII032',
          40 'BANNER CUSTOMER COUNT ERROR REPORT'.
  WRITE: 100 'DATE:', sy-datum, '@', sy-uzeit.
  WRITE: /1 'IFFI056',
          116 'Page:', sy-pagno.
  FORMAT INTENSIFIED OFF.
  SKIP .

  WRITE: / sy-uline.

  WRITE: /001 'Premise',
          010 'G/L Class',
          021 'Serv. Cl',
          031 'Rate Cl',
          041 'Geo. Area',
          053 'Banner',
          065 'Banner',
          078 'Error'.

  WRITE: /001 'Number',
          010 'Code',
          021 'Code',
          031 'Code',
          041 'Code',
          053 'Bill Date',
          065 'Group Count',
          078 'Message'.

  WRITE: / sy-uline.
  FORMAT INTENSIFIED OFF.

*---------------------------------------------------------------------*
* report the errors
*---------------------------------------------------------------------*
FORM f_report_error.
  DATA: va_date(10) TYPE c.
  LOOP AT wa_err.
    CONCATENATE wa_err-billdate+4(4) '/' wa_err-billdate(2) '/'
      wa_err-billdate+2(2) INTO va_date.

    WRITE: /001 wa_err-premise,
            010 wa_err-glcl,
            021 wa_err-scls,
            031 wa_err-ratecls,
            041 wa_err-geca,
            053 va_date,
            065 wa_err-grpcnt,
            078 wa_err-errmsg.
  ENDLOOP.
ENDFORM.                    "F_REPORT_ERROR

*---------------------------------------------------------------------*
* create output file
*---------------------------------------------------------------------*
FORM f_create_output_file.
  DATA: switch TYPE i VALUE 0.
  LOOP AT isum.
    CLEAR z_zzpa1100.
    MOVE-CORRESPONDING isum TO z_zzpa1100.
    TRANSFER z_zzpa1100 TO p_fileot LENGTH p_reclen.
  ENDLOOP.
ENDFORM.                    "F_CREATE_OUTPUT_FILE

*---------------------------------------------------------------------*
* form F_UNIQUE
*---------------------------------------------------------------------*
FORM f_unique.
  REFRESH isum.
*- sort data
  SORT isort BY ikndnr iartnr ibukrs ivkorg iwerks
                 iwwbrn iwwsct iwwseg iwwrat iwwser iwwrsn ifadat. "Q159
*                iwwbrn iwwsct iwwseg iwwrat iwwser ifadat.        "Q159

* Used for testing purposes only. Compare outfile to zbis103b.srt
*
*  loop at isort.
*    transfer isort to p_fileot.
*  endloop.

  LOOP AT isort.
    MOVE-CORRESPONDING isort TO isum.
    MOVE sy-tabix TO isum-posnr.
    SHIFT isum-posnr RIGHT DELETING TRAILING space.
    APPEND isum. CLEAR isum.
  ENDLOOP.

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
ENDFORM.                    "F_UNIQUE

*---------------------------------------------------------------------*
* form F_NUMBCUS1
*---------------------------------------------------------------------*
FORM f_numbcus1.
  DATA: va_tabix LIKE sy-tabix.
  REFRESH: isort, wa_err.
  LOOP AT wa.
    MOVE sy-tabix TO va_tabix.
    CLEAR zfb01.
    SELECT SINGLE * FROM zfb01 WHERE glcode = wa-glcl.
    IF sy-subrc = 0.
      CLEAR zfb02.
      SELECT SINGLE * FROM zfb02 WHERE glcode   = wa-glcl
                                   AND b_servcl = wa-scls.
      IF sy-subrc = 0.
*       clear ZFGECA.
        CLEAR zlsdbn002.
*        select single * from ZFGECA where TOWNCODE = wa-GECA+0(2)
*                                     and MUNICODE = wa-GECA+2(4).
        SELECT SINGLE * FROM zlsdbn002 WHERE towncode = wa-geca+0(2)
                                         AND municode = wa-geca+2(4).
        IF sy-subrc = 0.
*         -------------------------------------
          PERFORM f_single_sets USING va_tabix.
*         -------------------------------------
        ELSE.
          MOVE-CORRESPONDING wa TO wa_err.
          MOVE
          'Town & Municipality code not found in table ZLSDBN002'
            TO wa_err-errmsg.
          APPEND wa_err. CLEAR wa_err.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING wa TO wa_err.
        MOVE
          'G/L Class & Service Class not found in table ZFB02'
          TO wa_err-errmsg.
        APPEND wa_err. CLEAR wa_err.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING wa TO wa_err.
      MOVE 'G/L Class code not found in table ZFB01' TO wa_err-errmsg.
      APPEND wa_err. CLEAR wa_err.
    ENDIF.
  ENDLOOP.

* Used for testing purposes only. Compare outfile to zbis103a.tmp
* output file from mercator map Numbcus1
*
*  loop at isort.
*    transfer isort to p_fileot.
*  endloop.

ENDFORM.                                                    "F_NUMBCUS1

*---------------------------------------------------------------------*
* form F_SINGLE_SETS
*---------------------------------------------------------------------*
FORM f_single_sets USING itabix.

  PERFORM init_table_record.

* --------------------------------------
* SORTKEY GROUP
* --------------------------------------

* customer
*START OF TR804 CHNAGES

  MOVE 'BANNER' TO isort-ikndnr.
*  move '1'   to isort-IKNDNR.
*  shift isort-IKNDNR right deleting trailing space.
*  translate isort-IKNDNR using ' 0'.

*END OF TR804 CHNAGES

* material number (ARTNR)
  IF NOT zfb01-matnr IS INITIAL.
    MOVE zfb01-matnr TO isort-iartnr.
  ELSE.
    CONCATENATE '*GLCC:' wa-glcl '*' INTO isort-iartnr.
  ENDIF.

* company code
  MOVE 'UGL' TO isort-ibukrs.

* sales organization (VKORG)
*START OF TR804 CHNAGES
  IF NOT zfb01-matnr IS INITIAL.
    MOVE 'Z001' TO isort-ivkorg.
*    if ZFGECA-WERKS = 'S100'.
*      if ( wa-SCLS = '21' or wa-SCLS = '31' ).
*        move 'U015' to isort-IVKORG.
*      else.
*        move 'U010' to isort-IVKORG.
*      endif.
*    else.
*      if ( wa-SCLS = '21' or wa-SCLS = '31' ).
*        move 'C015' to isort-IVKORG.
*      else.
*        move 'C010' to isort-IVKORG.
*      endif.
*    endif.
  ENDIF.

*END OF TR804 CHNAGES

* plant
  IF NOT zlsdbn002-werks IS INITIAL.
    MOVE zlsdbn002-werks TO isort-iwerks.
  ELSE.
    CONCATENATE '*' wa-geca(2) '*' INTO isort-iwerks.
  ENDIF.

* branch
*  if not ZFGECA-WWBRN is initial.
*    move ZFGECA-WWBRN to isort-IWWBRN.
  IF NOT zlsdbn002-vkbur IS INITIAL.
    MOVE zlsdbn002-vkbur TO isort-iwwbrn.
  ELSE.
    MOVE wa-geca(4) TO isort-iwwbrn.
  ENDIF.

*Sales District
  IF NOT zlsdbn002-bzirk IS INITIAL.
    MOVE zlsdbn002-bzirk TO isort-iwwsld.
  ELSE.
    MOVE wa-geca(4) TO isort-iwwsld.
  ENDIF.
* sector
  MOVE 'Z'  TO isort-iwwsct.

* segment
  MOVE 'ZZ' TO isort-iwwseg.

* rate class
  MOVE wa-ratecls TO isort-iwwrat.
*  if not zfb02-RATECL is initial.
*    move zfb02-RATECL to isort-IWWRAT.
*  else.
*    move wa-GLCL+2(2) to isort-IWWRAT.
*  endif.

* service class
  IF NOT zfb02-servcl IS INITIAL.
    MOVE zfb02-servcl TO isort-iwwser.
  ELSE.
    MOVE wa-scls TO isort-iwwser.
  ENDIF.

* transaction date
  CONCATENATE wa-billdate+4(4) wa-billdate(2) wa-billdate+2(2) INTO
              w_billdate.

*Start of Q159 changes
  SELECT SINGLE augru INTO zlsdbcycdate-augru
    FROM zlsdbcycdate
   WHERE zcydt = w_billdate.
  IF sy-subrc = 0.
    MOVE zlsdbcycdate-augru TO isort-iwwrsn.
  ELSE.
    MESSAGE e100(zm) WITH text-101.
    STOP.
  ENDIF.
*End   of Q159 changes

* Correct all cycle 1 transaction (Bill) dates if the it
* falls in the previous month.  If so, use the first day
* of the current month.

  CONCATENATE wa-billdate+4(4) wa-billdate(2) INTO transdt.

  MOVE wa-billdate+4(4) TO trandt_yr.
  MOVE wa-billdate(2) TO trandt_mm.
  MOVE wa-billdate+2(2) TO trandt_dd.

  IF cycle01 < cycle20.
    IF transdt < cycle20.
      MOVE wa_hdr-cyc20yyyy TO trandt_yr.
      MOVE wa_hdr-cyc20mm TO trandt_mm.
      MOVE '01' TO trandt_dd.
    ENDIF.
  ENDIF.

  CONCATENATE trandt_yr trandt_mm trandt_dd INTO w_billdate.

* -----------------------------------------------
* COMMON MOVES FOR ISORT
* Replace '/' with spaces to avoid RKEVEXT3 abend
* -----------------------------------------------
  MOVE  '1'            TO isort-vrgar.

  MOVE trandt_mm       TO isort-perde.
  MOVE w_billdate      TO isort-fadat.
  MOVE w_billdate      TO isort-budat.
  MOVE isort-ikndnr    TO isort-kndnr.
  MOVE isort-iartnr    TO isort-artnr.

  MOVE 'CAD'           TO isort-frwae.

  MOVE isort-ibukrs    TO isort-bukrs.
  MOVE isort-iwerks    TO isort-werks.
  MOVE isort-ivkorg    TO isort-vkorg.
  MOVE ' '             TO isort-vtweg.
  MOVE ' '             TO isort-wwdvn.
  MOVE isort-iwwsct    TO isort-wwsct.
  MOVE isort-iwwseg    TO isort-wwseg.
  MOVE isort-iwwrat    TO isort-wwrat.
  MOVE isort-iwwser    TO isort-wwser.
  MOVE isort-iwwrsn    TO isort-wwrsn.                      "Q159

  MOVE 'M3'            TO isort-vvbvl_me.
  MOVE 'M3'            TO isort-vvuvl_me.
  MOVE 'EA'            TO isort-vvcut_me.

  MOVE 0               TO isort-vvbvl.
  TRANSLATE isort-vvbvl USING ' 0'.
  MOVE 0               TO isort-vvuvl.
  TRANSLATE isort-vvuvl USING ' 0'.
  MOVE 0               TO isort-vvbrv.
  TRANSLATE isort-vvbrv USING ' 0'.
  MOVE 0               TO isort-vvurv.
  TRANSLATE isort-vvurv USING ' 0'.
  MOVE 0               TO isort-vvord.
  TRANSLATE isort-vvord USING ' 0'.
  MOVE 0               TO isort-vvori.
  TRANSLATE isort-vvori USING ' 0'.

  MOVE wa-grpcnt       TO isort-vvcut.
  SHIFT isort-vvcut RIGHT DELETING TRAILING space.
  TRANSLATE isort-vvcut USING ' 0'.
  MOVE 'BANN'          TO isort-wwsid.
  MOVE isort-iwwbrn    TO isort-wwbrn.
  MOVE ' '             TO isort-wwmrt.
  MOVE ' '             TO isort-wwmsc.
  MOVE ' '             TO isort-wwltl.
  MOVE ' '             TO isort-crgrt.
  MOVE ' '             TO isort-linfd.
  MOVE isort-iwwsld    TO isort-wwsld.

  APPEND isort. CLEAR isort.

ENDFORM.                    "F_SINGLE_SETS

*---------------------------------------------------------------------*
*  Used to initialize the record to '/' - for internal table ISORT
*---------------------------------------------------------------------*
FORM init_table_record.
  DATA: vf_index TYPE i VALUE 1.
  WHILE vf_index <= 51.
    ASSIGN COMPONENT vf_index OF STRUCTURE isort TO <f1>.
    <f1> = nodata.
    ADD 1 TO vf_index.
  ENDWHILE.
ENDFORM.                    "init_table_record

*-----------------------------------------------------------------------
*  This routine reads all the records from the input area, and adds
*  them, one-by-one, to the internal work table (wa), separating
*  the record into its various fields and save to internal table itab.
*-----------------------------------------------------------------------
FORM f_moves.
  REFRESH wa.
  DO.
    CLEAR: wa, inrec.
    READ DATASET p_filein INTO inrec.
    IF ( inrec IS INITIAL OR sy-subrc <> 0 ).
      EXIT.
    ELSE.
      MOVE inrec TO wa.
      APPEND wa.
    ENDIF.
  ENDDO.
ENDFORM.                    "F_MOVES

*-----------------------------------------------------------------------
*  This routine reads all the header data from the input area
*  and save to internal table wa_hdr.
*-----------------------------------------------------------------------
FORM f_extract_hdr_file.
*- extract header file
  CLEAR: wa_hdr, inrec.
  READ DATASET p_filehd INTO inrec.
  IF ( inrec IS INITIAL OR sy-subrc <> 0 ).
    MESSAGE e019 WITH infile 'Header file is empty!'.
    STOP.
  ELSE.
    MOVE inrec TO wa_hdr.
  ENDIF.
  MOVE wa_hdr+0(6) TO cycle01.
  MOVE wa_hdr+9(6) TO cycle20.
  MOVE wa_hdr-cyc20mm TO banmthno.
  MOVE wa_hdr-cyc20yyyy TO banyear.
ENDFORM.                    "F_EXTRACT_HDR_FILE

*-----------------------------------------------------------------------
*  Routine to open the physical file to determine if there are any
*  errors reading it.
*-----------------------------------------------------------------------
FORM open_files.
  DATA: msg(100).
*-----------------------------------------------------------------------
  OPEN DATASET p_filein FOR INPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH infile msg.
  ENDIF.
*-----------------------------------------------------------------------
  OPEN DATASET p_filehd FOR INPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH infile msg.
  ENDIF.
*-----------------------------------------------------------------------
  OPEN DATASET p_fileot FOR OUTPUT IN TEXT MODE MESSAGE msg.
  IF ( sy-subrc <> 0 ).
    MESSAGE e002 WITH outfile msg.
  ENDIF.
ENDFORM.                    "OPEN_FILES
