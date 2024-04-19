FUNCTION ZFI_DUPL_CHECK_WF.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BUKRS) LIKE  BKPF-BUKRS
*"     VALUE(I_XBLNR) LIKE  BKPF-XBLNR
*"     VALUE(I_WRBTR) LIKE  BSEG-WRBTR OPTIONAL
*"     VALUE(I_BELNR) LIKE  BSEG-BELNR OPTIONAL
*"     VALUE(I_GJAHR) LIKE  BSEG-GJAHR OPTIONAL
*"  EXPORTING
*"     VALUE(E_NOSTD) LIKE  BOOLE-BOOLE
*"     VALUE(E_POST) LIKE  BOOLE-BOOLE
*"----------------------------------------------------------------------

  DATA : git_bsip      TYPE STANDARD TABLE OF bsip,
         gwa_bsip      TYPE bsip.
  DATA: git_vbkpf TYPE STANDARD TABLE OF vbkpf,
        git_bkpf TYPE STANDARD TABLE OF bkpf,
        git_bseg TYPE STANDARD TABLE OF bseg,
        git_vbsegd TYPE STANDARD TABLE OF vbsegd,
        git_vbsegk TYPE STANDARD TABLE OF vbsegk,
        git_vbsegs TYPE STANDARD TABLE OF vbsegs.

  DATA : gv_dup_chkmsg TYPE string,
         gv_wrbtr(15)  TYPE c,
         gvcnt_res     TYPE i,
         gv1_bukrs     TYPE  bkpf-bukrs,
         gv1_lifnr     TYPE  bseg-lifnr,
         gv1_waers     TYPE  bkpf-waers,
         gv1_xblnr     TYPE  bkpf-xblnr,
         gv1_wrbtr     TYPE  bseg-wrbtr.

  DATA : gvstr_bukrs     TYPE  string,
         gvstr_lifnr     TYPE  string,
         gvstr_waers     TYPE  string,
         gvstr_xblnr     TYPE  string,
         gvstr_wrbtr     TYPE  string.

  TYPE-POOLS slis.

* Used for dispalying duplicates of reference and amount
  TYPES : BEGIN OF ty_display_duplicates,
           bukrs  TYPE bkpf-bukrs,                                 " Company Code
           belnr  TYPE bkpf-belnr,                                 " Document No
           fis_yr TYPE bkpf-gjahr,                                " Fiscal Year
          END OF ty_display_duplicates.

*Define work area
  DATA : gwa_bkpf                   TYPE bkpf,
         gwa_bseg                   TYPE bseg,
         gwa_vbsegk                 TYPE vbsegk,
         gwa_fieldcat_display       TYPE slis_fieldcat_alv,
         gwa_extab                  TYPE slis_extab,
         gwa_display_duplicates     TYPE ty_display_duplicates,
         gwa_param                  TYPE          spar.


  DATA: git_display_duplicates           TYPE STANDARD TABLE OF ty_display_duplicates,
        git_param                        TYPE TABLE OF spar,
        git_fieldcat_display             TYPE slis_t_fieldcat_alv,
        git_extab                        TYPE slis_t_extab,
        gv_rep                           TYPE sy-repid,
        gv_no_col                        TYPE i,
        gc_ans(1)                        TYPE c,
        gc_msg_length                    TYPE i,
        gc_capture_ucomm                 TYPE sy-ucomm,
        gc_table_lines                   TYPE i.


  CONSTANTS : co_3(1) TYPE c VALUE '3',
              co_bu(2) TYPE c VALUE 'BU',
              co_h(1)   TYPE c VALUE 'H',
              co_s(1) TYPE c VALUE 'S',
              co_park(2)             TYPE c VALUE 'BP',
              co_post(2)             TYPE c VALUE 'BU',
              co_stimulate(2)        TYPE c VALUE 'BS',
              co_save_as_complete(4) TYPE c VALUE 'PBBP',
              co_excel(7)            TYPE c VALUE '&VEXCEL',
              co_eta(4)              TYPE c VALUE '&ETA',
              co_oup(4)              TYPE c VALUE '&OUP',
              co_odn(4)              TYPE c VALUE '&ODN',
              co_sc(3)               TYPE c VALUE '%SC',
              co_sc_plus(4)          TYPE c VALUE '%SC+',
              co_ilt(4)              TYPE c VALUE '&ILT',
              co_rnt(4)              TYPE c VALUE '&RNT',
              co_olo(4)              TYPE c VALUE '&OL0',
              co_elp(4)              TYPE c VALUE '&ELP',
              co_lfo(4)              TYPE c VALUE '&LFO',
              co_crb(4)              TYPE c VALUE '&CRB',
              co_crl(4)              TYPE c VALUE '&CRL',
              co_crr(4)              TYPE c VALUE '&CRR',
              co_cre(4)              TYPE c VALUE '&CRE',
              co_ac1(4)              TYPE c VALUE '&AC1',
              co_no_1(1)             TYPE c VALUE '1',
              co_60(2)               TYPE c VALUE '60',
              co_pop1(9)             TYPE c VALUE 'LC_POPUP1',
              co_pop2(9)             TYPE c VALUE 'LC_POPUP2',
              co_pop3(9)             TYPE c VALUE 'LC_POPUP3',
              co_tcode_fbv0(4)       TYPE c VALUE 'FBV0',
              co_tcode_fv60(4)       TYPE c VALUE 'FV60',
              co_tcode_fv65(4)       TYPE c VALUE 'FV65',
              co_tcode_fb60(4)       TYPE c VALUE 'FB60',
              co_tcode_fb65(4)       TYPE c VALUE 'FB65'.

  REFRESH :   git_vbkpf,
              git_bkpf,
              git_bseg,
              git_vbsegk,
              git_display_duplicates,
              git_fieldcat_display,
              git_extab.


  CLEAR : gwa_bsip,
          git_bsip,
          git_bseg,
          gwa_bkpf,
          gwa_bseg,
          gwa_vbsegk,
          gwa_display_duplicates,
          gwa_fieldcat_display,
          gwa_extab,
          gv_rep,
          gc_capture_ucomm.

  DATA: W_LIFNR LIKE VBSEGK-LIFNR,
        W_AWKEY LIKE VBKPF-AWKEY,
        W_REPRF LIKE LFB1-REPRF.

  SELECT SINGLE LIFNR FROM VBSEGK INTO W_LIFNR
    WHERE AUSBK = I_BUKRS AND
          BELNR = I_BELNR AND
          GJAHR = I_GJAHR AND
          LIFNR NE ' '.
  IF SY-SUBRC = 0.
    SELECT SINGLE REPRF FROM LFB1 INTO W_REPRF
      WHERE LIFNR = W_LIFNR AND
            BUKRS = I_BUKRS.
  ELSE.
    CONCATENATE I_BELNR i_gjahr INTO W_AWKEY.
    CONDENSE W_AWKEY NO-GAPS.
    SELECT SINGLE AUSBK BELNR GJAHR FROM VBKPF INTO (i_bukrs, I_BELNR, I_GJAHR)
      WHERE AWKEY = W_AWKEY.
    IF SY-SUBRC = 0.
      SELECT SINGLE LIFNR FROM VBSEGK INTO W_LIFNR
        WHERE AUSBK = I_BUKRS AND
              BELNR = I_BELNR AND
              GJAHR = I_GJAHR AND
              LIFNR NE ' '.
      IF SY-SUBRC = 0.
        SELECT SINGLE REPRF FROM LFB1 INTO W_REPRF
          WHERE LIFNR = W_LIFNR AND
                BUKRS = I_BUKRS.
      ENDIF.
    ENDIF.
  ENDIF.

  IF  i_xblnr IS NOT INITIAL AND W_REPRF IS NOT INITIAL.
    SELECT * FROM  bkpf
             INTO TABLE git_bkpf
             WHERE xblnr = i_xblnr .

    IF git_bkpf[] IS NOT INITIAL.


      SELECT * FROM bseg
                INTO TABLE git_bseg
                FOR ALL ENTRIES IN git_bkpf
                WHERE bukrs = git_bkpf-bukrs
                AND belnr = git_bkpf-belnr
                AND gjahr = git_bkpf-gjahr
                AND wrbtr = i_wrbtr
                AND shkzg = co_h.

      IF sy-subrc = 0 AND git_bseg[] IS NOT INITIAL.

        CLEAR : gwa_bsip, gv_dup_chkmsg,
        gvstr_wrbtr, gvstr_xblnr.

        MOVE i_wrbtr TO gv_wrbtr.
        CONDENSE gv_wrbtr.
        CONCATENATE text-005 gv_wrbtr                       "gv1_waers
        INTO gvstr_wrbtr
        SEPARATED BY space.

        CONCATENATE text-004 i_xblnr
        INTO gvstr_xblnr
        SEPARATED BY space.

* Logic to display warning message along with document no and company code
        CLEAR : gwa_bkpf,
                gwa_bseg.
        REFRESH : git_display_duplicates .

        LOOP AT git_bkpf INTO  gwa_bkpf
                         WHERE xblnr = i_xblnr.
          READ TABLE git_bseg INTO     gwa_bseg
                              WITH KEY bukrs  = gwa_bkpf-bukrs
                                       belnr  = gwa_bkpf-belnr
                                       gjahr  = gwa_bkpf-gjahr
                                       wrbtr  = i_wrbtr.

          IF sy-subrc = 0."              AND

            CLEAR: gwa_display_duplicates.

            gwa_display_duplicates-bukrs      = gwa_bseg-bukrs.
            gwa_display_duplicates-belnr      = gwa_bseg-belnr.
            gwa_display_duplicates-fis_yr     = gwa_bseg-gjahr.
            APPEND gwa_display_duplicates TO git_display_duplicates.

          ENDIF.
        ENDLOOP.

        IF gwa_display_duplicates-belnr IS NOT INITIAL.
          CONCATENATE text-008                            " Duplicate invoice found with same
                      gvstr_xblnr
                      gvstr_wrbtr
                      text-009                            " under company code
                      gwa_display_duplicates-bukrs
                      text-010                            " document number
                      gwa_display_duplicates-belnr
                 INTO gv_dup_chkmsg   SEPARATED BY space.
          E_NOSTD = 'X'.
        ENDIF.
        CLEAR : gwa_display_duplicates.

        MESSAGE gv_dup_chkmsg TYPE 'I' .

        REFRESH : git_fieldcat_display.
        CLEAR : gv_no_col.
        IF git_display_duplicates[] IS NOT INITIAL AND sy-batch NE 'X'.
          gv_no_col = co_no_1.

          gwa_fieldcat_display-fieldname   = text-011.            " BELNR
          gwa_fieldcat_display-row_pos     = co_no_1.
          gwa_fieldcat_display-col_pos     = gv_no_col.
          gwa_fieldcat_display-seltext_l   = text-014.            " Document No
          gwa_fieldcat_display-tabname     = text-017.            " GIT_DISPLAY_DUPLICATES

          APPEND  gwa_fieldcat_display TO git_fieldcat_display.
          CLEAR gwa_fieldcat_display .

          gv_no_col = gv_no_col + co_no_1.

          gwa_fieldcat_display-fieldname   = text-012.            " BUKRS
          gwa_fieldcat_display-row_pos     = co_no_1.
          gwa_fieldcat_display-col_pos     = gv_no_col.
          gwa_fieldcat_display-seltext_l   = text-015.            " Company Code
          gwa_fieldcat_display-tabname     = text-017.            " GIT_DISPLAY_DUPLICATES

          APPEND  gwa_fieldcat_display TO git_fieldcat_display.
          CLEAR gwa_fieldcat_display .

          gv_no_col = gv_no_col + co_no_1.

          gwa_fieldcat_display-fieldname   = text-013.           " FIS_YR
          gwa_fieldcat_display-row_pos     = co_no_1.
          gwa_fieldcat_display-col_pos     = gv_no_col.
          gwa_fieldcat_display-seltext_l   = text-016.           " Fiscal Year
          gwa_fieldcat_display-tabname     = text-017.           " GIT_DISPLAY_DUPLICATES

          APPEND  gwa_fieldcat_display TO git_fieldcat_display.
          CLEAR   gwa_fieldcat_display .

          CLEAR : gv_no_col.
*Assigning report Name
          gv_rep = sy-repid .

          CLEAR gwa_extab.
          gwa_extab-fcode = co_excel."'&VEXCEL'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_eta."'&ETA'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_oup."'&OUP'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_odn."'&ODN'.
          APPEND  gwa_extab TO git_extab.
          CLEAR gwa_extab.
          gwa_extab-fcode = co_sc."'%SC'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_sc_plus."'%SC+'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_ilt."'&ILT'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_rnt."'&RNT'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_olo.                         "'&OL0'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_elp."'&ELP'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_lfo."'&LFO'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_crb."'&CRB'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_crl."'&CRL'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_crr."'&CRR'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_cre."'&CRE'.
          APPEND  gwa_extab TO git_extab.

          CLEAR gwa_extab.
          gwa_extab-fcode = co_ac1.                         "'&AC1'.
          APPEND  gwa_extab TO git_extab.

          CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
            EXPORTING
              i_title               = text-018                     " Duplication of same Reference No and Amount
              i_selection           = ''
              i_screen_start_column = '20'
              i_screen_start_line   = '10'
              i_screen_end_column   = '80'
              i_screen_end_line     = '20'
              i_tabname             = text-017                     " GIT_DISPLAY_DUPLICATES
              it_fieldcat           = git_fieldcat_display
              it_excluding          = git_extab
              i_callback_program    = gv_rep
            TABLES
              t_outtab              = git_display_duplicates
            EXCEPTIONS
              program_error         = 1
              OTHERS                = 2.
          IF sy-subrc = 0.
            sy-ucomm = gc_capture_ucomm.
          ENDIF.
        ENDIF.
*        ENDIF.    " IF git_vbkpf IS NOT INITIAL.
        " Check for Parked Documents "
      ELSE.
        SELECT * FROM vbsegk
        INTO TABLE git_vbsegk
        FOR ALL ENTRIES IN git_bkpf
        WHERE bukrs = git_bkpf-bukrs
        AND belnr = git_bkpf-belnr
        AND gjahr = git_bkpf-gjahr
        AND wrbtr = i_wrbtr
        AND ( shkzg = co_h OR
              shkzg = co_s ).  " Added SHKZG = 'S' so that parking document can be checked for duplication.

        IF sy-subrc = 0 AND git_vbsegk[] IS NOT INITIAL.

          DELETE git_vbsegk WHERE bukrs = i_bukrs
                            AND   belnr = i_belnr
                            AND   gjahr = i_gjahr
                            AND   wrbtr = i_wrbtr.

          IF git_vbsegk[] IS NOT INITIAL.

            CLEAR : gwa_bsip, gv_dup_chkmsg,
            gvstr_wrbtr, gvstr_xblnr.

            MOVE i_wrbtr TO gv_wrbtr.
            CONDENSE gv_wrbtr.
            CONCATENATE text-005 gv_wrbtr                   "gv1_waers
            INTO gvstr_wrbtr
            SEPARATED BY space.

            CONCATENATE text-004 i_xblnr
            INTO gvstr_xblnr
            SEPARATED BY space.

* Logic to display warning message along with document no and company code
            CLEAR : gwa_bkpf,
                    gwa_vbsegk.

            REFRESH : git_display_duplicates .

            LOOP AT git_bkpf INTO  gwa_bkpf
                             WHERE xblnr = i_xblnr.
              READ TABLE git_vbsegk INTO   gwa_vbsegk
                                  WITH KEY belnr  = gwa_bkpf-belnr
                                           gjahr  = gwa_bkpf-gjahr
                                           bukrs  = gwa_bkpf-bukrs
                                           wrbtr  = i_wrbtr.
              IF sy-subrc = 0  ."              AND
*                   gwa_vbsegk-bukrs NE i_bukrs AND
*                   gwa_vbsegk-belnr NE i_belnr AND
*                   gwa_vbsegk-gjahr NE i_gjahr .

                CLEAR : gwa_display_duplicates.

                gwa_display_duplicates-bukrs      = gwa_vbsegk-bukrs.
                gwa_display_duplicates-belnr      = gwa_vbsegk-belnr.
                gwa_display_duplicates-fis_yr     = gwa_vbsegk-gjahr.
                APPEND gwa_display_duplicates TO git_display_duplicates.
              ENDIF.

            ENDLOOP.

            IF gwa_display_duplicates-belnr IS NOT INITIAL.
              CONCATENATE text-008                            " Duplicate invoice found with same
                          gvstr_xblnr
                          gvstr_wrbtr
                          text-009                            " under company code
                          gwa_display_duplicates-bukrs
                          text-010                            " document number
                          gwa_display_duplicates-belnr
                     INTO gv_dup_chkmsg   SEPARATED BY space.
              E_NOSTD = 'X'.
            ENDIF.
            CLEAR : gwa_display_duplicates.

            MESSAGE gv_dup_chkmsg TYPE 'I'.

            REFRESH : git_fieldcat_display.
            CLEAR : gv_no_col.

            IF git_display_duplicates[] IS NOT INITIAL AND sy-batch NE 'X'.
              gv_no_col = co_no_1.

              gwa_fieldcat_display-fieldname   = text-011.          " BELNR
              gwa_fieldcat_display-row_pos     = co_no_1.
              gwa_fieldcat_display-col_pos     = gv_no_col.
              gwa_fieldcat_display-seltext_l   = text-014.          " Document No
              gwa_fieldcat_display-tabname     = text-017.          " GIT_DISPLAY_DUPLICATES

              APPEND  gwa_fieldcat_display TO git_fieldcat_display.
              CLEAR gwa_fieldcat_display .

              gv_no_col = gv_no_col + co_no_1.

              gwa_fieldcat_display-fieldname   = text-012.         " BUKRS
              gwa_fieldcat_display-row_pos     = co_no_1.
              gwa_fieldcat_display-col_pos     = gv_no_col.
              gwa_fieldcat_display-seltext_l   = text-015.         " Company Code
              gwa_fieldcat_display-tabname     = text-017.         " GIT_DISPLAY_DUPLICATES

              APPEND  gwa_fieldcat_display TO git_fieldcat_display.
              CLEAR gwa_fieldcat_display .

              gv_no_col = gv_no_col + co_no_1.

              gwa_fieldcat_display-fieldname   = text-013.          " FIS_YR
              gwa_fieldcat_display-row_pos     = co_no_1.
              gwa_fieldcat_display-col_pos     = gv_no_col.
              gwa_fieldcat_display-seltext_l   = text-016.          " Fiscal Year
              gwa_fieldcat_display-tabname     = text-017.          " GIT_DISPLAY_DUPLICATES

              APPEND  gwa_fieldcat_display TO git_fieldcat_display.
              CLEAR   gwa_fieldcat_display .

              CLEAR : gv_no_col.
*Assigning report Name
              gv_rep = sy-repid .

              CLEAR gwa_extab.
              gwa_extab-fcode = co_excel."'&VEXCEL'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_eta."'&ETA'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_oup."'&OUP'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_odn."'&ODN'.
              APPEND  gwa_extab TO git_extab.
              CLEAR gwa_extab.
              gwa_extab-fcode = co_sc."'%SC'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_sc_plus."'%SC+'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_ilt."'&ILT'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_rnt."'&RNT'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_olo.                     "'&OL0'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_elp."'&ELP'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_lfo."'&LFO'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_crb."'&CRB'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_crl."'&CRL'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_crr."'&CRR'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_cre."'&CRE'.
              APPEND  gwa_extab TO git_extab.

              CLEAR gwa_extab.
              gwa_extab-fcode = co_ac1.                     "'&AC1'.
              APPEND  gwa_extab TO git_extab.

              CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
                EXPORTING
                  i_title               = text-018                 " Duplication of same Reference No and Amount
                  i_selection           = ''
                  i_screen_start_column = '20'
                  i_screen_start_line   = '10'
                  i_screen_end_column   = '80'
                  i_screen_end_line     = '20'
                  i_tabname             = text-017                  " GIT_DISPLAY_DUPLICATES
                  it_fieldcat           = git_fieldcat_display
                  it_excluding          = git_extab
                  i_callback_program    = gv_rep
                TABLES
                  t_outtab              = git_display_duplicates
                EXCEPTIONS
                  program_error         = 1
                  OTHERS                = 2.

            ENDIF.
          ENDIF. " IF git_vbsegk[] IS NOT INITIAL.
        ENDIF.
      ENDIF.
    ENDIF.    "if git_vbkpf [] is not initial.
  ENDIF.

  IF E_NOSTD = 'X'.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Duplication exists'(019)
        text_question         = 'Post the Invoice ?'(021)
        text_button_1         = 'Yes'(022)
        icon_button_1         = 'ICON_OKAY'(023)
        text_button_2         = 'No'(024)
        icon_button_2         = 'ICON_CANCEL'(025)
        default_button        = '2'
        display_cancel_button = ''
      IMPORTING
        answer                = gc_ans
*    TABLES
*      parameter             = git_param
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
*
    IF sy-subrc = 0 AND gc_ans = '1'.
      E_POST = 'X'.
    ENDIF.
*    IF sy-subrc = 0 AND gc_ans = '2'.
*      MESSAGE i001(zfi_workflow) WITH 'Please return to workitem and reject the document'.
*    ENDIF.
  ELSE.
    E_POST = 'X'.
  ENDIF.



ENDFUNCTION.
