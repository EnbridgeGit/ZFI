REPORT ZFTVI001_P2C_TV_001 NO STANDARD PAGE HEADING LINE-SIZE 132.

TABLES: pcl1, sscrfields.

INCLUDE zftvi001_rprccd00.
*INCLUDE rprccd00.
INCLUDE zftvi001_rpc1ta00.
*INCLUDE rpc1ta00.

******************** Selection-screen Steuerung   ********************
INCLUDE zftvi001_read_kr1025_s01.
*INCLUDE RPRCCC_READ_KR1025_S01.

********************    Typen + interne Daten    *********************
TYPE-POOLS: slis.

INCLUDE zftvi001_read_kr1025_t01.
*INCLUDE RPRCCC_READ_KR1025_T01.

DATA:  t_info_sap_asc TYPE type_info_sap_asc OCCURS 0 WITH HEADER LINE,
       t_info_ax_asc  TYPE type_info_ax_asc  OCCURS 0 WITH HEADER LINE,
       t_errors       TYPE type_info_ax_asc  OCCURS 0 WITH HEADER LINE,
       header_sap     LIKE ccheader,
       header_ax      TYPE type_header_ax,
       info_sap       TYPE type_main_sap,
       info_ax        TYPE type_info_ax.

DATA: a_protokoll     TYPE type_protokoll,
      t_protokoll     TYPE type_t_protokoll.

DATA: t_tcurc         LIKE tcurc OCCURS 0 WITH HEADER LINE,
*      t_tcurx         like tcurx occurs 0 with header line,
      t_t005          LIKE t005  OCCURS 0 WITH HEADER LINE.

* Begin of SSN_Nb
DATA:  t_ssn          TYPE type_ssn         OCCURS 0 WITH HEADER LINE,
       t_pernr        TYPE type_pernr       OCCURS 0 WITH HEADER LINE,
* Begin of SSNDate
       t_mult_pernr   TYPE type_pernr       OCCURS 0 WITH HEADER LINE,
* End of SSNDate
       t_concurr_ssn  TYPE type_concurr_ssn OCCURS 0 WITH HEADER LINE.
* End of SSN_Nb

DATA: text_id(8).
DATA: color_error_1(3) TYPE c,
      color_error_2(3) TYPE c.
DATA: write_status     TYPE i.
DATA: conv_ok          TYPE i,
      conv_ko          TYPE i.
DATA: op_type          TYPE c.
DATA: two_currencies     TYPE c,
      wrong_customer   TYPE c,
      wrong_footer     TYPE c,
      wrong_head_curr  TYPE c,
      wrong_run        TYPE c,  "Run
      entry_e_text(78) TYPE c.
DATA: error_by_ccc     TYPE c.
DATA: ccc_sum          TYPE type_ccc_sum.
DATA: sum_purchase     TYPE p,
      sum_addcharge    TYPE p.
DATA: mwst_fees        TYPE p DECIMALS 3.
data: w_waers like TCURC-WAERS.

* Begin of SSN_Nb
DATA: v_ssn_cnt        TYPE i,
      v_concurr_cnt    TYPE i.
* End of SSN_Nb

***********************    Macro Definition    ***********************
DEFINE do_protokoll.
  clear a_protokoll.
  conv_ko = conv_ko + 1.
  if rb_all = 'X'.
    color_error_1 = 'C60'. color_error_2 = 'C61'.
  else.
    color_error_1 = 'C40'. color_error_2 = 'C41'.
  endif.
  a_protokoll-conv_status = &1.
  a_protokoll-index_dataset = index_dataset.
  a_protokoll-data = info_ax.
  a_protokoll-conv_data = info_sap.
  a_protokoll-fehler_wert = &2.
  if last_color = color_error_1.
    a_protokoll-color = last_color = color_error_2.
  else.
    a_protokoll-color = last_color = color_error_1.
  endif.
  concatenate 'TEXT-J' &1 into text_id.
  write (text_id) to a_protokoll-fehler_text.
  append a_protokoll to t_protokoll.
  append t_info_ax_asc to t_errors.
  clear field_status.
END-OF-DEFINITION.

************************    Other Includes   *************************
INCLUDE zftvi001_read_kr1025_f01.
*INCLUDE RPRCCC_READ_KR1025_F01.
INCLUDE zftvi001_read_kr1025_f02.
*INCLUDE RPRCCC_READ_KR1025_F02.



**********************   BEGIN OF MAIN PROGRAM   *********************
START-OF-SELECTION.
  PERFORM check_entry USING entry_e_text.
  IF NOT entry_e_text IS INITIAL.
    subrc = 1.
    WRITE:/ entry_e_text.
    EXIT.
  ENDIF.
  CHECK subrc = 0.
  PERFORM initialization.

* Comment out by sahmad
*  PERFORM f_select_xparam.
* end of comment by sahmad

  PERFORM read_dataset USING op_type
                             subrc
                             msg.
  IF NOT subrc IS INITIAL.
    PERFORM write_err_msg USING op_type subrc.
  ENDIF.
  CHECK subrc = 0.
* Table T_Info_Amex_Asc is filled.
  PERFORM set_sap_fix_header.
  PERFORM load_all_ssn.              "SSN_Nb

  PERFORM f_load_z_tables.

  PERFORM f_map_transaction_data.



  IF wrong_run = 'X'.
    subrc = 1.
    WRITE:/ text-e21.
    EXIT.
  ENDIF.
* Replaces
*  perform konvert_ax_sap_ccc using 2_currencies wrong_customer
*                                                wrong_head_curr.
* End of Run
  IF wrong_footer = 'X'.
    subrc = 1.
    WRITE:/ text-e14.
    EXIT.
  ENDIF.
  IF wrong_customer = 'X'.
    subrc = 1.
    WRITE:/ text-e13.
    EXIT.
  ENDIF.
  IF header_sap-compcode IS INITIAL.
    subrc = 1.
    WRITE:/ text-e14.
    EXIT.
  ENDIF.
  IF two_currencies = 'X'.
    subrc = 1.
    WRITE:/ text-e12.
    EXIT.
  ENDIF.
  IF wrong_head_curr = 'X'.
    subrc = 1.
    WRITE:/ text-e17.
    EXIT.
  ENDIF.
  PERFORM write_sum_line.
*  move header_sap to t_info_sap_asc.   "#EC ENHOK          "QSCK005115
  MOVE header_sap(44) TO t_info_sap_asc.   "#EC ENHOK       "QSCK005115
  MOVE header_sap+46 TO t_info_sap_asc+44. "#EC ENHOK       "QSCK005115
  INSERT t_info_sap_asc INDEX 1.
  IF testlauf = ' '.
    PERFORM speichern_output USING op_type
                                   subrc
                                   msg.
    IF subrc IS INITIAL.
      PERFORM save_errors USING  op_type
                                 subrc
                                 msg.

*Comment by SAHMAD
*      PERFORM f_archive_file.
* end of comment by sahmad

    ENDIF.
    IF ( subrc NE 0 ).
      WRITE:/ text-e09.
      WRITE:/ msg.
    ENDIF.
  ENDIF.
*  IF d_import = 'X' AND subrc = 0.
*    PERFORM credit_card_clearing USING error_by_ccc.
*  ENDIF.
  IF p_prot = 'X'.
    write_status = subrc.
    CLEAR subrc.
    PERFORM display_protokoll USING error_by_ccc.
  ENDIF.


************************************************************************

TOP-OF-PAGE.
* Top of Page:
*                   Titel und Laufdatum
*                   Kreditkartenfirma
*                   Abrechnungsdatum
*                   Laufende Nummer der Abrechnung
*                   Richtige und fehlerhafte Einträge
*                   Test / Speicherung von Datei
  ULINE.
  FORMAT COLOR 1 INTENSIFIED.
  WRITE:/ sy-title CENTERED,
          70 sy-datum DD/MM/YYYY.
  ULINE.
  CHECK subrc = 0.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: /1 sy-vline,
         3 text-d01,
         30 header_sap-cardcomp,
         132 sy-vline,
         /1 sy-vline,
         3 text-d02,
         30 header_sap-abrdatum,
         132 sy-vline,
         /1 sy-vline,
         3 text-d03,
         30 header_sap-lfdnum,
         132 sy-vline.
  ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: /1 sy-vline,
         3 text-d05,
         40 conv_ok,
         70 text-d06,
         95 conv_ko,
         132 sy-vline.
  ULINE.
  IF testlauf EQ 'X'.
    IF write_status = 0.
      WRITE:/  sy-vline NO-GAP,
               text-t03 CENTERED,
           132 sy-vline NO-GAP.
    ELSE.
      FORMAT COLOR 6 INTENSIFIED OFF.
      WRITE:/  sy-vline NO-GAP,
               text-t02 CENTERED,
           132 sy-vline NO-GAP.
    ENDIF.
    ULINE.
  ENDIF.

  FORMAT RESET.
  SKIP 1.



************   Begin of Event AT SELECTION-SCREEN OUTPUT   ************
* Activate / Deactivate fields for file save according to test mode.
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name CS 'BS03' OR screen-name CS 'FIL_'.
      IF testlauf = 'X'.
        screen-invisible = '1'.
        screen-active = '0'.
        MODIFY SCREEN.
      ELSE.
        screen-invisible = '0'.
        screen-active = 'X'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.



************************    End of main code    ***********************

*********************    Begin of formroutines    *********************



*&---------------------------------------------------------------------*
*&      Form  Initialization
*&---------------------------------------------------------------------*
*       Initialisierung der Variablen                                  *
*----------------------------------------------------------------------*
FORM initialization.

* Initialisierung der internen Tabellen :
  CLEAR:   dataset,t_errors,
           errors, buffer, trips, t_info_ax_asc, t_info_sap_asc,
           conv_ok, conv_ko, write_status, op_type, header_sap-abrwaehr,
* Begin of Run
           two_currencies, wrong_customer, wrong_run, error_by_ccc.
*           2_currencies, wrong_customer, error_by_ccc.
* End of Run

  REFRESH: dataset, t_errors,
           errors, buffer, trips, t_info_ax_asc, t_info_sap_asc.

  SELECT * FROM tcurc INTO TABLE t_tcurc.               "#EC CI_GENBUFF
  SELECT * FROM t005  INTO TABLE t_t005.

ENDFORM.                               " INIT_CLEAR
*-------------------   End of form INITIALIZATION   -------------------





***********************************************************************
*                        Processing Formroutines                      *
***********************************************************************


*&---------------------------------------------------------------------*
*&      Form  Konvert_AX_SAP_CCC
*&---------------------------------------------------------------------*
* Purpose: converts one complete position from the AX format into the
*          SAP format.
*----------------------------------------------------------------------*
* Begin of Run
FORM konvert_ax_sap_ccc USING p_invalid_file    TYPE c
                              p_wrong_customer  TYPE c
                              p_wrong_footer    TYPE c
                              p_wrong_head_curr TYPE c
                              p_wrong_run       TYPE c.
* Replaces
*form konvert_ax_sap_ccc using p_invalid_file    type c
*                              p_wrong_customer  type c
*                              p_wrong_head_curr type c.
* End of Run.

  DATA: index_dataset TYPE i,
        field_status  TYPE i,
        last_color(3) TYPE c,
        exp           TYPE i,
        diff_round    LIKE info_sap-umstotal,
        stat_head     TYPE i,
        header_curr   LIKE info_ax-billed_curr.

  DATA: prev_pernr LIKE pernr-pernr. "SSNDate

  CLEAR: header_ax, info_ax, info_sap, index_dataset, last_color,
         two_currencies, stat_head, sum_purchase,
         sum_addcharge, header_curr.
  REFRESH: t_info_sap_asc.


*note 1381537
*checking footer information
  p_wrong_footer = 'X'.
  LOOP AT t_info_ax_asc.
    IF t_info_ax_asc(1) = '9'.
* *   Trailer (Type 9): File complete.
      p_wrong_footer = space.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF p_wrong_footer = 'X'.
    RETURN.
  ENDIF.

* Für alle Einträge in der AX Datei.
  LOOP AT t_info_ax_asc.
    index_dataset = index_dataset + 1.
* * Wenn Header (Type 0 Records): Header Info speichern.
    IF t_info_ax_asc(1) = '0'.
      MOVE t_info_ax_asc TO header_ax.
      IF header_ax-rec_nb NE firmennr.
        p_wrong_customer = 'X'.
        EXIT.
      ENDIF.
* Begin of Run.
      ta_key-ccomp = 'AX'.
      rp_imp_c1_ta.
      IF run_nb IS INITIAL.
        CONCATENATE header_ax-rep_create_date+2(6)
                    header_ax-rep_version_date+4(4) INTO run_nb.
      ENDIF.
      READ TABLE ccdat WITH KEY abnum = run_nb.
      IF sy-subrc = 0.
        p_wrong_run = 'X'.
        EXIT.
      ENDIF.
* End of Run.
      header_sap-compcode = header_ax-rec_nb.
      header_sap-abrdatum = header_ax-rep_create_date.
* Begin of Run
      WRITE run_nb TO header_sap-lfdnum RIGHT-JUSTIFIED.
* Replaces
*      write header_ax-file_version_nb to header_sap-lfdnum
*                                                  right-justified.
* End of Run
      OVERLAY header_sap-lfdnum WITH '0000000000'.
* * Wenn Einzelbeleg (Type 1 Records):
    ELSEIF t_info_ax_asc(1) = '1'.
      CLEAR: info_sap, info_ax, a_protokoll.
      MOVE t_info_ax_asc TO info_ax.
      IF info_ax-billed_curr NE header_curr AND
         ( NOT header_curr IS INITIAL ).
        p_invalid_file = 'X'.
        EXIT.
      ELSE.
        header_curr = info_ax-billed_curr.
      ENDIF.
* * * F1, LINETYPE
      info_sap-linetype = 'M'.
* * * F5, CARDFLAG
* * * * Konvertieren
      PERFORM convert_card_type USING info_ax-account_type
                                      info_sap-cardflag
                                      field_status.
      IF field_status NE 0.
        do_protokoll '04' info_ax-account_type.
        CONTINUE.
      ENDIF.
* * * F2, PERNR.
* Begin of SSN_Nb
      IF ssn_proc = 'X'.
*-----If the SSN field is not empty, retrieve employee# from t_pernr.
*-----Otherwise, set blank PERNR.
        IF NOT info_ax-social_sec_nb IS INITIAL.
          READ TABLE t_pernr WITH KEY perid = info_ax-social_sec_nb+1(9)
          BINARY SEARCH.
          IF sy-subrc = 0.
            info_sap-pernr = t_pernr-pernr.
          ELSE.
* Begin of SSNDate
            CLEAR prev_pernr.
            LOOP AT t_mult_pernr
                    WHERE perid = info_ax-social_sec_nb+1(9)
                      AND begda LE info_ax-charge_date
                      AND endda GE info_ax-charge_date.
              IF t_mult_pernr-pernr NE prev_pernr AND
                 prev_pernr NE '00000000'.
                info_sap-pernr = '00000000'.
                EXIT.
              ELSE.
                info_sap-pernr = prev_pernr = t_mult_pernr-pernr.
              ENDIF.
            ENDLOOP.
* Replaces
*            Info_SAP-PerNr = '00000000'.
* End of SSNDate.
          ENDIF.
        ELSE.
          info_sap-pernr = '00000000'.
        ENDIF.
      ELSE.
* End of SSN_Nb.
        IF ( info_ax-pernr IS INITIAL ) OR ( info_ax-pernr CO '0' ).
          info_sap-pernr = '00000000'.
        ELSE.
          info_sap-pernr = info_ax-pernr+2(8).
        ENDIF.
      ENDIF.   " SSN_Nb
* * * F3, PNAME
      WRITE info_ax-card_name TO info_sap-pname LEFT-JUSTIFIED.
* * * F4, CARDNUM
* * * * Mussfeld
      WRITE info_ax-bill_acc_nb TO info_sap-cardnum LEFT-JUSTIFIED.
      IF info_sap-cardnum IS INITIAL.
        do_protokoll '03' space.
        CONTINUE.
      ENDIF.
* * * F6, UMSDATUM
      info_sap-umsdatum = info_ax-charge_date.
* * * F7, UMSZEIT, not avail in AX KR-1025
*      clear info_sap-ums_zeit.                             "QKZK086857
      info_sap-ums_zeit = '000000'.                         "QKZK086857
* * * F8-F9, SP_KATEG, SPESTEXT
* * * * konvertieren
      PERFORM convert_exp_cat USING info_ax-mis_ind
                                    info_sap-sp_kateg
                                    info_sap-spestext
                                    field_status.
      IF field_status NE 0.
        do_protokoll '06' info_ax-mis_ind.
        CONTINUE.
      ENDIF.
* * * F10, S_H_FLAG
* * * * Mussfeld, Festwerte '+' oder '-'.
* Begin of +/-.
      IF info_ax-sign = '+'.
        info_sap-s_h_flag = '-'.
      ELSEIF info_ax-sign = '-'.
        info_sap-s_h_flag = '+'.
      ELSE.
        do_protokoll '07' info_ax-sign.
        CONTINUE.
      ENDIF.
* Replaces
*      info_sap-s_h_flag = info_ax-sign.
*      if info_ax-sign ne '+' and info_ax-sign ne '-'.
*        do_protokoll '07' info_ax-sign.
*        continue.
*      endif.
* End of +/-.

* * * F11 UMSBETRG
* * * * Format prüfen: maximal 10 St. für SAP, 15 möglich für AX.

* Begin of locl_curr.
      IF NOT ( lcl_conv = 'X' AND
               info_ax-local_curr_code NE info_ax-billed_curr ).
* End of locl_curr.
        info_sap-umsbetrg = info_ax-local_charge_amount.
        IF NOT ( info_ax-local_charge_amount(5) CO '0' ).
          do_protokoll '08' info_ax-local_charge_amount.
          CONTINUE.
        ENDIF.
* * * * Muss NUMC sein
        IF ( NOT info_ax-local_charge_amount CO '0123456789' ).
          do_protokoll '09' info_ax-local_charge_amount.
          CONTINUE.
        ENDIF.
* * * F12-13 UMSNACHK and UMSWAEHR
        info_sap-umsnachk = info_ax-local_dec_place.
* * * * Prüfen Decimal Place ist numerisch
        IF NOT info_sap-umsnachk CO '0123456789 '.
          do_protokoll '10' info_ax-local_dec_place.
          CONTINUE.
        ENDIF.
* * * * Lokale Währung ist ein Mussfeld: prüfen.
        IF info_ax-local_curr_code IS INITIAL.
          do_protokoll '11' space.
          CONTINUE.
        ELSE.
* * * * Prüfen daß die lokale Währung SAP bekannt ist.
          READ TABLE t_tcurc WITH KEY altwr = info_ax-local_curr_code.
          IF sy-subrc NE 0.
            do_protokoll '12' info_ax-local_curr_code.
            CONTINUE.
          ELSE.
            info_sap-umswaehr = t_tcurc-isocd.
          ENDIF.
        ENDIF.
* Begin of Locl_Curr, Country of purchase transferred further down in
* the code, out of the Local Currency test.
* * * F14, UMS_LAND
*      read table T_T005 with key INTCN3 = info_AX-se_Country_Code.
*      if sy-subrc is initial.
*        info_sap-ums_land = T_T005-Intca3.
*      endif.
* End of Locl_curr.
*
* * * F15, UMS_MWST. Diese Information ist generell in KKClearing
*                    nicht benutzt.
* * * F16, UMS_KURS
* * * * Format prüfen: Maxi 11 Stellig in SAP.
        info_sap-ums_kurs = info_ax-curr_exchange_rate+3(11).
        IF ( NOT ( info_ax-curr_exchange_rate(3) CO '0' ) ).
          do_protokoll '16' info_ax-curr_exchange_rate.
          CONTINUE.
        ENDIF.
* * * * Umtauschkurs muss NUMC sein.
        IF ( NOT info_sap-ums_kurs CO '0123456789' ).
          do_protokoll '17' info_ax-curr_exchange_rate.
          CONTINUE.
        ENDIF.
* * * F17-F19, UMS_HEIM, UMSTOTAL.
        IF header_sap-abrnachk IS INITIAL.
          exp = info_ax-billed_dec_place - info_sap-umsnachk - 8.
        ELSE.
          exp = header_sap-abrnachk - info_sap-umsnachk - 8.
        ENDIF.
        info_sap-ums_heim = info_ax-curr_exchange_rate * ( 10 ** exp ) *
                            info_ax-local_charge_amount.
        info_sap-umstotal = info_ax-billed_amount.
* * * * Format prüfen: Maximal 10 St in SAP, 15 für AX
        IF NOT ( info_ax-billed_amount(5) CO '0' ).
          do_protokoll '18' info_ax-billed_amount.
          CONTINUE.
        ENDIF.
* * * * Muss numerisch sein.
        IF NOT ( info_sap-umstotal CO '0123456789 ' ).
          do_protokoll '19' info_ax-billed_amount.
          CONTINUE.
        ENDIF.
* * * F18, UMSGEBUR
        info_sap-umsgebur = info_sap-umstotal - info_sap-ums_heim.
        diff_round =  ( 10 ** exp * info_ax-local_charge_amount / 2 ) + 1.
        IF info_sap-umsgebur LT diff_round.
          CLEAR info_sap-umsgebur.
        ENDIF.
* Begin of Locl_curr.
      ELSE.
        info_sap-umsbetrg = info_ax-billed_amount.
        info_sap-umstotal = info_ax-billed_amount.
        info_sap-ums_heim = info_ax-billed_amount.
        CLEAR info_sap-umsgebur.
        info_sap-ums_kurs = '00010000000'.
        IF header_sap-abrwaehr IS INITIAL.
          READ TABLE t_tcurc WITH KEY altwr = info_ax-billed_curr.
          IF sy-subrc = 0.
            header_sap-abrwaehr = t_tcurc-isocd.
          ELSE.
            p_wrong_head_curr = 'X'.
            EXIT.
          ENDIF.
          header_sap-abrnachk = info_ax-billed_dec_place.
        ENDIF.
        info_sap-umsnachk = header_sap-abrnachk.
        info_sap-umswaehr = header_sap-abrwaehr.
      ENDIF.
* * * F14, UMS_LAND
      READ TABLE t_t005 WITH KEY intcn3 = info_ax-se_country_code.
      IF sy-subrc IS INITIAL.
*       info_sap-ums_land = T_T005-Intca3.                  "MAWK009107
        info_sap-ums_land = t_t005-intca.                   "MAWK009107
      ENDIF.
* End of Locl_curr.

* * * F20, UMSADDCOUNT
* * * F21, TEXT_LANG
      info_sap-text_lang = info_ax-descr1.
* * * F22, UMSDOKNR
      info_sap-umsdoknr = info_ax-txn_nb.
* Header füllen mit Info die sich in Transaktion Records befinden.
      PERFORM finish_filling_header USING info_ax header_sap stat_head.
      IF stat_head NE 0.
        p_wrong_head_curr = 'X'.
        EXIT.
      ENDIF.
*
      MOVE info_sap TO t_info_sap_asc.
      APPEND t_info_sap_asc.

      ccc_sum-numberrecords = ccc_sum-numberrecords + 1.
      IF info_ax-sign = '+'.
*        sum_total = sum_total + info_sap-umstotal.
        sum_purchase = sum_purchase + info_sap-ums_heim.
        sum_addcharge = sum_addcharge + info_sap-umsgebur.
      ELSE.
*        sum_total = sum_total - info_sap-umstotal.
        sum_purchase = sum_purchase - info_sap-ums_heim.
        sum_addcharge = sum_addcharge - info_sap-umsgebur.
      ENDIF.

* Speichern für die Protokollieriung.
      IF p_prot = 'X'.
        conv_ok = conv_ok + 1.
        a_protokoll-data = info_ax.
        a_protokoll-conv_data = info_sap.
        a_protokoll-conv_status = 0.
        a_protokoll-index_dataset = index_dataset.
        APPEND a_protokoll TO t_protokoll.
        CLEAR last_color.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "konvert_ax_sap_ccc




*---------------------------------------------------------------------*
*       FORM finish_filling_header                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_INFO                                                        *
*  -->  P_HEADER                                                      *
*  -->  P_STATUS                                                      *
*---------------------------------------------------------------------*
FORM finish_filling_header USING p_info LIKE info_ax
                                 p_header LIKE ccheader
* Begin of Syntax-1
                                 p_status TYPE i.
* Replaces
*                                 p_status.
* End of Syntax-1

  CLEAR p_status.
  p_header-abrdatum = p_info-bill_date.
  CHECK p_header-abrwaehr IS INITIAL.   " Locl_curr
  READ TABLE t_tcurc WITH KEY altwr = p_info-billed_curr.
  IF sy-subrc = 0.
    p_header-abrwaehr = t_tcurc-isocd.
  ELSE.
    p_status = 1.
  ENDIF.
  p_header-abrnachk = p_info-billed_dec_place.
ENDFORM.                    "finish_filling_header


*&---------------------------------------------------------------------*
*&      Form  Set_SAP_Fix_Header
*&---------------------------------------------------------------------*
* Purpose: sets the fixed part of the SAP Header                       *
*----------------------------------------------------------------------*
FORM set_sap_fix_header.
  header_sap-linetype = 'H'.
  header_sap-version = 'SAPR3V0100'.
  header_sap-abrdatum = sy-datum.
*  header_sap-cardcomp = 'AX'.
  header_sap-cardcomp = 'MC'.
  header_sap-abrwaehr = 'CAD'.
  header_sap-abrnachk = '2'.
  header_sap-abrmwsts = '00000'.
  header_sap-abrtestp = testlauf.


ENDFORM.                    "set_sap_fix_header





*&---------------------------------------------------------------------*
*&      Form  Convert_Exp_Cat
*&---------------------------------------------------------------------*
* Purpose: Converts the Expense Category from AX Format into SAP Format
*----------------------------------------------------------------------*
FORM convert_exp_cat USING p_ind       TYPE c
                           p_exp_type  TYPE c
                           p_exp_text  TYPE c
                           p_status    TYPE i.


  CLEAR p_status.
  CASE p_ind.
    WHEN '01'.
      p_exp_type = 'P'.
      p_exp_text = text-x01.
    WHEN '02'.
      p_exp_type = 'T'.
      p_exp_text = text-x02.
    WHEN '03'.
      p_exp_type = 'A'.
      p_exp_text = text-x03.
    WHEN '04'.
      p_exp_type = 'R'.
      p_exp_text = text-x04.
    WHEN '05'.
      p_exp_type = 'M'.
      p_exp_text = text-x05.
*Begin of QKWK070097
*   when '06'.
*     p_exp_type = 'C'.
*     p_exp_text = text-x06.
*End   of QKWK070097
    WHEN '07'.
      p_exp_type = 'C'.
      p_exp_text = text-x06.
    WHEN 'TC'.
      p_exp_type = 'K'.
      p_exp_text = text-x07.
    WHEN '08' OR 'CA' OR 'DC' OR 'FE' OR 'IN' OR 'NG' OR 'OA' OR 'OI'
*         or 'PA' or 'NF' or 'SP' or 'DS' or 'CT'.          "QKWK070097
          OR 'PA' OR 'NF' OR 'SP' OR 'DS' OR 'CT' OR '06'.  "QKWK070097
      p_exp_type = 'O'.
      p_exp_text = text-x08.
    WHEN OTHERS.
      p_status = 1.
  ENDCASE.
ENDFORM.                    "convert_exp_cat




*&---------------------------------------------------------------------*
*&      Form  Convert_Card_Type
*&---------------------------------------------------------------------*
* Purpose: Converts the CCType from AX Format into SAP Format
*          ( personal / central CC)
*----------------------------------------------------------------------*
FORM convert_card_type USING p_acc_type   TYPE c
                             p_card_flag  TYPE c
                             p_status      TYPE i.

  p_status = 0.
  CASE p_acc_type.
    WHEN 'CRCB'.
      p_card_flag = 'X'.
    WHEN 'FCB'.
      p_card_flag = 'X'.
    WHEN 'BTBA'.
  ENDCASE.
ENDFORM.                    "convert_card_type




*---------------------------------------------------------------------*
*       FORM write_err_msg                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_OP                                                          *
*  -->  P_SUBRC                                                       *
*---------------------------------------------------------------------*
FORM write_err_msg USING p_op    TYPE c
                         p_subrc LIKE sy-subrc.

  DATA: disp_mess(132).

  IF p_op = 'G'.                    " Fehler beim Filename Get
    CASE p_subrc.
      WHEN 1.
        WRITE:/ text-e01.
      WHEN 2.
        WRITE:/ text-e02.
      WHEN 3.
        WRITE:/ text-e03.
      WHEN OTHERS.                                          " 4-5
        WRITE:/ text-e04.
    ENDCASE.
  ELSEIF p_op = 'U'.                " Op_Type = U - Upload
    CASE p_subrc.
      WHEN 1.
        CONCATENATE text-e05 file_nin
                          INTO disp_mess SEPARATED BY space.
        WRITE:/ disp_mess.
      WHEN 2.
        WRITE:/ text-e04.
      WHEN 3.
        CONCATENATE text-e06 file_nin
                          INTO disp_mess SEPARATED BY space.
        WRITE:/ disp_mess.
      WHEN 6.
        WRITE:/ text-e02.
      WHEN 8.
        WRITE:/ text-e16.
      WHEN OTHERS.                                          " 4-5-7-9
        WRITE:/ text-e99.
    ENDCASE.
  ELSEIF p_op = 'R'.                " Read dataset
    CASE p_subrc.
      WHEN 1.
        WRITE:/ text-e08.
      WHEN 2.
        CONCATENATE text-e05 file_nin
                          INTO disp_mess SEPARATED BY space.
        WRITE:/ disp_mess.
      WHEN 3.
        CONCATENATE text-e06 file_nin
                          INTO disp_mess SEPARATED BY space.
        WRITE:/ disp_mess.
      WHEN OTHERS.
        WRITE:/ text-e99.
    ENDCASE.
  ELSE.                             " Op_Type = 'D' - Download
    CASE p_subrc.
      WHEN 1.
        WRITE:/ text-e04.
      WHEN 2.
        WRITE:/ text-e05.
      WHEN 3.
        WRITE:/ text-e06.
      WHEN 6.
        WRITE:/ text-e02.
      WHEN 8.
        WRITE:/ text-e16.
      WHEN OTHERS.                                          " 4-5-7-9
        WRITE:/ text-e99.
    ENDCASE.
  ENDIF.

ENDFORM.                    "write_err_msg



*---------------------------------------------------------------------*
*       FORM credit_card_clearing                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ERROR                                                       *
*---------------------------------------------------------------------*
* Begin of Syntax-1.
FORM credit_card_clearing USING p_error TYPE c.
* Replaces
* form credit_card_clearing using p_error.
* End of Syntax-1.

  DATA: proceed LIKE bapitrvxxx-ccc_proc.
  DATA: dataset LIKE bapitrvcdf OCCURS 100 WITH HEADER LINE,
        errors  LIKE bapitrvcce OCCURS 100 WITH HEADER LINE,
        buffer  LIKE bapitrvctc OCCURS 100 WITH HEADER LINE,
        trips   LIKE bapitrvctt OCCURS 100 WITH HEADER LINE.

  dataset[] = t_info_sap_asc[].                             "#EC ENHOK
  proceed = 1.
  CALL FUNCTION 'HR_TRV_CREDIT_CARD_CLEARING'
    EXPORTING
      compcode     = firmennr
      proceed_flag = proceed
      testrun      = testlauf
    IMPORTING
*     return       = return
      fileheader   = header_sap
    TABLES
      dataset      = dataset
      errors       = errors
      rec_buffer   = buffer
      rec_trips    = trips.
  READ TABLE errors INDEX 1.
* if sy-subrc ne 0.                                         "QKWK006579
  IF sy-subrc EQ 0.                                         "QKWK006579
    p_error = 'X'.
  ENDIF.
ENDFORM.                    "credit_card_clearing


* Begin of Syntax-1.
FORM check_entry USING e_txt TYPE c.
* Replaces
*form check_entry using e_txt.
* End of Syntax-1.
*
  CLEAR e_txt.
*
  IF testlauf = ' '.
    IF run_nb IS INITIAL.
*      e_txt = text-e20.                   "Run
    ELSE.
      ta_key-ccomp = 'AX'.
      rp_imp_c1_ta.
      READ TABLE ccdat WITH KEY abnum = run_nb
                                final = ' '.
      IF sy-subrc = 0.
        e_txt = text-e21.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "check_entry





*---------------------------------------------------------------------*
*       FORM write_sum_line                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_sum_line.
*
  DATA: sum_p TYPE num15,
        sum_a TYPE num15,
        sum_t TYPE num15.

  ccc_sum-linetype = 'S'.
  ccc_sum-accnumber = header_sap-lfdnum.

  ccc_sum-sumpurchase = abs( gv_total_amt ).
  ccc_sum-sumtotal    = abs( gv_total_amt ).


  MOVE ccc_sum TO t_info_sap_asc.
  APPEND t_info_sap_asc.
*
ENDFORM.                    "write_sum_line




* Begin of SSN_Nb
*---------------------------------------------------------------------*
*       FORM load_all_SSN                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*       This subroutine retrieves the corresponding employee numbers  *
*       of all the SSNs on the input file.  It also checks for SSNs   *
*       that has multiple employee numbers (concurrent employment)    *
*       in which case record will error out.                          *
*---------------------------------------------------------------------*
FORM load_all_ssn.

  DATA: l_hold_perid   LIKE p0002-perid.

* Begin of SSNDate.
  DATA: t_pernr_wait  TYPE type_pernr OCCURS 0 WITH HEADER LINE,
        l_hold        TYPE type_pernr,
        to_mult_pernr.
* End of SSNDate.

  CHECK ssn_proc = 'X'.

  LOOP AT t_info_ax_asc.
    IF t_info_ax_asc(1) = '1'.
      MOVE t_info_ax_asc TO info_ax.

      IF NOT info_ax-social_sec_nb IS INITIAL.
        " SSN field is right-justified. Use the last nine digits
        MOVE info_ax-social_sec_nb+1(9) TO t_ssn-perid.
        APPEND t_ssn.
        CLEAR t_ssn.
      ENDIF.

    ENDIF.
  ENDLOOP.


  SORT t_ssn BY perid ASCENDING.
  DELETE ADJACENT DUPLICATES FROM t_ssn COMPARING perid.
  DESCRIBE TABLE t_ssn LINES v_ssn_cnt.

  IF v_ssn_cnt > 0.
    " Retrieve corresponding PERNR of all the SSNs on the file
* Begin of SSNDate
    SELECT perid pernr begda endda
           INTO CORRESPONDING FIELDS OF TABLE t_pernr
    FROM pa0002
    FOR ALL ENTRIES IN t_ssn
    WHERE perid = t_ssn-perid.
* Replaces
*    select perid pernr into corresponding fields of t_pernr
*    from pa0002
*    for all entries in t_ssn
*    where perid = t_ssn-perid.
*      append t_pernr.
*      clear t_pernr.
*    endselect.
* End of SSNDate
  ENDIF.

  SORT t_pernr BY perid ASCENDING.

* check for SSN that has multiple corresponding PERNRs, i.e. concurrent
* employment
  LOOP AT t_pernr.
* Begin of SSNDate.
    IF t_pernr-perid EQ l_hold-perid.
      IF to_mult_pernr = 'X'.
        APPEND t_pernr TO t_mult_pernr.
      ELSEIF t_pernr-pernr NE l_hold-pernr.
        MOVE 'IEQ' TO t_concurr_ssn(3).
        MOVE t_pernr-perid TO t_concurr_ssn-low.
        APPEND t_concurr_ssn.
        CLEAR  t_concurr_ssn.
        LOOP AT t_pernr_wait INTO t_mult_pernr.
          APPEND t_mult_pernr.
        ENDLOOP.
        APPEND l_hold TO t_mult_pernr.
        to_mult_pernr = 'X'.
        APPEND t_pernr TO t_mult_pernr.
      ELSE.
        APPEND l_hold TO t_pernr_wait.
      ENDIF.
    ELSE.
      CLEAR to_mult_pernr.
      CLEAR t_pernr_wait. REFRESH t_pernr_wait.
    ENDIF.
    l_hold = t_pernr.
*
* Replaces
*
*    if not t_pernr-perid eq l_hold_perid.
*      l_hold_perid = t_pernr-perid.
*    else.
*      move 'IEQ' to t_concurr_ssn(3).
*      move t_pernr-perid to t_concurr_ssn-low.
*      append t_concurr_ssn.
*      clear  t_concurr_ssn.
*    endif.
*
* End of SSNDate
  ENDLOOP.

  SORT t_concurr_ssn BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM t_concurr_ssn COMPARING low.
  DESCRIBE TABLE  t_concurr_ssn LINES v_concurr_cnt.

* remove from t_pernr all the SSNs that are concurrent employment
  IF v_concurr_cnt > 0.
    DELETE t_pernr WHERE perid IN t_concurr_ssn.
  ENDIF.

ENDFORM.                    "load_all_SSN

*End of SSN_Nb

FORM f_load_z_tables.

  LOOP AT t_info_ax_asc.

    IF t_info_ax_asc-info+0(4) = '5000'.

      CASE t_info_ax_asc-info+39(2).

        WHEN '0'.
          PERFORM f_populate_addendum_0.

        WHEN '01'.
          PERFORM f_populate_addendum_01.

        WHEN '1'.
          PERFORM f_populate_addendum_1.

        WHEN '11'.
          PERFORM f_populate_addendum_11.

        WHEN '2'.
          PERFORM f_populate_addendum_2.

        WHEN '21'.
          PERFORM f_populate_addendum_21.

        WHEN '3'.
          PERFORM f_populate_addendum_3.

        WHEN '4'.
          PERFORM f_populate_addendum_4.

        WHEN '5'.
          PERFORM f_populate_addendum_5.

        WHEN '6'.
          PERFORM f_populate_addendum_6.

        WHEN '61'.
          PERFORM f_populate_addendum_61.

        WHEN '7'.
          PERFORM f_populate_addendum_7.

      ENDCASE.

    ENDIF.

  ENDLOOP.
******************* start of change by DDwivedi on 8/4*********************


***  IF git_addendum_0 IS NOT INITIAL.
***
***    SELECT record_idntifier issuer_ica issuer_number corp_number addendum_type
***           accnt_number ifile_rec_number
***           FROM ztet_addendum_00
***           INTO TABLE git_add_00
***           FOR ALL ENTRIES IN git_addendum_0
***           WHERE ifile_rec_number = git_addendum_0-ifile_rec_number.
***
***    IF sy-subrc <> 0.
***      INSERT ztet_addendum_00 FROM TABLE git_addendum_0
***             ACCEPTING DUPLICATE KEYS.
***    ELSE.
***      MESSAGE i000(zfi01) WITH text-100.
***      LEAVE LIST-PROCESSING.
***    ENDIF.
***
***  ENDIF.

* ******************** end of change by DDwivedi on 8/4*********************

  IF git_addendum_0 IS NOT INITIAL.
    INSERT zftv_addendum_0 FROM TABLE git_addendum_0
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_01 IS NOT INITIAL.
    INSERT zftv_addendum_01 FROM TABLE git_addendum_01
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_1 IS NOT INITIAL.
    INSERT zftv_addendum_1 FROM TABLE git_addendum_1
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_11 IS NOT INITIAL.
    INSERT zftv_addendum_11 FROM TABLE git_addendum_11
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_2 IS NOT INITIAL.
    INSERT zftv_addendum_2 FROM TABLE git_addendum_2
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_21 IS NOT INITIAL.
    INSERT zftv_addendum_21 FROM TABLE git_addendum_21
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_3 IS NOT INITIAL.
    INSERT zftv_addendum_3 FROM TABLE git_addendum_3
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_4 IS NOT INITIAL.
    INSERT zftv_addendum_4 FROM TABLE git_addendum_4
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_5 IS NOT INITIAL.
    INSERT zftv_addendum_5 FROM TABLE git_addendum_5
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_6 IS NOT INITIAL.
    INSERT zftv_addendum_6 FROM TABLE git_addendum_6
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_61 IS NOT INITIAL.
    INSERT zftv_addendum_61 FROM TABLE git_addendum_61
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  IF git_addendum_7 IS NOT INITIAL.
    INSERT zftv_addendum_7 FROM TABLE git_addendum_7
           ACCEPTING DUPLICATE KEYS.
  ENDIF.

  COMMIT WORK.

ENDFORM.                    "f_load_z_tables

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_0
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_0.

  data:  lv_amt_b type c LENGTH 7,
       lv_amt_a type c LENGTH 4,
       l1 type i,
       l2 type i.
  data:  lv_amount TYPE string,
         lv_orig_amount type string,
         lv_total_amt   TYPE string,
         lv_exch        TYPE string,
         lv_exch_11     type c LENGTH 11,
         lv_orig_curr type c LENGTH 3,
         lv_wrbtr type PTRV_DOC_IT-wrbtr.

  CLEAR: gv_card_number,
         gv_txn_date,
         gv_reference.

  gwa_addendum_0-mandt             = sy-mandt.
  gwa_addendum_0-record_idntifier  = t_info_ax_asc-info+0(4).
  gwa_addendum_0-issuer_ica        = t_info_ax_asc-info+4(5).
  gwa_addendum_0-issuer_number     = t_info_ax_asc-info+9(11).
  gwa_addendum_0-corp_number       = t_info_ax_asc-info+20(19).
  gwa_addendum_0-addendum_type     = t_info_ax_asc-info+39(1).
  gwa_addendum_0-accnt_number      = t_info_ax_asc-info+66(19).
  gwa_addendum_0-ifile_rec_number  = t_info_ax_asc-info+495(6).
  gwa_addendum_0-merchant_act_ind  = t_info_ax_asc-info+65(1).
  gwa_addendum_0-ref_number        = t_info_ax_asc-info+95(23).
  gwa_addendum_0-record_type       = t_info_ax_asc-info+118(1).
  gwa_addendum_0-txn_type          = t_info_ax_asc-info+119(1).
  gwa_addendum_0-dr_cr_ind         = t_info_ax_asc-info+120(1).
  gwa_addendum_0-post_date         = t_info_ax_asc-info+137(8).
  gwa_addendum_0-txn_date          = t_info_ax_asc-info+145(8).
  gwa_addendum_0-processing_date   = t_info_ax_asc-info+153(8).
  gwa_addendum_0-merchant_name     = t_info_ax_asc-info+161(25).
  gwa_addendum_0-merchant_city     = t_info_ax_asc-info+186(13).
  gwa_addendum_0-merchant_state    = t_info_ax_asc-info+199(3).
  gwa_addendum_0-merchant_country  = t_info_ax_asc-info+202(2).
  gwa_addendum_0-merchant_cc       = t_info_ax_asc-info+205(4).

*Account / Transaction amount
  lv_amount = t_info_ax_asc-info+121(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr = lv_amount.
*gwa_addendum_0-txn_amount        = lv_amount.
  write lv_wrbtr to gwa_addendum_0-txn_amount DECIMALS 2.

**Original Amount
**divide by 10000 because amount is in 4 decimal places
  lv_orig_amount = t_info_ax_asc-info+209(16).
  SHIFT lv_orig_amount LEFT DELETING LEADING '0'.
  lv_orig_amount = lv_orig_amount / 10000.
  lv_wrbtr = lv_orig_amount.
  write lv_wrbtr to gwa_addendum_0-amount_org DECIMALS 2.
*gwa_addendum_0-amount_org  = lv_orig_amount.

*  SHIFT lv_amount LEFT DELETING LEADING '0'.
*  lv_amount = lv_amount / 10000.
*  gwa_addendum_0-amount_org        = lv_amount.
*  CLEAR lv_amount.

**  gwa_addendum_0-org_curr_code     = t_info_ax_asc-info+225(3).
  gwa_addendum_0-org_curr_code     = gc_currency.
  lv_orig_curr = t_info_ax_asc-info+225(3).

* Insert SPARGA
  clear w_waers.
  SELECT SINGLE waers from TCURC into w_waers
    WHERE ALTWR = lv_orig_curr.
  IF sy-subrc = 0.
    gwa_addendum_0-org_curr_code = w_waers.
  ELSE.
    if lv_orig_curr = '124'.
      gwa_addendum_0-org_curr_code = 'CAD'.
    ELSEIF lv_orig_curr = '840'.
      gwa_addendum_0-org_curr_code = 'USD'.
    endif.
  ENDIF.
* End of insert SPARGA

*  lv_amount = gc_exch.
*  SHIFT lv_amount LEFT DELETING LEADING '0'.
*  lv_amount = lv_amount / 10000000.
*  gwa_addendum_0-conv_rate         = lv_amount.
*  CLEAR lv_amount.
*Echange Rate
  lv_exch = t_info_ax_asc-info+236(16).
  SHIFT lv_exch LEFT DELETING LEADING '0'.
  lv_exch_11 = lv_exch / 10000000.
  if lv_exch_11 is NOT INITIAL or
     lv_exch_11 <> '0'.
    lv_exch_11 = 1 / lv_exch_11.
  endif.
  split lv_exch_11 at '.' into lv_amt_a lv_amt_b.
  CONDENSE lv_amt_a NO-GAPS.
  CONDENSE lv_amt_b NO-GAPS.
  l1 = strlen( lv_amt_a ).
  l2 = strlen( lv_amt_b ).
*  WHILE l1 < 4.
*        CONCATENATE '0' lv_amt_a INTO lv_amt_a.
*        l1 = l1 + 1.
*  ENDWHILE.
  WHILE l2 < 7.
    CONCATENATE lv_amt_b '0' INTO lv_amt_b.
    l2 = l2 + 1.
  ENDWHILE.
  CONCATENATE lv_amt_a '.' lv_amt_b INTO gwa_addendum_0-conv_rate.
*****************
  gwa_addendum_0-curr_conv_date    = t_info_ax_asc-info+228(5).
  gwa_addendum_0-post_curr_code    = gc_currency. "t_info_ax_asc-info+233(3).
  gwa_addendum_0-conv_exp          = t_info_ax_asc-info+252(1).
  gwa_addendum_0-acq_ica           = t_info_ax_asc-info+253(4).
  gwa_addendum_0-cust_code         = t_info_ax_asc-info+257(17).

  lv_amount = t_info_ax_asc-info+274(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
*  gwa_addendum_0-st_amount         = lv_amount.
  lv_wrbtr = lv_amount.
  write lv_wrbtr to gwa_addendum_0-st_amount DECIMALS 2.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+296(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
*gwa_addendum_0-freight_amount    = lv_amount.
  lv_wrbtr = lv_amount.
  write lv_wrbtr to gwa_addendum_0-freight_amount  DECIMALS 2.
  CLEAR lv_amount.

  gwa_addendum_0-dest_pcode        = t_info_ax_asc-info+312(10).
  gwa_addendum_0-merchant_type     = t_info_ax_asc-info+322(4).
  gwa_addendum_0-merchant_pcode    = t_info_ax_asc-info+326(10).

  lv_amount = t_info_ax_asc-info+336(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
*  gwa_addendum_0-duty_amt          = lv_amount.
  lv_wrbtr = lv_amount.
  write lv_wrbtr to gwa_addendum_0-duty_amt  DECIMALS 2.
  CLEAR lv_amount.

  gwa_addendum_0-merchant_tax_id   = t_info_ax_asc-info+352(15).
  gwa_addendum_0-merchant_st_code  = t_info_ax_asc-info+367(3).
  gwa_addendum_0-ship_from_pcode   = t_info_ax_asc-info+370(10).

  lv_amount = t_info_ax_asc-info+380(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
*  gwa_addendum_0-alt_tax_amt       = lv_amount.
  lv_wrbtr = lv_amount.
  write lv_wrbtr to  gwa_addendum_0-alt_tax_amt  DECIMALS 2.
  CLEAR lv_amount.

  gwa_addendum_0-dest_ccode        = t_info_ax_asc-info+396(3).
  gwa_addendum_0-merchant_ref_no   = t_info_ax_asc-info+399(17).
  gwa_addendum_0-alt_tax_ind       = t_info_ax_asc-info+416(1).
  gwa_addendum_0-alt_tax_idn       = t_info_ax_asc-info+417(15).
  gwa_addendum_0-st_coll_ind       = t_info_ax_asc-info+432(1).
  gwa_addendum_0-adn_detail_ind    = t_info_ax_asc-info+433(1).
  gwa_addendum_0-merchant_id       = t_info_ax_asc-info+434(15).
  gwa_addendum_0-adj_reason_code   = t_info_ax_asc-info+449(5).
  gwa_addendum_0-adj_desc          = t_info_ax_asc-info+454(40).
  gwa_addendum_0-mtn_action_code   = t_info_ax_asc-info+494(1).
  gwa_addendum_0-error_code        = t_info_ax_asc-info+501(6).
  gwa_addendum_0-upd_date          = sy-datum.

  gv_card_number = gwa_addendum_0-accnt_number.
  gv_txn_date    = gwa_addendum_0-txn_date.
  gv_reference   = gwa_addendum_0-ifile_rec_number.

  APPEND gwa_addendum_0 TO git_addendum_0.
  CLEAR gwa_addendum_0.

  CONCATENATE 'MC' gv_card_number INTO gwa_card_number-card_number.
  CONDENSE gwa_card_number-card_number.
  APPEND gwa_card_number TO git_card_number.
  CLEAR gwa_card_number.

ENDFORM.                    "f_populate_addendum_0

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_01.

  gwa_addendum_01-mandt            = sy-mandt.
  gwa_addendum_01-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_01-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_01-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_01-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_01-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_01-accnt_number     = gv_card_number.
  gwa_addendum_01-ifile_rec_number = t_info_ax_asc-info+84(6).
  gwa_addendum_01-user_amount      = t_info_ax_asc-info+65(16).
  gwa_addendum_01-field_idn        = t_info_ax_asc-info+81(3).
  gwa_addendum_01-error_code       = t_info_ax_asc-info+90(6).
  gwa_addendum_01-upd_date         = sy-datum.
  gwa_addendum_01-txn_date         = gv_txn_date.
  gwa_addendum_01-reference        = gv_reference.

  APPEND gwa_addendum_01 TO git_addendum_01.
  CLEAR gwa_addendum_01.

ENDFORM.                   "f_populate_addendum_01

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_1.

  DATA: lv_amount TYPE string,
        lv_wrbtr type PTRV_DOC_IT-wrbtr.

  gwa_addendum_1-mandt            = sy-mandt.
  gwa_addendum_1-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_1-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_1-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_1-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_1-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_1-accnt_number     = gv_card_number.
  gwa_addendum_1-ifile_rec_number = t_info_ax_asc-info+180(6).
  gwa_addendum_1-product_code     = t_info_ax_asc-info+65(12).
  gwa_addendum_1-item_description = t_info_ax_asc-info+77(35).
  gwa_addendum_1-item_qty         = t_info_ax_asc-info+112(5).
  gwa_addendum_1-item_uom         = t_info_ax_asc-info+117(3).

  lv_amount = t_info_ax_asc-info+120(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
*    gwa_addendum_1-ext_item_amt     = lv_amount.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_1-ext_item_amt DECIMALS 2.
  CLEAR lv_amount.

  gwa_addendum_1-dr_cr_ind        = t_info_ax_asc-info+136(1).
  gwa_addendum_1-ng_ind_ext_item  = t_info_ax_asc-info+137(1).

  lv_amount = t_info_ax_asc-info+138(5).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 100000.
  gwa_addendum_1-tax_rate_appl    = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_1-tax_type_appl    = t_info_ax_asc-info+143(4).

  lv_amount = t_info_ax_asc-info+147(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
*  gwa_addendum_1-tax_amt          = lv_amount.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_1-tax_amt DECIMALS 2.
  CLEAR lv_amount.

  gwa_addendum_1-discount_ind     = t_info_ax_asc-info+163(1).

  lv_amount = t_info_ax_asc-info+164(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
*  gwa_addendum_1-discount_amt     = lv_amount.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_1-discount_amt DECIMALS 2.
  CLEAR lv_amount.

  gwa_addendum_1-error_code       = t_info_ax_asc-info+186(6).
  gwa_addendum_1-upd_date         = sy-datum.
  gwa_addendum_1-txn_date         = gv_txn_date.
  gwa_addendum_1-reference        = gv_reference.

  APPEND gwa_addendum_1 TO git_addendum_1.
  CLEAR gwa_addendum_1.

ENDFORM.                    "f_populate_addendum_1

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_11.

  DATA: lv_amount TYPE string,
        lv_wrbtr type PTRV_DOC_IT-wrbtr.

  gwa_addendum_11-mandt            = sy-mandt.
  gwa_addendum_11-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_11-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_11-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_11-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_11-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_11-accnt_number     = gv_card_number.
  gwa_addendum_11-ifile_rec_number = t_info_ax_asc-info+84(6).

  lv_amount = t_info_ax_asc-info+65(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_11-user_amt DECIMALS 2.
*  gwa_addendum_11-user_amt         = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_11-field_idn        = t_info_ax_asc-info+81(3).
  gwa_addendum_11-error_code       = t_info_ax_asc-info+90(6).
  gwa_addendum_11-upd_date         = sy-datum.
  gwa_addendum_11-txn_date         = gv_txn_date.
  gwa_addendum_11-reference        = gv_reference.

  APPEND gwa_addendum_11 TO git_addendum_11.
  CLEAR gwa_addendum_11.

ENDFORM.                    "f_populate_addendum_11

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_2.

  DATA: lv_amount TYPE string,
       lv_wrbtr type PTRV_DOC_IT-wrbtr.

  CLEAR gv_ref_ifile_21.

  gwa_addendum_2-mandt            = sy-mandt.
  gwa_addendum_2-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_2-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_2-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_2-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_2-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_2-accnt_number     = gv_card_number.
  gwa_addendum_2-ifile_rec_number = t_info_ax_asc-info+275(6).
  gwa_addendum_2-passenger_name   = t_info_ax_asc-info+65(25).
  gwa_addendum_2-departure_date   = t_info_ax_asc-info+90(8).
  gwa_addendum_2-city_of_origin   = t_info_ax_asc-info+98(5).
  gwa_addendum_2-ta_code          = t_info_ax_asc-info+103(8).
  gwa_addendum_2-ta_name          = t_info_ax_asc-info+111(25).
  gwa_addendum_2-ticket_number    = t_info_ax_asc-info+136(15).
  gwa_addendum_2-customer_code    = t_info_ax_asc-info+151(64).
  gwa_addendum_2-issue_date       = t_info_ax_asc-info+215(8).
  gwa_addendum_2-issuing_carrier  = t_info_ax_asc-info+223(4).

  lv_amount = t_info_ax_asc-info+227(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_2-total_fare  DECIMALS 2.
*  gwa_addendum_2-total_fare       = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+243(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_2-total_fees DECIMALS 2.
*  gwa_addendum_2-total_fees       = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+259(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_2-total_taxes DECIMALS 2.
*  gwa_addendum_2-total_taxes      = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_2-error_code       = t_info_ax_asc-info+281(6).
  gwa_addendum_2-upd_date         = sy-datum.
  gwa_addendum_2-txn_date         = gv_txn_date.
  gwa_addendum_2-reference        = gv_reference.

  gv_ref_ifile_21 = gwa_addendum_2-ifile_rec_number.


  APPEND gwa_addendum_2 TO git_addendum_2.
  CLEAR gwa_addendum_2.

ENDFORM.                    "f_populate_addendum_2

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_21
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_21.

  DATA: lv_amount TYPE string,
        lv_wrbtr type PTRV_DOC_IT-wrbtr.

  gwa_addendum_21-mandt            = sy-mandt.
  gwa_addendum_21-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_21-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_21-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_21-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_21-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_21-accnt_number     = gv_card_number.
  gwa_addendum_21-ifile_rec_number = t_info_ax_asc-info+218(6).
  gwa_addendum_21-trip_leg_number  = t_info_ax_asc-info+65(2).
  gwa_addendum_21-carrier_code     = t_info_ax_asc-info+67(2).
  gwa_addendum_21-service_class    = t_info_ax_asc-info+69(2).
  gwa_addendum_21-stop_over_code   = t_info_ax_asc-info+71(1).
  gwa_addendum_21-city_of_origin   = t_info_ax_asc-info+72(5).
  gwa_addendum_21-cjn_tkt          = t_info_ax_asc-info+77(14).
  gwa_addendum_21-travel_date      = t_info_ax_asc-info+91(8).
  gwa_addendum_21-exchange_tkt     = t_info_ax_asc-info+99(15).
  gwa_addendum_21-coupon_number    = t_info_ax_asc-info+114(1).
  gwa_addendum_21-city_of_dest     = t_info_ax_asc-info+115(5).
  gwa_addendum_21-fare_base_code   = t_info_ax_asc-info+120(15).
  gwa_addendum_21-flight_number    = t_info_ax_asc-info+135(5).
  gwa_addendum_21-dept_time        = t_info_ax_asc-info+140(4).
  gwa_addendum_21-dept_time_seg    = t_info_ax_asc-info+144(1).
  gwa_addendum_21-arr_time         = t_info_ax_asc-info+145(4).
  gwa_addendum_21-arr_time_seg     = t_info_ax_asc-info+149(1).

  lv_amount = t_info_ax_asc-info+150(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_21-fare  DECIMALS 2.
*  gwa_addendum_21-fare             = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+166(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_21-fee  DECIMALS 2.
*  gwa_addendum_21-fee              = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+182(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_21-taxes  DECIMALS 2.
*  gwa_addendum_21-taxes            = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_21-endorsements     = t_info_ax_asc-info+198(20).
  gwa_addendum_21-error_code       = t_info_ax_asc-info+224(6).
  gwa_addendum_21-upd_date         = sy-datum.
  gwa_addendum_21-txn_date         = gv_txn_date.
  gwa_addendum_21-ref_ifile_rec    = gv_ref_ifile_21.
  gwa_addendum_21-reference        = gv_reference.

  APPEND gwa_addendum_21 TO git_addendum_21.
  CLEAR gwa_addendum_21.

ENDFORM.                    "f_populate_addendum_21

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_3.

  DATA: lv_amount TYPE string,
        lv_wrbtr type PTRV_DOC_IT-wrbtr.

  gwa_addendum_3-mandt            = sy-mandt.
  gwa_addendum_3-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_3-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_3-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_3-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_3-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_3-accnt_number     = gv_card_number.
  gwa_addendum_3-ifile_rec_number = t_info_ax_asc-info+291(6).
  gwa_addendum_3-arrival_date     = t_info_ax_asc-info+65(8).
  gwa_addendum_3-departure_date   = t_info_ax_asc-info+73(8).
  gwa_addendum_3-folio_number     = t_info_ax_asc-info+81(10).
  gwa_addendum_3-property_phone   = t_info_ax_asc-info+91(25).
  gwa_addendum_3-cust_service_no  = t_info_ax_asc-info+116(25).

  lv_amount = t_info_ax_asc-info+141(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_3-room_rate  DECIMALS 2.
*  gwa_addendum_3-room_rate        = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+157(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_3-room_tax DECIMALS 2.
*  gwa_addendum_3-room_tax         = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_3-program_code     = t_info_ax_asc-info+173(2).

  lv_amount = t_info_ax_asc-info+175(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_3-tel_charges  DECIMALS 2.
*  gwa_addendum_3-tel_charges      = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+191(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_3-rstrnt_room_ser  DECIMALS 2.
*  gwa_addendum_3-rstrnt_room_ser  = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+207(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_3-bar_charges  DECIMALS 2.
*  gwa_addendum_3-bar_charges      = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+223(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_3-gift_shp_charges  DECIMALS 2.
*  gwa_addendum_3-gift_shp_charges = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+239(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_3-laundry_amt  DECIMALS 2.
*  gwa_addendum_3-laundry_amt      = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_3-other_ser_code   = t_info_ax_asc-info+255(3).

  lv_amount = t_info_ax_asc-info+258(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_3-other_ser_charge  DECIMALS 2.
*  gwa_addendum_3-other_ser_charge = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_3-bill_adj_ind     = t_info_ax_asc-info+274(1).

  lv_amount = t_info_ax_asc-info+275(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_3-bill_adj_amt  DECIMALS 2.
*  gwa_addendum_3-bill_adj_amt     = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_3-error_code       = t_info_ax_asc-info+297(6).
  gwa_addendum_3-upd_date         = sy-datum.
  gwa_addendum_3-txn_date         = gv_txn_date.
  gwa_addendum_3-reference        = gv_reference.

  APPEND gwa_addendum_3 TO git_addendum_3.
  CLEAR gwa_addendum_3.

ENDFORM.                    "f_populate_addendum_3

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_4.

  DATA: lv_amount TYPE string,
        lv_wrbtr type PTRV_DOC_IT-wrbtr.

  gwa_addendum_4-mandt            = sy-mandt.
  gwa_addendum_4-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_4-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_4-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_4-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_4-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_4-accnt_number     = gv_card_number.
  gwa_addendum_4-ifile_rec_number = t_info_ax_asc-info+251(6).
  gwa_addendum_4-rental_agr_no    = t_info_ax_asc-info+65(9).
  gwa_addendum_4-renter_name      = t_info_ax_asc-info+74(20).
  gwa_addendum_4-rtl_ret_city     = t_info_ax_asc-info+94(18).
  gwa_addendum_4-rtl_ret_state    = t_info_ax_asc-info+112(3).
  gwa_addendum_4-rtl_ret_country  = t_info_ax_asc-info+115(3).
  gwa_addendum_4-rtl_return_date  = t_info_ax_asc-info+118(8).
  gwa_addendum_4-return_loc_id    = t_info_ax_asc-info+126(10).
  gwa_addendum_4-cust_serv_no     = t_info_ax_asc-info+136(25).
  gwa_addendum_4-rental_class     = t_info_ax_asc-info+161(2).

  lv_amount = t_info_ax_asc-info+163(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_4-daily_rtl_rate DECIMALS 2.
*  gwa_addendum_4-daily_rtl_rate   = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+179(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_4-rate_per_mile  DECIMALS 2.
*  gwa_addendum_4-rate_per_mile    = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_4-total_miles      = t_info_ax_asc-info+195(6).
  gwa_addendum_4-max_free_miles   = t_info_ax_asc-info+201(6).
  gwa_addendum_4-insurance_ind    = t_info_ax_asc-info+207(1).

  lv_amount = t_info_ax_asc-info+208(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_4-insurance_charge  DECIMALS 2.
*  gwa_addendum_4-insurance_charge = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_4-adj_amt_ind      = t_info_ax_asc-info+224(1).

  lv_amount = t_info_ax_asc-info+225(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_4-adj_amt  DECIMALS 2.
*  gwa_addendum_4-adj_amt          = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_4-program_code     = t_info_ax_asc-info+241(2).
  gwa_addendum_4-check_out_date   = t_info_ax_asc-info+243(8).
  gwa_addendum_4-error_code       = t_info_ax_asc-info+257(6).
  gwa_addendum_4-upd_date         = sy-datum.
  gwa_addendum_4-txn_date         = gv_txn_date.
  gwa_addendum_4-reference        = gv_reference.

  APPEND gwa_addendum_4 TO git_addendum_4.
  CLEAR gwa_addendum_4.

ENDFORM.                    "f_populate_addendum_4

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_5.

  gwa_addendum_5-mandt            = sy-mandt.
  gwa_addendum_5-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_5-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_5-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_5-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_5-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_5-accnt_number     = gv_card_number.
  gwa_addendum_5-ifile_rec_number = t_info_ax_asc-info+196(6).
  gwa_addendum_5-generic_adn_data = t_info_ax_asc-info+65(131).
  gwa_addendum_5-error_code       = t_info_ax_asc-info+202(6).
  gwa_addendum_5-upd_date         = sy-datum.
  gwa_addendum_5-txn_date         = gv_txn_date.
  gwa_addendum_5-reference        = gv_reference.

  APPEND gwa_addendum_5 TO git_addendum_5.
  CLEAR gwa_addendum_5.

ENDFORM.                    "f_populate_addendum_5

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_6
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_6.

  DATA: lv_amount TYPE string,
         lv_wrbtr type PTRV_DOC_IT-wrbtr.

  CLEAR gv_ref_ifile_61.

  gwa_addendum_6-mandt            = sy-mandt.
  gwa_addendum_6-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_6-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_6-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_6-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_6-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_6-accnt_number     = gv_card_number.
  gwa_addendum_6-ifile_rec_number = t_info_ax_asc-info+277(6).
  gwa_addendum_6-oil_co_brand     = t_info_ax_asc-info+65(4).
  gwa_addendum_6-mchant_st_addr   = t_info_ax_asc-info+69(35).
  gwa_addendum_6-mchant_post_code = t_info_ax_asc-info+104(10).
  gwa_addendum_6-purchase_time    = t_info_ax_asc-info+114(4).
  gwa_addendum_6-mfuel_ser_type   = t_info_ax_asc-info+118(1).
  gwa_addendum_6-mfuel_prod_code  = t_info_ax_asc-info+119(3).

  lv_amount = t_info_ax_asc-info+122(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_6-mfuel_unit_price DECIMALS 4.
*  gwa_addendum_6-mfuel_unit_price = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_6-mfuel_uom        = t_info_ax_asc-info+138(1).

  lv_amount = t_info_ax_asc-info+139(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_6-mfuel_qty DECIMALS 4.
*  gwa_addendum_6-mfuel_qty        = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+155(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_6-mfuel_sale_amt DECIMALS 2.
*  gwa_addendum_6-mfuel_sale_amt   = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_6-odo_reading      = t_info_ax_asc-info+171(7).
  gwa_addendum_6-vehicle_no       = t_info_ax_asc-info+178(17).
  gwa_addendum_6-driver_no        = t_info_ax_asc-info+195(17).
  gwa_addendum_6-mspt_code        = t_info_ax_asc-info+212(1).

  lv_amount = t_info_ax_asc-info+213(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_6-coupon_amt DECIMALS 2.
*  gwa_addendum_6-coupon_amt       = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+229(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_6-tax_exmp_amt DECIMALS 2.
*  gwa_addendum_6-tax_exmp_amt     = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+245(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_6-tax_amt_1 DECIMALS 2.
*  gwa_addendum_6-tax_amt_1        = lv_amount.
  CLEAR lv_amount.

  lv_amount = t_info_ax_asc-info+261(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_6-tax_amt_2 DECIMALS 4.
*  gwa_addendum_6-tax_amt_2        = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_6-error_code       = t_info_ax_asc-info+283(6).
  gwa_addendum_6-upd_date         = sy-datum.
  gwa_addendum_6-txn_date         = gv_txn_date.
  gwa_addendum_6-reference        = gv_reference.
  gv_ref_ifile_61                 = gwa_addendum_6-ifile_rec_number.

  APPEND gwa_addendum_6 TO git_addendum_6.
  CLEAR gwa_addendum_6.

ENDFORM.                    "f_populate_addendum_6

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_61
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_61.

  DATA: lv_amount TYPE string,
        lv_wrbtr type PTRV_DOC_IT-wrbtr.

  gwa_addendum_61-mandt            = sy-mandt.
  gwa_addendum_61-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_61-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_61-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_61-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_61-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_61-accnt_number     = gv_card_number.
  gwa_addendum_61-ifile_rec_number = t_info_ax_asc-info+195(6).
  gwa_addendum_61-item_prod_code   = t_info_ax_asc-info+65(12).
  gwa_addendum_61-item_description = t_info_ax_asc-info+77(35).
  gwa_addendum_61-item_qty         = t_info_ax_asc-info+112(5).
  gwa_addendum_61-item_uom         = t_info_ax_asc-info+117(3).

  lv_amount = t_info_ax_asc-info+120(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_61-ext_item_amt DECIMALS 2.
*  gwa_addendum_61-ext_item_amt     = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_61-discount_ind     = t_info_ax_asc-info+136(1).

  lv_amount = t_info_ax_asc-info+137(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_61-discount_amt DECIMALS 2.
*  gwa_addendum_61-discount_amt     = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_61-net_gr_ind       = t_info_ax_asc-info+153(1).

  lv_amount = t_info_ax_asc-info+154(5).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 100000.
  gwa_addendum_61-tax_rate_appl    = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_61-tax_type_appl    = t_info_ax_asc-info+159(4).

  lv_amount = t_info_ax_asc-info+163(16).
  SHIFT lv_amount LEFT DELETING LEADING '0'.
  lv_amount = lv_amount / 10000.
  lv_wrbtr =  lv_amount.
  write lv_wrbtr to gwa_addendum_61-tax_amt DECIMALS 2.
*  gwa_addendum_61-tax_amt          = lv_amount.
  CLEAR lv_amount.

  gwa_addendum_61-dr_cr_ind        = t_info_ax_asc-info+179(1).
  gwa_addendum_61-alt_tax_idn      = t_info_ax_asc-info+180(15).
  gwa_addendum_61-error_code       = t_info_ax_asc-info+201(6).
  gwa_addendum_61-upd_date         = sy-datum.
  gwa_addendum_61-txn_date         = gv_txn_date.
  gwa_addendum_61-ref_ifile_rec    = gv_ref_ifile_61.
  gwa_addendum_61-reference        = gv_reference.

  APPEND gwa_addendum_61 TO git_addendum_61.
  CLEAR gwa_addendum_61.

ENDFORM.                    "f_populate_addendum_61

*&---------------------------------------------------------------------*
*&      Form  f_populate_addendum_7
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_populate_addendum_7.

  gwa_addendum_7-mandt            = sy-mandt.
  gwa_addendum_7-record_idntifier = t_info_ax_asc-info+0(4).
  gwa_addendum_7-issuer_ica       = t_info_ax_asc-info+4(5).
  gwa_addendum_7-issuer_number    = t_info_ax_asc-info+9(11).
  gwa_addendum_7-corp_number      = t_info_ax_asc-info+20(19).
  gwa_addendum_7-addendum_type    = t_info_ax_asc-info+39(3).
  gwa_addendum_7-accnt_number     = gv_card_number.
  gwa_addendum_7-ifile_rec_number = t_info_ax_asc-info+180(6).
  gwa_addendum_7-mchant_st_addr   = t_info_ax_asc-info+65(25).
  gwa_addendum_7-mchant_phone     = t_info_ax_asc-info+90(25).
  gwa_addendum_7-sole_prop_name   = t_info_ax_asc-info+115(25).
  gwa_addendum_7-legal_corp_name  = t_info_ax_asc-info+140(25).
  gwa_addendum_7-mchant_post_code = t_info_ax_asc-info+165(10).
  gwa_addendum_7-dun_number       = t_info_ax_asc-info+175(15).
  gwa_addendum_7-error_code       = t_info_ax_asc-info+196(6).
  gwa_addendum_7-upd_date         = sy-datum.
  gwa_addendum_7-txn_date         = gv_txn_date.
  gwa_addendum_7-reference        = gv_reference.

  APPEND gwa_addendum_7 TO git_addendum_7.
  CLEAR gwa_addendum_7.

ENDFORM.                    "f_populate_addendum_7

*&---------------------------------------------------------------------*
*&      Form  f_map_transaction_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_map_transaction_data.

  DATA: index_dataset TYPE i,
        field_status  TYPE i,
        last_color(3) TYPE c,
        exp           TYPE i,
        diff_round    LIKE info_sap-umstotal,
        stat_head     TYPE i,
        header_curr   LIKE info_ax-billed_curr.

  DATA: lv_amount      TYPE string,
        lv_orig_amount type string,
        lv_total_amt   TYPE string,
        lv_exch        TYPE string,
        lv_exch_11     type c LENGTH 11,
        lv_card_number TYPE pa0105-usrid,
        lv_orig_curr type c LENGTH 3.
  data:  lv_amt_b type c LENGTH 7,
         lv_amt_a type c LENGTH 4,
         l1 type i,
         l2 type i.
  DATA: lv_flag        TYPE c,
        lv_rec_cnt     TYPE i.
* Get MCC Code from ZTET_MCC
  PERFORM f_select_ztet_mcc.

* Get Employee Number from PA0105 based on credit card number
  PERFORM f_select_pa0105.

* Get Employee Name from PA0001 based on Employee Number
  PERFORM f_select_pa0001.

* Get Currency Code corresponding to ISO Code
  PERFORM f_select_tcurc.

  CLEAR:lv_flag,
        lv_rec_cnt,
        gv_total_amt.

  DESCRIBE TABLE t_info_ax_asc LINES lv_rec_cnt.

  LOOP AT t_info_ax_asc.

    index_dataset = index_dataset + 1.

    IF t_info_ax_asc+0(4) = '1000'.
      header_sap-lfdnum   = t_info_ax_asc-info+115(10).
      header_sap-compcode = t_info_ax_asc-info+125(5).
    ENDIF.

    IF t_info_ax_asc-info+0(4) = '5000' AND t_info_ax_asc-info+39(2) = '0'.

      info_ax = t_info_ax_asc.

      info_sap-linetype = 'M'.

      lv_card_number = t_info_ax_asc-info+066(19).
      CONCATENATE 'MC' lv_card_number INTO lv_card_number.
      CONDENSE lv_card_number.

* Employee Number & Name
      READ TABLE git_pa0105 INTO gwa_pa0105
                 WITH KEY usrid = lv_card_number.

      IF sy-subrc = 0.

        info_sap-pernr = gwa_pa0105-pernr.

        READ TABLE git_pa0001 INTO gwa_pa0001
                   WITH KEY pernr = gwa_pa0105-pernr.

        IF sy-subrc = 0.
          info_sap-pname = gwa_pa0001-ename.
        ENDIF.

      ELSE.

        do_protokoll '01' t_info_ax_asc-info+066(19).
        CONTINUE.

      ENDIF.

      info_sap-cardnum  = t_info_ax_asc-info+066(19).
      IF info_sap-cardnum IS INITIAL.
        do_protokoll '03' space.
        CONTINUE.
      ENDIF.

      info_sap-cardflag = 'X'.
      info_sap-umsdatum = t_info_ax_asc-info+137(8).
      info_sap-ums_zeit = '000000'.

* Transaction Category and Transaction Key
      READ TABLE git_ztet_mcc INTO gwa_ztet_mcc
                 WITH KEY mcc = t_info_ax_asc-info+205(4).
      IF sy-subrc = 0.
        info_sap-sp_kateg = gwa_ztet_mcc-te_code.
        info_sap-spestext = gwa_ztet_mcc-te_type.
      ELSE.
        do_protokoll '99' t_info_ax_asc-info+205(4).
        IF index_dataset NE lv_rec_cnt.
          CONTINUE.
        ELSE.
          lv_flag = 'X'.
        ENDIF.
      ENDIF.

      IF t_info_ax_asc-info+120(1) = 'D'.
        info_sap-s_h_flag = '-'.
      ELSEIF t_info_ax_asc-info+120(1) = 'C'.
        info_sap-s_h_flag = '+'.
      ELSE.
        do_protokoll '07' t_info_ax_asc-info+120(1).
        CONTINUE.
      ENDIF.

*comment the orignal code
** Amount in Original Currency
**      lv_amount = t_info_ax_asc-info+209(16).
**      SHIFT lv_amount LEFT DELETING LEADING '0'.
**      lv_amount = lv_amount / 100.
**      info_sap-umsbetrg = lv_amount.
**      CLEAR lv_amount.
**
**      info_sap-umsnachk = 2.
      IF lv_flag = 'X'.
        t_info_ax_asc-info+121(16) = '0'.
        t_info_ax_asc-info+209(16) = '0'.
        lv_flag = ' '.
      ENDIF.

**Original Amount / Transaction amount
      lv_orig_amount = t_info_ax_asc-info+209(16).
      SHIFT lv_orig_amount LEFT DELETING LEADING '0'.
      lv_orig_amount = lv_orig_amount / 100.
      info_sap-umsbetrg = lv_orig_amount.
**Account / Local amount
      lv_amount = t_info_ax_asc-info+121(16).
      SHIFT lv_amount LEFT DELETING LEADING '0'.
      lv_amount = lv_amount / 100.
*      info_sap-umsbetrg = lv_amount.
*      CLEAR lv_amount.

      info_sap-umsnachk = 2.

* Get Currency based on ISO Code
*      READ TABLE git_tcurc INTO gwa_tcurc
*                 WITH KEY altwr = t_info_ax_asc-info+225(3).
      clear w_waers.
      lv_orig_curr = t_info_ax_asc-info+225(3).
      SELECT SINGLE waers from TCURC into w_waers
        WHERE ALTWR = lv_orig_curr.
*  IF sy-subrc = 0.
      IF sy-subrc = 0.
        info_sap-umswaehr = w_waers.
      ELSE.
        info_sap-umswaehr = gc_currency. " transaction currency
        if lv_orig_curr = '124'.
          info_sap-umswaehr = 'CAD'.
        ELSEIF lv_orig_curr = '840'.
          info_sap-umswaehr = 'USD'.
        endif.
      ENDIF.

* Exchange Rate
**  Default to 1.0000000 Conversion Rate
*      lv_exch = gc_exch.
      lv_exch = t_info_ax_asc-info+236(16).

      SHIFT lv_exch LEFT DELETING LEADING '0'.
      lv_exch_11 = lv_exch / 10000000.
      if lv_exch_11 is NOT INITIAL or
         lv_exch_11 <> '0'.
        lv_exch_11 = 1 / lv_exch_11.
      endif.
*      info_sap-ums_kurs = lv_exch_11.
********************
      split lv_exch_11 at '.' into lv_amt_a lv_amt_b.
      CONDENSE lv_amt_a NO-GAPS.
      CONDENSE lv_amt_b NO-GAPS.
      l1 = strlen( lv_amt_a ).
      l2 = strlen( lv_amt_b ).
      WHILE l1 < 4.
        CONCATENATE '0' lv_amt_a INTO lv_amt_a.
        l1 = l1 + 1.
      ENDWHILE.
      WHILE l2 < 7.
        CONCATENATE lv_amt_b '0' INTO lv_amt_b.
        l2 = l2 + 1.
      ENDWHILE.
      CONCATENATE lv_amt_a lv_amt_b INTO info_sap-ums_kurs.
******************
*      lv_amount = t_info_ax_asc-info+121(16).
*      SHIFT lv_amount LEFT DELETING LEADING '0'.
*      lv_amount = lv_amount / 100.

* BOI - VKAMATH - 12JAN12
*      IF t_info_ax_asc-info+120(1) = 'C'.
* This multiplication action was adding the 1 decimal place (.0)
* which in turn cause problem in total amount
*        lv_amount = lv_amount * -1.
*        lv_orig_amount = lv_orig_amount * -1.
*      ENDIF.
* EOI - VKAMATH - 12JAN12
      info_sap-ums_heim = lv_amount.
      info_sap-umstotal = lv_amount.
*      info_sap-ums_heim = lv_amount * lv_exch.
*      info_sap-umstotal = lv_amount * lv_exch.
      info_sap-addcount = '00'.
      info_sap-text_lang = t_info_ax_asc-info+161(25).
      info_sap-umsdoknr = t_info_ax_asc-info+399(17).

      MOVE info_sap TO t_info_sap_asc.
      APPEND t_info_sap_asc.

*      lv_total_amt = lv_total_amt + ( lv_amount * lv_exch ). "info_sap-umstotal.
*      lv_total_amt = lv_total_amt + lv_amount. "info_sap-umstotal.
      if t_info_ax_asc-info+120(1) = 'C'.
        gv_total_amt = gv_total_amt - lv_amount. "info_sap-umstotal.
      else.
        gv_total_amt = gv_total_amt + lv_amount.
      endif.
      ccc_sum-numberrecords = ccc_sum-numberrecords + 1.

      CLEAR: lv_amount, lv_exch, lv_orig_amount,
             gwa_pa0105, gwa_pa0001, lv_card_number, gwa_tcurc.

      IF p_prot = 'X'.
        conv_ok = conv_ok + 1.
        a_protokoll-data = info_ax.
        a_protokoll-conv_data = info_sap.
        a_protokoll-conv_status = 0.
        a_protokoll-index_dataset = index_dataset.
        APPEND a_protokoll TO t_protokoll.
        CLEAR last_color.
      ENDIF.

    ENDIF.

***************BOC********
*    AT LAST.
*      CLEAR  gv_total_amt .
*      gv_total_amt = abs( lv_total_amt ).
**      MOVE   lv_total_amt TO gv_total_amt .
*    ENDAT.
**************EOC*********

    CLEAR info_sap.
  ENDLOOP.

ENDFORM.                    "f_map_transaction_data

*&---------------------------------------------------------------------*
*&      Form  f_select_ztet_mcc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_select_ztet_mcc.

  SELECT mcc te_code te_type
         FROM ztet_mcc
         INTO TABLE git_ztet_mcc.

ENDFORM.                    "f_select_ztet_mcc

*&---------------------------------------------------------------------*
*&      Form  f_select_pa0105
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_select_pa0105.

  DATA: lit_card_number TYPE TABLE OF ty_card_number.

  lit_card_number = git_card_number.
  SORT lit_card_number BY card_number.
  DELETE ADJACENT DUPLICATES FROM lit_card_number
         COMPARING card_number.


  SELECT pernr subty objps sprps endda begda seqnr usrid
         FROM pa0105
         INTO TABLE git_pa0105
         FOR ALL ENTRIES IN lit_card_number
         WHERE usrid = lit_card_number-card_number AND
               usrty = '0011'.

ENDFORM.                    "f_select_pa0105

*&---------------------------------------------------------------------*
*&      Form  f_select_pa0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_select_pa0001.

  DATA: lit_pa0105 TYPE TABLE OF ty_pa0105.

  lit_pa0105 = git_pa0105.
  SORT lit_pa0105 BY pernr.
  DELETE ADJACENT DUPLICATES FROM lit_pa0105
         COMPARING pernr.

  SELECT pernr subty objps sprps endda begda seqnr ename
         FROM pa0001
         INTO TABLE git_pa0001
         FOR ALL ENTRIES IN lit_pa0105
         WHERE pernr = lit_pa0105-pernr.

ENDFORM.                    "f_select_pa0001

*&---------------------------------------------------------------------*
*&      Form  f_select_tcurc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_select_tcurc.

  IF git_addendum_0 IS NOT INITIAL.

*    SELECT waers isocd altwr
*           FROM tcurc
*           INTO TABLE git_tcurc
*           FOR ALL ENTRIES IN git_addendum_0
*           WHERE altwr = git_addendum_0-org_curr_code.

  ENDIF.

ENDFORM.                    "f_select_tcurc
