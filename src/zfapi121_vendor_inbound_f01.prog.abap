*&---------------------------------------------------------------------*
*&  Include           ZFAPI121_VENDOR_INBOUND_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFAPI121_VENDOR_INBOUND                       *
* Program Include    :   ZFAPI121_VENDOR_INBOUND_F01                   *
* Author             :                                                 *
* Date               :   Apr 15, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Vendor Inbound Interface                      *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 15-Apr-2018  CPALYAM     D30K929071-Initial development              *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_POPULATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_populate_data .

  FIELD-SYMBOLS: <lfs_vle>    TYPE gty_vle,
                 <lfs_ler>    TYPE gty_ler,
                 <lfs_ccd>    TYPE gty_ccd,
                 <lfs_pap>    TYPE gty_pap,
                 <lfs_cuw>    TYPE gty_cuw,
                 <lfs_pod>    TYPE gty_pod,
                 <lfs_sler>   TYPE gty_sler,
                 <lfs_field>.                     "Dynamic fields

  DATA: lv_tabix TYPE sytabix,
        lcl_line TYPE REF TO data,     "Work area
        lt_pap   TYPE gtt_pap,
        lv_recty TYPE c LENGTH 2.

  CLEAR: gt_vle[],
         gt_ler[],
         gt_ccd[],
         gt_pap[],
         gt_pap_d[],
         gt_pap_c[],
         gt_pap_cd[],
         gt_cuw[],
         gt_pod[],
         gt_sler[].

  IF NOT gt_tab_data[] IS INITIAL.
    LOOP AT gt_tab_data INTO gs_tab_data.

      CLEAR: lv_tabix,
             gt_data[],
             lv_recty.

      SPLIT gs_tab_data AT '|~' INTO TABLE gt_data.
      IF NOT gt_data[] IS INITIAL.
        READ TABLE gt_data INTO gs_data INDEX 3.
        IF sy-subrc EQ 0.
          lv_recty = gs_data.
        ENDIF.
      ENDIF.

      CLEAR: gt_data[],
             gs_data.

      lv_tabix = 1.

*      CASE gs_tab_data+20(2).
      CASE lv_recty.
        WHEN '01'.
          CREATE DATA lcl_line LIKE LINE OF gt_vle.
          ASSIGN lcl_line->* TO <lfs_vle>.
          SPLIT gs_tab_data AT '|~' INTO TABLE gt_data.
          LOOP AT gt_data INTO gs_data.
            IF sy-tabix GT 49.
              EXIT.
            ENDIF.
            CLEAR lv_tabix.
            lv_tabix = sy-tabix.
            ASSIGN COMPONENT lv_tabix OF STRUCTURE <lfs_vle> TO <lfs_field>.
            <lfs_field> = gs_data.
          ENDLOOP.
          APPEND <lfs_vle> TO gt_vle.
          CLEAR <lfs_vle>.

        WHEN '02'.
          CREATE DATA lcl_line LIKE LINE OF gt_ler.
          ASSIGN lcl_line->* TO <lfs_ler>.
          SPLIT gs_tab_data AT '|~' INTO TABLE gt_data.
          LOOP AT gt_data INTO gs_data.
            IF ( sy-tabix GT 5 ) AND ( sy-tabix LT 50 ).
              CONTINUE.
            ELSEIF sy-tabix GT 64.
              EXIT.
            ENDIF.

            ASSIGN COMPONENT lv_tabix OF STRUCTURE <lfs_ler> TO <lfs_field>.
            <lfs_field> = gs_data.
            lv_tabix = lv_tabix + 1.
          ENDLOOP.
          APPEND <lfs_ler> TO gt_ler.
          CLEAR <lfs_ler>.

        WHEN '03'.
          CREATE DATA lcl_line LIKE LINE OF gt_ccd.
          ASSIGN lcl_line->* TO <lfs_ccd>.
          SPLIT gs_tab_data AT '|~' INTO TABLE gt_data.
          LOOP AT gt_data INTO gs_data.
            IF ( sy-tabix GT 5 ) AND ( sy-tabix LT 65 ).
              CONTINUE.
            ELSEIF sy-tabix GT 84.
              EXIT.
            ENDIF.

            ASSIGN COMPONENT lv_tabix OF STRUCTURE <lfs_ccd> TO <lfs_field>.
            <lfs_field> = gs_data.
            lv_tabix = lv_tabix + 1.
          ENDLOOP.
          APPEND <lfs_ccd> TO gt_ccd.
          CLEAR <lfs_ccd>.

        WHEN '04'.
          CREATE DATA lcl_line LIKE LINE OF gt_pap.
          ASSIGN lcl_line->* TO <lfs_pap>.
          SPLIT gs_tab_data AT '|~' INTO TABLE gt_data.
          LOOP AT gt_data INTO gs_data.
            IF ( sy-tabix GT 5 ) AND ( sy-tabix LT 85 ) AND ( sy-tabix NE 65 ). "Consider Repeated Fields
              CONTINUE.
            ELSEIF sy-tabix GT 86.
              EXIT.
            ENDIF.

            ASSIGN COMPONENT lv_tabix OF STRUCTURE <lfs_pap> TO <lfs_field>.
            <lfs_field> = gs_data.
            lv_tabix = lv_tabix + 1.
          ENDLOOP.
          APPEND <lfs_pap> TO gt_pap.
          CLEAR <lfs_pap>.

        WHEN '05'.
          CREATE DATA lcl_line LIKE LINE OF gt_cuw.
          ASSIGN lcl_line->* TO <lfs_cuw>.
          SPLIT gs_tab_data AT '|~' INTO TABLE gt_data.
          LOOP AT gt_data INTO gs_data.
            IF ( sy-tabix GT 5 ) AND ( sy-tabix LT 87 ) AND ( sy-tabix NE 65 )."Consider Repeated Fields
              CONTINUE.
            ELSEIF sy-tabix GT 92.
              EXIT.
            ENDIF.

            ASSIGN COMPONENT lv_tabix OF STRUCTURE <lfs_cuw> TO <lfs_field>.
            <lfs_field> = gs_data.
            lv_tabix = lv_tabix + 1.
          ENDLOOP.
          APPEND <lfs_cuw> TO gt_cuw.
          CLEAR <lfs_cuw>.

        WHEN '06'.
          CREATE DATA lcl_line LIKE LINE OF gt_pod.
          ASSIGN lcl_line->* TO <lfs_pod>.
          SPLIT gs_tab_data AT '|~' INTO TABLE gt_data.
          LOOP AT gt_data INTO gs_data.
            IF ( sy-tabix GT 5 ) AND ( sy-tabix LT 93 ).
              CONTINUE.
            ELSEIF sy-tabix GT 104.
              EXIT.
            ENDIF.

            ASSIGN COMPONENT lv_tabix OF STRUCTURE <lfs_pod> TO <lfs_field>.
            <lfs_field> = gs_data.
            lv_tabix = lv_tabix + 1.
          ENDLOOP.
          APPEND <lfs_pod> TO gt_pod.
          CLEAR <lfs_pod>.

        WHEN '07'.
          CREATE DATA lcl_line LIKE LINE OF gt_sler.
          ASSIGN lcl_line->* TO <lfs_sler>.
          SPLIT gs_tab_data AT '|~' INTO TABLE gt_data.
          LOOP AT gt_data INTO gs_data.
            IF ( sy-tabix GT 5 ) AND ( sy-tabix LT 105 ) AND ( sy-tabix NE 93 ). "Consider Repeated Fields
              CONTINUE.
            ELSEIF sy-tabix GT 109.
              EXIT.
            ENDIF.

            ASSIGN COMPONENT lv_tabix OF STRUCTURE <lfs_sler> TO <lfs_field>.
            <lfs_field> = gs_data.
            lv_tabix = lv_tabix + 1.
          ENDLOOP.
          APPEND <lfs_sler> TO gt_sler.
          CLEAR <lfs_sler>.
      ENDCASE.
    ENDLOOP.

    LOOP AT gt_vle INTO gs_vle.
      CLEAR gs_kdata.
      MOVE: gs_vle-zz_iap_vendor_id TO gs_kdata-zz_iap_vendor_id,
            gs_vle-lifnr            TO gs_kdata-lifnr.
      APPEND gs_kdata TO gt_kdata.
    ENDLOOP.

    SORT gt_kdata BY zz_iap_vendor_id.
    DELETE ADJACENT DUPLICATES FROM gt_kdata COMPARING zz_iap_vendor_id.

    SORT: gt_vle BY zz_iap_vendor_id,
          gt_ler BY zz_iap_vendor_id,
          gt_ccd BY zz_iap_vendor_id,
          gt_pap BY zz_iap_vendor_id,
          gt_cuw BY zz_iap_vendor_id,
          gt_pod BY zz_iap_vendor_id,
          gt_sler BY zz_iap_vendor_id.

* To seperate the records to be deleted
    CLEAR lt_pap[].
    LOOP AT gt_pap INTO gs_pap.
      IF gs_pap-bukrs IS INITIAL." To Check whether company code level or not
* From File A - Active, I - Inactive
        IF gs_pap-chngind_lfza EQ 'A'.
          APPEND gs_pap TO lt_pap.
        ELSE.
          APPEND gs_pap TO gt_pap_d.
        ENDIF.
      ELSE.
* From File A - Active, I - Inactive
        IF gs_pap-chngind_lfza EQ 'A'.
          APPEND gs_pap TO gt_pap_c.
        ELSE.
          APPEND gs_pap TO gt_pap_cd.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CLEAR gt_pap[].
    gt_pap[] = lt_pap[].
    SORT: gt_pap BY zz_iap_vendor_id,
          gt_pap_d BY zz_iap_vendor_id,
          gt_pap_c BY zz_iap_vendor_id,
          gt_pap_cd BY zz_iap_vendor_id.

    FREE lt_pap[].

  ENDIF.

ENDFORM.                    " F_POPULATE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_prepare_data .

  DATA: ls_vendors      TYPE vmds_ei_extern,
        lt_vendors      TYPE vmds_ei_extern_t,
        ls_header       TYPE vmds_ei_header,
        ls_central_data TYPE vmds_ei_central_data,
        ls_company      TYPE vmds_ei_company,
        lt_company      TYPE vmds_ei_company_t,
        ls_company_data TYPE vmds_ei_vmd_company,
        ls_address      TYPE cvis_ei_address1,
        ls_central      TYPE vmds_ei_vmd_central,
        lt_contacts     TYPE vmds_ei_contacts_t,
        ls_contacts     TYPE vmds_ei_contacts,
        lt_phone        TYPE cvis_ei_phone_t,
        ls_phone        TYPE cvis_ei_phone_str,
        lt_fax          TYPE cvis_ei_fax_t,
        ls_fax          TYPE cvis_ei_fax_str,
        lt_smtp         TYPE cvis_ei_smtp_t,
        ls_smtp         TYPE cvis_ei_smtp_str,
        ls_communication TYPE cvis_ei_cvi_communication,
        lt_bankdetails  TYPE cvis_ei_bankdetail_t,
        ls_bankdetails  TYPE cvis_ei_cvi_bankdetail,
        lt_wtax_type    TYPE vmds_ei_wtax_type_t,
        ls_wtax_type    TYPE vmds_ei_wtax_type,
        ls_purchasing_data TYPE vmds_ei_vmd_purchasing,
        lt_purchasing   TYPE vmds_ei_purchasing_t,
        ls_purchasing   TYPE vmds_ei_purchasing,
        lt_functions    TYPE vmds_ei_functions_t,
        ls_functions    TYPE vmds_ei_functions,
        ls_vmds_extern  TYPE vmds_ei_main,
        ls_vmds_succ    TYPE vmds_ei_main,
        ls_vmds_error   TYPE vmds_ei_main,
        ls_err_messages TYPE cvis_message,
        ls_succ_messages TYPE cvis_message,
        lv_cvendor      TYPE lifnr, "Created Vendor
        lv_tabix        TYPE sytabix,
        lt_sler         TYPE STANDARD TABLE OF gty_sler,
        ls_sler         TYPE gty_sler,
        lv_vend_st      TYPE c. "Status of Vendor Create/Change

  LOOP AT gt_kdata INTO gs_kdata.

* To check whether the data is for Create/Change
    CLEAR lv_vend_st.
    IF NOT gs_kdata-lifnr IS INITIAL.
      lv_vend_st = gc_x.
    ENDIF.

* Clearing Flag variables
    CLEAR: gv_trns,
           gv_errr,
           gv_wrng,
           gv_pass.

    CLEAR gs_vle.
    READ TABLE gt_vle INTO gs_vle WITH KEY
                                  zz_iap_vendor_id = gs_kdata-zz_iap_vendor_id
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_header-object_instance-lifnr = gs_vle-lifnr.

*   M - Create/Change, I - Create, U - Change

      ls_header-object_task = 'M'.

      ls_address-task = 'M'.

      ls_address-postal-data-name = gs_vle-name1.
      ls_address-postal-datax-name = gc_x.

      ls_address-postal-data-name_2 = gs_vle-name2.
      ls_address-postal-datax-name_2 = gc_x.

      ls_address-postal-data-sort1 = gs_vle-mcod1.
      ls_address-postal-datax-sort1 = gc_x.

      ls_address-postal-data-sort2 = gs_vle-mcod2.
      ls_address-postal-datax-sort2 = gc_x.

      ls_central-data-ktokk = gs_vle-ktokk.
      ls_central-datax-ktokk = gc_x.

      ls_central-data-begru = gs_vle-begru.
      ls_central-datax-begru = gc_x.

      ls_central-data-konzs = gs_vle-konzs.
      ls_central-datax-konzs = gc_x.

      ls_central-data-bahns = gs_vle-bahns.
      ls_central-datax-bahns = gc_x.

*** Updating Custom Fields by appending the respective structures
      ls_central-data-zz_iap_create_dt = sy-datum.
      ls_central-datax-zz_iap_create_dt = gc_x.

      ls_central-data-zz_iap_ritm_id = gs_vle-zz_iap_ritm_id.
      ls_central-datax-zz_iap_ritm_id = gc_x.


      ls_central-data-zz_iap_vendor_id = gs_vle-zz_iap_vendor_id.
      ls_central-datax-zz_iap_vendor_id = gc_x.


      ls_address-postal-data-country = gs_vle-land1.
      ls_address-postal-datax-country = gc_x.

* Populate Contact Data 1 - AP Remittance Contact
      CLEAR: lt_contacts[],
             lt_phone[],
             lt_fax[],
             lt_smtp[],
             ls_contacts.

      ls_contacts-task = 'M'.
      ls_contacts-address_type_3-task = 'M'.

      ls_contacts-address_type_3-postal-data-firstname  = gs_vle-namev_ap.
      ls_contacts-address_type_3-postal-datax-firstname  = gc_x.

      ls_contacts-address_type_3-postal-data-lastname  = gs_vle-name1_ap.
      ls_contacts-address_type_3-postal-datax-lastname  = gc_x.

      ls_contacts-data-abtnr = gs_vle-abtnr_ap.
      ls_contacts-datax-abtnr = gc_x.

      ls_contacts-data-pafkt = gs_vle-pafkt_ap.
      ls_contacts-datax-pafkt = gc_x.

* I - Create, U - Change, D - Delete
      CLEAR ls_phone.
      IF lv_vend_st IS INITIAL.
        ls_phone-contact-task = 'I'.
      ELSE.
        ls_phone-contact-task = 'U'.
      ENDIF.
      ls_phone-contact-data-telephone = gs_vle-telf1_ap.
      ls_phone-contact-datax-telephone = gc_x.


      CLEAR ls_fax.
      IF lv_vend_st IS INITIAL.
        ls_fax-contact-task = 'I'.
      ELSE.
        ls_fax-contact-task = 'U'.
      ENDIF.
      ls_fax-contact-data-fax = gs_vle-fax_ap.
      ls_fax-contact-datax-fax = gc_x.


      CLEAR ls_smtp.
      IF lv_vend_st IS INITIAL.
        ls_smtp-contact-task = 'I'.
      ELSE.
        ls_smtp-contact-task = 'U'.
      ENDIF.
      ls_smtp-contact-data-e_mail = gs_vle-smtp_ap.
      ls_smtp-contact-datax-e_mail = gc_x.

      APPEND ls_phone TO lt_phone.
      APPEND ls_fax TO lt_fax.
      APPEND ls_smtp TO lt_smtp.

      ls_contacts-address_type_3-communication-phone-phone = lt_phone[].
      ls_contacts-address_type_3-communication-fax-fax = lt_fax[].
      ls_contacts-address_type_3-communication-smtp-smtp = lt_smtp[].

      APPEND ls_contacts TO lt_contacts.

* Populate Contact Data 1 - Supply Chain Contact

      CLEAR ls_contacts.
      ls_contacts-task = 'M'.
      ls_contacts-address_type_3-task = 'M'.

      ls_contacts-address_type_3-postal-data-firstname  = gs_vle-namev_sc.
      ls_contacts-address_type_3-postal-datax-firstname  = gc_x.

      ls_contacts-address_type_3-postal-data-lastname  = gs_vle-name1_sc.
      ls_contacts-address_type_3-postal-datax-lastname  = gc_x.

      ls_contacts-data-abtnr = gs_vle-abtnr_sc.
      ls_contacts-datax-abtnr = gc_x.

      ls_contacts-data-pafkt = gs_vle-pafkt_sc.
      ls_contacts-datax-pafkt = gc_x.

      CLEAR ls_phone.
      IF lv_vend_st IS INITIAL.
        ls_phone-contact-task = 'I'.
      ELSE.
        ls_phone-contact-task = 'U'.
      ENDIF.
      ls_phone-contact-data-telephone = gs_vle-telf1_sc.
      ls_phone-contact-datax-telephone = gc_x.


      CLEAR ls_fax.
      IF lv_vend_st IS INITIAL.
        ls_fax-contact-task = 'I'.
      ELSE.
        ls_fax-contact-task = 'U'.
      ENDIF.
      ls_fax-contact-data-fax = gs_vle-fax_sc.
      ls_fax-contact-datax-fax = gc_x.


      CLEAR ls_smtp.
      IF lv_vend_st IS INITIAL.
        ls_smtp-contact-task = 'I'.
      ELSE.
        ls_smtp-contact-task = 'U'.
      ENDIF.
      ls_smtp-contact-data-e_mail = gs_vle-smtp_sc.
      ls_smtp-contact-datax-e_mail = gc_x.

      APPEND ls_phone TO lt_phone.
      APPEND ls_fax TO lt_fax.
      APPEND ls_smtp TO lt_smtp.


      ls_contacts-address_type_3-communication-phone-phone = lt_phone[].
      ls_contacts-address_type_3-communication-fax-fax = lt_fax[].
      ls_contacts-address_type_3-communication-smtp-smtp = lt_smtp[].

      APPEND ls_contacts TO lt_contacts.

      CLEAR: ls_communication,
             lt_phone[],
             ls_phone.

      IF lv_vend_st IS INITIAL.
        ls_phone-contact-task = 'I'.
      ELSE.
        ls_phone-contact-task = 'U'.
      ENDIF.
      ls_phone-contact-data-telephone = gs_vle-telf1.
      ls_phone-contact-datax-telephone = gc_x.

      ls_phone-contact-data-extension = gs_vle-extn_tel.
      ls_phone-contact-datax-extension = gc_x.
      APPEND ls_phone TO lt_phone.

      CLEAR: lt_fax[],
             ls_fax.

      IF lv_vend_st IS INITIAL.
        ls_fax-contact-task = 'I'.
      ELSE.
        ls_fax-contact-task = 'U'.
      ENDIF.
      ls_fax-contact-data-fax = gs_vle-telfx.
      ls_fax-contact-datax-fax = gc_x.

      ls_fax-contact-data-extension = gs_vle-extn_fax.
      ls_fax-contact-datax-extension = gc_x.
      APPEND ls_fax TO lt_fax.

      ls_communication-phone-phone = lt_phone[].
      ls_communication-fax-fax = lt_fax[].


      ls_central-data-stcd1 = gs_vle-stcd1.
      ls_central-datax-stcd1 = gc_x.

      ls_central-data-stcd2 = gs_vle-stcd2.
      ls_central-datax-stcd2 = gc_x.

      ls_central-data-loevm = gs_vle-loevm.
      ls_central-datax-loevm = gc_x.

      ls_central-data-sperr = gs_vle-sperr.
      ls_central-datax-sperr = gc_x.

      ls_central-data-sperm = gs_vle-sperm.
      ls_central-datax-sperm = gc_x.

      ls_address-postal-data-location = gs_vle-name1_adrc.
      ls_address-postal-datax-location = gc_x.

      ls_address-postal-data-street = gs_vle-street.
      ls_address-postal-datax-street = gc_x.

      ls_address-postal-data-str_suppl2 = gs_vle-str_suppl2.
      ls_address-postal-datax-str_suppl2 = gc_x.

      ls_address-postal-data-city = gs_vle-city1.
      ls_address-postal-datax-city = gc_x.

      ls_address-postal-data-region = gs_vle-region.
      ls_address-postal-datax-region = gc_x.

      ls_address-postal-data-postl_cod1 = gs_vle-post_code1.
      ls_address-postal-datax-postl_cod1 = gc_x.

*      ls_address-postal-data-po_box = gs_vle-po_box.
*      ls_address-postal-datax-po_box = gc_x.
*
*      ls_address-postal-data-postl_cod2 = gs_vle-post_code2.
*      ls_address-postal-datax-postl_cod2 = gc_x.
*
*      ls_address-postal-data-postl_cod3 = gs_vle-post_code3.
*      ls_address-postal-datax-postl_cod3 = gc_x.

      ls_address-postal-data-country = gs_vle-country.
      ls_address-postal-datax-country = gc_x.

      ls_central-data-lnrza = gs_vle-lnrza.
      ls_central-datax-lnrza = gc_x.

      IF NOT gs_vle-xlfza IS INITIAL.
*   Proceed for Alternate Payee written below
      ENDIF.

      CLEAR gs_ler.
      READ TABLE gt_ler INTO gs_ler WITH KEY
                                  zz_iap_vendor_id = gs_kdata-zz_iap_vendor_id
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR ls_bankdetails.

* From File A - Active, I - Inactive
        IF gs_ler-chngind_lfbk EQ 'A'.
          ls_bankdetails-task = 'M'. "Deletion is possible by passing 'D'
        ELSE.
          ls_bankdetails-task = 'D'.
        ENDIF.

        ls_bankdetails-data-bvtyp = gs_ler-bvtyp.
        ls_bankdetails-datax-bvtyp = gc_x.

        ls_bankdetails-data_key-banks = gs_ler-banks.
        ls_bankdetails-data_key-bankl = gs_ler-bankl.
        ls_bankdetails-data_key-bankn = gs_ler-bankn.

        ls_bankdetails-data-bkref = gs_ler-bkref.
        ls_bankdetails-datax-bkref = gc_x.

        ls_bankdetails-data-bkont = gs_ler-bkont.
        ls_bankdetails-datax-bkont = gc_x.

        APPEND ls_bankdetails TO lt_bankdetails.

      ENDIF.

      CLEAR gs_ccd.
      READ TABLE gt_ccd INTO gs_ccd WITH KEY
                                  zz_iap_vendor_id = gs_kdata-zz_iap_vendor_id
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR ls_company_data.

        ls_company-task = 'M'.

        ls_company-data_key-bukrs = gs_ccd-bukrs.

        ls_company-data-zterm = gs_ccd-zterm_lfb1.
        ls_company-datax-zterm = gc_x.

        ls_company-data-zwels = gs_ccd-zwels.
        ls_company-datax-zwels = gc_x.

        ls_company-data-loevm = gs_ccd-loevm_lfb1.
        ls_company-datax-loevm = gc_x.

        ls_company-data-sperr = gs_ccd-sperr_lfb1.
        ls_company-datax-sperr = gc_x.

        ls_company-data-zahls = gs_ccd-zahls.
        ls_company-datax-zahls = gc_x.

        ls_company-data-busab = gs_ccd-busab.
        ls_company-datax-busab = gc_x.

        ls_company-data-akont = gs_ccd-akont.
        ls_company-datax-akont = gc_x.

        ls_company-data-zuawa = gs_ccd-zuawa.
        ls_company-datax-zuawa = gc_x.

        ls_company-data-fdgrv = gs_ccd-fdgrv.
        ls_company-datax-fdgrv = gc_x.

        ls_company-data-mindk = gs_ccd-mindk. "Some validation needed
        ls_company-datax-mindk = gc_x.

        ls_company-data-cerdt = gs_ccd-cerdt.
        ls_company-datax-cerdt = gc_x.

        ls_company-data-zindt = gs_ccd-zindt.
        ls_company-datax-zindt = gc_x.

        ls_company-data-reprf = gs_ccd-reprf.
        ls_company-datax-reprf = gc_x.

        ls_company-data-lnrzb = gs_ccd-lnrzb.
        ls_company-datax-lnrzb = gc_x.

        ls_company-data-hbkid = gs_ccd-hbkid.
        ls_company-datax-hbkid = gc_x.

        ls_company-data-xpore = gs_ccd-xpore.
        ls_company-datax-xpore = gc_x.

        ls_company-data-uzawe = gs_ccd-uzawe. "Some validation needed
        ls_company-datax-uzawe = gc_x.

        ls_company-data-qland = gs_ccd-qland.
        ls_company-datax-qland = gc_x.

        CLEAR gs_cuw.
        READ TABLE gt_cuw INTO gs_cuw WITH KEY
                                  zz_iap_vendor_id = gs_kdata-zz_iap_vendor_id
                                  BINARY SEARCH.
        IF sy-subrc EQ 0.
          CLEAR ls_wtax_type.
* From File A - Active, I - Inactive
          IF gs_cuw-chngind_lfbw EQ 'A'.
            ls_wtax_type-task = 'M'. "Deletion is possible by passing 'D'
          ELSE.
            ls_wtax_type-task = 'D'.
          ENDIF.

          ls_wtax_type-data_key-witht = gs_cuw-witht.

          ls_wtax_type-data-wt_withcd = gs_cuw-wt_withcd.
          ls_wtax_type-datax-wt_withcd = gc_x.

          ls_wtax_type-data-wt_subjct = gs_cuw-wt_subjct.
          ls_wtax_type-datax-wt_subjct = gc_x.

          ls_wtax_type-data-qsrec = gs_cuw-qsrec.
          ls_wtax_type-datax-qsrec = gc_x.

          ls_wtax_type-data-wt_wtstcd = gs_cuw-wt_wtstcd.
          ls_wtax_type-datax-wt_wtstcd = gc_x.

          APPEND ls_wtax_type TO lt_wtax_type.

        ENDIF.

        ls_company-wtax_type-wtax_type = lt_wtax_type.

        APPEND ls_company TO lt_company.

      ENDIF.

      CLEAR gs_pod.
      READ TABLE gt_pod INTO gs_pod WITH KEY
                                  zz_iap_vendor_id = gs_kdata-zz_iap_vendor_id
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR: ls_purchasing_data,
               lt_purchasing[],
               ls_purchasing.

        ls_purchasing-task = 'M'.

        ls_purchasing-data_key-ekorg = gs_pod-ekorg.

        ls_purchasing-data-sperm = gs_pod-sperm_lfm1.
        ls_purchasing-datax-sperm = gc_x.

        ls_purchasing-data-loevm = gs_pod-loevm_lfm1.
        ls_purchasing-datax-loevm = gc_x.

        ls_purchasing-data-waers = gs_pod-waers_lfm1. " Validation is required
        ls_purchasing-datax-waers = gc_x.

        ls_purchasing-data-zterm = gs_pod-zterm_lfm1. " Validation is required
        ls_purchasing-datax-zterm = gc_x.

        ls_purchasing-data-inco1 = gs_pod-inco1.      " Validation is required
        ls_purchasing-datax-inco1 = gc_x.

        ls_purchasing-data-inco2 = gs_pod-inco2.
        ls_purchasing-datax-inco2 = gc_x.

        ls_purchasing-data-kalsk = gs_pod-kalsk. " Some validation needed
        ls_purchasing-datax-kalsk = gc_x.

        ls_purchasing-data-meprf = gs_pod-meprf. " Some validation needed
        ls_purchasing-datax-meprf = gc_x.

        ls_purchasing-data-eikto = gs_pod-eikto.
        ls_purchasing-datax-eikto = gc_x.

        ls_purchasing-data-webre = gs_pod-webre.
        ls_purchasing-datax-webre = gc_x.

        ls_purchasing-data-kzaut = gs_pod-kzaut.
        ls_purchasing-datax-kzaut = gc_x.

        CLEAR gs_sler.
        READ TABLE gt_sler INTO gs_sler WITH KEY
                                  zz_iap_vendor_id = gs_kdata-zz_iap_vendor_id
                                  BINARY SEARCH.
        IF sy-subrc EQ 0.
          CLEAR: lt_functions[],
                 ls_functions,
                 lt_sler[],
                 ls_sler,
                 lv_tabix.

          lv_tabix = sy-tabix.
          lt_sler[] = gt_sler[].
          LOOP AT lt_sler INTO ls_sler FROM lv_tabix.
            IF ls_sler-zz_iap_vendor_id NE gs_sler-zz_iap_vendor_id.
              EXIT.
            ENDIF.
* From File A - Active, I - Inactive
            IF ls_sler-chngind_wyt3 EQ 'A'.
              ls_functions-task = 'M'. "Deletion is possible by passing D
            ELSE.
              ls_functions-task = 'D'.
            ENDIF.

            ls_functions-data_key-parvw = ls_sler-parvw.
            ls_functions-data_key-parza = ls_sler-parza.

            ls_functions-data-partner = ls_sler-lifn2.
            ls_functions-datax-partner = gc_x.

            ls_functions-data-defpa = ls_sler-defpa.
            ls_functions-datax-defpa = gc_x.

            APPEND ls_functions TO lt_functions.

          ENDLOOP.
          ls_purchasing-functions-functions = lt_functions.

        ENDIF.

        APPEND ls_purchasing TO lt_purchasing.
      ENDIF.

      ls_address-communication = ls_communication.

      ls_central_data-central = ls_central.
      ls_central_data-address = ls_address.
      ls_central_data-contact-contacts = lt_contacts.

      ls_central_data-bankdetail-bankdetails = lt_bankdetails.

      ls_company_data-company = lt_company.

      CLEAR ls_vendors.
      ls_vendors-header = ls_header.
      ls_vendors-central_data = ls_central_data.
      ls_vendors-company_data = ls_company_data.

* Sending Purchasing information only a change record
      IF NOT ls_header-object_instance-lifnr IS INITIAL.
        ls_purchasing_data-purchasing = lt_purchasing.
        ls_vendors-purchasing_data = ls_purchasing_data.
      ENDIF.

      APPEND ls_vendors TO lt_vendors.

    ENDIF.

    IF NOT lt_vendors[] IS INITIAL.
*   Initialize all the data
      vmd_ei_api=>initialize( ).

      CLEAR: ls_vmds_extern,
             ls_vmds_succ,
             ls_succ_messages,
             ls_vmds_error,
             ls_err_messages.

      ls_vmds_extern-vendors = lt_vendors[].

*   Call the Method for creation of Vendor.
      CALL METHOD vmd_ei_api=>maintain_bapi
        EXPORTING
          is_master_data           = ls_vmds_extern
        IMPORTING
          es_master_data_correct   = ls_vmds_succ
          es_message_correct       = ls_succ_messages
          es_master_data_defective = ls_vmds_error
          es_message_defective     = ls_err_messages.

      IF ls_err_messages-is_error IS INITIAL.
        COMMIT WORK.
        CLEAR: lv_cvendor,
               lt_vendors[].
        lt_vendors[] = ls_vmds_succ-vendors.
        READ TABLE lt_vendors INTO ls_vendors INDEX 1.
        IF sy-subrc EQ 0.
          CLEAR ls_header.
          ls_header = ls_vendors-header.
          lv_cvendor = ls_header-object_instance-lifnr.

          IF NOT gs_vle-xlfza IS INITIAL. "Check whether alternate payee exists
* To check whether the alternate payee at Company Code Level or Vendor Level

            IF NOT gt_pap_c IS INITIAL OR
               NOT gt_pap_cd IS INITIAL.
              PERFORM f_upd_palt_cc USING gs_kdata-zz_iap_vendor_id
                                         lv_cvendor.
              PERFORM f_del_palt_cc USING gs_kdata-zz_iap_vendor_id
                                         lv_cvendor.
            ENDIF.
            IF NOT gt_pap IS INITIAL OR
               NOT gt_pap_d IS INITIAL.
*            ELSE.
              PERFORM f_upd_palt_pay USING gs_kdata-zz_iap_vendor_id
                                         lv_cvendor.
              PERFORM f_del_palt_pay USING gs_kdata-zz_iap_vendor_id
                                         lv_cvendor.
            ENDIF.
*            ENDIF.
          ENDIF.
* To update the bank chains
          PERFORM f_upd_bank_ch USING gs_kdata-zz_iap_vendor_id.

          PERFORM f_upd_errors USING ls_succ_messages
                                     lv_cvendor.

* Sending Purchasing information only for a new record created
          IF ls_vendors-purchasing_data IS INITIAL.

*** To update the purchasing data
            CLEAR: lt_vendors[],
                   ls_vendors,
                   ls_header.
            ls_header-object_instance-lifnr = lv_cvendor.
            ls_header-object_task = 'M'.

            ls_vendors-header = ls_header.

            ls_purchasing_data-purchasing = lt_purchasing.
            ls_vendors-purchasing_data = ls_purchasing_data.

            APPEND ls_vendors TO lt_vendors.

            IF NOT lt_vendors[] IS INITIAL.
*   Initialize all the data
              vmd_ei_api=>initialize( ).

              CLEAR: ls_vmds_extern,
                     ls_vmds_succ,
                     ls_succ_messages,
                     ls_vmds_error,
                     ls_err_messages.

              ls_vmds_extern-vendors = lt_vendors[].

*   Call the Method for creation of Vendor.
              CALL METHOD vmd_ei_api=>maintain_bapi
                EXPORTING
                  is_master_data           = ls_vmds_extern
                IMPORTING
                  es_master_data_correct   = ls_vmds_succ
                  es_message_correct       = ls_succ_messages
                  es_master_data_defective = ls_vmds_error
                  es_message_defective     = ls_err_messages.

              IF ls_err_messages-is_error IS INITIAL.
                COMMIT WORK.
                CLEAR: lv_cvendor,
                       lt_vendors[].
                lt_vendors[] = ls_vmds_succ-vendors.
                READ TABLE lt_vendors INTO ls_vendors INDEX 1.
                IF sy-subrc EQ 0.
                  CLEAR ls_header.
                  ls_header = ls_vendors-header.
                  lv_cvendor = ls_header-object_instance-lifnr.
                  PERFORM f_upd_errors USING ls_succ_messages
                                             lv_cvendor.
                ENDIF.
              ELSE.
                PERFORM f_upd_errors USING ls_err_messages
                                           lv_cvendor.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        PERFORM f_upd_errors USING ls_err_messages
                                   lv_cvendor.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_PREPARE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_UPD_PALT_PAY
*&---------------------------------------------------------------------*
*       To update permitted alternate payee
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_upd_palt_pay USING iv_iap_vendor_id     TYPE z_iap_vendor_id
                          iv_lifnr             TYPE lifnr.

  DATA: ls_opt        TYPE ctu_params,
        lv_tabix      TYPE sytabix,
        ls_pap        TYPE gty_pap,
        lt_pap        TYPE STANDARD TABLE OF gty_pap,
        lv_lines      TYPE i,
        lv_mod        TYPE i,
        lv_div        TYPE i,
        lv_cline      TYPE numc2,
        lv_fvar       TYPE fnam_____4.

  CONSTANTS lc_xk02(4) TYPE c VALUE 'XK02'.

  CLEAR: gt_bdcdata[],
         gt_messages[],
         gs_pap,
         ls_pap,
         ls_opt.

  ls_opt-dismode = 'N'.
  ls_opt-defsize = gc_x.
  ls_opt-updmode = 'S'.

  READ TABLE gt_pap INTO ls_pap WITH KEY zz_iap_vendor_id = iv_iap_vendor_id.
  IF sy-subrc EQ 0.
    CLEAR lv_tabix.
    lv_tabix = sy-tabix.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0101'.
    PERFORM f_bdc_field  USING 'RF02K-LIFNR'   iv_lifnr.
    PERFORM f_bdc_field  USING 'RF02K-BUKRS'   ls_pap-bukrs.
    PERFORM f_bdc_field  USING 'RF02K-D0130'   'X'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '/00'.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0130'.
    PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFBK-BANKS(01)'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=LFZA'.

    CLEAR: lt_pap[],
           gt_lfza[],
           lv_lines.

    lt_pap[] = gt_pap[].
    SORT lt_pap BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_pap COMPARING lifnr.

    SELECT empfk FROM lfza
                 INTO TABLE gt_lfza
                 FOR ALL ENTRIES IN lt_pap
                 WHERE lifnr = lt_pap-lifnr
                 AND   bukrs = ''.
    IF sy-subrc EQ 0.
      SORT gt_lfza.
      DESCRIBE TABLE gt_lfza LINES lv_lines.
    ENDIF.

    lv_div = lv_lines DIV 7.
    lv_mod = lv_lines MOD 7.
    lv_cline = lv_mod + 1.

    DO lv_div TIMES.
      PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
      PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFZA-EMPFK(01)'.
      PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=P+'.
    ENDDO.

    LOOP AT gt_pap INTO gs_pap FROM lv_tabix.
      IF gs_pap-zz_iap_vendor_id NE ls_pap-zz_iap_vendor_id.
        EXIT.
      ENDIF.

      IF lv_cline LE 7.

        CONCATENATE 'LFZA-EMPFK(' lv_cline ')' INTO lv_fvar.
        PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
        PERFORM f_bdc_field  USING 'BDC_CURSOR'    lv_fvar."'LFZA-EMPFK(01)'.
        PERFORM f_bdc_field  USING 'BDC_OKCODE'    '/00'.
        PERFORM f_bdc_field  USING lv_fvar         gs_pap-empfk.
        lv_cline = lv_cline + 1.
      ENDIF.

      IF lv_cline GT 7.
        PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
        PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFZA-EMPFK(01)'.
        PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=P+'.
        lv_cline = 1.
      ENDIF.

    ENDLOOP.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
    PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFZA-EMPFK(01)'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=BACK'.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0130'.
    PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFBK-BANKS(01)'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=UPDA'.

  ENDIF.

  IF NOT gt_bdcdata[] IS INITIAL.

    CALL TRANSACTION lc_xk02 USING         gt_bdcdata
                             OPTIONS FROM  ls_opt
                             MESSAGES INTO gt_messages.

    PERFORM f_bdc_errors USING iv_lifnr.

  ENDIF.
ENDFORM.                    " F_UPD_PALT_PAY
*&---------------------------------------------------------------------*
*&      Form  F_UPD_PALT_CC
*&---------------------------------------------------------------------*
*       To update permitted alternate payee at company code
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_upd_palt_cc  USING    iv_iap_vendor_id     TYPE z_iap_vendor_id
                             iv_lifnr             TYPE lifnr.
  DATA: ls_opt        TYPE ctu_params,
        lv_tabix      TYPE sytabix,
        ls_pap        TYPE gty_pap,
        lt_pap        TYPE STANDARD TABLE OF gty_pap,
        lv_lines      TYPE i,
        lv_mod        TYPE i,
        lv_div        TYPE i,
        lv_cline      TYPE numc2,
        lv_fvar       TYPE fnam_____4.

  CONSTANTS lc_xk02(4) TYPE c VALUE 'XK02'.

  CLEAR: gt_bdcdata[],
         gt_messages[],
         gs_pap,
         ls_pap,
         ls_opt.

  ls_opt-dismode = 'N'.
  ls_opt-defsize = gc_x.
  ls_opt-updmode = 'S'.

  READ TABLE gt_pap_c INTO ls_pap WITH KEY zz_iap_vendor_id = iv_iap_vendor_id.
  IF sy-subrc EQ 0.
    CLEAR lv_tabix.
    lv_tabix = sy-tabix.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0101'.
    PERFORM f_bdc_field  USING 'RF02K-LIFNR'   iv_lifnr.
    PERFORM f_bdc_field  USING 'RF02K-BUKRS'   ls_pap-bukrs.
    PERFORM f_bdc_field  USING 'RF02K-D0215'   'X'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '/00'.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0215'.
    PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFB1-ZTERM'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=LFZA'.

    CLEAR: lt_pap[],
           gt_lfza[],
           lv_lines.

    lt_pap[] = gt_pap_c[].
    SORT lt_pap BY lifnr bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_pap COMPARING lifnr bukrs.

    SELECT empfk FROM lfza
                 INTO TABLE gt_lfza
                 FOR ALL ENTRIES IN lt_pap
                 WHERE lifnr = lt_pap-lifnr
                 AND   bukrs = lt_pap-bukrs.
    IF sy-subrc EQ 0.
      SORT gt_lfza.
      DESCRIBE TABLE gt_lfza LINES lv_lines.
    ENDIF.

    lv_div = lv_lines DIV 7.
    lv_mod = lv_lines MOD 7.
    lv_cline = lv_mod + 1.

    DO lv_div TIMES.
      PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
      PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFZA-EMPFK(01)'.
      PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=P+'.
    ENDDO.

    LOOP AT gt_pap_c INTO gs_pap FROM lv_tabix.
      IF gs_pap-zz_iap_vendor_id NE ls_pap-zz_iap_vendor_id.
        EXIT.
      ENDIF.

      IF lv_cline LE 7.

        CONCATENATE 'LFZA-EMPFK(' lv_cline ')' INTO lv_fvar.
        PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
        PERFORM f_bdc_field  USING 'BDC_CURSOR'    lv_fvar."'LFZA-EMPFK(01)'.
        PERFORM f_bdc_field  USING 'BDC_OKCODE'    '/00'.
        PERFORM f_bdc_field  USING lv_fvar         gs_pap-empfk.
        lv_cline = lv_cline + 1.
      ENDIF.

      IF lv_cline GT 7.
        PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
        PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFZA-EMPFK(01)'.
        PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=P+'.
        lv_cline = 1.
      ENDIF.

    ENDLOOP.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
    PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFZA-EMPFK(01)'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=BACK'.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0215'.
    PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFB1-ZTERM'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=UPDA'.

  ENDIF.

  IF NOT gt_bdcdata[] IS INITIAL.

    CALL TRANSACTION lc_xk02 USING         gt_bdcdata
                             OPTIONS FROM  ls_opt
                             MESSAGES INTO gt_messages.

    PERFORM f_bdc_errors USING iv_lifnr.

  ENDIF.
ENDFORM.                    " F_UPD_PALT_CC
*&---------------------------------------------------------------------*
*&      form f_bdc_dynpro
*&---------------------------------------------------------------------*
FORM f_bdc_dynpro USING iv_program TYPE any
                        iv_dynpro TYPE any.

  CLEAR gs_bdcdata.
  gs_bdcdata-program = iv_program.
  gs_bdcdata-dynpro = iv_dynpro.
  gs_bdcdata-dynbegin = 'X'.
  APPEND gs_bdcdata TO gt_bdcdata.

ENDFORM.                    "f_bdc_dynpro
*&---------------------------------------------------------------------*
*&      form f_bdc_field
*&---------------------------------------------------------------------*
FORM f_bdc_field USING iv_fnam TYPE any
                       iv_fval TYPE any.
  DATA lv_fval(150).
  lv_fval = iv_fval.
  CONDENSE lv_fval.

  CLEAR gs_bdcdata.
  gs_bdcdata-fnam = iv_fnam.
  gs_bdcdata-fval = lv_fval.
  APPEND gs_bdcdata TO gt_bdcdata.

ENDFORM.                    "f_bdc_field
*&---------------------------------------------------------------------*
*&      Form  F_UPD_BANK_CH
*&---------------------------------------------------------------------*
*       To update the bank chains
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_upd_bank_ch USING    iv_iap_vendor_id     TYPE z_iap_vendor_id.

  TYPES: BEGIN OF lty_tbchain2,
         mandt    TYPE mandt,
         banksrec	TYPE banksrec,
         bankkrec	TYPE bankkrec,
         banknrec	TYPE banknrec,
         waers    TYPE waers,
         bankssnd	TYPE bankssnd,
         bankksnd	TYPE bankksnd,
         uzawe    TYPE uzawe,
         action(1)  TYPE c,
         mark(1)    TYPE c,
         END OF   lty_tbchain2,

         BEGIN OF lty_tbchain21,
         mandt    TYPE mandt,
         banksrec	TYPE banksrec,
         bankkrec	TYPE bankkrec,
         banknrec	TYPE banknrec,
         waers    TYPE waers,
         bankssnd	TYPE bankssnd,
         bankksnd	TYPE bankksnd,
         uzawe    TYPE uzawe,
         chainno  TYPE chainno,
         chainbankt	TYPE chainbankt,
         chainbanks	TYPE chainbanks,
         chainbankk	TYPE chainbankk,
         chainbankn	TYPE chainbankn,
         iban       TYPE iban,
         action(1)  TYPE c,
         mark(1)    TYPE c,
         END OF   lty_tbchain21.

  DATA: lt_tbchain2     TYPE STANDARD TABLE OF lty_tbchain2,
        lt_tbchain21    TYPE STANDARD TABLE OF lty_tbchain21,
        ls_tbchain2    TYPE lty_tbchain2,
        ls_tbchain21   TYPE lty_tbchain21,
        lt_ler          TYPE STANDARD TABLE OF gty_ler,
        ls_ler         TYPE gty_ler,
        lv_tabix        TYPE sytabix.

  CONSTANTS: lc_n(1) TYPE c VALUE 'N',
             lc_u(1) TYPE c VALUE 'U',
             lc_d(1) TYPE c VALUE 'D'.

  CLEAR: lt_tbchain2[],
         lt_tbchain21[],
         lt_ler[].

  lt_ler[] = gt_ler[].
  SORT lt_ler BY zz_iap_vendor_id.

  READ TABLE lt_ler INTO ls_ler WITH KEY zz_iap_vendor_id = iv_iap_vendor_id.
  IF sy-subrc EQ 0.
    CLEAR lv_tabix.
    lv_tabix = sy-tabix.
    LOOP AT gt_ler INTO gs_ler FROM lv_tabix.
      IF gs_ler-zz_iap_vendor_id NE ls_ler-zz_iap_vendor_id.
        EXIT.
      ENDIF.
      CLEAR: ls_tbchain2,
             ls_tbchain21.

      MOVE-CORRESPONDING: gs_ler TO ls_tbchain2,
                          gs_ler TO ls_tbchain21.

      MOVE: gs_ler-banks TO ls_tbchain2-banksrec,
            gs_ler-banks TO ls_tbchain21-banksrec,

            gs_ler-bankl TO ls_tbchain2-bankkrec,
            gs_ler-bankl TO ls_tbchain21-bankkrec,

            gs_ler-bankn TO ls_tbchain2-banknrec,
            gs_ler-bankn TO ls_tbchain21-banknrec,

            gs_ler-chainno TO ls_tbchain21-chainno.

*FM FI_BL_BANKCHAIN_ASSGNP_SAVE_BC where actions will be done
      IF gs_ler-chngind_lfbk IS INITIAL.
        IF NOT gs_ler-lifnr IS INITIAL.
          ls_tbchain2-action = ls_tbchain21-action = lc_n.
        ELSE.
          ls_tbchain2-action = ls_tbchain21-action = lc_u.
        ENDIF.
      ELSE.
        ls_tbchain2-action = ls_tbchain21-action = lc_d.
      ENDIF.

      APPEND: ls_tbchain2 TO lt_tbchain2,
              ls_tbchain21 TO lt_tbchain21.

    ENDLOOP.

    CALL FUNCTION 'FI_BL_BANKCHAIN_VCL_SET_DATA'
      EXPORTING
        i_changed = 'X'
      TABLES
        table1    = lt_tbchain2
        table2    = lt_tbchain21.

    CALL FUNCTION 'FI_BL_BANKCHAIN_ASSGNP_SAVE'
      .

  ENDIF.

ENDFORM.                    " F_UPD_BANK_CH
*&---------------------------------------------------------------------*
*&      Form  f_build_file_list
*&---------------------------------------------------------------------*
*       Build the list of files to be processed
*----------------------------------------------------------------------*
FORM f_build_file_list.

  DATA:    ls_file_list                TYPE gty_file_list,
           ls_dir_list                 TYPE gty_dir_list,
           lt_dir_list                 TYPE gtt_dir_list.

  DATA:    lv_dir_name                 TYPE epsdirnam,
           lv_file_mask                TYPE epsfilnam,
           lv_rc                       TYPE numc5,
           lv_text                     TYPE string,
           lv_pattern                  TYPE string,
           lv_token                    TYPE string,
           lv_offset                   TYPE i,
           lv_length                   TYPE i,
           lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix.

  FIELD-SYMBOLS: <fs_file_list>        TYPE gty_file_list.

  CLEAR    gt_file_list[].

  IF     ( gv_fl_errr_prcs IS NOT INITIAL ).
    RETURN.
  ENDIF.

  IF     ( rb_fsprs        IS NOT INITIAL ).

    CLEAR                                   ls_file_list.
    MOVE     abap_true                   TO ls_file_list-fsprs.
    MOVE     p_fnam1p                    TO ls_file_list-fn_in_main.
    APPEND   ls_file_list                TO gt_file_list.

*eject
  ELSEIF ( rb_fsapl IS NOT INITIAL ).

    CLEAR                                   lv_dir_name.
    MOVE     p_fpth1a                    TO lv_dir_name.

    CLEAR                                   lv_file_mask.
    MOVE     p_fnam1a                    TO lv_file_mask.

    CLEAR    lt_dir_list[].

    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name               = lv_dir_name
        file_mask              = lv_file_mask
      TABLES
        dir_list               = lt_dir_list
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.

    lv_rc = sy-subrc.

    IF   ( ( lv_rc NE 0 ) AND ( lv_rc NE 7 ) ).
      CLEAR    lt_dir_list[].
      CLEAR                                 lv_text.
      CONCATENATE                           text-231 text-232
                                            lv_rc lv_dir_name
                                       INTO lv_text
                               SEPARATED BY space.
      macro_msg                     'B' 'A' lv_text.
      RETURN.
    ENDIF.

    DELETE   lt_dir_list              WHERE name IS INITIAL.

    IF     ( lt_dir_list[]               IS INITIAL ).
      CLEAR                                 lv_text.
      MOVE     text-233                  TO lv_text.
      macro_msg                     'B' 'I' lv_text.
      RETURN.
    ENDIF.

*eject
* Evaluate the files in the inbound application directory
    IF     ( p_fregxa                    IS NOT INITIAL ).

      CLEAR                                 ls_dir_list.
      LOOP AT    lt_dir_list           INTO ls_dir_list.

* Search the string using a pattern; return offset, length, and token
        CLEAR                               lv_text.
        MOVE     ls_dir_list-name        TO lv_text.

        CLEAR                               lv_pattern.
        MOVE     p_fregxa                TO lv_pattern.

        CLEAR    lv_offset.
        CLEAR    lv_length.
        CLEAR    lv_token.
        CLEAR    lv_subrc.

        CALL FUNCTION 'ZFI_PARSE_STRING_USING_PATTERN'
          EXPORTING
            iv_text       = lv_text
            iv_pattern    = lv_pattern
          IMPORTING
            cv_offset     = lv_offset
            cv_length     = lv_length
            cv_token      = lv_token
            cv_subrc      = lv_subrc
          EXCEPTIONS
            no_text       = 1
            no_pattern    = 2
            search_error  = 3
            pattern_error = 4
            parse_error   = 5
            OTHERS        = 6.

        lv_rc = sy-subrc.

        IF     ( lv_rc NE 0 ).
          CLEAR    gt_file_list[].
          CLEAR                             lv_text.
          CONCATENATE                       text-235 text-232
                                            lv_rc lv_pattern
                                       INTO lv_text
                               SEPARATED BY space.
          macro_msg                 'B' 'A' lv_text.
          RETURN.
        ENDIF.

*eject
* If the pattern is found, then save the filename in the file list
        IF     ( ( lv_subrc EQ 0 ) AND ( lv_token IS NOT INITIAL ) ).
          CLEAR                             ls_file_list.
          MOVE     abap_true             TO ls_file_list-fsapl.
          MOVE     p_fpth1a              TO ls_file_list-fp_in_main.
          MOVE     ls_dir_list-name      TO ls_file_list-fn_in_main.
          APPEND   ls_file_list          TO gt_file_list.
        ENDIF.

        CLEAR  ls_dir_list.
      ENDLOOP.

    ELSE.

      CLEAR                                 ls_dir_list.
      LOOP AT      lt_dir_list         INTO ls_dir_list.
        CLEAR                               ls_file_list.
        MOVE       abap_true             TO ls_file_list-fsapl.
        MOVE       p_fpth1a              TO ls_file_list-fp_in_main.
        MOVE       ls_dir_list-name      TO ls_file_list-fn_in_main.
        APPEND     ls_file_list          TO gt_file_list.
        CLEAR      ls_dir_list.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF     ( gt_file_list[]                IS INITIAL ).
    CLEAR                                   lv_text.
    MOVE     text-233                    TO lv_text.
    macro_msg                       'B' 'I' lv_text.
    RETURN.
  ENDIF.

  SORT     gt_file_list        ASCENDING BY fp_in_main fn_in_main.

  LOOP AT  gt_file_list           ASSIGNING <fs_file_list>.
    lv_tabix = sy-tabix.

    MOVE     lv_tabix                    TO <fs_file_list>-file_nbr.

  ENDLOOP.

ENDFORM.                    " f_build_file_list
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_MAIN
*&---------------------------------------------------------------------*
*       The Main Form to process
*----------------------------------------------------------------------*
FORM f_process_main .

  DATA:    lv_tabix                    TYPE sytabix.

  FIELD-SYMBOLS: <fs_file_list>        TYPE gty_file_list.

  IF     ( gv_fl_errr_prcs IS NOT INITIAL ).
    RETURN.
  ENDIF.

* Loop at the file list
  LOOP AT  gt_file_list           ASSIGNING <fs_file_list>.
    lv_tabix = sy-tabix.

    IF     ( lv_tabix GT 1 ).
      WAIT UP TO 2 SECONDS.
    ENDIF.

* Initial the data elements
    PERFORM  f_initial_data_elements.

* Generate the filepaths and filenames
    PERFORM  f_generate_filenames  CHANGING <fs_file_list>.

* Open the log file
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_open_file            USING <fs_file_list>
                                            'L' "log
                                   CHANGING gv_filename_log
                                            gv_filename_lgx.
    ENDIF.

* Open the inbound file
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_open_file            USING <fs_file_list>
                                            'I' "inbound
                                   CHANGING gv_filename_in
                                            gv_filename_inx.
    ENDIF.

* Read the data
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_read_data         CHANGING <fs_file_list>.
    ENDIF.

* Close the inbound file
    PERFORM  f_close_file             USING 'I' "inbound
                                   CHANGING gv_filename_in
                                            gv_filename_inx.

*eject
* Open the outbound file
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_open_file            USING <fs_file_list>
                                            'O' "outbound
                                   CHANGING gv_filename_out
                                            gv_filename_otx.
    ENDIF.

* Process the data
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_populate_data.
      PERFORM  f_prepare_data.
    ENDIF.

* Close the outbound file
    PERFORM  f_close_file             USING 'O' "outbound
                                   CHANGING gv_filename_out
                                            gv_filename_otx.

* Dispatch the files
    IF       ( rb_fsapl                  IS NOT INITIAL ).
      PERFORM  f_dispatch_files       USING <fs_file_list>.
    ENDIF.

* Close the log file
    PERFORM  f_close_file             USING 'L' "log
                                   CHANGING gv_filename_log
                                            gv_filename_lgx.

  ENDLOOP.

* Download files to the presentation server
  IF       ( rb_fsprs                    IS NOT INITIAL ).
    PERFORM  f_download_files         USING <fs_file_list>.
  ENDIF.

ENDFORM.                    " F_PROCESS_MAIN
*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZATION
*&---------------------------------------------------------------------*
*       Initialization
*----------------------------------------------------------------------*
FORM f_initialization .

  DATA:    lt_sscr_restrict            TYPE sscr_restrict,
           ls_sscr_opt_list            TYPE sscr_opt_list,
           ls_sscr_ass                 TYPE sscr_ass.

  MOVE   gc_fpth1a                       TO p_fpth1a.
  MOVE   gc_fnam1a                       TO p_fnam1a.
  MOVE   gc_fregxa                       TO p_fregxa.
  MOVE   gc_farc1a                       TO p_farc1a.
  MOVE   gc_fpth2a                       TO p_fpth2a.
  MOVE   gc_fnam2a                       TO p_fnam2a.
  MOVE   gc_farc2a                       TO p_farc2a.
  MOVE   gc_fpth3a                       TO p_fpth3a.
  MOVE   gc_fnam3a                       TO p_fnam3a.

* Build option list
  CLEAR    lt_sscr_restrict.

  CLEAR                                ls_sscr_opt_list.
  MOVE     'OPT_LIST'               TO ls_sscr_opt_list-name.
  MOVE     'X'                      TO ls_sscr_opt_list-options-eq.
  APPEND                               ls_sscr_opt_list
                                    TO lt_sscr_restrict-opt_list_tab.

* Apply the option list to the selection-screen select-option lists
  CLEAR                                ls_sscr_ass.
  MOVE     'S'                      TO ls_sscr_ass-kind.
  MOVE     'S_EMAIL'                TO ls_sscr_ass-name.
  MOVE     'I'                      TO ls_sscr_ass-sg_main.
  MOVE     'OPT_LIST'               TO ls_sscr_ass-op_main.
  APPEND                               ls_sscr_ass
                                    TO lt_sscr_restrict-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction = lt_sscr_restrict.

ENDFORM.                    " F_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  F_SEL_SCRN_OUTPUT
*&---------------------------------------------------------------------*
*       Selection screen output
*----------------------------------------------------------------------*
FORM f_sel_scrn_output.

  LOOP AT SCREEN.

* Set the screen fields to display only
    IF     ( screen-group1 EQ gc_modif_id_dsp ).
      screen-input  = 0.
      MODIFY   SCREEN.
    ENDIF.

    IF     ( rb_fsapl      IS NOT INITIAL ).

* Enable the application server file parameters
      IF   ( screen-group1 EQ gc_modif_id_fas ).
        screen-input  = 1.
        MODIFY SCREEN.
      ENDIF.

* Disable the presentation server file parameters
      IF   ( screen-group1 EQ gc_modif_id_fps ).
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

    ELSEIF ( rb_fsprs      IS NOT INITIAL ).

* Disable the application server file parameters
      IF   ( screen-group1 EQ gc_modif_id_fas ).
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

* Enable the presentation server file parameters
      IF   ( screen-group1 EQ gc_modif_id_fps ).
        screen-input  = 1.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_sel_scrn_output
*&---------------------------------------------------------------------*
*&      Form  F_F4_HELP_PRES_FILE
*&---------------------------------------------------------------------*
*       F4 help - presentation server - file open dialog
*----------------------------------------------------------------------*
FORM f_f4_help_pres_file
  CHANGING cv_fname                    TYPE text255.

  DATA:    ls_file_table               TYPE file_table,
           lt_file_table               TYPE filetable.

  DATA:    lv_window_title             TYPE string,
           lv_initial_directory        TYPE string,
           lv_rc                       TYPE i.

  CLEAR                                     lv_window_title.
  MOVE     text-c01                      TO lv_window_title.
  CLEAR                                     lv_initial_directory.
  MOVE     text-c02                      TO lv_initial_directory.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_window_title
      initial_directory       = lv_initial_directory
      multiselection          = ' '
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF   ( ( sy-subrc EQ 0 ) AND ( lv_rc GT 0 ) ).

    CLEAR                                   ls_file_table.
    READ   TABLE lt_file_table         INTO ls_file_table
                                      INDEX 1.
    IF ( sy-subrc EQ 0 ).
      CLEAR                                 cv_fname.
      MOVE       ls_file_table-filename  TO cv_fname.
    ENDIF.

  ENDIF.

ENDFORM.                     " F_F4_HELP_PRES_FILE
*&---------------------------------------------------------------------*
*&      Form  F_INITIAL_DATA_ELEMENTS
*&---------------------------------------------------------------------*
*       To clear the global variables
*----------------------------------------------------------------------*
FORM f_initial_data_elements .


  CLEAR    gv_fl_errr_prcs.
  CLEAR    gv_cnt_doc_trns.
  CLEAR    gv_cnt_doc_errr.
  CLEAR    gv_cnt_doc_wrng.
  CLEAR    gv_cnt_doc_pass.
  CLEAR    gv_filename_in.
  CLEAR    gv_filename_inx.
  CLEAR    gv_filename_out.
  CLEAR    gv_filename_otx.
  CLEAR    gv_filename_log.
  CLEAR    gv_filename_lgx.

  CLEAR    gt_out_data[].
  CLEAR    gt_log_data[].

ENDFORM.                    " F_INITIAL_DATA_ELEMENTS
*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_FILENAMES
*&---------------------------------------------------------------------*
*       Read the data
*----------------------------------------------------------------------*
FORM f_generate_filenames  CHANGING cs_file_list                TYPE gty_file_list.

  DATA:    lv_filepath                 TYPE text128,
           lv_filename                 TYPE text128.

* Inbound archive
  CLEAR                                     lv_filename.
  MOVE       cs_file_list-fn_in_main     TO lv_filename.
  TRANSLATE  lv_filename              USING '._'.
  CONCATENATE                               lv_filename '_'
                                            sy-datum    '_'
                                            sy-uzeit    text-c21
                                       INTO lv_filename.

  MOVE     p_farc1a                      TO cs_file_list-fp_in_arch.
  MOVE     lv_filename                   TO cs_file_list-fn_in_arch.

* Outbound temporary
  CLEAR                                     lv_filepath.
  MOVE     p_fpth1a                      TO lv_filepath.
  CONCATENATE                               lv_filepath text-c05 gc_slash
                                       INTO lv_filepath.

  CLEAR                                     lv_filename.
  MOVE     p_fnam2a                      TO lv_filename.
  REPLACE  'ERPID'                       IN lv_filename WITH p_erpid.
  REPLACE  'YYYYMMDD'                    IN lv_filename WITH sy-datum.
  REPLACE  'HHMMSS'                      IN lv_filename WITH sy-uzeit.

  MOVE     lv_filepath                   TO cs_file_list-fp_out_temp.
  MOVE     lv_filename                   TO cs_file_list-fn_out_temp.

* Outbound archive
  CLEAR                                     lv_filename.
  MOVE       cs_file_list-fn_out_temp    TO lv_filename.

  IF     ( cb_test IS INITIAL ).
    TRANSLATE  lv_filename            USING '._'.
    CONCATENATE                             lv_filename text-c21
                                       INTO lv_filename.
  ELSE.
    TRANSLATE  lv_filename            USING '._'.
    CONCATENATE                             lv_filename text-c22
                                       INTO lv_filename.
  ENDIF.

  MOVE     p_farc2a                      TO cs_file_list-fp_out_arch.
  MOVE     lv_filename                   TO cs_file_list-fn_out_arch.

*eject
* Outbound main
  IF     ( rb_fsapl                      IS NOT INITIAL ).
    MOVE   p_fpth2a                      TO cs_file_list-fp_out_main.
    MOVE   cs_file_list-fn_out_temp      TO cs_file_list-fn_out_main.
  ELSE.
    MOVE   p_fnam2p                      TO cs_file_list-fn_out_main.
  ENDIF.

* Log main
  CLEAR                                     lv_filename.
  MOVE     p_fnam3a                      TO lv_filename.
  REPLACE  'ERPID'                       IN lv_filename WITH p_erpid.
  REPLACE  'YYYYMMDD'                    IN lv_filename WITH sy-datum.
  REPLACE  'HHMMSS'                      IN lv_filename WITH sy-uzeit.

  IF     ( cb_test IS INITIAL ).
  ELSE.
    TRANSLATE  lv_filename            USING '._'.
    CONCATENATE                             lv_filename text-c22
                                       INTO lv_filename.
  ENDIF.

  IF     ( rb_fsapl                      IS NOT INITIAL ).
    MOVE   p_fpth3a                      TO cs_file_list-fp_log_main.
    MOVE   lv_filename                   TO cs_file_list-fn_log_main.
  ELSE.
    MOVE   p_fnam3p                      TO cs_file_list-fn_log_main.
  ENDIF.

ENDFORM.                    " F_GENERATE_FILENAMES
*&---------------------------------------------------------------------*
*&      Form  F_OPEN_FILE
*&---------------------------------------------------------------------*
*       Open a file
*----------------------------------------------------------------------*
FORM f_open_file
  USING    is_file_list                TYPE gty_file_list
           iv_file_funct               TYPE char1
  CHANGING cv_filename                 TYPE text255
           cv_filename_x               TYPE xflag.

  DATA:    lv_filename                 TYPE text255,
           lv_subrc                    TYPE sysubrc,
           lv_string                   TYPE string,
           lv_rc                       TYPE numc5,
           lv_text                     TYPE string,
           lv_date_time                TYPE char20.

  DATA:    lr_file_open                TYPE REF TO
                                       cx_sy_file_open,
           lr_file_authority           TYPE REF TO
                                       cx_sy_file_authority,
           lr_codepage_init            TYPE REF TO
                                       cx_sy_codepage_converter_init,
           lr_codepage_conv            TYPE REF TO
                                       cx_sy_conversion_codepage.

  CLEAR    cv_filename.
  CLEAR    cv_filename_x.

  CLEAR    lv_filename.
  CLEAR    lv_subrc.
  CLEAR    lv_string.
  CLEAR    lv_rc.
  CLEAR    lv_text.
  CLEAR    lv_date_time.

  IF     ( rb_fsapl IS NOT INITIAL ).

    CASE     iv_file_funct.
      WHEN     'I'.
        CONCATENATE is_file_list-fp_in_main is_file_list-fn_in_main
                                       INTO lv_filename.
        MOVE   text-241                  TO lv_text.
      WHEN     'O'.
        CONCATENATE is_file_list-fp_out_temp is_file_list-fn_out_temp
                                       INTO lv_filename.
        MOVE   text-242                  TO lv_text.
      WHEN     'L'.
        CONCATENATE is_file_list-fp_log_main is_file_list-fn_log_main
                                       INTO lv_filename.
        MOVE   text-243                  TO lv_text.
      WHEN     OTHERS.
    ENDCASE.

*eject
    CLEAR    lv_subrc.
    CLEAR    lv_string.

    TRY.

        IF     ( iv_file_funct                EQ 'I' ).
          OPEN   DATASET lv_filename         FOR INPUT
                                              IN TEXT MODE
                                        ENCODING DEFAULT.
        ELSE.
          OPEN   DATASET lv_filename         FOR OUTPUT
                                              IN TEXT MODE
                                        ENCODING DEFAULT.
        ENDIF.

      CATCH  cx_sy_file_open                INTO lr_file_open.
        lv_subrc  = 1.
        lv_string = lr_file_open->get_text( ).
      CATCH  cx_sy_file_authority           INTO lr_file_authority.
        lv_subrc  = 2.
        lv_string = lr_file_authority->get_text( ).
      CATCH  cx_sy_codepage_converter_init  INTO lr_codepage_init.
        lv_subrc  = 3.
        lv_string = lr_codepage_init->get_text( ).
      CATCH  cx_sy_conversion_codepage      INTO lr_codepage_conv.
        lv_subrc  = 4.
        lv_string = lr_codepage_conv->get_text( ).

    ENDTRY.

    IF   ( ( sy-subrc EQ 0 ) AND ( lv_subrc EQ 0 ) ).

      cv_filename   = lv_filename.
      cv_filename_x = abap_true.

    ELSE.

      cv_filename   = lv_filename.
      cv_filename_x = abap_false.

      lv_rc = lv_subrc.

      CONCATENATE                           lv_text text-232
                                            lv_rc   lv_filename
                                       INTO lv_text
                               SEPARATED BY space.
      macro_msg                     'B' 'A' lv_text.
      macro_msg                     'B' 'A' lv_string.
      macro_log                           1 lv_text.
      RETURN.
    ENDIF.

  ENDIF.

*eject
* Write the selection screen parameters to the log file

  IF     ( iv_file_funct EQ 'L' ).

* Run Date and Time
    WRITE             sy-datum           TO lv_date_time.
    WRITE             sy-uzeit           TO lv_date_time+11.
    CLEAR                                   lv_text.
    CONCATENATE       text-l01              lv_date_time
                                       INTO lv_text.
    macro_log                             2 lv_text.

* File Number
    CLEAR                                   lv_text.
    CONCATENATE       text-l02              is_file_list-file_nbr
                                       INTO lv_text.
    macro_log                             0 lv_text.

* ERP ID
    CLEAR                                   lv_text.
    CONCATENATE       text-l03 p_erpid INTO lv_text.
    macro_log                             0 lv_text.

* Test Run / Post Run
    CLEAR                                   lv_text.
    IF     ( cb_test                     IS NOT INITIAL ).
      CONCATENATE     text-l04 'Yes'   INTO lv_text.
    ELSE.
      CONCATENATE     text-l05 'Yes'   INTO lv_text.
    ENDIF.
    macro_log                             0 lv_text.

* Inbound File
    CLEAR                                   lv_text.
    CONCATENATE       text-l11              is_file_list-fp_in_main
                                            is_file_list-fn_in_main
                                       INTO lv_text.
    macro_log                             1 lv_text.

* Inbound Archive File
    CLEAR                                   lv_text.
    CONCATENATE       text-l12              is_file_list-fp_in_arch
                                            is_file_list-fn_in_arch
                                       INTO lv_text.
    IF     ( cb_test IS INITIAL ).
      macro_log                           0 lv_text.
    ENDIF.

*eject
* Outbound File
    CLEAR                                   lv_text.
    CONCATENATE       text-l13              is_file_list-fp_out_main
                                            is_file_list-fn_out_main
                                       INTO lv_text.
    IF     ( cb_test IS INITIAL ).
      macro_log                           0 lv_text.
    ENDIF.

* Outbound Archive File
    CLEAR                                   lv_text.
    CONCATENATE       text-l14              is_file_list-fp_out_arch
                                            is_file_list-fn_out_arch
                                       INTO lv_text.
    macro_log                             0 lv_text.

* Log File
    CLEAR                                   lv_text.
    CONCATENATE       text-l15              is_file_list-fp_log_main
                                            is_file_list-fn_log_main
                                       INTO lv_text.
    macro_log                             0 lv_text.

  ENDIF.

ENDFORM.                    " F_OPEN_FILE
*&---------------------------------------------------------------------*
*&      Form  F_READ_DATA
*&---------------------------------------------------------------------*
*       Read the data
*----------------------------------------------------------------------*
FORM f_read_data
  CHANGING cs_file_list                TYPE gty_file_list.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_lines                    TYPE syindex,
           lv_filename                 TYPE string,
           lv_string                   TYPE string,
           lv_rc                       TYPE numc5,
           lv_text                     TYPE string.

  DATA:    lr_codepage_init            TYPE REF TO
                                       cx_sy_codepage_converter_init,
           lr_codepage_conv            TYPE REF TO
                                       cx_sy_conversion_codepage,
           lr_file_io                  TYPE REF TO cx_sy_file_io.

  IF     ( gv_fl_errr_prcs IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    gt_tab_data[].

*eject
* Read from the presentation server
  IF     ( rb_fsprs                      IS NOT INITIAL ).

    CLEAR                                   lv_filename.
    MOVE     cs_file_list-fn_in_main     TO lv_filename.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                      = lv_filename
*       FILETYPE                      = 'ASC'
*     IMPORTING
*       FILELENGTH                    =
*       HEADER                        =
      TABLES
        data_tab                      = gt_tab_data
*     CHANGING
*       ISSCANPERFORMED               = ' '
      EXCEPTIONS
        file_open_error               = 1
        file_read_error               = 2
        no_batch                      = 3
        gui_refuse_filetransfer       = 4
        invalid_type                  = 5
        no_authority                  = 6
        unknown_error                 = 7
        bad_data_format               = 8
        header_not_allowed            = 9
        separator_not_allowed         = 10
        header_too_long               = 11
        unknown_dp_error              = 12
        access_denied                 = 13
        dp_out_of_memory              = 14
        disk_full                     = 15
        dp_timeout                    = 16
        OTHERS                        = 17.

    lv_rc = sy-subrc.

    IF     ( lv_rc NE 0 ).
      CLEAR                                 lv_text.
      CONCATENATE                           text-246 text-232
                                            lv_rc lv_filename
                                       INTO lv_text
                               SEPARATED BY space.
      macro_msg                     'B' 'A' lv_text.
      RETURN.
    ENDIF.

*eject
* Read from the application server
  ELSEIF ( rb_fsapl                      IS NOT INITIAL ).

* Read the file
    CLEAR    lv_subrc.
    CLEAR    lv_string.

    DO.

      CLEAR    gs_tab_data.

      TRY.
          READ   DATASET gv_filename_in     INTO gs_tab_data.
        CATCH cx_sy_codepage_converter_init INTO lr_codepage_init.
          lv_subrc  = 1.
          lv_string = lr_codepage_init->get_text( ).
        CATCH  cx_sy_conversion_codepage    INTO lr_codepage_conv.
          lv_subrc  = 2.
          lv_string = lr_codepage_conv->get_text( ).
        CATCH  cx_sy_file_io                INTO lr_file_io.
          lv_subrc  = 3.
          lv_string = lr_file_io->get_text( ).
      ENDTRY.

      IF     ( sy-subrc NE 0 ).
        EXIT.
      ELSEIF ( lv_subrc NE 0 ).
        EXIT.
      ENDIF.

      APPEND   gs_tab_data               TO gt_tab_data.

    ENDDO.

    IF     ( lv_subrc NE 0 ).

      lv_rc = lv_subrc.

      CLEAR                                 lv_text.
      CONCATENATE                           text-245 text-232
                                            lv_rc lv_filename
                                       INTO lv_text
                               SEPARATED BY space.
      macro_msg                     'B' 'A' lv_text.
      macro_msg                     'B' 'A' lv_string.
      RETURN.
    ENDIF.

  ENDIF.

  CLEAR                                     lv_lines.
  DESCRIBE TABLE gt_tab_data          LINES lv_lines.

  CLEAR                                     cs_file_list-cn_recs.
  MOVE     lv_lines                      TO cs_file_list-cn_recs.

* Inbound File Record Count
  CLEAR                                   lv_text.
  CONCATENATE       text-l21              cs_file_list-cn_recs
                                     INTO lv_text.
  macro_log                             0 lv_text.

ENDFORM.                    " F_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  F_CLOSE_FILE
*&---------------------------------------------------------------------*
*       Close a file
*----------------------------------------------------------------------*
FORM f_close_file
  USING    iv_file_funct               TYPE char1
  CHANGING cv_filename                 TYPE text255
           cv_filename_x               TYPE xflag.

  DATA:    lv_text                     TYPE string.

* Write the transaction counts to the log file

  IF     ( iv_file_funct EQ 'O' ).

    CLEAR                                   lv_text.
    CONCATENATE       text-l51              gv_cnt_doc_trns
                                       INTO lv_text.
    macro_log                             2 lv_text.
    CLEAR                                   lv_text.

    CLEAR                                   lv_text.
    CONCATENATE       text-l52              gv_cnt_doc_errr
                                       INTO lv_text.
    macro_log                             0 lv_text.

    CLEAR                                   lv_text.
    CLEAR                                   lv_text.
    CONCATENATE       text-l53              gv_cnt_doc_wrng
                                       INTO lv_text.
    macro_log                             0 lv_text.

    CLEAR                                   lv_text.
    CLEAR                                   lv_text.
    CONCATENATE       text-l54              gv_cnt_doc_pass
                                       INTO lv_text.
    macro_log                             0 lv_text.

  ENDIF.

* Close the file

  IF     ( rb_fsapl      IS NOT INITIAL ).
    IF   ( cv_filename_x IS NOT INITIAL ).
      CLOSE    DATASET cv_filename.
*     CLEAR            cv_filename. "*** do not uncomment ***
      CLEAR            cv_filename_x.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_CLOSE_FILE
*&---------------------------------------------------------------------*
*&      Form  F_DISPATCH_FILES
*&---------------------------------------------------------------------*
*       Move the files
*----------------------------------------------------------------------*
FORM f_dispatch_files
  USING    is_file_list                TYPE gty_file_list.

  DATA:    lv_filename                 TYPE text128,
           lv_source_dir               TYPE btcxpgpar,
           lv_target_dir               TYPE btcxpgpar,
           lv_source_fname             TYPE btcxpgpar,
           lv_target_fname             TYPE btcxpgpar,
           ls_return                   TYPE bapireturn,
           lv_text_c120                TYPE text120.

  IF     ( rb_fsapl IS INITIAL ).
    RETURN.
  ENDIF.

*eject

*** Dispatch the inbound file (only in post mode

  IF     ( cb_test IS INITIAL ).

* Rename the input file
    CLEAR                                   lv_filename.
    MOVE     is_file_list-fn_in_arch     TO lv_filename.
    IF     ( gv_fl_errr_prcs             IS NOT INITIAL ).
      TRANSLATE                             lv_filename USING '._'.
      CONCATENATE                           lv_filename text-c23
                                       INTO lv_filename.
    ENDIF.

    CLEAR                                   lv_source_dir.
    MOVE     is_file_list-fp_in_main     TO lv_source_dir.
    CLEAR                                   lv_source_fname.
    MOVE     is_file_list-fn_in_main     TO lv_source_fname.
    CLEAR                                   lv_target_dir.
    MOVE     is_file_list-fp_in_main     TO lv_target_dir.
    CLEAR                                   lv_target_fname.
    MOVE     lv_filename                 TO lv_target_fname.

    CLEAR                                   lv_text_c120.
    MOVE     text-c13                    TO lv_text_c120+04.
    MOVE     text-c15                    TO lv_text_c120+12.
    MOVE     lv_source_dir               TO lv_text_c120+20.
    macro_log                             2 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_source_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     text-c16                    TO lv_text_c120+12.
    MOVE     lv_target_dir               TO lv_text_c120+20.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_target_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.

    CLEAR    ls_return.

*   CALL FUNCTION 'ZFI_FILE_HANDLE'
*     EXPORTING
*       I_SOURCE_DIR   = lv_source_dir
*       I_SOURCE_FNAME = lv_source_fname
*       I_TARGET_FNAME = lv_target_fname
*       I_COMMAND      = 'R'
*     IMPORTING
*       E_RETURN       = ls_return.

    IF     ( ls_return-type EQ 'E' ).
      macro_msg  'B' 'A' text-253.
      RETURN.
    ENDIF.

*eject
* Move the input file to the inbound archive directory
    CLEAR                                   lv_source_dir.
    MOVE     is_file_list-fp_in_main     TO lv_source_dir.
    CLEAR                                   lv_source_fname.
    MOVE     lv_filename                 TO lv_source_fname.
    CLEAR                                   lv_target_dir.
    MOVE     is_file_list-fp_in_arch     TO lv_target_dir.
    CLEAR                                   lv_target_fname.
    MOVE     lv_filename                 TO lv_target_fname.

    CLEAR                                   lv_text_c120.
    MOVE     text-c12                    TO lv_text_c120+04.
    MOVE     text-c15                    TO lv_text_c120+12.
    MOVE     lv_source_dir               TO lv_text_c120+20.
    macro_log                             1 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_source_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     text-c16                    TO lv_text_c120+12.
    MOVE     lv_target_dir               TO lv_text_c120+20.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_target_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.

    CLEAR    ls_return.

*   CALL FUNCTION 'ZFI_FILE_HANDLE'
*     EXPORTING
*       I_SOURCE_DIR   = lv_source_dir
*       I_TARGET_DIR   = lv_target_dir
*       I_SOURCE_FNAME = lv_source_fname
*       I_TARGET_FNAME = lv_target_fname
*       I_COMMAND      = 'M'
*     IMPORTING
*       E_RETURN       = ls_return.

    IF     ( ls_return-type EQ 'E' ).
      macro_msg  'B' 'A' text-252.
      RETURN.
    ENDIF.

  ENDIF.

*eject

*** Dispatch the outbound file

* Copy the output file to the outbound archive directory
  CLEAR                                     lv_filename.
  MOVE     is_file_list-fn_out_arch      TO lv_filename.
  IF     ( gv_fl_errr_prcs               IS NOT INITIAL ).
    TRANSLATE                               lv_filename USING '._'.
    CONCATENATE                             lv_filename text-c23
                                       INTO lv_filename.
  ENDIF.

  CLEAR                                     lv_source_dir.
  MOVE     is_file_list-fp_out_temp      TO lv_source_dir.
  CLEAR                                     lv_source_fname.
  MOVE     is_file_list-fn_out_temp      TO lv_source_fname.
  CLEAR                                     lv_target_dir.
  MOVE     is_file_list-fp_out_arch      TO lv_target_dir.
  CLEAR                                     lv_target_fname.
  MOVE     lv_filename                   TO lv_target_fname.

  CLEAR                                     lv_text_c120.
  MOVE     text-c11                      TO lv_text_c120+04.
  MOVE     text-c15                      TO lv_text_c120+12.
  MOVE     lv_source_dir                 TO lv_text_c120+20.
  macro_log                               1 lv_text_c120.
  CLEAR                                     lv_text_c120.
  MOVE     lv_source_fname               TO lv_text_c120+25.
  macro_log                               0 lv_text_c120.
  CLEAR                                     lv_text_c120.
  MOVE     text-c16                      TO lv_text_c120+12.
  MOVE     lv_target_dir                 TO lv_text_c120+20.
  macro_log                               0 lv_text_c120.
  CLEAR                                     lv_text_c120.
  MOVE     lv_target_fname               TO lv_text_c120+25.
  macro_log                               0 lv_text_c120.

  CLEAR    ls_return.

* CALL FUNCTION 'ZFI_FILE_HANDLE'
*   EXPORTING
*     I_SOURCE_DIR   = lv_source_dir
*     I_TARGET_DIR   = lv_target_dir
*     I_SOURCE_FNAME = lv_source_fname
*     I_TARGET_FNAME = lv_target_fname
*     I_COMMAND      = 'C'
*   IMPORTING
*     E_RETURN       = ls_return.

  IF     ( ls_return-type EQ 'E' ).
    macro_msg  'B' 'A' text-251.
    RETURN.
  ENDIF.

*eject
* If this is a test run or there is a process error, then delete
  IF   ( ( cb_test                       IS NOT INITIAL ) OR
         ( gv_fl_errr_prcs               IS NOT INITIAL )    ).

    CLEAR                                   lv_source_dir.
    MOVE     is_file_list-fp_out_temp    TO lv_source_dir.
    CLEAR                                   lv_source_fname.
    MOVE     is_file_list-fn_out_temp    TO lv_source_fname.

    CLEAR                                   lv_text_c120.
    MOVE     text-c14                    TO lv_text_c120+04.
    MOVE     text-c15                    TO lv_text_c120+12.
    MOVE     lv_source_dir               TO lv_text_c120+20.
    macro_log                             1 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_source_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.

    IF   ( gv_filename_out               IS NOT INITIAL ).
      CATCH SYSTEM-EXCEPTIONS file_access_errors = 1.
*       DELETE   DATASET gv_filename_out.
      ENDCATCH.
    ENDIF.

  ELSE.

*eject
* Move the output file to the outbound directory
    CLEAR                                   lv_source_dir.
    MOVE     is_file_list-fp_out_temp    TO lv_source_dir.
    CLEAR                                   lv_source_fname.
    MOVE     is_file_list-fn_out_temp    TO lv_source_fname.
    CLEAR                                   lv_target_dir.
    MOVE     is_file_list-fp_out_main    TO lv_target_dir.
    CLEAR                                   lv_target_fname.
    MOVE     is_file_list-fn_out_main    TO lv_target_fname.

    CLEAR                                   lv_text_c120.
    MOVE     text-c12                    TO lv_text_c120+04.
    MOVE     text-c15                    TO lv_text_c120+12.
    MOVE     lv_source_dir               TO lv_text_c120+20.
    macro_log                             1 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_source_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     text-c16                    TO lv_text_c120+12.
    MOVE     lv_target_dir               TO lv_text_c120+20.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_target_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.

    CLEAR    ls_return.

*   CALL FUNCTION 'ZFI_FILE_HANDLE'
*     EXPORTING
*       I_SOURCE_DIR   = lv_source_dir
*       I_TARGET_DIR   = lv_target_dir
*       I_SOURCE_FNAME = lv_source_fname
*       I_TARGET_FNAME = lv_target_fname
*       I_COMMAND      = 'M'
*     IMPORTING
*       E_RETURN       = ls_return.

    IF     ( ls_return-type EQ 'E' ).
      macro_msg  'B' 'A' text-252.
      RETURN.
    ENDIF.

  ENDIF.

ENDFORM.                    " F_DISPATCH_FILES
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_FILES
*&---------------------------------------------------------------------*
*       Download files to the presentation server
*----------------------------------------------------------------------*
FORM f_download_files
  USING    is_file_list                TYPE gty_file_list.

  DATA:    lv_filename                 TYPE string.

  IF     ( rb_fsprs IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_filename.
  MOVE     is_file_list-fn_out_main      TO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = gt_out_data
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF     ( sy-subrc NE 0 ).
*
  ENDIF.

*eject
  CLEAR                                     lv_filename.
  MOVE     is_file_list-fn_log_main      TO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = gt_log_data
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF     ( sy-subrc NE 0 ).
*
  ENDIF.

ENDFORM.                    " F_DOWNLOAD_FILES
*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL
*&---------------------------------------------------------------------*
*       Send a notification that there was a process error
*----------------------------------------------------------------------*
FORM f_send_email.

  DATA:    lo_send_request             TYPE REF TO cl_bcs
                                       VALUE IS INITIAL.

  CLASS    cl_bcs DEFINITION LOAD.

  DATA:    lo_document                 TYPE REF TO cl_document_bcs
                                       VALUE IS INITIAL,
           lo_sender                   TYPE REF TO if_sender_bcs
                                       VALUE IS INITIAL,
           lo_recipient                TYPE REF TO if_recipient_bcs
                                       VALUE IS INITIAL,
           lx_document_bcs             TYPE REF TO cx_document_bcs,
           lx_send_req_bcs             TYPE REF TO cx_send_req_bcs,
           lx_address_bcs              TYPE REF TO cx_address_bcs.

  DATA:    lt_text                     TYPE soli_tab,
           ls_text                     TYPE soli,
           lv_text                     TYPE so_text255,
           lv_subject                  TYPE text50,
           ls_email                    LIKE LINE OF s_email.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_text_msg                 TYPE string,
           lv_rc                       TYPE numc5.

  IF   ( ( gv_fl_errr_prcs               IS     INITIAL ) OR
         ( rb_fsapl                      IS     INITIAL ) OR
         ( cb_test                       IS NOT INITIAL ) OR
         ( s_email[]                     IS     INITIAL )    ).
    RETURN.
  ENDIF.

  CLEAR    lv_subrc.
  CLEAR    lv_text_msg.

*eject
* Build the email body
  CLEAR                                     lv_subject.
  MOVE     text-ebs                      TO lv_subject.

  CLEAR    lt_text[].

  CLEAR                                     ls_text.
  APPEND   ls_text                       TO lt_text.

  MOVE     text-eb1                      TO ls_text-line.
  APPEND   ls_text                       TO lt_text.

  CLEAR                                     ls_text.
  APPEND   ls_text                       TO lt_text.

  CLEAR                                     lv_text.
  CONCATENATE  text-eb2  sy-sysid  sy-mandt
               text-eb3  sy-datum  sy-uzeit sy-zonlo
                                       INTO lv_text
                               SEPARATED BY space.
  MOVE     lv_text                       TO ls_text-line.
  APPEND   ls_text                       TO lt_text.

  CLEAR                                     ls_text.
  APPEND   ls_text                       TO lt_text.

* MOVE     gv_tx_errr_prcs               TO ls_text-line.
  APPEND   ls_text                       TO lt_text.

  TRY.

      lo_send_request = cl_bcs=>create_persistent( ).

      lo_document = cl_document_bcs=>create_document(
          i_type    = 'RAW'
          i_text    = lt_text
          i_subject = lv_subject ).

      lo_send_request->set_document( lo_document ).

*eject
* Set the sender of the email
      lo_sender = cl_sapuser_bcs=>create( sy-uname ).

      lo_send_request->set_sender(
        EXPORTING
          i_sender = lo_sender ).

* Set the recipients of the email
      CLEAR                                 ls_email.
      LOOP AT  s_email                 INTO ls_email.

        lo_recipient = cl_cam_address_bcs=>create_internet_address(
          ls_email-low ).

        lo_send_request->add_recipient(
          EXPORTING
            i_recipient = lo_recipient ).

        CLEAR  ls_email.
      ENDLOOP.

* Send the email
      CALL METHOD lo_send_request->set_send_immediately
        EXPORTING
          i_send_immediately = space.

      lo_send_request->send(
        EXPORTING
          i_with_error_screen = space ).

    CATCH    cx_document_bcs           INTO lx_document_bcs.
      lv_subrc    = 1.
      lv_text_msg = lx_document_bcs->get_text( ).
    CATCH    cx_send_req_bcs           INTO lx_send_req_bcs.
      lv_subrc    = 2.
      lv_text_msg = lx_send_req_bcs->get_text( ).
    CATCH    cx_address_bcs            INTO lx_address_bcs.
      lv_subrc    = 3.
      lv_text_msg = lx_address_bcs->get_text( ).

  ENDTRY.

*eject
  IF     ( lv_subrc IS INITIAL ).

    COMMIT WORK.

  ELSE.

    lv_rc = lv_subrc.

    CLEAR                                   lv_text.
    CONCATENATE                             text-261 text-232
                                            lv_rc lv_text_msg
                                       INTO lv_text
                               SEPARATED BY space.
    macro_msg                       'B' 'A' lv_text.

  ENDIF.

ENDFORM.                    " F_SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       Validate the selection screen
*----------------------------------------------------------------------*
FORM f_validate_selection_screen.

  IF     ( p_erpid  IS INITIAL ).
    macro_msg                       'B' 'A' text-101.
  ENDIF.

  IF     ( rb_fsapl IS NOT INITIAL ).

    IF   ( p_fpth1a IS INITIAL ).
      macro_msg                     'B' 'A' text-111.
    ENDIF.
    IF   ( p_fnam1a IS INITIAL ).
      macro_msg                     'B' 'A' text-112.
    ENDIF.
    IF   ( p_fregxa IS INITIAL ).
      macro_msg                     'B' 'A' text-113.
    ENDIF.
    IF   ( p_farc1a IS INITIAL ).
      macro_msg                     'B' 'A' text-114.
    ENDIF.
    IF   ( p_fpth2a IS INITIAL ).
      macro_msg                     'B' 'A' text-121.
    ENDIF.
    IF   ( p_fnam2a IS INITIAL ).
      macro_msg                     'B' 'A' text-122.
    ENDIF.
    IF   ( p_farc2a IS INITIAL ).
      macro_msg                     'B' 'A' text-123.
    ENDIF.
    IF   ( p_fpth3a IS INITIAL ).
      macro_msg                     'B' 'A' text-131.
    ENDIF.
    IF   ( p_fnam3a IS INITIAL ).
      macro_msg                     'B' 'A' text-132.
    ENDIF.

  ELSEIF ( rb_fsprs IS NOT INITIAL ).

    IF   ( p_fnam1p IS INITIAL ).
      macro_msg                     'B' 'A' text-112.
    ENDIF.
    IF   ( p_fnam2p IS INITIAL ).
      macro_msg                     'B' 'A' text-122.
    ENDIF.
    IF   ( p_fnam3p IS INITIAL ).
      macro_msg                     'B' 'A' text-132.
    ENDIF.

  ENDIF.

ENDFORM.                    " F_VALIDATE_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  F_DEL_PALT_PAY
*&---------------------------------------------------------------------*
*       To delete permitted alternate payee
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_del_palt_pay USING iv_iap_vendor_id     TYPE z_iap_vendor_id
                          iv_lifnr             TYPE lifnr.

  DATA: ls_opt        TYPE ctu_params,
        lv_tabix      TYPE sytabix,
        ls_pap        TYPE gty_pap,
        lt_pap        TYPE STANDARD TABLE OF gty_pap,
        lv_lines      TYPE i,
        lv_mod        TYPE i,
        lv_div        TYPE i,
        lv_cline      TYPE numc2,
        lv_fvar       TYPE fnam_____4,
        lv_flg        TYPE c. "To check whether payee are available or not.

  CONSTANTS lc_xk02(4) TYPE c VALUE 'XK02'.

  CLEAR: gt_bdcdata[],
         gt_messages[],
         gs_pap,
         ls_pap,
         lv_flg,
         ls_opt.

  ls_opt-dismode = 'N'.
  ls_opt-defsize = gc_x.
  ls_opt-updmode = 'S'.

  READ TABLE gt_pap_d INTO ls_pap WITH KEY zz_iap_vendor_id = iv_iap_vendor_id.
  IF sy-subrc EQ 0.
    CLEAR lv_tabix.
    lv_tabix = sy-tabix.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0101'.
    PERFORM f_bdc_field  USING 'RF02K-LIFNR'   iv_lifnr.
    PERFORM f_bdc_field  USING 'RF02K-BUKRS'   ls_pap-bukrs.
    PERFORM f_bdc_field  USING 'RF02K-D0130'   'X'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '/00'.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0130'.
    PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFBK-BANKS(01)'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=LFZA'.

    CLEAR: lt_pap[],
           gt_lfza[],
           lv_lines.

    lt_pap[] = gt_pap_d[].
    SORT lt_pap BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_pap COMPARING lifnr.

    SELECT empfk FROM lfza
                 INTO TABLE gt_lfza
                 FOR ALL ENTRIES IN lt_pap
                 WHERE lifnr = lt_pap-lifnr
                 AND   bukrs = ''.
    IF sy-subrc EQ 0.
      SORT gt_lfza.
    ENDIF.

    CLEAR lt_pap[].
    LOOP AT gt_pap_d INTO gs_pap FROM lv_tabix.
      IF gs_pap-zz_iap_vendor_id NE ls_pap-zz_iap_vendor_id.
        EXIT.
      ENDIF.
      APPEND gs_pap TO lt_pap.
    ENDLOOP.

    SORT lt_pap BY empfk.
    DESCRIBE TABLE lt_pap LINES lv_lines.

    LOOP AT lt_pap INTO gs_pap.

      READ TABLE gt_lfza WITH KEY empfk = gs_pap-empfk
                         BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.

        lv_flg = gc_x. " Payee updation confirmation

        CLEAR lv_tabix.
        lv_tabix = sy-tabix.

        lv_div = lv_tabix DIV 7.
        lv_mod = lv_tabix MOD 7.
        IF NOT lv_mod IS INITIAL.
          lv_cline = lv_mod.
        ELSE.
          lv_cline = 7.
        ENDIF.

        DO lv_div TIMES.
          PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
          PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFZA-EMPFK(01)'.
          PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=P+'.
        ENDDO.


        CONCATENATE 'LFZA-EMPFK(' lv_cline ')' INTO lv_fvar.
        PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
        PERFORM f_bdc_field  USING 'BDC_CURSOR'    lv_fvar."'LFZA-EMPFK(01)'.
        PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=LDEL'.
        CLEAR lv_fvar.
        CONCATENATE 'SELECTED(' lv_cline ')' INTO lv_fvar.
        PERFORM f_bdc_field  USING lv_fvar         'X'.

        PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
        CONCATENATE 'LFZA-EMPFK(' lv_cline ')' INTO lv_fvar.
        PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=BACK'.

* If it is last record then should be saved else again deletion process
        IF sy-tabix LT lv_lines.
          PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0130'.
          PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFBK-BANKS(01)'.
          PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=LFZA'.
        ELSE.
          PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0130'.
          PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFBK-BANKS(01)'.
          PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=UPDA'.
        ENDIF.

* Delete the record from the database extract table for next run
        DELETE gt_lfza INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF NOT gt_bdcdata[] IS INITIAL AND
     NOT lv_flg       IS INITIAL.

    CALL TRANSACTION lc_xk02 USING         gt_bdcdata
                             OPTIONS FROM  ls_opt
                             MESSAGES INTO gt_messages.

    PERFORM f_bdc_errors USING iv_lifnr.

  ENDIF.
ENDFORM.                    " F_DEL_PALT_PAY
*&---------------------------------------------------------------------*
*&      Form  F_DEL_PALT_CC
*&---------------------------------------------------------------------*
*       To delete permitted alternate payee at company code
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_del_palt_cc  USING    iv_iap_vendor_id     TYPE z_iap_vendor_id
                             iv_lifnr             TYPE lifnr.
  DATA: ls_opt        TYPE ctu_params,
        lv_tabix      TYPE sytabix,
        ls_pap        TYPE gty_pap,
        lt_pap        TYPE STANDARD TABLE OF gty_pap,
        lv_lines      TYPE i,
        lv_mod        TYPE i,
        lv_div        TYPE i,
        lv_cline      TYPE numc2,
        lv_fvar       TYPE fnam_____4,
        lv_flg        TYPE c. "To check whether payee are available or not.

  CONSTANTS lc_xk02(4) TYPE c VALUE 'XK02'.

  CLEAR: gt_bdcdata[],
         gt_messages[],
         gs_pap,
         ls_pap,
         lv_flg,
         ls_opt.

  ls_opt-dismode = 'N'.
  ls_opt-defsize = gc_x.
  ls_opt-updmode = 'S'.

  READ TABLE gt_pap_cd INTO ls_pap WITH KEY zz_iap_vendor_id = iv_iap_vendor_id.
  IF sy-subrc EQ 0.
    CLEAR lv_tabix.
    lv_tabix = sy-tabix.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0101'.
    PERFORM f_bdc_field  USING 'RF02K-LIFNR'   iv_lifnr.
    PERFORM f_bdc_field  USING 'RF02K-BUKRS'   ls_pap-bukrs.
    PERFORM f_bdc_field  USING 'RF02K-D0215'   'X'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '/00'.

    PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0215'.
    PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFB1-ZTERM'.
    PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=LFZA'.

    CLEAR: lt_pap[],
           gt_lfza[],
           lv_lines.

    lt_pap[] = gt_pap_cd[].
    SORT lt_pap BY lifnr bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_pap COMPARING lifnr bukrs.

    SELECT empfk FROM lfza
                 INTO TABLE gt_lfza
                 FOR ALL ENTRIES IN lt_pap
                 WHERE lifnr = lt_pap-lifnr
                 AND   bukrs = lt_pap-bukrs.
    IF sy-subrc EQ 0.
      SORT gt_lfza.
    ENDIF.

    CLEAR lt_pap[].
    LOOP AT gt_pap_cd INTO gs_pap FROM lv_tabix.
      IF gs_pap-zz_iap_vendor_id NE ls_pap-zz_iap_vendor_id.
        EXIT.
      ENDIF.
      APPEND gs_pap TO lt_pap.
    ENDLOOP.

    SORT lt_pap BY empfk.
    DESCRIBE TABLE lt_pap LINES lv_lines.

    LOOP AT lt_pap INTO gs_pap.
      READ TABLE gt_lfza WITH KEY empfk = gs_pap-empfk
                   BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.

        lv_flg = gc_x. " Payee updation confirmation

        CLEAR lv_tabix.
        lv_tabix = sy-tabix.

        lv_div = lv_tabix DIV 7.
        lv_mod = lv_tabix MOD 7.
        IF NOT lv_mod IS INITIAL.
          lv_cline = lv_mod.
        ELSE.
          lv_cline = 7.
        ENDIF.

        DO lv_div TIMES.
          PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
          PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFZA-EMPFK(01)'.
          PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=P+'.
        ENDDO.

        CONCATENATE 'LFZA-EMPFK(' lv_cline ')' INTO lv_fvar.
        PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
        PERFORM f_bdc_field  USING 'BDC_CURSOR'    lv_fvar."'LFZA-EMPFK(01)'.
        PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=LDEL'.
        CLEAR lv_fvar.
        CONCATENATE 'SELECTED(' lv_cline ')' INTO lv_fvar.
        PERFORM f_bdc_field  USING lv_fvar         'X'.

        PERFORM f_bdc_dynpro USING 'SAPMF02K'      '1130'.
        CONCATENATE 'LFZA-EMPFK(' lv_cline ')' INTO lv_fvar.
        PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=BACK'.

* If it is last record then should be saved else again deletion process
        IF sy-tabix LT lv_lines.
          PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0215'.
          PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFB1-ZTERM'.
          PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=LFZA'.
        ELSE.
          PERFORM f_bdc_dynpro USING 'SAPMF02K'      '0215'.
          PERFORM f_bdc_field  USING 'BDC_CURSOR'    'LFB1-ZTERM'.
          PERFORM f_bdc_field  USING 'BDC_OKCODE'    '=UPDA'.
        ENDIF.

* Delete the record from the database extract table for next run
        DELETE gt_lfza INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF NOT gt_bdcdata[] IS INITIAL AND
     NOT lv_flg       IS INITIAL.

    CALL TRANSACTION lc_xk02 USING         gt_bdcdata
                             OPTIONS FROM  ls_opt
                             MESSAGES INTO gt_messages.

    PERFORM f_bdc_errors USING iv_lifnr.

  ENDIF.
ENDFORM.                    " F_DEL_PALT_CC
*&---------------------------------------------------------------------*
*&      Form  F_UPD_ERRORS
*&---------------------------------------------------------------------*
*       To update the errors to the log
*----------------------------------------------------------------------*
*      -->P_LS_ERR_MESSAGES  text
*----------------------------------------------------------------------*
FORM f_upd_errors  USING    is_return TYPE cvis_message
                            iv_vendor TYPE lifnr.

  FIELD-SYMBOLS: <fs_return>           TYPE bapiret2.

  DATA: lv_fl_type_e                TYPE xflag,
        lv_fl_type_w                TYPE xflag,
        lv_text_c120                TYPE text120,
        lv_msgty                    TYPE symsgty,
        lv_text                     TYPE string,
        lv_text_p                   TYPE string,
        lv_tabix                    TYPE sytabix.

  LOOP AT  is_return-messages      ASSIGNING <fs_return>.
    IF   ( <fs_return>-type              CA 'AaEe' ).
      lv_fl_type_e = abap_true.
    ENDIF.
    IF   ( <fs_return>-type              CA 'Ww'   ).
      lv_fl_type_w = abap_true.
    ENDIF.
    CLEAR                                   lv_text_c120.
    MOVE   <fs_return>-type              TO lv_text_c120+02(01).
    MOVE   <fs_return>-id                TO lv_text_c120+05(10).
    MOVE   <fs_return>-number            TO lv_text_c120+17(03).
    MOVE   <fs_return>-message           TO lv_text_c120+22(98).
    macro_log                             0 lv_text_c120.
  ENDLOOP.

*  IF     ( lv_fl_type_e                  IS NOT INITIAL ).
*    MOVE     'E'                         TO lv_msgty.
*    DELETE   is_return-messages        WHERE type CA 'SsIi'.
*  ELSEIF ( lv_fl_type_w                  IS NOT INITIAL ).
*    MOVE     'W'                         TO lv_msgty.
*  ELSE.
*    MOVE     'S'                         TO lv_msgty.
*  ENDIF.

*eject
  CLEAR    lv_text.
  CLEAR    lv_text_p.

  LOOP AT  is_return-messages          ASSIGNING <fs_return>.
*    lv_tabix = sy-tabix.
*    CLEAR                                   lv_text.
*    CONCATENATE                             <fs_return>-number '('
*                                            <fs_return>-id     ')-'
*                                            <fs_return>-message
*                                       INTO lv_text.
*    IF     ( lv_tabix EQ 1 ).
*      CONCATENATE                           iv_ritm_id      gc_delim
*                                            lv_msgty        gc_delim
*                                            lv_text
*                                       INTO lv_text_p.
*    ELSE.
*      CONCATENATE                           lv_text_p '*|*'
*                                            lv_text
*                                       INTO lv_text_p
*                               SEPARATED BY space.
*    ENDIF.

    CLEAR:                                  lv_text,
                                            lv_text_p,
                                            lv_msgty.

    CONCATENATE                             <fs_return>-number '('
                                            <fs_return>-id     ')-'
                                            <fs_return>-message
                                       INTO lv_text.

    IF   ( <fs_return>-type              CA 'AaEe' ).
      MOVE     'E'                         TO lv_msgty.
    ELSEIF   ( <fs_return>-type          CA 'Ww'   ).
      MOVE     'S'                         TO lv_msgty.
    ELSE.
      MOVE     'S'                         TO lv_msgty.
    ENDIF.

    IF iv_vendor IS INITIAL.
      CONCATENATE                             gs_vle-erpid
                                              gs_vle-zz_iap_ritm_id
                                              gs_vle-zz_iap_vendor_id
                                              gs_vle-name1
                                              gs_vle-lifnr
                                              lv_msgty
                                              lv_text
                                         INTO lv_text_p
                                         SEPARATED BY gc_delim.
    ELSE.
      CONCATENATE                             gs_vle-erpid
                                              gs_vle-zz_iap_ritm_id
                                              gs_vle-zz_iap_vendor_id
                                              gs_vle-name1
                                              iv_vendor
                                              lv_msgty
                                              lv_text
                                         INTO lv_text_p
                                         SEPARATED BY gc_delim.
    ENDIF.

    IF       ( rb_fsapl IS NOT INITIAL ).

      TRANSFER lv_text_p                   TO gv_filename_out.

    ELSE.

      CLEAR                                   gs_out_data.
      MOVE     lv_text_p                   TO gs_out_data.
      APPEND   gs_out_data                 TO gt_out_data.

    ENDIF.

  ENDLOOP.

*  IF       ( rb_fsapl IS NOT INITIAL ).
*
*    TRANSFER lv_text_p                   TO gv_filename_out.
*
*  ELSE.
*
*    CLEAR                                   gs_out_data.
*    MOVE     lv_text_p                   TO gs_out_data.
*    APPEND   gs_out_data                 TO gt_out_data.
*
*  ENDIF.

  IF gv_trns IS INITIAL. " To update the transaction count only for once
    gv_trns = gc_x.
    ADD      1                             TO gv_cnt_doc_trns.
  ENDIF.

  IF     ( lv_fl_type_e                  IS NOT INITIAL ).
    IF gv_errr IS INITIAL .
      ADD    1                           TO gv_cnt_doc_errr.
    ENDIF.
    gv_errr = gc_x.
  ELSEIF ( lv_fl_type_w                  IS NOT INITIAL ).
    IF gv_wrng IS INITIAL.
      ADD    1                           TO gv_cnt_doc_wrng.
    ENDIF.
    gv_wrng = gc_x.
  ELSE.
    IF gv_errr IS INITIAL AND
       gv_wrng IS INITIAL AND
       gv_pass IS INITIAL.
      ADD    1                           TO gv_cnt_doc_pass.
    ENDIF.
    gv_pass = gc_x.
  ENDIF.
ENDFORM.                    " F_UPD_ERRORS
*&---------------------------------------------------------------------*
*&      Form  F_BDC_ERRORS
*&---------------------------------------------------------------------*
*       Update BDC Errors
*----------------------------------------------------------------------*
FORM f_bdc_errors  USING   iv_vendor TYPE lifnr.

  FIELD-SYMBOLS: <fs_return>           TYPE bdcmsgcoll.

  DATA: lv_fl_type_e                TYPE xflag,
        lv_fl_type_w                TYPE xflag,
        lv_text_c120                TYPE text120,
        lv_msgty                    TYPE symsgty,
        lv_text                     TYPE string,
        lv_text_p                   TYPE string,
        lv_tabix                    TYPE sytabix,
        lv_msg                      TYPE sylisel,
        ls_msg                      TYPE bapiret2.


  LOOP AT  gt_messages      ASSIGNING <fs_return>.
    CLEAR                                   ls_msg.

    IF   ( <fs_return>-msgtyp            CA 'AaEe' ).
      lv_fl_type_e = abap_true.
    ENDIF.
    IF   ( <fs_return>-msgtyp            CA 'Ww'   ).
      lv_fl_type_w = abap_true.
    ENDIF.
    CLEAR                                   lv_text_c120.
    MOVE   <fs_return>-msgtyp            TO lv_text_c120+02(01).
    MOVE   <fs_return>-msgid             TO lv_text_c120+05(10).
    MOVE   <fs_return>-msgnr             TO lv_text_c120+17(03).

    MOVE:  <fs_return>-msgnr             TO ls_msg-number,
           <fs_return>-msgv1             TO ls_msg-message_v1,
           <fs_return>-msgv2             TO ls_msg-message_v2,
           <fs_return>-msgv3             TO ls_msg-message_v3,
           <fs_return>-msgv4             TO ls_msg-message_v4.

    CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
      EXPORTING
        language          = sy-langu
        message_id        = <fs_return>-msgid
        message_number    = ls_msg-number
        message_var1      = ls_msg-message_v1
        message_var2      = ls_msg-message_v2
        message_var3      = ls_msg-message_v3
        message_var4      = ls_msg-message_v4
      IMPORTING
        message_text      = lv_msg
      EXCEPTIONS
        message_not_found = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    MOVE   lv_msg                        TO lv_text_c120+22(98).
    macro_log                             0 lv_text_c120.
  ENDLOOP.

*  IF     ( lv_fl_type_e                  IS NOT INITIAL ).
*    MOVE     'E'                         TO lv_msgty.
*    DELETE   gt_messages                 WHERE msgtyp CA 'SsIi'.
*  ELSEIF ( lv_fl_type_w                  IS NOT INITIAL ).
*    MOVE     'W'                         TO lv_msgty.
*  ELSE.
*    MOVE     'S'                         TO lv_msgty.
*  ENDIF.

*eject
  CLEAR    lv_text.
  CLEAR    lv_text_p.

  LOOP AT  gt_messages              ASSIGNING <fs_return>.
*    lv_tabix = sy-tabix.
*    CLEAR                                   lv_text.
*    CLEAR                                   lv_msg.
*    CLEAR                                   ls_msg.

    MOVE:  <fs_return>-msgnr             TO ls_msg-number,
           <fs_return>-msgv1             TO ls_msg-message_v1,
           <fs_return>-msgv2             TO ls_msg-message_v2,
           <fs_return>-msgv3             TO ls_msg-message_v3,
           <fs_return>-msgv4             TO ls_msg-message_v4.

    CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
      EXPORTING
        language          = sy-langu
        message_id        = <fs_return>-msgid
        message_number    = ls_msg-number
        message_var1      = ls_msg-message_v1
        message_var2      = ls_msg-message_v2
        message_var3      = ls_msg-message_v3
        message_var4      = ls_msg-message_v4
      IMPORTING
        message_text      = lv_msg
      EXCEPTIONS
        message_not_found = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*    CONCATENATE                             <fs_return>-msgnr  '('
*                                            <fs_return>-msgid  ')-'
*                                            lv_msg
*                                       INTO lv_text.
*    IF     ( lv_tabix EQ 1 ).
*      CONCATENATE                           iv_ritm_id      gc_delim
*                                            lv_msgty        gc_delim
*                                            lv_text
*                                       INTO lv_text_p.
*    ELSE.
*      CONCATENATE                           lv_text_p '*|*'
*                                            lv_text
*                                       INTO lv_text_p
*                               SEPARATED BY space.
*    ENDIF.

    CLEAR:                                  lv_text,
                                            lv_text_p,
                                            lv_msgty.

    CONCATENATE                             <fs_return>-msgnr  '('
                                            <fs_return>-msgid  ')-'
                                            lv_msg
                                       INTO lv_text.

    IF   ( <fs_return>-msgtyp            CA 'AaEe' ).
      MOVE     'E'                         TO lv_msgty.
    ELSEIF   ( <fs_return>-msgtyp          CA 'Ww'   ).
      MOVE     'S'                       TO lv_msgty.
    ELSE.
      MOVE     'S'                         TO lv_msgty.
    ENDIF.

    IF iv_vendor IS INITIAL.
      CONCATENATE                             gs_vle-erpid
                                              gs_vle-zz_iap_ritm_id
                                              gs_vle-zz_iap_vendor_id
                                              gs_vle-name1
                                              gs_vle-lifnr
                                              lv_msgty
                                              lv_text
                                         INTO lv_text_p
                                         SEPARATED BY gc_delim.
    ELSE.
      CONCATENATE                             gs_vle-erpid
                                              gs_vle-zz_iap_ritm_id
                                              gs_vle-zz_iap_vendor_id
                                              gs_vle-name1
                                              iv_vendor
                                              lv_msgty
                                              lv_text
                                         INTO lv_text_p
                                         SEPARATED BY gc_delim.
    ENDIF.

    IF       ( rb_fsapl IS NOT INITIAL ).

      TRANSFER lv_text_p                   TO gv_filename_out.

    ELSE.

      CLEAR                                   gs_out_data.
      MOVE     lv_text_p                   TO gs_out_data.
      APPEND   gs_out_data                 TO gt_out_data.

    ENDIF.

  ENDLOOP.

*  IF       ( rb_fsapl IS NOT INITIAL ).
*
*    TRANSFER lv_text_p                   TO gv_filename_out.
*
*  ELSE.
*
*    CLEAR                                   gs_out_data.
*    MOVE     lv_text_p                   TO gs_out_data.
*    APPEND   gs_out_data                 TO gt_out_data.
*
*  ENDIF.


  IF     ( lv_fl_type_e                  IS NOT INITIAL ).
    IF gv_errr IS INITIAL .
      ADD    1                           TO gv_cnt_doc_errr.
    ENDIF.
    gv_errr = gc_x.
  ELSEIF ( lv_fl_type_w                  IS NOT INITIAL ).
    IF gv_wrng IS INITIAL.
      ADD    1                           TO gv_cnt_doc_wrng.
    ENDIF.
    gv_wrng = gc_x.
  ELSE.
    IF gv_errr IS INITIAL AND
       gv_wrng IS INITIAL AND
       gv_pass IS INITIAL.
      ADD    1                           TO gv_cnt_doc_pass.
    ENDIF.
    gv_pass = gc_x.
  ENDIF.

ENDFORM.                    " F_BDC_ERRORS
