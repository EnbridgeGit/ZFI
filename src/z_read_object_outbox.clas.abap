class Z_READ_OBJECT_OUTBOX definition
  public
  create public .

public section.
*"* public components of class Z_READ_OBJECT_OUTBOX"
*"* do not include other source files here!!!

  class-methods READ_OBJECT_OUTBOX
    importing
      !I_INSTID_A type SIBFBORIID
      !I_TYPEID_A type SIBFTYPEID
      !I_CATID_A type SIBFCATID default 'BO'
    exporting
      !ETAB_SOOD type ZTAB_SOOD
      !ETAB_SOOS type ZTAB_SOOS
      !ETAB_SO_NOTES type ZSO_NOTES_TAB
    raising
      CX_OBL_PARAMETER_ERROR .
protected section.
*"* protected components of class Z_READ_OBJECT_OUTBOX"
*"* do not include other source files here!!!
private section.
*"* private components of class Z_READ_OBJECT_OUTBOX" "
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z_READ_OBJECT_OUTBOX IMPLEMENTATION.


method READ_OBJECT_OUTBOX.
*In Case of FIPP, I_INSTID_A = Company code, document + year i.e. 01  01200000092011.
*In case of BUS2081, Document + Year i.e. 52000000092011
  DATA: lv_exception_text TYPE sotr_conc VALUE 'No data found...',
          lt_srbcsbrel TYPE TABLE OF srbcsbrel,
          ls_srbcsbrel TYPE srbcsbrel,
          lt_bcst_sr TYPE TABLE OF bcst_sr,
          lv_os_guid TYPE os_guid.

  DATA: lv_doc_oid TYPE bcst_sr-doc_oid
              VALUE '00000000000000000000000000000000',
        lv_doc_cls TYPE bcst_sr-doc_cls
        VALUE '00000000000000000000000000000000'.
  DATA: lv_recnam TYPE tdline,
        lv_tabix TYPE sy-tabix,
        ls_sood TYPE sood,
        ls_soodk TYPE soodk,
        ls_soos TYPE soos,
        ls_objcont  TYPE soli,
        lt_objhead  TYPE TABLE OF soli,
        lt_objcont  TYPE TABLE OF soli,
        lt_objpara  TYPE TABLE OF selc,
        lt_objparb  TYPE TABLE OF soop1,
        lt_tdline  TYPE TABLE OF zso_notes,
        ls_tdline  TYPE zso_notes.


  IF i_instid_a IS INITIAL OR
     i_typeid_a IS INITIAL OR
     i_catid_a IS INITIAL.

    RAISE EXCEPTION TYPE cx_obl_parameter_error
      EXPORTING
        textid    = cx_obl_parameter_error=>invalid_object
*       previous  =
        method    = 'READ_OBJECT_OUTBOX'
        classname = 'Z_READ_OBJECT_OUTBOX'.

  ENDIF.

  SELECT * FROM srbcsbrel INTO TABLE lt_srbcsbrel
    WHERE instid_a = i_instid_a
      AND typeid_a = i_typeid_a
      AND catid_a  = i_catid_a.
  IF lt_srbcsbrel IS INITIAL.
*     lv_exception_text = 'No data found...'.
    RAISE EXCEPTION TYPE cx_obl_parameter_error
      EXPORTING
        textid    = lv_exception_text
*       previous  =
        method    = 'READ_OBJECT_OUTBOX'
        classname = 'Z_READ_OBJECT_OUTBOX'.
  ENDIF.
  LOOP AT lt_srbcsbrel INTO ls_srbcsbrel.
    lv_os_guid = ls_srbcsbrel-instid_b. "(32).
    SELECT * FROM bcst_sr APPENDING TABLE lt_bcst_sr
          WHERE os_guid = lv_os_guid.
  ENDLOOP.
  IF lt_bcst_sr IS INITIAL.
*     lv_exception_text = 'No data found...'.
    RAISE EXCEPTION TYPE cx_obl_parameter_error
      EXPORTING
        textid    = lv_exception_text
*       previous  =
        method    = 'READ_OBJECT_OUTBOX'
        classname = 'Z_READ_OBJECT_OUTBOX'.
  ENDIF.
  DELETE lt_bcst_sr WHERE doc_oid = lv_doc_oid OR
                          doc_cls = lv_doc_cls.
  SELECT * FROM sood INTO TABLE etab_sood
    FOR ALL ENTRIES IN lt_bcst_sr
    WHERE if_doc_bcs = lt_bcst_sr-doc_oid
      AND if_doc_cls = lt_bcst_sr-doc_cls.
  IF etab_sood IS INITIAL.
*     lv_exception_text = 'No data found...'.
    RAISE EXCEPTION TYPE cx_obl_parameter_error
      EXPORTING
        textid    = lv_exception_text
*       previous  =
        method    = 'READ_OBJECT_OUTBOX'
        classname = 'Z_READ_OBJECT_OUTBOX'.
  ENDIF.

  SELECT * FROM soos INTO TABLE etab_soos
      FOR ALL ENTRIES IN etab_sood
    WHERE objtp = etab_sood-objtp
      AND objyr = etab_sood-objyr
      AND objno = etab_sood-objno.
  SORT etab_soos BY objtp objyr objno.
  CLEAR: lt_tdline.
  LOOP AT etab_sood INTO ls_sood.
    ls_soodk-objtp = ls_sood-objtp.
    ls_soodk-objyr = ls_sood-objyr.
    ls_soodk-objno = ls_sood-objno.
    CLEAR: lv_recnam,
           lt_objhead,
           lt_objcont,
           lt_objpara,
           lt_objparb,
           lv_tabix.
    READ TABLE etab_soos WITH KEY objtp = ls_sood-objtp
                                  objyr = ls_sood-objyr
                                  objno = ls_sood-objno
                                  TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT etab_soos INTO ls_soos FROM lv_tabix.
      IF ls_soos-objtp <> ls_sood-objtp OR
         ls_soos-objyr <> ls_sood-objyr OR
         ls_soos-objno <> ls_sood-objno.
        EXIT.
      ENDIF.
      IF lv_recnam is INITIAL.
         lv_recnam = ls_soos-recnam.
      ELSE.
         CONCATENATE lv_recnam ',' ls_soos-recnam INTO lv_recnam
                                             SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'SO_OBJECT_GET_CONTENT'
      EXPORTING
        object_id        = ls_soodk
*       ARCHIVE          =
      TABLES
        objhead          = lt_objhead
        objcont          = lt_objcont
        objpara          = lt_objpara
        objparb          = lt_objparb
      EXCEPTIONS
        archive_error    = 1
        object_not_exist = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF lt_objcont[] IS NOT INITIAL.
      LOOP AT lt_objcont INTO ls_objcont.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN ls_objcont-line WITH space.
        ls_tdline-note1 = ls_objcont-line(128).
        ls_tdline-note2 = ls_objcont-line+128(127).
        ls_tdline-cronam = ls_sood-cronam.
        ls_tdline-crdat  = ls_sood-crdat.
        ls_tdline-crtim  = ls_sood-crtim.
        ls_tdline-objtp = ls_sood-objtp.
        ls_tdline-objyr = ls_sood-objyr.
        ls_tdline-objno = ls_sood-objno.
        ls_tdline-recnam = lv_recnam.
        APPEND ls_tdline TO lt_tdline.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  etab_so_notes[] = lt_tdline[].

endmethod.
ENDCLASS.
