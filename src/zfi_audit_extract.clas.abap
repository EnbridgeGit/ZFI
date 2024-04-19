class ZFI_AUDIT_EXTRACT definition
  public
  final
  create public .

public section.
*"* public components of class ZFI_AUDIT_EXTRACT
*"* do not include other source files here!!!
  type-pools SLIS .

  types:
    BEGIN OF gty_field_pos,
               position   TYPE tabfdpos,
               fieldname  TYPE fieldname,
          END OF gty_field_pos .
  types:
    BEGIN OF gty_wh_fields.
            INCLUDE TYPE fieldnames.
    TYPES:range_num TYPE i,
          END OF gty_wh_fields .
  types:
    tt_ddfields   TYPE STANDARD TABLE OF dfies         INITIAL SIZE 0 .
  types:
    tt_field_pos  TYPE STANDARD TABLE OF gty_field_pos INITIAL SIZE 0 .
  types:
    tt_wh_fields  TYPE STANDARD TABLE OF gty_wh_fields INITIAL SIZE 0 .

  constants GC_A type AS4LOCAL value 'A'. "#EC NOTEXT
  constants GC_X type CHAR1 value 'X'. "#EC NOTEXT
  constants GC_VER type AS4VERS value '0000'. "#EC NOTEXT
  constants GC_MANDT type CHAR5 value 'MANDT'. "#EC NOTEXT

  methods INPUT_VALIDATIONS
    importing
      !I_TABNAME type TABNAME
      !IT_FIELDS type TT_WH_FIELDS
    exporting
      !ET_FIELDLIST type TT_FIELD_POS
      !E_FIELD_SEQUENCE type CHAR90
      !ET_DDFIELDS type TT_DDFIELDS
      !E_PACK_SIZE type I .
  methods VALIDATE_TABLE
    importing
      !I_TABLE type TABNAME .
  methods VALIDATE_DATA
    importing
      !I_FIELD_NAME type FIELDNAME
      !IT_RANGE_VALID type /SAPDII/TWTY_RSDS_SELOPT
      !I_CHECK_TAB type CHECKTABLE optional
      !I_TABNAME type TABNAME optional .
  methods BUILD_DYN_RANGE
    importing
      !ET_VALUE type /SAPDII/TWTY_RSDS_SELOPT
    exporting
      !IT_RANGES type /SAPDII/TWTY_RSDS_SELOPT .
  methods CHECK_PRIMARY_KEY
    importing
      !IT_FIELDNAMES type TT_DDFIELDS
      !IT_FIELDLIST type TT_WH_FIELDS .
  methods BUILD_DYNAMIC_TABLE
    importing
      !I_FIELDNAME type FIELDNAME optional
      !I_CHECKTAB type CHECKTABLE optional
      !I_TABNAME type TABNAME optional
    returning
      value(ET_INTTAB) type ref to DATA .
  methods BUILD_SELECT_QUERY
    importing
      !I_DBTAB type TABNAME
      !IT_FIELDS_WHERE type TT_WH_FIELDS
      !IT_FIELD_WHERE_DB type TT_FIELD_POS
      !IT_RANGE1 type /SAPDII/TWTY_RSDS_SELOPT
      !IT_RANGE2 type /SAPDII/TWTY_RSDS_SELOPT
      !IT_RANGE3 type /SAPDII/TWTY_RSDS_SELOPT
      !IT_RANGE4 type /SAPDII/TWTY_RSDS_SELOPT
      !IT_RANGE5 type /SAPDII/TWTY_RSDS_SELOPT
      !IT_RANGE6 type /SAPDII/TWTY_RSDS_SELOPT
      !I_PACK_SIZE type I
      !IT_RANGE7 type /SAPDII/TWTY_RSDS_SELOPT
    changing
      value(CT_INTTAB) type ref to DATA .
  methods CREATE_FIELDCAT
    importing
      !I_DBTAB type TABNAME
    exporting
      !ET_FIELDCAT type LVC_T_FCAT .
  methods USE_CONVERSION_EXIT
    importing
      !I_FIELD type FIELDNAME
      !I_TABLE type TABNAME
    changing
      !CT_VALUES type /SAPDII/TWTY_RSDS_SELOPT .
  type-pools ABAP .
  methods GET_INCL_COMPONENTS
    importing
      !IM_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR
    changing
      !CT_COMPONENTS type ABAP_COMPONENT_TAB .
protected section.
*"* protected components of class ZFI_AUDIT_EXTRACT
*"* do not include other source files here!!!
private section.
*"* private components of class ZFI_AUDIT_EXTRACT
*"* do not include other source files here!!!

  methods VALIDATE_FIELDNAME
    importing
      !I_TAB type TABNAME
    exporting
      !ET_DDFIELDS type TT_DDFIELDS
      !E_STRUCT_SIZE type I .
  methods VALIDATE_WHEREFIELD
    importing
      !IT_DDFIELDS type TT_DDFIELDS
      !I_FIELD type CHAR30
    exporting
      !ET_FIELDPOS type TT_FIELD_POS .
  methods VALIDATE_SEQUENCE
    importing
      !IT_FIELD_POS type TT_FIELD_POS
    changing
      !C_SEQUENCE type CHAR90 .
ENDCLASS.



CLASS ZFI_AUDIT_EXTRACT IMPLEMENTATION.


METHOD build_dynamic_table.
*field symbols declaration
  FIELD-SYMBOLS :<lfs_dyn_table>    TYPE STANDARD TABLE,
                 <lfs_dyn_wa>       TYPE any.
  TYPES:lty_fieldref TYPE REF TO data.
  DATA: lv_fieldname TYPE rollname.
  DATA: ls_dynwa     TYPE lty_fieldref.
  TRY.
      IF i_tabname IS NOT INITIAL.
        CREATE DATA et_inttab TYPE STANDARD TABLE OF (i_tabname).
        ASSIGN et_inttab->* TO <lfs_dyn_table>.
      ELSE.
        SELECT rollname
         FROM dd03l
         INTO lv_fieldname
          UP TO 1 ROWS
        WHERE tabname   = i_checktab
         AND  fieldname = i_fieldname.
        ENDSELECT.
        IF sy-subrc IS INITIAL.
          CREATE DATA ls_dynwa TYPE (lv_fieldname).
          ASSIGN ls_dynwa->* TO <lfs_dyn_wa>.
          CREATE DATA et_inttab TYPE STANDARD TABLE OF (lv_fieldname).
          ASSIGN et_inttab->* TO <lfs_dyn_table>.
          APPEND <lfs_dyn_wa> TO <lfs_dyn_table>.
        ENDIF.
      ENDIF.
    CATCH cx_sy_create_data_error ##no_handler.
  ENDTRY.

ENDMETHOD.


METHOD build_dyn_range.
  DATA:ls_value         TYPE rsdsselopt.
  DATA lt_ranges        TYPE REF TO data.
  DATA:lo_elem          TYPE REF TO cl_abap_typedescr.
  DATA:lv_length        TYPE int2,
       lv_table_name    TYPE char30,
       lv_offset        TYPE i.
       "lv_output_length TYPE i.
  DATA:ls_dd04v        TYPE  dd04v.
  DATA:lt_fldcat       TYPE lvc_t_fcat,
       ls_fldcat       TYPE lvc_s_fcat,
       lv_r_oref       TYPE REF TO cx_root.
  DATA ls_range_tab    TYPE REF TO data.
  FIELD-SYMBOLS:<lfs_field>         TYPE any,
                <lfs_range_tab>     TYPE STANDARD TABLE,
                <lfs_any_structure> TYPE any,
                <lfs_value>         TYPE any.

*Read the data type for the select options.
  ASSIGN ls_value TO <lfs_field>.
  lo_elem = cl_abap_typedescr=>describe_by_data( ls_value-low ).
  lv_length = strlen( lo_elem->absolute_name ).
  FIND FIRST OCCURRENCE OF '=' IN lo_elem->absolute_name
              MATCH OFFSET lv_offset.
  lv_offset = lv_offset + 1.
  lv_table_name = lo_elem->absolute_name+lv_offset(lv_length).

  CALL FUNCTION 'DDIF_DTEL_GET'
    EXPORTING
      name          = lv_table_name
    IMPORTING
      dd04v_wa      = ls_dd04v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc = 0.
*Create range dynamically
    ls_fldcat-fieldname = 'SIGN'.
    ls_fldcat-datatype  = 'CHAR'.
    ls_fldcat-intlen    = 1.
    APPEND ls_fldcat TO lt_fldcat.
    CLEAR ls_fldcat.

    ls_fldcat-fieldname = 'OPTION'.
    ls_fldcat-datatype  = 'CHAR'.
    ls_fldcat-intlen    = 2.
    APPEND ls_fldcat TO lt_fldcat.
    CLEAR ls_fldcat.

    ls_fldcat-fieldname = 'LOW'.
    ls_fldcat-datatype  = ls_dd04v-datatype.
    ls_fldcat-intlen    = ls_dd04v-leng.
    ls_fldcat-decimals  = ls_dd04v-decimals.
    APPEND ls_fldcat TO lt_fldcat.
    CLEAR ls_fldcat.

    ls_fldcat-fieldname = 'HIGH'.
    ls_fldcat-datatype  = ls_dd04v-datatype.
    ls_fldcat-intlen    = ls_dd04v-leng.
    ls_fldcat-decimals  = ls_dd04v-decimals.
    APPEND ls_fldcat TO lt_fldcat.
    CLEAR ls_fldcat.
  ENDIF.
*Create dynamic internal table and assign to fs
  TRY.
      CALL METHOD cl_alv_table_create=>create_dynamic_table
        EXPORTING
          it_fieldcatalog           = lt_fldcat
        IMPORTING
          ep_table                  = lt_ranges
        EXCEPTIONS
          generate_subpool_dir_full = 1
          OTHERS                    = 2.
      IF sy-subrc = 0.
*Populate values into range table
        ASSIGN lt_ranges->* TO  <lfs_range_tab>.
        IF <lfs_range_tab> IS ASSIGNED .
          CREATE DATA ls_range_tab LIKE LINE OF <lfs_range_tab>.
          ASSIGN ls_range_tab->* TO <lfs_any_structure>.
          IF <lfs_any_structure> IS ASSIGNED.
            LOOP AT et_value ASSIGNING <lfs_value>.
              IF <lfs_value> IS ASSIGNED.
                ASSIGN <lfs_value> TO <lfs_any_structure>.
                APPEND <lfs_any_structure> TO <lfs_range_tab>.
              ENDIF.
            ENDLOOP.
          ENDIF.
          it_ranges = <lfs_range_tab>.
        ENDIF.
      ENDIF.
    CATCH cx_root INTO lv_r_oref.
  ENDTRY.
ENDMETHOD.


METHOD build_select_query.
  DATA:lv_and_flag       TYPE flag,
       lv_package_size   TYPE i,
       ls_where_clause   TYPE edpline,
       lt_where_clause   TYPE STANDARD TABLE OF edpline INITIAL SIZE 0.
  DATA:ls_fields_where   TYPE gty_wh_fields,
       ls_field_where_db TYPE gty_field_pos.
  DATA:db_cursor TYPE cursor.
  FIELD-SYMBOLS:<lfs_tab> TYPE STANDARD TABLE.
  CONSTANTS:lc_and    TYPE char3  VALUE 'AND',
            lc_in     TYPE char2  VALUE 'IN',
            lc_range1 TYPE char10 VALUE 'IT_RANGE1',
            lc_range2 TYPE char10 VALUE 'IT_RANGE2',
            lc_range3 TYPE char10 VALUE 'IT_RANGE3',
            lc_range4 TYPE char10 VALUE 'IT_RANGE4',
            lc_range5 TYPE char10 VALUE 'IT_RANGE5',
            lc_range6 TYPE char10 VALUE 'IT_RANGE6',
            lc_range7 TYPE char10 VALUE 'IT_RANGE7'.
  DATA:lo_root    TYPE REF TO cx_root,
       lv_message TYPE string.

  ASSIGN ct_inttab->* TO <lfs_tab>.
  IF <lfs_tab> IS ASSIGNED.
    lv_package_size = 2147483648 / i_pack_size.
    IF  it_range1 IS INITIAL
    AND it_range2 IS INITIAL
    AND it_range3 IS INITIAL
    AND it_range4 IS INITIAL
    AND it_range5 IS INITIAL
    AND it_range6 IS INITIAL
    AND it_range7 IS INITIAL
    AND it_fields_where IS INITIAL
    AND it_field_where_db IS INITIAL.
***** To calculaten maximum no of records that can be accomodated in 2gb
      TRY.
          OPEN CURSOR WITH HOLD db_cursor FOR
          SELECT *
          FROM (i_dbtab)
          BYPASSING BUFFER.
          DO.
*** To Fetch data in chunks of 2gb
            FETCH NEXT CURSOR db_cursor
            INTO CORRESPONDING FIELDS OF TABLE <lfs_tab>
            PACKAGE SIZE lv_package_size.
            IF sy-subrc NE 0.
              CLOSE CURSOR db_cursor.
              EXIT.
            ENDIF.
*** Here do the operation you want on internal table <xtab_buf>
          ENDDO.
        CATCH cx_sy_dynamic_osql_syntax    INTO lo_root.
          IF lo_root IS NOT INITIAL.
            lv_message = lo_root->get_text( ).
            MESSAGE s000(zfi01) WITH lv_message DISPLAY LIKE 'E'.
            "Dynamic SQL exceptions
            LEAVE LIST-PROCESSING.
          ENDIF.
        CATCH cx_sy_dynamic_osql_semantics INTO lo_root.
          IF lo_root IS NOT INITIAL.
            lv_message = lo_root->get_text( ).
            MESSAGE s000(zfi01) WITH lv_message DISPLAY LIKE 'E'.
            "Dynamic SQL exceptions
            LEAVE LIST-PROCESSING.
          ENDIF.
      ENDTRY.
      IF <lfs_tab> IS NOT INITIAL.
        SORT <lfs_tab>.
      ELSE.
        MESSAGE s000(zfi01) WITH text-e03 DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      LOOP AT  it_field_where_db INTO ls_field_where_db.
        READ TABLE it_fields_where
              INTO ls_fields_where
          WITH KEY fieldname = ls_field_where_db-fieldname.
        IF sy-subrc IS INITIAL.
          CASE ls_fields_where-range_num.
            WHEN 1.
              IF it_range1 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range1
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range1
                         INTO ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 2.
              IF it_range2 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range2
                  INTO ls_where_clause SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range2
                         INTO ls_where_clause SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 3.
              IF it_range3 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range3
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range3
                         INTO ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 4.
              IF it_range4 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range4
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE  lc_and ls_field_where_db-fieldname lc_in
                               lc_range4
                         INTO  ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 5.
              IF it_range5 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range5
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE  lc_and ls_field_where_db-fieldname lc_in
                               lc_range5
                          INTO ls_where_clause SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 6.
              IF it_range6 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range6
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range6
                         INTO ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN 7.
              IF it_range7 IS NOT INITIAL.
                IF lv_and_flag IS INITIAL.
                  CONCATENATE ls_field_where_db-fieldname lc_in lc_range7
                         INTO ls_where_clause
                 SEPARATED BY space.
                ELSE.
                  CONCATENATE lc_and ls_field_where_db-fieldname lc_in
                              lc_range7
                         INTO ls_where_clause
                 SEPARATED BY space.
                ENDIF.
                APPEND ls_where_clause TO lt_where_clause.
              ENDIF.
              lv_and_flag = gc_x.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.
        CLEAR:ls_fields_where,ls_field_where_db,ls_where_clause.
      ENDLOOP.
      IF lt_where_clause IS NOT INITIAL.
        APPEND '.' TO lt_where_clause.
      ENDIF.
      IF lt_where_clause IS NOT INITIAL.
        TRY.
            OPEN CURSOR WITH HOLD db_cursor FOR
              SELECT *
                FROM (i_dbtab)
                BYPASSING BUFFER
                "INTO CORRESPONDING FIELDS of TABLE <lfs_tab>
                WHERE (lt_where_clause).
            DO.
*** To Fetch data in chunks of 2gb
              FETCH NEXT CURSOR db_cursor
              INTO CORRESPONDING FIELDS OF TABLE <lfs_tab>
              PACKAGE SIZE lv_package_size.
              IF sy-subrc NE 0.
                CLOSE CURSOR db_cursor.
                EXIT.
              ENDIF.
*** Here do the operation you want on internal table <xtab_buf>
            ENDDO.
          CATCH cx_sy_dynamic_osql_syntax    INTO lo_root.
            IF lo_root IS NOT INITIAL.
              lv_message = lo_root->get_text( ).
              MESSAGE s000(zfi01) WITH lv_message DISPLAY LIKE 'E'.
              "Dynamic SQL exceptions
              LEAVE LIST-PROCESSING.
            ENDIF.
          CATCH cx_sy_dynamic_osql_semantics INTO lo_root.
            IF lo_root IS NOT INITIAL.
              lv_message = lo_root->get_text( ).
              MESSAGE s000(zfi01) WITH lv_message DISPLAY LIKE 'E'.
              "Dynamic SQL exceptions
              LEAVE LIST-PROCESSING.
            ENDIF.
        ENDTRY.
        IF <lfs_tab> IS NOT INITIAL.
          SORT <lfs_tab>.
        ELSE.
          MESSAGE s000(zfi01) WITH text-e03 DISPLAY LIKE 'E'.
        ENDIF."endif sy-surc chck on select
      ENDIF."endif lt_where_clause is initial
    ENDIF."endif range values check
  ENDIF.
ENDMETHOD.


METHOD check_primary_key.
  DATA:ls_fieldnames TYPE dfies,
       lt_fieldlist  TYPE STANDARD TABLE OF gty_wh_fields
                     INITIAL SIZE 0,
       ls_fieldlist  TYPE gty_wh_fields.
  DATA :lv_flag        TYPE sy-index,
        lv_msg         TYPE string,
        lv_primary_key TYPE fieldname.
  lt_fieldlist = it_fieldlist.
  SORT lt_fieldlist BY fieldname.
*loop through each of the key fields
  LOOP AT it_fieldnames INTO ls_fieldnames WHERE keyflag = gc_x.
    CASE sy-tabix.
      WHEN 1.
        lv_primary_key = ls_fieldnames-fieldtext.
        READ TABLE lt_fieldlist
              INTO ls_fieldlist
          WITH KEY fieldname = ls_fieldnames-fieldname
          BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_flag = 1.
          EXIT.
        ELSE.
          READ TABLE lt_fieldlist
                INTO ls_fieldlist
            WITH KEY fieldname = ls_fieldnames-domname
            BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            lv_flag = 1.
            EXIT.
          ENDIF.
        ENDIF.
      WHEN 2.
        READ TABLE lt_fieldlist
              INTO ls_fieldlist
          WITH KEY fieldname = ls_fieldnames-fieldname
          BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_flag = 2.
          EXIT.
        ELSE.
          READ TABLE lt_fieldlist
                INTO ls_fieldlist
            WITH KEY fieldname = ls_fieldnames-domname
            BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            lv_flag = 2.
            EXIT.
          ENDIF.
        ENDIF.
      WHEN 3.
        READ TABLE lt_fieldlist
              INTO ls_fieldlist
          WITH KEY fieldname = ls_fieldnames-fieldname
          BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_flag = 2.
          EXIT.
        ELSE.
          READ TABLE lt_fieldlist
                INTO ls_fieldlist
            WITH KEY fieldname = ls_fieldnames-domname
            BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            lv_flag = 2.
            EXIT.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        READ TABLE lt_fieldlist
              INTO ls_fieldlist
          WITH KEY fieldname = ls_fieldnames-fieldname
          BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_flag = 2.
          EXIT.
        ELSE.
          READ TABLE lt_fieldlist
                INTO ls_fieldlist
            WITH KEY fieldname = ls_fieldnames-domname
            BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            lv_flag = 2.
            EXIT.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.
  IF lv_flag = 0." OR lv_flag = 2.
    CONCATENATE text-w01 lv_primary_key text-w02
           INTO lv_msg
    SEPARATED BY space.
    CONDENSE lv_msg.
    MESSAGE i000(zfi01) WITH lv_msg .
  ENDIF.
ENDMETHOD.


METHOD create_fieldcat.
*data declaration for dynamic internal table and alv
  DATA:lo_structure            TYPE REF TO data,
       lo_struc_desc           TYPE REF TO cl_abap_structdescr.
  DATA:ls_fieldcatalogue      TYPE lvc_s_fcat,
       lt_fieldcatalogue      TYPE lvc_t_fcat.
*field symbols declaration
  FIELD-SYMBOLS : <lfs_dyn_str>      TYPE any,
                  <lfs_str_comp>     TYPE abap_compdescr.
* Dynamic creation of a structure
  CREATE DATA lo_structure TYPE (i_dbtab).
  ASSIGN lo_structure->* TO <lfs_dyn_str>.
  IF <lfs_dyn_str> IS ASSIGNED.
* Fields Structure
  lo_struc_desc ?= cl_abap_typedescr=>describe_by_data( <lfs_dyn_str> ).
    LOOP AT lo_struc_desc->components ASSIGNING <lfs_str_comp>.
*   Build Fieldcatalog
      ls_fieldcatalogue-fieldname   = <lfs_str_comp>-name.
      ls_fieldcatalogue-ref_table   = i_dbtab.
      APPEND ls_fieldcatalogue TO lt_fieldcatalogue.
    ENDLOOP.
  ENDIF.
  IF lt_fieldcatalogue IS NOT INITIAL.
    et_fieldcat = lt_fieldcatalogue.
  ENDIF.
ENDMETHOD.


METHOD get_incl_components.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0174059
  DATA:   lo_incl_stru       TYPE REF TO cl_abap_structdescr,
          lt_incl_components TYPE abap_component_tab,
          lv_curr_index      TYPE i.

  FIELD-SYMBOLS:  <lfs_comp>      LIKE LINE OF ct_components,
                  <lfs_incl_comp> LIKE LINE OF lt_incl_components.

  ct_components = im_structdescr->get_components( ).
*Get all fields in include structure present in table
  LOOP AT ct_components ASSIGNING <lfs_comp>.
    IF <lfs_comp>-as_include = 'X'.
      lo_incl_stru ?= <lfs_comp>-type.  " not the include struc type
      lv_curr_index = sy-tabix.    " and the index it is to be included
      DELETE ct_components INDEX lv_curr_index.
*Recursive call
      CALL METHOD me->get_incl_components
        EXPORTING
          im_structdescr = lo_incl_stru
        CHANGING
          ct_components  = lt_incl_components.
      LOOP AT lt_incl_components ASSIGNING <lfs_incl_comp>.
        INSERT <lfs_incl_comp> INTO ct_components INDEX lv_curr_index.
        lv_curr_index = lv_curr_index + 1.
      ENDLOOP.
    ENDIF.
    CLEAR lv_curr_index.
  ENDLOOP.
*END OF CHANGES BY KBANERJEE FOR CHG0174059
ENDMETHOD.


METHOD input_validations.
  DATA:lt_ddfields  TYPE tt_ddfields.
  DATA:lv_sequence  TYPE char90.
  DATA:ls_fieldname TYPE fieldnames.
  DATA:lt_field_pos TYPE STANDARD TABLE OF gty_field_pos INITIAL SIZE 0.
*Get all fieldnames in the database table
  CALL METHOD me->validate_fieldname
    EXPORTING
      i_tab         = i_tabname
    IMPORTING
      et_ddfields   = lt_ddfields
      e_struct_size = e_pack_size.

*Validate teh fields enetred in where if any
  IF it_fields IS NOT INITIAL.
    LOOP AT it_fields INTO ls_fieldname.
      CALL METHOD me->validate_wherefield
        EXPORTING
          it_ddfields = lt_ddfields
          i_field     = ls_fieldname-fieldname
        IMPORTING
          et_fieldpos = lt_field_pos.
      CONCATENATE ls_fieldname-fieldname lv_sequence
             INTO lv_sequence
      SEPARATED BY '#'.
    ENDLOOP.
  ENDIF.
  IF lt_field_pos IS NOT INITIAL.
    SORT lt_field_pos BY position.
    et_fieldlist = lt_field_pos.
  ENDIF.
  IF lv_sequence IS NOT INITIAL.
    e_field_sequence = lv_sequence.
  ENDIF.
  IF lt_ddfields IS NOT INITIAL.
    et_ddfields = lt_ddfields.
  ENDIF.
ENDMETHOD.


METHOD use_conversion_exit.
  DATA: ls_dd01l TYPE dd01l,
        ls_dd03l TYPE dd03l,
        ls_dd04l TYPE dd04l.

  DATA: lv_exit_name TYPE string,
        lo_data      TYPE REF TO data,
        lo_root      TYPE REF TO cx_root.

  FIELD-SYMBOLS: <lfs_value>  TYPE any,
                 <lfs_values> TYPE rsdsselopt.
  DATA:lv_value_conv TYPE string.
  CLEAR: ls_dd01l, ls_dd03l, ls_dd04l, lv_value_conv,lv_exit_name, lo_data.

**********************************************************************
* 1. get data-element of the field
**********************************************************************
  SELECT * FROM dd03l
    UP TO 1 ROWS
    INTO ls_dd03l
    WHERE tabname   = i_table
     AND fieldname = i_field.
  ENDSELECT.
  IF sy-subrc = 0.

**********************************************************************
* 2. get the rollname of the data-element
**********************************************************************
    SELECT * FROM dd04l
      INTO ls_dd04l
      UP TO 1 ROWS
      WHERE rollname = ls_dd03l-rollname.
    ENDSELECT.
    IF sy-subrc = 0.

**********************************************************************
* 3. get the convert routine
**********************************************************************
      SELECT * FROM dd01l
        INTO ls_dd01l
        UP TO 1 ROWS
        WHERE domname = ls_dd04l-domname.
      ENDSELECT.
      IF sy-subrc = 0 AND ls_dd01l-convexit IS NOT INITIAL.

**********************************************************************
* 4. apply convert routine
**********************************************************************

        " create corresponding field
        TRY.
            CREATE DATA lo_data TYPE (ls_dd03l-rollname).
          CATCH cx_sy_create_data_error INTO lo_root.
            RETURN.
        ENDTRY.
        " dereferencing
        ASSIGN lo_data->* TO <lfs_value>.
        IF sy-subrc = 0 AND <lfs_value> IS ASSIGNED.
          " apply convert routine
          CONCATENATE 'CONVERSION_EXIT_' ls_dd01l-convexit '_INPUT'
                 INTO lv_exit_name.
          IF sy-subrc = 0.
            LOOP AT ct_values ASSIGNING <lfs_values>.
              IF <lfs_values> IS ASSIGNED.
                CALL FUNCTION lv_exit_name
                  EXPORTING
                    input  = <lfs_values>-low
                  IMPORTING
                    output = <lfs_value>.
                " return value
                lv_value_conv = <lfs_value>.
                <lfs_values>-low = lv_value_conv.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD validate_data.
  FIELD-SYMBOLS:<lfs_table> TYPE STANDARD TABLE.
  DATA:lt_fld_table    TYPE REF TO data.
  DATA:ls_where_clause TYPE edpline,
       lt_where_clause TYPE STANDARD TABLE OF edpline INITIAL SIZE 0.
  CONSTANTS:lc_in      TYPE char2     VALUE 'IN',
            lc_range   TYPE fieldname VALUE 'IT_RANGE_VALID'.
  DATA:lo_root         TYPE REF TO cx_root,
       lv_message      TYPE string.
  DATA:lt_ddfields     TYPE STANDARD TABLE OF dfies INITIAL SIZE 0,
       ls_ddfields     TYPE dfies.
  DATA:lv_pack_size    TYPE i,
       lv_fieldname    TYPE fieldname.
  IF it_range_valid IS NOT INITIAL.
    IF i_check_tab IS NOT INITIAL.
*Get all fieldnames in the database table
      CALL METHOD me->validate_fieldname
        EXPORTING
          i_tab         = i_check_tab
        IMPORTING
          et_ddfields   = lt_ddfields
          e_struct_size = lv_pack_size.
      READ TABLE lt_ddfields WITH KEY fieldname = i_field_name
                             TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE lt_ddfields INTO ls_ddfields
                               WITH KEY domname = i_field_name.
        IF sy-subrc IS INITIAL.
          lv_fieldname = ls_ddfields-fieldname.
          CALL METHOD me->build_dynamic_table
            EXPORTING
              i_fieldname = ls_ddfields-fieldname
              i_checktab  = i_check_tab
            RECEIVING
              et_inttab   = lt_fld_table.
        ENDIF.
      ELSE.
        lv_fieldname =  i_field_name.
        CALL METHOD me->build_dynamic_table
          EXPORTING
            i_fieldname = i_field_name
            i_checktab  = i_check_tab
          RECEIVING
            et_inttab   = lt_fld_table.
      ENDIF."endif lt_ddfields read
    ELSEIF i_tabname IS NOT INITIAL.
*Get all fieldnames in the database table
      CALL METHOD me->validate_fieldname
        EXPORTING
          i_tab         = i_tabname
        IMPORTING
          et_ddfields   = lt_ddfields
          e_struct_size = lv_pack_size.
      READ TABLE lt_ddfields WITH KEY fieldname = i_field_name
                             TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        READ TABLE lt_ddfields INTO ls_ddfields
                               WITH KEY domname = i_field_name.
        IF sy-subrc IS INITIAL.
          lv_fieldname = ls_ddfields-fieldname.
          CALL METHOD me->build_dynamic_table
            EXPORTING
              i_fieldname = ls_ddfields-fieldname
              i_tabname   = i_tabname
            RECEIVING
              et_inttab   = lt_fld_table.
        ENDIF.
      ELSE.
        lv_fieldname = i_field_name.
        CALL METHOD me->build_dynamic_table
          EXPORTING
            i_fieldname = i_field_name
            i_tabname   = i_tabname
          RECEIVING
            et_inttab   = lt_fld_table.
      ENDIF."endif lt_ddfields read
    ENDIF."endif i_check_tab IS NOT INITIAL.

    ASSIGN lt_fld_table->* TO <lfs_table>.
    IF <lfs_table> IS ASSIGNED.
      CONCATENATE lv_fieldname lc_in lc_range
             INTO ls_where_clause
      SEPARATED BY space.
      APPEND ls_where_clause TO lt_where_clause.
      IF lt_where_clause IS NOT INITIAL.
        TRY.
            SELECT (lv_fieldname)
            INTO TABLE <lfs_table>
              FROM (i_check_tab)
              UP TO 1 ROWS
              WHERE (lt_where_clause).
          CATCH cx_sy_dynamic_osql_syntax    INTO lo_root.
          CATCH cx_sy_dynamic_osql_semantics INTO lo_root.
        ENDTRY.
        IF lo_root IS NOT INITIAL.
          lv_message = lo_root->get_text( ).
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE e000(zfi01) WITH text-t01.
  ENDIF.
ENDMETHOD.


METHOD validate_fieldname.
  DATA:lv_iname TYPE tabname.
  DATA:ls_ddfields TYPE dfies.
  lv_iname = i_tab.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = lv_iname
    TABLES
      dfies_tab      = et_ddfields
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc IS INITIAL.
    IF NOT et_ddfields[] IS INITIAL.
      LOOP AT et_ddfields INTO ls_ddfields.
        e_struct_size = e_struct_size + ls_ddfields-leng.
      ENDLOOP.
    ENDIF.
    DELETE et_ddfields WHERE domname = gc_mandt.
  ENDIF.
ENDMETHOD.


METHOD validate_sequence.
  DATA:lv_custom_sequence  TYPE char90,
       lv_db_sequence      TYPE char90.
  DATA:ls_field_pos TYPE gty_field_pos.
  lv_custom_sequence = c_sequence.

  LOOP AT it_field_pos INTO ls_field_pos.
    CONCATENATE ls_field_pos-fieldname lv_db_sequence "p_field4 p_field5
           INTO lv_db_sequence
    SEPARATED BY '#'.
  ENDLOOP.
  IF lv_custom_sequence NE lv_db_sequence.
    CLEAR c_sequence.
    c_sequence = lv_db_sequence.
  ENDIF.
ENDMETHOD.


METHOD validate_table.
  DATA:lv_tabname TYPE tabname.
  SELECT SINGLE tabname
  INTO lv_tabname
  FROM dd02l
  WHERE tabname  = i_table
    AND as4local = gc_a
    AND as4vers  = gc_ver.
  IF sy-subrc <> 0 OR lv_tabname IS INITIAL.
    MESSAGE e000(zfi01) WITH text-e01.
  ENDIF.
ENDMETHOD.


METHOD validate_wherefield.
  DATA:ls_field_pos TYPE gty_field_pos,
       ls_ddfields  TYPE dfies.
  IF NOT i_field IS INITIAL.
    READ TABLE it_ddfields INTO ls_ddfields
                           WITH KEY fieldname = i_field.
    IF sy-subrc <> 0.
      READ TABLE it_ddfields INTO ls_ddfields
                             WITH KEY domname = i_field.
      IF sy-subrc <> 0.
        MESSAGE e000(zfi01) WITH text-e02."Please enter valid field name
      ELSE.
        ls_field_pos-position  = ls_ddfields-position.
        ls_field_pos-fieldname = ls_ddfields-fieldname.
        APPEND ls_field_pos TO et_fieldpos.
      ENDIF.
    ELSE.
      ls_field_pos-position  = ls_ddfields-position.
      ls_field_pos-fieldname = ls_ddfields-fieldname.
      APPEND ls_field_pos TO et_fieldpos.
    ENDIF.
  ENDIF.
ENDMETHOD.
ENDCLASS.
