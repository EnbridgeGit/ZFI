"Name: \PR:VMD_EI_API_MAP_STRUCTURE======CP\EX:EHP603_EXTERN_TO_ECC_UPDATE_02\EI
ENHANCEMENT 0 ZFAPE121_VEND_COPY_DATA.
  CALL METHOD vmd_ei_api_map_structure=>extern_format_central_data_u
    EXPORTING
      is_central_data_extern = ls_master_data_extern-central_data
      is_global_mem_central  = ls_global_mem_central
      is_header_extern       = ls_master_data_extern-header
      is_address             = es_address_new
      it_address_contact     = et_address_contact
    IMPORTING
      es_central_data_ecc    = ls_central_data_ecc
      es_error               = ls_error.
  IF ls_error-is_error IS INITIAL.
    ls_central_data_ecc-lfa1-zz_iap_vendor_id = ls_master_data_extern-zz_iap_vendor_id.
    ls_central_data_ecc-lfa1-zz_iap_change_dt = ls_master_data_extern-zz_iap_create_dt.
    ls_central_data_ecc-lfa1-zz_iap_ritm_id = ls_master_data_extern-zz_iap_ritm_id.
  ENDIF.
ENDENHANCEMENT.
