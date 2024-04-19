"Name: \PR:SAPLCJDW\FO:DISTRIBUTE_PROJECT_ALE\SE:BEGIN\EI
ENHANCEMENT 0 ZIDOCGENERATE_CRTWBS_STATUS.

  DATA: error_flag1,
        loc_serial_id1 LIKE serial-chnum,
        l_text05 LIKE tj02t-txt04,
        l_text31 LIKE tj02t-txt30.


  DATA: it_filterobjects1  LIKE bdi_fobj   OCCURS 0 WITH HEADER LINE,
        it_receiver_input1 LIKE bdi_logsys OCCURS 0 WITH HEADER LINE,
        it_receivers1      LIKE bdi_logsys OCCURS 0 WITH HEADER LINE.

* interne Tabellen und WA für externe Strukturen
  DATA: it_masterdata1   LIKE bapi2054_masterdata_ale
                               OCCURS 0 WITH HEADER LINE,
        it_hierarchie1   LIKE bapi_wbs_hierarchie
                               OCCURS 0 WITH HEADER LINE,
        it_statusheader1 LIKE bapi2054_statusheader_ale
                               OCCURS 0 WITH HEADER LINE,
        it_objectstatus1 LIKE bapi2054_objectstatus_ale
                               OCCURS 0 WITH HEADER LINE,
        wa_projdefinition1 LIKE bapi2054_projdefinition,

        it_extension1 LIKE bapiparex
                           OCCURS 0 WITH HEADER LINE. "D30 Spau Adjustments.

DATA:  error_flag_ext1. "D30 Spau Adjustments.

* logisches System ermitteln und in Stammdaten füllen
  PERFORM fill_logsystem.

* Empfängerliste ermitteln
  PERFORM get_filterobjects_ale TABLES it_filterobjects1.

* Wenn logsystem mitgegeben, nur dieses als Empfänger zulässig
  if not logsystem is initial.
    it_receiver_input1-logsys = logsystem.
    APPEND it_receiver_input1.
  endif.

  CALL FUNCTION 'ALE_ASYNC_BAPI_GET_RECEIVER'
    EXPORTING
      object                   = 'BUS2054'
      method                   = 'SAVEREPLICA'
    TABLES
      receiver_input           = it_receiver_input1
      receivers                = it_receivers1
      filterobject_values      = it_filterobjects1
    EXCEPTIONS
      error_in_filterobjects   = 1
      error_in_ale_customizing = 2.
  IF sy-subrc <> 0.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb      = sy-msgid
        msgty      = sy-msgty
        msgv1      = sy-msgv1
        msgv2      = sy-msgv1
        msgv3      = sy-msgv1
        msgv4      = sy-msgv1
        txtnr      = sy-msgno
      EXCEPTIONS
        not_active = 1.
  ENDIF.
  IF it_receivers1[] IS INITIAL.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb      = '2054ALE'
        msgty      = 'I'
        msgv1      = *proj-pspid
        txtnr      = '013'
      EXCEPTIONS
        not_active = 1.
    EXIT.
  ENDIF.

  READ TABLE it_receivers1 WITH KEY logsys = 'WARP'.
    IF sy-subrc NE 0.
* Verteilung nur für freigegebene PSP anstossen
  CALL FUNCTION 'STATUS_CHECK'
    EXPORTING
      objnr             = *proj-objnr
      status            = 'I0001'
    EXCEPTIONS
      object_not_found  = 1
      status_not_active = 2.
  IF NOT sy-subrc = 2.

    CALL FUNCTION 'STATUS_NUMBER_CONVERSION'
      EXPORTING
        language      = sy-langu
        status_number = 'I0001'
      IMPORTING
        txt04         = l_text05
        txt30         = l_text31.

    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb      = 'BS'
        msgty      = 'I'
        msgv1      = l_text05
        msgv2      = *proj-pspid
        msgv3      = l_text31
        msgv4      = space
        txtnr      = '013'
      EXCEPTIONS
        not_active = 1.

    EXIT.
  ENDIF.
ENDIF.

* Bapi Strukturen füllen
  PERFORM map_internal_2_bapi_structure TABLES it_masterdata1
                                               it_hierarchie1
                                               it_statusheader1
                                               it_objectstatus1
                                               it_extension1   "D30 SPAU Adjustmnts
                                      CHANGING wa_projdefinition1
                                               error_flag1
                                               error_flag_ext1. "D30 SPAU Adjustmnts

* Serialisierungs-Id füllen (letzte 4 Stellen der internen Nummer)
  loc_serial_id1 = *proj-pspnr+4(4).

  CHECK error_flag1 IS INITIAL.

* ALE-SaveReplica Baustein aufrufen
  CALL FUNCTION 'ALE_PROJECT_SAVEREPLICA'
    EXPORTING
      projectdefinition    = wa_projdefinition1
      serial_id            = loc_serial_id1
    TABLES
      wbselement           = it_masterdata1
      wbshierarchie        = it_hierarchie1
      statusheader         = it_statusheader1
      objectstatus         = it_objectstatus1
      receivers            = it_receivers1
    EXCEPTIONS
      error_creating_idocs = 1.
  IF sy-subrc <> 0.
    PERFORM put_sy_message IN PROGRAM saplco2o.
  ENDIF.
  EXIT.
ENDENHANCEMENT.
