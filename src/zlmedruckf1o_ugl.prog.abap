* 2009/01/19 mdemeest TR580 Upgrade to ERP 6.0/SAPWeaver 7.00       UGL
*                           All changes identified by UGL in        UGL
*                           rightmost column                        UGL
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FORMULAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_formular USING p_screen     TYPE c
                            value(p_toa_dara) TYPE toa_dara
                            p_arc_params TYPE arc_params
                            p_fonam      LIKE tnapr-fonam
                   CHANGING p_retco      TYPE i.
  CLEAR p_retco.

  DATA: xdevice(10),                   "Hilfsfeld Formular
        xprogramm TYPE  tdprogram,     "Hilfsfeld Programm
        xdialog.                       "Hilfsfeld Formular

  SET LANGUAGE ekko-spras.
  SET COUNTRY  lfa1-land1.

*- Formular festlegen -------------------------------------------------*

  tnapr-fonam = p_fonam.

  IF tnapr-fonam EQ space.
    tnapr-fonam = 'MEDRUCK'.
  ENDIF.

  CLEAR: xdialog, xdevice, itcpo.
  MOVE-CORRESPONDING nast TO itcpo.
  itcpo-tdtitle = nast-tdcovtitle.
  itcpo-tdfaxuser = nast-usnam.
*- Ausgabemedium festlegen --------------------------------------------*
  CASE nast-nacha.
    WHEN '2'.
      xdevice = 'TELEFAX'.
      IF nast-telfx EQ space.
        xdialog = 'X'.
      ELSE.
        itcpo-tdtelenum  = nast-telfx.
        IF nast-tland IS INITIAL.
          itcpo-tdteleland = lfa1-land1.
        ELSE.
          itcpo-tdteleland = nast-tland.
        ENDIF.
      ENDIF.
    WHEN '3'.
      xdevice = 'TELETEX'.
      IF nast-teltx EQ space.
        xdialog = 'X'.
      ELSE.
        itcpo-tdtelenum  = nast-teltx.
        itcpo-tdteleland = lfa1-land1.
      ENDIF.
    WHEN '4'.
      xdevice = 'TELEX'.
      IF nast-telx1 EQ space.
        xdialog = 'X'.
      ELSE.
        itcpo-tdtelenum  = nast-telx1.
        itcpo-tdteleland = lfa1-land1.
      ENDIF.
    WHEN '5'.
      DATA:  lvs_comm_type     TYPE   ad_comm,
             lvs_comm_values   TYPE   szadr_comm_values.
*   ... use stratagy to get communication type
      CALL FUNCTION 'ADDR_GET_NEXT_COMM_TYPE'
        EXPORTING
          strategy           = nast-tcode
          address_number     = lfa1-adrnr
        IMPORTING
          comm_type          = lvs_comm_type
          comm_values        = lvs_comm_values
        EXCEPTIONS
          address_not_exist  = 1
          person_not_exist   = 2
          no_comm_type_found = 3
          internal_error     = 4
          parameter_error    = 5
          OTHERS             = 6.
      IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
* convert communication data
      MOVE-CORRESPONDING nast TO intnast.
      MOVE sy-repid           TO xprogramm.
      CALL FUNCTION 'CONVERT_COMM_TYPE_DATA'
        EXPORTING
          pi_comm_type              = lvs_comm_type
          pi_comm_values            = lvs_comm_values
          pi_country                = lfa1-land1
          pi_repid                  = xprogramm
          pi_snast                  = intnast
        IMPORTING
          pe_itcpo                  = itcpo
          pe_device                 = xdevice
          pe_mail_recipient         = lvs_recipient
          pe_mail_sender            = lvs_sender
        EXCEPTIONS
          comm_type_not_supported   = 1
          recipient_creation_failed = 2
          sender_creation_failed    = 3
          OTHERS                    = 4.
      IF sy-subrc <> 0.
*       Avoids cancellation with message TD421
        p_retco = '1'.                                           "831984
        PERFORM protocol_update USING '740' space space space space.
*       dummy message to make the message appear in the where-used list
        IF 1 = 2.
          MESSAGE S740(me).
        ENDIF.
        EXIT.
      ENDIF.

      IF xdevice = 'MAIL'.                                       "885787
*     Check validity of email address to avoid cancellation with TD463
        CALL FUNCTION 'SX_ADDRESS_TO_DEVTYPE'                    "831984
          EXPORTING
            recipient_id            = lvs_recipient
            sender_id               = lvs_sender
          EXCEPTIONS
            err_invalid_route       = 1
            err_system              = 2
            OTHERS                  = 3.
        IF SY-SUBRC <> 0.
          p_retco = '1'.
          PERFORM protocol_update USING '740' space space space space.
*        dummy message to make the message appear in the where-used list
          IF 1 = 2.
            MESSAGE S740(me).
          ENDIF.
          EXIT.
        ENDIF.
      ENDIF.

    WHEN OTHERS.
      xdevice = 'PRINTER'.
      IF nast-ldest EQ space.
        xdialog = 'X'.
      ELSE.
        itcpo-tddest   = nast-ldest.
      ENDIF.
  ENDCASE.

*- Testausgabe auf Bildschirm -----------------------------------------*
  IF p_screen NE space.
    itcpo-tdpreview = 'X'.
  ENDIF.

* Bei Probedruck, wenn das Medium keine Drucker ist.
  IF nast-sndex EQ 'X' AND nast-nacha NE '1'.
    xdevice = 'PRINTER'.
    IF nast-ldest EQ space.
      xdialog = 'X'.
    ELSE.
      itcpo-tddest   = nast-ldest.
    ENDIF.
  ENDIF.

  itcpo-tdnoprint  = 'X'.
  itcpo-tdcover    = nast-tdocover.
  itcpo-tdcopies   = nast-anzal.
  IF sy-ucomm EQ 'DRPR' OR
     nast-sndex EQ 'X'.
    itcpo-tdnoprint  = ' '.
    itcpo-tdnoprev   = 'X'.
    itcpo-tdcopies = 1.
  ENDIF.
  itcpo-tddataset  = nast-dsnam.
  itcpo-tdsuffix1  = nast-dsuf1.
* -------------------- start of ERP 6.0 Change ---------------  UGL
*  itcpo-tdsuffix2  = nast-dsuf2.                               UGL
*  Place PO number into the spool request name so that if there UGL
*  is a PO number referenced in a daily error report, it checks UGL
*  the spool table for errors when the PO is faxed              UGL
*                                                               UGL
  concatenate '*' ekko-ebeln '*' into itcpo-tdsuffix2.         "UGL
  select single * from lfa1 where lifnr = ekko-lifnr.          "UGL
*------------------------- end of ERP 6.0 Change -------------  UGL
  itcpo-tdimmed    = nast-dimme.
  itcpo-tddelete   = nast-delet.
  itcpo-tdsenddate = nast-vsdat.
  itcpo-tdsendtime = nast-vsura.
  itcpo-tdprogram  = sy-repid.
  itcpo-bcs_reqst  = nast-forfb.                       "1078312
  itcpo-bcs_status = nast-prifb.                       "1078312
*ENHANCEMENT-POINT lmedruckf10_02 SPOTS es_saplmedruck.
  itcpo-tdnewid    = 'X'.

* Formular festlegen -------------------------------------------------*
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      form           = tnapr-fonam
      language       = ekko-spras
      OPTIONS        = itcpo
      archive_index  = p_toa_dara
      archive_params = p_arc_params
      device         = xdevice
      dialog         = xdialog
      mail_sender    = lvs_sender
      mail_recipient = lvs_recipient
    EXCEPTIONS
      canceled       = 01
      device         = 02
      OTHERS         = 03.
  IF sy-subrc NE 0.
    p_retco = '1'.
    PERFORM protocol_update USING '142' ekko-ebeln space space space.
    EXIT.
  ENDIF.

ENDFORM.                              "#EC CI_VALPAR " PREPARE_FORMULAR
