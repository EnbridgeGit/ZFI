FUNCTION-POOL zfi_ap_vendorcontact.         "MESSAGE-ID ..

TYPES:
      BEGIN OF ty_contact,
        parnr       TYPE knvk-parnr,
        fname       TYPE knvk-namev,
        lname       TYPE knvk-name1,
        abtnr       TYPE knvk-abtnr,
        abtnrd      TYPE tsabt-vtext,
        pafkt       TYPE knvk-pafkt,
        pafktd      TYPE tpfkt-vtext,
        addr        TYPE ad_addrnum,
        pers        TYPE ad_persnum,
      END OF ty_contact.


DATA:
      gs_lfa1     TYPE lfa1,
      gv_aktyp    TYPE aktyp,

      gt_zvar     TYPE TABLE OF zvar,
      gt_contact  TYPE TABLE OF ty_contact,

      ls_contact LIKE LINE OF gt_contact.



CONTROLS:
      tbl_contact TYPE TABLEVIEW USING SCREEN 9001.
