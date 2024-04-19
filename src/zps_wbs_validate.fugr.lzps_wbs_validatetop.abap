FUNCTION-POOL ZPS_WBS_VALIDATE.             "MESSAGE-ID ..

* INCLUDE LZPS_WBS_VALIDATED...              " Local class definition

include lcjpnmod.

tables:
  prps,                                "wbs element
  tcj01,                               "edit indicator,arg. length,must
  tcjed.                               "edit masks

constants:
   zahlen(10)   value '1234567890',
   init_anz like prps-posid value '0                       ',
   zero         value '0',
   numerknz     value '0',
   con_yes      value 'X'.

data: flg_wildc.                                            "note 49239

data:
  sonderzeichen(8) type c,             "special character from tcj01
  hilfstring(24)   type c,             "edit string from tcjed
  flag_exit        type c,             "exit flag for matchcode
  tcj01_strkz      type i,
  aktspalte        type i,             "offset project number part
  projnr_edit_old  like prps-posid,
  projnr_unedt_old like prps-posid,
  subrc1           like sy-subrc.      "return code from subroutines

data: begin of edit_tab occurs 1.      "work table for
        include structure tcjed.       "TCJED
data:   flg_exist.
data: end of edit_tab.
