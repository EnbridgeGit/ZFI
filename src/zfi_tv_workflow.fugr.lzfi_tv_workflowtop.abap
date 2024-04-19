FUNCTION-POOL zfi_tv_workflow.              "MESSAGE-ID ..

DATA: gv_sourcetext TYPE text80.
DATA: gv_targettext TYPE text80.
DATA: gv_sourcetext1 TYPE text80.
DATA: gv_targettext1 TYPE text80.
DATA: gv_hide       TYPE c.
DATA: gv_twoline    TYPE c.
DATA: gv_must       TYPE c.
DATA: gv_titel      TYPE text60.
DATA: gv_role      TYPE agr_name.
DATA: ok_code       TYPE syucomm.
DATA  save_ok_code LIKE sy-tcode.
DATA  antwort TYPE xfeld.

DATA: BEGIN OF popup,
      defaultoption,
      title(35),
      textline1(55),
      textline2(55),
      textline3(55),
      textline4(55),
      textline5(55),
      option1(20),
      option2(20),
      option3(20),
      END OF popup.
DATA  option.
DATA: w_bname LIKE usr02-bname.
TYPES: BEGIN OF t_user,
  bname TYPE xubname,
  name TYPE ad_namtext,
  END OF t_user.

DATA: it_user TYPE STANDARD TABLE OF t_user,
      wa_user TYPE t_user,
      it_exclude LIKE zfit_tne_admin OCCURS 0,
      wa_admin LIKE zfit_tne_admin,
      imp_role LIKE agr_logsys.

RANGES: r_role FOR agr_define-agr_name.

* INCLUDE LZFI_TV_WORKFLOWD...               " Local class definition
