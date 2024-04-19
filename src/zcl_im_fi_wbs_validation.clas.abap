class ZCL_IM_FI_WBS_VALIDATION definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_FI_WBS_VALIDATION
*"* do not include other source files here!!!

  interfaces IF_EX_WORKBREAKDOWN_UPDATE .
protected section.
*"* protected components of class ZCL_IM_FI_WBS_VALIDATION
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_FI_WBS_VALIDATION
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_FI_WBS_VALIDATION IMPLEMENTATION.


method IF_EX_WORKBREAKDOWN_UPDATE~AT_SAVE.

  DATA:LS_WBS_ELEMENT TYPE cjbai_wbselement,
       gv_pspnr       TYPE ps_posnr,
       gv_usr08       type USRDATE,
       gv_psphi       type PS_PSPHI,
       GV_STAT        type J_STATUS, " ADDED BY AKMADASU CHG0138620
       GV_AGR_NAME    TYPE AGR_NAME, " ADDED BY AKMADASU CHG0138620
       gv_objnr       TYPE objnr,
       jest_tab       TYPE TABLE OF jest_upd,
       jest_st TYPE jest_upd.
  if sy-tcode eq 'CJ02' OR SY-TCODE EQ 'CJ20N' OR SY-TCODE EQ 'CJ20'.
    CALL FUNCTION 'STATUS_CHANGES_GET'
      TABLES
        t_changes = jest_tab.
    READ TABLE it_wbs_element INTO ls_wbs_element INDEX 1.
    IF sy-subrc IS INITIAL.
      gv_pspnr = ls_wbs_element-pspnr.
      gv_PSPHI = ls_wbs_element-psphi.
      SELECT SINGLE objnr USR08 FROM prps INTO (gv_objnr,gv_usr08) WHERE PSPHI = gv_PSPHI
                                                                   AND   STUFE = '1'."pspnr EQ gv_pspnr.
      IF SY-SUBRC IS INITIAL.
        READ TABLE jest_tab INTO jest_st WITH KEY objnr = gv_objnr
                                              stat  = 'I0045'
                                              INACT = space.
        IF sy-subrc IS INITIAL and gv_usr08 is INITIAL.
          IF LS_WBS_ELEMENT-USR08 is INITIAL.
            MESSAGE 'The In-Service date info is required, when "TECO"' TYPE 'E' RAISING error_with_message.
          ELSE.
            "DO NOTHING
          ENDIF.
        ELSE.
          "DO NOTHING
        ENDIF.
**-- start of changes by akmadasu CHG0138620
*** CLSD TO TECO Validation
        SELECT SINGLE STAT FROM JEST INTO (GV_STAT) WHERE OBJNR = gv_objnr
                                                    AND   STAT  = 'I0046'
                                                    AND   INACT = space.
        IF sy-subrc is INITIAL.
          READ TABLE jest_tab INTO jest_st WITH KEY objnr = gv_objnr
                                                    stat  = 'I0045'
                                                    INACT = space.
          IF sy-subrc is INITIAL.
            SELECT SINGLE AGR_NAME FROM AGR_USERS
                                   INTO (GV_AGR_NAME)
                                   WHERE UNAME = SY-UNAME
                                   AND agr_name = 'Z:TEST_CJ02' "'FI-Plant Accounting'
                                   AND to_dat >= sy-datum
                                   AND from_dat <= sy-datum.
            IF SY-SUBRC IS NOT INITIAL.
              MESSAGE 'You do not have authorization change project/WBS from CLSD to TECO' TYPE 'E' RAISING error_with_message.
            ELSE.
              " SAVE THE WBS
            ENDIF.
          ENDIF.
        ENDIF.
**-- end of changes by akmadasu CHG0138620
      ENDIF.
    ENDIF.
  ENDIF.
endmethod.


method IF_EX_WORKBREAKDOWN_UPDATE~BEFORE_UPDATE.
endmethod.
ENDCLASS.
