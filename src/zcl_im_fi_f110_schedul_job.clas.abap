class ZCL_IM_FI_F110_SCHEDUL_JOB definition
  public
  final
  create public .

*"* public components of class ZCL_IM_FI_F110_SCHEDUL_JOB
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_FI_F110_SCHEDULE_JOB .
*"* protected components of class ZCL_IM_FI_F110_SCHEDUL_JOB
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_FI_F110_SCHEDUL_JOB
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_FI_F110_SCHEDUL_JOB IMPLEMENTATION.


method IF_EX_FI_F110_SCHEDULE_JOB~CHECK_PARAMETER.
  break sahmad.
if I_F110V-XMITD = 'X' and
   I_F110V-XVORL = 'X'.
   clear E_PARAM_OK.
   MESSAGE e000(zfi01) WITH
   'De-select Create Payment Medium at proposal run'.
*   & & & & &

endif.
endmethod.
ENDCLASS.
