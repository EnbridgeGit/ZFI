"Name: \PR:SAPLKAEP\FO:LIST\SE:BEGIN\EI
ENHANCEMENT 0 ZPERNR.

*Field-symbols: <fs_pernr> type kaep_covp_ext .
*
*  DATA: ls_agr_users TYPE agr_users .
*
*  loop at <gt_pos_data> assigning <fs_pernr> .
*
*    SELECT * UP TO 1 ROWS
*      INTO ls_agr_users
*      FROM agr_users
*     WHERE agr_name = 'Z:CO_PAYROLL_DOCS_DISPLAY'
*       AND uname    = syst-uname
*       AND from_dat LE syst-datum
*       AND to_dat   GE syst-datum .
*
*    ENDSELECT .
*
*  IF syst-subrc NE 0 .
*
*    clear: <fs_pernr>-pernr .
*
*    modify <gt_pos_data> from <fs_pernr> index syst-tabix .
*
*  endif .
*
*  endloop .

ENDENHANCEMENT.
