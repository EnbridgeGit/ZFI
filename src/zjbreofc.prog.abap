
REPORT ZJBREOFC MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-SIZE 255.
************************************************************************
* SAP note 872175
*********************

Data: gt_tjbs type tjbs occurs 0,
      ls_tjbs type tjbs,
      gt_tjbf type tjbf occurs 0,
      ls_tjbf type tjbf.

select * from tjbs into table gt_tjbs
    where usgfl = 'S' or usgfl = 'F' or usgfl = 'T' or usgfl = 'V' or
          usgfl = 'U'.

select * from tjbf into table gt_tjbf
    where usgfl = 'S' or usgfl = 'F' or usgfl = 'T' or usgfl = 'V' or
          usgfl = 'U' or ( usgfl = 'B' and rffie ne 'REC_WAERS' ).

loop at gt_tjbs into ls_tjbs where usgfl ne 'U'.
    read table gt_tjbf  with KEY  fienm = ls_tjbs-fienm into ls_tjbf.
    if sy-subrc NE 0.
       move-corresponding ls_tjbs to ls_tjbf.
       insert tjbf from ls_tjbf.
       if sy-subrc = 0.
          write: / ls_tjbf-fienm, 'inserted into TJBF'.
       endif.
    endif.
endloop.

loop at gt_tjbf into ls_tjbf where usgfl = 'B'.
   read table gt_tjbf with key fienm = ls_tjbf-rffie
      transporting no fields.
   if sy-subrc ne 0.
      read  table gt_tjbs with KEY fienm = ls_tjbs-rffie into ls_tjbs.
      if sy-subrc = 0.
         move-corresponding ls_tjbs to ls_tjbf.
         insert tjbf from ls_tjbf.
         if sy-subrc = 0.
            write: / ls_tjbf-fienm, 'inserted in TJBF'.
         endif.
       endif.
   endif.
endloop.

   write: / 'Reorg of TJBF fixed fields done'.
