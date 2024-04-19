*****           Implementation of object type ZFI_FWORD            *****
INCLUDE <OBJECT>.
BEGIN_DATA OBJECT. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF KEY,
      WORKITEM LIKE ZFIS_FORWARD_WORKITEM-WORKITEM,
      USERNAME LIKE ZFIS_FORWARD_WORKITEM-USERNAME,
  END OF KEY.
END_DATA OBJECT. " Do not change.. DATA is generated
