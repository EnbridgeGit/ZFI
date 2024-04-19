*----------------------------------------------------------------------*
*   INCLUDE RPRCCCD0                                                   *
*----------------------------------------------------------------------*

** tables

TABLES: tcurx.

* Typdeclarations:


* global internal tables:

DATA: texttab LIKE bapitrvcdf OCCURS 100 WITH HEADER LINE,
      dataset LIKE bapitrvcdf OCCURS 100 WITH HEADER LINE,
      errors  LIKE bapitrvcce OCCURS 100 WITH HEADER LINE,
      buffer  LIKE bapitrvctc OCCURS 100 WITH HEADER LINE,
      trips   LIKE bapitrvctt OCCURS 100 WITH HEADER LINE.


* global variables:

DATA: subrc      LIKE sy-subrc,
      proceed    LIKE bapitrvxxx-ccc_proc,
      headdata   LIKE ccheader,
*     msg(100).                                 "QJEK011472 "XOWK000892
      msg TYPE string.                                      "XOWK000892

* Begin of MAWH1510789
CONSTANTS cv_logical_filename TYPE filename-fileintern
                                VALUE 'FI_TV_CCC_AMEX_FILE'.
* End of MAWH1510789
