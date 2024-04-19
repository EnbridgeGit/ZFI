* 2009/01/20 TR580 mdemeest All changes identified with UGL     UGL
*                           in rightmost column                 UGL
*
*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE ZLMEDRUCKTOP_UGL.                       " Global Data
  INCLUDE LZMEDRUCKUXX.                       " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE LMEDRUCKF...                       " Subprograms
* INCLUDE LMEDRUCKO...                       " PBO-Modules
* INCLUDE LMEDRUCKI...                       " PAI-Modules

INCLUDE LMEDRUCKF01.

INCLUDE ZLMEDRUCKF02_UGL.                                       "UGL

INCLUDE LMEDRUCKF1S.

INCLUDE LMEDRUCKE01.

*ENHANCEMENT-POINT SAPLMEDRUCK_01 SPOTS ES_SAPLMEDRUCK STATIC.  "UGL
*$*$-Start:SAPLMEDRUCK_01----------------------------------$*$*
*ENHANCEMENT 2  /NFM/MM_SAPLMEDRUCK.    "active version         "UGL
*include /nfm/medruck_f01.                    "/NFM/            *UGL
*ENDENHANCEMENT.
*$*$-End:
*$*$-End: SAPLMEDRUCK_01----------------------------$*$*
INCLUDE LMEDRUCKF1T.

INCLUDE LMEDRUCKF1U.

INCLUDE LZMEDRUCKF01.
