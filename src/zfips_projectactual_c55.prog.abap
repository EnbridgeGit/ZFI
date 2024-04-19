*&---------------------------------------------------------------------*
*& Report  ZFIPS_PROJECTACTUAL_C55
*&
*&---------------------------------------------------------------------*
*& Program Name       :  ZFIPS_PROJECTACTUAL_C55                       *
*& Author             :  Rajeshwar Reddy                               *
*& Creation Date      :  12th-Nov-2019                                 *
*& Object ID          :  ENHC0027017                                   *
*& Application Area   :  FICO                                          *
*& Description        :  New Interface from SAP to C55                 *
*                        Copied Logic from ZBPCI003_PROJECTACTUAL prog *
*                        and from the out put data of                  *
*                        ZBPCI003_PROJECTACTUAL prepared final data    *
*&-------------------------------------------------------------------- *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 12-11-2019   JOOKONTR       D30K930280  Initial                      *
************************************************************************

REPORT  ZFIPS_PROJECTACTUAL_C55 MESSAGE-ID zs.

******data declaration ******
INCLUDE ZFIPS_PROJECTACTUAL_C55_top.

******selection screen
INCLUDE ZFIPS_PROJECTACTUAL_C55_sel.
*
*********program logic****
INCLUDE ZFIPS_PROJECTACTUAL_C55_lgc.
*
******** form routines******
INCLUDE ZFIPS_PROJECTACTUAL_C55_f01.
