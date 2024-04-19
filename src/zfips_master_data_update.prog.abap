*&---------------------------------------------------------------------*
*& Report  ZFIPS_MASTER_DATA_UPDATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFIPS_MASTER_DATA_UPDATE                      *
*& Author             :  KMB                                           *
*& Creation Date      :  19/11/2020                                    *
*& Object ID          :  CHG0203062                                    *
*& Application Area   :  FICO                                          *
*& Description        :  Optimization of PS master data update         *
*&-------------------------------------------------------------------- *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 19-03-2020   KMB         D30K930800  CHG020306  Initial              *
************************************************************************

REPORT  ZFIPS_MASTER_DATA_UPDATE.

******data declaration******
INCLUDE ZFIPS_MASTER_DATA_UPDATE_TOP.

******selection screen******
INCLUDE ZFIPS_MASTER_DATA_UPDATE_SCR.

********program logic****
INCLUDE ZFIPS_MASTER_DATA_UPDATE_LGC.

******* form routines******
INCLUDE ZFIPS_MASTER_DATA_UPDATE_F01.
