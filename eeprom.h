//********************************************************************
//
//      EEPROM Initialization Part
//      4DClass
//      Copyright(C)  2017
//      All rights reserved
//***********************************************************************
#ifndef __EEPROM_H
#define __EEPROM_H
#include "includes.h"

void EEPROM_Init(void);
uint8_t Read_EEPROM_u8(uint8_t EEAddress);   //eeprom ����u8��ȡ
uint16_t Read_EEPROM_u16(uint8_t EEAddress); //eeprom ����u16��ȡ
void Write_EEPROM_u8(uint8_t EEAddress,uint8_t EEDatas);   //eeprom ����u8д��
void Write_EEPROM_u16(uint8_t EEAddress,uint16_t EEDatas); //eeprom ����u16д��




#endif










