//********************************************************************
//
//      ADC Init Initialization Part
//      4DClass
//      Copyright(C)  2017
//      All rights reserved
//***********************************************************************
#include "includes.h"

/*******************EEPROM ��ʼ��*********************/
//eeprom ��ʼ��
void EEPROM_Init(void)
{
    Write_EEPROM_u8(0xFF,0xAA);			//��δʹ�õ�������һ����ַд����0xAA
    Write_EEPROM_u8(0xFF,0xAA);			//��������ж�дEEPROM,�˲�������ִ��
}

/*******************************************************************************
 * ��������EEPROM����
 * ����  ������u8/u16д ��u8/16��ȡ���� ��ַ��0x00��ʼ
 * ����  ����
 * ���  ����
 * ����  ���ڲ�����
 ******************************************************************************/
//дEEPROM ����  uint16_t
void Write_EEPROM_u16(uint8_t EEAddress,uint16_t EEDatas)
{
    uint8_t DATA_H=0;
    uint8_t DATA_L=0;

    DATA_H = (EEDatas>>8)&0xff;
    DATA_L =  EEDatas&0xff;
    Write_EEPROM_u8(EEAddress,DATA_H);
    Write_EEPROM_u8(EEAddress+1,DATA_L);
    //Delay_xms(1);
}
//��EEPROM ����  uint16_t
uint16_t Read_EEPROM_u16(uint8_t EEAddress)
{
    uint16_t EepromData=0;

    EepromData = Read_EEPROM_u8(EEAddress);
    EepromData = EepromData<<8;
    EepromData |= Read_EEPROM_u8(EEAddress+1);
    return 	EepromData;
}
//дEEPROM ����  uint8_t
void Write_EEPROM_u8(uint8_t EEAddress,uint8_t EEDatas)
{
    GIE = 0;		//дEEPROM����ر��ж�
    while(GIE) CLRWDT();	//�ȴ�GIEΪ0
    EEADR = EEAddress;
    EEDAT = EEDatas;
    EEIF = 0;
    EECON1 = 0x34;
    WR = 1;
    while(WR) CLRWDT();	//�ȴ�дEEPROM���,����п������Ź��ҿ��Ź����ʱ��������С�ڵ���2ms.
    //��Ҫ�幷,��ΪдEEPOM�����Ҫ2ms��ʱ��
    GIE = 1;
}

//��ȡEEPROM���� uint8_t
uint8_t Read_EEPROM_u8(uint8_t EEAddress)
{
    unsigned char ReEepromData;

    EEADR = EEAddress;
    RD = 1;
    ReEepromData = EEDAT;
    RD = 0;
    return ReEepromData;
}






