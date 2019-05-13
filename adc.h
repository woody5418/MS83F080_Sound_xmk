//********************************************************************
//
//      ADC Initialization Part
//      4DClass
//      Copyright(C)  2017
//      All rights reserved
//***********************************************************************
#ifndef __ADC_H
#define __ADC_H
#include "includes.h"


void ADC_Init(void);
uint16_t GetADCValue(void);
void ExchChannel(unsigned char ch_temp);

#endif










