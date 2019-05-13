/*
*********************************************************************************************************
*
*                                           MASTER INCLUDES
*
*                                     ST Microelectronics STM32
*                                              with the
*                                   STM32F030 Evaluation Board
*
* Filename      : includes.h
* Version       : V1.10
* Programmer(s) : BAN
*************************************************************************************************/
#ifndef __INCLUDES_H
#define __INCLUDES_H
#include "syscfg.h"
//#include "pic16f685.h"
/*  Defines ////////////////////////////////////////////////////////////////*/
#define _XTAL_FREQ 		16000000		//

#define uint8_t         unsigned char
#define uint16_t        unsigned int
#define uint32_t        unsigned long int

#include "eeprom.h"
#include "adc.h"
#include "key.h"
#include "tim2.h"

#define INPUT       PC3
#define OUTPUT      PC2
#define LED_STUDY   PA6

/*  函数声明 --//////////////////////////////////////////////////////////////*/
void System_init(void);   //系统初始化
void GPIO_Init(void);     //GPIO管脚初始化
void Delay_xms(uint16_t x);  //硬延时初始化
void LED_Study_End(void);




#endif




