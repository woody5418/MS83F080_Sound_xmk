//********************************************************************
//
//      TIMMER5 Init Initialization Part
//      4DClass
//      Copyright(C)  2017
//      All rights reserved
//***********************************************************************
#include "includes.h"


/*******************��ʱ��2 ��ʼ��*********************/
void Timer2_Init(void)
{
    //T2CON
    TOUTPS0=0;
    TOUTPS1=0;
    TOUTPS2=0;
    TOUTPS3=0;//����������Ƶ��1~16
    T2CKPS0=0;
    T2CKPS1=1;//ʱ��Ԥ��Ƶ��00=1 01=4 1x=16

    TMR2=0;//��ʱ��2����������ֵ
    PR2=50;//��ʱ��2���ڱȽϼĴ��� PR2��ֵҪ����TMR2  T=(1/16MHz)*16*2*50 = 0.1ms
    //INTCON
    PEIE=1;//�����ж�ʹ�� 1=enble 0=disable
    TMR2IE=1;//TMR2��PR2�Ƚ�����ж�ʹ��λ 1=enable 0=disable
    TMR2IF=0;//TMR2��PR2�Ƚ�����жϱ�־λ Ӳ����1�������0
    GIE=1; //ȫ���ж�ʹ�� 1=enble 0=disable
    TMR2ON=1;//�򿪶�ʱ��2ʹ��λ
}




















