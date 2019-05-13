//********************************************************************
//
//      TIMMER5 Init Initialization Part
//      4DClass
//      Copyright(C)  2017
//      All rights reserved
//***********************************************************************
#include "includes.h"


/*******************定时器2 初始化*********************/
void Timer2_Init(void)
{
    //T2CON
    TOUTPS0=0;
    TOUTPS1=0;
    TOUTPS2=0;
    TOUTPS3=0;//设置输出后分频比1~16
    T2CKPS0=0;
    T2CKPS1=1;//时钟预分频比00=1 01=4 1x=16

    TMR2=0;//定时器2计数器付初值
    PR2=50;//定时器2周期比较寄存器 PR2的值要大于TMR2  T=(1/16MHz)*16*2*50 = 0.1ms
    //INTCON
    PEIE=1;//外设中断使能 1=enble 0=disable
    TMR2IE=1;//TMR2与PR2比较相等中断使能位 1=enable 0=disable
    TMR2IF=0;//TMR2与PR2比较相等中断标志位 硬件置1，软件清0
    GIE=1; //全局中断使能 1=enble 0=disable
    TMR2ON=1;//打开定时器2使能位
}




















